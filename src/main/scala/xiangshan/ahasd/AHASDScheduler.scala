package xiangshan.ahasd

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._

/**
 * AHASD Task Scheduler
 * 
 * Manages scheduling of drafting and verification tasks
 * between PIM and NPU based on EDC/TVC decisions.
 */

class AHASDSchedulerIO(implicit p: Parameters) extends XSBundle {
  // Task submission interface
  val draft_task = Flipped(Valid(new DraftTaskBundle))
  val verify_task = Flipped(Valid(new VerifyTaskBundle))
  val preverify_task = Flipped(Valid(new PreVerifyTaskBundle))
  
  // Task completion interface
  val draft_complete = Valid(new DraftCompleteBundle)
  val verify_complete = Valid(new VerifyCompleteBundle)
  
  // Control signals from AHASD Control
  val edc_continue = Input(Bool())
  val tvc_preverify_len = Input(UInt(8.W))
  
  // Resource availability
  val pim_available = Input(Bool())
  val npu_available = Input(Bool())
  
  // Task dispatch to hardware
  val pim_task = Valid(new PIMTaskBundle)
  val npu_task = Valid(new NPUTaskBundle)
}

class DraftTaskBundle(implicit p: Parameters) extends XSBundle {
  val model_id = UInt(8.W)
  val start_token = UInt(32.W)
  val kv_cache_addr = UInt(48.W)
  val kv_cache_len = UInt(32.W)
  val max_draft_len = UInt(8.W)
}

class VerifyTaskBundle(implicit p: Parameters) extends XSBundle {
  val draft_batch_id = UInt(32.W)
  val draft_tokens_addr = UInt(48.W)
  val draft_length = UInt(8.W)
  val kv_cache_addr = UInt(48.W)
  val kv_cache_len = UInt(32.W)
}

class PreVerifyTaskBundle(implicit p: Parameters) extends XSBundle {
  val draft_batch_id = UInt(32.W)
  val draft_tokens_addr = UInt(48.W)
  val verify_length = UInt(8.W)  // Small batch length from TVC
  val kv_cache_addr = UInt(48.W)
}

class DraftCompleteBundle(implicit p: Parameters) extends XSBundle {
  val batch_id = UInt(32.W)
  val draft_length = UInt(8.W)
  val avg_entropy = UInt(16.W)
  val cycles_taken = UInt(64.W)
}

class VerifyCompleteBundle(implicit p: Parameters) extends XSBundle {
  val batch_id = UInt(32.W)
  val accepted_length = UInt(8.W)
  val cycles_taken = UInt(64.W)
}

class PIMTaskBundle(implicit p: Parameters) extends XSBundle {
  val task_type = UInt(2.W)  // 0: draft, 1: pre-verify
  val data = UInt(256.W)
}

class NPUTaskBundle(implicit p: Parameters) extends XSBundle {
  val task_type = UInt(2.W)  // 0: verify
  val data = UInt(256.W)
}

class AHASDScheduler(implicit p: Parameters) extends XSModule {
  val io = IO(new AHASDSchedulerIO)
  
  // Task queues
  val draft_queue = Module(new Queue(new DraftTaskBundle, entries = 4))
  val verify_queue = Module(new Queue(new VerifyTaskBundle, entries = 8))
  val preverify_queue = Module(new Queue(new PreVerifyTaskBundle, entries = 4))
  
  // Connect task submission to queues
  draft_queue.io.enq <> io.draft_task
  verify_queue.io.enq <> io.verify_task
  preverify_queue.io.enq <> io.preverify_task
  
  // State
  val pim_busy = RegInit(false.B)
  val npu_busy = RegInit(false.B)
  val current_pim_task = Reg(new PIMTaskBundle)
  val current_npu_task = Reg(new NPUTaskBundle)
  
  // Statistics
  val total_drafts_scheduled = RegInit(0.U(32.W))
  val total_verifications_scheduled = RegInit(0.U(32.W))
  val total_preverifications_scheduled = RegInit(0.U(32.W))
  val edc_suppressed_drafts = RegInit(0.U(32.W))
  
  // Default outputs
  io.pim_task.valid := false.B
  io.pim_task.bits := DontCare
  io.npu_task.valid := false.B
  io.npu_task.bits := DontCare
  io.draft_complete.valid := false.B
  io.draft_complete.bits := DontCare
  io.verify_complete.valid := false.B
  io.verify_complete.bits := DontCare
  
  // PIM scheduling logic
  when(!pim_busy && io.pim_available) {
    // Priority: pre-verify > draft
    when(preverify_queue.io.deq.valid) {
      // Schedule pre-verification on PIM
      val task = preverify_queue.io.deq.bits
      io.pim_task.valid := true.B
      io.pim_task.bits.task_type := 1.U  // Pre-verify
      // Pack task data (simplified)
      io.pim_task.bits.data := Cat(task.draft_batch_id, task.verify_length, task.draft_tokens_addr(31, 0))
      
      preverify_queue.io.deq.ready := true.B
      pim_busy := true.B
      total_preverifications_scheduled := total_preverifications_scheduled + 1.U
      
    }.elsewhen(draft_queue.io.deq.valid && io.edc_continue) {
      // Schedule drafting on PIM (only if EDC allows)
      val task = draft_queue.io.deq.bits
      io.pim_task.valid := true.B
      io.pim_task.bits.task_type := 0.U  // Draft
      io.pim_task.bits.data := Cat(task.model_id, task.max_draft_len, task.kv_cache_addr(39, 0))
      
      draft_queue.io.deq.ready := true.B
      pim_busy := true.B
      total_drafts_scheduled := total_drafts_scheduled + 1.U
      
    }.elsewhen(draft_queue.io.deq.valid && !io.edc_continue) {
      // EDC suppresses drafting - don't schedule
      draft_queue.io.deq.ready := false.B
      edc_suppressed_drafts := edc_suppressed_drafts + 1.U
    }
  }
  
  // NPU scheduling logic
  when(!npu_busy && io.npu_available) {
    when(verify_queue.io.deq.valid) {
      // Schedule verification on NPU
      val task = verify_queue.io.deq.bits
      io.npu_task.valid := true.B
      io.npu_task.bits.task_type := 0.U  // Verify
      io.npu_task.bits.data := Cat(task.draft_batch_id, task.draft_length, task.draft_tokens_addr(39, 0))
      
      verify_queue.io.deq.ready := true.B
      npu_busy := true.B
      total_verifications_scheduled := total_verifications_scheduled + 1.U
    }
  }
  
  // Task completion handling
  when(io.draft_complete.valid) {
    pim_busy := false.B
  }
  
  when(io.verify_complete.valid) {
    npu_busy := false.B
  }
}

