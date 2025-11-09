package xiangshan.ahasd

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._

/**
 * AHASD Control Module for XiangShan Processor
 * 
 * Integrates EDC (Entropy-History-Aware Drafting Control) and 
 * TVC (Time-Aware Pre-Verification Control) into CPU scheduler
 * 
 * This module polls the EDC/TVC hardware units (implemented in PIM memory-mapped regions)
 * and makes scheduling decisions for speculative decoding tasks.
 */

class AHASDControlIO(implicit p: Parameters) extends XSBundle {
  // Memory-mapped interface to EDC/TVC hardware
  val edc_poll = Output(Bool())
  val edc_decision = Input(Bool())  // Continue drafting or not
  val edc_update = Valid(new EDCUpdateBundle)
  
  val tvc_poll = Output(Bool())
  val tvc_preverify_length = Input(UInt(8.W))
  val tvc_update = Valid(new TVCUpdateBundle)
  
  // Queue management
  val queue_status = Input(new QueueStatusBundle)
  val queue_control = Valid(new QueueControlBundle)
  
  // Interrupt signals
  val edc_interrupt = Input(Bool())
  val tvc_interrupt = Input(Bool())
  val queue_overflow = Input(Bool())
}

class EDCUpdateBundle(implicit p: Parameters) extends XSBundle {
  val batch_id = UInt(32.W)
  val avg_entropy = UInt(16.W)  // Fixed-point entropy value
  val accepted = Bool()
}

class TVCUpdateBundle(implicit p: Parameters) extends XSBundle {
  val npu_cycles = UInt(64.W)
  val pim_cycles = UInt(64.W)
  val kv_length = UInt(32.W)
  val draft_length = UInt(16.W)
}

class QueueStatusBundle(implicit p: Parameters) extends XSBundle {
  val unverified_count = UInt(8.W)
  val feedback_count = UInt(8.W)
  val preverify_count = UInt(8.W)
}

class QueueControlBundle(implicit p: Parameters) extends XSBundle {
  val clear_unverified = Bool()
  val clear_feedback = Bool()
  val priority_boost = Bool()
}

class AHASDControl(implicit p: Parameters) extends XSModule {
  val io = IO(new AHASDControlIO)
  
  // Configuration registers (memory-mapped)
  val cfg_enable = RegInit(true.B)
  val cfg_edc_poll_interval = RegInit(100.U(32.W))
  val cfg_tvc_poll_interval = RegInit(50.U(32.W))
  val cfg_queue_poll_interval = RegInit(20.U(32.W))
  
  // State machines
  val edc_state = RegInit(0.U(2.W))
  val tvc_state = RegInit(0.U(2.W))
  
  // Counters
  val edc_poll_counter = RegInit(0.U(32.W))
  val tvc_poll_counter = RegInit(0.U(32.W))
  val queue_poll_counter = RegInit(0.U(32.W))
  
  // Statistics
  val total_edc_polls = RegInit(0.U(64.W))
  val total_tvc_polls = RegInit(0.U(64.W))
  val edc_suppressions = RegInit(0.U(32.W))
  val tvc_preverifications = RegInit(0.U(32.W))
  
  // Default outputs
  io.edc_poll := false.B
  io.tvc_poll := false.B
  io.edc_update.valid := false.B
  io.edc_update.bits := DontCare
  io.tvc_update.valid := false.B
  io.tvc_update.bits := DontCare
  io.queue_control.valid := false.B
  io.queue_control.bits := DontCare
  
  when(cfg_enable) {
    // EDC polling logic
    edc_poll_counter := edc_poll_counter + 1.U
    when(edc_poll_counter >= cfg_edc_poll_interval) {
      edc_poll_counter := 0.U
      io.edc_poll := true.B
      total_edc_polls := total_edc_polls + 1.U
      
      // If EDC suggests suppression, increment counter
      when(!io.edc_decision) {
        edc_suppressions := edc_suppressions + 1.U
      }
    }
    
    // TVC polling logic
    tvc_poll_counter := tvc_poll_counter + 1.U
    when(tvc_poll_counter >= cfg_tvc_poll_interval) {
      tvc_poll_counter := 0.U
      io.tvc_poll := true.B
      total_tvc_polls := total_tvc_polls + 1.U
      
      // If TVC suggests pre-verification, increment counter
      when(io.tvc_preverify_length > 0.U) {
        tvc_preverifications := tvc_preverifications + 1.U
      }
    }
    
    // Queue management logic
    queue_poll_counter := queue_poll_counter + 1.U
    when(queue_poll_counter >= cfg_queue_poll_interval) {
      queue_poll_counter := 0.U
      
      // Check for queue overflow
      when(io.queue_status.unverified_count > 56.U) {
        io.queue_control.valid := true.B
        io.queue_control.bits.priority_boost := true.B
        io.queue_control.bits.clear_unverified := false.B
        io.queue_control.bits.clear_feedback := false.B
      }
    }
    
    // Handle EDC updates from NPU
    when(io.edc_update.valid) {
      // Forward update to EDC hardware
      // (actual implementation would write to memory-mapped EDC registers)
    }
    
    // Handle TVC updates
    when(io.tvc_update.valid) {
      // Forward update to TVC hardware
      // (actual implementation would write to memory-mapped TVC registers)
    }
  }
  
  // Interrupt handling
  val edc_interrupt_reg = RegNext(io.edc_interrupt)
  val tvc_interrupt_reg = RegNext(io.tvc_interrupt)
  val queue_overflow_reg = RegNext(io.queue_overflow)
  
  // Rising edge detection for interrupts
  val edc_interrupt_trigger = io.edc_interrupt && !edc_interrupt_reg
  val tvc_interrupt_trigger = io.tvc_interrupt && !tvc_interrupt_reg
  val queue_overflow_trigger = io.queue_overflow && !queue_overflow_reg
  
  when(edc_interrupt_trigger) {
    // Handle EDC interrupt (e.g., pattern history table update complete)
  }
  
  when(tvc_interrupt_trigger) {
    // Handle TVC interrupt (e.g., pre-verification timing critical)
  }
  
  when(queue_overflow_trigger) {
    // Handle queue overflow - may need to clear or boost priority
    io.queue_control.valid := true.B
    io.queue_control.bits.clear_feedback := true.B
    io.queue_control.bits.clear_unverified := false.B
    io.queue_control.bits.priority_boost := false.B
  }
}

/**
 * Memory-mapped control interface for AHASD
 * 
 * This module exposes AHASD control as memory-mapped registers
 * that can be accessed from software or other hardware modules.
 */
class AHASDControlMemMap(baseAddr: Long)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val mem = Flipped(new MemPortIO)
    val ahasd = Flipped(new AHASDControlIO)
  })
  
  // Register map
  val OFFSET_CFG_ENABLE = 0x00
  val OFFSET_EDC_POLL_INTERVAL = 0x04
  val OFFSET_TVC_POLL_INTERVAL = 0x08
  val OFFSET_QUEUE_POLL_INTERVAL = 0x0C
  val OFFSET_EDC_DECISION = 0x10
  val OFFSET_TVC_PREVERIFY_LEN = 0x14
  val OFFSET_QUEUE_STATUS = 0x18
  val OFFSET_STATISTICS = 0x20
  
  // Register read/write logic
  // (Implementation details omitted for brevity)
}

