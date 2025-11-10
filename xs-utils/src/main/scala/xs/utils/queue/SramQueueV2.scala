package xs.utils.queue

import chisel3._
import chisel3.util._
import xs.utils.sram.DualPortSramTemplate

class SramQueueV2[T <: Data](gen:T, size:Int, hasMbist:Boolean, suffix:String = "") extends Module with HasCircularQueuePtrHelper {
  private class SramQueuePtr extends CircularQueuePtr[SramQueuePtr](size)
  private object SramQueuePtr {
    def apply(f:Boolean, v:Int):SramQueuePtr = {
      val ptr = Wire(new SramQueuePtr)
      ptr.flag := f.B
      ptr.value := v.U
      ptr
    }
  }
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(gen))
    val deq = Decoupled(gen)
  })
  private val enqPtr = RegInit(SramQueuePtr(f= false, v = 0))
  private val deqPtr = RegInit(SramQueuePtr(f= false, v = 0))
  private val oq = Module(new Queue(gen, 2))
  private val ram = Module(new DualPortSramTemplate[T](
    gen = gen,
    set = size,
    way = 1,
    hasMbist = hasMbist,
    suffix = suffix,
    moduleName = Some("SramQueueSram")
  ))
  private val full = enqPtr.flag =/= deqPtr.flag && enqPtr.value === deqPtr.value
  private val empty = enqPtr === deqPtr
  private val ramOutVld = RegInit(false.B)
  private val readPipeEn = !ramOutVld || oq.io.enq.ready

  ramOutVld := Mux(readPipeEn, !empty, ramOutVld)
  io.enq.ready := !full
  ram.io.wreq.valid := io.enq.fire
  ram.io.wreq.bits.addr := enqPtr.value
  ram.io.wreq.bits.data := VecInit(Seq.fill(1)(io.enq.bits))
  ram.io.rreq.valid := !empty && readPipeEn
  ram.io.rreq.bits := deqPtr.value

  oq.io.enq.valid := ramOutVld
  oq.io.enq.bits := ram.io.rresp.bits.head
  io.deq <> oq.io.deq

  when(ram.io.wreq.valid) {
    enqPtr := enqPtr + 1.U
  }
  when(ram.io.rreq.valid) {
    deqPtr := deqPtr + 1.U
  }
  assert(deqPtr <= enqPtr)
}
