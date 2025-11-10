package xs.utils.queue

import chisel3._
import chisel3.util._

class RegInQueue[T <: Data](gen:T, entries:Int) extends Module {
  val io = IO(new Bundle{
    val enq = Flipped(Decoupled(gen))
    val deq = Decoupled(gen)
  })
  require(entries > 2)
  private val q = Module(new Queue(gen, entries, flow = true))
  private val vld = RegNext(io.enq.valid, false.B)
  private val dat = RegNext(io.enq.bits)
  private val cnt = RegInit(0.U(log2Ceil(entries + 1).W))
  private val cntNext = WireInit(cnt)
  private val rdy = RegNext(cntNext < (entries - 1).U, true.B)

  q.io.enq.valid := vld && RegNext(rdy, true.B)
  q.io.enq.bits := dat
  io.deq <> q.io.deq
  when(q.io.enq.valid) {
    assert(q.io.enq.ready)
  }
  io.enq.ready := rdy

  when(q.io.enq.fire && !q.io.deq.fire) {
    cntNext := q.io.count + 1.U
  }.elsewhen(!q.io.enq.fire && q.io.deq.fire) {
    cntNext := q.io.count - 1.U
  }
  cnt := cntNext
}

object RegInQueue {
  def apply[T <: Data](enq:DecoupledIO[T], entries:Int, name:Option[String]): DecoupledIO[T] = {
    val q = Module(new RegInQueue(enq.bits.cloneType, entries))
    q.io.enq <> enq
    name.foreach(n => q.suggestName(n))
    q.io.deq
  }
  def apply[T <: Data](enq:DecoupledIO[T], entries:Int, name:String): DecoupledIO[T] = apply(enq, entries, Some(name))
  def apply[T <: Data](enq:DecoupledIO[T], entries:Int): DecoupledIO[T] = apply(enq, entries, None)
  def apply[T <: Data](enq:DecoupledIO[T], name:String): DecoupledIO[T] = apply(enq, 3, Some(name))
  def apply[T <: Data](enq:DecoupledIO[T]): DecoupledIO[T] = apply(enq, 3, None)
}