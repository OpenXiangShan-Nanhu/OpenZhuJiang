package xs.utils.queue

import chisel3._
import chisel3.util._

class FastQueue[T <: Data](gen:T, size:Int, deqDataNoX:Boolean) extends Module with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val enq     = Flipped(Decoupled(gen))
    val deq     = Decoupled(gen)
    val count   = Output(UInt(log2Ceil(size + 1).W))
    val freeNum = Output(UInt(log2Ceil(size + 1).W))
  })
  require(size > 1)
  private val valids = RegInit(VecInit(Seq.fill(size)(false.B)))
  private val array = Reg(Vec(size, gen))
  private val enqRdyReg = RegInit(true.B)

  private val enqFire = io.enq.fire
  private val deqFire = io.deq.fire
  private val lastEmptyOH = Cat(valids.reverse) +& 1.U
  private val enqPtrOH = Mux(deqFire, Cat(false.B, lastEmptyOH(size - 1, 1)), lastEmptyOH(size - 1, 0))
  for(i <- 0 until (size - 1)) {
    when(enqFire && enqPtrOH(i)) {
      valids(i) := true.B
      array(i) := io.enq.bits
    }.elsewhen(deqFire) {
      valids(i) := valids(i + 1)
      array(i) := Mux(valids(i + 1), array(i + 1), array(i))
    }
  }
  when(enqFire && enqPtrOH(size - 1)) {
    valids.last := true.B
    array.last := io.enq.bits
  }.elsewhen(deqFire) {
    valids.last := false.B
  }

  when(enqFire && !deqFire) {
    enqRdyReg := !valids(size - 2)
  }.elsewhen(!enqFire && deqFire) {
    enqRdyReg := true.B
  }

  io.deq.valid := valids.head
  io.deq.bits := array.head
  io.enq.ready := enqRdyReg
  if(deqDataNoX) io.deq.bits := Mux(io.deq.valid, array.head, 0.U.asTypeOf(gen))

  io.count := PopCount(valids)
  io.freeNum := size.U - io.count
  when(io.count.orR) {
    assert(io.deq.valid)
  }
  when(io.count === size.U) {
    assert(!io.enq.ready)
  }
  assert(PopCount(lastEmptyOH) === 1.U)
}

object FastQueueRaw {
  def apply[T <: Data](enq: DecoupledIO[T], size: Int, noX:Boolean, name:Option[String]): DecoupledIO[T] = {
    val q = Module(new FastQueue(chiselTypeOf(enq.bits), size, noX))
    name.foreach(n => q.suggestName(n))
    q.io.enq <> enq
    q.io.deq
  }
}

class FastQueueFactory(noX:Boolean) {
  def apply[T <: Data](enq: DecoupledIO[T], size: Int, name:Option[String]): DecoupledIO[T] = FastQueueRaw(enq, size, noX, name)

  def apply[T <: Data](enq: DecoupledIO[T], name:Option[String]): DecoupledIO[T] = apply(enq, 2, name)

  def apply[T <: Data](enq: DecoupledIO[T], size: Int): DecoupledIO[T] = apply(enq, size, None)

  def apply[T <: Data](enq: DecoupledIO[T]): DecoupledIO[T] = apply(enq, 2, None)

  def apply[T <: Data](enq: DecoupledIO[T], deq: DecoupledIO[T], size: Int, name:Option[String]):Unit = deq <> apply(enq, size, name)

  def apply[T <: Data](enq: DecoupledIO[T], deq: DecoupledIO[T], size: Int):Unit = deq <> apply(enq, size, None)

  def apply[T <: Data](enq: DecoupledIO[T], deq: DecoupledIO[T], name:Option[String]):Unit = deq <> apply(enq, 2, name)

  def apply[T <: Data](enq: DecoupledIO[T], deq: DecoupledIO[T]):Unit = deq <> apply(enq, 2, None)

  def apply[T <: Data](enq: DecoupledIO[T], enable: Boolean, size: Int): DecoupledIO[T] = if(enable) apply(enq, size, None) else enq

  def apply[T <: Data](enq: DecoupledIO[T], enable: Boolean): DecoupledIO[T] = apply(enq, enable, 2)
}

object FastQueueNoX extends FastQueueFactory(true)

object FastQueue extends FastQueueFactory(false)
