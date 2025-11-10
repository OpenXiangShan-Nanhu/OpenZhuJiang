package xs.utils.arb
import chisel3._
import chisel3.util._

class SelNto1[T <: Data](gen: T, size:Int, selfCmpOtherFunc: (T, T) => Bool) extends Module {
  val io = IO(new Bundle {
    val in = Vec(size, Flipped(Valid(gen)))
    val out = Output(UInt(size.W))
  })
  private val oldestOhSeq = io.in.zipWithIndex.map({ case (self, idx) =>
    val cmpVec = io.in.zipWithIndex.filterNot(_._2 == idx).map(i => self.valid && Mux(i._1.valid, selfCmpOtherFunc(self.bits, i._1.bits), true.B))
    Cat(cmpVec).andR
  })
  private val valids = Cat(io.in.map(_.valid).reverse)
  io.out := Cat(oldestOhSeq.reverse)
  assert((valids & io.out) === io.out, "selected entries should all be valid!")
}

abstract class ConditionArbiter[T <: Data](gen: T, size:Int, selfCmpOtherFunc: (T, T) => Bool, arb: => BaseArbiter[T]) extends BaseArbiter(gen, size)  {

  private val active = Cat(io.in.map(_.valid)).orR
  private val selector = Module(new SelNto1(gen, size, selfCmpOtherFunc))
  private val selReg = RegEnable(selector.io.out, 0.U, active)
  private val selArb = Module(arb)

  for(i <- io.in.indices) {
    selector.io.in(i).valid := io.in(i).valid && !selArb.io.in(i).fire
    selector.io.in(i).bits := io.in(i).bits
    io.in(i).ready := selArb.io.in(i).fire

    selArb.io.in(i).valid := selReg(i)
    selArb.io.in(i).bits := io.in(i).bits

    when(selReg(i)) {
      assert(io.in(i).valid)
    }
  }
  io.chosen := selArb.io.chosen
  io.out <> selArb.io.out
}

class ConditionRRArbiter[T <: Data](gen: T, size:Int, selfCmpOtherFunc: (T, T) => Bool) extends ConditionArbiter(gen, size, selfCmpOtherFunc, new XsRRArbiter(gen, size))

class ConditionVipArbiter[T <: Data](gen: T, size:Int, selfCmpOtherFunc: (T, T) => Bool) extends ConditionArbiter(gen, size, selfCmpOtherFunc, new VipArbiter(gen, size))

class ConditionStepVipArbiter[T <: Data](gen: T, size:Int, selfCmpOtherFunc: (T, T) => Bool) extends ConditionArbiter(gen, size, selfCmpOtherFunc, new StepVipArbiter(gen, size))