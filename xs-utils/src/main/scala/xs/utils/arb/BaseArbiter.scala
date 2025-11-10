package xs.utils.arb

import chisel3._
import chisel3.util._
import xs.utils.ResetRRArbiter
import firrtl.passes.InlineAnnotation

abstract class BaseArbiter[T <: Data](gen: T, size:Int) extends Module {
  val io = IO(new ArbiterIO(gen, size))
}

class XsRRArbiter [T <: Data](gen: T, size:Int) extends BaseArbiter(gen, size) {
  private val arb = Module(new ResetRRArbiter(gen, size))
  chisel3.experimental.annotate(this)(Seq(InlineAnnotation(arb.toNamed)))
  arb.io.in <> io.in
  io.out <> arb.io.out
  io.chosen := arb.io.chosen
}