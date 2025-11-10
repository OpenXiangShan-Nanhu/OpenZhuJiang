package xs.utils.cvl.basic

import chisel3._
import chisel3.util._
import chisel3.ltl._
import chisel3.ltl.Sequence.BoolSequence

import xs.utils.cvl._

object CVL_ASSERT_ZERO_ONE_HOT extends CVL_COMMON {
  def apply(
    assertEn: Boolean,
    coverEn: Boolean,
    cvlLongSequence: Boolean = false,
    clock: Clock,
    reset: Reset,
    name: String,
    test_expr: UInt
  ): Unit = {
    val isOneHot = PopCount(test_expr) === 1.U
    val isZero = test_expr === 0.U
    gen_sva(
      assertEn = assertEn,
      coverEn = coverEn,
      cvlLongSequence = cvlLongSequence,
      clock = clock,
      reset = reset,
      label = "SVA_ASSERT_ZERO_ONE_HOT_" + name,
      test_expr = isOneHot || isZero
    )
  }
}