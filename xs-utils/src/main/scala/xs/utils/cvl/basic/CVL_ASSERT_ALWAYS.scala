package xs.utils.cvl.basic

import chisel3._
import chisel3.util._
import chisel3.ltl._
import chisel3.ltl.Sequence.BoolSequence

import xs.utils.cvl._

object CVL_ASSERT_ALWAYS extends CVL_COMMON {
  def apply(
    assertEn: Boolean,
    coverEn: Boolean,
    cvlLongSequence: Boolean = false,
    clock: Clock,
    reset: Reset,
    name: String,
    test_expr: Bool
  ): Unit = {
    gen_sva(
      assertEn = assertEn,
      coverEn = coverEn,
      cvlLongSequence = cvlLongSequence,
      clock = clock,
      reset = reset,
      label = "SVA_ASSERT_ALWAYS_" + name,
      test_expr = test_expr
    )
  }
}