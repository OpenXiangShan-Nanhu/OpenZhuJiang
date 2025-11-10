package xs.utils.cvl.advanced

import chisel3._
import chisel3.util._
import chisel3.ltl._
import chisel3.ltl.Sequence.BoolSequence

import xs.utils.cvl._

object CVL_ASSERT_MUTEX extends CVL_COMMON {
  def apply(
    assertEn: Boolean,
    coverEn: Boolean,
    cvlLongSequence: Boolean = false,
    clock: Clock,
    reset: Reset,
    name: String,
    a: Bool,
    b: Bool
  ): Unit = {
    gen_sva(
      assertEn = assertEn,
      coverEn = coverEn,
      cvlLongSequence = cvlLongSequence,
      clock = clock,
      reset = reset,
      label = "SVA_ASSERT_MUTEX_" + name,
      test_expr = !(a && b)
    )
  }
}