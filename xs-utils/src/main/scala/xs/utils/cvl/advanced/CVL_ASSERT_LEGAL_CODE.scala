package xs.utils.cvl.advanced

import chisel3._
import chisel3.util._
import chisel3.ltl._
import chisel3.ltl.Sequence.BoolSequence

import xs.utils.cvl._

object CVL_ASSERT_LEGAL_CODE extends CVL_COMMON {
  def apply(
    assertEn: Boolean,
    coverEn: Boolean,
    cvlLongSequence: Boolean = false,
    clock: Clock,
    reset: Reset,
    name: String,
    legalCodeSeq: Seq[UInt],
    code: Valid[UInt]
  ): Unit = {
    val legal = !code.valid || (code.valid && legalCodeSeq.map { c => c === code.bits}.reduce(_ || _))
    gen_sva(
      assertEn = assertEn,
      coverEn = coverEn,
      cvlLongSequence = cvlLongSequence,
      clock = clock,
      reset = reset,
      label = "SVA_ASSERT_MUTEX_" + name,
      test_expr = legal
    )
  }
}