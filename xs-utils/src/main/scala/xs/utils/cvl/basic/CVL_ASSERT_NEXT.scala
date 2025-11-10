package xs.utils.cvl.basic

import chisel3._
import chisel3.util._
import chisel3.ltl._
import chisel3.ltl.Sequence.BoolSequence
import xs.utils.DelayN

import xs.utils.cvl._

object CVL_ASSERT_NEXT extends CVL_COMMON {
  def apply(
    assertEn: Boolean,
    coverEn: Boolean,
    cvlLongSequence: Boolean = false,
    clock: Clock,
    reset: Reset,
    name: String,
    num_cks: Int,
    start_event: Bool,
    test_expr: Bool
  ): Unit = {
    if(cvlLongSequence) {
      val start = BoolSequence(start_event)
      val test = BoolSequence(test_expr)
      gen_sva(
        assertEn = assertEn,
        coverEn = coverEn,
        clock = clock,
        reset = reset,
        label = "SVA_ASSERT_NEXT_" + name,
        test_expr = start |-> test.delay(num_cks)
      )
    } else {
      val antecedent_expr = DelayN(start_event, num_cks)
      val test = Mux(antecedent_expr, test_expr, true.B)
      assert(test, "SVA_ASSERT_IMPLICATION_" + name)
    }
  }
}