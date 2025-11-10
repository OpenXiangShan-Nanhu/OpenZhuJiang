package xs.utils.cvl.basic

import chisel3._
import chisel3.util._
import chisel3.ltl._
import chisel3.ltl.Sequence.BoolSequence

import xs.utils.cvl._

object CVL_ASSERT_IMPLICATION extends CVL_COMMON {
  def apply(
    assertEn: Boolean,
    coverEn: Boolean,
    cvlLongSequence: Boolean = false,
    clock: Clock,
    reset: Reset,
    name: String,
    antecedent_expr: Bool,
    consequent_expr: Bool
  ): Unit = {
    if(cvlLongSequence) {
      gen_sva(
        assertEn = assertEn,
        coverEn = coverEn,
        clock = clock,
        reset = reset,
        label = "SVA_ASSERT_IMPLICATION_" + name,
        test_expr = BoolSequence(antecedent_expr) |-> BoolSequence(consequent_expr)
      )
    } else {
      val test_expr = Mux(antecedent_expr, consequent_expr, true.B)
      assert(test_expr, "SVA_ASSERT_IMPLICATION_" + name)
    }
  }
}