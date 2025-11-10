package xs.utils.cvl

import chisel3._
import chisel3.util._
import chisel3.ltl._
import chisel3.ltl.Sequence.BoolSequence

trait CVL_COMMON {
  def gen_sva(
    assertEn: Boolean,
    coverEn: Boolean,
    cvlLongSequence: Boolean,
    clock: Clock,
    reset: Reset,
    label: String,
    test_expr: Bool
  ): Unit = {
    if(cvlLongSequence) {
      if (assertEn) {
        AssertProperty(prop = BoolSequence(test_expr), clock = Some(clock), disable = Some(reset.asDisable), label = Some(label))
      }
      if (coverEn) {
        CoverProperty(prop = BoolSequence(test_expr), clock = Some(clock), disable = Some(reset.asDisable), label = Some(label))
      }
    } else {
      assert(test_expr, label)
    }
  }

  def gen_sva(
    assertEn: Boolean,
    coverEn: Boolean,
    clock: Clock,
    reset: Reset,
    label: String,
    test_expr: Property
  ): Unit = {
    if (assertEn) {
      AssertProperty(prop = test_expr, clock = Some(clock), disable = Some(reset.asDisable), label = Some(label))
    }
    if (coverEn) {
      CoverProperty(prop = test_expr, clock = Some(clock), disable = Some(reset.asDisable), label = Some(label))
    }
  }
}
