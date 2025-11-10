package xs.utils.svl.basic

import chisel3._
import chisel3.util._

class assert_next(
  severity_level: Int,
  num_cks: Int,
  check_overlapping: Int,
  only_if: Int,
  property_type: Int,
  msg: String,
  category: Int,
  coverage_level_1: Int,
  coverage_level_2: Int,
  coverage_level_3: Int) extends BlackBox(
    Map(
      "severity_level" -> severity_level,
      "num_cks" -> num_cks,
      "check_overlapping" -> check_overlapping,
      "only_if" -> only_if,
      "property_type" -> property_type,
      "msg" -> msg,
      "category" -> category,
      "coverage_level_1" -> coverage_level_1,
      "coverage_level_2" -> coverage_level_2,
      "coverage_level_3" -> coverage_level_3
    )
  ) {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val reset_n = Input(Reset())
    val start_event = Input(Bool())
    val test_expr = Input(Bool())
  })
}

object SVL_ASSERT_NEXT {
  def apply(
    name: String,
    clock: Clock,
    reset: Reset,
    severity_level: Int,
    num_cks: Int,
    check_overlapping: Int,
    only_if: Int,
    property_type: Int,
    msg: String,
    category: Int,
    coverage_level_1: Int,
    coverage_level_2: Int,
    coverage_level_3: Int,
    start_event: Bool,
    test_expr: Bool
  ): Unit = {
    val assertNext = Module(new assert_next(
      severity_level = severity_level,
      num_cks = num_cks,
      check_overlapping = check_overlapping,
      only_if = only_if,
      property_type = property_type,
      msg = msg,
      category = category,
      coverage_level_1 = coverage_level_1,
      coverage_level_2 = coverage_level_2,
      coverage_level_3 = coverage_level_3
    ))
    assertNext.io.clk := clock
    assertNext.io.reset_n := ~reset.asBool
    assertNext.io.test_expr := test_expr
    assertNext.io.start_event := start_event
    assertNext.suggestName(name)
  }
}
