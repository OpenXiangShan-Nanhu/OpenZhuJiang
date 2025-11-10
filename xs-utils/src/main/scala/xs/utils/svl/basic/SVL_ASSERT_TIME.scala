package xs.utils.svl.basic

import chisel3._
import chisel3.util._

class assert_time(
  severity_level: Int,
  num_cks: Int,
  action_on_new_start: Int,
  property_type: Int,
  options: Int,
  msg: String,
  category: Int,
  coverage_level_1: Int,
  coverage_level_2: Int,
  coverage_level_3: Int) extends BlackBox(
    Map(
      "severity_level" -> severity_level,
      "num_cks" -> num_cks,
      "action_on_new_start" -> action_on_new_start,
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

object SVL_ASSERT_TIME {
  def apply(
    name: String,
    clock: Clock,
    reset: Reset,
    severity_level: Int,
    num_cks: Int,
    action_on_new_start: Int,
    property_type: Int,
    options: Int,
    msg: String,
    category: Int,
    coverage_level_1: Int,
    coverage_level_2: Int,
    coverage_level_3: Int,
    start_event: Bool,
    test_expr: Bool
  ): Unit = {
    val assertTime = Module(new assert_time(
      severity_level = severity_level,
      num_cks = num_cks,
      action_on_new_start = action_on_new_start,
      property_type = property_type,
      options = options,
      msg = msg,
      category = category,
      coverage_level_1 = coverage_level_1,
      coverage_level_2 = coverage_level_2,
      coverage_level_3 = coverage_level_3
    ))
    assertTime.io.clk := clock
    assertTime.io.reset_n := ~reset.asBool
    assertTime.io.start_event := start_event
    assertTime.io.test_expr := test_expr
    assertTime.suggestName(name)
  }
}
