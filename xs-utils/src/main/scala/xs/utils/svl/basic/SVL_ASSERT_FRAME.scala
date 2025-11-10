package xs.utils.svl.basic

import chisel3._
import chisel3.util._

class assert_frame(
  severity_level: Int,
  min_cks: Int,
  max_cks: Int,
  action_on_new_start: Int,
  property_type: Int,
  msg: String,
  category: Int,
  coverage_level_1: Int,
  coverage_level_2: Int,
  coverage_level_3: Int) extends BlackBox(
    Map(
      "severity_level" -> severity_level,
      "min_cks" -> min_cks,
      "max_cks" -> max_cks,
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

object SVL_ASSERT_FRAME {
  def apply(
    name: String,
    clock: Clock,
    reset: Reset,
    severity_level: Int,
    min_cks: Int,
    max_cks: Int,
    action_on_new_start: Int,
    property_type: Int,
    msg: String,
    category: Int,
    coverage_level_1: Int,
    coverage_level_2: Int,
    coverage_level_3: Int,
    start_event: Bool,
    test_expr: Bool
  ): Unit = {
    val assertFrame = Module(new assert_frame(
      severity_level = severity_level,
      min_cks = min_cks,
      max_cks = max_cks,
      action_on_new_start = action_on_new_start,
      property_type = property_type,
      msg = msg,
      category = category,
      coverage_level_1 = coverage_level_1,
      coverage_level_2 = coverage_level_2,
      coverage_level_3 = coverage_level_3
    ))
    assertFrame.io.clk := clock
    assertFrame.io.reset_n := ~reset.asBool
    assertFrame.io.start_event := start_event
    assertFrame.io.test_expr := test_expr
    assertFrame.suggestName(name)
  }
}
