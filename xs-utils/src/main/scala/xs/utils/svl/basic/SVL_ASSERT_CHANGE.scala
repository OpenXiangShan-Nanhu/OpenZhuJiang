package xs.utils.svl.basic

import chisel3._
import chisel3.util._

class assert_change(
  severity_level: Int,
  width: Int,
  num_cks: Int,
  flag: Int,
  action_on_new_start: Int,
  property_type: Int,
  msg: String,
  category: Int,
  coverage_level_1: Int,
  coverage_level_2: Int,
  coverage_level_3: Int) extends BlackBox(
    Map(
      "severity_level" -> severity_level,
      "width" -> width,
      "num_cks" -> num_cks,
      "flag" -> flag,
      "action_on_new_start" -> action_on_new_start,
      "property_type" -> property_type,
      "msg" -> msg,
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

object SVL_ASSERT_CHANGE {
  def apply(
    name: String,
    clock: Clock,
    reset: Reset,
    severity_level: Int,
    width: Int,
    num_cks: Int,
    flag: Int,
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
    val assertChange = Module(new assert_change(
      severity_level = severity_level,
      width = width,
      num_cks = num_cks,
      flag = flag,
      action_on_new_start = action_on_new_start,
      property_type = property_type,
      msg = msg,
      category = category,
      coverage_level_1 = coverage_level_1,
      coverage_level_2 = coverage_level_2,
      coverage_level_3 = coverage_level_3
    ))
    assertChange.io.clk := clock
    assertChange.io.reset_n := ~reset.asBool
    assertChange.io.start_event := start_event
    assertChange.io.test_expr := test_expr
    assertChange.suggestName(name)
  }
}
