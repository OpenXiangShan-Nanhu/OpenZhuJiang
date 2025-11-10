package xs.utils.svl.basic

import chisel3._
import chisel3.util._

class assert_unchange(
  severity_level: Int,
  width: Int,
  num_cks: Int,
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
    val test_expr = Input(UInt(width.W))
  })
}

object SVL_ASSERT_UNCHANGE {
  def apply(
    name: String,
    clock: Clock,
    reset: Reset,
    severity_level: Int,
    width: Int,
    num_cks: Int,
    action_on_new_start: Int,
    property_type: Int,
    msg: String,
    category: Int,
    coverage_level_1: Int,
    coverage_level_2: Int,
    coverage_level_3: Int,
    start_event: Bool,
    test_expr: UInt
  ): Unit = {
    val assertUnchange = Module(new assert_unchange(
      severity_level = severity_level,
      width = width,
      num_cks = num_cks,
      action_on_new_start = action_on_new_start,
      property_type = property_type,
      msg = msg,
      category = category,
      coverage_level_1 = coverage_level_1,
      coverage_level_2 = coverage_level_2,
      coverage_level_3 = coverage_level_3
    ))
    assertUnchange.io.clk := clock
    assertUnchange.io.reset_n := ~reset.asBool
    assertUnchange.io.start_event := start_event
    assertUnchange.io.test_expr := test_expr
    assertUnchange.suggestName(name)
  }
}
