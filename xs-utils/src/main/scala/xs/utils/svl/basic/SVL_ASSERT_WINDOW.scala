package xs.utils.svl.basic

import chisel3._
import chisel3.util._

class assert_window(
  severity_level: Int,
  property_type: Int,
  msg: String,
  category: Int,
  coverage_level_1: Int,
  coverage_level_2: Int,
  coverage_level_3: Int) extends BlackBox(
    Map(
      "severity_level" -> severity_level,
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
    val end_event = Input(Bool())
  })
}

object SVL_ASSERT_WINDOW {
  def apply(
    name: String,
    clock: Clock,
    reset: Reset,
    severity_level: Int,
    property_type: Int,
    msg: String,
    category: Int,
    coverage_level_1: Int,
    coverage_level_2: Int,
    coverage_level_3: Int,
    start_event: Bool,
    test_expr: Bool,
    end_event: Bool
  ): Unit = {
    val assertWindow = Module(new assert_window(
      severity_level = severity_level,
      property_type = property_type,
      msg = msg,
      category = category,
      coverage_level_1 = coverage_level_1,
      coverage_level_2 = coverage_level_2,
      coverage_level_3 = coverage_level_3
    ))
    assertWindow.io.clk := clock
    assertWindow.io.reset_n := ~reset.asBool
    assertWindow.io.start_event := start_event
    assertWindow.io.test_expr := test_expr
    assertWindow.io.end_event := end_event
    assertWindow.suggestName(name)
  }
}
