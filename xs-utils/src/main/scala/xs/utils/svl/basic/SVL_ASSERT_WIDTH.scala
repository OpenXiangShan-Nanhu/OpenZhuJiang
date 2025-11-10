package xs.utils.svl.basic

import chisel3._
import chisel3.util._

class assert_width(
  severity_level: Int,
  min_cks: Int,
  max_cks: Int,
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
    val test_expr = Input(Bool())
  })
}

object SVL_ASSERT_WIDTH {
  def apply(
    name: String,
    clock: Clock,
    reset: Reset,
    severity_level: Int,
    min_cks: Int,
    max_cks: Int,
    property_type: Int,
    msg: String,
    category: Int,
    coverage_level_1: Int,
    coverage_level_2: Int,
    coverage_level_3: Int,
    start_event: Bool,
    test_expr: Bool
  ): Unit = {
    val assertWidth = Module(new assert_width(
      severity_level = severity_level,
      min_cks = min_cks,
      max_cks = max_cks,
      property_type = property_type,
      msg = msg,
      category = category,
      coverage_level_1 = coverage_level_1,
      coverage_level_2 = coverage_level_2,
      coverage_level_3 = coverage_level_3
    ))
    assertWidth.io.clk := clock
    assertWidth.io.reset_n := ~reset.asBool
    assertWidth.io.test_expr := test_expr
    assertWidth.suggestName(name)
  }
}
