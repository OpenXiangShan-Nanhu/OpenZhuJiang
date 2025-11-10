package xs.utils.svl.basic

import chisel3._
import chisel3.util._

class assert_delta(
  severity_level: Int,
  width: Int,
  min: Int,
  max: Int,
  property_type: Int,
  msg: String,
  category: Int,
  coverage_level_1: Int,
  coverage_level_2: Int,
  coverage_level_3: Int) extends BlackBox(
    Map(
      "severity_level" -> severity_level,
      "width" -> width,
      "min" -> min,
      "max" -> max,
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

object SVL_ASSERT_DELTA {
  def apply(
    name: String,
    clock: Clock,
    reset: Reset,
    severity_level: Int,
    width: Int,
    min: Int,
    max: Int,
    property_type: Int,
    msg: String,
    category: Int,
    coverage_level_1: Int,
    coverage_level_2: Int,
    coverage_level_3: Int,
    test_expr: Bool
  ): Unit = {
    val assertDelta = Module(new assert_delta(
      severity_level = severity_level,
      width = width,
      min = min,
      max = max,
      property_type = property_type,
      msg = msg,
      category = category,
      coverage_level_1 = coverage_level_1,
      coverage_level_2 = coverage_level_2,
      coverage_level_3 = coverage_level_3
    ))
    assertDelta.io.clk := clock
    assertDelta.io.reset_n := ~reset.asBool
    assertDelta.io.test_expr := test_expr
    assertDelta.suggestName(name)
  }
}
