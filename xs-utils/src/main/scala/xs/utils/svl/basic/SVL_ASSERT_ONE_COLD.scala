package xs.utils.svl.basic

import chisel3._
import chisel3.util._

class assert_one_cold(
  severity_level: Int,
  width: Int,
  inactive: Int,
  property_type: Int,
  msg: String,
  coverage_level_1: Int,
  coverage_level_2: Int,
  coverage_level_3: Int) extends BlackBox(
    Map(
      "severity_level" -> severity_level,
      "width" -> width,
      "inactive" -> inactive,
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
    val test_expr = Input(UInt(width.W))
  })
}

object SVL_ASSERT_ONE_COLD {
  def apply(
    name: String,
    clock: Clock,
    reset: Reset,
    severity_level: Int,
    width: Int,
    inactive: Int,
    property_type: Int,
    msg: String,
    coverage_level_1: Int,
    coverage_level_2: Int,
    coverage_level_3: Int,
    test_expr: UInt
  ): Unit = {
    val assertOneCold = Module(new assert_one_cold(
      severity_level = severity_level,
      width = width,
      inactive = inactive,
      property_type = property_type,
      msg = msg,
      coverage_level_1 = coverage_level_1,
      coverage_level_2 = coverage_level_2,
      coverage_level_3 = coverage_level_3
    ))
    assertOneCold.io.clk := clock
    assertOneCold.io.reset_n := ~reset.asBool
    assertOneCold.io.test_expr := test_expr
    assertOneCold.suggestName(name)
  }
}
