package xs.utils.svl.basic

import chisel3._
import chisel3.util._

class assert_always(
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

object SVL_ASSERT_ALWAYS {
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
    test_expr: Bool
  ): Unit = {
    val assertAlways = Module(new assert_always(
      severity_level = severity_level,
      property_type = property_type,
      msg = msg,
      category = category,
      coverage_level_1 = coverage_level_1,
      coverage_level_2 = coverage_level_2,
      coverage_level_3 = coverage_level_3
    ))
    assertAlways.io.clk := clock
    assertAlways.io.reset_n := ~reset.asBool
    assertAlways.io.test_expr := test_expr
    assertAlways.suggestName(name)
  }
}