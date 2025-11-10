package xs.utils.svl.basic

import chisel3._
import chisel3.util._

class assert_win_unchange(
  severity_level: Int,
  width: Int,
  property_type: Int,
  msg: String,
  category: Int,
  coverage_level_1: Int,
  coverage_level_2: Int,
  coverage_level_3: Int) extends BlackBox(
    Map(
      "severity_level" -> severity_level,
      "width" -> width,
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
    val end_event = Input(Bool())
  })
}

object SVL_ASSERT_WIN_UNCHANGE {
  def apply(
    name: String,
    clock: Clock,
    reset: Reset,
    severity_level: Int,
    width: Int,
    property_type: Int,
    msg: String,
    category: Int,
    coverage_level_1: Int,
    coverage_level_2: Int,
    coverage_level_3: Int,
    start_event: Bool,
    test_expr: UInt,
    end_event: Bool
  ): Unit = {
    val assertWinUnchange = Module(new assert_win_unchange(
      severity_level = severity_level,
      width = width,
      property_type = property_type,
      msg = msg,
      category = category,
      coverage_level_1 = coverage_level_1,
      coverage_level_2 = coverage_level_2,
      coverage_level_3 = coverage_level_3
    ))
    assertWinUnchange.io.clk := clock
    assertWinUnchange.io.reset_n := ~reset.asBool
    assertWinUnchange.io.start_event := start_event
    assertWinUnchange.io.test_expr := test_expr
    assertWinUnchange.io.end_event := end_event
    assertWinUnchange.suggestName(name)
  }
}
