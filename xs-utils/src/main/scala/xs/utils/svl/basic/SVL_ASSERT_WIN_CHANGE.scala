package xs.utils.svl.basic

import chisel3._
import chisel3.util._

class assert_win_change(
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

object SVL_ASSERT_WIN_CHANGE {
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
    val assertWinChange = Module(new assert_win_change(
      severity_level = severity_level,
      width = width,
      property_type = property_type,
      msg = msg,
      category = category,
      coverage_level_1 = coverage_level_1,
      coverage_level_2 = coverage_level_2,
      coverage_level_3 = coverage_level_3
    ))
    assertWinChange.io.clk := clock
    assertWinChange.io.reset_n := ~reset.asBool
    assertWinChange.io.start_event := start_event
    assertWinChange.io.test_expr := test_expr
    assertWinChange.io.end_event := end_event
    assertWinChange.suggestName(name)
  }
}
