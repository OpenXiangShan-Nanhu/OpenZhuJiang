package xs.utils.svl.basic

import chisel3._
import chisel3.util._

class assert_quiescent_state(
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
    val state_expr = Input(UInt(width.W))
    val check_value = Input(UInt(width.W))
    val sample_event = Input(Bool())
  })
}

object SVL_ASSERT_QUIESCENT_STATE {
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
    state_expr: UInt,
    check_value: UInt,
    sample_event: Bool
  ): Unit = {
    val assertQuiescentState = Module(new assert_quiescent_state(
      severity_level = severity_level,
      width = width,
      property_type = property_type,
      msg = msg,
      category = category,
      coverage_level_1 = coverage_level_1,
      coverage_level_2 = coverage_level_2,
      coverage_level_3 = coverage_level_3
    ))
    assertQuiescentState.io.clk := clock
    assertQuiescentState.io.reset_n := ~reset.asBool
    assertQuiescentState.io.state_expr := state_expr
    assertQuiescentState.io.check_value := check_value
    assertQuiescentState.io.sample_event := sample_event
    assertQuiescentState.suggestName(name)
  }
}
