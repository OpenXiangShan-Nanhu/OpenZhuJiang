package xs.utils.svl.basic

import chisel3._
import chisel3.util._

class assert_always_on_edge(
  severity_level: Int,
  edge_type: Int,
  property_type: Int,
  msg: String,
  category: Int,
  coverage_level_1: Int,
  coverage_level_2: Int,
  coverage_level_3: Int) extends BlackBox(
    Map(
      "severity_level" -> severity_level,
      "edge_type" -> edge_type,
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
    val sampling_event = Input(Bool())
    val test_expr = Input(Bool())
  })
}

object SVL_ASSERT_ALWAYS_ON_EDGE {
  def apply(
    name: String,
    clock: Clock,
    reset: Reset,
    severity_level: Int,
    edge_type: Int,
    property_type: Int,
    msg: String,
    category: Int,
    coverage_level_1: Int,
    coverage_level_2: Int,
    coverage_level_3: Int,
    sampling_event: Bool,
    test_expr: Bool
  ): Unit = {
    val assertAlwaysOnEdge = Module(new assert_always_on_edge(
      severity_level = severity_level,
      edge_type = edge_type,
      property_type = property_type,
      msg = msg,
      category = category,
      coverage_level_1 = coverage_level_1,
      coverage_level_2 = coverage_level_2,
      coverage_level_3 = coverage_level_3
    ))
    assertAlwaysOnEdge.io.clk := clock
    assertAlwaysOnEdge.io.reset_n := ~reset.asBool
    assertAlwaysOnEdge.io.sampling_event := sampling_event
    assertAlwaysOnEdge.io.test_expr := test_expr
    assertAlwaysOnEdge.suggestName(name)
  }
}