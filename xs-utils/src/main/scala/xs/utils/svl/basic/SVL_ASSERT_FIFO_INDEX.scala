package xs.utils.svl.basic

import chisel3._
import chisel3.util._

class assert_fifo_index(
  severity_level: Int,
  depth: Int,
  push_width: Int,
  pop_width: Int,
  property_type: Int,
  msg: String,
  category: Int,
  coverage_level_1: Int,
  coverage_level_2: Int,
  coverage_level_3: Int) extends BlackBox(
    Map(
      "severity_level" -> severity_level,
      "depth" -> depth,
      "push_width" -> push_width,
      "pop_width" -> pop_width,
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
    val push = Input(Bool())
    val pop = Input(Bool())
  })
}

object SVL_ASSERT_FIFO_INDEX {
  def apply(
    name: String,
    clock: Clock,
    reset: Reset,
    severity_level: Int,
    depth: Int,
    push_width: Int,
    pop_width: Int,
    property_type: Int,
    msg: String,
    category: Int,
    coverage_level_1: Int,
    coverage_level_2: Int,
    coverage_level_3: Int,
    push: Bool,
    pop: Bool
  ): Unit = {
    val assertFifoIndex = Module(new assert_fifo_index(
      severity_level = severity_level,
      depth = depth,
      push_width = push_width,
      pop_width = pop_width,
      property_type = property_type,
      msg = msg,
      category = category,
      coverage_level_1 = coverage_level_1,
      coverage_level_2 = coverage_level_2,
      coverage_level_3 = coverage_level_3
    ))
    assertFifoIndex.io.clk := clock
    assertFifoIndex.io.reset_n := ~reset.asBool
    assertFifoIndex.io.push := push
    assertFifoIndex.io.pop := pop
    assertFifoIndex.suggestName(name)
  }
}
