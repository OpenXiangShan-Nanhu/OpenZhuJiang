package xs.utils.svl.basic

import chisel3._
import chisel3.util._

class assert_cycle_sequence(
  severity_level: Int,
  num_cks: Int,
  necessary_condition: Int,
  property_type: Int,
  msg: String,
  category: Int,
  coverage_level_1: Int,
  coverage_level_2: Int,
  coverage_level_3: Int) extends BlackBox(
    Map(
      "severity_level" -> severity_level,
      "num_cks" -> num_cks,
      "necessary_condition" -> necessary_condition,
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
    val event_sequence = Input(UInt(num_cks.W))
  })
}

object SVL_ASSERT_CYCLE_SEQUENCE {
  def apply(
    name: String,
    clock: Clock,
    reset: Reset,
    severity_level: Int,
    num_cks: Int,
    necessary_condition: Int,
    property_type: Int,
    msg: String,
    category: Int,
    coverage_level_1: Int,
    coverage_level_2: Int,
    coverage_level_3: Int,
    event_sequence: UInt
  ): Unit = {
    val assertCycleSequence = Module(new assert_cycle_sequence(
      severity_level = severity_level,
      num_cks = num_cks,
      necessary_condition = necessary_condition,
      property_type = property_type,
      msg = msg,
      category = category,
      coverage_level_1 = coverage_level_1,
      coverage_level_2 = coverage_level_2,
      coverage_level_3 = coverage_level_3
    ))
    assertCycleSequence.io.clk := clock
    assertCycleSequence.io.reset_n := ~reset.asBool
    assertCycleSequence.io.event_sequence := event_sequence
    assertCycleSequence.suggestName(name)
  }
}
