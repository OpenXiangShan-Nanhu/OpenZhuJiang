package xs.utils.svl.basic

import chisel3._
import chisel3.util._

class assert_no_transaction(
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
    val test_expr = Input(UInt(width.W))
    val start_state = Input(UInt(width.W))
    val next_state = Input(UInt(width.W))
  })
}

object SVL_ASSERT_NO_TRANSACTION {
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
    test_expr: UInt,
    start_state: UInt,
    next_state: UInt
  ): Unit = {
    val assertNoTransaction = Module(new assert_no_transaction(
      severity_level = severity_level,
      width = width,
      property_type = property_type,
      msg = msg,
      category = category,
      coverage_level_1 = coverage_level_1,
      coverage_level_2 = coverage_level_2,
      coverage_level_3 = coverage_level_3
    ))
    assertNoTransaction.io.clk := clock
    assertNoTransaction.io.reset_n := ~reset.asBool
    assertNoTransaction.io.test_expr := test_expr
    assertNoTransaction.io.start_state := start_state
    assertNoTransaction.io.next_state := next_state
    assertNoTransaction.suggestName(name)
  }
}
