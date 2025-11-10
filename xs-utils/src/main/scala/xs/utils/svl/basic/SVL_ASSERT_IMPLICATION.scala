package xs.utils.svl.basic

import chisel3._
import chisel3.util._

class assert_implication(
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
      "category" -> category,
      "coverage_level_1" -> coverage_level_1,
      "coverage_level_2" -> coverage_level_2,
      "coverage_level_3" -> coverage_level_3
    )
  ) {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val reset_n = Input(Reset())
    val antecedent_expr = Input(Bool())
    val consequent_expr = Input(Bool())
  })
}

object SVL_ASSERT_IMPLICATION {
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
    antecedent_expr: Bool,
    consequent_expr: Bool
  ): Unit = {
    val assertImplication = Module(new assert_implication(
      severity_level = severity_level,
      property_type = property_type,
      msg = msg,
      category = category,
      coverage_level_1 = coverage_level_1,
      coverage_level_2 = coverage_level_2,
      coverage_level_3 = coverage_level_3
    ))
    assertImplication.io.clk := clock
    assertImplication.io.reset_n := ~reset.asBool
    assertImplication.io.antecedent_expr := antecedent_expr
    assertImplication.io.consequent_expr := consequent_expr
    assertImplication.suggestName(name)
  }
}
