package xs.utils.svl.basic

import chisel3._
import chisel3.util._

class assert_never(
  severity_level: Int,
  property_type: Int,
  msg: String,
  category: Int) extends BlackBox(
    Map(
      "severity_level" -> severity_level,
      "property_type" -> property_type,
      "msg" -> msg,
      "category" -> category
    )
  ) {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val reset_n = Input(Reset())
    val test_expr = Input(Bool())
  })
}

object SVL_ASSERT_NEVER {
  def apply(
    name: String,
    clock: Clock,
    reset: Reset,
    severity_level: Int,
    property_type: Int,
    msg: String,
    category: Int,
    test_expr: Bool
  ): Unit = {
    val assertNever = Module(new assert_never(
      severity_level = severity_level,
      property_type = property_type,
      msg = msg,
      category = category
    ))
    assertNever.io.clk := clock
    assertNever.io.reset_n := ~reset.asBool
    assertNever.io.test_expr := test_expr
    assertNever.suggestName(name)
  }
}
