package xs.utils.svl.basic

import chisel3._
import chisel3.util._

class assert_proposition(
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
    val reset_n = Input(Reset())
    val test_expr = Input(Bool())
  })
}

object SVL_ASSERT_PROPOSITION {
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
    val assertProposition = Module(new assert_proposition(
      severity_level = severity_level,
      property_type = property_type,
      msg = msg,
      category = category
    ))
    assertProposition.io.reset_n := ~reset.asBool
    assertProposition.io.test_expr := test_expr
    assertProposition.suggestName(name)
  }
}
