package xs.utils.svl.basic

import chisel3._
import chisel3.util._

class assert_handshake(
  severity_level: Int,
  min_ack_cycle: Int,
  max_ack_cycle: Int,
  req_drop: Int,
  deassert_count: Int,
  max_ack_length: Int,
  property_type: Int,
  msg: String,
  category: Int,
  coverage_level_1: Int,
  coverage_level_2: Int,
  coverage_level_3: Int) extends BlackBox(
    Map(
      "severity_level" -> severity_level,
      "min_ack_cycle" -> min_ack_cycle,
      "max_ack_cycle" -> max_ack_cycle,
      "req_drop" -> req_drop,
      "deassert_count" -> deassert_count,
      "max_ack_length" -> max_ack_length,
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
    val req = Input(Bool())
    val ack = Input(Bool())
  })
}

object SVL_ASSERT_HANDSHAKE {
  def apply(
    name: String,
    clock: Clock,
    reset: Reset,
    severity_level: Int,
    min_ack_cycle: Int,
    max_ack_cycle: Int,
    req_drop: Int,
    deassert_count: Int,
    max_ack_length: Int,
    property_type: Int,
    msg: String,
    category: Int,
    coverage_level_1: Int,
    coverage_level_2: Int,
    coverage_level_3: Int,
    req: Bool,
    ack: Bool
  ): Unit = {
    val assertHandshake = Module(new assert_handshake(
      severity_level = severity_level,
      min_ack_cycle = min_ack_cycle,
      max_ack_cycle = max_ack_cycle,
      req_drop = req_drop,
      deassert_count = deassert_count,
      max_ack_length = max_ack_length,
      property_type = property_type,
      msg = msg,
      category = category,
      coverage_level_1 = coverage_level_1,
      coverage_level_2 = coverage_level_2,
      coverage_level_3 = coverage_level_3
    ))
    assertHandshake.io.clk := clock
    assertHandshake.io.reset_n := ~reset.asBool
    assertHandshake.io.req := req
    assertHandshake.io.ack := ack
    assertHandshake.suggestName(name)
  }
}
