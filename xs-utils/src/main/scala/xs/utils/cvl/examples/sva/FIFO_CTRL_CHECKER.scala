package xs.utils.cvl.examples.sva

import chisel3._
import chisel3.util._

import chisel3.ltl.Sequence.BoolSequence
import chisel3.ltl.AssumeProperty
import xs.utils.cvl.basic.CVL_ASSERT_NEXT

class FIFO_CTRL_CHECKER(depth: Int, width: Int) extends Module {
  val io = IO(new Bundle {
    val push_req = Input(Bool())
    val push_ack = Input(Bool())
    val data_in = Input(UInt(width.W))
    val pop_req = Input(Bool())
    val pop_ack = Input(Bool())
    val data_out = Input(UInt(width.W))
    val fifo_full = Input(Bool())
    val fifo_empty = Input(Bool())
  })

    val push_req_seq = BoolSequence(io.push_req && io.fifo_full)
    val fifo_full_seq = BoolSequence(io.fifo_full)
    val pop_req_seq = BoolSequence(io.pop_req)
    val fifo_empty_seq = BoolSequence(io.pop_ack)
    val data_in_stable_seq = BoolSequence(io.data_in === RegNext(io.data_in))

    AssumeProperty((push_req_seq and fifo_full_seq) |=> push_req_seq, clock = Some(clock), disable = Some(reset.asDisable), label = Some("p_push_req_stable"))
    AssumeProperty((push_req_seq and fifo_full_seq) |=> data_in_stable_seq, clock = Some(clock), disable = Some(reset.asDisable), label = Some("p_data_in_stable"))
    AssumeProperty((pop_req_seq and fifo_full_seq) |=> fifo_full_seq, clock = Some(clock), disable = Some(reset.asDisable), label = Some("p_pop_req_stable"))

    CVL_ASSERT_NEXT(
      assertEn = true,
      coverEn = false,
      cvlLongSequence = true,
      clock = clock,
      reset = reset,
      name = "p_no_underflow",
      num_cks = 0,
      start_event = io.fifo_empty,
      test_expr = !io.pop_ack
    )

    CVL_ASSERT_NEXT(
      assertEn = true,
      coverEn = false,
      cvlLongSequence = true,
      clock = clock,
      reset = reset,
      name = "p_no_overflow",
      num_cks = 0,
      start_event = io.fifo_full,
      test_expr = !io.push_ack
    )
}
