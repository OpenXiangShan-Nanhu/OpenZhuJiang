package xs.utils.cvl.examples.design

import chisel3._
import chisel3.util._

class FIFO_CTRL(depth: Int, width: Int)(useCVL: Boolean, useExternSVA: Boolean, useAIP: Boolean) extends Module {
  val io = IO(new Bundle {
    val push_req = Input(Bool())
    val push_ack = Output(Bool())
    val data_in = Input(UInt(width.W))
    val pop_req = Input(Bool())
    val pop_ack = Output(Bool())
    val data_out = Output(UInt(width.W))
  })
  
  val ptrWidth = log2Up(depth)

  val wptr = RegInit(0.U(ptrWidth.W))
  val rptr = RegInit(0.U(ptrWidth.W))
  val fifo_cnt = RegInit(0.U(log2Up(depth + 1).W))
  val data = Reg(Vec(depth, UInt(width.W)))

  val wptr_next = Wire(UInt(ptrWidth.W))
  val rptr_next = Wire(UInt(ptrWidth.W))
  val fifo_full = Wire(Bool())
  val fifo_empty = Wire(Bool())
  val fifo_wren = Wire(Bool())
  val fifo_rden = Wire(Bool())

  wptr_next := Mux(wptr === (depth - 1).U, 0.U, wptr + 1.U)
  rptr_next := Mux(rptr === (depth - 1).U, 0.U, rptr + 1.U)

  wptr := wptr_next
  rptr := rptr_next

  fifo_empty := (fifo_cnt === 0.U)
  fifo_full := (fifo_cnt === depth.U)

  fifo_wren := io.push_req && !fifo_full
  fifo_rden := io.pop_req && !fifo_empty

  fifo_cnt := fifo_cnt + fifo_wren.asUInt - fifo_rden.asUInt

  data.zipWithIndex.foreach { case (d, i) =>
    when(io.push_req && fifo_cnt < depth.U && wptr === i.U) {
      d := io.data_in
    }
  }

  io.push_ack := ~fifo_full
  io.pop_ack := ~fifo_empty
  io.data_out := data(rptr)

  if(useCVL) {
    import chisel3.ltl.Sequence.BoolSequence
    import chisel3.ltl.AssumeProperty
    import xs.utils.cvl.basic.CVL_ASSERT_NEXT

    val push_req_seq = BoolSequence(io.push_req && fifo_full)
    val fifo_full_seq = BoolSequence(fifo_full)
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
      start_event = fifo_empty,
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
      start_event = fifo_full,
      test_expr = !io.push_ack
    )
  }

  // Probe
  import chisel3.probe._
  val probeWire = IO(Output(new Bundle {
    val push_req = Probe(Bool())
    val push_ack = Probe(Bool())
    val data_in = Probe(UInt(width.W))
    val pop_req = Probe(Bool())
    val pop_ack = Probe(Bool())
    val data_out = Probe(UInt(width.W))
    val fifo_full = Probe(Bool())
    val fifo_empty = Probe(Bool())
  }))
  define(probeWire.push_req, ProbeValue(io.push_req))
  define(probeWire.push_ack, ProbeValue(io.push_ack))
  define(probeWire.pop_req, ProbeValue(io.pop_req))
  define(probeWire.pop_ack, ProbeValue(io.pop_ack))
  define(probeWire.data_in, ProbeValue(io.data_in))
  define(probeWire.data_out, ProbeValue(io.data_out))
  define(probeWire.fifo_full, ProbeValue(fifo_full))
  define(probeWire.fifo_empty, ProbeValue(fifo_empty))
}