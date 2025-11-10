package xs.utils.test

import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util._
import firrtl.AnnotationSeq
import org.chipsalliance.cde.config.{Config, Parameters}
import xs.utils.FileRegisters
import xs.utils.debug.{HAssert, HardwareAssertionKey, HwaParams}
import xs.utils.stage.XsStage
import xs.utils.cvl.examples.design._
import xs.utils.cvl.examples.sva._
import chisel3.probe._

class CVL_FORMAL_AIP_TB extends Module {
  val io = IO(new Bundle {
    val push_req = Input(Bool())
    val push_ack = Output(Bool())
    val data_in = Input(UInt(8.W))
    val pop_req = Input(Bool())
    val pop_ack = Output(Bool())
    val data_out = Output(UInt(8.W))
  })
  val fifo_ctrl = Module(new FIFO_CTRL(4, 8)(useCVL = false, useExternSVA = false, useAIP = true))
  val fifo_ctrl_chk = Module(new FIFO_CTRL_CHECKER(4, 8))
  fifo_ctrl_chk.io.push_req := read(fifo_ctrl.probeWire.push_req)
  fifo_ctrl_chk.io.push_ack := read(fifo_ctrl.probeWire.push_ack)
  fifo_ctrl_chk.io.pop_req := read(fifo_ctrl.probeWire.pop_req)
  fifo_ctrl_chk.io.pop_ack := read(fifo_ctrl.probeWire.pop_ack)
  fifo_ctrl_chk.io.data_in := read(fifo_ctrl.probeWire.data_in)
  fifo_ctrl_chk.io.data_out := read(fifo_ctrl.probeWire.data_out)
  fifo_ctrl_chk.io.fifo_full := read(fifo_ctrl.probeWire.fifo_full)
  fifo_ctrl_chk.io.fifo_empty := read(fifo_ctrl.probeWire.fifo_empty)

  io <> fifo_ctrl.io
}

object CVL_FORMAL_AIP_TestTop extends App {
  val (config, firrtlOpts) = Parser(args)
  (new XsStage).execute(firrtlOpts, AnnotationSeq(TestTopHelper.firtoolOpts) :+ ChiselGeneratorAnnotation(() => new CVL_FORMAL_AIP_TB))
}
