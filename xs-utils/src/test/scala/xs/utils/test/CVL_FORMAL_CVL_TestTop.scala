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

object CVL_FORMAL_CVL_TestTop extends App {
  val (config, firrtlOpts) = Parser(args)
  (new XsStage).execute(firrtlOpts, AnnotationSeq(TestTopHelper.firtoolOpts) :+ ChiselGeneratorAnnotation(() => new FIFO_CTRL(4, 8)(useCVL = true, useExternSVA = false, useAIP = false)))
}
