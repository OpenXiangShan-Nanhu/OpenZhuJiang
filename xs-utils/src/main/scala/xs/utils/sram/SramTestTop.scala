package xs.utils.sram
import chisel3._
import chisel3.util._
import xs.utils.mbist.{MbistInterface, MbistPipeline}

class SramTestTop extends Module {
  private val dpSram = Module(new DualPortSramTemplate(
    gen = UInt(512.W),
    set = 128,
    way = 2,
    shouldReset = true,
    setup = 2,
    latency = 2,
    hasMbist = true
  ))
  private val spSram = Module(new SinglePortSramTemplate(
    gen = UInt(64.W),
    set = 128,
    way = 4,
    shouldReset = true,
    setup = 2,
    latency = 2,
    hasMbist = true
  ))

  private val io = IO(new Bundle {
    val dp = dpSram.io.cloneType
    val sp = spSram.io.cloneType
    val broadCast = new SramBroadcastBundle
  })
  io.dp <> dpSram.io
  io.sp <> spSram.io
  private val brcBd = SramHelper.genBroadCastBundleTop()
  brcBd := io.broadCast
  private val mbistPipeline = MbistPipeline.PlaceMbistPipeline(Int.MaxValue, s"MbistPipeTestTop")
  private val intf = Module(new MbistInterface(
    params = Seq(mbistPipeline.get.nodeParams),
    ids = Seq(mbistPipeline.get.childrenIds),
    name = s"MbistIntfTestTop",
    pipelineNum = 1
  ))
  mbistPipeline.get.registerCSV(intf.info, "TestTop")
  intf.toPipeline.head <> mbistPipeline.get.mbist
  intf.mbist := DontCare
  dontTouch(intf.mbist)
}
