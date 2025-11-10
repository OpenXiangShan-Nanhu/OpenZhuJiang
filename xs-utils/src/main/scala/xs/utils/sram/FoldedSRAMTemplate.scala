package xs.utils.sram

import chisel3._
import chisel3.util._
import xs.utils.HoldUnless

class FoldedSRAMTemplate[T <: Data](
  gen: T,
  set: Int,
  way: Int = 1,
  width: Int = 4,
  singlePort: Boolean = false,
  shouldReset: Boolean = false,
  extraReset: Boolean = false,
  holdRead: Boolean = false,
  bypassWrite: Boolean = false,
  useBitmask:  Boolean = false,
  multicycle: Int = 1,
  hasMbist: Boolean = false,
  powerCtl: Boolean = false,
  foundry: String = "Unknown",
  sramInst: String = "STANDARD",
  suffix: String = "")
  extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way, useBitmask))
    val pwctl = if(powerCtl) Some(new SramPowerCtl) else None
  })
  val extra_reset = if(extraReset) Some(IO(Input(Bool()))) else None
  //   |<----- setIdx ----->|
  //   | ridx | width | way |

  require(width > 0 && isPow2(width))
  require(way > 0 && isPow2(way))
  require(set % width == 0)

  private val nRows = set / width

  private val array = Module(
    new SRAMTemplate(
      gen,
      set = nRows,
      way = width * way,
      shouldReset = shouldReset,
      extraReset = extraReset,
      holdRead = holdRead,
      bypassWrite = bypassWrite,
      singlePort = singlePort,
      setup = multicycle,
      hasMbist = hasMbist,
      useBitmask = useBitmask,
      powerCtl = powerCtl,
      foundry = foundry,
      sramInst = sramInst,
      suffix = suffix
    )
  )
  if(array.extra_reset.isDefined) array.extra_reset.get := extra_reset.get
  if(powerCtl) array.io.pwctl.get := io.pwctl.get

  io.r.req.ready := array.io.r.req.ready
  io.w.req.ready := array.io.w.req.ready

  private val raddr = io.r.req.bits.setIdx >> log2Ceil(width)
  private val ridx = RegNext(if(width != 1) io.r.req.bits.setIdx(log2Ceil(width) - 1, 0) else 0.U(1.W))
  private val ren = io.r.req.valid

  array.io.r.req.valid := ren
  array.io.r.req.bits.setIdx := raddr

  private val rdata = array.io.r.resp.data
  for(w <- 0 until way) {
    val wayData = VecInit(rdata.indices.filter(_ % way == w).map(rdata(_)))
    val holdRidx = HoldUnless(ridx, RegNext(io.r.req.valid))
    val realRidx = if(holdRead) holdRidx else ridx
    io.r.resp.data(w) := Mux1H(UIntToOH(realRidx, width), wayData)
  }
  io.r.resp.valid := array.io.r.resp.valid

  private val wen = io.w.req.valid
  private val wdata = VecInit(Seq.fill(width)(io.w.req.bits.data).flatten)
  private val waddr = io.w.req.bits.setIdx >> log2Ceil(width)
  private val widthIdx = if(width != 1) io.w.req.bits.setIdx(log2Ceil(width) - 1, 0) else 0.U
  private val wmask = (width, way) match {
    case (1, 1) => 1.U(1.W)
    case (x, 1) => UIntToOH(widthIdx)
    case _ =>
      VecInit(Seq.tabulate(width * way)(n => (n / way).U === widthIdx && io.w.req.bits.waymask.get(n % way))).asUInt
  }
  require(wmask.getWidth == way * width)
  
  if (useBitmask) {
    array.io.w.apply(wen, wdata, waddr, wmask, io.w.req.bits.bitmask.get)
  } else {
    array.io.w.apply(wen, wdata, waddr, wmask)
  }
}
