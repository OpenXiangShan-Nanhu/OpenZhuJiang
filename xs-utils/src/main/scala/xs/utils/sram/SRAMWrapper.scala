package xs.utils.sram

import chisel3._
import chisel3.util._
import xs.utils.mbist.MbistPipeline

class SRAMWrapper[T <: Data](
  gen: T,
  set: Int,
  n: Int = 1,
  multicycle: Int = 1,
  hasMbist: Boolean = false,
  powerCtl: Boolean = false,
)
  extends Module {
  val io = IO(new Bundle() {
    val r = Flipped(new SRAMReadBus(gen, set, 1))
    val w = Flipped(new SRAMWriteBus(gen, set, 1))
    val pwctl = if(powerCtl) Some(new SramPowerCtl) else None
  })
  require(set % n == 0)
  require((1 << log2Ceil(n)) == n)
  require((1 << log2Ceil(set)) == set)

  private val innerSet = set / n
  private val selBits = log2Ceil(n)
  private val innerSetBits = log2Up(set) - selBits
  private val r_setIdx = io.r.req.bits.setIdx(innerSetBits - 1, 0)
  private val r_sel = if(n == 1) 0.U else io.r.req.bits.setIdx(innerSetBits + selBits - 1, innerSetBits)
  private val w_setIdx = io.w.req.bits.setIdx(innerSetBits - 1, 0)
  private val w_sel = if(n == 1) 0.U else io.w.req.bits.setIdx(innerSetBits + selBits - 1, innerSetBits)

  val banks: Seq[SRAMTemplate[T]] = (0 until n).map { i =>
    val ren = if(n == 1) true.B else i.U === r_sel
    val wen = if(n == 1) true.B else i.U === w_sel
    val sram = Module(
      new SRAMTemplate[T](
        gen,
        innerSet,
        1,
        singlePort = true,
        setup = multicycle,
        hasMbist = hasMbist
      )
    )
    sram.clock := clock
    sram.io.r.req.valid := io.r.req.valid && ren
    sram.io.r.req.bits.apply(r_setIdx)
    sram.io.w.req.valid := io.w.req.valid && wen
    sram.io.w.req.bits.apply(io.w.req.bits.data(0), w_setIdx, 1.U)
    if(powerCtl) sram.io.pwctl.get := io.pwctl.get
    sram
  }
  private val mbistPl = MbistPipeline.PlaceMbistPipeline(1, place = hasMbist)
  private val renBd = Wire(Valid(UInt(n.W)))
  renBd.valid := banks.map(_.io.r.req.fire).reduce(_ | _)
  renBd.bits := Cat(banks.map(_.io.r.req.fire).reverse)
  private val finalRenBd = Pipe(renBd, multicycle)
  io.r.req.ready := Cat(banks.map(_.io.r.req.ready)).andR
  io.r.resp.data := Mux1H(finalRenBd.bits & Fill(n, finalRenBd.valid), banks.map(_.io.r.resp.data))
  io.w.req.ready := Cat(banks.map(_.io.w.req.ready)).andR
}
