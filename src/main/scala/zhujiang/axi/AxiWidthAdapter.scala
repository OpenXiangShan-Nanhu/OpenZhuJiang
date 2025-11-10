package zhujiang.axi

import chisel3._
import chisel3.experimental.noPrefix
import chisel3.util._
import xs.utils.{CircularQueuePtr, HasCircularQueuePtrHelper}
import xs.utils.PickOneLow

class AxiWidthAdapterWBundle(axiP: AxiParams) extends Bundle {
    require(axiP.addrBits > 12)
    val addr_sfx = UInt(12.W)
    val size     = UInt(axiP.sizeBits.W)
    val id       = UInt(axiP.idBits.W)
    val byteMask = UInt(12.W)

    def :=(in: AWFlit): Unit = {
        this.addr_sfx := in.addr(11, 0)
        this.size     := in.size
        this.id       := in.id
        this.byteMask := PriorityMux(
            Seq(
                AXI.isIncr(in.burst) -> 0xfff.U,
                AXI.isWrap(in.burst) -> (AXI.byteMask(in.len, in.size)),
                true.B               -> 0.U
            )
        )
    }
}

object AXI {
    def FIX = "b00".U
    def INCR = "b01".U
    def WRAP = "b10".U
    def RSV = "b11".U

    def isFix(burst: UInt): Bool = {
        !(burst.orR)
    }

    def isIncr(burst: UInt): Bool = {
        burst(0).asBool
    }

    def isWrap(burst: UInt): Bool = {
        burst(1).asBool
    }

    def byteMask(len: UInt, size: UInt) = {
        val maxShift = 1 << 3
        val tail     = ((BigInt(1) << maxShift) - 1).U
        (Cat(len, tail) << size) >> maxShift
    }
    def nextAddr(addr: UInt, size: UInt, byteMask: UInt): UInt = {
        (~byteMask & addr | (addr + (1.U(6.W) << size)) & byteMask)
    }
}

class AxiWidthAdapterRBundle(axiP: AxiParams, outstanding: Int) extends Bundle {
    val addr_sfx = UInt(12.W)
    val size     = UInt(axiP.sizeBits.W)
    val id       = UInt(axiP.idBits.W)
    val nid      = UInt(log2Ceil(outstanding).W)
    val byteMask = UInt(12.W)

    def :=(in: ARFlit): Unit = {
        this.addr_sfx := in.addr(11, 0)
        this.size     := in.size
        this.id       := in.id
        this.byteMask := PriorityMux(
            Seq(
                AXI.isIncr(in.burst) -> 0xfff.U,
                AXI.isWrap(in.burst) -> (AXI.byteMask(in.len, in.size)),
                true.B               -> 0.U
            )
        )
    }
}

class AxiWidthAdapter(slvParams: AxiParams, mstParams: AxiParams, outstanding: Int) extends Module with HasCircularQueuePtrHelper {
    private class CirQAxiEntryPtr extends CircularQueuePtr[CirQAxiEntryPtr](outstanding)
    private object CirQAxiEntryPtr {
        def apply(f: Bool, v: UInt): CirQAxiEntryPtr = {
            val ptr = Wire(new CirQAxiEntryPtr)
            ptr.flag  := f
            ptr.value := v
            ptr
        }
    }
    val io = IO(new Bundle {
        val mst = Flipped(new AxiBundle(mstParams))
        val slv = new AxiBundle(slvParams)
    })
    private val mdw = mstParams.dataBits
    private val sdw = slvParams.dataBits
    private val seg = sdw / mdw

    private val awinfo        = Reg(Vec(outstanding, new AxiWidthAdapterWBundle(mstParams)))
    private val wHeadPtr      = RegInit(CirQAxiEntryPtr(f = false.B, v = 0.U))
    private val wTailPtr      = RegInit(CirQAxiEntryPtr(f = false.B, v = 0.U))
    private val wq            = Module(new Queue(new WFlit(mstParams), entries = 2))
    private val rq            = Module(new Queue(new RFlit(slvParams), entries = 1, pipe = true))
    private val arvld         = RegInit(VecInit(Seq.fill(outstanding)(false.B)))
    private val arinfo        = Reg(Vec(outstanding, new AxiWidthAdapterRBundle(mstParams, outstanding)))
    private val arsel         = PickOneLow(arvld)
    private val infoSelOH     = Wire(Vec(outstanding, Bool()))
    private val nidCalcVec    = Wire(Vec(outstanding, Bool()))
    private val rawNid        = PopCount(nidCalcVec)
    private val cncrtWkVld    = io.mst.r.fire && io.mst.ar.fire && io.mst.r.bits._last && io.mst.r.bits.id === io.mst.ar.bits.id
    private val cncrtWkVldReg = RegNext(cncrtWkVld)
    private val cncrtWkEtrReg = RegEnable(arsel.bits, cncrtWkVld)
    require(mdw <= sdw, "AXI width adapter dose not support wide-to-narrow convert for now!")

    for (i <- arvld.indices) noPrefix {
        val rFireMayHit = WireInit(io.mst.r.valid && io.mst.r.ready && io.mst.r.bits.id === arinfo(i).id && arvld(i))
        rFireMayHit.suggestName(s"r_fire_may_hit_$i")
        val arFireHit = WireInit(io.mst.ar.fire && arsel.bits(i))
        arFireHit.suggestName(s"ar_fire_hit_$i")

        when(arFireHit) {
            arvld(i) := true.B
        }.elsewhen(rFireMayHit && io.mst.r.bits._last && arinfo(i).nid === 0.U) {
            arvld(i) := false.B
        }

        when(arFireHit) {
            arinfo(i) := io.mst.ar.bits
        }

        when(arFireHit) {
            arinfo(i).nid := rawNid
        }.elsewhen((cncrtWkVldReg && cncrtWkEtrReg(i)) && (rFireMayHit && io.mst.r.bits._last && arinfo(i).nid =/= 0.U)) {
            arinfo(i).nid := arinfo(i).nid - 2.U
        }.elsewhen((cncrtWkVldReg && cncrtWkEtrReg(i)) || (rFireMayHit && io.mst.r.bits._last && arinfo(i).nid =/= 0.U)) {
            arinfo(i).nid := arinfo(i).nid - 1.U
        }

        when(rFireMayHit && arinfo(i).nid === 0.U) {
            arinfo(i).addr_sfx := AXI.nextAddr(arinfo(i).addr_sfx, arinfo(i).size, arinfo(i).byteMask)
        }

        infoSelOH(i)  := arvld(i) && arinfo(i).id === io.mst.r.bits.id && arinfo(i).nid === 0.U
        nidCalcVec(i) := arvld(i) && arinfo(i).id === io.mst.ar.bits.id
    }

    when(io.mst.aw.fire) {
        awinfo(wHeadPtr.value) := io.mst.aw.bits
        wHeadPtr               := wHeadPtr + 1.U
    }
    io.slv.aw.valid := io.mst.aw.valid && !isFull(wHeadPtr, wTailPtr)
    io.slv.aw.bits  := io.mst.aw.bits
    io.mst.aw.ready := io.slv.aw.ready && !isFull(wHeadPtr, wTailPtr)

    io.slv.ar.valid := io.mst.ar.valid && arsel.valid
    io.slv.ar.bits  := io.mst.ar.bits
    io.mst.ar.ready := io.slv.ar.ready && arsel.valid

    private val strb     = Wire(Vec(seg, UInt((mdw / 8).W)))
    private val waddrcvt = if (sdw > mdw) awinfo(wTailPtr.value).addr_sfx(log2Ceil(sdw / 8) - 1, log2Ceil(mdw / 8)) else 0.U
    strb.zipWithIndex.foreach({ case (s, i) => s := Mux(waddrcvt === i.U, wq.io.deq.bits.strb, 0.U) })

    wq.io.enq          <> io.mst.w
    io.slv.w.valid     := wq.io.deq.valid && !isEmpty(wHeadPtr, wTailPtr)
    io.slv.w.bits      := wq.io.deq.bits
    io.slv.w.bits.data := Fill(seg, wq.io.deq.bits.data)
    io.slv.w.bits.strb := strb.asUInt
    wq.io.deq.ready    := io.slv.w.ready && !isEmpty(wHeadPtr, wTailPtr)
    val awInfoTail = awinfo(wTailPtr.value)
    when(wq.io.deq.valid && io.slv.w.ready && !isEmpty(wHeadPtr, wTailPtr)) {
        awInfoTail.addr_sfx := AXI.nextAddr(awInfoTail.addr_sfx, awInfoTail.size, awInfoTail.byteMask)
    }
    when(wq.io.deq.valid && io.slv.w.ready && wq.io.deq.bits._last && !isEmpty(wHeadPtr, wTailPtr)) {
        wTailPtr := wTailPtr + 1.U
    }

    io.mst.b <> io.slv.b

    private val infoSel  = Mux1H(infoSelOH, arinfo)
    private val raddrcvt = if (sdw > mdw) infoSel.addr_sfx(log2Ceil(sdw / 8) - 1, log2Ceil(mdw / 8)) else 0.U
    private val rdata    = rq.io.deq.bits.data.asTypeOf(Vec(seg, UInt(mdw.W)))
    rq.io.enq          <> io.slv.r
    io.mst.r           <> rq.io.deq
    io.mst.r.bits.data := rdata(raddrcvt)

    when(io.mst.r.fire) {
        assert(PopCount(infoSelOH) === 1.U, s"Multiple R entries are hit!")
    }
}

object AxiWidthAdapter {
    def apply(slv: AxiBundle, mst: AxiBundle, outstanding: Int): AxiWidthAdapter = {
        val wadpt = Module(new AxiWidthAdapter(slv.params, mst.params, outstanding))
        wadpt.io.mst <> mst
        slv          <> wadpt.io.slv
        wadpt
    }
    def apply(slv: AxiBundle, mst: AxiBundle): AxiWidthAdapter = apply(slv, mst, 4)
}
