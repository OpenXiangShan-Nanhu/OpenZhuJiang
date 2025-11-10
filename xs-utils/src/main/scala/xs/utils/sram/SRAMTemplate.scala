/** *************************************************************************************
 * Copyright (c) 2020 Institute of Computing Technology, CAS
 * Copyright (c) 2020 University of Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * NutShell is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 * http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 * *************************************************************************************
 */

// See LICENSE.SiFive for license details.

package xs.utils.sram

import chisel3._
import chisel3.util._
import xs.utils.mbist.MbistClockGateCell

class SRAMBundleA(val set: Int) extends Bundle {
  val setIdx = Output(UInt(log2Up(set).W))

  def apply(setIdx: UInt): SRAMBundleA = {
    this.setIdx := setIdx
    this
  }
}

class SRAMBundleAW[T <: Data](
  private val gen: T,
  set: Int,
  val way: Int = 1,
  val useBitmask: Boolean = false )
  extends SRAMBundleA(set) {
  private val dataWidth = gen.getWidth
  val data:    Vec[T] = Output(Vec(way, gen))
  val waymask: Option[UInt] = if (way > 1) Some(Output(UInt(way.W))) else None
  // flattened_bitmask is the flattened form of [waymask, bitmask], can be use directly to mask memory
  val flattened_bitmask: Option[UInt] = if (useBitmask) Some(Output(UInt((way * dataWidth).W))) else None
  // bitmask is the original bitmask passed from parameter
  val bitmask: Option[UInt] = if (useBitmask) Some(Output(UInt((dataWidth).W))) else None

  def apply(data: Vec[T], setIdx: UInt, waymask: UInt): SRAMBundleAW[T] = {
    require(
      waymask.getWidth == way,
      s"waymask width does not equal nWays, waymask width: ${waymask.getWidth}, nWays: ${way}"
    )
    super.apply(setIdx)
    this.data := data
    this.waymask.foreach(_ := waymask)
    this
  }

  def apply(data: Vec[T], setIdx: UInt, waymask: UInt, bitmask: UInt): SRAMBundleAW[T] = {
    require(useBitmask, "passing bitmask when not using bitmask")
    require(
      bitmask.getWidth == dataWidth,
      s"bitmask width does not equal data width, bitmask width: ${bitmask.getWidth}, data width: ${dataWidth}"
    )
    apply(data, setIdx, waymask)
    this.flattened_bitmask.foreach(
      _ :=
        VecInit.tabulate(way * dataWidth)(n => waymask(n / dataWidth) && bitmask(n % dataWidth)).asUInt
    )
    this.bitmask.foreach(_ := bitmask)
    this
  }

  // this could only be used when waymask is onehot or nway is 1
  def apply(data: T, setIdx: UInt, waymask: UInt): SRAMBundleAW[T] = {
    apply(VecInit(Seq.fill(way)(data)), setIdx, waymask)
    this
  }

  def apply(data: T, setIdx: UInt, waymask: UInt, bitmask: UInt): SRAMBundleAW[T] = {
    apply(VecInit(Seq.fill(way)(data)), setIdx, waymask, bitmask)
    this
  }
}

class SRAMBundleR[T <: Data](private val gen: T, val way: Int = 1) extends Bundle {
  val data = Output(Vec(way, gen))
  val valid = Output(Bool())
}

class SRAMReadBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleA(set))
  val resp = Flipped(new SRAMBundleR(gen, way))

  def apply(valid: Bool, setIdx: UInt): SRAMReadBus[T] = {
    this.req.bits.apply(setIdx)
    this.req.valid := valid
    this
  }
}

class SRAMWriteBus[T <: Data](
  private val gen: T,
  val set:         Int,
  val way:         Int = 1,
  val useBitmask:  Boolean = false)
  extends Bundle {
  val req = Decoupled(new SRAMBundleAW(gen, set, way, useBitmask))

  def apply(valid: Bool, data: Vec[T], setIdx: UInt, waymask: UInt): SRAMWriteBus[T] = {
    this.req.bits.apply(data = data, setIdx = setIdx, waymask = waymask)
    this.req.valid := valid
    this
  }

  def apply(valid: Bool, data: Vec[T], setIdx: UInt, waymask: UInt, bitmask: UInt): SRAMWriteBus[T] = {
    this.req.bits.apply(data = data, setIdx = setIdx, waymask = waymask, bitmask = bitmask)
    this.req.valid := valid
    this
  }

  def apply(valid: Bool, data: T, setIdx: UInt, waymask: UInt): SRAMWriteBus[T] = {
    apply(valid, VecInit(Seq.fill(way)(data)), setIdx, waymask)
    this
  }

  def apply(valid: Bool, data: T, setIdx: UInt, waymask: UInt, bitmask: UInt): SRAMWriteBus[T] = {
    apply(valid, VecInit(Seq.fill(way)(data)), setIdx, waymask, bitmask)
    this
  }
}

class SramPowerCtl extends Bundle {
  val deact = Input(Bool())
  val ret = Input(Bool())
  val stop = Input(Bool())
}

// WARNING: this SRAMTemplate assumes the SRAM lib itself supports holdRead.
class SRAMTemplate[T <: Data](
  gen: T,
  set: Int,
  way: Int = 1,
  singlePort: Boolean = false,
  shouldReset: Boolean = false,
  extraReset: Boolean = false,
  holdRead: Boolean = false,
  bypassWrite: Boolean = false,
  useBitmask:  Boolean = false,
  setup:Int = 1, // ask your leader to changed this
  latency: Int = 1, // ask your leader to changed this
  extraHold: Boolean = false,  //ask your leader to changed this
  hasMbist: Boolean = false,
  explictBist:Boolean = false,
  suffix: String = "",
  powerCtl: Boolean = false,
  pipeDepth:  Int = 0,
  explicitHold: Boolean = false,
  val foundry: String = "Unknown",
  val sramInst: String = "STANDARD")
  extends Module {
  require(!(explicitHold && extraHold))
  private val inputMcp = setup > 1 || extraHold
  private val actualWay = if (useBitmask) gen.getWidth * way else way
  private val elementWidth = if (useBitmask) 1 else gen.getWidth
  val sp = SramInfo(elementWidth, actualWay, hasMbist || explictBist)
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way, useBitmask))
    val pwctl = if(powerCtl) Some(new SramPowerCtl) else None
    val broadcast = if(explictBist) Some(new SramBroadcastBundle) else None
  })
  require(latency >= 1)
  require(setup >= 1)
  private val pwctl = if(powerCtl || setup > 1) {
    Some(Wire(new GenericSramPowerCtl))
  } else {
    None
  }
  private val isc = if(extraHold) setup + 1 else setup // input steady cycles
  private val rcg = Module(new MbistClockGateCell(isc > 1))
  private val wcg = if(!singlePort) Some(Module(new MbistClockGateCell(isc > 1))) else None
  private val dataWidth = gen.getWidth * way
  private val (mbistBd, array, vname) = SramHelper.genRam(
    sp = sp,
    set = set,
    dp = !singlePort,
    setup = setup,
    hold = if(extraHold || explicitHold) setup else setup - 1,
    latency = latency,
    bist = hasMbist,
    extraHold = extraHold,
    broadcast = io.broadcast,
    pwctl = pwctl,
    rclk = rcg.out_clock,
    wclk = wcg.map(_.out_clock),
    suffix = suffix,
    foundry = foundry,
    sramInst = sramInst,
    pipeDepth = pipeDepth,
    template = this
  )
  private val brcBd = io.broadcast.getOrElse(mbistBd.broadcast)
  val sramName: String = vname
  if(extraReset) require(shouldReset)

  private val resetState = WireInit(false.B)
  private val resetAddr = WireInit(0.U(log2Ceil(set + 1).W))
  private val resetWen = WireInit(false.B)

  val extra_reset = if(extraReset) Some(IO(Input(Bool()))) else None
  if(shouldReset) {
    val resetGen = Module(new SramResetGen(set = set, interval = isc, extraReset = extraReset))
    resetGen.clock := clock
    resetGen.reset := reset
    resetGen.io.extraRst.foreach(_ := extra_reset.get)
    resetState := resetGen.io.resetState
    resetAddr := resetGen.io.waddr
    resetWen := resetGen.io.wen
  }
  private val enableBypass = bypassWrite && !singlePort
  private val concurrentRW = io.w.req.fire && io.r.req.fire && io.w.req.bits.setIdx === io.r.req.bits.setIdx
  private val doBypass = if(enableBypass) Some(concurrentRW) else None

  private val wen = Mux(resetState, resetWen, io.w.req.fire)
  private val _wmask =
    if (useBitmask) io.w.req.bits.flattened_bitmask.getOrElse("b1".U) else io.w.req.bits.waymask.getOrElse("b1".U)
  private val wmask = Mux(resetState, Fill(actualWay, true.B), _wmask)
  private val wdata = Mux(resetState, 0.U, io.w.req.bits.data.asUInt)
  private val waddr = Mux(resetState, resetAddr, io.w.req.bits.setIdx)
  private val ren = io.r.req.fire
  private val raddr = io.r.req.bits.setIdx

  private val ramWen = if(hasMbist) Mux(mbistBd.ack, mbistBd.we, wen) else wen
  private val ramWaddr = if(hasMbist & singlePort) {
    Mux(mbistBd.ack, mbistBd.addr_rd, waddr)
  } else if(hasMbist & !singlePort) {
    Mux(mbistBd.ack, mbistBd.addr, waddr)
  } else {
    waddr
  }

  private val funcWmask = sp.funcMaskConverse(wmask)

  private val ramWmask = if(hasMbist) Mux(mbistBd.ack, mbistBd.wmask, funcWmask) else funcWmask
  private val ramWdata = if(hasMbist) Mux(mbistBd.ack, mbistBd.wdata, wdata) else wdata
  private val ramRen = if(hasMbist) Mux(mbistBd.ack, mbistBd.re, ren) else ren
  private val ramRaddr = if(hasMbist) Mux(mbistBd.ack, mbistBd.addr_rd, raddr) else raddr

  private val (ckRen, renStretched) = if(inputMcp) {
    val rreqReg = RegInit(0.U(isc.W))
    rreqReg.suggestName("rreqReg")
    val rstate = Cat(false.B, rreqReg)
    when(ramRen) {
      rreqReg := Fill(rreqReg.getWidth, true.B)
    }.otherwise {
      rreqReg := rstate >> 1.U
    }
    val cgEn = if(extraHold) rstate(2, 1) === "b01".U else rstate(1, 0) === "b01".U
    val ramEn = rreqReg(0)
    (cgEn, ramEn)
  } else {
    (ramRen, if(explicitHold) Mux(mbistBd.ack, mbistBd.re, io.r.req.valid) else ramRen)
  }

  private val (ckWen, wenStretched) = if(inputMcp) {
    val wreqReg = RegInit(0.U(isc.W))
    wreqReg.suggestName("wreqReg")
    val wstate = Cat(false.B, wreqReg)
    when(ramWen) {
      wreqReg := Fill(wreqReg.getWidth, true.B)
    }.otherwise {
      wreqReg := wstate >> 1.U
    }
    val cgEn = if(extraHold) wstate(2, 1) === "b01".U else wstate(1, 0) === "b01".U
    val ramEn = wreqReg(0)
    (cgEn, ramEn)
  } else {
    (ramWen, if(explicitHold) Mux(mbistBd.ack, mbistBd.we, io.w.req.valid) else ramWen)
  }

  private val respReg = RegInit(0.U(latency.W))
  when(ckRen) {
    respReg := (1 << (latency - 1)).U
  }.otherwise {
    respReg := Cat(false.B, respReg) >> 1.U
  }

  private val ramRdata = SramProto.read(array, singlePort, ramRaddr, renStretched && !brcBd.ram_hold)
  when(wenStretched && !brcBd.ram_hold) {
    SramProto.write(array, singlePort, ramWaddr, ramWdata, ramWmask)
  }

  rcg.dft.fromBroadcast(brcBd)
  if(singlePort) {
    rcg.E := ckRen | ckWen
  } else {
    rcg.E := ckRen
  }

  wcg.foreach(cg => {
    cg.dft.fromBroadcast(brcBd)
    cg.E := ckWen
  })

  if(pwctl.isDefined) {
    if(setup > 1) {
      val activeLength = setup + latency + 1
      val activeReg = RegInit(0.U(activeLength.W))
      when(ramWen | ramRen) {
        activeReg := Fill(activeLength, true.B)
      }.elsewhen(activeReg.orR) {
        activeReg := Cat(false.B, activeReg) >> 1
      }
      pwctl.get.light_sleep := !activeReg(0)
    } else {
      pwctl.get.light_sleep := Mux(mbistBd.ack, false.B, io.pwctl.get.deact)
    }
    if(powerCtl) {
      pwctl.get.deep_sleep := Mux(mbistBd.ack, false.B, io.pwctl.get.ret)
      pwctl.get.shut_down := Mux(mbistBd.ack, false.B, io.pwctl.get.stop)
    } else {
      pwctl.get.deep_sleep := false.B
      pwctl.get.shut_down := false.B
    }
  }

  private val rdataReg = Reg(UInt(dataWidth.W))
  private val doActualBypass = doBypass.getOrElse(false.B)
  private val wBypassMask = Mux(doActualBypass, _wmask, 0.U(_wmask.getWidth.W))
  private val wBypassMaskReg = RegEnable(wBypassMask, wen)
  private val wBypassBitMaskP = Cat(wBypassMaskReg.asBools.map(m => Fill(elementWidth, m)).reverse)
  private val wBypassBitMaskN = (~wBypassBitMaskP).asUInt
  private val doBypassReg = RegEnable(doActualBypass, ren)
  private val bypassData = (rdataReg & wBypassBitMaskP) | (ramRdata & wBypassBitMaskN)
  when(doActualBypass) {
    rdataReg := wdata.asUInt
  }.elsewhen((respReg(0) && holdRead.B) || (mbistBd.ack && hasMbist.B)) {
    rdataReg := Mux(doBypassReg, bypassData, ramRdata)
  }

  if(holdRead && !enableBypass) {
    io.r.resp.data := Mux(respReg(0), ramRdata, rdataReg).asTypeOf(io.r.resp.data)
  } else if(!holdRead && enableBypass) {
    io.r.resp.data := Mux(respReg(0) & doBypassReg, bypassData, ramRdata).asTypeOf(io.r.resp.data)
  } else if(holdRead && enableBypass) {
    io.r.resp.data := Mux(respReg(0), Mux(doBypassReg, bypassData, ramRdata), rdataReg).asTypeOf(io.r.resp.data)
  } else {
    io.r.resp.data := ramRdata.asTypeOf(io.r.resp.data)
  }

  io.r.resp.valid := respReg(0)
  mbistBd.rdata := rdataReg

  private val interval = latency.max(isc)
  private val (intvCntR, intvCntW) = if(singlePort) {
    val intvCnt = RegInit(0.U(log2Ceil(interval + 1).W))
    intvCnt.suggestName("intvCnt")
    when(ramRen || ramWen) {
      intvCnt := (interval - 1).U
    }.elsewhen(intvCnt.orR) {
      intvCnt := intvCnt - 1.U
    }
    (intvCnt, intvCnt)
  } else {
    val rIntvCnt = RegInit(0.U(log2Ceil(interval + 1).W))
    val wIntvCnt = RegInit(0.U(log2Ceil(interval + 1).W))
    rIntvCnt.suggestName("intvCntR")
    wIntvCnt.suggestName("intvCntW")

    when(ramRen) {
      rIntvCnt := (interval - 1).U
    }.elsewhen(rIntvCnt.orR) {
      rIntvCnt := rIntvCnt - 1.U
    }
    when(ramWen) {
      wIntvCnt := (interval - 1).U
    }.elsewhen(wIntvCnt.orR) {
      wIntvCnt := wIntvCnt - 1.U
    }
    (rIntvCnt, wIntvCnt)
  }

  private val singleHold = if(singlePort) io.w.req.valid else false.B
  private val resetHold = if(shouldReset) resetState else false.B
  io.r.req.ready := intvCntR === 0.U && !resetHold && !singleHold
  io.w.req.ready := intvCntW === 0.U && !resetHold

  when(io.r.req.valid) {
    assert(io.r.req.bits.setIdx < set.U, cf"Illegal read addr 0x${io.r.req.bits.setIdx}%x on SRAM, max addr is 0x${(set - 1).toHexString}")
  }
  when(io.w.req.valid) {
    assert(io.w.req.bits.setIdx < set.U, cf"Illegal write addr 0x${io.w.req.bits.setIdx}%x on SRAM, max addr is 0x${(set - 1).toHexString}")
  }
}
