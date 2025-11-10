package xs.utils.sram

import chisel3._
import chisel3.experimental.hierarchy.Instance
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import xs.utils.mbist.Mbist._
import xs.utils.mbist.{Mbist, Ram2Mbist, Ram2MbistParams}
import xs.utils.sram.SramHelper._

import scala.collection.mutable
import scala.math.sqrt

case class SramInfo (
  dataBits:Int,
  way:Int,
  bist:Boolean,
  mdw:Int = maxMbistDataWidth
) {
  private val ew = dataBits
  private val isNto1 = ew > mdw
  //** ******implement mbist interface node(multiple nodes for one way)******
  private val (mbistNodeNumForEachWay, mbistNodeNumNto1) = getNodeNumForEachWayAndNodeNum_Nto1(ew, way, mdw)
  private val maskWidthNto1 = 1
  private val mbistDataWidthNto1 = (ew + mbistNodeNumForEachWay - 1) / mbistNodeNumForEachWay
  //** *******implement mbist interface node(one node for multiple ways)******
  private val (wayNumForEachNode, mbistNodeNum1toN) = getWayNumForEachNodeAndNodeNum_1toN(ew, way, mdw)
  private val mbistDataWidth1toN = wayNumForEachNode * ew
  private val maskWidth1toN = wayNumForEachNode

  val mbistNodeNum = if(isNto1) mbistNodeNumNto1 else mbistNodeNum1toN
  val mbistDataWidth = if(isNto1) mbistDataWidthNto1 else mbistDataWidth1toN
  val mbistMaskWidth = if(isNto1) maskWidthNto1 else maskWidth1toN
  val mbistArrayIds = Seq.tabulate(mbistNodeNum)(idx => getDomainID + idx)
  val bitWrite = way != 1
  val sramMaskBits = if(isNto1) mbistNodeNum else way
  val sramDataBits = way * dataBits
  val sramSegBits = sramDataBits / sramMaskBits
  if(bist) {
    val addId = if(isNto1) mbistNodeNumNto1 else mbistNodeNum1toN
    increaseNodeID(addId)
    increaseDomainID(addId)
  }
  def mbistMaskConverse(wmask:UInt, nodeSelectOH:UInt):UInt = {
    val fullMask = if(way > 1) Fill(mbistNodeNum, wmask) else Fill(mbistNodeNum, true.B)
    if(isNto1) {
      nodeSelectOH & fullMask
    } else {
      val n = sramMaskBits / mbistNodeNum
      val selMask = Cat(Seq.tabulate(sramMaskBits)(i => nodeSelectOH(i / n)).reverse)
      selMask & fullMask
    }
  }

  def funcMaskConverse(mask:UInt): UInt = {
    if(isNto1) {
      val n = sramMaskBits / way
      Cat(Seq.tabulate(sramMaskBits)(i => mask(i / n)).reverse)
    } else {
      mask
    }
  }
}

object SramHelper {
  private var nodeId = 0
  private var wrapperId = 0
  private var domainId = 0
  val sramCtrlQueue = new mutable.Queue[SramCtrlBundle]

  def getWayNumForEachNodeAndNodeNum_1toN(dw: Int, way: Int, mw: Int): (Int, Int) = {
    val dataNum1toNNode = mw / dw
    val numVec = (1 to dataNum1toNNode)
    val validVec = numVec.map(num => (way % num == 0) && (way >= num))
    val validNum = numVec.zip(validVec).filter(_._2)
    val res = if(validNum.isEmpty) (1, way) else validNum.last
    (res._1, way / res._1)
  }

  private def getDivisor(in: Int): Seq[Int] = {
    val end = sqrt(in).toInt
    val divisors =
      Seq.tabulate(end)(_ + 1).map(idx => (in % idx == 0, Seq(idx, in / idx))).filter(_._1).flatMap(_._2).sorted
    divisors
  }

  def getNodeNumForEachWayAndNodeNum_Nto1(dw: Int, way: Int, mw: Int): (Int, Int) = {
    val divisors = getDivisor(dw)
    val validDivisors = divisors.filter(_ <= mw)
    val goodNodeNumForEachWay = dw / validDivisors.max
    val defaultNodeNumForEachWay = ((dw + mw - 1) / mw)
    val finalNodeNumForEachWay =
      if(goodNodeNumForEachWay > 4 * defaultNodeNumForEachWay) defaultNodeNumForEachWay else goodNodeNumForEachWay
    (finalNodeNumForEachWay, way * finalNodeNumForEachWay)
  }

  def restartIndexing(): Unit = domainId = 0

  def getDomainID: Int = domainId

  def increaseDomainID(add: Int): Unit = domainId += add

  def increaseNodeID(add: Int): Unit = nodeId += add

  def genBroadCastBundleTop(): SramBroadcastBundle = {
    val res = Wire(new SramBroadcastBundle)
    res
  }
  def genSramCtrlBundleTop(): SramCtrlBundle = {
    val sramCtrlCfg = Wire(new SramCtrlBundle)
    sramCtrlQueue.toSeq.foreach(bd => {
      BoringUtils.bore(bd) := sramCtrlCfg
    })
    sramCtrlQueue.clear()
    sramCtrlCfg
  }

  private def mapMbistBore(bore: Ram2Mbist): Ram2Mbist = {
    val boreP = bore.params
    val sp = boreP.sramParams
    val nodeNum = sp.mbistNodeNum
    val resMp = Ram2MbistParams(
      sramParams = SramInfo(
        dataBits = sp.sramDataBits / sp.sramMaskBits,
        way = sp.sramMaskBits,
        bist = false,
        mdw = sp.sramDataBits
      ),
      set = boreP.set,
      singlePort = boreP.singlePort
    )
    val res = Wire(new Ram2Mbist(resMp))
    val selectOhReg = RegEnable(bore.selectedOH, bore.re)
    res := DontCare
    res.addr := bore.addr
    res.addr_rd := bore.addr_rd
    res.re := bore.re
    res.we := bore.we
    res.ack := bore.ack
    res.broadcast := bore.broadcast
    res.wdata := Fill(nodeNum, bore.wdata)
    res.wmask := sp.mbistMaskConverse(bore.wmask, bore.selectedOH)
    bore.rdata := Mux1H(selectOhReg, res.rdata.asTypeOf(Vec(nodeNum, UInt((sp.sramDataBits / nodeNum).W))))
    res
  }

  def genMbistBoreSink(bdParam: Ram2MbistParams, bist:Boolean, een:Boolean): Ram2Mbist = {
    val sp = bdParam.sramParams
    val mbist = Wire(new Ram2Mbist(bdParam))
    mbist := DontCare
    mbist.selectedOH := Fill(mbist.selectedOH.getWidth, 1.U(1.W))
    mbist.ack := false.B
    mbist.we := false.B
    mbist.re := false.B
    mbist.wmask := Fill(sp.mbistMaskWidth, true.B)
    if(bist) {
      Mbist.addRamNode(mbist, sp.mbistArrayIds, een)
    }
    mapMbistBore(mbist)
  }

  def genRam(
    sp: SramInfo,
    set: Int,
    dp: Boolean,
    setup: Int,
    hold: Int,
    latency: Int,
    bist: Boolean,
    extraHold: Boolean,
    broadcast: Option[SramBroadcastBundle],
    pwctl: Option[GenericSramPowerCtl],
    rclk: Clock,
    wclk: Option[Clock],
    suffix: String,
    foundry: String,
    sramInst: String,
    pipeDepth:  Int = 0,
    template: RawModule
  ): (Ram2Mbist, Instance[SramArray], String) = {


    val (array, vname) = SramProto(rclk, !dp, set, sp.sramDataBits, sp.sramMaskBits, setup, hold, latency, wclk, bist || broadcast.isDefined, suffix, pwctl.isDefined)
    val bdParam = Ram2MbistParams(
      sp,
      set,
      !dp,
      vname,
      "",
      foundry,
      sramInst,
      pipeDepth,
      "None",
      template
    )
    val isc = if(hold > 0) setup + 1 else setup
    val mbist = genMbistBoreSink(bdParam, bist, extraHold)
    if(broadcast.isDefined) {
      array.mbist.get.dft_ram_bp_clken := broadcast.get.ram_bp_clken
      array.mbist.get.dft_ram_bypass := broadcast.get.ram_bypass
    } else if(bist) {
      array.mbist.get.dft_ram_bp_clken := mbist.broadcast.ram_bp_clken
      array.mbist.get.dft_ram_bypass := mbist.broadcast.ram_bp_clken
    }
    val ramctl = Wire(new SramCtrlBundle)
    ramctl := DontCare
    SramHelper.sramCtrlQueue.enqueue(ramctl)
    array.ctrl := ramctl
    if(pwctl.isDefined) {
      array.pwctl.get := pwctl.get
    }
    (mbist, array, vname)
  }
}
