package xs.utils.debug

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import xs.utils.{FileRegisters, ResetRRArbiter}
import chisel3.experimental.{SourceInfo, SourceLine}
import org.chipsalliance.cde.config.{Field, Parameters}

import scala.collection.mutable

case object HardwareAssertionKey extends Field[HwaParams]

case class HwaParams(
  enable: Boolean = false,
  maxInfoBits: Int = 16,
  maxAssertRepeatNum: Int = 4,
  hwaDevDepth: Int = 2048
)

class HAssertBundle(width: Option[Int]) extends Bundle {
  val cond = Option.when(width.isEmpty)(Output(Bool()))
  val bus = width.map(w => Decoupled(UInt(w.W)))
}

case class HAssertNode(
  hassert: HAssertBundle,
  desc: Seq[(Int, String)],
  level: Int
)

object HAssert {
  def apply(cond:Bool, desc:Printable)(implicit p: Parameters, s: SourceInfo):Unit = HardwareAssertion(cond, desc)(p, s)
  def apply(cond:Bool)(implicit p: Parameters, s: SourceInfo):Unit = HardwareAssertion(cond)(p, s)
  def withEn(cond: Bool, en: Bool, desc: Printable)(implicit p: Parameters, s: SourceInfo): Unit = HardwareAssertion.withEn(cond, en, desc)(p ,s)
  def withEn(cond: Bool, en: Bool)(implicit p: Parameters, s: SourceInfo): Unit = HardwareAssertion.withEn(cond, en)(p ,s)
  def checkTimeout(clear: Bool, timeout: Int, desc: Printable)(implicit p: Parameters, s: SourceInfo): Unit = HardwareAssertion.checkTimeout(clear, timeout, desc)(p, s)
  def checkTimeout(clear: Bool, timeout: Int)(implicit p: Parameters, s: SourceInfo): Unit = HardwareAssertion.checkTimeout(clear, timeout)(p, s)
  def placePipe(level: Int, moduleTop: Boolean = false)(implicit p: Parameters): Option[Seq[HAssertNode]] = HardwareAssertion.placePipe(level, moduleTop)
  def release(node: Option[HAssertNode], dir: String, pfx: String = "")(implicit p: Parameters): Unit = HardwareAssertion.release(node, dir, pfx)
}

object HardwareAssertion {
  private var gid = 0
  private var nodeSeq = Seq[HAssertNode]()

  private val hashToCountMap = mutable.Map[String, Int]()

  private def extractStr(pt:Printable): String = {
    pt match {
      case Printables(pts) => pts.map(extractStr).reduce(_ + _)
      case PString(str) => str
      case _ => ""
    }
  }

  private def squashPoints(pts: Seq[HAssertNode])(implicit p: Parameters): Seq[HAssertNode] = {
    val hwaP = p(HardwareAssertionKey)
    pts.groupBy(_.desc.head._2).map({case(desc, ns) =>
      val asrtCnt = RegInit(hwaP.maxAssertRepeatNum.U(log2Ceil(hwaP.maxAssertRepeatNum + 1).W))
      val asrtVlds = ns.map(n => RegNext(BoringUtils.bore(n.hassert.cond.get), false.B))
      val squashCond = Wire(new HAssertBundle(Some(hwaP.maxInfoBits)))
      squashCond.bus.get.valid := Cat(asrtVlds).orR && asrtCnt.orR
      squashCond.bus.get.bits := gid.U
      when(squashCond.bus.get.fire) {
        asrtCnt := asrtCnt - 1.U
      }
      val res = HAssertNode(squashCond, Seq((gid, desc)), 0)
      gid = gid + 1
      res
    }).toSeq
  }

  private def genDescStr(desc:Printable, s: SourceInfo):Printable = {
    s match {
      case SourceLine(filename, line, col) =>
        val fn = filename.replaceAll("\\\\", "/")
        cf"$fn:$line:$col: " + desc
      case _ => desc
    }
  }

  /** Checks for a condition to be valid in the circuit at rising clock edge
   * when not in reset. If the condition evaluates to false, the circuit
   * simulation stops with an error. The assert id and user bits will be
   * output to the module interface
   *
   * @param cond condition, assertion fires (simulation fails) when false
   * @param desc optional format string to print when the assertion fires
   * @note desc must be defined as Printable(e.g. cf"xxx") to print chisel-type values
   */
  def apply(cond:Bool, desc:Printable)(implicit p: Parameters, s: SourceInfo): Unit = {
    val descStr = genDescStr(desc, s)
    val assertCond = cond
    assert(assertCond, descStr)(s)
    val hwaP = p(HardwareAssertionKey)
    if(hwaP.enable) {
      val pdesc = extractStr(descStr)
      val hashCode = s"${pdesc.hashCode}"
      if(!hashToCountMap.contains(hashCode)) {
        hashToCountMap.addOne((hashCode, 0))
      }
      hashToCountMap(hashCode) = hashToCountMap(hashCode) + 1
      val pcode = s"${hashCode}_${hashToCountMap(hashCode) - 1}"
      val thisCond = IO(new HAssertBundle(None))
      thisCond.cond.get := !assertCond
      thisCond.suggestName(s"hwa_$pcode")
      val node = HAssertNode(thisCond, Seq((0, pdesc)), 0)
      nodeSeq = nodeSeq :+ node
    }
  }
  def apply(cond: Bool)(implicit p: Parameters, s: SourceInfo): Unit = apply(cond, "")(p, s)

  /** Apply an assertion in the hardware design with an enable signal.
   *
   * @param cond condition, assertion fires (simulation fails) when false
   * @param en   enable signal for the assertion
   * @param desc optional format string to print when the assertion fires
   * @note desc must be defined as Printable(e.g. cf"xxx") to print chisel-type values
   */
  def withEn(cond: Bool, en: Bool, desc: Printable)(implicit p: Parameters, s: SourceInfo): Unit = apply(Mux(en, cond, true.B), desc)(p ,s)
  def withEn(cond: Bool, en: Bool)(implicit p: Parameters, s: SourceInfo): Unit = withEn(cond, en, "")(p ,s)

  /** Checks for timeout condition by counting cycles since last clear signal.
   * If the counter reaches its maximum value (300_0000 cycles), the circuit
   * simulation stops with an error. The assert id and user bits will be
   * output to the module interface.
   *
   * @param clear   reset signal that clears the timeout counter when asserted
   * @param timeout EDA assert max timeout value
   * @param desc    optional format string to print when timeout occurs
   * @note desc must be defined as Printable (e.g. cf"xxx") to print chisel-type values
   * @note Default timeout threshold of 3,000,000 cycles corresponds to 1ms at 3GHz clock frequency
   */
  def checkTimeout(clear: Bool, timeout: Int, desc: Printable)(implicit p: Parameters, s: SourceInfo): Bool = {
    val to_val = 0x1L << log2Ceil(3_000_000)
    require(timeout <= to_val)
    val to_cnt = Reg(UInt(log2Ceil(to_val + 1).W))
    when(clear || to_cnt < to_val.U) {
      to_cnt := Mux(clear, 0.U, to_cnt + 1.U)
    }
    val eda_err = to_cnt >= timeout.U
    val hwa_err = to_cnt >= to_val.U
    val descStr = genDescStr(desc, s)
    assert(!eda_err, descStr)
    apply(!hwa_err, desc)(p, s)
    eda_err
  }

  def checkTimeout(clear: Bool, timeout: Int)(implicit p: Parameters, s: SourceInfo): Bool = {
    checkTimeout(clear, timeout, cf"timeout!")(p, s)
  }

  def placePipe(level: Int, moduleTop: Boolean = false)(implicit p: Parameters): Option[Seq[HAssertNode]] = {
    if(p(HardwareAssertionKey).enable && nodeSeq.count(_.level < level) != 0) {
      val candidates = nodeSeq.filter(_.level < level)
      val children = candidates.filter(_.level > 0) ++ squashPoints(candidates.filter(_.level == 0))
      val width = p(HardwareAssertionKey).maxInfoBits
      require(gid < (1L << width))
      val nrPipe = if(moduleTop) 1 else (children.size + 15) / 16
      val segLen = (children.size + nrPipe - 1) / nrPipe
      val childrenSegSeq = children.grouped(segLen).toSeq
      require(nrPipe == childrenSegSeq.size)
      val res = for(cs <- childrenSegSeq) yield {
        val hwa_arb = Module(new ResetRRArbiter(gen = UInt(width.W), n = cs.size))
        val hwa_q = Module(new Queue(gen = UInt(width.W), entries = 2))
        val hwa_out = Wire(new HAssertBundle(Some(width)))
        dontTouch(hwa_arb.io)
        hwa_out := DontCare
        hwa_arb.io.in.zip(cs).foreach({ case(a, b) =>
          val hwa = BoringUtils.bore(b.hassert).bus.get
          a.valid := hwa.valid
          hwa.ready := a.ready
          a.bits := hwa.bits
        })
        hwa_q.io.enq <> hwa_arb.io.out
        hwa_out.bus.get <> hwa_q.io.deq
        HAssertNode(hwa_out, cs.flatMap(_.desc), level)
      }
      if(!moduleTop) {
        nodeSeq = nodeSeq.filterNot(_.level < level) ++ res
      } else {
        gid = 0
        hashToCountMap.clear()
      }
      Some(res)
    } else {
      None
    }
  }

  def release(node: Option[HAssertNode], dir: String, pfx: String = "")(implicit p: Parameters): Unit = {
    node.foreach(n => {
      nodeSeq = Nil
      gid = 0
      val str = n.desc
        .map(d => s"assertion ${d._1}: ${d._2}")
        .reduce((a, b) => a + '\n' + b)
      FileRegisters.add(dir, s"${pfx}_hardware_assertion.txt", str, dontCarePrefix = true)
    })
  }
}