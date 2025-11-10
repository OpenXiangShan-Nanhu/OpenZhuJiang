package xs.utils.tl

import chisel3._
import freechips.rocketchip.util.{BundleField, ControlKey}
import org.chipsalliance.cde.config.{Field, Parameters}

case object TLUserKey extends Field[TLUserParams]
case class TLUserParams(
  aliasBits: Int = 0,
  vaddrBits: Int = 0
)
class TLNanhuUserBundle(implicit p:Parameters) extends Bundle {
  val pfHint = Output(Bool())
  val alias = if(p(TLUserKey).aliasBits > 0) Some(Output(UInt(p(TLUserKey).aliasBits.W))) else None
  val vaddr = if(p(TLUserKey).vaddrBits > 0) Some(Output(UInt(p(TLUserKey).vaddrBits.W))) else None
}
case object TLNanhuBusKey extends ControlKey[TLNanhuUserBundle]("nanhu")
case class TLNanhuBusField()(implicit p:Parameters) extends BundleField[TLNanhuUserBundle](TLNanhuBusKey, new TLNanhuUserBundle, _ := DontCare)
