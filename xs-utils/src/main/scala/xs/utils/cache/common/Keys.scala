
package xs.utils.cache.common

import chisel3._
import freechips.rocketchip.util.{BundleField, ControlKey}
import org.chipsalliance.cde.config.Field
import freechips.rocketchip.tilelink._
import xs.utils.cache.L2Param
// Pass virtual address of upper level cache
case object VaddrKey extends ControlKey[UInt]("vaddr")
case class VaddrField(width: Int) extends BundleField[UInt](VaddrKey, Output(UInt(width.W)), _ := 0.U(width.W))

// Pass load_miss_acquire_keyword of upper level cache (L1)
case object IsKeywordKey extends ControlKey[Bool]("isKeyword")
case class IsKeywordField() extends BundleField[Bool](IsKeywordKey, Output(Bool()), _ := false.B)

case object PrefetchKey extends ControlKey[Bool](name = "needHint")
case class PrefetchField() extends BundleField[Bool](PrefetchKey, Output(Bool()), _ := false.B)

case object PreferCacheKey extends ControlKey[Bool](name = "preferCache")
case class PreferCacheField() extends BundleField[Bool](PreferCacheKey, Output(Bool()), _ := false.B)

case object AliasKey extends ControlKey[UInt]("alias")
case class AliasField(width: Int) extends BundleField[UInt](AliasKey, Output(UInt(width.W)), _ := 0.U(width.W))

case object DirtyKey extends ControlKey[Bool](name = "blockisdirty")
case class DirtyField() extends BundleField[Bool](DirtyKey, Output(Bool()), _ := true.B)

case object IsHitKey extends ControlKey[Bool](name = "isHitInL3")
case class IsHitField() extends BundleField[Bool](IsHitKey, Output(Bool()), _ := true.B)

case object EdgeInKey extends Field[TLEdgeIn]

case object EdgeOutKey extends Field[TLEdgeOut]

case object BankBitsKey extends Field[Int]

case object L2ParamKey extends Field[L2Param](L2Param())

case object L2NBanksKey extends Field[Int]

case object SliceIdKey extends Field[Int]
