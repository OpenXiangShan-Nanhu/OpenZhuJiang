package xs.utils
import chisel3._
import chisel3.experimental.SourceLine
import chisel3.util._

object PickOneHigh {
  def apply(in:UInt)(implicit valName: sourcecode.Name):Valid[UInt] = {
    val res = Wire(Valid(UInt(in.getWidth.W)))
    res.valid := in.orR
    res.bits := in & ((~in).asUInt + 1.U)
    res.suggestName(valName.value)
    res
  }
  def apply(in: Seq[Bool])(implicit valName: sourcecode.Name): Valid[UInt] = apply(Cat(in.reverse))(valName)
}

object PickOneLow {
  def apply(in: UInt)(implicit valName: sourcecode.Name): Valid[UInt] = PickOneHigh((~in).asUInt)(valName)

  def apply(in: Seq[Bool])(implicit valName: sourcecode.Name): Valid[UInt] = apply(Cat(in.reverse))(valName)
}