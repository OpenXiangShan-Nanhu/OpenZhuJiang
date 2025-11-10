import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import scala.collection.mutable


object DebugSignals {
  private val globalSignalMap = mutable.HashMap[String, UInt]()

  def addSignal(name:String, signal:UInt):Unit = {
    require(!globalSignalMap.contains(name))
    globalSignalMap(name) = signal
  }

  def genTopSignal():(MixedVec[UInt], Seq[String]) = {
    val dataType = globalSignalMap.values.toSeq
    val res = Wire(MixedVec(dataType))
    res.zip(globalSignalMap.values.toSeq).foreach({case(sink, source) =>
      sink := BoringUtils.tap(source)
    })
    (res, globalSignalMap.keys.toSeq)
  }
}
