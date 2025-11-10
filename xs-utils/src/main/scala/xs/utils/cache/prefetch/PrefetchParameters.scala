package xs.utils.cache.prefetch

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xs.utils.tl.MemReqSource


trait PrefetchParameters {
  val hasPrefetchBit:  Boolean
  val hasPrefetchSrc:  Boolean
  val inflightEntries: Int // max num of inflight prefetch reqs
}

object PfSource extends Enumeration {
  val NoWhere = Value("NoWhere")
  val SMS     = Value("SMS")
  val BOP     = Value("BOP")
  val PBOP     = Value("PBOP")
  val Stream  = Value("Stream")
  val Stride  = Value("Stride")
  val TP      = Value("TP")

  val PfSourceCount = Value("PfSourceCount")
  val pfSourceBits = log2Ceil(PfSourceCount.id)

  def fromMemReqSource(s: UInt): UInt = {
    val pfsrc = WireInit(NoWhere.id.U.asTypeOf(UInt(pfSourceBits.W)))
    switch(s) {
      is (MemReqSource.Prefetch2L2BOP.id.U) { pfsrc := BOP.id.U }
      is (MemReqSource.Prefetch2L2PBOP.id.U) { pfsrc := PBOP.id.U }
      is (MemReqSource.Prefetch2L2SMS.id.U) { pfsrc := SMS.id.U }
      is (MemReqSource.Prefetch2L2TP.id.U)  { pfsrc := TP.id.U  }
      is (MemReqSource.Prefetch2L2Stream.id.U) { pfsrc := Stream.id.U }
      is (MemReqSource.Prefetch2L2Stride.id.U) { pfsrc := Stride.id.U }
    }
    pfsrc
  }
}