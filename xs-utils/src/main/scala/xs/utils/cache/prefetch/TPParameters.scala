package xs.utils.cache.prefetch

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xs.utils.cache.common.PrefetchRecv

case class TPParameters(
    tpTableEntries: Int = 16384,
    tpTableAssoc: Int = 16,
    vaddrBits: Int = 39,
    blockOffBits: Int = 6,
    dataReadQueueDepth: Int = 8,
    dataWriteQueueDepth: Int = 4,
    tpDataQueueDepth: Int = 8,
    triggerQueueDepth: Int = 4,
    throttleCycles: Int = 4,  // unused yet
    replacementPolicy: String = "random",
    debug: Boolean = false
) extends PrefetchParameters {
  override val hasPrefetchBit: Boolean = true
  override val hasPrefetchSrc: Boolean = true
  override val inflightEntries: Int = 16 // changed in sv48
}

trait TPmetaParameters {
  val metaEntries: Int
  val metaAssoc: Int
  val busBytes: Int
  val deltaBits: Int
  val nrDelta: Int
}

case class DefaultTPmetaParameters() extends TPmetaParameters {
  override val metaEntries = 16384
  override val metaAssoc = 16
  override val busBytes = 256
  override val deltaBits = 36
  override val nrDelta = 16
}
