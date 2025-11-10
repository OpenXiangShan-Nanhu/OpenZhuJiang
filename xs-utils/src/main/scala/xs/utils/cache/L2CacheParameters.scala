/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  * *************************************************************************************
  */

// See LICENSE.SiFive for license details.

package xs.utils.cache


import chisel3._
import chisel3.util.log2Ceil
import freechips.rocketchip.diplomacy.{BufferParams, AddressSet}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.Field
import xs.utils.cache.common.{AliasKey, IsHitKey, PrefetchKey, VaddrKey}
import xs.utils.tl.ReqSourceKey
import xs.utils.cache.CacheParameters
import xs.utils.cache.prefetch.PrefetchParameters
import xs.utils.debug.HardwareAssertionKey

case object EnableCHI extends Field[Boolean](false)

// L1 Cache Params, used for TestTop generation
case class L1Param
(
  name: String = "L1D",
  sets: Int = 32,
  ways: Int = 8,
  blockBytes: Int = 64,
  aliasBitsOpt: Option[Int] = None,
  vaddrBitsOpt: Option[Int] = None,
  isKeywordBitsOpt : Option[Boolean] = None
) {
  val capacity = sets * ways * blockBytes
  val setBits = log2Ceil(sets)
  val offsetBits = log2Ceil(blockBytes)
  val needResolveAlias = aliasBitsOpt.nonEmpty
}

// Pass PMA and uncached memory attribute from PBMT to MMIOBridge
case object DeviceType extends ControlKey[Bool]("deviceType")
case class DeviceTypeField() extends BundleField[Bool](DeviceType, Output(Bool()), _ := false.B)

case class L2Param(
  name: String = "L2",
  ways: Int = 4,
  sets: Int = 128,
  blockBytes: Int = 64,
  pageBytes: Int = 4096,
  channelBytes: TLChannelBeatBytes = TLChannelBeatBytes(32),
  clientCaches: Seq[L1Param] = Nil,
  replacement: String = "drrip",
  mshrs: Int = 16,
  releaseData: Int = 3,
  /* 0 for dirty alone
   * 1 for dirty and accessed
   * 2 for all except prefetch & !accessed
   * 3 for all
   */
  mmioBridgeSize: Int = 16,

  // Client
  echoField: Seq[BundleFieldBase] = Nil,
  reqField: Seq[BundleFieldBase] = Nil,
  respKey: Seq[BundleKeyBase] = Seq(IsHitKey),
  // Manager
  reqKey: Seq[BundleKeyBase] = Seq(AliasKey, VaddrKey, PrefetchKey, ReqSourceKey),
  respField: Seq[BundleFieldBase] = Nil,

  innerBuf: TLBufferParams = TLBufferParams(),
  outerBuf: TLBufferParams = TLBufferParams(
    a = BufferParams.default,
    b = BufferParams.default,
    c = BufferParams.none,
    d = BufferParams.default,
    e = BufferParams.default
  ),

  hartId: Int = 0,
  // Prefetch
  prefetch: Seq[PrefetchParameters] = Nil,
  blockCycle: Int = 200,
  // L2 Flush
  enableL2Flush: Boolean = false,
  // Performance analysis
  enablePerf: Boolean = true,
  // RollingDB
  enableRollingDB: Boolean = true,
  // Monitor
  enableMonitor: Boolean = true,
  // TLLog
  enableTLLog: Boolean = true,
  // CHILog
  enableCHILog: Boolean = true,
  // TopDown
  elaboratedTopDown: Boolean = true,
  // env
  FPGAPlatform: Boolean = false,
  // ECC
  tagECC: Option[String] = None,
  dataECC: Option[String] = None,
  enableTagECC: Boolean = false,
  enableDataECC: Boolean = false,
  // DataCheck
  dataCheck: Option[String] = Some("oddparity"),
  enablePoison: Boolean = true,

  // Network layer SAM
  sam: Seq[(AddressSet, Int)] = Seq(AddressSet.everything -> 0),
  hasMbist:Boolean = false
) {
  lazy val capacity = sets * ways * blockBytes
  def toCacheParams: CacheParameters = CacheParameters(
    name = name,
    sets = sets,
    ways = ways,
    blockGranularity = log2Ceil(sets),
    blockBytes = blockBytes
  )
  def tagCode: Code = Code.fromString(tagECC)
  def dataCode: Code = Code.fromString(dataECC)
}


