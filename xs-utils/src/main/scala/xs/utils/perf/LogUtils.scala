/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2024 Institute of Computing Technology, Chinese Academy of Sciences
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
***************************************************************************************/

package xs.utils.perf

import chisel3._
import org.chipsalliance.cde.config.{Field, Parameters}
import XSLogLevel.XSLogLevel
import chisel3.experimental.{SourceInfo, SourceLine}

object XSLogLevel extends Enumeration {
  type XSLogLevel = Value
  val ALL   = Value(0, "ALL  ")
  val DEBUG = Value("DEBUG")
  val INFO  = Value("INFO ")
  val PERF  = Value("PERF ")
  val WARN  = Value("WARN ")
  val ERROR = Value("ERROR")
  val OFF   = Value("OFF  ")
}

case object LogUtilsOptionsKey extends Field[LogUtilsOptions]
case class LogUtilsOptions
(
  enableDebug: Boolean,
  enablePerf: Boolean,
  fpgaPlatform: Boolean
)


object XSLog {
  val MagicStr = "_LOG_MODULE_PATH_"
  def apply(debugLevel: XSLogLevel, ctrlInfoOpt: Option[LogPerfIO] = None)
           (prefix: Boolean, cond: Bool, pable: Printable)(implicit p: Parameters, s: SourceInfo): Unit =
  {
    if (debugLevel >= XSLogLevel.ERROR) {
      val descStr = s match {
        case SourceLine(filename, line, col) =>
          val fn = filename.replaceAll("\\\\", "/")
          cf"$fn:$line:$col: " + pable
        case _ => pable
      }
      assert(!cond, descStr)(s)
    } else {
      val logOpts = p(LogUtilsOptionsKey)
      val enableDebug = logOpts.enableDebug && debugLevel != XSLogLevel.PERF
      val enablePerf = logOpts.enablePerf && debugLevel == XSLogLevel.PERF
      if(!logOpts.fpgaPlatform && (enableDebug || enablePerf)) {
        val ctrlInfo = ctrlInfoOpt.getOrElse(Module(new LogPerfHelper).io)
        when(ctrlInfo.logEnable && cond) {
          val commonInfo = p"[$debugLevel][time=${ctrlInfo.timer}] $MagicStr: "
          printf((if(prefix) commonInfo else p"") + pable)
        }
      }
    }
  }
  def apply(debugLevel: XSLogLevel, ctrlInfo: LogPerfIO)
           (prefix: Boolean, cond: Bool, pable: Printable)(implicit p: Parameters, s: SourceInfo): Unit = {
    apply(debugLevel, Some(ctrlInfo))(prefix, cond, pable)(p, s)
  }
}

sealed abstract class LogHelperCommon(val logLevel: XSLogLevel){

  def apply(cond: Bool, fmt: String, data: Bits*)(implicit p: Parameters, s: SourceInfo): Unit =
    apply(cond, Printable.pack(fmt, data:_*))(p, s)
  def apply(cond: Bool, pable: Printable)(implicit p: Parameters, s: SourceInfo): Unit =
    apply(true, cond, pable)(p, s)
  def apply(fmt: String, data: Bits*)(implicit p: Parameters, s: SourceInfo): Unit =
    apply(Printable.pack(fmt, data:_*))(p, s)
  def apply(pable: Printable)(implicit p: Parameters, s: SourceInfo): Unit =
    apply(true.B, pable)(p, s)
  def apply(prefix: Boolean, cond: Bool, fmt: String, data: Bits*)(implicit p: Parameters, s: SourceInfo): Unit =
    apply(prefix, cond, Printable.pack(fmt, data:_*))(p, s)
  def apply(prefix: Boolean, cond: Bool, pable: Printable)(implicit p: Parameters, s: SourceInfo): Unit ={
    XSLog(logLevel)(prefix, cond, pable)(p, s)
  }
}

object XSDebug extends LogHelperCommon(XSLogLevel.DEBUG)

object XSInfo extends LogHelperCommon(XSLogLevel.INFO)

object XSWarn extends LogHelperCommon(XSLogLevel.WARN)

object XSError extends LogHelperCommon(XSLogLevel.ERROR)
