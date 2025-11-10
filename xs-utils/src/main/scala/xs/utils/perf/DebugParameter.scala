package xs.utils.perf

import org.chipsalliance.cde.config.Field
case object DebugOptionsKey extends Field[DebugOptions]

case class DebugOptions
(
  FPGAPlatform: Boolean = false,
  EnableDifftest: Boolean = false,
  AlwaysBasicDiff: Boolean = false,
  EnableDebug: Boolean = false,
  EnablePerfDebug: Boolean = true,
  UseDRAMSim: Boolean = false,
  EnableTopDown: Boolean = false,
  EnableChiselDB: Boolean = false,
  EnableLuaScoreBoard: Boolean = false,
  ResetGen: Boolean = false,
  EnableConstantin: Boolean = false,
  AlwaysBasicDB: Boolean = false,
  EnableRollingDB: Boolean = false,
  EnableHWMoniter: Boolean = true
)