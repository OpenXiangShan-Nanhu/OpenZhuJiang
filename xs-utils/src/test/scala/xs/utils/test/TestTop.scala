package xs.utils.test

import circt.stage.FirtoolOption
import org.chipsalliance.cde.config.{Config, Field, Parameters}

import scala.annotation.tailrec

case object OptKey extends Field[Opt]

case class Opt(build: String = "build")

class DefaultConfig extends Config((site, here, up) => {
  case OptKey => Opt()
})

object Parser {

  def apply(args: Array[String]): (Parameters, Array[String]) = {
    val defaultConfig = new DefaultConfig
    var firrtlOpts = Array[String]()
    var hasHelp: Boolean = false
    var hasTd: Boolean = false

    @tailrec
    def parse(config: Parameters, args: List[String]): Parameters = {
      args match {
        case Nil => config

        case "-td" :: bdStr :: tail =>
          hasTd = true
          parse(config.alter((site, here, up) => {
            case OptKey => up(OptKey).copy(build = bdStr)
          }), tail)

        case option :: tail =>
          firrtlOpts :+= option
          parse(config, tail)
      }
    }

    val cfg = parse(defaultConfig, args.toList)
    if (hasHelp) firrtlOpts :+= "--help"
    if (hasTd) {
      firrtlOpts :+= "-td"
      firrtlOpts :+= cfg(OptKey).build
    }
    (cfg, firrtlOpts)
  }
}

object TestTopHelper {
  val firtoolOpts = Seq(
    FirtoolOption("-O=release"),
    FirtoolOption("--disable-all-randomization"),
    FirtoolOption("--disable-annotation-unknown"),
    FirtoolOption("--strip-debug-info"),
    FirtoolOption("--lower-memories"),
    FirtoolOption("--add-vivado-ram-address-conflict-synthesis-bug-workaround"),
    FirtoolOption("--lowering-options=noAlwaysComb," +
      " disallowPortDeclSharing, disallowLocalVariables," +
      " emittedLineLength=120, explicitBitcast, locationInfoStyle=plain," +
      " disallowExpressionInliningInPorts, disallowMuxInlining")
  )
}