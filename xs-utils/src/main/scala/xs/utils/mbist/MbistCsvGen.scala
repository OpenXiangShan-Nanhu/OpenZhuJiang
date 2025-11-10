package xs.utils.mbist

import xs.utils.GlobalData

import java.util.regex.Pattern

abstract class MbistFileGenerator {
  def generate(): String
}

class MbistCsvGen(val intf: InterfaceInfo, val pip: MbistPipeline, val csvName: String) extends MbistFileGenerator {
  def generate(): String = {
    val fileName = s"$csvName.csv"
    println(s"Generating $fileName")
    var contents = "\"INTF Name\", \"INTF Addr\", \"INTF Data\", \"INTF Array\", \"INTF Be\", \"Has TpSRAM\"\n"
    contents += GlobalData.prefix + intf.toString + '\n'
    contents += "\"SRAM Name\",\"SRAM Type\",\"SRAM array\",\"pipeline depth\",\"bitWrite\",\"bank addr\",\"selectOH width\",\"foundry\",\"SRAM Inst\"\n"
    val pipPath = pip.pathName.split("\\.").init.map(_ + ".").reduce(_ + _)
    val pattern = Pattern.compile(pipPath)

    def removeSubstring(str: String): String = {
      val matcher = pattern.matcher(str)
      matcher.replaceAll("").replace(".", "_")
    }

    val node = pip.myNode
    node.ramParamsBelongToThis
      .zip(node.array_id)
      .zip(node.array_depth)
      .foreach({
        case ((p, id), depth) =>
          contents += removeSubstring(p.holder.pathName) + p.nodeSuffix + ","
          contents += GlobalData.prefix + p.vname + ".sv,"
          contents += id.toString + ","
          contents += (depth * 2 + p.pipeDepth).toString + ","
          contents += (if (p.bitWrite) "true," else "false,")
          contents += p.bankRange + ","
          contents += p.nodeNum + ","
          contents += p.foundry + ","
          contents += p.sramInst
          contents += "\n"
      })
    contents
  }
}
