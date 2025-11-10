package xs.utils.sram

import chisel3._
import chisel3.util._
import xs.utils.GlobalData

class SpRamRwIO(dw:Int, be:Int, set:Int) extends Bundle {
  val clk = Input(Clock())
  val addr = Input(UInt(log2Ceil(set).W))
  val en = Input(Bool())
  val wmode = Input(Bool())
  val wmask = if(be > 1) Some(Input(UInt(be.W))) else None
  val wdata = Input(UInt(dw.W))
  val rdata = Output(UInt(dw.W))
}

class DpRamRIO(dw:Int, set:Int) extends Bundle {
  val clk = Input(Clock())
  val addr = Input(UInt(log2Ceil(set).W))
  val en = Input(Bool())
  val data = Output(UInt(dw.W))
}

class DpRamWIO(dw:Int, be:Int, set:Int) extends Bundle {
  val clk = Input(Clock())
  val addr = Input(UInt(log2Ceil(set).W))
  val en = Input(Bool())
  val data = Input(UInt(dw.W))
  val mask = if(be > 1) Some(Input(UInt(be.W))) else None
}

class SramInstGen(sp:Boolean, dw:Int, be:Int, set:Int, delay:Boolean) extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle{
    val RW0 = if(sp) Some(new SpRamRwIO(dw, be, set)) else None
    val R0 = if(!sp) Some(new DpRamRIO(dw, set)) else None
    val W0 = if(!sp) Some(new DpRamWIO(dw, be, set)) else None
  })
  private val seg = dw / be
  private val fn = s"${GlobalData.prefix}GENERIC_RAM_${if(sp) 1 else 2}P${set}D${dw}W${be}M${if(delay)"D" else ""}"
  override val desiredName = fn

  private def genIO:String = {
    if(sp) {
      s"""  input RW0_clk,
         |  input [${log2Ceil(set) - 1}:0] RW0_addr,
         |  input RW0_en,
         |  input RW0_wmode,
         |  ${if(be > 1) s"input [${be - 1}:0] RW0_wmask," else ""}
         |  input [${dw - 1}:0] RW0_wdata,
         |  output [${dw - 1}:0] RW0_rdata""".stripMargin
    } else {
      s"""  input R0_clk,
         |  input R0_en,
         |  input [${log2Ceil(set) - 1}:0] R0_addr,
         |  output [${dw - 1}:0] R0_data,
         |
         |  input W0_clk,
         |  input W0_en,
         |  input [${log2Ceil(set) - 1}:0] W0_addr,
         |  ${if(be > 1) s"input [${be - 1}:0] W0_mask," else ""}
         |  input [${dw - 1}:0] W0_data""".stripMargin
    }
  }

  private def genWriteLoop:String = {
    val mask = if(sp) "RW0_wmask" else "W0_mask"
    val addr = if(sp) "RW0_addr" else "W0_addr"
    val data = if(sp) "RW0_wdata" else "W0_data"
    val inc = if(sp) "  " else ""
    if(be == 1) {
      s"      ${inc}mem[$addr] <= $data;".stripMargin
    } else {
      val strSeq = for(i <- 0 until be) yield {
        val range = s"${i * seg + seg - 1}:${i * seg}"
        s"      ${inc}if($mask[$i]) mem[$addr][$range] <= $data[$range];"
      }
      strSeq.reduce(_ + "\n" + _)
    }
  }

  private def genDpReadWrite:String = s"""
       |  always @ (posedge W0_clk) begin
       |    if(W0_en) begin
       |$genWriteLoop
       |    end
       |  end
       |
       |  always @ (posedge R0_clk) begin
       |    if(R0_en) rdata <= mem[R0_addr];
       |  end
       |${if(delay) "`ifdef DELAY_READ" else "  assign R0_data = rdata;"}
       |${if(delay) "  always @ (posedge R0_clk) begin" else ""}
       |${if(delay) "    rdata_delay <= rdata;" else ""}
       |${if(delay) "  end" else ""}
       |${if(delay) "  assign R0_data = rdata_delay;" else ""}
       |${if(delay) "`else" else ""}
       |${if(delay) "  assign R0_data = rdata;" else ""}
       |${if(delay) "`endif" else ""}
       |""".stripMargin


  private def genSpReadWrite:String =
    s"""
       |  always @ (posedge RW0_clk) begin
       |    if(RW0_en) begin
       |      if(RW0_wmode) begin
       |$genWriteLoop
       |      end else begin
       |        rdata <= mem[RW0_addr];
       |      end
       |    end
       |  end
       |${if(delay) "`ifdef DELAY_READ" else "  assign RW0_rdata = rdata;"}
       |${if(delay) "  always @ (posedge RW0_clk) begin" else ""}
       |${if(delay) "    rdata_delay <= rdata;" else ""}
       |${if(delay) "  end" else ""}
       |${if(delay) "  assign RW0_rdata = rdata_delay;" else ""}
       |${if(delay) "`else" else ""}
       |${if(delay) "  assign RW0_rdata = rdata;" else ""}
       |${if(delay) "`endif" else ""}
       |""".stripMargin

  setInline(fn + ".sv",
    s"""// VCS coverage exclude_file
       |`ifdef SYNTHESIS
       |  `define DELAY_READ
       |`elsif BYPASS_CLOCKGATE
       |  `define DELAY_READ
       |`endif
       |module $fn (
       |$genIO
       |);
       |${if(!sp) "  (* rw_addr_collision = \"yes\" *)" else ""}
       |  reg [${dw - 1}:0] mem [${set - 1}:0];
       |  reg [${dw - 1}:0] rdata;
       |${if(delay) s"`ifdef DELAY_READ" else ""}
       |${if(delay) s"  reg [${dw - 1}:0] rdata_delay;" else ""}
       |${if(delay) s"`endif" else ""}
       |${if(sp) genSpReadWrite else genDpReadWrite}
       |endmodule
       |`ifdef DELAY_READ
       |  `undef DELAY_READ
       |`endif
       |""".stripMargin)
}