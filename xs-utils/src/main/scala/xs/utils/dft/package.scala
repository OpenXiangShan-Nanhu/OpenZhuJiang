package xs.utils

import chisel3._
import chisel3.util._
import xs.utils.sram.SramBroadcastBundle

package object dft {
  class RamTestBundle extends Bundle {
    val hold = Input(Bool())
    val bypass = Input(Bool())
    val bp_clken = Input(Bool())
    val aux_clk = Input(Bool())
    val aux_ckbp = Input(Bool())
    val mcp_hold = Input(Bool())
  }

  class PowerDomainTestBundle(pwrctl:Boolean) extends Bundle {
    val clk_on = Input(Bool())
    val clk_off = Input(Bool())
    val pwr_req = Option.when(pwrctl)(Input(Bool()))
    val pwr_ack = Option.when(pwrctl)(Output(Bool()))
    val iso_on = Option.when(pwrctl)(Input(Bool()))
  }

  class BaseTestBundle extends Bundle {
    val lgc_rst_n = Input(Bool())
    val mode = Input(Bool())
    val scan_mode = Input(Bool())
    val cgen = Input(Bool())
    val ram = new RamTestBundle

    def toSramBroadCastBundle: SramBroadcastBundle = {
      val brc = Wire(new SramBroadcastBundle)
      for((name, data) <- this.ram.elements) {
        brc.elements(s"ram_$name") := data
      }
      brc.cgen := this.cgen
      brc
    }

    def toResetDftBundle: DFTResetSignals = {
      val dft = Wire(new DFTResetSignals)
      dft.lgc_rst_n := this.lgc_rst_n.asAsyncReset
      dft.mode := this.mode
      dft.scan_mode := this.scan_mode
      dft
    }

    def from(that: BaseTestBundle):Unit = {
      this.lgc_rst_n := that.lgc_rst_n
      this.mode := that.mode
      this.scan_mode := that.scan_mode
      this.cgen := that.cgen
      this.ram := that.ram
    }
  }
}
