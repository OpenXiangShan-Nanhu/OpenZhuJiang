
package xs.utils.cache.common

import chisel3._
import chisel3.util.BitPat.bitPatToUInt
import chisel3.util._
import xs.utils.tl._


class PrefetchRecv extends Bundle {
  val addr = UInt(64.W)
  val pf_source = UInt(MemReqSource.reqSourceBits.W)
  val addr_valid = Bool()
  val l2_pf_en = Bool()
}

// custom l2 - l1 CMO inst req
class CMOReq extends Bundle {
  val opcode = UInt(3.W)   // 0-cbo.clean, 1-cbo.flush, 2-cbo.inval, 3-cbo.zero
  val address = UInt(64.W)
}
// custom l2 - l1 CMO inst resp(ack)
class CMOResp extends Bundle {
  val address = UInt(64.W)
  val nderr = Bool()
}

class TPmetaReq(hartIdLen: Int, fullAddressBits: Int, offsetBits: Int) extends Bundle {
  // TODO: rawData's width is determined by L2; when L2's offsetBits change, rawData should change accordingly
  val hartid = UInt(hartIdLen.W)
  val set = UInt(32.W) // determined by TP
  val way = UInt(4.W)
  val wmode = Bool()
  val rawData = Vec(log2Floor(512 / (fullAddressBits - offsetBits)), UInt((fullAddressBits - offsetBits).W))
}

class TPmetaResp(hartIdLen: Int, fullAddressBits: Int, offsetBits: Int) extends Bundle {
  val hartid = UInt(hartIdLen.W)
  val rawData = Vec(log2Floor(512 / (fullAddressBits - offsetBits)), UInt((fullAddressBits - offsetBits).W))
}

class PrefetchCtrlFromCore extends Bundle {
  val l2_pf_master_en = Bool()
  val l2_pf_recv_en = Bool()
  val l2_pbop_en = Bool()
  val l2_vbop_en = Bool()
  val l2_tp_en = Bool()
}
class PrefetchCtrl extends Bundle {
  val l1I_pf_enable = Bool()
  val l2_pf_enable = Bool()
  val l1D_pf_enable = Bool()
  val l1D_pf_train_on_hit = Bool()
  val l1D_pf_enable_agt = Bool()
  val l1D_pf_enable_pht = Bool()
  val l1D_pf_active_threshold = UInt(4.W)
  val l1D_pf_active_stride = UInt(6.W)
  val l1D_pf_enable_stride = Bool()
  val l2_pf_store_only = Bool()
  val l2_pf_recv_enable = Bool()
  val l2_pf_pbop_enable = Bool()
  val l2_pf_vbop_enable = Bool()
  val l2_pf_tp_enable = Bool()

  def toL2PrefetchCtrl(): PrefetchCtrlFromCore = {
    val res = Wire(new PrefetchCtrlFromCore)
    res.l2_pf_master_en := l2_pf_enable
    res.l2_pf_recv_en := l2_pf_recv_enable
    res.l2_pbop_en := l2_pf_pbop_enable
    res.l2_vbop_en := l2_pf_vbop_enable
    res.l2_tp_en := l2_pf_tp_enable
    res
  }
}