package dongjiang.backend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import zhujiang.chi._
import dongjiang._
import dongjiang.utils._
import dongjiang.bundle._
import xs.utils.debug._
import dongjiang.directory.{DirEntry, DirMsg}
import dongjiang.frontend._
import dongjiang.frontend.decode._
import zhujiang.chi.RspOpcode._
import dongjiang.backend._

class DatalessCM(implicit p: Parameters) extends DJModule {

    val io = IO(new Bundle {
        val config = new DJConfigIO()

        val alloc = Flipped(Decoupled(new CMTask))
        val resp  = Decoupled(new CMResp)

        val txReq = Decoupled(new ReqFlit(true))
        val txRsp = Decoupled(new RespFlit())
        val rxRsp = Flipped(Valid(new RespFlit()))

        val updPosNest = Decoupled(new PosCanNest with HasQoS)
    })
    HardwareAssertion(!io.alloc.valid)
    io <> DontCare

    HardwareAssertion.placePipe(1)
}
