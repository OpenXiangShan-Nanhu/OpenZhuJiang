package xs.utils.cvl.advanced

import chisel3._
import chisel3.util._
import chisel3.ltl._
import chisel3.ltl.Sequence.BoolSequence

import xs.utils.cvl._

object CVL_ASSERT_REQ_ACK_MULTI_ACK extends CVL_COMMON {
  def apply(
    assertEn: Boolean,
    coverEn: Boolean,
    cvlLongSequence: Boolean = false,
    clock: Clock,
    reset: Reset,
    name: String,
    min_time: Int,
    max_time: Int,
    req: Valid[UInt],
    ackWidth: Int = 1,
    ack: Vec[Valid[UInt]]
  ): Unit = {
    require(min_time >= 0, "min_time must be non-negative")
    require(max_time >= min_time, "max_time must be greater than or equal to min_time")
    
    if(cvlLongSequence) {
      
    } else {
      val validVec = RegInit(VecInit(0.U((max_time + 1).W).asBools))
      val idxVec = Reg(Vec((max_time + 1), UInt(req.bits.getWidth.W)))
      val inProgress = RegInit(VecInit(0.U((max_time + 1).W).asBools))
      validVec.zip(idxVec).zipWithIndex.foreach {
        case ((v, idx), i) => {
          if(i == 0) {
            if(min_time == 0) {
              v := req.valid
              idx := req.bits
              inProgress(0) := req.valid && ack.map{ case a => a.valid && (a.bits === req.bits) }.reduce(_ || _)
            } else {
              v := req.valid
              idx := req.bits
              inProgress(0) := false.B
            }
          }
          else {
            v := validVec(i - 1)
            idx := idxVec(i - 1)
            if(i >= min_time) {
              inProgress(i) := inProgress(i - 1) || validVec(i - 1) && ack.map{ case a => a.valid && (a.bits === idxVec(i - 1)) }.reduce(_ || _)
            } else {
              inProgress(i) := false.B
            }
          }
        }
      }

      val ackSuccess = validVec(max_time) && inProgress(max_time) || !validVec(max_time)

      gen_sva(
        assertEn = assertEn,
        coverEn = coverEn,
        cvlLongSequence = cvlLongSequence,
        clock = clock,
        reset = reset,
        label = "SVA_ASSERT_REQ_ACK_" + name,
        test_expr = ackSuccess
      )
    }
  }
}