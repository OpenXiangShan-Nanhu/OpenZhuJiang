/***************************************************************************************
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
***************************************************************************************/

package xs.utils.queue

import chisel3._
import chisel3.util._

class MimoDeqDriver[T <: Data](gen:T, deqNum:Int) extends Module {
  val io = IO(new Bundle{
    val in = Input(Vec(deqNum, Valid(gen)))
    val deq = Vec(deqNum, Decoupled(gen))
  })

  private val validsReg = RegInit(VecInit(Seq.fill(deqNum)(false.B)))
  private val bitsRegs = Reg(Vec(deqNum, gen))
  io.deq.zip(validsReg).zip(bitsRegs).foreach({case((d, v), b) =>
    d.valid := v
    d.bits := b
  })

  for(((in, v), b) <- io.in.zip(validsReg).zip(bitsRegs)){
    v := in.valid
    when(in.valid){
      b := in.bits
    }
  }
}

class MimoQueue[T <: Data]
(
  gen: T,
  enqNum: Int,
  deqNum: Int,
  size: Int,
  enqIsCompact: Boolean = false,
  deqReduceFanout: Boolean = false
) extends Module with HasCircularQueuePtrHelper {
  private class MimoQueuePtr extends CircularQueuePtr[MimoQueuePtr](size)
  private object MimoQueuePtr {
    def apply(f:Boolean, v:Int):MimoQueuePtr = {
      val ptr = Wire(new MimoQueuePtr)
      ptr.flag := f.B
      ptr.value := v.U
      ptr
    }

    def genVec(f:Boolean, size:Int): Vec[MimoQueuePtr] = {
      VecInit(Seq.tabulate(size)(i => MimoQueuePtr(f = f, v = i)))
    }
  }

  val io = IO(new Bundle {
    val enq = Vec(enqNum, Flipped(Decoupled(gen)))
    val deq = Vec(deqNum, Decoupled(gen))
    val count = Output(UInt(log2Ceil(size + 1).W))
  })

  private val array = Reg(Vec(size, gen))
  private val enqPtrVec = RegInit(MimoQueuePtr.genVec(f= false, size = enqNum))
  private val deqPtrVec = RegInit(MimoQueuePtr.genVec(f= false, size = deqNum))
  private val deqDriver = if(deqReduceFanout) Some(Module(new MimoDeqDriver(gen, deqNum))) else None
  private val emptyEntries = RegInit(size.U(log2Ceil(size + 1).W))
  private val deqPtrVecNext = deqPtrVec.map(WireInit(_))
  private val enqPtr = enqPtrVec.head
  private val deqPtr = deqPtrVec.head

  io.count := size.U(log2Ceil(size + 1).W) - emptyEntries
  deqPtrVec := deqPtrVecNext

  private val enqAddrVec = Wire(Vec(enqNum, UInt(log2Ceil(size).W)))
  dontTouch(enqAddrVec)
  if(enqNum == 1) {
    enqAddrVec.head := enqPtr.value
    io.enq.head.ready := emptyEntries.orR
  } else {
    for(i <- 0 until enqNum) {
      val ptrIdx = Wire(UInt(log2Ceil(enqNum).W))
      if(enqIsCompact) {
        ptrIdx := i.U
      } else {
        ptrIdx := PopCount(false.B +: io.enq.take(i).map(_.valid))
      }
      enqAddrVec(i) := enqPtrVec(ptrIdx).value
      io.enq(i).ready := emptyEntries > ptrIdx
    }
  }

  for(i <- array.indices) {
    val enqSel = io.enq.zip(enqAddrVec).map(e => e._1.fire && e._2 === i.U)
    val enqValid = Cat(enqSel).orR
    when(enqValid) {
      array(i) := Mux1H(enqSel, io.enq.map(_.bits))
    }
  }

  for(i <- 0 until deqNum) {
    if(deqReduceFanout) {
      deqDriver.get.io.in(i).valid := deqPtrVecNext(i) < enqPtr
      deqDriver.get.io.in(i).bits := array(deqPtrVecNext(i).value)
      val enqHitsVec = io.enq.zip(enqAddrVec).map(e => e._1.fire && e._2 === deqPtrVecNext(i).value)
      val enqUpdateEn = Cat(enqHitsVec).orR
      when(enqUpdateEn) {
        deqDriver.get.io.in(i).valid := true.B
        deqDriver.get.io.in(i).bits := Mux1H(enqHitsVec, io.enq.map(_.bits))
      }
      io.deq(i) <> deqDriver.get.io.deq(i)
    } else {
      io.deq(i).valid := deqPtrVec(i) < enqPtr
      io.deq(i).bits := array(deqPtrVec(i).value)
    }
  }
  private val arrive = Cat(io.enq.map(d => d.fire)).orR
  private val arriveNum = if(enqIsCompact) PriorityEncoder(io.enq.map(d => !d.fire) :+ true.B) else PopCount(io.enq.map(_.fire))
  private val leave = Cat(io.deq.map(d => d.fire)).orR
  private val leaveNum = PriorityEncoder(io.deq.map(d => !d.fire) :+ true.B)

  when(arrive || leave) {
    emptyEntries := emptyEntries +& leaveNum - arriveNum
    enqPtrVec.foreach(e => e := e + arriveNum)
    deqPtrVec.foreach(d => d := d + leaveNum)
  }

  private val enqValidNum = PopCount(io.enq.map(_.valid))
  private val deqReadyNum = PopCount(io.deq.map(_.ready))
  for(i <- 0 until enqNum) {
    if(enqIsCompact) {
      assert(Mux(i.U < enqValidNum, io.enq(i).valid, !io.enq(i).valid), "enq valids should be continuous from the lowest port")
    }
  }
  for(i <- 0 until deqNum) {
    assert(Mux(i.U < deqReadyNum, io.deq(i).ready, !io.deq(i).ready), "deq readies should be continuous from the lowest port")
  }
  assert(emptyEntries === (size.U - distanceBetween(enqPtr, deqPtr)))
}
