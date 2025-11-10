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

package xs.utils

import chisel3._
import chisel3.util._

object LSFRTap {
  def bits0: Seq[Int] = Seq(0)
  def bits1: Seq[Int] = Seq(0)
  def bits2: Seq[Int] = Seq(0, 1)
  def bits3: Seq[Int] = Seq(1, 2)
  def bits4: Seq[Int] = Seq(2, 3)
  def bits5: Seq[Int] = Seq(2, 4)
  def bits6: Seq[Int] = Seq(4, 5)
  def bits7: Seq[Int] = Seq(5, 6)
  def bits8: Seq[Int] = Seq(3, 4, 5, 7)
  def bits9: Seq[Int] = Seq(4, 8)
  def bits10: Seq[Int] = Seq(6, 9)
  def bits11: Seq[Int] = Seq(8, 10)
  def bits12: Seq[Int] = Seq(0, 3, 5 ,11)
  def bits13: Seq[Int] = Seq(0, 2 ,3, 12)
  def bits14: Seq[Int] = Seq(0, 2 ,4, 13)
  def bits15: Seq[Int] = Seq(13, 14)
  def bits16: Seq[Int] = Seq(3, 12, 14, 15)
  def bits17: Seq[Int] = Seq(13, 16)
  def bits18: Seq[Int] = Seq(10, 17)
  def bits19: Seq[Int] = Seq(0, 1, 5, 18)
  def bits20: Seq[Int] = Seq(16, 19)
  def bits21: Seq[Int] = Seq(18, 20)
  def bits22: Seq[Int] = Seq(20, 21)
  def bits23: Seq[Int] = Seq(17, 22)
  def bits24: Seq[Int] = Seq(16, 21, 22, 23)
  def bits25: Seq[Int] = Seq(21, 24)
  def bits26: Seq[Int] = Seq(0, 1, 5, 25)
  def bits27: Seq[Int] = Seq(0, 1, 4, 26)
  def bits28: Seq[Int] = Seq(24, 27)
  def bits29: Seq[Int] = Seq(26, 28)
  def bits30: Seq[Int] = Seq(0, 3, 5, 29)
  def bits31: Seq[Int] = Seq(27, 30)
  def bits32: Seq[Int] = Seq(0, 1, 21, 31)

  def TapSeq = Seq(
    bits0,  bits1,  bits2,  bits3,
    bits4,  bits5,  bits6,  bits7,
    bits8,  bits9,  bits10, bits11,
    bits12, bits13, bits14, bits15,
    bits16, bits17, bits18, bits19,
    bits20, bits21, bits22, bits23,
    bits24, bits25, bits26, bits27,
    bits28, bits29, bits30, bits31,
  )
}
class LSFR(width:Int, init:Option[Long] = None) extends Module{
  val io = IO(new Bundle{
    val en = Input(Bool())
    val value = Output(UInt(width.W))
  })
  require(width > 0)
  private val tap = LSFRTap.TapSeq(width)
  private val lfsr = RegInit(init.getOrElse((1L << width) - 1).U(width.W)) // random initial value based on simulation seed
  private val xor = Cat(tap.map(t => lfsr.asBools.reverse(t))).xorR
  when(io.en) {
    lfsr := Mux(lfsr === 0.U, 1.U, Cat(xor, lfsr(width - 1, 1)))
  }
  io.value := lfsr
}

object LFSR64 {
  def apply(increment: Bool = true.B, seed: Option[Long] = Some(0x1234567887654321L)): UInt = {
    val wide = 64
    val lfsr = seed match {
      case None => RegInit(scala.util.Random.nextLong().abs.U(wide.W))
      case Some(long) => RegInit(long.U(wide.W)) 
    }
    val xor = lfsr(0) ^ lfsr(1) ^ lfsr(3) ^ lfsr(4)
    when (increment) {
      lfsr := Mux(lfsr === 0.U, 1.U, Cat(xor, lfsr(wide-1,1)))
    }
    lfsr
  }
}