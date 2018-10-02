package com.intel.cg.hpfd.madisonbay.wm.extensions

object ExtInt {

  implicit class Implicits(x: Int) extends ExtIntegers[Int] {

    override def addWithSaturation(number1: Int, number2: Long, limit: Long): Int =
      Math.min(number1 + number2.toInt, limit.toInt)

    def nib(i: Int): Int = {
      val mask = 0xf << i
      (x & mask) >> (4 * i)
    }

  }

  def reverseInt(i: Int): Int = {
    ((i >> 24) & 0xff) |
      ((i >> 8) & 0xff00) |
      ((i << 8) & 0x00ff0000) |
      ((i << 24) & 0xff000000)
  }

}
