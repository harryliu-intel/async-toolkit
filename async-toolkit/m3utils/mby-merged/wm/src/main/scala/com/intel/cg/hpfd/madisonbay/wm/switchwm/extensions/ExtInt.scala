package com.intel.cg.hpfd.madisonbay.wm.switchwm.extensions

object ExtInt {

  implicit class Implicits(val x: Int) {

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
