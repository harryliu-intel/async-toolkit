package com.intel.cg.hpfd.madisonbay.wm.switchwm.extensions


object ExtShort {

  implicit class Implicits(val self : Byte) {

    def hex: String = {
      f"0x$self%02X"
    }

  }

  def reverseShort(s: Short): Short = {
    (((s >> 8) & 0xff) | (s << 8)).toShort
  }

}
