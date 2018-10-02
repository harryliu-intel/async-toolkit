package com.intel.cg.hpfd.madisonbay.wm.extensions

object ExtByte {

  implicit class Implicits(byte: Byte) {

    def hex: String = {
      f"0x$byte%02X"
    }

  }

}
