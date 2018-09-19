package com.intel.cg.hpfd.madisonbay.wm.util

object ImplicitExtensions {
  implicit class RichByte(val self : Byte) {
    def hex: String = {
      f"0x$self%02X"
    }
  }

  implicit class RichByteArray(val self: Array[Byte]) {
    def hexdump(): Unit = {
      print("Dump is: ")
      var count: Int = 0

      def cr(): Unit = {
        if ((count % 16) == 0) print("\n" + count.toHexString + "\t")
        count += 1
      }

      self.foreach(s => {
        cr()
        print(f"$s%02X" + " ")
      }
      )
      println()
    }
  }

  implicit class nibbles(val x : Int) {
    def nib(i : Int) : Int = {
      val mask = 0xf << i
      (x & mask) >> (4 * i)
    }
  }
}
