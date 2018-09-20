
package com.intel.cg.hpfd.madisonbay.wm.switchwm.extensions

object ExtArrayByte {

  implicit class RichByteArray(val self: Array[Byte]) {

    def hexdump(): Unit = {
      print("Dump is: ")
      var count: Int = 0

      def cr(): Unit = {
        if ((count % 16) == 0) print("\n" + count.toHexString + "\t")
        count += 1
      }

      self.foreach {s =>
        cr()
        print(f"$s%02X" + " ")
      }
    }

  }

}
