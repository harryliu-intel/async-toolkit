/** Implement an executable spec for the Madison Bay 25 Terabit Switch Product
  *
  * ==Overview==
  * The architecture team provides a 'zero-time' executable spec of the intended architecture for our
  * switch product.
  *
  * ==Limitations==
  * ===Zero-time abstraction===
  * The model does not allow us to simulate more than one packet 'in-flight' at a time, which makes modeling congestion
  * handling difficult.
  * ===Security Policy Groups===
  * The model does not yet implement the security policy described in the SystemRDL, we believe this should be fixed.
 **/
package object switch_wm {
  implicit class RichByteArray(val self : Array[Byte]) {
    def hexdump: Unit = {
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
}
