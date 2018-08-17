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
  case class Packet(bytes : Array[Byte]) {

  }

  /**
    * Compute 1's complement based IPv4 checksum over an array of bytes
    * @see https://en.wikipedia.org/wiki/IPv4_header_checksum
    * @param bytes
    * @return
    */
  def checksum(bytes : Seq[Byte]) : Short = {
    def foldToShort(x : Int) : Short = {
      val lo = x & 0xffff
      val hi = (x >> 16) & 0xffff
      val folded = lo + hi
      if ((folded >> 16) != 0) foldToShort(folded)
      else folded.toShort
    }
    def addUnsignedShort(acc : Int, addend : Short) : Int = acc + (addend.toInt & 0xffff)

    // sum up the 16-bit words
    val sum = bytes.sliding(2,2).toList.foldLeft(0)(
      { (acc, byte) =>
        if (byte.length == 1) {
          acc + (byte.head & 0xFF)
        }
        else {
          require(byte.length == 2, "Something wrong if sliding give either 1/2 sized lists")
          val hibits = (byte(0) << 8) & 0xff00
          val lowbits = byte(1) & 0xff
          val addend : Int = lowbits + hibits
          addUnsignedShort(acc, addend.toShort )
        }
    })
    // fold the high and low 16 bit components together and bitwise invert the result to get the checksum
    (~foldToShort(sum) & 0xFFFF).toShort
  }

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
  implicit class RichByte(val self : Byte) {
    def hex: String = {
      f"0x$self%02X"
    }
  }
}
