package madisonbay.wm.switchwm.epl

import madisonbay.wm.utils.extensions.UIntegers._

object IPv4Utils {

  /**
    * Compute 1's complement based IPv4 checksum over an array of bytes
    * @see https://en.wikipedia.org/wiki/IPv4_header_checksum
    * @param bytes bytes whose checksum is to be calculated
    * @return
    */
  def checksum(bytes: Seq[Byte]): Short = {

    def foldToShort(x: Int): Short = {
      val lo = getLower16(x)
      val hi = getLower16(x >> 16)
      val folded = lo + hi
      if ((folded >> 16) != 0) {
        foldToShort(folded)
      } else {
        folded.toShort
      }
    }

    def addUnsignedShort(acc: Int, addend: Short): Int = acc + getLower16(addend.toInt)

    // sum up the 16-bit words
    val sum = bytes.sliding(2,2).toList.foldLeft(0){
      (acc, byte) =>
        if (byte.length == 1) {
          acc + getLower8(byte.head)
        } else {
          require(byte.length == 2, "Something wrong if sliding give either 1/2 sized lists")
          val hibits = getUpper8From16(byte.head << 8)
          val lowbits = getLower8(byte(1))
          val addend: Int = lowbits + hibits
          addUnsignedShort(acc, addend.toShort)
        }
      }
    // fold the high and low 16 bit components together and bitwise invert the result to get the checksum
    getLower16(~foldToShort(sum)).toShort
  }

}
