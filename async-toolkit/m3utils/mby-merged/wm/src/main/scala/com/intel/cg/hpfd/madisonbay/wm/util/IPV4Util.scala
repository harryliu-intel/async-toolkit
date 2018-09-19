package com.intel.cg.hpfd.madisonbay.wm.util

object IPV4Util {
  /**
    * Compute 1's complement based IPv4 checksum over an array of bytes
    * @see https://en.wikipedia.org/wiki/IPv4_header_checksum
    * @param bytes bytes whose checksum is to be calculated
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
          val hibits = (byte.head << 8) & 0xff00
          val lowbits = byte(1) & 0xff
          val addend : Int = lowbits + hibits
          addUnsignedShort(acc, addend.toShort )
        }
      })
    // fold the high and low 16 bit components together and bitwise invert the result to get the checksum
    (~foldToShort(sum) & 0xFFFF).toShort
  }
}
