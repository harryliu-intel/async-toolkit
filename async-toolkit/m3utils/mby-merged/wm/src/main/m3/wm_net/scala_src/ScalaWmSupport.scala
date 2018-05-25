package switch_wm

class BitStruct

object BitStruct {
  def extractField(ba : Array[Byte], bitOffset : Int, fieldSize : Int) : Long = {
    def shiftAndAdd (r : Long, bitPos : Int): Long = {
        // get the relevant byte, shift to the relevant bit in position 0
        // and mask it alone
        val bit = (ba(bitPos / 8) >> (bitPos % 8)) & 0x1
        r << 1 | bit
      }
    (bitOffset until bitOffset + fieldSize).foldLeft(0l)((r, c) => shiftAndAdd(r,c))
  }
  def extractShortField(ba : Array[Byte], bitOffset : Int, fieldSize : Int) : Short = {
    require(fieldSize <= 16)
    extractField(ba, bitOffset, fieldSize).toShort
  }
  def extractBooleanField(ba : Array[Byte], bitOffset : Int, fieldSize : Int) : Boolean = {
    require(fieldSize == 1)
    extractField(ba, bitOffset, fieldSize) == 1
  }

  def setField(a : Array[Byte], bitPos : Int, length : Int, value : Long) = {
    def setBit(bitPos : Int, value : Boolean) = {
      val initByte = a(bitPos / 8)
      // zero out the bit from the byte
      val zeroBit = (initByte & ~(1 << (bitPos % 8)))
      // and then set if, if value = true
      val shifted = value match  {
        case true => (1 << (bitPos % 8))
        case false => 0
      }
      a(bitPos / 8) = (zeroBit | shifted).toByte
    }
    for (i <- 0 until length) {
      setBit(i + bitPos, ((value >> i) & 1l) == 1l)
    }
  }
}

object PrimitiveTypes {
 type U8 = Byte
 type U16 = Short
 type U32 = Int
 type U64 = Long
}
