package switch_wm

class WhiteModelConstants

class BitStruct

object BitStruct {
  def extractField(ba : Array[Byte], bitOffset : Int, fieldSize : Int) : Long = {
    // println("Extracting bitOffset " + bitOffset + " with size " + fieldSize)
    def shiftAndAdd (r : Long, bitPos : Int): Long = {
        // get the relevant byte, shift to the relevant bit in position 0
        // and mask it alone
        val bit = (ba(bitPos / 8) >> (bitPos % 8)) & 0x1
        val result = r << 1 | bit
        // println(" result now " + result.toHexString)
      result
      }
    (bitOffset until bitOffset + fieldSize).foldRight(0l)((r, c) => shiftAndAdd(c,r))
  }
  def extractIntField(ba : Array[Byte], bitOffset : Int, fieldSize : Int) : Int = {
    require(fieldSize <= 32)
    extractField(ba, bitOffset, fieldSize).toInt
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

 implicit def isToPrimitiveTypeInputStream(is : java.io.InputStream) : PrimitiveTypeInputStream = new PrimitiveTypeInputStream(is)
 implicit def osToPrimitiveTypeOutputStream(os : java.io.OutputStream) : PrimitiveTypeOutputStream = new PrimitiveTypeOutputStream(os)

 implicit def intToU8(i : Int) : U8 = { 
    require (i <= 255)
    i.toByte
 }
 implicit def intToU16(i : Int) : U16 = {
    require (i <= (1 << 16) - 1)
    i.toByte
 }
  def unsignedU8toInt(x : U8) : Int = x.toInt& 0xff
  def unsignedU16toInt(x : U16) : Int = x.toInt & 0xffff


}

import java.io._
class PrimitiveTypeInputStream(is : InputStream) extends DataInputStream(is) {
   import PrimitiveTypes._
    def readU8() : U8 = readByte()
    def readU16() : U16 = readShort()
    def readU32() : U32 = readInt()
    def readU64() : U64 = readLong()

    def readArrayU8(size : Int) : Array[U8] = {
       val array = Array.ofDim[U8](size)
       readFully(array)
       array
    }
}

class PrimitiveTypeOutputStream(os : OutputStream) extends DataOutputStream(os) {
   import PrimitiveTypes._
    def writeU8(x : U8) : Unit = writeByte(x)
    def writeU16(x : U16) : Unit = writeShort(x)
    def writeU32(x : U32) : Unit = writeInt(x)
    def writeU64(x : U64) : Unit = writeLong(x)

    def writeArrayU8(a : Array[U8] ) : Unit = {
       write(a,0,a.size)
    }
}


// object PrimitiveTypeInputStream {
//     implicit def isToPrimitiveTypeInputStream(is : InputStream) : PrimitiveTypeInputStream = new PrimitiveTypeInputStream(is)
// }

