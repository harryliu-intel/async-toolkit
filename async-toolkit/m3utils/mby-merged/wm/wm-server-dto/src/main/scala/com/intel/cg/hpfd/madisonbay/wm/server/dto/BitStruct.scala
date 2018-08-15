package com.intel.cg.hpfd.madisonbay.wm.server.dto

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