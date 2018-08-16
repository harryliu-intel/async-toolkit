package com.intel.cg.hpfd.madisonbay.wm.server.dto

import java.io.{DataInputStream, DataOutputStream, InputStream, OutputStream}

object PrimitiveTypes {
  type U8 = Byte
  type U16 = Short
  type U32 = Int
  type U64 = Long


  implicit class PrimitiveTypeInputStream(is : InputStream) extends DataInputStream(is) {
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

  implicit class PrimitiveTypeOutputStream(os : OutputStream) extends DataOutputStream(os) {

    def writeU8(x: U8): Unit = writeByte(x)

    def writeU16(x: U16): Unit = writeShort(x)

    def writeU32(x: U32): Unit = writeInt(x)

    def writeU64(x: U64): Unit = writeLong(x)

    def writeArrayU8(a: Array[U8]): Unit = {
      write(a, 0, a.size)
    }
  }

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