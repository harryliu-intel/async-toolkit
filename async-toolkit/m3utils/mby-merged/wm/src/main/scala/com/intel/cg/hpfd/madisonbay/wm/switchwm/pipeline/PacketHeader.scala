package com.intel.cg.hpfd.madisonbay.wm.switchwm.pipeline

import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe.IPVersion
import com.intel.cg.hpfd.madisonbay.wm.switchwm.extensions.ExtInt.Implicits

object PacketHeader {

  def apply( bytes : Array[Byte]) : PacketHeader = new PacketHeader(bytes)

}


class PacketHeader(val bytes : IndexedSeq[Byte]) {

  val maxSegmentSize = 192
  val adjustedSegmentLength: Int = {
    if (bytes.length < 4) bytes.length
    else if ((bytes.length - 4) > maxSegmentSize) maxSegmentSize
    else bytes.length - 4
  }

  /**
    * The packetheader  includes the entire packet
    *
    * Used in parser to help distinguish between exceeding parser depth and
    * exceeding packet size
    */
  val eop: Boolean = (bytes.length - 4) <= maxSegmentSize

  def apply(addr : Int) : Byte = bytes(addr)
  def getWord(addr : Int) : Short = ((apply(addr + 1) << 8) | apply(addr)).toShort

  def ipVersion : IPVersion.Value = {
    bytes(0).nib(1) match {
      case 0x4 => IPVersion.V4
      case 0x6 => IPVersion.V6
    }
  }
  def totalLength : Int = {
    getWord(2)
  }

}


