package com.intel.cg.hpfd.madisonbay.wm.switchwm.epl

case class Packet(bytes: IndexedSeq[Byte]) extends NetworkPacket

object Packet {

  def strHexToBytesArray(packetData: String): Array[Byte] = packetData.
    grouped(2).
    map(Integer.parseUnsignedInt(_, 16).toByte).  //scalastyle:ignore magic.number
    toArray

  def strHexToPacket(packetData: String): Packet = Packet(strHexToBytesArray(packetData))

}
