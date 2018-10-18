package com.intel.cg.hpfd.madisonbay.wm.switchwm.epl

case class Packet(bytes: Array[Byte])

//scalastyle:off magic.number
object Packet {

  def strHexToBytesArray(packetData: String): Array[Byte] = packetData.
    toUpperCase.
    grouped(2).
    map(Integer.parseUnsignedInt(_, 16).toByte).
    toArray

  def strHexToPacket(packetData: String): Packet = Packet(strHexToBytesArray(packetData))

}
