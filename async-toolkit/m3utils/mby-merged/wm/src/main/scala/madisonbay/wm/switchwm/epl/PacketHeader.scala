package madisonbay.wm.switchwm.epl

import madisonbay.wm.utils.extensions.ExtInt.Implicits
import PacketHeader.portionSegmentFPP
import madisonbay.wm.switchwm.epl.IPVersion._

object PacketHeader {

  def apply(packet: Packet): PacketHeader = new PacketHeader(packet.bytes)

  // Full Packet Processing portion of the header segment (192B)
  // Header information that exceed 192B will be ignored
  val portionSegmentFPP = 192

  // Light Packet Processing portion of the header segment (128B)
  val portionSegmentLPP = 128

  implicit def toPacketHeader(packet: Packet): PacketHeader = PacketHeader(packet)

  case class IPv4CorrectCheckSum(outerHeaderBit0: Boolean, innerHeaderBit1: Boolean)

}


class PacketHeader private (val bytes: IndexedSeq[Byte]) extends NetworkPacket {

  /**
    * The packetheader  includes the entire packet
    *
    * condition-end-of-packet
    * Used in parser to help distinguish between exceeding parser depth and
    * exceeding packet size
    */
  val (adjustedSegmentLength: Int, conditionEOP: Boolean) = {
    if (bytes.length < 4) {
      (bytes.length, true)
    } else if ((bytes.length - 4) > portionSegmentFPP) {
      (portionSegmentFPP, false)
    } else {
      (bytes.length - 4, true)
    }
  }

  def trimmed: PacketHeader = new PacketHeader(bytes.slice(0, PacketHeader.portionSegmentFPP))

  def apply(addr: Int): Byte = bytes(addr)

  def ipVersion: IPVersion = bytes(0).nib(1) match {
    case IPVersion.IPv4Int => IPv4
    case IPVersion.IPv6Int => IPv6
    case any => throw new Exception(s"Expected ip4 or ip6, retrieved: $any")
  }

  def totalLength: Int = getWordSafe(2)

  /**
    * Validate Packet Length from Header. Only checked for IPv4 packets.
    *
    * @see https://en.wikipedia.org/wiki/IPv4#Header
    * @return
    */
  def ipv4ihlValidate: Boolean = {
    require(ipVersion == IPv4)
    val ipv4_ihl = bytes(0).nib(1)
    val ihlLargeEnough = ipv4_ihl >= 5
    val headerLargeEnough = totalLength >= (4 * ipv4_ihl)
    headerLargeEnough & ihlLargeEnough
  }

}

