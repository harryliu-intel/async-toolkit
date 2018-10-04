package com.intel.cg.hpfd.madisonbay.wm.switchwm.epl

import com.intel.cg.hpfd.madisonbay.wm.utils.extensions.ExtInt.Implicits
import PacketHeader.portionSegmentFPP

object PacketHeader {

  def apply(bytes: Array[Byte]): PacketHeader = new PacketHeader(bytes)

  // Full Packet Processing portion of the header segment (192B)
  // Header information that exceed 192B will be ignored
  val portionSegmentFPP = 192

  // Light Packet Processing portion of the header segment (128B)
  val portionSegmentLPP = 128

}


class PacketHeader(bytes: IndexedSeq[Byte]) {

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

  def getWord(addr: Int): Short = ((apply(addr + 1) << 8) | apply(addr)).toShort

  def ipVersion: IPVersion.Value = bytes(0).nib(1) match {
    case IPVersion.IpV4Int => IPVersion.IPV4
    case IPVersion.IpV6Int => IPVersion.IPV6
  }

  def totalLength: Int = getWord(2)

  /**
    * Validate Packet Length from Header. Only checked for IPv4 packets.
    *
    * @see https://en.wikipedia.org/wiki/IPv4#Header
    * @return
    */
  def ipv4ihlValidate: Boolean = {
    require(ipVersion == IPVersion.IPV4)
    val ipv4_ihl = bytes(0).nib(1)
    val ihlLargeEnough = ipv4_ihl >= 5
    val headerLargeEnough = totalLength >= (4 * ipv4_ihl)
    headerLargeEnough & ihlLargeEnough
  }

}

