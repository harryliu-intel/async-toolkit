package com.intel.cg.hpfd.madisonbay.wm.switchwm.pipeline

import com.intel.cg.hpfd.madisonbay.wm.switchwm.extensions.ExtInt.Implicits
import PacketHeader.maxSegmentSize
import com.intel.cg.hpfd.madisonbay.wm.switchwm.util.IPVersion

object PacketHeader {

  def apply(bytes: Array[Byte]): PacketHeader = new PacketHeader(bytes)

  val maxSegmentSize = 192

}


class PacketHeader(val bytes: IndexedSeq[Byte]) {

  val adjustedSegmentLength: Int = {
    if (bytes.length < 4) {
      bytes.length
    } else if ((bytes.length - 4) > maxSegmentSize) {
      maxSegmentSize
    } else {
      bytes.length - 4
    }
  }

  /**
    * The packetheader  includes the entire packet
    *
    * Used in parser to help distinguish between exceeding parser depth and
    * exceeding packet size
    */
  val eop: Boolean = (bytes.length - 4) <= maxSegmentSize

  def apply(addr: Int): Byte = bytes(addr)

  def getWord(addr: Int): Short = ((apply(addr + 1) << 8) | apply(addr)).toShort

  def ipVersion: IPVersion.Value = bytes(0).nib(1) match {
      case IPVersion.IpV4Int => IPVersion.IPV4
      case IPVersion.IpV6Int => IPVersion.IPV6
    }

  def totalLength: Int = {
    getWord(2)
  }

  /**
    * Validate Packet Length from Header. Only checked for IPv4 packets.
    *
    * @see https://en.wikipedia.org/wiki/IPv4#Header
    * @param ph
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

