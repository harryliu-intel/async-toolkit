package com.intel.cg.hpfd.madisonbay.wm.switchwm

import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.PacketFlags
import com.intel.cg.hpfd.madisonbay.wm.utils.Binary
import org.scalatest._

import scala.collection.BitSet

//scalastyle:off
class PacketFlagsSpec extends FlatSpec with Matchers {

  "Packet Flags" should "Provide 0L on lack of flags" in {
    PacketFlags().toLong shouldEqual 0L
  }

  "Packet Flags" should " fit proper flags" in {
    PacketFlags().set(1).set(3).set(7).toLong.toInt shouldEqual Binary("10001010")
  }

  "Packet Flags" should " fit proper flags with BitSet constructor" in {
    PacketFlags(BitSet(4,5,8)).toInt shouldEqual Binary("100110000")
  }

  "Packet Flags" should " clear flags" in {
    PacketFlags(BitSet(4,5,8)).clear(8).toInt shouldEqual Binary("00110000")
  }

  "Packet Flags" should " assign flags" in {
    PacketFlags(BitSet(4,5,8)).assign(8, false).assign(1, true).toInt shouldEqual Binary("00110010")
  }
}
