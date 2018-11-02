package com.intel.cg.hpfd.madisonbay.wm.switchwm.epl

import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.PacketFlags
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.PacketFlags._
import com.intel.cg.hpfd.madisonbay.wm.utils.Binary.BinaryInterpolator
import org.scalatest._

import scala.collection.BitSet

//scalastyle:off
class PacketFlagsSpec extends FlatSpec with Matchers {

  "Packet Flags" should "Provide 0L on lack of flags" in {
    PacketFlags().toLong shouldEqual 0L
  }

  it should "fit proper flags" in {
    PacketFlags().set(1).set(3).set(7).toLong.toInt shouldEqual b"10001010"
  }

  it should "fit proper flags with BitSet constructor" in {
    PacketFlags(BitSet(4,5,8)).toInt shouldEqual b"100110000"
  }

  it should "clear flags" in {
    PacketFlags(BitSet(4,5,8)).clear(8).toInt shouldEqual b"00110000"
  }

  it should "assign flags" in {
    PacketFlags(BitSet(4,5,8)).assign(8, false).assign(1, true).toInt shouldEqual b"00110010"
  }

  it should "properly translate flags IDs" in {
    val pckFlags = PacketFlags(BitSet(4,5,8,31)).get
    pckFlags.zip(List(Flag_otr_l4_udp_v, Flag_otr_l4_tcp_v, Flag_GeneralFlags, Flag_inr_l4_v)).
      count { case (flagId, flagObj) => getGenericFlag(flagId).contains(flagObj) } shouldEqual pckFlags.size
  }

  it should "properly find flags by IDs" in {
    val pckFlags = PacketFlags(BitSet(4,5,8,31)).get
    // don't count General Flags
    pckFlags.zip(List(Flag_otr_l4_udp_v, Flag_otr_l4_tcp_v, Flag_GeneralFlags, Flag_inr_l4_v)).
      count { case (flagId, flagObj) => getGenericFlagId(flagObj).contains(flagId) } shouldEqual pckFlags.size - 1
  }

}
