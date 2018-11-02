package com.intel.cg.hpfd.madisonbay.wm.switchwm.epl

import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.PacketFlags._
import com.intel.cg.hpfd.madisonbay.wm.utils.BitFlags
import org.scalatest._

import scala.collection.BitSet

//scalastyle:off
class PacketFlagsSpec extends FlatSpec with Matchers {

  "Packet Flags" should "properly translate flags IDs" in {
    val pckFlags = BitFlags(BitSet(4,5,8,31)).get
    pckFlags.zip(List(Flag_otr_l4_udp_v, Flag_otr_l4_tcp_v, Flag_GeneralFlags, Flag_inr_l4_v)).
      count { case (flagId, flagObj) => getGenericFlag(flagId).contains(flagObj) } shouldEqual pckFlags.size
  }

  it should "properly find flags by IDs" in {
    val pckFlags = BitFlags(BitSet(4,5,8,31)).get
    // don't count General Flags
    pckFlags.zip(List(Flag_otr_l4_udp_v, Flag_otr_l4_tcp_v, Flag_GeneralFlags, Flag_inr_l4_v)).
      count { case (flagId, flagObj) => getGenericFlagId(flagObj).contains(flagId) } shouldEqual pckFlags.size - 1
  }

  it should "get flag IDs from Generic Flags" in {
    Flag_otr_l4_udp_v.flagOpt shouldEqual Some(4)
  }

}
