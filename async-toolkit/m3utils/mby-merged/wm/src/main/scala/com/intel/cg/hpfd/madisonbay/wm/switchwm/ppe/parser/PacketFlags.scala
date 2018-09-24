package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser

import scala.collection.BitSet



/**
  * Parser's output
  * Flags which may be set or cleared by each parser stage
  */
class PacketFlags(val flags: BitSet) {

  def set(x: Int): PacketFlags = {
    if (x == 0) {
      this
    } else {
      new PacketFlags(flags + x)
    }
  }

  def clear(x: Int): PacketFlags = {
    if (x == 0) {
      this
    } else {
      new PacketFlags(flags - x)
    }
  }

  def assign(x: Int, v: Boolean): PacketFlags = {
    if (v) { set (x) }
    else { clear (x) }
  }

  def toLong: Long = {
    flags.foldLeft(0L)((acc, bit) => acc | (1 << bit))
  }

}

object PacketFlags {

  def apply(): PacketFlags = {
    new PacketFlags(BitSet.empty)
  }

  //scalastyle:off
  object TypicalPacketFlags extends Enumeration {
    val NOP: Value = Value(0, "NOP")
    val otr_l2_vlan1: Value = Value(1, "otr_l2_vlan1")
    val otr_l2_valn2: Value = Value(2, "otr_l2_vlan2")
    val otr_l2_v2first: Value = Value(3, "otr_l2_v2first")
    val otr_l4_udp_v: Value = Value(4, "otr_l4_udp_v")
    val otr_l4_tcp_v: Value = Value(5, "otr_l4_tcp_v")
    val otr_ipv6_v: Value = Value(6, "otr_ipv6_v")
    val future: IndexedSeq[Value] = (12 to 20).map(Value(_, "Available for future cases not defined currently"))
    val generic: IndexedSeq[Value] = (32 to 47).map(Value(_, "gf"))
  }
  //scalastyle:on

}
