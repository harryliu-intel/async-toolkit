package switch_wm.ppe

import scala.collection.BitSet

/**
  * Flags which may be set or cleared by each parser stage
  */
class PacketFlags(val flags: BitSet) {
  def set(x : Int) : PacketFlags = {
    if (x == 0) this
    else new PacketFlags(flags + x)
  }
  def clear(x : Int) : PacketFlags = {
    if (x== 0) this
    else new PacketFlags(flags - x)
  }
  def assign(x : Int, v : Boolean) : PacketFlags = {
    if (v) { set (x) }
    else { clear (x) }
  }
  def toLong : Long = {
    flags.foldLeft(0l)((acc, bit) => acc | (1 << bit))
  }
}

object PacketFlags {
  /**
    * Used for debug output, this is _typical_ usage as described by the spec
    */
  val typicalUsage : Int => (String, String) = i =>
  {
    i match {
      case 0 => ("NOP", "Not used")
      case 1 => ("otr_l2_vlan1", "Outer VLAN1 is present (required for 802.1Q tag handling, see the chapter in modify)")
      case 2 => ("otr_l2_vlan2", "Outer VLAN2 is present (required for 802.1ad tag handling, see the chapter in modify)")
      case 3 => ("otr_l2_v2first",	"stage	VLAN2 is present but occurs earlier before VLAN1 (required for 802.1ad tag handling, see the chapter in modify)")
      case 4 => ("otr_l4_udp_v", "Stage\tIdentifies the presence of an outer UDP L4 header")
      case 5 => ("otr_l4_tcp_v", "Stage\tIdentifies the presence of an outer TCP L4 header")
      case 6 => ("otr_ipv6_v", "Stage	Indicates outer L3 is IPv6")
      // TODO -- fill in additional values from wiki-spec
      case x if (12 to 20) contains x => ("gf", "Available for future cases not defined currently")
      case x if (32 to 47) contains x => ("gf", "These flags are either generic or potentially usable for IP options trapping. It is up to the user to determined the number of bits to be used.")
    }
  }

  def apply() : PacketFlags = {
    new PacketFlags(BitSet.empty)
  }
}