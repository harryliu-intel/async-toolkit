package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output

object PacketFlags {

  sealed trait GenericPacketFlag {
    def flagOpt: Option[Int] = getGenericFlagId(this)
  }
  case object Flag_NOP                extends GenericPacketFlag
  case object Flag_otr_l2_vlan1       extends GenericPacketFlag
  case object Flag_otr_l2_vlan2       extends GenericPacketFlag
  case object Flag_otr_l2_v2first     extends GenericPacketFlag
  case object Flag_otr_l4_udp_v       extends GenericPacketFlag
  case object Flag_otr_l4_tcp_v       extends GenericPacketFlag
  case object Flag_otr_ipv6_v         extends GenericPacketFlag
  case object Flag_otr_head_frag_v    extends GenericPacketFlag
  case object Flag_otr_payload_frag_v extends GenericPacketFlag
  case object Flag_otr_mpls_v         extends GenericPacketFlag
  case object Flag_otr_l3_v           extends GenericPacketFlag
  case object Flag_otr_l4_v           extends GenericPacketFlag
  case object Flag_inr_ipv6_v         extends GenericPacketFlag
  case object Flag_inr_l2_v           extends GenericPacketFlag
  case object Flag_inr_l2_vlan1       extends GenericPacketFlag
  case object Flag_inr_l2_vlan2       extends GenericPacketFlag
  case object Flag_inr_l2_v2first     extends GenericPacketFlag
  case object Flag_inr_mpls_v         extends GenericPacketFlag
  case object Flag_inr_l3_v           extends GenericPacketFlag
  case object Flag_inr_l4_v           extends GenericPacketFlag
  case object Flag_GeneralFlags       extends GenericPacketFlag

  private val GenericFlags = Map(
    0 -> Flag_NOP,
    1 -> Flag_otr_l2_vlan1,
    2 -> Flag_otr_l2_vlan2,
    3 -> Flag_otr_l2_v2first,
    4 -> Flag_otr_l4_udp_v,
    5 -> Flag_otr_l4_tcp_v,
    6 -> Flag_otr_ipv6_v,
    10 -> Flag_otr_head_frag_v,
    11 -> Flag_otr_payload_frag_v,
    21 -> Flag_otr_mpls_v,
    22 -> Flag_otr_l3_v,
    23 -> Flag_otr_l4_v,
    24 -> Flag_inr_ipv6_v,
    25 -> Flag_inr_l2_v,
    26 -> Flag_inr_l2_vlan1,
    27 -> Flag_inr_l2_vlan2,
    28 -> Flag_inr_l2_v2first,
    29 -> Flag_inr_mpls_v,
    30 -> Flag_inr_l3_v,
    31 -> Flag_inr_l4_v
  )

  //scalastyle:off
  def getGenericFlag(idFlag: Int): Option[GenericPacketFlag] = idFlag match {
    case n if GenericFlags.contains(n) => Some(GenericFlags(n))
    case n if (n >=7 && n <= 9) || (n >= 12 && n <= 20) || (n>=32 && n<=47) => Some(Flag_GeneralFlags)
    case _ => None
  }

  def getGenericFlagId(flag: GenericPacketFlag): Option[Int] = GenericFlags.find { case (_, value) => flag == value }.map(_._1)

}
