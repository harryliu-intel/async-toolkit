package madisonbay.wm.switchwm.ppe.parser.defs

import madisonbay.wm.utils.defs.IndexedConstant

//scalastyle:off magic.number
object PacketFlags extends IndexedConstant.Container {

  sealed trait GenericPacketFlag      extends IndexedConstant.Element
  case object Flag_NOP                extends GenericPacketFlag { val index = Some(0) }
  case object Flag_otr_l2_vlan1       extends GenericPacketFlag { val index = Some(1) }
  case object Flag_otr_l2_vlan2       extends GenericPacketFlag { val index = Some(2) }
  case object Flag_otr_l2_v2first     extends GenericPacketFlag { val index = Some(3) }
  case object Flag_otr_l4_udp_v       extends GenericPacketFlag { val index = Some(4) }
  case object Flag_otr_l4_tcp_v       extends GenericPacketFlag { val index = Some(5) }
  case object Flag_otr_ipv6_v         extends GenericPacketFlag { val index = Some(6) }
  case object Flag_otr_head_frag_v    extends GenericPacketFlag { val index = Some(10) }
  case object Flag_otr_payload_frag_v extends GenericPacketFlag { val index = Some(11) }
  case object Flag_otr_mpls_v         extends GenericPacketFlag { val index = Some(21) }
  case object Flag_otr_l3_v           extends GenericPacketFlag { val index = Some(22) }
  case object Flag_otr_l4_v           extends GenericPacketFlag { val index = Some(23) }
  case object Flag_inr_ipv6_v         extends GenericPacketFlag { val index = Some(24) }
  case object Flag_inr_l2_v           extends GenericPacketFlag { val index = Some(25) }
  case object Flag_inr_l2_vlan1       extends GenericPacketFlag { val index = Some(26) }
  case object Flag_inr_l2_vlan2       extends GenericPacketFlag { val index = Some(27) }
  case object Flag_inr_l2_v2first     extends GenericPacketFlag { val index = Some(28) }
  case object Flag_inr_mpls_v         extends GenericPacketFlag { val index = Some(29) }
  case object Flag_inr_l3_v           extends GenericPacketFlag { val index = Some(30) }
  case object Flag_inr_l4_v           extends GenericPacketFlag { val index = Some(31) }
  case object Flag_GeneralFlags       extends GenericPacketFlag { val index = Option.empty[Int] }

  val constants = List(
    Flag_NOP,
    Flag_otr_l2_vlan1,
    Flag_otr_l2_vlan2,
    Flag_otr_l2_v2first,
    Flag_otr_l4_udp_v,
    Flag_otr_l4_tcp_v,
    Flag_otr_ipv6_v,
    Flag_otr_head_frag_v,
    Flag_otr_payload_frag_v,
    Flag_otr_mpls_v,
    Flag_otr_l3_v,
    Flag_otr_l4_v,
    Flag_inr_ipv6_v,
    Flag_inr_l2_v,
    Flag_inr_l2_vlan1,
    Flag_inr_l2_vlan2,
    Flag_inr_l2_v2first,
    Flag_inr_mpls_v,
    Flag_inr_l3_v,
    Flag_inr_l4_v
  )

  def getFlag(idFlag: Int): Option[GenericPacketFlag] = (idFlag, getConstant(idFlag)) match {
    case (_, Some(flag)) => Some(flag.asInstanceOf[GenericPacketFlag])
    case (n, _) if (n >=7 && n <= 9) || (n >= 12 && n <= 20) || (n>=32 && n<=47) => Some(Flag_GeneralFlags)
    case _ => None
  }

}
