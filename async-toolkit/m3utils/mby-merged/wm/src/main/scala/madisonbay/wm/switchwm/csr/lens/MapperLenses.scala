package madisonbay.wm.switchwm.csr.lens

import madisonbay.csr.all._
import monocle.Optional
import monocle.function.Index.index

object MapperLenses {
  def portDefaultTargetLens(port: Int, entry: Int): Optional[mby_ppe_mapper_map, Long] = portDefaultLens(port, entry) composeLens
    map_port_default_r._TARGET composeLens map_port_default_r.TARGET._value
  def portDefaultValueLens(port: Int, entry: Int): Optional[mby_ppe_mapper_map, Long] = portDefaultLens(port, entry) composeLens
    map_port_default_r._VALUE composeLens map_port_default_r.VALUE._value

  private def portDefaultLens(port: Int, entry: Int) = mby_ppe_mapper_map._MAP_PORT_DEFAULT composeOptional
    index(port) composeLens map_port_default_rf._MAP_PORT_DEFAULT composeOptional index(entry)

  def profileActionProfileLens(entry: Int): Optional[mby_ppe_mapper_map, Long] = profileActionLens(entry) composeLens
    map_profile_action_r._PROFILE composeLens map_profile_action_r.PROFILE._value
  def profileActionTrigValidLens(entry: Int): Optional[mby_ppe_mapper_map, Long] = profileActionLens(entry) composeLens
    map_profile_action_r._TRIG_VALID composeLens map_profile_action_r.TRIG_VALID._value
  def profileActionIpOptionsMaskLens(entry: Int): Optional[mby_ppe_mapper_map, Long] = profileActionLens(entry) composeLens
    map_profile_action_r._IP_OPTIONS_MASK composeLens map_profile_action_r.IP_OPTIONS_MASK._value
  def profileActionProfileValidLens(entry: Int): Optional[mby_ppe_mapper_map, Long] = profileActionLens(entry) composeLens
    map_profile_action_r._PROFILE_VALID composeLens map_profile_action_r.PROFILE_VALID._value

  private def profileActionLens(entry: Int) = mby_ppe_mapper_map._MAP_PROFILE_ACTION composeOptional index(entry)

  def macMacLens(entry: Int): Optional[mby_ppe_mapper_map, Long] = macLens(entry) composeLens map_mac_r._MAC composeLens map_mac_r.MAC._value
  def macValidLens(entry: Int): Optional[mby_ppe_mapper_map, Long] = macLens(entry) composeLens map_mac_r._VALID composeLens map_mac_r.VALID._value
  def macMapMacLens(entry: Int): Optional[mby_ppe_mapper_map, Long] = macLens(entry) composeLens map_mac_r._MAP_MAC composeLens map_mac_r.MAP_MAC._value
  private def macLens(entry: Int) = mby_ppe_mapper_map._MAP_MAC composeOptional index(entry)

  def mapRewriteSrcId(output: Int, nibble: Int): Optional[mby_ppe_mapper_map, Long] = mapRewriteLens(output, nibble) composeLens
    map_rewrite_r._SRC_ID composeLens map_rewrite_r.SRC_ID._value
  private def mapRewriteLens(output: Int, nibble: Int) = mby_ppe_mapper_map._MAP_REWRITE composeOptional
    index(output) composeLens map_rewrite_rf._MAP_REWRITE composeOptional index(nibble)

  def mapProtProtLens(entry: Int): Optional[mby_ppe_mapper_map, Long] = mapProtLens(entry) composeLens
    map_prot_r._PROT composeLens map_prot_r.PROT._value
  def mapProtMapProtLens(entry: Int): Optional[mby_ppe_mapper_map, Long] = mapProtLens(entry) composeLens
    map_prot_r._MAP_PROT composeLens map_prot_r.MAP_PROT._value
  private def mapProtLens(entry: Int) = mby_ppe_mapper_map._MAP_PROT composeOptional index(entry)

  def mapL4DstL4Dst(entry: Int): Optional[mby_ppe_mapper_map, Long] = mapL4DstLens(entry) composeLens
    map_l4_dst_r._L4_DST composeLens map_l4_dst_r.L4_DST._value
  def mapL4DstMapProt(entry: Int): Optional[mby_ppe_mapper_map, Long] = mapL4DstLens(entry) composeLens
    map_l4_dst_r._MAP_PROT composeLens map_l4_dst_r.MAP_PROT._value
  def mapL4DstValid(entry: Int): Optional[mby_ppe_mapper_map, Long] = mapL4DstLens(entry) composeLens
    map_l4_dst_r._VALID composeLens map_l4_dst_r.VALID._value
  def mapL4DstMapL4Dst(entry: Int): Optional[mby_ppe_mapper_map, Long] = mapL4DstLens(entry) composeLens
    map_l4_dst_r._MAP_L4_DST composeLens map_l4_dst_r.MAP_L4_DST._value
  private def mapL4DstLens(entry: Int) = mby_ppe_mapper_map._MAP_L4_DST composeOptional index(entry)

  def mapDomainProfilePriorityProfileLens(l2Domain: Int): Optional[mby_ppe_mapper_map, Long] =
    mapDomainProfileLens(l2Domain) composeLens map_domain_profile_r._PRIORITY_PROFILE composeLens
      map_domain_profile_r.PRIORITY_PROFILE._value
  private def mapDomainProfileLens(l2Domain: Int) = mby_ppe_mapper_map._MAP_DOMAIN_PROFILE composeOptional index(l2Domain)

  def mapDomainAction0DefaultPri(entry: Int): Optional[mby_ppe_mapper_map, Long] = mapDomainAction0(entry) composeLens
    map_domain_action0_r._DEFAULT_PRI composeLens map_domain_action0_r.DEFAULT_PRI._value
  def mapDomainAction0PriSource(entry: Int): Optional[mby_ppe_mapper_map, Long] = mapDomainAction0(entry) composeLens
    map_domain_action0_r._PRI_SOURCE composeLens map_domain_action0_r.PRI_SOURCE._value
  def mapDomainAction0LearnMode(entry: Int): Optional[mby_ppe_mapper_map, Long] = mapDomainAction0(entry) composeLens
    map_domain_action0_r._LEARN_MODE composeLens map_domain_action0_r.LEARN_MODE._value
  private def mapDomainAction0(entry: Int) = mby_ppe_mapper_map._MAP_DOMAIN_ACTION0 composeOptional index(entry)

  def mapDomainAction1VlanCounter(entry: Int): Optional[mby_ppe_mapper_map, Long] = mapDomainAction1(entry) composeLens
    map_domain_action1_r._VLAN_COUNTER composeLens map_domain_action1_r.VLAN_COUNTER._value
  private def mapDomainAction1(entry: Int) = mby_ppe_mapper_map._MAP_DOMAIN_ACTION1 composeOptional index(entry)
}
