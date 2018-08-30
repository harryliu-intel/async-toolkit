// This file is auto generated with tools/dump_gen/dump_gen.py
#include "mby_log_dump.h"
#include <stdio.h>
void DUMP_mbyParserToMapper(const mbyParserToMapper * const s) {
  printf("---- BEGIN struct mbyParserToMapper\n");
  printf(" RX_PORT = %d\n", s->RX_PORT);
  printf(" PA_ADJ_SEG_LEN = %d\n", s->PA_ADJ_SEG_LEN);
  printf(" PA_KEYS = {");
  for(int i=0; i < 80; ++i)
    printf("%d, ", s->PA_KEYS[i]);
  printf("}\n");
  printf(" PA_KEYS_VALID = {");
  for(int i=0; i < 80; ++i)
    printf("%d, ", s->PA_KEYS_VALID[i]);
  printf("}\n");
  printf(" PA_FLAGS = {");
  for(int i=0; i < 48; ++i)
    printf("%d, ", s->PA_FLAGS[i]);
  printf("}\n");
  printf(" PA_PTRS = {");
  for(int i=0; i < 8; ++i)
    printf("%d, ", s->PA_PTRS[i]);
  printf("}\n");
  printf(" PA_PTRS_VALID = {");
  for(int i=0; i < 8; ++i)
    printf("%d, ", s->PA_PTRS_VALID[i]);
  printf("}\n");
  printf(" PA_CSUM_OK = %d\n", s->PA_CSUM_OK);
  printf(" PA_EX_STAGE = %d\n", s->PA_EX_STAGE);
  printf(" PA_EX_DEPTH_EXCEED = %s\n", s->PA_EX_DEPTH_EXCEED ? "TRUE" : "FALSE");
  printf(" PA_EX_TRUNC_HEADER = %s\n", s->PA_EX_TRUNC_HEADER ? "TRUE" : "FALSE");
  printf(" PA_EX_PARSING_DONE = %s\n", s->PA_EX_PARSING_DONE ? "TRUE" : "FALSE");
  printf(" PA_DROP = %s\n", s->PA_DROP ? "TRUE" : "FALSE");
  printf(" PA_L3LEN_ERR = %s\n", s->PA_L3LEN_ERR ? "TRUE" : "FALSE");
  printf(" PA_PACKET_TYPE = %d\n", s->PA_PACKET_TYPE);
  printf("---- END struct mbyParserToMapper\n");
}
void DUMP_mbyMapperToClassifier(const mbyMapperToClassifier * const s) {
  printf("---- BEGIN struct mbyMapperToClassifier\n");
  // ----- struct mbyParserInfo
  printf(" PARSER_INFO.otr_l2_len = %d\n", s->PARSER_INFO.otr_l2_len);
  printf(" PARSER_INFO.otr_l2_vlan1 = %s\n", s->PARSER_INFO.otr_l2_vlan1 ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.otr_l2_vlan2 = %s\n", s->PARSER_INFO.otr_l2_vlan2 ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.otr_l2_v2first = %s\n", s->PARSER_INFO.otr_l2_v2first ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.otr_mpls_len = %d\n", s->PARSER_INFO.otr_mpls_len);
  printf(" PARSER_INFO.otr_l3_len = %d\n", s->PARSER_INFO.otr_l3_len);
  printf(" PARSER_INFO.otr_l3_v6 = %s\n", s->PARSER_INFO.otr_l3_v6 ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.otr_l4_udp = %s\n", s->PARSER_INFO.otr_l4_udp ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.otr_l4_tcp = %s\n", s->PARSER_INFO.otr_l4_tcp ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.otr_tun_len = %d\n", s->PARSER_INFO.otr_tun_len);
  printf(" PARSER_INFO.inr_l2_len = %d\n", s->PARSER_INFO.inr_l2_len);
  printf(" PARSER_INFO.inr_l2_vlan1 = %s\n", s->PARSER_INFO.inr_l2_vlan1 ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.inr_l2_vlan2 = %s\n", s->PARSER_INFO.inr_l2_vlan2 ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.inr_l2_v2first = %s\n", s->PARSER_INFO.inr_l2_v2first ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.inr_mpls_len = %d\n", s->PARSER_INFO.inr_mpls_len);
  printf(" PARSER_INFO.inr_l3_len = %d\n", s->PARSER_INFO.inr_l3_len);
  printf(" PARSER_INFO.inr_l3_v6 = %s\n", s->PARSER_INFO.inr_l3_v6 ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.inr_l4_udp = %s\n", s->PARSER_INFO.inr_l4_udp ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.inr_l4_tcp = %s\n", s->PARSER_INFO.inr_l4_tcp ? "TRUE" : "FALSE");
  printf(" PARSER_ERROR = %s\n", s->PARSER_ERROR ? "TRUE" : "FALSE");
  printf(" OTR_MPLS_V = %s\n", s->OTR_MPLS_V ? "TRUE" : "FALSE");
  // ----- struct mbyClassifierKeys
  printf(" FFU_KEYS.key32 = {");
  for(int i=0; i < 16; ++i)
    printf("%d, ", s->FFU_KEYS.key32[i]);
  printf("}\n");
  printf(" FFU_KEYS.key16 = {");
  for(int i=0; i < 32; ++i)
    printf("%d, ", s->FFU_KEYS.key16[i]);
  printf("}\n");
  printf(" FFU_KEYS.key8 = {");
  for(int i=0; i < 64; ++i)
    printf("%d, ", s->FFU_KEYS.key8[i]);
  printf("}\n");
  // ----- struct mbyClassifierActions
  printf(" FFU_ACTIONS.act24 = UNPRINTABLE");
  printf(" FFU_ACTIONS.act4 = UNPRINTABLE");
  printf(" FFU_ACTIONS.act1 = UNPRINTABLE");
  printf(" FFU_SCENARIO = %d\n", s->FFU_SCENARIO);
  printf(" FFU_VRID = %d\n", s->FFU_VRID);
  printf(" IP_OPTION = {");
  for(int i=0; i < 2; ++i)
    printf("%d, ", s->IP_OPTION[i]);
  printf("}\n");
  printf(" OPERATOR_ID = %s\n", s->OPERATOR_ID ? "TRUE" : "FALSE");
  printf(" L2_IDOMAIN = %d\n", s->L2_IDOMAIN);
  printf(" L3_IDOMAIN = %d\n", s->L3_IDOMAIN);
  printf(" PRIORITY_PROFILE = %d\n", s->PRIORITY_PROFILE);
  printf(" NO_PRI_ENC = %s\n", s->NO_PRI_ENC ? "TRUE" : "FALSE");
  printf(" LEARN_MODE = %s\n", s->LEARN_MODE ? "TRUE" : "FALSE");
  printf(" L2_IVLAN1_CNT = %d\n", s->L2_IVLAN1_CNT);
  printf("---- END struct mbyMapperToClassifier\n");
}
void DUMP_mbyClassifierToHash(const mbyClassifierToHash * const s) {
  printf("---- BEGIN struct mbyClassifierToHash\n");
  printf(" L34_HASH = %lld\n", s->L34_HASH);
  printf(" L2_IDOMAIN = %d\n", s->L2_IDOMAIN);
  printf(" L3_IDOMAIN = %d\n", s->L3_IDOMAIN);
  printf(" MOD_IDX = %d\n", s->MOD_IDX);
  printf(" L2_ETYPE = %d\n", s->L2_ETYPE);
  printf(" MPLS_POP = %d\n", s->MPLS_POP);
  printf(" ENCAP = %s\n", s->ENCAP ? "TRUE" : "FALSE");
  printf(" DECAP = %s\n", s->DECAP ? "TRUE" : "FALSE");
  printf(" QOS_SWPRI = %d\n", s->QOS_SWPRI);
  printf(" SGLORT = %d\n", s->SGLORT);
  printf(" IDGLORT = %d\n", s->IDGLORT);
  printf(" IS_IPV4 = %s\n", s->IS_IPV4 ? "TRUE" : "FALSE");
  printf(" IS_IPV6 = %s\n", s->IS_IPV6 ? "TRUE" : "FALSE");
  printf(" L3_LENGTH = %d\n", s->L3_LENGTH);
  printf(" OUTER_L3_LENGTH = %d\n", s->OUTER_L3_LENGTH);
  printf(" INNER_L3_LENGTH = %d\n", s->INNER_L3_LENGTH);
  printf(" TRAP_IP_OPTIONS = %s\n", s->TRAP_IP_OPTIONS ? "TRUE" : "FALSE");
  printf(" DROP_TTL = %s\n", s->DROP_TTL ? "TRUE" : "FALSE");
  printf(" TRAP_ICMP = %s\n", s->TRAP_ICMP ? "TRUE" : "FALSE");
  printf(" TRAP_IGMP = %s\n", s->TRAP_IGMP ? "TRUE" : "FALSE");
  printf(" TTL_CTRL = %d\n", s->TTL_CTRL);
  // ----- struct mbyClassifierFlags
  printf(" FFU_FLAGS.drop = %s\n", s->FFU_FLAGS.drop ? "TRUE" : "FALSE");
  printf(" FFU_FLAGS.trap = %s\n", s->FFU_FLAGS.trap ? "TRUE" : "FALSE");
  printf(" FFU_FLAGS.log = %s\n", s->FFU_FLAGS.log ? "TRUE" : "FALSE");
  printf(" FFU_FLAGS.no_route = %s\n", s->FFU_FLAGS.no_route ? "TRUE" : "FALSE");
  printf(" FFU_FLAGS.rx_mirror = %s\n", s->FFU_FLAGS.rx_mirror ? "TRUE" : "FALSE");
  printf(" FFU_FLAGS.capture_time = %s\n", s->FFU_FLAGS.capture_time ? "TRUE" : "FALSE");
  printf(" FFU_FLAGS.tx_tag = %d\n", s->FFU_FLAGS.tx_tag);
  printf(" FFU_ROUTE = %d\n", s->FFU_ROUTE);
  printf(" NO_LEARN = %s\n", s->NO_LEARN ? "TRUE" : "FALSE");
  printf(" L2_IVID1 = %d\n", s->L2_IVID1);
  printf(" TX_TAG = %d\n", s->TX_TAG);
  printf(" QOS_L2_VPRI1 = %d\n", s->QOS_L2_VPRI1);
  printf(" FFU_TRIG = %d\n", s->FFU_TRIG);
  printf(" QOS_L3_DSCP = %d\n", s->QOS_L3_DSCP);
  printf(" POLICER_ACTION = {");
  for(int i=0; i < 4; ++i)
    printf("%d, ", s->POLICER_ACTION[i]);
  printf("}\n");
  printf(" PARITY_ERROR = %s\n", s->PARITY_ERROR ? "TRUE" : "FALSE");
  printf(" ECN = %d\n", s->ECN);
  printf(" AQM_MARK_EN = %d\n", s->AQM_MARK_EN);
  printf(" LEARN_MODE = %s\n", s->LEARN_MODE ? "TRUE" : "FALSE");
  printf("---- END struct mbyClassifierToHash\n");
}
void DUMP_mbyHashToNextHop(const mbyHashToNextHop * const s) {
  printf("---- BEGIN struct mbyHashToNextHop\n");
  // ----- struct mbyHashKeys
  printf(" HASH_KEYS.crc34 = %lld\n", s->HASH_KEYS.crc34);
  printf(" HASH_KEYS.crc234 = %lld\n", s->HASH_KEYS.crc234);
  printf(" HASH_KEYS.l234Key = {");
  for(int i=0; i < 17; ++i)
    printf("%d, ", s->HASH_KEYS.l234Key[i]);
  printf("}\n");
  printf(" HASH_KEYS.zeroL2 = %s\n", s->HASH_KEYS.zeroL2 ? "TRUE" : "FALSE");
  printf(" HASH_KEYS.zeroL34 = %s\n", s->HASH_KEYS.zeroL34 ? "TRUE" : "FALSE");
  printf(" HASH_KEYS.useL34 = %s\n", s->HASH_KEYS.useL34 ? "TRUE" : "FALSE");
  printf(" HASH_KEYS.rotA = %d\n", s->HASH_KEYS.rotA);
  printf(" HASH_KEYS.rotB = %d\n", s->HASH_KEYS.rotB);
  printf(" HASH_KEYS.arpEcmpCfg = %s\n", s->HASH_KEYS.arpEcmpCfg ? "TRUE" : "FALSE");
  printf(" HASH_ROT_A_PTABLE_INDEX = %d\n", s->HASH_ROT_A_PTABLE_INDEX);
  printf(" HASH_ROT_B_PTABLE_INDEX = %d\n", s->HASH_ROT_B_PTABLE_INDEX);
  printf(" HASH_ROT_A = %d\n", s->HASH_ROT_A);
  printf(" HASH_ROT_B = %d\n", s->HASH_ROT_B);
  printf(" RAW_HASH = %d\n", s->RAW_HASH);
  printf(" ARP_HASH = {");
  for(int i=0; i < 16; ++i)
    printf("%d, ", s->ARP_HASH[i]);
  printf("}\n");
  // ----- struct mbyClassifierFlags
  printf(" FFU_FLAGS.drop = %s\n", s->FFU_FLAGS.drop ? "TRUE" : "FALSE");
  printf(" FFU_FLAGS.trap = %s\n", s->FFU_FLAGS.trap ? "TRUE" : "FALSE");
  printf(" FFU_FLAGS.log = %s\n", s->FFU_FLAGS.log ? "TRUE" : "FALSE");
  printf(" FFU_FLAGS.no_route = %s\n", s->FFU_FLAGS.no_route ? "TRUE" : "FALSE");
  printf(" FFU_FLAGS.rx_mirror = %s\n", s->FFU_FLAGS.rx_mirror ? "TRUE" : "FALSE");
  printf(" FFU_FLAGS.capture_time = %s\n", s->FFU_FLAGS.capture_time ? "TRUE" : "FALSE");
  printf(" FFU_FLAGS.tx_tag = %d\n", s->FFU_FLAGS.tx_tag);
  printf(" FFU_ROUTE = %d\n", s->FFU_ROUTE);
  printf(" ENCAP = %s\n", s->ENCAP ? "TRUE" : "FALSE");
  printf(" DECAP = %s\n", s->DECAP ? "TRUE" : "FALSE");
  printf(" L2_IDOMAIN = %d\n", s->L2_IDOMAIN);
  printf(" L3_IDOMAIN = %d\n", s->L3_IDOMAIN);
  printf(" L2_IVID1 = %d\n", s->L2_IVID1);
  printf(" LEARN_MODE = %s\n", s->LEARN_MODE ? "TRUE" : "FALSE");
  printf("---- END struct mbyHashToNextHop\n");
}
void DUMP_mbyNextHopToMaskGen(const mbyNextHopToMaskGen * const s) {
  printf("---- BEGIN struct mbyNextHopToMaskGen\n");
  printf(" ARP_TABLE_INDEX = %d\n", s->ARP_TABLE_INDEX);
  printf(" ENCAP = %s\n", s->ENCAP ? "TRUE" : "FALSE");
  printf(" DECAP = %s\n", s->DECAP ? "TRUE" : "FALSE");
  printf(" L2_IDOMAIN = %d\n", s->L2_IDOMAIN);
  printf(" L3_IDOMAIN = %d\n", s->L3_IDOMAIN);
  printf(" L2_IVID1 = %d\n", s->L2_IVID1);
  printf(" L2_EDOMAIN = %d\n", s->L2_EDOMAIN);
  printf(" L3_EDOMAIN = %d\n", s->L3_EDOMAIN);
  printf(" L2_EVID1 = %d\n", s->L2_EVID1);
  printf(" MTU_INDEX = %d\n", s->MTU_INDEX);
  printf(" FLOOD_SET = %s\n", s->FLOOD_SET ? "TRUE" : "FALSE");
  printf(" IDGLORT = %d\n", s->IDGLORT);
  printf(" MARK_ROUTED = %s\n", s->MARK_ROUTED ? "TRUE" : "FALSE");
  printf(" MOD_IDX = %d\n", s->MOD_IDX);
  printf(" RX_PORT = %d\n", s->RX_PORT);
  printf(" PARSER_WINDOW_V = %s\n", s->PARSER_WINDOW_V ? "TRUE" : "FALSE");
  printf(" PARSER_ERROR = %s\n", s->PARSER_ERROR ? "TRUE" : "FALSE");
  printf(" PARITY_ERROR = %s\n", s->PARITY_ERROR ? "TRUE" : "FALSE");
  printf(" L2_ETYPE = %d\n", s->L2_ETYPE);
  // ----- enum mbySTPState
  switch(s->L2_IFID1_STATE) {
  case MBY_STP_STATE_DISABLE:
    printf("L2_IFID1_STATE = MBY_STP_STATE_DISABLE\n");
    break;
  case MBY_STP_STATE_LISTENING:
    printf("L2_IFID1_STATE = MBY_STP_STATE_LISTENING\n");
    break;
  case MBY_STP_STATE_LEARNING:
    printf("L2_IFID1_STATE = MBY_STP_STATE_LEARNING\n");
    break;
  case MBY_STP_STATE_FORWARD:
    printf("L2_IFID1_STATE = MBY_STP_STATE_FORWARD\n");
    break;
  default:
    printf("L2_IFID1_STATE = %d (Unknown?)\n", s->L2_IFID1_STATE);
  }
  printf(" L2_EFID1_STATE = %d\n", s->L2_EFID1_STATE);
  printf(" L2_IVLAN1_MEMBERSHIP = %s\n", s->L2_IVLAN1_MEMBERSHIP ? "TRUE" : "FALSE");
  printf(" L2_IVLAN1_REFLECT = %s\n", s->L2_IVLAN1_REFLECT ? "TRUE" : "FALSE");
  printf(" L2_EVLAN1_MEMBERSHIP = %d\n", s->L2_EVLAN1_MEMBERSHIP);
  printf(" NO_LEARN = %s\n", s->NO_LEARN ? "TRUE" : "FALSE");
  printf(" GLORT_CAM_MISS = %s\n", s->GLORT_CAM_MISS ? "TRUE" : "FALSE");
  printf(" GLORT_FORWARDED = %s\n", s->GLORT_FORWARDED ? "TRUE" : "FALSE");
  printf(" GLORT_DMASK = %d\n", s->GLORT_DMASK);
  printf(" TARGETED_DETERMINISTIC = %s\n", s->TARGETED_DETERMINISTIC ? "TRUE" : "FALSE");
  printf(" CPU_TRAP = %s\n", s->CPU_TRAP ? "TRUE" : "FALSE");
  printf(" TRAP_ICMP = %s\n", s->TRAP_ICMP ? "TRUE" : "FALSE");
  printf(" TRAP_IGMP = %s\n", s->TRAP_IGMP ? "TRUE" : "FALSE");
  printf(" TRAP_IP_OPTIONS = %s\n", s->TRAP_IP_OPTIONS ? "TRUE" : "FALSE");
  printf(" PRE_RESOLVE_DMASK = %d\n", s->PRE_RESOLVE_DMASK);
  printf(" ACTION = %d\n", s->ACTION);
  printf(" OPERATOR_ID = %d\n", s->OPERATOR_ID);
  printf(" QOS_SWPRI = %d\n", s->QOS_SWPRI);
  // ----- struct mbyTriggerResults
  printf(" TRIGGERS.action = %d\n", s->TRIGGERS.action);
  // ----- enum mbyTriggerActionForwarding
  switch(s->TRIGGERS.forwardingAction) {
  case MBY_TRIG_ACTION_FORWARDING_AS_IS:
    printf("TRIGGERS.forwardingAction = MBY_TRIG_ACTION_FORWARDING_AS_IS\n");
    break;
  case MBY_TRIG_ACTION_FORWARDING_FORWARD:
    printf("TRIGGERS.forwardingAction = MBY_TRIG_ACTION_FORWARDING_FORWARD\n");
    break;
  case MBY_TRIG_ACTION_FORWARDING_REDIRECT:
    printf("TRIGGERS.forwardingAction = MBY_TRIG_ACTION_FORWARDING_REDIRECT\n");
    break;
  case MBY_TRIG_ACTION_FORWARDING_DROP:
    printf("TRIGGERS.forwardingAction = MBY_TRIG_ACTION_FORWARDING_DROP\n");
    break;
  default:
    printf("TRIGGERS.forwardingAction = %d (Unknown?)\n", s->TRIGGERS.forwardingAction);
  }
  printf(" TRIGGERS.destGlort = %d\n", s->TRIGGERS.destGlort);
  printf(" TRIGGERS.destMask = %lld\n", s->TRIGGERS.destMask);
  printf(" TRIGGERS.filterDestMask = %s\n", s->TRIGGERS.filterDestMask ? "TRUE" : "FALSE");
  // ----- enum mbyTriggerActionTrap
  switch(s->TRIGGERS.trapAction) {
  case MBY_TRIG_ACTION_TRAP_AS_IS:
    printf("TRIGGERS.trapAction = MBY_TRIG_ACTION_TRAP_AS_IS\n");
    break;
  case MBY_TRIG_ACTION_TRAP_TRAP:
    printf("TRIGGERS.trapAction = MBY_TRIG_ACTION_TRAP_TRAP\n");
    break;
  case MBY_TRIG_ACTION_TRAP_LOG:
    printf("TRIGGERS.trapAction = MBY_TRIG_ACTION_TRAP_LOG\n");
    break;
  case MBY_TRIG_ACTION_TRAP_REVERT:
    printf("TRIGGERS.trapAction = MBY_TRIG_ACTION_TRAP_REVERT\n");
    break;
  default:
    printf("TRIGGERS.trapAction = %d (Unknown?)\n", s->TRIGGERS.trapAction);
  }
  printf(" TRIGGERS.cpuCode = %d\n", s->TRIGGERS.cpuCode);
  printf(" TRIGGERS.trapCode = %d\n", s->TRIGGERS.trapCode);
  printf(" TRIGGERS.logAction = %s\n", s->TRIGGERS.logAction ? "TRUE" : "FALSE");
  // ----- enum mbyTriggerActionMirroring
  switch(s->TRIGGERS.mirroringAction0) {
  case MBY_TRIG_ACTION_MIRRORING_AS_IS:
    printf("TRIGGERS.mirroringAction0 = MBY_TRIG_ACTION_MIRRORING_AS_IS\n");
    break;
  case MBY_TRIG_ACTION_MIRRORING_MIRROR:
    printf("TRIGGERS.mirroringAction0 = MBY_TRIG_ACTION_MIRRORING_MIRROR\n");
    break;
  case MBY_TRIG_ACTION_MIRRORING_CANCEL:
    printf("TRIGGERS.mirroringAction0 = MBY_TRIG_ACTION_MIRRORING_CANCEL\n");
    break;
  default:
    printf("TRIGGERS.mirroringAction0 = %d (Unknown?)\n", s->TRIGGERS.mirroringAction0);
  }
  // ----- enum mbyTriggerActionMirroring
  switch(s->TRIGGERS.mirroringAction1) {
  case MBY_TRIG_ACTION_MIRRORING_AS_IS:
    printf("TRIGGERS.mirroringAction1 = MBY_TRIG_ACTION_MIRRORING_AS_IS\n");
    break;
  case MBY_TRIG_ACTION_MIRRORING_MIRROR:
    printf("TRIGGERS.mirroringAction1 = MBY_TRIG_ACTION_MIRRORING_MIRROR\n");
    break;
  case MBY_TRIG_ACTION_MIRRORING_CANCEL:
    printf("TRIGGERS.mirroringAction1 = MBY_TRIG_ACTION_MIRRORING_CANCEL\n");
    break;
  default:
    printf("TRIGGERS.mirroringAction1 = %d (Unknown?)\n", s->TRIGGERS.mirroringAction1);
  }
  printf(" TRIGGERS.rxMirror = %s\n", s->TRIGGERS.rxMirror ? "TRUE" : "FALSE");
  printf(" TRIGGERS.mirrorProfileIndex0 = %d\n", s->TRIGGERS.mirrorProfileIndex0);
  printf(" TRIGGERS.mirrorProfileIndex1 = %d\n", s->TRIGGERS.mirrorProfileIndex1);
  printf(" TRIGGERS.mirror0ProfileV = %s\n", s->TRIGGERS.mirror0ProfileV ? "TRUE" : "FALSE");
  printf(" TRIGGERS.mirror1ProfileV = %s\n", s->TRIGGERS.mirror1ProfileV ? "TRUE" : "FALSE");
  printf(" TRIGGERS.mirror0ProfileIdx = %d\n", s->TRIGGERS.mirror0ProfileIdx);
  printf(" TRIGGERS.mirror1ProfileIdx = %d\n", s->TRIGGERS.mirror1ProfileIdx);
  // ----- enum mbyTriggerActionTC
  switch(s->TRIGGERS.TCAction) {
  case MBY_TRIG_ACTION_TC_AS_IS:
    printf("TRIGGERS.TCAction = MBY_TRIG_ACTION_TC_AS_IS\n");
    break;
  case MBY_TRIG_ACTION_TC_REASSIGN:
    printf("TRIGGERS.TCAction = MBY_TRIG_ACTION_TC_REASSIGN\n");
    break;
  default:
    printf("TRIGGERS.TCAction = %d (Unknown?)\n", s->TRIGGERS.TCAction);
  }
  printf(" TRIGGERS.TC = %d\n", s->TRIGGERS.TC);
  // ----- enum mbyTriggerActionVlan
  switch(s->TRIGGERS.vlanAction) {
  case MBY_TRIG_ACTION_VLAN_AS_IS:
    printf("TRIGGERS.vlanAction = MBY_TRIG_ACTION_VLAN_AS_IS\n");
    break;
  case MBY_TRIG_ACTION_VLAN_REASSIGN:
    printf("TRIGGERS.vlanAction = MBY_TRIG_ACTION_VLAN_REASSIGN\n");
    break;
  default:
    printf("TRIGGERS.vlanAction = %d (Unknown?)\n", s->TRIGGERS.vlanAction);
  }
  printf(" TRIGGERS.vlan = %d\n", s->TRIGGERS.vlan);
  // ----- enum mbyTriggerActionLearning
  switch(s->TRIGGERS.learningAction) {
  case MBY_TRIG_ACTION_LEARNING_AS_IS:
    printf("TRIGGERS.learningAction = MBY_TRIG_ACTION_LEARNING_AS_IS\n");
    break;
  case MBY_TRIG_ACTION_LEARNING_DONT_LEARN:
    printf("TRIGGERS.learningAction = MBY_TRIG_ACTION_LEARNING_DONT_LEARN\n");
    break;
  case MBY_TRIG_ACTION_LEARNING_FORCE_LEARN:
    printf("TRIGGERS.learningAction = MBY_TRIG_ACTION_LEARNING_FORCE_LEARN\n");
    break;
  default:
    printf("TRIGGERS.learningAction = %d (Unknown?)\n", s->TRIGGERS.learningAction);
  }
  printf(" TRIGGERS.rateLimitAction = %s\n", s->TRIGGERS.rateLimitAction ? "TRUE" : "FALSE");
  printf(" TRIGGERS.rateLimitNum = %d\n", s->TRIGGERS.rateLimitNum);
  printf(" TRIGGERS.metadataTrigNum = {");
  for(int i=0; i < 4; ++i)
    printf("%d, ", s->TRIGGERS.metadataTrigNum[i]);
  printf("}\n");
  printf(" TRIGGERS.metadataAction = {");
  for(int i=0; i < 4; ++i)
    printf("%d, ", s->TRIGGERS.metadataAction[i]);
  printf("}\n");
  printf(" TRIGGERS.egressL2DomainAction = %d\n", s->TRIGGERS.egressL2DomainAction);
  printf(" TRIGGERS.egressL3DomainAction = %d\n", s->TRIGGERS.egressL3DomainAction);
  printf(" TRIGGERS.qcnValid0 = %d\n", s->TRIGGERS.qcnValid0);
  printf(" TRIGGERS.qcnValid1 = %d\n", s->TRIGGERS.qcnValid1);
  printf(" TRIGGERS.policerAction = %d\n", s->TRIGGERS.policerAction);
  printf(" TRIGGERS.noModifyAction = %d\n", s->TRIGGERS.noModifyAction);
  printf(" IP_MCAST_IDX = %d\n", s->IP_MCAST_IDX);
  printf(" MIRROR0_PROFILE_IDX = %d\n", s->MIRROR0_PROFILE_IDX);
  printf(" MTU_VIOLATION = %s\n", s->MTU_VIOLATION ? "TRUE" : "FALSE");
  printf(" DROP_TTL = %s\n", s->DROP_TTL ? "TRUE" : "FALSE");
  printf(" IS_IPV4 = %s\n", s->IS_IPV4 ? "TRUE" : "FALSE");
  printf(" IS_IPV6 = %s\n", s->IS_IPV6 ? "TRUE" : "FALSE");
  printf(" SA_HIT = %s\n", s->SA_HIT ? "TRUE" : "FALSE");
  printf(" DA_HIT = %s\n", s->DA_HIT ? "TRUE" : "FALSE");
  // ----- struct mbyMaTable
  printf(" SA_RESULT._RSVD5_ = %d\n", s->SA_RESULT._RSVD5_);
  printf(" SA_RESULT.OLD_PORT = %d\n", s->SA_RESULT.OLD_PORT);
  printf(" SA_RESULT.NEW_PORT = %d\n", s->SA_RESULT.NEW_PORT);
  // ----- enum mbyMaLookupEntryType
  switch(s->SA_RESULT.ENTRY_TYPE) {
  case MBY_MA_LOOKUP_ENTRY_TYPE_NOTUSED:
    printf("SA_RESULT.ENTRY_TYPE = MBY_MA_LOOKUP_ENTRY_TYPE_NOTUSED\n");
    break;
  case MBY_MA_LOOKUP_ENTRY_TYPE_PROVISIONAL:
    printf("SA_RESULT.ENTRY_TYPE = MBY_MA_LOOKUP_ENTRY_TYPE_PROVISIONAL\n");
    break;
  case MBY_MA_LOOKUP_ENTRY_TYPE_DYNAMIC:
    printf("SA_RESULT.ENTRY_TYPE = MBY_MA_LOOKUP_ENTRY_TYPE_DYNAMIC\n");
    break;
  case MBY_MA_LOOKUP_ENTRY_TYPE_SECURE:
    printf("SA_RESULT.ENTRY_TYPE = MBY_MA_LOOKUP_ENTRY_TYPE_SECURE\n");
    break;
  case MBY_MA_LOOKUP_ENTRY_TYPE_STATIC:
    printf("SA_RESULT.ENTRY_TYPE = MBY_MA_LOOKUP_ENTRY_TYPE_STATIC\n");
    break;
  case MBY_MA_LOOKUP_ENTRY_TYPE_SECURESTATIC:
    printf("SA_RESULT.ENTRY_TYPE = MBY_MA_LOOKUP_ENTRY_TYPE_SECURESTATIC\n");
    break;
  default:
    printf("SA_RESULT.ENTRY_TYPE = %d (Unknown?)\n", s->SA_RESULT.ENTRY_TYPE);
  }
  printf(" SA_RESULT._RSVD3_ = %d\n", s->SA_RESULT._RSVD3_);
  printf(" SA_RESULT.TRIG_ID = %d\n", s->SA_RESULT.TRIG_ID);
  printf(" SA_RESULT.S_GLORT = %d\n", s->SA_RESULT.S_GLORT);
  printf(" SA_RESULT.D_GLORT = %d\n", s->SA_RESULT.D_GLORT);
  printf(" SA_RESULT._RSVD2_ = %d\n", s->SA_RESULT._RSVD2_);
  printf(" SA_RESULT._RSVD1_ = %s\n", s->SA_RESULT._RSVD1_ ? "TRUE" : "FALSE");
  printf(" SA_RESULT.L2_DOMAIN = %d\n", s->SA_RESULT.L2_DOMAIN);
  printf(" SA_RESULT.VID = %d\n", s->SA_RESULT.VID);
  printf(" SA_RESULT.MAC_ADDRESS = %lld\n", s->SA_RESULT.MAC_ADDRESS);
  // ----- struct mbyMaTable
  printf(" DA_RESULT._RSVD5_ = %d\n", s->DA_RESULT._RSVD5_);
  printf(" DA_RESULT.OLD_PORT = %d\n", s->DA_RESULT.OLD_PORT);
  printf(" DA_RESULT.NEW_PORT = %d\n", s->DA_RESULT.NEW_PORT);
  // ----- enum mbyMaLookupEntryType
  switch(s->DA_RESULT.ENTRY_TYPE) {
  case MBY_MA_LOOKUP_ENTRY_TYPE_NOTUSED:
    printf("DA_RESULT.ENTRY_TYPE = MBY_MA_LOOKUP_ENTRY_TYPE_NOTUSED\n");
    break;
  case MBY_MA_LOOKUP_ENTRY_TYPE_PROVISIONAL:
    printf("DA_RESULT.ENTRY_TYPE = MBY_MA_LOOKUP_ENTRY_TYPE_PROVISIONAL\n");
    break;
  case MBY_MA_LOOKUP_ENTRY_TYPE_DYNAMIC:
    printf("DA_RESULT.ENTRY_TYPE = MBY_MA_LOOKUP_ENTRY_TYPE_DYNAMIC\n");
    break;
  case MBY_MA_LOOKUP_ENTRY_TYPE_SECURE:
    printf("DA_RESULT.ENTRY_TYPE = MBY_MA_LOOKUP_ENTRY_TYPE_SECURE\n");
    break;
  case MBY_MA_LOOKUP_ENTRY_TYPE_STATIC:
    printf("DA_RESULT.ENTRY_TYPE = MBY_MA_LOOKUP_ENTRY_TYPE_STATIC\n");
    break;
  case MBY_MA_LOOKUP_ENTRY_TYPE_SECURESTATIC:
    printf("DA_RESULT.ENTRY_TYPE = MBY_MA_LOOKUP_ENTRY_TYPE_SECURESTATIC\n");
    break;
  default:
    printf("DA_RESULT.ENTRY_TYPE = %d (Unknown?)\n", s->DA_RESULT.ENTRY_TYPE);
  }
  printf(" DA_RESULT._RSVD3_ = %d\n", s->DA_RESULT._RSVD3_);
  printf(" DA_RESULT.TRIG_ID = %d\n", s->DA_RESULT.TRIG_ID);
  printf(" DA_RESULT.S_GLORT = %d\n", s->DA_RESULT.S_GLORT);
  printf(" DA_RESULT.D_GLORT = %d\n", s->DA_RESULT.D_GLORT);
  printf(" DA_RESULT._RSVD2_ = %d\n", s->DA_RESULT._RSVD2_);
  printf(" DA_RESULT._RSVD1_ = %s\n", s->DA_RESULT._RSVD1_ ? "TRUE" : "FALSE");
  printf(" DA_RESULT.L2_DOMAIN = %d\n", s->DA_RESULT.L2_DOMAIN);
  printf(" DA_RESULT.VID = %d\n", s->DA_RESULT.VID);
  printf(" DA_RESULT.MAC_ADDRESS = %lld\n", s->DA_RESULT.MAC_ADDRESS);
  printf(" SV_DROP = %d\n", s->SV_DROP);
  printf(" CSGLORT = %d\n", s->CSGLORT);
  printf(" FLOOD_FORWARDED = %s\n", s->FLOOD_FORWARDED ? "TRUE" : "FALSE");
  printf(" RX_MIRROR = %s\n", s->RX_MIRROR ? "TRUE" : "FALSE");
  // ----- struct mbyClassifierFlags
  printf(" FFU_FLAGS.drop = %s\n", s->FFU_FLAGS.drop ? "TRUE" : "FALSE");
  printf(" FFU_FLAGS.trap = %s\n", s->FFU_FLAGS.trap ? "TRUE" : "FALSE");
  printf(" FFU_FLAGS.log = %s\n", s->FFU_FLAGS.log ? "TRUE" : "FALSE");
  printf(" FFU_FLAGS.no_route = %s\n", s->FFU_FLAGS.no_route ? "TRUE" : "FALSE");
  printf(" FFU_FLAGS.rx_mirror = %s\n", s->FFU_FLAGS.rx_mirror ? "TRUE" : "FALSE");
  printf(" FFU_FLAGS.capture_time = %s\n", s->FFU_FLAGS.capture_time ? "TRUE" : "FALSE");
  printf(" FFU_FLAGS.tx_tag = %d\n", s->FFU_FLAGS.tx_tag);
  printf(" HASH_ROT_A = %d\n", s->HASH_ROT_A);
  printf(" HASH_ROT_B = %d\n", s->HASH_ROT_B);
  printf(" AMASK = %lld\n", s->AMASK);
  printf("---- END struct mbyNextHopToMaskGen\n");
}
void DUMP_mbyMaskGenToTriggers(const mbyMaskGenToTriggers * const s) {
  printf("---- BEGIN struct mbyMaskGenToTriggers\n");
  printf(" LEARNING_ENABLED = %s\n", s->LEARNING_ENABLED ? "TRUE" : "FALSE");
  printf(" AMASK = %lld\n", s->AMASK);
  printf(" DMASK = %d\n", s->DMASK);
  printf(" FNMASK = %d\n", s->FNMASK);
  printf(" LOG_AMASK = %d\n", s->LOG_AMASK);
  printf(" CPU_TRAP = %s\n", s->CPU_TRAP ? "TRUE" : "FALSE");
  printf(" OPERATOR_ID = %d\n", s->OPERATOR_ID);
  printf(" QOS_SWPRI = %d\n", s->QOS_SWPRI);
  printf(" STORE_TRAP_ACTION = %s\n", s->STORE_TRAP_ACTION ? "TRUE" : "FALSE");
  printf(" IDGLORT = %d\n", s->IDGLORT);
  printf(" LOGGING_HIT = %s\n", s->LOGGING_HIT ? "TRUE" : "FALSE");
  printf(" MIRROR0_PORT = %d\n", s->MIRROR0_PORT);
  printf(" MIRROR1_PORT = %d\n", s->MIRROR1_PORT);
  printf(" MIRROR0_PROFILE_V = %d\n", s->MIRROR0_PROFILE_V);
  printf(" MIRROR1_PROFILE_V = %d\n", s->MIRROR1_PROFILE_V);
  printf(" MIRROR0_PROFILE_IDX = %d\n", s->MIRROR0_PROFILE_IDX);
  printf(" MIRROR1_PROFILE_IDX = %d\n", s->MIRROR1_PROFILE_IDX);
  printf(" QCN_MIRROR0_PROFILE_V = %s\n", s->QCN_MIRROR0_PROFILE_V ? "TRUE" : "FALSE");
  printf(" QCN_MIRROR1_PROFILE_V = %s\n", s->QCN_MIRROR1_PROFILE_V ? "TRUE" : "FALSE");
  printf(" ACTION = %d\n", s->ACTION);
  printf(" L2_EDOMAIN = %d\n", s->L2_EDOMAIN);
  printf(" L3_EDOMAIN = %d\n", s->L3_EDOMAIN);
  printf(" MAC_MOVED = %s\n", s->MAC_MOVED ? "TRUE" : "FALSE");
  printf(" FCLASS = %d\n", s->FCLASS);
  printf(" XCAST = %d\n", s->XCAST);
  printf(" RX_MIRROR = %s\n", s->RX_MIRROR ? "TRUE" : "FALSE");
  printf(" CPU_CODE = %d\n", s->CPU_CODE);
  printf(" TARGETED_DETERMINISTIC = %s\n", s->TARGETED_DETERMINISTIC ? "TRUE" : "FALSE");
  printf(" STRICT_GLORT_ROUTING = %s\n", s->STRICT_GLORT_ROUTING ? "TRUE" : "FALSE");
  printf(" GLORT_CAM_MISS = %s\n", s->GLORT_CAM_MISS ? "TRUE" : "FALSE");
  printf(" GLORT_DMASK = %d\n", s->GLORT_DMASK);
  printf(" SKIP_DGLORT_DEC = %s\n", s->SKIP_DGLORT_DEC ? "TRUE" : "FALSE");
  printf(" IP_MCAST_IDX = %d\n", s->IP_MCAST_IDX);
  printf(" DA_HIT = %s\n", s->DA_HIT ? "TRUE" : "FALSE");
  printf("---- END struct mbyMaskGenToTriggers\n");
}
void DUMP_mbyTriggersToCongMgmt(const mbyTriggersToCongMgmt * const s) {
  printf("---- BEGIN struct mbyTriggersToCongMgmt\n");
  printf(" FOO = %s\n", s->FOO ? "TRUE" : "FALSE");
  printf("---- END struct mbyTriggersToCongMgmt\n");
}
void DUMP_mbyCongMgmtToRxStats(const mbyCongMgmtToRxStats * const s) {
  printf("---- BEGIN struct mbyCongMgmtToRxStats\n");
  printf(" RX_LENGTH = %d\n", s->RX_LENGTH);
  printf(" RX_PORT = %d\n", s->RX_PORT);
  printf(" IS_IPV4 = %s\n", s->IS_IPV4 ? "TRUE" : "FALSE");
  printf(" IS_IPV6 = %s\n", s->IS_IPV6 ? "TRUE" : "FALSE");
  printf(" L2_IVLAN1_CNT_INDEX = %d\n", s->L2_IVLAN1_CNT_INDEX);
  printf(" FNMASK = %lld\n", s->FNMASK);
  printf(" SEG_META_ERR = %s\n", s->SEG_META_ERR ? "TRUE" : "FALSE");
  printf(" allowStateChange = %s\n", s->allowStateChange ? "TRUE" : "FALSE");
  printf(" ACTION = %d\n", s->ACTION);
  printf(" TC = %d\n", s->TC);
  printf("---- END struct mbyCongMgmtToRxStats\n");
}
void DUMP_mbyRxStatsToRxOut(const mbyRxStatsToRxOut * const s) {
  printf("---- BEGIN struct mbyRxStatsToRxOut\n");
  printf(" SAF_ERROR = %s\n", s->SAF_ERROR ? "TRUE" : "FALSE");
  printf("---- END struct mbyRxStatsToRxOut\n");
}
void DUMP_mbyTxInToModifier(const mbyTxInToModifier * const s) {
  printf("---- BEGIN struct mbyTxInToModifier\n");
  printf(" RX_LENGTH = %d\n", s->RX_LENGTH);
  printf(" RX_DATA = %p\n", s->RX_DATA);
  printf(" TX_LENGTH = %d\n", s->TX_LENGTH);
  printf(" TX_PORT = %d\n", s->TX_PORT);
  printf(" TX_DROP = %s\n", s->TX_DROP ? "TRUE" : "FALSE");
  printf(" TX_TAG = %d\n", s->TX_TAG);
  printf(" TX_STATS_LAST_LEN = %d\n", s->TX_STATS_LAST_LEN);
  // ----- struct mbyParserInfo
  printf(" PARSER_INFO.otr_l2_len = %d\n", s->PARSER_INFO.otr_l2_len);
  printf(" PARSER_INFO.otr_l2_vlan1 = %s\n", s->PARSER_INFO.otr_l2_vlan1 ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.otr_l2_vlan2 = %s\n", s->PARSER_INFO.otr_l2_vlan2 ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.otr_l2_v2first = %s\n", s->PARSER_INFO.otr_l2_v2first ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.otr_mpls_len = %d\n", s->PARSER_INFO.otr_mpls_len);
  printf(" PARSER_INFO.otr_l3_len = %d\n", s->PARSER_INFO.otr_l3_len);
  printf(" PARSER_INFO.otr_l3_v6 = %s\n", s->PARSER_INFO.otr_l3_v6 ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.otr_l4_udp = %s\n", s->PARSER_INFO.otr_l4_udp ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.otr_l4_tcp = %s\n", s->PARSER_INFO.otr_l4_tcp ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.otr_tun_len = %d\n", s->PARSER_INFO.otr_tun_len);
  printf(" PARSER_INFO.inr_l2_len = %d\n", s->PARSER_INFO.inr_l2_len);
  printf(" PARSER_INFO.inr_l2_vlan1 = %s\n", s->PARSER_INFO.inr_l2_vlan1 ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.inr_l2_vlan2 = %s\n", s->PARSER_INFO.inr_l2_vlan2 ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.inr_l2_v2first = %s\n", s->PARSER_INFO.inr_l2_v2first ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.inr_mpls_len = %d\n", s->PARSER_INFO.inr_mpls_len);
  printf(" PARSER_INFO.inr_l3_len = %d\n", s->PARSER_INFO.inr_l3_len);
  printf(" PARSER_INFO.inr_l3_v6 = %s\n", s->PARSER_INFO.inr_l3_v6 ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.inr_l4_udp = %s\n", s->PARSER_INFO.inr_l4_udp ? "TRUE" : "FALSE");
  printf(" PARSER_INFO.inr_l4_tcp = %s\n", s->PARSER_INFO.inr_l4_tcp ? "TRUE" : "FALSE");
  printf(" NO_MODIFY = %s\n", s->NO_MODIFY ? "TRUE" : "FALSE");
  printf(" L2_EVID1 = %d\n", s->L2_EVID1);
  printf(" EDGLORT = %d\n", s->EDGLORT);
  // ----- enum mbyMirrorType
  switch(s->MIRTYP) {
  case MBY_MIRTYPE_NORMAL:
    printf("MIRTYP = MBY_MIRTYPE_NORMAL\n");
    break;
  case MBY_MIRTYPE_MIR0:
    printf("MIRTYP = MBY_MIRTYPE_MIR0\n");
    break;
  case MBY_MIRTYPE_MIR1:
    printf("MIRTYP = MBY_MIRTYPE_MIR1\n");
    break;
  default:
    printf("MIRTYP = %d (Unknown?)\n", s->MIRTYP);
  }
  printf(" QOS_L3_DSCP = %d\n", s->QOS_L3_DSCP);
  printf(" ECN = %d\n", s->ECN);
  printf(" MARK_ROUTED = %s\n", s->MARK_ROUTED ? "TRUE" : "FALSE");
  printf(" MOD_IDX = %d\n", s->MOD_IDX);
  printf(" TAIL_CSUM_LEN = %lld\n", s->TAIL_CSUM_LEN);
  printf(" XCAST = %d\n", s->XCAST);
  printf(" DROP_TTL = %s\n", s->DROP_TTL ? "TRUE" : "FALSE");
  printf(" IS_TIMEOUT = %s\n", s->IS_TIMEOUT ? "TRUE" : "FALSE");
  printf(" OOM = %s\n", s->OOM ? "TRUE" : "FALSE");
  printf(" PM_ERR = %s\n", s->PM_ERR ? "TRUE" : "FALSE");
  printf(" PM_ERR_NONSOP = %s\n", s->PM_ERR_NONSOP ? "TRUE" : "FALSE");
  printf(" SAF_ERROR = %s\n", s->SAF_ERROR ? "TRUE" : "FALSE");
  printf("---- END struct mbyTxInToModifier\n");
}
void DUMP_mbyModifierToTxStats(const mbyModifierToTxStats * const s) {
  printf("---- BEGIN struct mbyModifierToTxStats\n");
  printf(" TX_DATA = %p\n", s->TX_DATA);
  printf(" TX_LENGTH = %d\n", s->TX_LENGTH);
  printf(" TX_PORT = %d\n", s->TX_PORT);
  printf(" TX_STATS_LENGTH = %d\n", s->TX_STATS_LENGTH);
  printf(" TX_DISP = %d\n", s->TX_DISP);
  printf(" TX_DROP = %s\n", s->TX_DROP ? "TRUE" : "FALSE");
  printf(" TX_REASONCODE = %d\n", s->TX_REASONCODE);
  printf(" SEG_DROP = %s\n", s->SEG_DROP ? "TRUE" : "FALSE");
  printf("---- END struct mbyModifierToTxStats\n");
}
//File end
