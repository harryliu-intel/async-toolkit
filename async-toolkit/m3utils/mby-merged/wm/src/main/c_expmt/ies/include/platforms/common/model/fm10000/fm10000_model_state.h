/* vim:et:sw=4:ts=4:sw=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm10000_model_state.h
 * Creation Date:   August  7, 2012
 * Last Updated:    $IA:Timestamp: January 11, 2014 00:33:25 UTC $
 * Description:     FM10000 white model packet state data structure.
 *
 *                  DO NOT MODIFY THIS FILE - IT IS AUTOMATICALLY GENERATED
 *                  FROM fm10000/fm10000_state.tt.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2012 - 2014 Intel Corporation. All Rights Reserved.
 *
 * The source code contained or described herein and all documents related
 * to the source code ("Material") are owned by Intel Corporation or its
 * suppliers or licensors. Title to the Material remains with Intel
 * Corporation or its suppliers and licensors. The Material contains trade
 * secrets and proprietary and confidential information of Intel or its
 * suppliers and licensors. The Material is protected by worldwide copyright
 * and trade secret laws and treaty provisions. No part of the Material may
 * be used, copied, reproduced, modified, published, uploaded, posted,
 * transmitted, distributed, or disclosed in any way without Intel's prior
 * express written permission.
 *
 * No license under any patent, copyright, trade secret or other intellectual
 * property right is granted to or conferred upon you by disclosure or
 * delivery of the Materials, either expressly, by implication, inducement,
 * estoppel or otherwise. Any license under such intellectual property rights
 * must be express and approved by Intel in writing.
 *****************************************************************************/

#ifndef __FM10000_MODEL_STATE_H
#define __FM10000_MODEL_STATE_H

typedef struct _fm10000_modelState
{
    /* The ingress packet data. */
    fm_byte *                   RX_DATA;

    /* The sideband data. */
    fm_modelSidebandData *      SB_DATA;

    /* The ingress packet data length in units of bytes. */
    fm_uint32                   RX_LENGTH;

    /* The ingress port. */
    fm_int                      RX_PORT;

    /* The 4-bit set of RX EPL flags. Bits [7:3] are reserved and always
     * set to zero. For bits [2:0] see also ''fm10000_modelRxFlags''. */
    fm_byte                     RX_FLAGS;

    /* The Layer 2 destination address. */
    fm_macaddr                  L2_DMAC;

    /* The Layer 2 source address. */
    fm_macaddr                  L2_SMAC;

    /* Boolean indicating whether the packet contains a F56 tag. */
    fm_bool                     F56_TAGGED;

    /* The 2-bit ingress ISL frame type. This channel is used throughout
     * the ingress portion of the FM10000 white model pipeline. See
     * ''ISL_EFTYPE'' for the egress ISL frame type. */
    fm10000_modelFType          ISL_IFTYPE;

    /* The 2-bit ingress ISL Vlan type. Bit 0 reflects whether VID1 is
     * present or not. Likewise, bit 1 reflects the status of VID2. */
    fm_byte                     ISL_IVTYPE;

    /* The 4-bit ISL priority. */
    fm_byte                     QOS_ISL_PRI;

    /* The 8-bit ISL user field. */
    fm_byte                     ISL_USER;

    /* The 16-bit ISL source GLORT. */
    fm_uint16                   ISL_SGLORT;

    /* The 16-bit ingress ISL destination GLORT. This channel is used
     * throughout the ingress portion of the FM10000 white model pipeline.
     * See ''ISL_EDGLORT'' for the egress ISL destination GLORT. */
    fm_uint16                   ISL_IDGLORT;

    /* The 2-bit outer ingress VLAN tag type. */
    fm10000_modelVType          L2_IVTYPE1;

    /* The 12-bit ingress VLAN ID. This channel is initialized by the
     * PARSER and updated by other stages */
    fm_uint16                   L2_IVID1;

    /* The 4-bit outer ingress VLAN priority. This channel is initialized
     * by the PARSER and should be left as-is. See ''QOS_L2_VPRI1'' for the
     * QOS VLAN priority. */
    fm_byte                     L2_IVPRI1;

    /* The 2-bit inner ingress VLAN tag type. */
    fm10000_modelVType          L2_IVTYPE2;

    /* The 12-bit inner ingress VLAN ID. */
    fm_uint16                   L2_IVID2;

    /* The 4-bit inner ingress VLAN priority. */
    fm_byte                     L2_IVPRI2;

    /* The 16-bit innermost Ethernet type. */
    fm_uint16                   L2_ETYPE;

    /* The 4-bit IP version. */
    fm_byte                     L3_VERSION;

    /* The 4-bit IP header length in units of 32-bit words. */
    fm_byte                     L3_HLENGTH;

    /* The 8-bit IPv4 Type of Service or 8-bit IPv6 Traffic Class field. */
    fm_byte                     L3_TOS;

    /* The 20-bit IPv6 flow label. */
    fm_uint32                   L3_FLOW;

    /* The 16-bit IPv4 datagram length (including the IPv4 header and
     * payload) or 16-bit IPv6 payload length (including any extension
     * headers) in units of octets. */
    fm_uint16                   L3_LENGTH;

    /* The 8-bit set of miscellaneous IP flags. Bits [7:3] are reserved and
     * always set to zero. Bits [2:0] contain {HeadFrag, DoNotFrag,
     * HasIPOpts} (MSB first). */
    fm_byte                     L3_MISC;

    /* The 8-bit IP Time to Live field. */
    fm_byte                     L3_TTL;

    /* The 8-bit Layer 3/4 protocol. */
    fm_byte                     L3_PROT;

    /* The 32/128-bit Layer 3 source address. */
    fm_uint32                   L3_SIP[4];

    /* The 32/128-bit Layer 3 destination address. */
    fm_uint32                   L3_DIP[4];

    /* The 16-bit Layer 4 source port. */
    fm_uint16                   L4_SRC;

    /* The 16-bit Layer 4 destination port. */
    fm_uint16                   L4_DST;

    /* The 6-bit port number. */
    fm_byte                     SRC;

    /* 16-bit Layer 4 deep inspection half-word A. */
    fm_uint16                   L4A;

    /* 16-bit Layer 4 deep inspection half-word B. */
    fm_uint16                   L4B;

    /* 16-bit Layer 4 deep inspection half-word C. */
    fm_uint16                   L4C;

    /* 16-bit Layer 4 deep inspection half-word D. */
    fm_uint16                   L4D;

    /* 8-bit tag passed to FFU to reflect the presence of various tags in
     * the packets. */
    fm_byte                     RX_TAG;

    /* 8-bit Pause Class vector from Pause Frame. */
    fm_byte                     CBP_VEC;

    /* Pause timers from Pause Frame. */
    fm_uint16                   TIMER[8];

    /* Boolean indicating whether the packet should be dropped because it
     * is an in-band management packet. */
    fm_bool                     DROP_MGMT;

    /* Boolean indicating whether this packet should be dropped becuase TTL
     * is either 0 or 1. */
    fm_bool                     DROP_TTL;

    /* Boolean indicating whether this ICMP packet should be trapped. */
    fm_bool                     TRAP_ICMP;

    /* Boolean indicating whether this IGMP packet should be trapped. */
    fm_bool                     TRAP_IGMP;

    /* 2-bit value indicating the type of Pause frame: 0 = non pause / 1 =
     * normal / 2 = class based. */
    fm_byte                     PAUSE_TYPE;

    /* Boolean indicating whether a header parse error has occurred. */
    fm_bool                     PARSE_ERROR;

    /* Boolean indicating whether a parity error has been detected in any
     * of the memories while processing this packet. */
    fm_bool                     PARITY_ERROR;

    /* Boolean indicating whether the packet should be dropped because it
     * has a VLAN tag and the outer ingress VLAN ID is non-zero. */
    fm_bool                     DROP_TAGGED;

    /* Boolean indicating whether the packet should be dropped because it
     * does not have a VLAN tag or the outer ingress VLAN ID is zero. */
    fm_bool                     DROP_UNTAGGED;

    /* The 4-bit QOS VLAN priority. */
    fm_byte                     QOS_L2_VPRI1;

    /* The 4-bit QOS VLAN priority. */
    fm_byte                     QOS_L2_VPRI2;

    /* The 6-bit QOS Differentiated Services Code Point. */
    fm_byte                     QOS_L3_DSCP;

    /* The 5-bit FFU scenario. */
    fm_byte                     FFU_SCENARIO;

    fm10000_modelFfuKey         FFU_KEY;

    fm_uint64                   FFU_EGRESS_ACTIONS;

    /* The 6-bit set of FFU flags. Bits [5:0] contain {CAPTURE-TIME,
     * RX_MIRROR, NO_ROUTE, LOG, TRAP, DROP}. */
    fm_byte                     FFU_FLAGS;

    fm_byte                     FFU_TRIG;

    fm_uint32                   FFU_COUNT[4];

    fm_uint32                   FFU_ROUTE;

    fm_byte                     TX_TAG;

    /* The Layer 2, 3 & 4 and Layer 3 & 4 hash keys and the associated
     * configuration information. */
    fm10000_modelHashKeys       HASH_KEYS;

    /* The 16-entry next hop hash array. Each entry is 4 bits wide. */
    fm_byte                     ARP_HASH[16];

    /* The raw hash value. */
    fm_uint16                   RAW_HASH;

    /* The rotation A hash value. */
    fm_uint32                   HASH_ROT_A;

    /* The rotation B hash value. */
    fm_uint32                   HASH_ROT_B;

    /* The 12-bit outer egress VLAN ID. */
    fm_uint16                   L2_EVID1;

    /* The 6-bit logging action mask. */
    fm_byte                     LOG_AMASK;

    fm_bool                     MARK_ROUTED;

    /* The 8-bit virtual router ID. */
    fm_byte                     L3_VRID;

    fm_uint16                   ARP_TABLE_INDEX;

    fm_uint32                   ARP_MTU_INDEX;

    /* Boolean indicating whether the MTU index stored in the next hop
     * table or the MTU index stored in the egress VLAN table should be
     * used. */
    fm_bool                     USE_ARP_MTU_INDEX;

    /* The 12-bit ingress forwarind ID. */
    fm_uint16                   L2_IFID1;

    fm_byte                     L2_IMST_INDEX;

    /* Boolean indicating whether ingress VLAN reflection is enabled. */
    fm_bool                     L2_IVLAN1_REFLECT;

    fm_byte                     L2_IVLAN1_CNT_INDEX;

    /* Boolean indicating whether the ingress port is part of the ingress
     * VLAN. */
    fm_bool                     L2_IVLAN1_MEMBERSHIP;

    /* The 12-bit egress forwarding ID. */
    fm_uint16                   L2_EFID1;

    fm_byte                     L2_EMST_INDEX;

    /* The 48-bit egress VLAN port membership vector. */
    fm_uint64                   L2_EVLAN1_MEMBERSHIP;

    fm_byte                     L2_EVLAN1_TRIG;

    /* Boolean indicating whether this packet violates the MTU. */
    fm_bool                     MTU_VIOLATION;

    /* The 11-bit hash bin index of the destination MAC address lookup
     * result. */
    fm_uint32                   DA_INDEX;

    /* The 2-bit hash set of the destination MAC address lookup result. */
    fm_byte                     DA_SET;

    /* Boolean indicating whether the destination MAC address lookup
     * resulted in a hit or miss. */
    fm_bool                     DA_HIT;

    /* The destination MAC address lookup result. */
    fm10000_modelMaTable *      DA_RESULT;

    /* The 11-bit hash bin index of the source MAC address lookup result.
     * */
    fm_uint32                   SA_INDEX;

    /* The 2-bit hash set of the source MAC address lookup result. */
    fm_byte                     SA_SET;

    /* Boolean indicating whether the source MAC address lookup resulted in
     * a hit or miss. */
    fm_bool                     SA_HIT;

    /* The source MAC address lookup result. */
    fm10000_modelMaTable *      SA_RESULT;

    /* The 2-bit spanning tree state for the ingress port. */
    fm10000_modelSTPState       L2_IFID1_STATE;

    /* The 48-bit egress forwarding vector. */
    fm_uint64                   L2_EFID1_STATE;

    /* Boolean indicating whether glort is forwarded due to FFU rule. */
    fm_bool                     GLORT_FORWARDED;

    /* Boolean indicating whether glort is flood forwarded rule. */
    fm_bool                     FLOOD_FORWARDED;

    /* Boolean indicating whether the GLORT lookup resulted in a miss or
     * hit. */
    fm_bool                     GLORT_CAM_MISS;

    /* The 46-bit action mask. */
    fm_uint64                   AMASK;

    /* Boolean indicating that the packet is strict GLORT routed. */
    fm_bool                     STRICT_GLORT_ROUTING;

    /* The 48-bit GLORT based destination mask. */
    fm_uint64                   GLORT_DMASK;

    fm_uint16                   IP_MCAST_IDX;

    fm_byte                     PORT_FIELD_SIZE;

    /* The 16-bit canonical source GLORT. */
    fm_uint16                   CSGLORT;

    fm_byte                     TCN_FIFO_MASK;

    /* The 48-bit destination mask. */
    fm_uint64                   DMASK;

    fm_bool                     RX_MIRROR;

    fm_bool                     MIRROR1_PROFILE_V;

    fm_int                      MIRROR1_PROFILE_IDX;

    /* The 48-bit ACL based destination mask. */
    fm_uint64                   DST_MASK_ACL;

    /* The 48-bit FFU egress drop count mask. */
    fm_uint64                   EGRESS_DROP_COUNT_MASK;

    fm_uint32                   PRE_RESOLVE_ACTION;

    fm_uint64                   PRE_RESOLVE_DMASK;

    fm_uint16                   PRE_RESOLVE_DGLORT;

    /* The resolved action. */
    fm_uint32                   ACTION;

    /* The 8-bit CPU code which forms the lower 8 bits of the CPU bound
     * destination glort. */
    fm_byte                     CPU_CODE;

    /* Boolean indicating whether CPU bound packets should be truncated. */
    fm_bool                     TRUNC_CPU;

    /* Boolean indicating whether mirrored packets should be truncated. */
    fm_bool                     TRUNC_MIRROR;

    /* The 48-bit normal forwarding mask. */
    fm_uint64                   FNMASK;

    fm_bool                     BCPU;

    fm_bool                     TX_MIRROR;

    /* The 48-bit external port destination mask. */
    fm_uint64                   MTMASK;

    /* Boolean indicating that a non-secure MAC was found when source
     * lookup was performed */
    fm_bool                     MAC_MOVED;

    /* Boolean indicating learning enabled status within action codes. */
    fm_bool                     LEARNING_ENABLED;

    /* Boolean that defines current epoch for multicast garbage collection
     * */
    fm_bool                     MCAST_EPOCH;

    /* Class state (Unicast, Broadcast or Multicast) detected in GEN_MASK
     * and available for apply. */
    fm_byte                     FCLASS;

    fm_bool                     LOGGING_HIT;

    /* The mirror0 port. */
    fm_int                      MIRROR0_PORT;

    /* The mirror1 port. */
    fm_int                      MIRROR1_PORT;

    /* Boolean indicates if frame should be sent to CPU */
    fm_bool                     CPU_TRAP;

    /* Boolean indicates if frame should be dropped by policer */
    fm_bool                     POLICER_DROP;

    fm_uint64                   TRIG_HIT_MASK_RESOLVED;

    fm_byte                     MIRROR0_PROFILE_V;

    fm_int                      MIRROR0_PROFILE_IDX;

    fm10000_modelTriggerResults TRIGGERS;

    /* The 3-bit traffic class. */
    fm_byte                     TC;

    /* The 2-bit SMP membership. */
    fm_byte                     SMP_MEMBERSHIP;

    /* The 8-bit vector of TCs to pause */
    fm_byte                     PAUSE_TC_XOFF;

    /* SAF'ed frame has error.. */
    fm_bool                     SAF_ERROR;

    /* Boolean indicating whether the FM10000 white model FSCHED stage has
     * been initialized for this ingress packet. */
    fm_bool                     FSCHED_INIT;

    /* Used by FSCHED state to remember which ports have already been
     * serviced. */
    fm_byte                     FSCHED_PORT;

    /* The 48-bit Layer 2 multicast destination mask. This channel is to be
     * used by the FSCHED stage only. */
    fm_uint64                   L2MASK_FN;

    /* The 48-bit Layer 2 TX mirroring destination mask. This channel is to
     * be used by the FSCHED stage only. */
    fm_uint64                   L2MASK_TX;

    /* Used by the FSCHED stage only to identify which vlan copy is being
     * sent */
    fm_uint16                   mcast_offset;

    /* Used by the FSCHED stage only to identify which vlan copy is being
     * sent */
    fm_uint16                   l3cnt;

    /* Used by the FSCHED stage to determine if mirror0 copy has been sent
     * */
    fm_byte                     mir0_done;

    /* Used by the FSCHED stage to determine if mirror1 copy has been sent
     * */
    fm_byte                     mir1_done;

// deprecated
//    /* The 48-bit Layer 3 multicast destination mask. This channel is to be
//     * used by the FSCHED stage only. */
//    fm_uint64                   L3MASK_FN;

// deprecated
//    /* The 48-bit Layer 3 TX mirroring destination mask. This channel is to
//     * be used by the FSCHED stage only. */
//    fm_uint64                   L3MASK_TX;

// deprecated
//    /* The 48-bit logging destination mask. This channel is to be used by
//     * the FSCHED stage only. */
//    fm_uint64                   MASK_CPU;
//
// deprecated
//    /* The 14-bit MTABLE pointer. This channel is to be used by the FSCHED
//     * stage only. */
//    fm_uint16                   MTPTR_FN;

    /* The 14-bit TX mirroring MTABLE pointer. This channel is to be used
     * by the FSCHED stage only. */
    fm_uint16                   MTPTR_TX;

    /* The 2-bit egress ISL frame type. This channel is used throughout the
     * egress portion of the FM10000 white model pipeline. See
     * ''ISL_IFTYPE'' for the ingress ISL frame type. */
    fm10000_modelFType          ISL_EFTYPE;

    /* The 16-bit egress destination GLORT. This channel is used throughout
     * the egress portion of the FM10000 white model pipeline. See
     * ''ISL_IDGLORT'' for the ingress ISL destination GLORT. */
    fm_uint16                   ISL_EDGLORT;

    fm_bool                     IS_PAUSE;

    fm10000_modelMirTyp         MIRTYP;

    /* The egress port. */
    fm_int                      TX_PORT;

    fm_byte                     TX_FLAGS;

    /* The 1-bit timeout indicator from ModCtrl.pf */
    fm_byte                     IS_TIMEOUT;

    /* The egress packet data length in units of bytes. */
    fm_uint32                   TX_LENGTH;

    /* The 4-bit egress frame disposition */
    fm_byte                     TX_DISP;

    /* 1-bit egress drop indicator */
    fm_byte                     TX_DROP;

    /* For DV-use only: data from modify stage to modify unit level
     * testbench. */
    fm_byte                     MODIFY_DV_DATA[64];

} fm10000_modelState;


#endif  /* __FM10000_MODEL_STATE_H */

