/* vim:et:sw=4:ts=4:sw=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm4000_model_state.h
 * Creation Date:   August  7, 2012
 * Last Updated:    July 15, 2013
 * Description:     FM4000 white model packet state data structure.
 *
 *                  DO NOT MODIFY THIS FILE - IT IS AUTOMATICALLY GENERATED
 *                  FROM fm4000/fm4000_state.tt.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2012 - 2013 Intel Corporation. All Rights Reserved.
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

#ifndef __FM4000_MODEL_STATE_H
#define __FM4000_MODEL_STATE_H

typedef struct _fm4000_modelState
{
    /* The ingress packet data. */
    fm_byte *                  RX_DATA;

    /* The sideband data. */
    fm_modelSidebandData *     SB_DATA;

    /* The ingress packet data length in units of bytes. */
    fm_uint32                  RX_LENGTH;

    /* The ingress port. */
    fm_int                     RX_PORT;

    /* The 4-bit set of RX EPL flags. Bits [7:3] are reserved and always
     * set to zero. For bits [2:0] see also ''fm4000_modelRxFlags''. */
    fm_byte                    RX_FLAGS;

    /* The Layer 2 destination address. */
    fm_macaddr                 L2_DMAC;

    /* The Layer 2 source address. */
    fm_macaddr                 L2_SMAC;

    /* Boolean indicating whether the packet contains a F64 tag. */
    fm_bool                    F64_TAGGED;

    /* Boolean indicating whether the packet contains a F96 tag. */
    fm_bool                    F96_TAGGED;

    /* Boolean indicating whether the packet contains a X32 tag. */
    fm_bool                    X32_TAGGED;

    /* Boolean indicating whether the packet contains a X64 tag. */
    fm_bool                    X64_TAGGED;

    /* Boolean indicating whether the packet contains a X96 tag. */
    fm_bool                    X96_TAGGED;

    /* The 2-bit ingress ISL frame type. This channel is used throughout
     * the ingress portion of the FM4000 white model pipeline. See
     * ''ISL_EFTYPE'' for the egress ISL frame type. */
    fm4000_modelFType          ISL_IFTYPE;

    /* The 2-bit ISL management type. */
    fm4000_modelMType          ISL_MTYPE;

    /* The 4-bit ISL priority. */
    fm_byte                    QOS_ISL_PRI;

    /* The 8-bit ISL user field. */
    fm_byte                    ISL_USER;

    /* The 16-bit ISL source GLORT. */
    fm_uint16                  ISL_SGLORT;

    /* The 16-bit ingress ISL destination GLORT. This channel is used
     * throughout the ingress portion of the FM4000 white model pipeline.
     * See ''ISL_EDGLORT'' for the egress ISL destination GLORT. */
    fm_uint16                  ISL_IDGLORT;

    /* The 2-bit outer ingress VLAN tag type. */
    fm4000_modelVType          L2_IVTYPE1;

    /* The 12-bit outer ingress VLAN ID. This channel is initialized by the
     * PARSER and should be left as-is. See ''L2_VID1'' for the internal
     * ingress VLAN ID. */
    fm_uint16                  L2_IVID1;

    /* The 4-bit outer ingress VLAN priority. This channel is initialized
     * by the PARSER and should be left as-is. See ''QOS_L2_VPRI1'' for the
     * QOS VLAN priority. */
    fm_byte                    L2_IVPRI1;

    /* The 2-bit inner ingress VLAN tag type. */
    fm4000_modelVType          L2_IVTYPE2;

    /* The 12-bit inner ingress VLAN ID. */
    fm_uint16                  L2_IVID2;

    /* The 4-bit inner ingress VLAN priority. */
    fm_byte                    L2_IVPRI2;

    /* Boolean indicating whether the packet contains a RLT tag. */
    fm_bool                    RLT_TAGGED;

    /* The 16-bit RLT tag. */
    fm_uint64                  RLT;

    /* Boolean indicating whether the packet contains a F32 tag. */
    fm_bool                    F32_TAGGED;

    /* Boolean indicating whether the packet should contain a F32 tag, but
     * does not. */
    fm_bool                    F32_UNTAGGED;

    /* The 16-bit innermost Ethernet type. */
    fm_uint16                  L2_ETYPE;

    /* The 4-bit IP version. */
    fm_byte                    L3_VERSION;

    /* The 4-bit IP header length in units of 32-bit words. */
    fm_byte                    L3_HLENGTH;

    /* The 8-bit IPv4 Type of Service or 8-bit IPv6 Traffic Class field. */
    fm_byte                    L3_TOS;

    /* The 20-bit IPv6 flow label. */
    fm_uint32                  L3_FLOW;

    /* The 16-bit IPv4 datagram length (including the IPv4 header and
     * payload) or 16-bit IPv6 payload length (including any extension
     * headers) in units of octets. */
    fm_uint16                  L3_LENGTH;

    /* The 8-bit set of miscellaneous IP flags. Bits [7:3] are reserved and
     * always set to zero. Bits [2:0] contain {HeadFrag, DoNotFrag,
     * HasIPOpts} (MSB first). */
    fm_byte                    L3_MISC;

    /* The 8-bit IP Time to Live field. */
    fm_byte                    L3_TTL;

    /* The 8-bit Layer 3/4 protocol. */
    fm_byte                    L3_PROT;

    /* The 32/128-bit Layer 3 source address. */
    fm_uint32                  L3_SIP[4];

    /* The 32/128-bit Layer 3 destination address. */
    fm_uint32                  L3_DIP[4];

    /* The 16-bit Layer 4 source port. */
    fm_uint16                  L4_SRC;

    /* The 16-bit Layer 4 destination port. */
    fm_uint16                  L4_DST;

    /* 16-bit Layer 4 deep inspection half-word A. */
    fm_uint16                  L4A;

    /* 16-bit Layer 4 deep inspection half-word B. */
    fm_uint16                  L4B;

    /* 16-bit Layer 4 deep inspection half-word C. */
    fm_uint16                  L4C;

    /* 16-bit Layer 4 deep inspection half-word D. */
    fm_uint16                  L4D;

    /* Boolean indicating whether a header parse error has occurred. */
    fm_bool                    PARSE_ERROR;

    /* Boolean indicating whether a parity error has been detected in any
     * of the memories while processing this packet. */
    fm_bool                    PARITY_ERROR;

    /* Boolean indicating whether the packet should be dropped because it
     * has a VLAN tag and the outer ingress VLAN ID is non-zero. */
    fm_bool                    DROP_TAGGED;

    /* Boolean indicating whether the packet should be dropped because it
     * does not have a VLAN tag or the outer ingress VLAN ID is zero. */
    fm_bool                    DROP_UNTAGGED;

    /* Boolean indicating whether the packet should be droppd because it is
     * an in-band management packet. */
    fm_bool                    DROP_MGMT;

    /* Boolean indicating whether the packet is a special delivery frame.
     * */
    fm_bool                    SPECIAL_DELIVERY;

    /* The 12-bit internal ingress VLAN ID. */
    fm_uint16                  L2_VID1;

    /* The 4-bit QOS VLAN priority. */
    fm_byte                    QOS_L2_VPRI1;

    /* The 6-bit QOS Differentiated Services Code Point. */
    fm_byte                    QOS_L3_DSCP;

    fm_bool                    DROP_TTL;

    fm_bool                    TRAP_ICMP;

    /* Boolean indicating whether this IGMP packet should be trapped. */
    fm_bool                    TRAP_IGMP;

    /* The Layer 2, 3 & 4 and Layer 3 & 4 hash keys and the associated
     * configuration information. */
    fm4000_modelHashKeys       HASH_KEYS;

    /* The 5-bit FFU scenario. */
    fm_byte                    FFU_SCENARIO;

    fm4000_modelFfuKey         FFU_KEY;

    fm_uint32                  FFU_EGRESS_ACTIONS[FM4000_MODEL_FFU_EGRESS_ACTIONS_COUNT];

    /* The 5-bit set of FFU flags. Bits [4:0] contain {RX_MIRROR, NO_ROUTE,
     * LOG, TRAP, DROP}. */
    fm_byte                    FFU_FLAGS;

    fm_byte                    FFU_TRIG;

    fm_uint32                  FFU_COUNT[4];

    fm_uint32                  FFU_ACTION_A;

    fm_uint32                  FFU_ACTION_B;

    fm_uint32                  FFU_ROUTE;

    /* The 16-entry next hop hash array. Each entry is 4 bits wide. */
    fm_byte                    ARP_HASH[16];

    /* The rotation A hash value. */
    fm_uint32                  HASH_ROT_A;

    /* The rotation B hash value. */
    fm_uint32                  HASH_ROT_B;

    /* The 12-bit outer egress VLAN ID. */
    fm_uint16                  L2_EVID1;

    /* The 6-bit logging action mask. */
    fm_byte                    LOG_AMASK;

    fm_bool                    MARK_ROUTED;

    /* The 8-bit virtual router ID. */
    fm_byte                    L3_VRID;

    fm_uint32                  ARP_MTU_INDEX;

    /* Boolean indicating whether the MTU index stored in the next hop
     * table or the MTU index stored in the egress VLAN table should be
     * used. */
    fm_bool                    USE_ARP_MTU_INDEX;

    /* The 12-bit ingress forwarind ID. */
    fm_uint16                  L2_IFID1;

    /* Boolean indicating whether ingress VLAN reflection is enabled. */
    fm_bool                    L2_IVLAN1_REFLECT;

    fm_byte                    L2_IVLAN1_CNT_INDEX;

    /* Boolean indicating whether the ingress port is part of the ingress
     * VLAN. */
    fm_bool                    L2_IVLAN1_MEMBERSHIP;

    /* The 12-bit egress forwarding ID. */
    fm_uint16                  L2_EFID1;

    /* The 25-bit egress VLAN port membership vector. */
    fm_uint32                  L2_EVLAN1_MEMBERSHIP;

    fm_byte                    L2_EVLAN1_TRIG;

    /* Boolean indicating whether this packet violates the MTU. */
    fm_bool                    MTU_VIOLATION;

    /* The command executed by the L2_LOOKUP stage. */
    fm4000_modelFhCmd          FH_CMD;

    /* Boolean indicating whether the destination MAC address lookup
     * resulted in a hit or miss. */
    fm_bool                    DA_HIT;

    /* The destination MAC address lookup result. */
    fm4000_modelMaTable *      DA_RESULT;

    /* The 11-bit hash bin index of the source MAC address lookup result.
     * */
    fm_uint32                  SA_INDEX;

    /* The 3-bit hash set of the source MAC address lookup result. */
    fm_uint32                  SA_SET;

    /* Boolean indicating whether the source MAC address lookup resulted in
     * a hit or miss. */
    fm_bool                    SA_HIT;

    /* The source MAC address lookup result. */
    fm4000_modelMaTable *      SA_RESULT;

    /* The 2-bit spanning tree state for the ingress port. */
    fm4000_modelSTPState       L2_IFID1_STATE;

    /* The 25-bit egress forwarding vector. */
    fm_uint32                  L2_EFID1_STATE;

    /* Boolean indicating whether the GLORT lookup resulted in a miss or
     * hit. */
    fm_bool                    GLORT_CAM_MISS;

    /* The 46-bit action mask. */
    fm_uint64                  AMASK;

    /* Boolean indicating that the packet is strict GLORT routed. */
    fm_bool                    STRICT_GLORT_ROUTING;

    /* The 25-bit GLORT based destination mask. */
    fm_uint32                  GLORT_DMASK;

    fm_uint16                  IP_MCAST_IDX;

    /* The 16-bit canonical source GLORT. */
    fm_uint16                  CSGLORT;

    fm_byte                    TCN_FIFO_MASK;

    /* The 25-bit destination mask. */
    fm_uint32                  DMASK;

    fm_bool                    RX_MIRROR;

    fm_uint16                  RX_MIRROR_GLORT;

    fm_int                     RX_MIRROR_PORT;

    /* The 25-bit FFU egress action count mask. */
    fm_uint32                  EGRESS_COUNT_MASK;

    fm4000_modelActionType     PRE_RESOLVE_ACTION;

    fm_uint32                  PRE_RESOLVE_DMASK;

    fm_uint16                  PRE_RESOLVE_DGLORT;

    /* The resolved action. */
    fm4000_modelActionType     ACTION;

    /* The 8-bit CPU code which forms the lower 8 bits of the CPU bound
     * destination glort. */
    fm_byte                    CPU_CODE;

    /* Boolean indicating whether CPU bound packets should be truncated. */
    fm_bool                    TRUNC_CPU;

    /* Boolean indicating whether mirrored packets should be truncated. */
    fm_bool                    TRUNC_MIRROR;

    /* The 25-bit normal forwarding mask. */
    fm_uint32                  FNMASK;

    fm_bool                    BCPU;

    fm_bool                    TX_MIRROR;

    /* The 25-bit external port destination mask. */
    fm_uint32                  MTMASK;

    fm_uint64                  TRIG_HIT_MASK_RESOLVED;

    fm4000_modelTriggerResults TRIGGERS;

    /* The 3-bit traffic class. */
    fm_byte                    TC;

    /* The 2-bit SMP membership. */
    fm_byte                    SMP_MEMBERSHIP;

    /* Boolean indicating whether the FM4000 white model FSCHED stage has
     * been initialized for this ingress packet. */
    fm_bool                    FSCHED_INIT;

    /* The 25-bit Layer 2 multicast destination mask. This channel is to be
     * used by the FSCHED stage only. */
    fm_uint32                  L2MASK_FN;

    /* The 25-bit Layer 2 TX mirroring destination mask. This channel is to
     * be used by the FSCHED stage only. */
    fm_uint32                  L2MASK_TX;

    /* The 25-bit Layer 3 multicast destination mask. This channel is to be
     * used by the FSCHED stage only. */
    fm_uint32                  L3MASK_FN;

    /* The 25-bit Layer 3 TX mirroring destination mask. This channel is to
     * be used by the FSCHED stage only. */
    fm_uint32                  L3MASK_TX;

    /* The 25-bit logging destination mask. This channel is to be used by
     * the FSCHED stage only. */
    fm_uint32                  MASK_CPU;

    /* The 14-bit MTABLE pointer. This channel is to be used by the FSCHED
     * stage only. */
    fm_uint16                  MTPTR_FN;

    /* The 14-bit TX mirroring MTABLE pointer. This channel is to be used
     * by the FSCHED stage only. */
    fm_uint16                  MTPTR_TX;

    /* The 2-bit egress ISL frame type. This channel is used throughout the
     * egress portion of the FM4000 white model pipeline. See
     * ''ISL_IFTYPE'' for the ingress ISL frame type. */
    fm4000_modelFType          ISL_EFTYPE;

    /* The 16-bit egress destination GLORT. This channel is used throughout
     * the egress portion of the FM4000 white model pipeline. See
     * ''ISL_IDGLORT'' for the ingress ISL destination GLORT. */
    fm_uint16                  ISL_EDGLORT;

    fm4000_modelMirTyp         MIRTYP;

    /* The egress port. */
    fm_int                     TX_PORT;

    fm_byte                    TX_FLAGS;

    /* The egress packet data length in units of bytes. */
    fm_uint32                  TX_LENGTH;

} fm4000_modelState;


#endif  /* __FM4000_MODEL_STATE_H */

