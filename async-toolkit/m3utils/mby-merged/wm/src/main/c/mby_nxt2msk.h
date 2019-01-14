// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_NXT2MSK_H
#define MBY_NXT2MSK_H

#include "fm_types.h"
#include "mby_cgrp_types.h"
#include "mby_ma_table.h"
#include "mby_trig_results.h"
#include "mby_mirror_type.h"
#include "mby_parser_info.h"
#include "mby_par_hdr_ptrs.h"

typedef struct mbyNextHopToMaskGenStruct
{
    fm_uint16          CSGLORT;                          ///< 16-bit canonical source GLORT
    fm_bool            DA_HIT;                           ///< destination MAC address lookup hit
    fm_bool            DROP_TTL;                         ///< packet should be dropped
    mbyClassifierFlags CGRP_FLAGS;                       ///< flags {CAPTURE-TIME, RX_MIRROR, NO_ROUTE, LOG, TRAP, DROP}
    fm_bool            FLOOD_FORWARDED;                  ///< glort is flood-forwarded
    fm_uint64          GLORT_DMASK[MBY_DMASK_REGISTERS]; ///< 258-bit GLORT-based destination mask
    fm_bool            GLORT_FORWARDED;                  ///< glort forwarded due to FFU rule
    fm_uint32          HASH_ROT_A;                       ///< rotation A hash value
    fm_uint32          HASH_ROT_B;                       ///< rotation B hash value
    fm_uint16          IDGLORT;                          ///< 16-bit ingress destination GLORT
    fm_bool            IS_IPV4;                          ///< packet is IPv4
    fm_bool            IS_IPV6;                          ///< packet is IPv6
    fm_macaddr         L2_DMAC;                          ///< layer 2 destination MAC address
    fm_byte            L2_EDOMAIN;                       ///< egress L2 domain
    fm_uint16          L2_ETYPE;                         ///< 16-bit innermost Ethernet type
    fm_uint16          L2_EVID1;                         ///< 12-bit egress VLAN ID
    fm_uint16          L2_IVID1;                         ///< 12-bit ingress VLAN ID
    fm_bool            L2_IVLAN1_MEMBERSHIP;             ///< ingress port is part of the ingress VLAN flag
    fm_bool            L2_IVLAN1_REFLECT;                ///< ingress VLAN reflection is enabled
    fm_macaddr         L2_SMAC;                          ///< layer 2 source MAC address
    fm_byte            L3_EDOMAIN;                       ///< egress L3 domain
    fm_bool            LEARN_NOTIFY;                     ///< learning is diabled flag
    fm_bool            MARK_ROUTED;                      ///<
    fm_bool            MTU_VIOLATION;                    ///< packet violates the MTU
    fm_byte            OPERATOR_ID;                      ///< 4-bit operator ID
    fm_bool            PARITY_ERROR;                     ///< memory parity error flag
    fm_bool            PARSER_WINDOW_V;                  ///< parser window valid flag
    fm_bool            PARSER_ERROR;                     ///< header parse error flag
    fm_bool            PA_DROP;                          ///< checksum validation error, drop pkt in tail proc
    fm_byte            QOS_TC;                           ///< 4-bit switch priority
    fm_bool            RX_MIRROR;                        ///< rx mirror frame
    fm_uint32          RX_PORT;                          ///< receive port number
    fm_bool            SA_HIT;                           ///< source MAC address lookup hit
    mbyMaTable         SA_RESULT;                        ///< source MAC address lookup result
    fm_byte            SEG_META_ERR;                     ///< segment error
    fm_byte            SV_DROP;                          ///< MAC security violation info
    fm_bool            TRAP_ICMP;                        ///< ICMP packet should be trapped
    fm_bool            TRAP_IGMP;                        ///< IGMP packet should be trapped
    fm_bool            TRAP_IP_OPTIONS;                  ///< IP options present
    mbyTriggerResults  TRIGGERS;                         ///< trigger results

    // pass-thru:
    fm_uint32          ACTION;                           ///< resolved action
    fm_uint16          ARP_TABLE_INDEX;
    fm_byte            CGRP_TRIG;                        ///< classifier action triggers
    fm_uint32          CONTENT_ADDR;                     ///< MOD Content address, expressed in 32B units
    fm_bool            CPU_TRAP;                         ///< CPU trap
    fm_bool            DECAP;
    fm_byte            ECN;                              ///< ECN value to use in egress packet
    fm_uint16          EDGLORT;                          ///< egress destination glort
    fm_bool            ENCAP;
    fm_bool            FLOOD_SET;
    fm_uint16          IP_MCAST_IDX;                     ///< index into the MCAST_VLAN_TABLE
    fm_bool            IS_TIMEOUT;
    fm_uint16          L2_IDOMAIN;                       ///<
    fm_uint16          L2_IVLAN1_CNT;                    ///< ingress VLAN counter
    fm_byte            L3_IDOMAIN;
    fm_uint32          MIRROR0_PROFILE_IDX;              ///< mirror 0 profile index
    mbyMirrorType      MIRTYP;                           ///< mirror type
    fm_uint32          MOD_IDX;                          ///< index into the MODIFY descriptor tables
    fm_byte            MOD_PROF_IDX;                     ///< modify profile index
    fm_byte            MTU_INDEX;
    fm_bool            PA_L3LEN_ERR;           ///> l3 length error
    fm_bool            OOM;                  ///< out of memory
    mbyParserInfo      PARSER_INFO;          ///< parser info structure
    mbyParserHdrPtrs   PA_HDR_PTRS;          ///< parser header pointers
    fm_bool            PM_ERR;               ///< ECC error on PM
    fm_bool            PM_ERR_NONSOP;        ///<
    fm_uint32          PRE_RESOLVE_DMASK;    ///< destination mask before action resolution
    fm_byte            QOS_L3_DSCP;          ///< 6-bit QOS Differentiated Services Code Point (DSCP):
    fm_uint64          TAIL_CSUM_LEN;        ///< L4 CSUM related information
    fm_byte            TRAFFIC_CLASS;        ///< traffic class
    fm_byte            TX_TAG;               ///< transmit tag from Classifier
    fm_uint32          RX_LENGTH;            ///< Ingress packet data length [bytes]

} mbyNextHopToMaskGen;

#endif // MBY_NXT2MSK_H
