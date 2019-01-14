// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_MSK2TRG_H
#define MBY_MSK2TRG_H

#include "fm_types.h"
#include "mby_dmask_regs.h"
#include "mby_mirror_type.h"
#include "mby_parser_info.h"
#include "mby_par_hdr_ptrs.h"

typedef struct mbyMaskGenToTriggersStruct
{
    fm_uint32         ACTION;                                 ///< resolved action
    fm_uint64         AMASK;                                  ///< 46-bit action mask
    fm_byte           CPU_CODE;                               ///< 4-bit CPU code
    fm_bool           CPU_TRAP;                               ///< flag indicating frame should be sent to CPU
    fm_uint16         CSGLORT;                                ///< 16-bit canonical source GLORT
    fm_bool           DA_HIT;                                 ///< destination MAC address lookup hit
    fm_uint64         DMASK[MBY_DMASK_REGISTERS];             ///< 258-bit destination mask
    fm_bool           DROP_TTL;
    fm_byte           FCLASS;                                 ///< class state (Unicast, Broadcast or Multicast) detected
    fm_bool           GLORT_CAM_MISS;
    fm_uint64         GLORT_DMASK[MBY_DMASK_REGISTERS];       ///< 258-bit GLORT-based destination mask
    fm_uint32         HASH_ROT_A;                             ///< rotation A hash value
    fm_uint32         HASH_ROT_B;                             ///< rotation B hash value
    fm_uint16         IDGLORT;                                ///< 16-bit ingress destination GLORT
    fm_uint16         IP_MCAST_IDX;                           ///< index into the MCAST_VLAN_TABLE
    fm_bool           IS_IPV4;                                ///< packet is of type IP v4
    fm_bool           IS_IPV6;                                ///< packet is of type IP v6
    fm_macaddr        L2_DMAC;                                ///< layer 2 destination address
    fm_byte           L2_EDOMAIN;                             ///< egress L2 domain
    fm_uint16         L2_EVID1;                               ///< 12-bit egress VLAN ID
    fm_macaddr        L2_SMAC;                                ///< layer 2 source MAC address
    fm_byte           L3_EDOMAIN;                             ///< egress L3 domain
    fm_bool           LEARNING_ENABLED;                       ///< learning enabled status within action codes
    fm_bool           LOGGING_HIT;                            ///< flag indicating whether logging was hit
    fm_byte           LOG_AMASK;                              ///< 6-bit logging action mask
    fm_bool           MAC_MOVED;                              ///< flag indicating that a non-secure MAC was found
    fm_bool           MARK_ROUTED;                            ///<
    fm_bool           MCAST_EPOCH;                            ///< current epoch for multicast garbage collection flag
    fm_uint32         MIRROR0_PORT;                           ///< mirror 0 port
    fm_uint32         MIRROR0_PROFILE_IDX;                    ///< mirror 0 profile index
    fm_byte           MIRROR0_PROFILE_V;                      ///< mirror 0 profile valid
    fm_uint32         MIRROR1_PORT;                           ///< mirror 1 port
    fm_uint32         MIRROR1_PROFILE_IDX;                    ///< mirror 1 profile index
    fm_byte           MIRROR1_PROFILE_V;                      ///< mirror 1 profile valid
    fm_byte           OPERATOR_ID;                            ///< 4-bit operator ID
    fm_bool           PA_L3LEN_ERR;                           ///< l3 length error
    fm_uint32         PRE_RESOLVE_ACTION;
    fm_uint16         PRE_RESOLVE_DGLORT;
    fm_uint64         PRE_RESOLVE_DMASK[MBY_DMASK_REGISTERS];
    fm_bool           QCN_MIRROR0_PROFILE_V;                  ///< qcn mirror 0 profile valid
    fm_bool           QCN_MIRROR1_PROFILE_V;                  ///< qcn mirror 1 profile valid
    fm_byte           QOS_TC;                                 ///< 4-bit switch priority
    fm_bool           RX_MIRROR;                              ///< rx mirror frame
    fm_uint32         RX_PORT;                                ///< RX port number
    fm_byte           SEG_META_ERR;                           ///< segment error
    fm_bool           SKIP_DGLORT_DEC;
    fm_bool           STORE_TRAP_ACTION;                      ///< flag indicating whether 4bit trap action code will be stored in metadata
    fm_bool           STRICT_GLORT_ROUTING;
    fm_bool           TARGETED_DETERMINISTIC;                 ///< mode is set to targeted deterministic
    fm_byte           XCAST;                                  ///< indicate Unicast, Multicast, or Broadcast
    // pass-thru:
    fm_byte           CGRP_TRIG;                              ///< classifier action triggers
    fm_uint32         CONTENT_ADDR;                           ///< MOD Content address, expressed in 32B units
    fm_byte           ECN;                                    ///< ECN value to use in egress packet
    fm_uint16         EDGLORT;                                ///< egress destination glort
    fm_bool           IS_TIMEOUT;
    fm_uint16         L2_IVLAN1_CNT;                          ///< ingress VLAN counter
    mbyMirrorType     MIRTYP;                                 ///< mirror type
    fm_uint32         MOD_IDX;                                ///< index into the MODIFY descriptor tables
    fm_byte           MOD_PROF_IDX;                           ///< modify profile index
    fm_bool           OOM;                                    ///< out of memory
    fm_bool           PA_DROP;                                ///< checksum validation error, drop pkt in tail proc
    mbyParserInfo     PARSER_INFO;                            ///< parser info struct
    mbyParserHdrPtrs  PA_HDR_PTRS;                            ///< parser header pointers
    fm_bool           PM_ERR;                                 ///< ECC error on PM
    fm_bool           PM_ERR_NONSOP;
    fm_byte           QOS_L3_DSCP;                            ///< 6-bit QOS Differentiated Services Code Point (DSCP)
    fm_uint64         TAIL_CSUM_LEN;                          ///< L4 CSUM related information
    fm_byte           TRAFFIC_CLASS;                          ///< 3-bit traffic class
    fm_byte           TX_TAG;                                 ///< egress tag
    fm_uint32         RX_LENGTH;                              ///< Ingress packet data length [bytes]

} mbyMaskGenToTriggers;

#endif // MBY_MSK2TRG_H
