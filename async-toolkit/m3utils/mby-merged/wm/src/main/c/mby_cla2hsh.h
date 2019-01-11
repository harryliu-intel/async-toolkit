// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_CLA2HSH_H
#define MBY_CLA2HSH_H

#include "fm_types.h"
#include "mby_cgrp_types.h"
#include "mby_parser_info.h"
#include "mby_par_hdr_ptrs.h"

typedef struct mbyClassifierToHashStruct
{
    mbyClassifierFunctions CGRP_FUNCTIONS;                              ///< classifier functions {MPLS_MUX, VID, VPRI, DSCP, TC, HASH_PROFILE[], MOD_META[], FWD, POLICER[], MOD_PROFILE, REMAP, USED}
    mbyClassifierFlags     CGRP_FLAGS;                                  ///< classifier flags {CAPTURE-TIME, RX_MIRROR, NO_ROUTE, LOG, TRAP, DROP}
    fm_byte                AQM_MARK_EN;                                 ///< egress AQM_MARK_EN
    fm_byte                DECAP;                                       ///< decapsulate flag
    fm_macaddr             DMAC_FROM_IPV6;                              ///< DMAC embedded in DIP of IPV6 packet
    fm_bool                DROP_TTL;                                    ///< drop packet (depending on TTL value)
    fm_byte                ECN;                                         ///< egress ECN value
    fm_bool                ENCAP;                                       ///< encapsulate flag
    fm_uint32              FWD;                                         ///< classifier route
    fm_byte                CGRP_TRIG;                                   ///< classifier action triggers
    fm_uint32              CONTENT_ADDR;                                ///< MOD Content address, expressed in 32B units
    fm_uint16              IDGLORT;                                     ///< 16-bit ingress destination GLORT
    fm_uint16              INNER_L3_LENGTH;                             ///< 16-bit IPv4 datagram length for Inner header
    fm_bool                IS_IPV4;                                     ///< packet is of type IP version 4
    fm_bool                IS_IPV6;                                     ///< packet is of type IP version 6
    fm_macaddr             L2_DMAC;                                     ///< layer 2 destination address
    fm_uint16              L2_ETYPE;                                    ///< 16-bit innermost Ethernet type
    fm_uint16              L2_IVID1;                                    ///< 12-bit ingress VLAN ID
    fm_macaddr             L2_SMAC;                                     ///< layer 2 source address
    fm_byte                HASH_PROFILE[MBY_CGRP_HASH_PROFILE_ACTIONS]; ///< hash actions from the classifier
    fm_byte                MOD_META[MBY_CGRP_META_ACTIONS];             ///< META fields from the classifier
    fm_uint16              L3_LENGTH;                                   ///< 16-bit IPv4 datagram length for outer/inner header
    fm_uint32              MOD_IDX;                                     ///< index into the MODIFY descriptor tables
    fm_byte                MOD_PROF_IDX;                                ///< modify profile index
    fm_byte                MPLS_POP;                                    ///< # of MPLS labels to pop in Modify
    fm_bool                LEARN_NOTIFY;                                ///< enable learning
    fm_uint16              OUTER_L3_LENGTH;                             ///< 16-bit IPv4 datagram length for outer header
    fm_uint32              POLICER_ACTION[MBY_CGRP_POLICER_ACTIONS];    ///< policer actions
    fm_byte                QOS_L2_VPRI1;                                ///< 4-bit QOS VLAN priority
    fm_byte                QOS_L3_DSCP;                                 ///< 6-bit QOS Differentiated Services Code Point (DSCP):
    fm_byte                QOS_TC;                                      ///< 4-bit quality of service priority
    fm_uint16              SGLORT;                                      ///< 16-bit source GLORT
    fm_bool                TRAP_ICMP;                                   ///< trap ICMP packet
    fm_bool                TRAP_IGMP;                                   ///< trap IGMP packet
    fm_bool                TRAP_IP_OPTIONS;                             ///< flag indicating presence of IP options
    fm_byte                TTL_CTRL;                                    ///< controls update of TTL field of egress packet
    fm_byte                TX_TAG;                                      ///< transmit tag
    // pass-thru:
    mbyClassifierKeys      CLASSIFIER_KEYS;                             ///< classifier TCAM lookup keys
    fm_bool                LEARN_MODE;                                  ///< learn mode: 0=SVL, 1=IVL
    fm_uint16              L2_IDOMAIN;                                  ///< ingress L2 domain
    fm_byte                L3_IDOMAIN;                                  ///< ingress L3 domain
    fm_byte                OPERATOR_ID;                                 ///< 4-bit operator ID (NAD)
    fm_bool                PARSER_ERROR;                                ///< header parse error
    mbyParserInfo          PARSER_INFO;                                 ///< parser info structure
    fm_bool                PARITY_ERROR;                                ///< parity error detected flag
    fm_bool                PA_DROP;                                     ///< checksum validation error, drop pkt in tail
    fm_bool                PA_L3LEN_ERR;                                ///< l3 length error
    mbyParserHdrPtrs       PA_HDR_PTRS;                                 ///< parser header pointers
    fm_uint32              RX_PORT;                                     ///< ingress port
    fm_byte                TRAFFIC_CLASS;                               ///< 3-bit traffic class
    fm_uint32              RX_LENGTH;                                   ///< Ingress packet data length [bytes]

} mbyClassifierToHash;

#endif // MBY_CLA2HSH_H
