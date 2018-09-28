// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_CLASSIFIER_H
#define MBY_CLASSIFIER_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"
#include "mby_clsfr_regs.h"

// Defines:

// Enums:

// Structs:

typedef struct mbyClassifierToHashStruct
{
    fm_byte                 AQM_MARK_EN;                         // egress AQM_MARK_EN
    fm_bool                 DECAP;                               // decapsulate flag
    fm_macaddr              DMAC_FROM_IPV6;                      // DMAC embedded in DIP of IPV6 packet
    fm_bool                 DROP_TTL;                            // drop packet (depending on TTL value)
    fm_byte                 ECN;                                 // egress ECN value
    fm_bool                 ENCAP;                               // encapsulate flag
    mbyClassifierFlags      FFU_FLAGS;                           // classifier flags {CAPTURE-TIME, RX_MIRROR, NO_ROUTE, LOG, TRAP, DROP}
    fm_uint32               FFU_ROUTE;                           // classifier route
    fm_byte                 FFU_TRIG;                            // classifier action triggers
    fm_uint16               IDGLORT;                             // 16-bit ingress destination GLORT
    fm_uint16               INNER_L3_LENGTH;                     // 16-bit IPv4 datagram length for Inner header
    fm_bool                 IS_IPV4;                             // packet is of type IP version 4
    fm_bool                 IS_IPV6;                             // packet is of type IP version 6
    fm_macaddr              L2_DMAC;                             // layer 2 destination address
    fm_uint16               L2_ETYPE;                            // 16-bit innermost Ethernet type
    fm_uint16               L2_IVID1;                            // 12-bit ingress VLAN ID
    fm_macaddr              L2_SMAC;                             // layer 2 source address
    fm_uint64               L34_HASH;                            // hash value to be used by ARP_TABLE
    fm_uint16               L3_LENGTH;                           // 16-bit IPv4 datagram length for outer/inner header
    fm_uint32               MOD_IDX;                             // index into the MODIFY descriptor tables
    fm_byte                 MPLS_POP;                            // # of MPLS labels to pop in Modify
    fm_bool                 NO_LEARN;                            // disable learning
    fm_uint16               OUTER_L3_LENGTH;                     // 16-bit IPv4 datagram length for outer header
    fm_uint32               POLICER_ACTION[MBY_FFU_POL_ACTIONS]; // policer actions
    fm_byte                 QOS_L2_VPRI1;                        // 4-bit QOS VLAN priority
    fm_byte                 QOS_L3_DSCP;                         // 6-bit QOS Differentiated Services Code Point (DSCP):
    fm_byte                 QOS_SWPRI;                           // 4-bit quality of service priority
    fm_uint16               SGLORT;                              // 16-bit source GLORT
    fm_bool                 TRAP_ICMP;                           // trap ICMP packet
    fm_bool                 TRAP_IGMP;                           // trap IGMP packet
    fm_bool                 TRAP_IP_OPTIONS;                     // flag indicating presence of IP options
    fm_byte                 TTL_CTRL;                            // controls update of TTL field of egress packet
    fm_byte                 TX_TAG;                              // transmit tag
    // pass-thru:
    fm_bool                 LEARN_MODE;                          // learn mode: 0=SVL, 1=IVL
    fm_uint16               L2_IDOMAIN;                          // ingress L2 domain
    fm_byte                 L3_IDOMAIN;                          // ingress L3 domain
    fm_bool                 PARSER_ERROR;                        // header parse error
    mbyParserInfo           PARSER_INFO;                         // parser info structure
    fm_bool                 PARITY_ERROR;                        // parity error detected flag
    fm_bool                 PA_DROP;                             // checksum validation error, drop pkt in tail
    fm_bool                 PA_L3LEN_ERR;                        // l3 length error
    fm_byte               * RX_DATA;                             // ingress (receive) packet data
    fm_uint32               RX_LENGTH;                           // ingress packet data length [bytes]
    fm_uint32               RX_PORT;                             // ingress port
    fm_byte                 TRAFFIC_CLASS;                       // 3-bit traffic class

} mbyClassifierToHash;

#endif /* MBY_CLASSIFIER_H */
