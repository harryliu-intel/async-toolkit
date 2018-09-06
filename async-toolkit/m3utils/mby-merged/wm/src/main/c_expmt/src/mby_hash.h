// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_HASH_H
#define MBY_HASH_H

// Includes:

#include "mby_common.h"

// Defines:

#define MBY_ARP_VLAN_BASE                                       (0x3700000)
#define MBY_ARP_VLAN_SIZE                                       (0x0020000)

#define MBY_FWD_HASHING_CFG_WIDTH                               2
#define MBY_FWD_HASHING_CFG(word)                               (((word)*4) + (0x0051880) + (MBY_ARP_VLAN_BASE))

#define MBY_FWD_HASHING_CFG_b_USE_METADATA                      13
#define MBY_FWD_HASHING_CFG_b_ECMP_ROTATION                     12
#define MBY_FWD_HASHING_CFG_l_ROTATION_B                        10
#define MBY_FWD_HASHING_CFG_h_ROTATION_B                        11
#define MBY_FWD_HASHING_CFG_l_ROTATION_A                        8
#define MBY_FWD_HASHING_CFG_h_ROTATION_A                        9
#define MBY_FWD_HASHING_CFG_b_USE_VID                           7
#define MBY_FWD_HASHING_CFG_b_USE_VPRI                          6
#define MBY_FWD_HASHING_CFG_b_USE_TYPE                          5
#define MBY_FWD_HASHING_CFG_b_USE_SMAC                          4
#define MBY_FWD_HASHING_CFG_b_USE_DMAC                          3
#define MBY_FWD_HASHING_CFG_b_SYMMETRIC                         2
#define MBY_FWD_HASHING_CFG_b_USE_L34                           1
#define MBY_FWD_HASHING_CFG_b_USE_L2_IF_IP                      0

// Enums:

// Structs:

typedef struct mbyFwdHashingCfgStruct
{
    fm_bool                 USE_METADATA;
    fm_bool                 ECMP_ROTATION;
    fm_byte                 ROTATION_B;
    fm_byte                 ROTATION_A;
    fm_bool                 USE_VID;
    fm_bool                 USE_VPRI;
    fm_bool                 USE_TYPE;
    fm_bool                 USE_SMAC;
    fm_bool                 USE_DMAC;
    fm_bool                 SYMMETRIC;
    fm_bool                 USE_L34;
    fm_bool                 USE_L2_IF_IP;

} mbyFwdHashingCfg;

typedef struct mbyHashKeysStruct
{
    fm_uint64 crc34;
    fm_uint64 crc234;
    fm_byte   l234Key[17];
    fm_bool   zeroL2;
    fm_bool   zeroL34;
    fm_bool   useL34;
    fm_byte   rotA;
    fm_byte   rotB;
    fm_bool   arpEcmpCfg;

} mbyHashKeys;

typedef struct mbyHashToNextHopStruct
{
    fm_byte                 ARP_HASH[16];
    mbyHashKeys             HASH_KEYS;
    fm_uint32               HASH_ROT_A;
    fm_uint16               HASH_ROT_A_PTABLE_INDEX;
    fm_uint32               HASH_ROT_B;
    fm_uint16               HASH_ROT_B_PTABLE_INDEX;
    fm_macaddr              L2_DMAC;
    fm_macaddr              L2_SMAC;
    fm_uint16               RAW_HASH;
    // pass-thru:
    fm_uint32               ACTION;                 // resolved action
    fm_bool                 CPU_TRAP;               // CPU trap
    fm_uint16               CSGLORT;                // 16-bit canonical source GLORT
    fm_bool                 DECAP;
    fm_macaddr              DMAC_FROM_IPV6;
    fm_bool                 DROP_TTL;               // packet should be dropped
    fm_bool                 ENCAP;
    mbyClassifierFlags      FFU_FLAGS;              // flags {CAPTURE-TIME, RX_MIRROR, NO_ROUTE, LOG, TRAP, DROP}
    fm_uint32               FFU_ROUTE;
    fm_bool                 GLORT_CAM_MISS;         // GLORT lookup resulted in a miss flag
    fm_uint32               GLORT_DMASK;            // 24-bit GLORT-based destination mask
    fm_uint16               IP_MCAST_IDX;           // index into the MCAST_VLAN_TABLE
    fm_bool                 IS_IPV4;                // packet is IPv4
    fm_bool                 IS_IPV6;                // packet is IPv6
    fm_uint32               L2_EFID1_STATE;         // 24-bit egress forwarding vector
    fm_uint16               L2_ETYPE;               // 16-bit innermost Ethernet type
    fm_uint32               L2_EVLAN1_MEMBERSHIP;   // 24-bit egress VLAN port membership vector
    fm_uint16               L2_IDOMAIN;
    mbyStpState             L2_IFID1_STATE;         // 2-bit spanning tree state for the ingress port
    fm_uint16               L2_IVID1;
    fm_bool                 L2_IVLAN1_MEMBERSHIP;   // ingress port is part of the ingress VLAN flag
    fm_bool                 L2_IVLAN1_REFLECT;      // ingress VLAN reflection is enabled
    fm_byte                 L3_IDOMAIN;
    fm_bool                 LEARN_MODE;
    fm_uint32               MIRROR0_PROFILE_IDX;    // mirror 0 profile index
    fm_bool                 MTU_VIOLATION;          // packet violates the MTU
    fm_bool                 NO_LEARN;               // learning is diabled flag
    fm_byte                 OPERATOR_ID;            // 4-bit operator ID
    fm_bool                 PARITY_ERROR;           // memory parity error flag
    fm_bool                 PARSER_ERROR;           // header parse error flag
    mbyParserInfo           PARSER_INFO;            // parser info structure
    fm_bool                 PARSER_WINDOW_V;        // parser window valid flag
    fm_bool                 PA_DROP;                // checksum validation error, drop pkt in tail proc
    fm_bool                 PA_L3LEN_ERR;           // l3 length error
    fm_uint32               PRE_RESOLVE_DMASK;      // destination mask before action resolution
    fm_byte                 QOS_SWPRI;              // 4-bit switch priority
    fm_uint32               RX_LENGTH;              // RX packet length
    fm_bool                 RX_MIRROR;              // rx mirror frame
    fm_uint32               RX_PORT;                // receive port number
    fm_bool                 SA_HIT;                 // source MAC address lookup hit
    mbyMaTable              SA_RESULT;              // source MAC address lookup result
    fm_byte                 SEG_META_ERR;           // segment error
    fm_byte                 SV_DROP;                // MAC security violation info
    fm_bool                 TARGETED_DETERMINISTIC; // mode is set to targeted deterministic
    fm_byte                 TRAFFIC_CLASS;          // 3-bit traffic class
    fm_bool                 TRAP_ICMP;              // ICMP packet should be trapped
    fm_bool                 TRAP_IGMP;              // IGMP packet should be trapped
    fm_bool                 TRAP_IP_OPTIONS;        // IP options present
    mbyTriggerResults       TRIGGERS;               // trigger results

} mbyHashToNextHop;

#endif
