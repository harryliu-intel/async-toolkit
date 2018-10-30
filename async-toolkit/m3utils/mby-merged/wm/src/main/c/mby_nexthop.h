// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_NEXTHOP_H
#define MBY_NEXTHOP_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"
#include "mby_classifier.h" // mbyClassifierFlags
#include "mby_maskgen.h"

// Defines:

#define MBY_ARP_HASH_ENTRIES              16

/******** ARP_VLAN_BASE *******/
#define MBY_ARP_VLAN_BASE                 (0x3700000)
#define MBY_ARP_VLAN_SIZE                 (0x0020000)

#define MBY_ARP_TABLE_WIDTH               4
#define MBY_ARP_TABLE_ENTRIES             16384
#define MBY_ARP_TABLE(index, word)        ((0x0000010) * ((index) - 0) + ((word)*4)+ (0x0000000) + (MBY_ARP_VLAN_BASE))

#define MBY_ARP_TABLE_b_UPDATE_L3_DOMAIN  113
#define MBY_ARP_TABLE_b_UPDATE_L2_DOMAIN  112
#define MBY_ARP_TABLE_l_L3_DOMAIN         106
#define MBY_ARP_TABLE_h_L3_DOMAIN         111
#define MBY_ARP_TABLE_l_L2_DOMAIN         97
#define MBY_ARP_TABLE_h_L2_DOMAIN         105
#define MBY_ARP_TABLE_l_MOD_IDX           79
#define MBY_ARP_TABLE_h_MOD_IDX           96
#define MBY_ARP_TABLE_l_MTU_INDEX         76
#define MBY_ARP_TABLE_h_MTU_INDEX         78
#define MBY_ARP_TABLE_l_EVID              64
#define MBY_ARP_TABLE_h_EVID              75
#define MBY_ARP_TABLE_l_RESERVED          50
#define MBY_ARP_TABLE_h_RESERVED          63
#define MBY_ARP_TABLE_b_IPV6_ENTRY        49
#define MBY_ARP_TABLE_b_ENTRY_TYPE        48
#define MBY_ARP_TABLE_l_DST_MAC           0
#define MBY_ARP_TABLE_h_DST_MAC           47

#define MBY_ARP_USED_WIDTH                2
#define MBY_ARP_USED_ENTRIES              256
#define MBY_ARP_USED(index, word)         ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0051000) + (MBY_ARP_VLAN_BASE))

#define MBY_ARP_USED_l_USED               0
#define MBY_ARP_USED_h_USED               63

#define MBY_ARP_ENTRY_GLORT_WIDTH         2
#define MBY_ARP_ENTRY_GLORT_l_DGLORT      0
#define MBY_ARP_ENTRY_GLORT_h_DGLORT      15
#define MBY_ARP_ENTRY_GLORT_b_MARK_ROUTED 16

// Below defines are related to L2Lookup and temporary placed in nexthop <-- REVISIT!!!
#define MBY_FLOOD_GLORT_TABLE_WIDTH                   2
#define MBY_FLOOD_GLORT_TABLE_ENTRIES                 512
#define MBY_FLOOD_GLORT_TABLE(index, word)            ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0050000) + (MBY_ARP_VLAN_BASE))

#define MBY_FLOOD_GLORT_TABLE_l_BROADCAST_GLORT       32
#define MBY_FLOOD_GLORT_TABLE_h_BROADCAST_GLORT       47
#define MBY_FLOOD_GLORT_TABLE_l_FLOOD_MULTICAST_GLORT 16
#define MBY_FLOOD_GLORT_TABLE_h_FLOOD_MULTICAST_GLORT 31
#define MBY_FLOOD_GLORT_TABLE_l_FLOOD_UNICAST_GLORT   0
#define MBY_FLOOD_GLORT_TABLE_h_FLOOD_UNICAST_GLORT   15

#define MBY_INGRESS_VID_TABLE_WIDTH                   2
#define MBY_INGRESS_VID_TABLE_ENTRIES                 4096
#define MBY_INGRESS_VID_TABLE(index, word)            ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0040000) + (MBY_ARP_VLAN_BASE))

#define MBY_INGRESS_VID_TABLE_b_TRAP_IGMP             25
#define MBY_INGRESS_VID_TABLE_b_REFLECT               24
#define MBY_INGRESS_VID_TABLE_l_MEMBERSHIP            0
#define MBY_INGRESS_VID_TABLE_h_MEMBERSHIP            23

#define MBY_EGRESS_VID_TABLE_WIDTH                    2
#define MBY_EGRESS_VID_TABLE_ENTRIES                  4096
#define MBY_EGRESS_VID_TABLE(index, word)             ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0048000) + (MBY_ARP_VLAN_BASE))

#define MBY_EGRESS_VID_TABLE_l_TRIG_ID                24
#define MBY_EGRESS_VID_TABLE_h_TRIG_ID                29
#define MBY_EGRESS_VID_TABLE_l_MEMBERSHIP             0
#define MBY_EGRESS_VID_TABLE_h_MEMBERSHIP             23

#define MBY_INGRESS_MST_TABLE_WIDTH                   2
#define MBY_INGRESS_MST_TABLE_ENTRIES                 4096
#define MBY_INGRESS_MST_TABLE(index, word)            ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0000000) + (MBY_MST_GLORT_BASE))

#define MBY_INGRESS_MST_TABLE_l_STP_STATE_23          46
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_23          47
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_22          44
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_22          45
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_21          42
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_21          43
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_20          40
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_20          41
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_19          38
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_19          39
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_18          36
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_18          37
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_17          34
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_17          35
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_16          32
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_16          33
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_15          30
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_15          31
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_14          28
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_14          29
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_13          26
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_13          27
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_12          24
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_12          25
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_11          22
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_11          23
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_10          20
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_10          21
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_9           18
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_9           19
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_8           16
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_8           17
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_7           14
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_7           15
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_6           12
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_6           13
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_5           10
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_5           11
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_4           8
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_4           9
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_3           6
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_3           7
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_2           4
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_2           5
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_1           2
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_1           3
#define MBY_INGRESS_MST_TABLE_l_STP_STATE_0           0
#define MBY_INGRESS_MST_TABLE_h_STP_STATE_0           1

#define MBY_EGRESS_MST_TABLE_WIDTH                    2
#define MBY_EGRESS_MST_TABLE_ENTRIES                  4096
#define MBY_EGRESS_MST_TABLE(index, word)             ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0008000) + (MBY_MST_GLORT_BASE))

#define MBY_EGRESS_MST_TABLE_l_FORWARDING             0
#define MBY_EGRESS_MST_TABLE_h_FORWARDING             23

#define MBY_L2LOOKUP_BASE                             (0x3800000)
#define MBY_L2LOOKUP_SIZE                             (0x0200000)

#define MBY_MA_TABLE_WIDTH                            4
#define MBY_MA_TABLE_ENTRIES_0                        8192
#define MBY_MA_TABLE_ENTRIES_1                        6
#define MBY_MA_TABLE(index1, index0, word)            ((0x0020000) * ((index1) - 0) + (0x0000010) * ((index0) - 0) + ((word)*4)+ (0x0000000) + (MBY_L2LOOKUP_BASE))

#define MBY_MA_TABLE_l__RSVD5_                        125
#define MBY_MA_TABLE_h__RSVD5_                        127
#define MBY_MA_TABLE_l_OLD_PORT                       120
#define MBY_MA_TABLE_h_OLD_PORT                       124
#define MBY_MA_TABLE_l_NEW_PORT                       115
#define MBY_MA_TABLE_h_NEW_PORT                       119
#define MBY_MA_TABLE_l_ENTRY_TYPE                     112
#define MBY_MA_TABLE_h_ENTRY_TYPE                     114
#define MBY_MA_TABLE_l__RSVD3_                        110
#define MBY_MA_TABLE_h__RSVD3_                        111
#define MBY_MA_TABLE_l_TRIG_ID                        104
#define MBY_MA_TABLE_h_TRIG_ID                        109
#define MBY_MA_TABLE_l_S_GLORT                        88
#define MBY_MA_TABLE_h_S_GLORT                        103
#define MBY_MA_TABLE_l_D_GLORT                        72
#define MBY_MA_TABLE_h_D_GLORT                        87
#define MBY_MA_TABLE_l__RSVD2_                        70
#define MBY_MA_TABLE_h__RSVD2_                        71
#define MBY_MA_TABLE_b__RSVD1_                        69
#define MBY_MA_TABLE_l_L2_DOMAIN                      60
#define MBY_MA_TABLE_h_L2_DOMAIN                      68
#define MBY_MA_TABLE_l_VID                            48
#define MBY_MA_TABLE_h_VID                            59
#define MBY_MA_TABLE_l_MAC_ADDRESS                    0
#define MBY_MA_TABLE_h_MAC_ADDRESS                    47

#define MBY_MA_TABLE_CAM_BANK           5
#define MBY_MA_TABLE_CAM_ENTRIES        1024

/* The number of banks in the MAC Address Table. */
#define MBY_MAC_ADDR_BANK_COUNT         6

/* The size of MA_TABLE tcam size*/
#define MBY_MA_TABLE_TCAM_SIZE          1024

#define MBY_MA_ENTRY_TYPE_NOT_USED      0
#define MBY_MA_ENTRY_TYPE_PROVISIONAL   1
#define MBY_MA_ENTRY_TYPE_DYNAMIC       2
#define MBY_MA_ENTRY_TYPE_SECURE        3
#define MBY_MA_ENTRY_TYPE_STATIC        4
#define MBY_MA_ENTRY_TYPE_SECURE_STATIC 5

// Enums:

typedef enum mbyArpEntryTypeEnum
{
    MBY_ARP_TYPE_GLORT = 0,
    MBY_ARP_TYPE_MAC   = 1

} mbyArpEntryType;

// Structs:

typedef struct mbyArpTableStruct
{

    mbyArpEntryType    EntryType; // 1 = MAC entry, 0 - Glort Entry
    fm_macaddr         DMAC; // used for DMAC entries
    fm_uint16          DGLORT; // used for GLORT entries
    fm_byte            MTU_Index; // used for GLORT entries
    fm_bool            markRouted; // used for GLORT entries
    fm_bool            IPv6Entry; // used for GLORT entries
    fm_uint16          EVID;
    fm_byte            L3Domain;
    fm_uint16          L2Domain;
    fm_bool            UpdateL3Domain;
    fm_bool            UpdateL2Domain;
    fm_uint32          ModIdx;

} mbyArpTable;

typedef struct mbyIngressVidTableStruct
{
  fm_bool              TRAP_IGMP;
  fm_bool              REFLECT;
  fm_uint32            MEMBERSHIP;
} mbyIngressVidTable;

typedef struct mbyEgressVidTableStruct
{
  fm_byte              TRIG_ID;
  fm_uint32            MEMBERSHIP;
} mbyEgressVidTable;

typedef struct mbyNextHopToMaskGenStruct
{
    fm_uint64          AMASK;                // action mask
    fm_uint16          CSGLORT;              // 16-bit canonical source GLORT
    fm_bool            DA_HIT;               // destination MAC address lookup hit
    fm_bool            DROP_TTL;             // packet should be dropped
    mbyClassifierFlags CGRP_FLAGS;           // flags {CAPTURE-TIME, RX_MIRROR, NO_ROUTE, LOG, TRAP, DROP}
    fm_bool            FLOOD_FORWARDED;      // glort is flood-forwarded
    fm_uint32          GLORT_DMASK;          // 24-bit GLORT-based destination mask
    fm_bool            GLORT_FORWARDED;      // glort forwarded due to FFU rule
    fm_uint32          HASH_ROT_A;           // rotation A hash value
    fm_uint32          HASH_ROT_B;           // rotation B hash value
    fm_uint16          IDGLORT;              // 16-bit ingress destination GLORT
    fm_bool            IS_IPV4;              // packet is IPv4
    fm_bool            IS_IPV6;              // packet is IPv6
    fm_macaddr         L2_DMAC;              // layer 2 destination MAC address
    fm_uint16          L2_EDOMAIN;           // egress L2 domain
    fm_uint16          L2_ETYPE;             // 16-bit innermost Ethernet type
    fm_uint16          L2_EVID1;             // 12-bit egress VLAN ID
    fm_uint16          L2_IVID1;             // 12-bit ingress VLAN ID
    fm_bool            L2_IVLAN1_MEMBERSHIP; // ingress port is part of the ingress VLAN flag
    fm_bool            L2_IVLAN1_REFLECT;    // ingress VLAN reflection is enabled
    fm_macaddr         L2_SMAC;              // layer 2 source MAC address
    fm_byte            L3_EDOMAIN;           // egress L3 domain
    fm_bool            MARK_ROUTED;          //
    fm_bool            MTU_VIOLATION;        // packet violates the MTU
    fm_bool            NO_LEARN;             // learning is diabled flag
    fm_byte            OPERATOR_ID;          // 4-bit operator ID
    fm_bool            PARITY_ERROR;         // memory parity error flag
    fm_bool            PARSER_WINDOW_V;      // parser window valid flag
    fm_bool            PARSER_ERROR;         // header parse error flag
    fm_bool            PA_DROP;              // checksum validation error, drop pkt in tail proc
    fm_bool            PA_L3LEN_ERR;         // l3 length error
    fm_byte            QOS_TC;               // 4-bit switch priority
    fm_uint32          RX_LENGTH;            // RX packet length
    fm_bool            RX_MIRROR;            // rx mirror frame
    fm_uint32          RX_PORT;              // receive port number
    fm_bool            SA_HIT;               // source MAC address lookup hit
    mbyMaTable         SA_RESULT;            // source MAC address lookup result
    fm_byte            SEG_META_ERR;         // segment error
    fm_byte            SV_DROP;              // MAC security violation info
    fm_bool            TRAP_ICMP;            // ICMP packet should be trapped
    fm_bool            TRAP_IGMP;            // IGMP packet should be trapped
    fm_bool            TRAP_IP_OPTIONS;      // IP options present
    mbyTriggerResults  TRIGGERS;             // trigger results

    // pass-thru:
    fm_byte            ECN;                  // ECN value to use in egress packet
    fm_uint16          EDGLORT;              // egress destination glort
    fm_bool            IS_TIMEOUT;           //
    fm_uint16          L2_IVLAN1_CNT;        // ingress VLAN counter
    mbyMirrorType      MIRTYP;               // mirror type
    fm_uint32          MOD_IDX;              // index into the MODIFY descriptor tables
    fm_byte            MOD_PROF_IDX;         // modify profile index
    fm_bool            OOM;                  // out of memory
    mbyParserInfo      PARSER_INFO;          // parser info structure
    mbyParserHdrPtrs   PA_HDR_PTRS;          // parser header pointers
    fm_bool            PM_ERR;               // ECC error on PM
    fm_bool            PM_ERR_NONSOP;        //
    fm_byte            QOS_L3_DSCP;          // 6-bit QOS Differentiated Services Code Point (DSCP):
    fm_byte          * RX_DATA;              // ingress (receive) packet data
    fm_uint64          TAIL_CSUM_LEN;        // L4 CSUM related information
    fm_byte            TRAFFIC_CLASS;        // traffic class
    fm_byte            TX_TAG;               // transmit tag from Classifier
    fm_uint16          ARP_TABLE_INDEX;
    fm_bool            ENCAP;
    fm_bool            DECAP;
    fm_uint16          L2_IDOMAIN;
    fm_byte            L3_IDOMAIN;
    fm_byte            MTU_INDEX;
    fm_bool            FLOOD_SET;
    mbyMaTable         DA_RESULT;            // destination MAC address lookup result
    fm_bool            CPU_TRAP;             // CPU trap
    fm_uint32          PRE_RESOLVE_DMASK;    // destination mask before action resolution
    fm_uint32          ACTION;               // resolved action
    fm_uint16          IP_MCAST_IDX;         // index into the MCAST_VLAN_TABLE
    fm_uint32          MIRROR0_PROFILE_IDX;  // mirror 0 profile index
} mbyNextHopToMaskGen;

#endif
