// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_NEXTHOP_H
#define MBY_NEXTHOP_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"
#include "mby_classifier.h" // mbyClassifierFlags
//#include "mby_maskgen.h"

// Defines:
#define MBY_NH_NEIGHBORS_ENTRIES   16384
#define MBY_NH_MAX_WCMP_GROUP_SIZE 64

/* Bit numbers for mapping neighbor index to NH_USED entry */
#define MBY_NH_USED_h_INDEX 13
#define MBY_NH_USED_l_INDEX  6
#define MBY_NH_USED_h_BIT    5
#define MBY_NH_USED_l_BIT    0

/* Bit numbers for fields of FWD Action from Classifier */
#define MBY_FWD_b_SUBTYPE              21
// Bit numbers when FWD Subtype == FWD GLORT
#define MBY_FWD_GLORT_h_RESERVED       20
#define MBY_FWD_GLORT_l_RESERVED       18
#define MBY_FWD_GLORT_b_FORWARDED_TYPE 17
#define MBY_FWD_GLORT_b_IS_FLOODSET    16
#define MBY_FWD_GLORT_h_DGLORT         15
#define MBY_FWD_GLORT_l_DGLORT         0
// Bit numbers when FWD Subtype == ROUTE ARP
#define MBY_FWD_ARP_b_ROUTE_TYPE       20
#define MBY_FWD_ARP_h_RESERVED         19
#define MBY_FWD_ARP_l_RESERVED         16
#define MBY_FWD_ARP_h_ROUTE_INDEX      15
#define MBY_FWD_ARP_l_ROUTE_INDEX      0


// Enums:

typedef enum mbyNextHopFlowletPolicyEnum
{
    MBY_NH_FLOWLET_POLICY_SOFTWARE = 0,
    MBY_NH_FLOWLET_POLICY_LETFLOW,
    MBY_NH_FLOWLET_POLICY_PATH_LOADING,
    MBY_NH_FLOWLET_POLICY_SDN_MANAGED,
    MBY_NH_FLOWLET_POLICY_LOCAL_CONGESTION,
    MBY_NH_FLOWLET_POLICY_CONGESTION_NOTIFICATION,
    MBY_NH_FLOWLET_POLICY_POWER_AWARE,
    MBY_NH_FLOWLET_POLICY_RESERVED

} mbyNextHopFlowletPolicy;

typedef enum mbyNextHopGroupTypeEnum
{
    MBY_NH_GROUP_TYPE_ECMP         = 0,
    MBY_NH_GROUP_TYPE_WCMP         = 1,
    MBY_NH_GROUP_TYPE_ECMP_FLOWLET = 2,
    MBY_NH_GROUP_TYPE_RESERVED     = 3

} mbyNextHopGroupType;

typedef enum mbyNextHopGroupSizeTypeEnum
{
    MBY_NH_GROUP_SIZE_TYPE_LITERAL = 0,
    MBY_NH_GROUP_SIZE_TYPE_POWER_OF_2

} mbyNextHopGroupSizeType;

typedef enum mbyNextHopNeighborTypeEnum
{
    MBY_NH_ENTRY_TYPE_GLORT_FORWARDING = 0,
    MBY_NH_ENTRY_TYPE_IP_ROUTING

} mbyNextHopNeighborEntryType;

typedef enum mbyNextHopRouteTypeEnum
{
    MBY_NH_ROUTE_TYPE_SINGLE = 0,
    MBY_NH_ROUTE_TYPE_GROUP

} mbyNextHopRouteType;


// Structs:

typedef struct mbyNextHopRouteStruct
{
    fm_byte   age_counter;
    fm_uint16 group_index;
    fm_uint16 neighbor_index;

} mbyNextHopRoute;

typedef struct mbyNextHopGroupStruct
{
    mbyNextHopGroupType     group_type;
    fm_uint16               base_index;
    mbyNextHopGroupSizeType n_group_size_type;
    fm_uint16               n_group_size;
    fm_uint16               weight_row;
    fm_byte                 weight_row_offset;
    mbyNextHopGroupSizeType r_group_size_type;
    fm_uint16               r_group_size;
    mbyNextHopFlowletPolicy flowlet_policy;
    fm_byte                 flowlet_age_reset;
    fm_uint16               group_min_index;

} mbyNextHopGroup;

typedef struct mbyNextHopNeighborStruct
{
    mbyNextHopNeighborEntryType entry_type;
    fm_uint16                   dglort;
    fm_bool                     update_l2domain;
    fm_byte                     l2domain; // To be moved to NH_EGR_TABLE? <-- REVISIT!!!
    fm_bool                     update_l3domain;
    fm_byte                     l3domain; // To be moved to NH_EGR_TABLE? <-- REVISIT!!!
    fm_bool                     mark_routed;
    fm_uint32                   mod_idx;
    fm_bool                     IPv6_entry;
    fm_byte                     mtu_idx;
    fm_macaddr                  dmac;     // To be moved to NH_EGR_TABLE? <-- REVISIT!!!
    fm_uint16                   evid;

} mbyNextHopNeighbor;

typedef struct mbyNextHopConfigStruct
{
    fm_byte sweeper_rate;
    fm_bool flowlet_int_en;
    fm_bool flowlet_enable; // if FALSE => Nexthop_Sweep && processPacketTail are disabled

} mbyNextHopConfig;

typedef struct mbyIngressVidTableStruct
{
  fm_bool      TRAP_IGMP;
  fm_bool      REFLECT;
  fm_uint32    MEMBERSHIP;

} mbyIngressVidTable;

typedef struct mbyEgressVidTableStruct
{
  fm_byte      TRIG_ID;
  fm_uint32    MEMBERSHIP;

} mbyEgressVidTable;

typedef struct mbyNextHopApplyInputStruct
{
    fm_uint16  ecmp_hash;
    fm_uint32  fwd;
    fm_byte    l2_idomain;
    fm_byte    l3_idomain;
    fm_uint16  l2_ivid1;
    fm_macaddr dmac_from_ipv6;
    fm_bool    flowlet_enable;
    fm_uint32  rx_length;

} mbyNextHopApplyInput;

typedef struct mbyNextHopApplyOutputStruct
{
    fm_macaddr l2_dmac;
    fm_uint16  l2_evid;
    fm_uint16  dglort;
    fm_bool    mark_routed;
    fm_byte    l2_edomain;
    fm_byte    l3_edomain;
    fm_bool    flowlet_enabled_packet;
    fm_uint16  route_neighbor_idx;
    fm_uint16  group_min_index;
    fm_bool    route;
    fm_bool    mtu_violation;

} mbyNextHopApplyOutput;

typedef struct mbyNextHopSweepInputStruct
{
    fm_bool    flowlet_enabled_packet;
    fm_uint16  route_neighbor_idx;
    fm_uint16  group_min_index;

} mbyNextHopSweepInput;

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

#endif
