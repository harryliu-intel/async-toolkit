// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_NEXTHOP_H
#define MBY_NEXTHOP_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"

// Defines:

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
#define MBY_ARP_ENTRY_GLORT_b_markRouted  16

// Enums:

typedef enum mbyArpEntryTypeEnum
{
    MBY_ARP_TYPE_GLORT = 0,
    MBY_ARP_TYPE_MAC   = 1

} mbyArpEntryType;

typedef enum mbySTPStateEnum
{
    MBY_STP_STATE_DISABLE = 0,
    MBY_STP_STATE_LISTENING,
    MBY_STP_STATE_LEARNING,
    MBY_STP_STATE_FORWARD

} mbySTPState;

typedef enum mbyTriggerActionForwardingEnum
{
    MBY_TRIG_ACTION_FORWARDING_AS_IS = 0,
    MBY_TRIG_ACTION_FORWARDING_FORWARD,
    MBY_TRIG_ACTION_FORWARDING_REDIRECT,
    MBY_TRIG_ACTION_FORWARDING_DROP

} mbyTriggerActionForwarding;

typedef enum mbyTriggerActionTrapEnum
{
    MBY_TRIG_ACTION_TRAP_AS_IS = 0,
    MBY_TRIG_ACTION_TRAP_TRAP,
    MBY_TRIG_ACTION_TRAP_LOG,
    MBY_TRIG_ACTION_TRAP_REVERT

} mbyTriggerActionTrap;

typedef enum mbyTriggerActionMirroringEnum
{
    MBY_TRIG_ACTION_MIRRORING_AS_IS = 0,
    MBY_TRIG_ACTION_MIRRORING_MIRROR,
    MBY_TRIG_ACTION_MIRRORING_CANCEL

} mbyTriggerActionMirroring;

typedef enum mbyTriggerActionTCEnum
{
    MBY_TRIG_ACTION_TC_AS_IS = 0,
    MBY_TRIG_ACTION_TC_REASSIGN

} mbyTriggerActionTC;

typedef enum mbyTriggerActionVlanEnum
{
    MBY_TRIG_ACTION_VLAN_AS_IS = 0,
    MBY_TRIG_ACTION_VLAN_REASSIGN

} mbyTriggerActionVlan;

typedef enum mbyTriggerActionLearningEnum
{
    MBY_TRIG_ACTION_LEARNING_AS_IS = 0,
    MBY_TRIG_ACTION_LEARNING_DONT_LEARN,
    MBY_TRIG_ACTION_LEARNING_FORCE_LEARN

} mbyTriggerActionLearning;

typedef enum mbyMaLookupEntryTypeEnum
{
    MBY_MA_LOOKUP_ENTRY_TYPE_NOTUSED      = 0,
    MBY_MA_LOOKUP_ENTRY_TYPE_PROVISIONAL  = 1,
    MBY_MA_LOOKUP_ENTRY_TYPE_DYNAMIC      = 2,
    MBY_MA_LOOKUP_ENTRY_TYPE_SECURE       = 3,
    MBY_MA_LOOKUP_ENTRY_TYPE_STATIC       = 4,
    MBY_MA_LOOKUP_ENTRY_TYPE_SECURESTATIC = 5

} mbyMaLookupEntryType;

// Structs:

typedef struct mbyArpTableStruct
{

    mbyArpEntryType         EntryType; // 1 = MAC entry, 0 - Glort Entry
    fm_macaddr              DMAC; // used for DMAC entries
    fm_uint16               DGLORT; // used for GLORT entries
    fm_byte                 MTU_Index; // used for GLORT entries
    fm_bool                 markRouted; // used for GLORT entries
    fm_bool                 IPv6Entry; // used for GLORT entries
    fm_uint16               EVID;
    fm_byte                 L3Domain;
    fm_uint16               L2Domain;
    fm_bool                 UpdateL3Domain;
    fm_bool                 UpdateL2Domain;
    fm_uint32               ModIdx;

} mbyArpTable;

typedef struct mbyTriggerResultsStruct
{
    fm_uint32                           action;
    mbyTriggerActionForwarding          forwardingAction;
    fm_uint16                           destGlort;
    fm_uint64                           destMask;
    fm_bool                             filterDestMask;
    mbyTriggerActionTrap                trapAction;
    fm_byte                             cpuCode;
    fm_byte                             trapCode;
    fm_bool                             logAction;
    mbyTriggerActionMirroring           mirroringAction0;
    mbyTriggerActionMirroring           mirroringAction1;
    fm_bool                             rxMirror;
    fm_byte                             mirrorProfileIndex0;
    fm_byte                             mirrorProfileIndex1;
    fm_bool                             mirror0ProfileV;
    fm_bool                             mirror1ProfileV;
    fm_byte                             mirror0ProfileIdx;
    fm_byte                             mirror1ProfileIdx;
    mbyTriggerActionTC                  TCAction;
    fm_byte                             TC;
    mbyTriggerActionVlan                vlanAction;
    fm_uint16                           vlan;
    mbyTriggerActionLearning            learningAction;
    fm_bool                             rateLimitAction;
    fm_byte                             rateLimitNum;
    fm_int                              metadataTrigNum[4];
    fm_byte                             metadataAction[4];
    fm_byte                             egressL2DomainAction;
    fm_byte                             egressL3DomainAction;
    fm_byte                             qcnValid0;
    fm_byte                             qcnValid1;
    fm_byte                             policerAction;
    fm_byte                             noModifyAction;

} mbyTriggerResults;

typedef struct mbyMaTableStruct
{
    fm_byte                 _RSVD5_;
    fm_byte                 OLD_PORT;
    fm_byte                 NEW_PORT;
    mbyMaLookupEntryType    ENTRY_TYPE;
    fm_byte                 _RSVD3_;
    fm_byte                 TRIG_ID;
    fm_uint16               S_GLORT;
    fm_uint16               D_GLORT;
    fm_byte                 _RSVD2_;
    fm_bool                 _RSVD1_;
    fm_uint16               L2_DOMAIN;
    fm_uint16               VID;
    fm_uint64               MAC_ADDRESS;

} mbyMaTable;

typedef struct mbyNextHopToMaskGenStruct
{
    fm_uint16               ARP_TABLE_INDEX;
    fm_bool                 ENCAP;
    fm_bool                 DECAP;
    fm_macaddr              L2_SMAC;        // Layer 2 source      MAC address
    fm_macaddr              L2_DMAC;        // Layer 2 destination MAC address
    fm_uint16               L2_IDOMAIN;
    fm_byte                 L3_IDOMAIN;    
    fm_uint16               L2_IVID1;
    fm_uint16               L2_EDOMAIN;      // egress L2 domain
    fm_byte                 L3_EDOMAIN;      // egress L3 domain
    fm_uint16               L2_EVID1;        // 12-bit egress VLAN ID
    fm_byte                 MTU_INDEX;
    fm_bool                 FLOOD_SET;
    fm_uint16               IDGLORT;
    fm_bool                 MARK_ROUTED;
    fm_uint32               MOD_IDX;

    // Added for MaskGen's benefit:
    fm_uint32               RX_PORT;                // receive port number
    fm_bool                 PARSER_WINDOW_V;        // parser window valid flag
    fm_bool                 PARSER_ERROR;           // header parse error flag
    fm_bool                 PARITY_ERROR;           // memory parity error flag
    fm_uint16               L2_ETYPE;               // 16-bit innermost Ethernet type
    mbySTPState             L2_IFID1_STATE;         // 2-bit spanning tree state for the ingress port
    fm_bool                 L2_IVLAN1_MEMBERSHIP;   // ingress port is part of the ingress VLAN flag
    fm_bool                 L2_IVLAN1_REFLECT;      // ingress VLAN reflection is enabled
    fm_uint32               L2_EVLAN1_MEMBERSHIP;   // 24-bit egress VLAN port membership vector
    fm_bool                 NO_LEARN;               // learning is diabled flag
    fm_bool                 GLORT_CAM_MISS;         // GLORT lookup resulted in a miss flag
    fm_bool                 GLORT_FORWARDED;        // glortforwarded due to FFU rule
    fm_uint32               GLORT_DMASK;            // 24-bit GLORT-based destination mask
    fm_bool                 TARGETED_DETERMINISTIC; // mode is set to targeted deterministic
    fm_bool                 CPU_TRAP;               // CPU trap
    fm_bool                 TRAP_ICMP;              // ICMP packet should be trapped    
    fm_bool                 TRAP_IGMP;              // IGMP packet should be trapped
    fm_bool                 TRAP_IP_OPTIONS;        // IP options present
    fm_uint32               PRE_RESOLVE_DMASK;      // destination mask before action resolution
    fm_uint32               ACTION;                 // resolved action
    fm_byte                 OPERATOR_ID;            // 4-bit operator ID
    fm_byte                 QOS_SWPRI;              // 4-bit switch priority
    mbyTriggerResults       TRIGGERS;               // trigger results
    fm_uint16               IP_MCAST_IDX;           // index into the MCAST_VLAN_TABLE
    fm_uint32               MIRROR0_PROFILE_IDX;    // mirror 0 profile index
    fm_bool                 MTU_VIOLATION;          // packet violates the MTU
    fm_bool                 DROP_TTL;               // packet should be dropped
    fm_bool                 IS_IPV4;                // packet is IPv4
    fm_bool                 IS_IPV6;                // packet is IPv6
    fm_bool                 SA_HIT;                 // source MAC address lookup hit
    mbyMaTable              SA_RESULT;              // source MAC address lookup result
    fm_byte                 SV_DROP;                // MAC security violation info
    fm_uint16               CSGLORT;                // 16-bit canonical source GLORT
    fm_bool                 FLOOD_FORWARDED;        // glort is flood-forwarded

} mbyNextHopToMaskGen;

#endif
