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

typedef struct mbyNextHopToMaskGenStruct
{
    fm_uint16               ARP_TABLE_INDEX;
    fm_bool                 ENCAP;
    fm_bool                 DECAP;
    fm_macaddr              L2_DMAC;
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

} mbyNextHopToMaskGen;

#endif
