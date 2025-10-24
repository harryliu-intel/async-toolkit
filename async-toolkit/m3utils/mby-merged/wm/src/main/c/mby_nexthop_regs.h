// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_NEXTHOP_REGS_H
#define MBY_NEXTHOP_REGS_H

// Includes:

#include <mby_top_map.h> // header file auto-generated from RDL

#include "mby_bitfield.h"

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

typedef enum mbyClassifierFwdSubtypeEnum
{
    MBY_FWD_SUBTYPE_FWD_GLORT = 0,
    MBY_FWD_SUBTYPE_ROUTE_ARP

} mbyClassifierFwdSubtype;

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

typedef enum mbyNextHopGroupSizeTypeEnum
{
    MBY_NH_GROUP_SIZE_TYPE_LITERAL = 0,
    MBY_NH_GROUP_SIZE_TYPE_POWER_OF_2

} mbyNextHopGroupSizeType;

typedef enum mbyNextHopGroupTypeEnum
{
    MBY_NH_GROUP_TYPE_ECMP         = 0,
    MBY_NH_GROUP_TYPE_WCMP         = 1,
    MBY_NH_GROUP_TYPE_ECMP_FLOWLET = 2,
    MBY_NH_GROUP_TYPE_RESERVED     = 3

} mbyNextHopGroupType;

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

typedef struct mbyEgressVidTableStruct
{
  fm_byte      TRIG_ID;
  fm_uint32    MEMBERSHIP;

} mbyEgressVidTable;

typedef struct mbyIngressVidTableStruct
{
  fm_bool      TRAP_IGMP;
  fm_bool      REFLECT;
  fm_uint32    MEMBERSHIP;

} mbyIngressVidTable;

typedef struct mbyNextHopConfigStruct
{
    fm_byte sweeper_rate;
    fm_bool flowlet_int_en;
    fm_bool flowlet_enable; // if FALSE => Nexthop_Sweep && processPacketTail are disabled

} mbyNextHopConfig;

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
    fm_bool                     ipv6_entry;
    fm_byte                     mtu_idx;
    fm_macaddr                  dmac;     // To be moved to NH_EGR_TABLE? <-- REVISIT!!!
    fm_uint16                   evid;

} mbyNextHopNeighbor;

typedef struct mbyNextHopRouteStruct
{
    fm_byte   age_counter;
    fm_uint16 group_index;
    fm_uint16 neighbor_index;

} mbyNextHopRoute;

// Functions:

/* Helpers: */

fm_uint16 getNextHopGroupSize
(
    mbyNextHopGroupSizeType const group_size_type,
    fm_byte                 const x_group_size
);

fm_uint16 setNextHopGroupSize
(
    mbyNextHopGroupSizeType const group_size_type,
    fm_uint16               const group_size
);

/* Getters: */

mbyIngressVidTable getIvidTableEntry
(
    mby_ppe_nexthop_map const * const nexthop_map,
    fm_uint16                         vid
);

mbyNextHopConfig getNextHopConfig
(
    mby_ppe_nexthop_map const * const nexthop_map
);

mbyNextHopGroup getNextHopGroupEntry
(
    mby_ppe_nexthop_map const * const nexthop_map,
    fm_uint16                   const group_idx
);

mbyNextHopNeighbor getNextHopNeighborEntry
(
    mby_ppe_nexthop_map const * const nexthop_map,
    fm_uint16                   const neighbor_idx
);

mbyNextHopRoute getNextHopRouteEntry
(
    mby_ppe_nexthop_map const * const nexthop_map,
    fm_uint16                   const route_bin_idx
);

void getNexthopWeights
(
    mby_ppe_nexthop_map const * const nexthop_map,
    fm_uint16                   const group_size,
    fm_uint16                   const weight_row,
    fm_byte                     const weight_row_offset,
    fm_byte                           weights[MBY_NH_MAX_WCMP_GROUP_SIZE]
);

/* Setters: */

void setNextHopGroupEntry
(
    mby_ppe_nexthop_map__addr const * const nexthop_w,
    fm_uint16                         const group_idx,
    mbyNextHopGroup           const * const nh_group
);

void setNextHopRouteEntry
(
    mby_ppe_nexthop_map__addr const * const nexthop_w,
    fm_uint16                         const route_idx,
    mbyNextHopRoute           const * const nh_route
);

void setNextHopNeighborEntry
(
    mby_ppe_nexthop_map__addr const * const nexthop_w,
    fm_uint16                         const neighbor_idx,
    mbyNextHopNeighbor        const * const nh_neigbor
);

void setNextHopStatus
(
    mby_ppe_nexthop_map__addr const * const nexthop_w,
    fm_uint16                         const entry_id
);

void setNextHopUsedEntry
(
    mby_ppe_nexthop_map       const * const nexthop_map,
    mby_ppe_nexthop_map__addr const * const nexthop_w,
    fm_uint32                         const neighbor_idx
);

#endif /* MBY_NEXTHOP_REGS_H */