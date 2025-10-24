/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_NEXTHOP_H
#define MBY_NEXTHOP_H

// Includes:

#include <mby_top_map.h> // header file auto-generated from RDL

#include "mby_hsh2nxt.h"
#include "mby_nxt2msk.h"
#include "mby_nexthop_regs.h"

// Defines:

// Enums:

// Structs:

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
    fm_bool    flowlet_int_en;

} mbyNextHopSweepInput;

typedef struct mbyNextHopUsageInputStruct
{
    fm_uint16  route_neighbor_idx;
    fm_uint16  group_min_index;
    fm_uint32  rx_length;

} mbyNextHopUsageInput;

// Function prototype:

void NextHop
(
    mby_ppe_nexthop_map       const * const nexthop_map,
    mby_ppe_nexthop_map__addr const * const nexthop_w,
    mbyHashToNextHop          const * const in,
    mbyNextHopToMaskGen             * const out
);

#endif
