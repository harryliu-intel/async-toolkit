/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include <model_c_write.h> // write_field()

#include "mby_common.h"
#include "mby_nexthop.h"

static fm_uint16 findWinningWeight
(
    fm_byte   const nh_weights[MBY_NH_MAX_WCMP_GROUP_SIZE],
    fm_uint16 const group_size,
    fm_byte   const hash
)
{
    fm_uint16 index = 0;
    fm_byte temp_val = 0xFF;

    for (fm_uint16 i = 0 ; i < group_size ; i++)
    {
        if (nh_weights[i] >= hash && nh_weights[i] < temp_val)
        {
            index    = i;
            temp_val = nh_weights[i];
        }
    }

    return index;
}

static void setForwardedType
(
    mby_ppe_nexthop_map const * const nexthop_map,
    fm_uint32                         rx_port,
    fm_macaddr                        l2_dmac,
    fm_uint16                         ivid1,
    fm_bool                           flood_set,
    fm_uint16                         l2_edomain,
    fm_bool                           normal_fwd,
    fm_uint16                 * const idglort,
    fm_bool                   * const glort_forwarded,
    fm_bool                   * const flood_forwarded,
    fm_bool                   * const da_hit,
    fm_bool                   * const l2_ivlan1_membership,
    fm_bool                   * const l2_ivlan1_reflect,
    fm_bool                   * const trap_igmp
)
{
    /* Perform ingress VLAN lookup. */
    mbyIngressVidTable ividTable = getIvidTableEntry(nexthop_map, ivid1);

    *l2_ivlan1_reflect    = ividTable.REFLECT;
    *trap_igmp           &= ividTable.TRAP_IGMP;
    *l2_ivlan1_membership = FM_GET_UNNAMED_FIELD(ividTable.MEMBERSHIP, rx_port, 1);

    *glort_forwarded = 0;
    *flood_forwarded = 0;

    if (*idglort)
    {
        if (flood_set)
            *flood_forwarded = 1;
        else if (normal_fwd)
            *da_hit = 1;
        else
            *glort_forwarded = 1;
    }
    else
    {
        flood_glort_table_r const * const flood_glort_table = &(nexthop_map->FLOOD_GLORT_TABLE[l2_edomain]);
        if (isBroadcastMacAddress(l2_dmac))
            *idglort = flood_glort_table->BROADCAST_GLORT;
        else if (isMulticastMacAddress(l2_dmac))
        {
            *idglort = flood_glort_table->FLOOD_MULTICAST_GLORT;
            *flood_forwarded = 1;
        }
        else
        {
            *idglort = flood_glort_table->FLOOD_UNICAST_GLORT;
            *flood_forwarded = 1;
        }
    }
}

static fm_bool checkMtu
(
    mby_ppe_nexthop_map const * const nexthop_map,
    fm_uint32                   const rx_length,
    fm_byte                     const mtu_idx
)
{
    fm_byte mtu_table_idx = mtu_idx;

    mtu_table_r const * const mtu_table = &(nexthop_map->MTU_TABLE[mtu_table_idx]);

    fm_uint16 mtu = mtu_table->MTU;

    /**
     * If the actual packet length (as contained in the IP header) is greater than the selected MTU size,
     * the packet is declared oversized and the parameter - FWD_SYS_CFG_1.TRAP_MTU_VIOLATIONS
     * defines the disposition of the frame either:
     * - trap to CPU or
     * - silently discard
     *
     * See [MTU Checking & Disposition](https://securewiki.ith.intel.com/display/25T/RX-PPE+Next-Hop+Lookup#RX-PPENext-HopLookup-MTUChecking&Disposition) for details.
     */

    if (rx_length > mtu)
        return TRUE;
    else
        return FALSE;
}

static void resetAgeCounter
(
    mby_ppe_nexthop_map__addr const * const nexthop_w,
    fm_uint16                         const route_bin_idx,
    fm_byte                           const age_counter
)
{
    nexthop_routes_table_r__addr const * const nh_routes_w = &(nexthop_w->NH_ROUTES[route_bin_idx]);

    write_field(nh_routes_w->AGE_COUNTER, age_counter);
}

static void processNeighborEntry
(
    mby_ppe_nexthop_map const * const nexthop_map,
    fm_uint16                   const neighbor_idx,
    fm_macaddr                  const dmac_ipv6,
    fm_uint32                   const rx_length,
    mbyNextHopApplyOutput     * const out
)
{
    mbyNextHopNeighbor nh_neighbor = getNextHopNeighborEntry(nexthop_map, neighbor_idx);

    fm_bool ip_routing_type = (nh_neighbor.entry_type == MBY_NH_ENTRY_TYPE_IP_ROUTING);

    if (ip_routing_type)
    {
        out->l2_dmac = (nh_neighbor.ipv6_entry) ? dmac_ipv6 : nh_neighbor.dmac;
        out->route   = TRUE;
    }
    else
    {
        out->route       = nh_neighbor.mark_routed;
        out->mark_routed = nh_neighbor.mark_routed;
    }

    out->dglort = nh_neighbor.dglort;

    if (nh_neighbor.update_l2domain)
        out->l2_edomain = nh_neighbor.l2domain;

    if (nh_neighbor.update_l3domain)
        out->l3_edomain = nh_neighbor.l3domain;

    out->l2_evid = nh_neighbor.evid;

    out->mtu_violation = checkMtu(nexthop_map, rx_length, nh_neighbor.mtu_idx);
}

static void performFlowletSoftwarePolicy
(
    mby_ppe_nexthop_map       const * const nexthop_map,
    mby_ppe_nexthop_map__addr const * const nexthop_w,
    fm_uint16                         const route_bin_idx
)
{
    nexthop_status_r__addr const * const nh_status = &(nexthop_w->NH_STATUS);
    write_field(nh_status->FLOWLET, route_bin_idx);

    nexthop_config_r__addr const * const nh_config = &(nexthop_w->NH_CONFIG);
    write_field(nh_config->FLOWLET_INT_EN, FALSE);
}

static void performFlowletLetFlowPolicy
(
    mby_ppe_nexthop_map       const * const nexthop_map,
    mby_ppe_nexthop_map__addr const * const nexthop_w,
    fm_uint16                         const route_bin_idx,
    fm_uint16                         const group_base_idx,
    fm_uint16                         const n_group_size
)
{
    nexthop_routes_table_r       const * const nh_route = &(nexthop_map->NH_ROUTES[route_bin_idx]);
    nexthop_routes_table_r__addr const * const nh_route_w = &(nexthop_w->NH_ROUTES[route_bin_idx]);

    fm_uint16 neighbor_idx = nh_route->NEIGHBOR_IDX;
    neighbor_idx = ( neighbor_idx - group_base_idx + 1 ) % n_group_size + group_base_idx;
    write_field(nh_route_w->NEIGHBOR_IDX, neighbor_idx);
}

static void performFlowletPathLoadingPolicy
(
    mby_ppe_nexthop_map       const * const nexthop_map,
    mby_ppe_nexthop_map__addr const * const nexthop_w,
    fm_uint16                         const route_bin_idx,
    fm_uint16                         const group_min_idx
)
{
    nexthop_routes_table_r__addr const * const nh_route_w   = &(nexthop_w->NH_ROUTES[route_bin_idx]);

    fm_uint16 bin = group_min_idx / 4;
    fm_byte   x   = group_min_idx % 4;

    nexthop_group_min_r    const * const nh_group_min = &(nexthop_map->NH_GROUP_MIN[bin]);

    fm_uint16 group_mins[4] = {nh_group_min->MIN_0, nh_group_min->MIN_1, nh_group_min->MIN_2, nh_group_min->MIN_3};

    fm_uint16 neighbor_idx = group_mins[x];
    write_field(nh_route_w->NEIGHBOR_IDX, neighbor_idx);
}

static void applyNextHop
(
    mby_ppe_nexthop_map       const * const nexthop_map,
    mby_ppe_nexthop_map__addr const * const nexthop_w,
    mbyNextHopApplyInput      const * const in,
    mbyNextHopApplyOutput           * const out
)
{
    fm_uint16  const ecmp_hash  = in->ecmp_hash;
    fm_uint32  const fwd        = in->fwd;
    fm_byte    const l2_idomain = in->l2_idomain;
    fm_byte    const l3_idomain = in->l3_idomain;
    fm_uint16  const l2_ivid1   = in->l2_ivid1;
    fm_macaddr const dmac_ipv6  = in->dmac_from_ipv6;

    mbyNextHopRouteType route_type = FM_GET_BIT  (fwd, MBY_FWD_ARP, ROUTE_TYPE );
    fm_uint16           route_idx  = FM_GET_FIELD(fwd, MBY_FWD_ARP, ROUTE_INDEX);

    fm_uint16 neighbor_idx;

    /**
     *  There are two Route Types used:
     * - [Single Path Routing](https://securewiki.ith.intel.com/display/25T/RX-PPE+Next-Hop+Lookup#RX-PPENext-HopLookup-SinglePathRouting)
     *   - [Single Path Routed Packets](https://securewiki.ith.intel.com/display/25T/RX-PPE+Next-Hop+Lookup#RX-PPENext-HopLookup-SinglePathRoutedPackets)
     * - [Group Path Routing](https://securewiki.ith.intel.com/display/25T/RX-PPE+Next-Hop+Lookup#RX-PPENext-HopLookup-GroupPathRouting)\n
     *   There are two Group Types used:
     *   - [ECMP Group Routed Packets](https://securewiki.ith.intel.com/display/25T/RX-PPE+Next-Hop+Lookup#RX-PPENext-HopLookup-ECMPGroupRoutedPackets)
     *   - (_TBD_) [WCMP Group Routed Packets](https://securewiki.ith.intel.com/display/25T/RX-PPE+Next-Hop+Lookup#RX-PPENext-HopLookup-WCMPGroupRoutedPackets)
     */
    if (route_type == MBY_NH_ROUTE_TYPE_SINGLE)
    {
        // The incoming Route Index is used to index directly into the neighbor table.
        neighbor_idx = route_idx;
        processNeighborEntry(nexthop_map, neighbor_idx, dmac_ipv6, in->rx_length, out);
    }
    else if (route_type == MBY_NH_ROUTE_TYPE_GROUP)
    {
        // The incoming Route Index is used as an offset into the group table to retrieve the Group Type.
        fm_uint16 group_idx = route_idx;
        mbyNextHopGroup nh_group = getNextHopGroupEntry(nexthop_map, group_idx);
        mbyNextHopGroupType group_type = nh_group.group_type;

        fm_uint16 group_base_idx = nh_group.base_index;
        if (group_type == MBY_NH_GROUP_TYPE_ECMP || group_type == MBY_NH_GROUP_TYPE_ECMP_FLOWLET)
        {
            fm_uint16 bin_count  = nh_group.r_group_size;
            fm_uint16 bin_offset = (ecmp_hash * bin_count) >> 12;

            fm_uint16 route_bin_idx  = group_base_idx + bin_offset;
            mbyNextHopRoute nh_route = getNextHopRouteEntry(nexthop_map, route_bin_idx);

            nh_route.age_counter = nh_group.flowlet_age_reset;
            resetAgeCounter(nexthop_w, route_bin_idx, nh_route.age_counter);

            neighbor_idx = nh_route.neighbor_index;
            processNeighborEntry(nexthop_map, neighbor_idx, dmac_ipv6, in->rx_length, out);

            if (group_type == MBY_NH_GROUP_TYPE_ECMP_FLOWLET)
            {
                out->flowlet_enabled_packet = TRUE;
                out->route_neighbor_idx = nh_route.neighbor_index;
                out->group_min_index = nh_group.group_min_index;
            }
            else
            {
                out->flowlet_enabled_packet = FALSE;
            }
        }
        else if (group_type == MBY_NH_GROUP_TYPE_WCMP)
        {
            fm_byte hash = ecmp_hash & 0xFF;

            fm_byte weights[MBY_NH_MAX_WCMP_GROUP_SIZE] = { 0 };

            getNexthopWeights(nexthop_map, nh_group.n_group_size, nh_group.weight_row, nh_group.weight_row_offset, weights);

            fm_uint16 bin_offset = findWinningWeight(weights, nh_group.n_group_size, hash);

            neighbor_idx  = group_base_idx + bin_offset;
            processNeighborEntry(nexthop_map, neighbor_idx, dmac_ipv6, in->rx_length, out);

            out->flowlet_enabled_packet = FALSE;
        }
    }

    /// The NH_USED bit corresponding to the Neighbor Table Index is set.
    setNextHopUsedEntry(nexthop_map, nexthop_w, neighbor_idx);
}

static void sweepNextHop
(
    mby_ppe_nexthop_map       const * const nexthop_map,
    mby_ppe_nexthop_map__addr const * const nexthop_w,
    mbyNextHopSweepInput      const * const in
)
{
    // HOW should walking the NH_ROUTES table be done?
    // What about input to NextHop_Sweep stage: in->route_neighbor_idx ??
    // fm_uint16 route_bin_idx = in->route_neighbor_idx;

    /// The sweeper walks the route table looking for a bin with a non-zero Age Counter.
    /// When one is encountered, the following steps are taken:
    for (fm_uint16 route_bin_idx = 0 ; route_bin_idx < MBY_NH_NEIGHBORS_ENTRIES ; route_bin_idx++)
    {
        mbyNextHopRoute nh_route = getNextHopRouteEntry(nexthop_map, route_bin_idx);
        if (nh_route.age_counter != 0)
        {
            /// 1. Group Table Index is retrieved from the bin and used to identify the corresponding group table entry.
            fm_uint16 group_idx = nh_route.group_index;
            mbyNextHopGroup nh_group = getNextHopGroupEntry(nexthop_map, group_idx);
            /// 2. Group Type is retrieved from the group table entry, and if 2, processing continues with the next step, otherwise done with this bin.
            mbyNextHopGroupType group_type = nh_group.group_type;
            if (group_type == MBY_NH_GROUP_TYPE_ECMP_FLOWLET)
            {
                /// 3. The Age Counter in the bin is decremented, and if it becomes zero, processing continues with the next step, otherwise done with this bin.
                fm_byte age_counter = nh_route.age_counter - 1;
                write_field(nexthop_w->NH_ROUTES[route_bin_idx].AGE_COUNTER, age_counter);
                if (age_counter == 0)
                {
                    /// 4. The Flowlet Policy is loaded from the group table and the specified policy performed
                    mbyNextHopFlowletPolicy flowlet_policy = nh_group.flowlet_policy;
                    switch (flowlet_policy)
                    {
                    case MBY_NH_FLOWLET_POLICY_SOFTWARE:
                        performFlowletSoftwarePolicy(nexthop_map, nexthop_w, route_bin_idx);
                        break;
                    case MBY_NH_FLOWLET_POLICY_LETFLOW:
                        performFlowletLetFlowPolicy(nexthop_map, nexthop_w, route_bin_idx, nh_group.base_index, nh_group.n_group_size);
                        break;
                    case MBY_NH_FLOWLET_POLICY_PATH_LOADING:
                        performFlowletPathLoadingPolicy(nexthop_map, nexthop_w, route_bin_idx, nh_group.group_min_index);
                        break;
                    case MBY_NH_FLOWLET_POLICY_SDN_MANAGED:
                        // This policy is not implemented
                        // performFlowletSDNDirectedPolicy();
                        break;
                    case MBY_NH_FLOWLET_POLICY_LOCAL_CONGESTION:
                        // This policy is not implemented
                        // performFlowletLocalCongestionPolicy();
                        break;
                    case MBY_NH_FLOWLET_POLICY_CONGESTION_NOTIFICATION:
                        // This policy is not implemented
                        // performFlowletCongestionNotificationPolicy();
                        break;
                    case MBY_NH_FLOWLET_POLICY_POWER_AWARE:
                        // This policy is not implemented
                        // performFlowletPowerAwarePolicy();
                        break;
                    case MBY_NH_FLOWLET_POLICY_RESERVED:
                    default:
                        break;
                    }
                    /// 5. Flowlet Age Reset value is retrieved from the group table and written to the Age Counter in the bin
                    fm_byte age_reset = nh_group.flowlet_age_reset;
                    write_field(nexthop_w->NH_ROUTES[route_bin_idx].AGE_COUNTER, age_reset);
                }
            }
        }
        else
            continue;
    }
}
static void trackNextHopUsage
(
    mby_ppe_nexthop_map       const * const nexthop_map,
    mby_ppe_nexthop_map__addr const * const nexthop_w,
    mbyNextHopUsageInput      const * const in
)
{
    fm_uint16 neighbor_idx  = in->route_neighbor_idx;
    fm_uint16 group_min_idx = in->group_min_index;
    fm_uint16 bin = group_min_idx / 4;
    fm_byte   x   = group_min_idx % 4;

    nexthop_group_min_r       const * const nh_group_min = &(nexthop_map->NH_GROUP_MIN[bin]);
    nexthop_group_min_r__addr const * const nh_group_min_w = &(nexthop_w->NH_GROUP_MIN[bin]);
    nexthop_path_ctrs_r       const * const nh_path_ctrs = &(nexthop_map->NH_PATH_CTRS[neighbor_idx]);
    nexthop_path_ctrs_r__addr const * const nh_path_ctrs_w = &(nexthop_w->NH_PATH_CTRS[neighbor_idx]);

    fm_uint16 group_mins[4] = {nh_group_min->MIN_0, nh_group_min->MIN_1, nh_group_min->MIN_2, nh_group_min->MIN_3};
    /// For each packet, the following actions are taken by the NextHop_Usage stage:

    /// 1. NH_PATH_CTRS[Neighbor Table Index] is incremented by the number of bytes in the packet
    fm_uint64 bytes = nh_path_ctrs->BYTES + in->rx_length;
    write_field(nh_path_ctrs_w->BYTES, bytes);
    /// 2. The NH_GROUP_MIN[Group Min Index] entry is retrieved and if it is not equal to Neighbor Table Index, processing continues with the next step, otherwise tail processing ends
    if (group_mins[x] != neighbor_idx)
    {
        /// 3. If:
        /// NH_PATH_CTRS[Neighbor Table Index] < NH_PATH_CTRS[NH_GROUP_MIN[Group Min Index]]
        /// then processing continues with the next step, otherwise tail processing ends
        nexthop_path_ctrs_r const * const nh_path_ctrs_group_min = &(nexthop_map->NH_PATH_CTRS[group_mins[x]]);
        if (nh_path_ctrs->BYTES < nh_path_ctrs_group_min->BYTES)
        {
            /// 4. Neighbor Table Index is stored to NH_GROUP_MIN[Group Min Index]
            switch (x)
            {
            case 0:
                write_field(nh_group_min_w->MIN_0, neighbor_idx);
                break;
            case 1:
                write_field(nh_group_min_w->MIN_1, neighbor_idx);
                break;
            case 2:
                write_field(nh_group_min_w->MIN_2, neighbor_idx);
                break;
            case 3:
                write_field(nh_group_min_w->MIN_3, neighbor_idx);
                break;
            }
        }
    }
}

/**
 * NextHop stage implementation.
 * See: [NextHop FS](https://securewiki.ith.intel.com/display/25T/RX-PPE+Next-Hop+Lookup)
 *
 * The NextHop block is implemented in three relatively independent stages:
 * - NextHop_Apply for packet lookup processing on the head of the packet
 * - NextHop_Sweep for flowlet detection and rebalancing
 * - NextHop_Usage for path state tracking
 *
 * @param[in]   nexthop_map  Pointer to NextHop register map  (read only).
 * @param[in]   nexthop_w    Pointer to NextHop register map.
 * @param[in]   in           Pointer to input structure       (read only).
 * @param[out]  out          Pointer to output structure.
 */
void NextHop
(
    mby_ppe_nexthop_map       const * const nexthop_map,
    mby_ppe_nexthop_map__addr const * const nexthop_w,
    mbyHashToNextHop          const * const in,
    mbyNextHopToMaskGen             * const out
)
{
    // Read inputs:
    mbyClassifierFlags const cgrp_flags     = in->CGRP_FLAGS;
    fm_uint16          const ecmp_hash      = in->ECMP_HASH;
    fm_uint32          const fwd            = in->FWD;
    fm_byte            const l2_idomain     = in->L2_IDOMAIN;
    fm_byte            const l3_idomain     = in->L3_IDOMAIN;
    fm_uint16          const l2_ivid1       = in->L2_IVID1;
    fm_macaddr         const dmac_from_ipv6 = in->DMAC_FROM_IPV6;
    // Prepare default outputs:
    fm_uint16                idglort        = 0;
    fm_macaddr               l2_dmac        = in->L2_DMAC;
    fm_byte                  l2_edomain     = l2_idomain;
    fm_byte                  l3_edomain     = l3_idomain;
    fm_uint16                l2_evid1       = l2_ivid1;
    fm_bool                  routed         = FALSE;
    // Verify if these are still necessary? <-- REVISIT!!!
    fm_byte    const * const arp_hash       = in->ARP_HASH; // [16]
    fm_bool                  decap          = in->DECAP;
    fm_bool                  encap          = in->ENCAP;
    fm_macaddr         const l2_smac        = in->L2_SMAC;
    fm_bool            const learn_mode     = in->LEARN_MODE;
    fm_uint32          const rx_port        = in->RX_PORT;
    fm_bool                  trap_igmp      = in->TRAP_IGMP;


    fm_bool   normal_fwd = !FM_GET_BIT   (fwd, MBY_FWD_GLORT, FORWARDED_TYPE);
    fm_bool   flood_set  =  FM_GET_BIT   (fwd, MBY_FWD_GLORT, IS_FLOODSET);
    fm_uint16 dglort     =  FM_GET_FIELD (fwd, MBY_FWD_GLORT, DGLORT);

    // Get NH_CONFIG:
    mbyNextHopConfig nh_config = getNextHopConfig(nexthop_map);
    fm_bool flowlet_enable = nh_config.flowlet_enable;

    mbyClassifierFwdSubtype fwd_subtype = FM_GET_BIT (fwd, MBY_FWD, SUBTYPE);
    fm_bool fwd_glort = fwd_subtype == MBY_FWD_SUBTYPE_FWD_GLORT;
    fm_bool route_arp = fwd_subtype == MBY_FWD_SUBTYPE_ROUTE_ARP;
    fm_bool no_route  = cgrp_flags.no_route;

    if (fwd_glort)
    {
        idglort = dglort;
        routed  = FALSE;
    }

    mbyNextHopApplyInput  apply_in;
    mbyNextHopApplyOutput apply_out = { 0 };

    /**
     * NextHop lookup is only performed if in addition to the FWD action indicating a ROUTE_ARP subtype,
     * the NO_ROUTE action flag is not set by the Classifier.
     */
    if (route_arp && !no_route){


        apply_in.fwd            = fwd;
        apply_in.ecmp_hash      = ecmp_hash;
        apply_in.dmac_from_ipv6 = dmac_from_ipv6;
        apply_in.l2_idomain     = l2_idomain;
        apply_in.l2_ivid1       = l2_ivid1;
        apply_in.l3_idomain     = l3_idomain;
        apply_in.flowlet_enable = flowlet_enable;
        apply_in.rx_length      = in->RX_LENGTH;

        apply_out.l2_dmac = l2_dmac;
        apply_out.dglort  = dglort;

        applyNextHop(nexthop_map, nexthop_w, &apply_in, &apply_out);

        l2_evid1    = apply_out.l2_evid;
        idglort     = apply_out.dglort;
        l2_dmac     = apply_out.l2_dmac;
        l2_edomain  = apply_out.l2_edomain;
        l2_evid1    = apply_out.l2_evid;
        l3_edomain  = apply_out.l3_edomain;
        routed      = apply_out.route;
    }

    mbyNextHopSweepInput sweep_in;

    /**
     * NextHop_Sweep stage will be scheduled only during packet idle periods to avoid contention.
     */
    // <-- How it should be handled in the FM? REVISIT!!!
    // if NextHop_Sweep stage is enabled:
    if (flowlet_enable)
    {
        sweep_in.flowlet_enabled_packet = apply_out.flowlet_enabled_packet;
        sweep_in.route_neighbor_idx     = apply_out.route_neighbor_idx;
        sweep_in.group_min_index        = apply_out.group_min_index;
        sweep_in.flowlet_int_en         = nh_config.flowlet_int_en;

        sweepNextHop(nexthop_map, nexthop_w, &sweep_in);
    }

    /**
     * NextHop_Usage task need not be implemented, if the Path Loading Policy is deemed not useful.
     * See [here](https://securewiki.ith.intel.com/display/25T/RX-PPE+Next-Hop+Lookup#RX-PPENext-HopLookup-NextHop_Usage_warning).
     */

    mbyNextHopUsageInput usage_in;

    if (apply_out.flowlet_enabled_packet)
    {
        usage_in.group_min_index    = apply_out.group_min_index;
        usage_in.route_neighbor_idx = apply_out.route_neighbor_idx;
        usage_in.rx_length          = in->RX_LENGTH;

        trackNextHopUsage(nexthop_map, nexthop_w, &usage_in);
    }

    fm_bool     glort_forwarded      = FALSE;
    fm_bool     flood_forwarded      = FALSE;
    fm_bool     da_hit               = FALSE;
    fm_bool     l2_ivlan1_membership = FALSE;
    fm_bool     l2_ivlan1_reflect    = FALSE;

    setForwardedType
    (
        nexthop_map,
        rx_port,
        l2_dmac,
        l2_ivid1,
        flood_set,
        l2_edomain,
        normal_fwd,
        &idglort,
        &glort_forwarded,
        &flood_forwarded,
        &da_hit,
        &l2_ivlan1_membership,
        &l2_ivlan1_reflect,
        &trap_igmp
    );

    // Write outputs:
    for(fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
        out->GLORT_DMASK[i] = in->GLORT_DMASK; //temporary, because nexthop is not adapted to 258bits DMASK

    // out->ARP_TABLE_INDEX      = neighbor_idx;
    out->DECAP                = decap;
    out->ENCAP                = encap;
    out->FLOOD_FORWARDED      = flood_forwarded;
    out->FLOOD_SET            = flood_set;
    out->GLORT_FORWARDED      = glort_forwarded;
    out->IDGLORT              = idglort;
    out->L2_DMAC              = l2_dmac;
    out->L2_EDOMAIN           = l2_edomain;
    out->L2_EVID1             = l2_evid1;
    out->L2_IDOMAIN           = l2_idomain;
    out->L2_IVID1             = l2_ivid1;
    out->L2_IVLAN1_MEMBERSHIP = l2_ivlan1_membership;
    out->L2_IVLAN1_REFLECT    = l2_ivlan1_reflect;
    out->L2_SMAC              = l2_smac;
    out->L3_EDOMAIN           = l3_edomain;
    out->L3_IDOMAIN           = l3_idomain;
    out->ROUTED               = routed;
    // out->MOD_IDX              = in->mod_index;
    // out->MTU_INDEX            = mtu_index;
    out->RX_PORT              = rx_port;
    out->TRAP_IGMP            = trap_igmp;

    // Pass thru:
    out->ACTION               = in->ACTION;
    out->CGRP_TRIG            = in->CGRP_TRIG;
    out->CONTENT_ADDR         = in->CONTENT_ADDR;
    out->CPU_TRAP             = in->CPU_TRAP;
    out->CSGLORT              = in->CSGLORT;
    out->DROP_TTL             = in->DROP_TTL;
    out->CGRP_FLAGS           = in->CGRP_FLAGS;
    out->HASH_ROT_A           = in->HASH_ROT_A;
    out->HASH_ROT_B           = in->HASH_ROT_B;
    out->IP_MCAST_IDX         = in->IP_MCAST_IDX;
    out->IS_IPV4              = in->IS_IPV4;
    out->IS_IPV6              = in->IS_IPV6;
    out->L2_ETYPE             = in->L2_ETYPE;
    out->MIRROR0_PROFILE_IDX  = in->MIRROR0_PROFILE_IDX;
    out->MOD_PROF_IDX         = in->MOD_PROF_IDX;
    out->MTU_VIOLATION        = apply_out.mtu_violation;
    out->LEARN_NOTIFY         = in->LEARN_NOTIFY;
    out->NAD                  = in->NAD;
    out->PARITY_ERROR         = in->PARITY_ERROR;
    out->PARSER_ERROR         = in->PARSER_ERROR;
    out->PARSER_INFO          = in->PARSER_INFO;
    out->PARSER_WINDOW_V      = in->PARSER_WINDOW_V;
    out->PA_DROP              = in->PA_DROP;
    out->PA_HDR_PTRS          = in->PA_HDR_PTRS;
    out->PA_L3LEN_ERR         = in->PA_L3LEN_ERR;
    out->PRE_RESOLVE_DMASK    = in->PRE_RESOLVE_DMASK;
    out->QOS_TC               = in->QOS_TC;
    out->RX_MIRROR            = in->RX_MIRROR;
    out->SA_HIT               = in->SA_HIT;
    out->SA_RESULT            = in->SA_RESULT;
    out->SEG_META_ERR         = in->SEG_META_ERR;
    out->SV_DROP              = in->SV_DROP;
    out->TRAP_ICMP            = in->TRAP_ICMP;
    out->TRAP_IP_OPTIONS      = in->TRAP_IP_OPTIONS;
    out->TRIGGERS             = in->TRIGGERS;
    out->RX_LENGTH            = in->RX_LENGTH;
}
