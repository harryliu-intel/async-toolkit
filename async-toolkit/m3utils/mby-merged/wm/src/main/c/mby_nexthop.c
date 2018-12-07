// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_common.h"
#include "mby_classifier.h"
#include "mby_hash.h"
#include "mby_nexthop.h"
#include "mby_maskgen.h"
#include <model_c_write.h> // write_field()

static fm_uint16 getNextHopGroupSize
(
    mbyNextHopGroupSizeType const group_size_type,
    fm_byte                 const x_group_size
)
{
    fm_uint16 group_size = 0;

    if (group_size_type == MBY_NH_GROUP_SIZE_TYPE_LITERAL)
    {
        group_size = x_group_size;
        if (x_group_size == 0)
            group_size == 64;
    }
    else if (group_size_type == MBY_NH_GROUP_SIZE_TYPE_POWER_OF_2)
        group_size = 1 << x_group_size;

    return group_size;
}

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

static void getNexthopWeights
(
    mby_ppe_nexthop_map const * const nexthop_map,
    fm_uint16                   const group_size,
    fm_uint16                   const weight_row,
    fm_byte                     const weight_row_offset,
    fm_byte                           weights[MBY_NH_MAX_WCMP_GROUP_SIZE]
)
{
    nexthop_weights_table_rf const * const nh_table = &(nexthop_map->NH_WEIGHTS[weight_row]);

    fm_byte bank   = weight_row_offset / 8;
    fm_byte offset = weight_row_offset % 8;
    fm_int16 remaining_bytes = group_size;
    fm_byte copied_weights = 0;

    while (remaining_bytes > 0)
    {
        // temporary array of weights
        fm_byte temp[8] = { 0 };

        temp[0] = nh_table[bank]->WEIGHT_0;
        temp[1] = nh_table[bank]->WEIGHT_1;
        temp[2] = nh_table[bank]->WEIGHT_2;
        temp[3] = nh_table[bank]->WEIGHT_3;
        temp[4] = nh_table[bank]->WEIGHT_4;
        temp[5] = nh_table[bank]->WEIGHT_5;
        temp[6] = nh_table[bank]->WEIGHT_6;
        temp[7] = nh_table[bank]->WEIGHT_7;

        for( ; offset < 8 ; offset++)
        {
            // copy valid entries to output struct
            weights[copied_weights++] = temp[offset];

            remaining_bytes--;
            if (remaining_bytes == 0)
                break;
        }

        offset = 0;
        bank++;
    }
}

static mbyNextHopRoute getNexthopRouteEntry
(
    mby_ppe_nexthop_map const * const nexthop_map,
    fm_uint16                   const route_bin_idx
)
{
    mbyNextHopRoute nh_route = { 0 };

    nexthop_routes_table_r const * const nh_table = &(nexthop_map->NH_ROUTES[route_bin_idx]);

    nh_route.age_counter    = nh_table->AGE_COUNTER;
    nh_route.group_index    = nh_table->GROUP_IDX;
    nh_route.neighbor_index = nh_table->NEIGHBOR_IDX;

    return nh_route;
}

static mbyNextHopGroup getNexthopGroupEntry
(
    mby_ppe_nexthop_map const * const nexthop_map,
    fm_uint16                   const route_idx
)
{
    mbyNextHopGroup nh_group = { 0 };

    nexthop_groups_table_0_r const * const nh_table_0 = &(nexthop_map->NH_GROUPS_0[route_idx]);
    nexthop_groups_table_1_r const * const nh_table_1 = &(nexthop_map->NH_GROUPS_1[route_idx]);

    nh_group.group_type        = nh_table_0->GROUP_TYPE;
    nh_group.base_index        = nh_table_0->BASE_INDEX;
    nh_group.n_group_size_type = nh_table_0->N_GROUP_SZ_TYPE;
    nh_group.r_group_size_type = nh_table_1->R_GROUP_SZ_TYPE;
    nh_group.flowlet_policy    = nh_table_1->FLOWLET_POLICY;
    nh_group.flowlet_age_reset = nh_table_1->FLOWLET_AGE_RESET;
    nh_group.group_min_index   = nh_table_1->GROUP_MIN_INDEX;
    nh_group.weight_row        = nh_table_0->WEIGHT_ROW;
    nh_group.weight_row_offset = nh_table_0->WEIGHT_ROW_OFFSET;

    // Is this the right way to ignore fields? <-- REVISIT!!!
    if (nh_group.group_type == MBY_NH_GROUP_TYPE_WCMP)
    {
        nh_group.n_group_size_type = 0; // ignore
        nh_group.r_group_size_type = 0; // ignore
    }
    else
    {
        nh_group.weight_row        = 0; // ignore
        nh_group.weight_row_offset = 0; // ignore
    }

    if (nh_group.group_type != MBY_NH_GROUP_TYPE_ECMP_FLOWLET)
    {
        nh_group.flowlet_age_reset = 0; // ignore
        nh_group.flowlet_policy    = 0; // ignore
    }

    nh_group.n_group_size = getNextHopGroupSize(nh_group.n_group_size_type, nh_table_0->N_GROUP_SIZE);
    nh_group.r_group_size = getNextHopGroupSize(nh_group.r_group_size_type, nh_table_1->R_GROUP_SIZE);

    return nh_group;
}

static mbyNextHopNeighbor getNextHopNeighborEntry
(
    mby_ppe_nexthop_map const * const nexthop_map,
    fm_uint16                   const route_idx
)
{
    mbyNextHopNeighbor nh_neigbor;

    nexthop_neighbors_table_0_r const * const nh_table_0 = &(nexthop_map->NH_NEIGHBORS_0[route_idx]);
    nexthop_neighbors_table_1_r const * const nh_table_1 = &(nexthop_map->NH_NEIGHBORS_1[route_idx]);

    nh_neigbor.entry_type      = nh_table_1->ENTRY_TYPE;
    nh_neigbor.update_l3domain = nh_table_1->UPDATE_L3_DOMAIN;
    nh_neigbor.update_l2domain = nh_table_1->UPDATE_L2_DOMAIN;
    nh_neigbor.l3domain        = nh_table_1->L3_DOMAIN;
    nh_neigbor.l2domain        = nh_table_1->L2_DOMAIN;
    nh_neigbor.mod_idx         = nh_table_1->MOD_IDX;
    nh_neigbor.mtu_idx         = nh_table_1->MTU_INDEX;
    nh_neigbor.dglort          = nh_table_0->DGLORT;

    fm_bool type_glort  = (nh_neigbor.entry_type == MBY_NH_ENTRY_TYPE_GLORT_FORWARDING);
    fm_bool type_ip     = (nh_neigbor.entry_type == MBY_NH_ENTRY_TYPE_IP_ROUTING);

    fm_macaddr dmac        = nh_table_0->DST_MAC;
    fm_uint16  evid        = nh_table_1->EVID;
    fm_bool    ipv6_entry  = nh_table_1->IPV6_ENTRY;
    fm_bool    mark_routed = nh_table_1->MARK_ROUTED;

    if (type_glort)
    {
        nh_neigbor.dmac       = dmac;
        nh_neigbor.evid       = evid;
        nh_neigbor.IPv6_entry = ipv6_entry;
    }
    else if (type_ip)
    {
        nh_neigbor.mark_routed = mark_routed;
    }

    return nh_neigbor;
}

static mbyNextHopConfig getNextHopConfig
(
    mby_ppe_nexthop_map const * const nexthop_map
)
{
    mbyNextHopConfig nh_config;

    nexthop_config_r const * const nh_config_reg = &(nexthop_map->NH_CONFIG);

    nh_config.sweeper_rate   = nh_config_reg->SWEEPER_RATE;
    nh_config.flowlet_int_en = nh_config_reg->FLOWLET_INT_EN;
    nh_config.flowlet_enable = nh_config_reg->FLOWLET_ENABLE;

    return nh_config;
}

/**
 * The NH_USED table is updated by the NextHop_Apply stage
 * at the time the header is processed and
 * is updated regardless of whether the packet has framing or data errors.
 */
static void setNextHopUsedEntry
(
    mby_ppe_nexthop_map const * const nexthop_map,
    mby_ppe_nexthop_map__addr * const nexthop_w,
    fm_uint32                   const neighbor_idx
)
{
    fm_byte index = FM_GET_FIELD(neighbor_idx, MBY_NH_USED, INDEX);
    fm_byte bit   = FM_GET_FIELD(neighbor_idx, MBY_NH_USED, BIT);

    nexthop_used_r const * const nh_used = &(nexthop_map->NH_USED[index]);
    nexthop_used_r__addr * const nh_used_w = &(nexthop_w->NH_USED[index]);

    fm_uint64 used_value = nh_used->USED;

    used_value |= (FM_LITERAL_U64(1) << bit);

    write_field(nh_used_w->USED, used_value);
}

static void setNextHopStatus
(
    mby_ppe_nexthop_map__addr * const nexthop_w,
    fm_uint16                   const entry_id
)
{
    nexthop_status_r__addr * const nh_status_w = &(nexthop_w->NH_STATUS);

    write_field(nh_status_w->FLOWLET, entry_id & 0x3FFF);
}


static mbyIngressVidTable getIvidTableEntry
(
    mby_ppe_nexthop_map const * const nexthop_map,
    fm_uint16                         vid
)
{
    mbyIngressVidTable entry;

    ingress_vid_table_r const * const vid_table = &(nexthop_map->INGRESS_VID_TABLE[vid]);

    entry.TRAP_IGMP  = vid_table->TRAP_IGMP;
    entry.REFLECT    = vid_table->REFLECT;
    entry.MEMBERSHIP = vid_table->MEMBERSHIP;

    return entry;
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

static fm_bool mtuCheck
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
    mby_ppe_nexthop_map__addr * const nexthop_w,
    fm_uint16                   const route_bin_idx,
    fm_byte                     const age_counter
)
{
    nexthop_routes_table_r__addr * const nh_routes_w = &(nexthop_w->NH_ROUTES[route_bin_idx]);

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
        out->l2_dmac = (nh_neighbor.IPv6_entry) ? dmac_ipv6 : nh_neighbor.dmac;
        out->route = TRUE;
    }
    else
    {
        out->dglort      = nh_neighbor.dglort;
        out->mark_routed = nh_neighbor.mark_routed;
        out->route       = nh_neighbor.mark_routed;
    }

    if (nh_neighbor.update_l2domain)
        out->l2_edomain = nh_neighbor.l2domain;

    if (nh_neighbor.update_l3domain)
        out->l3_edomain = nh_neighbor.l3domain;

    out->l2_evid = nh_neighbor.evid;

    out->mtu_violation = mtuCheck(nexthop_map, rx_length, nh_neighbor.mtu_idx);
}

static void NextHop_Apply
(
    mby_ppe_nexthop_map  const * const nexthop_map,
    mby_ppe_nexthop_map__addr  * const nexthop_w,
    mbyNextHopApplyInput const * const in,
    mbyNextHopApplyOutput      * const out
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
        mbyNextHopGroup nh_group = getNexthopGroupEntry(nexthop_map, group_idx);
        mbyNextHopGroupType group_type = nh_group.group_type;

        fm_uint16 group_base_idx = nh_group.base_index;
        if (group_type == MBY_NH_GROUP_TYPE_ECMP || group_type == MBY_NH_GROUP_TYPE_ECMP_FLOWLET)
        {
            fm_uint16 bin_count  = nh_group.r_group_size;
            fm_uint16 bin_offset = (ecmp_hash * bin_count) >> 12;

            fm_uint16 route_bin_idx  = group_base_idx + bin_offset;
            mbyNextHopRoute nh_route = getNexthopRouteEntry(nexthop_map, route_bin_idx);

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

static void NextHop_Sweep
(
    mby_ppe_nexthop_map  const * const nexthop_map,
    mby_ppe_nexthop_map__addr  * const nexthop_w,
    mbyNextHopSweepInput const * const in
)
{

}
static void NextHop_Usage
(
    mby_ppe_nexthop_map const * const nexthop_map
)
{

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
    mby_ppe_nexthop_map__addr       * const nexthop_w,
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
    fm_bool                  mark_routed    = FALSE;
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
        idglort     = dglort;
        mark_routed = FALSE;
    }

    /**
     * NextHop lookup is only performed if in addition to the FWD action indicating a ROUTE_ARP subtype,
     * the NO_ROUTE action flag is not set by the Classifier.
     */
    mark_routed  = route_arp && !no_route;

    mbyNextHopApplyInput  apply_in;
    mbyNextHopApplyOutput apply_out = { 0 };

    apply_in.fwd            = fwd;
    apply_in.ecmp_hash      = ecmp_hash;
    apply_in.dmac_from_ipv6 = dmac_from_ipv6;
    apply_in.l2_idomain     = l2_idomain;
    apply_in.l2_ivid1       = l2_ivid1;
    apply_in.l3_idomain     = l3_idomain;
    apply_in.flowlet_enable = flowlet_enable;

    apply_out.l2_dmac = l2_dmac;
    apply_out.dglort  = dglort;

    // if NextHop_Apply stage is enabled:
    if (mark_routed){
        NextHop_Apply(nexthop_map, nexthop_w, &apply_in, &apply_out);

        l2_evid1   = apply_out.l2_evid;
        idglort    = apply_out.dglort;
        l2_dmac    = apply_out.l2_dmac;
        l2_edomain = apply_out.l2_edomain;
        l2_evid1   = apply_out.l2_evid;
        l3_edomain = apply_out.l3_edomain;
    }


    /**
     * NextHop_Sweep stage will be scheduled only during packet idle periods to avoid contention.
     */
    // <-- How it should be handled in the FM? REVISIT!!!

    mbyNextHopSweepInput  sweep_in;

    sweep_in.flowlet_enabled_packet = apply_out.flowlet_enabled_packet;
    sweep_in.route_neighbor_idx     = apply_out.route_neighbor_idx;
    sweep_in.group_min_index        = apply_out.group_min_index;

    // if NextHop_Sweep stage is enabled:
    if (flowlet_enable)
        NextHop_Sweep(nexthop_map, nexthop_w, &sweep_in);

    /**
     * NextHop_Usage task need not be implemented, if the Path Loading Policy is deemed not useful.
     * See [here](https://securewiki.ith.intel.com/display/25T/RX-PPE+Next-Hop+Lookup#RX-PPENext-HopLookup-NextHop_Usage_warning).
     */
    NextHop_Usage(nexthop_map);

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
    out->DA_HIT               = da_hit;
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
    out->MARK_ROUTED          = mark_routed;
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
    out->OPERATOR_ID          = in->OPERATOR_ID;
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
