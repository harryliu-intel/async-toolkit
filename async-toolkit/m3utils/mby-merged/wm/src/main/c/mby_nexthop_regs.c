// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include <model_c_write.h> // write_field()

#include "mby_nexthop_regs.h"

// Helpers:

fm_uint16 getNextHopGroupSize
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

fm_uint16 setNextHopGroupSize
(
    mbyNextHopGroupSizeType const group_size_type,
    fm_uint16               const group_size
)
{
    fm_byte x_group_size = 0;

    if (group_size_type == MBY_NH_GROUP_SIZE_TYPE_LITERAL)
    {
        x_group_size = group_size;
        if (group_size == 64)
            x_group_size == 0;
    }
    else if (group_size_type == MBY_NH_GROUP_SIZE_TYPE_POWER_OF_2)
    {
        fm_uint16 group = group_size;
        while (group >>= 1)
            x_group_size++;
    }

    return x_group_size;
}

// Getters:

mbyIngressVidTable getIvidTableEntry
(
    mby_ppe_nexthop_map const * const nexthop_map,
    fm_uint16                   const vid
)
{
    mbyIngressVidTable entry;

    ingress_vid_table_r const * const vid_table = &(nexthop_map->INGRESS_VID_TABLE[vid]);

    entry.TRAP_IGMP  = vid_table->TRAP_IGMP;
    entry.REFLECT    = vid_table->REFLECT;
    entry.MEMBERSHIP = vid_table->MEMBERSHIP;

    return entry;
}

mbyNextHopConfig getNextHopConfig
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

mbyNextHopGroup getNextHopGroupEntry
(
    mby_ppe_nexthop_map const * const nexthop_map,
    fm_uint16                   const group_idx
)
{
    mbyNextHopGroup nh_group = { 0 };

    nexthop_groups_table_0_r const * const nh_table_0 = &(nexthop_map->NH_GROUPS_0[group_idx]);
    nexthop_groups_table_1_r const * const nh_table_1 = &(nexthop_map->NH_GROUPS_1[group_idx]);

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

mbyNextHopNeighbor getNextHopNeighborEntry
(
    mby_ppe_nexthop_map const * const nexthop_map,
    fm_uint16                   const neighbor_idx
)
{
    mbyNextHopNeighbor nh_neigbor;

    nexthop_neighbors_table_0_r const * const nh_table_0 = &(nexthop_map->NH_NEIGHBORS_0[neighbor_idx]);
    nexthop_neighbors_table_1_r const * const nh_table_1 = &(nexthop_map->NH_NEIGHBORS_1[neighbor_idx]);

    nh_neigbor.dglort          = nh_table_0->DGLORT;
    nh_neigbor.dmac            = nh_table_0->DST_MAC;
    nh_neigbor.entry_type      = nh_table_1->ENTRY_TYPE;
    nh_neigbor.evid            = nh_table_1->EVID;
    nh_neigbor.ipv6_entry      = nh_table_1->IPV6_ENTRY;
    nh_neigbor.l2domain        = nh_table_1->L2_DOMAIN;
    nh_neigbor.l3domain        = nh_table_1->L3_DOMAIN;
    nh_neigbor.mark_routed     = nh_table_1->MARK_ROUTED;
    nh_neigbor.mod_idx         = nh_table_1->MOD_IDX;
    nh_neigbor.mtu_idx         = nh_table_1->MTU_INDEX;
    nh_neigbor.update_l2domain = nh_table_1->UPDATE_L2_DOMAIN;
    nh_neigbor.update_l3domain = nh_table_1->UPDATE_L3_DOMAIN;

    return nh_neigbor;
}

mbyNextHopRoute getNextHopRouteEntry
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

void getNexthopWeights
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

// Setters:

void setNextHopGroupEntry
(
    mby_ppe_nexthop_map__addr const * const nexthop_w,
    fm_uint16                         const group_idx,
    mbyNextHopGroup           const * const nh_group
)
{
    nexthop_groups_table_0_r__addr const * const nh_table_0 = &(nexthop_w->NH_GROUPS_0[group_idx]);
    nexthop_groups_table_1_r__addr const * const nh_table_1 = &(nexthop_w->NH_GROUPS_1[group_idx]);

    fm_byte n_group_size = setNextHopGroupSize(nh_group->n_group_size_type, nh_group->n_group_size);
    fm_byte r_group_size = setNextHopGroupSize(nh_group->r_group_size_type, nh_group->r_group_size);

    write_field(nh_table_0->GROUP_TYPE        , nh_group->group_type       );
    write_field(nh_table_0->BASE_INDEX        , nh_group->base_index       );
    write_field(nh_table_0->N_GROUP_SZ_TYPE   , nh_group->n_group_size_type);
    write_field(nh_table_1->R_GROUP_SZ_TYPE   , nh_group->r_group_size_type);
    write_field(nh_table_1->FLOWLET_POLICY    , nh_group->flowlet_policy   );
    write_field(nh_table_1->FLOWLET_AGE_RESET , nh_group->flowlet_age_reset);
    write_field(nh_table_1->GROUP_MIN_INDEX   , nh_group->group_min_index  );
    write_field(nh_table_0->WEIGHT_ROW        , nh_group->weight_row       );
    write_field(nh_table_0->WEIGHT_ROW_OFFSET , nh_group->weight_row_offset);
    write_field(nh_table_0->N_GROUP_SIZE      , n_group_size               );
    write_field(nh_table_1->R_GROUP_SIZE      , r_group_size               );
}

void setNextHopNeighborEntry
(
    mby_ppe_nexthop_map__addr const * const nexthop_w,
    fm_uint16                         const neighbor_idx,
    mbyNextHopNeighbor        const * const nh_neigbor
)
{
    nexthop_neighbors_table_0_r__addr const * const nh_table_0 = &(nexthop_w->NH_NEIGHBORS_0[neighbor_idx]);
    nexthop_neighbors_table_1_r__addr const * const nh_table_1 = &(nexthop_w->NH_NEIGHBORS_1[neighbor_idx]);

    write_field(nh_table_1->ENTRY_TYPE       , nh_neigbor->entry_type     );
    write_field(nh_table_0->DGLORT           , nh_neigbor->dglort         );
    write_field(nh_table_0->DST_MAC          , nh_neigbor->dmac           );
    write_field(nh_table_1->EVID             , nh_neigbor->evid           );
    write_field(nh_table_1->IPV6_ENTRY       , nh_neigbor->ipv6_entry     );
    write_field(nh_table_1->UPDATE_L3_DOMAIN , nh_neigbor->update_l3domain);
    write_field(nh_table_1->UPDATE_L2_DOMAIN , nh_neigbor->update_l2domain);
    write_field(nh_table_1->L3_DOMAIN        , nh_neigbor->l3domain       );
    write_field(nh_table_1->L2_DOMAIN        , nh_neigbor->l2domain       );
    write_field(nh_table_1->MOD_IDX          , nh_neigbor->mod_idx        );
    write_field(nh_table_1->MTU_INDEX        , nh_neigbor->mtu_idx        );
    write_field(nh_table_1->MARK_ROUTED      , nh_neigbor->mark_routed    );
}

void setNextHopRouteEntry
(
    mby_ppe_nexthop_map__addr const * const nexthop_w,
    fm_uint16                         const route_idx,
    mbyNextHopRoute           const * const nh_route
)
{
    nexthop_routes_table_r__addr const * const nh_table = &(nexthop_w->NH_ROUTES[route_idx]);

    write_field(nh_table->AGE_COUNTER  , nh_route->age_counter   );
    write_field(nh_table->GROUP_IDX    , nh_route->group_index   );
    write_field(nh_table->NEIGHBOR_IDX , nh_route->neighbor_index);
}

void setNextHopStatus
(
    mby_ppe_nexthop_map__addr const * const nexthop_w,
    fm_uint16                         const entry_id
)
{
    nexthop_status_r__addr const * const nh_status_w = &(nexthop_w->NH_STATUS);

    write_field(nh_status_w->FLOWLET, entry_id & 0x3FFF);
}

/**
 * The NH_USED table is updated by the NextHop_Apply stage
 * at the time the header is processed and
 * is updated regardless of whether the packet has framing or data errors.
 */
void setNextHopUsedEntry
(
    mby_ppe_nexthop_map       const * const nexthop_map,
    mby_ppe_nexthop_map__addr const * const nexthop_w,
    fm_uint32                         const neighbor_idx
)
{
    fm_byte index = FM_GET_FIELD(neighbor_idx, MBY_NH_USED, INDEX);
    fm_byte bit   = FM_GET_FIELD(neighbor_idx, MBY_NH_USED, BIT);

    nexthop_used_r       const * const nh_used = &(nexthop_map->NH_USED[index]);
    nexthop_used_r__addr const * const nh_used_w = &(nexthop_w->NH_USED[index]);

    fm_uint64 used_value = nh_used->USED;

    used_value |= (FM_LITERAL_U64(1) << bit);

    write_field(nh_used_w->USED, used_value);
}
