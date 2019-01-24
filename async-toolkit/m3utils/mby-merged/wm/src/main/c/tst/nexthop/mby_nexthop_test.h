#ifndef MBY_NEXTHOP_TEST_H
#define MBY_NEXTHOP_TEST_H

#include "mby_nexthop.h"

#define COLOR_RED     "\x1b[31m"
#define COLOR_GREEN   "\x1b[32m"
#define COLOR_RESET   "\x1b[0m"

typedef struct mby_nh_test_data_in_struct
{
    /* Next Hop input data. */
    fm_uint16          ecmp_hash;
    fm_macaddr         IPv6_dmac;

    /* FWD Action */
        // FWD_GLORT
    fm_bool            glort_routed;
    fm_bool            normal_fwd;
    fm_bool            is_floodset;
    fm_uint16          dglort;
        // ROUTE_ARP
    fm_bool            route_type;
    fm_uint16          route_index;

    /* Next Hop registers data. */
    fm_uint16          nh_route_idx;    // 0..16383
    fm_uint16          nh_neighbor_idx; // 0..16383
    mbyNextHopGroup    nh_group;
    mbyNextHopRoute    nh_route;
    mbyNextHopNeighbor nh_neighbor;
} mby_nh_test_data_in;

typedef struct mby_nh_test_data_out_struct
{
    fm_macaddr dmac;
    fm_uint16  evid;
    fm_byte    mtu_idx;
    fm_uint32  mod_idx;
    fm_bool    encap;
    fm_bool    decap;
    fm_uint16  l2_domain;
    fm_uint16  l3_domain;
    fm_uint16  dglort;
    fm_bool    routed;
    fm_uint64  nh_used;
} mby_nh_test_data_out;

typedef struct test_data_struct
{
    mby_nh_test_data_in  in;
    mby_nh_test_data_out out;
    char *               name;
} nh_test_data;

nh_test_data nexthop_tests[] =
{

    {
        .in =
        {
            .glort_routed = TRUE,
            .dglort       = 0x100,
        },
        .out =
        {
            .dglort = 0x100,
            .routed = FALSE,
        },
        .name = "Nexthop GLORT routed"
    },
    {
        .in =
        {
            .glort_routed                = FALSE,
            .route_index                 = 16,
            .route_type                  = MBY_NH_ROUTE_TYPE_SINGLE,
            .ecmp_hash                   = 2, // Not used
            .nh_neighbor.entry_type      = MBY_NH_ENTRY_TYPE_GLORT_FORWARDING,
            .nh_neighbor.dmac            = 0,
            .nh_neighbor.ipv6_entry      = FALSE,
            .nh_neighbor.evid            = 2,
            .nh_neighbor.mtu_idx         = 1,
            .nh_neighbor.mod_idx         = 17,
            .nh_neighbor.l2domain        = 2,
            .nh_neighbor.update_l2domain = FALSE,
            .nh_neighbor.l3domain        = 3,
            .nh_neighbor.update_l3domain = TRUE,
            .nh_neighbor.dglort          = 0x101,
            .nh_neighbor.mark_routed     = TRUE,
        },
        .out =
        {
            .evid      = 2,
            .mtu_idx   = 1,
            .mod_idx   = 4,
            .encap     = 1,
            .decap     = 0,
            .l2_domain = 0,
            .l3_domain = 3,
            .dglort    = 0x101,
            .routed    = TRUE,
            .nh_used   = 0x40000,
        },
        .name = "Nexthop ARP single GLORT non-IPv6"
    },
    {
        .in =
        {
            .glort_routed                = FALSE,
            .ecmp_hash                   = 0xC00,
            .route_type                  = MBY_NH_ROUTE_TYPE_GROUP,
            .route_index                 = 0xBF,
            .nh_group.group_type         = MBY_NH_GROUP_TYPE_ECMP,
            .nh_group.base_index         = 0x100, // (0xBF + ((0x400 << 4) >> 12)) & (MBY_NH_NEIGHBORS_ENTRIES - 1),//arp_tbl_idx = 0xC3
            // .nh_group.n_group_size_type  = ,
            // .nh_group.n_group_size       = ,
            // .nh_group.weight_row         = ,
            // .nh_group.weight_row_offset  = ,
            .nh_group.r_group_size_type  = MBY_NH_GROUP_SIZE_TYPE_LITERAL,
            .nh_group.r_group_size       = 4,
            // .nh_group.flowlet_policy     = ,
            // .nh_group.flowlet_age_reset  = ,
            // .nh_group.group_min_index    = ,
            .nh_route_idx                = 0x103,
            // .nh_route.age_counter        = ,
            .nh_route.group_index        = 0xBF,
            .nh_route.neighbor_index     = 0x1,
            .nh_neighbor_idx             = 0x1,
            .nh_neighbor.entry_type      = MBY_NH_ENTRY_TYPE_IP_ROUTING,
            .nh_neighbor.dmac            = 0x0022446688aa,
            .nh_neighbor.ipv6_entry      = FALSE,
            .nh_neighbor.evid            = 12,
            .nh_neighbor.mtu_idx         = 4,
            .nh_neighbor.mod_idx         = 35,
            .nh_neighbor.l2domain        = 17,
            .nh_neighbor.update_l2domain = TRUE,
            .nh_neighbor.l3domain        = 23,
            .nh_neighbor.update_l3domain = TRUE,
        },
        .out =
        {
            .evid      = 12,
            .mtu_idx   = 4,
            .mod_idx   = 8,
            .encap     = 1,
            .decap     = 1,
            .l2_domain = 17,
            .l3_domain = 23,
            .dmac      = 0x0022446688aa,
            .routed    = TRUE,
            .nh_used   = 0x1,
        },
        .name = "Nexthop ARP group MAC non-IPv6"
    },
    {
        .in =
        {
            .glort_routed                = FALSE,
            .ecmp_hash                   = 0x7F8,
            .IPv6_dmac                   = 0x003377fedcba,
            .route_type                  = MBY_NH_ROUTE_TYPE_GROUP,
            .route_index                 = 0xCEF,
            .nh_group.group_type         = MBY_NH_GROUP_TYPE_ECMP,
            .nh_group.base_index         = 0xEF2,
            .nh_group.r_group_size_type  = MBY_NH_GROUP_SIZE_TYPE_LITERAL,
            .nh_group.r_group_size       = 8,
            .nh_route_idx                = 0xEF5,
            .nh_route.group_index        = 0xCEF,
            .nh_route.neighbor_index     = 0x2,
            .nh_neighbor_idx             = 0x2,
            .nh_neighbor.entry_type      = MBY_NH_ENTRY_TYPE_IP_ROUTING,
            .nh_neighbor.dmac            = 0x123456789abc,
            .nh_neighbor.ipv6_entry      = TRUE,
            .nh_neighbor.evid            = 3,
            .nh_neighbor.mtu_idx         = 2,
            .nh_neighbor.mod_idx         = 6,
            .nh_neighbor.l2domain        = 5,
            .nh_neighbor.update_l2domain = TRUE,
            .nh_neighbor.l3domain        = 0,
            .nh_neighbor.update_l3domain = FALSE,
        },
        .out =
        {
            .evid      = 3,
            .mtu_idx   = 2,
            .mod_idx   = 1,
            .encap     = 0,
            .decap     = 1,
            .l2_domain = 5,
            .l3_domain = 0,
            .dmac      = 0x003377fedcba,
            .routed    = TRUE,
            .nh_used   = 0x2,// (1 << 1)
        },
        .name = "Nexthop ARP routed MAC entry type IPV6"
    }
};

#endif
