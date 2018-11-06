#ifndef MBY_NEXTHOP_TEST_H
#define MBY_NEXTHOP_TEST_H

#include "mby_nexthop.h"

#define COLOR_RED     "\x1b[31m"
#define COLOR_GREEN   "\x1b[32m"
#define COLOR_RESET   "\x1b[0m"

typedef struct mby_nh_test_data_in_struct
{
    /* Next Hop input data. */
    fm_bool         glort_routed;
    fm_uint16       arp_index;
    fm_bool         group_type;
    fm_byte         group_size;
    fm_byte         arp_hash;
    fm_uint16       arp_tbl_idx;
    fm_uint16       ecmp_hash;

    /* Next Hop register data. */
    mbyArpEntryType entry_type; // 1 = MAC entry, 0 - Glort Entry
    fm_macaddr      dmac;
    fm_uint16       dglort;
    fm_byte         mtu_index;
    fm_bool         mark_routed;
    fm_bool         ipv6_entry;
    fm_uint16       evid;
    fm_byte         l3_domain;
    fm_uint16       l2_domain;
    fm_bool         update_l3_domain;
    fm_bool         update_l2_domain;
    fm_uint32       mod_idx;
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
    fm_bool    mark_routed;
    fm_uint64  arp_used;
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
            .glort_routed     = TRUE,
            .dglort           = 0x100,
            .mark_routed      = TRUE,
        },
        .out =
        {
            .dglort           = 0x100,
            .mark_routed      = FALSE,
        },
        .name = "Nexthop GLORT routed"
    },
    {
        .in =
        {
            .glort_routed     = FALSE,
            .arp_index        = 16,
            .group_type       = 0,
            .group_size       = 4,
            .arp_hash         = 2,
            .arp_tbl_idx      = (16 + 2) & (MBY_ARP_TABLE_ENTRIES - 1),
            .entry_type       = MBY_ARP_TYPE_GLORT,
            .dmac             = 0,
            .ipv6_entry       = FALSE,
            .evid             = 2,
            .mtu_index        = 1,
            .mod_idx          = 17,
            .l2_domain        = 0,
            .update_l2_domain = FALSE,
            .l3_domain        = 3,
            .update_l3_domain = TRUE,
            .dglort           = 0x101,
            .mark_routed      = TRUE,
        },
        .out =
        {
            .evid             = 2,
            .mtu_idx          = 1,
            .mod_idx          = 4,
            .encap            = 1,
            .decap            = 0,
            .l2_domain        = 0,
            .l3_domain        = 3,
            .dglort           = 0x101,
            .mark_routed      = TRUE,
            .arp_used         = 0x40000,//(1 << ((16 + 2) & 0x3f))
        },
        .name = "Nexthop ARP routed GLORT entry type"
    },
    {
        .in =
        {
            .glort_routed     = FALSE,
            .arp_index        = 0xBF,
            .group_type       = 1,
            .group_size       = 4,
            .ecmp_hash        = 0x400,
            .arp_tbl_idx      = (0xBF + ((0x400 << 4) >> 12)) & (MBY_ARP_TABLE_ENTRIES - 1),//arp_tbl_idx = 0xC3
            .entry_type       = MBY_ARP_TYPE_MAC,
            .dmac             = 0x0022446688aa,
            .ipv6_entry       = FALSE,
            .evid             = 12,
            .mtu_index        = 4,
            .mod_idx          = 35,
            .l2_domain        = 17,
            .update_l2_domain = TRUE,
            .l3_domain        = 23,
            .update_l3_domain = TRUE,
        },
        .out =
        {
            .evid             = 12,
            .mtu_idx          = 4,
            .mod_idx          = 8,
            .encap            = 1,
            .decap            = 1,
            .l2_domain        = 17,
            .l3_domain        = 23,
            .dmac             = 0x0022446688aa,
            .arp_used         = 0x8,// (1 << 3)
        },
        .name = "Nexthop ARP routed MAC entry type non-IPV6"
    },
    {
        .in =
        {
            .glort_routed     = FALSE,
            .arp_index        = 0xEF2,
            .group_type       = 1,
            .group_size       = 8,
            .ecmp_hash        = 0xFF,
            .arp_tbl_idx      = (0xEF2 + ((0xFF << 8) >> 12)) & (MBY_ARP_TABLE_ENTRIES - 1),//arp_tbl_idx = 0xF01
            .entry_type       = MBY_ARP_TYPE_MAC,
            .dmac             = 0x003377fedcba,
            .ipv6_entry       = TRUE,
            .evid             = 3,
            .mtu_index        = 2,
            .mod_idx          = 6,
            .l2_domain        = 5,
            .update_l2_domain = TRUE,
            .l3_domain        = 0,
            .update_l3_domain = FALSE,
        },
        .out =
        {
            .evid             = 3,
            .mtu_idx          = 2,
            .mod_idx          = 1,
            .encap            = 0,
            .decap            = 1,
            .l2_domain        = 5,
            .l3_domain        = 0,
            .dmac             = 0x003377fedcba,
            .arp_used         = 0x2,// (1 << 1)
        },
        .name = "Nexthop ARP routed MAC entry type IPV6"
    },
};

#endif
