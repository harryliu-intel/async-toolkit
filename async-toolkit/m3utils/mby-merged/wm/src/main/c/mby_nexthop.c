// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_common.h"
#include "mby_classifier.h"
#include "mby_hash.h"
#include "mby_nexthop.h"
#include "mby_maskgen.h"

static mbyArpTable getARPTableEntry
(
    mby_ppe_nexthop_map * const nexthop,
    const fm_uint16             arp_tbl_idx
)
{
    mbyArpTable arp_table;

    nexthop_neighbors_table_0_r const * const nh_table_0 = &(nexthop->NH_NEIGHBORS_0[arp_tbl_idx]);
    nexthop_neighbors_table_1_r const * const nh_table_1 = &(nexthop->NH_NEIGHBORS_1[arp_tbl_idx]);

    arp_table.DMAC           = nh_table_0->DST_MAC;
    arp_table.EntryType      = nh_table_1->ENTRY_TYPE;
    arp_table.IPv6Entry      = nh_table_1->IPV6_ENTRY;
    arp_table.EVID           = nh_table_1->EVID;
    arp_table.MTU_Index      = nh_table_1->MTU_INDEX;
    arp_table.ModIdx         = nh_table_1->MOD_IDX;
    arp_table.L3Domain       = nh_table_1->L3_DOMAIN;
    arp_table.L2Domain       = nh_table_1->L2_DOMAIN;
    arp_table.UpdateL3Domain = nh_table_1->UPDATE_L3_DOMAIN;
    arp_table.UpdateL2Domain = nh_table_1->UPDATE_L2_DOMAIN;

    fm_uint16 dglort      = nh_table_0->DGLORT;
    fm_bool   mark_routed = nh_table_1->MARK_ROUTED;
    fm_bool   type_glort  = (arp_table.EntryType == MBY_ARP_TYPE_GLORT);

    arp_table.DGLORT     = (type_glort) ? dglort      : 0;
    arp_table.markRouted = (type_glort) ? mark_routed : FALSE;


    return arp_table;
}

static void setARPUsedEntry
(
    mby_ppe_nexthop_map * const nexthop,
    const fm_uint32             arp_tbl_idx
)
{
    nexthop_used_r * const nh_used = &(nexthop->NH_USED[arp_tbl_idx >> 6]);

    fm_uint64 used_value = nh_used->USED;
    used_value          |= (FM_LITERAL_U64(1) << (arp_tbl_idx & 0x3f));
    nh_used->USED        = used_value;
}


static mbyIngressVidTable getIvidTableEntry
(
    mby_ppe_nexthop_map * const nexthop,
    fm_uint16                   vid
)
{
    mbyIngressVidTable entry;

    ingress_vid_table_r * const vid_table = &(nexthop->INGRESS_VID_TABLE[vid]);

    entry.TRAP_IGMP  = vid_table->TRAP_IGMP;
    entry.REFLECT    = vid_table->REFLECT;
    entry.MEMBERSHIP = vid_table->MEMBERSHIP;

    return entry;
}

static void lookUpL2
(
    mby_ppe_nexthop_map * const nexthop,
    fm_uint32                   rx_port,
    fm_macaddr                  l2_dmac,
    fm_uint16                   ivid1,
    fm_uint16                   evid1,
    fm_bool                     flood_set,
    fm_uint16                   l2_edomain,
    fm_bool                     learn_mode,
    fm_uint16   * const         idglort,
    fm_bool     * const         glort_forwarded,
    fm_bool     * const         flood_forwarded,
    fm_bool     * const         da_hit,
    mbyMaTable  * const         da_result,
    fm_uint64   * const         amask,
    fm_bool     * const         l2_ivlan1_membership,
    fm_bool     * const         l2_ivlan1_reflect,
    fm_bool     * const         trap_igmp
)
{
    /* Perform ingress VLAN lookup. */
    mbyIngressVidTable ividTable = getIvidTableEntry(nexthop, ivid1);

    *l2_ivlan1_reflect    = ividTable.REFLECT;
    *trap_igmp           &= ividTable.TRAP_IGMP;
    *l2_ivlan1_membership = FM_GET_UNNAMED_FIELD(ividTable.MEMBERSHIP, rx_port, 1);

    *glort_forwarded = 0;
    *flood_forwarded = 0;

    if (*idglort && !flood_set)
        /* no change to dglort */
        *glort_forwarded = 0;
    else
    {
        flood_glort_table_r * const flood_glort_table = &(nexthop->FLOOD_GLORT_TABLE[l2_edomain]);
        if (*da_hit && da_result->D_GLORT)
        {
            *idglort = da_result->D_GLORT;
            if(da_result->ENTRY_TYPE == MBY_MA_LOOKUP_ENTRY_TYPE_PROVISIONAL)
                *amask |= MBY_AMASK_DROP_PROVISIONAL;
        }
        else if ((idglort != 0) && (flood_set == 1))
            /* no change to dglort */
            *glort_forwarded = 1;
        else if (isBroadcastMacAddress(l2_dmac))
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

void NextHop
(
    mby_ppe_nexthop_map       * const nexthop,
    const mbyHashToNextHop    * const in,
          mbyNextHopToMaskGen * const out
)
{
    // Read inputs:
    const mbyClassifierFlags cgrp_flags  = in->CGRP_FLAGS;
    const fm_uint32          cgrp_route  = in->CGRP_ROUTE;
          fm_bool            encap       = in->ENCAP;
          fm_bool            decap       = in->DECAP;
    const fm_macaddr         dmac_ipv6   = in->DMAC_FROM_IPV6;
    const fm_macaddr         l2_smac     = in->L2_SMAC;
          fm_macaddr         l2_dmac     = in->L2_DMAC;
    const fm_uint16          l2_ivid1    = in->L2_IVID1;
    const fm_uint16          l2_idomain  = in->L2_IDOMAIN;
          fm_byte            l3_idomain  = in->L3_IDOMAIN;
          fm_uint16          l2_evid1    = in->L2_IVID1;
          fm_uint16          l2_edomain  = in->L2_IDOMAIN;
          fm_byte            l3_edomain  = in->L3_IDOMAIN;
    const fm_bool            learn_mode  = in->LEARN_MODE;
    const fm_uint16          raw_hash    = in->RAW_HASH;
    const fm_byte * const    arp_hash    = in->ARP_HASH; // [16]
          fm_bool            trap_igmp   = in->TRAP_IGMP;
    const fm_uint32          rx_port     = in->RX_PORT;

    fm_bool   no_route     = cgrp_flags.no_route;
    fm_bool   group_type   = FM_GET_BIT  (cgrp_route, MBY_CGRP_ROUTE, GROUP_TYPE);
    fm_byte   group_size   = FM_GET_FIELD(cgrp_route, MBY_CGRP_ROUTE, GROUP_SIZE);
    fm_uint16 arp_index    = FM_GET_FIELD(cgrp_route, MBY_CGRP_ROUTE, ARP_INDEX) & 0x3fff; // 14-bits
    fm_bool   glort_routed = !FM_GET_BIT (cgrp_route, MBY_CGRP_ROUTE, ARP_ROUTE);
    fm_uint16 dglort       = FM_GET_FIELD(cgrp_route, MBY_CGRP_ROUTE, DGLORT);
    fm_bool   flood_set    = (glort_routed) ? FM_GET_BIT  (cgrp_route, MBY_CGRP_ROUTE, FLOODSET) : 0;
    fm_byte   sel_hash     = (group_type == 0) ? arp_hash[group_size] : ((raw_hash << group_size) >> 12);
    fm_uint16 arp_tbl_idx  = (arp_index + sel_hash) & (MBY_ARP_TABLE_ENTRIES - 1);
    fm_uint16 idglort      = 0;
    fm_bool   mark_routed  = !glort_routed && !no_route;
    fm_byte   mtu_index    = 0;
    fm_uint32 mod_index    = 0;

    if (glort_routed)
    {
        idglort     = dglort;
        mark_routed = FALSE;
    }
    else // do ARP lookup
    {
        mbyArpTable arp_table = getARPTableEntry(nexthop, arp_tbl_idx);

        fm_bool arp_type_mac = (arp_table.EntryType == MBY_ARP_TYPE_MAC);

        if (arp_type_mac) {
            l2_dmac     = (arp_table.IPv6Entry == 1) ? dmac_ipv6 : arp_table.DMAC;
        } else {
            idglort     = arp_table.DGLORT;
            mark_routed = arp_table.markRouted;
        }

        if (arp_table.UpdateL2Domain)
            l2_edomain = arp_table.L2Domain;

        if (arp_table.UpdateL3Domain)
            l3_edomain = arp_table.L3Domain;

        l2_evid1   =  arp_table.EVID;
        mtu_index  =  arp_table.MTU_Index;
        mod_index  = (arp_table.ModIdx >> 2) & 0xFFFF;
        decap      = (arp_table.ModIdx >> 1) & 0x1;
        encap      =  arp_table.ModIdx       & 0x1;
        setARPUsedEntry(nexthop, arp_tbl_idx);
    }

    // l2Lookup is temporary placed in nexthop - will be moved to Exact Match table <-- REVISIT!!!
    fm_bool     glort_forwarded      = FALSE;
    fm_bool     flood_forwarded      = FALSE;
    fm_bool     da_hit               = FALSE;
    mbyMaTable  da_result            = { 0 };
    fm_uint64   amask                = 0;
    fm_bool     l2_ivlan1_membership = FALSE;
    fm_bool     l2_ivlan1_reflect    = FALSE;

    lookUpL2
    (
        nexthop,
        rx_port,
        l2_dmac,
        l2_ivid1,
        l2_evid1,
        flood_set,
        l2_edomain,
        learn_mode,
        &idglort,
        &glort_forwarded,
        &flood_forwarded,
        &da_hit,
        &da_result,
        &amask,
        &l2_ivlan1_membership,
        &l2_ivlan1_reflect,
        &trap_igmp
    );

    // Write outputs:
    out->AMASK                = amask;
    out->ARP_TABLE_INDEX      = arp_tbl_idx;
    out->DA_HIT               = da_hit;
    out->DA_RESULT            = da_result;
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
    out->MOD_IDX              = mod_index;
    out->MTU_INDEX            = mtu_index;
    out->RX_PORT              = rx_port;
    out->TRAP_IGMP            = trap_igmp;

    // Pass thru:
    out->ACTION               = in->ACTION;
    out->CPU_TRAP             = in->CPU_TRAP;
    out->CSGLORT              = in->CSGLORT;
    out->DROP_TTL             = in->DROP_TTL;
    out->CGRP_FLAGS           = in->CGRP_FLAGS;
    out->GLORT_DMASK          = in->GLORT_DMASK;
    out->HASH_ROT_A           = in->HASH_ROT_A;
    out->HASH_ROT_B           = in->HASH_ROT_B;
    out->IP_MCAST_IDX         = in->IP_MCAST_IDX;
    out->IS_IPV4              = in->IS_IPV4;
    out->IS_IPV6              = in->IS_IPV6;
    out->L2_ETYPE             = in->L2_ETYPE;
    out->MIRROR0_PROFILE_IDX  = in->MIRROR0_PROFILE_IDX;
    out->MOD_PROF_IDX         = in->MOD_PROF_IDX;
    out->MTU_VIOLATION        = in->MTU_VIOLATION;
    out->NO_LEARN             = in->NO_LEARN;
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
    out->RX_DATA              = in->RX_DATA;
    out->RX_LENGTH            = in->RX_LENGTH;
    out->RX_MIRROR            = in->RX_MIRROR;
    out->SA_HIT               = in->SA_HIT;
    out->SA_RESULT            = in->SA_RESULT;
    out->SEG_META_ERR         = in->SEG_META_ERR;
    out->SV_DROP              = in->SV_DROP;
    out->TRAP_ICMP            = in->TRAP_ICMP;
    out->TRAP_IP_OPTIONS      = in->TRAP_IP_OPTIONS;
    out->TRIGGERS             = in->TRIGGERS;
}
