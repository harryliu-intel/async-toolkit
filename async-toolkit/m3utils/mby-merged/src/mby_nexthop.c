// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_classifier.h"
#include "mby_hash.h"
#include "mby_nexthop.h"
#include "mby_maskgen.h"

static void getARPTableEntry
(
          fm_uint32           regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint16           arp_tbl_idx,
          mbyArpTable * const arp_table
)
{
    fm_uint32 arp_table_regs[MBY_ARP_TABLE_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_ARP_TABLE(arp_tbl_idx, 0), MBY_ARP_TABLE_WIDTH, arp_table_regs);

    arp_table->DMAC           = FM_ARRAY_GET_FIELD64(arp_table_regs, MBY_ARP_TABLE, DST_MAC);
    arp_table->EntryType      = FM_ARRAY_GET_BIT    (arp_table_regs, MBY_ARP_TABLE, ENTRY_TYPE);
    arp_table->IPv6Entry      = FM_ARRAY_GET_BIT    (arp_table_regs, MBY_ARP_TABLE, IPV6_ENTRY);
    arp_table->EVID           = FM_ARRAY_GET_FIELD  (arp_table_regs, MBY_ARP_TABLE, EVID);
    arp_table->MTU_Index      = FM_ARRAY_GET_FIELD  (arp_table_regs, MBY_ARP_TABLE, MTU_INDEX);
    arp_table->ModIdx         = FM_ARRAY_GET_FIELD  (arp_table_regs, MBY_ARP_TABLE, MOD_IDX);
    arp_table->L3Domain       = FM_ARRAY_GET_FIELD  (arp_table_regs, MBY_ARP_TABLE, L3_DOMAIN);
    arp_table->L2Domain       = FM_ARRAY_GET_FIELD  (arp_table_regs, MBY_ARP_TABLE, L2_DOMAIN);
    arp_table->UpdateL3Domain = FM_ARRAY_GET_BIT    (arp_table_regs, MBY_ARP_TABLE, UPDATE_L3_DOMAIN);
    arp_table->UpdateL2Domain = FM_ARRAY_GET_BIT    (arp_table_regs, MBY_ARP_TABLE, UPDATE_L2_DOMAIN);
    
    fm_uint16 dglort      = FM_ARRAY_GET_FIELD(arp_table_regs, MBY_ARP_ENTRY_GLORT, DGLORT);
    fm_bool   mark_routed = FM_ARRAY_GET_BIT  (arp_table_regs, MBY_ARP_ENTRY_GLORT, markRouted);
    fm_bool   type_glort  = (arp_table->EntryType == MBY_ARP_TYPE_GLORT);

    arp_table->DGLORT     = (type_glort) ? dglort      : 0;
    arp_table->markRouted = (type_glort) ? mark_routed : FALSE;
}

static void setARPUsedEntry
(
          fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint32 arp_tbl_idx
)
{   
    fm_uint64 arp_used_reg = 0;
    mbyModelReadCSR64(regs, MBY_ARP_USED(arp_tbl_idx, 0), &arp_used_reg);

    fm_uint64 used_value = FM_GET_FIELD64(arp_used_reg, MBY_ARP_USED, USED);

    used_value |= (FM_LITERAL_U64(1) << (arp_tbl_idx & 0x3f));

    FM_SET_FIELD64(arp_used_reg, MBY_ARP_USED, USED, used_value);

    mbyModelWriteCSR64(regs, MBY_ARP_USED(arp_tbl_idx, 0), arp_used_reg);
}

static void GetMaTableEntry
(
    fm_uint32           regs[MBY_REGISTER_ARRAY_SIZE],
    fm_byte             set,
    fm_uint16           ma_tbl_idx,
    mbyMaTable  * const entry
)
{
    fm_uint32 ma_table_regs[MBY_MA_TABLE_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_MA_TABLE(set, ma_tbl_idx, 0), MBY_MA_TABLE_WIDTH, ma_table_regs);

    entry->OLD_PORT    = FM_ARRAY_GET_FIELD(ma_table_regs, MBY_MA_TABLE, OLD_PORT);
    entry->NEW_PORT    = FM_ARRAY_GET_FIELD(ma_table_regs, MBY_MA_TABLE, NEW_PORT);
    entry->ENTRY_TYPE  = FM_ARRAY_GET_FIELD(ma_table_regs, MBY_MA_TABLE, ENTRY_TYPE);
    entry->TRIG_ID     = FM_ARRAY_GET_FIELD(ma_table_regs, MBY_MA_TABLE, TRIG_ID);
    entry->S_GLORT     = FM_ARRAY_GET_FIELD(ma_table_regs, MBY_MA_TABLE, S_GLORT);
    entry->D_GLORT     = FM_ARRAY_GET_FIELD(ma_table_regs, MBY_MA_TABLE, D_GLORT);
    entry->L2_DOMAIN   = FM_ARRAY_GET_FIELD(ma_table_regs, MBY_MA_TABLE, L2_DOMAIN);
    entry->VID         = FM_ARRAY_GET_FIELD(ma_table_regs, MBY_MA_TABLE, VID);
    entry->MAC_ADDRESS = FM_ARRAY_GET_FIELD64(ma_table_regs, MBY_MA_TABLE, MAC_ADDRESS);

} /* end GetMaTableEntry*/

void lookUpAddress
(
          fm_uint32          regs[MBY_REGISTER_ARRAY_SIZE],
          fm_macaddr         macAddr,
          fm_uint16          vid,
          fm_uint16          l2_domain,
          fm_bool            learn_mode,
          fm_bool    * const hit,
          mbyMaTable * const entry
)
{
    mbyMaTable ma_entry;

    //learn_mode = 1 for IVL, LEARN_MODE = 0 for SVL
    fm_uint16 lvid     = (learn_mode) ? vid : 0;
    fm_uint16 camIndex = MBY_MA_TABLE_TCAM_SIZE; /*1024*/
    fm_byte   lset     = MBY_MAC_ADDR_BANK_COUNT - 1;// Using only bank = 5 <-- REVISIT!!!

    while (camIndex > 0)
    {
        camIndex--;
        GetMaTableEntry(regs, lset, camIndex, &ma_entry);

        if ((ma_entry.MAC_ADDRESS == macAddr) && (ma_entry.VID == lvid) && (ma_entry.L2_DOMAIN == l2_domain) &&
            (ma_entry.ENTRY_TYPE != MBY_MA_ENTRY_TYPE_NOT_USED))
        {
            *hit = TRUE;
            *entry = ma_entry;
            break;
        }
    }
}

void l2LookUp
(
          fm_uint32                   regs[MBY_REGISTER_ARRAY_SIZE],
          fm_bool                     flood_set,
          fm_uint16                   l2_edomain,
          fm_uint16           * const idglort,
    const mbyHashToNextHop    * const in,
          mbyNextHopToMaskGen * const out)
{
    out->GLORT_FORWARDED = 0;
    out->FLOOD_FORWARDED = 0;

    if (*idglort && !flood_set)
    {
        /* no change to dglort */
        out->GLORT_FORWARDED = 1;
    }
    else
    {
        fm_uint64 flood_glort_table_reg = 0;
        mbyModelReadCSR64(regs, MBY_FLOOD_GLORT_TABLE(l2_edomain, 0), &flood_glort_table_reg);

        lookUpAddress(regs, in->L2_DMAC, in->L2_IVID1, l2_edomain, in->LEARN_MODE, &out->DA_HIT, &out->DA_RESULT);

        if (out->DA_HIT && out->DA_RESULT.D_GLORT)
        {
            *idglort = out->DA_RESULT.D_GLORT;
            if(out->DA_RESULT.ENTRY_TYPE == MBY_MA_LOOKUP_ENTRY_TYPE_PROVISIONAL)
                out->AMASK |= MBY_AMASK_DROP_PROVISIONAL;
        }
        else if ((idglort != 0) && (flood_set == 1))
            /* no change to dglort */
            out->GLORT_FORWARDED = 1;
        else if (isBroadcastMacAddress(in->L2_DMAC))
            *idglort = FM_GET_FIELD64(flood_glort_table_reg, MBY_FLOOD_GLORT_TABLE, BROADCAST_GLORT);

        else if (isMulticastMacAddress(in->L2_DMAC))
        {
            *idglort = FM_GET_FIELD64(flood_glort_table_reg, MBY_FLOOD_GLORT_TABLE, FLOOD_MULTICAST_GLORT);
            out->FLOOD_FORWARDED = 1;
        }
        else
        {
            *idglort = FM_GET_FIELD64(flood_glort_table_reg, MBY_FLOOD_GLORT_TABLE, FLOOD_UNICAST_GLORT);
            out->FLOOD_FORWARDED = 1;
        }
    }
}

void NextHop
(
    fm_uint32                         regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyHashToNextHop    * const in,
          mbyNextHopToMaskGen * const out
)
{
    mbyClassifierFlags  ffu_flags = in->FFU_FLAGS;
    fm_uint32           ffu_route = in->FFU_ROUTE;

    fm_bool   no_route     = ffu_flags.no_route;
    fm_bool   group_type   = FM_GET_BIT  (ffu_route, MBY_FFU_ROUTE, GROUP_TYPE);
    fm_byte   group_size   = FM_GET_FIELD(ffu_route, MBY_FFU_ROUTE, GROUP_SIZE);
    fm_uint16 arp_index    = FM_GET_FIELD(ffu_route, MBY_FFU_ROUTE, ARP_INDEX) & 0x3fff; // 14-bits
    fm_bool   glort_routed = !FM_GET_BIT  (ffu_route, MBY_FFU_ROUTE, ARP_ROUTE);
    fm_bool   flood_set    = (glort_routed) ? FM_GET_BIT  (ffu_route, MBY_FFU_ROUTE, FLOODSET) : 0;
    fm_byte   dglort       = FM_GET_FIELD(ffu_route, MBY_FFU_ROUTE, DGLORT);
    fm_byte   sel_hash     = (group_type == 0) ? in->ARP_HASH[group_size] : ((in->RAW_HASH << group_size) >> 12);
    fm_uint16 arp_tbl_idx  = (arp_index + sel_hash) & (MBY_ARP_TABLE_ENTRIES - 1);

    // set ingress L2/L3 domain, based on PKT_META, DSI ingress CPM to HLP pkt:
//T state->L2_IDOMAIN = (state->PKT_META[20] >> 4) & 0x0f | ((state->PKT_META[21] & 0x1f) << 4);
//T state->L3_IDOMAIN = (state->PKT_META[21] >> 5) & 0x07 | ((state->PKT_META[22] & 0x07) << 3);

    fm_bool    encap       = in->ENCAP;
    fm_bool    decap       = in->DECAP;
    fm_macaddr dmac_ipv6   = in->DMAC_FROM_IPV6;
    fm_macaddr l2_smac     = in->L2_SMAC;
    fm_macaddr l2_dmac     = in->L2_DMAC;
    fm_uint16  l2_ivid1    = in->L2_IVID1;
    fm_uint16  l2_idomain  = in->L2_IDOMAIN;
    fm_byte    l3_idomain  = in->L3_IDOMAIN;
    fm_uint16  l2_evid1    = in->L2_IVID1;
    fm_uint16  l2_edomain  = in->L2_IDOMAIN;
    fm_byte    l3_edomain  = in->L3_IDOMAIN;

    fm_uint16  idglort     = 0;
    fm_bool    mark_routed = !glort_routed && !no_route;
    fm_byte    mtu_index   = 0;
    fm_uint32  mod_index   = 0;
    
    if (glort_routed)
    {
        idglort     = dglort;
        mark_routed = FALSE;
    }
    else // do ARP lookup
    {
        mbyArpTable arp_table;
        getARPTableEntry(regs, arp_tbl_idx, &arp_table);

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

        setARPUsedEntry(regs, arp_tbl_idx);
    }

    // l2Lookup is temporary placed in nexthop <-- REVISIT!!!
    l2LookUp(regs, flood_set, l2_edomain, &idglort, in, out);


    // Write outputs:
    out->ARP_TABLE_INDEX = arp_tbl_idx;
    out->L2_SMAC         = l2_smac;
    out->L2_DMAC         = l2_dmac;
    out->L2_IDOMAIN      = l2_idomain;
    out->L2_EDOMAIN      = l2_edomain;
    out->L3_IDOMAIN      = l3_idomain;
    out->L3_EDOMAIN      = l3_edomain;
    out->L2_IVID1        = l2_ivid1;
    out->L2_EVID1        = l2_evid1;
    out->MTU_INDEX       = mtu_index;
    out->FLOOD_SET       = flood_set;
    out->IDGLORT         = idglort;
    out->MARK_ROUTED     = mark_routed;
    out->MOD_IDX         = mod_index;
    out->ENCAP           = encap;
    out->DECAP           = decap;
}
