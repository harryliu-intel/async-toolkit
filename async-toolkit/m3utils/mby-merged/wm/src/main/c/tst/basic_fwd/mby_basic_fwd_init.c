#include "mby_basic_fwd_init.h"

#include <mby_bitfield.h>
#include <mby_classifier.h>
#include <mby_common.h>
#include <mby_congmgmt.h>
#include <mby_crc32.h>
#include <mby_hash.h>
#include <mby_mapper.h>
#include <mby_maskgen.h>
#include <mby_model.h>
#include <mby_modifier.h>
#include <mby_nexthop.h>
#include <mby_parser.h>
#include <mby_pipeline.h>
#include <mby_rxstats.h>
#include <mby_triggers.h>
#include <mby_txstats.h>

static void convertKeysToBytes
(
    fm_uint32 key32[MBY_CGRP_KEY32],
    fm_uint16 key16[MBY_CGRP_KEY16],
    fm_byte   key8 [MBY_CGRP_KEY8],
    fm_byte   bytes[MBY_CGRP_HASH_KEYS]
)
{
    for (fm_uint i = 0; i < MBY_CGRP_KEY16; i++)
    {
        bytes[2*i + 1] =  key16[i]       & 0xFF;
        bytes[2*i    ] = (key16[i] >> 8) & 0xFF;
    }

    for (fm_uint i = 0; i < MBY_CGRP_KEY8; i++)
        bytes[MBY_CGRP_KEY16*2 + i] = key8[i];

    for (fm_uint i = 0; i < MBY_CGRP_KEY32; i++)
        for (fm_uint j = 0; j < 4; j++)
            bytes[MBY_CGRP_KEY16*2 + MBY_CGRP_KEY8 + i*4 + (3-j)] = (key32[i] >> (j * 8 )) & 0xFF;
}
static fm_uint16 calculateLookupPtr
(
    fm_uint16       const hash_index,
    em_hash_cfg_r * const hash_cfg
)
{
    return hash_cfg->BASE_PTR_0 + (hash_index % (1uL << hash_cfg->HASH_SIZE_0));
}

void basic_fwd_init
(
    mby_ppe_rx_top_map * const rx_top_map,
    mby_shm_map        * const shm_map,
    fm_uint32                  fwd_port,
    fm_macaddr                 dmac
)
{
    ingress_vid_table_r * const ivid_table = &(rx_top_map->nexthop.INGRESS_VID_TABLE[1]);

    ivid_table->TRAP_IGMP  = 0;
    ivid_table->REFLECT    = 0;
    ivid_table->MEMBERSHIP = 0x3ffff;

    egress_vid_table_r * const evid_table = &(rx_top_map->mst_glort.EGRESS_VID_TABLE[1][0]);

    evid_table->MEMBERSHIP = 0x3ffff;

    ingress_mst_table_r * const ingress_mst_table = &(rx_top_map->mst_glort.INGRESS_MST_TABLE[1]);

    ingress_mst_table->STP_STATE_0 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_1 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_2 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_3 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_4 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_5 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_6 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_7 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_8 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_9 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_10 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_11 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_12 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_13 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_14 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_15 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_16 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_16 = MBY_STP_STATE_FORWARD;

    egress_mst_table_r * const egress_mst_table = &(rx_top_map->mst_glort.EGRESS_MST_TABLE[1][0]);

    egress_mst_table->FORWARDING = 0x3ffff;

    glort_direct_map_dst0_r * const map_dst0 = &(rx_top_map->mst_glort.GLORT_DIRECT_MAP_DST0);
    glort_direct_map_dst4_r * const map_dst4 = &(rx_top_map->mst_glort.GLORT_DIRECT_MAP_DST4);
    map_dst4->IP_MULTICAST_INDEX             = 0x1;
    map_dst0->DEST_MASK                      = 0x20;

    /* This part is to add DMAC to WCM. */
    map_profile_key0_r * const map_prof_key0 = &(rx_top_map->mapper.MAP_PROFILE_KEY0[1]);
    map_prof_key0->CSUM = 0x3;

    fm_uint profile  = 1;
    fm_uint hash_num = 0;

    map_profile_action_r * const prof_action = &(rx_top_map->mapper.MAP_PROFILE_ACTION[1]);
    prof_action->PROFILE_VALID = 1;
    prof_action->PROFILE       = profile;

    em_key_sel0_r * const key_sel0 = &(rx_top_map->cgrp_b.EM.KEY_SEL0[hash_num][profile]);
    key_sel0->KEY8_MASK = 0;

    em_key_sel1_r * const key_sel1 = &(rx_top_map->cgrp_b.EM.KEY_SEL1[hash_num][profile]);
    key_sel1->KEY16_MASK = 0x1c0;
    key_sel1->KEY32_MASK = 0;

    em_hash_cfg_r * const hash_cfg = &(rx_top_map->cgrp_b.EM.HASH_CFG[profile]);
    hash_cfg->MODE         = 0;
    hash_cfg->HASH_LO      = 0;
    hash_cfg->HASH_HI      = 0;
    hash_cfg->ENTRY_SIZE_0 = 8;

    fm_uint16 key16[MBY_CGRP_KEY16] = { 0 };
    fm_byte   key8 [MBY_CGRP_KEY8]  = { 0 };
    fm_uint32 key32[MBY_CGRP_KEY8]  = { 0 };

    key16[6] = (dmac >> 32) & 0xff;
    key16[7] = (dmac >> 16) & 0xff;
    key16[8] = dmac & 0xff;

    fm_byte hash_bytes[MBY_CGRP_HASH_KEYS] = { 0 };
    convertKeysToBytes(key32, key16, key8, hash_bytes);

    fm_uint32 hash = mbyCrc32ByteSwap(hash_bytes, MBY_CGRP_HASH_KEYS);

    fm_uint16 hash_mask  = 0x1fff;
    fm_uint16 hash_index = hash & hash_mask;
    fm_uint16 hash_more  = (hash >> 16) & 0xffff;
    fm_uint16 lookup_ptr = calculateLookupPtr(hash_index, hash_cfg);

    fm_uint16 hash_lookup_ptr = 0;

    em_hash_lookup_r * const hash_lookup = &(rx_top_map->cgrp_b.B.EM_HASH_LOOKUP[lookup_ptr]);
    hash_lookup->SELECT_0 = 1;
    hash_lookup->MASK     = 0x40000001;
    hash_lookup->PTR      = hash_lookup_ptr;

    fm_uint32 offset = 1;
    fm_uint32 hash_lookup_addr = (hash_lookup_ptr + offset * hash_cfg->ENTRY_SIZE_0) * 4;

    fm_uint32 entry_idx = (hash_lookup_addr >> 3) & 0xFFFF;
    fm_uint16 block     = entry_idx / fwd_table0_rf_FWD_TABLE0__n;
    fm_uint16 cell      = entry_idx % fwd_table0_rf_FWD_TABLE0__n;

    fwd_table1_r * fwd_table1 = &(shm_map->FWD_TABLE1[block][cell]);
    fwd_table1->DATA = dmac << 16;

    /* Set actions. */
    cell += 3;
    fwd_table1 = &(shm_map->FWD_TABLE1[block][cell]);

    FM_SET_FIELD(fwd_table1->DATA, MBY_CGRP_ACTION, SET1_24B_INDEX, 4);
    FM_SET_FIELD(fwd_table1->DATA, MBY_CGRP_ACTION, SET1_24B_VALUE, (0x100 + fwd_port));
    FM_SET_FIELD(fwd_table1->DATA, MBY_CGRP_ACTION, PREC, 1);
    FM_SET_UNNAMED_FIELD(fwd_table1->DATA, 28, 1, 1);
}
