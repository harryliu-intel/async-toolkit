// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_bitfield.h"
#include "mby_lpm_regs.h"
#include "assert.h"


void mbyLpmGetKeySels
(
    mby_ppe_cgrp_a_map const * const cgrp_a_map,
    fm_byte                    const profile_id,
    mbyLpmKeySels            * const key_sels
)
{
    assert(profile_id < 64);
    assert(key_sels);

    for (fm_uint i = 0; i < MBY_LPM_KEY_MAX_BYTES_LEN; ++i)
        key_sels->key_mask[i] = cgrp_a_map->A.LPM_KEY_MASK[profile_id][i].MASK;

    key_sels->md_key16_sel   = cgrp_a_map->A.LPM_KEY_SEL0[profile_id].MD_KEY16_SEL;

    key_sels->addr_key8_sel  = cgrp_a_map->A.LPM_KEY_SEL1[profile_id].ADDR_KEY8_SEL;
    key_sels->md_key8_sel    = cgrp_a_map->A.LPM_KEY_SEL1[profile_id].MD_KEY8_SEL;

    key_sels->addr_key16_sel = cgrp_a_map->A.LPM_KEY_SEL2[profile_id].ADDR_KEY16_SEL;

    key_sels->addr_key32_sel = cgrp_a_map->A.LPM_KEY_SEL3[profile_id].ADDR_KEY32_SEL;
}

void mbyLpmGetTcamEntry
(
    mby_ppe_cgrp_a_map const * const cgrp_a_map,
    fm_uint16                  const index,
    mbyLpmTcamEntry          * const tcam_entry
)
{
    lpm_match_tcam_r const * lpm_match_tcam;

    assert(index < mby_ppe_cgrp_a_nested_map_LPM_MATCH_TCAM__nd);
    assert(tcam_entry);

    lpm_match_tcam         = &(cgrp_a_map->A.LPM_MATCH_TCAM[index]);

    tcam_entry->key        = lpm_match_tcam->KEY;
    tcam_entry->key_invert = lpm_match_tcam->KEY_INVERT;
}

void mbyLpmGetTcamSubtrie
(
    mby_ppe_cgrp_a_map const * const cgrp_a_map,
    fm_uint16                  const index,
    mbyLpmSubtrie            * const tcam_subtrie
)
{
    lpm_match_action_r const * lpm_match_action;

    assert(index < mby_ppe_cgrp_a_nested_map_LPM_MATCH_ACTION__nd);
    assert(tcam_subtrie);

    lpm_match_action             = &(cgrp_a_map->A.LPM_MATCH_ACTION[index]);

    tcam_subtrie->child_ptr_len  = lpm_match_action->CHILD_PTR_LEN;
    tcam_subtrie->child_base_ptr = lpm_match_action->CHILD_BASE_PTR;
    tcam_subtrie->root_ptr       = lpm_match_action->ROOT_PTR;
}

void mbyLpmGetSubtrie
(
    mby_ppe_cgrp_a_map const * const cgrp_a_map,
    fm_uint16                  const bank_index,
    fm_uint16                  const entry_index,
    mbyLpmSubtrie            * const subtrie
)
{
    lpm_subtrie_cptr_r const * lpm_subtrie_cptr;

    assert(bank_index < mby_ppe_cgrp_a_nested_map_LPM_SUBTRIE_CPTR__n);
    assert(entry_index < lpm_subtrie_cptr_rf_LPM_SUBTRIE_CPTR__n);
    assert(subtrie);

    lpm_subtrie_cptr        = &(cgrp_a_map->A.LPM_SUBTRIE_CPTR[bank_index][entry_index]);

    subtrie->child_ptr_len  = lpm_subtrie_cptr->CHILD_PTR_LEN;
    subtrie->child_base_ptr = lpm_subtrie_cptr->CHILD_BASE_PTR;
    subtrie->root_ptr       = lpm_subtrie_cptr->SUBTRIE_PTR;

}

void mbyLpmGetSubtrieStore
(
    mby_ppe_cgrp_a_map const * const cgrp_a_map,
    fm_uint16                  const bank_index,
    fm_uint16                  const entry_index,
    mbyLpmSubtrieStore       * const st_store
)
{
    assert(bank_index < mby_ppe_cgrp_a_nested_map_LPM_SUBTRIE_APTR__n);
    assert(entry_index < lpm_subtrie_aptr_rf_LPM_SUBTRIE_APTR__n);
    assert(st_store);

    lpm_subtrie_bitmaps_rf const * lpm_subtrie_bitmaps;
    lpm_subtrie_bitmaps = &(cgrp_a_map->A.LPM_SUBTRIE_BITMAPS[bank_index]);

    for (fm_uint16 i = 0; i < MBY_LPM_BITMAP_SIZE; ++i)
    {
        fm_uint16 bm_index = (entry_index * MBY_LPM_COLUMN_SIZE) + i;
        st_store->prefix_bitmap[i] = lpm_subtrie_bitmaps[bm_index]->BITMAP;
    }
    for (fm_uint16 i = 0; i < MBY_LPM_BITMAP_SIZE; ++i)
    {
        fm_uint16 bm_index = (entry_index * MBY_LPM_COLUMN_SIZE) + i + MBY_LPM_BITMAP_SIZE;
        st_store->child_bitmap[i] = lpm_subtrie_bitmaps[bm_index]->BITMAP;
    }

    st_store->action_base_ptr = cgrp_a_map->A.LPM_SUBTRIE_APTR[bank_index][entry_index].ACTION_BASE_PTR;
}

