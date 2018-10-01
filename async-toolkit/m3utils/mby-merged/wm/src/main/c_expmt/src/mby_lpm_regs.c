// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_bitfield.h"
#include "mby_lpm_regs.h"
#include "assert.h"

#ifdef USE_NEW_CSRS

void mbyLpmGetTcamEntry
(
    mby_ppe_cgrp_a_map     * const cgrp_a_map,
    fm_uint16                const index,
    mbyLpmTcamEntry        * const tcam_entry
)
{
    lpm_match_tcam_r const * lpm_match_tcam;

    assert(index < mby_ppe_cgrp_a_map_LPM_MATCH_TCAM__nd);
    assert(tcam_entry);

    lpm_match_tcam         = &(cgrp_a_map->LPM_MATCH_TCAM[index]);

    tcam_entry->key        = lpm_match_tcam->KEY;
    tcam_entry->key_invert = lpm_match_tcam->KEY_INVERT;
}

void mbyLpmGetTcamSubtrie
(
    mby_ppe_cgrp_a_map     * const cgrp_a_map,
    fm_uint16                const index,
    mbyLpmSubtrie          * const tcam_subtrie
)
{
    lpm_match_action_r const * lpm_match_action;

    assert(index < mby_ppe_cgrp_a_map_LPM_MATCH_ACTION__nd);
    assert(tcam_subtrie);

    lpm_match_action             = &(cgrp_a_map->LPM_MATCH_ACTION[index]);

    tcam_subtrie->child_ptr_len  = lpm_match_action->CHILD_PTR_LEN;
    tcam_subtrie->child_base_ptr = lpm_match_action->CHILD_BASE_PTR;
    tcam_subtrie->root_ptr       = lpm_match_action->ROOT_PTR;
}

void mbyLpmGetSubtrie
(
    mby_ppe_cgrp_a_map     * const cgrp_a_map,
    fm_uint16                const index,
    mbyLpmSubtrie          * const subtrie
)
{
    lpm_subtrie_cptr_r const * lpm_subtrie_cptr;

    assert(index < mby_ppe_cgrp_a_map_LPM_SUBTRIE_CPTR__nd);
    assert(subtrie);

    lpm_subtrie_cptr        = &(cgrp_a_map->LPM_SUBTRIE_CPTR[index]);

    subtrie->child_ptr_len  = lpm_subtrie_cptr->CHILD_PTR_LEN;
    subtrie->child_base_ptr = lpm_subtrie_cptr->CHILD_BASE_PTR;
    subtrie->root_ptr       = lpm_subtrie_cptr->SUBTRIE_PTR;

}

void mbyLpmGetSubtrieStore
(
    mby_ppe_cgrp_a_map     * const cgrp_a_map,
    fm_uint16                const index,
    mbyLpmSubtrieStore     * const st_store
)
{
    assert(index < mby_ppe_cgrp_a_map_LPM_SUBTRIE_BITMAPS__nd);
    assert(st_store);

    lpm_subtrie_bitmaps_rf *lpm_subtrie_bitmaps = &(cgrp_a_map->LPM_SUBTRIE_BITMAPS[index]);

    for (fm_uint i = 0; i < MBY_LPM_BITMAP_SIZE; ++i)
        st_store->prefix_bitmap[i] = lpm_subtrie_bitmaps[i]->BITMAP;

    for (fm_uint i = 0; i < MBY_LPM_BITMAP_SIZE; ++i)
        st_store->child_bitmap[i]  = lpm_subtrie_bitmaps[i + MBY_LPM_BITMAP_SIZE]->BITMAP;

    st_store->action_base_ptr = cgrp_a_map->LPM_SUBTRIE_APTR[index].ACTION_BASE_PTR;
}

#else /* HLP-like legacy register space */

void mbyLpmGetTcamEntry
(
    fm_uint32                      regs[MBY_REGISTER_ARRAY_SIZE],
    fm_uint16                const index,
    mbyLpmTcamEntry        * const tcam_entry
)
{
    /* FIXME set the 1st index of LPM_MATCH_TCAM[0..1][0..511] */
    const fm_uint16 idx0 = 0;
    fm_uint32 tcam_regs[MBY_LPM_MATCH_TCAM_WIDTH] = { 0 };

    assert(index < MBY_LPM_MATCH_TCAM_ENTRIES_0);
    assert(tcam_entry);

    mbyModelReadCSRMult(regs, MBY_LPM_MATCH_TCAM(idx0, index, 0),
                        MBY_LPM_MATCH_TCAM_WIDTH, tcam_regs);

    tcam_entry->key        = FM_ARRAY_GET_FIELD(tcam_regs, MBY_LPM_MATCH_TCAM, KEY);
    tcam_entry->key_invert = FM_ARRAY_GET_FIELD(tcam_regs, MBY_LPM_MATCH_TCAM, KEY_INVERT);
}

void mbyLpmGetTcamSubtrie
(
    fm_uint32                      regs[MBY_REGISTER_ARRAY_SIZE],
    fm_uint16                const index,
    mbyLpmSubtrie          * const tcam_subtrie
)
{
    /* FIXME set the 1st index of LPM_MATCH_ACTION[0..1][0..511] */
    const fm_uint16 idx0 = 0;
    fm_uint32 tcam_regs[MBY_LPM_MATCH_ACTION_WIDTH] = { 0 };

    assert(index < MBY_LPM_MATCH_ACTION_ENTRIES_0);
    assert(tcam_subtrie);

    mbyModelReadCSRMult(regs, MBY_LPM_MATCH_ACTION(idx0, index, 0),
                        MBY_LPM_MATCH_ACTION_WIDTH, tcam_regs);

    tcam_subtrie->child_ptr_len  = FM_ARRAY_GET_FIELD(tcam_regs, MBY_LPM_MATCH_ACTION, CHILD_PTR_LEN);
    tcam_subtrie->child_base_ptr = FM_ARRAY_GET_FIELD(tcam_regs, MBY_LPM_MATCH_ACTION, CHILD_BASE_PTR);
    tcam_subtrie->root_ptr       = FM_ARRAY_GET_FIELD(tcam_regs, MBY_LPM_MATCH_ACTION, ROOT_PTR);
}

void mbyLpmGetSubtrie
(
    fm_uint32                      regs[MBY_REGISTER_ARRAY_SIZE],
    fm_uint16                const index,
    mbyLpmSubtrie          * const subtrie
)
{
    /* FIXME set the 1st index of LPM_SUBTRIE_CPTR[0..1][0..24575] */
    const fm_uint16 idx0 = 0;
    fm_uint32 tcam_regs[MBY_LPM_SUBTRIE_CPTR_WIDTH] = { 0 };

    assert(index < MBY_LPM_SUBTRIE_CPTR_ENTRIES_0);
    assert(subtrie);

    mbyModelReadCSRMult(regs, MBY_LPM_SUBTRIE_CPTR(idx0, index, 0),
                        MBY_LPM_SUBTRIE_CPTR_WIDTH, tcam_regs);

    subtrie->child_ptr_len  = FM_ARRAY_GET_FIELD(tcam_regs, MBY_LPM_SUBTRIE_CPTR, CHILD_PTR_LEN);
    subtrie->child_base_ptr = FM_ARRAY_GET_FIELD(tcam_regs, MBY_LPM_SUBTRIE_CPTR, CHILD_BASE_PTR);
    subtrie->root_ptr       = FM_ARRAY_GET_FIELD(tcam_regs, MBY_LPM_SUBTRIE_CPTR, SUBTRIE_PTR);
}

void mbyLpmGetSubtrieStore
(
    fm_uint32                      regs[MBY_REGISTER_ARRAY_SIZE],
    fm_uint16                const index,
    mbyLpmSubtrieStore     * const st_store
)
{
    /* FIXME set the 1st index of LPM_SUBTRIE_BITMAPS[0..1][0..24575][0..15] */
    const fm_uint16 idx0 = 0;
    fm_uint32 st_regs[MBY_LPM_SUBTRIE_BITMAPS_WIDTH] = { 0 };

    assert(index < MBY_LPM_SUBTRIE_BITMAPS_ENTRIES_1);
    assert(st_store);

    for (fm_uint i = 0; i < MBY_LPM_BITMAP_SIZE; ++i)
    {
        mbyModelReadCSRMult(regs, MBY_LPM_SUBTRIE_BITMAPS(idx0, index, i, 0),
                            MBY_LPM_SUBTRIE_BITMAPS_WIDTH, st_regs);

        st_store->prefix_bitmap[i]  = FM_ARRAY_GET_FIELD64(st_regs, MBY_LPM_SUBTRIE_BITMAPS, BITMAP);
    }

    for (fm_uint i = 0; i < MBY_LPM_BITMAP_SIZE; ++i)
    {
        mbyModelReadCSRMult(regs, MBY_LPM_SUBTRIE_BITMAPS(idx0, index, i + MBY_LPM_BITMAP_SIZE, 0),
                            MBY_LPM_SUBTRIE_BITMAPS_WIDTH, st_regs);

        st_store->child_bitmap[i]  = FM_ARRAY_GET_FIELD64(st_regs, MBY_LPM_SUBTRIE_BITMAPS, BITMAP);
    }

    mbyModelReadCSRMult(regs, MBY_LPM_SUBTRIE_APTR(idx0, index, 0),
                        MBY_LPM_SUBTRIE_APTR_WIDTH, st_regs);

    st_store->action_base_ptr = FM_ARRAY_GET_FIELD(st_regs, MBY_LPM_SUBTRIE_APTR, ACTION_BASE_PTR);
}

#endif /* USE_NEW_CSRS */
