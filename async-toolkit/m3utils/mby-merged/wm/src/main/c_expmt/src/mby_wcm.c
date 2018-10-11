// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation


#include "mby_common.h"
#include "mby_wcm.h"

static mbyWcmKeyInfo selectWcmKey
(
    const mbyClassifierTcamCfg * const tcam_cfg,
    const mbyClassifierKeys    * const keys
)
{
    mbyWcmKeyInfo wcm_key_info = { 0 };

    fm_byte mux[5] = { 0 };
    mux[0] = tcam_cfg->SELECT0;
    mux[1] = tcam_cfg->SELECT1;
    mux[2] = tcam_cfg->SELECT2;
    mux[3] = tcam_cfg->SELECT3;
    mux[4] = tcam_cfg->SELECT_TOP + MBY_FFU_KEY16;

    wcm_key_info.key        = 0;
    wcm_key_info.key_invert = FM_LITERAL_U64(0xFFFFFFFFF);

    // Loop through the 5 bytes of key ({7:0}, {15:8}, {23,16}, {31:24}, {39:32})
    for (fm_uint i = 0; i < 5; i++)
    {
        // Set low bit:
        fm_uint lo_bit = 8 * i;

        // key8:
        fm_byte temp = 0;
        if ((mux[i] >= MBY_FFU_KEY8_BASE) && (mux[i] < (MBY_FFU_KEY8_BASE + MBY_FFU_KEY8)))
            temp = keys->key8[mux[i] - MBY_FFU_KEY8_BASE];

        // key16 and key32 are invalid for SelectTop:
        if (i >= 4)
            continue;

        // key16:
        if (mux[i] < (MBY_FFU_KEY16_BASE + MBY_FFU_KEY16))
            temp = FM_GET_UNNAMED_FIELD(keys->key16[mux[i] - MBY_FFU_KEY16_BASE], lo_bit % 16, 8);

        // key32:
        if ((mux[i] >= MBY_FFU_KEY32_BASE) && (mux[i] < (MBY_FFU_KEY32_BASE + MBY_FFU_KEY32)))
            temp = FM_GET_UNNAMED_FIELD(keys->key32[mux[i] - MBY_FFU_KEY32_BASE], lo_bit, 8);

        FM_SET_UNNAMED_FIELD64(wcm_key_info.key, lo_bit, 8, temp);
    }

    wcm_key_info.key_invert = ~wcm_key_info.key;

    return wcm_key_info;
}

static void lookUpWcmTcam
(
    MBY_CGRP_B_IN_REGS,
    fm_byte              const slice,
    fm_uint16            const chunk_mask,
    mbyWcmKeyInfo      * const wcm_key_info
)
{
    for (fm_uint i = 0; i < MBY_FFU_TCAM_ENTRIES_0; i++)
        wcm_key_info->raw_hits[i] = FALSE;

    fm_uint16 tcam_index = 0;

    while (tcam_index < MBY_FFU_TCAM_ENTRIES_0)
    {
        fm_byte tcam_chunk = tcam_index >> 6;

        if ((chunk_mask >> tcam_chunk) & 1)
        {
            mbyClassifierTcamEntry tcam_entry = mbyClsGetWcmTcamEntry(MBY_CGRP_B_IN_REGS_P, slice, tcam_index);

            fm_uint64 cam_key_inv = tcam_entry.KEY_INVERT;
            fm_uint64 cam_key     = tcam_entry.KEY;
            fm_uint64 mask        = cam_key ^ cam_key_inv;

            if (((cam_key & cam_key_inv) == 0) && ((wcm_key_info->key & mask) == (cam_key & mask)))
                wcm_key_info->raw_hits[tcam_index] = TRUE;

            tcam_index++;
        }
        else
        {
            tcam_index += 64;
        }
    }
}

static void lookUpWcmTcamCascade
(
    MBY_CGRP_B_IN_REGS,
    mbyClassifierKeys const * const keys,
    fm_byte                   const scenario,
    fm_byte                   const group,
    mbyClassifierHitInfo            tcam_hit_info[MBY_FFU_TCAM_CFG_ENTRIES_1]
)
{
    // --------------------------------------------------------------------------------
    // Compute cascade width:

    fm_byte cascade_width[MBY_FFU_TCAM_CFG_ENTRIES_1] = { 0 };

    for (fm_uint slice = 0; slice < MBY_FFU_TCAM_CFG_ENTRIES_1; slice++)
    {
        mbyClassifierTcamCfg tcam_cfg = mbyClsGetWcmTcamCfg(MBY_CGRP_B_IN_REGS_P, group, slice, scenario);

        if (tcam_cfg.START_COMPARE)
            cascade_width[slice]++;

        for (fm_uint i = slice + 1; i < MBY_FFU_TCAM_CFG_ENTRIES_1; i++)
        {
            mbyClassifierTcamCfg tcam_cfg1 = mbyClsGetWcmTcamCfg(MBY_CGRP_B_IN_REGS_P, group, i, scenario);

            if (tcam_cfg1.START_COMPARE)
                break;

            cascade_width[slice]++;
        }
    }

    // --------------------------------------------------------------------------------
    // Iterate through slices to look for a match:

    fm_bool fsc           = FALSE;
    fm_bool exclusion_set = FALSE;
    fm_bool set_hit       = FALSE;

    for (fm_uint slice = 0; slice < MBY_FFU_TCAM_CFG_ENTRIES_1; slice++)
    {
        if (cascade_width[slice] == 0)
            continue;

        mbyClassifierTcamCfg tcam_cfg = mbyClsGetWcmTcamCfg(MBY_CGRP_B_IN_REGS_P, group, slice, scenario);

        // Start Exclusion Set
        if (tcam_cfg.START_SET) {
            exclusion_set = TRUE;
            set_hit       = FALSE;
        }

        fm_bool hits[MBY_FFU_TCAM_ENTRIES_0] = { FALSE };

        for (fm_uint i = slice; i < slice + cascade_width[slice]; i++)
        {
            mbyClassifierTcamCfg tcam_cfg1 = mbyClsGetWcmTcamCfg(MBY_CGRP_B_IN_REGS_P, group, i, scenario);

            // Select TCAM key:
            mbyWcmKeyInfo wcm_key_info = selectWcmKey(&tcam_cfg1, keys);

            // Look up in the TCAM and update raw hits with results of lookup:
            lookUpWcmTcam(MBY_CGRP_B_IN_REGS_P, i, tcam_cfg1.CHUNK_MASK, &wcm_key_info);

            for (fm_uint j = 0; j < MBY_FFU_TCAM_ENTRIES_0; j++)
                hits[j] = (fsc || tcam_cfg1.START_COMPARE || hits[j]) && wcm_key_info.raw_hits[j];

            fsc = (i == slice) && tcam_cfg1.START_COMPARE && (tcam_cfg1.CHUNK_MASK == 0);
        }

        for (fm_int j = MBY_FFU_TCAM_ENTRIES_0 - 1; j >= 0; j--)
        {
            if (hits[j] && !set_hit) {
                fm_uint entry = slice + (cascade_width[slice] - 1);
                tcam_hit_info[entry].hit_index_valid = TRUE;
                tcam_hit_info[entry].hit_index       = j;
                if (exclusion_set == TRUE)
                    set_hit = TRUE;
                break;
            }
        }
    }
}

static void resolveActions
(
    MBY_CGRP_B_IN_REGS,
    fm_byte                const scenario,
    fm_byte                const group,
    mbyClassifierHitInfo         tcam_hit_info[MBY_FFU_TCAM_CFG_ENTRIES_1],
    mbyClassifierActions * const actions // = output actions
)
{
    for (fm_uint ram_num = 0; ram_num < MBY_FFU_ACTION_ENTRIES_1; ram_num++)
    {
        mbyClassifierActionCfg action_cfg = mbyClsGetWcmActionCfg(MBY_CGRP_B_IN_REGS_P, group, scenario, ram_num);

        if (!action_cfg.enable)
            continue; // skip action RAM if disabled

        fm_uint slice = action_cfg.slice;
        if (!tcam_hit_info[slice].hit_index_valid)
            continue;

        fm_uint hit_index = tcam_hit_info[slice].hit_index;
        for (fm_uint i = 0; i < MBY_FFU_ACTIONS_PER_ENTRY; i++) {
            fm_uint32 action_entry = mbyClsGetWcmActionEntry(MBY_CGRP_B_IN_REGS_P, ram_num, hit_index, i);
            // FIXME return only the list of action_entry
            // doAction(action_entry, actions);
        }
    }
}

void mbyMatchWildcard
(
    MBY_CGRP_B_IN_REGS,
    mbyClassifierKeys const * const keys,
    fm_byte                   const scenario,
    fm_byte                   const group,
    mbyClassifierActions    * const actions // = output actions
)
{
    mbyClassifierHitInfo tcam_hit_info[MBY_FFU_TCAM_CFG_ENTRIES_1];

    // Get hit index for each tcam slice:
    lookUpWcmTcamCascade(MBY_CGRP_B_IN_REGS_P, keys, scenario, group, tcam_hit_info);

    // Apply and resolve actions from action RAMs based on tcam hit index per slice:
    resolveActions(MBY_CGRP_B_IN_REGS_P, scenario, group, tcam_hit_info, actions);
}
