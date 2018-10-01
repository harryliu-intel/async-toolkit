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
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_b_map * const cgrp_b_map,
#else
    fm_uint32                  regs[MBY_REGISTER_ARRAY_SIZE],
#endif
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
#ifdef USE_NEW_CSRS
            mbyClassifierTcamEntry tcam_entry = mbyClsGetWcmTcamEntry(cgrp_b_map, slice, tcam_index);
#else
            mbyClassifierTcamEntry tcam_entry = mbyClsGetWcmTcamEntry(regs,       slice, tcam_index);
#endif
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
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_b_map      * const cgrp_b_map,
#else
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
#endif
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
#ifdef USE_NEW_CSRS
        mbyClassifierTcamCfg tcam_cfg = mbyClsGetWcmTcamCfg(cgrp_b_map, group, slice, scenario);
#else
        mbyClassifierTcamCfg tcam_cfg = mbyClsGetWcmTcamCfg(regs,       group, slice, scenario);
#endif
        if (tcam_cfg.START_COMPARE)
            cascade_width[slice]++;

        for (fm_uint i = slice + 1; i < MBY_FFU_TCAM_CFG_ENTRIES_1; i++)
        {
#ifdef USE_NEW_CSRS
            mbyClassifierTcamCfg tcam_cfg1 = mbyClsGetWcmTcamCfg(cgrp_b_map, group, i, scenario);
#else
            mbyClassifierTcamCfg tcam_cfg1 = mbyClsGetWcmTcamCfg(regs,       group, i, scenario);
#endif
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

#ifdef USE_NEW_CSRS
        mbyClassifierTcamCfg tcam_cfg = mbyClsGetWcmTcamCfg(cgrp_b_map, group, slice, scenario);
#else
        mbyClassifierTcamCfg tcam_cfg = mbyClsGetWcmTcamCfg(regs,       group, slice, scenario);
#endif
        // Start Exclusion Set
        if (tcam_cfg.START_SET) {
            exclusion_set = TRUE;
            set_hit       = FALSE;
        }

        fm_bool hits[MBY_FFU_TCAM_ENTRIES_0] = { FALSE };

        for (fm_uint i = slice; i < slice + cascade_width[slice]; i++)
        {
#ifdef USE_NEW_CSRS
            mbyClassifierTcamCfg tcam_cfg1 = mbyClsGetWcmTcamCfg(cgrp_b_map, group, i, scenario);
#else
            mbyClassifierTcamCfg tcam_cfg1 = mbyClsGetWcmTcamCfg(regs,       group, i, scenario);
#endif
            // Select TCAM key:
            mbyWcmKeyInfo wcm_key_info = selectWcmKey(&tcam_cfg1, keys);

            // Look up in the TCAM and update raw hits with results of lookup:
#ifdef USE_NEW_CSRS
            lookUpWcmTcam(cgrp_b_map, i, tcam_cfg1.CHUNK_MASK, &wcm_key_info);
#else
            lookUpWcmTcam(regs,       i, tcam_cfg1.CHUNK_MASK, &wcm_key_info);
#endif
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
#ifdef USE_NEW_CSRS
    lookUpWcmTcamCascade(cgrp_b_map, keys, scenario, group, tcam_hit_info);
#else
    lookUpWcmTcamCascade(regs,       keys, scenario, group, tcam_hit_info);
#endif

#if 0 // FIXME move this to the top layer file: mby_classifier
// --------------------------------------------------------------------------------
    // Apply and resolve actions from action RAMs based on tcam hit index per slice:
#ifdef USE_NEW_CSRS
    resolveActions(cgrp_b_map, scenario, group, tcam_hit_info, actions);
#else
    resolveActions(regs,       scenario, group, tcam_hit_info, actions);
#endif
// --------------------------------------------------------------------------------
#endif
}
