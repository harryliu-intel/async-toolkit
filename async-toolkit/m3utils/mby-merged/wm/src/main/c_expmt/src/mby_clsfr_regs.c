// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_clsfr_regs.h"

mbyClassifierKeyMaskCfg mbyClsGetEmAKeyMaskCfg
(
    MBY_CGRP_A_IN_REGS,
    fm_byte              const hash_num,
    fm_byte              const scenario
)
{
    mbyClassifierKeyMaskCfg key_mask_cfg;

#ifdef USE_NEW_CSRS
    key_mask_cfg.KEY_MASK_SEL = cgrp_a_map->EM_A_KEY_SEL1[hash_num][scenario].KEY_MASK_SEL; // [51:48]
    key_mask_cfg.KEY32_MASK   = cgrp_a_map->EM_A_KEY_SEL1[hash_num][scenario].KEY32_MASK;   // [47:32]
    key_mask_cfg.KEY16_MASK   = cgrp_a_map->EM_A_KEY_SEL1[hash_num][scenario].KEY16_MASK;   // [31: 0]
    key_mask_cfg.KEY8_MASK    = cgrp_a_map->EM_A_KEY_SEL0[hash_num][scenario].KEY8_MASK;    // [31: 0]
#else
    fm_byte const group = 0; // Exact Match "A"

    fm_uint64 ffu_key_mask0_reg = 0;
    fm_uint64 ffu_key_mask1_reg = 0;

    mbyModelReadCSR64(regs, MBY_FFU_KEY_MASK0(group, hash_num, scenario, 0), &ffu_key_mask0_reg);
    mbyModelReadCSR64(regs, MBY_FFU_KEY_MASK1(group, hash_num, scenario, 0), &ffu_key_mask1_reg);

    key_mask_cfg.KEY_MASK_SEL = 0; // <--- REVISIT!!!!
    key_mask_cfg.KEY32_MASK   = FM_GET_FIELD64(ffu_key_mask1_reg, MBY_FFU_KEY_MASK1, KEY32_MASK);
    key_mask_cfg.KEY16_MASK   = FM_GET_FIELD64(ffu_key_mask1_reg, MBY_FFU_KEY_MASK1, KEY16_MASK);
    key_mask_cfg.KEY8_MASK    = FM_GET_FIELD64(ffu_key_mask0_reg, MBY_FFU_KEY_MASK0, KEY8_MASK);
#endif
    return key_mask_cfg;
}

mbyClassifierKeyMaskCfg mbyClsGetEmBKeyMaskCfg
(
    MBY_CGRP_B_IN_REGS,
    fm_byte              const hash_num,
    fm_byte              const scenario
)
{
    mbyClassifierKeyMaskCfg key_mask_cfg;

#ifdef USE_NEW_CSRS
    key_mask_cfg.KEY_MASK_SEL = cgrp_b_map->EM_B_KEY_SEL1[hash_num][scenario].KEY_MASK_SEL; // [51:48]
    key_mask_cfg.KEY32_MASK   = cgrp_b_map->EM_B_KEY_SEL1[hash_num][scenario].KEY32_MASK;   // [47:32]
    key_mask_cfg.KEY16_MASK   = cgrp_b_map->EM_B_KEY_SEL1[hash_num][scenario].KEY16_MASK;   // [31: 0]
    key_mask_cfg.KEY8_MASK    = cgrp_b_map->EM_B_KEY_SEL0[hash_num][scenario].KEY8_MASK;    // [31: 0]
#else
    fm_byte const group = 1; // Exact Match "B"

    fm_uint64 ffu_key_mask0_reg = 0;
    fm_uint64 ffu_key_mask1_reg = 0;

    mbyModelReadCSR64(regs, MBY_FFU_KEY_MASK0(group, hash_num, scenario, 0), &ffu_key_mask0_reg);
    mbyModelReadCSR64(regs, MBY_FFU_KEY_MASK1(group, hash_num, scenario, 0), &ffu_key_mask1_reg);

    key_mask_cfg.KEY_MASK_SEL = 0; // <--- REVISIT!!!!
    key_mask_cfg.KEY32_MASK   = FM_GET_FIELD64(ffu_key_mask1_reg, MBY_FFU_KEY_MASK1, KEY32_MASK);
    key_mask_cfg.KEY16_MASK   = FM_GET_FIELD64(ffu_key_mask1_reg, MBY_FFU_KEY_MASK1, KEY16_MASK);
    key_mask_cfg.KEY8_MASK    = FM_GET_FIELD64(ffu_key_mask0_reg, MBY_FFU_KEY_MASK0, KEY8_MASK);
#endif
    return key_mask_cfg;
}

mbyClassifierHashCfg mbyClsGetEmAHashCfg
(
    MBY_CGRP_A_IN_REGS,
    fm_byte              const scenario
)
{
    mbyClassifierHashCfg hash_cfg;

#ifdef USE_NEW_CSRS
    em_a_hash_cfg_r const * const em_a_hash_cfg = &(cgrp_a_map->EM_A_HASH_CFG[scenario]);

    hash_cfg.mode          = em_a_hash_cfg->MODE;
    hash_cfg.base_ptr[0]   = em_a_hash_cfg->BASE_PTR_0;
    hash_cfg.base_ptr[1]   = em_a_hash_cfg->BASE_PTR_1;
    hash_cfg.hash_size[0]  = em_a_hash_cfg->HASH_SIZE_0;
    hash_cfg.hash_size[1]  = em_a_hash_cfg->HASH_SIZE_1;
    hash_cfg.entry_size[0] = em_a_hash_cfg->ENTRY_SIZE_0;
    hash_cfg.entry_size[1] = em_a_hash_cfg->ENTRY_SIZE_1;
#else
    fm_byte const group = 0; // Exact Match "A"
    fm_uint64 ffu_hash_cfg_reg = 0;
    mbyModelReadCSR64(regs, MBY_FFU_HASH_CFG(group, scenario, 0), &ffu_hash_cfg_reg);

    hash_cfg.mode          = FM_GET_BIT64  (ffu_hash_cfg_reg, MBY_FFU_HASH_CFG, MODE);
    hash_cfg.base_ptr[0]   = FM_GET_FIELD64(ffu_hash_cfg_reg, MBY_FFU_HASH_CFG, BASE_PTR_0);
    hash_cfg.base_ptr[1]   = FM_GET_FIELD64(ffu_hash_cfg_reg, MBY_FFU_HASH_CFG, BASE_PTR_1);
    hash_cfg.hash_size[0]  = FM_GET_FIELD64(ffu_hash_cfg_reg, MBY_FFU_HASH_CFG, HASH_SIZE_0);
    hash_cfg.hash_size[1]  = FM_GET_FIELD64(ffu_hash_cfg_reg, MBY_FFU_HASH_CFG, HASH_SIZE_1);
    hash_cfg.entry_size[0] = FM_GET_FIELD64(ffu_hash_cfg_reg, MBY_FFU_HASH_CFG, ENTRY_SIZE_0);
    hash_cfg.entry_size[1] = FM_GET_FIELD64(ffu_hash_cfg_reg, MBY_FFU_HASH_CFG, ENTRY_SIZE_1);
#endif
    return hash_cfg;
}

mbyClassifierHashCfg mbyClsGetEmBHashCfg
(
    MBY_CGRP_B_IN_REGS,
    fm_byte              const scenario
)
{
    mbyClassifierHashCfg hash_cfg;

#ifdef USE_NEW_CSRS
    em_b_hash_cfg_r const * const em_b_hash_cfg = &(cgrp_b_map->EM_B_HASH_CFG[scenario]);

    hash_cfg.mode          = em_b_hash_cfg->MODE;
    hash_cfg.base_ptr[0]   = em_b_hash_cfg->BASE_PTR_0;
    hash_cfg.base_ptr[1]   = em_b_hash_cfg->BASE_PTR_1;
    hash_cfg.hash_size[0]  = em_b_hash_cfg->HASH_SIZE_0;
    hash_cfg.hash_size[1]  = em_b_hash_cfg->HASH_SIZE_1;
    hash_cfg.entry_size[0] = em_b_hash_cfg->ENTRY_SIZE_0;
    hash_cfg.entry_size[1] = em_b_hash_cfg->ENTRY_SIZE_1;
#else
    fm_byte const group = 1; // Exact Match "B"
    fm_uint64 ffu_hash_cfg_reg = 0;
    mbyModelReadCSR64(regs, MBY_FFU_HASH_CFG(group, scenario, 0), &ffu_hash_cfg_reg);

    hash_cfg.mode          = FM_GET_BIT64  (ffu_hash_cfg_reg, MBY_FFU_HASH_CFG, MODE);
    hash_cfg.base_ptr[0]   = FM_GET_FIELD64(ffu_hash_cfg_reg, MBY_FFU_HASH_CFG, BASE_PTR_0);
    hash_cfg.base_ptr[1]   = FM_GET_FIELD64(ffu_hash_cfg_reg, MBY_FFU_HASH_CFG, BASE_PTR_1);
    hash_cfg.hash_size[0]  = FM_GET_FIELD64(ffu_hash_cfg_reg, MBY_FFU_HASH_CFG, HASH_SIZE_0);
    hash_cfg.hash_size[1]  = FM_GET_FIELD64(ffu_hash_cfg_reg, MBY_FFU_HASH_CFG, HASH_SIZE_1);
    hash_cfg.entry_size[0] = FM_GET_FIELD64(ffu_hash_cfg_reg, MBY_FFU_HASH_CFG, ENTRY_SIZE_0);
    hash_cfg.entry_size[1] = FM_GET_FIELD64(ffu_hash_cfg_reg, MBY_FFU_HASH_CFG, ENTRY_SIZE_1);
#endif
    return hash_cfg;
}

mbyClassifierHashLookup mbyClsGetEmAHashLookupEntry
(
    MBY_CGRP_A_IN_REGS,
    fm_uint16            const lookup_ptr
)
{
    mbyClassifierHashLookup lookup_entry;

#ifdef USE_NEW_CSRS
    em_a_hash_lookup_r const * const em_a_hash_lookup_entry = &(cgrp_a_map->EM_A_HASH_LOOKUP[lookup_ptr]);

    lookup_entry.PTR      = em_a_hash_lookup_entry->PTR;
    lookup_entry.SELECT_4 = em_a_hash_lookup_entry->SELECT_4;
    lookup_entry.SELECT_3 = em_a_hash_lookup_entry->SELECT_3;
    lookup_entry.SELECT_2 = em_a_hash_lookup_entry->SELECT_2;
    lookup_entry.SELECT_1 = em_a_hash_lookup_entry->SELECT_1;
    lookup_entry.SELECT_0 = em_a_hash_lookup_entry->SELECT_0;
    lookup_entry.MASK     = em_a_hash_lookup_entry->MASK;
#else
    fm_byte const group = 0; // Exact Match "A"
    fm_uint32 ffu_hash_lookup_regs[MBY_FFU_HASH_LOOKUP_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_FFU_HASH_LOOKUP(group, lookup_ptr, 0), MBY_FFU_HASH_LOOKUP_WIDTH, ffu_hash_lookup_regs);

    lookup_entry.PTR      = FM_ARRAY_GET_FIELD(ffu_hash_lookup_regs, MBY_FFU_HASH_LOOKUP, PTR);
    lookup_entry.SELECT_4 = FM_ARRAY_GET_FIELD(ffu_hash_lookup_regs, MBY_FFU_HASH_LOOKUP, SELECT_4);
    lookup_entry.SELECT_3 = FM_ARRAY_GET_FIELD(ffu_hash_lookup_regs, MBY_FFU_HASH_LOOKUP, SELECT_3);
    lookup_entry.SELECT_2 = FM_ARRAY_GET_FIELD(ffu_hash_lookup_regs, MBY_FFU_HASH_LOOKUP, SELECT_2);
    lookup_entry.SELECT_1 = FM_ARRAY_GET_FIELD(ffu_hash_lookup_regs, MBY_FFU_HASH_LOOKUP, SELECT_1);
    lookup_entry.SELECT_0 = FM_ARRAY_GET_FIELD(ffu_hash_lookup_regs, MBY_FFU_HASH_LOOKUP, SELECT_0);
    lookup_entry.MASK     = FM_ARRAY_GET_FIELD(ffu_hash_lookup_regs, MBY_FFU_HASH_LOOKUP, MASK);
#endif
    return lookup_entry;
}

mbyClassifierHashLookup mbyClsGetEmBHashLookupEntry
(
    MBY_CGRP_B_IN_REGS,
    fm_uint16            const lookup_ptr
)
{
    mbyClassifierHashLookup lookup_entry;

#ifdef USE_NEW_CSRS
    em_b_hash_lookup_r const * const em_b_hash_lookup_entry = &(cgrp_b_map->EM_B_HASH_LOOKUP[lookup_ptr]);

    lookup_entry.PTR      = em_b_hash_lookup_entry->PTR;
    lookup_entry.SELECT_4 = em_b_hash_lookup_entry->SELECT_4;
    lookup_entry.SELECT_3 = em_b_hash_lookup_entry->SELECT_3;
    lookup_entry.SELECT_2 = em_b_hash_lookup_entry->SELECT_2;
    lookup_entry.SELECT_1 = em_b_hash_lookup_entry->SELECT_1;
    lookup_entry.SELECT_0 = em_b_hash_lookup_entry->SELECT_0;
    lookup_entry.MASK     = em_b_hash_lookup_entry->MASK;
#else
    fm_byte const group = 1; // Exact Match "B"
    fm_uint32 ffu_hash_lookup_regs[MBY_FFU_HASH_LOOKUP_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_FFU_HASH_LOOKUP(group, lookup_ptr, 0), MBY_FFU_HASH_LOOKUP_WIDTH, ffu_hash_lookup_regs);

    lookup_entry.PTR      = FM_ARRAY_GET_FIELD(ffu_hash_lookup_regs, MBY_FFU_HASH_LOOKUP, PTR);
    lookup_entry.SELECT_4 = FM_ARRAY_GET_FIELD(ffu_hash_lookup_regs, MBY_FFU_HASH_LOOKUP, SELECT_4);
    lookup_entry.SELECT_3 = FM_ARRAY_GET_FIELD(ffu_hash_lookup_regs, MBY_FFU_HASH_LOOKUP, SELECT_3);
    lookup_entry.SELECT_2 = FM_ARRAY_GET_FIELD(ffu_hash_lookup_regs, MBY_FFU_HASH_LOOKUP, SELECT_2);
    lookup_entry.SELECT_1 = FM_ARRAY_GET_FIELD(ffu_hash_lookup_regs, MBY_FFU_HASH_LOOKUP, SELECT_1);
    lookup_entry.SELECT_0 = FM_ARRAY_GET_FIELD(ffu_hash_lookup_regs, MBY_FFU_HASH_LOOKUP, SELECT_0);
    lookup_entry.MASK     = FM_ARRAY_GET_FIELD(ffu_hash_lookup_regs, MBY_FFU_HASH_LOOKUP, MASK);
#endif
    return lookup_entry;
}

fm_uint64 mbyClsGetEmAHashCamEntry
(
    MBY_CGRP_A_IN_REGS,
    fm_uint32            const entry,
    fm_uint32            const word
)
{
#ifdef USE_NEW_CSRS
    em_a_hash_cam_r const * const em_a_hash_cam = &(cgrp_a_map->EM_A_HASH_CAM[entry][word]);

    fm_uint64 data = em_a_hash_cam->DATA;
#else
    fm_byte const group = 0; // Exact Match "A"
    fm_uint64 ffu_hash_cam_reg = 0;
    mbyModelReadCSR64(regs, MBY_FFU_HASH_CAM(group, entry, word, 0), &ffu_hash_cam_reg);

    fm_uint64 data = FM_GET_FIELD64(ffu_hash_cam_reg, MBY_FFU_HASH_CAM, DATA);
#endif
    return data;
}

fm_uint64 mbyClsGetEmBHashCamEntry
(
    MBY_CGRP_B_IN_REGS,
    fm_uint32            const entry,
    fm_uint32            const word
)
{
#ifdef USE_NEW_CSRS
    em_b_hash_cam_r const * const em_b_hash_cam_entry = &(cgrp_b_map->EM_B_HASH_CAM[entry][word]);

    fm_uint64 data = em_b_hash_cam_entry->DATA;
#else
    fm_byte const group = 1; // Exact Match "B"
    fm_uint64 ffu_hash_cam_reg = 0;
    mbyModelReadCSR64(regs, MBY_FFU_HASH_CAM(group, entry, word, 0), &ffu_hash_cam_reg);

    fm_uint64 data = FM_GET_FIELD64(ffu_hash_cam_reg, MBY_FFU_HASH_CAM, DATA);
#endif
    return data;
}

fm_uint64 mbyClsGetEmAHashCamMask
(
    MBY_CGRP_A_IN_REGS,
    fm_uint32            const row,
    fm_uint32            const rule
)
{
#ifdef USE_NEW_CSRS
    em_a_hash_cam_en_r const * const em_a_hash_cam_en = &(cgrp_a_map->EM_A_HASH_CAM_EN[row][rule]);

    fm_uint64 mask = em_a_hash_cam_en->MASK;
#else
    fm_byte const group = 0; // Exact Match "A"
    fm_uint64 ffu_hash_cam_en_reg = 0;
    mbyModelReadCSR64(regs, MBY_FFU_HASH_CAM_EN(group, row, rule, 0), &ffu_hash_cam_en_reg);

    fm_uint64 mask = FM_GET_FIELD64(ffu_hash_cam_en_reg, MBY_FFU_HASH_CAM_EN, MASK);
#endif
    return mask;
}

fm_uint64 mbyClsGetEmBHashCamMask
(
    MBY_CGRP_B_IN_REGS,
    fm_uint32            const row,
    fm_uint32            const rule
)
{
#ifdef USE_NEW_CSRS
    em_b_hash_cam_en_r const * const em_b_hash_cam_en = &(cgrp_b_map->EM_B_HASH_CAM_EN[row][rule]);

    fm_uint64 mask = em_b_hash_cam_en->MASK;
#else
    fm_byte const group = 0; // Exact Match "A"
    fm_uint64 ffu_hash_cam_en_reg = 0;
    mbyModelReadCSR64(regs, MBY_FFU_HASH_CAM_EN(group, row, rule, 0), &ffu_hash_cam_en_reg);

    fm_uint64 mask = FM_GET_FIELD64(ffu_hash_cam_en_reg, MBY_FFU_HASH_CAM_EN, MASK);
#endif
    return mask;
}

fm_uint64 mbyClsGetEmHashEntryRam
(
#ifdef USE_NEW_CSRS
    // REVISIT!!!
#else
    fm_uint32       regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    fm_uint32 const hash_num,
    fm_uint32 const hash_ram_addr
)
{
    fm_uint64 hash_entry = 0;

#ifdef USE_NEW_CSRS


#else
    if (hash_num == 0) {
        fm_uint64 hash_entry0_reg = 0;
        mbyModelReadCSR64(regs, MBY_HASH_ENTRY0(hash_ram_addr, 0), &hash_entry0_reg);
        hash_entry = FM_GET_FIELD64(hash_entry0_reg, MBY_HASH_ENTRY0, DATA);
    }
    else {
        fm_uint64 hash_entry1_reg = 0;
        mbyModelReadCSR64(regs, MBY_HASH_ENTRY1(hash_ram_addr, 0), &hash_entry1_reg);
        hash_entry = FM_GET_FIELD64(hash_entry1_reg, MBY_HASH_ENTRY1, DATA);
    }
#endif
    return hash_entry;
}

fm_byte mbyClsGetEmHashRamAlloc
(
#ifdef USE_NEW_CSRS
    // REVISIT!!!
#else
    fm_uint32                  regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    fm_uint32            const entry
)
{
    fm_byte ram_alloc = 0;

#ifdef USE_NEW_CSRS
    // repaced by shared fwd. memory <-- FIXME!!!
#else
    fm_uint64 hash_entry_ram_alloc_reg = 0;
    mbyModelReadCSR64(regs, MBY_HASH_ENTRY_RAM_ALLOC(entry, 0), &hash_entry_ram_alloc_reg);
    ram_alloc = FM_GET_FIELD64(hash_entry_ram_alloc_reg, MBY_HASH_ENTRY_RAM_ALLOC, GP_SEL);
#endif
    return ram_alloc;
}

void mbyClsGetEmHashMissActions
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_a_map      * const cgrp_a_map,
    mby_ppe_cgrp_b_map      * const cgrp_b_map,
#else
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    fm_byte                   const group,
    mbyClassifierHashCfg      const hash_cfg,
    fm_uint32                 const hash_num,
    fm_byte                   const scenario,
    fm_uint32                       hash_actions[MBY_FFU_MAX_HASH_ACTIONS]
)
{
#ifdef USE_NEW_CSRS
    for (fm_uint i = 0; i < MBY_FFU_MAX_HASH_ACTIONS; i++)
        hash_actions[i] = 0; // wrong <-- FIXME!!!
#else

    fm_uint64 ffu_hash_miss_reg = 0;
    mbyModelReadCSR64(regs, MBY_FFU_HASH_MISS(group, hash_num, scenario, 0), &ffu_hash_miss_reg);
    hash_actions[0] = FM_GET_FIELD64(ffu_hash_miss_reg, MBY_FFU_HASH_MISS, ACTION0);
    hash_actions[1] = FM_GET_FIELD64(ffu_hash_miss_reg, MBY_FFU_HASH_MISS, ACTION1);

    if (hash_cfg.mode == MBY_FFU_HASH_ENTRY_MODE_64B) {
        mbyModelReadCSR64(regs, MBY_FFU_HASH_MISS(group, 1, scenario, 0), &ffu_hash_miss_reg);
        hash_actions[2] = FM_GET_FIELD64(ffu_hash_miss_reg, MBY_FFU_HASH_MISS, ACTION0);
        hash_actions[3] = FM_GET_FIELD64(ffu_hash_miss_reg, MBY_FFU_HASH_MISS, ACTION1);
    }
#endif
}

mbyClassifierTcamCfg mbyClsGetWcmTcamCfg
(
    MBY_CGRP_B_IN_REGS,
    fm_byte              const group,
    fm_byte              const slice,
    fm_byte              const scenario
)
{
    mbyClassifierTcamCfg tcam_cfg;

#ifdef USE_NEW_CSRS
    wcm_tcam_cfg_r const * const wcm_tcam_cfg = &(cgrp_b_map->WCM_TCAM_CFG[slice][scenario]);

    tcam_cfg.CHUNK_MASK    = wcm_tcam_cfg->CHUNK_MASK;
    tcam_cfg.START_COMPARE = wcm_tcam_cfg->START_COMPARE;
    tcam_cfg.START_SET     = wcm_tcam_cfg->START_SET;
    tcam_cfg.SELECT_TOP    = wcm_tcam_cfg->SELECT_TOP;
    tcam_cfg.SELECT0       = wcm_tcam_cfg->SELECT0;
    tcam_cfg.SELECT1       = wcm_tcam_cfg->SELECT1;
    tcam_cfg.SELECT2       = wcm_tcam_cfg->SELECT2;
    tcam_cfg.SELECT3       = wcm_tcam_cfg->SELECT3;
#else
    fm_uint64 ffu_tcam_cfg_reg = 0;
    mbyModelReadCSR64(regs, MBY_FFU_TCAM_CFG(group, slice, scenario, 0), &ffu_tcam_cfg_reg);

    tcam_cfg.CHUNK_MASK    = FM_GET_FIELD64(ffu_tcam_cfg_reg, MBY_FFU_TCAM_CFG, CHUNK_MASK);
    tcam_cfg.START_COMPARE = FM_GET_BIT64  (ffu_tcam_cfg_reg, MBY_FFU_TCAM_CFG, START_COMPARE);
    tcam_cfg.START_SET     = FM_GET_BIT64  (ffu_tcam_cfg_reg, MBY_FFU_TCAM_CFG, START_SET);
    tcam_cfg.SELECT_TOP    = FM_GET_FIELD64(ffu_tcam_cfg_reg, MBY_FFU_TCAM_CFG, SELECT_TOP);
    tcam_cfg.SELECT0       = FM_GET_FIELD64(ffu_tcam_cfg_reg, MBY_FFU_TCAM_CFG, SELECT0);
    tcam_cfg.SELECT1       = FM_GET_FIELD64(ffu_tcam_cfg_reg, MBY_FFU_TCAM_CFG, SELECT1);
    tcam_cfg.SELECT2       = FM_GET_FIELD64(ffu_tcam_cfg_reg, MBY_FFU_TCAM_CFG, SELECT2);
    tcam_cfg.SELECT3       = FM_GET_FIELD64(ffu_tcam_cfg_reg, MBY_FFU_TCAM_CFG, SELECT3);
#endif
    return tcam_cfg;
}

mbyClassifierTcamEntry mbyClsGetWcmTcamEntry
(
    MBY_CGRP_B_IN_REGS,
    fm_byte              const slice,
    fm_uint16            const index
)
{
    mbyClassifierTcamEntry tcam_entry;

#ifdef USE_NEW_CSRS
    wcm_tcam_r const * const wcm_tcam_entry = &(cgrp_b_map->WCM_TCAM[slice][index]);

    fm_uint64 key_top     = ((fm_uint64) wcm_tcam_entry->KEY_TOP)        << 32;
    fm_uint64 key_top_inv = ((fm_uint64) wcm_tcam_entry->KEY_TOP_INVERT) << 32;

    tcam_entry.KEY        = wcm_tcam_entry->KEY        | key_top;
    tcam_entry.KEY_INVERT = wcm_tcam_entry->KEY_INVERT | key_top_inv;
#else
    fm_byte const group = 1; // group "B"

    fm_uint32 ffu_tcam_regs[MBY_FFU_TCAM_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_FFU_TCAM(group, slice, index, 0), MBY_FFU_TCAM_WIDTH, ffu_tcam_regs);

    tcam_entry.KEY        = FM_ARRAY_GET_FIELD64(ffu_tcam_regs, MBY_FFU_TCAM, KEY) |
                            FM_ARRAY_GET_FIELD64(ffu_tcam_regs, MBY_FFU_TCAM, KEY_TOP) << 32;

    tcam_entry.KEY_INVERT = FM_ARRAY_GET_FIELD64(ffu_tcam_regs, MBY_FFU_TCAM, KEY_INVERT) |
                            FM_ARRAY_GET_FIELD64(ffu_tcam_regs, MBY_FFU_TCAM, KEY_TOP_INVERT) << 32;
#endif
    return tcam_entry;
}

mbyClassifierActionCfg mbyClsGetWcmActionCfg
(
    MBY_CGRP_B_IN_REGS,
    fm_byte              const group,
    fm_byte              const scenario,
    fm_byte              const ram_num
)
{
    mbyClassifierActionCfg action_cfg;
#ifdef USE_NEW_CSRS
    wcm_action_cfg_r const * const wcm_action_cfg = &(cgrp_b_map->WCM_ACTION_CFG[ram_num]);

    fm_bool enable = 0;
    fm_byte index  = 0;

    switch (ram_num)
    {
        case  0: enable = wcm_action_cfg->ENABLE_0;  index = wcm_action_cfg->INDEX_0;  break;
        case  1: enable = wcm_action_cfg->ENABLE_1;  index = wcm_action_cfg->INDEX_1;  break;
        case  2: enable = wcm_action_cfg->ENABLE_2;  index = wcm_action_cfg->INDEX_2;  break;
        case  3: enable = wcm_action_cfg->ENABLE_3;  index = wcm_action_cfg->INDEX_3;  break;
        case  4: enable = wcm_action_cfg->ENABLE_4;  index = wcm_action_cfg->INDEX_4;  break;
        case  5: enable = wcm_action_cfg->ENABLE_5;  index = wcm_action_cfg->INDEX_5;  break;
        case  6: enable = wcm_action_cfg->ENABLE_6;  index = wcm_action_cfg->INDEX_6;  break;
        case  7: enable = wcm_action_cfg->ENABLE_7;  index = wcm_action_cfg->INDEX_7;  break;
        case  8: enable = wcm_action_cfg->ENABLE_8;  index = wcm_action_cfg->INDEX_8;  break;
        case  9: enable = wcm_action_cfg->ENABLE_9;  index = wcm_action_cfg->INDEX_9;  break;
        case 10: enable = wcm_action_cfg->ENABLE_10; index = wcm_action_cfg->INDEX_10; break;
        case 11: enable = wcm_action_cfg->ENABLE_11; index = wcm_action_cfg->INDEX_11; break;
        case 12: enable = wcm_action_cfg->ENABLE_12; index = wcm_action_cfg->INDEX_12; break;
        case 13: enable = wcm_action_cfg->ENABLE_13; index = wcm_action_cfg->INDEX_13; break;
        case 14: enable = wcm_action_cfg->ENABLE_14; index = wcm_action_cfg->INDEX_14; break;
        case 15: enable = wcm_action_cfg->ENABLE_15; index = wcm_action_cfg->INDEX_15; break;
        case 16: enable = wcm_action_cfg->ENABLE_16; index = wcm_action_cfg->INDEX_16; break;
        case 17: enable = wcm_action_cfg->ENABLE_17; index = wcm_action_cfg->INDEX_17; break;
        case 18: enable = wcm_action_cfg->ENABLE_18; index = wcm_action_cfg->INDEX_18; break;
        case 19: enable = wcm_action_cfg->ENABLE_19; index = wcm_action_cfg->INDEX_19; break;
        default: enable = 0;                         index = 0;
    }

    action_cfg.enable = enable;
    action_cfg.slice  = index;
#else
    fm_uint32 ffu_action_cfg_regs[MBY_FFU_ACTION_CFG_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_FFU_ACTION_CFG(group, scenario, 0), MBY_FFU_ACTION_CFG_WIDTH, ffu_action_cfg_regs);

    action_cfg.enable = FM_ARRAY_GET_UNNAMED_FIELD(ffu_action_cfg_regs, ram_num + (MBY_FFU_ACTION_ENTRIES_1 * 4), 1);
    action_cfg.slice  = FM_ARRAY_GET_UNNAMED_FIELD(ffu_action_cfg_regs, ram_num * 4, 4);
#endif
    return action_cfg;
}

fm_uint32 mbyClsGetWcmActionEntry
(
    MBY_CGRP_B_IN_REGS,
    fm_byte              const ram_num,
    fm_uint32            const hit_index,
    fm_uint32            const action
)
{
#ifdef USE_NEW_CSRS
    wcm_action_r const * const wcm_action = &(cgrp_b_map->WCM_ACTION[ram_num][hit_index]);
    fm_uint32 action_entry = (action == 0) ? wcm_action->ACTION0 : wcm_action->ACTION1;
#else
    fm_byte const group = 1; // group "B"
    fm_uint64 ffu_action_reg = 0;
    mbyModelReadCSR64(regs, MBY_FFU_ACTION(group, ram_num, hit_index, 0), &ffu_action_reg);
    fm_uint32 action_entry = (action == 0) ?
        FM_GET_FIELD64(ffu_action_reg, MBY_FFU_ACTION, ACTION0) :
        FM_GET_FIELD64(ffu_action_reg, MBY_FFU_ACTION, ACTION1) ;
#endif
    return action_entry;
}

mbyClassifierEntropyCfg mbyClsGetEntropyCfg
(
    MBY_ENTROPY_MAP_IN_REGS,
    fm_uint32             const hash_num,
    fm_byte               const hash_prof
)
{
    mbyClassifierEntropyCfg entropy_cfg = { 0 };

#ifdef USE_NEW_CSRS
    entropy_hash_cfg1_r const * const entropy_hash_cfg1 = &(entropy_map->ENTROPY_HASH_CFG1[hash_num][hash_prof]);
    entropy_hash_cfg0_r const * const entropy_hash_cfg0 = &(entropy_map->ENTROPY_HASH_CFG0[hash_num][hash_prof]);

    entropy_cfg.SYMMETRIC        = entropy_hash_cfg1->SYMMETRIC;        // [54:54]
    entropy_cfg.SYM_PROFILE      = entropy_hash_cfg1->SYM_PROFILE;      // [53:52]
    entropy_cfg.KEY_MASK_PROFILE = entropy_hash_cfg1->KEY_MASK_PROFILE; // [51:48]
    entropy_cfg.KEY32_MASK       = entropy_hash_cfg1->KEY_MASK32;       // [47:32]
    entropy_cfg.KEY16_MASK       = entropy_hash_cfg1->KEY_MASK16;       // [31: 0]
    entropy_cfg.KEY8_MASK        = entropy_hash_cfg0->KEY_MASK8;        // [31: 0]
#else
    fm_uint64 entropy_hash_cfg0_reg = 0;
    fm_uint64 entropy_hash_cfg1_reg = 0;

    mbyModelReadCSR64(regs, MBY_ENTROPY_HASH_CFG0(hash_num, hash_prof, 0), &entropy_hash_cfg0_reg);
    mbyModelReadCSR64(regs, MBY_ENTROPY_HASH_CFG1(hash_num, hash_prof, 0), &entropy_hash_cfg1_reg);

    entropy_cfg.SYMMETRIC        = 0; // new <--- REVISIT!!!
    entropy_cfg.SYM_PROFILE      = 0; // new <--- REVISIT!!!
    entropy_cfg.KEY_MASK_PROFILE = 0; // new <--- REVISIT!!!
    entropy_cfg.KEY32_MASK       = FM_GET_FIELD64(entropy_hash_cfg1_reg, MBY_FFU_KEY_MASK1, KEY32_MASK);
    entropy_cfg.KEY16_MASK       = FM_GET_FIELD64(entropy_hash_cfg1_reg, MBY_FFU_KEY_MASK1, KEY16_MASK);
    entropy_cfg.KEY8_MASK        = FM_GET_FIELD64(entropy_hash_cfg0_reg, MBY_FFU_KEY_MASK0, KEY8_MASK);
#endif
    return entropy_cfg;
}

mbyEntropyMetaCfg mbyClsGetEntropyMetaCfg
(
    MBY_ENTROPY_MAP_IN_REGS,
    fm_byte               const hash_prof
)
{
    mbyEntropyMetaCfg meta_cfg = { 0 };

#ifdef USE_NEW_CSRS
    entropy_meta_cfg_r const * const entropy_meta_cfg = &(entropy_map->ENTROPY_META_CFG[hash_prof]);

    meta_cfg.BYTE_DEFAULTS = entropy_meta_cfg->BYTE_DEFAULTS; // [23:12]
    meta_cfg.HASH_START    = entropy_meta_cfg->HASH_START;    // [11: 6]
    meta_cfg.HASH_SIZE     = entropy_meta_cfg->HASH_SIZE;     // [ 5: 0]
#else
    fm_uint64 entropy_meta_cfg_reg = 0;
    mbyModelReadCSR64(regs, MBY_ENTROPY_META_CFG(hash_prof, 0), &entropy_meta_cfg_reg);

    meta_cfg.BYTE_DEFAULTS = FM_GET_FIELD64(entropy_meta_cfg_reg, MBY_ENTROPY_META_CFG, BYTE_DEFAULTS);
    meta_cfg.HASH_START    = FM_GET_FIELD64(entropy_meta_cfg_reg, MBY_ENTROPY_META_CFG, HASH_START);
    meta_cfg.HASH_SIZE     = FM_GET_FIELD64(entropy_meta_cfg_reg, MBY_ENTROPY_META_CFG, HASH_SIZE);
#endif

    return meta_cfg;
}

// TODO this function is not strictly used to access the registers but it
// is used by various blocks of the classifiers
void mbyClsConvertKeysToBytes
(
    mbyClassifierKeys const keys,
    fm_byte                 bytes[MBY_FFU_HASH_KEYS]
)
{
    for (fm_uint i = 0; i < MBY_FFU_KEY16; i++) {
        bytes[2*i + 1] =  keys.key16[i]       & 0xFF;
        bytes[2*i    ] = (keys.key16[i] >> 8) & 0xFF;
    }

    for (fm_uint i = 0; i < MBY_FFU_KEY8; i++)
        bytes[MBY_FFU_KEY16*2 + i] = keys.key8[i];

    for (fm_uint i = 0; i < MBY_FFU_KEY32; i++)
        for (fm_uint j = 0; j < 4; j++)
            bytes[MBY_FFU_KEY16*2 + MBY_FFU_KEY8 + i*4 + (3-j)] = (keys.key32[i] >> (j * 8 )) & 0xFF;
}
