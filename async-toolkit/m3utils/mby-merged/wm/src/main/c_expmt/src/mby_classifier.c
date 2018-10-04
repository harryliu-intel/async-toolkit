// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifdef USE_NEW_CSRS
#include <mby_top_map.h>
#endif

#include "mby_mapper.h"
#include "mby_classifier.h"
#include "mby_crc32.h"

static mbyClassifierKeyMaskCfg getEmAKeyMaskCfg
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_a_map * const cgrp_a_map,
#else
    fm_uint32                  regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    fm_byte              const hash_num,
    fm_byte              const scenario
)
{
    mbyClassifierKeyMaskCfg key_mask_cfg;

#ifdef USE_NEW_CSRS
    key_mask_cfg.keyMaskSel = cgrp_a_map->EM_A_KEY_SEL1[hash_num][scenario].KEY_MASK_SEL; // [51:48]
    key_mask_cfg.key32Mask  = cgrp_a_map->EM_A_KEY_SEL1[hash_num][scenario].KEY32_MASK;   // [47:32]
    key_mask_cfg.key16Mask  = cgrp_a_map->EM_A_KEY_SEL1[hash_num][scenario].KEY16_MASK;   // [31: 0]
    key_mask_cfg.key8Mask   = cgrp_a_map->EM_A_KEY_SEL0[hash_num][scenario].KEY8_MASK;    // [31: 0]
#else
    fm_byte const group = 0; // Exact Match "A"

    fm_uint64 ffu_key_mask0_reg = 0;
    fm_uint64 ffu_key_mask1_reg = 0;

    mbyModelReadCSR64(regs, MBY_FFU_KEY_MASK0(group, hash_num, scenario, 0), &ffu_key_mask0_reg);
    mbyModelReadCSR64(regs, MBY_FFU_KEY_MASK1(group, hash_num, scenario, 0), &ffu_key_mask1_reg);

    key_mask_cfg.keyMaskSel = 0; // <--- REVISIT!!!!
    key_mask_cfg.key32Mask  = FM_GET_FIELD64(ffu_key_mask1_reg, MBY_FFU_KEY_MASK1, KEY32_MASK);
    key_mask_cfg.key16Mask  = FM_GET_FIELD64(ffu_key_mask1_reg, MBY_FFU_KEY_MASK1, KEY16_MASK);
    key_mask_cfg.key8Mask   = FM_GET_FIELD64(ffu_key_mask0_reg, MBY_FFU_KEY_MASK0, KEY8_MASK);
#endif
    return key_mask_cfg;
}

static mbyClassifierKeyMaskCfg getEmBKeyMaskCfg
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_b_map * const cgrp_b_map,
#else
    fm_uint32                  regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    fm_byte              const hash_num,
    fm_byte              const scenario
)
{
    mbyClassifierKeyMaskCfg key_mask_cfg;

#ifdef USE_NEW_CSRS
    key_mask_cfg.keyMaskSel = cgrp_b_map->EM_B_KEY_SEL1[hash_num][scenario].KEY_MASK_SEL; // [51:48]
    key_mask_cfg.key32Mask  = cgrp_b_map->EM_B_KEY_SEL1[hash_num][scenario].KEY32_MASK;   // [47:32]
    key_mask_cfg.key16Mask  = cgrp_b_map->EM_B_KEY_SEL1[hash_num][scenario].KEY16_MASK;   // [31: 0]
    key_mask_cfg.key8Mask   = cgrp_b_map->EM_B_KEY_SEL0[hash_num][scenario].KEY8_MASK;    // [31: 0]
#else
    fm_byte const group = 1; // Exact Match "B"

    fm_uint64 ffu_key_mask0_reg = 0;
    fm_uint64 ffu_key_mask1_reg = 0;

    mbyModelReadCSR64(regs, MBY_FFU_KEY_MASK0(group, hash_num, scenario, 0), &ffu_key_mask0_reg);
    mbyModelReadCSR64(regs, MBY_FFU_KEY_MASK1(group, hash_num, scenario, 0), &ffu_key_mask1_reg);

    key_mask_cfg.keyMaskSel = 0; // <--- REVISIT!!!!
    key_mask_cfg.key32Mask  = FM_GET_FIELD64(ffu_key_mask1_reg, MBY_FFU_KEY_MASK1, KEY32_MASK);
    key_mask_cfg.key16Mask  = FM_GET_FIELD64(ffu_key_mask1_reg, MBY_FFU_KEY_MASK1, KEY16_MASK);
    key_mask_cfg.key8Mask   = FM_GET_FIELD64(ffu_key_mask0_reg, MBY_FFU_KEY_MASK0, KEY8_MASK);
#endif
    return key_mask_cfg;
}

static mbyClassifierHashCfg getEmAHashCfg
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_a_map * const cgrp_a_map,
#else
    fm_uint32                  regs[MBY_REGISTER_ARRAY_SIZE],
#endif
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

static mbyClassifierHashCfg getEmBHashCfg
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_b_map * const cgrp_b_map,
#else
    fm_uint32                  regs[MBY_REGISTER_ARRAY_SIZE],
#endif
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

static mbyClassifierHashLookup getEmAHashLookupEntry
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_a_map * const cgrp_a_map,
#else
    fm_uint32                  regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    fm_uint16 const            lookup_ptr
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

static mbyClassifierHashLookup getEmBHashLookupEntry
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_b_map * const cgrp_b_map,
#else
    fm_uint32                  regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    fm_uint16 const            lookup_ptr
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

static fm_uint64 getEmAHashCamEntry
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_a_map * const cgrp_a_map,
#else
    fm_uint32                  regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    fm_uint32 const            entry,
    fm_uint32 const            word
)
{
#ifdef USE_NEW_CSRS
    em_a_hash_cam_r const * const em_a_hash_cam_entry = &(cgrp_a_map->EM_A_HASH_CAM[entry][word]);

    fm_uint64 data = em_a_hash_cam_entry->DATA;
#else
    fm_byte const group = 0; // Exact Match "A"
    fm_uint64 ffu_hash_cam_reg = 0;
    mbyModelReadCSR64(regs, MBY_FFU_HASH_CAM(group, entry, word, 0), &ffu_hash_cam_reg);

    fm_uint64 data = FM_GET_FIELD64(ffu_hash_cam_reg, MBY_FFU_HASH_CAM, DATA);
#endif
    return data;
}

static fm_uint64 getEmBHashCamEntry
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_b_map * const cgrp_b_map,
#else
    fm_uint32                  regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    fm_uint32 const            entry,
    fm_uint32 const            word
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

static fm_uint64 getEmAHashCamMask
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_a_map * const cgrp_a_map,
#else
    fm_uint32                  regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    fm_uint32 const            row,
    fm_uint32 const            rule
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

static fm_uint64 getEmBHashCamMask
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_b_map * const cgrp_b_map,
#else
    fm_uint32                  regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    fm_uint32 const            row,
    fm_uint32 const            rule
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

static inline fm_uint64 getHashEntryRam
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

static inline fm_byte getHashRamAlloc
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

static void getHashMissActions
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

static mbyClassifierTcamCfg getWcmTcamCfg
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_b_map * const cgrp_b_map,
#else
    fm_uint32                  regs[MBY_REGISTER_ARRAY_SIZE],
#endif
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

static mbyClassifierTcamEntry getWcmTcamEntry
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_b_map * const cgrp_b_map,
#else
    fm_uint32                  regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    fm_byte   const            slice,
    fm_uint16 const            index
)
{
    mbyClassifierTcamEntry tcam_entry;

#ifdef USE_NEW_CSRS
    wcm_tcam_r const * const wcm_tcam_entry = &(cgrp_b_map->WCM_TCAM[slice][index]);

    fm_uint64 key_top     = ((fm_uint64) wcm_tcam_entry->KEY_TOP)        << 32;
    fm_uint64 key_top_inv = ((fm_uint64) wcm_tcam_entry->KEY_TOP_INVERT) << 32;

    tcam_entry.key        = wcm_tcam_entry->KEY        | key_top;
    tcam_entry.keyInvert  = wcm_tcam_entry->KEY_INVERT | key_top_inv;
#else
    fm_byte const group = 1; // group "B"

    fm_uint32 ffu_tcam_regs[MBY_FFU_TCAM_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_FFU_TCAM(group, slice, index, 0), MBY_FFU_TCAM_WIDTH, ffu_tcam_regs);

    tcam_entry.key       = FM_ARRAY_GET_FIELD64(ffu_tcam_regs, MBY_FFU_TCAM, KEY) |
                           FM_ARRAY_GET_FIELD64(ffu_tcam_regs, MBY_FFU_TCAM, KEY_TOP) << 32;

    tcam_entry.keyInvert = FM_ARRAY_GET_FIELD64(ffu_tcam_regs, MBY_FFU_TCAM, KEY_INVERT) |
                           FM_ARRAY_GET_FIELD64(ffu_tcam_regs, MBY_FFU_TCAM, KEY_TOP_INVERT) << 32;
#endif
    return tcam_entry;
}

static mbyClassifierActionCfg getWcmActionCfg
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_b_map * const cgrp_b_map,
#else
    fm_uint32                  regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    fm_byte const              group,
    fm_byte const              scenario,
    fm_byte const              ram_num
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

static fm_uint32 getWcmActionEntry
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_b_map * const cgrp_b_map,
#else
    fm_uint32                  regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    fm_byte   const            ram_num,
    fm_uint32 const            hit_index,
    fm_uint32 const            action
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

static mbyClassifierEntropyCfg getEntropyCfg
(
#ifdef USE_NEW_CSRS
    mby_ppe_entropy_map * const entropy_map,
#else
    fm_uint32                   regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    fm_uint32 const             hash_num,
    fm_byte   const             hash_prof
)
{
    mbyClassifierEntropyCfg entropy_cfg = { 0 };

#ifdef USE_NEW_CSRS
    entropy_hash_cfg1_r const * const entropy_hash_cfg1 = &(entropy_map->ENTROPY_HASH_CFG1[hash_num][hash_prof]);
    entropy_hash_cfg0_r const * const entropy_hash_cfg0 = &(entropy_map->ENTROPY_HASH_CFG0[hash_num][hash_prof]);

    entropy_cfg.symmetric   = entropy_hash_cfg1->SYMMETRIC;        // [54:54]
    entropy_cfg.symProf     = entropy_hash_cfg1->SYM_PROFILE;      // [53:52]
    entropy_cfg.keyMaskProf = entropy_hash_cfg1->KEY_MASK_PROFILE; // [51:48]
    entropy_cfg.key32Mask   = entropy_hash_cfg1->KEY_MASK32;       // [47:32]
    entropy_cfg.key16Mask   = entropy_hash_cfg1->KEY_MASK16;       // [31: 0]
    entropy_cfg.key8Mask    = entropy_hash_cfg0->KEY_MASK8;        // [31: 0]
#else
    fm_uint64 entropy_hash_cfg0_reg = 0;
    fm_uint64 entropy_hash_cfg1_reg = 0;

    mbyModelReadCSR64(regs, MBY_ENTROPY_HASH_CFG0(hash_num, hash_prof, 0), &entropy_hash_cfg0_reg);
    mbyModelReadCSR64(regs, MBY_ENTROPY_HASH_CFG1(hash_num, hash_prof, 0), &entropy_hash_cfg1_reg);

    entropy_cfg.key8Mask    = FM_GET_FIELD64(entropy_hash_cfg0_reg, MBY_FFU_KEY_MASK0, KEY8_MASK);
    entropy_cfg.key16Mask   = FM_GET_FIELD64(entropy_hash_cfg1_reg, MBY_FFU_KEY_MASK1, KEY16_MASK);
    entropy_cfg.key32Mask   = FM_GET_FIELD64(entropy_hash_cfg1_reg, MBY_FFU_KEY_MASK1, KEY32_MASK);
    entropy_cfg.keyMaskProf = 0; // new <--- REVISIT!!!
    entropy_cfg.symProf     = 0; // new <--- REVISIT!!!
    entropy_cfg.symmetric   = 0; // new <--- REVISIT!!!
#endif
    return entropy_cfg;
}

static mbyEntropyMetaCfg getEntropyMetaCfg
(
#ifdef USE_NEW_CSRS
    mby_ppe_entropy_map * const entropy_map,
#else
    fm_uint32                   regs[MBY_REGISTER_ARRAY_SIZE],
#endif
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

// --------------------------------------------------------------------------------

static mbyLookupInfo selectWcmKeyMask
(
    const mbyClassifierTcamCfg * const tcam_cfg,
    const mbyClassifierKeys    * const keys
)
{
    mbyLookupInfo lookup_info = { 0 };

    fm_byte mux[5] = { 0 };
    mux[0] = tcam_cfg->SELECT0;
    mux[1] = tcam_cfg->SELECT1;
    mux[2] = tcam_cfg->SELECT2;
    mux[3] = tcam_cfg->SELECT3;
    mux[4] = tcam_cfg->SELECT_TOP + MBY_FFU_KEY16;

    lookup_info.key       = 0;
    lookup_info.keyInvert = FM_LITERAL_U64(0xFFFFFFFFF);

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

        FM_SET_UNNAMED_FIELD64(lookup_info.key, lo_bit, 8, temp);
    }

    lookup_info.keyInvert = ~lookup_info.key;

    return lookup_info;
}

static void lookUpWcmTcam
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_b_map * const cgrp_b_map,
#else
    fm_uint32                  regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    fm_byte   const            slice,
    fm_uint16 const            chunk_mask,
    mbyLookupInfo      * const lookup_info
)
{
    for (fm_uint i = 0; i < MBY_FFU_TCAM_ENTRIES_0; i++)
        lookup_info->rawHits[i] = FALSE;

    fm_uint16 tcam_index = 0;

    while (tcam_index < MBY_FFU_TCAM_ENTRIES_0)
    {
        fm_byte tcam_chunk = tcam_index >> 6;

        if ((chunk_mask >> tcam_chunk) & 1)
        {
#ifdef USE_NEW_CSRS
            mbyClassifierTcamEntry tcam_entry = getWcmTcamEntry(cgrp_b_map, slice, tcam_index);
#else
            mbyClassifierTcamEntry tcam_entry = getWcmTcamEntry(regs,       slice, tcam_index);
#endif
            fm_uint64 cam_key_inv = tcam_entry.keyInvert;
            fm_uint64 cam_key     = tcam_entry.key;
            fm_uint64 mask        = cam_key ^ cam_key_inv;

            if (((cam_key & cam_key_inv) == 0) && ((lookup_info->key & mask) == (cam_key & mask)))
                lookup_info->rawHits[tcam_index] = TRUE;

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
    const mbyClassifierKeys * const keys,
    const fm_byte                   scenario,
    const fm_byte                   group,
    mbyClassifierHitInfo            tcam_hit_info[MBY_FFU_TCAM_CFG_ENTRIES_1]
)
{
    // Compute cascade width:
    fm_byte cascade_width[MBY_FFU_TCAM_CFG_ENTRIES_1] = { 0 };

    for (fm_uint slice = 0; slice < MBY_FFU_TCAM_CFG_ENTRIES_1; slice++)
    {
#ifdef USE_NEW_CSRS
        mbyClassifierTcamCfg tcam_cfg = getWcmTcamCfg(cgrp_b_map, group, slice, scenario);
#else
        mbyClassifierTcamCfg tcam_cfg = getWcmTcamCfg(regs,       group, slice, scenario);
#endif
        if (tcam_cfg.START_COMPARE)
            cascade_width[slice]++;

        for (fm_uint i = slice + 1; i < MBY_FFU_TCAM_CFG_ENTRIES_1; i++)
        {
#ifdef USE_NEW_CSRS
            mbyClassifierTcamCfg tcam_cfg1 = getWcmTcamCfg(cgrp_b_map, group, i, scenario);
#else
            mbyClassifierTcamCfg tcam_cfg1 = getWcmTcamCfg(regs,       group, i, scenario);
#endif
            if (tcam_cfg1.START_COMPARE)
                break;
            cascade_width[slice]++;
        }
    }

    fm_bool fsc           = FALSE;
    fm_bool exclusion_set = FALSE;
    fm_bool set_hit       = FALSE;

    for (fm_uint slice = 0; slice < MBY_FFU_TCAM_CFG_ENTRIES_1; slice++)
    {
        if (cascade_width[slice] == 0)
            continue;

#ifdef USE_NEW_CSRS
        mbyClassifierTcamCfg tcam_cfg = getWcmTcamCfg(cgrp_b_map, group, slice, scenario);
#else
        mbyClassifierTcamCfg tcam_cfg = getWcmTcamCfg(regs,       group, slice, scenario);
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
            mbyClassifierTcamCfg tcam_cfg1 = getWcmTcamCfg(cgrp_b_map, group, i, scenario);
#else
            mbyClassifierTcamCfg tcam_cfg1 = getWcmTcamCfg(regs,       group, i, scenario);
#endif
            // Compute TCAM key:
            mbyLookupInfo lookup_info = selectWcmKeyMask(&tcam_cfg1, keys);

            // Look up in the TCAM and update raw hits with results of lookup:
#ifdef USE_NEW_CSRS
            lookUpWcmTcam(cgrp_b_map, i, tcam_cfg1.CHUNK_MASK, &lookup_info);
#else
            lookUpWcmTcam(regs,       i, tcam_cfg1.CHUNK_MASK, &lookup_info);
#endif
            for (fm_uint j = 0; j < MBY_FFU_TCAM_ENTRIES_0; j++)
                hits[j] = (fsc || tcam_cfg1.START_COMPARE || hits[j]) && lookup_info.rawHits[j];

            fsc = (i == slice) && (tcam_cfg1.START_COMPARE == 1) && (tcam_cfg1.CHUNK_MASK == 0);
        }

        for (fm_int j = MBY_FFU_TCAM_ENTRIES_0 - 1; j >= 0; j--)
        {
            if (hits[j] && !set_hit) {
                // introduce slice_info to fix klocwork error
                fm_uint slice_info = slice + cascade_width[slice]-1;
                tcam_hit_info[slice_info].hitIndexValid = TRUE;
                tcam_hit_info[slice_info].hitIndex = j;
                if (exclusion_set)
                    set_hit = TRUE;
                break;
            }
        }
    }
}

static inline void setPrec(mbyPrecVal *old, fm_byte prec, fm_uint32 value)
{
    if ((prec >= old->prec) && (prec > 0))
    {
        old->prec = prec;
        old->val  = value;
    }
}

static void doAction
(
    fm_uint32 const              action,
    mbyClassifierActions * const actions
)
{
    fm_byte prec   = FM_GET_FIELD        (action, MBY_FFU_ACTION, PREC);
    fm_byte encode = FM_GET_UNNAMED_FIELD(action, MBY_FFU_ACTION_l_ENTRYTYPE, 5);

    mbyClassifierActionEntryType entryType =
        (encode == 1)                ? MBY_FFU_ACTION_SET4_4B  :
        (encode == 2)                ? MBY_FFU_ACTION_SET8_1B  :
        (encode == 4)                ? MBY_FFU_ACTION_SET3_1B  :
        (((encode >> 3) & 0x3) == 1) ? MBY_FFU_ACTION_SET3_4B  :
        (((encode >> 4) & 0x1) == 1) ? MBY_FFU_ACTION_SET1_24B : MBY_FFU_ACTION_NOP;

    switch (entryType)
    {
        case MBY_FFU_ACTION_SET4_4B:
        {
            fm_byte   index  = FM_GET_FIELD(action, MBY_FFU_ACTION, SET4_4B_INDEX);
            fm_byte   enable = FM_GET_FIELD(action, MBY_FFU_ACTION, SET4_4B_ENABLE);
            fm_uint32 value  = FM_GET_FIELD(action, MBY_FFU_ACTION, SET4_4B_VALUE);
            for (fm_uint i = 0; i < 4; i++) {
                fm_uint j = index * 4 + i;
                if ((j < MBY_FFU_ACT4) && (enable & (1uL << i)))
                    setPrec(&(actions->act4[j]), prec, ((value >> 4*i) & 0xF));
            }
            break;
        }

        case MBY_FFU_ACTION_SET8_1B:
        {
            fm_byte   index  = FM_GET_FIELD(action, MBY_FFU_ACTION, SET8_1B_INDEX);
            fm_byte   enable = FM_GET_FIELD(action, MBY_FFU_ACTION, SET8_1B_ENABLE);
            fm_uint32 value  = FM_GET_FIELD(action, MBY_FFU_ACTION, SET8_1B_VALUE);
            for (fm_uint i = 0; i < 8; i++) {
                fm_uint j = index * 8 + i;
                if ((j < MBY_FFU_ACT1) && (enable & (1uL << i)))
                    setPrec(&(actions->act1[j]), prec, ((value >> i) & 0x1));
            }
            break;
        }

        case MBY_FFU_ACTION_SET3_1B:
        {
            fm_byte indexA  = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_1B_INDEXA);
            fm_bool enableA = FM_GET_BIT  (action, MBY_FFU_ACTION, SET3_1B_EA);
            fm_byte valueA  = FM_GET_BIT  (action, MBY_FFU_ACTION, SET3_1B_VA);
            if (enableA && (indexA < MBY_FFU_ACT1))
                setPrec(&(actions->act1[indexA]), prec, valueA);

            fm_byte indexB  = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_1B_INDEXB);
            fm_bool enableB = FM_GET_BIT  (action, MBY_FFU_ACTION, SET3_1B_EB);
            fm_byte valueB  = FM_GET_BIT  (action, MBY_FFU_ACTION, SET3_1B_VB);
            if (enableB && (indexB < MBY_FFU_ACT1))
                setPrec(&(actions->act1[indexB]), prec, valueB);

            fm_byte indexC  = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_1B_INDEXC);
            fm_bool enableC = FM_GET_BIT  (action, MBY_FFU_ACTION, SET3_1B_EC);
            fm_byte valueC  = FM_GET_BIT  (action, MBY_FFU_ACTION, SET3_1B_VC);
            if (enableC && (indexC < MBY_FFU_ACT1))
                setPrec(&(actions->act1[indexC]), prec, valueC);

            break;
        }

        case MBY_FFU_ACTION_SET3_4B:
        {
            fm_byte indexA = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_4B_INDEXA);
            fm_byte valueA = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_4B_VALUEA);
            if (indexA < MBY_FFU_ACT4)
                setPrec(&(actions->act4[indexA]), prec, valueA);

            fm_byte indexB = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_4B_INDEXB);
            fm_byte valueB = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_4B_VALUEB);
            if (indexB < MBY_FFU_ACT4)
                setPrec(&(actions->act4[indexB]), prec, valueB);

            fm_byte indexC = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_4B_INDEXC);
            fm_byte valueC = FM_GET_FIELD(action, MBY_FFU_ACTION, SET3_4B_VALUEC);
            if (indexC < MBY_FFU_ACT4)
                setPrec(&(actions->act4[indexC]), prec, valueC);

            break;
        }

        case MBY_FFU_ACTION_SET1_24B:
        {
            fm_byte   index = FM_GET_FIELD(action, MBY_FFU_ACTION, SET1_24B_INDEX);
            fm_uint32 value = FM_GET_FIELD(action, MBY_FFU_ACTION, SET1_24B_VALUE);
            if(index < MBY_FFU_ACT24)
                setPrec(&(actions->act24[index]), prec, value);
            break;
        }

        default:

        case MBY_FFU_ACTION_NOP:
            break;
    }
}

static void resolveActions
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_b_map * const   cgrp_b_map,
#else
    fm_uint32                    regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    const fm_byte                scenario,
    const fm_byte                group,
    mbyClassifierHitInfo         tcam_hit_info[MBY_FFU_TCAM_CFG_ENTRIES_1],
    mbyClassifierActions * const actions // = output actions
)
{
    for (fm_uint ram_num = 0; ram_num < MBY_FFU_ACTION_ENTRIES_1; ram_num++)
    {
#ifdef USE_NEW_CSRS
        mbyClassifierActionCfg action_cfg = getWcmActionCfg(cgrp_b_map, group, scenario, ram_num);
#else
        mbyClassifierActionCfg action_cfg = getWcmActionCfg(regs,       group, scenario, ram_num);
#endif
        if (!action_cfg.enable)
            continue; // skip action RAM if disabled

        fm_uint slice = action_cfg.slice;
        if (!tcam_hit_info[slice].hitIndexValid)
            continue;

        fm_uint hit_index = tcam_hit_info[slice].hitIndex;
        for (fm_uint i = 0; i < MBY_FFU_ACTIONS_PER_ENTRY; i++) {
#ifdef USE_NEW_CSRS
            fm_uint32 action_entry = getWcmActionEntry(cgrp_b_map, ram_num, hit_index, i);
#else
            fm_uint32 action_entry = getWcmActionEntry(regs,       ram_num, hit_index, i);
#endif
            doAction(action_entry, actions);
        }
    }
}

static void matchWildcard
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_b_map      * const cgrp_b_map,
#else
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    const mbyClassifierKeys * const keys,
    const fm_byte                   scenario,
    const fm_byte                   group,
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

    // Apply and resolve actions from action RAMs based on tcam hit index per slice:
#ifdef USE_NEW_CSRS
    resolveActions(cgrp_b_map, scenario, group, tcam_hit_info, actions);
#else
    resolveActions(regs,       scenario, group, tcam_hit_info, actions);
#endif
}

static void applyEmKeyMask
(
    const mbyClassifierKeyMaskCfg         key_mask_cfg,
    const mbyClassifierKeys               keys,
          mbyClassifierKeys       * const hash_keys
)
{
    for (fm_uint i = 0; i < MBY_FFU_KEY16; i++)
        hash_keys->key16[i] = (FM_GET_UNNAMED_FIELD  (key_mask_cfg.key16Mask, i, 1)) ? keys.key16[i] : 0;

    for (fm_uint i = 0; i < MBY_FFU_KEY8; i++)
        hash_keys->key8[i]  = (FM_GET_UNNAMED_FIELD64(key_mask_cfg.key8Mask,  i, 1)) ? keys.key8 [i] : 0;

    for (fm_uint i = 0; i < MBY_FFU_KEY32; i++)
        hash_keys->key32[i] = (FM_GET_UNNAMED_FIELD  (key_mask_cfg.key32Mask, i, 1)) ? keys.key32[i] : 0;
}

static void applyEntropyKeyMask
(
    const mbyClassifierEntropyCfg         entropy_cfg,
    const mbyClassifierKeys               keys,
          mbyClassifierKeys       * const hash_keys
)
{
    for (fm_uint i = 0; i < MBY_FFU_KEY16; i++)
        hash_keys->key16[i] = (FM_GET_UNNAMED_FIELD  (entropy_cfg.key16Mask, i, 1)) ? keys.key16[i] : 0;

    for (fm_uint i = 0; i < MBY_FFU_KEY8; i++)
        hash_keys->key8[i]  = (FM_GET_UNNAMED_FIELD64(entropy_cfg.key8Mask,  i, 1)) ? keys.key8 [i] : 0;

    for (fm_uint i = 0; i < MBY_FFU_KEY32; i++)
        hash_keys->key32[i] = (FM_GET_UNNAMED_FIELD  (entropy_cfg.key32Mask, i, 1)) ? keys.key32[i] : 0;
}

static void convertKeysToBytes
(
    const mbyClassifierKeys keys,
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

static void doKeyCompaction
(
    const mbyClassifierKeyMaskCfg         key_mask_cfg,
    const mbyClassifierKeys               hash_keys,
    fm_byte                               packed_keys[MBY_FFU_HASH_KEYS],
    fm_byte                       * const key_size
)
{
    fm_byte key_idx = 0;

    // KEY32:
    for (fm_uint i = 0; i < MBY_FFU_KEY32; i++) {
        if (FM_GET_UNNAMED_FIELD(key_mask_cfg.key32Mask, i, 1)) {
            for (fm_uint j = 0; j < 4; j++)
                packed_keys[key_idx + j] = (hash_keys.key32[i] >> (8 * (3-j))) & 0xFF;
            key_idx += 4;
        }
    }

    // KEY16:
    for (fm_uint i = 0; i < MBY_FFU_KEY16; i++) {
        if (FM_GET_UNNAMED_FIELD(key_mask_cfg.key16Mask, i, 1)) {
            packed_keys[key_idx    ] = (hash_keys.key16[i] >> 8) & 0xFF;
            packed_keys[key_idx + 1] =  hash_keys.key16[i]       & 0xFF;
            key_idx += 2;
        }

    }

    // KEY8:
    for (fm_uint i = 0; i < MBY_FFU_KEY8; i++) {
        if (FM_GET_UNNAMED_FIELD(key_mask_cfg.key8Mask, i, 1)) {
            packed_keys[key_idx] = hash_keys.key8[i];
            key_idx++;
        }
    }

    // pad key size to be multiple of 4B:
    for (fm_uint i = (key_idx % 4); (0 < i) && (i < 4); i++) {
        packed_keys[key_idx] = 0;
        key_idx++;
        if (key_idx == MBY_FFU_HASH_KEYS)
            break;
    }

    *key_size = key_idx;
}

static void getHashRamData
(
#ifdef USE_NEW_CSRS

#else
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    fm_byte                   const group,
    fm_uint32                 const hash_num,
    mbyClassifierHashLookup   const bucket,
    mbyClassifierHashCfg      const hash_cfg,
    fm_uint16                 const hash_more,
    fm_byte                         hash_ram_key[MBY_FFU_MAX_HASH_ENTRY_SIZE],
    fm_bool                 * const ram_data_ok
)
{
    fm_byte bucket_hash = 0;
    FM_SET_UNNAMED_FIELD(bucket_hash, 0, 1, FM_GET_UNNAMED_FIELD(hash_more, bucket.SELECT_0, 1));
    FM_SET_UNNAMED_FIELD(bucket_hash, 1, 1, FM_GET_UNNAMED_FIELD(hash_more, bucket.SELECT_1, 1));
    FM_SET_UNNAMED_FIELD(bucket_hash, 2, 1, FM_GET_UNNAMED_FIELD(hash_more, bucket.SELECT_2, 1));
    FM_SET_UNNAMED_FIELD(bucket_hash, 3, 1, FM_GET_UNNAMED_FIELD(hash_more, bucket.SELECT_3, 1));
    FM_SET_UNNAMED_FIELD(bucket_hash, 4, 1, FM_GET_UNNAMED_FIELD(hash_more, bucket.SELECT_4, 1));

    // Buckets valid entries are stored contiguously in the entry table
    fm_uint16 offset = 0;
    for (fm_uint i = 0; i < 32; i++) {
        if (i == bucket_hash)
            break;
        if ((bucket.MASK >> i) & 1)
           offset++;
    }

    fm_uint32 hash_ram_addr = (bucket.PTR + offset * hash_cfg.entry_size[hash_num]) * 4;

    fm_byte ram_alloc[MBY_FFU_KEY_MASK0_ENTRIES_1] = { 0 };
    for (fm_uint i = 0; i < MBY_FFU_KEY_MASK0_ENTRIES_1; i++) {
#ifdef USE_NEW_CSRS
        ram_alloc[i] = getHashRamAlloc(      i);
#else
        ram_alloc[i] = getHashRamAlloc(regs, i);
#endif
    }

    fm_bool   mode_32b   = (hash_cfg.mode == MBY_FFU_HASH_ENTRY_MODE_32B);
    fm_byte   start_bit  = ((mode_32b) ? 5 : 6);
    fm_uint16 line       = FM_GET_UNNAMED_FIELD(hash_ram_addr, start_bit, 14);
    fm_byte   start_byte = FM_GET_UNNAMED_FIELD(hash_ram_addr, 0, 5);
    fm_uint32 entry_idx0 = (line*4 + start_byte/8);
    fm_uint32 entry_idx1 = ((hash_ram_addr/8) & 0xFFFF);
    fm_uint32 entry_idx  = (mode_32b) ? entry_idx1 : entry_idx0;
    fm_uint   max_bytes  = (mode_32b) ? (MBY_FFU_MAX_HASH_ENTRY_SIZE / 2) : MBY_FFU_MAX_HASH_ENTRY_SIZE;
    fm_bool   rd_ram_num = (mode_32b) ? 0 : FM_GET_UNNAMED_FIELD(hash_ram_addr, 5, 1);

    if (!mode_32b) // reassign start_byte to 6b to indicate its position in 64B
        start_byte = FM_GET_UNNAMED_FIELD(hash_ram_addr, 0, 6);

    fm_uint rem_bytes = hash_cfg.entry_size[hash_num] * 4 - max_bytes + start_byte;

    fm_uint num_entries = ((max_bytes - start_byte) / 8) + (((max_bytes - start_byte) % 8 == 0) ? 0 : 1)
        + (rem_bytes / 8) + ((rem_bytes % 8 == 0) ? 0 : 1);

    fm_uint key_idx = 0;
    fm_bool rd_ram_ok = TRUE;

    for (fm_uint i = 0; i < num_entries; i++)
    {
        fm_uint col = entry_idx / 8192; // Each col has 8192 entries (= 65536 / 8)

        fm_uint64 hash_entry = 0;

        if (mode_32b)
        {
            fm_byte alloc = FM_GET_UNNAMED_FIELD(ram_alloc[hash_num], col, 1);
            if (alloc != group){
                rd_ram_ok = FALSE;
                break;
            }

#ifdef USE_NEW_CSRS
            hash_entry = getHashEntryRam(      hash_num, entry_idx); // REVISIT!!!
#else
            hash_entry = getHashEntryRam(regs, hash_num, entry_idx);
#endif
            entry_idx++;
        }
        else if (rd_ram_num == 0)
        {
            fm_byte alloc = FM_GET_UNNAMED_FIELD(ram_alloc[rd_ram_num], col, 1);
            if (alloc != group) {
                rd_ram_ok = FALSE;
                break;
            }

#ifdef USE_NEW_CSRS
            hash_entry = getHashEntryRam(      rd_ram_num, entry_idx); // REVISIT!!!
#else
            hash_entry = getHashEntryRam(regs, rd_ram_num, entry_idx);
#endif
            if ((entry_idx % 4) == 3) {
                rd_ram_num = 1;
                entry_idx = (entry_idx / 4) * 4;
            } else
                entry_idx++;
        }
        else
        {
            fm_byte alloc = FM_GET_UNNAMED_FIELD(ram_alloc[rd_ram_num], col, 1);
            if (alloc != group) {
                rd_ram_ok = FALSE;
                break;
            }

#ifdef USE_NEW_CSRS
            hash_entry = getHashEntryRam(      rd_ram_num, entry_idx); // REVISIT!!!
#else
            hash_entry = getHashEntryRam(regs, rd_ram_num, entry_idx);
#endif
            if ((entry_idx % 4) == 3)
                rd_ram_num = 0;

            entry_idx++;
        }

        for (fm_uint k = ((i == 0) ? start_byte % 8 : 0); k < 8; k++) {
            if ((i > 0) && (key_idx >= hash_cfg.entry_size[hash_num] * 4))
                break;
            fm_byte key = (hash_entry >> (8 * (7-k))) & 0xFF;
            hash_ram_key[key_idx] = key;
            key_idx++;
        }
    }

    if (FM_GET_UNNAMED_FIELD(bucket.MASK, bucket_hash, 1) == 0)
        rd_ram_ok = FALSE;

    *ram_data_ok = rd_ram_ok; // output read status
}

static void getHashActions
(
          fm_byte   entry[MBY_FFU_MAX_HASH_ENTRY_SIZE], // aka cam_key[]
    const fm_byte   entry_mode,
    const fm_byte   key_size,
    const fm_uint16 entry_size,
    const fm_bool   is_cam,
    fm_uint32       hash_actions[MBY_FFU_MAX_HASH_ACTIONS]
)
{
    fm_uint max_bytes_num  = (entry_mode == MBY_FFU_HASH_ENTRY_MODE_64B) ? 16 : 8;
    fm_uint bytes_num      = (entry_size > key_size) ? entry_size - key_size : 0;
    fm_uint read_bytes_num = (bytes_num > max_bytes_num) ? max_bytes_num : bytes_num;
    fm_uint act_idx        = (is_cam) ? ((entry_mode == MBY_FFU_HASH_ENTRY_MODE_64B) ? 2 : 0)
                                      : (read_bytes_num/4 - 1);

    for (fm_uint i = 0; i < read_bytes_num/4; i++)
    {
        for (fm_uint j = 0; j < 4; j++) {
            FM_SET_UNNAMED_FIELD(hash_actions[act_idx], 8*j, 8, entry[entry_size - (i*4) - j - 1]);
        }

        if (is_cam)
        {
            switch (i+1)
            {
            case MBY_FFU_HASH_CAM_ETY_7_BITS_63_32:
                act_idx++;
                break;
            case MBY_FFU_HASH_CAM_ETY_6_BITS_31_0:
                act_idx = 0;
                break;
            case MBY_FFU_HASH_CAM_ETY_6_BITS_63_32:
                act_idx = 1;
                break;
            default:
                break;
            }
        }
        else
            act_idx--;
    }
}

static void matchExact // i.e. look up EM hash
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_a_map      * const cgrp_a_map,
    mby_ppe_cgrp_b_map      * const cgrp_b_map,
#else
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyClassifierKeys const * const keys,
    fm_byte const                   scenario,
    fm_byte const                   group,
    mbyClassifierActions    * const actions // = output actions
)
{
    for (fm_uint hash_num = 0; hash_num < MBY_FFU_KEY_MASK0_ENTRIES_1; hash_num++)
    {
        // Initialize actions
        fm_uint32 hash_actions[MBY_FFU_MAX_HASH_ACTIONS] = { 0 };

        // Get FFU_KEY_MASK register fields:
#ifdef USE_NEW_CSRS
        mbyClassifierKeyMaskCfg key_mask_cfg = (group == 0)
            ? getEmAKeyMaskCfg(cgrp_a_map, hash_num, scenario)
            : getEmBKeyMaskCfg(cgrp_b_map, hash_num, scenario);
#else
        mbyClassifierKeyMaskCfg key_mask_cfg = (group == 0)
            ? getEmAKeyMaskCfg(regs,       hash_num, scenario)
            : getEmBKeyMaskCfg(regs,       hash_num, scenario);
#endif
        // Apply key mask on FFU keys:
        mbyClassifierKeys hash_keys;
        applyEmKeyMask(key_mask_cfg, *keys, &hash_keys);

        // Convert Keys into array of bytes:
        fm_byte hash_bytes[MBY_FFU_HASH_KEYS] = { 0 };
        convertKeysToBytes(hash_keys, hash_bytes);

        // Get hash value from CRC:
        fm_uint32 hash = (hash_num == 0) ?
            mbyCrc32ByteSwap (hash_bytes, MBY_FFU_HASH_KEYS) : // HASH0: CRC-32 (Ethernet)
            mbyCrc32CByteSwap(hash_bytes, MBY_FFU_HASH_KEYS) ; // HASH1: CRC-32C (iSCSI)

        // Key Compaction:
        fm_byte packed_keys[MBY_FFU_HASH_KEYS] = { 0 };
        fm_byte key_size = 0;
        doKeyCompaction(key_mask_cfg, hash_keys, packed_keys, &key_size);

        fm_uint16 hash_index =  hash        & 0x1fff;
        fm_uint16 hash_more  = (hash >> 16) & 0xffff;

#ifdef USE_NEW_CSRS
        mbyClassifierHashCfg hash_cfg = (group == 0)
            ? getEmAHashCfg(cgrp_a_map, scenario)
            : getEmBHashCfg(cgrp_b_map, scenario);
#else
        mbyClassifierHashCfg hash_cfg = (group == 0)
            ? getEmAHashCfg(regs, scenario)
            : getEmBHashCfg(regs, scenario);
#endif
        // Don't perform lookups if hash_num is 1 in non-split mode:
        if ((hash_cfg.mode == MBY_FFU_HASH_ENTRY_MODE_64B) && (hash_num == MBY_FFU_KEY_MASK0_ENTRIES_1 - 1))
           break;

        // Don't perform lookups if hash entry size is 0:
        if (hash_cfg.entry_size[hash_num] == 0)
           continue;

        fm_uint16 lookup_ptr = hash_cfg.base_ptr[hash_num]
            + (hash_index % (1uL << hash_cfg.hash_size[hash_num]))
            + ((hash_cfg.mode == MBY_FFU_HASH_ENTRY_MODE_32B && hash_num == 1) ? 4096 : 0);

#ifdef USE_NEW_CSRS
        mbyClassifierHashLookup bucket = (group == 0)
            ? getEmAHashLookupEntry(cgrp_a_map, lookup_ptr)
            : getEmBHashLookupEntry(cgrp_b_map, lookup_ptr);
#else
        mbyClassifierHashLookup bucket  = (group == 0)
            ? getEmAHashLookupEntry(regs, lookup_ptr)
            : getEmBHashLookupEntry(regs, lookup_ptr);
#endif
        fm_byte min_cam_key_size = (hash_cfg.mode == MBY_FFU_HASH_ENTRY_MODE_64B) ?
            (MBY_FFU_MAX_HASH_ENTRY_SIZE   - ( MBY_FFU_MAX_HASH_ACTIONS    * 4)) :
            (MBY_FFU_MAX_HASH_ENTRY_SIZE/2 - ((MBY_FFU_MAX_HASH_ACTIONS/2) * 4)) ;

        fm_bool hash_cam_en = FALSE;

        for (fm_int i = MBY_FFU_HASH_CAM_ENTRIES_1 - 1; i >= 0; i--)
        {
            // Initialize cam keys to 0
            fm_byte cam_key[MBY_FFU_MAX_HASH_ENTRY_SIZE] = { 0 };

            // Build Cam Key
            fm_uint key_idx = 0;
            for (fm_uint j = 0; j < MBY_FFU_HASH_CAM_ENTRIES_0; j++)
            {
                fm_bool entry_64b = (hash_cfg.mode == MBY_FFU_HASH_ENTRY_MODE_64B);
                fm_bool entry_32b = (hash_cfg.mode == MBY_FFU_HASH_ENTRY_MODE_32B);
                fm_bool hashn_32b = ( ((hash_num == 0) && (j < 4)) || ((hash_num == 1) && (j >= 4)) );

                if (entry_64b || (entry_32b && hashn_32b)) {
#ifdef USE_NEW_CSRS
                    fm_uint64 cam_entry = (group == 0)
                        ? getEmAHashCamEntry(cgrp_a_map, i, j)
                        : getEmBHashCamEntry(cgrp_b_map, i, j);
#else
                    fm_uint64 cam_entry = (group == 0)
                        ? getEmAHashCamEntry(regs, i, j)
                        : getEmBHashCamEntry(regs, i, j);
#endif
                    for (fm_uint k = 0; k < 8; k++) {
                        cam_key[key_idx] = (cam_entry >> (8 * (7-k))) & 0xFF;
                        key_idx++;
                    }
                }
            }

            // Compare Cam key with packeyKey
            fm_bool cam_hit = TRUE;
            if (key_size < MBY_FFU_MAX_HASH_ENTRY_SIZE)
            {
                for (fm_uint j = 0; j < key_size; j++) {
                    if (packed_keys[j] != cam_key[j]) {
                       cam_hit = FALSE;
                       break;
                    }
                }
                // If key_size < min_cam_key_size, packed keys need to be padded with 0's to minCamKeySize.
                // packedKeys[keySize...minCamKeySize-1] are 0 in this case, so we can get cam_hit
                // by checking if  cam_key[key_size...min_cam_key_size-1] are 0.
                for (fm_uint j = key_size; j < min_cam_key_size; j++) {
                    if (cam_key[j] != 0) {
                        cam_hit = FALSE;
                        break;
                    }
                }
            }

            if (cam_hit)
            {
#ifdef USE_NEW_CSRS
                fm_uint64 scenario_mask = (group == 0)
                    ? getEmAHashCamMask(cgrp_a_map, hash_num, i)
                    : getEmBHashCamMask(cgrp_b_map, hash_num, i);
#else
                fm_uint64 scenario_mask = (group == 0)
                    ? getEmAHashCamMask(regs, hash_num, i)
                    : getEmBHashCamMask(regs, hash_num, i);
#endif
                hash_cam_en = (scenario_mask >> scenario) & 1;

                // 64B entry mode needs to consider CAM_EN[0/1]
                if (hash_cfg.mode == MBY_FFU_HASH_ENTRY_MODE_64B) {
#ifdef USE_NEW_CSRS
                fm_uint64 scenario_mask = (group == 0)
                    ? getEmAHashCamMask(cgrp_a_map, hash_num + 1, i)
                    : getEmBHashCamMask(cgrp_b_map, hash_num + 1, i);
#else
                fm_uint64 scenario_mask = (group == 0)
                    ? getEmAHashCamMask(regs, hash_num + 1, i)
                    : getEmBHashCamMask(regs, hash_num + 1, i);
#endif
                    hash_cam_en &= (scenario_mask >> scenario) & 1;
                }

                // Get CAM Actions:
                if (hash_cam_en)
                {
                    fm_int cam_entry_size = (hash_cfg.mode == MBY_FFU_HASH_ENTRY_MODE_64B)
                        ?  MBY_FFU_MAX_HASH_ENTRY_SIZE
                        : (MBY_FFU_MAX_HASH_ENTRY_SIZE / 2);

                    fm_bool is_cam = TRUE;
                    getHashActions(cam_key, hash_cfg.mode, key_size, cam_entry_size, is_cam, hash_actions);
                    break;
                }
            }
        }

        fm_bool hash_ram_hit = TRUE;

        // Check if hit Hash Ram, then get actions from Ram
        if (!hash_cam_en)
        {
            fm_byte hash_ram_key[MBY_FFU_MAX_HASH_ENTRY_SIZE] = { 0 };
            fm_bool ram_data_ok = FALSE;

#ifdef USE_NEW_CSRS
            getHashRamData(      group, hash_num, bucket, hash_cfg, hash_more, hash_ram_key, &ram_data_ok); // <-- REVISIT!!!
#else
            getHashRamData(regs, group, hash_num, bucket, hash_cfg, hash_more, hash_ram_key, &ram_data_ok);
#endif
            // Compare Cam key with packeyKey:
            if (key_size < MBY_FFU_MAX_HASH_ENTRY_SIZE) {
                for (fm_uint j = 0; j < key_size; j++) {
                    if (packed_keys[j] != hash_ram_key[j]) {
                        hash_ram_hit = FALSE;
                        break;
                    }
                }
            }

            // bucket is invalid when MASK == 0 or ram_data_ok == 0
            if ((bucket.MASK == 0) || (ram_data_ok == FALSE))
                hash_ram_hit = FALSE;

            if (hash_ram_hit) {
                fm_int  cam_entry_size = hash_cfg.entry_size[hash_num] * 4;
                fm_bool is_cam = FALSE;
                getHashActions(hash_ram_key, hash_cfg.mode, key_size, cam_entry_size, is_cam, hash_actions);
            } else {
                // Get actions from FFU_HASH_MISS register since both cam and ram missed
#ifdef USE_NEW_CSRS
                getHashMissActions(cgrp_a_map, cgrp_b_map, group, hash_cfg, hash_num, scenario, hash_actions);
#else
                getHashMissActions(regs,                   group, hash_cfg, hash_num, scenario, hash_actions);
#endif
            }

        } // if !hash_cam_en

        for (fm_uint i = 0; i < MBY_FFU_MAX_HASH_ACTIONS ; i++)
            doAction(hash_actions[i], actions);

    } // for hash_num
}

static void populateMuxedAction
(
#ifdef USE_NEW_CSRS

#else
    fm_uint32                         regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    const mbyClassifierKeys           keys,
    const mbyClassifierActions        actions,
    const fm_byte                     pri_profile,
    mbyClassifierMuxedAction  * const muxed_action
)
{
    fm_byte mpls_pop  = actions.act4[MBY_FFU_ACTION_MPLS_POP].val;
    fm_byte ecn_ctrl  = actions.act4[MBY_FFU_ACTION_ECN_CTRL].val;
    fm_byte tc_ctrl   = actions.act4[MBY_FFU_ACTION_TC_CTRL].val;
    fm_byte ttl_ctrl  = actions.act4[MBY_FFU_ACTION_TTL_CTRL].val;
    fm_byte dscp_ctrl = actions.act4[MBY_FFU_ACTION_DSCP_CTRL].val;

    // Update ECN:
    muxed_action->ecn         = 0;
    muxed_action->aqm_mark_en = 0;

    fm_byte exp  = 0;
    fm_byte dscp = 0;

    if (ecn_ctrl < 4) // FFU directly specifies ECN value:
    {
        muxed_action->ecn         = (ecn_ctrl == 2) ? 1 : ecn_ctrl;
        muxed_action->aqm_mark_en = (ecn_ctrl == 2) ? 1 : 0;
    }
    else if (ecn_ctrl < 16) // Define ECN source and marking rules:
    {
        switch (ecn_ctrl & 3) // ECN
        {
            case 0: muxed_action->ecn = FM_GET_UNNAMED_FIELD(keys.key8[MBY_FFU_KEY8_OUTER_DS], 0, 2); break;
            case 1: muxed_action->ecn = FM_GET_UNNAMED_FIELD(keys.key8[MBY_FFU_KEY8_INNER_DS], 0, 2); break;

            case 2: // ECN-CTRL[1:0] = 2 : ECN source is MPLS label 1, i.e. MPLS_MUX_EXP_DS[mpls_labels[0].exp].ecn
            {
                exp = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL1_1], 9, 3);
#ifdef USE_NEW_CSRS
                muxed_action->ecn = 0; // FIXME!!!
#else
                fm_uint64 mpls_mux_exp_ds_reg = 0;
                mbyModelReadCSR64(regs, MBY_MPLS_MUX_EXP_DS(((pri_profile << 3) | exp), 0), &mpls_mux_exp_ds_reg);
                muxed_action->ecn = FM_GET_FIELD64(mpls_mux_exp_ds_reg, MBY_MPLS_MUX_EXP_DS, ECN);
#endif
                break;
            }
            case 3: // ECN_CTRL[1:0]=3: ECN source is MPLS label exposed after MPLS_POP
            {
                exp = (mpls_pop < 4) ? FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL1_1 + (mpls_pop * 2)], 9, 3) :
                      (mpls_pop < 6) ? FM_GET_UNNAMED_FIELD(keys.key8[MBY_FFU_KEY8_MPLS_LABEL5_2 + ((mpls_pop - 4) * 4)], 1, 3) : 0;
#ifdef USE_NEW_CSRS
                muxed_action->ecn = 0; // FIXME!!!
#else
                fm_uint64 mpls_mux_exp_ds_reg = 0;
                mbyModelReadCSR64(regs, MBY_MPLS_MUX_EXP_DS(((pri_profile << 3) | exp), 0), &mpls_mux_exp_ds_reg);
                muxed_action->ecn = FM_GET_FIELD64(mpls_mux_exp_ds_reg, MBY_MPLS_MUX_EXP_DS, ECN);
#endif
                break;
            }

            default: break;
        }

        // ECN_CTRL[3:2]=3: mark in FFU if ingress ECN is 01 or 10 (aqm_mark_en=0):
        if ((((ecn_ctrl >> 2) & 3) == 3) && (muxed_action->ecn == 1 || muxed_action->ecn == 2))
            muxed_action->ecn = 3;

        // ECN marking:
        muxed_action->aqm_mark_en = (((ecn_ctrl >> 2) & 3) == 2);
    }

    // Update DSCP:
    muxed_action->dscp = 0;

    switch (dscp_ctrl)
    {
        case  0: muxed_action->dscp = (actions.act4[MBY_FFU_ACTION_DSCP_LOW ].val & 0xF) |
                                     ((actions.act4[MBY_FFU_ACTION_DSCP_HIGH].val & 0xF) << 4); break;

        case  4: muxed_action->dscp = FM_GET_UNNAMED_FIELD(keys.key8[MBY_FFU_KEY8_OUTER_DS], 2, 6); break;
        case  5: muxed_action->dscp = FM_GET_UNNAMED_FIELD(keys.key8[MBY_FFU_KEY8_INNER_DS], 2, 6); break;

        case  6: exp = (mpls_pop < 4) ? FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL1_1 + (mpls_pop * 2)], 9, 3) :
                       (mpls_pop < 6) ? FM_GET_UNNAMED_FIELD(keys.key8[MBY_FFU_KEY8_MPLS_LABEL5_2  + ((mpls_pop - 4) * 4)], 1, 3) : 0; break;

        case  8: exp = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL1_1], 9, 3); break;
        case  9: exp = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL2_1], 9, 3); break;
        case 10: exp = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL3_1], 9, 3); break;
        case 11: exp = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL4_1], 9, 3); break;

        default: break;
    }

    if (dscp_ctrl == 6 || ((dscp_ctrl >= 8) && (dscp_ctrl <= 11))) {
#ifdef USE_NEW_CSRS
        muxed_action->dscp = 0; // FIXME!!!
#else
        fm_uint64 mpls_mux_exp_ds_reg = 0;
        mbyModelReadCSR64(regs, MBY_MPLS_MUX_EXP_DS(((pri_profile << 3) | exp), 0), &mpls_mux_exp_ds_reg);
        muxed_action->dscp = FM_GET_FIELD64(mpls_mux_exp_ds_reg, MBY_MPLS_MUX_EXP_DS, DSCP);
#endif
    }

    // Update SWPRI:
    muxed_action->swpri = 0;

    switch (tc_ctrl)
    {
        case  0: muxed_action->swpri = actions.act4[MBY_FFU_ACTION_TC].val; break;

        case  4:
        {
            dscp = FM_GET_UNNAMED_FIELD(keys.key8[MBY_FFU_KEY8_OUTER_DS], 2, 6);
#ifdef USE_NEW_CSRS
            muxed_action->swpri = 0; // FIXME!!!
#else
            fm_uint64 mpls_mux_dscp_tc_reg = 0;
            mbyModelReadCSR64(regs, MBY_MPLS_MUX_DSCP_TC(dscp, 0), &mpls_mux_dscp_tc_reg);
            muxed_action->swpri = FM_GET_FIELD64(mpls_mux_dscp_tc_reg, MBY_MPLS_MUX_DSCP_TC, TC);
#endif
            break;
        }
        case  5:
        {
            dscp = FM_GET_UNNAMED_FIELD(keys.key8[MBY_FFU_KEY8_INNER_DS], 2, 6);
#ifdef USE_NEW_CSRS
            muxed_action->swpri = 0; // FIXME!!!
#else
            fm_uint64 mpls_mux_dscp_tc_reg = 0;
            mbyModelReadCSR64(regs, MBY_MPLS_MUX_DSCP_TC(dscp, 0), &mpls_mux_dscp_tc_reg);
            muxed_action->swpri = FM_GET_FIELD64(mpls_mux_dscp_tc_reg, MBY_MPLS_MUX_DSCP_TC, TC);
#endif
            break;
        }
    case  6: exp = (mpls_pop < 4) ? FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL1_1 + (mpls_pop * 2)],   9, 3) :
                   (mpls_pop < 6) ? FM_GET_UNNAMED_FIELD(keys.key8 [MBY_FFU_KEY8_MPLS_LABEL5_2 + ((mpls_pop - 4) * 4)], 1, 3) : 0; break;

        case  8: exp = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL1_1], 9, 3); break;
        case  9: exp = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL2_1], 9, 3); break;
        case 10: exp = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL3_1], 9, 3); break;
        case 11: exp = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL4_1], 9, 3); break;

        default: break;
    }

    if (tc_ctrl == 6 || (tc_ctrl >= 8 && tc_ctrl <= 11)) {
#ifdef USE_NEW_CSRS
        muxed_action->swpri = 0; // FIXME!!!
#else
        fm_uint64 mpls_mux_exp_ds_reg = 0;
        mbyModelReadCSR64(regs, MBY_MPLS_MUX_EXP_DS(((pri_profile << 3) | exp), 0), &mpls_mux_exp_ds_reg);
        muxed_action->swpri = FM_GET_FIELD64(mpls_mux_exp_ds_reg, MBY_MPLS_MUX_EXP_DS, TC);
#endif
    }

    // get TTL value based on TTL_CTRL Action:
    fm_byte ttl = 0;

    switch (ttl_ctrl)
    {
        case  0: ttl = keys.key8[MBY_FFU_KEY8_OUTER_TTL]; break;
        case  1: ttl = keys.key8[MBY_FFU_KEY8_INNER_TTL]; break;
        case  2: ttl = (mpls_pop < 4) ? FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL1_1 + (mpls_pop * 2)], 0, 8) :
                       (mpls_pop < 6) ? keys.key8[MBY_FFU_KEY8_MPLS_LABEL5_3 + ((mpls_pop - 4) * 4)] : 0; break;
        case  4: ttl = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL1_1], 0, 8); break;
        case  5: ttl = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL2_1], 0, 8); break;
        case  6: ttl = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL3_1], 0, 8); break;
        case  7: ttl = FM_GET_UNNAMED_FIELD(keys.key16[MBY_FFU_KEY16_MPLS_LABEL4_1], 0, 8); break;

        default: break;
    }

    muxed_action->ttl01 = 0;

    if (ttl == 0)
        FM_SET_UNNAMED_FIELD(muxed_action->ttl01, 0, 1, 1)
    else if (ttl == 1)
        FM_SET_UNNAMED_FIELD(muxed_action->ttl01, 1, 1, 1)

    muxed_action->ttl_ctrl = ttl_ctrl;

    muxed_action->vpri = (actions.act4[MBY_FFU_ACTION_VPRI_LOW].prec >= actions.act4[MBY_FFU_ACTION_VPRI_HIGH].prec) ?
        actions.act4[MBY_FFU_ACTION_VPRI_LOW].val : actions.act4[MBY_FFU_ACTION_VPRI_HIGH].val;

    muxed_action->route = (actions.act1[MBY_FFU_ACTION_NO_ROUTE].val == 0 && actions.act24[MBY_FFU_ACTION_FWD].val != 0);
}

static void populateEntropy
(
#ifdef USE_NEW_CSRS
    mby_ppe_entropy_map        * const entropy_map,
#else
    fm_uint32                          regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyClassifierKeys const            keys,
    mbyClassifierActions const         actions,
    fm_uint32                  * const ecmp_hash,
    fm_uint64                  * const mod_meta
)
{
    fm_byte   hash_profiles[2] = { 0 };
    fm_uint32 hash_values  [2] = { 0 };

    for (fm_uint hash_num = 0; hash_num < 2; hash_num++)
    {
        fm_byte val0 = (actions.act4[MBY_FFU_ACTION_HASH_PROFILE_ECMP_0 + (2 * hash_num)].val);
        fm_byte val1 = (actions.act4[MBY_FFU_ACTION_HASH_PROFILE_ECMP_1 + (2 * hash_num)].val & 0x3);
        fm_byte prof = (val1 << 4) | val0;

        hash_profiles[hash_num] = prof;

        // Get FFU_KEY_MASK register fields:
#ifdef USE_NEW_CSRS
        mbyClassifierEntropyCfg entropy_cfg = getEntropyCfg(entropy_map, hash_num, prof);
#else
        mbyClassifierEntropyCfg entropy_cfg = getEntropyCfg(regs,        hash_num, prof);
#endif
        // Apply key mask on FFU keys:
        mbyClassifierKeys hash_keys;
        applyEntropyKeyMask(entropy_cfg, keys, &hash_keys);

        // Convert Keys into array of bytes:
        fm_byte hash_bytes[MBY_FFU_HASH_KEYS] = { 0 };
        convertKeysToBytes(hash_keys, hash_bytes);

        // Get hash value from CRC:
         hash_values[hash_num] = (hash_num == 0) ?
            mbyCrc32ByteSwap (hash_bytes, MBY_FFU_HASH_KEYS) : // HASH0: CRC-32 (Ethernet)
            mbyCrc32CByteSwap(hash_bytes, MBY_FFU_HASH_KEYS) ; // HASH1: CRC-32C (iSCSI)
    }


    // ECMP HASH for ARP_TABLE:
    *ecmp_hash = hash_values[0] & 0xFFFFFF;

    // Populate MOD_META for use by the Modifier:
#ifdef USE_NEW_CSRS
    mbyEntropyMetaCfg meta_cfg = getEntropyMetaCfg(entropy_map, hash_profiles[1]);
#else
    mbyEntropyMetaCfg meta_cfg = getEntropyMetaCfg(regs,        hash_profiles[1]);
#endif

    fm_uint64 mod_meta_l = 0; // local var

    // Apply Defaults to MOD_META:
    for (fm_uint i = 0; i < 6; i++)
    {
        fm_byte s = FM_GET_UNNAMED_FIELD(meta_cfg.BYTE_DEFAULTS, i*2, 2);
        switch (s)
        {
            case 0:
                FM_SET_UNNAMED_FIELD64(mod_meta_l, i*8,     4, actions.act4[MBY_FFU_ACTION_META0_LOW ].val);
                FM_SET_UNNAMED_FIELD64(mod_meta_l, i*8 + 4, 4, actions.act4[MBY_FFU_ACTION_META0_HIGH].val);
                break;
            case 1:
                FM_SET_UNNAMED_FIELD64(mod_meta_l, i*8,     4, actions.act4[MBY_FFU_ACTION_META1_LOW ].val);
                FM_SET_UNNAMED_FIELD64(mod_meta_l, i*8 + 4, 4, actions.act4[MBY_FFU_ACTION_META1_HIGH].val);
                break;
            case 2:
                FM_SET_UNNAMED_FIELD64(mod_meta_l, i*8,     4, actions.act4[MBY_FFU_ACTION_META2_LOW ].val);
                FM_SET_UNNAMED_FIELD64(mod_meta_l, i*8 + 4, 4, actions.act4[MBY_FFU_ACTION_META2_HIGH].val);
                break;
            case 3:
                FM_SET_UNNAMED_FIELD64(mod_meta_l, i*8,     8, 0);
                break;
        }
    }

    // Apply Hash to MOD_META:
    fm_uint64 meta_mask = 0;
    for (fm_uint i = 0; i < 48; i++)
        if ((i >= meta_cfg.HASH_START) && (i < (meta_cfg.HASH_START + meta_cfg.HASH_SIZE)))
            meta_mask |= (FM_LITERAL_U64(1) << i);

    fm_uint64 meta_hash = (((fm_uint64) hash_values[1]) << 32) | hash_values[1];

    *mod_meta = (mod_meta_l & ~meta_mask) | (meta_hash & meta_mask);
}

static fm_macaddr extractDmac(const fm_uint32 ip_lo, const fm_uint32 ip_hi)
{
    fm_macaddr mac_addr =  ((fm_macaddr)( ip_lo & 0xFFFFFF  )) |
                          (((fm_macaddr)( ip_hi & 0xFDFFFF00)) << 16) |
                          (((fm_macaddr)(~ip_hi & 0x2000000 )) << 16) ;
    return mac_addr;
}

static void transformActions
(
    mbyClassifierKeys          const keys,
    mbyClassifierActions       const actions,
    mbyClassifierMuxedAction   const muxed_action,
    fm_bool                    const ip_option[2],
    mbyParserInfo              const parser_info,
    fm_uint32                  const ecmp_hash,
    fm_uint64                  const mod_meta,
    fm_bool                  * const decap,
    fm_bool                  * const encap,
    fm_byte                  * const mod_idx,
    fm_uint16                * const l2_etype,
    fm_byte                  * const mpls_pop,
    fm_uint64                * const sglort,
    fm_uint64                * const idglort,
    fm_macaddr               * const l2_smac,
    fm_macaddr               * const l2_dmac,
    fm_macaddr               * const dmac_from_ipv6,
    fm_bool                  * const is_ipv4,
    fm_bool                  * const is_ipv6,
    fm_uint16                * const l3_length,
    fm_uint16                * const inner_l3_length,
    fm_uint16                * const outer_l3_length,
    fm_bool                  * const trap_ip_options,
    fm_bool                  * const drop_ttl,
    fm_bool                  * const trap_icmp,
    fm_bool                  * const trap_igmp,
    mbyClassifierFlags       * const ffu_flags,
    fm_uint32                * const ffu_route,
    fm_bool                  * const no_learn,
    fm_uint16                * const l2_ivid1,
    fm_byte                  * const qos_l2_vpri1,
    fm_byte                  * const ffu_trig,
    fm_uint32                * const policer_action
)
{
    *decap    = (actions.act24[MBY_FFU_ACTION_MOD_IDX ].val >> 1) & 0x1;
    *encap    =  actions.act24[MBY_FFU_ACTION_MOD_IDX ].val & 0x1;
    *mod_idx  = (actions.act24[MBY_FFU_ACTION_MOD_IDX ].val >> 2) & 0xFFFF;
    *mpls_pop =  actions.act4 [MBY_FFU_ACTION_MPLS_POP].val;

    *sglort = 0;
    FM_SET_UNNAMED_FIELD64(*sglort,  0,  8, keys.key8[(MBY_RE_KEYS_SGLORT - MBY_RE_KEYS_GENERAL_8B)*2 + 1]);
    FM_SET_UNNAMED_FIELD64(*sglort,  8, 16, keys.key8[(MBY_RE_KEYS_SGLORT - MBY_RE_KEYS_GENERAL_8B)*2    ]);

    *idglort = 0;
    FM_SET_UNNAMED_FIELD64(*idglort, 0,  8, keys.key8[(MBY_RE_KEYS_DGLORT - MBY_RE_KEYS_GENERAL_8B)*2 + 1]);
    FM_SET_UNNAMED_FIELD64(*idglort, 8, 16, keys.key8[(MBY_RE_KEYS_DGLORT - MBY_RE_KEYS_GENERAL_8B)*2    ]);

    *l2_smac = 0;
    fm_uint    l2_smac_off = (*decap) ? MBY_RE_KEYS_INNER_SMAC : MBY_RE_KEYS_OUTER_SMAC;
    for (fm_uint i = 0; i <= 2; i++)
        FM_SET_UNNAMED_FIELD64(*l2_smac, (16 * i), 16, keys.key16[l2_smac_off + (2 - i)])

    *l2_dmac  = 0;
    fm_uint   l2_dmac_off = (*decap) ? MBY_RE_KEYS_INNER_DMAC : MBY_RE_KEYS_OUTER_DMAC;
    for (fm_uint i = 0; i <= 2; i++)
        FM_SET_UNNAMED_FIELD64(*l2_dmac, (16 * i), 16, keys.key16[l2_dmac_off + (2 - i)])

    *l2_etype = (*decap) ? keys.key16[MBY_RE_KEYS_INNER_ETYPE] : keys.key16[MBY_RE_KEYS_OUTER_ETYPE];

    fm_uint16  ip_prot = (*decap) ? keys.key8[MBY_FFU_KEY8_INNER_PROT] : keys.key8[MBY_FFU_KEY8_OUTER_PROT];

    fm_uint    dmac_ipv6_off_lo = (*decap) ? MBY_FFU_KEY32_INNER_DIP_31_0  : MBY_FFU_KEY32_OUTER_DIP_31_0;
    fm_uint    dmac_ipv6_off_hi = (*decap) ? MBY_FFU_KEY32_INNER_DIP_63_32 : MBY_FFU_KEY32_OUTER_DIP_63_32;

    *dmac_from_ipv6 = extractDmac(keys.key32[dmac_ipv6_off_lo], keys.key32[dmac_ipv6_off_hi]);

    *is_ipv4 = (*decap) ? (parser_info.inr_l3_len && !parser_info.inr_l3_v6)
                        : (parser_info.otr_l3_len && !parser_info.otr_l3_v6);

    *is_ipv6 = (*decap) ? (parser_info.inr_l3_len &&  parser_info.inr_l3_v6)
                        : (parser_info.otr_l3_len &&  parser_info.otr_l3_v6);

    fm_uint32 l3_length_off = (*decap) ? MBY_FFU_KEY8_INNER_LEN : MBY_FFU_KEY8_OUTER_LEN;
    *l3_length = 0;
    FM_SET_UNNAMED_FIELD64(*l3_length, 0, 8, keys.key8[l3_length_off + 1]);
    FM_SET_UNNAMED_FIELD64(*l3_length, 8, 8, keys.key8[l3_length_off    ]);

    *trap_ip_options = ip_option[*decap];

    fm_bool is_ttl01 = (muxed_action.ttl01 & 1) || ((muxed_action.ttl01 >> 1) & 1);

    *drop_ttl = is_ttl01 && (*is_ipv4 || *is_ipv6);

    *trap_icmp = drop_ttl &&
        ((*is_ipv4 && (ip_prot == MBY_PROT_ICMPv4)) || (*is_ipv6 && (ip_prot == MBY_PROT_ICMPv6)));

    *trap_igmp = *is_ipv4 && (ip_prot == MBY_PROT_IGMP);

    *inner_l3_length = 0;
    fm_uint inner_index = (MBY_RE_KEYS_INNER_IP_LEN - MBY_RE_KEYS_GENERAL_8B) * 2 + 1;
    FM_SET_UNNAMED_FIELD64(*inner_l3_length, 0, 8, keys.key8[inner_index]);
    FM_SET_UNNAMED_FIELD64(*inner_l3_length, 8, 8, keys.key8[inner_index - 1]);

    *outer_l3_length = 0;
    fm_uint outer_index = (MBY_RE_KEYS_OUTER_IP_LEN - MBY_RE_KEYS_GENERAL_8B) * 2 + 1;
    FM_SET_UNNAMED_FIELD64(*outer_l3_length, 0, 8, keys.key8[outer_index]);
    FM_SET_UNNAMED_FIELD64(*outer_l3_length, 8, 8, keys.key8[outer_index - 1]);

    ffu_flags->drop         = actions.act1[MBY_FFU_ACTION_DROP     ].val;
    ffu_flags->trap         = actions.act1[MBY_FFU_ACTION_TRAP     ].val;
    ffu_flags->log          = actions.act1[MBY_FFU_ACTION_LOG      ].val;
    ffu_flags->no_route     = actions.act1[MBY_FFU_ACTION_NO_ROUTE ].val;
    ffu_flags->rx_mirror    = actions.act1[MBY_FFU_ACTION_RX_MIRROR].val;
    ffu_flags->capture_time = actions.act1[MBY_FFU_ACTION_CAPT_TIME].val;

    // FIXME cppcheck (error) Uninitialized struct member: ffu_flags.tx_tag
    for (fm_uint i = 0; i <= 1; i++)
        FM_SET_UNNAMED_FIELD(ffu_flags->tx_tag, i, 1, actions.act1[i+MBY_FFU_ACTION_TX_TAG0].val);

    *ffu_route = 0;
    FM_SET_UNNAMED_FIELD64(*ffu_route, 0, 22, FM_GET_UNNAMED_FIELD64(actions.act24[MBY_FFU_ACTION_FWD].val, 0, 22));

    *no_learn = FALSE;
    FM_SET_UNNAMED_FIELD(*no_learn, 0, 1, ~actions.act1[MBY_FFU_ACTION_LEARN].val);

    *l2_ivid1 = 0;
    if (*decap && actions.act4[MBY_FFU_ACTION_VID_LOW].prec <= 1)
        *l2_ivid1 = keys.key16[MBY_RE_KEYS_INNER_VLAN1];
    else
        for (fm_uint i = 0; i <= (MBY_FFU_ACTION_VID_HIGH - MBY_FFU_ACTION_VID_LOW); i++)
            FM_SET_UNNAMED_FIELD(*l2_ivid1, i * 4, 4, actions.act4[MBY_FFU_ACTION_VID_LOW+i].val);

    fm_bool qos_l2_vpri1_sel = *decap
        && (actions.act4[MBY_FFU_ACTION_VPRI_LOW ].val <= 1)
        && (actions.act4[MBY_FFU_ACTION_VPRI_HIGH].val <= 1);

    fm_byte qos_l2_vpri1_decap = (actions.act1[MBY_FFU_ACTION_COPY_OTR_VPRI].val)
           ? ((keys.key16[MBY_RE_KEYS_OUTER_VLAN1] >> 12) & 0xF)
           : ((keys.key16[MBY_RE_KEYS_INNER_VLAN1] >> 12) & 0xF);

    *qos_l2_vpri1 = (qos_l2_vpri1_sel) ? qos_l2_vpri1_decap : muxed_action.vpri;

    *ffu_trig = 0;
    for (fm_uint i = MBY_FFU_ACTION_TRIGGER0; i <= MBY_FFU_ACTION_TRIGGER7; i++)
        FM_SET_UNNAMED_FIELD(*ffu_trig, i - MBY_FFU_ACTION_TRIGGER0, 1, actions.act1[i].val);

    for (fm_uint i = MBY_FFU_ACTION_POLICER0; i <= MBY_FFU_ACTION_POLICER3; i++)
        policer_action[i] = actions.act24[i].val;
}

void Classifier
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_a_map          * const cgrp_a_map,
    mby_ppe_cgrp_b_map          * const cgrp_b_map,
    mby_ppe_entropy_map         * const entropy_map,
#else
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyMapperToClassifier const * const in,
    mbyClassifierToHash         * const out
)
{
    // Read inputs from the Mapper:
    const mbyClassifierActions  actions_in  = in->FFU_ACTIONS;
    const mbyClassifierKeys     keys        = in->FFU_KEYS;
    const fm_byte               scenario_in = in->FFU_SCENARIO;
    const fm_bool * const       ip_option   = in->IP_OPTION;
    const mbyParserInfo         parser_info = in->PARSER_INFO;
    const fm_byte               pri_profile = in->PRIORITY_PROFILE;

    fm_byte              scenario = scenario_in;
    mbyClassifierActions actions  = actions_in;

    // Use group = 0 for now (MBY spec group = 0..1) <-- FIXME!!!!
    for (fm_byte group = 0; group < 1; group++)
    {
        // Perform wildcard match (WCM) lookup:
#ifdef USE_NEW_CSRS
        matchWildcard(cgrp_b_map, &keys, scenario, group, &actions);
#else
        matchWildcard(regs,       &keys, scenario, group, &actions);
#endif
        // Perform exact match (EM) lookup:
#ifdef USE_NEW_CSRS
        matchExact(cgrp_a_map, cgrp_b_map, &keys, scenario, group, &actions);
#else
        matchExact(regs,                   &keys, scenario, group, &actions);
#endif

#if 0
        // Remap subset of keys going into next group:
        remapKeys(...);
#endif
        // Update scenario based on scenario action:
        for (fm_uint s = MBY_FFU_ACTION_SCENARIO0, i = 0; s <= MBY_FFU_ACTION_SCENARIO5; s++, i++) {
            if (actions.act1[s].prec != 0)
                FM_SET_UNNAMED_FIELD(scenario, i, 1, actions.act1[s].val & 1);
        }
    }

    // Populate muxed_action:
    mbyClassifierMuxedAction muxed_action;

    populateMuxedAction
    (
#ifdef USE_NEW_CSRS

#else
        regs,
#endif
        keys,
        actions,
        pri_profile,
        &muxed_action
    );

    // Populate ecmp_hash and mod_meta:
    fm_uint32 ecmp_hash = 0;
    fm_uint64 mod_meta  = 0;

    populateEntropy
    (
#ifdef USE_NEW_CSRS
        entropy_map,
#else
        regs,
#endif
        keys,
        actions,
        &ecmp_hash,
        &mod_meta
    );

    // Transform exisiting keys, actions, etc. into desired output:

    fm_bool                  decap           = FALSE;
    fm_bool                  encap           = FALSE;
    fm_byte                  mod_idx         = 0;
    fm_uint16                l2_etype        = 0;
    fm_byte                  mpls_pop        = 0;
    fm_uint64                sglort          = 0;
    fm_uint64                idglort         = 0;
    fm_macaddr               l2_smac         = 0;
    fm_macaddr               l2_dmac         = 0;
    fm_macaddr               dmac_from_ipv6  = 0;
    fm_bool                  is_ipv4         = FALSE;
    fm_bool                  is_ipv6         = FALSE;
    fm_uint16                l3_length       = 0;
    fm_uint16                inner_l3_length = 0;
    fm_uint16                outer_l3_length = 0;
    fm_bool                  trap_ip_options = FALSE;
    fm_bool                  drop_ttl        = FALSE;
    fm_bool                  trap_icmp       = FALSE;
    fm_bool                  trap_igmp       = FALSE;
    mbyClassifierFlags       ffu_flags       = { 0 };
    fm_uint32                ffu_route       = 0;
    fm_bool                  no_learn        = FALSE;
    fm_uint16                l2_ivid1        = 0;
    fm_byte                  qos_l2_vpri1    = 0;
    fm_byte                  ffu_trig        = 0;

    fm_uint32                policer_action[MBY_FFU_ACTION_POLICER3 + 1] = { 0 };

    transformActions
    (
        keys,
        actions,
        muxed_action,
        ip_option,
        parser_info,
        ecmp_hash,
        mod_meta,
        &decap,
        &encap,
        &mod_idx,
        &l2_etype,
        &mpls_pop,
        &sglort,
        &idglort,
        &l2_smac,
        &l2_dmac,
        &dmac_from_ipv6,
        &is_ipv4,
        &is_ipv6,
        &l3_length,
        &inner_l3_length,
        &outer_l3_length,
        &trap_ip_options,
        &drop_ttl,
        &trap_icmp,
        &trap_igmp,
        &ffu_flags,
        &ffu_route,
        &no_learn,
        &l2_ivid1,
        &qos_l2_vpri1,
        &ffu_trig,
        policer_action
    );

    // Write outputs:

    // Write outputs:
    out->AQM_MARK_EN      = muxed_action.aqm_mark_en;
    out->DECAP            = decap;
    out->DMAC_FROM_IPV6   = dmac_from_ipv6;
    out->DROP_TTL         = drop_ttl;
    out->ECN              = muxed_action.ecn;
    out->ENCAP            = encap;
    out->FFU_FLAGS        = ffu_flags;
    out->FFU_ROUTE        = ffu_route;
    out->FFU_TRIG         = ffu_trig;
    out->IDGLORT          = idglort;
    out->INNER_L3_LENGTH  = inner_l3_length;
    out->IS_IPV4          = is_ipv4;
    out->IS_IPV6          = is_ipv6;
    out->L2_DMAC          = l2_dmac;
    out->L2_ETYPE         = l2_etype;
    out->L2_IVID1         = l2_ivid1;
    out->L2_SMAC          = l2_smac;
    out->L34_HASH         = ecmp_hash;
    out->L3_LENGTH        = l3_length;
    out->MOD_IDX          = mod_idx;
    out->MPLS_POP         = mpls_pop;
    out->NO_LEARN         = no_learn;
    out->OUTER_L3_LENGTH  = outer_l3_length;
    out->PARSER_INFO      = parser_info;

    for (fm_uint i = MBY_FFU_ACTION_POLICER0; i <= MBY_FFU_ACTION_POLICER3; i++)
        out->POLICER_ACTION[i] = policer_action[i];

    out->QOS_L2_VPRI1     = qos_l2_vpri1;
    out->QOS_L3_DSCP      = muxed_action.dscp;
    out->QOS_SWPRI        = muxed_action.swpri;
    out->SGLORT           = sglort;
    out->TRAP_ICMP        = trap_icmp;
    out->TRAP_IGMP        = trap_igmp;
    out->TRAP_IP_OPTIONS  = trap_ip_options;
    out->TTL_CTRL         = muxed_action.ttl_ctrl;
    out->TX_TAG           = ffu_flags.tx_tag;

    // Pass thru:

    out->LEARN_MODE       = in->LEARN_MODE;
    out->L2_IDOMAIN       = in->L2_IDOMAIN;
    out->L3_IDOMAIN       = in->L3_IDOMAIN;
    out->PARITY_ERROR     = in->PARITY_ERROR;
    out->PARSER_ERROR     = in->PARSER_ERROR;
    out->PA_DROP          = in->PA_DROP;
    out->PA_L3LEN_ERR     = in->PA_L3LEN_ERR;
    out->RX_DATA          = in->RX_DATA;
    out->RX_LENGTH        = in->RX_LENGTH;
    out->RX_PORT          = in->RX_PORT;
    out->TRAFFIC_CLASS    = in->TRAFFIC_CLASS;
}
