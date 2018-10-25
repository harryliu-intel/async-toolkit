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

    key_mask_cfg.KEY_MASK_SEL = cgrp_a_map->EM.KEY_SEL1[hash_num][scenario].KEY_MASK_SEL; // [51:48]
    key_mask_cfg.KEY32_MASK   = cgrp_a_map->EM.KEY_SEL1[hash_num][scenario].KEY32_MASK;   // [47:32]
    key_mask_cfg.KEY16_MASK   = cgrp_a_map->EM.KEY_SEL1[hash_num][scenario].KEY16_MASK;   // [31: 0]
    key_mask_cfg.KEY8_MASK    = cgrp_a_map->EM.KEY_SEL0[hash_num][scenario].KEY8_MASK;    // [31: 0]
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

    key_mask_cfg.KEY_MASK_SEL = cgrp_b_map->EM.KEY_SEL1[hash_num][scenario].KEY_MASK_SEL; // [51:48]
    key_mask_cfg.KEY32_MASK   = cgrp_b_map->EM.KEY_SEL1[hash_num][scenario].KEY32_MASK;   // [47:32]
    key_mask_cfg.KEY16_MASK   = cgrp_b_map->EM.KEY_SEL1[hash_num][scenario].KEY16_MASK;   // [31: 0]
    key_mask_cfg.KEY8_MASK    = cgrp_b_map->EM.KEY_SEL0[hash_num][scenario].KEY8_MASK;    // [31: 0]
    return key_mask_cfg;
}

mbyClassifierHashCfg mbyClsGetEmAHashCfg
(
    MBY_CGRP_A_IN_REGS,
    fm_byte              const scenario
)
{
    mbyClassifierHashCfg hash_cfg;

    em_hash_cfg_r const * const em_a_hash_cfg = &(cgrp_a_map->EM.HASH_CFG[scenario]);

    hash_cfg.mode          = em_a_hash_cfg->MODE;
    hash_cfg.base_ptr[0]   = em_a_hash_cfg->BASE_PTR_0;
    hash_cfg.base_ptr[1]   = em_a_hash_cfg->BASE_PTR_1;
    hash_cfg.hash_size[0]  = em_a_hash_cfg->HASH_SIZE_0;
    hash_cfg.hash_size[1]  = em_a_hash_cfg->HASH_SIZE_1;
    hash_cfg.entry_size[0] = em_a_hash_cfg->ENTRY_SIZE_0;
    hash_cfg.entry_size[1] = em_a_hash_cfg->ENTRY_SIZE_1;
    return hash_cfg;
}

mbyClassifierHashCfg mbyClsGetEmBHashCfg
(
    MBY_CGRP_B_IN_REGS,
    fm_byte              const scenario
)
{
    mbyClassifierHashCfg hash_cfg;

    em_hash_cfg_r const * const em_b_hash_cfg = &(cgrp_b_map->EM.HASH_CFG[scenario]);

    hash_cfg.mode          = em_b_hash_cfg->MODE;
    hash_cfg.base_ptr[0]   = em_b_hash_cfg->BASE_PTR_0;
    hash_cfg.base_ptr[1]   = em_b_hash_cfg->BASE_PTR_1;
    hash_cfg.hash_size[0]  = em_b_hash_cfg->HASH_SIZE_0;
    hash_cfg.hash_size[1]  = em_b_hash_cfg->HASH_SIZE_1;
    hash_cfg.entry_size[0] = em_b_hash_cfg->ENTRY_SIZE_0;
    hash_cfg.entry_size[1] = em_b_hash_cfg->ENTRY_SIZE_1;
    return hash_cfg;
}

mbyClassifierHashLookup mbyClsGetEmAHashLookupEntry
(
    MBY_CGRP_A_IN_REGS,
    fm_uint16            const lookup_ptr
)
{
    mbyClassifierHashLookup lookup_entry;

    em_hash_lookup_r const * const em_a_hash_lookup_entry = &(cgrp_a_map->A.EM_HASH_LOOKUP[lookup_ptr]);

    lookup_entry.PTR      = em_a_hash_lookup_entry->PTR;
    lookup_entry.SELECT_4 = em_a_hash_lookup_entry->SELECT_4;
    lookup_entry.SELECT_3 = em_a_hash_lookup_entry->SELECT_3;
    lookup_entry.SELECT_2 = em_a_hash_lookup_entry->SELECT_2;
    lookup_entry.SELECT_1 = em_a_hash_lookup_entry->SELECT_1;
    lookup_entry.SELECT_0 = em_a_hash_lookup_entry->SELECT_0;
    lookup_entry.MASK     = em_a_hash_lookup_entry->MASK;
    return lookup_entry;
}

mbyClassifierHashLookup mbyClsGetEmBHashLookupEntry
(
    MBY_CGRP_B_IN_REGS,
    fm_uint16            const lookup_ptr
)
{
    mbyClassifierHashLookup lookup_entry;

    em_b_hash_lookup_r const * const em_b_hash_lookup_entry = &(cgrp_b_map->B.EM_HASH_LOOKUP[lookup_ptr]);

    lookup_entry.PTR      = em_b_hash_lookup_entry->PTR;
    lookup_entry.SELECT_4 = em_b_hash_lookup_entry->SELECT_4;
    lookup_entry.SELECT_3 = em_b_hash_lookup_entry->SELECT_3;
    lookup_entry.SELECT_2 = em_b_hash_lookup_entry->SELECT_2;
    lookup_entry.SELECT_1 = em_b_hash_lookup_entry->SELECT_1;
    lookup_entry.SELECT_0 = em_b_hash_lookup_entry->SELECT_0;
    lookup_entry.MASK     = em_b_hash_lookup_entry->MASK;
    return lookup_entry;
}

fm_uint64 mbyClsGetEmAHashCamEntry
(
    MBY_CGRP_A_IN_REGS,
    fm_uint32            const entry,
    fm_uint32            const word
)
{
    em_hash_cam_r const * const em_a_hash_cam = &(cgrp_a_map->EM.HASH_CAM[entry][word]);

    fm_uint64 data = em_a_hash_cam->DATA;
    return data;
}

fm_uint64 mbyClsGetEmBHashCamEntry
(
    MBY_CGRP_B_IN_REGS,
    fm_uint32            const entry,
    fm_uint32            const word
)
{
    em_hash_cam_r const * const em_b_hash_cam_entry = &(cgrp_b_map->EM.HASH_CAM[entry][word]);

    fm_uint64 data = em_b_hash_cam_entry->DATA;
    return data;
}

fm_uint64 mbyClsGetEmAHashCamMask
(
    MBY_CGRP_A_IN_REGS,
    fm_uint32            const row,
    fm_uint32            const rule
)
{
    em_hash_cam_en_r const * const em_a_hash_cam_en = &(cgrp_a_map->EM.HASH_CAM_EN[row][rule]);

    fm_uint64 mask = em_a_hash_cam_en->MASK;
    return mask;
}

fm_uint64 mbyClsGetEmBHashCamMask
(
    MBY_CGRP_B_IN_REGS,
    fm_uint32            const row,
    fm_uint32            const rule
)
{
    em_hash_cam_en_r const * const em_b_hash_cam_en = &(cgrp_b_map->EM.HASH_CAM_EN[row][rule]);

    fm_uint64 mask = em_b_hash_cam_en->MASK;
    return mask;
}

fm_uint64 mbyClsGetEmAShmEntry
(
    mby_shm_map * const shm_map,
    // fm_uint32     const hash_num, // How is this used in MBY? <-- REVISIT!!!
    fm_uint32     const entry_idx
)
{
    fm_uint64 data = 0;

    fwd_table0_r const * const fwd_table0 = &(shm_map->FWD_TABLE0[entry_idx][0]); // How to access correct piece of shared fwd. memory? <-- REVISIT!!!

    data = fwd_table0->DATA;

    return data;
}

fm_uint64 mbyClsGetEmBShmEntry
(
    mby_shm_map * const shm_map,
    // fm_uint32     const hash_num, // How is this used in MBY? <-- REVISIT!!!
    fm_uint32     const entry_idx
)
{
    fm_uint64 data = 0;

    fwd_table1_r const * const fwd_table1 = &(shm_map->FWD_TABLE1[entry_idx][0]); // How to access correct piece of shared fwd. memory? <-- REVISIT!!!

    data = fwd_table1->DATA;

    return data;
}

void mbyClsGetEmHashMissActions
(
    mby_ppe_cgrp_a_map      * const cgrp_a_map,
    mby_ppe_cgrp_b_map      * const cgrp_b_map,
    fm_byte                   const group,
    mbyClassifierHashCfg      const hash_cfg,
    fm_uint32                 const hash_num,
    fm_byte                   const scenario,
    fm_uint32                       hash_actions[MBY_FFU_MAX_HASH_ACTIONS]
)
{
    em_hash_miss_r const * const em_a_hash_miss   = &(cgrp_a_map->EM.HASH_MISS[hash_num][scenario]);
    em_hash_miss_r const * const em_a_hash_miss_1 = &(cgrp_a_map->EM.HASH_MISS[1][scenario]);
    em_hash_miss_r const * const em_b_hash_miss   = &(cgrp_b_map->EM.HASH_MISS[hash_num][scenario]);
    em_hash_miss_r const * const em_b_hash_miss_1 = &(cgrp_b_map->EM.HASH_MISS[1][scenario]);

    hash_actions[0] = (group == MBY_CLA_GROUP_A) ? em_a_hash_miss->ACTION0 : em_b_hash_miss->ACTION0;
    hash_actions[1] = (group == MBY_CLA_GROUP_A) ? em_a_hash_miss->ACTION1 : em_b_hash_miss->ACTION1;

    if (hash_cfg.mode == MBY_FFU_HASH_ENTRY_MODE_64B) {
        hash_actions[2] = (group == MBY_CLA_GROUP_A) ? em_a_hash_miss_1->ACTION0 : em_b_hash_miss_1->ACTION0;
        hash_actions[3] = (group == MBY_CLA_GROUP_A) ? em_a_hash_miss_1->ACTION1 : em_b_hash_miss_1->ACTION1;
    }
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

    wcm_tcam_cfg_r const * const wcm_tcam_cfg = &(cgrp_b_map->B.WCM_TCAM_CFG[slice][scenario]);

    tcam_cfg.CHUNK_MASK    = wcm_tcam_cfg->CHUNK_MASK;
    tcam_cfg.START_COMPARE = wcm_tcam_cfg->START_COMPARE;
    tcam_cfg.START_SET     = wcm_tcam_cfg->START_SET;
    tcam_cfg.SELECT_TOP    = wcm_tcam_cfg->SELECT_TOP;
    tcam_cfg.SELECT0       = wcm_tcam_cfg->SELECT0;
    tcam_cfg.SELECT1       = wcm_tcam_cfg->SELECT1;
    tcam_cfg.SELECT2       = wcm_tcam_cfg->SELECT2;
    tcam_cfg.SELECT3       = wcm_tcam_cfg->SELECT3;
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

    wcm_tcam_r const * const wcm_tcam_entry = &(cgrp_b_map->B.WCM_TCAM[slice][index]);

    fm_uint64 key_top     = ((fm_uint64) wcm_tcam_entry->KEY_TOP)        << 32;
    fm_uint64 key_top_inv = ((fm_uint64) wcm_tcam_entry->KEY_TOP_INVERT) << 32;

    tcam_entry.KEY        = wcm_tcam_entry->KEY        | key_top;
    tcam_entry.KEY_INVERT = wcm_tcam_entry->KEY_INVERT | key_top_inv;
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
    wcm_action_cfg_r const * const wcm_action_cfg = &(cgrp_b_map->B.WCM_ACTION_CFG[ram_num]);

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
    wcm_action_r const * const wcm_action = &(cgrp_b_map->B.WCM_ACTION[ram_num][hit_index]);
    fm_uint32 action_entry = (action == 0) ? wcm_action->ACTION0 : wcm_action->ACTION1;
    return action_entry;
}

mbyClassifierEntropyCfg mbyClsGetEntropyCfg
(
    MBY_ENTROPY_IN_REGS,
    fm_uint32             const hash_num,
    fm_byte               const hash_prof
)
{
    mbyClassifierEntropyCfg entropy_cfg = { 0 };

    entropy_hash_cfg1_r const * const entropy_hash_cfg1 = &(entropy_map->ENTROPY_HASH_CFG1[hash_num][hash_prof]);
    entropy_hash_cfg0_r const * const entropy_hash_cfg0 = &(entropy_map->ENTROPY_HASH_CFG0[hash_num][hash_prof]);

    entropy_cfg.SYMMETRIC        = entropy_hash_cfg1->SYMMETRIC;        // [54:54]
    entropy_cfg.SYM_PROFILE      = entropy_hash_cfg1->SYM_PROFILE;      // [53:52]
    entropy_cfg.KEY_MASK_PROFILE = entropy_hash_cfg1->KEY_MASK_PROFILE; // [51:48]
    entropy_cfg.KEY32_MASK       = entropy_hash_cfg1->KEY_MASK32;       // [47:32]
    entropy_cfg.KEY16_MASK       = entropy_hash_cfg1->KEY_MASK16;       // [31: 0]
    entropy_cfg.KEY8_MASK        = entropy_hash_cfg0->KEY_MASK8;        // [31: 0]
    return entropy_cfg;
}

mbyEntropyMetaCfg mbyClsGetEntropyMetaCfg
(
    MBY_ENTROPY_IN_REGS,
    fm_byte               const hash_prof
)
{
    mbyEntropyMetaCfg meta_cfg = { 0 };

    entropy_meta_cfg_r const * const entropy_meta_cfg = &(entropy_map->ENTROPY_META_CFG[hash_prof]);

    meta_cfg.BYTE_DEFAULTS = entropy_meta_cfg->BYTE_DEFAULTS; // [23:12]
    meta_cfg.HASH_START    = entropy_meta_cfg->HASH_START;    // [11: 6]
    meta_cfg.HASH_SIZE     = entropy_meta_cfg->HASH_SIZE;     // [ 5: 0]

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
