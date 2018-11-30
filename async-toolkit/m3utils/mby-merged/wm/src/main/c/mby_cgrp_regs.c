// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_cgrp_regs.h"

mbyClassifierHashLookup mbyClsGetEmHashLookupEntry
(
    em_hash_lookup_r * const em_hash_lookup_reg,
    fm_uint16          const lookup_ptr
)
{
    mbyClassifierHashLookup lookup_entry;

    em_hash_lookup_r const * const em_hash_lookup_entry = &(em_hash_lookup_reg[lookup_ptr]);

    lookup_entry.PTR      = em_hash_lookup_entry->PTR;
    lookup_entry.SELECT_4 = em_hash_lookup_entry->SELECT_4;
    lookup_entry.SELECT_3 = em_hash_lookup_entry->SELECT_3;
    lookup_entry.SELECT_2 = em_hash_lookup_entry->SELECT_2;
    lookup_entry.SELECT_1 = em_hash_lookup_entry->SELECT_1;
    lookup_entry.SELECT_0 = em_hash_lookup_entry->SELECT_0;
    lookup_entry.MASK     = em_hash_lookup_entry->MASK;

    return lookup_entry;
}

mbyClassifierKeyMaskCfg mbyClsGetEmKeyMaskCfg
(
    mby_ppe_cgrp_em_map * const cgrp_em_map,
    fm_byte               const hash_num,
    fm_byte               const profile
)
{
    mbyClassifierKeyMaskCfg key_mask_cfg;

    key_mask_cfg.KEY_MASK_SEL = cgrp_em_map->KEY_SEL1[hash_num][profile].KEY_MASK_SEL; // [51:48]
    key_mask_cfg.KEY32_MASK   = cgrp_em_map->KEY_SEL1[hash_num][profile].KEY32_MASK;   // [47:32]
    key_mask_cfg.KEY16_MASK   = cgrp_em_map->KEY_SEL1[hash_num][profile].KEY16_MASK;   // [31: 0]
    key_mask_cfg.KEY8_MASK    = cgrp_em_map->KEY_SEL0[hash_num][profile].KEY8_MASK;    // [31: 0]

    return key_mask_cfg;
}

mbyClassifierHashCfg mbyClsGetEmHashCfg
(
    mby_ppe_cgrp_em_map * const cgrp_em_map,
    fm_byte               const profile
)
{
    mbyClassifierHashCfg hash_cfg;

    em_hash_cfg_r const * const em_hash_cfg = &(cgrp_em_map->HASH_CFG[profile]);

    hash_cfg.mode          = em_hash_cfg->MODE;
    hash_cfg.base_ptr[0]   = em_hash_cfg->BASE_PTR_0;
    hash_cfg.base_ptr[1]   = em_hash_cfg->BASE_PTR_1;
    hash_cfg.hash_size[0]  = em_hash_cfg->HASH_SIZE_0;
    hash_cfg.hash_size[1]  = em_hash_cfg->HASH_SIZE_1;
    hash_cfg.entry_size[0] = em_hash_cfg->ENTRY_SIZE_0;
    hash_cfg.entry_size[1] = em_hash_cfg->ENTRY_SIZE_1;

    return hash_cfg;
}

fm_uint64 mbyClsGetEmHashCamEntry
(
    mby_ppe_cgrp_em_map * const cgrp_em_map,
    fm_uint32             const entry,
    fm_uint32             const word
)
{
    em_hash_cam_r const * const em_a_hash_cam = &(cgrp_em_map->HASH_CAM[entry][word]);

    fm_uint64 data = em_a_hash_cam->DATA;

    return data;
}

fm_uint64 mbyClsGetEmHashCamMask
(
    mby_ppe_cgrp_em_map * const cgrp_em_map,
    fm_uint32             const row,
    fm_uint32             const rule
)
{

    em_hash_cam_en_r const * const em_a_hash_cam_en = &(cgrp_em_map->HASH_CAM_EN[row][rule]);

    fm_uint64 mask = em_a_hash_cam_en->MASK;

    return mask;
}

fm_uint64 mbyClsGetEmAShmEntry
(
    mby_shm_map * const shm_map,
    fm_uint16     const block,
    fm_uint16     const cell
)
{
    fwd_table0_r const * const fwd_table0 = &(shm_map->FWD_TABLE0[block][cell]);

    fm_uint64 data = fwd_table0->DATA;

    return data;
}

fm_uint64 mbyClsGetEmBShmEntry
(
    mby_shm_map * const shm_map,
    fm_uint16     const block,
    fm_uint16     const cell
)
{
    fwd_table1_r const * const fwd_table1 = &(shm_map->FWD_TABLE1[block][cell]);

    fm_uint64 data = fwd_table1->DATA;

    return data;
}

void mbyClsGetEmHashMissActions
(
    mby_ppe_cgrp_em_map * const cgrp_em_map,
    mbyClassifierHashCfg  const hash_cfg,
    fm_uint32             const hash_num,
    fm_byte               const profile,
    fm_uint32                 * hash_actions
)
{
    em_hash_miss_r const * const em_hash_miss   = &(cgrp_em_map->HASH_MISS[hash_num][profile]);

    hash_actions[0] = em_hash_miss->ACTION0;
    hash_actions[1] = em_hash_miss->ACTION1;

    if (hash_cfg.mode == MBY_CGRP_HASH_ENTRY_MODE_64B) {
        em_hash_miss_r const * const em_hash_miss_1 = &(cgrp_em_map->HASH_MISS[1][profile]);

        hash_actions[2] = em_hash_miss_1->ACTION0;
        hash_actions[3] = em_hash_miss_1->ACTION1;
    }
}

mbyClassifierTcamCfg mbyClsGetWcmTcamCfg
(
    mby_ppe_cgrp_b_nested_map * const cgrp_b_map,
    fm_byte                     const slice,
    fm_byte                     const profile
)
{
    mbyClassifierTcamCfg tcam_cfg;

    wcm_tcam_cfg_r const * const wcm_tcam_cfg = &(cgrp_b_map->WCM_TCAM_CFG[slice][profile]);

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
    mby_ppe_cgrp_b_nested_map * const cgrp_b_map,
    fm_byte                     const slice,
    fm_uint16                   const index
)
{
    mbyClassifierTcamEntry tcam_entry;

    wcm_tcam_r const * const wcm_tcam_entry = &(cgrp_b_map->WCM_TCAM[slice][index]);

    fm_uint64 key_top     = ((fm_uint64) wcm_tcam_entry->KEY_TOP)        << 32;
    fm_uint64 key_top_inv = ((fm_uint64) wcm_tcam_entry->KEY_TOP_INVERT) << 32;

    tcam_entry.KEY        = wcm_tcam_entry->KEY        | key_top;
    tcam_entry.KEY_INVERT = wcm_tcam_entry->KEY_INVERT | key_top_inv;

    return tcam_entry;
}

mbyClassifierActionCfg mbyClsGetWcmActionCfg
(
    mby_ppe_cgrp_b_nested_map * const cgrp_b_map,
    fm_byte                     const profile,
    fm_byte                     const ram_num
)
{
    mbyClassifierActionCfg action_cfg;
    wcm_action_cfg_en_r const * const wcm_action_cfg_en = &(cgrp_b_map->WCM_ACTION_CFG_EN[profile]);
    wcm_action_cfg_r const * const wcm_action_cfg_idx   = &(cgrp_b_map->WCM_ACTION_CFG[profile][ram_num/
                                                            (wcm_action_cfg_r_INDEX__n/MBY_WCM_ACTION_CFG_INDEX_WIDTH)]);

    action_cfg.enable   = (wcm_action_cfg_en->ENABLE >> ram_num) & 0x1;
    fm_byte slice_shift = ((ram_num % (wcm_action_cfg_r_INDEX__n/MBY_WCM_ACTION_CFG_INDEX_WIDTH))
                            * MBY_WCM_ACTION_CFG_INDEX_WIDTH);
    action_cfg.slice    = (wcm_action_cfg_idx->INDEX >> slice_shift) & 0x1f;

    return action_cfg;
}

fm_uint32 mbyClsGetWcmActionEntry
(
    mby_ppe_cgrp_b_nested_map * const cgrp_b_map,
    fm_byte                     const ram_num,
    fm_uint32                   const hit_index,
    fm_uint32                   const action
)
{
    wcm_action_r const * const wcm_action = &(cgrp_b_map->WCM_ACTION[ram_num][hit_index]);

    fm_uint32 action_entry = (action == 0) ? wcm_action->ACTION0 : wcm_action->ACTION1;

    return action_entry;
}

// TODO this function is not strictly used to access the registers but it
// is used by various blocks of the classifiers
void mbyClsConvertKeysToBytes
(
    mbyClassifierKeys const keys,
    fm_byte                 bytes[MBY_CGRP_HASH_KEYS]
)
{
    for (fm_uint i = 0; i < MBY_CGRP_KEY16; i++) {
        bytes[2*i + 1] =  keys.key16[i]       & 0xFF;
        bytes[2*i    ] = (keys.key16[i] >> 8) & 0xFF;
    }

    for (fm_uint i = 0; i < MBY_CGRP_KEY8; i++)
        bytes[MBY_CGRP_KEY16*2 + i] = keys.key8[i];

    for (fm_uint i = 0; i < MBY_CGRP_KEY32; i++)
        for (fm_uint j = 0; j < 4; j++)
            bytes[MBY_CGRP_KEY16*2 + MBY_CGRP_KEY8 + i*4 + (3-j)] = (keys.key32[i] >> (j * 8 )) & 0xFF;
}
