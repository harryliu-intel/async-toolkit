// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include <stdio.h>
#include <string.h>

#include "mby_common.h"
#include "mby_exactmatch.h"
#include "mby_crc32.h"

static void applyEmKeyMask
(
    mbyClassifierKeyMaskCfg         const key_mask_cfg,
    mbyClassifierKeys               const keys,
    mbyClassifierKeys             * const hash_keys
)
{
    for (fm_uint i = 0; i < MBY_FFU_KEY16; i++)
        hash_keys->key16[i] = (FM_GET_UNNAMED_FIELD  (key_mask_cfg.KEY16_MASK, i, 1)) ? keys.key16[i] : 0;

    for (fm_uint i = 0; i < MBY_FFU_KEY8; i++)
        hash_keys->key8[i]  = (FM_GET_UNNAMED_FIELD64(key_mask_cfg.KEY8_MASK,  i, 1)) ? keys.key8 [i] : 0;

    for (fm_uint i = 0; i < MBY_FFU_KEY32; i++)
        hash_keys->key32[i] = (FM_GET_UNNAMED_FIELD  (key_mask_cfg.KEY32_MASK, i, 1)) ? keys.key32[i] : 0;
}

static fm_byte doKeyCompaction
(
    mbyClassifierKeyMaskCfg         const key_mask_cfg,
    mbyClassifierKeys               const hash_keys,
    fm_byte                               packed_keys[MBY_FFU_HASH_KEYS]
)
{
    fm_byte key_idx = 0;

    // KEY32:
    for (fm_uint i = 0; i < MBY_FFU_KEY32; i++) {
        if (FM_GET_UNNAMED_FIELD(key_mask_cfg.KEY32_MASK, i, 1)) {
            for (fm_uint j = 0; j < 4; j++)
                packed_keys[key_idx + j] = (hash_keys.key32[i] >> (8 * (3-j))) & 0xFF;
            key_idx += 4;
        }
    }

    // KEY16:
    for (fm_uint i = 0; i < MBY_FFU_KEY16; i++) {
        if (FM_GET_UNNAMED_FIELD(key_mask_cfg.KEY16_MASK, i, 1)) {
            packed_keys[key_idx    ] = (hash_keys.key16[i] >> 8) & 0xFF;
            packed_keys[key_idx + 1] =  hash_keys.key16[i]       & 0xFF;
            key_idx += 2;
        }

    }

    // KEY8:
    for (fm_uint i = 0; i < MBY_FFU_KEY8; i++) {
        if (FM_GET_UNNAMED_FIELD(key_mask_cfg.KEY8_MASK, i, 1)) {
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

    fm_byte key_size = key_idx;

    return key_size;
}

static void calculateHash(
    fm_byte           const group,
    fm_uint           const hash_num,
    mbyClassifierKeys const hash_keys,
    fm_uint16       * const hash_index,
    fm_uint16       * const hash_more
)
{
    // Convert Keys into array of bytes:
    fm_byte hash_bytes[MBY_FFU_HASH_KEYS] = { 0 };
    mbyClsConvertKeysToBytes(hash_keys, hash_bytes);

    // Get hash value from CRC:
    fm_uint32 hash = (hash_num == 0) ?
        mbyCrc32ByteSwap (hash_bytes, MBY_FFU_HASH_KEYS) : // HASH0: CRC-32 (Ethernet)
        mbyCrc32CByteSwap(hash_bytes, MBY_FFU_HASH_KEYS) ; // HASH1: CRC-32C (iSCSI)

    // for EM_A lookup size is 32768, for E_B lookup size is 8192
    fm_uint16 hash_mask  = (group == MBY_CLA_GROUP_A) ? 0x7fff : 0x1fff;

    *hash_index = hash & hash_mask;
    *hash_more  = (hash >> 16) & 0xffff;
}

static fm_uint16 calculateLookupPtr(
    fm_byte              const group,
    fm_uint              const hash_num,
    fm_uint16            const hash_index,
    mbyClassifierHashCfg const hash_cfg
)
{
    fm_uint16 lookup_start_index = 0;

    // Start from the 2nd half of the bucket table if hash_num is 1 in split mode
    if (hash_num == 1)
        lookup_start_index = (group == MBY_CLA_GROUP_A)
            ? MBY_EM_A_HASH_MODE_32B_LOOKUP_ENTRIES
            : MBY_EM_B_HASH_MODE_32B_LOOKUP_ENTRIES;

    fm_uint16 lookup_base_ptr = hash_cfg.base_ptr[hash_num];
    fm_uint16 bucket_table_index = hash_index % (1uL << hash_cfg.hash_size[hash_num]);

    fm_uint16 lookup_ptr = lookup_start_index + lookup_base_ptr + bucket_table_index;

    return lookup_ptr;
}

static void getEmHashHitActions
(
    fm_byte         entry[MBY_FFU_MAX_HASH_ENTRY_SIZE], // aka cam_keys[] or hash_lookup_keys[]
    fm_bool   const split_mode,
    fm_byte   const key_size,
    fm_uint16 const entry_size,
    fm_bool   const is_cam,
    fm_uint32       hash_actions[MBY_FFU_MAX_HASH_ACTIONS]
)
{
    fm_uint max_bytes_num  = split_mode ? 8 : 16;
    fm_uint bytes_num      = (entry_size > key_size) ? entry_size - key_size : 0;
    fm_uint read_bytes_num = (bytes_num > max_bytes_num) ? max_bytes_num : bytes_num;
    fm_uint act_idx        = (is_cam) ? (split_mode ? 0 : 2) : (read_bytes_num/4 - 1);

    for (fm_uint i = 0; i < read_bytes_num/4; i++)
    {
        for (fm_uint j = 0; j < 4; j++) {
            FM_SET_UNNAMED_FIELD(hash_actions[act_idx], 8*j, 8, entry[entry_size - (i*4) - j - 1]); // It seems that this changes endianess, but it's not correct according to the test... <-- REVISIT!!!
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

static fm_bool checkCamHits
(
    mby_ppe_cgrp_em_map * const cgrp_em_map,
    fm_bool               const split_mode,
    fm_byte               const group,
    fm_uint               const hash_num,
    fm_byte               const profile,
    fm_byte               const packed_keys[MBY_FFU_HASH_KEYS],
    fm_byte               const key_size,
    fm_byte               const max_hash_actions_num,
    fm_uint32                   hash_actions[MBY_FFU_MAX_HASH_ACTIONS]
)
{
    fm_byte max_hash_entry_size = split_mode
        ? MBY_FFU_MAX_HASH_ENTRY_SIZE / 2
        : MBY_FFU_MAX_HASH_ENTRY_SIZE;

    fm_byte min_cam_key_size = max_hash_entry_size - (max_hash_actions_num * 4);

    fm_bool hash_cam_en = FALSE;
    for (fm_int hash_entry_idx = MBY_FFU_HASH_CAM_ENTRIES_1 - 1 ; hash_entry_idx >= 0 ; hash_entry_idx--)
    {
        // Initialize cam keys to 0
        fm_byte cam_keys[MBY_FFU_MAX_HASH_ENTRY_SIZE] = { 0 };

        // Build Cam Key
        fm_uint key_idx = 0;
        for (fm_uint j = 0; j < MBY_FFU_HASH_CAM_ENTRIES_0; j++)
        {
            fm_bool hashn_32b = ( ((hash_num == 0) && (j < 4)) || ((hash_num == 1) && (j >= 4)) );

            if (!split_mode || (split_mode && hashn_32b)) {
                fm_uint64 cam_entry = mbyClsGetEmHashCamEntry(cgrp_em_map, hash_entry_idx, j);

                for (fm_uint k = 0; k < 8; k++) {
                    cam_keys[key_idx] = (cam_entry >> (8 * (7-k))) & 0xFF;
                    key_idx++;
                }
            }
        }

        // Compare cam_keys with packed_keys.
        fm_bool cam_hit = TRUE;
        if (key_size < MBY_FFU_MAX_HASH_ENTRY_SIZE)
        {
            for (fm_uint j = 0; j < key_size; j++) {
                // Should a 128b KEY_MASK from EM_X_KEY_MASK be applied here? <--REVISIT!!!
                if (packed_keys[j] != cam_keys[j]) {
                    cam_hit = FALSE;
                    break;
                }
            }
            // If key_size < min_cam_key_size, packed keys need to be padded with 0's to minCamKeySize.
            // packedKeys[keySize...minCamKeySize-1] are 0 in this case, so we can get cam_hit
            // by checking if cam_keys[key_size...min_cam_key_size-1] are 0.
            for (fm_uint j = key_size; j < min_cam_key_size; j++) {
                if (cam_keys[j] != 0) {
                    cam_hit = FALSE;
                    break;
                }
            }
        }

        if (cam_hit)
        {
            fm_uint64 profile_mask = mbyClsGetEmHashCamMask(cgrp_em_map, hash_num, hash_entry_idx);

            hash_cam_en = (profile_mask >> profile) & 1;

            // 64B entry mode needs to consider CAM_EN[0/1]
            if (!split_mode) {
                fm_uint64 profile_mask = mbyClsGetEmHashCamMask(cgrp_em_map, hash_num + 1, hash_entry_idx);

                hash_cam_en &= (profile_mask >> profile) & 1;
            }

            // Get CAM Actions:
            if (hash_cam_en)
            {
                fm_bool is_cam = TRUE;
                getEmHashHitActions(cam_keys, split_mode, key_size, max_hash_entry_size, is_cam, hash_actions);
                break;
            }

        } // if (cam_hit)

    } // for (hash_entry_idx)

    return hash_cam_en;
}

static void getEmHashShmData // How to fetch correct DATA from SHM in MBY?
(
    mby_shm_map             * const shm_map, // shared memory (forwarding tables)
    fm_byte                   const group,
    fm_uint32                 const hash_num,
    mbyClassifierHashLookup   const bucket,
    mbyClassifierHashCfg      const hash_cfg,
    fm_uint16                 const hash_more,
    fm_byte                         hash_lookup_key[MBY_FFU_MAX_HASH_ENTRY_SIZE],
    fm_bool                 * const lookup_data_ok
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

    fm_uint32 hash_lookup_addr = (bucket.PTR + offset * hash_cfg.entry_size[hash_num]) * 4;

    fm_bool   group_A    = group == MBY_CLA_GROUP_A;
    fm_bool   mode_32b   = (hash_cfg.mode == MBY_FFU_HASH_ENTRY_MODE_32B); // split_mode
    fm_byte   start_bit  = ((mode_32b) ? 5 : 6);
    fm_uint16 line       = FM_GET_UNNAMED_FIELD(hash_lookup_addr, start_bit, 14);
    fm_byte   start_byte = FM_GET_UNNAMED_FIELD(hash_lookup_addr, 0, 5);
    fm_uint32 entry_idx0 = (line*4 + start_byte/8);
    fm_uint32 entry_idx1 = ((hash_lookup_addr/8) & 0xFFFF);
    fm_uint32 entry_idx  = (mode_32b) ? entry_idx1 : entry_idx0;
    fm_uint   max_bytes  = (mode_32b) ? (MBY_FFU_MAX_HASH_ENTRY_SIZE / 2) : MBY_FFU_MAX_HASH_ENTRY_SIZE;
    // What is rd_lookup_num? Replaced by group in calls to mbyClsGetEmAShmEntry / mbyClsGetEmBShmEntry. <-- REVISIT!!!
    fm_bool   rd_lookup_num = (mode_32b) ? 0 : FM_GET_UNNAMED_FIELD(hash_lookup_addr, 5, 1);

    if (!mode_32b) // reassign start_byte to 6b to indicate its position in 64B
        start_byte = FM_GET_UNNAMED_FIELD(hash_lookup_addr, 0, 6);

    fm_int rem_bytes = hash_cfg.entry_size[hash_num] * 4 - max_bytes + start_byte;

    fm_uint num_entries = ((max_bytes - start_byte) / 8) + (((max_bytes - start_byte) % 8 == 0) ? 0 : 1)
        + (rem_bytes / 8) + ((rem_bytes % 8 == 0) ? 0 : 1);

    fm_uint key_idx = 0;
    fm_bool rd_lookup_ok = TRUE;

    for (fm_uint i = 0; i < num_entries; i++)
    {
        fm_uint64 hash_entry = 0;

        if (mode_32b)
        {
            hash_entry = group_A ? mbyClsGetEmAShmEntry(shm_map, entry_idx) : mbyClsGetEmBShmEntry(shm_map, entry_idx);

            entry_idx++;
        }
        else if (rd_lookup_num == 0) // && mode_64b
        {
            hash_entry = group_A ? mbyClsGetEmAShmEntry(shm_map, entry_idx) : mbyClsGetEmBShmEntry(shm_map, entry_idx);

            if ((entry_idx % 4) == 3) {
                rd_lookup_num = 1;
                entry_idx = (entry_idx / 4) * 4;
            }
            else
                entry_idx++;
        }
        else // rd_lookup_num == 1 && mode_64b
        {
            hash_entry = group_A ? mbyClsGetEmAShmEntry(shm_map, entry_idx) : mbyClsGetEmBShmEntry(shm_map, entry_idx);

            if ((entry_idx % 4) == 3)
                rd_lookup_num = 0;

            entry_idx++;
        }

        for (fm_uint k = ((i == 0) ? start_byte % 8 : 0); k < 8; k++) {
            if ((i > 0) && (key_idx >= hash_cfg.entry_size[hash_num] * 4))
                break;
            fm_byte key = (hash_entry >> (8 * (7-k))) & 0xFF;
            hash_lookup_key[key_idx] = key;
            key_idx++;
        }
    }

    if (FM_GET_UNNAMED_FIELD(bucket.MASK, bucket_hash, 1) == 0)
        rd_lookup_ok = FALSE;

    *lookup_data_ok = rd_lookup_ok; // output read status
}

static void checkLookupHits
(
    mby_ppe_cgrp_em_map   * const cgrp_em_map,
    mby_shm_map           * const shm_map,
    fm_byte                 const group,
    fm_uint                 const hash_num,
    fm_byte                 const profile,
    fm_byte                 const packed_keys[MBY_FFU_HASH_KEYS],
    fm_byte                 const key_size,
    mbyClassifierHashLookup const bucket,
    mbyClassifierHashCfg    const hash_cfg,
    fm_uint16               const hash_more,
    fm_uint32                     hash_actions[MBY_FFU_MAX_HASH_ACTIONS]

)
{
    fm_bool hash_lookup_hit = TRUE;
    fm_byte hash_lookup_key[MBY_FFU_MAX_HASH_ENTRY_SIZE] = { 0 };

    fm_bool lookup_data_ok = FALSE;
    getEmHashShmData(shm_map, group, hash_num, bucket, hash_cfg, hash_more, hash_lookup_key, &lookup_data_ok); // <-- REVISIT!!!

    // Compare hash lookup keys with packed keys:
    if (key_size < MBY_FFU_MAX_HASH_ENTRY_SIZE) {
        for (fm_uint j = 0; j < key_size; j++) {
            // Should a 128b KEY_MASK from EM_X_KEY_MASK be applied here? <--REVISIT!!!
            if (packed_keys[j] != hash_lookup_key[j]) {
                hash_lookup_hit = FALSE;
                break;
            }
        }
    }

    // bucket is invalid when MASK == 0 or lookup_data_ok == 0
    if ((bucket.MASK == 0) || (lookup_data_ok == FALSE))
        hash_lookup_hit = FALSE;

    if (hash_lookup_hit)
    {
        fm_int  lookup_entry_size = hash_cfg.entry_size[hash_num] * 4;
        fm_bool is_cam = FALSE;
        // Get actions from FWD_TABLE0/1 register since lookup hit
        getEmHashHitActions(hash_lookup_key, hash_cfg.mode, key_size, lookup_entry_size, is_cam, hash_actions);
    }
    else // Get actions from FFU_HASH_MISS register since both CAM and lookup missed
    {
        mbyClsGetEmHashMissActions(cgrp_em_map, hash_cfg, hash_num, profile, hash_actions);
    }
}

void mbyMatchExact // i.e. look up EM hash
(
    em_hash_lookup_r        * const em_hash_lookup_reg,
    mby_ppe_cgrp_em_map     * const cgrp_em_map,
    mby_shm_map             * const shm_map,
    mbyClassifierKeys const * const keys,
    fm_byte                   const profile,
    fm_byte                   const group,
    fm_uint32               * const actions // = the list of action entries to action resolution
)
{
    // In split mode are 2 lookups: hash_num = 0 and hash_num = 1
    // In non-split mode is only 1 lookup and this loop for hash_num = 1 is ommited
    for (fm_uint hash_num = 0; hash_num < MBY_FFU_KEY_MASK0_ENTRIES_1; hash_num++)
    {
        // Initialize actions
        fm_uint32 hash_actions[MBY_FFU_MAX_HASH_ACTIONS] = { 0 };
        fm_byte packed_keys[MBY_FFU_HASH_KEYS] = { 0 };

        // Get EM_X_HASH_CFG register fields:
        mbyClassifierHashCfg hash_cfg = mbyClsGetEmHashCfg(cgrp_em_map, profile);

        // Get split mode, for split mode is true, for non-split mode is false:
        fm_bool split_mode = hash_cfg.mode == MBY_FFU_HASH_ENTRY_MODE_32B;

        // Don't perform lookups if non-split mode and hash_num is 1:
        if (!split_mode && (hash_num == MBY_FFU_KEY_MASK0_ENTRIES_1 - 1))
           break;

        // Don't perform lookups if hash entry size is 0:
        if (hash_cfg.entry_size[hash_num] == 0)
           continue;

        // Get EM_X_KEY_MASK register fields:
        mbyClassifierKeyMaskCfg key_mask_cfg = mbyClsGetEmKeyMaskCfg(cgrp_em_map, hash_num, profile);

        // Apply key mask on EM keys:
        mbyClassifierKeys hash_keys;
        applyEmKeyMask(key_mask_cfg, *keys, &hash_keys);

        // Get packed_keys and key_size:
        fm_byte key_size = doKeyCompaction(key_mask_cfg, hash_keys, packed_keys);

        // Get hash_index and hash_more:
        fm_uint16 hash_index;
        fm_uint16 hash_more;
        calculateHash(group, hash_num, hash_keys, &hash_index, &hash_more);

        // Calculate the lookup pointer to the bucket table
        fm_uint16 lookup_ptr = calculateLookupPtr(group, hash_num, hash_index, hash_cfg);

        // Get bucket from EM_X_HASH_LOOKUP:
        mbyClassifierHashLookup bucket = mbyClsGetEmHashLookupEntry(em_hash_lookup_reg, lookup_ptr);

        fm_byte max_hash_actions_num = split_mode
            ? MBY_EM_MAX_ACTIONS_NUM / 2
            : MBY_EM_MAX_ACTIONS_NUM;

        // Check CAM hits
        fm_bool hash_cam_en = checkCamHits(cgrp_em_map, split_mode, group, hash_num, profile, packed_keys, key_size, max_hash_actions_num, hash_actions);

        if (!hash_cam_en)
        {
            // Check if hit Hash lookup, then get actions from SHM
            checkLookupHits(cgrp_em_map, shm_map, group, hash_num, profile, packed_keys, key_size, bucket, hash_cfg, hash_more, hash_actions);
        }

        // Copy actions to output
        fm_uint split_offset = (hash_num == 0) ? 0 : max_hash_actions_num;

        for (fm_uint i = 0 ; i < max_hash_actions_num ; i++)
        {
            actions[split_offset + i] = hash_actions[i];
        }

    } // for (hash_num)

}
