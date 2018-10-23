// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation


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

static void doKeyCompaction
(
    mbyClassifierKeyMaskCfg         const key_mask_cfg,
    mbyClassifierKeys               const hash_keys,
    fm_byte                               packed_keys[MBY_FFU_HASH_KEYS],
    fm_byte                       * const key_size
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

    *key_size = key_idx;
}

static void getEmHashHitActions
(
    fm_byte         entry[MBY_FFU_MAX_HASH_ENTRY_SIZE], // aka cam_key[]
    fm_byte   const entry_mode,
    fm_byte   const key_size,
    fm_uint16 const entry_size,
    fm_bool   const is_cam,
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

static void getEmHashRamData
(

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
        ram_alloc[i] = mbyClsGetEmHashRamAlloc(      i);
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

            hash_entry = mbyClsGetEmHashEntryRam(      hash_num, entry_idx); // REVISIT!!!
            entry_idx++;
        }
        else if (rd_ram_num == 0)
        {
            fm_byte alloc = FM_GET_UNNAMED_FIELD(ram_alloc[rd_ram_num], col, 1);
            if (alloc != group) {
                rd_ram_ok = FALSE;
                break;
            }

            hash_entry = mbyClsGetEmHashEntryRam(      rd_ram_num, entry_idx); // REVISIT!!!
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

            hash_entry = mbyClsGetEmHashEntryRam(      rd_ram_num, entry_idx); // REVISIT!!!
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

void mbyMatchExact // i.e. look up EM hash
(
    mby_ppe_cgrp_a_map      * const cgrp_a_map,
    mby_ppe_cgrp_b_map      * const cgrp_b_map,
    mbyClassifierKeys const * const keys,
    fm_byte                   const scenario,
    fm_byte                   const group,
    mbyClassifierActions    * const actions // = output actions
)
{
    for (fm_uint hash_num = 0; hash_num < MBY_FFU_KEY_MASK0_ENTRIES_1; hash_num++)
    {
        // Initialize actions
        fm_uint32 hash_actions[MBY_FFU_MAX_HASH_ACTIONS] = { 0 };

        // Get FFU_KEY_MASK register fields:
        mbyClassifierKeyMaskCfg key_mask_cfg = (group == 0)
            ? mbyClsGetEmAKeyMaskCfg(cgrp_a_map, hash_num, scenario)
            : mbyClsGetEmBKeyMaskCfg(cgrp_b_map, hash_num, scenario);
        // Apply key mask on FFU keys:
        mbyClassifierKeys hash_keys;
        applyEmKeyMask(key_mask_cfg, *keys, &hash_keys);

        // Convert Keys into array of bytes:
        fm_byte hash_bytes[MBY_FFU_HASH_KEYS] = { 0 };
        mbyClsConvertKeysToBytes(hash_keys, hash_bytes);

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

        mbyClassifierHashCfg hash_cfg = (group == 0)
            ? mbyClsGetEmAHashCfg(cgrp_a_map, scenario)
            : mbyClsGetEmBHashCfg(cgrp_b_map, scenario);
        // Don't perform lookups if hash_num is 1 in non-split mode:
        if ((hash_cfg.mode == MBY_FFU_HASH_ENTRY_MODE_64B) && (hash_num == MBY_FFU_KEY_MASK0_ENTRIES_1 - 1))
           break;

        // Don't perform lookups if hash entry size is 0:
        if (hash_cfg.entry_size[hash_num] == 0)
           continue;

        fm_uint16 lookup_ptr = hash_cfg.base_ptr[hash_num]
            + (hash_index % (1uL << hash_cfg.hash_size[hash_num]))
            + ((hash_cfg.mode == MBY_FFU_HASH_ENTRY_MODE_32B && hash_num == 1) ? 4096 : 0);

        mbyClassifierHashLookup bucket = (group == 0)
            ? mbyClsGetEmAHashLookupEntry(cgrp_a_map, lookup_ptr)
            : mbyClsGetEmBHashLookupEntry(cgrp_b_map, lookup_ptr);
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
                    fm_uint64 cam_entry = (group == 0)
                        ? mbyClsGetEmAHashCamEntry(cgrp_a_map, i, j)
                        : mbyClsGetEmBHashCamEntry(cgrp_b_map, i, j);
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
                fm_uint64 scenario_mask = (group == 0)
                    ? mbyClsGetEmAHashCamMask(cgrp_a_map, hash_num, i)
                    : mbyClsGetEmBHashCamMask(cgrp_b_map, hash_num, i);
                hash_cam_en = (scenario_mask >> scenario) & 1;

                // 64B entry mode needs to consider CAM_EN[0/1]
                if (hash_cfg.mode == MBY_FFU_HASH_ENTRY_MODE_64B) {
                fm_uint64 scenario_mask = (group == 0)
                    ? mbyClsGetEmAHashCamMask(cgrp_a_map, hash_num + 1, i)
                    : mbyClsGetEmBHashCamMask(cgrp_b_map, hash_num + 1, i);
                    hash_cam_en &= (scenario_mask >> scenario) & 1;
                }

                // Get CAM Actions:
                if (hash_cam_en)
                {
                    fm_int cam_entry_size = (hash_cfg.mode == MBY_FFU_HASH_ENTRY_MODE_64B)
                        ?  MBY_FFU_MAX_HASH_ENTRY_SIZE
                        : (MBY_FFU_MAX_HASH_ENTRY_SIZE / 2);

                    fm_bool is_cam = TRUE;
                    getEmHashHitActions(cam_key, hash_cfg.mode, key_size, cam_entry_size, is_cam, hash_actions);
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

            getEmHashRamData(      group, hash_num, bucket, hash_cfg, hash_more, hash_ram_key, &ram_data_ok); // <-- REVISIT!!!
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
                getEmHashHitActions(hash_ram_key, hash_cfg.mode, key_size, cam_entry_size, is_cam, hash_actions);
            } else {
                // Get actions from FFU_HASH_MISS register since both cam and ram missed
                mbyClsGetEmHashMissActions(cgrp_a_map, cgrp_b_map, group, hash_cfg, hash_num, scenario, hash_actions);
            }

        } // if !hash_cam_en

        // FIXME doAction should be called from mby_classifier
        // for (fm_uint i = 0; i < MBY_FFU_MAX_HASH_ACTIONS ; i++)
        //     doAction(hash_actions[i], actions);

    } // for hash_num
}
