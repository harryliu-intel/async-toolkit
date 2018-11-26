// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_classifier.h"
#include "mby_hash.h"
#include "mby_crc32.h"

static fm_uint16 lookUpPearsonHash(const fm_uint16 key)
{
    if (key > 0xfff)
    {
        /* "Invalid Pearson hash table key" */
        // What should happen here? REVISIT!!!
    }

    fm_byte   value  = ptable[key] & 0xF;
    fm_uint16 result = (key & 0xFF0) | value;

    return result;
}

static mbyClassifierEntropyCfg mbyClsGetEntropyCfg
(
    mby_ppe_entropy_map const * const entropy_map,
    fm_byte                     const hash_num,
    fm_byte                     const hash_profile
)
{
    mbyClassifierEntropyCfg entropy_cfg = { 0 };

    entropy_hash_cfg1_r const * const entropy_hash_cfg1 = &(entropy_map->ENTROPY_HASH_CFG1[hash_num][hash_profile]);
    entropy_hash_cfg0_r const * const entropy_hash_cfg0 = &(entropy_map->ENTROPY_HASH_CFG0[hash_num][hash_profile]);

    entropy_cfg.SYMMETRIC        = entropy_hash_cfg1->SYMMETRIC;        // [54:54]
    entropy_cfg.SYM_PROFILE      = entropy_hash_cfg1->SYM_PROFILE;      // [53:52]
    entropy_cfg.KEY_MASK_PROFILE = entropy_hash_cfg1->KEY_MASK_PROFILE; // [51:48]
    entropy_cfg.KEY32_MASK       = entropy_hash_cfg1->KEY_MASK32;       // [47:32]
    entropy_cfg.KEY16_MASK       = entropy_hash_cfg1->KEY_MASK16;       // [31: 0]
    entropy_cfg.KEY8_MASK        = entropy_hash_cfg0->KEY_MASK8;        // [31: 0]

    return entropy_cfg;
}

static mbyEntropyMetaCfg mbyClsGetEntropyMetaCfg
(
    mby_ppe_entropy_map const * const entropy_map,
    fm_byte                     const hash_profile
)
{
    mbyEntropyMetaCfg meta_cfg = { 0 };

    entropy_meta_cfg_r const * const entropy_meta_cfg = &(entropy_map->ENTROPY_META_CFG[hash_profile]);

    fm_uint16 byte_defaults = entropy_meta_cfg->BYTE_DEFAULTS; // [23:12]
    meta_cfg.HASH_START     = entropy_meta_cfg->HASH_START;    // [11: 6]
    meta_cfg.HASH_SIZE      = entropy_meta_cfg->HASH_SIZE;     // [ 5: 0]

    for (fm_byte i = 0 ; i < MBY_MOD_META_BYTES ; i++)
    {
        meta_cfg.BYTE_DEFAULTS[i] = ( byte_defaults << 2 * i ) & 0x3;
    }

    return meta_cfg;
}

static void mbyClsGetEntropyHashSym
(
    mby_ppe_entropy_map const * const entropy_map,
    fm_byte                     const hash_num,
    fm_byte                     const sym_profile,
    mbyEntropyHashSym         * const hash_sym
)
{
    entropy_hash_sym8_r  const * const entropy_hash_sym8  = &(entropy_map->ENTROPY_HASH_SYM8 [hash_num][sym_profile]);
    entropy_hash_sym16_r const * const entropy_hash_sym16 = &(entropy_map->ENTROPY_HASH_SYM16[hash_num][sym_profile]);
    entropy_hash_sym32_r const * const entropy_hash_sym32 = &(entropy_map->ENTROPY_HASH_SYM32[hash_num][sym_profile]);

    hash_sym->KEY8[0][0] = entropy_hash_sym8->PAIR0_KEY0;
    hash_sym->KEY8[0][1] = entropy_hash_sym8->PAIR0_KEY1;
    hash_sym->KEY8[1][0] = entropy_hash_sym8->PAIR1_KEY0;
    hash_sym->KEY8[1][1] = entropy_hash_sym8->PAIR1_KEY1;
    hash_sym->KEY8[2][0] = entropy_hash_sym8->PAIR2_KEY0;
    hash_sym->KEY8[2][1] = entropy_hash_sym8->PAIR2_KEY1;
    hash_sym->KEY8[3][0] = entropy_hash_sym8->PAIR3_KEY0;
    hash_sym->KEY8[3][1] = entropy_hash_sym8->PAIR3_KEY1;
    hash_sym->KEY8[4][0] = entropy_hash_sym8->PAIR4_KEY0;
    hash_sym->KEY8[4][1] = entropy_hash_sym8->PAIR4_KEY1;
    hash_sym->KEY8[5][0] = entropy_hash_sym8->PAIR5_KEY0;
    hash_sym->KEY8[5][1] = entropy_hash_sym8->PAIR5_KEY1;

    hash_sym->KEY16[0][0] = entropy_hash_sym16->PAIR0_KEY0;
    hash_sym->KEY16[0][1] = entropy_hash_sym16->PAIR0_KEY1;
    hash_sym->KEY16[1][0] = entropy_hash_sym16->PAIR1_KEY0;
    hash_sym->KEY16[1][1] = entropy_hash_sym16->PAIR1_KEY1;
    hash_sym->KEY16[2][0] = entropy_hash_sym16->PAIR2_KEY0;
    hash_sym->KEY16[2][1] = entropy_hash_sym16->PAIR2_KEY1;
    hash_sym->KEY16[3][0] = entropy_hash_sym16->PAIR3_KEY0;
    hash_sym->KEY16[3][1] = entropy_hash_sym16->PAIR3_KEY1;
    hash_sym->KEY16[4][0] = entropy_hash_sym16->PAIR4_KEY0;
    hash_sym->KEY16[4][1] = entropy_hash_sym16->PAIR4_KEY1;
    hash_sym->KEY16[5][0] = entropy_hash_sym16->PAIR5_KEY0;
    hash_sym->KEY16[5][1] = entropy_hash_sym16->PAIR5_KEY1;

    hash_sym->KEY32[0][0] = entropy_hash_sym32->PAIR0_KEY0;
    hash_sym->KEY32[0][1] = entropy_hash_sym32->PAIR0_KEY1;
    hash_sym->KEY32[1][0] = entropy_hash_sym32->PAIR1_KEY0;
    hash_sym->KEY32[1][1] = entropy_hash_sym32->PAIR1_KEY1;
    hash_sym->KEY32[2][0] = entropy_hash_sym32->PAIR2_KEY0;
    hash_sym->KEY32[2][1] = entropy_hash_sym32->PAIR2_KEY1;
    hash_sym->KEY32[3][0] = entropy_hash_sym32->PAIR3_KEY0;
    hash_sym->KEY32[3][1] = entropy_hash_sym32->PAIR3_KEY1;
    hash_sym->KEY32[4][0] = entropy_hash_sym32->PAIR4_KEY0;
    hash_sym->KEY32[4][1] = entropy_hash_sym32->PAIR4_KEY1;
    hash_sym->KEY32[5][0] = entropy_hash_sym32->PAIR5_KEY0;
    hash_sym->KEY32[5][1] = entropy_hash_sym32->PAIR5_KEY1;
    hash_sym->KEY32[6][0] = entropy_hash_sym32->PAIR6_KEY0;
    hash_sym->KEY32[6][1] = entropy_hash_sym32->PAIR6_KEY1;
    hash_sym->KEY32[7][0] = entropy_hash_sym32->PAIR7_KEY0;
    hash_sym->KEY32[7][1] = entropy_hash_sym32->PAIR7_KEY1;

    // PSEUDO-ARRAYS used in ENTROPY_HASH_SYM8/16/32 registers.
    // for (fm_byte pair = 0; pair < 8 ; pair++)
    // {
    //     for (fm_byte key = 0 ; key < 2 ; key++)
    //     {
    //         if (pair < 6)
    //         {
    //             hash_sym.KEY8 [pair][key] = entropy_hash_sym8->PAIR [pair][key];
    //             hash_sym.KEY16[pair][key] = entropy_hash_sym16->PAIR[pair][key];
    //         }
    //         hash_sym.KEY32[pair][key] = entropy_hash_sym32->PAIR[pair][key];
    //     }
    // }
}

static void mbyClsGetEntropyHashKeyMask
(
    mby_ppe_entropy_map   const * const entropy_map,
    fm_byte                       const hash_num,
    fm_byte                       const key_mask_profile,
    mbyEntropyHashKeyMask       * const hash_key_mask
)
{
    entropy_hash_key_mask_r const * entropy_hash_key_mask;
    fm_byte key_mask_idx;

    for (fm_byte dw = 0 ; dw < MBY_ENTROPY_HASH_KEY_MASK_DW_NUM ; dw++)
    {
        key_mask_idx = ( key_mask_profile * MBY_ENTROPY_HASH_KEY_MASK_DW_NUM ) + dw;

        entropy_hash_key_mask = &(entropy_map->ENTROPY_HASH_KEY_MASK[hash_num][key_mask_idx]);

        hash_key_mask->MASK[dw] = entropy_hash_key_mask->MASK;
    }
}

static void selectEntropyHashKeys
(
    mbyClassifierEntropyCfg const entropy_cfg,
    mbyClassifierKeys       const keys,
    mbyClassifierKeys     * const hash_keys
)
{
    for (fm_uint i = 0; i < MBY_CGRP_KEY32; i++)
        hash_keys->key32[i] = (FM_GET_UNNAMED_FIELD  (entropy_cfg.KEY32_MASK, i, 1)) ? keys.key32[i] : 0;

    for (fm_uint i = 0; i < MBY_CGRP_KEY16; i++)
        hash_keys->key16[i] = (FM_GET_UNNAMED_FIELD  (entropy_cfg.KEY16_MASK, i, 1)) ? keys.key16[i] : 0;

    for (fm_uint i = 0; i < MBY_CGRP_KEY8; i++)
        hash_keys->key8[i]  = (FM_GET_UNNAMED_FIELD64(entropy_cfg.KEY8_MASK,  i, 1)) ? keys.key8 [i] : 0;
}

void symmetrizeEntropyHash
(
    mby_ppe_entropy_map const * const entropy_map,
    fm_byte                     const hash_num,
    fm_byte                     const sym_profile,
    mbyClassifierKeys         * const hash_keys
)
{
    // REVISIT!!! This is not supported in code.
    // The behavior is undefined if any key number appears more than once within a single register
    // (i.e., indicated as paired more than once).
    mbyEntropyHashSym hash_sym = { 0 };

    mbyClsGetEntropyHashSym(entropy_map, hash_num, sym_profile, &hash_sym);

    fm_byte id0, id1;
    fm_byte   key8;
    fm_uint16 key16;
    fm_uint32 key32;

    for (fm_byte pair = 0; pair < 8 ; pair++)
    {
        if (pair < 6)
        {
            // KEY 8
            id0 = hash_sym.KEY8[pair][0];
            id1 = hash_sym.KEY8[pair][1];
            if( id0 != id1 )
            {
                key8 = hash_keys->key8[id0] ^ hash_keys->key8[id1];
                hash_keys->key8[id0] = key8;
                hash_keys->key8[id1] = key8;
            }
            // KEY 16
            id0 = hash_sym.KEY16[pair][0];
            id1 = hash_sym.KEY16[pair][1];
            if( id0 != id1 )
            {
                key16 = hash_keys->key16[id0] ^ hash_keys->key16[id1];
                hash_keys->key16[id0] = key16;
                hash_keys->key16[id1] = key16;
            }
        }
        // KEY 32
        id0 = hash_sym.KEY32[pair][0];
        id1 = hash_sym.KEY32[pair][1];
        if( id0 != id1 )
        {
            key32 = hash_keys->key32[id0] ^ hash_keys->key32[id1];
            hash_keys->key32[id0] = key32;
            hash_keys->key32[id1] = key32;
        }
    }
}

static void applyEntropyHashKeyMask
(
    mby_ppe_entropy_map const * const entropy_map,
    fm_byte                     const hash_num,
    fm_byte                     const key_mask_profile,
    fm_byte                     const hash_bytes[MBY_ENTROPY_HASH_KEY_MASK_BYTES],
    fm_byte                   * const masked_hash_bytes
)
{
    mbyEntropyHashKeyMask hash_key_mask = { 0 };

    mbyClsGetEntropyHashKeyMask(entropy_map, hash_num, key_mask_profile, &hash_key_mask);

    // MBY_CGRP_HASH_KEYS is 192, but 64 Bytes should be used here. <-- REVISIT!!!
    for (fm_byte dw = 0, i = 0 ; dw < MBY_ENTROPY_HASH_KEY_MASK_DW_NUM, i < MBY_ENTROPY_HASH_KEY_MASK_BYTES ; dw++)
    {
        fm_uint64 mask_dw = hash_key_mask.MASK[dw];
        for (fm_byte byte = 0 ; byte < BYTES_IN_DW ; byte++){
            masked_hash_bytes[i] = hash_bytes[i] & FM_GET_UNNAMED_FIELD(mask_dw, byte * BYTES_IN_DW, BYTES_IN_DW);
            i++;
        }
    }
}

static void generateEntropyHash
(
    mby_ppe_entropy_map const * const entropy_map,
    mbyClassifierKeys           const keys,
    fm_byte                   * const hash_profile,
    fm_byte                   * const mod_meta,
    fm_uint32                 * const hash0,
    fm_uint64                 * const hash1,
    fm_uint32                 * const hash2
)
{
    fm_uint32 hash_values  [mby_ppe_entropy_map_ENTROPY_HASH_CFG0__nd] = { 0 };

    for (fm_byte hash_num = 0; hash_num < mby_ppe_entropy_map_ENTROPY_HASH_CFG0__n; hash_num++)
    {
        // Get ENTROPY_HASH_CFG0/1 register fields:
        mbyClassifierEntropyCfg entropy_cfg = mbyClsGetEntropyCfg(entropy_map, hash_num, hash_profile[hash_num]);

        // Select hash keys from CLASSIFIER keys:
        // Based on FS it should be 64 Bytes maximum. Model uses 192 Bytes everywhere... <-- REVISIT!!!
        // Keys should be packed. <-- REVISIT!!!
        mbyClassifierKeys hash_keys;
        selectEntropyHashKeys(entropy_cfg, keys, &hash_keys);

        // Add keys packing into 64 Bytes vector here. <-- REVISIT!!!

        // Hash symmetrization
        // Should it be done before or after the keys selection? <-- REVISIT!!!
        if (entropy_cfg.SYMMETRIC)
        {
            symmetrizeEntropyHash(entropy_map, hash_num, entropy_cfg.SYM_PROFILE, &hash_keys);
        }

        // Convert Keys into array of bytes:
        // Based on FS it should be 64 Bytes maximum. Model uses 192 Bytes everywhere... <-- REVISIT!!!
        fm_byte hash_bytes[MBY_CGRP_HASH_KEYS] = { 0 };
        mbyClsConvertKeysToBytes(hash_keys, hash_bytes);

        // Apply hash key mask for the keys:
        // Based on FS it should be 64 Bytes maximum. Model uses 192 Bytes everywhere... <-- REVISIT!!!
        fm_byte masked_hash_bytes[MBY_CGRP_HASH_KEYS] = { 0 };
        applyEntropyHashKeyMask(entropy_map, hash_num, entropy_cfg.KEY_MASK_PROFILE, hash_bytes, masked_hash_bytes);

        // Get hash value from CRC:
        switch (hash_num)
        {
            case 0:
            {
                hash_values[hash_num] = mbyCrc32ByteSwap (hash_bytes, MBY_CGRP_HASH_KEYS); // HASH0: CRC-32 (Ethernet)
                break;
            }
            case 1:
            {
                hash_values[hash_num] = mbyCrc32CByteSwap(hash_bytes, MBY_CGRP_HASH_KEYS); // HASH1: CRC-32C (iSCSI)
                break;
            }
            case 2:
            {
                hash_values[hash_num] = mbyCrc32ByteSwap (hash_bytes, MBY_CGRP_HASH_KEYS); // HASH2: <-- REVISIT!!! Not specified in FS.
                break;
            }
            default:
                break;
        }
    }

    // ECMP HASH for ARP_TABLE:
    *hash0 = hash_values[0] & 0xFFFFFF;

    // Populate hash1 for use by the Modifier:
    mbyEntropyMetaCfg meta_cfg = mbyClsGetEntropyMetaCfg(entropy_map, hash_profile[1]);

    fm_uint64 hash1_default = 0; // local var

    // Apply Defaults to hash1:
    for (fm_uint i = 0; i < MBY_MOD_META_BYTES; i++)
    {
        fm_byte s = meta_cfg.BYTE_DEFAULTS[i];

        if (s < 3)
        {
            // hash1_default |= ((fm_uint64)mod_meta[s]) << i * BITS_IN_BYTE;
            // Which is more clear? Above or below? <-- REVISIT!!!
            FM_SET_UNNAMED_FIELD64(hash1_default, i * BITS_IN_BYTE, BITS_IN_BYTE, mod_meta[s]);
        }
        else
        {
            // Is this still accurate? There is additional META3 in MBY. <-- REVISIT!!!
            // Is this neccessary since it is initialized to 0 before the for loop? <-- REVISIT!!!
            FM_SET_UNNAMED_FIELD64(hash1_default, i * BITS_IN_BYTE, BITS_IN_BYTE, 0);
        }
    }

    // Apply Hash to hash1:
    fm_uint64 meta_mask = 0;
    for (fm_uint i = 0; i < 48; i++)
        if ((i >= meta_cfg.HASH_START) && (i < (meta_cfg.HASH_START + meta_cfg.HASH_SIZE)))
            meta_mask |= (FM_LITERAL_U64(1) << i);

    // This seems wrong: <-- REVISIT!!!
    // fm_uint64 meta_hash = (((fm_uint64) hash_values[1]) << 32) | hash_values[1];
    // This seems good:
    fm_uint64 meta_hash = (((fm_uint64) hash_values[1]) << meta_cfg.HASH_START) & meta_mask;

    *hash1 = (hash1_default & ~meta_mask) | meta_hash;

    // HASH0 and HASH1 are 32-bits, what about HASH2? <-- REVISIT!!!
    *hash2 = hash_values[2];
}

static void getFwdHashingCfg
(
    mby_ppe_entropy_map const * const entropy_map,
    mbyFwdHashingCfg          * const fwd_hashing_cfg
)
{
    entropy_fwd_hashing_cfg_r const * const fwd_hashing_cfg_reg = &(entropy_map->ENTROPY_FWD_HASHING_CFG[0 /* REVISIT!!! */]);

    fwd_hashing_cfg->ECMP_ROTATION = fwd_hashing_cfg_reg->ECMP_ROTATION;
    fwd_hashing_cfg->ROTATION_B    = fwd_hashing_cfg_reg->ROTATION_A;
    fwd_hashing_cfg->ROTATION_A    = fwd_hashing_cfg_reg->ROTATION_B;
}

static void insertPtable
(
    fm_uint16   const raw_hash,
    fm_uint16 * const final_hash
)
{
    fm_uint16 key   = raw_hash;

    fm_uint32 val   = lookUpPearsonHash(key);

    *final_hash = ((val & 0xF) << 8) | ((key >> 4) & 0xFF);
    // Should this 4b be placed as MSB or LSB? <-- REVISIT!!!
}

static void generateEcmpHash
(
    mbyFwdHashingCfg const * const fwd_hashing_cfg,
    fm_uint32                const hash0,
    mbyHashKeys            * const hash_keys
)
{
    fm_uint64 mask       = FM_LITERAL_U64(0xFFF);
    fm_uint16 ecmp_hash  = hash0 >> (fwd_hashing_cfg->ECMP_ROTATION * 12) & mask;

    insertPtable(ecmp_hash, &(hash_keys->ecmp_hash));
}

static void generateLagHash
(
    mbyFwdHashingCfg const * const fwd_hashing_cfg,
    fm_uint32                const hash2,
    mbyHashKeys            * const hash_keys
)
{
    fm_uint64 hash = FM_LITERAL_U64(0);
    fm_uint64 hash_mask = FM_LITERAL_U64(0xFFFFFF);
    fm_uint64 key_mask  = FM_LITERAL_U64(0xFFF);

    // Is this correct handling of hash2? REVISIT!!!
    hash ^= ( (hash2 & hash_mask) << 24 )
            | (hash2 & hash_mask);

    fm_uint16 rot_a_key = ( hash >> (fwd_hashing_cfg->ROTATION_A * 12) ) & key_mask;
    fm_uint16 rot_b_key = ( hash >> (fwd_hashing_cfg->ROTATION_B * 12) ) & key_mask;

    insertPtable(rot_a_key, &(hash_keys->lag_hashA));
    insertPtable(rot_b_key, &(hash_keys->lag_hashB));
}

static void generateOutput
(
    mbyFwdHashingCfg const * const fwd_hashing_cfg,
    fm_uint32                const hash0,
    fm_uint64                const hash1,
    fm_uint32                const hash2,
    mbyHashKeys            * const hash_keys
)
{
    // Calculate ecmp_hash:
    generateEcmpHash(fwd_hashing_cfg, hash0, hash_keys);

    // Copy hash1 to mod_meta:
    hash_keys->mod_meta = hash1;

    // Calculate lag_hash:
    generateLagHash(fwd_hashing_cfg, hash2, hash_keys);
}

static void calcArpHash
(
    fm_uint32   const operand,
    fm_byte           arp_hash[16]
)
{
    for (fm_uint32 i = 0, mult = 16; i < 16; i++, mult = (mult + 1) % 16)
        arp_hash[i] = (mult * operand) >> 12;
}

void Hash
(
    mby_ppe_entropy_map const * const entropy_map,
    mbyClassifierToHash const * const in,
    mbyHashToNextHop          * const out
)
{
    mbyClassifierKeys keys = in->FFU_KEYS;
    fm_byte hash_profile[MBY_CGRP_HASH_PROFILE_ACTIONS] = { 0 };
    fm_byte mod_meta[MBY_CGRP_META_ACTIONS]             = { 0 };

    for (fm_uint i = 0 ; i < MBY_CGRP_HASH_PROFILE_ACTIONS ; i++)
        hash_profile[i] = in->HASH_PROFILE[i];

    for (fm_uint i = 0 ; i < MBY_CGRP_META_ACTIONS ; i++)
        mod_meta[i] = in->MOD_META[i];

    // Calculate raw hash0, hash1 and hash2:
    fm_uint32 hash0 = 0;
    fm_uint64 hash1 = 0;
    fm_uint32 hash2 = 0;
    generateEntropyHash
    (
        entropy_map,
        keys,
        hash_profile,
        mod_meta,
        &hash0,
        &hash1,
        &hash2
    );

    // Get Hashing configuration:
    mbyFwdHashingCfg fwd_hashing_cfg;
    getFwdHashingCfg(entropy_map, &fwd_hashing_cfg);

    mbyHashKeys hash_keys = { 0 };
    generateOutput
    (
        &fwd_hashing_cfg,
        hash0,
        hash1,
        hash2,
        &hash_keys
    );

    // Verify if this is needed? <--- REVISIT!!!
    // Calculate ARP hash:
    fm_byte   arp_hash[16] = { 0 };
    calcArpHash
    (
        hash_keys.ecmp_hash,
        arp_hash
    );

    // Init SV drop
    fm_byte sv_drop = MBY_SV_MOVE_DROP_RESERVED;

    // Write outputs:

    for (fm_uint i = 0; i < 16; i++)
        out->ARP_HASH[i]         = arp_hash[i];

    out->HASH_KEYS               = hash_keys; // Is this useful later? <-- REVISIT!!!
    out->HASH_ROT_A              = hash_keys.lag_hashA;
    out->HASH_ROT_B              = hash_keys.lag_hashB;
    out->ECMP_HASH               = hash_keys.ecmp_hash;
    out->SV_DROP                 = sv_drop;

    // Pass thru:
    // REVISIT!!!
    /* Not all fields specified in mbyHashToNextHop
     * are copied from in to out. */
    out->L2_DMAC                 = in->L2_DMAC;
    out->L2_SMAC                 = in->L2_SMAC;
    out->CGRP_FLAGS              = in->CGRP_FLAGS;
    out->CGRP_ROUTE              = in->CGRP_ROUTE;
    out->CGRP_TRIG               = in->CGRP_TRIG;
    out->ENCAP                   = in->ENCAP;
    out->DECAP                   = in->DECAP;
    out->DMAC_FROM_IPV6          = in->DMAC_FROM_IPV6;
    out->DROP_TTL                = in->DROP_TTL;
    out->L2_IDOMAIN              = in->L2_IDOMAIN;
    out->L3_IDOMAIN              = in->L3_IDOMAIN;
    out->L2_IVID1                = in->L2_IVID1;
    out->LEARN_MODE              = in->LEARN_MODE;
    out->MOD_PROF_IDX            = in->MOD_PROF_IDX;
    out->PARITY_ERROR            = in->PARITY_ERROR;
    out->PARSER_ERROR            = in->PARSER_ERROR;
    out->PARSER_INFO             = in->PARSER_INFO;
    out->PA_HDR_PTRS             = in->PA_HDR_PTRS;
    out->PA_L3LEN_ERR            = in->PA_L3LEN_ERR;
    out->RX_DATA                 = in->RX_DATA;
    out->RX_LENGTH               = in->RX_LENGTH;
    out->RX_PORT                 = in->RX_PORT;
    out->TRAFFIC_CLASS           = in->TRAFFIC_CLASS;
    out->TRAP_IGMP               = in->TRAP_IGMP;
}
