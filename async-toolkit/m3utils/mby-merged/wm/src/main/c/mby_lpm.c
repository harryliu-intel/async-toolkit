// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "mby_lpm.h"

static void lookUpLpmTcam
(
    MBY_LPM_IN_REGS,
    mbyLpmTcamLookup * const tcam_lookup
)
{
    fm_uint16 tcam_index = 0;

    assert(tcam_lookup);

    tcam_lookup->hit_valid = FALSE;

    while (tcam_index < //MBY_LPM_REG_SIZE(LPM_MATCH_TCAM)
           mby_ppe_cgrp_a_nested_map_LPM_MATCH_TCAM__n
           )
    {
        mbyLpmTcamEntry tcam_entry;

        mbyLpmGetTcamEntry(MBY_LPM_IN_REGS_P, tcam_index, &tcam_entry);

        fm_uint64 cam_key_inv = tcam_entry.key_invert;
        fm_uint64 cam_key     = tcam_entry.key;
        fm_uint64 mask        = cam_key ^ cam_key_inv;

        if (((cam_key & cam_key_inv) == 0) && ((tcam_lookup->key & mask) == (cam_key & mask)))
        {
            tcam_lookup->hit_valid = TRUE;
            tcam_lookup->hit_index = tcam_index;
            // Search the entire table and return the match with highest index
        }

        ++tcam_index;
    }
}

static fm_bool getBitIn64BitsArray
(
    fm_uint64       const * const   array,
    fm_byte                         bit_num
)
{
    assert(array);
    // Used for prefix and child map - bit 0 is the msb
    return (array[bit_num / 64] >> (63 - (bit_num % 64))) & 0x1;
}

// Return the number of 1s between array lsb and bit_num (excluded)
static fm_byte countOneIn64BitsArray
(
    fm_uint64       const * const   array,
    fm_byte                         bit_num
)
{
    fm_byte count = 0;
    fm_byte i;

    assert(array);

    for (i = 0; i < bit_num; ++i)
        count += getBitIn64BitsArray(array, i);

    return count;
}

static fm_bool getSubtriePrefixNode
(
    mbyLpmSubtrieStore const * const    st_store,
    fm_byte                             idx
)
{
    assert(st_store);
    assert(idx < MBY_LPM_NUM_PREFIXES);
    return getBitIn64BitsArray(st_store->prefix_bitmap, idx);
}

static fm_bool getSubtrieChildNode
(
    mbyLpmSubtrieStore const * const    st_store,
    fm_byte                             idx
)
{
    assert(st_store);
    // Always true: assert(idx < MBY_LPM_NUM_CHILD);
    return getBitIn64BitsArray(st_store->child_bitmap, idx);
}

static void exploreSubtrie
(
    MBY_LPM_IN_REGS,
    mbyLpmSubtrie const * const subtrie,
    mbyLpmSubtrieLookup * const st_lookup
)
{
    /* Exploration steps
     * - Explore the trie 1 bit at a time
     * - if 1 => update the hit since we are looking for longest match
     * - Decrease the key_len at each step
     *
     * Stop when:
     * - The key len is 0
     * - Child subtrie is empty
     *
     * Recursive step:
     * - Move to the next 8 bits of the key
     * - Subtrie is pointed by CPTR
     */

    fm_byte key = st_lookup->key[0];
    mbyLpmSubtrieStore st_store;
    fm_byte node_idx = 0;
    fm_byte level = 0;
    fm_bool node_val;
    fm_bool key_bit;

    // The key can't be longer than 16B: 20B total key len - 4B tcam key len
//T:assert(st_lookup->key_len < 16 * 8);

    mbyLpmGetSubtrieStore(MBY_LPM_IN_REGS_P, subtrie->root_ptr, &st_store);

    do
    {
        node_val = getSubtriePrefixNode(&st_store, node_idx);
        if (node_val)
        {
            fm_byte action_idx = countOneIn64BitsArray(st_store.prefix_bitmap, node_idx);
            st_lookup->hit_ptr = st_store.action_base_ptr + action_idx;
            st_lookup->hit_valid = TRUE;
        }

        // Check if we have processed the entire key.
        // Note that also if key_len = 0 we might have a match on the root node
        // so I can't do this check earlier.
        if (st_lookup->key_len == 0)
            return;

        // Read next bit of the key and move to the next node
        key_bit = (key >> (7 - level)) & 0x1;
        node_idx = 1 + 2 * node_idx + key_bit;
        --st_lookup->key_len;
        ++level;
    }
    while (level < 8);

    // Process the key lsb by checking the child nodes
    node_idx -= MBY_LPM_NUM_PREFIXES;
    node_val = getSubtrieChildNode(&st_store, node_idx);

    if (node_val)
    {
        mbyLpmSubtrie child_subtrie;
        fm_byte child_idx;

        child_idx = countOneIn64BitsArray(st_store.child_bitmap, node_idx);

        mbyLpmGetSubtrie(MBY_LPM_IN_REGS_P, subtrie->child_base_ptr + child_idx, &child_subtrie);

        st_lookup->key = &(st_lookup->key[1]);

        exploreSubtrie(MBY_LPM_IN_REGS_P, &child_subtrie, st_lookup);
    }
}

// Internal LPM function that takes the processed key as an argument
static void lpmSearch
(
    MBY_LPM_IN_REGS,
    mbyLpmKey    const * const in,
    mbyLpmSearchResult * const out
)
{
    mbyLpmTcamLookup          tcam_lookup;
    mbyLpmSubtrie             tcam_subtrie;
    mbyLpmSubtrieLookup       st_lookup;

    // TODO verify what happens when key_len < 33 bits (i.e. TCAM key len + 1)
//T:assert(in->key_len >= 33);
//T:assert(in->key_len < MBY_LPM_KEY_MAX_BITS_LEN);

    // FIXME adjust based on how the key is stored in memory
    tcam_lookup.key = in->key[0] | (in->key[1] << 8) | (in->key[2] << 16) | (in->key[3] << 24);

    lookUpLpmTcam(MBY_LPM_IN_REGS_P, &tcam_lookup);

    if (!tcam_lookup.hit_valid)
    {
        out->hit_valid = FALSE;
        return;
    }

    mbyLpmGetTcamSubtrie(MBY_LPM_IN_REGS_P, tcam_lookup.hit_index, &tcam_subtrie);

    st_lookup.key       = (fm_byte *) &(in->key[4]);
    st_lookup.key_len   = in->key_len - 32;
    st_lookup.hit_valid = FALSE;

    exploreSubtrie(MBY_LPM_IN_REGS_P, &tcam_subtrie, &st_lookup);

    out->hit_valid = st_lookup.hit_valid;
    if (out->hit_valid)
    {
        // TODO verify alignment in SHM_FWD_TABLE0
        out->fwd_table0_idx = st_lookup.hit_ptr * 16;
    }
}

static void lpmGenerateKey
(
    MBY_LPM_IN_REGS,
    mbyClassifierKeys    const * const keys,
    fm_byte                            profile_id,
    mbyLpmKey                  * const lpmKey
)
{
    mbyLpmKeySels key_sels;
    fm_byte len = 0;
    fm_byte i;

    assert(keys);
    assert(lpmKey);
    assert(profile_id < 64); // 6 bits value

    mbyLpmGetKeySels(&(MBY_LPM_IN_REGS_P->A), profile_id, &key_sels);

    lpmKey->key_len = 0; // remember this is in bits
    memset(lpmKey->key, 0, MBY_LPM_KEY_MAX_BYTES_LEN);

#define PACK_LPM_KEY(key_type, key_size)                                       \
    for(i = 0; i < MBY_FFU_KEY ##key_size ; ++i)                               \
    {                                                                          \
        if ((key_sels.key_type ## _key ## key_size ## _sel >> i) & 0x1)      \
        {                                                                      \
            memcpy(lpmKey->key + len, keys->key## key_size + i, key_size / 8); \
            len += key_size / 8;                                               \
        }                                                                      \
    }

    // Start from the LSB (address key8s) to the MSB (metadata key16s)
    PACK_LPM_KEY(addr, 8);
    PACK_LPM_KEY(addr, 16);
    PACK_LPM_KEY(addr, 32);
    PACK_LPM_KEY(md,   8);
    PACK_LPM_KEY(md,   16);

    // Apply the 160 bit mask
    for (i = 0; i < MBY_LPM_KEY_MAX_BYTES_LEN; ++i)
        // FIXME why is the mask 20 x 64 bits long?
        lpmKey->key_len = key_sels.key_mask[i] & 0xff;

    lpmKey->key_len = len * 8;
}

static void lpmActions
(
    mby_shm_map                * const shm_map,
    fm_byte                            profile_id,
    mbyLpmSearchResult   const * const searchResult,
    fm_uint32                          actions[MBY_LPM_MAX_ACTIONS_NUM]
)
{

    assert(searchResult);
    assert(actions);

    // By default all actions are NOP
    memset(actions, 0, MBY_LPM_MAX_ACTIONS_NUM * sizeof(fm_uint32));

    if (!searchResult->hit_valid)
        return;

    // FIXME use profile_id to decide how many actions to read...HOW?
    fm_uint64 fwd_table_entry = shm_map->FWD_TABLE0[searchResult->fwd_table0_idx][0].DATA;
    actions[1] = fwd_table_entry >> 32 & 0xffffffff;
    actions[0] = fwd_table_entry & 0xffffffff;
}

void mbyMatchLpm
(
    MBY_LPM_IN_REGS,
    mby_shm_map                * const shm_map,
    mbyClassifierKeys    const * const keys,
    fm_byte                            profile_id,
    fm_uint32                          actions[MBY_LPM_MAX_ACTIONS_NUM]
)
{
    mbyLpmSearchResult searchResult;
    mbyLpmKey key;

    lpmGenerateKey(MBY_LPM_IN_REGS_P, keys, profile_id, &key);

    lpmSearch(MBY_LPM_IN_REGS_P, &key, &searchResult);

    lpmActions(shm_map, profile_id, &searchResult, actions);
}

//#ifdef UNIT_TEST
void mbyGetLpmStaticFuncs(struct mbyLpmStaticFuncs *funcs)
{
        funcs->_lookUpLpmTcam = lookUpLpmTcam;
        funcs->_getBitIn64BitsArray = getBitIn64BitsArray;
        funcs->_countOneIn64BitsArray = countOneIn64BitsArray;
        funcs->_getSubtriePrefixNode = getSubtriePrefixNode;
        funcs->_getSubtrieChildNode = getSubtrieChildNode;
        funcs->_exploreSubtrie = exploreSubtrie;
        funcs->_lpmSearch = lpmSearch;
        funcs->_lpmGenerateKey = lpmGenerateKey;
}
//#endif
