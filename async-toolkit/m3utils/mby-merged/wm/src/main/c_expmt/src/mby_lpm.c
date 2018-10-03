// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include <assert.h>
#include <stdio.h>
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

    while (tcam_index < MBY_REG_SIZE(LPM_MATCH_TCAM))
    {
        mbyLpmTcamEntry tcam_entry;

        mbyLpmGetTcamEntry(MBY_LPM_IN_REGS_P, tcam_index, &tcam_entry);

        fm_uint64 cam_key_inv = tcam_entry.key_invert;
        fm_uint64 cam_key     = tcam_entry.key;
        fm_uint64 mask        = cam_key ^ cam_key_inv;

        // TODO verify this check: I copy pasted it from the classifier
        if (((cam_key & cam_key_inv) == 0) && ((tcam_lookup->key & mask) == (cam_key & mask)))
        {
            tcam_lookup->hit_valid = TRUE;
            tcam_lookup->hit_index = tcam_index;
            // TODO verify it is OK to return on the first hit
            return;
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
    mbyLpmKey   const * const in,
    mbyLpmOut         * const out
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

void mbyMatchLpm
(
    MBY_LPM_IN_REGS,
    mbyClassifierKeys    const * const keys,
    fm_byte                            profile_id,
    mbyLpmOut                  * const out
)
{
    mbyLpmKeyMasks key_masks;

    mbyLpmGetKeyMasks(MBY_LPM_IN_REGS_P, profile_id, &key_masks);

    // TODO properly initialize these before calling the internal function
    mbyLpmKey in = { 0 };
    // in.key = ...;
    // in.key_len = ...;

    lpmSearch(MBY_LPM_IN_REGS_P, &in, out);

    // TODO read the action from FWD_TABLE0
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
}
//#endif
