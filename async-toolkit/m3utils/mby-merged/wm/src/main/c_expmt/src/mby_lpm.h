// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_LPM_H
#define MBY_LPM_H

// Includes:

#include "mby_common.h"

// Defines:

#define MBY_LPM_KEY_MAX_BITS_LEN    (20 * 8)

#define MBY_LPM_BITMAP_SIZE         4
#define MBY_LPM_NUM_PREFIXES        255
#define MBY_LPM_NUM_CHILD           256

// Enums:

// Structs:

typedef struct mbyLpmInStruct
{
    // TODO where does the key come from? Spec says is derived from Profile ID
    fm_byte key[MBY_LPM_KEY_MAX_BITS_LEN / 8];
    fm_byte key_len; // in bits
} mbyLpmIn;

typedef struct mbyLpmOutStruct
{
    fm_bool   hit_valid;
    fm_uint64 fwd_table0_idx;
} mbyLpmOut;

typedef struct mbyLpmTcamLookupStruct
{
    fm_uint32               key;
    fm_uint32               key_invert;
    fm_bool                 hit_valid;
    fm_uint16               hit_index;
} mbyLpmTcamLookup;

typedef struct mbyLpmSubtrieLookupStruct
{
    fm_byte               * key;
    fm_byte                 key_len;
    fm_bool                 hit_valid;
    fm_uint32               hit_ptr;
} mbyLpmSubtrieLookup;

// Functions :

void Lpm
(
    fm_uint32                 regs[MBY_REGISTER_ARRAY_SIZE],
    mbyLpmIn    const * const in,
    mbyLpmOut         * const out
);

#endif /* MYB_LPM_H */
