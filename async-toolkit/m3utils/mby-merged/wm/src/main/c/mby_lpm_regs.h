// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_LPM_REGS_H
#define MBY_LPM_REGS_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"

// Defines:

#include <mby_top_map.h>



// Input registers for all the functions in this block
#define MBY_LPM_IN_REGS    mby_ppe_cgrp_a_map * const cgrp_a_map
#define MBY_LPM_IN_REGS_P                             cgrp_a_map

// Retrieve the constant with the number of registers
#define MBY_REG_SIZE(reg_name) (mby_ppe_cgrp_a_map_## reg_name ##__nd)

#define MBY_LPM_KEY_MAX_BYTES_LEN   20
#define MBY_LPM_KEY_MAX_BITS_LEN    (MBY_LPM_KEY_MAX_BYTES_LEN * 8)

#define MBY_LPM_BITMAP_SIZE         4
#define MBY_LPM_NUM_PREFIXES        255
#define MBY_LPM_NUM_CHILD           256

// Enums:

// Structs:

typedef struct mbyLpmKeyMasksStruct
{
    fm_uint64               key_mask[MBY_LPM_KEY_MAX_BYTES_LEN];
    fm_uint64               addr_key8_mask;
    fm_uint32               addr_key16_mask;
    fm_uint16               addr_key32_mask;
    fm_uint64               md_key8_mask;
    fm_uint32               md_key16_mask;
} mbyLpmKeyMasks;

typedef struct mbyLpmTcamEntryStruct
{
    fm_uint32               key;
    fm_uint32               key_invert;

} mbyLpmTcamEntry;

typedef struct mbyLpmSubtrieStruct
{
    // TODO maybe this is not necessary
    fm_uint16             root_ptr;
    fm_uint16             child_base_ptr;
    fm_byte               child_ptr_len;
} mbyLpmSubtrie;

typedef struct mbyLpmSubtrieStoreStruct
{
    fm_uint64               prefix_bitmap[MBY_LPM_BITMAP_SIZE]; // 255b
    fm_uint64               child_bitmap[MBY_LPM_BITMAP_SIZE];  // 256b
    fm_uint32               action_base_ptr; // 19 (or 20?) bits
} mbyLpmSubtrieStore;

// Functions:

void mbyLpmGetKeyMasks
(
    MBY_LPM_IN_REGS,
    fm_byte                  const profile_id,
    mbyLpmKeyMasks         * const key_masks
);

void mbyLpmGetTcamEntry
(
    MBY_LPM_IN_REGS,
    const fm_uint16                index,
    mbyLpmTcamEntry        * const tcam_entry
);

void mbyLpmGetTcamSubtrie
(
    MBY_LPM_IN_REGS,
    const fm_uint16                index,
    mbyLpmSubtrie          * const tcam_subtrie
);

void mbyLpmGetSubtrie
(
    MBY_LPM_IN_REGS,
    const fm_uint16                index,
    mbyLpmSubtrie          * const subtrie
);

void mbyLpmGetSubtrieStore
(
    MBY_LPM_IN_REGS,
    const fm_uint16                index,
    mbyLpmSubtrieStore     * const st_store
);

#endif /* MYB_LPM_REGS_H */
