// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_LPM_REGS_H
#define MBY_LPM_REGS_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"

// Defines:

#ifdef USE_NEW_CSRS
#include <mby_top_map.h>

#else

/* Copy-pasted from mby_regs.h generated with reg_gen:
 *   cd hpfd-mby_hw/wm/src/main/c_expmt/tools/reg_gen
 *   RDL_DIR=/path/to/hpfd-mby_hw/tools/srdl/mby/ make
 * Note: tools requires modified top_map with flattened top level
 */
#define MBY_MPT_BASE                                            (0x0000000)
#define MBY_MPT_SIZE                                            (0x0000008)

#define MBY_LPM_SUBTRIE_APTR_WIDTH                              2
#define MBY_LPM_SUBTRIE_APTR_ENTRIES_0                          24576
#define MBY_LPM_SUBTRIE_APTR_ENTRIES_1                          2
#define MBY_LPM_SUBTRIE_APTR(index1, index0, word)              ((0x0000004) * ((index1) - 0) + (0x0000004) * ((index0) - 0) + ((word)*4)+ (0x0000000) + (MBY_MPT_BASE))

#define MBY_LPM_SUBTRIE_APTR_l_ACTION_BASE_PTR                  0
#define MBY_LPM_SUBTRIE_APTR_h_ACTION_BASE_PTR                  18

#define MBY_LPM_SUBTRIE_CPTR_WIDTH                              2
#define MBY_LPM_SUBTRIE_CPTR_ENTRIES_0                          24576
#define MBY_LPM_SUBTRIE_CPTR_ENTRIES_1                          2
#define MBY_LPM_SUBTRIE_CPTR(index1, index0, word)              ((0x0000004) * ((index1) - 0) + (0x0000004) * ((index0) - 0) + ((word)*4)+ (0x0000000) + (MBY_MPT_BASE))

#define MBY_LPM_SUBTRIE_CPTR_l_CHILD_PTR_LEN                    32
#define MBY_LPM_SUBTRIE_CPTR_h_CHILD_PTR_LEN                    39
#define MBY_LPM_SUBTRIE_CPTR_l_CHILD_BASE_PTR                   16
#define MBY_LPM_SUBTRIE_CPTR_h_CHILD_BASE_PTR                   31
#define MBY_LPM_SUBTRIE_CPTR_l_SUBTRIE_PTR                      0
#define MBY_LPM_SUBTRIE_CPTR_h_SUBTRIE_PTR                      15

#define MBY_LPM_SUBTRIE_BITMAPS_WIDTH                           2
#define MBY_LPM_SUBTRIE_BITMAPS_ENTRIES_0                       16
#define MBY_LPM_SUBTRIE_BITMAPS_ENTRIES_1                       24576
#define MBY_LPM_SUBTRIE_BITMAPS_ENTRIES_2                       2
#define MBY_LPM_SUBTRIE_BITMAPS(index2, index1, index0, word)   ((0x0000004) * ((index2) - 0)+(0x0000004) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0000000) + (MBY_MPT_BASE))

#define MBY_LPM_SUBTRIE_BITMAPS_l_BITMAP                        0
#define MBY_LPM_SUBTRIE_BITMAPS_h_BITMAP                        63

#define MBY_LPM_MATCH_TCAM_WIDTH                                2
#define MBY_LPM_MATCH_TCAM_ENTRIES_0                            512
#define MBY_LPM_MATCH_TCAM_ENTRIES_1                            2
#define MBY_LPM_MATCH_TCAM(index1, index0, word)                ((0x0000004) * ((index1) - 0) + (0x0000004) * ((index0) - 0) + ((word)*4)+ (0x0000000) + (MBY_MPT_BASE))

#define MBY_LPM_MATCH_TCAM_l_KEY_INVERT                         32
#define MBY_LPM_MATCH_TCAM_h_KEY_INVERT                         63
#define MBY_LPM_MATCH_TCAM_l_KEY                                0
#define MBY_LPM_MATCH_TCAM_h_KEY                                31

#define MBY_LPM_MATCH_ACTION_WIDTH                              2
#define MBY_LPM_MATCH_ACTION_ENTRIES_0                          512
#define MBY_LPM_MATCH_ACTION_ENTRIES_1                          2
#define MBY_LPM_MATCH_ACTION(index1, index0, word)              ((0x0000004) * ((index1) - 0) + (0x0000004) * ((index0) - 0) + ((word)*4)+ (0x0000000) + (MBY_MPT_BASE))

#define MBY_LPM_MATCH_ACTION_l_CHILD_PTR_LEN                    32
#define MBY_LPM_MATCH_ACTION_h_CHILD_PTR_LEN                    39
#define MBY_LPM_MATCH_ACTION_l_CHILD_BASE_PTR                   16
#define MBY_LPM_MATCH_ACTION_h_CHILD_BASE_PTR                   31
#define MBY_LPM_MATCH_ACTION_l_ROOT_PTR                         0
#define MBY_LPM_MATCH_ACTION_h_ROOT_PTR                         15

#define MBY_LPM_KEY_MASK_WIDTH                                  2
#define MBY_LPM_KEY_MASK_ENTRIES_0                              20
#define MBY_LPM_KEY_MASK_ENTRIES_1                              64
#define MBY_LPM_KEY_MASK_ENTRIES_2                              2
#define MBY_LPM_KEY_MASK(index2, index1, index0, word)          ((0x0000004) * ((index2) - 0)+(0x0000004) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0000000) + (MBY_MPT_BASE))

#define MBY_LPM_KEY_MASK_l_MASK                                 0
#define MBY_LPM_KEY_MASK_h_MASK                                 63

#define MBY_LPM_KEY_SEL0_WIDTH                                  2
#define MBY_LPM_KEY_SEL0_ENTRIES_0                              64
#define MBY_LPM_KEY_SEL0_ENTRIES_1                              2
#define MBY_LPM_KEY_SEL0(index1, index0, word)                  ((0x0000004) * ((index1) - 0) + (0x0000004) * ((index0) - 0) + ((word)*4)+ (0x0000000) + (MBY_MPT_BASE))

#define MBY_LPM_KEY_SEL0_l_MD_KEY16_MASK                        0
#define MBY_LPM_KEY_SEL0_h_MD_KEY16_MASK                        63

#define MBY_LPM_KEY_SEL1_WIDTH                                  2
#define MBY_LPM_KEY_SEL1_ENTRIES_0                              64
#define MBY_LPM_KEY_SEL1_ENTRIES_1                              2
#define MBY_LPM_KEY_SEL1(index1, index0, word)                  ((0x0000004) * ((index1) - 0) + (0x0000004) * ((index0) - 0) + ((word)*4)+ (0x0000000) + (MBY_MPT_BASE))

#define MBY_LPM_KEY_SEL1_l_ADDR_KEY8_MASK                       32
#define MBY_LPM_KEY_SEL1_h_ADDR_KEY8_MASK                       63
#define MBY_LPM_KEY_SEL1_l_MD_KEY8_MASK                         0
#define MBY_LPM_KEY_SEL1_h_MD_KEY8_MASK                         31

#define MBY_LPM_KEY_SEL2_WIDTH                                  2
#define MBY_LPM_KEY_SEL2_ENTRIES_0                              64
#define MBY_LPM_KEY_SEL2_ENTRIES_1                              2
#define MBY_LPM_KEY_SEL2(index1, index0, word)                  ((0x0000004) * ((index1) - 0) + (0x0000004) * ((index0) - 0) + ((word)*4)+ (0x0000000) + (MBY_MPT_BASE))

#define MBY_LPM_KEY_SEL2_l_ADDR_KEY16_MASK                      0
#define MBY_LPM_KEY_SEL2_h_ADDR_KEY16_MASK                      63

#define MBY_LPM_KEY_SEL3_WIDTH                                  2
#define MBY_LPM_KEY_SEL3_ENTRIES_0                              64
#define MBY_LPM_KEY_SEL3_ENTRIES_1                              2
#define MBY_LPM_KEY_SEL3(index1, index0, word)                  ((0x0000004) * ((index1) - 0) + (0x0000004) * ((index0) - 0) + ((word)*4)+ (0x0000000) + (MBY_MPT_BASE))

#define MBY_LPM_KEY_SEL3_l_ADDR_KEY32_MASK                      0
#define MBY_LPM_KEY_SEL3_h_ADDR_KEY32_MASK                      15

/******** SHM_BASE *******/
#define MBY_SHM_BASE                                            (0x0000000)
#define MBY_SHM_SIZE                                            (0x0000001)

#define MBY_FWD_TABLE0_WIDTH                                    2
#define MBY_FWD_TABLE0_ENTRIES_0                                256
#define MBY_FWD_TABLE0_ENTRIES_1                                3072
#define MBY_FWD_TABLE0(index1, index0, word)                    ((0x0000004) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0000000) + (MBY_SHM_BASE))

#define MBY_FWD_TABLE0_l_DATA                                   0
#define MBY_FWD_TABLE0_h_DATA                                   63

#define MBY_FWD_TABLE1_WIDTH                                    2
#define MBY_FWD_TABLE1_ENTRIES_0                                256
#define MBY_FWD_TABLE1_ENTRIES_1                                512
#define MBY_FWD_TABLE1(index1, index0, word)                    ((0x0000004) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0000000) + (MBY_SHM_BASE))

#define MBY_FWD_TABLE1_l_DATA                                   0
#define MBY_FWD_TABLE1_h_DATA                                   63

#define MBY_FWD_TABLE_MOD_WIDTH                                 2
#define MBY_FWD_TABLE_MOD_ENTRIES_0                             256
#define MBY_FWD_TABLE_MOD_ENTRIES_1                             640
#define MBY_FWD_TABLE_MOD(index1, index0, word)                 ((0x0000004) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0000000) + (MBY_SHM_BASE))

#define MBY_FWD_TABLE_MOD_l_DATA                                0
#define MBY_FWD_TABLE_MOD_h_DATA                                63

#endif /* USE_NEW_CSRS */


// Input registers for all the functions in this block
#ifdef USE_NEW_CSRS
#define MBY_LPM_IN_REGS    mby_ppe_cgrp_a_map * const cgrp_a_map
#define MBY_LPM_IN_REGS_P                             cgrp_a_map
#else
#define MBY_LPM_IN_REGS    fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE]
#define MBY_LPM_IN_REGS_P            regs
#endif

// Retrieve the constant with the number of registers
#ifdef USE_NEW_CSRS
#define MBY_REG_SIZE(reg_name) (mby_ppe_cgrp_a_map_## reg_name ##__nd)
#else
#define MBY_REG_SIZE(reg_name) (MBY_## reg_name ##_ENTRIES_0)
#endif

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
    fm_uint32               addr_key8_mask;
    fm_uint32               addr_key16_mask;
    fm_uint16               addr_key32_mask;
    fm_uint32               md_key8_mask;
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
