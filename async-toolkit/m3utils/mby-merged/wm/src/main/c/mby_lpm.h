// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_LPM_H
#define MBY_LPM_H

// Includes:

#include "mby_common.h"
#include "mby_lpm_regs.h"

// Defines:

// TODO review this with architects
#define MBY_LPM_MAX_ACTIONS_NUM 4

// Enums:

// Structs:

typedef struct mbyLpmKeyStruct
{
    fm_byte key[MBY_LPM_KEY_MAX_BYTES_LEN];
    fm_byte key_len; // in bits
} mbyLpmKey;

typedef struct mbyLpmSearchResultStruct
{
    fm_bool   hit_valid;
    fm_uint64 fwd_table0_idx;
} mbyLpmSearchResult;

typedef struct mbyLpmTcamLookupStruct
{
    fm_uint32               key;
    fm_bool                 hit_valid;
    fm_uint16               hit_index;
} mbyLpmTcamLookup;

typedef struct mbyLpmSubtrieLookupStruct
{
    fm_byte         const * key;     // key to search - unprocessed tail only
    fm_byte                 key_len; // in bits
    fm_bool                 hit_valid;
    fm_uint32               hit_ptr;
} mbyLpmSubtrieLookup;

// Functions :

void mbyMatchLpm
(
    mby_ppe_cgrp_a_map * const cgrp_a_map,
    mby_shm_map                * const shm_map,
    mbyClassifierKeys    const * const keys,
    fm_byte                            profile_id,
    fm_uint32                          actions[4]
);

//#ifdef UNIT_TEST
struct mbyLpmStaticFuncs {
    void    (*_lookUpLpmTcam)(mby_ppe_cgrp_a_map * const, mbyLpmTcamLookup * const);
    fm_bool (*_getBitIn64BitsArray)(fm_uint64 const * const, fm_byte);
    fm_byte (*_countOneIn64BitsArray)(fm_uint64 const * const, fm_byte);
    fm_bool (*_getSubtriePrefixNode)(mbyLpmSubtrieStore const * const, fm_byte);
    fm_bool (*_getSubtrieChildNode)(mbyLpmSubtrieStore const * const, fm_byte);
    void    (*_exploreSubtrie)(mby_ppe_cgrp_a_map * const cgrp_a_map, mbyLpmSubtrie const * const, mbyLpmSubtrieLookup * const);
    void    (*_lpmSearch)(mby_ppe_cgrp_a_map * const, mbyLpmKey const * const, mbyLpmSearchResult * const);
    void    (*_lpmGenerateKey)(mby_ppe_cgrp_a_map * const, mbyClassifierKeys const * const, fm_byte, mbyLpmKey * const);
};

void mbyGetLpmStaticFuncs(struct mbyLpmStaticFuncs *funcs);
//#endif

#endif /* MYB_LPM_H */
