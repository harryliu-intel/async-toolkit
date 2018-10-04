// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_LPM_H
#define MBY_LPM_H

// Includes:

#include "mby_common.h"
#include "mby_lpm_regs.h"

// Defines:

// Enums:

// Structs:

typedef struct mbyLpmKeyStruct
{
    fm_byte key[MBY_LPM_KEY_MAX_BYTES_LEN];
    fm_byte key_len; // in bits
} mbyLpmKey;

typedef struct mbyLpmOutStruct
{
    fm_bool   hit_valid;
    fm_uint64 fwd_table0_idx;
} mbyLpmOut;

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
    MBY_LPM_IN_REGS,
    mbyClassifierKeys    const * const keys,
    fm_byte                            profile_id,
    mbyLpmOut                  * const out
);

//#ifdef UNIT_TEST
struct mbyLpmStaticFuncs {
    void    (*_lookUpLpmTcam)(MBY_LPM_IN_REGS, mbyLpmTcamLookup * const);
    fm_bool (*_getBitIn64BitsArray)(fm_uint64 const * const, fm_byte);
    fm_byte (*_countOneIn64BitsArray)(fm_uint64 const * const, fm_byte);
    fm_bool (*_getSubtriePrefixNode)(mbyLpmSubtrieStore const * const, fm_byte);
    fm_bool (*_getSubtrieChildNode)(mbyLpmSubtrieStore const * const, fm_byte);
    void    (*_exploreSubtrie)(MBY_LPM_IN_REGS, mbyLpmSubtrie const * const, mbyLpmSubtrieLookup * const);
    void    (*_lpmSearch)(MBY_LPM_IN_REGS, mbyLpmKey const * const, mbyLpmOut * const);
    void    (*_generateLpmKey)(MBY_LPM_IN_REGS, mbyClassifierKeys const * const, fm_byte, mbyLpmKey * const);
};

void mbyGetLpmStaticFuncs(struct mbyLpmStaticFuncs *funcs);
//#endif

#endif /* MYB_LPM_H */
