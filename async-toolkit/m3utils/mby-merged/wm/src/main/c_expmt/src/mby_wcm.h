// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_WCM_H
#define MBY_WCM_H

// Includes:

#include "mby_clsfr_regs.h"

// Defines:

// Enums:

// Structs:

typedef struct mbyWcmKeyInfoStruct
{
    fm_uint64               key;        // 40b field
    fm_uint64               key_invert; // 40b
#ifdef USE_NEW_CSRS
    // FIXME is this the right number of entries?
    fm_bool                 raw_hits[wcm_tcam_rf_WCM_TCAM__n];
#else
    fm_bool                 raw_hits[MBY_FFU_TCAM_ENTRIES_0];
#endif
    fm_uint32               hit_index;
    fm_bool                 hit_index_valid;

} mbyWcmKeyInfo;

typedef struct mbyClassifierHitInfoStruct
{
    fm_uint32               hit_index;
    fm_bool                 hit_index_valid;

} mbyClassifierHitInfo;

// Functions:

void mbyMatchWildcard
(
    MBY_CGRP_B_IN_REGS,
    mbyClassifierKeys const * const keys,
    fm_byte                   const scenario,
    fm_byte                   const group,
    mbyClassifierActions    * const actions // = output actions
);

#endif /* MBY_WCM_H */
