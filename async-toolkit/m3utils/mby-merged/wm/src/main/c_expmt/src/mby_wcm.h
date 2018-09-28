// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_WCM_H
#define MBY_WCM_H

// Includes:

#include "mby_classifier_regs.h"

// Defines:

// Enums:

// Structs:

typedef struct mbyLookupInfoStruct
{
    fm_uint64               key;       // 40b field
    fm_uint64               keyInvert; // 40b
#ifdef USE_NEW_CSRS
    // FIXME is this the right number of entries?
    fm_bool                 rawHits[wcm_tcam_rf_WCM_TCAM__n];
#else
    fm_bool                 rawHits[MBY_FFU_TCAM_ENTRIES_0];
#endif
    fm_uint32               hitIndex;
    fm_bool                 hitIndexValid;

} mbyLookupInfo;

typedef struct mbyClassifierHitInfoStruct
{
    fm_uint32               hitIndex;
    fm_bool                 hitIndexValid;

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
