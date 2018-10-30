// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_WCM_H
#define MBY_WCM_H

// Includes:

#include "mby_cgrp_regs.h"

// Defines:

#define MBY_WCM_MAX_ACTIONS_NUM 48

// Enums:

// Structs:

typedef struct mbyWcmKeyInfoStruct
{
    fm_uint64 key;        // 40b field
    fm_uint64 key_invert; // 40b
    fm_bool   raw_hits[wcm_tcam_rf_WCM_TCAM__nd];
    fm_uint32 hit_index;
    fm_bool   hit_index_valid;

} mbyWcmKeyInfo;

typedef struct mbyClassifierHitInfoStruct
{
    fm_uint32 hit_index;
    fm_bool   hit_index_valid;

} mbyClassifierHitInfo;

// Functions:

void mbyMatchWildcard
(
    mby_ppe_cgrp_b_nested_map * const cgrp_b_map,
    mbyClassifierKeys const   * const keys,
    fm_byte                     const profile_id,
    fm_uint32                         actions[MBY_WCM_MAX_ACTIONS_NUM] // = the list of action_entry
);

#endif /* MBY_WCM_H */
