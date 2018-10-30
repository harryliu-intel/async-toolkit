// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_EXACTMATCH_H
#define MBY_EXACTMATCH_H

// Includes:

#include "mby_cgrp_regs.h"

// Defines:

#define MBY_EM_MAX_ACTIONS_NUM 4
#define MBY_CGRP_MAX_HASH_ENTRY_SIZE      64
#define MBY_CGRP_HASH_CAM_ETY_7_BITS_31_0  0
#define MBY_CGRP_HASH_CAM_ETY_7_BITS_63_32 1
#define MBY_CGRP_HASH_CAM_ETY_6_BITS_31_0  2
#define MBY_CGRP_HASH_CAM_ETY_6_BITS_63_32 3

// Enums:

// Structs:

// Functions:

void mbyMatchExact // i.e. look up EM hash
(
    em_hash_lookup_r        * const em_hash_lookup_reg,
    mby_ppe_cgrp_em_map     * const cgrp_em_map,
    mby_shm_map             * const shm_map,
    mbyClassifierKeys const * const keys,
    fm_byte                   const profile,
    fm_byte                   const group,
    fm_uint32                       actions[MBY_EM_MAX_ACTIONS_NUM] // = the list of action entries to action resolution
);

#endif /* MBY_EXACTMATCH_H */
