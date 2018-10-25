// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_EXACTMATCH_H
#define MBY_EXACTMATCH_H

// Includes:

#include "mby_clsfr_regs.h"

// Defines:

#define MBY_EM_MAX_ACTIONS_NUM 4

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
    fm_uint32               * const actions // = the list of action entries to action resolution
);

#endif /* MBY_EXACTMATCH_H */
