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
    mby_ppe_cgrp_a_map      * const cgrp_a_map,
    mby_ppe_cgrp_b_map      * const cgrp_b_map,
    mby_shm_map             * const shm_map,
    mbyClassifierKeys const * const keys,
    fm_byte                   const profile,
    fm_byte                   const group,
    fm_uint32               * const actions // = the list of action entries to action resolution
);

#endif /* MBY_EXACTMATCH_H */
