// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_EXACTMATCH_H
#define MBY_EXACTMATCH_H

// Includes:

#include "mby_clsfr_regs.h"

// Defines:

// FIXME random number - needs to be reviewed by architect
#define MBY_EM_A_MAX_ACTIONS_NUM 48
#define MBY_EM_B_MAX_ACTIONS_NUM 48

// Enums:

// Structs:

// Functions:

void mbyMatchExact // i.e. look up EM hash
(
    mby_ppe_cgrp_a_map      * const cgrp_a_map,
    mby_ppe_cgrp_b_map      * const cgrp_b_map,
    mbyClassifierKeys const * const keys,
    fm_byte                   const scenario,
    fm_byte                   const group,
    mbyClassifierActions    * const actions // = output actions
);

#endif /* MBY_EXACTMATCH_H */
