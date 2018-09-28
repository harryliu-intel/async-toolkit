// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_EXACTMATCH_H
#define MBY_EXACTMATCH_H

// Includes:

#include "mby_classifier_regs.h"

// Defines:

// Enums:

// Structs:

// Functions:

void mbyMatchExact // i.e. look up EM hash
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_a_map      * const cgrp_a_map,
    mby_ppe_cgrp_b_map      * const cgrp_b_map,
#else
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyClassifierKeys const * const keys,
    fm_byte                   const scenario,
    fm_byte                   const group,
    mbyClassifierActions    * const actions // = output actions
);

#endif /* MBY_EXACTMATCH_H */
