// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_CLASSIFIER_H
#define MBY_CLASSIFIER_H

// Includes:

#include <mby_top_map.h> // header file auto-generated from RDL

#include "fm_types.h"
#include "mby_map2cla.h"
#include "mby_cla2hsh.h"
#include "mby_cgrp_regs.h"
#include "mby_re_keys.h"

// Defines:

// Enums:

// Structs:

typedef mbyMapperToClassifier   Classifier_in_t;
typedef mbyClassifierToHash     Classifier_out_t;

// Function prototype:

void Classifier
(
    mby_ppe_cgrp_a_map    const * const cgrp_a_map,
    mby_ppe_cgrp_b_map    const * const cgrp_b_map,
    mby_shm_map           const * const shm_map,
    mbyMapperToClassifier const * const in,
    mbyClassifierToHash         * const out
);

#endif // MBY_CLASSIFIER_H
