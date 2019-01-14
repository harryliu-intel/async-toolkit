// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_TRIGGERS_H
#define MBY_TRIGGERS_H

// Includes:
#include <mby_top_map.h> // header file auto-generated from RDL

#include "mby_msk2trg.h"
#include "mby_trg2cgm.h"

// Defines:

#define MBY_TRIGGERS_COUNT         96
#define MBY_MA_TCN_FIFO_CAPACITY  511

// Structs:

// Function prototype:

void Triggers
(
    mby_ppe_trig_apply_map            const * const trig_apply_map,
    mby_ppe_trig_apply_map__addr      const * const trig_apply_map_w,
    mby_ppe_trig_apply_misc_map       const * const trig_apply_misc_map,
    mby_ppe_trig_apply_misc_map__addr const * const trig_apply_misc_map_w,
    mby_ppe_fwd_misc_map              const * const fwd_misc_map,
    mby_ppe_fwd_misc_map__addr        const * const fwd_misc_map_w,
    mby_ppe_cm_apply_map              const * const cm_apply_map,
    mby_ppe_mapper_map                const * const mapper_map,
    mbyMaskGenToTriggers              const * const in,
    mbyTriggersToCongMgmt                   * const out
);

#endif // MBY_TRIGGERS_H
