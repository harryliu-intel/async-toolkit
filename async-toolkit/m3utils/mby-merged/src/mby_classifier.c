// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_mapper.h"
#include "mby_classifier.h"

void Classifier
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyMapperToClassifier * const in,
          mbyClassifierToHash   * const out
)
{
    // Init to defaults from the Mapper:
    fm_byte               ffu_scenario = in->FFU_SCENARIO;
    mbyClassifierKeys     ffu_keys     = in->FFU_KEYS;
    mbyClassifierActions  ffu_actions  = in->FFU_ACTIONS;

    // Update scenario based on scenario action:
    for (fm_uint s = MBY_FFU_ACTION_SCENARIO0, i = 0; s <= MBY_FFU_ACTION_SCENARIO5; s++, i++) {
        if (in->FFU_ACTIONS.act1[s].prec != 0) {    
            fm_byte val = in->FFU_ACTIONS.act1[s].val & 1;
            FM_SET_UNNAMED_FIELD(ffu_scenario, i, 1, val);
        }     
    }                

    // Assign outputs:
    out->FFU_KEYS     = ffu_keys;
    out->FFU_ACTIONS  = ffu_actions;
    out->FFU_SCENARIO = ffu_scenario;
}
