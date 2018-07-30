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
    fm_byte group = 0;
    
    // Update scenario based on scenario action:
    for (fm_uint s = MBY_FFU_ACTION_SCENARIO0, i = 0; s <= MBY_FFU_ACTION_SCENARIO5; s++, i++) {
        if (in->FFU_ACTIONS.act1[s].prec != 0) {    
            fm_byte val = in->FFU_ACTIONS.act1[s].val & 1;
            FM_SET_UNNAMED_FIELD(out->FFU_SCENARIO, i, 1, val);
        }     
    }                

    // Assign FFU Group outputs:
    out->FFU_GRP_KEYS    [group] = in->FFU_KEYS;
    out->FFU_GRP_ACTIONS [group] = in->FFU_ACTIONS;
    out->FFU_GRP_SCENARIO[group] = in->FFU_SCENARIO;
}
