// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_pipeline.h"

void Pipeline
(
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyMacToParser    * const mac2par
)
{
    // intermediate structs:

    mbyParserToMapper     par2map;
    mbyParserToModifier   par2mod;
    mbyMapperToClassifier map2cla;
    mbyClassifierToHash   cla2hsh;
    
    // pipeline stages:
    
    Parser     (regs, mac2par, &par2map);

    Mapper     (regs, &par2map, &map2cla, &par2mod);

    Classifier (regs, &map2cla, &cla2hsh);
}
