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
    mbyMapperToClassifier map2cla;
    mbyClassifierToHash   cla2hsh;
    mbyHashToNextHop      hsh2nxt;
    mbyNextHopToMaskGen   nxt2msk;

    // pipeline stages:
    
    Parser     (regs,  mac2par, &par2map);

    Mapper     (regs, &par2map, &map2cla);

    Classifier (regs, &map2cla, &cla2hsh);

    Hash       (regs, &cla2hsh, &hsh2nxt);

    NextHop    (regs, &hsh2nxt, &nxt2msk);
}
