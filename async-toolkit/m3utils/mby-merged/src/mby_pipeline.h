// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_PIPELINE_H
#define MBY_PIPELINE_H

#include "mby_common.h"
#include "mby_parser.h"
#include "mby_mapper.h"
#include "mby_classifier.h"
#include "mby_hash.h"

// Function prototypes:

void Parser
(
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyMacToParser    * const in,
          mbyParserToMapper * const out
);

void Mapper
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyParserToMapper     * const in,
          mbyMapperToClassifier * const out
);

void Classifier
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyMapperToClassifier * const in,
          mbyClassifierToHash   * const out
);

void Hash
(
    fm_uint32                         regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyClassifierToHash * const in,
          mbyHashToNextHop    * const out
);

#endif // MBY_PIPELINE_H
