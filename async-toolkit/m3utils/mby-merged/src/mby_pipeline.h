// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_PIPELINE_H
#define MBY_PIPELINE_H

#include "mby_common.h"
#include "mby_parser.h"
#include "mby_mapper.h"
#include "mby_classifier.h"
#include "mby_hash.h"
#include "mby_nexthop.h"
#include "mby_maskgen.h"
#include "mby_triggers.h"
#include "mby_congmgmt.h"
#include "mby_rxstats.h"
#include "mby_rxtotx.h"
#include "mby_modifier.h"
#include "mby_txstats.h"

// Function prototypes:

void Pipeline
(
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyMacToParser    * const mac2par,
          mbyTxStatsToTxOut * const txs2txo
);

// TODO all the following should be moved to the header files corresponding to
// the C file where the function is actually implemented.
void Parser
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyMacToParser        * const in,
          mbyParserToMapper     * const out
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
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyClassifierToHash   * const in,
          mbyHashToNextHop      * const out
);

void NextHop
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyHashToNextHop      * const in,
          mbyNextHopToMaskGen   * const out
);

void MaskGen
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyNextHopToMaskGen   * const in,
          mbyMaskGenToTriggers  * const out
);

void Triggers
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyMaskGenToTriggers  * const in,
          mbyTriggersToCongMgmt * const out
);

void CongMgmt
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyTriggersToCongMgmt * const in,
          mbyCongMgmtToRxStats  * const out
);

void RxStats
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyCongMgmtToRxStats  * const in,
          mbyRxStatsToRxOut     * const out
);

void RxToTx
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyRxStatsToRxOut     * const in,
          mbyTxInToModifier     * const out
);

void Modifier
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyTxInToModifier     * const in,
          mbyModifierToTxStats  * const out
);

void TxStats
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyModifierToTxStats  * const in,
          mbyTxStatsToTxOut     * const out
);

#endif
