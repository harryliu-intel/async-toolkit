// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_PIPELINE_H
#define MBY_PIPELINE_H

#ifdef USE_NEW_CSRS
#include <mby_top_map.h>
#endif
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
#include "mby_modifier.h"
#include "mby_txstats.h"

// Function prototypes:

void RxPipeline
(
#ifdef USE_NEW_CSRS
    mby_ppe_rx_top_map      * const rx_top_map,
    mby_shm_map             * const shm_map,
#else
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyRxMacToParser  const * const mac2par,
    mbyRxStatsToRxOut       * const rxs2rxo
);

void TxPipeline
(
#ifdef USE_NEW_CSRS
    mby_ppe_tx_top_map      * const tx_top_map,
#else
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyTxInToModifier const * const txi2mod,
    mbyTxStatsToTxMac       * const txs2mac
);

// TODO all the following should be moved to the header files corresponding to
// the C file where the function is actually implemented.
void Parser
(
#ifdef USE_NEW_CSRS
    mby_ppe_parser_map          * const parser_map,
#else
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyRxMacToParser const      * const in,
    mbyParserToMapper           * const out
);

void Mapper
(
#ifdef USE_NEW_CSRS
    mby_ppe_mapper_map          * const mapper_map,
#else
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyParserToMapper const     * const in,
    mbyMapperToClassifier       * const out
);

void Classifier
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_a_map          * const cgrp_a_map,
    mby_ppe_cgrp_b_map          * const cgrp_b_map,
    mby_ppe_entropy_map         * const entropy_map,
    mby_shm_map                 * const shm_map, // shared memory (forwarding tables)
#else
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyMapperToClassifier const * const in,
    mbyClassifierToHash         * const out
);

void Hash
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    mbyClassifierToHash const   * const in,
    mbyHashToNextHop            * const out
);

void NextHop
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    mbyHashToNextHop const      * const in,
    mbyNextHopToMaskGen         * const out
);

void MaskGen
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    mbyNextHopToMaskGen const   * const in,
    mbyMaskGenToTriggers        * const out
);

void Triggers
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    mbyMaskGenToTriggers const  * const in,
    mbyTriggersToCongMgmt       * const out
);

void CongMgmt
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    mbyTriggersToCongMgmt const * const in,
    mbyCongMgmtToRxStats        * const out
);

void RxStats
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    mbyCongMgmtToRxStats const  * const in,
    mbyRxStatsToRxOut           * const out
);

void Modifier
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    mbyTxInToModifier const     * const in,
    mbyModifierToTxStats        * const out
);

void TxStats
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    mbyModifierToTxStats const  * const in,
    mbyTxStatsToTxMac           * const out
);

#endif
