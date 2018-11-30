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
#include "mby_modifier.h"
#include "mby_txstats.h"

// Function prototypes:

void RxPipeline
(
    mby_ppe_rx_top_map       * const rx_top_map,
    mby_shm_map              * const shm_map,
    mbyRxMacToParser   const * const mac2par,
    mbyRxStatsToRxOut        * const rxs2rxo
);

void TxPipeline
(
    mby_ppe_tx_top_map       * const tx_top_map,
    mby_shm_map              * const shm_map,
    mbyTxInToModifier  const * const txi2mod,
    mbyTxStatsToTxMac        * const txs2mac,
    fm_int                           max_pkt_size
);

void Parser
(
    mby_ppe_parser_map    const * const parser_map,
    mbyRxMacToParser      const * const in,
    mbyParserToMapper           * const out
);

void Mapper
(
    mby_ppe_mapper_map          * const mapper_map,
    mbyParserToMapper     const * const in,
    mbyMapperToClassifier       * const out
);

void Classifier
(
    mby_ppe_cgrp_a_map          * const cgrp_a_map,
    mby_ppe_cgrp_b_map          * const cgrp_b_map,
    mby_shm_map                 * const shm_map, // shared memory (forwarding tables)
    mbyMapperToClassifier const * const in,
    mbyClassifierToHash         * const out
);

void Hash
(
    mby_ppe_entropy_map   const * const entropy_map,
    mbyClassifierToHash   const * const in,
    mbyHashToNextHop            * const out
);

void NextHop
(
    mby_ppe_nexthop_map         * const nexthop,
    mbyHashToNextHop      const * const in,
    mbyNextHopToMaskGen         * const out
);

void MaskGen
(
    mby_ppe_fwd_misc_map        * const fwd_misc,
    mby_ppe_mst_glort_map       * const glort_map,
    mby_ppe_cm_apply_map        * const cm_apply,
    mbyNextHopToMaskGen   const * const in,
    mbyMaskGenToTriggers        * const out
);

void Triggers
(
    mby_ppe_trig_apply_map      * const trig_apply_map,
    mby_ppe_trig_apply_misc_map * const trig_apply_misc_map,
    mby_ppe_fwd_misc_map        * const fwd_misc_map,
    mby_ppe_mapper_map          * const mapper_map,
    mbyMaskGenToTriggers  const * const in,
    mbyTriggersToCongMgmt       * const out
);

void CongMgmt
(
    mby_ppe_cm_apply_map        * const cm_apply_map,
    mby_ppe_cm_usage_map        * const cm_usage_map,
    mbyTriggersToCongMgmt const * const in,
    mbyCongMgmtToRxStats        * const out
);

void RxStats
(
    mby_ppe_rx_stats_map        * const stats_map,
    mbyCongMgmtToRxStats  const * const in,
    mbyRxStatsToRxOut           * const out
);

void Modifier
(
    mby_ppe_modify_map          * const mod_map,
    mby_shm_map                 * const shm_map,
    mbyTxInToModifier     const * const in,
    mbyModifierToTxStats        * const out,
    fm_int                              max_pkt_size
);

void TxStats
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    mbyModifierToTxStats  const * const in,
    mbyTxStatsToTxMac           * const out
);

#endif
