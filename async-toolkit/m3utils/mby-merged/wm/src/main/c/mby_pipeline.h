/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_PIPELINE_H
#define MBY_PIPELINE_H

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
#include "varchar.h"

// Function prototypes:

void RxPipeline
(
    mby_ppe_rx_top_map       const * const rx_top_map,
    mby_ppe_rx_top_map__addr const * const rx_top_map_w,
    mby_shm_map              const * const shm_map,
    mbyRxMacToParser         const * const mac2par,
    mbyRxStatsToRxOut              * const rxs2rxo
);

void TxPipeline
(
    mby_ppe_tx_top_map       const * const tx_top_map,
    mby_ppe_tx_top_map__addr const * const tx_top_map_w,
    mby_shm_map              const * const shm_map,
    varchar_t                const *       rx_data,
    mbyTxInToModifier        const * const txi2mod,
    mbyTxStatsToTxMac              * const txs2mac,
    varchar_builder_t              * const tx_data_builder
);

#endif
