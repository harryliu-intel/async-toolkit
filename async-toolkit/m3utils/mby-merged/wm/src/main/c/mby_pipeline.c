// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#include "mby_pipeline.h"

void RxPipeline
(
    mby_ppe_rx_top_map       const * const rx_top_map,
    mby_ppe_rx_top_map__addr const * const rx_top_map_w,
    mby_shm_map              const * const shm_map,
    mbyRxMacToParser         const * const mac2par,
    mbyRxStatsToRxOut              * const rxs2rxo
)
{
    // Register map structs:
    mby_ppe_parser_map                const * const parser_map            = &(rx_top_map->parser);
    mby_ppe_mapper_map                const * const mapper_map            = &(rx_top_map->mapper);
    mby_ppe_cgrp_a_map                const * const cgrp_a_map            = &(rx_top_map->cgrp_a);
    mby_ppe_cgrp_b_map                const * const cgrp_b_map            = &(rx_top_map->cgrp_b);
    mby_ppe_entropy_map               const * const entropy_map           = &(rx_top_map->entropy);
    mby_ppe_nexthop_map               const * const nexthop_map           = &(rx_top_map->nexthop);
    mby_ppe_nexthop_map__addr         const * const nexthop_map_w         = &(rx_top_map_w->nexthop);
    mby_ppe_fwd_misc_map              const * const fwd_misc_map          = &(rx_top_map->fwd_misc);
    mby_ppe_fwd_misc_map__addr        const * const fwd_misc_map_w        = &(rx_top_map_w->fwd_misc);
    mby_ppe_mst_glort_map             const * const mst_glort_map         = &(rx_top_map->mst_glort);
    mby_ppe_policers_map              const * const policers_map          = &(rx_top_map->policers);
    mby_ppe_trig_apply_map            const * const trig_apply_map        = &(rx_top_map->trig_apply);
    mby_ppe_trig_apply_map__addr      const * const trig_apply_map_w      = &(rx_top_map_w->trig_apply);
    mby_ppe_trig_apply_misc_map       const * const trig_apply_misc_map   = &(rx_top_map->trig_apply_misc);
    mby_ppe_trig_apply_misc_map__addr const * const trig_apply_misc_map_w = &(rx_top_map_w->trig_apply_misc);
    mby_ppe_trig_usage_map            const * const trig_usage_map        = &(rx_top_map->trig_usage);
    mby_ppe_cm_apply_map              const * const cm_apply_map          = &(rx_top_map->cm_apply);
    mby_ppe_cm_usage_map              const * const cm_usage_map          = &(rx_top_map->cm_usage);
    mby_ppe_rx_stats_map              const * const stats_map             = &(rx_top_map->stats);
    mby_ppe_rx_stats_map__addr        const * const stats_map_w           = &(rx_top_map_w->stats);

    // Intermediate structs:
    mbyParserToMapper     par2map = { 0 };
    mbyMapperToClassifier map2cla = { 0 };
    mbyClassifierToHash   cla2hsh = { 0 };
    mbyHashToNextHop      hsh2nxt = { 0 };
    mbyNextHopToMaskGen   nxt2msk = { 0 };
    mbyMaskGenToTriggers  msk2trg = { 0 };
    mbyTriggersToCongMgmt trg2cgm = { 0 };
    mbyCongMgmtToRxStats  cgm2rxs = { 0 };

    // RX pipeline stages:
    Parser     (parser_map,           mac2par, &par2map);

    Mapper     (mapper_map,          &par2map, &map2cla);

    Classifier (cgrp_a_map,
                cgrp_b_map,
                shm_map,             &map2cla, &cla2hsh);

    Hash       (entropy_map,         &cla2hsh, &hsh2nxt);

    NextHop    (nexthop_map,
                nexthop_map_w,       &hsh2nxt, &nxt2msk);

    MaskGen    (fwd_misc_map,
                mst_glort_map,
                cm_apply_map,        &nxt2msk, &msk2trg);

    Triggers   (trig_apply_map,
                trig_apply_map_w,
                trig_apply_misc_map,
                trig_apply_misc_map_w,
                fwd_misc_map,
                fwd_misc_map_w,
                cm_apply_map,
                mapper_map,          &msk2trg, &trg2cgm);

    CongMgmt   (cm_apply_map,
                cm_usage_map,        &trg2cgm, &cgm2rxs);

    RxStats    (stats_map,
                stats_map_w,         &cgm2rxs,  rxs2rxo);
}

void TxPipeline
(
    mby_ppe_tx_top_map       const * const tx_top_map,
    mby_ppe_tx_top_map__addr const * const tx_top_map_w,
    mby_shm_map              const * const shm_map,
    varchar_t                const *       rx_data,
    mbyTxInToModifier        const * const txi2mod,
    mbyTxStatsToTxMac              * const txs2mac,
    varchar_builder_t              * const tx_data_builder
)
{
    // Register map structs:
    mby_ppe_modify_map const * const modify_map = &(tx_top_map->modify);

    // Intermediate struct.
    mbyModifierToTxStats mod2txs;

    // TX pipeline stages:
    Modifier(modify_map, shm_map, rx_data, txi2mod, &mod2txs, tx_data_builder);

    // Setting TX port  will be fixed with tx stats <--REVISIT!!!
    txs2mac->TX_PORT   = mod2txs.TX_PORT;

//  TxStats    (modify_map,    &mod2txs,  txs2mac);
}
