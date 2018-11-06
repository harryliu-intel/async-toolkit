// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_pipeline.h"

void RxPipeline
(
    mby_ppe_rx_top_map      * const rx_top_map,
    mby_shm_map             * const shm_map,
    mbyRxMacToParser  const * const mac2par,
    mbyRxStatsToRxOut       * const rxs2rxo
)
{
    // Register map structs:
    mby_ppe_parser_map          * const parser_map          = &(rx_top_map->parser);
    mby_ppe_mapper_map          * const mapper_map          = &(rx_top_map->mapper);
    mby_ppe_cgrp_a_map          * const cgrp_a_map          = &(rx_top_map->cgrp_a);
    mby_ppe_cgrp_b_map          * const cgrp_b_map          = &(rx_top_map->cgrp_b);
    mby_ppe_entropy_map         * const entropy_map         = &(rx_top_map->entropy);
    mby_ppe_nexthop_map         * const nexthop_map         = &(rx_top_map->nexthop);
    mby_ppe_fwd_misc_map        * const fwd_misc_map        = &(rx_top_map->fwd_misc);
    mby_ppe_mst_glort_map       * const mst_glort_map       = &(rx_top_map->mst_glort);
    mby_ppe_policers_map        * const policers_map        = &(rx_top_map->policers);
    mby_ppe_trig_apply_map      * const trig_apply_map      = &(rx_top_map->trig_apply);
    mby_ppe_trig_apply_misc_map * const trig_apply_misc_map = &(rx_top_map->trig_apply_misc);
    mby_ppe_trig_usage_map      * const trig_usage_map      = &(rx_top_map->trig_usage);
    mby_ppe_cm_apply_map        * const cm_apply_map        = &(rx_top_map->cm_apply);
    mby_ppe_cm_usage_map        * const cm_usage_map        = &(rx_top_map->cm_usage);
    mby_ppe_rx_stats_map        * const stats_map           = &(rx_top_map->stats);

    // Intermediate structs:
    mbyParserToMapper     par2map;
    mbyMapperToClassifier map2cla;
    mbyClassifierToHash   cla2hsh;
    mbyHashToNextHop      hsh2nxt;
    mbyNextHopToMaskGen   nxt2msk;
    mbyMaskGenToTriggers  msk2trg;
    mbyTriggersToCongMgmt trg2cgm;
    mbyCongMgmtToRxStats  cgm2rxs;

    // RX pipeline stages:
    Parser     (parser_map,    mac2par, &par2map);

    Mapper     (mapper_map,   &par2map, &map2cla);

    Classifier (cgrp_a_map,
                cgrp_b_map,
                shm_map,             &map2cla, &cla2hsh);

    Hash       (entropy_map, &(nexthop_map->FWD_HASHING_CFG), &cla2hsh, &hsh2nxt);

    NextHop    (nexthop_map,         &hsh2nxt, &nxt2msk);

    MaskGen    (fwd_misc_map,
                mst_glort_map,
                cm_apply_map,        &nxt2msk, &msk2trg);

    Triggers   (trig_apply_map,
                trig_apply_misc_map,
                trig_usage_map,      &msk2trg, &trg2cgm);

    CongMgmt   (cm_apply_map,
                cm_usage_map,        &trg2cgm, &cgm2rxs);

    RxStats    (stats_map,           &cgm2rxs,  rxs2rxo);
}

void TxPipeline
(
    mby_ppe_tx_top_map      * const tx_top_map,
    mbyTxInToModifier const * const txi2mod,
    mbyTxStatsToTxMac       * const txs2mac
)
{
    // Register map structs:
    mby_ppe_modify_map      * const modify_map = &(tx_top_map->modify);

    // Intermediate structs:
    mbyModifierToTxStats mod2txs;

    // TX pipeline stages:
//  Modifier   (modify_map,     txi2mod, &mod2txs);

//  TxStats    (modify_map,    &mod2txs,  txs2mac);
}
