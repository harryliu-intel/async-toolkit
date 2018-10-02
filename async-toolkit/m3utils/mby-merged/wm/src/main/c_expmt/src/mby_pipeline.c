// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_pipeline.h"

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
)
{
#ifdef USE_NEW_CSRS
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
#endif

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
#ifdef USE_NEW_CSRS
    Parser     (parser_map,    mac2par, &par2map);

    Mapper     (mapper_map,   &par2map, &map2cla);

    Classifier (cgrp_a_map,
                cgrp_b_map,
                entropy_map,
                shm_map,             &map2cla, &cla2hsh);

//  Hash       (                     &cla2hsh, &hsh2nxt);

    NextHop    (nexthop_map,         &hsh2nxt, &nxt2msk);

//  MaskGen    (                     &nxt2msk, &msk2trg);

    Triggers   (trig_apply_map,
                trig_apply_misc_map,
                trig_usage_map,      &msk2trg, &trg2cgm);

    CongMgmt   (cm_apply_map,
                cm_usage_map,        &trg2cgm, &cgm2rxs);

    RxStats    (stats_map,           &cgm2rxs,  rxs2rxo);
#else
    Parser     (regs,  mac2par, &par2map);

    Mapper     (regs, &par2map, &map2cla);

    Classifier (regs, &map2cla, &cla2hsh);

    Hash       (regs, &cla2hsh, &hsh2nxt);

    NextHop    (regs, &hsh2nxt, &nxt2msk);

    MaskGen    (regs, &nxt2msk, &msk2trg);

    Triggers   (regs, &msk2trg, &trg2cgm);

    CongMgmt   (regs, &trg2cgm, &cgm2rxs);

    RxStats    (regs, &cgm2rxs,  rxs2rxo);
#endif
}

void TxPipeline
(
#ifdef USE_NEW_CSRS
    mby_ppe_tx_top_map      * const tx_top_map,
#else
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
#endif
    mbyTxInToModifier const * const txi2mod,
    mbyTxStatsToTxMac       * const txs2mac
)
{
    // Register map structs:
#ifdef USE_NEW_CSRS
    mby_ppe_modify_map      * const modify_map = &(tx_top_map->modify);
#endif

    // Intermediate structs:
    mbyModifierToTxStats mod2txs;

    // TX pipeline stages:
#ifdef USE_NEW_CSRS
//  Modifier   (modify_map,     txi2mod, &mod2txs);

//  TxStats    (modify_map,    &mod2txs,  txs2mac);
#else
    Modifier   (regs,           txi2mod, &mod2txs);

    TxStats    (regs,          &mod2txs,  txs2mac);
#endif
}
