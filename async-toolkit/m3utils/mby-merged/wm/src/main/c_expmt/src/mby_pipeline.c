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

    /* Remove below assignments after HASH module is updated <--REVISIT!!! */
    for (fm_uint i = 0; i < 16; i++)
        hsh2nxt.ARP_HASH[i]         = 0;

    mbyHashKeys hash_keys = { 0 };
    hsh2nxt.HASH_KEYS               = hash_keys;
    hsh2nxt.HASH_ROT_A              = 0;
    hsh2nxt.HASH_ROT_A_PTABLE_INDEX = 0;
    hsh2nxt.HASH_ROT_B              = 0;
    hsh2nxt.HASH_ROT_B_PTABLE_INDEX = 0;
    hsh2nxt.RAW_HASH                = 0;
    hsh2nxt.SV_DROP                 = MBY_SV_MOVE_DROP_RESERVED;

    hsh2nxt.L2_DMAC        = cla2hsh.L2_DMAC;
    hsh2nxt.L2_SMAC        = cla2hsh.L2_SMAC;
    hsh2nxt.FFU_FLAGS      = cla2hsh.FFU_FLAGS;
    hsh2nxt.FFU_ROUTE      = cla2hsh.FFU_ROUTE;
    hsh2nxt.ENCAP          = cla2hsh.ENCAP;
    hsh2nxt.DECAP          = cla2hsh.DECAP;
    hsh2nxt.DMAC_FROM_IPV6 = cla2hsh.DMAC_FROM_IPV6;
    hsh2nxt.DROP_TTL       = cla2hsh.DROP_TTL;
    hsh2nxt.L2_IDOMAIN     = cla2hsh.L2_IDOMAIN;
    hsh2nxt.L3_IDOMAIN     = cla2hsh.L3_IDOMAIN;
    hsh2nxt.L2_IVID1       = cla2hsh.L2_IVID1;
    hsh2nxt.LEARN_MODE     = cla2hsh.LEARN_MODE;
    hsh2nxt.PARITY_ERROR   = cla2hsh.PARITY_ERROR;
    hsh2nxt.PARSER_ERROR   = cla2hsh.PARSER_ERROR;
    hsh2nxt.PARSER_INFO    = cla2hsh.PARSER_INFO;
    hsh2nxt.PA_L3LEN_ERR   = cla2hsh.PA_L3LEN_ERR;
    hsh2nxt.RX_DATA        = cla2hsh.RX_DATA;
    hsh2nxt.RX_LENGTH      = cla2hsh.RX_LENGTH;
    hsh2nxt.RX_PORT        = cla2hsh.RX_PORT;
    hsh2nxt.TRAFFIC_CLASS  = cla2hsh.TRAFFIC_CLASS;
    hsh2nxt.TRAP_IGMP      = cla2hsh.TRAP_IGMP;

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
