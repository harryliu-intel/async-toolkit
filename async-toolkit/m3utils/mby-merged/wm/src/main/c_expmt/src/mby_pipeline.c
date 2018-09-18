// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_pipeline.h"

void RxPipeline
(
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
    mby_ppe_rx_top_map      * const rx_tmap,
    mbyRxMacToParser  const * const mac2par,
    mbyRxStatsToRxOut       * const rxs2rxo
)
{
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
    Parser     (regs,           mac2par, &par2map);

    Mapper     (regs,          &par2map, &map2cla);

    Classifier (regs, rx_tmap, &map2cla, &cla2hsh);

    Hash       (regs,          &cla2hsh, &hsh2nxt);

    NextHop    (regs,          &hsh2nxt, &nxt2msk);

    MaskGen    (regs,          &nxt2msk, &msk2trg);

    Triggers   (regs,          &msk2trg, &trg2cgm);

    CongMgmt   (regs,          &trg2cgm, &cgm2rxs);

    RxStats    (regs,          &cgm2rxs,  rxs2rxo);
}

void TxPipeline
(
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
    mby_ppe_tx_top_map      * const tx_tmap,
    mbyTxInToModifier const * const txi2mod,
    mbyTxStatsToTxMac       * const txs2mac
)
{
    // Intermediate structs:
    mbyModifierToTxStats mod2txs;

    // TX pipeline stages:
    Modifier   (regs,           txi2mod, &mod2txs);

    TxStats    (regs,          &mod2txs,  txs2mac);
}
