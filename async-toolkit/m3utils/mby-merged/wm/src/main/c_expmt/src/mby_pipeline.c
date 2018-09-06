// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_pipeline.h"

void RxPipeline
(
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyRxMacToParser  * const mac2par,
          mbyRxStatsToRxOut * const rxs2rxo
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
    Parser     (regs,  mac2par, &par2map);

    Mapper     (regs, &par2map, &map2cla);

    Classifier (regs, &map2cla, &cla2hsh);

    Hash       (regs, &cla2hsh, &hsh2nxt);

    NextHop    (regs, &hsh2nxt, &nxt2msk);

    MaskGen    (regs, &nxt2msk, &msk2trg);

    Triggers   (regs, &msk2trg, &trg2cgm);

    CongMgmt   (regs, &trg2cgm, &cgm2rxs);

    RxStats    (regs, &cgm2rxs,  rxs2rxo);
}

void TxPipeline
(
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyTxInToModifier * const txi2mod,
          mbyTxStatsToTxMac * const txs2mac
)
{
    // Intermediate structs:
    mbyModifierToTxStats mod2txs;

    // TX pipeline stages:
    Modifier   (regs,  txi2mod, &mod2txs);

    TxStats    (regs, &mod2txs,  txs2mac);
}

// TODO remove if no longer required
void Pipeline
(
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyRxMacToParser  * const mac2par,
          mbyTxStatsToTxMac * const txs2mac
)
{
    // Intermediate structs:
    mbyRxStatsToRxOut rxs2rxo;
    mbyTxInToModifier txi2mod;

    // Pipeline stages:
    RxPipeline (regs,  mac2par, &rxs2rxo);

    RxToTx     (regs, &rxs2rxo, &txi2mod);

    TxPipeline (regs, &txi2mod,  txs2mac);
}
