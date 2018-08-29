// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_pipeline.h"

void Pipeline
(
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyRxMacToParser  * const mac2par,
          mbyTxStatsToTxMac * const txs2mac
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
    mbyRxStatsToRxOut     rxs2rxo;
    mbyTxInToModifier     txi2mod;
    mbyModifierToTxStats  mod2txs;

    // RX pipeline stages:

    Parser     (regs,  mac2par, &par2map);

    Mapper     (regs, &par2map, &map2cla);

    Classifier (regs, &map2cla, &cla2hsh);

    Hash       (regs, &cla2hsh, &hsh2nxt);

    NextHop    (regs, &hsh2nxt, &nxt2msk);

    MaskGen    (regs, &nxt2msk, &msk2trg);

    Triggers   (regs, &msk2trg, &trg2cgm);

    CongMgmt   (regs, &trg2cgm, &cgm2rxs);

    RxStats    (regs, &cgm2rxs, &rxs2rxo);

    // RX to TX adapter:

    RxToTx     (regs, &rxs2rxo, &txi2mod);

    // TX pipeline stages:
    
    Modifier   (regs, &txi2mod, &mod2txs);

    TxStats    (regs, &mod2txs,  txs2mac);
}
