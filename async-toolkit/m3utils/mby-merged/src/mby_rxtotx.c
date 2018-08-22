// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_rxstats.h"
#include "mby_modifier.h"
#include "mby_rxtotx.h"

void RxToTx
(
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyRxStatsToRxOut * const in,
          mbyTxInToModifier * const out
)
{
#if 0 // coding style example, not actual content <-- REVISIT!!!
    // Read inputs:
    fm_uint32 rx_port = in->RX_PORT;

    // Write outputs:
    out->TX_PORT = rx_port;
#endif
}
