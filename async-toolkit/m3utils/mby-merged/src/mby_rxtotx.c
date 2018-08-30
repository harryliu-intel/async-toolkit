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
    // For now, this stage only copies inputs to outputs,
    // but in the future it could do more than this. TBD.

    // Pass thru:
    out->PARSER_INFO       = in->PARSER_INFO;
    out->NO_MODIFY         = in->NO_MODIFY;
    out->RX_LENGTH         = in->RX_LENGTH;
    out->RX_DATA           = in->RX_DATA;
    out->TX_PORT           = in->TX_PORT;
    out->TX_DROP           = in->TX_DROP;
    out->TX_LENGTH         = in->TX_LENGTH;
    out->TX_TAG            = in->TX_TAG;
    out->TX_STATS_LAST_LEN = in->TX_STATS_LAST_LEN;
    out->L2_EVID1          = in->L2_EVID1;
    out->EDGLORT           = in->EDGLORT;
    out->MIRTYP            = in->MIRTYP;
    out->QOS_L3_DSCP       = in->QOS_L3_DSCP;
    out->ECN               = in->ECN;
    out->MARK_ROUTED       = in->MARK_ROUTED;
    out->MOD_IDX           = in->MOD_IDX;
    out->TAIL_CSUM_LEN     = in->TAIL_CSUM_LEN;
    out->XCAST             = in->XCAST;
    out->DROP_TTL          = in->DROP_TTL;
    out->IS_TIMEOUT        = in->IS_TIMEOUT;
    out->OOM               = in->OOM;
    out->PM_ERR_NONSOP     = in->PM_ERR_NONSOP;
    out->PM_ERR            = in->PM_ERR;
    out->SAF_ERROR         = in->SAF_ERROR;
    out->L2_DMAC           = in->L2_DMAC;
}
