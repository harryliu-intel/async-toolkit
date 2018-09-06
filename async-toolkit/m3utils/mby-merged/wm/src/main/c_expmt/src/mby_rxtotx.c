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
    out->DROP_TTL          = in->DROP_TTL;
    out->ECN               = in->ECN;
    out->EDGLORT           = in->EDGLORT;
    out->IS_TIMEOUT        = in->IS_TIMEOUT;
    out->L2_DMAC           = in->L2_DMAC;
    out->L2_EVID1          = in->L2_EVID1;
    out->MARK_ROUTED       = in->MARK_ROUTED;
    out->MIRTYP            = in->MIRTYP;
    out->MOD_IDX           = in->MOD_IDX;
    out->NO_MODIFY         = in->NO_MODIFY;
    out->OOM               = in->OOM;
    out->PARSER_INFO       = in->PARSER_INFO;
    out->PM_ERR            = in->PM_ERR;
    out->PM_ERR_NONSOP     = in->PM_ERR_NONSOP;
    out->QOS_L3_DSCP       = in->QOS_L3_DSCP;
    out->RX_DATA           = in->RX_DATA;
    out->RX_LENGTH         = in->RX_LENGTH;
    out->SAF_ERROR         = in->SAF_ERROR;
    out->TAIL_CSUM_LEN     = in->TAIL_CSUM_LEN;
    out->TX_DROP           = in->TX_DROP;
    out->TX_LENGTH         = in->TX_LENGTH;
    out->TX_PORT           = in->TX_PORT;
    out->TX_TAG            = in->TX_TAG;
    out->XCAST             = in->XCAST;
}
