// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_maskgen.h"
#include "mby_triggers.h"

void Triggers
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyMaskGenToTriggers  * const in,
          mbyTriggersToCongMgmt * const out
)
{
    // Note: this stage is empty for now. Will be coded up in the next sprint. <-- REVISIT!!!

    // Pass thru:
    out->IS_IPV4           = in->IS_IPV4;
    out->IS_IPV6           = in->IS_IPV6;
    out->L2_DMAC           = in->L2_DMAC;
    out->L2_IVLAN1_CNT     = in->L2_IVLAN1_CNT;
    out->FNMASK            = in->FNMASK;
    out->SEG_META_ERR      = in->SEG_META_ERR;
    out->ACTION            = in->ACTION;
    out->TRAFFIC_CLASS     = in->TRAFFIC_CLASS;
    out->PARSER_INFO       = in->PARSER_INFO;
    out->NO_MODIFY         = in->NO_MODIFY;
    out->RX_LENGTH         = in->RX_LENGTH;
    out->RX_DATA           = in->RX_DATA;
    out->RX_PORT           = in->RX_PORT;
    out->TX_DROP           = in->TX_DROP;
    out->TX_TAG            = in->TX_TAG;
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
}
