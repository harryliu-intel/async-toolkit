// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_triggers.h"
#include "mby_congmgmt.h"

void CongMgmt
(
    mby_ppe_cm_apply_map        * const cm_apply_map,
    mby_ppe_cm_usage_map        * const cm_usage_map,
    const mbyTriggersToCongMgmt * const in,
          mbyCongMgmtToRxStats  * const out
)
{
    // Note: this stage is empty for now. Will be coded up in the next sprint. <-- REVISIT!!!

    // Pass thru:
    out->ACTION            = in->ACTION;
    out->CONTENT_ADDR      = in->CONTENT_ADDR;
    out->DROP_TTL          = in->DROP_TTL;
    out->ECN               = in->ECN;
    out->EDGLORT           = in->EDGLORT;
    out->FNMASK            = in->FNMASK;
    out->IS_IPV4           = in->IS_IPV4;
    out->IS_IPV6           = in->IS_IPV6;
    out->IS_TIMEOUT        = in->IS_TIMEOUT;
    out->L2_DMAC           = in->L2_DMAC;
    out->L2_EVID1          = in->L2_EVID1;
    out->L2_IVLAN1_CNT     = in->L2_IVLAN1_CNT;
    out->MARK_ROUTED       = in->MARK_ROUTED;
    out->MIRTYP            = in->MIRTYP;
    out->MOD_IDX           = in->MOD_IDX;
    out->MOD_PROF_IDX      = in->MOD_PROF_IDX;
    out->OOM               = in->OOM;
    out->PARSER_INFO       = in->PARSER_INFO;
    out->PA_HDR_PTRS       = in->PA_HDR_PTRS;
    out->PM_ERR            = in->PM_ERR;
    out->PM_ERR_NONSOP     = in->PM_ERR_NONSOP;
    out->QOS_L3_DSCP       = in->QOS_L3_DSCP;
    out->RX_DATA           = in->RX_DATA;
    out->RX_LENGTH         = in->RX_LENGTH;
    out->RX_PORT           = in->RX_PORT;
    out->SAF_ERROR         = in->SAF_ERROR;
    out->SEG_META_ERR      = in->SEG_META_ERR;
    out->TAIL_CSUM_LEN     = in->TAIL_CSUM_LEN;
    out->TRAFFIC_CLASS     = in->TRAFFIC_CLASS;
    out->TX_DROP           = in->TX_DROP;
    out->TX_TAG            = in->TX_TAG;
    out->XCAST             = in->XCAST;
}
