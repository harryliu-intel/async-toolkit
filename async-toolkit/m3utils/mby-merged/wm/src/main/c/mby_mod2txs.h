// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_MOD2TXS_H
#define MBY_MOD2TXS_H

#include "fm_types.h"
#include "mby_par_hdr_ptrs.h"

typedef struct mbyModifierToTxStatsStruct
{
    fm_bool                 NO_PRI_ENC;      // do not use priority encoding, use default enc.
//  fm_byte               * TX_DATA;         // egress packet data
    fm_uint16               TX_DISP;         // egress frame disposition
//  fm_uint32               TX_LENGTH;       // egress packet data length [bytes]
    fm_uint32               TX_PORT;         // egress port
    fm_uint32               TX_STATS_LENGTH; // egress packet data stats length [bytes]
    mbyParserHdrPtrs        PA_HDR_PTRS;     // parser header pointers

} mbyModifierToTxStats;

#endif // MBY_MOD2TXS_H
