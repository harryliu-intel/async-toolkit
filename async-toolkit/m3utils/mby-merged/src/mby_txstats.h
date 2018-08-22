// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_TXSTATS_H
#define MBY_TXSTATS_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"

// Defines:

// Enums:

// Structs:

typedef struct mbyTxStatsToTxOutStruct
{
    fm_byte                *TX_DATA;   // egress packet data
    fm_uint32               TX_LENGTH; // egress packet data length [bytes]
    fm_uint32               TX_PORT;   // egress port

} mbyTxStatsToTxOut;

#endif

