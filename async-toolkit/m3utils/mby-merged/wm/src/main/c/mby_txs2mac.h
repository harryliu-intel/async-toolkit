/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_TXS2MAC_H
#define MBY_TXS2MAC_H

#include "fm_types.h"

typedef struct mbyTxStatsToTxMacStruct
{
//  fm_byte               * TX_DATA;   // egress packet data
//  fm_uint32               TX_LENGTH; // egress packet data length [bytes]
    fm_uint32               TX_PORT;   // egress port

} mbyTxStatsToTxMac;

#endif // MBY_TXS2MAC_H
