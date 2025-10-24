/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_TXSTATS_H
#define MBY_TXSTATS_H

// Includes:

#include <mby_top_map.h> // header file auto-generated from RDL

#include "mby_mod2txs.h"
#include "mby_txs2mac.h"

// Defines:

#define MBY_REGISTER_ARRAY_SIZE  0x1800000 // obsolete, need to get rid of old regs <-- REVISIT!!!

/******** MOD_BASE *******/
#define MBY_MOD_BASE                                            (0x4000000)
#define MBY_MOD_SIZE                                            (0x0800000)

#define MBY_MOD_STATS_BANK_FRAME_WIDTH                          2
#define MBY_MOD_STATS_BANK_FRAME_ENTRIES                        384
#define MBY_MOD_STATS_BANK_FRAME(index, word)                   ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x011E000) + (MBY_MOD_BASE))

#define MBY_MOD_STATS_BANK_FRAME_l_FRAME_COUNTER                0
#define MBY_MOD_STATS_BANK_FRAME_h_FRAME_COUNTER                47

#define MBY_MOD_STATS_BANK_BYTE_WIDTH                           2
#define MBY_MOD_STATS_BANK_BYTE_ENTRIES                         384
#define MBY_MOD_STATS_BANK_BYTE(index, word)                    ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x011F000) + (MBY_MOD_BASE))

#define MBY_MOD_STATS_BANK_BYTE_l_BYTE_COUNTER                  0
#define MBY_MOD_STATS_BANK_BYTE_h_BYTE_COUNTER                  55

// Enums:

// Structs:

// Function prototype:

void TxStats
(
    fm_uint32                          regs[MBY_REGISTER_ARRAY_SIZE],
    mbyModifierToTxStats const * const in,
    mbyTxStatsToTxMac          * const out
);

#endif // MBY_TXSTATS_H
