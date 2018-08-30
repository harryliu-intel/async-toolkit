// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_TXSTATS_H
#define MBY_TXSTATS_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"
#include "mby_modifier.h"

// Defines:

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

typedef struct mbyTxStatsToTxMacStruct
{
    fm_byte               * TX_DATA;   // egress packet data
    fm_uint32               TX_LENGTH; // egress packet data length [bytes]
    fm_uint32               TX_PORT;   // egress port

} mbyTxStatsToTxMac;

#endif

