// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_MODIFIER_H
#define MBY_MODIFIER_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"

// Defines:

// Enums:

// Structs:

typedef struct mbyTxInToModifierStruct
{
    fm_bool                 FOO;       // dummy field <-- FIXME!!!

} mbyTxInToModifier;

typedef struct mbyModifierToTxStatsStruct
{
    fm_byte                *TX_DATA;         // egress packet data
    fm_uint32               TX_LENGTH;       // egress packet data length [bytes]
    fm_uint32               TX_PORT;         // egress port
    fm_uint16               TX_DISP;         // 4-bit egress frame disposition
    fm_uint32               TX_STATS_LENGTH; // egress packet data stats length [bytes]

} mbyModifierToTxStats;

#endif
