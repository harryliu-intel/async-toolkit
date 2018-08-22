// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_RXTOTX_H
#define MBY_RXTOTX_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"

// Defines:

// Enums:

// Structs:

typedef struct mbyTxInToModifierStruct
{
    fm_bool                 NO_MODIFY;  // skip most of modifications in Modifier
    fm_uint32               RX_LENGTH;  // ingress packet data length [bytes]
    fm_byte                *RX_DATA;
    fm_bool                 TX_DROP;
    fm_byte                 TX_TAG;
    fm_uint32               TX_STATS_LAST_LEN;

} mbyTxInToModifier;

#endif
