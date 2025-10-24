/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_MAC2PAR_H
#define MBY_MAC2PAR_H

#include "fm_types.h"

#define MBY_PA_MAX_SEG_LEN  192 // max segment length

typedef struct mbyRxMacToParserStruct
{
    fm_uint32        RX_PORT;                      ///< Ingress port
    fm_uint32        RX_LENGTH;                    ///< Ingress packet data length [bytes]
    fm_byte          SEG_DATA[MBY_PA_MAX_SEG_LEN]; ///< Ingress segment data

} mbyRxMacToParser;

#endif
