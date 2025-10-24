/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_PA_KEYS_H
#define MBY_PA_KEYS_H

typedef enum mbyParserKeysEnum
{
    MBY_PA_KEYS_INNER_DMAC          =  0,
    MBY_PA_KEYS_INNER_SMAC          =  3,
    MBY_PA_KEYS_OUTER_DMAC          =  6,
    MBY_PA_KEYS_OUTER_SMAC          =  9,
    MBY_PA_KEYS_OUTER_ETYPE         = 12,
    MBY_PA_KEYS_OUTER_VLAN1         = 14,
    MBY_PA_KEYS_OUTER_VLAN2         = 15,
    MBY_PA_KEYS_OUTER_L4SRC         = 16,
    MBY_PA_KEYS_OUTER_L4DST         = 17,
    MBY_PA_KEYS_INNER_ETYPE         = 18,
    MBY_PA_KEYS_INNER_VLAN1         = 20,
    MBY_PA_KEYS_INNER_VLAN2         = 21,
    MBY_PA_KEYS_INNER_L4SRC         = 22,
    MBY_PA_KEYS_INNER_L4DST         = 23,
    MBY_PA_KEYS_MPLS                = 24,
    MBY_PA_KEYS_GENERAL             = 32,
    MBY_PA_KEYS_INNER_IP_HEADER     = 36,
    MBY_PA_KEYS_OUTER_IP_HEADER     = 42,
    MBY_PA_KEYS_OUTER_SIPDIP        = 48,
    MBY_PA_KEYS_INNER_SIPDIP        = 64

} mbyParserKeys;

#endif //  MBY_PA_KEYS_H
