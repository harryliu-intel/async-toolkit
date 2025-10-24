/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_MODEL_BASIC_H
#define MBY_MODEL_BASIC_H

#include <mby_top_map.h>

// interface required by the M3 model_server

void mby_top_map_Setup
(
    mby_top_map       const * r,
    mby_top_map__addr const * w
);

void mby_top_map_SendPacket
(
    mby_top_map       const * r,
    mby_top_map__addr const * w,
    int                       port,
    unsigned char           * packet,
    unsigned int              length
);

// void mby_top_map_ReceivePacket(); // missing <-- FIXME!!!

#endif
