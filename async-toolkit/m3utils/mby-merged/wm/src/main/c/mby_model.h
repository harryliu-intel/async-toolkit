/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_MODEL_H
#define MBY_MODEL_H

#include <mby_top_map.h> // header file auto-generated from RDL

#include "fm_types.h"
#include "varchar.h"

/* Public interfaces exposed by the MBY functional model */
fm_status mbyResetModel
(
    mby_top_map__addr const * const w
);

fm_status mbyInitRegs
(
    mby_top_map__addr const * const w
);

fm_status mbyTopMapSetup
(
    mby_top_map       const * const r,
    mby_top_map__addr const * const w
);

fm_status mbySendPacket
(
    mby_top_map       const * const r,
    mby_top_map__addr const * const w,
    fm_uint32                 const port,
    fm_byte           const * const packet,
    fm_uint32                 const length
);

fm_status mbyReceivePacket
(
    mby_top_map       const * const r,
    mby_top_map__addr const * const w,
    varchar_t         const *       rx_data,
    fm_uint32                 const max_pkt_size,
    fm_uint32               * const port,
    varchar_t               * const tx_data
);

#endif // MBY_MODEL_H
