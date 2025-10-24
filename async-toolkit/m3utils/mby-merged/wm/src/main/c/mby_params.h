/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_PARAMS_H
#define MBY_PARAMS_H

// Defines:

#define MBY_PORTS_COUNT          16 // <-- REVISIT!!!
#define MBY_FABRIC_LOG_PORTS     MBY_PORTS_COUNT

#define MBY_MAX_FABRIC_LAG_PORT  17
#define MBY_DEST_PORTS_COUNT     257

// Used for tests and C client APIs for DV
#define MBY_MAX_DATA_LEN         (32 * 1024)

#define MBY_SEGMENT_LEN          256

#define MAC_ADDR_BYTES           6

#endif // MBY_PARAMS_H
