/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_MASKGEN_H
#define MBY_MASKGEN_H

// Includes:

#include <mby_top_map.h> // header file auto-generated from RDL

#include "mby_nxt2msk.h"
#include "mby_msk2trg.h"
#include "mby_maskgen_regs.h"
#include "mby_action_codes.h"
#include "mby_fclass_type.h"
#include "mby_log_type.h"

// Defines:

#define MBY_SAF_MATRIX_h_CUT_THRU_MODE                   2
#define MBY_SAF_MATRIX_b_IGNORE_FRAME_ERROR              0

#define MBY_DMAC_IEEE_PREFIX                 FM_LITERAL_U64(0x0180c2000000)
#define MBY_SPECIAL_DMASK                    0xFFFFFFFFFF00

#define MBY_SV_MOVE_DROP_RESERVED            0
#define MBY_SV_MOVE_DROP_PORT                1
#define MBY_SV_MOVE_DROP_ADDR                2
#define MBY_SV_MOVE_DROP_STATIC              3

#define MBY_DEFAULT_DMASK                    0xFFFFFFFFFFFFFFFF


// Enums:

typedef enum mbyStpStateEnum
{
    MBY_STP_STATE_DISABLE = 0,
    MBY_STP_STATE_LISTENING,
    MBY_STP_STATE_LEARNING,
    MBY_STP_STATE_FORWARD

} mbyStpState;

typedef enum mbyIeeeReservedMacActionActionEnum
{
    MBY_IEEE_RESERVED_MAC_ACTION_ACTION_SWITCHNORMALLY = 0,
    MBY_IEEE_RESERVED_MAC_ACTION_ACTION_TRAP           = 1,
    MBY_IEEE_RESERVED_MAC_ACTION_ACTION_DROP           = 2,
    MBY_IEEE_RESERVED_MAC_ACTION_ACTION_LOG            = 3

} mbyIeeeReservedMacActionAction;

// Structs:

// Function prototype:

void MaskGen
(
    mby_ppe_fwd_misc_map  const * const fwd_misc,
    mby_ppe_mst_glort_map const * const glort_map,
    mby_ppe_cm_apply_map  const * const cm_apply,
    mbyNextHopToMaskGen   const * const in,
    mbyMaskGenToTriggers        * const out
);

#endif
