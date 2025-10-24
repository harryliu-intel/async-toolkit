/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_CONGMGMT_H
#define MBY_CONGMGMT_H

// Includes:

#include <mby_top_map.h> // header file auto-generated from RDL

#include "mby_bitfield.h"
#include "mby_trg2cgm.h"
#include "mby_cgm2rxs.h"
#include "mby_mirror_type.h"
#include "mby_parser_info.h"  // mbyParserInfo
#include "mby_par_hdr_ptrs.h" // mbyParserHdrPtrs

// Function prototype:

void CongMgmt
(
    mby_ppe_cm_apply_map  const * const cm_apply_map,
    mby_ppe_cm_usage_map  const * const cm_usage_map,
    mbyTriggersToCongMgmt const * const in,
    mbyCongMgmtToRxStats        * const out
);

#endif
