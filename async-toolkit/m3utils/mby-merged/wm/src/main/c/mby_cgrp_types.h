/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_CGRP_STRUCTS_H
#define MBY_CGRP_STRUCTS_H

#include "fm_types.h"

typedef enum mbyClassifierGroupEnum
{
    MBY_CGRP_A = 0,
    MBY_CGRP_B = 1

} mbyClassifierGroup;

typedef struct mbyClassifierMuxedActionStruct
{
    fm_byte   dscp_ctl;    //  4b field
    fm_byte   ttl_ctl;     //  3b field
    fm_byte   tc_ctl;      //  4b field
    fm_byte   ecn_ctl;     //  4b field
    fm_byte   dscp;
    fm_byte   ttl01;
    fm_byte   tc;
    fm_byte   ecn;
    fm_bool   aqm_mark_en;

} mbyClassifierMuxedAction;

#define MBY_CGRP_HASH_PROFILE_ACTIONS  3 // MBY_CGRP_ACTION_HASH_PROFILE[0..2] == _ECMP/_MOD/_LAG
#define MBY_CGRP_META_ACTIONS          4 // MBY_CGRP_ACTION_META[0..3]
#define MBY_CGRP_POLICER_ACTIONS       4 // MBY_CGRP_ACTION_POLICER[0..3]
#define MBY_CGRP_REMAP_ACTIONS         8 // MBY_CGRP_ACTION_REMAP[0..7]

typedef struct mbyClassifierFunctionsStruct
{
    mbyClassifierMuxedAction muxed_action;                   // { ECN, TC, TTL, DSCP }
    fm_uint16 vid;                                           // 12b field
    fm_byte   vpri;                                          //  4b field
    fm_byte   dscp;                                          //  8b field
    fm_byte   tc;                                            //  4b field
    fm_byte   hash_profile  [MBY_CGRP_HASH_PROFILE_ACTIONS]; // 3 x  6b fields
    fm_byte   mod_meta      [MBY_CGRP_META_ACTIONS];         // 4 x  8b fields
    fm_uint32 fwd;                                           // 22b field
    fm_uint32 policer_action[MBY_CGRP_POLICER_ACTIONS];      // 4 x 24b fields
    fm_uint32 mod_profile;                                   // 24b field
    fm_uint32 remap         [MBY_CGRP_REMAP_ACTIONS];        // 8 x 24b fields
    fm_uint32 used;                                          // 24b field

} mbyClassifierFunctions;

typedef struct mbyClassifierFlags
{
    fm_bool   drop;
    fm_bool   trap;
    fm_bool   log;
    fm_bool   no_route;
    fm_bool   rx_mirror;
    fm_byte   tx_tag;  // 2b field
    fm_byte   trigger; // 8b field
    fm_byte   profile; // 6b field
    fm_bool   learn_notify;

} mbyClassifierFlags;

#define MBY_CGRP_KEY8   64
#define MBY_CGRP_KEY16  32
#define MBY_CGRP_KEY32  16

#define MBY_CGRP_KEY16_BASE      0
#define MBY_CGRP_KEY8_BASE       ( MBY_CGRP_KEY16_BASE + MBY_CGRP_KEY16 )
#define MBY_CGRP_KEY32_BASE      ( MBY_CGRP_KEY8_BASE  + MBY_CGRP_KEY8 )

#define MBY_CGRP_KEYS            ( MBY_CGRP_KEY8 + MBY_CGRP_KEY16   + MBY_CGRP_KEY32   )
#define MBY_CGRP_HASH_KEYS       ( MBY_CGRP_KEY8 + MBY_CGRP_KEY16*2 + MBY_CGRP_KEY32*4 )

typedef struct mbyClassifierKeysStruct
{
    fm_uint32 key32[MBY_CGRP_KEY32];
    fm_uint16 key16[MBY_CGRP_KEY16];
    fm_byte   key8 [MBY_CGRP_KEY8 ];

} mbyClassifierKeys;

#define MBY_CGRP_ACT24           16
#define MBY_CGRP_ACT4            26
#define MBY_CGRP_ACT1            24

typedef struct mbyActionPrecValStruct
{
    fm_byte    prec; // 3b field
    fm_uint32  val;  // act24.val is 24b, act4.val is 4b, act1.val is 1b

} mbyActionPrecVal;

typedef struct mbyClassifierActionsStruct
{
    mbyActionPrecVal act24[MBY_CGRP_ACT24];
    mbyActionPrecVal act4 [MBY_CGRP_ACT4 ];
    mbyActionPrecVal act1 [MBY_CGRP_ACT1 ];

} mbyClassifierActions;

#endif // MBY_CGRP_STRUCTS_H
