/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_TXI2MOD_H
#define MBY_TXI2MOD_H

#include "fm_types.h"
#include "mby_mirror_type.h"
#include "mby_parser_info.h"
#include "mby_par_hdr_ptrs.h"

typedef struct mbyTxInToModifierStruct
{
    fm_uint32               CONTENT_ADDR;  // MOD Content address, expressed in 32B units
    fm_bool                 DROP_TTL;      //
    fm_byte                 ECN;           // ECN value to use in egress packet
    fm_uint16               EDGLORT;       // egress destination glort
    fm_uint32               FNMASK;        // forwarding normal mask
    fm_bool                 IS_TIMEOUT;    //
    fm_macaddr              L2_DMAC;       // L2 destination MAC address
    fm_uint16               L2_EVID1;      // 12-bit egress VLAN ID
    fm_bool                 ROUTED;        //
    mbyMirrorType           MIRTYP;        // mirror type
    fm_uint32               MOD_IDX;       // index into the MODIFY descriptor tables
    fm_byte                 MOD_PROF_IDX;  // modify profile index
    fm_bool                 NO_MODIFY;     // skip most of modifications in Modifier
    fm_bool                 OOM;           // out of memory
    mbyParserInfo           PARSER_INFO;   //
    mbyParserHdrPtrs        PA_HDR_PTRS;   // parser header pointers
    fm_bool                 PM_ERR;        // ECC error on PM
    fm_bool                 PM_ERR_NONSOP; //
    fm_byte                 QOS_L3_DSCP;   // 6-bit QOS Differentiated Services Code Point (DSCP)
    fm_bool                 SAF_ERROR;     // SAF error
    fm_uint64               TAIL_CSUM_LEN; // L4 CSUM related information
    fm_byte               * TX_DATA;       // egress (transmit) packet data
    fm_bool                 TX_DROP;       // flag indicating packet drop
    fm_byte                 TX_TAG;        //
    fm_byte                 XCAST;         //

} mbyTxInToModifier;

#endif // MBY_TXI2MOD_H
