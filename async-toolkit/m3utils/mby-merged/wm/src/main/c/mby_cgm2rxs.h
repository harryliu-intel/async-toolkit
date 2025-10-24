/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_CGM2RXS_H
#define MBY_CGM2RXS_H

#include "fm_types.h"
#include "mby_mirror_type.h"
#include "mby_parser_info.h"
#include "mby_par_hdr_ptrs.h"

typedef struct mbyCongMgmtToRxStatsStruct
{
    // pass-thru:
    fm_uint32               ACTION;        ///< resolved action
    fm_uint32               CONTENT_ADDR;  ///< MOD Content address, expressed in 32B units
    fm_bool                 DROP_TTL;
    fm_byte                 ECN;           ///< ECN value to use in egress packet
    fm_uint16               EDGLORT;       ///< egress destination glort
    fm_uint32               FNMASK;        ///< forwarding normal mask
    fm_bool                 IS_IPV4;       ///< packet is of type IP v4
    fm_bool                 IS_IPV6;       ///< packet is of type IP v6
    fm_bool                 IS_TIMEOUT;
    fm_macaddr              L2_DMAC;       ///< layer 2 destination address
    fm_uint16               L2_EVID1;      ///< 12-bit egress VLAN ID
    fm_uint16               L2_IVLAN1_CNT; ///< ingress VLAN counter
    fm_bool                 ROUTED;
    mbyMirrorType           MIRTYP;        ///< mirror type
    fm_uint32               MOD_IDX;       ///< index into the MODIFY descriptor tables
    fm_byte                 MOD_PROF_IDX;  ///< modify profile index
    fm_bool                 NO_MODIFY;     ///< skip most of modifications in Modifier
    fm_bool                 OOM;           ///< out of memory
    mbyParserInfo           PARSER_INFO;   ///< parser info struct
    mbyParserHdrPtrs        PA_HDR_PTRS;   ///< parser header pointers
    fm_bool                 PM_ERR;        ///< ECC error on PM
    fm_bool                 PM_ERR_NONSOP;
    fm_byte                 QOS_L3_DSCP;   ///< 6-bit QOS Differentiated Services Code Point (DSCP)
    fm_uint32               RX_PORT;       ///< RX port number
    fm_bool                 SAF_ERROR;     ///< SAF error
    fm_bool                 SEG_META_ERR;  ///< segment error
    fm_uint64               TAIL_CSUM_LEN; ///< L4 CSUM related information
    fm_byte                 TRAFFIC_CLASS; ///< 3-bit traffic class
    fm_bool                 TX_DROP;       ///< flag indicating packet drop
    fm_uint32               TX_PORT;       ///< egress port
    fm_byte                 TX_TAG;        ///< egress tag
    fm_byte                 XCAST;
    fm_uint32               RX_LENGTH;     ///< Ingress packet data length [bytes]

} mbyCongMgmtToRxStats;

#endif // MBY_CGM2RXS_H
