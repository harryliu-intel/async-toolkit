// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_RXTOTX_H
#define MBY_RXTOTX_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"

// Defines:

// Enums:

// Structs:

typedef struct mbyTxInToModifierStruct
{
    fm_bool                 DROP_TTL;          //
    fm_byte                 ECN;               // ECN value to use in egress packet
    fm_uint16               EDGLORT;           // egress destination glort
    fm_bool                 IS_TIMEOUT;        //
    fm_macaddr              L2_DMAC;           // L2 destination MAC address
    fm_uint16               L2_EVID1;          // 12-bit egress VLAN ID
    fm_bool                 MARK_ROUTED;       //
    mbyMirrorType           MIRTYP;            // mirror type
    fm_uint32               MOD_IDX;           // index into the MODIFY descriptor tables
    fm_bool                 NO_MODIFY;         // skip most of modifications in Modifier
    fm_bool                 OOM;               // out of memory
    mbyParserInfo           PARSER_INFO;       //
    fm_bool                 PM_ERR;            // ECC error on PM
    fm_bool                 PM_ERR_NONSOP;     //
    fm_byte                 QOS_L3_DSCP;       // 6-bit QOS Differentiated Services Code Point (DSCP)
    fm_byte               * RX_DATA;           // ingress (receive) packet data
    fm_uint32               RX_LENGTH;         // ingress packet data length [bytes]
    fm_bool                 SAF_ERROR;         // SAF error
    fm_uint64               TAIL_CSUM_LEN;     // L4 CSUM related information
    fm_byte               * TX_DATA;           // egress (transmit) packet data
    fm_bool                 TX_DROP;           // flag indicating packet drop
    fm_uint32               TX_LENGTH;         // egress packet data length[byte]
    fm_uint32               TX_PORT;           // egress port
    fm_byte                 TX_TAG;            //
    fm_byte                 XCAST;             //

} mbyTxInToModifier;

#endif
