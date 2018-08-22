// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_CONGMGMT_H
#define MBY_CONGMGMT_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"

// Defines:

// Enums:

// Structs:

typedef struct mbyCongMgmtToRxStatsStruct
{
    fm_uint32               RX_LENGTH;          // RX packet length
    fm_uint32               RX_PORT;            // RX port number
    fm_bool                 IS_IPV4;            // packet is of type IP v4
    fm_bool                 IS_IPV6;            // packet is of type IP v6
    fm_macaddr              L2_DMAC;            // layer 2 destination address
    fm_uint16               L2_IVLAN1_CNT_INDEX;
    fm_uint64               FNMASK;             // forwarding normal mask
    fm_bool                 SEG_META_ERR;       // segment error
    fm_bool                 allowStateChange;   // allow 
    fm_uint32               ACTION;             // resolved action
    fm_byte                 TC;                 // 3-bit traffic class

} mbyCongMgmtToRxStats;

#endif
