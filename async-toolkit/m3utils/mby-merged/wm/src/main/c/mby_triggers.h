// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_TRIGGERS_H
#define MBY_TRIGGERS_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"
#include "mby_maskgen.h"
#include "mby_triggers_regs.h"

#include "../m3/genviews/src/build_c/mby_c/src/mby_top_map.h"

// Defines:
#define MBY_TRG_IN_REGS    mby_ppe_trig_apply_map      * const trig_apply_map, \
                           mby_ppe_trig_apply_misc_map * const trig_apply_misc_map, \
                           mby_ppe_trig_usage_map      * const trig_usage_map

#define MBY_TRG_IN_REGS_P                                      trig_apply_map, \
                                                               trig_apply_misc_map, \
                                                               trig_usage_map

#define MBY_TRIGGERS_COUNT            96 

// Enums:

// Structs:

typedef struct mbyTriggersToCongMgmtSruct
{
    // pass-thru:
    fm_uint32               ACTION;        // resolved action
    fm_bool                 DROP_TTL;
    fm_byte                 ECN;           // ECN value to use in egress packet
    fm_uint16               EDGLORT;       // egress destination glort
    fm_uint32               FNMASK;        // forwarding normal mask
    fm_bool                 IS_IPV4;       // packet is of type IP v4
    fm_bool                 IS_IPV6;       // packet is of type IP v6
    fm_bool                 IS_TIMEOUT;
    fm_macaddr              L2_DMAC;       // layer 2 destination address
    fm_uint16               L2_EVID1;      // 12-bit egress VLAN ID
    fm_uint16               L2_IVLAN1_CNT; // ingress VLAN counter
    fm_bool                 MARK_ROUTED;
    mbyMirrorType           MIRTYP;        // mirror type
    fm_uint32               MOD_IDX;       // index into the MODIFY descriptor tables
    fm_bool                 NO_MODIFY;     // skip most of modifications in Modifier
    fm_bool                 OOM;           // out of memory
    mbyParserInfo           PARSER_INFO;   // parser info struct
    fm_bool                 PM_ERR;        // ECC error on PM
    fm_bool                 PM_ERR_NONSOP;
    fm_byte                 QOS_L3_DSCP;   // 6-bit QOS Differentiated Services Code Point (DSCP)
    fm_byte               * RX_DATA;       // ingress (receive) packet data
    fm_uint32               RX_LENGTH;     // RX packet length
    fm_uint32               RX_PORT;       // RX port number
    fm_bool                 SAF_ERROR;     // SAF error
    fm_bool                 SEG_META_ERR;  // segment error
    fm_uint64               TAIL_CSUM_LEN; // L4 CSUM related information
    fm_byte                 TRAFFIC_CLASS; // 3-bit traffic class
    fm_bool                 TX_DROP;       // flag indicating packet drop
    fm_byte                 TX_TAG;        // egress tag
    fm_byte                 XCAST;         //

} mbyTriggersToCongMgmt;

#endif
