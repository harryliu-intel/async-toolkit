/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_HSH2NXT_H
#define MBY_HSH2NXT_H

#include "fm_types.h"
#include "mby_parser_info.h"
#include "mby_par_hdr_ptrs.h"
#include "mby_cgrp_types.h"
#include "mby_ma_table.h"
#include "mby_trig_results.h"

typedef struct mbyHashKeysStruct
{
    fm_uint16 ecmp_hash; // 12b field
    fm_uint64 mod_meta;  // 48b field
    fm_uint16 lag_hashA; // 12b field
    fm_uint16 lag_hashB; // 12b field
    fm_byte   rotA;
    fm_byte   rotB;
    fm_bool   ecmp_rotation;

} mbyHashKeys;

typedef struct mbyHashToNextHopStruct
{
    fm_byte            ARP_HASH[16];
    mbyHashKeys        HASH_KEYS;              ///> Grouped output of HASH block (ECMP, MOD_META, LAG hashes)
    fm_uint16          ECMP_HASH;
    fm_uint32          HASH_ROT_A;
    fm_uint32          HASH_ROT_B;
    fm_uint16          RAW_HASH;
    // pass-thru:
    fm_macaddr         L2_DMAC;
    fm_macaddr         L2_SMAC;
    fm_uint32          ACTION;                 ///> resolved action
    fm_byte            CGRP_TRIG;              ///> classifier action triggers
    fm_bool            CPU_TRAP;               ///> CPU trap
    fm_uint16          CSGLORT;                ///> 16-bit canonical source GLORT
    fm_bool            DECAP;
    fm_macaddr         DMAC_FROM_IPV6;
    fm_bool            DROP_TTL;               ///> packet should be dropped
    fm_bool            ENCAP;
    mbyClassifierFlags CGRP_FLAGS;             ///> flags {TX_TAG, RX_MIRROR, NO_ROUTE, LOG, TRAP, DROP}
    fm_uint32          CONTENT_ADDR;           ///> MOD Content address, expressed in 32B units
    fm_uint32          FWD;
    fm_bool            GLORT_CAM_MISS;         ///> GLORT lookup resulted in a miss flag
    fm_uint32          GLORT_DMASK;            ///> 24-bit GLORT-based destination mask
    fm_uint16          IP_MCAST_IDX;           ///> index into the MCAST_VLAN_TABLE
    fm_bool            IS_IPV4;                ///> packet is IPv4
    fm_bool            IS_IPV6;                ///> packet is IPv6
    fm_uint16          L2_ETYPE;               ///> 16-bit innermost Ethernet type
    fm_byte            L2_IDOMAIN;
    fm_uint16          L2_IVID1;
    fm_byte            L3_IDOMAIN;
    fm_bool            LEARN_MODE;
    fm_bool            LEARN_NOTIFY;           ///> learning is enabled flag
    fm_uint32          MIRROR0_PROFILE_IDX;    ///> mirror 0 profile index
    fm_byte            MOD_PROF_IDX;           ///> modify profile index
    fm_bool            MTU_VIOLATION;          ///> packet violates the MTU
    fm_byte            NAD;                    ///> 4-bit NAD (network addressing domain)
    fm_bool            PARITY_ERROR;           ///> memory parity error flag
    fm_bool            PARSER_ERROR;           ///> header parse error flag
    mbyParserInfo      PARSER_INFO;            ///> parser info structure
    fm_bool            PARSER_WINDOW_V;        ///> parser window valid flag
    fm_bool            PA_DROP;                ///> checksum validation error, drop pkt in tail proc
    fm_bool            PA_L3LEN_ERR;           ///> l3 length error
    mbyParserHdrPtrs   PA_HDR_PTRS;            ///> parser header pointers
    fm_uint32          PRE_RESOLVE_DMASK;      ///> destination mask before action resolution
    fm_byte            QOS_TC;                 ///> 4-bit switch priority
    fm_bool            RX_MIRROR;              ///> rx mirror frame
    fm_uint32          RX_PORT;                ///> receive port number
    fm_bool            SA_HIT;                 ///> source MAC address lookup hit
    mbyMaTable         SA_RESULT;              ///> source MAC address lookup result
    fm_byte            SEG_META_ERR;           ///> segment error
    fm_byte            SV_DROP;                ///> MAC security violation info
    fm_bool            TARGETED_DETERMINISTIC; ///> mode is set to targeted deterministic
    fm_byte            TRAFFIC_CLASS;          ///> 3-bit traffic class
    fm_bool            TRAP_ICMP;              ///> ICMP packet should be trapped
    fm_bool            TRAP_IGMP;              ///> IGMP packet should be trapped
    fm_bool            TRAP_IP_OPTIONS;        ///> IP options present
    mbyTriggerResults  TRIGGERS;               ///> trigger results
    fm_uint32          RX_LENGTH;              ///< Ingress packet data length [bytes]

} mbyHashToNextHop;


#endif // MBY_HSH2NXT_H
