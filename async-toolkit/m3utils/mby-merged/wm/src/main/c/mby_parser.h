// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_PARSER_H
#define MBY_PARSER_H

// Includes:

#include <mby_top_map.h> // header file auto-generated from RDL

#include "nulltype.h"
#include "mby_bitfield.h"
#include "mby_mac2par.h"
#include "mby_par2map.h"
#include "mby_pa_keys.h"
#include "mby_pa_flags.h"
#include "mby_pa_ptrs.h"

// Defines:

//efine MBY_PA_ANA_STAGES              32    // 32 stages
#define MBY_PA_ANA_STAGES              mby_ppe_parser_map_PARSER_ANA_S__nd

//efine MBY_PA_ANA_RULES               16    // 16 rules per stage
#define MBY_PA_ANA_RULES               parser_ana_s_rf_PARSER_ANA_S__nd

#define MBY_PA_PTYPE_ENTRIES           parser_ptype_tcam_rf_PARSER_PTYPE_TCAM__nd

#define MBY_PA_MAX_PTR_LEN             255
#define MBY_PA_MAX_DATA_SZ             16384
#define MBY_PSEUDOHEADER_SIZE          40

#define MBY_OTR_IPHDR_KEY              42    // Note: if IPv6, add 2
#define MBY_OTR_IPADDR_KEY             48
#define MBY_L4CSUM_KEY                 32
#define MBY_L4LEN_KEY                  35

#define MBY_OTR_L3_PTR                 2
#define MBY_OTR_L4_PTR                 3
#define MBY_PA_OTR_L4_UDP_V_FLAG       4     // otr_l4_udp_v flag
#define MBY_PA_OTR_L4_TCP_V_FLAG       5     // otr_l4_tcp_v flag
#define MBY_PA_OTR_HEAD_FRAG_V_FLAG    10    // otr_head_frag_v flag
#define MBY_PA_OTR_PAYLOAD_FRAG_V_FLAG 11    // otr_payload_frag_v flag
#define MBY_PA_OTR_L3_V_FLAG           22    // otr_l3_v flag
#define MBY_PA_ANA_OP_MASK_BITS        0xFFF
#define MBY_PA_ANA_OP_ROT_BITS         0x0F
#define MBY_PA_ANA_OP_ROT_SHIFT        0xC
#define MBY_PA_PROT_ID_NOP             0xFF

// Type defs:

typedef mby_ppe_parser_map Parser_rstate_t;
typedef NullType           Parser_wstate_t;
typedef mbyRxMacToParser   Parser_in_t;
typedef mbyParserToMapper  Parser_out_t;

// Function prototype:

void Parser
(
    mby_ppe_parser_map    const * const parser_map,
    mbyRxMacToParser      const * const in,
    mbyParserToMapper           * const out
);

#endif // MBY_PARSER_H
