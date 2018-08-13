// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_PARSER_H
#define MBY_PARSER_H

#include "mby_common.h"
#include "../m3/genviews/src/build_c/mby_c/src/mby_top_map.h"

// --------------------------------------------------------------------------------

// Constants:
#define MBY_PA_MAX_SEG_LEN               192
#define MBY_PA_MAX_PTR_LEN               255
#define MBY_PA_MAX_DATA_SZ               16384
#define MBY_PSEUDOHEADER_SIZE            40
#define MBY_PA_ANA_STAGES                32 // 32 stages
#define MBY_PA_ANA_RULES                 16 // 16 rules per stage
#define MBY_OTR_IPHDR_KEY                42 // Note: if IPv6, add 2
#define MBY_OTR_IPADDR_KEY               48
#define MBY_L4CSUM_KEY                   32
#define MBY_L4LEN_KEY                    35

#define MBY_OTR_L3_PTR                   2
#define MBY_OTR_L4_PTR                   3
#define MBY_PA_OTR_L4_UDP_V_FLAG         4  // otr_l4_udp_v flag
#define MBY_PA_OTR_L4_TCP_V_FLAG         5  // otr_l4_tcp_v flag
#define MBY_PA_OTR_HEAD_FRAG_V_FLAG      10 // otr_head_frag_v flag
#define MBY_PA_OTR_PAYLOAD_FRAG_V_FLAG   11 // otr_payload_frag_v flag
#define MBY_PA_OTR_L3_V_FLAG             22 // otr_l3_v flag
#define MBY_PA_ANA_OP_MASK_BITS          0xFFF
#define MBY_PA_ANA_OP_ROT_BITS           0x0F
#define MBY_PA_ANA_OP_ROT_SHIFT          0xC

// Function prototypes:
void mbyParser
(
    const mby_ppe_parser_map   *       r,
    const mbyMacToParser       * const in, 
          mbyParserToMapper    * const out
);

#endif // MBY_PARSER_H
