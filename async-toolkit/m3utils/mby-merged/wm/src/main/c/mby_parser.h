// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_PARSER_H
#define MBY_PARSER_H

#include "mby_common.h"

// --------------------------------------------------------------------------------

// Constants:
const fm_uint MBY_PA_MAX_SEG_LEN             = 192;
const fm_uint MBY_PA_MAX_PTR_LEN             = 255;
const fm_uint MBY_PA_MAX_DATA_SZ             = 16384;
const fm_uint MBY_PSEUDOHEADER_SIZE          = 40;
const fm_uint MBY_PA_ANA_STAGES              = 32; // 32 stages
const fm_uint MBY_PA_ANA_RULES               = 16; // 16 rules per stage
const fm_uint MBY_MAX_PA_KEY_LEN             = 80;
const fm_uint MBY_MAX_PA_PTR_NUM             = 8;
const fm_uint MBY_MAX_PA_FLAGS               = 48;
const fm_uint MBY_OTR_IPHDR_KEY              = 42; // Note: if IPv6, add 2
const fm_uint MBY_OTR_IPADDR_KEY             = 48;
const fm_uint MBY_L4CSUM_KEY                 = 32;
const fm_uint MBY_L4LEN_KEY                  = 35;

const fm_uint MBY_OTR_L3_PTR                 = 2;
const fm_uint MBY_OTR_L4_PTR                 = 3;
const fm_uint MBY_PA_OTR_L4_UDP_V_FLAG       = 4;  // otr_l4_udp_v flag
const fm_uint MBY_PA_OTR_L4_TCP_V_FLAG       = 5;  // otr_l4_tcp_v flag
const fm_uint MBY_PA_OTR_HEAD_FRAG_V_FLAG    = 10; // otr_head_frag_v flag
const fm_uint MBY_PA_OTR_PAYLOAD_FRAG_V_FLAG = 11; // otr_payload_frag_v flag
const fm_uint MBY_PA_OTR_L3_V_FLAG           = 22; // otr_l3_v flag
const fm_uint MBY_PA_ANA_OP_MASK_BITS        = 0xFFF;
const fm_uint MBY_PA_ANA_OP_ROT_BITS         = 0x0F;
const fm_uint MBY_PA_ANA_OP_ROT_SHIFT        = 0xC;

// Function prototypes:
void Parser
(
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyMacToParser    * const in, 
          mbyParserToMapper * const out
);

#endif // MBY_PARSER_H
