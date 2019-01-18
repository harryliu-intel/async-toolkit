// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_PARSER_INFO_H
#define MBY_PARSER_INFO_H

#include "fm_types.h"

typedef enum mbyParserInfoIndexEnum
{
    MBY_PA_INFO_OTR_L2   = 0,
    MBY_PA_INFO_OTR_MPLS = 1,
    MBY_PA_INFO_OTR_L3   = 2,
    MBY_PA_INFO_OTR_L4   = 3,
    MBY_PA_INFO_INR_L2   = 4,
    MBY_PA_INFO_INR_MPLS = 5,
    MBY_PA_INFO_INR_L3   = 6,
    MBY_PA_INFO_INR_L4   = 7

} mbyParserInfoIndex;

typedef struct mbyParserInfoStruct
{
    fm_byte  otr_l2_len;    // 3b field
    fm_bool  otr_l2_vlan1;
    fm_bool  otr_l2_vlan2;
    fm_bool  otr_l2_v2first;
    fm_byte  otr_mpls_len;  // 3b field
    fm_byte  otr_l3_len;    // 4b field
    fm_bool  otr_l3_v6;
    fm_bool  otr_l4_udp;
    fm_bool  otr_l4_tcp;
    fm_byte  otr_tun_len;   // 5b field
    fm_byte  inr_l2_len;    // 3b field
    fm_bool  inr_l2_vlan1;
    fm_bool  inr_l2_vlan2;
    fm_bool  inr_l2_v2first;
    fm_byte  inr_mpls_len;  // 3b field
    fm_byte  inr_l3_len;    // 4b field
    fm_bool  inr_l3_v6;
    fm_bool  inr_l4_udp;
    fm_bool  inr_l4_tcp;

} mbyParserInfo;

#endif // MBY_PARSER_INFO_H
