// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_PA_FLAGS_H
#define MBY_PA_FLAGS_H

typedef enum mbyParserFlagsEnum
{
    MBY_PA_FLAGS_NOP                =  0,
    MBY_PA_FLAGS_OTR_L2_VLAN1       =  1,
    MBY_PA_FLAGS_OTR_L2_VLAN2       =  2,
    MBY_PA_FLAGS_OTR_L2_V2FIRST     =  3,
    MBY_PA_FLAGS_OTR_L4_UDP_V       =  4,
    MBY_PA_FLAGS_OTR_L4_TCP_V       =  5,
    MBY_PA_FLAGS_OTR_L4_SCTP_V      =  6,
    MBY_PA_FLAGS_GENERAL_7          =  7,
    MBY_PA_FLAGS_OTR_ESP_V          =  8,
    MBY_PA_FLAGS_WINDOW_PARSE_V     =  9,
    MBY_PA_FLAGS_OTR_HEAD_FRAG_V    = 10,
    MBY_PA_FLAGS_OTR_PAYLOAD_FRAG_V = 11,
    MBY_PA_FLAGS_OTR_MPLS_V         = 21,
    MBY_PA_FLAGS_OTR_L3_V           = 22,
    MBY_PA_FLAGS_OTR_L4_V           = 23,
    MBY_PA_FLAGS_GENERAL_24         = 24,
    MBY_PA_FLAGS_INR_L2_V           = 25,
    MBY_PA_FLAGS_INR_L2_VLAN1       = 26,
    MBY_PA_FLAGS_INR_L2_VLAN2       = 27,
    MBY_PA_FLAGS_INR_L2_V2FIRST     = 28,
    MBY_PA_FLAGS_INR_MPLS_V         = 29,
    MBY_PA_FLAGS_INR_L3_V           = 30,
    MBY_PA_FLAGS_INR_L4_V           = 31,
    MBY_PA_FLAGS_OTR_OPT_FLAGS      = 32,
    MBY_PA_FLAGS_INR_OPT_FLAGS      = 38,
    MBY_PA_FLAGS_GENERAL_2          = 44

} mbyParserFlags;

#endif //  MBY_PA_FLAGS_H
