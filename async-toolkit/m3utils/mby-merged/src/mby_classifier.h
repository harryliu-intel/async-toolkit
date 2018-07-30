// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_CLASSIFIER_H
#define MBY_CLASSIFIER_H

#include "mby_common.h"
#include "mby_mapper.h"
#include "mby_classifier_defines.h"

// Enums:

typedef enum mbyFfuKey8Enum
{
    MBY_FFU_KEY8_MPLS_LABEL5_0 = 0,
    MBY_FFU_KEY8_MPLS_LABEL5_1 = 1,
    MBY_FFU_KEY8_MPLS_LABEL5_2 = 2,
    MBY_FFU_KEY8_MPLS_LABEL5_3 = 3,
    MBY_FFU_KEY8_MPLS_LABEL6_0 = 4,
    MBY_FFU_KEY8_MPLS_LABEL6_1 = 5,
    MBY_FFU_KEY8_MPLS_LABEL6_2 = 6,
    MBY_FFU_KEY8_MPLS_LABEL6_3 = 7,
    MBY_FFU_KEY8_INNER_TTL     = 8,
    MBY_FFU_KEY8_INNER_PROT    = 9,
    MBY_FFU_KEY8_INNER_LEN     = 10,
    MBY_FFU_KEY8_INNER_DS      = 12,
    MBY_FFU_KEY8_OUTER_TTL     = 20,
    MBY_FFU_KEY8_OUTER_PROT    = 21,
    MBY_FFU_KEY8_OUTER_LEN     = 22,
    MBY_FFU_KEY8_OUTER_DS      = 24

} mbyFfuKey8;

typedef enum mbyFfuKey16Enum
{
    MBY_FFU_KEY16_OUTER_VLAN1     = 14,
    MBY_FFU_KEY16_INNER_VLAN1     = 20,
    MBY_FFU_KEY16_MPLS_LABEL1_0   = 24,
    MBY_FFU_KEY16_MPLS_LABEL1_1   = 25,
    MBY_FFU_KEY16_MPLS_LABEL2_0   = 26,
    MBY_FFU_KEY16_MPLS_LABEL2_1   = 27,
    MBY_FFU_KEY16_MPLS_LABEL3_0   = 28,
    MBY_FFU_KEY16_MPLS_LABEL3_1   = 29,
    MBY_FFU_KEY16_MPLS_LABEL4_0   = 30,
    MBY_FFU_KEY16_MPLS_LABEL4_1   = 31

} mbyFfuKey16;

#endif
