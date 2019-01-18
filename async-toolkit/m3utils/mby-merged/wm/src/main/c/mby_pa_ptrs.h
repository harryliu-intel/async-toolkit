// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_PA_PTRS_H
#define MBY_PA_PTRS_H

typedef enum mbyParserPtrsIndexEnum
{
    MBY_PA_PTRS_NOP          = 0,
    MBY_PA_PTRS_OTR_MPLS_PTR = 1,
    MBY_PA_PTRS_OTR_L3_PTR   = 2,
    MBY_PA_PTRS_OTR_L4_PTR   = 3,
    MBY_PA_PTRS_INR_L2_PTR   = 4,
    MBY_PA_PTRS_INR_MPLS_PTR = 5,
    MBY_PA_PTRS_INR_L3_PTR   = 6,
    MBY_PA_PTRS_INR_L4_PTR   = 7,
    MBY_PA_PTRS_ESP_PTR      = 7 // Same location as INR_L4

} mbyParserPtrsIndex;


#endif // MBY_PA_PTRS_H