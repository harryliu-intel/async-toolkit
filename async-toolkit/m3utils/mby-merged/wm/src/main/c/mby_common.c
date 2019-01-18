// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include <stdlib.h> // rand_r()

#include "mby_common.h"

// Support functions to build the MBY model as a standalone library:

inline fm_bool isBroadcastMacAddress(fm_uint64 const addr)
{
    fm_bool is_broad = (addr) == FM_LITERAL_U64(0xFFFFFFFFFFFF);
    return is_broad;
}

inline fm_bool isUnicastMacAddress(fm_uint64 const addr)
{
    fm_bool is_uni = ((addr) &  FM_LITERAL_U64(0x010000000000)) == 0;
    return is_uni;
}

inline fm_bool isMulticastMacAddress(fm_uint64 const addr)
{
    fm_bool is_multi = !isUnicastMacAddress(addr) && !isBroadcastMacAddress(addr);
    return is_multi;
}

fm_int fmRand(void)
{
    fm_uint rand_seed = 1;

    fm_int  rand_result = rand_r(&rand_seed);

    return rand_result;
}
