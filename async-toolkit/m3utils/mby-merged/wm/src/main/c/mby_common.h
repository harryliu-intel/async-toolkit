// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_COMMON_H
#define MBY_COMMON_H

// Includes:

#include "fm_types.h"    // basic data types

// External function prototypes:

fm_bool isBroadcastMacAddress(fm_uint64 const addr);
fm_bool   isUnicastMacAddress(fm_uint64 const addr);
fm_bool isMulticastMacAddress(fm_uint64 const addr);

fm_int fmRand(void);

#endif // MBY_COMMON_H
