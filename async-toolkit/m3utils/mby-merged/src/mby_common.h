// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_COMMON_H
#define MBY_COMMON_H

// Includes:
#include "mby_common_defines.h"

// Basic Data Types:
typedef char                  fm_char;
typedef short                 fm_int16;
typedef int                   fm_int32;
typedef long long             fm_int64;
typedef int                   fm_int;

typedef unsigned char         fm_bool;
typedef unsigned char         fm_byte;
typedef unsigned int          fm_uint;
typedef unsigned short        fm_uint16;
typedef unsigned int          fm_uint32;
typedef unsigned long long    fm_uint64;

typedef char                 *fm_text;

// FM Data Types:
typedef fm_int                fm_status;
typedef unsigned long long    fm_macaddr;

// Constants:
const fm_status  FM_OK   = 0;
const fm_status  FM_FAIL = 1;
const fm_bool    TRUE    = 1;
const fm_bool    FALSE   = 0;

// External function prototypes:

fm_uint32 fmMultiWordBitfieldGet32(const fm_uint32 *array, fm_int hiBit, fm_int loBit);
fm_uint64 fmMultiWordBitfieldGet64(const fm_uint32 *array, fm_int hiBit, fm_int loBit);

void      fmMultiWordBitfieldSet32(fm_uint32 *array, fm_int hiBit, fm_int loBit, fm_uint32 value);
void      fmMultiWordBitfieldSet64(fm_uint32 *array, fm_int hiBit, fm_int loBit, fm_uint64 value);

fm_bool   fmModelIsMulticastMacAddress(fm_macaddr keyMac);
fm_bool   fmModelIsBroadcastMacAddress(fm_macaddr keyMac);

fm_status mbyModelReadCSR(fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
                          const fm_uint32 byte_addr,
                          fm_uint32 *value);

fm_status mbyModelReadCSRMult(fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
                              const fm_uint32 byte_addr,
                              const fm_int len,
                              fm_uint32 *value);
#endif // MBY_COMMON_H
