// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_COMMON_H
#define MBY_COMMON_H

// Macros:

// MAC address utilities
#define mbyModelIsBroadcastMacAddress(addr)                                     \
    ( ( (addr) == FM_LITERAL_U64(0xFFFFFFFFFFFF) ) ? TRUE : FALSE )

#define mbyModelIsUnicastMacAddress(addr)                                       \
         ( ((addr) & FM_LITERAL_U64(0x010000000000)) == 0 )

#define mbyModelIsMulticastMacAddress(addr)                                     \
         ( ( ((addr) & FM_LITERAL_U64(0x010000000000)) != 0 ) &&               \
           !mbyModelIsBroadcastMacAddress(addr) )

// Defines:

#define MBY_REGISTER_ARRAY_SIZE  0x1800000

#define MBY_N_PARSER_KEYS        80
#define MBY_N_PARSER_FLAGS       48
#define MBY_N_PARSER_PTRS         8

#define MBY_FFU_N_KEY8           64
#define MBY_FFU_N_KEY16          32
#define MBY_FFU_N_KEY32          16
#define MBY_FFU_N_KEYS           ( MBY_FFU_N_KEY8 + MBY_FFU_N_KEY16   + MBY_FFU_N_KEY32   )
#define MBY_FFU_N_HASH_KEYS      ( MBY_FFU_N_KEY8 + MBY_FFU_N_KEY16*2 + MBY_FFU_N_KEY32*4 )
#define MBY_FFU_N_ACT24          16
#define MBY_FFU_N_ACT4           23
#define MBY_FFU_N_ACT1           24
#define MBY_FFU_N_REMAP_ACTIONS   8

#define MBY_PROT_TCP              6
#define MBY_PROT_UDP             17
#define MBY_PROT_ICMPv4           1
#define MBY_PROT_ICMPv6          58
#define MBY_PROT_IGMP             2

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
