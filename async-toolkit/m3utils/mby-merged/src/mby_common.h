// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_COMMON_H
#define MBY_COMMON_H

// Macros:
#define FM_LITERAL_U64(x) (x ## ULL)

// Get a named field of 2-32 bits within a 32-bit value
#define FM_GET_FIELD(rvalue, regname, fieldname) \
    ( (rvalue >> regname ## _l_ ## fieldname) &  \
      ( ( 2U << ((regname ## _h_ ## fieldname) - (regname ## _l_ ## fieldname)) ) - 1 ) )

// Set a named field of 2-32 bits within a 32-bit value
#define FM_SET_FIELD(lvalue, regname, fieldname, fieldvalue) \
    (lvalue ^= ( ( (lvalue >> regname ## _l_ ## fieldname) ^ fieldvalue ) & \
                 ( ( 2U << ((regname ## _h_ ## fieldname) - (regname ## _l_ ## fieldname)) ) - 1 ) ) \
               << regname ## _l_ ## fieldname)

// Get a named field of 2-64 bits within a 64-bit value.
#define FM_GET_FIELD64(rvalue, regname, fieldname) \
    ( (rvalue >> regname ## _l_ ## fieldname) & \
      ( ( FM_LITERAL_U64(2) << ((regname ## _h_ ## fieldname) - (regname ## _l_ ## fieldname)) ) - \
      FM_LITERAL_U64(1) ) )


// Set a named field of 2-64 bits within a 64-bit value
#define FM_SET_FIELD64(lvalue, regname, fieldname, fieldvalue) \
    (lvalue ^= ( ( (lvalue >> regname ## _l_ ## fieldname) ^ fieldvalue ) & \
                ( ( FM_LITERAL_U64(2) << (regname ## _h_ ## fieldname - regname ## _l_ ## fieldname) ) - \
                 FM_LITERAL_U64(1) ) ) << regname ## _l_ ## fieldname)

// Extract a field of 32 or fewer bits from an unnamed 32-bit value
#define FM_GET_UNNAMED_FIELD(lvalue, start, len) \
    ((lvalue >> (start)) & ((1 << (len)) - 1))

// Set a field of 32 or fewer bits for an unnamed 32-bit value
#define FM_SET_UNNAMED_FIELD(lvalue, start, len, value) \
{ \
    lvalue &= ~(((1 << (len)) - 1) << (start)); \
    lvalue |= ((value) & ((1 << (len)) - 1)) << (start); \
}

// Extract a field of 64 or fewer bits from an unnamed 64-bit value.
#define FM_GET_UNNAMED_FIELD64(lvalue, start, len) \
    ((lvalue >> (start)) & ((FM_LITERAL_U64(1) << (len)) - FM_LITERAL_U64(1))) 

// Set a field of 64 or fewer bits for an unnamed 64-bit value
#define FM_SET_UNNAMED_FIELD64(lvalue, start, len, value) \
{ \
    lvalue &= ~(((FM_LITERAL_U64(1) << (len)) - FM_LITERAL_U64(1)) << (start)); \
    lvalue |= ((value) & ((FM_LITERAL_U64(1) << (len)) - FM_LITERAL_U64(1))) << (start); \
}

// Get a named field of 2-32 bits within a >64-bit value
#define FM_ARRAY_GET_FIELD(array, regname, fieldname)            \
    fmMultiWordBitfieldGet32(array, regname ## _h_ ## fieldname, \
                             regname ## _l_ ## fieldname)

// Set a named field of 2-32 bits within a >64-bit value
#define FM_ARRAY_SET_FIELD(array, regname, fieldname, fieldvalue) \
    fmMultiWordBitfieldSet32(array, regname ## _h_ ## fieldname,  \
                             regname ## _l_ ## fieldname, fieldvalue)

// Get a named field of 2-32 bits within an array of 64-bit values
#define FM_ARRAY_GET_FIELD64(array, regname, fieldname)        \
    fmMultiWordBitfieldGet64(array, regname ## _h_ ## fieldname, \
                             regname ## _l_ ## fieldname)

// Set a named field of 33-64 bits within a >64-bit value
#define FM_ARRAY_SET_FIELD64(array, regname, fieldname, fieldvalue) \
    fmMultiWordBitfieldSet64(array, regname ## _h_ ## fieldname,    \
                             regname ## _l_ ## fieldname, fieldvalue)

// Extract an unnamed field of 32 or fewer bits from a >64-bit value
#define FM_ARRAY_GET_UNNAMED_FIELD(array, start, len) \
    fmMultiWordBitfieldGet32((array), (start) + (len) - 1, (start))

// Set an unnamed field of 32 or fewer bits within a >64-bit value
#define FM_ARRAY_SET_UNNAMED_FIELD(array, start, len, value) \
    fmMultiWordBitfieldSet32((array), (start) + (len) - 1, (start), (value))

// Get a named field of 1 bit within a 32-bit value
#define FM_GET_BIT(rvalue, regname, bitname) \
    ( (rvalue >> regname ## _b_ ## bitname) & 1 )

// Set a named field of 1 bit within a 32-bit value
#define FM_SET_BIT(lvalue, regname, bitname, bitvalue)          \
    ( lvalue = ( lvalue & ~(1 << regname ## _b_ ## bitname) ) | \
               ( (bitvalue & 1) << regname ## _b_ ## bitname ) )

// Get a named field of 1 bit within a 64-bit value
#define FM_GET_BIT64(rvalue, regname, bitname) \
    ( (rvalue >> regname ## _b_ ## bitname) & 1 )

// Set a named field of 1 bit within a 64-bit value.
#define FM_SET_BIT64(lvalue, regname, bitname, bitvalue)                      \
    ( lvalue = ( lvalue & ~(FM_LITERAL_U64(1) << regname ## _b_ ## bitname) ) \
               | ( ( bitvalue & FM_LITERAL_U64(1) ) << regname ## _b_ ## bitname ) )

// Get a named field of 1 bit within a >64-bit value
#define FM_ARRAY_GET_BIT(array, regname, bitname)              \
    fmMultiWordBitfieldGet32(array, regname ## _b_ ## bitname, \
                             regname ## _b_ ## bitname)

// Set a named field of 1 bit within a >64-bit value
#define FM_ARRAY_SET_BIT(array, regname, bitname, bitvalue)    \
    fmMultiWordBitfieldSet32(array, regname ## _b_ ## bitname, \
                             regname ## _b_ ## bitname, bitvalue)

// Extract an unnamed bit from a >64-bit value
#define FM_ARRAY_GET_UNNAMED_BIT(array, bit) \
    fmMultiWordBitfieldGet32((array), (bit), (bit))

// Set an unnamed field of 32 or fewer bits within a >64-bit value
#define FM_ARRAY_SET_UNNAMED_BIT(array, bit, value) \
    fmMultiWordBitfieldSet32((array), (bit), (bit), (value))

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
