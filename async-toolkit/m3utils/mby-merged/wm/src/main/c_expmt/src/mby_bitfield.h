// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_common.h"

#ifndef MBY_BITFIELD_H
#define MBY_BITFIELD_H

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
    mbyMultiWordBitfieldGet32(array, regname ## _h_ ## fieldname, \
                             regname ## _l_ ## fieldname)

// Set a named field of 2-32 bits within a >64-bit value
#define FM_ARRAY_SET_FIELD(array, regname, fieldname, fieldvalue) \
    mbyMultiWordBitfieldSet32(array, regname ## _h_ ## fieldname,  \
                             regname ## _l_ ## fieldname, fieldvalue)

// Get a named field of 2-32 bits within an array of 64-bit values
#define FM_ARRAY_GET_FIELD64(array, regname, fieldname)        \
    mbyMultiWordBitfieldGet64(array, regname ## _h_ ## fieldname, \
                             regname ## _l_ ## fieldname)

// Set a named field of 33-64 bits within a >64-bit value
#define FM_ARRAY_SET_FIELD64(array, regname, fieldname, fieldvalue) \
    mbyMultiWordBitfieldSet64(array, regname ## _h_ ## fieldname,    \
                             regname ## _l_ ## fieldname, fieldvalue)

// Extract an unnamed field of 32 or fewer bits from a >64-bit value
#define FM_ARRAY_GET_UNNAMED_FIELD(array, start, len) \
    mbyMultiWordBitfieldGet32((array), (start) + (len) - 1, (start))

// Set an unnamed field of 32 or fewer bits within a >64-bit value
#define FM_ARRAY_SET_UNNAMED_FIELD(array, start, len, value) \
    mbyMultiWordBitfieldSet32((array), (start) + (len) - 1, (start), (value))

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
    mbyMultiWordBitfieldGet32(array, regname ## _b_ ## bitname, \
                             regname ## _b_ ## bitname)

// Set a named field of 1 bit within a >64-bit value
#define FM_ARRAY_SET_BIT(array, regname, bitname, bitvalue)    \
    mbyMultiWordBitfieldSet32(array, regname ## _b_ ## bitname, \
                             regname ## _b_ ## bitname, bitvalue)

// Extract an unnamed bit from a >64-bit value
#define FM_ARRAY_GET_UNNAMED_BIT(array, bit) \
    mbyMultiWordBitfieldGet32((array), (bit), (bit))

// Set an unnamed field of 32 or fewer bits within a >64-bit value
#define FM_ARRAY_SET_UNNAMED_BIT(array, bit, value) \
    mbyMultiWordBitfieldSet32((array), (bit), (bit), (value))

// Extract an unnamed bit from a  64-bit value
#define FM_GET_UNNAMED_BIT64(rvalue, bit) \
    ( (rvalue >> bit) & 1 )

fm_uint32 mbyMultiWordBitfieldGet32(const fm_uint32 *array, fm_int hiBit, fm_int loBit);
fm_uint64 mbyMultiWordBitfieldGet64(const fm_uint32 *array, fm_int hiBit, fm_int loBit);

void      mbyMultiWordBitfieldSet32(fm_uint32 *array, fm_int hiBit, fm_int loBit, fm_uint32 value);
void      mbyMultiWordBitfieldSet64(fm_uint32 *array, fm_int hiBit, fm_int loBit, fm_uint64 value);

#endif // MBY_BITFIELD_H
