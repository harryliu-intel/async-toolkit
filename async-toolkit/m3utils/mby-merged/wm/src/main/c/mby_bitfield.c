// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_bitfield.h"

/** mbyMultiWordBitfieldGet32
 *
 * \desc            Extracts a bitfield, up to 32-bits in size, from
 *                  an arbitrary-precision integer represented as an
 *                  array of 32-bit words.
 *
 * \param[in]       array is an arbitrary-precision integer represented
 *                  as an array of 32-bit words, with array[0] being
 *                  the least-significant 32 bits of the integer.
 *
 * \param[in]       hiBit is the (inclusive) position of the
 *                  most-significant bit in the field to extract.
 *
 * \param[in]       loBit is the (inclusive) position of the
 *                  least-significant bit in the field to extract.
 *
 * \return          the extracted field
 */
fm_uint32 mbyMultiWordBitfieldGet32(const fm_uint32 *array,
                                    fm_int           hiBit,
                                    fm_int           loBit)
{
    fm_uint32 result         = 0;
    fm_int    resultPosition = 0;
    fm_int    word           = loBit / 32;
    fm_int    relativeLoBit  = loBit % 32;
    fm_int    relativeHiBit  = relativeLoBit + (hiBit - loBit);
    fm_uint32 mask           = ( 2 << (hiBit - loBit) ) - 1;

    do
    {
        result         |= (array[word++] >> relativeLoBit) << resultPosition;
        resultPosition += (32 - relativeLoBit);
        relativeHiBit  -= 32;
        relativeLoBit   = 0;
    }
    while (relativeHiBit >= 0);

    return result & mask;

}

/** mbyMultiWordBitfieldSet32
 *
 * \desc            Modifies a bitfield, up to 32-bits in size, in
 *                  an arbitrary-precision integer represented as an
 *                  array of 32-bit words.
 *
 * \param[in]       array is an arbitrary-precision integer represented
 *                  as an array of 32-bit words, with array[0] being
 *                  the least-significant 32 bits of the integer.
 *
 * \param[in]       hiBit is the (inclusive) position of the
 *                  most-significant bit in the field to set.
 *
 * \param[in]       loBit is the (inclusive) position of the
 *                  least-significant bit in the field to set.
 *
 * \param[in]       value is the new contents of the bitfield.
 *
 * \return          None
 */
void mbyMultiWordBitfieldSet32(fm_uint32 *array,
                               fm_int     hiBit,
                               fm_int     loBit,
                               fm_uint32  value)
{
    fm_int valuePosition = 0;
    fm_int word          = loBit / 32;
    fm_int relativeLoBit = loBit % 32;
    fm_int relativeHiBit = relativeLoBit + (hiBit - loBit);

    do
    {
        fm_uint32 mask = (relativeHiBit < 31 ? (2 << relativeHiBit) : 0 ) - 1;
        mask      >>= relativeLoBit;
        mask      <<= relativeLoBit;
        array[word] = ( (array[word] & ~mask) |
                       ( ( (value >> valuePosition) << relativeLoBit ) & mask ) );
        valuePosition += (32 - relativeLoBit);
        relativeHiBit -= 32;
        relativeLoBit  = 0;
        word++;
    }
    while (relativeHiBit >= 0);

}

/** mbyMultiWordBitfieldGet64
 *
 * \desc            Extracts a bitfield, up to 64-bits in size, from
 *                  an arbitrary-precision integer represented as an
 *                  array of 32-bit words.
 *
 * \param[in]       array is an arbitrary-precision integer represented
 *                  as an array of 32-bit words, with array[0] being
 *                  the least-significant 32 bits of the integer.
 *
 * \param[in]       hiBit is the (inclusive) position of the
 *                  most-significant bit in the field to extract.
 *
 * \param[in]       loBit is the (inclusive) position of the
 *                  least-significant bit in the field to extract.
 *
 * \return          the extracted field
 *
 *****************************************************************************/
fm_uint64 mbyMultiWordBitfieldGet64(const fm_uint32 *array,
                                    fm_int           hiBit,
                                    fm_int           loBit)
{
    fm_uint64 result         = 0;
    fm_int    resultPosition = 0;
    fm_int    word           = loBit / 32;
    fm_int    relativeLoBit  = loBit % 32;
    fm_int    relativeHiBit  = relativeLoBit + (hiBit - loBit);
    fm_uint64 mask           = ( FM_LITERAL_U64(2) << (hiBit - loBit) ) - 1;

    do
    {
        result |= ( ( (fm_uint64) array[word++] >> relativeLoBit )
                   << resultPosition );
        resultPosition += (32 - relativeLoBit);
        relativeHiBit  -= 32;
        relativeLoBit   = 0;
    }
    while (relativeHiBit >= 0);

    return result & mask;

}

/** mbyMultiWordBitfieldSet64
 *
 * \desc            Modifies a bitfield, up to 64-bits in size, in
 *                  an arbitrary-precision integer represented as an
 *                  array of 32-bit words.
 *
 * \param[in]       array is an arbitrary-precision integer represented
 *                  as an array of 32-bit words, with array[0] being
 *                  the least-significant 32 bits of the integer.
 *
 * \param[in]       hiBit is the (inclusive) position of the
 *                  most-significant bit in the field to set.
 *
 * \param[in]       loBit is the (inclusive) position of the
 *                  least-significant bit in the field to set.
 *
 * \param[in]       value is the new contents of the bitfield.
 *
 * \return          None
 */
void mbyMultiWordBitfieldSet64(fm_uint32 *array,
                              fm_int     hiBit,
                              fm_int     loBit,
                              fm_uint64  value)
{
    fm_int valuePosition = 0;
    fm_int word          = loBit / 32;
    fm_int relativeLoBit = loBit % 32;
    fm_int relativeHiBit = relativeLoBit + (hiBit - loBit);

    do
    {
        fm_uint32 mask = (relativeHiBit < 31 ? (2 << relativeHiBit) : 0 ) - 1;
        mask      >>= relativeLoBit;
        mask      <<= relativeLoBit;
        array[word] = ( (array[word] & ~mask) |
                       ( ( (fm_uint32) (value >> valuePosition)
                         << relativeLoBit ) & mask ) );
        valuePosition += (32 - relativeLoBit);
        relativeHiBit -= 32;
        relativeLoBit  = 0;
        word++;
    }
    while (relativeHiBit >= 0);
}
