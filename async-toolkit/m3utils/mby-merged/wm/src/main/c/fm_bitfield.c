/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:           fm_bitfield.c
 * Creation Date:  March 19, 2007
 * Description:    Functions to manipulate bitfields
 *
 * INTEL CONFIDENTIAL
 * Copyright 2007 - 2011 Intel Corporation. All Rights Reserved. 
 *
 * The source code contained or described herein and all documents related
 * to the source code ("Material") are owned by Intel Corporation or its
 * suppliers or licensors. Title to the Material remains with Intel
 * Corporation or its suppliers and licensors. The Material contains trade
 * secrets and proprietary and confidential information of Intel or its
 * suppliers and licensors. The Material is protected by worldwide copyright
 * and trade secret laws and treaty provisions. No part of the Material may
 * be used, copied, reproduced, modified, published, uploaded, posted,
 * transmitted, distributed, or disclosed in any way without Intel's prior
 * express written permission.
 *
 * No license under any patent, copyright, trade secret or other intellectual
 * property right is granted to or conferred upon you by disclosure or 
 * delivery of the Materials, either expressly, by implication, inducement,
 * estoppel or otherwise. Any license under such intellectual property rights
 * must be express and approved by Intel in writing.
 *****************************************************************************/

#include "fm_bitfield.h"


/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

/*****************************************************************************
 * Global Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** fmMultiWordBitfieldGet32
 * \ingroup intBitfield
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
 *
 *****************************************************************************/
fm_uint32 fmMultiWordBitfieldGet32(const fm_uint32 *array,
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

}   /* end fmMultiWordBitfieldGet32 */




/*****************************************************************************/
/** fmMultiWordBitfieldSet32
 * \ingroup intBitfield
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
 *
 *****************************************************************************/
void fmMultiWordBitfieldSet32(fm_uint32 *array,
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

}   /* end fmMultiWordBitfieldSet32 */




/*****************************************************************************/
/** fmMultiWordBitfieldGet64
 * \ingroup intBitfield
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
fm_uint64 fmMultiWordBitfieldGet64(const fm_uint32 *array,
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

}   /* end fmMultiWordBitfieldGet64 */




/*****************************************************************************/
/** fmMultiWordBitfieldSet64
 * \ingroup intBitfield
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
 *
 *****************************************************************************/
void fmMultiWordBitfieldSet64(fm_uint32 *array,
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

}   /* end fmMultiWordBitfieldSet64 */




/*****************************************************************************/
/** fmMulti64BitWordBitfieldGet32
 * \ingroup intBitfield
 *
 * \desc            Extracts a bitfield, up to 32-bits in size, from
 *                  an arbitrary-precision integer represented as an
 *                  array of 64-bit words.
 *
 * \param[in]       array is an arbitrary-precision integer represented
 *                  as an array of 64-bit words, with array[0] being
 *                  the least-significant 64 bits of the integer.
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
fm_uint32 fmMulti64BitWordBitfieldGet32(const fm_uint64 *array,
                                        fm_int           hiBit,
                                        fm_int           loBit)
{
    fm_uint64 result;

    result = fmMulti64BitWordBitfieldGet64(array, hiBit, loBit);

    return (fm_uint32) result;

}   /* end fmMulti64BitWordBitfieldGet32 */




/*****************************************************************************/
/** fmMulti64BitWordBitfieldSet32
 * \ingroup intBitfield
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
 *
 *****************************************************************************/
void fmMulti64BitWordBitfieldSet32(fm_uint64 *array,
                                   fm_int     hiBit,
                                   fm_int     loBit,
                                   fm_uint32  value)
{
    fmMulti64BitWordBitfieldSet64( array, hiBit, loBit, (fm_uint64) value );

}   /* end fmMulti64BitWordBitfieldSet32 */




/*****************************************************************************/
/** fmMulti64BitWordBitfieldGet64
 * \ingroup intBitfield
 *
 * \desc            Extracts a bitfield, up to 64-bits in size, from
 *                  an arbitrary-precision integer represented as an
 *                  array of 64-bit words.
 *
 * \param[in]       array is an arbitrary-precision integer represented
 *                  as an array of 64-bit words, with array[0] being
 *                  the least-significant 64 bits of the integer.
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
fm_uint64 fmMulti64BitWordBitfieldGet64(const fm_uint64 *array,
                                        fm_int           hiBit,
                                        fm_int           loBit)
{
    fm_uint64 result         = 0;
    fm_int    resultPosition = 0;
    fm_int    word           = loBit / 64;
    fm_int    relativeLoBit  = loBit % 64;
    fm_int    relativeHiBit  = relativeLoBit + (hiBit - loBit);
    fm_uint64 mask           = ( FM_LITERAL_U64(2) << (hiBit - loBit) ) - 1;

    do
    {
        result |= ( ( array[word++] >> relativeLoBit )
                   << resultPosition );
        resultPosition += (64 - relativeLoBit);
        relativeHiBit  -= 64;
        relativeLoBit   = 0;
    }
    while (relativeHiBit >= 0);

    return result & mask;

}   /* end fmMulti64BitWordBitfieldGet64 */




/*****************************************************************************/
/** fmMulti64BitWordBitfieldSet64
 * \ingroup intBitfield
 *
 * \desc            Modifies a bitfield, up to 64-bits in size, in
 *                  an arbitrary-precision integer represented as an
 *                  array of 64-bit words.
 *
 * \param[in]       array is an arbitrary-precision integer represented
 *                  as an array of 64-bit words, with array[0] being
 *                  the least-significant 64 bits of the integer.
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
 *
 *****************************************************************************/
void fmMulti64BitWordBitfieldSet64(fm_uint64 *array,
                                   fm_int     hiBit,
                                   fm_int     loBit,
                                   fm_uint64  value)
{
    fm_int    valuePosition = 0;
    fm_int    word          = loBit / 64;
    fm_int    relativeLoBit = loBit % 64;
    fm_int    relativeHiBit = relativeLoBit + (hiBit - loBit);
    fm_uint64 mask;

    do
    {
        if (relativeHiBit < 63)
        {
            mask = (FM_LITERAL_64(2) << relativeHiBit) - FM_LITERAL_64(1);
        }
        else
        {
            mask = -FM_LITERAL_64(1);
        }

        mask      >>= relativeLoBit;
        mask      <<= relativeLoBit;
        array[word] = ( (array[word] & ~mask) |
                       ( ( (value >> valuePosition)
                         << relativeLoBit ) & mask ) );
        valuePosition += (64 - relativeLoBit);
        relativeHiBit -= 64;
        relativeLoBit  = 0;
        word++;
    }
    while (relativeHiBit >= 0);

}   /* end fmMulti64BitWordBitfieldSet64 */



#if 0 /* Need to fix for gcc 6.1.0 */
/*****************************************************************************/
/** fmSignExtendInt
 * \ingroup intBitfield
 *
 * \desc            Sign-extends an n-bit integer to a machine-native
 *                  integer.
 *
 * \param[in]       value is the integer to sign-extend.
 *
 * \param[in]       signifigantBits is the number of significant bits in value.
 *                  signifigantBits must be between 1 and # of machine bits,
 *                  inclusive.
 *
 * \return          sign-extended version of value.
 *
 *****************************************************************************/
fm_int fmSignExtendInt(fm_int value, fm_uint signifigantBits)
{
    /**************************************************
     * The most obvious thing to do is this:
     *
     * fm_uint signBits = sizeof(fm_int) * CHAR_BIT - signifigantBits;
     * return ((value << signBits) >> signBits);
     *
     * However, that assumes that right shift on a signed
     * integer sign extends, which according to "The C
     * Programming Language, Second Edition", Section 2.9,
     * page 49, is not guaranteed to be true.  Although
     * I'll bet it is true on any modern processor we
     * are likely to target, let's do it the slightly
     * more cumbersome way that doesn't make this assumption.
     **************************************************/

    fm_int signBit = ( ( value >> (signifigantBits - 1) ) & 1 );

    if (signBit)
    {
        /**************************************************
         * This doesn't work:
         * return (value | (-1 << signifigantBits));
         * when signifigantBits is 32 (on a 32-bit machine).
         **************************************************/
        return value | ( ( -1 << (signifigantBits - 1) ) << 1 );
    }
    else
    {
        return value;
    }

}   /* end fmSignExtendInt */




/*****************************************************************************/
/** fmSignExtendInt32
 * \ingroup intBitfield
 *
 * \desc            Sign-extends an n-bit integer to a 32-bit integer.
 *
 * \param[in]       value is the integer to sign-extend.
 *
 * \param[in]       signifigantBits is the number of significant bits in value.
 *                  signifigantBits must be between 1 and 32, inclusive.
 *
 * \return          sign-extended version of value.
 *
 *****************************************************************************/
fm_int32 fmSignExtendInt32(fm_int32 value, fm_uint signifigantBits)
{
    /**************************************************
     * The most obvious thing to do is this:
     *
     * fm_uint signBits = sizeof(fm_int32) * CHAR_BIT - signifigantBits;
     * return ((value << signBits) >> signBits);
     *
     * However, that assumes that right shift on a signed
     * integer sign extends, which according to "The C
     * Programming Language, Second Edition", Section 2.9,
     * page 49, is not guaranteed to be true.  Although
     * I'll bet it is true on any modern processor we
     * are likely to target, let's do it the slightly
     * more cumbersome way that doesn't make this assumption.
     **************************************************/

    fm_int32 signBit = ( ( value >> (signifigantBits - 1) ) & 1 );

    if (signBit)
    {
        /**************************************************
         * This doesn't work:
         * return (value | (-1 << signifigantBits));
         * when signifigantBits is 32.
         **************************************************/
        return value | ( ( -1 << (signifigantBits - 1) ) << 1 );
    }
    else
    {
        return value;
    }

}   /* end fmSignExtendInt32 */




/*****************************************************************************/
/** fmSignExtendInt64
 * \ingroup intBitfield
 *
 * \desc            Sign-extends an n-bit integer to a 64-bit integer.
 *
 * \param[in]       value is the integer to sign-extend.
 *
 * \param[in]       signifigantBits is the number of significant bits in value.
 *                  signifigantBits must be between 1 and 64, inclusive.
 *
 * \return          sign-extended version of value.
 *
 *****************************************************************************/
fm_int64 fmSignExtendInt64(fm_int64 value, fm_uint signifigantBits)
{
    /**************************************************
     * The most obvious thing to do is this:
     *
     * fm_uint signBits = sizeof(fm_int64) * CHAR_BIT - signifigantBits;
     * return ((value << signBits) >> signBits);
     *
     * However, that assumes that right shift on a signed
     * integer sign extends, which according to "The C
     * Programming Language, Second Edition", Section 2.9,
     * page 49, is not guaranteed to be true.  Although
     * I'll bet it is true on any modern processor we
     * are likely to target, let's do it the slightly
     * more cumbersome way that doesn't make this assumption.
     **************************************************/

    fm_int64 signBit = ( ( value >> (signifigantBits - 1) ) & 1 );

    if (signBit)
    {
        /**************************************************
         * This doesn't work:
         * return (value | (FM_LITERAL_64(-1) << signifigantBits));
         * when signifigantBits is 64.
         **************************************************/
        return value | ( ( FM_LITERAL_64(-1) << (signifigantBits - 1) ) << 1 );
    }
    else
    {
        return value;
    }

}   /* end fmSignExtendInt64 */
#endif

/*****************************************************************************/
/** fmGenerateCAMKey
 * \ingroup intBitfield
 *
 * \desc            Convert a mask/value pair into a CAM Key/KeyInvert pair.
 *                  Operates on arrays of 32-bit words in order to support
 *                  keys wider than 32 bits.
 *
 * \note            The Key/KeyInvert method used in Alta provides:         \lb
 *                      * Key = ~KeyInvert for do care bits                 \lb
 *                      * Key = KeyInvert = 1 for don't care bits           \lb
 *                      * Key = KeyInvert = 0 to disable CAM entry
 *                                                                      \lb\lb
 *                  The Key/KenInvert values can be derived from value/mask:
 *                      * Key = (Value & Mask) | ~Mask
 *                      * KeyInvert = ~(Value & Mask) 
 *                                                                      \lb\lb
 *                  The conversion is illustrated here:                     \lb
 *                      Value                        = 1 0 1 0              \lb
 *                      Mask                         = 1 1 0 0              \lb
 *                      (Value & Mask)               = 1 0 0 0              \lb
 *                      ~Mask                        = 0 0 1 1              \lb
 *                      Key = (Value & Mask) | ~Mask = 1 0 1 1              \lb
 *                      KeyInvert = ~(Value & Mask)  = 0 1 1 1
 *
 * \param[in]       value is an array, size in length, of 32-bit words
 *                  comprising the value that the CAM entry should match on.
 *
 * \param[in]       mask is an array, size in length, of 32-bit bit words
 *                  that identify which bits in value should be used by the 
 *                  CAM for matching.
 *
 * \param[out]      key is an array, size in length, of 32-bit words, into 
 *                  which this function will place the resulting CAM key value.
 *
 * \param[out]      keyInvert is an array, size in length, of 32-bit words,
 *                  into which this function will place the resulting CAM 
 *                  keyinvert value.
 *
 * \param[in]       size is the number of 32-bit words in each of the
 *                  array arguments.
 *
 * \return          None.
 *
 *****************************************************************************/
void fmGenerateCAMKey(fm_uint32 *value,
                      fm_uint32 *mask,
                      fm_uint32 *key,
                      fm_uint32 *keyInvert,
                      fm_int     size)
{
    fm_int    i;
    fm_uint32 maskedValue;
    
    for (i = 0 ; i < size ; i++)
    {
        maskedValue  = value[i] & mask[i];
        key[i]       = maskedValue | ~mask[i];
        keyInvert[i] = ~maskedValue;
    }
    
    return;
    
}   /* end fmGenerateCAMKey */



/*****************************************************************************/
/** fmGenerateCAMValueMask
 * \ingroup intBitfield
 *
 * \desc            Convert a Key/KeyInvert pair to a mask/value pair.
 *                  Operates on arrays of 32-bit words in order to support
 *                  keys wider than 32 bits.
 *
 * \note            The Key/KeyInvert method used in Alta provides:         \lb
 *                      * Key = ~KeyInvert for do care bits                 \lb
 *                      * Key = KeyInvert = 1 for don't care bits           \lb
 *                      * Key = KeyInvert = 0 to disable CAM entry
 *                                                                      \lb\lb
 *                  The conversion is illustrated here:                     \lb
 *                      Key                             = 1 0 1 1           \lb
 *                      KeyInvert                       = 0 1 1 1           \lb
 *                      Value = Key & (Key ^ KeyInvert) = 1 0 0 0           \lb
 *                      Mask = Key ^ KeyInvert          = 1 1 0 0
 *                                                                      \lb\lb
 *                  Using these operations, Value will always be 0 for the 
 *                  case where Key = KeyInvert = 1, even though it is possible
 *                  for Value to have been a 1. This is ok because Mask will
 *                  always be 0 for this case.
 *
 * \param[in]       key is an array, size in length, of 32-bit words,
 *                  containing the CAM Key value.
 *
 * \param[in]       keyInvert is an array, size in length, of 32-bit words,
 *                  containing the CAM KeyInvert value.
 *
 * \param[out]      value is an array, size in length, of 32-bit words
 *                  into which this function will put the equivalent match
 *                  value.
 *
 * \param[out]      mask is an array, size in length, of 32-bit bit words
 *                  into which this function will put the equivalent match
 *                  mask.
 *
 * \param[in]       size is the number of 32-bit words in each of the
 *                  array arguments.
 *
 * \return          None.
 *
 *****************************************************************************/
void fmGenerateCAMValueMask(fm_uint32 *key,
                            fm_uint32 *keyInvert,
                            fm_uint32 *value,
                            fm_uint32 *mask,
                            fm_int     size)
{
    fm_int    i;
    
    if ( IsCamKeyValid(key, keyInvert, size) )
    {
        for (i = 0 ; i < size ; i++)
        {
            mask[i]  = key[i] ^ keyInvert[i];
            value[i] = key[i] & mask[i];
        }
    }
    else
    {
        for (i = 0 ; i < size ; i++)
        {
            mask[i]  = 0;
            value[i] = 0;
        }
    }
    
    return;
    
}   /* end fmGenerateCAMValueMask */



/*****************************************************************************/
/** IsCamKeyValid
 * \ingroup intAddr
 *
 * \desc            Determine if a L2L sweeper CAM rule is invalid
 *
 * \note            The Key/KeyInvert method used in Alta provides:         \lb
 *                      * Key = ~KeyInvert for do care bits                 \lb
 *                      * Key = KeyInvert = 1 for don't care bits           \lb
 *                      * Key = KeyInvert = 0 to disable CAM entry
 *
 * \param[in]       key is an array, size in length, of 32-bit words,
 *                  containing the CAM Key value.
 *
 * \param[in]       keyInvert is an array, size in length, of 32-bit words,
 *                  containing the CAM KeyInvert value.
 *
 * \param[in]       size is the number of 32-bit words in each of the
 *                  array arguments.
 *
 * \return          TRUE if the rule is valid
 * \return          FALSE if the rule is invalid
 *
 *****************************************************************************/
fm_bool IsCamKeyValid(fm_uint32 *key,
                      fm_uint32 *keyInvert,
                      fm_int     size)
{
    fm_int  i;
    fm_bool result = TRUE;
    
    for (i = 0 ; i < size ; i++)
    {
        if ( ~(key[i] | keyInvert[i]) != 0 )
        {
            result = FALSE;
            break;
        }
    }
    
    return result;
    
}   /* end IsCamKeyValid */

