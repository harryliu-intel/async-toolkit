/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_bitarray.h
 * Creation Date:   Nov 10, 2006
 * Description:     This file contains definitions used to support bit arrays
 *
 * INTEL CONFIDENTIAL
 * Copyright 2006 - 2011 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM_BITARRAY_H
#define __FM_FM_BITARRAY_H


/**************************************************/
/** \ingroup typeStruct
 *  Used as the data type for the ''FM_PORT_MASK_WIDE'' 
 *  port attribute (and internally by the API for
 *  other purposes). It is a variable length array 
 *  of bits (physically, the array is an array of 
 *  words with multiple bits packed into each word).
 *                                              \lb\lb
 *  Structure memebers that are documented as "for
 *  internal use only" should not be accessed directly
 *  by the application. Instead, use the following 
 *  functions:
 *                                              \lb\lb
 *  ''fmCreateBitArray'' to intialize the bit array
 *  structure, including allocation of memory to
 *  hold the actual bit array data.
 *                                              \lb\lb
 *  ''fmDeleteBitArray'' to destroy the bit array,
 *  deallocating the memory that holds the actual
 *  bit array data.
 *                                              \lb\lb
 *  ''fmSetBitArrayBit'' or ''fmSetBitArrayBlock''
 *  to initialize bit values in the bit array.
 *                                              \lb\lb
 *  ''fmGetBitArrayBit'' to get the value of a
 *  bit in a bit array.
 **************************************************/
typedef struct _fm_bitArray
{
    /** For internal use only. */
    fm_int   bitsPerWord;
    
    /** Read-only: The number of bits held in the array (which does not 
     *  necessarily reflect the size of physical memory consumed by the 
     *  array). */
    fm_int   bitCount;
    
    /** For internal use only. */
    fm_int   wordCount;
    
    /** Read-only: The number of non-zero bits in the array. This is 
     *  useful for quickly determining if an array is non-zero without 
     *  having to scan the entire array. */
    fm_int   nonZeroBitCount;
    
    /** For internal use only. */
    fm_uint *bitArrayData;

} fm_bitArray;


/*****************************************************************************
 * fmCreateBitArray
 *
 * Description: Creates and Initializes a bit array
 *
 * Arguments:   bitArray                pointer to the bit array
 *
 *              bitcount                number of bits in the array
 *
 * Returns:     Fulcrum API status code
 *
 *****************************************************************************/
fm_status fmCreateBitArray(fm_bitArray *bitArray, fm_int bitcount);


/*****************************************************************************
 * fmResizeBitArray
 *
 * Description: Resizes and re-initializes an existing bit array
 *
 * Arguments:   bitArray                pointer to the bit array
 *
 *              newbitcount             new number of bits in the array
 *
 * Returns:     Fulcrum API status code
 *
 *****************************************************************************/
fm_status fmResizeBitArray(fm_bitArray *bitArray, fm_int newbitcount);


/*****************************************************************************
 * fmDeleteBitArray
 *
 * Description: Deletes a bit array
 *
 * Arguments:   bitArray                pointer to the bit array
 *
 * Returns:     Fulcrum API status code
 *
 *****************************************************************************/
fm_status fmDeleteBitArray(fm_bitArray *bitArray);


/*****************************************************************************
 * fmSetBitArrayBit
 *
 * Description: sets a bit to a specified boolean value (TRUE or FALSE)
 *
 * Arguments:   bitArray                pointer to the bit array
 *
 *              bitNumber               bit number (0 to bitCount-1)
 *
 *              bitValue                TRUE or FALSE
 *
 * Returns:     Fulcrum API status code
 *
 *****************************************************************************/
fm_status fmSetBitArrayBit(fm_bitArray *bitArray,
                           fm_int       bitNumber,
                           fm_bool      bitValue);


/*****************************************************************************
 * fmSetBitArrayBlock
 *
 * Description: sets a block of bits to a specified boolean value (TRUE or FALSE)
 *
 * Arguments:   bitArray                pointer to the bit array
 *
 *              bitNumber               bit number (0 to bitCount-1)
 *
 *              numBits                 Number of bits in block.
 *
 *              bitValue                TRUE or FALSE
 *
 * Returns:     Fulcrum API status code
 *
 *****************************************************************************/
fm_status fmSetBitArrayBlock(fm_bitArray *bitArray,
                             fm_int       startBitNumber,
                             fm_int       numBits,
                             fm_bool      bitValue);


/*****************************************************************************
 * fmGetBitArrayBit
 *
 * Description: Retrieves the boolean value of a specified bit
 *
 * Arguments:   bitArray                pointer to the bit array
 *
 *              bitNumber               bit number (0 to bitCount-1)
 *
 *              bitValue                pointer for returned TRUE/FALSE value
 *
 * Returns:     Fulcrum API status code
 *
 *****************************************************************************/
fm_status fmGetBitArrayBit(fm_bitArray *bitArray,
                           fm_int       bitNumber,
                           fm_bool *    bitValue);


/*****************************************************************************
 * fmClearBitArray
 *
 * Description: Clears all bits in a bit array
 *
 * Arguments:   bitArray                pointer to the bit array
 *
 * Returns:     Fulcrum API status code
 *
 *****************************************************************************/
fm_status fmClearBitArray(fm_bitArray *bitArray);


/*****************************************************************************
 * fmFindBitInBitArray
 *
 * Description: Searches a bit array, starting with a specified bit,
 *              looking for the next bit that has the specified value.
 *
 * Arguments:   bitArray                pointer to the bit array
 *
 *              firstBitNumber          starting bit number (0 to bitCount-1)
 *
 *              bitValue                specified bit value to find (TRUE/FALSE)
 *
 *              foundBit                pointer for returned bit position,
 *                                      will be 0 to bitCount-1 if found,
 *                                      -1 if not found
 *
 * Returns:     Fulcrum API status code
 *
 *****************************************************************************/
fm_status fmFindBitInBitArray(fm_bitArray *bitArray,
                              fm_int       firstBitNumber,
                              fm_bool      bitValue,
                              fm_int *     foundBit);


/*****************************************************************************
 * fmFindLastBitInBitArray
 *
 * Description: Searches a bit array in the reverse direction, starting with a
 *              specified bit, looking backward for the next bit that has the
 *              specified value.
 *
 * Arguments:   bitArray                pointer to the bit array
 *
 *              firstBitNumber          starting bit number (0 to bitCount-1)
 *
 *              bitValue                specified bit value to find
 *                                      (TRUE/FALSE)
 *
 *              foundBit                pointer for returned bit position,
 *                                      will be 0 to bitCount-1 if found,
 *                                      -1 if not found
 *
 * Returns:     Fulcrum API status code
 *
 *****************************************************************************/
fm_status fmFindLastBitInBitArray(fm_bitArray *bitArray,
                                  fm_int       firstBitNumber,
                                  fm_bool      bitValue,
                                  fm_int *     foundBit);


/*****************************************************************************
 * fmFindBitBlockInBitArray
 *
 * Description: Searches a bit array, starting with a specified bit,
 *              looking for a block of bits that have the specified value.
 *
 * Arguments:   bitArray                pointer to the bit array
 *
 *              firstBitNumber          starting bit number (0 to bitCount-1)
 *
 *              blockSize               Number of bits in block to be found.
 *
 *              bitValue                specified bit value to find
 *                                      (TRUE/FALSE)
 *
 *              foundBit                pointer for returned bit position,
 *                                      will be 0 to bitCount-1 if found,
 *                                      -1 if not found
 *
 * Returns:     Fulcrum API status code
 *
 *****************************************************************************/
fm_status fmFindBitBlockInBitArray(fm_bitArray *bitArray,
                                   fm_int       firstBitNumber,
                                   fm_int       blockSize,
                                   fm_bool      bitValue,
                                   fm_int *     foundBit);


/*****************************************************************************
 * fmFindLastBitBlockInBitArray
 *
 * Description: Searches a bit array in the reverse direction, starting with
 *              a specified bit, looking backward for a block of bits that
 *              have the specified value.
 *
 * Arguments:   bitArray                pointer to the bit array
 *
 *              firstBitNumber          starting bit number (0 to bitCount-1)
 *
 *              blockSize               Number of bits in block to be found.
 *
 *              bitValue                specified bit value to find
 *                                      (TRUE/FALSE)
 *
 *              foundBit                pointer for returned bit position,
 *                                      will be 0 to bitCount-1 if found,
 *                                      -1 if not found
 *
 * Returns:     Fulcrum API status code
 *
 *****************************************************************************/
fm_status fmFindLastBitBlockInBitArray(fm_bitArray *bitArray,
                                       fm_int       firstBitNumber,
                                       fm_int       blockSize,
                                       fm_bool      bitValue,
                                       fm_int *     foundBit);


/*****************************************************************************
 * fmGetBitArrayNonZeroBitCount
 *
 * Description: Retrieves the number of non-zero bits in the bit array
 *
 * Arguments:   bitArray                pointer to the bit array
 *
 *              bitCount                pointer for returned non-zero-bit count
 *
 * Returns:     Fulcrum API status code
 *
 *****************************************************************************/
fm_status fmGetBitArrayNonZeroBitCount(fm_bitArray *bitArray,
                                       fm_int *     bitCount);


/*****************************************************************************
 * fmDbgDumpBitArray
 *
 * Description: Displays the contents of the bit array
 *
 * Arguments:   bitArray                pointer to the bit array
 *
 *              n                       the number of bits to display
 *
 * Returns:     Fulcrum API status code
 *
 *****************************************************************************/
void fmDbgDumpBitArray(fm_bitArray *bitArray, fm_int n);


fm_bool fmCompareBitArrays(fm_bitArray *array1, fm_bitArray *array2);
fm_status fmUnionBitArrays(const fm_bitArray *src1,
                           const fm_bitArray *src2,
                           fm_bitArray *      dst);
fm_status fmAndBitArrays(fm_bitArray *src1,
                         fm_bitArray *src2,
                         fm_bitArray *      dst);
fm_bool fmIsBitInBitArray(fm_bitArray *bitArray, fm_int bitNumber);
fm_status fmCopyBitArray(fm_bitArray* dest, const fm_bitArray* src);


#endif /* __FM_FM_BITARRAY_H */
