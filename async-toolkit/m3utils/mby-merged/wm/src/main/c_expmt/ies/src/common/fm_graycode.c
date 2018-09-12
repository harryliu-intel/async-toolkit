/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces) */
/*****************************************************************************
 * File:            fm_graycode.c
 * Creation Date:   October 03, 2011
 * Description:     File containing helper functions related to the encoding or 
 *                  decoding of numbers using Gray Code (a.k.a. "reflected
 *                  binary code")
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

#include <fm_sdk_int.h>


/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

/*****************************************************************************
 * Local function prototypes
 *****************************************************************************/

/*****************************************************************************
 * Global Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** fmConvertBinaryToGray 
 * 
 * \ingroup intGrayCode
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            This function takes a raw binary number and 
 *                  converts it into Gray Code
 *
 * \param[in]       binary is the binary-encoded number to convert
 * \param[in]       nBits is the number of bits to be converted
 *
 * \return          The Gray-encoded number
 *
 *****************************************************************************/
fm_uint64 fmConvertBinaryToGray( fm_uint64 binary, fm_int nBits )
{

    fm_uint64 gray = 0;
    fm_int    i;

    /* scan all bits */
    for (i = 1 ; i < nBits  ; i++)
    {
        /* if the i-th bit is '1' the (i-1)-the bit is inverted */
        if ( (binary & (FM_LITERAL_U64(1) << i)) != 0 )
        {
            /* inverted */
            gray += ((binary & (FM_LITERAL_U64(1) << (i-1))) ^
                    (FM_LITERAL_U64(1) << (i-1))); 
        }
        else
        {
            /* not inverted */
            gray += (binary & (FM_LITERAL_U64(1) << (i-1)));
        }
    }

    /* Most significant bit is always the same */
    gray += (binary & (FM_LITERAL_U64(1) << (nBits-1)));

    return gray;

} /* end fmConvertBinaryToGray */


/*****************************************************************************/
/** fmConvertGrayToBinary
 * 
 * \ingroup intGrayCode
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            This function takes a Gray Code number and 
 *                  converts it into Binary
 *
 * \param[in]       gray is the gray-encoded number to be converted
 * \param[in]       nBits is the number of bits to be converted
 *
 * \return          The Binary-encoded number
 *
 *****************************************************************************/
fm_uint64 fmConvertGrayToBinary( fm_uint64 gray, fm_int nBits )
{

    fm_uint64 binary = 0;
    fm_int    i;

    /* Most significant bit is always the same */
    binary += (gray & (FM_LITERAL_U64(1) << (nBits-1)));

    for (i = nBits - 1 ; i > 0  ; i--)
    {
        /* if the i-th bit is '1' the (i-1)-the bit is inverted */
        if ( (binary & (FM_LITERAL_U64(1) << i)) != 0 )
        {
            /* inverted */
            binary += ((gray & (FM_LITERAL_U64(1) << (i-1))) ^
                    (FM_LITERAL_U64(1) << (i-1))); 
        }
        else
        {
            /* not inverted */
            binary += (gray & (FM_LITERAL_U64(1) << (i-1)));
        }
    }

    return binary;

} /* end fmConvertGrayToBinary */



 
