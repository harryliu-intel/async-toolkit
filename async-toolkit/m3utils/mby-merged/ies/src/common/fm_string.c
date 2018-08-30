/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:           fm_string.c
 * Creation Date:  March 26, 2008
 * Description:    String manipulation functions missing from standard C.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2008 - 2012 Intel Corporation. All Rights Reserved.
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
/** fmStringCopy
 * \ingroup intString
 *
 * \desc            Copy a string without overflowing the destination buffer.
 *
 * \note            This is similar to the non-standard C library
 *                  function strlcpy.
 *
 * \param[out]      dst is the destination buffer.  Unlike strncpy,
 *                  dst will always be nul-terminated.
 *
 * \param[in]       src is the string to be copied.
 *
 * \param[in]       dstSize is the total size of the destination buffer.
 *
 * \return          None
 *
 *****************************************************************************/
void fmStringCopy(char *dst, const char *src, fm_uint dstSize)
{
    fm_uint i;

    for (i = 0 ; i + 1 < dstSize && src[i] != 0 ; i++)
    {
        dst[i] = src[i];
    }

    dst[i] = 0;

}   /* end fmStringCopy */


/*****************************************************************************/
/** fmStringAppend
 * \ingroup intString
 *
 * \desc            Append one string to another, without overflowing the
 *                  destination buffer.
 *
 * \note            This is similar to the non-standard C library
 *                  function strlcat.
 *
 * \param[in,out]   dst is the string that src will be appended to.
 *
 * \param[in]       src is the string to be appended to dst.
 *
 * \param[in]       dstSize is the total size of the destination buffer.
 *
 * \return          None
 *
 *****************************************************************************/
void fmStringAppend(char *dst, const char *src, fm_uint dstSize)
{
    fm_uint i;
    fm_uint j;

    for (i = 0 ; i + 1 < dstSize && dst[i] != 0 ; i++)
    {
    }

    for (j = 0 ; i + 1 < dstSize && src[j] != 0 ; i++, j++)
    {
        dst[i] = src[j];
    }

    dst[i] = 0;

}   /* end fmStringAppend */


/*****************************************************************************/
/** fmStringDuplicate
 * \ingroup intString
 *
 * \desc            Allocate memory for a new string and copy an existing
 *                  string into it.
 *
 * \note            This is similar to the POSIX function strdup, but uses
 *                  fmAlloc to allocate memory.
 *
 * \param[in]       src is the string to be copied.
 *
 * \return          copy of src if successful
 * \return          NULL if out of memory
 *
 *****************************************************************************/
char *fmStringDuplicate(const char *src)
{
    fm_uint size;
    char *result;

    size = strlen(src) + 1;
    result = fmAlloc(size);
    if (result != NULL)
    {
        FM_MEMCPY_S(result, size, src, size);
    }

    return result;

}   /* end fmStringDuplicate */
