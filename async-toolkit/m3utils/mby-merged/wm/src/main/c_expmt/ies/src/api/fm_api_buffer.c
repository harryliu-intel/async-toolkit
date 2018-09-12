/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_buffer.c
 * Creation Date:   May 15, 2007
 * Description:     Functions dealing with packet buffers
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2012 Intel Corporation. All Rights Reserved.
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
/** fmAllocateBuffer
 * \ingroup buffer
 *
 * \desc            Allocate a packet buffer.  The buffer is accessed through
 *                  an ''fm_buffer'' structure, which keeps housekeeping
 *                  information for chaining multiple data blocks together.
 *
 * \param[in]       sw is not used. This is a legacy argument for backward
 *                  compatibility with existing applications.
 *
 * \return          A pointer to an ''fm_buffer'' structure.
 * \return          NULL is returned if there are no buffers available.
 *
 *****************************************************************************/
fm_buffer *fmAllocateBuffer(int sw)
{
    fm_buffer *outBuffer = NULL;

    FM_NOT_USED(sw);

    FM_LOG_ENTRY_API(FM_LOG_CAT_BUFFER, "sw=%d\n", sw);

    outBuffer = fmPlatformAllocateBuffer();

    if (outBuffer == NULL)
    {
        fmDbgGlobalDiagCountIncr(FM_GLOBAL_CTR_OUT_OF_BUFFERS, 1);
    }
    else
    {
        fmDbgGlobalDiagCountIncr(FM_GLOBAL_CTR_BUFFER_ALLOCS, 1);
    }

    FM_LOG_EXIT_API_CUSTOM(FM_LOG_CAT_BUFFER,
                           outBuffer,
                           "outBuffer=%p\n",
                           (void *) outBuffer);

}   /* end fmAllocateBuffer */




/*****************************************************************************/
/** fmFreeBuffer
 * \ingroup buffer
 *
 * \desc            Return a packet buffer, previously allocated with a
 *                  call to ''fmAllocateBuffer'', to the free buffer pool.
 *
 * \note            If the buffer is part of a chain of buffers, this function
 *                  will not dispose of the other buffers in the chain.  See
 *                  fmFreeBufferChain for disposing of an entire chain.
 *
 * \param[in]       sw is not used. This is a legacy argument for backward
 *                  compatibility with existing applications.
 *
 * \param[in]       buf points to the buffer's ''fm_buffer'' structure.
 *
 * \return          FM_OK
 *
 *****************************************************************************/
fm_status fmFreeBuffer(int sw, fm_buffer *buf)
{
    fm_status err;

    FM_NOT_USED(sw);

    FM_LOG_ENTRY_API(FM_LOG_CAT_BUFFER, "sw=%d\n", sw);

    err = fmPlatformFreeBuffer(buf);

    if (err == FM_OK)
    {
        fmDbgGlobalDiagCountIncr(FM_GLOBAL_CTR_BUFFER_FREES, 1);
    }

    FM_LOG_EXIT_API(FM_LOG_CAT_BUFFER, err);

}   /* end fmFreeBuffer */




/*****************************************************************************/
/** fmFreeBufferChain
 * \ingroup buffer
 *
 * \desc            Return an entire chain of packet buffers, previously
 *                  allocated with calls to 'fmAllocateBuffer'', to the free
 *                  buffer pool.
 *
 * \param[in]       sw is not used. This is a legacy argument for backward
 *                  compatibility with existing applications.
 *
 * \param[in]       bufChain points to the first buffer in the chain.
 *
 * \return          FM_OK
 *
 *****************************************************************************/
fm_status fmFreeBufferChain(int sw, fm_buffer *bufChain)
{
    fm_buffer *nextBuffer;
    fm_buffer *curBuffer;
    fm_status  status = FM_OK;

    FM_LOG_ENTRY_API(FM_LOG_CAT_BUFFER, "sw=%d\n", sw);

    curBuffer = bufChain;

    while (curBuffer != NULL)
    {
        nextBuffer = curBuffer->next;
        status     = fmFreeBuffer(sw, curBuffer);

        if (status != FM_OK)
        {
            break;
        }

        curBuffer = nextBuffer;
    }

    FM_LOG_EXIT_API(FM_LOG_CAT_BUFFER, status);

}   /* end fmFreeBufferChain */




/*****************************************************************************/
/** fmGetBufferDataPtr
 * \ingroup buffer
 *
 * \desc            Return a pointer to a buffer's data section.
 *
 * \param[in]       buf is a pointer to the ''fm_buffer'' structure.
 *
 * \return          A pointer to the data payload of the buffer.
 * \return          Returns NULL if buf is invalid.
 *
 *****************************************************************************/
fm_uint32 *fmGetBufferDataPtr(fm_buffer *buf)
{
    fm_uint32 *data;

    FM_LOG_ENTRY_API(FM_LOG_CAT_BUFFER, "buf=%p\n", (void *) buf);

    if (buf)
    {
        data = buf->data;
    }
    else
    {
        data = NULL;
    }

    FM_LOG_EXIT_API_CUSTOM(FM_LOG_CAT_BUFFER, data, "data=%p\n", (void *) data);

}   /* end fmGetBufferDataPtr */




fm_status fmSetBufferDataPtr(fm_buffer *buf, fm_uint32 *ptr)
{
    if (!buf)
    {
        return FM_ERR_BAD_BUFFER;
    }

    buf->data = ptr;

    return FM_OK;

}   /* end fmSetBufferDataPtr */




/*****************************************************************************/
/** fmGetBufferDataLength
 * \ingroup buffer
 *
 * \desc            Return the length of a buffer's data section actually used.
 *
 * \param[in]       buf is a pointer to the ''fm_buffer'' structure.
 *
 * \param[out]      len points to caller-allocated storage where this
 *                  function should place the length of the buffer.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_BAD_BUFFER if buf is invalid.
 *
 *****************************************************************************/
fm_status fmGetBufferDataLength(fm_buffer *buf, fm_int *len)
{
    fm_status err = FM_OK;

    FM_LOG_ENTRY_API(FM_LOG_CAT_BUFFER, "buf=%p\n", (void *) buf);

    if (!buf)
    {
        err = FM_ERR_BAD_BUFFER;
    }
    else
    {
        *len = buf->len;
    }

    FM_LOG_EXIT_API(FM_LOG_CAT_BUFFER, err);

}   /* end fmGetBufferDataLength */




/*****************************************************************************/
/** fmSetBufferDataLength
 * \ingroup buffer
 *
 * \desc            Set a buffer's length.
 *
 * \param[in]       buf is a pointer to the ''fm_buffer'' structure.
 *
 * \param[in]       len contains the length in bytes of the data in the buffer
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_BAD_BUFFER if buf is invalid.
 *
 *****************************************************************************/
fm_status fmSetBufferDataLength(fm_buffer *buf, fm_int len)
{
    fm_status err = FM_OK;

    FM_LOG_ENTRY_API(FM_LOG_CAT_BUFFER, "buf=%p len=%d\n", (void *) buf, len);

    if (!buf)
    {
        err = FM_ERR_BAD_BUFFER;
    }
    else
    {
        buf->len = len;
    }

    FM_LOG_EXIT_API(FM_LOG_CAT_BUFFER, err);

}   /* end fmSetBufferDataLength */




/*****************************************************************************/
/** fmGetNextBuffer
 * \ingroup buffer
 *
 * \desc            Get the next buffer in a chain.
 *
 * \param[in]       buf is a pointer to the ''fm_buffer'' structure.
 *
 * \return          A pointer to the next buffer in the chain.
 * \return          NULL if the end of the chain was reached.
 *
 *****************************************************************************/
fm_buffer *fmGetNextBuffer(fm_buffer *buf)
{
    fm_buffer *nextBuf;

    FM_LOG_ENTRY_API(FM_LOG_CAT_BUFFER, "buf=%p\n", (void *) buf);

    if (buf)
    {
        nextBuf = buf->next;
    }
    else
    {
        nextBuf = NULL;
    }

    FM_LOG_EXIT_API_CUSTOM(FM_LOG_CAT_BUFFER,
                           nextBuf,
                           "nextBuf=%p\n",
                           (void *) nextBuf);

}   /* end fmGetNextBuffer */




/*****************************************************************************/
/** fmAddBuffer
 * \ingroup buffer
 *
 * \desc            Add a buffer to the end of a chain of buffers.
 *
 * \param[in]       frame is a pointer to the first ''fm_buffer'' structure in
 *                  a chain of buffers to which the new buffer should be added.
 *
 * \param[in]       buf is a pointer to the new ''fm_buffer'' structure to be
 *                  added to the chain.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_BAD_BUFFER if buf is invalid.
 *
 *****************************************************************************/
fm_status fmAddBuffer(fm_buffer *frame, fm_buffer *buf)
{
    fm_status  err = FM_OK;
    fm_buffer *ptr = frame;

    FM_LOG_ENTRY_API(FM_LOG_CAT_BUFFER,
                     "frame=%p buf=%p\n",
                     (void *) frame,
                     (void *) buf);

    if (!buf)
    {
        err = FM_ERR_BAD_BUFFER;
    }
    else
    {
        while (ptr->next)
        {
            ptr = ptr->next;
        }

        ptr->next = buf;
        buf->next = NULL;
    }

    FM_LOG_EXIT_API(FM_LOG_CAT_BUFFER, err);

}   /* end fmAddBuffer */




/*****************************************************************************/
/** fmDuplicateBufferChain
 * \ingroup buffer
 *
 * \desc            Duplicate a buffer chain by allocating buffers and copying
 *                  the contents from the source buffer(s) into the new
 *                  buffer(s).
 *
 * \param[in]       sw is not used. This is a legacy argument for backward
 *                  compatibility with existing applications.
 *
 * \param[in]       srcFrame is a pointer to the first ''fm_buffer'' structure
 *                  in a chain of buffers which are to be cloned into a new
 *                  chain.
 *
 * \return          Pointer to the first buffer in the new chain, or NULL if
 *                  the new chain could not be allocated.
 *
 *****************************************************************************/
fm_buffer *fmDuplicateBufferChain(fm_int sw, fm_buffer *srcFrame)
{
    fm_buffer *newChain;
    fm_buffer *currentSrc;
    fm_buffer *currentDest;

    FM_LOG_ENTRY_API(FM_LOG_CAT_BUFFER,
                     "sw=%d srcFrame=%p\n",
                     sw,
                     (void *) srcFrame);

    newChain = NULL;

    currentSrc = srcFrame;

    while (currentSrc != NULL)
    {
        if (currentSrc->len > 0)
        {
            currentDest = fmAllocateBuffer(sw);

            if (currentDest == NULL)
            {
                FM_LOG_WARNING(FM_LOG_CAT_BUFFER,
                               "fmDuplicateBufferChain unable to allocate "
                               "buffer - cancelling duplication\n");

                if (newChain != NULL)
                {
                    fmFreeBufferChain(sw, newChain);
                    newChain = NULL;
                }

                break;
            }

            FM_MEMCPY_S(currentDest->data,
                        currentSrc->len,
                        currentSrc->data,
                        currentSrc->len);
            currentDest->len  = currentSrc->len;
            currentDest->next = NULL;

            if (newChain == NULL)
            {
                newChain = currentDest;
            }
            else
            {
                fmAddBuffer(newChain, currentDest);
            }
        }

        currentSrc = currentSrc->next;
    }

    FM_LOG_EXIT_API_CUSTOM(FM_LOG_CAT_BUFFER,
                           newChain,
                           "newChain=%p\n",
                           (void *) newChain);

}   /* end fmDuplicateBufferChain */
