/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_buffer.h
 * Creation Date:   May 15, 2007
 * Description:     Structures and functions for dealing with packet buffers
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2011 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM_API_BUFFER_H
#define __FM_FM_API_BUFFER_H


/**************************************************/
/** \ingroup typeStruct
 * Structure used for communicating Ethernet
 * packets between the API and the application. May
 * be chained together for packets that exceed the
 * basic data block (chunk) size.
 **************************************************/
typedef struct _fm_buffer
{
    /** Pointer to a "chunk" of packet data. The packet data is in network
     *  byte order and must begin at the first byte of the location pointed
     *  to by this structure member. Partially filled chunks are not permitted,
     *  except of course for the last in the chain. */
    fm_uint32 *data;

    /** Number of bytes pointed to by data (the number of bytes in this
     *  chunk). */
    int        len;

    /** Pointer to the next buffer in a chain, or NULL for the last. */
    struct _fm_buffer *next;

    /** Private data used by the API. The application should not touch this
     *  member. The index is a uniquely assigned value for this buffer
     *  instance, used for indexing and identifying buffer instances. */
    int        index;

} fm_buffer;

/* returns a pointer to a newly allocated buffer */
fm_buffer *fmAllocateBuffer(int sw);


/* releases memory for an allocated buffer */
fm_status fmFreeBuffer(int sw, fm_buffer *buf);


/* releases memory for a chain of buffers */
fm_status fmFreeBufferChain(int sw, fm_buffer *bufChain);


/* wrappers to access internal members of the buffer */
fm_uint32 *fmGetBufferDataPtr(fm_buffer *buf);
fm_status fmSetBufferDataPtr(fm_buffer *buf, fm_uint32 *ptr);
fm_status fmGetBufferDataLength(fm_buffer *buf, fm_int *len);
fm_status fmSetBufferDataLength(fm_buffer *buf, fm_int len);
fm_buffer *fmGetNextBuffer(fm_buffer *buf);


/* walk the frame and add a buffer to the end */
fm_status fmAddBuffer(fm_buffer *frame, fm_buffer *buf);


/* helper for duplicating */
fm_buffer *fmDuplicateBufferChain(fm_int sw, fm_buffer *srcFrame);


#endif /* __FM_FM_API_BUFFER_H */
