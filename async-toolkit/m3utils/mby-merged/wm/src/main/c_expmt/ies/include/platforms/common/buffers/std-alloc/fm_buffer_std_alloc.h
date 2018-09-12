/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_buffer_std_alloc.h
 * Creation Date:   Sept. 19, 2007
 * Description:     Standard circular list buffer allocator.
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

#ifndef __FM_FM_BUFFER_STD_ALLOC_H
#define __FM_FM_BUFFER_STD_ALLOC_H

/* manages the circular list of buffers */
typedef struct
{
    /* Index of first free chunk in the chunk list. */
    fm_int     firstFree;

    /* Allocator: array of chunk indices indexed by chunk index */
    fm_int *   freeList;

    /* Total number of available buffers */
    fm_int     totalBufferCount;

    /* Number of available buffers */
    fm_int     availableBuffers;

    /* Table of buffers */
    fm_buffer *table;

    /* Pointer to the backing memory pool */
    fm_uint32 *pool;

} fm_bufferAllocState;

fm_status fmPlatformInitBuffers(fm_uint32 *bufferMemoryPool);


#endif /* __FM_FM_BUFFER_STD_ALLOC_H */
