/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            platform_buffer_defs.h
 * Creation Date:   June 5, 2012
 * Description:     Constants definining memory buffer related information.
 *                  Shared between user and kernel layer.
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

#ifndef __FM_PLATFORM_BUFFER_DEFS_H
#define __FM_PLATFORM_BUFFER_DEFS_H

/** Defines the number of frame buffers to be allocated.
 *  \ingroup constSystem */
#define FM_NUM_BUFFERS               128

/** The size of each ''fm_buffer'' data "chunk" in bytes. This value must be
 *  divisible by 4.
 *  \ingroup constSystem */
#define FM_BUFFER_SIZE_BYTES         1024

/* The size of each buffer chunk in words. */
#define FM_BUFFER_SIZE_WORDS         (FM_BUFFER_SIZE_BYTES >> 2)

/** The number of bytes of memory consumed by frame buffers. Calculated
 *  automatically from ''FM_NUM_BUFFERS'' and ''FM_BUFFER_SIZE_BYTES''.
 *  \ingroup constSystem */
#define FM_BUFFER_MEMORY_SIZE_BYTES  (FM_NUM_BUFFERS * FM_BUFFER_SIZE_BYTES)

#endif

