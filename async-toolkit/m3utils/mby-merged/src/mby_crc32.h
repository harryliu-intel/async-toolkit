/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_crc32.h
 * Creation Date:   Nov 10, 2006
 * Description:     This file contains definitions used to support bit arrays
 *
 * INTEL CONFIDENTIAL
 * Copyright 2006 - 2012 Intel Corporation. All Rights Reserved. 
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

#ifndef MBY_CRC32_H
#define MBY_CRC32_H

#include "mby_common.h"

// Function prototypes:
fm_uint32 mbyCrc32          (const fm_byte * const buf, const fm_uint32 len);
fm_uint32 mbyCrc32Math      (const fm_byte * const buf, const fm_uint32 len);
fm_uint32 mbyCrc32ByteSwap  (const fm_byte * const buf, const fm_uint32 len);
fm_uint32 mbyCrc32C         (const fm_byte * const buf, const fm_uint32 len);
fm_uint32 mbyCrc32CMath     (const fm_byte * const buf, const fm_uint32 len);
fm_uint32 mbyCrc32CByteSwap (const fm_byte * const buf, const fm_uint32 len);
fm_uint32 mbyCrc32K         (const fm_byte * const buf, const fm_uint32 len);
fm_uint32 mbyCrc32KMath     (const fm_byte * const buf, const fm_uint32 len);
fm_uint32 mbyCrc32KByteSwap (const fm_byte * const buf, const fm_uint32 len);
fm_uint32 mbyCrc32Q         (const fm_byte * const buf, const fm_uint32 len);

#endif

