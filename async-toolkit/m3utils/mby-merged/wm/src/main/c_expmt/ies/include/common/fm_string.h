/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:           fm_string.h
 * Creation Date:  March 26, 2008
 * Description:    String manipulation functions missing from standard C.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2008 - 2013 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM_STRING_H
#define __FM_FM_STRING_H

void fmStringCopy(char *dst, const char *src, fm_uint dstSize);
void fmStringAppend(char *dst, const char *src, fm_uint dstSize);
char *fmStringDuplicate(const char *src);

/* Define buffer size for strerror */
#define FM_STRERROR_BUF_SIZE 128

#endif  /* __FM_FM_STRING_H */
