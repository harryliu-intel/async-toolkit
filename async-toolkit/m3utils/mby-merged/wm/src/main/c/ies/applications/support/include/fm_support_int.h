/* vim:et:sw=4:ts=4:tw=79:
 * (No tabs, indent level is 4 spaces, text width is 79)  */

/*****************************************************************************
 * File:            fm_support_int.h
 * Creation Date:   October 26, 2007
 * Description:     
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

#ifndef __FM_FM_SUPPORT_INT_H
#define __FM_FM_SUPPORT_INT_H

#include <fm_sdk_int.h>

#include <internal/fm_support_root_int.h>

#include <internal/fm_packet_classify_int.h>

#include <internal/fm_packet_arp_int.h>

#include <internal/fm_packet_igmp_int.h>

#define FM_SUPPORT_VERIFY(status)                                             \
    if ((status) != FM_OK)                                                    \
    {                                                                         \
        goto ABORT;                                                           \
    }                                                                         \

#endif /* __FM_FM_SUPPORT_INT_H */

