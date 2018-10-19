/* vim:et:sw=4:ts=4:tw=79:
 * (No tabs, indent level is 4 spaces, text width is 79)  */

/*****************************************************************************
 * File:            fm_support_root_int.h
 * Creation Date:   October 31, 2007
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


#ifndef __FM_FM_SUPPORT_ROOT_INT_H
#define __FM_FM_SUPPORT_ROOT_INT_H

typedef struct _fm_rootSupport {
    /* The support lock handle */
    fm_lock   lock;

    /* The packet processing state */
    fm_uint64 packetState;

    /* The fm_rootSupport initialization state */
    fm_bool   initialize;

    /* The IGMP address type */
    fm_multicastAddressType  igmpType;
} fm_rootSupport;

extern fm_rootSupport fmRootSupport[FM_MAX_NUM_SWITCHES];

#define /* fm_status */ TAKE_SUPPORT_LOCK(/* fm_int  */ switchNum)            \
    fmCaptureLock(&(fmRootSupport[switchNum].lock), FM_WAIT_FOREVER)

#define /* fm_status */ DROP_SUPPORT_LOCK(/* fm_int  */ switchNum)            \
    fmReleaseLock(&(fmRootSupport[switchNum].lock))

#endif /* __FM_FM_SUPPORT_ROOT_INT_H */

