/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_trigger.h
 * Creation Date:   May 29, 2008
 * Description:     Prototypes and structure definitions for low-level trigger
 *                  allocation.
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

#ifndef __FM_FM_API_TRIGGER_H
#define __FM_FM_API_TRIGGER_H

/****************************************************************************/
/** \ingroup typeStruct
 *
 * Defines hints passed to the trigger allocation function, 
 * ''fmAllocateTrigger''.  Some hints may apply only to certain device families.
 ****************************************************************************/

typedef struct _fm_triggerRequestInfo
{
    /** Indicates if a rate limiter must be allocated for the
     *  the trigger.  See the FM4000 datasheet for how to program a trigger
     *  rate limiter. */
    fm_bool requestRateLimiter;

} fm_triggerRequestInfo;

fm_status fmAllocateTrigger(fm_int sw, 
                            fm_int *trigger, 
                            fm_triggerRequestInfo *info);

fm_status fmAllocateTriggerExt(fm_int sw, 
                               fm_text name, 
                               fm_int *trigger, 
                               fm_triggerRequestInfo *info);

fm_status fmFreeTrigger(fm_int sw, fm_int trigger); 

#endif /* __FM_FM_API_TRIGGER_H */
