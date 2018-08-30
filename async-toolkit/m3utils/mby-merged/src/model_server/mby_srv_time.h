/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_alos_time.h
 * Creation Date:   2005
 * Description:     Timestamp functions
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

#ifndef __FM_FM_ALOS_TIME_H
#define __FM_FM_ALOS_TIME_H

#include "mby_model.h"

/**************************************************/
/** \ingroup typeStruct
 * Structure used to hold a timestamp value.
 **************************************************/
typedef struct _fm_timestamp
{
    /** Number of whole seconds. */
    fm_uint64 sec;

    /** Number of microseconds in addition to sec. */
    fm_uint64 usec;

} fm_timestamp;


/* returns: -1 if t1 < t2
 *           0 if t1 = t2
 *           1 if t1 > t2
 */
fm_int fmCompareTimestamps(fm_timestamp *t1, fm_timestamp *t2);


/* performs t1 = t1 + t2 */
void fmAddTimestamps(fm_timestamp *t1, fm_timestamp *t2);


/* performs t3 = t1 - t2 */
void fmSubTimestamps(const fm_timestamp *t1,
                     const fm_timestamp *t2,
                     fm_timestamp *      t3);

fm_status fmGetTime(fm_timestamp *tvp);


/* delay for some time */
fm_status fmDelay(fm_int sec, fm_int nsec);


/* get a formatted date string */
fm_status fmGetFormattedTime(char *dateStr);

/* constants used by mutual exclusion function calls */

#define FM_WAIT_FOREVER  (NULL)
#define FM_NO_WAIT       &fmNoWaitTimeConstant;

extern const fm_timestamp fmNoWaitTimeConstant;

#endif /* __FM_FM_ALOS_TIME_H */
