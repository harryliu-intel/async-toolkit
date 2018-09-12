/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_alos_lock.h
 * Creation Date:   2005
 * Description:     ALOS routines for dealing with locks abstractly
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

#ifndef __FM_FM_ALOS_LOCK_H
#define __FM_FM_ALOS_LOCK_H


/* May be used as an argument to fmCreateLockV2. */
#define FM_LOCK_SWITCH_NONE                 -1

typedef fm_uint32 fm_lockPrecedence;


/**************************************************/
/** \ingroup intTypeStruct
 *  Lock type that abstracts operating system
 *  implementations for ensuring mutually exclusive
 *  access to system resources (mutexes).
 **************************************************/
typedef struct _fm_lock
{
    /** Used internally by ALOS to hold the operating system's mutex handle. */
    void *              handle;

    /** Name by which to identify the lock. */
    fm_text             name;
    
    /** Switch with which the lock is associated. Will be zero for locks
     *  not associated with any switch (this field is used to
     *  track lock inversions, not for unambiguously identifying the
     *  associated switch). */
    fm_int              switchNumber;
    
    /** A singleton bit mask that represents the precedence for this lock.
     *  ''fmCaptureLock'' will enforce the order in which locks may be
     *  captured by a thread. Locks with lower precedence may not be
     *  captured after locks with higher precedence. This mechanism is
     *  used to prevent lock inversion between two threads. */
    fm_lockPrecedence   precedence;
    
    /** Used internally by ALOS to hold the number of times the lock has been 
     *  taken by a thread. Incrmented on capture, decremented on release. 
     *  Used to know when it is time to remove the lock's precedence from the 
     *  thread's lock collection. */
    fm_uint             takenCount;
    
    /** Used internally by ALOS to hold the thread ID of owner. */
    void *              owner;

} fm_lock;


extern fm_status fmCreateLock(fm_text lockName, fm_lock *lck);
extern fm_status fmCreateLockV2(fm_text lockName, 
                                fm_int  sw,
                                fm_int  precedence,
                                fm_lock *lck);
extern fm_status fmDeleteLock(fm_lock *lck);

extern fm_status fmCaptureLock(fm_lock *lck, fm_timestamp *timeout);
extern fm_status fmReleaseLock(fm_lock *lck);
extern fm_status fmIsLockTaken(fm_lock *lck, fm_bool *isTaken);

extern void fmDbgDumpLocks(void);

extern fm_status fmDbgTakeLock(fm_int      sw,
                               fm_lock*     lockPtr,
                               fm_int       tryTime,
                               fm_int       numTries,
                               const char*  function);

void fmDbgDumpLockPrecMask(fm_uint64 logCat, 
                           fm_uint64 logLevel, 
                           fm_lockPrecedence precMask);

#endif /* __FM_FM_ALOS_LOCK_H */
