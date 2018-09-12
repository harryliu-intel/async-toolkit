/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_alos_rwlock.h
 * Creation Date:   2005
 * Description:     ALOS routines for dealing with read/write locks abstractly
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

#ifndef __FM_FM_ALOS_RWLOCK_H
#define __FM_FM_ALOS_RWLOCK_H


/**************************************************
 *  Reader-writer lock per-thread information.
 **************************************************/
typedef struct _fm_rwLockThreadEntry
{
    /** thread id of a thread who has this lock */
    void *  id;

    /** number of recursive reader lock acquisitions */
    fm_uint numReaders;

    /** number of recursive writer lock acquisitions */
    fm_uint numWriters;

    /** entry in use? (because we can't rely on NULL id) */
    fm_bool used;
    
    /** Number of times the lock has been taken by a thread. Incrmented
     *  on capture, decremented on release. Used to know when it is
     *  time to remove the lock's precedence from the thread's lock
     *  collection. */
    fm_uint             takenCount;

} fm_rwLockThreadEntry;


/**************************************************/
/** \ingroup intTypeStruct
 *  Reader-writer lock (mutex).
 **************************************************/
typedef struct _fm_rwLock
{
    /** Used internally by ALOS to hold an operating system mutex handle. */
    void *                accessHandle;

    /** Used internally by ALOS to hold an operating system semaphore handle
     *  for pending readers. */
    void *                readHandle;

    /** Used internally by ALOS to hold an operating system semaphore handle
     *  for pending writers. */
    void *                writeHandle;

    /** Used internally by ALOS to hold the number of active readers. */
    fm_int                numActiveReaders;

    /** Used internally by ALOS to hold the number of active writers. */
    fm_int                numActiveWriters;

    /** Used internally by ALOS to hold the number of pending readers. */
    fm_int                numPendingReaders;

    /** Used internally by ALOS to hold the number of pending writers. */
    fm_int                numPendingWriters;

    /** Used internally by ALOS to hold the highest-used index into the 
     *  userList table */
    fm_int                maxThreads;

    /** Used internally by ALOS to hold the list of threads using this lock
     *  and associated per-thread state. */
    fm_rwLockThreadEntry *userList;

    /** Name by which to identify the lock for diagnostic purposes. */
    fm_text               name;
    
    /** Switch with which the lock is associated. Will be zero for locks
     *  not associated with any switch (this field is used to
     *  track lock inversions, not for unambiguously identifying the
     *  associated switch). */
    fm_int              switchNumber;
    
    /** A singleton bit mask that represents the precedence for this lock.
     *  ''fmCaptureReadLock'' and ''fmCaptureWriteLock'' will enforce the 
     *  order in which locks may be captured by a thread. Locks with lower 
     *  precedence may not be captured after locks with higher precedence. 
     *  This mechanism is used to prevent lock inversion between two threads. */
    fm_lockPrecedence   precedence;

    /** Used internally to keep track of whether a member of the userList is 
        waiting to be promoted */
    fm_bitArray           readerToBePromoted;

} fm_rwLock;

fm_status fmCreateRwLock(fm_text lockName, fm_rwLock *lck);
fm_status fmCreateRwLockV2(fm_text lockName, fm_int sw, fm_int precedence, fm_rwLock *lck);
fm_status fmDeleteRwLock(fm_rwLock *lck);
fm_status fmCaptureReadLock(fm_rwLock *lck, fm_timestamp *timeout);
fm_status fmCaptureWriteLock(fm_rwLock *lck, fm_timestamp *timeout);
fm_status fmReleaseReadLock(fm_rwLock *lck);
fm_status fmReleaseWriteLock(fm_rwLock *lck);
void fmDbgDiagDumpRwLockState(int sw);
void fmDbgDiagDumpRwLockStats(int sw);
fm_status fmGetThreadRwLockStatus(fm_rwLock *lck, fm_int *reads, fm_int *writes);


#endif /* __FM_FM_ALOS_RWLOCK_H */
