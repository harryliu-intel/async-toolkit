/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:           fm_alos_init_int.h
 * Creation Date:  June 18, 2007
 * Description:    Structures internal to ALOS.
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

#ifndef __FM_FM_ALOS_INIT_INT_H
#define __FM_FM_ALOS_INIT_INT_H


#define FM_ALOS_INTERNAL_RWL_CTR_MAX  24


/* "global" ALOS variables that are shared between processes */
typedef struct _fm_rootAlos
{
    /* fm_alos_lock.c */
    fm_lock             LockLock;
    fm_lock *           LockList[FM_ALOS_INTERNAL_MAX_LOCKS];

    /* fm_alos_rwlock.c */
    fm_lock             dbgRwLockListLock;
    fm_rwLock *         dbgRwLockList[FM_ALOS_INTERNAL_MAX_DBG_RW_LOCKS];
    fm_lock             rwLockDebugCounterLock;
    int                 rwLockDebugCounters[FM_ALOS_INTERNAL_RWL_CTR_MAX];

    /* fm_alos_sem.c */
    pthread_mutex_t     dbgAccessLock;
    fm_semaphore *      dbgSemaphoreList[FM_ALOS_INTERNAL_MAX_SEMAPHORES];

    /* fm_alos_logging.c */
    fm_loggingState     fmLoggingState;

    /* common/fm_attr.c */
    fm_customTree       attributeTree;
    fm_lock             attributeLock;
    
    /* Mask of non-switch-specific locks that have a precedence. */
    fm_lockPrecedence   nonSwitchLockPrecs;

    /* Table of Dynamically-Loaded Libraries. (fm_alos_dynamic_load.c). */
    fm_dynLoadLib *     dlLibs[FM_ALOS_INTERNAL_DYN_LOAD_LIBS];

    /* Lock to control access to the dlLibs table. */
    fm_lock             dlAccessLock;

    /* common/fm_tree.c */
    fm_tree             treeTree;
    pthread_mutex_t     treeTreeLock;

} fm_rootAlos;

extern fm_rootAlos *      fmRootAlos;

/* Information about threads, which is private to each process */
typedef struct _fm_alosThreadState
{
    /** Tree mapping pthread_t to fm_thread* for the threads in this process */
    fm_tree         dbgThreadTree;

    /** Mutex for access to dbgThreadTree */
    pthread_mutex_t threadTreeLock;

    /** thread to hold main process info */
    fm_thread       mainThread;

    /** Indicates that this data structure has been initialized */
    fm_bool         initialized;
    
    fm_int          foreignThreadCount;

} fm_alosThreadState;

extern fm_alosThreadState fmAlosThreadState;

/* internal initialization functions */
fm_status fmAlosLockInit(void);
fm_status fmAlosRwlockInit(void);
fm_status fmAlosSemInit(void);
fm_status fmAlosThreadInit(void);
fm_status fmAlosLoggingInit(void);


#endif /* __FM_FM_ALOS_INIT_INT_H */
