/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_alos_sem.h
 * Creation Date:   2005
 * Description:     ALOS routines for dealing with semaphores abstractly
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

#ifndef __FM_FM_ALOS_SEM_H
#define __FM_FM_ALOS_SEM_H


/* Sempahore types */
/**************************************************/
/** \ingroup typeEnum
 *  When a semaphore is created, the type of
 *  semaphore must be indicated.
 **************************************************/
typedef enum
{
    /** A binary semaphore is either "full" or "empty." */
    FM_SEM_BINARY = 0,

    /** A counting semaphore holds a numeric value. */
    FM_SEM_COUNTING

} fm_semType;


/**************************************************/
/** \ingroup typeStruct
 *  Semaphore type used to abstract operating system
 *  sempahore implementations.
 **************************************************/
typedef struct _fm_semaphore
{
    /** Used internally by ALOS to hold the operating system's sempahore
     *  handle. */
    void *     handle;

    /** The type of sempahore (see 'fm_semType'). */
    fm_semType semType;

    /** Name by which to identify the semaphore. */
    fm_text    name;

} fm_semaphore;

fm_status fmCreateSemaphore(fm_text       semName,
                            fm_semType    semType,
                            fm_semaphore *semHandle,
                            fm_int        initial);
fm_status fmFindSemaphore(fm_text semName, fm_semaphore *semHandle);
fm_status fmDeleteSemaphore(fm_semaphore *semHandle);
fm_status fmCaptureSemaphore(fm_semaphore *semHandle, fm_timestamp *timeout);
fm_status fmReleaseSemaphore(fm_semaphore *semHandle);


/** \ingroup macroSynonym
 * @{ */

/** A synonym for ''fmCaptureSemaphore''. */
#define fmWaitSemaphore(h, t)  fmCaptureSemaphore( (h), (t) )

/** A synonym for ''fmReleaseSemaphore''. */
#define fmSignalSemaphore(h)   fmReleaseSemaphore( (h) )

/** @} (end of Doxygen group) */

void fmDbgDumpAllSemaphores(void);


#endif /* __FM_FM_ALOS_SEM_H */
