/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_lock_prec.h
 * Creation Date:   November 3, 2009
 * Description:     Specifies precedence for all locks used by the API.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2009 - 2011 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM_LOCK_PREC_H
#define __FM_FM_LOCK_PREC_H

/**************************************************
 * The following enumeration dictates the order
 * in which a thread may take these locks. A lock
 * listed earlier in the list may not be taken
 * after a lock later in the list.
 **************************************************/

enum
{
    FM_LOCK_PREC_SWITCH,                    /* fmRootApi->fmSwitchLockTable[sw] */
    FM_LOCK_PREC_LBGS,                      /* switchPtr->lbgInfo.lbgLock */
    FM_LOCK_PREC_ROUTING,                   /* swstate->routingLock */
    FM_LOCK_PREC_LAGS,                      /* swstate->lagLock */
    FM_LOCK_PREC_L2,                        /* swstate->L2Lock */
    FM_LOCK_PREC_PURGE,                     /* swstate->maPurgeLock */
    FM_LOCK_PREC_MATABLE_MAINT,             /* swstate->macTableMaintWorkListLock */
    FM_LOCK_PREC_TABLE_UPDATE_STATS,        /* fmRootApi->tableUpdateStats.lck */
    FM_LOCK_PREC_ACLS,                      /* swstate->aclLock */
    FM_LOCK_PREC_TRIGGERS,                  /* swstate->triggerLock */
    FM_LOCK_PREC_CRM,                       /* swstate->crmLock */
    FM_LOCK_PREC_MTABLE,                    /* swstate->mtableLock */
    FM_LOCK_PREC_STATE_LOCK,                /* swstate->stateLock */
    FM_LOCK_PREC_FFU,                       /* switchExt->ffuAtomicAccessLock */
    FM_LOCK_PREC_PLATFORM,                  /* ps->accessLocks[FM_MEM_TYPE_CSR] */
    FM_LOCK_PREC_TREE_TREE,                 /* fmRootApi->treeTreeLock */

};


/**************************************************
 * Special lock precedence value that is used to
 * transcend the precedence check. Locks with this
 * precedence may be taken in any order with respect
 * to any other lock.
 **************************************************/

#define FM_LOCK_SUPER_PRECEDENCE            -1


#endif /* __FM_FM_LOCK_PREC_H */
