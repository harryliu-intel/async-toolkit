/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_replication.c
 * Creation Date:   June 29, 2011
 * Description:     Structures and functions for dealing with replication groups.
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


#include <fm_sdk_int.h>


/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/


/*****************************************************************************
 * Global Variables
 *****************************************************************************/


/*****************************************************************************
 * Local Variables
 *****************************************************************************/


/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/


/*****************************************************************************
 * Local Functions
 *****************************************************************************/



/*****************************************************************************/
/** findReplicationGroup
 * \ingroup intReplicate
 *
 * \desc            Find a replication group, given its handle.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       handle is the replication group's handle.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_intReplicationGroup *findReplicationGroup(fm_int sw, fm_int handle)
{
    fm_switch *             switchPtr;
    fm_intReplicationGroup *group;
    fm_status               status;

    switchPtr = GET_SWITCH_PTR(sw);

    status = fmTreeFind( &switchPtr->replicationTree,
                        (fm_uint64) handle,
                        (void **) &group );

    if (status != FM_OK)
    {
        group = NULL;
    }

    return group;

}   /* end findReplicationGroup */




/*****************************************************************************/
/** fmCreateReplicationGroupInt
 * \ingroup intReplicate
 *
 * \desc            Create a replication group.
 *
 * \note            The replication group number returned by this function is
 *                  a handle, not a logical port number.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      groupHandle points to caller-allocated storage where this
 *                  function should place the replication group number (handle)
 *                  of the newly created replication group.
 * 
 * \param[in]       mcastLogicalPort is the logical port of the multicast group.
 *
 * \param[out]      mcastIndex points to caller-allocated storage where this
 *                  function should place the index within the associated
 *                  hardware resource table.
 * 
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_NO_MEM if not enough memory is available for the
 *                  replication group structure.
 *
 *****************************************************************************/
fm_status fmCreateReplicationGroupInt(fm_int  sw, 
                                      fm_int *groupHandle,
                                      fm_int  mcastLogicalPort, 
                                      fm_int *mcastIndex)
{
    fm_status               err;
    fm_switch *             switchPtr;
    fm_intReplicationGroup *group = NULL;
    fm_int                  handle;

    FM_LOG_ENTRY_API(FM_LOG_CAT_MULTICAST,
                     "sw = %d, groupHandle = %p\n",
                     sw,
                     (void *) groupHandle);

    switchPtr = GET_SWITCH_PTR(sw);

    err = fmFindBitInBitArray(&switchPtr->replicationHandles,
                              0,
                              FALSE,
                              &handle);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MULTICAST, err);

    if (handle < 0)
    {
        err = FM_ERR_NO_MCAST_RESOURCES;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MULTICAST, err);
    }

    FM_LOG_DEBUG(FM_LOG_CAT_MULTICAST,
                 "New replication group handle = %d\n", handle);

    FM_API_CALL_FAMILY(err, 
                       switchPtr->ReserveReplicationGroupMcastIndex, 
                       sw,
                       handle,
                       mcastLogicalPort, 
                       mcastIndex);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MULTICAST, err);

    err = fmSetBitArrayBit(&switchPtr->replicationHandles, handle, TRUE);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MULTICAST, err);

    group = (fm_intReplicationGroup *) fmAlloc( sizeof(fm_intReplicationGroup) );

    if (group == NULL)
    {
        err = FM_ERR_NO_MEM;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MULTICAST, err);
    }

    memset( group, 0, sizeof(fm_intReplicationGroup) );

    group->handle      = handle;
    group->hwDestIndex = *mcastIndex;

    FM_LOG_DEBUG(FM_LOG_CAT_MULTICAST,
                 "Replication group %d allocated: hwDestIndex = %d\n",
                 group->handle, group->hwDestIndex);

    fmTreeInit(&group->mcastGroupList);

    err = fmTreeInsert(&switchPtr->replicationTree,
                       (fm_uint64) group->handle,
                       (void *) group);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MULTICAST, err);

    *groupHandle = group->handle;

ABORT:

    FM_LOG_EXIT_API(FM_LOG_CAT_MULTICAST, err);

}   /* end fmCreateReplicationGroupInt */



/*****************************************************************************/
/** fmDeleteReplicationGroupInt
 * \ingroup intReplicate
 *
 * \desc            Delete a replication group.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupHandle is the replication group number.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_MULTICAST_GROUP if the replication group
 *                  is invalid.
 *
 *****************************************************************************/
fm_status fmDeleteReplicationGroupInt(fm_int sw, 
                                      fm_int groupHandle)
{
    fm_status               err;
    fm_switch *             switchPtr;
    fm_intReplicationGroup *group = NULL;

    FM_LOG_ENTRY_API(FM_LOG_CAT_MULTICAST,
                     "sw = %d, groupHandle = %d\n",
                     sw, groupHandle);

    switchPtr = GET_SWITCH_PTR(sw);

    group = findReplicationGroup(sw, groupHandle);
    if (group == NULL)
    {
        err = FM_ERR_INVALID_MULTICAST_GROUP;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MULTICAST, err);
    }

    FM_API_CALL_FAMILY(err, 
                       switchPtr->ReleaseReplicationGroupMcastIndex, 
                       sw,
                       group->handle);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MULTICAST, err);

    err = fmTreeRemove( &switchPtr->replicationTree,
                       (fm_uint64) group->handle,
                       NULL );
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MULTICAST, err);

    err = fmSetBitArrayBit(&switchPtr->replicationHandles,
                           group->handle,
                           FALSE);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MULTICAST, err);

    FM_LOG_DEBUG(FM_LOG_CAT_MULTICAST,
                 "Replication group %d deleted\n",
                 group->handle);

    fmFree(group);

ABORT:

    FM_LOG_EXIT_API(FM_LOG_CAT_MULTICAST, err);

}   /* end fmDeleteReplicationGroupInt */




/*****************************************************************************/
/** fmMoveReplicationGroupMcastGroupInt
 * \ingroup intReplicate
 *
 * \chips           FM6000
 *
 * \desc            Add a mcast group to a replication group.
 *
 * \param[in]       sw is the switch on which to operate.
 * 
 * \param[in]       groupHandle is the replication group number.
 *
 * \param[in]       mcastGroup is the multicast group number.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_MULTICAST_GROUP if the replication group
 *                  is invalid.
 * \return          FM_ERR_NO_MEM if not enough memory is available for the
 *                  multicast group structure.
 *
 *****************************************************************************/
fm_status fmMoveReplicationGroupMcastGroupInt(fm_int sw, 
                                              fm_int groupHandle, 
                                              fm_int mcastGroup)
{
    fm_status  err;
    fm_switch *switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_MULTICAST,
                     "sw = %d, groupHandle = %d, mcastGroup %d\n",
                     sw,
                     groupHandle,
                     mcastGroup);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY(err, 
                       switchPtr->MoveReplicationGroupMcastGroup, 
                       sw, 
                       groupHandle,
                       mcastGroup);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MULTICAST, err);

ABORT:

    FM_LOG_EXIT_API(FM_LOG_CAT_MULTICAST, err);

}   /* end fmMoveReplicationGroupMcastGroupInt */



/*****************************************************************************
 * Public Functions
 *****************************************************************************/


/*****************************************************************************/
/** fmCreateReplicationGroup
 * \ingroup replicate
 *
 * \chips           FM6000
 *
 * \desc            Create a replication group.
 *
 * \note            The replication group number returned by this function is
 *                  a handle, not a logical port number.
 *
 * \note            A multicast group may be associated with this replication
 *                  group by setting its ''FM_MCASTGROUP_SHARED_REPLICATION_GROUP''
 *                  attribute.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      groupHandle points to caller-allocated storage where this
 *                  function should place the replication group number (handle)
 *                  of the newly created replication group.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_NO_MEM if not enough memory is available for the
 *                  replication group structure.
 *
 *****************************************************************************/
fm_status fmCreateReplicationGroup(fm_int sw, fm_int *groupHandle)
{
    fm_status  err;
    fm_switch *switchPtr;
    fm_int     mcastIndex;
    fm_bool    lockTaken = FALSE;

    FM_LOG_ENTRY_API(FM_LOG_CAT_MULTICAST,
                     "sw = %d, groupHandle = %p\n",
                     sw,
                     (void *) groupHandle);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MULTICAST, err);
    lockTaken = TRUE;

    err = fmCreateReplicationGroupInt(sw, groupHandle, -1, &mcastIndex);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MULTICAST, err);

ABORT:

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_MULTICAST, err);

}   /* end fmCreateReplicationGroup */




/*****************************************************************************/
/** fmDeleteReplicationGroup
 * \ingroup replicate
 *
 * \chips           FM6000
 *
 * \desc            Delete a replication group. There must be no multicast
 *                  groups associated with the replication group in order
 *                  for it to be deleted.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupHandle is the replication group number returned in
 *                  a prior call to ''fmCreateReplicationGroup''.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_MULTICAST_GROUP if the replication group
 *                  is invalid or is not empty of multicast groups.
 *
 *****************************************************************************/
fm_status fmDeleteReplicationGroup(fm_int sw, fm_int groupHandle)
{
    fm_status               err;
    fm_switch *             switchPtr;
    fm_intReplicationGroup *group = NULL;
    fm_bool                 lockTaken = FALSE;
    fm_int                  groupMember;

    FM_LOG_ENTRY_API(FM_LOG_CAT_MULTICAST,
                     "sw = %d, groupHandle = 0x%x\n",
                     sw,
                     groupHandle);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MULTICAST, err);
    lockTaken = TRUE;

    group = findReplicationGroup(sw, groupHandle);
    if (group == NULL)
    {
        err = FM_ERR_INVALID_MULTICAST_GROUP;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MULTICAST, err);
    }

    /* Make sure that there is no more mcast group into this replication group */
    groupMember = (fm_int) fmTreeSize(&group->mcastGroupList);
    if (groupMember > 0)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_MULTICAST,
                     "Mcast groups must be removed first: %d members remaining\n",
                     groupMember);
        err = FM_ERR_INVALID_MULTICAST_GROUP;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MULTICAST, err);
    }

    err = fmDeleteReplicationGroupInt(sw, groupHandle);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MULTICAST, err);

ABORT:

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_MULTICAST, err);

}   /* end fmDeleteReplicationGroup */

