/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_lag.c
 * Creation Date:   2005
 * Description:     Structures and functions for dealing with link aggregation
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2012 Intel Corporation. All Rights Reserved. 
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


/*****************************************************************************
 * Public Functions
 *****************************************************************************/




/*****************************************************************************/
/** fmCreateLAG
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Create a link aggregation group. Deprecated in favor of
 *                  ''fmCreateLAGExt''.
 *
 * \note            This function operates on the first switch in the system.
 *                  To use on an arbitrary switch, use ''fmCreateLAGExt''.
 *                                                                      \lb\lb
 *                  This function should not be used after calling
 *                  ''fmAllocateStackLAGs'' (see ''Stacking and GloRT 
 *                  Management''). Instead, use ''fmCreateStackLAG''.
 *
 * \param[out]      lagNumber points to caller-allocated storage where this
 *                  function should place the LAG number (handle) of the
 *                  newly created LAG.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NO_FREE_LAG if max LAGs (''FM_MAX_NUM_LAGS'') already
 *                  created.
 * \return          FM_ERR_NO_MEM if not enough memory is available for the
 *                  lag structure.
 *
 *****************************************************************************/
fm_status fmCreateLAG(fm_int *lagNumber)
{
    fm_status err;
    fm_int    sw;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG, "lagNumber = %p\n", (void *) lagNumber);

    /* Retrieve the first existing switch number */
    err = fmGetSwitchFirst(&sw);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LAG, err);

    err = fmCreateLAGExt(sw, lagNumber);

ABORT:
    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmCreateLAG */




/*****************************************************************************/
/** fmCreateLAGExt
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Create a link aggregation group.
 *
 * \note            This function should not be used after calling
 *                  ''fmAllocateStackLAGs'' (see ''Stacking and GloRT 
 *                  Management''). Instead, use ''fmCreateStackLAG''.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[out]      lagNumber points to caller-allocated storage where this
 *                  function should place the LAG number (handle) of the
 *                  newly created LAG.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NO_FREE_LAG if max LAGs (''FM_MAX_NUM_LAGS'') already
 *                  created.
 * \return          FM_ERR_NO_MEM if not enough memory is available for the
 *                  lag structure.
 *
 *****************************************************************************/
fm_status fmCreateLAGExt(fm_int sw, fm_int *lagNumber)
{
    fm_status err;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG,
                     "sw = %d, lagNumber = %p\n",
                     sw,
                     (void *) lagNumber);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    err = fmCreateLAGInt(sw, lagNumber, FALSE);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmCreateLAGExt */




/*****************************************************************************/
/** fmDeleteLAG
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Delete a link aggregation group. Deprecated in favor of
 *                  ''fmDeleteLAGExt''.
 *
 * \note            This function operates on the first switch in the system.
 *                  To use on an arbitrary switch, use ''fmDeleteLAGExt''.
 *
 * \param[in]       lagNumber is the LAG number of the LAG to delete, as
 *                  returned by ''fmCreateLAG'', ''fmCreateLAGExt'' or
 *                  specified in the call to ''fmCreateStackLAG''.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_LAG if lagNumber is out of range or is
 *                  not the handle of an existing LAG.
 *
 *****************************************************************************/
fm_status fmDeleteLAG(fm_int lagNumber)
{
    fm_status err;
    fm_int    sw;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG, "lagNumber = %d\n", lagNumber);

    /* Retrieve the first existing switch number */
    err = fmGetSwitchFirst(&sw);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LAG, err);

    err = fmDeleteLAGExt(sw, lagNumber);

ABORT:
    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmDeleteLAG */




/*****************************************************************************/
/** fmDeleteLAGExt
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Delete a link aggregation group.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       lagNumber is the LAG number of the LAG to delete, as
 *                  returned by ''fmCreateLAG'', ''fmCreateLAGExt'' or
 *                  ''fmCreateStackLAG''.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_LAG if lagNumber is out of range or is
 *                  not the handle of an existing LAG.
 *
 *****************************************************************************/
fm_status fmDeleteLAGExt(fm_int sw, fm_int lagNumber)
{
    fm_switch *  switchPtr;
    fm_int       lagIndex;
    fm_status    err;
    fm_bool      asyncDeletion;
    fm_lag *     lagPtr = NULL;
    fm_char      semName[20];
    fm_timestamp wait;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG, "sw = %d, lagNumber = %d\n", sw, lagNumber);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    asyncDeletion = fmGetBoolApiAttribute(FM_AAK_API_ASYNC_LAG_DELETION, 
                                          FM_AAD_API_ASYNC_LAG_DELETION);

    switchPtr = GET_SWITCH_PTR(sw);

    /**************************************************
     * We need to take the routing lock (for the benefit 
     * of fmMcastDeleteLagNotify) before the LAG lock
     * to prevent an inversion.
     **************************************************/
    
    err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    if (err != FM_OK)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);
    }
    
    TAKE_LAG_LOCK(sw);

    /* Get the internal index for this LAG. */
    lagIndex = fmGetLagIndex(sw, lagNumber);
    if (lagIndex < 0)
    {
        err = FM_ERR_INVALID_LAG;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LAG, err);
    }

    err = fmMcastDeleteLagNotify(sw, lagIndex);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LAG, err);

    lagPtr = GET_LAG_PTR(sw, lagIndex);

    /* Asynchronous deletion used unless disabled and the switch is
     * not a member of a switch aggregate.  For switch aggregates, the
     * synchronized deletion is handled at the swag switch level.
     */
    if ( !asyncDeletion && (switchPtr->swag < 0) )
    {
        lagPtr->deleteSemaphore = (fm_semaphore *) fmAlloc( sizeof(fm_semaphore) );

        if (lagPtr->deleteSemaphore == NULL)
        {
            err = FM_ERR_NO_MEM;
            goto ABORT;
        }

        snprintf( semName, sizeof(semName), "lagDelSem%d", lagIndex );
        err = fmCreateSemaphore(semName,
                                FM_SEM_BINARY,
                                lagPtr->deleteSemaphore,
                                0);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LAG, err);

    }

    FM_API_CALL_FAMILY(err, switchPtr->DeleteLagFromSwitch, sw, lagIndex);

    if ( (err == FM_OK) && (lagPtr->deleteSemaphore != NULL) )
    {
        wait.sec = fmGetIntApiAttribute(FM_AAK_API_LAG_DELETE_SEMAPHORE_TIMEOUT,
                                        FM_AAD_API_LAG_DELETE_SEMAPHORE_TIMEOUT);
        wait.usec = 0;
        DROP_LAG_LOCK(sw);

        err = fmWaitSemaphore(lagPtr->deleteSemaphore, &wait);

        TAKE_LAG_LOCK(sw);

        if (err == FM_OK)
        {
            /* Note that fmFreeLAG releases and deletes the deleteSemaphore */
            fmFreeLAG(sw, lagIndex);
            lagPtr = NULL;
        }
        else
        {
            FM_LOG_FATAL(FM_LOG_CAT_LAG, "The deleteSemaphore timed out\n");
        }
    }

ABORT:

    if ( (lagPtr != NULL) && (lagPtr->deleteSemaphore != NULL) )
    {
        fmReleaseSemaphore(lagPtr->deleteSemaphore);
        fmDeleteSemaphore(lagPtr->deleteSemaphore);

        lagPtr->deleteSemaphore = NULL;
    }

    DROP_LAG_LOCK(sw);
    fmReleaseWriteLock(&switchPtr->routingLock);
    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmDeleteLAGExt */




/*****************************************************************************/
/** fmAddLAGPort
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Add a port to a link aggregation group.
 *
 * \param[in]       sw is the switch number on which to operate.
 *
 * \param[in]       lagNumber is the LAG number (returned by
 *                  fmCreateLAG) to which the port should be added.
 *
 * \param[in]       port is the number of the port to be added to the LAG.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_PORT if port is invalid.
 * \return          FM_ERR_INVALID_LAG if lagNumber is out of range or is
 *                  not the handle of an existing LAG.
 * \return          FM_ERR_ALREADYUSED_PORT if the port is already a member
 *                  of a LAG.
 * \return          FM_ERR_FULL_LAG if the LAG already contains the maximum
 *                  number of ports (''FM_MAX_NUM_LAG_MEMBERS'').
 *
 *****************************************************************************/
fm_status fmAddLAGPort(fm_int sw, fm_int lagNumber, fm_int port)
{
    fm_switch *switchPtr;
    fm_int     lagIndex;
    fm_status  err;
    fm_status  err2;
    fm_lag *   lagPtr;
    fm_port *  portPtr;
    fm_bool    lagLockTaken = FALSE;
    fm_bool    routeLockTaken = FALSE;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG,
                     "sw = %d, lagNumber = %d, port = %d\n",
                     sw,
                     lagNumber,
                     port);

    VALIDATE_AND_PROTECT_SWITCH(sw);
 
    switchPtr = GET_SWITCH_PTR(sw);

    /* Allowing remote logical ports and local physical ports */
    VALIDATE_LOGICAL_PORT(sw, port, ALLOW_REMOTE);

    /* Validate the port is LAG capable */
    portPtr = GET_PORT_PTR(sw, port);

    if ( !fmIsRemotePort(sw, port)
        && !(portPtr->capabilities & FM_PORT_CAPABILITY_LAG_CAPABLE) )
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LAG, err = FM_ERR_INVALID_PORT);
    }

    err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LAG, err);

    routeLockTaken = TRUE;

    TAKE_LAG_LOCK(sw);

    lagLockTaken = TRUE;

    /* Get the internal index for this LAG. */
    lagIndex = fmGetLagIndex(sw, lagNumber);
    if (lagIndex < 0)
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LAG, err = FM_ERR_INVALID_LAG);
    }

    if ( fmPortIsInALAG(sw, port) )
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LAG, err = FM_ERR_ALREADYUSED_PORT);
    }

    /* Internal LAG should be filled only by Internal Port AND */
    /* External LAG should be filled only by External Port     */
    if (fmIsCardinalPort(sw, port))
    {
        lagPtr = GET_LAG_PTR(sw, lagIndex);
        if ( lagPtr->isInternalPort != fmIsInternalPort(sw, port) )
        {
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LAG, err = FM_FAIL);
        }
    }

    /* TODO: this shouldn't be necessary. */
    switchPtr->portTable[port]->lagIndex = lagIndex;

    FM_API_CALL_FAMILY(err, switchPtr->AddPortToLag, sw, lagIndex, port);

    if (err != FM_OK)
    {
        /* TODO: this shouldn't be necessary. */
        switchPtr->portTable[port]->lagIndex = -1;
    }
    else if (switchPtr->perLagMgmt)
    {
        /* Now the port is member of the LAG configure it
         * using the LAG settings.*/
        err = fmApplyLAGSettingsToPort(sw, port, lagIndex);
        if (err != FM_OK)
        {
            /* Something went wrong so delete the port from the LAG. */
            FM_API_CALL_FAMILY(err2, 
                               switchPtr->DeletePortFromLag, 
                               sw, 
                               lagIndex, 
                               port);

            /* TODO: this shouldn't be necessary. */
            switchPtr->portTable[port]->lagIndex = -1;
        }
    }

ABORT:

    if (lagLockTaken)
    {
        DROP_LAG_LOCK(sw);
    }

    if (routeLockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmAddLAGPort */




/*****************************************************************************/
/** fmDeleteLAGPort
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Delete a port from a link aggregation group.
 *
 * \param[in]       sw is the switch number on which to operate.
 *
 * \param[in]       lagNumber is the LAG number (returned by
 *                  fmCreateLAG) from which the port should be deleted.
 *
 * \param[in]       port is the number of the port to be deleted from the LAG.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_PORT if port is invalid.
 * \return          FM_ERR_INVALID_LAG if lagNumber is out of range or is
 *                  not the handle of an existing LAG.
 * \return          FM_ERR_INVALID_PORT if the port is not a member of the
 *                  specified LAG.
 *
 *****************************************************************************/
fm_status fmDeleteLAGPort(fm_int sw, fm_int lagNumber, fm_int port)
{
    fm_switch *switchPtr;
    fm_int     lagIndex;
    fm_status  err;
    fm_bool    lagLockTaken = FALSE;
    fm_bool    routeLockTaken = FALSE;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG,
                     "sw = %d, lagNumber = %d, port = %d\n",
                     sw,
                     lagNumber,
                     port);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    /* Allowing remote logical ports and local physical ports */
    VALIDATE_LOGICAL_PORT(sw, port, ALLOW_REMOTE);

    switchPtr = GET_SWITCH_PTR(sw);

    err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LAG, err);

    routeLockTaken = TRUE;

    TAKE_LAG_LOCK(sw);

    lagLockTaken = TRUE;

    /* Get the internal index for this LAG. */
    lagIndex = fmGetLagIndex(sw, lagNumber);
    if (lagIndex < 0)
    {
        err = FM_ERR_INVALID_LAG;
        goto ABORT;
    }

    /* Check that the port is actually in the LAG we're trying to
     * remove it from.
     */
    if ( !fmPortIsInLAG(sw, port, lagIndex) )
    {
        err = FM_ERR_INVALID_PORT;
        goto ABORT;
    }

    FM_API_CALL_FAMILY(err, switchPtr->DeletePortFromLag, sw, lagIndex, port);

ABORT:

    if (lagLockTaken)
    {
        DROP_LAG_LOCK(sw);
    }

    if (routeLockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmDeleteLAGPort */




/*****************************************************************************/
/** fmGetLAGList
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Function to return a list of valid LAGs.
 *                  Deprecated in favor of ''fmGetLAGListExt''.
 *
 * \note            This function operates on the first switch in the system.
 *                  To use on an arbitrary switch, use ''fmGetLAGListExt''.
 *
 * \param[out]      nLAG contains the number of valid LAGs found.
 *
 * \param[out]      lagNumbers contains the array of valid LAG numbers.
 *
 * \param[in]       maxLags is the maximum number of LAGs, i.e., size of
 *                  lagNumbers array.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_BUFFER_FULL if maxLags was too small to accommodate the 
 *                  entire list of valid LAGs.
 *
 *****************************************************************************/
fm_status fmGetLAGList(fm_int *nLAG, fm_int *lagNumbers, fm_int maxLags)
{
    fm_status err;
    fm_int    sw;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG,
                     "nLAG = %p, lagNumbers = %p, maxLags = %d\n",
                     (void *) nLAG,
                     (void *) lagNumbers,
                     maxLags);

    /* Retrieve the first existing switch number */
    err = fmGetSwitchFirst(&sw);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LAG, err);

    err = fmGetLAGListExt(sw, nLAG, lagNumbers, maxLags);

ABORT:
    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmGetLAGList */




/*****************************************************************************/
/** fmGetLAGListExt
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Function to return a list of valid LAGs.
 *
 * \param[in]       sw is the switch number on which to operate.
 *
 * \param[out]      nLAG contains the number of valid LAGs found.
 *
 * \param[out]      lagNumbers contains the array of valid LAG numbers.
 *
 * \param[in]       maxLags is the maximum number of LAGs, i.e., the size
 *                  of the lagNumbers array.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_BUFFER_FULL if maxLags was too small to accommodate 
 *                  the entire list of valid LAGs.
 *
 *****************************************************************************/
fm_status fmGetLAGListExt(fm_int  sw,
                          fm_int *nLAG,
                          fm_int *lagNumbers,
                          fm_int  maxLags)
{
    fm_switch *switchPtr;
    fm_int     lagCount = 0;
    fm_int     lagIndex;
    fm_status  err;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG,
                     "sw = %d, nLAG = %p, lagNumbers = %p, maxLags = %d\n",
                     sw,
                     (void *) nLAG,
                     (void *) lagNumbers,
                     maxLags);

    VALIDATE_AND_PROTECT_SWITCH(sw);
    TAKE_LAG_LOCK(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    for (lagIndex = 0 ; lagIndex < FM_MAX_NUM_LAGS ; lagIndex++)
    {
        if (switchPtr->lagInfoTable.lag[lagIndex])
        {
            if (lagCount >= maxLags)
            {
                err = FM_ERR_BUFFER_FULL;
                goto ABORT;
            }

            *lagNumbers++ = fmGetLagLogicalPort(sw, lagIndex);
            lagCount++;
        }
    }

    err = FM_OK;

ABORT:
    *nLAG = lagCount;

    DROP_LAG_LOCK(sw);
    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmGetLAGListExt */




/*****************************************************************************/
/** fmGetLAGPortList
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Function to return a list of ports in a LAG
 *                  Deprecated in favor of ''fmGetLAGPortListExt''.
 *
 * \note            This function operates on the first switch in the system.
 *                  To use on an arbitrary switch, use ''fmGetLAGPortListExt''.
 *
 * \param[in]       lagNumber is the lag number
 *
 * \param[out]      nPorts contains the number of ports found.
 *
 * \param[out]      ports contains the array of ports found
 *
 * \param[out]      switches contains the array of switches for the ports
 *
 * \param[in]       maxPorts is the maximum number of ports, i.e., size of
 *                  ports and switches arrays.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmGetLAGPortList(fm_int  lagNumber,
                           fm_int *nPorts,
                           fm_int *ports,
                           fm_int *switches,
                           fm_int  maxPorts)
{
    fm_int    i;
    fm_status err;
    fm_int    sw;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG,
                     "lagNumber = %d, nPorts = %p, ports = %p, switches = %p, "
                     "maxPorts = %d\n",
                     lagNumber,
                     (void *) nPorts,
                     (void *) ports,
                     (void *) switches,
                     maxPorts);

    /* Retrieve the first existing switch number */
    err = fmGetSwitchFirst(&sw);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LAG, err);

    err = fmGetLAGPortListExt(sw, lagNumber, nPorts, ports, maxPorts);

    for (i = 0 ; i < *nPorts ; i++)
    {
        switches[i] = sw;
    }

ABORT:
    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmGetLAGPortList */




/*****************************************************************************/
/** fmGetLAGPortListExt
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Function to return a list of ports in a LAG
 *
 * \param[in]       sw is the switch number on which to operate.
 *
 * \param[in]       lagNumber is the lag number
 *
 * \param[out]      nPorts contains the number of ports found.
 *
 * \param[out]      ports contains the array of ports found
 *
 * \param[in]       maxPorts is the maximum number of ports, i.e., size of
 *                  ports and switches arrays.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmGetLAGPortListExt(fm_int  sw,
                              fm_int  lagNumber,
                              fm_int *nPorts,
                              fm_int *ports,
                              fm_int  maxPorts)
{
    fm_int    lagIndex;
    fm_status err;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG,
                     "sw = %d, lagNumber = %d, nPorts = %p, ports = %p, "
                     "maxPorts = %d\n",
                     sw,
                     lagNumber,
                     (void *) nPorts,
                     (void *) ports,
                     maxPorts);

    VALIDATE_AND_PROTECT_SWITCH(sw);
    TAKE_LAG_LOCK(sw);

    /* Get the internal index for this LAG. */
    lagIndex = fmGetLagIndex(sw, lagNumber);
    if (lagIndex < 0)
    {
        err = FM_ERR_INVALID_LAG;
        goto ABORT;
    }

    err = fmGetLAGMemberPorts(sw, lagIndex, nPorts, ports, maxPorts, FALSE);

ABORT:
    DROP_LAG_LOCK(sw);
    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmGetLAGPortListExt */




/*****************************************************************************/
/** fmGetLAGFirst
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Retrieve the first link aggregation group number. 
 *                  Deprecated in favor of ''fmGetLAGFirstExt''.
 *
 * \note            This function operates on the first switch in the system.
 *                  To use on an arbitrary switch, use ''fmGetLAGFirstExt''.
 *
 * \param[out]      firstLagNumber points to caller-allocated storage where
 *                  this function should place the number of the first LAG.
 *                  Will be set to -1 if no LAGs found.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NO_LAGS if no LAG found.
 *
 *****************************************************************************/
fm_status fmGetLAGFirst(fm_int *firstLagNumber)
{
    fm_status err;
    fm_int    sw;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG,
                     "firstLagNumber = %p\n",
                     (void *) firstLagNumber);

    /* Retrieve the first existing switch number */
    err = fmGetSwitchFirst(&sw);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LAG, err);

    err = fmGetLAGFirstExt(sw, firstLagNumber);

ABORT:
    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmGetLAGFirst */




/*****************************************************************************/
/** fmGetLAGFirstExt
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Retrieve the first link aggregation group number.
 *
 * \param[in]       sw is the switch number on which to operate.
 *
 * \param[out]      firstLagNumber points to caller-allocated storage where
 *                  this function should place the number of the first LAG.
 *                  Will be set to -1 if no LAGs found.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_NO_LAGS if no LAG found.
 *
 *****************************************************************************/
fm_status fmGetLAGFirstExt(fm_int sw, fm_int *firstLagNumber)
{
    fm_switch *switchPtr;
    fm_int     lagIndex;
    fm_status  err;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG,
                     "sw = %d, firstLagNumber = %p\n",
                     sw,
                     (void *) firstLagNumber);

    VALIDATE_AND_PROTECT_SWITCH(sw);
    TAKE_LAG_LOCK(sw);

    *firstLagNumber = -1;

    switchPtr = GET_SWITCH_PTR(sw);

    for (lagIndex = 0 ; lagIndex < FM_MAX_NUM_LAGS ; lagIndex++)
    {
        if (switchPtr->lagInfoTable.lag[lagIndex])
        {
            *firstLagNumber = fmGetLagLogicalPort(sw, lagIndex);
            err = FM_OK;
            goto ABORT;
        }
    }

    err = FM_ERR_NO_LAGS;

ABORT:
    DROP_LAG_LOCK(sw);
    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmGetLAGFirstExt */




/*****************************************************************************/
/** fmGetLAGNext
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Retrieve the next link aggregation group number, following
 *                  a prior call to this function or to fmGetLAGFirst.
 *                  Deprecated in favor of ''fmGetLAGNextExt''.
 *
 * \note            This function operates on the first switch in the system.
 *                  To use on an arbitrary switch, use ''fmGetLAGNextExt''.
 *
 * \param[in]       currentLagNumber is the last LAG number found by a previous
 *                  call to this function or to fmGetLAGFirst.
 *
 * \param[out]      nextLagNumber points to caller-allocated storage where
 *                  this function should place the number of the next LAG.
 *                  Will be set to -1 if no LAGs found.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NO_LAGS if no more LAGs found.
 *
 *****************************************************************************/
fm_status fmGetLAGNext(fm_int currentLagNumber, fm_int *nextLagNumber)
{
    fm_status err;
    fm_int    sw;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG,
                     "currentLagNumber = %d, nextLagNumber = %p\n",
                     currentLagNumber,
                     (void *) nextLagNumber);

    /* Retrieve the first existing switch number */
    err = fmGetSwitchFirst(&sw);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LAG, err);

    err = fmGetLAGNextExt(sw, currentLagNumber, nextLagNumber);

ABORT:
    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmGetLAGNext */




/*****************************************************************************/
/** fmGetLAGNextExt
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Retrieve the next link aggregation group number, following
 *                  a prior call to this function or to fmGetLAGFirst.
 *
 * \param[in]       sw is the switch number on which to operate.
 *
 * \param[in]       currentLagNumber is the last LAG number found by a previous
 *                  call to this function or to fmGetLAGFirst.
 *
 * \param[out]      nextLagNumber points to caller-allocated storage where
 *                  this function should place the number of the next LAG.
 *                  Will be set to -1 if no LAGs found.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_NO_LAGS if no more LAGs found.
 *
 *****************************************************************************/
fm_status fmGetLAGNextExt(fm_int  sw,
                          fm_int  currentLagNumber,
                          fm_int *nextLagNumber)
{
    fm_switch *switchPtr;
    fm_int     lagIndex;
    fm_status  err;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG,
                     "sw = %d, currentLagNumber = %d, nextLagNumber = %p\n",
                     sw,
                     currentLagNumber,
                     (void *) nextLagNumber);

    VALIDATE_AND_PROTECT_SWITCH(sw);
    TAKE_LAG_LOCK(sw);

    *nextLagNumber = -1;

    switchPtr = GET_SWITCH_PTR(sw);

    /* Get the internal index for the current LAG. */
    lagIndex = fmGetLagIndex(sw, currentLagNumber);
    if (lagIndex < 0)
    {
        err = FM_ERR_INVALID_LAG;
        goto ABORT;
    }

    for (lagIndex = lagIndex + 1 ; lagIndex < FM_MAX_NUM_LAGS ; lagIndex++)
    {
        if (switchPtr->lagInfoTable.lag[lagIndex])
        {
            *nextLagNumber = fmGetLagLogicalPort(sw, lagIndex);
            err = FM_OK;
            goto ABORT;
        }
    }

    err = FM_ERR_NO_LAGS;

ABORT:
    DROP_LAG_LOCK(sw);
    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmGetLAGNextExt */




/*****************************************************************************/
/** fmGetLAGPortFirst
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Retrieve the first port in a LAG.
 *                  Deprecated in favor of ''fmGetLAGPortFirstExt''.
 *
 * \note            This function operates on the first switch in the system.
 *                  To use on an arbitrary switch, use ''fmGetLAGPortFirstExt''.
 *
 * \param[in]       lagNumber is the number of the LAG (returned by
 *                  fmCreateLAG) that should be searched for ports.
 *
 * \param[out]      firstPort points to caller-allocated storage where this
 *                  function should place the first port in the LAG.
 *                  Will be set to -1 if no ports found in LAG.
 *
 * \param[out]      firstSwitch points to caller-allocated storage where this
 *                  function should place the switch on which the port resides.
 *                  Will be set to -1 if no ports found in LAG.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_LAG if lagNumber is out of range or is
 *                  not the handle of an existing LAG.
 * \return          FM_ERR_NO_PORTS_IN_LAG if no ports in LAG.
 *
 *****************************************************************************/
fm_status fmGetLAGPortFirst(fm_int  lagNumber,
                            fm_int *firstPort,
                            fm_int *firstSwitch)
{
    fm_status err;
    fm_int    sw;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG,
                     "lagNumber = %d, firstPort = %p, firstSwitch = %p\n",
                     lagNumber,
                     (void *) firstPort,
                     (void *) firstSwitch);

    /* Retrieve the first existing switch number */
    err = fmGetSwitchFirst(&sw);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LAG, err);

    err = fmGetLAGPortFirstExt(sw, lagNumber, firstPort);

    if (*firstPort == -1)
    {
        *firstSwitch = -1;
    }
    else
    {
        *firstSwitch = sw;
    }

ABORT:
    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmGetLAGPortFirst */




/*****************************************************************************/
/** fmGetLAGPortFirstExt
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Retrieve the first port in a LAG.
 *
 * \param[in]       sw is the switch number on which to operate.
 *
 * \param[in]       lagNumber is the number of the LAG (returned by
 *                  fmCreateLAG) that should be searched for ports.
 *
 * \param[out]      firstPort points to caller-allocated storage where this
 *                  function should place the first port in the LAG.
 *                  Will be set to -1 if no ports found in LAG.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_LAG if lagNumber is out of range or is
 *                  not the handle of an existing LAG.
 * \return          FM_ERR_NO_PORTS_IN_LAG if no ports in LAG.
 *
 *****************************************************************************/
fm_status fmGetLAGPortFirstExt(fm_int  sw,
                               fm_int  lagNumber,
                               fm_int *firstPort)
{
    fm_lag *  lagPtr;
    fm_int    lagIndex;
    fm_status err;
    fm_int    i;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG,
                     "sw = %d, lagNumber = %d, firstPort = %p\n",
                     sw,
                     lagNumber,
                     (void *) firstPort);

    VALIDATE_AND_PROTECT_SWITCH(sw);
    TAKE_LAG_LOCK(sw);

    *firstPort = -1;

    /* Get the internal index for this LAG. */
    lagIndex = fmGetLagIndex(sw, lagNumber);
    if (lagIndex < 0)
    {
        err = FM_ERR_INVALID_LAG;
        goto ABORT;
    }

    lagPtr = GET_LAG_PTR(sw, lagIndex);

    for (i = 0 ; i < FM_MAX_NUM_LAG_MEMBERS ; i++)
    {
        if (lagPtr->member[i].port != FM_LAG_UNUSED_PORT)
        {
            *firstPort = lagPtr->member[i].port;
            err = FM_OK;
            goto ABORT;
        }
    }

    err = FM_ERR_NO_PORTS_IN_LAG;

ABORT:
    DROP_LAG_LOCK(sw);
    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmGetLAGPortFirstExt */




/*****************************************************************************/
/** fmGetLAGPortNext
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Retrieve the next port in a LAG.
 *                  Deprecated in favor of ''fmGetLAGPortNextExt''.
 *
 * \note            This function operates on the first switch in the system.
 *                  To use on an arbitrary switch, use ''fmGetLAGPortNextExt''.
 *
 * \param[in]       lagNumber is the number of the LAG (returned by
 *                  fmCreateLAG) that should be searched for ports.
 *
 * \param[in]       currentPort is the last port number found by a previous
 *                  call to this function or to fmGetLAGPortFirst.
 *
 * \param[in]       currentSwitch is the last switch number found by a previous
 *                  call to this function or to fmGetLAGPortFirst.
 *
 * \param[out]      nextPort points to caller-allocated storage where this
 *                  function should place the next port in the LAG.
 *                  Will be set to -1 if no more ports found in LAG.
 *
 * \param[out]      nextSwitch points to caller-allocated storage where this
 *                  function should place the switch on which the port resides.
 *                  Will be set to -1 if no more ports found in LAG.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_LAG if lagNumber is out of range or is
 *                  not the handle of an existing LAG.
 * \return          FM_ERR_INVALID_PORT if nextPort is not a port in lagNumber
 *                  on currentSwitch.
 * \return          FM_ERR_NO_PORTS_IN_LAG if no more ports in LAG.
 *
 *****************************************************************************/
fm_status fmGetLAGPortNext(fm_int  lagNumber,
                           fm_int  currentPort,
                           fm_int  currentSwitch,
                           fm_int *nextPort,
                           fm_int *nextSwitch)
{
    fm_status err;
    fm_int    sw;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG,
                     "lagNumber = %d, currentPort = %d, currentSwitch = %d, "
                     "nextPort = %p, nextSwitch = %p\n",
                     lagNumber,
                     currentPort,
                     currentSwitch,
                     (void *) nextPort,
                     (void *) nextSwitch);
    
    FM_NOT_USED(currentSwitch);

    /* Retrieve the first existing switch number */
    err = fmGetSwitchFirst(&sw);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LAG, err);

    err = fmGetLAGPortNextExt(sw, lagNumber, currentPort, nextPort);

    if (*nextPort == -1)
    {
        *nextSwitch = -1;
    }
    else
    {
        *nextSwitch = sw;
    }

ABORT:
    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmGetLAGPortNext */




/*****************************************************************************/
/** fmGetLAGPortNextExt
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Retrieve the next port in a LAG.
 *
 * \param[in]       sw is the switch number on which to operate.
 *
 * \param[in]       lagNumber is the number of the LAG (returned by
 *                  fmCreateLAG) that should be searched for ports.
 *
 * \param[in]       currentPort is the last port number found by a previous
 *                  call to this function or to fmGetLAGPortFirst.
 *
 * \param[out]      nextPort points to caller-allocated storage where this
 *                  function should place the next port in the LAG.
 *                  Will be set to -1 if no more ports found in LAG.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_LAG if lagNumber is out of range or is
 *                  not the handle of an existing LAG.
 * \return          FM_ERR_INVALID_PORT if nextPort is not a port in lagNumber
 *                  on currentSwitch.
 * \return          FM_ERR_NO_PORTS_IN_LAG if no more ports in LAG.
 *
 *****************************************************************************/
fm_status fmGetLAGPortNextExt(fm_int  sw,
                              fm_int  lagNumber,
                              fm_int  currentPort,
                              fm_int *nextPort)
{
    fm_lag *  lagPtr;
    fm_int    lagIndex;
    fm_int    memberIndex;
    fm_status err;
    fm_int    i;
 
    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG,
                     "sw = %d, lagNumber = %d, currentPort = %d, nextPort = %p\n",
                     sw,
                     lagNumber,
                     currentPort,
                     (void *) nextPort);

    VALIDATE_AND_PROTECT_SWITCH(sw);
    TAKE_LAG_LOCK(sw);

    *nextPort = -1;

    /* Get the internal index for this LAG. */
    lagIndex = fmGetLagIndex(sw, lagNumber);
    if (lagIndex < 0)
    {
        err = FM_ERR_INVALID_LAG;
        goto ABORT;
    }

    lagPtr = GET_LAG_PTR(sw, lagIndex);

    /* Find the previous port in the member list. */
    memberIndex = fmGetPortMemberIndex(sw, currentPort);
    if (memberIndex < 0)
    {
        err = FM_ERR_INVALID_PORT;
        goto ABORT;
    }

    /* Find the next port in the member list. */
    err = FM_ERR_NO_PORTS_IN_LAG;

    for (i = memberIndex + 1 ; i < FM_MAX_NUM_LAG_MEMBERS ; ++i)
    {
        if (lagPtr->member[i].port != FM_LAG_UNUSED_PORT)
        {
            *nextPort = lagPtr->member[i].port;
            err = FM_OK;
            break;
        }
    }

ABORT:
    DROP_LAG_LOCK(sw);
    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmGetLAGPortNextExt */




/*****************************************************************************/
/** fmGetLAGAttribute
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Get a link aggregation group attribute.
 *
 * \note            This function operates on the first switch in the system.
 *                  To use on an arbitrary switch, use ''fmGetLAGAttributeExt''.
 *
 * \param[in]       attribute is the link aggregation group attribute
 *                  (see 'Link Aggregation Group Attributes') to get.
 *
 * \param[in]       index is an attribute-specific index. See
 *                  ''Link Aggregation Group Attributes'' to determine the
 *                  use of this argument.
 *
 * \param[out]      value points to caller-allocated storage where this
 *                  function should place the attribute value.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if no switches are up.
 * \return          FM_ERR_INVALID_ATTRIB if unrecognized attribute.
 * \return          FM_ERR_UNSUPPORTED if attribute is not supported.
 *
 *****************************************************************************/
fm_status fmGetLAGAttribute(fm_int attribute, fm_int index, void *value)
{
    fm_status err;
    fm_int    sw;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG,
                     "attribute = %d, index = %d, value = %p\n",
                     attribute,
                     index,
                     (void *) value);

    /* Retrieve the first existing switch number */
    err = fmGetSwitchFirst(&sw);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LAG, err);

    err = fmGetLAGAttributeExt(sw, attribute, index, value);

ABORT:
    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmGetLAGAttribute */




/*****************************************************************************/
/** fmGetLAGAttributeExt
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Get a link aggregation group attribute.
 *
 * \param[in]       sw is the switch number on which to operate.
 *
 * \param[in]       attribute is the link aggregation group attribute
 *                  (see 'Link Aggregation Group Attributes') to get.
 *
 * \param[in]       index is an attribute-specific index. See
 *                  ''Link Aggregation Group Attributes'' to determine the
 *                  use of this argument.
 *
 * \param[out]      value points to caller-allocated storage where this
 *                  function should place the attribute value.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH sw is invalid or switch is not up.
 * \return          FM_ERR_INVALID_ATTRIB if unrecognized attribute.
 * \return          FM_ERR_UNSUPPORTED if attribute is not supported.
 *
 *****************************************************************************/
fm_status fmGetLAGAttributeExt(fm_int sw,
                               fm_int attribute,
                               fm_int index,
                               void * value)
{
    fm_switch *switchPtr;
    fm_status  err;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG,
                     "sw = %d, attribute = %d, index = %d, value = %p\n",
                     sw,
                     attribute,
                     index,
                     (void *) value);

    VALIDATE_AND_PROTECT_SWITCH(sw);
    TAKE_LAG_LOCK(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY(err,
                       switchPtr->GetLagAttribute,
                       sw,
                       attribute,
                       index,
                       value);

    DROP_LAG_LOCK(sw);
    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmGetLAGAttributeExt */




/*****************************************************************************/
/** fmSetLAGAttribute
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Set a link aggregation group attribute.
 *                  Deprecated in favor of ''fmSetLAGAttributeExt''.
 *
 * \note            This function operates on the first switch in the system.
 *                  To use on an arbitrary switch, use ''fmSetLAGAttributeExt''.
 *
 * \param[in]       attribute is the link aggregation group attribute
 *                  (see 'Link Aggregation Group Attributes') to set.
 *
 * \param[in]       index is an attribute-specific index. See
 *                  ''Link Aggregation Group Attributes'' to determine the
 *                  use of this argument.
 *
 * \param[in]       value points to the attribute value to set.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ATTRIB if unrecognized attribute.
 * \return          FM_ERR_UNSUPPORTED if attribute is not supported.
 *
 *****************************************************************************/
fm_status fmSetLAGAttribute(fm_int attribute, fm_int index, void *value)
{
    fm_status err;
    fm_int    sw;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG,
                     "attribute = %d, index = %d, value = %p\n",
                     attribute,
                     index,
                     (void *) value);

    /* Retrieve the first existing switch number */
    err = fmGetSwitchFirst(&sw);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LAG, err);

    err = fmSetLAGAttributeExt(sw, attribute, index, value);

ABORT:
    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmSetLAGAttribute */




/*****************************************************************************/
/** fmSetLAGAttributeExt
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Set a link aggregation group attribute.
 *
 * \param[in]       sw is the switch number on which to operate.
 *
 * \param[in]       attribute is the link aggregation group attribute
 *                  (see 'Link Aggregation Group Attributes') to set.
 *
 * \param[in]       index is an attribute-specific index. See
 *                  ''Link Aggregation Group Attributes'' to determine the
 *                  use of this argument.
 *
 * \param[in]       value points to the attribute value to set.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH sw is invalid or switch is not up.
 * \return          FM_ERR_INVALID_ATTRIB if unrecognized attribute.
 * \return          FM_ERR_UNSUPPORTED if attribute is not supported.
 *
 *****************************************************************************/
fm_status fmSetLAGAttributeExt(fm_int sw,
                               fm_int attribute,
                               fm_int index,
                               void * value)
{
    fm_switch *switchPtr;
    fm_status  err;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG,
                     "sw = %d, attribute = %d, index = %d, *value = %u\n",
                     sw,
                     attribute,
                     index,
                     *(fm_uint32 *) value);

    VALIDATE_AND_PROTECT_SWITCH(sw);
    TAKE_LAG_LOCK(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY(err,
                       switchPtr->SetLagAttribute,
                       sw,
                       attribute,
                       index,
                       value);

    DROP_LAG_LOCK(sw);
    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_LAG, err);

}   /* end fmSetLAGAttributeExt */




/*****************************************************************************/
/** fmLogicalPortToLAGNumber
 * \ingroup lag
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Convert a LAG's logical port number to the LAG number
 *                  generated by ''fmCreateLAG'' or ''fmCreateLAGExt''.
 *
 * \note            While this function is supported for all devices, it need
 *                  only be used for FM2000 devices. On all other devices,
 *                  the LAG number returned by ''fmCreateLAG'' and 
 *                  ''fmCreateLAGExt'' is always identical to the LAG's logical 
 *                  port number, so this function just acts as an identify 
 *                  function. 
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       logicalPort is the LAG's logical port number.
 *
 * \param[out]      lagNumber points to caller allocated storage where the
 *                  value of the LAG number will be written by this function.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_PORT if logicalPort is not the logical port
 *                  of a LAG.
 *
 *****************************************************************************/
fm_status fmLogicalPortToLAGNumber(fm_int  sw,
                                   fm_int  logicalPort,
                                   fm_int *lagNumber)
{
    fm_port * portPtr;
    fm_status err = FM_OK;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG,
                     "sw = %d, logicalPort = %d\n",
                     sw,
                     logicalPort);

    VALIDATE_AND_PROTECT_SWITCH(sw);
    VALIDATE_LOGICAL_PORT(sw, logicalPort, ALLOW_LAG | ALLOW_REMOTE);
    TAKE_LAG_LOCK(sw);

    portPtr = GET_PORT_PTR(sw, logicalPort);

    /* Make sure it is a LAG logical port number */
    if (portPtr->portType != FM_PORT_TYPE_LAG)
    {
        err = FM_ERR_INVALID_PORT;
        *lagNumber = -1;
    }
    else
    {
        /* The lag number is the lag logical port. */
        *lagNumber = logicalPort;
    }

    DROP_LAG_LOCK(sw);
    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API_CUSTOM(FM_LOG_CAT_LAG,
                           err,
                           "lagNumber = %d\n",
                           *lagNumber);

}   /* end fmLogicalPortToLAGNumber */




/*****************************************************************************/
/** fmLAGNumberToLogicalPort
 * \ingroup  lag 
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Convert the LAG number generated by ''fmCreateLAG'' or
 *                  ''fmCreateLAGExt'' to the LAG's logical port number.
 *
 * \note            While this function is supported for all devices, it need
 *                  only be used for FM2000 devices. On all other devices,
 *                  the LAG number returned by ''fmCreateLAG'' and 
 *                  ''fmCreateLAGExt'' is always identical to the LAG's logical 
 *                  port number, so this function just acts as an identify 
 *                  function. 
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       lagNumber is the LAG number returned by ''fmCreateLAG'' or
 *                  ''fmCreateLAGExt''.
 *
 * \param[out]      logicalPort points to caller allocated storage where the
 *                  LAG's logicalPort will be written by this function.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_LAG if lagNumber is not valid.
 *
 *****************************************************************************/
fm_status fmLAGNumberToLogicalPort(fm_int  sw,
                                   fm_int  lagNumber,
                                   fm_int *logicalPort)
{
    fm_port * portPtr;
    fm_status err = FM_OK;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LAG,
                     "sw = %d, lagNumber = %d\n",
                     sw,
                     lagNumber);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    /* lagNumber contains a LAG logical port.*/
    VALIDATE_LOGICAL_PORT(sw, lagNumber, ALLOW_LAG | ALLOW_REMOTE);
    TAKE_LAG_LOCK(sw);

    portPtr = GET_PORT_PTR(sw, lagNumber);

    /* Make sure it is a LAG logical port number */
    if (portPtr->portType != FM_PORT_TYPE_LAG)
    {
        err = FM_ERR_INVALID_LAG;
        *logicalPort = -1;
    }
    else
    {
        /* The lag number is the lag logical port. */
        *logicalPort = lagNumber;
    }

    DROP_LAG_LOCK(sw);
    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API_CUSTOM(FM_LOG_CAT_LAG,
                           err,
                           "logicalPort = %d\n",
                           *logicalPort);

}   /* end fmLAGNumberToLogicalPort */
