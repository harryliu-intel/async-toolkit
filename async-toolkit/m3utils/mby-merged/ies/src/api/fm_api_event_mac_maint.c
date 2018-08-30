/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_event_mac_maint.c
 * Creation Date:   2005
 * Description:     Contains driver level functions related to table update
 *                  handling as well as the table maintenance thread
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

/* MAC maintenance statistic. */
typedef struct
{
    fm_text                 name;
    fm_trackingCounterIndex index;

} macMaintStat;


/*****************************************************************************
 * Global Variables
 *****************************************************************************/


/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/* MAC maintenance statistics table. */
static macMaintStat MacMaintStatTable[] =
{
    { "Flush Dynamic Addresses:",   FM_CTR_MAC_WORK_FLUSH_DYN_ADDR },
    { "Update Overflows:",          FM_CTR_MAC_WORK_UPD_OVFLW },
    { "Periodic Scans:",            FM_CTR_MAC_WORK_PERIODIC_SCAN },
    { "Purge Requests:",            FM_CTR_MAC_WORK_HANDLE_PURGE },
    { "Port Address Flushes:",      FM_CTR_MAC_WORK_PORT_ADDR_FLUSH },
    { "Port ACL Updates:",          FM_CTR_MAC_WORK_PORT_ACL_UPDATE },
    { "Vlan Address Flushes:",      FM_CTR_MAC_WORK_VLAN_ADDR_FLUSH },
    { "Vlan Port Address Flushes:", FM_CTR_MAC_WORK_VLAN_PORT_ADDR_FLUSH },
    { "MAC FIFO Services:",         FM_CTR_MAC_WORK_SERVICE_FIFO },
    { "MAC FIFO Events:",           FM_CTR_MAC_WORK_FIFO_EVENTS },
    { NULL, FM_SWITCH_CTR_MAX },
};


/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/


/*****************************************************************************
 * Local Functions
 *****************************************************************************/


/*****************************************************************************/
/** AllocWorkList
 * \ingroup intMacMaint
 *
 * \desc            Allocates the data structures for a single MAC Table 
 *                  work list.
 *
 * \param[in]       switchPtr points to the switch state structure being
 *                  initialized.
 *
 * \param[out]      workList points to the MAC Table work list being 
 *                  initialized.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status AllocWorkList(fm_switch *              switchPtr,
                               fm_addrMaintWorkList *   workList)
{
    fm_status result;
    fm_int    portCount;

    FM_CLEAR(*workList);

    portCount = switchPtr->numCardinalPorts;

    result = fmCreateBitArray(&workList->portAddressFlushArray, portCount);

    if (result == FM_OK)
    {
        result = fmCreateBitArray(&workList->portAclUpdateArray, portCount);
    }

    if (result == FM_OK)
    {
        result = fmCreateBitArray(&workList->vlanAddressFlushArray, 
                                  switchPtr->vlanTableSize);
    }

    if (result == FM_OK)
    {
        result = fmCreateBitArray(&workList->vlanPortAddressFlushArray,
                                  switchPtr->vlanTableSize * portCount);
    }

    return result;

}   /* end AllocWorkList */




/*****************************************************************************/
/** FreeWorkList
 * \ingroup intMacMaint
 *
 * \desc            Frees the data structures for a single MAC Table work list.
 *
 * \param[in]       workList points to the MAC Table work list being freed.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status FreeWorkList(fm_addrMaintWorkList * workList)
{
    fm_status result;

    result = fmDeleteBitArray(&workList->portAddressFlushArray);

    if (result == FM_OK)
    {
        result = fmDeleteBitArray(&workList->portAclUpdateArray);
    }

    if (result == FM_OK)
    {
        result = fmDeleteBitArray(&workList->vlanAddressFlushArray);
    }

    if (result == FM_OK)
    {
        result = fmDeleteBitArray(&workList->vlanPortAddressFlushArray);
    }

    return result;

}   /* end FreeWorkList */




/*****************************************************************************/
/** ResetWorkList
 * \ingroup intMacMaint
 *
 * \desc            Resets the data structures for a single MAC Table 
 *                  work list.
 *
 * \param[in]       workList points to the MAC Table work list being reset.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status ResetWorkList(fm_addrMaintWorkList * workList)
{
    fm_status result;

    workList->maintFlags = 0;

    result = fmClearBitArray(&workList->portAddressFlushArray);

    if (result == FM_OK)
    {
        result = fmClearBitArray(&workList->portAclUpdateArray);
    }

    if (result == FM_OK)
    {
        result = fmClearBitArray(&workList->vlanAddressFlushArray);
    }

    if (result == FM_OK)
    {
        result = fmClearBitArray(&workList->vlanPortAddressFlushArray);
    }

    return result;

}   /* end ResetWorkList */




/*****************************************************************************/
/** fmInitializeTableUpdateStats
 * \ingroup intMacMaint
 *
 * \desc            Initialize the table update diagnostic statistics.
 *
 * \return          None.
 *
 *****************************************************************************/
void fmInitializeTableUpdateStats(void)
{
    fm_int i;

    FM_LOG_ENTRY_NOARGS(FM_LOG_CAT_EVENT_MAC_MAINT);

    fmRootApi->tableUpdateStats.index   = 0;
    fmRootApi->tableUpdateStats.maxTime = 0;
    fmRootApi->tableUpdateStats.minTime = (fm_uint) -1;

    for (i = 0 ; i < TABLE_UPDATE_STATS_HISTORY_SIZE ; i++)
    {
        fmRootApi->tableUpdateStats.history[i] = (fm_uint) -1;
    }

    FM_LOG_EXIT_VOID(FM_LOG_CAT_EVENT_MAC_MAINT);

}   /* end fmInitializeTableUpdateStats */




/*****************************************************************************/
/** fmScanMATable
 * \ingroup intMacMaint
 *
 * \desc            Scans the MA table, reconciling any differences between
 *                  the hardware and the software cache.
 *
 *                  Called by the maintenance handler. This is a generic
 *                  routine. It will be superseded if the switch has its own
 *                  ScanMATable function.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       workList points to the list of work to be performed.
 *
 * \param[in]       eventHandler points to the handler to send events to.
 *
 * \return          None.
 *
 *****************************************************************************/
static void fmScanMATable(fm_int                sw,
                          fm_addrMaintWorkList* workList,
                          fm_thread *           eventHandler)
{
    fm_switch * switchPtr;
    fm_event *  outEvent;
    fm_uint32   numUpdates;
    fm_bool     needToScanAgain;
    fm_int      entryIndex;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_MAC_MAINT,
                 "sw = %d, maintFlags = %04x\n",
                 sw,
                 workList->maintFlags);

    switchPtr = GET_SWITCH_PTR(sw);

    /**************************************************
     * Do not pre-allocate an event buffer, as 
     * fmGenerateUpdateForEvent will allocate one when needed
     **************************************************/

    outEvent        = NULL;
    numUpdates      = 0;
    needToScanAgain = FALSE;

    fmDbgDiagCountIncr(sw, FM_CTR_MAC_SCAN_STARTED, 1);

    /**************************************************
     * For each entry in the MA Table...
     **************************************************/

    /* Clear flags that may be set during entry processing. */
    workList->maintFlags &= ~FM_MAC_MAINT_INTERNAL_REQUEST;

    /* Pick up where we left off last time. */
    entryIndex = switchPtr->macTableScanIndex;

    for ( ; ; )
    {
        /* TODO: replace needToScanAgain with bit in maintFlags. */
        needToScanAgain |= switchPtr->ProcessMATableEntry(sw,
                                                          entryIndex,
                                                          workList,
                                                          eventHandler,
                                                          &numUpdates,
                                                          &outEvent);

        /* MA Table scan suspended? */
        if (workList->maintFlags & FM_MAC_MAINT_SUSPEND_SCAN)
        {
            /* Yes, start the next scan with the current entry. */
            switchPtr->macTableScanIndex = entryIndex;
            fmDbgDiagCountIncr(sw, FM_CTR_MAC_SCAN_SUSPENDED, 1);
            break;
        }

        /* Count the number of entry scans performed. */
        fmDbgDiagCountIncr(sw, FM_CTR_MAC_ENTRY_SCANNED, 1);

        /* Increment the entry index, wrapping to zero. */
        ++entryIndex;
        if (entryIndex >= switchPtr->macTableSize)
        {
            entryIndex = 0;
        }

        /* Terminate scan when it returns to the starting point. */
        if (entryIndex == switchPtr->macTableScanIndex)
        {
            fmDbgDiagCountIncr(sw, FM_CTR_MAC_SCAN_FINISHED, 1);
            break;
        }

    }   /* end for ( ;; ) */

    /**************************************************
     * After scanning the whole MA Table, if there
     * are any updates that have been prepped but not
     * yet sent to the API, then send them now.
     **************************************************/

    if (numUpdates > 0)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_MAC_MAINT,
                     "finished scan: numUpdates=%d\n", 
                     numUpdates);

        fmSendMacUpdateEvent(sw,
                             eventHandler,
                             &numUpdates,
                             &outEvent,
                             FALSE);
    }

    if (outEvent != NULL)
    {
        fmReleaseEvent(outEvent);
    }

    /**************************************************
     * Do we need to scan table again?
     **************************************************/

    if (needToScanAgain)
    {
        fm_maWorkTypeData data;
        FM_CLEAR(data);
        fmAddMacTableMaintenanceWork(sw, FM_UPD_SYNC_CACHE, data, NULL, NULL);
    }

    FM_LOG_EXIT_VOID(FM_LOG_CAT_EVENT_MAC_MAINT);

}   /* end fmScanMATable */




/*****************************************************************************/
/** UpdateWorkStatistics
 * \ingroup intMacMaint
 *
 * \desc            Inspects the work list and updates the statistics for the
 *                  requested maintenance operations.
 *
 *                  Called by the maintenance handler.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       workList points to the list of work to be performed.
 *
 * \return          None.
 *
 *****************************************************************************/
static void UpdateWorkStatistics(fm_int sw, fm_addrMaintWorkList* workList)
{
    fm_int  numTasks = 0;
    fm_int  bitCount;

    if (workList->maintFlags & FM_MAC_MAINT_FLUSH_DYN_ADDR)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_MAC_MAINT,
                     "servicing DynamicAddressFlush\n");
        fmDbgDiagCountIncr(sw, FM_CTR_MAC_WORK_FLUSH_DYN_ADDR, 1);
        numTasks++;
    }

    if (workList->maintFlags & FM_MAC_MAINT_HANDLE_OVERFLOW)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_MAC_MAINT,
                     "servicing UpdateOverflow\n");

        fmDbgDiagCountIncr(sw, FM_CTR_MAC_WORK_UPD_OVFLW, 1);
        numTasks++;
    }

    if (workList->maintFlags & FM_MAC_MAINT_SYNC_CACHE)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_MAC_MAINT, "servicing PeriodicScan\n");
        fmDbgDiagCountIncr(sw, FM_CTR_MAC_WORK_PERIODIC_SCAN, 1);
        numTasks++;
    }

    if (workList->maintFlags & FM_MAC_MAINT_SERVICE_FIFO)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_MAC_MAINT, "servicing TCN FIFO\n");
        fmDbgDiagCountIncr(sw, FM_CTR_MAC_WORK_SERVICE_FIFO, 1);
        numTasks++;
    }

    if (workList->maintFlags & FM_MAC_MAINT_HANDLE_PURGE)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_MAC_MAINT, "handling purge request\n");
        fmDbgDiagCountIncr(sw, FM_CTR_MAC_WORK_HANDLE_PURGE, 1);
        numTasks++;
    }

    if (workList->maintFlags & FM_MAC_MAINT_PURGE_COMPLETE)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_MAC_MAINT, "handling purge request\n");
        fmDbgDiagCountIncr(sw, FM_CTR_PURGE_COMPLETE, 1);
        numTasks++;
    }

    if (workList->maintFlags & FM_MAC_MAINT_FLUSH_PORT)
    {
        bitCount = 0;
        fmGetBitArrayNonZeroBitCount(&workList->portAddressFlushArray, &bitCount);

        if (bitCount > 0)
        {
            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_MAC_MAINT,
                         "servicing PortAddressFlush\n");
            fmDbgDiagCountIncr(sw, FM_CTR_MAC_WORK_PORT_ADDR_FLUSH, 1);
            numTasks += bitCount;
        }
    }

    if (workList->maintFlags & FM_MAC_MAINT_UPDATE_ACL)
    {
        bitCount = 0;
        fmGetBitArrayNonZeroBitCount(&workList->portAclUpdateArray, &bitCount);

        if (bitCount > 0)
        {
            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_MAC_MAINT,
                         "servicing PortAclUpdate\n");
            fmDbgDiagCountIncr(sw, FM_CTR_MAC_WORK_PORT_ACL_UPDATE, 1);
            numTasks += bitCount;
        }
    }

    if (workList->maintFlags & FM_MAC_MAINT_FLUSH_VLAN)
    {
        bitCount = 0;
        fmGetBitArrayNonZeroBitCount(&workList->vlanAddressFlushArray, &bitCount);

        if (bitCount > 0)
        {
            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_MAC_MAINT,
                         "servicing VlanAddressFlush\n");
            fmDbgDiagCountIncr(sw, FM_CTR_MAC_WORK_VLAN_ADDR_FLUSH, 1);
            numTasks += bitCount;
        }
    }

    if (workList->maintFlags & FM_MAC_MAINT_FLUSH_VLAN_PORT)
    {
        bitCount = 0;
        fmGetBitArrayNonZeroBitCount(&workList->vlanPortAddressFlushArray, &bitCount);

        if (bitCount > 0)
        {
            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_MAC_MAINT,
                         "servicing VlanPortAddressFlush\n");
            fmDbgDiagCountIncr(sw, FM_CTR_MAC_WORK_VLAN_PORT_ADDR_FLUSH, 1);
            numTasks += bitCount;
        }
    }

    fmDbgDiagCountIncr(sw, FM_CTR_MAC_WORK_TOTAL_TASKS, numTasks);

    if (numTasks > fmRootApi->macTableMaintMaxTasks[sw])
    {
        fmRootApi->macTableMaintMaxTasks[sw] = numTasks;
    }

}   /* end UpdateWorkStatistics */




/*****************************************************************************/
/** fmCommonHandleScanRequest
 * \ingroup intMacMaint
 *
 * \desc            For FM2000 and FM4000 only.                             \lb
 *                  Performs a maintenance scan of the MA Table and cache.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       workList points to the list of work to be performed.
 *
 * \param[in]       thread is the maintenance thread handle.
 *
 * \param[in]       eventHandler points to the handler to send events to.
 *
 * \return          None.
 *
 *****************************************************************************/
void fmCommonHandleScanRequest(fm_int                sw,
                               fm_addrMaintWorkList* workList,
                               fm_thread*            thread,
                               fm_thread*            eventHandler)
{
    fm_switch *             switchPtr;
    fm_status               timeErr;
    fm_timestamp            startTime;
    fm_timestamp            endTime;
    fm_uint                 deltaTime;
    fm_status               err;

    FM_NOT_USED(thread);
    
    switchPtr = GET_SWITCH_PTR(sw);
    
    FM_LOG_ABORT_ON_ASSERT(FM_LOG_CAT_EVENT_MAC_MAINT,
                           switchPtr->switchFamily != FM_SWITCH_FAMILY_FM6000,
                           (void)err,
                           "FM2000/FM4000-only function called for FM6000\n");
    
    /**************************************************
     * Keep track of how long it takes to do a scan.
     **************************************************/

    timeErr = fmGetTime(&startTime);

    /**************************************************
     * Do any necessary initialization.
     **************************************************/

    FM_API_CALL_FAMILY(err, switchPtr->MacTableOverflowStart, sw);

    /**************************************************
     * Scan the whole MA Table (potentially multiple
     * times until no more differences are seen).
     **************************************************/

    if (switchPtr->ScanMATable != NULL)
    {
        switchPtr->ScanMATable(sw, workList, eventHandler);
    }
    else
    {
        fmScanMATable(sw, workList, eventHandler);
    }

    /**************************************************
     * Determine how long it took to synchronize the
     * table.
     **************************************************/

    if (timeErr == FM_OK)
    {
        if (fmGetTime(&endTime) == FM_OK)
        {
            err = fmCaptureLock(&fmRootApi->tableUpdateStats.lck, 
                                FM_WAIT_FOREVER);
            FM_THREAD_ERR_CHECK(err);

            if (err == FM_OK)
            {
                deltaTime = (fm_uint) ( ( (endTime.sec *
                                           1000000) + endTime.usec )
                                       - ( (startTime.sec *
                                            1000000) + startTime.usec ) );

                fmRootApi->tableUpdateStats.
                    history[fmRootApi->tableUpdateStats.index] = deltaTime;

                /**************************************************
                 * Capture high and low watermarks.
                 **************************************************/

                if (fmRootApi->tableUpdateStats.maxTime < deltaTime)
                {
                    fmRootApi->tableUpdateStats.maxTime = deltaTime;
                }

                if (fmRootApi->tableUpdateStats.minTime > deltaTime)
                {
                    fmRootApi->tableUpdateStats.minTime = deltaTime;
                }

                ++fmRootApi->tableUpdateStats.index;
                fmRootApi->tableUpdateStats.index %= TABLE_UPDATE_STATS_HISTORY_SIZE;

                err = fmReleaseLock(&fmRootApi->tableUpdateStats.lck);
                FM_THREAD_ERR_CHECK(err);

            }   /* end if (err == FM_OK) */

        }   /* end if (fmGetTime(&endTime) == FM_OK) */

    }   /* end if (timeErr == FM_OK) */
    
ABORT:
    return;

}   /* end fmCommonHandleScanRequest */




/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** fmTableMaintenanceHandler
 * \ingroup intMacMaint
 *
 * \desc            This task runs in its own thread, reading the entire
 *                  MA Table, synchronizing the cached table with the
 *                  hardware and reporting changes to the application
 *                  as an update.
 *
 *                  Cases where this thread is invoked to scan the table:
 *
 *                  * When the table update FIFO overflows.
 *
 *                  * To address a silicon problem whereby station moves
 *                    do not generate a table update interrupt (when such
 *                    events are not trapped as security violations).
 *                    Thus, the entire table must be scanned for changes on
 *                    a frequent periodic basis.
 *
 *                  * When a port goes down (as detected by the driver), 
 *                    we must report age events to the application for all
 *                    table entries on that port.
 *
 *                  * When an ACL rule is changed for a port, all table
 *                    entries on that port must be updated with the new rule
 *                    (i.e. trigger).
 *
 *                  * When a VLAN is deleted, all table entries on that
 *                    VLAN must be deleted.
 *
 * \param[in]       args contains the pointer to the thread argument array
 *
 * \return          None.
 *
 *****************************************************************************/
void *fmTableMaintenanceHandler(void *args)
{
    fm_thread *           thread;
    fm_thread *           eventHandler;
    fm_int                sw;
    fm_switch *           switchPtr;
    fm_bool               swIsProtected;
    fm_status             err = FM_OK;
    fm_addrMaintWorkList *workList;
    fm_timestamp          maintenanceTimeout;
    fm_timestamp *        maintTimeoutPtr;
    fm_uint64             lastTicks = 0;
    fm_uint64             curTicks;
    fm_timestamp          curTime;
    fm_lockPrecedence *   lockUsage;
#if FM_SUPPORT_SWAG       
    fm_int                aggSw;
    fm_bool               aggSwIsProtected;
    fm_switch *           aggSwPtr;
#endif

    maintenanceTimeout.sec = FM_API_MAC_TABLE_MAINT_DELAY - 
                             FM_API_MAC_TABLE_MAINT_THROTTLE;
    maintenanceTimeout.usec = 0;
    
    maintTimeoutPtr = &maintenanceTimeout;
    
    if ( !fmPlatformMACMaintenanceSupported(-1) )
    {
        maintTimeoutPtr = FM_WAIT_FOREVER;
    }

    /* grab arguments */
    thread       = FM_GET_THREAD_HANDLE(args);
    eventHandler = FM_GET_THREAD_PARAM(fm_thread, args);

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_MAC_MAINT,
                 "thread = %s, eventHandler = %s\n",
                 thread->name,
                 eventHandler->name);

    /* clear the maintenance statistics */
    FM_CLEAR(fmRootApi->macTableMaintMaxTasks);

    /**************************************************
     * Main task loop.
     **************************************************/

    sw = FM_FIRST_FOCALPOINT - 1;
    swIsProtected = FALSE;
#if FM_SUPPORT_SWAG
    aggSwIsProtected = FALSE;
    aggSw = -1;
#endif

    while (1)
    {
        if (swIsProtected)
        {
            UNPROTECT_SWITCH(sw);
            swIsProtected = FALSE;
        }

#if FM_SUPPORT_SWAG
        if (aggSwIsProtected)
        {
            UNPROTECT_SWITCH(aggSw);
            aggSwIsProtected = FALSE;
        }
#endif

        lockUsage = fmGetCurrentThreadLockCollection();

        if (lockUsage != NULL)
        {
            if ( (sw >= FM_FIRST_FOCALPOINT) && (lockUsage[sw] != 0) )
            {
                FM_LOG_ERROR(FM_LOG_CAT_EVENT_MAC_MAINT,
                             "switch %d still holding a lock, usage=0x%X\n",
                             sw,
                             lockUsage[sw]);
            }

#if FM_SUPPORT_SWAG
            if ( (aggSw >= 0) && (lockUsage[aggSw] != 0) )
            {
                FM_LOG_ERROR(FM_LOG_CAT_EVENT_MAC_MAINT,
                             "swag switch %d still holding a lock, usage=0x%X\n",
                             aggSw,
                             lockUsage[aggSw]);
            }
#endif
        }

        /* Check the next switch.  Wrap back to first switch when
         *  all switches have been checked.  If back to the first switch,
         *  wait for something to do or for a timeout before looping through
         *  all switches again. */
        if (++sw > FM_LAST_FOCALPOINT)
        {
            sw = FM_FIRST_FOCALPOINT;
        }

        if (sw == FM_FIRST_FOCALPOINT)
        {
            /*******************************************************
             * we will at least wait for FM_MA_TABLE_MAINT_THROTTLE
             * seconds before the thread starts to run. This is set
             * to 0 in the API, but a user application can change it
             * according to its need.
             ******************************************************/
            fmDelay(FM_API_MAC_TABLE_MAINT_THROTTLE, 0);

            /* wait for something to do, time out as often as configured */
            err = fmWaitSemaphore(&fmRootApi->macTableMaintSemaphore,
                                  maintTimeoutPtr);
            if ( (err != FM_OK) && (err != FM_ERR_SEM_TIMEOUT) )
            {
                FM_LOG_ERROR( FM_LOG_CAT_EVENT_MAC_MAINT,
                              "Unexpected error from fmWaitSemaphore: %s\n",
                              fmErrorMsg(err) );
            }
        }

        if ( !SWITCH_LOCK_EXISTS(sw) )
        {
            continue;
        }

        /* Take a lock on the physical switch */
        PROTECT_SWITCH(sw);
        swIsProtected = TRUE;

        switchPtr = GET_SWITCH_PTR(sw);

#if FM_SUPPORT_SWAG

AGAIN:

#endif

        if (!switchPtr || 
            (switchPtr->state != FM_SWITCH_STATE_UP) ||
            !fmPlatformMACMaintenanceSupported(sw))
        {
            continue;
        }

#if FM_SUPPORT_SWAG
        /* See if the switch is part of a switch aggregate */
        if (switchPtr->swag >= 0)
        {
            /* Get the switch aggregate id */
            aggSw = switchPtr->swag;

            /* Drop the physical switch lock */
            UNPROTECT_SWITCH(sw);
            swIsProtected = FALSE;

            /* Take the lock on the switch aggregate */
            PROTECT_SWITCH(aggSw);
            aggSwIsProtected = TRUE;
            aggSwPtr = GET_SWITCH_PTR(aggSw);

            if (aggSwPtr == NULL)
            {
                UNPROTECT_SWITCH(aggSw);
                aggSwIsProtected = FALSE;
            }

            /* Take the lock on the physical switch again */
            PROTECT_SWITCH(sw);
            swIsProtected = TRUE;

            switchPtr = GET_SWITCH_PTR(sw);

            /* Make sure the physical switch is still a member of the SWAG */
            if ( (switchPtr == NULL) || (switchPtr->swag != aggSw) )
            {
                /* It isn't, drop the swag lock and try again. */
                UNPROTECT_SWITCH(aggSw);
                aggSwIsProtected = FALSE;
                goto AGAIN;
            }
            
        }   /* end if (switchPtr->swag >= 0) */
        else
        {
            aggSw = -1;
        }
        
#endif
        
        /***************************************************
         * Perform any general maintenance.
         **************************************************/

        if (switchPtr->MiscMACMaintenance)
        {
            switchPtr->MiscMACMaintenance(sw);
        }

        /**************************************************
         * Get the work list.
         **************************************************/

        fmDbgDiagCountIncr(sw, FM_CTR_MAC_WORK_POLL_COUNT, 1);

        /* Take exclusive access to the work list */
        err = TAKE_MAC_MAINT_LOCK(sw);

        if (err != FM_OK)
        {
            continue;
        }

        /* Grab the currently pending work list */
        workList = switchPtr->pPendingWorkList;

        /* swap the work lists */
        if (workList == &switchPtr->workList1)
        {
            switchPtr->pPendingWorkList = &switchPtr->workList2;
        }
        else
        {
            switchPtr->pPendingWorkList = &switchPtr->workList1;
        }

        /* clear out the new pending work list */
        ResetWorkList(switchPtr->pPendingWorkList);

        /* Release the work list lock */
        DROP_MAC_MAINT_LOCK(sw);

        /**************************************************
         * Get the current time in ticks and calculate the 
         * number of ticks that have elapsed since the last 
         * polling pass.
         **************************************************/

        fmGetTime(&curTime);
        curTicks = fmConvertTimestampToTicks(&curTime);

        if (lastTicks == 0)
        {
            switchPtr->macAgingTickDelta = 0;
        }
        else
        {
            switchPtr->macAgingTickDelta = curTicks - lastTicks;
        }

        lastTicks = curTicks;

        /**************************************************
         * Examine work requests and update statistics.
         **************************************************/

        /* See whether periodic scan is needed */
        if (switchPtr->pollMacTable)
        {
            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_MAC_MAINT, "polling MA table\n");
            workList->maintFlags |= FM_MAC_MAINT_SYNC_CACHE;
        }

        UpdateWorkStatistics(sw, workList);

        /**************************************************
         * Service the TCN FIFO.
         **************************************************/

        if ((workList->maintFlags & FM_MAC_MAINT_SERVICE_FIFO) && 
            (switchPtr->HandleMACTableEvents != NULL) )
        {
            switchPtr->HandleMACTableEvents(sw);
        }

        /**************************************************
         * Service the purge request queue.
         **************************************************/

        if ((workList->maintFlags & FM_MAC_MAINT_HANDLE_PURGE) &&
            (switchPtr->HandlePurgeRequest != NULL))
        {
            switchPtr->HandlePurgeRequest(sw);
        }

        if ((workList->maintFlags & FM_MAC_MAINT_PURGE_COMPLETE) &&
            (switchPtr->HandlePurgeComplete != NULL))
        {
            switchPtr->HandlePurgeComplete(sw);
        }

        /**************************************************
         * Perform a maintenance scan.
         **************************************************/

        if (workList->maintFlags & FM_MAC_MAINT_SCAN_NEEDED &&
            switchPtr->HandleScanRequest != NULL)
        {
            switchPtr->HandleScanRequest(sw, workList, thread, eventHandler);
        }

        /**************************************************
         * If we had been dispatched by the table update
         * handler on a real overflow event, then reenable
         * the overflow interrupt.
         **************************************************/

        if (workList->maintFlags & FM_MAC_MAINT_HANDLE_OVERFLOW)
        {
            err = switchPtr->MacTableOverflowDone(sw);
            FM_THREAD_ERR_CHECK(err);
        }

        if (swIsProtected)
        {
            UNPROTECT_SWITCH(sw);
            swIsProtected = FALSE;
        }

#if FM_SUPPORT_SWAG

        if (aggSwIsProtected)
        {
            UNPROTECT_SWITCH(aggSw);
            aggSwIsProtected = FALSE;
        }

#endif

        lockUsage = fmGetCurrentThreadLockCollection();

        if (lockUsage != NULL)
        {
            if ( (sw >= 0) && (lockUsage[sw] != 0) )
            {
                FM_LOG_ERROR(FM_LOG_CAT_EVENT_MAC_MAINT,
                             "switch %d still holding a lock, usage=0x%X, "
                             "workList flags=0x%X\n",
                             sw,
                             lockUsage[sw],
                             workList->maintFlags);
            }

#if FM_SUPPORT_SWAG
            if ( (aggSw >= 0) && (lockUsage[aggSw] != 0) )
            {
                FM_LOG_ERROR(FM_LOG_CAT_EVENT_MAC_MAINT,
                             "swag switch %d still holding a lock, usage=0x%X, "
                             "sw=%d, workList flags=0x%X\n",
                             aggSw,
                             lockUsage[aggSw],
                             sw,
                             workList->maintFlags);
            }
#endif
        }

        fmYield();

    }   /* end while(1) */

    return NULL;

}   /* end fmTableMaintenanceHandler */




/*****************************************************************************/
/** fmCheckFIDFilter
 * \ingroup intMacMaint
 *
 * \desc            Determine if a received packet should be dropped due to
 *                  the spanning tree state of the port on which it was
 *                  received.  This functionality is needed to work around a
 *                  bug in the silicon whereby LACP frames are not forwarded
 *                  when the port is in any state but forwarding.
 *                  It is implemented with a generic wrapper around chip-
 *                  specific functionality as FM2000 and FM4000 chips
 *                  will require different checking.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       event points to the received packet event structure.
 *
 * \return          TRUE if the packet should be filtered (dropped), FALSE if
 *                  it should be forwarded.
 *
 *****************************************************************************/
fm_bool fmCheckFIDFilter(fm_int sw, fm_eventPktRecv *event)
{
    fm_status  err;
    fm_int     state;
    fm_macaddr destAddr;
    fm_buffer *pktData;
    fm_bool    filter = FALSE;

    /**************************************************
     * Get the spanning tree state of the port for the
     * VLAN on which the packet was recevied.
     **************************************************/

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_MAC_MAINT,
                 "sw = %d, event->vlan = %d, event->srcPort = %d\n",
                 sw,
                 event->vlan,
                 event->srcPort);

    err = fmGetVlanPortStateInternal(sw, event->vlan, event->srcPort, &state);

    if (err == FM_OK)
    {
        /**************************************************
         * Extract the destination address from the
         * packet.
         **************************************************/

        pktData  = event->pkt;
        destAddr = fmGetPacketDestAddr(sw, pktData);

        /**************************************************
         * Filter according to the spanning tree state.
         **************************************************/

        switch (state)
        {
            /**************************************************
             * Listening: only LACP frames, BPDUs and dot1x
             * frames should be forwarded.
             **************************************************/
            case FM_STP_STATE_LISTENING:
            case FM_STP_STATE_BLOCKING:

                if (destAddr != FM_BPDU_DEST_ADDRESS &&
                    destAddr != FM_LACP_DEST_ADDRESS &&
                    destAddr != FM_DOT1X_DEST_ADDRESS)
                {
                    filter = TRUE;
                }

                break;

                /**************************************************
                 * Disabled: only LACP and dot1x frames should be
                 * forwarded.
                 **************************************************/
            case FM_STP_STATE_DISABLED:

                if (destAddr != FM_LACP_DEST_ADDRESS &&
                    destAddr != FM_DOT1X_DEST_ADDRESS)
                {
                    filter = TRUE;
                }

                break;

                /**************************************************
                 * All other states: all frames forwarded.
                 **************************************************/
            default:
                break;

        }   /* end switch (state) */

    }   /* end if (err == FM_OK) */

    FM_LOG_EXIT_CUSTOM( FM_LOG_CAT_EVENT_MAC_MAINT,
                       filter,
                       "%s\n",
                       FM_BOOLSTRING(filter) );

}   /* end fmCheckFIDFilter */




/*****************************************************************************/
/** fmAddUpdateToEvent
 * \ingroup intMacMaint
 *
 * \desc            Adds an update to an event buffer.
 *
 * \note            This function must not block.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       updateType is the type of update.
 *
 * \param[in]       reason is the reason for the update.
 *
 * \param[in]       tableIndex is the MA Table index.
 *
 * \param[in]       updatePtr points to the update structure containing 
 *                  the data to be sent in the event.
 *
 * \param[in,out]   numUpdates points to a variable containing the total 
 *                  number of updates in the event buffer.
 *
 * \param[in,out]   eventPtr points to the event buffer.
 *
 * \return          FM_OK if the update was stored in the event buffer.
 * \return          FM_ERR_BUFFER_FULL if the event buffer is full.
 * \return          FM_ERR_INVALID_ARGUMENT if an argument is invalid.
 *
 *****************************************************************************/
fm_status fmAddUpdateToEvent(fm_int                   sw,
                             fm_int                   updateType,
                             fm_int                   reason,
                             fm_int                   tableIndex,
                             fm_internalMacAddrEntry *updatePtr,
                             fm_uint32 *              numUpdates,
                             fm_event *               eventPtr)
{
    fm_eventTableUpdate * fpUpdateEvent;

    if (numUpdates == NULL || eventPtr == NULL)
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    if (*numUpdates >= FM_TABLE_UPDATE_BURST_SIZE)
    {
        return FM_ERR_BUFFER_FULL;
    }

    fpUpdateEvent = &eventPtr->info.fpUpdateEvent.updates[(*numUpdates)++];

    fpUpdateEvent->event  = updateType;
    fpUpdateEvent->reason = (fm_byte) reason;
    fpUpdateEvent->index  = tableIndex;

    switch (updatePtr->state)
    {
        case FM_MAC_ENTRY_STATE_YOUNG:
        case FM_MAC_ENTRY_STATE_MOVED:
            fpUpdateEvent->age    = 1;
            fpUpdateEvent->locked = FALSE;
            fpUpdateEvent->valid  = TRUE;
            break;

        case FM_MAC_ENTRY_STATE_OLD:
            fpUpdateEvent->age    = 0;
            fpUpdateEvent->locked = FALSE;
            fpUpdateEvent->valid  = TRUE;
            break;

        case FM_MAC_ENTRY_STATE_LOCKED:
            fpUpdateEvent->age    = 0;
            fpUpdateEvent->locked = TRUE;
            fpUpdateEvent->valid  = TRUE;
            break;

        case FM_MAC_ENTRY_STATE_INVALID:
        default:
            fpUpdateEvent->age    = 0;
            fpUpdateEvent->locked = FALSE;
            fpUpdateEvent->valid  = FALSE;
            break;

    }   /* end switch (updatePtr->state) */

    fpUpdateEvent->trigger    = updatePtr->trigger;
    fpUpdateEvent->macAddress = updatePtr->macAddress;
    fpUpdateEvent->destMask   = updatePtr->destMask;
    fpUpdateEvent->port       = updatePtr->port;
    fpUpdateEvent->vlanID     = updatePtr->vlanID;
    fpUpdateEvent->vlanID2    = updatePtr->vlanID2;
    fpUpdateEvent->remoteID   = updatePtr->remoteID;
    fpUpdateEvent->remoteMac  = updatePtr->remoteMac;

    if (fpUpdateEvent->event == FM_EVENT_ENTRY_LEARNED)
    {
        fmDbgDiagCountIncr(sw, FM_CTR_MAC_REPORT_LEARN, 1);

    }
    else if (fpUpdateEvent->event == FM_EVENT_ENTRY_AGED)
    {
        fmDbgDiagCountIncr(sw, FM_CTR_MAC_REPORT_AGE, 1);
    }

    return FM_OK;

}   /* end fmAddUpdateToEvent */




/*****************************************************************************/
/** fmGenerateUpdateForEvent
 * \ingroup intMacMaint
 *
 * \desc            Create an update entry for an event.  If there is no event,
 *                  allocate one. If the event gets filled, send it along to
 *                  its target task.
 *
 * \note            This function may block.
 *
 * \param[in]       sw is the switch number
 *
 * \param[in]       eventHandler points to the event handler to send the
 *                  event to.
 *
 * \param[in]       updateType is the type of update.
 *
 * \param[in]       reason is the reason for the update.
 *
 * \param[in]       tableIndex is the MA Table index.
 *
 * \param[in]       update points to the update structure containing the
 *                  data to be sent in the event.
 *
 * \param[in,out]   numUpdates points to where the total number of updates
 *                  in the current event is stored. Will be incremented
 *                  or reset by this function.
 *
 * \param[in,out]   outEvent points to the pointer to the event buffer.
 *                  Will be reset to NULL if event is sent to the target task.
 *
 * \return          Nothing.
 *
 *****************************************************************************/
void fmGenerateUpdateForEvent(fm_int                   sw,
                              fm_thread *              eventHandler,
                              fm_int                   updateType,
                              fm_int                   reason,
                              fm_int                   tableIndex,
                              fm_internalMacAddrEntry *update,
                              fm_uint32 *              numUpdates,
                              fm_event **              outEvent)
{
    fm_event *eventPtr;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_MAC_MAINT,
                 "sw = %d, eventHandler = %s, updateType = %d, "
                 "tableIndex = %d, " 
                 "update->macAddress = " FM_FORMAT_ADDR ", "
                 "numUpdates = %u, *outEvent = %p\n",
                 sw,
                 (eventHandler ? eventHandler->name : "NULL"),
                 updateType,
                 tableIndex,
                 update->macAddress,
                 *numUpdates,
                 (void *) *outEvent);

    if (eventHandler)
    {
        eventPtr = *outEvent;

        /* Allocate an event buffer. */
        if (eventPtr == NULL)
        {
            /* NOTE: This call will block if the number of free event 
             * buffers drops below the acceptable threshold. */
            eventPtr = fmAllocateEvent(sw,
                                       FM_EVID_HIGH_TABLE_UPDATE,
                                       FM_EVENT_TABLE_UPDATE,
                                       FM_EVENT_PRIORITY_LOW);

            if (eventPtr == NULL)
            {
                /* Only get here when semaphore timeout while blocking.
                 * Only expected to be a rare occurance, and the mac
                 * maintenance code will recover, so no additional logic
                 * is required here.
                 */
                fmDbgDiagCountIncr(sw, FM_CTR_MAC_EVENT_ALLOC_ERR, 1);

                FM_LOG_EXIT_VOID(FM_LOG_CAT_EVENT_MAC_MAINT);
            }

            *outEvent = eventPtr;
            *numUpdates = 0;
        }

        /* If we were called with a full buffer, send it to the
         * event handler and allocate a new one. */
        if (*numUpdates >= FM_TABLE_UPDATE_BURST_SIZE)
        {
            fmSendMacUpdateEvent(sw,
                                 eventHandler,
                                 numUpdates,
                                 outEvent,
                                 TRUE);
            fmYield();
        }

        /* Add the update to the event buffer. */
        fmAddUpdateToEvent(sw,
                           updateType,
                           reason,
                           tableIndex,
                           update,
                           numUpdates,
                           eventPtr);

        /* If the buffer is full, send it to the event handler. */
        if (*numUpdates == FM_TABLE_UPDATE_BURST_SIZE)
        {
            fmSendMacUpdateEvent(sw,
                                 eventHandler,
                                 numUpdates,
                                 outEvent,
                                 FALSE);
            fmYield();
        }

    }   /* end if (eventHandler) */

    FM_LOG_EXIT_VOID(FM_LOG_CAT_EVENT_MAC_MAINT);

}   /* end fmGenerateUpdateForEvent */




/*****************************************************************************/
/** fmSendMacUpdateEvent
 * \ingroup intMacMaint
 *
 * \desc            Button up an event and send it to its target task.
 *                  Then, allocate a new event.
 *
 * \param[in]       sw is the switch number
 *
 * \param[in]       eventHandler points to the event handler target task.
 *
 * \param[in,out]   numUpdates points to where the number of updates being
 *                  sent in the event is stored.  Will be reset by this
 *                  function.
 *
 * \param[in,out]   event points to the pointer to the event to send.
 *                  Will be updated by this function with a freshly
 *                  allocated event.
 *
 * \param[in]       needNewEvent is set to TRUE if a new event should be
 *                  allocated to replace the one consumed, or FALSE if a
 *                  new event is not needed.
 *
 * \return          None.
 *
 *****************************************************************************/
void fmSendMacUpdateEvent(fm_int     sw,
                          fm_thread *eventHandler,
                          fm_uint32 *numUpdates,
                          fm_event **event,
                          fm_bool    needNewEvent)
{
    fm_status err;
    fm_event *eventPtr;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_MAC_MAINT,
                 "sw = %d, eventHandler = %s, *numUpdates = %u, *event = %p, "
                 "needNewEvent = %s\n",
                 sw,
                 (eventHandler ? eventHandler->name : "NULL"),
                 *numUpdates,
                 (void *) *event,
                 FM_BOOLSTRING(needNewEvent) );

    if (eventHandler)
    {
        eventPtr                                = *event;
        *event                                  = NULL;
        eventPtr->info.fpUpdateEvent.numUpdates = *numUpdates;

        err = fmSendThreadEvent(eventHandler, eventPtr);

        if (err != FM_OK)
        {
            FM_LOG_ERROR(FM_LOG_CAT_EVENT_MAC_MAINT,
                         "error: %s\n",
                         fmErrorMsg(err) );

            fmDbgDiagCountIncr(sw, FM_CTR_MAC_EVENT_SEND_ERR, 1);

            /* Free the event since we could not send it to thread */
            fmReleaseEvent(eventPtr);
        }

        if (needNewEvent)
        {
            /* NOTE: This call will block if the number of free event 
             * buffers drops below the acceptable threshold. */
            *event = fmAllocateEvent(sw,
                                     FM_EVID_HIGH_TABLE_UPDATE,
                                     FM_EVENT_TABLE_UPDATE,
                                     FM_EVENT_PRIORITY_LOW);

            if (*event == NULL)
            {
                FM_LOG_ERROR(FM_LOG_CAT_EVENT_MAC_MAINT,
                             "out of event buffers\n");

                fmDbgDiagCountIncr(sw, FM_CTR_MAC_EVENT_ALLOC_ERR, 1);
            }
        }

    }   /* end if (eventHandler) */

    *numUpdates = 0;

    FM_LOG_EXIT_VOID(FM_LOG_CAT_EVENT_MAC_MAINT);

}   /* end fmSendMacUpdateEvent */




/*****************************************************************************/
/* fmSendPurgeScanCompleteEvent
 * \ingroup intMacMaint
 *
 * Description:     Send the Purge Complete event to Global Event Handler
 *
 * \param[in]       sw is the switch number
 *
 * \param[in]       eventHandler points to the event handler target task.
 *
 * \return          None.
 *
 *****************************************************************************/
void fmSendPurgeScanCompleteEvent(fm_int     sw,
                                  fm_thread *eventHandler)
{
    fm_status err;
    fm_event *event;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_MAC_MAINT,
                 "sw = %d, eventHandler = %s ",
                 sw,
                 (eventHandler ? eventHandler->name : "NULL"));

    if( eventHandler ) 
    {
        event = fmAllocateEvent(sw,
                                FM_EVID_HIGH_PURGE_SCAN_COMPLETE,
                                FM_EVENT_PURGE_SCAN_COMPLETE,
                                FM_EVENT_PRIORITY_LOW);
        if(event == NULL)
        {
            /* Only get here when semaphore timeout while blocking.
             * Only expected to be a rare occurance, and the mac
             * maintenance code will recover, so no additional logic
             * is required here.
             */
            fmDbgDiagCountIncr(sw, FM_CTR_MAC_EVENT_ALLOC_ERR, 1);
        
            FM_LOG_EXIT_VOID(FM_LOG_CAT_EVENT_MAC_MAINT);
        }

        event->info.purgeScanComplete = TRUE;

        err = fmSendThreadEvent(eventHandler, event);

        if(err != FM_OK)
        {
            fmReleaseEvent(event);
        }
    }

    FM_LOG_EXIT_VOID(FM_LOG_CAT_EVENT_MAC_MAINT );
}




/*****************************************************************************
 * fmAddMacTableMaintenanceWork
 *
 * Description: Update the Mac Table Maintenance Handler's Work list.
 *
 * Arguments:   sw is the switch number.
 *
 *              workType is the update type to be added to the work list
 *
 *              data is a data filter. Depending on the workType,
 *              the port, vid1 and vid2 need to be configured.
 *
 * Note: The pending work structures currently allow for events that require
 *          either a port or a vlan.  There is no support for events that
 *          require both, although such support could be added if needed.
 *
 * Returns:     FM API status value.
 *
 *****************************************************************************/
fm_status fmAddMacTableMaintenanceWork(fm_int                 sw,
                                       fm_maWorkType          workType,
                                       fm_maWorkTypeData      data,
                                       fm_addrMaintHandler    handler,
                                       void *                 context)
{
    fm_switch *           switchPtr;
    fm_addrMaintWorkList *workList;
    fm_status             status = FM_OK;
    fm_uint32             workFound = 0;
    fm_int                cpi;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_MAC_MAINT,
                 "sw=%d, workType=%s (%d), port=%d, vid1=%d, vid2=%d,context=%p\n",
                 sw,
                 fmMATableWorkTypeToText(workType),
                 workType,
                 data.port,
                 data.vid1,
                 data.vid2,
                 context);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_MAC_MAINT, FM_ERR_INVALID_ARGUMENT);
    }

    /**************************************************
     * Always grab the lock, to ensure that the maintenance 
     * thread doesn't change switchPtr->pPendingWorkList 
     * until we're done with it.
     **************************************************/

    TAKE_MAC_MAINT_LOCK(sw);

    workList = switchPtr->pPendingWorkList;

    switch (workType)
    {
        case FM_UPD_UPDATE_OVERFLOW:
            workFound = FM_MAC_MAINT_HANDLE_OVERFLOW;
            break;

        case FM_UPD_FLUSH_PORT_ADDRESSES:
            if (data.port == -1)
            {
                status =
                    fmSetBitArrayBlock(&workList->portAddressFlushArray,
                                       1,
                                       switchPtr->numCardinalPorts - 1,
                                       TRUE);
            }
            else
            {
                cpi    = GET_PORT_INDEX(sw, data.port);
                status = fmSetBitArrayBit(&workList->portAddressFlushArray,
                                          cpi,
                                          TRUE);
            }
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_MAC_MAINT, status);
            workFound = FM_MAC_MAINT_FLUSH_PORT;
            break;

        case FM_UPD_ACL_UPDATE:
            cpi    = GET_PORT_INDEX(sw, data.port);
            status = fmSetBitArrayBit(&workList->portAclUpdateArray,
                                      cpi,
                                      TRUE);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_MAC_MAINT, status);
            workFound = FM_MAC_MAINT_UPDATE_ACL;
            break;

        case FM_UPD_FLUSH_DYN_ADDRESSES:
            workFound = FM_MAC_MAINT_FLUSH_DYN_ADDR;
            break;

        case FM_UPD_FLUSH_VLAN_ADDRESSES:
            status = fmSetBitArrayBit(&workList->vlanAddressFlushArray,
                                      data.vid1,
                                      TRUE);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_MAC_MAINT, status);
            workFound = FM_MAC_MAINT_FLUSH_VLAN;
            break;

        case FM_UPD_FLUSH_PORT_VLAN_ADDRESSES:
            cpi    = GET_PORT_INDEX(sw, data.port);
            status =
                fmSetBitArrayBit(&workList->vlanPortAddressFlushArray,
                                 (cpi * switchPtr->vlanTableSize) + data.vid1,
                                 TRUE);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_MAC_MAINT, status);
            workFound = FM_MAC_MAINT_FLUSH_VLAN_PORT;
            break;

        case FM_UPD_SERVICE_MAC_FIFO:
            workFound = FM_MAC_MAINT_SERVICE_FIFO;
            break;

        case FM_UPD_HANDLE_PURGE:
            workFound = FM_MAC_MAINT_HANDLE_PURGE;
            break;

        case FM_UPD_PURGE_COMPLETE:
            workFound = FM_MAC_MAINT_PURGE_COMPLETE;
            break;

        case FM_UPD_SYNC_CACHE:
            workFound = FM_MAC_MAINT_SYNC_CACHE;
            break;

        case FM_UPD_REFRESH_REMOTE:
            workFound = FM_MAC_MAINT_REFRESH_REMOTE;
            break;

        default:
            status = FM_ERR_INVALID_ARGUMENT;
            break;

    }   /* end switch (workType) */

    if (workFound)
    {
        if (handler != NULL && workList->handler != NULL)
        {
            FM_LOG_ERROR(FM_LOG_CAT_EVENT_MAC_MAINT,
                         "handler already defined: "
                         "sw=%d, workType=%s (%d), port=%d, vid1=%d, vid2=%d\n",
                         sw,
                         fmMATableWorkTypeToText(workType),
                         workType,
                         data.port,
                         data.vid1,
                         data.vid2);
            status = FM_FAIL;
            goto ABORT;
        }

        workList->maintFlags |= workFound;
        
        workList->handler = handler;
        workList->context = context;

        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_MAC_MAINT,
                     "signaling MAC address maintenance task\n");

        status = fmSignalSemaphore(&fmRootApi->macTableMaintSemaphore);
        
    }   /* end if (workFound) */

ABORT:
    DROP_MAC_MAINT_LOCK(sw);
    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_MAC_MAINT, status);

}   /* end fmAddMacTableMaintenanceWork */




/*****************************************************************************
 * fmAllocateMacTableMaintenanceDataStructures
 *
 * Description: Performs switch initialization for the MAC Table maintenance
 *              thread.
 *
 * Arguments:   switchPtr points to the switch state structure being allocated.
 *
 * Returns:     FM_OK if successful.
 *              FM_ERR_INVALID_ARGUMENT if switchPtr is NULL.
 *              FM_ERR_NO_MEM if no memory for data structures.
 *
 *****************************************************************************/
fm_status fmAllocateMacTableMaintenanceDataStructures(fm_switch* switchPtr)
{
    fm_status err;

    if (switchPtr == NULL)
    {
        FM_LOG_ERROR(FM_LOG_CAT_EVENT_MAC_MAINT, "switchPtr = NULL\n");
        return FM_ERR_INVALID_ARGUMENT;
    }

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_MAC_MAINT,
                 "switchNumber = %d\n",
                 switchPtr->switchNumber);

    err = AllocWorkList(switchPtr, &switchPtr->workList1);

    if (err == FM_OK)
    {
        err = AllocWorkList(switchPtr, &switchPtr->workList2);
    }
    
    if (err != FM_OK)
    {
        FreeWorkList(&switchPtr->workList1);
        goto ABORT;
    }
    
    /**************************************************
     * Initialize purge list.
     **************************************************/
     
     err = fmAllocateMacTablePurgeList(switchPtr);
     
     if (err != FM_OK)
     {
        FreeWorkList(&switchPtr->workList1);
        FreeWorkList(&switchPtr->workList2);
     }
     
ABORT:    
    FM_LOG_EXIT(FM_LOG_CAT_EVENT_MAC_MAINT, err);

}   /* end fmAllocateMacTableMaintenanceDataStructures */




/*****************************************************************************
 * fmFreeMacTableMaintenanceDataStructures
 *
 * Description: Performs switch deallocation for the MAC Table maintenance
 *              thread.
 *
 * Arguments:   switchPtr points to the switch state structure being deallocated.
 *
 * Returns:     FM_OK if successful.
 *              FM_ERR_INVALID_ARGUMENT if switchPtr is NULL.
 *
 *****************************************************************************/
fm_status fmFreeMacTableMaintenanceDataStructures(fm_switch* switchPtr)
{
    fm_status err;
    fm_status status;
    
    if (switchPtr == NULL)
    {
        FM_LOG_ERROR(FM_LOG_CAT_EVENT_MAC_MAINT, "switchPtr = NULL\n");
        return FM_ERR_INVALID_ARGUMENT;
    }

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_MAC_MAINT,
                 "sw = %d\n",
                 switchPtr->switchNumber);

    /**************************************************
     * Deallocate the purge administration stuff.
     **************************************************/
    
    err = fmFreeMacTablePurgeList(switchPtr);
    status = err;
    
    if (err != FM_OK)
    {
        FM_LOG_ERROR(FM_LOG_CAT_EVENT_MAC_MAINT,
                     "Unable to free MAC table purge list!\n");
    }

    err = FreeWorkList(&switchPtr->workList1);
    
    if (err != FM_OK)
    {
        status = err;
        FM_LOG_ERROR(FM_LOG_CAT_EVENT_MAC_MAINT,
                     "Unable to free MAC maintenance worklist1!\n");
    }

    err = FreeWorkList(&switchPtr->workList2);

    if (err != FM_OK)
    {
        status = err;
        FM_LOG_ERROR(FM_LOG_CAT_EVENT_MAC_MAINT,
                     "Unable to free MAC maintenance worklist2!\n");
    }

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_MAC_MAINT, status);

}   /* end fmFreeMacTableMaintenanceDataStructures */




/*****************************************************************************
 * fmInitMacTableMaintenance
 *
 * Description: Performs switch initialization for the MAC Table maintenance
 *              thread.
 *
 * Arguments:   switchPtr points to the switch state structure being initialized.
 *
 * Returns:     FM_OK if successful.
 *              FM_ERR_INVALID_ARGUMENT if switchPtr is NULL.
 *
 *****************************************************************************/
fm_status fmInitMacTableMaintenance(fm_switch* switchPtr)
{
    fm_status err;

    if (switchPtr == NULL)
    {
        FM_LOG_ERROR(FM_LOG_CAT_EVENT_MAC_MAINT, "switchPtr = NULL\n");
        return FM_ERR_INVALID_ARGUMENT;
    }

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_MAC_MAINT,
                 "sw=%d\n",
                 switchPtr->switchNumber);

    /* Determine whether mac table polling is needed */
    if (switchPtr->switchFamily == FM_SWITCH_FAMILY_FM2000)
    {
        switchPtr->pollMacTable = TRUE;
    }
    else
    {
        switch (switchPtr->switchVersion)
        {
            case FM_SWITCH_VERSION_FM4224_A1:
            case FM_SWITCH_VERSION_FM4224_A1_5:
            case FM_SWITCH_VERSION_FM4224_A1_6:
                switchPtr->pollMacTable = TRUE;
                break;

            default:
                switchPtr->pollMacTable = FALSE;
                break;

        }   /* end switch (switchPtr->switchVersion) */

    }

    err = TAKE_MAC_MAINT_LOCK(switchPtr->switchNumber);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_MAC_MAINT, err);

    err = ResetWorkList(&switchPtr->workList1);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_MAC_MAINT, err);

    err = ResetWorkList(&switchPtr->workList2);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_MAC_MAINT, err);

    switchPtr->pPendingWorkList = &switchPtr->workList1;

    DROP_MAC_MAINT_LOCK(switchPtr->switchNumber);

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_EVENT_MAC_MAINT, err);

}   /* end fmInitMacTableMaintenance */




/*****************************************************************************
 * fmUpdateMATable
 * \ingroup intMacMaint
 *
 * \desc            Initiates a scan of the MAC address table, updating entries
 *                  according to the function arguments.
 *
 *                  An event is generated and sent to the the MAC address table
 *                  maintenance task.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       workType is the type of MAC address table scan to perform,
 *                  see fm_maWorkType for possible values. A few examples:
 *                      FM_UPD_FLUSH_PORT_ADDRESSES
 *                      FM_UPD_ACL_UPDATE
 *                      FM_UPD_FLUSH_DYN_ADDRESSES
 *                      FM_UPD_FLUSH_VLAN_ADDRESSES
 *
 * \param[in]       data is a data filter. Depending on the workType,
 *                  the port, vid1 and vid2 need to be configured.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmUpdateMATable(fm_int                sw,
                          fm_maWorkType         workType,
                          fm_maWorkTypeData     data,
                          fm_addrMaintHandler   handler,
                          void *                context)
{
    fm_switch *switchPtr;
    fm_status  status;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_MAC_MAINT,
                 "sw=%d, workType=%s (%d), port=%d, "
                 "vid1=%d, vid2=%d, context=%p\n",
                 sw,
                 fmMATableWorkTypeToText(workType),
                 workType,
                 data.port,
                 data.vid1,
                 data.vid2,
                 context);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->UpdateMATable != NULL)
    {
        /**************************************************
         * Switch provides hardware support for MAC Table
         * update events.
         * Call the switch-specific event handler.
         **************************************************/

        FM_API_CALL_FAMILY(status,
                           switchPtr->UpdateMATable,
                           sw,
                           workType,
                           data,
                           handler,
                           context);
    }
    else
    {
        /**************************************************
         * Switch does not provide hardware support for
         * MAC Table update events.
         * Add this task to the pending work list
         **************************************************/
        status = fmAddMacTableMaintenanceWork(sw,
                                              workType,
                                              data,
                                              handler,
                                              context);
    }

    UNPROTECT_SWITCH(sw);
    FM_LOG_EXIT(FM_LOG_CAT_EVENT_MAC_MAINT, status);

}   /* end fmUpdateMATable */




/*****************************************************************************/
/** fmMATableWorkTypeToText
 * \ingroup intMacMaint
 *
 * \desc            Returns a textual representation of an MA Table work type.
 *
 * \param[in]       workType is the MA Table work type.
 *
 * \return          Pointer to a string representation of workType.
 *
 *****************************************************************************/
char* fmMATableWorkTypeToText(fm_int workType)
{

    switch (workType)
    {
        case FM_UPD_UPDATE_OVERFLOW:
            return "OVERFLOW";

        case FM_UPD_BINFULL:
            return "BINFULL";

        case FM_UPD_FLUSH_PORT_ADDRESSES:
            return "FLUSH_PORT";

        case FM_UPD_ACL_UPDATE:
            return "ACL_UPDATE";

        case FM_UPD_FLUSH_DYN_ADDRESSES:
            return "FLUSH_DYN_ADDR";

        case FM_UPD_FLUSH_VLAN_ADDRESSES:
            return "FLUSH_VLAN";

        case FM_UPD_FLUSH_PORT_VLAN_ADDRESSES:
            return "FLUSH_PORT_VLAN";

        case FM_UPD_FLUSH_PORT_VID1_VID2_ADDRESSES:
            return "FLUSH_PORT_VID1_VID2";

        case FM_UPD_FLUSH_VID1_VID2_ADDRESSES:
            return "FLUSH_VID1_VID2";
        
        case FM_UPD_FLUSH_PORT_VID2_ADDRESSES:
            return "FLUSH_PORT_VID2";

        case FM_UPD_FLUSH_VID2_ADDRESSES:
            return "FLUSH_VID2";

        case FM_UPD_FLUSH_PORT_VID1_REMOTEID_ADDRESSES:
            return "FLUSH_PORT_VID1_REMOTEID";

        case FM_UPD_FLUSH_VID1_REMOTEID_ADDRESSES:
            return "FLUSH_VID1_REMOTEID";

        case FM_UPD_FLUSH_REMOTEID_ADDRESSES:
            return "FLUSH_REMOTEID";

        case FM_UPD_SERVICE_MAC_FIFO:
            return "SERVICE_FIFO";

        case FM_UPD_SYNC_CACHE:
            return "SYNC_CACHE";

        case FM_UPD_HANDLE_PURGE:
            return "HANDLE_PURGE";

        case FM_UPD_REFRESH_REMOTE:
            return "REFRESH_REMOTE";

        default:
            return "unknown";

    }   /* end switch (workType) */

}   /* end fmMATableWorkTypeToText */




/*****************************************************************************/
/** fmMATableReasonToText
 * \ingroup intMacMaint
 *
 * \desc            Returns a textual representation of the 'reason' field
 *                  in the ''fm_eventTableUpdate'' structure.
 *
 * \param[in]       reason is the reason for the event (see ''fm_macReason'').
 *
 * \return          Pointer to a string representation of reason.
 *
 *****************************************************************************/
const char* fmMATableReasonToText(fm_int reason)
{

    switch (reason)
    {
        case FM_MAC_REASON_NONE:
            return "NONE";

        case FM_MAC_REASON_PORT_DOWN:
            return "PORT_DOWN";

        case FM_MAC_REASON_LINK_DOWN:
            return "LINK_DOWN";

        case FM_MAC_REASON_STP_DOWN:
            return "STP_DOWN";

        case FM_MAC_REASON_ALL_PORTS_DOWN:
            return "ALL_PORTS_DOWN";

        case FM_MAC_REASON_SCAN_AGED:
            return "SCAN_AGED";

        case FM_MAC_REASON_SCAN_REPLACED:
            return "SCAN_REPLACED";

        case FM_MAC_REASON_SCAN_CHANGED:
            return "SCAN_CHANGED";

        case FM_MAC_REASON_SCAN_LEARNED:
            return "SCAN_LEARNED";

        case FM_MAC_REASON_HARD_AGING:
            return "HARD_AGING";

        case FM_MAC_REASON_HARD_LEARNING:
            return "HARD_LEARNING";

        case FM_MAC_REASON_SOFT_AGING:
            return "SOFT_AGING";

        case FM_MAC_REASON_SOFT_LEARNING:
            return "SOFT_LEARNING";

        case FM_MAC_REASON_TCAM_MIGRATED:
            return "TCAM_MIGRATED";

        case FM_MAC_REASON_TCAM_PURGED:
            return "TCAM_PURGED";

        case FM_MAC_REASON_TCAM_AGED:
            return "TCAM_AGED";

        case FM_MAC_REASON_TCAM_CHANGED:
            return "TCAM_CHANGED";

        case FM_MAC_REASON_ACL_COUNT:
            return "ACL_COUNT";

        case FM_MAC_REASON_ACL_MONITOR:
            return "ACL_MONITOR";

        case FM_MAC_REASON_ACL_PERMIT:
            return "ACL_PERMIT";

        case FM_MAC_REASON_ACL_DENY:
            return "ACL_DENY";

        case FM_MAC_REASON_FLUSH_DYN_ADDR:
            return "FLUSH_DYN_ADDR";

        case FM_MAC_REASON_FLUSH_PORT:
            return "FLUSH_PORT";

        case FM_MAC_REASON_FLUSH_VLAN:
            return "FLUSH_VLAN";

        case FM_MAC_REASON_FLUSH_VLAN_PORT:
            return "FLUSH_VLAN_PORT";

        case FM_MAC_REASON_AGE_EVENT:
            return "AGE_EVENT";

        case FM_MAC_REASON_LEARN_EVENT:
            return "LEARN_EVENT";

        case FM_MAC_REASON_LEARN_CHANGED:
            return "LEARN_CHANGED";

        case FM_MAC_REASON_LEARN_REPLACED:
            return "LEARN_REPLACED";

        case FM_MAC_REASON_API_AGED:
            return "API_AGED";

        case FM_MAC_REASON_API_LEARNED:
            return "API_LEARNED";

        case FM_MAC_REASON_API_LEARN_CHANGED:
            return "API_LEARN_CHANGED";

        case FM_MAC_REASON_API_LEARN_REPLACED:
            return "API_LEARN_REPLACED";

        case FM_MAC_REASON_VLAN_STATE:
            return "VLAN_STATE";

        case FM_MAC_REASON_MEM_ERROR:
            return "MEM_ERROR";

        default:
            return "UNKNOWN";

    }   /* end switch (reason) */

}   /* end fmMATableReasonToText */




/*****************************************************************************/
/** fmDbgDumpMacTableMaintStats
 * \ingroup diagMATable 
 *
 * \chips           FM2000, FM3000, FM4000
 *
 * \desc            Displays MA Table maintenance statistics.
 *
 * \param           None.
 *
 * \return          None.
 *
 *****************************************************************************/
void fmDbgDumpMacTableMaintStats(void)
{
    fm_int          sw;
    fm_uint64       avgTasks;
    fm_uint64       statValue;
    fm_uint64       pollCount;
    fm_uint64       totalTasks;
    macMaintStat*   stat;
    fm_status       err;

    FM_LOG_ENTRY_NOARGS(FM_LOG_CAT_EVENT_MAC_MAINT);

    for (sw = FM_FIRST_FOCALPOINT ; sw <= FM_LAST_FOCALPOINT ; sw++)
    {
        fmDbgDiagCountGet(sw, FM_CTR_MAC_WORK_POLL_COUNT, &pollCount);
        fmDbgDiagCountGet(sw, FM_CTR_MAC_WORK_TOTAL_TASKS, &totalTasks);

        if ( (sw == FM_FIRST_FOCALPOINT) || (pollCount > 0) )
        {
            avgTasks = (pollCount) ? (totalTasks / pollCount) : 0;

            FM_LOG_PRINT("Switch %d     Poll Count %" FM_FORMAT_64
                         "u    Avg Tasks %" FM_FORMAT_64 "u    Max Tasks %d\n",
                         sw,
                         pollCount,
                         avgTasks,
                         fmRootApi->macTableMaintMaxTasks[sw]);

            for (stat = &MacMaintStatTable[0] ; stat->name != NULL ; ++stat)
            {
                err = fmDbgDiagCountGet(sw, stat->index, &statValue);
                if (err != FM_OK)
                {
                    FM_LOG_ERROR(FM_LOG_CAT_EVENT_MAC_MAINT,
                                 "%s\n",
                                 fmErrorMsg(err));
                    break;
                }

                FM_LOG_PRINT("    %-31s %" FM_FORMAT_64 "u\n",
                             stat->name,
                             statValue);
            }


        } /* end if ( (sw == FM_FIRST_FOCALPOINT) || (pollCount > 0) ) */

    } /* end for (sw = FM_FIRST_FOCALPOINT ; sw <= FM_LAST_FOCALPOINT ; sw++) */

    FM_LOG_EXIT_VOID(FM_LOG_CAT_EVENT_MAC_MAINT);

}   /* end fmDbgDumpMacTableMaintStats */



/*****************************************************************************/
/** fmCommonUpdateMATable
 * \ingroup intMacMaint
 *
 * \desc            Handle software-generated MAC Table update events
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       workType is the type of update to perform.
 *
 * \param[in]       data contains a parameter whose meaning depends upon
 *                  the value of type.
 *
 * \param[in]       handler is a pointer to the maintenance handler.
 *
 * \param[in]       context is the user-specified argument to be passed to
 *                  the maintenance handler.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if workType is not recognized.
 *
 *****************************************************************************/
fm_status fmCommonUpdateMATable(fm_int              sw,
                                fm_maWorkType       workType,
                                fm_maWorkTypeData   data,
                                fm_addrMaintHandler handler,
                                void *              context)
{
    fm_status status   = FM_OK;
    fm_bool   purge    = FALSE;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_MAC_MAINT,
                 "sw=%d, workType=%d, port=%d, vid1=%d, vid2=%d, context=%p\n",
                 sw,
                 workType,
                 data.port,
                 data.vid1,
                 data.vid2,
                 context);

    switch (workType)
    {
        /***************************************************
         * These events are NOT handled by hardware.
         **************************************************/
        case FM_UPD_UPDATE_OVERFLOW:
        case FM_UPD_BINFULL:
        case FM_UPD_SYNC_CACHE:
        case FM_UPD_HANDLE_PURGE:
            status = fmAddMacTableMaintenanceWork(sw,
                                                  workType,
                                                  data,
                                                  handler,
                                                  context);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_MAC_MAINT, status);
            break;

        /***************************************************
         * These events are handled by hardware.
         **************************************************/
        case FM_UPD_FLUSH_PORT_ADDRESSES:
        case FM_UPD_FLUSH_DYN_ADDRESSES:
        case FM_UPD_FLUSH_VLAN_ADDRESSES:
        case FM_UPD_FLUSH_PORT_VLAN_ADDRESSES:
        case FM_UPD_FLUSH_PORT_VID1_VID2_ADDRESSES:
        case FM_UPD_FLUSH_PORT_VID2_ADDRESSES:
        case FM_UPD_FLUSH_VID1_VID2_ADDRESSES:
        case FM_UPD_FLUSH_VID2_ADDRESSES:
        case FM_UPD_FLUSH_PORT_VID1_REMOTEID_ADDRESSES:
        case FM_UPD_FLUSH_VID1_REMOTEID_ADDRESSES:
        case FM_UPD_FLUSH_REMOTEID_ADDRESSES:
            purge = TRUE;
            break;

        default:
            status = FM_ERR_INVALID_ARGUMENT;
            break;

    }   /* end switch (workType) */

    if (purge)
    {
        status = fmEnqueueMAPurge(sw,
                                  workType,
                                  data,
                                  handler,
                                  context);

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_MAC_MAINT, status);
    }

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_EVENT_MAC_MAINT, status);

}   /* end fmCommonUpdateMATable */

