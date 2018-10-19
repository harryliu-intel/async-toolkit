/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_event_fast_maint.c
 * Creation Date:   April 2, 2008
 * Description:     Generic thread wrapper for chip specific fast maintenance
 *                  thread.
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
/** fmFastMaintenanceTask
 * \ingroup intEvent
 *
 * \desc            Generic thread wrapper for chip specific fast maintenance
 *                  thread.
 *
 * \param[in]       args is the thread argument pointer.
 *
 * \return          None.
 *
 *****************************************************************************/
void *fmFastMaintenanceTask(void *args)
{
    fm_switch *  switchPtr;
    fm_thread *  eventHandler;
    fm_thread *  thread;
    fm_int       sw;
    fm_bool      doFastTask = FALSE;
    fm_int       delayTime;
    fm_timestamp curTime;
    fm_timestamp nextRefreshTime;
    fm_int       msecCount = 1000;  /* Trigger remote refresh on first run */
    fm_bool      checkRemoteRefresh;
    fm_int       refreshInterval;

    /* grab arguments */
    thread       = FM_GET_THREAD_HANDLE(args);
    eventHandler = FM_GET_THREAD_PARAM(fm_thread, args);

    /* If logging is disabled, thread and eventHandler won't be used */
    FM_NOT_USED(thread);
    FM_NOT_USED(eventHandler);

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_FAST_MAINT,
                 "thread=%s eventHandler=%s\n",
                 thread->name,
                 eventHandler->name);

    /* First check to see if we need to call the fast maintenance task */
    do 
    {
        for(sw = FM_FIRST_FOCALPOINT ; sw <= FM_LAST_FOCALPOINT ; sw++)
        {
            if (!SWITCH_LOCK_EXISTS(sw))
            {
                continue;
            }

            PROTECT_SWITCH(sw);

            switchPtr = fmRootApi->fmSwitchStateTable[sw];
            
            /***************************************************
             * Iff the switch has been fully booted AND a switch
             * specific fast maintenance task has been defined,
             * perform a switch specific fast maintenance
             * function call.
             **************************************************/
            if (switchPtr
                && (switchPtr->state == FM_SWITCH_STATE_UP)
                && switchPtr->FastMaintenanceTask)
            {
                doFastTask = TRUE;
            }

            UNPROTECT_SWITCH(sw);
        }
        
        fmDelay(5, 0);

    } 
    while (!doFastTask);


    delayTime = fmGetIntApiAttribute(FM_AAK_API_FAST_MAINTENANCE_PERIOD,
                                        FM_AAD_API_FAST_MAINTENANCE_PERIOD);

    do 
    {
        /* Check for remote refresh timeouts once each quarter-second */
        checkRemoteRefresh = FALSE;
        msecCount += delayTime;

        if (msecCount >= 250)   /* 250 milliseconds per half-second */
        {
            msecCount = 0;

            if ( fmGetTime(&curTime) == FM_OK)
            {
                checkRemoteRefresh = TRUE;
            }
        }

        for(sw = FM_FIRST_FOCALPOINT ; sw <= FM_LAST_FOCALPOINT ; sw++)
        {
            if (!SWITCH_LOCK_EXISTS(sw))
            {
                continue;
            }

            PROTECT_SWITCH(sw);

            switchPtr = GET_SWITCH_PTR(sw);

            /* Only check for remote refresh intervals if hardware aging
             * is active */
            if (switchPtr
                && (switchPtr->state == FM_SWITCH_STATE_UP)
                && checkRemoteRefresh
                && switchPtr->macAgingTicks)
            {
                /* refresh interval is 1/3 the hardware aging time but
                 * don't refresh more often than twice per second */
                refreshInterval = switchPtr->macAgingTicks / 3;
                nextRefreshTime.sec  = refreshInterval / FM_TICKS_PER_SECOND;
                nextRefreshTime.usec = (refreshInterval % FM_TICKS_PER_SECOND)
                                        * 1000; /* 1000 usecs per msec */
                if ( (nextRefreshTime.sec == 0) &&
                    (nextRefreshTime.usec < 500000) )
                {
                    nextRefreshTime.usec = 500000; /* 500k usecs = 1/2 second */
                }

                fmAddTimestamps(&nextRefreshTime,
                                &switchPtr->macTableLastRemoteRefresh);

                if ( fmCompareTimestamps(&curTime, &nextRefreshTime) >= 0 )
                {
                    fm_maWorkTypeData data;

                    FM_CLEAR(data);
                    fmAddMacTableMaintenanceWork(sw,
                                                 FM_UPD_REFRESH_REMOTE,
                                                 data,
                                                 NULL,
                                                 NULL);
                    switchPtr->macTableLastRemoteRefresh = curTime;
                }
            }

            /***************************************************
             * Iff the switch has been fully booted AND a switch
             * specific fast maintenance task has been defined,
             * perform a switch specific fast maintenance
             * function call.
             **************************************************/
            if (switchPtr
                && (switchPtr->state == FM_SWITCH_STATE_UP)
                && switchPtr->FastMaintenanceTask)
            {
                switchPtr->FastMaintenanceTask(sw, args);
            }

            UNPROTECT_SWITCH(sw);
        }
        
        fmDelay(0, delayTime);

    } 
    while (TRUE);
  
    fmExitThread(thread);

    FM_LOG_EXIT_CUSTOM(FM_LOG_CAT_EVENT_FAST_MAINT, NULL, "\n");

}   /* end fmFastMaintenanceTask */
