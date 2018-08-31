/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_event_intr.c
 * Creation Date:   May 4, 2007
 * Description:     FocalPoint interrupt handler wrapper task
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


/*****************************************************************************
 * fmInterruptHandler
 *
 * Description: Generic Focalpoint interrupt handler task wrapper.
 *
 * Arguments:   data is a pointer to the device control block.
 *
 * Returns:     None.
 *
 * This task wakes up on a semaphore that may be signaled by the
 * interrupt level ISR or some other task.
 *
 * When the task wakes up, it will call the chip specific interrupt
 * handler.
 *
 * Then, if the interrupt was triggered by the ISR, the task will
 * re-enable the interrupt.
 *
 *****************************************************************************/
void *fmInterruptHandler(void *args)
{
    fm_int     sw;
    fm_switch *switchPtr;
    fm_status  err;
    fm_uint    intrSource;
    fm_int     handleFibmSlave;

    /* There is a duplicate interrupt handler thread if FIBM is enabled
     * Since the interrupt thread processing for remote switch will be
     * suspended while doing registers read/ write
     */
    /* Args set to NULL for remote thread */
    handleFibmSlave = FM_GET_THREAD_PARAM(void, args) ? 0 : 1;

    FM_NOT_USED(args);
    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_INTR, "%p\n", args);

    /**************************************************
     * Loop forever, waiting for signals from ISR.
     **************************************************/

    while (TRUE)
    {
        /**************************************************
         * Wait for a signal from the ISR.
         **************************************************/

        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_INTR, "Waiting for interrupt..\n");

        if (handleFibmSlave)
        {
            err = fmWaitSemaphore(&fmRootApi->fibmSlaveIntrAvail, FM_WAIT_FOREVER);
        }
        else
        {
            err = fmWaitSemaphore(&fmRootApi->intrAvail, FM_WAIT_FOREVER);
        }

        if (err != FM_OK)
        {
            FM_LOG_FATAL( FM_LOG_CAT_EVENT_INTR, "%s\n", fmErrorMsg(err) );

            continue;
        }

        for (sw = FM_FIRST_FOCALPOINT ; sw <= FM_LAST_FOCALPOINT ; sw++)
        {
            /* Only process interrupt for same type of switch */
            if (!handleFibmSlave != !fmRootApi->isSwitchFibmSlave[sw])
            {
                continue;
            }


            err = fmPlatformGetInterrupt(sw,
                                         FM_INTERRUPT_SOURCE_ISR,
                                         &intrSource);


            if (err != FM_OK)
            {
                FM_LOG_FATAL( FM_LOG_CAT_EVENT_INTR, "%s\n", fmErrorMsg(err) );

                continue;
            }

            if (intrSource == FM_INTERRUPT_SOURCE_NONE)
            {
                continue;
            }

            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_INTR, "Interrupt seen (source 0x%x)\n",
                         intrSource);


            if ( !SWITCH_LOCK_EXISTS(sw) )
            {
                goto REENABLE;
            }

            PROTECT_SWITCH(sw);

            /* If the switch is not up yet, keep going */
            if ( (fmRootApi->fmSwitchStateTable[sw] == NULL)
                || !FM_IS_STATE_ALIVE(fmRootApi->fmSwitchStateTable[sw]->state) )
            {
                UNPROTECT_SWITCH(sw);

                goto REENABLE;
            }

            switchPtr = fmRootApi->fmSwitchStateTable[sw];

            /* Call the chip specific handler */
            switchPtr->InterruptHandler(switchPtr);

            UNPROTECT_SWITCH(sw);

REENABLE:

            if (intrSource & FM_INTERRUPT_SOURCE_ISR)
            {
                /* Re-enable the interrupt */
                err = fmPlatformEnableInterrupt(sw, intrSource);

                if (err != FM_OK)
                {
                    FM_LOG_FATAL( FM_LOG_CAT_EVENT_INTR, "%s\n", fmErrorMsg(err) );

                    continue;
                }

            }   /* end if (intrSource & FM_INTERRUPT_SOURCE_ISR) */

        }       /* end for (sw = FM_FIRST_FOCALPOINT ; sw <= FM_LAST_FOCALPOINT ; sw++) */

    }           /* end while (TRUE) */

    /**************************************************
     * Should never exit.
     **************************************************/

    FM_LOG_FATAL(FM_LOG_CAT_EVENT_INTR, "Task exiting inadvertently!\n");

    return NULL;

}   /* end fmInterruptHandler */


/*****************************************************************************/
/** fmSendSoftwareEvent
 * \ingroup intSwitch
 *
 * \desc            Generate a software interrupt event. Generally called
 *                  in response to a software interrupt on FM4000 and FM6000
 *                  devices.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       events is a bit mask of software events as reported in
 *                  the chip's SW_IP register.
 *
 * \return          
 *
 *****************************************************************************/
fm_status fmSendSoftwareEvent(fm_int sw, fm_uint32 events)
{
    fm_status         err = FM_OK;
    fm_event *        event;
    fm_eventSoftware *swEvent;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_INTR, 
                 "sw=%d events=0x%08x\n",
                 sw, 
                 events);

    event = fmAllocateEvent(sw,
                            FM_EVID_LOW_SOFTWARE,
                            FM_EVENT_SOFTWARE,
                            FM_EVENT_PRIORITY_HIGH);

    if (event == NULL)
    {
        FM_LOG_FATAL(FM_LOG_CAT_EVENT_INTR, "Out of event buffers\n");
        err = FM_ERR_NO_EVENTS_AVAILABLE;
        goto ABORT;
    }

    swEvent = &event->info.fpSoftwareEvent;
    memset( swEvent, 0, sizeof(fm_eventSoftware) );

    swEvent->activeEvents = events;

    err = fmSendThreadEvent(&fmRootApi->eventThread, event);

    if (err != FM_OK)
    {
        /* Free the event since we could not send it to thread */
        fmReleaseEvent(event);
    }

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_EVENT_INTR, err);

}   /* end fmSendSoftwareEvent */




