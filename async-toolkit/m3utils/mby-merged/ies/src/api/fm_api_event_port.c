/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_event_port.c
 * Creation Date:   May 18, 2007
 * Description:     Generic thread wrapper for chip specific debounce handler
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

/*****************************************************************************/
/** fmDebounceLinkStateTask
 * \ingroup intEvent
 *
 * \desc            Function to debounce link states, and pass link state events
 *                  up to the API event queue.
 *
 * \param[in]       args is the thread argument pointer.
 *
 * \return          None.
 *
 *****************************************************************************/
void *fmDebounceLinkStateTask(void *args)
{
    fm_thread *thread;
    fm_thread *eventHandler;
    fm_switch *switchPtr;
    fm_int     sw;
#if FM_SUPPORT_SWAG
    fm_int     swagId;
#endif

    /* grab arguments */
    thread       = FM_GET_THREAD_HANDLE(args);
    eventHandler = FM_GET_THREAD_PARAM(fm_thread, args);

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT,
                 "thread = %s, eventHandler = %s\n",
                 thread->name,
                 eventHandler->name);

    while (1)
    {
        /* process each switch and port.. */
        for (sw = FM_FIRST_FOCALPOINT ; sw <= FM_LAST_FOCALPOINT ; sw++)
        {
            if ( !SWITCH_LOCK_EXISTS(sw) )
            {
                continue;
            }

#if FM_SUPPORT_SWAG
            if (fmIsSwitchInASWAG(sw, &swagId) == FM_OK)
            {
                PROTECT_SWITCH(swagId);
                if ( (fmRootApi->fmSwitchStateTable[swagId] == NULL)
                    || (fmRootApi->fmSwitchStateTable[swagId]->state != FM_SWITCH_STATE_UP) )
                {
                    UNPROTECT_SWITCH(swagId);
                    continue;
                }
                UNPROTECT_SWITCH(swagId);
            }
#endif

            PROTECT_SWITCH(sw);

            /* If the switch is not up yet, keep going */
            if ( (fmRootApi->fmSwitchStateTable[sw] == NULL)
                || !FM_IS_STATE_ALIVE(fmRootApi->fmSwitchStateTable[sw]->state) )
            {
                UNPROTECT_SWITCH(sw);
                continue;
            }

            switchPtr = fmRootApi->fmSwitchStateTable[sw];

            /* Call the switch specific handler */
            FM_API_CALL_FAMILY_VOID(switchPtr->DebounceLinkStates,
                                    sw, 
                                    thread, 
                                    eventHandler);

            UNPROTECT_SWITCH(sw);
        }

        /* sleep for specified delay period (default is 1/4 second) */
        fmDelay(0, FM_API_LINK_STATE_DEBOUNCE_DELAY);
    }

    fmExitThread(thread);

    return NULL;

}   /* end fmDebounceLinkStateTask */
