/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fmSWAG_fibm.c
 * Creation Date:   June 1, 2010
 * Description:     SWAG FIBM implementation
 *
 * INTEL CONFIDENTIAL
 * Copyright 2006 - 2011 Intel Corporation. All Rights Reserved. 
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


#include <fm_sdk_fm4000_int.h>


/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/




/*****************************************************************************
 * Public Functions
 *****************************************************************************/




/*****************************************************************************/
/** fmSWAGFibmAddSlaveSwitch
 * \ingroup intSwitch
 *
 * \desc            Function to simulate a slave/remote switch insertion. This can
 *                  be called on the local switch after the remote switch
 *                  is brought up.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmSWAGFibmAddSlaveSwitch(fm_int sw)
{
#if FM_SUPPORT_SWAG
    fm_status               err = FM_OK;
    fm_event *              insertEvent;
    fm_eventSwitchInserted *insert;
    fm_platform_state *     ps;
    fm_int     swagId;
    fm_int i;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "fmSWAGFibmAddSlaveSwitch: sw %d\n", sw);

    if ( (sw > FM_MAX_NUM_FOCALPOINTS) || (sw < 0) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_INVALID_ARGUMENT);
    }

    if (!fmRootPlatform->fmPlatformState)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_UNINITIALIZED);
    }

    for (i = FM_FIRST_FOCALPOINT; i <= FM_LAST_FOCALPOINT; i++)
    {
        ps = &fmRootPlatform->fmPlatformState[i];
        /* assign switch number */
        ps->sw         = i;
        ps->family     = FM_SWITCH_FAMILY_REMOTE_FM4000;
        ps->intrSource = FM_INTERRUPT_SOURCE_NONE;

        /***************************************************
         * Allocate and generate the switch inserted event
         * for this switch.
         **************************************************/

        /* This is priority high because we don't want to be throttled */
        insertEvent = fmAllocateEvent(i,
                                      FM_EVID_SYSTEM,
                                      FM_EVENT_SWITCH_INSERTED,
                                      FM_EVENT_PRIORITY_HIGH);

        if (!insertEvent)
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "Unable to allocate event for switch insertion\n");

            /**************************************************
             * We have to return here.  If we keep going, we'll
             * dereference a NULL pointer at "insert->model = -1".
             **************************************************/
            FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_FAIL);
        }

        insert = &insertEvent->info.fpSwitchInsertedEvent;

        insert->model = -1;
        insert->slot  = i;
        err = fmSendThreadEvent(&fmRootApi->eventThread, insertEvent);
    }

    sleep(2);

    /* Create a switch aggregate for the unit */
    err = fmCreateSWAG(FM_SWAG_TOPOLOGY_FAT_TREE,
                       NULL,
                       &swagId);
    if (err != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
    }

    ps = &fmRootPlatform->fmPlatformState[sw];
    /* assign switch number */
    ps->sw         = sw;
    ps->family     = FM_SWITCH_FAMILY_SWAG;
    ps->intrSource = FM_INTERRUPT_SOURCE_NONE;

    /***************************************************
     * Allocate and generate the switch inserted event
     * for this swag.
     **************************************************/

    /* This is priority high because we don't want to be throttled */
    insertEvent = fmAllocateEvent(swagId,
                                  FM_EVID_SYSTEM,
                                  FM_EVENT_SWITCH_INSERTED,
                                  FM_EVENT_PRIORITY_HIGH);

    if (!insertEvent)
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Unable to allocate event for switch insertion\n");

        /**************************************************
         * We have to return here.  If we keep going, we'll
         * dereference a NULL pointer at "insert->model = -1".
         **************************************************/
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_FAIL);
    }

    insert = &insertEvent->info.fpSwitchInsertedEvent;

    insert->model = -1;
    insert->slot  = swagId;
    err = fmSendThreadEvent(&fmRootApi->eventThread, insertEvent);

    fmRootPlatform->swagId = swagId;

    sleep(2);

    FM_LOG_EXIT(FM_LOG_CAT_FIBM, err);
#else
    FM_NOT_USED(sw);
    FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_FAIL);
#endif

} /* end fmSWAGFibmAddSlaveSwitch */




/*****************************************************************************/
/** fmSWAGFibmRemoveSlaveSwitch
 * \ingroup intSwitch
 *
 * \desc            Function to simulate a slave/remote switch removal.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmSWAGFibmRemoveSlaveSwitch(fm_int sw)
{
#if FM_SUPPORT_SWAG
    fm_status               err = FM_OK;
    fm_event *              removeEvent;
    fmSWAG_switch *         saSwitchPtr;
    fm_swagMember *         curMember;


    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "fmSWAGFibmRemoveSlaveSwitch: sw %d\n", sw);

    if ( (sw > FM_MAX_NUM_FOCALPOINTS) || (sw < 0) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_INVALID_ARGUMENT);
    }

    saSwitchPtr = GET_SWITCH_EXT(sw);

    curMember = fmGetFirstSwitchInSWAG(saSwitchPtr);

    err = FM_OK;
    while ( (err == FM_OK) && (curMember != NULL) )
    {
        /***************************************************
         * Allocate and generate the switch remove event
         * for this switch.
         **************************************************/
    
        /* This is priority high because we don't want to be throttled */
        removeEvent = fmAllocateEvent(curMember->swId,
                                      FM_EVID_SYSTEM,
                                      FM_EVENT_SWITCH_REMOVED,
                                      FM_EVENT_PRIORITY_HIGH);
    
        if (!removeEvent)
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "Unable to allocate event for switch insertion\n");
    
            /**************************************************
             * We have to return here.  If we keep going, we'll
             * dereference a NULL pointer at "insert->model = -1".
             **************************************************/
            FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_FAIL);
        }
    
        removeEvent->info.fpSwitchRemovedEvent.slot = curMember->swId;

        err = fmSendThreadEvent(&fmRootApi->eventThread, removeEvent);

        curMember = fmGetNextSwitchInSWAG(curMember);
    }

    sleep(2);

    err = fmDeleteSWAG(sw);

    FM_LOG_EXIT(FM_LOG_CAT_FIBM, err);
#else
    FM_NOT_USED(sw);
    FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_FAIL);
#endif

} /* end fmSWAGFibmRemoveSlaveSwitch */

