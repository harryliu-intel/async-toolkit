/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_parity.c
 * Creation Date:   December 2009
 * Description:     Generic thread wrapper for chip specific Parity
 *                  sweeper thread.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2009 - 2012 Intel Corporation. All Rights Reserved. 
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

/* Define ENABLE_TIMER to measure the time it takes
   for the sweeper to go through the entire loop */
#undef ENABLE_TIMER

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
/** fmParitySweeperTask
 * \ingroup intApi
 *
 * \desc            Generic thread wrapper for chip specific parity
 *                  sweeper thread.
 *
 * \param[in]       args contains a pointer to the thread information.
 *
 * \return          Should never exit.
 *
 *****************************************************************************/
void *fmParitySweeperTask(void *args)
{
    fm_thread *  thread;
    fm_switch *  switchPtr;
    fm_thread *  eventHandler;
    fm_int       sw;
    fm_bool      doParityTask = FALSE;
    fm_bool      switchProtected = FALSE;

#ifdef ENABLE_TIMER
    fm_bool      startTimer = FALSE;
    fm_timestamp t1;
    fm_timestamp t2;
    fm_timestamp t3;
#endif

    thread       = FM_GET_THREAD_HANDLE(args);
    eventHandler = FM_GET_THREAD_PARAM(fm_thread, args);

    /* If logging is disabled, thread and eventHandler won't be used */
    FM_NOT_USED(thread);
    FM_NOT_USED(eventHandler);

    FM_LOG_ENTRY(FM_LOG_CAT_SWITCH,
                 "thread = %s, eventHandler = %s\n",
                 thread->name,
                 eventHandler->name);

    /* First check to see if we need to call the parity sweeper task */
    do 
    {
        for(sw = FM_FIRST_FOCALPOINT ; sw <= FM_LAST_FOCALPOINT ; sw++)
        {
            if (!SWITCH_LOCK_EXISTS(sw))
            {
                continue;
            }

            PROTECT_SWITCH(sw);

            switchPtr = GET_SWITCH_PTR(sw);

            if ( switchPtr &&
                (switchPtr->state == FM_SWITCH_STATE_UP) &&
                 switchPtr->paritySweeperCfg.enabled  &&
                 switchPtr->ParitySweeperTask )
            {
                doParityTask = TRUE;
            }

            UNPROTECT_SWITCH(sw);
        }
        
        fmDelay(5, 0);

    } 
    while (!doParityTask);

    /* Loop forever */

    while (TRUE)
    {
#ifdef ENABLE_TIMER
        fmGetTime(&t1);
#endif
        for (sw = FM_FIRST_FOCALPOINT ; sw <= FM_LAST_FOCALPOINT ; sw++)
        {
            if (!SWITCH_LOCK_EXISTS(sw))
            {
                continue;
            }

            PROTECT_SWITCH(sw);
            switchProtected = TRUE;

            switchPtr = GET_SWITCH_PTR(sw);

            if ( switchPtr &&
                (switchPtr->state == FM_SWITCH_STATE_UP) &&
                 switchPtr->paritySweeperCfg.enabled  &&
                 switchPtr->ParitySweeperTask )
            {
                switchPtr->ParitySweeperTask(sw, &switchProtected, args);
#ifdef ENABLE_TIMER
                startTimer = TRUE;
#endif
            }

            if ( switchProtected )
            {
                UNPROTECT_SWITCH(sw);
            }
        }

#ifdef ENABLE_TIMER
        if (startTimer)
        {
            /* Debugging purpose only */
            fmGetTime(&t2);
            fmSubTimestamps(&t2, &t1, &t3);
            FM_LOG_DEBUG(FM_LOG_CAT_SWITCH, " time: %lld,%lld sec.\n",
                         t3.sec,
                         t3.usec);
        }
#endif

        fmYield();
        
    } /* end while (TRUE) */


    /**************************************************
     * Should never exit.
     **************************************************/

    FM_LOG_ERROR(FM_LOG_CAT_SWITCH,
                 "ERROR: fmParitySweeperTask: exiting inadvertently!\n");

    fmExitThread(thread);
    return NULL;

}   /* end fmParitySweeperTask */




/*****************************************************************************/
/** fmSendParityErrorEvent
 * \ingroup intApi
 *
 * \desc            Send a Parity error event to the upper layer.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       parityEvent contains the parity event error information.
 *
 * \param[in]       eventHandler points to the event handler to which the
 *                  parity event should be sent.
 *
 * \return          Should never exit.
 *
 *****************************************************************************/
fm_status fmSendParityErrorEvent(fm_int sw, 
                                 fm_eventParityError parityEvent, 
                                 fm_thread *eventHandler)
{
    fm_status            err;
    fm_event *           eventPtr;
    fm_eventParityError *parityErrEvent;

    FM_LOG_ENTRY(FM_LOG_CAT_SWITCH, "sw=%d ParityEvent=%p\n",
                 sw, (void *) &parityEvent);

    eventPtr = fmAllocateEvent(sw,
                               FM_EVID_SYSTEM,
                               FM_EVENT_PARITY_ERROR,
                               FM_EVENT_PRIORITY_LOW);

    if (eventPtr == NULL)
    {
        FM_LOG_FATAL(FM_LOG_CAT_SWITCH, "Out of event buffers\n");
        FM_LOG_EXIT(FM_LOG_CAT_SWITCH, FM_ERR_NO_EVENTS_AVAILABLE);
    }

    /* Init event structure */
    parityErrEvent = &eventPtr->info.fpParityErrorEvent;

    FM_MEMCPY_S( parityErrEvent,
                 sizeof(*parityErrEvent),
                 &parityEvent,
                 sizeof(parityEvent) );

    err = fmSendThreadEvent(eventHandler, eventPtr);

    if (err != FM_OK)
    {
        /* Free the event since we could not send it to thread */
        fmReleaseEvent(eventPtr);
        FM_LOG_EXIT(FM_LOG_CAT_SWITCH, err);
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWITCH, FM_OK);

}   /* end fmSendParityErrorEvent */




/*****************************************************************************/
/** fmParityErrTypeToText
 * \ingroup intSwitch
 *
 * \desc            Returns the text representation of a parity error type.
 *
 * \param[in]       errType is the memory type (see ''fm_parityErrType'').
 *
 * \return          Pointer to a string representing the parity error type.
 *
 *****************************************************************************/
const char * fmParityErrTypeToText(fm_parityErrType errType)
{

    switch ( errType )
    {
        case FM_PARITY_ERRTYPE_NONE:
            return "NONE";

        case FM_PARITY_ERRTYPE_CACHE_MISMATCH:
            return "CACHE_MISMATCH";

        case FM_PARITY_ERRTYPE_SRAM_CORRECTED:
            return "SRAM_CORRECTED";

        case FM_PARITY_ERRTYPE_SRAM_UNCORRECTABLE:
            return "SRAM_UNCORRECTABLE";

        default:
            return "UNKNOWN";
    }

}   /* end fmParityErrTypeToText */




/*****************************************************************************/
/** fmParityMemAreaToText
 * \ingroup intSwitch
 *
 * \desc            Returns the text representation of a memory area.
 *
 * \param[in]       memArea is the memory type (see ''fm_parityMemArea'').
 *
 * \return          Pointer to a string representing the memory area.
 *
 *****************************************************************************/
const char * fmParityMemAreaToText(fm_parityMemArea memArea)
{

    switch (memArea)
    {
        case FM_PARITY_AREA_UNDEFINED:
            return "UNDEFINED";

        /**************************************************
         * FM4000 Memory Areas
         **************************************************/

        case FM_PARITY_AREA_MAC_TABLE:
            return "MAC_TABLE";
    
        case FM_PARITY_AREA_VLAN_TAG_TABLE:
            return "VLAN_TAG_TABLE";
    
        case FM_PARITY_AREA_IP_MULTICAST_TABLE:
            return "IP_MULTICAST_TABLE";
    
        case FM_PARITY_AREA_INGRESS_VID_TABLE:
            return "INGRESS_VID_TABLE";
    
        case FM_PARITY_AREA_EGRESS_VID_TABLE:
            return "EGRESS_VID_TABLE";
    
        case FM_PARITY_AREA_INGRESS_FID_TABLE:
            return "INGRESS_FID_TABLE";
    
        case FM_PARITY_AREA_EGRESS_FID_TABLE:
            return "EGRESS_FID_TABLE";
    
        case FM_PARITY_AREA_GLORT_RAM:
            return "GLORT_RAM";
    
        case FM_PARITY_AREA_GLORT_CAM:
            return "GLORT_CAM";
    
        case FM_PARITY_AREA_GLORT_DEST_TABLE:
            return "GLORT_DEST_TABLE";
    
        case FM_PARITY_AREA_FFU_TCAM:
            return "FFU_TCAM";
    
        case FM_PARITY_AREA_FFU_SRAM:
            return "FFU_SRAM";
    
        case FM_PARITY_AREA_FFU_MAP_VLAN:
            return "FFU_MAP_VLAN";
    
        case FM_PARITY_AREA_TCN_FIFO:
            return "TCN_FIFO";
    
        case FM_PARITY_AREA_ARP_TABLE:
            return "ARP_TABLE";
    
        case FM_PARITY_AREA_PERR_IP:
            return "PERR_IP";
    
        case FM_PARITY_AREA_PARITY_IP:
            return "PARITY_IP";

        /**************************************************
         * FM6000 Memory Areas
         **************************************************/
    
        case FM_PARITY_AREA_ARRAY:
            return "ARRAY";
    
        case FM_PARITY_AREA_CM:
            return "CM";
    
        case FM_PARITY_AREA_CMM:
            return "CMM";
    
        case FM_PARITY_AREA_EACL:
            return "EACL";
    
        case FM_PARITY_AREA_FFU:
            return "FFU";
    
        case FM_PARITY_AREA_GLORT:
            return "GLORT";
    
        case FM_PARITY_AREA_INTERNAL:
            return "INTERNAL";
    
        case FM_PARITY_AREA_L2AR:
            return "L2AR";
    
        case FM_PARITY_AREA_L2F:
            return "L2F";
    
        case FM_PARITY_AREA_L2L:
            return "L2L";
    
        case FM_PARITY_AREA_L2L_MAC:
            return "L2L_MAC";
    
        case FM_PARITY_AREA_L2L_SWEEPER:
            return "L2L_SWEEPER";
    
        case FM_PARITY_AREA_MAPPER:
            return "MAPPER";
    
        case FM_PARITY_AREA_MCAST_MID:
            return "MCAST_MID";
    
        case FM_PARITY_AREA_MCAST_POST:
            return "MCAST_POST";
    
        case FM_PARITY_AREA_MODIFY:
            return "MODIFY";
    
        case FM_PARITY_AREA_NEXTHOP:
            return "NEXTHOP";
    
        case FM_PARITY_AREA_PARSER:
            return "PARSER";
    
        case FM_PARITY_AREA_POLICER:
            return "POLICER";
    
        case FM_PARITY_AREA_STATS_BANK:
            return "STATS_BANK";

        default:
            return "UNKNOWN";

    }   /* end switch (memArea) */

}   /* end fmParityMemAreaToText */




/*****************************************************************************/
/** fmParitySeverityToText
 * \ingroup intSwitch
 *
 * \desc            Returns the text representation of a parity severity code.
 *
 * \param[in]       severity is the memory type (see ''fm_paritySeverity'').
 *
 * \return          Pointer to a string representing the severity of the
 *                  parity error.
 *
 *****************************************************************************/
const char * fmParitySeverityToText(fm_paritySeverity severity)
{

    switch ( severity )
    {
        case FM_PARITY_SEVERITY_UNDEFINED:
            return "UNDEFINED";

        case FM_PARITY_SEVERITY_USER_FIXABLE:
            return "USER_FIXABLE";

        case FM_PARITY_SEVERITY_TRANSIENT:
            return "TRANSIENT";

        case FM_PARITY_SEVERITY_CUMULATIVE:
            return "CUMULATIVE";

        case FM_PARITY_SEVERITY_FATAL:
            return "FATAL";

        case FM_PARITY_SEVERITY_CORRECTED:
            return "CORRECTED";

        default:
            return "UNKNOWN";
    }

}   /* end fmParitySeverityToText */




/*****************************************************************************/
/** fmParityStatusToText
 * \ingroup intSwitch
 *
 * \desc            Returns the text representation of a parity status code.
 *
 * \param[in]       status is the memory type (see ''fm_parityStatus'').
 *
 * \return          Pointer to a string representing the status of the
 *                  parity error.
 *
 *****************************************************************************/
const char * fmParityStatusToText(fm_parityStatus status)
{

    switch ( status )
    {
        case FM_PARITY_STATUS_NO_ERROR_DETECTED:
            return "NO_ERROR_DETECTED";

        case FM_PARITY_STATUS_ERROR_FIXED:
            return "ERROR_FIXED";

        case FM_PARITY_STATUS_FIX_FAILED:
            return "FIX_FAILED";

        case FM_PARITY_STATUS_NO_ACTION_REQUIRED:
            return "NO_ACTION_REQUIRED";

        case FM_PARITY_STATUS_FATAL_ERROR:
            return "FATAL_ERROR";

        case FM_PARITY_STATUS_ECC_CORRECTED:
            return "ECC_CORRECTED";

        case FM_PARITY_STATUS_NO_FIX_ATTEMPTED:
            return "NO_FIX_ATTEMPTED";

        case FM_PARITY_STATUS_UNDEFINED:
            return "UNDEFINED";

        default:
            return "UNKNOWN";
    }

}   /* end fmParityStatusToText */




/*****************************************************************************/
/** fmDbgDumpParityErrorEvent
 * \ingroup intSwitch
 *
 * \desc            Dumps a parity error event structure.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       parityEvent points to the parity event object.
 *
 * \return          None
 *
 *****************************************************************************/
void fmDbgDumpParityErrorEvent(fm_int                sw,
                               fm_eventParityError * parityEvent)
{
    fm_int  i;

    FM_LOG_PRINT("\nParity event on switch %d:\n", sw);

    FM_LOG_PRINT("  Error Type  : %s\n",
                 fmParityErrTypeToText(parityEvent->errType));

    FM_LOG_PRINT("  Severity    : %s\n",
                 fmParitySeverityToText(parityEvent->paritySeverity));

    FM_LOG_PRINT("  Status      : %s\n",
                 fmParityStatusToText(parityEvent->parityStatus));

    FM_LOG_PRINT("  Memory Area : %s\n",
                 fmParityMemAreaToText(parityEvent->memoryArea));

    switch ( parityEvent->errType )
    {
        case FM_PARITY_ERRTYPE_CACHE_MISMATCH:
            FM_LOG_PRINT("  Base Addr   : %08x\n", parityEvent->baseAddr);
            break;

        case FM_PARITY_ERRTYPE_SRAM_CORRECTED:
        case FM_PARITY_ERRTYPE_SRAM_UNCORRECTABLE:
            FM_LOG_PRINT("  SRAM Number : %d\n", parityEvent->sramNo);
            break;

        default:
            break;
    }

    if ( parityEvent->numIndices != 0 )
    {
        FM_LOG_PRINT("  Indices     :");
        for ( i = 0 ; i < (fm_int)parityEvent->numIndices ; i++ )
        {
            FM_LOG_PRINT(" %d", parityEvent->tableIndices[i]);
        }
        FM_LOG_PRINT("\n");
    }

    if ( parityEvent->numValidData != 0 )
    {
        FM_LOG_PRINT("  Bad Data    :");
        for ( i = (fm_int)parityEvent->numValidData - 1 ; i >= 0 ; i-- )
        {
            FM_LOG_PRINT(" %08x", parityEvent->badData[i]);
        }
        FM_LOG_PRINT("\n");

        FM_LOG_PRINT("  Good Data   :");
        for ( i = (fm_int)parityEvent->numValidData - 1 ; i >= 0 ; i-- )
        {
            FM_LOG_PRINT(" %08x", parityEvent->cachedData[i]);
        }
        FM_LOG_PRINT("\n");
    }

    if ( parityEvent->memoryArea == FM_PARITY_AREA_PERR_IP ||
         parityEvent->ipRegValue != 0)
    {
        FM_LOG_PRINT("  PERR_IP     : %08x\n", parityEvent->ipRegValue);
    }

    if ( parityEvent->parityRegValue != 0 )
    {
        FM_LOG_PRINT("  PARITY_IP   : %08x\n", parityEvent->parityRegValue);
    }

    if ( parityEvent->numErrors != 0 )
    {
        FM_LOG_PRINT("  Error Count : %d\n", parityEvent->numErrors);
    }

}   /* end fmDbgDumpParityErrorEvent */

