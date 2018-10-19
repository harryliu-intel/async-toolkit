/* vim:et:sw=4:syntax=c:ts=4:tw=79:
 */
/******************************************************************************
 * File:            sdk.i
 * Creation Date:   August 15, 2007
 * Description:     
 *
 * INTEL CONFIDENTIAL
 * Copyright 2007 - 2012 Intel Corporation. All Rights Reserved. 
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

/******************************************************************************
 *
 *                                CONVENTIONS
 *
 * (I)  Wrapped structures are to be named by appending the capital letter W to
 *      the structure that is being wrapped.
 * 
 *****************************************************************************/

%module "SDK";

%ignore fmVprintf_s;
%ignore fmVfprintf_s;
%ignore fmVsnprintf_s;

%{

#include "fm_sdk.h"
#include "fm_sdk_int.h"
#include "fm_support.h"

#include "fm_proto_lldp.h"
#include "fm_proto_dcbx.h"

#include "sdk_types.h"

%}

%include "sdk_library.i"
%include "sdk_callback.i"
%include "sdk_fragments.i"
%include "sdk_typemaps.i"
%include "sdk_perlcode.i"
%include <reference.i>
%apply int *REFERENCE { int * };

%include "sdk_packet_handling.i"
#ifdef SWIG_LCI_DIRECT_ACCESS
%include "sdk_lci.i"
#endif /* SWIG_LCI_DIRECT_ACCESS */

FM_SWIG_FRAGMENT_ASVAL(IV, fm_int);
FM_SWIG_FRAGMENT_FROMVAL(IV, fm_int);

FM_SWIG_CALLBACK(fmInitialize,
                 fm_eventHandler,
                 void,
                 fm_int, , event,
                 fm_int, , sw,
                 void, *, ptr);

FM_SWIG_CALLBACK(fmSetEventHandler,
                 fm_eventHandler,
                 void,
                 fm_int, , event,
                 fm_int, , sw,
                 void, *, ptr);

#ifndef USE_ITHREADS
/******************************************************************************
 *
 *                                WRAPPED FUNCTIONS
 *
 *****************************************************************************/

%{

SWIGINTERN fm_semaphore swigSemHandle;

#define TP_MAX_LOCAL_EVENTS 128
#define TP_MAX_LOCAL_EVENTS_PLATFORM 128

fm_event localEventQueue[TP_MAX_LOCAL_EVENTS];
fm_int   localEventQueuePos;
fm_lock  localEventQueueLock;

/* Share localEventQueueLock */
fm_event localEventPlatformQueue[TP_MAX_LOCAL_EVENTS_PLATFORM];
fm_int   localEventPlatformQueuePos;

fm_timestamp    startTimestamp;
fm_switchInfo   swInfo;


/** fmSWIGEventHandler
 *
 * \desc        handles events, such as packet reception
 *
 * \param[in]   event the event to be handled
 *
 * \param[in]   sw the switch that generated the event
 *
 * \param[in]   ptr points to a event structure containing information about
 *              the event to be handled
 *
 *****************************************************************************/
void fmSWIGEventHandler(fm_int event, fm_int sw, void *arg)
{
    fm_status       status;

    switch (event)
    {
        case FM_EVENT_SWITCH_INSERTED:
            status = fmSupportInitialize(sw);
            if (status == FM_OK)
            {
                fmReleaseSemaphore(&swigSemHandle);
            }
            break;
        case FM_EVENT_PKT_RECV:
        case FM_EVENT_SFLOW_PKT_RECV:
            tpProcessPacketReceiveEvent(sw, (fm_eventPktRecv *) arg);
            break;
        case FM_EVENT_PORT:
            fmCaptureLock(&localEventQueueLock, FM_WAIT_FOREVER);
            if (localEventQueuePos < TP_MAX_LOCAL_EVENTS)
            {
                localEventQueue[localEventQueuePos].sw = sw;
                localEventQueue[localEventQueuePos].type = event;
                fmGetTime(&localEventQueue[localEventQueuePos].postedTimestamp);
                memcpy(&localEventQueue[localEventQueuePos++].info.fpPortEvent, 
                       arg, sizeof(fm_eventPort));
            }
            fmReleaseLock(&localEventQueueLock);
            break;
        case FM_EVENT_TABLE_UPDATE:
            fmCaptureLock(&localEventQueueLock, FM_WAIT_FOREVER);
            if (localEventQueuePos < TP_MAX_LOCAL_EVENTS)
            {
                localEventQueue[localEventQueuePos].sw = sw;
                localEventQueue[localEventQueuePos].type = event;
                fmGetTime(&localEventQueue[localEventQueuePos].postedTimestamp);
                memcpy(&localEventQueue[localEventQueuePos++].info.fpUpdateEvent, 
                       arg, sizeof(fm_eventTableUpdateBurst));
            }
            fmReleaseLock(&localEventQueueLock);
            break;
        case FM_EVENT_SECURITY:
            fmCaptureLock(&localEventQueueLock, FM_WAIT_FOREVER);
            if (localEventQueuePos < TP_MAX_LOCAL_EVENTS)
            {
                localEventQueue[localEventQueuePos].sw = sw;
                localEventQueue[localEventQueuePos].type = event;
                fmGetTime(&localEventQueue[localEventQueuePos].postedTimestamp);
                memcpy(&localEventQueue[localEventQueuePos++].info.fpSecEvent, 
                       arg, sizeof(fm_eventSecurity));
            }
            fmReleaseLock(&localEventQueueLock);
            break;
        case FM_EVENT_ARP:
            fmCaptureLock(&localEventQueueLock, FM_WAIT_FOREVER);
            if (localEventQueuePos < TP_MAX_LOCAL_EVENTS)
            {
                localEventQueue[localEventQueuePos].sw = sw;
                localEventQueue[localEventQueuePos].type = event;
                fmGetTime(&localEventQueue[localEventQueuePos].postedTimestamp);
                memcpy(&localEventQueue[localEventQueuePos++].info.fpArpEvent, 
                       arg, sizeof(fm_eventArp));
            }
            fmReleaseLock(&localEventQueueLock);
            break;
        case FM_EVENT_PLATFORM:
            fmCaptureLock(&localEventQueueLock, FM_WAIT_FOREVER);
            if (localEventPlatformQueuePos < TP_MAX_LOCAL_EVENTS_PLATFORM)
            {
                localEventPlatformQueue[localEventPlatformQueuePos].sw = sw;
                localEventPlatformQueue[localEventPlatformQueuePos].type = event;
                fmGetTime(&localEventPlatformQueue[localEventPlatformQueuePos].postedTimestamp);
                memcpy(&localEventPlatformQueue[localEventPlatformQueuePos++].info.fpPlatformEvent, 
                       arg, sizeof(fm_eventPlatform));
            }
            fmReleaseLock(&localEventQueueLock);
            break;
    }
}

%}

%inline
%{

/** fmInitializeW
 *
 * \desc        initializes the Fulcrum Microsystems SDK 2.0 and upon
 *              initialization of the SDK initializes a set of switches
 *
 * \param[in]   switchCount the number of switches to be initialized initially
 *
 *****************************************************************************/
void fmInitializeW(fm_int switchCount) 
{
    fm_status       status;
    fm_timestamp    timeout = { 60ULL, 0ULL };
    fm_int          currentSwitch, nextSwitch;
    fm_int          i;
    fm_switchInfo   switchInfo;
    const fm_text   swigSemName = "swigEventSwitchInserted";

    fmGetTime(&startTimestamp);

    status = fmOSInitialize();
    if (status != FM_OK)
    {
        croak("%s", fmErrorMsg(status));
    }
    status = fmCreateSemaphore(swigSemName, FM_SEM_COUNTING, &swigSemHandle, 0);
    if (status != FM_OK)
    {
        croak("%s", fmErrorMsg(status));
    }
    status = fmCreateLock("localEventQueue", &localEventQueueLock);
    if (status != FM_OK)
    {
        croak("%s", fmErrorMsg(status));
    }
    localEventQueuePos = 0;
    memset(&localEventQueue, 0, sizeof(fm_event) * TP_MAX_LOCAL_EVENTS);

    localEventPlatformQueuePos = 0;
    memset(&localEventPlatformQueue, 0, sizeof(fm_event) * TP_MAX_LOCAL_EVENTS_PLATFORM);

    status = fmInitialize(fmSWIGEventHandler);
    if (status != FM_OK)
    {
        croak("%s", fmErrorMsg(status));
    }

    /*************************************************************************
     * BZ 1970: TestPoint does not set the EventMask and the default value
     *          for this mask is dependent on TestPoint being the first process
     *          to access the FocalPoint device or not. If it is not the
     *          EventMask is set to ~(FM_EVENT_PKT_RECV) preventing all
     *          FM_EVENT_PKT_RECV events from being duplicated to TestPoint.
     *************************************************************************/

    fmSetProcessEventMask(0xffffffff);

    for (i = 0; i < FM_MAX_NUM_SWITCHES; i++)
    {
        status = fmSupportBeforeInitialize(i);
        if (status != FM_OK)
        {
            croak("%s", fmErrorMsg(status));
        }
    }

    if (switchCount > 0)
    {
        fmGetSwitchFirst(&currentSwitch);
        do
        {
            FM_CLEAR(switchInfo);

            status = fmGetSwitchInfo(currentSwitch, &switchInfo);
            if (status != FM_OK)
            {
                /* Assume the switch is DOWN. Reinitialize the switch
                 * information data structure to enforce this assumption.
                 */
                FM_CLEAR(switchInfo);
            }

            /******************************************************************
             * BZ 1970: If TestPoint is started as a secondary process
             *          accessing the FocalPoint device and the switch is
             *          already up, it will never receive a
             *          FM_EVENT_SWITCH_INSERTED event.
             *****************************************************************/

            if (switchInfo.up)
            {
                status = fmSupportInitialize(currentSwitch);
                if (status != FM_OK)
                {
                    croak("%s", fmErrorMsg(status));
                }
            }
            else
            {
                status = fmCaptureSemaphore(&swigSemHandle, &timeout);
                if (status != FM_OK)
                {
                    croak("%s", fmErrorMsg(status));
                }

                if (currentSwitch == -1)
                {
                    fmGetSwitchFirst(&currentSwitch);
                }
            }

            fmGetSwitchNext(currentSwitch, &nextSwitch);
            currentSwitch = nextSwitch;
        }
        while (currentSwitch != -1);

    }   /* end if (switchCount > 0) */

}

/** fmResetAllSwitchesW
 *
 * \desc            resets all switches
 *
 * \param[in]       freq is the frequency desired (0=unchanged). 
 *
 * \return          FM_OK if successful
 *                  FM_ERR_NO_SWITCHES if no switches have been found
 *                  FM_FAIL if a switch structures access failure has occured
 *
 *****************************************************************************/
fm_status fmResetAllSwitchesW(fm_int freq)
{
    fm_int          switches[FM_MAX_NUM_FOCALPOINTS];
    fm_status       status;
    int             i, n;

    /* Retrieve all switches.  */
    n = 0;
    do
    {
        if (n == 0)
        {
            status = fmGetSwitchFirst(&(switches[n]));
        }
        else
        {
            status = fmGetSwitchNext(switches[n - 1], &(switches[n]));
        }
    } while (status == FM_OK && ++n);
    if (n == 0)
    {
        return FM_ERR_NO_SWITCHES;
    }

    /* Ensure that all switches can be locked.  */
    for (i = 0; i < n; i++)
    {
        if (!SWITCH_LOCK_EXISTS(switches[i]))
        {
            return FM_FAIL;
        }
    }

    /* Lock all switches, then reset all switches and unlock all switches.  */
    for (i = 0; i < n; i++)
    {
        LOCK_SWITCH(switches[i]);
    }
    for (i = 0; i < n; i++)
    {
        status = fmSetSwitchState(switches[i], FM_DISABLED);
        if (status)
        {
            return status;
        }
    }
    if ( freq != 0 )
    {
        status = fmSetApiAttribute(FM_AAK_API_FM4000_BOOT_FHFREQ,FM_API_ATTR_INT,&freq);
        if (status)
        {
            return status;
        }
    }
    for (i = 0; i < n; i++)
    {
        status = fmSetSwitchState(switches[i], FM_ENABLED);
        if (status)
        {
            return status;
        }
    }
    for (i = 0; i < n; i++)
    {
        UNLOCK_SWITCH(switches[i]);
    }
    return FM_OK;
}

fm_status fmGetLogicalPortType(fm_int sw, fm_int port, fm_uint32 *type)
{
    fm_switch * switchPtr;
    fm_port *   portPtr;

    switchPtr = fmRootApi->fmSwitchStateTable[sw];

    if (!switchPtr)
    {
        return FM_ERR_INVALID_SWITCH;
    }

    portPtr = switchPtr->portTable[port];

    if (!portPtr)
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    *type = portPtr->portType;

    return FM_OK;
}

fm_status fmGetAddressTrigger(fm_int sw,
                              fm_macaddr addr,
                              fm_int vlan,
                              fm_int *trig)
{
    fm_internalMacAddrEntry *e = NULL;
    fm_uint32                index;
    fm_status                err;

    err = fmGetAddressInternal(sw, addr, vlan, &e, &index);

    if ( (err == FM_OK) && e )
    {
        *trig = e->trigger;
    }
    else
    {
        *trig = -1;
    }

    return FM_OK;
}

fm_status fmGetAddressBankSet(fm_int sw,
                              fm_macaddr addr,
                              fm_int vlan,
                              fm_int vlan2,
                              fm_int *bank,
                              fm_int *set)
{
    fm_internalMacAddrEntry *e = NULL;
    fm_uint32                index;
    fm_status                err;
    fm_switchInfo            info;

    *bank = -1;
    *set = -1;

    fmGetSwitchInfo(sw, &info);
    
    if (info.switchFamily == FM_SWITCH_FAMILY_FM6000)
    {
        err = fmGetAddressIndex(sw, addr, vlan, vlan2, bank, set);
    }
    else
    {
        err = fmGetAddressInternal(sw, addr, vlan, &e, &index);

        if ( (err == FM_OK) && e )
        {
            *bank = (index & 0x7ff);
            *set = (index >> 11); 
        }
    }

    return FM_OK;
}

void fmDbgClearEventLog()
{
    fmCaptureLock(&localEventQueueLock, FM_WAIT_FOREVER);
    localEventQueuePos = 0;
    memset(&localEventQueue, 0, sizeof(fm_event) * TP_MAX_LOCAL_EVENTS);
    fmReleaseLock(&localEventQueueLock);
}

void fmDbgClearEventPlatformLog()
{
    fmCaptureLock(&localEventQueueLock, FM_WAIT_FOREVER);
    localEventPlatformQueuePos = 0;
    memset(&localEventPlatformQueue, 0, sizeof(fm_event) * TP_MAX_LOCAL_EVENTS_PLATFORM);
    fmReleaseLock(&localEventQueueLock);
}

void fmDbgDumpEventLog()
{
    fm_int      i, j;
    fm_event *  event;
    fm_timestamp delTime;
    fm_uint64 sec;
    fm_char     sipStr[40];
    fm_char     dipStr[40];

    fmCaptureLock(&localEventQueueLock, FM_WAIT_FOREVER);

    printf("\n");
    printf("%16s%-14s Details\n", "Time Stamp   ", "Event Type");

    for (i = 0; i < 79; i++)
    {
        printf("-");
    }
    printf("\n");

    if (localEventQueuePos)
    {
        fmGetSwitchInfo(localEventQueue[0].sw, &swInfo);
    }

    for (i = 0; i < localEventQueuePos; i++)
    {
        event = &localEventQueue[i];
        fmSubTimestamps(&localEventQueue[i].postedTimestamp, &startTimestamp, &delTime);
        sec = delTime.sec + delTime.usec/1000000;
        printf("%4llu:%02llu:%02llu.%03llu  ",
               sec / 3600,
               (sec % 3600)/60,
               sec % 60,
               (delTime.usec % 1000000) / 1000);
        switch (event->type)
        {
            case FM_EVENT_PORT:
                if ( (event->info.fpPortEvent.linkStatus == FM_PORT_STATUS_LINK_UP) ||
                     (event->info.fpPortEvent.linkStatus == FM_PORT_STATUS_LINK_DOWN) )
                {
                    printf("%-14s Port %2d went %s",
                           "Port State", event->info.fpPortEvent.port,
                           event->info.fpPortEvent.linkStatus ? "up" : "down");
                    if (swInfo.switchFamily == FM_SWITCH_FAMILY_FM6000 ||
                        swInfo.switchFamily == FM_SWITCH_FAMILY_REMOTE_FM6000)
                    {
                        printf(". Mac %d (%s)\n",
                               event->info.fpPortEvent.mac,
                               event->info.fpPortEvent.activeMac ? "active" : "standby");
                    }
                    else
                    {
                        printf("\n");
                    }
                }
                else if ( event->info.fpPortEvent.linkStatus == FM_PORT_STATUS_DFE_COMPLETE ||
                          event->info.fpPortEvent.linkStatus == FM_PORT_STATUS_DFE_TIMEOUT  ||
                          event->info.fpPortEvent.linkStatus == FM_PORT_STATUS_DFE_ERROR )
                {
                    if (swInfo.switchFamily == FM_SWITCH_FAMILY_FM6000 ||
                        swInfo.switchFamily == FM_SWITCH_FAMILY_REMOTE_FM6000)
                    {
                        fm_text dfeStatus;

                        switch( event->info.fpPortEvent.linkStatus )
                        {
                            case FM_PORT_STATUS_DFE_COMPLETE:
                                dfeStatus = "Complete";
                                break;
                            case FM_PORT_STATUS_DFE_TIMEOUT:
                                dfeStatus = "Timeout";
                                break;
                            default:
                                dfeStatus = "Error";
                                break;
                        }
                        
                        printf("DFE %s event received on port %2d, mac %d, "
                               "lane %d\n",
                               dfeStatus,
                               event->info.fpPortEvent.port,
                               event->info.fpPortEvent.mac,
                               event->info.fpPortEvent.lane);
                    }
                    else
                    {
                        printf("\n");
                    }

                }
                else
                {
                    printf("%-14s Port %d auto-negotiation %s: codeword 0x%llx\n",
                           "Port State", event->info.fpPortEvent.port,
                           (event->info.fpPortEvent.linkStatus ==
                            FM_PORT_STATUS_AUTONEG_COMPLETE) ? 
                            "complete" :
                            "failed",
                            event->info.fpPortEvent.autonegCode);
                }
                break;
            case FM_EVENT_TABLE_UPDATE:
                for (j = 0; j < (fm_int) event->info.fpUpdateEvent.numUpdates; j++)
                {
                    if (j > 0)
                    {
                        printf("%16s", "");
                    }

                    switch(event->info.fpUpdateEvent.updates[j].event)
                    {
                        case FM_EVENT_ENTRY_LEARNED:
                            printf("%-14s Learned %012llx/%d/%d => ",
                                   "Table Learn", 
                                   event->info.fpUpdateEvent.updates[j].macAddress,
                                   event->info.fpUpdateEvent.updates[j].vlanID,
                                   event->info.fpUpdateEvent.updates[j].vlanID2);

                            if (event->info.fpUpdateEvent.updates[j].destMask == 0xffffffff)
                            {
                                printf("Port %d\n", 
                                       event->info.fpUpdateEvent.updates[j].port);

                            }
                            else
                            {
                                printf("Dest. Mask 0x%x\n", 
                                       event->info.fpUpdateEvent.updates[j].destMask);

                            }
                            break;
                        case FM_EVENT_ENTRY_AGED:
                            printf("%-14s Aged %012llx/%d/%d\n",
                                   "Table Learn", 
                                   event->info.fpUpdateEvent.updates[j].macAddress,
                                   event->info.fpUpdateEvent.updates[j].vlanID,
                                   event->info.fpUpdateEvent.updates[j].vlanID2);
                            break;
                        case FM_EVENT_ENTRY_MEMORY_ERROR:
                            printf("%-14s Mem Error @%d %012llx/%d => ",
                                   "Table Learn", 
                                   event->info.fpUpdateEvent.updates[j].index,
                                   event->info.fpUpdateEvent.updates[j].macAddress,
                                   event->info.fpUpdateEvent.updates[j].vlanID);

                            if (event->info.fpUpdateEvent.updates[j].destMask == 0xffffffff)
                            {
                                printf("Port %d\n", 
                                       event->info.fpUpdateEvent.updates[j].port);

                            }
                            else
                            {
                                printf("Dest. Mask 0x%x\n", 
                                       event->info.fpUpdateEvent.updates[j].destMask);

                            }
                            break;
                    }
                }
                break;
            case FM_EVENT_SECURITY:
                printf("%-14s Violation on port %d, vlan %d, MAC %012llx\n",
                       "Security", 
                       event->info.fpSecEvent.port,
                       event->info.fpSecEvent.vlan,
                       event->info.fpSecEvent.address);
                break;
            case FM_EVENT_ARP:
                fmDbgConvertIPAddressToString(&(event->info.fpArpEvent.arpRedirectSip), sipStr);
                fmDbgConvertIPAddressToString(&(event->info.fpArpEvent.arpRedirectDip), dipStr);
                printf("%-14s Src=%s Dst=%s\n",
                       "Arp-Redirect", 
                       sipStr,
                       dipStr);
            case FM_EVENT_PLATFORM:
                printf("%-14s type %d\n",
                       "Platform", 
                       event->info.fpPlatformEvent.type);
                break;
            default:
                printf("Unknown Event Type(%d)\n", event->type);
                break;
        }
    }

    printf("\n");

    fmReleaseLock(&localEventQueueLock);

}


/* Platform specific event, can't decode it here */
fm_status fmSWIGGetNextEventPlatformType(fm_int *idx, fm_uint64 *timestamp, fm_uint32 *type)
{
    fm_status   status = FM_OK;
    fm_event *  event;
    fm_timestamp delTime;

    *idx = (*idx < 0) ? 0 : *idx + 1;

    if (*idx >= localEventPlatformQueuePos || localEventPlatformQueuePos == 0)
    {
        return FM_ERR_NO_MORE;
    }

    fmCaptureLock(&localEventQueueLock, FM_WAIT_FOREVER);

    event = &localEventPlatformQueue[*idx];
    fmSubTimestamps(&localEventPlatformQueue[*idx].postedTimestamp, &startTimestamp, &delTime);
    *timestamp = delTime.sec*1000000 + delTime.usec;
    switch (event->type)
    {
        case FM_EVENT_PLATFORM:
            *type = event->info.fpPlatformEvent.type;
            break;
        default:
            printf("Unknown Event Type(%d)\n", event->type);
            status = FM_FAIL;
            break;
    }

    fmReleaseLock(&localEventQueueLock);

    return status;

}


/* Platform specific event, can't decode it here into platform event structure */
fm_status fmSWIGGetEventPlatformField(fm_int idx, fm_int offset, fm_int size, void *pEvent)
{
    fm_status   status = FM_OK;
    fm_event *  event;
    fm_timestamp delTime;

    if (idx < 0 || idx >= localEventPlatformQueuePos)
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    fmCaptureLock(&localEventQueueLock, FM_WAIT_FOREVER);

    event = &localEventPlatformQueue[idx];
    memcpy(pEvent, event->info.fpPlatformEvent.eventData + offset, size);

    fmReleaseLock(&localEventQueueLock);

    return status;

}



/** fmCaptureFileCallback
 *
 * \desc        Provided to optimize the TestPoint register capture command:
 *              This function is used as a logging system callback, but it
 *              actually provides two other functions, being the opening and
 *              closing of the capture file. When called as the log callback,
 *              it writes the text string to the file.
 *
 * \param[in]   buf is used according to cookie1:                           \lb
 *                  ccokie1 = 0: buf is the text string to write to the 
 *                               capture file                               \lb
 *                  cookie1 = 1: buf is the name of the capture file        \lb
 *                  cookie1 = 2: buf is not used                            \lb
 *
 * \param[in]   cookie1 is a pointer to a function code to indicate what 
 *              this function should do:                                    \lb
 *                  0 - Write buf to the capture file.
 *                  1 - Register the callback with the logging system and
 *                      open the capture file.                              \lb
 *                  2 - Close the capture file and unregiter the callback   \lb
 *
 * \param[out]  cookie2 points to caller-allocated fm_status that will be set
 *              by this function with one of the following:                 \lb
 *                  FM_OK if successful.                                    \lb
 *                  FM_FAIL if not successful.                              \lb
 *              when cookie1 is anything but 0, otherwise it is ignored.
 *
 *****************************************************************************/
void fmCaptureFileCallback(fm_text buf, void *cookie1, void *cookie2)
{
    static FILE *fileHandle = NULL;
    fm_status err = FM_OK;
    static fm_int cbCookie1 = 0;
    static fm_int cbCookie2 = 0;
    
    fm_logCallBackSpec cbSpec = {fmCaptureFileCallback, &cbCookie1, &cbCookie2};
    
    switch ( *(fm_int *) cookie1 )
    {
        /* Write to the capture file */
        case 0:
            if (fileHandle != NULL)
            {
                /* Write to the file. */
                fprintf(fileHandle, "%s", buf);
            }
            else
            {
                err = FM_FAIL;
            }
            break;
            
        /* Register the callback and open the capture file. */
        case 1:
            if (fileHandle == NULL)
            {
                /* Open the file. */
                fileHandle = fopen(buf, "w");
                if (fileHandle == NULL)
                {
                    printf("ERROR: Unable to open file %s.\n", buf);
                    err = FM_FAIL;
                }
                else
                {
                    err = fmSetLoggingType(FM_LOG_TYPE_CALLBACK,
                                           TRUE,
                                           &cbSpec);
                }
            }
            else
            {
                err = FM_FAIL;
            }
            break;
            
        /* Close the capture file and unregister the callback */
        case 2:
            err = fmSetLoggingType(FM_LOG_TYPE_CONSOLE,
                                   FALSE,
                                   NULL);
                                   
            if (fileHandle != NULL)
            {
                /* Close the file. */
                fclose(fileHandle);
                fileHandle = NULL;
            }
            break;
            
        default:
            printf("ERROR: Unknown function code in cookie1: %d\n", 
                    *(fm_int *) cookie1);
            err = FM_FAIL;
            break;
            
    }   /* end switch ( *(fm_int *) cookie1 ) */
    
    /* Write the status code to cookie2 */
    if ( *(fm_int *) cookie1 != 0 )
    {
        *(fm_int *) cookie2 = err;
    }
    
    return;
}

/** fmGetRootPlatformSwitchFamilyW
 *
 * \desc        Get the switch root family (especially useful to check if the 
 *              switch is remote)
 *
 * \param[in]   sw to get family
 *
 * \param[out]  swFamily is the family of the switch
 *
 *****************************************************************************/
void fmGetRootPlatformSwitchFamilyW(fm_int sw, fm_switchFamily *swFamily)
{
    *swFamily = GET_PLAT_STATE(sw)->family;
}

fm_status fmSWIGSetLBGBins(fm_int sw, fm_int lbgNumber, void *mapRange, void *bins)
{
    fm_status err;
    fm_LBGDistributionMapRange *mapRangePtr = mapRange;

    mapRangePtr->ports = fmAlloc(mapRangePtr->numberOfBins * sizeof(fm_int));
    memcpy(mapRangePtr->ports, bins, mapRangePtr->numberOfBins * sizeof(fm_int));
    err = fmSetLBGAttribute(sw,
                            lbgNumber,
                            FM_LBG_DISTRIBUTION_MAP_RANGE,
                            (void*) mapRangePtr);
    fmFree(mapRangePtr->ports);

    return err;
}

fm_status fmSWIGGetLBGBins(fm_int sw, fm_int lbgNumber, void *mapRange, void *bins)
{
    fm_status err;
    fm_LBGDistributionMapRange *mapRangePtr = mapRange;

    mapRangePtr->ports = fmAlloc(mapRangePtr->numberOfBins * sizeof(fm_int));
    err = fmGetLBGAttribute(sw,
                            lbgNumber,
                            FM_LBG_DISTRIBUTION_MAP_RANGE,
                            (void*) mapRangePtr);

    memcpy(bins, mapRangePtr->ports, mapRangePtr->numberOfBins * sizeof(fm_int));
    fmFree(mapRangePtr->ports);

    return err;
}

fm_int fmSWIGGetMacEntrySize()
{
    return sizeof(fm_macAddressEntry);
}


fm_status fmSWIGSetAnNextPages(fm_int sw, fm_int port, fm_int numPages, void *pages)
{
    fm_status err;
    fm_anNextPages nextPages;

    if (numPages > 0)
    {
        nextPages.nextPages = fmAlloc(numPages * sizeof(fm_uint64));

        if (nextPages.nextPages == NULL)
        {
            return FM_ERR_NO_MEM;
        }

        memcpy(nextPages.nextPages, pages, numPages * sizeof(fm_uint64));
    }
    else
    {
        nextPages.nextPages = NULL;
    }

    nextPages.numPages = numPages;

    err = fmSetPortAttribute(sw,
                             port,
                             FM_PORT_AUTONEG_NEXTPAGES,
                             (void*) &nextPages);

    if (numPages > 0)
    {
        fmFree(nextPages.nextPages);
    }

    return err;
}

/* Assume max of 32 pages */
fm_status fmSWIGGetAnNextPages(fm_int sw, fm_int port, fm_int attr, void *numPages, void *pages)
{

    fm_status      err;
    fm_anNextPages nextPages;
    fm_int        *numPg = (fm_int *)numPages;

    if ( !(attr == FM_PORT_AUTONEG_NEXTPAGES ||
           attr == FM_PORT_AUTONEG_PARTNER_NEXTPAGES) )
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    err = fmGetPortAttribute(sw,
                             port,
                             attr,
                             (void*) &nextPages);

    if (err)
    {
        *numPg = 0;
        return err;
    }

    if (nextPages.numPages > 32)
    {
        printf("Max number of next pages is 32, but got %d\n",
               nextPages.numPages);
        nextPages.numPages = 32;
    }

    if (nextPages.numPages > 0)
    {
        memcpy(pages, nextPages.nextPages, nextPages.numPages * sizeof(fm_uint64));

        /* Free the API allocated memory */
        fmFree(nextPages.nextPages);
    }

    *numPg = nextPages.numPages;

    return err;
}

/* To be used in expert mode when taking a lock is required before
 * executing some functions */
fm_status fmSWIGLockSwitch(fm_int sw)
{
    if (fmRootApi == NULL ||
        fmRootApi->fmSwitchLockTable[sw] == NULL)
    {
        return FM_ERR_INVALID_SWITCH;
    }

    return fmCaptureWriteLock(fmRootApi->fmSwitchLockTable[sw], FM_WAIT_FOREVER);
}

fm_status fmSWIGUnlockSwitch(fm_int sw)
{
    if (fmRootApi == NULL ||
        fmRootApi->fmSwitchLockTable[sw] == NULL)
    {
        return FM_ERR_INVALID_SWITCH;
    }

    return fmReleaseWriteLock(fmRootApi->fmSwitchLockTable[sw]);
}

%}

#endif  /* end USE_ITHREADS */

%ignore fm4000CachedRegisterList;
%ignore fm6000CachedRegisterList;

/* Functions of the files imported will not be wrapped */
%import "platforms/common/model/fm_model_message.h"
%import "platforms/common/model/fm_model_packet_queue.h"
%import "platforms/common/model/fm6000/fm6000_model.h" 

%include <carrays.i>
%array_functions(unsigned char, intArray);
%array_functions(bool, boolArray);

/* The following headers are parsed for conversion.  */
%include "fm_sdk.h"
%include "platform.h"
%include "fm_support.h"

%include "fm_proto_lldp.h"
%include "fm_proto_dcbx.h"

%include "sdk_types.h"
