/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_events_int.h
 * Creation Date:   2005
 * Description:     Contains non-exposed functions for event handling threads
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

#ifndef __FM_FM_API_EVENTS_INT_H
#define __FM_FM_API_EVENTS_INT_H

typedef struct _fm_localDelivery
{
    /* each bit represents an event type-- set means deliver to this thread */
    fm_uint32  mask;

    /* thread that delivers events to its process */
    fm_thread *thread;

    /* the ID of the process that this thread is for */
    fm_int     processId;

} fm_localDelivery;


/* event handler for dispatching events to the stack */
void *fmGlobalEventHandler(void *args);


/* event handler for dispatching events to a particular process */
void *fmLocalEventHandler(void *args);

/* Receive Packet Thread */
void *fmReceivePacketTask(void *args);

void *fmFastMaintenanceTask(void *args);

void *fmParitySweeperTask(void *args);

fm_status fmSendParityErrorEvent(fm_int sw, 
                                 fm_eventParityError parityEvent, 
                                 fm_thread *eventHandler);

const char * fmParityErrTypeToText(fm_parityErrType errType);
const char * fmParityMemAreaToText(fm_parityMemArea memArea);
const char * fmParitySeverityToText(fm_paritySeverity severity);
const char * fmParityStatusToText(fm_parityStatus status);

void fmDbgDumpParityErrorEvent(fm_int                sw,
                               fm_eventParityError * parityEvent);

/* Distribute Events to interested processes */
void fmDistributeEvent(fm_event *event);

/* Remove event handler */
fm_status fmRemoveEventHandler(fm_localDelivery ** delivery);

/* Switch-Specific Event Handler */
typedef void (*fm_switchEventHandler)(fm_event *event);


/* gets the upper layer event handler for a specific switch.  Returns
 * NULL if the switch is using the global event handler */
fm_status fmGetSwitchEventHandler(fm_int                 sw,
                                  fm_switchEventHandler *eventHandlerFuncPtr);


/* sets the upper layer event handler for a specific switch */
fm_status fmSetSwitchEventHandler(fm_int                sw,
                                  fm_switchEventHandler eventHandlerFunc);


extern fm_bool localDispatchThreadExit;

#endif /* __FM_FM_API_EVENTS_INT_H */
