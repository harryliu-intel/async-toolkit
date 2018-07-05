/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_event_mgmt.h
 * Creation Date:   May 15, 2007
 * Description:     Functions for dealing with events
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

#ifndef __FM_FM_API_EVENT_MGMT_H
#define __FM_FM_API_EVENT_MGMT_H

typedef enum
{
    /* Notification is done in strict order with this first */
    EVENT_FREE_NOTIFY_PKT_INTR,
    EVENT_FREE_NOTIFY_LINK_TRANSITION,

    /* Add additional notification above here */
    MAX_EVENT_FREE_NOTIFY_HANDLER,
} fm_eventFreeNotify;

typedef void (*fm_eventFreeNotifyHndlr)(fm_int sw);

/* allocate an event buffer */
fm_event *fmAllocateEvent(fm_int           sw,
                          fm_eventID       eventID,
                          fm_int           eventType,
                          fm_eventPriority priority);


/* release an event buffer */
fm_status fmReleaseEvent(fm_event *event);

fm_status fmAddEventFreeNotify(fm_int sw,
                               fm_eventFreeNotify type,
                               fm_eventFreeNotifyHndlr handler);

/* initialize event handling subsystem */
fm_status fmEventHandlingInitialize(void);


/* prototypes for the generic event tasks */
void *fmDebounceLinkStateTask(void *args);
void *fmInterruptHandler(void *args);
fm_status fmSendSoftwareEvent(fm_int sw, fm_uint32 events);


#endif /* __FM_FM_API_EVENT_MGMT_H */
