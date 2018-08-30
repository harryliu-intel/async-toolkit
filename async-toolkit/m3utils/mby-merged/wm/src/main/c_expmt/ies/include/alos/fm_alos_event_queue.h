/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_alos_event_queue.h
 * Creation Date:   2005
 * Description:     Defines an event structure and a wrapper to hold a
 *                  queue of events
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

#ifndef __FM_FM_ALOS_EVENT_QUEUE_H
#define __FM_FM_ALOS_EVENT_QUEUE_H


/**************************************************/
/** \ingroup intTypeStruct
 *
 *  Encapsulates a thread-safe event queue.
 **************************************************/
typedef struct _fm_eventQueue
{
    /** The heart of the queue is a doubly-linked list */
    fm_dlist eventQueue;

    /** lock for both queue read and write access */
    fm_lock  accessLock;

    /** size for convenience */
    fm_int   size;

    /** total number of allowed events */
    fm_int   max;

    /** event queue name, for debugging */
    fm_text  name;

    /** Other debug variables. */
    fm_uint  totalEventsPosted;
    fm_uint  totalEventsPopped;
    fm_int   maxSize; 
    
    /** Event propogation timings. */
    fm_float avgTime;
    fm_float minTime;
    fm_float maxTime;

} fm_eventQueue;


/* (non-blocking) initializes the queue, should be only done once */
fm_status fmEventQueueInitialize(fm_eventQueue *q, int maxSize, fm_text qName);


/* (blocking) enqueue an event at current + timeDelta time */
fm_status fmEventQueueAdd(fm_eventQueue *q, fm_event *event);


/* (blocking) get the next event whose timestamp has expired */
fm_status fmEventQueueGet(fm_eventQueue *q, fm_event **eventPtr);


/* (non-blocking) peek the next event */
fm_status fmEventQueuePeek(fm_eventQueue *q, fm_event **eventPtr);


/* (non-blocking) cleans up the queue */
fm_status fmEventQueueDestroy(fm_eventQueue *q);


/* (non-blocking) returns the number of entries currently in the queue */
fm_status fmEventQueueCount(fm_eventQueue *q, fm_int *eventCount);


#endif /* __FM_FM_ALOS_EVENT_QUEUE_H */
