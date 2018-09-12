/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            lldp_event.h
 * Creation Date:   September 1, 2010
 * Description:     Prototypes and structure definitions for Event-Queue.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2010 - 2011 Intel Corporation. All Rights Reserved. 
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

#ifndef LLDP_EVENT_H
#define LLDP_EVENT_H

#include <stdint.h>
#include <pthread.h>
#include <sys/time.h>

#include <lldp_list.h>

#define LLDP_EVENT_MAX_DATA_SIZE 2048
#define LLDP_EVENT_WAIT_FOREVER  NULL

/** LLDP Event types. */
typedef enum {
    /** Event notifying the LLDP manager that local settings has being changed */
    LLDP_EVENT_LOCAL_CHANGED,

    /** Event notifying the LLDP manager with new RX LLDP packet */ 
    LLDP_EVENT_PACKET
} lldp_eventType;

/** LLDP event */
typedef struct lldp_event {
    /** Type is the type of the LLDP event */
    lldp_eventType type;

    /** Port is the port that reports the event */
    int port;

    /** Size if event data */
    int size;

    /** Data of the event */
    uint8_t data[LLDP_EVENT_MAX_DATA_SIZE];

    lldp_list node;
} lldp_event;

/** Event-queue */
typedef struct lldp_eventQueue {
    /** Event queue lock */
    pthread_mutex_t lock;
 
    /** Event queue condition */
    pthread_cond_t cond;

    /** Link-list of current events stored in event queue */
    lldp_list list;
} lldp_eventQueue;

/** Initialize Event-Queue Object */
void lldpEventQueueInit(lldp_eventQueue *queue);

/** Cleanup Event-Queue Object */
void lldpEventQueueDestroy(lldp_eventQueue *queue);

/** Push Event to Event-Queue */
void lldpEventQueuePost(lldp_eventQueue *queue, lldp_event *event);

/** Pop Event from Event-Queue */
lldp_event *lldpEventQueueWait(lldp_eventQueue *queue, const struct timeval *timeout);

#endif /* LLDP_EVENT_H */
