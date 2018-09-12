/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            lldp_event.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of LLDP Event-Queue.
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#include <lldp_list.h>
#include <lldp_event.h>

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
/** lldpEventQueueInit
 * \ingroup lldp
 *
 * \desc            Initializes event-queue object.
 *
 * \param[in]       queue is a pointer to the event-queue object to be
 *                  initialized.
 *
 *****************************************************************************/
void lldpEventQueueInit(lldp_eventQueue *queue)
{
    lldpListInit(&queue->list);
    pthread_mutex_init(&queue->lock, NULL);
    pthread_cond_init(&queue->cond, NULL);
}

/*****************************************************************************/
/** lldpEventQueueDestroy
 * \ingroup lldp
 *
 * \desc            Cleans-up event-queue object.
 *
 * \param[in]       queue is a pointer to the event-queue object to be
 *                  cleared.
 *
 *****************************************************************************/
void lldpEventQueueDestroy(lldp_eventQueue *queue)
{
    lldp_event *event;
    lldp_elem *node;

    /* release any task that might me pending for event */
    pthread_mutex_lock(&queue->lock);

    do {
        node  = lldpListPop(&queue->list);
        event = CONTAINER(node, lldp_event, node);

        if (event)
            free(event);
    }
    while (event);

    pthread_cond_broadcast(&queue->cond);
    pthread_mutex_unlock(&queue->lock);

    pthread_mutex_destroy(&queue->lock);
    pthread_cond_destroy(&queue->cond);
}

/*****************************************************************************/
/** lldpEventQueuePost
 * \ingroup lldp
 *
 * \desc            Push event object to event-queue.
 *
 * \param[in]       queue is a pointer to the event-queue.
 *
 * \param[in]       event is a pointer to the event that to be pushed into
 *                  the event-queue.
 *
 *****************************************************************************/
void lldpEventQueuePost(lldp_eventQueue *queue, lldp_event *event)
{
    pthread_mutex_lock(&queue->lock);
    lldpListPush(&queue->list, &event->node);
    pthread_cond_signal(&queue->cond);
    pthread_mutex_unlock(&queue->lock);
}

/*****************************************************************************/
/** lldpEventQueueWait
 * \ingroup lldp
 *
 * \desc            Pop event object to event-queue. Wait until timeout.
 *
 * \param[in]       queue is a pointer to the event-queue.
 *
 * \param[in]       timeout is the wait expiration time (relative time).
 *
 * \return          pointer to the popped event if successful.
 * \return          NULL on timeout.
 *
 *****************************************************************************/
lldp_event *lldpEventQueueWait(lldp_eventQueue *queue,
                               const struct timeval *timeout)
{
    struct timeval tv;
    struct timespec ts;

    lldp_event *event = NULL;
    lldp_list  *node = NULL;

    pthread_mutex_lock(&queue->lock);

    if (lldpListIsEmpty(&queue->list)) {
        if (timeout) {
            /* get timeout absolute time */
            gettimeofday(&tv, NULL);
            ts.tv_sec = (tv.tv_sec + timeout->tv_sec);
            ts.tv_nsec = (tv.tv_usec + timeout->tv_usec) * 1000;

            /* handle overflow of nsec */
            while (ts.tv_nsec >= 1000000000) {
                ts.tv_sec++;
                ts.tv_nsec -= 1000000000;
            }

            /* wait for event for specified timeout */
            pthread_cond_timedwait(&queue->cond, &queue->lock, &ts);
        } else {
            /* wait for event forever */
            pthread_cond_wait(&queue->cond, &queue->lock);
        }
    }

    /* get event */
    node  = lldpListPop(&queue->list);
    event = CONTAINER(node, lldp_event, node);

    pthread_mutex_unlock(&queue->lock);

    return event;
}

