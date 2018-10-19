/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_alos_event_queue.c
 * Creation Date:   September 16, 2005
 * Description:     Linux-native specific functions for dealing with event queues
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
/** fmEventQueueInitialize
 * \ingroup intAlosEvent
 *
 * \desc            (non-blocking) initializes the queue
 *
 * \note            should be only done once
 *
 * \param[in]       q is the pointer to the event queue to initialize
 *
 * \param[in]       maxSize is the maximum number of events in the queue
 *
 * \param[in]       qName is the name of the queue, for debugging purposes
 *
 * \return          Status code
 *
 *****************************************************************************/
fm_status fmEventQueueInitialize(fm_eventQueue *q, int maxSize, fm_text qName)
{
    fm_status err;

    FM_LOG_ENTRY(FM_LOG_CAT_ALOS, "queue=%p maxSize=%d name=%s\n",
                 (void *) q, maxSize, qName);

    fmDListInit(&q->eventQueue);

    if ( ( err = fmCreateLock(qName, &q->accessLock) ) != FM_OK )
    {
        FM_LOG_EXIT(FM_LOG_CAT_ALOS, err);
    }

    q->size = 0;
    q->max  = maxSize;
    q->name = fmStringDuplicate(qName);

    if (q->name == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ALOS, FM_ERR_NO_MEM);
    }

    /* Notify debug system. */
    fmDbgEventQueueCreated(q);

    FM_LOG_EXIT(FM_LOG_CAT_ALOS, FM_OK);

}   /* end fmEventQueueInitialize */




/*****************************************************************************/
/** fmEventQueueAdd
 * \ingroup intAlosEvent
 *
 * \desc            (blocking) enqueue an event
 *
 * \param[in]       q is the pointer to the event queue
 *
 * \param[in]       event is the pointer to the event to be added to the queue
 *
 * \return          Status code
 *
 *****************************************************************************/
fm_status fmEventQueueAdd(fm_eventQueue *q, fm_event *event)
{
    fm_status err, rerr = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_ALOS, "queue=%p event=%p\n",
                 (void *) q, (void *) event);

    if (q->size == q->max)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ALOS, FM_ERR_EVENT_QUEUE_FULL);
    }

    /* FIXME: timeout not implememented, so we block forever if we deadlock */
    if ( ( err = fmCaptureLock(&q->accessLock, FM_WAIT_FOREVER) ) != FM_OK )
    {
        FM_LOG_EXIT(FM_LOG_CAT_ALOS, err);
    }

#ifdef ENABLE_EVENTQ_TIMESTAMP
    /* Don't enable by default, slow down packet delivery */
    if (fmGetTime(&event->postedTimestamp) != 0)
    {
        rerr = FM_ERR_BAD_GETTIME;
    }
    else
#endif
    {
        if ( ( err = fmDListInsertEnd(&q->eventQueue, event) ) == FM_OK )
        {
            q->totalEventsPosted++;
            q->size++;
            q->maxSize = q->size > q->maxSize ? q->size : q->maxSize; 
        }
        else
        {
            rerr = err;
        }
    }

    if ( ( err = fmReleaseLock(&q->accessLock) ) != FM_OK )
    {
        FM_LOG_EXIT(FM_LOG_CAT_ALOS, err);
    }

    FM_LOG_EXIT(FM_LOG_CAT_ALOS, rerr);

}   /* end fmEventQueueAdd */




/*****************************************************************************/
/** fmEventQueueGet
 * \ingroup intAlosEvent
 *
 * \desc            (blocking) get the next event
 *
 * \param[in]       q is the pointer to the event queue
 *
 * \param[out]      eventPtr is a pointer to storage for the event pointer
 *
 * \return          Status code
 *
 *****************************************************************************/
fm_status fmEventQueueGet(fm_eventQueue *q, fm_event **eventPtr)
{
    fm_status err;
    fm_event *ev;

    FM_LOG_ENTRY(FM_LOG_CAT_ALOS, "queue=%p event=%p\n",
                 (void *) q, (void *) eventPtr);

    if (eventPtr == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ALOS, FM_ERR_INVALID_ARGUMENT);
    }

    /* FIXME: timeout not implememented, so we block forever if we deadlock */
    if ( ( err = fmCaptureLock(&q->accessLock, FM_WAIT_FOREVER) ) != FM_OK )
    {
        FM_LOG_EXIT(FM_LOG_CAT_ALOS, err);
    }

    ev = (fm_event *) fmDListRemove(&q->eventQueue, q->eventQueue.head);

    if (ev)
    {
        /* record the time of its removal before notifying debug. */
#ifdef ENABLE_EVENTQ_TIMESTAMP
        fmGetTime(&ev->poppedTimestamp);
#endif

        *eventPtr = ev;
        q->size--;
#ifdef ENABLE_EVENTQ_TIMESTAMP
        fmDbgEventQueueEventPopped(q, ev);
#endif
    }
    else
    {
        *eventPtr = NULL;

        /**************************************************
         * The event queue is empty, this an error condiion 
         * Release the event queue access lock then increase 
         * the appropriate diagnostic counter, which requires 
         * the debug lock to be taken 
         ***************************************************/

        err = fmReleaseLock(&q->accessLock);

        fmDbgGlobalDiagCountIncr(FM_GLOBAL_CTR_NO_EVENTS_AVAILABLE, 1);

        if ( err != FM_OK )
        {
            FM_LOG_EXIT(FM_LOG_CAT_ALOS, err);
        }

        FM_LOG_EXIT(FM_LOG_CAT_ALOS, FM_ERR_NO_EVENTS_AVAILABLE);
    }

    if ( ( err = fmReleaseLock(&q->accessLock) ) != FM_OK )
    {
        FM_LOG_EXIT(FM_LOG_CAT_ALOS, err);
    }

    FM_LOG_EXIT(FM_LOG_CAT_ALOS, FM_OK);

}   /* end fmEventQueueGet */




/*****************************************************************************/
/** fmEventQueuePeek
 * \ingroup intAlosEvent
 *
 * \desc            peek the next event
 *
 * \param[in]       q is the pointer to the event queue
 *
 * \param[out]      eventPtr is a pointer to storage for the event pointer
 *
 * \return          Status code
 *
 *****************************************************************************/
fm_status fmEventQueuePeek(fm_eventQueue *q, fm_event **eventPtr)
{
    fm_status err;
    fm_event *ev;

    FM_LOG_ENTRY(FM_LOG_CAT_ALOS, "queue=%p event=%p\n",
                 (void *) q, (void *) eventPtr);

    if (eventPtr == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ALOS, FM_ERR_INVALID_ARGUMENT);
    }

    /* FIXME: timeout not implememented, so we block forever if we deadlock */
    if ( ( err = fmCaptureLock(&q->accessLock, FM_WAIT_FOREVER) ) != FM_OK )
    {
        FM_LOG_EXIT(FM_LOG_CAT_ALOS, err);
    }

    if (q->eventQueue.head)
    {
        ev        = (fm_event *) q->eventQueue.head->data;
        *eventPtr = ev;
    }
    else
    {
        *eventPtr = NULL;

        if ( ( err = fmReleaseLock(&q->accessLock) ) != FM_OK )
        {
            FM_LOG_EXIT(FM_LOG_CAT_ALOS, err);
        }

        FM_LOG_EXIT(FM_LOG_CAT_ALOS, FM_ERR_NO_EVENTS_AVAILABLE);
    }

    if ( ( err = fmReleaseLock(&q->accessLock) ) != FM_OK )
    {
        FM_LOG_EXIT(FM_LOG_CAT_ALOS, err);
    }

    FM_LOG_EXIT(FM_LOG_CAT_ALOS, FM_OK);

}   /* end fmEventQueuePeek */




/*****************************************************************************/
/** fmEventQueueDestroy
 * \ingroup intAlosEvent
 *
 * \desc            (non-blocking) cleans up the queue
 *
 * \param[in]       q is the pointer to the event queue
 *
 * \return          Status code
 *
 *****************************************************************************/
fm_status fmEventQueueDestroy(fm_eventQueue *q)
{
    fm_status err;

    FM_LOG_ENTRY(FM_LOG_CAT_ALOS, "queue=%p\n", (void *) q);

    /* Notify debug system first */
    fmDbgEventQueueDestroyed(q);

    if ( ( err = fmDeleteLock(&q->accessLock) ) != FM_OK )
    {
        FM_LOG_EXIT(FM_LOG_CAT_ALOS, err);
    }

    fmFree(q->name);
    q->name = NULL;

    fmDListFree(&q->eventQueue);

    FM_LOG_EXIT(FM_LOG_CAT_ALOS, FM_OK);

}   /* end fmEventQueueDestroy */




/*****************************************************************************/
/** fmEventQueueCount
 * \ingroup intAlosEvent
 *
 * \desc            (non-blocking) returns the number of entries currently
 *                  in the queue
 *
 * \param[in]       q is the pointer to the event queue
 *
 * \param[in]       eventCount points to where the count should be stored
 *
 * \return          Status code
 *
 *****************************************************************************/
fm_status fmEventQueueCount(fm_eventQueue *q, fm_int *eventCount)
{
    FM_LOG_ENTRY(FM_LOG_CAT_ALOS, "queue=%p count=%p\n",
                 (void *) q, (void *) eventCount);

    *eventCount = q->size;

    FM_LOG_EXIT(FM_LOG_CAT_ALOS, FM_OK);

}   /* end fmEventQueueCount */
