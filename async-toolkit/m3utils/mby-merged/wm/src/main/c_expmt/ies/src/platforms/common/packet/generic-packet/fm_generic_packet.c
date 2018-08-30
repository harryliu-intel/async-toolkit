/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_generic_packet.c
 * Creation Date:   August, 2011
 * Description:     Generic packet sending code.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2006 - 2012 Intel Corporation. All Rights Reserved. 
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

/* Some platforms might not define this */
#ifndef FM_PACKET_OFFSET_ETHERTYPE
#define FM_PACKET_OFFSET_ETHERTYPE 3
#endif

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
/** fmPacketQueueLock
 * \ingroup intPlatformCommon
 *
 * \desc            Lock packet queue.
 *
 * \param[in]       queue is the pointer to the packet queue.
 *
 * \return          NONE
 *
 *****************************************************************************/
void fmPacketQueueLock(fm_packetQueue *queue)
{
#if 1
    if (pthread_mutex_lock(&queue->mutex))
    {
        FM_LOG_ASSERT(FM_LOG_CAT_EVENT_PKT_TX, 
                      FALSE,
                      "Failed to lock queue's mutex!\n");
    }
#else
    TAKE_PLAT_LOCK(queue->switchNum, FM_PLAT_INFO);
#endif

}   /* end fmPacketQueueLock */




/*****************************************************************************/
/** fmPacketQueueUnlock
 * \ingroup intPlatformCommon
 *
 * \desc            Unlock packet queue.
 *
 * \param[in]       queue is the pointer to the packet queue.
 *
 * \return          NONE
 *
 *****************************************************************************/
void fmPacketQueueUnlock(fm_packetQueue *queue)
{
#if 1
    if (pthread_mutex_unlock(&queue->mutex))
    {
        FM_LOG_ASSERT(FM_LOG_CAT_EVENT_PKT_TX, 
                      FALSE,
                      "Failed to unlock queue's mutex!\n");
    }
#else
    DROP_PLAT_LOCK(queue->switchNum, FM_PLAT_INFO);
#endif

}   /* end fmPacketQueueUnlock */




/*****************************************************************************/
/** fmPacketQueueInit
 * \ingroup intPlatformCommon
 *
 * \desc            Initialize packet queue.
 *
 * \param[in]       queue is the pointer to the packet queue.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmPacketQueueInit(fm_packetQueue *queue, fm_int sw)
{
    /* Initialize mutex if it is enabled */
    pthread_mutexattr_t attr;

    if (!queue) 
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    memset(queue, 0, sizeof(*queue));
    queue->switchNum = sw;

    pthread_mutexattr_init(&attr);

    if ( pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE) )
    {
        pthread_mutexattr_destroy(&attr);
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, FM_ERR_LOCK_INIT);
    }

    pthread_mutex_init(&queue->mutex, &attr);
    pthread_mutexattr_destroy(&attr);

    return FM_OK;

}   /* end fmPacketQueueInit */




/*****************************************************************************/
/** fmPacketQueueFree
 * \ingroup intPlatformCommon
 *
 * \desc            Free all buffers in packet queue.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmPacketQueueFree(fm_int sw)
{
    fm_status       err       = FM_OK;
    fm_packetQueue *txQueue;
    fm_packetEntry *packet;

    txQueue = &GET_PLAT_PKT_STATE(sw)->txQueue;
    fmPacketQueueLock(txQueue);

    for ( ;
          txQueue->pullIndex != txQueue->pushIndex ;
          txQueue->pullIndex = (txQueue->pullIndex + 1) % FM_PACKET_QUEUE_SIZE)
    {
        packet = &txQueue->packetQueueList[txQueue->pullIndex];

        if (packet && packet->freePacketBuffer)
        {
            fmFreeBuffer(sw, packet->packet);
        }

        fmDbgGlobalDiagCountIncr(FM_GLOBAL_CTR_TX_BUFFER_FREES, 1);

    }

    fmPacketQueueUnlock(txQueue);

    return err;

}   /* end fmPacketQueueFree */




/*****************************************************************************/
/** fmPacketQueueUpdate
 * \ingroup intPlatformCommon
 *
 * \desc            Update packet queue by advancing one entry
 *
 * \param[in]       queue is the pointer to the packet queue.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_TX_PACKET_QUEUE_FULL if queue if full.
 *
 *****************************************************************************/
fm_status fmPacketQueueUpdate(fm_packetQueue *queue)
{

    /* check if the Tx queue is full */
    if ( (queue->pushIndex + 1) % FM_PACKET_QUEUE_SIZE == queue->pullIndex )
    {
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                     "fmPacketQueueUpdate:"
                     "TX queue is full?: pushIndex = %d, pullIndex = %d\n",
                     queue->pushIndex, queue->pullIndex);
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, FM_ERR_TX_PACKET_QUEUE_FULL);
    }
    else
    {
        /* updated indices */
        ++queue->pushIndex;
        queue->pushIndex = queue->pushIndex % FM_PACKET_QUEUE_SIZE;
    }

    return FM_OK;

}   /* end fmPacketQueueUpdate */




/*****************************************************************************/
/** fmPacketQueueEnqueue
 * \ingroup intPlatformCommon
 *
 * \desc            Queue packet to the tx packet queue
 *
 * \param[in]       queue is the pointer to the packet queue.
 *
 * \param[in]       packet is the buffer containing the packet.
 *
 * \param[in]       packetLength is the length of the packet.
 *
 * \param[in]       islTag is the pointer containing ISL tag words.
 *
 * \param[in]       suppressVlanTag is the flag to indicate whether
 *                  to suppress the vlan tag in the data.
 *
 * \param[in]       freeBuffer is the flag to indicate whether
 *                  to free the packet buffer or not.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmPacketQueueEnqueue(fm_packetQueue * queue,
                               fm_buffer *      packet,
                               fm_int           packetLength,
                               fm_islTag *      islTag,
                               fm_bool          suppressVlanTag,
                               fm_bool          freeBuffer)
{
    fm_packetEntry *entry;

    entry = &queue->packetQueueList[queue->pushIndex];

    entry->packet = packet;
    entry->length = packetLength;
    entry->fcsVal = FM_USE_DEFAULT_FCS;
    FM_MEMCPY_S( &entry->islTag,
                 sizeof(entry->islTag),
                 islTag,
                 sizeof(fm_islTag) );
    entry->suppressVlanTag  = suppressVlanTag;
    entry->freePacketBuffer = freeBuffer;

    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                 "fm_packet_queue_enqueue: packet queued "
                 "in slot %d, length %d bytes\n",
                 queue->pushIndex,
                 entry->length);

    /* check if the Tx queue is full */
    if ( (queue->pushIndex + 1) % FM_PACKET_QUEUE_SIZE == queue->pullIndex )
    {
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                     "fm_packet_queue_enqueue:"
                     "TX queue is full?: pushIndex = %d, pullIndex = %d\n",
                     queue->pushIndex, queue->pullIndex);
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, FM_ERR_TX_PACKET_QUEUE_FULL);
    }
    else
    {
        /* updated indices */
        ++queue->pushIndex;
        queue->pushIndex = queue->pushIndex % FM_PACKET_QUEUE_SIZE;
    }

    return FM_OK;

}   /* end fmPacketQueueEnqueue */




/*****************************************************************************/
/** fmPacketReceiveEnqueue
 * \ingroup intPlatformCommon
 *
 * \desc            Processes a complete received packet event and
 *                  tries to send it upward.
 *
 * \note            This is an optimized version for packet processing without
 *                  any locking. The input parameters should already been
 *                  validated before calling this function.
 *
 * \param[in]       sw is the switch to send the event for.
 *
 * \param[in]       event is a fully filled event structure.
 *
 * \param[in]       selfTestEventHandler is a pointer to the event handler.
 *
 * \return          FM_OK if successful
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmPacketReceiveEnqueue(fm_int     sw,
                                 fm_event * event,
                                 fm_switchEventHandler selfTestEventHandler)
{
    fm_eventPktRecv *pktEvent = &event->info.fpPktEvent;
    fm_status               err      = FM_OK;
    fm_bool                 isLacpToBeDropped;
    fm_bool                 isPktSFlowLogged;
    fm_switchEventHandler   switchEventHandler;
    fm_switch              *switchPtr;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_RX,
                 "sw = %d, event->type = %d, event->sw = %d, "
                 "event->eventID = %d\n",
                 sw,
                 event->type,
                 event->sw,
                 event->eventID);

    switchPtr = GET_SWITCH_PTR(sw);

    switchEventHandler = switchPtr->eventHandler;

    /***************************************************
     * Check the LACP filter configuration based on the
     * switch the event is associated with, not the
     * switch the frame was received on.  These are
     * potentially different in an FIBM environment.
     **************************************************/
    fmCheckLACPFilter(event->sw, pktEvent, &isLacpToBeDropped);

    if (!isLacpToBeDropped)
    {
        /* Check if the packet is logged by a sFlow instance */
        (void)switchPtr->CheckSFlowLogging(sw, pktEvent, &isPktSFlowLogged);

        if (isPktSFlowLogged)
        {
            /* Change the type to FM_EVENT_SFLOW_PKT_RECV */
            event->type = FM_EVENT_SFLOW_PKT_RECV;
        }

        /* Don't do a direct enqueue if in self-test */
        if (GET_PLAT_PKT_STATE(sw)->rxDirectEnqueueing &&
            switchEventHandler != selfTestEventHandler)
        {
            /* Pass directly up to the application */
            if (fmEventHandler != NULL)
            {
                fmEventHandler(event->type, event->sw, &event->info);
                /* Don't need to release event, since event structure
                 * is on the stack for direct enqueueing method
                 */
            }
            else
            {
                /* Drop the packet without forwarding it. */
                fmFreeBufferChain(sw, (fm_buffer *) pktEvent->pkt);
                fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_DROPS_NO_PORT, 1);
            }
            FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, FM_OK);
        }
        else
        {
            err = fmSendThreadEvent(&fmRootApi->eventThread, event);

            if (err != FM_OK)
            {
                /* Free the event since we could not send it to thread */
                fmFreeBufferChain(sw, (fm_buffer *) pktEvent->pkt);
                fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_DROPS_NO_EVENT, 1);
                fmReleaseEvent(event);
            }
        }

    }
    else
    {
        /* Drop the packet without forwarding it. */
        fmFreeBufferChain(sw, (fm_buffer *) pktEvent->pkt);
        fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_DROPS_LACP, 1);
        fmReleaseEvent(event);
    }

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, err);

}   /* end fmPacketReceiveEnqueue */




/*****************************************************************************/
/** fmPacketClearCRC
 * \ingroup intPlatformCommon
 *
 * \desc            Clear the last 4 bytes in the buffer chain. 
 *
 * \param[in]       buffer points to the receive packet buffer.
 *
 * \return          NONE
 *
 *****************************************************************************/
void fmPacketClearCRC(fm_buffer *buffer)
{
    fm_buffer *lastBuf = buffer;
    fm_buffer *nextToLastBuf = buffer;
    fm_byte   *pByte;
    fm_int    rem;

    if (buffer->next == NULL)
    {
        /* Quick check for small packets */
        pByte = (fm_byte*)buffer->data;
        memset(pByte+buffer->len-4, 0, 4);
        return;
    }

    while (buffer)
    {
        nextToLastBuf = lastBuf;
        lastBuf = buffer;
        buffer = buffer->next;
    }


    if (lastBuf->len >=4)
    {
        pByte = (fm_byte*)lastBuf->data;
        memset(pByte+lastBuf->len-4, 0, 4);
    }
    else
    {
        pByte = (fm_byte*)lastBuf->data;
        memset(pByte, 0, lastBuf->len);

        rem = 4 - lastBuf->len;
        pByte = (fm_byte*)nextToLastBuf->data;
        memset(pByte+nextToLastBuf->len-rem, 0, rem);
    }

}   /* end fmPacketClearCRC */




/*****************************************************************************/
/** fmPacketGetCRC
 * \ingroup intPlatformCommon
 *
 * \desc            Returns the last 4 bytes in the buffer chain.
 *
 * \param[in]       buffer points to the receive packet buffer.
 *
 * \return          FCS value for the buffer.
 *
 *****************************************************************************/
fm_uint32 fmPacketGetCRC(fm_buffer *buffer)
{
    fm_buffer * lastBuf = NULL;
    fm_buffer * prevBuf = NULL;
    fm_byte   * pByte;
    fm_int      rem;

    union {
        fm_uint32 w;
        fm_byte   b[4];
    } fcsVal;

    if (buffer == NULL)
    {
        return 0;
    }

    while (buffer)
    {
        prevBuf = lastBuf;
        lastBuf = buffer;
        buffer = buffer->next;
    }

    if (lastBuf->len >= 4)
    {
        pByte = ((fm_byte *) lastBuf->data) + lastBuf->len - 4;
        FM_MEMCPY_S(&fcsVal.b[0], sizeof(fcsVal.b), pByte, 4);
    }
    else if (prevBuf == NULL)
    {
        return 0;
    }
    else
    {
        rem = 4 - lastBuf->len;
        pByte = ((fm_byte *) prevBuf->data) + prevBuf->len - rem;
        FM_MEMCPY_S(fcsVal.b, sizeof(fcsVal.b), pByte, rem);
        FM_MEMCPY_S(&fcsVal.b[rem], 4 - rem, lastBuf->data, lastBuf->len);
    }

    return ntohl(fcsVal.w);

}   /* end fmPacketGetCRC */




/*****************************************************************************/
/** fmFindSlaveSwitchPortByGlort
 * \ingroup intPlatformCommon
 *
 * \desc            Find out which switch and port this glort belong to
 *
 * \param[in]       glort is the glort number.
 *
 * \param[out]      switchNum is the pointer to hold the switch number.
 *
 * \param[out]      port is the pointer to hold the port number.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL if not found.
 *
 *****************************************************************************/
fm_status fmFindSlaveSwitchPortByGlort(fm_uint32 glort, 
                                       fm_int *switchNum, 
                                       fm_int *port)
{
    fm_int    sw;
    fm_status err = FM_FAIL;

    for (sw = FM_FIRST_FOCALPOINT ; sw <= FM_LAST_FOCALPOINT ; sw++)
    {
        fm_switch *switchPtr;

        switchPtr = GET_SWITCH_PTR(sw);

        if (!switchPtr)
        {
            continue;
        }
        /* Need to take a switch lock here ?? */
        if ( (glort & ~switchPtr->glortRange.glortMask) ==
            switchPtr->glortRange.glortBase )
        {
            *switchNum = sw;
            err = fmGetGlortLogicalPort(sw, glort, port);
            break;
        }
    }

    return err;

}   /* end fmFindSlaveSwitchPortByGlort */




/*****************************************************************************/
/** fmComputeTotalPacketLength
 * \ingroup intPlatformCommon
 *
 * \desc            Computes the packet length by iterating through buffers.
 *
 * \param[in]       packet is a pointer to an fm_buffer representing the first
 *                  buffer in the packet.
 *
 * \return          The packet length in bytes.
 *
 *****************************************************************************/
fm_int fmComputeTotalPacketLength(fm_buffer *packet)
{
    fm_int length = 0;

    while (packet)
    {
        if ( (packet->len < 0) || (packet->len > FM_BUFFER_SIZE_BYTES) )
        {
            length = -1;
            return length;
        }
        length += packet->len;
        packet  = packet->next;
    }

    return length;

}   /* end fmComputeTotalPacketLength */




/*****************************************************************************/
/** fmGetCpuPort
 * \ingroup intPlatformCommon
 *
 * \desc            Returns the logical port number for the CPU port for
 *                  the specified switch.
 *
 * \note            This is an optimized version for packet processing without
 *                  any locking. The input parameters should already been
 *                  validated before calling this function.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      cpuPort contains the logical port number for the CPU port.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmGetCpuPortInt(fm_int sw, fm_int *cpuPort)
{
    fm_switch *switchPtr;

    switchPtr = GET_SWITCH_PTR(sw);

    return switchPtr->GetCpuPort(sw, cpuPort);

}   /* end fmGetCpuPortInt */




/*****************************************************************************/
/** fmGetPortDefVlanInt
 * \ingroup intPlatformCommon
 *
 * \desc            Returns the port default vlan.
 *
 * \note            This is an optimized version for packet processing without
 *                  any locking. The input parameters should already been
 *                  validated before calling this function.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the logical port to get the default vlan.
 *
 * \param[out]      vlan points to caller allocated storage where default vlan
 *                  is stored.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmGetPortDefVlanInt(fm_int     sw,
                              fm_int     port,
                              fm_uint16 *vlan)
{
    fm_portAttr *portAttr;

    portAttr = GET_PORT_ATTR(sw, port);

    *vlan = portAttr->defVlan;

    return FM_OK;

}   /* end fmGetPortDefVlanInt */




/*****************************************************************************/
/** fmGetPortDefVlanDefPriorityInt
 * \ingroup intPlatformCommon
 *
 * \desc            Returns the port default vlan and priority.
 *
 * \note            This is an optimized version for packet processing without
 *                  any locking. The input parameters should already been
 *                  validated before calling this function.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the logical port to get the default vlan and priority.
 *
 * \param[out]      vlan points to caller allocated storage where default vlan
 *                  is stored.
 *
 * \param[out]      priority points to caller allocated storage where default priority
 *                  is stored.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmGetPortDefVlanDefPriorityInt(fm_int     sw,
                                         fm_int     port,
                                         fm_uint16 *vlan,
                                         fm_byte *  priority)
{
    fm_portAttr *portAttr;

    portAttr = GET_PORT_ATTR(sw, port);

    *vlan = portAttr->defVlan;
    *priority = portAttr->defVlanPri;

    return FM_OK;

}   /* end fmGetPortDefVlanDefPriorityInt */




/*****************************************************************************/
/** fmGetPortMaxFrameSizeInt
 * \ingroup intPlatformCommon
 *
 * \desc            Returns the port max frame size
 *
 * \note            This is an optimized version for packet processing without
 *                  any locking. The input parameters should already been
 *                  validated before calling this function.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the logical port to get the max frame size.
 *
 * \param[out]      maxSize points to caller allocated storage where max size
 *                  is stored.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmGetPortMaxFrameSizeInt(fm_int  sw,
                                   fm_int  port,
                                   fm_int *maxSize)
{
    fm_portAttr *portAttr;

    portAttr  = GET_PORT_ATTR(sw, port);
    *maxSize  = portAttr->maxFrameSize;

    return FM_OK;

}   /* end fmGetPortMaxFrameSizeInt */




/*****************************************************************************/
/** fmGetVlanPortStateInt
 * \ingroup intPlatformCommon
 *
 * \desc            Retrieve the spanning tree forwarding state of a port in a
 *                  VLAN.
 *
 * \note            This is an optimized version for packet processing without
 *                  any locking. The input parameters should already been
 *                  validated before calling this function.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       vlanID is the VLAN number on which to operate.
 *
 * \param[in]       port is the number of the port on which to operate. May
 *                  not be the CPU interface port.
 *
 * \param[out]      state points to caller-allocated storage where this
 *                  function should place the spanning tree state
 *                  (see 'Spanning Tree States').
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmGetVlanPortStateInt(fm_int sw, 
                                 fm_uint16 vlanID, 
                                 fm_int port, 
                                 fm_int *state)
{
    fm_switch *switchPtr;

    switchPtr = GET_SWITCH_PTR(sw);
    
    return switchPtr->GetVlanPortState(sw, vlanID, port, state);

}   /* end fmGetVlanPortStateInt */




/*****************************************************************************/
/** fmGetStpModeInt
 * \ingroup intPlatformCommon
 *
 * \desc            Wrapper to call fmGetSpanningTreePortState.  Only valid in
 *                  shared mode.
 *
 * \note            This is an optimized version for packet processing without
 *                  any locking. The input parameters should already been
 *                  validated before calling this function.
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[out]      stpMode points to caller allocated storage where the 
 *                  stp mode is written.
 *
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmGetStpModeInt(fm_int sw, fm_stpMode *stpMode)
{
    fm_switch  *switchPtr;

    switchPtr = GET_SWITCH_PTR(sw);

    return switchPtr->GetSwitchAttribute(sw, FM_SPANNING_TREE_MODE, stpMode);

}   /* end fmGetStpModeInt */




/*****************************************************************************/
/** fmGenericPacketHandlingInitialize
 * \ingroup intPlatformCommon
 *
 * \desc            Performs generic packet transfer initialization.
 *
 * \param[in]       sw is the switch number to initialize.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmGenericPacketHandlingInitialize(fm_int sw)
{
    return fmGenericPacketHandlingInitializeV2(sw, FALSE);
}




/*****************************************************************************/
/** fmGenericPacketHandlingInitializeV2
 * \ingroup intPlatformCommon
 *
 * \desc            Performs generic packet transfer initialization.
 *
 * \param[in]       sw is the switch number to initialize.
 * 
 * \param[in]       hasFcs is TRUE if the packet includes the FCS field.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmGenericPacketHandlingInitializeV2(fm_int sw, fm_bool hasFcs)
{
    fm_packetHandlingState *ps = GET_PLAT_PKT_STATE(sw);
    fm_status               err;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_TX,
                 "sw = %d hasFcs = %s\n",
                 sw,
                 FM_BOOLSTRING(hasFcs));

    /* clear out all state */
    memset( ps, 0, sizeof(fm_packetHandlingState) );

    fmPacketQueueInit(&ps->txQueue, sw);
    
    /* reset state here */
    ps->recvInProgress       = FALSE;
    ps->recvBufferOffset     = 0;
    ps->currentWordsReceived = 0;
    ps->cachedEndianness     = -1; /* -1 indicates unspecified endianness */
    ps->sendUserFcs          = hasFcs;

    ps->rxDirectEnqueueing   = fmGetBoolApiAttribute(
                                 FM_AAK_API_PACKET_RX_DIRECT_ENQUEUEING,
                                 FALSE);

    /* initialize the signal sem for event availability */
    err = fmCreateSemaphore("netdevEventsAvailable",
                            FM_SEM_BINARY,
                            &ps->eventsAvailableSignal, 
                            0);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end fmGenericPacketHandlingInitializeV2 */




/*****************************************************************************/
/** fmGeneratePacketIsl
 * \ingroup intPlatformCommon
 *
 * \desc            Generate packet ISL words and suppressVlanTag flag.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       buffer is the packet buffer.
 *
 * \param[in]       info is the packet info data.
 *
 * \param[out]      entry is the packet entry containing the ISL words and
 *                  suppressVlanTag flags to be modified.
 *
 * \param[in]       cpuPort contains the logical port number for the CPU port. 
 *
 * \param[in]       stagTypeA is the SVLAN tag type A.
 *
 * \param[in]       stagTypeB is the SVLAN tag type B.
 *
 * \param[in]       switchPriority is the switch priority.
 *
 * \param[in]       trapGlort is the source glort to use if not specified in
 *                  info.
 *
 * \param[in]       suppressVlanTagAllowed is the flag to indicate whether
 *                  to suppress the vlan tag in the data is allowed.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmGeneratePacketIsl(fm_int          sw,
                              fm_buffer      *buffer,
                              fm_packetInfo  *info,
                              fm_packetEntry *entry,
                              fm_int          cpuPort,
                              fm_uint32       stagTypeA,
                              fm_uint32       stagTypeB,
                              fm_uint32       switchPriority,
                              fm_uint32       trapGlort,
                              fm_bool         suppressVlanTagAllowed)
{
    fm_uint       destGlort;
    fm_uint16     payloadVlanTag, egressVlanTag = 0;
    fm_bool       pTag;
    fm_uint16     pvid;
    fm_vlanEntry *ventry;
    fm_port *     dPort;
    fm_int        firstLAGPort;
    fm_uint32     value;
    fm_status     err;
    fm_switch *   switchPtr;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_TX,
                 "sw = %d, "
                 "info->logicalPort = %d, "
                 "info->sourcePort = %d, "
                 "info->vlanId = %d, "
                 "info->vlanPriority = %d, "
                 "info->switchPriority = %d, "
                 "info->useEgressRules = %s, "
                 "info->directSendToCpu = %s\n",
                 sw,
                 info->logicalPort,
                 info->sourcePort,
                 info->vlanId,
                 info->vlanPriority,
                 info->switchPriority,
                 FM_BOOLSTRING(info->useEgressRules),
                 FM_BOOLSTRING(info->directSendToCpu));

    entry->suppressVlanTag = 0;

    /* Set destGlort to zero and lower code must fill in
     * if needed.
     * Cannot directly set destGlort = info->logicalPort
     * because it is not the same anymore, especially
     * under stacking configuration
     */
    destGlort = 0;

    value = ntohl(buffer->data[FM_PACKET_OFFSET_ETHERTYPE]);

    payloadVlanTag = (value >> 16) & 0xffff;

    /* construct and send the first word of the ISL tag */
    if (!info->useEgressRules)
    {
        if (payloadVlanTag == 0x8100 ||
            payloadVlanTag == stagTypeA ||
            payloadVlanTag == stagTypeB)
        {
            egressVlanTag = value & 0xffff;

            if (suppressVlanTagAllowed)
            {
                /***********************************************
                 * We need to skip sending the vlan tag in the
                 * payload since the switch will add a VLAN tag
                 * using the info from the ISL tag. BALI only.
                 ***********************************************/
                entry->suppressVlanTag = 1;
            }
        }
        else
        {
            /* the frame is untagged */
            egressVlanTag = 0;
        }

    }
    else if (info->vlanId == 0)
    {
        FM_LOG_FATAL(FM_LOG_CAT_EVENT_PKT_TX,
                     "the user application needs to provide a "
                     "vlanId in packet info structure\n");
    }
    else
    {
        if ( (payloadVlanTag == 0x8100) ||
             (payloadVlanTag == stagTypeA) ||
             (payloadVlanTag == stagTypeB) )
        {
            /***********************************************
             * For tagged frame use the vlan info. carried
             * in the frame. Do not use packet->info.vlanId
             * and packet->info.vlanPriority.
             ***********************************************/
            egressVlanTag = value & 0xffff;

            if (suppressVlanTagAllowed)
            {
                /***********************************************
                 * We need to skip sending the vlan tag in the
                 * payload since the switch will add a VLAN tag
                 * using the info from the ISL tag. BLAI only. 
                 ***********************************************/
                entry->suppressVlanTag = 1;
            }
        }
        else
        {
            egressVlanTag  = info->vlanId;
            egressVlanTag |= (info->vlanPriority << 13);
        }
    }

    value = egressVlanTag & 0xffff;

    /**********************************************************
     * In the normal mode pTag == TRUE will skip the next block.
     * As a result frames will always have a VTYPE != 0, and a 
     * corresponding vlan id of either (1) the pvid of the cpu 
     * port; or (2) the vlan id carried by the frame itself or 
     * added by the user.
     *********************************************************/
    pTag = TRUE;

    if ( (info->logicalPort != FM_LOG_PORT_USE_FTYPE_NORMAL)
        || (info->directSendToCpu) )
    {
        if (info->useEgressRules)
        {
            /***************************************************
             * In directed mode, the tagging state is first
             * determined by useEgressRules.
             **************************************************/

            /******************************************************
             * In the directed mode find out the tagging rules of the
             * egress port packet->info.logicalPort, in the vlan
             * pvid that is associated with the frame. Note pvid
             * equals the default vlan of the CPU port for the untagged
             * frame, and the vlan id carried in the tagged frame.
             **********************************************************/

            if (value == 0)
            {
                /* for an untagged frame use the pvid of the cpu port */
                err = fmGetPortDefVlanInt(sw, cpuPort, &pvid);
                FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
            }
            else
            {
                pvid = value & 0xfff;
            }

            /* check if the egress port is tagging or not */
            ventry = GET_VLAN_PTR(sw, pvid);
            dPort = GET_PORT_PTR(sw, info->logicalPort);

            if (dPort->portType == FM_PORT_TYPE_LAG)
            {
                err = fmGetLAGPortFirstExt(sw,
                                           info->logicalPort,
                                           &firstLAGPort);
                FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

                err = fmGetVlanTag(sw,
                                   FM_VLAN_SELECT_VLAN1,
                                   ventry,
                                   firstLAGPort,
                                   &pTag);
                FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

            }
            else if (dPort->portType == FM_PORT_TYPE_REMOTE)
            {
                /* FIXME: How to get this info */
                pTag = TRUE;
            }
            else
            {
                err = fmGetVlanTag(sw,
                                   FM_VLAN_SELECT_VLAN1,
                                   ventry,
                                   info->logicalPort,
                                   &pTag);
                FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

            }
        }
        else
        {
            /***************************************************
             * Otherwise use the payload to determine whether
             * or not to tag.
             **************************************************/
            pTag = (value != 0);
        }
    }

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY(err, 
                       switchPtr->SetIslVType, 
                       sw, 
                       cpuPort,
                       stagTypeA, 
                       stagTypeB, 
                       payloadVlanTag, 
                       pTag, 
                       &value);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    /**********************************************************
     * If the logicalPort is 0, and we are not directly sending
     * to the CPU, we use normal delivery so that
     * the switch will determine the destination of the packet.
     *********************************************************/

    if ( (info->logicalPort == FM_LOG_PORT_USE_FTYPE_NORMAL) &&
         (!info->directSendToCpu) )
    {
        value |= (FM_FTYPE_NORMAL << 30);
    }
    else
    {
        value |= (FM_FTYPE_SPECIAL_DELIVERY << 30);

         err = fmGetLogicalPortGlort(sw,
                                     info->logicalPort,
                                     &destGlort);            
        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
    }

    /* set the switch priority */
    if (switchPriority != FM_USE_VLAN_PRIORITY)
    {
        value |= ( (switchPriority & 0xf) << 24 );
    }
    else if (info->switchPriority != FM_USE_VLAN_PRIORITY)
    {
        value |= ( (info->switchPriority & 0xf) << 24 );
    }
    else
    {
        value |= ( ( (egressVlanTag >> 12) & 0xf ) << 24 );
    }

    entry->islTag.f64.tag[0] = value;

    /* construct and send the second word of the ISL tag */

    /* If requested, force the source glort to zero */
    if (info->zeroSourceGlort)
    {
        value = 0;
    }
    /* else use the trap glort (cpu glort) as the source glort
     *  for send? */
    else if (info->sourcePort == 0)
    {
        /* User does not provide the source glort. Use default.*/
        value = trapGlort;
    }
    else
    {
        err = fmGetLogicalPortGlort(sw, info->sourcePort, &value);
        if (err != FM_OK)
        {
            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                         "fmGeneratePacketIsl: Invalid sourcePort %d\n",
                         info->sourcePort);
            FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
        }
    }

    value = value << 16;

    /* retrieve the destination glort (logical port) */
    value |= (destGlort & 0xffff);

    entry->islTag.f64.tag[1] = value;

    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                 "ISL Tag: [0] = %X, [1] = %X\n",
                 entry->islTag.f64.tag[0],
                 entry->islTag.f64.tag[1]);

    return FM_OK;

}   /* end fmGeneratePacketIsl */



/*****************************************************************************/
/** fmGenericSendPacketISL
 * \ingroup intPlatformCommon
 *
 * \desc            Called to add a packet to the TX packet queue.
 *
 * \param[in]       sw is the switch on which to send the packet.
 *
 * \param[in]       islTagList points to an array of islTag to send out
 *                  along with packet
 *
 * \param[in]       numPorts is the number of elements in islTagList.
 *
 * \param[in]       packet points to the packet buffer's first ''fm_buffer''
 *                  structure in a chain of one or more buffers.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmGenericSendPacketISL(fm_int     sw,
                                 fm_islTag *islTagList,
                                 fm_int     numPorts,
                                 fm_buffer *packet)
{
    fm_packetQueue         *txQueue;
    fm_int                  packetLength;
    fm_int                  port;
    fm_int                  cpuMaxFrameSize;
    fm_int                  oldPushIndex;
    fm_int                  cpuPort;
    fm_int                  masterSw; /* For support FIBM slave switch */
    fm_status               err = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_TX,
                 "sw = %d, "
                 "islTag = %p, "
                 "numPorts = %d, "
                 "packet->index = 0x%x\n",
                 sw,
                 (void *) islTagList,
                 numPorts,
                 packet->index);

    masterSw  = fmFibmSlaveGetMasterSwitch(sw);

    if (masterSw >= 0)
    {
        VALIDATE_AND_PROTECT_SWITCH_NO_RETURN(err, masterSw);
        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
        
        err = fmGetCpuPortInt(masterSw, &cpuPort);
        
        if (err == FM_OK)
        {
            err = fmGetPortMaxFrameSizeInt(masterSw,
                                           cpuPort,
                                           &cpuMaxFrameSize);
        }
        
        UNPROTECT_SWITCH(masterSw);
        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
    }
    else
    {
        /* This is for FIBM using the NIC. */
        cpuMaxFrameSize = 10000; /* Just allow to pass */
        
        /* Standalone use itself as master switch */
        masterSw = sw;
    }

    packetLength = fmComputeTotalPacketLength(packet);

    if (packetLength <= 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, FM_ERR_INVALID_ARGUMENT);
    }

    if (packetLength > cpuMaxFrameSize - 4)
    {
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, FM_ERR_FRAME_TOO_LARGE);
    }

    txQueue = &GET_PLAT_PKT_STATE(masterSw)->txQueue;
     
    fmPacketQueueLock(txQueue);

    oldPushIndex = txQueue->pushIndex;

    for (port = 0 ; port < numPorts ; port++)
    {
        fm_bool freePacketBuffer;

        if (port < numPorts - 1)
        {
            freePacketBuffer = FALSE;
        }
        else
        {
            /* only free the packet buffer if we have sent to all target
             *  ports in the vlan */
            freePacketBuffer = TRUE;
        }
        err = fmPacketQueueEnqueue(txQueue,
                                   packet,
                                   packetLength,
                                   &islTagList[port],
                                   FALSE,
                                   freePacketBuffer);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
    }

    fmPacketQueueUnlock(txQueue);

    if (err == FM_OK)
    {
        if (!fmRootPlatform->dmaEnabled)
        {
            fm_switch *switchPtr;
            switchPtr = GET_SWITCH_PTR(sw);

            /**************************************************
             * We must take a lock before writing
             * intrSendPackets because the lock is used
             * by the API to ensure an atomic read-modify-write
             * to intrSendPackets.
             *
             * The platform lock is used instead of state lock
             * because on FIBM platforms, there is an access
             * to intrSendPackets that must be protected before
             * the switch's locks are even created. 
             **************************************************/
            
            FM_TAKE_PKT_INT_LOCK(sw);
            switchPtr->intrSendPackets = TRUE;
            FM_DROP_PKT_INT_LOCK(sw);
        }

        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                     "fmGenericSendPacketISL: triggering interrupt handler\n");

        /* Handle the following error case cleanly! */
        err = fmPlatformTriggerInterrupt(masterSw, FM_INTERRUPT_SOURCE_API);

        if (err != FM_OK)
        {
            /***************************************************************
             * We are here because sem_post() fails. In this case we can not
             * cleanly unwind the TX queue to recover, so we simply log a
             * fatal error, and return FM_OK.
             **************************************************************/
            FM_LOG_FATAL(FM_LOG_CAT_EVENT_PKT_TX,
                         "fmGenericSendPacketISL: "
                         "fmPlatformTriggerInterrupt returned error");

            err = FM_OK;
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);

ABORT:
    txQueue->pushIndex = oldPushIndex;
    fmPacketQueueUnlock(txQueue);
    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);

}   /* end fmGenericSendPacketISL */




/*****************************************************************************/
/** fmGenericSendPacketDirected
 * \ingroup intPlatformCommon
 *
 * \desc            Called to add a packet to the TX packet queue.
 *
 * \param[in]       sw is the switch on which to send the packet.
 *
 * \param[in]       portList points to an array of logical port numbers the
 *                  switch is to send the packet.
 *
 * \param[in]       numPorts is the number of elements in portList.
 *
 * \param[in]       packet points to the packet buffer's first ''fm_buffer''
 *                  structure in a chain of one or more buffers.
 * 
 * \param[in]       fcsValue is the value to be sent in the FCS field.
 *
 * \param[in]       cpuPort contains the logical port number for the CPU port. 
 *
 * \param[in]       stagTypeA is the SVLAN tag type A.
 *
 * \param[in]       stagTypeB is the SVLAN tag type B.
 *
 * \param[in]       switchPriority is the switch priority.
 *
 * \param[in]       trapGlort is the source glort to use if not specified in
 *                  info.
 *
 * \param[in]       suppressVlanTagAllowed is the flag to indicate whether
 *                  to suppress the vlan tag in the data is allowed.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmGenericSendPacketDirected(fm_int     sw,
                                      fm_int *   portList,
                                      fm_int     numPorts,
                                      fm_buffer *packet,
                                      fm_uint32  fcsValue,
                                      fm_int     cpuPort,
                                      fm_uint32  stagTypeA,
                                      fm_uint32  stagTypeB,
                                      fm_uint32  switchPriority,
                                      fm_uint32  trapGlort,
                                      fm_bool    suppressVlanTagAllowed)
{
    fm_switch *             switchPtr;
    fm_packetQueue         *txQueue;
    fm_packetEntry *        entry;
    fm_int                  packetLength;
    fm_int                  listIndex;
    fm_int                  port;
    fm_int                  state;
    fm_bool                 packetSent = FALSE;
    fm_int                  stpState;
    fm_macaddr              destMacAddress;
    fm_uint32               frameTag;
    fm_uint32               frameTagType;
    fm_uint16               outVlanId;
    fm_int                  cpuMaxFrameSize;
    fm_bool                 allowDirectSendToCpu = TRUE;
    fm_stpMode              spanningTreeMode;
    fm_int                  firstLAGPort;
    fm_bool                 firstLAGValid;
    fm_port *               dPort;
    fm_int                  oldPushIndex;
    fm_int                  masterSw; /* For support FIBM slave switch */

    fm_status               err = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_TX,
                 "sw = %d, "
                 "portList = %p, "
                 "numPorts = %d, "
                 "packet->index = 0x%x\n",
                 sw,
                 (void *) portList,
                 numPorts,
                 packet->index);

    switchPtr = GET_SWITCH_PTR(sw);
    txQueue   = &GET_PLAT_PKT_STATE(sw)->txQueue;
    outVlanId = 0;

    /* Verify the port numbers. */
    for (listIndex = 0 ; listIndex < numPorts ; listIndex++)
    {
        port = portList[listIndex];

        /* must be a valid cardinal, LAG, or remote port */
        if ( !fmIsValidPort(sw, port, ALLOW_CPU | ALLOW_LAG | ALLOW_REMOTE) )
        {
            FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, FM_ERR_INVALID_PORT);
        }
    }

    packetLength = fmComputeTotalPacketLength(packet);

    if (packetLength <= 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, FM_ERR_INVALID_ARGUMENT);
    }

    err = fmGetPortMaxFrameSizeInt(sw,
                                   cpuPort,
                                   &cpuMaxFrameSize);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    if (packetLength > cpuMaxFrameSize - 4)
    {
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, FM_ERR_FRAME_TOO_LARGE);
    }

    /****************************************************************
     * In the svl stp mode, if the packet is not a LACP or 802.1x
     * frame, then we check if the destination port is in the
     * blocking/disabled state on the vlan associated with the packet.
     ****************************************************************/

    err = fmGetStpModeInt(sw, &spanningTreeMode);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    destMacAddress = fmGetPacketDestAddr(sw, packet);

    /* Find out the outgoing vlan the packet will be associated with */
    if ( (spanningTreeMode == FM_SPANNING_TREE_SHARED)
        && (destMacAddress != FM_LACP_DEST_ADDRESS)
        && (destMacAddress != FM_DOT1X_DEST_ADDRESS) )
    {
        frameTag = ntohl(packet->data[FM_PACKET_OFFSET_ETHERTYPE]);

        frameTagType = (frameTag >> 16 ) & 0xffff;

        /* we check if the frame has a vlan tag */
        if ( (frameTagType != FM_VLAN_TAG_TYPE_8100 ) &&
             (frameTagType != stagTypeA) &&
             (frameTagType != stagTypeB) )
        {
            /* frame is not tagged */
            err = fmGetPortDefVlanInt(sw,
                                       cpuPort,
                                       &outVlanId);
            FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
        }
        else
        {
            /* frame is tagged */
            outVlanId = frameTag & 0xfff;
        }
    }

    if (fmRootApi->isSwitchFibmSlave[sw])
    {
        masterSw  = fmFibmSlaveGetMasterSwitch(sw);
        if (masterSw < 0)
        {
            /* In standalone NIC mode, the 
             * master is also the same as the slave.
             */
            masterSw = sw;
        }
        else
        {
            /* We are sending packet via master switch
             * These is no platform lock on slave switch in 
             * master slave mode.
             */
            switchPtr = GET_SWITCH_PTR(masterSw);
            txQueue   = &GET_PLAT_PKT_STATE(masterSw)->txQueue;
        }
    }
    else
    {
        masterSw = sw;
    }

    fmPacketQueueLock(txQueue);

    /***********************************************************
     * oldSendPushIndex records the current push index in the TX
     * queue. We use it to keep tab on where we started upon
     * entering this function, in case of the need for roll back
     * on the push index when (1) the tx queue is full; or (2)
     * the function calls returns an error which we return to the
     * user application, after having enqued some entries in the
     * tx queue.
     **********************************************************/
    oldPushIndex = txQueue->pushIndex;

    for (listIndex = 0 ; listIndex < numPorts ; listIndex++)
    {
        fm_packetInfo tempInfo;

        port  = portList[listIndex];
        dPort = GET_PORT_PTR(sw, port);

        firstLAGValid = FALSE;

        entry = &txQueue->packetQueueList[txQueue->pushIndex];

        /* Build the packetEntry */
        entry->packet = packet;
        entry->length = packetLength;
        entry->fcsVal = fcsValue;

        memset( &tempInfo, 0, sizeof(tempInfo) );
        tempInfo.logicalPort = port;

        if (tempInfo.sourcePort == 0)
        {
            if (switchPtr->defaultSourcePort < 0)
            {
                tempInfo.zeroSourceGlort = TRUE;
            }
            else
            {
                tempInfo.sourcePort = switchPtr->defaultSourcePort;
            }
        }

        /* we will call fmGeneratePacketIsl since some fields
         * in tempInfo will be changed
         */

        /**********************************************************
         * If any of ports in the portList
         * is UP and not in Blocking or Disabled state in outVlanId
         * we will flag packetSent as TRUE, so that the packet
         * buffer will not be freed
         *********************************************************/
        if (port != cpuPort)
        {
            /******************************************************
             * Check if the portList[port] is a LAG. If so
             * we proceed if the LAG has at least one member.
             * Note we do not check the state of the member ports.
             * In case a frame is hashed to a down member of the lag
             * the frame will be dropped by the switch.
             *****************************************************/
            if (dPort->portType == FM_PORT_TYPE_LAG)
            {
                firstLAGValid = TRUE;
                err           = fmGetLAGPortFirstExt(sw, 
                                                     port, 
                                                     &firstLAGPort);
                if (err != FM_OK)
                {
                    /* The LAG is empty */
                    err = FM_ERR_INVALID_PORT_STATE;
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
                }

                state = FM_PORT_STATE_UP;
            }
            else
            {
                firstLAGPort = port;

                if (fmIsPortLinkUp(sw, port))
                {
                    state = FM_PORT_STATE_UP;
                }
                else
                {
                    state = FM_PORT_STATE_DOWN;
                }
            }
        }
        else
        {
            /* Special handling of sending to the CPU port in the directed mode */
            allowDirectSendToCpu = fmGetBoolApiAttribute(
                FM_AAK_API_DIRECT_SEND_TO_CPU,
                FM_AAD_API_DIRECT_SEND_TO_CPU);

            if (allowDirectSendToCpu)
            {
                tempInfo.directSendToCpu = TRUE;
                state                    = FM_PORT_STATE_UP;
            }
            else
            {
                /************************************************************
                 * We set the state to FM_PORT_STATE_DOWN so packet will not
                 * be sent to the CPU port.
                 ***********************************************************/
                state = FM_PORT_STATE_DOWN;
            }
        }

        if (state == FM_PORT_STATE_UP)
        {
            /***********************************************
             *  We only send to ports that are up
             ***********************************************/

            if ( (spanningTreeMode == FM_SPANNING_TREE_SHARED)
                && (destMacAddress != FM_LACP_DEST_ADDRESS)
                && (destMacAddress != FM_DOT1X_DEST_ADDRESS) )
            {
                /***********************************************
                 *  In the svl STP mode, we only send non-LACP
                 *  and non-Dot1X frames to ports not in BLOCKING
                 *  or DISABLED stp state.
                 ***********************************************/

                if (dPort->portType == FM_PORT_TYPE_LAG)
                {
                    if (firstLAGValid)
                    {
                        if ( switchPtr->portTable[firstLAGPort]->portType == FM_PORT_TYPE_REMOTE )
                        {
                            /* Assume remote ports are always forwarding */
                            stpState = FM_STP_STATE_FORWARDING;
                        }
                        else
                        {
                            err = fmGetVlanPortStateInt(sw,
                                                        outVlanId,
                                                        firstLAGPort,
                                                        &stpState);
                        }
                    }
                    else
                    {
                        FM_LOG_ERROR(FM_LOG_CAT_EVENT_PKT_TX,
                                     "firstLAGPort is "
                                     "uninitialized-- this isn't supposed "
                                     "to happen (bug 11402)\n");
                        err = FM_FAIL;
                    }

                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
                }
                else if (dPort->portType == FM_PORT_TYPE_REMOTE)
                {
                    /* Assume remote ports are always forwarding */
                    stpState = FM_STP_STATE_FORWARDING;
                }
                else
                {
                    err = fmGetVlanPortStateInt(sw,
                                                outVlanId,
                                                port,
                                                &stpState);
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
                }

                if ( (stpState != FM_STP_STATE_BLOCKING)
                    && (stpState != FM_STP_STATE_DISABLED) )
                {
                    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                                 "fmGenericSendPacketDirected: packet queued "
                                 "in slot %d, length %d bytes, port %d\n",
                                 txQueue->pushIndex,
                                 entry->length,
                                 port);

                    err = fmPacketQueueUpdate(txQueue);
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

                    packetSent = TRUE;
                }
                else
                {
                    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                                 "fmGenericSendPacketDirected:"
                                 "stpState = %d, outVlanId = %d, port = %d\n",
                                 stpState,
                                 outVlanId, port);

                }
            }
            else
            {
                FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                             "fmGenericSendPacket: packet queued "
                             "in slot %d, length %d bytes, port %d\n",
                             txQueue->pushIndex,
                             entry->length, port);

                err = fmPacketQueueUpdate(txQueue);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

                packetSent = TRUE;

            }
        }

        err = fmGeneratePacketIsl(sw, packet, &tempInfo, entry, 
                                  cpuPort, stagTypeA, stagTypeB,
                                  switchPriority, trapGlort,
                                  suppressVlanTagAllowed);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

        if (listIndex < numPorts - 1)
        {
            entry->freePacketBuffer = FALSE;
        }
        else
        {
            /* only free the packet buffer if we have sent to all target
            *  ports in the vlan */
            entry->freePacketBuffer = TRUE;
        }
    }

    if (!packetSent)
    {
        /********************************************************
         * If none of the ports in the portList is in the
         * proper state, return an error to the user application.
         ********************************************************/
        err = FM_ERR_INVALID_PORT_STATE;
    }

    fmPacketQueueUnlock(txQueue);

    if (err == FM_OK)
    {
        if (!fmRootPlatform->dmaEnabled)
        {
            /**************************************************
             * We must take a lock before writing
             * intrSendPackets because the lock is used
             * by the API to ensure an atomic read-modify-write
             * to intrSendPackets.
             *
             * The platform lock is used instead of state lock
             * because on FIBM platforms, there is an access
             * to intrSendPackets that must be protected before
             * the switch's locks are even created. 
             **************************************************/
            
            FM_TAKE_PKT_INT_LOCK(sw);
            switchPtr->intrSendPackets = TRUE;
            FM_DROP_PKT_INT_LOCK(sw);
        }

        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                     "fmGenericSendPacketDirected: "
                     "triggering interrupt handler\n");

        err = fmPlatformTriggerInterrupt(masterSw, FM_INTERRUPT_SOURCE_API);

        /* Handle the following error case cleanly! */
        if (err != FM_OK)
        {
            /***************************************************************
             * We are here because sem_post() fails. In this case we can not
             * cleanly unwind the TX queue to recover, so we simply log a
             * fatal error, and return FM_OK.
             **************************************************************/
            FM_LOG_FATAL(FM_LOG_CAT_EVENT_PKT_TX, 
                         "fmGenericSendPacket: "
                         "fmPlatformTriggerInterrupt returned error");

            err = FM_OK;
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);

ABORT:
    txQueue->pushIndex = oldPushIndex;
    fmPacketQueueUnlock(txQueue);
    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);

}   /* end fmGenericSendPacketDirected */




/*****************************************************************************/
/** fmGenericSendPacketSwitched
 * \ingroup intPlatformCommon
 *
 * \desc            Called to add a packet to the TX packet queue.
 *
 * \param[in]       sw is the switch on which to send the packet.
 *
 * \param[in]       packet points to the packet buffer's first ''fm_buffer''
 *                  structure in a chain of one or more buffers.
 *
 * \param[in]       cpuPort contains the logical port number for the CPU port. 
 *
 * \param[in]       stagTypeA is the SVLAN tag type A.
 *
 * \param[in]       stagTypeB is the SVLAN tag type B.
 *
 * \param[in]       switchPriority is the switch priority.
 *
 * \param[in]       trapGlort is the source glort to use if not specified in
 *                  info.
 *
 * \param[in]       suppressVlanTagAllowed is the flag to indicate whether
 *                  to suppress the vlan tag in the data is allowed.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmGenericSendPacketSwitched(fm_int     sw,
                                      fm_buffer *packet,
                                      fm_int     cpuPort,
                                      fm_uint32  stagTypeA,
                                      fm_uint32  stagTypeB,
                                      fm_uint32  switchPriority,
                                      fm_uint32  trapGlort,
                                      fm_bool    suppressVlanTagAllowed)
{
    fm_switch *             switchPtr = GET_SWITCH_PTR(sw);
    fm_packetQueue         *txQueue;
    fm_packetEntry *        entry;
    fm_int                  packetLength;
    fm_status               err = FM_OK;
    fm_int                  cpuMaxFrameSize;
    fm_packetInfo           tempInfo;
    fm_int                  masterSw;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_TX,
                 "sw = %d, "
                 "packet->index = 0x%x\n",
                 sw,
                 packet->index);

    packetLength = fmComputeTotalPacketLength(packet);
    txQueue   = &GET_PLAT_PKT_STATE(sw)->txQueue;

    if (packetLength <= 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, FM_ERR_INVALID_ARGUMENT);
    }

    err = fmGetPortMaxFrameSizeInt(sw,
                                   cpuPort,
                                   &cpuMaxFrameSize);

    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    if (packetLength > cpuMaxFrameSize - 4)
    {
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, FM_ERR_FRAME_TOO_LARGE);
    }

    if (fmRootApi->isSwitchFibmSlave[sw])
    {
        masterSw  = fmFibmSlaveGetMasterSwitch(sw);
        if (masterSw < 0)
        {
            /* In standalone NIC mode, the 
             * master is also the same as the slave.
             */
            masterSw = sw;
        }
        else
        {
            /* We are sending packet via master switch
             * These is no platform lock on slave switch in 
             * master slave mode.
             */
            switchPtr = GET_SWITCH_PTR(masterSw);
            txQueue   = &GET_PLAT_PKT_STATE(masterSw)->txQueue;
        }
    }
    else
    {
        masterSw = sw;
    }

    fmPacketQueueLock(txQueue); 

    entry = &txQueue->packetQueueList[txQueue->pushIndex];

    /* Build the packetEntry */
    entry->packet           = packet;
    entry->length           = packetLength;
    entry->fcsVal           = FM_USE_DEFAULT_FCS;
    entry->freePacketBuffer = TRUE;

    memset(&tempInfo,0,sizeof(fm_packetInfo));
    tempInfo.logicalPort     = FM_LOG_PORT_USE_FTYPE_NORMAL;
    tempInfo.sourcePort      = switchPtr->defaultSourcePort;
    tempInfo.directSendToCpu = FALSE;

    err = fmGeneratePacketIsl(sw, packet, &tempInfo, entry, 
                              cpuPort, stagTypeA, stagTypeB,
                              switchPriority, trapGlort,
                              suppressVlanTagAllowed);
    if (err != FM_OK)
    {
        fmPacketQueueUnlock(txQueue); 
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);
    }

    
    err = fmPacketQueueUpdate(txQueue);

    if (err != FM_OK)
    {
        /* Check for queue FULL */
        fmPacketQueueUnlock(txQueue);
    
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);
    }

    fmPacketQueueUnlock(txQueue);

    if (!fmRootPlatform->dmaEnabled)
    {
        /**************************************************
         * We must take a lock before writing
         * intrSendPackets because the lock is used
         * by the API to ensure an atomic read-modify-write
         * to intrSendPackets.
         *
         * The platform lock is used instead of state lock
         * because on FIBM platforms, there is an access
         * to intrSendPackets that must be protected before
         * the switch's locks are even created. 
         **************************************************/
        
        FM_TAKE_PKT_INT_LOCK(sw);
        switchPtr->intrSendPackets = TRUE;
        FM_DROP_PKT_INT_LOCK(sw);
    }

    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                 "fmGenericSendPacketSwitched: "
                 "triggering interrupt handler\n");

    err = fmPlatformTriggerInterrupt(masterSw, FM_INTERRUPT_SOURCE_API);

    /* Handle the following error case cleanly! */
    if (err != FM_OK)
    {
        /***************************************************************
         * We are here because sem_post() fails. In this case we can not
         * cleanly unwind the TX queue to recover, so we simply log a
         * fatal error, and return FM_OK.
         **************************************************************/
        FM_LOG_FATAL(FM_LOG_CAT_EVENT_PKT_TX,
                     "fmGenericSendPacket: "
                     "fmPlatformTriggerInterrupt returned error");

        err = FM_OK;
    }

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);

}   /* end fmGenericSendPacketSwitched */




/*****************************************************************************/
/** fmGenericSendPacket
 * \ingroup intPlatformCommon
 *
 * \desc            Called to add a packet to the TX packet queue.
 *
 * \param[in]       sw is the switch on which to send the packet.
 *
 * \param[in]       info is a pointer to associated information about
 *                  the packet including where it is going.
 *
 * \param[in]       packet is a pointer to a chain of fm_buffer structures
 *                  containing the payload.
 *
 * \param[in]       cpuPort contains the logical port number for the CPU port. 
 *
 * \param[in]       stagTypeA is the SVLAN tag type A.
 *
 * \param[in]       stagTypeB is the SVLAN tag type B.
 *
 * \param[in]       switchPriority is the switch priority.
 *
 * \param[in]       trapGlort is the source glort to use if not specified in
 *                  info.
 *
 * \param[in]       suppressVlanTagAllowed is the flag to indicate whether
 *                  to suppress the vlan tag in the data is allowed.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmGenericSendPacket(fm_int         sw,
                              fm_packetInfo *info,
                              fm_buffer *    packet,
                              fm_int         cpuPort,
                              fm_uint32      stagTypeA,
                              fm_uint32      stagTypeB,
                              fm_uint32      switchPriority,
                              fm_uint32      trapGlort,
                              fm_bool        suppressVlanTagAllowed)
{
    fm_switch *             switchPtr;
    fm_packetQueue *        txQueue;
    fm_int                  masterSw;
    fm_packetEntry *        entry;
    fm_int                  firstPort;
    fm_int                  nextPort;
    fm_int                  state;
    fm_bool                 packetSent = FALSE;
    fm_int                  stpState;
    fm_uint64               destMacAddress;
    fm_uint32               frameTag;
    fm_uint32               frameTagType;
    fm_uint16               outVlanId;
    fm_port *               dPort;
    fm_int                  firstLAGPort;
    fm_bool                 allowDirectSendToCpu = TRUE;
    fm_stpMode              spanningTreeMode;
    fm_int                  oldPushIndex;
    fm_packetInfo           tempInfo;
    fm_int                  packetLength;
    fm_int                  cpuMaxFrameSize;
    fm_status               err;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_TX,
                 "sw = %d, "
                 "info->destMask = 0x%08x, "
                 "info->logicalPort = %d, "
                 "info->sourcePort = %d, "
                 "info->vlanId = %d, "
                 "info->vlanPriority = %d, "
                 "info->switchPriority = %d, "
                 "info->useEgressRules = %s, "
                 "packet->index = 0x%x\n",
                 sw,
                 info->destMask,
                 info->logicalPort,
                 info->sourcePort,
                 info->vlanId,
                 info->vlanPriority,
                 info->switchPriority,
                 FM_BOOLSTRING(info->useEgressRules),
                 packet->index);

    err          = FM_OK;
    outVlanId    = info->vlanId;
    packetLength = fmComputeTotalPacketLength(packet);
    switchPtr    = GET_SWITCH_PTR(sw);

    if (packetLength <= 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, FM_ERR_INVALID_ARGUMENT);
    }

    err = fmGetPortMaxFrameSizeInt(sw,
                                   cpuPort,
                                   &cpuMaxFrameSize);

    if (err != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);
    }

    if (packetLength > cpuMaxFrameSize - 4)
    {
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, FM_ERR_FRAME_TOO_LARGE);
    }

    if (fmRootApi->isSwitchFibmSlave[sw])
    {
        masterSw  = fmFibmSlaveGetMasterSwitch(sw);
        if (masterSw < 0)
        {
            /* In standalone NIC mode, the 
             * master is also the same as the slave.
             */
            masterSw = sw;
        }
    }
    else
    {
        masterSw = sw;
    }

    txQueue = &GET_PLAT_PKT_STATE(masterSw)->txQueue;

    fmPacketQueueLock(txQueue);

    /***********************************************************
     * oldSendPushIndex records the current push index in the TX
     * queue. We use it to keep tab on where we started upon
     * entering this function, in case of the need for roll back
     * on the push index when the tx queue is full or if we
     * encounter an error condition after having enqued some
     * entries in the tx queue;
     **********************************************************/
    oldPushIndex = txQueue->pushIndex;

    /************************************************************
     * In the svl stp mode, if the packet is not a LACP or 802.1x
     * frame, then we check if the destination port is in the
     * blocking/disabled state on the outgoing vlan.
     ***********************************************************/

    err = fmGetStpModeInt(sw, &spanningTreeMode);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    destMacAddress = fmGetPacketDestAddr(sw, packet);

    /* we first find out the vlan the packet will be associated with */
    if ( (spanningTreeMode == FM_SPANNING_TREE_SHARED)
        && (destMacAddress != FM_LACP_DEST_ADDRESS)
        && (destMacAddress != FM_DOT1X_DEST_ADDRESS) )
    {
        frameTag = ntohl(packet->data[FM_PACKET_OFFSET_ETHERTYPE]);

        frameTagType = (frameTag >> 16 ) & 0xffff;

        /* we check if the frame has a vlan tag */
        if (frameTagType != FM_VLAN_TAG_TYPE_8100 &&
            frameTagType != stagTypeA &&
            frameTagType != stagTypeB)
        {
            /* frame is not tagged */
            if (info->useEgressRules)
            {
                outVlanId = info->vlanId;
            }
            else
            {
                err = fmGetPortDefVlanInt(sw, cpuPort, &outVlanId);

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
            }

        }
        else
        {
            /* frame is tagged */
            outVlanId = frameTag & 0xfff;
        }
    }

    if (info->logicalPort != FM_DIRECT_VLAN_SEND)
    {
        /* direct send to a single destination port, or normal send */
        if ( (info->logicalPort != FM_LOG_PORT_USE_FTYPE_NORMAL)
            || info->directSendToCpu )
        {
            /*********************************************************
             * In the case of direct sending to a physical port,
             * check the port state, only proceed if the port is up.
             * If the info->logicalPort is a LAG we proceed if the
             * LAG has at least one member.
             * Note we do not check the state of the member ports.
             * In case the frame is hashed to a down member of the lag
             * the frame will be dropped by the switch.
             *********************************************************/
            dPort = GET_PORT_PTR(sw, info->logicalPort);

            if (dPort->portType == FM_PORT_TYPE_LAG)
            {
                err = fmGetLAGPortFirstExt(sw, info->logicalPort, &firstLAGPort);

                if (err != FM_OK)
                {
                    /* The LAG is empty */
                    err = FM_ERR_INVALID_PORT_STATE;
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
                }
            }
            else
            {
                /* work-around a Klocwork issue. It gets confused by our
                 * repeated setting of dPort and assumes that the values
                 * might be different, thus leading to different portTypes.
                 * Setting firstLAGPort will at least keep it from complaining
                 * about an uninitialized variable. */
                firstLAGPort = info->logicalPort;

                /* check the destination port is a physical port */
                if (!info->directSendToCpu)
                {
                    if (! (fmIsCardinalPort(sw, info->logicalPort) ||
                           fmIsRemotePort(sw, info->logicalPort)) )
                    {
                        err = FM_ERR_INVALID_PORT;
                        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
                    }

                    if (!fmIsPortLinkUp(sw, info->logicalPort))
                    {
                        err = FM_ERR_INVALID_PORT_STATE;
                        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
                    }
                }
                else
                {
                    /* direct send to the CPU */
                    allowDirectSendToCpu = fmGetBoolApiAttribute(
                        FM_AAK_API_DIRECT_SEND_TO_CPU,
                        FM_AAD_API_DIRECT_SEND_TO_CPU);

                    if (!allowDirectSendToCpu)
                    {
                        err = FM_ERR_INVALID_PORT;
                        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
                    }
                }
            }
        }

        /************************************************************
         * Checking the blocking stp state in the direct send mode,
         *  for frames of type other than LACP and DOT1X.
         *  Note we do not check the stp state in the lookup mode,
         *  or if we are directly sending to the CPU port (always in
         *  the FM_STP_FORWARDING state)
         ***********************************************************/
        if (info->logicalPort != FM_LOG_PORT_USE_FTYPE_NORMAL)
        {
            if ( (spanningTreeMode == FM_SPANNING_TREE_SHARED)
                && (destMacAddress != FM_LACP_DEST_ADDRESS)
                && (destMacAddress != FM_DOT1X_DEST_ADDRESS) )
            {
                /******************************************************
                 *  We only send non-LACP and non-Dot1X frames
                 *  to ports not in BLOCKING or DISABLED stp state.
                 *  Check if the info->logicalPort is a LAG. If so find
                 *  the STP state of the first member port of the LAG.
                 *****************************************************/
                dPort = GET_PORT_PTR(sw, info->logicalPort);

                if (dPort->portType == FM_PORT_TYPE_LAG)
                {
                    /******************************************************
                     * Note: we have already retrieved valid 
                     * firstLAGPort from the check in the privious
                     * block.
                     ******************************************************/
                    err = fmGetVlanPortStateInt(sw,
                                                outVlanId,
                                                firstLAGPort,
                                                &stpState);

                    /************************************************
                     *  we get here only if info->vlanId is invalid.
                     *  err = FM_ERR_INVALID_VLAN;
                     ***********************************************/
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
                }
                else
                {
                    err = fmGetVlanPortStateInt(sw,
                                                outVlanId,
                                                info->logicalPort,
                                                &stpState);

                    /************************************************
                     *  we get here only if info->vlanId is invalid.
                     *  err = FM_ERR_INVALID_VLAN;
                     ***********************************************/
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
                }

                if ( (stpState == FM_STP_STATE_BLOCKING) ||
                     (stpState == FM_STP_STATE_DISABLED) )
                {
                    err = FM_ERR_INVALID_PORT_STATE;
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
                }
            }
        }

        entry = &txQueue->packetQueueList[txQueue->pushIndex];

        /* copy over packet info */
        FM_MEMCPY_S( &tempInfo, sizeof(tempInfo), info, sizeof(*info) );

        if (tempInfo.sourcePort == 0)
        {
            if (switchPtr->defaultSourcePort < 0)
            {
                tempInfo.zeroSourceGlort = TRUE;
            }
            else
            {
                tempInfo.sourcePort = switchPtr->defaultSourcePort;
            }
        }

        entry->packet = packet;
        entry->length = packetLength;
        entry->fcsVal = FM_USE_DEFAULT_FCS;
        entry->freePacketBuffer = TRUE;

        /* The rest of entry fields are generated here */
        err = fmGeneratePacketIsl(sw, packet, &tempInfo, entry, 
                                  cpuPort, stagTypeA, stagTypeB,
                                  switchPriority, trapGlort,
                                  suppressVlanTagAllowed);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                     "fmGenericSendPacket: packet queued "
                     "in slot %d, length %d bytes\n",
                     txQueue->pushIndex,
                     entry->length);

        err = fmPacketQueueUpdate(txQueue);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    }
    else
    {
        /* direct send to an entire vlan */
        err = fmGetVlanPortFirst(sw, info->directSendVlanId, &firstPort);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

        while (firstPort != -1)
        {
            entry = &txQueue->packetQueueList[txQueue->pushIndex];

            /* copy over packet info to tempInfo so later can build isl */
            FM_MEMCPY_S( &tempInfo, sizeof(tempInfo), info, sizeof(*info) );
            tempInfo.logicalPort = firstPort;

            if (tempInfo.sourcePort == 0)
            {
                if (switchPtr->defaultSourcePort < 0)
                {
                    tempInfo.zeroSourceGlort = TRUE;
                }
                else
                {
                    tempInfo.sourcePort = switchPtr->defaultSourcePort;
                }
            }

            entry->packet = packet;
            entry->length = packetLength;
            entry->fcsVal = FM_USE_DEFAULT_FCS;

            /**********************************************************
             * If any of ports in the vlan info->directSendVlanId
             * is UP and not in Blocking or Disabled state in outVlanId
             * we will flag packetSent as TRUE, so that the packet
             * buffer will not be freed
             *********************************************************/
            if (firstPort != cpuPort)
            {
                if (fmIsPortLinkUp(sw, firstPort))
                {
                    state = FM_PORT_STATE_UP;
                }
                else
                {
                    state = FM_PORT_STATE_DOWN;
                }
                
            }
            else
            {
                /* Special handling of the CPU port as the member of the vlan. */
                allowDirectSendToCpu = fmGetBoolApiAttribute(
                    FM_AAK_API_DIRECT_SEND_TO_CPU,
                    FM_AAD_API_DIRECT_SEND_TO_CPU);

                if (allowDirectSendToCpu)
                {
                    tempInfo.directSendToCpu = TRUE;
                    state                    = FM_PORT_STATE_UP;
                }
                else
                {
                    /***********************************************************
                     * We set the state to FM_PORT_STATE_DOWN so packet will not
                     * be sent to the CPU port.
                     **********************************************************/
                    state = FM_PORT_STATE_DOWN;
                }
            }

            if (state == FM_PORT_STATE_UP)
            {
                /***********************************************
                 *  We only send to ports that are up
                 ***********************************************/

                if ( (spanningTreeMode == FM_SPANNING_TREE_SHARED)
                    && (destMacAddress != FM_LACP_DEST_ADDRESS)
                    && (destMacAddress != FM_DOT1X_DEST_ADDRESS) )
                {
                    /***********************************************
                     *  In the svl STP mode, we only send non-LACP
                     *  and non-Dot1X frames to ports not in BLOCKING
                     *  or DISABLED stp state.
                     ***********************************************/
                    err = fmGetVlanPortStateInt(sw,
                                                outVlanId,
                                                firstPort,
                                                &stpState);

                    if (err != FM_OK)
                    {
                        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
                    }

                    if ( (stpState != FM_STP_STATE_BLOCKING) &&
                         (stpState != FM_STP_STATE_DISABLED) )
                    {
                        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                                     "fmGenericSendPacket: packet queued "
                                     "in slot %d, length %d bytes, port %d\n",
                                     txQueue->pushIndex,
                                     entry->length, firstPort);

                        err = fmPacketQueueUpdate(txQueue);
                        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

                        packetSent = TRUE;
                    }

                }
                else
                {
                    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                                 "fmGenericSendPacket: packet queued "
                                 "in slot %d, length %d bytes, port %d\n",
                                 txQueue->pushIndex,
                                 entry->length, firstPort);

                    err = fmPacketQueueUpdate(txQueue);
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

                    packetSent = TRUE;

                }
            }

            err = fmGetVlanPortNext(sw,
                                    info->directSendVlanId,
                                    firstPort,
                                    &nextPort);

            if (err != FM_OK)
            {
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
            }

            /* Fill in the remaining packet entry fields */
            err = fmGeneratePacketIsl(sw, packet, &tempInfo, entry, 
                                      cpuPort, stagTypeA, stagTypeB,
                                      switchPriority, trapGlort,
                                      suppressVlanTagAllowed);

            if (err != FM_OK)
            {
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
            }

            if (nextPort != -1)
            {
                entry->freePacketBuffer = FALSE;
            }
            else
            {
                /*******************************************************
                 * Free the packet buffer if we have sent to all target
                 * ports in the vlan.
                 ******************************************************/
                entry->freePacketBuffer = TRUE;
            }

            firstPort = nextPort;
        }

        if (!packetSent)
        {
            /***************************************************************
             * If none of the ports in the info->directSendVlanId is in the
             * proper state, return an error to the user application.
             * Note at this point no entry has been enqued in the tx queue.
             ***************************************************************/
            err = FM_ERR_INVALID_PORT_STATE;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
        }
    }

    fmPacketQueueUnlock(txQueue);

    if (err == FM_OK)
    {
        if (!fmRootPlatform->dmaEnabled)
        {
            /**************************************************
             * We must take a lock before writing
             * intrSendPackets because the lock is used
             * by the API to ensure an atomic read-modify-write
             * to intrSendPackets.
             *
             * The platform lock is used instead of state lock
             * because on FIBM platforms, there is an access
             * to intrSendPackets that must be protected before
             * the switch's locks are even created. 
             **************************************************/
            
            FM_TAKE_PKT_INT_LOCK(sw);
            GET_SWITCH_PTR(masterSw)->intrSendPackets = TRUE;
            FM_DROP_PKT_INT_LOCK(sw);
        }

        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                     "fmGenericSendPackets: triggering interrupt handler\n");

        /* Handle the following error case cleanly! */
        err = fmPlatformTriggerInterrupt(masterSw, FM_INTERRUPT_SOURCE_API);

        if (err != FM_OK)
        {
            /***************************************************************
             * We are here because sem_post() fails. In this case we can not
             * cleanly unwind the TX queue to recover, so we simply log a
             * fatal error, and return FM_OK.
             **************************************************************/
            FM_LOG_FATAL(FM_LOG_CAT_EVENT_PKT_TX,
                         "fmGenericSendPacket: "
                         "fmPlatformTriggerInterrupt returned error");

            err = FM_OK;
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);

ABORT:
    txQueue->pushIndex = oldPushIndex;
    fmPacketQueueUnlock(txQueue);

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);

}   /* end fmGenericSendPacket */


