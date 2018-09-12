/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm4000_generic_lci.c
 * Creation Date:   May 22, 2007
 * Description:     Generic LCI send and receive for the FM4000 series
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


#include <fm_sdk_fm4000_int.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/
#define POLL_TX_READY(sw, t, s) \
    fm4000PollLCIStatus( (sw), 0, 1, (t), (s) )

/* The number of buffers to leave for the send side */
#define FM_RECV_BUFFER_THRESHOLD  1

#define FM_RECV_PKT_MAX_BURST     32 

/*****************************************************************************
 * Global Variables
 *****************************************************************************/


/*****************************************************************************
 * Local Variables
 *****************************************************************************/


/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

fm_bool fm4000PollLCIStatus(fm_int     sw,
                            fm_int     bit,
                            fm_int     value,
                            fm_int     timeout,
                            fm_uint32 *status);

void *fm4000LCIReceivePacketTask(void *args);

fm_status fm4000LCIProcessPacketReceive(fm_int sw, fm_event *event);


/*****************************************************************************
 * Local Functions
 *****************************************************************************/



/*****************************************************************************
 * Public Functions
 *****************************************************************************/



/*****************************************************************************/
/** fm4000LCISendPackets
 * \ingroup intPlatformCommon
 *
 * \desc            When called, iterates through the packet queue and
 *                  continues to send packets until either the queue empties
 *                  or the LCI times out.
 *
 * \param[in]       sw refers to the switch number to send packets to.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fm4000LCISendPackets(fm_int sw)
{
    fm_status               err       = FM_OK;
    fm_switch *             switchPtr = GET_SWITCH_PTR(sw);
    fm_packetHandlingState *ps        = &fmRootPlatform->fmPlatformState[sw].packetState;
    fm_packetQueue *        txQueue;
    fm_packetEntry *        packet;
    fm_int                  numWords;
    fm_uint32               value;
    fm_uint32               status;
    fm_buffer *             oldBuffer;
    fm_int                  sendOffset;
    fm_int                  sendBufferOffset;
    fm_bool                 sentIslTag = FALSE;
    fm_int                  packetTxCount;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_TX, "sw = %d\n", sw);


    /* This function shouldn't be called while in DMA */
    if (fmRootPlatform->dmaEnabled)
    {
        goto NOT_NEEDED;
    }

    txQueue = &fmRootPlatform->fmPlatformState[sw].packetState.txQueue;
    fmPacketQueueLock(txQueue);

    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                 "fm4000LCISendPackets(%d)\n", sw);

    sendOffset       = FM_PACKET_OFFSET_HEADER1;
    sendBufferOffset = 0;
    packetTxCount    = 0;

    for ( ;
          txQueue->pullIndex != txQueue->pushIndex ;
          txQueue->pullIndex = (txQueue->pullIndex + 1) % FM_PACKET_QUEUE_SIZE)
    {
        packet = &txQueue->packetQueueList[txQueue->pullIndex];

        /* Can we send data? */
        if ( !POLL_TX_READY(sw, FM_LCI_POLL_COUNT, &status) )
        {
            goto UNMASK;
        }

        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                     "fm4000LCISendPackets: sending packet in "
                     "slot %d, length=%d tag=%d off=%d boff=%d\n",
                     txQueue->pullIndex, packet->length,
                     packet->suppressVlanTag,
                     sendOffset, sendBufferOffset);


        numWords = (packet->length >> 2) + ( (packet->length % 4) ? 1 : 0 );

        if (sendOffset == FM_PACKET_OFFSET_HEADER1)
        {
            if (packet->suppressVlanTag)
            {
                /***********************************************************
                 *  Add 8 bytes since we will send the F64 ISL tag, but
                 *  we will not send the the (outer) vlan tag in the payload
                 *  of the tagged frame, since the switch will insert
                 *  the vlan tag derived from the ISL tag.
                 ***********************************************************/
                value = packet->length + 4;
            }
            else
            {
                /* adding 8 bytes for the F64 ISL tag */
                value = packet->length + 8;
            }

            /***************************************************************
             * Set the attachCRC bit to 1, and let the hardware calculate
             * and attach the CRC word of the packet.
             **************************************************************/
            value = ( value << 16 ) | FM4000_ATTACH_CRC;

            err = switchPtr->WriteUINT32(sw, FM4000_LCI_TX_FIFO, value);

            if (err != FM_OK)
            {
                goto UNMASK;
            }

            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                         "fm4000LCISendPackets: header word 0: 0x%08x\n",
                         value);

            sendOffset++;

            /* Assign current packet buffer */
            ps->currentSendBuffer = packet->packet;
            sendBufferOffset  = 0;
            packetTxCount++;

        }   /* end if (sendOffset ... */

        /* iterate through all buffers */
        for ( ; ( sendOffset < numWords  ) && ps->currentSendBuffer ; )
        {
            /* iterate through all words in a buffer */
            while ( ( sendOffset < numWords ) &&
                   ( sendBufferOffset < FM_BUFFER_SIZE_WORDS ) )
            {
                if ( (sendOffset == FM_PACKET_OFFSET_ETHERTYPE )
                    && sentIslTag == FALSE )
                {
                    /* It is assume here that the CPU port is configured for
                     * F64 tag. If not then things are really broken. So we
                     * don't need to repeatedly check if CPU port is configured
                     * for F64 tag. It is wasteful.
                     */
                    value = htonl(packet->islTag.f64.tag[0]);

                    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                                 "fm4000LCISendPackets: ISL word 0: 0x%08x\n",
                                 value);

                    err = switchPtr->WriteUINT32(sw, FM4000_LCI_TX_FIFO, value);

                    if (err != FM_OK)
                    {
                        goto UNMASK;
                    }

                    value = htonl(packet->islTag.f64.tag[1]);
                    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                                 "fm4000LCISendPackets: ISL word 1: 0x%08x\n",
                                 value);

                    err = switchPtr->WriteUINT32(sw, FM4000_LCI_TX_FIFO, value);

                    if (err != FM_OK)
                    {
                        goto UNMASK;
                    }

                    /* Skip the vlan tag in the data */
                    if (packet->suppressVlanTag)
                    {
                        sendOffset++;
                        sendBufferOffset++;
                    }

                    sentIslTag = TRUE;

                    continue;
                }

                value = ps->currentSendBuffer->data[sendBufferOffset];
                err   = switchPtr->WriteUINT32(sw, FM4000_LCI_TX_FIFO, value);

                if (err != FM_OK)
                {
                    goto UNMASK;
                }

                FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                             "fm4000LCISendPackets: data word %d: 0x%08x\n",
                             sendBufferOffset, value);

                sendOffset++;
                sendBufferOffset++;

            }   /* end while ... */

            /* update buffer pointers */
            oldBuffer             = ps->currentSendBuffer;
            ps->currentSendBuffer = ps->currentSendBuffer->next;

            /**************************************************
             * free buffer only when
             * (1) sending to a single port;
             * or (2) this is the last packet sent to a vlan
             **************************************************/

            if (packet->freePacketBuffer)
            {
                err = fmFreeBuffer(sw, oldBuffer);

                if (err != FM_OK)
                {
                    goto UNMASK;
                }
            }

            fmDbgGlobalDiagCountIncr(FM_GLOBAL_CTR_TX_BUFFER_FREES, 1);

            /* reset counter into new buffer */
            sendBufferOffset = 0;

        }   /* end for ( ... */

        FM_LOG_ASSERT(FM_LOG_CAT_PLATFORM, 
                      ps->currentSendBuffer || (sendOffset >= numWords),
                      "ASSERTION FAILURE: no buffers left: "
                      "sendOffset=%d numWords=%d\n",
                      sendOffset, 
                      numWords);

        /* reset counters here */
        sendOffset       = FM_PACKET_OFFSET_HEADER1;
        sendBufferOffset = 0;
        sentIslTag       = FALSE;

        fmDbgDiagCountIncr(sw, FM_CTR_TX_PKT_COMPLETE, 1);

    }

UNMASK:
    /**************************************************
     * Re-enable the TX ready interrupt.
     **************************************************/
    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                 "fm4000LCISendPacket: unmasking TX RDY interrupt\n");

    err = switchPtr->MaskUINT32(sw, FM4000_LCI_IM, FM4000_INT_LCI_TX_RDY, FALSE);

    fmPacketQueueUnlock(txQueue);

NOT_NEEDED:
    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);

}   /* end fm4000LCISendPackets */




/*****************************************************************************/
/** fm4000LCISendPacketDMA
 * \ingroup intPlatformCommon
 *
 * \desc            When called, iterates through the packet queue and
 *                  continues to enqueue packets to the DMA controller
 *                  until either all packets have been processed or
 *                  until the queue is full.
 *
 * \param[in]       sw refers to the switch number to send packets to.
 *
 * \param[in]       full is a pointer to a user allocate variable which
 *                  will indicate if transmit was blocked if the DMA
 *                  transmit queue was full.
 *
 * \param[in]       empty is a pointer to a user allocate variable which
 *                  will indicate if anything was queued for transmission
 *                  or not.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fm4000LCISendPacketDMA(fm_int sw, fm_bool *full, fm_bool *empty)
{
    fm_status                     err       = FM_OK;
    fm_packetQueue *              txQueue;
    volatile fm_dmaController *   dma = fmRootPlatform->dma;
    fm_packetEntry *              packet;
    fm_uint32                     value;
    volatile fm_bufferDescriptor *bd;
    fm_int                        nBuffers;
    fm_int                        nBDs;
    fm_int                        tail;
    fm_uint32 *                   dst;
    fm_buffer *                   buffer;
    fm_uint32 *                   src;
    fm_uint32                     len;


    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_TX, "sw = %d\n", sw);

    txQueue = &fmRootPlatform->fmPlatformState[sw].packetState.txQueue;

    fmPacketQueueLock(txQueue);

    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                 "fm4000LCISendPacketDMA(%d)\n", sw);

    *full  = FALSE;
    *empty = TRUE;
    CPLD_TRACE(0x200, txQueue->pullIndex);
    CPLD_TRACE(0x201, txQueue->pushIndex);

    /* Now enqueue as many packets as possible */
    if (txQueue->pullIndex != txQueue->pushIndex)
    {
        packet = &txQueue->packetQueueList[txQueue->pullIndex];

        /* Count the number of buffers in this packet */

        buffer   = packet->packet;
        nBuffers = 1;

        while (buffer->next)
        {
            nBuffers++;
            buffer = buffer->next;
        }

        CPLD_TRACE(0x202, nBuffers);

        /* Count the free space in the DMA queue */

        if (dma->txFree > dma->txTail)
        {
            nBDs = dma->txFree - dma->txTail;
        }
        else
        {
            nBDs = FM_DMA_TX_BUFFER_DESCRIPTORS - dma->txTail + dma->txFree;
        }

        CPLD_TRACE(0x203, nBDs);

        /* Can we queue this packet ? */
        if (nBuffers >= nBDs)
        {
            /* No we can't, mark that we are full and wait for
             *  an interrupt from the driver to restart */
            *full = TRUE;
            CPLD_TRACE(0x204, 0);
            goto UNMASK;
        }

        CPLD_TRACE(0xF03, 0);
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                     "fm4000LCISendPacketDMA: sending packet in "
                     "slot %d, length=%d\n",
                     txQueue->pullIndex, packet->length);


        /* Where to store the header! */

        tail             = dma->txTail;
        bd               = &dma->txBufferDescriptors[tail];
        dst              = (fm_uint32 *) bd->header;
        bd->headerLength = 0;
        buffer           = packet->packet;
        src              = buffer->data;
        len              = buffer->len;
        CPLD_TRACE(0x205, dma->txTail);

        /***** Start by placing the Control Word *****/

        if (packet->suppressVlanTag)
        {
            /*  adding 8 bytes since we will send the F64 ISL tag, but
             *  we will not send the the (outer) vlan tag in the payload
             *  of the tagged frame, since the switch will insert
             *  the vlan tag derived from the ISL tag. */
            value = packet->length + 4;
        }
        else
        {
            /* adding 8 bytes for the F64 ISL tag */
            value = packet->length + 8;
        }

        /* Write the LCI_TX_CMD word */
        /* set the attachCRC bit to 1, and let the hardware to calculate
         *  and attach the CRC word of the packet */
        *dst++ = ( value << 16 ) | FM4000_ATTACH_CRC;
        bd->headerLength++;

        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                     "fm4000LCISendPacketDMA: header word 0: 0x%08x\n",
                     value);


        /*** Copy the next 3 words of the first buffer into the header ***/

        *dst++            = *src++;
        *dst++            = *src++;
        *dst++            = *src++;
        len              -= 12;
        bd->headerLength += 3;

        /*** Add the first word of the F64 tag ***/

        if (packet->suppressVlanTag)
        {
            /* we need to skip sending the vlan tag in the
             *  payload since the switch will add a VLAN tag
             *  using the info from the ISL tag */
            len -= 4;
            src++;
        }

        value = htonl(packet->islTag.f64.tag[0]);
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                     "ISL word 0: 0x%08x\n", value);

        *dst++ = value;
        bd->headerLength++;

        /*** Add the second word of the F64 tag ***/
        value = htonl(packet->islTag.f64.tag[1]);
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                     "ISL word 1: 0x%08x\n", value);

        *dst++ = value;
        bd->headerLength++;

        /*** Load the first buffer on the BD ***/

        bd->data        = src;
        bd->length      = (len + 3) / 4;
        bd->buffer      = buffer;
        bd->endOfPacket = (buffer->next) ? FALSE : TRUE;

        bd->free        = packet->freePacketBuffer;
        bd->sw          = sw;
        tail            = (tail + 1) % FM_DMA_TX_BUFFER_DESCRIPTORS;

        /*** iterate through all other buffers ***/
        while (buffer->next)
        {
            buffer           = buffer->next;
            bd               = &dma->txBufferDescriptors[tail];
            bd->data         = buffer->data;
            bd->length       = (buffer->len + 3) / 4;
            bd->headerLength = 0;
            bd->endOfPacket  = (buffer->next) ? FALSE : TRUE;
            bd->free         = packet->freePacketBuffer;
            bd->buffer       = buffer;
            bd->sw           = sw;
            tail             = (tail + 1) % FM_DMA_TX_BUFFER_DESCRIPTORS;
        }

        dma->txTail = tail;

        /* Clear this entry out of the queue */
        txQueue->pullIndex = (txQueue->pullIndex + 1) % FM_PACKET_QUEUE_SIZE;
        CPLD_TRACE(0x20F, txQueue->pullIndex);

        /* Indicate that one packet was sent */
        *empty = FALSE;

        fmDbgDiagCountIncr(sw, FM_CTR_TX_PKT_COMPLETE, 1);

    }

UNMASK:

    fmPacketQueueUnlock(txQueue);

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);

}   /* end fm4000LCISendPacketDMA */




/*****************************************************************************/
/** fm4000LCIReceivePackets
 * \ingroup intPlatformCommon
 *
 * \desc            Performs a generic LCI packet receive for FM4000 series
 *                  devices
 *
 * \param[in]       sw is the switch to receive packets on
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fm4000LCIReceivePackets(fm_int sw)
{
    fm_status err;

    FM_NOT_USED(sw);

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_RX, "sw = %d\n", sw);

    err = fmSignalSemaphore(&fmRootApi->packetReceiveSemaphore);
    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, err);

}   /* end fm4000LCIReceivePackets */




/*****************************************************************************/
/** fm4000LCIReceivedPacketHandler
 * \ingroup intPlatformCommon
 *
 * \desc            Handles reception of packets.  Triggered by a semaphore
 *                  from fm4000LCIReceivePackets
 *                  devices
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fm4000LCIReceivedPacketHandler(fm_int sw)
{
    fm_status               err = FM_OK;
    fm_switch *             switchPtr;
    fm_packetHandlingState *ps
    = &fmRootPlatform->fmPlatformState[sw].packetState;
    fm_uint32               value;
    fm_uint32               status;
    fm_int                  availableBuffers;
    fm_int                  burst;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_RX, "sw = %d\n", sw);

    if (fmRootPlatform->dmaEnabled)
    {
        /* Should not get here */
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, FM_OK);
    }

    switchPtr = GET_SWITCH_PTR(sw);
    value     = 0;

    TAKE_PLAT_LOCK(sw, FM_PLAT_INFO);

    for (burst = 0 ; burst < FM_RECV_PKT_MAX_BURST ; burst++)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                     "fm4000LCIReceivePackets: entering burst\n");

        /* Are we currently receiving a packet? */
        if (!ps->recvInProgress)
        {
            /* No packet in progress; starting a new packet. */

            /**************************************************
             * Ready to start receiving a packet. Is there
             * (another) one to receive?
             **************************************************/

            err = switchPtr->ReadUINT32(sw, FM4000_LCI_STATUS, &status);

            if (err != FM_OK)
            {
                /* Unable to read LCI status! */
                goto UNMASK;
            }

            if ( !(status & FM4000_LCI_STATUS_RX_RDY) )
            {
                /* No (more) packets to receive. */
                FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                             "fm4000LCIReceivePackets: no packet to receive\n");
                goto UNMASK;
            }

            /* Set status indicator */
            ps->recvInProgress = TRUE;

            /* Reset local state */
            ps->currentRecvBuffer    = NULL;
            ps->recvChainHead        = NULL;
            ps->recvChainTail        = NULL;
            ps->recvBufferOffset     = 0;
            ps->currentWordsReceived = 0;

            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                         "fm4000LCIReceivePackets: start receiving packet\n");
        }
        else
        {
            /* We are already in the middle of receiving a packet and have
             * already allocated it, so continue using the same packet. */
            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                         "fm4000LCIReceivePackets: packet in progress\n");

        }   /* end if(!ps->recvInProgress) */

        switchPtr->ReadUINT32(sw, FM4000_LCI_STATUS, &status);

        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                     "fm4000LCIReceivePackets: LCI status = 0x%08x\n", status);

        /* While EOT is not set */
        while ( !(status & FM4000_LCI_STATUS_RX_EOT) )
        {
            /* If we don't already have a buffer allocated... */
            if (ps->currentRecvBuffer == NULL)
            {
                /* get the number of available buffers from the buffer manager*/
                fmPlatformGetAvailableBuffers(&availableBuffers);

                /* ...allocate a buffer to receive frame data into. */
                if (availableBuffers > FM_RECV_BUFFER_THRESHOLD)
                {
                    ps->currentRecvBuffer = fmAllocateBuffer(sw);
                }

                /* Did we get a buffer? */
                if (!ps->currentRecvBuffer)
                {
                    /* No buffers available.
                     * Set out of buffers flag.
                     * Increment relavant statistic.
                     */
                    if (availableBuffers > 0)
                    {
                        fmDbgGlobalDiagCountIncr(
                            FM_GLOBAL_CTR_NO_BUFFERS_FOR_RX, 1);
                    }
                    else
                    {
                        fmDbgGlobalDiagCountIncr(
                            FM_GLOBAL_CTR_RX_OUT_OF_BUFFERS, 1);
                    }

                    /* Set the out of buffers flag so that when the
                     * the next buffer is freed, we will get notified
                     */
                    switchPtr->buffersNeeded = TRUE;

                    /* Do not unmask, we will re-enter the interrupt
                     * handler via the flag set by the free buffer
                     * code
                     */
                    goto ABORT;
                }

                /* reset length */
                ps->currentRecvBuffer->len = 0;

                if (ps->recvChainHead == NULL)
                {
                    /* This is the first buffer in the chain. */
                    ps->recvChainHead    = ps->currentRecvBuffer;
                    ps->recvChainTail    = ps->currentRecvBuffer;
                }
                else
                {
                    /* Add new buffer to end of chain. We don't set tail
                     * to point to this new buffer until we are done
                     * using it so we can discard it if it ends up having
                     * no data put into it. */
                    ps->recvChainTail->next = ps->currentRecvBuffer;

                }

                /* Terminate the buffer chain. */
                ps->currentRecvBuffer->next = NULL;

                /* Increment the receive alloc statistic. */
                fmDbgGlobalDiagCountIncr(FM_GLOBAL_CTR_RX_BUFFER_ALLOCS, 1);

                /* Clear out of chunks flag since we got one. */
                switchPtr->buffersNeeded = FALSE;

                /* Reset the offset into the buffer at which to place data. */
                ps->recvBufferOffset = 0;

            }   /* end if (ps->currentRecvBuffer == NULL) */

            switchPtr->ReadUINT32(sw, FM4000_LCI_STATUS, &status);

            switchPtr->ReadUINT32(sw, FM4000_LCI_RX_FIFO, &value);

            if ( !(status & FM4000_LCI_STATUS_RX_EOT) )
            {
                /* we don't count the status word  */
                ps->currentWordsReceived++;
            }

            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                         "fm4000LCIReceivePackets: data word 0x%08x offset %d\n",
                         value, ps->currentWordsReceived);

            if ( !(status & FM4000_LCI_STATUS_RX_EOT) )
            {
                /* Also have the ISL Tag in the buffer to be consistent with
                 * other receive methods, and this will give us some space
                 * to insert the vlan tag if needed.
                 */
                ps->currentRecvBuffer->data[ps->recvBufferOffset++] = value;
                ps->currentRecvBuffer->len                         += 4;

                /* Have we come to the end of the buffer? */
                if (ps->recvBufferOffset == FM_BUFFER_SIZE_WORDS)
                {
                    /* Yes.  Reset the offset. */
                    ps->recvBufferOffset = 0;

                    /* Indicate we need a new buffer allocated. */
                    ps->recvChainTail     = ps->currentRecvBuffer;
                    ps->currentRecvBuffer = NULL;

                }
            }

        }   /* end while(!(status & FM4000_LCI_STATUS_RX_EOT)) */

        /**************************************************
         * ps->curentRecvBuffer can be NULL in the event that the
         * last word of the packet payload fills a buffer and the
         * request for a new buffer fails. When the buffer becomes
         * available, since the next word in the RX_FIFO is the
         * RX_FRAME_STATUS word, a read of the LCI_STATUS returns
         * a set EOT bit. This causes the above while loop being skipped,
         * and no new buffer is allocated.
         **************************************************/

        if (ps->currentRecvBuffer == NULL)
        {
            /* need to finish reading the RX_FRAME_STATUS word */
            switchPtr->ReadUINT32(sw, FM4000_LCI_RX_FIFO, &value);

            ps->currentRecvBuffer = ps->recvChainTail;

            if (ps->currentRecvBuffer == NULL)
            {
                FM_LOG_FATAL(FM_LOG_CAT_EVENT_PKT_RX, "recvChainTail is NULL!\n");
                goto ABORT;
            }
        }

        /**************************************************
         * Make sure we didn't have to get a new buffer
         * just to read the frame status. If so, discard
         * the last buffer since it has nothing in it.
         **************************************************/

        if (ps->currentRecvBuffer->len == 0)
        {
            /* This should not be the only buffer. */
            if (ps->currentRecvBuffer == ps->recvChainHead)
            {
                FM_LOG_ERROR(FM_LOG_CAT_EVENT_PKT_RX,
                             "LCI indicated packet, but with no data!\n");
                fmFreeBufferChain(sw, ps->recvChainHead);
                fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_DROPS_NO_DATA, 1);
                ps->recvInProgress    = FALSE;
                ps->currentRecvBuffer = NULL;
                ps->recvChainHead     = NULL;
                ps->recvChainTail     = NULL;
                ps->recvBufferOffset  = 0;
                continue;
            }
            else
            {
                ps->recvChainTail->next = NULL;
                fmFreeBuffer(sw, ps->currentRecvBuffer);
                ps->currentRecvBuffer = ps->recvChainTail;
            }
        }

        /***************************************************
         * We need to calculate the valid byte count.  The
         * currentWordsReceived state variables contains the
         * total number of words read from the RX FIFO.
         * The difference between that and the
         * length in the status word indicates padding
         * that should be removed.
         **************************************************/
        ps->currentRecvBuffer->len -= (ps->currentWordsReceived * 4) -
                                      ( (value & FM4000_LCI_RX_LENGTH_MASK)
                                       >> FM4000_LCI_RX_LENGTH );

        /* reinitialize this so we grab the next one later */
        ps->recvInProgress    = FALSE;

        if (value & 1)
        {
            fmFreeBufferChain(sw, ps->recvChainHead);
            fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_DROPS_FOR_ERROR, 1);
            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX, "CRC error in frame!\n");
        }
        else
        {
            /* Process the complete packet */
            fm4000PacketReceiveProcess(sw, ps->recvChainHead, NULL, 0);
        } 
        ps->recvChainHead = NULL;

    }   /* end for(... */

UNMASK:
    /**************************************************
     * Re-enable the RX ready interrupt.
     **************************************************/
    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                 "fm4000LCIReceivePackets: unmasking RX RDY interrupt\n");


    switchPtr->MaskUINT32(sw, FM4000_LCI_IM, FM4000_INT_LCI_RX_RDY, FALSE);

ABORT:

    DROP_PLAT_LOCK(sw, FM_PLAT_INFO);

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, err);

}   /* end fm4000LCIReceivedPacketHandler */




/*****************************************************************************/
/** fm4000LCIReceivedPacketDMA
 * \ingroup intPlatformCommon
 *
 * \desc            Handles reception of packets by DMA.  This is called
 *                  by the DMA controller task.
 *
 * \param[in]       sw is the switch number
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fm4000LCIReceivedPacketDMA(fm_int sw)
{
    fm_status                     err = FM_OK;
    volatile fm_dmaController *   dma = fmRootPlatform->dma;
    volatile fm_bufferDescriptor *bd;
    fm_int                        head;
    fm_buffer *                   buffer;
    fm_buffer *                   firstBuffer;
    fm_buffer *                   lastBuffer;
    fm_int                        totalLen;
    fm_uint32                     rxStatusWord;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_RX, "sw = %d\n", sw);

    TAKE_PLAT_LOCK(sw, FM_PLAT_INFO);

    CPLD_TRACE(0xF08, 0);
    CPLD_TRACE(0x850, dma->rxHead);

    /* Construct the buffer chain from the BDs */
    head = dma->rxHead;
    bd   = &dma->rxBufferDescriptors[head];
    CPLD_TRACE(0x851, head);
    CPLD_TRACE(0x852, bd->length);
    firstBuffer      = bd->buffer;
    firstBuffer->len = bd->length * 4;
    totalLen         = bd->length * 4;
    lastBuffer       = NULL;

    buffer = firstBuffer;

    while (!bd->endOfPacket)
    {
        head = (head + 1) % FM_DMA_RX_BUFFER_DESCRIPTORS;
        bd   = &dma->rxBufferDescriptors[head];
        CPLD_TRACE(0x853, head);
        CPLD_TRACE(0x854, bd->length);
        lastBuffer       = buffer;
        buffer           = bd->buffer;
        buffer->len      = bd->length * 4;
        lastBuffer->next = buffer;
        totalLen        += bd->length * 4;
    }

    /* Pick up the status word which is the last word of the last buffer */
    rxStatusWord = buffer->data[buffer->len / 4 - 1];

    /* One less word in the packet */
    totalLen -= 4;

    /* If the less buffer has only one word, then return this buffer
     * to the pool as it contains nothing useful anymore */
    if (buffer->len <= 4)
    {
        if (lastBuffer != NULL)
        {
            lastBuffer->next = NULL;
        }

        fmFreeBuffer(sw, buffer);
    }
    else
    {
        buffer->next = NULL;
        buffer->len  = buffer->len - 4;
        lastBuffer   = buffer;
    }

    dma->rxHead = (head + 1) % FM_DMA_RX_BUFFER_DESCRIPTORS;
    CPLD_TRACE(0x855, dma->rxHead);

    /***************************************************
     * We need to calculate the valid byte count.  The
     * currentWordsReceived state variables contains the
     * total number of words read from the RX FIFO.
     * The difference between that and the
     * length in the status word indicates padding
     * that should be removed.
     **************************************************/
    if (lastBuffer != NULL)
    {
        lastBuffer->len -= (totalLen) -
                           ( (rxStatusWord & FM4000_LCI_RX_LENGTH_MASK)
                            >> FM4000_LCI_RX_LENGTH );
    }

    CPLD_TRACE(0x856, bd->length);

    if (rxStatusWord & 1)
    {
        fmFreeBufferChain(sw, firstBuffer);
        fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_DROPS_FOR_ERROR, 1);
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX, "CRC error in frame!\n");
    }
    else
    {
     /* Process the complete packet */
     (void)fm4000PacketReceiveProcess(sw, firstBuffer, NULL, 0);
 
    }

    /**************************************************
     * Re-enable the RX ready interrupt.
     **************************************************/
    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                 "fm4000LCIReceivePacketDMA: unmasking RX RDY interrupt\n");


    DROP_PLAT_LOCK(sw, FM_PLAT_INFO);

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, err);

}   /* end fm4000LCIReceivedPacketDMA */




/*****************************************************************************/
/** fm4000PollLCIStatus
 * \ingroup intPlatformCommon
 *
 * \desc            Polls bits in the LCI_STATUS register.
 *
 * \param[in]       sw is the switch whose LCI status to poll.
 *
 * \param[in]       bit is the bit number to poll.
 *
 * \param[in]       value is the value of the bit to poll for.
 *
 * \param[in]       timeout is a timeout in number of reads.
 *
 * \param[out]      status will contain the last value of the
 *                  LCI_STATUS register.
 *
 * \return          TRUE if the poll succeeded, FALSE otherwise.
 *
 *****************************************************************************/
fm_bool fm4000PollLCIStatus(fm_int     sw,
                            fm_int     bit,
                            fm_int     value,
                            fm_int     timeout,
                            fm_uint32 *status)
{
    fm_status  err;
    fm_int     timer     = 0;
    fm_uint32  uvalue    = (fm_uint32) value;
    fm_switch *switchPtr = GET_SWITCH_PTR(sw);

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw = %d, bit = %d, value = %d, timeout = %d, status = %p\n",
                 sw,
                 bit,
                 value,
                 timeout,
                 (void *) status);

    do
    {
        err = switchPtr->ReadUINT32(sw, FM4000_LCI_STATUS, status);

        if (err != FM_OK)
        {
            FM_LOG_EXIT_CUSTOM( FM_LOG_CAT_PLATFORM,
                               FALSE,
                               "false - %s\n",
                               fmErrorMsg(err) );
        }

        timer++;
    }
    while ( ( ( *status & ( 1 << (bit) ) ) != ( (uvalue) << (bit) ) ) &&
           (timer < timeout) );

    if (timer >= timeout)
    {
        FM_LOG_EXIT_CUSTOM(FM_LOG_CAT_PLATFORM,
                           FALSE,
                           "false, *status = %u\n",
                           *status);
    }

    FM_LOG_EXIT_CUSTOM(FM_LOG_CAT_PLATFORM,
                       TRUE,
                       "true, *status = %u\n",
                       *status);

} /* end fm4000PollLCIStatus */



