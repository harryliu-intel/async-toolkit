/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm6000_generic_lci.c
 * Creation Date:   2010
 * Description:     Generic LCI send and receive for the FM6000 series
 *
 * INTEL CONFIDENTIAL
 * Copyright 2010 - 2012 Intel Corporation. All Rights Reserved.
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

#include <stdlib.h>

#include <fm_sdk_fm6000_int.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/
#define POLL_TX_READY(sw, t, s) \
    fm6000PollLCIStatus( (sw), 0, 1, (t), (s) )

/* The number of buffers to leave for the send side */
#define FM_RECV_BUFFER_THRESHOLD  1
#define FM_RECV_PKT_MAX_BURST     32 

/* Table 138: Format of the EBI RX Status Word */
#define FM6000_LCI_RX_LENGTH_MASK        0x3FFF0000
#define FM6000_LCI_RX_LENGTH_OFFSET      16
#define FM6000_LCI_RX_ERROR              0x1

#define FM6000_LCI_TX_LENGTH_OFFSET      16

/*****************************************************************************
 * Global Variables
 *****************************************************************************/


/*****************************************************************************
 * Local Variables
 *****************************************************************************/


/*****************************************************************************
 * Local Function Prototypes
 *****************************************************************************/


/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** fm6000PollLCIStatus
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
static fm_bool fm6000PollLCIStatus(fm_int     sw,
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
        err = switchPtr->ReadUINT32(sw, FM6000_LCI_STATUS, status);

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

}   /* end fm6000PollLCIStatus */




/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** fm6000LCIReceivePackets
 * \ingroup intPlatformCommon
 *
 * \desc            Notify that packets are pending
 *
 * \param[in]       sw is the switch to receive packets on
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fm6000LCIReceivePackets(fm_int sw)
{
    fm_status err;

    FM_NOT_USED(sw);

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_RX, "sw = %d\n", sw);

    err = fmSignalSemaphore(&fmRootApi->packetReceiveSemaphore);
    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, err);

}   /* end fm6000LCIReceivePackets */




/*****************************************************************************/
/** fm6000LCIReceivedPacketHandler
 * \ingroup intPlatformCommon
 *
 * \desc            Handles reception of packets.  Triggered by a semaphore
 *                  from fm6000LCIReceivePackets
 *                  devices
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fm6000LCIReceivedPacketHandler(fm_int sw)
{
    fm_status               err = FM_OK;
    fm_switch *             switchPtr;
    fm_packetHandlingState *ps = GET_PLAT_PKT_STATE(sw);
    fm_uint32               value;
    fm_uint32               status;
    fm_int                  availableBuffers;
    fm_int                  burst;
    fm_int                  rxLength;

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
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX, "entering burst\n");

        /* Are we currently receiving a packet? */
        if (!ps->recvInProgress)
        {
            /* No packet in progress; starting a new packet. */

            /**************************************************
             * Ready to start receiving a packet. Is there
             * (another) one to receive?
             **************************************************/

            err = switchPtr->ReadUINT32(sw, FM6000_LCI_STATUS, &status);

            if (err != FM_OK)
            {
                /* Unable to read LCI status! */
                goto UNMASK;
            }

            if ( !(FM_GET_BIT(status, FM6000_LCI_STATUS, rxReady)) )
            {
                /* No (more) packets to receive. */
                FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX, "no packet to receive\n");
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

            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX, "start receiving packet\n");
        }
        else
        {
            /* We are already in the middle of receiving a packet and have
             * already allocated it, so continue using the same packet. */
            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX, "packet in progress\n");

        }   /* end if(!ps->recvInProgress) */

        switchPtr->ReadUINT32(sw, FM6000_LCI_STATUS, &status);

        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX, "LCI status = 0x%08x\n", status);

        /* While EOT is not set */
        while ( !(FM_GET_BIT(status, FM6000_LCI_STATUS, rxEndOfFrame)) )
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

            switchPtr->ReadUINT32(sw, FM6000_LCI_STATUS, &status);

            switchPtr->ReadUINT32(sw, FM6000_LCI_RX_FIFO, &value);

            if ( !(FM_GET_BIT(status, FM6000_LCI_STATUS, rxEndOfFrame)) )
            {
                /* we don't count the status word  */
                ps->currentWordsReceived++;
            }

            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                         "data word 0x%08x offset %d\n",
                         value, ps->currentWordsReceived);

            if ( !(FM_GET_BIT(status, FM6000_LCI_STATUS, rxEndOfFrame)) )
            {
                /* Also have the ISL Tag in the buffer to be consistent with
                 * other receive methods, and this will give us some space
                 * to insert the vlan tag if needed.
                 */
                ps->currentRecvBuffer->data[ps->recvBufferOffset++] = value;
                ps->currentRecvBuffer->len += 4;

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

        }   /* end while(!(FM_GET_BIT(status, FM6000_LCI_STATUS, rxEndOfFrame))) */

        /**************************************************
         * ps->curentRecvBuffer can be NULL in the event that the
         * last word of the packet payload fills a buffer and the
         * request for a new buffer fails. When the buffer becomes
         * available, since the next word in the RX_FIFO is the
         * RX_FRAME_STATUS word, a read of the LCI_STATUS returns
         * a set EOT bit. This causes the above while loop being skipped,
         * and no new buffer is allocated.
         **************************************************/

        if (!ps->currentRecvBuffer)
        {
            /* need to finish reading the RX_FRAME_STATUS word */
            switchPtr->ReadUINT32(sw, FM6000_LCI_RX_FIFO, &value);

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
        rxLength = (value & FM6000_LCI_RX_LENGTH_MASK) >> FM6000_LCI_RX_LENGTH_OFFSET;
        ps->currentRecvBuffer->len -= (ps->currentWordsReceived * 4) - rxLength;
                                      
        /* reinitialize this so we grab the next one later */
        ps->recvInProgress    = FALSE;

        if (value & FM6000_LCI_RX_ERROR)
        {
            fmFreeBufferChain(sw, ps->recvChainHead);
            fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_DROPS_FOR_ERROR, 1);
            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX, "CRC error in frame!\n");
        }
        else
        {
            /* Process the complete packet */
            fm6000PacketReceiveProcess(sw, ps->recvChainHead, NULL, 0);
        } 
        ps->recvChainHead = NULL;

    }   /* end for(... */

UNMASK:
    /**************************************************
     * Re-enable the RX ready interrupt.
     **************************************************/
    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                 "unmasking RX RDY interrupt\n");

    switchPtr->MaskUINT32(sw,
                          FM6000_LCI_IM,
                          (1 << FM6000_LCI_IP_b_newFrameRecv),
                          FALSE);

ABORT:

    DROP_PLAT_LOCK(sw, FM_PLAT_INFO);

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, err);

}   /* end fm6000LCIReceivedPacketHandler */




/*****************************************************************************/
/** fm6000LCISendPackets
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
fm_status fm6000LCISendPackets(fm_int sw)
{
    fm_status               err       = FM_OK;
    fm_switch *             switchPtr = GET_SWITCH_PTR(sw);
    fm_packetHandlingState *ps        = GET_PLAT_PKT_STATE(sw);
    fm_packetQueue *        txQueue;
    fm_packetEntry *        packet;
    fm_int                  numWords;
    fm_int                  numBytes;
    fm_uint32               value = 0;
    fm_uint32               status;
    fm_buffer *             oldBuffer;
    fm_int                  sendOffset;
    fm_int                  sendBufferOffset;
    fm_bool                 sentIslTag = FALSE;
    fm_int                  packetTxCount;
    fm_int                  pktLen;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_TX, "sw = %d\n", sw);

    /* This function shouldn't be called while in DMA */
    if (fmRootPlatform->dmaEnabled)
    {
        goto NOT_NEEDED;
    }

    txQueue = &ps->txQueue;
    fmPacketQueueLock(txQueue);

    FM_LOG_DEBUG_VERBOSE(FM_LOG_CAT_EVENT_PKT_TX,
                         "fm6000LCISendPackets(%d)\n", sw);

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
            goto ABORT;
        }

        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                     "sending packet in slot %d, "
                     "length=%d suppressTag=%d off=%d boff=%d\n",
                     txQueue->pullIndex, packet->length,
                     packet->suppressVlanTag,
                     sendOffset, sendBufferOffset);

        /* Add 4 bytes for the FCS. */
        /* Note: cannot modify the packet->length directly */
        pktLen = packet->length + 4;

        numWords = pktLen >> 2;
        numBytes = packet->length & 3;

        if (sendOffset == FM_PACKET_OFFSET_HEADER1)
        {
            if (packet->suppressVlanTag)
            {
                /***********************************************************
                 *  Add 4 bytes since we will send the F64 ISL tag, but
                 *  we will not send the the (outer) vlan tag in the payload
                 *  of the tagged frame, since the switch will insert
                 *  the vlan tag derived from the ISL tag.
                 ***********************************************************/
                value = pktLen + 4;
            }
            else
            {
                /* adding 8 bytes for the F64 ISL tag */
                value = pktLen + 8;
            }

            /***************************************************************
             * Send the LCI control word. Note that the FM6000 LCI does not 
             * have an ATTACH_CRC feature.
             **************************************************************/
            value = ( value << FM6000_LCI_TX_LENGTH_OFFSET );

            err = switchPtr->WriteUINT32(sw, FM6000_LCI_TX_FIFO, value);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                         "header word 0: 0x%08x\n",
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
                if ( (sendOffset == FM_PACKET_OFFSET_ETHERTYPE) && !sentIslTag )
                {
                    /* It is assumed here that the CPU port is configured for
                     * F64 tag. If not then things are really broken. So we
                     * don't need to repeatedly check if CPU port is configured
                     * for F64 tag. It is wasteful.
                     */
                    value = htonl(packet->islTag.f64.tag[0]);

                    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                                 "ISL word 0: 0x%08x\n",
                                 value);

                    err = switchPtr->WriteUINT32(sw, FM6000_LCI_TX_FIFO, value);
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

                    value = htonl(packet->islTag.f64.tag[1]);
                    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                                 "ISL word 1: 0x%08x\n",
                                 value);

                    err = switchPtr->WriteUINT32(sw, FM6000_LCI_TX_FIFO, value);
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

                    /* Skip the vlan tag in the data */
                    if (packet->suppressVlanTag)
                    {
                        sendOffset++;
                        sendBufferOffset++;
                        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                                     "Suppress vlan tag in packet data\n");
                    }

                    sentIslTag = TRUE;

                    continue;
                }

                if (sendOffset < (numWords-1))
                {
                    /* We have not yet reached the last word in the buffer.
                     * Send a word consisting solely of buffer data. */
                    value = ps->currentSendBuffer->data[sendBufferOffset];

                    err = switchPtr->WriteUINT32(sw, FM6000_LCI_TX_FIFO, value);
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

                    FM_LOG_DEBUG_VERBOSE(FM_LOG_CAT_EVENT_PKT_TX,
                                         "data word %d: 0x%08x\n",
                                         sendBufferOffset,
                                         value);
                }
                else if (numBytes == 0)
                {
                    /* The FCS is word-aligned, and we've sent all the data.
                     * Now send a word consisting solely of the FCS. */
                    value = htonl(packet->fcsVal);

                    err = switchPtr->WriteUINT32(sw, FM6000_LCI_TX_FIFO, value);
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

                    FM_LOG_DEBUG_VERBOSE(FM_LOG_CAT_EVENT_PKT_TX,
                                         "FCS word 0: 0x%08x\n",
                                         value);
                }
                else
                {
                    /* The last word in the buffer is a partial chunk, so the
                     * FCS will span a word boundary. We append the FCS to the
                     * partial word, pad it with zeroes, and send two words. */
                    union
                    {
                        fm_uint32   words[2];
                        fm_byte     bytes[8];
                    } fcsData;

                    union
                    {
                        fm_uint32   word;
                        fm_byte     bytes[4];
                    } fcsValue;

                    fm_int  i;

                    /* Start with the partial data word and the padding. */
                    fcsData.words[0] = ps->currentSendBuffer->data[sendBufferOffset];
                    fcsData.words[1] = 0;

                    /* Overlay with the FCS in network byte order. */
                    fcsValue.word = htonl(packet->fcsVal);

                    for (i = 0 ; i < 4 ; i++)
                    {
                        fcsData.bytes[numBytes + i] = fcsValue.bytes[i];
                    }

                    /* Write both words to the Tx FIFO. */
                    for (i = 0 ; i < 2 ; i++)
                    {
                        err = switchPtr->WriteUINT32(sw,
                                                     FM6000_LCI_TX_FIFO,
                                                     fcsData.words[i]);
                        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

                        FM_LOG_DEBUG_VERBOSE(FM_LOG_CAT_EVENT_PKT_TX,
                                             "FCS word %d: 0x%08x\n",
                                             i,
                                             fcsData.words[i]);
                    }

                }   /* end else */

                sendBufferOffset++;
                sendOffset++;

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
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
            }

            fmDbgGlobalDiagCountIncr(FM_GLOBAL_CTR_TX_BUFFER_FREES, 1);

            /* reset counter into new buffer */
            sendBufferOffset = 0;

        }   /* end for ... */

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

    }   /* end for ... */

ABORT:
    /**************************************************
     * Re-enable the TX ready interrupt.
     **************************************************/
    FM_LOG_DEBUG_VERBOSE(FM_LOG_CAT_EVENT_PKT_TX,
                         "unmasking TX RDY interrupt\n");

    err = switchPtr->MaskUINT32(sw,
                                FM6000_LCI_IM,
                                (1 << FM6000_LCI_IM_b_endOfFrameSend),
                                FALSE);

    fmPacketQueueUnlock(txQueue);

NOT_NEEDED:
    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);

}   /* end fm6000LCISendPackets */




