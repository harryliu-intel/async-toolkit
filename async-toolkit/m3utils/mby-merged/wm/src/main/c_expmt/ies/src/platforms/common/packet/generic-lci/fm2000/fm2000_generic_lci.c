/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm2000_generic_lci.c
 * Creation Date:   May 22, 2007
 * Description:     Generic LCI send and receive for the FM2000 series
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


#include <fm_sdk_fm2000_int.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/
/**
 * Note we redefine these macros here because normally they are called from
 * a function whose assumed return type is fm_status.  Our thread body
 * returns void * so we must redefine them
 */
#undef VALIDATE_AND_PROTECT_SWITCH
#undef VALIDATE_SWITCH_LOCK

#define VALIDATE_SWITCH_LOCK(sw)   \
    if ( !SWITCH_LOCK_EXISTS(sw) ) \
    {                              \
        return NULL;               \
    }

#define VALIDATE_AND_PROTECT_SWITCH(sw)                                      \
    if ( (sw) < 0 || (sw) >= FM_MAX_NUM_SWITCHES )                           \
    {                                                                        \
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,                                    \
                     "(VALIDATE_AND_PROTECT_SWITCH): %d not in [0,%d]\n",    \
                     (sw), FM_MAX_NUM_SWITCHES);                             \
        return NULL;                                                         \
    }                                                                        \
    VALIDATE_SWITCH_LOCK(sw);                                                \
    /* Take read access to the switch lock */                                \
    PROTECT_SWITCH(sw);                                                      \
    if (!fmRootApi->fmSwitchStateTable[(sw)])                                \
    {                                                                        \
        FM_LOG_ERROR( FM_LOG_CAT_PLATFORM,                                   \
                     "(VALIDATE_AND_PROTECT_SWITCH): %d is not allocated\n", \
                     (sw) );                                                 \
        UNPROTECT_SWITCH(sw);                                                \
        return NULL;                                                         \
    }                                                                        \
    if (!fmRootApi->fmSwitchStateTable[(sw)]->up)                            \
    {                                                                        \
        FM_LOG_ERROR( FM_LOG_CAT_PLATFORM,                                   \
                     "(VALIDATE_AND_PROTECT_SWITCH): %d is not up\n",        \
                     (sw) );                                                 \
        UNPROTECT_SWITCH(sw);                                                \
        return NULL;                                                         \
    }                                                                        \
    swProtected = TRUE

#define POLL_TX_READY(sw, t, s) \
    fm2000PollLCIStatus( (sw), 0, 1, (t), (s) )

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

static fm_int fm2000ComputeTotalPacketLength(fm_buffer *packet);

static fm_bool fm2000PollLCIStatus(fm_int     sw,
                                   fm_int     bit,
                                   fm_int     value,
                                   fm_int     timeout,
                                   fm_uint32 *status);

static void fm2000LCIProcessPacketReceive(fm_int sw, fm_event *event);


/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** PaddingFrame
 * \ingroup intPlatformCommon
 *
 * \desc            Zero-padding a frame to a larger size.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       packet is a pointer to a chain of fm_buffer structures
 *                  containing the payload.
 *
 * \param[in]       frameSize is the size of the packet in bytes.
 *
 * \param[in]       targetSize is the size to pad the packet to in bytes. 
 *
 * \return          FM_OK.
 * \return          FM_ERR_NO_MEM if no buffer available.
 *
 *****************************************************************************/
static fm_status PaddingFrame(fm_int sw, 
                              fm_buffer *packet, 
                              fm_int frameSize, 
                              fm_int targetSize)
{
    fm_int      packetWordIndex;
    fm_int      packetWordLastByte;
    fm_int      i;
    fm_buffer * pkt; 
    fm_int      numBuffer;
    fm_int      numTargetBuffer; 
    fm_uint32   paddingMask;

    if (frameSize >= targetSize)
    {
        return FM_OK;
    }

    numBuffer = frameSize / FM_BUFFER_SIZE_BYTES + 
                            (frameSize % FM_BUFFER_SIZE_BYTES )?1:0;
 
    numTargetBuffer = targetSize / FM_BUFFER_SIZE_BYTES + 
                            (targetSize % FM_BUFFER_SIZE_BYTES )?1:0;

    /* Check if we have to pad over to the next buffer */
  
    /* finding the last buffer in the packet*/    
    pkt = packet;

    /* packet is already checked to be non-null */
    while (pkt->next)
    {
        pkt = pkt->next;
    }

    /* packetWordIndex is the index of the word to start padding */
    packetWordIndex = (frameSize - (numBuffer - 1) * FM_BUFFER_SIZE_BYTES - 1) / 4;

    /* packetWordLastByte is the index of the byte in word packetWordIndex 
       to start padding*/

    packetWordLastByte = (frameSize - (numBuffer - 1) * FM_BUFFER_SIZE_BYTES - 1) % 4;

    /* the mask */
    paddingMask = (0xFFFFFFFF << (8 * (3 - packetWordLastByte)) );

    pkt->data[packetWordIndex] &=  htonl(paddingMask);
   
    /* zero out the rest of the last buffer */ 
    for ( i = packetWordIndex + 1 ; i < FM_BUFFER_SIZE_WORDS ; i++ )
    {
        pkt->data[i] = 0;
    }

    /* at this point pkt->next == NULL */
    if (numBuffer == numTargetBuffer)
    {
        pkt->len = targetSize - FM_BUFFER_SIZE_BYTES * (numTargetBuffer - 1);
    }
    else
    {
        pkt->len = FM_BUFFER_SIZE_BYTES;
    }

    /* allocate and zero out the additional buffers */
    for ( i = 0 ; i < numTargetBuffer - numBuffer ; i++ )
    {
        pkt->next = fmAllocateBuffer(sw);

        if (!pkt->next)
        {
            return FM_ERR_NO_MEM; 
        }

        memset(pkt->next->data, 0, FM_BUFFER_SIZE_BYTES);

        pkt = pkt->next;
        
        pkt->next = NULL;

        if ( i < numTargetBuffer - numBuffer - 1)
        {
            pkt->len = FM_BUFFER_SIZE_BYTES;
        }
        else
        {
            pkt->len = targetSize - (numTargetBuffer - 1) * FM_BUFFER_SIZE_BYTES;
        }
    }

    return FM_OK;

}   /* end PaddingFrame */




/*****************************************************************************/
/** fm2000LCISendPacketInt
 * \ingroup intPlatformCommon
 *
 * \desc            Called to add a packet to the TX packet queue.
 *
 * \param[in]       sw is the switch on which to send the packet.
 *
 * \param[in]       info is a pointer to associated information about
 *                  the packet including where it is going.
 *
 * \param[in]       destMask contains the destination port mask for the frame.
 *
 * \param[in]       packet is a pointer to a chain of fm_buffer structures
 *                  containing the payload.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
static fm_status fm2000LCISendPacketInt(fm_int         sw,
                                        fm_packetInfo *info,
                                        fm_uint        destMask,
                                        fm_buffer *    packet)
{
    fm_packetQueue *        txQueue;
    fm_packetEntry *        entry;
    fm_int                  packetLength;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw = %d, "
                 "info->destMask = 0x%08x, "
                 "info->vlanId = %d, "
                 "info->vlanPriority = %d, "
                 "info->useEgressRules = %s, "
                 "destMask = 0x%08x, "
                 "packet->index = %d\n",
                 sw,
                 info->destMask,
                 info->vlanId,
                 info->vlanPriority,
                 FM_BOOLSTRING(info->useEgressRules),
                 destMask,
                 packet->index);

    packetLength = fm2000ComputeTotalPacketLength(packet);

    txQueue = &fmRootPlatform->fmPlatformState[sw].packetState.txQueue;
    entry = &txQueue->packetQueueList[txQueue->pushIndex];

    /* copy over packet info */
    memcpy( &entry->info, info, sizeof(fm_packetInfo) );
    entry->packet        = packet;
    entry->info.destMask = destMask;
    entry->length        = packetLength;

    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                 "fm2000LCISendPacket: packet queued "
                 "in slot %d, length %d bytes\n",
                 txQueue->pushIndex,
                 entry->length);


    /* check if the Tx queue is full */
    if ( ((txQueue->pushIndex + 1) % FM_PACKET_QUEUE_SIZE) == txQueue->pullIndex )
    {
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX, 
                     "fm2000LCISendPacketInt:"
                     "TX queue is full?: pushIndex = %d, pullIndex = %d\n",
                     txQueue->pushIndex, txQueue->pullIndex);

        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_TX_PACKET_QUEUE_FULL);
    }
    else
    {
        /* updated indices */
        txQueue->pushIndex = (txQueue->pushIndex + 1) % FM_PACKET_QUEUE_SIZE;
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);
    }

}   /* end fm2000LCISendPacketInt */




/*****************************************************************************/
/** EventFreeHandler
 * \ingroup intPlatformCommon
 *
 * \desc            Handles notification when a event free is available to
 *                  continue processing receiving packets.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          None
 *
 *****************************************************************************/
static void EventFreeHandler(fm_int sw)
{
    /**************************************************
     * We must take a lock before writing
     * intrReceivePackets because the lock is used
     * by the API to ensure an atomic read-modify-write
     * to intrReceivePackets.
     *
     * The platform lock is used instead of state lock
     * because on FIBM platforms, there is an access
     * to intrSendPackets that must be protected before
     * the switch's locks are even created. 
     **************************************************/
    
    FM_TAKE_PKT_INT_LOCK(sw);
    GET_SWITCH_PTR(sw)->intrReceivePackets = TRUE;
    FM_DROP_PKT_INT_LOCK(sw);

    /* Wake up the interrupt handler so it will see the message. */
    fmPlatformTriggerInterrupt(sw, FM_INTERRUPT_SOURCE_API);

}   /* end EventFreeHandler */



/*****************************************************************************
 * Public Functions
 *****************************************************************************/


/*****************************************************************************/
/** fm2000LCIPacketHandlingInitialize
 * \ingroup intPlatformCommon
 *
 * \desc            Performs initialization for the generic LCI packet transfer
 *                  module for the FM2000 series.  This starts the send thread
 *                  that monitors when packets are added to the queue
 *
 * \param[in]       sw refers to the switch number to initialize for.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fm2000LCIPacketHandlingInitialize(fm_int sw)
{
    fm_packetHandlingState *ps = &fmRootPlatform->fmPlatformState[sw].packetState;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "sw = %d\n", sw);

    /* clear out all state */
    memset( ps, 0, sizeof(fm_packetHandlingState) );

    fmPacketQueueInit(&ps->txQueue, sw);

    /* reset state here */
    ps->sendOffset       = FM_PACKET_OFFSET_HEADER0;
    ps->sendBufferOffset = 0;
    ps->currentRecvEvent = NULL;
    ps->recvBufferOffset = 0;
    ps->cachedEndianness = -1;

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);

}   /* end fm2000LCIPacketHandlingInitialize */




/*****************************************************************************/
/** fm2000LCISendPacketDirected
 * \ingroup int
 *
 * \desc            Sends a packet over the CPU interface in the directed mode.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       portList points to an array of logical port numbers the 
 *                  switch is to send the packet.
 *
 * \param[in]       numPorts is the number of elements in portList.
 *
 * \param[in]       pkt points to the packet buffer's first ''fm_buffer''
 *                  structure in a chain of one or more buffers.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_PORT_STATE if no port in portList is in the
 *                  correct port state or stp states
 * \return          FM_ERR_INVALID_PORT if portList contains invalid port 
 *                  number.
 * \return          FM_ERR_BAD_IOCTL if the transmit packet queue is full.
 * \return          FM_ERR_FRAME_TOO_LARGE if the packet is too long.
 *
 *****************************************************************************/
fm_status fm2000LCISendPacketDirected(fm_int sw, 
                                      fm_int *portList,
                                      fm_int numPorts, 
                                      fm_buffer *pkt)
{
    fm_switch *             switchPtr = GET_SWITCH_PTR(sw);
    fm_packetQueue *        txQueue;
    fm_int                  i;
    fm_int                  packetLength;
    fm_packetEntry *        entry;
    fm_uint32               destMask;
    fm_uint32               logLinkUpDestMask;
    fm_uint32               frameTag;
    fm_uint32               frameTagType;
    fm_uint32               outVlanId;
    fm_uint32               serdesTestMode;
    fm_uint32               lbMask = 0;
    fm_int                  port;
    fm_int                  mirrorPort;
    fm_int                  stpState;
    fm_int                  lp;
    fm_status               err = FM_OK;
    fm_macaddr              destMacAddress;
    fm_int                  cpuMinFrameSize;
    fm_int                  cpuMaxFrameSize;
    fm_bool                 allowDirectSendToCpu = TRUE;
    fm_bool                 isCurrentlyInLoopback;
    fm_stpMode              spanningTreeMode;
    fm_int                  cpuPort;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "sw=%d portList=%p numPort=%d "
                    "pkt=%p\n", sw, (void *)portList, numPorts, (void *) pkt);

    for (i = 0 ; i < numPorts ; i++)
    {
        if ( !fmIsCardinalPort(sw, portList[i]) )  
        {
            FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_PORT);
        }
    }

    err = fmGetCpuPort(sw, &cpuPort);
    if (err != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
    }


    err = fm2000GetPortAttribute(sw, 
                                 cpuPort, 
                                 FM_PORT_ACTIVE_MAC,
                                 0,
                                 FM_PORT_MIN_FRAME_SIZE, 
                                 &cpuMinFrameSize);

    if (err != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
    }

    err = fm2000GetPortAttribute(sw, 
                                 cpuPort, 
                                 FM_PORT_ACTIVE_MAC,
                                 0,
                                 FM_PORT_MAX_FRAME_SIZE, 
                                 &cpuMaxFrameSize);

    if (err != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
    }

    packetLength = fm2000ComputeTotalPacketLength(pkt);

    if (packetLength <= 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    if (packetLength > cpuMaxFrameSize - 4)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_FRAME_TOO_LARGE);
    }
    /*************************************************************
     * Padding the packets to the min frame size. Four additional 
     * CRC bytes will be added later. 
     ************************************************************/

    if (packetLength < cpuMinFrameSize - 4)
    {
        err = PaddingFrame(sw, pkt, packetLength, cpuMinFrameSize - 4);
        if (err != FM_OK)
        {
            FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
        }
        packetLength = cpuMinFrameSize - 4;
    }

    destMask = 0;
    for (port = 0 ; port < numPorts ; port++)
    {
        destMask |= (1 << portList[port]);
    }

    /* Due to an erratum in A4, ports in loopback are not shown as being up.
     * There may be situations in which it is desirable to send traffic 
     * to a port in loopback mode. */

    /* Remove ports that are down from the destination mask */
    fmMapBitMaskLogicalToLinkUpMask(switchPtr, 
                                    destMask,
                                    &logLinkUpDestMask);

    if (switchPtr->switchVersion == FM_SWITCH_VERSION_FM2224_A0_A4)
    {
        /* re-add ports that are in loopback that were part of the original
         * destMask */
        for (port = 1 ; port <= switchPtr->maxPhysicalPort ; port++)
        {
            
            if ( ( err = switchPtr->ReadUINT32(sw,
                                       FM2000_SERDES_TEST_MODE(port),
                                       &serdesTestMode) ) != FM_OK )
            {
                FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);
            }

            if ( (serdesTestMode & 0x8) )
            {
                isCurrentlyInLoopback = TRUE;
            }
            else
            {
                isCurrentlyInLoopback = FALSE;
            }

            if ( (err = fmMapPhysicalPortToLogical(switchPtr, port, &lp))
                 != FM_OK)
            {
                FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);
            }

            /* port is in loopback and part of the original destMask */
            if ( (isCurrentlyInLoopback) &&
                 (destMask & (1 << lp)) )
            {
                lbMask |= (1 << lp);
            }
        }

        logLinkUpDestMask |= lbMask;
    }

    /* See if traffic to any destination port is being mirrored.  If it is,
     *  add the mirror port(s) to the destination mask */
    for (port = 1 ; port <= switchPtr->maxPhysicalPort ; port++)
    {
        if ( logLinkUpDestMask & (1 << port) )
        {
            mirrorPort = fmGetMirrorPortDest(sw, port, FM_MIRROR_TYPE_EGRESS);

            if (mirrorPort > 0)
            {
                logLinkUpDestMask |= 1 << mirrorPort;
            }
        }
    }
    
    /* Filter the destination mask through the link up mask one more time. */
    fmMapBitMaskLogicalToLinkUpMask(switchPtr,
                                    logLinkUpDestMask,
                                    &logLinkUpDestMask);

    if (switchPtr->switchVersion == FM_SWITCH_VERSION_FM2224_A0_A4)
    {
        /* we still know which ports were in loopback as part of the
         * original destMask */
        logLinkUpDestMask |= lbMask;
    }


    allowDirectSendToCpu = fmGetBoolApiAttribute(
                                FM_AAK_API_DIRECT_SEND_TO_CPU,
                                FM_AAD_API_DIRECT_SEND_TO_CPU);

    if (!allowDirectSendToCpu)
    {
        /* remove CPU port from mask! */
        logLinkUpDestMask &= ~1;         
    }

    if (!logLinkUpDestMask)
    {
        err = fmFreeBufferChain(sw, pkt);
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
    }

    /********************************************************************** 
     * In shared STP mode, if the packet is not a LACP/802.1x frame, then we
     * do not send it to the destination port in the blocking/disabled state 
     * on the outgoing vlan.
     *********************************************************************/
    if ((err = fm2000GetSwitchAttribute(0, 
                                        FM_SPANNING_TREE_MODE, 
                                        &spanningTreeMode)) != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
    }

    destMacAddress = fmGetPacketDestAddr(sw, pkt);

    if ( (spanningTreeMode == FM_SPANNING_TREE_SHARED)
        && (destMacAddress != FM_LACP_DEST_ADDRESS )
        && (destMacAddress != FM_DOT1X_DEST_ADDRESS ) )
    {
        frameTag = ntohl(pkt->data[FM_PACKET_OFFSET_ETHERTYPE]);

        frameTagType = (frameTag >> 16 ) & 0xffff;

        /* Check if the frame has a vlan tag */
        if ( (frameTagType != FM_VLAN_TAG_TYPE_8100)
             && (frameTagType != FM_VLAN_TAG_TYPE_9100)
             && (frameTagType != FM_VLAN_TAG_TYPE_9200) )
        {
            err = fm2000GetPortAttribute(sw, 
                                         cpuPort, 
                                         FM_PORT_ACTIVE_MAC,
                                         0,
                                         FM_PORT_DEF_VLAN, 
                                         &outVlanId);
            if (err != FM_OK)
            {
                FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
            }
        }
        else
        {
            /* frame is tagged */
            outVlanId = frameTag & 0xfff;
        }
        for (port = 1 ; port <= switchPtr->maxPhysicalPort ; port++)
        {
            if ( logLinkUpDestMask & (1 << port) )
            {
                err = fmGetVlanPortStateInternal(sw,
                                                 (fm_uint16) outVlanId,
                                                 port,
                                                 &stpState);
                
                if (err != FM_OK)
                {
                    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
                }

                if ( (stpState == FM_STP_STATE_BLOCKING) ||
                     (stpState == FM_STP_STATE_DISABLED) )
                {
                    logLinkUpDestMask &= ~(1 << port);
                }
            }

        }

    }

    if (!logLinkUpDestMask)
    {
        err = fmFreeBufferChain(sw, pkt);
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
    }

    /* convert destination mask from logical to physical */
    fmMapBitMaskLogicalToPhysical(switchPtr,
                                  logLinkUpDestMask,
                                  &destMask);

    txQueue = &fmRootPlatform->fmPlatformState[sw].packetState.txQueue;

    fmPacketQueueLock(txQueue);

    entry = &txQueue->packetQueueList[txQueue->pushIndex];

    /* Initialize the packet info struct*/ 
    memset( &entry->info, 0, sizeof(fm_packetInfo) );
    entry->packet             = pkt;
    entry->info.destMask      = destMask;
    entry->length             = packetLength;

    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                 "fm2000LCISendPacketDirected: packet queued "
                 "in slot %d, length %d bytes\n",
                 txQueue->pushIndex,
                 entry->length);

    err = fmPacketQueueUpdate(txQueue);
    if (err != FM_OK)
    {
        fmPacketQueueUnlock(txQueue);
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
    }

    fmPacketQueueUnlock(txQueue);

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

    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
            "fm2000LCISendPacketDirected: triggering interrupt handler\n");

    err = fmPlatformTriggerInterrupt(sw, FM_INTERRUPT_SOURCE_API);
    /* Handle the following error case cleanly! */
    if (err != FM_OK)
    {
        /*************************************************************** 
         * We are here because sem_post() fails. In this case we can not
         * cleanly unwind the TX queue to recover, so we simply log a
         * fatal error, and return FM_OK.
         **************************************************************/
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, "fm2000LCISendPacketDirected: " 
                     "fmPlatformTriggerInterrupt returned error");
        
        err = FM_OK;
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end fm2000LCISendPacketDirected */




/*****************************************************************************/
/** fm2000LCISendPacketSwitched
 * \ingroup int
 *
 * \desc            Sends a packet over the CPU interface in the switched mode.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       pkt points to the packet buffer's first ''fm_buffer''
 *                  structure in a chain of one or more buffers.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_BAD_IOCTL if the transmit packet queue is full.
 * \return          FM_ERR_FRAME_TOO_LARGE if the packet is too long.
 *
 *****************************************************************************/
fm_status fm2000LCISendPacketSwitched(fm_int sw, fm_buffer *pkt)
{
    fm_switch *             switchPtr = GET_SWITCH_PTR(sw);
    fm_packetQueue *        txQueue;
    fm_int                  packetLength;
    fm_packetEntry *        entry;
    fm_int                  cpuMinFrameSize;
    fm_int                  cpuMaxFrameSize;
    fm_int                  cpuPort;
    fm_status               err = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "sw=%d pkt=%p\n", sw, (void *) pkt);

    err = fmGetCpuPort(sw, &cpuPort);
    if (err != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
    }


    err = fm2000GetPortAttribute(sw, 
                                 cpuPort, 
                                 FM_PORT_ACTIVE_MAC,
                                 0,
                                 FM_PORT_MIN_FRAME_SIZE, 
                                 &cpuMinFrameSize);

    if (err != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
    }

    err = fm2000GetPortAttribute(sw, 
                                 cpuPort, 
                                 FM_PORT_ACTIVE_MAC,
                                 0,
                                 FM_PORT_MAX_FRAME_SIZE, 
                                 &cpuMaxFrameSize);

    if (err != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
    }

    packetLength = fm2000ComputeTotalPacketLength(pkt);

    if (packetLength <= 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    if (packetLength > cpuMaxFrameSize - 4)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_FRAME_TOO_LARGE);
    }

    /******************************************************************** 
     * Padding the packets to the min frame size.  4 additional CRC bytes 
     * will be added later 
     ********************************************************************/
    if (packetLength < cpuMinFrameSize - 4)
    {
        err = PaddingFrame(sw, pkt, packetLength, cpuMinFrameSize - 4);
        if (err != FM_OK)
        {
            FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
        }
        packetLength = cpuMinFrameSize - 4;
    }

    txQueue = &fmRootPlatform->fmPlatformState[sw].packetState.txQueue;

    fmPacketQueueLock(txQueue);

    entry = &txQueue->packetQueueList[txQueue->pushIndex];

    memset( &entry->info, 0, sizeof(fm_packetInfo) );
    entry->packet             = pkt;
    entry->info.destMask      = 0;
    entry->length             = packetLength;

    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                 "fm2000LCISendPacketSwitched: packet queued "
                 "in slot %d, length %d bytes\n",
                 txQueue->pushIndex,
                 entry->length);

    err = fmPacketQueueUpdate(txQueue);
    if (err != FM_OK)
    {
        fmPacketQueueUnlock(txQueue);
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
    }

    fmPacketQueueUnlock(txQueue);

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

    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                 "fm2000LCISendPacketSwitched: triggering interrupt handler\n");

    err = fmPlatformTriggerInterrupt(sw, FM_INTERRUPT_SOURCE_API);
    /* Handle the following error case cleanly! */
    if (err != FM_OK)
    {
        /*************************************************************** 
         * We are here because sem_post() fails. In this case we can not
         * cleanly unwind the TX queue to recover, so we simply log a
         * fatal error, and return FM_OK.
         **************************************************************/
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, "fm2000LCISendPacketSwitched: " 
                     "fmPlatformTriggerInterrupt returned error");
        err = FM_OK;
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end fm2000LCISendPacketSwitched */




/*****************************************************************************/
/** fm2000LCISendPacket
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
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm2000LCISendPacket(fm_int         sw,
                              fm_packetInfo *info,
                              fm_buffer *    packet)
{
    fm_switch *             switchPtr = GET_SWITCH_PTR(sw);
    fm_packetQueue *        txQueue;
    fm_status               err = FM_OK;
    fm_uint64               vidEntry;
    fm_uint64               tagMask;
    fm_uint32               egressVlanDestMask;
    fm_uint32               rawDestMask;
    fm_int                  cpuMinFrameSize;
    fm_int                  cpuMaxFrameSize;
    fm_int                  cpuPort;
    fm_int                  port;
    fm_int                  packetLength;
    fm_packetInfo           rawInfo;
    fm_buffer *             dupBuffer;
    fm_int                  oldPushIndex;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw = %d, "
                 "info->destMask = 0x%08x, "
                 "info->vlanId = %d, "
                 "info->vlanPriority = %d, "
                 "info->useEgressRules = %s, "
                 "packet->index = %d\n",
                 sw,
                 info->destMask,
                 info->vlanId,
                 info->vlanPriority,
                 FM_BOOLSTRING(info->useEgressRules),
                 packet->index);

    err = fmGetCpuPort(sw, &cpuPort);
    if (err != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
    }

    err = fm2000GetPortAttribute(sw, 
                                 cpuPort, 
                                 FM_PORT_ACTIVE_MAC,
                                 0,
                                 FM_PORT_MIN_FRAME_SIZE, 
                                 &cpuMinFrameSize);

    if (err != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
    }

    err = fm2000GetPortAttribute(sw, 
                                 cpuPort, 
                                 FM_PORT_ACTIVE_MAC,
                                 0,
                                 FM_PORT_MAX_FRAME_SIZE, 
                                 &cpuMaxFrameSize);

    if (err != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
    }

    packetLength = fm2000ComputeTotalPacketLength(packet);

    if (packetLength <= 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    if (packetLength > cpuMaxFrameSize - 4)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_FRAME_TOO_LARGE);
    }
    /************************************************************** 
     * Padding the packets to the min frame size. 4 additional CRC 
     * bytes will be added later. 
     *************************************************************/
    if (packetLength < cpuMinFrameSize - 4)
    {
        err = PaddingFrame(sw, packet, packetLength, cpuMinFrameSize - 4);
        if (err != FM_OK)
        {
            FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
        }
    }

    /* if egress rules are to be applied, check for destination ports with
     *  egress-tagging enabled. Build raw and egress dest masks */
    if (info->useEgressRules)
    {
        egressVlanDestMask = 0;
        rawDestMask        = 0;
        port               = 1;

        /* read in the VID table entry for the specified vlan */
        err = switchPtr->ReadUINT64(sw,
                                    FM2000_VID_TABLE(info->vlanId, 0),
                                    &vidEntry);

        if (err != FM_OK)
        {
            /* unable to read vid table, just ignore the egress rule request */
            port        = switchPtr->maxPhysicalPort + 1;
            rawDestMask = info->destMask;
        }

        while ( ( port = fmFindNextPortInMask(sw, info->destMask, port) ) > 0 )
        {
            tagMask = FM_LITERAL_64(1) << ( 14 + (port * 2) );

            /* found a destination port, is egress tagging active? */
            /* check the tag bit in the VID entry for the port */
            if (vidEntry & tagMask)
            {
                /* yes, add this port to the egressDestMask */
                egressVlanDestMask |= 1 << port;
            }
            else
            {
                /* no, add this port to the rawDestMask */
                rawDestMask |= 1 << port;
            }

            port++;
        }

        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                     "egressVlanDestMask=0x%x rawDestMask=0x%x\n",
                     egressVlanDestMask, rawDestMask);
    }
    else
    {
        rawDestMask        = info->destMask;
        egressVlanDestMask = 0;
    }

    txQueue = &fmRootPlatform->fmPlatformState[sw].packetState.txQueue;

    fmPacketQueueLock(txQueue);

    /***********************************************************
     * oldPushIndex records the current push index in the TX
     * queue. We use it to keep tab on where we started upon 
     * entering this function, in case of the need for roll back
     * on the push index when (1) the tx queue is full; or (2) 
     * the function calls returns an error which we return to the 
     * user application, after having enqued some entries in the
     * tx queue. 
     **********************************************************/
    oldPushIndex = txQueue->pushIndex;

    if (egressVlanDestMask != 0)
    {

        dupBuffer = fmDuplicateBufferChain(sw, packet);

        if (dupBuffer != NULL)
        {
            err = fm2000LCISendPacketInt(sw,
                                         info,
                                         egressVlanDestMask,
                                         dupBuffer);
            if (err != FM_OK)
            {
                fmFreeBufferChain(sw, dupBuffer); 
                fmPacketQueueUnlock(txQueue);
                FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
            }
        }
        else
        {
            fmPacketQueueUnlock(txQueue);
            FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_NO_MEM);
        }
    }

    if ( (rawDestMask != 0) || (egressVlanDestMask == 0) )
    {
        /* this block covers both the directed mode and the switched mode */
        memcpy(&rawInfo, info, sizeof(fm_packetInfo));
        if (rawDestMask != 0)
        {
            /* the direct send mode */
            rawInfo.useEgressRules = FALSE;
        }

        err = fm2000LCISendPacketInt(sw, &rawInfo, rawDestMask, packet);
        if (err != FM_OK)
        {
            /******************************************************************** 
             * The TX queue is full. Unwind the (possibly) queued TX queue entry
             * for dupBuffer.
             *******************************************************************/
            txQueue->pushIndex = oldPushIndex;
            fmPacketQueueUnlock(txQueue);
            FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
        }
    }
    else
    {
        /* If no port is untagged in info->destMask, free the packet buffer */
        fmFreeBufferChain(sw, packet);
    }

    fmPacketQueueUnlock(txQueue);

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

    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                 "fm2000LCISendPackets: triggering interrupt handler\n");

    err = fmPlatformTriggerInterrupt(sw, FM_INTERRUPT_SOURCE_API);
    /* Handle the following error case cleanly! */
    if (err != FM_OK)
    {
        /*************************************************************** 
         * We are here because sem_post() fails. In this case we can not
         * cleanly unwind the TX queue to recover, so we simply log a
         * fatal error, and return FM_OK.
         **************************************************************/
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, "fm2000LCISendPacket: " 
                     "fmPlatformTriggerInterrupt returned error");

        err = FM_OK; 
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end fm2000LCISendPacket */




/*****************************************************************************/
/** fm2000LCISendPackets
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
fm_status fm2000LCISendPackets(fm_int sw)
{
    fm_status               err = FM_OK;
    fm_switch *             switchPtr = GET_SWITCH_PTR(sw);
    fm_packetHandlingState *ps = &fmRootPlatform->fmPlatformState[sw].packetState;
    fm_packetQueue *        txQueue;
    fm_packetEntry *        packet;
    fm_int                  numWords;
    fm_uint32               value;
    fm_uint32               status;
    fm_buffer *             oldBuffer;
    fm_macaddr              destMacAddress;
    fm_bool                 sentVlanReserved = FALSE;
    fm_bool                 isFrameTagged = TRUE;
    fm_uint32               frameTag;
    fm_int                  vlanMode;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "sw = %d\n", sw);
    
    /**************************************************
     * Get the VLAN mode. We'll need this information
     * later on.
     **************************************************/
    
    fm2000GetSwitchAttribute(sw, FM_VLAN_TYPE, &vlanMode);

    txQueue = &fmRootPlatform->fmPlatformState[sw].packetState.txQueue;

    fmPacketQueueLock(txQueue);

    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                 "fm2000LCISendPackets(%d)\n", sw);

    for ( ;
          txQueue->pullIndex != txQueue->pushIndex ;
          txQueue->pullIndex = (txQueue->pullIndex + 1) % FM_PACKET_QUEUE_SIZE)
    {
        packet = &txQueue->packetQueueList[txQueue->pullIndex];
        sentVlanReserved = FALSE;

        destMacAddress = fmGetPacketDestAddr(sw, packet->packet);

        /* Can we send data? */
        if ( !POLL_TX_READY(sw, FM_LCI_POLL_COUNT, &status) )
        {
            goto UNMASK;
        }

        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                     "fm2000LCISendPackets: sending packet in "
                     "slot %d, length=%d off=%d boff=%d\n",
                     txQueue->pullIndex, packet->length, ps->sendOffset,
                     ps->sendBufferOffset);

        numWords = (fm_int)
                   (packet->length >> 2) + ( (packet->length % 4) ? 1 : 0 );

        if (ps->sendOffset == FM_PACKET_OFFSET_HEADER0)
        {
            /***************************************************************** 
             * Write frame length to LCI_TX_FIFO, adding 4 bytes
             * for the CRC word length. In addition, if an egress vlan tag
             * is to be injected into the frame, add an additional 4 bytes. 
             ****************************************************************/
            value = packet->length + 4;

            if (packet->info.useEgressRules
                || (destMacAddress == FM_LACP_DEST_ADDRESS) 
                || (destMacAddress == FM_DOT1X_DEST_ADDRESS ))
            {
                frameTag = ntohl(packet->packet->data[FM_PACKET_OFFSET_ETHERTYPE]);

                frameTag = (frameTag >> 16 ) & 0xffff;

                /* we check if the frame has a vlan tag */
                if (frameTag != FM_VLAN_TAG_TYPE_8100 &&
                    frameTag != FM_VLAN_TAG_TYPE_9100 &&
                    frameTag != FM_VLAN_TAG_TYPE_9200)
                {
                    value        += 4;
                    isFrameTagged = FALSE;
                    
                    /**************************************************
                     * If this is an LACP or 802.1x frame and we are
                     * not in 802.1q mode, then back out the length
                     * increase for VLAN tag since we will not be
                     * inserting one after all.
                     **************************************************/
                    
                    if ( (vlanMode != FM_VLAN_MODE_8021Q) &&
                         ( (destMacAddress == FM_LACP_DEST_ADDRESS) ||
                           (destMacAddress == FM_DOT1X_DEST_ADDRESS) ) )
                    {
                        value -= 4;
                    }
                }
            }


            /* If the mask is set, then enable bit 31 for direct send */
            value |= (packet->info.destMask) ? (1 << 31) : 0;

            err = switchPtr->WriteUINT32(sw, FM2000_LCI_TX_FIFO, value);

            if (err != FM_OK)
            {
                goto UNMASK;
            }

            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                         "fm2000LCISendPackets: header word 0: 0x%08x\n",
                         value);

            ps->sendOffset++;

        }   /* end if (ps->sendOffset ... */

        if (ps->sendOffset == FM_PACKET_OFFSET_HEADER1)
        {
            err = switchPtr->WriteUINT32(sw, 
                                         FM2000_LCI_TX_FIFO, 
                                         packet->info.destMask);

            if (err != FM_OK)
            {
                goto UNMASK;
            }

            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                         "fm2000LCISendPackets: header word 1: 0x%08x\n",
                         packet->info.destMask);

            ps->sendOffset++;

            /* Assign current packet buffer */
            ps->currentSendBuffer = packet->packet;
            ps->sendBufferOffset  = 0;

        }   /* end if (ps->sendOffset ... */


        /* iterate through all buffers */
        for ( ; ( ps->sendOffset < numWords ) && ps->currentSendBuffer ; )
        {
            /* iterate through all words in a buffer */
            while ( ( ps->sendOffset < numWords ) &&
                   ( ps->sendBufferOffset < FM_BUFFER_SIZE_WORDS ) )
            {
                if ( !sentVlanReserved 
                     && (ps->sendOffset == FM_PACKET_OFFSET_ETHERTYPE)
                     && ( ( packet->info.useEgressRules && !isFrameTagged )
                            || (destMacAddress == FM_LACP_DEST_ADDRESS) 
                            || (destMacAddress == FM_DOT1X_DEST_ADDRESS) ) )
                {
                    /********************************************************** 
                     * Here we implement a workaround so that LACP and 802.1X 
                     * are sent out untagged. We use the fact that vlan 4095 is
                     * configured so that all member ports are untagging,
                     * we explicitly insert a vlan tag with vlan Id 4095. 
                     *
                     * Note that this only works (and is only necessary) if
                     * we are in 802.1q VLAN mode. 
                     *********************************************************/
                     
                    if ( (destMacAddress == FM_LACP_DEST_ADDRESS) 
                        || (destMacAddress == FM_DOT1X_DEST_ADDRESS) )
                    {
                        /* Make sure we don't come through here again. */
                        sentVlanReserved = TRUE;
                        
                        /* With port-based VLANs, the frame will egress as it 
                         * ingresses and we assume the application did not 
                         * include a VLAN tag in its LACP or 802.1x frame. */
                        if (vlanMode != FM_VLAN_MODE_8021Q)
                        {
                            /* Continue without updating the send offset. */
                            continue;
                        }
                        
                        packet->egressVlanTag = switchPtr->reservedVlan; 
                        packet->egressVlanTag |= (FM_VLAN_TAG_TYPE_8100 << 16);
                    }
                    else
                    {
                        packet->egressVlanTag = packet->info.vlanId;
                        packet->egressVlanTag |= (packet->info.vlanPriority << 12);
                        packet->egressVlanTag |= (FM_VLAN_TAG_TYPE_8100 << 16);
                        packet->info.useEgressRules = FALSE;
                    }

                    packet->egressVlanTag = htonl(packet->egressVlanTag);

                    err = switchPtr->WriteUINT32(sw, 
                                                 FM2000_LCI_TX_FIFO,
                                                 packet->egressVlanTag);

                    if (err != FM_OK)
                    {
                        goto UNMASK;
                    }

                    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                                 "fm2000LCISendPackets: added vlan tag word: "
                                 "0x%08x\n",
                                 packet->egressVlanTag);

                    /**************************************************
                     * Continue without updating the send offset so
                     * that the next time we come through here we skip
                     * this if block and continue sending the rest of
                     * the words of the packet.
                     **************************************************/
                    continue;
                }

                value = ps->currentSendBuffer->data[ps->sendBufferOffset];

                err = switchPtr->WriteUINT32(sw, FM2000_LCI_TX_FIFO, value);

                if (err != FM_OK)
                {
                    goto UNMASK;
                }

                FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                             "fm2000LCISendPackets: data word %d: 0x%08x\n",
                             ps->sendBufferOffset, value);

                ps->sendOffset++;
                ps->sendBufferOffset++;

            }   /* end while ... */

            /* update buffer pointers */
            oldBuffer             = ps->currentSendBuffer;
            ps->currentSendBuffer = ps->currentSendBuffer->next;

            /* free buffer */
            err = fmFreeBuffer(sw, oldBuffer);

            if (err != FM_OK)
            {
                goto UNMASK;
            }

            fmDbgGlobalDiagCountIncr(FM_GLOBAL_CTR_TX_BUFFER_FREES, 1);

            /* reset counter into new buffer */
            ps->sendBufferOffset = 0;

        }   /* end for ( ... */

        FM_LOG_ASSERT(FM_LOG_CAT_PLATFORM, 
                      ps->currentSendBuffer || (ps->sendOffset >= numWords), 
                      "No buffers left!\n");

        /* Time to send the dummy CRC place-holder word? */
        if (ps->sendOffset >= numWords)
        {
            /* write dummy CRC data */
            err = switchPtr->WriteUINT32(sw, FM2000_LCI_TX_FIFO, 0);
            if (err != FM_OK)
            {
                goto UNMASK;
            }

            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                         "fm2000LCISendPackets: crc word %d: 0x%08x\n",
                         ps->sendBufferOffset, 0);
        }

        /* reset counters here */
        ps->sendOffset       = FM_PACKET_OFFSET_HEADER0;
        ps->sendBufferOffset = 0;

        fmDbgDiagCountIncr(sw, FM_CTR_TX_PKT_COMPLETE, 1);
    }

UNMASK:
    /**************************************************
     * Re-enable the TX ready interrupt.
     **************************************************/
    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                 "fm2000LCISendPacket: unmasking TX RDY interrupt\n");

    err = switchPtr->MaskUINT32(sw, 
                                FM2000_LCI_IM, 
                                FM2000_INT_LCI_TX_RDY, 
                                FALSE);

    fmPacketQueueUnlock(txQueue);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end fm2000LCISendPackets */




/*****************************************************************************/
/** fm2000LCIReceivePackets
 * \ingroup intPlatformCommon
 *
 * \desc            Performs a generic LCI packet receive for FM2000 series
 *                  devices
 *
 * \param[in]       sw is the switch to receive packets on
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fm2000LCIReceivePackets(fm_int sw)
{
    fm_status err;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "sw = %d\n", sw);

    FM_NOT_USED(sw);

    err = fmSignalSemaphore(&fmRootApi->packetReceiveSemaphore);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end fm2000LCIReceivePackets */




/*****************************************************************************/
/** fm2000LCIReceivedPacketHandler
 * \ingroup intPlatformCommon
 *
 * \desc            Handles reception of packets.  Triggered by a semaphore
 *                  from fm2000LCIReceivePackets
 *                  devices
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fm2000LCIReceivedPacketHandler(fm_int sw)
{
    fm_status               err = FM_OK;
    fm_switch *             switchPtr;
    fm_portAttr *           portAttr;
    fm_packetHandlingState *ps;
    fm_eventPktRecv *       recvEvent;
    fm_uint32               value;
    fm_uint32               status;
    fm_int                  availableBuffers;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "sw = %d\n", sw);

    switchPtr = GET_SWITCH_PTR(sw);
    value     = 0;

    ps = &fmRootPlatform->fmPlatformState[sw].packetState;

    TAKE_PLAT_LOCK(sw, FM_PLAT_INFO);

    for (fm_int burst = 0 ; burst < FM_RECV_PKT_MAX_BURST ; burst++)
    {
        /* Are we currently receiving a packet? */
        if (ps->currentRecvEvent == NULL)
        {
            /**************************************************
             * Ready to start receiving a packet. Is there
             * (another) one to receive?
             **************************************************/

            err = switchPtr->ReadUINT32(sw, FM2000_LCI_STATUS, &status);

            if (err != FM_OK)
            {
                /* Unable to read LCI status! */
                goto UNMASK;
            }

            if ( !(status & FM2000_LCI_STATUS_RX_RDY) )
            {
                /* No (more) packets to receive. */
                goto UNMASK;
            }

            /* Attempt to allocate an event to be used when this
             * packet completes. */
            FM_LOG_ASSERT(FM_LOG_CAT_PLATFORM, 
                          !ps->currentRecvEvent,
                          "Overwriting existing event in ps->currentRecvEvent!\n");

            ps->currentRecvEvent = fmAllocateEvent(sw,
                                                   FM_EVID_HIGH_PKT_RECV,
                                                   FM_EVENT_PKT_RECV,
                                                   FM_EVENT_PRIORITY_LOW);

            /* High priority event allocation will not block, so in the
             * event that we have no events, hold off on receiving
             * the packet
             */
            if (!ps->currentRecvEvent)
            {
                /* Will get notify when a free event is available */
                fmAddEventFreeNotify(sw, EVENT_FREE_NOTIFY_PKT_INTR, EventFreeHandler);

                fmDbgDiagCountIncr(sw, FM_CTR_RX_OUT_OF_EVENTS, 1);

                /* Do not unmask.  We have to wait until we are notified
                 * that events are free
                 */
                goto ABORT;
            }

            /* Read header word */
            switchPtr->ReadUINT32(sw, FM2000_LCI_RX_FIFO, &value);

            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                         "fm2000LCIReceivePackets: header word 0x%08x\n",
                         value);

            /* Reset local state */
            ps->currentRecvBuffer = NULL;
            ps->recvChainHead     = NULL;
            ps->recvChainTail     = NULL;
            ps->recvBufferOffset  = 0;

            /* Set packet properties from header word */
            recvEvent = &ps->currentRecvEvent->info.fpPktEvent;

            /* Save the switch number */
            recvEvent->switchNum = sw;

            /* Bits 23:18 are the physical source port */
            fmMapPhysicalPortToLogical(switchPtr,
                                       (value >> 18) & 0x3f,
                                       &recvEvent->srcPort);

            /* Bits 11:0 are the VLAN */
            recvEvent->vlan = (value & 0xfff);
            /* Bits 15:12 are the priority */
            recvEvent->priority = ( (value >> 12) & 0xf );
            /* Bits 17:16 are the action */
            recvEvent->action = ( (value >> 16) & 0x3 );

            /* This will be set when we allocate the first buffer */
            recvEvent->pkt = NULL;

            /* FM2000 does not support timestamping. */
            FM_CLEAR(recvEvent->ingressTime);
            recvEvent->rawIngressTime = 0;

            /**************************************************
             * If the native VLAN is the reserved VLAN, then
             * overwrite it with the actual default VLAN
             * for the source port.
             **************************************************/

            if (recvEvent->vlan == switchPtr->reservedVlan)
            {
                portAttr = GET_PORT_ATTR(sw, recvEvent->srcPort);

                recvEvent->vlan = portAttr->defVlan;
            }

            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                         "fm2000LCIReceivePackets: src=%d vlan=%d\n",
                         recvEvent->srcPort, recvEvent->vlan);

        }
        else
        {
            /* We are already in the middle of receiving a packet and have
             * already allocated it, so continue using the same packet. */
            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                         "fm2000LCIReceivePackets: packet in progress\n");

        }

        /* end if(ps->currentRecvEvent == NULL) */

        switchPtr->ReadUINT32(sw, FM2000_LCI_STATUS, &status);

        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                     "fm2000LCIReceivePackets: LCI status = 0x%08x\n", status);

        /* While EOT is not set */
        while ( !(status & FM2000_LCI_STATUS_RX_EOT) )
        {
            /* If we don't already have a buffer allocated... */
            if (ps->currentRecvBuffer == NULL)
            {
                /* get the number of available buffers from the buffer manager */
                fmPlatformGetAvailableBuffers(&availableBuffers);

                /* ...allocate a buffer to receive frame data into. */
                if (availableBuffers > FM_RECV_BUFFER_THRESHOLD)
                {
                    ps->currentRecvBuffer = fmAllocateBuffer(sw);
                }

                /* Did we get a buffer? */
                if (ps->currentRecvBuffer == NULL)
                {
                    /* No buffers available.
                     * Set out of buffers flag.
                     * Increment relavant statistics.
                     */
                    if (availableBuffers > 0)
                    {
                        fmDbgGlobalDiagCountIncr(FM_GLOBAL_CTR_NO_BUFFERS_FOR_RX, 1);
                    }
                    else
                    {
                        fmDbgGlobalDiagCountIncr(FM_GLOBAL_CTR_RX_OUT_OF_BUFFERS, 1);
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
                    ps->recvChainHead = ps->currentRecvBuffer;
                    ps->recvChainTail = ps->currentRecvBuffer;
                    ps->currentRecvEvent->info.fpPktEvent.pkt
                    = ps->recvChainHead;
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

                /* Reset the offset into the buffer at which to place data. */
                ps->recvBufferOffset = 0;

            }

            /* end if (ps->currentRecvBuffer == NULL) */

            /* Have we reached the end of the frame? */
            switchPtr->ReadUINT32(sw, FM2000_LCI_STATUS, &status);

            switchPtr->ReadUINT32(sw, FM2000_LCI_RX_FIFO, &value);

            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                         "fm2000LCIReceivePackets: data word 0x%08x\n",
                         value);

            /* Place data into chunk and advance offset, but not if
             * this is the frame status word. */
            if ( !(status & FM2000_LCI_STATUS_RX_EOT) )
            {
                ps->currentRecvBuffer->data[ps->recvBufferOffset++] = value;
                ps->currentRecvBuffer->len                         += 4;

            }

            /* Have we come to the end of the buffer? */
            if (ps->recvBufferOffset == FM_BUFFER_SIZE_WORDS)
            {
                /* Yes.  Reset the offset. */
                ps->recvBufferOffset = 0;

                /* Indicate we need a new buffer allocated. */
                ps->recvChainTail     = ps->currentRecvBuffer;
                ps->currentRecvBuffer = NULL;

            }

            /* end if(ps->recvBufferOffset == ...) */


        }

        /* end while(!(status & FM2000_LCI_STATUS_RX_EOT)) */

        /**************************************************
         * Make sure we didn't have to get a new buffer
         * just to read the frame status. If so, discard
         * the last buffer since it has nothing in it.
         **************************************************/

        /* ps->curentRecvBuffer can be NULL in the event that the
         * last word of the packet payload fills a buffer and the
         * request for a new buffer fails. When the buffer becomes
         * available, since the next word in the RX_FIFO is the
         * RX_FRAME_STATUS word, a read of the LCI_STATUS returns
         * a set EOT bit. This causes the above while loop being skipped,
         * and no new buffer is allocated. */

        if (ps->currentRecvBuffer == NULL)
        {
            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                         "fm2000LCIReceivePackets: RX_FRAME_STATUS has not been read\n");
            /* need to finish reading the RX_FRAME_STATUS word */
            switchPtr->ReadUINT32(sw, FM2000_LCI_RX_FIFO, &value);
            ps->currentRecvBuffer = ps->recvChainTail;

            if (ps->currentRecvBuffer == NULL)
            {
                FM_LOG_FATAL(FM_LOG_CAT_EVENT_PKT_RX, "recvChainTail is NULL!\n");
                goto ABORT;
            }
        }

        if (ps->currentRecvBuffer->len == 0)
        {
            /* This should not be the only buffer. */
            if (ps->currentRecvBuffer == ps->recvChainHead)
            {
                /* It's the only buffer! Drop the whole event. */
                FM_LOG_ERROR(FM_LOG_CAT_EVENT_PKT_RX,
                             "LCI indicated packet, but with no data!\n");
                fmFreeBufferChain(sw, ps->recvChainHead);
                fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_DROPS_NO_DATA, 1);
                fmReleaseEvent(ps->currentRecvEvent);
                ps->currentRecvEvent  = NULL;
                ps->currentRecvBuffer = NULL;
                ps->recvChainHead     = NULL;
                ps->recvChainTail     = NULL;
                ps->recvBufferOffset  = 0;
                continue;
            }
            else
            {
                /* Drop the last buffer in the chain since it is empty. */
                ps->recvChainTail->next = NULL;
                fmFreeBuffer(sw, ps->currentRecvBuffer);
                ps->currentRecvBuffer = ps->recvChainTail;
            }
        }

        /**************************************************
         * Do not change the length. The frame status was
         * never stored to the buffer and we want to include
         * the CRC in case the application wants to include
         * it in octet counts or examine it.
         **************************************************/

        /* Bits 5:3 of the last word read (the LCI_RX_FRAME_STATUS) are
         * the number of bytes-not-valid in the last frame payload word.
         * Deduct this amount from the packet length. */
        ps->currentRecvBuffer->len -= ( (value >> 3) & 7 );

        /* reinitialize this so we grab the next one later */
        ps->currentRecvBuffer = NULL;
        ps->recvChainHead     = NULL;
        ps->recvChainTail     = NULL;
        ps->recvBufferOffset  = 0;

        fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_COMPLETE, 1);

        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                     "fm2000LCIReceivePackets: packet complete\n");

        /**************************************************
         * Send the event up to the event handler
         **************************************************/

        fm2000LCIProcessPacketReceive(sw, ps->currentRecvEvent);
        ps->currentRecvEvent = NULL;

    }

    /* end for(...) */

UNMASK:
    /**************************************************
     * Re-enable the RX ready interrupt.
     **************************************************/
    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                 "fm2000LCIReceivePackets: unmasking RX RDY interrupt\n");

    switchPtr->MaskUINT32(sw, FM2000_LCI_IM, FM2000_LCI_ENABLE_RX, FALSE);

ABORT:

    DROP_PLAT_LOCK(sw, FM_PLAT_INFO);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM,
                err);

}   /* end fm2000LCIReceivedPacketHandler */




/*****************************************************************************/
/** fm2000LCIProcessPacketReceive
 * \ingroup intPlatformCommon
 *
 * \desc            Processes a complete received packet event and
 *                  tries to send it upward.
 *
 * \param[in]       sw is the switch to send the event for.
 *
 * \param[in]       event is a fully filled event structure.
 *
 * \return          None.
 *
 *****************************************************************************/
static void fm2000LCIProcessPacketReceive(fm_int sw, fm_event *event)
{
    fm_switch *      switchPtr = GET_SWITCH_PTR(sw);
    fm_eventPktRecv *pktEvent  = &event->info.fpPktEvent;
    fm_status        err;
    fm_int           mirrorPort;
    fm_bool          isSecViolation;
    fm_bool          isSpanningTreeInvalid;
    fm_bool          isLacpToBeDropped;
    fm_int           use8021q;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw = %d, event->type = %d, event->sw = %d, event->eventID = %d\n",
                 sw,
                 event->type,
                 event->sw,
                 event->eventID);

    /* If ingress frames on the source port are being mirrored,
     * duplicate the buffer chain and transmit the duplicate
     * frame to the mirror port.
     */
    mirrorPort = fmGetMirrorPortDest(pktEvent->switchNum,
                                     pktEvent->srcPort,
                                     FM_MIRROR_TYPE_INGRESS);

    if (mirrorPort > 0)
    {
        fm_buffer *dupChain;

        dupChain = fmDuplicateBufferChain(pktEvent->switchNum,
                                          pktEvent->pkt);

        if (dupChain != NULL)
        {
            fmSendPacket(pktEvent->switchNum,
                         1 << mirrorPort,
                         dupChain);
        }
    }

    isSecViolation = switchPtr->CheckSecurityViolation(pktEvent, &fmRootApi->eventThread);

    /* only check if we use 801.1q vlan tagging since otherwise the vlan
     * from the received frame, pktEvent->vlan, should be ignored */
    fmGetSwitchAttribute(sw, FM_VLAN_TYPE, &use8021q);

    if (use8021q)
    {
        isSpanningTreeInvalid = fmCheckFIDFilter(sw, pktEvent);
    }
    else
    {
        isSpanningTreeInvalid = FALSE;
    }

    fmCheckLACPFilter(sw, pktEvent, &isLacpToBeDropped);


    /* Determine if this is a security violation */
    if (!isSecViolation && !isSpanningTreeInvalid && !isLacpToBeDropped)
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
    else
    {
        fmFreeBufferChain(sw, (fm_buffer *) pktEvent->pkt);

        if (isSecViolation)
        {
            fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_DROPS_SECURITY, 1);
        }
        else if (isSpanningTreeInvalid)
        {
            fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_DROPS_STP, 1);
        }
        else if (isLacpToBeDropped)
        {
            fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_DROPS_LACP, 1);
        }

        fmReleaseEvent(event);
    }

    FM_LOG_EXIT_VOID(FM_LOG_CAT_PLATFORM);

}   /* end fm2000LCIProcessPacketReceive */




/*****************************************************************************/
/** fm2000PollLCIStatus
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
static fm_bool fm2000PollLCIStatus(fm_int     sw,
                                   fm_int     bit,
                                   fm_int     value,
                                   fm_int     timeout,
                                   fm_uint32 *status)
{
    fm_status  err;
    fm_switch *switchPtr = GET_SWITCH_PTR(sw);
    fm_int     timer     = 0;
    fm_uint32  uvalue    = (fm_uint32) value;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw = %d, bit = %d, value = %d, timeout = %d, status = %p\n",
                 sw,
                 bit,
                 value,
                 timeout,
                 (void *) status);

    do
    {
        err = switchPtr->ReadUINT32(sw, FM2000_LCI_STATUS, status);

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

}   /* end fm2000PollLCIStatus */




/*****************************************************************************/
/** fm2000ComputeTotalPacketLength
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
static fm_int fm2000ComputeTotalPacketLength(fm_buffer *packet)
{
    fm_int length = 0;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "packet = %p\n", (void *) packet);

    while (packet)
    {
        length += packet->len;
        packet  = packet->next;
    }

    FM_LOG_EXIT_CUSTOM(FM_LOG_CAT_PLATFORM, length, "length = %d\n", length);

}   /* end fm2000ComputeTotalPacketLength */
