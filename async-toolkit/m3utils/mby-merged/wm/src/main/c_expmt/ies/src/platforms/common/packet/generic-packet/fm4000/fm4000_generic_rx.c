/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm4000_generic_rx.c
 * Creation Date:   Jan 5, 2009
 * Description:     Generic receive for the FM4000 series
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
    fmSignalSemaphore(&fmRootPlatform->fmPlatformState[sw].packetState.eventsAvailableSignal);
}


/*****************************************************************************
 * Public Functions
 *****************************************************************************/



/*****************************************************************************/
/** fm4000PacketReceiveProcess
 * \ingroup intPlatformCommon
 *
 * \desc            Processing the received packet and enqueue to higher layer.
 *
 * \note            To support FIBM, this function cannot make any function
 *                  calls that read from hardware. This will cause a deadlock
 *                  if a such attempt is made.
 *
 * \note            This function handles for all different interfaces: LCI, 
 *                  netlink, nic, etc. Use flags option if different behaviour
 *                  is needed for a specific interface.
 *
 * \note            This function is also responsible for freeing the buffer
 *                  if needed.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       buffer points to the receive packet buffer. If ISL tag is 
 *                  present in data buffer, the ISL tags should be in 
 *                  network-byte order.
 *
 * \param[in]       pIslTag points to the ISL tag, if not present if data buffer.
 *                  Set to NULL, if ISL tag is present in data buffer. If ISL
 *                  tag is not present in the data buffer, the data buffer must
 *                  have enough space for vlantag to be inserted. This tags
 *                  should be in host-byte order.
 *
 * \param[in]       flags to pass to the function, if any.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fm4000PacketReceiveProcess(fm_int     sw,
                                     fm_buffer *buffer,
                                     fm_uint32 *pIslTag,
                                     fm_uint    flags)
{
    fm_packetHandlingState*       ps;
    fm_status                     err = FM_OK;
    fm_switch *                   switchPtr;
    fm_eventPktRecv *             recvEvent = NULL;
    fm_uint32                     ctagType;
    fm_uint32                     fType;
    fm_uint32                     vType;
    fm_uint32                     user;
    fm_uint32                     stagTypeA;
    fm_uint32                     stagTypeB;
    fm_uint32                     addedTagWord;
    fm_port *                     sPort;
    fm_event *                    currentRecvEvent = NULL;
    fm_bool                       addVlan;
    fm_bool                       cleanupFreeBuffer = TRUE;
    fm_int                        cpuPort;
    fm_uint32                     cpuGlort;
    fm_uint32                     ISLTag[2];
    fm_uint                       srcGlort;
    fm_uint                       dstGlort;
    fm_event                      directEnqueueEvent;
    fm_timestamp                  ts = { 30, 0 };
    fm_switchEventHandler         switchEventHandler;

    FM_NOT_USED(flags);

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_RX, "sw = %d flags = 0x%x\n", sw, flags);

    if ( (sw < 0) || (sw >= FM_MAX_NUM_SWITCHES) )
    {
        fmFreeBufferChain(sw, buffer);
        fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_DROPS_NO_PORT, 1);
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, FM_ERR_INVALID_SWITCH);
    }

    if ( !SWITCH_LOCK_EXISTS(sw) )
    {
        fmFreeBufferChain(sw, buffer);
        fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_DROPS_NO_PORT, 1);
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, FM_ERR_INVALID_SWITCH);
    }

    /* Take read access to the switch lock */
    PROTECT_SWITCH(sw);

    ps = &fmRootPlatform->fmPlatformState[sw].packetState;

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr == NULL)
    {
        fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_DROPS_NO_PORT, 1);

        err = FM_ERR_SWITCH_NOT_UP;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_RX, err);
    }

    if (switchPtr->state < FM_SWITCH_STATE_INIT ||
        switchPtr->state > FM_SWITCH_STATE_GOING_DOWN)
    {
        fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_DROPS_NO_PORT, 1);

        err = FM_ERR_SWITCH_NOT_UP;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_RX, err);
    }

    if (pIslTag)
    {
        /* ISL tag is given separately */
        ISLTag[0] = pIslTag[0];
        ISLTag[1] = pIslTag[1];
    }
    else
    {
        /* ISL tag is included in the data */
        ISLTag[0]   = ntohl(buffer->data[3]);
        ISLTag[1]   = ntohl(buffer->data[4]);
    }

    fType = (ISLTag[0] >> 30 ) & 0x3;

    /**************************************************
     * This is a mgmt message, let fibm handle it
     **************************************************/
    if ( fType == FM_FTYPE_MANAGEMENT )
    {
        fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_FIBM, 1);

        if (switchPtr->ProcessMgmtPacket)
        {
            err = switchPtr->ProcessMgmtPacket(sw, buffer, pIslTag);

            /* fmFibmProcessPktHandler will free the buffer after use */
            cleanupFreeBuffer = FALSE;
        }
        else
        {
            fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_DROPS_NO_PORT, 1);
        }

        goto ABORT;
    }

    fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_COMPLETE, 1);

    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                 "fm4000PacketReceiveProcess: packet complete\n");
    
    fmGetSwitchEventHandler(sw, &switchEventHandler);

    if ( (ps->rxDirectEnqueueing) && 
         (switchEventHandler != fm4000DbgSelfTestEventHandler) )
    {
        directEnqueueEvent.sw       = sw;
        directEnqueueEvent.eventID  = FM_EVID_HIGH_PKT_RECV;
        directEnqueueEvent.type     = FM_EVENT_PKT_RECV;
        directEnqueueEvent.priority = FM_EVENT_PRIORITY_LOW;
        currentRecvEvent = &directEnqueueEvent;
    }
    else
    {
        do 
        {
            currentRecvEvent = fmAllocateEvent(sw,
                                            FM_EVID_HIGH_PKT_RECV,
                                            FM_EVENT_PKT_RECV,
                                            FM_EVENT_PRIORITY_LOW);

            if (!currentRecvEvent)
            {
                /* Will get notify when a free event is available */
                fmAddEventFreeNotify(sw, EVENT_FREE_NOTIFY_PKT_INTR, EventFreeHandler);

                err = fmWaitSemaphore(&ps->eventsAvailableSignal, &ts);

                if (err == FM_ERR_SEM_TIMEOUT)
                {
                    FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                                 "Unable to acquire event object in 30 seconds!\n");
                }
            }
        }
        while (!currentRecvEvent);
    }

    /* retrieve the alternate vlan tagging information for double-tagged
     * frames, using CPU port's configuration */
    err = fmGetCpuPortInt(sw, &cpuPort);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_RX, err);

    if (switchEventHandler != fm4000DbgSelfTestEventHandler)
    {
        VALIDATE_SWITCH_LOCK(sw);

        /* Take read access to the switch lock */
        PROTECT_SWITCH(sw);
        
        err = fm4000GetPortStagType(sw, cpuPort, &stagTypeA, &stagTypeB);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_RX, err);

        UNPROTECT_SWITCH(sw);
    }
    else
    {
        stagTypeA = 0;
        stagTypeB = 0;
    }

    /* Set packet properties from header word */
    recvEvent = &currentRecvEvent->info.fpPktEvent;

    /* Save the switch number */
    recvEvent->switchNum = sw;

    /* This will be set when we allocate the first buffer */
    recvEvent->pkt = buffer;

    /* Store the ISL tag */
    recvEvent->ISLTag[0] = ISLTag[0];
    recvEvent->ISLTag[1] = ISLTag[1];

    /* Store the VLAN, priority and trap code */
    vType                   = (ISLTag[0] >> 28 ) & 0x3;
    user                    = (ISLTag[0] >> 16) & 0xff;
    recvEvent->vlan         = ISLTag[0] & 0xfff;
    /* priority in fm_eventPktRecv struct is documented to
     * contain internal switch priority
     */
    recvEvent->priority     = (ISLTag[0] >> 24) & 0xf;
    recvEvent->vlanPriority = (ISLTag[0] >> 13) & 0x7;
    recvEvent->trapAction   = ISLTag[1] & 0xff;

    /* Validate received port */
    srcGlort = (ISLTag[1] >> 16) & 0xffff;
    dstGlort = ISLTag[1] & 0xffff;
    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                 "ISL: 0x%08x 0x%08x srcGlort = 0x%04x destGlort = 0x%04x user = %d\n",
                 ISLTag[0], ISLTag[1], srcGlort, dstGlort, user);

    /* Workaround for errata #24
     * Use the user bit to save the lag index
     * So the packet receive code can figure
     * out which member port is the packet from.
     */
    {
        /* Assuming the code must set the USR field to zero
         * for non-LAG ports. This avoid calling
         * fm4000GetLogicalPort twice, once to find out if is
         * a LAG and then again to get the correct member port
         */

        /* The user contains the member index */
        srcGlort += user;
    }    /* End of workaround for errata #24 */

    if (switchEventHandler == fm4000DbgSelfTestEventHandler)
    {
        /* The switch is in self-test, all the incoming frames come
         * from the CPU */
        recvEvent->srcPort = 0;

    }
    else if (fmGetGlortLogicalPort(sw, srcGlort, &recvEvent->srcPort) != FM_OK)
    {
        /* The code will return the physical or remote port if found above */

        /* Can't associate with a port on local switch,
         * maybe it from fibm without a remote port created
         */
        if (fmFindSlaveSwitchPortByGlort(srcGlort, &recvEvent->switchNum,
                                         &recvEvent->srcPort) == FM_OK)
        {
            /* Set this for event handler to pass up the correct switch
             * Actually this should be the master switch, but most of
             * the upper code uses this info, instead of switchNum
             */
            currentRecvEvent->sw = recvEvent->switchNum;
        }
        else
        {
            /**************************************************
             * Just drop the packet. A possible reason for
             * fm4000GetLogicalPort to fail is if a packet
             * was received on a LAG, then the LAG was deleted
             * before we pulled it across the LCI.
             **************************************************/
            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                         "fm4000PacketReceiveProcess: "
                         "Unable to get source port from glort 0x%x\n",
                         srcGlort);

            fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_DROPS_NO_PORT, 1);

            goto ABORT;
        }
    }

    /**********************************************************
     * vType == 0  will only happen if we have an untagged
     * trapped frame since in this case the frame handling
     * pipeline uses the incoming
     * data info., ie. this is a untagged frame., and ignore the
     * tagging configuration on the (egress) CPU port.
     *
     * This case will be handled below as we need the default
     *  vlan info. from the srcPort, which is in the next ISL word
     **********************************************************/

    addVlan = FALSE;
    addedTagWord = 0;

    /* Check if the frame is a trapped frame */
    err = fmGetLogicalPortGlort(sw, cpuPort, &cpuGlort);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_RX, err);

    sPort = switchPtr->portTable[recvEvent->srcPort];

    err = fmGetPortAttribute(sw, cpuPort, FM_PORT_PARSER_CVLAN_TAG, &ctagType);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_RX, err);

    if ( (fType == FM_FTYPE_SPECIAL_DELIVERY) 
         && ((dstGlort & 0xFF00 ) == (cpuGlort & 0xFF00))
         && ((dstGlort & 0xFF) <= FM4000_MAX_FRAME_TRAP_CODE) )
    {
        /* handle the trapped frames */
        if (vType == 0)
        {
            if (sPort->portType == FM_PORT_TYPE_REMOTE)
            {
                /* We don't have this info for remote ports */
                recvEvent->vlan = 0;
                recvEvent->vlanPriority = 0;
            }
            else
            {
                fm_uint16 vlan;
                fm_byte   priority;

                fmGetPortDefVlanDefPriorityInt(sw, recvEvent->srcPort, &vlan, &priority);
                recvEvent->vlan = vlan;
                recvEvent->vlanPriority = priority;
            }

            recvEvent->vlanEtherType = ctagType;

        }
        else
        {
            addedTagWord = 0;

            switch (vType)
            {
                case 1:
                    recvEvent->vlanEtherType = ctagType;
                    addedTagWord |= (ctagType << 16);
                    break;

                case 2:
                    recvEvent->vlanEtherType = stagTypeA;
                    addedTagWord |= (stagTypeA << 16);
                    break;
                case 3:
                    recvEvent->vlanEtherType = stagTypeB;
                    addedTagWord |= (stagTypeB << 16);
                    break;

                default:
                    break;

            }   /* end switch (vType) */

            addedTagWord |= ISLTag[0] & 0xffff;

            addVlan = TRUE;
            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                     "fm4000LCIReceivePacketDMA: "
                     "added tag word 0x%08x, vType=%d \n",
                     addedTagWord, vType);
        }
    }
    else
    {
        /********************************************************************
         * Retreive the default VLAN and default Priority of the source port 
         * and configure the fm_eventPktRecv struct accordingly.
         ********************************************************************/
        if (vType == 0)
        {
            fm_uint16 vlan;
            fm_byte   priority;

            fmGetPortDefVlanDefPriorityInt(sw, recvEvent->srcPort, &vlan, &priority);
            recvEvent->vlan = vlan;
            recvEvent->vlanPriority = priority;
        }

        if (((fm4000_vlanEntry *) (switchPtr->vidTable[recvEvent->vlan].vlanExt))->cpuTagging)
        {
            addedTagWord = 0;

            switch (vType)
            {
                case 1:
                    recvEvent->vlanEtherType = ctagType;
                    addedTagWord |= (ctagType << 16);
                    break;

                case 2:
                    recvEvent->vlanEtherType = stagTypeA;
                    addedTagWord |= (stagTypeA << 16);
                    break;

                case 3:
                    recvEvent->vlanEtherType = stagTypeB;
                    addedTagWord |= (stagTypeB << 16);
                    break;

                default:
                    break;

            }   /* end switch (vType) */

            addedTagWord |= ISLTag[0] & 0xffff;

            addVlan = TRUE;
            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                     "fm4000LCIReceivePacketDMA: "
                     "added tag word 0x%08x, vType=%d \n",
                     addedTagWord, vType);
        }
        else
        {
            switch(vType)
            {
                case 1:
                    recvEvent->vlanEtherType = ctagType;
                    break;
                case 2:
                    recvEvent->vlanEtherType = stagTypeA;
                    break;
                case 3:
                    recvEvent->vlanEtherType = stagTypeB;
                    break;
                default:
                    break;
            }
        }
    }

    if (pIslTag)
    {
        /* No ISL Tag in data */
        if (addVlan)
        {
            /****************************************************************
             * Add a VLAN tag. We know there is room at the front since that
             * is where the netlink header is located. Move the mac addresses
             * back a word and then insert the VLAN tag.
             ****************************************************************/
            buffer->data--;
            buffer->len += 4;
            buffer->data[0] = buffer->data[1];
            buffer->data[1] = buffer->data[2];
            buffer->data[2] = buffer->data[3];
            buffer->data[3] = htonl(addedTagWord);
        }
    }
    else
    {
        if (addVlan)
        {
            /******************************************
             * Add a VLAN tag and remove the ISL Tag. 
             ******************************************/
            buffer->data[4] = htonl(addedTagWord);
            buffer->data[3] = buffer->data[2];
            buffer->data[2] = buffer->data[1];
            buffer->data[1] = buffer->data[0];
            buffer->data    += 1;
            buffer->len     -= 4;
        }
        else
        {
            /******************************************
             * Remove the ISL Tag. 
             ******************************************/
            buffer->data[4] = buffer->data[2];
            buffer->data[3] = buffer->data[1];
            buffer->data[2] = buffer->data[0];
            buffer->data   += 2;
            buffer->len    -= 8;
        }
    }

    /* FM4000 does not support timestamping. */
    FM_CLEAR(recvEvent->ingressTime);
    recvEvent->rawIngressTime = 0;

    /***************************************************
     * The CRC is incorrect since it includes the ISL tags
     * we removed. Making it all zero so the user knows that
     * the CRC should not be used.
     **************************************************/
    fmPacketClearCRC(buffer);
    

    /**************************************************
     * Send the event up to the event handler
     **************************************************/

    if ( ( err = fmPacketReceiveEnqueue(sw, currentRecvEvent,
                                        fm4000DbgSelfTestEventHandler) )
        != FM_OK )
    {
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                     "fm4000PacketReceiveProcess: unable to send event "
                     "to event handler\n");

        /***************************************************
         * fm4000PacketReceiveEnqueue already handles
         * freeing the buffer and the event, so we 
         * simply let the code take the normal
         * exit path.
         **************************************************/
    }
    else
    {
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                     "fm4000PacketReceiveProcess: successfully sent event "
                     "to event handler\n");
    }

    /* Clean exit */
    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, err);

ABORT:
    /* Free the buffer */
    if (cleanupFreeBuffer)
    {
        fmFreeBufferChain(sw, buffer);
    }

    if (!ps->rxDirectEnqueueing && currentRecvEvent)
    {
        /* Free also the event */
        fmReleaseEvent(currentRecvEvent);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, err);

}   /* end fm4000PacketReceiveProcess */

