/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm6000_generic_rx.c
 * Creation Date:   Jan 5, 2009
 * Description:     Generic receive for the FM6000 series
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


#include <fm_sdk_fm6000_int.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define BPDU_DMAC FM_LITERAL_64(0x0180C2000000)

#define EXTRACT_DMAC(buf) \
    (((fm_uint64) ntohl((buf)->data[0])) << 16) | \
     ((ntohl((buf)->data[1]) >> 16))

/*****************************************************************************
 * Global Variables
 *****************************************************************************/


/*****************************************************************************
 * Local Variables
 *****************************************************************************/


/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/
//FIXME: To be removed once the correct one is defined
void fm6000DbgSelfTestEventHandler(fm_event * event)
{

}   /* end fm6000DbgSelfTestEventHandler */


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
    fmSignalSemaphore(&GET_PLAT_PKT_STATE(sw)->eventsAvailableSignal);
}




/*****************************************************************************/
/** TimestampsEnabled
 * \ingroup intPlatformCommon
 *
 * \desc            Determines whether timestamps are enabled for the
 *                  specified ingress port.
 *
 * \param[in]       sw is the switch on which to operate.
 * 
 * \param[in]       port is the logical port number.
 *
 * \return          TRUE if timestamps are enabled, FALSE otherwise.
 *
 *****************************************************************************/
static fm_bool TimestampsEnabled(fm_int sw, fm_int port)
{
    fm_status   err;
    fm_bool     isEnabled;

    err = fm6000GetPortAttribute(sw,
                                 port,
                                 FM_PORT_ACTIVE_MAC,
                                 FM_PORT_LANE_NA,
                                 FM_PORT_TIMESTAMP_GENERATION,
                                 (void *) &isEnabled);

#if 0
    if (err != FM_OK)
    {
        FM_LOG_PRINT("TimestampsEnabled: "
                     "fmGetPortAttribute(%d,%d): %s\n",
                     sw,
                     port,
                     fmErrorMsg(err));
    }
#endif

    return (err == FM_OK) && isEnabled;

}   /* end TimestampsEnabled */


/*****************************************************************************
 * Public Functions
 *****************************************************************************/



/*****************************************************************************/
/** fm6000PacketReceiveProcess
 * \ingroup intPlatformCommon
 *
 * \desc            Processing the received packet and enqueue to higher layer.
 *
 * \note            To support FIBM, this function cannot make any function
 *                  calls that read from hardware. This will cause a deadlock
 *                  if such an attempt is made.
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
fm_status fm6000PacketReceiveProcess(fm_int     sw,
                                     fm_buffer *buffer,
                                     fm_uint32 *pIslTag,
                                     fm_uint    flags)
{
    fm_packetHandlingState*       ps;
    fm_status                     err = FM_OK;
    fm_switch *                   switchPtr;
    fm_eventPktRecv *             recvEvent = NULL;
    fm_uint32                     fType;
    fm_uint32                     user;
    fm_uint32                     addedTagWord=0;
    fm_event *                    currentRecvEvent = NULL;
    fm_bool                       addVlan;
    fm_bool                       cleanupFreeBuffer = TRUE;
    fm_uint32                     ISLTag[2];
    fm_uint                       srcGlort;
    fm_uint                       dstGlort;
    fm_event                      directEnqueueEvent;
    fm_timestamp                  ts = { 30, 0 };
    fm_switchEventHandler         switchEventHandler;
    fm_int                        stpInstance;
    fm_int                        portState;
    fm_uint64                     dmac;
    fm_int                        cpuPort;
    fm_int                        cpuPhysPort;
    fm_int                        vlan;
    fm_uint16                     pvid;
    fm_uint32                     fcsVal;

    FM_NOT_USED(flags);

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_RX, 
                 "sw = %d pIslTag = %p flags = 0x%x\n", 
                 sw, (void *) pIslTag, flags);

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

    ps = GET_PLAT_PKT_STATE(sw);

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
        ISLTag[0] = ntohl(buffer->data[3]);
        ISLTag[1] = ntohl(buffer->data[4]);
    }

    fType = (ISLTag[0] >> 30 ) & 0x3;
    //fType = (ISLTag[0] >> 28 ) & 0xf;

    /**************************************************
     * This is a mgmt message, let fibm handle it FIXME
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

    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX, "packet complete\n");
    
    fmGetSwitchEventHandler(sw, &switchEventHandler);

    if ( (ps->rxDirectEnqueueing) && 
         (switchEventHandler != fm6000DbgSelfTestEventHandler) )
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
    user            = (ISLTag[0] >> 16) & 0xff;
    vlan            = ISLTag[0] & 0xfff;
    recvEvent->vlan = vlan;

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
                 "ISL: 0x%08x 0x%08x srcGlort=0x%04x destGlort=0x%04x fType=%04x "
                 "user=%d vlan=%d trapAction=%04x\n",
                 ISLTag[0], ISLTag[1], srcGlort, dstGlort, fType, 
                 user, recvEvent->vlan, recvEvent->trapAction);

    if (switchEventHandler == fm6000DbgSelfTestEventHandler)
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
             * fm6000GetLogicalPort to fail is if a packet
             * was received on a LAG, then the LAG was deleted
             * before we pulled it across the LCI.
             **************************************************/
            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                         "Unable to get source port from glort 0x%x\n",
                         srcGlort);

            fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_DROPS_NO_PORT, 1);

            goto ABORT;
        }
    }

    if (vlan)
    {
        recvEvent->vlanEtherType = 0x8100;
    }
    else
    {
        recvEvent->vlanEtherType = 0;
        /* for an untagged frame use the pvid of the ingress port */
        fmGetPortDefVlanInt(sw, recvEvent->srcPort, &pvid);
        vlan = pvid;
    }

    err = fmGetCpuPort(sw, &cpuPort);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_RX, err);

    err = fmMapLogicalPortToPhysical(switchPtr, cpuPort, &cpuPhysPort);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_EVENT_PKT_RX, err);

    /* Add vlan to the packet if the CPU port tagging is enabled */    
    addVlan = FM_PORTMASK_GET_BIT( &((fm6000_vlanEntry *) 
                                    (switchPtr->vidTable[vlan].vlanExt))->tag, cpuPhysPort);

    /*********************************************************
     * Verify the spanning tree state of the ingress port:
     * if the state is disabled, BPDU packets should be
     * dropped at sw level. Behavior can be overriden with the
     * "api.FM6000.trapBpduOnStpDisc" api attribute
     ********************************************************/
    if (!fmGetBoolApiAttribute(FM_AAK_API_FM6000_TRAP_BPDU_ON_STP_DISC,
                               FM_AAD_API_FM6000_TRAP_BPDU_ON_STP_DISC))
    {
        dmac = EXTRACT_DMAC(buffer);

        if (dmac == BPDU_DMAC)
        {
            err = fmFindInstanceForVlan(sw, vlan, &stpInstance);
            if (err == FM_OK)
            {
                err = fmGetSpanningTreePortState(sw, 
                                                 stpInstance, 
                                                 recvEvent->srcPort, 
                                                 &portState);

                if (err == FM_OK)
                {
                    /* Ingress port is disabled and DMAC == BPDU */
                    if (portState == FM_STP_STATE_DISABLED)
                    {
                        /* Dropping BPDU packet */
                        fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_DROPS_NO_PORT, 1);
                        goto ABORT;
                    }
                }
                else
                {
                    FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                                 "Unable to find STP port state for instance "
                                 "%d, port %d\n",
                                 stpInstance,
                                 recvEvent->srcPort);
                }
            }
            else
            {
                FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                             "Unable to find STP instance for vlan %d\n",
                             vlan);
            }
        }
    }

    if (fType == FM_FTYPE_SPECIAL_DELIVERY)
    {
        if (!pIslTag)
        {
            /******************************************
             * Remove the ISL Tag present in the data.  
             ******************************************/
            buffer->data[4] = buffer->data[2];
            buffer->data[3] = buffer->data[1];
            buffer->data[2] = buffer->data[0];
            buffer->data   += 2;
            buffer->len    -= 8;
        }
    }
    else
    {
        /* Normal packet */
        if (addVlan)
        {
            addedTagWord = (0x8100 << 16);
            addedTagWord |= ISLTag[0] & 0xffff;
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
    }

    /*********************************************************
     * Retrieve ingress timestamp from the FCS field of the 
     * buffer, and convert it to a precision timestamp.
     ********************************************************/
    if ( switchPtr->ConvertTimestamp &&
         TimestampsEnabled(sw, recvEvent->srcPort) )
    {
        fcsVal = fmPacketGetCRC(buffer);

        recvEvent->rawIngressTime = ((fcsVal >> 1) & 0x7fffff80) | (fcsVal & 0x7f);

        err = switchPtr->ConvertTimestamp(sw,
                                          recvEvent->rawIngressTime,
                                          0,
                                          &recvEvent->ingressTime);
        if (err != FM_OK && err != FM_ERR_UNSUPPORTED)
        {
            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                         "Unable to convert ingress timestamp\n");
        }

        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                     "srcPort=%d rawIngressTime=0x%08x ingressTime=%llu %llu\n",
                     recvEvent->srcPort,
                     recvEvent->rawIngressTime,
                     recvEvent->ingressTime.seconds,
                     recvEvent->ingressTime.scaledNanoseconds);
    }
    else
    {
        /* Timestamps not supported. */
        FM_CLEAR(recvEvent->ingressTime);
        recvEvent->rawIngressTime = 0;
    }

    /***************************************************
     * The CRC is incorrect since it includes the ISL tags
     * we removed. Making it all zero so the user knows that
     * the CRC should not be used.
     **************************************************/
    fmPacketClearCRC(buffer);

    /**************************************************
     * Send the event up to the event handler
     **************************************************/

    err = fmPacketReceiveEnqueue(sw,
                                 currentRecvEvent,
                                 fm6000DbgSelfTestEventHandler);
    if (err != FM_OK)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                     "Unable to send event to event handler\n");

        /***************************************************
         * fm6000PacketReceiveEnqueue already handles
         * freeing the buffer and the event, so we 
         * simply let the code take the normal
         * exit path.
         **************************************************/
    }
    else
    {
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_RX,
                     "Successfully sent event to event handler\n");
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

}   /* end fm6000PacketReceiveProcess */

