/* vim:et:sw=4:syntax=c:ts=4:tw=79:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            sdk_packet_handling.i
 * Creation Date:   June 1, 2007
 * Description:     TestPoint helper functions to handle packet reception and
 *                  packet sending.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2007 - 2013 Intel Corporation. All Rights Reserved.
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

%{

#include <fm_support.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

/*****************************************************************************
 * Global Variables
 *****************************************************************************/

/* Ensure that a very large number of packets can be buffered. */
#define MAX_LOCAL_RECV_QUEUE        10240   /* #packets */
#define MAX_PKT_SIZE                2560    /* 32-bit words */
#define MIN_TX_PKT_SIZE             32      /* bytes */
#define MAX_TX_PKT_SIZE             15864   /* bytes */

/***************************************************
 * A local buffer to transfer the send payload into
 * from the perl side, to be converted to buffers
 * on the C side.
 **************************************************/
static fm_uint32 sendPayload[MAX_TX_PKT_SIZE / 4];

/***************************************************
 * A local buffer to place received payload bytes
 * into so that the interactive packet reception
 * mechanism on the perl side can access them.
 **************************************************/
static fm_uint32 recvPayload[MAX_LOCAL_RECV_QUEUE][MAX_PKT_SIZE];

/* The associated length for each packet above. */
static int recvLen[MAX_LOCAL_RECV_QUEUE];

/* A copy of the packet reception info for each packet above. */
static fm_eventPktRecv recvInfo[MAX_LOCAL_RECV_QUEUE];

/* This lock protects the state above. */
static fm_lock recvLock;

/* State used to ensure the above lock is initialized the first time */
static fm_bool lockInit = FALSE;

/* Keeps track of where we are in the local queues */
static int nextRecvSlot = 0;
static int nextGrabSlot = -1;

/* Set the LCI port in loopback */
static int lciLoopback = 0;

%}
%inline
%{

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
/** tpPacketPayloadClear
 * \ingroup tpPacket
 *
 * \desc            Clears the sending payload queue that is filled in by the
 *                  perl functions.
 *
 *****************************************************************************/
void tpPacketPayloadClear()
{
    memset(sendPayload, 0, sizeof(sendPayload));
}

/*****************************************************************************/
/** tpInitializePacketHandling
 * \ingroup tpPacket
 *
 * \desc            Initializes the local state for receiving packets and
 *                  creates the lock used to protect the local state.
 *
 *****************************************************************************/
void tpInitializePacketHandling()
{
    nextRecvSlot = 0;
    nextGrabSlot = -1;
    fmCreateLock("Local Packet Queue", &recvLock);
    lockInit = TRUE;
}

/*****************************************************************************/
/** tpSetLCILoopback
 * \ingroup tpSetLCILoopback
 *
 * \desc            Set the LCI port in loopback or not.
 *
 * \param[in]       enable defines the loopback state
 *
 *****************************************************************************/
void tpSetLCILoopback(fm_int enable)
{
    lciLoopback = enable;
}

/*****************************************************************************/
/** tpPacketPayloadSetByte
 * \ingroup tpPacket
 *
 * \desc            Sets a byte within the local payload cache.
 *
 * \deprecated      See ''tpPacketPayloadSet'' for a better performing
 *                  alternative to this function.
 *
 * \param[in]       pos is the position within the cache of the byte to set.
 *
 * \param[in]       value is the byte value to set.
 *
 *****************************************************************************/
void tpPacketPayloadSetByte(int pos, fm_int value)
{
    char * pBuf;

    pBuf = (char*)sendPayload;

    pBuf[pos] = value;
}

/*****************************************************************************/
/** tpPacketPayloadSet
 * \ingroup tpPacket
 *
 * \desc            Copies the specified payload to the local payload cache.
 *
 * \param[in]       n is the number of payload bytes to copy.
 *
 * \param[in]       payload points to a n-entry byte array that contains the
 *                  payload to be copied.
 *
 *****************************************************************************/
void tpPacketPayloadSet(fm_int n, fm_byte *payload)
{
    const size_t bufsiz = sizeof(sendPayload);

    if ((size_t) n > bufsiz)
    {
        n = (fm_int) bufsiz;
        fprintf(stderr,
                "error: transmitted packet truncated to %zd bytes\n",
                bufsiz);
    }

    memcpy(sendPayload, payload, n);
}

/*****************************************************************************/
/** tpRetryingAllocBuffer
 * \ingroup tpPacket
 *
 * \desc            Tries to allocate a buffer and retries if it fails up to
 *                  some limit.
 *
 * \param[in]       sw is the switch number to allocate buffers on
 *
 * \return          The allocated buffer pointer or NULL on failure
 *
 *****************************************************************************/
fm_buffer *tpRetryingAllocBuffer(fm_int sw)
{
    int         nr = 0;
    fm_buffer * ptr;

    for (nr = 0; nr < 32; nr++)
    {
        ptr = fmAllocateBuffer(sw);

        if (ptr)
        {
            return ptr;
        }
        else
        {
            /* wait 0.5s before trying again */
            fmDelay(0, 500000000);
        }
    }

    return NULL;
}

/*****************************************************************************/
/** tpSendPacketHelper
 * \ingroup tpPacket
 *
 * \desc            Sends the currently stored packet on the given list of
 *                  ports with the given length.
 *
 * \param[in]       sw is the switch number to send the packet out on.
 *
 * \param[in]       port points to an array of port numbers.
 *
 * \param[in]       nPorts is the number of entries in the above array.
 *
 * \param[in]       vlan is the vlan to which the packet is sent.
 *
 * \param[in]       directVlan is the vlan to which the packet will be
 *                  replicated to (if port is FM_DIRECT_VLAN_SEND).
 *
 * \param[in]       length is the packet size in bytes.
 *
 *****************************************************************************/
fm_status tpSendPacketHelper(fm_int sw,
                             fm_int *port,
                             fm_int nPorts,
                             fm_int vlan,
                             fm_int directVlan,
                             fm_int length)
{
    fm_status       err;
    fm_packetInfo   info;
    fm_buffer *     pkt = NULL, *ptr;
    int             bytesDone = 0;
    int             chunkLen;
    int             p;

    length -= 4;

    if (length < 0)
    {
        length = 0;
    }

    for (p = 0; p < nPorts; p++)
    {
        bytesDone = 0;
        pkt = NULL;

        while (bytesDone < length)
        {
            if (pkt)
            {
                ptr->next = tpRetryingAllocBuffer(sw);
                ptr = ptr->next;
            }
            else
            {
                pkt = ptr = tpRetryingAllocBuffer(sw);
            }

            if (!ptr)
            {
                printf("Unable to allocate buffers to send packet!\n");

                if (pkt)
                {
                    fmFreeBufferChain(sw, pkt);
                }

                return FM_ERR_NO_MEM;
            }

            chunkLen = ((length - bytesDone) > FM_BUFFER_SIZE_BYTES) ?
                   FM_BUFFER_SIZE_BYTES :
                   (length - bytesDone);

            memcpy(ptr->data, &sendPayload[bytesDone >> 2], chunkLen);

            ptr->len = chunkLen;
            ptr->next = NULL;
            bytesDone += chunkLen;
        }

        memset(&info, 0, sizeof(fm_packetInfo));

        /* handle both 2k & 4k */
        info.useEgressRules = FALSE;
        info.vlanId = vlan;
        info.directSendVlanId = directVlan;
        if (port[p] != FM_DIRECT_VLAN_SEND )
        {
            if (port[p] > 0)
            {
                info.logicalPort = port[p];
                info.destMask = 1 << port[p];
            }
            else if ( vlan != 0 )
            {
                info.useEgressRules = TRUE;
            }
        }
        else
        {
            info.logicalPort = port[p];
            if ( vlan != 0 )
            {
                info.useEgressRules = TRUE;
            }
        }

        /* FIXME: this is switch specific, and we should not be
         * referring to switch specific constants in swig code
         * since swig code implies a certain level of switch
         * agnosticity -NS
         */

        /* currently in TP we always use the vlan priority info.
         * from the packet payload to set up the SWPRI field of
         * the ISL tag */
        info.switchPriority = 0xffff; /* FM4000_USE_VLAN_PRIORITY ; */

        err =  fmSendPacketExt(sw, &info, pkt);
        /* note we can't do direct vlan send using fmSendPacket */
        /* err = fmSendPacket(sw, (fm_int) info.destMask, pkt); */
        /* err = fmSendPacketDirected(sw, &port[p], 1, pkt); */

        if (err != FM_OK)
        {
            printf("ERROR: unable to send packet: %s\n", fmErrorMsg(err));
            tpPacketPayloadClear();
            err = fmFreeBufferChain(sw, pkt);
        }
    }

    tpPacketPayloadClear();

    return FM_OK;
}

/*****************************************************************************/
/** tpSendPacketHelperL2
 * \ingroup tpPacket
 *
 * \desc            Sends an L2 packet to the CPU port. The L2 packet
 *                  is either send "directed" or "switched" depending
 *                  if the port is specified or not.
 *
 * \param[in]       sw is the switch number to send the packet out on.
 *
 * \param[in]       port points to the port to send the packet to. If the
 *                  the port number is smaller than 0, then the packet is
 *                  switched normally.
 *
 * \param[in]       dmacHigh is upper 6 bytes of DMAC address
 *
 * \param[in]       dmacLow is lower 6 bytes of DMAC address
 *
 * \param[in]       smacHigh is upper 6 bytes of SMAC address
 *
 * \param[in]       smacLow is lower 6 bytes of SMAC address
 *
 * \param[in]       vid is vlan ID. If the value is smaller than 0, then
 *                  the VLAN tag is not added.
 *
 * \param[in]       vpri is the 4-bit VLAN priority field
 *
 * \param[in]       etype is Ethernet type
 *
 * \param[in]       length is the packet size in bytes (including FCS)
 * 
 * \param[in]       swPri is Switch Priority located into ISL tag
 *
 * \param[in]       fcs is the Frame Check Sequence to be sent with
 *                  the packet.
 *
 *****************************************************************************/
fm_status tpSendPacketHelperL2(fm_int sw,
                               fm_int port,
                               fm_int dmacHigh,
                               fm_int dmacLow,
                               fm_int smacHigh,
                               fm_int smacLow,
                               fm_int vid,
                               fm_int vpri,
                               fm_int etype,
                               fm_int length,
                               fm_int swPri,
                               fm_int fcs)
{
    fm_buffer *     pkt = NULL;
    fm_buffer *     ptr;
    fm_packetInfoV2 info;
    fm_status       err;
    fm_int          bytesDone = 0;
    fm_int          chunkLen;
    fm_int          n;
    fm_byte *       pBuf;
    fm_byte *       pPayload = (fm_byte *) sendPayload;

    /* Make sure length requested is within reason */
    if (length < MIN_TX_PKT_SIZE)
    {
        length = MIN_TX_PKT_SIZE;
    }
    if (length > MAX_TX_PKT_SIZE)
    {
        length = MAX_TX_PKT_SIZE;
    }

    /* FCS will be not be added */
    length -= 4;

    /* Load packet into buffers */
    bytesDone = 0;
    pkt = NULL;

    while (bytesDone < length)
    {
        if (pkt)
        {
            ptr->next = tpRetryingAllocBuffer(sw);
            ptr = ptr->next;
        }
        else
        {
            pkt = ptr = tpRetryingAllocBuffer(sw);

        }

        if (!ptr)
        {
            printf("Unable to allocate buffers to send packet!\n");

            if (pkt)
            {
                fmFreeBufferChain(sw, pkt);
            }

            return FM_ERR_NO_MEM;
        }

        chunkLen = ( ( length - bytesDone ) > FM_BUFFER_SIZE_BYTES  ?
                     FM_BUFFER_SIZE_BYTES :
                     ( length - bytesDone ) );
        pBuf = (fm_byte *) (ptr->data);

        if (bytesDone == 0)
        {
            /* First chunk, load header, assumption is buffer size is bigger
             * than 18 bytes */
            *pBuf++ = (dmacHigh >> 16) & 0xFF;
            *pBuf++ = (dmacHigh >> 8) & 0xFF;
            *pBuf++ = dmacHigh & 0xFF;
            *pBuf++ = (dmacLow >> 16) & 0xFF;
            *pBuf++ = (dmacLow >> 8) & 0xFF;
            *pBuf++ = dmacLow & 0xFF;
            *pBuf++ = (smacHigh >> 16) & 0xFF;
            *pBuf++ = (smacHigh >> 8) & 0xFF;
            *pBuf++ = smacHigh & 0xFF;
            *pBuf++ = (smacLow >> 16) & 0xFF;
            *pBuf++ = (smacLow >> 8) & 0xFF;
            *pBuf++ = smacLow & 0xFF;
            n = 12;
            if (vid >= 0)
            {
                *pBuf++ = 0x81;
                *pBuf++ = 0x00;
                *pBuf++ = ((vpri & 0xF) << 4) | ((vid >> 8) & 0xF);
                *pBuf++ = vid & 0xFF;
                n += 4;
            }
            *pBuf++ = (etype >> 8) & 0xFF;
            *pBuf++ = etype & 0xFF;
            n += 2;

            /* load payload */
            memcpy(pBuf, pPayload, chunkLen - n);
            pPayload += chunkLen - n;
        }
        else
        {
            /* other chunks, load payload */
            memcpy(pBuf, pPayload, chunkLen);
            pPayload += chunkLen;
        }

        ptr->len = chunkLen;
        ptr->next = NULL;
        bytesDone += chunkLen;

    }   /* end while (bytesDone < length) */


    /* Send packet */
    if (port > 0)
    {
        if ( ( swPri > 0 ) || ( fcs != 0 ) )
        {
            memset(&info, 0, sizeof(info));
            info.switchPriority = swPri;
            info.fcsValue = fcs;
            err = fmSendPacketDirectedV2(sw, &port, 1, pkt, &info);
        }
        else
        {
            err = fmSendPacketDirected(sw, &port, 1, pkt);
        }
    }
    else
    {
        err = fmSendPacketSwitched(sw, pkt);
    }
    if (err != FM_OK)
    {
        printf("ERROR: unable to send packet: %s\n", fmErrorMsg(err));
        err = fmFreeBufferChain(sw, pkt);
    }
    return err;
}

/*****************************************************************************/
/** tpSendPacketDirectHelper
 * \ingroup tpPacket
 *
 * \desc            Sends an packet to the CPU port. The packet is either send
 *                  "directed" or "switched" depending if the port is
 *                  specified or not.
 *
 * \param[in]       sw is the switch number to send the packet out on.
 *
 * \param[in]       port points to the port to send the packet to. If the
 *                  the port number is equal or smaller than 0, then the
 *                  packet is switched normally.
 *
 * \param[in]       length is the packet size in bytes NOT including FCS
 *
 *****************************************************************************/
fm_status tpSendPacketDirectHelper(fm_int  sw,
                                   fm_int* port,
                                   fm_int  nPorts,
                                   fm_int  length)
{
    fm_status       err = FM_OK;
    fm_buffer*      pkt = NULL;
    fm_buffer*      ptr = NULL;
    int             bytesDone = 0;
    int             chunkLen = 0;

    /* Verify the packet size */
    if (length > MAX_TX_PKT_SIZE - 4) {
        err = FM_FAIL;
        return err;
    }

    /* Allocate buffers and copy the packet into them */
    while (bytesDone < length)
    {
        if (ptr == NULL)
        {
            /* alloc the first Buffer  */
            pkt = tpRetryingAllocBuffer(sw);
            ptr = pkt;
        }
        else
        {
            /* add additional buffers to the chain */
            ptr->next = tpRetryingAllocBuffer(sw);
            ptr = ptr->next;
        }
        if (!ptr)
        {
            printf("ERROR: Unable to allocate buffers to send packet!\n");

            if (pkt)
            {
                fmFreeBufferChain(sw, pkt);
            }

            err = FM_ERR_NO_MEM;
            break;
        }

        chunkLen = ((length - bytesDone) > FM_BUFFER_SIZE_BYTES) ?
                   FM_BUFFER_SIZE_BYTES :
                   (length - bytesDone);

        /* copy the packet into the allocated buffer */
        memcpy(ptr->data, &sendPayload[bytesDone >> 2], chunkLen);

        ptr->len = chunkLen;
        ptr->next = NULL;
        bytesDone += chunkLen;
    }

    /*****************************************************************
     *  if there were no errors, send the packet
     *  Send direct to port if at least 1 port non null was specified
     *  Send the packet Switched if port = 0.
     ****************************************************************/
    if (err == FM_OK)
    {
        /* Send packet */
        if ( nPorts > 1 || *port > 0 )
        {
            err =  fmSendPacketDirected(sw, port, nPorts, pkt);
        }
        else
        {
            err =  fmSendPacketSwitched(sw, pkt);
        }
        if (err != FM_OK)
        {
            printf("ERROR: unable to send packet: %s\n", fmErrorMsg(err));
            fmFreeBufferChain(sw, pkt);
        }
    }
    return err;
}

/*****************************************************************************/
/** tpSendISLPacketHelper
 * \ingroup tpPacket
 *
 * \desc            Sends an ISL packet to the CPU port.
 *
 * \param[in]       sw is the switch number to send the packet out on.
 *
 * \param[in]       tagFormat is the ISL tag format (F56 or F64).
 *
 * \param[in]       islTag points to an array of 2 32-bit unsigned integers
 *                  which are the contents of the ISL tag to be attached to
 *                  the frame.
 *
 * \param[in]       dmacHigh is upper 6 bytes of DMAC address
 *
 * \param[in]       dmacLow is lower 6 bytes of DMAC address
 *
 * \param[in]       smacHigh is upper 6 bytes of SMAC address
 *
 * \param[in]       smacLow is lower 6 bytes of SMAC address
 *
 * \param[in]       vid is vlan ID. If the value is smaller than 0, then
 *                  the VLAN tag is not added.
 *
 * \param[in]       vpri is the 4-bit VLAN priority field
 *
 * \param[in]       etype is Ethernet type
 *
 * \param[in]       length is the packet size in bytes (including FCS)
 *
 * \param[in]       swPri is Switch Priority located into ISL tag
 *
 * \param[in]       fcs is the Frame Check Sequence to be sent with
 *                  the packet.
 *
 *****************************************************************************/
fm_status tpSendISLPacketHelper(fm_int          sw,
                                fm_islTagFormat tagFormat,
                                fm_uint32 *     islTag,
                                fm_int          dmacHigh,
                                fm_int          dmacLow,
                                fm_int          smacHigh,
                                fm_int          smacLow,
                                fm_int          vid,
                                fm_int          vpri,
                                fm_int          etype,
                                fm_int          length,
                                fm_int          swPri,
                                fm_int          fcs)
 {
    fm_buffer *pkt = NULL;
    fm_buffer *ptr;
    fm_status  err;
    fm_int     bytesDone = 0;
    fm_int     chunkLen;
    fm_int     n;
    fm_byte *  pBuf;
    fm_byte *  pPayload = (fm_byte *) sendPayload;

    /* Make sure length requested is within reason */
    if (length < MIN_TX_PKT_SIZE)
    {
        length = MIN_TX_PKT_SIZE;
    }
    if (length > MAX_TX_PKT_SIZE)
    {
        length = MAX_TX_PKT_SIZE;
    }

    /* FCS will be not be added */
    length -= 4;

    /* Load packet into buffers */
    bytesDone = 0;
    pkt = NULL;

    while (bytesDone < length)
    {
        if (pkt)
        {
            ptr->next = tpRetryingAllocBuffer(sw);
            ptr = ptr->next;
        }
        else
        {
            pkt = ptr = tpRetryingAllocBuffer(sw);

        }

        if (!ptr)
        {
            printf("Unable to allocate buffers to send packet!\n");

            if (pkt)
            {
                fmFreeBufferChain(sw, pkt);
            }

            return FM_ERR_NO_MEM;
        }

        chunkLen = ( ( length - bytesDone ) > FM_BUFFER_SIZE_BYTES  ?
                     FM_BUFFER_SIZE_BYTES :
                     ( length - bytesDone ) );
        pBuf = (fm_byte *) (ptr->data);

        if (bytesDone == 0)
        {
            /* First chunk, load header, assumption is buffer size is bigger
             * than 18 bytes */
            *pBuf++ = (dmacHigh >> 16) & 0xFF;
            *pBuf++ = (dmacHigh >> 8) & 0xFF;
            *pBuf++ = dmacHigh & 0xFF;
            *pBuf++ = (dmacLow >> 16) & 0xFF;
            *pBuf++ = (dmacLow >> 8) & 0xFF;
            *pBuf++ = dmacLow & 0xFF;
            *pBuf++ = (smacHigh >> 16) & 0xFF;
            *pBuf++ = (smacHigh >> 8) & 0xFF;
            *pBuf++ = smacHigh & 0xFF;
            *pBuf++ = (smacLow >> 16) & 0xFF;
            *pBuf++ = (smacLow >> 8) & 0xFF;
            *pBuf++ = smacLow & 0xFF;
            n = 12;
            if (vid >= 0)
            {
                *pBuf++ = 0x81;
                *pBuf++ = 0x00;
                *pBuf++ = ((vpri & 0xF) << 4) | ((vid >> 8) & 0xF);
                *pBuf++ = vid & 0xFF;
                n += 4;
            }
            *pBuf++ = (etype >> 8) & 0xFF;
            *pBuf++ = etype & 0xFF;
            n += 2;

            /* load payload */
            memcpy(pBuf, pPayload, chunkLen - n);
            pPayload += chunkLen - n;
        }
        else
        {
            /* other chunks, load payload */
            memcpy(pBuf, pPayload, chunkLen);
            pPayload += chunkLen;
        }

        ptr->len = chunkLen;
        ptr->next = NULL;
        bytesDone += chunkLen;

    }   /* end while (bytesDone < length) */


    /* Send packet */
    err = fmSendPacketISL(sw, islTag, tagFormat, pkt);

    if (err != FM_OK)
    {
        printf("ERROR: unable to send packet: %s\n", fmErrorMsg(err));
        err = fmFreeBufferChain(sw, pkt);
    }

    return err;

}




/*****************************************************************************/
/** tpProcessPacketReceiveEvent
 * \ingroup tpPacket
 *
 * \desc            Processes a receive event, including classification and
 *                  calling of handlers.  The default fall-through case is
 *                  the user accessible receive packet cache.
 *
 * \param[in]       sw is the switch number on which the packet event occurred.
 *
 * \param[in]       pkt is packet receive event info structure.
 *
 *****************************************************************************/
void tpProcessPacketReceiveEvent(int sw, fm_eventPktRecv *pkt)
{
    fm_buffer *   buf = (fm_buffer *) pkt->pkt;
    fm_buffer *   prev;
    fm_buffer *   ptr;
    fm_packetInfo info;
    fm_status     err;
    fm_int        len;
    int           numSlots;
    int           pos;

    if (!lockInit)
    {
        tpInitializePacketHandling();
    }

    /* Loopback the packet if desired */
    if ( lciLoopback )
    {
        /* Compute packet length */
        len = 0;
        ptr = buf;
        prev = NULL;
        while (ptr->next != NULL)
        {
            prev = ptr;
            ptr = ptr->next;
        }
        len += ptr->len;

        /* Chop off the FCS */
        if ( ptr->len > 4 )
        {
             /* Remove the FCS, no boundary conditions */
             ptr->len -= 4;
        }
        else if ( prev != NULL )
        {
             /* Remove the FCS, which will free up last buffer and may
                cause removing some bytes from previous buffer */
             prev->len -= (ptr->len - 4);
             prev->next = NULL;
             fmFreeBuffer(sw, ptr);
        }

        /* Return packet to switch */
        memset(&info, 0, sizeof(fm_packetInfo));
        info.useEgressRules = FALSE;
        info.vlanId = 0;
        info.directSendVlanId = 0;
        info.logicalPort = pkt->srcPort;
        info.destMask = 1 << pkt->srcPort;

        /* FIXME: this is switch specific, and we should not be
         * referring to switch specific constants in swig code
         * since swig code implies a certain level of switch
         * agnosticity -NS
         */
        info.switchPriority = 0xffff; /* FM4000_USE_VLAN_PRIORITY ; */

        err =  fmSendPacketExt(sw, &info, buf);
        if ( err != FM_OK )
        {
            fmFreeBufferChain(sw, buf);
            fprintf(stderr,
                    "error: can't loop back packet: %s\n",
                    fmErrorMsg(err));
        }
        return;
     }

    /***************************************************
     * Let the classifier handle the packet, if it
     * returns true then we free the chain and don't
     * push it into the user accessible local queue.
     **************************************************/
    if (fmPacketClassify(pkt))
    {
        fmFreeBufferChain(sw, buf);
        return;
    }

    fmCaptureLock(&recvLock, FM_WAIT_FOREVER);

    /* Determine the number of unused receive queue slots. */
    if (nextGrabSlot == -1)
    {
        /* The receive queue is empty. */
        numSlots = MAX_LOCAL_RECV_QUEUE;
    }
    else
    {
        /* One or more receive queue slots are in use. */
        numSlots = (nextRecvSlot - nextGrabSlot) % MAX_LOCAL_RECV_QUEUE;
    }

    if (numSlots > 0)
    {
        /* One or more receive queue slots are free for use. */
        memcpy(&recvInfo[nextRecvSlot], pkt, sizeof(fm_eventPktRecv));

        recvLen[nextRecvSlot] = 0;
        for ( ptr = buf, pos = 0;
              ptr != NULL && pos < MAX_PKT_SIZE;
              ptr = ptr->next )
        {
            recvLen[nextRecvSlot] += ptr->len;

            memcpy(&recvPayload[nextRecvSlot][pos], ptr->data, ptr->len);
            pos += (ptr->len >> 2) + ((ptr->len % 4) ? 1 : 0);
        }

        if (ptr != NULL)
        {
            fprintf(stderr,
                    "error: received packet truncated to %zd bytes\n",
                    sizeof(recvPayload[0][0]) * MAX_PKT_SIZE);
        }

        if (nextGrabSlot == -1)
        {
            nextGrabSlot = nextRecvSlot;
        }

        nextRecvSlot = (nextRecvSlot + 1) % MAX_LOCAL_RECV_QUEUE;
    }

    fmReleaseLock(&recvLock);

    fmFreeBufferChain(sw, buf);
}

/*****************************************************************************/
/** tpGrabRecvPktPayload
 * \ingroup tpPacket
 *
 * \desc            Returns the value of a byte within the local payload cache.
 *
 * \param[in]       pos is the position within the payload.
 *
 * \param[in]       value points to caller allocated storage where the byte
 *                  value is written.
 *
 *****************************************************************************/
void tpGrabRecvPktPayload(int pos, int *value)
{
    char *pBuf;

    if (!lockInit)
    {
        tpInitializePacketHandling();
    }

    fmCaptureLock(&recvLock, FM_WAIT_FOREVER);

    if (nextGrabSlot == -1)
    {
        *value = -1;
    }
    else
    {
        pBuf = (char*) &recvPayload[nextGrabSlot];

        *value = ((int) pBuf[pos]) & 0xFF;
    }

    fmReleaseLock(&recvLock);
}

/*****************************************************************************/
/** tpGrabRecvPktInfo
 * \ingroup tpPacket
 *
 * \desc            Returns various information about the packet at the head
 *                  of the local packet receive queue.
 *
 * \param[in]       src points to caller allocated storage where the source
 *                  port is stored.
 *
 * \param[in]       vlan points to caller allocated storage where the VLAN
 *                  number is stored.
 *
 * \param[in]       frameAction points to caller allocated storage where
 *                  an 8-bit value is written indicating the trap action.
 *
 * \param[in]       valid points to caller allocated storage that is set to 1
 *                  if the current head of the queue contains a valid frame.
 *
 *****************************************************************************/
void tpGrabRecvPktInfo(int *src,
                       int *vlan,
                       int *len,
                       int *frameAction,
                       int *valid)
{
    if (!lockInit)
    {
        tpInitializePacketHandling();
    }

    fmCaptureLock(&recvLock, FM_WAIT_FOREVER);

    if (nextGrabSlot == -1)
    {
        /* The receive queue is empty. */
        *valid = 0;
    }
    else
    {
        /* One or more receive queue slots are in use. */
        *src = recvInfo[nextGrabSlot].srcPort;
        *vlan = recvInfo[nextGrabSlot].vlan;
        *len = recvLen[nextGrabSlot];
        /* The frame action is the lower 8 bits of the destination GLORT. */
        *frameAction = recvInfo[nextGrabSlot].trapAction;
        *valid = 1;
    }

    fmReleaseLock(&recvLock);
}

/*****************************************************************************/
/** tpGrabRecvPkt
 * \ingroup tpPacket
 *
 * \desc            Invalidates the head of the local queue.  Assumes that the
 *                  caller has already used tpGrabRecvPktInfo et. al. to get
 *                  the contents of the frame.
 *
 *****************************************************************************/
void tpGrabRecvPkt()
{
    if (!lockInit)
    {
        tpInitializePacketHandling();
    }

    fmCaptureLock(&recvLock, FM_WAIT_FOREVER);

    if (nextGrabSlot != -1)
    {
        /* One or more receive queue slots are in use. */
        nextGrabSlot = (nextGrabSlot + 1) % MAX_LOCAL_RECV_QUEUE;

        if (nextGrabSlot == nextRecvSlot)
        {
            /* Mark the receive queue as being empty. */
            nextGrabSlot = -1;
        }
    }

    fmReleaseLock(&recvLock);
}

/*****************************************************************************/
/** tpClearRecvQueue
 *
 * \desc            Invalidates the head of the local queue and mark it as 
 *                  being empty.
 *
 *****************************************************************************/
void tpClearRecvQueue()
{
     if (!lockInit)
    {
        tpInitializePacketHandling();
    }

    fmCaptureLock(&recvLock, FM_WAIT_FOREVER);

    if (nextGrabSlot != -1)
    {
        /* Mark the receive queue as being empty. */
        nextGrabSlot = -1;
    }

    fmReleaseLock(&recvLock);
}

%}
