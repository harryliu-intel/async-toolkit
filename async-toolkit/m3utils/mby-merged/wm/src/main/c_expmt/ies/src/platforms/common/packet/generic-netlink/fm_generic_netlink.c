/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_generic_netlink.c
 * Creation Date:   September, 2011
 * Description:     Netlink socket packet methods. This code assumes
 *                  mutiple switches driven by one single driver.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2006 - 2013 Intel Corporation. All Rights Reserved. 
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
#include <platforms/common/packet/generic-netlink/fm_generic_netlink.h>
#include <sys/socket.h>
#include <net/if.h>
#include <linux/netlink.h>
#include <poll.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define FM_SYSTEM0_NAME             FM_GLOBAL_NETDEV_NAME
#define FM_ETH_NAME                 "eth0"
#define FM_RECV_BUFFER_THRESHOLD    4

/*****************************************************************************
 * Global Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

fm_status fmMACAddressInitialize(void);


/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** fmNetlinkPacketHandlingInitialize
 * \ingroup intPlatformCommon
 *
 * \desc            Performs initialization for the Netlink packet transfer
 *                  module.
 *
 * \param[in]       none
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmNetlinkPacketHandlingInitialize(void)
{
    return fmNetlinkPacketHandlingInitializeV2(0, FALSE);
}




/*****************************************************************************/
/** fmNetlinkPacketHandlingInitializeV2
 * \ingroup intPlatformCommon
 *
 * \desc            Initializes the Netlink packet transfer module.
 *
 * \param[in]       sw is the switch number to initialize.
 * 
 * \param[in]       hasFcs is TRUE if the packet includes the FCS field.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmNetlinkPacketHandlingInitializeV2(fm_int sw, fm_bool hasFcs)
{
    fm_int    nlSock;
    fm_status err = FM_OK;
    struct sockaddr_nl nlDest;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_TX,
                 "sw=%d hasFcs=%s\n",
                 sw,
                 FM_BOOLSTRING(hasFcs));

    fmGenericPacketHandlingInitializeV2(sw, hasFcs);

    /* initialize the netlink sockaddr */
    nlSock = socket(AF_NETLINK, SOCK_RAW, FM_NL_SOCK);
    if (nlSock == -1)
    {
        FM_LOG_FATAL(FM_LOG_CAT_EVENT_PKT_TX, 
                     "fmNetlinkPacketHandlingInitialize: "
                     "couldn't create netlink socket");
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_FAIL);
    }
    else
    {
        FM_CLEAR(nlDest);
        nlDest.nl_family = AF_NETLINK;
        nlDest.nl_pid = getpid();
        nlDest.nl_groups = FM_NL_GROUP_ALL;
        if (bind(nlSock, (struct sockaddr *)&nlDest, sizeof(nlDest)) < 0)
        {
            close(nlSock);
            FM_LOG_FATAL(FM_LOG_CAT_EVENT_PKT_TX, 
                         "fmNetlinkPacketHandlingInitialize: "
                         "couldn't bind netlink socket");
            FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_FAIL);
        }
    }

    GET_PLAT_STATE(0)->nlSocket = nlSock;

    fmMACAddressInitialize();

    /* Create the receive packet thread */
    err = fmCreateThread("netlink receive",
                         FM_EVENT_QUEUE_SIZE_NONE,
                         &fmNetlinkReceivePackets,
                         NULL,
                         &fmRootPlatform->netlinkThread);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end fmNetlinkPacketHandlingInitializeV2 */




/*****************************************************************************/
/** fmMACAddressInitialize
 * \ingroup intPlatformCommon
 *
 * \desc            Sets the MAC address for the in-band system interface.
 *
 * \param[in]       none
 *
 * \return          FM_OK if successful.
 *
 *
 *****************************************************************************/
fm_status fmMACAddressInitialize(void)
{
    struct ifreq ifr;
    int          s;
    fm_byte      *mac;

    FM_LOG_ENTRY_NOARGS(FM_LOG_CAT_EVENT_PKT_TX);

    s =  socket(AF_INET, SOCK_DGRAM, 0);
    if (s < 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, FM_FAIL);
    }

    /* retrieve the MAC address of the dedicated management port */
    FM_CLEAR(ifr);
    FM_STRNCPY_S( ifr.ifr_name,
                  sizeof(ifr.ifr_name),
                  FM_ETH_NAME,
                  sizeof(ifr.ifr_name) );
    if (ioctl(s, SIOCGIFHWADDR, &ifr) < 0)
    {
        close(s);
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, FM_FAIL);
    }

    /* set the system interface MAC */
    mac = (fm_byte *)&(ifr.ifr_hwaddr.sa_data);
    mac[5]++;
    FM_STRNCPY_S( ifr.ifr_name,
                  sizeof(ifr.ifr_name),
                  FM_SYSTEM0_NAME,
                  sizeof(ifr.ifr_name) );

    if (ioctl(s, SIOCSIFHWADDR, &ifr) < 0)
    {
        close(s);
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, FM_FAIL);
    }

    close(s);

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, FM_OK);

}   /* end fmMACAddressInitialize */




/*****************************************************************************/
/** fmNetlinkSendPackets
 * \ingroup intPlatformCommon
 *
 * \desc            When called, iterates through the packet queue and
 *                  continues to send packets until either the queue empties.
 *
 * \param[in]       sw refers to the switch number to send packets to.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmNetlinkSendPackets(fm_int sw)
{
    fm_packetHandlingState *pktState;
    fm_packetQueue *        txQueue;
    fm_packetEntry *        packet;
    fm_status               err = FM_OK;
    fm_int32                rc;
    fm_buffer               *sendBuf;
    struct msghdr           msg;
    struct iovec            iov[UIO_MAXIOV];
    struct sockaddr_nl      nlDest;
    struct fm_nlHdr         fnm;
    fm_switch *             switchPtr;
    fm_uint32               fcs;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_TX, "sw = %d\n", sw);

    FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                 "fmNetlinkSendPackets(%d)\n", sw);

    switchPtr = GET_SWITCH_PTR(sw);
    pktState = GET_PLAT_PKT_STATE(sw);

    /* initialize the message header */
    FM_CLEAR(msg);
    msg.msg_name = (void*)&nlDest;
    msg.msg_namelen = sizeof(nlDest);
    msg.msg_iov = iov;
    msg.msg_iovlen = 0;
    msg.msg_flags = 0;

    /* initialize the netlink sockaddr */
    FM_CLEAR(nlDest);
    nlDest.nl_family = AF_NETLINK;
    nlDest.nl_pid = 0;
    nlDest.nl_groups = 0;

    txQueue = &pktState->txQueue;
    fmPacketQueueLock(txQueue);

    for ( ;
          txQueue->pullIndex != txQueue->pushIndex ;
          txQueue->pullIndex = (txQueue->pullIndex + 1) % FM_PACKET_QUEUE_SIZE)
    {
        packet = &txQueue->packetQueueList[txQueue->pullIndex];

        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                     "fmNetlinkSendPackets: sending packet in "
                     "slot %d, length=%d tag=%d fcs=%08x\n",
                     txQueue->pullIndex, packet->length,
                     packet->suppressVlanTag, packet->fcsVal);

        msg.msg_iovlen = 0;

        /* first iovec is the netlink header, which contains the f64 tag */
        fnm.fm64[0] = packet->islTag.f64.tag[0];
        fnm.fm64[1] = packet->islTag.f64.tag[1];
        fnm.sw = sw;
        iov[msg.msg_iovlen].iov_base = &fnm;
        iov[msg.msg_iovlen].iov_len = sizeof(fnm);
        msg.msg_iovlen++;

        /* iterate through all buffers */
        for ( sendBuf = packet->packet ; sendBuf ; sendBuf = sendBuf->next )
        {
            /* if first buffer ... */
            if (sendBuf == packet->packet)
            {
                /* Cannot modify the send buffer, since the same buffer can be
                 * used multiple times to send to multiple ports
                 */

                /* second iovec is the mac header */
                iov[msg.msg_iovlen].iov_base = sendBuf->data;
                iov[msg.msg_iovlen].iov_len = 12;
                msg.msg_iovlen++;

                /* Third is the data in the first chain */
                if (packet->suppressVlanTag)
                {
                    iov[msg.msg_iovlen].iov_base = &sendBuf->data[4];
                    iov[msg.msg_iovlen].iov_len = sendBuf->len-16;
                    msg.msg_iovlen++;
                }
                else
                {
                    iov[msg.msg_iovlen].iov_base = &sendBuf->data[3];
                    iov[msg.msg_iovlen].iov_len = sendBuf->len-12;
                    msg.msg_iovlen++;
                }
            }
            else
            {
                /* The rest of the chain */
                iov[msg.msg_iovlen].iov_base = sendBuf->data;
                iov[msg.msg_iovlen].iov_len = sendBuf->len;
                msg.msg_iovlen++;
            }

        }   /* end for (...) */

        /* Append user-supplied FCS value to packet. */
        if (pktState->sendUserFcs)
        {
            fcs = htonl(packet->fcsVal);
            iov[msg.msg_iovlen].iov_base = &fcs;
            iov[msg.msg_iovlen].iov_len = sizeof(fcs);
            msg.msg_iovlen++;
        }

        /* now send it to the driver */
        rc = sendmsg(GET_PLAT_STATE(0)->nlSocket, &msg, MSG_DONTWAIT);
        if (rc == -1)
        {
            switchPtr->transmitterLock = TRUE;
            if (errno != EWOULDBLOCK)
            {
                FM_LOG_ERROR(FM_LOG_CAT_EVENT_PKT_TX, 
                             "sendmsg failed - errno %d\n", errno);
                err = FM_FAIL;
            }

            goto ABORT;
        }
        else
        {
            switchPtr->transmitterLock = FALSE;
            fmDbgDiagCountIncr(sw, FM_CTR_TX_PKT_COMPLETE, 1);
        }

        /**************************************************
         * free buffer only when
         * (1) sending to a single port;
         * or (2) this is the last packet sent to a vlan
         **************************************************/

        if (packet->freePacketBuffer)
        {
            /* ignore the error code since it's better to continue */
            (void) fmFreeBufferChain(sw, packet->packet);

            fmDbgGlobalDiagCountIncr(FM_GLOBAL_CTR_TX_BUFFER_FREES, 1);

        }

    }

ABORT:
    fmPacketQueueUnlock(txQueue);

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);

}   /* end fmNetlinkSendPackets */




/*****************************************************************************/
/** fmNetlinkReceivePackets
 * \ingroup intPlatformCommon
 *
 * \desc            Handles reception of packets by Netlink socket.
 *
 * \param[in]       args is a dummy argument that is not used by this function.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
void * fmNetlinkReceivePackets(void *args)
{
    fm_buffer *                   recvChainHead = NULL;
    fm_buffer *                   nextBuffer;
    struct sockaddr_nl            nlDest;
    struct fm_nlHdr               *fnm;
    struct msghdr                 msg;
    struct iovec                  iov[UIO_MAXIOV];
    fm_int                        len;
    fm_int                        iov_offset;
    fm_int                        iov_count = 0;
    fm_int                        maxMtu = 0;
    fm_int                        newMtu;
    fm_status                     status;
    char                          strErrBuf[FM_STRERROR_BUF_SIZE];
    errno_t                       strErrNum;

    FM_NOT_USED(args);

    /* initialize the message header */
    FM_CLEAR(msg);
    msg.msg_name = (void*)&nlDest;
    msg.msg_namelen = sizeof(nlDest);
    msg.msg_iov = iov;
    msg.msg_iovlen = 0;
    msg.msg_flags = 0;

    /* initialize the netlink sockaddr */
    FM_CLEAR(nlDest);
    nlDest.nl_family = AF_NETLINK;
    nlDest.nl_pid = 0;
    nlDest.nl_groups = FM_NL_GROUP_ALL;


    /**************************************************
     * Loop forever calling packet receive handler.
     **************************************************/

    while (TRUE)
    {
        struct pollfd  rfds;
        fm_int  retval;
        fm_int  availableBuffers;

        rfds.fd = GET_PLAT_STATE(0)->nlSocket;
        rfds.events = POLLIN;
        rfds.revents = 0;

        retval = poll(&rfds, 1, FM_FDS_POLL_TIMEOUT_USEC);

        if (retval == -1)
        {
            strErrNum = FM_STRERROR_S(strErrBuf, FM_STRERROR_BUF_SIZE, errno);
            if (strErrNum == 0)
            {
                FM_LOG_WARNING(FM_LOG_CAT_SWITCH,
                               "ERROR: fmNetlinkReceivePackets: select failed: %s!\n",
                               strErrBuf);
            }
            else
            {
                FM_LOG_WARNING(FM_LOG_CAT_SWITCH,
                               "ERROR: fmNetlinkReceivePackets: select failed: %d!\n",
                               errno);
            }
            continue;
        }
        else if (!retval)
        {
            continue; /* timeout */
        }

        /* get the number of available buffers from the buffer manager*/
        fmPlatformGetAvailableBuffers(&availableBuffers);

        if (availableBuffers <= FM_RECV_BUFFER_THRESHOLD)
        {
            /* wait for buffer to come back, before dequeueing data */
            fmYield();
            continue;
        }

#if 0
        status = fmGetPortAttribute(FM_FIRST_FOCALPOINT,
                                    0,
                                    FM_PORT_MAX_FRAME_SIZE,
                                    &newMtu);
#else
        /* Temporary work-around until the kernel driver supports on-the-fly
         * MTU size changes.
         */
        newMtu = FM_PORT_0_MAX_FRAME_SIZE;
        status = FM_OK;
#endif
        if (status != FM_OK)
        {
            FM_LOG_WARNING( FM_LOG_CAT_SWITCH,
                           "WARNING: fmNetlinkReceivePackets unable to read "
                           "max frame size for switch %d port 0, status = %d (%s)\n",
                           FM_FIRST_FOCALPOINT,
                           status,
                           fmErrorMsg(status) );
            continue;
        }

        /* MTU Size change */
        if (newMtu != maxMtu)
        {
            if (recvChainHead != NULL)
            {
                /* release the existing buffer chain */
                status = fmFreeBufferChain(FM_FIRST_FOCALPOINT, recvChainHead);

                if (status != FM_OK)
                {
                    FM_LOG_ERROR( FM_LOG_CAT_SWITCH,
                                 "Unable to release prior buffer chain, "
                                 "status = %d (%s)\n",
                                 status,
                                 fmErrorMsg(status) );
                }

                recvChainHead = NULL;
            }

            /* compute new buffer count */
            iov_count = newMtu / FM_BUFFER_SIZE_BYTES;
            if (newMtu % FM_BUFFER_SIZE_BYTES)
            {
                iov_count++;
            }

            maxMtu = newMtu;
        }

        if (recvChainHead == NULL)
        {
            /* allocate a new buffer chain and initialize iovec array */
            msg.msg_iovlen = 0;
            for (iov_offset = 0 ; iov_offset < iov_count ; iov_offset++)
            {
                do
                {
                    nextBuffer = fmAllocateBuffer(FM_FIRST_FOCALPOINT);

                    if (nextBuffer == NULL)
                    {
                        /* Wait a little while for buffer to return */
                        fmDbgGlobalDiagCountIncr(FM_GLOBAL_CTR_RX_OUT_OF_BUFFERS,
                                                 1);
                        fmYield();
                    }
                }
                while (nextBuffer == NULL);

                if (recvChainHead == NULL)
                {
                    recvChainHead    = nextBuffer;
                    nextBuffer->next = NULL;
                }
                else
                {
                    status = fmAddBuffer(recvChainHead, nextBuffer);

                    if (status != FM_OK)
                    {
                        FM_LOG_ERROR( FM_LOG_CAT_SWITCH,
                                     "Unable to add buffer %d (%p) to chain %p\n",
                                     iov_offset,
                                     (void *) nextBuffer,
                                     (void *) recvChainHead );
                        break;
                    }
                }

                iov[iov_offset].iov_base = nextBuffer->data;
                iov[iov_offset].iov_len  = FM_BUFFER_SIZE_BYTES;
                msg.msg_iovlen++;
            }
        }

        /* now receive from the driver */
        len = recvmsg(GET_PLAT_STATE(FM_FIRST_FOCALPOINT)->nlSocket,
                      &msg,
                      0);

        if (len == -1)
        {
            continue;
        }

        fnm = (struct fm_nlHdr *) recvChainHead->data;

        /* move the first buffer pointer past the header */
        recvChainHead->data = (fm_uint32 *) (fnm + 1);

        /* fill in the used buffer sizes */
        nextBuffer = recvChainHead;

        while (nextBuffer != NULL)
        {
            if (len > FM_BUFFER_SIZE_BYTES)
            {
                nextBuffer->len = FM_BUFFER_SIZE_BYTES;
            }
            else
            {
                nextBuffer->len = len;
            }

            len -= nextBuffer->len;

            if (nextBuffer == recvChainHead)
            {
                /* remove the header from the buffer byte count */
                nextBuffer->len -= sizeof(*fnm);
            }

            if ( (len <= 0) && (nextBuffer->next != NULL) )
            {
                status = fmFreeBufferChain(FM_FIRST_FOCALPOINT,
                                           nextBuffer->next);

                if (status != FM_OK)
                {
                    FM_LOG_ERROR( FM_LOG_CAT_SWITCH,
                                 "Unable to release unused buffer chain, "
                                 "status = %d (%s)\n",
                                 status,
                                 fmErrorMsg(status) );
                }

                nextBuffer->next = NULL;
            }

            nextBuffer = nextBuffer->next;
        }

        if (recvChainHead == NULL)
        {
            continue;
        }

        /* The fm64 tag should be in host-byte order */
        status = fmPlatformReceiveProcess(fnm->sw,
                                          recvChainHead,
                                          fnm->fm64,
                                          0);

        if (status != FM_OK)
        {
            FM_LOG_ERROR( FM_LOG_CAT_SWITCH,
                         "fm4000PacketReceiveProcess returned error status %d"
                         "(%s)\n",
                         status,
                         fmErrorMsg(status) );
        }
        /* Buffer chain has now been consumed */
        recvChainHead = NULL; 

    }   /* end while (TRUE) */

    /**************************************************
     * Should never exit.
     **************************************************/

    FM_LOG_ERROR(FM_LOG_CAT_SWITCH,
                 "ERROR: fmNetlinkReceivePackets: exiting inadvertently!\n");

    return NULL;


}   /* end fmNetlinkReceivePackets */





