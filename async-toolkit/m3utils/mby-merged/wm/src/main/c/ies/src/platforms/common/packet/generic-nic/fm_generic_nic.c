/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_generic_nic.c
 * Creation Date:   Sep 1, 2008
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

#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <linux/if.h>
#include <linux/if_ether.h>
#include <linux/if_packet.h>
#include <netinet/in.h>
#include <poll.h>

#include <fm_sdk_fm4000_int.h>
#include <fm_sdk_fm6000_int.h>
#include <platforms/common/packet/generic-nic/fm_generic_nic.h>



/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define NIC_PACKET_MAX_LEN          10000
#define MAX_IO_VEC                  10
#define FM_RECV_BUFFER_THRESHOLD    4

#define FIBM_RESPONSE_ETHERTYPE     0xdf00

/* Set the maximum number of NIC equal to the number of switches. */
#define MAX_NUM_NIC                 FM_MAX_NUM_FOCALPOINTS

/*****************************************************************************
 * Global Variables
 *****************************************************************************/


/*****************************************************************************
 * Local Variables
 *****************************************************************************/

static fm_thread    fmNicRxThread;
static fm_thread    fmNicTxThread;
static fm_buffer   *packetRxBuffer[MAX_IO_VEC];
static struct iovec rxVector[MAX_IO_VEC];

static fm_nic       fmNic[MAX_NUM_NIC];
static fm_int       socketListCnt;

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

static fm_status fmNicReceivePackets(void);

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

static fm_int fmNicDevOpen(fm_nic *pNic)
{
    struct  ifreq ethreq;
    struct  sockaddr_ll sock_addr;
    fm_int  sockFd;
    char    strErrBuf[FM_STRERROR_BUF_SIZE];
    errno_t strErrNum;

    FM_LOG_DEBUG(FM_LOG_CAT_FIBM,"fmNicDevOpen: devName=%s\n", pNic->devName);
    /* Every packet (be careful!!!) */
    if ((sockFd = socket(PF_PACKET, SOCK_RAW, htons(0x0003))) < 0)
    {
        strErrNum = FM_STRERROR_S(strErrBuf, FM_STRERROR_BUF_SIZE, errno);
        if (strErrNum == 0)
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, 
                         "Failed to open socket: %s\n",
                         strErrBuf);
        }
        else
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, 
                         "Failed to open socket: %d\n",
                         errno);
        }
        return FM_FAIL;    }

    FM_STRNCPY_S( ethreq.ifr_name,
                  sizeof(ethreq.ifr_name),
                  pNic->devName,
                  IFNAMSIZ );

    if (ioctl(sockFd, SIOCGIFINDEX, &ethreq) == -1)
    {
        strErrNum = FM_STRERROR_S(strErrBuf, FM_STRERROR_BUF_SIZE, errno);
        if (strErrNum == 0)
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, 
                         "Failed to get socket ifindex for %s: %s\n",
                         pNic->devName, 
                         strErrBuf);
        }
        else
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, 
                         "Failed to get socket ifindex for %s: %d\n",
                         pNic->devName, 
                         errno);
        }
        close(sockFd);
        return FM_FAIL;
    }
    
    FM_LOG_DEBUG(FM_LOG_CAT_FIBM,
                 "fmNicDevOpen: socket=%d, index=%d\n",
                 sockFd,
                 ethreq.ifr_ifindex);

    sock_addr.sll_family = AF_PACKET;
    sock_addr.sll_protocol = 0;
    sock_addr.sll_ifindex = ethreq.ifr_ifindex;
    sock_addr.sll_hatype = 0;
    sock_addr.sll_pkttype = 0;
    sock_addr.sll_halen = 0;

    /* SECURITY NOTE: Binding to INADDR_ANY is an insecure practice. We do so
     * here in order to simplify testing. Customers should consider this issue 
     * when implementing their platforms and adjust their code accordingly. 
     * Ignored in Klocwork. 
     */ 
    if (bind(sockFd, (struct sockaddr *)&sock_addr, sizeof(sock_addr)))
    {
        strErrNum = FM_STRERROR_S(strErrBuf, FM_STRERROR_BUF_SIZE, errno);
        if (strErrNum == 0)
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, 
                         "Failed to bind socket to %s: %s\n",
                         pNic->devName, 
                         strErrBuf);
        }
        else
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, 
                         "Failed to bind socket to %s: %d\n",
                         pNic->devName, 
                         errno);
        }
        close(sockFd);
        return FM_FAIL;
    }

    ethreq.ifr_mtu = 7000; /* NIC card specific */
    if (ioctl(sockFd, SIOCSIFMTU, &ethreq) == -1)
    {
        strErrNum = FM_STRERROR_S(strErrBuf, FM_STRERROR_BUF_SIZE, errno);
        if (strErrNum == 0)
        {
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                         "PHY: Failed to set device mtu: %s\n",
                         strErrBuf);
        }
        else
        {
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                         "PHY: Failed to set device mtu: %d\n",
                         errno);
        }
        /* Okay to continue */
    }

    if (ioctl(sockFd, SIOCGIFFLAGS, &ethreq) == -1)
    {
        strErrNum = FM_STRERROR_S(strErrBuf, FM_STRERROR_BUF_SIZE, errno);
        if (strErrNum == 0)
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, 
                         "Failed to get %s socket flags: %s\n",
                         pNic->devName, 
                         strErrBuf);
        }
        else
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, 
                         "Failed to get %s socket flags: %d\n",
                         pNic->devName, 
                         errno);
        }
        close(sockFd);
        return FM_FAIL;
    }

    /* Set the NIC promiscuous mode */
    ethreq.ifr_flags |= (pNic->promiscuous) ? IFF_PROMISC : 0;

    /* Bring the NIC up */
    ethreq.ifr_flags |= IFF_UP;

    if (ioctl(sockFd, SIOCSIFFLAGS, &ethreq) == -1)
    {
        strErrNum = FM_STRERROR_S(strErrBuf, FM_STRERROR_BUF_SIZE, errno);
        if (strErrNum == 0)
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, 
                         "Failed to bring up %s in promiscuous mode: %s\n",
                         pNic->devName, 
                         strErrBuf);
        }
        else
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, 
                         "Failed to bring up %s in promiscuous mode: %d\n",
                         pNic->devName, 
                         errno);
        }
        close(sockFd);
        return FM_FAIL;
    }

    if (ioctl(sockFd, SIOCGIFHWADDR, &ethreq) == -1)
    {
        strErrNum = FM_STRERROR_S(strErrBuf, FM_STRERROR_BUF_SIZE, errno);
        if (strErrNum == 0)
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "Failed getting hwaddr for %s:%s\n",
                         pNic->devName,
                         strErrBuf);
        }
        else
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "Failed getting hwaddr for %s:%d\n",
                         pNic->devName,
                         errno);
        }

        close(sockFd);
        return -1;
    }

    /* Save the NIC mac addr in the switch structure */
    memcpy(pNic->nicMacAddr, &(ethreq.ifr_hwaddr.sa_data), sizeof(pNic->nicMacAddr));

    pNic->sockFd = sockFd;
    socketListCnt++;

    return FM_OK;

} /* end fmNicDevOpen */

static fm_status ResetNicStructure(fm_nic *pNic)
{
    pNic->sockFd = -1;
    pNic->numSwitches = 0;
    pNic->promiscuous = FM_USE_PROMISCUOUS_MODE;
    pNic->inSelectList = FALSE;
    pNic->isWaitingOnSemaphore = FALSE;
    memset(pNic->devName, 0, sizeof(pNic->devName));
    
    return FM_OK;
}

static fm_status fmNicInit(void)
{
    fm_platform_state *ps;
    fm_status          status;
    fm_int             sw;
    fm_int             i;

    socketListCnt = 0;

    for (i = 0 ; i < MAX_NUM_NIC ; i++)
    {
        /* Reset the NIC structure */
        ResetNicStructure(&fmNic[i]);

        status = fmCreateSemaphore("NIC select list",
                                   FM_SEM_BINARY,
                                   &fmNic[i].selectSemaphore,
                                   0);
        if (status != FM_OK)
        {
            FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);
        }
    }

    /* Open the default NIC interface */
    FM_STRNCPY_S( fmNic[0].devName,
                  sizeof(fmNic[0].devName),
                  FM_DEFAULT_DEV_NAME,
                  FM_DEV_NAME_LEN);

    FM_LOG_DEBUG(FM_LOG_CAT_FIBM,
                 "fmNicInit: Default NIC dev: %s\n",
                 fmNic[0].devName);

    /* Don't return error even we can't open the device since the default
     * device might not exist yet and the application can change it
     */
    fmNicDevOpen(&fmNic[0]);


    for (sw = FM_FIRST_FOCALPOINT ; sw <= FM_LAST_FOCALPOINT ; sw++)
    {
        ps = &fmRootPlatform->fmPlatformState[sw];

        ps->pNic = &fmNic[0];
        fmNic[0].numSwitches++;

        ps->useNicMacAddr = FM_USE_NIC_MAC_AS_FIBM_SMAC_ADDR;

        FM_LOG_INFO(FM_LOG_CAT_PLATFORM, 
                    "Switch# %d using dev: %s\n",
                    sw,
                    ps->pNic->devName);
    }

    return FM_OK;
} /* end fmNICInit */



/*****************************************************************************/
/** fmNicRxThreadHandler
 * \ingroup intPlatformFibm
 *
 * \desc            thread to receive packet from net device 
 *
 * \param[in]       args contains nothing useful
 *
 * \return          None.
 *
 *****************************************************************************/
static void *fmNicRxThreadHandler(void *args)
{

    FM_NOT_USED(args);
    fmNicReceivePackets();
    return NULL;
} /* end fmNicRxThreadHandler */



/*****************************************************************************/
/** fmNicTxThreadHandler
 * \ingroup intPlatformFibm
 *
 * \desc            thread to transmit packet to net device 
 *
 * \param[in]       args contains nothing useful
 *
 * \return          None.
 *
 *****************************************************************************/
static void *fmNicTxThreadHandler(void *args)
{
    fm_int     sw;
    fm_status  err;
    fm_uint    intrSource;

    FM_NOT_USED(args);

    while (TRUE)
    {
        /**************************************************
         * Wait for a signal from the ISR.
         **************************************************/

        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_INTR, "Waiting for interrupt..\n");

        err = fmWaitSemaphore(&fmRootApi->nicPacketTxSemaphore, FM_WAIT_FOREVER);

        if (err != FM_OK)
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, "%s\n", fmErrorMsg(err) );

            continue;
        }

        for (sw = FM_FIRST_FOCALPOINT ; sw <= FM_LAST_FOCALPOINT ; sw++)
        {

            if (fmRootApi->fmSwitchStateTable[sw] == NULL ||
                !fmRootApi->isSwitchFibmSlave[sw])
            {
                continue;
            }

            /* Only interrupts triggered from packet send functions */
            err = fmPlatformGetInterrupt(sw,
                                         FM_INTERRUPT_SOURCE_API,
                                         &intrSource);

            if (err != FM_OK)
            {
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, "%s\n", fmErrorMsg(err) );

                continue;
            }

            if (intrSource == FM_INTERRUPT_SOURCE_NONE)
            {
                continue;
            }

            FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "Interrupt seen (source 0x%x)\n",
                         intrSource);


            /* Can't take switch lock, since switch might not be up when
             * first packet needs to send out. Just make sure the nic packet
             * send function can handle this properly
             */
            fmPlatformSendPackets(sw);

            if (intrSource & FM_INTERRUPT_SOURCE_ISR)
            {
                /* Re-enable the interrupt */
                err = fmPlatformEnableInterrupt(sw, intrSource);

                if (err != FM_OK)
                {
                    FM_LOG_FATAL( FM_LOG_CAT_PLATFORM, "%s\n", fmErrorMsg(err) );

                    continue;
                }

            }   /* end if (intrSource & FM_INTERRUPT_SOURCE_ISR) */

        }       /* end for (sw = FM_FIRST_FOCALPOINT ; sw <= FM_LAST_FOCALPOINT ; sw++) */

    }           /* end while (TRUE) */

    /**************************************************
     * Should never exit.
     **************************************************/

    FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, "Task exiting inadvertently!\n");
    return NULL;

}   /* end fmNicTxThreadHandler */



/*****************************************************************************/
/** fmFindSwitchByFibmGlort
 * \ingroup intPlatformFibm
 *
 * \desc            Called to find out which switch this glort belong to
 *                  Note: switch might not be up by now. This is also
 *                  platform specific of how the glort maps to the switch.
 *                  In this platform, we just use the fibm glort, which is
 *                  unique for each switch.
 *
 * \param[in]       socket is the socket where the packet is received.
 *
 * \param[in]       glort is the glort number.
 *
 * \param[out]      switchNum is the pointer to hold the switch number.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL if not found.
 *
 *****************************************************************************/
static fm_status fmFindSwitchByFibmGlort(fm_int socket,
                                         fm_uint32 glort, 
                                         fm_int *switchNum)
{
    fm_int    sw;
    fm_status err = FM_FAIL;
    fm_fibmSwitchConfig *config;

    FM_NOT_USED(socket);

#if 0
    /* This example assumes unique socket for each switch */

    FM_NOT_USED(glort);
    for (sw = FM_FIRST_FOCALPOINT ; sw <= FM_LAST_FOCALPOINT ; sw++)
    {
        if (socket == fmRootPlatform->fmPlatformState[sw].fibmSocket)
        {
            *switchNum = sw;
            return FM_OK;
        }
    }

#else
    /* This example assumes unique glort for each switch */

    FM_NOT_USED(socket);
    for (sw = FM_FIRST_FOCALPOINT ; sw <= FM_LAST_FOCALPOINT ; sw++)
    {
        config = &fmRootPlatform->fmPlatformState[sw].fibmConfig;
        if (config->slaveGlort == glort)
        {
            *switchNum = sw;
            return FM_OK;
        }
    }
#endif

    return err;

} /* end fmFindSwitchByFibmGlort */


/*****************************************************************************/
/** fmFindSwitchByGlort
 * \ingroup intPlatformFibm
 *
 * \desc            Called to find out which switch this glort belong to
 *
 * \param[in]       glort is the glort number.
 *
 * \param[out]      switchNum is the pointer to hold the switch number.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL if not found.
 *
 *****************************************************************************/
static fm_status fmFindSwitchByGlort(fm_uint32 glort, 
                                     fm_int *switchNum)
{
    fm_int    sw;
    fm_status err = FM_FAIL;

    for (sw = FM_FIRST_FOCALPOINT ; sw <= FM_LAST_FOCALPOINT ; sw++)
    {
        fm_switch *switchPtr;

        switchPtr = fmRootApi->fmSwitchStateTable[sw];

        if (!switchPtr)
        {
            continue;
        }
        /* Need to take a switch lock here ?? */
        /* Need to account for LAG */
        if ( (glort & ~switchPtr->glortRange.glortMask) ==
            switchPtr->glortRange.glortBase )
        {
            *switchNum = sw;
            return FM_OK;
        }
    }

    return err;

} /* end fmFindSwitchByGlort */


/*****************************************************************************/
/** fmNICHandleIntrSwitchDown
 * \ingroup intPlatformFibm
 *
 * \desc            Handles unknown interrupt.
 *
 * \param[in]       sw is the switch for the interrupt
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static void fmNICHandleIntrSwitchDown(fm_int sw)
{
    /* Probably it is best to give this info to another thread which verifies
     * that the switch is the correct one and bootstrap up sucessfully
     * Most of this code is dependent on the behaviour of each user platform.
     */

    /* Then we can send a switch insert event to automatically insert the sw */

    /* We are blindly insert the switch here for our test platform here */
    /* Either send on master switch or queue to itself when using the NIC */
    switch (fmRootPlatform->fmPlatformState[sw].family)
    {
        case FM_SWITCH_FAMILY_FM4000:
        case FM_SWITCH_FAMILY_REMOTE_FM4000:
            fm4000FibmAddSlaveSwitch(sw);
            break;


        case FM_SWITCH_FAMILY_FM6000:
        case FM_SWITCH_FAMILY_REMOTE_FM6000:
            fm6000FibmAddSlaveSwitch(sw);
            break;

        default:
            return;
    }
    
}

/*****************************************************************************/
/** fmNicProcessSocket
 * \ingroup intPlatformFibm
 *
 * \desc            Handles reception of packets by NIC socket.
 *
 * \param[in]       socket is the socket with data ready
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static void fmNicProcessSocket(fm_int socket)
{
    fm_status                     err = FM_OK;
    fm_int                        sw = 0;
    fm_int                        nextSw = 0;
    fm_switch *                   switchPtr;
    fm_buffer *                   firstBuffer;
    fm_buffer *                   lastBuffer;
    fm_buffer *                   buffer;
    fm_int                        packetLen;
    fm_int                        cnt;
    fm_uint32                     ISLTag[4];
    fm_uint32                     srcGlort;
    fm_bool                       isFibmPkt = FALSE;
    fm_uint32 *                   pData; /* Make it easier to access per uint32 */
    fm_int                        availableBuffers;
    fm_switchEventHandler         switchEventHandler = NULL;
    fm_bool                       selfTestEventHandlerFound = FALSE;

    /* get the number of available buffers from the buffer manager*/
    fmPlatformGetAvailableBuffers(&availableBuffers);

    if (availableBuffers <= FM_RECV_BUFFER_THRESHOLD)
    {
        /* wait for buffer to come back, before dequeueing data */
        fmYield();
        return;
    }

    for (cnt = 0 ; cnt < MAX_IO_VEC ; cnt++)
    {
        /* If empty allocate one */
        if (packetRxBuffer[cnt] == NULL)
        {
            buffer = fmAllocateBuffer(sw);
            if (buffer == NULL)
            {
                fmDbgGlobalDiagCountIncr(FM_GLOBAL_CTR_RX_OUT_OF_BUFFERS, 1);
                return;
            }
            rxVector[cnt].iov_base = buffer->data;
            rxVector[cnt].iov_len = FM_BUFFER_SIZE_BYTES;
            packetRxBuffer[cnt] = buffer;
        }
    }
    /* now receive from the driver */
    packetLen = readv(socket, rxVector, MAX_IO_VEC);
    if (packetLen < 20)
    {
        /* Should not get anything less than 20 bytes */
        return;
    }

    firstBuffer = packetRxBuffer[0];;
    pData       = (fm_uint32*)firstBuffer->data;

    ISLTag[0]   = ntohl(pData[3]);
    ISLTag[1]   = ntohl(pData[4]);
    srcGlort = (ISLTag[1] >> 16) & 0xffff;

    if ( (ISLTag[0] >> 30) == FM_FTYPE_MANAGEMENT )
    {
        isFibmPkt = TRUE;
        if (FM_OK != fmFindSwitchByFibmGlort(socket, srcGlort, &sw))
        {
            /* Fibm packet, but can't find the switch 
             * then just drop by doing nothing
             */
        
            /* Which stats to increase? */
            FM_LOG_WARNING(FM_LOG_CAT_EVENT_PKT_RX,
                "Unable to find switch from fibm glort 0x%04x\n",srcGlort);
            return;
        }

        fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_FIBM, 1);

        /* Normal interrupts should only arrive when the switch 
         * is up. If we get an interupt and switch is not up, 
         * then it could be interrupt posted after bootstrap is done
         */
        if ( (ntohl(pData[FM_FIBM_OFFSET_DATA]) == (fm_uint32) FM_FIBM_HW_INTR) &&
            !(fmRootApi->fmSwitchStateTable[sw] && 
              fmRootApi->fmSwitchStateTable[sw]->state == FM_SWITCH_STATE_UP) )
        {
            fmNICHandleIntrSwitchDown(sw);
            return;
        }

    } 
    else
    {
        /* Normal packet processing */
        if ( (FM_OK != fmFindSwitchByGlort(srcGlort, &sw)))
        {
            err = fmGetSwitchFirst(&nextSw);

            while (err == FM_OK)
            {
                sw = nextSw;

                fmGetSwitchEventHandler(sw, &switchEventHandler);
                switchPtr = fmRootApi->fmSwitchStateTable[sw];
                
                if (switchEventHandler == switchPtr->DbgSelfTestEventHandler)
                {
                    selfTestEventHandlerFound = TRUE;
                    break;
                }

                err = fmGetSwitchNext(sw, &nextSw);
            }

            /* Continue processing packet if we are in self test */
            if (selfTestEventHandlerFound != TRUE)
            {
                /* What stats to increase? */
                FM_LOG_WARNING(FM_LOG_CAT_EVENT_PKT_RX,
                    "Unable to find switch from glort 0x%04x\n",srcGlort);
                return;
            }
        }
    }

    /* figure out how many buffers needed, and chain them together */
    cnt = 0;
    lastBuffer = NULL; /* Just to make the compiler happy */
    while (packetLen)
    {

        if (cnt >= MAX_IO_VEC)
        {
            FM_LOG_FATAL(FM_LOG_CAT_EVENT_PKT_RX,
                "packetRxBuffer array size overflow.\n");
            break;
        }

        if (packetLen > FM_BUFFER_SIZE_BYTES)
        {
            packetLen -= FM_BUFFER_SIZE_BYTES;
            packetRxBuffer[cnt]->len = FM_BUFFER_SIZE_BYTES;
        }
        else
        {
            packetRxBuffer[cnt]->len = packetLen;
            packetLen = 0;
        }

        if (cnt > 0)
        {
            lastBuffer->next = packetRxBuffer[cnt];
        }
        lastBuffer = packetRxBuffer[cnt];
        packetRxBuffer[cnt]->next = NULL;
        packetRxBuffer[cnt] = NULL; /* reset to reallocate again*/
        cnt++;
    }

    /* Add an CRC len to make it similar to other interfaces */
    if (lastBuffer->len <= (FM_BUFFER_SIZE_BYTES - 4))
    {
        lastBuffer->len += 4;
    }
    else
    {
        /* Not enough space in last buffer, use the next one */
        lastBuffer->len = FM_BUFFER_SIZE_BYTES;
        if (cnt >= MAX_IO_VEC)
        {
            FM_LOG_FATAL(FM_LOG_CAT_EVENT_PKT_RX,
                "packetRxBuffer array size overflow.\n");
        }
        else
        {
            lastBuffer->next = packetRxBuffer[cnt];
            packetRxBuffer[cnt]->len = 4 - (FM_BUFFER_SIZE_BYTES - lastBuffer->len);
            packetRxBuffer[cnt]->next = NULL;
            packetRxBuffer[cnt] = NULL; /* reset to reallocate again*/
        }
        
    }

    /**************************************************
     * This is a mgmt message, let fibm handle it
     **************************************************/
    if (isFibmPkt)
    {
        /* We have to call here instead of the fm4000PacketReceiveProcess
         * processing, since the switch might not be up yet
         * to call in the function pointer handler
         */
        err = fmFibmProcessPktHandler(sw, firstBuffer, NULL);
        if (err != FM_OK)
        {
            FM_LOG_ERROR( FM_LOG_CAT_PLATFORM,
                          "Error returned from fmFibmProcessPktHandler: %s\n",
                          fmErrorMsg(err) );
        }
        return;
    }

    switchPtr = fmRootApi->fmSwitchStateTable[sw];

    /* Take switch lock here ? */
    if (switchPtr && switchPtr->state == FM_SWITCH_STATE_UP)
    {
        switch (fmRootPlatform->fmPlatformState[sw].family)
        {
            case FM_SWITCH_FAMILY_FM4000:
            case FM_SWITCH_FAMILY_REMOTE_FM4000:
                err = fm4000PacketReceiveProcess(sw, firstBuffer, NULL, 0);
                if (err != FM_OK)
                {
                    FM_LOG_ERROR( FM_LOG_CAT_PLATFORM,
                                  "Error returned from "
                                  "fm4000PacketReceiveProcess: %s\n",
                                  fmErrorMsg(err) );
                }
                break;


            case FM_SWITCH_FAMILY_FM6000:
            case FM_SWITCH_FAMILY_REMOTE_FM6000:
                err = fm6000PacketReceiveProcess(sw, firstBuffer, NULL, 0);
                if (err != FM_OK)
                {
                    FM_LOG_ERROR( FM_LOG_CAT_PLATFORM,
                                  "Error returned from "
                                  "fm6000PacketReceiveProcess: %s\n",
                                  fmErrorMsg(err) );
                }
                break;

            default:
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, "Invalid switch family\n");
        }
    }
    else
    {
        fmFreeBufferChain(sw, firstBuffer);
        /* Not correct stats */
        fmDbgDiagCountIncr(sw, FM_CTR_RX_PKT_DROPS_NO_PORT, 1);
    }
} /* end fmNicProcessSocket */


/*****************************************************************************/
/** fmNicReceivePackets
 * \ingroup intPlatformFibm
 *
 * \desc            Handles reception of packets by NIC sockets.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status fmNicReceivePackets(void)
{
    struct pollfd   rfds[FM_MAX_FDS_NUM];
    fm_int          rfdsCnt;
    fm_int          retval;
    fm_int          i;
    fm_int          sockFd;
    char            strErrBuf[FM_STRERROR_BUF_SIZE];
    errno_t         strErrNum;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_RX, "");

    /**************************************************
     * Loop forever calling packet receive handler.
     **************************************************/

    for ( ; ; )
    {
        
        if (socketListCnt <= 0)
        {
            /* No socket to read */
            fmYield();
            continue;
        }

        rfdsCnt = 0;

        for (i = 0 ; i < MAX_NUM_NIC ; i++)
        {
            sockFd = fmNic[i].sockFd;
            if (sockFd != -1)
            {
                rfds[rfdsCnt].fd = sockFd;
                rfds[rfdsCnt].events = POLLIN;
                rfds[rfdsCnt].revents = 0;
                rfdsCnt++;

                fmNic[i].inSelectList = TRUE;

#ifdef FM_FIBM_NIC_SELECT_TIMEOUT
                if (fmNic[i].isWaitingOnSemaphore)
                {
                    FM_LOG_DEBUG(FM_LOG_CAT_FIBM,"fmSignalSemaphore: fd=%d (i=%d)\n", sockFd, i);
                    fmNic[i].isWaitingOnSemaphore = FALSE;

                    /* Signal the semaphore to wake up the pending thread */
                    fmSignalSemaphore(&fmNic[i].selectSemaphore);
                }
#endif
            }
            else
            {
                fmNic[i].inSelectList = FALSE;
            }
        }

#ifdef FM_FIBM_NIC_SELECT_TIMEOUT
        retval = poll(rfds, rfdsCnt, FM_FIBM_NIC_SELECT_TIMEOUT * 1000);
#else
        retval = poll(rfds, rfdsCnt, -1);
#endif

        if (retval == -1)
        {
            strErrNum = FM_STRERROR_S(strErrBuf, FM_STRERROR_BUF_SIZE, errno);
            if (strErrNum == 0)
            {
                FM_LOG_WARNING(FM_LOG_CAT_SWITCH,
                               "ERROR: fmNicReceivePackets: select failed: %s!\n",
                               strErrBuf);
            }
            else
            {
                FM_LOG_WARNING(FM_LOG_CAT_SWITCH,
                               "ERROR: fmNicReceivePackets: select failed: %d!\n",
                               errno);
            }
            continue;
        }
        else if (!retval)
        {
            continue; /* timeout */
        }

        rfdsCnt = 0;

        for (i = 0 ; i < MAX_NUM_NIC ; i++)
        {
            sockFd = fmNic[i].sockFd;
            if (sockFd != -1 && rfds[rfdsCnt++].revents & POLLIN)
            {
                fmNicProcessSocket(sockFd);
            }
        }
    } /* end for ( ; ; ) */

    /**************************************************
     * Should never exit.
     **************************************************/

    FM_LOG_ERROR(FM_LOG_CAT_SWITCH,
                 "ERROR: fmNicReceivePackets: exiting inadvertently!\n");

    return FM_OK;

}   /* end fmNicReceivePackets */



/*****************************************************************************
 * Public Functions
 *****************************************************************************/


/*****************************************************************************/
/** fmNicGetDeviceName
 * \ingroup intPlatformFibm
 *
 * \desc            Return the device name for the given switch.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       devName is the device name where the switch is connected to.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmNicGetDeviceName(fm_int sw, fm_char *devName)
{
    fm_platform_state *ps;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, 
                 "sw = %d, devName = %p",
                 sw, 
                 (void*)devName);

    if (sw < 0 || sw > FM_MAX_NUM_FOCALPOINTS)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_SWITCH);
    }
    
    if (devName == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_FAIL);
    }

    ps = &fmRootPlatform->fmPlatformState[sw];

    FM_STRNCPY_S(devName, FM_DEV_NAME_LEN, ps->pNic->devName, FM_DEV_NAME_LEN);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);

}



/*****************************************************************************/
/** fmNicSetDeviceName
 * \ingroup intPlatformFibm
 *
 * \desc            Specifies the device name for each switch.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       devName is the device name where the switch is connected to.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmNicSetDeviceName(fm_int sw, fm_char *devName)
{
    fm_platform_state *ps;
    fm_nic            *currentNic;
    fm_nic            *newNic;
    fm_status          status;
    fm_int             i;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, 
                 "sw = %d, devName = %p",
                 sw, 
                 (void*)devName);
    
    if (sw < 0 || sw > FM_MAX_NUM_FOCALPOINTS)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_SWITCH);
    }
    
    if (devName == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_FAIL);
    }

    ps = &fmRootPlatform->fmPlatformState[sw];
    currentNic = ps->pNic;

    if (strcmp(currentNic->devName, devName) == 0)
    {
        /* This switch already runs on that NIC */
        newNic = currentNic;
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                     "Switch %d already attached to device %s\n",
                     sw,
                     devName);
    }
    else
    {
        /* Detach the switch from its current NIC */
        currentNic->numSwitches--;
    
        if (currentNic->numSwitches <= 0)
        {
            FM_LOG_DEBUG(FM_LOG_CAT_FIBM,"fmNicSetDeviceName: Close %s\n", currentNic->devName);

            /* There's no more switches attached to this NIC,
               so we can close its socket. */
            close(currentNic->sockFd);

            if (socketListCnt > 0)
                socketListCnt--;
    
            /* Reset the NIC structure */
            ResetNicStructure(currentNic);
        }
    
        newNic = NULL;
    
        /* See if the new devName already exists */
        for (i = 0 ; i < MAX_NUM_NIC ; i++)
        {
            if (strcmp(fmNic[i].devName, devName) == 0)
            {
                /* Yes it exists, attach the switch to it. */
                newNic = ps->pNic = &fmNic[i];
                newNic->numSwitches++;
                FM_LOG_DEBUG(FM_LOG_CAT_FIBM,"fmNicSetDeviceName: NIC %d dev: %s exist\n", i, newNic->devName);
                break;
            }
        }
    
        if (newNic == NULL)
        {
            /* The devName doesn't exist so find an free fm_nic */
            for (i = 0 ; i < MAX_NUM_NIC ; i++)
            {
                if (strcmp(fmNic[i].devName, "") == 0)
                {
                    /* Found an free fm_nic, attach the switch to it. */
                    newNic = ps->pNic = &fmNic[i];
                    newNic->numSwitches++;
                    FM_STRNCPY_S( newNic->devName,
                                  sizeof(newNic->devName),
                                  devName,
                                  FM_DEV_NAME_LEN);
                    FM_LOG_DEBUG(FM_LOG_CAT_FIBM,"fmNicSetDeviceName: Found free NIC %d dev: %s\n", i, newNic->devName);
                    break;
                }
            }
        }
    }

    if (newNic == NULL)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM, "newNic is still NULL!\n");
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_FAIL);
    }

    /* Open this NIC interface if not already opened */
    if (newNic->sockFd == -1)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_FIBM,"fmNicSetDeviceName: Open NIC %s\n", newNic->devName);
        status = fmNicDevOpen(newNic);
        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }
 
    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);

}



/*****************************************************************************/
/** fmNicPacketHandlingInitialize
 * \ingroup intPlatformFibm
 *
 * \desc            Performs initialization for the netdev packet transfer.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmNicPacketHandlingInitialize(void)
{
    fm_packetHandlingState *ps;
    fm_status               err = FM_OK;
    fm_int                  sw;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "");

    /* Use switch 0 */
    for (sw = FM_FIRST_FOCALPOINT ; sw <= FM_LAST_FOCALPOINT ; sw++)
    {
        ps = &fmRootPlatform->fmPlatformState[sw].packetState;

        /* clear out all state */
        memset( ps, 0, sizeof(fm_packetHandlingState) );

        fmPacketQueueInit(&ps->txQueue, sw);

        /* reset state here */
        ps->cachedEndianness     = -1; /* -1 indicates unspecified endianness */
    }

    /* initialize the signal sem for event availability */
    fmCreateSemaphore("netdevEventsAvailable",
                      FM_SEM_BINARY,
                      &fmRootPlatform->fmPlatformState[sw].packetState.eventsAvailableSignal, 
                      0);

    /* initialize fibm Packet Transmit Semaphore */
    err = fmCreateSemaphore("fibmPacketTxSemaphore",
                            FM_SEM_BINARY,
                            &fmRootApi->nicPacketTxSemaphore,
                            0);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

    /* initialize the NIC interface */
    err = fmNicInit();
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_PLATFORM, err);


    /* create thread to receive packets from the NIC */
    err = fmCreateThread("NIC Rx thread",
                         FM_EVENT_QUEUE_SIZE_NONE,
                         fmNicRxThreadHandler,
                         NULL,
                         &fmNicRxThread);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

    /* Create thread to send packets to the NIC */
    err = fmCreateThread("NIC Tx Task",
                         FM_EVENT_QUEUE_SIZE_NONE,
                         fmNicTxThreadHandler,
                         NULL,
                         &fmNicTxThread);

    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

    err = fmSignalSemaphore(&fmRootApi->packetReceiveSemaphore);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end fmNicPacketHandlingInitialize */



/*****************************************************************************/
/** fmNicSendPackets
 * \ingroup intPlatformFibm
 *
 * \desc            When called, iterates through the packet queue and
 *                  continues to send packets until either the queue empties.
 *
 * \param[in]       sw refers to the switch number to send packets to.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmNicSendPackets(fm_int sw)
{
    fm_packetQueue *        txQueue;
    fm_packetEntry *        packet;
    fm_status               err = FM_OK;
    fm_int32                rc;
    fm_buffer               *sendBuf;
    fm_int                  packetCount;
    struct iovec            vector[MAX_IO_VEC];
    fm_int                  vecCnt;
    fm_byte                 packetTxBuffer[32];
    char                    strErrBuf[FM_STRERROR_BUF_SIZE];
    errno_t                 strErrNum;


    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_TX, "sw = %d\n", sw);


    txQueue = &fmRootPlatform->fmPlatformState[sw].packetState.txQueue;

    fmPacketQueueLock(txQueue);    

    packetCount = 0;
    for ( ;
          txQueue->pullIndex != txQueue->pushIndex ;
          txQueue->pullIndex = (txQueue->pullIndex + 1) % FM_PACKET_QUEUE_SIZE)
    {
        packet = &txQueue->packetQueueList[txQueue->pullIndex];

        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_PKT_TX,
                     "fmNicSendPackets: sending packet in "
                     "slot %d, length=%d tag=%d\n",
                     txQueue->pullIndex, packet->length,
                     packet->suppressVlanTag);


        sendBuf = packet->packet;

        /* Copy the mac header */
        memcpy(packetTxBuffer, sendBuf->data, 12);

        /* Copy the ISL tag */
        *(fm_uint32 *)(packetTxBuffer+12) = htonl(packet->islTag.f64.tag[0]);
        *(fm_uint32 *)(packetTxBuffer+16) = htonl(packet->islTag.f64.tag[1]);

        vector[0].iov_base = packetTxBuffer;
        vector[0].iov_len = 20;

        if (packet->suppressVlanTag)
        {
            vector[1].iov_base = &sendBuf->data[4];
            vector[1].iov_len = sendBuf->len-16;
            if (vector[1].iov_len < 40)
            {
                /* Pad the buffer to 60 bytes */
                memset(((fm_byte*)&sendBuf->data[4]) + vector[1].iov_len,0,40-vector[1].iov_len);
                vector[1].iov_len = 40;
            }
        }
        else
        {
            vector[1].iov_base = &sendBuf->data[3];
            vector[1].iov_len = sendBuf->len-12;
            if (vector[1].iov_len < 40)
            {
                /* Pad the buffer to 60 bytes */
                memset(((fm_byte*)&sendBuf->data[3]) + vector[1].iov_len,0,40-vector[1].iov_len);
                vector[1].iov_len = 40;
            }
        }

        vecCnt = 2;

        /* Add the rest of the chain */
        for ( sendBuf = sendBuf->next ; sendBuf ; sendBuf = sendBuf->next )
        {
            if (vecCnt >= MAX_IO_VEC)
            {
                FM_LOG_FATAL(FM_LOG_CAT_EVENT_PKT_TX, "Packet chain too large\n");
                break;
            }
            vector[vecCnt].iov_base = &sendBuf->data;
            vector[vecCnt].iov_len = sendBuf->len;
            vecCnt++;
        }


        rc = writev(fmRootPlatform->fmPlatformState[sw].pNic->sockFd, vector, vecCnt);

        packetCount++;

        if (rc == -1)
        {
            strErrNum = FM_STRERROR_S(strErrBuf, FM_STRERROR_BUF_SIZE, errno);
            if (strErrNum == 0)
            {
                FM_LOG_ERROR(FM_LOG_CAT_EVENT_PKT_TX,
                             "send failed - rc %d [%s]\n", rc, strErrBuf);
            }
            else
            {
                FM_LOG_ERROR(FM_LOG_CAT_EVENT_PKT_TX,
                             "send failed - rc %d [%d]\n", rc, errno);
            }
            
            err = FM_FAIL;
        }
        else
        {
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

        /* Break out of the loop to release platform lock,
         * and go back again later
         */
        if (packetCount > 10)
        {
            err = fmPlatformTriggerInterrupt(sw, FM_INTERRUPT_SOURCE_API);
            break;
        }
    }

    fmPacketQueueUnlock(txQueue);

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);

}   /* end fmNicSendPackets */


/*****************************************************************************/
/** fmNicGetDeviceName
 * \ingroup intPlatformFibm
 *
 * \desc            Return the device name for the given switch.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmNicIsDeviceReady(fm_int sw)
{
    fm_platform_state *ps;
    fm_timestamp       timeout;
    fm_status          status;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "sw = %d\n", sw);

    if (sw < 0 || sw > FM_MAX_NUM_FOCALPOINTS)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_SWITCH);
    }
    
    ps = &fmRootPlatform->fmPlatformState[sw];

    if (!ps->pNic || ps->pNic->sockFd == -1)
    {
        status = FM_ERR_UNINITIALIZED;
    }
    else if (!ps->pNic->inSelectList)
    {
#ifdef FM_FIBM_NIC_SELECT_TIMEOUT
        timeout.sec  = FM_FIBM_NIC_SELECT_TIMEOUT + 1;
        timeout.usec = 0;

        ps->pNic->isWaitingOnSemaphore = TRUE;
        FM_LOG_DEBUG(FM_LOG_CAT_FIBM,"fmWaitSemaphore: FD=%d \n", ps->pNic->sockFd);
        status = fmWaitSemaphore(&ps->pNic->selectSemaphore, &timeout);

        FM_LOG_DEBUG(FM_LOG_CAT_FIBM,"fmWaitSemaphore: (%s)\n",fmErrorMsg(status));

        if (status == FM_ERR_SEM_TIMEOUT)
#endif
        {
            status = FM_ERR_UNINITIALIZED;
        }
    }
    else
    {
        status = FM_OK;
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);
}


