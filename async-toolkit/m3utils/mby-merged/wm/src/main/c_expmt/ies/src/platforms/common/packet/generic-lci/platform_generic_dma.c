/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            platform_generic_dma.c
 * Creation Date:   2006
 * Description:     DMA controller methods. This code assumes
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


#include <fm_sdk_fm2000_int.h>
#include <fm_sdk_fm4000_int.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define FM_RECV_PKT_MAX_BURST  32


/*****************************************************************************
 * Global Variables
 *****************************************************************************/


/*****************************************************************************
 * Local Variables
 *****************************************************************************/


/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

static void *fmPlatformDMAController(void *args);
static void fmPlatformDMAReceivePackets(fm_bool *rxEmpty);
static void fmPlatformDMATransmitPackets(void);


/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** fmPlatformDMAInitialize
 * \ingroup intPlatformCommon
 *
 * \desc            Called to initialize DMA controller. The initialization
 *                  is limited to create the DMA controller thread and
 *                  the semaphore used by this thread and setting the
 *                  starting switch for transmission.
 *
 * \param[in]       dma is a pointer to the dma controller structure
 *                  shared with the driver
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmPlatformDMAInitialize(fm_uint32 *dma)
{
    fm_int err = FM_OK;


    FM_LOG_ENTRY_NOARGS(FM_LOG_CAT_PLATFORM);

    /* Preserve a pointer to the dma controller */
    fmRootPlatform->dma = (volatile fm_dmaController *) dma;

    /* Create a semaphore for the DMA controller thread */
    err = fmCreateSemaphore("dmaSemaphore",
                            FM_SEM_BINARY,
                            &fmRootPlatform->dmaSemaphore,
                            0);

    if (err != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWITCH, err);
    }

    /* Create the DMA controller thread */
    err = fmCreateThread("dma_controller",
                         FM_EVENT_QUEUE_SIZE_NONE,
                         &fmPlatformDMAController,
                         NULL,
                         &fmRootPlatform->dmaThread);

    if (err != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);
    }

    /* Declare the DMA disabled */
    fmRootPlatform->dmaEnabled = FALSE;


    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end fmPlatformDMAInitialize */




/*****************************************************************************/
/** fmPlatformDMAEnable
 * \ingroup intPlatformCommon
 *
 * \desc            Called to either start or terminate DMA control.
 *
 * \param[in]       enable inidicates if the DMA controller is enabled or not.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmPlatformDMAEnable(fm_bool enable)
{
    volatile fm_dmaController *   dma = fmRootPlatform->dma;
    volatile fm_bufferDescriptor *bd;
    fm_buffer *                   buffer;
    fm_uint32                     offsets[2];
    fm_uint32                     status;
    fm_status                     err = FM_OK;
    fm_int                        i;
    fm_uint32                     version;
    int                           ioctlResult;
    fm_int                        sw;
    fm_status                     err2;
    char                          strErrBuf[FM_STRERROR_BUF_SIZE];
    errno_t                       strErrNum;

    FM_LOG_ENTRY_NOARGS(FM_LOG_CAT_PLATFORM);


    /* Check if it needs to be enabled and if it is disabled now */

    if (enable && !fmRootPlatform->dmaEnabled)
    {
        /* Go read CPLD version */

        err      = fmPlatformReadCPLD(FM_CPLD_VERSION, &version);
        version &= 0xFFFF;

        /* Check if it can be enabled. The CPLD must be at the
         *  right CPLD version and the code is only ready for FM4000. */

        if (version >= FM_CPLD_DMA_MIN_VERSION && version <= FM_CPLD_DMA_MAX_VERSION &&
            fmRootPlatform->fmPlatformState[0].family == FM_SWITCH_FAMILY_FM4000)
        {
            /* Clear the DMA controller structure */
            memset( (void *) dma, 0, sizeof(fm_dmaController) );

            /* Then enable it */
            fmRootPlatform->dmaEnabled         = TRUE;
            fmRootPlatform->dmaCurrentTxSwitch = FM_FIRST_FOCALPOINT;

            /* Disable LCI interrupts */
            for (i = 0 ; i < /*FM_MAX_NUM_FOCALPOINTS*/ 1 ; i++)
            {
                sw = i + FM_FIRST_FOCALPOINT;
                VALIDATE_AND_PROTECT_SWITCH_NO_RETURN(err2, sw);
                
                if (err2 == FM_OK)
                {
                    fmRootApi->fmSwitchStateTable[sw]->WriteUINT32(sw, 
                                                                   FM4000_LCI_IM, 
                                                                   -1); 
                    UNPROTECT_SWITCH(sw);
                }
            }

            /* Initialize driver (the driver will initialize the dma structure) */
            offsets[0] = FM4000_LCI_RX_FIFO;
            offsets[1] = FM4000_LCI_TX_FIFO;
            CPLD_TRACE(0xFFF, 0x87654321);
            ioctlResult = ioctl(fmPlatformProcessState[0].fd,
                                FM_IOCTL_START_DMA, &offsets);
            if (ioctlResult < 0)
            {
                strErrNum =
                    FM_STRERROR_S(strErrBuf, FM_STRERROR_BUF_SIZE, errno);
                if (strErrNum == 0)
                {   
                    FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                                 "ioctl failed with '%s'\n",
                                 strErrBuf);
                }
                else
                {
                    FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                                 "ioctl failed with '%d\n",
                                 errno);
                }
            }

            /* Allocate buffers for reception */
            for (i = 0 ; i < FM_DMA_RX_BUFFER_DESCRIPTORS - 1 ; i++)
            {
                bd          = &dma->rxBufferDescriptors[dma->rxFree];
                buffer      = fmAllocateBuffer(FM_FIRST_FOCALPOINT);
                if (buffer == NULL)
                {
                    FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                                 "Unable to allocate rx buffer %d!\n",
                                 i);
                }
                else
                {
                    bd->buffer  = buffer;
                    bd->data    = buffer->data;
                    bd->length  = FM_BUFFER_SIZE_WORDS;
                    dma->rxFree = (dma->rxFree + 1 ) % FM_DMA_RX_BUFFER_DESCRIPTORS;
                }
            }

            /* Instruct the driver to start to receive packets */
            ioctlResult = ioctl(fmPlatformProcessState[0].fd,
                                FM_IOCTL_RX_PACKETS, &status);
            if (ioctlResult < 0)
            {
                strErrNum =
                    FM_STRERROR_S(strErrBuf, FM_STRERROR_BUF_SIZE, errno);
                if (strErrNum == 0)
                {       
                    FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                                 "ioctl failed with '%s'\n",
                                 strErrBuf);
                }
                else
                {       
                    FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                                 "ioctl failed with '%d'\n",
                                 errno);
                }
            }
        }
        else
        {
            FM_LOG_WARNING(FM_LOG_CAT_PLATFORM,
                           "DMA cannot be used as CPLD not at level required\n");
        }

    }

    /* Check if it needs to be disabled and if it is enabled now */

    else if (!enable && fmRootPlatform->dmaEnabled)
    {
        /* Then disable it */
        fmRootPlatform->dmaEnabled = FALSE;

        /* Stop DMA */
        ioctlResult =
            ioctl(fmPlatformProcessState[0].fd, FM_IOCTL_STOP_DMA, &status);
        if (ioctlResult < 0)
        {
            strErrNum = FM_STRERROR_S(strErrBuf, FM_STRERROR_BUF_SIZE, errno);
            if (strErrNum == 0)
            {       
                FM_LOG_ERROR(FM_LOG_CAT_PLATFORM, "ioctl failed with '%s'\n",
                             strErrBuf);
            }
            else
            {       
                FM_LOG_ERROR(FM_LOG_CAT_PLATFORM, "ioctl failed with '%d'\n",
                             errno);
            }
        }

        /* Recover all BDs in RX */
        for (i = 0 ; i < FM_DMA_RX_BUFFER_DESCRIPTORS - 1 ; i++)
        {
            bd = &dma->rxBufferDescriptors[i];

            if (bd->buffer)
            {
                fmPlatformFreeBuffer(bd->buffer);
                bd->buffer = NULL;
            }

            bd->data = NULL;
        }

        /* Recover all BDs in TX */
        for (i = 0 ; i < FM_DMA_TX_BUFFER_DESCRIPTORS - 1 ; i++)
        {
            bd = &dma->txBufferDescriptors[i];

            if (bd->buffer && bd->free)
            {
                fmPlatformFreeBuffer(bd->buffer);
                bd->buffer = NULL;
                fmDbgGlobalDiagCountIncr(FM_GLOBAL_CTR_TX_BUFFER_FREES, 1);
            }

            bd->data = NULL;
        }

        /* Re-enable interrupt (only works for default switch 0 at this point) */
        for (i = 0 ; i < /*FM_MAX_NUM_FOCALPOINTS*/ 1 ; i++)
        {
            sw = i + FM_FIRST_FOCALPOINT;
            VALIDATE_AND_PROTECT_SWITCH_NO_RETURN(err2, sw);
            
            if (err2 == FM_OK)
            {
                fmRootApi->fmSwitchStateTable[sw]->WriteUINT32(sw, 
                                                               FM4000_LCI_IM, 
                                                               0); 
                UNPROTECT_SWITCH(sw);
            }
        }

    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end fmPlatformDMAEnable */




/*****************************************************************************/
/** fmPlatformDMAController
 * \ingroup intPlatformCommon
 *
 * \desc            DMA Controller Thread. This thread is waked up when
 *                  a semaphore is asserted and will cause the
 *                  the transmit and receive for the generic DMA
 *                  controller. This code assumed one driver globally
 *                  capable to handle multiple switches.
 *
 *****************************************************************************/

void *fmPlatformDMAController(void *args)
{
    fm_status  err;
    fm_bool    rxEmpty;

    FM_NOT_USED(args);

    /********************************************************************
     * Loop forever, waiting for signals from driver or application
     ********************************************************************/

    rxEmpty = TRUE;

    while (TRUE)
    {
        if (rxEmpty || !fmRootPlatform->dmaEnabled)
        {
            /****************************************************************
             * Wait for a signal from the ISR.
             ****************************************************************/

            FM_LOG_DEBUG(FM_LOG_CAT_EVENT_INTR, "Waiting for interrupt..\n");
            
            CPLD_TRACE(0x8AB, rxEmpty);
            err = fmWaitSemaphore(&fmRootPlatform->dmaSemaphore,
                                  FM_WAIT_FOREVER);
            if (err != FM_OK)
            {
                FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                             "fmWaitSemaphore failed with '%s'\n",
                             fmErrorMsg(err));
            }
        }

        /* Check for packets received and transmitted */

        fmPlatformDMAReceivePackets(&rxEmpty);
        fmPlatformDMATransmitPackets();

        /* Yield to other processes */

        fmYield();

    }

    return NULL;

}   /* end fmPlatformDMAController */




/*****************************************************************************/
/** fmPlatformDMAReceivePackets
 * \ingroup intPlatformCommon
 *
 * \desc            DMA Receive packets. The function scan the receive
 *                  queue and call an handler for each packet received.
 *                  If the handler cannot take the packet for any reason
 *                  then the packet stays at the top of the queue.
 *
 *****************************************************************************/

static void fmPlatformDMAReceivePackets(fm_bool *rxEmpty)
{
    volatile fm_bufferDescriptor *bd;
    volatile fm_dmaController *   dma = fmRootPlatform->dma;
    fm_int                        err = FM_OK;
    fm_int                        burst;
    fm_int                        head;
    fm_int                        sw;
    fm_int                        n;
    fm_bool                       restart;
    fm_buffer *                   buffer;
    fm_int                        isr;
    fm_int                        rv;
    char                          strErrBuf[FM_STRERROR_BUF_SIZE];
    errno_t                       strErrNum;

    FM_LOG_ENTRY_NOARGS(FM_LOG_CAT_PLATFORM);

    *rxEmpty = FALSE;

    for (burst = 0 ; burst < FM_RECV_PKT_MAX_BURST ; burst++)
    {
        CPLD_TRACE(0x800, dma->rxTail);
        CPLD_TRACE(0x801, dma->rxFree);
        CPLD_TRACE(0x802, dma->rxHead);

        /* Get out if there are no packets */
        if (dma->rxHead == dma->rxTail)
        {
            *rxEmpty = TRUE;

            /* Indicate to the driver that the API receiver is waiting for
               new packets. */               
            dma->rxWait = TRUE;
            break;
        }

        /* Determine where the packet is coming from */
        head = dma->rxHead;
        sw   = dma->rxBufferDescriptors[head].sw;

        /* Dispatch to the proper handler */

        switch (fmRootPlatform->fmPlatformState[sw].family)
        {
            case FM_SWITCH_FAMILY_FM4000:
                err = fm4000LCIReceivedPacketDMA(sw);
                break;

            case FM_SWITCH_FAMILY_FM2000:
                /* Not supported for now */
                break;

            default:
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                             "Invalid switch family\n");

        }   /* end switch (fmRootPlatform->fmPlatformState[sw].family) */

        /* If the packet couldn't be accepted, then get out and yield */
        if (err != FM_OK)
        {
            break;
        }

    }

    /*****************************************************************
     * Replenish the free pool and restart driver if it was stopped
     *****************************************************************/

    /* Compute number of BDs available */
    if (dma->rxFree > dma->rxHead)
    {
        n = FM_DMA_RX_BUFFER_DESCRIPTORS - dma->rxFree + dma->rxHead - 1;
    }
    else
    {
        n = dma->rxHead - dma->rxFree - 1;
    }

    CPLD_TRACE(0x803, n);

    /* Replenish the pool */
    restart = FALSE;

    while (n)
    {
        buffer = fmAllocateBuffer(FM_FIRST_FOCALPOINT);

        if (buffer == NULL)
        {
            break;
        }

        bd               = &dma->rxBufferDescriptors[dma->rxFree];
        bd->buffer       = buffer;
        bd->data         = buffer->data;
        bd->length       = FM_BUFFER_SIZE_WORDS;
        bd->free         = FALSE;
        bd->headerLength = 0;
        bd->endOfPacket  = FALSE;
        dma->rxFree      = (dma->rxFree + 1) % FM_DMA_RX_BUFFER_DESCRIPTORS;
        restart          = TRUE;
        n--;
    }

    /* Check if fully replenished or waiting for more buffers */
    if (n)
    {
        CPLD_TRACE(0x8AC, n);
        /* No buffers available. Increment relavant statistic. */
        fmDbgGlobalDiagCountIncr(FM_GLOBAL_CTR_RX_OUT_OF_BUFFERS, 1);

        /* Set the out of buffers flag so that when the
         * the next buffer is freed, we will get notified */
        fmRootApi->fmSwitchStateTable[0]->buffersNeeded = TRUE;

    }

    /* Restart driver if it was stopped and we gave at least one buffer... */

    if ( restart && dma->rxIdle )
    {        
        CPLD_TRACE(0x804, dma->rxTail);  
        CPLD_TRACE(0x805, dma->rxFree);
        rv = ioctl(fmPlatformProcessState[0].fd, FM_IOCTL_RX_PACKETS, &isr);
        if (rv < 0)
        {
            strErrNum = FM_STRERROR_S(strErrBuf, FM_STRERROR_BUF_SIZE, errno);
            if (strErrNum == 0)
            {
                FM_LOG_ERROR(FM_LOG_CAT_PLATFORM, "ioctl failed with '%s'\n",
                         strErrBuf);
            }
            else
            {
                FM_LOG_ERROR(FM_LOG_CAT_PLATFORM, "ioctl failed with '%d'\n",
                         errno);
            }
        }
    }

    FM_LOG_EXIT_VOID(FM_LOG_CAT_PLATFORM);

}   /* end fmPlatformDMAReceivePackets */




/*****************************************************************************/
/** fmPlatformDMATransmitPackets
 * \ingroup intPlatformCommon
 *
 * \desc            DMA Transmit Packets.
 *
 *****************************************************************************/

static void fmPlatformDMATransmitPackets()
{
    volatile fm_dmaController *   dma = fmRootPlatform->dma;
    volatile fm_bufferDescriptor *bd;
    fm_bool                       full;
    fm_bool                       empty;
    fm_int                        err;
    fm_int                        sw;
    fm_int                        burst;
    fm_int                        isr;
    fm_int                        rv;
    fm_int                        atLeastOneTransmitted;
    fm_int                        lastTransmitted;
    fm_int                        n;
    char                          strErrBuf[FM_STRERROR_BUF_SIZE];
    errno_t                       strErrNum;

    CPLD_TRACE(0x100, dma->txFree);
    CPLD_TRACE(0x101, dma->txHead);
    CPLD_TRACE(0x102, dma->txTail);
    FM_LOG_ENTRY_NOARGS(FM_LOG_CAT_PLATFORM);

    /* Free any packets that were transmitted */

    n = 0;

    while (dma->txFree != dma->txHead)
    {
        /* Pick up pointer to buffer descriptor */
        bd = &dma->txBufferDescriptors[dma->txFree];

        /* Free this buffer if required */
        if (bd->free)
        {
            err = fmPlatformFreeBuffer(bd->buffer);

            if (err)
            {
                FM_LOG_WARNING(FM_LOG_CAT_PLATFORM, "can't free buffer err=%d", err);
            }

            fmDbgGlobalDiagCountIncr(FM_GLOBAL_CTR_TX_BUFFER_FREES, 1);
            n++;
        }

        /* Count this frame as transmitted if this BD hold end of frame */
        if (bd->endOfPacket)
        {
            sw = bd->sw;
            fmDbgDiagCountIncr(sw, FM_CTR_TX_PKT_COMPLETE, 1);
        }

        /* Move the entry ahead */
        dma->txFree = (dma->txFree + 1) % FM_DMA_TX_BUFFER_DESCRIPTORS;
    }

    if (n > 0)
    {
        CPLD_TRACE(0x103, n);
    }

    /* Now scan all switches as long as there are room and some
     *  buffers to send for each one and we are not running for too
     *  long */

    sw                    = fmRootPlatform->dmaCurrentTxSwitch;
    atLeastOneTransmitted = FALSE;
    lastTransmitted       = sw;

    for (burst = 0 ; burst < FM_RECV_PKT_MAX_BURST ; burst++)
    {
        /* Dispatch to the proper handler. This handler will attempt
         *  to send one packet. */

        switch (fmRootPlatform->fmPlatformState[sw].family)
        {
            case FM_SWITCH_FAMILY_FM4000:
                err = fm4000LCISendPacketDMA(sw, &full, &empty);
                if (err != FM_OK)
                {
                    FM_LOG_ERROR( FM_LOG_CAT_PLATFORM,
                                  "fm4000LCISendPacketDMA returned error: %s\n",
                                  fmErrorMsg(err) );
                }
                break;

            case FM_SWITCH_FAMILY_FM2000:
                /* Not supported for now */
                full = TRUE;    /* need to set full on all execution paths */
                empty = TRUE;   /* need to set empty on all execution paths */
                break;

            default:
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                             "Invalid switch family\n");
                full = TRUE;    /* need to set full on all execution paths */
                empty = TRUE;   /* need to set empty on all execution paths */

        }   /* end switch (fmRootPlatform->fmPlatformState[sw].family) */

        /* Check if at least one packet was transmitted */
        if (!empty && !full)
        {
            atLeastOneTransmitted = TRUE;
            lastTransmitted       = sw;
        }

        /* Check if the DMA queue was full */
        if (full)
        {
            /* If the DMA was full, then just get out of this loop as nothing
             *  was queued */
            break;
        }
        else
        {
            /* If the DMA was not full, then go to the next switch
             *  sw++;
             *  if (sw > FM_LAST_FOCALPOINT)
             *  {
             *   sw = FM_FIRST_FOCALPOINT;
             *  }
             */
        }

        CPLD_TRACE(0x104, dma->txIdle);
        CPLD_TRACE(0x105, dma->txTail);
        CPLD_TRACE(0x106, dma->txHead);

        /* Restart the driver if needed */
        if (dma->txIdle && dma->txTail != dma->txHead)
        {
            CPLD_TRACE(0x107, 0);
            dma->txIdle = FALSE;
            rv          = ioctl(fmPlatformProcessState[0].fd,
                                FM_IOCTL_TX_PACKETS, &isr);
            if (rv < 0)
            {
                strErrNum =
                    FM_STRERROR_S(strErrBuf, FM_STRERROR_BUF_SIZE, errno);
                if (strErrNum == 0)
                {
                    FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                                 "ioctl failed with '%s'\n",
                                 strErrBuf);
                }
                else
                {
                    FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                                 "ioctl failed with '%d'\n",
                                 errno);
                }
            }
        }

        /* If all switches were allowed to transmit (back to the last one that
         *  transmitted anything) and none has transmitted anything, then
         *  get out of this loop. */

        if (sw == lastTransmitted)
        {
            if (!atLeastOneTransmitted)
            {
                break;
            }

            atLeastOneTransmitted = FALSE;
        }

    }

    /* Always remember which switch needs to be processed next time */

    fmRootPlatform->dmaCurrentTxSwitch = sw;

    FM_LOG_EXIT_VOID(FM_LOG_CAT_PLATFORM);

}   /* end fmPlatformDMATransmitPackets */




#define __FM_PLATFORM_KERNEL_H_PART_2

#include <platforms/fm85xxep/kernel/platform_kernel.h>


static void fmPlatformDMADebugVar(fm_char *s, void *p);


static void fmPlatformDMADebugVar(fm_char *s, void *p)
{
    printf( "%40s (0x%p) = 0x%8.8X\n", s, p, *( (fm_uint32 *) p ) );

}   /* end fmPlatformDMADebugVar */




void fmPlatformDMADebug(fm_int arg1, fm_int arg2, fm_int arg3)
{
    volatile fm_dmaController *dma = fmRootPlatform->dma;
    volatile fm_dmaDriver *    dmaDriver;
    fm_uint32 *                ptr;
    fm_int                     i;
    fm_uint32                  uint32Value;
    fm_char                    varName[50];

    dmaDriver = (fm_dmaDriver *) ( (fm_uint32) dma + 
                                   dma->driverControllerOffset ); 
    switch (arg1)
    {
        case 0:
            printf("dma = %p dmaDriver = %p \n", (void *) dma, (void *) dmaDriver);
            for (i = 0 ;i < FM_DMA_RX_BUFFER_DESCRIPTORS ; i++)
            {
                FM_SNPRINTF_S( varName,
                               sizeof(varName),
                               "dmaController.rxBufferDescriptors[%d]",
                               i);
                fmPlatformDMADebugVar(varName,
                                      (void *) &dma->rxBufferDescriptors[i]);
            }
            for (i = 0 ;i < FM_DMA_TX_BUFFER_DESCRIPTORS ; i++)
            {
                FM_SNPRINTF_S( varName,
                               sizeof(varName),
                               "dmaController.txBufferDescriptors[%d]",
                               i);
                fmPlatformDMADebugVar(varName,
                                      (void *) &dma->txBufferDescriptors[i]);
            }
            fmPlatformDMADebugVar("dmaController.rxHead",
                                  (void *) &dma->rxHead);
            fmPlatformDMADebugVar("dmaController.rxtail",
                                  (void *) &dma->rxTail);
            fmPlatformDMADebugVar("dmaController.rxFree",
                                  (void *) &dma->rxFree);
            fmPlatformDMADebugVar("dmaController.txHead",
                                  (void *) &dma->txHead);
            fmPlatformDMADebugVar("dmaController.txTail",
                                  (void *) &dma->txTail);
            fmPlatformDMADebugVar("dmaController.txFree",
                                  (void *) &dma->txFree);
            fmPlatformDMADebugVar("dmaController.txIdle",
                                  (void *) &dma->txIdle);
            fmPlatformDMADebugVar("dmaController.rxWait",
                                  (void *) &dma->rxWait);
            fmPlatformDMADebugVar("dmaController.rxIdle",
                                  (void *) &dma->rxIdle);
            fmPlatformDMADebugVar("dmaController.driverControllerOffset",
                                  (void *) &dma->driverControllerOffset);
            break;

        case 1:
            fmPlatformDMADebugVar("dmaDriver.rxDataBuffer",
                                  (void *) &dmaDriver->rxDataBuffer);
            fmPlatformDMADebugVar("dmaDriver.rxLengthBuffer",
                                  (void *) &dmaDriver->rxLengthBuffer);
            fmPlatformDMADebugVar("dmaDriver.dma0LinkDescriptors",
                                  (void *) &dmaDriver->dma0LinkDescriptors);
            fmPlatformDMADebugVar("dmaDriver.dma1LinkDescriptors",
                                  (void *) &dmaDriver->dma1LinkDescriptors);
            fmPlatformDMADebugVar("dmaDriver.dma2LinkDescriptors",
                                  (void *) &dmaDriver->dma2LinkDescriptors);
            fmPlatformDMADebugVar("dmaDriver.dma3LinkDescriptors",
                                  (void *) &dmaDriver->dma3LinkDescriptors);
            fmPlatformDMADebugVar("dmaDriver.dma0Tail",
                                  (void *) &dmaDriver->dma0Tail);
            fmPlatformDMADebugVar("dmaDriver.dma0Head",
                                  (void *) &dmaDriver->dma0Head);
            fmPlatformDMADebugVar("dmaDriver.dma1Tail",
                                  (void *) &dmaDriver->dma1Tail);
            fmPlatformDMADebugVar("dmaDriver.dma1Head",
                                  (void *) &dmaDriver->dma1Head);
            fmPlatformDMADebugVar("dmaDriver.dma2LinkTail",
                                  (void *) &dmaDriver->dma2LinkTail);
            fmPlatformDMADebugVar("dmaDriver.dma2LinkHead",
                                  (void *) &dmaDriver->dma2LinkHead);
            fmPlatformDMADebugVar("dmaDriver.dma3LinkTail",
                                  (void *) &dmaDriver->dma3LinkTail);
            fmPlatformDMADebugVar("dmaDriver.dma3LinkHead",
                                  (void *) &dmaDriver->dma3LinkHead);
            fmPlatformDMADebugVar("dmaDriver.dma2BufferTails",
                                  (void *) &dmaDriver->dma2BufferTails);
            fmPlatformDMADebugVar("dmaDriver.dma3BufferTails",
                                  (void *) &dmaDriver->dma3BufferTails);
            fmPlatformDMADebugVar("dmaDriver.rxDataHead",
                                  (void *) &dmaDriver->rxDataHead);
            fmPlatformDMADebugVar("dmaDriver.rxLengthHead",
                                  (void *) &dmaDriver->rxLengthHead);
            fmPlatformDMADebugVar("dmaDriver.rxDataTail",
                                  (void *) &dmaDriver->rxDataTail);
            fmPlatformDMADebugVar("dmaDriver.rxLengthTail",
                                  (void *) &dmaDriver->rxLengthTail);
            fmPlatformDMADebugVar("dmaDriver.dmaRxFifoAddr",
                                  (void *) &dmaDriver->dmaRxFifoAddr);
            fmPlatformDMADebugVar("dmaDriver.dmaTxFifoAddr",
                                  (void *) &dmaDriver->dmaTxFifoAddr);
            fmPlatformDMADebugVar("dmaDriver.dma0Running",
                                  (void *) &dmaDriver->dma0Running);
            fmPlatformDMADebugVar("dmaDriver.dma1Running",
                                  (void *) &dmaDriver->dma1Running);
            fmPlatformDMADebugVar("dmaDriver.dma2Running",
                                  (void *) &dmaDriver->dma2Running);
            fmPlatformDMADebugVar("dmaDriver.dma3Running",
                                  (void *) &dmaDriver->dma3Running);
            fmPlatformDMADebugVar("dmaDriver.isRxPacketInProgress",
                                  (void *) &dmaDriver->isRxPacketInProgress);
            fmPlatformDMADebugVar("dmaDriver.isRecoveringDma0DescriptorsInProgress",
                                  (void *) &dmaDriver->isRecoveringDma0DescriptorsInProgress);
            fmPlatformDMADebugVar("dmaDriver.isRecoveringDma1DescriptorsInProgress",
                                  (void *) &dmaDriver->isRecoveringDma1DescriptorsInProgress);
            fmPlatformDMADebugVar("dmaDriver.rxWait",
                                  (void *) &dmaDriver->rxWait);
            fmPlatformDMADebugVar("dmaDriver.txWait",
                                  (void *) &dmaDriver->txWait);
            fmPlatformDMADebugVar("dmaDriver.dmaTxStopCommand",
                                  (void *) &dmaDriver->dmaTxStopCommand);
            fmPlatformDMADebugVar("dmaDriver.dmaRxStopCommand",
                                  (void *) &dmaDriver->dmaRxStopCommand);
            break;

        case 2:
            ptr = (fm_uint32 *) &dmaDriver->rxDataBuffer[arg2];

            for (i = arg2 ; i <= arg3 ; i++)
            {
                printf("dmaDriver.rxDataBuffer[%d] = 0x%8.8X\n", i, ptr[i]);
            }

            break;

        case 3:
            ptr = (fm_uint32 *) &dmaDriver->rxLengthBuffer[arg2];

            for (i = arg2 ; i <= arg3 ; i++)
            {
                uint32Value = ptr[i];
                printf("dmaDriver.rxLengthBuffer[%d] (0x%p) = 0x%8.8X\n", 
                       i, (void *) &ptr[i], (uint32Value & 0xffff) );
            }

            break;

    }   /* end switch (arg1) */

}   /* end fmPlatformDMADebug */
