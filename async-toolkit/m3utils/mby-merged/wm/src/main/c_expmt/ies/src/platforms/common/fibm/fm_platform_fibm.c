/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_platform_fibm.c
 * Creation Date:   June 20, 2008
 * Description:     Platform FIBM implementation
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
#include <platforms/common/packet/generic-packet/fm6000/fm6000_generic_tx.h>
#include <platforms/common/fibm/fm6000/fm6000_fibm.h>


/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/
typedef enum
{
    FIBM_SEND_MODE_SYNC,  /* Wait for each write transaction complete */
    FIBM_SEND_MODE_ASYNC, /* Don't need to wait for write transation complete */
    FIBM_SEND_MODE_RESEND /* FIBM message resent mode */

} fm_fibmSendMode;

#define FIBM_DBG(...)  printf("DEBUG: " __VA_ARGS__);

/* Reserve some space at buffer end, just in case some calculation is off */
#define FIBM_RESERVED_GAP 16

/*****************************************************************************
 * Global Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/* Thread for handling FIBM retries */
fm_thread    fibmProcessTask;

/* Semaphore for holding the thread until needed to be waked up or timeout
 * signaled to be waked up only required when batching mode is enabled
 */
fm_semaphore fibmProcessTaskSem;

/* Debug stuff to be removed */
fm_uint      fibmDropPkt = 0;
fm_uint      fibmDbg     = 0;

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/


/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/* Debug for removal */
static void fmFibmDumpBuf(fm_uint32 *data, fm_uint len)
{
    fm_uint32 cnt;
    fm_byte *bytes;

    len *=4;
    bytes = (fm_byte *) data;
    for (cnt = 0 ; cnt < len ; cnt++)
    {
        printf("%02x", bytes[cnt]);
        if (cnt % 4 == 3)
        {
            printf(" ");
        }

        if (cnt % 32 == 31)
        {
            printf("\n");
        }
    }

    printf("\n");

} /* fmFibmDumpBuf */



/*****************************************************************************/
/** GetMasterSwitch
 * \ingroup intFibm
 *
 * \desc            Helper function to return the master switch.
 *
 * \return          master switch for master slave mode.
 * \return          self switch for standalone NIC mode.
 *
 *****************************************************************************/
static fm_int GetMasterSwitch(fm_int sw)
{
    if (FIBM_INFO(sw)->cfg.masterSwitch < 0)
    {
        /* Standalone NIC mode */
        return sw;
    }

    return FIBM_INFO(sw)->cfg.masterSwitch;
}


/*****************************************************************************/
/** fmFibmFindSwitchByGlort
 * \ingroup intFibm
 *
 * \desc            Helper function to  find corresponding switch given a glort.
 *
 * \return          switch number.
 * \return          -1 if not found.
 *
 *****************************************************************************/
fm_int fmFibmFindSwitchByGlort(fm_uint32 glort)
{
    fm_int sw;

    for (sw = FM_FIRST_FOCALPOINT ; sw <= FM_LAST_FOCALPOINT ; sw++)
    {
        if ( !FIBM_INFO_VALID(sw) )
        {
            continue;
        }

        if (FIBM_INFO(sw)->cfg.slaveGlort == glort)
        {
            return sw;
        }
    }

    FM_LOG_DEBUG(FM_LOG_CAT_FIBM, "Unable to find switch for glort %x\n", glort);
    return -1;

}   /* end fmFibmFindSwitchByGlort */




/*****************************************************************************/
/** fmSendFibmInt
 * \ingroup intFibm
 *
 * \desc            Common send function used by fmSendFibm and fmReSendFibm.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       buf is buffer containing the FIBM message to be sent.
 *
 * \param[in]       mode is the different FIBM_SEND_MODE_XXX.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
static fm_status fmSendFibmInt(fm_int sw, fm_buffer *buf, fm_fibmSendMode mode)
{
    fm_status status;
    fm_islTag islTag;

    TAKE_FIBM_TXRX_LOCK(sw);

    memset( &islTag, 0, sizeof(islTag) );
    islTag.f64.tag[0] = 0 |                            /* vlanid */
                        (0 << 12) |                    /* vlan priority */
                        (0 << 16) |                    /* user */
                        (0xF << 24) |                  /* swpri */
                        (0 << 28) |                    /* VTYPE or MTYPE*/
                        (0x3 << 30);                   /* FTYPE */
    islTag.f64.tag[1] = (FIBM_INFO(sw)->cfg.masterGlort << 16) | 
                         FIBM_INFO(sw)->cfg.slaveGlort;

    /* bug 11966: Pad the response message if expected length is 12 */
    if (FIBM_INFO(sw)->respLen == 12)
    {
        /* Add some command to make response not equal to 12 words */
        buf->data[buf->len/4] =  htonl(0x21040304);
        buf->len += 4;
    }

    if (fibmDbg)
    {
        FIBM_DBG("Sw#%u: FIBM TX SeqTag: %u Len: %u ISL0: %08x ISL1: %08x\n",
                 sw, FIBM_INFO(sw)->seqTag, buf->len, 
                 islTag.f64.tag[0], islTag.f64.tag[1]);

        fmFibmDumpBuf(buf->data, buf->len / 4);
    }

    /* Increment sentCount here before sending since 
     * comming back from the send function might be after the 
     * receive thread receiving the message. And we use this
     * to check if we are expecting a message or not
     */
    FIBM_INFO(sw)->sentCount++;

    FIBM_INFO(sw)->timeoutCount = 0;

    if (mode == FIBM_SEND_MODE_ASYNC)
    {
        FIBM_STATS(sw).request++;
        /* Don't need to wait for response here */
        FIBM_INFO(sw)->flags &= ~FM_FIBM_FLAGS_WAIT_RESPONSE;
    }
    else if (mode == FIBM_SEND_MODE_RESEND)
    {
        if (FIBM_INFO(sw)->sentCount == 2)
        {
            FIBM_STATS(sw).resendOnce++;
        }
        else if (FIBM_INFO(sw)->sentCount == 3)
        {
            FIBM_STATS(sw).resendOnce--;
            FIBM_STATS(sw).resendTwiceOrMore++;
        }
    }
    else
    {
        FIBM_STATS(sw).request++;
        FIBM_INFO(sw)->flags |= FM_FIBM_FLAGS_WAIT_RESPONSE;
        FIBM_INFO(sw)->respTimeout = FALSE;
    }

    /* Either send on master switch or queue to itself when using the NIC */
    switch (GET_PLAT_STATE(sw)->family)
    {
#ifdef FM_SUPPORT_FM4000
        case FM_SWITCH_FAMILY_FM4000:
        case FM_SWITCH_FAMILY_REMOTE_FM4000:
            status = fm4000GenericSendPacketISL(GetMasterSwitch(sw),
                                                &islTag,
                                                1,
                                                buf);
            break;
#endif
       
        case FM_SWITCH_FAMILY_FM6000:
        case FM_SWITCH_FAMILY_REMOTE_FM6000:
            status = fm6000GenericSendPacketISL(GetMasterSwitch(sw),
                                                &islTag,
                                                1,
                                                buf);
            break;

        default:
            status = FM_FAIL;
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, "Invalid switch family\n");
    }

    if (status != FM_OK)
    {
        FIBM_STATS(sw).sendFail++;
        fmFreeBufferChain(sw, buf);
        /* Allow to continue
         * Handle this as a packet is being dropped during transmission
         * So just let the fibm task to handle resend, and eventually
         * timeout if retries still don't work
         */
    }

    /* Always do this code, even send returns error */
    if (mode == FIBM_SEND_MODE_SYNC)
    {
        /* wait for response */
        DROP_FIBM_TXRX_LOCK(sw);
        status = fmWaitSemaphore(&FIBM_INFO(sw)->fibmResponseSemaphore,
                                 FM_WAIT_FOREVER);
        return status;
        }

    DROP_FIBM_TXRX_LOCK(sw);
    return FM_OK;

}   /* end fmSendFibmInt */




/*****************************************************************************/
/** fmSendFibm
 * \ingroup intFibm
 *
 * \desc            For sending FIBM message to slave/remote switch.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       mode is the different FIBM_SEND_MODE_XXX.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmSendFibm(fm_int sw, fm_fibmSendMode mode)
{
    fm_buffer *       buf;
    unsigned char *   eh;

    buf = FIBM_INFO(sw)->buffer;

    if (!buf)
    {
        return FM_ERR_NO_MEM;
    }

    /* Current implementation only permits one message sent at a time
     * This will ensure ordering of register read and write
     */
    if (FIBM_INFO(sw)->sentCount)
    {
        FIBM_DBG("Unexpected sentCount %llu (sw %d) while sending new message\n",
                 FIBM_INFO(sw)->sentCount,sw);
        return FM_FAIL;
    }

    /* NULL the buffer so next buffer can be allocated */
    FIBM_INFO(sw)->buffer = NULL;

    if (!GET_PLAT_STATE(sw)->useNicMacAddr)
    {
        /* Dest and Src Mac are not important */
        /* Place some debug info in the mac addresses */
        buf->data[0x0] = htonl(0xeeeeeee0 | sw);
        buf->data[0x1] = htonl(0xeeee0000 | (GetMasterSwitch(sw)&0xF) << 12);
        buf->data[0x2] = htonl((FIBM_INFO(sw)->sentCount << 28) | 
                               (FIBM_STATS(sw).request&0xFFFFFF));
    }
    else
    {
        /* The Dest Mac is: ee:ee:ee:ee:ee:ex, where 'x' is the switch number */
        eh = (unsigned char *) buf->data;
        buf->data[0x0] = htonl(0xeeeeeee0 | sw);
        eh[4] = 0xee;
        eh[5] = 0xee;

        /* Use the NIC Mac as Src Mac */
        memcpy(&eh[6], GET_PLAT_STATE(sw)->pNic->nicMacAddr, 6);
    }

    /* Ethertype and FIBM Tag */
    buf->data[FM_FIBM_OFFSET_ETHERTYPE] = htonl((FM_FIBM_ETHERTYPE << 16) | (FIBM_INFO(sw)->seqTag & 0xFFFF));

    /* Hardware will take care of minimum Ethernet packet length */

    /* This buf->len is in fm_uint32, so need to convert to bytes */
    buf->len *= 4;

    /* Save message so we can resend if lost */
    memcpy(FIBM_INFO(sw)->sentBuffer->data, buf->data, buf->len);
    FIBM_INFO(sw)->sentBuffer->len = buf->len;

    return fmSendFibmInt(sw, buf, mode);

}   /* end fmSendFibm */




/*****************************************************************************/
/** fmReSendFibm
 * \ingroup intFibm
 *
 * \desc            For resending FIBM message if ACK timeouts.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmReSendFibm(fm_int sw)
{
    fm_buffer *buf;

    buf = fmAllocateBuffer(sw);

    if (!buf)
    {
        return FM_ERR_NO_MEM;
    }

    /* Copy over old data */
    buf->len = FIBM_INFO(sw)->sentBuffer->len;
    memcpy(buf->data, FIBM_INFO(sw)->sentBuffer->data, buf->len);

    if (!GET_PLAT_STATE(sw)->useNicMacAddr)
    {
        /* Place some debug info in the mac addresses
         * This allows us to check for unexpected packets
         */
        buf->data[0x2] = htonl((FIBM_INFO(sw)->sentCount << 28) +
                               (FIBM_STATS(sw).request&0xFFFFFF));
    }
    return fmSendFibmInt(sw, buf, FIBM_SEND_MODE_RESEND);

}   /* end fmReSendFibm */




/*****************************************************************************/
/** fmFibmStartBatchingInt
 * \ingroup intFibm
 *
 * \desc            Temporary enable asynchronous write. That is batching
 *                  all write commands until ''fmFibmFlush'' is called.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          NONE
 *
 *****************************************************************************/
void fmFibmStartBatchingInt(fm_int sw)
{
    /* Disable synchronous write */
    FIBM_INFO(sw)->flags &= ~FM_FIBM_FLAGS_WRITE_SYNC;
}




/*****************************************************************************/
/** fmFibmFlushMsgAck
 * \ingroup intFibm
 *
 * \desc            Flushing pending fibm buffer and wait for a response to
 *                  make sure the message is successfully received. Caller
 *                  must ensure lock is taken appropriately.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_FIBM_TIMEOUT if no response after a timeout.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmFibmFlushMsgAck(fm_int sw)
{
    fm_status status;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM, "sw = %d\n", sw);

    /* Send the message and wait for response */
    status = fmSendFibm(sw, FIBM_SEND_MODE_SYNC);
    if (status == FM_ERR_NO_MEM)
    {
        /* Okay, no pending buffer to flush */
        if (FIBM_INFO(sw)->response)
        {
            /* Should not get here, but just to be safe */
            fmFreeBufferChain(GetMasterSwitch(sw), FIBM_INFO(sw)->response);
            FIBM_INFO(sw)->response = NULL;
        }

        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
    }

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, status);

    /* Even though we don't care about the response
     * But we still need a response back when expected
     */
    if (FIBM_INFO(sw)->respTimeout)
    {
        /* No response back, timeout */
        status = FM_ERR_FIBM_TIMEOUT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, status);
    }

    if (FIBM_INFO(sw)->response == NULL)
    {
        status = FM_ERR_FIBM_TIMEOUT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, status);
    }

    fmFreeBufferChain(GetMasterSwitch(sw), FIBM_INFO(sw)->response);

    /* We don't expect another response back from receive thread */
    FIBM_INFO(sw)->response = NULL;

ABORT:

    FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);
}   /* end fmFibmFlushMsgAck */


/*****************************************************************************/
/** fmFibmFlush
 * \ingroup intFibm
 *
 * \desc            Flushing pending fibm buffer and enable synchronous
 *                  write afterward if running under synchronous write mode.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_FIBM_TIMEOUT if no response after a timeout.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmFibmFlush(fm_int sw)
{
    fm_status status;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM, "sw = %d\n", sw);

    if (FIBM_INFO(sw)->mode & FM_FIBM_MODE_WRITE_SYNC)
    {
        status = fmFibmFlushMsgAck(sw);

        /* Restore sync mode if running under sync mode */
        FIBM_INFO(sw)->flags |= FM_FIBM_FLAGS_WRITE_SYNC;
    }
    else
    {
        /* Send the message and dont wait for response */
        status = fmSendFibm(sw, FIBM_SEND_MODE_ASYNC);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, status);
    }

ABORT:

    FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);
}




/*****************************************************************************/
/** fmFibmProcessTask
 * \ingroup intFibm
 *
 * \desc            Handles retries of FIBM messages, and batching commands.
 *
 * \param[in]       args contains a pointer to the thread information.
 *
 * \return          Never returns.
 *
 *****************************************************************************/
void *fmFibmProcessTask(void *args)
{
    fm_thread *        thread;
    fm_status          err = FM_OK;
    fm_int             sw;
    fm_timestamp       timeout;
    fm_event *         event;

    thread = FM_GET_THREAD_HANDLE(args);


    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "thread = %s timeout = %u usec\n",
                 thread->name, FM_FIBM_TASK_TIMEOUT_USEC);

    while (TRUE)
    {
        /**************************************************
         * Wait for a signal from FibmSend command or timeout.
         **************************************************/

        timeout.sec  = 0;
        timeout.usec = FM_FIBM_TASK_TIMEOUT_USEC;
        err          = fmWaitSemaphore(&fibmProcessTaskSem, &timeout);

        if (err == FM_OK)
        {
            FM_LOG_DEBUG(FM_LOG_CAT_FIBM,
                         "fmFibmProcessTask: signal\n");

            for (sw = FM_FIRST_FOCALPOINT ; sw <= FM_LAST_FOCALPOINT ; sw++)
            {
                if ( !FIBM_INFO_VALID(sw) )
                {
                    continue;
                }

                /* Check to see if there is pending write batched */
                if (FIBM_INFO(sw)->buffer)
                {
                    FIBM_STATS(sw).sendDelay++;

                    /* Make sure the buffer is not removed */
                    if (FIBM_INFO(sw)->buffer)
                    {
                        fmSendFibm(sw, FIBM_SEND_MODE_ASYNC);
                    }
                }
            }
        }
        else if (err == FM_ERR_SEM_TIMEOUT)
        {
            for (sw = FM_FIRST_FOCALPOINT ; sw <= FM_LAST_FOCALPOINT ; sw++)
            {
                if ( !FIBM_INFO_VALID(sw) )
                {
                    continue;
                }

                TAKE_FIBM_TXRX_LOCK(sw);

                /* Check to see if there is any pending FIBM */
                if (FIBM_INFO(sw)->sentCount)
                {
                    FIBM_INFO(sw)->timeoutCount++;

                    if (FIBM_INFO(sw)->timeoutCount > FM_FIBM_MAX_TIMEOUT)
                    {
                        /* Send events to the application at a rate of
                           fibmThresholdRetries retries */ 
                        if (!(FIBM_INFO(sw)->sentCount % 
                              GET_PLAT_STATE(sw)->fibmThresholdRetries))
                        {
                            /* Send an event to the application */
                            /* NOTE: This call will block if the number of free event 
                             * buffers drops below the acceptable threshold. */
                            event = fmAllocateEvent(sw,
                                                    FM_EVID_SYSTEM,
                                                    FM_EVENT_FIBM_THRESHOLD,
                                                    FM_EVENT_PRIORITY_HIGH);

                            if (event)
                            {
                                event->info.fibmRetries.retries =
                                    FIBM_INFO(sw)->sentCount;

                                err = fmSendThreadEvent(&fmRootApi->eventThread,
                                                        event);

                                if (err != FM_OK)
                                {
                                    fmDbgDiagCountIncr(sw,
                                                       FM_CTR_MAC_EVENT_SEND_ERR,
                                                       1);

                                    /* Free the event since it could not be
                                       sent to the event thread. */
                                    fmReleaseEvent(event);
                                }
                            } 
                            else
                            {
                                fmDbgDiagCountIncr(sw,
                                                   FM_CTR_MAC_EVENT_ALLOC_ERR,
                                                   1);
                            }
                        }

                        if ( (GET_PLAT_STATE(sw)->fibmMaxRetries != 0 &&
                              FIBM_INFO(sw)->sentCount > GET_PLAT_STATE(sw)->fibmMaxRetries) ||
                             GET_PLAT_STATE(sw)->fibmForceTimeout )
                        {
                            FIBM_DBG("FATAL: sw %d FIBM Message timeout %x %u %d\n",
                                     sw,
                                     FIBM_INFO(sw)->flags,
                                     FIBM_INFO(sw)->respTimeout,
                                     FIBM_INFO(sw)->seqTag);

                            if (FM_IS_STATE_ALIVE(
                                    fmRootApi->fmSwitchStateTable[sw]->state))
                            {
                                /* Once timeout, the switch is as good as dead 
                                 * The application must bring it up again
                                 */
                                FIBM_DBG("FATAL: SET SW TO FAIL STATE, FIBM Message timeout %x %u\n",
                                         FIBM_INFO(sw)->flags,
                                         FIBM_INFO(sw)->respTimeout);

                                fmRootApi->fmSwitchStateTable[sw]->state =
                                    FM_SWITCH_STATE_FAILED;
                            }

                            FIBM_INFO(sw)->sentCount = 0; /* Stop retries*/
                            /* Increase the tag for next message */
                            FIBM_INFO(sw)->seqTag++;

                            GET_PLAT_STATE(sw)->fibmForceTimeout = FALSE;

                            if (FIBM_INFO(sw)->flags &
                                FM_FIBM_FLAGS_WAIT_RESPONSE)
                            {
                                FIBM_INFO(sw)->respTimeout = TRUE;
                                fmSignalSemaphore(&FIBM_INFO(sw)->fibmResponseSemaphore);
                            }
                        }
                        else
                        {
                            FIBM_DBG("Resend FIBM sw %d SeqTag %u timeoutCount %u sentCount %llu, master:0x%x/%d , slave:0x%x/%d \n",
                                     sw,
                                     FIBM_INFO(sw)->seqTag,
                                     FIBM_INFO(sw)->timeoutCount,
                                     FIBM_INFO(sw)->sentCount,
                                     FIBM_INFO(sw)->cfg.masterGlort,
                                     FIBM_INFO(sw)->cfg.masterMgmtPort,
                                     FIBM_INFO(sw)->cfg.slaveGlort,
                                     FIBM_INFO(sw)->cfg.slaveMgmtPort);
                            fmReSendFibm(sw);
                        }
                    }
                }
                DROP_FIBM_TXRX_LOCK(sw);
            }
        }
        else
        {
            FM_LOG_ERROR( FM_LOG_CAT_FIBM,
                         "%s: %s\n",
                         thread->name,
                         fmErrorMsg(err) );
            continue;
        }

    }   /* end while (TRUE) */

    /**************************************************
     * Should never exit.
     **************************************************/

    FM_LOG_ERROR(FM_LOG_CAT_FIBM,
                 "ERROR: fmFibmProcessTask: exiting inadvertently!\n");

    return NULL;

}   /* end fmFibmProcessTask */




/*****************************************************************************/
/** fmFibmGetBuf
 * \ingroup intFibm
 *
 * \desc            Return pending FIBM buffer if present or allocate new
 *                  buffer and initialize the buffer appropriately. If the
 *                  request length is greater than remaining buffer, the
 *                  pending buffer will be sent out before a new buffer will
 *                  then allocated.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       neededLen is length needed for new commands.
 *
 * \return          pointer to buffer if successful.
 * \return          NULL if no buffer memory is available.
 *
 *****************************************************************************/
static fm_buffer *fmFibmGetBuf(fm_int sw, fm_uint neededLen)
{
    fm_buffer *buf;

    if (!FIBM_INFO(sw)->buffer)
    {
        /* Request length is too long, just return NULL */
        if ( neededLen > (FM_BUFFER_SIZE_WORDS - FM_FIBM_OFFSET_DATA) )
        {
            return NULL;
        }

        buf = fmAllocateBuffer(sw);

        if (!buf)
        {
            return NULL;
        }

        /* Reserve for mac and ethertype and tag */
        /* Note the usage of len is in fm_uint32 for convenience */
        buf->len               = FM_FIBM_OFFSET_DATA;
        FIBM_INFO(sw)->respLen = FM_FIBM_OFFSET_DATA;
        FIBM_INFO(sw)->buffer  = buf;
    }
    else
    {
        buf = FIBM_INFO(sw)->buffer;
    }

    return buf;

}   /* end fmFibmGetBuf */




/*****************************************************************************/
/** fmFibmAddWriteCommandMult
 * \ingroup intFibm
 *
 * \desc            Adding a multiple write commands to pending FIBM buffer.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       cmd is the read hw command list.
 *
 * \param[in]       numRegs is the number of commands.
 *
 * \param[in]       val points to the array of register values to write.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NO_MEM if no buffer memory is available.
 *
 *****************************************************************************/
static fm_status fmFibmAddWriteCommandMult(fm_int     sw,
                                           fm_uint32  cmd,
                                           fm_int     numRegs,
                                           fm_uint32 *val)
{
    fm_buffer *buf;
    fm_int     i;
    fm_status  status;

    buf = fmFibmGetBuf(sw, numRegs + 1);

    if (!buf)
    {
        return FM_ERR_NO_MEM;
    }

    /* Not enough buffer, send pending one and get new one */
    if ( ((FIBM_INFO(sw)->respLen + FIBM_RESERVED_GAP) >= FM_BUFFER_SIZE_WORDS) ||
         ((buf->len + numRegs + FIBM_RESERVED_GAP) >= FM_BUFFER_SIZE_WORDS) )
    {

       /* Send the pending buffer before getting new one */
        status = fmSendFibm(sw, FIBM_SEND_MODE_SYNC);
        if (status != FM_OK)
        {
            return status;
        }

        /* wait for response */
        /* Even though we don't care about the response
         * But we still need a response back when expected
         */
        if (FIBM_INFO(sw)->respTimeout)
        {
            FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_FIBM_TIMEOUT);
        }

        if (FIBM_INFO(sw)->response == NULL)
        {
            FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_FIBM_TIMEOUT);
        }

        fmFreeBufferChain(GetMasterSwitch(sw), FIBM_INFO(sw)->response);

        /* We don't expect another response back from receive thread */
        FIBM_INFO(sw)->response = NULL;

        /* Get the new buffer */
        buf = fmFibmGetBuf(sw, numRegs + 1);
        if (!buf)
        {
            return FM_ERR_NO_MEM;
        }
    }

    /* Note the usage of len is in fm_uint32 for convenience */
    buf->data[buf->len] = htonl(cmd);
    buf->len++;

    for (i = 0 ; i < numRegs ; i++)
    {
        buf->data[buf->len] = htonl(val[i]);
        buf->len++;
    }
    FIBM_INFO(sw)->respLen += 1;

    return FM_OK;

}   /* end fmFibmAddWriteCommandMult */




/*****************************************************************************/
/** fmFibmAddReadCommandMult
 * \ingroup intFibm
 *
 * \desc            Adding a single read command to pending FIBM buffer.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       cmd is the read hw command.
 *
 * \param[in]       numRegs is the number of registers requested.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NO_MEM if no buffer memory is available.
 *
 *****************************************************************************/
static fm_status fmFibmAddReadCommandMult(fm_int sw, fm_uint32 cmd, fm_int numRegs)
{
    fm_buffer *buf;

    buf = fmFibmGetBuf(sw, numRegs + 1); /* how many words needed for response */

    if (!buf)
    {
        return FM_ERR_NO_MEM;
    }

    /* Not enough buffer, send pending one and get new one */
    if ( ((FIBM_INFO(sw)->respLen + numRegs + FIBM_RESERVED_GAP) >= FM_BUFFER_SIZE_WORDS) ||
         ((buf->len + FIBM_RESERVED_GAP) >= FM_BUFFER_SIZE_WORDS) )
    {
        /* For read, the caller must ensure there is enough buffer */
        return FM_ERR_BUFFER_FULL;
    }

    /* Note the usage of len is in fm_uint32 for convenience */
    buf->data[buf->len] = htonl(cmd);
    buf->len++;
    FIBM_INFO(sw)->respLen += (numRegs + 1);

    return FM_OK;

}   /* end fmFibmAddReadCommandMult */




/*****************************************************************************/
/** fmFibmReadCSRMultIntChunk
 * \ingroup intFibm
 *
 * \desc            Read multiple CSR registers via FIBM. This is internal
 *                  API that does not check if adding the read commands
 *                  to the pending buffer will cause the response not to fit
 *                  into one buffer. The caller of this function must take
 *                  lock properly.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the starting CSR register address to read.
 *
 * \param[in]       numRegs is the number of consecutive register addresses
 *                  to read.
 *
 * \param[out]      value points to an array to be filled in with
 *                  the register data read. The array must be n elements in
 *                  length.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_FIBM_TIMEOUT if no response after a timeout.
 * \return          FM_ERR_FIBM_INVALID_RESPONSE if reponse is not expected.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmFibmReadCSRMultIntChunk(fm_int     sw,
                                    fm_uint32  addr,
                                    fm_int     numRegs,
                                    fm_uint32 *value)
{
    fm_status  status;
    int        i, cnt, rem;
    fm_uint32  fibmCmd, curAddr;
    fm_int     off, masterSw;
    fm_uint32 *buf, *curVal;
    fm_buffer *response;

    /* Handle multiple writes greater than FM_FIBM_HW_CMD_MAX_LEN elements */
    curAddr = addr;
    curVal  = value;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "sw = %d, addr = 0x%08x, numRegs = %d\n",
                 sw,
                 addr,
                 numRegs);


    /* The switch is going down or fail, just return */
    if (fmRootApi->fmSwitchStateTable[sw]->state >= FM_SWITCH_STATE_GOING_DOWN)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
    }

    for (cnt = 0 ; cnt < (numRegs / FM_FIBM_HW_CMD_MAX_LEN) ; cnt++)
    {
        /* len = 0x0 means 16 words */
        fibmCmd = FM_FIBM_MAKE_READ_CMD(FM_FIBM_HW_CMD_MAX_LEN, curAddr);
        fmFibmAddReadCommandMult(sw, fibmCmd, FM_FIBM_HW_CMD_MAX_LEN);
        /* Save the fibmCmd temporary in value so later we can verify it */
        curVal[0] = fibmCmd;
        /* Advance to next chunk */
        curAddr += FM_FIBM_HW_CMD_MAX_LEN;
        curVal  += FM_FIBM_HW_CMD_MAX_LEN;
    }

    /* Any remaining data */
    rem = numRegs % FM_FIBM_HW_CMD_MAX_LEN;

    if (rem)
    {
        fibmCmd = FM_FIBM_MAKE_READ_CMD(rem, curAddr);
        fmFibmAddReadCommandMult(sw, fibmCmd, rem);
        /* Save the fibmCmd temporary in value so later we can verify it */
        curVal[0] = fibmCmd;
    }

    /* Send the message and wait for response */
    status = fmSendFibm(sw, FIBM_SEND_MODE_SYNC);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);
    }

    /* When we wake up, the data is saved in
     * mRootApi->fmSwitchStateTable[sw]->fibmInfo->response
     */

    if (FIBM_INFO(sw)->respTimeout)
    {
        /* No response back, timeout */
        *value = 0xDEADDEAD;
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_FIBM_TIMEOUT);
    }

    if (FIBM_INFO(sw)->response == NULL)
    {
        *value = 0xDEADDEAD;
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_FIBM_TIMEOUT);
    }

    response = FIBM_INFO(sw)->response;
    masterSw = GetMasterSwitch(sw);

    /* We don't expect another response back from receive thread */
    FIBM_INFO(sw)->response = NULL;

    buf = response->data;
    off = FM_FIBM_OFFSET_DATA; /* Start of response data */

    curAddr = addr;
    curVal  = value;

    while (off < response->len/4)
    {
        /* Search for expected fibmCmd */
        if (ntohl(buf[off]) == curVal[0])
        {
            /* Handle multiple read greater than 16 elements */
            for (cnt = 0 ; cnt < (numRegs / FM_FIBM_HW_CMD_MAX_LEN) ; cnt++)
            {
                /* Verify expected fibmCmd */
                if (ntohl(buf[off]) != curVal[0])
                {
                    FM_LOG_ERROR(FM_LOG_CAT_FIBM,
                                 "FibmReadMult: Unexpected FIBM CMD wanted "
                                 "0x%08x got 0x%08x at offset %u\n",
                                 curVal[0], ntohl(buf[off]), off);
                    fmFreeBufferChain(masterSw, response);
                    FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_FIBM_INVALID_RESPONSE);
                }

                for (i = 0 ; i < FM_FIBM_HW_CMD_MAX_LEN ; i++)
                {
                    curVal[i] = ntohl(buf[off + i + 1]);
                }

                /* Advance to next chunk */
                curAddr += FM_FIBM_HW_CMD_MAX_LEN;
                curVal  += FM_FIBM_HW_CMD_MAX_LEN;
                off     += (FM_FIBM_HW_CMD_MAX_LEN + 1);
            }

            /* Any remaining data chunk */
            rem = numRegs % FM_FIBM_HW_CMD_MAX_LEN;

            if (rem)
            {
                /* Verify expected fibmCmd */
                if (ntohl(buf[off]) != curVal[0])
                {
                    FM_LOG_ERROR(FM_LOG_CAT_FIBM,
                                 "FibmReadMult: Unexpected FIBM CMD wanted "
                                 "0x%08x got 0x%08x at offset %u\n",
                                 curVal[0], ntohl(buf[off]), off);

                    fmFreeBufferChain(masterSw, response);
                    FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_FIBM_INVALID_RESPONSE);
                }

                for (i = 0 ; i < rem ; i++)
                {
                    curVal[i] = ntohl(buf[off + i + 1]);
                }
            }

            /* All data received ok */
            fmFreeBufferChain(masterSw, response);
            FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
        }

        if (!(ntohl(buf[off])&(FM_FIBM_HW_CMD_READ|FM_FIBM_HW_CMD_WRITE)))
        {
            /* End of data but not found correct fibmCmd */
            break;
        }

        off++;
    }

    /* No more data and can't find result */
    *value = (fm_uint32) 0xBADBADBAD;

    fmFreeBufferChain(masterSw, response);
    FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_FIBM_INVALID_RESPONSE);

}   /* end fmFibmReadCSRMultIntChunk */



/*****************************************************************************/
/** fmFibmReadCSRMultInt
 * \ingroup intFibm
 *
 * \desc            Read multiple CSR registers via FIBM.
 * \note            Equivalent to ''fmFibmReadCSRMult'', but no lock is taken.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the starting CSR register address to read.
 *
 * \param[in]       numRegs contains the number of consecutive register addresses
 *                  to read.
 *
 * \param[out]      value points to an array to be filled in with
 *                  the register data read. The array must be n elements in
 *                  length.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_FIBM_TIMEOUT if no response after a timeout.
 * \return          FM_ERR_FIBM_INVALID_RESPONSE if response is not expected.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmFibmReadCSRMultInt(fm_int     sw,
                               fm_uint32  addr,
                               fm_int     numRegs,
                               fm_uint32 *value)
{
    int        numRead;
    fm_uint32  curAddr;
    fm_uint32 *curVal;
    fm_status  status = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "sw = %d, addr = 0x%08x, numRegs = %d, value = %p\n",
                 sw,
                 addr,
                 numRegs,
                 (void *) value);

    /* The switch is going down or fail, just return */
    if (fmRootApi->fmSwitchStateTable[sw]->state >= FM_SWITCH_STATE_GOING_DOWN)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
    }

    curAddr = addr;
    curVal  = value;

    while (numRegs)
    {
        /* Since we don't allow buffer chaining, we must ensure
         * that the response is limited to one buffer
         */

        /* Figuring out how many register read we can do here */
        numRead  = FM_BUFFER_SIZE_WORDS - FIBM_INFO(sw)->respLen;
        numRead -= (numRead / FM_FIBM_HW_CMD_MAX_LEN + FIBM_RESERVED_GAP);

        if (numRead < 1)
        {
            /* Not enough buffer, flush and start new buffer */
            fmFibmFlushMsgAck(sw); /* These are batch writes */

            if (status != FM_OK)
            {
                FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);
            }
            
            /* Start again */
            continue;
        }

        if (numRegs < numRead)
        {
            numRead = numRegs;
        }

        status = fmFibmReadCSRMultIntChunk(sw, curAddr, numRead, curVal);

        if (status != FM_OK)
        {
            FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);
        }

        numRegs -= numRead;
        curAddr += numRead;
        curVal  += numRead;
    }

    FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);

}   /* end fmFibmReadCSRMultInt */




/*****************************************************************************/
/** fmFibmWriteCSRMultIntChunk
 * \ingroup intFibm
 *
 * \desc            Write multiple CSR registers via FIBM. This is internal
 *                  API that does not check if adding the write commands
 *                  to the pending buffer will cause the sent or response
 *                  message not to fit into one buffer. The caller of this
 *                  function must take lock appropriately
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       numRegs is the number of consecutive register addresses
 *                  to write.
 *
 * \param[in]       addr contains the starting CSR register address to write.
 *
 * \param[in]       value points to an array of values to be written. The
 *                  array must be n elements in length.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_FIBM_TIMEOUT if no response after a timeout.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmFibmWriteCSRMultIntChunk(fm_int     sw,
                                     fm_uint32  addr,
                                     fm_int     numRegs,
                                     fm_uint32 *value)
{
    fm_uint32 fibmCmd;
    fm_int    cnt;
    fm_status status = FM_OK;
    fm_int    cmdMaxLen;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "sw = %d, addr = 0x%08x, numRegs = %d\n",
                 sw,
                 addr,
                 numRegs);

    cmdMaxLen = FM_FIBM_HW_CMD_MAX_LEN;

    /* Per Bug 10788: FIBM control software needs to satisfy the
     * following burst length constraints:
     *  - Three or less when writing to MTable or the Async Stats units.
     *  - Four or less when writing to any Frame Handler registers.
     */
    
    switch (GET_PLAT_STATE(sw)->family)
    {
    /* FIXME: Chip specific function can not be called here directly */
#ifdef FM_SUPPORT_FM4000
        case FM_SWITCH_FAMILY_FM4000:
        case FM_SWITCH_FAMILY_REMOTE_FM4000:
            cmdMaxLen = fm4000GetMaxFibmCmdLength(addr);
            break;
#endif

        case FM_SWITCH_FAMILY_FM6000:
        case FM_SWITCH_FAMILY_REMOTE_FM6000:
            /* cmdMaxLen = fm6000GetMaxFibmCmdLength(addr); */
            break;

        default:
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, "Invalid switch family\n");
    }

    /* Handle multiple writes greater than 16 elements */
    for (cnt = 0 ; cnt < (numRegs / cmdMaxLen) ; cnt++)
    {
        fibmCmd = FM_FIBM_MAKE_WRITE_CMD(cmdMaxLen, addr);
        fmFibmAddWriteCommandMult(sw, fibmCmd, cmdMaxLen, value);
        /* Advance to next chunk */
        addr  += cmdMaxLen;
        value += cmdMaxLen;
    }

    /* Any remaining data */
    numRegs %= cmdMaxLen;

    if (numRegs)
    {
        fibmCmd = FM_FIBM_MAKE_WRITE_CMD(numRegs, addr);
        fmFibmAddWriteCommandMult(sw, fibmCmd, numRegs, value);
    }

    if (FIBM_INFO(sw)->flags & FM_FIBM_FLAGS_WRITE_SYNC)
    {
        /* Send the message and wait for response */
        status = fmSendFibm(sw, FIBM_SEND_MODE_SYNC);

        if (status != FM_OK)
        {
            FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);
        }

        /* Even though we don't care about the response
         * But we still need a response back when expected
         */
        if (FIBM_INFO(sw)->respTimeout)
        {
            /* No response back, timeout */
            FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_FIBM_TIMEOUT);
        }

        if (FIBM_INFO(sw)->response == NULL)
        {
            FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_FIBM_TIMEOUT);
        }

        fmFreeBufferChain(GetMasterSwitch(sw), FIBM_INFO(sw)->response);

        /* We don't expect another response back from receive thread */
        FIBM_INFO(sw)->response = NULL;
    }

    FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);

}   /* end fmFibmWriteCSRMultIntChunk */


/*****************************************************************************/
/** fmFibmWriteCSRMultInt
 * \ingroup intFibm
 *
 * \desc            Write multiple CSR registers via FIBM.
 * \note            Equivalent to ''fmFibmWriteCSRMult'', but no lock is taken.
 *
 * \note            Input value array buffer might be modified.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       numRegs contains the number of consecutive register addresses
 *                  to write.
 *
 * \param[in]       addr contains the starting CSR register address to write.
 *
 * \param[in]       value points to an array of values to be written. The
 *                  array must be n elements in length.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_FIBM_TIMEOUT if no response after a timeout.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmFibmWriteCSRMultInt(fm_int     sw,
                                fm_uint32  addr,
                                fm_int     numRegs,
                                fm_uint32 *value)
{
    fm_int      numWrite;
    fm_uint32  curAddr;
    fm_uint32 *curVal;
    fm_status  status = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "sw = %d, addr = 0x%08x, numRegs = %d, value = %p\n",
                 sw,
                 addr,
                 numRegs,
                 (void *) value);

    /* The switch is going down or fail, just return */
    if (fmRootApi->fmSwitchStateTable[sw]->state >= FM_SWITCH_STATE_GOING_DOWN ||
        fmPlatformBypassEnabled(sw))
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
    }

    curAddr = addr;
    curVal  = value;

    while (numRegs)
    {
        /* Since we don't allow buffer chaining, we must ensure
         * that the response is limited to one buffer
         */

        /* Figuring out how many register write we can do here */
        if (FIBM_INFO(sw)->buffer)
        {
            numWrite = FM_BUFFER_SIZE_WORDS - FIBM_INFO(sw)->buffer->len;
        }
        else
        {
            numWrite = FM_BUFFER_SIZE_WORDS - FM_FIBM_OFFSET_DATA;
        }

        numWrite -= (numWrite / FM_FIBM_HW_CMD_MAX_LEN + FIBM_RESERVED_GAP);

        if (numWrite < 1)
        {
            /* Not enough buffer, flush and start new buffer */
            status = fmFibmFlushMsgAck(sw); /* These are batch writes */

            if (status != FM_OK)
            {
                FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);
            }
            /* start again after flush */
            continue;
        }

        if (numRegs < numWrite)
        {
            numWrite = numRegs;
        }

        status = fmFibmWriteCSRMultIntChunk(sw, curAddr, numWrite, curVal);

        if (status != FM_OK)
        {
            FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);
        }

        numRegs -= numWrite;
        curAddr += numWrite;
        curVal  += numWrite;
    }

    FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);

}   /* end fmFibmWriteCSRMultInt */




/*****************************************************************************/
/** SendAndProcessResponse
 * \ingroup intFibm
 *
 * \desc            Send and process scatter gather read request.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       nEntries is the number of scatter-gather entries
 *                  in sgList.
 *
 * \param[in]       sgList is an array of length nEntries, which
 *                  describes each region to be read.
 *
 * \param[in,out]   readIdx is pointer to the current read index.
 *
 * \param[in,out]   readOff is pointer to the current read offset.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_FIBM_TIMEOUT if no response after a timeout.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status SendAndProcessResponse(fm_int sw, fm_uint nEntries,
                                 fm_scatterGatherListEntry *sgList,
                                 fm_uint *readIdx, fm_uint *readOff)
{
    fm_int     i;
    fm_int     numRead = 0;
    fm_status  status = FM_OK;
    fm_uint32  fibmCmd;
    fm_int     off, masterSw;
    fm_uint32 *buf;
    fm_buffer *response;


    status = fmSendFibm(sw, FIBM_SEND_MODE_SYNC);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);
    }

    /* When we wake up, the data is saved in
     * mRootApi->fmSwitchStateTable[sw]->fibmInfo->response
     */

    if (FIBM_INFO(sw)->respTimeout || (FIBM_INFO(sw)->response == NULL))
    {
        /* No response back, timeout */
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_FIBM_TIMEOUT);
    }

    response = FIBM_INFO(sw)->response;
    masterSw = GetMasterSwitch(sw);

    /* We don't expect another response back from receive thread */
    FIBM_INFO(sw)->response = NULL;

    buf = response->data;
    off = FM_FIBM_OFFSET_DATA; /* Start of response data */


    while (off < response->len/4)
    {
        /* Search for expected fibmCmd */
        fibmCmd = sgList[*readIdx].data[*readOff];
        if (ntohl(buf[off]) == fibmCmd) 
        {
            numRead = FM_FIBM_GET_CMD_LEN(fibmCmd);

            /* Copy over the data */
            for (i = 0 ; i < numRead ; i++)
            {
                sgList[*readIdx].data[*readOff] = ntohl(buf[off + i + 1]);
                (*readOff)++;
            }

            if (*readOff == sgList[*readIdx].count)
            {
                /* Next index in the sgList array */
                (*readIdx)++;
                if ((*readIdx) >= nEntries)
                {
                    /* end of data */
                    break;
                }
                *readOff = 0;
            }
            else if (*readOff > sgList[*readIdx].count)
            {
                /* Should not get more than what is request */
                /* One problem is having the same data pointer for multiple reads */
                FM_LOG_ERROR(FM_LOG_CAT_FIBM,
                    "FibmReadSg: Unexpected FIBM read size "
                     "0x%08x readIdx %u readOff %u sgListCount %u\n",
                     fibmCmd, *readIdx, *readOff,
                     sgList[*readIdx].count);
                break; /* Let code below exit from error */
            }

            off += numRead;
        }

        off++;


        if (!(ntohl(buf[off])&(FM_FIBM_HW_CMD_READ|FM_FIBM_HW_CMD_WRITE)))
        {
            /* End of data but not found correct fibmCmd */
            break;
        }
    }

    fmFreeBufferChain(masterSw, response);
    FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);
}   /* end SendAndProcessResponse */



/*****************************************************************************
 * Public Functions
 *****************************************************************************/


/*****************************************************************************/
/** fmFibmReadCSR
 * \ingroup intFibm
 *
 * \desc            Read a CSR register via FIBM.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the CSR register address to read
 *
 * \param[out]      value points to storage where this function will place
 *                  the read register value.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_FIBM_TIMEOUT if no response after a timeout.
 * \return          FM_ERR_FIBM_INVALID_RESPONSE if reponse is not expected.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmFibmReadCSR(fm_int sw, fm_uint32 addr, fm_uint32 *value)
{
    fm_status  status = FM_OK;
    fm_uint32  fibmCmd;
    fm_int     off, masterSw;
    fm_uint32 *buf;
    fm_buffer *response;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "sw = %d, addr = 0x%08x, value = %p\n",
                 sw,
                 addr,
                 (void *) value);

	*value = (fm_uint32) 0xDEADBEEF;

    /* The switch is going down or fail, just return
     * Don't really need to write to hardware when 
     * switch is going down.
     */
    if (fmRootApi->fmSwitchStateTable[sw]->state >= FM_SWITCH_STATE_GOING_DOWN)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
    }

    TAKE_FIBM_LOCK(sw);
    FIBM_STATS(sw).read++;

    /* We could just call fmFibmReadCSRMult,
     * but this is a little more efficient
     */

    fibmCmd = FM_FIBM_MAKE_READ_CMD(1, addr);
    fmFibmAddReadCommandMult(sw, fibmCmd, 1);

    /* Send the message and wait for response */
    status = fmSendFibm(sw, FIBM_SEND_MODE_SYNC);

    if (status != FM_OK)
    {
        *value = (fm_uint32) 0xDEADBEEF;
        DROP_FIBM_LOCK(sw);
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);
    }

    /* When we wake up, the data is saved in
     * mRootApi->fmSwitchStateTable[sw]->fibmInfo->response
     */

    if (FIBM_INFO(sw)->respTimeout)
    {
        /* No response back, timeout */
        *value = 0xDEADDEAD;
        DROP_FIBM_LOCK(sw);
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_FIBM_TIMEOUT);
    }

    if (FIBM_INFO(sw)->response == NULL)
    {
        *value = 0xDEADDEAD;
        DROP_FIBM_LOCK(sw);
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_FIBM_TIMEOUT);
    }

    response = FIBM_INFO(sw)->response;
    masterSw = GetMasterSwitch(sw);

    /* We don't expect another response back from receive thread */
    FIBM_INFO(sw)->response = NULL;

    buf = response->data;
    off = FM_FIBM_OFFSET_DATA; /* Start of response data */

    while (off < response->len/4)
    {
        if (ntohl(buf[off]) == fibmCmd)
        {
            *value = ntohl(buf[off + 1]);
            DROP_FIBM_LOCK(sw);
            fmFreeBufferChain(masterSw, response);
            FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
        }

        if (!(ntohl(buf[off])&(FM_FIBM_HW_CMD_READ|FM_FIBM_HW_CMD_WRITE)))
        {
            /* End of data */
            break;
        }

        off++;
    }

    /* No more data and can't find result */
    *value = (fm_uint32) 0xBADBADBAD;

    DROP_FIBM_LOCK(sw);
    fmFreeBufferChain(masterSw, response);
    FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_FIBM_INVALID_RESPONSE);

}   /* end fmFibmReadCSR */




/*****************************************************************************/
/** fmFibmWriteCSR
 * \ingroup intFibm
 *
 * \desc            Write a CSR register via FIBM.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the CSR register address to write.
 *
 * \param[in]       value is the data value to write to the register.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_FIBM_TIMEOUT if no response after a timeout.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmFibmWriteCSR(fm_int sw, fm_uint32 addr, fm_uint32 value)
{
    fm_uint32 fibmCmd;
    fm_status status = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "sw = %d, addr = 0x%08x, value = 0x%08x\n",
                 sw,
                 addr,
                 value);

    /* The switch is going down or fail, just return */
    if (fmRootApi->fmSwitchStateTable[sw]->state >= FM_SWITCH_STATE_GOING_DOWN ||
        fmPlatformBypassEnabled(sw))
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
    }

    TAKE_FIBM_LOCK(sw);
    FIBM_STATS(sw).write++;

    /* We could just call fmFibmWriteCSRMult,
     * but this is a little more efficient
     */

    fibmCmd = FM_FIBM_MAKE_WRITE_CMD(1, addr);
    fmFibmAddWriteCommandMult(sw, fibmCmd, 1, &value);

    /* platform lock is adequate for this here */
    if (FIBM_INFO(sw)->flags & FM_FIBM_FLAGS_WRITE_SYNC)
    {
        /* Send the message and wait for response */
        status = fmSendFibm(sw, FIBM_SEND_MODE_SYNC);

        if (status != FM_OK)
        {
            DROP_FIBM_LOCK(sw);
            FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);
        }

        if (FIBM_INFO(sw)->respTimeout)
        {
            /* No response back, timeout */
            DROP_FIBM_LOCK(sw);
            FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_FIBM_TIMEOUT);
        }

        if (FIBM_INFO(sw)->response == NULL)
        {
            DROP_FIBM_LOCK(sw);
            FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_FIBM_TIMEOUT);
        }

        fmFreeBufferChain(GetMasterSwitch(sw), FIBM_INFO(sw)->response);

        /* We don't expect another response back from receive thread */
        FIBM_INFO(sw)->response = NULL;

    }

    DROP_FIBM_LOCK(sw);
    FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);

}   /* end fmFibmWriteCSR */




/*****************************************************************************/
/** fmFibmMaskCSR
 * \ingroup intFibm
 *
 * \desc            Mask on or off the bits in a single 32-bit register.
 *
 * \note            This function is not called by the API directly, but by
 *                  platform layer code that is commonly available to all
 *                  platforms.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr is the word offset into the switch's register file.
 *
 * \param[in]       mask is the bit mask to turn on or off.
 *
 * \param[in]       on should be TRUE to set the masked bits in the register
 *                   or FALSE to clear the masked bits in the register.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_FIBM_TIMEOUT if no response after a timeout.
 * \return          FM_ERR_FIBM_INVALID_RESPONSE if reponse is not expected.
 *
 *****************************************************************************/
fm_status fmFibmMaskCSR(fm_int sw, fm_uint addr, fm_uint32 mask, fm_bool on)
{
    fm_status err = FM_OK;
    fm_uint32 value;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "sw = %d, addr = %u, mask = 0x%08x, on = %s\n",
                 sw,
                 addr,
                 mask,
                 FM_BOOLSTRING(on) );

    /* The switch is going down or fail, just return */
    if (fmRootApi->fmSwitchStateTable[sw]->state >= FM_SWITCH_STATE_GOING_DOWN)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
    }

    TAKE_FIBM_LOCK(sw);
    FIBM_STATS(sw).mask++;
    DROP_FIBM_LOCK(sw);

    err = fmFibmReadCSR(sw, addr, &value);

    if (err)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, err);
    }

    if (on)
    {
        value |= mask;
    }
    else
    {
        value &= ~mask;
    }

    err = fmFibmWriteCSR(sw, addr, value);

    FM_LOG_EXIT(FM_LOG_CAT_FIBM, err);

}   /* end fmFibmMaskCSR */




/*****************************************************************************/
/** fmFibmReadCSRMult
 * \ingroup intFibm
 *
 * \desc            Read multiple CSR registers via FIBM.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the starting CSR register address to read.
 *
 * \param[in]       numRegs contains the number of consecutive register addresses
 *                  to read.
 *
 * \param[out]      value points to an array to be filled in with
 *                  the register data read. The array must be n elements in
 *                  length.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_FIBM_TIMEOUT if no response after a timeout.
 * \return          FM_ERR_FIBM_INVALID_RESPONSE if response is not expected.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmFibmReadCSRMult(fm_int     sw,
                            fm_uint32  addr,
                            fm_int     numRegs,
                            fm_uint32 *value)
{
    fm_status  status = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "sw = %d, addr = 0x%08x, numRegs = %d, value = %p\n",
                 sw,
                 addr,
                 numRegs,
                 (void *) value);

    /* The switch is going down or fail, just return */
    if (fmRootApi->fmSwitchStateTable[sw]->state >= FM_SWITCH_STATE_GOING_DOWN)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
    }

    TAKE_FIBM_LOCK(sw);
    FIBM_STATS(sw).readMulti++;

    status = fmFibmReadCSRMultInt(sw, addr, numRegs, value);

    DROP_FIBM_LOCK(sw);
    FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);

}   /* end fmFibmReadCSRMult */




/*****************************************************************************/
/** fmFibmWriteCSRMult
 * \ingroup intFibm
 *
 * \desc            Write multiple CSR registers via FIBM.
 *
 * \note            Input value array buffer might be modified.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       numRegs contains the number of consecutive register addresses
 *                  to write.
 *
 * \param[in]       addr contains the starting CSR register address to write.
 *
 * \param[in]       value points to an array of values to be written. The
 *                  array must be n elements in length.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_FIBM_TIMEOUT if no response after a timeout.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmFibmWriteCSRMult(fm_int     sw,
                             fm_uint32  addr,
                             fm_int     numRegs,
                             fm_uint32 *value)
{
    fm_status  status = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "sw = %d, addr = 0x%08x, numRegs = %d, value = %p\n",
                 sw,
                 addr,
                 numRegs,
                 (void *) value);

    /* The switch is going down or fail, just return */
    if (fmRootApi->fmSwitchStateTable[sw]->state >= FM_SWITCH_STATE_GOING_DOWN ||
        fmPlatformBypassEnabled(sw))
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
    }

    TAKE_FIBM_LOCK(sw);
    FIBM_STATS(sw).writeMulti++;

    status = fmFibmWriteCSRMultInt(sw, addr, numRegs, value);

    DROP_FIBM_LOCK(sw);
    FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);

}   /* end fmFibmWriteCSRMult */




/*****************************************************************************/
/** fmFibmReadCSR64
 * \ingroup intFibm
 *
 * \desc            Read a 64-bit CSR register via FIBM.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the CSR register address to read.
 *
 * \param[out]      value points to storage where the 64-bit read data value
 *                  will be stored by this function.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_FIBM_TIMEOUT if no response after a timeout.
 * \return          FM_ERR_FIBM_INVALID_RESPONSE if reponse is not expected.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmFibmReadCSR64(fm_int sw, fm_uint32 addr, fm_uint64 *value)
{
    fm_uint32 val32[2];
    fm_status status;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "sw = %d, addr = 0x%08x, value = %p\n",
                 sw,
                 addr,
                 (void *) value);

    /* The switch is going down or fail, just return */
    if (fmRootApi->fmSwitchStateTable[sw]->state >= FM_SWITCH_STATE_GOING_DOWN)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
    }

    /* Dont need to take lock the whole function since
     * fmFibmReadCSRMult is already take lock
     */
    TAKE_FIBM_LOCK(sw);
    FIBM_STATS(sw).read64++;

    status = fmFibmReadCSRMultInt(sw, addr, 2, val32);

    if (status != FM_OK)
    {
        DROP_FIBM_LOCK(sw);
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);
    }

    *value  = val32[1];
    *value  = (*value << 32);
    *value |= val32[0];

    DROP_FIBM_LOCK(sw);
    FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);

}   /* end fmFibmReadCSR64 */




/*****************************************************************************/
/** fmFibmWriteCSR64
 * \ingroup intFibm
 *
 * \desc            Writes a 64-bit CSR register via FIBM.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the CSR register address to write.
 *
 * \param[in]       value is the 64-bit data value to write.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_FIBM_TIMEOUT if no response after a timeout.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmFibmWriteCSR64(fm_int sw, fm_uint32 addr, fm_uint64 value)
{
    fm_uint32 val32[2];
    fm_status status;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "sw = %d, addr = 0x%08x, value = 0x%016" FM_FORMAT_64 "x\n",
                 sw,
                 addr,
                 value);

    /* The switch is going down or fail, just return */
    if (fmRootApi->fmSwitchStateTable[sw]->state >= FM_SWITCH_STATE_GOING_DOWN ||
        fmPlatformBypassEnabled(sw))
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
    }

    TAKE_FIBM_LOCK(sw);
    FIBM_STATS(sw).write64++;

    val32[0] = (value & 0xffffffffL);
    val32[1] = (value >> 32);

    status = fmFibmWriteCSRMultInt(sw, addr, 2, val32);

    DROP_FIBM_LOCK(sw);

    FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);

}   /* end fmFibmWriteCSR64 */




/*****************************************************************************/
/** fmFibmReadCSRMult64
 * \ingroup intFibm
 *
 * \desc            Read multiple 64-bit CSR registers via FIBM.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       numRegs contains the number of register addresses to read.
 *
 * \param[in]       addr contains the starting CSR register address to read.
 *
 * \param[out]      value points to an array of to be filled in with
 *                  the 64-bit register data read. The array must be n elements
 *                  in length.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_FIBM_TIMEOUT if no response after a timeout.
 * \return          FM_ERR_FIBM_INVALID_RESPONSE if reponse is not expected.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmFibmReadCSRMult64(fm_int     sw,
                              fm_uint32  addr,
                              fm_int     numRegs,
                              fm_uint64 *value)
{
    int       i;
    fm_status status;
    fm_uint32 *pVal32;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "sw = %d, addr = 0x%08x, numRegs = %d, value = %p\n",
                 sw,
                 addr,
                 numRegs,
                 (void *) value);

    /* The switch is going down or fail, just return */
    if (fmRootApi->fmSwitchStateTable[sw]->state >= FM_SWITCH_STATE_GOING_DOWN)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
    }

    TAKE_FIBM_LOCK(sw);
    FIBM_STATS(sw).readMulti64++;

    /* Use the value buffer to store data */
    pVal32 = (fm_uint32 *) value;
    status = fmFibmReadCSRMultInt(sw, addr, numRegs * 2, pVal32);

    if (status != FM_OK)
    {
      DROP_FIBM_LOCK(sw);
      FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);
    }

    for (i = 0 ; i < numRegs ; i++)
    {
        fm_uint32 temp;

        /* 32-bit endian is already taken care in fmFibmReadCSRMult */
        temp = pVal32[i*2];
        value[i]  = pVal32[i*2 + 1];
        value[i]  <<= 32;
        value[i] |= temp;
    }

    DROP_FIBM_LOCK(sw);
    FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);

}   /* end fmFibmReadCSRMult64 */




/*****************************************************************************/
/** fmFibmWriteCSRMult64
 * \ingroup intFibm
 *
 * \desc            Write multiple 64-bit CSR registers via FIBM.
 *
 * \note            Input value array buffer might be modified.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       numRegs contains the number of register addresses to write.
 *
 * \param[in]       addr contains the starting CSR register address to write.
 *
 * \param[in]       value points to an array of 64-bit values to write. The
 *                  array must be n elements in length.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_FIBM_TIMEOUT if no response after a timeout.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmFibmWriteCSRMult64(fm_int     sw,
                               fm_uint32  addr,
                               fm_int     numRegs,
                               fm_uint64 *value)
{
    int       i;
    fm_status status;
    fm_uint32 *pVal32;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "sw = %d, addr = 0x%08x, numRegs = %d, value = %p\n",
                 sw,
                 addr,
                 numRegs,
                 (void *) value);

    /* The switch is going down or fail, just return */
    if (fmRootApi->fmSwitchStateTable[sw]->state >= FM_SWITCH_STATE_GOING_DOWN ||
        fmPlatformBypassEnabled(sw))
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
    }

    TAKE_FIBM_LOCK(sw);
    FIBM_STATS(sw).writeMulti64++;

    pVal32 = fmAlloc( numRegs * sizeof(fm_uint64) );

    if (!pVal32)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_NO_MEM);
    }

    for (i = 0 ; i < numRegs ; i++)
    {
        fm_uint64 temp;

        /* 32-bit endian is already taken care in fmFibmWriteCSRMult */
        temp = value[i];
        pVal32[i*2]     = (temp & 0xffffffffL);
        pVal32[i*2 + 1] = (temp >> 32);
    }

    /* Use the value buffer to store data */
    status = fmFibmWriteCSRMultInt(sw, addr, numRegs * 2, pVal32);

    fmFree(pVal32);

    DROP_FIBM_LOCK(sw);

    FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);

}   /* end fmFibmWriteCSRMult64 */




/*****************************************************************************/
/** fmFibmReadScatterGather
 * \ingroup intFibm
 *
 * \desc            Read multiple discontiguous regions of register space.
 * \note            This assume all data pointer are unique, since the code
 *                  will save the fibm command in the data before setting
 *                  the return value.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       nEntries is the number of scatter-gather entries
 *                  in sgList.
 *
 * \param[in]       sgList is an array of length nEntries, which
 *                  describes each region to be read.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_FIBM_TIMEOUT if no response after a timeout.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmFibmReadScatterGather(fm_int sw, fm_int nEntries,
                                  fm_scatterGatherListEntry *sgList)
{
    fm_int     numRead;
    fm_status  status = FM_OK;
    fm_uint32  fibmCmd;
    fm_uint    readIdx;
    fm_uint    readOff;
    fm_uint    addrCnt;
    fm_uint    cnt = 0;
    fm_int     cmdLen;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "sw = %d, nEntries = %d, sgList = %p\n",
                 sw,
                 nEntries,
                 (void *) sgList);

    /* The switch is going down or fail, just return */
    if (fmRootApi->fmSwitchStateTable[sw]->state >= FM_SWITCH_STATE_GOING_DOWN)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
    }

    TAKE_FIBM_LOCK(sw);
    FIBM_STATS(sw).readSg++;

    /* Just flush the pending fibm if there is one */
    if (FIBM_INFO(sw)->buffer)
    {
        status = fmFibmFlushMsgAck(sw); /* These are batch writes */

        if (status != FM_OK)
        {
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, status);
        }
    }


    readOff = 0;  /* The current offset for data to be saved */
    readIdx = 0;  /* The current index for data to be saved */
    for (addrCnt = 0 ; addrCnt < (fm_uint)nEntries ; addrCnt++)
    {
        cnt = 0;
        while (cnt < sgList[addrCnt].count)
        {
            /* Figure out how many we need to request */
            cmdLen = sgList[addrCnt].count - cnt;
            if (cmdLen > FM_FIBM_HW_CMD_MAX_LEN)
            {
                cmdLen = FM_FIBM_HW_CMD_MAX_LEN;
            }
            /* Since we don't allow buffer chaining, we must ensure
             * that the response is limited to one buffer
             */

            /* Figuring out how many register read we can do here */
            numRead  = FM_BUFFER_SIZE_WORDS - FIBM_INFO(sw)->respLen;
            numRead -= (numRead / FM_FIBM_HW_CMD_MAX_LEN + FIBM_RESERVED_GAP);

            if (numRead < cmdLen)
            {
                /* Not enough buffer for more command, send the request
                 * and wait for response
                 */
                status = SendAndProcessResponse(sw, nEntries, sgList, &readIdx, &readOff);
                if (status != FM_OK)
                {
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, status);
                }

                /* The final result, should fill in all the request data
                 * If not, there is something wrong
                 */
                if ( (readIdx != addrCnt) || (readOff != cnt) )
                {
                    FM_LOG_ERROR(FM_LOG_CAT_FIBM,
                        "FibmReadSg: Expect readIdx %d == addrCnt %d, "
                        "readOff %d == cnt %d\n",
                        readIdx, addrCnt, readOff, cnt);
                    status = FM_ERR_FIBM_INVALID_RESPONSE;
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, status);
                }

            }   /* end if (numRead < cmdLen) */

            fibmCmd = FM_FIBM_MAKE_READ_CMD(cmdLen, (sgList[addrCnt].addr + cnt));
            fmFibmAddReadCommandMult(sw, fibmCmd, cmdLen);
            /* Temporary save the command so we can verify the response */
            sgList[addrCnt].data[cnt] = fibmCmd;

            cnt += cmdLen;
        }   /* end while (cnt < sgList[addrCnt].count) */
    }   /* end for (addrCnt = 0 ; addrCnt < (fm_uint)nEntries ; addrCnt++) */

    /* Process the last request */
    status = SendAndProcessResponse(sw, nEntries, sgList, &readIdx, &readOff);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, status);

    /* The final result, should fill in all the request data
     * If not, there is something wrong
     */
    if ( (readIdx != addrCnt) || (readOff != cnt) )
    {
        FM_LOG_ERROR(FM_LOG_CAT_FIBM,
            "FibmReadSg: Expect readIdx %d == addrCnt %d, "
            "readOff %d == cnt %d\n",
            readIdx, addrCnt, readOff, cnt);
        status = FM_ERR_FIBM_INVALID_RESPONSE;
    }

ABORT:
    DROP_FIBM_LOCK(sw);
    FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);

}   /* end fmFibmReadScatterGather */


/*****************************************************************************/
/** fmFibmWriteScatterGather
 * \ingroup intFibm
 *
 * \desc            Write multiple discontiguous regions of register space.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       nEntries is the number of scatter-gather entries
 *                  in sgList.
 *
 * \param[in]       sgList is an array of length nEntries, which
 *                  describes each region to be written.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_FIBM_TIMEOUT if no response after a timeout.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmFibmWriteScatterGather(fm_int sw, fm_int nEntries,
                                   fm_scatterGatherListEntry *sgList)
{
    fm_status status = FM_OK;
    fm_int    cnt;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "sw = %d, nEntries = %d, sgList = %p\n",
                 sw,
                 nEntries,
                 (void *) sgList);

    /* The switch is going down or fail, just return */
    if (fmRootApi->fmSwitchStateTable[sw]->state >= FM_SWITCH_STATE_GOING_DOWN ||
        fmPlatformBypassEnabled(sw))
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
    }

    TAKE_FIBM_LOCK(sw);
    FIBM_STATS(sw).writeSg++;

    /* Start batching the commands */
    fmFibmStartBatchingInt(sw);

    for (cnt = 0 ; cnt < nEntries ; cnt++)
    {
        status = fmFibmWriteCSRMultInt(sw, sgList[cnt].addr, 
                                       sgList[cnt].count,
                                       sgList[cnt].data);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, status);
    }

    /* Flush the pending command */
    status = fmFibmFlush(sw);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, status);


ABORT:
    DROP_FIBM_LOCK(sw);
    FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);

}   /* end fmFibmWriteScatterGather */





/*****************************************************************************/
/** fmFibmInit
 * \ingroup intFibm
 *
 * \desc            Perform any platform FIBM initialization.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmFibmInit(void)
{
    fm_status status;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "fmFibmInit: Create FIBM Task\n");

    /* Create the fibm task semaphore */
    status = fmCreateSemaphore("FibmProcessTaskSem",
                               FM_SEM_BINARY,
                               &fibmProcessTaskSem,
                               0);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);
    }

    /* initialize interrupt signaling semaphore */
    status = fmCreateSemaphore("fibmSlaveIntrAvail",
                               FM_SEM_BINARY,
                               &fmRootApi->fibmSlaveIntrAvail,
                               0);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);
    }

    /* Create the packet receive thread */
    status = fmCreateThread("FibmProcessTask",
                            FM_EVENT_QUEUE_SIZE_NONE,
                            fmFibmProcessTask,
                            NULL,
                            &fibmProcessTask);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);
    }

    /* Create the interrupt handler task */
    status = fmCreateThread("FibmSlaveInterruptHandlerTask",
                            FM_EVENT_QUEUE_SIZE_NONE,
                            fmInterruptHandler,
                            NULL,
                            &fmRootApi->fibmSlaveInterruptTask);


    FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);


}   /* end fmFibmInit */




/*****************************************************************************/
/** fmFibmInitSwitch
 * \ingroup intSwitch
 *
 * \desc            Perform any generic initialization to support FIBM on sw.
 *                  This structure will not be freed when a slave switch
 *                  is removed. Instead it will be reused when a slave switch
 *                  is inserted again. It is done this way to avoid using
 *                  lock on FIBM_INFO structure in the FIBM timer task and
 *                  the fibm packet receive handler.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NO_MEM if can not allocate memory.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmFibmInitSwitch(fm_int sw)
{
    fm_status     status;
    fm_fibmSwInt *fibmInfo;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM, "sw = %d\n", sw);

    if ( FIBM_INFO_VALID(sw) )
    {
        /* This can be called twice from the platform */
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
    }

    if (FIBM_INFO(sw) == NULL)
    {
        fibmInfo = fmAlloc( sizeof(fm_fibmSwInt) );

        if (!fibmInfo)
        {
            FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_NO_MEM);
        }
    }
    else
    {
        /* Use existing structure */
        fibmInfo = FIBM_INFO(sw);
    }

    memset( fibmInfo, 0, sizeof(fm_fibmSwInt) );

    /* Allocate buffer to save pending sent message */
    fibmInfo->sentBuffer = fmAllocateBuffer(sw);

    if (!fibmInfo->sentBuffer)
    {
        fmFree(fibmInfo);
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_NO_MEM);
    }

    /* Set this switch is fibm slave. One use for this is for selecting
     * which interrupt task to process interrupt for this switch
     */
    fmRootApi->isSwitchFibmSlave[sw] = TRUE;

    /* Create the fibm response semaphore */
    status = fmCreateSemaphore("FIBM Response",
                               FM_SEM_BINARY,
                               &fibmInfo->fibmResponseSemaphore,
                               0);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);

    /* Choose reg access mode */
    fibmInfo->mode |= FM_FIBM_MODE_WRITE_SYNC; /* Configured state */
    fibmInfo->flags |= FM_FIBM_FLAGS_WRITE_SYNC; /* Running state */

    /********************************************************
     * Data below are just temporary
     * Each platform must provide the correct info
     * via the platform API fmPlatformFibmGetSwitchConfig
     ********************************************************/
    fibmInfo->cfg.masterSwitch = 0;
    /* Set up default dest and src glort */
    fibmInfo->cfg.masterGlort = 0x1000;
    fibmInfo->cfg.slaveGlort  = 0x1000;

    /* which logical port on the master switch
     * the slave/remote sw is connected to
     */
    fibmInfo->cfg.masterMgmtPort = sw;
    fibmInfo->cfg.slaveMgmtPort  = fibmInfo->cfg.masterMgmtPort;


    /* Create lock last for various check FIBM_INFO_VALID */
    status = fmCreateLock("Fibm Access",
                          &GET_PLAT_STATE(sw)->fibmLock);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);
    }

    /* Create lock for multi-thread protection (Tx, Rx and Retransmit) */
    status = fmCreateLock("Fibm Tx/Rx",
                          &GET_PLAT_STATE(sw)->fibmTxRxLock);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);
    }

    /* set it here last to make sure no thread check for this
     * when this structure is not initalized
     */
    FIBM_INFO(sw) = fibmInfo;

    /* Set the default retries values */
    GET_PLAT_STATE(sw)->fibmMaxRetries = FM_FIBM_MAX_RETRIES;
    GET_PLAT_STATE(sw)->fibmThresholdRetries = FM_FIBM_MAX_RETRIES;
    GET_PLAT_STATE(sw)->fibmForceTimeout = FALSE;

    FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);

}   /* end fmFibmInitSwitch */




/*****************************************************************************/
/** fmFibmCleanupSwitch
 * \ingroup intSwitch
 *
 * \desc            Perform generic fibm cleanup. Don't free the FIBM info.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmFibmCleanupSwitch(fm_int sw)
{
    fm_status     status;
    fm_fibmSwInt *fibmInfo;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM, "sw = %d\n", sw);

    if ( !FIBM_INFO_VALID(sw) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
    }

    status = fmDeleteLock(&GET_PLAT_STATE(sw)->fibmTxRxLock);

    if (status != FM_OK)
    {
        FM_LOG_WARNING(FM_LOG_CAT_FIBM,
              "ERROR: fmFibmCleanupSwitch - Unable to delete fibm TxRx lock\n");
    }

    status = fmDeleteLock(&GET_PLAT_STATE(sw)->fibmLock);

    if (status != FM_OK)
    {
        FM_LOG_WARNING(FM_LOG_CAT_FIBM,
                       "ERROR: fmFibmCleanupSwitch - Unable to delete fibm lock\n");
    }

    /* Instead of freeing the FIBM_INFO, we set this to NULL to
     * indicate the FIBM_INFO is invalid
     */
    GET_PLAT_STATE(sw)->fibmLock.handle = NULL;

    fibmInfo = FIBM_INFO(sw);

    /* Reset the INIT flag in order to be able to switch back in FIBM mode
       and use the new fm_fibmSwitchConfig provided by the application. */
    fibmInfo->flags &= ~FM_FIBM_FLAGS_INITED;

    /* FIXME: Is this the correct way to do this */
    /* Must signal semaphore before call delete since delete will try
     * to capture the semaphore first
     */
    fmSignalSemaphore(&fibmInfo->fibmResponseSemaphore);
    /* Delete the fibm response semaphore */
    status = fmDeleteSemaphore(&fibmInfo->fibmResponseSemaphore);

    if (status != FM_OK)
    {
        FM_LOG_WARNING(FM_LOG_CAT_FIBM,
                       "ERROR: fmFibmCleanupSwitch - Unable to delete fibm semaphore\n");
    }

    /* Free any stored buffer */
    if (fibmInfo->response)
    {
        /* There is an old buffer stored. Free it */
        fmFreeBufferChain(sw, fibmInfo->response);
    }

    if (fibmInfo->sentBuffer)
    {
        fmFreeBufferChain(sw, fibmInfo->sentBuffer);
    }

    if (fibmInfo->buffer)
    {
        fmFreeBufferChain(sw, fibmInfo->buffer);
    }

    /* Clear this flag in case non-remote switch is inserted this slot */
    fmRootApi->isSwitchFibmSlave[sw] = FALSE;

    FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);

}   /* end fmFibmCleanupSwitch */




/*****************************************************************************/
/** fmFibmDeleteSwitch
 * \ingroup intSwitch
 *
 * \desc            Perform generic fibm cleanup of the switch and
 *                  free the FIBM info.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmFibmDeleteSwitch(fm_int sw)
{
    fm_status     status;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM, "sw = %d\n", sw);

    if ( !FIBM_INFO_VALID(sw) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
    }

    status = fmFibmCleanupSwitch(sw);

    if (status != FM_OK)
    {
        FM_LOG_WARNING(FM_LOG_CAT_FIBM,
          "ERROR: fmFibmDeleteSwitch - Unable to cleanup fibm switch %d\n", sw);
    }

    if (FIBM_INFO(sw))
    {
        fmFree(FIBM_INFO(sw));
        FIBM_INFO(sw) = NULL;
    }

    FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);

}   /* end fmFibmDeleteSwitch */




/*****************************************************************************/
/** fmFibmProcessPktHandler
 * \ingroup intFibm
 *
 * \desc            The receive task calls this handler to process any mgmt
 *                  packet. This will hold the buffer and is responsible for
 *                  freeing the buffer after use.
 *
 * \param[in]       receiveSw is the switch number receive the packet.
 *
 * \param[in]       buf is buffer containing the mgmt message, including ISLTag.
 *
 * \param[in]       pIslTag points to array containing ISL tags, if ISLTag is not
 *                  included in the packet data.
 *
 * \return          FM_OK.
 *
 *****************************************************************************/
fm_status fmFibmProcessPktHandler(fm_int receiveSw, fm_buffer *buf, fm_uint32 *pIslTag)
{
    fm_status status = FM_OK;
    fm_int    sw;
    fm_uint32 IslTag[2];
    fm_uint16 tag, srcGlort;

    FM_NOT_USED(receiveSw);

    if (pIslTag)
    {
        /* Buffer data does not contain ISL tag */
        IslTag[0] = pIslTag[0];
        IslTag[1] = pIslTag[1];
        tag = ntohl(buf->data[FM_FIBM_OFFSET_ETHERTYPE]) & 0xFFFF;
    }
    else
    {
        /* Buffer data does contain ISL tag */
        IslTag[0] = ntohl(buf->data[3]);
        IslTag[1] = ntohl(buf->data[4]);
        tag = ntohl(buf->data[FM_FIBM_OFFSET_ETHERTYPE+2]) & 0xFFFF;
    }

    /* Associate response to correct switch */
    srcGlort = IslTag[1] >> 16;

    sw = fmFibmFindSwitchByGlort(srcGlort);

    if (fibmDbg)
    {
        /* Due to the fact that dumping out the message takes
         * too long so the fibm timer thread thinks that the
         * the message has timeout, so we clear it here for now
         */
        if ((sw >= 0) && FIBM_INFO(sw)) 
        {
            /* We don't validate sw later so need to check here also */
            FIBM_INFO(sw)->sentCount = 0;
        }
        FIBM_DBG("Sw#%u: FIBM RX Len: %u Tag: %u ISLTag: 0x%08x 0x%08x\n",
                 sw, buf->len, tag, IslTag[0], IslTag[1]);

        fmFibmDumpBuf(buf->data, buf->len / 4);
    }

    if ( (sw > FM_LAST_FOCALPOINT) ||
        (sw < 0) || !FIBM_INFO_VALID(sw) )
    {
        fmFreeBufferChain(sw, buf);
        return FM_OK;
    }

    if (pIslTag == NULL)
    {
        /* Skip 2 words for ISL Tags, for correct FM_FIBM_OFFSET_DATA */
        buf->data += 2;
    }

    TAKE_FIBM_TXRX_LOCK(sw);

    if (ntohl(buf->data[FM_FIBM_OFFSET_DATA]) == (fm_uint32) FM_FIBM_HW_INTR)
    {
        /* Next 8 words are zero, but don't really need to check */
        FM_LOG_DEBUG(FM_LOG_CAT_EVENT,
                     "fmFibmProcessPktHandler: interrupt!\n");

        FIBM_STATS(sw).intrRx++;

        if (IS_PLAT_STATE_INITED)
        {
            TAKE_PLAT_LOCK(sw, FM_PLAT_INFO);
            GET_PLAT_STATE(sw)->intrSource |= FM_INTERRUPT_SOURCE_ISR;
            DROP_PLAT_LOCK(sw, FM_PLAT_INFO);

            fmSignalSemaphore(&fmRootApi->fibmSlaveIntrAvail);
        }

        goto ABORT;
    }

    FIBM_STATS(sw).response++;

    if (!FIBM_INFO(sw)->sentCount && !fibmDbg)
    {
        if ((tag+1) == FIBM_INFO(sw)->seqTag)
        {
            /* This is a stale packet caused by resent
             * No need to do anything since resent stats already
             * taken care of it
             */
            FM_LOG_DEBUG(FM_LOG_CAT_FIBM,
                       "Sw#%u: Got a stale packet FIBM tag %u current tag %u\n",
                       sw, tag, FIBM_INFO(sw)->seqTag);
            FIBM_STATS(sw).staleRxPkts++;
        }
        else
        {
            FM_LOG_WARNING(FM_LOG_CAT_FIBM,
                       "Sw#%u: Not waiting for any FIBM tag %u current tag %u\n",
                       sw, tag, FIBM_INFO(sw)->seqTag);
            FIBM_STATS(sw).unexpectedTag++;
        }

        goto ABORT;
    }

    if (tag != FIBM_INFO(sw)->seqTag)
    {
        if ((tag+1) == FIBM_INFO(sw)->seqTag)
        {
            /* This is a stale packet caused by resent
             * No need to do anything since resent stats already
             * taken care of it
             */
            FM_LOG_DEBUG(FM_LOG_CAT_FIBM,
                       "Sw#%u: Got a stale packet FIBM tag %u current tag %u\n",
                       sw, tag, FIBM_INFO(sw)->seqTag);
            FIBM_STATS(sw).staleRxPkts++;
        }
        else
        {
            FM_LOG_WARNING(FM_LOG_CAT_FIBM,
                       "Sw#%u: Unexpected FIBM tag %u wanted %u\n",
                       sw, tag, FIBM_INFO(sw)->seqTag);
            FIBM_STATS(sw).unexpectedTag++;
        }

        goto ABORT;
    }

    if (fibmDropPkt)
    {
        fibmDropPkt--;
        FIBM_DBG("Dropping FIBM response message. fibmDropPkt %u\n", fibmDropPkt);
        goto ABORT;
    }

    FIBM_INFO(sw)->sentCount    = 0; /* Sucessfuly received */
    FIBM_INFO(sw)->timeoutCount = 0;
    /* Increase the tag for next message */
    FIBM_INFO(sw)->seqTag++;

    /* Just a sanity check. We dont expect chaining here */
    if ( buf->next || (buf->len > FM_BUFFER_SIZE_BYTES) )
    {
        FM_LOG_WARNING(FM_LOG_CAT_FIBM,
                       "ERROR: Not expected a chained buffer %p of length %u\n",
                       (void *) buf->next, buf->len);
    }

    if (FIBM_INFO(sw)->flags & FM_FIBM_FLAGS_WAIT_RESPONSE)
    {
        /* We are not expecting a valid buffer here. A saved one
         * should be comsumed by the time another one is received.
         * If there is still some buffer here, this might indicate
         * a memory leak or something unexpected.
         */
        if (FIBM_INFO(sw)->response)
        {
            FM_LOG_WARNING(FM_LOG_CAT_FIBM,
                           "ERROR: Not expecting a valid buffer\n");
        }

        /* Save the new one */
        FIBM_INFO(sw)->response = buf;

        /* Signal the semaphore to wake up the pending thread */
        status = fmSignalSemaphore(&FIBM_INFO(sw)->fibmResponseSemaphore);

        DROP_FIBM_TXRX_LOCK(sw);

        /* Return here, since we are saving the buffer for later processing */
        return status;
    }

ABORT:
    DROP_FIBM_TXRX_LOCK(sw);
    fmFreeBufferChain(sw, buf);
    return FM_OK;

}   /* end fmFibmProcessPktHandler */



/*****************************************************************************/
/** fmFibmEnableBatching
 * \ingroup intFibm
 *
 * \desc            Enable batching of write commands. Specify TRUE to start
 *                  batching of write commands, and FALSE to stop batching
 *                  write commands and flush the pending buffer.
 *
 * \param[in]       sw is switch number.
 *
 * \param[in]       enabled specifies where to TRUE or FALSE
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *****************************************************************************/
fm_status fmFibmEnableBatching(fm_int sw, fm_bool enabled)
{
    fm_status status = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM, "sw = %d enabled %d\n", sw, enabled);

    TAKE_FIBM_LOCK(sw);

    if (enabled)
    {
        fmFibmStartBatchingInt(sw);
        FIBM_INFO(sw)->enableBatchingCnt++;
    }
    else
    {
        FIBM_INFO(sw)->enableBatchingCnt--;
        if (FIBM_INFO(sw)->enableBatchingCnt <= 0)
        {
            status = fmFibmFlush(sw);

            /* Make sure the counter has not a negative value. */
            FIBM_INFO(sw)->enableBatchingCnt = 0;
        }
    }

    DROP_FIBM_LOCK(sw);

    FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);

}   /* end fmFibmEnableBatching */



/*****************************************************************************/
/** fmFibmGetStats
 * \ingroup intFibm
 *
 * \desc            Return FIBM statistics for a given switch.
 *
 * \param[in]       sw is switch number.
 *
 * \param[in]       stats points to a ''fm_fibmStats'' structure to be filled
 *                  in by this function with the statistics.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if sw is out of range.
 * \return          FM_ERR_FIBM_NOT_ENABLED if Fibm is not enabled.
 *
 *****************************************************************************/
fm_status fmFibmGetStats(fm_int sw, fm_fibmStats *stats)
{

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM, "sw = %d\n", sw);

    if ( (sw > FM_MAX_NUM_FOCALPOINTS) || (sw < 0) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_INVALID_ARGUMENT);
    }

    /* It is okay to get stats when FIBM_INFO is not valid */
    if (FIBM_INFO(sw) == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_FIBM_NOT_ENABLED);
    }

    TAKE_FIBM_LOCK(sw);
    memcpy( stats, &FIBM_STATS(sw), sizeof(fm_fibmStats) );
    DROP_FIBM_LOCK(sw);

    FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);

}   /* end fmFibmGetStats */




/*****************************************************************************/
/** fmFibmResetStats
 * \ingroup intFibm
 *
 * \desc            Clear FIBM statistics for a given switch.
 *
 * \param[in]       sw is switch number.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if sw is out of range.
 * \return          FM_ERR_FIBM_NOT_ENABLED if Fibm is not enabled.
 *
 *****************************************************************************/
fm_status fmFibmResetStats(fm_int sw)
{

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM, "sw = %d\n", sw);

    if ( (sw > FM_MAX_NUM_FOCALPOINTS) || (sw < 0) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_INVALID_ARGUMENT);
    }

    if (FIBM_INFO(sw) == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
    }

    TAKE_FIBM_LOCK(sw);
    memset( &FIBM_STATS(sw), 0, sizeof(fm_fibmStats) );
    DROP_FIBM_LOCK(sw);

    FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);

}   /* end fmFibmResetStats */




/*****************************************************************************/
/** fmFibmForceTimeout
 * \ingroup intFibm
 *
 * \desc            Force a FIBM retry timeout if a timeout occurs.
 *
 * \param[in]       sw is switch number.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if sw is out of range.
 * \return          FM_ERR_FIBM_NOT_ENABLED if Fibm is not enabled.
 *
 *****************************************************************************/
fm_status fmFibmForceTimeout(fm_int sw)
{
    FM_LOG_ENTRY(FM_LOG_CAT_FIBM, "sw = %d\n", sw);

    if ( (sw > FM_MAX_NUM_FOCALPOINTS) || (sw < 0) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_INVALID_ARGUMENT);
    }

    if (FIBM_INFO(sw) == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_FIBM_NOT_ENABLED);
    }

    GET_PLAT_STATE(sw)->fibmForceTimeout = TRUE;

    FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);

}   /* end fmFibmForceTimeout */
