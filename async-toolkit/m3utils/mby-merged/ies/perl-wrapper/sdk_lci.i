/* vim:et:sw=4:syntax=c:ts=4:tw=79:
 */
/******************************************************************************
 * File:            sdk_lci.i
 * Creation Date:   14/11/07
 * Description:     Test Engine 2 Port 0 Traffic Generator
 *
 * INTEL CONFIDENTIAL
 * Copyright 2007 - 2011 Intel Corporation. All Rights Reserved. 
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

#ifdef SWIG_LCI_DIRECT_ACCESS
%perlcode
%{
package SDK::LCI;
use strict;
use warnings;

our @rx = ();
our @tx = ();

%}

%insert(init)
%{
    FM_NOT_USED(items);

    newXS("SDK::LCI::Dequeue", fmLCIDequeue, __FILE__);
    newXS("SDK::LCI::Enqueue", fmLCIEnqueue, __FILE__);
    newXS("SDK::LCI::GetRXQueueLength", fmLCIGetRXQueueLength, __FILE__);
    newXS("SDK::LCI::SetBurstSize", fmLCISetBurstSize, __FILE__);
    newXS("SDK::LCI::SetRepetitionCount", fmLCISetRepetitionCount, __FILE__);
    newXS("SDK::LCI::SetState", fmLCISetState, __FILE__);
%}

%{

#include <stdlib.h>
#include <stdbool.h>
#include <endian.h>
#include <math.h>
#include <string.h>

#include <api/internal/fm4000/fm4000_api_regs_int.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define TAKE_LCI_CFG_LOCK                                                     \
    fmCaptureLock(&lciConfigurationLock, FM_WAIT_FOREVER);

#define DROP_LCI_CFG_LOCK                                                     \
    fmReleaseLock(&lciConfigurationLock);

#define TAKE_LCI_RX_QUEUE_LOCK                                                \
    fmCaptureLock(&lciRxQueueLock, FM_WAIT_FOREVER);

#define DROP_LCI_RX_QUEUE_LOCK                                                \
    fmReleaseLock(&lciRxQueueLock);

#define TAKE_LCI_TX_QUEUE_LOCK                                                \
    fmCaptureLock(&lciTxQueueLock, FM_WAIT_FOREVER);

#define DROP_LCI_TX_QUEUE_LOCK                                                \
    fmReleaseLock(&lciTxQueueLock);

#define LCI_RX_QUEUE        "SDK::LCI::rx"
#define LCI_TX_QUEUE        "SDK::LCI::tx"

#define LCI_BUFSIZ          2560    /* words */

#define LCI_LENGTH_INC      (1 << 28)
#define LCI_LENGTH_RAND     (2 << 28)

#define LCI_FILL_ZERO       0
#define LCI_FILL_INC        1
#define LCI_FILL_RAND       2

#define LCI_LOOP_FOREVER    0

typedef struct _fm_rootTg {
    /* the burst repetition count */
    int     N;

    /* the packet burst size */
    int     M;

    /* the traffic generator state */
    fm_bool state;
} fm_rootTg;

/*****************************************************************************
 * Global Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

SWIGINTERN fm_lock      lciConfigurationLock;
SWIGINTERN fm_lock      lciRxQueueLock;
SWIGINTERN fm_lock      lciTxQueueLock;
SWIGINTERN fm_semaphore lciSemHandle;
SWIGINTERN fm_thread    lciPacketTransmissionTask;

SWIGINTERN fm_rootTg    fmRootTg;

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

void* fmCustomInterruptHandler(void *threadArg);
void* fmCustomPacketTransmissionHandler(void *threadArg);

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/** fmCustomInterruptHandler
 *
 *****************************************************************************/
void* fmCustomInterruptHandler(void *threadArg)
{
    AV           *payload;
    AV           *queue;
    HV           *packet;
    SV           **sv;
    fm_status    status;
    fm_thread    *thread;
    const fm_int switchNum = 0;
    fm_uint      source;
    fm_uint32    lci_ip, lci_status, word;

    thread = FM_GET_THREAD_HANDLE(threadArg);

    while (TRUE)
    {
        status = fmCaptureSemaphore(&(fmRootApi->intrAvail), FM_WAIT_FOREVER);
        if (status != FM_OK)
        {
            FM_LOG_FATAL(FM_LOG_CAT_EVENT_INTR, "%s\n", fmErrorMsg(status));
            fmExitThread(thread);
        }

        fmPlatformGetInterrupt(switchNum, FM_INTERRUPT_SOURCE_ISR, &source);
        if (status != FM_OK)
        {
            continue;
        }

        switch (source)
        {
            case FM_INTERRUPT_SOURCE_ISR:
                fmReadUINT32(switchNum, FM4000_LCI_IP, &lci_ip);
                if (FM_GET_BIT(lci_ip, FM4000_LCI_IP, newFrameRecv))
                {
                    /***************************************************
                     * Mask the LCI `newFrameRecv' interrupt to allow 
                     * the current interrupt to be handled.
                     **************************************************/
                    fmWriteUINT32Field(switchNum,
                                       FM4000_LCI_IM,
                                       FM4000_LCI_IM_b_newFrameRecv,
                                       1,
                                       1);

                    /***************************************************
                     * Reset the LCI `newFrameRecv' interrupt to allow a
                     * future interrupt to be handled.
                     **************************************************/
                    fmWriteUINT32Field(switchNum,
                                       FM4000_LCI_IP,
                                       FM4000_LCI_IP_b_newFrameRecv,
                                       1,
                                       1);

                    fmReadUINT32(switchNum, FM4000_LCI_STATUS, &lci_status);
                    while (FM_GET_BIT(lci_status, FM4000_LCI_STATUS, rxReady))
                    {
                        packet = newHV();
                        if (!(sv = fmSWIGAddHashKey(packet, "payload")))
                        {
                            goto NEXT;
                        }
                        sv_setsv(*sv, newRV_noinc((SV *) newAV()));
                        payload = (AV *) SvRV(*sv);

                        TAKE_REG_LOCK(switchNum);

                        fmReadUINT32(switchNum, FM4000_LCI_STATUS, &lci_status);
                        while (!FM_GET_BIT(lci_status,
                                           FM4000_LCI_STATUS,
                                           rxEndOfFrame))
                        {
                            fmReadUINT32(switchNum, FM4000_LCI_RX_FIFO, &word);
                            av_push(payload, newSVuv((UV) word));

                            fmReadUINT32(switchNum,
                                         FM4000_LCI_STATUS,
                                         &lci_status);
                        }

                        /***************************************************
                         * Read the RX status word.
                         **************************************************/
                        fmReadUINT32(switchNum, FM4000_LCI_RX_FIFO, &word);

                        DROP_REG_LOCK(switchNum);

                        if (!(sv = fmSWIGAddHashKey(packet, "length")))
                        {
                            goto NEXT;
                        }
                        sv_setiv(*sv, (IV) (word >> 16));

                        /***************************************************
                         * Push the packet onto the RX packet queue.
                         **************************************************/
                        TAKE_LCI_RX_QUEUE_LOCK;

                        queue = get_av(LCI_RX_QUEUE, 0);
                        if (queue != NULL)
                        {
                            av_push(queue, newRV_noinc((SV *) packet));
                        }

                        DROP_LCI_RX_QUEUE_LOCK;

                        fmReadUINT32(switchNum, FM4000_LCI_STATUS, &lci_status);
                        continue;

NEXT:
                        /***************************************************
                         * Prevent memory leaks by destroying the hash.
                         **************************************************/
                        hv_undef(packet);

                        fmReadUINT32(switchNum, FM4000_LCI_STATUS, &lci_status);
                    }

                    /***************************************************
                     * Unmask the LCI `newFrameRecv' interrupt to allow
                     * a new interrupt to be handled.
                     **************************************************/
                    fmWriteUINT32Field(switchNum,
                                       FM4000_LCI_IM,
                                       FM4000_LCI_IM_b_newFrameRecv,
                                       1,
                                       0);
                }
                break;

            default:
                break;
        }

        /***************************************************
         * Re-enable the interrupt.
         **************************************************/
        status = fmPlatformEnableInterrupt(switchNum, source);
        if (status != FM_OK)
        {
            FM_LOG_FATAL(FM_LOG_CAT_EVENT_INTR, "%s\n", fmErrorMsg(status));
            fmExitThread(thread);
        }
    }
}

/** fmCustomPacketTransmissionHandler
 *
 *****************************************************************************/
void *fmCustomPacketTransmissionHandler(void *threadArg)
{
    AV           *payload;
    AV           *queue;
    HV           *packet;
    SV           *e;
    SV           **sv;
    I32          index;
    fm_status    status;
    fm_thread    *thread;
    const fm_int switchNum = 0;
    fm_uint32    data[LCI_BUFSIZ];
    int          fill, i, j, length, M, N, n, shift, size;
    fm_bool      loop, send;

    thread = FM_GET_THREAD_HANDLE(threadArg);

    while (TRUE)
    {
        status = fmCaptureSemaphore(&lciSemHandle, FM_WAIT_FOREVER);

        TAKE_LCI_CFG_LOCK;

        index = 0;
        loop = fmRootTg.N == LCI_LOOP_FOREVER ? true : false;
        M = fmRootTg.M;
        N = fmRootTg.N;

        DROP_LCI_CFG_LOCK;

        send = true;
        while (TRUE)
        {
            TAKE_LCI_CFG_LOCK;

            if (!send || fmRootTg.state == FM_DISABLED)
            {
                /***************************************************
                 * The traffic generator has been stopped or the end
                 * of the TX queue has been reached.
                 **************************************************/
                fmRootTg.state = FM_DISABLED;

                DROP_LCI_CFG_LOCK;

                goto STOP;
            }

            DROP_LCI_CFG_LOCK;

            TAKE_LCI_TX_QUEUE_LOCK;

            e = NULL;

            if (loop)
            {
                /***************************************************
                 * The traffic generator is configured to send until
                 * stopped explicitly.
                 **************************************************/
                if ((queue = get_av(LCI_TX_QUEUE, 0)))
                {
                    if ((sv = av_fetch(queue, index, 0)))
                    {
                        e = *sv;
                        index = (index + 1) % ((int) (av_len(queue) + 1));
                    }
                }
            }
            else
            {
                /***************************************************
                 * The traffic generator is configured to send
                 * `fmRootTg.N' bursts of packets.
                 **************************************************/
                if ((queue = get_av(LCI_TX_QUEUE, 0)))
                {
                    e = av_shift(queue);
                    if (!SvTRUE(e))
                    {
                        send = false;
                    }
                }
            }

            DROP_LCI_TX_QUEUE_LOCK;

            if (e && SvROK(e) && SvTYPE(SvRV(e)) == SVt_PVHV)
            {
                packet = (HV *) SvRV(e);
            }
            else
            {
                goto NEXT;
            }

            /***************************************************
             * Verify the tg_packet_t hash.
             **************************************************/
            if ((sv = fmSWIGGetHashKey(packet, "length")) == NULL)
            {
                goto NEXT;
            }
            length = MIN((int) SvIV(*sv), 4 * LCI_BUFSIZ);

            /* Compensate for the frame check sequence.  */
            length -= 4;

            if ((sv = fmSWIGGetHashKey(packet, "fill")) == NULL)
            {
                goto NEXT;
            }
            fill = (int) SvIV(*sv);

            if ((sv = fmSWIGGetHashKey(packet, "payload")) == NULL
                || !SvROK(*sv) || SvTYPE(SvRV(*sv)) != SVt_PVAV)
            {
                goto NEXT;
            }
            payload = (AV *) SvRV(*sv);

            /***************************************************
             * Reformat the packet header.
             **************************************************/
            memset(data, 0, sizeof(data));
            n = (int) (av_len(payload) + 1);
            for (i = 0; i < n; i++)
            {
                if ((sv = av_fetch(payload, (I32) i, 0)) == NULL)
                {
                    continue;
                }
                shift = 24 - ((i % 4) << 3);
                data[(i >> 2)] |= (((fm_uint32) SvUV(*sv)) & 0xFF) << shift;
            }

            for (i = 0; ; i++)
            {
                TAKE_LCI_CFG_LOCK;

                if (i >= M || fmRootTg.state == FM_DISABLED)
                {
                    /***************************************************
                     * `fmRootTg.M' copies of the current packet have
                     * been sent or the traffic generator has been
                     * stopped.
                     **************************************************/
                    DROP_LCI_CFG_LOCK;

                    goto NEXT;
                }

                DROP_LCI_CFG_LOCK;

                if (!loop)
                {
                    N -= 1;
                    if (N <= 0)
                    {
                        /***************************************************
                         * `fmRootTg.N' packets have been sent.
                         **************************************************/
                        send = false;
                        goto NEXT;
                    }
                }

                switch (length)
                {
                    case LCI_LENGTH_INC:
                        size = ++size > 1518 ? 64 : size;
                        
                        break;

                    case LCI_LENGTH_RAND:
                        size = MAX(64, MIN(1518, random() & 0x7FF));
                        break;

                    default:
                        size = length;
                }

                switch (fill)
                {
                    case LCI_FILL_ZERO:
                        break;

                    case LCI_FILL_RAND:
                        for (j = n; j < size; j++)
                        {
                            shift = 24 - ((j % 4) << 3);
                            data[j >> 2] |=
                                (((fm_uint32) random()) & 0xFF) << shift;
                        }
                        break;

                    case LCI_FILL_INC:
                    default:
                        for (j = n; j < size; j++)
                        {
                            shift = 24 - ((j % 4) << 3);
                            data[j >> 2] |= ((fm_uint32) (j & 0xFF)) << shift;
                        }
                        break;
                }

                n = (int) ceilf(((float) size) / 4.0);

                TAKE_REG_LOCK(switchNum);

                fmWriteUINT32(switchNum,
                              FM4000_LCI_TX_FIFO,
                              (((fm_uint32) size) << 16) | 1);
                for (j = 0; j < n; j++)
                {
                    fmWriteUINT32(switchNum, FM4000_LCI_TX_FIFO, data[j]);
                }

                DROP_REG_LOCK(switchNum);
            }

NEXT:
            if (!loop && packet)
            {
                /***************************************************
                 * Prevent memory leaks by destroying the hash.
                 **************************************************/
                hv_undef(packet);
            }
        }
STOP:
        ;
    }
}

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/** fmLCIDequeue
 *
 * \desc        dequeues a packet
 *
 * \return      a tg_packet_t Perl HASH if successful
 * \return      &PL_sv_undef otherwise
 *
 *****************************************************************************/
XS(fmLCIDequeue)
{
    AV *queue;
    SV *result;

    dXSARGS;

    FM_NOT_USED(items);

    result = &PL_sv_undef;

    TAKE_LCI_RX_QUEUE_LOCK;

    queue = get_av(LCI_RX_QUEUE, 0);
    if (queue != NULL)
    {
        result = sv_2mortal(av_shift(queue));
    }

    DROP_LCI_RX_QUEUE_LOCK;

    ST(0) = result;
    XSRETURN(1);
}

/** fmLCIEnqueue
 *
 * \desc        enqueues a packet
 *
 * \param[in]   packet a tg_packet_t Perl HASH containing the packet to be
 *              enqueued
 *
 * \return      FM_OK if successful
 * \return      FM_FAIL otherwise
 *
 *****************************************************************************/
XS(fmLCIEnqueue)
{
    AV *queue;
    SV *packet;
    SV *result;

    dXSARGS;

    result = sv_2mortal(newSViv((IV) FM_FAIL));

    if (items == 1)
    {
        packet = ST(0);

        if (SvROK(packet) && SvTYPE(SvRV(packet)) == SVt_PVHV)
        {
            TAKE_LCI_TX_QUEUE_LOCK;

            queue = get_av(LCI_TX_QUEUE, 0);
            if (queue != NULL)
            {
                /***************************************************
                 * Increment the reference count for `packet' to
                 * prevent the SV it points to to become undefined
                 * whenever the calling function goes out-of-scope.
                 **************************************************/
                av_push(queue, SvREFCNT_inc(packet));
                sv_setiv(result, (IV) FM_OK);
            }

            DROP_LCI_TX_QUEUE_LOCK;
        }
    }

    ST(0) = result;
    XSRETURN(1);
}

/** fmLCIGetRXQueueLength
 *
 * \desc        retrieves the RX queue length
 *
 * \return      the RX queue length
 *
 *****************************************************************************/
XS(fmLCIGetRXQueueLength)
{
    AV *queue;
    SV *result;

    dXSARGS;

    FM_NOT_USED(items);

    result = sv_2mortal(newSViv(0));

    TAKE_LCI_RX_QUEUE_LOCK;

    queue = get_av(LCI_RX_QUEUE, 0);
    if (queue != NULL)
    {
        sv_setiv(result, (IV) (av_len(queue) + 1));
    }

    DROP_LCI_RX_QUEUE_LOCK;

    ST(0) = result;
    XSRETURN(1);
}

/** fmLCISetBurstSize
 *
 * \desc        sets the packet burst size
 *
 * \param[in]   count the packet burst size
 *
 * \return      FM_OK if successful
 * \return      FM_FAIL otherwise
 *
 *****************************************************************************/
XS(fmLCISetBurstSize)
{
    SV *result;

    dXSARGS;

    result = sv_2mortal(newSViv((IV) FM_FAIL));

    TAKE_LCI_CFG_LOCK;

    if (items == 1 && fmRootTg.state == FM_DISABLED)
    {
        fmRootTg.M = (fm_int) SvIV(ST(0));
        sv_setiv(result, (IV) FM_OK);
    }

    DROP_LCI_CFG_LOCK;

    ST(0) = result;
    XSRETURN(1);
}

/** fmLCISetRepetitionCount
 *
 * \desc        sets the burst repitition count
 *
 * \param[in]   count the burst repitition count
 *
 * \return      FM_OK if successful
 * \return      FM_FAIL otherwise
 *
 *****************************************************************************/
XS(fmLCISetRepetitionCount)
{
    SV *result;

    dXSARGS;

    result = sv_2mortal(newSViv((IV) FM_FAIL));

    TAKE_LCI_CFG_LOCK;

    if (items == 1 && fmRootTg.state == FM_DISABLED)
    {
        fmRootTg.N = (fm_int) SvIV(ST(0));
        sv_setiv(result, (IV) FM_OK);
    }

    DROP_LCI_CFG_LOCK;

    ST(0) = result;
    XSRETURN(1);
}

/** fmLCISetState
 *
 * \desc        sets the traffic generator state
 *
 * \param[in]   state boolean indicating whether the traffic generator is
 *              enabled (FM_ENABLED) or disabled (FM_DISABLED)
 *
 * \return      FM_OK if successful
 * \return      FM_FAIL otherwise
 *
 *****************************************************************************/
XS(fmLCISetState)
{
    SV *result;

    dXSARGS;

    result = sv_2mortal(newSViv((IV) FM_FAIL));

    if (items == 1)
    {
        TAKE_LCI_CFG_LOCK;

        if (SvUV(ST(0)))
        {
            fmRootTg.state = FM_ENABLED;
            fmReleaseSemaphore(&lciSemHandle);
        }
        else
        {
            fmRootTg.state = FM_DISABLED;
        }

        DROP_LCI_CFG_LOCK;

        sv_setiv(result, (IV) FM_OK);
    }

    ST(0) = result;
    XSRETURN(1);
}

%}

%inline
%{

/** fmLCISetDirectAccess
 *
 * \desc        enables or disables the LCI direct access mode
 *
 * \param[in]   state indicates whether to enable the LCI direct access mode
 *              (TRUE) or to disable the LCI direct access mode (FALSE)
 *
 * \return      FM_OK if successful
 *
 *****************************************************************************/
fm_status fmLCISetDirectAccess(fm_bool state)
{
    fm_status    status;
    const fm_int switchNum = 0;

    if (state)
    {
        /***************************************************
         * Create a LCI configuration lock.
         **************************************************/
        status = fmCreateLock("LCIConfigurationLock", &lciConfigurationLock);
        if (status != FM_OK)
        {
            goto ABORT;
        }

        /***************************************************
         * Create a lock for the LCI RX queue and start the
         * interrupt handler which will handle LCI packet
         * reception interrupts.
         **************************************************/
        status = fmCreateLock("LCIRXQueueLock", &lciRxQueueLock);
        if (status != FM_OK)
        {
            goto ABORT;
        }

        status = fmCreateThread("CustomInterruptHandlerTask",
                                FM_EVENT_QUEUE_SIZE_NONE,
                                fmCustomInterruptHandler,
                                NULL,
                                &fmRootApi->interruptTask);
        if (status != FM_OK)
        {
            goto ABORT;
        }

        /***************************************************
         * Create a lock for the TX queue, start the packet
         * transmission thread and create a counting
         * semaphore which is used to signal the packet
         * transmission thread that one or more frames are
         * to be transmitted.
         **************************************************/
        status = fmCreateLock("LCITXQueueLock", &lciTxQueueLock);
        if (status != FM_OK)
        {
            goto ABORT;
        }

        status = fmCreateSemaphore("LCITXQueueSemaphore",
                                   FM_SEM_BINARY,
                                   &lciSemHandle,
                                   0);
        if (status != FM_OK)
        {
            goto ABORT;
        }

        status = fmCreateThread("CustomPacketTransmissionHandlerTask",
                                FM_EVENT_QUEUE_SIZE_NONE,
                                fmCustomPacketTransmissionHandler,
                                NULL,
                                &lciPacketTransmissionTask);
        if (status != FM_OK)
        {
            goto ABORT;
        }

#if BYTE_ORDER == BIG_ENDIAN
        /***************************************************
         * Configure the LCI for a big-endian CPU.
         **************************************************/
        fmWriteUINT32Field(switchNum,
                           FM4000_LCI_CFG,
                           FM4000_LCI_CFG_b_endianness,
                           1,
                           1);
#endif /* BYTE_ORDER */
        
        /***************************************************
         * Unmask the LCI `newFrameRecv' interrupt to allow
         * frames to be received.
         **************************************************/
        fmWriteUINT32Field(switchNum,
                           FM4000_LCI_IM,
                           FM4000_LCI_IM_b_newFrameRecv,
                           1,
                           0);
    }
    else
    {
        status = FM_OK;
    }

ABORT:
    return status;
}

%}

#endif /* SWIG_LCI_DIRECT_ACCESS */

