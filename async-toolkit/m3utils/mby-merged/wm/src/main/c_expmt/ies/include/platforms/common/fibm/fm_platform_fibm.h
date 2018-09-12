/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_platform_fibm.h
 * Creation Date:   May 22, 2008
 * Description:     Internal definitions related to FIBM
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2011 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM_PLATFORM_FIBM_H
#define __FM_FM_PLATFORM_FIBM_H

/* Hardware defines are only for fm4000 only now, but assume future
 * chips will have the same definitions. If not then these will
 * need to be moved to chip specific file.
 */

/* Ethertype to use for FIBM interrupt frames */
#define FM_FIBM_ETHERTYPE         0x3333

/* ISL tag Sys Pri for FIBM interrupt frames */
#define FM_FIBM_SYS_PRI           0xf

/* Location of ethertype in message without ISL Tag */
#define FM_FIBM_OFFSET_ETHERTYPE  3

/* Location of data in message without ISL Tag */
#define FM_FIBM_OFFSET_DATA       4

/* Header word command read */
#define FM_FIBM_HW_CMD_READ       (1 << 29)

/* Header word command write */
#define FM_FIBM_HW_CMD_WRITE      (2 << 29)

/* Header word interrupt */
#define FM_FIBM_HW_INTR           (7 << 29)

/* Mask of header word command length. To make 16 becomes 0 */
#define FM_FIBM_HW_CMD_LEN_MASK   (0xF) /* Mask so that we can make len 16->0 */

/* Max number of access per command. Hardware max is 16, software can use less */
#define FM_FIBM_HW_CMD_MAX_LEN    (15) /* Can use up to 16 */

/* Macro to make read header word */
#define FM_FIBM_MAKE_READ_CMD(len, addr) \
    ( FM_FIBM_HW_CMD_READ | ( (len & FM_FIBM_HW_CMD_LEN_MASK) << 24 ) | (addr & 0xFFFFFF) )

/* Macro to make write header word */
#define FM_FIBM_MAKE_WRITE_CMD(len, addr) \
    ( FM_FIBM_HW_CMD_WRITE | ( (len & FM_FIBM_HW_CMD_LEN_MASK) << 24 ) | (addr & 0xFFFFFF) )

/* Macro to get the len from the fibm command */
#define FM_FIBM_GET_CMD_LEN(cmd) \
    ( (cmd&(FM_FIBM_HW_CMD_LEN_MASK<<24))?((cmd>>24)&FM_FIBM_HW_CMD_LEN_MASK):16 )

/* For mode: synchronous operation every write command 
 *           This is for configuration. Use the flag
 *           FM_FIBM_FLAGS_WRITE_SYNC for actual running
 *           state
 */
#define FM_FIBM_MODE_WRITE_SYNC      (1 << 0)

/* For flags: to indicate that FIBM is inited already */
#define FM_FIBM_FLAGS_INITED         (1 << 0)

/* For flags: to indicate that a thread is suspended and needed to be waked up */
#define FM_FIBM_FLAGS_WAIT_RESPONSE  (1 << 1)

/* For flags: to indicate current state using asynchronous write */
#define FM_FIBM_FLAGS_WRITE_SYNC     (1 << 2)


#define FIBM_INFO(sw)   (GET_PLAT_STATE(sw)->fibmInfo)
#define FIBM_STATS(sw)  (GET_PLAT_STATE(sw)->fibmInfo->stats)

/* This is used to indicate if FIBM_INFO is valid, since FIBM_INFO will not
 * be freed once it is allocated. So the code should not just check for
 * FIBM_INFO(sw) != NULL
 */
#define FIBM_INFO_VALID(sw) \
    (FIBM_INFO(sw) && GET_PLAT_STATE(sw)->fibmLock.handle)

/***************************************************
 * This structure holds FIBM statistics per switch
 **************************************************/
typedef struct _fm_fibmStats
{
    fm_uint32 read;
    fm_uint32 readMulti;
    fm_uint32 read64;
    fm_uint32 readMulti64;
    fm_uint32 readSg;    /* Scatter Gather */
    
    fm_uint32 write;
    fm_uint32 writeMulti;
    fm_uint32 write64;
    fm_uint32 writeMulti64;
    fm_uint32 writeSg;
    fm_uint32 mask;

    fm_uint32 intrRx;     /* number of interrupt message rx */

    fm_uint32 request;
    fm_uint32 response;
    fm_uint32 resendOnce;
    fm_uint32 resendTwiceOrMore;
    fm_uint32 sendFail;
    fm_uint32 unexpectedTag;
    fm_uint32 staleRxPkts; /* Stale packets, caused by resend */

    fm_uint32 sendDelay; /* For batched write without read */

} fm_fibmStats;


/***************************************************
 * This structure holds information on what is needed
 * to use FIBM per switch
 **************************************************/
typedef struct _fm_fibmSwInt
{
    /* Semaphore for holding the thread until response is received */
    fm_semaphore        fibmResponseSemaphore;

    /* Some configuration to setup fibm */
    fm_fibmSwitchConfig cfg;

    /* FIBM Tag for ack */
    fm_uint16           seqTag;

    /* For mode */
    fm_byte             mode;

    /* For flags */
    fm_byte             flags;

    /* For checking how many times a message has been sent */
    fm_uint64           sentCount;

    /* For checking message timeout */
    fm_byte             timeoutCount;

    /* Buffer to hold pending FIBM HW Commands */
    fm_buffer *         buffer;

    /* Buffer to hold sent FIBM HW Commands if needed to be resent
     * and don't allow buffer chaining
     */
    fm_buffer *         sentBuffer;

    /* To save the response buffer, if NULL when the request thread
     * is waked up, then request is timeout
     */
    fm_buffer *         response;

    /* To indicate there is no response */
    fm_bool             respTimeout;

    /* Expected response length. Used for determining when to stop
     * adding commands to the buffer, since we don't want the response
     * message to chain multiple buffer
     */
    fm_uint32           respLen;

    /* structure to hold fibm statistics */
    fm_fibmStats        stats;

    /* Use to set the batching flag to enabled when enableBatching is first
     * called, and then disabled it when the last disableBatching is called 
     */
    fm_uint16           enableBatchingCnt;

} fm_fibmSwInt;


/* macros to deal with the Fibm locks */
#define TAKE_FIBM_LOCK(sw)                                         \
    fmCaptureLock(&GET_PLAT_STATE(sw)->fibmLock, \
                  FM_WAIT_FOREVER);
#define DROP_FIBM_LOCK(sw) \
    fmReleaseLock(&GET_PLAT_STATE(sw)->fibmLock);

#define TAKE_FIBM_TXRX_LOCK(sw)                                         \
    fmCaptureLock(&GET_PLAT_STATE(sw)->fibmTxRxLock, \
                  FM_WAIT_FOREVER);
#define DROP_FIBM_TXRX_LOCK(sw) \
    fmReleaseLock(&GET_PLAT_STATE(sw)->fibmTxRxLock);

/* System init function */
fm_status fmFibmInit(void);


/* Initialization per switch */
fm_status fmFibmInitSwitch(fm_int sw);
fm_status fmFibmCleanupSwitch(fm_int sw);
fm_status fmFibmDeleteSwitch(fm_int sw);

/* Function called to handle mgmt Fibm packets */
fm_status fmFibmProcessPktHandler(fm_int sw, fm_buffer *buf, fm_uint32 *pIslTag);


/* Basic register read write functions */
fm_status fmFibmReadCSR(fm_int sw, fm_uint32 addr, fm_uint32 *value);
fm_status fmFibmWriteCSR(fm_int sw, fm_uint32 addr, fm_uint32 value);
fm_status fmFibmMaskCSR(fm_int sw, fm_uint addr, fm_uint32 mask, fm_bool on);
fm_status fmFibmReadCSRMult(fm_int     sw,
                            fm_uint32  addr,
                            fm_int     n,
                            fm_uint32 *value);
fm_status fmFibmWriteCSRMult(fm_int     sw,
                             fm_uint32  addr,
                             fm_int     n,
                             fm_uint32 *value);
fm_status fmFibmReadCSR64(fm_int sw, fm_uint32 addr, fm_uint64 *value);
fm_status fmFibmWriteCSR64(fm_int sw, fm_uint32 addr, fm_uint64 value);
fm_status fmFibmReadCSRMult64(fm_int     sw,
                              fm_uint32  addr,
                              fm_int     n,
                              fm_uint64 *value);
fm_status fmFibmWriteCSRMult64(fm_int     sw,
                               fm_uint32  addr,
                               fm_int     n,
                               fm_uint64 *value);
fm_status fmFibmReadScatterGather(fm_int sw, fm_int nEntries,
                                   fm_scatterGatherListEntry *sgList);
fm_status fmFibmWriteScatterGather(fm_int sw, fm_int nEntries,
                                   fm_scatterGatherListEntry *sgList);

fm_status fmFibmEnableBatching(fm_int sw, fm_bool enabled);

/* Fibm statistics */
fm_status fmFibmGetStats(fm_int sw, fm_fibmStats *stats);
fm_status fmFibmResetStats(fm_int sw);
fm_status fmFibmForceTimeout(fm_int sw);


#endif /* __FM_FM_PLATFORM_FIBM_H */
