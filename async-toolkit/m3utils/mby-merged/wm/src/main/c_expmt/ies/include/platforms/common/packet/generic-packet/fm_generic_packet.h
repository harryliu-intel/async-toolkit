/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_generic_packet.h
 * Creation Date:   Jan 5, 2009
 * Description:     Header file for generic packet transfer
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2012 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM_GENERIC_PACKET_H
#define __FM_FM_GENERIC_PACKET_H

/* Packet Transfer Definnitions */
#define FM_MAX_FDS_NUM                            1024
#define FM_FDS_POLL_TIMEOUT_USEC                  1000

/* holds a packet entry */
typedef struct _fm_packetEntry
{
    /* holds information about where the packet is going */
    fm_packetInfo info;

    /* holds the data for the packet */
    fm_buffer *   packet;

    /* the total length of the packet */
    fm_uint       length;

    /* FM6000 only. The FCS value to be sent with the packet. */
    fm_uint32     fcsVal;

    /* ISL Ftag */
    fm_islTag     islTag;

    /* if need to suppress vlantag */
    fm_bool       suppressVlanTag;

    /* This is filled in by the API when useEgressRules is set to true. */
    fm_uint32     egressVlanTag;

    /* For FM4000 use only.  This is used to indicate whether or not to free
     *  the packet buffer. For direct sending to a single port this will be
     *  set to TRUE. For direct sending to an entire vlan, the last packet
     *  entry enqueued at sendPacketQueue, corresponding to the last port
     *  in the target vlan, will have this field set to TRUE. */
    fm_bool       freePacketBuffer;

} fm_packetEntry;


typedef struct _fm_packetQueue
{
    /* holds the current send queue of packets in a circular buffer */
    fm_packetEntry packetQueueList[FM_PACKET_QUEUE_SIZE];

    /**************************************************
     * The packetQueue is a rotary queue of packets
     * intended to be transmitted.  We maintain an
     * index to the next position to push into,
     * and an index to the next position to pull
     * from.
     *
     * The queue is empty when the push index is equal
     * to the pull index (push == pull => empty).
     *
     * The queue is full when incrementing the push
     * index would make it equal to the pull index
     * (push + 1 % QueueSize == pull => full).
     **************************************************/
    fm_uint        pushIndex;
    fm_uint        pullIndex;

    fm_int         switchNum;

    pthread_mutex_t mutex;

} fm_packetQueue;


/* manages packet sending state */
typedef struct
{
    /**************************************************
     * Packet sending state.
     **************************************************/
    fm_packetQueue txQueue;

    /* To signal receive thread to continue when events
     * are available again
     */
    fm_semaphore   eventsAvailableSignal;

    /**************************************************
     * This control whether packets are directly
     * enqueued to the application, bypassing
     * sending to event thread.
     * Note: no support for FM2000 yet
     **************************************************/
    fm_bool        rxDirectEnqueueing;

    /**************************************************
     * This maintains an offset into current packet,
     * with -2 and -1 referring to the first and second
     * pre-data header words.
     * Note: only used for FM2000
     **************************************************/
    fm_int         sendOffset;

    /* This maintains an offset into current buffer. */
    fm_int         sendBufferOffset;

    /**************************************************
     * This maintains a pointer to the current active
     * buffer for the current packet. NULL if none.
     **************************************************/
    fm_buffer *    currentSendBuffer;

    /**************************************************
     * Whether we should include the user-supplied FCS 
     * when sending a packet.
     **************************************************/
    fm_bool        sendUserFcs;

    /**************************************************
     * Packet receive state.
     **************************************************/

    /* This maintains an offset into current buffer. */
    fm_int         recvBufferOffset;

    /**************************************************
     * This maintains a pointer to the current active
     * buffer for the current packet.
     **************************************************/
    fm_buffer *    currentRecvBuffer;

    /* Pointer to first buffer in receive buffer chain. */
    fm_buffer *    recvChainHead;

    /* Pointer to the last buffer in receive buffer chain. */
    fm_buffer *    recvChainTail;

    /* Whether a recv is in progress right now */
    fm_bool        recvInProgress;

    /**************************************************
     * This maintains a pointer to the current active
     * event pointer for the current packet.
     * Note: only for FM2000
     **************************************************/
    fm_event *     currentRecvEvent;

    /* Keeps track of the number of words read out of the fifo */
    fm_int         currentWordsReceived;

    /*********************************************************
     * the software cache of the LCI_CFG.endinaness so we don't
     * have to read LCI_CFG more than once after its written to
     **********************************************************/
    fm_int         cachedEndianness;

} fm_packetHandlingState;

fm_status fmPacketQueueInit(fm_packetQueue *queue, fm_int sw);
fm_status fmPacketQueueFree(fm_int sw);
void      fmPacketQueueLock(fm_packetQueue *queue);
void      fmPacketQueueUnlock(fm_packetQueue *queue);
fm_status fmPacketQueueUpdate(fm_packetQueue *queue);

fm_status fmPacketQueueEnqueue(fm_packetQueue * queue,
                               fm_buffer *      packet,
                               fm_int           packetLength,
                               fm_islTag *      islTag,
                               fm_bool          suppressVlanTag,
                               fm_bool          freeBuffer);

fm_status fmPacketReceiveEnqueue(fm_int sw, fm_event *event,
                                 fm_switchEventHandler selfTestEventHandler);

fm_int    fmComputeTotalPacketLength(fm_buffer *packet);
fm_uint32 fmPacketGetCRC(fm_buffer *buffer);
void fmPacketClearCRC(fm_buffer *buffer);
fm_status fmFindSlaveSwitchPortByGlort(fm_uint32 glort, 
                                       fm_int *switchNum, 
                                       fm_int *port);
fm_int fmGetCpuPortInt(fm_int sw, fm_int *cpuPort);
fm_status fmGetPortDefVlanInt(fm_int     sw,
                              fm_int     port,
                              fm_uint16 *vlan);
fm_status fmGetPortDefVlanDefPriorityInt(fm_int     sw,
                                         fm_int     port,
                                         fm_uint16 *vlan,
                                         fm_byte *  priority);
fm_status fmGetPortMaxFrameSizeInt(fm_int  sw,
                                   fm_int  port,
                                   fm_int *maxSize);

fm_status fmGenericPacketHandlingInitialize(fm_int sw);
fm_status fmGenericPacketHandlingInitializeV2(fm_int sw, fm_bool hasFcs);

fm_status fmGeneratePacketIsl(fm_int          sw,
                              fm_buffer      *buffer,
                              fm_packetInfo  *info,
                              fm_packetEntry *entry,
                              fm_int          cpuPort,
                              fm_uint32       stagTypeA,
                              fm_uint32       stagTypeB,
                              fm_uint32       switchPriority,
                              fm_uint32       trapGlort,
                              fm_bool         suppressVlanTagAllowed);

fm_status fmGenericSendPacketISL(fm_int     sw,
                                 fm_islTag *islTagList,
                                 fm_int     numPorts,
                                 fm_buffer *packet);

fm_status fmGenericSendPacketDirected(fm_int     sw,
                                      fm_int *   portList,
                                      fm_int     numPorts,
                                      fm_buffer *packet,
                                      fm_uint32  fcsValue,
                                      fm_int     cpuPort,
                                      fm_uint32  stagTypeA,
                                      fm_uint32  stagTypeB,
                                      fm_uint32  switchPriority,
                                      fm_uint32  trapGlort,
                                      fm_bool    suppressVlanTagAllowed);

fm_status fmGenericSendPacketSwitched(fm_int sw, fm_buffer *packet,
                                      fm_int     cpuPort,
                                      fm_uint32  stagTypeA,
                                      fm_uint32  stagTypeB,
                                      fm_uint32  switchPriority,
                                      fm_uint32  trapGlort,
                                      fm_bool    suppressVlanTagAllowed);

fm_status fmGenericSendPacket(fm_int         sw,
                              fm_packetInfo *info,
                              fm_buffer *    packet,
                              fm_int         cpuPort,
                              fm_uint32      stagTypeA,
                              fm_uint32      stagTypeB,
                              fm_uint32      switchPriority,
                              fm_uint32      trapGlort,
                              fm_bool        suppressVlanTagAllowed);

#endif /* __FM_FM_GENERIC_PACKET_H */
