/* vim:et:sw=4:ts=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_model_packet_queue.h
 * Creation Date:   August 26, 2010
 * Description:     Data structures for sending/receiving model messages
 *
 * INTEL CONFIDENTIAL
 * Copyright 2010 - 2014 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_MODEL_PACKET_QUEUE_H
#define __FM_MODEL_PACKET_QUEUE_H

/* The maximum number of connected clients */
#define FM_MAX_MODEL_PACKET_QUEUE_CLIENTS   2

/* White model packet queue interface message queue size in bytes. */
#define FM_MODEL_PKTQ_MSG_QUEUE_SIZE                                           \
    (FM_MODEL_MSG_SIZE * FM_MODEL_PKTQ_MAX_PACKETS)

/* The maximum number of unit in a topology */
#define FM_MAX_MODEL_TOPOLOGY_UNIT          10

/* The maximum number of internal connection between unit in a single topology
 */
#define FM_MAX_MODEL_TOPOLOGY_INTERLINK     500

#define FM_MODEL_PKTQ_GET_HEAD(mpq, imsg, msgLength)                           \
{                                                                              \
    *(msgLength) = *( (fm_int32 *) &(mpq)->msgQueue[(mpq)->msgQueueHead] );    \
    *(imsg) = (fm_modelMessage *) &(mpq)->msgQueue[ (mpq)->msgQueueHead +      \
                                                    sizeof(fm_int32) ];        \
}

#define FM_MODEL_PKTQ_UPDATE_HEAD(mpq, msgLength)                              \
{                                                                              \
    fm_int newHead = (mpq)->msgQueueHead + sizeof(fm_int32) + (msgLength);     \
    (mpq)->msgQueueHead = newHead < (fm_int) FM_MODEL_PKTQ_MSG_QUEUE_SIZE ?    \
                          newHead :                                            \
                          0;                                                   \
}

#define FM_MODEL_PKTQ_GET_TAIL(mpq, emsg)                                      \
{                                                                              \
    *(emsg) = (fm_modelMessage *) &(mpq)->msgQueue[ (mpq)->msgQueueTail +      \
                                                    sizeof(fm_int32) ];        \
}

#define FM_MODEL_PKTQ_UPDATE_TAIL(mpq, msgLength)                              \
{                                                                              \
    fm_int newTail = (mpq)->msgQueueTail + sizeof(fm_int32) + (msgLength);     \
    *( (fm_int32 *) &(mpq)->msgQueue[(mpq)->msgQueueTail] ) = (msgLength);     \
    (mpq)->msgQueueTail = newTail < (fm_int) FM_MODEL_PKTQ_MSG_QUEUE_SIZE ?    \
                          newTail :                                            \
                          0;                                                   \
}

/****************************************************************************/
/** \ingroup intTypeStruct
 *
 * Defines the per-switch sockets and stats counters for the packet queue.
 ****************************************************************************/
typedef struct _fm_modelPacketQueueSwitch
{
    /* egress destination sockets */
    fm_socket *            destSocket;

    /* stats on packets sent and received */
    fm_uint32 *            packetReceived;
    fm_uint32 *            packetSent;
    fm_uint32 *            packetDropped;

} fm_modelPacketQueueSwitch;



/****************************************************************************/
/** \ingroup typeStruct
 * 
 *  Defines pointers to platform-specific functions.
 *
 *  Used as an argument to ''fmModelPacketQueueInitialize''.
 ****************************************************************************/
typedef struct _fm_modelPacketQueueServices
{
    /** Apply all ingress processing steps to the packet received on the
     *  specified port. This function returns a status word and takes as
     *  arguments:
     *                                                                  \lb\lb
     *  - fm_int sw - the switch on which to operate. The switch number must
     *                already have been validated.
     *                                                                  \lb\lb
     *  - fm_int port - the physical port on which the packet has been received.
     *                                                                  \lb\lb
     *  - fm_byte packet - points to the ingress packet data.
     *                                                                  \lb\lb
     *  - fm_int length - the ingress packet length in units of bytes.
     *                                                                  \lb\lb
     *  - fm_modelSidebandData *sbData - points to the sideband data.
     */
    fm_status (*SendPacket)(fm_int                sw,
                            fm_int                port,
                            fm_byte *             packet,
                            fm_int                length,
                            fm_modelSidebandData *sbData);

    /** Apply all egress processing steps to a previously processed ingress
     *  packet. This function returns a status word (''FM_OK'' if
     *  successful, ''FM_ERR_NO_MORE'' if no more egress are available,
     *  other ''Status Codes'' otherwise) and takes as arguments:
     *                                                                  \lb\lb
     *  - fm_int sw - the switch on which to operate. The switch number must
     *                already have been validated.
     *                                                                  \lb\lb
     *  - fm_int *port - points to caller-allocated storage where this function
     *                   places the phsyical egress port.
     *                                                                  \lb\lb
     *  - fm_byte *packet - points to caller-allocated storage where this
     *                      function places the egress packet data.
     *                                                                  \lb\lb
     *  - fm_int *length - points to caller-allocated storage where this
     *                     function places the length of the egress packet.
     *                                                                  \lb\lb
     *  - fm_int maxPktSize - the maximum egress packet size in units of bytes.
     *                                                                  \lb\lb
     *  - fm_modelSidebandData *sbData - points to caller-allocated storage
     *                                   where this function places the sideband
     *                                   data.
     */
    fm_status (*ReceivePacket)(fm_int                sw,
                               fm_int *              port,
                               fm_byte *             packet,
                               fm_int *              length,
                               fm_int                maxPktSize,
                               fm_modelSidebandData *sbData); 

    /** Check whether the received packet is a BPDU packet, and if so,
     *  whether the BPDU packet should be trapped to the CPU or silently
     *  dropped. This function returns a status word and takes as arguments:
     *                                                                  \lb\lb
     *  - fm_int sw - the switch on which to operate. The switch number must
     *                already have been validated.
     *                                                                  \lb\lb
     *  - fm_eventPktRecv *pktEvent - points to the ''fm_eventPktRecv''
     *                                structure describing the received packet.
     *                                                                  \lb\lb
     *  - fm_int vlan - the VLAN number on which the packet has been received.
     *                                                                  \lb\lb
     *  - fm_bool *dropBpdu - points to caller-allocated storage where this
     *                        function places the drop decision.
     */
    fm_status (*CheckBpduDropping)(fm_int           sw,
                                   fm_eventPktRecv *pktEvent,
                                   fm_int           vlan,
                                   fm_bool *        dropBpdu);

    /** Return the maximum frame size supported by the CPU port in units of
     *  bytes. This function returns a status word and takes as arguments:
     *                                                                  \lb\lb
     *  - fm_int sw - the switch on which to operate. The switch number must
     *                already have been validated.
     *                                                                  \lb\lb
     *  - fm_uint32 *maxFrameSize - points to caller-allocated storage where
     *                              this function places the maximum frame size
     *                              supported by the CPU port.
     */
    fm_status (*GetCpuMaxFrameSize)(fm_int sw, fm_uint32 *maxFrameSize);

    /** Return the VLAN tagging mode for the CPU port. This function returns
     *  a status word and takes as arguments:
     *                                                                  \lb\lb
     *  - fm_int sw - the switch on which to operate.
     *                                                                  \lb\lb
     *  - fm_int vlan - the VLAN number whose tagging mode is to be returned.
     *                                                                  \lb\lb
     *  - fm_bool *tag - points to caller-allocated storage where this function
     *                   places the VLAN tagging mode for the CPU port.
     */
    fm_status (*GetCpuVlanTag)(fm_int sw, fm_int vlan, fm_bool *tag);

    /** Return the number of ports supported by the switch model. This
     *  function returns a status word and takes as arguments:
     *                                                                  \lb\lb
     *  - fm_int sw - the switch on which to operate. The switch number must
     *                already have been validated.
     *                                                                  \lb\lb
     *  - fm_int *numPorts - points to caller-allocated storage where this
     *                       function places the number of supported ports.
     */
    fm_status (*GetNumPorts)(fm_int sw, fm_int *numPorts);

} fm_modelPacketQueueServices;



/****************************************************************************/
/** \ingroup intTypeStruct
 *
 * Defines the common packet queue used by one or more models.
 ****************************************************************************/

typedef struct _fm_modelPacketQueue
{
    /* sockets to use to send messages */
    fm_socket                   serverSocket;

    /* for local delivery to the server */
    fm_socket                   localSocket;
    
    /* per-switch state */
    fm_modelPacketQueueSwitch  *queueState[FM_MAX_NUM_FOCALPOINTS];
    
    /* Bypass the API for packets destined to the CPU port */
    fm_bool                     alternativeRxDataPathEnable;

    /* Indicates if the white model packet queue interface should send a
     * FM_MODEL_MSG_PACKET_EOT message to its listeners after each received
     * FM_MODEL_MSG_PACKET message has been fully processed. */
    fm_bool                     sendEOT;
        
    /* thread handle for queue handler task */
    fm_thread                   queueProcessorHandle;

    /* thread handle for inter queue handler task */
    fm_thread                   interQueueProcessorHandle;

    /* message queue */
    fm_byte                     msgQueue[ FM_MODEL_PKTQ_MSG_QUEUE_SIZE + 
                                          sizeof(fm_int32) +
                                          FM_MODEL_MSG_SIZE ];
    fm_int                      msgQueueHead;
    fm_int                      msgQueueTail;
    fm_int                      msgQueueOutputLimit;

    /* Structure holding the function ptrs to platform specific functions */
    fm_modelPacketQueueServices pktQueueServices;

} fm_modelPacketQueue;

/****************************************************************************/
/** White Model Packet Queue Interface Attributes
 *  \ingroup constModelPktQAttr
 *
 *  Used as an argument to ''fmModelPacketQueueSetAttribute''.
 *                                                                      \lb\lb
 *  For each attribute, the data type of the corresponding attribute value is
 *  indicated.
 ****************************************************************************/
enum _fm_modelPacketQueueAttr
{
    /** Type fm_uint32: The maximum number of packets in the white model
     *  packet queue that will be processed at a time before checking the
     *  server socket. Defaults to 32. */
    FM_MODEL_PACKET_QUEUE_EGRESS_LIMIT,

    /** Type fm_text: The name of the file containing the topology
     *  specification for a multi-chip system. See ''Multiple Model 
     *  Environments'' in the Software API User Guide for more information. */
    FM_MODEL_PACKET_QUEUE_TOPOLOGY,

    /** Type fm_bool: Whether the white model packet queue should send a
     * FM_MODEL_MSG_PACKET_EOT message to its listeners after each received
     * FM_MODEL_MSG_PACKET has been fully processed. */
    FM_MODEL_PACKET_QUEUE_SEND_EOT,

    /* ----- Add new attributes above this line ----- */
    
    /** For internal use only. */
    FM_MODEL_PACKET_QUEUE_ATTR_MAX,

};

/****************************************************************************/
/** \ingroup intTypeStruct
 * 
 * Define the destination of an inter queue socket 
 ****************************************************************************/
typedef struct _fm_modelPacketInterQueue
{
    /* egress destination sockets */
    fm_socket *            destSocket;

    /* egress destination switch */
    fm_int                 destSwitch;

    /* egress destination port */
    fm_int                 destPort;

} fm_modelPacketInterQueue;




fm_status fmModelPacketQueueInitialize(void **statePtr,
                                       fm_modelPacketQueueServices *funcInfo);

fm_status fmModelPacketQueueAddSwitch(void *statePtr, 
                                      fm_int sw, 
                                      fm_int numPorts);

fm_status fmModelPacketQueueSetAttribute(fm_uint32 attr, void *value);

fm_status fmModelPacketQueueSendSwitched(fm_int sw, fm_buffer *buffer);

fm_status fmModelPacketQueueSendDirected(fm_int           sw,
                                         fm_int *         portList,
                                         fm_int           numPorts,
                                         fm_buffer *      buffer,
                                         fm_packetInfoV2 *info);

fm_status fmModelPacketQueueSendISL(fm_int          sw,
                                    fm_islTagFormat islTagFormat,
                                    fm_uint32 *     islTag,
                                    fm_buffer *     buffer);

void fmModelPrintServerStats(void);


#endif /* __FM_MODEL_PACKET_QUEUE_H */

