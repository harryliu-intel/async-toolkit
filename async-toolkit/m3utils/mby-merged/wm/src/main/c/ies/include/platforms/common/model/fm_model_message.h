/* vim:et:sw=4:ts=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_model_message.h
 * Creation Date:   August 26, 2010
 * Description:     Data structures for sending/receiving model messages
 *
 * INTEL CONFIDENTIAL
 * Copyright 2009 - 2014 Intel Corporation. All Rights Reserved.
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

#ifndef __FM_MODEL_MESSAGE_H
#define __FM_MODEL_MESSAGE_H

#define FM_MODEL_MSG_VERSION            2

/* Use this UDP port number when passing a model message of type
 * FM_MODEL_MSG_SET_EGRESS_INFO to disable the socket on that port.
 * Can be used to disable the model from generating socket frames
 * on that port. */
#define FM_MODEL_SOCKET_PORT_DISABLE    0xFFFF

/****************************************************************************/
/** \ingroup typeStruct
 *
 *  Defines the format for sideband data.
 ****************************************************************************/
typedef struct _fm_modelSidebandData
{
    /** The packet identifier. */
    fm_uint32 idTag;

    /** The egress traffic class. */
    fm_byte tc;

    /* Packet Meta Data, passed from packet send
       and is copied over to modelState before
       the packet is processed in the pipeline.
       Might be moved or removed.
     */
    fm_byte pktMeta[32];

} fm_modelSidebandData;


/****************************************************************************/
/** \ingroup typeStruct
 *
 *  Defines the message format for the ''White Model Packet Queue Interface''.
 ****************************************************************************/
typedef struct _fm_modelMessage
{
    /* -- 16/32-bit members must be aligned to a 2/4-byte boundary -- */

    /** Length of the entire message in bytes, including the header */
    fm_int32 msgLength;

    /** The ''fm_modelMessage'' version. */
    fm_uint16 version;

    /** Message type (see ''fm_modelMsgType''). */
    fm_uint16 type;

    /** Target model switch number for this message. */
    fm_uint16 sw;

    /** Target model switch port for this message. */
    fm_uint16 port;

    /** The message payload. */
    fm_byte data[FM_MODEL_MAX_TLV_SIZE + FM_MODEL_MAX_PACKET_SIZE];

    /*  -- Add new members above this line -- */

    /** The on the wire size of ''fm_modelMessage'' independent of compiler
     * and/or machine alignment requirements. To be used in conjunction with the
     * offsetof() macro. For internal use only. */
    fm_byte __size;

} fm_modelMessage;


typedef struct _fm_modelDataTlv
{
    fm_byte type;

    fm_int32 length;

    fm_byte *value;

} fm_modelDataTlv;


/****************************************************************************/
/*  White Model Packet Queue Interface Message Types */
/** \ingroup typeEnum
 *
 *  The set of possible message type values for ''fm_modelMessage'' when
 *  sending a message to the white model packet queue interface.
 ****************************************************************************/
typedef enum
{
    /** A standard Ethernet frame to be switched through the model. */
    FM_MODEL_MSG_PACKET = 0,

    /** Message to force the link state of a port to either up or down
     *  as if a cable had been connected or disconnected. */
    FM_MODEL_MSG_LINK_STATE,

    /** Message to force the state of the switch to either up or down. */
    FM_MODEL_MSG_SWITCH_STATE,

    /** This message type is used by the application test bench to register
     *  a socket interface with a model switch port. Packets egressing the
     *  switch port will be sent on the socket with the indicated TCP port
     *  number and host name. The first two octets of payload are
     *  the TCP port number and the remainder of the payload (up to 510
     *  octets) is the host name.
     *                                                                  \lb\lb
     *  Set the TCP port number to FM_MODEL_SOCKET_PORT_DISABLE to disable
     *  a previously registered socket interface for the indicated switch
     *  port. */
    FM_MODEL_MSG_SET_EGRESS_INFO,

    /** This message type is used by the application test bench to register
     *  a TCP port number to which a CPU-bound data frame should be sent
     *  instead of to the API. The first two octets of payload are the TCP
     *  port number and the remainder of the payload (up to 510 octets)
     *  is the host name. */
    FM_MODEL_MSG_ENABLE_ALTERNATIVE_DATA_PATH,

    /** A standard Ethernet frame to be looped back into the model. This should
     *  not be used by application generated messages. */
    FM_MODEL_MSG_PACKET_LOOPBACK,

    /** Message to indicate that a received FM_MODEL_MSG_PACKET message has
     *  been fully processed and no more FM_MODEL_MSG_PACKET messages will be
     *  generated in response to it. The 2-octet payload contains the
     *  transmission size.
     *                                                                  \lb\lb
     *  This message type should not be used by the application test bench. */
    FM_MODEL_MSG_PACKET_EOT,

    /** Message to read or write a single 32-bit or 64-bit register word.
     *  The 9-octet or 13-octet payload consists of a 1-octet management
     *  message type field (see ''fm_modelMgmtType'') followed by a 4-octet
     *  hardware address and a 4-octet or 8-octet value. */
    FM_MODEL_MSG_MGMT,

    /** Message to get or set an API attribute. The 518-octet payload consists
     *  of a 1-octet attribute message type field (see ''fm_modelAttrType'')
     *  followed by a 1-octet API attribute (see ''fm_apiAttrType''), a
     *  2-octet key length, a 256-octet NUL-terminated key, a 2-octet value
     *  length and a 256-octet NUL-terminated (if appropriate) value. */
    FM_MODEL_MSG_ATTR,

    /** Message to get model information. The 54-octet payload consists of a
     *  1-octet information message type field (see ''fm_modelInfoType'')
     *  followed by a 2-octet field containing the number of ports supported
     *  by the switch model and 51 octets of padding. */
    FM_MODEL_MSG_GET_INFO,

    /** Error message. 1 byte for message type followed by detailed error,
      * message string. This message is sent back when some request can not
      * be processed */
    FM_MODEL_MSG_ERROR,

    /** Encapsulated IOSF message */
    FM_MODEL_MSG_IOSF,

    /** Control message */
    FM_MODEL_MSG_CTRL,

    /** Version info message */
    FM_MODEL_MSG_VERSION_INFO,

    /** NVM read message */
    FM_MODEL_MSG_NVM_READ

} fm_modelMsgType;


/****************************************************************************/
/* White Model Message Attribute Types */
/** \ingroup typeEnum
 *
 * The set of possible attribute message type values for ''fm_modelMessage''
 * when sending a ''FM_MODEL_MSG_ATTR'' message to the white model packet queue
 * interface.
 ****************************************************************************/
typedef enum
{
    /** Request to get the value of an API attribute. */
    FM_MODEL_ATTR_GET_REQUEST = 1,

    /** Response to a get request. This attribute type should not be used by
     * messages created by the application test bench. */
    FM_MODEL_ATTR_GET_RESPONSE,

    /** Request to set an API attribute. */
    FM_MODEL_ATTR_SET,

    /** Acknowledgement for a set request. This attribute type should not be
     * used by messages created by the application test bench. */
    FM_MODEL_ATTR_SET_ACK

} fm_modelAttrType;


/****************************************************************************/
/* White Model Management Message Types */
/** \ingroup typeEnum
 *
 *  The set of possible management message type values for ''fm_modelMessage''
 *  when sending a ''FM_MODEL_MSG_MGMT'' message to the white model packet queue
 *  interface.
 ****************************************************************************/
typedef enum
{
    /** Request to read a single 32-bit register word. */
    FM_MODEL_MGMT_READ_REQUEST = 1,

    /** Response to a read request. This management type should not be used by
     *  messages created by the application test bench. */
    FM_MODEL_MGMT_READ_RESPONSE,

    /** Request to write a single 32-bit register word. */
    FM_MODEL_MGMT_WRITE,

    /** Acknowledgement of a write request. This management type should not be
     * used by messages created by the application test bench. */
    FM_MODEL_MGMT_WRITE_ACK,

    /** Request to read a single 64-bit register word. */
    FM_MODEL_MGMT_READ64_REQUEST,

    /** Response to a read64 request. This management type should not be used by
     *  messages created by the application test bench. */
    FM_MODEL_MGMT_READ64_RESPONSE,

    /** Request to write a single 64-bit register word. */
    FM_MODEL_MGMT_WRITE64,

    /** Acknowledgement of a write64 request. This management type should not be
     * used by messages created by the application test bench. */
    FM_MODEL_MGMT_WRITE64_ACK

} fm_modelMgmtType;


/****************************************************************************/
/* White Model Information Message Types */
/** \ingroup typeEnum
 *
 *  The set of possible information message type values for ''fm_modelMessage''
 *  when sending a ''FM_MODEL_MSG_GET_INFO'' message to the white model packet
 *  queue interface.
 ****************************************************************************/
typedef enum
{
    /** Request to get model information. */
    FM_MODEL_INFO_REQUEST = 1,

    /** Response to a model information request. This information type should
     * not be used by messages created by the application test bench. */
    FM_MODEL_INFO_RESPONSE

} fm_modelInfoType;


/****************************************************************************/
/* White Model Data Types */
/** \ingroup typeEnum
 *
 * The set of possible types for the type-length-value (TLV) elements stored in
 * the ''fm_modelMessage'' message payload.
 ****************************************************************************/
typedef enum
{
    /** Ethernet frame. */
    FM_MODEL_DATA_PACKET = 0xA0, /* Non-zero field so it is easier to locate */

    /** 32-bit packet identifier. */
    FM_MODEL_DATA_SB_ID,

    /** 8-bit egress traffic class. */
    FM_MODEL_DATA_SB_TC,

    /** Packet Meta. */
    FM_MODEL_PACKET_META,

} fm_modelDataType;


/****************************************************************************/
/* White Model Control Message Types */
/** \ingroup typeEnum
 *
 *  The set of possible information message type values for ''fm_modelMessage''
 *  when sending a ''FM_MODEL_MSG_CTRL'' message to the white model packet
 *  queue interface.
 ****************************************************************************/
typedef enum
{
    /** Request to reset chip. */
    FM_MODEL_CTRL_CHIP_RESET_REQ = 1,

    /** Response to a chip reset request. This information type should
     * not be used by messages created by the application test bench. */
    FM_MODEL_CTRL_CHIP_RESET_REP

} fm_modelCtrlType;


#define FM_MODEL_MSG_SIZE               (offsetof(fm_modelMessage, __size))

#define FM_MODEL_MSG_HEADER_SIZE        (offsetof(fm_modelMessage, data))

#define FM_MODEL_MSG_LENGTH_SIZE                                               \
    (sizeof( ( (fm_modelMessage *) 0 )->msgLength ))

#define FM_MODEL_MSG_VERSION_SIZE                                              \
    (sizeof( ( (fm_modelMessage *) 0 )->version ))

#define FM_MODEL_MSG_DATA_SIZE                                                 \
    (sizeof( ( (fm_modelMessage *) 0 )->data ))

#define FM_MODEL_DATA_TYPE_SIZE                                                \
    (sizeof( ( (fm_modelDataTlv *) 0 )->type ))

#define FM_MODEL_DATA_LENGTH_SIZE                                              \
    (sizeof( ( (fm_modelDataTlv *) 0 )->length ))

#define FM_MODEL_MSG_TLV_SIZE                                                  \
    (FM_MODEL_DATA_TYPE_SIZE + FM_MODEL_DATA_LENGTH_SIZE)

/* The maximum length of an attribute key including the terminating NUL
 * character. */
#define FM_MODEL_ATTR_MAX_KEY_SIZE      256

#endif /* __FM_MODEL_MESSAGE_H */

