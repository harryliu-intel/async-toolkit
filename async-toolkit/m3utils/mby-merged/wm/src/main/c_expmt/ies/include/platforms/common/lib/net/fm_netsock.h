/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_netsock.h
 * Creation Date:   October 4, 2010
 * Description:     Header file for simple unix UDP & TCP wrappers.
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

#ifndef __FM_FM_NETSOCK_H
#define __FM_FM_NETSOCK_H

typedef enum
{
    /* Indicates a socket of type SOCK_STREAM */
    FM_SOCKET_TYPE_TCP,

    /* Indicates a socket of type SOCK_DGRAM */
    FM_SOCKET_TYPE_UDP,

    /* Indicates a closed socket */
    FM_SOCKET_TYPE_CLOSED,

    /** For internal use only */
    FM_SOCKET_TYPE_MAX

} fm_socketType;

typedef struct _fm_socket
{
    /** socket descriptor, may be UDP or TCP */
    fm_int             sock;

    /** socket stream type */
    fm_socketType      type;

    /** the address structure associated with the socket */
    struct sockaddr_in address;

    /** the port a server socket is bound to */
    fm_int             serverPort;

} fm_socket;

typedef enum
{
    /** Indicates no event */
    FM_NETWORK_EVENT_NONE = 0,

    /** Indicates a new client connected */
    FM_NETWORK_EVENT_NEW_CLIENT,

    /** Indicates data available */
    FM_NETWORK_EVENT_DATA_AVAILABLE,

    /** For internal use only */
    FM_NETWORK_EVENT_MAX,

} fm_networkEvent;

fm_status fmCreateNetworkServer(fm_socket *   socketInfo,
                                fm_socketType type,
                                fm_int        port,
                                fm_int        backlog);

fm_status fmCreateNetworkClient(fm_socket *   socketInfo,
                                fm_socketType type,
                                fm_text       host,
                                fm_int        port);

/* Assumes numSockets > 1, index 0 is the server */
fm_status fmWaitForNetworkEvent(fm_socket **  sockets,
                                fm_int *      numSockets,
                                fm_int        maxSockets,
                                fm_int *      eventReceived,
                                fm_timestamp *timeout);

fm_status fmCloseNetworkConnection(fm_socket *client);

fm_status fmSendNetworkData(fm_socket *socketInfo,
                            void *     data,
                            fm_int     numBytes);

fm_status fmReceiveNetworkData(fm_socket *socketInfo,
                               void *     data,
                               fm_int     maxBytes,
                               fm_int *   numBytes);

#endif /* __FM_FM_NETSOCK_H */
