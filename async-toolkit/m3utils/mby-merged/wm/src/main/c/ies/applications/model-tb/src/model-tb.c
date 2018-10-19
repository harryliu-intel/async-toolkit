/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            module-tb.c
 * Creation Date:   2009
 * Description:     Conduit for taking frames to/from a platform and
 *                  passing them through a white model via sockets
 *
 * INTEL CONFIDENTIAL
 * Copyright 2008 - 2012 Intel Corporation. All Rights Reserved. 
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

#define FM_MODEL_MAX_PACKET_SIZE            15864

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <sys/types.h>
#include <net/if.h>
#include <sys/ioctl.h>
#include <net/if_arp.h>

#include <getopt.h>
#include <limits.h>
#include <inttypes.h>
#include <netdb.h>

#include "fm_sdk_fm4000_int.h"
#include <platforms/common/model/fm_model_message.h>
#include <platforms/common/lib/net/fm_netsock.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define VERSION                   0x1
#define MAX_PKT_SIZE              9000
#define DEFAULT_MODEL_IP          "localhost"
#define DEFAULT_MODEL_PORT        15667
#define DEFAULT_APPLICATION_PORT  15668
#define IFRNAME                   "eth0"

#define MAX_CONNECTIONS             160

static const int SOCKET_ADDRLEN = sizeof(struct sockaddr_in);

#define FM_LOG_SYS_EXIT_ON_COND(cat, cond)                  \
    if ((cond)) {                                           \
        FM_LOG_FATAL((cat),                                 \
                     "System error %d: %s\n",               \
                     errno, strerror(errno));               \
        FM_LOG_EXIT_VERBOSE((cat), FM_FAIL);                \
    }

/*****************************************************************************
 * Global Variables
 *****************************************************************************/

fm_semaphore              seqSem;

char                      modelIP[256];
fm_int                    numPorts;
char                      fm_ip_addr[256];
fm_uint32                 sendModelPort;
fm_uint32                 recvModelPort;

fm_socket                 modelSocket;

static fm_int             verbose = 0;
fm_socket *               tcpSockets[MAX_CONNECTIONS + 1];

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

fm_thread     ListenerHandle;

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

static void usage(void);


/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** fmCreateNetworkServer
 *
 * \desc            Create a server socket other application will connet to
 *                  to send information.
 *
 * \param[in]       socketInfo points to the strucrture containing informatin
 *                  on the socket.
 *
 * \param[in]       type defines if the requested socket is of type TCP
 *                  (FM_SOCKET_TYPE_TCP) or UDP (FM_SOCKET_TYPE_UDP).
 *
 * \param[in]       port contains the local UDP or TCP port associated with
 *                  the socket.
 *
 * \param[in]       backlog is not use at this point.
 *
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/

fm_status fmCreateNetworkServer(fm_socket *socketInfo, 
                                fm_socketType type,
                                fm_int port, 
                                fm_int backlog)
{
    fm_status          status = FM_OK;
    fm_int             errResult;
    struct sockaddr_in addrInfo;
    socklen_t          addrLen = sizeof(addrInfo);

    FM_LOG_ENTRY_VERBOSE(FM_LOG_CAT_PLATFORM,
                         "socketInfo=%p, port=%d, backlog=%d\n",
                         (void *) socketInfo, 
                         port, 
                         backlog);

    if (!socketInfo)
    {
        FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    memset(socketInfo, 0, sizeof(fm_socket));

    if (type == FM_SOCKET_TYPE_TCP)
    {
        socketInfo->sock = socket(AF_INET, SOCK_STREAM, 0);
        FM_LOG_SYS_EXIT_ON_COND(FM_LOG_CAT_PLATFORM, socketInfo->sock == -1);

        memset(&socketInfo->address, 0, sizeof(struct sockaddr_in));

        socketInfo->address.sin_family      = AF_INET;
        socketInfo->address.sin_addr.s_addr = htonl(INADDR_ANY);
        socketInfo->address.sin_port        = htons(port);

        errResult = bind(socketInfo->sock, 
                         (struct sockaddr *) &socketInfo->address,  
                         SOCKET_ADDRLEN);
        FM_LOG_SYS_EXIT_ON_COND(FM_LOG_CAT_PLATFORM, errResult == -1);

        errResult = listen(socketInfo->sock, 3);
        FM_LOG_SYS_EXIT_ON_COND(FM_LOG_CAT_PLATFORM, errResult == -1);

#ifdef VERBOSE
        FM_LOG_DEBUG2(FM_LOG_CAT_PLATFORM,
                      "Socket descriptor %d initialized\n",
                      socketInfo->sock);
#endif

    }
    else if (type == FM_SOCKET_TYPE_UDP)
    {
        socketInfo->sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
        FM_LOG_SYS_EXIT_ON_COND(FM_LOG_CAT_PLATFORM, socketInfo->sock == -1);

        memset(&socketInfo->address, 0, sizeof(struct sockaddr_in));

        socketInfo->address.sin_addr.s_addr = htonl(INADDR_ANY);
        socketInfo->address.sin_port        = htons(port);

        errResult = bind(socketInfo->sock, 
                         (struct sockaddr *) &socketInfo->address,  
                         SOCKET_ADDRLEN);
        FM_LOG_SYS_EXIT_ON_COND(FM_LOG_CAT_PLATFORM, errResult == -1);

    }
    else
    {
        FM_LOG_ASSERT(FM_LOG_CAT_PLATFORM,
                      FALSE,
                      "Unexpected socket type %d\n", type);
    }

    errResult = getsockname(socketInfo->sock,
                            (struct sockaddr *) &addrInfo,
                            &addrLen);
    FM_LOG_SYS_EXIT_ON_COND(FM_LOG_CAT_PLATFORM, errResult == -1);

    socketInfo->serverPort = ntohs(addrInfo.sin_port);

    FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, status);

} /* end fmCreateNetworkServer */


/*****************************************************************************/
/** fmReceiveNetworkData
 *
 * \desc            Read the data received on a socket.
 *
 * \param[in]       socketInfo points to the strucrture containing informatin
 *                  on the socket.
 *
 * \param[in]       data point to the memory area where received data will be
 *                  saved.
 *
 * \param[in]       maxBytes containg the maximum number of bytes the caller
 *                  is ready to receive.
 *
 * \param[out]      numBytes containd the number of bytes read from the socket.
 *
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/

fm_status fmReceiveNetworkData(fm_socket *socketInfo, 
                               void *data,
                               fm_int maxBytes, 
                               fm_int *numBytes)
{
    fm_byte *          ptr;
    struct sockaddr_in addrInfo;
    socklen_t          addrLen;
    fm_int             nb;

    FM_LOG_ENTRY_VERBOSE(FM_LOG_CAT_PLATFORM,
                         "socketInfo=%p, data=%p, maxBytes=%d, numBytes=%p\n",
                         (void *) socketInfo, 
                         (void *) data, 
                         maxBytes, 
                         (void *) numBytes);

    if (!socketInfo || !data || !numBytes)
    {
        FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    if (socketInfo->type == FM_SOCKET_TYPE_UDP)
    {
        *numBytes = recvfrom(socketInfo->sock,
                             data,
                             maxBytes,
                             0,
                             (struct sockaddr *) &addrInfo,
                             &addrLen);
    }
    else if (socketInfo->type == FM_SOCKET_TYPE_TCP)
    {
        nb        = 0;
        ptr       = data;
        *numBytes = 0;

        do
        {
            nb = recv(socketInfo->sock, ptr, maxBytes - *numBytes, 0);
            FM_LOG_SYS_EXIT_ON_COND(FM_LOG_CAT_PLATFORM, nb == -1);

            /* Closed connection */
            if (nb == 0)
            {
                FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, FM_ERR_NO_MORE);
            }

            ptr       += nb;
            *numBytes += nb;

        } while (*numBytes < maxBytes);
    }


    FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, FM_OK);

} /* end fmReceiveNetworkData */


/*****************************************************************************/
/** fmSendNetworkData
 *
 * \desc            Send data through a socket.
 *
 * \param[in]       socketInfo points to the strucrture containing informatin
 *                  on the socket.
 *
 * \param[in]       data point to the memory area where the bytes to be sent
 *                  are located.
 *
 * \param[in]       numBytes contains the number of bytes to be sent through the
 *                  socket.
 *
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/

fm_status fmSendNetworkData(fm_socket *socketInfo, 
                            void *data, 
                            fm_int numBytes)
{
    fm_int errResult;
    fm_int nb;
    
    FM_LOG_ENTRY_VERBOSE(FM_LOG_CAT_PLATFORM,
                         "socketInfo=%p, data=%p, numBytes=%d\n",
                         (void *) socketInfo, 
                         (void *) data, 
                         numBytes);

    if (!socketInfo || !data)
    {
        FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    if (socketInfo->type == FM_SOCKET_TYPE_UDP)
    {
        errResult = sendto(socketInfo->sock,
                           data,
                           numBytes,
                           0,
                           (struct sockaddr *) &socketInfo->address,
                           SOCKET_ADDRLEN);
        FM_LOG_SYS_EXIT_ON_COND(FM_LOG_CAT_PLATFORM, errResult != 0);
    }
    else if (socketInfo->type == FM_SOCKET_TYPE_TCP)
    {
        nb = 0;

        do
        {
            nb = send(socketInfo->sock, data, numBytes, 0);
            FM_LOG_SYS_EXIT_ON_COND(FM_LOG_CAT_PLATFORM, nb == -1);

            if (nb > 0)
            {
                data = (((fm_byte *) data) + nb);
                numBytes -= nb;
            }
        } while ((nb > 0) && numBytes);

    }

    FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, FM_OK);

} /* end fmSendNetworkData */


/*****************************************************************************/
/** fmWaitForNetworkEvent
 *
 * \desc            Listen on server sockets for either a connection of data
 *                  being received.
 *
 * \param[in]       sockets points to the array of pointer to socket structures
 *                  use to hold information on each socket i.e. the server
 *                  sockets and the ones created when a remote connects to the
 *                  server. The sise of the array is define by maxSockets.
 *
 * \param[in]       maxSockets defines the maximum number of socket description
 *                  structures the function has to use.
 *
 * \param[in]       eventReceived points to an array of size maxSockets use to
 *                  flag which socket received an event.
 * 
 * \param[in]       timeout defines the time the function will wait to receive
 *                  an event on one of the socket.
 * 
 *                  
 * \return          FM_OK if successful.
 *
 *****************************************************************************/

fm_status fmWaitForNetworkEvent(fm_socket **sockets, 
                                fm_int *numSockets,
                                fm_int maxSockets,
                                fm_int *eventReceived, 
                                fm_timestamp *timeout)
{
    fd_set         fds;
    fm_int         maxDescriptor;
    fm_int         i;
    fm_int         errResult;
    struct timeval ts;
    socklen_t      addrLen = SOCKET_ADDRLEN;
    fm_int         currNumSockets = *numSockets;

    FM_LOG_ENTRY_VERBOSE(FM_LOG_CAT_PLATFORM,
                         "sockets=%p, numSockets=%p(%d), maxSockets=%d, "
                         "eventReceived=%p, timeout=%p\n",
                         (void *) sockets, 
                         (void *) numSockets, 
                         (numSockets ? *numSockets : -1),
                         maxSockets,
                         (void *) eventReceived, 
                         (void *) timeout);

    if (!sockets || !eventReceived || !numSockets || (*numSockets == 0))
    {
        FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    FD_ZERO(&fds);

    for ( i = 0, maxDescriptor = 0 ; i < currNumSockets ; i++ )
    {
#ifdef VERBOSE
        FM_LOG_DEBUG2(FM_LOG_CAT_PLATFORM, "Watching descriptor %d\n", sockets[i]->sock);
#endif

        if (sockets[i]->sock > maxDescriptor)
        {
            maxDescriptor = sockets[i]->sock;
        }

        if (sockets[i]->type != FM_SOCKET_TYPE_CLOSED)
        {
            FD_SET(sockets[i]->sock, &fds);
        }
    }

    memset(&ts, 0, sizeof(struct timeval));
    ts.tv_sec  = (int) timeout->sec;
    ts.tv_usec = (int) timeout->usec;

    errResult = select(maxDescriptor + 1, &fds, NULL, NULL, &ts);
    FM_LOG_SYS_EXIT_ON_COND(FM_LOG_CAT_PLATFORM, errResult == -1);

    for ( i = 0 ; i < currNumSockets ; i++ )
    {
        /* Default case */
        eventReceived[i] = FM_NETWORK_EVENT_NONE;

        if (FD_ISSET(sockets[i]->sock, &fds))
        {
            /* Handle TCP connection on server socket */
            if ((sockets[i]->type == FM_SOCKET_TYPE_TCP) &&
                (sockets[i]->serverPort > 0))
            {
                if (*numSockets >= maxSockets)
                {
                    FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                                 "Not enough socket slots left to accept client\n");
                    break;
                }

                memset(sockets[*numSockets], 0, sizeof(fm_socket));

                sockets[*numSockets]->sock 
                    = accept(sockets[i]->sock,
                             (struct sockaddr *) &sockets[*numSockets]->address,
                             &addrLen);
                FM_LOG_SYS_EXIT_ON_COND(FM_LOG_CAT_PLATFORM, sockets[*numSockets]->sock == -1);

                sockets[*numSockets]->type = FM_SOCKET_TYPE_TCP;

                eventReceived[i] = FM_NETWORK_EVENT_NEW_CLIENT;

#ifdef VERBOSE
                FM_LOG_DEBUG2(FM_LOG_CAT_PLATFORM,
                              "Handled new client connection\n");
#endif

                (*numSockets)++;

                break;
            }
            else
            {
                eventReceived[i] = FM_NETWORK_EVENT_DATA_AVAILABLE;

#ifdef VERBOSE
                FM_LOG_DEBUG2(FM_LOG_CAT_PLATFORM,
                              "Data available on client #%d\n", i - 1);
#endif
            }
        }
    }

    FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, FM_OK);

} /* end fmWaitForNetworkEvent */



static void fm_get_addrs(char *iname)
{
    int          sockfd;
    struct ifreq ifr;
    fm_uint32    ip;

    if ( 0 > ( sockfd = socket(AF_INET, SOCK_DGRAM, IPPROTO_IP) ) )
    {
        return;
    }

    strncpy( ifr.ifr_name, iname, sizeof(ifr.ifr_name) );

    if ( 0 == ioctl(sockfd, SIOCGIFADDR, &ifr) )
    {
        ip =
            ( (struct sockaddr_in *) &(ifr.ifr_addr) )->sin_addr.s_addr;
        sprintf(fm_ip_addr, "%d.%d.%d.%d",
                (ip >> 24) & 0xff,
                (ip >> 16) & 0xff,
                (ip >> 8) & 0xff,
                (ip >> 0) & 0xff);
    }

    close(sockfd);
} /* fm_get_addrs */




/*
 * Configure the real switch to trap all received frames to the CPU
 * so that they can be sent to the white model
 *
 * Need a trigger for regular frames and dropped frames if the Bali
 * would not normally accept the frame without some sort of configuration
 **/
void trapAll(int sw)
{
    fm_int                myTrigger;
    fm_triggerRequestInfo info;

    info.requestRateLimiter = FALSE;

    fmAllocateTrigger(sw, &myTrigger, &info);
    /* Trap on all ports */
    fmWriteUINT32(sw, FM4000_TRIGGER_CONDITION_RX(myTrigger), 0xFFFFFFFE);
    /* Trap frames going to any port */
    fmWriteUINT32(sw, FM4000_TRIGGER_CONDITION_TX(myTrigger), 0xFFFFFFFF);
    /* Trap all types */
    fmWriteUINT32(sw, FM4000_TRIGGER_CONDITION_AMASK_1(myTrigger),
                  0xFFFFFFFF);
    fmWriteUINT32(sw, FM4000_TRIGGER_CONDITION_AMASK_2(myTrigger),
                  0xFFFFFFFF);
    /* Take Action = Trap */
    fmWriteUINT32(sw, FM4000_TRIGGER_ACTION_CFG_1(myTrigger), 0x4);

    fmAllocateTrigger(sw, &myTrigger, &info);

    /* Trap on all ports */
    fmWriteUINT32(sw, FM4000_TRIGGER_CONDITION_RX(myTrigger), 0xFFFFFFFE);
    /* Trap dropped frames */
    fmWriteUINT32(sw, FM4000_TRIGGER_CONDITION_TX(myTrigger), 0x0);
    /* Trap on dropped frames and "undrop" them */
    fmWriteUINT32(sw, FM4000_TRIGGER_CONDITION_PARAM(myTrigger),
                  0x9fc00000);
    /* Trap all types */
    fmWriteUINT32(sw, FM4000_TRIGGER_CONDITION_AMASK_1(myTrigger),
                  0xFFFFFFFF);
    fmWriteUINT32(sw, FM4000_TRIGGER_CONDITION_AMASK_2(myTrigger),
                  0xFFFFFFFF);
    /* Take Action = Trap */
    fmWriteUINT32(sw, FM4000_TRIGGER_ACTION_CFG_1(myTrigger), 0x4);
} /* trapAll */




void parse(char *arg, fm_uint64 *val)
{
    if (arg == NULL)
    {
        printf("Null Arguemnt\n");
        exit(-1);
    }

    *val = (fm_uint64) strtoll(arg, (char **) NULL, 0);

} /* parse */




/*****************************************************************************/
/** retryingAllocBuffer
 *
 * \desc            Tries to allocate a buffer and retries if it fails up to
 *                  some limit.
 *
 * \param[in]       sw is the switch number to allocate buffers on
 *
 * \return          The allocated buffer pointer or NULL on failure
 *
 *****************************************************************************/
fm_buffer *retryingAllocBuffer(fm_int sw)
{
    int        nr = 0;
    fm_buffer *ptr;

    for (nr = 0 ; nr < 32 ; nr++)
    {
        ptr = fmAllocateBuffer(sw);

        if (ptr)
        {
            return ptr;
        }
        else
        {
            /* wait 0.5s before trying again */
            fmDelay(0, 500000000);
        }
    }

    return NULL;
}  /* retryingAllocBuffer */




static fm_int             packetCount = 0;
/*****************************************************************************/
/** processPacketReceiveEvent
 *
 * \desc            Processes a receive event, including classification and
 *                  calling of handlers.  The default fall-through case is
 *                  the user accessible receive packet cache.
 *
 * \param[in]       sw is the switch number on which the packet event occurred.
 *
 * \param[in]       pkt is packet receive event info structure.
 *
 *****************************************************************************/
void processPacketReceiveEvent(int sw, fm_eventPktRecv *event)
{
    socklen_t       fromLength = sizeof(struct sockaddr_in);
    fm_modelMessage packet;
    fm_int          i, c, offset, nbBytesCopied;
    fm_buffer *     buffer     = (fm_buffer *) event->pkt;
    fm_buffer *     pkt        = buffer;
    fm_int32        sourcePort = event->srcPort;
    fm_byte         *ptrData;
    fm_int numBytes;
    fm_int nb;
    fm_int  p;
    fm_byte *framePtr;

    packetCount++;

    if (verbose)
    {
        printf("Packet #%d received on source port %d pkt->len = %d\n",
               packetCount, sourcePort, pkt->len);
    }

    packet.type = htons(FM_MODEL_MSG_PACKET);
    packet.sw   = htons(sw);
    packet.port = htons(sourcePort);

    c = 0;
    nbBytesCopied = 0;
    offset = 0;
    while (pkt)
    {
        for (i = 0; nbBytesCopied < pkt->len; i++, c += 4, nbBytesCopied += 4, offset++)
        {
            if (verbose)
            {
                printf("%02x %02x %02x %02x ",
                       (pkt->data[i] >> 24) & 0xff,
                       (pkt->data[i] >> 16) & 0xff,
                       (pkt->data[i] >> 8 ) & 0xff,
                       (pkt->data[i] >> 0 ) & 0xff);

                if ( (offset > 0) && ( (offset % 5) == 4 ) )
                {
                    printf("\n");
                }
            }

            packet.data[c + 0] = (pkt->data[i] >> 24) & 0xff;
            packet.data[c + 1] = (pkt->data[i] >> 16) & 0xff;
            packet.data[c + 2] = (pkt->data[i] >> 8 ) & 0xff;
            packet.data[c + 3] = (pkt->data[i] >> 0 ) & 0xff;
        }

        pkt = pkt->next;
    }

    if (verbose)
    {
        printf("\n");
    }

    if (modelSocket.sock < 0)
    {
        printf("ERROR UDP Socket Not Initialized\n");
    }
    else
    {
        numBytes = buffer->len + FM_MODEL_MSG_HEADER_SIZE;
        packet.msgLength = htonl(numBytes);
        fmSendNetworkData(&modelSocket, &packet, numBytes);
    }

    fmFreeBufferChain(sw, buffer);
} /* processPacketReceiveEvent */




static void sendEgressInfo(fm_int port, fm_uint16 udpport)
{
    socklen_t       fromLength = sizeof(struct sockaddr_in);
    fm_modelMessage packet;
    fm_int          msgLength;
    fm_int          l;
    fm_byte         *ptrData;
    fm_int numBytes;
    fm_int nb;

    packet.type = htons(FM_MODEL_MSG_SET_EGRESS_INFO);
    packet.sw   = 0;
    packet.port = htons(port);
    *( (fm_uint16 *) &packet.data ) = htons(tcpSockets[port-1]->serverPort);
    strcpy( (fm_char *) packet.data + 2, fm_ip_addr );
    packet.data[2 + strlen(fm_ip_addr)] = '\0';
    msgLength = strlen(fm_ip_addr) + 3 + FM_MODEL_MSG_HEADER_SIZE;
    packet.msgLength = htonl(msgLength);

    nb = 0;
    ptrData = (fm_byte *) &packet;
    numBytes = msgLength;

    fmSendNetworkData(&modelSocket, &packet, msgLength);
    if (verbose)
    {
        printf("Requesting to receive frames for port %d on %s:%d\n",
               port, modelIP, tcpSockets[port-1]->serverPort);
    }
} /* sendEgressInfo */




void fmAppEventHandler(fm_int event, fm_int sw, void *arg)
{
    fm_eventPort *portEvent;

    switch (event)
    {
        case FM_EVENT_SWITCH_INSERTED:
            printf("Switch #%d inserted!\n", sw);
            fmSignalSemaphore(&seqSem);
            break;

        case FM_EVENT_PKT_RECV:
            processPacketReceiveEvent(sw, (fm_eventPktRecv *) arg);
            break;

        case FM_EVENT_PORT:
            portEvent = (fm_eventPort *) arg;

            switch (portEvent->linkStatus)
            {
                case FM_PORT_STATUS_LINK_UP:
                    printf("Port %d UP\n", portEvent->port);
                    sendEgressInfo(portEvent->port, recvModelPort);
                    break;

                case FM_PORT_STATUS_LINK_DOWN:
                    printf("Port %d DOWN\n", portEvent->port);
                    sendEgressInfo(portEvent->port, FM_MODEL_SOCKET_PORT_DISABLE);
                    break;

                default:
                    printf("Autoneg Event\n");
            } /* switch */

            break;

        default:
            printf("ERROR Unregistered event, may be slowly things down.\n");
    } /* switch */
} /* fmAppEventHandler */




static void forwardPacket(fm_uint32 sw, fm_uint32 port,
                          fm_uint32 length, fm_modelMessage packet)
{
    fm_int32   portList[1];
    fm_buffer *pkt  = NULL;
    fm_buffer *ptr  = NULL;
    fm_uint32  bytesDone = 0;
    fm_uint32  chunkLen;
    fm_int     result;
    fm_status  err;
    fm_int     mode;
    fm_int     state;
    fm_uint32  p;
    fm_int     laneInfo[4] =
    {
        0, 0, 0, 0
    };

    portList[0] = port;

    if ( ( err = fmGetPortState(sw, port, &mode, &state, laneInfo) )
        == FM_OK )
    {
        if (state != FM_PORT_STATE_UP)
        {
            if (verbose)
            {
                printf("Port %d is not up, not sending\n", port);
            }

            return;
        }
    }

    while (bytesDone < length)
    {
        if (pkt)
        {
            ptr->next = retryingAllocBuffer(sw);
            ptr       = ptr->next;
        }
        else
        {
            pkt = ptr = retryingAllocBuffer(sw);
        }

        if (!ptr)
        {
            printf("ERROR Unable to allocate buffers to send packet!\n");

            if (pkt)
            {
                fmFreeBufferChain(sw, pkt);
            }

            return;
        }

        chunkLen = ( (length - bytesDone) > FM_BUFFER_SIZE_BYTES ) ?
                   FM_BUFFER_SIZE_BYTES :
                   (length - bytesDone);

        memcpy(ptr->data, &packet.data[bytesDone], chunkLen);

        ptr->len   = chunkLen;
        ptr->next  = NULL;
        bytesDone += chunkLen;
    }

    if (verbose)
    {
        printf("Packet forwarded to port %d length = %d\n",
               port, length);

        for (p = 0 ; p < length ; p++)
        {
            FM_LOG_PRINT("%02x ", packet.data[p]);
    
            if ( p && ( (p % 16) == 15 ) )
            {
                FM_LOG_PRINT("\n");
            }
        }
        FM_LOG_PRINT("\n");
    }

    result = fmSendPacketDirected(sw, portList, 1, pkt);
} /* forwardPacket */




static void *waitForMessages(void *args)
{
    fm_uint32          sw = 0;
    fm_thread *        thread;
    int                sockfd;
    int                error, flags = 0;
    fm_uint32          senderLen = 0;
    struct sockaddr_in recvAddr;
    fm_int             swi;
    fm_int             port;
    fm_int             type;
    fm_int             length;
    fm_int             i;
    fm_modelMessage    packet;
    fm_int             errResult;
    fm_int             numSockets;
    fm_uint16          msgLength;
    fm_int             slot;
    fm_int             recvLength;
    fm_timestamp       timeout;
    fm_int             eventsReceived[MAX_CONNECTIONS + 1];
    fm_uint32 p;
    fm_byte msgLengthMn[2];

    thread = FM_GET_THREAD_HANDLE(args);

    /**************************************************
     * Open local client socket to send messages to API
     **************************************************/

    /* Create a socket */
    modelSocket.sock = socket(AF_INET, SOCK_STREAM, 0);

    if (modelSocket.sock < 0)
    {
        printf("pkt_module: can't open client socket, error = %d\n", modelSocket.sock);
    }
    /* Get host address */
    fm_get_addrs(IFRNAME);

    /* Complete socket adress structure ready for usage */
    bzero( &modelSocket.address, sizeof(struct sockaddr_in) );
    modelSocket.address.sin_family      = AF_INET;
    modelSocket.address.sin_addr.s_addr = inet_addr(modelIP);
    modelSocket.address.sin_port        = htons(sendModelPort);

    errResult = connect(modelSocket.sock,
                        (struct sockaddr *) &modelSocket.address,
                        SOCKET_ADDRLEN);

    if (errResult != 0)
    {
        fprintf(stderr, "Could not connect to server err = %d\n", errResult);
    }


    /**************************************************
     * Open a socket to receive messages from API.
     **************************************************/

    numSockets = 0;
    for (port = 1 ; port < numPorts ; port++)
    {
        packet.type = htons(FM_MODEL_MSG_SET_EGRESS_INFO);
        packet.sw   = htons(sw);
        packet.port = htons(port);

        tcpSockets[numSockets] = malloc(sizeof(fm_socket));

        fmCreateNetworkServer(tcpSockets[numSockets],
                              FM_SOCKET_TYPE_TCP,
                              0,
                              3);

        /* socket server network port */
        *( (fm_uint16 *) &packet.data ) = htons(tcpSockets[numSockets]->serverPort);

        /* assume TP is running on the same host */
        strcpy( (fm_char *) packet.data + 2, "localhost" );

        msgLength = strlen("localhost") + 2 + FM_MODEL_MSG_HEADER_SIZE;
        packet.msgLength = htonl(msgLength);

        numSockets++;
    }

    for ( slot = numSockets ; slot <= MAX_CONNECTIONS ; slot++ )
    {
        tcpSockets[slot] = malloc(sizeof(fm_socket));
        memset(tcpSockets[slot], 0, sizeof(fm_socket));
        tcpSockets[slot]->sock = -1;
    }

    for ( ; ; )
    {
        timeout.sec  = 0;
        timeout.usec = 1;

        memset(eventsReceived, 0, sizeof(eventsReceived));

        fmWaitForNetworkEvent(tcpSockets, 
                              &numSockets, 
                              MAX_CONNECTIONS + 1,
                              eventsReceived, 
                              &timeout);

        for ( slot = 0 ; slot < numSockets ; slot++ )
        {
            if (eventsReceived[slot] == FM_NETWORK_EVENT_DATA_AVAILABLE)
            {
                /* Receive length first */

                fmReceiveNetworkData(tcpSockets[slot],
                                     &packet.msgLength,
                                     FM_MODEL_MSG_LENGTH_SIZE,
                                     &recvLength);

                /* Closed socket */
                if (recvLength == 0)
                {
                    tcpSockets[slot]->type = FM_SOCKET_TYPE_CLOSED;
                    continue;
                }

                msgLength = ntohl(packet.msgLength);

                /* Now receive the data */
                fmReceiveNetworkData(tcpSockets[slot],
                                     ((fm_byte *) &packet) + FM_MODEL_MSG_LENGTH_SIZE,
                                     packet.msgLength - FM_MODEL_MSG_LENGTH_SIZE,
                                     &recvLength);

                FM_LOG_ASSERT(FM_LOG_CAT_PLATFORM,
                              recvLength == ( (fm_int) (msgLength - FM_MODEL_MSG_LENGTH_SIZE) ),
                              "Mismatch in packet length %d - 2, versus receive length %d\n",
                              msgLength,
                              recvLength);

                /* Read packet info */
                type      = ntohs(packet.type);
                sw        = ntohs(packet.sw);
                port      = ntohs(packet.port);
                msgLength -= FM_MODEL_MSG_HEADER_SIZE;

                /* Skip for any type but PACKET */
                if (type != FM_MODEL_MSG_PACKET)
                {
                    continue;
                }

                if ( (sw == 0) && (port > 0) && (port <= 24) )
                {
                    forwardPacket(sw, port, msgLength, packet);
                }
            }
        }
    }

    close(sockfd);

    fmExitThread(thread);

    return NULL;

} /* waitForMessages */




void cleanup(const char *src, int err)
{
    printf( "ERROR: %s: %s\n", src, fmErrorMsg(err) );
    fmTerminate();
    exit(1);

}   /* end cleanup */




void switchInitialize(fm_int sw, fm_int numports)
{
    fm_int result;
    fm_int i;
    fm_int max_frame_size = 10240;

    fmCreateVlan(sw, 1);

    /* bring up all ports, add them to default VLAN */
    for (i = 1 ; i < numports ; i++)
    {
        if ( ( result = fmSetPortState(sw, i, FM_PORT_STATE_UP, 0) ) != FM_OK )
        {
            cleanup("fmSetPortState", result);
        }

        if ( ( result = fmAddVlanPort(sw, 1, i, FALSE) ) != FM_OK )
        {
            cleanup("fmAddVlanPort", result);
        }

        if ( ( result = fmSetVlanPortState(sw,
                                           1,
                                           i,
                                           FM_STP_STATE_FORWARDING) ) != FM_OK )
        {
            cleanup("fmSetVlanPortState", result);
        }

        if ( ( result = fmSetPortAttribute(sw,
                                           i,
                                           FM_PORT_MAX_FRAME_SIZE,
                                           &max_frame_size) ) != FM_OK )
        {
            cleanup("fmSetPortAttribute", result);
        }

    }

    if ( ( result = fmSetPortAttribute(sw,
                                       0,
                                       FM_PORT_MAX_FRAME_SIZE,
                                       &max_frame_size) ) != FM_OK )
    {
        cleanup("fmSetPortAttribute", result);
    }

    trapAll(sw);

} /* switchInitialize */




/*****************************************************************************
 * External Functions
 *****************************************************************************/


/*****************************************************************************
 * Public Functions
 *****************************************************************************/

void app_cleanup(void)
{
    fmTerminate();
} /* app_cleanup */




/*****************************************************************************/
/* main
 * \ingroup
 *
 * \desc            Entry point for program
 *
 * \param[in]       argc is the number of command-line arguments
 *
 * \param[in]       argv points to an array of command-line argument strings
 *
 * \return          0 for success
 *
 *****************************************************************************/
int main(int argc, char *argv[])
{
    int           c;
    fm_uint64     parseInt;
    fm_int        sw = 0;
    fm_status     err;

    fm_int        result;
    fm_timestamp  wait =
    {
        3, 0
    };
    fm_switchInfo swinfo;

    sendModelPort = DEFAULT_MODEL_PORT;
    recvModelPort = DEFAULT_APPLICATION_PORT;

    bzero(modelIP, 256);

    while (1)
    {
        static struct option long_options[] =
        {
            { "verbose",    no_argument,       &verbose, 1   },
            { "model-ip",   required_argument, 0,        'm' },
            { "model-port", required_argument, 0,        'p' },
            { "port",       required_argument, 0,        'q' },
            { 0,            0,                 0,        0   }
        };
        /* getopt_long stores the option index here. */
        int                  option_index = 0;

        c = getopt_long_only(argc, argv, "smintv",
                             long_options, &option_index);

        /* Detect the end of the options. */
        if (c == -1)
        {
            break;
        }

        switch (c)
        {
            case 0:
                break;

            case 'm':
                strcpy(modelIP, optarg);
                printf("Model IP set to %s\n", modelIP);
                break;

            case 'p':
                parse(optarg, &parseInt);
                sendModelPort = (fm_int) parseInt;
                printf("Model Port set to %d\n", sendModelPort);
                break;

            case 'q':
                parse(optarg, &parseInt);
                recvModelPort = (fm_int) parseInt;
                printf("Application Port set to %d\n", recvModelPort);
                break;

            default:
                usage();
        }  /* switch */
    }

    printf("verbose = %d\n", verbose);

    fmOSInitialize();

    /* create the sequencing semaphore to wait until switch is inserted */
    fmCreateSemaphore("seq", FM_SEM_BINARY, &seqSem, 0);

    /* initialize globals */
    if ( ( err = fmInitialize(fmAppEventHandler) ) != FM_OK )
    {
        fprintf(stderr, "%s: Could not initialize SDK\n", argv[0]);
        exit(1);
    }

    /* wait for switch inserted */
    fmWaitSemaphore(&seqSem, &wait);

    /* bring up switch #0 */
    if ( ( result = fmSetSwitchState(sw, TRUE) ) != FM_OK )
    {
        cleanup("fmSetSwitchState", result);
    }

    printf("Getting switch %d info\n", sw);
    result = fmGetSwitchInfo(sw, &swinfo);

    if (result != FM_OK)
    {
        printf("Could not access switch ERR(%d)\n",
               result);
        fmTerminate();
        exit(1);
    }

    numPorts = swinfo.numPorts;

    switchInitialize(sw, swinfo.numPorts);
    printf("Switch is UP, all ports are now enabled\n");

    if ( atexit(app_cleanup) )
    {
        fprintf(stderr, "%s: Could not set exit handler\n", argv[0]);
        fmTerminate();
        exit(1);
    }
    fmSetLoggingType(FM_LOG_TYPE_CONSOLE, 0, NULL);

    fmSetProcessEventMask(FM_EVENT_PORT | FM_EVENT_PKT_RECV);
    fmSetEventHandler(fmAppEventHandler);

    /* create thread to listen to sockets */
    err = fmCreateThread("simple_net_listener",
                         0,
                         &waitForMessages,
                         0,
                         &ListenerHandle);
    if (err != FM_OK)
    {
        printf("SimpleTestbench: error creating thread (err=%d)\n", err);
    }

    for ( ; ; )
    {
        fm_char c;

        fmYield();

        c = getchar();

        switch (c)
        {
            case 's':
                fmDbgDumpStatChanges(0,1); 
                break;

            case 'q':
                return 0;
                break;
        }
    }

}   /* end main */




static void usage(void)
{
    (void) fprintf(stderr, "%s\n%s\n%s%s%s\n%s%d%s\n%s%d%s\n",
                   "usage: ",
                   "   pkt_module       --verbose           Enable LOTS of Output     (optional)",
                   "                    --model-ip          IP address of model (def: ",
                   DEFAULT_MODEL_IP, ")",
                   "                    --model-port        TCP port of model   (def: ",
                   DEFAULT_MODEL_PORT, ")",
                   "                    --port              TCP port of app     (def: ",

                   DEFAULT_APPLICATION_PORT, ")"
                  );
    exit(1);
} /* usage */
