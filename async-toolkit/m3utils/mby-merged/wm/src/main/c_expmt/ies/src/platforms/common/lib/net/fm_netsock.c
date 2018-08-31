/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            platforms/common/lib/net/fm_netsock.c
 * Creation Date:   October 20, 2010
 * Description:     Wrapper library for basic sockets.
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

#include <fm_sdk_int.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <poll.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define FM_LOG_SYS_EXIT_ON_COND(cat, cond)                  \
    if ((cond)) {                                           \
        strErrNum = FM_STRERROR_S(strErrBuf,                \
                                  FM_STRERROR_BUF_SIZE,     \
                                  errno);                   \
        if (strErrNum == 0)                                 \
        {                                                   \
            FM_LOG_FATAL((cat),                             \
                         "System error %d: %s\n",           \
                         errno, strErrBuf);                 \
        }                                                   \
        else                                                \
        {                                                   \
            FM_LOG_FATAL((cat),                             \
                         "System error %d\n", errno);       \
        }                                                   \
        FM_LOG_EXIT_VERBOSE((cat), FM_FAIL);                \
    }

#if 0
#define VERBOSE
#endif

#define FM_GETHOSTBYNAME_BUF_SIZE 2048

/*****************************************************************************
 * Global Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

static const int SOCKET_ADDRLEN = sizeof(struct sockaddr_in);

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** fmCreateNetworkServer
 * \ingroup platformUtil
 *
 * \desc            Insert description here...
 *
 * \param[out]      socketInfo is
 *
 * \param[in]       type is
 *
 * \param[in]       port is
 *
 * \param[in]       backlog is
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
    char               strErrBuf[FM_STRERROR_BUF_SIZE];
    errno_t            strErrNum;

    FM_LOG_ENTRY_VERBOSE(FM_LOG_CAT_PLATFORM,
                         "socketInfo=%p, port=%d, backlog=%d\n",
                         (void *) socketInfo, 
                         port, 
                         backlog);

    if (!socketInfo)
    {
        FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    FM_CLEAR(*socketInfo);

    if (type == FM_SOCKET_TYPE_TCP)
    {
        socketInfo->sock = socket(AF_INET, SOCK_STREAM, 0);
        FM_LOG_SYS_EXIT_ON_COND(FM_LOG_CAT_PLATFORM, socketInfo->sock == -1);

        FM_CLEAR(socketInfo->address);

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

        FM_CLEAR(socketInfo->address);

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
/** fmCreateNetworkClient
 * \ingroup platformUtil
 *
 * \desc            Insert description here...
 *
 * \param[out]      socketInfo is
 *
 * \param[in]       type is
 *
 * \param[in]       host is
 *
 * \param[in]       port is
 *
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmCreateNetworkClient(fm_socket *socketInfo, 
                                fm_socketType type,
                                fm_text host, 
                                fm_int port)
{
    struct hostent  h;
    struct hostent *hp;
    fm_int          errResult;
    char            strErrBuf[FM_STRERROR_BUF_SIZE];
    errno_t         strErrNum;
    char            buf[FM_GETHOSTBYNAME_BUF_SIZE];
    int             herrno;

    FM_LOG_ENTRY_VERBOSE(FM_LOG_CAT_PLATFORM,
                         "socketInfo=%p, port=%d\n",
                         (void *) socketInfo, 
                         port);

    if (!socketInfo)
    {
        FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    FM_CLEAR(*socketInfo);

    if (type == FM_SOCKET_TYPE_TCP)
    {
        socketInfo->sock = socket(AF_INET, SOCK_STREAM, 0);
    }
    else if (type == FM_SOCKET_TYPE_UDP)
    {
        socketInfo->sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    }

    if (socketInfo->sock == -1)
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Unable to create socket type %d\n",
                     type);
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_FAIL);
    }

    FM_CLEAR(socketInfo->address);

    socketInfo->address.sin_family = AF_INET;

    if ((gethostbyname_r(host,
                         &h,
                         buf,
                         FM_GETHOSTBYNAME_BUF_SIZE,
                         &hp,
                         &herrno) != 0)
        || (hp == NULL))
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Unable to resolve hostname: %s\n", 
                     host);
    }

    FM_LOG_SYS_EXIT_ON_COND(FM_LOG_CAT_PLATFORM, !hp);

    FM_MEMCPY_S(&socketInfo->address.sin_addr.s_addr,
                sizeof(socketInfo->address.sin_addr.s_addr),
                hp->h_addr,
                hp->h_length);

    socketInfo->address.sin_port = htons(port);

    if (type == FM_SOCKET_TYPE_TCP)
    {
        errResult = connect(socketInfo->sock,
                            (struct sockaddr *) &socketInfo->address,
                            SOCKET_ADDRLEN);
        FM_LOG_SYS_EXIT_ON_COND(FM_LOG_CAT_PLATFORM, errResult != 0);
    }

    FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, FM_OK);

} /* end fmCreateNetworkClient */




/*****************************************************************************/
/** fmWaitForNetworkEvent
 * \ingroup platformUtil
 *
 * \desc            Waits for network events.
 *
 * \param[out]      sockets points to an array of active and inactive sockets.
 *                  This function can activate inactive sockets when a client
 *                  attempts to make a connection. The array must be maxSockets
 *                  in length.
 *
 * \param[in]       numSockets is the current number of active sockets; it must
 *                  be in the range [1..maxSockets).
 *
 * \param[in]       maxSockets is the maximum number of sockets this function
 *                  can activate.
 *
 * \param[out]      eventReceived points to an array where this function places
 *                  the network event type for each active socket. The array
 *                  must be maxSockets elements in length.
 *
 * \param[in]       timeout is the amount of time to wait for a network event.
 *                  Set to ''FM_WAIT_FOREVER'' to wait indefinitely.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmWaitForNetworkEvent(fm_socket **  sockets, 
                                fm_int *      numSockets,
                                fm_int        maxSockets,
                                fm_int *      eventReceived, 
                                fm_timestamp *timeout)
{
    fm_int        currNumSockets;
    fm_int        errResult;
    fm_int        i;
    fm_int        j;
    fm_int        slot;
    fm_bool       doIncrement;
    struct pollfd fds[FM_MAX_FDS_NUM] = { {0} };
    fm_int        fdsCnt;
    fm_int        fdsTimeout;
    socklen_t     addrLen = SOCKET_ADDRLEN;
    char          strErrBuf[FM_STRERROR_BUF_SIZE];
    errno_t       strErrNum;

    FM_LOG_ENTRY_VERBOSE(FM_LOG_CAT_PLATFORM,
                         "sockets=%p, numSockets=%p(%d), maxSockets=%d, "
                         "eventReceived=%p, timeout=%p\n",
                         (void *) sockets, 
                         (void *) numSockets, 
                         (numSockets ? *numSockets : -1),
                         maxSockets,
                         (void *) eventReceived, 
                         (void *) timeout);

    if ((!sockets || !eventReceived || !numSockets || (*numSockets == 0) || \
        (maxSockets > FM_MAX_FDS_NUM) || (*numSockets > FM_MAX_FDS_NUM)))
    {
        FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    currNumSockets = *numSockets;

    for ( i = 0, fdsCnt = 0 ; i < currNumSockets ; i++ )
    {
#ifdef VERBOSE
        FM_LOG_DEBUG2(FM_LOG_CAT_PLATFORM, "Watching descriptor %d\n", sockets[i]->sock);
#endif

        if (sockets[i]->type != FM_SOCKET_TYPE_CLOSED)
        {
            fds[fdsCnt].fd = sockets[i]->sock;
            fds[fdsCnt].events = POLLIN;
            fds[fdsCnt].revents = 0;
            fdsCnt++;
        }
    }

    if (timeout != FM_WAIT_FOREVER)
    {
        fdsTimeout = timeout->sec * 1000 + timeout->usec / 1000;
    }
    else
    {
        fdsTimeout = -1;
    }

    do
    {
        errResult = poll(fds, fdsCnt, fdsTimeout);
    }
    while ( ( errResult == -1 ) && ( errno == EINTR ) );
    FM_LOG_SYS_EXIT_ON_COND(FM_LOG_CAT_PLATFORM, errResult == -1);

    for ( i = 0, fdsCnt = 0 ; i < currNumSockets ; i++, fdsCnt++)
    {
        /* Default case */
        eventReceived[i] = FM_NETWORK_EVENT_NONE;

        if (sockets[i]->type != FM_SOCKET_TYPE_CLOSED && fds[fdsCnt].revents & POLLIN)
        {
            /* Handle TCP connection on server socket */
            if ((sockets[i]->type == FM_SOCKET_TYPE_TCP) &&
                (sockets[i]->serverPort > 0))
            {
                doIncrement = TRUE;
                slot = *numSockets;

                for ( j = 0 ; j < currNumSockets ; j++ )
                {
                    if (sockets[j]->type == FM_SOCKET_TYPE_CLOSED)
                    {
                        doIncrement = FALSE;
                        slot = j;
                        break;
                    }
                }

                if (slot >= maxSockets)
                {
                    FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                                 "Not enough socket slots left to accept client\n");
                    break;
                }

                FM_CLEAR(*sockets[slot]);

                sockets[slot]->sock =
                    accept(sockets[i]->sock,
                           (struct sockaddr *) &sockets[slot]->address,
                           &addrLen);
                FM_LOG_SYS_EXIT_ON_COND(FM_LOG_CAT_PLATFORM,
                                        sockets[slot]->sock == -1);

                sockets[slot]->type = FM_SOCKET_TYPE_TCP;

                eventReceived[i] = FM_NETWORK_EVENT_NEW_CLIENT;

#ifdef VERBOSE
                FM_LOG_DEBUG2(FM_LOG_CAT_PLATFORM,
                              "Handled new client connection\n");
#endif

                if (doIncrement)
                {
                    eventReceived[slot] = FM_NETWORK_EVENT_NONE;
                    (*numSockets)++;
                }

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




/*****************************************************************************/
/** fmCloseNetworkConnection
 * \ingroup platformUtil
 *
 * \desc            Insert description here...
 *
 * \param[out]      client is
 *
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmCloseNetworkConnection(fm_socket *client)
{
    FM_LOG_ENTRY_VERBOSE(FM_LOG_CAT_PLATFORM, "client=%p\n", (void *) client);

    if (!client)
    {
        FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    if (client->type == FM_SOCKET_TYPE_TCP)
    {
        close(client->sock);
    }

    FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, FM_OK);

} /* end fmCloseNetworkConnection */




/*****************************************************************************/
/** fmSendNetworkData
 * \ingroup platformUtil
 *
 * \desc            Insert description here...
 *
 * \param[out]      socketInfo is
 *
 * \param[out]      data is
 *
 * \param[in]       numBytes is
 *
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmSendNetworkData(fm_socket *socketInfo, void *data, fm_int numBytes)
{
    fm_int  errResult;
    fm_int  nb;
    char    strErrBuf[FM_STRERROR_BUF_SIZE];
    errno_t strErrNum;
    
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
/** fmReceiveNetworkData
 * \ingroup platformUtil
 *
 * \desc            Insert description here...
 *
 * \param[out]      socketInfo is
 *
 * \param[out]      data is
 *
 * \param[in]       maxBytes is
 *
 * \param[out]      numBytes is
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
    char               strErrBuf[FM_STRERROR_BUF_SIZE];
    errno_t            strErrNum;

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
