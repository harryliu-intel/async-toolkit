
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <netdb.h>
#include <poll.h>
#include <unistd.h>
#include <linux/stat.h>
#include <fcntl.h>

#include "mby_srv_socket.h"
#include "mby_srv_log.h"
#include "mby_srv_utils.h"
#include "mby_srv_errno.h"


#define FM_GETHOSTBYNAME_BUF_SIZE           2048
#define FM_STRERROR_BUF_SIZE 128
static const int SOCKET_ADDRLEN = sizeof(struct sockaddr_in);

#define FM_MAX_FDS_NUM                      1024

/*****************************************************************************/
/** InitializeSocketInfoFile
 * \ingroup intModel
 *
 * \desc            Creates an empty socket info file.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmInitializeSocketInfoFile(void)
{
    FILE *  fd;
    fm_text filePath;
    fm_char currentPath[] = ".";
    fm_char filename[512];
    fm_int  modelPos;
    char    strErrBuf[FM_STRERROR_BUF_SIZE];

    // FM_LOG_ENTRY_NOARGS(FM_LOG_CAT_PLATFORM);

    filePath = getenv("WMODEL_INFO_PATH");

    if (!filePath)
    {
        filePath = currentPath;
    }

    snprintf(filename, sizeof(filename), "%s/models.packetServer", filePath);

	// TODO what is this?
    // modelPos = fmGetIntApiAttribute(FM_AAK_API_PLATFORM_MODEL_POSITION,
    //                                 FM_AAD_API_PLATFORM_MODEL_POSITION);
	modelPos = 0;

    /* Reset the models.packetServer info file when first position is
     * initialize. */
    if (modelPos == 0)
    {
        if ( ( fd = fopen(filename, "wt") ) != NULL )
        {
            fclose(fd);
        }
        else
        {
            char *e = strerror_r(errno, strErrBuf, FM_STRERROR_BUF_SIZE);
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                         "Can't create server info file %s: %s\n",
                         filename, strErrBuf );
#if 0
            if (strErrNum == 0)
            {
                FM_LOG_ERROR( FM_LOG_CAT_PLATFORM,
                              "Can't create server info file %s: %s\n",
                              filename, strErrBuf );
            }
            else
            {
                FM_LOG_ERROR( FM_LOG_CAT_PLATFORM,
                              "Can't create server info file %s: %d\n",
                              filename, errno );
            }
#endif
            FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_FAIL);
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);

}   /* end InitializeSocketInfoFile */



/*****************************************************************************/
/** AddSocketInfoToFile
 * \ingroup intModel
 *
 * \desc            Appends a host/port pair to the socket info file.
 *
 * \param[in]       hostname is the machine host.
 *
 * \param[in]       port is the TCP port number.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmAddSocketInfoToFile(fm_text hostname, fm_int port,
                                     fm_bool usePipe)
{
    FILE *  fd;
    fm_text filePath;
    fm_char currentPath[] = ".";
    fm_char filename[512];
    fm_int  modelPos;
    char    strErrBuf[FM_STRERROR_BUF_SIZE];

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "port=%d\n",
                 port);

    filePath = getenv("WMODEL_INFO_PATH");

    if (!filePath)
    {
        filePath = currentPath;
    }

    snprintf(filename, sizeof(filename), "%s/%s", filePath, MBY_MODEL_FILE);

#if 0
    modelPos = fmGetIntApiAttribute(FM_AAK_API_PLATFORM_MODEL_POSITION,
                                    FM_AAD_API_PLATFORM_MODEL_POSITION);
#endif

	modelPos = 0;
    if ( ( fd = fopen(filename, "at") ) != NULL )
    {
        fprintf(fd, "%d:%s:%d\n", modelPos, hostname, port);
        if (usePipe)
        {
            fprintf(fd, "UsePipeForMgmtAccess\n");
        }
        fclose(fd);
    }
    else
    {
        char * e = strerror_r(errno, strErrBuf, FM_STRERROR_BUF_SIZE);
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                     "Can't append to server info file: %s - %d\n", strErrBuf, errno);

        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_FAIL);
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);

}   /* end AddSocketInfoToFile */



/*****************************************************************************/
/** fmCreateNetworkServer
 * \ingroup intModel
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

    FM_NOT_USED(backlog);

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
    socketInfo->type = type;

    FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, status);

} /* end fmCreateNetworkServer */


/*****************************************************************************/
/** fmCreateNetworkClient
 * \ingroup intModel
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

    memcpy(&socketInfo->address.sin_addr.s_addr,
                // sizeof(socketInfo->address.sin_addr.s_addr),
                hp->h_addr,
                hp->h_length);

    socketInfo->address.sin_port = htons(port);
    socketInfo->type = type;

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
 * \ingroup intModel
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
    fm_int        k;
    fm_int        i;
    fm_int        j;
    fm_int        slot;
    fm_bool       doIncrement;
    struct pollfd fds[FM_MAX_FDS_NUM] = { {0} };
    fm_int        fdsCnt;
    fm_int        fdsTimeout;
    socklen_t     addrLen = SOCKET_ADDRLEN;
    char          strErrBuf[FM_STRERROR_BUF_SIZE];
    int           e = 1;
    int           fdsMap[MAX_PERSISTENT_CONNECTIONS + 1];

#ifdef VERBOSE
    FM_LOG_ENTRY_VERBOSE(FM_LOG_CAT_PLATFORM,
                         "sockets=%p, numSockets=%p(%d), maxSockets=%d, "
                         "eventReceived=%p, timeout=%p\n",
                         (void *) sockets,
                         (void *) numSockets,
                         (numSockets ? *numSockets : -1),
                         maxSockets,
                         (void *) eventReceived,
                         (void *) timeout);
#endif

    if ((!sockets || !eventReceived || !numSockets || (*numSockets == 0) || \
        (maxSockets > FM_MAX_FDS_NUM) || (*numSockets > FM_MAX_FDS_NUM)))
    {
        FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    currNumSockets = *numSockets;

    for ( i = 0, fdsCnt = 0 ; i < currNumSockets ; i++ )
    {
#ifdef VERBOSE
        FM_LOG_DEBUG_VERBOSE(FM_LOG_CAT_PLATFORM, "Watching descriptor %d type %d\n",
                             sockets[i]->sock, sockets[i]->type);
#endif
        eventReceived[i] = FM_NETWORK_EVENT_NONE;
        if (sockets[i]->type != FM_SOCKET_TYPE_CLOSED)
        {
            fds[fdsCnt].fd = (sockets[i]->type==FM_SOCKET_TYPE_PIPE)?
                sockets[i]->rxPipe:sockets[i]->sock;
            fds[fdsCnt].events = POLLIN;
            fds[fdsCnt].revents = 0;
            fdsMap[fdsCnt] = i;
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

    for (k = 0; k < fdsCnt; k++)
    {
        i = fdsMap[k];

#ifdef VERBOSE
        FM_LOG_DEBUG_VERBOSE(FM_LOG_CAT_PLATFORM,
                             "POLL Sock#%d fd %d type %d event %x\n",
                             i, sockets[i]->sock, sockets[i]->type,
                             fds[k].revents);
#endif
        if (fds[k].revents & POLLIN)
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
                setsockopt(sockets[slot]->sock, IPPROTO_TCP, TCP_NODELAY,
                           (void *)&e, sizeof(e));

				FM_LOG_DEBUG2(FM_LOG_CAT_PLATFORM,
						"New client (fd=%d) connection on slot %d\n",
						sockets[slot]->sock, slot);

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

//#ifdef VERBOSE
#if 1
                FM_LOG_DEBUG2(FM_LOG_CAT_PLATFORM,
                              "Data available on client #%d\n", i - 1);
#endif
            }
        }
    }

#ifdef VERBOSE
    FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, FM_OK);
#else
    return FM_OK;
#endif

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
 * \ingroup intModel
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
            nb = send(socketInfo->sock, data, numBytes, MSG_DONTWAIT);
            if (nb < 0 && errno == ECONNRESET)
            {
                FM_LOG_WARNING(FM_LOG_CAT_PLATFORM,
                              "Socket is reset, close socket %d\n",
                              socketInfo->sock);
                socketInfo->sock = -1;
                FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, FM_FAIL);
            }
            FM_LOG_SYS_EXIT_ON_COND(FM_LOG_CAT_PLATFORM, nb == -1);

            if (nb > 0)
            {
                data = (((fm_byte *) data) + nb);
                numBytes -= nb;
            }
        } while ((nb > 0) && numBytes);
    }
    else if (socketInfo->type == FM_SOCKET_TYPE_PIPE)
    {
		FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, "Unsupported type: pipes\n");
    }

    FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, FM_OK);

} /* end fmSendNetworkData */



/*****************************************************************************/
/** fmReceiveNetworkData
 * \ingroup intModel
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
            if (nb < 0)
                FM_LOG_WARNING(FM_LOG_CAT_PLATFORM,
                               "Sock %d Connection failed reading %d bytes\n",
                                socketInfo->sock, maxBytes - *numBytes);
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
    else if (socketInfo->type == FM_SOCKET_TYPE_PIPE)
    {
        ptr       = data;
        *numBytes = 0;

        do
        {
            nb = read(socketInfo->rxPipe, ptr, maxBytes - *numBytes);
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


