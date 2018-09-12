/* vim:et:sw=4:ts=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_model_packet_queue.c
 * Creation Date:   August 27, 2010
 * Description:     Contains functions to manage and process the shared packet
 *                  queue used by model instances.
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

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <poll.h>

#include <fm_sdk_int.h>
#include <platforms/common/packet/generic-packet/fm_generic_packet.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define MAX_PERSISTENT_CONNECTIONS 4

#if 0
#define DEBUG_PRINT_RECEIVED_PACKET
#endif

#define MAP_CARDINAL_PORTS

#define FM_MODEL_CPK_META_SIZE 32
#define FM_MODEL_META_TYPE_LAN_TX    1

/*****************************************************************************
 * Global Variables
 *****************************************************************************/

/* Refer to the current topology file */
fm_char currentTopology[512];


/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

static void DecodePacket(fm_modelMessage *     imsg,
                         fm_int32              msgLength,
                         fm_byte **            packet,
                         fm_int32 *            pktLength,
                         fm_modelSidebandData *sbData);
static void FinalizePacket(fm_modelMessage *     emsg,
                           fm_int32              pktLength,
                           fm_modelSidebandData *sbData,
                           fm_int32 *            msgLength);
static fm_status MapLogicalPortToPhysical(fm_int   sw,
                                          fm_int   logPort,
                                          fm_int * physPort);
static fm_status MapPhysicalPortToLogical(fm_int   sw,
                                          fm_int   physPort,
                                          fm_int * logPort);
static fm_status ReceiveMessage(fm_socket *      socket,
                                fm_bool          closeSocket,
                                fm_modelMessage *imsg);
static fm_status SendMessage(fm_socket *      socket,
                             fm_modelMessage *emsg,
                             fm_int32         msgLength);

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** InitializeSocketInfoFile
 * \ingroup intModelPktQ
 *
 * \desc            Creates an empty socket info file.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status InitializeSocketInfoFile(void)
{
    FILE *  fd;
    fm_text filePath;
    fm_char currentPath[] = ".";
    fm_char filename[512];
    fm_int  modelPos;
    char    strErrBuf[FM_STRERROR_BUF_SIZE];
    errno_t strErrNum;

    FM_LOG_ENTRY_NOARGS(FM_LOG_CAT_PLATFORM);

    filePath = getenv("WMODEL_INFO_PATH");

    if (!filePath)
    {
        filePath = currentPath;
    }

    snprintf(filename, sizeof(filename), "%s/models.packetServer", filePath);

    modelPos = fmGetIntApiAttribute(FM_AAK_API_PLATFORM_MODEL_POSITION,
                                    FM_AAD_API_PLATFORM_MODEL_POSITION);

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
            strErrNum = FM_STRERROR_S(strErrBuf, FM_STRERROR_BUF_SIZE, errno);
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
            FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_FAIL);
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);

}   /* end InitializeSocketInfoFile */



/*****************************************************************************/
/** AddSocketInfoToFile
 * \ingroup intModelPktQ
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
static fm_status AddSocketInfoToFile(fm_text hostname, fm_int port)
{
    FILE *  fd;
    fm_text filePath;
    fm_char currentPath[] = ".";
    fm_char filename[512];
    fm_int  modelPos;
    char    strErrBuf[FM_STRERROR_BUF_SIZE];
    errno_t strErrNum;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "port=%d\n",
                 port);

    filePath = getenv("WMODEL_INFO_PATH");

    if (!filePath)
    {
        filePath = currentPath;
    }

    snprintf(filename, sizeof(filename), "%s/models.packetServer", filePath);

    modelPos = fmGetIntApiAttribute(FM_AAK_API_PLATFORM_MODEL_POSITION,
                                    FM_AAD_API_PLATFORM_MODEL_POSITION);

    if ( ( fd = fopen(filename, "at") ) != NULL )
    {
        fprintf(fd, "%d:%s:%d\n", modelPos, hostname, port);
        fclose(fd);
    }
    else
    {
        strErrNum = FM_STRERROR_S(strErrBuf, FM_STRERROR_BUF_SIZE, errno);
        if (strErrNum == 0)
        {
            FM_LOG_ERROR( FM_LOG_CAT_PLATFORM,
                          "Can't append to server info file: %s\n",
                          strErrBuf );
        }
        else

        {
            FM_LOG_ERROR( FM_LOG_CAT_PLATFORM,
                          "Can't append to server info file: %d\n",
                          errno );
        }

        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_FAIL);
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);

}   /* end AddSocketInfoToFile */




/*****************************************************************************/
/** UpdateEgressInfo
 * \ingroup intModelPktQ
 *
 * \desc            Update the egress information of a specific Unit/Switch/Port
 *
 * \param[in]       socket refer to the host to update
 *
 * \param[in]       sw is the switch into the host to update
 *
 * \param[in]       port is the port into the host to update
 *
 * \param[in]       destPort refer to the egress socket to redirect the traffic
 *                  to.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status UpdateEgressInfo(fm_socket* socket,
                                  fm_int     sw,
                                  fm_int     port,
                                  fm_int     destPort)
{
    fm_modelMessage packet;
    fm_status       err;
    fm_int32        msgLength;

    FM_CLEAR(packet);
    packet.version = htons(FM_MODEL_MSG_VERSION);
    packet.type    = htons(FM_MODEL_MSG_SET_EGRESS_INFO);
    packet.sw      = htons(sw);
    packet.port    = htons(port);

    *( (fm_uint16 *) &packet.data ) = htons(destPort);
    FM_STRCPY_S( (fm_char *) packet.data + 2,
                 FM_MODEL_MAX_PACKET_SIZE - (1 + 2),
                 "localhost" );

    msgLength = FM_MODEL_MSG_HEADER_SIZE + 2 + strlen("localhost");
    packet.msgLength = htonl(msgLength);

    err = SendMessage(socket, &packet, msgLength);

    return err;

}   /* end UpdateEgressInfo */




/*****************************************************************************/
/** DefineSocket
 * \ingroup intModelPktQ
 *
 * \desc            Creates a socket to use when switching packets between
 *                  internal links.
 *
 * \param[in]       curSocket is the host socket.
 *
 * \param[in]       curSwitch is the switch number for the link.
 *
 * \param[in]       curPort is the port number for the link.
 *
 * \return          Pointer to the socket for one side of the link.
 *                  NULL if the request fails.
 *
 *****************************************************************************/
static fm_socket * DefineSocket(fm_socket * curSocket,
                                fm_int      curSwitch,
                                fm_int      curPort)
{
    fm_socket * newSocket;
    fm_status   err;
    fm_int      acceptfd;
    socklen_t   addrLen = sizeof(struct sockaddr_in);

    newSocket = fmAlloc(sizeof(fm_socket));
    if (newSocket == NULL)
    {
        return NULL;
    }

    FM_CLEAR(*newSocket);
    newSocket->sock = -1;

    err = fmCreateNetworkServer(newSocket, FM_SOCKET_TYPE_TCP, 0, 3);
    if (err != FM_OK)
    {
        close(newSocket->sock);
        fmFree(newSocket);
        return NULL;
    }

    err = UpdateEgressInfo(curSocket, curSwitch, curPort, newSocket->serverPort);
    if (err != FM_OK)
    {
        close(newSocket->sock);
        fmFree(newSocket);
        return NULL;
    }

    acceptfd = accept(newSocket->sock,
                      (struct sockaddr *) &newSocket->address,
                      &addrLen);
    if (acceptfd < 0)
    {
        close(newSocket->sock);
        fmFree(newSocket);
        return NULL;
    }

    close(newSocket->sock);
    newSocket->sock = acceptfd;

    return newSocket;

}   /* end DefineSocket */




/*****************************************************************************/
/** fmModelPacketInterQueueTask
 * \ingroup intModelPktQ
 *
 * \desc            Thread used to switch packet between internal link.
 *
 * \param[in]       args is not used
 *
 * \return          NULL if error.
 *
 *****************************************************************************/
static void *fmModelPacketInterQueueTask(void *args)
{
    fm_modelPacketInterQueue interQueue[FM_MAX_MODEL_TOPOLOGY_INTERLINK];
    fm_modelMessage          packet;
    fm_socket *              portSocket[FM_MAX_MODEL_TOPOLOGY_INTERLINK];
    fm_socket *              tmpSocket;
    fm_socket                wmSocket[FM_MAX_MODEL_TOPOLOGY_UNIT];
    fm_status                err = FM_OK;
    fm_int32                 msgLength;
    fm_int                   curPort;
    fm_int                   curPort2;
    fm_int                   curPos;
    fm_int                   curPos2;
    fm_int                   curSwitch;
    fm_int                   curSwitch2;
    fm_int                   i;
    fm_int                   numSockets = 0;
    fm_int                   posixError;
    fm_int                   type;
    fm_text                  filePath;
    fm_char                  currentPath[] = ".";
    fm_char                  filename[512];
    fm_char                  s[800];
    fm_char *                p;
    fm_char *                p2;
    FILE *                   fd;
    struct pollfd            fds[FM_MAX_FDS_NUM];
    fm_int                   fdsCnt;
    char                     strErrBuf[FM_STRERROR_BUF_SIZE];
    errno_t                  strErrNum;

    FM_NOT_USED(args);

    filePath = getenv("WMODEL_INFO_PATH");

    if (!filePath)
    {
        filePath = currentPath;
    }

    snprintf(filename, sizeof(filename), "%s/models.packetServer", filePath);

    /* Extract all the unit in the configuration */
    if ( ( fd = fopen(filename, "r") ) != NULL )
    {
        while (TRUE)
        {
            p = fgets(s, sizeof(s), fd);
            if (p == NULL)
            {
                break;
            }
            p  = strchr(s, ':');
            if (p == NULL)
            {
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                             "Unable to parse string s: %s\n",
                             s);
                fclose(fd);
                return NULL;
            }

            *p++ = 0;
            FM_SSCANF_S(s, "%d", &curPos);

            if (curPos >= FM_MAX_MODEL_TOPOLOGY_UNIT)
            {
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                             "Position %d exceeds "
                             "maximum of %d\n",
                             curPos,
                             FM_MAX_MODEL_TOPOLOGY_UNIT - 1);

                fclose(fd);
                return NULL;
            }
            p2  = strchr(p, ':');
            if (p2 == NULL)
            {
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                             "Unable to parse string p: %s\n",
                             p);
                fclose(fd);
                return NULL;
            }

            *p2++ = 0;

            if (strlen(p) > 512)
            {
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                             "Server host %s exceeds "
                             "maximum of 512 characters\n",
                             p);

                fclose(fd);
                return NULL;
            }
            FM_SSCANF_S(p2, "%d", &curPort);

            err = fmCreateNetworkClient(&wmSocket[curPos],
                                        FM_SOCKET_TYPE_TCP,
                                        p,
                                        curPort);
            if (err != FM_OK)
            {
                fclose(fd);
                return NULL;
            }
        }
        fclose(fd);
    }
    else
    {
        strErrNum = FM_STRERROR_S(strErrBuf, FM_STRERROR_BUF_SIZE, errno);
        if (strErrNum == 0)
        {
            FM_LOG_ERROR( FM_LOG_CAT_PLATFORM,
                          "Can't read the server info file: %s\n",
                          strErrBuf );
        }
        else
        {
            FM_LOG_ERROR( FM_LOG_CAT_PLATFORM,
                          "Can't read the server info file: %d\n",
                          errno );
        }

        return NULL;
    }

    snprintf(filename, sizeof(filename), "%s/%s", filePath, currentTopology);

    /* Extract information into the topology file */
    if ( ( fd = fopen(filename, "r") ) != NULL )
    {
        numSockets = 0;
        while (TRUE)
        {
            p = fgets(s, sizeof(s), fd);
            if (p == NULL)
            {
                break;
            }

            /* Ignore comment line */
            if (strchr(s, '#') != NULL)
            {
                continue;
            }

            if (numSockets > (FM_MAX_MODEL_TOPOLOGY_INTERLINK - 2))
            {
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                             "Maximum (%d) of interlink exceeds\n",
                             FM_MAX_MODEL_TOPOLOGY_INTERLINK);

                fclose(fd);
                return NULL;
            }

            /* Extract the first Unit position */
            p  = strchr(s, ',');
            if (p == NULL)
            {
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                             "First unit position not in s string %s\n",
                             s);
                fclose(fd);
                return NULL;
            }

            *p++ = 0;
            FM_SSCANF_S(s, "%d", &curPos);

            if (curPos >= FM_MAX_MODEL_TOPOLOGY_UNIT)
            {
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                             "Position %d exceeds "
                             "maximum of %d\n",
                             curPos,
                             FM_MAX_MODEL_TOPOLOGY_UNIT - 1);

                fclose(fd);
                return NULL;
            }

            /* Extract the first switch */
            p2  = strchr(p, ',');
            if (p2 == NULL)
            {
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                             "First switch not in p string %s\n",
                             p);
                fclose(fd);
                return NULL;
            }

            *p2++ = 0;
            FM_SSCANF_S(p, "%d", &curSwitch);

            /* Extract the first port */
            p  = strchr(p2, ',');
            if (p == NULL)
            {
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                             "First port not in p2 string %s\n",
                             p2);
                fclose(fd);
                return NULL;
            }

            *p++ = 0;
            FM_SSCANF_S(p2, "%d", &curPort);

            /* Extract the second Unit position */
            p2  = strchr(p, ',');
            if (p2 == NULL)
            {
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                             "Second unit position not in p string %s\n",
                             p);
                fclose(fd);
                return NULL;
            }

            *p2++ = 0;
            FM_SSCANF_S(p, "%d", &curPos2);

            if (curPos2 >= FM_MAX_MODEL_TOPOLOGY_UNIT)
            {
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                             "Position %d exceeds "
                             "maximum of %d\n",
                             curPos2,
                             FM_MAX_MODEL_TOPOLOGY_UNIT - 1);

                fclose(fd);
                return NULL;
            }

            /* Extract the second switch */
            p  = strchr(p2, ',');
            if (p == NULL)
            {
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                             "Second switch not in p2 string %s\n",
                             p2);
                fclose(fd);
                return NULL;
            }

            *p++ = 0;
            FM_SSCANF_S(p2, "%d", &curSwitch2);

            /* Extract the second port */
            FM_SSCANF_S(p, "%d", &curPort2);

            /* Close the actual egress socket for the first Unit/port pair */
            err = UpdateEgressInfo(&wmSocket[curPos],
                                   curSwitch,
                                   curPort,
                                   FM_MODEL_SOCKET_PORT_DISABLE);
            if (err != FM_OK)
            {
                fclose(fd);
                return NULL;
            }

            /* Close the actual egress socket for the second Unit/port pair */
            err = UpdateEgressInfo(&wmSocket[curPos2],
                                   curSwitch2,
                                   curPort2,
                                   FM_MODEL_SOCKET_PORT_DISABLE);
            if (err != FM_OK)
            {
                fclose(fd);
                return NULL;
            }

            /* Create socket that will handle first Unit/port pair */
            interQueue[numSockets].destSwitch = curSwitch2;
            interQueue[numSockets].destSocket = &wmSocket[curPos2];
            interQueue[numSockets].destPort = curPort2;

            tmpSocket = DefineSocket(&wmSocket[curPos], curSwitch, curPort);
            if (tmpSocket == NULL)
            {
                fclose(fd);
                return NULL;
            }

            portSocket[numSockets] = tmpSocket;

            /* Create socket that will handle second Unit/port pair */
            interQueue[numSockets+1].destSwitch = curSwitch;
            interQueue[numSockets+1].destSocket = &wmSocket[curPos];
            interQueue[numSockets+1].destPort = curPort;

            tmpSocket = DefineSocket(&wmSocket[curPos2], curSwitch2, curPort2);
            if (tmpSocket == NULL)
            {
                fclose(fd);
                return NULL;
            }

            portSocket[numSockets+1] = tmpSocket;

            /* Two sockets are needed for each interlink to cover them in a
             * bidirectional way. */
            numSockets+= 2;
        }
        fclose(fd);
    }
    else
    {
        strErrNum = FM_STRERROR_S(strErrBuf, FM_STRERROR_BUF_SIZE, errno);
        if (strErrNum == 0)
        {
            FM_LOG_ERROR( FM_LOG_CAT_PLATFORM,
                          "Can't read the topology file: %s\n",
                          strErrBuf );
        }
        else
        {
            FM_LOG_ERROR( FM_LOG_CAT_PLATFORM,
                          "Can't read the topology file: %d\n",
                          errno );
        }

        return NULL;
    }

    if (numSockets > (FM_MAX_FDS_NUM))
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Maximum (%d) of interlink exceeded\n",
                     FM_MAX_FDS_NUM);

        return NULL;
    }

    /* This while loop switches internal packets from one unit to the other
     * based on the provided topology file. */
    while (TRUE)
    {
        for ( i = 0, fdsCnt = 0 ; i < numSockets ; i++ )
        {
            fds[fdsCnt].fd = portSocket[i]->sock;
            fds[fdsCnt].events = POLLIN;
            fds[fdsCnt].revents = 0;
            fdsCnt++;
        }

        /* Try to find interlink data */
        posixError = poll(fds, fdsCnt, FM_FDS_POLL_TIMEOUT_USEC);
        if (posixError == -1)
        {
            continue;
        }

        for ( i = 0, fdsCnt = 0; i < numSockets ; i++, fdsCnt++)
        {
            if (!fds[fdsCnt].revents & POLLIN)
            {
                continue;
            }

            /* Network data is available. Process it. */
            err = ReceiveMessage(portSocket[i], FALSE, &packet);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

            /* Do not switch this message unless it is a packet. */
            type = ntohs(packet.type);
            if ( type != ( (fm_uint16) FM_MODEL_MSG_PACKET ) )
            {
                continue;
            }

            msgLength   = ntohl(packet.msgLength);
            packet.sw   = htons(interQueue[i].destSwitch);
            packet.port = htons(interQueue[i].destPort);
            err = SendMessage(interQueue[i].destSocket, &packet, msgLength);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
        
        }   /* end for ( i = 0, fdsCnt = 0; i < numSockets ; i++, fdsCnt++) */

    }   /* end while (TRUE) */

ABORT:
    return NULL;

}   /* end fmModelPacketInterQueueTask */




/*****************************************************************************/
/** BuildTopology
 * \ingroup intModelPktQ
 *
 * \desc            Build the specified topology.
 *
 * \param[in]       topology points to the topology config file to apply.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status BuildTopology(fm_text topology)
{
    fm_status err;
    fm_modelPacketQueue *state;

    FM_STRNCPY_S(currentTopology, sizeof(currentTopology), topology, 512);

    state = (fm_modelPacketQueue *) fmRootPlatform->packetQueue;

    /* create thread to listen to sockets */
    err = fmCreateThread("model_inter_pkt_queue_task",
                         FM_EVENT_QUEUE_SIZE_NONE,
                         &fmModelPacketInterQueueTask,
                         0,
                         &state->interQueueProcessorHandle);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end BuildTopology */


static void DecodeTlvPacket(fm_byte              *data,
                            fm_int32              dataLength,
                            fm_byte **            packet,
                            fm_int32 *            pktLength,
                            fm_modelSidebandData *sbData)
{
    fm_int32  length;
    fm_int32  p;
    fm_byte   type;

    p = 0;

    while (p < dataLength)
    {
        type = data[p];
        p += FM_MODEL_DATA_TYPE_SIZE;
        length = ntohl(*( (fm_int32 *) &data[p] ));
        p += FM_MODEL_DATA_LENGTH_SIZE;
        switch (type)
        {
            case FM_MODEL_DATA_PACKET:
                *packet = &data[p];
                *pktLength = length;
                break;

            case FM_MODEL_DATA_SB_ID:
                sbData->idTag = ntohl(*( (fm_uint32 *) &data[p] ));
                break;

            case FM_MODEL_DATA_SB_TC:
                sbData->tc = data[p];
                break;

            case FM_MODEL_PACKET_META:
                FM_MEMCPY_S(sbData->pktMeta,
                     sizeof(sbData->pktMeta),
                     &data[p],
                     length);
                break;
        }
        p += length;

    }   /* end while (p < dataLength) */

}   /* end DecodeTlvPacket */


static void DecodePacket(fm_modelMessage *     imsg,
                         fm_int32              msgLength,
                         fm_byte **            packet,
                         fm_int32 *            pktLength,
                         fm_modelSidebandData *sbData)
{
    fm_int32  dataLength;

    dataLength = msgLength - FM_MODEL_MSG_HEADER_SIZE;

    DecodeTlvPacket(imsg->data, dataLength, packet, pktLength, sbData);

}   /* end DecodePacket */




static void FinalizePacket(fm_modelMessage *     emsg,
                           fm_int32              pktLength,
                           fm_modelSidebandData *sbData,
                           fm_int32 *            msgLength)
{
    fm_int32 p;

    p = ( (fm_int32) FM_MODEL_MSG_TLV_SIZE ) + pktLength;

    emsg->data[p] = FM_MODEL_DATA_SB_ID;
    p += FM_MODEL_DATA_TYPE_SIZE;
    *( (fm_int32 *) &emsg->data[p] ) = htonl(sizeof(sbData->idTag));
    p += FM_MODEL_DATA_LENGTH_SIZE;
    *( (fm_uint32 *) &emsg->data[p] ) = htonl(sbData->idTag);
    p += sizeof(sbData->idTag);

    emsg->data[p] = FM_MODEL_DATA_SB_TC;
    p += FM_MODEL_DATA_TYPE_SIZE;
    *( (fm_int32 *) &emsg->data[p] ) = htonl(sizeof(sbData->tc));
    p += FM_MODEL_DATA_LENGTH_SIZE;
    emsg->data[p] = sbData->tc;
    p += sizeof(sbData->tc);

    emsg->data[p] = FM_MODEL_PACKET_META;
    p += FM_MODEL_DATA_TYPE_SIZE;
    *( (fm_int32 *) &emsg->data[p] ) = htonl(sizeof(sbData->pktMeta));
    p += FM_MODEL_DATA_LENGTH_SIZE;
    FM_MEMCPY_S(&emsg->data[p],
                sizeof(sbData->pktMeta),
                sbData->pktMeta,
                sizeof(sbData->pktMeta));
    p += sizeof(sbData->pktMeta);

    *msgLength += p;

}   /* end FinalizePacket */




/*****************************************************************************/
/** HandleMsgAttr
 * \ingroup intModelPktQ
 *
 * \desc            Processes attribute messages and creates and enqueues
 *                  response message as necessary.
 *                                                                      \lb\lb
 *                  The attribute message format is:
 *                                                                      \lb\lb
 *                  +--------+------+------+--------+-----+--------+-------+
 *                  |        |      | attr |  key   |     | value  |       |
 *                  | header | type | type | length | key | length | value |
 *                  |  (12B) | (1B) | (1B) |  (2B)  |     |  (2B)  |       |
 *                  +--------+------+------+--------+-----+--------+-------+
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[in]       mpq points to the packet queue state.
 *
 * \param[in]       imsg points to the received message that is to be processed.
 *
 * \param[in]       msgLength is the length of the received message in units of
 *                  bytes.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if no white model is associated with
 *                  the specified switch number.
 * \return          FM_ERR_INVALID_ARGUMENT if the received message is not a
 *                  valid attribute message.
 *
 *****************************************************************************/
static fm_status HandleMsgAttr(fm_int               sw,
                               fm_modelPacketQueue *mpq,
                               fm_modelMessage *    imsg,
                               fm_int32             msgLength)
{
    fm_modelMessage *emsg;
    fm_apiAttrType   attrType;
    fm_status        status;
    fm_uint16        keyLength;
    fm_uint16        size;
    fm_int           intValue;
    fm_int           offset;
    fm_byte          type;
    fm_text          textValue;
    fm_char          key[FM_MODEL_ATTR_MAX_KEY_SIZE];
    fm_bool          boolValue;
    errno_t          rv;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d mpq=%p imsg=%p msgLength=%d\n",
                 sw,
                 (void *) mpq,
                 (void *) imsg,
                 msgLength);

    if (mpq->queueState[sw] == NULL)
    {
        status = FM_ERR_INVALID_SWITCH;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    type = imsg->data[0];
    attrType = imsg->data[1];
    keyLength = ntohs( *( (fm_uint16 *) &imsg->data[2] ) );

    if (keyLength == 0 || keyLength > FM_MODEL_ATTR_MAX_KEY_SIZE)
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    FM_CLEAR(key);

    rv = FM_MEMCPY_S(key,
                     FM_MODEL_ATTR_MAX_KEY_SIZE,
                     &imsg->data[4],
                     keyLength);
    if (rv != 0)
    {
        status = FM_FAIL;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    if (key[keyLength - 1] != '\0')
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    offset = 4 + keyLength;

    switch (type)
    {
        case FM_MODEL_ATTR_GET_REQUEST:
            FM_MODEL_PKTQ_GET_TAIL(mpq, &emsg);

            FM_CLEAR(*emsg);

            rv = FM_MEMCPY_S(emsg,
                             FM_MODEL_MSG_SIZE,
                             imsg,
                             FM_MODEL_MSG_HEADER_SIZE + offset);
            if (rv != 0)
            {
                status = FM_FAIL;
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            }

            emsg->data[0] = FM_MODEL_ATTR_GET_RESPONSE;

            offset += 2;
            switch (attrType)
            {
                case FM_API_ATTR_INT:
                    status = fmGetApiAttribute(key, attrType, &intValue);
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

                    size = 4;
                    *( (fm_uint32 *) &emsg->data[offset] ) = htonl(intValue);
                    break;

                case FM_API_ATTR_BOOL:
                    status = fmGetApiAttribute(key, attrType, &boolValue);
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

                    size = 1;
                    emsg->data[offset] = boolValue;
                    break;

                case FM_API_ATTR_TEXT:
                    status = fmGetApiAttribute(key, attrType, &textValue);
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

                    size = FM_STRNLEN_S(textValue, FM_API_ATTR_TEXT_MAX_LENGTH);
                    if (size >= FM_API_ATTR_TEXT_MAX_LENGTH)
                    {
                        status = FM_ERR_INVALID_ARGUMENT;
                        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
                    }
                    rv = FM_MEMCPY_S(&emsg->data[offset],
                                     FM_MODEL_MAX_PACKET_SIZE - offset,
                                     textValue,
                                     size);
                    if (rv != 0)
                    {
                        status = FM_FAIL;
                        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
                    }

                    /* The terminating NUL character is included in the text
                     * attribute length. */
                    size += 1;
                    break;

                case FM_API_ATTR_FLOAT:
                    /* FIXME Add support for float based attributes. */
                default:
                    /* The supplied attribute type is unrecognized. */
                    status = FM_ERR_INVALID_ARGUMENT;
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            }

            msgLength = FM_MODEL_MSG_HEADER_SIZE + offset + size;
            emsg->msgLength = htonl(msgLength);
            *( (fm_uint16 *) &emsg->data[offset - 2] ) = htons(size);

            FM_MODEL_PKTQ_UPDATE_TAIL(mpq, msgLength);
            break;

        case FM_MODEL_ATTR_SET:
            size = ntohs( *( (fm_uint16 *) &imsg->data[offset] ) );
            offset += 2;

            switch (attrType)
            {
                case FM_API_ATTR_INT:
                    if (size != 4)
                    {
                        status = FM_ERR_INVALID_ARGUMENT;
                        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
                    }
                    intValue = ntohl(*( (fm_uint32 *) &imsg->data[offset] ));

                    status = fmSetApiAttribute(key, attrType, &intValue);
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
                    break;

                case FM_API_ATTR_BOOL:
                    if (size != 1)
                    {
                        status = FM_ERR_INVALID_ARGUMENT;
                        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
                    }
                    boolValue = imsg->data[offset];

                    status = fmSetApiAttribute(key, attrType, &boolValue);
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
                    break;

                case FM_API_ATTR_TEXT:
                    if ( ( size > FM_API_ATTR_TEXT_MAX_LENGTH ) ||
                         ( imsg->data[offset + size] != '\0' ) )
                    {
                        status = FM_ERR_INVALID_ARGUMENT;
                        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
                    }
                    textValue = (fm_text) &imsg->data[offset];

                    status = fmSetApiAttribute(key, attrType, &textValue);
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
                    break;

                case FM_API_ATTR_FLOAT:
                    /* FIXME Add support for float based attributes. */
                default:
                    /* The supplied attribute type is unrecognized. */
                    status = FM_ERR_INVALID_ARGUMENT;
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            }

            FM_MODEL_PKTQ_GET_TAIL(mpq, &emsg);

            FM_CLEAR(*emsg);

            rv = FM_MEMCPY_S(emsg, FM_MODEL_MSG_SIZE, imsg, msgLength);
            if (rv != 0)
            {
                status = FM_FAIL;
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            }

            emsg->data[0] = FM_MODEL_ATTR_SET_ACK;

            FM_MODEL_PKTQ_UPDATE_TAIL(mpq, msgLength);
            break;

        default:
            /* The supplied attribute message type is unrecognized. */
            status = FM_ERR_INVALID_ARGUMENT;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end HandleMsgAttr */




/*****************************************************************************/
/** HandleMsgGetInfo
 * \ingroup intModelPktQ
 *
 * \desc            Processes model information messages and creates and
 *                  enqueues response messages as necessary.
 *                                                                      \lb\lb
 *                  The model information message format is:
 *                                                                      \lb\lb
 *                  +--------------+-----------+-------------+
 *                  | header (12B) | type (1B) | #ports (2B) |
 *                  +--------------+-----------+-------------+
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[in]       mpq points to the packet queue state.
 *
 * \param[in]       imsg points to the received message that is to be processed.
 *
 * \param[in]       msgLength is the length of the received message in units of
 *                  bytes.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if no white model is associated with
 *                  the specified switch number.
 * \return          FM_ERR_INVALID_ARGUMENT if the received message is not a
 *                  valid model information message.
 *
 *****************************************************************************/
fm_status HandleMsgGetInfo(fm_int               sw,
                           fm_modelPacketQueue *mpq,
                           fm_modelMessage *    imsg,
                           fm_int32             msgLength)
{
    fm_modelMessage *emsg;
    fm_status        status;
    const fm_int32   reqLength = FM_MODEL_MSG_HEADER_SIZE + 3;
    fm_int           numPorts;
    errno_t          rv;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d mpq=%p imsg=%p msgLength=%d\n",
                 sw,
                 (void *) mpq,
                 (void *) imsg,
                 msgLength);

    if (mpq->queueState[sw] == NULL)
    {
        status = FM_ERR_INVALID_SWITCH;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }
    else if (msgLength != reqLength)
    {
        /* This model information message is too short or too long. */
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    status = mpq->pktQueueServices.GetNumPorts(sw, &numPorts);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    FM_MODEL_PKTQ_GET_TAIL(mpq, &emsg);

    FM_CLEAR(*emsg);

    rv = FM_MEMCPY_S(emsg, FM_MODEL_MSG_SIZE, imsg, FM_MODEL_MSG_HEADER_SIZE);
    if (rv != 0)
    {
        status = FM_FAIL;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    emsg->data[0]                     = FM_MODEL_INFO_RESPONSE;
    *( (fm_uint16 *) &emsg->data[1] ) = htons(numPorts);

    FM_MODEL_PKTQ_UPDATE_TAIL(mpq, reqLength);

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end HandleMsgGetInfo */




/*****************************************************************************/
/** HandleMsgMgmt
 * \ingroup intModelPktQ
 *
 * \desc            Processes management messages and creates and enqueues
 *                  response messages as necessary.
 *                                                                      \lb\lb
 *                  The management message format is:
 *                                                                      \lb\lb
 *                  +--------------+-----------+--------------+------------+
 *                  | header (12B) | type (1B) | address (4B) | value (4B) |
 *                  +--------------+-----------+--------------+------------+
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[in]       mpq points to the packet queue state.
 *
 * \param[in]       imsg points to the received message that is to be processed.
 *
 * \param[in]       msgLength is the length of the received message in units of
 *                  bytes.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if no white model is associated with
 *                  the specified switch number.
 * \return          FM_ERR_INVALID_ARGUMENT if the received message is not a
 *                  valid management message.
 *
 *****************************************************************************/
static fm_status HandleMsgMgmt(fm_int               sw,
                               fm_modelPacketQueue *mpq,
                               fm_modelMessage *    imsg,
                               fm_int32             msgLength)
{
    fm_modelMessage *emsg;
    fm_status        status = FM_ERR_INVALID_ARGUMENT;
    fm_uint32        addr;
    fm_uint32        value;
    const fm_int32   reqLength = FM_MODEL_MSG_HEADER_SIZE + 9;
    fm_byte          type;
    errno_t          rv;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d mpq=%p imsg=%p msgLength=%d\n",
                 sw,
                 (void *) mpq,
                 (void *) imsg,
                 msgLength);

    if (mpq->queueState[sw] == NULL)
    {
        status = FM_ERR_INVALID_SWITCH;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }
    else if (msgLength != reqLength)
    {
        /* This management message is too short or too long. */
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    type  = imsg->data[0];
    addr  = ntohl( *( (fm_uint32 *) &imsg->data[1] ) );
    value = ntohl( *( (fm_uint32 *) &imsg->data[5] ) );

    switch (type)
    {
        case FM_MODEL_MGMT_READ_REQUEST:
            status = fmReadUINT32(sw, addr, &value);

            if (status == FM_OK)
            {
                /* Create and enqueue a read response message. */
                FM_MODEL_PKTQ_GET_TAIL(mpq, &emsg);

                FM_CLEAR(*emsg);

                rv = FM_MEMCPY_S(emsg,
                                 FM_MODEL_MSG_SIZE,
                                 imsg,
                                 FM_MODEL_MSG_HEADER_SIZE);
                if (rv != 0)
                {
                    status = FM_FAIL;
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
                }

                emsg->data[0] = FM_MODEL_MGMT_READ_RESPONSE;
                *( (fm_uint32 *) &emsg->data[1] ) = htonl(addr);
                *( (fm_uint32 *) &emsg->data[5] ) = htonl(value);

                FM_MODEL_PKTQ_UPDATE_TAIL(mpq, reqLength);
            }
            else
            {
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                             "Can't read 0x%08x: %s\n",
                             addr,
                             fmErrorMsg(status));
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            }
            break;

        case FM_MODEL_MGMT_WRITE:
            status = fmWriteUINT32(sw, addr, value);

            if (status == FM_OK)
            {
                FM_MODEL_PKTQ_GET_TAIL(mpq, &emsg);

                FM_CLEAR(*emsg);

                rv = FM_MEMCPY_S(emsg, FM_MODEL_MSG_SIZE, imsg, reqLength);
                if (rv != 0)
                {
                    status = FM_FAIL;
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
                }

                emsg->data[0] = FM_MODEL_MGMT_WRITE_ACK;

                FM_MODEL_PKTQ_UPDATE_TAIL(mpq, reqLength);
            }
            else
            {
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                             "Can't write 0x%08x: %s\n",
                             addr,
                             fmErrorMsg(status));
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            }
            break;

        default:
            /* The supplied management type is unrecognized. */
            status = FM_ERR_INVALID_ARGUMENT;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end HandleMsgMgmt */




/*****************************************************************************/
/** HandleMsgPacket
 * \ingroup intModelPktQ
 *
 * \desc            Processes packet and loopback packet messages and creates
 *                  and enqueues egress packet message as necessary.
 *                                                                      \lb\lb
 *                  The packet and loopback packet message format is:
 *                                                                      \lb\lb
 *                  +--------------+--------------------+-------------+
 *                  | header (12B) | sideband data (4B) | packet data |
 *                  +--------------+--------------------+-------------+
 *                                                                      \lb\lb
 *                  The length of the packet data field is variable, but can't
 *                  exceed ''FM_MODEL_MAX_PACKET_SIZE''.
 *
 * \param[in]       sw is the switch to operate on.
 *
 * \param[in]       mpq points to the packet queue state.
 *
 * \param[in]       imsg points to the received message that is to be processed.
 *
 * \param[in]       logPort is the logical port on which the packet ingresses
 *                  the model.
 *
 * \param[in]       imsgLength is the length of the received message in units of
 *                  bytes.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
static fm_status HandleMsgPacket(fm_int               sw,
                                 fm_modelPacketQueue *mpq,
                                 fm_modelMessage *    imsg,
                                 fm_int               logPort,
                                 fm_int32             imsgLength)
{
    fm_modelMessage *    emsg;
    fm_modelSidebandData sbData;
    fm_status            status;
    fm_int32             emsgLength;
    fm_int32             pktLength;
    fm_int               freeSpace;
    fm_int               physPort;
    fm_uint16            numPkts;
    fm_byte *            data;
    fm_byte *            packet;
    fm_bool              hasSpace;
    errno_t              rv;

    FM_LOG_ENTRY( FM_LOG_CAT_PLATFORM,
                  "sw=%d, mpq=%p, imsg=%p, logPort=%d, imsgLength=%d\n",
                  sw,
                  (void *) mpq,
                  (void *) imsg,
                  logPort,
                  imsgLength );

    if (mpq->queueState[sw] == NULL)
    {
        status = FM_ERR_INVALID_SWITCH;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    packet = NULL;
    pktLength = 0;
    FM_CLEAR(sbData);

    DecodePacket(imsg, imsgLength, &packet, &pktLength, &sbData);

    if ( ( packet == NULL ) || ( pktLength == 0 ) )
    {
        /* This packet message is too short. */
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    /**************************************************
     * Step 3:
     * Send packet through the model.
     **************************************************/

    status = MapLogicalPortToPhysical(sw, logPort, &physPort);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

RESEND_PACKET:
    status = mpq->pktQueueServices.SendPacket(sw,
                                              physPort,
                                              packet,
                                              pktLength,
                                              &sbData);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    mpq->queueState[sw]->packetSent[logPort]++;

    numPkts = 0;

    while (status == FM_OK)
    {
        /***************************************************
         * Step 4a:
         * Compute the packet queue free space and decide
         * whether the egressing packet should be stored
         * in the ingress or egress message data buffer.
         **************************************************/
        if (mpq->msgQueueHead == mpq->msgQueueTail)
        {
            freeSpace = FM_MODEL_PKTQ_MSG_QUEUE_SIZE;
        }
        else if (mpq->msgQueueHead > mpq->msgQueueTail)
        {
            freeSpace = mpq->msgQueueHead - mpq->msgQueueTail;
        }
        else
        {
            freeSpace = FM_MODEL_PKTQ_MSG_QUEUE_SIZE -
                        (mpq->msgQueueTail - mpq->msgQueueHead);
        }

        FM_LOG_ABORT_ON_ASSERT(FM_LOG_CAT_PLATFORM,
                               freeSpace >= 0,
                               status = FM_ERR_ASSERTION_FAILED,
                               "ERROR: invalid free space computation\n");

        FM_MODEL_PKTQ_GET_TAIL(mpq, &emsg);
        hasSpace = freeSpace >= (fm_int) (FM_MODEL_MSG_SIZE + sizeof(fm_int32));
        data = ( hasSpace ? emsg->data : imsg->data );

        /***************************************************
         * Step 4b:
         * Receive the packet(s) egressing from the model.
         **************************************************/
        *data = FM_MODEL_DATA_PACKET;
        packet = &data[FM_MODEL_MSG_TLV_SIZE];
        FM_CLEAR(sbData);

        status = mpq->pktQueueServices.ReceivePacket(sw,
                                                     &physPort,
                                                     packet,
                                                     &pktLength,
                                                     FM_MODEL_MAX_PACKET_SIZE,
                                                     &sbData);

        *( (fm_int32 *) &data[FM_MODEL_DATA_TYPE_SIZE] ) = htonl(pktLength);
        /* status value of FM_ERR_SPARE1 is used to indicate that
         * loopback is requested for the current packet */
        if (status == FM_ERR_SPARE1)
        {
            status = FM_OK;
            FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                         "TB: loopback packet received on phys port %d,"
                         " resending\n",
                         physPort);
            goto RESEND_PACKET;

        }

        if ( ( status != FM_ERR_NO_MORE ) &&
             ( MapPhysicalPortToLogical(sw, physPort, &logPort) != FM_OK ) )
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "ERROR: Can't map physical port %d to logical port\n",
                         physPort);
            continue;
        }

        /***************************************************
         * Step 5:
         * Scrap the packet if there is no free queue space.
         * Otherwise, put the packet into the packet queue.
         **************************************************/
        if (!hasSpace)
        {
            if ( ( status != FM_ERR_NO_MORE ) || mpq->sendEOT )
            {
                mpq->queueState[sw]->packetDropped[logPort]++;
            }

            FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                         "TB: Dropped packet received on port %d\n",
                         logPort);

            continue;
        }

        /* Check for valid packet. */
        if ( ( ( status == FM_OK ) &&
               ( pktLength >= 0 ) &&
               ( pktLength < FM_MODEL_MAX_PACKET_SIZE ) ) ||
             ( ( status == FM_ERR_NO_MORE ) && mpq->sendEOT ) )
        {
            /* We received a valid packet... */
            mpq->queueState[sw]->packetReceived[logPort]++;

            /* Finalize and enqueue the packet. */
            emsgLength = FM_MODEL_MSG_HEADER_SIZE;
            if (status == FM_ERR_NO_MORE)
            {
                emsg->type                        = FM_MODEL_MSG_PACKET_EOT;
                emsgLength                        += 2;
                *( (fm_uint16 *) &emsg->data[0] ) = htons(numPkts);
            }
            else
            {
                emsg->type = FM_MODEL_MSG_PACKET;
                numPkts    += 1;

                /* Copy the sideband data into the egress packet. */
                FinalizePacket(emsg, pktLength, &sbData, &emsgLength);

            }
            emsg->msgLength = htonl(emsgLength);
            emsg->version   = htons(FM_MODEL_MSG_VERSION);
            emsg->sw        = htons(sw);
            emsg->port      = htons(logPort);
            emsg->type      = htons(emsg->type);

            /* Advance the tail pointer */
            FM_MODEL_PKTQ_UPDATE_TAIL(mpq, emsgLength);

            FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                         "TB: Egress packet on port %d, length %d, "
                         "count is now %d, new tail is %d\n",
                         logPort,
                         emsgLength,
                         mpq->queueState[sw]->packetReceived[logPort],
                         mpq->msgQueueTail);
        }
        else
        {
            /* No packet received or a processing error has occurred... */
            FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                         "TB: No egress packet, ingress length %d, "
                         "status %d (%s)\n",
                         pktLength,
                         status,
                         fmErrorMsg(status));

            /* Packets may still be queued */
            if (status == FM_ERR_MCAST_INVALID_STATE)
            {
                status = FM_OK;
            }
        }

    }   /* end while (status == FM_OK) */

    if (status == FM_ERR_NO_MORE)
    {
        status = FM_OK;
    }

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end HandleMsgPacket */




/*****************************************************************************/
/** HandleMsgSetEgressInfo
 * \ingroup intModelPktQ
 *
 * \desc            Processes egress information messages.
 *                                                                      \lb\lb
 *                  The egress information message format is:
 *                                                                      \lb\lb
 *                  +--------------+---------------+-----------------+
 *                  | header (12B) | TCP port (2B) | hostname (510B) |
 *                  +--------------+---------------+-----------------+
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[out]      mpq points to the state for the packet queue
 *
 * \param[in]       imsg points to the message received.
 *
 * \param[in]       type is the message type.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if no white model is associated with
 *                  the specified switch number.
 *
 *****************************************************************************/
static fm_status HandleMsgSetEgressInfo(fm_int               sw,
                                        fm_modelPacketQueue *mpq,
                                        fm_modelMessage *    imsg,
                                        fm_int               type)
{
    fm_status           status = FM_OK;
    fm_uint16           port;
    fm_uint16           networkPort;
    fm_text             host;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d, mpq=%p, imsg=%p type=%d\n",
                 sw,
                 (void *) mpq,
                 (void *) imsg,
                 type);

    if (mpq->queueState[sw] == NULL)
    {
        status = FM_ERR_INVALID_SWITCH;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    port        = ntohs(imsg->port);
    networkPort = ntohs( *( (fm_uint16 *) imsg->data ) );
    host        = (fm_text) &imsg->data[2];

    if (networkPort == FM_MODEL_SOCKET_PORT_DISABLE)
    {
        /**************************************************
         * Shut down the socket on this model egress port.
         **************************************************/
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "Disabling egress port %d\n", port);

        if (type == FM_MODEL_MSG_ENABLE_ALTERNATIVE_DATA_PATH)
        {
            mpq->alternativeRxDataPathEnable = FALSE;
        }

        /* If there was a socket on this port previously, close it down. */
        if (mpq->queueState[sw]->destSocket[port].sock != -1)
        {
            fmCloseNetworkConnection(&mpq->queueState[sw]->destSocket[port]);
        }

        mpq->queueState[sw]->destSocket[port].sock = -1;
    }
    else
    {
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                     "Setting destination for egress port %d to %s:%d\n",
                     port, host, networkPort);

        if (type == FM_MODEL_MSG_ENABLE_ALTERNATIVE_DATA_PATH)
        {
            mpq->alternativeRxDataPathEnable = TRUE;
        }

        fmCreateNetworkClient(&mpq->queueState[sw]->destSocket[port],
                              FM_SOCKET_TYPE_TCP,
                              host,
                              networkPort);
    }

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end HandleMsgSetEgressInfo */




static fm_status ReceiveMessage(fm_socket *      socket,
                                fm_bool          closeSocket,
                                fm_modelMessage *imsg)
{
    fm_status status;
    fm_int32  msgLength;
    fm_int    length;
    fm_int    offset;
    fm_int    recvLength;
    fm_uint16 version;
    fm_byte * data;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "socket=%p imsg=%p\n",
                 (void *) socket,
                 (void *) imsg);

    status = fmReceiveNetworkData(socket,
                                  &imsg->msgLength,
                                  FM_MODEL_MSG_LENGTH_SIZE,
                                  &recvLength);
    if ( closeSocket && ( status == FM_ERR_NO_MORE ) )
    {
        /* The connection has been closed. */
        socket->type = FM_SOCKET_TYPE_CLOSED;
    }
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    if ( recvLength != ( (fm_int) FM_MODEL_MSG_LENGTH_SIZE ) )
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Message corruption detected: Expected %zd bytes, received %d bytes\n",
                     FM_MODEL_MSG_LENGTH_SIZE,
                     recvLength);

        status = FM_FAIL;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    msgLength = ntohl(imsg->msgLength);
    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                 "Received message length of %d bytes\n",
                 msgLength);

    status = fmReceiveNetworkData(socket,
                                  &imsg->version,
                                  FM_MODEL_MSG_VERSION_SIZE,
                                  &recvLength);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    if ( recvLength != ( (fm_int) FM_MODEL_MSG_VERSION_SIZE ) )
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Message corruption detected: Expected %zd bytes, received %d bytes\n",
                     FM_MODEL_MSG_VERSION_SIZE,
                     recvLength);

        status = FM_FAIL;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    version = ntohs(imsg->version);
    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                 "Received message version of %d\n",
                 version);

    if ( version != ( (fm_uint16) FM_MODEL_MSG_VERSION ) )
    {
        FM_LOG_WARNING(FM_LOG_CAT_PLATFORM,
                       "%hu: Unsupported message version; Ignoring message\n",
                       version);

        status = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    offset = ( (fm_int) FM_MODEL_MSG_LENGTH_SIZE ) +
             ( (fm_int) FM_MODEL_MSG_VERSION_SIZE );
    data   = ( (fm_byte *) imsg ) + offset;
    length = msgLength - offset;

    status = fmReceiveNetworkData(socket, data, length, &recvLength);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    if ( recvLength != ( (fm_int) length ) )
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Message corruption detected: Expected %d bytes, received %d bytes\n",
                     length,
                     recvLength);

        status = FM_FAIL;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end ReceiveMessage */




static fm_status SendMessage(fm_socket *      socket,
                             fm_modelMessage *emsg,
                             fm_int32         msgLength)
{
    fm_status status;
    fm_int32  dataLength;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "socket=%p emsg=%p msgLength=%d\n",
                 (void *) socket,
                 (void *) emsg,
                 msgLength);

    status = fmSendNetworkData(socket, emsg, msgLength);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end SendMessage */




/*****************************************************************************/
/** TimestampsEnabled
 * \ingroup intPlatformCommon
 *
 * \desc            Determines whether timestamps are enabled for the
 *                  specified ingress port.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the logical port number.
 *
 * \return          TRUE if timestamps are enabled, FALSE otherwise.
 *
 *****************************************************************************/
static fm_bool TimestampsEnabled(fm_int sw, fm_int port)
{
    fm_status   err;
    fm_bool     isEnabled;

    err = fmGetPortAttribute(sw,
                             port,
                             FM_PORT_TIMESTAMP_GENERATION,
                             (void *) &isEnabled);

#if 0
    if (err != FM_OK)
    {
        FM_LOG_PRINT("TimestampsEnabled: "
                     "fmGetPortAttribute(%d,%d): %s\n",
                     sw,
                     port,
                     fmErrorMsg(err));
    }
#endif

    return (err == FM_OK) && isEnabled;

}   /* end TimestampsEnabled */




/*****************************************************************************/
/** ModelForwardPacketToAPI
 * \ingroup intModelPktQ
 *
 * \desc            Converts packet data received from the model on the CPU
 *                  port into an ''fm_buffer'' chain that is then forwarded
 *                  to the API.
 *
 * \param[in]       sw is the switch number of the model from which the
 *                  packet was received.
 *
 * \param[out]      packet points to the packet byte data.
 *
 * \param[in]       length is the number of bytes in packet.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL if unable to create ''fm_buffer'' chain or if
 *                  unable to allocate an event buffer for the packet.
 * \return          FM_ERR_INVALID_PORT if unable to identify the source port
 *                  from the source glort.
 *
 *****************************************************************************/
static fm_status ModelForwardPacketToAPI(fm_int   sw,
                                         fm_byte *data,
                                         fm_int   dataLength)
{
    fm_status            err;
    fm_switch *          switchPtr;
    fm_eventPktRecv *    recvEvent;
    fm_byte             *packet;
    fm_int               length;
    fm_modelSidebandData sbData;
    fm_buffer *          first = NULL;
    fm_buffer *          current;
    fm_buffer *          previous = NULL;
    fm_int               n;
    fm_int               l;
    fm_modelPacketQueue *mpq;
    fm_int               offset;
    fm_event *           event;

#if 0
    fm_uint16            srcGlort;
    fm_uint16            dstGlort;

    fm_int               fType;
    fm_uint32            tagWord;
    fm_bool              isPktSFlowLogged;
    fm_int               vlan;
    fm_uint16            pvid;
    fm_bool              addVlan;
    fm_bool              dropBpdu;
    fm_uint32            fcsVal;
#endif

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d, data=%p, dataLength=%d\n",
                 sw,
                 (void *) data,
                 dataLength);

    switchPtr = GET_SWITCH_PTR(sw);

    mpq = (fm_modelPacketQueue *) fmRootPlatform->packetQueue;

    DecodeTlvPacket(data, dataLength, &packet, &length, &sbData);

    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                 "TB: Sending packet of length %d bytes to API\n",
                 length);

    /* Create a buffer chain */
    first  = NULL;
    offset = 0;
    n      = length;

    /* Loop while there is data */
    while (n)
    {
        current = fmAllocateBuffer(sw);

        if (current == NULL)
        {
            break;
        }

        l = FM_BUFFER_SIZE_BYTES;

        if (l > n)
        {
            l = n;
        }

        FM_MEMCPY_S(current->data, l, packet + offset, l);

        n            -= l;
        offset       += l;
        current->len  = l;
        current->next = NULL;

        if (first == NULL)
        {
            first = current;
        }
        else
        {
            previous->next = current;
        }

        previous = current;
    }

    /* If failed to create the buffer chain, then exit with error */
    if (n != 0)
    {
        if (first != NULL)
        {
            fmFreeBufferChain(sw, first);
        }

        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_FAIL);
    }

    if (first == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_FAIL);
    }

    /* Get an event, exit if couldn't have one */
    event = fmAllocateEvent(sw,
                            FM_EVID_HIGH_PKT_RECV,
                            FM_EVENT_PKT_RECV,
                            FM_EVENT_PRIORITY_LOW);

    if (event == NULL)
    {
        fmFreeBufferChain(sw, first);
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "unable to allocate an event\n");
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_FAIL);
    }

    /* Fill up the event and forward to API */
    recvEvent = &event->info.fpPktEvent;

    /* Save the switch number */
    recvEvent->switchNum = sw;

    /* Set buffer */
    recvEvent->pkt = first;

#if 0
    /* Store the ISL tag */
    recvEvent->ISLTag[0] = F64Tag[0];
    recvEvent->ISLTag[1] = F64Tag[1];

    /* Store the VLAN, priority and trap code */
    fType           = (F64Tag[0] >> 30 ) & 0x3;
    /* user */
    vlan            = F64Tag[0] & 0xfff;
    recvEvent->vlan = vlan;

    /* priority in fm_eventPktRecv struct is documented to
     * contain internal switch priority
     */
    recvEvent->priority     = (F64Tag[0] >> 24) & 0xf;
    recvEvent->vlanPriority = (F64Tag[0] >> 13) & 0x7;
    recvEvent->trapAction   = F64Tag[1] & 0xff;

    /* Validate received port */
    srcGlort = (F64Tag[1] >> 16) & 0xffff;
    dstGlort = F64Tag[1] & 0xffff;


    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                 "TB: received F64 tag: fType=%d "
                 "VLAN=0x%x VPRI=%d action=%d SGLORT=0x%x DGLORT=0x%x\n",
                 fType,
                 recvEvent->vlan,
                 recvEvent->vlanPriority,
                 recvEvent->trapAction,
                 srcGlort,
                 dstGlort);

    /* Translate source port to its glort */
    err = fmGetGlortLogicalPort(sw, srcGlort, &recvEvent->srcPort);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                 "TB: Generating packet event for packet from port %d\n",
                 recvEvent->srcPort);

    if (vlan)
    {
        recvEvent->vlanEtherType = 0x8100;
    }
    else
    {
        recvEvent->vlanEtherType = 0;
        /* for an untagged frame use the pvid of the ingress port */
        fmGetPortDefVlanInt(sw, recvEvent->srcPort, &pvid);
        vlan = pvid;
    }

    /* Add vlan to the packet if the CPU port tagging is enabled */
    err = mpq->pktQueueServices.GetCpuVlanTag(sw, vlan, &addVlan);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

    if ( (fType == FM_FTYPE_SPECIAL_DELIVERY) || !addVlan )
    {
        /******************************************
         * Remove the ISL Tag.
         ******************************************/
        first->data[4] = first->data[2];
        first->data[3] = first->data[1];
        first->data[2] = first->data[0];
        first->data   += 2;
        first->len    -= 8;
    }
    else
    {
        /******************************************
         * Add a VLAN tag and remove the ISL Tag.
         ******************************************/
        tagWord  = 0;
        tagWord |= (0x8100 << 16);
        tagWord |= F64Tag[0] & 0xffff;

        first->data[4] = htonl(tagWord);
        first->data[3] = first->data[2];
        first->data[2] = first->data[1];
        first->data[1] = first->data[0];
        first->data   += 1;
        first->len    -= 4;
    }

    /* Check if BPDU packets are to be dropped silently. */
    err = mpq->pktQueueServices.CheckBpduDropping(sw,
                                                  recvEvent,
                                                  vlan,
                                                  &dropBpdu);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

    if (dropBpdu)
    {
        fmFreeBufferChain(sw, first);
        fmReleaseEvent(event);

        return FM_FAIL;
    }

    /* Check if the packet is logged by a sFlow instance */
    err = fmCheckSFlowLogging(sw, recvEvent, &isPktSFlowLogged);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

    if (isPktSFlowLogged)
    {
        /* Change the type to FM_EVENT_SFLOW_PKT_RECV */
        event->type = FM_EVENT_SFLOW_PKT_RECV;
    }

    /*********************************************************
     * Retrieve ingress timestamp from the FCS field of the
     * buffer, and convert it to a precision timestamp.
     ********************************************************/
    if ( switchPtr->ConvertTimestamp &&
         TimestampsEnabled(sw, recvEvent->srcPort) )
    {
        fcsVal = fmPacketGetCRC(first);

        recvEvent->rawIngressTime = ((fcsVal >> 1) & 0x7fffff80) | (fcsVal & 0x7f);

        err = switchPtr->ConvertTimestamp(sw,
                                          recvEvent->rawIngressTime,
                                          0,
                                          &recvEvent->ingressTime);
        if (err != FM_OK && err != FM_ERR_UNSUPPORTED)
        {
            FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                         "Unable to convert ingress timestamp\n");
        }

        /* for debugging */
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                     "srcPort=%d rawIngressTime=0x%08x ingressTime=%llu %llu\n",
                     recvEvent->srcPort,
                     recvEvent->rawIngressTime,
                     recvEvent->ingressTime.seconds,
                     recvEvent->ingressTime.scaledNanoseconds);
    }
    else
    {
        /* Timestamps not supported */
        FM_CLEAR(recvEvent->ingressTime);
        recvEvent->rawIngressTime = 0;
    }

    /***************************************************
     * The CRC is incorrect since it includes the ISL
     * tags we removed. Making it all zero so the user
     * knows that the CRC should not be used.
     **************************************************/
    fmPacketClearCRC(first);
#endif

    recvEvent->srcPort = sbData.pktMeta[18] & 0x1F; //FIXME

    /* Send event to API */
    err = fmSendThreadEvent(&fmRootApi->eventThread, event);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

ABORT:

    if (first != NULL)
    {
        fmFreeBufferChain(sw, first);
    }

    fmReleaseEvent(event);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end ModelForwardPacketToAPI */




/*****************************************************************************/
/** MapLogicalPortToPhysical
 * \ingroup intModelPktQ
 *
 * \desc            Maps the logical port number used by the application
 *                  (actually a cardinal port index) to the corresponding
 *                  physical port number in the model.
 *
 * \param[in]       sw is the logical switch number.
 *
 * \param[in]       logPort is the logical port number.
 *
 * \param[out]      physPort points to a location in which the physical
 *                  port number should be stored.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status MapLogicalPortToPhysical(fm_int   sw,
                                          fm_int   logPort,
                                          fm_int * physPort)
{

    FM_NOT_USED(sw);

    /* FIXME why no tempPort and tempSwitch as in MapPhysicalPortToLogical? */

#if !defined(MAP_CARDINAL_PORTS)
    fm_int      physSwitch;
#endif
    fm_status   status;

#if defined(MAP_CARDINAL_PORTS)

    /* FIXME bug 24962 override fmPlatformMapCardinalToPhysical
     * because the call results in invalid argument when cpi=56 (>47)
     * see bug 24962 comment #8
     */
    /*
    status = fmPlatformMapCardinalToPhysical(sw, logPort, physPort);
    if (status != FM_OK)
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Error mapping ingress port (sw=%d cpi=%d): %s\n",
                     sw, logPort,
                     fmErrorMsg(status));
    }
    */

    *physPort = logPort;
    status = FM_OK;

    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                 "override call to fmPlatformMapCardinalToPhysical "
                 "set physPort to %d\n", *physPort);

#else
    status = fmPlatformMapLogicalPortToPhysical(sw,
                                                logPort,
                                                &physSwitch,
                                                physPort);
    if (status != FM_OK || *physPort < 0)
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "fmPlatformMapLogicalPortToPhysical(%d, %d) "
                     "returned %d (%s)\n",
                     sw, logPort,
                     *physPort,
                     fmErrorMsg(status));
    }

    if (physSwitch != sw)
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "On ingress, physical switch %d != logical switch %d!\n",
                     sw, physSwitch);
    }
#endif

    return status;

}   /* end MapLogicalPortToPhysical */




/*****************************************************************************/
/** MapPhysicalPortToLogical
 * \ingroup intModelPktQ
 *
 * \desc            Maps the physical port number in the model to the
 *                  logical port number (actually a cardinal port index)
 *                  used by the application.
 *
 * \param[in]       sw is the logical switch number.
 *
 * \param[in]       physPort is the physical port number in the model.
 *
 * \param[out]      logPort points to a location in which the logical port
 *                  number should be stored.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status MapPhysicalPortToLogical(fm_int   sw,
                                          fm_int   physPort,
                                          fm_int * logPort)
{
#if !defined(MAP_CARDINAL_PORTS)
    fm_int      tempSwitch;
#endif
    fm_int      tempPort;
    fm_status   status;

    FM_NOT_USED(sw);

#if defined(MAP_CARDINAL_PORTS)

    /* FIXME bug 24962 override fmPlatformMapPhysicalToCardinal
     * because call results in invalid argument when cpi=56 (>47)
     * see bug 24962 comment #8
     */
    /*
    status = fmPlatformMapPhysicalToCardinal(sw, physPort, &tempPort);
    if (status != FM_OK)
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Error mapping egress port (sw=%d physPort=%d): %s\n",
                     sw, physPort,
                     fmErrorMsg(status));
    }
    */

    tempPort = physPort;
    status = FM_OK;

    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                 "override call to fmPlatformMapPhysicalToCardinal "
                 "set logPort to %d\n", tempPort);

#else
    status = fmPlatformMapPhysicalPortToLogical(sw,
                                                physPort,
                                                &tempSwitch,
                                                &tempPort);
    if (tempSwitch != sw)
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "On egress, physical switch %d != logical switch %d!\n",
                     sw, tempSwitch);
    }
#endif

    /* Only return a value if the mapping succeeds. */
    if (status == FM_OK)
    {
        *logPort = tempPort;
    }

    return status;

}   /* end MapPhysicalPortToLogical */




/*****************************************************************************/
/** ProcessMessage
 * \ingroup intModelPktQ
 *
 * \desc            Processes a received message.
 *
 * \param[in]       imsg points to the message to be processed.
 *
 * \param[in]       msgLength is the received message length in units of bytes.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if the switch number stored in the
 *                  message is invalid.
 * \return          FM_ERR_INVALID_PORT if the port number stored in the
 *                  message is invalid.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status ProcessMessage(fm_modelMessage *imsg, fm_int32 msgLength)
{
    fm_modelPacketQueue *mpq;
    fm_status            status = FM_OK;
    fm_int               numPorts;
    fm_int               port;
    fm_int               sw;
    fm_int               type;
#ifdef DEBUG_PRINT_RECEIVED_PACKET
    fm_int32             pktLength;
    fm_int               p;
#endif

    FM_LOG_ENTRY( FM_LOG_CAT_PLATFORM,
                  "imsg=%p, msgLength=%d\n",
                  (void *) imsg,
                  msgLength );

    mpq = (fm_modelPacketQueue *) fmRootPlatform->packetQueue;

    if ( msgLength <= ( (fm_int32) FM_MODEL_MSG_HEADER_SIZE ) )
    {
        /* Silent exit */
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);
    }

    /* Retrieve switch and port */
    type = ntohs(imsg->type);
    sw   = ntohs(imsg->sw);
    port = ntohs(imsg->port);

    if (msgLength > FM_MODEL_MAX_PACKET_SIZE)
    {
        FM_LOG_PRINT("TB: Length to large: sw=%d port=%d type=%d pktLength=%d\n",
                 sw, port, type, msgLength);
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);
    }

#ifdef DEBUG_PRINT_RECEIVED_PACKET
    pktLength = msgLength - FM_MODEL_MSG_HEADER_SIZE;

    FM_LOG_PRINT("TB: Received packet datagram: sw=%d port=%d type=%d pktLength=%d\n",
                 sw, port, type, pktLength);

    for (p = 0 ; p < pktLength ; p++)
    {
        FM_LOG_PRINT("%02x ", imsg->data[p]);

        if ( p && ( (p % 16) == 15 ) )
        {
            FM_LOG_PRINT("\n");
        }
    }

    FM_LOG_PRINT("\n");

#endif

    if ( ( sw < 0 ) || ( sw >= FM_MAX_NUM_FOCALPOINTS ) )
    {
        /* The switch number is invalid. Return immediately. */
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_SWITCH);
    }

    status = mpq->pktQueueServices.GetNumPorts(sw, &numPorts);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    if ( ( port < 0 ) || ( port >= numPorts ) )
    {
        /* The port number is invalid. Return immediately. */
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_PORT);
    }

    /**************************************************
     * The switch and port are valid. Call the white
     * model to process this packet and forward any
     * packets out the proper destination socket.
     **************************************************/
            
    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
        "TB: process message type=%d\n", type);

    /* Switch according to packet type received */
    switch (type)
    {
        case FM_MODEL_MSG_SET_EGRESS_INFO:
        case FM_MODEL_MSG_ENABLE_ALTERNATIVE_DATA_PATH:
            status = HandleMsgSetEgressInfo(sw, mpq, imsg, type);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            break;

        case FM_MODEL_MSG_LINK_STATE:
            status = FM_OK;
            break;

        case FM_MODEL_MSG_SWITCH_STATE:
            status = FM_OK;
            break;

        case FM_MODEL_MSG_PACKET_LOOPBACK:
            FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                "TB: process message type=FM_MODEL_MSG_PACKET_LOOPBACK\n");
        case FM_MODEL_MSG_PACKET:
            status = HandleMsgPacket(sw, mpq, imsg, port, msgLength);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            break;

        case FM_MODEL_MSG_MGMT:
            status = HandleMsgMgmt(sw, mpq, imsg, msgLength);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            break;

        case FM_MODEL_MSG_ATTR:
            status = HandleMsgAttr(sw, mpq, imsg, msgLength);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            break;

        case FM_MODEL_MSG_GET_INFO:
            status = HandleMsgGetInfo(sw, mpq, imsg, msgLength);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            break;

        default:
            FM_LOG_WARNING(FM_LOG_CAT_PLATFORM,
                           "Ignoring message with unknown type %d\n",
                           type);

    }   /* end switch (type) */

ABORT:
    if (status != FM_OK)
    {
        FM_LOG_WARNING(FM_LOG_CAT_PLATFORM,
                       "Unable to handle message: %s\n",
                       fmErrorMsg(status) );
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end ProcessMessage */




/*****************************************************************************/
/** ModelPacketQueueSend
 * \ingroup intModelPktQ
 *
 * \desc            Send a packet into the CPU port of the model.
 *
 * \param[in]       sw is the switch on which to send the packet.
 *
 * \param[in]       dGlort is the destination glort to set. If islTag is not
 *                  NULL, this parameter is ignored.
 *
 * \param[in]       fType is the frame type to set. If islTag is not NULL,
 *                  this parameter is ignored.
 *
 * \param[in]       buffer is a pointer to a chain of ''fm_buffer'' structures
 *                  containing the packet payload.
 *
 * \param[in]       info points to an ''fm_packetInfoV2'' structure that
 *                  contains additional arguments.
 * 
 * \param[in]       islTag points to an array of unsigned integers containing
 *                  the ISL tag contents that should be used instead of values
 *                  determined by this function.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_FAIL if the packet pointed to by buffer is too small
 *                  to be a valid packet.
 *
 *****************************************************************************/
static fm_status ModelPacketQueueSend(fm_int           sw,
                                      fm_uint32        dGlort,
                                      fm_int           fType,
                                      fm_buffer *      buffer,
                                      fm_packetInfoV2 *info,
                                      fm_uint32 *      islTag)
{
    fm_modelPacketQueue *mpq;
    fm_modelMessage      packet;
    fm_int32             length;
    fm_int32             msgLength;
    fm_buffer *          current = buffer;
    fm_int               n;
    fm_uint32            vTag;
    fm_byte *            t;
    fm_port *            cpuPortPtr;
    fm_int               cpuPort;
    fm_status            status;
    fm_uint32            bufferTmpLen = buffer->len;
    fm_uint32 *          bufferTmpData = buffer->data;
    fm_uint16            switchPri;
    fm_uint32            fcs;
    fm_byte *            data;
    fm_int               pktOff;
    fm_int               offset;
    fm_int               metaLen;

    FM_LOG_ENTRY( FM_LOG_CAT_PLATFORM,
                  "sw=%d dGlort=0x%x fType=%d buffer=%p info=%p fcs=0x%08x "
                  "islTag=%p\n",
                  sw,
                  dGlort,
                  fType,
                  (void *) buffer,
                  (void *) info,
                  info->fcsValue,
                  (void *) islTag );
    
    mpq = (fm_modelPacketQueue *) fmRootPlatform->packetQueue;

    /* Grab CPU port's default VLAN */
    status = fmGetCpuPort(sw, &cpuPort);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    cpuPortPtr  = GET_PORT_PTR(sw, cpuPort);

    /* Write switch and cpu port */
    packet.version = htons(FM_MODEL_MSG_VERSION);
    packet.type    = htons(FM_MODEL_MSG_PACKET);
    packet.sw      = htons(sw);
    packet.port    = htons(cpuPort);

    /* Packet meta TLV */
    offset               = 0;
    packet.data[offset]  = FM_MODEL_PACKET_META;
    *( (fm_int32 *) &packet.data[offset + FM_MODEL_DATA_TYPE_SIZE] ) =
                                htonl(FM_MODEL_CPK_META_SIZE);
    offset              += FM_MODEL_MSG_TLV_SIZE;
    memset(&packet.data[offset], 0, FM_MODEL_CPK_META_SIZE);
    packet.data[offset]  = FM_MODEL_META_TYPE_LAN_TX; /* Type from CPK to HLP */
    /* Dest. Port */
    FM_ARRAY_SET_UNNAMED_FIELD((fm_uint32*)&packet.data[offset], 16, 5, dGlort);
    /* Fwd */
    FM_ARRAY_SET_UNNAMED_FIELD((fm_uint32*)&packet.data[offset], 22, 2, 0);
    /* PF */
    FM_ARRAY_SET_UNNAMED_FIELD((fm_uint32*)&packet.data[offset], 31, 1, 1);
    offset              += FM_MODEL_CPK_META_SIZE;
    metaLen              = offset;

    /* Packet data TLV */
    pktOff = offset;
    packet.data[offset]   = FM_MODEL_DATA_PACKET;
    offset               += FM_MODEL_MSG_TLV_SIZE;

    /* Copy first 12 bytes (DMAC, SMAC) */
    data = &packet.data[offset];
    length = 0;

    if (bufferTmpLen <= 12)
    {
        /* Odd case, the first packet is very small, walk slowly */
        while (length != 12)
        {
            t = (fm_byte *) current->data;
            n = 0;

            while (length != 12 && n < current->len)
            {
                data[length++] = *t++;
                n++;
            }

            current = current->next;

            if (current == NULL)
            {
                break;
            }
        }

        /* Bad case, the total packet is tiny ! */
        if (length != 12)
        {
            fmFreeBufferChain(sw, buffer);

            FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_FAIL);
        }
    }
    else
    {
        /* Normal case, vroooom ! */
        FM_MEMCPY_S(&data[length], 12, bufferTmpData, 12);
        length        += 12;
        bufferTmpData += 3;
        bufferTmpLen  -= 12;
    }

    vTag = htonl(bufferTmpData[0]);
    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "Word 3: 0x%08x\n", vTag);

#if 0
    if (islTag == NULL)
    {
        if (info->switchPriority == FM_USE_VLAN_PRIORITY)
        {
            switchPri = 0;
        }
        else
        {
            switchPri = info->switchPriority & 0xf;
        }

        /* Add F64 tag */
        if (fType == FM_FTYPE_SPECIAL_DELIVERY)
        {
            data[length++] = 0x80 | switchPri; /* directed */
        }
        else
        {
            data[length++] = 0x00 | switchPri; /* normal */
        }
        data[length++] = 0;           /* user bits */

        /* Grab vlan tag if 0x8100 tagged */
        /* FIXME: this should pull the vlan A ethertype value */
        if ((vTag & 0xffff0000) == 0x81000000)
        {
            data[length++] = (vTag >> 8) & 0xff;
            data[length++] = vTag & 0xff;

            if (fType == FM_FTYPE_NORMAL)
            {
                /* The VLAN tag is removed from the frame */
                bufferTmpData += 1;
                bufferTmpLen  -= 4;
            }
        }
        else
        {
            /* This is an untagged frame. clear VID/VPRI bits */
            data[length++] = 0;
            data[length++] = 0;
        }

        data[length++] = (cpuPortPtr->glort >> 8) & 0xff;
        data[length++] = cpuPortPtr->glort & 0xff;
        data[length++] = dGlort >> 8;
        data[length++] = dGlort;
    }
    else
    {
        data[length++] = (islTag[0] >> 24) & 0xFF;
        data[length++] = (islTag[0] >> 16) & 0xFF;
        data[length++] = (islTag[0] >> 8) & 0xFF;
        data[length++] = (islTag[0] >> 0) & 0xFF;
        data[length++] = (islTag[1] >> 24) & 0xFF;
        data[length++] = (islTag[1] >> 16) & 0xFF;
        data[length++] = (islTag[1] >> 8) & 0xFF;
        data[length++] = (islTag[1] >> 0) & 0xFF;
    }
#endif

    /* Copy the rest */
    while (current != NULL)
    {
        /* Check we won't overload the packet */
        if (length + bufferTmpLen + 4 > FM_MODEL_MAX_PACKET_SIZE)
        {
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                         "The whiteModel's maximum packet size is limited "
                         "to %d, actual frame size is %d\n",
                         FM_MODEL_MAX_PACKET_SIZE,
                         length + bufferTmpLen + 4);
            FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_FRAME_TOO_LARGE);
        }

        /* Copy the rest */
        FM_MEMCPY_S(&data[length],
                    bufferTmpLen,
                    bufferTmpData,
                    bufferTmpLen);
        length += bufferTmpLen;
        current = current->next;

        if (current != NULL)
        {
            bufferTmpData = current->data;
            bufferTmpLen  = current->len;
        }
    }

    if ( (length + 4) > FM_MODEL_MAX_PACKET_SIZE )
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                     "The whiteModel's maximum packet size is limited "
                     "to %d, actual frame size is %d\n",
                     FM_MODEL_MAX_PACKET_SIZE,
                     length + 4);
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_FRAME_TOO_LARGE);
    }

    info->fcsValue = htonl(fmCrc32(data, length));

    switch (info->fcsMode)
    {
        case FM_FCS_MODE_DEFAULT:
        case FM_FCS_MODE_VALUE:
            fcs = htonl(info->fcsValue);
            break;

        case FM_FCS_MODE_ZERO:
            fcs = 0;
            break;

        case FM_FCS_MODE_TIMESTAMP:
            fcs = htonl(0x00000080);
            break;

        default:
            FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, FM_ERR_INVALID_ARGUMENT);
    }
    /* Append user-specified FCS to buffer */
    FM_MEMCPY_S(&data[length], 4, &fcs, sizeof(fcs));
    length += 4;

    /* Fill in the final length */
    *( (fm_int32 *) &packet.data[pktOff+FM_MODEL_DATA_TYPE_SIZE] ) = htonl(length);
    msgLength = FM_MODEL_MSG_HEADER_SIZE + metaLen + FM_MODEL_MSG_TLV_SIZE + length;
    packet.msgLength = htonl(msgLength);

    /* Then send to the server socket and free the buffer */
    status = SendMessage(&mpq->localSocket, &packet, msgLength);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end ModelPacketQueueSend */




/*****************************************************************************/
/** fmModelPacketQueueTask
 * \ingroup intModelPktQ
 *
 * \desc            Socket listener task that forwards packets to the model
 *                  instances.
 *                                                                      \lb\lb
 *                  After initializing itself, this task performs the
 *                  following steps in a loop:
 *                                                                      \lb\lb
 *                  1.  Check the packet queue for packets that have egressed
 *                      the chip (placed in the queue in step 4):
 *                          - If a socket has been opened for the egress
 *                            port (the FM_MODEL_MSG_SET_EGRESS_INFO or
 *                            FM_MODEL_MSG_ENABLE_ALTERNATIVE_DATA_PATH case in
 *                            step 2), send the packet on through that socket
 *                            to the listening application.
 *                          - Otherwise, if the packet egressed on port 0,
 *                            send it on to the API.
 *                                                                      \lb\lb
 *                  2.  If a message is waiting on the server socket, receive
 *                      it from the socket and handle it. Possibilities are:
 *                          - FM_MODEL_MSG_SET_EGRESS_INFO: Create socket for
 *                            switch egressing packets to be sent to.
 *                          - FM_MODEL_MSG_LINK_STATE: Set link state.
 *                          - FM_MODEL_MSG_SWITCH_STATE: Set switch state.
 *                          - FM_MODEL_MSG_ENABLE_ALTERNATIVE_DATA_PATH: Set
 *                            Alternate path for CPU-bound packets.
 *                          - FM_MODEL_MSG_PACKET: Process packet from
 *                            application being sent to white model.
 *                                                                      \lb\lb
 *                  3.  For the FM_MODEL_MSG_PACKET case in step 2, call
 *                      ModelSendPacket to pass packet into the model.
 *                                                                      \lb\lb
 *                  4.  Following step 3, call ModelReceivePacket to
 *                      pull an egressing packet out of the model and put it
 *                      into the pakcet queue.
 *
 * \param[out]      args is the standard thread argument.
 *
 *
 * \return          Unused result.
 *
 *****************************************************************************/
static void *fmModelPacketQueueTask(void *args)
{
    fm_modelPacketQueue *mpq = NULL;
    fm_modelMessage *    emsg = NULL;
    fm_modelMessage      imsg;
    fm_socket *          sockets[MAX_PERSISTENT_CONNECTIONS + 1];
    fm_thread *          thread;
    fm_timestamp         timeout;
    fm_status            status;
    fm_int32             msgLength;
    fm_int32             pktLength;
    fm_uint16            version;
    fm_int               eventsReceived[MAX_PERSISTENT_CONNECTIONS + 1];
    fm_int               i;
    fm_int               numSockets = 1;
    fm_int               port;
    fm_int               sw = 0;
    fm_int               type;
    fm_bool              dataPresent;

    thread = FM_GET_THREAD_HANDLE(args);
    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "thread = %s\n", thread->name);

    status = InitializeSocketInfoFile();

    if (status != FM_OK)
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Unable to create socket info file!\n");

        return NULL;
    }

    mpq = (fm_modelPacketQueue *) fmRootPlatform->packetQueue;

    fmCreateNetworkServer(&mpq->serverSocket,
                          FM_SOCKET_TYPE_TCP,
                          0,
                          3);

    status = AddSocketInfoToFile("localhost", mpq->serverSocket.serverPort);

    if (status != FM_OK)
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Unable to add socket info to file!\n");

        return NULL;
    }

    fmCreateNetworkClient(&mpq->localSocket,
                          FM_SOCKET_TYPE_TCP,
                          "localhost",
                          mpq->serverSocket.serverPort);

    /***************************************************
     * Prepare for connections
     **************************************************/

    for ( i = 0 ; i < MAX_PERSISTENT_CONNECTIONS ; i++ )
    {
        sockets[i+1] = fmAlloc(sizeof(fm_socket));

        if (!sockets[i+1])
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "Unable to allocate memory for socket structure\n");

            return NULL;
        }
    }

    /* Initially just the server */
    sockets[0] = &mpq->serverSocket;
    numSockets = 1;

    /**************************************************
     * Process messages forever
     **************************************************/

    while (TRUE)
    {
        /**************************************************
         * Step 1:
         * Forward messages in queue to destination sockets
         * if there are messages to forward.
         **************************************************/

        if (mpq->msgQueueHead != mpq->msgQueueTail)
        {
            /* Limit the quantity within reason. This is to avoid deadlocks */
            fm_int n = 0;

            while ( ( n <= mpq->msgQueueOutputLimit ) &&
                    ( mpq->msgQueueHead != mpq->msgQueueTail ) )
            {
                /* Point to message at the head of the queue */
                FM_MODEL_PKTQ_GET_HEAD(mpq, &emsg, &msgLength);
                pktLength = msgLength - FM_MODEL_MSG_HEADER_SIZE;

                version = ntohs(emsg->version);
                if ( version != ( (fm_uint16) FM_MODEL_MSG_VERSION ) )
                {
                    FM_LOG_WARNING(FM_LOG_CAT_PLATFORM,
                                   "%d: Unsupported message version; Ignoring message\n",
                                   version);
                    goto FWD_NEXT;
                }

                /* Process the message */
                type    = ntohs(emsg->type);
                sw      = ntohs(emsg->sw);
                port    = ntohs(emsg->port);

                /* If CPU port (we assume that logical port  0 is CPU) and
                 * there is no destination socket, forward to API. */
                if ( ( port == 0 ) &&
                     ( type == FM_MODEL_MSG_PACKET ) &&
                     ( mpq->alternativeRxDataPathEnable == FALSE ) )
                {
                    (void)ModelForwardPacketToAPI(sw, emsg->data, pktLength);
                }
                /* If we need to requeue this packet into the server socket */
                else if (type == FM_MODEL_MSG_PACKET_LOOPBACK)
                {
                    SendMessage(&mpq->localSocket, emsg, msgLength);
                }
                /* If there is a destination socket, then forward */
                else if ( ( mpq->queueState[sw] != NULL ) &&
                          ( mpq->queueState[sw]->destSocket[port].sock >= 0 ) )
                {
                    SendMessage(&mpq->queueState[sw]->destSocket[port],
                                emsg,
                                msgLength);
                }

FWD_NEXT:
                /* Advance the head pointer (round it to next boundary) */
                FM_MODEL_PKTQ_UPDATE_HEAD(mpq, msgLength);

                /* Decrease credit */
                n += msgLength;

            }   /* end while (n <= mpq->msgQueueOutputLimit &&... */

        }   /* end if (mpq->msgQueueHead != mpq->msgQueueTail) */

        /**************************************************
         * Step 2:
         * Process incoming messages. If the message queue
         * to destination sockets isn't empty, then we will
         * only peek at the incoming socket and not block
         * on it as we do have other things to do!
         **************************************************/

        while (TRUE)
        {
            dataPresent = FALSE;

            timeout.sec  = 0;
            timeout.usec = FM_FDS_POLL_TIMEOUT_USEC;

            status = fmWaitForNetworkEvent(sockets,
                                           &numSockets,
                                           MAX_PERSISTENT_CONNECTIONS + 1,
                                           eventsReceived,
                                           &timeout);
            FM_LOG_ASSERT(FM_LOG_CAT_PLATFORM,
                          status == FM_OK,
                          "Network corruption detected: %s\n",
                          fmErrorMsg(status));

            for ( i = 0 ; i < numSockets ; i++ )
            {
                if (eventsReceived[i] == FM_NETWORK_EVENT_DATA_AVAILABLE)
                {
                    dataPresent = TRUE;
                }
            }

            if (!dataPresent)
            {
                /***************************************************
                 * No data available but there are packets to
                 * forward, so break to the main loop.
                 **************************************************/
                if (mpq->msgQueueHead != mpq->msgQueueTail)
                {
                    break;
                }

                /***************************************************
                 * No data available, no packets to forward so go wait
                 * for a message forever.
                 **************************************************/
                else
                {
                    continue;
                }
            }

            /**************************************************
             * Process actual messages
             **************************************************/

            for (i = 0 ; i < numSockets ; i++)
            {
                if (eventsReceived[i] != FM_NETWORK_EVENT_DATA_AVAILABLE)
                {
                    continue;
                }

                FM_CLEAR(imsg);

                if (ReceiveMessage(sockets[i], TRUE, &imsg) != FM_OK)
                {
                    continue;
                }

                msgLength = ntohl(imsg.msgLength);
                (void)ProcessMessage(&imsg, msgLength);
            }

        }   /* end while (TRUE) [Process incoming messages] */

    }   /* end while (TRUE) [Main task loop] */

    return NULL;

}   /* end fmModelPacketQueueTask */




/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** fmModelPacketQueueInitialize
 * \ingroup modelPktQ
 *
 * \desc            Initialize the white model port packet queue interface.
 *                  If the packet queue interface is to be used, this function
 *                  should be called by the platform layer at initialization
 *                  time.
 *
 * \note            This function should be called only once, regardless of
 *                  the number of chips to be instantiated
 *                  (''fmModelPacketQueueAddSwitch'' is used to add each chip
 *                  instance to the packet queue interface).
 *
 * \param[out]      statePtr is the address of a pointer to void which will be
 *                  filled in by this function with the address of a packet
 *                  queue object. The object will be allocated by this function
 *                  and must be provided by the caller in subsequent calls to
 *                  ''fmModelPacketQueueAddSwitch'' as an opaque handle.
 *
 * \param[in]       funcInfo is the pointer to the structure which has function
 *                  pointers refering to platform specific functions.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmModelPacketQueueInitialize(void **statePtr,
                                       fm_modelPacketQueueServices *funcInfo)
{
    fm_modelPacketQueue *state;
    fm_status            err = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "statePtr=%p\n", (void *) statePtr);

    state = fmAlloc(sizeof(fm_modelPacketQueue));

    if (state == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_NO_MEM);
    }

    FM_CLEAR(*state);

    state->msgQueueOutputLimit = 32;
    state->pktQueueServices = *funcInfo;

    *statePtr = state;

    /* create thread to listen to sockets */
    err = fmCreateThread("model_pkt_queue_listener",
                         FM_EVENT_QUEUE_SIZE_NONE,
                         &fmModelPacketQueueTask,
                         0,
                         &state->queueProcessorHandle);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end fmModelPacketQueueInitialize */




/*****************************************************************************/
/** fmModelPacketQueueAddSwitch
 * \ingroup modelPktQ
 *
 * \desc            Allocate switch state for a white model instance and add
 *                  it to the white model packet queue interface.
 *
 * \param[in]       statePtr points to the packet queue object returned in a
 *                  prior call to ''fmModelPacketQueueInitialize''.
 *
 * \param[in]       sw is the switch number of the model to add.
 *
 * \param[in]       numPorts is the number of supported physical ports on the
 *                  switch model.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_NO_MEM if there is no memory available for allocating
 *                  switch state.
 *
 *****************************************************************************/
fm_status fmModelPacketQueueAddSwitch(void * statePtr,
                                      fm_int sw,
                                      fm_int numPorts)
{
    fm_modelPacketQueueSwitch *pqs;
    fm_modelPacketQueue *      state;
    fm_int                     i;

    state = (fm_modelPacketQueue *) statePtr;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "sw=%d, numPorts=%d\n", sw, numPorts);

    if (sw >= FM_MAX_NUM_FOCALPOINTS)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_SWITCH);
    }

    state->queueState[sw] = fmAlloc(sizeof(fm_modelPacketQueueSwitch));
    pqs                   = state->queueState[sw];
    FM_CLEAR(*pqs);

    pqs->destSocket       = fmAlloc(sizeof(fm_socket) * numPorts);

    pqs->packetReceived = fmAlloc(sizeof(fm_int) * numPorts);
    pqs->packetDropped  = fmAlloc(sizeof(fm_int) * numPorts);
    pqs->packetSent     = fmAlloc(sizeof(fm_int) * numPorts);

    if (!pqs->destSocket ||
        !pqs->packetReceived ||
        !pqs->packetDropped ||
        !pqs->packetSent)
    {
        if (pqs->destSocket)
        {
            fmFree(pqs->destSocket);
        }
        if (pqs->packetReceived)
        {
            fmFree(pqs->packetReceived);
        }
        if (pqs->packetDropped)
        {
            fmFree(pqs->packetDropped);
        }
        if (pqs->packetSent)
        {
            fmFree(pqs->packetSent);
        }
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_NO_MEM);
    }

    for ( i = 0 ; i < numPorts ; i++ )
    {
        pqs->destSocket[i].sock = -1;
        pqs->packetReceived[i]  = 0;
        pqs->packetDropped[i]   = 0;
        pqs->packetSent[i]      = 0;
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);

}   /* end fmModelPacketQueueAddSwitch */




/*****************************************************************************/
/** fmModelPacketQueueSetAttribute
 * \ingroup modelPktQ
 *
 * \desc            Set an attribute for the white model packet queue interface.
 *
 * \param[in]       attr is the interface attribute to set. See
 *                  ''White Model Packet Queue Interface Attributes''.
 *
 * \param[in]       value points to the attribute value to set.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ATTRIB if attr is not recognized.
 *
 *****************************************************************************/
fm_status fmModelPacketQueueSetAttribute(fm_uint32 attr, void *value)
{
    fm_modelPacketQueue *mpq;
    fm_status            err = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "value=%p\n", (void *) value);

    mpq = (fm_modelPacketQueue *) fmRootPlatform->packetQueue;

    switch (attr)
    {
        case FM_MODEL_PACKET_QUEUE_EGRESS_LIMIT:
            mpq->msgQueueOutputLimit = *((fm_uint32 *) value);
            break;

        case FM_MODEL_PACKET_QUEUE_TOPOLOGY:
            err = BuildTopology((fm_text) value);
            break;

        case FM_MODEL_PACKET_QUEUE_SEND_EOT:
            mpq->sendEOT = *((fm_bool *) value);
            break;

        default:
            err = FM_ERR_INVALID_ATTRIB;
            break;
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end fmModelPacketQueueSetAttribute */




/*****************************************************************************/
/** fmModelPacketQueueSendDirected
 * \ingroup modelPktQ
 *
 * \desc            Send a packet into the CPU port of the model in
 *                  directed mode through the white model packet queue interface.
 *                  The packet will be sent directly to the specified list of
 *                  ports without modification of packet contents.
 *
 * \param[in]       sw is the switch on which to send the packet.
 *
 * \param[in]       portList points to an array of logical port numbers the
 *                  switch is to send the packet to.
 *
 * \param[in]       numPorts is the number of ports in portList.
 *
 * \param[in]       buffer is a pointer to a chain of ''fm_buffer'' structures
 *                  containing the packet payload.
 *
 * \param[in]       info points to an ''fm_packetInfoV2'' structure that
 *                  contains additional arguments.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_FAIL if the packet pointed to by buffer is too small
 *                  to be a valid packet.
 *
 *****************************************************************************/
fm_status fmModelPacketQueueSendDirected(fm_int           sw,
                                         fm_int *         portList,
                                         fm_int           numPorts,
                                         fm_buffer *      buffer,
                                         fm_packetInfoV2 *info)
{
    fm_int    port;
    fm_uint32 dglort;
    fm_status status;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d portList=%p numPorts=%d buffer=%p\n",
                 sw,
                 (void *) portList,
                 numPorts,
                 (void *) buffer);

    /* Send to all ports requested */
    while (numPorts--)
    {
        /* get the final port and derive dglort */
        port = *portList++;

//        fmGetLogicalPortGlort(sw, port, &dglort);
dglort = port;
        status = ModelPacketQueueSend(sw,
                                      dglort,
                                      FM_FTYPE_SPECIAL_DELIVERY,
                                      buffer,
                                      info,
                                      NULL);
        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    fmFreeBufferChain(sw, buffer);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);

}   /* end fmModelPacketQueueSendDirected */




/*****************************************************************************/
/** fmModelPacketQueueSendSwitched
 * \ingroup modelPktQ
 *
 * \desc            Send a packet into the CPU port of the model in
 *                  switched mode through the white model packet queue interface.
 *                  The packet will switched and potentially modified (i.e.
 *                  routed) according to its contents and the configuration
 *                  of the switch.
 *
 * \param[in]       sw is the switch on which to send the packet.
 *
 * \param[in]       buffer is a pointer to a chain of ''fm_buffer'' structures
 *                  containing the payload.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL if the packet pointed to by buffer is too small
 *                  to be a valid packet.
 *
 *****************************************************************************/
fm_status fmModelPacketQueueSendSwitched(fm_int sw, fm_buffer *buffer)
{
    fm_status status;
    fm_packetInfoV2 info;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "sw=%d buffer=%p\n", sw, (void *) buffer);

    FM_CLEAR(info);
    info.switchPriority = FM_USE_VLAN_PRIORITY;

    status = ModelPacketQueueSend(sw,
                                  0,
                                  FM_FTYPE_NORMAL,
                                  buffer,
                                  &info,
                                  NULL);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    fmFreeBufferChain(sw, buffer);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);

}   /* end fmModelPacketQueueSendSwitched */




/*****************************************************************************/
/** fmModelPacketQueueSendISL
 * \ingroup modelPktQ
 *
 * \desc            Send a packet into the CPU port of the model in
 *                  switched mode through the white model packet queue interface.
 *                  The packet will switched and potentially modified (i.e.
 *                  routed) according to its contents and the configuration
 *                  of the switch.
 *
 * \param[in]       sw is the switch on which to send the packet.
 *
 * \param[in]       buffer is a pointer to a chain of ''fm_buffer'' structures
 *                  containing the payload.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL if the packet pointed to by buffer is too small
 *                  to be a valid packet.
 *
 *****************************************************************************/
fm_status fmModelPacketQueueSendISL(fm_int          sw,
                                    fm_islTagFormat islTagFormat,
                                    fm_uint32 *     islTag,
                                    fm_buffer *     buffer)
{
    fm_status status;
    fm_packetInfoV2 info;

    FM_LOG_ENTRY( FM_LOG_CAT_PLATFORM,
                  "sw=%d buffer=%p\n",
                  sw,
                  (void *) buffer );

    FM_CLEAR(info);

    switch (islTagFormat)
    {
        case FM_ISL_TAG_F56:
        case FM_ISL_TAG_F64:
            break;

        default:
            FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    status = ModelPacketQueueSend(sw,
                                  0,
                                  FM_FTYPE_NORMAL,  /* Not used */
                                  buffer,
                                  &info,
                                  islTag);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    fmFreeBufferChain(sw, buffer);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);

}   /* end fmModelPacketQueueSendISL */




/*****************************************************************************/
/* fmModelPacketQueueDumpStats
 * \ingroup modelPktQ
 *
 * \desc            Diagnostic function to dump statistics accumulated from
 *                  the white model socket packet queue interface for each
 *                  white model instance.
 *
 * \param[in]       None.
 *
 * \return          None.
 *
 *****************************************************************************/
void fmModelPrintServerStats(void)
{
    fm_modelPacketQueue *      mpq;
    fm_modelPacketQueueSwitch *swQueueState;
    fm_int                     numPorts;
    fm_int                     port;
    fm_int                     sw;
    fm_bool                    allZero = TRUE;
    fm_bool                    zero;

    mpq = (fm_modelPacketQueue *) fmRootPlatform->packetQueue;

    for (sw = 0 ; sw < FM_MAX_NUM_FOCALPOINTS ; sw++)
    {
        swQueueState = mpq->queueState[sw];

        if (swQueueState == NULL)
        {
            continue;
        }

        if (mpq->pktQueueServices.GetNumPorts(sw, &numPorts) != FM_OK)
        {
            continue;
        }

        zero = TRUE;

        for (port = 0 ; port < numPorts ; port++)
        {
            if (swQueueState->packetReceived[port] ||
                swQueueState->packetSent[port]     ||
                swQueueState->packetDropped[port])
            {
                zero = FALSE;
            }
        }

        if (!zero)
        {
            FM_LOG_PRINT("---------------------------------------------\n");
            FM_LOG_PRINT("Switch %d: PORT        IN       OUT      DROP\n", sw);

            for (port = 0 ; port < numPorts ; port++)
            {
                if (swQueueState->packetReceived[port] ||
                    swQueueState->packetSent[port]     ||
                    swQueueState->packetDropped[port])
                {
                    FM_LOG_PRINT("            %2d%10d%10d%10d\n",
                                 port,
                                 swQueueState->packetSent[port],
                                 swQueueState->packetReceived[port],
                                 swQueueState->packetDropped[port]);
                }
            }

            FM_LOG_PRINT("---------------------------------------------\n");
            allZero = FALSE;

        }   /* end if (!zero) */

    }   /* end for (sw = 0 ; sw < FM_MAX_NUM_FOCALPOINTS ; sw++) */

    if (allZero)
    {
        FM_LOG_PRINT("All counters are zero\n");
    }
    else
    {
        FM_LOG_PRINT(" IN  : received by the switch\n");
        FM_LOG_PRINT(" OUT : sent by the switch\n");
        FM_LOG_PRINT(" DROP: sent by the switch but socket queues full\n");
    }

}   /* end fmModelPrintServerStats */

