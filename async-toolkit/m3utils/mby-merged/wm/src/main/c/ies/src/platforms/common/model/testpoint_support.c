/* vim:ts=4:sw=4:expandtab:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            testpoint_support.c
 * Creation Date:   August 30, 2010
 * Description:     This module provides a support interface for TestPoint
 *                  to pass packet traffic to and from the white model through
 *                  the white model packet queue interface.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2010 - 2013 Intel Corporation. All Rights Reserved.
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

#include <fm_sdk_int.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define FM_MODEL_RIMON_META_SIZE 8

#define CAPTURE_FILE                "tester_capture.txt"
#define MAX_FOPEN_RETRIES           60
#define FOPEN_RETRY_DELAY           5
#define THREAD_ALIVE_DELAY          1
#define MAX_HOSTNAME_LEN            512
#define MAX_CONNECTIONS             160 * FM_MAX_NUM_FOCALPOINTS

#define LISTENER_THREAD_NAME        "testpoint_model_intf"

typedef enum _fm_testPointListenerThreadState
{
    FM_TP_LSTN_STATE_OPENING_PKT_SERVER,
    FM_TP_LSTN_STATE_OPENING_CAP_FILE,
    FM_TP_LSTN_STATE_SEND_INFO_API,
    FM_TP_LSTN_STATE_RECEIVING_PKTS,
} fm_testPointListenerThreadState;

/*****************************************************************************
 * Global Variables
 *****************************************************************************/

typedef struct _fm_testPointSupportState
{
    /** The thread which forwards packets to TP */
    fm_thread                       listenerHandle;

    /** Hostname of server host */
    fm_char                         modelServer[MAX_HOSTNAME_LEN];

    /** Port of server host */
    fm_int                          modelServerPort;

    /** The input socket to the model */
    fm_socket                       modelSocket;

    /** Indicates if ingress and egress packets are verbosely displayed */
    fm_bool                         monitorOn;

    /** Indicates if we are ready to send packets */
    fm_testPointListenerThreadState tpLstnState;

    /* Mechanism to check if listenerThread is alive */
    fm_int                          aliveCounter;

} fm_testPointSupportState;


/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/* Capture file descriptor for traces */
FILE *  outputFile;

/* Flag to exit thread */
fm_bool exitThread;

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

static void ComputeEgressCRC(fm_int packetLength, fm_byte *packet);

static void *ModelTestPointIntfListener(void *args);

static void ModelTestPointIntfSendPacket(fm_modelMessage *packet, int length);

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** ComputeEgressCRC
 * \ingroup intPlatform
 *
 * \desc            Computes CRC of the passed packet and overwrites the last
 *                  4 bytes with the CRC
 *
 * \param[in]       packetLength is the length of the packet including CRC
 *
 * \param[in,out]   packet is the frame on which CRC has to be calculated
 *                  and the last 4 bytes of packet should be overwritten with
 *                  the calculated CRC.
 *
 * \return          None
 *
 *****************************************************************************/
static void ComputeEgressCRC(fm_int packetLength, fm_byte *packet)
{
    fm_uint32 fcs;
    fm_int    length   = ( packetLength < 4 ? 0 : packetLength - 4 );
    fm_int    i;
    fm_int    p;
    fm_int    s;

    /***************************************************
     * Calculate the FCS and add the FCS to the packet.
     **************************************************/
    fcs = fmCrc32(packet, length);

    /* Note that the FCS is tranmitted high-order bit first. */
    for ( i = 0, p = length, s = 0 ;
          i < 4 && p < packetLength ;
          i++, p++, s += 8 )
    {
        packet[p] = (fm_byte) ((fcs >> s) & 0xFF);
    }

}   /* end ComputeEgressCRC */




/*****************************************************************************/
/** GetModelInfo
 * \ingroup intPlatform
 *
 * \desc            Queries the model packet queue for the model information for
 *                  the specified switch model and returns this information.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      numPorts points to caller-allocated storage where this
 *                  function places the number of ports supported by the
 *                  specified switch model.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
static fm_status GetModelInfo(fm_int sw, fm_int *numPorts)
{
    fm_modelMessage           msg;
    fm_socket *               sockets[2];
    fm_testPointSupportState *state = fmRootPlatform->tpState;
    fm_status                 status;
    fm_int32                  infoLength = 64;
    fm_int32                  msgLength;
    fm_int                    events[2];
    fm_int                    i;
    fm_int                    numSlots;
    fm_int                    recvLength;
    const fm_char             host[] = "localhost";
    fm_bool                   closeSocket = FALSE;
    errno_t                   rv;

    /***************************************************
     * Create a temporary network server to receive the
     * information response message.
     **************************************************/
    if (FM_MEMSET_S(sockets, sizeof(sockets), 0, sizeof(sockets)) != 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_FAIL);
    }

    for (i = 0 ; i < (fm_int) FM_NENTRIES(sockets) ; i++)
    {
        if ( ( sockets[i] = (fm_socket *) fmAlloc(sizeof(fm_socket)) ) == NULL )
        {
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status = FM_ERR_NO_MEM);
        }

        FM_CLEAR(*sockets[i]);
    }

    status = fmCreateNetworkServer(sockets[0], FM_SOCKET_TYPE_TCP, 0, 3);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    closeSocket = TRUE;
    numSlots    = 1;

    /***************************************************
     * Temporarily redirect all CPU bound traffic to the
     * TestPoint interface listener.
     **************************************************/
    FM_CLEAR(msg);

    *( (fm_uint16 *) &msg.data[0] ) = htons(sockets[0]->serverPort);
    rv = FM_STRCPY_S((fm_char *) &msg.data[2],
                     FM_MODEL_MSG_DATA_SIZE - 2,
                     host);
    if (rv != 0)
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status = FM_FAIL);
    }

    msgLength     = FM_MODEL_MSG_HEADER_SIZE + 2 + strlen(host) + 1;
    msg.msgLength = htonl(msgLength);
    msg.version   = htons(FM_MODEL_MSG_VERSION);
    msg.type      = htons(FM_MODEL_MSG_ENABLE_ALTERNATIVE_DATA_PATH);
    msg.sw        = htons(sw);
    msg.port      = htons(0);

    status = fmSendNetworkData(&state->modelSocket, &msg, msgLength);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    /***************************************************
     * Retrieve the number of ports associated with the
     * specified switch model.
     **************************************************/
    /* Transmit the model information request message. */
    FM_CLEAR(msg);

    infoLength = FM_MODEL_MSG_HEADER_SIZE + 3;
    msg.msgLength = htonl(infoLength);
    msg.version   = htons(FM_MODEL_MSG_VERSION);
    msg.type      = htons(FM_MODEL_MSG_GET_INFO);
    msg.sw        = htons(sw);
    msg.port      = htons(0);
    msg.data[0]   = FM_MODEL_INFO_REQUEST;

    status = fmSendNetworkData(&state->modelSocket, &msg, infoLength);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    /* Wait for the MPQ server to make a connection and transmit an information
     * response message. */
    status = fmWaitForNetworkEvent(sockets,
                                   &numSlots,
                                   FM_NENTRIES(sockets),
                                   events,
                                   FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    if (events[0] != FM_NETWORK_EVENT_NEW_CLIENT)
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, "MPQ server did not connect\n");
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status = FM_FAIL);
    }

    status = fmWaitForNetworkEvent(sockets,
                                   &numSlots,
                                   FM_NENTRIES(sockets),
                                   events,
                                   FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    if (events[1] != FM_NETWORK_EVENT_DATA_AVAILABLE)
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "MPQ server did not transmit a message\n");
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status = FM_FAIL);
    }

    /* Receive and process the model information response message. */
    FM_CLEAR(msg);

    status = fmReceiveNetworkData(sockets[1],
                                  &msg.msgLength,
                                  FM_MODEL_MSG_LENGTH_SIZE,
                                  &recvLength);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    msgLength = ntohl(msg.msgLength);

    status = fmReceiveNetworkData(sockets[1],
                                  ((fm_byte *) &msg) + FM_MODEL_MSG_LENGTH_SIZE,
                                  msgLength - FM_MODEL_MSG_LENGTH_SIZE,
                                  &recvLength);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    if ( ( msgLength != infoLength ) ||
         ( ntohs(msg.type) != FM_MODEL_MSG_GET_INFO ) ||
         ( ntohs(msg.sw) != sw ) ||
         ( msg.data[0] != FM_MODEL_INFO_RESPONSE ) )
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Did not receive a model information response message\n");
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status = FM_FAIL);
    }

    *numPorts = ntohs( *( (fm_uint16 *) &msg.data[1] ) );

    /***************************************************
     * Stop redirecting CPU bound traffic.
     **************************************************/
    FM_CLEAR(msg);

    msgLength     = FM_MODEL_MSG_HEADER_SIZE + 2;
    msg.msgLength = htonl(msgLength);
    msg.version   = htons(FM_MODEL_MSG_VERSION);
    msg.type      = htons(FM_MODEL_MSG_ENABLE_ALTERNATIVE_DATA_PATH);
    msg.sw        = htons(sw);
    msg.port      = htons(0);
    *( (fm_uint16 *) &msg.data[0] ) = htons(FM_MODEL_SOCKET_PORT_DISABLE);

    status = fmSendNetworkData(&state->modelSocket, &msg, msgLength);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

ABORT:
    if (status != FM_OK)
    {
        *numPorts = -1;
    }

    if (closeSocket)
    {
        fmCloseNetworkConnection(sockets[0]);
    }

    for (i = 0 ; i < (fm_int) FM_NENTRIES(sockets) ; i++)
    {
        if (sockets[i] != NULL)
        {
            fmFree(sockets[i]);
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end GetModelInfo */




/*****************************************************************************/
/** ModelTestPointIntfListener
 * \ingroup intPlatform
 *
 * \desc            Thread to receive and process packets from the white model.
 *                                                                      \lb\lb
 * \param[in]       args contains thread-initialization parameters
 *
 * \return          Never
 *
 *****************************************************************************/
static void *ModelTestPointIntfListener(void *args)
{
    fm_thread *               thread;
    fm_status                 err;
    fm_int                    sw;
    fm_int                    port;
    fm_uint16                 type;
    fm_modelMessage           packet;
    fm_int                    msgLength;
    fm_int                    recvLength;
    fm_int                    slot;
    fm_int                    i;
    FILE *                    fd;
    fm_int                    retries;
    fm_testPointSupportState *state = fmRootPlatform->tpState;
    fm_socket *               sockets[MAX_CONNECTIONS + 1];
    fm_int                    eventsReceived[MAX_CONNECTIONS + 1];
    fm_int                    numPorts[FM_MAX_NUM_FOCALPOINTS];
    fm_int                    numSockets = 1;
    fm_timestamp              timeout;
    fm_char                   currentPath[] = ".";
    fm_text                   filePath;
    fm_char                   filename[512];
    fm_int                    modelPos;
    fm_int                    curPos;
    char                      strErrBuf[FM_STRERROR_BUF_SIZE];
    errno_t                   strErrNum;

    thread = FM_GET_THREAD_HANDLE(args);
    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "thread = %s\n",
                 thread->name);

    /**************************************************
     * Recover API hostname and port
     **************************************************/
    state->tpLstnState = FM_TP_LSTN_STATE_OPENING_PKT_SERVER;

    filePath = getenv("WMODEL_INFO_PATH");

    if (!filePath)
    {
        filePath = currentPath;
    }

    snprintf(filename, 512, "%s/models.packetServer", filePath);

    modelPos = fmGetIntApiAttribute(FM_AAK_API_PLATFORM_MODEL_POSITION,
                                    FM_AAD_API_PLATFORM_MODEL_POSITION);

    /* Read socket info from file */
    retries = 0;
    while( retries < MAX_FOPEN_RETRIES )
    {
        if ( ( fd = fopen(filename, "r") ) != NULL )
        {
            fm_char  s[800];
            fm_char *p;
            fm_char *p2;

            while (TRUE)
            {
                p = fgets(s, sizeof(s), fd);
                if (p == NULL)
                {
                    FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                                 "Model Position %d not found in "
                                 "models.packetServer\n",
                                 modelPos);
                    fclose(fd);
                    return NULL;
                }
                p  = strchr(s, ':');
                if (p == NULL)
                {
                    FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                                 "current position not found in string s %s\n",
                                 s);
                    fclose(fd);
                    return NULL;
                }

                *p++ = 0;
                FM_SSCANF_S(s, "%d", &curPos);

                if (curPos == modelPos)
                {
                    break;
                }
            }
            fclose(fd);
            p2  = strchr(p, ':');
            if (p2 == NULL)
            {
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                             "error parsing string p: %s\n",
                             p);
                return NULL;
            }

            *p2++ = 0;

            if ( ( strlen(p) + 1 ) > MAX_HOSTNAME_LEN )
            {
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                             "Server host %s exceeds "
                             "maximum of 512 characters\n",
                             p);

                return NULL;
            }

            FM_STRCPY_S( state->modelServer,
                         sizeof(state->modelServer),
                         p );

            FM_SSCANF_S(p2, "%d", &state->modelServerPort);

            break;
        }
        else
        {
            FM_LOG_WARNING(FM_LOG_CAT_PLATFORM,
                           "Unable to open %s, retrying in %is\n",
                           filename,
                           FOPEN_RETRY_DELAY);
            fmDelay(FOPEN_RETRY_DELAY, 0);
        }
        retries++;
    }

    if ( retries >= MAX_FOPEN_RETRIES )
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Unable to open %s!\n", filename);

        return NULL;
    }

    /**************************************************
     * Open local client socket to send messages to API
     **************************************************/

    /* FIXME: if TP has been started, then closed, then started again,
       TP can't seem to establish client connection */
    fmCreateNetworkClient(&state->modelSocket,
                          FM_SOCKET_TYPE_TCP,
                          state->modelServer,
                          state->modelServerPort);

    /**************************************************
     * Open capture file
     **************************************************/
    state->tpLstnState = FM_TP_LSTN_STATE_OPENING_CAP_FILE;

    outputFile = fopen(CAPTURE_FILE, "w");

    if (outputFile == NULL)
    {
        strErrNum = FM_STRERROR_S(strErrBuf, FM_STRERROR_BUF_SIZE, errno);
        if (strErrNum == 0)
        {       
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                         "Can't open output file: %s\n",
                         strErrBuf);
        }
        else
        {       
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                         "Can't open output file: %d\n",
                         errno);
        }

        return NULL;
    }

    /**************************************************
     * Send this info to the API.
     **************************************************/
    state->tpLstnState = FM_TP_LSTN_STATE_SEND_INFO_API;

    numSockets = 0;
    for (sw = 0 ; sw < FM_MAX_NUM_FOCALPOINTS ; sw++)
    {
        numPorts[sw] = -1;

        if (GetModelInfo(sw, &numPorts[sw]) != FM_OK)
        {
            continue;
        }

        for (port = 1 ; port < numPorts[sw] ; port++)
        {
            FM_CLEAR(packet);

            packet.version   = htons(FM_MODEL_MSG_VERSION);
            packet.type = htons(FM_MODEL_MSG_SET_EGRESS_INFO);
            packet.sw   = htons(sw);
            packet.port = htons(port);

            sockets[numSockets] = fmAlloc(sizeof(fm_socket));

            fmCreateNetworkServer(sockets[numSockets],
                                  FM_SOCKET_TYPE_TCP,
                                  0,
                                  3);

            /* socket server network port */
            *( (fm_uint16 *) &packet.data ) = htons(sockets[numSockets]->serverPort);

            /* assume TP is running on the same host */
            FM_STRCPY_S( (fm_char *) packet.data + sizeof(fm_uint16),
                         sizeof(packet.data) - sizeof(fm_uint16),
                         "localhost" );

            msgLength = FM_MODEL_MSG_HEADER_SIZE
                        + sizeof(fm_uint16) + strlen("localhost");
            packet.msgLength = htonl(msgLength);
            fmSendNetworkData(&state->modelSocket, &packet, msgLength);
            numSockets++;
        }
    }

    /**************************************************
     * Get packets forever
     **************************************************/

    for ( slot = numSockets ; slot <= MAX_CONNECTIONS ; slot++ )
    {
        sockets[slot] = fmAlloc(sizeof(fm_socket));
        FM_CLEAR(*sockets[slot]);
        sockets[slot]->sock = -1;
    }

    state->tpLstnState = FM_TP_LSTN_STATE_RECEIVING_PKTS;

    while (TRUE)
    {
        /* increment counter to indicate that thread is alive */
        state->aliveCounter++;

        if (exitThread == TRUE)
        {
            break;
        }

        /**************************************************
         * Wait to receive a packet from the white model
         * packet queue interface.
         **************************************************/

        timeout.sec  = 0;
        timeout.usec = FM_FDS_POLL_TIMEOUT_USEC;

        FM_CLEAR(eventsReceived);

        fmWaitForNetworkEvent(sockets,
                              &numSockets,
                              MAX_CONNECTIONS + 1,
                              eventsReceived,
                              &timeout);

        for ( slot = 0 ; slot < numSockets ; slot++ )
        {
            if (eventsReceived[slot] != FM_NETWORK_EVENT_DATA_AVAILABLE)
            {
                continue;
            }

            FM_CLEAR(packet);

            /* First receive the message length ... */
            fmReceiveNetworkData(sockets[slot],
                                 &packet.msgLength,
                                 FM_MODEL_MSG_LENGTH_SIZE,
                                 &recvLength);

            if (recvLength == 0)
            {
                /* The socket is closed; proceed to the next network event. */
                sockets[slot]->type = FM_SOCKET_TYPE_CLOSED;
                continue;
            }

            FM_LOG_ASSERT(FM_LOG_CAT_PLATFORM,
                          recvLength == FM_MODEL_MSG_LENGTH_SIZE,
                          "Message corruption detected: Expected %d bytes, received %d bytes\n",
                          (fm_int) FM_MODEL_MSG_LENGTH_SIZE,
                          recvLength);

            msgLength = ntohl(packet.msgLength);

            /* ... then receive the remaining data. */
            fmReceiveNetworkData(sockets[slot],
                                 ((fm_byte *) &packet) + FM_MODEL_MSG_LENGTH_SIZE,
                                 msgLength - FM_MODEL_MSG_LENGTH_SIZE,
                                 &recvLength);

            FM_LOG_ASSERT(FM_LOG_CAT_PLATFORM,
                          recvLength == ( (fm_int) (msgLength - FM_MODEL_MSG_LENGTH_SIZE) ),
                          "Message corruption detected: Expected %d bytes, received %d bytes\n",
                          (fm_int) (msgLength - FM_MODEL_MSG_LENGTH_SIZE),
                          recvLength);

            /* Do not process this message unless it is a packet. */
            type  = ntohs(packet.type);
            if ( type != ( (fm_uint16) FM_MODEL_MSG_PACKET ) )
            {
                continue;
            }

            msgLength -= FM_MODEL_MSG_HEADER_SIZE;
            sw        = ntohs(packet.sw);
            port      = ntohs(packet.port);

            /* Print on screen if requested */
            if (state->monitorOn)
            {
                printf("--- Received from switch %d, port %d, length %d ----\n",
                       sw,
                       port,
                       msgLength);

                for (i = 0 ; i < msgLength ; i++)
                {
                    printf("%02x ", packet.data[i]);

                    if ( (i % 16) == 15 )
                    {
                        printf("\n");
                    }
                }

                if ( (i % 16) != 15 )
                {
                    printf("\n");
                }

                fflush(stdout);
            }

            /* Save packet in a file */
            fprintf(outputFile,
                    "--- Received from switch %d, port %d, length %d ----\n",
                    sw,
                    port,
                    msgLength);

            for (i = 0 ; i < msgLength ; i++)
            {
                if ( (i % 20) == 19 )
                {
                    fprintf(outputFile, "%02x\n", packet.data[i]);
                }
                else
                {
                    fprintf(outputFile, "%02x ", packet.data[i]);
                }
            }

            if ( (i % 20) != 0 )
            {
                fprintf(outputFile, "\n");
            }

            fflush(outputFile);

        }   /* end for ( slot = 0 ; slot < numSockets ; slot++ ) */

    }   /* end while (TRUE) */

    /**************************************************
     * Unregister ports with API
     **************************************************/
    numSockets = 0;
    for (sw = 0 ; sw < FM_MAX_NUM_FOCALPOINTS ; sw++)
    {
        for (port = 1 ; port < numPorts[sw] ; port++)
        {
            FM_CLEAR(packet);

            packet.version   = htons(FM_MODEL_MSG_VERSION);
            packet.type = htons(FM_MODEL_MSG_SET_EGRESS_INFO);
            packet.sw   = htons(sw);
            packet.port = htons(port);

            /* socket server network port */
            *( (fm_uint16 *) &packet.data ) = htons(FM_MODEL_SOCKET_PORT_DISABLE);

            /* assume TP is running on the same host */
            FM_STRCPY_S( (fm_char *) packet.data + sizeof(fm_uint16),
                         sizeof(packet.data) - sizeof(fm_uint16),
                         "localhost" );

            msgLength = FM_MODEL_MSG_HEADER_SIZE
                        + sizeof(fm_uint16) + strlen("localhost");
            packet.msgLength = htonl(msgLength);

            fmSendNetworkData(&state->modelSocket, &packet, msgLength);

            fmFree(sockets[numSockets]);

            numSockets++;
        }
    }

    for ( slot = numSockets ; slot <= MAX_CONNECTIONS ; slot++ )
    {
        fmFree(sockets[slot]);
    }

    fmCloseNetworkConnection(&state->modelSocket);

    fclose(outputFile);

    err = fmExitThread(thread);

    if (err != FM_OK)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                     "Can't exit thread: %s\n",
                     LISTENER_THREAD_NAME);
    }

    return NULL;

}   /* end ModelTestPointIntfListener */




/*****************************************************************************/
/** ModelTestPointIntfSendPacket
 * \ingroup intPlatform
 *
 * \desc            Forward the packet to the white model. The packet
 *                  must be prepended with 01-sw-port-00.
 *
 * \param[in]       packet is the pointer to the packet to send to
 *
 * \param[in]       length is the length of valid buffer in message.
 *
 * \return          None.
 *
 *****************************************************************************/

static void ModelTestPointIntfSendPacket(fm_modelMessage *packet, int length)
{
    fm_testPointSupportState *state = fmRootPlatform->tpState;
    fm_int                    i;
    fm_int                    sw;
    fm_int                    port;
    fm_int                    totalFrmLength;

    if (state->tpLstnState != FM_TP_LSTN_STATE_RECEIVING_PKTS)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                     "Platform is not ready to send!\n");
        return;
    }

    if(outputFile == NULL)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                     "Only the first instance of TestPoint can send packets\n");
        return;
    }

    sw   = ntohs(packet->sw);
    port = ntohs(packet->port);

    if (length <= FM_MODEL_MAX_PACKET_SIZE)
    {
        /* Add the length of the white model header to the total length */

        totalFrmLength = length + FM_MODEL_MSG_HEADER_SIZE;

        packet->msgLength = htonl(totalFrmLength);

        if (fmSendNetworkData(&state->modelSocket, packet, totalFrmLength) == FM_OK)
        {

            /* Print on screen if requested */
            if (state->monitorOn)
            {
                printf("--- Sent to switch %d, port %d, length %d ----\n", sw, port, length);

                for (i = 0 ; i < length ; i++)
                {
                    printf("%02x ", packet->data[i]);

                    if ( (i % 16) == 15 )
                    {
                        printf("\n");
                    }
                }

                if ( (i % 16) != 15 )
                {
                    printf("\n");
                }

                fflush(stdout);
            }

            /* Save packet in a file */
            fprintf(outputFile, "--- Sent to switch %d, port %d, length %d ----\n", sw, port, length);

            for (i = 0 ; i < length ; i++)
            {
                if ( (i % 20) == 19 )
                {
                    fprintf(outputFile, "%02x\n", packet->data[i]);
                }
                else
                {
                    fprintf(outputFile, "%02x ", packet->data[i]);
                }
            }

            if ( (i % 20) != 19 )
            {
                fprintf(outputFile, "\n");
            }

            fflush(outputFile);
        }
    }

}   /* end ModelTestPointIntfSendPacket */




/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/* fmModelTestPointIntfInit
 * \ingroup intPlatform
 *
 * \desc            Initialize the TestPoint support interface.
 *                  Starts a listener thread for processing packets received
 *                  from switch ports on the model.
 *
 * \param           None.
 *
 * \return          None.
 *
 *****************************************************************************/
void fmModelTestPointIntfInit(void)
{
    fm_status                 err = FM_OK;
    fm_testPointSupportState *state = NULL;
    fm_int                    cnt1 = 0;
    fm_int                    cnt2 = 0;

    exitThread = FALSE;

    if (!fmRootPlatform->tpState)
    {
        fmRootPlatform->tpState = fmAlloc(sizeof(fm_testPointSupportState));

        if (!fmRootPlatform->tpState)
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "Unable to allocate TestPoint support state!\n");

            return;
        }

        state = fmRootPlatform->tpState;

        FM_CLEAR(*state);

        /* create thread to listen to sockets */
        err = fmCreateThread(LISTENER_THREAD_NAME,
                             0,
                             &ModelTestPointIntfListener,
                             0,
                             &state->listenerHandle);

        if (err != FM_OK)
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "Unable to create TestPoint listener thread: %s\n",
                         fmErrorMsg(err));
        }
    }
    else
    {
        /* If TP has been closed and started again, the thread should not be
         * alive and we must do the init for TP to properly work */
        state = fmRootPlatform->tpState;

        cnt1 = state->aliveCounter;
        fmDelay(THREAD_ALIVE_DELAY, 0);
        cnt2 = state->aliveCounter;

        if (cnt1 == cnt2)
        {
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                         "TestPoint was not properly shutdown, restart all API "
                         "instances for proper operation of TestPoint\n");
        }
    }

}   /* end fmModelTestPointIntfInit */


/*****************************************************************************/
/* fmModelTestPointIntfExit
 * \ingroup intPlatform
 *
 * \desc            Exits the TestPoint support interface.
 *                  Closes the listener thread and free's
 *                  allocated memory
 *
 * \param           None.
 *
 * \return          None.
 *
 *****************************************************************************/
void fmModelTestPointIntfExit(void)
{
    fm_int                    cnt1 = 0;
    fm_int                    cnt2 = 0;
    fm_testPointSupportState *state = fmRootPlatform->tpState;


    /* Only do the exit procedure if outputFile exists, process is therefore
    *  owner of the listener thread */
    if (outputFile != NULL)
    {
        /* Set the flag to let the thread exit itself */
        exitThread = TRUE;
        fmDelay(THREAD_ALIVE_DELAY, 0);
        cnt1 = state->aliveCounter;
        fmDelay(THREAD_ALIVE_DELAY, 0);
        cnt2 = state->aliveCounter;

        if (cnt1 != cnt2)
        {
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                         "Could not properly exit thread %s\n",
                         LISTENER_THREAD_NAME);
        }

        if (fmRootPlatform->tpState)
        {
            fmFree(fmRootPlatform->tpState);
            fmRootPlatform->tpState = NULL;
        }
    }

}   /* end fmModelTestPointIntfExit */


/*****************************************************************************/
/* fmModelTestPointIntfPrintPackets
 * \ingroup intPlatform
 *
 * \desc            Controls printing on stdout
 *
 * \param[in]       enabled specifies whether printing on stdout is enabled
 *
 * \return          None.
 *
 *****************************************************************************/
void fmModelTestPointIntfPrintPackets(fm_bool enabled)
{
    fm_testPointSupportState *state = fmRootPlatform->tpState;

    state->monitorOn = enabled;

    if (state->monitorOn)
    {
        printf("Monitor (screen printing) ON\n");
    }
    else
    {
        printf("Monitor (screen printing) OFF\n");
    }

}   /* end fmModelTestPointIntfPrintPackets */




/*****************************************************************************/
/* fmSimplePrintCapture
 * \ingroup intPlatform
 *
 * \desc            Print captured packets.
 *
 * \param           None.
 *
 * \return          None.
 *
 *****************************************************************************/
void fmModelTestPointIntfPrintCapture(void)
{
    FILE *in;
    char  s[80];

    if(outputFile == NULL)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                     "Only the first instance of TestPoint can show capture\n");
        return;
    }

    printf("Dumping %s\n", CAPTURE_FILE);
    in = fopen(CAPTURE_FILE, "r");

    if (in == NULL)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                     "Unable to open capture file %s\n",
                     CAPTURE_FILE);
        return;
    }

    while ( fgets(s, sizeof(s), in) )
    {
        fputs(s, stdout);
    }

    fclose(in);

}   /* end fmSimplePrintCapture */




/*****************************************************************************/
/* fmModelTestPointIntfSendL2Packet
 * \ingroup intPlatform
 *
 * \desc            Print captured packets.
 *
 * \param           None.
 *
 * \return          None.
 *
 *****************************************************************************/
void fmModelTestPointIntfSendL2Packet(fm_int sw,
                                      fm_int port,
                                      fm_int dmacHigh,
                                      fm_int dmacLow,
                                      fm_int smacHigh,
                                      fm_int smacLow,
                                      fm_int vid,
                                      fm_int vpri,
                                      fm_int type,
                                      fm_int length)
{
    fm_modelMessage packet;
    fm_int          offset;
    fm_int          pktOff;
    fm_byte         b;

    packet.version       = htons(FM_MODEL_MSG_VERSION);
    packet.type           = htons(FM_MODEL_MSG_PACKET);
    packet.sw             = htons(sw);
    packet.port           = htons(port);

    /* Packet meta TLV */
    offset = 0;
    packet.data[offset]   = FM_MODEL_PACKET_META;
    *( (fm_int32 *) &packet.data[offset + FM_MODEL_DATA_TYPE_SIZE] ) =
                                htonl(FM_MODEL_RIMON_META_SIZE);
    offset               += FM_MODEL_MSG_TLV_SIZE;
    packet.data[offset++] = 0x18; /* Type from Rimmon to HLP */
    packet.data[offset++] = 0;
    packet.data[offset++] = 0;
    packet.data[offset++] = 0;
    packet.data[offset++] = 0;
    packet.data[offset++] = 0;
    packet.data[offset++] = 0;
    packet.data[offset++] = 0;

    /* Packet data TLV */
    packet.data[offset]   = FM_MODEL_DATA_PACKET;
    *( (fm_int32 *) &packet.data[offset + FM_MODEL_DATA_TYPE_SIZE] ) =
                                htonl(length);
    offset               += FM_MODEL_MSG_TLV_SIZE;

    /* Start of packet data */
    pktOff                = offset;
    packet.data[offset++] = dmacHigh >> 16;
    packet.data[offset++] = dmacHigh >> 8;
    packet.data[offset++] = dmacHigh;
    packet.data[offset++] = dmacLow >> 16;
    packet.data[offset++] = dmacLow >> 8;
    packet.data[offset++] = dmacLow;
    packet.data[offset++] = smacHigh >> 16;
    packet.data[offset++] = smacHigh >> 8;
    packet.data[offset++] = smacHigh;
    packet.data[offset++] = smacLow >> 16;
    packet.data[offset++] = smacLow >> 8;
    packet.data[offset++] = smacLow;

    if (vid >= 0)
    {
        packet.data[offset++] = 0x81;
        packet.data[offset++] = 0x00;
        packet.data[offset++] = ( (vpri & 7) << 5 ) + ( (vid >> 8) & 0xF );
        packet.data[offset++] = vid;
    }

    packet.data[offset++] = type >> 8;
    packet.data[offset++] = type;
    b                     = 0;

    while (offset < length && offset < FM_MODEL_MAX_PACKET_SIZE)
    {
        packet.data[offset++] = b++;
    }

    ComputeEgressCRC(length, &packet.data[pktOff]);

    ModelTestPointIntfSendPacket(&packet, offset);

}   /* end fmModelTestPointIntfSendL2Packet */



/*****************************************************************************/
/* fmModelTestPointIntfSendFullPacket
 * \ingroup intPlatform
 *
 * \desc            Print captured packets.
 *
 * \param           None.
 *
 * \return          None.
 *
 *****************************************************************************/
void fmModelTestPointIntfSendFullPacket(fm_int sw,
                                        fm_int port,
                                        fm_int *sendData,
                                        fm_int length)
{
    fm_modelMessage packet;
    fm_int          offset;
    fm_int          pktOff;
    fm_int          i;

    packet.version        = htons(FM_MODEL_MSG_VERSION);
    packet.type           = htons(FM_MODEL_MSG_PACKET);
    packet.sw             = htons(sw);
    packet.port           = htons(port);

    /* Packet meta TLV */
    offset                = 0;
    packet.data[offset]   = FM_MODEL_PACKET_META;
    *( (fm_int32 *) &packet.data[offset + FM_MODEL_DATA_TYPE_SIZE] ) =
                                htonl(FM_MODEL_RIMON_META_SIZE);
    offset               += FM_MODEL_MSG_TLV_SIZE;
    packet.data[offset++] = 0x18; /* Type from Rimmon to HLP */
    packet.data[offset++] = 0;
    packet.data[offset++] = 0;
    packet.data[offset++] = 0;
    packet.data[offset++] = 0;
    packet.data[offset++] = 0;
    packet.data[offset++] = 0;
    packet.data[offset++] = 0;

    /* Packet data TLV */
    packet.data[offset]   = FM_MODEL_DATA_PACKET;
    *( (fm_int32 *) &packet.data[offset + FM_MODEL_DATA_TYPE_SIZE] ) =
                            htonl(length);
    offset               += FM_MODEL_MSG_TLV_SIZE;

    /* Start of packet data */
    pktOff                = offset;
    for ( i = 0; i < length ; i++ )
    {
        packet.data[offset++]= sendData[i];
    }

    ComputeEgressCRC(length, &packet.data[pktOff]);

    ModelTestPointIntfSendPacket(&packet, offset);

}   /* end fmModelTestPointIntfSendFullPacket */

