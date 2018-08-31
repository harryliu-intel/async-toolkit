/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            platform_remote_regs.c
 * Creation Date:   April 26, 2016
 * Description:     Remote registers access.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2012-2016 Intel Corporation. All Rights Reserved. 
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
#include <sys/socket.h>
#include <poll.h>
#include <netinet/in.h>
#include <netdb.h>

#include <fm_sdk_int.h>

#define READ_TIMEOUT                2000
#define FM_MODEL_CPK_META_SIZE      32
#define FM_MODEL_META_TYPE_LAN_TX   1

static int debug =0;
static int clientSockFd;


static void getHostInfo(fm_text filename, fm_char *host,
                    fm_int hostSize, fm_int *port)
{
    FILE *fd;
    fm_char buffer[256];
    fm_char tmp[256];
    fm_int sw;
    fm_uint start, cnt;

    fd = fopen(filename, "r");
    if (fd == NULL)
    {
        printf("Unable to open file %s\n", filename);
        exit(0);
    }

    fgets(&buffer[0], 256, fd);

    start = 0;
    cnt = 0;
    while( (cnt < strlen(buffer))
            && buffer[start+cnt] != ':')
    {
        cnt++;
    }

    bzero(&tmp, 256);
    memcpy(&tmp, &buffer[0], cnt);
    sw = atoi(tmp);

    start = start+cnt+1;
    cnt = 0;
    while( (start+cnt < strlen(buffer))
            && buffer[start+cnt] != ':')
    {
        cnt++;
    }

    bzero(host, hostSize);
    memcpy(host, &buffer[start], cnt);

    start = start+cnt+1;
    cnt = 0;
    while( (start+cnt < strlen(buffer))
            && buffer[start+cnt] != ':')
    {
        cnt++;
    }

    bzero(&tmp, 256);
    memcpy(&tmp, &buffer[start], cnt);
    *port = atoi(tmp);
}

/* Read from a socket with timeout 

 int socket            The socket descriptor to read from
 unsigned char *data   The data buffer to store received data
 int dataSize          The size of data buffer
 int timeoutMsec       The timeout if millisecond to return if no response
*/
/*****************************************************************************/
/** readData
 * \ingroup platform
 *
 * \desc            Read from a socket with timeout.
 *
 * \param[in]       socket is the socket descriptor to read from.
 *
 * \param[in]       data is the buffer to store received data.
 *
 * \param[in]       dataSize is the size of data buffer.
 *
 * \param[in]       timeoutMsec is the timeout to wait for data.
 *
 * \return          number of received bytes or 0.
 *
 *****************************************************************************/
static fm_uint readData(int socket, fm_byte *data,
                        fm_int dataSize, fm_int timeoutMsec)
{
    struct pollfd fds[1] = { {0} };
    fm_int           fdsCnt;
    fm_int           fdsTimeout;
    fm_int           errResult;


    if (timeoutMsec > 0)
    {
        fdsTimeout = timeoutMsec;
    }
    else
    {
        fdsTimeout = -1;
    }

    fds[0].fd = socket;
    fds[0].events = POLLIN;
    fds[0].revents = 0;

    do
    {
        errResult = poll(fds, 1, fdsTimeout);
    }
    while ( ( errResult == -1 ) && ( errno == EINTR ) );

    if (fds[0].revents & POLLIN)
    {
        return read(socket, data, dataSize);
    }

     FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
        "Timeout waiting for message\n");
    return 0;
}


/*****************************************************************************
 * Public Functions
 *****************************************************************************/
/* These functions below are just stubs or not supported */
fm_status platformRemoteReset(fm_int sw)
{
    FM_NOT_USED(sw);

    return FM_OK;
}
fm_status platformRemoteResetV2(fm_int sw, fm_int domain)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(domain);

    return FM_OK;
}
fm_status platformRemoteReadCSRMult(fm_int     sw,
                                    fm_uint32  addr,
                                    fm_int     n,
                                    fm_uint32 *value)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(addr);
    FM_NOT_USED(n);
    FM_NOT_USED(value);

    return FM_ERR_UNSUPPORTED;
}
fm_status platformRmoteWriteCSRMult(fm_int     sw,
                                    fm_uint32  addr,
                                    fm_int     n,
                                    fm_uint32 *newValue)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(addr);
    FM_NOT_USED(n);
    FM_NOT_USED(newValue);

    return FM_ERR_UNSUPPORTED;
}
fm_status platformRemoteReadCSRMult64(fm_int     sw,
                                      fm_uint32  addr,
                                      fm_int     n,
                                      fm_uint64 *value)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(addr);
    FM_NOT_USED(n);
    FM_NOT_USED(value);

    return FM_ERR_UNSUPPORTED;
}
fm_status platformRmoteWriteCSRMult64(fm_int     sw,
                                      fm_uint32  addr,
                                      fm_int     n,
                                      fm_uint64 *newValue)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(addr);
    FM_NOT_USED(n);
    FM_NOT_USED(newValue);

    return FM_ERR_UNSUPPORTED;
}
fm_status platformRemoteTick(fm_int sw, fm_uint32 *interrupt)
{

    FM_NOT_USED(sw);
    FM_NOT_USED(interrupt);

   /* Return interrupts */
    *interrupt = FM_LITERAL_U64(0);;

    return FM_OK;

}



/*****************************************************************************/
/** platformRemoteInitialize
 * \ingroup platform
 *
 * \desc            Initialize the model by connect to the remote model
 *                  specifying by the FM_AAK_API_PLATFORM_MODEL_SERVER.
 *                  Arguments to function are not used.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status platformRemoteInitialize(void **chipModel, fm_int sw, void *funcPtrs)
{
    fm_char hostname[256];
    fm_int port = 51093;
    struct sockaddr_in serv_addr;
    struct hostent *server;
    fm_text serverLoc;

    FM_NOT_USED(chipModel);
    FM_NOT_USED(sw);
    FM_NOT_USED(funcPtrs);

    /* Model is remote */
    *chipModel = NULL;
    serverLoc = fmGetTextApiAttribute(
                    FM_AAK_API_PLATFORM_MODEL_SERVER,
                    FM_AAD_API_PLATFORM_MODEL_SERVER);

    FM_LOG_PRINT("Server location: %s\n", serverLoc);
    getHostInfo(serverLoc, &hostname[0], sizeof(hostname), &port);

    FM_LOG_PRINT("Connecting to model server at %s:%d \n",
        hostname, port);

    clientSockFd = socket(AF_INET, SOCK_STREAM, 0);
    if(clientSockFd < 0)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
            "Error creating socket fd\n");
        return FM_FAIL;
    }

    server = gethostbyname(hostname);

    if(server == NULL)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
            "Error unable to find host %s\n", hostname);
        return FM_FAIL;
    }

    bzero( (char *)&serv_addr, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    bcopy( (char *)server->h_addr,
           (char *)&serv_addr.sin_addr.s_addr,
           server->h_length);
    serv_addr.sin_port = htons(port);
    

    if(connect(clientSockFd, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
            "Error unable to connect to server\n");
        return FM_FAIL;
    }

    FM_LOG_PRINT("Connected model server\n");
    return FM_OK;

}


/*****************************************************************************/
/** platformRemoteReadCSR
 * \ingroup platform
 *
 * \desc            Read 32 bits of an CSR register.
 *
 * \param[in]       sw is the switch number of the model.
 *
 * \param[in]       addr contains the CSR register address to read.
 *
 * \param[out]      value points to caller-allocated storage where this
 *                  function will place the read register value.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status platformRemoteReadCSR(fm_int sw, fm_uint32 addr, fm_uint32 *value)
{
    fm_byte buffer[256];
    fm_uint d,n;
    fm_byte type;
    fm_uint len = 0;
    fm_uint16 msgType;

    bzero(buffer, sizeof(buffer));
    
    *( (fm_uint16 *) &buffer[4]) = htons(2); 
    len +=2;
    *( (fm_uint16 *) &buffer[6]) = htons(FM_MODEL_MSG_MGMT); 
    len += 2;
    *( (fm_uint16 *) &buffer[8]) = htons(sw);        //Sw
    len += 2;
    *( (fm_uint16 *) &buffer[10]) = htons(0);       //Port
    len += 2;

    /* Message format is:
     *   1B type, 4B addr, 4B data 
     */
    buffer[12] = FM_MODEL_MGMT_READ_REQUEST;
    len ++;
    *( (fm_uint *) &buffer[13]) = htonl(addr);
    len += 4;
    *( (fm_uint *) &buffer[17]) = htonl(0);       //Data
    len += 4;
    
    len += 4;
    *( (fm_uint *) &buffer[0]) = htonl(len); 

    write(clientSockFd, &buffer, len);

    bzero(buffer, sizeof(buffer));
    n = readData(clientSockFd, buffer, 4, READ_TIMEOUT);
    if (n != 4)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
            "Expect %d bytes but got %d\n", 4, n);
        return FM_FAIL;
    }
    
    len = ntohl( *( (fm_uint *) &buffer[0]));
    n = readData(clientSockFd, &buffer[4], len-4, READ_TIMEOUT);
    if (n != len-4)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
            "Expect %d bytes but got %d\n", len-4, n);
        return FM_FAIL;
    }

    msgType = ntohs(*( (fm_uint16 *) &buffer[6]));
    switch (msgType)
    {
        case FM_MODEL_MSG_ERROR:
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                "%s: %s\n", __func__, buffer + 12);
            return FM_ERR_INVALID_ARGUMENT;
        case FM_MODEL_MSG_MGMT:
            break;
        default:
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM, 
                "%s: Unexpected msgType %d\n", __func__, msgType);
            return FM_ERR_INVALID_ARGUMENT;
    }

    type = buffer[12];
    addr = ntohl( *( (fm_uint *) &buffer[13]));
    *value = ntohl( *( (fm_uint *) &buffer[17]));

    if(type != FM_MODEL_MGMT_READ_RESPONSE)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM, 
            "Error unexpected MGMT type %d\n", type);
        return FM_ERR_INVALID_ARGUMENT;
    }

    return FM_OK;
}


/*****************************************************************************/
/** platformRemoteReadCSR64
 * \ingroup platform
 *
 * \desc            Read 64 bits of an CSR register.
 *
 * \param[in]       sw is the switch number of the model.
 *
 * \param[in]       addr contains the CSR register address to read.
 *
 * \param[out]      value points to caller-allocated storage where this
 *                  function will place the read register value.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status platformRemoteReadCSR64(fm_int sw, fm_uint32 addr, fm_uint64 *value)
{
    fm_byte buffer[256];
    fm_uint d,n;
    fm_byte type;
    fm_uint len = 0;
    fm_uint16 msgType;

    bzero(buffer, 256);
    
    *( (fm_uint16 *) &buffer[4]) = htons(2); 
    len +=2;
    *( (fm_uint16 *) &buffer[6]) = htons(FM_MODEL_MSG_MGMT); 
    len += 2;
    *( (fm_uint16 *) &buffer[8]) = htons(sw);        //Sw
    len += 2;
    *( (fm_uint16 *) &buffer[10]) = htons(0);       //Port
    len += 2;

    /* Message format is:
     *   1B type, 4B addr, 8B data 
     */
    buffer[12] = FM_MODEL_MGMT_READ64_REQUEST;
    len ++;
    *( (fm_uint *) &buffer[13]) = htonl(addr);
    len += 4;
    *( (fm_uint *) &buffer[17]) = htonl(0);       //Data
    len += 4;
    *( (fm_uint *) &buffer[21]) = htonl(0);       //Data
    len += 4;
    
    len += 4;
    *( (fm_uint *) &buffer[0]) = htonl(len); 

    write(clientSockFd, &buffer, len);

    bzero(buffer, 256);
    n = readData(clientSockFd, buffer, 4, READ_TIMEOUT);
    if (n != 4)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
            "Expect %d bytes but got %d\n", 4, n);
        return FM_FAIL;
    }
    
    len = ntohl( *( (fm_uint *) &buffer[0]));
    n = readData(clientSockFd, &buffer[4], len-4, READ_TIMEOUT);
    if (n != len-4)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
            "Expect %d bytes but got %d\n", len-4, n);
        return FM_FAIL;
    }

    msgType = ntohs(*( (fm_uint16 *) &buffer[6]));
    switch (msgType)
    {
        case FM_MODEL_MSG_ERROR:
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                "%s: %s\n", __func__, buffer + 12);
            return FM_ERR_INVALID_ARGUMENT;
        case FM_MODEL_MSG_MGMT:
            break;
        default:
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM, 
                "%s: Unexpected msgType %d\n", __func__, msgType);
            return FM_ERR_INVALID_ARGUMENT;
    }

    type = buffer[12];
    addr = ntohl( *( (fm_uint *) &buffer[13]));
    *value = ntohl( *( (fm_uint *) &buffer[17]));
    *value <<= 32;
    *value |= ntohl( *( (fm_uint *) &buffer[21]));

    if (type != FM_MODEL_MGMT_READ64_RESPONSE)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM, 
            "Error unexpected MGMT type %d\n", type);
        return FM_ERR_INVALID_ARGUMENT;
    }

    return FM_OK;
}



/*****************************************************************************/
/** platformRemoteWriteCSR
 * \ingroup platform
 *
 * \desc            Write 32 bits of an CSR register.
 *
 * \param[in]       sw is the switch number of the model.
 *
 * \param[in]       addr contains the CSR register address to write.
 *
 * \param[in]       vValue is the data value to write to the register.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status platformRemoteWriteCSR(fm_int sw, fm_uint32 addr, fm_uint32 value)
{
    fm_byte buffer[256];
    fm_uint len = 0;
    fm_uint d, n;
    fm_byte type;
    fm_uint16 msgType;

    if (debug) printf("writing 0x%x data 0x%x\n", addr, value);
    bzero(buffer, 256);

    *( (fm_uint16 *) &buffer[4]) = htons(2); 
    len +=2;
    *( (fm_uint16 *) &buffer[6]) = htons(FM_MODEL_MSG_MGMT); 
    len += 2;
    *( (fm_uint16 *) &buffer[8]) = htons(sw);        //Sw
    len += 2;
    *( (fm_uint16 *) &buffer[10]) = htons(0);       //Port
    len += 2;

    /* Message format is:
     *   1B type, 4B addr, 4B data 
     */
    buffer[12] = FM_MODEL_MGMT_WRITE;
    len ++;
    *( (fm_uint *) &buffer[13]) = htonl(addr);
    len += 4;
    *( (fm_uint *) &buffer[17]) = htonl(value);
    len += 4;
    
    len += 4;
    *( (fm_uint *) &buffer[0]) = htonl(len); 

    write(clientSockFd, &buffer, len);

    bzero(buffer, 256);
    n = readData(clientSockFd, buffer, 4, READ_TIMEOUT);
    
    len = ntohl( *( (fm_uint *) &buffer[0]));
    n = read(clientSockFd, &buffer[4], len-4);
    if (n != len-4)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
            "Expect %d bytes but got %d\n", len-4, n);
        return FM_FAIL;
    }

    msgType = ntohs(*( (fm_uint16 *) &buffer[6]));
    switch (msgType)
    {
        case FM_MODEL_MSG_ERROR:
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                "%s: %s\n", __func__, buffer + 12);
            return FM_ERR_INVALID_ARGUMENT;
        case FM_MODEL_MSG_MGMT:
            break;
        default:
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM, 
                "%s: Unexpected msgType %d\n", __func__, msgType);
            return FM_ERR_INVALID_ARGUMENT;
    }

    type = buffer[12];
    if(type != FM_MODEL_MGMT_WRITE_ACK)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM, 
            "Error unexpected MGMT type %d\n", type);
        return FM_ERR_INVALID_ARGUMENT;
    }

    return FM_OK;
}


/*****************************************************************************/
/** platformRemoteWriteCSR64
 * \ingroup platform
 *
 * \desc            Write 64 bits of an CSR register.
 *
 * \param[in]       sw is the switch number of the model.
 *
 * \param[in]       addr contains the CSR register address to write.
 *
 * \param[in]       value is the data value to write to the register.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status platformRemoteWriteCSR64(fm_int sw, fm_uint32 addr, fm_uint64 value)
{
    fm_byte buffer[256];
    fm_uint len = 0;
    fm_uint d, n;
    fm_byte type;
    fm_uint16 msgType;

    if (debug) printf("writing 0x%x data 0x%llx\n", addr, value);
    bzero(buffer, 256);

    *( (fm_uint16 *) &buffer[4]) = htons(2); 
    len +=2;
    *( (fm_uint16 *) &buffer[6]) = htons(FM_MODEL_MSG_MGMT); 
    len += 2;
    *( (fm_uint16 *) &buffer[8]) = htons(sw);        //Sw
    len += 2;
    *( (fm_uint16 *) &buffer[10]) = htons(0);       //Port
    len += 2;

    /* Message format is:
     *   1B type, 4B addr, 4B data 
     */
    buffer[12] = FM_MODEL_MGMT_WRITE64;
    len ++;
    *( (fm_uint *) &buffer[13]) = htonl(addr);
    len += 4;
    *( (fm_uint *) &buffer[17]) = htonl(value>>32);
    len += 4;
    *( (fm_uint *) &buffer[21]) = htonl(value);
    len += 4;
    
    len += 4;
    *( (fm_uint *) &buffer[0]) = htonl(len); 

    write(clientSockFd, &buffer, len);

    bzero(buffer, 256);
    n = readData(clientSockFd, buffer, 4, READ_TIMEOUT);
    if (n != 4)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
            "Expect %d bytes but got %d\n", 4, n);
        return FM_FAIL;
    }
    
    len = ntohl( *( (fm_uint *) &buffer[0]));
    n = readData(clientSockFd, &buffer[4], len-4, READ_TIMEOUT);
    if (n != len-4)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
            "Expect %d bytes but got %d\n", len-4, n);
        return FM_FAIL;
    }

    msgType = ntohs(*( (fm_uint16 *) &buffer[6]));
    switch (msgType)
    {
        case FM_MODEL_MSG_ERROR:
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                "%s: %s\n", __func__, buffer + 12);
            return FM_ERR_INVALID_ARGUMENT;
        case FM_MODEL_MSG_MGMT:
            break;
        default:
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM, 
                "%s: Unexpected msgType %d\n", __func__, msgType);
            return FM_ERR_INVALID_ARGUMENT;
    }

    type = buffer[12];
    if (type != FM_MODEL_MGMT_WRITE64_ACK)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM, 
            "Error unexpected MGMT type %d\n", type);
        return FM_ERR_INVALID_ARGUMENT;
    }

    return FM_OK;
}


/*****************************************************************************/
/** platformRemoteSendPkt
 * \ingroup platform
 *
 * \desc            Called to add the given packet to the internal packet
 *                  queue in the lookup packet send mode.
 *
 * \param[in]       sw is the switch on which to send the packet.
 *
 * \param[in]       buffer is a pointer to a chain of fm_buffer structures
 *                  containing the payload.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status platformRemoteSendPkt(fm_int sw, fm_buffer *buffer)
{
    fm_modelMessage      packet;
    fm_int32             length;
    fm_int32             msgLength;
    fm_buffer *          current = buffer;
    fm_int               n;
    fm_byte *            t;
    fm_int               physPort;
    fm_status            status;
    fm_uint32            bufferTmpLen = buffer->len;
    fm_uint32 *          bufferTmpData = buffer->data;
    fm_uint16            switchPri;
    fm_uint32            fcs;
    fm_byte *            data;
    fm_int               pktOff;
    fm_int               offset;
    fm_int               metaLen;
    fm_int               nb;

    FM_LOG_ENTRY( FM_LOG_CAT_PLATFORM,
                  "sw=%d buffer=%p",
                  sw,
                  (void *) buffer);

    status = FM_OK; 
    physPort = 0; /* Assume 0 for host port */
    
    /* Write switch and cpu port */
    packet.version = htons(FM_MODEL_MSG_VERSION);
    packet.type    = htons(FM_MODEL_MSG_PACKET);
    packet.sw      = htons(sw);
    packet.port    = htons(physPort);

    /* Packet meta TLV */
    offset               = 0;
    packet.data[offset]  = FM_MODEL_PACKET_META;
    *( (fm_int32 *) &packet.data[offset + FM_MODEL_DATA_TYPE_SIZE] ) =
                                htonl(FM_MODEL_CPK_META_SIZE);
    offset              += FM_MODEL_MSG_TLV_SIZE;
    memset(&packet.data[offset], 0, FM_MODEL_CPK_META_SIZE);
    packet.data[offset]  = FM_MODEL_META_TYPE_LAN_TX; /* Type from CPK to HLP */
    /* Dest. Port */
    FM_ARRAY_SET_UNNAMED_FIELD((fm_uint32*)&packet.data[offset], 16, 5, 0);
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

    fcs = htonl(fmCrc32(data, length));

    /* Append user-specified FCS to buffer */
    FM_MEMCPY_S(&data[length], 4, &fcs, sizeof(fcs));
    length += 4;

    /* Fill in the final length */
    *( (fm_int32 *) &packet.data[pktOff+FM_MODEL_DATA_TYPE_SIZE] ) = htonl(length);
    msgLength = FM_MODEL_MSG_HEADER_SIZE + metaLen + FM_MODEL_MSG_TLV_SIZE + length;
    packet.msgLength = htonl(msgLength);

    data = (fm_byte*)&packet;
    do
    {
        nb = send(clientSockFd, data, msgLength, 0);
        if (nb == -1)
        {
            FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, FM_FAIL);
        }

        if (nb > 0)
        {
            data = (((fm_byte *) data) + nb);
            msgLength -= nb;
        }
    } while ((nb > 0) && msgLength);

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}

