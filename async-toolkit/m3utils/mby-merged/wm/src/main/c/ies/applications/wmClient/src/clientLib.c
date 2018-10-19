/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */

#include "clientLib.h"
#include "sbiosf.h"

static int debug = 0;
static int clientSockFd;
static int serverSockFd;
static unsigned short portServerPort;
static int portServerClients[MAX_PORTS];

/* ***********************************************************************/
/* Private functions                                                     */
/* ***********************************************************************/

static int createPortServer()
{
    struct sockaddr_in addr;
    socklen_t           addrLen = sizeof(addr);
    int err;

    serverSockFd = socket(AF_INET, SOCK_STREAM, 0);
    if(serverSockFd < 0)
    {
        printf("Error creating port socket server\n");
        exit(0);
    }

    bzero(&addr, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    addr.sin_port = htons(0);

    err = bind(serverSockFd, (struct sockaddr *)&addr, 
                    sizeof(addr));
    if(err != 0)
    {
        printf("Error binding to port socket server %d\n", err);
        exit(0);
    }

    err = listen(serverSockFd, 10);
    if(err != 0)
    {
        printf("Error listening on port socket server %d\n", err);
        exit(0);
    }

    err = getsockname(serverSockFd, (struct sockaddr *)&addr,
                        &addrLen);
    if(err != 0)
    {
        printf("Error getting port socket server name %d\n", err);
        exit(0);
    }

    portServerPort = ntohs(addr.sin_port);
    return portServerPort;
}

static void connectPortServerToWm(unsigned int physPort)
{
    unsigned char buffer[256];
    unsigned int len = 4;
    char hostname[] = "localhost";

    bzero(buffer, 256);

    *( (unsigned short *) &buffer[len]) = htons(2); 
    len +=2;
    *( (unsigned short *) &buffer[len]) = htons(FM_MODEL_MSG_SET_EGRESS_INFO); 
    len += 2;
    *( (unsigned short *) &buffer[len]) = htons(0);        //Sw
    len += 2;
    *( (unsigned short *) &buffer[len]) = htons(physPort);
    len += 2;

    /* Message format is:
     *   2B tcp port, 510B hostname
     */
    *( (unsigned short *) &buffer[len]) = htons(portServerPort);
    len += 2;

    strcpy(&buffer[len], &hostname[0]);
    len += strlen(hostname);

    *( (unsigned int *) &buffer[0]) = htonl(len); 

    write(clientSockFd, &buffer, len);

    /* We sent the connect info, so we should get a client connection
     * request*/
    portServerClients[physPort] = accept(serverSockFd, NULL, NULL);
    if(portServerClients[physPort] < 0) {
        printf("error accepting connection\n");
        exit(0);
    } else if (debug > 1) {
        printf("Client connected\n");
    }
}

/* Parses the WM generated file models.packetServer for hostname and tcp port
 * number. Returns both via host and port.
 */
static void getHostInfo(unsigned char *filename, char *host, int hostSize, unsigned int *port)
{
    FILE *fd;
    char buffer[256];
    char tmp[256];
    unsigned int sw;
    int start, cnt;

    fd = fopen(filename, "r");
    if (fd == NULL)
    {
        printf("Unable to open file %s\n", filename);
        exit(0);
    }

    fgets(&buffer[0], 256, fd);

    {
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
    }

    {
        start = start+cnt+1;
        cnt = 0;
        while( (start+cnt < strlen(buffer))
                && buffer[start+cnt] != ':')
        {
            cnt++;
        }

        bzero(host, hostSize);
        memcpy(host, &buffer[start], cnt);
    }
 
    {
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
}

/* Creates a tpc client socket that connects to the WM
    TCP server cpu port

 char *hostname     The host name the WM app is running on
 int port           The TCP port the server is listening on
*/
static void createClientSocket(char *hostname, int port)
{
    struct sockaddr_in serv_addr;
    struct hostent *server;

    clientSockFd = socket(AF_INET, SOCK_STREAM, 0);
    if(clientSockFd < 0)
    {
        printf("Error creating socket fd\n");
        exit(0);
    }

    server = gethostbyname(hostname);

    if(server == NULL)
    {
        printf("Error unable to find host %s\n", hostname);
    }

    bzero( (char *)&serv_addr, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(port);
	bcopy( (char *)server->h_addr_list[0],
	       (char *)&serv_addr.sin_addr.s_addr, server->h_length);

    if(connect(clientSockFd, (struct sockaddr *)&serv_addr,
               sizeof(serv_addr)) < 0)
    {
        printf("Error unable to connect to server %s(%08x)\n",
               hostname, serv_addr.sin_addr.s_addr);
        exit(0);
    }
}


/* Read from a socket with timeout

 int socket            The socket descriptor to read from
 unsigned char *data   The data buffer to store received data
 int dataSize          The size of data buffer
 int timeoutMsec       The timeout if millisecond to return if no response
*/
static unsigned int readData(int socket, unsigned char *data, int dataSize,
                             int timeoutMsec)
{
    struct pollfd fds[1] = { {0} };
    int           fdsCnt;
    int           fdsTimeout;
    int           errResult;


    if (timeoutMsec >= 0)
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

    if (timeoutMsec)
    {
        printf("Timeout waiting for message\n");
        return -1;
    }

    return 0;
}

int checkEventMsg()
{
    unsigned char buffer[256];
    unsigned int len = 0;
    unsigned int d, n;
    unsigned char type;
    unsigned short msgType;
    unsigned char opcode;

    bzero(buffer, 256);
    n = readData(clientSockFd, buffer, 4, 0);
    if (n <= 0)
    {
        return n;
    }
    if (n != 4)
    {
        printf("Expect %d bytes but got %d\n", 4, n);
        return -1;
    }
    
    len = ntohl( *( (unsigned int *) &buffer[0]));
    if (len > sizeof(buffer))
    {
        printf("Length %d is out of bound\n", len);
        return -1;
    }
    n = readData(clientSockFd, &buffer[4], len-4, READ_TIMEOUT);
    if (n == -1)
    {
        return n;
    }
    if (n != len-4)
    {
        printf("Expect %d bytes but got %d\n", len-4, n);
        return -1;
    }

    msgType = ntohs(*( (unsigned short *) &buffer[6]));
    switch (msgType)
    {
        case FM_MODEL_MSG_ERROR:
            printf("%s: %s\n", __func__, buffer + 12);
            return -1;
        case FM_MODEL_MSG_IOSF:
            break;
        default:
            printf("%s: Unexpected msgType %d\n", __func__, msgType);
            return -1;
    }

    opcode = buffer[12 + 2];
    switch(opcode)
    {
        case 0x7:
            printf("Got IOSF GLOBAL_INTR  = 0x%llx\n",
                *(unsigned long int *)&buffer[12 + 12]);
            break;
        default:
            printf("Got opcode 0x%x\n", buffer[12 + 2]);
            break;
    }

    /* Check again until empty */
    return checkEventMsg();

}


/*************************************************************************/
/* public functions                                                      */
/*************************************************************************/

int resetChip()
{
    unsigned char buffer[256];
    unsigned int len = 0;
    unsigned int d, n;
    unsigned char type;
    unsigned short msgType;

    bzero(buffer, 256);

    *( (unsigned short *) &buffer[4]) = htons(2); 
    len +=2;
    *( (unsigned short *) &buffer[6]) = htons(FM_MODEL_MSG_CTRL); 
    len += 2;
    *( (unsigned short *) &buffer[8]) = htons(0);        //Sw
    len += 2;
    *( (unsigned short *) &buffer[10]) = htons(0);
    len += 2;

    /* Message format is:
     *   1B type, 4B addr, 4B data 
     */
    buffer[12] = FM_MODEL_CTRL_CHIP_RESET_REQ;
    len ++;

    len += 4;
    *( (unsigned int *) &buffer[0]) = htonl(len); 

    write(clientSockFd, &buffer, len);

    bzero(buffer, 256);
    n = readData(clientSockFd, buffer, 4, 30*1000);
    if (n == -1)
    {
        return n;
    }
    if (n != 4)
    {
        printf("Expect %d bytes but got %d\n", 4, n);
        return -1;
    }

    len = ntohl( *( (unsigned int *) &buffer[0]));
    n = readData(clientSockFd, &buffer[4], len-4, READ_TIMEOUT);
    if (n == -1)
    {
        return n;
    }
    if (n != len-4)
    {
        printf("Expect %d bytes but got %d\n", len-4, n);
        return -1;
    }

    msgType = ntohs(*( (unsigned short *) &buffer[6]));
    switch (msgType)
    {
        case FM_MODEL_MSG_ERROR:
            printf("%s: %s\n", __func__, buffer + 12);
            return -1;
        case FM_MODEL_MSG_CTRL:
            break;
        default:
            printf("%s: Unexpected msgType %d\n", __func__, msgType);
            return -1;
    }

    type = buffer[12];
    if (type != 0)
    {
        printf("Error unexpected type %d\n", type);
        return -1;
    }

    if (buffer[13])
    {
        printf("Error: %s\n", buffer + 14);
        return -1;
    }
    else
    {
        printf("Reset done\n");
    }

    return 0;
}

#if 0
int readUINT32(unsigned int addr, unsigned int *value)
{
    unsigned char buffer[256];
    unsigned int d,n;
    unsigned char type;
    unsigned int len = 0;
    unsigned short msgType;

    bzero(buffer, 256);
    
    *( (unsigned short *) &buffer[4]) = htons(2); 
    len +=2;
    *( (unsigned short *) &buffer[6]) = htons(FM_MODEL_MSG_MGMT); 
    len += 2;
    *( (unsigned short *) &buffer[8]) = htons(0);        //Sw
    len += 2;
    *( (unsigned short *) &buffer[10]) = htons(NONPOSTED_PORT);
    len += 2;

    /* Message format is:
     *   1B type, 4B addr, 4B data 
     */
    buffer[12] = FM_MODEL_MGMT_READ_REQUEST;
    len ++;
    *( (unsigned int *) &buffer[13]) = htonl(addr);
    len += 4;
    *( (unsigned int *) &buffer[17]) = htonl(0);       //Data
    len += 4;
    
    len += 4;
    *( (unsigned int *) &buffer[0]) = htonl(len); 

    write(clientSockFd, &buffer, len);

    bzero(buffer, 256);
    n = readData(clientSockFd, buffer, 4, READ_TIMEOUT);
    if (n == -1)
    {
        return n;
    }
    if (n != 4)
    {
        printf("Expect %d bytes but got %d\n", 4, n);
        return -1;
    }
    
    len = ntohl( *( (unsigned int *) &buffer[0]));
    n = readData(clientSockFd, &buffer[4], len-4, READ_TIMEOUT);
    if (n == -1)
    {
        return n;
    }
    if (n != len-4)
    {
        printf("Expect %d bytes but got %d\n", len-4, n);
        return -1;
    }

    msgType = ntohs(*( (unsigned short *) &buffer[6]));
    switch (msgType)
    {
        case FM_MODEL_MSG_ERROR:
            printf("%s: %s\n", __func__, buffer + 12);
            return -1;
        case FM_MODEL_MSG_MGMT:
            break;
        default:
            printf("%s: Unexpected msgType %d\n", __func__, msgType);
            return -1;
    }

    type = buffer[12];
    addr = ntohl( *( (unsigned int *) &buffer[13]));
    *value = ntohl( *( (unsigned int *) &buffer[17]));

    if(type != FM_MODEL_MGMT_READ_RESPONSE)
    {
        printf("Error unexpected type %d\n", type);
        return -1;
    }

    return 0;
}

int writeUINT32(unsigned int addr, unsigned int value)
{
    unsigned char buffer[256];
    unsigned int len = 0;
    unsigned int d, n;
    unsigned char type;
    unsigned short msgType;

    if (debug) printf("writing 0x%x data 0x%x\n", addr, value);
    bzero(buffer, 256);

    *( (unsigned short *) &buffer[4]) = htons(2); 
    len +=2;
    *( (unsigned short *) &buffer[6]) = htons(FM_MODEL_MSG_MGMT); 
    len += 2;
    *( (unsigned short *) &buffer[8]) = htons(0);        //Sw
    len += 2;
    *( (unsigned short *) &buffer[10]) = htons(NONPOSTED_PORTNONPOSTED_PORT);
    len += 2;

    /* Message format is:
     *   1B type, 4B addr, 4B data 
     */
    buffer[12] = FM_MODEL_MGMT_WRITE;
    len ++;
    *( (unsigned int *) &buffer[13]) = htonl(addr);
    len += 4;
    *( (unsigned int *) &buffer[17]) = htonl(value);
    len += 4;
    
    len += 4;
    *( (unsigned int *) &buffer[0]) = htonl(len); 

    write(clientSockFd, &buffer, len);

    bzero(buffer, 256);
    n = readData(clientSockFd, buffer, 4, READ_TIMEOUT);
    if (n == -1)
    {
        return n;
    }
    if (n != 4)
    {
        printf("Expect %d bytes but got %d\n", 4, n);
        return -1;
    }
    
    len = ntohl( *( (unsigned int *) &buffer[0]));
    n = readData(clientSockFd, &buffer[4], len-4, READ_TIMEOUT);
    if (n == -1)
    {
        return n;
    }
    if (n != len-4)
    {
        printf("Expect %d bytes but got %d\n", len-4, n);
        return -1;
    }

    msgType = ntohs(*( (unsigned short *) &buffer[6]));
    switch (msgType)
    {
        case FM_MODEL_MSG_ERROR:
            printf("%s: %s\n", __func__, buffer + 12);
            return -1;
        case FM_MODEL_MSG_MGMT:
            break;
        default:
            printf("%s: Unexpected msgType %d\n", __func__, msgType);
            return -1;
    }

    type = buffer[12];
    if(type != FM_MODEL_MGMT_WRITE_ACK)
    {
        printf("Error unexpected type %d\n", type);
        return -1;
    }

    return 0;
}

int readUINT64(unsigned int addr, unsigned long int *value)
{
    unsigned char buffer[256];
    unsigned int d,n;
    unsigned char type;
    unsigned int len = 0;
    unsigned short msgType;

    bzero(buffer, 256);
    
    *( (unsigned short *) &buffer[4]) = htons(2); 
    len +=2;
    *( (unsigned short *) &buffer[6]) = htons(FM_MODEL_MSG_MGMT); 
    len += 2;
    *( (unsigned short *) &buffer[8]) = htons(0);        //Sw
    len += 2;
    *( (unsigned short *) &buffer[10]) = htons(NONPOSTED_PORT);
    len += 2;

    /* Message format is:
     *   1B type, 4B addr, 8B data 
     */
    buffer[12] = FM_MODEL_MGMT_READ64_REQUEST;
    len ++;
    *( (unsigned int *) &buffer[13]) = htonl(addr);
    len += 4;
    *( (unsigned int *) &buffer[17]) = htonl(0);       //Data
    len += 4;
    *( (unsigned int *) &buffer[21]) = htonl(0);       //Data
    len += 4;
    
    len += 4;
    *( (unsigned int *) &buffer[0]) = htonl(len); 

    write(clientSockFd, &buffer, len);

    bzero(buffer, 256);
    n = readData(clientSockFd, buffer, 4, READ_TIMEOUT);
    if (n == -1)
    {
        return n;
    }
    if (n != 4)
    {
        printf("Expect %d bytes but got %d\n", 4, n);
        return -1;
    }
    
    len = ntohl( *( (unsigned int *) &buffer[0]));
    n = readData(clientSockFd, &buffer[4], len-4, READ_TIMEOUT);
    if (n == -1)
    {
        return n;
    }
    if (n != len-4)
    {
        printf("Expect %d bytes but got %d\n", len-4, n);
        return -1;
    }

    msgType = ntohs(*( (unsigned short *) &buffer[6]));
    switch (msgType)
    {
        case FM_MODEL_MSG_ERROR:
            printf("%s: %s\n", __func__, buffer + 12);
            return -1;
        case FM_MODEL_MSG_MGMT:
            break;
        default:
            printf("%s: Unexpected msgType %d\n", __func__, msgType);
            return -1;
    }

    type = buffer[12];
    addr = ntohl( *( (unsigned int *) &buffer[13]));
    *value = ntohl( *( (unsigned int *) &buffer[17]));
    *value <<= 32;
    *value |= ntohl( *( (unsigned int *) &buffer[21]));

    if (type != FM_MODEL_MGMT_READ64_RESPONSE)
    {
        printf("Error unexpected MGMT type %d\n", type);
        return -1;
    }

    return 0;
}

int writeUINT64(unsigned int addr, unsigned long int value)
{
    unsigned char buffer[256];
    unsigned int len = 0;
    unsigned int d, n;
    unsigned char type;
    unsigned short msgType;

    if (debug > 1) printf("writing 0x%x data 0x%llx\n", addr, value);
    bzero(buffer, 256);

    *( (unsigned short *) &buffer[4]) = htons(2); 
    len +=2;
    *( (unsigned short *) &buffer[6]) = htons(FM_MODEL_MSG_MGMT); 
    len += 2;
    *( (unsigned short *) &buffer[8]) = htons(0);        //Sw
    len += 2;
    *( (unsigned short *) &buffer[10]) = htons(NONPOSTED_PORT);
    len += 2;

    /* Message format is:
     *   1B type, 4B addr, 4B data 
     */
    buffer[12] = FM_MODEL_MGMT_WRITE64;
    len ++;
    *( (unsigned int *) &buffer[13]) = htonl(addr);
    len += 4;
    *( (unsigned int *) &buffer[17]) = htonl(value>>32);
    len += 4;
    *( (unsigned int *) &buffer[21]) = htonl(value);
    len += 4;
    
    len += 4;
    *( (unsigned int *) &buffer[0]) = htonl(len); 

    write(clientSockFd, &buffer, len);

    bzero(buffer, 256);
    n = readData(clientSockFd, buffer, 4, READ_TIMEOUT);
    if (n == -1)
    {
        return n;
    }
    if (n != 4)
    {
        printf("Expect %d bytes but got %d\n", 4, n);
        return -1;
    }
    
    len = ntohl( *( (unsigned int *) &buffer[0]));
    n = readData(clientSockFd, &buffer[4], len-4, READ_TIMEOUT);
    if (n == -1)
    {
        return n;
    }
    if (n != len-4)
    {
        printf("Expect %d bytes but got %d\n", len-4, n);
        return -1;
    }

    msgType = ntohs(*( (unsigned short *) &buffer[6]));
    switch (msgType)
    {
        case FM_MODEL_MSG_ERROR:
            printf("%s: %s\n", __func__, buffer + 12);
            return -1;
        case FM_MODEL_MSG_MGMT:
            break;
        default:
            printf("%s: Unexpected msgType %d\n", __func__, msgType);
            return -1;
    }

    type = buffer[12];
    if (type != FM_MODEL_MGMT_WRITE64_ACK)
    {
        printf("Error unexpected type %d\n", type);
        return -1;
    }

    return 0;
}
#endif





int readUINT64(unsigned int addr, unsigned long int *value)
{
    struct sbiosf_msg *msg;
    unsigned int *array;
    int dwords;
    unsigned char buffer[128];
    unsigned int n;
    unsigned short msgType;
    unsigned int rsp;
    unsigned int len = 0;

    checkEventMsg();

    msg = create_reg_read_sbiosf_msg(1);
    add_rd_wr_op_to_msg(msg, 0, addr, 2, NULL);
    if (debug > 2) print_sbiosf_msg(msg);
    array = pack_sbiosf_msg(msg, &dwords);
    if (debug > 2) print_packed_array(array, dwords);

    bzero(buffer, sizeof(buffer));
    
    *( (unsigned short *) &buffer[4]) = htons(2); 
    *( (unsigned short *) &buffer[6]) = htons(FM_MODEL_MSG_IOSF); 
    *( (unsigned short *) &buffer[8]) = htons(0);        //Sw
    *( (unsigned short *) &buffer[10]) = htons(NONPOSTED_PORT);

    memcpy(&buffer[12], array, 16);
    free(array);
    free_sbiosf_msg(msg);

    len = 12 + 16;
    *( (unsigned int *) &buffer[0]) = htonl(len); 

    write(clientSockFd, &buffer, len);

    bzero(buffer, sizeof(buffer));
    n = readData(clientSockFd, buffer, 4, READ_TIMEOUT);
    if (n == -1)
    {
        return n;
    }
    if (n != 4)
    {
        printf("Expect %d bytes but got %d\n", 4, n);
        return -1;
    }
    
    len = ntohl( *( (unsigned int *) &buffer[0]));
    if (len > sizeof(buffer))
    {
        printf("Length %d is out of bound\n", len);
        return -1;
    }
    n = readData(clientSockFd, &buffer[4], len-4, READ_TIMEOUT);
    if (n == -1)
    {
        return n;
    }
    if (n != len-4)
    {
        printf("Expect %d bytes but got %d\n", len-4, n);
        return -1;
    }

    msgType = ntohs(*( (unsigned short *) &buffer[6]));
    switch (msgType)
    {
        case FM_MODEL_MSG_ERROR:
            printf("%s: %s\n", __func__, buffer + 12);
            return -1;
        case FM_MODEL_MSG_IOSF:
            break;
        default:
            printf("%s: Unexpected msgType %d\n", __func__, msgType);
            return -1;
    }

    /* Temp code until unpack is added */
    //msg = unpack_sbiosf_msg(buffer + 12, len - 8);
    rsp = (buffer[12 + 3] >> 3) & 0x3;
    if (rsp)
    {
        return -1;
    }

    *value =  *( (unsigned int *) &buffer[12 + 12]);
    *value <<= 32;
    *value |=  *( (unsigned int *) &buffer[12 + 8]);

    return 0;
}


int readUINTMult64(unsigned int addr, unsigned int numRead, unsigned long int *value)
{
    struct sbiosf_msg *msg;
    unsigned int *array;
    int dwords;
    unsigned char buffer[512 + 64];
    unsigned int n;
    unsigned int rsp;
    unsigned short msgType;
    unsigned int len = 0;

    checkEventMsg();

    msg = create_block_read_sbiosf_msg(1);
    add_rd_wr_op_to_msg(msg, 0, addr, 2*numRead, NULL);
    if (debug > 2) print_sbiosf_msg(msg);
    array = pack_sbiosf_msg(msg, &dwords);
    if (debug > 2) print_packed_array(array, dwords);

    bzero(buffer, sizeof(buffer));
    
    *( (unsigned short *) &buffer[4]) = htons(2); 
    *( (unsigned short *) &buffer[6]) = htons(FM_MODEL_MSG_IOSF); 
    *( (unsigned short *) &buffer[8]) = htons(0);        //Sw
    *( (unsigned short *) &buffer[10]) = htons(NONPOSTED_PORT);

    memcpy(&buffer[12], array, 16);
    free(array);
    free_sbiosf_msg(msg);

    len = 12 + 16;
    *( (unsigned int *) &buffer[0]) = htonl(len); 

    write(clientSockFd, &buffer, len);

    bzero(buffer, sizeof(buffer));
    n = readData(clientSockFd, buffer, 4, READ_TIMEOUT);
    if (n == -1)
    {
        return n;
    }
    if (n != 4)
    {
        printf("Expect %d bytes but got %d\n", 4, n);
        return -1;
    }
    
    len = ntohl( *( (unsigned int *) &buffer[0]));
    if (len > sizeof(buffer))
    {
        printf("Length %d is out of bound\n", len);
        return -1;
    }
    n = readData(clientSockFd, &buffer[4], len-4, READ_TIMEOUT);
    if (n == -1)
    {
        return n;
    }
    if (n != len-4)
    {
        printf("Expect %d bytes but got %d\n", len-4, n);
        return -1;
    }

    msgType = ntohs(*( (unsigned short *) &buffer[6]));
    switch (msgType)
    {
        case FM_MODEL_MSG_ERROR:
            printf("%s: %s\n", __func__, buffer + 12);
            return -1;
        case FM_MODEL_MSG_IOSF:
            break;
        default:
            printf("%s: Unexpected msgType %d\n", __func__, msgType);
            return -1;
    }

    /* Temp code until unpack is added */
    //msg = unpack_sbiosf_msg(buffer + 12, len - 8);
    rsp = (buffer[12 + 3] >> 3) & 0x3;
    if (rsp)
    {
        return -1;
    }

    for (n - 0; n < numRead; n++)
    {
        value[n] =  *( (unsigned int *) &buffer[12 + 12 + n*8]);
        value[n] <<= 32;
        value[n] |=  *( (unsigned int *) &buffer[12 + 8 + n*8]);
    }


    return 0;
}



int writeUINT64(unsigned int addr, unsigned long int value)
{
    struct sbiosf_msg *msg;
    unsigned int *array;
    int dwords;
    unsigned char buffer[512+64];
    unsigned int n;
    unsigned short msgType;
    unsigned int len = 0;
    unsigned long *d;
    unsigned int rsp;

    checkEventMsg();

    msg = create_reg_write_sbiosf_msg(1);
    d = malloc(3*sizeof(d));
    d[0] = value;
    add_rd_wr_op_to_msg(msg, 0, addr, 2, d);
    if (debug > 2) print_sbiosf_msg(msg);
    array = pack_sbiosf_msg(msg, &dwords);
    if (debug > 2) print_packed_array(array, dwords);

    bzero(buffer, sizeof(buffer));
    
    *( (unsigned short *) &buffer[4]) = htons(2); 
    *( (unsigned short *) &buffer[6]) = htons(FM_MODEL_MSG_IOSF); 
    *( (unsigned short *) &buffer[8]) = htons(0);        //Sw
    *( (unsigned short *) &buffer[10]) = htons(NONPOSTED_PORT);

    memcpy(&buffer[12], array, 24);
    free(array);
    free_sbiosf_msg(msg);

    len = 12 + 24;
    *( (unsigned int *) &buffer[0]) = htonl(len); 

    write(clientSockFd, &buffer, len);

    bzero(buffer, sizeof(buffer));
    n = readData(clientSockFd, buffer, 4, READ_TIMEOUT);
    if (n == -1)
    {
        return n;
    }
    if (n != 4)
    {
        printf("Expect %d bytes but got %d\n", 4, n);
        return -1;
    }
    
    len = ntohl( *( (unsigned int *) &buffer[0]));
    if (len > sizeof(buffer))
    {
        printf("Length %d is out of bound\n", len);
        return -1;
    }
    n = readData(clientSockFd, &buffer[4], len-4, READ_TIMEOUT);
    if (n == -1)
    {
        return n;
    }
    if (n != len-4)
    {
        printf("Expect %d bytes but got %d\n", len-4, n);
        return -1;
    }

    msgType = ntohs(*( (unsigned short *) &buffer[6]));
    switch (msgType)
    {
        case FM_MODEL_MSG_ERROR:
            printf("%s: %s\n", __func__, buffer + 12);
            return -1;
        case FM_MODEL_MSG_IOSF:
            break;
        default:
            printf("%s: Unexpected msgType %d\n", __func__, msgType);
            return -1;
    }

    /* Temp code until unpack is added */
    //msg = unpack_sbiosf_msg(buffer + 12, len - 8);

    rsp = (buffer[12 + 3] >> 3) & 0x3;
    if (rsp)
    {
        return -1;
    }

    return 0;
}

int writeUINTMult64(unsigned int addr, unsigned int numVal, unsigned long int *value)
{
    struct sbiosf_msg *msg;
    unsigned int *array;
    int dwords;
    unsigned char buffer[128];
    unsigned int n;
    unsigned short msgType;
    unsigned int len = 0;
    unsigned long *d;
    unsigned int rsp;
    unsigned int i;

    checkEventMsg();

    if (numVal > 124/2)
    {
        printf("Only supporting %d entries\n", 124/2);
        return -1;
    }

    msg = create_block_write_sbiosf_msg(1);
    d = malloc(numVal*sizeof(d));
    for (int i  = 0 ; i < numVal; i++) {
        d[i] = value[i];
    }

    add_rd_wr_op_to_msg(msg, 0, addr, numVal*2, d);

    if (debug > 2) print_sbiosf_msg(msg);
    array = pack_sbiosf_msg(msg, &dwords);
    if (debug > 2) print_packed_array(array, dwords);

    bzero(buffer, sizeof(buffer));
    
    *( (unsigned short *) &buffer[4]) = htons(2); 
    *( (unsigned short *) &buffer[6]) = htons(FM_MODEL_MSG_IOSF); 
    *( (unsigned short *) &buffer[8]) = htons(0);        //Sw
    *( (unsigned short *) &buffer[10]) = htons(NONPOSTED_PORT);

    memcpy(&buffer[12], array, dwords*4);
    free(array);
    free_sbiosf_msg(msg);

    len = 12 + dwords*4;
    *( (unsigned int *) &buffer[0]) = htonl(len); 

    write(clientSockFd, &buffer, len);

    bzero(buffer, sizeof(buffer));
    n = readData(clientSockFd, buffer, 4, READ_TIMEOUT);
    if (n == -1)
    {
        return n;
    }
    if (n != 4)
    {
        printf("Expect %d bytes but got %d\n", 4, n);
        return -1;
    }
    
    len = ntohl( *( (unsigned int *) &buffer[0]));
    if (len > sizeof(buffer))
    {
        printf("Length %d is out of bound\n", len);
        return -1;
    }
    n = readData(clientSockFd, &buffer[4], len-4, READ_TIMEOUT);
    if (n == -1)
    {
        return n;
    }
    if (n != len-4)
    {
        printf("Expect %d bytes but got %d\n", len-4, n);
        return -1;
    }

    msgType = ntohs(*( (unsigned short *) &buffer[6]));
    switch (msgType)
    {
        case FM_MODEL_MSG_ERROR:
            printf("%s: %s\n", __func__, buffer + 12);
            return -1;
        case FM_MODEL_MSG_IOSF:
            break;
        default:
            printf("%s: Unexpected msgType %d\n", __func__, msgType);
            return -1;
    }

    /* Temp code until unpack is added */
    //msg = unpack_sbiosf_msg(buffer + 12, len - 8);

    rsp = (buffer[12 + 3] >> 3) & 0x3;
    if (rsp)
    {
        return -1;
    }

    return 0;
}


void sendPkt(int sw,
             int physPort,
             unsigned char *sendData,
             int length,
             unsigned char *pktMeta,
             int metaLen, int logPort)
{
    unsigned char data[MAX_MSG_BUF];
    int           off;
    int           pktOff;
    int           i;

    checkEventMsg();

    off = 0;
    *( (unsigned int *) &data[off]) = htonl(0); //len to be set later
    off +=4;
    *( (unsigned short *) &data[off]) = htons(2);
    off +=2;
    *( (unsigned short *) &data[off]) = htons(FM_MODEL_MSG_PACKET); 
    off +=2;
    *( (unsigned short *) &data[off]) = htons(sw);
    off +=2;
    *( (unsigned short *) &data[off]) = htons(physPort);
    off +=2;


    /* Packet meta TLV */
    off         = 12;
    data[off]   = FM_MODEL_PACKET_META; //Type
    off        +=FM_MODEL_DATA_TYPE_SIZE;
    if (metaLen < 0 || metaLen > FM_MODEL_CPK_META_SIZE)
    {
        printf("Meta length %d is out of range\n", metaLen);
        return;
    }

    if (metaLen)
    {
        *( (unsigned int*) &data[off] ) =
                                htonl(metaLen); //Length
        off += FM_MODEL_DATA_LENGTH_SIZE;
        for (i = 0; i < metaLen; i++)
        {
            data[off++] = pktMeta[i];
        }
        if (logPort > 0)
        {
            printf("WARNING: Packet sent with user-specified packet meta. Ignore logPort %d value.\n",
                   logPort);
        }
    }
    else if (logPort >= 0)
    {
        *( (unsigned int*) &data[off] ) =
                                htonl(FM_MODEL_CPK_META_SIZE); //Length
        off        += FM_MODEL_DATA_LENGTH_SIZE;

        for (i = 0; i < FM_MODEL_CPK_META_SIZE; i++)
        {
            data[off + i] = 0;
        }

        data[off] = 0x01; /* Type from CPK to HLP */
        /* Dest. Port */
        /* Fwd */
        data[off+2] = (logPort & 0x1F) | (1 << 7);
        /* PF */
        data[off+3] = (1 << 7);

        off += FM_MODEL_CPK_META_SIZE;
    }
    else
    {
        *( (unsigned int*) &data[off] ) =
                                htonl(FM_MODEL_RIMMON_META_SIZE); //Length
        off        += FM_MODEL_DATA_LENGTH_SIZE;
        data[off++] = 0x18; /* Type from Rimmon to HLP */
        data[off++] = 0xa1;
        data[off++] = 0xb2;
        data[off++] = 0xc3;
        data[off++] = 0xd4;
        data[off++] = 0xe5;
        data[off++] = 0xf6;
        data[off++] = 0x07;
    }

    /* Packet data TLV */
    data[off]   = FM_MODEL_DATA_PACKET;
    *( (unsigned int *) &data[off + FM_MODEL_DATA_TYPE_SIZE] ) =
                            htonl(length);
    off        += FM_MODEL_MSG_TLV_SIZE;

    /* Start of packet data */
    pktOff      = off;
    for ( i = 0; i < length ; i++ )
    {
        data[off++] = sendData[i];
    }

    /* Set the total message length */
    *( (unsigned int *) &data[0]) = htonl(off);
    write(clientSockFd, &data, off);

}   /* end rimmonSendPkt */


void cpkSendPkt(int sw,
                   int physPort,
                   unsigned char *sendData,
                   int length)
{
    /* Use cpkPort to flood or switch */
    sendPkt(sw, physPort, sendData, length, NULL, 0, 20);
}

void rimmonSendPkt(int sw,
                   int physPort,
                   unsigned char *sendData,
                   int length)
{
#if 1
    sendPkt(sw, physPort, sendData, length, NULL, 0, -1);
#else
    unsigned char data[MAX_MSG_BUF];
    int           off;
    int           pktOff;
    int           i;

    checkEventMsg();

    off = 0;
    *( (unsigned int *) &data[off]) = htonl(0); //len to be set later
    off +=4;
    *( (unsigned short *) &data[off]) = htons(2);
    off +=2;
    *( (unsigned short *) &data[off]) = htons(FM_MODEL_MSG_PACKET); 
    off +=2;
    *( (unsigned short *) &data[off]) = htons(sw);
    off +=2;
    *( (unsigned short *) &data[off]) = htons(physPort);
    off +=2;


    /* Packet meta TLV */
    off         = 12;
    data[off]   = FM_MODEL_PACKET_META; //Type
    off        +=FM_MODEL_DATA_TYPE_SIZE;
    *( (unsigned int*) &data[off] ) =
                                htonl(FM_MODEL_RIMMON_META_SIZE); //Length
    off        += FM_MODEL_DATA_LENGTH_SIZE;
    data[off++] = 0x18; /* Type from Rimmon to HLP */
    data[off++] = 0xa1;
    data[off++] = 0xb2;
    data[off++] = 0xc3;
    data[off++] = 0xd4;
    data[off++] = 0xe5;
    data[off++] = 0xf6;
    data[off++] = 0x07;

    /* Packet data TLV */
    data[off]   = FM_MODEL_DATA_PACKET;
    *( (unsigned int *) &data[off + FM_MODEL_DATA_TYPE_SIZE] ) =
                            htonl(length);
    off        += FM_MODEL_MSG_TLV_SIZE;

    /* Start of packet data */
    pktOff      = off;
    for ( i = 0; i < length ; i++ )
    {
        data[off++] = sendData[i];
    }

    /* Set the total message length */
    *( (unsigned int *) &data[0]) = htonl(off);
    write(clientSockFd, &data, off);
#endif

}   /* end rimmonSendPkt */

void cleanup()
{
    shutdown(clientSockFd, SHUT_WR);
    if(serverSockFd != -1)
    {
        shutdown(serverSockFd, SHUT_WR);
    }
}

void setDebug(int val)
{
    debug = val;
}

void connectCpuToWm(char *filename)
{
    unsigned char hostname[256];
    unsigned int port;
    int serverPort;

    serverSockFd = -1;
    for(int i = 0; i < MAX_PORTS; i++)
    {
        portServerClients[i] = -1;
    }

    getHostInfo(filename, &hostname[0], sizeof(hostname), &port);
    

    if (debug)
        printf("Connecting client to WM on %s:%d\n", hostname, port);
    createClientSocket(hostname, port);

    /* Create a server port, packets egressing WM will arrive here */
    serverPort = createPortServer();
    if (debug)
        printf("Created port server on localhost:%d\n", serverPort);
}

void connectToWmEthernetPort(unsigned int physPort)
{
    if (debug > 1)
        printf("Connecting to WM port %d\n", physPort);
    connectPortServerToWm(physPort);
}

void printMsg(unsigned char *d, int l)
{
    for(int i = 0; i < l ; i++) {
        if(i % 16 == 0) printf("[%04d]: ", i);
        printf("%02x ", d[i]);
        if(i % 16 == 15) printf("\n");
    }
}

/********************************************************************************* 
    Receive a packet from the WM 
 ********************************************************************************* 
    unsigned int physPort            
                        The WM port to receive a packet on. 

    int timeout         
                        in ms, -1 == infinite 

    unsigned char **packet       
                        function will return a pointer to allocated space
                        containing the packet received. User will need to 
                        free this pointer if not null .

    unsigned char *sbData  
                        function will return a pointer to allocated
                        space containing the sideband data received.
                        Assume max length of 32.

    unsigned int *len   
                        Function will return the length of the packet
                        data received (unsigned char **packet)
                        
 ********************************************************************************/
void recvPacket(unsigned int physPort,
                int timeout,
                unsigned char **packet,
                unsigned int *len,
                unsigned char sbData[])
{
    unsigned char   buffer[256];
    struct pollfd   fds[1];
    struct sockaddr addr;
    socklen_t       addrLen;
    ssize_t         nb;
    int             msgLen;
    unsigned char tmp[16000];
    int err;
    unsigned int tlvLen;

    if(portServerClients[physPort] == -1)
    {
        printf("Error call connectPortServerToWM on physPort %d\n",
            physPort);
        exit(0);
    }

    bzero(fds, sizeof(struct pollfd));
    fds[0].fd = portServerClients[physPort];
    fds[0].events = POLLIN;

    nb = poll(fds, 1, timeout);
    if(!(fds[0].revents & POLLIN))
    {
        *packet = NULL;
        return;
    }

    nb = recv(portServerClients[physPort], &msgLen, 4, 0 );
    if(nb == -1)
    {
        *packet = NULL;
        return;
    } else if (nb != 4)
    {
        printf("Error expected to receive 4Bytes saw %d\n", nb);
        exit(0);
    } else if (debug > 1)
    {
        printf("received msgLen %d\n", ntohl(msgLen));
    }
    msgLen = ntohl(msgLen);

    /* header is: len(4), vers(2), type(2), sw(2), port(2), {type(1), len(4),
     * data(N)}*/
    nb = recv(portServerClients[physPort], &buffer[0], 13, 0);

    if(nb != 13)
    {
        printf("Unable to receive message header saw %d bytes\n", nb);
        exit(0);
    }

    if ( ntohs(*((unsigned short*)(&buffer[0]))) != 2)
    {
        printf("Invalid message version number %d\n",
                    ntohs(*((unsigned short*)(&buffer[0]))) );
        exit(0);
    }

    if (ntohs(*((unsigned short*)(&buffer[2]))) != FM_MODEL_MSG_PACKET)
    {
        printf("only expecting packet message types saw %d\n",
                    ntohs(*((unsigned short*)(&buffer[2]))) );
        exit(0);
    }

    if ( (ntohs(*((unsigned short*)(&buffer[4]))) != 0)  &&
         ( ntohs(*((unsigned short*)(&buffer[6])))!= 0))
    {
        printf("expected sw and port to be 0, saw %d and %d\n",
                    ntohs(*((unsigned short*)(&buffer[4]))),
                    ntohs(*((unsigned short*)(&buffer[6]))) );
        exit(0);
    }

    /* FIXME This needs to support PACKET_META */
    if(buffer[8] != 0xA0)
    {
        printf("Unsupported data type %d\n", buffer[8]);
    }

    *len = ntohl(* (unsigned int *)&buffer[9]);

    *packet = (unsigned char *)malloc(*len * sizeof(unsigned char));

    if(*packet == NULL)
    {
        printf("Unable to allocate %d bytes for packet \n", msgLen);
        exit(1);
    }

    nb = recv(portServerClients[physPort], *packet, *len, 0);
    if(nb != *len)
    {
        printf("Did not receive rest of packet data, saw %d expected %d\n",
                    nb, *len);
        exit(1);
    }

    msgLen -= nb;

    //Empty the rest of the message looking for metadata
    while (msgLen > 0) {
        /* Getting MetatData */
        nb = recv(portServerClients[physPort], &buffer[0], 5, 0);

        if(nb != 5)
        {
            printf("Unable to receive tlv header saw %d bytes\n", nb);
            exit(0);
        }

        /* Receive the remaining data. */
        tlvLen = ntohl(* (unsigned int *)&buffer[1]);
        msgLen -= tlvLen;

        if(buffer[0] == 0xA3)
        {
            nb = recv(portServerClients[physPort], sbData, tlvLen, 0);
            if (nb != 32)
            {
                printf("Expect 32 bytes of metadata but read %d, tlvLen %d\n",
                       nb, tlvLen);
            }
        } else if (tlvLen < sizeof(buffer)) {
            nb = recv(portServerClients[physPort], buffer, tlvLen, 0);
            if (nb != tlvLen)
            {
                printf("Expect %d bytes of metadata but read %d\n",
                       tlvLen, nb);
            }

        } else {
            printf("Not enough space to flush %d bytes\n", tlvLen);
            exit(-1);
        }
        msgLen -= nb;
    }
}

static int phys_to_log_map[32] = {
		15, 14, 13, 12,
		22, -1, -1, -1,		/* CPM */
		16, 17 , 18, 19,
		21, -1, -1, -1,		/* CPM */
		0, 1, 2, 3,
		8, 9, 10, 11,
		7, 6, 5, 4,
		20, -1, -1, -1};	/* CPK */

int map_phys_to_log(int phys)
{
    if (phys < 0 || phys >= 32)
    {
        printf("Invalid phys port %d\n", phys);
        return 0;
    }
    return phys_to_log_map[phys];
}

int map_log_to_phys(int log)
{
    int phys;

    for (phys = 0; phys < 32; phys++)
    {
        if (phys_to_log_map[phys] == log)
            return phys;
    }
    printf("Invalid log port %d\n", log);
    return 0;
}
