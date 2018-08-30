
#ifndef _MBY_SERVER_SOCKET_H_
#define _MBY_SERVER_SOCKET_H_

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h> /* superset of previous */
#include <netinet/tcp.h>

#include "mby_model.h"
#include "mby_srv_time.h"

#define MBY_MODEL_FILE "models.packetServer"

#define MAX_PERSISTENT_CONNECTIONS          24

typedef enum
{
    /* Indicates a closed socket */
    FM_SOCKET_TYPE_CLOSED,

    /* Indicates a socket of type SOCK_STREAM */
    FM_SOCKET_TYPE_TCP,

    /* Indicates a socket of type SOCK_DGRAM */
    FM_SOCKET_TYPE_UDP,

    /* Indicates type named pipe instead of socket */
    FM_SOCKET_TYPE_PIPE,

    /** For internal use only */
    FM_SOCKET_TYPE_MAX

} fm_socketType;

typedef struct _fm_socket
{
    /** socket descriptor, may be UDP or TCP */
    fm_int             sock;

    /** pipe instead of socket */
    fm_int             txPipe;
    fm_int             rxPipe;

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

typedef struct _fm_modelPacketInterQueue
{
    /* egress destination sockets */
    fm_socket *            destSocket;

    /* egress destination switch */
    fm_int                 destSwitch;

    /* egress destination port */
    fm_int                 destPort;

} fm_modelPacketInterQueue;

typedef struct _fm_msg_stats
{
    fm_int  packet;
    fm_int  mgmt;
    fm_int  ctrl;
    fm_int  iosf;

} fm_msg_stats;

fm_status fmInitializeSocketInfoFile(void);
fm_status fmAddSocketInfoToFile(fm_text hostname, fm_int port,
                                     fm_bool usePipe);
fm_status fmCreateNetworkServer(fm_socket *socketInfo,
                                fm_socketType type,
                                fm_int port,
                                fm_int backlog);
fm_status fmCreateNetworkClient(fm_socket *socketInfo,
                                fm_socketType type,
                                fm_text host,
                                fm_int port);
fm_status fmWaitForNetworkEvent(fm_socket **  sockets,
                                fm_int *      numSockets,
                                fm_int        maxSockets,
                                fm_int *      eventReceived,
                                fm_timestamp *timeout);
fm_status fmCloseNetworkConnection(fm_socket *client);
fm_status fmSendNetworkData(fm_socket *socketInfo, void *data, fm_int numBytes);
fm_status fmReceiveNetworkData(fm_socket *socketInfo,
                               void *data,
                               fm_int maxBytes,
                               fm_int *numBytes);
#endif /* _MBY_SERVER_SOCKET_H_ */
