/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * Copyright (c) 2006 - 2018, Intel Corporation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *     * Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of Intel Corporation nor the names of its contributors
 *       may be used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*****************************************************************************/

#ifndef MODEL_SERVER_H
#define MODEL_SERVER_H

#include <fm_std.h>

//#define VERBOSE
//#define DEBUG_PRINT_RECEIVED_PACKET

#ifdef PLATFORM_DUKE
/* In Duke, check interrupts less frequently to improve reg access perf */
#define INTERRUPT_READ_DELAY    10
#elif defined(PLATFORM_VELOCE)
#define INTERRUPT_READ_DELAY    1
#else
#define INTERRUPT_READ_DELAY    1
#endif

#define NONPOSTED_PORT  0
#define POSTED_PORT     1

/* Offsets into IOSF message */
#define DEST_OFF        0
#define SOURCE_OFF      1
#define OPCODE_OFF      2
#define TAG_OFF         3
#define SAI_OFF         5

/* Relative to dataOff */
#define NDW_OFF         0
#define COMP_DATA_OFF   0

/*IOSF OPCODE */
#define IOSF_REG_READ               0x0
#define IOSF_REG_WRITE              0x1
#define IOSF_REG_WR_PRIV_CTRL       0x7
#define IOSF_REG_BLK_READ           0x10
#define IOSF_REG_BLK_WRITE          0x11
#define IOSF_COMP_NO_DATA           0x20
#define IOSF_COMP_DATA              0x21
#define IOSF_FUSE_REQ               0x45
#define IOSF_IP_READY               0xD0


#define MAX_READ_BASE_NDW       124
#define MAX_READ_REP_NDW        14

/*IOSF response status */
#define IOSF_RSP_SUCCESS        0
#define IOSF_RSP_FAIL           1

#define NVM_HDR_SIZE            0x1000
#define NVM_MOD_SIZE            (4096*16)
#define UNDEF_VAL               0xdeadbeef

#define PORT_LINK_UP            1

/*Only valid on LE machine */
#define GET_64(p) (*(fm_uint64*)(p))
#define SET_64(p,val) *(fm_uint64*)(p) = val

#define MODEL_FILE                          "models.packetServer"
#define TX_PIPE                             MODEL_FILE ".txpipe"
#define RX_PIPE                             MODEL_FILE ".rxpipe"
#define PIPE_IDX                            1

#define GLOBAL_INTR_MASK                    0x3FFFFFFFFFFFFFFFULL

#define FM_MAX_FDS_NUM                      1024
#define FM_FDS_POLL_TIMEOUT_USEC            1*1000

#define MAX_PHYS_PORT                       32
#define MAX_PERSISTENT_CONNECTIONS          24

#define FM_GETHOSTBYNAME_BUF_SIZE           2048

#define FM_SWITCH_NVM_IMAGE_SIZE            524288

#define FM_NVM_IMG_FILE_NAME                "nvm.img"

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

// --------------------------------------------------------------------------------
#if !defined(_NETINET_IN_H) && !defined(_BITS_SOCKADDR_H)

typedef fm_uint16 sa_family_t;
typedef fm_uint16 in_port_t;

typedef struct in_addr
{
    fm_uint32 addr;
} in_addr_t;

typedef struct sockaddr
{
    sa_family_t sa_family;  /* Common data: address family and length.  */
    char sa_data[14];       /* Address data.  */
} sockaddr_t;

/* Structure describing an Internet socket address.  */
struct sockaddr_in
{
    sa_family_t sin_family;             /* Socket address family */
    in_port_t   sin_port;       /* Port number.  */
    in_addr_t   sin_addr;       /* Internet address.  */

    /* Pad to size of `struct sockaddr', i.e. 16 bytes */
    unsigned char sin_zero[8]; // 14 - 2(port) - 4(addr) = 8 bytes
};

#endif // _NETINET_IN_H
// --------------------------------------------------------------------------------
//
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

typedef struct _fm_pkt_loopback
{
    fm_int               physPort;
    fm_int               pktLen;
    fm_byte             *pkt;
    fm_modelSidebandData sbData;

} fm_pkt_loopback;

typedef struct _fm_aqc_nvm_cpk_has_1p2 {
    fm_byte   command_flags;
    fm_byte   module_pointer;
    fm_uint16 length;
    fm_uint32 offset;
    fm_uint32 addr_high;
    fm_uint32 addr_low;
} fm_aqc_nvm_cpk_has_1p2;

typedef struct _fm_aqc_nvm {
    fm_uint16 offset_low;
    fm_byte   offset_high;
    fm_byte   command_flags;
    fm_uint16 module_pointer;
    fm_uint16 length;
    fm_uint32 addr_high;
    fm_uint32 addr_low;
} fm_aqc_nvm;

typedef struct _fm_aq_desc {
    fm_uint16 flags;
    fm_uint16 opcode;
    fm_uint16 datalen;
    fm_uint16 retval;
    fm_uint32 cookie_high;
    fm_uint32 cookie_low;
    union {
        fm_byte raw[16];
        fm_aqc_nvm_cpk_has_1p2 nvm_1p2; /* OLD Format */
    fm_aqc_nvm nvm;
    } params;
} fm_aq_desc;

/* Admin queue error codes. */
typedef enum _fm_aq_err {
    FM_AQ_RC_EINVAL        = 14, /* Invalid argument */
} fm_aq_err;
// Function prototypes:

fm_status ReadCSR(fm_int sw, fm_uint addr, fm_uint32 *val);
fm_status WriteCSR(fm_int sw, fm_uint addr, fm_uint32 val);
fm_status ReadCSR64(fm_int sw, fm_uint addr, fm_uint64 *val);
fm_status WriteCSR64(fm_int sw, fm_uint addr, fm_uint64 val);
fm_status WriteCSRForce64(fm_int sw, fm_uint addr, fm_uint64 val);

int loadImg(char *filename, fm_uint32 *alBitmask);
int loadTextCfg(char *filename);
int loadParserCfg(char *filename);

#ifdef DMPINJ_MODS
void setMsgEchoMode(fm_bool mode);
fm_bool getMsgEchoMode(void);
void setMsgDumpMode(fm_bool mode);
fm_bool getMsgDumpMode(void);
void setMsgInjectMode(fm_bool mode);
fm_bool getMsgInjectMode(void);
fm_uint32 getMsgMatchCount(void);
fm_uint32 getMsgMismatchCount(void);
#endif

void initPhysicalPorts (void);

void handleIntr(fm_int socketNum, fm_socket **sockets);

fm_status ProcessMessage(fm_socket *socket,
                         fm_modelMessage *imsg,
                         fm_int32 msgLength);

#if 0

void HexDump(fm_byte *buf, fm_int nbytes);

static fm_status InitializeSocketInfoFile(void);

static fm_status AddSocketInfoToFile(fm_text hostname,
                                     fm_int port,
                                     fm_bool usePipe);

fm_status fmCreateNetworkServer(fm_socket *socketInfo,
                                fm_socketType type,
                                fm_int port,
                                fm_int backlog);

fm_status fmCreateNetworkClient(fm_socket *socketInfo,
                                fm_socketType type,
                                fm_text host,
                                fm_int port);

fm_status fmWaitForNetworkEvent(fm_socket **sockets,
                                fm_int *numSockets,
                                fm_int maxSockets,
                                fm_int *eventReceived,
                                fm_timestamp *timeout);

fm_status fmCloseNetworkConnection(fm_socket *client);


fm_status fmSendNetworkData(fm_socket *socketInfo,
                            void *data,
                            fm_int numBytes);

fm_status fmReceiveNetworkData(fm_socket *socketInfo,
                               void *data,
                               fm_int maxBytes,
                               fm_int *numBytes);


fm_status SendMessage(fm_socket *socket,
                      fm_modelMessage *emsg,
                      fm_int32 msgLength);

fm_status ReceiveMessage(fm_socket *socket,
                         fm_bool closeSocket,
                         fm_modelMessage *imsg);

static fm_status HandleMsgLinkState(fm_int sw,
                                    fm_socket *socket,
                                    fm_modelMessage *imsg,
                                    fm_int32 msgLength);


static fm_status HandleMsgVersionInfo(fm_int sw,
                                      fm_socket *socket,
                                      fm_modelMessage *imsg,
                                      fm_int32 msgLength);

static fm_status HandleMsgCtrl(fm_int sw,
                               fm_socket *socket,
                               fm_modelMessage *imsg,
                               fm_int32 msgLength);

static fm_status HandleMsgMgmt(fm_int sw,
                               fm_socket *socket,
                               fm_modelMessage *imsg,
                               fm_int32 msgLength);

static fm_status HandleMsgIosf(fm_int sw,
                               fm_socket *socket,
                               fm_modelMessage *imsg,
                               fm_int32 imsgLength);


static void SendNvmReadErrorMsg(fm_socket *socket,
                                fm_modelMessage *emsg,
                                fm_uint16 length);

static fm_status HandleMsgNvmRead(fm_int sw,
                                  fm_socket *socket,
                                  fm_modelMessage *imsg,
                                  fm_int32 msgLength);


static void DecodeTlvPacket(fm_byte *data,
                            fm_int32 dataLength,
                            fm_byte **packet,
                            fm_int32 *pktLength,
                            fm_modelSidebandData *sbData);

static void DecodePacket(fm_modelMessage *imsg,
                         fm_int32 msgLength,
                         fm_byte **packet,
                         fm_int32 *pktLength,
                         fm_modelSidebandData *sbData);

static void FinalizePacket(fm_modelMessage *emsg,
                           fm_int32 pktLength,
                           fm_modelSidebandData *sbData,
                           fm_int32 *msgLength);

static fm_status HandleMsgPacket(fm_int sw,
                                 fm_modelMessage *imsg,
                                 fm_int physPort,
                                 fm_int32 imsgLength);

static fm_status HandleMsgSetEgressInfo(fm_int sw,
                                        fm_modelMessage *imsg,
                                        fm_int type);

fm_status SendInterruptEvent(fm_socket *socket,
                             fm_int64 intr);

fm_status ParseUint64(char *string,
                      fm_uint64 *result);

fm_status ParseInt(char *string,
                   fm_int *result);

static int processChunk(unsigned int modNum,
                        unsigned int chnkNum,
                        uint16_t *chunk,
                        unsigned int chunkLen);

static int processModule(unsigned int modNum,
                         uint16_t *module,
                         unsigned int len);

void logPrintHandler(fm_uint64 level,
                     char *log);

static void runRegBenchmark();

static void print_usage(char *cmd);

int main(int argc, char *argv[]);

#endif // if 0

#endif // MODEL_SERVER_H
