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

#ifdef PLATFORM_VELOCE
#include "Python.h"
#ifndef COM
#define COM "/dev/ttyUSB0"
#endif
#ifndef BAUDRATE
#define BAUDRATE 9600
#endif
#endif

//#define VERBOSE
//#define DEBUG_PRINT_RECEIVED_PACKET

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <poll.h>
#include <sys/select.h>
#include <netinet/tcp.h>
#include <sys/stat.h>
#include <linux/stat.h>
#include <fcntl.h>
#include <time.h>
#include <fm_sdk.h>

#include <fm_sdk_hlp_int.h>
#include "../src/platforms/common/model/hlp/hlp_model_reg_ctrl.c"
#include <platforms/whiteModelLib/platform_types.h>
#include "api/internal/hlp/hlp_api_regs_int.h"
#include "duke_iosf.h"

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
        fm_aqc_nvm_cpk_has_1p2 nvm_1p2;	/* OLD Format */
	fm_aqc_nvm nvm;
    } params;
} fm_aq_desc;

/* Admin queue error codes. */
typedef enum _fm_aq_err {
    FM_AQ_RC_EINVAL        = 14, /* Invalid argument */
} fm_aq_err;

FILE *outFp = NULL;

static const int SOCKET_ADDRLEN = sizeof(struct sockaddr_in);
static int debug = 1;
static fm_socket pktRecvSockets[MAX_PHYS_PORT];
static fm_msg_stats msg_stat;
static fm_int portLinkState[MAX_PHYS_PORT];
static fm_text nvmImgFile = FM_NVM_IMG_FILE_NAME;

/* Could make these configurable */
static fm_byte strap_iosfsb_endpt_id  = 0xef;
static fm_byte strap_interrupt_destid = 0xfe;
static fm_uint16 strap_endpt_sai      = 0xab;

extern fm_uint64 log_cat_mask;
extern char *log_cat_names[];

fm_status SendInterruptEvent(fm_socket *socket, fm_int64 intr);

#ifdef PLATFORM_DUKE

fm_status ReadCSR(fm_int sw, fm_uint addr, fm_uint32 *val)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(addr);
    FM_NOT_USED(val);

    printf("ERROR: ReadCSR no support\n");
    return FM_FAIL;
}

fm_status WriteCSR(fm_int sw, fm_uint addr, fm_uint32 val)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(addr);
    FM_NOT_USED(val);

    printf("ERROR: WriteCSR no support\n");
    return FM_FAIL;
}

fm_status ReadCSR64(fm_int sw, fm_uint addr, fm_uint64 *val)
{
    FM_NOT_USED(sw);

    *val = dukeHlpRead(addr);
    return FM_OK;
}

fm_status WriteCSR64(fm_int sw, fm_uint addr, fm_uint64 val)
{
    FM_NOT_USED(sw);

    dukeHlpWrite(addr, val);
    return FM_OK;
}

fm_status WriteCSRForce64(fm_int sw, fm_uint addr, fm_uint64 val)
{
    FM_NOT_USED(sw);

    dukeHlpWrite(addr, val);
    return FM_OK;
}

void enableBSMAccess()
{
    dukeHlpWrite(0x180, 0x2);
    dukeHlpWrite(0x188, 0x2);
    dukeHlpWrite(0x190, 0x2);
    dukeHlpWrite(0x198, 0x2);
}

/* Use customized default values for Duke take from:
 * nd_switching-regression/sv/tests/HLP/Shared/Veloce_1p1/sia.cfg */
void DukeCustomization()
{
    dukeHlpWrite(HLP_SIA_RX_PORT_CFG(0, 0, 0), 0x000000000000017d);
    dukeHlpWrite(HLP_SIA_RX_PORT_CFG(0, 1, 0), 0x0000000000000180);
    dukeHlpWrite(HLP_SIA_RX_PORT_CFG(0, 2, 0), 0x000000000000017d);
    dukeHlpWrite(HLP_SIA_RX_PORT_CFG(0, 3, 0), 0x000000000000017d);
    dukeHlpWrite(HLP_SIA_RX_PORT_CFG(1, 0, 0), 0x000000000000017d);
    dukeHlpWrite(HLP_SIA_RX_PORT_CFG(1, 1, 0), 0x0000000000000180);
    dukeHlpWrite(HLP_SIA_RX_PORT_CFG(1, 2, 0), 0x000000000000017d);
    dukeHlpWrite(HLP_SIA_RX_PORT_CFG(1, 3, 0), 0x000000000000017d);

    dukeHlpWrite(HLP_SIA_RX_IARB_TOKEN_RATE(0, 0), 0x0000000000000001);
    dukeHlpWrite(HLP_SIA_RX_IARB_TOKEN_RATE(1, 0), 0x0000000000000001);
    dukeHlpWrite(HLP_SIA_RX_IARB_TOKEN_RATE(2, 0), 0x0000000000000001);
    dukeHlpWrite(HLP_SIA_RX_IARB_TOKEN_RATE(3, 0), 0x0000000000000001);
    dukeHlpWrite(HLP_SIA_RX_IARB_TOKEN_RATE(4, 0), 0x0000000000000001);
    dukeHlpWrite(HLP_SIA_RX_IARB_TOKEN_RATE(5, 0), 0x0000000000000001);
    dukeHlpWrite(HLP_SIA_RX_IARB_TOKEN_RATE(6, 0), 0x0000000000000001);
    dukeHlpWrite(HLP_SIA_RX_IARB_TOKEN_RATE(7, 0), 0x0000000000000001);

    dukeHlpWrite(HLP_SIA_TX_PORT_MEM_BOUND_PTR(0, 0, 0), 0x00000000000f0000);
    dukeHlpWrite(HLP_SIA_TX_PORT_MEM_BOUND_PTR(0, 1, 0), 0x00000000001f0010);
    dukeHlpWrite(HLP_SIA_TX_PORT_MEM_BOUND_PTR(0, 2, 0), 0x00000000002f0020);
    dukeHlpWrite(HLP_SIA_TX_PORT_MEM_BOUND_PTR(0, 3, 0), 0x00000000003f0030);
    dukeHlpWrite(HLP_SIA_TX_PORT_MEM_BOUND_PTR(1, 0, 0), 0x00000000000f0000);
    dukeHlpWrite(HLP_SIA_TX_PORT_MEM_BOUND_PTR(1, 1, 0), 0x00000000001f0010);
    dukeHlpWrite(HLP_SIA_TX_PORT_MEM_BOUND_PTR(1, 2, 0), 0x00000000002f0020);
    dukeHlpWrite(HLP_SIA_TX_PORT_MEM_BOUND_PTR(1, 3, 0), 0x00000000003f0030);

    dukeHlpWrite(HLP_SIA_TX_PORT_JITTER(0, 0, 0), 0x20801e3);
    dukeHlpWrite(HLP_SIA_TX_PORT_JITTER(0, 1, 0), 0x20801e3);
    dukeHlpWrite(HLP_SIA_TX_PORT_JITTER(0, 2, 0), 0x20801e3);
    dukeHlpWrite(HLP_SIA_TX_PORT_JITTER(0, 3, 0), 0x20801e3);
    dukeHlpWrite(HLP_SIA_TX_PORT_JITTER(1, 0, 0), 0x20801e3);
    dukeHlpWrite(HLP_SIA_TX_PORT_JITTER(1, 1, 0), 0x20801e3);
    dukeHlpWrite(HLP_SIA_TX_PORT_JITTER(1, 2, 0), 0x20801e3);
    dukeHlpWrite(HLP_SIA_TX_PORT_JITTER(1, 3, 0), 0x20801e3);

    dukeHlpWrite(HLP_SIA_TX_PORT_CTL(0, 0, 0), 0x00000000a5010108);
    dukeHlpWrite(HLP_SIA_TX_PORT_CTL(0, 1, 0), 0x00000000a5010108);
    dukeHlpWrite(HLP_SIA_TX_PORT_CTL(0, 2, 0), 0x00000000a5010108);
    dukeHlpWrite(HLP_SIA_TX_PORT_CTL(0, 3, 0), 0x00000000a5010108);
    dukeHlpWrite(HLP_SIA_TX_PORT_CTL(1, 0, 0), 0x00000000a5010108);
    dukeHlpWrite(HLP_SIA_TX_PORT_CTL(1, 1, 0), 0x00000000a5010108);
    dukeHlpWrite(HLP_SIA_TX_PORT_CTL(1, 2, 0), 0x00000000a5010108);
    dukeHlpWrite(HLP_SIA_TX_PORT_CTL(1, 3, 0), 0x00000000a5010108);

    dukeHlpWrite(HLP_SIA_TX_CONTROL(0, 0), 0x0000000000000172);
    dukeHlpWrite(HLP_SIA_TX_CONTROL(1, 0), 0x0000000000000172);

    dukeHlpWrite(HLP_MAC_PIA_CFG(0, 0), 0x0000000000000005);
    dukeHlpWrite(HLP_MAC_PIA_CFG(1, 0), 0x0000000000000005);
    dukeHlpWrite(HLP_MAC_PIA_CFG(2, 0), 0x0000000000000005);
    dukeHlpWrite(HLP_MAC_PIA_CFG(3, 0), 0x0000000000000005);
    dukeHlpWrite(HLP_MAC_PIA_CFG(4, 0), 0x0000000000000005);
    dukeHlpWrite(HLP_MAC_PIA_CFG(5, 0), 0x0000000000000005);
    dukeHlpWrite(HLP_MAC_PIA_CFG(6, 0), 0x0000000000000005);
    dukeHlpWrite(HLP_MAC_PIA_CFG(7, 0), 0x0000000000000005);

    dukeHlpWrite(HLP_IARB_ADAPTIVE_SCHEDULE(0, 0, 0), 0x0000000000000001);
    dukeHlpWrite(HLP_EARB_ADAPTIVE_SCHEDULE(0, 0, 0), 0x0000000000000001);
    dukeHlpWrite(HLP_IARB_ADAPTIVE_SCHEDULE(0, 1, 0), 0x0000000000010010);
    dukeHlpWrite(HLP_EARB_ADAPTIVE_SCHEDULE(0, 1, 0), 0x0000000000010010);
    dukeHlpWrite(HLP_IARB_ADAPTIVE_SCHEDULE(0, 4, 0), 0x0000000000020100);
    dukeHlpWrite(HLP_EARB_ADAPTIVE_SCHEDULE(0, 4, 0), 0x0000000000020100);
    dukeHlpWrite(HLP_IARB_ADAPTIVE_SCHEDULE(0, 5, 0), 0x0000000000031000);
    dukeHlpWrite(HLP_EARB_ADAPTIVE_SCHEDULE(0, 5, 0), 0x0000000000031000);

    dukeHlpWrite(HLP_IARB_ADAPTIVE_SCHEDULE(0, 2, 0), 0x00000000000f0000);
    dukeHlpWrite(HLP_EARB_ADAPTIVE_SCHEDULE(0, 2, 0), 0x00000000000f0000);
    dukeHlpWrite(HLP_IARB_ADAPTIVE_SCHEDULE(0, 3, 0), 0x00000000000f0000);
    dukeHlpWrite(HLP_EARB_ADAPTIVE_SCHEDULE(0, 3, 0), 0x00000000000f0000);
    dukeHlpWrite(HLP_IARB_ADAPTIVE_SCHEDULE(0, 6, 0), 0x00000000000f0000);
    dukeHlpWrite(HLP_EARB_ADAPTIVE_SCHEDULE(0, 6, 0), 0x00000000000f0000);
    dukeHlpWrite(HLP_IARB_ADAPTIVE_SCHEDULE(0, 7, 0), 0x00000000000f0000);
    dukeHlpWrite(HLP_EARB_ADAPTIVE_SCHEDULE(0, 7, 0), 0x00000000000f0000);

    dukeHlpWrite(HLP_EARB_CTRL(0), 0x00000000002600f3);
    dukeHlpWrite(HLP_IARB_CTRL(0), 0x00000000000019fb);
}

#elif defined PLATFORM_VELOCE

static int declarePyObj = 0;
PyObject *pVeloce;
void setPyObj()
{
    if (declarePyObj == 0)
    {

        PyObject *pName, *pModule, *pDict, *pClass, *pArgsTuple, *pCom, *pBaudrate;
        Py_Initialize();
        pName = PyString_FromString("veloce_register_access_py");
        pModule = PyImport_Import(pName);
        pDict = PyModule_GetDict(pModule);
        pClass = PyDict_GetItemString(pDict, "VeloceRegisterAccess");
        pCom = PyString_FromString(COM);
        pBaudrate = PyInt_FromLong(BAUDRATE);
        pArgsTuple = PyTuple_New(2);
        PyTuple_SetItem(pArgsTuple, 0, pCom);
        PyTuple_SetItem(pArgsTuple, 1, pBaudrate);
        pVeloce = PyObject_CallObject(pClass, pArgsTuple);
        if (pVeloce == NULL)
            printf("FAILED to create a python object for Veloce\n");
        declarePyObj = 1;
    }
}
long long int readVeloceReg(int addr)
{
    if (debug > 4)
        printf("readVeloceReg addr = %02x\n", addr);
    PyObject *pFunc, *pClass, *lsb, *msb;

    setPyObj();

    pFunc = PyObject_CallMethod(pVeloce, "Write_Reg", "II", 0x80008000, 0xc500aa1f);
    pFunc = PyObject_CallMethod(pVeloce, "Write_Reg", "II", 0x80008001, 0x00000300);
    pFunc = PyObject_CallMethod(pVeloce, "Write_Reg", "II", 0x80008002, ((addr & 0xffff) << 16 ) | 0x00ff);
    pFunc = PyObject_CallMethod(pVeloce, "Write_Reg", "II", 0x80008003, addr >> 16);
    pFunc = PyObject_CallMethod(pVeloce, "Write_Reg", "II", 0x8000800f, 0x00000001);
    if (pFunc == NULL)
                 printf("FAILED to PyObject_Call in readVeloceReg\n");

    lsb = PyObject_CallMethod(pVeloce, "Read_Reg", "I", 0x80008008);
    msb = PyObject_CallMethod(pVeloce, "Read_Reg", "I", 0x80008009);

    if (debug > 4)
        printf ("upper32: 0x%lx   lower32: 0x%lx\n", PyInt_AsLong(msb),
                PyInt_AsLong(lsb));
    long long int ret = (PyInt_AsLong(msb) << 32 ) | PyInt_AsLong(lsb);

    return ret;

}

long long int writeVeloceReg(int addr, unsigned long long int val)
{
    if (debug > 4)
        printf("writeVeloceReg addr = %02x, val = %02llu\n", addr, val);

    PyObject *pFunc, *pClass, *pValue, *presult;

    setPyObj();
    pFunc = PyObject_CallMethod(pVeloce, "Write_Reg", "II", 0x80008000, 0xc501aa1f);
    pFunc = PyObject_CallMethod(pVeloce, "Write_Reg", "II", 0x80008001, 0x00000300);
    pFunc = PyObject_CallMethod(pVeloce, "Write_Reg", "II", 0x80008002, ((addr & 0xffff) << 16 ) | 0x00ff);
    pFunc = PyObject_CallMethod(pVeloce, "Write_Reg", "II", 0x80008003, addr >> 16);
    pFunc = PyObject_CallMethod(pVeloce, "Write_Reg", "II", 0x80008004, val & 0xffffffff);
    pFunc = PyObject_CallMethod(pVeloce, "Write_Reg", "II", 0x80008005, val >> 32);
    pFunc = PyObject_CallMethod(pVeloce, "Write_Reg", "II", 0x8000800f, 0x00000001);
    if (pFunc == NULL)
                 printf("FAILED to PyObject_Call in readVeloceReg\n");

    return 0;
}


fm_status ReadCSR(fm_int sw, fm_uint addr, fm_uint32 *val)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(addr);
    FM_NOT_USED(val);

    printf("ERROR: ReadCSR no support\n");
    return FM_FAIL;
}

fm_status WriteCSR(fm_int sw, fm_uint addr, fm_uint32 val)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(addr);
    FM_NOT_USED(val);

    printf("ERROR: WriteCSR no support\n");
    return FM_FAIL;
}

fm_status ReadCSR64(fm_int sw, fm_uint addr, fm_uint64 *val)
{
    FM_NOT_USED(sw);

    *val = readVeloceReg(addr);
    return FM_OK;
}

fm_status WriteCSR64(fm_int sw, fm_uint addr, fm_uint64 val)
{
    FM_NOT_USED(sw);

    writeVeloceReg(addr, val);
    return FM_OK;
}

fm_status WriteCSRForce64(fm_int sw, fm_uint addr, fm_uint64 val)
{
    FM_NOT_USED(sw);

    writeVeloceReg(addr, val);
    return FM_OK;
}

#else
fm_status ReadCSR(fm_int sw, fm_uint addr, fm_uint32 *val)
{
    return fmModelReadCSR(sw, addr, val);
}

fm_status WriteCSR(fm_int sw, fm_uint addr, fm_uint32 val)
{
    return fmModelWriteCSR(sw, addr, val);
}

fm_status ReadCSR64(fm_int sw, fm_uint addr, fm_uint64 *val)
{
    return fmModelReadCSR64(sw, addr, val);
}

fm_status WriteCSR64(fm_int sw, fm_uint addr, fm_uint64 val)
{
    return fmModelWriteCSR64(sw, addr, val);
}

fm_status WriteCSRForce64(fm_int sw, fm_uint addr, fm_uint64 val)
{
    return hlpModelWriteCSRForce64(sw, addr, val);
}

#endif

void handleIntr(fm_int socketNum, fm_socket **  sockets)
{
    fm_uint64 val64;
    fm_int i;

    ReadCSR64(0, HLP_GLOBAL_INTERRUPT(0), &val64);

    if ((val64 & GLOBAL_INTR_MASK) &&
#ifdef PLATFORM_DUKE
        FM_GET_BIT64(val64, HLP_GLOBAL_INTERRUPT, COMPLETION_PENDING) &&
        FM_GET_BIT64(val64, HLP_GLOBAL_INTERRUPT, INTERRUPT_PENDING))
#else
        !FM_GET_BIT64(val64, HLP_GLOBAL_INTERRUPT, COMPLETION_PENDING) &&
        !FM_GET_BIT64(val64, HLP_GLOBAL_INTERRUPT, INTERRUPT_PENDING))
#endif
    {
        printf("Interrupt 0x%llx is pending\n", val64);
        for ( i = 1 ; i < socketNum ; i++ )
        {
            SendInterruptEvent(sockets[i], val64);
        }
        /* There is no response back from Admin Interrupt Queue,
	 * hence clear COMPLENTION_PENDING bit. */
#ifdef PLAFORM_DUKE
        FM_SET_BIT64(val64, HLP_GLOBAL_INTERRUPT, COMPLETION_PENDING, 1);
#else
        FM_SET_BIT64(val64, HLP_GLOBAL_INTERRUPT, COMPLETION_PENDING, 0);
        FM_SET_BIT64(val64, HLP_GLOBAL_INTERRUPT, INTERRUPT_PENDING, 1);
#endif
        WriteCSRForce64(0, HLP_GLOBAL_INTERRUPT(0), val64);
    }
}

void HexDump(fm_byte *buf, fm_int nbytes)
{
    fm_int linebytes;
    fm_int  j;
    fm_int  cnt;

    cnt = 0;
    do
    {
        linebytes = (nbytes > 16) ? 16 : nbytes;

        FM_LOG_PRINT("%02x:", cnt);

        for (j = 0 ; j < linebytes ; j++)
        {
            FM_LOG_PRINT(" %02x", buf[cnt + j]);
        }

        FM_LOG_PRINT("    ");

        for (j = 0 ; j < linebytes ; j++)
        {
            if ( (buf[cnt + j] < 0x20) || (buf[cnt + j] > 0x7e) )
            {
                FM_LOG_PRINT(".");
            }
            else
            {
                FM_LOG_PRINT("%c", buf[cnt + j]);
            }
        }

        FM_LOG_PRINT("\n");

        cnt += linebytes;
        nbytes -= linebytes;

    }
    while (nbytes > 0);

} /* end HexDump */


/*****************************************************************************/
/** InitializeSocketInfoFile
 * \ingroup intModel
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
static fm_status AddSocketInfoToFile(fm_text hostname, fm_int port,
                                     fm_bool usePipe)
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

    snprintf(filename, sizeof(filename), "%s/%s", filePath, MODEL_FILE);

    modelPos = fmGetIntApiAttribute(FM_AAK_API_PLATFORM_MODEL_POSITION,
                                    FM_AAD_API_PLATFORM_MODEL_POSITION);

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
    errno_t            strErrNum;

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
    errno_t       strErrNum;
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

                //FM_LOG_DEBUG2(FM_LOG_CAT_PLATFORM,
                printf("New client (fd=%d) connection on slot %d\n",
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

#ifdef VERBOSE
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
        if (socketInfo->txPipe <= 0)
        {
            FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                         "Opening TX pipe\n");
            socketInfo->txPipe = open(TX_PIPE, O_WRONLY | O_NONBLOCK);
            if (socketInfo->txPipe <= 0)
            {
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                    "Unable to open TX pipe\n");
            }
        }

        errResult = write(socketInfo->txPipe, data, numBytes);
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                     "Write %d bytes to TX pipe. %d written\n",
                     numBytes, errResult);
        FM_LOG_SYS_EXIT_ON_COND(FM_LOG_CAT_PLATFORM, errResult != numBytes);
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


static fm_status ReceiveMessage(fm_socket *      socket,
                                fm_bool          closeSocket,
                                fm_modelMessage *imsg)
{
    fm_status status;
    fm_int32  msgLength;
    fm_int    length;
    fm_int    bufferSize;
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
        printf("Connection (fd=%d) is closed\n", socket->sock);
        socket->type = FM_SOCKET_TYPE_CLOSED;
    }
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    if ( recvLength != ( (fm_int) FM_MODEL_MSG_LENGTH_SIZE ) )
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Message corruption detected: Expected %d bytes, received "
                     "%d bytes. fd %d, type %d\n",
                     (fm_int) FM_MODEL_MSG_LENGTH_SIZE,
                     recvLength, socket->sock, socket->type);

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
                     "Message corruption detected: Expected %d bytes, received %d bytes\n",
                     (fm_int) FM_MODEL_MSG_VERSION_SIZE,
                     recvLength);

        status = FM_FAIL;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    version = ntohs(imsg->version);
    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                 "Received message version of %d vs %d\n",
                 version, FM_MODEL_MSG_VERSION);

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
    bufferSize = FM_MODEL_MSG_SIZE -
                 (FM_MODEL_MSG_LENGTH_SIZE + FM_MODEL_MSG_VERSION_SIZE);
    if (length > bufferSize) {
        FM_LOG_WARNING(FM_LOG_CAT_PLATFORM,
                "Message length (%d) > buffer size (%d). Data will be lost to avoid overflow\n",
                length, bufferSize);
        length = bufferSize;
    }

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


/*****************************************************************************/
/** HandleMsgLinkState
 * \ingroup intModel
 *
 * \desc            Processes link state info.
 *                                                                      \lb\lb
 *                  The control request message format is:
 *                                                                      \lb\lb
 *                  sw field in header is the version number.
 *                  +--------------+-----------+--------------+------------+
 *                  | header (12B) | state (1B)
 *                  +--------------+-----------+--------------+------------+
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[in]       socket points to the socket where the message is originated.
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
static fm_status HandleMsgLinkState(fm_int               sw,
                                    fm_socket           *socket,
                                    fm_modelMessage *    imsg,
                                    fm_int32             msgLength)
{
    fm_status        status = FM_OK;
    fm_modelMessage  emsg;
    fm_int           linkState;
    fm_int           port;

    FM_NOT_USED(sw);
    FM_NOT_USED(socket);
    FM_NOT_USED(msgLength);

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d imsg=%p msgLength=%d\n",
                 sw,
                 (void *) imsg,
                 msgLength);

    port = ntohs(imsg->port);
    linkState = imsg->data[0];

    if (linkState != portLinkState[port])
	printf("PhysPort %d linkState %d oldState %d\n", port, linkState,
	       portLinkState[port]);

    if (port < MAX_PHYS_PORT)
        portLinkState[port] = linkState;

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

} /* end HandleMsgLinkState */

/*****************************************************************************/
/** HandleMsgVersionInfo
 * \ingroup intModel
 *
 * \desc            Processes version info.
 *                                                                      \lb\lb
 *                  The control request message format is:
 *                                                                      \lb\lb
 *                  sw field in header is the version number.
 *                  +--------------+-----------+--------------+------------+
 *                  | header (12B) | version tag string
 *                  +--------------+-----------+--------------+------------+
 *                  The version response has the same format.
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[in]       socket points to the socket where the message is originated.
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
static fm_status HandleMsgVersionInfo(fm_int               sw,
                                      fm_socket           *socket,
                                      fm_modelMessage *    imsg,
                                      fm_int32             msgLength)
{
    fm_status        status = FM_OK;
    fm_modelMessage  emsg;
    fm_int           versionNum;

    FM_NOT_USED(sw);

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d imsg=%p msgLength=%d\n",
                 sw,
                 (void *) imsg,
                 msgLength);

    versionNum = imsg->data[0] << 8 | imsg->data[1];
    if (versionNum != HLP_REG_VERSION)
    {
        printf("****** WARNING ******\n");
        printf("Version number mismatch between API and Server\n");
    }

    printf("API version 0x%04x. Server version 0x%04x\n",
        versionNum, HLP_REG_VERSION);
    printf("API VersionTag: %s\n", imsg->data + 2);
    printf("Server VersionTag: %s\n", fmModelGetVersionTag());
    printf("Connection Socket %d\n", socket->sock);

    FM_CLEAR(emsg);
    FM_MEMCPY_S(&emsg,
                FM_MODEL_MSG_SIZE,
                imsg,
                FM_MODEL_MSG_HEADER_SIZE);

    emsg.data[0] = (HLP_REG_VERSION >> 8) & 0xFF;
    emsg.data[1] = HLP_REG_VERSION & 0xFF;
    strcpy((char*)emsg.data + 2, fmModelGetVersionTag());
    msgLength = FM_MODEL_MSG_HEADER_SIZE + strlen((char*)(emsg.data + 2)) + 3;
    emsg.msgLength = htonl(msgLength);

    if (debug >= 2)
    {
        printf("Version info sent back:\n");
        HexDump((unsigned char *)&emsg, ntohl(emsg.msgLength));
    }
    SendMessage(socket, &emsg, ntohl(emsg.msgLength));

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

} /* end HandleMsgVersionInfo */



/*****************************************************************************/
/** HandleMsgCtrl
 * \ingroup intModel
 *
 * \desc            Processes control messages and creates and enqueues
 *                  response messages as necessary.
 *                                                                      \lb\lb
 *                  The control request message format is:
 *                                                                      \lb\lb
 *                  +--------------+-----------+--------------+------------+
 *                  | header (12B) | type (1B) | type specific ...
 *                  +--------------+-----------+--------------+------------+
 *                  The control response message format is:
 *                                                                      \lb\lb
 *                  +--------------+-----------+--------------+------------+
 *                  | header (12B) | type (1B) | 8-bit status | Error string..
 *                  +--------------+-----------+--------------+------------+
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[in]       socket points to the socket where the message is originated.
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
static fm_status HandleMsgCtrl(fm_int               sw,
                               fm_socket           *socket,
                               fm_modelMessage *    imsg,
                               fm_int32             msgLength)
{
    fm_status        status = FM_ERR_INVALID_ARGUMENT;
    fm_uint32        addr;
    fm_uint32        value;
    fm_uint64        val64;
    fm_int32         reqLength;
    fm_byte          type;
    fm_modelMessage  emsg;

    FM_NOT_USED(sw);

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d imsg=%p msgLength=%d\n",
                 sw,
                 (void *) imsg,
                 msgLength);

    type  = imsg->data[0];
    FM_CLEAR(emsg);

    switch (type)
    {
        case FM_MODEL_CTRL_CHIP_RESET_REQ:
            reqLength = FM_MODEL_MSG_HEADER_SIZE + 1;
            break;
        default:
            /* The supplied management type is unrecognized. */
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                "Unexpected mgmt message type %d\n", type);
            status = FM_ERR_INVALID_ARGUMENT;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }
    if (msgLength != reqLength)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
            "Unexpected length %d instead of %d\n", msgLength, reqLength);
        /* This management message is too short or too long. */
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    switch (type)
    {
        case FM_MODEL_CTRL_CHIP_RESET_REQ:
            status = fmModelReset(0);
            emsg.data[0] = FM_MODEL_CTRL_CHIP_RESET_REP;
            break;
        default:
            break;
    }



ABORT:
    FM_MEMCPY_S(&emsg,
                FM_MODEL_MSG_SIZE,
                imsg,
                FM_MODEL_MSG_HEADER_SIZE);

    if (status)
    {
        emsg.data[1] = 1; /* Error status */
        FM_SNPRINTF_S((fm_text)emsg.data + 2, sizeof(emsg.data - 2),
                    "%s: HandleMsgMgmt",
                    fmErrorMsg(status));
        msgLength = FM_MODEL_MSG_HEADER_SIZE + 2 +
                    strlen((fm_text)emsg.data + 2) + 1;
        emsg.msgLength = htonl(msgLength);
    }
    else
    {
        emsg.data[1] = 0; /* SUCCESS status */
        msgLength = FM_MODEL_MSG_HEADER_SIZE + 2;
        emsg.msgLength = htonl(msgLength);

        if (debug >= 2)
        {
            printf("Control data sent back:\n");
            HexDump(emsg.data, ntohl(emsg.msgLength));
        }
        SendMessage(socket, &emsg, ntohl(emsg.msgLength));
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

} /* end HandleMsgCtrl */




/*****************************************************************************/
/** HandleMsgMgmt
 * \ingroup intModel
 *
 * \desc            Processes management messages and creates and enqueues
 *                  response messages as necessary.
 *                                                                      \lb\lb
 *                  The management message format is:
 *                                                                      \lb\lb
 *                  +--------------+-----------+--------------+------------+
 *                  | header (12B) | type (1B) | address (4B) | value (4/8B) |
 *                  +--------------+-----------+--------------+------------+
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[in]       socket points to the socket where the message is originated.
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
                               fm_socket           *socket,
                               fm_modelMessage *    imsg,
                               fm_int32             msgLength)
{
    fm_status        status = FM_ERR_INVALID_ARGUMENT;
    fm_uint32        addr;
    fm_uint32        value;
    fm_uint64        val64;
    fm_int32         reqLength;
    fm_byte          type;
    errno_t          rv;
    fm_modelMessage  emsg;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d imsg=%p msgLength=%d\n",
                 sw,
                 (void *) imsg,
                 msgLength);

    type  = imsg->data[0];
    FM_CLEAR(emsg);

    switch (type)
    {
        case FM_MODEL_MGMT_READ_REQUEST:
        case FM_MODEL_MGMT_WRITE:
            reqLength = FM_MODEL_MSG_HEADER_SIZE + 9;
            break;
        case FM_MODEL_MGMT_READ64_REQUEST:
        case FM_MODEL_MGMT_WRITE64:
            reqLength = FM_MODEL_MSG_HEADER_SIZE + 13;
            break;
        default:
            /* The supplied management type is unrecognized. */
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                "Unexpected mgmt message type %d\n", type);
            status = FM_ERR_INVALID_ARGUMENT;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    if (msgLength != reqLength)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
            "Unexpected length %d instead of %d\n", msgLength, reqLength);
        /* This management message is too short or too long. */
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }


    addr  = ntohl( *( (fm_uint32 *) &imsg->data[1] ) );

    switch (type)
    {
        case FM_MODEL_MGMT_READ_REQUEST:
            status = ReadCSR(sw, addr, &value);

            if (status == FM_OK)
            {
                rv = FM_MEMCPY_S(&emsg,
                                 FM_MODEL_MSG_SIZE,
                                 imsg,
                                 FM_MODEL_MSG_HEADER_SIZE);
                if (rv != 0)
                {
                    status = FM_FAIL;
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
                }

                emsg.data[0] = FM_MODEL_MGMT_READ_RESPONSE;
                *( (fm_uint32 *) &(emsg.data[1]) ) = htonl(addr);
                *( (fm_uint32 *) &(emsg.data[5]) ) = htonl(value);
                emsg.msgLength = htonl(reqLength);
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
            value = ntohl( *( (fm_uint32 *) &imsg->data[5] ) );
            status = WriteCSR(sw, addr, value);

            if (status == FM_OK)
            {
                rv = FM_MEMCPY_S(&emsg, FM_MODEL_MSG_SIZE, imsg,
                                 FM_MODEL_MSG_HEADER_SIZE);
                if (rv != 0)
                {
                    status = FM_FAIL;
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
                }

                emsg.data[0] = FM_MODEL_MGMT_WRITE_ACK;
                *( (fm_uint32 *) &(emsg.data[1]) ) = htonl(addr);
                *( (fm_uint32 *) &(emsg.data[5]) ) = htonl(value);
                emsg.msgLength = htonl(reqLength);
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
        case FM_MODEL_MGMT_READ64_REQUEST:
            status = ReadCSR64(sw, addr, &val64);

            if (status == FM_OK)
            {
                rv = FM_MEMCPY_S(&emsg,
                                 FM_MODEL_MSG_SIZE,
                                 imsg,
                                 FM_MODEL_MSG_HEADER_SIZE);
                if (rv != 0)
                {
                    status = FM_FAIL;
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
                }

                emsg.data[0] = FM_MODEL_MGMT_READ64_RESPONSE;
                *( (fm_uint32 *) &(emsg.data[1]) ) = htonl(addr);
                *( (fm_uint32 *) &(emsg.data[5]) ) = htonl(val64>>32);
                *( (fm_uint32 *) &(emsg.data[9]) ) = htonl(val64);
                emsg.msgLength = htonl(reqLength);
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

        case FM_MODEL_MGMT_WRITE64:
            val64 = ntohl( *( (fm_uint32 *) &imsg->data[5] ) );
            val64 <<= 32;
            val64 |= ntohl( *( (fm_uint32 *) &imsg->data[9] ) );
            status = WriteCSR64(sw, addr, val64);

            if (status == FM_OK)
            {
                rv = FM_MEMCPY_S(&emsg, FM_MODEL_MSG_SIZE, imsg,
                                 FM_MODEL_MSG_HEADER_SIZE);
                if (rv != 0)
                {
                    status = FM_FAIL;
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
                }

                emsg.data[0] = FM_MODEL_MGMT_WRITE64_ACK;
                *( (fm_uint32 *) &(emsg.data[1]) ) = htonl(addr);
                *( (fm_uint32 *) &(emsg.data[5]) ) = htonl(val64>>32);
                *( (fm_uint32 *) &(emsg.data[9]) ) = htonl(val64);
                emsg.msgLength = htonl(reqLength);
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
    if (status)
    {
        emsg.type = htons(FM_MODEL_MSG_ERROR);
        FM_SNPRINTF_S((fm_text)emsg.data, sizeof(emsg.data),
                    "%s: HandleMsgMgmt",
                    fmErrorMsg(status));
        msgLength = strlen((fm_text)emsg.data) + FM_MODEL_MSG_HEADER_SIZE + 1;
        emsg.msgLength = htonl(msgLength);
        SendMessage(socket, &emsg, msgLength);
    }
    else
    {
        if (debug >= 2)
        {
            printf("Mgmt data sent back:\n");
            HexDump(emsg.data, ntohl(emsg.msgLength));
        }
        SendMessage(socket, &emsg, ntohl(emsg.msgLength));
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end HandleMsgMgmt */



/*****************************************************************************/
/** HandleMsgIosf
 * \ingroup intModel
 *
 * \desc            Processes management messages and creates and enqueues
 *                  response messages as necessary.
 *                                                                      \lb\lb
 *                  The management message format is:
 *                                                                      \lb\lb
 *                  +--------------+-----------+--------------+------------+
 *                  | header (12B) | IOSF Message
 *                  +--------------+-----------+--------------+------------+
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[in]       socket points to the socket where the message is originated.
 *
 * \param[in]       imsg points to the received message that is to be processed.
 *
 * \param[in]       imsgLength is the length of the received message in units of
 *                  bytes.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if the received message is not a
 *                  valid management message.
 *
 *****************************************************************************/
#ifndef PLATFORM_DUKE
static fm_status HandleMsgIosf(fm_int               sw,
                               fm_socket           *socket,
                               fm_modelMessage *    imsg,
                               fm_int32             imsgLength)
{
    fm_status        status = FM_OK;
    fm_uint32        value;
    fm_int32         reqLength;
    fm_byte          dest;
    fm_byte          source;
    fm_byte          opcode;
    fm_byte          respOpcode;
    fm_byte          tag;
    fm_byte          rsp;
    fm_byte          bar;
    fm_byte          fbe;
    fm_byte          exphdr;
    fm_byte          sbe;
    fm_uint16        sai;
    fm_uint32        addr;
    fm_uint64        val64;
    fm_modelMessage  emsg;
    fm_int           i;
    fm_uint          ndw;
    fm_uint          cnt;
    fm_uint          eDataLen;
    fm_uint          iDataLen;
    fm_uint          dataOff;
    fm_uint          ndwOff;
    fm_uint          valOff;
    fm_uint          msgLength;
    fm_bool          saiHdr;
    fm_uint16        port;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d imsg=%p imsgLength=%d\n",
                 sw,
                 (void *) imsg,
                 imsgLength);

    if (debug >=3)
    {
        FM_LOG_PRINT("Request:\n");
        HexDump((fm_byte*)imsg, imsgLength);
    }

     /* Not included in the header if FALSE, similar to HW from AQ point of view */
    saiHdr = TRUE;
    sai = -1;

    port = ntohs(imsg->port);
    dest =  imsg->data[DEST_OFF];
    source =  imsg->data[SOURCE_OFF];
    opcode  = imsg->data[OPCODE_OFF];
    tag = imsg->data[TAG_OFF] & 0x7;
    bar = (imsg->data[TAG_OFF] >> 3) & 0x7;

    dataOff = 4;
    if (saiHdr)
    {
        /* Optional header from ATQ/ARQ perpective */
        exphdr = imsg->data[4] & 0x7F;
        fbe = imsg->data[8] & 0xF;
        sbe = (imsg->data[8] >> 4) & 0xF;

        sai = *(fm_uint16*)(imsg->data + SAI_OFF);
        dataOff = 8;
    }

    rsp = IOSF_RSP_SUCCESS;

    /* Validate dest and source port */
    if (dest != 0x1)
    {
        FM_LOG_INFO(FM_LOG_CAT_PLATFORM,
                "dest=0x%x is not valid strap for HLP - ignoring\n", dest);
	    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);
    }
    if (source != 0)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
            "source=0x%x is non-zero\n", source);
            rsp = IOSF_RSP_FAIL;
    }

    /* Validate sai */
    //ValidateSai(sai);

    if (bar)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
            "bar=0x%x is non-zero\n", bar);
            rsp = IOSF_RSP_FAIL;
    }

    if (saiHdr)
    {
        if (exphdr)
        {
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                "EXPHDR=0x%x is non-zero\n", exphdr);
                rsp = IOSF_RSP_FAIL;
        }

        if (opcode == IOSF_REG_READ || opcode == IOSF_REG_WRITE)
        {
            if (fbe != 0xF)
            {
                FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                    "fbe=0x%x is not 0xF\n", fbe);
                    rsp = IOSF_RSP_FAIL;
            }
            if (!(sbe == 0xF || sbe == 0x0))
            {
                FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                    "sbe=0x%x is not 0xF or 0x0\n", sbe);
                    rsp = IOSF_RSP_FAIL;
            }
        }
    }

    addr = (*(fm_uint32*)(imsg->data + dataOff + 4)) >> 12;
    if (addr)
    {
        /* Ignore but check high order bits */
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
            "Addr[47:28]=0x%x is non-zero\n", addr);
        rsp = IOSF_RSP_FAIL;
    }

    FM_CLEAR(emsg);
    eDataLen = dataOff + COMP_DATA_OFF;

    if (rsp == IOSF_RSP_SUCCESS)
    {
        addr = *(fm_uint32*)(imsg->data + dataOff + 2);

        if (debug >= 3)
            printf("dest 0x%x src 0x%x opcode 0x%x tag %d sai 0x%x addr 0x%x\n",
                dest, source, opcode, tag, sai, addr);
//FIXME: how to handle failure in the middle of block operation
        switch (opcode)
        {
            case IOSF_REG_READ:
                if (FM_OK == ReadCSR64(sw, addr, &val64))
                {
                    rsp = IOSF_RSP_SUCCESS;
                    emsg.data[OPCODE_OFF] = IOSF_COMP_DATA;
                    SET_64(emsg.data + eDataLen, val64);
                    eDataLen += 8;
                }
                else
                {
                    rsp = IOSF_RSP_FAIL;
                    emsg.data[OPCODE_OFF] = IOSF_COMP_NO_DATA;
                }
                break;
            case IOSF_REG_WRITE:
                val64 = GET_64(imsg->data + dataOff + 8);
                if (debug >= 3)
                {
                    printf("Write 0x%x <= 0x%llx\n", addr, val64);
                }
                if (FM_OK == WriteCSR64(sw, addr, val64))
                {
                    rsp = IOSF_RSP_SUCCESS;
                }
                else
                {
                    rsp = IOSF_RSP_FAIL;
                }
                emsg.data[OPCODE_OFF] = IOSF_COMP_NO_DATA;
                break;
            case IOSF_REG_BLK_READ:
                emsg.data[OPCODE_OFF] = IOSF_COMP_NO_DATA;
                rsp = IOSF_RSP_SUCCESS;
                ndw = imsg->data[dataOff + NDW_OFF];
                ndwOff = dataOff + 8; //Start of the multiple read addresses
                iDataLen = imsgLength - FM_MODEL_MSG_HEADER_SIZE;
                if (debug > 4) {
                    printf("#### ndwOff %d iDataLen %d\n", ndwOff, iDataLen);
                }

                if (ndw > MAX_READ_BASE_NDW)
                {
                    FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                        "ndw %d is out of range\n", ndw);
                    ndw =  MAX_READ_BASE_NDW;
                    rsp = IOSF_RSP_FAIL;
                    break;
                }

                do
                {
                    if (debug >= 3)
                    {
                        printf("ndw %d addr 0x%x\n", ndw, addr);
                    }
                    if (ndw & 1)
                    {
                        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                            "ndw %d is odd value\n", ndw);
                        rsp = IOSF_RSP_FAIL;
                        break;
                    }
                    for (cnt = 0; cnt < ndw/2; cnt++)
                    {
                        if (FM_OK == ReadCSR64(sw, addr + cnt*2*4,
                                                      &val64))
                        {
                            if (debug >= 3)
                                printf("Read: Addr 0x%x = 0x%llx\n",
                                       addr + cnt*2*4, val64);
                            SET_64(emsg.data + eDataLen, val64);
                            eDataLen += 8;
                        }
                        else
                        {
                            /* FIXME: Failure in middle, what should be the response */
                            rsp = IOSF_RSP_FAIL;
                            break;
                        }
                    }

                    if (ndwOff <= (iDataLen - 4))
                    {
                        ndw = imsg->data[ndwOff] & 0xF;
                        if (ndw > MAX_READ_REP_NDW)
                        {
                            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                                "ndw is out of range %d\n", ndw);
                            ndw =  MAX_READ_REP_NDW;
                            rsp = IOSF_RSP_FAIL;
                            break;
                        }
                        addr = (*(fm_uint32*)(imsg->data+ndwOff)) >> 4;
                    }
                    ndwOff += 4;
                } while (ndwOff <= iDataLen && rsp == IOSF_RSP_SUCCESS);
                rsp = IOSF_RSP_SUCCESS;
                emsg.data[OPCODE_OFF] = IOSF_COMP_DATA;
                break;
            case IOSF_REG_BLK_WRITE:
                emsg.data[OPCODE_OFF] = IOSF_COMP_NO_DATA;
                rsp = IOSF_RSP_SUCCESS;
                ndw = imsg->data[dataOff + NDW_OFF];
                valOff = dataOff + 8;
                ndwOff = valOff + ndw*4; //Start of the multiple write addresses
                iDataLen = imsgLength - FM_MODEL_MSG_HEADER_SIZE;

                if (ndw > MAX_READ_BASE_NDW)
                {
                    FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                        "ndw %d is out of range\n", ndw);
                    ndw =  MAX_READ_BASE_NDW;
                    rsp = IOSF_RSP_FAIL;
                    break;
                }

                do
                {
                    if (debug >= 3)
                    {
                        printf("ndw %d addr 0x%x valOff %d ndwOff %d iDataLen %d\n",
                            ndw, addr, valOff, ndwOff, iDataLen);
                    }
                    if (ndw & 1)
                    {
                        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                            "ndw %d is odd value\n", ndw);
                        rsp = IOSF_RSP_FAIL;
                        break;
                    }
                    for (cnt = 0; cnt < ndw/2; cnt++)
                    {
                        val64 = GET_64(imsg->data + valOff + cnt*8);
                            if (debug >= 3)
                                printf("Write: Addr 0x%x = 0x%llx\n",
                                       addr + cnt*2*4, val64);
                        if (FM_OK != WriteCSR64(sw, addr + cnt*2*4,
                                                       val64))
                        {
                            /* FIXME: Failure in middle, what should be the response */
                            rsp = IOSF_RSP_FAIL;
                            break;
                        }
                    }

                    valOff = valOff + ndw*4 + 4; /* next values */
                    if (valOff <= iDataLen)
                    {
                        ndw = imsg->data[ndwOff] & 0xF;
                        if (ndw > MAX_READ_REP_NDW)
                        {
                            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                                "ndw is out of range %d\n", ndw);
                            rsp = IOSF_RSP_FAIL;
                            break;
                        }
                        addr = (*(fm_uint32*)(imsg->data+ndwOff)) >> 4;
                        ndwOff = valOff + ndw*4; /* Next ndwOff */
                    }
                } while (((valOff + ndw*4) <= iDataLen) &&
                         rsp == IOSF_RSP_SUCCESS);
                break;
            default:
                rsp = IOSF_RSP_FAIL;
                FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                            "Invalid IOSF opcode 0x%x",
                            opcode);
                break;
        }
    }

    emsg.type = htons(FM_MODEL_MSG_IOSF);
    msgLength = eDataLen + FM_MODEL_MSG_HEADER_SIZE;
    emsg.msgLength = htonl(msgLength);

    emsg.data[DEST_OFF] = source;
    emsg.data[SOURCE_OFF] = strap_iosfsb_endpt_id;

    /* EH=1, resp, tag */
    emsg.data[TAG_OFF] = (1 << 7) | (rsp << 3) | tag;
    if (saiHdr)
    {
        *(fm_uint16*)(emsg.data + SAI_OFF) = strap_endpt_sai;
    }

    if (debug >=3)
    {
        FM_LOG_PRINT("Reply: rsp=%d. posted port=%d fd=%d\n",
                     rsp, port, socket->sock);
        HexDump((fm_byte*)&emsg, msgLength);
    }

    if (port == NONPOSTED_PORT)
    {
        SendMessage(socket, &emsg, msgLength);
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

} /* end HandleMsgIosf */

#else /* Duke use case */

#define SBIOSF_REQ_HDR_SIZE 4
#define SBIOSF_RESP_HDR_SIZE 2
#define SBIOSF_MAX_DATA_SIZE 128

static fm_status HandleMsgIosf(fm_int               sw,
        fm_socket           *socket,
        fm_modelMessage *    imsg,
        fm_int32             imsgLength)
{
    fm_uint32 buf[SBIOSF_REQ_HDR_SIZE + SBIOSF_MAX_DATA_SIZE];
    fm_modelMessage emsg;
    fm_int32 emsgLen;
    fm_int32 dataLen;
    fm_int32 bufLen;
    fm_byte ndw;
    fm_status err;
    fm_int32 i;
    fm_byte dest;
    fm_byte source;
    fm_byte opcode;
    fm_byte tag;
    fm_byte rsp;
    fm_uint dataOff;
    fm_uint32 addr;

    /* Validate dest and source port */
    dest = imsg->data[DEST_OFF];
    source = imsg->data[SOURCE_OFF];
    if (dest != 0x1) {
        FM_LOG_INFO(FM_LOG_CAT_PLATFORM,
                "dest=0x%x is not valid strap for HLP - ignoring\n", dest);
	    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);
    }

    if (source != 0) {
        FM_LOG_WARNING(FM_LOG_CAT_PLATFORM,
            "source=0x%x is non-zero\n", source);
    }

    /* Simulate CPK FW and replace the dest/source with straps ID */
    imsg->data[DEST_OFF] = 0x1f;
    imsg->data[SOURCE_OFF] = 0xaa;
    /* Enable SAI header and write SAI target number: 0x55 */
    imsg->data[TAG_OFF] = 0xc0;
    imsg->data[SAI_OFF] = 0x55;
    dataLen = imsgLength - FM_MODEL_MSG_HEADER_SIZE;

    if (debug >= 3) {
        opcode = imsg->data[OPCODE_OFF];
        tag = imsg->data[TAG_OFF] & 0x7;
        dataOff = 8;

        addr = *(fm_uint32*)(imsg->data + dataOff + 2);
        printf("API->HLP: dest 0x%x src 0x%x opcode 0x%x tag %d addr 0x%x\n",
                dest, source, opcode, tag, addr);

        switch (opcode)
        {
            case IOSF_REG_READ:
                printf("Register read\n");
                break;
            case IOSF_REG_WRITE:
                printf("Register write\n");
                break;
            case IOSF_REG_BLK_READ:
                printf("Bulk register read\n");
                break;
            case IOSF_REG_BLK_WRITE:
                printf("Bulk register write\n");
                break;
            default:
                FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                        "Invalid IOSF opcode 0x%x",
                        opcode);
                break;
        }

        HexDump(imsg->data, dataLen);
    }

    bufLen = dataLen / 4;
    memset(buf, 0, sizeof(buf));
    memcpy(buf, imsg->data, dataLen);
    dukeWrite("master", DUKE_MASTER_HDR, buf, bufLen);

    /* ndw that are part of the data (exclude header) */
    ndw = bufLen - SBIOSF_REQ_HDR_SIZE;

    /* HDR is all valid and type is non-posted. Reset rx counter and trigger */
    buf[0] = 0x0400501f | (ndw << 5);
    dukeWrite("message ctrl", DUKE_MSG_CTRL, buf, 1);

    dukeWaitForCompletion();

    dukeRead("target (rx) count", DUKE_TARGET_COUNT, buf, 1);
    dataLen = buf[0] / 4; /* dataLen = SB-IOSF header + data */
    ndw = dataLen  - SBIOSF_RESP_HDR_SIZE; /* ndw = data only */

    FM_CLEAR(emsg);

    dukeRead("target header", DUKE_TARGET_HDR,
            (uint32_t *)emsg.data, SBIOSF_RESP_HDR_SIZE);
    dukeRead("target data", DUKE_TARGET_DATA,
            (uint32_t *)emsg.data + SBIOSF_RESP_HDR_SIZE, ndw);

    /* Workaround for HLP registers not supported in Duke */
    rsp = (emsg.data[TAG_OFF] >> 3) & 0x3;
    if (rsp != IOSF_RSP_SUCCESS) {
        emsg.data[TAG_OFF] &= ~(0x3 << 3);
        memset(emsg.data + (SBIOSF_RESP_HDR_SIZE * 4), 0, 8);
        dataLen += 8;
    }

    emsg.type = htons(FM_MODEL_MSG_IOSF);
    emsgLen = dataLen * 4 + FM_MODEL_MSG_HEADER_SIZE;
    emsg.msgLength = htonl(emsgLen);

    if (debug >= 3) {
        printf("IOSF response - len=%d\n", emsgLen);

        dest = emsg.data[DEST_OFF];
        source = emsg.data[SOURCE_OFF];
        opcode = emsg.data[OPCODE_OFF];
        tag = emsg.data[TAG_OFF] & 0x7;
        rsp = (emsg.data[TAG_OFF] >> 3) & 0x3;

        printf("HLP->API: dest 0x%x src 0x%x opcode 0x%x tag %d rsp 0x%x\n",
                dest, source, opcode, tag, rsp);
        HexDump(emsg.data, emsgLen);
    }

    if (htons(imsg->port) == NONPOSTED_PORT) {
        SendMessage(socket, &emsg, emsgLen);
    }

    return 0;
}
#endif /* #ifndef PLATFORM_DUKE */

/*****************************************************************************/
/** SendNvmReadErrorMsg
 * \ingroup intModel
 *
 * \desc            Send nvm read response error message.
 *
 * \param[in]       socket points to the socket where the message is originated.
 *
 * \param[in]       emsg points to the message that is to be send.
 *
 * \param[in]       length is the buffer length.
 *
 *****************************************************************************/
static void SendNvmReadErrorMsg(fm_socket       *socket,
                                fm_modelMessage *emsg,
                                fm_uint16        length)
{
    fm_aq_desc *out_aq_desc;
    fm_int32    msgLength;

    out_aq_desc = (fm_aq_desc *)emsg->data;
    msgLength = FM_MODEL_MSG_HEADER_SIZE + sizeof(*out_aq_desc) + length;
    emsg->msgLength = htonl(msgLength);
    out_aq_desc->params.nvm.length = length;
    out_aq_desc->datalen = length;
    out_aq_desc->retval = FM_AQ_RC_EINVAL;
    SendMessage(socket, emsg, ntohl(emsg->msgLength));
}

/*****************************************************************************/
/** HandleMsgNvmRead
 * \ingroup intModel
 *
 * \desc            Processes nvm read message.
 *                                                                      \lb\lb
 *                  The control request message format is:
 *                                                                      \lb\lb
 *                  sw field in header is the version number.
 *                  +--------------+-----------+--------------+------------+
 *                  | header (12B) | NVM_READ message
 *                  +--------------+-----------+--------------+------------+
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[in]       socket points to the socket where the message is originated.
 *
 * \param[in]       imsg points to the received message that is to be processed.
 *
 * \param[in]       msgLength is the length of the received message in units of
 *                  bytes.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status HandleMsgNvmRead(fm_int               sw,
                                  fm_socket           *socket,
                                  fm_modelMessage *    imsg,
                                  fm_int32             msgLength)
{
    fm_status       status = FM_OK;
    fm_uint         img_file_size;
    fm_aq_desc    * in_aq_desc;
    fm_bool         read_file;
    fm_uint32       offset;
    fm_modelMessage emsg;
    unsigned int    len;
    FILE *fp;

    FM_NOT_USED(sw);
    FM_NOT_USED(socket);
    FM_NOT_USED(msgLength);

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d imsg=%p msgLength=%d\n",
                 sw,
                 (void *) imsg,
                 msgLength);

    in_aq_desc = (fm_aq_desc *)imsg->data;

    FM_CLEAR(emsg);

    /* Copy header along with aqc_nvm structure. */
    FM_MEMCPY_S(&emsg,
                FM_MODEL_MSG_SIZE,
                imsg,
                FM_MODEL_MSG_HEADER_SIZE + sizeof(*in_aq_desc));

    /* Let's attempt to be smart and guess the NVM read version. */
    if (in_aq_desc->params.nvm.module_pointer == 0x52 &&
        in_aq_desc->params.nvm_1p2.module_pointer == 0x52) {
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                     "WM cannot guess which NVM Read Version to use. Using NVM read from HAS 1.3\n");

    } else if (in_aq_desc->params.nvm_1p2.module_pointer == 0x52) {
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "Using NVM read from HAS 1.2\n");

        /* Translate in NVM 1.3 format */
        fm_aqc_nvm nvm_tmp;
        nvm_tmp.offset_low = in_aq_desc->params.nvm_1p2.offset & 0xFFFF;
        nvm_tmp.offset_high = (in_aq_desc->params.nvm_1p2.offset >> 16) & 0xFF;
        nvm_tmp.command_flags = in_aq_desc->params.nvm_1p2.command_flags;
        nvm_tmp.length = in_aq_desc->params.nvm_1p2.length;
        nvm_tmp.module_pointer = in_aq_desc->params.nvm_1p2.module_pointer;
        nvm_tmp.addr_high = in_aq_desc->params.nvm_1p2.addr_high;
        nvm_tmp.addr_low = in_aq_desc->params.nvm_1p2.addr_low;
        in_aq_desc->params.nvm = nvm_tmp;
    } else  {
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "Using NVM read from HAS 1.3\n");
    }

    offset  = in_aq_desc->params.nvm.offset_high << 16;
    offset |= in_aq_desc->params.nvm.offset_low;
    if ((in_aq_desc->params.nvm.length + offset) >
        FM_SWITCH_NVM_IMAGE_SIZE) {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                     "Out of range offset/byte_count for module 0x%x\n",
                     in_aq_desc->params.nvm.module_pointer);
        SendNvmReadErrorMsg(socket, &emsg, 0);
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);
    }

    fp = fopen(nvmImgFile, "r");
    if (!fp) {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                     "Unable to open nvm image file '%s' - '%d'\n",
                     nvmImgFile, errno);
        SendNvmReadErrorMsg(socket, &emsg, 0);
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);
    }

    /* Get image file size. */
    fseek(fp, 0L, SEEK_END);
    img_file_size = ftell(fp);
    rewind(fp);

    read_file = TRUE;
    if (offset > 0) {
        if (offset < img_file_size) {
            if (fseek(fp, offset, SEEK_SET) != 0) {
                FM_LOG_ERROR(FM_LOG_CAT_PLATFORM, "Unable to find offset %u\n",
                             offset);
                SendNvmReadErrorMsg(socket, &emsg, 0);
                status = FM_ERR_INVALID_ARGUMENT;
                FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);
            }
        } else {
            len = 0;
            read_file = FALSE;
        }
    }

    if (read_file)
        len = fread(&emsg.data[sizeof(*in_aq_desc)],
                    1,
                    in_aq_desc->params.nvm.length,
                    fp);

    if (len < in_aq_desc->params.nvm.length) {
        if ((len + offset) < img_file_size) {
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                         "Unexpected errror while reading nvm image file\n");
            SendNvmReadErrorMsg(socket, &emsg, len);
            status = FM_ERR_INVALID_ARGUMENT;
            FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);
        } else {
            /* Fill rest of response with zeroes. */
            memset(&emsg.data[sizeof(*in_aq_desc) + len],
                   0,
                   in_aq_desc->params.nvm.length - len);
        }
    }

    msgLength = FM_MODEL_MSG_HEADER_SIZE + sizeof(*in_aq_desc) +
                in_aq_desc->params.nvm.length;
    emsg.msgLength = htonl(msgLength);

    SendMessage(socket, &emsg, ntohl(emsg.msgLength));

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

} /* end HandleMsgNvmRead */


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
            default:
                FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                    "Uhandled Packet TLV type %d\n", type);
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
 * \param[in]       imsg points to the received message that is to be processed.
 *
 * \param[in]       physPort is the logical port on which the packet ingresses
 *                  the model.
 *
 * \param[in]       imsgLength is the length of the received message in units of
 *                  bytes.
 *
 * \param[out]      emsg points to the send message that is to be processed.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
static fm_status HandleMsgPacket(fm_int               sw,
                                 fm_modelMessage *    imsg,
                                 fm_int               physPort,
                                 fm_int32             imsgLength)
{
    fm_modelSidebandData sbData;
    fm_status            status;
    fm_int32             emsgLength;
    fm_int32             pktLength;
    fm_int               freeSpace;
    fm_uint16            numPkts;
    fm_byte *            data;
    fm_byte *            packet;
    errno_t              rv;
    fm_modelMessage     *emsg;
    fm_modelMessage      msg;
#define MAX_PKT_LB      24
    fm_pkt_loopback      pktLoopback[MAX_PKT_LB];
    fm_int               numPktLb;
    fm_int               numPktLbProcessed;
    static fm_int        idCounter = 0;

    FM_LOG_ENTRY( FM_LOG_CAT_PLATFORM,
                  "sw=%d, imsg=%p, physPort=%d, imsgLength=%d\n",
                  sw,
                  (void *) imsg,
                  physPort,
                  imsgLength );

    if (portLinkState[physPort] != PORT_LINK_UP)
    {
        FM_LOG_INFO(FM_LOG_CAT_PLATFORM,
                     "TB: Link down %d. Dropping RX packet on phys port %d.\n",
                     portLinkState[physPort], physPort);
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);
    }

    packet = NULL;
    pktLength = 0;
    FM_CLEAR(sbData);
    numPktLb = 0;
    numPktLbProcessed = 0;

    DecodePacket(imsg, imsgLength, &packet, &pktLength, &sbData);

    if ( ( packet == NULL ) || ( pktLength == 0 ) )
    {
        /* This packet message is too short. */
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
    }

    sbData.idTag = idCounter;
    idCounter++;
    status = fmModelSendPacket(sw,
                               physPort,
                               packet,
                               pktLength,
                               &sbData);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

RESEND_PACKET:
    if (numPktLbProcessed < numPktLb) {
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                     "TB: Sending loopback packet#%d to phys port %d\n",
                     numPktLbProcessed, physPort);
        pktLoopback[numPktLbProcessed].sbData.idTag = idCounter;
        idCounter++;
        status = fmModelSendPacket(sw,
                                  pktLoopback[numPktLbProcessed].physPort,
                                  pktLoopback[numPktLbProcessed].pkt,
                                  pktLoopback[numPktLbProcessed].pktLen,
                                  &pktLoopback[numPktLbProcessed].sbData);
        if (status)
        {
            FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,"TB: Failed to resend loopback packet\n");
        }
        status = FM_OK;
        fmFree(pktLoopback[numPktLbProcessed].pkt);
        numPktLbProcessed++;
    }
    numPkts = 0;
    emsgLength = 0;
    FM_CLEAR(msg);
    emsg = &msg;

    while (status == FM_OK)
    {
        data = emsg->data;

        /***************************************************
         * Receive the packet(s) egressing from the model.
         **************************************************/
        *data = FM_MODEL_DATA_PACKET;
        packet = &data[FM_MODEL_MSG_TLV_SIZE];
        FM_CLEAR(sbData);

        status = fmModelReceivePacket(sw,
                                      &physPort,
                                      packet,
                                      &pktLength,
                                      FM_MODEL_MAX_PACKET_SIZE,
                                      &sbData);

	if (portLinkState[physPort] != PORT_LINK_UP)
	{
		FM_LOG_INFO(FM_LOG_CAT_PLATFORM,
			    "TB: Link down %d. Dropping TX packet on phys port %d.\n",
			    portLinkState[physPort], physPort);
		continue;
	}

        *( (fm_int32 *) &data[FM_MODEL_DATA_TYPE_SIZE] ) = htonl(pktLength);
        /* status value of FM_ERR_SPARE1 is used to indicate that
         * loopback is requested for the current packet */
        if (status == FM_ERR_SPARE1)
        {
            FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                         "TB: loopback packet received on phys port %d,"
                         " requeue to send process\n",
                         physPort);
            if (numPktLb < MAX_PKT_LB)
            {
                pktLoopback[numPktLb].physPort = physPort;
                pktLoopback[numPktLb].pktLen = pktLength;
                pktLoopback[numPktLb].pkt = fmAlloc(pktLength);
                FM_MEMCPY_S(pktLoopback[numPktLb].pkt, pktLength, packet,
                            pktLength);
                FM_MEMCPY_S(&pktLoopback[numPktLb].sbData,
                            sizeof(fm_modelSidebandData), &sbData,
                            sizeof(fm_modelSidebandData));
                numPktLb++;
            }
            else
            {
                FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                             "TB: loopback packet received on phys port %d,"
                             " Too many loopback packets. Dropping.\n",
                             physPort);
            }
            /* Packet is loopbacked, and not copied out */
            status = FM_OK;
            continue;
        }

        /* Check for valid packet. */
        if ( ( ( status == FM_OK ) &&
               ( pktLength >= 0 ) &&
               ( pktLength < FM_MODEL_MAX_PACKET_SIZE ) ) ||
             ( ( status == FM_ERR_NO_MORE ) ) )
        {
            /* Finalize and enqueue the packet. */
            emsgLength = FM_MODEL_MSG_HEADER_SIZE;
            if (status == FM_ERR_NO_MORE)
            {
                emsg->type                        = FM_MODEL_MSG_PACKET_EOT;
                emsgLength                        += 2;
                *( (fm_uint16 *) &emsg->data[0] ) = htons(numPkts);
//FIXME
                continue;
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
            emsg->port      = htons(physPort);
            emsg->type      = htons(emsg->type);

            FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                         "TB: Egress packet #%d on physPort %d, length %d\n",
                         numPkts,
                         physPort,
                         emsgLength);

            if (debug >= 2)
            {
                printf("Packet sent back to port %d\n", physPort);
                HexDump(emsg->data, ntohl(emsg->msgLength));
            }
            if (pktRecvSockets[physPort].sock  < 0)
            {
                printf("Dropping packet egressing port %d\n", physPort);
            }
            else
            {
                SendMessage(&pktRecvSockets[physPort] , emsg,
                            ntohl(emsg->msgLength));
            }
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
    /* requeue loopback packets */
    if (numPktLbProcessed < numPktLb) {
        goto RESEND_PACKET;
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
                                        fm_modelMessage *    imsg,
                                        fm_int               type)
{
    fm_status           status = FM_OK;
    fm_uint16           port;
    fm_uint16           networkPort;
    fm_text             host;

    FM_NOT_USED(sw);
    FM_NOT_USED(type);

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d, imsg=%p type=%d\n",
                 sw,
                 (void *) imsg,
                 type);

    port        = ntohs(imsg->port);
    if (port >= MAX_PHYS_PORT)
    {
        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM, "Egress port %d is out of range\n",
                     port);
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    networkPort = ntohs( *( (fm_uint16 *) imsg->data ) );
    host        = (fm_text) &imsg->data[2];

    if (networkPort == FM_MODEL_SOCKET_PORT_DISABLE)
    {
        /**************************************************
         * Shut down the socket on this model egress port.
         **************************************************/
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "Disabling egress port %d\n", port);


        /* If there was a socket on this port previously, close it down. */
        if (pktRecvSockets[port].sock != -1)
        {
            fmCloseNetworkConnection(&pktRecvSockets[port]);
        }

        pktRecvSockets[port].sock = -1;
    }
    else
    {
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                     "Setting destination for egress port %d to %s:%d\n",
                     port, host, networkPort);

        if (pktRecvSockets[port].sock >= 0)
        {
            FM_LOG_WARNING(FM_LOG_CAT_PLATFORM,
                    "Packet receive socket for port %d is active. Close existing one %d.\n",
                    port, pktRecvSockets[port].sock);
            fmCloseNetworkConnection(&pktRecvSockets[port]);
            pktRecvSockets[port].sock = -1;
        }

        fmCreateNetworkClient(&pktRecvSockets[port],
                              FM_SOCKET_TYPE_TCP,
                              host,
                              networkPort);
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                     "Packet connection for port %d created. sock = %d\n",
                     port, pktRecvSockets[port].sock);
    }

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end HandleMsgSetEgressInfo */



/*****************************************************************************/
/** ProcessMessage
 * \ingroup intModel
 *
 * \desc            Processes a received message.
 *
 * \param[in]       socket points to the socket where the message is originated.
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
fm_status ProcessMessage(fm_socket *socket, fm_modelMessage *imsg,
                         fm_int32 msgLength)
{
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

    FM_LOG_PRINT("TB: Received packet datagram: sw=%d port=%d sock=%d type=%d pktLength=%d\n",
                 sw, port, socket->sock, type, pktLength);

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

    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
        "TB: process message type=%d\n", type);

    /* Switch according to packet type received */
    switch (type)
    {
        case FM_MODEL_MSG_LINK_STATE:
            status = HandleMsgLinkState(sw, socket, imsg, msgLength);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            break;
        case FM_MODEL_MSG_SET_EGRESS_INFO:
        case FM_MODEL_MSG_ENABLE_ALTERNATIVE_DATA_PATH:
            status = HandleMsgSetEgressInfo(sw, imsg, type);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            break;
        case FM_MODEL_MSG_PACKET_LOOPBACK:
            FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                "TB: process message type=FM_MODEL_MSG_PACKET_LOOPBACK\n");
            /* FALLTHRU */
        case FM_MODEL_MSG_PACKET:
            msg_stat.packet++;
            status = HandleMsgPacket(sw, imsg, port, msgLength);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            break;

        case FM_MODEL_MSG_MGMT:
            msg_stat.mgmt++;
            status = HandleMsgMgmt(sw, socket, imsg, msgLength);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            break;
        case FM_MODEL_MSG_CTRL:
            msg_stat.ctrl++;
            status = HandleMsgCtrl(sw, socket, imsg, msgLength);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            break;
        case FM_MODEL_MSG_IOSF:
            msg_stat.iosf++;
            status = HandleMsgIosf(sw, socket, imsg, msgLength);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            break;
        case FM_MODEL_MSG_VERSION_INFO:
            status = HandleMsgVersionInfo(sw, socket, imsg, msgLength);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            break;
        case FM_MODEL_MSG_NVM_READ:
            status = HandleMsgNvmRead(sw, socket, imsg, msgLength);
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
/** SendInterruptEvent
 * \ingroup intModel
 *
 * \desc            Send IOSF interrupt event message.
 *
 * \param[in]       socket points to the socket where the message is originated.
 *
 * \param[in]       intr is the global interrupt value.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status SendInterruptEvent(fm_socket *socket, fm_int64 intr)
{
    fm_modelMessage  emsg;
    fm_uint          msgLength;


    FM_CLEAR(emsg);

    emsg.type = htons(FM_MODEL_MSG_IOSF);
    msgLength = 20 + FM_MODEL_MSG_HEADER_SIZE;
    emsg.msgLength = htonl(msgLength);

    emsg.data[DEST_OFF] = strap_interrupt_destid;
    emsg.data[SOURCE_OFF] = strap_iosfsb_endpt_id;
    emsg.data[OPCODE_OFF] = IOSF_REG_WR_PRIV_CTRL;

    /* EH=1, AL=1, bar=0, tag=0 */
    emsg.data[3] = (0x3 << 6);
    *(fm_uint16*)(emsg.data + SAI_OFF) = strap_endpt_sai;
    /* fbe=0xf, sbe=0xf */
    emsg.data[8] = 0xFF;
    *(fm_uint64*)&emsg.data[12] = intr;

    if (debug >=3)
    {
        FM_LOG_PRINT("INTR: 0x%llx\n", intr);
        HexDump((fm_byte*)&emsg, msgLength);
    }

    SendMessage(socket, &emsg, msgLength);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);
}

fm_status ParseUint64(char *string, fm_uint64 *result)
{
    char *endptr = NULL;

    if (string == NULL)
    {
        *result = 0;
        return -1;
    }

    *result = 0;

    if (string[1] == 'x')
    {
        *result = strtoull(string + 2, &endptr, 16);
    }
    else
    {
        *result = strtoull(string, &endptr, 10);
    }

    return (string == endptr) ? FM_ERR_INVALID_ARGUMENT : FM_OK;

}

fm_status ParseInt(char *string, fm_int *result)
{
    char *endptr = NULL;

    if (string == NULL)
    {
        *result = 0;
        return -1;
    }

    *result = 0;

    if (string[1] == 'x')
    {
        *result = strtoul(string + 2, &endptr, 16);
    }
    else
    {
        *result = strtoul(string, &endptr, 10);
    }

    return (string == endptr) ? FM_ERR_INVALID_ARGUMENT : FM_OK;

}

int loadTextCfg(char *filename)
{
	size_t line_size = 0;
	char *line = NULL;
    unsigned int addr;
    time_t start;
    time_t end;
    uint64_t val;
	FILE *fp;
	int len;
	int err;
    int cnt = 0;
    int lineNum = 0;

	if (!filename)
		return -1;

    if (strlen(filename) < 1) return 0;

    start = time(NULL);
	fp = fopen(filename, "r");

	if (!fp) {
		printf("Unable to open '%s' - '%d'\n", filename, errno);
		return -1;
	}
    printf("Loading text file: %s\n", filename);
	while ((len = getline(&line, &line_size, fp)) != -1) {
        lineNum++;
		if (len <= 2 || line[0] == '#')
			continue;

		/* Remove CR */
		if ((line[len - 1] == '\n') || (line[len - 1] == '\r')) {
			len--;
			line[len] = '\0';
		}
		if ((line[len - 1] == '\n') || (line[len - 1] == '\r')) {
			len--;
			line[len] = '\0';
		}

        if (line[0] == '#')
            continue;

        if (2 != sscanf(line, "0x%x 0x%lx\n", &addr, &val)) {
            printf("ERROR: Fail to parse line#%d: %s\n", lineNum, line);
            continue;
        }

        if (debug >= 3) printf("Addr %x %lx\n", addr, val);
        cnt++;

        if (WriteCSR64(0, addr, val))
        {
            printf("ERROR: Fail to write 0x%lx to 0x%x\n", val, addr);
        }

	}

	if (line)
		free(line);

	fclose(fp);

    end = time(NULL);
    printf("%s: %d entries loaded in %lld seconds\n", filename,
           cnt, (long long)(end - start));

	return 0;
}

int loadParserCfg(char *filename)
{
    fm_status       status = FM_OK;
    fm_uint32       addr;
    fm_uint32       value;

    printf("Setting parser registers default values\n");
    for (addr = HLP_PARSER_BASE; addr < HLP_PARSER_BASE + HLP_PARSER_SIZE; addr += 4)
    {
        value = hlpModelGetRegisterDefault(addr);

        status = WriteCSR(0, addr, value);
        if (status == FM_ERR_NOT_FOUND)
        {
            /* Ignore out-of-bounds errors. */
            status = FM_OK;
        }
        if (status)
            return status;

    }

    status = loadTextCfg(filename);
    return status;
}

static int processChunk(unsigned int modNum, unsigned int chnkNum,
                        uint16_t *chunk, unsigned int chunkLen) {
    unsigned int type;
    unsigned int addr;
    unsigned int size;
    unsigned int cnt;
    unsigned int n = 0;
    uint64_t val;
    int numEntries = 0;

    while (n < chunkLen) {
        type = chunk[n] & 0xF;
        switch (type) {
        case 5:
            size = 6;
            break;
        default:
            printf("MOD[%d] CHNK[%d]: Invalid type %d at chunk word %d\n",
                   modNum, chnkNum, type, n);
            return -1;
        }
        addr = chunk[n + 1];
        addr <<= 12;
        addr |= (chunk[n] >> 4);

        val = 0;
        for (cnt = 3; cnt < 4; cnt--) {
            val <<= 16;
            val |= chunk[n + cnt + 2];
        }
        numEntries++;
        if (outFp)
        {
                fprintf(outFp, "0x%07x 0x%lx\n", addr, val);
        }
        WriteCSR64(0, addr, val);
        n += size;
    }
    return numEntries;
}

static int processModule(unsigned int modNum, uint16_t *module,
                         unsigned int len)
{
    unsigned int n = 0;
    uint16_t cl;
    int cnt = 0;
    int num;
    int numEntries = 0;

    while (n < len) {
        cl = module[n];
        if (debug >= 3)
            printf("MOD[%d] CHNK[%d] LEN: %d\n", modNum, cnt, cl);
        if (cl > 4096) {
            printf("MOD[%d]: Invalid chunk len %d(0x%x) at byte offset %d(0x%x)\n",
                   modNum, cl, cl, n * 2, n * 2);
            return -1;
        }
        if (cl == 0) {
            printf("MOD[%d]: Invalid chunk len %d\n", cnt, cl);
            return -1;
        }
        num = processChunk(modNum, cnt, module + n + 1, cl);
        if (num <= 0)
            return -1;
        numEntries += num;

        cnt++;
        n += (cl + 1);
    }
    return numEntries;
}

static int loadImg(char *filename, fm_uint32 *alBitmask)
{
    unsigned int addr;
    time_t start;
    time_t end;
    uint64_t val;
	FILE *fp;
	unsigned int len;
	int err;
    unsigned int cnt = 0;
    unsigned int num, numEntries = 0;
    uint32_t header[NVM_HDR_SIZE/4];
    uint32_t offset, modLen;
    uint16_t module[NVM_MOD_SIZE];

	if (!filename)
		return -1;

    if (strlen(filename) < 1) return 0;

    start = time(NULL);
	printf("Loading file: %s\n", filename);
	fp = fopen(filename, "r");

	if (!fp) {
		printf("Unable to open '%s' - '%d'\n", filename, errno);
		return -1;
	}

	if ((len = fread(header, NVM_HDR_SIZE, 1, fp)) != NVM_HDR_SIZE) {

        /* read the default alBitmask from Header PFA if not overwriten*/
        for (cnt = 0; cnt < 8; cnt++) {
            if (alBitmask[cnt] == UNDEF_VAL)
                alBitmask[cnt] = header[0x820/4 + cnt];
        }

        if (debug >= 2) {
            printf("IMG_ID : %08x\n", header[0]);
            printf("IMG_VER: %04x\n", header[1] & 0xFFFF);
            printf("REG_VER: %04x\n", header[1] >> 16);
            printf("CHKSM  : %04x\n", header[2]);
            printf("IMG_LEN: %d\n", header[3]);
            printf("MAX_LEN: %d\n", header[4]);
            printf("\n");
            printf("AL_BITMASK:\n");
            for (cnt = 0; cnt < 8; cnt++) {
                printf(" %08x", alBitmask[cnt]);
            }
            printf("\n");

        }
        if (outFp)
        {
            fprintf(outFp, "# Generated from %s: version %d checksum %04X\n",
                    filename, header[1] & 0xFFFF, header[2]);
        }
        if (header[0] != 0x4563AABB) {
            /* If text, load text image */
            if (isprint(header[0] & 0xFF)) {
                fclose(fp);
                printf("Loading text file: %s\n", filename);
                return loadTextCfg(filename);
            }
            printf("Expect header of 0x4563AABB but got 0x%08x\n", header[0]);
            return -1;
        }
        printf("Signed binary image version %02x.%02x with checksum %04X\n",
                        (header[1] >> 8) & 0xFF, header[1] & 0xFF, header[2]);
        for (cnt = 0; cnt < 255; cnt++) {
            offset = header[0x20/4 + cnt];
            if (debug >= 3)
                printf("AL_MODULE_BASE[%d]: %08x\n", cnt, offset);
            if (offset < 0xFFFFFF) {
                if (!(alBitmask[cnt/32] & (1ULL << (cnt%32)))) {
                    if (debug >= 2)
                        printf("Skipping module %d due to bit mask disabled\n",
                               cnt);
                    continue;
                }
                if (outFp)
                {
                        fprintf(outFp, "# PFA #%d\n", cnt);
                }
                if (fseek(fp, offset, SEEK_SET) != 0) {
                    printf("Unable to find module %d at offset %u\n",
                           cnt, offset);
                    return -1;
                }
                if ((len = fread(&modLen, 4, 1, fp)) != 1) {
                    printf("Unable to read module length. Got %d\n", len);
                    return -1;
                }
                if (debug >= 3)
                    printf("MODULE[%d] LEN:: %d\n", cnt, modLen);
                if (modLen > NVM_MOD_SIZE) {
                    printf("Invalid module length %d greater than module size %d\n",
                           modLen, NVM_MOD_SIZE);
                }
                if (modLen >
                    (header[0x20/4 + cnt + 1] - header[0x20/4 + cnt])) {
                    printf("Invalid module length %d greater then next module offset %d\n",
                           modLen,
                           header[0x20/4 + cnt + 1] - header[0x20/4 + cnt]);
                }
                if ((len = fread(module, 2, modLen, fp)) != modLen) {
                    printf("Unable to read module of size %d . Got %d\n",
                           modLen, len);
                    return -1;
                }
                if (debug >= 3) {
                    printf("MODULE[%d]:\n", cnt);
                    HexDump((unsigned char*)module, modLen * 2);
                }
                num = processModule(cnt, module, modLen);
                if (num <= 0) {
                    printf("Unable to process module %d\n", cnt);
                    return -1;
                }
                numEntries += num;
            }
        }
        /* Calculate FWD_PORT_CFG_1(cpkPort) */
        addr = HLP_FWD_PORT_CFG_1(20,0);
        val = 0x3FFFFFF;
        if (outFp)
        {
                fprintf(outFp, "0x%07x 0x%lx\n", addr, val);
        }
        WriteCSR64(0, addr, val);

	} else {
        printf("Failed to read image header. Got %d bytes\n", len);
        return -1;
    }

	fclose(fp);

    end = time(NULL);
    printf("%s: %d entries loaded in %lld seconds\n", filename,
           numEntries, (long long)(end - start));

	return 0;
}

void logPrintHandler(fm_uint64 level, char *log)
{
    FM_NOT_USED(level);
    printf("%s\n", log);
}

static void runRegBenchmark()
{
    unsigned int numops = 1000;
    struct timeval start;
    struct timeval end;
    fm_uint64 addr;
    fm_uint64 writeVal;
    fm_uint64 readVal;
    unsigned int i;
    double usec;

    /* Basic sanity check before testing performance */
    addr = HLP_MAC_SCRATCH(0, 0);
    writeVal = 0x1234567890abcdef;
    WriteCSR64(0, addr, writeVal);
    ReadCSR64(0, addr, &readVal);
    if (readVal != writeVal) {
        printf("Sanity check failed: register addr=%02llx writeVal=%02llx readVal=%02llx\n",
                addr, writeVal, readVal);
        return;
    }

    /* Benchmark register write */
    gettimeofday(&start, NULL);
    for (i = 0; i < numops; ++i)
        WriteCSR64(0, HLP_MAC_SCRATCH(i % HLP_MAC_SCRATCH_ENTRIES, 0),
                   writeVal);
    gettimeofday(&end, NULL);
    usec = (double)(end.tv_sec - start.tv_sec) * 1.0e6 +
           (double)(end.tv_usec - start.tv_usec);
    printf("Register write avg time: %0.3f usec\n", usec / numops);

    /* Benchmark register read */
    gettimeofday(&start, NULL);
    for (i = 0; i < numops; ++i)
        ReadCSR64(0, HLP_MAC_SCRATCH(i % HLP_MAC_SCRATCH_ENTRIES, 0),
                   &readVal);
    gettimeofday(&end, NULL);
    usec = (double)(end.tv_sec - start.tv_sec) * 1.0e6 +
           (double)(end.tv_usec - start.tv_usec);
    printf("Register read avg time: %0.3f usec\n", usec / numops);

}

static void print_usage(char *cmd)
{
	int i = 0;
	printf("Usage: %s [options]\n", cmd);
	printf("    -h                    - This help.\n");
	printf("    -p <port>             - Set server port\n");
	printf("    -l                    - Disable all model logging output\n");
	printf("    -r                    - Don't reset chip.\n");
	printf("    -F                    - Don't load config file.\n");
	printf("    -I                    - Don't check and send interrupt messages.\n");
	printf("    -f <file>             - Load specified NVM image instead of nvm.img\n");
	printf("    -fp <file>            - Load specified config file for parser configuration\n");
#ifdef PLATFORM_DUKE
	printf("    -D                    - Apply Duke custom configuration on top of NVM image\n");
#endif
	printf("    -o <file>             - Save startup register writes to config file.\n");
	printf("    -m <mask>             - Override first 64-bit AL BitMask.\n");
	printf("    -d <debug>            - Specify logging level.\n");
	printf("    -v <cat1,cat2>        - Enable logging from selected categories.\n");
	printf("    -B                    - Benchmark register access performance and exit.\n\n");

	printf("Allowed values for category names are: \n");
	printf(" - all: Enable output from all the categories \n");
	printf(" - none: Disable output from all the categories \n");
	printf(" - Comma separated list of the following category names:\n     ");

    while (log_cat_names[i]) {
       printf("%s  ", log_cat_names[i]);
       if (++i % 8 == 0 && log_cat_names[i])
           printf("\n     ");
    }
    printf("\n");
	exit(0);
}

/*****************************************************************************/
/* main
 * \ingroup intModel
 *
 * \desc            Entry point for model server.
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
    fm_status       status;
    fm_int          sw = 0;
    fm_uint32       val32;
    fm_uint64       val64;
    fm_int          i;
    fm_libCfg       libCfg;
    fm_bool         resetChip = TRUE;
    fm_text         parserCfgFile = NULL;
    fm_bool         useDukeSettings = FALSE;
    fm_text         outFile = NULL;
    fm_int          sendIntr = 1;
    fm_int          intrStep = INTERRUPT_READ_DELAY;
    fm_bool         benchmarkMode = FALSE;
    fm_uint32       alBitmask[8] = {UNDEF_VAL, UNDEF_VAL, UNDEF_VAL, UNDEF_VAL,
                                    UNDEF_VAL, UNDEF_VAL, UNDEF_VAL, UNDEF_VAL};
    fm_socket            serverSocket;
    fm_int               serverPort = 0;
    fm_socket            regAccessPipe;
    fm_bool              usePipe = FALSE;

    fm_bool              dataPresent;
    fm_modelMessage      imsg;
    fm_int32             msgLength;
    fm_timestamp         timeout;
    fm_socket           *sockets[MAX_PERSISTENT_CONNECTIONS + 1];
    fm_int               numSockets = 1;
    fm_int               eventsReceived[MAX_PERSISTENT_CONNECTIONS + 1];

	fd_set rfds;
	struct timeval tv;
    char c;
    int rv;
    int fd = 0;


    memset(&libCfg, 0, sizeof(libCfg));
    libCfg.logLevel = debug;
    libCfg.logHandler = logPrintHandler;
    for (i = 1 ; i < argc ; i++)
    {
        if (!strcmp(argv[i], "-d") && (i+1 < argc) )
        {
           ParseInt(argv[i+1], &debug);
           printf("Debug is set to %d\n", debug);
           libCfg.logLevel = debug;
#ifdef PLATFORM_DUKE
		   dukeSetDebug(debug);
#endif
           i++;
        }
        else if (!strcmp(argv[i], "-p") && (i+1 < argc) )
        {
           ParseInt(argv[i+1], &serverPort);
           printf("Server port is %d\n", serverPort);
           i++;
        }
        else if (!strcmp(argv[i], "-m") && (i+1 < argc) )
        {
           ParseUint64(argv[i+1], &val64);
           alBitmask[0] = val64;
           alBitmask[1] = val64 >> 32;
           printf("AL Bitmask[0-1] is 0x%08x %08x\n",
                  alBitmask[0], alBitmask[1]);
           i++;
        }
        else if (!strcmp(argv[i], "-P"))
        {
            usePipe = !usePipe;
        }
        else if (!strcmp(argv[i], "-f") && (i+1 < argc) )
        {
            nvmImgFile = argv[i+1];
            i++;
        }
        else if (!strcmp(argv[i], "-fp") && (i+1 < argc) )
        {
            parserCfgFile = argv[i+1];
            i++;
        }
#ifdef PLATFORM_DUKE
        else if (!strcmp(argv[i], "-D") )
        {
            useDukeSettings = TRUE;
        }
#endif
        else if (!strcmp(argv[i], "-F") )
        {
            nvmImgFile = NULL;
        }
        else if (!strcmp(argv[i], "-o") && (i+1 < argc) )
        {
            outFile = argv[i+1];
            i++;
        }
        else if (!strcmp(argv[i], "-l") )
        {
            libCfg.logLevel = -1 ;
            printf("Logging is set to disabled\n");
        }
        else if (!strcmp(argv[i], "-r"))
        {
            resetChip = !resetChip;
        }
        else if (!strcmp(argv[i], "-I"))
        {
            sendIntr = !sendIntr;
        }
        else if (!strcmp(argv[i], "-v"))
        {
            fmModelLibSetLogCat(argv[++i]);
        }
        else if (!strcmp(argv[i], "-B") )
        {
            benchmarkMode = TRUE;
        }
        else
        {
			print_usage(argv[0]);
			exit(0);
        }
    }

	/* TODO can we avoid these in the Duke case? */
    status = fmModelLibInit(sw, &libCfg);
    if (status)
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "FAILED to init model lib: err= %d\n", status);
        return status;
    }

    if (benchmarkMode)
    {
        runRegBenchmark();
        exit(0);
    }

    /* Can skip this for debug if defaults values are not important */
    if (resetChip)
    {
        status = fmModelReset(sw);
        if (status)
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "FAILED to reset chip. err=%d\n", status);
        }
    }
    else
    {
        printf("Skipping chip reset\n");
    }
	if (outFile) {
		printf("Saving register writes to file: %s\n", outFile);
		outFp = fopen(outFile, "w+");
	}

#ifdef PLATFORM_DUKE
	dukeSetupHandle();
	dukeCleanRxQueue();

    if (useDukeSettings) {
        enableBSMAccess();
    }
#endif

    if (nvmImgFile) {
        if (loadImg(nvmImgFile, alBitmask))
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "Failed to load NVM image: %s\n", nvmImgFile);
    }
    if (parserCfgFile) {
        /* Need to clear NVM parser related stuff and load new cfg. */
        if (loadParserCfg(parserCfgFile))
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "Failed to load parser config: %s\n", parserCfgFile);
    }
#ifdef PLATFORM_DUKE
    if (useDukeSettings) {
        DukeCustomization();
    }
#endif

    status = InitializeSocketInfoFile();
    if (status != FM_OK)
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Unable to create socket info file!\n");

        return status;
    }

    fmCreateNetworkServer(&serverSocket,
                          FM_SOCKET_TYPE_TCP,
                          serverPort,
                          3);
    status = AddSocketInfoToFile("localhost", serverSocket.serverPort, usePipe);
    if (status != FM_OK)
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Unable to add socket info to file!\n");

        return status;
    }

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

            return FM_ERR_NO_MEM;
        }
        sockets[i + 1]->type = FM_SOCKET_TYPE_TCP;
    }

    /* Initially just the server */
    sockets[0] = &serverSocket;
    numSockets = 1;
    for (i = 0; i < MAX_PHYS_PORT; i++)
    {
        pktRecvSockets[i].sock = -1;
        pktRecvSockets[i].type = FM_SOCKET_TYPE_TCP;
        portLinkState[i] = PORT_LINK_UP;
    }

    /* Using pipe for mgmt access */
    if (usePipe)
    {
        printf("Using pipe for mgmt access\n");
        mkfifo(TX_PIPE, 0666);
        mkfifo(RX_PIPE, 0666);

        regAccessPipe.rxPipe = open(RX_PIPE, O_RDONLY | O_NONBLOCK);
        if (regAccessPipe.rxPipe <= 0)
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                "Unable to open RX pipe\n");
        }

        /* Cannot open TX_PIPE here, must be done after client open first */
        regAccessPipe.txPipe = -1;

        sockets[PIPE_IDX] = &regAccessPipe;
        sockets[PIPE_IDX]->type = FM_SOCKET_TYPE_PIPE;
        numSockets = 2;
    }

    printf("Waiting for TCP connections on port %d\n", serverSocket.serverPort);
    printf("Type 'h' for help\n");
    fflush(0); /* for redirect stdout to file */

    while (TRUE)
    {
			FD_ZERO(&rfds);
			FD_SET(fd, &rfds);
			tv.tv_sec = 0;
			tv.tv_usec = 0;

			rv = select(fd + 1, &rfds, NULL, NULL, &tv);
            if (rv) {
                read(fd, &c, 1);
                switch (c)
                {
                    case 'h':
                        printf("Help:\n");
                        printf("\tq             - Quit\n");
                        printf("\ts             - Show messages statistics\n");
                        printf("\tr             - Reset messages statistics\n");
                        printf("\tl             - Toggle logging output\n");
                        printf("\t0..6          - Set debug level. 1=PRINT, 2=DEBUG, 4=DEBUG3, etc\n");
                        printf("\ta             - Enable output log for all categories\n");
                        printf("\tn             - Disable output log for all categories\n");
                        printf("\tt<c>          - Toggle output log for category <c> (see below)\n");
                        i = 0;
                        while (log_cat_names[i]) {
                            printf("\tt%c            - Toggle %s (%s)\n",
                                   'a' + i, log_cat_names[i],
                                   log_cat_mask & (1<<i) ? "enabled" : "disabled");
                            i++;
                        }
                    break;
                    case 's':
                        printf("Messages stats:\n");
                        printf("\tPACKET    : %d\n", msg_stat.packet);
                        printf("\tMGMT      : %d\n", msg_stat.mgmt);
                        printf("\tCTRL      : %d\n", msg_stat.ctrl);
                        printf("\tIOSF      : %d\n", msg_stat.iosf);
                    break;
                    case 'r':
                        memset(&msg_stat, 0, sizeof(msg_stat));
                    break;
                    case 'q':
                        if (usePipe)
                        {
                            unlink(MODEL_FILE ".rxpipe");
                            unlink(MODEL_FILE ".txpipe");
                        }
                        exit(0);
                    break;
                    case '0':
                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                        debug = c - '0';
#ifndef PLATFORM_DUKE
                        libCfg.logLevel = debug;
                        fmModelLibSetLogLevel(libCfg.logLevel);
#else
						dukeSetDebug(debug);
                        printf("Debug is set to %d\n", debug);
#endif
                    break;
                    case 'l':
                        if (libCfg.logLevel < 0)
                        {
                            libCfg.logLevel = 0;
                            printf("Log level is set to %d\n", libCfg.logLevel);
                        }
                        else
                        {
                            libCfg.logLevel = -1;
                            printf("All logging is disabled\n");
                        }
                        fmModelLibSetLogLevel(libCfg.logLevel);
                    break;
                    case 'g':
                        for (i = 0; i < numSockets; i++)
                        {
                            printf("Socket#%d fd %d type %d\n", i,
                                   sockets[i]->sock, sockets[i]->type);
                        }
                    break;
                    case 'a':
                        printf("Output log for all categories is enabled\n");
                        log_cat_mask = 0xFFFFFFFFFFFFFFFF;
                    break;
                    case 'n':
                        printf("Output log for all categories is disabled\n");
                        log_cat_mask = 0;
                    break;
                    case 't':
						if (read(fd, &c, 1) <= 0 || c < 'a' || c > 'n') {
							printf("Category is not valid. Type 'h' for help\n");
							break;
						}
                        unsigned int j = c - 'a';
                        if (log_cat_mask & (1<<j)) {
                            printf("Output of category %s is disabled\n", log_cat_names[j]);
                            log_cat_mask &= ~(1 << j);
                        }
                        else {
                            printf("Output of category %s is enabled\n", log_cat_names[j]);
                            log_cat_mask |= 1 << j;
                        }
                    break;
                }
            }

        //fmModelTick(0, &val32);
        if (sendIntr && intrStep <= 0)
        {
            handleIntr(numSockets, sockets);
            intrStep = INTERRUPT_READ_DELAY;
        }
        --intrStep;

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
             * No data available, no packets to forward so go wait
             * for a message forever.
             **************************************************/
            continue;
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
            (void)ProcessMessage(sockets[i], &imsg, msgLength);
        }

        fflush(0); /* for redirect stdout to file */

    }   /* end while (TRUE) [Process incoming messages] */

#ifdef PLATFORM_VELOCE
    Py_Finalize();
#endif

    exit(0);

}   /* end main */

