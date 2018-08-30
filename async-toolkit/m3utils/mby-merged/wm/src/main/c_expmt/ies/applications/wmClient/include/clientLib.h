
#ifndef _CLIENTLIB_H_
#define _CLIENTLIB_H_

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <poll.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>

#define CPK_LOG_PORT	20
#define CPM0_LOG_PORT	21
#define CPM1_LOG_PORT	22

#define FM_MODEL_MSG_PACKET           0
#define FM_MODEL_MSG_SET_EGRESS_INFO  3
#define FM_MODEL_MSG_MGMT             7
#define FM_MODEL_MSG_ERROR            10
#define FM_MODEL_MSG_IOSF             11
#define FM_MODEL_MSG_CTRL             12

#define FM_MODEL_MGMT_READ_REQUEST    1
#define FM_MODEL_MGMT_READ_RESPONSE   2
#define FM_MODEL_MGMT_WRITE           3
#define FM_MODEL_MGMT_WRITE_ACK       4
#define FM_MODEL_MGMT_READ64_REQUEST  5
#define FM_MODEL_MGMT_READ64_RESPONSE 6
#define FM_MODEL_MGMT_WRITE64         7
#define FM_MODEL_MGMT_WRITE64_ACK     8

#define FM_MODEL_CTRL_CHIP_RESET_REQ 1

#define NONPOSTED_PORT                0
#define POSTED_PORT                   1

#define MAX_PORTS                     32

#define FM_MODEL_MAX_PACKET_SIZE            /* 65791 */ 32767
#define FM_MODEL_MAX_TLV_SIZE               256

#define FM_MODEL_RIMMON_META_SIZE	8
#define FM_MODEL_CPK_META_SIZE		32
#define MAX_MSG_BUF			15000

#define READ_TIMEOUT			2000 /* msec */

typedef enum
{
    /** Ethernet frame. */
    FM_MODEL_DATA_PACKET = 0xA0, /* Non-zero field so it is easier to locate */

    /** 32-bit packet identifier. */
    FM_MODEL_DATA_SB_ID,

    /** 8-bit egress traffic class. */
    FM_MODEL_DATA_SB_TC,

    /** Packet Meta. */
    FM_MODEL_PACKET_META,

} fm_modelDataType;


#define FM_MODEL_MSG_LENGTH_SIZE	4
#define FM_MODEL_MSG_VERSION_SIZE	2
#define FM_MODEL_MSG_HEADER_SIZE        12

#define FM_MODEL_MSG_DATA_SIZE		(FM_MODEL_MAX_TLV_SIZE + FM_MODEL_MAX_PACKET_SIZE)
#define FM_MODEL_MSG_SIZE               (FM_MODEL_MSG_HEADER_SIZE + FM_MODEL_MSG_DATA_SIZE)


#define FM_MODEL_DATA_TYPE_SIZE		1
#define FM_MODEL_DATA_LENGTH_SIZE	4
#define FM_MODEL_MSG_TLV_SIZE           (FM_MODEL_DATA_TYPE_SIZE + FM_MODEL_DATA_LENGTH_SIZE)

int resetChip();
int readUINT64(unsigned int addr, unsigned long int *value);
int writeUINT64(unsigned int addr, unsigned long int value);
int readUINTMult64(unsigned int addr, unsigned int numRead,
		   unsigned long int *value);
int writeUINTMult64(unsigned int addr, unsigned int n,
		    unsigned long int *value);
void cpkSendPkt(int sw, int physPort, unsigned char *sendData, int length);
void rimmonSendPkt(int sw, int physPort, unsigned char *sendData, int length);
void sendPkt(int sw, int physPort, unsigned char *sendData,
             int length, unsigned char *pktMeta, int metaLen, int logPort);
void cleanup();
void setDebug(int val);
void connectCpuToWm(char *filename);
void connectToWmEthernetPort(unsigned int physPort);
void recvPacket(unsigned int port, int timeout, unsigned char **packet,
                unsigned int *len, unsigned char sbData[]);
int map_phys_to_log(int phys);
int map_log_to_phys(int log);

#endif

