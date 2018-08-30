
#include <stdio.h>
#include <zlib.h>
#include "crc32.h"
#include "clientLib.h"
#include "reg_types.h"
#include "api/internal/hlp/hlp_api_regs_int.h"


static void l2Cfg()
{
	int i;
	/* Enable CPM ports
	 *    Technically you can't write this on the fly without changing
	 *    pages, but the WM doesn't implement the scheduler anyway
	 */
	writeUINT64(HLP_IARB_ADAPTIVE_SCHEDULE(0,map_log_to_phys(CPM0_LOG_PORT),0), 
				(CPM0_LOG_PORT << 16) | 0xffff);
	writeUINT64(HLP_EARB_ADAPTIVE_SCHEDULE(0,map_log_to_phys(CPM0_LOG_PORT),0), 
				(CPM0_LOG_PORT << 16) | 0xffff);
	writeUINT64(HLP_IARB_ADAPTIVE_SCHEDULE(0,map_log_to_phys(CPM1_LOG_PORT),0), 
				(CPM1_LOG_PORT << 16) | 0xffff);
	writeUINT64(HLP_EARB_ADAPTIVE_SCHEDULE(0,map_log_to_phys(CPM1_LOG_PORT),0), 
				(CPM1_LOG_PORT << 16) | 0xffff);

	/* Enable port to port forwarding */
	for (i = 0; i < 20; i++)
	{
		writeUINT64(HLP_FWD_PORT_CFG_1(i,0), 
					(3 << 24) | 0x7fffff);
	}
}

static void printPacket(unsigned char *packet, int len)
{
	int i;
	int j;

	for (i = 0 ; i < len ; ) {
		printf("[%04d]: ", i);
		for( j = 0 ; j < 16 && i < len; j++, i++) 
			printf("%02x ", packet[i]);
		printf("\n");
	}
}

static unsigned char *generatePacket( unsigned char *dmac, unsigned char *smac, 
				int len)
{
	int i;
	unsigned char *packet;
	unsigned int crc;

	packet = malloc(len*sizeof(*packet));

	if(!packet) {
		printf("Error allocating memory\n");
		exit(0);
	}

	memcpy(&packet[0], dmac, 6);
	memcpy(&packet[6], smac, 6);

	for (i = 12 ; i < len-4 ; i++)
		packet[i] = i;

	crc = fmCrc32(packet, len-4);

	for (i = 0 ; i < 4; i++)
		packet[len-4+i] = (crc >> (8*i)) & 0xff;

	printPacket(packet, len);
	return packet;
}

int main(int argc, char *argv[])
{
	int i;
	char *pktServerFile = NULL;
	unsigned char mac1[] = {0x00,0x01,0x02,0x03,0x04,0x05};
	unsigned char mac2[] = {0x00,0x07,0x08,0x09,0x0A,0x0B};
	int pktLen = 64;

	if (argc < 1) {
		printf("Please specify models.packetServer file\n");
		exit(1);
	}

	pktServerFile = argv[1];
	connectCpuToWm(pktServerFile);
	for ( i = 0 ; i < 32; i++)
		connectToWmEthernetPort(i);

	l2Cfg();

	unsigned int logPort = 1;
	unsigned int logPort2 = 2;

	printf("Sending first packet to port %d\n", logPort);
	unsigned char *packet = generatePacket(mac1, mac2, pktLen);

	rimmonSendPkt(0, map_log_to_phys(logPort), packet, pktLen);
	free(packet);

	unsigned char *packetRcv = NULL;
	unsigned char sbData[32];
	unsigned int lenRcv = -1;
	/* Packet should flood to all ports, except source */
	for ( i = 0 ; i < 23 ; i++) {
		printf("Checking log port %d...\n", i);
		recvPacket(map_log_to_phys(i), 0.5*1000, &packetRcv, &lenRcv,
			   sbData);

		if (packetRcv != NULL)
			printf("Received packet on port %d len %d\n", i, lenRcv);

		if(packetRcv != NULL) free(packetRcv);
	}

	pktLen = 256;
	/* Cross the streams */
	printf("Sending second packet in on port %d to newly learned mac on port %d\n",
			logPort2, logPort);
	packet = generatePacket(mac2, mac1, pktLen);
	rimmonSendPkt(0, map_log_to_phys(logPort2), packet, pktLen);
	free(packet);

	for ( i = 0 ; i < 23 ; i++) {
		printf("Checking log port %d...\n", i);
		recvPacket(map_log_to_phys(i), 0.5*1000, &packetRcv, &lenRcv,
			   sbData);

		if (i == 1 && packetRcv == NULL)
			printf("ERROR: expecting packet on port 1\n");
		else if (i == 1)
			printf("Received packet on port %d len %d\n", i, lenRcv);
		else if (packetRcv != NULL)
			printf("ERROR: unexpected packet on port %d len %d\n",
					i, lenRcv);

		if(packetRcv != NULL) free(packetRcv);
	}

	cleanup();
	exit(0);
}
