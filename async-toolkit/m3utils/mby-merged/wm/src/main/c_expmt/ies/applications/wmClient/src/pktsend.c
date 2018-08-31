
#include <stdio.h>
#include <zlib.h>
#include "crc32.h"
#include "clientLib.h"
#include "reg_types.h"
#include "api/internal/hlp/hlp_api_regs_int.h"

#include <arpa/inet.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <net/if.h>
#include <netinet/ether.h>
#include <netinet/if_ether.h>
#include <linux/if_packet.h>
#include <linux/if_ether.h>
//#include <linux/if_arp.h>
#include <time.h>
#include <sys/time.h>

#define MAX_PCAP_PTKS	1000
#define PCAP_MAGIC	0xa1b2c3d4

#define NUM_LOG_PORT	24
#define MAX_MTU		5000
#define MAX_IO_VEC	2
#define NOT_USED	-1234

struct netdev {
	int port;
	char ifName[IFNAMSIZ];
	int fd;
	unsigned char mac[6];
};

struct pcapHdr {
	uint32_t     magic;		/* magic */
	uint16_t     majorVer;		/* major version number */
	uint16_t     minorVer;		/* minor version number */
	uint32_t     thisZone;		/* GMT to local correction */
	uint32_t     tsAccuracy;	/* accuracy of timestamps */
	uint32_t     snapLen;		/* max length of captured packets, in octets */
	uint32_t     network;		/* data link type */
};

struct pcapRecHdr {
	int32_t      tsSec;		/* timestamp seconds */
	uint32_t     tsUsec;		/* timestamp microseconds */
	uint32_t     savedLen;		/* number of octets of packet saved in file */
	uint32_t     pktlen;		/* actual length of packet */
};

struct pcap_pkt {
	struct pcapRecHdr hdr;
	unsigned char *bytes;
};

static unsigned char rxbuf[MAX_IO_VEC][MAX_MTU];
int debug = 0;
static unsigned int pktCnt = 0;
static int wait_netdev_coming_up = 0;

int ParseUint(char *string, unsigned int *result)
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

    return (string == endptr) ? -1 : 0;

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

int ParseBytes(unsigned char *packet, char *string, unsigned int maxLen,
	       int calCrc)
{
	unsigned int i;
	unsigned int strLen;
	unsigned int pktLen;
	unsigned int crc;
	int rv;

	if (!string) {
		for (i = 0; i < maxLen; i++)
			packet[i] = i;
		pktLen = maxLen;
	} else {
		strLen = strlen(string);

		i = 0;
		pktLen = 0;
		while (i < strLen) {
			if (string[i] == ' ' || string[i] == ':') {
				i++;
				continue;
			}
			rv = sscanf(string + i, "%02x", &packet[pktLen]);
			if (rv != 1)
				break;
			pktLen++;
			i = i + 2;
		}

		for (i = pktLen; i < maxLen; i++)
			packet[i] = i - pktLen;

		pktLen = i;
	}

	if (calCrc)
	{
		crc = fmCrc32(packet, pktLen - 4);

		for (i = 0 ; i < 4; i++)
			packet[pktLen-4+i] = (crc >> (8*i)) & 0xff;
	}

	return pktLen;
}

static int GetPcapPkt(char *filename, struct pcap_pkt *pcapPkts,
		      unsigned int size, int calCrc)
{

	FILE *fp;
	struct pcapHdr fh;
        int freads;
	int i;
	unsigned int crc;
	unsigned int cnt = 0;

	if((fp = fopen(filename, "r")) == NULL) {
                printf("ERROR: Opening file %s\n", filename);
                return -2;
        }

	/* first we read the pcap file header */
        freads = fread(&fh, sizeof(fh), 1, fp);
        /* if EOF, exit */
        if (freads == 0) {
		printf("No header found\n");
                return 1;
	}

        /* if magic number in NOK, exit */
        if (fh.magic != PCAP_MAGIC) {
		printf("Invalid MAGIC %x\n", fh.magic);
                return -2;
	}

	for (cnt = 0; cnt < size; cnt++) {
		/* next the  pcap packet header */
		freads = fread(&pcapPkts[cnt].hdr, sizeof(pcapPkts[cnt].hdr),
			       1, fp);

		/* if EOF, exit */
		if (freads == 0) {
			if (!cnt) {
				printf("No packet header found\n");
				return -2;
			}
			return 0;
		}

		/* 4 bytes for CRC */
		pcapPkts[cnt].bytes = malloc(pcapPkts[cnt].hdr.pktlen + 4);
		if (!pcapPkts[cnt].bytes) {
			printf("Pkt[%d]: Unable to malloc of length %u\n",
				cnt, pcapPkts[cnt].hdr.pktlen);
			return -2;
		}

		if (pcapPkts[cnt].hdr.savedLen > pcapPkts[cnt].hdr.pktlen) {
			printf("Pkt[%d]: savedLen %u is greater than pktlen %u\n",
				cnt, pcapPkts[cnt].hdr.pktlen);
			pcapPkts[cnt].hdr.savedLen = pcapPkts[cnt].hdr.pktlen;
		}

		/* and the packet itself, but only up to the capture length */
		freads = fread(pcapPkts[cnt].bytes, pcapPkts[cnt].hdr.savedLen,
			       1, fp);

		/* if EOF, exit */
		if (freads == 0) {
			printf("Pkt[%d]: No packet data found\n", cnt);
			/* Mark end of packet array */
			free(pcapPkts[cnt].bytes);
			pcapPkts[cnt].bytes = NULL;
			return -2;
		}

		/* Clear rest of bytes, if needed */
		for (i = pcapPkts[cnt].hdr.savedLen;
		     i < pcapPkts[cnt].hdr.pktlen; i++)
			pcapPkts[cnt].bytes[i] = 0;

		if (calCrc)
		{
			crc = fmCrc32(pcapPkts[cnt].bytes,
				      pcapPkts[cnt].hdr.pktlen);

			for (i = 0; i < 4; i++)
				pcapPkts[cnt].bytes[pcapPkts[cnt].hdr.pktlen+i] =
					(crc >> (8*i)) & 0xff;

			pcapPkts[cnt].hdr.pktlen += 4;
		}
	}

	fclose(fp);
	return 0;

}

static int dev_present(char *ifName)
{
	struct  ifreq ethreq;
	int     sockFd;

	if ((sockFd = socket(PF_PACKET, SOCK_RAW, htons(0x0003))) < 0)
	{
		perror("socket");
		return -1;
	}

	strncpy(ethreq.ifr_name, ifName, IFNAMSIZ);
	if (ioctl(sockFd, SIOCGIFINDEX, &ethreq) == -1)
	{
		return 0;
	}
	return 1;
}

static int netdev_open(struct netdev *netdev)
{
	char *ifName = netdev->ifName;
	struct  ifreq ethreq;
	struct  sockaddr_ll sock_addr;
	int     sockFd;

	if ((sockFd = socket(PF_PACKET, SOCK_RAW, htons(0x0003))) < 0)
	{
		perror("socket");
		return -1;
	}

	strncpy(ethreq.ifr_name, ifName, IFNAMSIZ);

	if (ioctl(sockFd, SIOCGIFINDEX, &ethreq) == -1)
	{
		netdev->fd = -1;
		return -1;
		perror("SIOCGIFINDEX");
	}

	sock_addr.sll_family = AF_PACKET;
	sock_addr.sll_protocol = 0;
	sock_addr.sll_ifindex = ethreq.ifr_ifindex;
	sock_addr.sll_hatype = 0;
	sock_addr.sll_pkttype = 0;
	sock_addr.sll_halen = 0;

	if (bind(sockFd, (struct sockaddr *)&sock_addr, sizeof(sock_addr)))
	{
		perror("bind");
	}

	ethreq.ifr_mtu = MAX_MTU; /* NIC card specific */
	if (ioctl(sockFd, SIOCSIFMTU, &ethreq) == -1)
	{
		printf("Failed to set device mtu: %d\n", ethreq.ifr_mtu);
		perror("MTU");
        }

	if (ioctl(sockFd, SIOCGIFFLAGS, &ethreq) == -1)
	{
		perror("SIOCGIFFLAGS:");
        }
	if (!(ethreq.ifr_flags & IFF_UP)) {
		wait_netdev_coming_up = 1;
	}

	/* Set the NIC promiscuous mode */
	ethreq.ifr_flags |= IFF_PROMISC;

	/* Bring the NIC up */
	ethreq.ifr_flags |= IFF_UP;

	if (ioctl(sockFd, SIOCSIFFLAGS, &ethreq) == -1)
	{
		perror("SIOCSIFFLAGS: PROMISC|UP");
	}

	/* Get MAC addr */
	if (ioctl(sockFd, SIOCGIFHWADDR, &ethreq) == -1)
	{
		perror("SIOCGIFHWADDR|UP");
	}

	/* Save the NIC mac addr in the switch structure */
	memcpy(netdev->mac, &(ethreq.ifr_hwaddr.sa_data), sizeof(netdev->mac));
	netdev->fd = sockFd;

	if (debug > 1)
		printf("Dev: %-10s: fd=%-2d MAC=%02x:%02x:%02x:%02x:%02x:%02x\n",
		       ifName, sockFd,
		       netdev->mac[0], netdev->mac[1], netdev->mac[2],
		       netdev->mac[3], netdev->mac[4], netdev->mac[5]);

	return 0;
}

int netdev_send(struct netdev *netdev, unsigned char *buf, int len)
{
	struct iovec	vector[2];
	int		vecCnt;
	int		rc;

	if (netdev->fd == NOT_USED) {
		return 0;
	}
	if (netdev->fd < 0) {
		printf("%s: Unable to send, invalid fd\n", netdev->ifName);
		return -1;
	}

	len -= 4; /* Not including CRC */

        vector[0].iov_base = buf;
        vector[0].iov_len = len;
        vecCnt = 1;

	if (debug > 2) {
		printf("Actual packet sent on %s:\n", netdev->ifName);
		printPacket(buf, len);
	}
	rc = writev(netdev->fd, vector, vecCnt);

        if (rc == -1)
        {
		perror("SEND");
	}

	return 0;
}

int netdev_recv(struct netdev *netdev)
{
	struct pollfd rfds[1];
	int           rfdsCnt;
	int           retval;
	int           i;
	int           sockFd;
	struct iovec  rxVector[MAX_IO_VEC];
	int           len;

	if (netdev->fd < 0) {
		return 0;
	}

	while (1) {
		rfdsCnt = 0;
		rfds[rfdsCnt].fd = netdev->fd;
		rfds[rfdsCnt].events = POLLIN;
		rfds[rfdsCnt].revents = 0;
		rfdsCnt++;

		retval = poll(rfds, rfdsCnt, 0);

		if (retval == -1)
		{
			perror("POLL");
			return 0;
		}
		else if (!retval)
		{
		    return 0;
		}

		rfdsCnt = 0;

		for (i = 0; i < MAX_IO_VEC; i++) {
			rxVector[i].iov_base = rxbuf[i];
			rxVector[i].iov_len = MAX_MTU;
		}

		len = readv(netdev->fd, rxVector, MAX_IO_VEC);
		if (len > 0) {
			pktCnt++;
			printf("#%d: Received %d-byte packet on port %d(%s)\n",
			       pktCnt, len, netdev->port, netdev->ifName);
			if (debug) {
				printPacket(rxbuf[0], len);
			}
		}
	}
	return 0;
}

static void delay(uint64_t usec)
{
	struct timespec ns_time;

	ns_time.tv_sec = usec/1000000;
	ns_time.tv_nsec = usec%1000;

	nanosleep(&ns_time, NULL);
}

static uint64_t get_time(uint64_t offset)
{
	struct timeval tv;
	uint64_t t;

	gettimeofday(&tv, NULL);

	return (tv.tv_sec*1000000 + tv.tv_usec - offset);
}

static void print_usage(char *cmd)
{
	printf("Usage: %s [options] [raw packet bytes]\n", cmd);
	printf("    -h                       - This help.\n");
	printf("    -N                       - Specify using netdev mode instead WM server.\n");
	printf("    -B <basename>            - Specify netdev basename if different from eth or rmn_tap or simics_tap.\n");
	printf("    -p <port>                - Specify egress port to send out, ingressed from CPK port.\n");
	printf("    -i <port>                - Specify ingress port to send to.(1)\n");
	printf("    -I <physPort>	     - Specify ingress physical port to send to.(1)\n");
	printf("    -f <filename>            - Specify pcap file instead of raw packet bytes.\n");
	printf("    -l <pktlen>              - Extend packet length for raw packet bytes.\n");
	printf("    -n <numpkts>             - Specify number of times to repeat each packet.\n");
	printf("    -z                       - Increase source mac for multiple packets sent.\n");
	printf("    -Z                       - Increase destination mac for multiple packets sent.\n");
	printf("    -P                       - Increase port for multiple packets sent.\n");
	printf("    -m <hexBytes>            - Specify packet meta. Default CPK port has LAN TX and other ports has Rimmon type.(1)\n");
	printf("                             - If port is specified, then ingress physical port is set to 28.\n");
	printf("    -w <sec>	             - Specify number of seconds to poll rx packets. 0 to skip\n");
	printf("    -W <msec>	             - Specify number of milli-seconds delay between packets. Ignore pcap timestamp.\n");
	printf("    -c                       - Don't update CRC.(1)\n");
	printf("    -x                       - Don't send, just poll for RX packets.\n");
	printf("    -d <debug>               - Specify debug value.\n");
	printf("NOTE: (1) Only applicable on model server and not undernetdev mode.\n");
	printf("Examples:\n");
	printf("    %s -i 1 00:11:22:33:44:55 - Send a packet ingress on Rimmon physical port 1 with specified DEST MAC\n", cmd);
	printf("    %s -l 96 -p 20            - Send a 96-byte packet ingress on CPK port for switching\n", cmd);
	printf("    %s -i 28 -m 01008280      - Send a packet ingress on CPK port with specified meta data\n", cmd);
	printf("    %s -p 2                   - Send a packet ingress on host port for direct send to port 2\n", cmd);
	printf("    %s -w 100 -x	      - No sending but just poll for RX packets for 100 seconds\n", cmd);


	exit(0);
}

int main(int argc, char *argv[])
{
	int i, j, k;
	char *pktServerFile = NULL;
	int pktLen = 64;
	int numPkts = 1;
	int bufLen;
	unsigned int ingressPort = 1;
	unsigned int ingressPhysPort = -1;
	int logPort = -1;
	unsigned char *packet = NULL;
	char *packetStr = NULL;
	unsigned char *packetMeta = NULL;
	char *packetMetaStr = NULL;
	int metaLen = 0;
	char *pcapFile = NULL;
	int calCrc = 1;
	unsigned int wait = 2;
	int incr_src_mac = 0;
	int incr_dst_mac = 0;
	int incr_port = 0;
	uint32_t *pu32;
	uint32_t val32;
	unsigned int crc;
	struct netdev netdev_eth[NUM_LOG_PORT];
	char *devname = NULL;
	struct pcap_pkt pcapPkts[MAX_PCAP_PTKS];
	time_t start;
	time_t end;
	long long delTime;
	int no_tx = 0;
	uint64_t start_time;
	uint64_t pcap_start_time;
	uint64_t pkt_time;
	uint64_t tx_time;
	uint64_t cur_time = 0;
	int pkt_delay_ms = -1; /* delay between packets */

	for (i = 1; i < argc; i++) {
		if (!strcmp(argv[i], "-d") && (i+1 < argc) ) {
			ParseUint(argv[i+1], &debug);
			printf("Debug is set to %d\n", debug);
			i++;
		} else if (!strcmp(argv[i], "-l") && (i+1 < argc) ) {
			ParseUint(argv[i+1], &pktLen);
			i++;
		} else if (!strcmp(argv[i], "-n") && (i+1 < argc) ) {
			ParseUint(argv[i+1], &numPkts);
			i++;
		} else if (!strcmp(argv[i], "-i") && (i+1 < argc) ) {
			ParseUint(argv[i+1], &ingressPort);
			i++;
		} else if (!strcmp(argv[i], "-I") && (i+1 < argc) ) {
			ParseUint(argv[i+1], &ingressPhysPort);
			i++;
		} else if (!strcmp(argv[i], "-p") && (i+1 < argc) ) {
			ParseUint(argv[i+1], &logPort);
			i++;
		} else if (!strcmp(argv[i], "-m") && (i+1 < argc) ) {
			packetMetaStr = argv[i+1];
			i++;
		} else if (!strcmp(argv[i], "-f") && (i+1 < argc) ) {
			pcapFile = argv[i+1];
			i++;
		} else if (!strcmp(argv[i], "-W") && (i+1 < argc) ) {
			ParseUint(argv[i+1], &pkt_delay_ms);
			i++;
		} else if (!strcmp(argv[i], "-w") && (i+1 < argc) ) {
			ParseUint(argv[i+1], &wait);
			i++;
		} else if (!strcmp(argv[i], "-B") && (i+1 < argc) ) {
			devname = argv[i+1];
			i++;
		} else if (!strcmp(argv[i], "-N") ) {
			if (dev_present("rmn_tap19"))
				devname = "rmn_tap";
			else if (dev_present("eth19"))
				devname = "eth";
			else if (dev_present("simics_tap19"))
				devname = "simics_tap";
			else {
				printf("Default netdev(ethX or rmn_tapX or simics_tapX) name is not present. Specify basename instead.\n");
			}
		} else if (!strcmp(argv[i], "-x") ) {
			no_tx = 1;
		} else if (!strcmp(argv[i], "-z") ) {
			incr_src_mac = 1;
		} else if (!strcmp(argv[i], "-Z") ) {
			incr_dst_mac = 1;
		} else if (!strcmp(argv[i], "-P") ) {
			incr_port = 1;
		} else if (!strcmp(argv[i], "-c") ) {
			calCrc = !calCrc;
		} else if (strlen(argv[i]) >= 6) {
			packetStr = argv[i];
		} else {
			print_usage(argv[0]);
			exit(0);
		}
	}

	if (!devname) {
		pktServerFile = getenv("SBIOSF_SERVER");
		if (!pktServerFile) {
			printf("SBIOSF_SERVER environment is not set\n");
			return -1;
		}
	}

	if (logPort >= 0)
		ingressPort = 20; /* CPK physical port */
	if (ingressPhysPort > 32)
		ingressPhysPort = map_log_to_phys(ingressPort);
	else
		ingressPort = map_phys_to_log(ingressPhysPort);

	if (devname && logPort < 0)
		logPort = ingressPort;

	if (debug > 1)
		printf("logPort %d ingress %d ingressPhys %d\n", logPort,
		       ingressPort, ingressPhysPort);

	setDebug(debug);

	if (packetMetaStr)
	{
		if (devname)
			printf("ERROR: packet meta option is not support under netdev mode\n");
		bufLen = strlen(packetMetaStr)/2;
		packetMeta = malloc(bufLen * sizeof(*packet));
		metaLen = ParseBytes(packetMeta, packetMetaStr, 0, 0);
	}

	if (pcapFile)
	{
		memset(pcapPkts, 0, sizeof(pcapPkts));
		if (GetPcapPkt(pcapFile, pcapPkts, MAX_PCAP_PTKS, calCrc))
		{
			printf("Failed to process file %s\n", pcapFile);
			exit(-1);
		}
	}
	else
	{
		bufLen = pktLen;
		if (packetStr && (bufLen < strlen(packetStr)/2))
			bufLen = strlen(packetStr)/2;
		pcapPkts[0].bytes = malloc(bufLen * sizeof(*packet));
		/* pktLen could increase due to size of packet string */
		pcapPkts[0].hdr.pktlen = ParseBytes(pcapPkts[0].bytes,
						    packetStr, pktLen, calCrc);
	}

	if (!devname) {
		connectCpuToWm(pktServerFile);
		for ( i = 0; i < 32; i++)
			connectToWmEthernetPort(i);
	} else {
		for ( i = 0; i < 20; i++) {
			sprintf(netdev_eth[i].ifName, "%s%d", devname, i);
			netdev_eth[i].port = i;
			if (netdev_open(&netdev_eth[i])) {
				printf("Failed to open %s\n",
				       netdev_eth[i].ifName);
			}
		}
		if (wait_netdev_coming_up) {
			/* Not sure why waiting would not help, so exit to let
			 * user try again */
			printf("Netdevs are being brought up. Try command again\n");
			exit(0);
		}
		for ( i = 20; i < 24; i++) {
			sprintf(netdev_eth[i].ifName, "netdev%d", i);
			netdev_eth[i].port = i;
			netdev_eth[i].fd = NOT_USED;
		}
	}

	pcap_start_time = pcapPkts[0].hdr.tsSec*1000000 +
		pcapPkts[0].hdr.tsUsec;
	start_time = get_time(0);
	if (debug > 0 && !no_tx) {
		for (i = 0; i < MAX_PCAP_PTKS; i++)
		{
			if (!pcapPkts[i].bytes)
				break;
			if (devname)
				printf("Sending %d %d-byte packets to %s%d\n",
				       numPkts, pcapPkts[i].hdr.pktlen,
				       devname, logPort);
			else
				printf("Sending %d %d-byte packets ingress on port %d phys %d to port %d\n",
				       numPkts, pcapPkts[i].hdr.pktlen,
				       ingressPort, ingressPhysPort, logPort);
			if (debug > 1)
			{
				printf("Packet[%d]\n", i);
				printf("tsSec: %d tsUsec: %d delTimeMsec: %lu\n",
				       pcapPkts[i].hdr.tsSec,
				       pcapPkts[i].hdr.tsUsec,
				       (pcapPkts[i].hdr.tsSec*1000000 +
				       pcapPkts[i].hdr.tsUsec - start_time)/1000);
				printf("savedLen : %d pktLen; %d\n",
				       pcapPkts[i].hdr.savedLen,
				       pcapPkts[i].hdr.pktlen);
			}
			printPacket(pcapPkts[i].bytes, pcapPkts[i].hdr.pktlen);
		}
	}

	for (i = 0; i < MAX_PCAP_PTKS; i++)
	{
		if (no_tx)
			break;
		if (!pcapPkts[i].bytes)
			break;
		pkt_time = (pcapPkts[i].hdr.tsSec*1000000 +
			    pcapPkts[i].hdr.tsUsec - pcap_start_time);
		for (j = 0; j < numPkts; j++) {
			tx_time = pkt_time;
			if (pkt_delay_ms >= 0) {
				/* Ignore pcap timestamp if delay is specified */
				tx_time = ((i*numPkts) + j)*pkt_delay_ms*1000;
			}
			while (tx_time > get_time(start_time)) {
				uint64_t usec = tx_time - get_time(start_time);
				delay(usec);
				if (debug > 3)
					printf("%.3f tcap_ts: %.3f delay pkt #%d\n",
						get_time(start_time)/1000000.0,
						tx_time/1000000.0,
						(i*numPkts) + j);

			}
			if (debug > 0) {
				printf("%.3f pcap_ts: %.3f: Sending packet#%d "
				       "len %d\n",
				       get_time(start_time)/1000000.0,
				       pkt_time/1000000.0, (i*numPkts) + j,
				       pcapPkts[i].hdr.pktlen);
			}
			if (debug > 2) {
				printPacket(pcapPkts[i].bytes,
					    pcapPkts[i].hdr.pktlen);
			}
			if (!devname) {
				if (incr_port)
					printf("No support in WM mode for increasing port\n");
				sendPkt(0, ingressPhysPort,
					pcapPkts[i].bytes,
					pcapPkts[i].hdr.pktlen,
					packetMeta, metaLen, logPort);
			} else {
				if (logPort >=0 && logPort < 20) {
					netdev_send(&netdev_eth[logPort],
						    pcapPkts[i].bytes,
						    pcapPkts[i].hdr.pktlen);
				} else {
					printf("No support for logPort %d ingress %d ingressPhys %d\n",
					       logPort, ingressPort,
					       ingressPhysPort);

					return -1;
				}
				logPort = (logPort + incr_port) % 20;
			}
			if (incr_src_mac || incr_dst_mac) {
				pu32 = (uint32_t *)(pcapPkts[i].bytes + 2);
				val32 = ntohl(*pu32);
				val32 += incr_dst_mac;
				*pu32 = htonl(val32);
				pu32 = (uint32_t *)(pcapPkts[i].bytes + 8);
				val32 = ntohl(*pu32);
				val32 += incr_src_mac;
				*pu32 = htonl(val32);
				crc = fmCrc32(pcapPkts[i].bytes,
				      pcapPkts[i].hdr.pktlen - 4);
				for (k = 0; k < 4; k++)
					pcapPkts[i].bytes[
						pcapPkts[i].hdr.pktlen-4+k] =
						(crc >> (8*k)) & 0xff;
			}
		}
	}

	for (i = 0; i < MAX_PCAP_PTKS; i++)
	{
		if (!pcapPkts[i].bytes)
			break;
		free(pcapPkts[i].bytes);
	}

	if (wait)
		printf("Poll for %d seconds for packets to be forwarded\n",
		       wait);

	start = time(NULL);

	if (no_tx && debug == 0) {
		debug = 1; /* Show rx packets */
	}

	delTime = 0;
	while (delTime < wait) {
		/* WM version */
		if (!devname)
		{
			unsigned char *packetRcv = NULL;
			unsigned char sbData[32];
			unsigned int lenRcv = -1;
			for (i = 0 ; i < 32 ; i++) {
				do {
					if(packetRcv != NULL) free(packetRcv);
					recvPacket(i, 0, &packetRcv, &lenRcv,
						   sbData);

					if (packetRcv != NULL) {
						pktCnt++;
						printf("#%d: Received %d-byte packet on port %d phys %d\n",
						       pktCnt, lenRcv,
						       map_phys_to_log(i), i);
						if (debug) {
							printPacket(packetRcv,
								    lenRcv);
							printf("MetaData:\n");
							printPacket(sbData, 32);
						}
					}
				} while (packetRcv != NULL);
			}
		} else {
			/* netdev version */
			for ( i = 0 ; i < 20; i++) {
				netdev_recv(&netdev_eth[i]);
			}
		}
		end = time(NULL);
		delTime = (long long)(end - start);
		sleep(1);
		if (debug > 2)
			printf("Wait Time %lld\n", delTime);
	}
	if (wait)
		printf("%d packets received\n", pktCnt);


	cleanup();
	exit(0);
}
