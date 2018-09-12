/**
 * @file		ies_sbiosf_wm.c
 * @date		May 2016
 * @author		Thanh Nguyen
 * @brief		Sideband IOSF support functions
 *
 * Functions to send and receive data using sideband IOSF.
 *
 * TODO: Insert license text here.
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <poll.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>
#include <pthread.h>

#include "ies_sbiosf.h"
#include "ies_sbiosf_wm.h"

#define READ_TIMEOUT			2000 /* msec */

#define IES_MODEL_MSG_ERROR		10
#define IES_MODEL_MSG_IOSF		11

static int debug;

static pthread_t sbiosf_thread;
static int wm_sock_fd;

static struct sbiosf_txq *txq;
static struct sbiosf_rxq *rxq;

static struct admin_queue_desc *txq_desc;
static struct admin_queue_desc *rxq_desc;

static struct cpk_queue cpk_txq;
static struct cpk_queue cpk_rxq;

void hex_dump(u8 *buf, int nbytes)
{
	int linebytes;
	int  j;
	int  cnt;

	cnt = 0;
	do {
		linebytes = (nbytes > 16) ? 16 : nbytes;

		printf("%02x:", cnt);

		for (j = 0 ; j < linebytes ; j++)
			printf(" %02x", buf[cnt + j]);

		printf("    ");

		for (j = 0 ; j < linebytes ; j++) {
			if ((buf[cnt + j] < 0x20) || (buf[cnt + j] > 0x7e))
				printf(".");
			else
				printf("%c", buf[cnt + j]);
		}

		printf("\n");

		cnt += linebytes;
		nbytes -= linebytes;

	} while (nbytes > 0);
}

/* Parses the WM generated file models.packetServer for hostname and tcp port
 * number. Returns both via host and port.
 */
static int get_host_info(char *host, int host_size, unsigned int *port)
{
	char *filename;
	char buffer[256];
	unsigned int sw;
	int start, cnt;
	char tmp[256];
	FILE *fd;

	filename = getenv("WM_MODEL_SERVER");
	if (!filename) {
		printf("WM_MODEL_SERVER environment is not set\n");
		return -1;
	}

	fd = fopen(filename, "r");
	if (!fd) {
		printf("Unable to open file %s\n", filename);
		return -1;
	}

	fgets(buffer, sizeof(buffer), fd);

	start = 0;
	cnt = 0;
	while ((cnt < strlen(buffer)) && buffer[start + cnt] != ':')
		cnt++;

	bzero(&tmp, sizeof(tmp));
	memcpy(&tmp, buffer, cnt);
	sw = atoi(tmp);

	start = start + cnt + 1;
	cnt = 0;
	while ((start + cnt < strlen(buffer)) && buffer[start + cnt] != ':')
		cnt++;

	bzero(host, host_size);
	memcpy(host, &buffer[start], cnt);

	start = start + cnt + 1;
	cnt = 0;
	while ((start + cnt < strlen(buffer)) && buffer[start + cnt] != ':')
		cnt++;

	bzero(&tmp, sizeof(tmp));
	memcpy(&tmp, &buffer[start], cnt);
	*port = atoi(tmp);

	return 0;
}

/* Creates a tpc client socket that connects to the WM
    TCP server cpu port

 char *hostname     The host name the WM app is running on
 int port           The TCP port the server is listening on
*/
static int connect_to_wm(char *hostname, int port)
{
	struct sockaddr_in serv_addr;
	struct hostent *server;

	wm_sock_fd = socket(AF_INET, SOCK_STREAM, 0);
	if (wm_sock_fd < 0) {
		printf("Error creating socket fd\n");
		return -1;
	}

	server = gethostbyname(hostname);

	if (!server)
		printf("Error unable to find host %s\n", hostname);

	bzero((char *)&serv_addr, sizeof(serv_addr));
	serv_addr.sin_family = AF_INET;
	serv_addr.sin_port = htons(port);
	bcopy( (char *)server->h_addr_list[0],
	       (char *)&serv_addr.sin_addr.s_addr, server->h_length);

	if (connect(wm_sock_fd, (struct sockaddr *)&serv_addr,
		    sizeof(serv_addr)) < 0) {
		printf("Error unable to connect to server\n");
		return -1;
	}

	return 0;
}

/* Read from a socket with timeout
 int socket            The socket descriptor to read from
 unsigned char *data   The data buffer to store received data
 int data_size          The size of data buffer
 int timeout_msec       The timeout if millisecond to return if no response
*/
static int read_data(int socket, unsigned char *data, int data_size,
		     int timeout_msec)
{
	struct pollfd fds[1] = { {0} };
	int           fds_timeout;
	int           err_result;

	if (timeout_msec >= 0)
		fds_timeout = timeout_msec;
	else
		fds_timeout = -1;

	fds[0].fd = socket;
	fds[0].events = POLLIN;
	fds[0].revents = 0;

	do {
		err_result = poll(fds, 1, fds_timeout);
	} while ((err_result == -1) && (errno == EINTR));

	if (fds[0].revents & POLLIN)
		return read(socket, data, data_size);

	if (timeout_msec)
		return -1;

	return 0;
}

void *sbiosf_thread_func(void *arg)
{
	unsigned char buffer[256];
	unsigned int len = 0;
	unsigned int d;
	int n;
	unsigned char type;
	unsigned short msg_type;
	unsigned char opcode;
	u64 tail_sent;
	u64 ptr;
	uint id;
	uint tag;

	printf("Sbiosf thread starting...\n");

	tail_sent = cpk_txq.tail;
	while (1) {
		if (cpk_txq.head != tail_sent) {
			/* Some data need to be send */

			id = (tail_sent - cpk_txq.base) / AQ_DESC_SIZE;
			id = (id + 1) % cpk_txq.n;

			tail_sent = cpk_txq.base + id * AQ_DESC_SIZE;

			if (debug)
				printf("Sending index %d. len %d\n",
				       id, txq_desc[id].data_len);
			len = txq_desc[id].data_len +
				IES_MODEL_MSG_HEADER_SIZE;
			txq[id].msg_length = htonl(len);
			txq[id].version = htons(IES_MODEL_VERSION);
			txq[id].type = htons(IES_MODEL_MSG_IOSF);
			txq[id].sw = htons(0);
			txq[id].port = htons(0);

			/* set the id to verify received message */
			txq[id].data[3] = id & 0x7;
			if (debug)
				hex_dump(txq[id].data, txq_desc[id].data_len);
			write(wm_sock_fd, &txq[id], len);
		}

		n = read_data(wm_sock_fd, buffer, 4, 1000);
		if (n <= 0)
			continue;

		if (n != 4) {
			printf("Expect %d bytes but got %d\n", 4, n);
			continue;
		}

		len = ntohl(*((unsigned int *)&buffer[0]));
		if (len > sizeof(buffer)) {
			printf("Length %d is out of bound\n", len);
			continue;
		}

		n = read_data(wm_sock_fd, &buffer[4], len - 4, READ_TIMEOUT);
		if (n <= 0)
			continue;

		if (n != len - 4) {
			printf("Expect %d bytes but got %d\n", len - 4, n);
			continue;
		}

		msg_type = ntohs(*((unsigned short *)&buffer[6]));
		switch (msg_type) {
		case IES_MODEL_MSG_ERROR:
			printf("%s: %s\n", __func__, buffer + 12);
			continue;
		case IES_MODEL_MSG_IOSF:
			break;
		default:
			printf("Unexpected msg_type -0x%x\n", msg_type);
			continue;
		}

		opcode = buffer[IES_MODEL_MSG_HEADER_SIZE + 2];
		switch (opcode) {
		case 0x7:
			if (debug)
				printf("Got IOSF GLOBAL_INTR  = 0x%llx\n",
				       *(unsigned long int *)
				       &buffer[IES_MODEL_MSG_HEADER_SIZE + 12]);
			id = (cpk_rxq.tail - cpk_rxq.base) / AQ_DESC_SIZE;
			/* Get next entry */
			id  = (id + 1) % cpk_rxq.n;
			if (debug)
				printf("Got opcode 0x%x for id %d. %x %x\n",
				       buffer[IES_MODEL_MSG_HEADER_SIZE + 2],
				       id, cpk_rxq.head, cpk_rxq.tail);
			ptr = cpk_rxq.base + AQ_DESC_SIZE * id;
			if (ptr == cpk_rxq.head) {
				printf("No more space to save ARQ INTR message\n");
				break;
			}
			/* FIXME: might need to verify for correct response */
			memcpy(rxq[id].data,
			       buffer + IES_MODEL_MSG_HEADER_SIZE, len);
			rxq_desc[id].data_len = len;

			/* Now increase the tail to mark data is available */
			cpk_rxq.tail = ptr;
			break;
		default:
			id = (cpk_txq.tail - cpk_txq.base) / AQ_DESC_SIZE;
			/* Get next entry */
			id  = (id + 1) % cpk_txq.n;
			if (debug)
				printf("Got opcode 0x%x for id %d. %x %x\n",
				       buffer[IES_MODEL_MSG_HEADER_SIZE + 2],
				       id, cpk_txq.head, cpk_txq.tail);
			tag = buffer[IES_MODEL_MSG_HEADER_SIZE + 3] & 0x7;
			if (tag != id) {
				printf("Expect response for id %d but got %d\n",
				       id, tag);
			}
			/* FIXME: might need to verify for correct response */
			memcpy(txq[id].data,
			       buffer + IES_MODEL_MSG_HEADER_SIZE, len);
			txq_desc[id].data_len = len;

			/* Now increase the tail to mark data is available */
			cpk_txq.tail = cpk_txq.base + AQ_DESC_SIZE * id;
			break;
		}
	}

	return NULL;
}

/**
 * ies_sbiosf_init_txq() - Initialize sideband IOSF TX Queue.
 *
 * Returns: 0 if successful.
 */
int ies_sbiosf_init_txq(void)
{
	unsigned long ptr;
	int i;

	txq_desc = malloc(IES_SBIOSF_NUM_TXQ * sizeof(struct admin_queue_desc));
	if (!txq_desc)
		return -ENOMEM;

	txq = malloc(IES_SBIOSF_NUM_TXQ * sizeof(struct sbiosf_txq));
	if (!txq) {
		free(txq_desc);
		txq_desc = NULL;
		return -ENOMEM;
	}

	for (i = 0; i < IES_SBIOSF_NUM_TXQ; i++) {
		ptr = (unsigned long)txq[i].data;
		txq_desc[i].addr_hi  = ptr >> 32;
		txq_desc[i].addr_low = ptr;
	}

	cpk_txq.base = (unsigned long)&txq_desc[0];
	cpk_txq.n = IES_SBIOSF_NUM_TXQ;
	cpk_txq.head = cpk_txq.base;
	cpk_txq.tail = cpk_txq.base;
	cpk_txq.desc_size = sizeof(struct cpk_queue);

	return 0;
}

/**
 * ies_sbiosf_init_rxq() - Initialize sideband IOSF RX Queue.
 *
 * Returns: 0 if successful.
 */
int ies_sbiosf_init_rxq(void)
{
	unsigned long ptr;
	int err;
	int i;

	rxq_desc = malloc(IES_SBIOSF_NUM_RXQ * sizeof(struct admin_queue_desc));
	if (!rxq_desc)
		return -ENOMEM;

	rxq = malloc(IES_SBIOSF_NUM_RXQ * sizeof(struct sbiosf_rxq));
	if (!rxq) {
		free(rxq_desc);
		rxq_desc = NULL;
		return -ENOMEM;
	}

	for (i = 0; i < IES_SBIOSF_NUM_RXQ; i++) {
		ptr = (unsigned long)rxq[i].data;
		rxq_desc[i].addr_hi  = ptr >> 32;
		rxq_desc[i].addr_low = ptr;
	}

	cpk_rxq.base = (unsigned long)&rxq_desc[0];
	cpk_rxq.n = IES_SBIOSF_NUM_RXQ;
	cpk_rxq.head = cpk_rxq.base;
	cpk_rxq.tail = cpk_rxq.base;
	cpk_rxq.desc_size = sizeof(struct cpk_queue);

	return 0;
}

/**
 * ies_sbiosf_wm_init() - Initialize sideband IOSF for White Model
 *
 * Returns: 0 if successful.
 */
int ies_sbiosf_wm_init(void)
{
	char host[128];
	int port;
	int err;

	if (sizeof(struct admin_queue_desc) != AQ_DESC_SIZE) {
		printf("Size (%d) of admin_queue_desc is wrong\n",
		       sizeof(struct admin_queue_desc));
		return -1;
	}

	err = ies_sbiosf_init_txq();
	if (err) {
		printf("Unable to initialize TX SBIOSF queue\n");
		return err;
	}

	err = ies_sbiosf_init_rxq();
	if (err) {
		printf("Unable to initialize RX SBIOSF queue\n");
		return err;
	}

	err = get_host_info(host, sizeof(host), &port);
	if (err) {
		printf("Unable to get host info\n");
		return err;
	}

	err = connect_to_wm(host, port);
	if (err) {
		printf("Unable to connect to %s:%d :[%s]",
		       host, port, strerror(err));
		return err;
	}

	err = pthread_create(&sbiosf_thread, NULL, &sbiosf_thread_func, NULL);
	if (err != 0)
		printf("Can't create sbiosf thread :[%s]", strerror(err));

	return err;
}

/**
 * ies_sbiosf_wm_init() - Cleanup sideband IOSF for White Model
 *
 * Returns: 0 if successful.
 */
int ies_sbiosf_wm_cleanup(void)
{
	//Need to terminate thread first
	//close(wm_sock_fd);

	if (txq_desc)
		free(txq_desc);
	if (txq)
		free(txq);

	if (rxq_desc)
		free(rxq_desc);
	if (rxq)
		free(rxq);
}

u64 ies_sbiosf_wm_atq_read_base(void)
{
	return cpk_txq.base;
}

u64 ies_sbiosf_wm_atq_read_n(void)
{
	return cpk_txq.n;
}

u64 ies_sbiosf_wm_atq_read_head(void)
{
	return cpk_txq.head;
}

void ies_sbiosf_wm_atq_set_head(u64 head)
{
	cpk_txq.head = head;
}

u64 ies_sbiosf_wm_atq_read_tail(void)
{
	return cpk_txq.tail;
}

u64 ies_sbiosf_wm_arq_read_base(void)
{
	return cpk_rxq.base;
}

u64 ies_sbiosf_wm_arq_read_n(void)
{
	return cpk_rxq.n;
}

u64 ies_sbiosf_wm_arq_read_head(void)
{
	return cpk_rxq.head;
}

u64 ies_sbiosf_wm_arq_read_tail(void)
{
	return cpk_rxq.tail;
}

void ies_sbiosf_wm_arq_set_head(u64 head)
{
	cpk_rxq.head = head;
}
