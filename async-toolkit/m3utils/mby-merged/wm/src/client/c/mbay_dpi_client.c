/*****************************************************************************
 * @file	mbay_dpi_client.c
 * @brief	Minimal client for reg access through model_server socket
 *
 * INTEL CONFIDENTIAL
 * Copyright 2018 Intel Corporation.  All Rights Reserved.
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

#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <poll.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/tcp.h>
#include <pthread.h>
#include <fcntl.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <libgen.h>
#include <sys/param.h>
#ifndef NO_SV
#include "svdpi_src.h"
#endif

#include "mbay_dpi_client.h"
#include "client_types.h"

/* TODO replace log functions placeholders with improved macros */
#define LOG_ERROR    printf
#define LOG_WARNING  printf
#define LOG_INFO     printf
#define LOG_DEBUG(x, ...)
//#define LOG_DEBUG    printf
#define LOG_HEX_DUMP(a,b,c)
//#define LOG_HEX_DUMP hex_dump

/* FD of the socket used to send commands to the model_server.
 * For example, it is used to send IOSF messages or inject traffic.
 */
static int wm_server_fd;

/* FD of the socket used to recevive connections from model_server.
 * For example, it is used to accept connections for egress ports.
 */
static int wm_client_fd;
static int wm_client_port;

/* FD used to get egress traffic from WM ports */
static int wm_egress_fd;

/* Temporary file used for models.packetServer */
#define TMP_FILE_TEMPLATE   "/tmp/models.packetServer.XXXXXX"
#define TMP_FILE_LEN		100
static char server_tmpfile[TMP_FILE_LEN] = "";

/* Default Java path only used when ToolConfig.pl fails */
#define DEFAULT_JAVA "/usr/intel/pkgs/java/1.8.0.151/bin/java\n"

/* Path of the m3/scala server relative to $MODEL_ROOT */
#define M3_SERVER_PATH "wm/src/main/m3/model_server/AMD64_LINUX/modelserver"
#define SCALA_SERVER_PATH "target/GenRTL/wm/mbay_wm.jar"

/* Static functions defined at the end of the file */
static int iosf_send_receive(uint8_t *tx_msg, uint32_t tx_len,
							 uint8_t *rx_msg, uint32_t *rx_len);
static int wm_send(int fd, const uint8_t *msg, uint32_t len, uint16_t type,
				   uint16_t port);
static int wm_receive(int fd, uint8_t *msg, uint32_t *len, uint16_t *type,
					  uint16_t *port);
static void hex_dump(const uint8_t *bytes, int nbytes, char show_ascii);
static int create_client_socket(int *fd, int *port);
static int connect_server(const char *addr_str, const char *port, int *server_fd);
static int connect_egress(int phys_port, int server_fd, int client_fd,
						  int client_port, int *egress_fd);
static int read_host_info(FILE *fd, char *host, char *port);
static int wm_read_data(int socket, uint8_t *data, uint32_t data_len,
					    int timeout_msec);

/**
 * wm_server_start() - Start the model server.
 *
 * Fork a new process and run the model server. The infopath and infofile
 * arguments are set to a temporary file.
 * It then waits until the server comes up and connects to it.
 *
 * @param_in	type is a string with the type of server: "scala" or "m3"
 *
 * @retval		OK if successful
 */
int wm_server_start(char *type)
{
	char cmd[500], jar_path[500];
	char *m3_exec_args[] = {cmd, "-n", "-m", "mby", /* Select the MBY model */
							"-ip", NULL, /* Path of models.packetServer */
							"-if", NULL, /* Actual name of the file */
							NULL};
	char *scala_exec_args[] = {cmd, "-jar", jar_path,
								"--ip", NULL, /* Path of models.packetServer */
								"--if", NULL, /* Actual name of the file */
								NULL};
	const int max_retries = 30;
	char server_if[TMP_FILE_LEN];
	char server_ip[TMP_FILE_LEN];
	char *model_root;
	int use_m3 = 0;
	pid_t pid;
	int i = 0;
	int file;
	FILE *fd;
	int err;

	/* Mandatory env variable set by NHDK */
	model_root = getenv("MODEL_ROOT");
	if (!model_root) {
		LOG_ERROR("Env variable MODEL_ROOT is not set\n");
		return ERR_INVALID_ARG;
	}

	if (!type) {
		LOG_ERROR("Server path string cannot be NULL\n");
		return ERR_INVALID_ARG;
	}

	/* TODO remove support for m3 code to cleanup this messy code */
	if (!strcasecmp(type, "scala")) {
		/* For scala I need to retrieve the java exec path */
		fd = popen("ToolConfig.pl get_tool_exec java", "r");
		if (!fd) {
			LOG_ERROR("Cannot get path of Java binaries\n");
			return ERR_RUNTIME;
		}
		if (!fgets(cmd, sizeof(cmd), fd)) {
			LOG_INFO("Workaround: try default java %s\n", DEFAULT_JAVA);
			strcpy(cmd, DEFAULT_JAVA);
		}
		pclose(fd);
		cmd[strlen(cmd) - 1] = '\0';
		sprintf(jar_path, "%s/%s", model_root, SCALA_SERVER_PATH);
	} else if (!strcasecmp(type, "m3")) {
		use_m3 = 1;
		sprintf(cmd, "%s/%s", model_root, M3_SERVER_PATH);
	} else {
		LOG_ERROR("Invalid type of server: %s\n", type);
		return ERR_INVALID_ARG;
	}

	/* Generate a temporary file name and extract directory name (infopath)
	 * and file name (infoname). Note that the functions are destructive so
	 * we need to make copies of the strings */
	strcpy(server_tmpfile, TMP_FILE_TEMPLATE);
	file = mkstemp(server_tmpfile);
	close(file);
	LOG_INFO("Using temporary file: %s\n", server_tmpfile);

	pid = fork();
	if (pid == 0) {
		/* Child process: start m3/scala model_server */
		strcpy(server_if, server_tmpfile);
		strcpy(server_ip, server_tmpfile);
		if (use_m3) {
			m3_exec_args[5] = dirname(server_ip);
			m3_exec_args[7] = basename(server_if);
			execvp(m3_exec_args[0], m3_exec_args);
		} else {
			scala_exec_args[4] = dirname(server_ip);
			scala_exec_args[6] = basename(server_if);
			execvp(scala_exec_args[0], scala_exec_args);
		}
		LOG_ERROR("Could not start %s server: %s - error: %s\n",
				use_m3 ? "m3" : "Scala", cmd, strerror(errno));
		exit(1);
	} else if (pid > 0) {
		/* Parent process: wait 5sec then if child is alive try to connect */
		sleep(5);
		waitpid(pid, &err, WNOHANG);
		if (WIFEXITED(err)) {
			LOG_ERROR("Server failed to start: pid %d has exited\n", pid);
			return ERR_RUNTIME;
		}
		do {
			sleep(1);
			err = wm_connect(server_tmpfile);
			++i;
		} while (err == ERR_TIMEOUT && i < max_retries);
		if (err) {
			LOG_ERROR("Could not connect to the model_server\n");
			return err;
		}
	} else {
		LOG_ERROR("Could not start model_server: fork() failed\n");
		return ERR_RUNTIME;
	}

	return OK;
}

/**
 * wm_server_stop() - Send shutdown request to model_server.
 *
 * Send a message to request the shutdown of the server. If a temp file
 * has been created to store the infopath, the file is deleted.
 * It also disconnects from the server by closing the socket.
 *
 * @retval		OK if successful
 */
int wm_server_stop(void)
{
	unsigned char empty_msg = 0;
	int err;

	err = wm_send(wm_server_fd, &empty_msg, 0, MODEL_MSG_COMMAND_QUIT, 0);
	if (err)
		LOG_ERROR("Error while sending shutdown request to server: %d\n", err);

	if (strcmp(server_tmpfile, "")) {
		err = remove(server_tmpfile);
		if (err)
			LOG_ERROR("Could not delete temp file %s\n", server_tmpfile);
	}

	err = wm_disconnect();
	return err;
}

/**
 * wm_connect() - Connect to WM server.
 *
 * @param_in	server_file path of the file created when the model_server is
 * 				started. If NULL, the env variable "SBIOSF_SERVER" is used.
 *
 * @retval	OK if successful.
 */
int wm_connect(const char *server_file)
{
	const char *filename;
	char serv_addr[256];
	char serv_port[256];
	struct stat sb;
	FILE *fd;
	int err;
	int i;

	if (server_file) {
		filename = server_file;
	} else {
		filename = getenv("SBIOSF_SERVER");
		if (!filename) {
			LOG_ERROR("server_file is NULL and SBIOSF_SERVER env var is not set\n");
			return ERR_INVALID_ARG;
		}
	}

	if ((stat(filename, &sb) == 0) && !S_ISREG(sb.st_mode)) {
		LOG_ERROR("Server path is not a valid file %s\n", filename);
		return ERR_INVALID_ARG;
	}

	fd = fopen(filename, "r");
	if (!fd) {
		LOG_ERROR("Unable to open file %s\n", filename);
		return ERR_INVALID_ARG;
	}

	bzero(serv_addr, sizeof(serv_addr));
	bzero(serv_port, sizeof(serv_port));
	err = read_host_info(fd, serv_addr, serv_port);
	if (err) {
		LOG_ERROR("Unable to get server connection info\n");
		return err;
	}

	err = connect_server(serv_addr, serv_port, &wm_server_fd);
	if (err)
		return err;

	err = create_client_socket(&wm_client_fd, &wm_client_port);
	if (err)
		return err;

	wm_egress_fd = -1;

	for (i = 0; i < NUM_PORTS; ++i) {
		err = connect_egress(i, wm_server_fd, wm_client_fd, wm_client_port,
							 &wm_egress_fd);
		if (err)
			return err;
	}

	return OK;
}

/**
 * wm_disconnect() - Disconnect from WM server.
 *
 * @retval	OK if successful.
 */
int wm_disconnect(void)
{
	return close(wm_server_fd);
}

/**
 * wm_reg_read() - Send register read request to model_server.
 *
 * @param_in	addr address of the register.
 * @param_out	val pointer to caller-allocated memory to store the result.
 * @retval		OK if successful
 */
int wm_reg_read(const uint32_t addr, uint64_t *val)
{
	uint32_t iosf_msg[512];
	uint32_t iosf_len;
	unsigned char be;
	int err;

	if (!val)
		return ERR_INVALID_ARG;

	/* See SB-IOSF specs for details on the message format */
	be = addr & 0x7 ? 0xf : 0xff;
	iosf_msg[0] = 0x40000001;
	iosf_msg[1] = 0x00000000;
	iosf_msg[2] = ((addr & 0xffff) << 16) | be;
	iosf_msg[3] = addr >> 16;
	iosf_len = 16;

	err = iosf_send_receive((uint8_t *)iosf_msg, iosf_len,
							(uint8_t *)iosf_msg, &iosf_len);
	if (err) {
		LOG_ERROR("Error with iosf message tx/rx: %d\n", err);
		return err;
	}

    *val = iosf_msg[3];
	*val = (*val << 32) | iosf_msg[2];
    return OK;
}


/**
 * wm_reg_write() - Send register write request to model server.
 *
 * @param_in	addr address of the register.
 * @param_in	val is the value to be written.
 * @retval		OK if successful
 */
int wm_reg_write(const uint32_t addr, const uint64_t val)
{
	uint32_t iosf_msg[512];
	uint32_t iosf_len;
	int err;

	/* See SB-IOSF specs for details on the message format
	 * FIXME that we are using bulk write here since the m3 model_server
	 * has some issue with single register operations */
	iosf_msg[0] = 0x40110001;
	iosf_msg[1] = 0x00000000;
	iosf_msg[2] = ((addr & 0xffff) << 16) | 2;
	iosf_msg[3] = addr >> 16;
	iosf_msg[4] = val & 0xffffffff;
	iosf_msg[5] = val >> 32;
	iosf_len = 24;

	err = iosf_send_receive((uint8_t *)iosf_msg, iosf_len,
							(uint8_t *)iosf_msg, &iosf_len);

	if (err)
		LOG_ERROR("Error with iosf message tx/rx: %d\n", err);

	return err;
}

/* wm_pkt_push() - Send a frame to the WM
 *
 * The frame will be sent as it is without any processing.
 * Default metadata will be included for compatibility with HLP WM.
 * TODO check if this is the desired behavior.
 *
 * @param_in	port is the phys port where the frame will be injected
 * @param_in	data pointer to the frame data
 * @param_in	len is the lenght of the frame
 * @retval		OK if successful
 */
int wm_pkt_push(uint16_t port, const uint8_t *data, uint32_t len)
{
	uint8_t pkt_msg[MAX_MSG_LEN];
	int pkt_len;
	int off = 0;
	int err;

	if (!data) {
		LOG_ERROR("Input buffer data is NULL\n");
		return ERR_INVALID_ARG;
	}

	if (len > MAX_MSG_LEN) {
		LOG_ERROR("Input length %d exceeds maximum: %d\n", len, MAX_MSG_LEN);
		return ERR_INVALID_ARG;
	}

	LOG_DEBUG("Pushing packet len=%d\n", len);
	LOG_HEX_DUMP(data, len, 0);
	/* First TLV is the meta-data. For now I will assume this is a frame
	 * entering HLP from RMN. TODO this will need to be removed/updated
	 */
	pkt_msg[off] = WM_DATA_TYPE_META;
	off += WM_DATA_TYPE_SIZE;
	*((uint32_t *)&pkt_msg[off]) = htonl(RIMMON_META_SIZE);
	off += WM_DATA_LENGTH_SIZE;
	pkt_msg[off++] = 0x18;
	pkt_msg[off++] = 0xa1;
	pkt_msg[off++] = 0xb2;
	pkt_msg[off++] = 0xc3;
	pkt_msg[off++] = 0xd4;
	pkt_msg[off++] = 0xe5;
	pkt_msg[off++] = 0xf6;
	pkt_msg[off++] = 0x07;

	/* Second TLV is the actual packet payload */
	pkt_msg[off] = WM_DATA_TYPE_PACKET;
	off += WM_DATA_TYPE_SIZE;
	*((uint32_t *)&pkt_msg[off]) = htonl(len);
	off += WM_DATA_LENGTH_SIZE;
	memcpy(pkt_msg + off, data, len);
	pkt_len = off + len;

	err = wm_send(wm_server_fd, pkt_msg, pkt_len, MODEL_MSG_PACKET, port);
	if (err) {
		LOG_ERROR("Could not send data to WM: %d\n", err);
	}

	return err;
}

/* wm_pkt_get() - Receive a frame from the WM
 *
 * @param_out	port is the phys port where the frame egressed
 * @param_out	data pointer to caller-allocated storage where the frame will be
 * 				saved. It must be at least MAX_MSG_LEN bytes.
 * @param_out	len is the lenght of the frame
 * @retval		ERR_NO_DATA if there are no egress frames on any of the ports.
 * @retval		OK if successful
 */
int wm_pkt_get(uint16_t *port, uint8_t *data, uint32_t *len)
{
	uint8_t msg[MAX_MSG_LEN];
	/*struct pollfd pfd;*/
	uint32_t msg_len;
	uint32_t pkt_len;
	uint16_t type;
	int err;

	if (wm_egress_fd == -1) {
		LOG_ERROR("Socket not initialized, call wm_connect()\n");
		return ERR_RUNTIME;
	}

	if (!data || !port || !len) {
		LOG_ERROR("Input pointer to data/port/len is NULL\n");
		return ERR_INVALID_ARG;
	}

	err = wm_receive(wm_egress_fd, msg, &msg_len, &type, port);
	if (err)
		return err;

	if (type == MODEL_MSG_PACKET_EOT) {
		LOG_DEBUG("Received EOT packet\n");
		*len = 0;
		*port = -1;
		return OK;
	}

	if (type != MODEL_MSG_PACKET) {
		LOG_ERROR("Received unexpected message types %d\n", type);
		return ERR_RUNTIME;
	}

	/* TODO do we need to support PACKET_META? */
	if (msg[0] != WM_DATA_TYPE_PACKET) {
		LOG_ERROR("Unsupported data type %d\n", msg[0]);
		return ERR_RUNTIME;
	}

	pkt_len = ntohl(*(uint32_t *)&msg[WM_DATA_TYPE_SIZE]);

	if (pkt_len > msg_len - WM_DATA_TLV_SIZE) {
		LOG_ERROR("Packet len is %d but I only received %d byte from server\n",
				pkt_len, msg_len - WM_DATA_TLV_SIZE);
		*len = 0;
		return ERR_RUNTIME;
	}
	*len = pkt_len;
	memcpy(data, msg + WM_DATA_TLV_SIZE, *len);

	/* TODO what to do with other TLVs in the message */

	return OK;
}

#ifndef NO_SV

/* wm_svpkt_push() - SV wrapper for wm_pkt_push()
 *
 * Required to convert the memory buffer from SV to C format.
 */
int wm_svpkt_push(int port, const svOpenArrayHandle sv_data, uint32_t len)
{
	uint8_t *data;

    data = (uint8_t*) svGetArrayPtr(sv_data);
	if (!data) {
		LOG_ERROR("Cannot convert data buffer from SV to C\n");
		return ERR_INVALID_ARG;
	}

	return wm_pkt_push(port, data, len);
}

/* wm_svpkt_get() - SV wrapper for wm_pkt_get()
 *
 * Required to convert the memory buffer from SV to C format.
 */
int wm_svpkt_get(int *port, svOpenArrayHandle sv_data, uint32_t *len)
{
	uint8_t *data;

    data = (uint8_t*) svGetArrayPtr(sv_data);
	if (!data) {
		LOG_ERROR("Cannot convert data buffer from SV to C\n");
		return ERR_INVALID_ARG;
	}

	return wm_pkt_get(port, data, len);
}
#endif /* #ifndef NO_SV */

/*****************************************************************************
 *************************** Auxiliary functions *****************************
 ****************************************************************************/

/* Timeout for socket read operations in ms */
#define READ_TIMEOUT 5000

/* TODO what is this? */
#define NONPOSTED_PORT			0
#define POSTED_PORT				1

/**
 * iosf_send_receive() - Send IOSF message and wait for response.
 *
 * @param_in	tx_msg caller-allocated buffer with the message to send
 * @param_in	tx_len length of the message to send.
 * @param_out	rx_msg caller-allocated buffer where the response will be stored
 * @param_out	rx_len caller-allocated used to store the length of the reposnse
 * @retval		OK if successful
 */
static int iosf_send_receive(uint8_t *tx_msg, uint32_t tx_len,
							 uint8_t *rx_msg, uint32_t *rx_len)
{
	unsigned char rsp;
    uint16_t type;
    uint16_t port;
	int err;

	err = wm_send(wm_server_fd, tx_msg, tx_len, MODEL_MSG_IOSF, NONPOSTED_PORT);
	if (err) {
		LOG_ERROR("Could not send data to WM: %d\n", err);
		return err;
	}

	err = wm_receive(wm_server_fd, rx_msg, rx_len, &type, &port);
	if (err) {
		LOG_ERROR("Did not receive data from WM: %d\n", err);
		return err;
	}

	/* TODO there might be problems if this library is used in
	 * multi-thread applications
	 */
	if (type != MODEL_MSG_IOSF) {
		LOG_ERROR("Unexpected response message type: %d\n", type);
		return ERR_INVALID_RESPONSE;
	}

	/* Check the rsp field to validate the content of the response */
	rsp = (((uint8_t *)rx_msg)[3] >> 3) & 0x3;
	if (rsp) {
		LOG_ERROR("Register write operation failed - rsp=%d", rsp);
		return ERR_INVALID_RESPONSE;
	}

	return OK;
}

/**
 * wm_receive() - Receive message from model_server socket interface.
 *
 * @param_in	fd of the socket used to receive the data
 * @param_out	msg caller-allocated buffer where the message will be placed.
 * @param_out	len caller-allocated storage where the length will be placed.
 * @param_out	type caller-allocated storage where the type will be placed.
 * @param_out	port caller-allocated storage where teh port will be placed.
 * @retval		OK if successful
 */
static int wm_receive(int fd, uint8_t *msg, uint32_t *len, uint16_t *type,
					  uint16_t *port)
{
	struct wm_msg wm_msg;
	uint32_t wm_len;
	int err;

	bzero(&wm_msg, sizeof(wm_msg));
	/* Read the first 4 bytes to see the length of the message */
	err = wm_read_data(fd, (uint8_t *)&wm_msg, sizeof(wm_len), READ_TIMEOUT);
	if (err) {
		LOG_ERROR("Could not receive message preamble from WM\n");
		return err;
	}

	wm_len = ntohl(wm_msg.msg_length);
	if (wm_len > sizeof(wm_msg)) {
		LOG_ERROR("Message length %d exceeds max %ld\n", wm_len, sizeof(wm_msg));
		return ERR_INVALID_RESPONSE;
	}

	/* Receive the remaining bytes */
	err = wm_read_data(fd, (uint8_t *)&wm_msg.version,
			wm_len - sizeof(wm_len), READ_TIMEOUT);
	if (err) {
		LOG_ERROR("Could not receive message contents from WM\n");
		return err;
	}

	*len = wm_len - offsetof(struct wm_msg, data);
	*type = ntohs(wm_msg.type);
	*port = ntohs(wm_msg.port);

	LOG_DEBUG("Received message type %d - port %d - len %d\n", *type, *port, wm_len);
	LOG_HEX_DUMP((uint8_t *)&wm_msg, wm_len, 0);

	if (ntohs(wm_msg.version) != MODEL_VERSION) {
		LOG_WARNING("Model version mismatch: received %d - expected %d\n",
				ntohs(wm_msg.version), MODEL_VERSION);
	}

	switch (*type) {
	case MODEL_MSG_ERROR:
		LOG_ERROR("Received error msg: %s", wm_msg.data);
		return ERR_INVALID_RESPONSE;
	case MODEL_MSG_IOSF:
	case MODEL_MSG_PACKET:
	case MODEL_MSG_PACKET_EOT:
		break;
	default:
		LOG_ERROR("Unexpected msg_type 0x%04x\n", *type);
		return ERR_INVALID_RESPONSE;
	}

	memcpy(msg, wm_msg.data, *len);
	LOG_DEBUG("Message data  - len %d\n", *len);
	LOG_HEX_DUMP(msg, *len, 0);
	return OK;
}

/**
 * wm_send() - Send generic message to model_server socket interface.
 *
 * @param_in	fd of the socket used to send the data
 * @param_in	msg pointer to the content of the message.
 * @param_in	len the length of the message.
 * @param_in	type the type of the message (e.g. SB-IOSF).
 * @param_in	port the ingress port used when sending traffic to model.
 * @retval		OK if successful
 */
static int wm_send(int fd, const uint8_t *msg, uint32_t len, uint16_t type,
				   uint16_t port)
{
	struct wm_msg wm_msg;
	uint32_t wm_len;
	uint32_t wr_len;

	wm_msg.version = htons(MODEL_VERSION);
	wm_msg.type = htons(type);
	wm_msg.sw = htons(0);
	wm_msg.port = htons(port);

	//memcpy_s(msg.data, sizeof(msg.data), pmsg, msg_len);
	memcpy(wm_msg.data, msg, len);

	/* Type specific checks */
	if (type == MODEL_MSG_IOSF && len > IOSF_MSG_MAX_LEN) {
		LOG_ERROR("Message len %d is too large", len);
		return ERR_INVALID_ARG;
	}

	wm_len = len + MODEL_MSG_HEADER_SIZE;
	wm_msg.msg_length = htonl(wm_len);

	// LOG_HEX_DUMP((uint8_t *)&wm_msg, wm_len, 0);

	wr_len = write(fd, &wm_msg, wm_len);
	if (wm_len != wr_len) {
		LOG_ERROR("ERROR: write %d to socket. Only %d written",
				wm_len, wr_len);
		return ERR_NETWORK;
	}

	return OK;
}

/**
 * hex_dump() - Dumps buffer in hex output.
 *
 * @param_in	bytes is the buffer to dump.
 * @param_in	nbytes is the size of the buffer.
 * @param_in	show_ascii whether to append ascii format.
 */
static void hex_dump(const uint8_t *bytes, int nbytes, char show_ascii)
{
	char line[128];
	int linebytes;
	int cnt;
	int j;
	int l;

	cnt = 0;
	do {
		linebytes = (nbytes > 16) ? 16 : nbytes;

		sprintf(line, "%02x:", cnt);
		l = strlen(line);

		for (j = 0; j < linebytes; j++) {
			sprintf(line + l, " %02x", bytes[cnt + j]);
			l = strlen(line);
		}

		if (show_ascii) {
			sprintf(line + l, "    ");
			l = strlen(line);

			for (j = 0; j < linebytes; j++) {
				if ((bytes[cnt + j] < 0x20) ||
				    (bytes[cnt + j] > 0x7e))
					sprintf(line + l, ".");
				else
					sprintf(line + l, "%c", bytes[cnt + j]);
				l = strlen(line);
			}
		}
		printf("%s\n", line);
		cnt += linebytes;
		nbytes -= linebytes;

	} while (nbytes > 0);
}

/**
 * connect_main() - Connect to the WM server socket
 *
 * The fd of the socket is saved in a global variable and it will
 * be reused later on to send all the messages.
 *
 * @param_in	addr_str is the string with the server address.
 * @param_in	port_str is the port where the server is listening.
 * @param_out	server_fd is the caller-alloc storage where the server fd will
 * 				be placed
 */
static int connect_server(const char *addr_str, const char *port_str, int *server_fd)
{
	struct addrinfo *result, *rp;
	struct addrinfo hints;
	int on = 1;
	int err;

	memset(&hints, 0, sizeof(struct addrinfo));
	hints.ai_family = AF_INET;
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_flags = 0;
	hints.ai_protocol = 0;          /* Any protocol */

	LOG_DEBUG("Connecting to model_server at %s:%s\n", addr_str, port_str);
	err = getaddrinfo(addr_str, port_str, &hints, &result);
	if (err != 0) {
		LOG_ERROR("Error in getaddrinfo: %s\n", gai_strerror(err));
		return ERR_NETWORK;
	}

	for (rp = result; rp != NULL; rp = rp->ai_next) {
		*server_fd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
		if (*server_fd == -1) {
			LOG_ERROR("Error creating socket fd: %s\n", strerror(errno));
			return ERR_NETWORK;
		}

		if (connect(*server_fd, rp->ai_addr, rp->ai_addrlen) != -1)
			break; /* Success */

		LOG_ERROR("Unable to connect to server at %s:%s: %s\n", addr_str,
				port_str, strerror(errno));
		close(*server_fd);
	}
        freeaddrinfo(result);
	LOG_DEBUG("Connected to model_server at %s:%s\n", addr_str, port_str);
	setsockopt(*server_fd, IPPROTO_TCP, TCP_NODELAY, (void *)&on, sizeof(on));

	return OK;
}

/* connect_egress() - Establish connection to receive egress frames
 *
 * @param_in	phys_port the switch physical port number
 * @param_in	server_fd is the server socket fd where the request will be sent
 * @param_in	client_fd is the client socket fd that will get the connection
 * @param_in	client_port is the port number of where the client is listening
 * @param_out	egress_fd is the caller-alloc storage where the egress socket fd
 * 				will be placed
 */
static int connect_egress(int phys_port, int server_fd, int client_fd,
						  int client_port, int *egress_fd)
{
    char hostname[MAXHOSTNAMELEN];
    char buffer[MAXHOSTNAMELEN + 2];
    uint32_t len;
	int err;

    err = gethostname(hostname, MAXHOSTNAMELEN);
    if (err)
		LOG_ERROR("Problem getting hostname: %s", strerror(errno));

    bzero(buffer, MAXHOSTNAMELEN + 2);

    /* Message format is: 2B tcp client port, 510B hostname */
    *((uint16_t *)buffer) = htons(client_port);
    strcpy(buffer + 2, hostname);
    len = 2 + strlen(hostname);

	wm_send(server_fd, (uint8_t *)buffer, len, MODEL_MSG_SET_EGRESS_INFO,
			phys_port);

	/* If socket hasn't been init before, wait for connection from server */
	if (*egress_fd == -1)
		*egress_fd = accept(client_fd, NULL, NULL);

	if(*egress_fd  < 0) {
		LOG_ERROR("Error accepting connection for egress port %d: %s\n",
				phys_port, strerror(errno));
		return ERR_NETWORK;
	}
	return OK;
}

/* create_client_socket() - Create a socket to accept connection from WM
 *
 * This socket is mainly used to setup the egress ports
 *
 * @param_out	fd is a pointer to caller-allocated storage in which the fd
 * 				of the socket will be placed.
 * @param_out	port is a pointer to caller-allocated storage in which the port
 * 				number will be placed.
 */
static int create_client_socket(int *fd, int *port)
{
	struct sockaddr_in addr;
	socklen_t addr_len;
	int err;

	if (!fd || !port)
		return ERR_INVALID_ARG;

	*fd = socket(AF_INET, SOCK_STREAM, 0);
	if (*fd < 0) {
		printf("Error creating client socket: %s\n", strerror(errno));
		return ERR_NETWORK;
	}

	bzero(&addr, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_addr.s_addr = htonl(INADDR_ANY);
	addr.sin_port = htons(0);

	err = bind(*fd, (struct sockaddr *)&addr, sizeof(addr));
	if(err == -1) {
		printf("Error binding to port socket server: %s\n", strerror(errno));
		return ERR_NETWORK;
	}

	err = listen(*fd, NUM_PORTS);
	if(err == -1) {
		printf("Error listening on port socket server: %s\n", strerror(errno));
		return ERR_NETWORK;
	}

	addr_len = sizeof(addr);
	err = getsockname(*fd, (struct sockaddr *)&addr, &addr_len);
	if(err == -1) {
		printf("Error getting port socket server name: %s\n", strerror(errno));
		return ERR_NETWORK;
	}

	*port = ntohs(addr.sin_port);
	LOG_DEBUG("Client socket created - listen on port %d\n", *port);
	return OK;
}

/**
 * read_host_info() - Read host info from model_server file
 *
 * The expected content is in the format: "0:localhost:57548"
 *
 * @param_in	fd is the file pointer to read from.
 * @param_out	host is buffer to store host name.
 * @param_out	port is buffer to store host port number.
 * @retval	IES_OK if successful.
 */
static int read_host_info(FILE *fd, char *host, char *port)
{
	unsigned int start, cnt;
	char buffer[256];

	if (!fd || !host || !port)
		return ERR_INVALID_ARG;

	if (!fgets(buffer, sizeof(buffer), fd)) {
		LOG_ERROR("Could not read host info from file\n");
		return ERR_RUNTIME;
	}

	start = 0;
	cnt = 0;
	while ((cnt < strlen(buffer)) && buffer[start + cnt] != ':')
		cnt++;

	start = start + cnt + 1;
	cnt = 0;
	while ((start + cnt < strlen(buffer)) &&
			buffer[start + cnt] != ':')
		cnt++;
	if (start + cnt >= sizeof(buffer) || cnt == 0)
		return ERR_RUNTIME;

	memcpy(host, &buffer[start], cnt);
	printf("cnt = %d - host = %s\n", cnt, host);

	start = start + cnt + 1;
	cnt = 0;
	while ((start + cnt < strlen(buffer)) &&
			buffer[start + cnt] != ':')
		cnt++;
	if (start + cnt >= sizeof(buffer) || cnt == 0)
		return ERR_RUNTIME;

	/* Make sure to not copy the \n */
	if (buffer[start + cnt - 1] == '\n')
		cnt--;

	memcpy(port, &buffer[start], cnt);
	fclose(fd);
	return OK;
}

/**
 * wm_read_data() - Reads from a socket with timeout.
 *
 * @param_in	socket is the descriptor to read from.
 * @param_out	data is the buffer to store received data.
 * @param_in	data_size is the size of data buffer.
 * @param_in	timeout_msec is the timeout to return if no response.
 * @return		OK if successful
 */
static int wm_read_data(int socket, uint8_t *data, uint32_t data_len,
					    int timeout_msec)
{
	struct pollfd fds[1] = { {0} };
	int remaining_len = data_len;
	const int delay_ms = 100;
	int num_retries = 10;
	int fds_timeout;
	int err_result;
	int n;

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


	if (!(fds[0].revents & POLLIN)) {
		if (timeout_msec) {
			LOG_ERROR("Connection timeout while receiving data\n");
			return ERR_TIMEOUT;
		} else {

			LOG_ERROR("Lost connection to WM\n");
			return ERR_NO_RESOURCE;
		}
	}

	while (num_retries) {
		n = read(socket, data, data_len);
		if (n == -1) {
			LOG_ERROR("Error while reading data from socket %s\n",
					strerror(errno));
			return ERR_INVALID_RESPONSE;
		}
		remaining_len -= n;
		if (remaining_len <= 0)
			break;
		usleep(1000 * delay_ms);
		data += n;
		--num_retries;
	}

	if (remaining_len > 0) {
		LOG_ERROR("Expected %d bytes but got %d\n", data_len,
				data_len - remaining_len);
		return ERR_INVALID_RESPONSE;
	}

	return OK;
}

