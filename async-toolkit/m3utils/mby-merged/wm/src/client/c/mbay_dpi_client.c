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
#include <libgen.h>

#include "mbay_dpi_client.h"
#include "client_types.h"

/* TODO replace log functions placeholders with improved macros */
#define LOG_ERROR   printf
#define LOG_WARNING printf
#define LOG_INFO    printf
#define LOG_DEBUG   printf
#define LOG_DUMP    printf

/* FD of the socket used to talk with model_server */
static int wm_sock_fd;

/* Temporary file used for models.packetServer */
static char server_tmpfile[L_tmpnam] = "";

/* Static functions defined at the end of the file */
static int iosf_send_receive(uint8_t *tx_msg, uint32_t tx_len,
							 uint8_t *rx_msg, uint32_t *rx_len);
static int wm_send(uint8_t *msg, uint32_t len, uint16_t type);
static int wm_receive(uint8_t *msg, uint32_t *len, uint16_t *type);
static void hex_dump(uint8_t *bytes, int nbytes, char show_ascii);
static int read_host_info(FILE *fd, char *host, int host_size, int *port);
static int wm_read_data(int socket, uint8_t *data, uint32_t data_len,
					    int timeout_msec);

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
	struct sockaddr_in serv_addr;
	char server_addr[128];
	const char *filename;
	int port = 0;
	int on = 1;
	FILE *fd;

	if (server_file) {
		filename = server_file;
	}
	else {
		filename = getenv("SBIOSF_SERVER");
		if (!filename) {
			LOG_ERROR("server_file is NULL and SBIOSF_SERVER env var is not set\n");
			return ERR_INVALID_ARG;
		}
	}

	fd = fopen(filename, "r");
	if (!fd) {
		LOG_ERROR("Unable to open file %s\n", filename);
		return ERR_INVALID_ARG;
	}

	if (read_host_info(fd, server_addr, sizeof(server_addr), &port)) {
		LOG_ERROR("Unable to get server connection info\n");
		return ERR_INVALID_ARG;
	}

	wm_sock_fd = socket(AF_INET, SOCK_STREAM, 0);
	if (wm_sock_fd < 0) {
		LOG_ERROR("Error creating socket fd\n");
		return ERR_INVALID_ARG;
	}

	bzero((char *)&serv_addr, sizeof(serv_addr));

	/* Use ip address instead of hosthame due to klocwork */
	if (!strcmp("localhost", server_addr))
		serv_addr.sin_addr.s_addr = inet_addr("127.0.0.1");
	else
		serv_addr.sin_addr.s_addr = inet_addr(server_addr);

	if (serv_addr.sin_addr.s_addr == INADDR_NONE) {
		LOG_ERROR("Cannot parse server IP address: %s\n",
			  server_addr);
		return ERR_INVALID_ARG;
	}

	serv_addr.sin_family = AF_INET;
	serv_addr.sin_port = htons(port);

	if (connect(wm_sock_fd, (struct sockaddr *)&serv_addr,
		    sizeof(serv_addr)) < 0) {
		LOG_ERROR("Unable to connect to server at %s:%d\n",
			  server_addr, port);
		return ERR_NETWORK;
	}

	LOG_DEBUG("Connected to model_server at %s:%d\n",
		  server_addr, port);
	setsockopt(wm_sock_fd, IPPROTO_TCP, TCP_NODELAY, (void *)&on,
		   sizeof(on));

	return OK;
}

/**
 * wm_disconnect() - Disconnect from WM server.
 *
 * @retval	OK if successful.
 */
int wm_disconnect()
{
	return close(wm_sock_fd);
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
    be = addr & 0x7 ? 0xf: 0xff;
    iosf_msg[0] = 0x40000001;
    iosf_msg[1] = 0x00000000;
    iosf_msg[2] = ((addr & 0xffff) << 16 ) | be;
    iosf_msg[3] = addr >> 16;
	iosf_len = 16;

	err = iosf_send_receive((uint8_t *) iosf_msg, iosf_len,
				     		(uint8_t *) iosf_msg, &iosf_len);
	if (err) {
		LOG_ERROR("Error with iosf message tx/rx: %d\n", err);
		return err;
	}

    *val = iosf_msg[3];
    *val = (*val << 32 ) | iosf_msg[2];
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
	iosf_msg[2] = ((addr & 0xffff) << 16 ) | 2;
	iosf_msg[3] = addr >> 16;
	iosf_msg[4] = val & 0xffffffff;
	iosf_msg[5] = val >> 32;
	iosf_len = 24;

	err = iosf_send_receive((uint8_t *) iosf_msg, iosf_len,
					  		(uint8_t *) iosf_msg, &iosf_len);

	if (err)
		LOG_ERROR("Error with iosf message tx/rx: %d\n", err);

	return err;
}

/**
 * wm_server_start() - Start the model server.
 *
 * Fork a new process and run the model server. The infopath and infofile
 * arguments are set to a temporary file.
 * It then waits until the server comes up and connects to it.
 *
 * @param_in	cmd the executable file of the model server
 *
 * @retval		OK if successful
 */
int wm_server_start(char *cmd)
{
	char *exec_args[] = {cmd, "-n", "-m", "mby", "-ip", NULL, "-if", NULL, NULL};
	const int max_retries = 30;
	char server_if[L_tmpnam];
	char server_ip[L_tmpnam];
	const int delay = 1;
	pid_t pid;
	int i = 0;
	int err;

	if (!cmd) {
		LOG_ERROR("Server path string cannot be NULL\n");
		return ERR_INVALID_ARG;
	}

	/* Generate a temporary file name and extract directory name (infopath)
	 * and file name (infoname). Note that the functions are destructive so
	 * we need to make copies of the strings */
	tmpnam(server_tmpfile);
	LOG_INFO("Using temp models.packetServer: %s\n", server_tmpfile);

	pid = fork();
	if (pid == 0) {
		/* Child process: start model_server */
		strcpy(server_if, server_tmpfile);
		strcpy(server_ip, server_tmpfile);
		exec_args[5] = dirname(server_ip);
		exec_args[7] = basename(server_if);
		err = execvp(exec_args[0], exec_args);
		LOG_ERROR("Could not execute command: %s\n", cmd);
	}
	else if (pid > 0) {
		/* Parent process: wait 10sec then try to connect */
		// sprintf(fname, "%s/models.packetServer", infopath);
		sleep(10);
		do {
			sleep(delay);
			err = wm_connect(server_tmpfile);
			++i;
		} while(err && i < max_retries);
		if (err) {
			LOG_ERROR("Could not connect to the model_server\n");
			return ERR_TIMEOUT;
		}
	}
	else {
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
int wm_server_stop()
{
	unsigned char empty_msg = 0;
	int err;

	err = wm_send(&empty_msg, 0, MODEL_MSG_COMMAND_QUIT);
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
	int err;

	err = wm_send(tx_msg, tx_len, MODEL_MSG_IOSF);
	if (err) {
		LOG_ERROR("Could not send data to WM: %d\n", err);
		return err;
	}

	err = wm_receive(rx_msg, rx_len, &type);
	if (err) {
		LOG_ERROR("Did not receive data from WM: %d\n", err);
		return err;
	}

	/* TODO there might be problems if this library is used in
	 * multi-thread applications */
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
 * @param_out	msg caller-allocated buffer where the message will be stored.
 * @param_out	len caller-allocated used to store the length of the message.
 * @param_out	type caller-allocated used to store the type of the message.
 * @retval		OK if successful
 */
static int wm_receive(uint8_t *msg, uint32_t *len, uint16_t *type)
{
    uint8_t wm_msg[512];
	uint32_t wm_len;
	int err;

	/* Read the first 4 bytes to see the length of the message */
	err = wm_read_data(wm_sock_fd, wm_msg, 4, READ_TIMEOUT);
	if (err) {
		LOG_ERROR("Could not receive message from WM\n");
		return err;
	}

	wm_len = ntohl(*((unsigned int *)&wm_msg[0]));
	if (wm_len > (sizeof(wm_msg) - MODEL_MSG_HEADER_SIZE)) {
		LOG_ERROR("Length %d is out of bound", wm_len);
		return ERR_INVALID_RESPONSE;
	}

	/* Receive the remaining bytes */
	err = wm_read_data(wm_sock_fd, wm_msg + 4, wm_len - 4, READ_TIMEOUT);
	if (err) {
		LOG_ERROR("Could not receive message from WM\n");
		return err;
	}

	// *len = (wm_msg[0] << 24) | (wm_msg[1] << 16) |
	// 		(wm_msg[2] << 8) | wm_msg[3];
	*len = wm_len - MODEL_MSG_ERROR;
	*type = (wm_msg[6] << 8) | wm_msg[7];

	switch (*type) {
	case MODEL_MSG_ERROR:
		LOG_ERROR("%s: %s", __func__, wm_msg + 12);
		return ERR_INVALID_RESPONSE;
		break;
	case MODEL_MSG_IOSF:
		break;
	default:
		LOG_ERROR("Unexpected msg_type -0x%x", *type);
		return ERR_INVALID_RESPONSE;
	}

	// hex_dump((uint8_t *)&wm_msg, wm_len, 0);
	memcpy(msg, wm_msg + MODEL_MSG_HEADER_SIZE, *len);
	return OK;
}

/**
 * wm_send() - Send generic message to model_server socket interface.
 *
 * @param_int	msg pointer to the content of the message.
 * @param_int	len the length of the message.
 * @param_int	type the type of the message (e.g. SB-IOSF).
 * @retval		OK if successful
 */
static int wm_send(uint8_t *msg, uint32_t len, uint16_t type)
{
	struct wm_cq_msg wm_msg;
	uint32_t wm_len;
	uint32_t wr_len;

	wm_msg.version = htons(MODEL_VERSION);
	wm_msg.type = htons(type);
	wm_msg.sw = htons(0);

	//memcpy_s(msg.data, sizeof(msg.data), pmsg, msg_len);
	memcpy(wm_msg.data, msg, len);

	/* Type specific checks */
	if (type == MODEL_MSG_IOSF && len > IOSF_MSG_MAX_LEN) {
		LOG_ERROR("Message len %d is too large", len);
		return ERR_INVALID_ARG;
	}

	wm_len = len + MODEL_MSG_HEADER_SIZE;
	wm_msg.msg_length = htonl(wm_len);
	wm_msg.port = NONPOSTED_PORT;

	// hex_dump((uint8_t *)&wm_msg, wm_len, 0);

	wr_len = write(wm_sock_fd, &wm_msg, wm_len);
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
static void hex_dump(uint8_t *bytes, int nbytes, char show_ascii)
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
 * read_host_info() - Read host info from model_server file
 *
 * The expected content is in the format: "0:localhost:57548"
 *
 * @param_in	fd is the file pointer to read from.
 * @param_out	host is buffer to store host name.
 * @param_in	host_size is the size of host buffer.
 * @param_out	port is buffer to store host port number.
 * @retval	IES_OK if successful.
 */
static int read_host_info(FILE *fd, char *host, int host_size, int *port)
{
	unsigned int start, cnt;
	char buffer[256];
	char tmp[256];

	if (!fd || !host || host_size <= 0)
		return ERR_INVALID_ARG;

	if (fgets(buffer, sizeof(buffer), fd)) {
		start = 0;
		cnt = 0;
		while ((cnt < strlen(buffer)) && buffer[start + cnt] != ':')
			cnt++;

		bzero(&tmp, sizeof(tmp));
		memcpy(&tmp, buffer, cnt);

		start = start + cnt + 1;
		cnt = 0;
		while ((start + cnt < strlen(buffer)) &&
		       buffer[start + cnt] != ':')
			cnt++;
		if (start + cnt >= sizeof(buffer))
			return ERR_INVALID_ARG;

		bzero(host, host_size);
		memcpy(host, &buffer[start], cnt);

		start = start + cnt + 1;
		cnt = 0;
		while ((start + cnt < strlen(buffer)) &&
		       buffer[start + cnt] != ':')
			cnt++;
		if (start + cnt >= sizeof(buffer))
			return ERR_INVALID_ARG;

		bzero(&tmp, sizeof(tmp));
		//memcpy_s(&tmp, sizeof(tmp), &buffer[start], cnt);
		memcpy(&tmp, &buffer[start], cnt);
		*port = atoi(tmp);
		fclose(fd);
	}
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
		}
		else {

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

