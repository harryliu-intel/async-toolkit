/*****************************************************************************
 * @file	client_sample.c
 * @brief	Sample code for mbay_dpi_client library
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
#include <mbay_dpi_client.h>

#define SERVER_PATH "../../main/m3/"
#define SERVER_EXEC SERVER_PATH "model_server/AMD64_LINUX/modelserver"
#define SERVER_FILE SERVER_PATH "models.packetServer"

void print_help(void)
{
	printf("sample_client - C-DPI sample client application\n\n");
	printf("Start or connect to the model server and perform a register\n");
	printf("write followed by a read to verify the value.\n\n");
	printf("Options:\n");
	printf(" -s         Start the server instead of only connecting to it\n");
	printf(" -e <path>  Path of the executable file of the model server\n");
	printf(" -m <path>  Path of the models.packetServer including file name\n");
	printf(" -h         Print this help message and exit\n");
}

int main(int argc, char **argv)
{
	char *model_server_file = SERVER_FILE;
	char *model_server_exec = SERVER_EXEC;
	int start_server = 0;
	uint32_t addr;
	uint64_t val;
	int err;
	int c;

	/********** Process command line arguments ***********/
	while ((c = getopt(argc, argv, "se:m:h")) != -1)
		switch (c) {
		case 's':
			start_server = 1;
			break;
		case 'e':
			model_server_exec = optarg;
			break;
		case 'm':
			model_server_file = optarg;
			break;
		case 'h':
			print_help();
			return 0;
		default:
			printf("Invalid command line argument: %c\n\n", c);
			print_help();
			return 1;
		}

	/********** Start or connect to the server ***********/
	if (start_server)
		err = wm_server_start(model_server_exec);
	else
		err = wm_connect(model_server_file);
	if (err) {
		printf("Error while connecting to the WM\n");
		return 1;
	}

	/********** Test write/read register operations ***********/
	/* In HLP this is BSM_SCRATCH_0[0] */
	addr = 0x0010000;
	val = 0x1234567890abcdef;
	printf("Write: addr=0x%x val=0x%lx\n", addr, val);
	err = wm_reg_write(addr, val);
	if (err) {
		printf("Error writing register: %d\n", err);
		goto CLEANUP;
	}

	printf("OK\n");
	printf("Read: addr=0x%x\n", addr);
	err = wm_reg_read(addr, &val);
	if (err) {
		printf("Error reading register: %d\n", err);
		goto CLEANUP;
	}

	if (val != 0x1234567890abcdef)
		printf("Unexpected value: 0x%lx\n", val);
	else
		printf("OK\n");


	/********** Test send/receive traffic ***********/
	err = wm_pkt_push(1, (uint8_t *)"12345678", 8);
	if (err) {
		printf("Error sending traffic: %d\n", err);
		goto CLEANUP;
	}

CLEANUP:
	err = start_server ? wm_server_stop() : wm_disconnect();
	if (err)
		printf("Error while disconnecting to the WM: %d\n", err);
	else
		printf("Disconnected from model_server\n");

	return err;
}

