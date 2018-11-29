/*****************************************************************************
 * @file	mby_tcp_client_sample.c
 * @brief	Sample code for mby_tcp_client_library
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
#include <string.h>

#include "mby_tcp_client_library.h"

// Default path to the server file, can be overidden with -m <path>
#define SERVER_FILE "../../main/m3/model_server/AMD64_LINUX/models.packetServer"

void print_help(void);
int test_regs(void);
int test_pkts(void);

int
main(int argc, char *argv[])
{
  char *model_server_file = SERVER_FILE;
  char *server_type = NULL;
  int err = 0;
  int c = -1;

  /********** Process command line arguments ***********/
  if (argc < 1) {
    fprintf(stderr, "Invalid command line\n!");
    print_help();
  }

  while ((c = getopt(argc, argv, "s:m:h")) != -1)
    switch (c) {
    case 's':
      server_type = optarg;
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
  if (server_type)
    err = wm_server_start(server_type);
  else
    err = wm_connect(model_server_file);

  if (err) {
    printf("Error while connecting or starting to the WM\n");
    return 1;
  }
  else {
    printf("Started/connected to WM\n");
  }

  /********** Test write/read register operations ***********/
  err = test_regs();
  if (err)
    goto CLEANUP;


  /********** Test send/receive traffic ***********/
  err = test_pkts();
  if (err)
    goto CLEANUP;

 CLEANUP:
  /********** Disconnect (or stop) from the server ***********/
  err = server_type ? wm_server_stop() : wm_disconnect();
  if (err)
    printf("Error while disconnecting to the WM: %d\n", err);
  else
    printf("Disconnected from model_server\n");

  return err;
}

void
print_help(void)
{
  printf("sample_client - C-DPI sample client application\n\n");
  printf("Start or connect to the model server and perform some basic\n");
  printf("tests: reg access, pkt injection.\n\n");
  printf("Options:\n");
  printf(" -s [scala/m3]  Start the specified server. By default only connect to running process\n");
  printf(" -m <path>      Path and name of existing models.packetServer used when connecting\n");
  printf(" -h             Print this help message and exit\n");
}

int
test_regs(void)
{
  uint32_t addr;
  uint64_t val;
  int err;

  /* In HLP this is BSM_SCRATCH_0[0] */
  addr = 0x0010000;
  val = 0x1234567890abcdef;
  printf("Write: addr=0x%x val=0x%lx\n", addr, val);
  err = wm_reg_write(addr, val);
  if (err) {
    printf("Error writing register: %d\n", err);
    return WM_ERR_RUNTIME;
  }

  printf("OK\n");
  printf("Read: addr=0x%x\n", addr);
  err = wm_reg_read(addr, &val);
  if (err) {
    printf("Error reading register: %d\n", err);
    return WM_ERR_RUNTIME;
  }

  if (val != 0x1234567890abcdef)
    printf("Unexpected value: 0x%lx\n", val);
  else
    printf("OK\n");

  return WM_OK;
}

int
test_pkts(void)
{
  /* Hardcoded test frame with Crc */
  uint8_t tx_pkt_data[] = {
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b,
    0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
    0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23,
    0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
    0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x3b,
    0xee, 0x7f, 0xec, 0xb0
  };
  struct wm_pkt tx_pkt;
  struct wm_pkt rx_pkt;
  unsigned int i;
  int err;
  int phys_port = 1;

  tx_pkt.port = phys_port;
  tx_pkt.len = sizeof(tx_pkt_data);
  memcpy(tx_pkt.data, tx_pkt_data, sizeof(tx_pkt_data));
  printf("sleeping a bit\n");
  sleep(5);
  printf("Pushing packet\n");
  err = wm_pkt_push(&tx_pkt);
  if (err) {
    printf("Error sending traffic: %d\n", err);
    return err;
  }

  printf("Getting packet\n");
  err = wm_pkt_get(phys_port, &rx_pkt);
  if (err) {
    printf("Error receiving traffic: %d\n", err);
    return err;
  }

  printf("Received %d bytes on port %d\n", rx_pkt.len, rx_pkt.port);
  if (tx_pkt.len != rx_pkt.len) {
    printf("Unexpected difference between in length between sent (%d) and received (%d) pkt\n",
           tx_pkt.len, tx_pkt.len);
    return WM_ERR_RUNTIME;
  }

  if (memcmp(tx_pkt.data, rx_pkt.data, tx_pkt.len)) {
    printf("Unexpected difference between sent and received pkt\n");
    for (i = 0; i < tx_pkt.len; ++i)
      if (tx_pkt.data[i] != rx_pkt.data[i])
        printf("tx_pkt.data[%d] = 0x%x - rx_pkt.data[%d] = 0x%x\n",
               i, tx_pkt.data[i], i, rx_pkt.data[i]);
    return WM_ERR_RUNTIME;
  }

  err = wm_pkt_get(phys_port, &rx_pkt);
  if (err == WM_NO_DATA) {
    printf("Received EOT as expected\n");
  } else {
    printf("Unexpected error code %d - should be WM_NO_DATA %d\n", err,
           WM_NO_DATA);
    return WM_ERR_RUNTIME;
  }

  err = wm_pkt_get(phys_port, &rx_pkt);
  if (err != WM_NO_DATA && err != WM_OK) {
    printf("Did not receive anything from WM as expected\n");
  } else {
    printf("Unexpected error code %d\n", err);
    return WM_ERR_RUNTIME;
  }

  return WM_OK;
}
