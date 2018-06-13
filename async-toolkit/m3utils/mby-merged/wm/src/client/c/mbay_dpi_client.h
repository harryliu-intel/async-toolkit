/*****************************************************************************
 * @file	mbay_dpi_client.h
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

#ifndef __MBAY_DPI_CLIENT_H_
#define __MBAY_DPI_CLIENT_H_

#include <stdint.h>

/* Error codes */
#define OK		0
#define ERR_INVALID_ARG 		1
#define	ERR_NETWORK 			2
#define	ERR_TIMEOUT 			3
#define	ERR_NO_RESOURCE 		4
#define	ERR_INVALID_RESPONSE	5
#define	ERR_RUNTIME				6
#define	ERR_NO_MORE				7

int wm_server_start(char *cmd);
int wm_server_stop(void);

int wm_connect(const char *server_file);
int wm_disconnect(void);

int wm_reg_write(const uint32_t addr, const uint64_t val);
int wm_reg_read(const uint32_t addr, uint64_t *val);

int wm_pkt_push(int port, const uint8_t *data, uint32_t len);
int wm_pkt_get(int *port, uint8_t *data, uint32_t *len);

#endif /* __MBAY_DPI_CLIENT_H_ */
