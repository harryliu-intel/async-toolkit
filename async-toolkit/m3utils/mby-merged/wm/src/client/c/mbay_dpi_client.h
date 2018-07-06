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
#ifndef NO_SV
#include "svdpi.h"
#endif

/* Error codes */
enum wm_error {
	WM_OK = 0,
	WM_NO_DATA = 1,
	WM_ERR_INVALID_ARG,
	WM_ERR_NETWORK,
	WM_ERR_TIMEOUT,
	WM_ERR_NO_RESOURCE,
	WM_ERR_INVALID_RESPONSE,
	WM_ERR_RUNTIME
};

int wm_server_start(char *cmd);
int wm_server_stop(void);

int wm_connect(const char *server_file);
int wm_disconnect(void);

int wm_reg_write(const uint32_t addr, const uint64_t val);
int wm_reg_read(const uint32_t addr, uint64_t *val);

int wm_pkt_push(uint16_t port, const uint8_t *data, uint32_t len);
int wm_pkt_get(uint16_t *port, uint8_t *data, uint32_t *len);

#ifndef NO_SV
int wm_svpkt_push(uint16_t  port, const svOpenArrayHandle sv_data, uint32_t len);
int wm_svpkt_get(uint16_t  *port, svOpenArrayHandle sv_data, uint32_t *len);
#endif

#endif /* __MBAY_DPI_CLIENT_H_ */
