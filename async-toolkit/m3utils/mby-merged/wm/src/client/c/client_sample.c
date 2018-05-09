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
#include <mbay_dpi_client.h>

int main()
{
	uint32_t addr;
	uint64_t val;
	int err;

	err = wm_connect();
	if (err) {
		printf("Error while connecting to the WM\n");
		return 1;
	}

	/* BSM_SCRATCH_0 of HLP */
	addr = 0x0010000;
	val = 0x1234;
	err = reg_write(addr, val);
	if (err) {
		printf("Error writing register\n");
		return 1;
	}

	err = reg_read(addr, &val);
	if (err) {
		printf("Error writing register\n");
		return 1;
	}

	if (val != 0x1234) {
		printf("Read unexpected value: %ld\n", val);
		return 1;
	}

	err = wm_disconnect();
	if (err)
		printf("Error while disconnecting to the WM\n");

	return err;
}

