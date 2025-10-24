/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

/*****************************************************************************
 * @file	fpps_reg_table.c
 * @brief	Madison Bay registers table
 *
 * INTEL CONFIDENTIAL
 * Copyright 2015 - 2018 Intel Corporation.  All Rights Reserved.
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

#include "fpps_reg_table.h"

const struct fpps_reg_entry fpps_reg_table[] = {
    /* regname          address */
    {"example_a_reg_1", 0x0011000 },
    {"example_a_reg_2", 0x0012000 },
    {"example_a_reg_3", 0x0013000 },
    {"example_b_reg_1", 0x0021000 },
    {"example_b_reg_2", 0x0022000 },
    {"example_b_reg_3", 0x0023000 },
    {"example_c_reg_1", 0x0031000 },
    {"example_c_reg_2", 0x0032000 },
    {"example_d_reg_1", 0x0041000 },
    {"", 0},
};

const unsigned int fpps_reg_table_size = sizeof(fpps_reg_table) / sizeof(fpps_reg_table[0]);
