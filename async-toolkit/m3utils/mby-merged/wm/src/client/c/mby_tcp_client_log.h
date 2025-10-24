/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

/*****************************************************************************
 * @file	mby_tcp_client_log.h
 * @brief	Logging macros for TCP client
 *
 * INTEL CONFIDENTIAL
 * Copyright 2018 - 2019 Intel Corporation.  All Rights Reserved.
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

#ifndef MBY_TCP_CLIENT_LOG_H
#define MBY_TCP_CLIENT_LOG_H

// Macros to control logging levels
#define LOG_LEVEL_DEBUG   4
#define LOG_LEVEL_INFO    3
#define LOG_LEVEL_WARNING 2
#define LOG_LEVEL_ERROR   1
#define LOG_LEVEL_NONE    0

// By default we enable debug level
#define LOG_LEVEL LOG_LEVEL_DEBUG

// Color codes in case we want colored output
#define LOG_COLOR_NRM  "\x1B[0m"
#define LOG_COLOR_RED  "\x1B[31m"
#define LOG_COLOR_GRN  "\x1B[32m"
#define LOG_COLOR_YEL  "\x1B[33m"
#define LOG_COLOR_BLU  "\x1B[34m"
#define LOG_COLOR_MAG  "\x1B[35m"
#define LOG_COLOR_CYN  "\x1B[36m"
#define LOG_COLOR_WHT  "\x1B[37m"

#ifdef LOG_COLOR
// Define the format of all log prints with or without colors
# define LOG(level, color, ...)\
    do { \
        fprintf(stderr, color "[" #level "]" LOG_COLOR_NRM "[%s:%d]: ", __func__, __LINE__);\
        fprintf(stderr, __VA_ARGS__);\
    } while(0);
#else
# define LOG(level, color, ...)\
    do { \
        fprintf(stderr, "[" #level "][%s:%d]: ", __func__, __LINE__);\
        fprintf(stderr, __VA_ARGS__);\
    } while(0);
#endif

#if defined(LOG_LEVEL) && LOG_LEVEL >= LOG_LEVEL_DEBUG
# define LOG_DEBUG(...) LOG(debug, LOG_COLOR_BLU,  __VA_ARGS__)
# define LOG_HEX_DUMP hex_dump
#else
# define LOG_DEBUG(...)
# define LOG_HEX_DUMP hex_dump
#endif

#if defined(LOG_LEVEL) && LOG_LEVEL >= LOG_LEVEL_INFO
# define LOG_INFO(...) LOG(info, LOG_COLOR_GRN,  __VA_ARGS__)
#else
# define LOG_INFO(...)
#endif

#if defined(LOG_LEVEL) && LOG_LEVEL >= LOG_LEVEL_WARNING
# define LOG_WARNING(...) LOG(warn, LOG_COLOR_YEL, __VA_ARGS__)
#else
# define LOG_WARNING(...)
#endif

#if defined(LOG_LEVEL) && LOG_LEVEL >= LOG_LEVEL_ERROR
# define LOG_ERROR(...) LOG(error, LOG_COLOR_RED, __VA_ARGS__)
#else
# define LOG_ERROR(...)
#endif

#endif /* MBY_TCP_CLIENT_LOG_H */

