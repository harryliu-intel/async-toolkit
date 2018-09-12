/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            lldp_int.h
 * Creation Date:   September 1, 2010
 * Description:     Prototypes and structure definitions for LLDP manager.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2010 - 2011 Intel Corporation. All Rights Reserved. 
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

#ifndef LLDP_INT_H
#define LLDP_INT_H

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>

/** LLDP log level */
extern int lldpLogLevel;

/** LLDP return codes */
enum lldpStatus {
    LLDP_OK,
    LLDP_ERR_UNKNOWN,
    LLDP_ERR_INVALID_ARG,
    LLDP_ERR_INVALID_RANGE,
    LLDP_ERR_INVALID_PACKET,
    LLDP_ERR_INVALID_FORMAT,
    LLDP_ERR_NOT_FOUND,
    LLDP_ERR_ALREADY_EXISTS,
    LLDP_ERR_NO_MEMORY
};

#ifndef bool
#define bool unsigned char
#define true 1
#define false 0
#endif

#ifndef MAX
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

#define LLDP_ERR_ENABLED()   (lldpLogLevel > 0)
#define LLDP_WARN_ENABLED()  (lldpLogLevel > 1)
#define LLDP_INFO_ENABLED()  (lldpLogLevel > 2)
#define LLDP_DBG_ENABLED()   (lldpLogLevel > 3)

#define LLDP_PRINT(...)      do { printf(__VA_ARGS__); } while (0)
#define LLDP_ERR(...)        do { if (LLDP_ERR_ENABLED())  printf(__VA_ARGS__); } while (0)
#define LLDP_WARN(...)       do { if (LLDP_WARN_ENABLED()) printf(__VA_ARGS__); } while (0)
#define LLDP_INFO(...)       do { if (LLDP_INFO_ENABLED()) printf(__VA_ARGS__); } while (0)
#define LLDP_DBG(...)        do { if (LLDP_DBG_ENABLED())  printf(__VA_ARGS__); } while (0)

/** Copy source string to destination. */
#define lldp_strcpy(DST,SRC) \
    do { \
        snprintf(DST, sizeof(DST), "%s", (const char *)(SRC)); \
    } while (0)

/** Copy source string to destination. */
#define lldp_strxcpy(DST,DST_SIZE,SRC,SRC_SIZE) \
    do { \
        int _slen = MIN((DST_SIZE - 1), (SRC_SIZE)); \
        memcpy((void *)(DST), (void *)(SRC), _slen); \
        ((char *)(DST))[_slen] = '\0'; \
    } while (0)

#endif /* LLDP_INT_H */

