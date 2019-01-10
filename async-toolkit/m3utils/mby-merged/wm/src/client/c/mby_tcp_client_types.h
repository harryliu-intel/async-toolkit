/*****************************************************************************
 * @file	mby_tcp_client_types.h
 * @brief	Client types for reg access through model_server socket
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

#ifndef MBY_TCP_CLIENT_TYPES_H
#define MBY_TCP_CLIENT_TYPES_H

#include <stdint.h>

/* In the future, the content of this message could be auto-generated */

#define MODEL_MSG_HEADER_SIZE           12

#define MODEL_VERSION                   2
#define MODEL_MSG_LINK_STATE            1
#define MODEL_MSG_PACKET                0
#define MODEL_MSG_PACKET_EOT            6
#define MODEL_MSG_SET_EGRESS_INFO       3
#define MODEL_MSG_MGMT                  7
#define MODEL_MSG_ERROR                 10
#define MODEL_MSG_IOSF                  11
#define MODEL_MSG_CTRL                  12
#define MODEL_MSG_VERSION_INFO          13
#define MODEL_MSG_NVM_READ              14
#define MODEL_MSG_COMMAND_QUIT          15
#define MODEL_MSG_STAGE                 16

/* Max length of IOSF messages used for read/write operations */
#define IOSF_MSG_MAX_LEN                512

/* In HLP this is the max length of a mailbox (?) message */
#define MAX_MSG_LEN (32 * 1024)

/* Model server message format */
struct wm_msg {
        /* -- 16/32-bit members must be aligned to a 2/4-byte boundary -- */

        /** Length of the entire message in bytes, including the header */
        uint32_t msg_length;

        /** The wm_cq_msg version. */
        uint16_t version;

        /** Message type (see IES_MODEL_MSG_*). */
        uint16_t type;

        /** Target model switch number for this message. */
        uint16_t sw;

        /** Target model switch port for this message. */
        uint16_t port;

        /* Data buffer.
         *
         * For SBIOSF messages:
         * - Buffer with SAI header from server point of view
         * - From API perpective, buffer does not include SAI header
         *
         * For PACKET messages:
         * - A sequence of TLV (see types defs below)
         */
        uint8_t data[MAX_MSG_LEN];
};

/* The payload of a message MODEL_MSG_PACKET is a sequence of TLV.
 * The constants below define the structure of the type and length.
 */
#define WM_DATA_TYPE_SIZE       1
#define WM_DATA_LENGTH_SIZE     4
#define WM_DATA_TLV_SIZE        (WM_DATA_TYPE_SIZE + WM_DATA_LENGTH_SIZE)

/* Type of the data sent with a message MODEL_MSG_PACKET
 * Use the same values that were defined in HLP.
 */
typedef enum {
        /** Ethernet frame. */
        WM_DATA_TYPE_PACKET = 0xA0, /* Non-zero field so it is easier to locate */

        /** 32-bit packet identifier. */
        WM_DATA_TYPE_SB_ID,

        /** 8-bit egress traffic class. */
        WM_DATA_TYPE_SB_TC,

        /** Packet Meta. */
        WM_DATA_TYPE_META,

} wm_data_type;

/* TODO this constants will be removed since metadata will change */
#define RIMMON_META_SIZE   8
#define CPK_META_SIZE      32

/* Number of external ports available to inject/receive traffic */
#define NUM_PORTS 32

#endif
