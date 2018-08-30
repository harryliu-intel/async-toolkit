/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_generic_netlink.h
 * Creation Date:   September, 2011
 * Description:     Header file for generic netlink packet I/O
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2012 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM_GENERIC_NETLINK_H
#define __FM_FM_GENERIC_NETLINK_H


/*****************************************************************************
 * START OF DEFINITIONS SHARED BETWEEN PLATFORM AND KERNEL CODE. 
 *  
 * These definitions MUST be synchronized with the corresponding definitions 
 * in fm_generic_netlink_kernel.h.
 *****************************************************************************/

/**************************************************
 * Netlink packet message header.
 *
 * This structure is a common structure visible by
 * both the API and the driver.
 **************************************************/
struct fm_nlHdr
{
    u_short     sw;             /* switch number */
    u_short     reserved;
    u_int       fm64[2];        /* F64 tag */
};

/* Netlink socket, we use the same number as NETLINK_USERSOCK */
#define FM_NL_SOCK              2

/* Netlink listening group types when reading from the netlink inteface */
#define FM_NL_GROUP_L2          1
#define FM_NL_GROUP_IP          2
#define FM_NL_GROUP_ARP         4
#define FM_NL_GROUP_RAW_CB      8

#define FM_NL_GROUP_ALL         (FM_NL_GROUP_L2 | FM_NL_GROUP_RAW_CB | \
                                 FM_NL_GROUP_ARP | FM_NL_GROUP_IP)

/*****************************************************************************
 * END OF DEFINITIONS SHARED BETWEEN PLATFORM AND KERNEL CODE
 *****************************************************************************/

/* Netlink function prototypes */
fm_status fmNetlinkSendPackets(fm_int sw);
void * fmNetlinkReceivePackets(void *args);
fm_status fmNetlinkPacketHandlingInitialize(void);
fm_status fmNetlinkPacketHandlingInitializeV2(fm_int sw, fm_bool hasFcs);

#endif /* __FM_FM_GENERIC_NETLINK_H */
