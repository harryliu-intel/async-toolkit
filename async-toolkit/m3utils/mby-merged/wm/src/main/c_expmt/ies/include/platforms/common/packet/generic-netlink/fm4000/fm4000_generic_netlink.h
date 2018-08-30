/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm4000_generic_netlink.h
 * Creation Date:   Jan 13, 2009
 * Description:     Header file for generic netlink packet I/O
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2011 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM4000_GENERIC_NETLINK_H
#define __FM_FM4000_GENERIC_NETLINK_H

/* For backward compatible with customer platforms */

#define fm4000NetlinkSendPackets fmNetlinkSendPackets
#define fm4000NetlinkReceivePackets fmNetlinkReceivePackets
#define fm4000NetlinkPacketHandlingInitialize fmNetlinkPacketHandlingInitialize

#endif /* __FM_FM4000_GENERIC_NETLINK_H */
