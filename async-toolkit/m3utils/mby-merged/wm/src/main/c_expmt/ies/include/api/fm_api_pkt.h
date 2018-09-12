/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_pkt.h
 * Creation Date:   May 15, 2007
 * Description:     Functions for dealing with packets
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2013 Intel Corporation. All Rights Reserved.
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

#ifndef __FM_FM_API_PKT_H
#define __FM_FM_API_PKT_H

fm_status fmSendPacketExt(fm_int sw, fm_packetInfo *info, fm_buffer *pkt);
fm_status fmSendPacket(fm_int sw, fm_int destMask, fm_buffer *pkt);
fm_status fmSendPacketDirected(fm_int     sw,
                               fm_int *   portList,
                               fm_int     numPorts,
                               fm_buffer *pkt);
fm_status fmSendPacketDirectedV2(fm_int           sw, 
                                 fm_int *         portList, 
                                 fm_int           numPorts,
                                 fm_buffer *      pkt,
                                 fm_packetInfoV2 *info);
fm_status fmSendPacketSwitched(fm_int sw, fm_buffer *pkt);
fm_status fmSendPacketSwitchedOnward(fm_int     sw, 
                                     fm_int     srcPort, 
                                     fm_int     vlan,
                                     fm_buffer *pkt);
fm_status fmSendPacketISL(fm_int          sw,
                          fm_uint32 *     islTag,
                          fm_islTagFormat islTagFormat,
                          fm_buffer *     pkt);

#endif /* __FM_FM_API_PKT_H */
