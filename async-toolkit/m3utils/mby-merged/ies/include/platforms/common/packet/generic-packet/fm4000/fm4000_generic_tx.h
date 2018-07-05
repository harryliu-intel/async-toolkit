/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm4000_generic_tx.h
 * Creation Date:   Jan 5, 2009
 * Description:     Header file for generic tx code
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

#ifndef __FM_FM4000_GENERIC_TX_H
#define __FM_FM4000_GENERIC_TX_H

#define FM4000_MAX_FRAME_TRAP_CODE 0x96

fm_status fm4000GenericSendPacketDirected(fm_int           sw, 
                                          fm_int *         portList,
                                          fm_int           numPorts, 
                                          fm_buffer *      packet,
                                          fm_packetInfoV2 *info);

fm_status fm4000GenericSendPacketSwitched(fm_int sw, fm_buffer *packet);


fm_status fm4000GenericSendPacket(fm_int         sw,
                                  fm_packetInfo *info,
                                  fm_buffer *    packet);

fm_status fm4000GetPortStagType(fm_int     sw,
                                fm_int     port,
                                fm_uint32 *stagTypeA,
                                fm_uint32 *stagTypeB);

fm_status fm4000SetIslVType(fm_int     sw,
                            fm_int     cpuPort,
                            fm_uint32  stagTypeA,
                            fm_uint32  stagTypeB,
                            fm_uint16  payloadVlanTag,
                            fm_bool    pTag,
                            fm_uint32 *value);

/* Use the generic function, but could later overide with fm4000 specific */
#define fm4000GenericPacketHandlingInitialize fmGenericPacketHandlingInitialize
#define fm4000GenericSendPacketISL fmGenericSendPacketISL

/* For backward compatible with customer platforms */
#define fm4000LCIPacketHandlingInitialize fm4000GenericPacketHandlingInitialize
#define fm4000LCISendPacketDirected fm4000GenericSendPacketDirected
#define fm4000LCISendPacketSwitched fm4000GenericSendPacketSwitched
#define fm4000LCISendPacket fm4000GenericSendPacket

#endif /* __FM_FM4000_GENERIC_TX_H */
