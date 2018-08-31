/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm2000_generic_lci.h
 * Creation Date:   2005
 * Description:     Header file for FM2000 based generic LCI packet transfer
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

#ifndef __FM_FM2000_GENERIC_LCI_H
#define __FM_FM2000_GENERIC_LCI_H

#include <platforms/common/packet/generic-lci/fm_generic_lci.h>

fm_status fm2000LCIPacketHandlingInitialize(fm_int sw);

fm_status fm2000LCISendPacketDirected(fm_int        sw, 
                                      fm_int *      portList,
                                      fm_int        numPorts, 
                                      fm_buffer *   packet);

fm_status fm2000LCISendPacketSwitched(fm_int sw, fm_buffer * packet);

fm_status fm2000LCISendPackets(fm_int sw);

fm_status fm2000LCISendPacket(fm_int         sw,
                              fm_packetInfo *info,
                              fm_buffer *    packet);

fm_status fm2000LCIReceivePackets(fm_int sw);

fm_status fm2000LCIReceivedPacketHandler(fm_int sw);


#endif /* __FM_FM2000_GENERIC_LCI_H */
