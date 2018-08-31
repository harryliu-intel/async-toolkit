/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            testpoint_support.h
 * Creation Date:   August 30, 2010
 * Description:     A simple interface for connecting to the model packet
 *                  queue.
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

#ifndef __FM_MODEL_PACKET_INTF_H
#define __FM_MODEL_PACKET_INTF_H

void fmModelTestPointIntfInit(void);
void fmModelTestPointIntfExit(void);

/* Helper function to send a L2 packet to the packet queue */
void fmModelTestPointIntfSendL2Packet(fm_int sw, 
                                   fm_int port, 
                                   fm_int dmacHigh, 
                                   fm_int dmacLow, 
                                   fm_int smacHigh, 
                                   fm_int smacLow, 
                                   fm_int vlan, 
                                   fm_int vpri, 
                                   fm_int type,
                                   fm_int length);

void fmModelTestPointIntfSendFullPacket(fm_int sw, 
                                        fm_int port, 
                                        fm_int * sendData,
                                        fm_int length);

void fmModelTestPointIntfPrintCapture(void);
void fmModelTestPointIntfPrintPackets(fm_bool enable);

#endif /* __FM_MODEL_PACKET_INTF_H */


