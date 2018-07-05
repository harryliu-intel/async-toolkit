/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            platform_remote.h
 * Creation Date:   April 26, 2016
 * Description:     Remote operations to white model server.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2016 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_PLATFORM_REMOTE_H
#define __FM_PLATFORM_REMOTE_H

fm_status platformRemoteInitialize(void **chipModel, fm_int sw, void *funcPtrs);
fm_status platformRemoteReset(fm_int sw);
fm_status platformRemoteResetV2(fm_int sw, fm_int domain);

fm_status platformRemoteTick(fm_int sw, fm_uint32 *interrupt);

fm_status platformRemoteReadCSR(fm_int sw, fm_uint32 addr, fm_uint32 *value);
fm_status platformRemoteReadCSR64(fm_int sw, fm_uint32 addr, fm_uint64 *value);
fm_status platformRemoteWriteCSR(fm_int sw, fm_uint32 addr, fm_uint32 value);
fm_status platformRemoteWriteCSR64(fm_int sw, fm_uint32 addr, fm_uint64 value);

fm_status platformRemoteReadCSRMult(fm_int     sw,
                                    fm_uint32  addr,
                                    fm_int     n,
                                    fm_uint32 *value);
fm_status platformRmoteWriteCSRMult(fm_int     sw,
                                    fm_uint32  addr,
                                    fm_int     n,
                                    fm_uint32 *newValue);
fm_status platformRemoteReadCSRMult64(fm_int     sw,
                                      fm_uint32  addr,
                                      fm_int     n,
                                      fm_uint64 *value);
fm_status platformRmoteWriteCSRMult64(fm_int     sw,
                                      fm_uint32  addr,
                                      fm_int     n,
                                      fm_uint64 *newValue);

fm_status platformRemoteSendPkt(fm_int sw, fm_buffer *buffer);

#endif /* __FM_PLATFORM_REMOTE_H */

