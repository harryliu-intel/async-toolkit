/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm4000_model.h
 * Creation Date:   June 5, 2012
 * Description:     General prototypes for the FM4000 model.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2009 - 2013 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM4000_MODEL_H
#define __FM4000_MODEL_H

#ifndef __FM_PLATFORM_PORT_T
#define __FM_PLATFORM_PORT_T

typedef struct
{
    /* Logical port number. */
    fm_int      logPort;

    /* Physical port number. */
    fm_int      physPort;

} fm_platformPort;

#endif  /* __FM_PLATFORM_PORT_T */

/*****************************************************************************
 * These prototypes can be called from the platform initialization code and
 * from within the standard platform services.
 *****************************************************************************/

fm_status fm4000ModelGetPortMap(fm_platformPort *portMap, fm_int numPorts);

fm_status fm4000ModelInitialize(void **chipModel, fm_int sw, void *funcPtrs);

fm_status fm4000ModelPlatformCheckBpduDropping(fm_int           sw,
                                               fm_eventPktRecv *pktEvent,
                                               fm_int           vlan,
                                               fm_bool *        dropBpdu);

fm_status fm4000ModelPlatformGetCpuMaxFrameSize(fm_int     sw,
                                                fm_uint32 *cpuMaxFrameSize);

fm_status fm4000ModelPlatformGetCpuVlanTag(fm_int   sw,
                                           fm_int   vlan,
                                           fm_bool *tag);

fm_status fm4000ModelPlatformGetNumPorts(fm_int sw, fm_int *numPorts);

fm_status fm4000ModelReadCSR(fm_int sw, fm_uint32 addr, fm_uint32 *value);

fm_status fm4000ModelReadCSR64(fm_int sw, fm_uint32 addr, fm_uint64 *value);

fm_status fm4000ModelReadCSRMult(fm_int     sw,
                                 fm_uint32  addr,
                                 fm_int     n,
                                 fm_uint32 *value);

fm_status fm4000ModelReadCSRMult64(fm_int     sw,
                                   fm_uint32  addr,
                                   fm_int     n,
                                   fm_uint64 *value);

fm_status fm4000ModelReceivePacket(fm_int   sw,
                                   fm_int * port,
                                   fm_byte *packet,
                                   fm_int * length,
                                   fm_int   maxPktSize);

fm_status fm4000ModelReset(fm_int sw);

fm_status fm4000ModelSendPacket(fm_int                sw,
                                fm_int                port,
                                fm_byte *             packet,
                                fm_int                length,
                                fm_modelSidebandData *sbData);

fm_status fm4000ModelTick(fm_int sw, fm_uint32 *interrupt);

fm_status fm4000ModelWriteCSR(fm_int sw, fm_uint32 addr, fm_uint32 newValue);

fm_status fm4000ModelWriteCSRMult(fm_int     sw,
                                  fm_uint32  addr,
                                  fm_int     n,
                                  fm_uint32 *newValue);

fm_status fm4000ModelWriteCSR64(fm_int     sw,
                                fm_uint32  addr,
                                fm_uint64  newValue);

#endif /* __FM4000_MODEL_H */

