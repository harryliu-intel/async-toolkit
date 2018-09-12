/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_lag.h
 * Creation Date:   May 5, 2005
 * Description:     Structures and functions for dealing with link aggregation
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

#ifndef __FM_FM_API_LAG_H
#define __FM_FM_API_LAG_H

/* creates a link aggregation group */
fm_status fmCreateLAG(fm_int *lagNumber);
fm_status fmCreateLAGExt(fm_int sw, fm_int *lagNumber);

/* deletes a link aggregation group */
fm_status fmDeleteLAG(fm_int lagNumber);
fm_status fmDeleteLAGExt(fm_int sw, fm_int lagNumber);

/* adds a port to a link aggregation group */
fm_status fmAddLAGPort(fm_int sw, fm_int lagNumber, fm_int port);

/* deletes a port from a link aggregation group */
fm_status fmDeleteLAGPort(fm_int sw, fm_int lagNumber, fm_int port);

/* retrieves a list of link aggregation groups */
fm_status fmGetLAGList(fm_int *nLAG, fm_int *lagNumbers, fm_int maxLags);
fm_status fmGetLAGListExt(fm_int  sw,
                          fm_int* nLAG,
                          fm_int* lagNumbers,
                          fm_int  maxLags);

/* retrieves a list of ports in a link aggregation group */
fm_status fmGetLAGPortList(fm_int  lagNumber,
                           fm_int *nPorts,
                           fm_int *ports,
                           fm_int *sw,
                           fm_int  maxPorts);
fm_status fmGetLAGPortListExt(fm_int  sw,
                              fm_int  lagNumber,
                              fm_int *nPorts,
                              fm_int *ports,
                              fm_int  maxPorts);

/* retrieves the first LAG on a switch */
fm_status fmGetLAGFirst(fm_int *firstLagNumber);
fm_status fmGetLAGFirstExt(fm_int sw, fm_int *firstLagNumber);

/* retrieves the next LAG on a switch */
fm_status fmGetLAGNext(fm_int currentLagNumber, fm_int *nextLagNumber);
fm_status fmGetLAGNextExt(fm_int sw,
                          fm_int currentLagNumber,
                          fm_int *nextLagNumber);

/* retrieves the first port in a LAG */
fm_status fmGetLAGPortFirst(fm_int  lagNumber,
                            fm_int *firstPort,
                            fm_int *firstSwitch);
fm_status fmGetLAGPortFirstExt(fm_int  sw,
                               fm_int  lagNumber,
                               fm_int *firstPort);

/* retrieves the next port in a LAG */
fm_status fmGetLAGPortNext(fm_int  lagNumber,
                           fm_int  currentPort,
                           fm_int  currentSwitch,
                           fm_int *nextPort,
                           fm_int *nextSwitch);
fm_status fmGetLAGPortNextExt(fm_int  sw,
                              fm_int  lagNumber,
                              fm_int  currentPort,
                              fm_int *nextPort);

/* retrieves LAG attributes */
fm_status fmGetLAGAttribute(fm_int attribute, fm_int index, void *value);
fm_status fmGetLAGAttributeExt(fm_int sw, fm_int attribute, fm_int index, void *value);

/* sets LAG attributes */
fm_status fmSetLAGAttribute(fm_int attribute, fm_int index, void *value);
fm_status fmSetLAGAttributeExt(fm_int sw, fm_int attribute, fm_int index, void *value);

/* converts LAG handle to/from logical port number */
fm_status fmLogicalPortToLAGNumber(fm_int  sw,
                                   fm_int  logicalPort,
                                   fm_int *lagNumber);
fm_status fmLAGNumberToLogicalPort(fm_int  sw,
                                   fm_int  lagNumber,
                                   fm_int *logicalPort);


#endif /* __FM_FM_API_LAG_H */
