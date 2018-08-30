/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_stp.h
 * Creation Date:   January 16, 2008
 * Description:     Structures and functions for dealing with spanning tree
 *                  instances.
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

#ifndef __FM_FM_API_STP_H
#define __FM_FM_API_STP_H

/*****************************************************************************
 * Macros & Constants
 *****************************************************************************/

#define FM_DEFAULT_STP_INSTANCE 0

/*****************************************************************************
 * Types
 *****************************************************************************/


/*****************************************************************************
 * Function prototypes
 *****************************************************************************/


fm_status fmCreateSpanningTree(fm_int sw, fm_int stpInstance);
fm_status fmDeleteSpanningTree(fm_int sw, fm_int stpInstance);

fm_status fmAddSpanningTreeVlan(fm_int sw,
                                fm_int stpInstance, fm_int vlanID);

fm_status fmDeleteSpanningTreeVlan(fm_int sw,
                                   fm_int stpInstance,
                                   fm_int vlanID);

fm_status fmFindSpanningTreeByVlan (fm_int sw,
                                    fm_int vlanID,
                                    fm_int *instance);

fm_status fmSetSpanningTreePortState(fm_int sw,
                                     fm_int stpInstance,
                                     fm_int port,
                                     fm_int stpState);

fm_status fmGetSpanningTreePortState(fm_int  sw,
                                     fm_int  stpInstance,
                                     fm_int  port,
                                     fm_int *stpState);

fm_status fmGetSpanningTreeFirst(fm_int  sw,
                                 fm_int *firstStpInstance);

fm_status fmGetSpanningTreeNext(fm_int  sw,
                                fm_int  currentStpInstance,
                                fm_int *nextStpInstance);

fm_status fmGetSpanningTreeVlanFirst(fm_int  sw,
                                     fm_int  stpInstance,
                                     fm_int *firstVlan);

fm_status fmGetSpanningTreeVlanNext(fm_int  sw,
                                    fm_int  stpInstance,
                                    fm_int  currentVlan,
                                    fm_int *nextVlan);

void fmDbgDumpSpanningTree(fm_int sw, fm_int instance);

#endif /* __FM_FM_API_STP_H */
