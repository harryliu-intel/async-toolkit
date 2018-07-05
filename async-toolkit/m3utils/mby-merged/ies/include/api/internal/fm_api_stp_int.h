/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_stp_int.h
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

#ifndef __FM_FM_API_STP_INT_H
#define __FM_FM_API_STP_INT_H

/*****************************************************************************
 * Macros & Constants
 *****************************************************************************/

/** The maximum spanning tree instance number.
 *  \ingroup constSystem */
#define FM_MAX_STP_INSTANCE  4094



/*****************************************************************************
 * Types
 *****************************************************************************/


typedef struct
{
    /* The instance number for reference */
    fm_int      instance;

    /* A bitmap of the VLANs, with set bits indicating membership */
    fm_bitArray vlans;

    /* A list of states per port in the system */
    fm_int *    states;

} fm_stpInstanceInfo;


/*****************************************************************************
 * Function prototypes
 *****************************************************************************/


fm_status fmAllocateStpInstanceTreeDataStructures(fm_switch *switchPtr);
fm_status fmInitStpInstanceTree(fm_switch *switchPtr);
fm_status fmDestroyStpInstanceTree(fm_switch *switchPtr);
fm_status fmFreeStpInstanceTreeDataStructures(fm_switch *switchPtr);
fm_status fmResetMultipleSpanningTreeState(fm_int sw);
fm_status fmRefreshSpanningTreeStateForVlan(fm_int sw, fm_int vlanID);
fm_status fmFindInstanceForVlan(fm_int sw, fm_int vlanID, fm_int *instance);

fm_status fmRefreshStpStateInternal(fm_switch *switchPtr, 
                                    fm_stpInstanceInfo *instance, 
                                    fm_int vlanID, 
                                    fm_int port);
fm_status AddSpanningTreeVlanInternal(fm_int sw,
                                      fm_int stpInstance,
                                      fm_stpInstanceInfo *instance,
                                      fm_int vlanID);
fm_status fmDeleteSpanningTreeInternal(fm_int sw, 
                                       fm_stpInstanceInfo *instance);
fm_status fmRefreshStpState(fm_int sw,
                            fm_int stpInstance,
                            fm_int vlanID,
                            fm_int port);

#endif /* __FM_FM_API_STP_INT_H */
