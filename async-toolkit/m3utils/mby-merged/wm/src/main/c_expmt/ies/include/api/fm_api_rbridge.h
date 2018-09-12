/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_rbridge.h
 * Creation Date:   August 31, 2012
 * Description:     Constants for attributes and attribute values
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

#ifndef __FM_FM_API_RBRIDGE_H
#define __FM_FM_API_RBRIDGE_H

/** Maximum number of distribution tree        \ingroup constSystem */
#define FM_MAX_DISTRIBUTION_TREE    256

/** Maximum number of remote RBridge supported \ingroup constSystem */
#define FM_MAX_REMOTE_RBRIDGE       (4096 - FM_MAX_DISTRIBUTION_TREE)

/* First Distribution Tree index */
#define FM_FIRST_DISTRIBUTION_TREE_INDEX    0

/* First Distribution Tree index */
#define FM_FIRST_RBRIDGE_INDEX              (0 + FM_MAX_DISTRIBUTION_TREE)

/* Max Distribution Tree Hop Count */
#define FM_MAX_HOP_COUNT   0x3f


/****************************************************************************/
/** \ingroup typeStruct
 *
 * Defines a structure containing information that describes a
 * remote RBridge.
 * 
 * Referenced by:
 * ''fmCreateRBridge'',
 * ''fmGetRBridgeEntry'',
 * ''fmGetRBridgeFirst'',
 * ''fmGetRBridgeNext'',
 * ''fmUpdateRBridgeEntry''.
 ****************************************************************************/
typedef struct _fm_remoteRBridge
{
    /** NextHop RBridge MAC address used to reach this RBridge. */
    fm_macaddr    address;
   
    /** Remote RBridge nickname. */
    fm_uint16     egressNick;
   
} fm_remoteRBridge;


/****************************************************************************/
/** \ingroup typeStruct
 *
 * Defines a structure containing information that describes a
 * Distribution Tree.
 * 
 * Referenced by:
 * ''fmCreateRBridgeDistTree'',
 * ''fmGetRBridgeDistTree'',
 * ''fmGetRBridgeDistTreeFirst'',
 * ''fmGetRBridgeDistTreeNext'',
 * ''fmUpdateRBridgeDistTree''.
 ****************************************************************************/
typedef struct _fm_distTree
{
    /** Hop Count for this specific distribution tree. */
    fm_uint16   hopCnt;
   
    /** Nickname associated with this distribution tree. */
    fm_uint16   nick;
   
} fm_distTree;


fm_status fmCreateRBridge(fm_int            sw, 
                          fm_remoteRBridge *rbridge, 
                          fm_int *          tunnelId);

fm_status fmDeleteRBridge(fm_int sw, fm_int tunnelId);

fm_status fmUpdateRBridgeEntry(fm_int            sw,
                               fm_int            tunnelId,
                               fm_remoteRBridge *rbridge);

fm_status fmCreateRBridgeDistTree(fm_int       sw, 
                                  fm_distTree *distTree, 
                                  fm_int *     tunnelId);

fm_status fmDeleteRBridgeDistTree(fm_int sw, fm_int tunnelId);

fm_status fmUpdateRBridgeDistTree(fm_int       sw,
                                  fm_int       tunnelId,
                                  fm_distTree *distTree);

fm_status fmGetRBridgeEntry(fm_int            sw,
                            fm_int            tunnelId,
                            fm_remoteRBridge *rbridge);

fm_status fmGetRBridgeFirst(fm_int            sw,
                            fm_int *          tunnelId,
                            fm_remoteRBridge *rbridge);

fm_status fmGetRBridgeNext(fm_int            sw,
                           fm_int            currentTunnelId,
                           fm_int *          nextTunnelId,
                           fm_remoteRBridge *rbridge);

fm_status fmGetRBridgeDistTree(fm_int       sw,
                               fm_int       tunnelId,
                               fm_distTree *distTree);

fm_status fmGetRBridgeDistTreeFirst(fm_int       sw,
                                    fm_int *     tunnelId,
                                    fm_distTree *distTree);

fm_status fmGetRBridgeDistTreeNext(fm_int       sw,
                                   fm_int       currentTunnelId,
                                   fm_int *     nextTunnelId,
                                   fm_distTree *distTree);

fm_status fmSetRBridgePortHopCount(fm_int sw, fm_int port, fm_uint32 value);

fm_status fmGetRBridgePortHopCount(fm_int sw, fm_int port, fm_uint32 *value);

#endif /* __FM_FM_API_RBRIDGE_H */
