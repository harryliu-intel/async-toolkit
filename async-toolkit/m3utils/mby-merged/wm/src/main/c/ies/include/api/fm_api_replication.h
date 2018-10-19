/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces) */
/*****************************************************************************
 * File:             fm_api_replication.h
 * Creation Date:    June 29, 2011
 * Description:      Header file for replication services.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2007 - 2011 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM_API_REPLICATION_H
#define __FM_FM_API_REPLICATION_H


/*****************************************************************************
 *
 * Replication Group Entry
 *
 *****************************************************************************/
typedef struct _fm_intReplicationGroup
{
    /* The replication group handle, which must be used to identify the
     * replication group in all replication group functions */
    fm_int              handle;

    /* The index within the associated hardware resource table */
    fm_int              hwDestIndex;

    /* List of multicast group  */
    fm_tree             mcastGroupList;

} fm_intReplicationGroup;


fm_intReplicationGroup *findReplicationGroup(fm_int sw, fm_int handle);

fm_status fmCreateReplicationGroupInt(fm_int  sw, 
                                      fm_int *groupHandle,
                                      fm_int  mcastLogicalPort, 
                                      fm_int *mcastIndex);

fm_status fmDeleteReplicationGroupInt(fm_int sw, fm_int groupHandle);

fm_status fmMoveReplicationGroupMcastGroupInt(fm_int sw, 
                                              fm_int groupHandle, 
                                              fm_int mcastGroup);

fm_status fmCreateReplicationGroup(fm_int sw, fm_int *groupHandle);

fm_status fmDeleteReplicationGroup(fm_int sw, fm_int groupHandle);

#endif /* __FM_FM_API_REPLICATION_H */
