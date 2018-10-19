/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_stacking_int.h
 * Creation Date:   June 11, 2008
 * Description:     Internal prototypes for managing stacked intra and extra 
 *                  switch aggregate systems.
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

#ifndef __FM_FM_API_STACKING_INT_H
#define __FM_FM_API_STACKING_INT_H


/***************************************************
 * The internal version of the forwarding rule
 * object, pointing at the specific CAM entry that
 * the resource maps to.
 **************************************************/
typedef struct _fm_forwardRuleInternal
{
    /* The original rule */
    fm_forwardRule rule;

    /* Chip specific extensions to forward rules */
    void *extension;

} fm_forwardRuleInternal;

/***************************************************
 * This state structure holds all the per-switch
 * information related to stacking that is not
 * already held in some other state structure.
 **************************************************/
typedef struct _fm_stackingInfo
{
    /***************************************************
     * Holds a tree of forwarding rules by ID.  A 
     * different structure will hold them in LPM order.
     **************************************************/
    fm_tree fwdRules;

    /* Holds a list of used forwarding rule IDs */
    fm_bitArray usedRuleIDs;

} fm_stackingInfo;

fm_status fmInitStacking(fm_int sw);
fm_status fmFreeStackingResources(fm_int sw);
fm_status fmFindForwardingRulePortByGlort(fm_int    sw, 
                                          fm_uint32 glort, 
                                          fm_int *  logicalPort);
fm_status fmGetInternalPortFromRemotePort(fm_int sw, 
                                          fm_int remotePort,
                                          fm_int *internalPort);

#endif /* __FM_FM_API_STACKING_INT_H */
