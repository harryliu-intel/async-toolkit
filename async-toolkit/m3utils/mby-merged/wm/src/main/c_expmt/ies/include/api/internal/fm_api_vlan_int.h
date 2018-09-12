/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_vlan_int.h
 * Creation Date:   January 1, 2005
 * Description:     Structures and functions for dealing with VLAN 
 *                  configuration
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

#ifndef __FM_FM_API_VLAN_INT_H
#define __FM_FM_API_VLAN_INT_H


/*****************************************************************************
 * Macros & Constants
 *****************************************************************************/

/* the vlan tag types */
#define FM_VLAN_TAG_TYPE_8100  0x8100
#define FM_VLAN_TAG_TYPE_9100  0x9100
#define FM_VLAN_TAG_TYPE_9200  0x9200


/*****************************************************************************
 * Types
 *****************************************************************************/

/* structure to hold VLAN entry */
typedef struct
{
    /* The vlan number this entry refers to. */
    fm_uint16 vlanId;

    /* Used internally to store validity of this entry */
    fm_bool   valid;

    /* When true, indicates that this VLAN allows traffic to
     * flood back to itself.
     */
    fm_bool   reflect;

    /* Indicates that IGMP trapping is permitted on this VLAN.
     * 
     * \chips 4000, 5000, 6000
     */
    fm_bool   trapIGMP;

    /* Indicates that frames may route on this VLAN.
     * 
     * \chips 4000, 5000, 6000
     */
    fm_bool   routable;

    /* Pointer to the switch specific extension */
    void *    vlanExt;

} fm_vlanEntry;


/*****************************************************************************
 * Function prototypes
 *****************************************************************************/

/* initializes state of Vlan table */
fm_status fmAllocateVlanTableDataStructures(fm_switch *switchPtr);
fm_status fmInitVlanTable(fm_switch *switchPtr);
fm_status fmFreeVlanTableDataStructures(fm_switch *switchPtr);


/* helper functions to abstract entry */
fm_status fmSetVlanMembership(fm_int        sw,
                              fm_vlanEntry *entry,
                              fm_int        port,
                              fm_bool       state);
fm_status fmSetVlanTag(fm_int        sw,
                       fm_vlanSelect vlanSel,
                       fm_vlanEntry *entry,
                       fm_int        port,
                       fm_bool       tag);
fm_status fmGetVlanMembership(fm_int        sw,
                              fm_vlanEntry *entry,
                              fm_int        port,
                              fm_bool *     state);
fm_status fmGetVlanTag(fm_int        sw,
                       fm_vlanSelect vlanSel,
                       fm_vlanEntry *entry,
                       fm_int        port,
                       fm_bool *     tag);

fm_status fmGetVlanPortStateInternal(fm_int    sw,
                                     fm_uint16 vlanID,
                                     fm_int    port,
                                     fm_int *  state);

fm_status fmSetVlanPortStateInternal(fm_int    sw,
                                     fm_uint16 vlanID,
                                     fm_int    port,
                                     fm_int    state);

/* Port list extraction utilities. */
fm_status fmExtractVlanPhysicalPortList(fm_int   sw,
                                        fm_int   numVlanPorts,
                                        fm_int * vlanPortList,
                                        fm_int * numPhysPorts,
                                        fm_int * physPortList,
                                        fm_int   maxPhysPorts);

fm_status fmExtractVlanLagPortList(fm_int   sw,
                                   fm_int   numVlanPorts,
                                   fm_int * vlanPortList,
                                   fm_int * numLagPorts,
                                   fm_int * lagPortList,
                                   fm_int   maxLagPorts);

/* counter helper */
fm_status fmSetVlanCounterID(fm_int sw, fm_uint vlanID, fm_uint vcnt);


#endif /* __FM_FM_API_VLAN_INT_H */
