/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_vlan.h
 * Creation Date:   April 26, 2005
 * Description:     Structures and functions for dealing with 
 *                  VLAN configuration
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

#ifndef __FM_FM_API_VLAN_H
#define __FM_FM_API_VLAN_H

/*****************************************************************************
 * Macros & Constants
 *****************************************************************************/


/** The maximum VLAN number.
 *  \ingroup constSystem */
#define FM_MAX_VLAN      4096


/*****************************************************************************
 * Types
 *****************************************************************************/


/****************************************************************************/
/** \ingroup constSTStates
 *
 * Spanning Tree States, used as an argument in calls to ''fmSetVlanPortState''
 * and ''fmSetSpanningTreePortState''.
 *                                                                      \lb\lb
 * Each state controls the following conditions:                            \lb
 *  Rx BPDU: Whether ingress (recevied) BPDU frames are forwarded to the CPU.   \lb
 *  Rx Traffic: Whether other ingress (recevied) frames are forwarded.          \lb
 *  Tx BPDU: Whether BPDU frames sent from the CPU application are transmitted. \lb
 *  Tx Traffic: Whether other transmitted frames are egressed.                  \lb
 *  Learn: Wether source MAC addresses on received frames are learned.          \lb
 *                                                                          \lb
 * In all states, network management frames (e.g., LACP, 802.1x) are
 * received and transmitted normally between the port and the CPU.
 ****************************************************************************/
enum _fm_stStates
{
    /** Rx BPDU: No                                                         \lb
     *  Rx Traffic: No                                                      \lb
     *  Tx BPDU: No                                                         \lb
     *  Tx Traffic: No                                                      \lb
     *  Learn: No                                                           */
    FM_STP_STATE_DISABLED = 0,

    /** Rx BPDU: Yes                                                        \lb
     *  Rx Traffic: No                                                      \lb
     *  Tx BPDU: Yes                                                        \lb
     *  Tx Traffic: No                                                      \lb
     *  Learn: No                                                           */
    FM_STP_STATE_LISTENING,

    /** Rx BPDU: Yes                                                        \lb
     *  Rx Traffic: No (except on FM2000)                                   \lb
     *  Tx BPDU: Yes                                                        \lb
     *  Tx Traffic: No (except on FM2000)                                   \lb
     *  Learn: Yes                                                          */
    FM_STP_STATE_LEARNING,

    /** Rx BPDU: Yes                                                        \lb
     *  Rx Traffic: Yes                                                     \lb
     *  Tx BPDU: Yes                                                        \lb
     *  Tx Traffic: Yes                                                     \lb
     *  Learn: Yes                                                          */
    FM_STP_STATE_FORWARDING,

    /** Rx BPDU: Yes                                                        \lb
     *  Rx Traffic: No                                                      \lb
     *  Tx BPDU: No                                                         \lb
     *  Tx Traffic: No                                                      \lb
     *  Learn: No                                                           */
    FM_STP_STATE_BLOCKING

};


/**************************************************/
/** \ingroup typeEnum
 *  Used as an argument to ''fmChangeVlanPortExt''
 *  and ''fmGetVlanPortTagExt''. Indicates which 
 *  VLAN field in the packet to consider when 
 *  deciding whether to include or exclude a VLAN 
 *  tag.
 **************************************************/
typedef enum
{
    /** Select VLAN1 */
    FM_VLAN_SELECT_VLAN1 = 0,

    /** Select VLAN2 */
    FM_VLAN_SELECT_VLAN2,

} fm_vlanSelect;


/*****************************************************************************
 * Function prototypes
 *****************************************************************************/


/* functions to mark valid or invalid a particular VLAN */
fm_status fmCreateVlan(fm_int sw, fm_uint16 vlanID);
fm_status fmDeleteVlan(fm_int sw, fm_uint16 vlanID);


/* functions to change membership and egress tagging state */
fm_status fmAddVlanPort(fm_int sw, fm_uint16 vlanID, fm_int port, fm_bool tag);
fm_status fmDeleteVlanPort(fm_int sw, fm_uint16 vlanID, fm_int port);
fm_status fmChangeVlanPort(fm_int    sw,
                           fm_uint16 vlanID,
                           fm_int    port,
                           fm_bool   tag);
fm_status fmChangeVlanPortExt(fm_int        sw,
                              fm_vlanSelect vlanSel,
                              fm_uint16     vlanID,
                              fm_int        port,
                              fm_bool       tag);


/* functions to retrieve a list of valid VLANs and a list of ports per VLAN */
fm_status fmGetVlanList(fm_int     sw,
                        fm_int *   nVlan,
                        fm_uint16 *vlanIDs,
                        fm_int     maxVlans);
fm_status fmGetVlanPortList(fm_int    sw,
                            fm_uint16 vlanID,
                            fm_int *  nPorts,
                            fm_int *  ports,
                            fm_int    maxPorts);


/* SNMP style functionas as per the spec for the above */
fm_status fmGetVlanFirst(fm_int sw, fm_int *firstID);
fm_status fmGetVlanNext(fm_int sw, fm_int startID, fm_int *nextID);
fm_status fmGetVlanPortFirst(fm_int sw, fm_int vlanID, fm_int *firstPort);
fm_status fmGetVlanPortNext(fm_int  sw,
                            fm_int  vlanID,
                            fm_int  startPort,
                            fm_int *nextPort);
fm_status fmGetVlanPortTag(fm_int sw, fm_int vlanID, fm_int port, fm_bool *tag);
fm_status fmGetVlanPortTagExt(fm_int        sw,
                              fm_vlanSelect vlanSel,
                              fm_int        vlanID,
                              fm_int        port,
                              fm_bool      *tag);


/* functions to modify the spanning tree state of a port in a VLAN */
fm_status fmSetVlanPortState(fm_int    sw,
                             fm_uint16 vlanID,
                             fm_int    port,
                             fm_int    state);
fm_status fmGetVlanPortState(fm_int    sw,
                             fm_uint16 vlanID,
                             fm_int    port,
                             fm_int *  state);


/* functions to update a list of ports in a VLAN */
fm_status fmAddVlanPortList(fm_int    sw,
                            fm_uint16 vlanID,
                            fm_int    numPorts,
                            fm_int *  portList,
                            fm_bool   tag);

fm_status fmDeleteVlanPortList(fm_int    sw,
                               fm_uint16 vlanID,
                               fm_int    numPorts,
                               fm_int *  portList);

fm_status fmSetVlanPortListState(fm_int    sw,
                                 fm_uint16 vlanID,
                                 fm_int    numPorts,
                                 fm_int *  portList,
                                 fm_int    state);


/* attribute setting for VLAN settings */
fm_status fmGetVlanAttribute(fm_int    sw,
                             fm_uint16 vlanID,
                             fm_int    attr,
                             void *    value);
fm_status fmSetVlanAttribute(fm_int    sw,
                             fm_uint16 vlanID,
                             fm_int    attr,
                             void *    value);

fm_status fmGetVlanPortAttribute(fm_int    sw,
                                 fm_uint16 vlanID,
                                 fm_int    port,
                                 fm_int    attr,
                                 void *    value);


/* functions to manage customer VLANs for provider bridging */
fm_status fmAddCVlan(fm_int    sw,
                     fm_int    port,
                     fm_uint16 cVlan,
                     fm_uint16 sVlan);
fm_status fmDeleteCVlan(fm_int    sw,
                        fm_int    port,
                        fm_uint16 cVlan);
fm_status fmGetCVlanFirst(fm_int    sw,
                          fm_int *  firstPort,
                          fm_int *  firstCVlan);
fm_status fmGetCVlanNext(fm_int    sw,
                         fm_int     startPort,
                         fm_uint16  startCVlan,
                         fm_int *   nextPort,
                         fm_int *   nextCVlan);

fm_status fmGetSVlanFromPortCVlan(fm_int    sw,
                                  fm_int    port,
                                  fm_uint16 cVlan,
                                  fm_int *  sVlan);

fm_status fmDbgDumpCVlanCounter(fm_int sw);


#endif /* __FM_FM_API_VLAN_H */
