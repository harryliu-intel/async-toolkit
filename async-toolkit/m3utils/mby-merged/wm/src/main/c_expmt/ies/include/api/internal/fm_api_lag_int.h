/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_lag_int.h
 * Creation Date:   2005
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

#ifndef __FM_FM_API_LAG_INT_H
#define __FM_FM_API_LAG_INT_H

/** The maximum number of separate glort ranges that can be allocated for
 *  Link Aggregation Groups.  In other words, this is the number of times that
 *  fmAllocateStackLAGs can be called for a given switch. */
#define FM_ALLOC_LAGS_MAX                   4

/* Indicates the port is unsed */
#define FM_LAG_UNUSED_PORT          -1

#define FM_LAG_VLAN_MEMBERSHIP      (1 << 0)
#define FM_LAG_VLAN_TAG             (1 << 1)
#define FM_LAG_VLAN2_TAG            (1 << 2)
#define FM_LAG_VLAN_STP_LOW_BIT     3
#define FM_LAG_VLAN_STP_MASK        (7 << FM_LAG_VLAN_STP_LOW_BIT)


#define LAG_INDEX_OUT_OF_BOUNDS(lagIndex)   \
    ( (lagIndex) < 0 || (lagIndex) >= FM_MAX_NUM_LAGS )

/**
 * Returns a LAG pointer given a switch number and LAG index. 
 *  
 * \param[in]   sw is the switch on which to operation. 
 *              Assumed to be valid. 
 *  
 * \param[in]   lagIndex is the index of the LAG on the switch. 
 *              Assumed to be valid. 
 *  
 * \return      Pointer to the LAG structure, or null if the 
 *              specified LAG does not exist.
 */
#define GET_LAG_PTR(sw, lagIndex) \
    GET_SWITCH_PTR(sw)->lagInfoTable.lag[lagIndex]

/**
 * Returns a LAG extension pointer given a switch number and LAG index. 
 *  
 * \param[in]   sw is the switch on which to operate.
 *  
 * \param[in]   lagIndex is the index of the LAG on the switch. 
 *              Assumed to be valid.
 *  
 * \return      Pointer to the LAG extension structure.
 */
#define GET_LAG_EXT(sw, lagIndex) \
    GET_LAG_PTR(sw, lagIndex)->extension

/**
 * Returns a LAG info pointer given a switch number. 
 *  
 * \param[in]   sw is the switch on which to operation. 
 *              Assumed to be valid. 
 *  
 * \return      Pointer to the LAG info structure.
 */
#define GET_LAG_INFO_PTR(sw) \
    &GET_SWITCH_PTR(sw)->lagInfoTable

/**
 * Returns a port pointer given a switch pointer and port number. 
 *  
 * \param[in]   switchPtr points to the switch structure. May not be null.
 *  
 * \param[in]   port is the logical port number. 
 *  
 * \return      Pointer to the port structure, or null if the port number 
 *              is invalid or the port does not exist.
 */
#define GET_SWITCH_PORT_PTR(switchPtr, port) \
    ((port) >= 0 && (port) < (switchPtr)->maxPort) ? \
    (switchPtr)->portTable[port] : \
    NULL

/**
 * Returns a LAG pointer given a switch pointer and LAG index. 
 *  
 * \param[in]   switchPtr points to the switch structure. May not be null.
 *  
 * \param[in]   lagIndex is the index of the LAG on the switch. 
 *  
 * \return      Pointer to the LAG structure, or null if the LAG index is 
 *              invalid or the specified LAG does not exist.
 */
#define GET_SWITCH_LAG_PTR(switchPtr, lagIndex) \
    !LAG_INDEX_OUT_OF_BOUNDS(lagIndex) ? \
    (switchPtr)->lagInfoTable.lag[lagIndex] : \
    NULL

/**
 * Determines whether a port is a member of a LAG. 
 *  
 * \param[in]   portPtr points to the port structure. May not be null. 
 *  
 * \return      TRUE if the port is a member of a lag
 * \return      FALSE if the port is not a member of a lag
 */
#define FM_IS_PORT_IN_A_LAG(portPtr) \
    ((portPtr)->lagIndex >= 0 && (portPtr)->memberIndex >= 0)

#define FM_NUM_ROTATION  2

typedef enum
{
    /* This enum must not be changed. ROT_A must be 0 and ROT_B must be 1 */
    FM_HASH_ROTATION_A = 0,
    FM_HASH_ROTATION_B = 1

} fm_hashRotation;


/* Structure to store enough info to remap handle return
 * from lower layer to lag number to return to user
 * when the user is doing LAG allocation. */
typedef struct 
{
    /* The base lag index used for this allocation resource */
    fm_int         baseLagIndex;

    /* If numLags == 0, then this entry is invalid */
    fm_int         numLags;

    /* The base handle returned from lower layer */
    fm_int         baseHandle;
    
    /* step to increment the base to get next handle */
    fm_int         step;

} fm_allocLags;


/* global structure to hold LAG entries */
typedef struct
{
    /* This holds the LAG structure for each LAG */
    fm_lag *   lag[FM_MAX_NUM_LAGS];

    /* Keeps track of if LAG is being used or reserved */
    fm_byte    resvLag[FM_MAX_NUM_LAGS];

    /* Keeps track of all LAGs allocations */
    fm_allocLags allocLags[FM_ALLOC_LAGS_MAX];

    /* Keeps track of which ports are up and which are down.
     * This is useful for making sure that we assign only
     * ports that are "up" as canonical ports.
     * NOTE: Need to find a better way to save this info
     * since the array is large for large number of ports
     * Maybe don't need to save this and get directly from
     * the portTable structure.
     */
    fm_bool    portIsUp[FM_MAX_LOGICAL_PORT + 1];

    /* Indicates whether LACP frames trapped to the CPU port will
     * be dropped by the API or forwarded to the application. */
    fm_bool    dropLACPFrames;

    /* indicates what LAG mode we are in, static or dynamic, the difference
     * being whether we will send LACP frames to the application or just
     * drop them. This is redundant with dropLACPFrames. */
    fm_lagMode lagMode;

    /* Only uses filtering and not pruning */
    fm_bool    pruningDisabled;

} fm_lag_info;


typedef struct
{
    /* Logical port number of this member, or FM_LAG_UNUSED_PORT if unused. */
    fm_int  port;

    /* TRUE if the member is active; FALSE otherwise. */
    fm_bool isActive;

} fm_lagMember;


struct _fm_lag
{
    /* This LAG's index into LAG array in fm_lag_info */
    fm_int  index;

    /* Logical port number assigned to this LAG */
    fm_int  logicalPort;

    /* Ports that have been added to this LAG. */
    fm_lagMember member[FM_MAX_NUM_LAG_MEMBERS];

    /* Indicate that this LAG is an internal LAG within a stack. */
    fm_bool isInternalPort;

    /* Indicate whether LAG filtering is enable on this LAG. */
    fm_bool filteringEnabled;

    /* pointer to additional lag information specific to the lag type */
    void *  extension;

    /* Pointer to semaphore used when synchronous lag deletion is used */
    fm_semaphore *deleteSemaphore;

    /* List of VLANs of which the LAG is a member */
    fm_byte *vlanMembership;

    /* Selects the hash rotation for this LAG (A or B). */
    fm_hashRotation hashRotation;

};

/*****************************************************************************
 * Macros to determine whether a LAG is in use or reserved.
 *****************************************************************************/

/* unreserve must ensure LAG is not in use first, just clear everything */
#define FM_UNRESERVE_LAG(info, index)   (info)->resvLag[index] = 0
#define FM_SET_LAG_FREE(info, index)    (info)->resvLag[index] &= ~0x1
#define FM_SET_LAG_IN_USE(info, index)  (info)->resvLag[index] |= 0x1
#define FM_RESERVE_LAG(info, index)     (info)->resvLag[index] |= 0x2

/* Either in use or reserved */
#define FM_IS_LAG_TAKEN(info, index)    ((info)->resvLag[index])

/* Check for both free and reserved */
#define FM_IS_LAG_IN_USE(info, index)   ((info)->resvLag[index] & 0x1)
#define FM_IS_LAG_RESERVED(info, index) ((info)->resvLag[index] & 0x2)

/*****************************************************************************
 * Internal functions used only within the API code.
 *****************************************************************************/

fm_status fmCreateLAGInt(fm_int sw, fm_int *lagNumber, fm_bool stacking);
fm_status fmAllocateLAGsInt(fm_int     sw,
                            fm_uint    startGlort,
                            fm_uint    glortSize,
                            fm_int    *baseLagIndex,
                            fm_int    *numLags,
                            fm_int    *step);
fm_status fmFreeLAGsInt(fm_int sw, fm_int baseLagIndex);

fm_status fmDeleteLagCallback(fm_int sw, struct _fm_lag *lagPtr);

void fmFreeLAG(fm_int sw, fm_int lagIndex);

fm_int fmGetLagIndex(fm_int sw, fm_int lagNumber);
fm_int fmGetLagLogicalPort(fm_int sw, fm_int lagIndex);

fm_int fmGetPortLagIndex(fm_int sw, fm_int port);
fm_int fmGetPortMemberIndex(fm_int sw, fm_int port);
fm_bool fmIsValidLagPort(fm_int sw, fm_int port);

fm_status fmLagIndexToLogicalPort(fm_int    sw,
                                  fm_int    lagIndex,
                                  fm_int*   logicalPort);

fm_status fmLogicalPortToLagIndex(fm_int    sw,
                                  fm_int    port,
                                  fm_int*   lagIndex);

fm_bool fmPortIsInALAG(fm_int sw, fm_int port);
fm_bool fmPortIsInLAG(fm_int sw, fm_int port, fm_int lagIndex);
fm_int fmCountLAGMembers(fm_int sw, fm_int lagIndex);
fm_int fmCountActiveLAGMembers(fm_int sw, fm_int lagIndex);

fm_status fmAddLAGMember(fm_int sw, fm_int lagIndex, fm_int port);
fm_status fmAddLAGActiveMember(fm_int sw, fm_int lagIndex, fm_int port);
fm_status fmRemoveLAGMember(fm_int sw, fm_int lagIndex, fm_int port);
fm_status fmDeactivateLagMember(fm_int sw, fm_int lagIndex, fm_int port);

fm_status fmGetLAGMemberPorts(fm_int sw, 
                              fm_int lagIndex,
                              fm_int *numPorts,
                              fm_int *portList,
                              fm_int maxPorts,
                              fm_bool active);

fm_status fmGetLAGMemberPortsForPort(fm_int sw,
                                     fm_int memberPort,
                                     fm_int *numPorts,
                                     fm_int *portList,
                                     fm_int maxPorts);

fm_status fmGetFirstPhysicalMemberPort(fm_int sw,
                                       fm_int port,
                                       fm_int *physMember);

fm_status fmGetPhysicalMemberPortList(fm_int sw,
                                      fm_int port,
                                      fm_int *numMembers,
                                      fm_int *members,
                                      fm_int maxPorts);

fm_status fmGetRemotePortDestMask(fm_int        sw,
                                  fm_int        port,
                                  fm_portmask * destMask,
                                  fm_portmask * activeDestMask);

fm_status fmGetLAGMemberIndex(fm_int sw, fm_int memberPort, fm_int *memberIndex);

/* initializes state of LAG table */
fm_status fmInitLAGTable(fm_int sw);


/* free lag structure components */
fm_status fmFreeLAGTable(fm_int sw);


/* Called to inform the LAG code that the state of a port is
 * no longer "up".  If the port was an active member of
 * and LAG, it will be set to inactive.
 */
fm_status fmInformLAGPortDown(fm_int sw, fm_int port);


/* Called to inform the LAG code that the state of a port is
 * now "up".  If this port was an inactive member of any
 * LAG, it will be reactivated.
 */
fm_status fmInformLAGPortUp(fm_int sw, fm_int port);

fm_status fmCheckLACPFilter(fm_int sw, fm_eventPktRecv *event, fm_bool *filter);

fm_int fmGetLAGForPort(fm_int sw, fm_int port);

fm_status fmApplyLAGSettingsToPort(fm_int sw, fm_int port, fm_int lagIndex);

fm_status fmSetLAGVlanMembership(fm_int    sw, 
                                 fm_uint16 vlanID, 
                                 fm_int    port, 
                                 fm_bool   state,
                                 fm_bool   tag);

fm_status fmSetLAGVlanPortState(fm_int    sw, 
                                fm_uint16 vlanID, 
                                fm_int    port, 
                                fm_int    state);

fm_status fmGetLAGVlanPortState(fm_int    sw, 
                                fm_uint16 vlanID, 
                                fm_int    port, 
                                fm_int *  state);

fm_status fmSetLAGVlanTag(fm_int        sw, 
                          fm_vlanSelect vlanSel,
                          fm_uint16     vlanID, 
                          fm_int        port, 
                          fm_bool       tag);

fm_status fmGetLAGVlanTag(fm_int        sw, 
                          fm_vlanSelect vlanSel,
                          fm_uint16     vlanID, 
                          fm_int        port, 
                          fm_bool      *tag);

fm_status fmLAGPortAddDelete(fm_int sw, fm_int lagIndex);

fm_status fmGetLAGVlanAttribute(fm_int    sw,
                                fm_uint16 vlanID,
                                fm_int    port,
                                fm_int    attr,
                                void *    value);

fm_status fmSetLagListVlanMembership(fm_int     sw,
                                     fm_uint16  vlanID,
                                     fm_int     numPorts,
                                     fm_int *   portList,
                                     fm_bool    state,
                                     fm_bool    tag);

fm_status fmSetLagListVlanPortState(fm_int     sw,
                                    fm_uint16  vlanID,
                                    fm_int     numPorts,
                                    fm_int *   portList,
                                    fm_int     state);

#endif /* __FM_FM_API_LAG_INT_H */
