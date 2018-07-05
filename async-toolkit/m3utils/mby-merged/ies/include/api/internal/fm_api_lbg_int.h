/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_lbg_int.h
 * Creation Date:   March 31, 2008
 * Description:     Prototypes for managing load balancing groups.  This
 *                  API is not valid on FM2000 deviecs.  This header
 *                  defines the internal helper functions related to
 *                  load balancing groups.
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

#ifndef __FM_FM_API_LBG_INT_H
#define __FM_FM_API_LBG_INT_H

#define FM_MAX_PORTS_FOR_NON_MAPPED_MODE        16
#define FM_DEST_ENTRIES_FOR_NON_MAPPED_MODE     16
#define FM_LOGICAL_PORTS_FOR_NON_MAPPED_MODE    1

#define FM_DEST_ENTRIES_FOR_MAPPED_MODE         256
#define FM_MAX_PORTS_FOR_MAPPED_MODE            20
#define FM_ARP_ENTRIES_FOR_MAPPED_MODE          16
#define FM_LOGICAL_PORTS_FOR_MAPPED_MODE        16

#define FM_DEST_ENTRIES_PER_LOGICAL_PORT        16


typedef enum
{
    /** The load balancer group's RouteData is an ARP table index. */
    FM_LBG_ROUTE_ARP = 0,

    /** The load balancer group's RouteData is a GloRT. */
    FM_LBG_ROUTE_GLORT

} fm_LBGRouteType;


/***************************************************
 * This structure contains the state information
 * for a single member of a load balancing group.
 **************************************************/
typedef struct _fm_LBGMember
{
    /* The logical port number of the member. */
    fm_int  lbgMemberPort;

    /* The cardinal port mask of the member port.
     * Will be zero if the member is a remote port.
     * (FM4000 only) */
    fm_uint32 memberPortMask;

    /* Port mode, specified via the FM_LBG_PORT_MODE port attribute
     * and the _fm_lbgPortMode enumeration. */
    fm_int  mode;

    /* Logical port number to which traffic is to be redirected.
     * Set via the FM_LBG_PORT_REDIRECT_TARGET port attribute. */
    fm_int  redirectTarget;

    /* Whether this port is used in standby mode */
    fm_bool standbyUsed;

    /* Points to the member for the target, if any */
    struct _fm_LBGMember *redirectTargetPtr;

    /* Points to the LBG group this port belongs to */
    struct _fm_LBGGroup *group;

    /* LBG member lists */
    FM_DLL_DEFINE_NODE(_fm_LBGMember, nextMember, prevMember);

    /* Global member lists */
    FM_DLL_DEFINE_NODE(_fm_LBGMember, nextGlobalMember, prevGlobalMember);

} fm_LBGMember;

/***************************************************
 * This structure contains the state information
 * for a single load balancing group.
 **************************************************/
typedef struct _fm_LBGGroup
{
    /* The logical port number of this group */
    fm_int                lbgPort;

    /* The number of bins to be used */
    fm_int                numBins;

    /* The current mode for this LBG */
    fm_int                lbgMode;

    /* Redirect mode */
    fm_int                redirectMode;

    /* Whether redirect mode is set on the LBG */
    fm_bool               redirectModeSet;

    /* The state for this LBG */
    fm_int                state;

    /* The VLAN associated with this LBG */
    fm_int                vlan;

    /* The configured mapped size */
    fm_int                mapSize;

    /* The member list */
    FM_DLL_DEFINE_LIST(_fm_LBGMember, firstMember, lastMember);

    /* The member count */
    fm_int                numMembers;

    /* Chip specific state structure */
    void *                extension;

} fm_LBGGroup;

/***************************************************
 * This structure maintains all of the soft-state of
 * the load balancing group.
 **************************************************/
typedef struct
{
    /* A tree of LBGs keyed by their logical port */
    fm_tree    groups;

    /* The current global mode */
    fm_lbgMode mode;

    /* Global lock state */
    fm_bool    globalLock;

    /* Accessor lock */
    fm_lock    lbgLock;

    /* The member list */
    FM_DLL_DEFINE_LIST(_fm_LBGMember, firstMember, lastMember);

} fm_LBGInfo;

fm_status fmGetLBGRouteData(fm_int           sw,
                            fm_int           lbgNumber,
                            fm_LBGRouteType *routeType,
                            fm_int *         routeData,
                            fm_int *         dataCount);


fm_status fmGetLBGUnlock(fm_int sw, fm_int lbgNumber);

fm_status fmAllocateLBGDataStructures(fm_switch *switchPtr);
fm_status fmInitLBG(fm_switch *switchPtr);
fm_status fmFreeLBGDataStructures(fm_switch *switchPtr);
fm_status fmCreateLBGInt(fm_int sw, 
                         fm_int *lbgNumber, 
                         fm_LBGParams *params, 
                         fm_bool stacking);
fm_status fmAllocateLBGsInt(fm_int     sw,
                            fm_uint    startGlort,
                            fm_uint    glortSize,
                            fm_int    *baseLbgNumber,
                            fm_int    *numLbgs,
                            fm_int    *step);
fm_status fmFreeLBGsInt(fm_int sw, fm_int baseLbgNumber);



#endif /* __FM_FM_API_LBG_INT_H */
