/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces) */
/*****************************************************************************
 * File: fm_api_mcast_groups_int.h
 * Creation Date: October 8, 2007
 * Description: internal header file for multicast group services.
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

#ifndef __FM_FM_API_MCAST_GROUPS_INT_H
#define __FM_FM_API_MCAST_GROUPS_INT_H


typedef struct _fm_mcastAddrKey
{
    fm_vlanLearningMode     vlMode;
    fm_multicastAddress     addr;
    fm_intRouteEntry *      routePtr;

} fm_mcastAddrKey;


/***************************************************
 * Since groups are either in L2 or L3 mode, this
 * enum identifies which.
 **************************************************/
typedef enum _fm_mtableGroupType
{
    FM_MULTICAST_GROUP_TYPE_L2,
    FM_MULTICAST_GROUP_TYPE_L3

} fm_mtableGroupType;


/*****************************************************************************
 *
 * Multicast Group Entry
 *
 *****************************************************************************/
typedef struct _fm_intMulticastGroup
{
    /* The multicast group handle, which must be used to identify the multicast
     * group in all multicast group functions. */
    fm_int              handle;

    /* logical port associated with this group.  If no logical port has been
     * assigned to the group, logicalPort will be set to FM_LOGICAL_PORT_NONE. */
    fm_int              logicalPort;

    /* TRUE if this multicast group uses the older multicast group
     * restrictions: only a single multicast address per group, the address
     * must be specified before group activation, and L2 vs. L3 resources
     * are determined automatically unless the l2-switching-only flag is
     * set. */
    fm_bool             singleAddressMode;

    /* TRUE if the multicast group handle was allocated by the
     * fmCreateMcastGroupInt function, which implies that it must be
     * deallocated by fmDeleteMcastGroup.  Any handles allocated by
     * switch-specific AssignMcastGroup function must be released by
     * the appropriate switch-specific code. */
    fm_bool             localHandle;

    /* TRUE if the logical port (if any) was allocated by this group when
     * the group was activated.  This is used to determine whether to
     * release the logical port when deactivating the group. */
    fm_bool             localLogicalPort;

    /* Pointer to the multicast address set for the group if and only if
     * the group is in single-address mode. */
    fm_mcastAddrKey *   singleMcastAddr;

    /* TRUE if L3 vlan-replication (MTABLE) resources have been assigned to
     * the group. */
    fm_bool             hasL3Resources;

    /* TRUE if the group has been activated. */
    fm_bool             activated;

    /* ECMP Group ID for the group. */
    fm_int              ecmpGroup;

    /* TRUE if the group is supposed to forward frames to the CPU. */
    fm_bool             fwdToCpu;

    /* TRUE if an L3 group is supposed to use switching only.  In other words,
     * the group will not use vlan-replication resources, but will simply
     * switch multicast frames using the ingress vlan. */
    fm_bool             l2SwitchingOnly;

    /* TRUE if an L3 group is supposed to use switching only but with the usage
     * of the vlan-replication table. */
    fm_bool             l3SwitchingOnly;

    /* TRUE if an L3 group is configured to be used as a flood-set. */
    fm_bool             l3FloodSet;

    /* TRUE if an L2 group is configured to be used as a flood-set. */
    fm_bool             l2FloodSet;

    /* TRUE if an L2 group is configured to be used as a VxLan Decap flood-set. */
    fm_bool             l2VxLanDecap;

    /* TRUE if the group is being used to match on ip tunneling VLANS */
    fm_bool             ipTunneling;

    /* TRUE if egress STP state should not be checked prior to adding entries
     * to the replication table. */
    fm_bool             bypassEgressSTPCheck;

    /* TRUE if the group was created using the stacking API */
    fm_bool             stackGroup;

    /* TRUE if the group can update the hardware status */
    fm_bool             updateHardware;

    /* The routing action to be taken for multicast frames */
    fm_routeAction      groupAction;

    /* The routing state for the group. */
    fm_routeState       groupState;

    /* The group's default vlan, only used for L2 groups and for L3 groups
     * with the l2 switching only attribute set. */
    fm_uint16           defaultVlan;

    /* TRUE if this multicast group belongs to a private replication group. */
    fm_bool             privateGroup;

    /* the replication group for the mcast group */
    fm_int              repliGroup;

    /* The MTU index to use with this multicast group. */
    fm_int              mtuIndex;

    /* tree of multicast address keys for the group. */
    fm_customTree       addressTree;

    /* temporary pointer to a route entry pointer. When adding a multicast
     * route, the multicast code stores the pointer to the address-key's
     * route pointer so that the routing code can store the route pointer
     * for later use by the multicast code. */
    fm_intRouteEntry ** routePtrPtr;

    /* pointer to the switch extension structure */
    void *              switchExt;

    /* pointer to additional group information specific to the switch-type */
    void *              extension;

    /* Tree of multicast listeners (fm_intMulticastListener *) indexed by
     * (vlan, port) tuple. */
    fm_tree             listenerTree;

    /* tree containing listener tuples for ports that were listeners before
     * being added to a LAG, which causes them to be removed as listeners
     * (unless the LAG is also a listener).  They will be added back as
     * listeners, and removed from this tree, when they are removed from
     * membership in a LAG. */
    fm_tree             preLagListenerTree;

    /* Head/tail pointers for linked list of internal listeners. */
    FM_DLL_DEFINE_LIST(_fm_intMulticastInternalListener, firstIntListener, lastIntListener);

    /* Tree of multicast ECMP group (fm_intMulticastEcmp *) indexed by
     * ECMP handle. */
    fm_tree             ecmpTree;

    /* TRUE if repliGroup is a read-only resource owned by another multicast
     * group. */
    fm_bool             readOnlyRepliGroup;
} fm_intMulticastGroup;


/*****************************************************************************
 *
 * Multicast Group Listener
 *
 *****************************************************************************/
typedef struct _fm_intMulticastListener
{
    /* The VLAN to which the multicast should be sent. */
    fm_uint16           vlan;

    /* The logical port to which the multicast should be sent. */
    fm_int              port;

    /* Flag to let the pipeline know that this is a remote
     * listener. Use by tunneling protocol, such TRILL, VxLan, etc.
     */ 
    fm_bool   remoteFlag;

    /* Pointer to the internal listener, only if 'port' is a remote port. */
    struct _fm_intMulticastInternalListener *internal;

    /* Pointer to the parent group */
    fm_intMulticastGroup * group;

    /* TRUE if the listener has been added at the chip level */
    fm_bool             addedToChip;

    /* Head/tail pointers for linked list of sub-listeners. */
    FM_DLL_DEFINE_LIST(_fm_intMulticastListener, firstSubListener, lastSubListener);

    /* Next/previous pointers for linked list of sub-listeners. */
    FM_DLL_DEFINE_NODE(_fm_intMulticastListener, nextSubListener, prevSubListener);

} fm_intMulticastListener;


/*****************************************************************************
 *
 * Multicast Group Internal Listener
 *
 *****************************************************************************/
typedef struct _fm_intMulticastInternalListener
{
    /* The internal port number. */
    fm_int port;

    /* The number of remote listeners served by this internal port. */
    fm_int listenerCount;

    /* Next/previous pointers for linked list of internal listeners. */
    FM_DLL_DEFINE_NODE(_fm_intMulticastInternalListener, nextListener, prevListener);

} fm_intMulticastInternalListener;


/*****************************************************************************
 *
 * Multicast Group Internal ECMP
 *
 *****************************************************************************/
typedef struct _fm_intMulticastEcmp
{
    /* The lbs vlan */
    fm_int vlan;

    /* The ECMP ID */
    fm_int ecmpId;

} fm_intMulticastEcmp;


/*****************************************************************************
 * Manipulate linked list of Internal Listeners.
 *****************************************************************************/
#define fmGetFirstMcastInternalListener(groupPtr) \
    FM_DLL_GET_FIRST(groupPtr, firstIntListener)

#define fmGetLastMcastInternalListener(groupPtr) \
    FM_DLL_GET_LAST(groupPtr, lastIntListener)

#define fmGetNextMcastInternalListener(listener) \
    FM_DLL_GET_NEXT(listener, nextListener)

#define fmGetPreviousMcastInternalListener(listener) \
    FM_DLL_GET_PREVIOUS(listener, prevListener)

#define fmAppendMcastInternalListener(groupPtr, newListener)          \
    FM_DLL_INSERT_LAST(groupPtr, firstIntListener, lastIntListener,   \
                       newListener, nextListener, prevListener)

#define fmRemoveMcastInternalListener(groupPtr, internalListener)     \
    FM_DLL_REMOVE_NODE(groupPtr, firstIntListener, lastIntListener,   \
                       internalListener, nextListener, prevListener)

/*****************************************************************************
 * Manipulate linked list of Sub-listeners.
 *****************************************************************************/
#define fmAppendMcastSubListener(listenerPtr, subListenerPtr)           \
    FM_DLL_INSERT_LAST(listenerPtr, firstSubListener, lastSubListener,  \
                       subListenerPtr, nextSubListener, prevSubListener)

#define fmRemoveMcastSubListener(listenerPtr, subListenerPtr)           \
    FM_DLL_REMOVE_NODE(listenerPtr, firstSubListener, lastSubListener,  \
                       subListenerPtr, nextSubListener, prevSubListener)

fm_intMulticastGroup *fmFindMcastGroup(fm_int sw, fm_int handle);
fm_intMulticastGroup *fmFindMcastGroupByPort(fm_int sw, fm_int port);
void fmGetMcastDestAddress(fm_multicastAddress *multicast, fm_ipAddr *destAddr);
fm_status fmApplyMasksToMulticastAddress(fm_multicastAddress *multicast);

fm_status fmMcastGroupInit(fm_int sw);
fm_status fmCreateMcastGroupInt(fm_int sw, fm_int *mcastGroup, fm_bool stacking );

fm_status fmAllocateMcastGroupsInt(fm_int    sw,
                                   fm_uint    startGlort,
                                   fm_uint    glortSize,
                                   fm_int    *baseMcastGroup,
                                   fm_int    *numMcastGroups,
                                   fm_int    *step);
fm_status fmFreeMcastGroupsInt(fm_int sw, fm_int baseMcastGroup);
fm_status fmFreeMcastGroupDataStructures(fm_switch *switchPtr);

fm_status fmMcastAddPortToLagNotify(fm_int sw, fm_int lagIndex, fm_int port);
fm_status fmMcastRemovePortFromLagNotify(fm_int sw, fm_int lagIndex, fm_int port);
fm_status fmMcastDeleteLagNotify(fm_int sw, fm_int lagIndex);
fm_status fmMcastDeleteVlanNotify(fm_int sw, fm_int vlan);
fm_status fmClearMcastGroupAddressesInt(fm_int                sw,
                                        fm_intMulticastGroup *group);

fm_status fmMcastBuildMacEntry(fm_int                  sw,
                               fm_intMulticastGroup *  group,
                               fm_multicastMacAddress *addr,
                               fm_macAddressEntry *    macEntry,
                               fm_int *                trigger);

fm_status fmRewriteMcastGroupMacAddresses(fm_int                sw,
                                          fm_intMulticastGroup *group);

fm_status fmSetMcastGroupRouteActiveFlags(fm_int                sw,
                                          fm_intMulticastGroup *group,
                                          fm_routeState         routeState);

fm_status fmGetMcastGroupUsedInt(fm_int   sw,
                                 fm_int   mcastGroup,
                                 fm_bool *used,
                                 fm_bool  resetFlag);

#endif
