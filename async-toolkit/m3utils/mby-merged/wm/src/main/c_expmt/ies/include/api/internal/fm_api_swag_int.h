/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_swag_int.h
 * Creation Date:   April 07, 2008
 * Description:     Internal definitions related to switch aggregates
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

#ifndef __FM_FM_API_SWAG_INT_H
#define __FM_FM_API_SWAG_INT_H


/* If the customer platform failed to define the allocation array sizes,
 * do it now. */
#ifndef FM_SWAG_MAX_MCAST_GROUP_ALLOCS
#define FM_SWAG_MAX_MCAST_GROUP_ALLOCS 1
#endif

#ifndef FM_SWAG_MAX_LAG_ALLOCS
#define FM_SWAG_MAX_LAG_ALLOCS 1
#endif

#ifndef FM_SWAG_MAX_LBG_ALLOCS
#define FM_SWAG_MAX_LBG_ALLOCS 1
#endif

#ifndef FM_SWAG_MAX_HASH_PROFILES
#define FM_SWAG_MAX_HASH_PROFILES 16
#endif


/***************************************************
 * This structure holds information about a link
 * between switches in a switch aggregate. A trunk may
 * be a single link or a link aggregation group
 * of links.
 **************************************************/
typedef struct _fm_swagIntLink
{
    /* link information */
    fm_swagLink              link;

    /* Whether the link is enabled (TRUE), or disabled (FALSE - default) */
    fm_bool                  enabled;

    /* The switch aggregate to which this link belongs */
    struct _fmSWAG_switch *  ext;

    /* The trunk to which this link belongs. */
    struct _fm_swagIntTrunk *trunk;

    /* forward/backward pointers */
    FM_DLL_DEFINE_NODE(_fm_swagIntLink, nextLink,      prevLink);
    FM_DLL_DEFINE_NODE(_fm_swagIntLink, nextTrunkLink, prevTrunkLink);

} fm_swagIntLink;


/***************************************************
 * This structure holds information about a trunk
 * between switches in a switch aggregate. A trunk may
 * be a single link or a link aggregation group
 * of links.
 **************************************************/
typedef struct _fm_swagIntTrunk
{
    /* Trunk information. */
    fm_swagTrunk trunk;

    /* Forwarding Rule IDs. Subscript is the switch number for the other
     * switches in the aggregate, value is the forwarding rule id number
     * used by this switch to specify the glort range and logical port used
     * by this switch in order to send traffic to that switch.
     */
    fm_int       forwardingRuleIds[FM_MAX_NUM_SWITCHES];

    /* A linked-list of pointers to the links which comprise the trunk. */
    FM_DLL_DEFINE_LIST(_fm_swagIntLink, firstLink, lastLink);

    /* forward/backward pointers */
    FM_DLL_DEFINE_NODE(_fm_swagIntTrunk, nextTrunk, prevTrunk);

} fm_swagIntTrunk;


/**************************************************
 *  Switch Aggregate Local Switch Glort Space Information
 **************************************************/
typedef struct _fmSWAG_switchGlort
{
    fm_bool inUse;
    fm_uint base;
    fm_uint mask;
    fm_uint mcastStartGlort;
    fm_uint mcastGlortCount;
    fm_int  mcastBaseGroup;
    fm_int  mcastGroupCount;
    fm_int  mcastGroupStep;
    fm_uint lagStartGlort;
    fm_uint lagGlortCount;
    fm_int  lagBaseGroup;
    fm_int  lagGroupCount;
    fm_int  lagGroupStep;
    fm_uint lbgStartGlort;
    fm_uint lbgGlortCount;
    fm_int  lbgBaseGroup;
    fm_int  lbgGroupCount;
    fm_int  lbgGroupStep;

} fmSWAG_switchGlort;


/**************************************************
 *  Switch Aggregate Forwarding Rule Information
 **************************************************/
typedef struct _fmSWAG_forwardingRule
{
    fm_forwardRule origRule;
    fm_int         realSw;
    fm_int         realPort;
    fm_int         realId[FM_MAX_NUM_SWITCHES];

} fmSWAG_forwardingRule;


/***************************************************
 * This structure holds information about a single
 * member switch in a switch aggregate.
 **************************************************/
typedef struct _fm_swagMember
{
    /* The switch identifier. */
    fm_int                swId;

    /* The switch's role in the switch aggregate. Set to
     * ''FM_SWITCH_ROLE_UNDEFINED'' if structure is unused.
     */
    fm_switchRole         role;

    /* Whether the switch is to be used as a masterCpu connection, allowing
     * for receipt of broadcast traffic, etc.
     */
    fm_bool               masterCpu;

    /* Application-desired state of this switch (TRUE=up, FALSE=down) */
    fm_bool               appState;

    /* GLORT Assignments for this switch */
    fmSWAG_switchGlort *  glortSpace;

    /* The number of trunks on the switch. */
    fm_int                numTrunks;

    /* Array of Link-Agg groups.  Subscript is switch-aggregate lag number,
     * content is underlying switch lag number.
     */
    fm_int                lagGroups[FM_MAX_NUM_LAGS];
    fm_int                lagGroupPortCount[FM_MAX_NUM_LAGS];
    fm_bool               lagGroupActive[FM_MAX_NUM_LAGS];

    /* Array of SWAG Link-Agg groups sorted by underlying switch lag number. */
    fm_int                swagLagGroups[FM_MAX_NUM_LAGS];

    /* Array of LBG Numbers.  Subscript is switch-aggregate LBG number,
     * content is underlying switch LBG number.
     */
    fm_int                lbgGroups[FM_MAX_LBGS_PER_SWAG];
    fm_int                lbgGroupPortCount[FM_MAX_LBGS_PER_SWAG];
    fm_bool               lbgGroupActive[FM_MAX_LBGS_PER_SWAG];

    /* Array of multicast group numbers.  Subscript is switch-aggregate
     * multicast group number, content is underlying switch multicast
     * group number.
     */
    fm_int                mcastGroups[FM_MAX_MCAST_GROUPS_PER_SWAG];
    fm_int                mcastGroupListenerCount[FM_MAX_MCAST_GROUPS_PER_SWAG];
    fm_bool               mcastGroupActive[FM_MAX_MCAST_GROUPS_PER_SWAG];

    /* Array of Storm-Controllers.  Subscript is switch-aggregate storm
     * controller number, content is underlying switch storm controller
     * number.  Content == -1 means not assigned.
     */
    fm_int                stormControllers[FM_MAX_NUM_STORM_CTRL];

    /* Array of ECMP Groups.  Subscript is switch-aggregate ECMP group
     * number, content is underlying switch ECMP Group number.  Content
     * == -1 means not assigned.
     */
    fm_int                ecmpGroups[FM_MAX_ARPS];

    /* Array of trunks for inter-switch communication.  Subscript is the
     * switch number for the other switches in the aggregate.
     */
    fm_swagIntTrunk *     switchTrunks[FM_MAX_NUM_SWITCHES];

    /* Array of remote ports.  Subscripts are the remote switch # and
     * port number within that remote switch, value is the remote port #
     * used to describe that port for this switch.
     */
    fm_int                remotePorts[FM_MAX_NUM_SWITCHES]
                            [FM_MAX_LOCAL_PORTS_PER_SWAG_SWITCH+1];

    /* Tree containing the information on the remote port associated
     * with a Glort for each of the physical switch in the SWAG 
     */
    fm_tree               stackedRemotePorts;

    /* forward/backward pointers */
    FM_DLL_DEFINE_NODE(_fm_swagMember, nextSwitch, prevSwitch);

} fm_swagMember;


/**************************************************
 *  Switch Aggregate Port Table Extension Definition
 **************************************************/
typedef struct _fmSWAG_port
{
    /*
     * pointer to the generic port structure,
     * kept here for cross-referencing purposes
     */
    fm_port *        base;

    /* physical switch number */
    fm_int           sw;

    /* logical port number within the physical switch */
    fm_int           logicalPort;

    /* Link Type */
    fm_swagLinkType  linkType;

    /* Pointer to the link description */
    fm_swagIntLink * linkPtr;

    /* Pointer to the trunk description */
    fm_swagIntTrunk *trunk;

    /* Port Mode */
    fm_int           mode;

    /* Array of member switch logical ports, subscript is member switch number,
     * value is the logical port used on that member switch */
    fm_int           memberPorts[FM_MAX_NUM_SWITCHES];

} fmSWAG_port;


/**************************************************
 *  Switch Aggregate IP Interface Extension Definition
 **************************************************/
typedef struct _fmSWAG_ipInterface
{
    /*
     * pointer to the generic IP Interface structure,
     * kept here for cross-referencing purposes
     */
    fm_intIpInterfaceEntry *base;

    /* cross-reference for each switch in the aggregate.  Subscript is the
     * underlying switch number, value is the interface number assigned by
     * that switch.
     */
    fm_int                  ifNums[FM_MAX_NUM_SWITCHES];

} fmSWAG_ipInterface;


/**************************************************
 *  Switch Aggregate Storm Controller Condition Information
 **************************************************/
typedef struct _fmSWAG_stormCtrlCondition
{
    fm_stormCondition condition;

    FM_DLL_DEFINE_NODE(_fmSWAG_stormCtrlCondition, nextCond, prevCond);

} fmSWAG_stormCtrlCondition;


/**************************************************
 *  Switch Aggregate Storm Controller Action Information
 **************************************************/
typedef struct _fmSWAG_stormCtrlAction
{
    fm_stormAction action;

    FM_DLL_DEFINE_NODE(_fmSWAG_stormCtrlAction, nextAction, prevAction);

} fmSWAG_stormCtrlAction;


/**************************************************
 *  Switch Aggregate Storm Controller Information
 **************************************************/
typedef struct _fmSWAG_stormCtrl
{
    fm_bool   inUse;
    fm_uint32 capacity;
    fm_uint32 rate;
    fm_uint64 count;
    FM_DLL_DEFINE_LIST(_fmSWAG_stormCtrlCondition, firstCond, lastCond);
    FM_DLL_DEFINE_LIST(_fmSWAG_stormCtrlAction, firstAction, lastAction);

} fmSWAG_stormCtrl;

/* TODO: this should be the max over the MTU tables of all sub-switches */
#define FM_MAX_MTU_ENTRIES  8


/**************************************************************************
 * State structures to keep track of the state of the logical ports.
 **************************************************************************/
typedef struct _fmSWAG_allocMcastGroups
{
    fm_uint          glort;
    /* If glortSize == 0, then this entry is invalid */
    fm_uint          glortSize;
    fm_int           baseHandle;
    fm_int           numHandles;
    fm_int           handleStep;
    fm_int           memberBaseHandles[FM_MAX_NUM_SWITCHES];
    fm_int           memberNumHandles[FM_MAX_NUM_SWITCHES];
    fm_int           memberHandleSteps[FM_MAX_NUM_SWITCHES];
 
} fmSWAG_allocMcastGroups;


typedef struct _fmSWAG_allocLags
{
    fm_uint          glort;
    /* If glortSize == 0, then this entry is invalid */
    fm_uint          glortSize;
    fm_int           baseHandle;
    fm_int           numHandles;
    fm_int           handleStep;
    fm_int           memberBaseHandles[FM_MAX_NUM_SWITCHES];
    fm_int           memberNumHandles[FM_MAX_NUM_SWITCHES];
    fm_int           memberHandleSteps[FM_MAX_NUM_SWITCHES];

} fmSWAG_allocLags;


typedef struct _fmSWAG_allocLbgs
{
    fm_uint          glort;
    /* If glortSize == 0, then this entry is invalid */
    fm_uint          glortSize;
    fm_int           baseHandle;
    fm_int           numHandles;
    fm_int           handleStep;
    fm_int           memberBaseHandles[FM_MAX_NUM_SWITCHES];
    fm_int           memberNumHandles[FM_MAX_NUM_SWITCHES];
    fm_int           memberHandleSteps[FM_MAX_NUM_SWITCHES];

} fmSWAG_allocLbgs;


typedef struct _fmSWAG_logicalPortInfo
{
    /* store preallocated glorts/ports for multicast entries */
    fmSWAG_allocMcastGroups allocMcastGroupsEntry[FM_SWAG_MAX_MCAST_GROUP_ALLOCS];

    /* store preallocated LAGs created by AllocateLAGs */
    fmSWAG_allocLags        allocLagsEntry[FM_SWAG_MAX_LAG_ALLOCS];

    /* store preallocated LBGs created by fmAllocateLBGs */
    fmSWAG_allocLbgs        allocLbgsEntry[FM_SWAG_MAX_LBG_ALLOCS];

} fmSWAG_logicalPortInfo;


/**************************************************
 *  Switch Aggregate Switch Table Extension Definition
 **************************************************/
typedef struct _fmSWAG_switch
{
    /* ptr to generic switch struct, kept here for cross-referencing purposes */
    fm_switch *              base;

    /* The switch aggregate topology (see 'fm_swagTopology'). */
    fm_swagTopology          topology;

    /* Pointer to the topology solver */
    fm_swagTopologySolver    solver;

    /* A table to cross-reference switch-aggregate logical port number to
     * underlying switch/logical port pair. */

    /* A list of switches in the switch aggregate (see 'fm_swagMember'). */
    FM_DLL_DEFINE_LIST(_fm_swagMember, firstSwitch, lastSwitch);

    /* The number of switches in the switch aggregate */
    fm_int                   numSwitches;

    /* The configured "masterCpu" switch */
    fm_int                   masterCpuSw;

    /* Glort space allocations for the entire switch aggregate */
    fm_uint                  baseGlort;
    fm_uint                  glortMask;
    fm_bool                  localLagsAllocated;
    fm_bool                  localMcastGroupsAllocated;
    fm_bool                  localLbgsAllocated;

    /* Logical port management */
    fmSWAG_logicalPortInfo   logicalPortInfo;

    /* Local Glort space allocations for each switch in the swag */
    fmSWAG_switchGlort       switchGlortSpace[FM_MAX_SWITCHES_PER_SWAG];

    /* A list of all links in the switch aggregate (see 'fm_swagIntLink'). */
    fm_customTree            links;
    FM_DLL_DEFINE_LIST(_fm_swagIntLink, firstLink, lastLink);
    fm_tree                  portLinkTree;

    /* A list of trunks in the switch aggregate (see 'fm_swagIntTrunk'). */
    FM_DLL_DEFINE_LIST(_fm_swagIntTrunk, firstTrunk, lastTrunk);

    /* The number of links in the switch aggregate */
    fm_int                   numLinks;

    /* MAC Table */
    fm_customTree            macTable;

    /* CPU Port Number */
    fm_int                   cpuPort;

    /* Array of LAG group handles. */
    fm_int                   lagGroupHandles[FM_MAX_LAGS_PER_SWAG * FM_SWAG_MAX_LAG_ALLOCS];

    /* Array of LAG groups. Subscript = lag group handle - swag lag base handle.
     * value = TRUE if the LAG group is in use, FALSE if not in use.
     */
    fm_bool                  lagsUsed[FM_MAX_LAGS_PER_SWAG * FM_SWAG_MAX_LAG_ALLOCS];

    /* Array of LBG groups. Subscript = lbg group handle - swag lbg base handle.
     * value = TRUE if the LBG is in use, FALSE if not in use.
     */
    fm_bool                  lbgsUsed[FM_MAX_LBGS_PER_SWAG];

    /* Linked list of available locally-allocated multicast group handles. */
    fm_dlist                 localAvailMcastGroupHandles;

    /* Array of multicast group handles. */
    fm_int                   mcastGroupHandles[FM_MAX_MCAST_GROUPS_PER_SWAG];

    /* Bit Array to track which multicast group handles are in use. */
    fm_bitArray              mcastGroupsInUse;

    /* Array of Storm-Controllers.  Subscript is switch-aggregate storm
     * controller number.
     */
    fmSWAG_stormCtrl         stormControllers[FM_MAX_NUM_STORM_CTRL];

    /* Array of forwarding rules.  Subscript is switch-aggregate forwarding
     * rule number.
     */
    fmSWAG_forwardingRule    forwardingRules[FM_MAX_STACKING_FORWARDING_RULES];

    /* Configuration information.  Stored so that a switch can be properly
     * configured after being added to a switch aggregate and started up */
    fm_int                   vlanTunnelMode;
    fm_macaddr               cpuMac;
    fm_bool                  trap8021x;
    fm_bool                  trapBpdu;
    fm_bool                  trapLacp;
    fm_bool                  trapGarp;
    fm_bool                  trapOther;
    fm_bool                  dropPause;
    fm_int                   ethTypeTrap;
    fm_bool                  trapMtuViolations;
    fm_bool                  dropMtuViolations;
    fm_uint32                mtuTable[FM_MAX_MTU_ENTRIES];
    fm_int                   bcastFlooding;
    fm_int                   mcastFlooding;
    fm_int                   ucastFlooding;
    fm_int                   frameTimeoutMsecs;
    fm_lagMode               lagMode;
    fm_uint32                routingHash;
    fm_uint32                hashRotation;
    fm_uint32                hashProt1;
    fm_uint32                hashProt2;
    fm_uint32                hashFlowDiffservMask;
    fm_uint32                hashFlowUserMask;
    fm_uint32                hashFlowLabelMask;
    fm_bool                  remapIeee;
    fm_bool                  remapCpu;
    fm_bool                  remapEthType;
    fm_int                   vlanType;
    fm_uint                  reservedVlan;
    fm_int                   lciEndianness;
    fm_int                   optionDisposition;
    fm_uint32                statGroupEnable;
    fm_lbgMode               lbgMode;
    fm_uint32                macTableHashPolynomial;
    fm_uint32                macAgingTime;
    fm_uint32                macTableEventMode;
    fm_uint32                macTableEventAgingThreshold;
    fm_uint32                macTableEventLearningThreshold;
    fm_uint32                macTableEventSecEventThreshold;
    fm_bool                  macSoftLearning;
    fm_uint32                lacpDisposition;
    fm_uint32                lagHashCfg;
    fm_uint32                lagHashCompatibility;
    fm_uint32                lagHashRotationA;
    fm_uint32                lagHashRotationB;
    fm_L2HashKey             l2HashKey[FM_SWAG_MAX_HASH_PROFILES];
    fm_L2HashRot             l2HashRotA[FM_SWAG_MAX_HASH_PROFILES];
    fm_L2HashRot             l2HashRotB[FM_SWAG_MAX_HASH_PROFILES];
    fm_L3HashConfig          l3HashCfg[FM_SWAG_MAX_HASH_PROFILES];
    fm_int                   qosTxHogWmMap[FM_MAX_TRAFFIC_CLASSES];
    fm_int                   qosPrivWm;
    fm_int                   qosSharedPauseOnWms[FM_MAX_MEMORY_PARTITIONS];
    fm_int                   qosSharedPauseOffWms[FM_MAX_MEMORY_PARTITIONS];
    fm_int                   qosSharedPriWms[FM_MAX_SWITCH_PRIORITIES];
    fm_uint32                qosTcSmpMaps[FM_MAX_TRAFFIC_CLASSES];
    fm_uint32                qosVpriSwpriMaps[FM_MAX_VLAN_PRIORITIES];
    fm_uint32                qosDscpSwpriMaps[FM_MAX_DSCP_PRIORITIES];
    fm_uint32                qosSwpriTcMaps[FM_MAX_SWITCH_PRIORITIES];
    fm_uint32                qosTcPriorities[FM_MAX_TRAFFIC_CLASSES];
    fm_uint32                qosCnMode;
    fm_uint32                qosCnFrameEtype;
    fm_uint32                qosCnFrameVpri;
    fm_uint32                qosCnFrameVlan;
    fm_int                   ifgPenalty;
    fm_bool                  trapPlusLog;
    fm_ffuSliceAllocations   sliceAlloc;

} fmSWAG_switch;


#define FM_ITERATE_SWAG_SWITCHES_NO_ARGS(sw, swType, status, func)  	\
{                                                                   	\
    fmSWAG_switch *ext;                                             	\
    fm_swagMember *curSwitch;                                       	\
    fm_bool        execute;                                         	\
    ext       = GET_SWITCH_EXT(sw);                                 	\
    curSwitch = ext->firstSwitch;                                   	\
    status    = FM_OK;                                              	\
    while ( (status == FM_OK) && (curSwitch != NULL) )              	\
    {                                                               	\
        switch (swType)                                             	\
        {                                                           	\
            case FM_SWITCH_ROLE_LEAF:                               	\
                switch (curSwitch->role)                            	\
                {                                                   	\
                    case FM_SWITCH_ROLE_LEAF:                       	\
                    case FM_SWITCH_ROLE_SPINE_LEAF:                 	\
                        execute = TRUE;                             	\
                        break;                                      	\
                    default:                                        	\
                        execute = FALSE;                            	\
                        break;                                      	\
                }                                                   	\
                break;                                              	\
            case FM_SWITCH_ROLE_SPINE:                              	\
                switch (curSwitch->role)                            	\
                {                                                   	\
                    case FM_SWITCH_ROLE_SPINE:                      	\
                    case FM_SWITCH_ROLE_SPINE_LEAF:                 	\
                        execute = TRUE;                             	\
                        break;                                      	\
                    default:                                        	\
                        execute = FALSE;                            	\
                        break;                                      	\
                }                                                   	\
                break;                                              	\
            case FM_SWITCH_ROLE_SPINE_LEAF:                         	\
                execute = TRUE;                                     	\
                break;                                              	\
            default:                                                	\
                execute = FALSE;                                    	\
                break;                                              	\
        }                                                           	\
        if (execute)                                                	\
        {                                                           	\
            VALIDATE_AND_PROTECT_SWITCH_NO_RETURN(status,               \
                                                  curSwitch->swId);     \
            if (status == FM_OK)                                        \
            {                                                           \
                status = func(curSwitch->swId);                         \
                UNPROTECT_SWITCH(curSwitch->swId);                      \
            }                                                           \
            switch (status)                                         	\
            {                                                       	\
                case FM_ERR_INVALID_SWITCH:                         	\
                case FM_ERR_UNSUPPORTED:                            	\
                case FM_ERR_SWITCH_NOT_UP:                          	\
                    status = FM_OK;                                 	\
                    break;                                          	\
                default:                                            	\
                    break;                                          	\
            }                                                       	\
        }                                                           	\
        curSwitch = curSwitch->nextSwitch;                          	\
    }                                                               	\
}


#define FM_ITERATE_SWAG_SWITCHES_ARGS(sw, swType, status, func, ...)	\
{                                                                   	\
    fmSWAG_switch *ext;                                             	\
    fm_swagMember *curSwitch;                                       	\
    fm_bool        execute;                                         	\
    ext       = GET_SWITCH_EXT(sw);                                 	\
    curSwitch = ext->firstSwitch;                                   	\
    status    = FM_OK;                                              	\
    while ( (status == FM_OK) && (curSwitch != NULL) )              	\
    {                                                               	\
        switch (swType)                                             	\
        {                                                           	\
            case FM_SWITCH_ROLE_LEAF:                               	\
                switch (curSwitch->role)                            	\
                {                                                   	\
                    case FM_SWITCH_ROLE_LEAF:                       	\
                    case FM_SWITCH_ROLE_SPINE_LEAF:                 	\
                        execute = TRUE;                             	\
                        break;                                      	\
                    default:                                        	\
                        execute = FALSE;                            	\
                        break;                                      	\
                }                                                   	\
                break;                                              	\
            case FM_SWITCH_ROLE_SPINE:                              	\
                switch (curSwitch->role)                            	\
                {                                                   	\
                    case FM_SWITCH_ROLE_SPINE:                      	\
                    case FM_SWITCH_ROLE_SPINE_LEAF:                 	\
                        execute = TRUE;                             	\
                        break;                                      	\
                    default:                                        	\
                        execute = FALSE;                            	\
                        break;                                      	\
                }                                                   	\
                break;                                              	\
            case FM_SWITCH_ROLE_SPINE_LEAF:                         	\
                execute = TRUE;                                     	\
                break;                                              	\
            default:                                                	\
                execute = FALSE;                                    	\
                break;                                              	\
        }                                                           	\
        if (execute)                                                	\
        {                                                           	\
            VALIDATE_AND_PROTECT_SWITCH_NO_RETURN(status,               \
                                                  curSwitch->swId);     \
            if (status == FM_OK)                                        \
            {                                                           \
                status = func(curSwitch->swId, __VA_ARGS__);            \
                UNPROTECT_SWITCH(curSwitch->swId);                      \
            }                                                           \
            switch (status)                                         	\
            {                                                       	\
                case FM_ERR_INVALID_SWITCH:                         	\
                case FM_ERR_UNSUPPORTED:                            	\
                case FM_ERR_SWITCH_NOT_UP:                          	\
                    status = FM_OK;                                 	\
                    break;                                          	\
                default:                                            	\
                    break;                                          	\
            }                                                       	\
        }                                                           	\
        curSwitch = curSwitch->nextSwitch;                          	\
    }                                                               	\
}


#define FM_ITERATE_SWAG_SWITCHES_PORT_0(sw, status, func, ...)      \
    FM_ITERATE_SWAG_SWITCHES_ARGS(sw,                               \
                                  FM_SWITCH_ROLE_SPINE_LEAF,        \
                                  status,                           \
                                  func,                             \
                                  __VA_ARGS__)


#define FM_ITERATE_SWAG_SWITCHES_LAG_PORT_NO_ARGS(sw, port, status, func)   \
{                                                                           \
    fmSWAG_switch *ext;                                                     \
    fm_swagMember *member;                                                  \
    fm_int         swagLag;                                                 \
    fm_int         realSw;                                                  \
    fm_int         realPort;                                                \
                                                                            \
    ext    = GET_SWITCH_EXT(sw);                                            \
    member = ext->firstSwitch;                                              \
    status = FM_OK;                                                         \
                                                                            \
    status = fmLogicalPortToLagIndex(sw, port, &swagLag);                   \
                                                                            \
    while ( (status == FM_OK) && (member != NULL) )                         \
    {                                                                       \
        realSw   = member->swId;                                            \
        realPort = member->lagGroups[swagLag];                              \
                                                                            \
        status = func(realSw, realPort);                                    \
                                                                            \
        if (status != FM_OK)                                                \
        {                                                                   \
            break;                                                          \
        }                                                                   \
                                                                            \
        member = member->nextSwitch;                                        \
    }                                                                       \
}


#define FM_ITERATE_SWAG_SWITCHES_LAG_PORT_ARGS(sw, port, status, func, ...) \
{                                                                           \
    fmSWAG_switch *ext;                                                     \
    fm_swagMember *member;                                                  \
    fm_int         swagLag;                                                 \
    fm_int         realSw;                                                  \
    fm_int         realPort;                                                \
                                                                            \
    ext    = GET_SWITCH_EXT(sw);                                            \
    member = ext->firstSwitch;                                              \
    status = FM_OK;                                                         \
                                                                            \
    status = fmLogicalPortToLagIndex(sw, port, &swagLag);                   \
                                                                            \
    while ( (status == FM_OK) && (member != NULL) )                         \
    {                                                                       \
        realSw   = member->swId;                                            \
        realPort = member->lagGroups[swagLag];                              \
                                                                            \
        status = func(realSw, realPort, __VA_ARGS__);                       \
                                                                            \
        if (status != FM_OK)                                                \
        {                                                                   \
            break;                                                          \
        }                                                                   \
                                                                            \
        member = member->nextSwitch;                                        \
    }                                                                       \
}


#define fmSWAGGetFirstStormCondition(stormPtr)                      \
    FM_DLL_GET_FIRST(stormPtr, firstCond)

#define fmSWAGGetNextStormCondition(condPtr)                        \
    FM_DLL_GET_NEXT(condPtr, nextCond)

#define fmSWAGAppendStormCondition(stormPtr, condPtr)               \
    FM_DLL_INSERT_LAST(stormPtr, firstCond, lastCond, condPtr,      \
                       nextCond, prevCond)

#define fmSWAGRemoveStormCondition(stormPtr, condPtr)               \
    FM_DLL_REMOVE_NODE(stormPtr, firstCond, lastCond, condPtr,      \
                       nextCond, prevCond)

#define fmSWAGGetFirstStormAction(stormPtr)                         \
    FM_DLL_GET_FIRST(stormPtr, firstAction)

#define fmSWAGGetNextStormAction(actionPtr)                         \
    FM_DLL_GET_NEXT(actionPtr, nextAction)

#define fmSWAGAppendStormAction(stormPtr, actionPtr)                \
    FM_DLL_INSERT_LAST(stormPtr, firstAction, lastAction,           \
                       actionPtr, nextAction, prevAction)

#define fmSWAGRemoveStormAction(stormPtr, actionPtr)                \
    FM_DLL_REMOVE_NODE(stormPtr, firstAction, lastAction,           \
                       actionPtr, nextAction, prevAction)

#define fmSWAGGetFirstTrunkLink(trunkPtr)                           \
    FM_DLL_GET_FIRST(trunkPtr, firstLink)

#define fmSWAGGetLastTrunkLink(trunkPtr)                            \
    FM_DLL_GET_LAST(trunkPtr, lastLink)

#define fmSWAGGetNextTrunkLink(linkPtr)                             \
    FM_DLL_GET_NEXT(linkPtr, nextTrunkLink)

#define fmSWAGAppendTrunkLink(trunkPtr, linkPtr)                    \
    FM_DLL_INSERT_LAST(trunkPtr, firstLink, lastLink,               \
                       linkPtr, nextTrunkLink, prevTrunkLink)

#define fmSWAGRemoveTrunkLink(trunkPtr, linkPtr)                    \
    FM_DLL_REMOVE_NODE(trunkPtr, firstLink, lastLink,               \
                       linkPtr, nextTrunkLink, prevTrunkLink)

#define fmSWAGIsTrunkLinkAgg(trunkPtr)                              \
    ( (trunkPtr->firstLink == trunkPtr->lastLink) ? FALSE : TRUE )

extern void fmSWAGEventHandler(fm_event * event);

extern fm_int fmCompareSWAGLinks(const void *first, const void *second);

extern fm_status fmHandleSWAGSwitchStateChange(fm_int  sw,
                                               fm_int  swagId,
                                               fm_bool state);

extern fm_swagMember *fmFindSwitchInSWAG(fmSWAG_switch *aggregatePtr,
                                         fm_int         sw);

extern fm_status fmApplySWAGAttributesToSwitch(fm_int swagId,
                                               fm_int sw);

extern fm_swagIntLink *fmFindLinkInLinkList(fmSWAG_switch *switchExt,
                                            fm_swagLink *  link);

extern fm_swagIntLink *fmFindPortInLinkList(fmSWAG_switch *switchExt,
                                            fm_int         port);

extern fm_status fmGetSwitchPortForSWAGPort(fmSWAG_switch *switchExt,
                                            fm_int         port,
                                            fm_int *       realSwPtr,
                                            fm_int *       realPortPtr);

extern fm_status fmSWAGCreateSWAGVlansForSwitch(fm_int swagId,
                                                fm_int sw);

extern fm_status fmSWAGCreateSWAGLagsForSwitch(fm_int swagId,
                                               fm_int sw);

extern fm_status fmSWAGApplyQOSToSwitch(fm_int swArrayId, fm_int sw);

extern fm_status fmIsSwitchInASWAG(fm_int sw, fm_int *swagPtr);

extern fm_status fmUpdateSwitchInSWAG(fm_int  swagId,
                                      fm_int  sw,
                                      fm_bool insert);

extern fm_status fmAddMACAddressToSWAGSwitches(fm_int                   sw,
                                               fm_internalMacAddrEntry *tblEntry,
                                               fm_int                   sourceSwitch);

extern fm_status fmDeleteMACAddressFromSWAGSwitches(fm_int                   sw,
                                                    fm_internalMacAddrEntry *tblEntry,
                                                    fm_int                   sourceSwitch);

fm_swagIntTrunk * fmGetSWAGTrunkForSwitchPair(fm_int sw,
                                              fm_int sourceSwitch,
                                              fm_int destSwitch);

fm_status fmPreInitializeSwitchInSWAG(fm_int sw);

fm_status fmSWAGPrepareLinks(fm_int swagId, fm_int sw);

fm_status fmSWAGGetNextAvailableLag(fm_int sw, fm_int *swagLag);

fm_status fmSelectGlortSpaceForSwitchInSWAG(fm_int swagId, fm_int sw);

fm_status fmSWAGCreateLagOnSubSwitch(fm_int swagId, fm_int sw, fm_int swagLag);

fm_status fmGetInternalPortList(fm_int  sw,
                                fm_int *portList,
                                fm_int  maxPorts,
                                fm_int *portCount);

fm_status fmGetInternalTrunkList(fm_int  sw,
                                 fm_int *portList,
                                 fm_int  maxPorts,
                                 fm_int *portCount);

fm_status fmConfigureSWAGSwitchMaster(fm_int sw);

fm_status fmSWAGApplyMACAttributesToSwitch(fm_int swagId, fm_int sw);

fm_status fmReleaseRemoteLogicalPorts(fm_swagMember *member, fm_int destSw);

fm_status fmSWAGDeleteLagCallback(fm_int swagId, fm_int sw, fm_int lagIndex);

fm_status fmAddRemoteLagPort(fm_int swagId, fm_int swagLag, fm_int port);

fm_status fmUpdateLagRemotePorts(fm_int swagId);

fm_status fmInitializeSwitchInSWAG(fm_int         swagId,
                                   fm_swagMember *member,
                                   fm_bool        fullInit);

fm_status fmInitializeAllSwitchesInSWAG(fm_int swagId);

fm_status fmAddRemoteMulticastListener(fm_int                   swagId,
                                       fm_intMulticastGroup *   group,
                                       fm_intMulticastListener *listener);

fm_status fmUpdateMulticastRemoteListeners(fm_int swagId);

fm_status fmSWAGValidateAndProtectSubSwitch(fm_int swagId, fm_int sw);

fm_status fmGetGlortForSwagPhysicalPort(fm_int     swagId,
                                        fm_int     port,
                                        fm_uint32 *glortPtr);

fm_status fmCrossReferencePortToLink(fm_int          sw,
                                     fm_int          port,
                                     fm_swagIntLink *link);

#define fmGetFirstSwitchInSWAG(extPtr) \
    FM_DLL_GET_FIRST(extPtr, firstSwitch)

#define fmGetLastSwitchInSWAG(extPtr) \
    FM_DLL_GET_LAST(extPtr, lastSwitch)

#define fmGetNextSwitchInSWAG(swptr) \
    FM_DLL_GET_NEXT(swptr, nextSwitch)

#define fmGetPreviousSwitchInSWAG(swptr) \
    FM_DLL_GET_PREV(swptr, prevSwitch)

#define fmAddSwitchToSwitchList(swptr, newSwitch) \
    FM_DLL_INSERT_LAST(swptr, firstSwitch, lastSwitch, newSwitch, \
                       nextSwitch, prevSwitch)

#define fmRemoveSwitchFromSwitchList(swptr, deadSwitch) \
    FM_DLL_REMOVE_NODE(swptr, firstSwitch, lastSwitch, \
                       deadSwitch, nextSwitch, prevSwitch)

#define fmGetFirstLinkInLinkList(extPtr) \
    FM_DLL_GET_FIRST(extPtr, firstLink)

#define fmGetLastLinkInLinkList(extPtr) \
    FM_DLL_GET_LAST(extPtr, lastLink)

#define fmGetNextLinkInLinkList(linkPtr) \
    FM_DLL_GET_NEXT(linkPtr, nextLink)

#define fmGetPrevLinkInLinkList(linkPtr) \
    FM_DLL_GET_PREV(linkPtr, prevLink)

#define fmAddLinkToLinkList(swptr, newLink) \
    FM_DLL_INSERT_LAST(swptr, firstLink, lastLink, newLink, \
                       nextLink, prevLink)

#define fmRemoveLinkFromLinkList(swptr, deadLink) \
    FM_DLL_REMOVE_NODE(swptr, firstLink, lastLink, \
                       deadLink, nextLink, prevLink)

#define fmGetFirstTrunkInSwitch(extPtr) \
    FM_DLL_GET_FIRST(extPtr, firstTrunk)

#define fmGetLastTrunkInSwitch(extPtr) \
    FM_DLL_GET_LAST(extPtr, lastTrunk)

#define fmGetNextTrunkInSwitch(trunkPtr) \
    FM_DLL_GET_NEXT(trunkPtr, nextTrunk)

#define fmGetPrevTrunkInSwitch(trunkPtr) \
    FM_DLL_GET_PREV(trunkPtr, prevTrunk)

#define fmAddTrunkToTrunkList(swptr, newTrunk) \
    FM_DLL_INSERT_LAST(swptr, firstTrunk, lastTrunk, newTrunk, \
                       nextTrunk, prevTrunk)

#define fmRemoveTrunkFromTrunkList(swptr, deadTrunk) \
    FM_DLL_REMOVE_NODE(swptr, firstTrunk, lastTrunk, \
                       deadTrunk, nextTrunk, prevTrunk)


#endif /* __FM_FM_API_SWAG_INT_H */
