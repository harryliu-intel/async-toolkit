/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces) */
/*****************************************************************************
 * File:            fm_api_storm.h
 * Creation Date:   February 7, 2007
 * Description:     Header file for routing services.
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

#ifndef __FM_FM_API_STORM_H
#define __FM_FM_API_STORM_H

/** The maximum number of storm controllers.
 *  \ingroup constSystem */
#define FM_MAX_NUM_STORM_CTRL  16

/****************************************************************************/
/** \ingroup constStormAttr
 *
 *  Storm Controller Attributes, used as an argument to 
 *  ''fmSetStormCtrlAttribute'' and ''fmGetStormCtrlAttribute''.
 *                                                                      \lb\lb
 * For each attribute, the data type of the corresponding attribute value is
 * indicated.
 ****************************************************************************/
enum _fm_stormAttr
{
    /** Type fm_uint32: The token bucket capacity in bytes, with a default
     *  value of 1024. This attribute is the traffic maximum burst size and
     *  represents the maximum amount of memory in the switch that may be
     *  consumed by traffic monitored by this storm controller. The actual
     *  value will be the nearest value supported by the hardware, so the
     *  value retrieved with a call to ''fmGetStormCtrlAttribute'' may not
     *  exactly match the value set with ''fmSetStormCtrlAttribute''.
     *
     *  \chips  FM3000, FM4000, FM6000 */
    FM_STORM_CAPACITY = 0,

    /** Type fm_uint32: The rate at which the token bucket is refilled
     *  in kilobits per second (10,000 => 10Mbps, 10,000,000 => 10Gbps),
     *  with a default value of 150,000. This attribute represents the maximum
     *  bandwidth that may be consumed by traffic that is monitored by
     *  this storm controller (after an initial burst whose size is controlled
     *  by ''FM_STORM_CAPACITY''). The actual value will be the nearest value
     *  supported by the hardware, so the value retrieved with a call to
     *  ''fmGetStormCtrlAttribute'' may not exactly match the value set with
     *  ''fmSetStormCtrlAttribute''. 
     *                                                                  \lb\lb
     *  Note that the minimum rate supported by storm controllers is
     *  approximately 10Mbps on FM3000/FM4000 devices and 24Kbps on FM6000
     *  devices. 
     *
     *  \chips  FM3000, FM4000, FM6000 */
    FM_STORM_RATE,

    /** Type fm_uint64: The number of packets matching the storm controller
     *  conditions. This counter is reset by setting it to 0. 
     *
     *  \chips  FM3000, FM4000 */
    FM_STORM_COUNT,

    /** For internal use only. */
    FM_STORM_ATTR_MAX

};  /* end enum _fm_stormAttr */

/****************************************************************************/
/** \ingroup typeEnum
 *
 * Storm controller conditions, used as a constant
 * in the ''fm_stormCondition'' structure.
 *                                                                      \lb\lb
 * Some conditions require an additional parameter, the purpose of which is
 * described with each condition type.
 *                                                                      \lb\lb
 * There are restrictions on which storm controller conditions can be
 * combined. The restrictions are noted below.
 ****************************************************************************/
typedef enum
{
    /** Include broadcast frames in the storm controller. The param member of
     *  ''fm_stormCondition'' is not used.
     *                                                                  \lb\lb
     *  On FM3000/FM4000 devices, may be combined with:                     \lb
     *  ''FM_STORM_COND_FIDFORWARD''                                        \lb
     *  ''FM_STORM_COND_FLOOD''                                             \lb
     *  ''FM_STORM_COND_LOG_ICMP''                                          \lb
     *  ''FM_STORM_COND_INGRESS_PORT''                                      \lb
     *  ''FM_STORM_COND_EGRESS_PORT''
     *  
     *  \chips  FM3000, FM4000, FM6000 */
    FM_STORM_COND_BROADCAST = 0,

    /** Include IGMP frames in the storm controller. The param member of
     *  ''fm_stormCondition'' is not used.
     *                                                                  \lb\lb
     *  Note: if using this storm controller condition, you must also set
     *  the ''FM_PORT_PARSER'' port attribute to an appropriate value.
     *                                                                  \lb\lb
     *  On FM3000/FM4000 devices, may be combined with:                     \lb
     *  ''FM_STORM_COND_802_1X''                                            \lb 
     *  ''FM_STORM_COND_BPDU''                                              \lb
     *  ''FM_STORM_COND_LACP''                                              \lb
     *  ''FM_STORM_COND_INGRESS_PORT''
     *  
     *  \chips  FM3000, FM4000 */
    FM_STORM_COND_IGMP,

    /** Include 802.1X frames in the storm controller. The param member of
     *  ''fm_stormCondition'' is not used.
     *                                                                  \lb\lb
     *  On FM3000/FM4000 devices, may be combined with:                     \lb
     *  ''FM_STORM_COND_BPDU''                                              \lb
     *  ''FM_STORM_COND_IGMP''                                              \lb
     *  ''FM_STORM_COND_LACP''                                              \lb
     *  ''FM_STORM_COND_INGRESS_PORT''
     *  
     *  \chips  FM3000, FM4000 */
    FM_STORM_COND_802_1X,

    /** Include BDPU frames in the storm controller. The param member of
     *  ''fm_stormCondition'' is not used.
     *                                                                  \lb\lb
     *  On FM3000/FM4000 devices, may be combined with:                     \lb
     *  ''FM_STORM_COND_802_1X''                                            \lb 
     *  ''FM_STORM_COND_IGMP''                                              \lb
     *  ''FM_STORM_COND_LACP''                                              \lb
     *  ''FM_STORM_COND_INGRESS_PORT''
     *  
     *  \chips  FM3000, FM4000 */
    FM_STORM_COND_BPDU,

    /** Include LACP frames in the storm controller. The param member of
     *  ''fm_stormCondition'' is not used.
     *                                                                  \lb\lb
     *  On FM3000/FM4000 devices, may be combined with:                     \lb
     *  ''FM_STORM_COND_802_1X''                                            \lb 
     *  ''FM_STORM_COND_BPDU''                                              \lb
     *  ''FM_STORM_COND_IGMP''                                              \lb
     *  ''FM_STORM_COND_INGRESS_PORT''
     *  
     *  \chips  FM3000, FM4000 */
    FM_STORM_COND_LACP,

    /** Include flooded unicast and multicast frames in the storm controller. 
     *  The param member of ''fm_stormCondition'' is not used. To control
     *  only unicast flooded traffic, use ''FM_STORM_COND_FLOOD_UCAST''. To
     *  control only multicast flooded traffic, use 
     *  ''FM_STORM_COND_FLOOD_MCAST''.
     *                                                                  \lb\lb
     *  On FM3000/FM4000 devices, may be combined with:                     \lb
     *  ''FM_STORM_COND_BROADCAST''                                         \lb
     *  ''FM_STORM_COND_FIDFORWARD''                                        \lb
     *  ''FM_STORM_COND_INGRESS_PORT''                                      \lb
     *  ''FM_STORM_COND_EGRESS_PORT''
     *  
     *  \chips  FM3000, FM4000 */
    FM_STORM_COND_FLOOD,

    /** Include flooded unicast frames in the storm controller. The param member
     *  of ''fm_stormCondition'' is not used.
     *                                                                  \lb\lb
     *  On FM3000/FM4000 devices, may be combined with:                     \lb
     *  ''FM_STORM_COND_FIDFORWARD_UCAST''                                  \lb
     *  ''FM_STORM_COND_INGRESS_PORT''                                      \lb
     *  ''FM_STORM_COND_EGRESS_PORT''
     *  
     *  \chips  FM3000, FM4000 */
    FM_STORM_COND_FLOOD_UCAST,

    /** Include flooded multicast frames in the storm controller. The param member
     *  of ''fm_stormCondition'' is not used.
     *                                                                  \lb\lb
     *  On FM3000/FM4000 devices, may be combined with:                     \lb
     *  ''FM_STORM_COND_FIDFORWARD_MCAST''                                  \lb
     *  ''FM_STORM_COND_LOG_ICMP''                                          \lb
     *  ''FM_STORM_COND_INGRESS_PORT''                                      \lb
     *  ''FM_STORM_COND_EGRESS_PORT''
     *  
     *  \chips  FM3000, FM4000 */
    FM_STORM_COND_FLOOD_MCAST,

    /** Include normally forwarded unicast and multicast frames in the storm 
     *  controller. The param member of ''fm_stormCondition'' is not used.
     *                                                                  \lb\lb
     *  On FM3000/FM4000 devices, may be combined with:                     \lb
     *  ''FM_STORM_COND_BROADCAST''                                         \lb
     *  ''FM_STORM_COND_FLOOD''                                             \lb
     *  ''FM_STORM_COND_INGRESS_PORT''                                      \lb
     *  ''FM_STORM_COND_EGRESS_PORT''
     *  
     *  \chips  FM3000, FM4000 */
    FM_STORM_COND_FIDFORWARD,

    /** Include normally forwarded unicast frames in the storm controller. 
     *  The param member of ''fm_stormCondition'' is not used.
     *                                                                  \lb\lb
     *  On FM3000/FM4000 devices, may be combined with:                     \lb
     *  ''FM_STORM_COND_FLOOD_UCAST''                                       \lb
     *  ''FM_STORM_COND_INGRESS_PORT''                                      \lb
     *  ''FM_STORM_COND_EGRESS_PORT''
     *  
     *  \chips  FM3000, FM4000 */
    FM_STORM_COND_FIDFORWARD_UCAST,

    /** Include normally forwarded multicast frames in the storm controller. 
     *  The param member of ''fm_stormCondition'' is not used.
     *                                                                  \lb\lb
     *  On FM3000/FM4000 devices, may be combined with:                     \lb
     *  ''FM_STORM_COND_FLOOD_MCAST''                                       \lb
     *  ''FM_STORM_COND_LOG_ICMP''                                          \lb
     *  ''FM_STORM_COND_INGRESS_PORT''                                      \lb
     *  ''FM_STORM_COND_EGRESS_PORT''
     *  
     *  \chips  FM3000, FM4000 */
    FM_STORM_COND_FIDFORWARD_MCAST,

    /** Include multicast frames in the storm controller. The param member of
     *  ''fm_stormCondition'' is not used.
     *                                                                  \lb\lb
     *  On FM3000/FM4000 devices, may be combined with:                     \lb
     *  ''FM_STORM_COND_INGRESS_PORT''                                      \lb
     *  ''FM_STORM_COND_EGRESS_PORT''
     *  
     *  \chips  FM3000, FM4000, FM6000 */
    FM_STORM_COND_MULTICAST,

    /** Include the logged multicast ICMP frames in the storm controller
     *  whose TTL is at most 1. The param member of ''fm_stormCondition''
     *  is not used.
     *                                                                  \lb\lb
     *  On FM3000/FM4000 devices, may be combined with:                     \lb
     *  ''FM_STORM_COND_BROADCAST''                                         \lb
     *  ''FM_STORM_COND_FIDFORWARD_MCAST''                                  \lb
     *  ''FM_STORM_COND_FLOOD_MCAST''                                       \lb
     *  ''FM_STORM_COND_INGRESS_PORT''                                      \lb
     *  ''FM_STORM_COND_EGRESS_PORT''
     *  
     *  \chips  FM3000, FM4000 */
    FM_STORM_COND_LOG_ICMP,

    /** Include the trapped unicast ICMP frames in the storm controller
     *  whose TTL is at most 1. The param member of ''fm_stormCondition''
     *  is not used.
     *                                                                  \lb\lb
     *  On FM3000/FM4000 devices, may be combined with:                     \lb
     *  ''FM_STORM_COND_CPU''                                               \lb
     *  ''FM_STORM_COND_INGRESS_PORT''
     *  
     *  \chips  FM3000, FM4000 */
    FM_STORM_COND_TRAP_ICMP,

    /** Include CPU frames in the storm controller. The param member of
     *  ''fm_stormCondition'' is not used.
     *                                                                  \lb\lb
     *  On FM3000/FM4000 devices, may be combined with:                     \lb
     *  ''FM_STORM_COND_TRAP_ICMP''                                         \lb
     *  ''FM_STORM_COND_INGRESS_PORT''
     *  
     *  \chips  FM3000, FM4000 */
    FM_STORM_COND_CPU,

    /** Include the security violation frames due to unknown source MAC 
     *  address. The param member of ''fm_stormCondition'' is not used.
     *                                                                  \lb\lb
     *  On FM3000/FM4000 devices, may be combined with:                     \lb
     *  ''FM_STORM_COND_SECURITY_VIOL_MOVE''                                \lb
     *  ''FM_STORM_COND_INGRESS_PORT''
     *  
     *  \chips  FM3000, FM4000 */
    FM_STORM_COND_SECURITY_VIOL_NEW_MAC,

    /** Include the security violation frames due to unknown source MAC 
     *  address. The param member of ''fm_stormCondition'' is not used.
     *                                                                  \lb\lb
     *  On FM3000/FM4000 devices, may be combined with:                     \lb
     *  ''FM_STORM_COND_SECURITY_VIOL_NEW_MAC''                             \lb
     *  ''FM_STORM_COND_INGRESS_PORT''
     *  
     *  \chips  FM3000, FM4000 */
    FM_STORM_COND_SECURITY_VIOL_MOVE,

    /** Include frames received on the port specified in the param member of
     *  ''fm_stormCondition''.
     *                                                                  \lb\lb
     *  (FM4000) By default, storm controllers apply to all ingress ports
     *  until this condition type is added. Multiple ports can be specified by
     *  adding this condition type multiple times, once for each port.
     *  Specifying the port as -1 resets the storm controller back to the
     *  default of applying to all ingress ports.
     *                                                                  \lb\lb
     *  On FM6000 devices, a storm controller must be associated with a single 
     *  ingress port. It will not become active until the port is specified.
     *  
     *  \chips  FM3000, FM4000, FM6000 */
    FM_STORM_COND_INGRESS_PORT,

    /** Include frames sent on the port specified in the param member of
     *  ''fm_stormCondition''.
     *                                                                  \lb\lb
     *  (FM4000) By default, storm controllers apply to all egress ports until
     *  this condition type is added. Multiple ports can be specified by
     *  adding this condition type multiple times, once for each port.
     *  Specifying the port as -1 resets the storm controller back to the
     *  default of applying to all egress ports.
     *                                                                  \lb\lb
     *  On FM3000/FM4000 devices, may be combined with:                     \lb
     *  ''FM_STORM_COND_BROADCAST''                                         \lb
     *  ''FM_STORM_COND_FIDFORWARD''                                        \lb
     *  ''FM_STORM_COND_FIDFORWARD_MCAST''                                  \lb
     *  ''FM_STORM_COND_FIDFORWARD_UCAST''                                  \lb
     *  ''FM_STORM_COND_FLOOD''                                             \lb
     *  ''FM_STORM_COND_FLOOD_MCAST''                                       \lb
     *  ''FM_STORM_COND_FLOOD_UCAST''                                       \lb
     *  ''FM_STORM_COND_LOG_ICMP''                                          \lb
     *  ''FM_STORM_COND_MULTICAST''                                         \lb
     *  ''FM_STORM_COND_INGRESS_PORT''                                      \lb
     *  ''FM_STORM_COND_EGRESS_PORT''
     *  
     *  \chips  FM3000, FM4000 */
    FM_STORM_COND_EGRESS_PORT,

    /** Include unicast frames in the storm controller. The param member of
     *  ''fm_stormCondition'' is not used.
     *  
     *  \chips  FM6000 */
    FM_STORM_COND_UNICAST,

    /** Include frames that have been routed to the CPU on an unresolved
     *  NEXTHOP entry. The param member of ''fm_stormCondition'' is not used.
     *  Automatically applies to all ports. May not be combined with
     *  ''FM_STORM_COND_INGRESS_PORT''.
     *  
     *  \chips  FM6000 */
    FM_STORM_COND_NEXTHOP_MISS,

    /** For internal use only. */
    FM_STORM_COND_MAX

} fm_stormCondType;

/****************************************************************************/
/** \ingroup typeEnum
 *
 * Storm controller action types. Used as a constant
 * in the ''fm_stormAction'' structure.
 *                                                                      \lb\lb
 * Some actions require an additional parameter, the purpose of which is
 * described with each action type.
 ****************************************************************************/
typedef enum
{
    /** Take no action. This is the default action for a storm
     *  controller and does not need to be explicitly set. The controller
     *  will still count associated conditions. The param member of
     *  ''fm_stormAction'' is not used. */
    FM_STORM_ACTION_DO_NOTHING = 0,

    /** The egress port specified in the param member of ''fm_stormAction''
     *  will not be sent any frames of the type(s) matching the conditions
     *  associated with the storm controller while a storm condition exists.
     *  Multiple egress ports can be specified by adding this action type
     *  multiple times, once for each egress port. Specifying the port as -1
     *  is equivalent to specifying all egress ports at once. */
    FM_STORM_ACTION_FILTER_PORT,

    /** For internal use only. */
    FM_STORM_ACTION_MAX

} fm_stormActionType;

/****************************************************************************/
/** \ingroup typeStruct
 *
 * Defines a condition to be monitored by a storm controller. Used as an
 * argument to ''fmAddStormCtrlCondition'', ''fmDeleteStormCtrlCondition''
 * ''fmGetStormCtrlConditionList'', ''fmGetStormCtrlConditionFirst'' and
 * ''fmGetStormCtrlConditionNext''.
 ****************************************************************************/

typedef struct _fm_stormCondition
{
    /** Identifies a condition to be monitored by the storm controller. See
     *  ''fm_stormCondType'' for details. */
    fm_stormCondType type;

    /** An optional parameter whose purpose is dependent on the the value of
     *  type. See ''fm_stormCondType'' for a description of param for
     *  each condition type. */
    fm_int           param;

} fm_stormCondition;


/****************************************************************************/
/** \ingroup typeStruct
 *
 * Defines an action to be taken when a storm controller detects a storm
 * condition (the controller's token bucket limit is reached). Used as an
 * argument to ''fmAddStormCtrlAction'', ''fmDeleteStormCtrlAction'',
 * ''fmGetStormCtrlActionList'', ''fmGetStormCtrlActionFirst'' and 
 * ''fmGetStormCtrlActionNext''.
 ****************************************************************************/

typedef struct _fm_stormAction
{
    /** Identifies an action to be taken when the storm controller's token
     *  bucket limit is reached. */
    fm_stormActionType type;

    /** An optional parameter whose purpose is dependent on the the value of
     *  type. See ''fm_stormActionType'' for a description of param for
     *  each action type. */
    fm_int             param;

} fm_stormAction;

/*****************************************************************************
 * Function prototypes.
 *****************************************************************************/

fm_status fmCreateStormCtrl(fm_int sw, fm_int *stormController);
fm_status fmDeleteStormCtrl(fm_int sw, fm_int stormController);
fm_status fmSetStormCtrlAttribute(fm_int sw,
                                  fm_int stormController,
                                  fm_int attr,
                                  void * value);
fm_status fmGetStormCtrlAttribute(fm_int sw,
                                  fm_int stormController,
                                  fm_int attr,
                                  void * value);
fm_status fmGetStormCtrlList(fm_int  sw,
                             fm_int *numStormControllers,
                             fm_int *stormControllers,
                             fm_int  max);
fm_status fmGetStormCtrlFirst(fm_int sw, fm_int *firstStormController);
fm_status fmGetStormCtrlNext(fm_int  sw,
                             fm_int  currentStormController,
                             fm_int *nextStormController);
fm_status fmAddStormCtrlCondition(fm_int             sw,
                                  fm_int             stormController,
                                  fm_stormCondition *condition);
fm_status fmDeleteStormCtrlCondition(fm_int             sw,
                                     fm_int             stormController,
                                     fm_stormCondition *condition);
fm_status fmGetStormCtrlConditionList(fm_int             sw,
                                      fm_int             stormController,
                                      fm_int *           numConditions,
                                      fm_stormCondition *conditionList,
                                      fm_int             max);
fm_status fmGetStormCtrlConditionFirst(fm_int             sw,
                                       fm_int             stormController,
                                       fm_stormCondition *firstCondition);
fm_status fmGetStormCtrlConditionNext(fm_int             sw,
                                      fm_int             stormController,
                                      fm_stormCondition *currentCondition,
                                      fm_stormCondition *nextCondition);
fm_status fmAddStormCtrlAction(fm_int          sw,
                               fm_int          stormController,
                               fm_stormAction *action);
fm_status fmDeleteStormCtrlAction(fm_int          sw,
                                  fm_int          stormController,
                                  fm_stormAction *action);
fm_status fmGetStormCtrlActionList(fm_int          sw,
                                   fm_int          stormController,
                                   fm_int *        numActions,
                                   fm_stormAction *actionList,
                                   fm_int          max);
fm_status fmGetStormCtrlActionFirst(fm_int          sw,
                                    fm_int          stormController,
                                    fm_stormAction *firstAction);
fm_status fmGetStormCtrlActionNext(fm_int          sw,
                                   fm_int          stormController,
                                   fm_stormAction *currentAction,
                                   fm_stormAction *nextAction);

void fmDbgDumpStormCtrl(fm_int sw, fm_int stormController);

const char * fmStormCondTypeToText(fm_stormCondType type);


#endif /* __FM_FM_API_STORM_H */
