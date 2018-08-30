/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_flow.h
 * Creation Date:   July 20, 2010
 * Description:     Constants for attributes and attribute values
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

#ifndef __FM_FM_API_FLOW_H
#define __FM_FM_API_FLOW_H

/** Maximum number of flow table types.               \ingroup constSystem */
#define FM_FLOW_MAX_TABLE_TYPE      10

/** The static port set for all ports.                \ingroup constSystem */
#define FM_FLOW_PORT_SET_ALL        FM_ACL_PORT_SET_ALL

/**************************************************/
/** Flow Condition Masks
 *  \ingroup constFlowCond
 *  \page flowCondMasks
 *
 *  The following set of bit masks may be ORed
 *  together to produce an ''fm_flowCondition'' value.
 *  Each bit mask selects a field within the received
 *  frame that should be matched when a flow key is
 *  tested.
 *                                              \lb\lb
 *  For each bit mask specified in ''fm_flowCondition'',
 *  a corresponding value and mask must be specified
 *  in the ''fm_flowValue'' argument to ''fmAddFlow'' 
 *  or ''fmModifyFlow''. The required value and mask
 *  fields of ''fm_flowValue'' are indicated for each
 *  bit mask.
 **************************************************/
/** \ingroup constFlowCond
 * @{ */

/** Match the source MAC address. Specify src and srcMask in ''fm_flowValue''.
 *  
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FLOW_MATCH_SRC_MAC           FM_ACL_MATCH_SRC_MAC

/** Match the destination MAC address. Specify dst and dstMask in 
 *  ''fm_flowValue''.
 * 
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FLOW_MATCH_DST_MAC           FM_ACL_MATCH_DST_MAC

/** Match the Ethernet type. Specify ethType and ethTypeMask in ''fm_flowValue''.
 *  
 *  \chips  FM3000, FM4000 */
#define FM_FLOW_MATCH_ETHERTYPE         FM_ACL_MATCH_ETHERTYPE

/** Match the VLAN ID. Specify vlanId and vlanIdMask in ''fm_flowValue''.
 *  
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FLOW_MATCH_VLAN              FM_ACL_MATCH_VLAN

/** Match the VLAN priority. Specify vlanPri and vlanPriMask in ''fm_flowValue''.
 *  
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FLOW_MATCH_VLAN_PRIORITY     FM_ACL_MATCH_PRIORITY

/** Match the IP source address. Specify srcIp and srcIpMask in ''fm_flowValue''.
 *  
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FLOW_MATCH_SRC_IP            FM_ACL_MATCH_SRC_IP

/** Match the IP destination address. Specify dstIp and dstIpMask in  
 *  ''fm_flowValue''.
 *  
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FLOW_MATCH_DST_IP            FM_ACL_MATCH_DST_IP

/** Match the L4 protocol (IPv4) or Next Header (IPv6). Specify protocol and 
 *  protocolMask in ''fm_flowValue''.
 *                                                                      \lb\lb
 *  For FM6000 devices, this condition uses the mapped value to reduce the 
 *  required number of slices needed. Up to 15 different protocols can be 
 *  mapped.
 *  
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FLOW_MATCH_PROTOCOL          FM_ACL_MATCH_PROTOCOL

/** Match the TCP/UDP source port.
 *                                                                      \lb\lb
 *  For FM4000 devices, specify L4SrcStart in ''fm_flowValue''.
 *  
 *  For FM6000 devices, this condition uses the mapped value to reduce the 
 *  required number of slices needed. Up to 32 differents L4 Src Port range 
 *  can be defined and this condition must always be paired with
 *  ''FM_FLOW_MATCH_PROTOCOL''. The entered range should not overlap with
 *  any other L4 source port range already configured for the same protocol.
 *  Specify L4SrcStart and L4SrcEnd in ''fm_flowValue''.
 *  
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FLOW_MATCH_L4_SRC_PORT       FM_ACL_MATCH_L4_SRC_PORT

/** Match the TCP/UDP destination port.
 *                                                                      \lb\lb
 *  For FM4000 devices, specify L4DstStart in ''fm_flowValue''.
 *                                                                      \lb\lb
 *  For FM6000 devices, this condition uses the mapped value to reduce the 
 *  required number of slices needed. Up to 32 differents L4 Dst Port range 
 *  can be defined and this condition must always be paired with
 *  ''FM_FLOW_MATCH_PROTOCOL''. The entered range should not overlap with
 *  any other L4 destination port range already configured for the same 
 *  protocol. Specify L4DstStart and L4DstEnd in ''fm_flowValue''.
 *  
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FLOW_MATCH_L4_DST_PORT       FM_ACL_MATCH_L4_DST_PORT

/** Match on ingress logical port mask. Specify ingressPortMask in 
 *  ''fm_flowValue''.
 *  
 *  \chips  FM3000, FM4000 */
#define FM_FLOW_MATCH_INGRESS_PORT_MASK FM_ACL_MATCH_INGRESS_PORT_MASK

/** Match on ingress logical port set. Specify portSet in ''fm_flowValue''.
 *                                                                      \lb\lb
 *  For FM6000 devices, this condition uses the mapped value to reduce the 
 *  required number of slices needed. Up to 8 different port sets can be 
 *  defined. Matching on the port set ''FM_FLOW_PORT_SET_ALL''  does not 
 *  consume any mapped resources.
 *  
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FLOW_MATCH_INGRESS_PORT_SET  FM_ACL_MATCH_INGRESS_PORT_SET

/** Match the Type of Service octet (IPv4) or Traffic Class octet (IPv6).
 *  Specify tos and tosMask in ''fm_flowValue''.
 *  
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FLOW_MATCH_TOS               FM_ACL_MATCH_TOS

/** Match the Frame Type. Specify frameType in ''fm_flowValue''.
 *  
 *  \chips  FM6000 */
#define FM_FLOW_MATCH_FRAME_TYPE        FM_ACL_MATCH_FRAME_TYPE

/** @} (end of Doxygen group) */

typedef enum
{
    /** FFU TCAM */
    FM_FLOW_TCAM_TABLE,

    /** Not supported yet  */
    FM_FLOW_BST_TABLE,

    /** Not supported yet */
    FM_FLOW_HASH_TABLE,

    /* ---- Add new types above this line. ---- */

    /** For internal use only. */
    FM_FLOW_TABLE_MAX

} fm_flowTableType;


/**************************************************/
/** \ingroup typeEnum
 *  Flow states, used as an argument to ''fmAddFlow''
 *  and ''fmSetFlowState''.
 **************************************************/
typedef enum
{
    /** Flow is added to the hardware in a standby (disabled) mode. */
    FM_FLOW_STATE_STANDBY,
    
    /** Flow is added to the hardware in an enabled mode. */
    FM_FLOW_STATE_ENABLED

} fm_flowState;


/****************************************************************************/
/** \ingroup constFlowAttr
 *
 *  Flow API attributes, used as an argument to ''fmSetFlowAttribute'' and 
 *  ''fmGetFlowAttribute''.
 *                                                                      \lb\lb
 *  For each attribute, the data type of the corresponding attribute value is
 *  indicated.
 ****************************************************************************/
enum _fm_flowGroupAttr
{
    /** Type fm_int: A read-only attribute used to get an identifier for a
     *  Default Flow Action triggered with ''FM_FLOW_ACTION_DEFAULT''. The
     *  identifier is the frame destination GLORT.
     *
     *  \chips  FM3000, FM4000, FM6000 */
    FM_FLOW_DEF_ACTION_CODE, 

    /** Type fm_int: A read-only attribute used to get an identifier for a
     *  Trap Action triggered with ''FM_FLOW_ACTION_TRAP''. The
     *  identifier is the frame destination GLORT.
     *
     *  \chips  FM3000, FM4000, FM6000 */
    FM_FLOW_TRAP_ACTION_CODE, 

    /** Type fm_int: A read-only attribute used to get an identifier for a
     *  Forward to CPU Action triggered with ''FM_FLOW_ACTION_FORWARD''
     *  coupled with the CPU logical port. The identifier is the frame
     *  destination GLORT.
     *
     *  \chips  FM3000, FM4000, FM6000 */
    FM_FLOW_FWD_TO_CPU_ACTION_CODE, 

    /** Type fm_bool: Specifies if this tableIndex supports priority
     *  between flows: FM_ENABLED or FM_DISABLED (default). Addition and
     *  removal of flows that are part of a table that supports priority
     *  takes more CPU resources. This attribute must be configured prior
     *  to calling ''fmCreateFlowTCAMTable''.
     *
     *  \chips  FM6000 */
    FM_FLOW_TABLE_WITH_PRIORITY, 

    /** Type fm_bool: Specifies if a low priority catch-all flow that redirects
     *  traffic to the CPU port will be automatically added when the tableIndex
     *  is created. This attribute is only valid for tableIndex without priority
     *  support (see ''FM_FLOW_TABLE_WITH_PRIORITY''): FM_ENABLED or 
     *  FM_DISABLED (default). This attribute must be configured prior to 
     *  calling ''fmCreateFlowTCAMTable''.
     *
     *  \chips  FM3000, FM4000, FM6000 */
    FM_FLOW_TABLE_WITH_DEFAULT_ACTION, 

    /** For internal use only. */
    FM_FLOW_ATTR_MAX
};


/**************************************************/
/** Flow Action Masks
 * \ingroup constFlowAction
 * \page flowActionMasks
 *
 *  These bit masks are used to define the action
 *  argument of type ''fm_flowAction'' in
 *  ''fmAddFlow'' and ''fmModifyFlow''. When a flow
 *  condition match occurs on a received packet,
 *  one or more associated actions may be taken as
 *  indicated in the action bit mask. Note that some
 *  actions require an associated param argument to
 * ''fmAddFlow'' or ''fmModifyFlow'' of type 
 *  ''fm_flowParam''. Actions requiring a param 
 *  argument are noted below.
 **************************************************/
/** \ingroup constFlowAction
 * @{ */

 /** Forward the packet to the logical port specified in the logicalPort 
  * field of ''fm_flowParam'', which may be a port, LAG, LBG or a flood
  * logical port. */
#define FM_FLOW_ACTION_FORWARD           FM_ACL_ACTIONEXT_REDIRECT 

/** Bypass the flow forwarding rules and forward the packet using normal
 *  L2/L3 switch processing. */
#define FM_FLOW_ACTION_FORWARD_NORMAL    0x80000000

/** Drop the packet (forward to a null destination). */
#define FM_FLOW_ACTION_DROP              0x40000000 //FM_ACL_ACTIONEXT_REDIRECT

/** Trap the packet to the CPU. */
#define FM_FLOW_ACTION_TRAP              0x20000000 //FM_ACL_ACTIONEXT_REDIRECT

/** Redirect the packet to the CPU. */
#define FM_FLOW_ACTION_DEFAULT           0x10000000 //FM_ACL_ACTIONEXT_REDIRECT 

/** Count the packet. */
#define FM_FLOW_ACTION_COUNT             FM_ACL_ACTIONEXT_COUNT

/** @} (end of Doxygen group) */


/**************************************************
 * The following actions are not yet supported.
 * They should be moved above the Doxygen group
 * delimiter once they are supported so that they
 * will appear in the API user guide.
 **************************************************/

/** Forward the packet back to the ingress port. */
#define FM_FLOW_ACTION_REFLECT

/** Sett the traffic class of the frame. */
#define FM_FLOW_ACTION_ENQUEUE

/** Update a field in the packet. */
#define FM_FLOW_ACTION_MODIFY_FIELD      

/*---------------- End of unsupported Flow actions ------------- */

/* FIXME: should be decoupled from the ACL interface */
/**************************************************/
/** \ingroup typeScalar
 *  A Flow API matching condition value, used as an 
 *  argument to ''fmAddFlow'' and ''fmModifyFlow'' . 
 *  It is identical to ''fm_aclValue'', so see that
*   type for a complete description. 
 **************************************************/
typedef fm_aclValue     fm_flowValue;

/**************************************************/
/** \ingroup typeScalar
 *  Used as an argument to ''fmAddFlow'' and 
 *  ''fmModifyFlow''. Some flow actions require an 
 *  argument upon which the action will be performed 
 *  or filtered against. The argument value is
 *  carried in this data type. It is identical to 
 *  ''fm_aclParamExt'', so see that type for a complete 
 *  description. 
 **************************************************/
typedef fm_aclParamExt  fm_flowParam;

/**************************************************/
/** \ingroup typeScalar
 *  Used as an argument to ''fmGetFlowCount''. It 
 *  is identical to ''fm_aclCounters'', so see that 
 *  type for a complete description. 
 **************************************************/
typedef fm_aclCounters  fm_flowCounters;

/**************************************************/
/** \ingroup typeScalar
 *  A Flow API matching condition, used as an argument 
 *  to ''fmAddFlow'', ''fmCreateFlowTCAMTable'',
 *  and ''fmModifyFlow'' . It is comprised of a bit 
 *  mask representing a set of matching conditions. 
 *  See ''Flow Condition Masks'' for 
 *  definitions of each bit in the mask.
 **************************************************/
typedef fm_uint64       fm_flowCondition;

/**************************************************/
/** \ingroup typeScalar
 *  A Flow API action to be taken on a matching 
 *  condition, used as an argument 
 *  to ''fmAddFlow'' and ''fmModifyFlow'' . It is 
 *  comprised of a bit mask representing a set of 
 *  actions. See ''Flow Action Masks'' for 
 *  definitions of each bit in the mask.
 **************************************************/
typedef fm_uint32       fm_flowAction;


fm_status fmCreateFlowTCAMTable(fm_int           sw, 
                                fm_int           tableIndex, 
                                fm_flowCondition condition,
                                fm_uint32        maxEntries,
                                fm_uint32        maxAction);

fm_status fmDeleteFlowTCAMTable(fm_int sw, 
                                fm_int tableIndex);

fm_status fmAddFlow(fm_int           sw, 
                    fm_int           tableIndex,
                    fm_uint16        priority,
                    fm_uint32        precedence, 
                    fm_flowCondition condition,
                    fm_flowValue *   condVal,
                    fm_flowAction    action,
                    fm_flowParam *   param,
                    fm_flowState     flowState,
                    fm_int *         flowId);

fm_status fmModifyFlow(fm_int           sw, 
                       fm_int           tableIndex,
                       fm_int           flowId,
                       fm_uint16        priority,
                       fm_uint32        precedence, 
                       fm_flowCondition condition,
                       fm_flowValue *   condVal,
                       fm_flowAction    action,
                       fm_flowParam *   param);

fm_status fmDeleteFlow(fm_int sw, fm_int tableIndex, fm_int flowId);

fm_status fmSetFlowState(fm_int       sw, 
                         fm_int       tableIndex, 
                         fm_int       flowId, 
                         fm_flowState flowState);

fm_status fmGetFlowCount(fm_int           sw, 
                         fm_int           tableIndex, 
                         fm_int           flowId,
                         fm_flowCounters *counters);

fm_status fmResetFlowCount(fm_int sw, 
                           fm_int tableIndex, 
                           fm_int flowId);

fm_status fmGetFlowUsed(fm_int   sw, 
                        fm_int   tableIndex, 
                        fm_int   flowId,
                        fm_bool  clear,
                        fm_bool *used);

fm_status fmSetFlowAttribute(fm_int sw,
                             fm_int tableIndex,
                             fm_int attr,
                             void * value);

fm_status fmGetFlowAttribute(fm_int sw,
                             fm_int tableIndex,
                             fm_int attr,
                             void * value);

#endif /* __FM_FM_API_FLOW_H */
