/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_sflow.h
 * Creation Date:   May 29, 2008
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

#ifndef __FM_FM_API_SFLOW_H
#define __FM_FM_API_SFLOW_H

#define FM_SFLOW_VLAN_ANY           0
#define FM_SFLOW_NO_TRUNC          -1
#define FM_SFLOW_PRIORITY_ORIGINAL  FM_MIRROR_PRIORITY_ORIGINAL


/****************************************************************************/
/** \ingroup typeEnum
 *  sFlow types, used as an argument to ''fmCreateSFlow''.
 *                                                                      \lb\lb
 *  sFlows are not supported on FM2000 devices.
 ****************************************************************************/
typedef enum
{
    /** The sFlow monitors only ingress traffic. */
    FM_SFLOW_TYPE_INGRESS = 0,
    
    /** The sFlow monitors only egress traffic. */
    FM_SFLOW_TYPE_EGRESS
    
} fm_sFlowType;


/****************************************************************************/
/** \ingroup constSFlowAttr
 *
 *  sFlow Attributes, used as an argument to ''fmSetSFlowAttribute'' and 
 *  ''fmGetSFlowAttribute''.
 *                                                                      \lb\lb
 *  For each attribute, the data type of the corresponding attribute value is
 *  indicated.
 *                                                                      \lb\lb
 *  sFlows are not supported on FM2000 devices.
 ****************************************************************************/
enum _fm_sFlowAttr
{
    /** Type fm_uint16: A VLAN ID to indicate the VLAN with which the sampled 
     *  traffic is associated. Set to FM_SFLOW_VLAN_ANY (default) to
     *  sample non-VLAN specific traffic. 
     *
     *  \chips  FM3000, FM4000, FM6000 */
    FM_SFLOW_VLAN = 0,
 
    /** Type fm_uint: The sFlow sampling rate, expressed as a positive integer
     *  N, indicating that every Nth frame is to be sampled. The specified
     *  value will be rounded up or down to the nearest value supported
     *  by the hardware so the value read back may not exactly match the
     *  value that was set. 
     *                                                              \lb\lb
     *  For FM3000 and FM4000 devices the value ranges from 1 (the default) 
     *  to 16,777,215. The value set applies only to the specified sFlow.
     *                                                              \lb\lb
     *  For FM6000 devices, the value ranges from 1 to 65535 with 
     *  a default of 1. Only one sample rate can be set for the entire 
     *  switch. All sFlow instances will share the same value, which will be
     *  the last one set.
     *
     *  \chips  FM3000, FM4000, FM6000 */
    FM_SFLOW_SAMPLE_RATE,
 
    /** Type fm_int: The sFlow sample frame truncation length in bytes. Set to
     *  FM_SFLOW_NO_TRUNC (the default) to disable truncation. 
     *                                                                  \lb\lb
     *  For FM3000 and FM4000 devices, the specified value will be rounded 
     *  down to the nearest multiple of 4. 
     *                                                                  \lb\lb
     *  For FM6000 devices, any value other than FM_SFLOW_NO_TRUNC 
     *  will enable truncation to a fixed length of 160 bytes.
     *
     *  \chips  FM3000, FM4000, FM6000 */
    FM_SFLOW_TRUNC_LENGTH,
    
    /** Type fm_int: A read-only attribute used to get an identifier for a
     *  specific sFlow. 
     *                                                                  \lb\lb
     *  For FM3000 and FM4000 devices, the identifier is the trigger Number 
     *  used for sampling. 
     *                                                                  \lb\lb
     *  For FM6000 devices, the identifier is the frame destination GLORT.
     *
     *  \chips  FM3000, FM4000, FM6000 */
    FM_SFLOW_TRAP_CODE, 

    /** Type fm_uint64: A read-only attribute used to get the number of packets
     *  matching the sFlow conditions for the specific sFlow. This counter 
     *  represents the trigger count associated with the identified sFlow.
     *
     *  \chips  FM3000, FM4000 */
    FM_SFLOW_COUNT, 

    /** Type fm_int: The priority traffic class for the sampled traffic. Values
     *  range from 0 to 11 with the default being FM_SFLOW_PRIORITY_ORIGINAL,
     *  which indicates that the priority should be the original priority of
     *  the frame being sampled.
     *
     *  \chips FM6000 */
    FM_SFLOW_PRIORITY,

    /** For internal use only. */
    FM_SFLOW_ATTR_MAX
    
};

fm_status fmCreateSFlow(fm_int sw, fm_int sFlowId, fm_sFlowType sFlowType);

fm_status fmDeleteSFlow(fm_int sw, fm_int sFlowId);

fm_status fmAddSFlowPort(fm_int sw, fm_int sFlowId, fm_int port);

fm_status fmDeleteSFlowPort(fm_int sw, fm_int sFlowId, fm_int port);

fm_status fmGetSFlowPortFirst(fm_int sw, fm_int sFlowId, fm_int * firstPort);

fm_status fmGetSFlowPortNext(fm_int   sw,
                             fm_int   sFlowId,
                             fm_int   startPort,
                             fm_int * nextPort);

fm_status fmGetSFlowPortList(fm_int   sw, 
                             fm_int   sFlowId, 
                             fm_int * numPorts, 
                             fm_int * portList, 
                             fm_int   max);

fm_status fmSetSFlowAttribute(fm_int sw, 
                              fm_int sFlowId, 
                              fm_int attr, 
                              void * value);

fm_status fmGetSFlowAttribute(fm_int sw, 
                              fm_int sFlowId, 
                              fm_int attr, 
                              void * value);

fm_status fmGetSFlowType(fm_int         sw, 
                         fm_int         sFlowId,
                         fm_sFlowType * sFlowType);

fm_status fmCheckSFlowLogging(fm_int            sw, 
                              fm_eventPktRecv * pktEvent, 
                              fm_bool         * isPktSFlowLogged);

#endif /* __FM_FM_API_SFLOW_H */
