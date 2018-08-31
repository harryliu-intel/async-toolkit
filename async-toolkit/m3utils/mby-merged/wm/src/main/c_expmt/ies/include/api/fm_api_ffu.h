/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:           fm_api_ffu.h
 * Creation Date:  March 08, 2007
 * Description:    Low-level API for manipulating the Filtering &
 *                 Forwarding Unit and the counter/policer banks
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

#ifndef __FM_FM_API_FFU_H
#define __FM_FM_API_FFU_H

/**************************************************
 * Legacy macros for published functions that have 
 * been renamed.
 **************************************************/

/** \ingroup macroSynonym
 * @{ */

/** A legacy synonym for ''fm4000FFUSetMasterValid''. */
#define fmFFUSetMasterValid(sw, validIn, validEg, useCache) \
        fm4000FFUSetMasterValid( (sw), (validIn), (validEg), (useCache) )

/** A legacy synonym for ''fm4000FFUGetMasterValid''. */
#define fmFFUGetMasterValid(sw, validIn, validEg, useCache) \
        fm4000FFUGetMasterValid( (sw), (validIn), (validEg), (useCache) )

/** A legacy synonym for ''fm4000FFUConfigureSlice''. */
#define fmFFUConfigureSlice(sw, slice, useCache) \
        fm4000FFUConfigureSlice( (sw), (slice), (useCache) )
        
/** A legacy synonym for ''fm4000FFUUnconfigureSlice''. */
#define fmFFUUnconfigureSlice(ww, slice, useCache) \
        fm4000FFUUnconfigureSlice( (ww), (slice), (useCache) )

/** A legacy synonym for ''fm4000FFUSetRule''. */
#define fmFFUSetRule(sw, slice, index, valid, value, mask, actionList, live, useCache) \
        fm4000FFUSetRule( (sw), (slice), (index), (valid), (value), (mask), (actionList), (live), (useCache) )

/** A legacy synonym for ''fm4000FFUSetRules''. */
#define fmFFUSetRules(sw, slice, index, nRules, valid, value, mask, actionList, live, useCache) \
        fm4000FFUSetRules( (sw), (slice), (index), (nRules), (valid), (value), (mask), (actionList), (live), (useCache) )

/** A legacy synonym for ''fm4000FFUSetRuleValid''. */
#define fmFFUSetRuleValid(sw, slice, index, valid,useCache) \
        fm4000FFUSetRuleValid( (sw), (slice), (index), (valid), (useCache) )

/** A legacy synonym for ''fm4000FFUGetRule''. */
#define fmFFUGetRule(sw, slice, index, valid, kase, value, mask, actionList, useCache) \
        fm4000FFUGetRule( (sw), (slice), (index), (valid), (kase), (value), (mask), (actionList), (useCache) )

/** A legacy synonym for ''fm4000FFUGetRules''. */
#define fmFFUGetRules(sw, slice, index, nRules, valid, kase, value, mask, actionList, useCache) \
        fm4000FFUGetRules( (sw), (slice), (index), (nRules), (valid), (kase), (value), (mask), (actionList), (useCache) )

/** A legacy synonym for ''fm4000FFUCopyRules''. */
#define fmFFUCopyRules(sw, slice, from, nRules, to, live, useCache) \
        fm4000FFUCopyRules( (sw), (slice), (from), (nRules), (to), (live), (useCache) )

/** A legacy synonym for ''fm4000FFUSetSourceMapper''. */
#define fmFFUSetSourceMapper(sw, port, mapSrc, routable, useCache) \
        fm4000FFUSetSourceMapper( (sw), (port), (mapSrc), (routable), (useCache) )

/** A legacy synonym for ''fm4000FFUSetSourceMapperMapSrc''. */
#define fmFFUSetSourceMapperMapSrc(sw, port, mapSrc, useCache) \
        fm4000FFUSetSourceMapperMapSrc( (sw), (port), (mapSrc), (useCache) )

/** A legacy synonym for ''fm4000FFUSetSourceMapperRoutable''. */
#define fmFFUSetSourceMapperRoutable(sw, port, routable, useCache) \
        fm4000FFUSetSourceMapperRoutable( (sw), (port), (routable), (useCache) )

/** A legacy synonym for ''fm4000FFUSetMACMapper''. */
#define fmFFUSetMACMapper(sw, slot, mac, ignoreLen, validSMAC, validDMAC, mapMAC, router, useCache) \
        fm4000FFUSetMACMapper( (sw), (slot), (mac), (ignoreLen), (validSMAC), (validDMAC), (mapMAC), (router), (useCache) )

/** A legacy synonym for ''fm4000FFUSetVLANMapper''. */
#define fmFFUSetVLANMapper(sw, vlan, mapVLAN, routable, useCache) \
        fm4000FFUSetVLANMapper( (sw), (vlan), (mapVLAN), (routable), (useCache) )

/** A legacy synonym for ''fm4000FFUSetVLANMapperMapVLAN''. */
#define fmFFUSetVLANMapperMapVLAN(sw, vlan, mapVLAN, useCache) \
        fm4000FFUSetVLANMapperMapVLAN( (sw), (vlan), (mapVLAN), (useCache) )

/** A legacy synonym for ''fm4000FFUSetVLANMapperRoutable''. */
#define fmFFUSetVLANMapperRoutable(sw, vlan, routable, useCache) \
        fm4000FFUSetVLANMapperRoutable( (sw), (vlan), (routable), (useCache) )

/** A legacy synonym for ''fm4000FFUSetVLANMappers''. */
#define fmFFUSetVLANMappers(sw, firstVLAN, nVLANs, mapVLAN, routable, useCache) \
        fm4000FFUSetVLANMappers( (sw), (firstVLAN), (nVLANs), (mapVLAN), (routable), (useCache) )

/** A legacy synonym for ''fm4000FFUSetVLANMapperMapVLANs''. */
#define fmFFUSetVLANMapperMapVLANs(sw, firstVLAN, nVLANs, mapVLAN, useCache) \
        fm4000FFUSetVLANMapperMapVLANs( (sw), (firstVLAN), (nVLANs), (mapVLAN), (useCache) )

/** A legacy synonym for ''fm4000FFUSetVLANMapperRoutables''. */
#define fmFFUSetVLANMapperRoutables(sw, firstVLAN, nVLANs, routable, useCache) \
        fm4000FFUSetVLANMapperRoutables( (sw), (firstVLAN), (nVLANs), (routable), (useCache) )

/** A legacy synonym for ''fm4000FFUSetEthertypeMapper''. */
#define fmFFUSetEthertypeMapper(sw, slot, ethertype, mapType, useCache) \
        fm4000FFUSetEthertypeMapper( (sw), (slot), (ethertype), (mapType), (useCache) )

/** A legacy synonym for ''fm4000FFUSetLengthMapper''. */
#define fmFFUSetLengthMapper(sw, slot, length, mapLength, useCache) \
        fm4000FFUSetLengthMapper( (sw), (slot), (length), (mapLength), (useCache) )

/** A legacy synonym for ''fm4000FFUSetIPMapper''. */
#define fmFFUSetIPMapper(sw, slot, ipAddress, ignoreLength, validSIP, validDIP, mapIP, live, useCache) \
        fm4000FFUSetIPMapper( (sw), (slot), (ipAddress), (ignoreLength), (validSIP), (validDIP), (mapIP), (live), (useCache) )

/** A legacy synonym for ''fm4000FFUSetIPMapperValid''. */
#define fmFFUSetIPMapperValid(sw, slot, validSIP, validDIP, useCache) \
        fm4000FFUSetIPMapperValid( (sw), (slot), (validSIP), (validDIP), (useCache) )

/** A legacy synonym for ''fm4000FFUSetProtocolMapper''. */
#define fmFFUSetProtocolMapper(sw, slot, protocol, mapProt, useCache) \
        fm4000FFUSetProtocolMapper( (sw), (slot), (protocol), (mapProt), (useCache) )

/** A legacy synonym for ''fm4000FFUSetL4Mapper''. */
#define fmFFUSetL4Mapper(sw, src, slot, l4port, mapProt, valid, mapPort, useCache) \
        fm4000FFUSetL4Mapper( (sw), (src), (slot), (l4port), (mapProt), (valid), (mapPort), (useCache) )

/** A legacy synonym for ''fm4000FFUGetSourceMapper''. */
#define fmFFUGetSourceMapper(sw, port, mapSrc, routable, useCache) \
        fm4000FFUGetSourceMapper( (sw), (port), (mapSrc), (routable), (useCache) )

/** A legacy synonym for ''fm4000FFUGetMACMapper''. */
#define fmFFUGetMACMapper(sw, slot, mac, igLen, valSMAC, valDMAC, mapMAC, router, useCache) \
        fm4000FFUGetMACMapper( (sw), (slot), (mac), (igLen), (valSMAC), (valDMAC), (mapMAC), (router), (useCache) )

/** A legacy synonym for ''fm4000FFUGetVLANMapper''. */
#define fmFFUGetVLANMapper(sw, vlan, mapVLAN, routable, useCache) \
        fm4000FFUGetVLANMapper( (sw), (vlan), (mapVLAN), (routable), (useCache) )

/** A legacy synonym for ''fm4000FFUGetVLANMappers''. */
#define fmFFUGetVLANMappers(sw, firstVLAN, nVLANs, mapVLAN, routable, useCache) \
        fm4000FFUGetVLANMappers( (sw), (firstVLAN), (nVLANs), (mapVLAN), (routable), (useCache) )

/** A legacy synonym for ''fm4000FFUGetEthertypeMapper''. */
#define fmFFUGetEthertypeMapper(sw, slot, etype, mapType, useCache) \
        fm4000FFUGetEthertypeMapper( (sw), (slot), (etype), (mapType), (useCache) )

/** A legacy synonym for ''fm4000FFUGetLengthMapper''. */
#define fmFFUGetLengthMapper(sw, slot, len, mapLen, useCache) \
        fm4000FFUGetLengthMapper( (sw), (slot), (len), (mapLen), (useCache) )

/** A legacy synonym for ''fm4000FFUGetIPMapper''. */
#define fmFFUGetIPMapper(sw, slot, ipAddr, igLen, valSIP, valDIP, mapIP, useCache) \
        fm4000FFUGetIPMapper( (sw), (slot), (ipAddr), (igLen), (valSIP), (valDIP), (mapIP), (useCache) )

/** A legacy synonym for ''fm4000FFUGetProtocolMapper''. */
#define fmFFUGetProtocolMapper(sw, slot, prot, mapProt, useCache) \
        fm4000FFUGetProtocolMapper( (sw), (slot), (prot), (mapProt), (useCache) )

/** A legacy synonym for ''fm4000FFUGetL4Mapper''. */
#define fmFFUGetL4Mapper(sw, src, slot, l4port, mapProt, valid, mapPort, useCache) \
        fm4000FFUGetL4Mapper( (sw), (src), (slot), (l4port), (mapProt), (valid), (mapPort), (useCache) )

/** A legacy synonym for ''fm4000FFUSetCounter''. */
#define fmFFUSetCounter(sw, bank, index, frameCount, byteCount) \
        fm4000FFUSetCounter( (sw), (bank), (index), (frameCount), (byteCount) )

/** A legacy synonym for ''fm4000FFUSetCounters''. */
#define fmFFUSetCounters(sw, bank, firstIndex, nCounters, frameCount, byteCount) \
        fm4000FFUSetCounters( (sw), (bank), (firstIndex), (nCounters), (frameCount), (byteCount) )

/** A legacy synonym for ''fm4000FFUSetPolicer''. */
#define fmFFUSetPolicer(sw, bank, policerIndex, committed, excess, timestamp) \
        fm4000FFUSetPolicer( (sw), (bank), (policerIndex), (committed), (excess), (timestamp) )

/** A legacy synonym for ''fm4000FFUSetPolicers''. */
#define fmFFUSetPolicers(sw, bank, fIndex, nPol, com, ex, timestamp) \
        fm4000FFUSetPolicers( (sw), (bank), (fIndex), (nPol), (com), (ex), (timestamp) )

/** A legacy synonym for ''fm4000FFUSetPolicerConfig''. */
#define fmFFUSetPolicerConfig(sw, bank, xLstPol, xLstCntNoInt, inColSrc, mkDSCP, mkSwPri, tsWEn, useCache) \
        fm4000FFUSetPolicerConfig( (sw), (bank), (xLstPol), (xLstCntNoInt), (inColSrc), (mkDSCP), (mkSwPri), (tsWEn), (useCache) )

/** A legacy synonym for ''fm4000FFUGetCounter''. */
#define fmFFUGetCounter(sw, bank, ctrIdx, frameCnt, byteCnt) \
        fm4000FFUGetCounter( (sw), (bank), (ctrIdx), (frameCnt), (byteCnt) )

/** A legacy synonym for ''fm4000FFUGetCounters''. */
#define fmFFUGetCounters(sw, bank, fIndex, nCounters, frameCnt, byteCnt) \
        fm4000FFUGetCounters( (sw), (bank), (fIndex), (nCounters), (frameCnt), (byteCnt) )

/** A legacy synonym for ''fm4000FFUGetPolicer''. */
#define fmFFUGetPolicer(sw, bank, polIdx, com, ex, timestamp) \
        fm4000FFUGetPolicer( (sw), (bank), (polIdx), (com), (ex), (timestamp) )

/** A legacy synonym for ''fm4000FFUGetPolicers''. */
#define fmFFUGetPolicers(sw, bank, fIndex, nPol, com, ex, ts) \
        fm4000FFUGetPolicers( (sw), (bank), (fIndex), (nPol), (com), (ex), (ts) )

/** A legacy synonym for ''fm4000FFUGetPolicerConfig''. */
#define fmFFUGetPolicerConfig(sw, bank, xLstPol, xLstCtNoInt, inColSrc, mkDSCP, mkSwPri, tsWEn, useCache) \
        fm4000FFUGetPolicerConfig( (sw), (bank), (xLstPol), (xLstCtNoInt), (inColSrc), (mkDSCP), (mkSwPri), (tsWEn), (useCache) )

/** A legacy synonym for ''fm4000FFUGetPolicerTimestamp''. */
#define fmFFUGetPolicerTimestamp(sw, timestamp) \
        fm4000FFUGetPolicerTimestamp( (sw), (timestamp) )

/** A legacy synonym for ''fm4000FFUSetPolicerDSCPDownMap''. */
#define fmFFUSetPolicerDSCPDownMap(sw, table, useCache) \
        fm4000FFUSetPolicerDSCPDownMap( (sw), (table), (useCache) )

/** A legacy synonym for ''fm4000FFUSetPolicerSwPriDownMap''. */
#define fmFFUSetPolicerSwPriDownMap(sw, table, useCache) \
        fm4000FFUSetPolicerSwPriDownMap( (sw), (table), (useCache) )

/** A legacy synonym for ''fm4000FFUGetPolicerDSCPDownMap''. */
#define fmFFUGetPolicerDSCPDownMap(sw, table, useCache) \
        fm4000FFUGetPolicerDSCPDownMap( (sw), (table), (useCache) )

/** A legacy synonym for ''fm4000FFUGetPolicerSwPriDownMap''. */
#define fmFFUGetPolicerSwPriDownMap(sw, table, useCache) \
        fm4000FFUGetPolicerSwPriDownMap( (sw), (table), (useCache) )

/** A legacy synonym for ''fm4000FFUSetEgressChunks''. */
#define fmFFUSetEgressChunks(sw, fChunk, nChunks, valScen, dstPhyPortMsk, cascade, useCache) \
        fm4000FFUSetEgressChunks( (sw), (fChunk), (nChunks), (valScen), (dstPhyPortMsk), (cascade), (useCache) )

/** A legacy synonym for ''fm4000FFUSetEgressChunkConfig''. */
#define fmFFUSetEgressChunkConfig(sw, chunk, dstPhyPortMsk, cascade, useCache) \
        fm4000FFUSetEgressChunkConfig( (sw), (chunk), (dstPhyPortMsk), (cascade), (useCache) )

/** A legacy synonym for ''fm4000FFUSetEgressAction''. */
#define fmFFUSetEgressAction(sw, index, drop, logToCpu, count, useCache) \
        fm4000FFUSetEgressAction( (sw), (index), (drop), (logToCpu), (count), (useCache) )

/** A legacy synonym for ''fm4000FFUSetEgressActions''. */
#define fmFFUSetEgressActions(sw, fIndex, nRules, drop, logToCpu, count, useCache) \
        fm4000FFUSetEgressActions( (sw), (fIndex), (nRules), (drop), (logToCpu), (count), (useCache) )

/** A legacy synonym for ''fm4000FFUGetEgressChunkConfig''. */
#define fmFFUGetEgressChunkConfig(sw, chunk, dstPhyPortMsk, cascade, useCache) \
        fm4000FFUGetEgressChunkConfig( (sw), (chunk), (dstPhyPortMsk), (cascade), (useCache) )

/** A legacy synonym for ''fm4000FFUGetEgressAction''. */
#define fmFFUGetEgressAction(sw, index, drop, logToCpu, count, useCache) \
        fm4000FFUGetEgressAction( (sw), (index), (drop), (logToCpu), (count), (useCache) )
        
/** A legacy synonym for ''fm4000FFUGetEgressActions''. */
#define fmFFUGetEgressActions(sw, fIndex, nRules, drop, logToCpu, count, useCache) \
        fm4000FFUGetEgressActions( (sw), (fIndex), (nRules), (drop), (logToCpu), (count), (useCache) )
        
/** A legacy synonym for ''fm4000FFUGetEgressACLCounter''. */
#define fmFFUGetEgressACLCounter(sw, port, frameCount, byteCount) \
        fm4000FFUGetEgressACLCounter( (sw), (port), (frameCount), (byteCount) )
        
/** @} (end of Doxygen group) */


/**************************************************
 * Legacy macros for unpublished functions that have 
 * been renamed. These are used by Fulcrum regression
 * tests.
 **************************************************/

/** \ingroup intMacroSynonym
 * @{ */

/** A legacy synonym for ''fm4000FFUSetEgressACLCounter''. */
#define fmFFUSetEgressACLCounter(sw, port, frameCount, byteCount) \
        fm4000FFUSetEgressACLCounter( (sw), (port), (frameCount), (byteCount) )
        
/** A legacy synonym for ''fm4000FFUSetSliceOwnership''. */
#define fmFFUSetSliceOwnership(sw, owner, first, last) \
        fm4000FFUSetSliceOwnership ( (sw), (owner), (first), (last) )
        
/** A legacy synonym for ''fm4000FFUGetSliceOwnership''. */
#define fmFFUGetSliceOwnership(sw, owner, first, last) \
        fm4000FFUGetSliceOwnership ( (sw), (owner), (first), (last) )

/** A legacy synonym for ''fm4000FFUGetSliceOwnership''. */
#define fmFFUGetSliceOwner(sw, slice, owner) \
        fm4000FFUGetSliceOwner ( (sw), (slice), (owner) )

/** A legacy synonym for ''fm4000FFUGetMapperOwnership''. */
#define fmFFUGetMapperOwnership(sw, owner, resource) \
        fm4000FFUGetMapperOwnership ( (sw), (owner), (resource) )

/** A legacy synonym for ''fm4000FFUSetMapperOwnership''. */
#define fmFFUSetMapperOwnership(sw, owner, resource) \
        fm4000FFUSetMapperOwnership ( (sw), (owner), (resource) )

/** @} (end of Doxygen group) */


/**************************************************/
/** Slice select constants
 *  \ingroup constSliceSelect
 *  \page sliceSelect
 *
 *  These constants are used in the "selects" member
 *  of ''fm_ffuSliceInfo''.  The Sn portion of the
 *  constant name indicates which of the five selects
 *  it may be used in (with S4 meaning SelectTop).
 *  A trailing pair of numbers in the constant
 *  name indicates the bit range of the named
 *  quantity which this select represents.
 **************************************************/
/** \ingroup constSliceSelect
 * @{ */

/** Select destination IP address bits 31:0.
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FFU_MUX_SELECT_DIP_31_0               0

/** Select destination IP address bits 63:32.
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FFU_MUX_SELECT_DIP_63_32              1

/** Select destination IP address bits 95:64.
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FFU_MUX_SELECT_DIP_95_64              2

/** Select destination IP address bits 127:96.
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FFU_MUX_SELECT_DIP_127_96             3

/** Select destination MAC address bits 31:0.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_DMAC_31_0              4

/** Select destination MAC address bits 47:16.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_DMAC_47_16             5

/** Select destination MAC address bits 39:8.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_DMAC_39_8              6

/** Select destination GloRT.
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FFU_MUX_SELECT_DGLORT                 7

/** Select source GloRT.
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FFU_MUX_SELECT_SGLORT                 8

/** Select user bits from F64 tag (8 bits).
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FFU_MUX_SELECT_USER                   9

/** Select ISL command in top nibble and
 *  System Priority in bottom nibble.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_ISLCMD_SYSPRI          10

/** Select VLAN Priority (as mapped by the ''FM_QOS_RX_PRIORITY_MAP''
 *  QoS attribute) in top nibble of top byte,
 *  VLAN ID bits 11:8 in bottom nibble of top byte,
 *  and VLAN ID bits 7:0 in bottom byte.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_VLAN_VPRI              11

/** Select layer 4 destination port.
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FFU_MUX_SELECT_L4DST                  12

/** Select layer 4 source port.
 *  For ICMP frames, the bottom byte contains the Code field.
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FFU_MUX_SELECT_L4SRC                  13

/** Select IPv4 Type of Service octet or IPv6 Traffic Class.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_TOS                    14

/** Select layer 4 field A.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_L4A                    15

/** Select layer 4 field B.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_L4B                    16

/** Select IP length.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_IP_LENGTH              17

/** Select mapped destination and source IP addresses.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_MAP_DIP_MAP_SIP        18

/** Select VLAN Priority (as mapped by the ''FM_QOS_RX_PRIORITY_MAP''
 *  QoS attribute) in top nibble of top byte,
 *  mapped VLAN ID bits 11:8 in bottom nibble of top byte,
 *  and mapped VLAN ID bits 7:0 in bottom byte.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_MAP_VLAN_VPRI          19

/** Select source IP address bits 31:0.
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FFU_MUX_SELECT_SIP_31_0               20

/** Select source IP address bits 63:32.
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FFU_MUX_SELECT_SIP_63_32              21

/** Select source IP address bits 95:64.
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FFU_MUX_SELECT_SIP_95_64              22

/** Select source IP address bits 127:96.
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FFU_MUX_SELECT_SIP_127_96             23

/** Select source MAC address bits 31:0.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_SMAC_31_0              24

/** Select source MAC address bits 47:16.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_SMAC_47_16             25

/** Select source MAC address bits 39:8.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_SMAC_39_8              26

/** Select EtherType.
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FFU_MUX_SELECT_ETHER_TYPE             27

/** Select mapped layer 4 destination port.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_MAP_L4DST              28

/** Select mapped layer 4 source port.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_MAP_L4SRC              29

/** Select IPv4 Protocol or IPv6 Next Header.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_PROT                   30

/** Select Scenario in top 5 bits, Head Fragment in bit 2,
 *  Don't Fragment in bit 1, Flagged IP Options in bit 0.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_MISC                   31

/** Select layer 4 field C.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_L4C                    32

/** Select layer 4 field D.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_L4D                    33

/** Select IPv4 Time To Live or IPv6 Hop Limit.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_TTL                    34

/** Select IPv6 Flow Label.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_IPV6FLOW               35

/** Select mapped physical source port in top nibble
 *  and mapped EtherType in bottom nibble.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_MAP_SRC_MAP_TYPE       36

/** Select mapped source and destination MAC addresses.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_MAP_SMAC_MAP_DMAC      37

/** Select mapped IPv4 Protocol or IPv6 Next Header in top nibble,
 *  and mapped IP length in bottom nibble.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_MAP_PROT_MAP_LENGTH    38

/** Select source physical port mask bits.
 *  This key contains a "1" in the bit position corresponding
 *  to the source port the frame came from.
 *  You should set up your TCAM entry to have a "don't care" in
 *  the bit position for each port number you want to match, and
 *  a "0" in the bit position for each port number you do not
 *  want to match.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_SRC_PHYSICAL_PORT      39

/** Selects zero as the key.  Works in any select position.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_MUX_SELECT_ZERO                   40

/** Select destination MAC address bits 15:0.
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_DMAC_15_0              41

/** Select destination MAC address bits 31:16.
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_DMAC_31_16             42

/** Select destination MAC address bits 47:32.
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_DMAC_47_32             43

/** Select source MAC address bits 15:0.
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_SMAC_15_0              44

/** Select source MAC address bits 31:16.
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_SMAC_31_16             45

/** Select source MAC address bits 47:32.
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_SMAC_47_32             46

/** Select L4 Destination Port ID (16 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_L4_DST_ID              47

/** Select L4 Source Port ID (16 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_L4_SRC_ID              48

/** Select Field 16A (16 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_FIELD_16A              49

/** Select Field 16B (16 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_FIELD_16B              50

/** Select Field 16C (16 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_FIELD_16C              51

/** Select Field 16D (16 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_FIELD_16D              52

/** Select Field 16E (16 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_FIELD_16E              53

/** Select Field 16F (16 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_FIELD_16F              54

/** Select Field 16G (16 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_FIELD_16G              55

/** Select Field 16H (16 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_FIELD_16H              56

/** Select BST Label 16A (16 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_BST_LABEL_16A          57

/** Select BST Label 16B (16 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_BST_LABEL_16B          58

/** Select QOS L3 Priority (8 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_QOS_L3_PRI             59

/** Select Source Port ID 1 (8 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_SRC_PORT_ID1           60

/** Select Source Port ID 2 (8 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_SRC_PORT_ID2           61

/** Select Scenario Flags Bits 7:0 (8 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_SCENARIO_FLAGS_7_0     62

/** Select Scenario Flags Bits 15:8 (8 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_SCENARIO_FLAGS_15_8    63

/** Select L2_VPRI1 and L2_VID1 (16 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_L2_VPRI1_L2_VID1       64

/** Select L2_VPRI2 and L2_VID2 (16 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_L2_VPRI2_L2_VID2       65

/** Select L3_TTL and L3_PROT (16 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_L3_TTL_L3_PROT         66

/** Select L2 DMAC ID1 and L2 SMAC ID1 (8 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_L2_DMAC_SMAC_ID1       67

/** Select L2 DMAC ID2 and L2 Ether Type ID1 (8 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_L2_DMAC_ID2_TYPE_ID1   68

/** Select ISL PRI and L2 VPRI1 (8 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_ISL_PRI_L2_VPRI1       69

/** Select MAP VID1 bits 7:0 (8 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_MAP_VID1_7_0           70

/** Select QOS.W4 and MAP VID1 bits 11:8 (8 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_QOS_W4_MAP_VID1_11_8   71

/** Select MAP VID2 bits 7:0 (8 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_MAP_VID2_7_0           72

/** Select L2 VPRI2 and MAP VID2 bits 11:8 (8 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_VPRI2_MAP_VID2_11_8    73

/** Select DIP ID1 and SIP ID1 (8 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_DIP_SIP_ID1            74

/** Select DIP ID2 and SIP ID2 (8 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_DIP_SIP_ID2            75

/** Select L3 Length Bin and L3 PROT ID1 (8 bits).
 *  \chips  FM6000 */
#define FM_FFU_MUX_SELECT_L3_LEN_BIN_PROT_ID1    76

/** Select top 4 bits, mapped source IP address. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_TOP_SELECT_MAP_SIP                128

/** Select top 4 bits, mapped destination IP address. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_TOP_SELECT_MAP_DIP                129

/** Select top 4 bits, VLAN priority (as mapped by the
 *  ''FM_QOS_RX_PRIORITY_MAP'' QoS attribute).
 *  \chips  FM3000, FM4000 */
#define FM_FFU_TOP_SELECT_VLAN_VPRI_15_12        130

/** Select top 4 bits, destination GloRT bits 15:12.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_TOP_SELECT_DGLORT_15_12           131

/** Select top 4 bits, mapped VLAN bits 3:0.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_TOP_SELECT_VLAN_3_0               132

/** Select top 4 bits, mapped VLAN bits 7:4.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_TOP_SELECT_VLAN_7_4               133

/** Select top 4 bits, mapped VLAN bits 11:8.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_TOP_SELECT_VLAN_11_8              134

/** Select top 4 bits, mapped physical source port.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_TOP_SELECT_MAP_SRC                135

/** Select top 4 bits, mapped EtherType.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_TOP_SELECT_MAP_ETHER_TYPE         136

/** Select top 4 bits, mapped source MAC address.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_TOP_SELECT_MAP_SMAC               137

/** Select top 4 bits, mapped destination MAC address.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_TOP_SELECT_MAP_DMAC               138

/** Select top 4 bits, mapped IPv4 Protocol or IPv6 Next Header.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_TOP_SELECT_MAP_PROT               139

/** Select top 4 bits, mapped IP length.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_TOP_SELECT_MAP_LENGTH             140

/** Select top 4 bits, EtherType ID 1.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_ETHER_TYPE_ID1         141

/** Select top 4 bits, QOS L3 Priority Bits 3:0.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_QOS_L3_PRI_3_0         142

/** Select top 4 bits, QOS L3 Priority Bits 7:4.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_QOS_L3_PRI_7_4         143

/** Select top 4 bits, Scenario Flags Bits 3:0.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_SCENARIO_FLAGS_3_0     144

/** Select top 4 bits, Scenario Flags Bits 7:4.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_SCENARIO_FLAGS_7_4     145

/** Select top 4 bits, Scenario Flags Bits 11:8.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_SCENARIO_FLAGS_11_8    146

/** Select top 4 bits, Scenario Flags Bits 15:12.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_SCENARIO_FLAGS_15_12   147

/** Select top 4 bits, L3 SIP ID1.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_L3_SIP_ID1             148

/** Select top 4 bits, L3 SIP ID2.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_L3_SIP_ID2             149

/** Select top 4 bits, Mapped VID1 Bits 3:0.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_MAP_VID1_3_0           150

/** Select top 4 bits, Mapped VID1 Bits 7:4.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_MAP_VID1_7_4           151

/** Select top 4 bits, Mapped VID1 Bits 11:8.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_MAP_VID1_11_8          152

/** Select top 4 bits, Mapped VID2 Bits 3:0.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_MAP_VID2_3_0           153

/** Select top 4 bits, Mapped VID2 Bits 7:4.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_MAP_VID2_7_4           154

/** Select top 4 bits, Mapped VID2 Bits 11:8.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_MAP_VID2_11_8          155

/** Select top 4 bits, QOS L2 VPRI 1.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_QOS_L2_VPRI1           156

/** Select top 4 bits, QOS L2 VPRI 2, Bits 3:0.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_QOS_L2_VPRI2_3_0       157

/** Select top 4 bits, QOS ISL Priority.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_QOS_ISL_PRI            158

/** Select top 4 bits, QOS W4 Bits 3:0.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_QOS_W4_3_0             159

/** Select top 4 bits, Source MAC Address ID1.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_SMAC_ID1               160

/** Select top 4 bits, Destination MAC Address ID1.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_DMAC_ID1               161

/** Select top 4 bits, Destination MAC Address ID2.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_DMAC_ID2               162

/** Select top 4 bits, ISL User Field, Bits 3:0.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_USER_3_0               163

/** Select top 4 bits, ISL User Field, Bits 7:4.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_USER_7_4               164

/** Select top 4 bits, Source Port ID2 Bits 3:0.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_SRC_PORT_ID2_3_0       165

/** Select top 4 bits, Source Port ID2 Bits 7:4.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_SRC_PORT_ID2_7_4       166

/** Select top 4 bits, L3 Destination IP Address ID1.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_L3_DIP_ID1             167

/** Select top 4 bits, L3 Destination IP Address ID2.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_L3_DIP_ID2             168

/** Select top 4 bits, L3 Prot Field ID1.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_L3_PROT_ID1            169

/** Select top 4 bits, Source Port ID1 Bits 3:0.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_SRC_PORT_ID1_3_0       170

/** Select top 4 bits, Source Port ID1 Bits 7:4.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_SRC_PORT_ID1_7_4       171

/** Select top 4 bits, L3 Length Bin.
 *  \chips  FM6000 */
#define FM_FFU_TOP_SELECT_L3_LENGTH_BIN          172

/** Selects zero as the key.  Alias for FM_FFU_MUX_SELECT_ZERO.
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_FFU_TOP_SELECT_ZERO                   FM_FFU_MUX_SELECT_ZERO


/** Select byte 0, destination IP address bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DIP_31_0''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_DIP_7_0                        FM_FFU_MUX_SELECT_DIP_31_0

/** Select byte 1, destination IP address bits 15:8. 
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DIP_31_0''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_DIP_15_8                       FM_FFU_MUX_SELECT_DIP_31_0

/** Select byte 2, destination IP address bits 23:16. 
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DIP_31_0''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_DIP_23_16                      FM_FFU_MUX_SELECT_DIP_31_0

/** Select byte 3, destination IP address bits 31:24. 
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DIP_31_0''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_DIP_31_24                      FM_FFU_MUX_SELECT_DIP_31_0

/** Select top 4 bits, mapped source IP address. 
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SIP_31_0''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S4_MAP_SIP                        FM_FFU_TOP_SELECT_MAP_SIP

/** Select byte 0, destination IP address bits 39:32.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DIP_63_32''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_DIP_39_32                      FM_FFU_MUX_SELECT_DIP_63_32

/** Select byte 1, destination IP address bits 47:40.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DIP_63_32''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_DIP_47_40                      FM_FFU_MUX_SELECT_DIP_63_32

/** Select byte 2, destination IP address bits 55:48.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DIP_63_32''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_DIP_55_48                      FM_FFU_MUX_SELECT_DIP_63_32

/** Select byte 3, destination IP address bits 63:56.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DIP_63_32''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_DIP_63_56                      FM_FFU_MUX_SELECT_DIP_63_32

/** Select top 4 bits, mapped destination IP address.
 *  Deprecated in favor of ''FM_FFU_TOP_SELECT_MAP_DIP''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S4_MAP_DIP                        FM_FFU_TOP_SELECT_MAP_DIP

/** Select byte 0, destination IP address bits 71:64.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DIP_95_64''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_DIP_71_64                      FM_FFU_MUX_SELECT_DIP_95_64

/** Select byte 1, destination IP address bits 79:72.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DIP_95_64''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_DIP_79_72                      FM_FFU_MUX_SELECT_DIP_95_64

/** Select byte 2, destination IP address bits 87:80.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DIP_95_64''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_DIP_87_80                      FM_FFU_MUX_SELECT_DIP_95_64

/** Select byte 3, destination IP address bits 95:88.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DIP_95_64''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_DIP_95_88                      FM_FFU_MUX_SELECT_DIP_95_64

/** Select top 4 bits, VLAN priority (as mapped by the
 *  ''FM_QOS_RX_PRIORITY_MAP'' QoS attribute).
 *  Deprecated in favor of ''FM_FFU_TOP_SELECT_VLAN_VPRI_15_12''. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S4_VLAN_VPRI_15_12                FM_FFU_TOP_SELECT_VLAN_VPRI_15_12

/** Select byte 0, destination IP address bits 103:96.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DIP_127_96''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_DIP_103_96                     FM_FFU_MUX_SELECT_DIP_127_96

/** Select byte 1, destination IP address bits 111:104.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DIP_127_96''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_DIP_111_104                    FM_FFU_MUX_SELECT_DIP_127_96

/** Select byte 2, destination IP address bits 119:112.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DIP_127_96''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_DIP_119_112                    FM_FFU_MUX_SELECT_DIP_127_96

/** Select byte 3, destination IP address bits 127:120.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DIP_127_96''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_DIP_127_120                    FM_FFU_MUX_SELECT_DIP_127_96

/** Select top 4 bits, destination GloRT bits 15:12.
 *  Deprecated in favor of ''FM_FFU_TOP_SELECT_DGLORT_15_12''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S4_DGLORT_15_12                   FM_FFU_TOP_SELECT_DGLORT_15_12

/** Select byte 0, destination MAC address bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DMAC_31_0''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_DMAC_7_0                       FM_FFU_MUX_SELECT_DMAC_31_0

/** Select byte 1, destination MAC address bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DMAC_31_0''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_DMAC_15_8                      FM_FFU_MUX_SELECT_DMAC_31_0

/** Select byte 2, destination MAC address bits 23:16.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DMAC_31_0''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_DMAC_23_16                     FM_FFU_MUX_SELECT_DMAC_31_0

/** Select byte 3, destination MAC address bits 31:24.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DMAC_31_0''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_DMAC_31_24                     FM_FFU_MUX_SELECT_DMAC_31_0

/** Select top 4 bits, mapped VLAN bits 3:0.
 *  Deprecated in favor of ''FM_FFU_TOP_SELECT_VLAN_3_0''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S4_MAP_VLAN_3_0                   FM_FFU_TOP_SELECT_VLAN_3_0

/** Select byte 0, destination MAC address bits 23:16.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DMAC_47_16''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_DMAC_23_16                     FM_FFU_MUX_SELECT_DMAC_47_16

/** Select byte 1, destination MAC address bits 31:24.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DMAC_47_16''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_DMAC_31_24                     FM_FFU_MUX_SELECT_DMAC_47_16

/** Select byte 2, destination MAC address bits 39:32.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DMAC_47_16''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_DMAC_39_32                     FM_FFU_MUX_SELECT_DMAC_47_16

/** Select byte 3, destination MAC address bits 47:40.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DMAC_47_16''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_DMAC_47_40                     FM_FFU_MUX_SELECT_DMAC_47_16

/** Select top 4 bits, mapped VLAN bits 7:4.
 *  Deprecated in favor of ''FM_FFU_TOP_SELECT_VLAN_7_4''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S4_MAP_VLAN_7_4                   FM_FFU_TOP_SELECT_VLAN_7_4

/** Select byte 0, destination MAC address bits 39:32.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DMAC_39_8''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_DMAC_39_32                     FM_FFU_MUX_SELECT_DMAC_39_8

/** Select byte 1, destination MAC address bits 47:40.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DMAC_39_8''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_DMAC_47_40                     FM_FFU_MUX_SELECT_DMAC_39_8

/** Select byte 2, destination MAC address bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DMAC_39_8''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_DMAC_7_0                       FM_FFU_MUX_SELECT_DMAC_39_8

/** Select byte 3, destination MAC address bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DMAC_39_8''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_DMAC_15_8                      FM_FFU_MUX_SELECT_DMAC_39_8

/** Select top 4 bits, mapped VLAN bits 11:8.
 *  Deprecated in favor of ''FM_FFU_TOP_SELECT_VLAN_11_8''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S4_MAP_VLAN_11_8                  FM_FFU_TOP_SELECT_VLAN_11_8

/** Select byte 0, destination GloRT bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DGLORT''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_DGLORT_7_0                     FM_FFU_MUX_SELECT_DGLORT

/** Select byte 1, destination GloRT bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DGLORT''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_DGLORT_15_8                    FM_FFU_MUX_SELECT_DGLORT

/** Select byte 2, source GloRT bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SGLORT''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_SGLORT_7_0                     FM_FFU_MUX_SELECT_SGLORT

/** Select byte 3, source GloRT bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SGLORT''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_SGLORT_15_8                    FM_FFU_MUX_SELECT_SGLORT

/** Select byte 0, source GloRT bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SGLORT''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_SGLORT_7_0                     FM_FFU_MUX_SELECT_SGLORT

/** Select byte 1, source GloRT bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SGLORT''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_SGLORT_15_8                    FM_FFU_MUX_SELECT_SGLORT

/** Select byte 2, user bits from F64 tag.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_USER''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_USER                           FM_FFU_MUX_SELECT_USER

/** Select byte 3, ISL command in top nibble and
 *  System Priority in bottom nibble.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_ISLCMD_SYSPRI''. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_ISLCMD_SYSPRI                  FM_FFU_MUX_SELECT_ISLCMD_SYSPRI

/** Select byte 0, user bits from F64 tag.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_USER''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_USER                           FM_FFU_MUX_SELECT_USER

/** Select byte 1, ISL command in top nibble and
 *  System Priority in bottom nibble.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_ISLCMD_SYSPRI''. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_ISLCMD_SYSPRI                  FM_FFU_MUX_SELECT_ISLCMD_SYSPRI

/** Select byte 2, destination GloRT bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DGLORT''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_DGLORT_7_0                     FM_FFU_MUX_SELECT_DGLORT

/** Select byte 3, destination GloRT bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_DGLORT''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_DGLORT_15_8                    FM_FFU_MUX_SELECT_DGLORT

/** Select byte 0, VLAN ID bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_VLAN_VPRI''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_VLAN_VPRI_7_0                  FM_FFU_MUX_SELECT_VLAN_VPRI

/** Select byte 1, VLAN Priority (as mapped by the
 *  ''FM_QOS_RX_PRIORITY_MAP'' QoS attribute) in top nibble,
 *  VLAN ID bits 11:8 in bottom nibble.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_VLAN_VPRI''. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_VLAN_VPRI_15_8                 FM_FFU_MUX_SELECT_VLAN_VPRI

/** Select byte 2, VLAN ID bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_VLAN_VPRI''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_VLAN_VPRI_7_0                  FM_FFU_MUX_SELECT_VLAN_VPRI

/** Select byte 3, VLAN Priority (as mapped by the
 *  ''FM_QOS_RX_PRIORITY_MAP'' QoS attribute) in top nibble,
 *  VLAN ID bits 11:8 in bottom nibble.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_VLAN_VPRI''. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_VLAN_VPRI_15_8                 FM_FFU_MUX_SELECT_VLAN_VPRI

/** Select byte 0, layer 4 destination port bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4DST''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_L4DST_7_0                      FM_FFU_MUX_SELECT_L4DST

/** Select byte 1, layer 4 destination port bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4DST''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_L4DST_15_8                     FM_FFU_MUX_SELECT_L4DST

/** Select byte 2, layer 4 destination port bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4DST''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_L4DST_7_0                      FM_FFU_MUX_SELECT_L4DST

/** Select byte 3, layer 4 destination port bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4DST''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_L4DST_15_8                     FM_FFU_MUX_SELECT_L4DST

/** Select byte 0, layer 4 source port bits 7:0.
 *  For ICMP frames, this byte contains the Code field.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4SRC''. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_L4SRC_7_0                      FM_FFU_MUX_SELECT_L4SRC

/** Select byte 1, layer 4 source port bits 15:8.
 *  For ICMP frames, this byte contains the Type field.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4SRC''. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_L4SRC_15_8                     FM_FFU_MUX_SELECT_L4SRC

/** Select byte 2, layer 4 source port bits 7:0.
 *  For ICMP frames, this byte contains the Code field.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4SRC''. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_L4SRC_7_0                      FM_FFU_MUX_SELECT_L4SRC

/** Select byte 3, layer 4 source port bits 15:8.
 *  For ICMP frames, this byte contains the Type field.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4SRC''. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_L4SRC_15_8                     FM_FFU_MUX_SELECT_L4SRC

/** Select byte 0, IPv4 Type of Service octet or IPv6 Traffic Class.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_TOS''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_TOS                            FM_FFU_MUX_SELECT_TOS

/** Select byte 1, IPv4 Type of Service octet or IPv6 Traffic Class.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_TOS''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_TOS                            FM_FFU_MUX_SELECT_TOS

/** Select byte 2, IPv4 Type of Service octet or IPv6 Traffic Class.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_TOS''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_TOS                            FM_FFU_MUX_SELECT_TOS

/** Select byte 3, IPv4 Type of Service octet or IPv6 Traffic Class.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_TOS''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_TOS                            FM_FFU_MUX_SELECT_TOS

/** Select byte 0, layer 4 field A bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4A''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_L4A_7_0                        FM_FFU_MUX_SELECT_L4A

/** Select byte 1, layer 4 field A bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4A''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_L4A_15_8                       FM_FFU_MUX_SELECT_L4A

/** Select byte 2, layer 4 field A bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4A''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_L4A_7_0                        FM_FFU_MUX_SELECT_L4A

/** Select byte 3, layer 4 field A bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4A''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_L4A_15_8                       FM_FFU_MUX_SELECT_L4A

/** Select byte 0, layer 4 field B bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4B''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_L4B_7_0                        FM_FFU_MUX_SELECT_L4B

/** Select byte 1, layer 4 field B bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4B''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_L4B_15_8                       FM_FFU_MUX_SELECT_L4B

/** Select byte 2, layer 4 field B bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4B''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_L4B_7_0                        FM_FFU_MUX_SELECT_L4B

/** Select byte 3, layer 4 field B bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4B''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_L4B_15_8                       FM_FFU_MUX_SELECT_L4B

/** Select byte 0, IP length bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_IP_LENGTH''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_IP_LENGTH_7_0                  FM_FFU_MUX_SELECT_IP_LENGTH

/** Select byte 1, IP length bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_IP_LENGTH''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_IP_LENGTH_15_8                 FM_FFU_MUX_SELECT_IP_LENGTH

/** Select byte 2, IP length bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_IP_LENGTH''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_IP_LENGTH_7_0                  FM_FFU_MUX_SELECT_IP_LENGTH

/** Select byte 3, IP length bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_IP_LENGTH''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_IP_LENGTH_15_8                 FM_FFU_MUX_SELECT_IP_LENGTH

/** Select byte 0, mapped destination and source IP addresses.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_DIP_MAP_SIP''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_MAP_DIP_MAP_SIP                FM_FFU_MUX_SELECT_MAP_DIP_MAP_SIP

/** Select byte 1, mapped destination and source IP addresses.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_DIP_MAP_SIP''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_MAP_DIP_MAP_SIP                FM_FFU_MUX_SELECT_MAP_DIP_MAP_SIP

/** Select byte 2, mapped destination and source IP addresses.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_DIP_MAP_SIP''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_MAP_DIP_MAP_SIP                FM_FFU_MUX_SELECT_MAP_DIP_MAP_SIP

/** Select byte 3, mapped destination and source IP addresses.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_DIP_MAP_SIP''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_MAP_DIP_MAP_SIP                FM_FFU_MUX_SELECT_MAP_DIP_MAP_SIP

/** Select byte 0, mapped VLAN bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_VLAN_VPRI''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_MAP_VLAN_7_0                   FM_FFU_MUX_SELECT_MAP_VLAN_VPRI

/** Select byte 1, VLAN priority (as mapped by the
 *  ''FM_QOS_RX_PRIORITY_MAP'' QoS attribute) in top nibble and
 *  mapped VLAN bits 11:8 in bottom nibble.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_VLAN_VPRI''. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_VLAN_VPRI_15_12_MAP_VLAN_11_8  FM_FFU_MUX_SELECT_MAP_VLAN_VPRI

/** Select byte 2, mapped VLAN bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_VLAN_VPRI''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_MAP_VLAN_7_0                   FM_FFU_MUX_SELECT_MAP_VLAN_VPRI

/** Select byte 3, VLAN priority (as mapped by the
 *  ''FM_QOS_RX_PRIORITY_MAP'' QoS attribute) in top nibble and
 *  mapped VLAN bits 11:8 in bottom nibble.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_VLAN_VPRI''. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_VLAN_VPRI_15_12_MAP_VLAN_11_8  FM_FFU_MUX_SELECT_MAP_VLAN_VPRI

/** Select byte 0, source IP address bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SIP_31_0''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_SIP_7_0                        FM_FFU_MUX_SELECT_SIP_31_0

/** Select byte 1, source IP address bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SIP_31_0''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_SIP_15_8                       FM_FFU_MUX_SELECT_SIP_31_0

/** Select byte 2, source IP address bits 23:16.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SIP_31_0''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_SIP_23_16                      FM_FFU_MUX_SELECT_SIP_31_0

/** Select byte 3, source IP address bits 31:24.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SIP_31_0''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_SIP_31_24                      FM_FFU_MUX_SELECT_SIP_31_0

/** Select top 4 bits, mapped physical source port.
 *  Deprecated in favor of ''FM_FFU_TOP_SELECT_MAP_SRC''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S4_MAP_SRC                        FM_FFU_TOP_SELECT_MAP_SRC

/** Select byte 0, source IP address bits 39:32.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SIP_63_32''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_SIP_39_32                      FM_FFU_MUX_SELECT_SIP_63_32

/** Select byte 1, source IP address bits 47:40.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SIP_63_32''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_SIP_47_40                      FM_FFU_MUX_SELECT_SIP_63_32

/** Select byte 2, source IP address bits 55:48.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SIP_63_32''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_SIP_55_48                      FM_FFU_MUX_SELECT_SIP_63_32

/** Select byte 3, source IP address bits 63:56.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SIP_63_32''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_SIP_63_56                      FM_FFU_MUX_SELECT_SIP_63_32

/** Select top 4 bits, mapped EtherType.
 *  Deprecated in favor of ''FM_FFU_TOP_SELECT_MAP_ETHER_TYPE''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S4_MAP_TYPE                       FM_FFU_TOP_SELECT_MAP_ETHER_TYPE

/** Select byte 0, source IP address bits 71:64.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SIP_95_64''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_SIP_71_64                      FM_FFU_MUX_SELECT_SIP_95_64

/** Select byte 1, source IP address bits 79:72.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SIP_95_64''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_SIP_79_72                      FM_FFU_MUX_SELECT_SIP_95_64

/** Select byte 2, source IP address bits 87:80.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SIP_95_64''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_SIP_87_80                      FM_FFU_MUX_SELECT_SIP_95_64

/** Select byte 3, source IP address bits 95:88.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SIP_95_64''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_SIP_95_88                      FM_FFU_MUX_SELECT_SIP_95_64

/** Select top 4 bits, mapped source MAC address.
 *  Deprecated in favor of ''FM_FFU_TOP_SELECT_MAP_SMAC''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S4_MAP_SMAC                       FM_FFU_TOP_SELECT_MAP_SMAC

/** Select byte 0, source IP address bits 103:96.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SIP_127_96''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_SIP_103_96                     FM_FFU_MUX_SELECT_SIP_127_96

/** Select byte 1, source IP address bits 111:104.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SIP_127_96''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_SIP_111_104                    FM_FFU_MUX_SELECT_SIP_127_96

/** Select byte 2, source IP address bits 119:112.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SIP_127_96''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_SIP_119_112                    FM_FFU_MUX_SELECT_SIP_127_96

/** Select byte 3, source IP address bits 127:120.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SIP_127_96''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_SIP_127_120                    FM_FFU_MUX_SELECT_SIP_127_96

/** Select top 4 bits, mapped destination MAC address.
 *  Deprecated in favor of ''FM_FFU_TOP_SELECT_MAP_DMAC''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S4_MAP_DMAC                       FM_FFU_TOP_SELECT_MAP_DMAC

/** Select byte 0, source MAC address bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SMAC_31_0''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_SMAC_7_0                       FM_FFU_MUX_SELECT_SMAC_31_0

/** Select byte 1, source MAC address bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SMAC_31_0''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_SMAC_15_8                      FM_FFU_MUX_SELECT_SMAC_31_0

/** Select byte 2, source MAC address bits 23:16.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SMAC_31_0''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_SMAC_23_16                     FM_FFU_MUX_SELECT_SMAC_31_0

/** Select byte 3, source MAC address bits 31:24.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SMAC_31_0''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_SMAC_31_24                     FM_FFU_MUX_SELECT_SMAC_31_0

/** Select top 4 bits, mapped IPv4 Protocol or IPv6 Next Header.
 *  Deprecated in favor of ''FM_FFU_TOP_SELECT_MAP_PROT''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S4_MAP_PROT                       FM_FFU_TOP_SELECT_MAP_PROT

/** Select byte 0, source MAC address bits 23:16.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SMAC_47_16''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_SMAC_23_16                     FM_FFU_MUX_SELECT_SMAC_47_16

/** Select byte 1, source MAC address bits 31:24.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SMAC_47_16''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_SMAC_31_24                     FM_FFU_MUX_SELECT_SMAC_47_16

/** Select byte 2, source MAC address bits 39:32.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SMAC_47_16''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_SMAC_39_32                     FM_FFU_MUX_SELECT_SMAC_47_16

/** Select byte 3, source MAC address bits 47:40.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SMAC_47_16''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_SMAC_47_40                     FM_FFU_MUX_SELECT_SMAC_47_16

/** Select top 4 bits, mapped IP length.
 *  Deprecated in favor of ''FM_FFU_TOP_SELECT_MAP_LENGTH''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S4_MAP_LENGTH                     FM_FFU_TOP_SELECT_MAP_LENGTH

/** Select byte 0, source MAC address bits 39:32.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SMAC_39_8''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_SMAC_39_32                     FM_FFU_MUX_SELECT_SMAC_39_8

/** Select byte 1, source MAC address bits 47:40.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SMAC_39_8''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_SMAC_47_40                     FM_FFU_MUX_SELECT_SMAC_39_8

/** Select byte 2, source MAC address bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SMAC_39_8''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_SMAC_7_0                       FM_FFU_MUX_SELECT_SMAC_39_8

/** Select byte 3, source MAC address bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SMAC_39_8''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_SMAC_15_8                      FM_FFU_MUX_SELECT_SMAC_39_8

/** Select byte 0, EtherType bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_ETHER_TYPE''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_TYPE_7_0                       FM_FFU_MUX_SELECT_ETHER_TYPE

/** Select byte 1, EtherType bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_ETHER_TYPE''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_TYPE_15_8                      FM_FFU_MUX_SELECT_ETHER_TYPE

/** Select byte 2, EtherType bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_ETHER_TYPE''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_TYPE_7_0                       FM_FFU_MUX_SELECT_ETHER_TYPE

/** Select byte 3, EtherType bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_ETHER_TYPE''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_TYPE_15_8                      FM_FFU_MUX_SELECT_ETHER_TYPE

/** Select byte 0, mapped layer 4 destination port bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_L4DST''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_MAP_L4DST_7_0                  FM_FFU_MUX_SELECT_MAP_L4DST

/** Select byte 1, mapped layer 4 destination port bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_L4DST''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_MAP_L4DST_15_8                 FM_FFU_MUX_SELECT_MAP_L4DST

/** Select byte 2, mapped layer 4 destination port bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_L4DST''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_MAP_L4DST_7_0                  FM_FFU_MUX_SELECT_MAP_L4DST

/** Select byte 3, mapped layer 4 destination port bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_L4DST''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_MAP_L4DST_15_8                 FM_FFU_MUX_SELECT_MAP_L4DST

/** Select byte 0, mapped layer 4 source port bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_L4SRC''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_MAP_L4SRC_7_0                  FM_FFU_MUX_SELECT_MAP_L4SRC

/** Select byte 1, mapped layer 4 source port bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_L4SRC''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_MAP_L4SRC_15_8                 FM_FFU_MUX_SELECT_MAP_L4SRC

/** Select byte 2, mapped layer 4 source port bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_L4SRC''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_MAP_L4SRC_7_0                  FM_FFU_MUX_SELECT_MAP_L4SRC

/** Select byte 3, mapped layer 4 source port bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_L4SRC''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_MAP_L4SRC_15_8                 FM_FFU_MUX_SELECT_MAP_L4SRC

/** Select byte 0, IPv4 Protocol or IPv6 Next Header.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_PROT''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_PROT                           FM_FFU_MUX_SELECT_PROT

/** Select byte 1, IPv4 Protocol or IPv6 Next Header.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_PROT''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_PROT                           FM_FFU_MUX_SELECT_PROT

/** Select byte 2, IPv4 Protocol or IPv6 Next Header.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_PROT''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_PROT                           FM_FFU_MUX_SELECT_PROT

/** Select byte 3, IPv4 Protocol or IPv6 Next Header.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_PROT''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_PROT                           FM_FFU_MUX_SELECT_PROT

/** Select byte 0, Scenario in top 5 bits, Head Fragment in bit 2,
 *  Don't Fragment in bit 1, Flagged IP Options in bit 0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MISC''. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_MISC                           FM_FFU_MUX_SELECT_MISC

/** Select byte 1, Scenario in top 5 bits, Head Fragment in bit 2,
 *  Don't Fragment in bit 1, Flagged IP Options in bit 0.
  *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MISC''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_MISC                           FM_FFU_MUX_SELECT_MISC

/** Select byte 2, Scenario in top 5 bits, Head Fragment in bit 2,
 *  Don't Fragment in bit 1, Flagged IP Options in bit 0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MISC''. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_MISC                           FM_FFU_MUX_SELECT_MISC

/** Select byte 3, Scenario in top 5 bits, Head Fragment in bit 2,
 *  Don't Fragment in bit 1, Flagged IP Options in bit 0.
  *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MISC''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_MISC                           FM_FFU_MUX_SELECT_MISC

/** Select byte 0, layer 4 field C bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4C''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_L4C_7_0                        FM_FFU_MUX_SELECT_L4C

/** Select byte 1, layer 4 field C bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4C''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_L4C_15_8                       FM_FFU_MUX_SELECT_L4C

/** Select byte 2, layer 4 field C bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4C''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_L4C_7_0                        FM_FFU_MUX_SELECT_L4C

/** Select byte 3, layer 4 field C bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4C''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_L4C_15_8                       FM_FFU_MUX_SELECT_L4C

/** Select byte 0, layer 4 field D bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4D''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_L4D_7_0                        FM_FFU_MUX_SELECT_L4D

/** Select byte 1, layer 4 field D bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4D''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_L4D_15_8                       FM_FFU_MUX_SELECT_L4D

/** Select byte 2, layer 4 field D bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4D''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_L4D_7_0                        FM_FFU_MUX_SELECT_L4D

/** Select byte 3, layer 4 field D bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_L4D''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_L4D_15_8                       FM_FFU_MUX_SELECT_L4D

/** Select byte 0, IPv4 Time To Live or IPv6 Hop Limit.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_TTL''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_TTL                            FM_FFU_MUX_SELECT_TTL

/** Select byte 1, IPv4 Time To Live or IPv6 Hop Limit.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_TTL''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_TTL                            FM_FFU_MUX_SELECT_TTL

/** Select byte 2, IPv4 Time To Live or IPv6 Hop Limit.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_TTL''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_TTL                            FM_FFU_MUX_SELECT_TTL

/** Select byte 3, IPv4 Time To Live or IPv6 Hop Limit.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_TTL''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_TTL                            FM_FFU_MUX_SELECT_TTL

/** Select byte 0, IPv6 Flow Label bits 7:0.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_IPV6FLOW''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_IPv6FLOW_7_0                   FM_FFU_MUX_SELECT_IPV6FLOW

/** Select byte 1, IPv6 Flow Label bits 15:8.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_IPV6FLOW''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_IPv6FLOW_15_8                  FM_FFU_MUX_SELECT_IPV6FLOW

/** Select byte 2, IPv6 Flow Label bits 19:16.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_IPV6FLOW''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_IPv6FLOW_19_16                 FM_FFU_MUX_SELECT_IPV6FLOW

/** Select byte 0, mapped physical source port in top nibble
 *  and mapped EtherType in bottom nibble.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_SRC_MAP_TYPE''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_MAP_SRC_MAP_TYPE               FM_FFU_MUX_SELECT_MAP_SRC_MAP_TYPE

/** Select byte 1, mapped physical source port in top nibble
 *  and mapped EtherType in bottom nibble.
  *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_SRC_MAP_TYPE''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_MAP_SRC_MAP_TYPE               FM_FFU_MUX_SELECT_MAP_SRC_MAP_TYPE

/** Select byte 2, mapped physical source port in top nibble
 *  and mapped EtherType in bottom nibble.
  *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_SRC_MAP_TYPE''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_MAP_SRC_MAP_TYPE               FM_FFU_MUX_SELECT_MAP_SRC_MAP_TYPE

/** Select byte 3, mapped physical source port in top nibble
 *  and mapped EtherType in bottom nibble.
  *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_SRC_MAP_TYPE''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_MAP_SRC_MAP_TYPE               FM_FFU_MUX_SELECT_MAP_SRC_MAP_TYPE

/** Select byte 0, mapped source and destination MAC addresses.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_SMAC_MAP_DMAC''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_MAP_SMAC_MAP_DMAC              FM_FFU_MUX_SELECT_MAP_SMAC_MAP_DMAC

/** Select byte 1, mapped source and destination MAC addresses.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_SMAC_MAP_DMAC''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_MAP_SMAC_MAP_DMAC              FM_FFU_MUX_SELECT_MAP_SMAC_MAP_DMAC

/** Select byte 2, mapped source and destination MAC addresses.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_SMAC_MAP_DMAC''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_MAP_SMAC_MAP_DMAC              FM_FFU_MUX_SELECT_MAP_SMAC_MAP_DMAC

/** Select byte 3, mapped source and destination MAC addresses.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_SMAC_MAP_DMAC''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_MAP_SMAC_MAP_DMAC              FM_FFU_MUX_SELECT_MAP_SMAC_MAP_DMAC

/** Select byte 0, mapped IPv4 Protocol or IPv6 Next Header in top nibble,
 *  and mapped IP length in bottom nibble.
  *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_PROT_MAP_LENGTH''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_MAP_PROT_MAP_LENGTH            FM_FFU_MUX_SELECT_MAP_PROT_MAP_LENGTH

/** Select byte 1, mapped IPv4 Protocol or IPv6 Next Header in top nibble,
 *  and mapped IP length in bottom nibble.
  *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_PROT_MAP_LENGTH''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_MAP_PROT_MAP_LENGTH            FM_FFU_MUX_SELECT_MAP_PROT_MAP_LENGTH

/** Select byte 2, mapped IPv4 Protocol or IPv6 Next Header in top nibble,
 *  and mapped IP length in bottom nibble.
  *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_PROT_MAP_LENGTH''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_MAP_PROT_MAP_LENGTH            FM_FFU_MUX_SELECT_MAP_PROT_MAP_LENGTH

/** Select byte 3, mapped IPv4 Protocol or IPv6 Next Header in top nibble,
 *  and mapped IP length in bottom nibble.
  *  Deprecated in favor of ''FM_FFU_MUX_SELECT_MAP_PROT_MAP_LENGTH''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_MAP_PROT_MAP_LENGTH            FM_FFU_MUX_SELECT_MAP_PROT_MAP_LENGTH

/** Select byte 0, source physical port mask bits 7:0.
 *  This key contains a "1" in the bit position corresponding
 *  to the source port the frame came from.
 *  You should set up your TCAM entry to have a "don't care" in
 *  the bit position for each port number you want to match, and
 *  a "0" in the bit position for each port number you do not
 *  want to match.
  *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SRC_PHYSICAL_PORT''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S0_SRC_PHYSICAL_PORT_7_0          FM_FFU_MUX_SELECT_SRC_PHYSICAL_PORT

/** Select byte 1, source physical port mask bits 15:8.
 *  This key contains a "1" in the bit position corresponding
 *  to the source port the frame came from.
 *  You should set up your TCAM entry to have a "don't care" in
 *  the bit position for each port number you want to match, and
 *  a "0" in the bit position for each port number you do not
 *  want to match.
  *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SRC_PHYSICAL_PORT''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S1_SRC_PHYSICAL_PORT_15_8         FM_FFU_MUX_SELECT_SRC_PHYSICAL_PORT

/** Select byte 2, source physical port mask bits 23:16.
 *  This key contains a "1" in the bit position corresponding
 *  to the source port the frame came from.
 *  You should set up your TCAM entry to have a "don't care" in
 *  the bit position for each port number you want to match, and
 *  a "0" in the bit position for each port number you do not
 *  want to match.
  *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SRC_PHYSICAL_PORT''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S2_SRC_PHYSICAL_PORT_23_16        FM_FFU_MUX_SELECT_SRC_PHYSICAL_PORT

/** Select byte 3, source physical port mask bits 31:24.
 *  This key contains a "1" in the bit position corresponding
 *  to the source port the frame came from.
 *  You should set up your TCAM entry to have a "don't care" in
 *  the bit position for each port number you want to match, and
 *  a "0" in the bit position for each port number you do not
 *  want to match.
  *  Deprecated in favor of ''FM_FFU_MUX_SELECT_SRC_PHYSICAL_PORT''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_S3_SRC_PHYSICAL_PORT_31_24        FM_FFU_MUX_SELECT_SRC_PHYSICAL_PORT

/** Selects zero as the key.  Works in any select position.
 *  Deprecated in favor of ''FM_FFU_MUX_SELECT_ZERO''.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SEL_ZERO                          FM_FFU_MUX_SELECT_ZERO

/** @} (end of Doxygen group) */

/**************************************************/
/** Scenario constants
 *  \ingroup constScenario
 *  \page scenario
 *
 *  These constants are used in the "validScenarios"
 *  member of ''fm_ffuSliceInfo''.  See the "Slice
 *  Activation and Overloading" section of the
 *  Frame Filtering and Forwarding Unit chapter of
 *  the FM4000 datasheet.
 **************************************************/
/** \ingroup constScenario
 * @{ */

/** Switched, not routed, not an IP packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_NOTIP_SW                      0x00000001

/** Switched on Glort, not routed, not an IP packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_NOTIP_SWGLORT                 0x00000010

/** Management frame, not an IP packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_NOTIP_MGMT                    0x00000100

/** Specially handled (e.g., mirrored or trapped), not an IP packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_NOTIP_SPECIAL                 0x00001000

/** Routable unicast, not an IP packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_NOTIP_ROUTABLE                0x00010000

/** Glort-routed unicast, not an IP packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_NOTIP_RTDGLORT                0x00100000

/** Routable multicast, not an IP packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_NOTIP_ROUTABLEMCAST           0x01000000

/** Glort-routed multicast, not an IP packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_NOTIP_RTDMCASTGLORT           0x10000000

/** Switched, not routed, IPv4 packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv4_SW                       0x00000002

/** Switched on Glort, not routed, IPv4 packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv4_SWGLORT                  0x00000020

/** Management frame, IPv4 packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv4_MGMT                     0x00000200

/** Specially handled (e.g., mirrored or trapped), IPv4 packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv4_SPECIAL                  0x00002000

/** Routable unicast, IPv4 packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv4_ROUTABLE                 0x00020000

/** Glort-routed unicast, IPv4 packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv4_RTDGLORT                 0x00200000

/** Routable multicast, IPv4 packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv4_ROUTABLEMCAST            0x02000000

/** Glort-routed multicast, IPv4 packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv4_RTDMCASTGLORT            0x20000000

/** Switched, not routed, IPv6 packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv6_SW                       0x00000004

/** Switched on Glort, not routed, IPv6 packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv6_SWGLORT                  0x00000040

/** Management frame, IPv6 packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv6_MGMT                     0x00000400

/** Specially handled (e.g., mirrored or trapped), IPv6 packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv6_SPECIAL                  0x00004000

/** Routable unicast, IPv6 packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv6_ROUTABLE                 0x00040000

/** Glort-routed unicast, IPv6 packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv6_RTDGLORT                 0x00400000

/** Routable multicast, IPv6 packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv6_ROUTABLEMCAST            0x04000000

/** Glort-routed multicast, IPv6 packet. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv6_RTDMCASTGLORT            0x40000000

/** Switched, not routed, IPv6 with an IPv4-in-IPv6 DIP. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv4INIPv6_SW                 0x00000008

/** Switched on Glort, not routed, IPv6 with an IPv4-in-IPv6 DIP. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv4INIPv6_SWGLORT            0x00000080

/** Management frame, IPv6 with an IPv4-in-IPv6 DIP. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv4INIPv6_MGMT               0x00000800

/** Specially handled (e.g., mirrored or trapped), IPv6 with an IPv4-in-IPv6 
 *  DIP. 
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv4INIPv6_SPECIAL            0x00008000

/** Routable unicast, IPv6 with an IPv4-in-IPv6 DIP.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv4INIPv6_ROUTABLE           0x00080000

/** Glort-routed unicast, IPv6 with an IPv4-in-IPv6 DIP.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv4INIPv6_RTDGLORT           0x00800000

/** Routable multicast, IPv6 with an IPv4-in-IPv6 DIP.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv4INIPv6_ROUTABLEMCAST      0x08000000

/** Glort-routed multicast, IPv6 with an IPv4-in-IPv6 DIP.
 *  \chips  FM3000, FM4000 */
#define FM_FFU_SCN_IPv4INIPv6_RTDMCASTGLORT      0x80000000

/** @} (end of Doxygen group) */

/** Defines the type of callers for the FFU API */
/**************************************************/
/** \ingroup typeEnum
 * These enumerated values indicate the owner of
 * an FFU mapping resource. They are used for some
 * ''API Attributes''.
 **************************************************/
typedef enum
{
    /** The resource is unowned. */
    FM_FFU_OWNER_NONE = 0,

    /** The resource is owned by the application. */
    FM_FFU_OWNER_APPLICATION,

    /** The resource is owned by the routing subsystem. */
    FM_FFU_OWNER_ROUTING,

    /** The resource is owned by the ACL subsystem. */
    FM_FFU_OWNER_ACL,

    /** The resource is owned by the TCAM learning subsystem. */
    FM_FFU_OWNER_LEARNING,

    /** The resource is owned by the cVlan subsystem. */
    FM_FFU_OWNER_CVLAN,

    /* --- Add other owners above this line --- */

    /** For internal use only. */
    FM_FFU_OWNER_MAX

} fm_ffuOwnerType;


/**************************************************/
/** \ingroup intTypeEnum
 * These enumerated values indicate the type of
 * FFU mapping resource.
 **************************************************/
typedef enum
{
    /** MAC address. */
    FM_FFU_MAPPER_MAC = 0,

    /** VLAN. */
    FM_FFU_MAPPER_VLAN,

    /** IP address. */
    FM_FFU_MAPPER_IP,

    /** IP protocol number. */
    FM_FFU_MAPPER_PROT,

    /** Layer 4 port number. */
    FM_FFU_MAPPER_L4,

    /** Ethernet type. */
    FM_FFU_MAPPER_ETH_TYPE,

    /** IP packet length. */
    FM_FFU_MAPPER_LENGTH,

    /** Source port number. */
    FM_FFU_MAPPER_SRC,

    /* --- Add other mapping resources above this line -- */

    /** For internal use only. */
    FM_FFU_MAPPER_MAX

} fm_ffuMappingResource;


/** The number of selects in each minslice.
 *  \ingroup constSystem
 */
#define FM_FFU_SELECTS_PER_MINSLICE  5

/**************************************************/
/** \ingroup typeStruct
 *  fm_ffuSliceInfo is defined by the user to
 *  indicate properties of an FFU slice.  Multiple
 *  minslices of key or of action may be chained
 *  together to create larger keys or to execute
 *  multiple actions.
 *                                              \lb\lb
 *  Each minslice has 36 bits of ternary CAM.  The
 *  key fed into each minslice has five separately
 *  configurable parts (four 8-bit and one 4-bit).
 **************************************************/
typedef struct _fm_ffuSliceInfo
{
    /** One bit for each of the 32 possible scenarios, indicating
     *  whether this slice is valid during that scenario. Affects
     *  the FFU_SLICE_VALID register. */
    fm_uint32      validScenarios;

    /** The case (0 or 1) which this slice uses.  Two slices
     *  can occupy the same range of minslices if and only if
     *  they use opposite cases, and their validScenarios are
     *  non-overlapping.  Case has no meaning, other than to
     *  disambiguate two slices occupying the same space. */
    fm_byte        kase;

    /** The first minslice containing the key for this slice. */
    fm_byte        keyStart;

    /** The last minslice containing the key for this slice, and the
     *  first minslice containing the action for this slice.  Must
     *  be greater than or equal to keyStart. */
    fm_byte        keyEnd;

    /** The last minslice containing the action for this slice.
     *  Must be greater than or equal to keyEnd. */
    fm_byte        actionEnd;

    /** Points to an array of bytes, of length                              \lb
     *  5 * (1 + keyEnd - keyStart).                                        \lb
     *  The bytes contain the five selects for each minslice,
     *  starting with Select0 and ending with SelectTop, then moving
     *  on to the next minslice, etc. */
    const fm_byte *selects;

} fm_ffuSliceInfo;

/**************************************************/
/** \ingroup typeEnum
 *  Referenced by ''fm_ffuAction'', these enumerated 
 *  values specify an action to be taken when an 
 *  FFU rule hits. Any data required by the action
 *  will be specified by ''fm_ffuAction'' in
 *  a ''fm_ffuActionData'' union.
 **************************************************/
typedef enum
{
    /** Do nothing. */
    FM_FFU_ACTION_NOP = 0,

    /** Route this frame using an ARP table entry. */
    FM_FFU_ACTION_ROUTE_ARP,

    /** Route this frame to s specific logical port. */
    FM_FFU_ACTION_ROUTE_LOGICAL_PORT,

    /** Set (or clear) the flags drop, trap, log, noRoute, and rxMirror. */
    FM_FFU_ACTION_SET_FLAGS,

    /** Set the trigger value. */
    FM_FFU_ACTION_SET_TRIGGER,

    /** Set the user field. */
    FM_FFU_ACTION_SET_USER,

    /** Set the DSCP field or the VLAN field or neither,
     *  and also optionally set the switch priority, the VLAN
     *  priority, or both.  (But if both priorities are set, they
     *  must be set to the same value.) */
    FM_FFU_ACTION_SET_FIELDS,

    /** For internal use only. */
    FM_FFU_ACTION_MAX

} fm_ffuActionType;

/**************************************************/
/** \ingroup typeEnum
 * These enumerated values specify a field to set
 * when using ''FM_FFU_ACTION_SET_FIELDS''.
 **************************************************/
typedef enum
{
    /** Set neither the DSCP field nor the VLAN field. */
    FM_FFU_FIELD_NEITHER = 0,

    /** Set the Differentiated Services Code Point. */
    FM_FFU_FIELD_DSCP,

    /** Set the VLAN ID. */
    FM_FFU_FIELD_VLAN,

    /** For internal use only. */
    FM_FFU_FIELD_MAX

} fm_ffuField;

/**************************************************/
/** \ingroup typeEnum
 * These enumerated values specify an operation to
 * be performed upon a flag when using
 * ''FM_FFU_ACTION_SET_FLAGS''.
 **************************************************/
typedef enum
{
    /** Leave this flag alone. */
    FM_FFU_FLAG_NOP = 0,

    /** Set this flag to FALSE. */
    FM_FFU_FLAG_CLEAR,

    /** Set this flag to TRUE. */
    FM_FFU_FLAG_SET,

    /** For internal use only. */
    FM_FFU_FLAG_MAX

} fm_ffuFlag;

/**************************************************/
/** \ingroup typeStruct
 * This structure specifies an ARP index and
 * optional ECMP information for use with the
 * ''FM_FFU_ACTION_ROUTE_ARP'' action.
 **************************************************/
typedef struct _fm_ffuRouteArp
{
    /** The base index within the ARP table. */
    fm_uint16 arpIndex;

    /** For equal cost multipath, the number of consecutive ARP
     *  table entries to choose among.  (For non-ECMP, set to 1.)
     *  The valid range for this field is 1-16. */
    fm_byte   count;

} fm_ffuRouteArp;

/**************************************************/
/** \ingroup typeStruct
 * This structure specifies operations to perform
 * on five independent flags for use with the
 * ''FM_FFU_ACTION_SET_FLAGS'' action.
 **************************************************/
typedef struct _fm_ffuActionFlags
{
    /** Do not send this frame to its normal destination. */
    fm_ffuFlag drop;

    /** Send this frame to the CPU instead of its normal destination. */
    fm_ffuFlag trap;

    /** Send this frame to the CPU in addition to its normal destination. */
    fm_ffuFlag log;

    /** Cancel any lower-precedence routing action. */
    fm_ffuFlag noRoute;

    /** Send a copy of this frame to the RX Mirror port. */
    fm_ffuFlag rxMirror;

} fm_ffuActionFlags;

/**************************************************/
/** \ingroup typeStruct
 * This structure specifies a value and mask for
 * use with the ''FM_FFU_ACTION_SET_TRIGGER'' or
 * ''FM_FFU_ACTION_SET_USER'' action.
 **************************************************/
typedef struct _fm_ffuMaskValue
{
    /** The value to set the field to. */
    fm_byte value;

    /** Mask indicating which bits of the field to set. */
    fm_byte mask;

} fm_ffuMaskValue;

/**************************************************/
/** \ingroup typeStruct
 * This structure specifies field values for use
 * with the ''FM_FFU_ACTION_SET_FIELDS'' action.
 **************************************************/
typedef struct _fm_ffuFields
{
    /** Set the VLAN, DSCP, or neither. */
    fm_ffuField fieldType;

    /** Value to set VLAN or DSCP to. */
    fm_uint16   fieldValue;

    /** Value to set switch priority and/or VLAN priority to.
     *  The VLAN priority is 4 bits and will be remapped by
     *  the ''FM_QOS_TX_PRIORITY_MAP'' port QoS attribute on egress.
     *  (Also see the ''FM_PORT_TXCFI'' port attribute.) */
    fm_byte     priority;

    /** Set switch priority? */
    fm_bool     setPri;

    /** Set VLAN priority? */
    fm_bool     setVpri;

} fm_ffuFields;

/**************************************************/
/** \ingroup typeStruct
 *  Referenced by ''fm_ffuAction'', this union 
 *  specifies the data for an FFU rule action,  
 *  specific to the action type (''fm_ffuActionType'').
 **************************************************/
typedef union _fm_ffuActionData
{
    /** Value for ''FM_FFU_ACTION_ROUTE_ARP''. */
    fm_ffuRouteArp    arp;

    /** Value for ''FM_FFU_ACTION_ROUTE_LOGICAL_PORT''. */
    fm_int            logicalPort;

    /** Value for ''FM_FFU_ACTION_SET_FLAGS''. */
    fm_ffuActionFlags flags;

    /** Value for ''FM_FFU_ACTION_SET_TRIGGER''. */
    fm_ffuMaskValue   trigger;

    /** Value for ''FM_FFU_ACTION_SET_USER''. */
    fm_ffuMaskValue   user;

    /** Value for ''FM_FFU_ACTION_SET_FIELDS''. */
    fm_ffuFields      fields;

} fm_ffuActionData;

/**************************************************/
/** \ingroup typeStruct
 *  Used by ''fm4000FFUSetRule'' and ''fm4000FFUSetRules'',
 *  this structure specifies a complete FFU action,
 *  both the action type, and the type-speicifc 
 *  data.
 **************************************************/
typedef struct _fm_ffuAction
{
    /** The Filtering and Forwarding Unit produces several different "action
     *  fields" which are subsequently used to modify the frame and/or
     *  determine its destination.  When a TCAM entry "hits" in the TCAM, its
     *  associated action entry (i. e. the rest of this struct) is
     *  evaluated to determine how the "action fields" should be modified.
     *  Since multiple TCAM entries may "hit" (up to one per slice), there may
     *  be multiple action entries which attempt to modify the "action fields"
     *  for a given frame.  As long as each action entry is attempting to
     *  modify different action fields, then there is no conflict.  However,
     *  if one action field is modified by more than one action entry that has
     *  hit, then the conflict must be resolved.  Each action entry contains a
     *  three-bit "precedence" field which is used to resolve such conflicts.
     *  If more than one action entry attempts to modify the same action field,
     *  then the action field is assigned the value from the action entry that
     *  has the highest precedence.  If more than one action entry for a given
     *  action field have equally high precedence, then the tie is resolved in
     *  favor of the action entry which resides in the higher-numbered slice. */
    fm_byte          precedence;

    /** Which action to perform. */
    fm_ffuActionType action;

    /** Data related to the action. */
    fm_ffuActionData data;

    /** The index of the counter within the bank.  0 means do not count. */
    fm_uint16        counter;

    /** The counter bank number. */
    fm_byte          bank;

} fm_ffuAction;

/**************************************************/
/** \ingroup typeEnum
 * This enumeration specifies the action a policer
 * should take when its committed or excess rate
 * is exceeded.
 **************************************************/
typedef enum
{
    /** Drop the frame. */
    FM_FFU_POLICER_ACTION_DROP = 0,

    /** Mark down the DSCP, switch priority, or both, as specified in
     *  the bank configuration. */
    FM_FFU_POLICER_ACTION_MARK_DOWN,

    /** For internal use only. */
    FM_FFU_POLICER_ACTION_MAX

} fm_ffuPolicerAction;

/**************************************************/
/** \ingroup typeStruct
 *  This structure specifies the state of a policer
 *  and is used as an argument for 
 *  ''fm4000FFUSetPolicer'', ''fm4000FFUSetPolicers'', 
 *  ''fm4000FFUGetPolicer'' and ''fm4000FFUGetPolicers''.
 *                                              \lb\lb
 * Each policer has two instances of this state,
 * one for the committed rate, and one for the
 * excess rate.
 **************************************************/
typedef struct _fm_policerState
{
    /** Number of 1/16th bytes that the committed or excess rate
     *  token bucket currently contains.  Signed quantity. */
    fm_int              current;

    /** Drop or mark down when rate exceeded. */
    fm_ffuPolicerAction action;

    /** Determines the refill rate of the committed or excess rate
     *  token bucket.  Amount of refill 1/16th bytes = (Current
     *  timestamp - Timestamp) * RateMantissa * 2 ^ -RateExponent * 2 ^ 4. */
    fm_uint             rateMantissa;

    /** Determines the refill rate of the committed or excess rate
     *  token bucket.  Amount of refill 1/16th bytes = (Current
     *  timestamp - Timestamp) * RateMantissa * 2 ^ -RateExponent * 2 ^ 4. */
    fm_uint             rateExponent;

    /** Capacity of committed or excess rate token bucket in
     *  1024 byte units. For the committed rate, the maximum value is
     *  8191 and for the excess rate, 4095. */
    fm_uint             limit;

} fm_ffuPolicerState;

/**************************************************/
/** \ingroup typeEnum
 * Determines what defines the color for a
 * policer bank.
 **************************************************/
typedef enum
{
    /** Differentiated Services Code Point defines the color. */
    FM_COLOR_SOURCE_DSCP = 0,

    /** Switch priority defines the color. */
    FM_COLOR_SOURCE_SWITCH_PRI,

    /** Assume the color is green. */
    FM_COLOR_SOURCE_ASSUME_GREEN,

    /** For internal use only. */
    FM_COLOR_SOURCE_MAX

} fm_ffuColorSource;

#ifdef FM_SUPPORT_FM4000

/* ownership functions */
/* FIXME: since these are not public API functions, they really
 * shouldn't be in a public header. */
fm_status fm4000FFUSetSliceOwnership(fm_int          physicalSw,
                                     fm_ffuOwnerType owner,
                                     fm_int          firstSlice,
                                 fm_int          lastSlice);
fm_status fm4000FFUGetSliceOwnership(fm_int          physicalSw,
                                     fm_ffuOwnerType owner,
                                     fm_int *        firstSlice,
                                     fm_int *        lastSlice);
fm_status fm4000FFUGetSliceOwner(fm_int           physicalSw,
                                 fm_int           slice,
                                 fm_ffuOwnerType *owner);
fm_status fm4000FFUGetMapperOwnership(fm_int                physicalSw,
                                      fm_ffuOwnerType *     owner,
                                      fm_ffuMappingResource mapResource);
fm_status fm4000FFUSetMapperOwnership(fm_int                physicalSw,
                                      fm_ffuOwnerType       owner,
                                      fm_ffuMappingResource mapResource);
fm_status fm4000FFUSetBankOwner(fm_int          physicalSw,
                                fm_int          bank,
                                fm_ffuOwnerType owner);
fm_status fm4000FFUGetBankOwner(fm_int           physicalSw,
                                fm_int           bank,
                                fm_ffuOwnerType *owner);
fm_status fm4000FFUGetBankOwnership(fm_int          physicalSw,
                                    fm_ffuOwnerType owner,
                                    fm_int *        firstBank,
                                    fm_int *        lastBank);

/* private initialization function called from fm4000PostBootSwitch */
fm_status fm4000FFUInit(fm_int physicalSw);


/* slice functions */
fm_status fm4000FFUSetMasterValid(fm_int    physicalSw,
                                  fm_uint32 validIngress,
                                  fm_uint32 validEgress,
                                  fm_bool   useCache);
fm_status fm4000FFUGetMasterValid(fm_int     physicalSw,
                                  fm_uint32 *validIngress,
                                  fm_uint32 *validEgress,
                                  fm_bool    useCache);
fm_status fm4000FFUConfigureSlice(fm_int                 physicalSw,
                                  const fm_ffuSliceInfo *slice,
                                  fm_bool                useCache);
fm_status fm4000FFUUnconfigureSlice(fm_int                 physicalSw,
                                    const fm_ffuSliceInfo *slice,
                                    fm_bool                useCache);
fm_status fm4000FFUSetRule(fm_int                 physicalSw,
                           const fm_ffuSliceInfo *slice,
                           fm_uint16              ruleIndex,
                           fm_bool                valid,
                           const fm_uint64 *      camValue,
                           const fm_uint64 *      camMask,
                           const fm_ffuAction *   actionList,
                           fm_bool                live,
                           fm_bool                useCache);
fm_status fm4000FFUSetRules(fm_int                 physicalSw,
                            const fm_ffuSliceInfo *slice,
                            fm_uint16              ruleIndex,
                            fm_uint16              nRules,
                            const fm_bool *        valid,
                            const fm_uint64 **     camValue,
                            const fm_uint64 **     camMask,
                            const fm_ffuAction **  actionList,
                            fm_bool                live,
                            fm_bool                useCache);
fm_status fm4000FFUSetRuleValid(fm_int                 physicalSw,
                                const fm_ffuSliceInfo *slice,
                                fm_uint16              ruleIndex,
                                fm_bool                valid,
                                fm_bool                useCache);
fm_status fm4000FFUGetRule(fm_int                 physicalSw,
                           const fm_ffuSliceInfo *slice,
                           fm_uint16              ruleIndex,
                           fm_bool *              valid,
                           fm_byte *              kase,
                           fm_uint64 *            camValue,
                           fm_uint64 *            camMask,
                           fm_ffuAction *         actionList,
                           fm_bool                useCache);
fm_status fm4000FFUGetRules(fm_int                 physicalSw,
                            const fm_ffuSliceInfo *slice,
                            fm_uint16              ruleIndex,
                            fm_uint16              nRules,
                            fm_bool *              valid,
                            fm_byte *              kase,
                            fm_uint64 **           camValue,
                            fm_uint64 **           camMask,
                            fm_ffuAction **        actionList,
                            fm_bool                useCache);
fm_status fm4000FFUCopyRules(fm_int                 physicalSw,
                             const fm_ffuSliceInfo *slice,
                             fm_uint16              fromIndex,
                             fm_uint16              nRules,
                             fm_uint16              toIndex,
                             fm_bool                live,
                             fm_bool                useCache);


/* mapper functions */
fm_status fm4000FFUSetSourceMapper(fm_int  physicalSw,
                                   fm_byte physicalPort,
                                   fm_byte mapSrc,
                                   fm_bool routable,
                                   fm_bool useCache);
fm_status fm4000FFUSetSourceMapperMapSrc(fm_int  physicalSw,
                                         fm_byte physicalPort,
                                         fm_byte mapSrc,
                                         fm_bool useCache);
fm_status fm4000FFUSetSourceMapperRoutable(fm_int  physicalSw,
                                           fm_byte physicalPort,
                                           fm_bool routable,
                                           fm_bool useCache);
fm_status fm4000FFUSetMACMapper(fm_int     physicalSw,
                                fm_byte    slot,
                                fm_macaddr mac,
                                fm_byte    ignoreLength,
                                fm_bool    validSMAC,
                                fm_bool    validDMAC,
                                fm_byte    mapMAC,
                                fm_bool    router,
                                fm_bool    useCache);
fm_status fm4000FFUSetVLANMapper(fm_int    physicalSw,
                                 fm_uint16 vlan,
                                 fm_uint16 mapVLAN,
                                 fm_bool   routable,
                                 fm_bool   useCache);
fm_status fm4000FFUSetVLANMapperMapVLAN(fm_int    physicalSw,
                                        fm_uint16 vlan,
                                        fm_uint16 mapVLAN,
                                        fm_bool   useCache);
fm_status fm4000FFUSetVLANMapperRoutable(fm_int    physicalSw,
                                         fm_uint16 vlan,
                                         fm_bool   routable,
                                         fm_bool   useCache);
fm_status fm4000FFUSetVLANMappers(fm_int           physicalSw,
                                  fm_uint16        firstVLAN,
                                  fm_uint16        nVLANs,
                                  const fm_uint16 *mapVLAN,
                                  const fm_bool *  routable,
                                  fm_bool          useCache);
fm_status fm4000FFUSetVLANMapperMapVLANs(fm_int           physicalSw,
                                         fm_uint16        firstVLAN,
                                         fm_uint16        nVLANs,
                                         const fm_uint16 *mapVLAN,
                                         fm_bool          useCache);
fm_status fm4000FFUSetVLANMapperRoutables(fm_int         physicalSw,
                                          fm_uint16      firstVLAN,
                                          fm_uint16      nVLANs,
                                          const fm_bool *routable,
                                          fm_bool        useCache);
fm_status fm4000FFUSetEthertypeMapper(fm_int    physicalSw,
                                      fm_byte   slot,
                                      fm_uint16 ethertype,
                                      fm_byte   mapType,
                                      fm_bool   useCache);
fm_status fm4000FFUSetLengthMapper(fm_int    physicalSw,
                                   fm_byte   slot,
                                   fm_uint16 length,
                                   fm_byte   mapLength,
                                   fm_bool   useCache);
fm_status fm4000FFUSetIPMapper(fm_int           physicalSw,
                               fm_byte          slot,
                               const fm_ipAddr *ipAddress,
                               fm_byte          ignoreLength,
                               fm_bool          validSIP,
                               fm_bool          validDIP,
                               fm_byte          mapIP,
                               fm_bool          live,
                               fm_bool          useCache);
fm_status fm4000FFUSetIPMapperValid(fm_int  physicalSw,
                                    fm_byte slot,
                                    fm_bool validSIP,
                                    fm_bool validDIP,
                                    fm_bool useCache);
fm_status fm4000FFUSetProtocolMapper(fm_int  physicalSw,
                                     fm_byte slot,
                                     fm_byte protocol,
                                     fm_byte mapProt,
                                     fm_bool useCache);
fm_status fm4000FFUSetL4Mapper(fm_int    physicalSw,
                               fm_bool   src,
                               fm_byte   slot,
                               fm_uint16 l4port,
                               fm_byte   mapProt,
                               fm_bool   valid,
                               fm_uint16 mapPort,
                               fm_bool   useCache);

fm_status fm4000FFUGetSourceMapper(fm_int   physicalSw,
                                   fm_byte  physicalPort,
                                   fm_byte *mapSrc,
                                   fm_bool *routable,
                                   fm_bool  useCache);
fm_status fm4000FFUGetMACMapper(fm_int      physicalSw,
                                fm_byte     slot,
                                fm_macaddr *mac,
                                fm_byte *   ignoreLength,
                                fm_bool *   validSMAC,
                                fm_bool *   validDMAC,
                                fm_byte *   mapMAC,
                                fm_bool *   router,
                                fm_bool     useCache);
fm_status fm4000FFUGetVLANMapper(fm_int     physicalSw,
                                 fm_uint16  vlan,
                                 fm_uint16 *mapVLAN,
                                 fm_bool *  routable,
                                 fm_bool    useCache);
fm_status fm4000FFUGetVLANMappers(fm_int     physicalSw,
                                  fm_uint16  firstVLAN,
                                  fm_uint16  nVLANs,
                                  fm_uint16 *mapVLAN,
                                  fm_bool *  routable,
                                  fm_bool    useCache);
fm_status fm4000FFUGetEthertypeMapper(fm_int     physicalSw,
                                      fm_byte    slot,
                                      fm_uint16 *ethertype,
                                      fm_byte *  mapType,
                                      fm_bool    useCache);
fm_status fm4000FFUGetLengthMapper(fm_int     physicalSw,
                                   fm_byte    slot,
                                   fm_uint16 *length,
                                   fm_byte *  mapLength,
                                   fm_bool    useCache);
fm_status fm4000FFUGetIPMapper(fm_int     physicalSw,
                               fm_byte    slot,
                               fm_ipAddr *ipAddress,
                               fm_byte *  ignoreLength,
                               fm_bool *  validSIP,
                               fm_bool *  validDIP,
                               fm_byte *  mapIP,
                               fm_bool    useCache);
fm_status fm4000FFUGetProtocolMapper(fm_int   physicalSw,
                                     fm_byte  slot,
                                     fm_byte *protocol,
                                     fm_byte *mapProt,
                                     fm_bool  useCache);
fm_status fm4000FFUGetL4Mapper(fm_int     physicalSw,
                               fm_bool    src,
                               fm_byte    slot,
                               fm_uint16 *l4port,
                               fm_byte *  mapProt,
                               fm_bool *  valid,
                               fm_uint16 *mapPort,
                               fm_bool    useCache);


/* counter/policer functions */
fm_status fm4000FFUSetCounter(fm_int    physicalSw,
                              fm_byte   bank,
                              fm_uint16 counterIndex,
                              fm_uint64 frameCount,
                              fm_uint64 byteCount);
fm_status fm4000FFUSetCounters(fm_int           physicalSw,
                               fm_byte          bank,
                               fm_uint16        firstIndex,
                               fm_uint16        nCounters,
                               const fm_uint64 *frameCount,
                               const fm_uint64 *byteCount);
fm_status fm4000FFUSetPolicer(fm_int                    physicalSw,
                              fm_byte                   bank,
                              fm_uint16                 policerIndex,
                              const fm_ffuPolicerState *committed,
                              const fm_ffuPolicerState *excess,
                              fm_uint32                 timestamp);
fm_status fm4000FFUSetPolicers(fm_int                    physicalSw,
                               fm_byte                   bank,
                               fm_uint16                 firstIndex,
                               fm_uint16                 nPolicers,
                               const fm_ffuPolicerState *committed,
                               const fm_ffuPolicerState *excess,
                               const fm_uint32 *         timestamp);
fm_status fm4000FFUSetPolicerConfig(fm_int            physicalSw,
                                    fm_byte           bank,
                                    fm_uint16         indexLastPolicer,
                                    fm_uint16         indexLastCountNoInt,
                                    fm_ffuColorSource ingressColorSource,
                                    fm_bool           markDSCP,
                                    fm_bool           markSwitchPri,
                                    fm_bool           timestampWriteEnable,
                                    fm_bool           useCache);

fm_status fm4000FFUGetCounter(fm_int     physicalSw,
                              fm_byte    bank,
                              fm_uint16  counterIndex,
                              fm_uint64 *frameCount,
                              fm_uint64 *byteCount);
fm_status fm4000FFUGetCounters(fm_int     physicalSw,
                               fm_byte    bank,
                               fm_uint16  firstIndex,
                               fm_uint16  nCounters,
                               fm_uint64 *frameCount,
                               fm_uint64 *byteCount);
fm_status fm4000FFUGetPolicer(fm_int              physicalSw,
                              fm_byte             bank,
                              fm_uint16           policerIndex,
                              fm_ffuPolicerState *committed,
                              fm_ffuPolicerState *excess,
                              fm_uint32 *         timestamp);
fm_status fm4000FFUGetPolicers(fm_int              physicalSw,
                               fm_byte             bank,
                               fm_uint16           firstIndex,
                               fm_uint16           nPolicers,
                               fm_ffuPolicerState *committed,
                               fm_ffuPolicerState *excess,
                               fm_uint32 *         timestamp);
fm_status fm4000FFUGetPolicerConfig(fm_int             physicalSw,
                                    fm_byte            bank,
                                    fm_uint16 *        indexLastPolicer,
                                    fm_uint16 *        indexLastCountNoInt,
                                    fm_ffuColorSource *ingressColorSource,
                                    fm_bool *          markDSCP,
                                    fm_bool *          markSwitchPri,
                                    fm_bool *          timestampWriteEnable,
                                    fm_bool            useCache);

fm_status fm4000FFUGetPolicerTimestamp(fm_int     physicalSw,
                                       fm_uint32 *timestamp);

fm_status fm4000FFUSetPolicerDSCPDownMap(fm_int         physicalSw,
                                         const fm_byte *table,
                                         fm_bool        useCache);
fm_status fm4000FFUSetPolicerSwPriDownMap(fm_int         physicalSw,
                                          const fm_byte *table,
                                          fm_bool        useCache);

fm_status fm4000FFUGetPolicerDSCPDownMap(fm_int   physicalSw,
                                         fm_byte *table,
                                         fm_bool  useCache);
fm_status fm4000FFUGetPolicerSwPriDownMap(fm_int   physicalSw,
                                          fm_byte *table,
                                          fm_bool  useCache);


/* egress ACL functions */
fm_status fm4000FFUSetEgressChunks(fm_int           physicalSw,
                                   fm_byte          firstChunk,
                                   fm_byte          nChunks,
                                   const fm_uint32 *validScenarios,
                                   const fm_uint32 *dstPhysicalPortMask,
                                   const fm_bool   *cascade,
                                   fm_bool          useCache);
fm_status fm4000FFUSetEgressChunkConfig(fm_int    physicalSw,
                                        fm_byte   chunk,
                                        fm_uint32 dstPhysicalPortMask,
                                        fm_bool   cascade,
                                        fm_bool   useCache);
fm_status fm4000FFUSetEgressAction(fm_int    physicalSw,
                                   fm_uint16 ruleIndex,
                                   fm_bool   drop,
                                   fm_bool   logToCpu,
                                   fm_bool   count,
                                   fm_bool   useCache);
fm_status fm4000FFUSetEgressActions(fm_int         physicalSw,
                                    fm_uint16      firstIndex,
                                    fm_uint16      nRules,
                                    const fm_bool *drop,
                                    const fm_bool *logToCpu,
                                    const fm_bool *count,
                                    fm_bool        useCache);

fm_status fm4000FFUGetEgressChunkConfig(fm_int     physicalSw,
                                        fm_byte    chunk,
                                        fm_uint32 *dstPhysicalPortMask,
                                        fm_bool *  cascade,
                                        fm_bool    useCache);
fm_status fm4000FFUGetEgressAction(fm_int    physicalSw,
                                   fm_uint16 ruleIndex,
                                   fm_bool * drop,
                                   fm_bool * logToCpu,
                                   fm_bool * count,
                                   fm_bool   useCache);
fm_status fm4000FFUGetEgressActions(fm_int    physicalSw,
                                    fm_uint16 firstIndex,
                                    fm_uint16 nRules,
                                    fm_bool * drop,
                                    fm_bool * logToCpu,
                                    fm_bool * count,
                                    fm_bool   useCache);

fm_status fm4000FFUSetEgressACLCounter(fm_int    physicalSw,
                                       fm_byte   physicalPort,
                                       fm_uint64 frameCount,
                                       fm_uint64 byteCount);

fm_status fm4000FFUGetEgressACLCounter(fm_int     physicalSw,
                                       fm_byte    physicalPort,
                                       fm_uint64 *frameCount,
                                       fm_uint64 *byteCount);

fm_status fm4000ScanFfuTcamForParityError(fm_int sw, 
                                          fm_bool *switchProtected, 
                                          fm_parityStatus *status,
                                          fm_eventParityError *event);

fm_status fm4000ScanFfuSramTableForParityError(fm_int   sw, 
                                               fm_bool *switchProtected, 
                                               fm_parityStatus *status,
                                               fm_eventParityError *event);

fm_status fm4000ScanFfuMapVlanForParityError(fm_int   sw, 
                                             fm_bool *switchProtected, 
                                             fm_parityStatus *status,
                                             fm_eventParityError *event);

void fm4000DbgDumpFfuTcam(fm_int sw);

#endif /* FM_SUPPORT_FM4000 */

#ifdef FM_SUPPORT_FM6000

#include <api/internal/fm6000/fm6000_api_ffu_int.h>
#include <api/internal/fm6000/fm6000_api_map_int.h>
#include <api/internal/fm6000/fm6000_api_policer_int.h>

#endif /* FM_SUPPORT_FM6000 */

#endif /* __FM_FM_API_FFU_H */
