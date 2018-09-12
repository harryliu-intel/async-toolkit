/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces) */
/*****************************************************************************
 * File:            fm_api_vn.h
 * Creation Date:   August 30, 2012
 * Description:     Header file for virtual networking services.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2012 Intel Corporation. All Rights Reserved.
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

#ifndef __FM_FM_API_VN_H
#define __FM_FM_API_VN_H


/** \ingroup constSystem
 *  The value used in virtual network tunneling to identify frames that need
 *  decapsulation. The application does this by using this value as the
 *  updated VLAN2 value in an ACL rule action. */
#define FM_VN_DECAPSULATION_VLAN2           4095


/**************************************************/
/** \ingroup typeEnum
 *  Referenced by ''fmCreateVNTunnel'', these
 *  enumerated values identify the protocol
 *  used by the tunnel.
 **************************************************/
typedef enum
{
    /** VxLAN Virtual Network */
    FM_VN_TUNNEL_TYPE_VXLAN,

    /** NVGRE Virtual Network */
    FM_VN_TUNNEL_TYPE_NVGRE,

    /** For Internal Use Only. */
    FM_VN_TUNNEL_TYPE_MAX       /* Must be last */

} fm_vnTunnelType;




/**************************************************/
/** \ingroup typeStruct
 *  Referenced by ''fmCreateVN'' and ''fmUpdateVN'',
 *  this structure contains information that
 *  describes a virtual network.
 **************************************************/
typedef struct _fm_vnDescriptor
{
    /** The Internal ID used for this virtual network.
     *  At the present time, this value must match a VLAN reserved
     *  for use for internal tunneling purposes, i.e., no ports
     *  should be members of this VLAN. In the future, it is
     *  expected that this VLAN-internal ID linkage will be
     *  removed. */
    fm_uint   internalId;

} fm_vnDescriptor;




/**************************************************/
/** \ingroup typeEnum
 *  Referenced by ''fmSetVNTunnelAttribute'' and
 *  ''fmGetVNTunnelAttribute'', these enumerated
 *  values identify an attribute of a virtual
 *  network tunnel.
 **************************************************/
typedef enum
{
    /** Type fm_ipAddr: The IP Address for the local end of this tunnel.
     *
     *  \chips FM6000 */
    FM_VNTUNNEL_ATTR_LOCAL_IP,

    /** Type fm_ipAddr: The IP Address for the remote end of this tunnel.
     *  Note: This attribute is considered as part of a tuple consisting
     *  of VRID and REMOTE_IP. REMOTE_IP may be changed without touching VRID,
     *  but if VRID is changed, REMOTE_IP must be updated after VRID is changed
     *  in order for the hardware to be updated.
     *
     *  \chips FM6000 */
    FM_VNTUNNEL_ATTR_REMOTE_IP,

    /** Type fm_int: The virtual router associated with this tunnel.
     *  Note: This attribute is considered as part of a tuple consisting
     *  of VRID and REMOTE_IP. REMOTE_IP may be changed without touching VRID,
     *  but if VRID is changed, REMOTE_IP must be updated after VRID is changed
     *  in order for the hardware to be updated.
     *
     *  \chips FM6000 */
    FM_VNTUNNEL_ATTR_VRID,

    /** Type fm_int: Tunnel Traffic Identifer. This identifier is used internally
     *  by the switch chip. It defaults to the tunnel ID + 1. Applications may
     *  change this if desired. Note that if an application specifies a traffic
     *  identifier for any tunnel, it must specify identifiers for all tunnels or
     *  conflicts may arise between automatically-selected identifiers for some
     *  tunnels and application-specified identifiers for other tunnels. The API
     *  does not attempt in any way to avoid or mitigate these conflicts. */
    FM_VNTUNNEL_ATTR_TRAFFIC_IDENTIFIER,

    /** Type fm_int: Multicast Group used with this tunnel.
     *
     *  \chips FM6000 */
    FM_VNTUNNEL_ATTR_MCAST_GROUP,

    /** Type fm_macaddr: Destination MAC Address used with this multicast
     *  tunnel.
     *
     *  \chips FM6000 */
    FM_VNTUNNEL_ATTR_MCAST_DMAC,

    /** For internal use only. */
    FM_VNTUNNEL_ATTR_MAX       /* Must be last */

} fm_vnTunnelAttrType;


fm_status fmCreateVN(fm_int           sw,
                     fm_uint32        vsId,
                     fm_vnDescriptor *descriptor);

fm_status fmDeleteVN(fm_int sw, fm_uint32 vsId);

fm_status fmUpdateVN(fm_int           sw,
                     fm_uint32        vsId,
                     fm_vnDescriptor *descriptor);

fm_status fmCreateVNTunnel(fm_int          sw,
                           fm_vnTunnelType tunnelType,
                           fm_int *        tunnelId);

fm_status fmDeleteVNTunnel(fm_int sw, fm_int tunnelId);

fm_status fmSetVNTunnelAttribute(fm_int              sw,
                                 fm_int              tunnelId,
                                 fm_vnTunnelAttrType attr,
                                 void *              value);

fm_status fmGetVNTunnelAttribute(fm_int              sw,
                                 fm_int              tunnelId,
                                 fm_vnTunnelAttrType attr,
                                 void *              value);

fm_status fmGetVNList(fm_int           sw,
                      fm_int           maxVNs,
                      fm_int *         numVNs,
                      fm_uint32 *      vsidList,
                      fm_vnDescriptor *descriptorList);

fm_status fmGetVNFirst(fm_int           sw,
                       fm_int *         searchToken,
                       fm_uint32 *      vsId,
                       fm_vnDescriptor *descriptor);

fm_status fmGetVNNext(fm_int           sw,
                      fm_int *         searchToken,
                      fm_uint32 *      vsId,
                      fm_vnDescriptor *descriptor);

fm_status fmGetVNTunnelList(fm_int  sw,
                            fm_int  maxTunnels,
                            fm_int *numTunnels,
                            fm_int *tunnelIds);

fm_status fmGetVNTunnelFirst(fm_int  sw,
                             fm_int *searchToken,
                             fm_int *tunnelId);

fm_status fmGetVNTunnelNext(fm_int  sw,
                            fm_int *searchToken,
                            fm_int *tunnelId);



#endif

