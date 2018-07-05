/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_vn_int.h
 * Creation Date:   Oct 11, 2012
 * Description:     Virtual Network Internal Definitions
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

#ifndef __FM_FM_API_VN_INT_H
#define __FM_FM_API_VN_INT_H


/* Internal IDs are 12-bits in length. This allows for a maximum of 4096
 * virtual networks. */
#define FM_MAX_NUM_VNS                  (1 << 12)


/*****************************************************************************
 * Structure to describe a virtual network.
 *****************************************************************************/
typedef struct _fm_virtualNetwork
{
    /* Virtual Service ID */
    fm_uint32       vsId;

    /* Descriptor for the virtual network */
    fm_vnDescriptor descriptor;

} fm_virtualNetwork;


/*****************************************************************************
 * Structure to describe a tunnel.
 * Note that a tunnel is not subordinate to a virtual network. A single tunnel
 * may handle traffic for multiple virtual networks.
 *****************************************************************************/
typedef struct _fm_vnTunnel
{
    /* Tunnel ID */
    fm_int            tunnelId;

    /* Tunnel Type */
    fm_vnTunnelType   tunnelType;

    /* Unique Traffic Identifier */
    fm_int            trafficIdentifier;

    /* Local IP Address */
    fm_ipAddr         localIp;

    /* Remote IP Address */
    fm_ipAddr         remoteIp;

    /* Virtual Router ID */
    fm_int            vrid;

    /* Remote IP Virtual Router ID
     * This is the vrid used when the remote IP address was added to the
     * vnTunnelsByIp tree. Since the vrid may be changed before the
     * remote IP address is changed, we have to keep a backup copy of
     * the value. */
    fm_int            remoteIpVrid;

    /* Multicast Group Number */
    fm_int            mcastGroup;

    /* Multicast DMAC */
    fm_macaddr        mcastDmac;

    /* Pointer to the route that services this tunnel */
    fm_intRouteEntry *route;

    /* Pointer to a switch-specific extension structure */
    void *            extension;

} fm_vnTunnel;


fm_virtualNetwork *fmGetVN(fm_int sw, fm_uint32 vsId);
fm_vnTunnel *fmGetVNTunnel(fm_int sw, fm_int tunnelId);
fm_status fmVNAlloc(fm_int sw);
fm_status fmVNFree(fm_int sw);
fm_status fmVNInit(fm_int sw);
fm_status fmVNCleanup(fm_int sw);
fm_status fmNotifyVNTunnelAboutEcmpChange(fm_int sw, fm_intRouteEntry *route);
fm_status fmNotifyVNTunnelAboutRouteChange(fm_int sw);


#endif /* __FM_FM_API_VN_INT_H */

