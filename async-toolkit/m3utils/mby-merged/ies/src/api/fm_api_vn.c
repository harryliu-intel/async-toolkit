/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces) */
/*****************************************************************************
 * File: fm_api_vn.c
 * Creation Date: Sep. 10, 2012
 * Description: Virtual Network Services
 *
 * INTEL CONFIDENTIAL
 * Copyright 2012 - 2013 Intel Corporation. All Rights Reserved.
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

#include <fm_sdk_int.h>


/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/


/*****************************************************************************
 * Global Variables
 *****************************************************************************/


/*****************************************************************************
 * Local Variables
 *****************************************************************************/


/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/
static fm_status ConfigureTunnelRoute(fm_int            sw,
                                      fm_vnTunnel *     tunnel,
                                      fm_intRouteEntry *route);
static fm_status UnconfigureTunnelRoute(fm_int sw, fm_vnTunnel *tunnel);


/*****************************************************************************
 * Local Functions
 *****************************************************************************/


/*****************************************************************************/
/** ConfigureTunnelRoute
 * \ingroup intVN
 *
 * \chips           FM6000
 *
 * \desc            Configures a route and it's related ECMP Group for use
 *                  as a virtual network tunnel.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       tunnel points to the virtual network tunnel record.
 *
 * \param[in]       route points to the internal route record.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status ConfigureTunnelRoute(fm_int            sw,
                                      fm_vnTunnel *     tunnel,
                                      fm_intRouteEntry *route)
{
    fm_status  status;
    fm_switch *switchPtr;

    FM_LOG_ENTRY( FM_LOG_CAT_VN,
                  "sw = %d, route = %p, tunnel = %p\n",
                  sw,
                  (void *) route,
                  (void *) tunnel );

    switchPtr = GET_SWITCH_PTR(sw);

    if (tunnel->route != NULL)
    {
        /* Clean up the previous route */
        status = UnconfigureTunnelRoute(sw, tunnel);
        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    tunnel->route = route;

    if (route == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_VN, FM_OK);
    }

    if ( fmTreeSize(&route->vnTunnelsTree) == 0 )
    {
        status = fmCustomTreeInsert( &switchPtr->vnTunnelRoutes, route, route );
        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    status = fmTreeInsert(&route->vnTunnelsTree, tunnel->tunnelId, tunnel);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_VN, status);

    FM_LOG_EXIT(FM_LOG_CAT_VN, status);

}   /* end ConfigureTunnelRoute */




/*****************************************************************************/
/** UnconfigureTunnelRoute
 * \ingroup intVN
 *
 * \chips           FM6000
 *
 * \desc            Unconfigures a route and it's related ECMP Group from use
 *                  as a virtual network tunnel.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       tunnel points to the virtual network tunnel record.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status UnconfigureTunnelRoute(fm_int sw, fm_vnTunnel *tunnel)
{
    fm_status         status;
    fm_switch *       switchPtr;
    fm_intRouteEntry *route;

    FM_LOG_ENTRY( FM_LOG_CAT_VN,
                  "sw = %d, tunnel = %p\n",
                  sw,
                  (void *) tunnel );

    switchPtr = GET_SWITCH_PTR(sw);
    route     = tunnel->route;
    status    = FM_OK;

    if (route != NULL)
    {
        /* Clean up the previous route */
        status = fmTreeRemove(&route->vnTunnelsTree, tunnel->tunnelId, NULL);
        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_VN, status);

        if ( fmTreeSize(&route->vnTunnelsTree) == 0 )
        {
            status = fmCustomTreeRemove(&switchPtr->vnTunnelRoutes, route, NULL);
            FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_VN, status);
        }

        tunnel->route = NULL;
    }

    FM_LOG_EXIT(FM_LOG_CAT_VN, status);

}   /* end UnconfigureTunnelRoute */




/*****************************************************************************/
/** CreateVNTunnel
 * \ingroup intVN
 *
 * \chips           FM6000
 *
 * \desc            Creates a Virtual Network Tunnel given the tunnel ID.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       tunnelType contains the Tunnel type.
 *
 * \param[in]       tunnelId it the tunnel ID.
 *
 * \param[out]      tunnelPtrPtr points to caller-provided storage into
 *                  which the pointer to the allocated tunnel structure
 *                  will be written.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_TABLE_FULL if the hardware resources used to support
 *                  the tunnel are full.
 *
 *****************************************************************************/
fm_status CreateVNTunnel(fm_int          sw,
                         fm_vnTunnelType tunnelType,
                         fm_int          tunnelId,
                         fm_vnTunnel **  tunnelPtrPtr)
{
    fm_switch *  switchPtr;
    fm_status    status;
    fm_vnTunnel *tunnel;
    fm_bool      addedToTree;

    FM_LOG_ENTRY( FM_LOG_CAT_VN,
                  "sw = %d, tunnelType = %d, tunnelId = %d, tunnelPtrPtr = %p\n",
                  sw,
                  tunnelType,
                  tunnelId,
                  (void *) tunnelPtrPtr );

    switchPtr   = GET_SWITCH_PTR(sw);
    addedToTree = FALSE;

    /* Create and initialize a new tunnel record */
    tunnel = fmAlloc( sizeof(fm_vnTunnel) );

    if (tunnel == NULL)
    {
        status = FM_ERR_NO_MEM;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    FM_CLEAR(*tunnel);

    tunnel->tunnelId          = tunnelId;
    tunnel->tunnelType        = tunnelType;
    tunnel->mcastGroup        = -1;
    tunnel->trafficIdentifier = tunnelId + 1;

    FM_API_CALL_FAMILY(status, switchPtr->CreateVNTunnel, sw, tunnel);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    if (tunnelId != FM_VN_DECAPSULATION_VLAN2)
    {
        status = fmTreeInsert(&switchPtr->vnTunnels, tunnelId, tunnel);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

        addedToTree = TRUE;
    }

    status = fmSetBitArrayBit(&switchPtr->vnTunnelsInUse, tunnelId, TRUE);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    if (tunnelPtrPtr != NULL)
    {
        *tunnelPtrPtr = tunnel;
    }


ABORT:

    if (status != FM_OK)
    {
        if (tunnel != NULL)
        {
            if (addedToTree)
            {
                fmTreeRemove(&switchPtr->vnTunnels, tunnelId, NULL);
            }

            fmFree(tunnel);
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_VN, status);

}   /* end CreateVNTunnel */




/*****************************************************************************/
/** DeleteVNTunnel
 * \ingroup intVN
 *
 * \chips           FM6000
 *
 * \desc            Deletes a Virtual Network tunnel.
 *
 * \note            All remote hosts using this tunnel will be deleted.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       tunnel points to the tunnel record
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_TUNNEL_IN_USE if the tunnel is in use.
 *
 *****************************************************************************/
static fm_status DeleteVNTunnel(fm_int sw, fm_vnTunnel *tunnel)
{
    fm_switch *switchPtr;
    fm_status  status;
    fm_bool    tunnelInUse;
    fm_int     tunnelId;

    FM_LOG_ENTRY( FM_LOG_CAT_VN,
                  "sw = %d, tunnel = %p, tunnelId = %u\n",
                  sw,
                  (void *) tunnel,
                  tunnel->tunnelId );

    switchPtr = GET_SWITCH_PTR(sw);
    tunnelId  = tunnel->tunnelId;

    status = fmIsVNTunnelInUseByACLs(sw, tunnelId, &tunnelInUse);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_VN, status);

    if (tunnelInUse)
    {
        status = FM_ERR_TUNNEL_IN_USE;
        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    FM_API_CALL_FAMILY(status, switchPtr->DeleteVNTunnel, sw, tunnel);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_VN, status);

    status = UnconfigureTunnelRoute(sw, tunnel);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_VN, status);

    if ( !fmIsIPAddressEmpty(&tunnel->remoteIp) )
    {
        /* Remove previous IP address from tree */
        status = fmCustomTreeRemove(&switchPtr->vnTunnelsByIp[tunnel->remoteIpVrid],
                                    &tunnel->remoteIp,
                                    NULL);
        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    if (tunnelId != FM_VN_DECAPSULATION_VLAN2)
    {
        status = fmTreeRemoveCertain(&switchPtr->vnTunnels, tunnelId, NULL);
        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    status = fmSetBitArrayBit(&switchPtr->vnTunnelsInUse, tunnelId, FALSE);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_VN, status);

    fmFree(tunnel);

    FM_LOG_EXIT(FM_LOG_CAT_VN, status);

}   /* end DeleteVNTunnel */




/*****************************************************************************/
/** FreeVNTunnel
 * \ingroup intVN
 *
 * \chips           FM6000
 *
 * \desc            Releases a tunnel record. Called during switch shutdown.
 *
 * \param[in]       tunPtr points to the tunnel record.
 *
 * \return          nothing.
 *
 *****************************************************************************/
void FreeVNTunnel(void *tunPtr)
{
    fm_vnTunnel *tunnel;

    tunnel = tunPtr;

    if (tunnel->extension != NULL)
    {
        fmFree(tunnel->extension);
    }

    fmFree(tunnel);

}   /* end FreeVNTunnel */




/*****************************************************************************
 * Public Functions
 *****************************************************************************/


/*****************************************************************************/
/** fmCreateVN
 * \ingroup virtualNetwork
 *
 * \chips           FM6000
 *
 * \desc            Creates a Virtual Network.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       vsId is the 24-bit virtual network ID
 *
 * \param[in]       descriptor points to a record that contains descriptive
 *                  information about the virtual network. To ensure
 *                  compatibility with future changes, this record
 *                  should be pre-initialized by calling ''FM_CLEAR''
 *                  before initialization of individual fields.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if virtual networks are not supported.
 * \return          FM_ERR_ALREADY_EXISTS if a virtual network already exists
 *                  using this vsId or if the internalID is already in use
 *                  for another virtual network.
 * \return          FM_ERR_TABLE_FULL if all available virtual networks are in use.
 * \return          FM_ERR_NO_MEM if memory cannot be allocated.
 * \return          FM_ERR_INVALID_ARGUMENT if any of the arguments are invalid.
 *
 *****************************************************************************/
fm_status fmCreateVN(fm_int           sw,
                     fm_uint32        vsId,
                     fm_vnDescriptor *descriptor)
{
    fm_switch *        switchPtr;
    fm_status          status;
    fm_virtualNetwork *vn;
    fm_bool            lockTaken;
    fm_bool            addedToTree;

    FM_LOG_ENTRY_API( FM_LOG_CAT_VN,
                      "sw = %d, vsId = %u, descriptor=%p\n",
                      sw,
                      vsId,
                      (void *) descriptor );

    VALIDATE_AND_PROTECT_SWITCH(sw);

    lockTaken   = FALSE;
    vn          = NULL;
    addedToTree = FALSE;
    switchPtr   = GET_SWITCH_PTR(sw);

    if (descriptor == NULL)
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    FM_LOG_DEBUG(FM_LOG_CAT_VN, "internalId = %u\n", descriptor->internalId);

    if (switchPtr->maxVNTunnels <= 0)
    {
        status = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    if ( (vsId & 0xff000000) != 0 )
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    status = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    lockTaken = TRUE;

    if (descriptor->internalId >= FM_MAX_NUM_VNS)
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    /* Determine if the vsId is already in use */
    vn = fmGetVN(sw, vsId);

    if (vn != NULL)
    {
        /* VN already exists */
        vn     = NULL;      /* Don't free the existing record */
        status = FM_ERR_ALREADY_EXISTS;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    /* Determine if the internal ID is already in use. */
    if (switchPtr->vnInternalIds[descriptor->internalId] != NULL)
    {
        vn     = NULL;      /* Don't free the existing record */
        status = FM_ERR_ALREADY_EXISTS;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    /* Create and initialize a new virtual network record */
    vn = fmAlloc( sizeof(fm_virtualNetwork) );

    if (vn == NULL)
    {
        status = FM_ERR_NO_MEM;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    FM_CLEAR(*vn);
    vn->vsId       = vsId;
    vn->descriptor = *descriptor;

    status = fmTreeInsert(&switchPtr->virtualNetworks, vsId, vn);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    addedToTree = TRUE;

    switchPtr->vnInternalIds[descriptor->internalId] = vn;

    FM_API_CALL_FAMILY(status, switchPtr->CreateVirtualNetwork, sw, vn);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);


ABORT:

    if (status != FM_OK)
    {
        if (vn != NULL)
        {
            if (addedToTree)
            {
                fmTreeRemoveCertain(&switchPtr->virtualNetworks, vsId, NULL);
            }

            fmFree(vn);
        }
    }

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_VN, status);

}   /* end fmCreateVN */




/*****************************************************************************/
/** fmDeleteVN
 * \ingroup virtualNetwork
 *
 * \chips           FM6000
 *
 * \desc            Deletes a Virtual Network, removing the association
 *                  between the vsId and the internalId.
 *
 * \note            All remote hosts using this virtual network will be deleted.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       vsId is the 24-bit NVGRE virtual subscriber ID or VxLAN
 *                  Network Identifier
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if vsId is not associated with a
 *                  virtual network.
 *
 *****************************************************************************/
fm_status fmDeleteVN(fm_int sw, fm_uint32 vsId)
{
    fm_switch *        switchPtr;
    fm_status          status;
    fm_virtualNetwork *vn;
    fm_bool            lockTaken;

    FM_LOG_ENTRY_API(FM_LOG_CAT_VN, "sw = %d, vsId = %u\n", sw, vsId);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    lockTaken = FALSE;
    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxVNTunnels <= 0)
    {
        status = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    status = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    lockTaken = TRUE;

    /* Find the VN record */
    vn = fmGetVN(sw, vsId);

    if (vn == NULL)
    {
        /* VN doesn't exist */
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    FM_API_CALL_FAMILY(status, switchPtr->DeleteVirtualNetwork, sw, vn);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    status = fmTreeRemoveCertain(&switchPtr->virtualNetworks, vsId, NULL);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    switchPtr->vnInternalIds[vn->descriptor.internalId] = NULL;

    fmFree(vn);


ABORT:

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_VN, status);

}   /* end fmDeleteVN */




/*****************************************************************************/
/** fmUpdateVN
 * \ingroup virtualNetwork
 *
 * \chips           FM6000
 *
 * \desc            Updates a Virtual Network in the virtual network table by
 *                  changing the internalId associated with a virtual network
 *                  subscriber ID.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       vsId is the 24-bit NVGRE virtual subscriber ID or VxLAN
 *                  Network Identifier
 *
 * \param[in]       descriptor points to a record that contains descriptive
 *                  information about the virtual network. To ensure
 *                  compatibility with future changes, this record
 *                  should be pre-initialized by calling ''FM_CLEAR''
 *                  before initialization of individual fields.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if virtual networks are not supported.
 * \return          FM_ERR_TABLE_FULL if all available virtual networks are in use.
 * \return          FM_ERR_NO_MEM if memory cannot be allocated.
 * \return          FM_ERR_INVALID_ARGUMENT if bits 24-31 of vsId are non-zero
 *                  or bits 12-31 of internalId are non-zero.
 * \return          FM_ERR_ALREADY_EXISTS if the specified internal-id is
 *                  already in use.
 *
 *****************************************************************************/
fm_status fmUpdateVN(fm_int           sw,
                     fm_uint32        vsId,
                     fm_vnDescriptor *descriptor)
{
    fm_switch *        switchPtr;
    fm_status          status;
    fm_virtualNetwork *vn;
    fm_bool            lockTaken;
    fm_vnDescriptor    oldDescriptor;

    FM_LOG_ENTRY_API( FM_LOG_CAT_VN,
                      "sw = %d, vsId = %u, descriptor = %p\n",
                      sw,
                      vsId,
                      (void *) descriptor );

    VALIDATE_AND_PROTECT_SWITCH(sw);

    lockTaken = FALSE;
    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxVNTunnels <= 0)
    {
        status = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    if (descriptor == NULL)
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    FM_LOG_DEBUG(FM_LOG_CAT_VN, "internalId = %u\n", descriptor->internalId);

    status = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    lockTaken = TRUE;

    /* Find the VN record */
    vn = fmGetVN(sw, vsId);

    if (vn == NULL)
    {
        /* VN doesn't exist */
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    /* Determine if the new descriptor is identical to the existing one */
    if (descriptor->internalId == vn->descriptor.internalId)
    {
        /* Nothing to do */
        status = FM_OK;
        goto ABORT;
    }

    /* Determine if the new internal ID is already in use. */
    if (switchPtr->vnInternalIds[descriptor->internalId] != NULL)
    {
        status = FM_ERR_ALREADY_EXISTS;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    oldDescriptor  = vn->descriptor;
    vn->descriptor = *descriptor;

    FM_API_CALL_FAMILY(status, switchPtr->UpdateVirtualNetwork, sw, vn, &oldDescriptor);

    if (status != FM_OK)
    {
        vn->descriptor = oldDescriptor;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    switchPtr->vnInternalIds[oldDescriptor.internalId]  = NULL;
    switchPtr->vnInternalIds[vn->descriptor.internalId] = vn;


ABORT:

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_VN, status);

}   /* end fmUpdateVN */




/*****************************************************************************/
/** fmCreateVNTunnel
 * \ingroup virtualNetwork
 *
 * \chips           FM6000
 *
 * \desc            Creates a Virtual Network Tunnel, which is a source and
 *                  destination of encapsulated packets for virtual networks.
 *                  Note that API attribute
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       tunnelType contains the Tunnel type.
 *
 * \param[out]      tunnelId points to caller-provided storage into which the
 *                  ID value used to identify this tunnel will be written.
 *                  This tunnel ID is used in the IP_TUNNEL ACL action to link
 *                  each tunneling ACL rule to the appropriate tunnel.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if virtual networks are not supported.
 * \return          FM_ERR_INVALID_ARGUMENT if tunnelType is invalid or if
 *                  tunnelId is NULL.
 * \return          FM_ERR_NO_FFU_RES_FOUND if the configured next-hop
 *                  resources cannot be reserved. This should only be
 *                  possible on the first call to this function after
 *                  booting the switch, or if, after all tunnels have been
 *                  deleted, the API attribute is changed before creating
 *                  any new tunnels.
 * \return          FM_ERR_TABLE_FULL if the hardware resources used to support
 *                  the tunnel are full.
 *
 *****************************************************************************/
fm_status fmCreateVNTunnel(fm_int          sw,
                           fm_vnTunnelType tunnelType,
                           fm_int *        tunnelId)
{
    fm_switch *  switchPtr;
    fm_status    status;
    fm_vnTunnel *tunnel;
    fm_bool      lockTaken;
    fm_int       index;

    FM_LOG_ENTRY_API( FM_LOG_CAT_VN,
                      "sw = %d, tunnelType = %d, tunnelId = %p\n",
                      sw,
                      tunnelType,
                      (void *) tunnelId );

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);
    lockTaken = FALSE;

    if (switchPtr->maxVNTunnels <= 0)
    {
        status = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    if (tunnelId == NULL)
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    status = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    lockTaken = TRUE;

    if (switchPtr->decapsulationTunnel == NULL)
    {
        /**************************************************
         * Create and configure pseudo-tunnel for
         * FM_VN_DECAPSULATION_VLAN2.
         **************************************************/
        status = CreateVNTunnel(sw,
                                FM_VN_TUNNEL_TYPE_MAX,
                                FM_VN_DECAPSULATION_VLAN2,
                                &switchPtr->decapsulationTunnel);

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    /* Find an available tunnel ID */
    status = fmFindBitInBitArray(&switchPtr->vnTunnelsInUse, 0, FALSE, &index);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    if (index < 0)
    {
        status = FM_ERR_TABLE_FULL;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    /* Create and initialize a new tunnel record */
    status = CreateVNTunnel(sw, tunnelType, index, &tunnel);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    *tunnelId = index;


ABORT:

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_VN, status);

}   /* end fmCreateVNTunnel */




/*****************************************************************************/
/** fmDeleteVNTunnel
 * \ingroup virtualNetwork
 *
 * \chips           FM6000
 *
 * \desc            Deletes a Virtual Network tunnel.
 *
 * \note            All remote hosts using this tunnel will be deleted.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       tunnelId is the ID number of the tunnel to be deleted.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if virtual networks are not supported.
 * \return          FM_ERR_INVALID_ARGUMENT if the specified tunnel ID is not
 *                  a valid tunnel.
 * \return          FM_ERR_TUNNEL_IN_USE if the tunnel is in use.
 *
 *****************************************************************************/
fm_status fmDeleteVNTunnel(fm_int sw, fm_int tunnelId)
{
    fm_switch *  switchPtr;
    fm_status    status;
    fm_vnTunnel *tunnel;
    fm_bool      lockTaken;

    FM_LOG_ENTRY_API(FM_LOG_CAT_VN, "sw = %d, tunnelId = %u\n", sw, tunnelId);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    lockTaken = FALSE;
    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxVNTunnels <= 0)
    {
        status = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    status = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    lockTaken = TRUE;
    tunnel    = fmGetVNTunnel(sw, tunnelId);

    if (tunnel == NULL)
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    status = DeleteVNTunnel(sw, tunnel);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    if ( fmTreeSize(&switchPtr->vnTunnels) == 0 )
    {
        if (switchPtr->decapsulationTunnel != NULL)
        {
            /**************************************************
             * Delete pseudo-tunnel for FM_VN_DECAPSULATION_VLAN2.
             **************************************************/
            status = DeleteVNTunnel(sw, switchPtr->decapsulationTunnel);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

            switchPtr->decapsulationTunnel = NULL;
        }
    }

ABORT:

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_VN, status);

}   /* end fmDeleteVNTunnel */




/*****************************************************************************/
/** fmSetVNTunnelAttribute
 * \ingroup virtualNetwork
 *
 * \chips           FM6000
 *
 * \desc            Modifies a virtual network tunnel attribute.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       tunnelId is the tunnel Id.
 *
 * \param[in]       attr is the tunnel attribute type. See ''fm_vnTunnelAttrType''.
 *
 * \param[in]       value points to the attribute's new value.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if virtual networks are not supported.
 * \return          FM_ERR_INVALID_ATTRIB if the attribute type is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if tunnelId is not a valid tunnel
 *                  ID for the virtual network or if value is NULL.
 * \return          FM_ERR_NO_ROUTE_TO_HOST if there is no route to the
 *                  destination IP address.
 *
 *****************************************************************************/
fm_status fmSetVNTunnelAttribute(fm_int              sw,
                                 fm_int              tunnelId,
                                 fm_vnTunnelAttrType attr,
                                 void *              value)
{
    fm_switch *       switchPtr;
    fm_status         status;
    fm_vnTunnel *     tunnel = NULL;
    fm_bool           lockTaken;
    fm_intRouteEntry *route;
    fm_int            vrid;
    fm_intRouteEntry *oldRoute;
    fm_ipAddr         oldRemoteIp;
    fm_int            oldVrid;
    fm_ipAddr         remoteIp;
    fm_bool           remoteIpChanged;

    FM_LOG_ENTRY_API(FM_LOG_CAT_VN,
                     "sw = %d, tunnelId = %d, attr = %d, value=%p\n",
                     sw,
                     tunnelId,
                     attr,
                     value);

    lockTaken       = FALSE;
    remoteIpChanged = FALSE;
    FM_CLEAR(oldRemoteIp);
    oldVrid         = 0;
    oldRoute        = NULL;

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxVNTunnels <= 0)
    {
        status = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    status = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    lockTaken   = TRUE;
    tunnel      = fmGetVNTunnel(sw, tunnelId);

    if (tunnel == NULL)
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    switch (attr)
    {
        case FM_VNTUNNEL_ATTR_LOCAL_IP:
            tunnel->localIp = *( (fm_ipAddr *) value );
            break;

        case FM_VNTUNNEL_ATTR_REMOTE_IP:
            remoteIp = *( (fm_ipAddr *) value );

            if ( !fmIsIPAddressEmpty(&remoteIp) )
            {
                if ( fmIsUnicastIPAddress(&remoteIp) )
                {
                    /* Try to find a route */
                    status = fmGetIntRouteForIP(sw,
                                                tunnel->vrid,
                                                &remoteIp,
                                                &route);
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
                }
                else
                {
                    route = NULL;
                }
            }
            else
            {
                route = NULL;
            }

            if ( !fmIsIPAddressEmpty(&tunnel->remoteIp) )
            {
                /* Remove previous IP address from tree */
                status = fmCustomTreeRemove(&switchPtr->vnTunnelsByIp[tunnel->remoteIpVrid],
                                            &tunnel->remoteIp,
                                            NULL);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
            }

            oldRoute             = tunnel->route;
            oldRemoteIp          = tunnel->remoteIp;
            oldVrid              = tunnel->remoteIpVrid;
            tunnel->remoteIp     = remoteIp;
            tunnel->remoteIpVrid = tunnel->vrid;
            remoteIpChanged      = TRUE;

            if ( !fmIsIPAddressEmpty(&remoteIp) )
            {
                /* Add new address to tree */
                status = fmCustomTreeInsert( &switchPtr->vnTunnelsByIp[tunnel->vrid],
                                             &tunnel->remoteIp,
                                             (void *) tunnel );
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
            }

            if (route != NULL)
            {
                status = ConfigureTunnelRoute(sw, tunnel, route);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
            }
            else
            {
                status = UnconfigureTunnelRoute(sw, tunnel);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
            }
            break;

        case FM_VNTUNNEL_ATTR_VRID:
            vrid = *( (fm_int *) value );
            status = fmValidateVirtualRouterId(sw, vrid, NULL);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

            tunnel->vrid = vrid;
            break;

        case FM_VNTUNNEL_ATTR_MCAST_GROUP:
            tunnel->mcastGroup = *( (fm_int *) value );
            break;

        case FM_VNTUNNEL_ATTR_MCAST_DMAC:
            tunnel->mcastDmac = *( (fm_macaddr *) value );
            break;

        case FM_VNTUNNEL_ATTR_TRAFFIC_IDENTIFIER:
            tunnel->trafficIdentifier = *( (fm_int *) value );
            break;

        default:
            status = FM_ERR_INVALID_ATTRIB;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    }

    FM_API_CALL_FAMILY(status,
                       switchPtr->SetVNTunnelAttribute,
                       sw,
                       tunnel,
                       attr,
                       value);


ABORT:

    if ( (status != FM_OK) && remoteIpChanged )
    {
        if ( !fmIsIPAddressEmpty(&tunnel->remoteIp) )
        {
            /* Remove previous IP address from tree */
            fmCustomTreeRemove(&switchPtr->vnTunnelsByIp[tunnel->remoteIpVrid],
                               &tunnel->remoteIp,
                               NULL);
        }

        tunnel->remoteIp     = oldRemoteIp;
        tunnel->remoteIpVrid = oldVrid;

        if (oldRoute != NULL)
        {
            fmCustomTreeInsert( &switchPtr->vnTunnelsByIp[tunnel->remoteIpVrid],
                                &tunnel->remoteIp,
                                (void *) tunnel );

            ConfigureTunnelRoute(sw, tunnel, oldRoute);
        }
        else
        {
            UnconfigureTunnelRoute(sw, tunnel);
        }
    }

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_VN, status);

}   /* end fmSetVNTunnelAttribute */




/*****************************************************************************/
/** fmGetVNTunnelAttribute
 * \ingroup virtualNetwork
 *
 * \chips           FM6000
 *
 * \desc            Retrieves a virtual network tunnel attribute.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       tunnelId is the tunnel Id.
 *
 * \param[in]       attr is the tunnel attribute type. See ''fm_vnTunnelAttrType''.
 *
 * \param[out]      value points to caller-provided storage into which
 *                  the attributes value will be written. See
 *                  ''fm_vnTunnelAttrType'' for the data types of each attribute.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if virtual networks are not supported.
 * \return          FM_ERR_INVALID_ARGUMENT if tunnelId is not a valid tunnel
 *                  ID for the virtual network, if the attribute type is
 *                  invalid, or if value is NULL.
 *
 *****************************************************************************/
fm_status fmGetVNTunnelAttribute(fm_int              sw,
                                 fm_int              tunnelId,
                                 fm_vnTunnelAttrType attr,
                                 void *              value)
{
    fm_switch *  switchPtr;
    fm_status    status;
    fm_vnTunnel *tunnel;
    fm_bool      lockTaken;

    FM_LOG_ENTRY_API(FM_LOG_CAT_VN,
                     "sw = %d, tunnelId = %d, attr = %d, value=%p\n",
                     sw,
                     tunnelId,
                     attr,
                     value);

    lockTaken = FALSE;

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxVNTunnels <= 0)
    {
        status = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    status = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    lockTaken = TRUE;
    tunnel    = fmGetVNTunnel(sw, tunnelId);

    if (tunnel == NULL)
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    status = FM_OK;

    switch (attr)
    {
        case FM_VNTUNNEL_ATTR_REMOTE_IP:
            *( (fm_ipAddr *) value ) = tunnel->remoteIp;
            break;

        case FM_VNTUNNEL_ATTR_LOCAL_IP:
            *( (fm_ipAddr *) value ) = tunnel->localIp;
            break;

        case FM_VNTUNNEL_ATTR_VRID:
            *( (fm_int *) value ) = tunnel->vrid;
            break;

        case FM_VNTUNNEL_ATTR_MCAST_GROUP:
            *( (fm_int *) value ) = tunnel->mcastGroup;
            break;

        case FM_VNTUNNEL_ATTR_MCAST_DMAC:
            *( (fm_macaddr *) value ) = tunnel->mcastDmac;
            break;

        case FM_VNTUNNEL_ATTR_TRAFFIC_IDENTIFIER:
            *( (fm_int *) value ) = tunnel->trafficIdentifier;
            break;

        default:
            status = FM_ERR_INVALID_ATTRIB;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    }


ABORT:

    if (lockTaken)
    {
        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);
    FM_LOG_EXIT_API(FM_LOG_CAT_VN, status);

}   /* end fmGetVNTunnelAttribute */




/*****************************************************************************/
/** fmGetVNList
 * \ingroup virtualNetwork
 *
 * \chips           FM6000
 *
 * \desc            Returns a list of Virtual Networks.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       maxVNs is the size of vsidList and internalIdList, being
 *                  the maximum number of Virtual Networks that can be
 *                  contained inside vsidList and internalIdList.
 *
 * \param[out]      numVNs points to caller-provided storage into which
 *                  will be stored the number of virtual networks stored in
 *                  vlanList and vsidList.
 *
 * \param[out]      vsidList is an array, maxVNs elements in length, that this
 *                  function will fill with the list of virtual subscriber IDs.
 *
 * \param[in]       descriptorList points to an array, maxVNs elements in length,
 *                  that this function will fill with the descriptor records
 *                  associated with each virtual network returned in vsidList.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if maxVNs is <= 0 or any of the
 *                  pointer arguments are NULL.
 * \return          FM_ERR_UNSUPPORTED if virtual networks are not supported.
 * \return          FM_ERR_NO_MORE if there are no virtual networks.
 * \return          FM_ERR_BUFFER_FULL if maxVNs was too small to accommodate
 *                  the entire list of virtual networks.
 *
 *****************************************************************************/
fm_status fmGetVNList(fm_int           sw,
                      fm_int           maxVNs,
                      fm_int *         numVNs,
                      fm_uint32 *      vsidList,
                      fm_vnDescriptor *descriptorList)
{
    fm_switch *        switchPtr;
    fm_status          status;
    fm_uint64          vsid;
    fm_virtualNetwork *vn;
    fm_int             curVN;
    fm_treeIterator    iter;
    fm_bool            lockTaken;

    FM_LOG_ENTRY_API( FM_LOG_CAT_VN,
                      "sw = %d, maxVNs = %d, numVNs = %p, vsidList = %p, "
                      "descriptorList = %p\n",
                      sw,
                      maxVNs,
                      (void *) numVNs,
                      (void *) vsidList,
                      (void *) descriptorList );

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);
    curVN     = 0;
    lockTaken = FALSE;

    if (switchPtr->maxVNTunnels <= 0)
    {
        status = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    if ( (numVNs == NULL) || (vsidList == NULL) || (descriptorList == NULL) )
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    status = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    lockTaken = TRUE;

    fmTreeIterInit(&iter, &switchPtr->vnTunnels);

    while (1)
    {
        status = fmTreeIterNext( &iter, &vsid, (void **) &vn );

        if (status == FM_ERR_NO_MORE)
        {
            status = FM_OK;
            break;
        }

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

        if (curVN >= maxVNs)
        {
            status = FM_ERR_BUFFER_FULL;
            break;
        }

        vsidList[curVN]       = (fm_uint32) vsid;
        descriptorList[curVN] = vn->descriptor;

        ++curVN;
    }


ABORT:

    if (lockTaken)
    {
        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    if (numVNs != NULL)
    {
        *numVNs = curVN;
    }

    FM_LOG_EXIT_API(FM_LOG_CAT_VN, status);

}   /* end fmGetVNList */




/*****************************************************************************/
/** fmGetVNFirst
 * \ingroup virtualNetwork
 *
 * \chips           FM6000
 *
 * \desc            Gets the first virtual network.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      searchToken points to caller-provided storage into which
 *                  the function will place a search token used for future
 *                  calls to fmGetVNNext.
 *
 * \param[out]      vsId points to caller-provided storage into which the
 *                  function will write the first virtual subscriber ID.
 *
 * \param[out]      descriptor points to caller-provided storage into which
 *                  the descriptive information for the virtual network will
 *                  be written.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if virtual networks are not supported.
 * \return          FM_ERR_NO_MORE if there are no virtual networks.
 *
 *****************************************************************************/
fm_status fmGetVNFirst(fm_int           sw,
                       fm_int *         searchToken,
                       fm_uint32 *      vsId,
                       fm_vnDescriptor *descriptor)
{
    fm_switch *        switchPtr;
    fm_status          status;
    fm_uint64          vsid64;
    fm_virtualNetwork *vn;
    fm_treeIterator    iter;
    fm_bool            lockTaken;

    FM_LOG_ENTRY_API( FM_LOG_CAT_VN,
                      "sw = %d, searchToken = %p, vsid = %p, descriptor = %p\n",
                      sw,
                      (void *) searchToken,
                      (void *) vsId,
                      (void *) descriptor );

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);
    lockTaken = FALSE;

    if (switchPtr->maxVNTunnels <= 0)
    {
        status = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    if ( (searchToken == NULL) || (vsId == NULL) || (descriptor == NULL) )
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    status = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    lockTaken = TRUE;

    fmTreeIterInit(&iter, &switchPtr->virtualNetworks);

    status = fmTreeIterNext( &iter, &vsid64, (void **) &vn );
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    *vsId       = (fm_uint32) vsid64;
    *descriptor = vn->descriptor;

    *searchToken = *vsId;


ABORT:

    if (lockTaken)
    {
        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_VN, status);

}   /* end fmGetVNFirst */




/*****************************************************************************/
/** fmGetVNNext
 * \ingroup virtualNetwork
 *
 * \chips           FM6000
 *
 * \desc            Gets the next virtual network.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in,out]   searchToken points to caller-provided storage containing
 *                  a search token provided by an earlier call to
 *                  ''fmGetVNFirst'' or  ''fmGetVNNext''.
 *
 * \param[out]      vsId points to caller-provided storage into which the
 *                  function will write the next virtual subscriber ID.
 *
 * \param[out]      descriptor points to caller-provided storage into which
 *                  the descriptive information for the virtual network will
 *                  be written.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if virtual networks are not supported.
 * \return          FM_ERR_NO_MORE if there are no more virtual networks.
 *
 *****************************************************************************/
fm_status fmGetVNNext(fm_int           sw,
                      fm_int *         searchToken,
                      fm_uint32 *      vsId,
                      fm_vnDescriptor *descriptor)
{
    fm_switch *        switchPtr;
    fm_status          status;
    fm_uint64          vsid64;
    fm_virtualNetwork *vn;
    fm_treeIterator    iter;
    fm_bool            lockTaken;

    FM_LOG_ENTRY_API( FM_LOG_CAT_VN,
                      "sw = %d, searchToken = %p, vsid = %p, descriptor = %p\n",
                      sw,
                      (void *) searchToken,
                      (void *) vsId,
                      (void *) descriptor );

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);
    lockTaken = FALSE;

    if (switchPtr->maxVNTunnels <= 0)
    {
        status = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    if ( (searchToken == NULL) || (vsId == NULL) || (descriptor == NULL) )
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    status = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    lockTaken = TRUE;

    vsid64 = *searchToken;

    status = fmTreeIterInitFromKey(&iter,
                                   &switchPtr->virtualNetworks,
                                   vsid64);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    /* get the previous virtual network */
    status = fmTreeIterNext( &iter, &vsid64, (void **) &vn );
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    /* get the next virtual network */
    status = fmTreeIterNext( &iter, &vsid64, (void **) &vn );
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    *vsId       = (fm_uint32) vsid64;
    *descriptor = vn->descriptor;

    *searchToken = *vsId;


ABORT:

    if (lockTaken)
    {
        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_VN, status);

}   /* end fmGetVNNext */




/*****************************************************************************/
/** fmGetVNTunnelList
 * \ingroup virtualNetwork
 *
 * \chips           FM6000
 *
 * \desc            Returns a list of virtual network tunnels.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       maxTunnels is the size of tunnelIds, being the maximum
 *                  number of tunnels that can be contained inside the array.
 *
 * \param[out]      numTunnels points to caller-provided storage into which
 *                  will be stored the number of tunnels returned.
 *
 * \param[out]      tunnelIds is an array, maxTunnels in length, that this
 *                  function will fill with the IDs for each tunnel.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if maxTunnels is <= 0,
 *                  or either of the pointer arguments is NULL.
 * \return          FM_ERR_UNSUPPORTED if virtual networks are not supported.
 * \return          FM_ERR_NO_MORE if there are no virtual network tunnels.
 * \return          FM_ERR_BUFFER_FULL if maxTunnels was too small to
 *                  accomodate the entire list of virtual network tunnels.
 *
 *****************************************************************************/
fm_status fmGetVNTunnelList(fm_int  sw,
                            fm_int  maxTunnels,
                            fm_int *numTunnels,
                            fm_int *tunnelIds)
{
    fm_switch *     switchPtr;
    fm_status       status;
    fm_uint64       tunId64;
    fm_vnTunnel *   tunnel;
    fm_int          index;
    fm_treeIterator iter;
    fm_bool         lockTaken;

    FM_LOG_ENTRY_API( FM_LOG_CAT_VN,
                      "sw = %d, maxTunnels = %d, numTunnels = %p, "
                      "tunnelIds = %p\n",
                      sw,
                      maxTunnels,
                      (void *) numTunnels,
                      (void *) tunnelIds );

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);
    index     = 0;
    lockTaken = FALSE;

    if (switchPtr->maxVNTunnels <= 0)
    {
        status = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    if ( (numTunnels == NULL) || (tunnelIds == NULL) )
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    status = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    lockTaken = TRUE;

    fmTreeIterInit(&iter, &switchPtr->vnTunnels);

    while (1)
    {
        status = fmTreeIterNext( &iter, &tunId64, (void **) &tunnel );

        if (status == FM_ERR_NO_MORE)
        {
            status = FM_OK;
            break;
        }

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

        if (index >= maxTunnels)
        {
            status = FM_ERR_BUFFER_FULL;
            break;
        }

        tunnelIds[index] = (fm_uint32) tunId64;

        ++index;
    }


ABORT:

    if (lockTaken)
    {
        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    if (numTunnels != NULL)
    {
        *numTunnels = index;
    }

    FM_LOG_EXIT_API(FM_LOG_CAT_VN, status);

}   /* end fmGetVNTunnelList */




/*****************************************************************************/
/** fmGetVNTunnelFirst
 * \ingroup virtualNetwork
 *
 * \chips           FM6000
 *
 * \desc            Gets the first virtual network tunnel.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      searchToken points to caller-provided storage into which
 *                  the function will place a search token used for future
 *                  calls to ''fmGetVNTunnelNext''.
 *
 * \param[out]      tunnelId points to caller-provided storage into which the
 *                  function will write the first virtual network tunnel ID.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if virtual networks are not supported.
 * \return          FM_ERR_NO_MORE if there are no virtual network tunnels.
 *
 *****************************************************************************/
fm_status fmGetVNTunnelFirst(fm_int  sw,
                             fm_int *searchToken,
                             fm_int *tunnelId)
{
    fm_switch *     switchPtr;
    fm_status       status;
    fm_uint64       tunId64;
    fm_vnTunnel *   tunnel;
    fm_treeIterator iter;
    fm_bool         lockTaken;

    FM_LOG_ENTRY_API( FM_LOG_CAT_VN,
                      "sw = %d, searchToken = %p, tunnelId = %p\n",
                      sw,
                      (void *) searchToken,
                      (void *) tunnelId );

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);
    lockTaken = FALSE;

    if (switchPtr->maxVNTunnels <= 0)
    {
        status = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    if ( (searchToken == NULL) || (tunnelId == NULL) )
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    status = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    lockTaken = TRUE;

    fmTreeIterInit(&iter, &switchPtr->vnTunnels);

    status = fmTreeIterNext( &iter, &tunId64, (void **) &tunnel );
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    *tunnelId    = (fm_uint32) tunId64;
    *searchToken = *tunnelId;


ABORT:

    if (lockTaken)
    {
        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_VN, status);

}   /* end fmGetVNTunnelFirst */




/*****************************************************************************/
/** fmGetVNTunnelNext
 * \ingroup virtualNetwork
 *
 * \chips           FM6000
 *
 * \desc            Gets the next virtual network tunnel.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in,out]   searchToken points to caller-provided storage containing
 *                  a search token provided by an earlier call to
 *                  ''fmGetVNTunnelFirst'' or  ''fmGetVNTunnelNext''.
 *
 * \param[out]      tunnelId points to caller-provided storage into which the
 *                  function will write the tunneld ID for the next virtual
 *                  network tunnel.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if virtual networks are not supported.
 * \return          FM_ERR_NO_MORE if there are no more virtual network tunnels.
 *
 *****************************************************************************/
fm_status fmGetVNTunnelNext(fm_int  sw,
                            fm_int *searchToken,
                            fm_int *tunnelId)
{
    fm_switch *     switchPtr;
    fm_status       status;
    fm_uint64       tunId64;
    fm_vnTunnel *   tunnel;
    fm_treeIterator iter;
    fm_bool         lockTaken;

    FM_LOG_ENTRY_API( FM_LOG_CAT_VN,
                      "sw = %d, searchToken = %p, tunnelId = %p\n",
                      sw,
                      (void *) searchToken,
                      (void *) tunnelId );

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);
    lockTaken = FALSE;

    if (switchPtr->maxVNTunnels <= 0)
    {
        status = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    if ( (searchToken == NULL) || (tunnelId == NULL) )
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    status = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    lockTaken = TRUE;

    tunId64 = *searchToken;

    status = fmTreeIterInitFromKey(&iter, &switchPtr->vnTunnels, tunId64);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    /* Get the previous record */
    status = fmTreeIterNext( &iter, &tunId64, (void **) &tunnel );
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    /* Get the next record */
    status = fmTreeIterNext( &iter, &tunId64, (void **) &tunnel );
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_VN, status);

    *tunnelId    = (fm_uint32) tunId64;
    *searchToken = *tunnelId;


ABORT:

    if (lockTaken)
    {
        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_VN, status);

}   /* end fmGetVNTunnelNext */




/*****************************************************************************/
/** fmVNAlloc
 * \ingroup intRouter
 *
 * \desc            Allocate resources needed by the virtual network
 *                  subsystem for a switch. Called during switch insertion.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmVNAlloc(fm_int sw)
{
    fm_switch *switchPtr;

    FM_LOG_ENTRY(FM_LOG_CAT_VN, "sw = %d\n", sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxVNTunnels <= 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_VN, FM_OK);
    }

    /* Nothing to do */

    FM_LOG_EXIT(FM_LOG_CAT_VN, FM_OK);

}   /* end fmVNAlloc */




/*****************************************************************************/
/** fmVNFree
 * \ingroup intRouter
 *
 * \desc            Release all virtual network resources held by a switch.
 *                  Called during switch removal.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmVNFree(fm_int sw)
{
    fm_switch *switchPtr;

    FM_LOG_ENTRY(FM_LOG_CAT_VN, "sw = %d\n", sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxVNTunnels <= 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_VN, FM_OK);
    }

    /* Nothing to do */

    FM_LOG_EXIT(FM_LOG_CAT_VN, FM_OK);

}   /* end fmVNFree */




/*****************************************************************************/
/** fmVNInit
 * \ingroup intRouter
 *
 * \desc            Perform initialization for virtual network subsystem.
 *                  Called at switch initialization time.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if insufficient next-hops have
 *                  been reserved for tunnel use.
 *
 *****************************************************************************/
fm_status fmVNInit(fm_int sw)
{
    fm_switch *  switchPtr;
    fm_status    status;
    fm_int       i;
    fm_bool      supportLookups;

    FM_LOG_ENTRY(FM_LOG_CAT_VN, "sw = %d\n", sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxVNTunnels <= 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_VN, FM_OK);
    }

    /**************************************************
     * Allocate memory for custom trees.
     **************************************************/
    i = sizeof(fm_customTree) * switchPtr->maxVirtualRouters;

    switchPtr->vnTunnelsByIp = (fm_customTree *) fmAlloc(i);

    if (switchPtr->vnTunnelsByIp == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_VN, FM_ERR_NO_MEM);
    }

    FM_MEMSET_S(switchPtr->vnTunnelsByIp, i, 0, i);

    i = sizeof(fm_virtualNetwork *) * FM_MAX_NUM_VNS;

    switchPtr->vnInternalIds = fmAlloc(i);

    if (switchPtr->vnInternalIds == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_VN, FM_ERR_NO_MEM);
    }

    FM_MEMSET_S(switchPtr->vnInternalIds, i, 0, i);

    /**************************************************
     * Virtual Networking requires that the routing
     * subsystem support the route lookup feature.
     **************************************************/
    supportLookups = TRUE;
    status = fmSetApiAttribute( FM_AAK_API_SUPPORT_ROUTE_LOOKUPS,
                                FM_AAT_API_SUPPORT_ROUTE_LOOKUPS,
                                (void *) &supportLookups );
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_VN, status);

    /**************************************************
     * Create trees
     **************************************************/
    fmTreeInit(&switchPtr->virtualNetworks);
    fmTreeInit(&switchPtr->vnTunnels);
    fmCustomTreeInit(&switchPtr->vnTunnelRoutes, fmCompareIntRoutes);

    for (i = 0 ; i < switchPtr->maxVirtualRouters ; i++)
    {
        fmCustomTreeInit(&switchPtr->vnTunnelsByIp[i], fmCompareIPAddrs);
    }

    /**************************************************
     * Create bit arrays
     **************************************************/
    status = fmCreateBitArray(&switchPtr->vnTunnelsInUse,
                              switchPtr->maxVNTunnels);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_VN, status);

    /**************************************************
     * VN Tunnel Id 0 should not be used as it is reserved.
     **************************************************/
    status = fmSetBitArrayBit(&switchPtr->vnTunnelsInUse,
                              0,
                              TRUE);

    FM_LOG_EXIT(FM_LOG_CAT_VN, status);

}   /* end fmVNInit */



/*****************************************************************************/
/** fmVNCleanup
 * \ingroup intRouter
 *
 * \desc            Releases memory used by the virtual networking subsystem
 *                  to support a specified switch.
 *                  Called when a switch is going down.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmVNCleanup(fm_int sw)
{
    fm_switch *switchPtr;
    fm_status  status;
    fm_int     i;

    FM_LOG_ENTRY(FM_LOG_CAT_VN, "sw = %d\n", sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxVNTunnels <= 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_VN, FM_OK);
    }

    /**************************************************
     * Destroy trees
     **************************************************/
    if ( fmCustomTreeIsInitialized(&switchPtr->vnTunnelRoutes) )
    {
        fmCustomTreeDestroy(&switchPtr->vnTunnelRoutes, NULL);
    }

    if (switchPtr->vnTunnelsByIp != NULL)
    {
        for (i = 0 ; i < switchPtr->maxVirtualRouters ; i++)
        {
            if (fmCustomTreeIsInitialized(&switchPtr->vnTunnelsByIp[i]) )
            {
                fmCustomTreeDestroy(&switchPtr->vnTunnelsByIp[i], NULL);
            }
        }
    }

    if ( fmTreeIsInitialized( &switchPtr->vnTunnels) )
    {
        fmTreeDestroy(&switchPtr->vnTunnels, FreeVNTunnel);
    }

    if ( fmTreeIsInitialized(&switchPtr->virtualNetworks) )
    {
        fmTreeDestroy(&switchPtr->virtualNetworks, fmFreeP);
    }

    /**************************************************
     * Destroy bit arrays
     **************************************************/
    status = fmDeleteBitArray(&switchPtr->vnTunnelsInUse);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_VN, status);

    /**************************************************
     * Free memory.
     **************************************************/
    if (switchPtr->vnTunnelsByIp != NULL)
    {
        fmFree(switchPtr->vnTunnelsByIp);
        switchPtr->vnTunnelsByIp = NULL;
    }

    if (switchPtr->vnInternalIds != NULL)
    {
        fmFree(switchPtr->vnInternalIds);
        switchPtr->vnInternalIds = NULL;
    }

    if (switchPtr->decapsulationTunnel != NULL)
    {
        FreeVNTunnel(switchPtr->decapsulationTunnel);
    }

    FM_LOG_EXIT(FM_LOG_CAT_VN, FM_OK);

}   /* end fmVNCleanup */




/*****************************************************************************/
/** fmGetVN
 * \ingroup intVN
 *
 * \chips           FM6000
 *
 * \desc            Given a vsId, returns the pointer to the virtual network
 *                  record.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       vsId is the virtual network identifier.
 *
 * \return          Pointer to the virtual network record if successful.
 * \return          NULL if the virtual network record was not found.
 *
 *****************************************************************************/
fm_virtualNetwork *fmGetVN(fm_int sw, fm_uint32 vsId)
{
    fm_switch *        switchPtr;
    fm_status          status;
    fm_virtualNetwork *vn;

    FM_LOG_ENTRY(FM_LOG_CAT_VN,"sw = %d, vsId = %d\n", sw, vsId);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxVNTunnels <= 0)
    {
        vn = NULL;
    }
    else
    {
        status = fmTreeFind( &switchPtr->virtualNetworks, vsId, (void **) &vn);

        if (status != FM_OK)
        {
            vn = NULL;
        }
    }

    FM_LOG_EXIT_CUSTOM( FM_LOG_CAT_VN, vn, "vn = %p\n", (void *) vn );

}   /* end fmGetVN */




/*****************************************************************************/
/** fmGetVNTunnel
 * \ingroup intVN
 *
 * \chips           FM6000
 *
 * \desc            Given a tunnel ID, returns the pointer to the tunnel record.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       tunnelId is the tunnel identifier.
 *
 * \return          Pointer to the tunnel record if successful.
 * \return          NULL if the tunnel record was not found.
 *
 *****************************************************************************/
fm_vnTunnel *fmGetVNTunnel(fm_int sw, fm_int tunnelId)
{
    fm_switch *  switchPtr;
    fm_status    status;
    fm_vnTunnel *tunnel;

    FM_LOG_ENTRY(FM_LOG_CAT_VN,"sw = %d, tunnelId = %d\n", sw, tunnelId);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxVNTunnels <= 0)
    {
        tunnel = NULL;
    }
    else
    {
        status = fmTreeFind( &switchPtr->vnTunnels, tunnelId, (void **) &tunnel);

        if (status != FM_OK)
        {
            tunnel = NULL;
        }
    }

    FM_LOG_EXIT_CUSTOM( FM_LOG_CAT_VN, tunnel, "tunnel = %p\n", (void *) tunnel );

}   /* end fmGetVNTunnel */




/*****************************************************************************/
/** fmNotifyVNTunnelAboutEcmpChange
 * \ingroup intVN
 *
 * \chips           FM6000
 *
 * \desc            Updates tunnel information for all tunnels that use
 *                  a specified route.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       route points to the route structure.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmNotifyVNTunnelAboutEcmpChange(fm_int sw, fm_intRouteEntry *route)
{
    fm_status       status;
    fm_switch *     switchPtr;
    fm_treeIterator iter;
    fm_uint64       tunnelId;
    fm_vnTunnel *   tunnel;

    FM_LOG_ENTRY( FM_LOG_CAT_VN,
                  "sw = %d, route = %p\n",
                  sw,
                  (void *) route );

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxVNTunnels <= 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_VN, FM_OK);
    }

    if ( fmTreeSize(&route->vnTunnelsTree) == 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_VN, FM_OK);
    }

    if (switchPtr->UpdateVNTunnelECMPGroup == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_VN, FM_OK);
    }

    fmTreeIterInit(&iter, &route->vnTunnelsTree);

    while (1)
    {
        status = fmTreeIterNext( &iter, &tunnelId, (void **) &tunnel );
        if (status == FM_ERR_NO_MORE)
        {
            status = FM_OK;
            break;
        }

        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_VN, status);

        status = switchPtr->UpdateVNTunnelECMPGroup(sw, tunnel);
        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_VN, status);
    }

    FM_LOG_EXIT(FM_LOG_CAT_VN, status);

}   /* end fmNotifyVNTunnelAboutEcmpChange */




/*****************************************************************************/
/** fmNotifyVNTunnelAboutRouteChange
 * \ingroup intVN
 *
 * \chips           FM6000
 *
 * \desc            Updates tunnel routes when some change has occurred in
 *                  the routing table.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmNotifyVNTunnelAboutRouteChange(fm_int sw)
{
    fm_status         status;
    fm_switch *       switchPtr;
    fm_treeIterator   iter;
    fm_uint64         key;
    fm_vnTunnel *     tunnel;
    fm_intRouteEntry *route;

    FM_LOG_ENTRY(FM_LOG_CAT_VN, "sw = %d\n", sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxVNTunnels <= 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_VN, FM_OK);
    }

    if ( fmTreeSize(&switchPtr->vnTunnels) == 0 )
    {
        FM_LOG_EXIT(FM_LOG_CAT_VN, FM_OK);
    }

    if (switchPtr->UpdateVNTunnelECMPGroup == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_VN, FM_OK);
    }

    fmTreeIterInit(&iter, &switchPtr->vnTunnels);

    while (1)
    {
        status = fmTreeIterNext( &iter,
                                 &key,
                                 (void **) &tunnel );
        if (status == FM_ERR_NO_MORE)
        {
            status = FM_OK;
            break;
        }

        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_VN, status);

        if ( !fmIsIPAddressEmpty(&tunnel->remoteIp)
             && fmIsUnicastIPAddress(&tunnel->remoteIp) )
        {
            status = fmGetIntRouteForIP(sw, tunnel->vrid, &tunnel->remoteIp, &route);

            if (status != FM_OK)
            {
                FM_LOG_ERROR( FM_LOG_CAT_VN,
                              "Error %d (%s) while finding route for tunnel %d\n",
                              status,
                              fmErrorMsg(status),
                              tunnel->tunnelId );
                continue;
            }
        }
        else
        {
            route = NULL;
        }

        if (route != tunnel->route)
        {
            if (route != NULL)
            {
                status = ConfigureTunnelRoute(sw, tunnel, route);
                FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_VN, status);

                if (switchPtr->UpdateVNTunnelECMPGroup != NULL)
                {
                    status = switchPtr->UpdateVNTunnelECMPGroup(sw, tunnel);
                    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_VN, status);
                }
            }
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_VN, status);

}   /* end fmNotifyVNTunnelAboutRouteChange */

