/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces) */
/*****************************************************************************
 * File: fm_api_routing.c
 * Creation Date: February 7, 2007
 * Description: Rouing services
 *
 * INTEL CONFIDENTIAL
 * Copyright 2007 - 2013 Intel Corporation. All Rights Reserved.
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
#if 0
#define DEBUG_TRACK_MEMORY_USE
#endif


/*****************************************************************************
 * Global Variables
 *****************************************************************************/


/*****************************************************************************
 * Local Variables
 *****************************************************************************/


/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/


/*****************************************************************************
 * Local Functions
 *****************************************************************************/


/*****************************************************************************/
/** GetRouteTree
 * \ingroup intRouter
 *
 * \desc            Returns a pointer to the appropriate route tree to use
 *                  for searching for/editing a route, based upon the route
 *                  type.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       route points to the route.
 *
 * \return          Pointer to the route tree, NULL if route type was invalid.
 *
 *****************************************************************************/
static fm_customTree *GetRouteTree(fm_int sw, fm_routeEntry *route)
{
    fm_switch *      switchPtr;
    fm_intRouteEntry key;
    fm_customTree *  routeTree;

    FM_LOG_ENTRY(FM_LOG_CAT_ROUTING,
                 "sw = %d, route = %p\n",
                 sw,
                 (void *) route);

    switchPtr = GET_SWITCH_PTR(sw);

    /* Try to find the route */
    key.route = *route;

    switch (route->routeType)
    {
        case FM_ROUTE_TYPE_UNICAST:
        case FM_ROUTE_TYPE_UNICAST_ECMP:
            routeTree = &switchPtr->ecmpRouteTree;
            break;

        case FM_ROUTE_TYPE_MULTICAST:
            routeTree = &switchPtr->routeTree;
            break;

        default:
            routeTree = NULL;
            break;
    }

    FM_LOG_EXIT_CUSTOM( FM_LOG_CAT_ROUTING,
                       routeTree,
                       "routeTree = %p\n",
                       (void *) routeTree );

}   /* end GetRouteTree */




/*****************************************************************************/
/** FreeRoute
 * \ingroup intRouter
 *
 * \desc            Releases a Route during switch initialization/shutdown.
 *
 * \param[in]       key points to the key.
 *
 * \param[in]       value points to the route.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static void FreeRoute(void *key, void *value)
{
    fm_intRouteEntry *routePtr = value;

    FM_NOT_USED(key);

    fmFree(routePtr);

}   /* end FreeRoute */




/*****************************************************************************/
/** FreeArp
 * \ingroup intRouter
 *
 * \desc            Releases ARP entry during switch initialization/shutdown.
 *
 * \param[in]       key points to the key.
 *
 * \param[in]       value points to the arp entry.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static void FreeArp(void *key, void *value)
{
    fm_intArpEntry *arpPtr = value;

    FM_NOT_USED(key);

    fmCustomTreeDestroy(&arpPtr->nextHopTree, NULL);
    fmRemoveArp(arpPtr->switchPtr, arpPtr);
    fmFree(arpPtr);

}   /* end FreeArp */




/*****************************************************************************/
/** CompareRoutes
 * \ingroup intRouter
 *
 * \desc            Compare Route entries, either full comparison or ECMP
 *                  check only.
 *
 * \param[in]       first points to the first route.
 *
 * \param[in]       second points to the second route.
 *
 * \param[in]       ecmpCheck is TRUE if the function should simply determine
 *                  if both routes are part of the same ECMP group, FALSE if
 *                  a full route comparison should be performed.
 *
 * \return          -1 if the first route sorts before the second.
 * \return           0 if the routes are identical.
 * \return           1 if the first route sorts after the second.
 *
 *****************************************************************************/
static fm_int CompareRoutes(fm_routeEntry *first,
                            fm_routeEntry *second,
                            fm_bool        ecmpCheck)
{
    fm_int                         i;
    fm_ipAddr *                    destAddr1;
    fm_ipAddr *                    destAddr2;
    fm_int                         destPrefix1;
    fm_int                         destPrefix2;
    fm_ipAddr                      zeroAddr;
    fm_ipAddr *                    nextHopAddr1;
    fm_ipAddr *                    nextHopAddr2;
    fm_int                         nextHopPrefix1;
    fm_int                         nextHopPrefix2;
    fm_uint16                      vlan1;
    fm_uint16                      vlan2;
    fm_uint16                      vlanPrefix1;
    fm_uint16                      vlanPrefix2;
    fm_ipAddr *                    ifAddr1;
    fm_ipAddr *                    ifAddr2;
    fm_int                         vrid1;
    fm_int                         vrid2;
    fm_multicastAddress *          multicast;
    fm_multicastDstIpRoute *       mcastDstIpRoute;
    fm_multicastDstIpVlanRoute *   mcastDstIpVlanRoute;
    fm_multicastDstSrcIpRoute *    mcastDstSrcRoute;
    fm_multicastDstSrcIpVlanRoute *mcastDstSrcVlanRoute;
    fm_int                         ecmpGroup1;
    fm_int                         ecmpGroup2;
    fm_routeType                   route1Type;
    fm_routeType                   route2Type;

    FM_CLEAR(zeroAddr);

    if (first->routeType == FM_ROUTE_TYPE_UNICAST)
    {
        destAddr1    = &first->data.unicast.dstAddr;
        destPrefix1  = first->data.unicast.prefixLength;
        nextHopAddr1 = &first->data.unicast.nextHop;
        ecmpGroup1   = 0;
        route1Type   = FM_ROUTE_TYPE_UNICAST;

        if (nextHopAddr1->isIPv6)
        {
            nextHopPrefix1 = FM_IPV6_MAX_PREFIX_LENGTH;
        }
        else
        {
            nextHopPrefix1 = FM_IPV4_MAX_PREFIX_LENGTH;
        }

        ifAddr1 = &first->data.unicast.interfaceAddr;

        if ( fmIsIPAddressEmpty(ifAddr1) )
        {
            vlan1 = first->data.unicast.vlan;
        }
        else
        {
            vlan1 = 0;
        }

        vlanPrefix1 = FM_VLAN_MAX_PREFIX_LENGTH;
        vrid1       = first->data.unicast.vrid;
    }
    else if (first->routeType == FM_ROUTE_TYPE_UNICAST_ECMP)
    {
        destAddr1      = &first->data.unicastECMP.dstAddr;
        destPrefix1    = first->data.unicastECMP.prefixLength;
        nextHopAddr1   = &zeroAddr;
        nextHopPrefix1 = 0;
        ecmpGroup1     = first->data.unicastECMP.ecmpGroup;
        ifAddr1        = &zeroAddr;
        vlan1          = 0;
        route1Type     = FM_ROUTE_TYPE_UNICAST;
        vlanPrefix1    = 0;
        vrid1          = first->data.unicastECMP.vrid;
    }
    else
    {
        multicast  = &first->data.multicast;
        ecmpGroup1 = 0;
        route1Type = FM_ROUTE_TYPE_MULTICAST;

        switch (multicast->addressType)
        {
            case FM_MCAST_ADDR_TYPE_DSTIP:
                mcastDstIpRoute = &multicast->info.dstIpRoute;
                destAddr1       = &mcastDstIpRoute->dstAddr;
                destPrefix1     = mcastDstIpRoute->dstPrefixLength;
                nextHopPrefix1  = 0;
                nextHopAddr1    = &zeroAddr;
                ifAddr1         = &zeroAddr;
                vlan1           = 0;
                vlanPrefix1     = 0;
                vrid1           = 0;
                break;

            case FM_MCAST_ADDR_TYPE_DSTIP_VLAN:
                mcastDstIpVlanRoute = &multicast->info.dstIpVlanRoute;
                destAddr1           = &mcastDstIpVlanRoute->dstAddr;
                destPrefix1         = mcastDstIpVlanRoute->dstPrefixLength;
                nextHopPrefix1      = 0;
                nextHopAddr1        = &zeroAddr;
                ifAddr1             = &zeroAddr;
                vlan1               = mcastDstIpVlanRoute->vlan;
                vlanPrefix1         = mcastDstIpVlanRoute->vlanPrefixLength;
                vrid1               = 0;
                break;

            case FM_MCAST_ADDR_TYPE_DSTIP_SRCIP:
                mcastDstSrcRoute = &multicast->info.dstSrcIpRoute;
                destAddr1        = &mcastDstSrcRoute->dstAddr;
                destPrefix1      = mcastDstSrcRoute->dstPrefixLength;
                nextHopPrefix1   = mcastDstSrcRoute->srcPrefixLength;
                nextHopAddr1     = &mcastDstSrcRoute->srcAddr;
                ifAddr1          = &zeroAddr;
                vlan1            = 0;
                vlanPrefix1      = 0;
                vrid1            = 0;
                break;

            case FM_MCAST_ADDR_TYPE_DSTIP_SRCIP_VLAN:
                mcastDstSrcVlanRoute = &multicast->info.dstSrcIpVlanRoute;
                destAddr1            = &mcastDstSrcVlanRoute->dstAddr;
                destPrefix1          = mcastDstSrcVlanRoute->dstPrefixLength;
                nextHopPrefix1       = mcastDstSrcVlanRoute->srcPrefixLength;
                nextHopAddr1         = &mcastDstSrcVlanRoute->srcAddr;
                ifAddr1              = &zeroAddr;
                vlan1                = mcastDstSrcVlanRoute->vlan;
                vlanPrefix1          = mcastDstSrcVlanRoute->vlanPrefixLength;
                vrid1                = 0;
                break;

            default:
                return 1;

        }   /* end switch (multicast->addressType) */

    }

    if (second->routeType == FM_ROUTE_TYPE_UNICAST)
    {
        destAddr2    = &second->data.unicast.dstAddr;
        destPrefix2  = second->data.unicast.prefixLength;
        nextHopAddr2 = &second->data.unicast.nextHop;
        ecmpGroup2   = 0;
        route2Type   = FM_ROUTE_TYPE_UNICAST;

        if (nextHopAddr2->isIPv6)
        {
            nextHopPrefix2 = FM_IPV6_MAX_PREFIX_LENGTH;
        }
        else
        {
            nextHopPrefix2 = FM_IPV4_MAX_PREFIX_LENGTH;
        }

        ifAddr2 = &second->data.unicast.interfaceAddr;

        if ( fmIsIPAddressEmpty(ifAddr2) )
        {
            vlan2 = second->data.unicast.vlan;
        }
        else
        {
            vlan2 = 0;
        }

        vlanPrefix2 = FM_VLAN_MAX_PREFIX_LENGTH;
        vrid2       = second->data.unicast.vrid;
    }
    else if (second->routeType == FM_ROUTE_TYPE_UNICAST_ECMP)
    {
        destAddr2      = &second->data.unicastECMP.dstAddr;
        destPrefix2    = second->data.unicastECMP.prefixLength;
        nextHopAddr2   = &zeroAddr;
        nextHopPrefix2 = 0;
        ecmpGroup2     = second->data.unicastECMP.ecmpGroup;
        ifAddr2        = &zeroAddr;
        vlan2          = 0;
        route2Type     = FM_ROUTE_TYPE_UNICAST;
        vlanPrefix2    = 0;
        vrid2          = second->data.unicastECMP.vrid;
    }
    else
    {
        multicast  = &second->data.multicast;
        route2Type = FM_ROUTE_TYPE_MULTICAST;
        ecmpGroup2 = 0;

        switch (multicast->addressType)
        {
            case FM_MCAST_ADDR_TYPE_DSTIP:
                mcastDstIpRoute = &second->data.multicast.info.dstIpRoute;
                destAddr2       = &mcastDstIpRoute->dstAddr;
                destPrefix2     = mcastDstIpRoute->dstPrefixLength;
                nextHopPrefix2  = 0;
                nextHopAddr2    = &zeroAddr;
                ifAddr2         = &zeroAddr;
                vlan2           = 0;
                vlanPrefix2     = 0;
                vrid2           = 0;
                break;

            case FM_MCAST_ADDR_TYPE_DSTIP_VLAN:
                mcastDstIpVlanRoute = &second->data.multicast.info.dstIpVlanRoute;
                destAddr2           = &mcastDstIpVlanRoute->dstAddr;
                destPrefix2         = mcastDstIpVlanRoute->dstPrefixLength;
                nextHopPrefix2      = 0;
                nextHopAddr2        = &zeroAddr;
                ifAddr2             = &zeroAddr;
                vlan2               = mcastDstIpVlanRoute->vlan;
                vlanPrefix2         = mcastDstIpVlanRoute->vlanPrefixLength;
                vrid2               = 0;
                break;

            case FM_MCAST_ADDR_TYPE_DSTIP_SRCIP:
                mcastDstSrcRoute = &second->data.multicast.info.dstSrcIpRoute;
                destAddr2        = &mcastDstSrcRoute->dstAddr;
                destPrefix2      = mcastDstSrcRoute->dstPrefixLength;
                nextHopPrefix2   = mcastDstSrcRoute->srcPrefixLength;
                nextHopAddr2     = &mcastDstSrcRoute->srcAddr;
                ifAddr2          = &zeroAddr;
                vlan2            = 0;
                vlanPrefix2      = 0;
                vrid2            = 0;
                break;

            case FM_MCAST_ADDR_TYPE_DSTIP_SRCIP_VLAN:
                mcastDstSrcVlanRoute =
                    &second->data.multicast.info.dstSrcIpVlanRoute;
                destAddr2      = &mcastDstSrcVlanRoute->dstAddr;
                destPrefix2    = mcastDstSrcVlanRoute->dstPrefixLength;
                nextHopPrefix2 = mcastDstSrcVlanRoute->srcPrefixLength;
                nextHopAddr2   = &mcastDstSrcVlanRoute->srcAddr;
                ifAddr2        = &zeroAddr;
                vlan2          = mcastDstSrcVlanRoute->vlan;
                vlanPrefix2    = mcastDstSrcVlanRoute->vlanPrefixLength;
                vrid2          = 0;
                break;

            default:
                return -1;

        }   /* end switch (multicast->addressType) */

    }

    /* Full Sort order is:
     *      VRID (low-to-high)
     *      dest address prefix length (high-to-low)
     *      next hop/source address prefix (high-to-low)
     *      vlan prefix length (high-to-low)
     *      route type (unicast and unicastECMP are identical)
     *      dest address (IP address order)
     *      ecmp Group id (low-to-high)
     *      next hop/source address (IP address order)
     *      vlan (low-to-high)
     *      interface IP Address (IP address order)
     * ECMP Sort order is:
     *      VRID (low-to-high)
     *      dest address prefix length (high-to-low)
     *      next hop/source address prefix (high-to-low)
     *      vlan prefix length (high-to-low)
     *      route type (unicast and unicastECMP are identical)
     *      dest address (IP address order)
     *      next hop/source address (IP address order) (multicast only)
     *      vlan (low-to-high) (multicast only)
     */

    if (vrid1 < vrid2)
    {
        return -1;
    }
    else if (vrid1 > vrid2)
    {
        return 1;
    }

    if (destPrefix1 > destPrefix2)
    {
        return -1;
    }
    else if (destPrefix1 < destPrefix2)
    {
        return 1;
    }

    if (nextHopPrefix1 > nextHopPrefix2)
    {
        return -1;
    }
    else if (nextHopPrefix1 < nextHopPrefix2)
    {
        return 1;
    }

    if (vlanPrefix1 > vlanPrefix2)
    {
        return -1;
    }
    else if (vlanPrefix1 < vlanPrefix2)
    {
        return 1;
    }

    if (route1Type < route2Type)
    {
        return -1;
    }
    else if (route1Type > route2Type)
    {
        return 1;
    }

    i = fmCompareIPAddresses(destAddr1, destAddr2);

    if (i < 0)
    {
        return -1;
    }
    else if (i > 0)
    {
        return 1;
    }

    if ( ecmpCheck && (route1Type == FM_ROUTE_TYPE_UNICAST) )
    {
        return 0;
    }

    if (!ecmpCheck)
    {
        if (ecmpGroup1 < ecmpGroup2)
        {
            return -1;
        }
        else if (ecmpGroup1 > ecmpGroup2)
        {
            return 1;
        }
    }

    i = fmCompareIPAddresses(nextHopAddr1, nextHopAddr2);

    if (i < 0)
    {
        return -1;
    }
    else if (i > 0)
    {
        return 1;
    }

    if (vlan1 < vlan2)
    {
        return -1;
    }
    else if (vlan1 > vlan2)
    {
        return 1;
    }

    if (ecmpCheck)
    {
        return 0;
    }

    i = fmCompareIPAddresses(ifAddr1, ifAddr2);

    if (i < 0)
    {
        return -1;
    }
    else if (i > 0)
    {
        return 1;
    }

    return 0;

}   /* end CompareRoutes */




/*****************************************************************************/
/** FindArpEntry
 * \ingroup intRouterArp
 *
 * \desc            Finds an entry in the ARP table.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       arp identifies the ARP entry to be found.
 *
 * \param[out]      foundArp receives the pointer to the ARP entry,
 *                  or NULL if the entry wasn't found.
 *
 * \param[in]       logWarning is TRUE if internal errors should be logged 
 *                  at WARNING level; FALSE if they should be logged as DEBUG.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_NOT_FOUND if the entry wasn't found.
 *
 *****************************************************************************/
static fm_status FindArpEntry(fm_int              sw,
                              fm_arpEntry*        arp,
                              fm_intArpEntry**    foundArp,
                              fm_bool             logWarning)
{
    fm_switch *                 switchPtr;
    fm_status                   err;
    fm_intArpEntry              searchArp;
    fm_intIpInterfaceEntry *    ifEntry;
    fm_uint64                   logLevel;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    switchPtr = GET_SWITCH_PTR(sw);

    logLevel = (logWarning) ? FM_LOG_LEVEL_WARNING : FM_LOG_LEVEL_DEBUG;

    searchArp.arp.ipAddr = arp->ipAddr;

    /* get the vlan */
    if (arp->interface >= 0)
    {
        err = fmGetInterface(sw, arp->interface, &ifEntry);

        if (err != FM_OK)
        {
            FM_LOG_PRINTF(FM_LOG_CAT_ROUTING, 
                          logLevel, 
                          "fmGetInterface returned error %d\n",
                          err);
            *foundArp = NULL;
            goto ABORT;
        }

        searchArp.arp.vlan = ifEntry->vlan;
    }
    else
    {
        searchArp.arp.vlan = arp->vlan;
    }

    /* try to find the entry in the table */
    err = fmCustomTreeFind(&switchPtr->arpTree,
                           (void *) &searchArp,
                           (void **) foundArp);

    if (err != FM_OK)
    {
        FM_LOG_PRINTF(FM_LOG_CAT_ROUTING, 
                      logLevel, 
                      "fmCustomTreeFind returned error %d\n",
                      err);
        *foundArp = NULL;
    }

ABORT:

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end FindArpEntry */




/*****************************************************************************/
/** DestroyRecord
 * \ingroup intRoute
 *
 * \desc            Destroys a record.  Typically called by fmCustomTreeDestroy.
 *
 * \param[in]       key points to the tree key record.
 *
 * \param[in]       data points to the tree data record.
 *
 * \return          Nothing.
 *
 *****************************************************************************/
static void DestroyRecord(void *key, void *data)
{
    if (key != data)
    {
        fmFree(key);
    }

    fmFree(data);

}   /* end DestroyRecord */




/*****************************************************************************/
/** ValidateNextHop
 * \ingroup intRouterArp
 *
 * \desc            Determines if a next-hop is usable (i.e., are there any
 *                  conditions that prevent it's use) and stores the result.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       nextHop points to the next-hop record to be tested.
 *
 * \param[out]      updated points to caller-provided storage into which TRUE
 *                  is written if the group's usable status was changed, or the
 *                  usable status of any next-hop in the group was changed.
 *                  Otherwise, FALSE is returned.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status ValidateNextHop(fm_int         sw,
                                 fm_intNextHop *nextHop,
                                 fm_bool *      updated)
{
    fm_status   status;
    fm_bool     isUsable;
    fm_nextHop *arpNextHop;
    fm_bool     wasUsable;
    fm_intIpInterfaceEntry *ifEntry;

    FM_LOG_ENTRY( FM_LOG_CAT_ROUTING,
                  "sw = %d, nextHop = %p, updated = %p\n",
                  sw,
                  (void *) nextHop,
                  (void *) updated );

    if (nextHop == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }
        
    isUsable  = TRUE;
    status    = FM_OK;
    wasUsable = nextHop->isUsable;

    if (updated != NULL)
    {
        *updated = FALSE;
    }

    /* Next-Hops in a fixed-Size ECMP Group are always usable */
    if (nextHop->ecmpGroup->fixedSize)
    {
        nextHop->isUsable = isUsable;
        if ( (isUsable != wasUsable) && (updated != NULL) )
        {
            *updated = TRUE;
        }
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_OK);
    }

    /* If the ECMP group and next-hop were created via fmAddRoute (i.e., the
     * original method of creating ECMP groups), the route state needs to
     * be honored.  For all new ECMP groups, the routeState element defaults
     * to FM_ROUTE_STATE_UP when the next-hop was added.
     */
    if (nextHop->routeState != FM_ROUTE_STATE_UP ||
        nextHop->state == FM_NEXT_HOP_STATE_DOWN)    
    {
        isUsable = FALSE;
        nextHop->isUsable = isUsable;
        if ( (isUsable != wasUsable) && (updated != NULL) )
        {
            *updated = TRUE;
        }
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_OK);
    }

    switch (nextHop->nextHop.type)
    {
        case FM_NEXTHOP_TYPE_ARP:
            arpNextHop = &nextHop->nextHop.data.arp;

            /* If the next-hop uses an interface IP address, has that
             * address been attached to an interface?  If not, the
             * next-hop is not usable.
             */
            if ( !fmIsIPAddressEmpty(&arpNextHop->interfaceAddr) )
            {
                if (nextHop->interfaceAddressEntry == NULL)
                {
                    isUsable = FALSE;
                    break;
                }
            }

            /* Check the interface state */
            if (nextHop->interfaceAddressEntry != NULL)
            {
                ifEntry = nextHop->interfaceAddressEntry->ifEntry;

                if (ifEntry->state != FM_INTERFACE_STATE_ADMIN_UP)
                {
                    isUsable = FALSE;
                    break;
                }

                nextHop->vlan = ifEntry->vlan;
            }

            /* Check the VLAN for validity */
            if (nextHop->vlan == FM_INVALID_VLAN)
            {
                isUsable = FALSE;
            }
            break;

        case FM_NEXTHOP_TYPE_DROP:
            /* DROP uses arp next-hop record for the next-hop IP address,
             * but doesn't use any of the other fields, so there is nothing
             * to test here. It is always usable. */
            isUsable = TRUE;
            break;

        default:
            isUsable = TRUE;
            break;
    }

    /* Store the computed state in the next-hop */
    nextHop->isUsable = isUsable;

    if ( (isUsable != wasUsable) && (updated != NULL) )
    {
        *updated = TRUE;
    }

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, status);

}   /* end ValidateNextHop */




/*****************************************************************************/
/** ValidateEcmpGroup
 * \ingroup intRouterArp
 *
 * \desc            Determines if an ECMP group is usable, i.e., does it
 *                  have any next-hops that are usable.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       ecmpGroup points to the ECMP group to be tested.
 *
 * \param[out]      updated points to caller-provided storage into which TRUE
 *                  is written if the group's usable status was changed, or the
 *                  usable status of any next-hop in the group was changed.
 *                  Otherwise, FALSE is returned.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status ValidateEcmpGroup(fm_int           sw,
                                   fm_intEcmpGroup *ecmpGroup,
                                   fm_bool *        updated)
{
    fm_status             status;
    fm_intNextHop *       nextHop;
    fm_int                index;
    fm_bool               isUsable;
    fm_bool               hopUpdated;
    fm_bool               wasUsable;
    fm_customTreeIterator iter;
    fm_intRouteEntry *    routeKey;
    fm_intRouteEntry *    route;
    fm_int                nbUpdatedHops;


    FM_LOG_ENTRY(FM_LOG_CAT_ROUTING,
                 "sw = %d, ecmpGroup = %p (%d)\n",
                 sw,
                 (void *) ecmpGroup,
                 ecmpGroup->groupId);

    isUsable  = FALSE;
    nbUpdatedHops = 0;
    wasUsable = ecmpGroup->isUsable;

    if (updated != NULL)
    {
        *updated = FALSE;
    }

    /* Multicast and Drop ECMP groups are always ready */
    if (ecmpGroup->mcastGroup != NULL)
    {
        ecmpGroup->isUsable = TRUE;

        if ( (ecmpGroup->isUsable != wasUsable) && (updated != NULL) )
        {
            *updated = TRUE;
        }

        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_OK);
    }

    /* Check each next-hop in the group.  If we find a single next-hop that
     * is usable, the group is usable, but we still have to check all of the
     * next-hops to insure that they are all initialized properly so that
     * other functions can rely upon each next-hop's isUsable flag.
     */
    for (index = 0 ; index < ecmpGroup->nextHopCount ; index++)
    {
        nextHop    = ecmpGroup->nextHops[index];
        hopUpdated = FALSE;

        if (nextHop != NULL) 
        {
            status = ValidateNextHop(sw, nextHop, &hopUpdated);

            if (status == FM_OK)
            {
                if (nextHop->isUsable)
                {
                    isUsable = TRUE;
                }
                if (hopUpdated)
                {
                    nbUpdatedHops++;
                }
            }
        }
    }

    /* If the group's usability is changing, update all routes dependent
     * on the group.
     */
    if (isUsable != wasUsable || nbUpdatedHops > 0)
    {
        /* Store the new usability state in the group */
        ecmpGroup->isUsable = isUsable;

        if (updated != NULL)
        {
            *updated = TRUE;
        }

        fmCustomTreeIterInit(&iter, &ecmpGroup->routeTree);

        while (1)
        {
            status = fmCustomTreeIterNext(&iter,
                                          (void **) &routeKey,
                                          (void **) &route);

            if (status == FM_ERR_NO_MORE)
            {
                break;
            }

            FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, status);

            status = fmSetRouteActiveFlag(sw, route, FALSE);
            FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, status);
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_OK);

}   /* end ValidateEcmpGroup */




/*****************************************************************************/
/** UpdateEcmpGroup
 * \ingroup intRouterIf
 *
 * \chips           FM4000, FM6000
 *
 * \desc            When an ECMP group is updated, every route that uses that
 *                  group must also be updated to point to the group's new
 *                  location and length in the next-hop table. This function
 *                  performs that update.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       ecmpGroup points to the ECMP group to be updated.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status UpdateEcmpGroup(fm_int           sw,
                                 fm_intEcmpGroup *ecmpGroup)
{
    fm_switch *                    switchPtr;
    fm_status                      err;
    fm_intRouteEntry *             route;
    fm_customTreeIterator          routeIter;
    fm_intRouteEntry *             routeKey;
    fm_bool                        updated;

    FM_LOG_ENTRY( FM_LOG_CAT_ROUTING,
                  "sw = %d, ecmpGroup=%p\n",
                  sw,
                  (void *) ecmpGroup );

    switchPtr = GET_SWITCH_PTR(sw);

    /* Validate the ECMP Group */
    ValidateEcmpGroup(sw, ecmpGroup, &updated);

    if (!updated)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_OK);
    }

    /* Update every route that refers to this ECMP group */
    fmCustomTreeIterInit(&routeIter, &ecmpGroup->routeTree);

    while (1)
    {
        err = fmCustomTreeIterNext(&routeIter,
                                   (void **) &routeKey,
                                   (void **) &route);

        if (err == FM_ERR_NO_MORE)
        {
            break;
        }

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        /* Update the route's status - don't update the hardware yet */
        err = fmSetRouteActiveFlag(sw, route, FALSE);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        err = fmNotifyVNTunnelAboutEcmpChange(sw, route);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    }   /* while (1) */

    /* Update the hardware ARP table and affected routes */
    FM_API_CALL_FAMILY(err,
                       switchPtr->UpdateEcmpGroup,
                       sw,
                       ecmpGroup);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);


ABORT:

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, err);

}   /* end UpdateEcmpGroup */




/*****************************************************************************/
/** UpdateEcmpGroupsForInterface
 * \ingroup intRouterArp
 *
 * \desc            Updates ECMP groups and routes affected by an interface
 *                  change.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       ifEntry points to the interface entry.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status UpdateEcmpGroupsForInterface(fm_int                  sw,
                                              fm_intIpInterfaceEntry *ifEntry)
{
    fm_switch *                    switchPtr;
    fm_status                      status;
    fm_customTreeIterator          hopIter;
    fm_intNextHop *                nextHopKey;
    fm_intNextHop *                nextHop;
    fm_bitArray                    ecmpGroupList;
    fm_int                         ecmpGroupId;
    fm_intIpInterfaceAddressEntry *addrEntry;
    fm_intEcmpGroup *              ecmpGroup;

    FM_LOG_ENTRY(FM_LOG_CAT_ROUTING,
                 "sw = %d, ifEntry = %p (%d)\n",
                 sw,
                 (void *) ifEntry,
                 ifEntry->interfaceNum);

    switchPtr = GET_SWITCH_PTR(sw);

    status = fmCreateBitArray(&ecmpGroupList, switchPtr->maxArpEntries);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, status);

    /* Look at all next-hops associated with each interface address entry.
     * Create a list of ECMP groups that need to be updated. */
    addrEntry = ifEntry->firstAddr;

    while (addrEntry != NULL)
    {
        fmCustomTreeIterInit(&hopIter, &addrEntry->nextHopTree);

        while ( ( status = fmCustomTreeIterNext( &hopIter,
                                                 (void **) &nextHopKey,
                                                 (void **) &nextHop ) ) == FM_OK )
        {
            status = fmSetBitArrayBit(&ecmpGroupList,
                                      nextHop->ecmpGroup->groupId,
                                      TRUE);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
        }

        addrEntry = addrEntry->nextAddr;
    }
    
    /* Update all ECMP Groups with next-hops using this interface */
    ecmpGroupId = -1;

    while (1)
    {
        status = fmFindBitInBitArray(&ecmpGroupList,
                                     ecmpGroupId + 1,
                                     TRUE,
                                     &ecmpGroupId);

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

        if (ecmpGroupId < 0)
        {
            break;
        }

        ecmpGroup = switchPtr->ecmpGroups[ecmpGroupId];

        status = UpdateEcmpGroup(sw, ecmpGroup);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }


ABORT:

    fmDeleteBitArray(&ecmpGroupList);

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, status);

}   /* end UpdateEcmpGroupsForInterface */




/*****************************************************************************/
/** FindNextHop
 * \ingroup intRouterArp
 *
 * \desc            Finds a next hop record in an ECMP group.
 *
 * \note            This function assumes that the routing lock has already
 *                  been taken.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       group points to the ECMP group.
 *
 * \param[in]       nextHop points to a user-interface next-hop record that
 *                  identifies the internal next-hop record to be found.
 *
 * \param[out]      hopIndexPtr points to caller-allocated storage into which
 *                  the index of the next hop in the group's next-hop list
 *                  will be placed, if hopIndexPtr is not NULL.
 *
 * \return          pointer to the internal next hop record or NULL if not found.
 *
 *****************************************************************************/
static fm_intNextHop *FindNextHop(fm_int           sw,
                                  fm_intEcmpGroup *group,
                                  fm_ecmpNextHop * nextHop,
                                  fm_int *         hopIndexPtr)
{
    fm_intNextHop  tempNextHop;
    fm_intNextHop *intNextHop;
    fm_int         hopIndex;

    FM_LOG_ENTRY( FM_LOG_CAT_ROUTING,
                  "sw = %d, group = %p(%d), nextHop = %p\n",
                  sw,
                  (void *) group,
                  group->groupId,
                  (void *) nextHop );

    FM_CLEAR( tempNextHop );
    tempNextHop.sw        = sw;
    tempNextHop.ecmpGroup = group;
    FM_MEMCPY_S( &tempNextHop.nextHop,
                 sizeof(tempNextHop.nextHop),
                 nextHop,
                 sizeof(*nextHop) );

    tempNextHop.hopIndex = FM_NEXTHOP_INDEX_UNSPECIFIED;

    for ( hopIndex = 0 ; hopIndex < group->nextHopCount ; hopIndex++)
    {
        intNextHop = group->nextHops[hopIndex];

        if (intNextHop == NULL)
        {
            continue;
        }

        if ( fmCompareInternalNextHops(&tempNextHop, intNextHop) == 0 )
        {
            if (hopIndexPtr != NULL)
            {
                *hopIndexPtr = hopIndex;
            }
            break;
        }
    }

    if (hopIndex >= group->nextHopCount)
    {
        intNextHop = NULL;
    }

    FM_LOG_EXIT_CUSTOM( FM_LOG_CAT_ROUTING,
                        intNextHop,
                        "intNextHop=%p\n",
                        (void *) intNextHop );

}   /* end FindNextHop */




/*****************************************************************************
 * Public Functions
 *****************************************************************************/


/*****************************************************************************/
/** fmCompareIntRoutes
 * \ingroup intRouter
 *
 * \desc            Compare internal Route entries.
 *
 * \param[in]       first points to the first route.
 *
 * \param[in]       second points to the second route.
 *
 * \return          -1 if the first route sorts before the second.
 * \return           0 if the routes are identical.
 * \return           1 if the first route sorts after the second.
 *
 *****************************************************************************/
fm_int fmCompareIntRoutes(const void *first, const void *second)
{
    fm_intRouteEntry *firstRoute  = (fm_intRouteEntry *) first;
    fm_intRouteEntry *secondRoute = (fm_intRouteEntry *) second;
    fm_int            i;

    i = CompareRoutes(&firstRoute->route, &secondRoute->route, FALSE);

    return i;

}   /* end fmCompareIntRoutes */




/*****************************************************************************/
/** fmCompareIPAddrs
 * \ingroup intRouter
 *
 * \desc            Compare IP Addresses.
 *
 * \param[in]       first points to the first IP Address.
 *
 * \param[in]       second points to the second IP Address.
 *
 * \return          -1 if the first address sorts before the second.
 * \return           0 if the addresses are identical.
 * \return           1 if the first address sorts after the second.
 *
 *****************************************************************************/
fm_int fmCompareIPAddrs(const void *first, const void *second)
{
    fm_ipAddr *firstAddr  = (fm_ipAddr *) first;
    fm_ipAddr *secondAddr = (fm_ipAddr *) second;
    fm_int     i;

    i = fmCompareIPAddresses(firstAddr, secondAddr);

    return i;

}   /* end fmCompareIPAddrs */




/*****************************************************************************/
/** fmCompareEcmpIntRoutes
 * \ingroup intRouter
 *
 * \desc            Compare internal Route entries.
 *
 * \param[in]       first points to the first route.
 *
 * \param[in]       second points to the second route.
 *
 * \return          -1 if the first route sorts before the second.
 * \return           0 if the routes are identical.
 * \return           1 if the first route sorts after the second.
 *
 *****************************************************************************/
fm_int fmCompareEcmpIntRoutes(const void *first, const void *second)
{
    fm_intRouteEntry *firstRoute  = (fm_intRouteEntry *) first;
    fm_intRouteEntry *secondRoute = (fm_intRouteEntry *) second;
    fm_int            i;

    i = CompareRoutes(&firstRoute->route, &secondRoute->route, TRUE);

    return i;

}   /* end fmCompareEcmpIntRoutes */




/*****************************************************************************/
/** fmCompareArps
 * \ingroup intRouter
 *
 * \desc            Compare ARP entries.
 *
 * \param[in]       first points to the first arp entry.
 *
 * \param[in]       second points to the second arp entry.
 *
 * \return          -1 if the first arp entry sorts before the second.
 * \return           0 if the arp entries are identical.
 * \return           1 if the first arp entry sorts after the second.
 *
 *****************************************************************************/
fm_int fmCompareArps(const void *first, const void *second)
{
    fm_arpEntry *firstArp  = (fm_arpEntry *) first;
    fm_arpEntry *secondArp = (fm_arpEntry *) second;
    fm_int          i;

    i = (fm_int) firstArp->vlan - (fm_int) secondArp->vlan;

    if (i < 0)
    {
        return -1;
    }
    else if (i > 0)
    {
        return 1;
    }

    i = fmCompareIPAddresses(&firstArp->ipAddr, &secondArp->ipAddr);

    return i;

}   /* end fmCompareArps */




/*****************************************************************************/
/** fmCompareInternalArps
 * \ingroup intRouter
 *
 * \desc            Compare internal ARP entries.
 *
 * \param[in]       first points to the first arp entry.
 *
 * \param[in]       second points to the second arp entry.
 *
 * \return          -1 if the first arp entry sorts before the second.
 * \return           0 if the arp entries are identical.
 * \return           1 if the first arp entry sorts after the second.
 *
 *****************************************************************************/
fm_int fmCompareInternalArps(const void *first, const void *second)
{
    fm_intArpEntry *firstArp  = (fm_intArpEntry *) first;
    fm_intArpEntry *secondArp = (fm_intArpEntry *) second;
    fm_int          i;

    i = (fm_int) firstArp->arp.vlan - (fm_int) secondArp->arp.vlan;

    if (i < 0)
    {
        return -1;
    }
    else if (i > 0)
    {
        return 1;
    }

    i = fmCompareIPAddresses(&firstArp->arp.ipAddr, &secondArp->arp.ipAddr);

    return i;

}   /* end fmCompareInternalArps */



/*****************************************************************************/
/** fmIsIPAddressEmpty
 * \ingroup intRouter
 *
 * \desc            See if an IP Address is composed entirely of zeros.
 *
 * \param[in]       addr points to the IP address to check.
 *
 * \return          TRUE if the address is empty.
 * \return          FALSE if the address is not empty.
 *
 *****************************************************************************/
fm_bool fmIsIPAddressEmpty(fm_ipAddr *addr)
{
    fm_bool isEmpty = TRUE;
    fm_int  i;

    if (addr->isIPv6)
    {
        isEmpty = FALSE;
    }
    else
    {
        for (i = 0 ; i < 4 ; i++)
        {
            if (addr->addr[i] != 0)
            {
                isEmpty = FALSE;
                break;
            }
        }
    }

    return isEmpty;

}   /* end fmIsIPAddressEmpty */




/*****************************************************************************/
/** fmGetInterfaceVlan
 * \ingroup intRouter
 *
 * \desc            Retrieves a vlan from an interface.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       ifAddr points to the interface address.
 *
 * \param[in]       alternateVlan is the vlan to be returned if ifAddr
 *                  contains an empty IP address.
 *
 * \return          interface vlan or alternateVlan.
 *
 *****************************************************************************/
fm_uint16 fmGetInterfaceVlan(fm_int     sw,
                             fm_ipAddr *ifAddr,
                             fm_uint16  alternateVlan)
{
    fm_status               status;
    fm_uint16               vlan;
    fm_intIpInterfaceEntry *ifEntry;

    status = fmFindInterface(sw, ifAddr, &ifEntry);

    if (status == FM_OK)
    {
        if (ifEntry != NULL)
        {
            vlan = ifEntry->vlan;
        }
        else
        {
            vlan = alternateVlan;
        }
    }
    else
    {
        vlan = FM_INVALID_VLAN;
    }

    return vlan;

}   /* end fmGetInterfaceVlan */




/*****************************************************************************/
/** fmCompareInternalNextHops
 * \ingroup intRouter
 *
 * \desc            Compare Next-Hop Addresses.
 *
 * \param[in]       first points to the first next-hop Address.
 *
 * \param[in]       second points to the second next-hop Address.
 *
 * \return          -1 if the first next-hop address sorts before the second.
 * \return           0 if the next-hop addresses are identical.
 * \return           1 if the first next-hop address sorts after the second.
 *
 *****************************************************************************/
fm_int fmCompareInternalNextHops(const void *first, const void *second)
{
    fm_int         retval = 0;
    fm_intNextHop *hop1;
    fm_intNextHop *hop2;
    fm_uint16      vlan1;
    fm_uint16      vlan2;
    fm_int         i;
    fm_nextHop *   arpHop1;
    fm_nextHop *   arpHop2;

    hop1 = (fm_intNextHop *) first;
    hop2 = (fm_intNextHop *) second;

    if (hop1->sw < hop2->sw)
    {
        return -1;
    }
    else if (hop1->sw > hop2->sw)
    {
        return 1;
    }

    if (hop1->ecmpGroup->groupId < hop2->ecmpGroup->groupId)
    {
        return -1;
    }
    else if (hop1->ecmpGroup->groupId > hop2->ecmpGroup->groupId)
    {
        return 1;
    }

    if (hop1->hopIndex < hop2->hopIndex)
    {
        return -1;
    }
    else if (hop1->hopIndex > hop2->hopIndex)
    {
        return 1;
    }


    if (hop1->nextHop.type < hop2->nextHop.type)
    {
        return -1;
    }
    else if (hop1->nextHop.type > hop2->nextHop.type)
    {
        return 1;
    }

    switch (hop1->nextHop.type)
    {
        case FM_NEXTHOP_TYPE_ARP:
            arpHop1 = &hop1->nextHop.data.arp;
            arpHop2 = &hop2->nextHop.data.arp;

            vlan1 = fmGetInterfaceVlan(hop1->sw,
                                       &arpHop1->interfaceAddr,
                                       arpHop1->vlan);

            vlan2 = fmGetInterfaceVlan(hop2->sw,
                                       &arpHop2->interfaceAddr,
                                       arpHop2->vlan);

            if (vlan1 < vlan2)
            {
                return -1;
            }
            else if (vlan1 > vlan2)
            {
                return 1;
            }

            retval = fmCompareIPAddresses(&arpHop1->addr, &arpHop2->addr);
            break;

        case FM_NEXTHOP_TYPE_DROP:
            arpHop1 = &hop1->nextHop.data.arp;
            arpHop2 = &hop2->nextHop.data.arp;

            retval = fmCompareIPAddresses(&arpHop1->addr, &arpHop2->addr);
            break;

        case FM_NEXTHOP_TYPE_RAW_NARROW:
            if (hop1->nextHop.data.rawNarrow.value < 
                hop2->nextHop.data.rawNarrow.value)
            {
                return -1;
            }
            else if (hop1->nextHop.data.rawNarrow.value > 
                     hop2->nextHop.data.rawNarrow.value)
            {
                return 1;
            }
            retval = 0;
            break;

        case FM_NEXTHOP_TYPE_RAW_WIDE:
            for (i = 0 ; i < FM_RAW_WIDE_NEXTHOP_SIZE ; i++)
            {
                if (hop1->nextHop.data.rawWide.values[i] < 
                    hop2->nextHop.data.rawWide.values[i])
                {
                    return -1;
                }
                else if (hop1->nextHop.data.rawWide.values[i] > 
                         hop2->nextHop.data.rawWide.values[i])
                {
                    return 1;
                }
            }
            retval = 0;
            break;

        default:
            retval = 0;
            break;
    }

    return retval;

}   /* end fmCompareInternalNextHops */




/*****************************************************************************/
/** fmGetRouteDestAddress
 * \ingroup intRouter
 *
 * \desc            Retrieves the destination address from a route entry.
 *
 * \param[in]       route points to the route entry.
 *
 * \param[in]       destAddr points to caller-allocated storage into which
 *                  the destination address is placed.
 *
 * \return          none.
 *
 *****************************************************************************/
void fmGetRouteDestAddress(fm_routeEntry *route, fm_ipAddr *destAddr)
{
    switch (route->routeType)
    {
        case FM_ROUTE_TYPE_UNICAST:
            FM_MEMCPY_S( destAddr,
                         sizeof(*destAddr),
                         &route->data.unicast.dstAddr,
                         sizeof(fm_ipAddr) );
            break;

        case FM_ROUTE_TYPE_UNICAST_ECMP:
            FM_MEMCPY_S( destAddr,
                         sizeof(*destAddr),
                         &route->data.unicastECMP.dstAddr,
                         sizeof(fm_ipAddr) );
            break;

        case FM_ROUTE_TYPE_MULTICAST:
            fmGetMcastDestAddress(&route->data.multicast, destAddr);
            break;

        default:
            FM_CLEAR(*destAddr);
            break;

    }   /* end switch (route->routeType) */

}   /* end fmGetRouteDestAddress */




/*****************************************************************************/
/** fmGetRouteMcastSourceAddress
 * \ingroup intRouter
 *
 * \desc            Retrieves the multicast source address from a route entry.
 *
 * \param[in]       route points to the route entry.
 *
 * \param[in]       srcAddr points to caller-allocated storage into which
 *                  the source address is placed.
 *
 * \return          none.
 *
 *****************************************************************************/
void fmGetRouteMcastSourceAddress(fm_routeEntry *route, fm_ipAddr *srcAddr)
{
    fm_multicastAddress *multicast;

    if (route->routeType != FM_ROUTE_TYPE_MULTICAST)
    {
        FM_CLEAR(*srcAddr);
        return;
    }

    multicast = &route->data.multicast;

    switch (multicast->addressType)
    {
        case FM_MCAST_ADDR_TYPE_DSTIP_SRCIP:
            FM_MEMCPY_S( srcAddr,
                         sizeof(*srcAddr),
                         &multicast->info.dstSrcIpRoute.srcAddr,
                         sizeof(fm_ipAddr) );
            break;

        case FM_MCAST_ADDR_TYPE_DSTIP_SRCIP_VLAN:
            FM_MEMCPY_S( srcAddr,
                         sizeof(*srcAddr),
                         &multicast->info.dstSrcIpVlanRoute.srcAddr,
                         sizeof(fm_ipAddr) );
            break;

        case FM_MCAST_ADDR_TYPE_DSTIP:
        case FM_MCAST_ADDR_TYPE_DSTIP_VLAN:
        default:
            FM_CLEAR(*srcAddr);
            break;

    }   /* end switch (multicast->addressType) */

}   /* end fmGetRouteMcastSourceAddress */




/*****************************************************************************/
/** fmGetRouteLookupTree
 * \ingroup intRouter
 *
 * \desc            Returns the route lookup tree associated with a vrid
 *                  and prefix.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       vrid is the virtual router ID number.
 *
 * \param[in]       prefix is the route mask prefix value.
 *
 * \param[out]      treePtrPtr points to caller-provided storage into which
 *                  the pointer to the custom tree will be written.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_UNSUPPORTED if route lookups are not supported.
 * \return          FM_ERR_INVALID_ARGUMENT if any of the arguments are invalid.
 *
 *****************************************************************************/
fm_status fmGetRouteLookupTree(fm_int          sw,
                               fm_int          vrid,
                               fm_int          prefix,
                               fm_customTree **treePtrPtr)
{
    fm_switch *switchPtr;
    fm_int     index;

    VALIDATE_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->routeLookupTrees == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_UNSUPPORTED);
    }

    if(treePtrPtr == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    index = (vrid * FM_MAX_NUM_IP_PREFIXES) + prefix;

    *treePtrPtr = &switchPtr->routeLookupTrees[index];

    return FM_OK;

}   /* end fmGetRouteLookupTree */




/*****************************************************************************/
/** fmGetIntRouteForIP
 * \ingroup intRouter
 *
 * \desc            Given an IP address, attempts to find the route in the
 *                  routing table that would be used to route frames to that
 *                  address.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       vrid is the virtual router ID number.
 *
 * \param[in]       ip points to the IP address.
 *
 * \param[out]      routePtrPtr points to caller-provided storage into which
 *                  the pointer to the destination route will be written.
 *                  NULL will be written if a route could not be found.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_UNSUPPORTED if route lookups are not supported.
 * \return          FM_ERR_INVALID_ARGUMENT if ip or routePtrPtr are NULL.
 * \return          FM_ERR_NO_ROUTE_TO_HOST if no route was found for this IP.
 *
 *****************************************************************************/
fm_status fmGetIntRouteForIP(fm_int             sw,
                             fm_int             vrid,
                             fm_ipAddr *        ip,
                             fm_intRouteEntry **routePtrPtr)
{
    fm_switch *       switchPtr;
    fm_int            prefixLength;
    fm_customTree *   routeLookupTree;
    fm_intRouteEntry *route;
    fm_status         status;
    fm_ipAddr         maskedIP;

    FM_LOG_ENTRY( FM_LOG_CAT_ROUTING,
                  "sw = %d, vrid = %d, ip = %p, routePtrPtr = %p\n",
                  sw,
                  vrid,
                  (void *) ip,
                  (void *) routePtrPtr );

    VALIDATE_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->routeLookupTrees == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_UNSUPPORTED);
    }

    if ( (ip == NULL) || (routePtrPtr == NULL) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    if (ip->isIPv6)
    {
        prefixLength = FM_IPV6_MAX_PREFIX_LENGTH;
    }
    else
    {
        prefixLength = FM_IPV4_MAX_PREFIX_LENGTH;
    }

    while (prefixLength >= 0)
    {
        maskedIP = *ip;
        fmMaskIPAddress(&maskedIP, prefixLength);

        status = fmGetRouteLookupTree(sw, vrid, prefixLength, &routeLookupTree);
        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, status);

        status = fmCustomTreeFind( routeLookupTree,
                                   &maskedIP,
                                   (void **) &route );
        if (status == FM_OK)
        {
            *routePtrPtr = route;
            FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_OK);
        }

        --prefixLength;
    }

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_NO_ROUTE_TO_HOST);

}   /* end fmGetIntRouteForIP */




/*****************************************************************************/
/** fmValidateVirtualRouterId
 * \ingroup intRouter
 *
 * \desc            Validates a virtual router id.  If the virtual router
 *                  id is not in the table and there is room in the
 *                  table for another virtual router, the vrid is stored
 *                  into the table and the needed hardware resources are
 *                  initialized.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       vrid is the virtual router id number.
 *
 * \param[out]      vroffPtr contains the offset of the virtual route in
 *                  the virtual router ID table.  If the virtual router ID
 *                  is not found, and a virtual router slot is available,
 *                  the offset of the available slot will be provided and
 *                  the function will return FM_ERR_NOT_FOUND.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmValidateVirtualRouterId(fm_int  sw,
                                    fm_int  vrid,
                                    fm_int *vroffPtr)
{
    fm_switch *switchPtr;
    fm_int     availvroff;
    fm_int     vroff;
    fm_status  err = FM_OK;

    switchPtr = GET_SWITCH_PTR(sw);

    vroff = -1;

    if ( ((switchPtr->virtualRouterIds == NULL)
        || (vrid < 0)
        || (vrid >= FM_MAX_VIRTUAL_ROUTERS))
        && (vrid != FM_ROUTER_ANY ) )
    {
        err = FM_ERR_INVALID_VRID;
    }

    else if ( (vrid == 0) || (vrid == FM_ROUTER_ANY) )
    {
        vroff = 0;
    }

    else
    {
        availvroff = -1;

        for (vroff = 1 ; vroff < switchPtr->maxVirtualRouters ; vroff++)
        {
            if (switchPtr->virtualRouterIds[vroff] == vrid)
            {
                break;
            }
            else if ( (availvroff < 0)
                     && (switchPtr->virtualRouterIds[vroff] < 0) )
            {
                availvroff = vroff;
            }
        }

        if (vroff >= switchPtr->maxVirtualRouters)
        {
            if (availvroff < 0)
            {
                err = FM_ERR_TOO_MANY_VIRTUAL_ROUTERS;
            }
            else
            {
                vroff = availvroff;
                err   = FM_ERR_NOT_FOUND;
            }
        }
    }

    if (vroffPtr != NULL)
    {
        *vroffPtr = vroff;
    }

    return err;

}   /* end fmValidateVirtualRouterId */




/*****************************************************************************/
/** fmRouterAlloc
 * \ingroup intRouter
 *
 * \desc            Allocate resources needed by the routing system for a
 *                  switch.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmRouterAlloc(fm_int sw)
{
    fm_switch *switchPtr;
    fm_int     i;
    fm_status  err;

    FM_LOG_ENTRY(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    switchPtr = GET_SWITCH_PTR(sw);

    /* Initialize all pointers, etc. */
    fmCustomTreeInit(&switchPtr->routeTree, fmCompareIntRoutes);
    fmCustomTreeInit(&switchPtr->ecmpRouteTree, fmCompareEcmpIntRoutes);
    fmCustomTreeInit(&switchPtr->arpTree, fmCompareInternalArps);
    FM_DLL_INIT_LIST(switchPtr, firstArp, lastArp);
    fmCustomTreeInit(&switchPtr->noArpNextHops, fmCompareInternalNextHops);
    switchPtr->ipInterfaceEntries  = NULL;
    switchPtr->virtualRouterStates = NULL;
    switchPtr->virtualRouterIds    = NULL;

    /* If routing is not supported, exit */
    if (switchPtr->RouterInit == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_OK);
    }

    /**************************************************
     * Allocate ECMP Groups
     **************************************************/
    if (switchPtr->maxArpEntries > 0)
    {
        i = sizeof(fm_intEcmpGroup *) * switchPtr->maxArpEntries;

        switchPtr->ecmpGroups = (fm_intEcmpGroup **) fmAlloc(i);

        if (switchPtr->ecmpGroups == NULL)
        {
            FM_LOG_EXIT(FM_LOG_CAT_SWITCH, FM_ERR_NO_MEM);
        }

#ifdef DEBUG_TRACK_MEMORY_USE
        FM_LOG_PRINT("Routing MALLOC: ecmp groups %p/%d\n",
                     (void *) switchPtr->ecmpGroups,
                     i);
#endif

        FM_MEMSET_S(switchPtr->ecmpGroups, i, 0, i);

        err = fmCreateBitArray(&switchPtr->ecmpGroupsInUse,
                               switchPtr->maxArpEntries);

        if (err != FM_OK)
        {
            FM_LOG_EXIT(FM_LOG_CAT_SWITCH, err);
        }

#ifdef DEBUG_TRACK_MEMORY_USE
        FM_LOG_PRINT("Routing CREATE BIT ARRAY: ecmpGroupsInUse\n");
#endif

    }

    /**************************************************
     * Allocate Interface Table
     **************************************************/
    if (switchPtr->maxIpInterfaces > 0)
    {
        i = sizeof(fm_intIpInterfaceEntry) * switchPtr->maxIpInterfaces;

        switchPtr->ipInterfaceEntries = (fm_intIpInterfaceEntry *) fmAlloc(i);

        if (switchPtr->ipInterfaceEntries == NULL)
        {
            FM_LOG_EXIT(FM_LOG_CAT_SWITCH, FM_ERR_NO_MEM);
        }

#ifdef DEBUG_TRACK_MEMORY_USE
        FM_LOG_PRINT("Routing MALLOC: ipInterface entries %p/%d\n",
                     (void *) switchPtr->ipInterfaceEntries,
                     i);
#endif

        FM_MEMSET_S(switchPtr->ipInterfaceEntries, i, 0, i);

        err = fmCreateBitArray(&switchPtr->ipInterfaceEntriesInUse,
                               switchPtr->maxIpInterfaces);

        if (err != FM_OK)
        {
            FM_LOG_EXIT(FM_LOG_CAT_SWITCH, err);
        }

#ifdef DEBUG_TRACK_MEMORY_USE
        FM_LOG_PRINT("Routing CREATE BIT ARRAY: ipInterfaceEntriesInUse\n");
#endif

        for (i = 0 ; i < switchPtr->maxIpInterfaces ; i++)
        {
            fm_intIpInterfaceEntry *entry;

            entry               = &switchPtr->ipInterfaceEntries[i];
            entry->interfaceNum = -1;
            entry->vlan         = FM_INVALID_VLAN;
            entry->state        = FM_INTERFACE_STATE_ADMIN_DOWN;
            fmInitInterfaceEntryLinkedLists(entry);
        }
    }

    /**************************************************
     * Allocate Virtual Router Tables
     **************************************************/
    if (switchPtr->maxVirtualRouters > 0)
    {
        i = sizeof(fm_routerState) * switchPtr->maxVirtualRouters;

        switchPtr->virtualRouterStates = (fm_routerState *) fmAlloc(i);

        if (switchPtr->virtualRouterStates == NULL)
        {
            FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_NO_MEM);
        }

#ifdef DEBUG_TRACK_MEMORY_USE
        FM_LOG_PRINT("Routing MALLOC: virtual Router states %p/%d\n",
                     (void *) switchPtr->virtualRouterStates,
                     i);
#endif

        FM_MEMSET_S(switchPtr->virtualRouterStates, i, 0, i);

        i = sizeof(fm_int) * switchPtr->maxVirtualRouters;

        switchPtr->virtualRouterIds = (fm_int *) fmAlloc(i);

        if (switchPtr->virtualRouterIds == NULL)
        {
            FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_NO_MEM);
        }

#ifdef DEBUG_TRACK_MEMORY_USE
        FM_LOG_PRINT("Routing MALLOC: virtual router ids %p/%d\n",
                     (void *) switchPtr->virtualRouterIds,
                     i);
#endif

        switchPtr->virtualRouterStates[0] = FM_ROUTER_STATE_ADMIN_UP;
        switchPtr->virtualRouterIds[0]    = 0;

        for (i = 1 ; i < switchPtr->maxVirtualRouters ; i++)
        {
            switchPtr->virtualRouterStates[i] = FM_ROUTER_STATE_ADMIN_DOWN;
            switchPtr->virtualRouterIds[i]    = -1;
        }
    }

    switchPtr->physicalRouterMac = 0;
    switchPtr->virtualRouterMac  = 0;

    err = FM_OK;

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, err);

}   /* end fmRouterAlloc */




/*****************************************************************************/
/** fmRouterFree
 * \ingroup intRouter
 *
 * \desc            Release all routing resources held by a switch.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmRouterFree(fm_int sw)
{
    fm_switch *      switchPtr;
    fm_status        err;

    FM_LOG_ENTRY(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    switchPtr = GET_SWITCH_PTR(sw);

    /**************************************************
     * Destroy route and ARP tables
     **************************************************/
    fmCustomTreeDestroy(&switchPtr->routeTree, DestroyRecord);
    fmCustomTreeDestroy(&switchPtr->ecmpRouteTree, NULL);
    fmCustomTreeDestroy(&switchPtr->arpTree, FreeArp);

    /**************************************************
     * Deallocate ECMP Groups
     **************************************************/
    if (switchPtr->ecmpGroups != NULL)
    {
        fmFree(switchPtr->ecmpGroups);
        switchPtr->ecmpGroups = NULL;
        fmCustomTreeDestroy(&switchPtr->noArpNextHops, NULL);
        fmDeleteBitArray(&switchPtr->ecmpGroupsInUse);
    }

    /**************************************************
     * Deallocate Interface Table
     **************************************************/
    if (switchPtr->ipInterfaceEntries != NULL)
    {
        fmFree(switchPtr->ipInterfaceEntries);
        switchPtr->ipInterfaceEntries = NULL;
#ifdef DEBUG_TRACK_MEMORY_USE
        FM_LOG_PRINT("Routing FREE: ip interface entries %p\n",
                     (void *) switchPtr->ipInterfaceEntries);
#endif
        fmDeleteBitArray(&switchPtr->ipInterfaceEntriesInUse);
#ifdef DEBUG_TRACK_MEMORY_USE
        FM_LOG_PRINT("Routing DELETE BIT ARRAY: ipInterfaceEntriesInUse\n");
#endif
    }

    /**************************************************
     * Deallocate Virtual Router Tables
     **************************************************/
    if (switchPtr->virtualRouterStates != NULL)
    {
        fmFree(switchPtr->virtualRouterStates);
        switchPtr->virtualRouterStates = NULL;
#ifdef DEBUG_TRACK_MEMORY_USE
        FM_LOG_PRINT("Routing FREE: virtual router states %p\n",
                     (void *) switchPtr->virtualRouterStates);
#endif
    }

    if (switchPtr->virtualRouterIds != NULL)
    {
        fmFree(switchPtr->virtualRouterIds);
        switchPtr->virtualRouterIds = NULL;
#ifdef DEBUG_TRACK_MEMORY_USE
        FM_LOG_PRINT("Routing FREE: virtual router IDs %p\n",
                     (void *) switchPtr->virtualRouterIds);
#endif
    }

    err = FM_OK;

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, err);

}   /* end fmRouterFree */




/*****************************************************************************/
/** fmRouterInit
 * \ingroup intRouter
 *
 * \desc            Perform initialization for routing subsystem, called at
 *                  switch initialization time.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmRouterInit(fm_int sw)
{
    fm_switch *    switchPtr;
    fm_int         i;
    fm_status      err;
    fm_bool        supportRouteLookups;
    fm_int         vrid;
    fm_int         prefix;
    fm_customTree *routeLookupTree;

    FM_LOG_ENTRY(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    switchPtr = GET_SWITCH_PTR(sw);

    /* If routing is not supported, exit */
    if (switchPtr->RouterInit == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_OK);
    }

    /**************************************************
     * Init Route Table
     **************************************************/
    if (switchPtr->maxRoutes > 0)
    {
        fmCustomTreeInit(&switchPtr->routeTree, fmCompareIntRoutes);
        fmCustomTreeInit(&switchPtr->ecmpRouteTree, fmCompareEcmpIntRoutes);
    }

    /**************************************************
     * Initialize ECMP Groups
     **************************************************/
    if (switchPtr->maxArpEntries > 0)
    {
        fmClearBitArray(&switchPtr->ecmpGroupsInUse);
        fmCustomTreeInit(&switchPtr->noArpNextHops, fmCompareInternalNextHops);
    }

    /**************************************************
     * Init ARP Table
     **************************************************/
    if (switchPtr->maxArpEntries > 0)
    {
        fmCustomTreeInit(&switchPtr->arpTree, fmCompareInternalArps);
        FM_DLL_INIT_LIST(switchPtr, firstArp, lastArp);
    }

    /**************************************************
     * Init Interface Table
     **************************************************/
    if (switchPtr->maxIpInterfaces > 0)
    {
        fmClearBitArray(&switchPtr->ipInterfaceEntriesInUse);

        for (i = 0 ; i < switchPtr->maxIpInterfaces ; i++)
        {
            fm_intIpInterfaceEntry *entry;

            entry               = &switchPtr->ipInterfaceEntries[i];
            entry->interfaceNum = -1;
            entry->vlan         = FM_INVALID_VLAN;
            entry->state        = FM_INTERFACE_STATE_ADMIN_DOWN;
            fmInitInterfaceEntryLinkedLists(entry);
        }

        fmCustomTreeInit(&switchPtr->noInterfaceNextHops, fmCompareInternalNextHops);
    }

    /**************************************************
     * Init Virtual Router Tables
     **************************************************/
    if (switchPtr->maxVirtualRouters > 0)
    {
        switchPtr->virtualRouterStates[0] = FM_ROUTER_STATE_ADMIN_UP;
        switchPtr->virtualRouterIds[0]    = 0;

        for (i = 1 ; i < switchPtr->maxVirtualRouters ; i++)
        {
            switchPtr->virtualRouterStates[i] = FM_ROUTER_STATE_ADMIN_DOWN;
            switchPtr->virtualRouterIds[i]    = -1;
        }
    }

    switchPtr->physicalRouterMac = 0;
    switchPtr->virtualRouterMac  = 0;

    /************************************************************************
     * If Route Lookups must be supported, allocate and initialize
     * route lookup trees. This must be done during switch initialization
     * since the application may not have an opportunity to configure the
     * API attribute prior to switch insertion.
     ************************************************************************/
    supportRouteLookups = fmGetBoolApiAttribute(FM_AAK_API_SUPPORT_ROUTE_LOOKUPS,
                                                FALSE);
    if (supportRouteLookups)
    {
        i = sizeof(fm_customTree) * switchPtr->maxVirtualRouters * FM_MAX_NUM_IP_PREFIXES;

        switchPtr->routeLookupTrees = fmAlloc(i);

        if (switchPtr->routeLookupTrees == NULL)
        {
            FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_NO_MEM);
        }

#ifdef DEBUG_TRACK_MEMORY_USE
        FM_LOG_PRINT( "Routing MALLOC: route lookup trees %p/%d\n",
                      (void *) switchPtr->routeLookupTrees,
                      i );
#endif

        routeLookupTree = switchPtr->routeLookupTrees;
        for (vrid = 0 ; vrid < switchPtr->maxVirtualRouters ; vrid++)
        {
            for (prefix = 0 ; prefix < FM_MAX_NUM_IP_PREFIXES ; prefix++)
            {
                fmCustomTreeInit(routeLookupTree, fmCompareIPAddrs);
                ++routeLookupTree;
            }
        }
    }


    /**************************************************
     * Perform Switch-Specific Initialization
     **************************************************/
#if 0
    /* commented out because it is happening too early - before the switch
     * is up.  This must be fixed, i.e., the functions that fail ought to
     * be executable pre-boot, but until that is done, the function is
     * executed directly from post-boot-switch code in the fm4000 init file.
     */
    err = switchPtr->RouterInit(sw);
#else
    err = FM_OK;
#endif

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, err);

}   /* end fmRouterInit */



/*****************************************************************************/
/** fmRouterCleanup
 * \ingroup intRouter
 *
 * \desc            Releases memory used by the routing subsystem to support
 *                  a specified switch.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmRouterCleanup(fm_int sw)
{
    fm_switch *           switchPtr;
    fm_int                i;
    fm_status             err;
    fm_intEcmpGroup *     ecmpGroup;
    fm_intNextHop *       nextHopKey;
    fm_intNextHop *       nextHop;
    fm_int                grp;
    fm_int                hop;
    fm_customTreeIterator iter;
    fm_intRouteEntry *    key;
    fm_intRouteEntry *    route;
    fm_intArpEntry *      arpKey;
    fm_intArpEntry *      arpPtr;
    fm_int                vrid;
    fm_int                prefix;
    fm_customTree *       routeLookupTree;

    FM_LOG_ENTRY(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    switchPtr = GET_SWITCH_PTR(sw);

    /***************************************************
     * Destroy route lookup trees.
     ***************************************************/
    if (switchPtr->routeLookupTrees != NULL)
    {
        routeLookupTree = switchPtr->routeLookupTrees;
        for (vrid = 0 ; vrid < switchPtr->maxVirtualRouters ; vrid++)
        {
            for (prefix = 0 ; prefix < FM_MAX_NUM_IP_PREFIXES ; prefix++)
            {
                fmCustomTreeDestroy(routeLookupTree, NULL);
                ++routeLookupTree;
            }
        }

        fmFree(switchPtr->routeLookupTrees);
    }

    /**************************************************
     * Remove all routes from the route trees
     **************************************************/
    if ( fmCustomTreeIsInitialized(&switchPtr->routeTree) )
    {
        while (1)
        {
            fmCustomTreeIterInit(&iter, &switchPtr->routeTree);

            err = fmCustomTreeIterNext(&iter, (void **) &key, (void **) &route);

            if (err == FM_ERR_NO_MORE)
            {
                break;
            }

            FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, err);

            fmCustomTreeRemoveCertain(&switchPtr->routeTree, key, FreeRoute);
        }
    }

    if ( fmCustomTreeIsInitialized(&switchPtr->ecmpRouteTree) )
    {
        while (1)
        {
            fmCustomTreeIterInit(&iter, &switchPtr->ecmpRouteTree);

            err = fmCustomTreeIterNext(&iter, (void **) &key, (void **) &route);

            if (err == FM_ERR_NO_MORE)
            {
                break;
            }

            FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, err);

            fmCustomTreeRemoveCertain(&switchPtr->ecmpRouteTree, key, NULL);
        }
    }

    /**************************************************
     * Remove all ARPs from the arp tree
     **************************************************/
    if (switchPtr->firstArp != NULL)
    {
        if ( fmCustomTreeIsInitialized(&switchPtr->arpTree) )
        {
            while (1)
            {
                fmCustomTreeIterInit(&iter, &switchPtr->arpTree);

                err = fmCustomTreeIterNext( &iter,
                                           (void **) &arpKey,
                                           (void **) &arpPtr);

                if (err == FM_ERR_NO_MORE)
                {
                    break;
                }

                FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, err);

                fmCustomTreeRemoveCertain(&switchPtr->arpTree,
                                          arpKey,
                                          FreeArp);
            }
        }
    }

    /**************************************************
     * Delete all ECMP Groups and Next-Hops
     **************************************************/
    if (switchPtr->ecmpGroups != NULL)
    {
        if ( fmCustomTreeIsInitialized(&switchPtr->noArpNextHops) )
        {
            while (1)
            {
                fmCustomTreeIterInit(&iter, &switchPtr->noArpNextHops);

                err = fmCustomTreeIterNext( &iter,
                                           (void **) &nextHopKey,
                                           (void **) &nextHop);

                if (err == FM_ERR_NO_MORE)
                {
                    break;
                }

                FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, err);

                fmCustomTreeRemoveCertain(&switchPtr->noArpNextHops,
                                          nextHopKey,
                                          NULL);
            }
        }

        if (switchPtr->ecmpGroups != NULL)
        {
            for ( grp = 0 ; grp < switchPtr->maxArpEntries ; grp++)
            {
                ecmpGroup = switchPtr->ecmpGroups[grp];

                if (ecmpGroup == NULL)
                {
                    continue;
                }

                for (hop = 0 ; hop < ecmpGroup->nextHopCount ; hop++)
                {
                    nextHop = ecmpGroup->nextHops[hop];

                    if (nextHop == NULL)
                    {
                        continue;
                    }

                    ecmpGroup->nextHops[hop] = NULL;

                    fmFree(nextHop);
                }

                if ( fmCustomTreeIsInitialized(&ecmpGroup->routeTree) )
                {
                    while (1)
                    {
                        fmCustomTreeIterInit(&iter, &ecmpGroup->routeTree);

                        err = fmCustomTreeIterNext( &iter,
                                                   (void **) &key,
                                                   (void **) &route);

                        if (err == FM_ERR_NO_MORE)
                        {
                            break;
                        }

                        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, err);

                        fmCustomTreeRemoveCertain(&ecmpGroup->routeTree,
                                                  key,
                                                  NULL);
                    }
                }

                switchPtr->ecmpGroups[grp] = NULL;

                FM_API_CALL_FAMILY(err, switchPtr->FreeEcmpGroup, sw, ecmpGroup);

                if (err == FM_ERR_UNSUPPORTED)
                {
                    err = FM_OK;
                }

                FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, err);

                fmFree(ecmpGroup->nextHops);
                fmFree(ecmpGroup);
            }
        }

        fmClearBitArray(&switchPtr->ecmpGroupsInUse);

        switchPtr->dropEcmpGroup = -1;
    }

    /**************************************************
     * Remove All Interfaces
     **************************************************/
    if ( (switchPtr->maxIpInterfaces > 0)
         && (switchPtr->ipInterfaceEntries != NULL) )
    {
        for (i = 0 ; i < switchPtr->maxIpInterfaces ; i++)
        {
            fm_intIpInterfaceEntry *entry;

            entry = &switchPtr->ipInterfaceEntries[i];

            while (entry->firstAddr != NULL)
            {
                fm_intIpInterfaceAddressEntry *addrPtr;

                addrPtr = entry->firstAddr;
                if ( fmCustomTreeIsInitialized(&addrPtr->nextHopTree) )
                {
                    fmCustomTreeDestroy(&addrPtr->nextHopTree, NULL);
                }

                fmRemoveInterfaceAddress(entry, addrPtr);
                fmFree(addrPtr);
#ifdef DEBUG_TRACK_MEMORY_USE
                FM_LOG_PRINT("Routing CLEANUP: ip interface address entry %p\n",
                             (void *) addrPtr);
#endif
            }

            entry->interfaceNum = -1;
        }

        if ( fmCustomTreeIsInitialized(&switchPtr->noInterfaceNextHops) )
        {
            fmCustomTreeDestroy(&switchPtr->noInterfaceNextHops, NULL);
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_OK);

}   /* end fmRouterCleanup */




/*****************************************************************************/
/** fmSetRouterAttribute
 * \ingroup routerBase
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Set a router attribute.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       attr is the router attribute to set (see
 *                  ''Router Attributes'').
 *
 * \param[in]       value points to the attribute value to set
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ATTRIB if attr is not a recognized attribute.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 *
 *****************************************************************************/
fm_status fmSetRouterAttribute(fm_int sw,
                               fm_int attr,
                               void * value)
{
    fm_switch *switchPtr;
    fm_status  err;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, attr = %d\n",
                     sw,
                     attr);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->SetRouterAttribute != NULL)
    {
        err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

        if (err == FM_OK)
        {
            switch (attr)
            {
                case FM_ROUTER_TRAP_TTL1:
                    switchPtr->routerTrapTTL1 = *( (fm_int *) value );
                    break;

                case FM_ROUTER_TRAP_REDIRECT_EVENT:
                    switchPtr->routerTrapRedirectEvent = *( (fm_bool *) value );
                    break;

                case FM_ROUTER_PHYSICAL_MAC_ADDRESS:
                    switchPtr->physicalRouterMac = *( (fm_macaddr *) value );
                    break;

                case FM_ROUTER_VIRTUAL_MAC_ADDRESS:
                    switchPtr->virtualRouterMac = *( (fm_macaddr *) value );
                    /* ignore the bottom 8 bits */
                    switchPtr->virtualRouterMac &= ~0xff;
                    break;

                case FM_ROUTER_TRAP_IP_OPTIONS:
                    switchPtr->routerTrapIpOptions = *( (fm_bool *) value );
                    break;

                default:
                    err = FM_ERR_INVALID_ATTRIB;
                    break;

            }   /* end switch (attr) */

            /* Update the attribute in the hardware */
            if (err == FM_OK)
            {
                err = switchPtr->SetRouterAttribute(sw, attr, value);
            }

            fmReleaseWriteLock(&switchPtr->routingLock);
        }

    }
    else
    {
        err = FM_ERR_UNSUPPORTED;
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmSetRouterAttribute */




/*****************************************************************************/
/** fmGetRouterAttribute
 * \ingroup routerBase
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Get the current value of a router attribute.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       attr is the router attribute to set (see
 *                  ''Router Attributes'').
 *
 * \param[out]      value points to a caller-allocated storage where
 *                  this function will place the value of the attribute.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ATTRIB if attr is not a recognized attribute.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 *
 *****************************************************************************/
fm_status fmGetRouterAttribute(fm_int sw,
                               fm_int attr,
                               void * value)
{
    fm_switch *switchPtr;
    fm_status  err;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, attr = %d\n",
                     sw,
                     attr);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    /* only allowed if chip supports routing: check SetRouterAttribute */
    if (switchPtr->SetRouterAttribute != NULL)
    {
        err = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

        if (err == FM_OK)
        {
            switch (attr)
            {
                case FM_ROUTER_TRAP_TTL1:
                    *( (fm_int *) value ) = switchPtr->routerTrapTTL1;
                    break;

                case FM_ROUTER_TRAP_REDIRECT_EVENT:
                    *( (fm_bool *) value ) = switchPtr->routerTrapRedirectEvent;
                    break;

                case FM_ROUTER_PHYSICAL_MAC_ADDRESS:
                    *( (fm_macaddr *) value ) = switchPtr->physicalRouterMac;
                    break;

                case FM_ROUTER_VIRTUAL_MAC_ADDRESS:
                    *( (fm_macaddr *) value ) = switchPtr->virtualRouterMac;
                    break;

                case FM_ROUTER_TRAP_IP_OPTIONS:
                    *( (fm_bool *) value ) = switchPtr->routerTrapIpOptions;
                    break;

                default:
                    err = FM_ERR_INVALID_ATTRIB;
                    break;

            }   /* end switch (attr) */

            fmReleaseReadLock(&switchPtr->routingLock);
        }

    }
    else
    {
        err = FM_ERR_UNSUPPORTED;
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetRouterAttribute */




/*****************************************************************************/
/** fmCreateVirtualRouter
 * \ingroup routerBase
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Create a virtual router.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       vrid is the ID of the virtual router to create
 *                  (1 to ''FM_MAX_VIRTUAL_ROUTERS'').
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_VRID if vrid is invalid.
 * \return          FM_ERR_VRID_ALREADY_IN_USE if the vrid is already in use.
 * \return          FM_ERR_TOO_MANY_VIRTUAL_ROUTERS if all available hardware
 *                  virtual router slots have been used.
 *
 *****************************************************************************/
fm_status fmCreateVirtualRouter(fm_int sw,
                                fm_int vrid)
{
    fm_switch  *switchPtr;
    fm_status   err = FM_OK;
    fm_int      vroff;
    fm_bool     lockTaken = FALSE;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, vrid = %d\n",
                     sw,
                     vrid);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    lockTaken = TRUE;

    err = fmValidateVirtualRouterId(sw, vrid, &vroff);

    if (err == FM_OK)
    {
        err = FM_ERR_VRID_ALREADY_IN_USE;
    }
    else if (err == FM_ERR_NOT_FOUND)
    {
        switchPtr->virtualRouterIds[vroff] = vrid;

        if (switchPtr->AddVirtualRouter != NULL)
        {
            err = switchPtr->AddVirtualRouter(sw, vroff);

            if (err != FM_OK)
            {
                switchPtr->virtualRouterIds[vroff] = -1;
            }
        }
        else
        {
            err = FM_OK;
        }
    }

ABORT:

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmCreateVirtualRouter */




/*****************************************************************************/
/** fmDeleteVirtualRouter
 * \ingroup routerBase
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Delete a virtual router.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       vrid is the ID of the virtual router to delete.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_VRID if vrid is invalid.
 *
 *****************************************************************************/
fm_status fmDeleteVirtualRouter(fm_int sw,
                                fm_int vrid)
{
    fm_switch *             switchPtr;
    fm_status               err = FM_OK;
    fm_int                  vroff;
    fm_intRouteEntry *      curRoute;
    fm_intRouteEntry *      nextRoute;
    fm_customTreeIterator   iter;
    fm_intRouteEntry *      key;
    fm_bool                 lockTaken = FALSE;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, vrid = %d\n",
                     sw,
                     vrid);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    lockTaken = TRUE;

    if (vrid == 0)
    {
        err = FM_ERR_PHYS_ROUTER_NOT_DELETABLE;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    err = fmValidateVirtualRouterId(sw, vrid, &vroff);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    fmCustomTreeIterInit(&iter, &switchPtr->routeTree);

    /* Delete all routes for this virtual router */
    while (1)
    {
        err = fmCustomTreeIterNext( &iter,
                                   (void **) &key,
                                   (void **) &curRoute );

        if (err == FM_ERR_NO_MORE)
        {
            err = FM_OK;
            break;
        }

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        if (curRoute == NULL)
        {
            break;
        }

        err = fmCustomTreeIterNext( &iter,
                                   (void **) &key,
                                   (void **) &nextRoute );

        if (err == FM_ERR_NO_MORE)
        {
            nextRoute = NULL;
            err       = FM_OK;
        }

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        if ( fmIsRouteEntryUnicast(&curRoute->route) )
        {
            if (curRoute->route.data.unicast.vrid == vrid)
            {
                err = fmDeleteRouteInternal(sw, &curRoute->route);

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

                if (nextRoute != NULL)
                {
                    err = fmCustomTreeIterInitFromKey( &iter,
                                                      &switchPtr->routeTree,
                                                      nextRoute );

                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
                }
            }
        }

        if (nextRoute == NULL)
        {
            break;
        }
    }

    if (switchPtr->RemoveVirtualRouter != NULL)
    {
        err = switchPtr->RemoveVirtualRouter(sw, vroff);
    }

    if (err == FM_OK)
    {
        switchPtr->virtualRouterIds[vroff] = -1;
    }


ABORT:

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmDeleteVirtualRouter */




/*****************************************************************************/
/** fmGetVirtualRouterList
 * \ingroup routerBase
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Retrieve a list of all virtual router IDs created on a
 *                  switch.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      numVrids points to caller allocated storage where this
 *                  function should place the number of virtual router IDs
 *                  returned in vridList.
 *
 * \param[out]      vridList is an array that this function will fill with the
 *                  list of virtual router IDs.
 *
 * \param[in]       max is the size of vridList, being the maximum number of
 *                  virtual router IDs that vridList can hold.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_BUFFER_FULL if max was too small to accommodate
 *                  the entire list of virtual router IDs.
 * \return          FM_ERR_UNSUPPORTED if the switch does not support routing.
 *
 *****************************************************************************/
fm_status fmGetVirtualRouterList(fm_int  sw,
                                 fm_int *numVrids,
                                 fm_int *vridList,
                                 fm_int  max)
{
    fm_switch *switchPtr;
    fm_status  status;
    fm_int     offset;
    fm_int     vrid;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, numVrids = %p, vridList = %p, max = %d\n",
                     sw,
                     (void *) numVrids,
                     (void *) vridList,
                     max);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->virtualRouterIds != NULL)
    {
        *numVrids = 0;
        status    = FM_OK;

        for (offset = 1 ; offset < switchPtr->maxVirtualRouters ; offset++)
        {
            vrid = switchPtr->virtualRouterIds[offset];

            if (vrid != -1)
            {
                if (*numVrids >= max)
                {
                    status = FM_ERR_BUFFER_FULL;
                    break;
                }

                vridList[*numVrids] = vrid;
                (*numVrids)++;
            }
        }
    }
    else
    {
        status = FM_ERR_UNSUPPORTED;
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);
}




/*****************************************************************************/
/** fmGetVirtualRouterFirst
 * \ingroup routerBase
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Retrieve the first virtual router ID.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      firstVrid points to caller allocated storage where this
 *                  function will store the first virtual router ID. Will be
 *                  set to -1 if no virtual routers have been created.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_NO_MORE if there are no virtual routers created.
 * \return          FM_ERR_UNSUPPORTED if the switch does not support routing.
 *
 *****************************************************************************/
fm_status fmGetVirtualRouterFirst(fm_int sw, fm_int *firstVrid)
{
    fm_switch *switchPtr;
    fm_status  status;
    fm_int     offset;
    fm_int     vrid;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, firstVrid = %p\n",
                     sw,
                     (void *) firstVrid);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->virtualRouterIds != NULL)
    {
        for (offset = 1 ; offset < switchPtr->maxVirtualRouters ; offset++)
        {
            vrid = switchPtr->virtualRouterIds[offset];

            if (vrid != -1)
            {
                *firstVrid = vrid;
                status     = FM_OK;

                break;
            }
        }

        if (offset >= switchPtr->maxVirtualRouters)
        {
            *firstVrid = -1;
            status     = FM_ERR_NO_MORE;
        }
    }
    else
    {
        status = FM_ERR_UNSUPPORTED;
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmGetVirtualRouterFirst */




/*****************************************************************************/
/** fmGetVirtualRouterNext
 * \ingroup routerBase
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Retrieve the next virtual router ID.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       currentVrid is the last virtual router ID found by a
 *                  previous call to this function or to
 *                  ''fmGetVirtualRouterFirst''.
 *
 * \param[out]      nextVrid points to caller allocated storage where this
 *                  function will store the next virtual router ID. Will be set
 *                  to -1 if no more virtual router have been created.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_NO_MORE if there are no more virtual router IDs.
 * \return          FM_ERR_UNSUPPORTED if the switch does not support routing.
 *
 *****************************************************************************/
fm_status fmGetVirtualRouterNext(fm_int  sw,
                                 fm_int  currentVrid,
                                 fm_int *nextVrid)
{
    fm_switch *switchPtr;
    fm_status  status;
    fm_int     offset;
    fm_int     vrid;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, currentVrid = %d, nextVrid = %p\n",
                     sw,
                     currentVrid,
                     (void *) nextVrid);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->virtualRouterIds != NULL)
    {
        status = fmValidateVirtualRouterId(sw, currentVrid, &offset);

        if (status == FM_OK)
        {
            for (offset++ ; offset < switchPtr->maxVirtualRouters ; offset++)
            {
                vrid = switchPtr->virtualRouterIds[offset];

                if (vrid != -1)
                {
                    *nextVrid = vrid;
                    break;
                }
            }

            if (offset >= switchPtr->maxVirtualRouters)
            {
                *nextVrid = -1;
                status    = FM_ERR_NO_MORE;
            }
        }
    }
    else
    {
        status = FM_ERR_UNSUPPORTED;
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);
}




/*****************************************************************************/
/** fmSetRouterState
 * \ingroup routerBase
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Set the state for a virtual router. If the
 *                  router state is set to DOWN, then all routes
 *                  using that router will immediately cease to be active. If
 *                  the router state is set to UP, then routes using that router
 *                  will become active only if their interfaces, VLANs and the
 *                  routes themselves are active.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       vrid is the virtual router ID  on which to operate
 *                  (0 for the real router).
 *
 * \param[in]       state is the desired state of the router.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_VRID if vrid is invalid.
 *
 *****************************************************************************/
fm_status fmSetRouterState(fm_int         sw,
                           fm_int         vrid,
                           fm_routerState state)
{
    fm_switch *           switchPtr = NULL; /* Avoid compiler warning */
    fm_status             err;
    fm_intRouteEntry *    curRoute;
    fm_bool               setRoute;
    fm_int                vroff;
    fm_bool               lockTaken = FALSE;
    fm_customTreeIterator iter;
    fm_intRouteEntry *    key;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, vrid = %d, state = %d\n",
                     sw,
                     vrid,
                     state);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    err = fmValidateVirtualRouterId(sw, vrid, &vroff);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    switchPtr = GET_SWITCH_PTR(sw);

    if ( (switchPtr->SetRouteActive == NULL) ||
         (switchPtr->SetRouterState == NULL) )
    {
        err = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    lockTaken = TRUE;

    if (switchPtr->virtualRouterStates[vroff] == state)
    {
        goto ABORT;
    }

    switchPtr->virtualRouterStates[vroff] = state;

    err = switchPtr->SetRouterState(sw, vroff, state);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    /*
     *  Update active status for each route
     *  in the virtual router
     */
    fmCustomTreeIterInit(&iter, &switchPtr->routeTree);

    err = fmCustomTreeIterNext( &iter, (void **) &key, (void **) &curRoute );

    if (err == FM_ERR_NO_MORE)
    {
        curRoute = NULL;
        err      = FM_OK;
    }

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    while (curRoute != NULL)
    {
        setRoute = FALSE;

        if ( fmIsRouteEntryUnicast(&curRoute->route) )
        {
            if (curRoute->route.data.unicast.vrid == vrid)
            {
                setRoute = TRUE;
            }
        }
        else if (vrid == 0)
        {
            /* all multicast routes use virtual router 0 */
            setRoute = TRUE;
        }

        if (setRoute)
        {
            err = fmSetRouteActiveFlag(sw, curRoute, TRUE);

            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
        }

        err = fmCustomTreeIterNext( &iter,
                                   (void **) &key,
                                   (void **) &curRoute );

        if (err == FM_ERR_NO_MORE)
        {
            curRoute = NULL;
            err      = FM_OK;
        }

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }


ABORT:

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmSetRouterState */




/*****************************************************************************/
/** fmGetRouterState
 * \ingroup routerBase
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Retrieve the current state for a virtual router.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       vrid is the virtual router ID  on which to operate
 *                  (0 for the real router).
 *
 * \param[out]      state points to caller-allocated storage where this
 *                  function will place the current router state.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_VRID if vrid is invalid.
 *
 *****************************************************************************/
fm_status fmGetRouterState(fm_int          sw,
                           fm_int          vrid,
                           fm_routerState *state)
{
    fm_switch *switchPtr;
    fm_int     vroff;
    fm_status  err;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, vrid = %d\n",
                     sw,
                     vrid);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    err = fmValidateVirtualRouterId(sw, vrid, &vroff);

    if (err == FM_OK)
    {
        switchPtr = GET_SWITCH_PTR(sw);

        *state = switchPtr->virtualRouterStates[vroff];
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetRouterState */




/*****************************************************************************/
/** fmAddRouteInternal
 * \ingroup intRoute
 *
 * \desc            Internal function to add a route.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       route points to the route to add.
 *
 * \param[in]       state is the desired state of this route when the route
 *                  is created.
 *
 * \param[in]       action points to the desired routing action for this route.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if action parameter is NULL.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_INVALID_ROUTE if the route description is invalid.
 * \return          FM_ERR_ALREADY_EXISTS if the route already exists.
 * \return          FM_ERR_TABLE_FULL if the routing table is full.
 *
 *****************************************************************************/
fm_status fmAddRouteInternal(fm_int          sw,
                             fm_routeEntry * route,
                             fm_routeState   state,
                             fm_routeAction *action)
{
    fm_switch *               switchPtr;
    fm_status                 err;
    fm_intRouteEntry *        routeEntry;
    fm_intRouteEntry *        curRoute;
    fm_intIpInterfaceEntry *  ifEntry;
    fm_bool                   routeAllocated;
    fm_bool                   routeAddedToRouteTree;
    fm_bool                   routeAddedToAlternateTree;
    fm_bool                   routeAddedToLookupTree;
    fm_bool                   routeAddedToHardware;
    fm_bool                   ecmpGroupCreated;
    fm_bool                   routeAddedToEcmpGroup;
    fm_intMulticastGroup *    group;
    fm_int                    vrid;
    fm_int                    vroff;
    fm_int                    prefixLength;
    fm_unicastRouteEntry *    unicast;
    fm_unicastECMPRouteEntry *unicastEcmp;
    fm_multicastAddress *     multicast;
    fm_intRouteEntry          key;
    fm_customTree *           routeTree;
    fm_ecmpNextHop            nextHop;
    fm_intEcmpGroup *         ecmpGroup;
    fm_bool                   needEcmpGroup;
    fm_nextHop *              arpNextHop;
    fm_int                    routePrefixLength;
    fm_customTree *           routeLookupTree;

    FM_LOG_ENTRY(FM_LOG_CAT_ROUTING,
                 "sw = %d, route=%p, state=%d, action=%p(%d)\n",
                 sw,
                 (void *) route,
                 state,
                 (void *) action,
                 (action != NULL) ? action->action : FM_ROUTE_ACTION_MAX);

    if ( (route == NULL) || (action == NULL) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    FM_CLEAR( nextHop );

    nextHop.type              = FM_NEXTHOP_TYPE_ARP;
    arpNextHop                = &nextHop.data.arp;
    switchPtr                 = GET_SWITCH_PTR(sw);
    routeEntry                = NULL;
    routeAllocated            = FALSE;
    routeAddedToRouteTree     = FALSE;
    routeAddedToAlternateTree = FALSE;
    ecmpGroupCreated          = FALSE;
    routeAddedToEcmpGroup     = FALSE;
    routeAddedToLookupTree    = FALSE;
    routeAddedToHardware      = FALSE;
    group                     = NULL;
    unicast                   = NULL;
    routeTree                 = NULL;
    ecmpGroup                 = NULL;
    routeLookupTree           = NULL;

    /* Apply Masks to route fields to ensure address consistency */
    err = fmApplyMasksToRoute(route);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    /* See if the exact route already exists */
    key.route = *route;
    err       = fmCustomTreeFind(&switchPtr->routeTree,
                                 &key,
                                 (void **) &curRoute);

    if (err != FM_ERR_NOT_FOUND)
    {
        if (err == FM_OK)
        {
            err = FM_ERR_ALREADY_EXISTS;
        }

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    /* Validate the action and determine whether an ECMP group is needed */
    switch (action->action)
    {
        case FM_ROUTE_ACTION_RPF_FAILURE:
        case FM_ROUTE_ACTION_NOP:
            needEcmpGroup = FALSE;
            break;

        case FM_ROUTE_ACTION_ROUTE:
        case FM_ROUTE_ACTION_DROP:
            needEcmpGroup = TRUE;
            break;

        default:
            err = FM_ERR_UNSUPPORTED;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    /********************************************************************
     * Collect vrid and prefixLength information needed for further
     * processing.
     * Note that prefixLength includes the destination IP prefix, source
     * IP prefix, and VLAN prefix using these specific bit positions:
     * 0-3 : Vlan prefix     (FM_VLAN_PREFIX_POSITION)
     * 4-11 : Src IP prefix  (FM_SRC_IP_PREFIX_POSITION)
     * 12-19 : Dst IP prefix (FM_DST_IP_PREFIX_POSITION)
     ********************************************************************/
    switch (route->routeType)
    {
        case FM_ROUTE_TYPE_UNICAST:
            arpNextHop->addr          = route->data.unicast.nextHop;
            arpNextHop->interfaceAddr = route->data.unicast.interfaceAddr;
            arpNextHop->vlan          = route->data.unicast.vlan;
            unicast                   = &route->data.unicast;
            vrid                      = unicast->vrid;
            routePrefixLength         = unicast->prefixLength;
            prefixLength              = (routePrefixLength << FM_DST_IP_PREFIX_POSITION)
                                        & FM_DST_IP_PREFIX_MASK;
            break;

        case FM_ROUTE_TYPE_UNICAST_ECMP:
            unicastEcmp       = &route->data.unicastECMP;
            vrid              = unicastEcmp->vrid;
            routePrefixLength = unicastEcmp->prefixLength;
            prefixLength      = (routePrefixLength << FM_DST_IP_PREFIX_POSITION)
                                & FM_DST_IP_PREFIX_MASK;
            break;

        case FM_ROUTE_TYPE_MULTICAST:
            vrid      = 0;
            multicast = &route->data.multicast;

            switch (multicast->addressType)
            {
                case FM_MCAST_ADDR_TYPE_DSTIP:
                    routePrefixLength = multicast->info.dstIpRoute.dstPrefixLength;
                    prefixLength      = (routePrefixLength<< FM_DST_IP_PREFIX_POSITION)
                                        & FM_DST_IP_PREFIX_MASK;
                    break;

                case FM_MCAST_ADDR_TYPE_DSTIP_VLAN:
                    routePrefixLength = multicast->info.dstIpVlanRoute.dstPrefixLength;
                    prefixLength      = ((routePrefixLength << FM_DST_IP_PREFIX_POSITION)
                                          & FM_DST_IP_PREFIX_MASK)
                                        | ((multicast->info.dstIpVlanRoute.vlanPrefixLength)
                                          & FM_VLAN_PREFIX_MASK);
                    break;

                case FM_MCAST_ADDR_TYPE_DSTIP_SRCIP:
                    routePrefixLength = multicast->info.dstSrcIpRoute.dstPrefixLength;
                    prefixLength      = ((routePrefixLength << FM_DST_IP_PREFIX_POSITION)
                                        & FM_DST_IP_PREFIX_MASK) |
                                        ((multicast->info.dstSrcIpRoute.srcPrefixLength
                                          << FM_SRC_IP_PREFIX_POSITION)
                                        & FM_SRC_IP_PREFIX_MASK);
                    break;

                case FM_MCAST_ADDR_TYPE_DSTIP_SRCIP_VLAN:
                    routePrefixLength = multicast->info.dstSrcIpVlanRoute.dstPrefixLength;
                    prefixLength      = ((routePrefixLength << FM_DST_IP_PREFIX_POSITION)
                                        & FM_DST_IP_PREFIX_MASK) |
                                        ((multicast->info.dstSrcIpVlanRoute.srcPrefixLength
                                          << FM_SRC_IP_PREFIX_POSITION)
                                        & FM_SRC_IP_PREFIX_MASK) |
                                        ((multicast->info.dstSrcIpVlanRoute.vlanPrefixLength)
                                        & FM_VLAN_PREFIX_MASK);
                    break;

                default:
                    err = FM_ERR_INVALID_ARGUMENT;
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
                    break;

            }   /* end switch (multicast->addressType) */
            break;

        default:
            err = FM_ERR_INVALID_ARGUMENT;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    err = fmValidateVirtualRouterId(sw, vrid, &vroff);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    ifEntry = NULL;

    if (route->routeType == FM_ROUTE_TYPE_UNICAST)
    {
        /* If the interface address is non-zero */
        if ( unicast->interfaceAddr.isIPv6
            || (unicast->interfaceAddr.addr[0] != 0) )
        {
            /* try to find an interface using the address.
             * If one is found, store the interface entry pointer and
             * retrieve the vlan for that interface.  Otherwise, set the
             * interface pointer to NULL and invalidate the vlan.
             */
            err = fmFindInterface(sw, &unicast->interfaceAddr, &ifEntry);

            if (err != FM_OK)
            {
                if (err != FM_ERR_INVALID_INTERFACE)
                {
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
                }
            }
        }
    }
    else if (route->routeType == FM_ROUTE_TYPE_MULTICAST)
    {
        group = fmFindMcastGroup(sw, route->data.multicast.mcastGroup);

        if (group == NULL)
        {
            err = FM_ERR_INVALID_MULTICAST_GROUP;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
        }
    }

    /* Allocate and initialize a new route record */
    routeEntry = fmAlloc( sizeof(fm_intRouteEntry) );

    if (routeEntry == NULL)
    {
        err = FM_ERR_NO_MEM;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    routeAllocated = TRUE;

    FM_CLEAR(*routeEntry);

    routeEntry->switchPtr   = switchPtr;
    routeEntry->route       = *route;
    routeEntry->state       = state;
    routeEntry->action      = *action;
    routeEntry->prefix      = prefixLength;
    routeEntry->ecmpGroupId = -1;
    routeEntry->mcastGroup  = group;

    /* Initialize vn tunnels tree. No tunnels are currently using this route. */
    fmTreeInit(&routeEntry->vnTunnelsTree);

    ecmpGroup = NULL;

    /* Determine which route tree to use for this route type */
    routeTree = GetRouteTree(sw, route);

    if (routeTree == NULL)
    {
        err = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    /* If an alternate route tree is used for this route type, search again. */
    if (routeTree != &switchPtr->routeTree)
    {
        err = fmCustomTreeFind(routeTree, &key, (void **) &curRoute);
    }
    else
    {
        err = FM_ERR_NOT_FOUND;
    }

    switch (route->routeType)
    {
        case FM_ROUTE_TYPE_UNICAST:
            routeEntry->destIPAddress = &routeEntry->route.data.unicast.dstAddr;

            if (err == FM_ERR_NOT_FOUND)
            {
                /* This is the first route in an ECMP group. */
                switch (action->action)
                {
                    case FM_ROUTE_ACTION_ROUTE:
                        /* Create the ECMP group */
                        err = fmCreateECMPGroupInternal(sw,
                                                        &routeEntry->ecmpGroupId,
                                                        NULL,
                                                        NULL);

                        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

                        err = fmAddECMPGroupNextHopsInternal(sw,
                                                             routeEntry->ecmpGroupId,
                                                             1,
                                                             &nextHop);

                        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

                        ecmpGroupCreated = TRUE;
                        break;

                    case FM_ROUTE_ACTION_RPF_FAILURE:
                    case FM_ROUTE_ACTION_NOP:
                        break;

                    case FM_ROUTE_ACTION_DROP:
                        /* Use the drop ECMP group */
                        if (switchPtr->dropEcmpGroup >= 0)
                        {
                            routeEntry->ecmpGroupId = switchPtr->dropEcmpGroup;
                        }
                        else
                        {
                            err = FM_ERR_UNSUPPORTED;
                            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
                        }
                        break;

                    default:
                        err = FM_ERR_UNSUPPORTED;
                        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
                }
            }
            else if (err == FM_OK)
            {
                /* a matching route was found in the ECMP route table.
                 * If the found route is not the same type, this is an error.
                 * If the requested action is anything other than route,
                 * this is also an error.
                 * Otherwise, add the new route's next hop to the existing
                 * route's ecmp group.
                 */
                if (curRoute->route.routeType != FM_ROUTE_TYPE_UNICAST)
                {
                    err = FM_ERR_ALREADY_EXISTS;
                }
                else if ( (action->action != FM_ROUTE_ACTION_ROUTE)
                    || (curRoute->action.action != FM_ROUTE_ACTION_ROUTE) )
                {
                    err = FM_ERR_UNSUPPORTED;
                }

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

                err = fmAddECMPGroupNextHopsInternal(sw,
                                                     curRoute->ecmpGroupId,
                                                     1,
                                                     &nextHop);

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

                routeEntry->ecmpGroupId = curRoute->ecmpGroupId;

                /* Add the route to the tree */
                err = fmCustomTreeInsert(&switchPtr->routeTree,
                                         routeEntry,
                                         routeEntry);

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

                /* This is all we need to do for this case.  err is already
                 * equal to FM_OK, so just set the route added flag
                 * and go to ABORT to finish up. */
                routeAddedToRouteTree = TRUE;
                goto ABORT;
            }
            else
            {
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
            }
            break;

        case FM_ROUTE_TYPE_UNICAST_ECMP:
            routeEntry->destIPAddress = &routeEntry->route.data.unicastECMP.dstAddr;

            if (err != FM_ERR_NOT_FOUND)
            {
                if (err == FM_OK)
                {
                    err = FM_ERR_ALREADY_EXISTS;
                }

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
            }

            if (action->action == FM_ROUTE_ACTION_ROUTE)
            {
                routeEntry->ecmpGroupId = route->data.unicastECMP.ecmpGroup;
            }
            else if (action->action == FM_ROUTE_ACTION_DROP)
            {
                routeEntry->ecmpGroupId = switchPtr->dropEcmpGroup;
            }
            break;

        case FM_ROUTE_TYPE_MULTICAST:
            if (err != FM_ERR_NOT_FOUND)
            {
                if (err == FM_OK)
                {
                    err = FM_ERR_ALREADY_EXISTS;
                }

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
            }

            if ( needEcmpGroup && (group != NULL) )
            {
                routeEntry->ecmpGroupId = group->ecmpGroup;
            }
            break;

        default:
            break;
    }

    /* Determine which route lookup tree to use, if any */
    if (switchPtr->routeLookupTrees != NULL)
    {
        if (routeEntry->destIPAddress != NULL)
        {
            err = fmGetRouteLookupTree(sw, vrid, routePrefixLength, &routeLookupTree);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
        }
    }

    if (routeEntry->ecmpGroupId >= 0)
    {
        ecmpGroup = switchPtr->ecmpGroups[routeEntry->ecmpGroupId];
    }

    fmSetRouteActiveFlag(sw, routeEntry, FALSE);

    /* Add the route to the tree */
    err = fmCustomTreeInsert(&switchPtr->routeTree, routeEntry, routeEntry);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    routeAddedToRouteTree = TRUE;

#if 0
    /* Find its successor route, if any */
    err = fmCustomTreeSuccessor(&switchPtr->routeTree,
                                routeEntry,
                                (void **) &curKey,
                                (void **) &curRoute);

    if (err != FM_OK)
    {
        if (err == FM_ERR_NOT_FOUND)
        {
            goto ABORT;
        }

        if (err == FM_ERR_NO_MORE)
        {
            curRoute = NULL;
        }
    }

    /* new entry goes before the current entry */
    fmInsertRouteBefore(switchPtr, curRoute, routeEntry);

    routeAddedToLinkedList = TRUE;
#endif

    /* If this route type uses a different route tree in addition to the
     * generic route tree, add the route to the additional tree now. */
    if (routeTree != &switchPtr->routeTree)
    {
        /* Add the route to the ECMP tree */
        err = fmCustomTreeInsert(routeTree, routeEntry, routeEntry);

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        routeAddedToAlternateTree = TRUE;
    }

    if (ecmpGroup != NULL)
    {
        err = fmCustomTreeInsert(&ecmpGroup->routeTree,
                                 routeEntry,
                                 routeEntry);

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        routeAddedToEcmpGroup = TRUE;
    }

    if (routeLookupTree != NULL)
    {
        err = fmCustomTreeInsert(routeLookupTree,
                                 routeEntry->destIPAddress,
                                 routeEntry);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        routeAddedToLookupTree = TRUE;
    }

    /* Now add the route into the hardware */
    err = switchPtr->AddRoute(sw, routeEntry);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    
    routeAddedToHardware = TRUE;

    if ( (group != NULL) && (group->routePtrPtr != NULL) )
    {
        *group->routePtrPtr = routeEntry;
    }

    err = fmNotifyVNTunnelAboutRouteChange(sw);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);


ABORT:

    if (err != FM_OK)
    {
        if (routeAddedToHardware)
        {
            switchPtr->DeleteRoute(sw, routeEntry);
            if ( (group != NULL) && (group->routePtrPtr != NULL) )
            {
                *group->routePtrPtr = NULL;
            }
        }
        if (routeAddedToEcmpGroup)
        {
            fmCustomTreeRemove(&ecmpGroup->routeTree, routeEntry, NULL);
        }

        if (routeAddedToRouteTree)
        {
            fmCustomTreeRemove(&switchPtr->routeTree, routeEntry, NULL);
        }

        if (routeAddedToAlternateTree)
        {
            fmCustomTreeRemove(routeTree, routeEntry, NULL);
        }

        if (ecmpGroupCreated)
        {
            fmDeleteECMPGroupInternal(sw, routeEntry->ecmpGroupId);
        }

        if (routeAddedToLookupTree)
        {
            fmCustomTreeRemove(routeLookupTree, routeEntry->destIPAddress, NULL);
        }

        if (routeAllocated)
        {
            fmFree(routeEntry);
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, err);


}   /* end fmAddRouteInternal */




/*****************************************************************************/
/** fmAddRoute
 * \ingroup routerRoute
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Add a new routing entry to the router table. This
 *                  function will automatically store the route in order to
 *                  ensure that the longest prefix match will be picked first.
 *                  This service allows setting the route state immediately.
 *                  The route is added to the switch regardless whether the
 *                  route is valid and/or active.
 *
 * \note            It is permitted to add a route before the associated
 *                  interface exists. The route will be loaded into the switch
 *                  but will be inactive until the interface is created and
 *                  brought up.
 *                                                                      \lb\lb
 *                  Also, if there is no ARP table entry to resolve the next
 *                  hop's destination MAC address, then this function
 *                  automatically uses the CPU's destination MAC address,
 *                  forcing any frame using that route to be forwarded to the
 *                  CPU for further processing.
 *                                                                      \lb\lb
 *                  Addition of multicast routes is not permitted using this
 *                  function.  To add a multicast route, use the multicast
 *                  group subsystem.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       route points to the route to add.
 *
 * \param[in]       state is the desired state of this route when the route
 *                  is created.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_INVALID_ROUTE if the route description is invalid.
 * \return          FM_ERR_ALREADY_EXISTS if the route already exists.
 * \return          FM_ERR_TABLE_FULL if the routing table is full.
 * \return          FM_ERR_ARP_TABLE_FULL if there is no more room in the ARP
 *                  table.
 * \return          FM_ERR_NO_FFU_RES_FOUND if no suitable FFU resources are
 *                  available for this route.
 * \return          FM_ERR_USE_MCAST_FUNCTIONS if an attempt was made to
 *                  add a multicast route.
 *
 *****************************************************************************/
fm_status fmAddRoute(fm_int         sw,
                     fm_routeEntry *route,
                     fm_routeState  state)
{
    fm_status             err;

    FM_LOG_ENTRY_API( FM_LOG_CAT_ROUTING,
                      "sw = %d, route=%p, state=%d\n",
                      sw,
                      (void *) route,
                      state );

    err = fmAddRouteExt(sw, route, state, NULL);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmAddRoute */




/*****************************************************************************/
/** fmAddRouteExt
 * \ingroup routerRoute
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Add a new routing entry to the router table along with
 *                  a specified route action. This function will automatically
 *                  store the route in order to ensure that the longest prefix
 *                  match will be picked first. This service allows setting 
 *                  the route state immediately. The route is added to the 
 *                  switch regardless whether the route is valid and/or active.
 *
 * \note            It is permitted to add a route before the associated
 *                  interface exists. The route will be loaded into the switch
 *                  but will be inactive until the interface is created and
 *                  brought up.
 *                                                                      \lb\lb
 *                  Also, if there is no ARP table entry to resolve the next
 *                  hop's destination MAC address, then this function
 *                  automatically uses the CPU's destination MAC address,
 *                  forcing any frame using that route to be forwarded to the
 *                  CPU for further processing.
 *                                                                      \lb\lb
 *                  Addition of multicast routes is not permitted using this
 *                  function.  To add a multicast route, use the multicast
 *                  group subsystem.
 *                                                                      \lb\lb
 *                  Routes that are added with a 'drop' action specified will
 *                  not have an ARP entry allocated to them.  This means that
 *                  a later call to ''fmSetRouteAction'' changing the route
 *                  action to 'route' could fail with FM_ERR_ARP_TABLE_FULL.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       route points to the route to add.
 *
 * \param[in]       state is the desired state of this route when the route
 *                  is created.
 *
 * \param[in]       action points to the desired routing action for this route.
 *                  If NULL, the default action is ''FM_ROUTE_ACTION_ROUTE''.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_INVALID_ROUTE if the route description is invalid.
 * \return          FM_ERR_ALREADY_EXISTS if the route already exists.
 * \return          FM_ERR_TABLE_FULL if the routing table is full.
 * \return          FM_ERR_ARP_TABLE_FULL if there is no more room in the ARP
 *                  table.
 * \return          FM_ERR_NO_FFU_RES_FOUND if no suitable FFU resources are
 *                  available for this route.
 * \return          FM_ERR_USE_MCAST_FUNCTIONS if an attempt was made to
 *                  add a multicast route.
 *
 *****************************************************************************/
fm_status fmAddRouteExt(fm_int          sw,
                        fm_routeEntry * route,
                        fm_routeState   state,
                        fm_routeAction *action)
{
    fm_switch *           switchPtr;
    fm_status             err;
    fm_ipAddr             destAddr;
    fm_int                maxPrefix;
    fm_int                prefixLength;
    static fm_routeAction defaultAction =
    {
        .action = FM_ROUTE_ACTION_ROUTE
    };
    fm_routeAction *      routeAction;

    if (action == NULL)
    {
        routeAction = &defaultAction;
    }
    else
    {
        routeAction = action;
    }

    FM_LOG_ENTRY_API( FM_LOG_CAT_ROUTING,
                      "sw = %d, route=%p, state=%d, action=%p(%d)\n",
                      sw,
                      (void *) route,
                      state,
                      (void *) action,
                      routeAction->action );

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if ( (switchPtr->AddRoute == NULL) || (switchPtr->maxRoutes <= 0) )
    {
        err = FM_ERR_UNSUPPORTED;
        goto ABORT;
    }

    /* error-check incoming route information */
    if (route == NULL)
    {
        err = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    if ( !fmIsRouteEntryUnicast(route) )
    {
        err = FM_ERR_USE_MCAST_FUNCTIONS;
        goto ABORT;
    }

    fmGetRouteDestAddress(route, &destAddr);

    prefixLength = route->data.unicast.prefixLength;

    maxPrefix = (destAddr.isIPv6)
                ? FM_IPV6_MAX_PREFIX_LENGTH
                : FM_IPV4_MAX_PREFIX_LENGTH;

    if ( (prefixLength < 0) || (prefixLength > maxPrefix) )
    {
        err = FM_ERR_INVALID_ARGUMENT;
        goto ABORT;
    }

    /* gain exclusive access to routing tables */
    err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    if (err != FM_OK)
    {
        goto ABORT;
    }

    err = fmAddRouteInternal(sw, route, state, routeAction);

    /* release exclusive access to routing tables */
    fmReleaseWriteLock(&switchPtr->routingLock);

ABORT:

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmAddRouteExt */




/*****************************************************************************/
/** fmDeleteRouteInternal
 * \ingroup intRoute
 *
 * \desc            Delete a routing entry from the router table.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       route points to the route to delete.
 *                  If the route is a unicast route and the nextHop IP address
 *                  is set to 0xffffffff, all routes with the specified
 *                  destination address and prefix length will be deleted.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if route is NULL.
 * \return          FM_ERR_INVALID_ROUTE if the route description is invalid.
 * \return          FM_ERR_NOT_FOUND if the route does not exist.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 *
 *****************************************************************************/
fm_status fmDeleteRouteInternal(fm_int         sw,
                                fm_routeEntry *route)
{
    fm_switch *           switchPtr;
    fm_status             err = FM_OK;
    fm_intRouteEntry *    curRoute;
    fm_intRouteEntry *    ecmpRoute;
    fm_intRouteEntry      key;
    fm_customTree *       routeTree;
    fm_intEcmpGroup *     ecmpGroup;
    fm_ecmpNextHop *      nextHopList = NULL;
    fm_int                nextHopCount = 0;
    fm_int                iplen;
    fm_int                i;
    fm_int                size;
    fm_intNextHop *       intNextHop;
    fm_bool               wildCard = FALSE;
    fm_int                routePrefixLength;
    fm_customTree *       routeLookupTree;
    fm_int                vrid;

    FM_LOG_ENTRY(FM_LOG_CAT_ROUTING,
                 "sw = %d, route=%p\n",
                 sw,
                 (void *) route);

    switchPtr = GET_SWITCH_PTR(sw);

    if (route == NULL)
    {
        err = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    routePrefixLength = 0;
    routeLookupTree   = NULL;
    vrid              = 0;

    /* Apply Masks to route fields to ensure address consistency */
    err = fmApplyMasksToRoute(route);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    /* Get destination IP address and route prefix length */
    switch (route->routeType)
    {
        case FM_ROUTE_TYPE_UNICAST:
            routePrefixLength = route->data.unicast.prefixLength;
            vrid              = route->data.unicast.vrid;
            break;

        case FM_ROUTE_TYPE_UNICAST_ECMP:
            routePrefixLength = route->data.unicastECMP.prefixLength;
            vrid              = route->data.unicastECMP.vrid;
            break;

        case FM_ROUTE_TYPE_MULTICAST:
            break;

        default:
            err = FM_ERR_INVALID_ARGUMENT;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    /* If this is unicast route type, is it also a wildcard deletion? */
    if (route->routeType == FM_ROUTE_TYPE_UNICAST)
    {
        /* Check for wildcard deletion - are all word(s) of nexthop IP
         * address equal to 0xffffffff? */
        if (route->data.unicast.nextHop.isIPv6)
        {
            iplen = 4;
        }
        else
        {
            iplen = 1;
        }

        for (i = 0 ; i < iplen ; i++)
        {
            if (route->data.unicast.nextHop.addr[i] != 0xffffffff)
            {
                break;
            }
        }

        if (i >= iplen)
        {
            wildCard = TRUE;
        }
    }

    routeTree = GetRouteTree(sw, route);

    if (routeTree == NULL)
    {
        err = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    /* Try to find the route */
    key.route = *route;

    if (wildCard)
    {
        err = fmCustomTreeFind( routeTree, &key, (void **) &curRoute );
    }
    else
    {
        err = fmCustomTreeFind( &switchPtr->routeTree,
                                &key,
                                (void **) &curRoute );
    }

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    /* Determine which route lookup tree to use, if any */
    if ( (switchPtr->routeLookupTrees != NULL) && (curRoute->destIPAddress != NULL) )
    {
        err = fmGetRouteLookupTree(sw, vrid, routePrefixLength, &routeLookupTree);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    if (curRoute->ecmpGroupId >= 0)
    {
        ecmpGroup = switchPtr->ecmpGroups[curRoute->ecmpGroupId];
    }
    else
    {
        ecmpGroup = NULL;
    }

    if (routeTree != &switchPtr->routeTree)
    {
        err = fmCustomTreeFind( routeTree, &key, (void **) &ecmpRoute );

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }
    else
    {
        ecmpRoute = NULL;
    }

    if ( (curRoute->route.routeType == FM_ROUTE_TYPE_UNICAST)
        && (ecmpGroup != NULL) )
    {
        /* Allocate storage for the nexthop(s) that need to be deleted. */
        size        = sizeof(fm_ecmpNextHop) * switchPtr->maxEcmpGroupSize;
        nextHopList = fmAlloc(size);

        if (nextHopList == NULL)
        {
            err = FM_ERR_NO_MEM;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
        }

        FM_MEMSET_S(nextHopList, size, 0, size);

        if (wildCard)
        {
            /* ECMP wild-card deletion, delete all next-hops */
            err = fmGetECMPGroupNextHopListInternal(sw,
                                                    curRoute->ecmpGroupId,
                                                    &nextHopCount,
                                                    nextHopList,
                                                    switchPtr->maxEcmpGroupSize);

            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
        }
        else
        {
            /* Single-route deletion */
            nextHopList->type                   = FM_NEXTHOP_TYPE_ARP;
            nextHopList->data.arp.addr          = route->data.unicast.nextHop;
            nextHopList->data.arp.interfaceAddr = route->data.unicast.interfaceAddr;
            nextHopList->data.arp.vlan          = route->data.unicast.vlan;
            nextHopCount                        = 1;
        }

        /* try to delete the next-hop(s) from the dedicated ECMP group */
        err = fmDeleteECMPGroupNextHopsInternal(sw,
                                                curRoute->ecmpGroupId,
                                                nextHopCount,
                                                nextHopList);

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        /* If dedicated ECMP group is not empty, we're done. */
        if (ecmpGroup->nextHopCount > 0)
        {
            /* Remove this route from the route tree. */
            fmCustomTreeRemove(&switchPtr->routeTree, curRoute, NULL);

            if (curRoute == ecmpRoute)
            {
                /* This route is the ECMP route, we have to remove it from the
                 * ECMP route tree and put one of the remaining routes
                 * in that tree in it's place. */
                fmCustomTreeRemove(routeTree, ecmpRoute, NULL);
                fmCustomTreeRemove(&ecmpGroup->routeTree, ecmpRoute, NULL);

                /* There is no way that ecmpRoute could be NULL, because we
                 * are only here if curRoute is equal to ecmpRoute, and if
                 * curRoute is NULL, we segfaulted a long ways back in the
                 * code. Nevertheless, Klocwork thinks it is possible, and
                 * it is easier to just make KW happy than to fight it. */
                if ( (routeLookupTree != NULL) && (ecmpRoute != NULL) )
                {
                    fmCustomTreeRemove(routeLookupTree,
                                       ecmpRoute->destIPAddress,
                                       NULL);
                }

                /* Get the first remaining next hop from the ECMP group. */
                intNextHop = ecmpGroup->nextHops[0];

                /* Build a new route key using the next hop information. */
                key.route.data.unicast.nextHop       = intNextHop->nextHop.data.arp.addr;
                key.route.data.unicast.interfaceAddr = intNextHop->nextHop.data.arp.interfaceAddr;
                key.route.data.unicast.vlan          = intNextHop->nextHop.data.arp.vlan;

                /* Find the target route in the main route tree. */
                err = fmCustomTreeFind( &switchPtr->routeTree,
                                       &key,
                                       (void **) &ecmpRoute);

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

                /* Add the target route to the ECMP group and ECMP route
                 * tree in place of the route that is being deleted. */
                err = fmCustomTreeInsert(routeTree, ecmpRoute, ecmpRoute);

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

                err = fmCustomTreeInsert(&ecmpGroup->routeTree,
                                         ecmpRoute,
                                         ecmpRoute);

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

                if (routeLookupTree != NULL)
                {
                    err = fmCustomTreeInsert(routeLookupTree,
                                             ecmpRoute->destIPAddress,
                                             ecmpRoute);
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
                }

                /* Copy the route state into the new base ECMP route */
                ecmpRoute->state  = curRoute->state;
                ecmpRoute->action = curRoute->action;
                ecmpRoute->active = curRoute->active;

                /* Update the switch-specific API */
                if (switchPtr->ReplaceECMPBaseRoute != NULL)
                {
                    err = switchPtr->ReplaceECMPBaseRoute(sw, curRoute, ecmpRoute);
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
                }

                FM_LOG_DEBUG(FM_LOG_CAT_ROUTING,
                             "Replaced ECMP route and ECMP group route, "
                             "old route %p, new route %p\n",
                             (void *) curRoute,
                             (void *) ecmpRoute);
            }

            fmFree(curRoute);

            err = FM_OK;
            goto ABORT;
        }
    }

    /* Remove the entry from the hardware */
    err = switchPtr->DeleteRoute(sw, curRoute);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    /* Remove the route from the ECMP group's route table */
    if (ecmpGroup != NULL)
    {
        err = fmCustomTreeRemove(&ecmpGroup->routeTree, curRoute, NULL);

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    /* Now remove the route from the route tree (and optionally from the
     * ECMP route tree as well). */
    fmCustomTreeRemove(&switchPtr->routeTree, curRoute, NULL);

    if (routeTree != &switchPtr->routeTree)
    {
        fmCustomTreeRemove(routeTree, curRoute, NULL);
    }

    if (routeLookupTree != NULL)
    {
        fmCustomTreeRemove(routeLookupTree, curRoute->destIPAddress, NULL);
    }

    switch (curRoute->route.routeType)
    {
        case FM_ROUTE_TYPE_UNICAST:
            if ( (curRoute->ecmpGroupId >= 0)
                && (curRoute->ecmpGroupId != switchPtr->dropEcmpGroup) )
            {
                err = fmDeleteECMPGroupInternal(sw, curRoute->ecmpGroupId);

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
            }
            break;

        default:
            break;

    }

    if ( (curRoute->mcastGroup != NULL)
        && (curRoute->mcastGroup->routePtrPtr != NULL) )
    {
        *curRoute->mcastGroup->routePtrPtr = NULL;
    }

    fmFree(curRoute);

    err = fmNotifyVNTunnelAboutRouteChange(sw);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);


ABORT:

    if (nextHopList != NULL)
    {
        fmFree(nextHopList);
    }

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, err);

}   /* end fmDeleteRouteInternal */




/*****************************************************************************/
/** fmDeleteRoute
 * \ingroup routerRoute
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Delete a routing entry from the router table.
 * \note            Deletion of multicast routes is not permitted using this
 *                  function.  To delete a multicast route, use the multicast
 *                  group subsystem.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       route points to the route to delete.
 *                  If the route is a unicast route and the nextHop IP address
 *                  is set to 0xffffffff, all routes with the specified
 *                  destination address and prefix length will be deleted.
 *                  If the route is a multicast route and the source IP
 *                  address is set to all one-bits (i.e., 0xffffffff is
 *                  written to every used word of the address), all routes
 *                  with the specified destination address will be deleted.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if route is NULL.
 * \return          FM_ERR_INVALID_ROUTE if the route description is invalid.
 * \return          FM_ERR_NOT_FOUND if the route does not exist.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_USE_MCAST_FUNCTIONS if an attempt was made to
 *                  delete a multicast route.
 *
 *****************************************************************************/
fm_status fmDeleteRoute(fm_int         sw,
                        fm_routeEntry *route)
{
    fm_switch *switchPtr;
    fm_status  err;

    FM_LOG_ENTRY(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (route == NULL)
    {
        err = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    if ( (switchPtr->DeleteRoute != NULL)
        && (switchPtr->maxRoutes > 0) )
    {
        if ( !fmIsRouteEntryUnicast(route) )
        {
            err = FM_ERR_USE_MCAST_FUNCTIONS;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
        }

        /* Get exclusive access to routing tables */
        err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        err = fmDeleteRouteInternal(sw, route);

        fmReleaseWriteLock(&switchPtr->routingLock);
    }
    else
    {
        err = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

ABORT:

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmDeleteRoute */




/*****************************************************************************/
/** fmReplaceRouteECMP
 * \ingroup routerRoute
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Replaces an existing next-hop in an ECMP route with a new
 *                  next-hop. This function may not be used to replace unrelated
 *                  routes. Both old and new routes must be identical except for
 *                  the next-hop information.
 *
 * \note            This function is intended for use when ''fmAddRoute'' 
 *                  has been called (perhaps repeatedly) for a route type
 *                  of ''FM_ROUTE_TYPE_UNICAST''. When ECMP groups are
 *                  created with ''fmCreateECMPGroup'' and 
 *                  ''fmAddECMPGroupNextHops'', use ''fmReplaceECMPGroupNextHop''
 *                  instead of this function.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       oldRoute points to the route to be replaced.
 *
 * \param[in]       newRoute points to the new route that is to replace
 *                  the old route.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_INVALID_ROUTE if the route description is invalid.
 * \return          FM_ERR_NOT_FOUND if the old route cannot be found in the
 *                  routing table.
 * \return          FM_ERR_ALREADY_EXISTS if the new route already exists in
 *                  the routing table.
 *
 *****************************************************************************/
fm_status fmReplaceRouteECMP(fm_int         sw,
                             fm_routeEntry *oldRoute,
                             fm_routeEntry *newRoute)
{
    fm_switch *       switchPtr = NULL;
    fm_bool           routeLockTaken = FALSE;
    fm_status         err;
    fm_intRouteEntry *curRoute;
    fm_intRouteEntry  key;
    fm_ecmpNextHop    oldNextHop;
    fm_ecmpNextHop    newNextHop;

    FM_LOG_ENTRY( FM_LOG_CAT_ROUTING,
                  "sw = %d, oldRoute = %p, newRoute = %p\n",
                  sw,
                  (void *) oldRoute,
                  (void *) newRoute );

    VALIDATE_AND_PROTECT_SWITCH(sw);

    if ( (oldRoute == NULL) || (newRoute == NULL) )
    {
        err = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    switchPtr = GET_SWITCH_PTR(sw);

    if ( (switchPtr->ReplaceECMPGroupNextHop == NULL)
        || (switchPtr->maxRoutes <= 0) )
    {
        err = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    if ( (oldRoute->routeType != newRoute->routeType)
        || (oldRoute->routeType != FM_ROUTE_TYPE_UNICAST) )
    {
        err = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    /* Apply Masks to route fields to ensure address consistency */
    err = fmApplyMasksToRoute(oldRoute);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    err = fmApplyMasksToRoute(newRoute);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    if ( fmCompareEcmpRoutes(oldRoute, newRoute) != 0)
    {
        err = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    /* gain exclusive access to routing tables */
    err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    routeLockTaken = TRUE;

    /* Make sure the new route isn't already in the table */
    key.route = *newRoute;

    err = fmCustomTreeFind( &switchPtr->routeTree,
                            &key,
                            (void **) &curRoute );

    if (err == FM_OK)
    {
        err = FM_ERR_ALREADY_EXISTS;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }
    else if (err != FM_ERR_NOT_FOUND)
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    /* Try to find the old route */
    key.route = *oldRoute;

    err = fmCustomTreeFind( &switchPtr->routeTree,
                            &key,
                            (void **) &curRoute );

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    FM_CLEAR( oldNextHop );
    oldNextHop.type                   = FM_NEXTHOP_TYPE_ARP;
    oldNextHop.data.arp.addr          = oldRoute->data.unicast.nextHop;
    oldNextHop.data.arp.interfaceAddr = oldRoute->data.unicast.interfaceAddr;
    oldNextHop.data.arp.vlan          = oldRoute->data.unicast.vlan;
    oldNextHop.data.arp.trapCode      = FM_TRAPCODE_L3_ROUTED_NO_ARP_0;

    FM_CLEAR( newNextHop );
    newNextHop.type                   = FM_NEXTHOP_TYPE_ARP;
    newNextHop.data.arp.addr          = newRoute->data.unicast.nextHop;
    newNextHop.data.arp.interfaceAddr = newRoute->data.unicast.interfaceAddr;
    newNextHop.data.arp.vlan          = newRoute->data.unicast.vlan;
    newNextHop.data.arp.trapCode      = FM_TRAPCODE_L3_ROUTED_NO_ARP_0;

    /* Remove the old route from the tree */
    err = fmCustomTreeRemoveCertain(&switchPtr->routeTree, curRoute, NULL);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    /* Convert the old route's record into the new route */
    curRoute->route.data.unicast.nextHop       = newNextHop.data.arp.addr;
    curRoute->route.data.unicast.interfaceAddr = newNextHop.data.arp.interfaceAddr;
    curRoute->route.data.unicast.vlan          = newNextHop.data.arp.vlan;

    /* Add the new route back into the tree */
    err = fmCustomTreeInsert(&switchPtr->routeTree, curRoute, curRoute);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    err = fmReplaceECMPGroupNextHopInternal(sw,
                                            curRoute->ecmpGroupId,
                                            &oldNextHop,
                                            &newNextHop);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);


ABORT:

    if (routeLockTaken)
    {
        /* release exclusive access to routing tables */
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmReplaceRouteECMP */




/*****************************************************************************/
/** fmSetRouteState
 * \ingroup routerRoute
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Set the state of a route.
 *
 * \note            Deactivated routes are not removed from the switch.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       route points the route whose state is to be set.
 *
 * \param[in]       state is the desired state for this route.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if route is NULL.
 * \return          FM_ERR_NOT_FOUND if the route does not exist.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 *
 *****************************************************************************/
fm_status fmSetRouteState(fm_int         sw,
                          fm_routeEntry *route,
                          fm_routeState  state)
{
    fm_switch *       switchPtr;
    fm_status         err;
    fm_intRouteEntry *curRoute;
    fm_bool           lockTaken = FALSE;
    fm_customTree *   routeTree;
    fm_intRouteEntry  key;
    fm_intEcmpGroup * ecmpGroup;
    fm_ecmpNextHop    nextHopRecord;
    fm_nextHop *      arpNextHop;
    fm_intNextHop *   nextHop;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, state = %d\n",
                     sw,
                     state);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if ( (switchPtr->SetRouteActive == NULL) ||
        (switchPtr->maxRoutes <= 0) )
    {
        err = FM_ERR_UNSUPPORTED;
        goto ABORT;
    }

    if (route == NULL)
    {
        err = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    /* Apply Masks to route fields to ensure address consistency */
    err = fmApplyMasksToRoute(route);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    if (err != FM_OK)
    {
        goto ABORT;
    }

    lockTaken = TRUE;

    /*
     * Find the route in the route table
     */
    routeTree = GetRouteTree(sw, route);

    if (routeTree == NULL)
    {
        err = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    key.route = *route;

    err = fmCustomTreeFind(routeTree, &key, (void **) &curRoute);

    if (err != FM_OK)
    {
        goto ABORT;
    }

    else if (state == FM_ROUTE_STATE_NEXT_HOP_UP ||
             state == FM_ROUTE_STATE_NEXT_HOP_DOWN)
    {
        if (route->routeType == FM_ROUTE_TYPE_UNICAST)
        {
            if (curRoute->ecmpGroupId >= 0)
            {
                ecmpGroup = switchPtr->ecmpGroups[curRoute->ecmpGroupId];
            }
            else
            {
                ecmpGroup = NULL;
            }
    
            if (ecmpGroup)
            {
                /* Create the Next Hop Record */
    
                FM_CLEAR( nextHop );
            
                nextHopRecord.type        = FM_NEXTHOP_TYPE_ARP;
                arpNextHop                = &nextHopRecord.data.arp;
                arpNextHop->addr          = route->data.unicast.nextHop;
                arpNextHop->interfaceAddr = route->data.unicast.interfaceAddr;
                arpNextHop->vlan          = route->data.unicast.vlan;
                    
                nextHop = FindNextHop(sw, ecmpGroup, &nextHopRecord, NULL);
    
                if (nextHop)
                {
                    if (state == FM_ROUTE_STATE_NEXT_HOP_UP)
                    {
                        nextHop->state = FM_NEXT_HOP_STATE_UP;
                    }
                    else
                    {
                        nextHop->state = FM_NEXT_HOP_STATE_DOWN;
                    }
                    err = UpdateEcmpGroup(sw, ecmpGroup);
                }
                else
                {
                    err = FM_ERR_NOT_FOUND;
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
                }
            }
        }
        else
        {
            err = FM_ERR_UNSUPPORTED;
            goto ABORT;
        }
    }

    /* Found route, update state if needed */
    else if (curRoute->state != state)
    {
        curRoute->state = state;
        err             = fmSetRouteActiveFlag(sw, curRoute, TRUE);
    }


ABORT:

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmSetRouteState */




/*****************************************************************************/
/** fmGetRouteState
 * \ingroup routerRoute
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Retrieve the state of a route.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       route points the route whose state is to be retrieved.
 *
 * \param[out]      state points to caller-allocated storage where this
 *                  function will place the current state of the route.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if route is NULL.
 * \return          FM_ERR_NOT_FOUND if the route does not exist.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 *
 *****************************************************************************/
fm_status fmGetRouteState(fm_int         sw,
                          fm_routeEntry *route,
                          fm_routeState *state)
{
    fm_switch *       switchPtr;
    fm_status         err;
    fm_bool           lockTaken = FALSE;
    fm_intRouteEntry *curRoute;
    fm_customTree *   routeTree;
    fm_intRouteEntry  key;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxRoutes <= 0)
    {
        err = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    if (route == NULL)
    {
        err = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    /* Apply Masks to route fields to ensure address consistency */
    err = fmApplyMasksToRoute(route);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    err = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    lockTaken = TRUE;

    /*
     * Find the route in the route table
     */
    routeTree = GetRouteTree(sw, route);

    if (routeTree == NULL)
    {
        err = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    key.route = *route;

    err = fmCustomTreeFind(routeTree, &key, (void **) &curRoute);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    if (curRoute != NULL)
    {
        /* found the route! */
        *state = curRoute->state;
    }
    else
    {
        /* Route wasn't found, return an error */
        err = FM_ERR_NOT_FOUND;
    }


ABORT:

    if (lockTaken)
    {
        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetRouteState */




/*****************************************************************************/
/** fmSetRouteAction
 * \ingroup routerRoute
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Set the routing action for a route.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       route points the route whose action is to be set.
 *
 * \param[in]       action points to the desired route action for this route.
 *                  If NULL, the default action is FM_ROUTE_ACTION_ROUTE.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if route is NULL.
 * \return          FM_ERR_NOT_FOUND if the route does not exist.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 *
 *****************************************************************************/
fm_status fmSetRouteAction(fm_int         sw,
                          fm_routeEntry * route,
                          fm_routeAction *action)
{
    fm_switch *           switchPtr;
    fm_status             err;
    fm_intRouteEntry *    curRoute;
    fm_bool               lockTaken = FALSE;
    fm_customTree *       routeTree;
    fm_intRouteEntry      key;
    fm_ecmpNextHop        nextHop;
    static fm_routeAction defaultAction =
    {
        .action = FM_ROUTE_ACTION_ROUTE
    };
    fm_routeAction *      routeAction;

    if (action == NULL)
    {
        routeAction = &defaultAction;
    }
    else
    {
        routeAction = action;
    }

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, route = %p, action = %p(%d)\n",
                     sw,
                     (void *) route,
                     (void *) action,
                     routeAction->action);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_CLEAR( nextHop );

    if ( (switchPtr->DeleteRoute == NULL) || (switchPtr->maxRoutes <= 0) )
    {
        err = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    if (route == NULL)
    {
        err = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    /* Apply Masks to route fields to ensure address consistency */
    err = fmApplyMasksToRoute(route);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    lockTaken = TRUE;

    /*
     * Find the route in the route table
     */
    routeTree = GetRouteTree(sw, route);

    if (routeTree == NULL)
    {
        err = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    key.route = *route;

    err = fmCustomTreeFind(routeTree, &key, (void **) &curRoute);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    /* If route wasn't found, return an error */
    if (curRoute == NULL)
    {
        err = FM_ERR_NOT_FOUND;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    /* Found route. */

    /* FIXME: This should really do an in-place change, not a delete and
     * re-add */

     /* Delete the route from the hardware */
    err = switchPtr->DeleteRoute(sw, curRoute);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    /* Replace the action */
    curRoute->action = *routeAction;

    if ( (routeAction->action == FM_ROUTE_ACTION_ROUTE) &&
         (curRoute->ecmpGroupId < 0) )
    {
        switch (curRoute->route.routeType)
        {
            case FM_ROUTE_TYPE_UNICAST:
                /* Create the ECMP group */
                nextHop.type                   = FM_NEXTHOP_TYPE_ARP;
                nextHop.data.arp.addr          = curRoute->route.data.unicast.nextHop;
                nextHop.data.arp.interfaceAddr = curRoute->route.data.unicast.interfaceAddr;
                nextHop.data.arp.vlan          = curRoute->route.data.unicast.vlan;

                err = fmCreateECMPGroupInternal(sw,
                                                &curRoute->ecmpGroupId,
                                                NULL,
                                                NULL);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

                err = fmAddECMPGroupNextHopsInternal(sw,
                                                     curRoute->ecmpGroupId,
                                                     1,
                                                     &nextHop);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
                break;

            case FM_ROUTE_TYPE_UNICAST_ECMP:
                curRoute->ecmpGroupId = curRoute->route.data.unicastECMP.ecmpGroup;
                break;

            case FM_ROUTE_TYPE_MULTICAST:
                err = fmCreateECMPGroupInternal(sw,
                                                &curRoute->ecmpGroupId,
                                                NULL,
                                                curRoute->mcastGroup);

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
                break;

            default:
                break;
        }
    }
    else if (routeAction->action == FM_ROUTE_ACTION_DROP)
    {
        curRoute->ecmpGroupId = switchPtr->dropEcmpGroup;
    }
    else if ( (routeAction->action == FM_ROUTE_ACTION_RPF_FAILURE)
        || (routeAction->action == FM_ROUTE_ACTION_NOP) )
    {
        if ( (curRoute->ecmpGroupId >= 0)
            && (curRoute->ecmpGroupId != switchPtr->dropEcmpGroup) )
        {
            switch (curRoute->route.routeType)
            {
                case FM_ROUTE_TYPE_UNICAST:
                case FM_ROUTE_TYPE_MULTICAST:
                    err = fmDeleteECMPGroupInternal(sw, curRoute->ecmpGroupId);

                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

                    curRoute->ecmpGroupId = -1;
                    break;

                case FM_ROUTE_TYPE_UNICAST_ECMP:
                    break;

                default:
                    break;
            }
        }
    }

    /* Add the route back into the hardware */
    err = switchPtr->AddRoute(sw, curRoute);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);


ABORT:

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmSetRouteAction */




/*****************************************************************************/
/** fmGetRouteAction
 * \ingroup routerRoute
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Set the routing action for a route.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       route points the route whose action is to be set.
 *
 * \param[out]      action points to caller-allocated memory into which the
 *                  route's action will be placed.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_NOT_FOUND if the route does not exist.
 * \return          FM_ERR_INVALID_ARGUMENT if route or action are NULL.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the 
 *                  switch.
 *
 *****************************************************************************/
fm_status fmGetRouteAction(fm_int         sw,
                          fm_routeEntry * route,
                          fm_routeAction *action)
{
    fm_switch *           switchPtr;
    fm_status             err;
    fm_intRouteEntry *    curRoute;
    fm_bool               lockTaken = FALSE;
    fm_intRouteEntry      key;
    fm_customTree *       routeTree;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, route = %p, action = %p\n",
                     sw,
                     (void *) route,
                     (void *) action);

    if ( (route == NULL) || (action == NULL) )
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    /* Apply Masks to route fields to ensure address consistency */
    err = fmApplyMasksToRoute(route);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    err = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    lockTaken = TRUE;

    /* Try to find the route */
    key.route = *route;

    routeTree = GetRouteTree(sw, route);

    if (routeTree == NULL)
    {
        err = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    err = fmCustomTreeFind(routeTree, &key, (void **) &curRoute);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    /* If route wasn't found, return an error */
    if (curRoute == NULL)
    {
        err = FM_ERR_NOT_FOUND;
    }

    /* Found route, update action */
    else
    {
        *action = curRoute->action;
        err     = FM_OK;
    }


ABORT:

    if (lockTaken)
    {
        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetRouteAction */




/*****************************************************************************/
/** fmGetRouteList
 * \ingroup routerRoute
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Return a list of all routes on a switch.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      numRoutes points to caller allocated storage where
 *                  this function should place the number of routes returned
 *                  in routeList.
 *
 * \param[out]      routeList is an array that this function will fill
 *                  with the list of routes.
 *
 * \param[in]       max is the size of routeList, being the maximum
 *                  number of routes that routeList can hold.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_BUFFER_FULL if max was too small to accommodate
 *                  the entire list of routes.
 *
 *****************************************************************************/
fm_status fmGetRouteList(fm_int         sw,
                         fm_int *       numRoutes,
                         fm_routeEntry *routeList,
                         fm_int         max)
{
    fm_switch *           switchPtr;
    fm_status             err;
    fm_intRouteEntry *    route;
    fm_int                curRoute = 0;
    fm_customTreeIterator iter;
    fm_intRouteEntry *    key;
    fm_bool               lockTaken = FALSE;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, numRoutes = %p, routeList = %p, max = %d\n",
                     sw,
                     (void *) numRoutes,
                     (void *) routeList,
                     max);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxRoutes <= 0)
    {
        err = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    err = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    lockTaken = TRUE;

    fmCustomTreeIterInit(&iter, &switchPtr->routeTree);

    while (1)
    {
        err = fmCustomTreeIterNext( &iter, (void **) &key, (void **) &route );

        if (err == FM_ERR_NO_MORE)
        {
            err = FM_OK;
            break;
        }

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        if (curRoute >= max)
        {
            err = FM_ERR_BUFFER_FULL;
            break;
        }

        routeList[curRoute] = route->route;
        curRoute++;
    }


ABORT:

    if (lockTaken)
    {
        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    *numRoutes = curRoute;

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetRouteList */




/*****************************************************************************/
/** fmGetRouteFirst
 * \ingroup routerRoute
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Retrieve the first route. The routes are sorted by VRID
 *                  and longest prefix match.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      searchToken points to caller-allocated storage of type
 *                  fm_voidptr, where this function will store a token
 *                  to be used in a subsequent call to ''fmGetRouteNext''.
 *
 * \param[out]      firstRoute points to caller-allocated storage where this
 *                  function will store the first route.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_NO_MORE if there are no routes in this switch.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 *
 *****************************************************************************/
fm_status fmGetRouteFirst(fm_int         sw,
                          fm_voidptr *   searchToken,
                          fm_routeEntry *firstRoute)
{
    fm_switch *           switchPtr;
    fm_status             err;
    fm_intRouteEntry *    firstRouteCandidate;
    fm_bool               lockTaken = FALSE;
    fm_customTreeIterator iter;
    fm_intRouteEntry *    key;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxRoutes <= 0)
    {
        err = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    err = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    lockTaken = TRUE;

    fmCustomTreeIterInit(&iter, &switchPtr->routeTree);

    err = fmCustomTreeIterNext( &iter,
                               (void **) &key,
                               (void **) &firstRouteCandidate );

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    if (firstRouteCandidate != NULL)
    {
        *firstRoute  = firstRouteCandidate->route;
        *searchToken = (fm_voidptr) firstRouteCandidate;
    }
    else
    {
        err = FM_ERR_NO_MORE;
    }


ABORT:

    if (lockTaken)
    {
        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetRouteFirst */




/*****************************************************************************/
/** fmGetRouteNext
 * \ingroup routerRoute
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Retrieve the next route. The routes are sorted by VRID
 *                  and longest prefix match.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in,out]   searchToken points to caller-allocated storage of type
 *                  fm_voidptr that has been filled in by a prior call to
 *                  this function or to ''fmGetRouteFirst''. It will be updated
 *                  by this function with a new value to be used in a
 *                  subsequent call to this function.
 *
 * \param[out]      nextRoute points to caller-allocated storage where this
 *                  function will store the next route.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_NOT_FOUND if currentRoute is invalid.
 * \return          FM_ERR_NO_MORE if there are no more routes in the switch.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 *
 *****************************************************************************/
fm_status fmGetRouteNext(fm_int         sw,
                         fm_voidptr *   searchToken,
                         fm_routeEntry *nextRoute)
{
    fm_switch *           switchPtr;
    fm_status             err;
    fm_intRouteEntry *    workingRoute;
    fm_bool               lockTaken = FALSE;
    fm_customTreeIterator iter;
    fm_intRouteEntry *    key;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxRoutes <= 0)
    {
        err = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    err = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    lockTaken = TRUE;

    workingRoute = (fm_intRouteEntry *) *searchToken;

    err = fmCustomTreeIterInitFromKey(&iter,
                                      &switchPtr->routeTree,
                                      workingRoute);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    /* get the previous route */
    err = fmCustomTreeIterNext( &iter,
                               (void **) &key,
                               (void **) &workingRoute);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    /* get the next route */
    err = fmCustomTreeIterNext( &iter,
                               (void **) &key,
                               (void **) &workingRoute);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    if (workingRoute != NULL)
    {
        *nextRoute   = workingRoute->route;
        *searchToken = (fm_voidptr) workingRoute;
    }
    else
    {
        err = FM_ERR_NO_MORE;
    }


ABORT:

    if (lockTaken)
    {
        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetRouteNext */




/*****************************************************************************/
/** fmAddARPEntry
 * \ingroup routerArp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Add an ARP table entry in the switch. This function does
 *                  not allow a new entry to be added if an identical entry
 *                  (not including destiation MAC address) already exists.
 *                  Use ''fmUpdateARPEntryDMAC'' to change the destination
 *                  MAC address of an existing entry. It is not permitted
 *                  to add an ARP entry to an interface that does not exist.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       arp points the ARP entry to add.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_DUPLICATE_ARP_ENTRY if the specified ARP entry
 *                  already exists.
 * \return          FM_ERR_INVALID_INTERFACE if the interface specified in the
 *                  ARP entry does not exist.
 * \return          FM_ERR_ARP_TABLE_FULL if routing is not available on the switch.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 *
 *****************************************************************************/
fm_status fmAddARPEntry(fm_int sw, fm_arpEntry *arp)
{
    fm_switch *             switchPtr;
    fm_status               err;
    fm_int                  i;
    fm_intArpEntry *        arpEntry;
    fm_bool                 arpEntryAllocated = FALSE;
    fm_bool                 lockTaken = FALSE;
    fm_char                 ipText[100];
    fm_intIpInterfaceEntry *ifEntry;
    fm_intArpEntry *        nextArpKey;
    fm_intArpEntry *        nextArp;
    fm_intArpEntry          searchArp;
    fm_customTreeIterator   iter;
    fm_intNextHop *         hopKey;
    fm_intNextHop *         nextHop;
    fm_customTree           nextHopTree;
    fm_nextHop *            arpNextHop;

    fmDbgConvertIPAddressToString(&arp->ipAddr, ipText);
    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, ip=%s, vlan=%d, interface=%d, mac=%012llX\n",
                     sw,
                     ipText,
                     arp->vlan,
                     arp->interface,
                     arp->macAddr);

    fmCustomTreeInit(&nextHopTree, fmCompareInternalNextHops);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxArpEntries <= 0)
    {
        err = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    lockTaken = TRUE;

    /* see if this entry already exists, report error if it does */
    searchArp.arp.ipAddr = arp->ipAddr;

    if (arp->interface >= 0)
    {
        err = fmGetInterface(sw, arp->interface, &ifEntry);

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        searchArp.arp.vlan = ifEntry->vlan;
    }
    else
    {
        ifEntry            = NULL;
        searchArp.arp.vlan = arp->vlan;
    }

    err = fmCustomTreeFind(&switchPtr->arpTree,
                           (void *) &searchArp,
                           (void **) &arpEntry);

    if (err == FM_OK)
    {
        err = FM_ERR_DUPLICATE_ARP_ENTRY;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    i = sizeof(fm_intArpEntry);

    arpEntry = (fm_intArpEntry *) fmAlloc(i);

    if (arpEntry == NULL)
    {
        err = FM_ERR_NO_MEM;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    FM_MEMSET_S(arpEntry, i, 0, i);

    arpEntryAllocated = TRUE;

    arpEntry->arp       = *arp;
    arpEntry->ifEntry   = ifEntry;
    arpEntry->switchPtr = switchPtr;
    arpEntry->vrid      = -1;
    fmCustomTreeInit(&arpEntry->nextHopTree, fmCompareInternalNextHops);
    FM_DLL_INIT_NODE(arpEntry, nextArp, prevArp);

    /* Store the active vlan */
    arpEntry->arp.vlan = searchArp.arp.vlan;

    /* Add the arp entry to the arp entry tree */
    err = fmCustomTreeInsert(&switchPtr->arpTree, arpEntry, arpEntry);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    /* Find its successor arp entry, if any */
    err = fmCustomTreeSuccessor(&switchPtr->arpTree,
                                arpEntry,
                                (void **) &nextArpKey,
                                (void **) &nextArp);

    if (err != FM_OK)
    {
        if (err == FM_ERR_NO_MORE)
        {
            nextArp = NULL;
        }
        else
        {
            fmCustomTreeRemove(&switchPtr->arpTree, arpEntry, NULL);
            fmFree(arpEntry);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
        }
    }

    /* new entry goes before the current entry */
    fmInsertArpBefore(switchPtr, nextArp, arpEntry);

    /* Update all next-hops that have been waiting for this ARP entry */
    fmCustomTreeIterInit(&iter, &switchPtr->noArpNextHops);

    while (1)
    {
        err = fmCustomTreeIterNext( &iter, 
                                   (void **) &hopKey,
                                   (void **) &nextHop );

        if (err != FM_OK)
        {
            if (err == FM_ERR_NO_MORE)
            {
                err = FM_OK;
                break;
            }

            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
        }

        if (nextHop->nextHop.type != FM_NEXTHOP_TYPE_ARP)
        {
            continue;
        }

        arpNextHop = &nextHop->nextHop.data.arp;

        if ( (fmCompareIPAddresses(&arp->ipAddr, &arpNextHop->addr) == 0)
             && (arpEntry->arp.vlan == nextHop->vlan ) )
        {
            /* Found a waiting next hop.  We can't remove it from the
             * noArpNextHops tree at this time without breaking our
             * iterator, so save it for now.
             */
            err = fmCustomTreeInsert(&nextHopTree, nextHop, nextHop);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

            nextHop->arp = arpEntry;

            FM_API_CALL_FAMILY(err,
                               switchPtr->UpdateNextHop,
                               sw,
                               nextHop);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

            /* Add the next-hop to this ARP record's nexthop list */
            err = fmCustomTreeInsert(&arpEntry->nextHopTree,
                                     nextHop,
                                     nextHop);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

            FM_LOG_DEBUG( FM_LOG_CAT_ROUTING,
                          "next hop %p added to arp %p next-hop tree\n",
                          (void *) nextHop,
                          (void *) arpEntry );
        }
    }

    /* Remove processed next-hops from the noArpNextHops tree */
    fmCustomTreeIterInit(&iter, &nextHopTree);

    while (1)
    {
        err = fmCustomTreeIterNext( &iter,
                                   (void **) &hopKey,
                                   (void **) &nextHop );

        if (err == FM_ERR_NO_MORE)
        {
            err = FM_OK;
            break;
        }

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        err = fmCustomTreeRemove(&switchPtr->noArpNextHops, nextHop, NULL);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        FM_LOG_DEBUG( FM_LOG_CAT_ROUTING,
                      "next hop %p removed from noArpNextHops tree\n",
                      (void *) nextHop );
    }

    /* If the switch requires extra processing, call the switch-specific
     * function now. */
    if (switchPtr->AddArpEntry != NULL)
    {
        err = switchPtr->AddArpEntry(sw, arp);
    }


ABORT:

    /* Kill the temporary tree, which will also empty it */
    fmCustomTreeDestroy(&nextHopTree, NULL);

    if (err != FM_OK)
    {
        if (arpEntryAllocated)
        {
            fmCustomTreeRemove(&switchPtr->arpTree, arpEntry, NULL);
            fmCustomTreeDestroy(&arpEntry->nextHopTree, NULL);
            fmFree(arpEntry);
        }
    }

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmAddARPEntry */




/*****************************************************************************/
/** fmDeleteARPEntry
 * \ingroup routerArp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Delete an ARP table entry from the switch.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       arp points the ARP entry to delete.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_NOT_FOUND if the ARP entry does not exist.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 *
 *****************************************************************************/
fm_status fmDeleteARPEntry(fm_int sw, fm_arpEntry *arp)
{
    fm_switch *             switchPtr;
    fm_status               err;
    fm_intArpEntry *        curArp;
    fm_bool                 lockTaken = FALSE;
    fm_intArpEntry          searchArp;
    fm_intIpInterfaceEntry *ifEntry;
    fm_char                 ipText[100];
    fm_customTreeIterator   iter;
    fm_intNextHop *         hopKey;
    fm_intNextHop *         nextHop;

    fmDbgConvertIPAddressToString(&arp->ipAddr, ipText);
    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, ip=%s, vlan=%d, interface=%d\n",
                     sw,
                     ipText,
                     arp->vlan,
                     arp->interface);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxArpEntries <= 0)
    {
        err = FM_ERR_UNSUPPORTED;
        goto ABORT;
    }

    err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    if (err != FM_OK)
    {
        goto ABORT;
    }

    lockTaken = TRUE;

    searchArp.arp.ipAddr = arp->ipAddr;

    /* get the vlan */
    if (arp->interface >= 0)
    {
        err = fmGetInterface(sw, arp->interface, &ifEntry);

        if (err != FM_OK)
        {
            goto ABORT;
        }

        searchArp.arp.vlan = ifEntry->vlan;
    }
    else
    {
        searchArp.arp.vlan = arp->vlan;
    }

    /* try to find the entry in the table */
    err = fmCustomTreeFind(&switchPtr->arpTree,
                           (void *) &searchArp,
                           (void **) &curArp);

    if (err != FM_OK)
    {
        goto ABORT;
    }

#if 0
    fmDbgConvertIPAddressToString(&curArp->arp.ipAddr, ipText);
    FM_LOG_PRINT(
        FM_LOG_CAT_ROUTING,
        FM_LOG_LEVEL_DEBUG,
        "Removing arp entry %p, ip=%s, vlan=%d, interface=%d, mac=%012llX\n",
        (void *)curArp,
        ipText,
        curArp->arp.vlan,
        curArp->arp.interface,
        curArp->arp.macAddr);
#endif

    /* remove the entry from the table */
    fmRemoveArp(switchPtr, curArp);
    err = fmCustomTreeRemove(&switchPtr->arpTree, curArp, NULL);

    if (err != FM_OK)
    {
        goto ABORT;
    }

    /* Find and update all affected next-hops */
    fmCustomTreeIterInit(&iter, &curArp->nextHopTree);

    while (1)
    {
        err = fmCustomTreeIterNext( &iter,
                                   (void **) &hopKey,
                                   (void **) &nextHop);

        if (err != FM_OK)
        {
            if (err != FM_ERR_NO_MORE)
            {
                goto ABORT;
            }

            err = FM_OK;
            break;
        }

        nextHop->arp = NULL;

        err = fmCustomTreeInsert(&switchPtr->noArpNextHops, nextHop, nextHop);

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
        FM_LOG_DEBUG( FM_LOG_CAT_ROUTING,
                      "next hop %p added to noArpNextHops tree\n",
                      (void *) nextHop );

        FM_API_CALL_FAMILY(err,
                           switchPtr->UpdateNextHop,
                           sw,
                           nextHop);

        if (err != FM_OK)
        {
            goto ABORT;
        }
    }

    fmCustomTreeDestroy(&curArp->nextHopTree, NULL);

    fmFree(curArp);

    /* If the switch requires extra processing, call the switch-specific
     * function now. */
    if (switchPtr->DeleteArpEntry != NULL)
    {
        err = switchPtr->DeleteArpEntry(sw, arp);
    }


ABORT:

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmDeleteARPEntry */




/*****************************************************************************/
/** fmUpdateARPEntryDMAC
 * \ingroup routerArp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Write a destination MAC address to an existing ARP entry.
 *                  this function is typically called by the application
 *                  when the actual MAC address has been learned for an IP
 *                  address that had been previously routed to the CPU.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       arp points the ARP entry to modify. All fields of the
 *                  ''fm_arpEntry'' structure except the macAddr field will
 *                  be used to identify the existing ARP entry.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_NOT_FOUND if the ARP entry does not exist.
 *
 *****************************************************************************/
fm_status fmUpdateARPEntryDMAC(fm_int sw, fm_arpEntry *arp)
{
    fm_switch *             switchPtr;
    fm_status               err;
    fm_intArpEntry          searchArp;
    fm_intArpEntry *        curArp;
    fm_bool                 lockTaken = FALSE;
    fm_intIpInterfaceEntry *ifEntry;
    fm_customTreeIterator   iter;
    fm_intNextHop *         hopKey;
    fm_intNextHop *         nextHop;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxArpEntries <= 0)
    {
        err = FM_ERR_UNSUPPORTED;
        goto ABORT;
    }

    err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    if (err != FM_OK)
    {
        goto ABORT;
    }

    lockTaken = TRUE;

    searchArp.arp.ipAddr = arp->ipAddr;

    /* get the vlan */
    if (arp->interface >= 0)
    {
        err = fmGetInterface(sw, arp->interface, &ifEntry);

        if (err != FM_OK)
        {
            FM_LOG_WARNING(FM_LOG_CAT_ROUTING,
                           "fmUpdateARPEntryDMAc fmGetInterface "
                           "returned error %d\n",
                           err);
            goto ABORT;
        }

        searchArp.arp.vlan = ifEntry->vlan;
    }
    else
    {
        searchArp.arp.vlan = arp->vlan;
    }

#if 0
    {
        fm_byte ipa1[100];
        fmDbgConvertIPAddressToString(&searchArp.arp.ipAddr, ipa1);
        FM_LOG_PRINT(
            "fmUpdateARPEntryDMAC searching for ip addr %s, vlan=%d\n",
            ipa1, searchArp.arp.vlan);
    }
#endif

    /* try to find the entry in the table */
    err = fmCustomTreeFind(&switchPtr->arpTree,
                           (void *) &searchArp,
                           (void **) &curArp);

    if (err != FM_OK)
    {
        FM_LOG_WARNING(FM_LOG_CAT_ROUTING,
                       "fmCustomTreeFind returned error %d\n",
                       err);
        goto ABORT;
    }

    /* update the destination mac address in the table entry */
    FM_MEMCPY_S( &curArp->arp.macAddr,
                 sizeof(curArp->arp.macAddr),
                 &arp->macAddr,
                 sizeof(arp->macAddr) );

    /* Find and update all affected next-hops */
    fmCustomTreeIterInit(&iter, &curArp->nextHopTree);

    while (1)
    {
        err = fmCustomTreeIterNext( &iter,
                                   (void **) &hopKey,
                                   (void **) &nextHop);

        if (err != FM_OK)
        {
            if (err != FM_ERR_NO_MORE)
            {
                goto ABORT;
            }

            err = FM_OK;
            break;
        }

        FM_API_CALL_FAMILY(err,
                           switchPtr->UpdateNextHop,
                           sw,
                           nextHop);

        if (err != FM_OK)
        {
            goto ABORT;
        }
    }

    /* If the switch requires extra processing, call the switch-specific
     * function now. */
    if (switchPtr->UpdateArpEntryDestMac != NULL)
    {
        err = switchPtr->UpdateArpEntryDestMac(sw, arp);
    }


ABORT:

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmUpdateARPEntryDMAC */




/*****************************************************************************/
/** fmUpdateARPEntryVrid
 * \ingroup routerArp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Update the egress virtual router ID associated with an 
 *                  existing ARP entry. This function is typically called by 
 *                  the application after calling ''fmAddARPEntry'' to modify 
 *                  the egress VRID.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       arp points the ARP entry to modify. All fields of the
 *                  ''fm_arpEntry'' structure will be used to identify the
 *                  existing ARP entry.
 * 
 * \param[in]       vrid is the new value to assign for the egress VRID.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_NOT_FOUND if the ARP entry does not exist.
 *
 *****************************************************************************/
fm_status fmUpdateARPEntryVrid(fm_int       sw,
                               fm_arpEntry *arp,
                               fm_int       vrid)
{
    fm_switch *             switchPtr;
    fm_status               err;
    fm_intArpEntry          searchArp;
    fm_intArpEntry *        curArp;
    fm_bool                 lockTaken = FALSE;
    fm_intIpInterfaceEntry *ifEntry;
    fm_customTreeIterator   iter;
    fm_intNextHop *         hopKey;
    fm_intNextHop *         nextHop;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxArpEntries <= 0)
    {
        err = FM_ERR_UNSUPPORTED;
        goto ABORT;
    }

    err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    if (err != FM_OK)
    {
        goto ABORT;
    }

    lockTaken = TRUE;

    searchArp.arp.ipAddr = arp->ipAddr;

    /* get the vlan */
    if (arp->interface >= 0)
    {
        err = fmGetInterface(sw, arp->interface, &ifEntry);

        if (err != FM_OK)
        {
            FM_LOG_WARNING(FM_LOG_CAT_ROUTING,
                           "fmUpdateARPEntryDMAc fmGetInterface "
                           "returned error %d\n",
                           err);
            goto ABORT;
        }

        searchArp.arp.vlan = ifEntry->vlan;
    }
    else
    {
        searchArp.arp.vlan = arp->vlan;
    }

    /* try to find the entry in the table */
    err = fmCustomTreeFind(&switchPtr->arpTree,
                           (void *) &searchArp,
                           (void **) &curArp);

    if (err != FM_OK)
    {
        FM_LOG_WARNING(FM_LOG_CAT_ROUTING,
                       "fmCustomTreeFind returned error %d\n",
                       err);
        goto ABORT;
    }

    /* update the egress vrid in the table entry */
    curArp->vrid = vrid;

    /* Find and update all affected next-hops */
    fmCustomTreeIterInit(&iter, &curArp->nextHopTree);

    while (1)
    {
        err = fmCustomTreeIterNext( &iter,
                                   (void **) &hopKey,
                                   (void **) &nextHop);

        if (err != FM_OK)
        {
            if (err != FM_ERR_NO_MORE)
            {
                goto ABORT;
            }

            err = FM_OK;
            break;
        }

        FM_API_CALL_FAMILY(err,
                           switchPtr->UpdateNextHop,
                           sw,
                           nextHop);

        if (err != FM_OK)
        {
            goto ABORT;
        }
    }

    /* If the switch requires extra processing, call the switch-specific
     * function now. */
    if (switchPtr->UpdateArpEntryVrid != NULL)
    {
        err = switchPtr->UpdateArpEntryVrid(sw, arp, vrid);
    }


ABORT:

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmUpdateARPEntryVrid */




/*****************************************************************************/
/** fmGetARPEntryList
 * \ingroup routerArp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Return a list of all ARP entries on a switch.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      numArps points to caller allocated storage where
 *                  this function should place the number of ARP entries
 *                  returned in arpList.
 *
 * \param[out]      arpList is an array that this function will fill
 *                  with the list of ARP entries.
 *
 * \param[in]       max is the size of arpList, being the maximum
 *                  number of ARP entries that arpList can hold.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_BUFFER_FULL if max was too small to accommodate
 *                  the entire list of ARP entries.
 *
 *****************************************************************************/
fm_status fmGetARPEntryList(fm_int       sw,
                            fm_int *     numArps,
                            fm_arpEntry *arpList,
                            fm_int       max)
{
    fm_switch *     switchPtr;
    fm_status       err;
    fm_int          arpCount = 0;
    fm_intArpEntry *arp;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, numArps = %p, arpList = %p, max = %d\n",
                     sw,
                     (void *) numArps,
                     (void *) arpList,
                     max);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxArpEntries > 0)
    {
        err = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

        if (err == FM_OK)
        {
            arp = fmGetFirstArp(switchPtr);

            while (arp != NULL)
            {
                if (arpCount >= max)
                {
                    err = FM_ERR_BUFFER_FULL;
                    break;
                }

                arpList[arpCount] = arp->arp;
                arpCount++;
                arp = fmGetNextArp(arp);
            }

            if (err == FM_ERR_NO_MORE)
            {
                err = FM_OK;
            }

            fmReleaseReadLock(&switchPtr->routingLock);
        }
    }
    else
    {
        err = FM_ERR_UNSUPPORTED;
    }

    UNPROTECT_SWITCH(sw);

    *numArps = arpCount;

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetARPEntryList */




/*****************************************************************************/
/** fmGetARPEntryFirst
 * \ingroup routerArp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Retrieve the first ARP entry. The ARP entries are returned
 *                  in order of VRID, then VLAN and then IP address.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      searchToken points to caller-allocated storage of type
 *                  fm_voidptr, where this function will store a token
 *                  to be used in a subsequent call to ''fmGetARPEntryNext''.
 *
 * \param[out]      firstArp points to caller-allocated storage where this
 *                  function will store the first ARP entry.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_NO_MORE if there are no arp entries.
 *
 *****************************************************************************/
fm_status fmGetARPEntryFirst(fm_int       sw,
                             fm_voidptr * searchToken,
                             fm_arpEntry *firstArp)
{
    fm_switch *     switchPtr;
    fm_status       err;
    fm_intArpEntry *curArp;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxArpEntries > 0)
    {
        err = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

        if (err == FM_OK)
        {
            curArp = fmGetFirstArp(switchPtr);

            if (curArp == NULL)
            {
                err = FM_ERR_NO_MORE;
            }
            else
            {
                *firstArp    = curArp->arp;
                *searchToken = (fm_voidptr) curArp;
            }

            fmReleaseReadLock(&switchPtr->routingLock);
        }
    }
    else
    {
        err = FM_ERR_UNSUPPORTED;
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetARPEntryFirst */




/*****************************************************************************/
/** fmGetARPEntryNext
 * \ingroup routerArp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Retrieve the next ARP entry. The ARP entries are returned
 *                  in order of VRID, then VLAN and then IP address.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in,out]   searchToken points to caller-allocated storage of type
 *                  fm_voidptr that has been filled in by a prior call to
 *                  this function or to ''fmGetARPEntryFirst''. It will be
 *                  updated by this function with a new value to be used in a
 *                  subsequent call to this function.
 *
 * \param[out]      nextArp points to caller-allocated storage where this
 *                  function will store the next ARP entry.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_NO_MORE if there are no more ARP entries.
 *
 *****************************************************************************/
fm_status fmGetARPEntryNext(fm_int       sw,
                            fm_voidptr * searchToken,
                            fm_arpEntry *nextArp)
{
    fm_switch *     switchPtr;
    fm_status       err;
    fm_intArpEntry *workingArp;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxArpEntries > 0)
    {
        err = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

        if (err == FM_OK)
        {
            workingArp = (fm_intArpEntry *) *searchToken;
            workingArp = fmGetNextArp(workingArp);

            if (workingArp == NULL)
            {
                err = FM_ERR_NO_MORE;
            }
            else
            {
                *nextArp     = workingArp->arp;
                *searchToken = (void *) workingArp;
            }

            fmReleaseReadLock(&switchPtr->routingLock);
        }
    }
    else
    {
        err = FM_ERR_UNSUPPORTED;
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetARPEntryNext */




/*****************************************************************************/
/** fmGetARPEntryInfo
 * \ingroup routerArp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Returns information about an ARP table entry.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       arpEntry points to the arp entry to be accessed.
 *
 * \param[out]      arpInfo points to a ''fm_arpEntryInfo'' structure into
 *                  which this function will place the results.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 *
 *****************************************************************************/
fm_status fmGetARPEntryInfo(fm_int           sw,
                            fm_arpEntry *    arpEntry,
                            fm_arpEntryInfo *arpInfo)
{
    fm_switch *     switchPtr;
    fm_status       err;
    fm_intArpEntry *foundArp;
    fm_bool         lockTaken = FALSE;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxArpEntries <= 0)
    {
        err = FM_ERR_UNSUPPORTED;
        goto ABORT;
    }

    err = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    if (err != FM_OK)
    {
        goto ABORT;
    }

    lockTaken = TRUE;

    err = FindArpEntry(sw, arpEntry, &foundArp, FALSE);

    if (err != FM_OK)
    {
        goto ABORT;
    }

    /* Fill in the ARP table entry structure. */
    arpInfo->arp = foundArp->arp;

    /* Get the hardware "used" flag. */
    err = fmGetARPEntryUsedInternal(sw, arpEntry, &arpInfo->used, FALSE);

    if (err != FM_OK)
    {
        FM_LOG_WARNING(FM_LOG_CAT_ROUTING,
                       "fmGetARPEntryUsedInternal returned error %d\n",
                       err);
    }

ABORT:

    if (lockTaken)
    {
        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetARPEntryInfo */




/*****************************************************************************/
/** fmGetARPEntryUsed
 * \ingroup routerArp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Retrieves the "used" flag for an ARP entry.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       arp points to the arp entry to be accessed.
 *
 * \param[out]      used points to caller-allocated storage where this function
 *                  should place the result.
 *
 * \param[in]       resetFlag specifies whether the flag should be reset
 *                  after it is read.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_NOT_FOUND if the specified ARP entry is not
 *                  recognized.
 *
 *****************************************************************************/
fm_status fmGetARPEntryUsed(fm_int       sw,
                            fm_arpEntry *arp,
                            fm_bool*     used,
                            fm_bool      resetFlag)
{
    fm_status  err;
    fm_switch *switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    err = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    if (err == FM_OK)
    {
        err = fmGetARPEntryUsedInternal(sw, arp, used, resetFlag);

        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetARPEntryUsed */




/*****************************************************************************/
/** fmGetARPEntryUsedInternal
 * \ingroup intRouterArp
 *
 * \desc            Retrieves the "used" flag for an ARP entry.
 *
 * \note            This function assumes the routing lock has been taken.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       arp points to the arp entry to be accessed.
 *
 * \param[out]      used points to caller-allocated storage where this function
 *                  should place the result.
 *
 * \param[in]       resetFlag specifies whether the flag should be reset
 *                  after it is read.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_NOT_FOUND if the specified ARP entry is not
 *                  recognized.
 *
 *****************************************************************************/
fm_status fmGetARPEntryUsedInternal(fm_int       sw,
                                    fm_arpEntry *arp,
                                    fm_bool *    used,
                                    fm_bool      resetFlag)
{
    fm_switch *           switchPtr;
    fm_status             err;
    fm_intArpEntry *      foundArp;
    fm_customTreeIterator iter;
    fm_intNextHop *       nextHopKey;
    fm_intNextHop *       nextHop;
    fm_bool               nextHopUsed;

    FM_LOG_ENTRY( FM_LOG_CAT_ROUTING,
                 "sw = %d, arp = %p, used = %p, resetFlag = %d\n",
                 sw,
                 (void *) arp,
                 (void *) used,
                 resetFlag);

    switchPtr = GET_SWITCH_PTR(sw);

    *used = FALSE;

    if (switchPtr->maxArpEntries <= 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_UNSUPPORTED);
    }

    err = FindArpEntry(sw, arp, &foundArp, FALSE);

    if (err != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, err);
    }

    /* Check every next-hop that uses this ARP to see if it was used.  If
     * resetFlag is FALSE, we can stop as soon as we find one that is used,
     * because we only return TRUE/FALSE. If resetFlag is TRUE, we have
     * to check every next-hop. */
    fmCustomTreeIterInit(&iter, &foundArp->nextHopTree);

    while (1)
    {
        err = fmCustomTreeIterNext( &iter,
                                   (void **) &nextHopKey,
                                   (void **) &nextHop );

        if (err != FM_OK)
        {
            if (err != FM_ERR_NO_MORE)
            {
                FM_LOG_EXIT(FM_LOG_CAT_ROUTING, err);
            }

            err = FM_OK;
            break;
        }

        FM_API_CALL_FAMILY(err,
                           switchPtr->GetNextHopUsed,
                           sw,
                           nextHop,
                           &nextHopUsed,
                           resetFlag);

        if (err != FM_OK)
        {
            FM_LOG_EXIT(FM_LOG_CAT_ROUTING, err);
        }

        if (nextHopUsed)
        {
            *used = TRUE;

            if (!resetFlag)
            {
                break;
            }
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetARPEntryUsedInternal */




/*****************************************************************************/
/** fmRefreshARPUsedCache
 * \ingroup routerArp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Takes a snapshot of the ARP used bits in hardware and
 *                  caches them in memory for subsequent examination with
 *                  ''fmGetARPEntryUsed'', ''fmGetMcastGroupUsed'' or
 *                  ''fmGetECMPGroupNextHopUsed''. This function may also be
 *                  used to invalidate the ARP used cache.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       invalidateCache should be set to TRUE if the cache is to 
 *                  be deleted.
 *
 * \param[in]       resetFlag specifies whether the hardware ARP used bits
 *                  should be reset after they are read into the cache or
 *                  the cache is deleted.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_NO_MEM if no memory available for creating the
 *                  cached snapshot.
 *
 *****************************************************************************/
fm_status fmRefreshARPUsedCache(fm_int  sw,
                                fm_bool invalidateCache,
                                fm_bool resetFlag)
{
    fm_status  err;
    fm_switch *switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, invalidateCache = %d, resetFlag = %d\n",
                     sw,
                     invalidateCache,
                     resetFlag);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    if (err == FM_OK)
    {
        FM_API_CALL_FAMILY(err,
                           switchPtr->RefreshARPUsedCache,
                           sw,
                           invalidateCache,
                           resetFlag);

        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmRefreshARPUsedCache */




/*****************************************************************************/
/** fmCreateInterface
 * \ingroup routerIf
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Create an interface between the layer 3 router and the
 *                  the layer 2 switch.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      interface points to caller-allocated storage where this
 *                  function should place the interface number (handle) of the
 *                  newly created interface.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_TABLE_FULL if the maximum number of interfaces
 *                  (''FM_MAX_IP_INTERFACES'') have already been created.
 *
 *****************************************************************************/
fm_status fmCreateInterface(fm_int  sw,
                            fm_int *interface)
{
    fm_switch *             switchPtr;
    fm_status               err;
    fm_bool                 lockTaken = FALSE;
    fm_int                  ifNum;
    fm_intIpInterfaceEntry *ifEntry;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->ipInterfaceEntries != NULL)
    {
        err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

        if (err != FM_OK)
        {
            goto ABORT;
        }

        lockTaken = TRUE;

        /* find an available interface */
        err = fmFindBitInBitArray(&switchPtr->ipInterfaceEntriesInUse,
                                  0,
                                  FALSE,
                                  &ifNum);

        if (err != FM_OK)
        {
            goto ABORT;
        }

        if ( (ifNum < 0) || (ifNum >= switchPtr->maxIpInterfaces) )
        {
            err = FM_ERR_TABLE_FULL;
            goto ABORT;
        }

        ifEntry               = &switchPtr->ipInterfaceEntries[ifNum];
        ifEntry->interfaceNum = ifNum;
        ifEntry->vlan         = FM_INVALID_VLAN;
        ifEntry->oldVlan      = FM_INVALID_VLAN;
        ifEntry->state        = FM_INTERFACE_STATE_ADMIN_UP;
        ifEntry->extension    = NULL;
        fmInitInterfaceEntryLinkedLists(ifEntry);

        err = fmSetBitArrayBit(&switchPtr->ipInterfaceEntriesInUse, 
                               ifNum,
                               TRUE);

        if (err != FM_OK)
        {
            goto ABORT;
        }

        if (switchPtr->CreateInterface != NULL)
        {
            err = switchPtr->CreateInterface(sw, ifNum);
        }
    }
    else
    {
        err = FM_ERR_UNSUPPORTED;
    }

ABORT:

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    if (err == FM_OK)
    {
        *interface = ifNum;
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmCreateInterface */




/*****************************************************************************/
/** fmDeleteInterface
 * \ingroup routerIf
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Delete an interface between the layer 3 router and the
 *                  the layer 2 switch, releasing any allocated resources for
 *                  this interface.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       interface is the interface number returned by
 *                  ''fmCreateInterface''.
 *
 * \return          FM_OK if successful
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_NOT_FOUND if interface is not recognized.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 *
 *****************************************************************************/
fm_status fmDeleteInterface(fm_int sw,
                            fm_int interface)
{
    fm_switch *                    switchPtr;
    fm_status                      err;
    fm_bool                        lockTaken = FALSE;
    fm_intIpInterfaceEntry *       ifEntry;
    fm_bool                        ifIsInUse;
    fm_intIpInterfaceAddressEntry *addrEntry;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, interface=%d\n",
                     sw,
                     interface);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->ipInterfaceEntries != NULL)
    {
        err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

        if (err != FM_OK)
        {
            goto ABORT;
        }

        lockTaken = TRUE;

        /* make sure the interface is in use */
        err = fmGetBitArrayBit(&switchPtr->ipInterfaceEntriesInUse,
                               interface,
                               &ifIsInUse);

        if (err != FM_OK)
        {
            goto ABORT;
        }

        if (!ifIsInUse)
        {
            err = FM_ERR_NOT_FOUND;
            goto ABORT;
        }

        /* delete all addresses associated with this interface */
        ifEntry   = &switchPtr->ipInterfaceEntries[interface];

        /* Change the interface state to be able to update the ECMP group */

        ifEntry->state        = FM_INTERFACE_STATE_ADMIN_DOWN;

        /* Disable all next-hops using this interface */
        err = UpdateEcmpGroupsForInterface(sw, ifEntry);

        /* delete all addresses associated with this interface */

        while ( ( addrEntry = fmGetFirstInterfaceAddress(ifEntry) ) != NULL )
        {
            fmRemoveInterfaceAddress(ifEntry, addrEntry);

            fmFree(addrEntry);

#ifdef DEBUG_TRACK_MEMORY_USE
            FM_LOG_PRINT("Routing DELETE I/F: address entry %p\n",
                         (void *) addrEntry);
#endif
        }

        if (err != FM_OK)
        {
            FM_LOG_ERROR(FM_LOG_CAT_ROUTING,
                         "Routing DELETE I/F: Updating ECMP Groups "
                         "for interface failed with error %d (%s)\n",
                         err,
                         fmErrorMsg(err) );
        }

        if (switchPtr->DeleteInterface != NULL)
        {
            err = switchPtr->DeleteInterface(sw, interface);

            if (err != FM_OK)
            {
                FM_LOG_ERROR(FM_LOG_CAT_ROUTING,
                             "DeleteInterface(%d,%d) failed: %s\n",
                             sw,
                             interface,
                             fmErrorMsg(err) );
            }
        }

        ifEntry->interfaceNum = -1;
        ifEntry->vlan         = FM_INVALID_VLAN;
        ifEntry->state        = FM_INTERFACE_STATE_ADMIN_DOWN;
        fmInitInterfaceEntryLinkedLists(ifEntry);

        /* Release the interface for future re-use */
        err = fmSetBitArrayBit(&switchPtr->ipInterfaceEntriesInUse,
                               interface,
                               FALSE);

        if (err != FM_OK)
        {
            goto ABORT;
        }
    }
    else
    {
        err = FM_ERR_UNSUPPORTED;
    }

ABORT:

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmDeleteInterface */




/*****************************************************************************/
/** fmGetInterfaceList
 * \ingroup routerIf
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Return a list of all interface numbers on a switch.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      numInterfaces points to caller allocated storage where
 *                  this function should place the number of interfaces
 *                  returned in interfaceList.
 *
 * \param[out]      interfaceList is an array that this function will fill
 *                  with the list of interfaces numbers.
 *
 * \param[in]       max is the size of interfaceList, being the maximum
 *                  number of interface numbers that interfaceList can hold.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_BUFFER_FULL if max was too small to accommodate
 *                  the entire list of interface numbers.
 * \return          FM_ERR_UNSUPPORTED if the switch doesn't support routing.
 *
 *****************************************************************************/
fm_status fmGetInterfaceList(fm_int  sw,
                             fm_int *numInterfaces,
                             fm_int *interfaceList,
                             fm_int  max)
{
    fm_switch *switchPtr;
    fm_int     interfaceNum;
    fm_status  err;
    fm_int     ifCount = 0;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, numInterfaces = %p, interfaceList = %p, max = %d\n",
                     sw,
                     (void *) numInterfaces,
                     (void *) interfaceList,
                     max);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->ipInterfaceEntries != NULL)
    {
        err = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

        if (err == FM_OK)
        {
            interfaceNum = -1;

            while (1)
            {
                err = fmFindBitInBitArray(&switchPtr->ipInterfaceEntriesInUse,
                                          interfaceNum + 1,
                                          TRUE,
                                          &interfaceNum);

                if (err != FM_OK)
                {
                    break;
                }

                if (interfaceNum < 0)
                {
                    break;
                }

                if (ifCount >= max)
                {
                    err = FM_ERR_BUFFER_FULL;
                    break;
                }

                interfaceList[ifCount] = interfaceNum;
                ifCount++;
            }

            fmReleaseReadLock(&switchPtr->routingLock);
        }
    }
    else
    {
        err = FM_ERR_UNSUPPORTED;
    }

    UNPROTECT_SWITCH(sw);

    *numInterfaces = ifCount;

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetInterfaceList */




/*****************************************************************************/
/** fmGetInterfaceFirst
 * \ingroup routerIf
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Retrieve the first interface number.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      firstInterface points to caller-allocated storage where
 *                  this function will store the first interface number. Will
 *                  be set to -1 if no interfaces found.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_NO_MORE if there are no interfaces.
 * \return          FM_ERR_UNSUPPORTED if the switch doesn't support routing.
 *
 *****************************************************************************/
fm_status fmGetInterfaceFirst(fm_int sw, fm_int *firstInterface)
{
    fm_switch *switchPtr;
    fm_int     interfaceNum;
    fm_status  err;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->ipInterfaceEntries != NULL)
    {
        err = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

        if (err == FM_OK)
        {
            interfaceNum = -1;

            err = fmFindBitInBitArray(&switchPtr->ipInterfaceEntriesInUse,
                                      interfaceNum + 1,
                                      TRUE,
                                      &interfaceNum);

            if (err == FM_OK)
            {
                *firstInterface = interfaceNum;

                if (interfaceNum < 0)
                {
                    err = FM_ERR_NO_MORE;
                }
            }

            fmReleaseReadLock(&switchPtr->routingLock);
        }
    }
    else
    {
        err = FM_ERR_UNSUPPORTED;
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetInterfaceFirst */




/*****************************************************************************/
/** fmGetInterfaceNext
 * \ingroup routerIf
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Retrieve the next interface number.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       currentInterface is the last interface found by a previous
 *                  call to this function or to ''fmGetInterfaceFirst''.
 *
 * \param[out]      nextInterface points to caller-allocated storage where this
 *                  function will store the next interface number. Will be set
 *                  to -1 if no more interfaces found.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_NO_MORE if there are no more interfaces.
 * \return          FM_ERR_UNSUPPORTED if the switch doesn't support routing.
 *
 *****************************************************************************/
fm_status fmGetInterfaceNext(fm_int  sw,
                             fm_int  currentInterface,
                             fm_int *nextInterface)
{
    fm_switch *switchPtr;
    fm_int     interfaceNum;
    fm_status  err;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, currentInterface = %d\n",
                     sw,
                     currentInterface);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->ipInterfaceEntries != NULL)
    {
        err = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

        if (err == FM_OK)
        {
            err = fmFindBitInBitArray(&switchPtr->ipInterfaceEntriesInUse,
                                      currentInterface + 1,
                                      TRUE,
                                      &interfaceNum);

            if (err == FM_OK)
            {
                *nextInterface = interfaceNum;

                if (interfaceNum < 0)
                {
                    err = FM_ERR_NO_MORE;
                }
            }

            fmReleaseReadLock(&switchPtr->routingLock);
        }
    }
    else
    {
        err = FM_ERR_UNSUPPORTED;
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetInterfaceNext */




/*****************************************************************************/
/** fmSetInterfaceAttribute
 * \ingroup routerIf
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Set an interface attribute.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       interface is the interface on which to operate.
 *
 * \param[in]       attr is the interface attribute to set (see
 *                  ''Router Interface Attributes'').
 *
 * \param[in]       value points to the attribute value to set
 *                  Note: To set the Interface VLAN attribute to "no vlan",
 *                  use the constant FM_INVALID_VLAN for the vlan value.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_INTERFACE if interface is unknown.
 * \return          FM_ERR_INVALID_ATTRIB if attr is not a recognized attribute.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 *
 *****************************************************************************/
fm_status fmSetInterfaceAttribute(fm_int sw,
                                  fm_int interface,
                                  fm_int attr,
                                  void * value)
{
    fm_switch *             switchPtr;
    fm_status               err;
    fm_bool                 lockTaken = FALSE;
    fm_intIpInterfaceEntry *ifEntry;
    fm_uint16               vlan;
    fm_int                  otherIf;
    fm_intIpInterfaceEntry *otherIfEntry;
    fm_intArpEntry *        arpEntry;
    fm_intArpEntry *        nextArpKey;
    fm_intArpEntry *        nextArp;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if ( (switchPtr->SetInterfaceAttribute != NULL)
        && (switchPtr->ipInterfaceEntries != NULL) )
    {
        err = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        lockTaken = TRUE;

        /* make sure the interface has been created */
        err = fmGetInterface(sw, interface, &ifEntry);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        switch (attr)
        {
            case FM_INTERFACE_STATE:
                ifEntry->oldState = ifEntry->state;
                ifEntry->state    = *( (fm_interfaceState *) value );

                if (ifEntry->oldState == ifEntry->state)
                {
                    break;
                }

                /* Update all ECMP Groups with next-hops using this interface */
                err = UpdateEcmpGroupsForInterface(sw, ifEntry);
                break;

            case FM_INTERFACE_VLAN:
                vlan = *( (fm_uint16 *) value );

                /* Per bz2529, we allow now frames on VLAN 0 */
                /*if (vlan == 0)
                {
                    err = FM_ERR_INVALID_ARGUMENT;
                    break;
                }*/

                ifEntry->oldVlan = ifEntry->vlan;
                ifEntry->vlan    = vlan;

                /* scan all other interfaces to make sure this vlan
                 * isn't already in use
                 */
                err = fmGetInterfaceFirst(sw, &otherIf);

                while (err == FM_OK)
                {
                    if (otherIf != interface)
                    {
                        err = fmGetInterface(sw, otherIf, &otherIfEntry);

                        if (err != FM_OK)
                        {
                            break;
                        }

                        if (otherIfEntry->vlan == vlan)
                        {
                            err = FM_ERR_VLAN_ALREADY_ASSIGNED;
                            break;
                        }
                    }

                    err = fmGetInterfaceNext(sw, otherIf, &otherIf);
                }

                if (err != FM_ERR_NO_MORE)
                {
                    break;
                }

                /* Find and update all ARP entries that use this interface */
                arpEntry = fmGetFirstArp(switchPtr);

                while (arpEntry != NULL)
                {
                    if (arpEntry->ifEntry == ifEntry)
                    {
                        /* remove the arp entry from the tree */
                        err = fmCustomTreeRemove(&switchPtr->arpTree,
                                                 arpEntry,
                                                 NULL);
                        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

                        /* update the ARP entry's vlan */
                        arpEntry->arp.vlan = vlan;

                        /* add the arp entry back into the tree */
                        err = fmCustomTreeInsert(&switchPtr->arpTree,
                                                 arpEntry,
                                                 arpEntry);
                        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

                        fmRemoveArp(switchPtr, arpEntry);

                        /* Find its successor arp entry, if any */
                        err = fmCustomTreeSuccessor(&switchPtr->arpTree,
                                                    arpEntry,
                                                    (void **) &nextArpKey,
                                                    (void **) &nextArp);

                        if (err != FM_OK)
                        {
                            if (err == FM_ERR_NO_MORE)
                            {
                                nextArp = NULL;
                            }
                            else
                            {
                                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
                            }
                        }

                        /* entry goes before the next entry */
                        fmInsertArpBefore(switchPtr, nextArp, arpEntry);

                        /* Don't update the hardware - the ECMP group updates
                         * that follow will take care of that
                         */
                    }

                    arpEntry = fmGetNextArp(arpEntry);
                }

                /* Update all ECMP Groups with next-hops using this interface */
                err = UpdateEcmpGroupsForInterface(sw, ifEntry);
                break;

            default:
                err = FM_ERR_INVALID_ATTRIB;
                break;

        }   /* end switch (attr) */

        /* Update the hardware */
        if (err == FM_OK)
        {
            err = switchPtr->SetInterfaceAttribute(sw, interface, attr, value);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
        }
    }
    else
    {
        err = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

ABORT:

    if (lockTaken)
    {
        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmSetInterfaceAttribute */




/*****************************************************************************/
/** fmGetInterfaceAttribute
 * \ingroup routerIf
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Get the current value of an interface attribute.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       interface is the interface on which to operate.
 *
 * \param[in]       attr is the interface attribute to set (see
 *                  ''Router Interface Attributes'').
 *
 * \param[out]      value points to a caller-allocated storage where
 *                  this function will place the value of the attribute.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_INTERFACE if interface is unknown.
 * \return          FM_ERR_INVALID_ATTRIB if attr is not a recognized attribute.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 *
 *****************************************************************************/
fm_status fmGetInterfaceAttribute(fm_int sw,
                                  fm_int interface,
                                  fm_int attr,
                                  void * value)
{
    fm_switch *             switchPtr;
    fm_status               err;
    fm_bool                 lockTaken = FALSE;
    fm_intIpInterfaceEntry *ifEntry;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->ipInterfaceEntries != NULL)
    {
        err = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        lockTaken = TRUE;

        /* make sure the interface has been created */
        err = fmGetInterface(sw, interface, &ifEntry);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        switch (attr)
        {
            case FM_INTERFACE_STATE:
                *( (fm_interfaceState *) value ) = ifEntry->state;
                break;

            case FM_INTERFACE_VLAN:
                *( (fm_uint16 *) value ) = ifEntry->vlan;
                break;

            default:
                err = FM_ERR_INVALID_ATTRIB;
                goto ABORT;

        }   /* end switch (attr) */

    }
    else
    {
        err = FM_ERR_UNSUPPORTED;
    }

ABORT:

    if (lockTaken)
    {
        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetInterfaceAttribute */




/*****************************************************************************/
/** fmAddInterfaceAddr
 * \ingroup routerIf
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Assign an IP address to an interface. Any route that was
 *                  using this address will automatically be activated if
 *                  possible. Multiple IP addresses may be added per interface.
 *
 * \note            The same IP address cannot be added twice on the same
 *                  switch.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       interface is the interface on which to operate.
 *
 * \param[in]       addr points to an IP address to assign to this interface.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_INTERFACE if interface is unknown.
 * \return          FM_ERR_NO_MEM if not enough memory for address storage.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 *
 *****************************************************************************/
fm_status fmAddInterfaceAddr(fm_int     sw,
                             fm_int     interface,
                             fm_ipAddr *addr)
{
    fm_switch *                    switchPtr;
    fm_status                      err;
    fm_bool                        lockTaken;
    fm_intIpInterfaceEntry *       ifEntry;
    fm_intIpInterfaceAddressEntry *addrEntry;
    fm_int                         ecmpGroupId;
    fm_intEcmpGroup *              ecmpGroup;
    fm_intNextHop *                nextHop;
    fm_intNextHop *                nextHopValue;
    fm_nextHop *                   arpNextHop;
    fm_customTreeIterator          nextHopIter;
    fm_customTree                  nextHopTree;
    fm_bitArray                    ecmpGroupList;
    fm_bool                        nextHopTreeUsed;
    fm_bool                        ecmpGroupListUsed;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, interface=%d\n",
                     sw,
                     interface);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr         = GET_SWITCH_PTR(sw);
    lockTaken         = FALSE;
    nextHopTreeUsed   = FALSE;
    ecmpGroupListUsed = FALSE;

    if (switchPtr->ipInterfaceEntries == NULL)
    {
        err = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    lockTaken = TRUE;

    /* make sure the interface exists */
    err = fmGetInterface(sw, interface, &ifEntry);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    /* Allocate and initialize a new address entry */
    addrEntry = (fm_intIpInterfaceAddressEntry *)
                fmAlloc( sizeof(fm_intIpInterfaceAddressEntry) );

    if (addrEntry == NULL)
    {
        err = FM_ERR_NO_MEM;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

#ifdef DEBUG_TRACK_MEMORY_USE
    FM_LOG_PRINT( "Routing MALLOC: ip Interface Address Entry %p/%d\n",
                 (void *) addrEntry,
                 sizeof(fm_intIpInterfaceAddressEntry) );
#endif

    FM_CLEAR(*addrEntry);

    addrEntry->ifEntry = ifEntry;
    addrEntry->addr    = *addr;
    fmCustomTreeInit(&addrEntry->nextHopTree, fmCompareInternalNextHops);

    /* Add the address entry to the interface address list */
    fmAppendInterfaceAddress(ifEntry, addrEntry);

    FM_LOG_DEBUG( FM_LOG_CAT_ROUTING,
                  "Interface Address Entry %p added to Interface %p (%d)\n",
                  (void *) addrEntry,
                  (void *) ifEntry,
                  ifEntry->interfaceNum );

    /* Deliver the add command to the lower-level, if needed */
    if (switchPtr->AddInterfaceAddr != NULL)
    {
        err = switchPtr->AddInterfaceAddr(sw, interface, addr);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    /**************************************************************************
     * Find and update all next-hops that use this interface address. Because
     * we can't modify the noInterfaceNextHops tree while iterating it, we
     * find each next-hop and save it's address in a local holding tree, then
     * process the next-hops once we have the full list.
     **************************************************************************/
    fmCustomTreeIterInit(&nextHopIter, &switchPtr->noInterfaceNextHops);
    fmCustomTreeInit(&nextHopTree, fmCompareInternalNextHops);
    nextHopTreeUsed = TRUE;

    /* Build a tree of next-hops that belong to the interface address */
    while ( ( err = fmCustomTreeIterNext( &nextHopIter,
                                        (void *) &nextHop,
                                        (void *) &nextHopValue ) ) == FM_OK )
    {
        switch (nextHop->nextHop.type)
        {
            case FM_NEXTHOP_TYPE_ARP:
                break;

            default:
                continue;
        }

        arpNextHop = &nextHop->nextHop.data.arp;

        if ( fmCompareIPAddresses(&arpNextHop->interfaceAddr, addr) == 0 )
        {
            /* Add the next-hop to the holding tree. */
            err = fmCustomTreeInsert(&nextHopTree, nextHop, nextHop);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
        }
    }

    /* Now process each next-hop and build a list of ECMP groups to update */
    err = fmCreateBitArray(&ecmpGroupList, switchPtr->maxArpEntries);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    ecmpGroupListUsed = TRUE;

    while (1)
    {
        /* Retrieve the first next-hop from the holding tree. */
        fmCustomTreeIterInit(&nextHopIter, &nextHopTree);
        err = fmCustomTreeIterNext( &nextHopIter,
                                    (void *) &nextHop,
                                    (void *) &nextHopValue );
        if (err == FM_ERR_NO_MORE)
        {
            break;
        }

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        /* Remove the next-hop from the holding tree. */
        err = fmCustomTreeRemoveCertain(&nextHopTree, nextHop, NULL);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        /* Point the next-hop to this interface address entry. */
        nextHop->interfaceAddressEntry = addrEntry;

        /* Add the next-hop to the interface address next-hop tree */
        err = fmCustomTreeInsert(&addrEntry->nextHopTree, nextHop, nextHop);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        FM_LOG_DEBUG( FM_LOG_CAT_ROUTING,
                      "NextHop %p added to addrEntry %p next-hop-tree\n",
                      (void *) nextHop,
                      (void *) addrEntry );

        /* Remove the next-hop from the no-interface-next-hops tree. */
        err = fmCustomTreeRemoveCertain(&switchPtr->noInterfaceNextHops,
                                        nextHop,
                                        NULL);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        ecmpGroup  = nextHop->ecmpGroup;
        err = fmSetBitArrayBit(&ecmpGroupList, ecmpGroup->groupId, TRUE);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    }

    /* Finally, update each ECMP group */
    ecmpGroupId = -1;

    while (1)
    {
        err = fmFindBitInBitArray(&ecmpGroupList,
                                  ecmpGroupId + 1,
                                  TRUE,
                                  &ecmpGroupId);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

        if (ecmpGroupId < 0)
        {
            break;
        }

        ecmpGroup = switchPtr->ecmpGroups[ecmpGroupId];

        /* Update the ECMP Group */
        UpdateEcmpGroup(sw, ecmpGroup);
    }


ABORT:

    if (ecmpGroupListUsed)
    {
        fmDeleteBitArray(&ecmpGroupList);
    }

    if (nextHopTreeUsed)
    {
        fmCustomTreeDestroy(&nextHopTree, NULL);
    }

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmAddInterfaceAddr */




/*****************************************************************************/
/** fmDeleteInterfaceAddr
 * \ingroup routerIf
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Delete an IP address from an interface. Note that any
 *                  route that was using this address will automatically be
 *                  placed in a disabled state.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       interface is the interface on which to operate.
 *
 * \param[in]       addr points to the IP address to be deleted from this
 *                  interface.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_INTERFACE if interface is unknown.
 * \return          FM_ERR_INVALID_IPADDR if the IP address is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 *
 *****************************************************************************/
fm_status fmDeleteInterfaceAddr(fm_int     sw,
                                fm_int     interface,
                                fm_ipAddr *addr)
{
    fm_switch *                    switchPtr;
    fm_status                      err;
    fm_bool                        lockTaken = FALSE;
    fm_intIpInterfaceEntry *       ifEntry;
    fm_intIpInterfaceAddressEntry *addrEntry;
    fm_customTreeIterator          nextHopIter;
    fm_intNextHop *                nextHop;
    fm_intNextHop *                nextHopValue;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, interface=%d\n",
                     sw,
                     interface);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->ipInterfaceEntries == NULL)
    {
        err = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    err = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    lockTaken = TRUE;

    /* make sure the interface exists */
    err = fmGetInterface(sw, interface, &ifEntry);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    /* find the address in the address list */
    addrEntry = fmGetFirstInterfaceAddress(ifEntry);

    while (addrEntry != NULL)
    {
        if (fmCompareIPAddresses(&addrEntry->addr, addr) == 0)
        {
            fmCustomTreeIterInit(&nextHopIter, &addrEntry->nextHopTree);
            while (1)
            {
                err = fmCustomTreeIterNext( &nextHopIter,
                                            (void *) &nextHop,
                                            (void *) &nextHopValue );
                if (err == FM_ERR_NO_MORE)
                {
                    break;
                }

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

                nextHop->interfaceAddressEntry = NULL;
            }

            /* Update all ECMP Groups and routes using this interface */
            err = UpdateEcmpGroupsForInterface(sw, ifEntry);

            /* Take each next-hop from the interface address next-hop tree
             * and put it into the noInterfaceNextHops tree */
            while (1)
            {
                fmCustomTreeIterInit(&nextHopIter, &addrEntry->nextHopTree);
                err = fmCustomTreeIterNext( &nextHopIter,
                                            (void *) &nextHop,
                                            (void *) &nextHopValue );
                if (err == FM_ERR_NO_MORE)
                {
                    err = FM_OK;
                    break;
                }

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

                err = fmCustomTreeRemoveCertain(&addrEntry->nextHopTree,
                                                nextHop,
                                                NULL);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);

                nextHop->interfaceAddressEntry = NULL;

                err = fmCustomTreeInsert( &switchPtr->noInterfaceNextHops,
                                          nextHop,
                                          nextHop);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
            }

            /* Remove the address record from the interface address list */
            fmRemoveInterfaceAddress(ifEntry, addrEntry);

            if (err != FM_OK)
            {
                FM_LOG_ERROR( FM_LOG_CAT_ROUTING,
                             "Routing DELETE I/F Addr: Updating ECMP Groups "
                             "for interface failed with error %d (%s)\n",
                             err,
                             fmErrorMsg(err) );
            }

            fmFree(addrEntry);

#ifdef DEBUG_TRACK_MEMORY_USE
            FM_LOG_PRINT("Routing DELETE I/F Addr: address entry %p\n",
                         (void *) addrEntry);
#endif

            break;
        }

        addrEntry = fmGetNextInterfaceAddress(addrEntry);
    }

    /* Deliver the delete command to the lower-level, if needed */
    if (switchPtr->DeleteInterfaceAddr != NULL)
    {
        err = switchPtr->DeleteInterfaceAddr(sw, interface, addr);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

ABORT:

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmDeleteInterfaceAddr */




/*****************************************************************************/
/** fmGetInterfaceAddrList
 * \ingroup routerIf
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Return a list of addresses assigned to an interface.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       interface is the interface number (returned by
 *                  ''fmCreateInterface'') from which the addresses should be
 *                  retrieved.
 *
 * \param[out]      numAddresses points to caller-allocated storage where
 *                  this function should place the number of addresses
 *                  returned in addressList.
 *
 * \param[out]      addressList is an array that this function will fill with
 *                  the list of addresses assigned to the interface.
 *
 * \param[in]       max is the size of addressList, being the maximum number
 *                  of addresses that addressList can hold.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_INTERFACE if interface is unknown.
 * \return          FM_ERR_BUFFER_FULL if max was too small to accommodate
 *                  the entire list of addresses.
 *
 *****************************************************************************/
fm_status fmGetInterfaceAddrList(fm_int     sw,
                                 fm_int     interface,
                                 fm_int *   numAddresses,
                                 fm_ipAddr *addressList,
                                 fm_int     max)
{
    fm_switch *                    switchPtr;
    fm_status                      err;
    fm_bool                        lockTaken = FALSE;
    fm_intIpInterfaceEntry *       ifEntry;
    fm_intIpInterfaceAddressEntry *addrEntry;
    fm_int                         addrCount = 0;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, interface = %d, numAddresses = %p, "
                     "addressList = %p, max = %d\n",
                     sw,
                     interface,
                     (void *) numAddresses,
                     (void *) addressList,
                     max);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->ipInterfaceEntries != NULL)
    {
        err = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

        if (err != FM_OK)
        {
            goto ABORT;
        }

        lockTaken = TRUE;

        /* make sure the interface exists */
        err = fmGetInterface(sw, interface, &ifEntry);

        if (err != FM_OK)
        {
            goto ABORT;
        }

        addrEntry = fmGetFirstInterfaceAddress(ifEntry);

        while (addrEntry != NULL)
        {
            if (addrCount >= max)
            {
                err = FM_ERR_BUFFER_FULL;
                break;
            }

            addressList[addrCount] = addrEntry->addr;
            addrCount++;
            addrEntry = fmGetNextInterfaceAddress(addrEntry);
        }
    }
    else
    {
        err = FM_ERR_UNSUPPORTED;
    }

ABORT:

    if (lockTaken)
    {
        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    *numAddresses = addrCount;

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetInterfaceAddrList */




/*****************************************************************************/
/** fmGetInterfaceAddrFirst
 * \ingroup routerIf
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Retrieve the first address assigned to this interface.
 *                  Addresses are retrieved in numerical order.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       interface is the interface on which to operate.
 *
 * \param[out]      firstAddr points to caller-allocated storage where this
 *                  function will store the first address.  The structure
 *                  will be filled with zeroes if there are no IP addresses
 *                  assigned to this interface.
 *
 * \param[out]      searchToken points to caller-allocated storage of type
 *                  fm_voidptr, where this function will store a token
 *                  to be used in a subsequent call to ''fmGetInterfaceAddrNext''.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_INVALID_INTERFACE if interface is unknown.
 * \return          FM_ERR_NO_MORE if there are no addresses on this interface.
 *
 *****************************************************************************/
fm_status fmGetInterfaceAddrFirst(fm_int      sw,
                                  fm_int      interface,
                                  fm_voidptr *searchToken,
                                  fm_ipAddr * firstAddr)
{
    fm_switch *                    switchPtr;
    fm_status                      err;
    fm_bool                        lockTaken = FALSE;
    fm_intIpInterfaceEntry *       ifEntry;
    fm_intIpInterfaceAddressEntry *addrEntry;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->ipInterfaceEntries != NULL)
    {
        err = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

        if (err != FM_OK)
        {
            goto ABORT;
        }

        lockTaken = TRUE;

        /* make sure the interface exists */
        err = fmGetInterface(sw, interface, &ifEntry);

        if (err != FM_OK)
        {
            goto ABORT;
        }

        addrEntry = fmGetFirstInterfaceAddress(ifEntry);

        if (addrEntry == NULL)
        {
            err = FM_ERR_NO_MORE;
        }
        else
        {
            *firstAddr   = addrEntry->addr;
            *searchToken = (fm_voidptr) addrEntry;
        }
    }
    else
    {
        err = FM_ERR_UNSUPPORTED;
    }

ABORT:

    if (lockTaken)
    {
        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetInterfaceAddrFirst */




/*****************************************************************************/
/** fmGetInterfaceAddrNext
 * \ingroup routerIf
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Retrieve the next address assigned to this interface.
 *                  Addresses are retrieved in numerical order.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in,out]   searchToken points to caller-allocated storage of type
 *                  fm_voidptr that has been filled in by a prior call to
 *                  this function or to ''fmGetInterfaceAddrFirst''. It will be
 *                  updated by this function with a new value to be used in a
 *                  subsequent call to this function.
 *
 * \param[out]      nextAddr points to caller-allocated storage where this
 *                  function will store the next address.  The structure
 *                  will be filled with zeroes if there are no more IP
 *                  addresses assigned to this interface.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_INVALID_INTERFACE if interface is unknown.
 * \return          FM_ERR_NO_MORE if there are no more addresses on this
 *                  interface.
 *
 *****************************************************************************/
fm_status fmGetInterfaceAddrNext(fm_int      sw,
                                 fm_voidptr *searchToken,
                                 fm_ipAddr * nextAddr)
{
    fm_switch *                    switchPtr;
    fm_status                      err;
    fm_bool                        lockTaken = FALSE;
    fm_intIpInterfaceAddressEntry *addrEntry;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->ipInterfaceEntries != NULL)
    {
        err = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

        if (err != FM_OK)
        {
            goto ABORT;
        }

        lockTaken = TRUE;

        addrEntry = (fm_intIpInterfaceAddressEntry *) *searchToken;

        addrEntry = fmGetNextInterfaceAddress(addrEntry);

        if (addrEntry == NULL)
        {
            err = FM_ERR_NO_MORE;
        }
        else
        {
            *nextAddr    = addrEntry->addr;
            *searchToken = (fm_voidptr) addrEntry;
        }
    }
    else
    {
        err = FM_ERR_UNSUPPORTED;
    }

ABORT:

    if (lockTaken)
    {
        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetInterfaceAddrNext */




/*****************************************************************************/
/** fmIsUnicastIPAddress
 * \ingroup intRouter
 *
 * \desc            Returns TRUE if an IP Address is a unicast address.
 *
 * \param[in]       addr points to the IP Address to inspect.
 *
 * \return          TRUE if the address is a unicast address.
 * \return          FALSE if the address is a multicast address.
 *
 *****************************************************************************/
fm_bool fmIsUnicastIPAddress(fm_ipAddr *addr)
{
    fm_byte ipByte;
    fm_bool isUnicast = TRUE;

    /* High order byte is the byte of interest for both IPv4 and IPv6 */
    if (addr->isIPv6)
    {
        /* IPv6 case */
        ipByte = (ntohl(addr->addr[3]) >> 24) & 0xff;
        if (ipByte == 0xff)
        {
            isUnicast = FALSE;
        }
    }
    else
    {
        /* IPv4 case */
        ipByte = (ntohl(addr->addr[0]) >> 24) & 0xff;
        if (ipByte >= 224 && ipByte <= 239)
        {
            isUnicast = FALSE;
        }
    }

    return isUnicast;

}   /* end fmIsUnicastIPAddress */




/*****************************************************************************/
/** fmIsMulticastIPAddress
 * \ingroup intRouter
 *
 * \desc            Returns TRUE if an IP Address is a multicast address.
 *
 * \param[in]       addr points to the IP Address to inspect.
 *
 * \return          TRUE if the address is a multicast address.
 * \return          FALSE if the address is a unicast address.
 *
 *****************************************************************************/
fm_bool fmIsMulticastIPAddress(fm_ipAddr *addr)
{
    fm_byte ipByte;
    fm_bool isMulticast = FALSE;

    /* High order byte is the byte of interest for both IPv4 and IPv6 */
    /* Must convert to host order since addr is defined as network order */

    if (addr->isIPv6)
    {
        /* IPv6 case */
        ipByte = (ntohl(addr->addr[3]) >> 24) & 0xff;
        if (ipByte == 0xff)
        {
            isMulticast = TRUE;
        }
    }
    else
    {
        /* IPv4 case */
        ipByte = (ntohl(addr->addr[0]) >> 24) & 0xff;
        if (ipByte >= 224 && ipByte <= 239)
        {
            isMulticast = TRUE;
        }
    }

    return isMulticast;

}   /* end fmIsMulticastIPAddress */




/*****************************************************************************/
/** fmIsRouteEntryUnicast
 * \ingroup routerRoute
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Returns TRUE if a route entry is a unicast entry.
 *
 * \param[in]       route points to the route entry to inspect.
 *
 * \return          TRUE if the destination address is a unicast address.
 * \return          FALSE if the destination address is a multicast address.
 *
 *****************************************************************************/
fm_bool fmIsRouteEntryUnicast(fm_routeEntry *route)
{
    fm_bool isUnicast;

    switch (route->routeType)
    {
        case FM_ROUTE_TYPE_UNICAST:
        case FM_ROUTE_TYPE_UNICAST_ECMP:
            isUnicast = TRUE;
            break;

        default:
            isUnicast = FALSE;
            break;
    }

    return isUnicast;

}   /* end fmIsRouteEntryUnicast */




/*****************************************************************************/
/** fmIsRouteEntryMulticast
 * \ingroup routerRoute
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Returns TRUE if a route entry is a multicast entry.
 *
 * \param[in]       route points to the route entry to inspect.
 *
 * \return          TRUE if the destination address is a multicast address.
 * \return          FALSE if the destination address is a unicast address.
 *
 *****************************************************************************/
fm_bool fmIsRouteEntryMulticast(fm_routeEntry *route)
{
    fm_bool isMulticast;

    if (route->routeType == FM_ROUTE_TYPE_UNICAST)
    {
        isMulticast = FALSE;
    }
    else
    {
        isMulticast = TRUE;
    }

    return isMulticast;

}   /* end fmIsRouteEntryMulticast */




/*****************************************************************************/
/** fmIsRouteEntrySGMulticast
 * \ingroup routerRoute
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Returns TRUE if a route entry is an (S,G) multicast entry.
 *
 * \param[in]       route points to the route entry to inspect.
 *
 * \return          TRUE if the route entry is an (S,G) multicast entry.
 * \return          FALSE if the route entry is a unicast or (*,G)
 *                  multicast entry.
 *
 *****************************************************************************/
fm_bool fmIsRouteEntrySGMulticast(fm_routeEntry *route)
{
    fm_bool isSGMulticast;

    if ( !fmIsRouteEntryMulticast(route) )
    {
        return FALSE;
    }

    switch (route->data.multicast.addressType)
    {
        case FM_MCAST_ADDR_TYPE_DSTIP_SRCIP:
        case FM_MCAST_ADDR_TYPE_DSTIP_SRCIP_VLAN:
            isSGMulticast = TRUE;
            break;

        default:
            isSGMulticast = FALSE;
            break;
    }

    return isSGMulticast;

}   /* end fmIsRouteEntrySGMulticast */




/*****************************************************************************/
/** fmCompareIPAddresses
 * \ingroup intRouter
 *
 * \desc            Compare IP Addresses.
 *
 * \param[in]       first points to the first IP Address.
 *
 * \param[in]       second points to the second IP Address.
 *
 * \return          -1 if the first IP Address sorts before the second.
 * \return           0 if the IP Addresses are identical.
 * \return           1 if the first IP Address sorts after the second.
 *
 *****************************************************************************/
fm_int fmCompareIPAddresses(fm_ipAddr *first, fm_ipAddr *second)
{
    fm_int retval = 0;
    fm_int i;

    if (first->isIPv6 == second->isIPv6)
    {
        /* addresses are same type */
        if (first->isIPv6)
        {
            /* IPv6 checks all words */
            for (i = 0 ; i < 4 ; i++)
            {
                if (first->addr[i] < second->addr[i])
                {
                    retval = -1;
                    break;
                }
                else if (first->addr[i] > second->addr[i])
                {
                    retval = 1;
                    break;
                }
            }
        }
        else
        {
            /* IPv4 just checks one word */
            if (first->addr[0] < second->addr[0])
            {
                retval = -1;
            }
            else if (first->addr[0] > second->addr[0])
            {
                retval = 1;
            }
        }
    }
    else
    {
        if (first->isIPv6)
        {
            /* second address must be IPv4, sort it before first address */
            retval = 1;
        }
        else
        {
            /* second address must be IPv6, sort it after first address */
            retval = -1;
        }
    }

    return retval;

}   /* end fmCompareIPAddresses */




/*****************************************************************************/
/** fmCompareRoutes
 * \ingroup intRouter
 *
 * \desc            Compare Route entries.
 *
 * \param[in]       first points to the first route.
 *
 * \param[in]       second points to the second route.
 *
 * \return          -1 if the first route sorts before the second.
 * \return           0 if the routes are identical.
 * \return           1 if the first route sorts after the second.
 *
 *****************************************************************************/
fm_int fmCompareRoutes(fm_routeEntry *first, fm_routeEntry *second)
{
    return CompareRoutes(first, second, FALSE);

}   /* end fmCompareRoutes */




/*****************************************************************************/
/** fmCompareEcmpRoutes
 * \ingroup intRouter
 *
 * \desc            Compare Route entries to determine if they are part
 *                  of the same ECMP group.
 *
 * \param[in]       first points to the first route.
 *
 * \param[in]       second points to the second route.
 *
 * \return          -1 if the routes are not part of the same ECMP group
 *                  and the first route sorts before the second.
 * \return          0 if the routes are part of the same ECMP group.
 * \return          1 if the routes are not part of the same ECMP group
 *                  and the first route sorts after the second.
 *
 *****************************************************************************/
fm_int fmCompareEcmpRoutes(fm_routeEntry *first, fm_routeEntry *second)
{
    return CompareRoutes(first, second, TRUE);

}   /* fmCompareRoutes */




/*****************************************************************************/
/** fmFindInterfaceAddrEntry
 * \ingroup intRouterIf
 *
 * \desc            Find an interface address entry for a specified interface IP.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       interfaceAddr points to the Interface IP Address.
 *
 * \param[out]      ifAddrEntry points to caller-provided storage into which the
 *                  interface address entry pointer is written if the entry
 *                  is found, or NULL if the entry wasn't found.
 *
 * \return          FM_OK if the interface entry was found.
 * \return          FM_FM_ERR_INVALID_INTERFACE if the entry wasn't found.
 *
 *****************************************************************************/
fm_status fmFindInterfaceAddrEntry(fm_int                          sw,
                                   fm_ipAddr *                     interfaceAddr,
                                   fm_intIpInterfaceAddressEntry **ifAddrEntry)
{
    fm_switch *                    switchPtr;
    fm_int                         interfaceNum;
    fm_intIpInterfaceEntry *       entryPtr;
    fm_status                      err;
    fm_intIpInterfaceAddressEntry *addrEntry;
    fm_int                         i;

    if ( fmIsIPAddressEmpty(interfaceAddr) )
    {
        if (ifAddrEntry != NULL)
        {
            *ifAddrEntry = NULL;
        }

        return FM_OK;
    }

    switchPtr    = GET_SWITCH_PTR(sw);
    addrEntry    = NULL;
    interfaceNum = -1;

    while (1)
    {
        err = fmFindBitInBitArray(&switchPtr->ipInterfaceEntriesInUse,
                                  interfaceNum + 1,
                                  TRUE,
                                  &interfaceNum);

        if (err != FM_OK)
        {
            return err;
        }

        if (interfaceNum < 0)
        {
            err = FM_ERR_INVALID_INTERFACE;
            break;
        }

        entryPtr = &switchPtr->ipInterfaceEntries[interfaceNum];

        addrEntry = fmGetFirstInterfaceAddress(entryPtr);

        while (addrEntry != NULL)
        {
            i = fmCompareIPAddresses(interfaceAddr,
                                     &addrEntry->addr);

            if (i == 0)
            {
                err = FM_OK;
                break;
            }

            addrEntry = fmGetNextInterfaceAddress(addrEntry);
        }

        if (addrEntry != NULL)
        {
            break;
        }
    }

    if (ifAddrEntry != NULL)
    {
        *ifAddrEntry = addrEntry;
    }

    return err;

}   /* end fmFindInterfaceAddrEntry */




/*****************************************************************************/
/** fmFindInterface
 * \ingroup intRouterIf
 *
 * \desc            Find an interface entry for a specified interface IP.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       interfaceAddr points to the Interface IP Address.
 *
 * \param[out]      ifEntry contains the pointer to the interface entry,
 *                  or NULL if the entry wasn't found.
 *
 * \return          FM_OK if the interface entry was found.
 * \return          FM_FM_ERR_INVALID_INTERFACE if the entry wasn't found.
 *
 *****************************************************************************/
fm_status fmFindInterface(fm_int                   sw,
                          fm_ipAddr *              interfaceAddr,
                          fm_intIpInterfaceEntry **ifEntry)
{
    fm_status                      err;
    fm_intIpInterfaceAddressEntry *addrEntry;

    err = fmFindInterfaceAddrEntry(sw, interfaceAddr, &addrEntry);
    if (err == FM_OK)
    {
        if (ifEntry != NULL)
        {
            if (addrEntry != NULL)
            {
                *ifEntry = addrEntry->ifEntry;
            }
            else
            {
                *ifEntry = NULL;
            }
        }
    }
    else if (ifEntry != NULL)
    {
        *ifEntry = NULL;
    }

    return err;

}   /* end fmFindInterface */




/*****************************************************************************/
/** fmGetInterface
 * \ingroup intRouterIf
 *
 * \desc            Given an interface number, validate that it is active and
 *                  return the pointer to the interface's entry.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       interface is the interface number
 *
 * \param[out]      ifEntry contains the pointer to the interface entry,
 *                  or NULL if the entry wasn't found.
 *
 * \return          FM_OK if the interface entry was found.
 * \return          FM_ERR_INVALID_INTERFACE if the entry wasn't found.
 *
 *****************************************************************************/
fm_status fmGetInterface(fm_int                   sw,
                         fm_int                   interface,
                         fm_intIpInterfaceEntry **ifEntry)
{
    fm_switch *switchPtr;
    fm_bool    ifIsInUse;
    fm_status  err;

    switchPtr = GET_SWITCH_PTR(sw);

    /* make sure the interface is in use */
    err = fmGetBitArrayBit(&switchPtr->ipInterfaceEntriesInUse,
                           interface,
                           &ifIsInUse);

    if (err != FM_OK)
    {
        return err;
    }

    if (!ifIsInUse)
    {
        return FM_ERR_INVALID_INTERFACE;
    }

    if (ifEntry != NULL)
    {
        *ifEntry = &switchPtr->ipInterfaceEntries[interface];
    }

    return err;

}   /* end fmGetInterface */




/*****************************************************************************/
/** fmFindArpEntry
 * \ingroup intRouterArp
 *
 * \desc            Find an ARP entry in the ARP table.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       arpAddr points to the ARP IP Address.
 *
 * \param[in]       vlan is the vlan number on which to filter the search.
 *
 * \param[out]      arpEntry contains the pointer to the ARP entry,
 *                  or NULL if the entry wasn't found.
 *
 * \return          FM_OK if the ARP entry was found.
 * \return          FM_ERR_NOT_FOUND if the entry wasn't found.
 *
 *****************************************************************************/
fm_status fmFindArpEntry(fm_int           sw,
                         fm_ipAddr *      arpAddr,
                         fm_uint16        vlan,
                         fm_intArpEntry **arpEntry)
{
    fm_switch *     switchPtr;
    fm_intArpEntry *entryPtr;
    fm_status       err;
    fm_intArpEntry  searchArp;
    fm_char         arpIPAddr[100];

    fmDbgConvertIPAddressToString(arpAddr, arpIPAddr);

    FM_LOG_ENTRY( FM_LOG_CAT_ROUTING,
                  "sw=%d, arpAddr=%p (%s), vlan=%u, arpEntry=%p\n",
                  sw,
                  (void *) arpAddr,
                  arpIPAddr,
                  vlan,
                  (void *) arpEntry );

    switchPtr = GET_SWITCH_PTR(sw);

    searchArp.arp.ipAddr = *arpAddr;
    searchArp.arp.vlan   = vlan;

    err = fmCustomTreeFind(&switchPtr->arpTree,
                           (void *) &searchArp,
                           (void **) &entryPtr);

    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, err);

    if (arpEntry != NULL)
    {
        *arpEntry = entryPtr;
        FM_LOG_DEBUG( FM_LOG_CAT_ROUTING,
                      "Found ARP, entryPtr=%p\n",
                      (void *) entryPtr );
    }

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, err);

}   /* end fmFindArpEntry */




/*****************************************************************************/
/** fmSetRouteActiveFlag
 * \ingroup intRouterRoute
 *
 * \desc            Determine whether a route should be active or not.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in,out]   routeEntry points to the route entry.  The "active" field
 *                  is updated by this function.
 *
 * \param[in]       updateHardware is TRUE if the router hardware should
 *                  be updated.  If TRUE, the route MUST have been already
 *                  added to the router hardware, i.e., switchPtr->AddRoute
 *                  must have already been called for this route.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmSetRouteActiveFlag(fm_int            sw,
                               fm_intRouteEntry *routeEntry,
                               fm_bool           updateHardware)
{
    fm_switch *             switchPtr;
    fm_bool                 active;
    fm_int                  vrid;
    fm_int                  vroff;
    fm_status               err;
    fm_bool                 wasActive;
    fm_intMulticastGroup *  group;
    fm_int                  ecmpGroupId;
    fm_intEcmpGroup *       ecmpGroup;

    FM_LOG_ENTRY(FM_LOG_CAT_ROUTING,
                 "sw = %d, routeEntry = %p, updateHardware = %d\n",
                 sw,
                 (void *) routeEntry,
                 (fm_int) updateHardware);

    switchPtr = GET_SWITCH_PTR(sw);

    wasActive = routeEntry->active;

    active = TRUE;

    switch (routeEntry->route.routeType)
    {
        case FM_ROUTE_TYPE_UNICAST:
        case FM_ROUTE_TYPE_UNICAST_ECMP:
            if (routeEntry->route.routeType == FM_ROUTE_TYPE_UNICAST)
            {
                vrid = routeEntry->route.data.unicast.vrid;
            }
            else
            {
                vrid = routeEntry->route.data.unicastECMP.vrid;
            }

            ecmpGroupId = routeEntry->ecmpGroupId;

            if (ecmpGroupId >= 0)
            {
                ecmpGroup = switchPtr->ecmpGroups[ecmpGroupId];

                if (!ecmpGroup->isUsable)
                {
                    active = FALSE;
                }
            }
            else if ( (routeEntry->action.action != FM_ROUTE_ACTION_NOP)
                && (routeEntry->action.action != FM_ROUTE_ACTION_RPF_FAILURE) )
            {
                active = FALSE;
            }
            break;

        case FM_ROUTE_TYPE_MULTICAST:
            vrid = 0;
            /* If the multicast group is not attached to an address,
             * don't route
             */
            group = fmFindMcastGroup(sw,
                                     routeEntry->route.data.multicast.mcastGroup);

            if (group == NULL)
            {
                active = FALSE;
            }
            break;

        default:
            active = FALSE;
            vrid   = 0;
            break;
    }

    err = fmValidateVirtualRouterId(sw, vrid, &vroff);

    if (err != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, err);
    }

    if (switchPtr->virtualRouterStates[vroff] != FM_ROUTER_STATE_ADMIN_UP)
    {
        active = FALSE;
    }

    if (routeEntry->state != FM_ROUTE_STATE_UP)
    {
        active = FALSE;
    }

    routeEntry->active = active;

    if ( updateHardware
        && (switchPtr->SetRouteActive != NULL)
        && (routeEntry->active != wasActive) )
    {
        err = switchPtr->SetRouteActive(sw, routeEntry);
    }
    else
    {
        err = FM_OK;
    }

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, err);

}   /* end fmSetRouteActiveFlag */




/*****************************************************************************/
/** fmDbgConvertIPAddressToString
 * \ingroup intDebug
 *
 * \desc            Convert an IP address into a string.
 *
 * \param[in]       ipAddr points to the IP Address to be converted.
 *
 * \param[in]       textOut points to a buffer for the string.  This buffer
 *                  must be large enough for the largest possible IP address
 *                  string.
 *
 * \return          nothing.
 *
 *****************************************************************************/
void fmDbgConvertIPAddressToString(const fm_ipAddr *ipAddr, fm_text textOut)
{
    fm_int    i;
    fm_uint32 segment1;
    fm_uint32 segment2;
    fm_char   tempString2[100];
    fm_uint   v4Word;
    fm_uint   v4Byte1;
    fm_uint   v4Byte2;
    fm_uint   v4Byte3;
    fm_uint   v4Byte4;
    fm_ipAddr tempAddr = *ipAddr;

    /**************************************************
     * For IPv6 address: 32 digits + 7 colons + 1 NUL = 40
     **************************************************/
    const fm_uint textOutLength = 40;

    /* First convert address to host byte order */
    if (ipAddr->isIPv6)
    {
        for (i = 3; i >= 0; i--)
        {
            tempAddr.addr[i] = ntohl(ipAddr->addr[i]);
        }
    }
    else
    {
        tempAddr.addr[0] = ntohl(ipAddr->addr[0]);
    }

    if (tempAddr.isIPv6)
    {
        *textOut = 0;

        for (i = 3 ; i >= 0 ; i--)
        {
            segment1 = (tempAddr.addr[i] >> 16) & 0xffff;
            segment2 = tempAddr.addr[i] & 0xffff;
            snprintf(tempString2, sizeof(tempString2),
                     "%04X:%04X", segment1, segment2);

            if (i != 0)
            {
                fmStringAppend(tempString2, ":", sizeof(tempString2));
            }

            fmStringAppend(textOut, tempString2, textOutLength);
        }
    }
    else
    {
        v4Word  = *( (fm_uint *) &tempAddr.addr[0] );
        v4Byte1 = (v4Word >> 24) & 0xFF;
        v4Byte2 = (v4Word >> 16) & 0xFF;
        v4Byte3 = (v4Word >> 8) & 0xFF;
        v4Byte4 = v4Word & 0xFF;
        snprintf(tempString2, sizeof(tempString2),
                 "%u.%u.%u.%u",
                 v4Byte1,
                 v4Byte2,
                 v4Byte3,
                 v4Byte4);
        fmStringCopy(textOut, tempString2, textOutLength);
    }

}   /* end fmDbgConvertIPAddressToString */




/*****************************************************************************/
/** fmDbgBuildMulticastDescription
 * \ingroup intDebug
 *
 * \desc            Convert a multicast address entry into a text string.
 *
 * \param[in]       mcastPtr points to the multicast address entry.
 *
 * \param[in]       textOut points to a 200-byte buffer for the string.
 *
 * \return          nothing.
 *
 *****************************************************************************/
void fmDbgBuildMulticastDescription(fm_multicastAddress *mcastPtr, fm_text textOut)
{
    fm_char       destAddr[40];
    fm_char       srcAddr[40];
    fm_ipAddr *   destIpAddr;
    fm_ipAddr *   srcIpAddr;
    fm_char       ipvFlag;
    fm_char       macAddr[40];
    const fm_uint textOutLength = 200;

    switch (mcastPtr->addressType)
    {
        case FM_MCAST_ADDR_TYPE_UNKNOWN:
            snprintf(textOut,
                     textOutLength,
                     "Multicast Address Type == UNKNOWN");
            break;

        case FM_MCAST_ADDR_TYPE_L2MAC_VLAN:
            fmDbgConvertMacAddressToString(
                mcastPtr->info.mac.destMacAddress, macAddr);
            snprintf(textOut,
                     textOutLength,
                     "MAC/VLAN: macAddr=%s, vlan=%d/vlan2=%d",
                     macAddr,
                     mcastPtr->info.mac.vlan,
                     mcastPtr->info.mac.vlan2);
            break;

        case FM_MCAST_ADDR_TYPE_DSTIP:
            destIpAddr = &mcastPtr->info.dstIpRoute.dstAddr;

            if (destIpAddr->isIPv6)
            {
                ipvFlag = '6';
            }
            else
            {
                ipvFlag = '4';
            }

            fmDbgConvertIPAddressToString(destIpAddr, destAddr);

            snprintf(textOut,
                     textOutLength,
                     "IPv%cM: dest=%s/%d",
                     ipvFlag,
                     destAddr,
                     mcastPtr->info.dstIpRoute.dstPrefixLength);
            break;

        case FM_MCAST_ADDR_TYPE_DSTIP_VLAN:
            destIpAddr = &mcastPtr->info.dstIpVlanRoute.dstAddr;

            if (destIpAddr->isIPv6)
            {
                ipvFlag = '6';
            }
            else
            {
                ipvFlag = '4';
            }

            fmDbgConvertIPAddressToString(destIpAddr, destAddr);

            snprintf(textOut,
                     textOutLength,
                     "IPv%cM: dest=%s/%d vlan=%d/%d",
                     ipvFlag,
                     destAddr,
                     mcastPtr->info.dstIpVlanRoute.dstPrefixLength,
                     mcastPtr->info.dstIpVlanRoute.vlan,
                     mcastPtr->info.dstIpVlanRoute.vlanPrefixLength);
            break;

        case FM_MCAST_ADDR_TYPE_DSTIP_SRCIP:
            destIpAddr = &mcastPtr->info.dstSrcIpRoute.dstAddr;
            srcIpAddr  = &mcastPtr->info.dstSrcIpRoute.srcAddr;

            if (destIpAddr->isIPv6)
            {
                ipvFlag = '6';
            }
            else
            {
                ipvFlag = '4';
            }

            fmDbgConvertIPAddressToString(destIpAddr, destAddr);
            fmDbgConvertIPAddressToString(srcIpAddr, srcAddr);

            snprintf(textOut,
                     textOutLength,
                     "IPv%cM: dest=%s/%d src=%s/%d",
                     ipvFlag,
                     destAddr,
                     mcastPtr->info.dstSrcIpRoute.dstPrefixLength,
                     srcAddr,
                     mcastPtr->info.dstSrcIpRoute.srcPrefixLength);
            break;

        case FM_MCAST_ADDR_TYPE_DSTIP_SRCIP_VLAN:
            destIpAddr = &mcastPtr->info.dstSrcIpVlanRoute.dstAddr;
            srcIpAddr  = &mcastPtr->info.dstSrcIpVlanRoute.srcAddr;

            if (destIpAddr->isIPv6)
            {
                ipvFlag = '6';
            }
            else
            {
                ipvFlag = '4';
            }

            fmDbgConvertIPAddressToString(destIpAddr, destAddr);
            fmDbgConvertIPAddressToString(srcIpAddr, srcAddr);

            snprintf(textOut,
                     textOutLength,
                     "IPv%cM: dest=%s/%d src=%s/%d vlan=%d/%d",
                     ipvFlag,
                     destAddr,
                     mcastPtr->info.dstSrcIpVlanRoute.dstPrefixLength,
                     srcAddr,
                     mcastPtr->info.dstSrcIpVlanRoute.srcPrefixLength,
                     mcastPtr->info.dstSrcIpVlanRoute.vlan,
                     mcastPtr->info.dstSrcIpVlanRoute.vlanPrefixLength);
            break;

        default:
            snprintf(textOut,
                     textOutLength,
                     "Unknown Multicast Address Type %d",
                     mcastPtr->addressType);
            break;

    }   /* end switch (mcastPtr->addressType) */

}   /* end fmDbgBuildMulticastDescription */




/*****************************************************************************/
/** fmDbgBuildRouteDescription
 * \ingroup intDebug
 *
 * \desc            Convert a route entry into a text string.
 *
 * \param[in]       route points to the route entry.
 *
 * \param[in]       textOut points to a 200-byte buffer for the string.
 *
 * \return          nothing.
 *
 *****************************************************************************/
void fmDbgBuildRouteDescription(fm_routeEntry *route, fm_text textOut)
{
    fm_char                   destAddr[40];
    fm_char                   nextHop[40];
    fm_char                   intfIP[40];
    fm_unicastRouteEntry *    unicast;
    fm_unicastECMPRouteEntry *unicastEcmp;
    fm_ipAddr                 destIpAddr;
    fm_char                   ipvFlag;
    const fm_uint             textOutLength = 200;

    fmGetRouteDestAddress(route, &destIpAddr);

    if (destIpAddr.isIPv6)
    {
        ipvFlag = '6';
    }
    else
    {
        ipvFlag = '4';
    }

    if (route->routeType == FM_ROUTE_TYPE_UNICAST)
    {
        fmDbgConvertIPAddressToString(&destIpAddr, destAddr);

        unicast = &route->data.unicast;
        fmDbgConvertIPAddressToString(&unicast->nextHop, nextHop);
        fmDbgConvertIPAddressToString(&unicast->interfaceAddr, intfIP);

        snprintf(textOut, textOutLength,
                "IPv%cU: dest=%s/%d nh=%s if=%s vlan=%u vrid=%d",
                ipvFlag,
                destAddr,
                unicast->prefixLength,
                nextHop,
                intfIP,
                unicast->vlan,
                unicast->vrid);
    }

    else if (route->routeType == FM_ROUTE_TYPE_UNICAST_ECMP)
    {
        fmDbgConvertIPAddressToString(&destIpAddr, destAddr);

        unicastEcmp = &route->data.unicastECMP;

        snprintf(textOut, textOutLength,
                "IPv%cU: dest=%s/%d ecmpGroup=%d vrid=%d",
                ipvFlag,
                destAddr,
                unicastEcmp->prefixLength,
                unicastEcmp->ecmpGroup,
                unicastEcmp->vrid);
    }

    else
    {
        fmDbgBuildMulticastDescription(&route->data.multicast, textOut);
    }

}   /* end fmDbgBuildRouteDescription */




/*****************************************************************************/
/** fmConvertPrefixLengthToDestMask
 * \ingroup intRouter
 *
 * \desc            Convert a destination route prefix length into a
 *                  destination mask.
 *
 * \param[in]       prefixLength is the desired prefix length.
 *
 * \param[in]       isIPv6 is TRUE if the destination mask needs to be
 *                  set for an IPV6 address, FALSE for an IPV4 address.
 *
 * \param[out]      destMask points to a buffer for the destination mask.
 *                  For IPV6, this must be 128 bits long, for IPv4, 32 bits.
 *
 * \return          nothing.
 *
 *****************************************************************************/
void fmConvertPrefixLengthToDestMask(fm_int     prefixLength,
                                     fm_bool    isIPv6,
                                     fm_uint32 *destMask)
{
    fm_int curOffset;
    fm_int curLen;

    if (isIPv6)
    {
        curOffset = 3;

        if (prefixLength > FM_IPV6_MAX_PREFIX_LENGTH)
        {
            prefixLength = FM_IPV6_MAX_PREFIX_LENGTH;
        }
    }
    else
    {
        curOffset = 0;

        if (prefixLength > FM_IPV4_MAX_PREFIX_LENGTH)
        {
            prefixLength = FM_IPV4_MAX_PREFIX_LENGTH;
        }
    }

    while (prefixLength > 0)
    {
        if (prefixLength > 32)
        {
            curLen = 32;
        }
        else
        {
            curLen = prefixLength;
        }

        destMask[curOffset] = ~0 & ~( ( 1 << (32 - curLen) ) - 1 );
        prefixLength       -= curLen;
        curOffset--;
    }

    while (curOffset >= 0)
    {
        destMask[curOffset--] = 0;
    }

}   /* end fmConvertPrefixLengthToDestMask */




/*****************************************************************************/
/** fmMaskIPAddress
 * \ingroup intRouter
 *
 * \desc            Masks an IP Address, given the desired prefix length.
 *
 * \param[in,out]   ipAddr points to the IP address to be masked.
 *
 * \param[in]       prefixLength is the desired prefix length.
 *
 * \return          nothing.
 *
 *****************************************************************************/
void fmMaskIPAddress(fm_ipAddr *ipAddr, fm_int prefixLength)
{
    fm_int    i;
    fm_int    curPrefix;
    fm_uint64 mask64;
    fm_uint32 mask;
    fm_ipAddr tempAddr;
    fm_int    j;

    if (ipAddr->isIPv6)
    {

        if (prefixLength > FM_IPV6_MAX_PREFIX_LENGTH)
        {
            prefixLength = FM_IPV6_MAX_PREFIX_LENGTH;
        }

        curPrefix = prefixLength;

        for (i = 3, j = 0 ; i >= 0 ; i--, j++)
        {
            tempAddr.addr[i] = ntohl(ipAddr->addr[j]);
        }

        for (i = 0 ; i < 4 ; i++)
        {
            if (curPrefix >= 32)
            {
                mask = ~0;
                curPrefix -= 32;
            }
            else if (curPrefix > 0)
            {
                mask64 = ~( ( 1LL << (32 - curPrefix) ) - 1 );
                mask = (fm_uint32) mask64;
                curPrefix = 0;
            }
            else
            {
                mask = 0;
            }

            tempAddr.addr[i] &= mask;
        }

        for (i = 3, j = 0 ; i >= 0 ; i--, j++)
        {
            ipAddr->addr[i] = htonl(tempAddr.addr[j]);
        }
    }
    else
    {
        if (prefixLength > FM_IPV4_MAX_PREFIX_LENGTH)
        {
            prefixLength = FM_IPV4_MAX_PREFIX_LENGTH;
        }

        mask64 = ~( ( 1LL << (32 - prefixLength) ) - 1 );
        mask = (fm_uint32) mask64;
        ipAddr->addr[0] &= htonl(mask);
        ipAddr->addr[1] = 0;
        ipAddr->addr[2] = 0;
        ipAddr->addr[3] = 0;
    }

}   /* end fmMaskIPAddress */




/*****************************************************************************/
/** fmApplyMasksToRoute
 * \ingroup intRouter
 *
 * \desc            Applies prefix masks to route values so that they
 *                  are all internally consistent. For instance, an IP
 *                  address such as 1.1.1.5/24 would be masked to 1.1.1.0/24.
 *
 * \param[in]       route points to the route entry.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmApplyMasksToRoute(fm_routeEntry *route)
{
    fm_status status;

    FM_LOG_ENTRY( FM_LOG_CAT_ROUTING, "route = %p\n", (void *) route );

    switch (route->routeType)
    {
        case FM_ROUTE_TYPE_UNICAST:
            fmMaskIPAddress(&route->data.unicast.dstAddr,
                            route->data.unicast.prefixLength);
            break;

        case FM_ROUTE_TYPE_UNICAST_ECMP:
            fmMaskIPAddress(&route->data.unicastECMP.dstAddr,
                            route->data.unicastECMP.prefixLength);
            break;

        case FM_ROUTE_TYPE_MULTICAST:
            status = fmApplyMasksToMulticastAddress(&route->data.multicast);
            FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, status);
            break;

        default:
            FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_OK);

}   /* end fmApplyMasksToRoute */




/*****************************************************************************/
/** fmGetVirtualRouterOffset
 * \ingroup intRouter
 *
 * \desc            Finds a virtual router id in the table and returns its
 *                  offset.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       vrid is the virtual router id number.
 *
 * \return          offset into the virtual router table, -1 if not found.
 *
 *****************************************************************************/
fm_int fmGetVirtualRouterOffset(fm_int sw, fm_int vrid)
{
    fm_switch *switchPtr;
    fm_int     vroff;

    switchPtr = GET_SWITCH_PTR(sw);

    if (vrid == FM_ROUTER_ANY)
    {
        return 0;
    }

    for (vroff = 0 ; vroff < switchPtr->maxVirtualRouters ; vroff++)
    {
        if (switchPtr->virtualRouterIds[vroff] == vrid)
        {
            break;
        }
    }

    if (vroff >= switchPtr->maxVirtualRouters)
    {
        vroff = -1;
    }

    return vroff;

}   /* end fmGetVirtualRouterOffset */




/*****************************************************************************/
/** fmDbgDumpRouteStats
 * \ingroup diagMisc
 *
 * \desc            Dump Router information (short version - see
 *                  ''fmDbgDumpRouteTables'' for the verbose version).
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          FM_OK if successful
 * \return          FM_ERR_INVALID_SWITCH if the switch ID is invalid
 * \return          FM_ERR_SWITCH_NOT_UP  if the switch is  not running
 *
 *****************************************************************************/
fm_status fmDbgDumpRouteStats(fm_int sw)
{
    fm_switch *switchPtr;

    VALIDATE_AND_PROTECT_SWITCH( sw );

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY_VOID(switchPtr->DbgDumpRouteStats, sw);

    UNPROTECT_SWITCH( sw );
    return FM_OK;

}   /* end fmDbgDumpRouteStats */




/*****************************************************************************/
/** fmDbgDumpRouteTables
 * \ingroup diagMisc
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Dump Router information (verbose version - see
 *                  ''fmDbgDumpRouteStats'' for the short version).
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       flags contains a bit-field describing which internal
 *                  tables to dump.  See ''Router Debug Dump Flags'' for
 *                  a list of flag values.
 *
 * \return          FM_OK if successful
 * \return          FM_ERR_INVALID_SWITCH if the switch ID is invalid
 * \return          FM_ERR_SWITCH_NOT_UP  if the switch is  not running
 * 
 *
 *****************************************************************************/
fm_status fmDbgDumpRouteTables(fm_int sw, fm_int flags)
{
    fm_switch *switchPtr;

    VALIDATE_AND_PROTECT_SWITCH( sw );

    switchPtr = GET_SWITCH_PTR( sw );

    FM_API_CALL_FAMILY_VOID(switchPtr->DbgDumpRouteTables, sw, flags);

    UNPROTECT_SWITCH( sw );
    return FM_OK;

}   /* end fmDbgDumpRouteTables */




/*****************************************************************************/
/** fmDbgDumpArpTable
 * \ingroup diagMisc
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Display the Cache/Hardware Glort Cam table.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       verbose indicates whether to include the first 20 entries,
 *                  even if they are all zeros.
 *
 * \return          FM_OK if successful
 * \return          FM_ERR_INVALID_SWITCH if the switch ID is invalid
 * \return          FM_ERR_SWITCH_NOT_UP  if the switch is  not running
 * 
 *
 *****************************************************************************/
fm_status fmDbgDumpArpTable(fm_int sw, fm_bool verbose)
{
    fm_switch *switchPtr;

    VALIDATE_AND_PROTECT_SWITCH( sw );

    switchPtr = GET_SWITCH_PTR( sw );

    FM_API_CALL_FAMILY_VOID(switchPtr->DbgDumpArpTable, sw, verbose);

    UNPROTECT_SWITCH( sw );
    return FM_OK;

}   /* end fmDbgDumpArpTable */




/*****************************************************************************/
/** fmDbgConvertStringToIPAddress
 * \ingroup diagMisc
 *
 * \desc            Function to convert an IP Address string into ''fm_ipAddr''
 *                  format.
 *
 * \param[in]       ipString points to the string.
 *
 * \param[in]       forceIPv6 is TRUE to force an IPv6 address, even if the
 *                  string only contains an IPv4 address.
 *
 * \param[in]       ipAddr points to a structure which will be initialized
 *                  by this function.
 *
 * \return          TRUE if the address was successfully converted.
 * \return          FALSE if the address string could not be parsed.
 *
 *****************************************************************************/
fm_bool fmDbgConvertStringToIPAddress(const fm_char *ipString,
                                      fm_bool        forceIPv6,
                                      fm_ipAddr *    ipAddr)
{
    fm_bool   isIPv6         = FALSE;
    fm_bool   hasIPv4Suffix  = FALSE;
    fm_bool   hasDoubleColon = FALSE;
    fm_char   tempString[200];
    fm_int    i;
    fm_text   ptr;
    fm_uint32 tempIpv4;
    fm_int    segCount;
    fm_int    dblColonSeg;
    fm_int    dblColonSegLen = 0;
    fm_uint16 segments[8];
    fm_bool   haveSegDigit;
    fm_byte   cur;
    fm_text   lastColonPtr;
    fm_int    segOffset;
    fm_int    segVal;
    fm_bool   lastCharWasColon;

    FM_CLEAR(*ipAddr);

    i = strlen(ipString);

    if ( i >= (fm_int) sizeof(tempString) )
    {
        return FALSE;
    }

    fmStringCopy(tempString, ipString, sizeof(tempString));
    ptr = tempString;

    if (forceIPv6)
    {
        ipAddr->isIPv6 = TRUE;
    }

    if (strchr(tempString, ':') != NULL)
    {
        isIPv6 = TRUE;
    }

    if (strchr(tempString, '.') != NULL)
    {
        hasIPv4Suffix = TRUE;
    }

    if (isIPv6)
    {
        ipAddr->isIPv6 = TRUE;
        segCount       = 0;
        dblColonSeg    = -1;
        FM_CLEAR(segments);
        haveSegDigit     = FALSE;
        lastColonPtr     = NULL;
        lastCharWasColon = FALSE;

        while (1)
        {
            cur = *ptr;

            if (cur == 0)
            {
                if (haveSegDigit)
                {
                    segCount++;
                }

                break;
            }

            if ( isxdigit(cur) )
            {
                if (segCount >= 8)
                {
                    return FALSE;
                }

                if (cur <= '9')
                {
                    cur -= '0';
                }
                else if (cur <= 'F')
                {
                    cur = cur - 'A' + 10;
                }
                else
                {
                    cur = cur - 'a' + 10;
                }

                segments[segCount] = (segments[segCount] << 4) | cur;
                haveSegDigit       = TRUE;
            }
            else if (cur == ':')
            {
                lastColonPtr = ptr;

                if (haveSegDigit)
                {
                    segCount++;
                    haveSegDigit     = FALSE;
                    lastCharWasColon = TRUE;
                }
                else if (lastCharWasColon)
                {
                    if (hasDoubleColon)
                    {
                        return FALSE;
                    }

                    hasDoubleColon = TRUE;
                    dblColonSeg    = segCount;
                    segCount++;
                }
                else
                {
                    lastCharWasColon = TRUE;
                }
            }
            else if (cur == '.')
            {
                if (haveSegDigit)
                {
                    ptr = lastColonPtr + 1;
                    break;
                }
            }

            ptr++;
        }

        if (hasDoubleColon)
        {
            dblColonSegLen = 8 - segCount;
            segCount       = 8;
        }

        if (segCount != 8)
        {
            if (!hasIPv4Suffix || segCount != 6)
            {
                return FALSE;
            }
        }

        segCount  = 0;
        segOffset = 0;

        while (segCount < 8)
        {
            i = segCount / 2;

            ipAddr->addr[i] = (ipAddr->addr[i] << 16) | segments[segOffset];
 
            if (segCount == dblColonSeg)
            {
                segCount += dblColonSegLen;
            }

            segCount++;
            segOffset++;
        }
    }

    if (hasIPv4Suffix)
    {
        tempIpv4     = 0;
        segCount     = 0;
        haveSegDigit = FALSE;
        segVal       = 0;

        while ( (cur = *ptr++) != 0 )
        {
            if ( isdigit(cur) )
            {
                if (segCount >= 4)
                {
                    return FALSE;
                }

                cur         -= '0';
                segVal       = (segVal * 10) + cur;
                haveSegDigit = TRUE;
            }
            else if (cur == '.')
            {
                if (haveSegDigit)
                {
                    if (segVal > 255)
                    {
                        return FALSE;
                    }

                    tempIpv4 |= segVal << ( 24 - (segCount * 8) );
                }

                haveSegDigit = FALSE;
                segCount++;
                segVal = 0;
            }
            else
            {
                return FALSE;
            }
        }

        if (haveSegDigit)
        {
            if (segVal > 255)
            {
                return FALSE;
            }

            tempIpv4 |= segVal << ( 24 - (segCount * 8) );
        }

        if (!ipAddr->isIPv6)
        {
            ipAddr->addr[0] = tempIpv4;
        }
        else
        {
            ipAddr->addr[3] = tempIpv4;
        }
    }

    /* Now convert address to network byte order */
    if (ipAddr->isIPv6)
    {
        fm_ipAddr tempAddr = *ipAddr;
        fm_int    i;
        fm_int    j;

        for (i = 3, j = 0 ; i >= 0 ; i--, j++)
        {
            ipAddr->addr[i] = htonl(tempAddr.addr[j]);
        }
    }
    else
    {
        ipAddr->addr[0] = htonl(ipAddr->addr[0]);
    }

    return TRUE;

}   /* end fmDbgConvertStringToIPAddress */




/*****************************************************************************/
/** fmDbgDumpRouteLookupTrees
 * \ingroup diagMisc
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Dumps out the contents of the route lookup tables for
 *                  a virtual router.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       vrid is the virtual router ID number.
 *
 * \return          FM_OK if successful
 * \return          FM_ERR_INVALID_SWITCH if the switch ID is invalid
 * \return          FM_ERR_SWITCH_NOT_UP  if the switch is not running
 *
 *
 *****************************************************************************/
fm_status fmDbgDumpRouteLookupTrees(fm_int sw, fm_int vrid)
{
    fm_status             status;
    fm_int                prefix;
    fm_customTree *       lookupTree;
    fm_customTreeIterator iter;
    fm_ipAddr *           routeIP;
    fm_intRouteEntry *    route;
    fm_char               routeDesc[1000];

    VALIDATE_AND_PROTECT_SWITCH( sw );

    for (prefix = FM_MAX_NUM_IP_PREFIXES - 1 ; prefix >= 0 ; prefix--)
    {
        status = fmGetRouteLookupTree(sw, vrid, prefix, &lookupTree);
        if (status != FM_OK)
        {
            FM_LOG_PRINT( "Error getting route lookup tree for prefix %d: %s\n",
                          prefix,
                          fmErrorMsg(status) );
            break;
        }

        if ( fmCustomTreeSize(lookupTree) != 0 )
        {
            fmCustomTreeIterInit(&iter, lookupTree);

            FM_LOG_PRINT("\nPrefix %d\n", prefix);

            while (1)
            {
                status = fmCustomTreeIterNext( &iter,
                                               (void **) &routeIP,
                                               (void **) &route );
                if (status == FM_ERR_NO_MORE)
                {
                    break;
                }
                else if (status != FM_OK)
                {
                    FM_LOG_PRINT( "Unexpected error from route lookup tree "
                                  "iterator for prefix %d: %s\n",
                                  prefix,
                                  fmErrorMsg(status) );
                }
                else
                {
                    fmDbgBuildRouteDescription(&route->route, routeDesc);
                    FM_LOG_PRINT("    %s\n", routeDesc);
                }
            }
        }
    }

    UNPROTECT_SWITCH( sw );

    return FM_OK;

}   /* end fmDbgDumpRouteLookupTrees */




/*****************************************************************************/
/** fmDbgDumpRouteForIP
 * \ingroup diagMisc
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Determines which route services a specified IP
 *                  address and dumps the route information to the
 *                  log.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       vrid is the virtual router ID number.
 *
 * \param[in]       ipAddr contains the IP address as a string.
 *
 * \return          FM_OK if successful
 * \return          FM_ERR_INVALID_SWITCH if the switch ID is invalid
 * \return          FM_ERR_SWITCH_NOT_UP  if the switch is  not running
 *
 *
 *****************************************************************************/
fm_status fmDbgDumpRouteForIP(fm_int sw, fm_int vrid, fm_text ipAddr)
{
    fm_ipAddr         ip;
    fm_status         status;
    fm_intRouteEntry *route;
    fm_char           routeDesc[1000];

    VALIDATE_AND_PROTECT_SWITCH( sw );

    fmDbgConvertStringToIPAddress(ipAddr, FALSE, &ip);

    status = fmGetIntRouteForIP(sw, vrid, &ip, &route);
    if (status != FM_OK)
    {
        FM_LOG_PRINT( "fmGetIntRouteForIP returned error %s\n", fmErrorMsg(status) );
    }
    else
    {
        fmDbgBuildRouteDescription(&route->route, routeDesc);
        FM_LOG_PRINT("%s\n", routeDesc);
    }

    UNPROTECT_SWITCH( sw );

    return FM_OK;

}   /* end fmDbgDumpRouteForIP */




/*****************************************************************************/
/** fmDbgValidateRouteTables
 * \ingroup intDebug
 *
 * \desc            Function to validate internal consistency of the routing
 *                  tables.
 *
 * \param[in]       sw contains the switch number.
 *
 * \return          FM_OK if the route tables are internally consistent.
 * \return          FM_FAIL if the route tables are not internally consistent.
 *
 *****************************************************************************/
fm_status fmDbgValidateRouteTables(fm_int sw)
{
    fm_switch *             switchPtr;
    fm_status               err = FM_FAIL;
    fm_int                  numRoutes;
    fm_intRouteEntry *      prevRoute;
    fm_intRouteEntry *      curRoute;
    fm_intRouteEntry *      routeKey;
    fm_customTreeIterator   iter;
    fm_int                  i;
    fm_char                 curRouteDesc[500];
    fm_char                 prevRouteDesc[500];
    fm_int                  index;
    fm_bool                 bitValue;
    fm_int                  numArps;
    fm_intArpEntry *        prevArp;
    fm_intArpEntry *        curArp;
    fm_intArpEntry *        arpKey;
    fm_intArpEntry *        arpValue;
    fm_char                 tempText[200];
    fm_char                 tempText2[200];
    fm_char                 curArpDesc[500];
    fm_char                 prevArpDesc[500];
    fm_intIpInterfaceEntry *ipIfEntry;

    FM_LOG_ENTRY(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    /**********************************************************************
     *  Validate the Route Table
     *********************************************************************/
    if (switchPtr->maxRoutes > 0)
    {
        err = fmCustomTreeValidate(&switchPtr->routeTree);

        if (err != FM_OK)
        {
            FM_LOG_PRINT( "fmDbgValidateRouteTables Validation Failed: "
                          "route custom tree validation returned "
                          "error %d (%s)\n",
                         err,
                         fmErrorMsg(err) );
            goto ABORT;
        }

        i = fmCustomTreeSize(&switchPtr->routeTree);

        if ( (i < 0) || (i > switchPtr->maxRoutes) )
        {
            FM_LOG_PRINT("fmDbgValidateRouteTables Validation Failed: "
                         "custom tree contains %d routes, max = %d\n",
                         i,
                         switchPtr->maxRoutes);
            goto ABORT;
        }

        err = fmCustomTreeValidate(&switchPtr->ecmpRouteTree);

        if (err != FM_OK)
        {
            FM_LOG_PRINT( "fmDbgValidateRouteTables Validation Failed: "
                          "ecmp route custom tree validation returned "
                          "error %d (%s)\n",
                         err,
                         fmErrorMsg(err) );
            goto ABORT;
        }

        i = fmCustomTreeSize(&switchPtr->ecmpRouteTree);

        if ( (i < 0) || (i > switchPtr->maxRoutes) )
        {
            FM_LOG_PRINT("fmDbgValidateRouteTables Validation Failed: "
                         "ecmp custom tree contains %d routes, max = %d\n",
                         i,
                         switchPtr->maxRoutes);
            goto ABORT;
        }

        fmCustomTreeIterInit(&iter, &switchPtr->routeTree);
        numRoutes = 0;
        err       = fmCustomTreeIterNext(&iter,
                                         (void **) &routeKey,
                                         (void **) &curRoute);
        prevRoute = NULL;

        while ( (err == FM_OK) && (curRoute != NULL) )
        {
            if (numRoutes > switchPtr->maxRoutes)
            {
                FM_LOG_PRINT("fmDbgValidateRouteTables Validation Failed: "
                             "too many routes, max = %d\n",
                             switchPtr->maxRoutes);
                goto ABORT;
            }

            if (err != FM_OK)
            {
                FM_LOG_PRINT("fmDbgValidateRouteTables Validation Failed: "
                             "route %d fmCustomTreeIterNext err = %d (%s), "
                             "curRoute = %p\n",
                             numRoutes,
                             err,
                             fmErrorMsg(err),
                             (void *) curRoute);
                goto ABORT;
            }

            if (curRoute != routeKey)
            {
                FM_LOG_PRINT("fmDbgValidateRouteTables Validation Failed: "
                             "route %d, custom tree key = %p value = %p\n",
                             numRoutes,
                             (void *) routeKey,
                             (void *) curRoute);
                goto ABORT;
            }

            if (prevRoute != NULL)
            {
                i = CompareRoutes(&prevRoute->route, &curRoute->route, FALSE);

                if (i == 0)
                {
                    fmDbgBuildRouteDescription(&prevRoute->route,
                                               prevRouteDesc);
                    fmDbgBuildRouteDescription(&curRoute->route,
                                               curRouteDesc);
                    FM_LOG_PRINT("fmDbgValidateRouteTables Validation Failed: "
                                 "route %d, invalid sort order\n"
                                 "    previous route = %s\n"
                                 "    current route  = %s\n",
                                 numRoutes,
                                 prevRouteDesc,
                                 curRouteDesc);
                    goto ABORT;
                }
                else if (i > 0)
                {
                    fmDbgBuildRouteDescription(&prevRoute->route,
                                               prevRouteDesc);
                    fmDbgBuildRouteDescription(&curRoute->route,
                                               curRouteDesc);
                    FM_LOG_PRINT("fmDbgValidateRouteTables Validation Failed: "
                                 "route %d, invalid sort order\n"
                                 "    previous route = %s\n"
                                 "    current route  = %s\n",
                                 numRoutes,
                                 prevRouteDesc,
                                 curRouteDesc);
                    goto ABORT;
                }
            }

            /* move to the next route */
            numRoutes++;
            prevRoute = curRoute;
            err       = fmCustomTreeIterNext(&iter,
                                             (void **) &routeKey,
                                             (void **) &curRoute);
        }

        if (err == FM_OK)
        {
            FM_LOG_PRINT("fmDbgValidateRouteTables Validation Failed: "
                         "end of route linked list with %d routes, but "
                         "custom tree contains additional routes\n",
                         numRoutes);
            goto ABORT;
        }

    }

    /* end if (switchPtr->maxRoutes > 0) */


    /**********************************************************************
     *  Validate the ARP Table
     *********************************************************************/
    if (switchPtr->maxArpEntries > 0)
    {
        err = fmCustomTreeValidate(&switchPtr->arpTree);

        if (err != FM_OK)
        {
            FM_LOG_PRINT( "fmDbgValidateRouteTables Validation Failed: "
                          "arp custom tree validation returned error "
                          "%d (%s)\n",
                         err,
                         fmErrorMsg(err) );
            goto ABORT;
        }

        i = fmCustomTreeSize(&switchPtr->arpTree);

        if ( (i < 0) || (i > switchPtr->maxArpEntries) )
        {
            FM_LOG_PRINT("fmDbgValidateRouteTables Validation Failed: "
                         "custom tree contains %d ARPs, max = %d\n",
                         i,
                         switchPtr->maxArpEntries);
            goto ABORT;
        }

        fmCustomTreeIterInit(&iter, &switchPtr->arpTree);
        numArps = 0;
        curArp  = fmGetFirstArp(switchPtr);
        err     = fmCustomTreeIterNext(&iter,
                                       (void **) &arpKey,
                                       (void **) &arpValue);
        prevArp = NULL;

        while (curArp != NULL)
        {
            if (numArps > switchPtr->maxArpEntries)
            {
                FM_LOG_PRINT("fmDbgValidateRouteTables Validation Failed: "
                             "too many ARPs, max = %d\n",
                             switchPtr->maxArpEntries);
                goto ABORT;
            }

            if (err != FM_OK)
            {
                FM_LOG_PRINT("fmDbgValidateRouteTables Validation Failed: "
                             "arp %d fmCustomTreeIterNext err = %d (%s), "
                             "curArp = %p\n",
                             numArps,
                             err,
                             fmErrorMsg(err),
                             (void *) curArp);
                goto ABORT;
            }

            if ( (curArp != arpKey) || (curArp != arpValue) )
            {
                FM_LOG_PRINT("fmDbgValidateRouteTables Validation Failed: "
                             "arp %d, linked list = %p, custom tree "
                             "key = %p value = %p\n",
                             numArps,
                             (void *) curArp,
                             (void *) arpKey,
                             (void *) arpValue);
                goto ABORT;
            }

            if (prevArp != NULL)
            {
                i = fmCompareInternalArps(&prevArp->arp, &curArp->arp);

                if (i == 0)
                {
                    fmDbgConvertIPAddressToString(&prevArp->arp.ipAddr,
                                                  tempText);
                    fmDbgConvertMacAddressToString(prevArp->arp.macAddr,
                                                   tempText2);
                    snprintf(prevArpDesc, sizeof(prevArpDesc),
                             "addr = %s, interface = %d, vlan = %u, "
                             "macAddr = %s",
                             tempText,
                             prevArp->arp.interface,
                             (fm_uint) prevArp->arp.vlan,
                             tempText2);

                    fmDbgConvertIPAddressToString(&curArp->arp.ipAddr,
                                                  tempText);
                    fmDbgConvertMacAddressToString(curArp->arp.macAddr,
                                                   tempText2);
                    snprintf(curArpDesc, sizeof(curArpDesc),
                             "addr = %s, interface = %d, vlan = %u, "
                             "macAddr = %s",
                             tempText,
                             curArp->arp.interface,
                             (fm_uint) curArp->arp.vlan,
                             tempText2);

                    FM_LOG_PRINT("fmDbgValidateRouteTables Validation Failed: "
                                 "arp %d, invalid sort order\n"
                                 "    previous arp = %s\n"
                                 "    current arp  = %s\n",
                                 numArps,
                                 prevArpDesc,
                                 curArpDesc);
                    goto ABORT;
                }
                else if (i > 0)
                {
                    fmDbgConvertIPAddressToString(&prevArp->arp.ipAddr,
                                                  tempText);
                    fmDbgConvertMacAddressToString(prevArp->arp.macAddr,
                                                   tempText2);
                    snprintf(prevArpDesc, sizeof(prevArpDesc),
                             "addr = %s, interface = %d, vlan = %u, "
                             "macAddr = %s",
                             tempText,
                             prevArp->arp.interface,
                             (fm_uint) prevArp->arp.vlan,
                             tempText2);

                    fmDbgConvertIPAddressToString(&curArp->arp.ipAddr,
                                                  tempText);
                    fmDbgConvertMacAddressToString(curArp->arp.macAddr,
                                                   tempText2);
                    snprintf(curArpDesc, sizeof(curArpDesc),
                             "addr = %s, interface = %d, vlan = %u, "
                             "macAddr = %s",
                             tempText,
                             curArp->arp.interface,
                             (fm_uint) curArp->arp.vlan,
                             tempText2);

                    FM_LOG_PRINT("fmDbgValidateRouteTables Validation Failed: "
                                 "arp %d, invalid sort order\n"
                                 "    previous arp = %s\n"
                                 "    current arp  = %s\n",
                                 numArps,
                                 prevArpDesc,
                                 curArpDesc);
                    goto ABORT;
                }
            }

            /* move to the next arp */
            numArps++;
            prevArp = curArp;
            curArp  = fmGetNextArp(curArp);
            err     = fmCustomTreeIterNext(&iter,
                                           (void **) &arpKey,
                                           (void **) &arpValue);
        }

        if (err == FM_OK)
        {
            FM_LOG_PRINT("fmDbgValidateRouteTables Validation Failed: "
                         "end of ARP linked list with %d ARPs, but "
                         "custom tree contains additional ARPs\n",
                         numArps);
            goto ABORT;
        }

    }

    /* end if (switchPtr->maxArpEntries > 0) */


    /**********************************************************************
     *  Validate the Interface Table
     *********************************************************************/
    if (switchPtr->maxIpInterfaces > 0)
    {
        if (switchPtr->ipInterfaceEntries == NULL)
        {
            FM_LOG_PRINT("fmDbgValidateRouteTables Validation Failed: "
                         "maxIpInterfaces = %d but ipInterfaceEntries == "
                         "NULL\n",
                         switchPtr->maxIpInterfaces);
            goto ABORT;
        }

        for (index = 0 ; index < switchPtr->maxIpInterfaces ; index++)
        {
            ipIfEntry = switchPtr->ipInterfaceEntries + index;

            /* Retrieve the bit from the entries-in-use array */
            err = fmGetBitArrayBit(&switchPtr->ipInterfaceEntriesInUse,
                                   index,
                                   &bitValue);

            if (err != FM_OK)
            {
                FM_LOG_PRINT( "fmDbgValidateRouteTables Validation Failed: "
                              "ip I/F unable to read bit array index "
                              "%d, err=%d (%s)\n",
                             index,
                             err,
                             fmErrorMsg(err) );
                goto ABORT;
            }

            if ( ( !bitValue && (ipIfEntry->interfaceNum != -1) )
                || ( bitValue && (ipIfEntry->interfaceNum == -1) ) )
            {
                FM_LOG_PRINT("fmDbgValidateRouteTables Validation Failed: "
                             "ip I/F %d (%p) interfaceNum is %d but the "
                             "in-use bit is %s set in the bit array\n",
                             index,
                             (void *) ipIfEntry,
                             ipIfEntry->interfaceNum,
                             (bitValue) ? "" : "not");
                goto ABORT;
            }

        }

        /* for (index = 0 ; ... ) */

    }

    /* end if (switchPtr->maxIpInterfaces > 0) */


    /**********************************************************************
     *  Validate the Virtual Router Table
     *********************************************************************/
    if (switchPtr->maxVirtualRouters > 0)
    {
        if (switchPtr->virtualRouterIds == NULL)
        {
            FM_LOG_PRINT("fmDbgValidateRouteTables Validation Failed: "
                         "maxVirtualRouters = %d but virtualRouterIds == "
                         "NULL\n",
                         switchPtr->maxVirtualRouters);
            goto ABORT;
        }

    }

    /* end if (switchPtr->maxVirtualRouters > 0) */

    /* validate consistency of the switch-specific routing tables */
    if (switchPtr->DbgValidateRouteTables != NULL)
    {
        err = switchPtr->DbgValidateRouteTables(sw);
    }
    else
    {
        err = FM_OK;
    }

ABORT:

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, err);

}   /* end fmDbgValidateRouteTables */




/*****************************************************************************/
/** fmDbgGetRouteCount
 * \ingroup diagMisc
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Returns the number of routes for a switch.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      countPtr points to caller-allocated storage where this
 *                  function should store the the number of routes.
 *
 * \return          FM_OK unconditionally.
 *
 *****************************************************************************/
fm_status fmDbgGetRouteCount(fm_int sw, fm_int *countPtr)
{
    fm_switch *switchPtr;

    FM_LOG_ENTRY(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    *countPtr = (fm_int) fmCustomTreeSize(&switchPtr->routeTree);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_OK);

}   /* end fmDbgGetRouteCount */




/*****************************************************************************/
/** fmSetRouteAttribute
 * \ingroup routerRoute
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Sets an attribute for a specific route.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       route points to the route entry.
 *
 * \param[in]       attr contains the attribute ID (see ''Route Attributes'').
 *
 * \param[in]       value points to the attribute value to be used.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ROUTE if the route description is invalid.
 * \return          FM_ERR_NOT_FOUND if the route does not exist.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_INVALID_ATTRIB if attr is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if value is invalid.
 *
 *****************************************************************************/
fm_status fmSetRouteAttribute(fm_int         sw,
                              fm_routeEntry *route,
                              fm_int         attr,
                              void *         value)
{
    fm_switch *       switchPtr;
    fm_status         status;
    fm_status         status2;
    fm_intRouteEntry *intRoute;
    fm_customTree *   routeTree;
    fm_intRouteEntry  key;
    fm_bool           routeLockTaken = FALSE;
    fm_int            oldEcmpGroupId = -1;
    fm_int            newEcmpGroupId = -1;
    fm_intEcmpGroup * oldEcmpGroup = NULL;
    fm_intEcmpGroup * newEcmpGroup = NULL;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, route = %p, attr = %d, value = %p\n",
                     sw,
                     (void *) route,
                     attr,
                     (void *) value);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    /* Get exclusive access to routing tables */
    status = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

    routeLockTaken = TRUE;

    /* Try to find the route */
    key.route = *route;

    routeTree = GetRouteTree(sw, route);

    if (routeTree == NULL)
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    status = fmCustomTreeFind(&switchPtr->routeTree,
                              &key,
                              (void **) &intRoute);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

    switch (attr)
    {
        case FM_ROUTE_ECMP_GROUP:
            oldEcmpGroupId = intRoute->ecmpGroupId;

            /* Validate the new ECMP group */
            newEcmpGroupId = *(fm_int *) value;

            if ( (newEcmpGroupId < 0)
                || (newEcmpGroupId >= switchPtr->maxArpEntries) )
            {
                status = FM_ERR_INVALID_ARGUMENT;
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
            }

            newEcmpGroup = switchPtr->ecmpGroups[newEcmpGroupId];

            if (newEcmpGroup == NULL)
            {
                status = FM_ERR_INVALID_ARGUMENT;
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
            }

            /* Remove the route from the old ECMP group */
            oldEcmpGroup = switchPtr->ecmpGroups[oldEcmpGroupId];

            status = fmCustomTreeRemove(&oldEcmpGroup->routeTree,
                                        intRoute,
                                        NULL);

            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

            /* Remove the route from the route tree. */
            status = fmCustomTreeRemove(&switchPtr->routeTree,
                                        intRoute,
                                        NULL);

            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

            /* Update the route's ECMP Group ID */
            intRoute->ecmpGroupId                      = newEcmpGroupId;
            intRoute->route.data.unicastECMP.ecmpGroup = newEcmpGroupId;

            /* Add the route to the new ECMP group */
            status = fmCustomTreeInsert(&newEcmpGroup->routeTree,
                                        intRoute,
                                        intRoute);

            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

            /* Add the route to the route tree. */
            status = fmCustomTreeInsert(&switchPtr->routeTree,
                                        intRoute,
                                        intRoute);

            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
            break;

        default:
            break;
    }

    FM_API_CALL_FAMILY(status,
                       switchPtr->SetRouteAttribute,
                       sw,
                       intRoute,
                       attr,
                       value);

    if (status != FM_OK)
    {
        switch (attr)
        {
            case FM_ROUTE_ECMP_GROUP:
                /* Remove the route from the new ECMP group */
                status2 = fmCustomTreeRemove(&newEcmpGroup->routeTree,
                                             intRoute,
                                             NULL);

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status2);

                /* Remove the route from the route tree. */
                status2 = fmCustomTreeRemove(&switchPtr->routeTree,
                                             intRoute,
                                             NULL);

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status2);

                /* Update the route's ECMP Group ID */
                intRoute->ecmpGroupId                      = oldEcmpGroupId;
                intRoute->route.data.unicastECMP.ecmpGroup = oldEcmpGroupId;

                /* Add the route to the old ECMP group */
                status2 = fmCustomTreeInsert(&oldEcmpGroup->routeTree,
                                             intRoute,
                                             intRoute);

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status2);

                /* Add the route to the route tree. */
                status2 = fmCustomTreeInsert(&switchPtr->routeTree,
                                             intRoute,
                                             intRoute);

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status2);
                break;

            default:
                break;
        }

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }


ABORT:

    if (routeLockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmSetRouteAttribute */




/*****************************************************************************/
/** fmGetRouteAttribute
 * \ingroup routerRoute
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Gets an attribute for a specific route.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       route points to the route entry.
 *
 * \param[in]       attr contains the attribute ID.
 *
 * \param[in]       value points to caller-allocated storage into which the
 *                  function will store the attribute's value.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmGetRouteAttribute(fm_int         sw,
                              fm_routeEntry *route,
                              fm_int         attr,
                              void *         value)
{
    fm_switch *       switchPtr;
    fm_status         status;
    fm_intRouteEntry *intRoute;
    fm_intRouteEntry  key;
    fm_bool           routeLockTaken = FALSE;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, route = %p, attr = %d, value = %p\n",
                     sw,
                     (void *) route,
                     attr,
                     (void *) value);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    /* Get shared access to routing tables */
    status = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    if (status != FM_OK)
    {
        goto ABORT;
    }

    routeLockTaken = TRUE;

    key.route = *route;
    status    = fmCustomTreeFind(&switchPtr->routeTree,
                                 &key,
                                 (void **) &intRoute);

    if (status != FM_OK)
    {
        goto ABORT;
    }

    FM_API_CALL_FAMILY(status,
                       switchPtr->GetRouteAttribute,
                       sw,
                       intRoute,
                       attr,
                       value);

ABORT:

    if (routeLockTaken)
    {
        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmGetRouteAttribute */




/*****************************************************************************/
/** fmCreateECMPGroup
 * \ingroup routerEcmp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Creates an ARP type, variable size ECMP group. See 
 *                  ''fmCreateECMPGroupV2''for creating alternative ECMP 
 *                  group types.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      groupId points to caller-allocated storage into which the
 *                  function will place the new ECMP group's ID.
 *
 * \param[in]       numNextHops contains the number of next-hops in nextHopList
 *                  that are to be added to the ECMP group when the group
 *                  is created. Set to 0 if the group is to be created with
 *                  no next-hops. Next-hops may be added later with 
 *                  ''fmAddECMPGroupNextHops''.
 *
 * \param[in]       nextHopList points to an array, numNextHops elements in
 *                  length, of next-hop definitions.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_TABLE_FULL if all available ECMP groups are in use.
 * \return          FM_ERR_NO_MEM if memory cannot be allocated.
 * \return          FM_ERR_TABLE_FULL if the hardware ARP table is full.
 *
 *****************************************************************************/
fm_status fmCreateECMPGroup(fm_int      sw,
                            fm_int *    groupId,
                            fm_int      numNextHops,
                            fm_nextHop *nextHopList)
{
    fm_status       status;
    fm_switch *     switchPtr;
    fm_ecmpNextHop *ecmpNextHopList;
    fm_int          i;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, groupId = %p, numNextHops = %d, "
                     "nextHopList = %p\n",
                     sw,
                     (void *) groupId,
                     numNextHops,
                     (void *) nextHopList);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr       = GET_SWITCH_PTR(sw);
    ecmpNextHopList = NULL;

    status = fmCreateECMPGroupInternal(sw, groupId, NULL, NULL);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

    if ( (numNextHops < 0) || (numNextHops > switchPtr->maxArpEntries) )
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    if (numNextHops > 0)
    {
        /* Allocate and initialize an array of fm_ecmpNextHop structures,
         * copying from the caller's fm_nextHop structures. */
        i = numNextHops * sizeof(fm_ecmpNextHop);

        ecmpNextHopList = fmAlloc(i);
        if (ecmpNextHopList == NULL)
        {
            status = FM_ERR_NO_MEM;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
        }

        FM_CLEAR(*ecmpNextHopList);

        for (i = 0 ; i < numNextHops ; i++)
        {
            ecmpNextHopList[i].type = FM_NEXTHOP_TYPE_ARP;
            FM_MEMCPY_S( &ecmpNextHopList[i].data.arp,
                         sizeof(ecmpNextHopList[i].data.arp),
                         &nextHopList[i],
                         sizeof(nextHopList[i]) );
        }

        status = fmAddECMPGroupNextHopsInternal(sw, 
                                                *groupId,
                                                numNextHops,
                                                ecmpNextHopList);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

ABORT:

    if (ecmpNextHopList != NULL)
    {
        fmFree(ecmpNextHopList);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmCreateECMPGroup */




/*****************************************************************************/
/** fmCreateECMPGroupV2
 * \ingroup routerEcmp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Creates an ECMP group. Provided as a more flexible
 *                  alternative to ''fmCreateECMPGroup''. In addition to being
 *                  able to create variable sized ARP-type ECMP groups, this 
 *                  function also permits creating fixed sized ECMP groups
 *                  for load balancing applications.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      groupId points to caller-allocated storage into which the
 *                  function will place the new ECMP group's ID.
 *
 * \param[in]       info points to a structure describing the characteristics
 *                  to be applied to this ECMP group. NULL means to create
 *                  a normal ECMP group (narrow, adjustable-size).
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_TABLE_FULL if all available ECMP groups are in use.
 * \return          FM_ERR_NO_MEM if memory cannot be allocated.
 * \return          FM_ERR_TABLE_FULL if the hardware ARP table is full.
 *
 *****************************************************************************/
fm_status fmCreateECMPGroupV2(fm_int            sw,
                              fm_int *          groupId,
                              fm_ecmpGroupInfo *info)
{
    fm_status        status;

    FM_LOG_ENTRY_API( FM_LOG_CAT_ROUTING,
                      "sw = %d, groupId = %p, info = %p\n",
                      sw,
                      (void *) groupId,
                      (void *) info );

    VALIDATE_AND_PROTECT_SWITCH(sw);

    status = fmCreateECMPGroupInternal(sw,
                                       groupId,
                                       info,
                                       NULL);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmCreateECMPGroupV2 */




/*****************************************************************************/
/** fmCreateECMPGroupInternal
 * \ingroup intRouterArp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Creates an ECMP group.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[out]      groupId points to caller-allocated storage into which the
 *                  function will place the new ECMP group's ID.
 *
 * \param[in]       info points to the structure describing characteristics
 *                  to be applied to this ECMP group. NULL means to create
 *                  a normal ECMP group (narrow, adjustable-size).
 *
 * \param[in]       mcastGroup points to the multicast group, if this ECMP
 *                  group is for a multicast address.  It is NULL if it is
 *                  for unicast.  ~0 means create the multicast drop group.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_TABLE_FULL if all available ECMP groups are in use.
 * \return          FM_ERR_NO_MEM if memory cannot be allocated.
 * \return          FM_ERR_TABLE_FULL if the hardware ARP table is full.
 *
 *****************************************************************************/
fm_status fmCreateECMPGroupInternal(fm_int                sw,
                                    fm_int *              groupId,
                                    fm_ecmpGroupInfo *    info,
                                    fm_intMulticastGroup *mcastGroup)
{
    fm_status          status;
    fm_switch *        switchPtr;
    fm_int             index;
    fm_intEcmpGroup *  group = NULL;
    fm_bool            groupAlloc = FALSE;
    fm_bool            nextHopsAlloc = FALSE;
    fm_int             i;
    fm_bool            dropGroup;
    fm_bool            multicast;
    fm_bool            wideNextHops;
    fm_int             numFixedEntries;
    fm_int             maxNextHops;
    fm_intNextHop *    intNextHop;
    fm_uint16          lbsVlan;

    FM_LOG_ENTRY( FM_LOG_CAT_ROUTING,
                  "sw = %d, groupId = %p, info = %p, mcastGroup=%p\n",
                  sw,
                  (void *) groupId,
                  (void *) info,
                  (void *) mcastGroup );

    if (groupId == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxArpEntries <= 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_UNSUPPORTED);
    }

    if (info != NULL)
    {
        /* Retrieve caller-requested characteristics */
        wideNextHops    = info->wideNextHops;
        numFixedEntries = info->numFixedEntries;
        lbsVlan         = info->lbsVlan;

        if (numFixedEntries < 0)
        {
            FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
        }
        else if (numFixedEntries == 0)
        {
            maxNextHops = switchPtr->maxEcmpGroupSize;
        }
        else
        {
            maxNextHops = numFixedEntries;
        }
    }
    else
    {
        /* Apply default characteristics. */
        wideNextHops    = FALSE;
        maxNextHops     = switchPtr->maxEcmpGroupSize;
        numFixedEntries = 0;
        lbsVlan         = 0;
    }

    if (mcastGroup == NULL)
    {
        dropGroup = FALSE;
        multicast = FALSE;
    }
    else if ( mcastGroup == (fm_intMulticastGroup *) ~0 )
    {
        dropGroup = TRUE;
        multicast = FALSE;
    }
    else
    {
        dropGroup = FALSE;
        multicast = TRUE;
    }

    /* gain exclusive access to routing tables */
    status = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, status);
    }

    /* find an available ECMP group */
    status = fmFindBitInBitArray(&switchPtr->ecmpGroupsInUse,
                                 0,
                                 FALSE,
                                 &index);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

    if ( (index < 0) || (index >= switchPtr->maxArpEntries) )
    {
        status = FM_ERR_TABLE_FULL;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    group = fmAlloc( sizeof(fm_intEcmpGroup) );

    if (group == NULL)
    {
        status = FM_ERR_NO_MEM;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    groupAlloc = TRUE;

    FM_CLEAR(*group);

    group->groupId      = index;
    group->nextHopCount = 0;
    group->mcastGroup   = mcastGroup;
    group->maxNextHops  = maxNextHops;
    group->lbsVlan      = lbsVlan;
    group->numFixedEntries = numFixedEntries;

    if (numFixedEntries > 0)
    {
        group->fixedSize = TRUE;

        if (wideNextHops)
        {
            group->wideGroup = TRUE;
        }
    }

    i = sizeof(fm_intNextHop *) * maxNextHops;

    group->nextHops = fmAlloc(i);

    if (group->nextHops == NULL)
    {
        status = FM_ERR_NO_MEM;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    nextHopsAlloc = TRUE;

    FM_MEMSET_S(group->nextHops, i, 0, i);

    for (i = 0 ; i < numFixedEntries ; i++)
    {
        intNextHop = fmAlloc( sizeof(fm_intNextHop) );

        if (intNextHop == NULL)
        {
            status = FM_ERR_NO_MEM;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
        }

        /* initialize the next hop structure */
        FM_CLEAR(*intNextHop);
        intNextHop->nextHop.type = FM_NEXTHOP_TYPE_DROP;
        intNextHop->sw           = sw;
        intNextHop->ecmpGroup    = group;
        intNextHop->hopIndex     = FM_NEXTHOP_INDEX_UNSPECIFIED;
        intNextHop->state        = FM_NEXT_HOP_STATE_UP;

        /* Save the nexthop pointer into the next hop table */
        group->nextHops[i] = intNextHop;
        group->nextHopCount++;
    }

    if (numFixedEntries > 0)
    {
        group->isGroupWidthKnown = TRUE;
        group->wideGroup         = wideNextHops;
    }

    fmCustomTreeInit(&group->routeTree, fmCompareIntRoutes);

    if (switchPtr->CreateECMPGroup != NULL)
    {
        status = switchPtr->CreateECMPGroup(sw, group);

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    switchPtr->ecmpGroups[index] = group;

    status = fmSetBitArrayBit(&switchPtr->ecmpGroupsInUse, index, TRUE);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

    *groupId = index;

    if (multicast || dropGroup || group->fixedSize)
    {
        status = ValidateEcmpGroup(sw, group, NULL);

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }


ABORT:

    if (status != FM_OK)
    {
        if (nextHopsAlloc)
        {
            for (i = 0 ; i < group->maxNextHops ; i++)
            {
                if (group->nextHops[i] != NULL)
                {
                    fmFree(group->nextHops[i]);
                }
            }

            fmFree(group->nextHops);
        }

        if (groupAlloc)
        {
            fmFree(group);
            switchPtr->ecmpGroups[index] = NULL;
            fmSetBitArrayBit(&switchPtr->ecmpGroupsInUse, index, FALSE);
        }
    }

    fmReleaseWriteLock(&switchPtr->routingLock);

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, status);

}   /* end fmCreateECMPGroupInternal */




/*****************************************************************************/
/** fmDeleteECMPGroup
 * \ingroup routerEcmp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Deletes an ECMP group.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupId is the ECMP group ID from a prior call to
 *                  ''fmCreateECMPGroup'' or ''fmCreateECMPGroupV2''.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if groupId is invalid.
 * \return          FM_ERR_ECMP_GROUP_IN_USE if the ECMP group is in use, i.e.,
 *                  it is associated with one or more routes.
 *
 *****************************************************************************/
fm_status fmDeleteECMPGroup(fm_int sw, fm_int groupId)
{
    fm_status        status;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING, "sw = %d, groupId = %d\n", sw, groupId);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    status = fmDeleteECMPGroupInternal(sw, groupId);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmDeleteECMPGroup */




/*****************************************************************************/
/** fmDeleteECMPGroupInternal
 * \ingroup intRouterArp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Deletes an ECMP group.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       groupId is the group ID from a prior call to
 *                  fmCreateECMPGroup.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if groupId is invalid.
 * \return          FM_ERR_ECMP_GROUP_IN_USE if the ECMP group is in use, i.e.,
 *                  it is associated with one or more routes.
 *
 *****************************************************************************/
fm_status fmDeleteECMPGroupInternal(fm_int sw, fm_int groupId)
{
    fm_status        status;
    fm_switch *      switchPtr;
    fm_intEcmpGroup *group;
    fm_intNextHop *  intNextHop;
    fm_ecmpNextHop * removedHops;
    fm_int           i;
    fm_uint          treeSize;

    FM_LOG_ENTRY(FM_LOG_CAT_ROUTING, "sw = %d, groupId = %d\n", sw, groupId);

    switchPtr = GET_SWITCH_PTR(sw);

    if ( (groupId < 0) || (groupId >= switchPtr->maxArpEntries) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    /* gain exclusive access to routing tables */
    status = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, status);
    }

    group = switchPtr->ecmpGroups[groupId];

    if (group == NULL)
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    treeSize = fmCustomTreeSize(&group->routeTree);

    if (treeSize != 0)
    {
        status = FM_ERR_ECMP_GROUP_IN_USE;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    if (group->nextHopCount > 0)
    {
        /* Remove all existing next hops */
        i = sizeof(fm_ecmpNextHop) * group->nextHopCount;

        removedHops = fmAlloc(i);

        if (removedHops == NULL)
        {
            status = FM_ERR_NO_MEM;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
        }

        FM_MEMSET_S(removedHops, i, 0, i);

        for (i = 0 ; i < group->nextHopCount ; i++)
        {
            intNextHop = group->nextHops[i];

            FM_MEMCPY_S( &removedHops[i],
                         sizeof(removedHops[i]),
                         &intNextHop->nextHop,
                         sizeof(intNextHop->nextHop) );
        }

        status = fmDeleteECMPGroupNextHopsInternal(sw,
                                                   groupId,
                                                   group->nextHopCount,
                                                   removedHops);

        fmFree(removedHops);

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    /* Delete the ECMP group from the hardware */
    if (switchPtr->DeleteECMPGroup != NULL)
    {
        status = switchPtr->DeleteECMPGroup(sw, group);

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    /* Delete the remaining structures and memory allocated by the group */
    fmCustomTreeDestroy(&group->routeTree, NULL);
    fmFree(group->nextHops);
    fmFree(group);

    switchPtr->ecmpGroups[groupId] = NULL;

    status = fmSetBitArrayBit(&switchPtr->ecmpGroupsInUse, groupId, FALSE);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

ABORT:

    fmReleaseWriteLock(&switchPtr->routingLock);

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, status);

}   /* end fmDeleteECMPGroupInternal */




/*****************************************************************************/
/** fmAddECMPGroupNextHops
 * \ingroup routerEcmp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Adds one or more next-hops to a variable sized ECMP group
 *                  using an ''fm_nextHop'' ARP-type next-hop specification.
 *
 * \note            See ''fmAddECMPGroupNextHopsV2'' for ''fm_ecmpNextHop'' 
 *                  type next-hop specifications.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupId is the ECMP group ID from a prior call to
 *                  ''fmCreateECMPGroup''.
 *
 * \param[in]       numNextHops is the number of next-hops in nextHopList.
 *
 * \param[in]       nextHopList points to an array of next hops to be added
 *                  to the group.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if groupId is invalid.
 * \return          FM_ERR_ECMP_GROUP_IS_FULL if the group has reached its
 *                  maximum capacity of ARP entries.
 * \return          FM_ERR_TABLE_FULL if the hardware ARP table is full.
 * \return          FM_ERR_NO_MEM if the function is unable to allocate
 *                  needed memory.
 * \return          FM_ERR_ALREADY_EXISTS if one of the next-hops being
 *                  added already exists in the ECMP group.
 * \return          FM_ERR_UNSUPPORTED if the ECMP group is a fixed sized
 *                  group.
 *
 *****************************************************************************/
fm_status fmAddECMPGroupNextHops(fm_int      sw,
                                 fm_int      groupId,
                                 fm_int      numNextHops,
                                 fm_nextHop *nextHopList)
{
    fm_status       status;
    fm_switch *     switchPtr;
    fm_ecmpNextHop *ecmpNextHopList;
    fm_int          i;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, groupId = %d, numNextHops = %d, "
                     "nextHopList = %p\n",
                     sw,
                     groupId,
                     numNextHops,
                     (void *) nextHopList);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr       = GET_SWITCH_PTR(sw);
    ecmpNextHopList = NULL;

    if ( (numNextHops <= 0) || (numNextHops > switchPtr->maxArpEntries) )
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    /* Allocate and initialize an array of fm_ecmpNextHop structures,
     * copying from the caller's fm_nextHop structures. */
    i = numNextHops * sizeof(fm_ecmpNextHop);

    ecmpNextHopList = fmAlloc(i);
    if (ecmpNextHopList == NULL)
    {
        status = FM_ERR_NO_MEM;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    FM_MEMSET_S(ecmpNextHopList, i, 0, i);

    for (i = 0 ; i < numNextHops ; i++)
    {
        ecmpNextHopList[i].type = FM_NEXTHOP_TYPE_ARP;
        FM_MEMCPY_S( &ecmpNextHopList[i].data.arp,
                     sizeof(ecmpNextHopList[i].data.arp),
                     &nextHopList[i],
                     sizeof(nextHopList[i]) );
    }

    /* Add the next-hops to the ECMP Group */
    status = fmAddECMPGroupNextHopsInternal(sw,
                                            groupId,
                                            numNextHops,
                                            ecmpNextHopList);

ABORT:

    if (ecmpNextHopList != NULL)
    {
        fmFree(ecmpNextHopList);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmAddECMPGroupNextHops */




/*****************************************************************************/
/** fmAddECMPGroupNextHopsV2
 * \ingroup routerEcmp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Adds one or more next-hops to a variable sized ECMP group
 *                  using an ''fm_ecmpNextHop'' type next-hop specification.
 *
 * \note            See ''fmAddECMPGroupNextHops'' for ''fm_nextHop'' 
 *                  type next-hop specifications.
 *
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupId is the ECMP group ID from a prior call to
 *                  ''fmCreateECMPGroup''.
 *
 * \param[in]       numNextHops is the number of next-hops in nextHopList.
 *
 * \param[in]       nextHopList points to an array of next hops to be added
 *                  to the group.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if groupId is invalid.
 * \return          FM_ERR_ECMP_GROUP_IS_FULL if the group has reached its
 *                  maximum capacity of ARP entries.
 * \return          FM_ERR_TABLE_FULL if the hardware ARP table is full.
 * \return          FM_ERR_NO_MEM if the function is unable to allocate
 *                  needed memory.
 * \return          FM_ERR_ALREADY_EXISTS if one of the next-hops being
 *                  added already exists in the ECMP group.
 *
 *****************************************************************************/
fm_status fmAddECMPGroupNextHopsV2(fm_int          sw,
                                   fm_int          groupId,
                                   fm_int          numNextHops,
                                   fm_ecmpNextHop *nextHopList)
{
    fm_status status;

    FM_LOG_ENTRY_API( FM_LOG_CAT_ROUTING,
                      "sw = %d, groupId = %d, numNextHops = %d, "
                      "nextHopList = %p\n",
                      sw,
                      groupId,
                      numNextHops,
                      (void *) nextHopList );

    VALIDATE_AND_PROTECT_SWITCH(sw);

    status = fmAddECMPGroupNextHopsInternal(sw,
                                            groupId,
                                            numNextHops,
                                            nextHopList);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmAddECMPGroupNextHopsV2 */




/*****************************************************************************/
/** fmAddECMPGroupNextHopsInternal
 * \ingroup intRouterArp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Adds one or more next hops to an ECMP group.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       groupId is the group ID from a prior call to
 *                  fmCreateECMPGroup.
 *
 * \param[in]       numNextHops is the number of next-hops in nextHopList.
 *
 * \param[in]       nextHopList points to an array of next hops to be added
 *                  to the group.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if groupId is invalid.
 * \return          FM_ERR_ECMP_GROUP_IS_FULL if the group's ARP table is full.
 * \return          FM_ERR_TABLE_FULL if the hardware ARP table is full.
 * \return          FM_ERR_NO_MEM if the function is unable to allocate
 *                  needed memory.
 * \return          FM_ERR_ALREADY_EXISTS if one of the next-hops being
 *                  added already exists in the ECMP group.
 *
 *****************************************************************************/
fm_status fmAddECMPGroupNextHopsInternal(fm_int          sw,
                                         fm_int          groupId,
                                         fm_int          numNextHops,
                                         fm_ecmpNextHop *nextHopList)
{
    fm_status               status;
    fm_switch *             switchPtr;
    fm_intEcmpGroup *       group;
    fm_int                  index;
    fm_ecmpNextHop *        nextHop;
    fm_intNextHop *         intNextHop;
    fm_int                  addedNextHops;
    fm_int                  newNextHopCount;
    fm_bool                 wideGroup;
    fm_customTreeIterator   iter;
    fm_intRouteEntry *      routeKey;
    fm_intRouteEntry *      route;

    FM_LOG_ENTRY( FM_LOG_CAT_ROUTING,
                  "sw = %d, groupId = %d, numNextHops = %d, nextHopList = %p\n",
                  sw,
                  groupId,
                  numNextHops,
                  (void *) nextHopList );

    switchPtr = GET_SWITCH_PTR(sw);

    FM_LOG_ASSERT(FM_LOG_CAT_ROUTING,
                  numNextHops > 0,
                  "numNextHops is <= 0, numNextHops = %d\n",
                  numNextHops);

    if ( (groupId < 0) || (groupId >= switchPtr->maxArpEntries) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    /* gain exclusive access to routing tables */
    status = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, status);
    }

    addedNextHops = 0;

    group = switchPtr->ecmpGroups[groupId];

    if (group == NULL)
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    /* This function may not be used with fixed-size ECMP Groups */
    if (group->fixedSize)
    {
        status = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    if (group->mcastGroup != NULL)
    {
        status = FM_ERR_USE_MCAST_FUNCTIONS;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    newNextHopCount = group->nextHopCount + numNextHops;

    if (newNextHopCount > switchPtr->maxEcmpGroupSize)
    {
        status = FM_ERR_ECMP_GROUP_IS_FULL;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    if (group->isGroupWidthKnown)
    {
        wideGroup = group->wideGroup;
    }
    else
    {
        switch (nextHopList[0].type)
        {
            case FM_NEXTHOP_TYPE_ARP:
            case FM_NEXTHOP_TYPE_RAW_NARROW:
                wideGroup = FALSE;
                break;
    
            case FM_NEXTHOP_TYPE_RAW_WIDE:
                wideGroup = TRUE;
                break;
    
            default:
                status = FM_ERR_INVALID_ARGUMENT;
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
        }

        /* Set the group width, now that it is known. */
        group->isGroupWidthKnown = TRUE;
        group->wideGroup         = wideGroup;
    }

    /* Verify that all next-hops are the same width and that the next-hop
     * does not already exist in the ECMP group. */
    for (index = 0 ; index < numNextHops ; index++)
    {
        nextHop = &nextHopList[index];

        /* Check the width */
        switch (nextHop->type)
        {
            case FM_NEXTHOP_TYPE_ARP:
            case FM_NEXTHOP_TYPE_RAW_NARROW:
            case FM_NEXTHOP_TYPE_DROP:
                if (wideGroup)
                {
                    status = FM_ERR_MIXING_NARROW_AND_WIDE_NEXTHOPS;
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
                }
                break;

            case FM_NEXTHOP_TYPE_RAW_WIDE:
                if (!wideGroup)
                {
                    status = FM_ERR_MIXING_NARROW_AND_WIDE_NEXTHOPS;
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
                }
                break;

            default:
                status = FM_ERR_INVALID_ARGUMENT;
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
                break;
        }

        /* Make sure the next-hop is not already in the ECMP group */
        if ( FindNextHop(sw, group, nextHop, NULL) != NULL )
        {
            status = FM_ERR_ALREADY_EXISTS;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
        }
    }

    for (index = 0 ; index < numNextHops ; index++)
    {
        /* Point at the new record */
        nextHop = &nextHopList[index];

        /* Allocate a new internal next-hop record */
        intNextHop = fmAlloc( sizeof(fm_intNextHop) );

        if (intNextHop == NULL)
        {
            status = FM_ERR_NO_MEM;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
        }

        /* Clear the next hop structure */
        FM_CLEAR(*intNextHop);

        /* Save the nexthop pointer into the next hop table */
        group->nextHops[group->nextHopCount] = intNextHop;
        group->nextHopCount++;
        addedNextHops++;

        /* set hopIndex to unspecified */
        intNextHop->hopIndex = FM_NEXTHOP_INDEX_UNSPECIFIED;

        /* Initialize the structure */
        status = fmInitializeNextHop(sw,
                                     group,
                                     intNextHop,
                                     nextHop);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    status = ValidateEcmpGroup(sw, group, NULL);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

    FM_API_CALL_FAMILY(status,
                       switchPtr->AddECMPGroupNextHops,
                       sw,
                       group,
                       numNextHops,
                       nextHopList);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

    fmCustomTreeIterInit(&iter, &group->routeTree);

    while (1)
    {
        status = fmCustomTreeIterNext( &iter,
                                       (void **) &routeKey,
                                       (void **) &route );
        if (status == FM_ERR_NO_MORE)
        {
            status = FM_OK;
            break;
        }

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

        status = fmNotifyVNTunnelAboutEcmpChange(sw, route);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }


    ABORT:

    if (status != FM_OK)
    {
        if (addedNextHops > 0)
        {
            /* attempt to remove the next-hops from the group */
            fmDeleteECMPGroupNextHopsInternal(sw,
                                              groupId,
                                              addedNextHops,
                                              nextHopList);
        }
        else if ( (group != NULL) && (group->nextHopCount == 0) )
        {
            group->isGroupWidthKnown = FALSE;
            group->wideGroup         = FALSE;
        }
    }

    fmReleaseWriteLock(&switchPtr->routingLock);

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, status);

}   /* end fmAddECMPGroupNextHopsInternal */




/*****************************************************************************/
/** fmAddECMPGroupRawNextHop
 * \ingroup intRouterEcmp
 *
 * \chips           FM6000
 *
 * \desc            Adds a raw next-hop from an ECMP group.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupId is the ECMP group ID from a prior call to
 *                  ''fmCreateECMPGroup''.
 * 
 * \param[in]       nextHopType is the type of raw next hop entry, see
 *                  ''fm_rawNextHopType'' for more details
 *                  
 * \param[in]       value0 is the first 64-bit value in the raw next-hop.
 *
 * \param[in]       value1 is the second 64-bit value in the raw next-hop (for
 *                  raw wide next hop entry only).
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if groupId is invalid.
 *
 *****************************************************************************/
fm_status fmAddECMPGroupRawNextHop(fm_int               sw,
                                   fm_int               groupId,
                                   fm_ecmpNextHopType   nextHopType,
                                   fm_uint64            value0,
                                   fm_uint64            value1)
{
    fm_status      status;
    fm_ecmpNextHop nextHop;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, groupId = %d, nextHopType = %d, "
                     "value0 = %" FM_FORMAT_64 "X, "
                     "value1 = %" FM_FORMAT_64 "X\n",
                     sw,
                     groupId,
                     nextHopType,
                     value0,
                     value1);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    FM_CLEAR( nextHop );

    nextHop.type = nextHopType;
    
    switch (nextHopType)
    {
        case FM_NEXTHOP_TYPE_RAW_NARROW:
            nextHop.data.rawNarrow.value = value0;
            break;

        case FM_NEXTHOP_TYPE_RAW_WIDE:
            nextHop.data.rawWide.values[0] = value0;
            nextHop.data.rawWide.values[1] = value1;
            break;

        default:
            status = FM_ERR_INVALID_ARGUMENT;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
            break;
    }
    
    status = fmAddECMPGroupNextHopsInternal(sw, groupId, 1, &nextHop);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

ABORT:

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmAddECMPGroupRawNextHop */




/*****************************************************************************/
/** fmDeleteECMPGroupNextHops
 * \ingroup routerEcmp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Deletes one or more next-hops from a variable sized ECMP 
 *                  group using an ''fm_nextHop'' ARP-type next-hop 
 *                  specification. 
 *
 * \note            See ''fmDeleteECMPGroupNextHopsV2'' for ''fm_ecmpNextHop'' 
 *                  type next-hop specifications.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupId is the ECMP group ID from a prior call to
 *                  ''fmCreateECMPGroup''.
 *
 * \param[in]       numNextHops is the number of next-hops in nextHopList.
 *
 * \param[in]       nextHopList points to an array of next-hops to be deleted
 *                  from the group.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if groupId is invalid or
 *                  nextHopList is NULL. If an address described in nextHopList
 *                  cannot be found in the ECMP group's next-hop table, that
 *                  next-hop will simply be ignored.
 * \return          FM_ERR_ECMP_GROUP_IN_USE if the group is being used by
 *                  one or more routes and the last next-hop in the group
 *                  is being deleted.
 * \return          FM_ERR_UNSUPPORTED if the ECMP group is a fixed sized
 *                  group.
 *
 *****************************************************************************/
fm_status fmDeleteECMPGroupNextHops(fm_int      sw,
                                    fm_int      groupId,
                                    fm_int      numNextHops,
                                    fm_nextHop *nextHopList)
{
    fm_status       status;
    fm_switch *     switchPtr;
    fm_ecmpNextHop *ecmpNextHopList;
    fm_int          i;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, groupId = %d, numNextHops = %d, "
                     "nextHopList = %p\n",
                     sw,
                     groupId,
                     numNextHops,
                     (void *) nextHopList);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr       = GET_SWITCH_PTR(sw);
    ecmpNextHopList = NULL;

    if ( (numNextHops <= 0) || (numNextHops > switchPtr->maxArpEntries) )
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    /* Allocate and initialize an array of fm_ecmpNextHop structures,
     * copying from the caller's fm_nextHop structures. */
    i = numNextHops * sizeof(fm_ecmpNextHop);

    ecmpNextHopList = fmAlloc(i);
    if (ecmpNextHopList == NULL)
    {
        status = FM_ERR_NO_MEM;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    FM_MEMSET_S( ecmpNextHopList, i, 0, i);

    for (i = 0 ; i < numNextHops ; i++)
    {
        ecmpNextHopList[i].type = FM_NEXTHOP_TYPE_ARP;
        FM_MEMCPY_S( &ecmpNextHopList[i].data.arp,
                     sizeof(ecmpNextHopList[i].data.arp),
                     &nextHopList[i],
                     sizeof(nextHopList[i]) );
    }

    status = fmDeleteECMPGroupNextHopsInternal(sw,
                                               groupId,
                                               numNextHops,
                                               ecmpNextHopList);

ABORT:

    if (ecmpNextHopList != NULL)
    {
        fmFree(ecmpNextHopList);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmDeleteECMPGroupNextHops */




/*****************************************************************************/
/** fmDeleteECMPGroupNextHopsV2
 * \ingroup routerEcmp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Deletes one or more next-hops from a variable sized ECMP 
 *                  group using an ''fm_ecmpNextHop'' type next-hop 
 *                  specification. 
 *
 * \note            See ''fmDeleteECMPGroupNextHops'' for ''fm_nextHop'' 
 *                  ARP-type next-hop specifications.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupId is the ECMP group ID from a prior call to
 *                  ''fmCreateECMPGroup''.
 *
 * \param[in]       numNextHops is the number of next-hops in nextHopList.
 *
 * \param[in]       nextHopList points to an array of next-hops to be deleted
 *                  from the group.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if groupId is invalid or
 *                  nextHopList is NULL. If an address described in nextHopList
 *                  cannot be found in the ECMP group's next-hop table, that
 *                  next-hop will simply be ignored.
 * \return          FM_ERR_ECMP_GROUP_IN_USE if the group is being used by
 *                  one or more routes and the last next-hop in the group
 *                  is being deleted.
 * \return          FM_ERR_UNSUPPORTED if the ECMP group is a fixed sized
 *                  group.
 *
 *****************************************************************************/
fm_status fmDeleteECMPGroupNextHopsV2(fm_int          sw,
                                      fm_int          groupId,
                                      fm_int          numNextHops,
                                      fm_ecmpNextHop *nextHopList)
{
    fm_status status;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, groupId = %d, numNextHops = %d, "
                     "nextHopList = %p\n",
                     sw,
                     groupId,
                     numNextHops,
                     (void *) nextHopList);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    status = fmDeleteECMPGroupNextHopsInternal(sw,
                                               groupId,
                                               numNextHops,
                                               nextHopList);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmDeleteECMPGroupNextHopsV2 */




/*****************************************************************************/
/** fmDeleteECMPGroupNextHopsInternal
 * \ingroup intRouterArp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Deletes one or more next hops from an ECMP group.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       groupId is the group ID from a prior call to
 *                  fmCreateECMPGroup.
 *
 * \param[in]       numNextHops is the number of next-hops in nextHopList.
 *
 * \param[in]       nextHopList points to an array of next hops to be deleted
 *                  from the group.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if groupId is invalid or
 *                  nextHopList is NULL.  If an address described in nextHopList
 *                  cannot be found in the ECMP group's nexthop table that
 *                  next-hop will simply be ignored.
 * \return          FM_ERR_ECMP_GROUP_IN_USE if the group is being used by
 *                  one or more routes and the last nexthop in the group
 *                  is being deleted.
 *
 *****************************************************************************/
fm_status fmDeleteECMPGroupNextHopsInternal(fm_int          sw,
                                            fm_int          groupId,
                                            fm_int          numNextHops,
                                            fm_ecmpNextHop *nextHopList)
{
    fm_status             status;
    fm_status             status2;
    fm_switch *           switchPtr;
    fm_intEcmpGroup *     group;
    fm_ecmpNextHop *      nextHop;
    fm_intNextHop *       intNextHop;
    fm_int                hopIndex;
    fm_int                index;
    fm_int                i;
    fm_intNextHop **      removedHops = NULL;
    fm_int                removedIndex = 0;
    fm_customTreeIterator iter;
    fm_intRouteEntry *    routeKey;
    fm_intRouteEntry *    route;

    FM_LOG_ENTRY( FM_LOG_CAT_ROUTING,
                  "sw = %d, groupId = %d, numNextHops = %d, "
                  "nextHopList = %p\n",
                  sw,
                  groupId,
                  numNextHops,
                  (void *) nextHopList );

    switchPtr = GET_SWITCH_PTR(sw);

    if ( (groupId < 0) || (groupId >= switchPtr->maxArpEntries) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    /* gain exclusive access to routing tables */
    status = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, status);
    }

    group = switchPtr->ecmpGroups[groupId];

    if (group == NULL)
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    if (group->fixedSize)
        numNextHops = group->nextHopCount;

    i = sizeof(fm_intNextHop *) * numNextHops;

    removedHops = fmAlloc(i);

    if (removedHops == NULL)
    {
        status = FM_ERR_NO_MEM;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    FM_MEMSET_S(removedHops, i, 0, i);

    for (index = 0 ; index < numNextHops ; index++)
    {
        if (group->fixedSize)
        {
            intNextHop = group->nextHops[index];
            group->nextHops[index] = NULL;
        }
        else
        {
            nextHop = &nextHopList[index];

            intNextHop = FindNextHop(sw, group, nextHop, &hopIndex);
            if (intNextHop == NULL)
            {
                /* Ignore next-hops that can't be found.  This allows the
                 * application to easily recover from earlier errors that left the
                 * system in an unknown state.  The application can simply attempt
                 * to remove all next-hops that MIGHT be in the group.
                 */
                continue;
            }

            group->nextHops[hopIndex] = NULL;

            for (hopIndex = hopIndex + 1 ;
                 hopIndex < group->nextHopCount ;
                 hopIndex++)
            {
                 group->nextHops[hopIndex - 1] = group->nextHops[hopIndex];
                 group->nextHops[hopIndex]     = NULL;
            }

        }
        removedHops[removedIndex] = intNextHop;
        removedIndex++;
        group->nextHopCount--;
    
        status = fmDeleteArpNextHopFromTrees(sw, intNextHop);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

    }   /* end for (index = 0 ; index < numNextHops ; index++) */
    if (group->nextHopCount == 0)
    {
        group->isGroupWidthKnown = FALSE;
        group->wideGroup         = FALSE;
    }

    fmCustomTreeIterInit(&iter, &group->routeTree);

    while (1)
    {
        status = fmCustomTreeIterNext( &iter,
                                       (void **) &routeKey,
                                       (void **) &route );
        if (status == FM_ERR_NO_MORE)
        {
            status = FM_OK;
            break;
        }

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

        status = fmNotifyVNTunnelAboutEcmpChange(sw, route);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }


ABORT:

    if (removedHops != NULL)
    {
        ValidateEcmpGroup(sw, group, NULL);

        FM_API_CALL_FAMILY(status2,
                           switchPtr->DeleteECMPGroupNextHops,
                           sw,
                           group,
                           removedIndex,
                           removedHops,
                           numNextHops,
                           nextHopList);

        for (i = 0 ; i < removedIndex ; i++)
        {
            if (removedHops[i] != NULL)
            {
                fmFree(removedHops[i]);
            }
        }

        fmFree(removedHops);

        if (status == FM_OK)
        {
            status = status2;
        }
    }

    fmReleaseWriteLock(&switchPtr->routingLock);

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, status);

}   /* end fmDeleteECMPGroupNextHopInternal */




/*****************************************************************************/
/** fmDeleteECMPGroupRawNextHop
 * \ingroup intRouterEcmp
 *
 * \chips           FM6000
 *
 * \desc            Deletes a raw next-hop from an ECMP group.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupId is the ECMP group ID from a prior call to
 *                  ''fmCreateECMPGroup''.
 * 
 * \param[in]       nextHopType is the type of raw next hop entry, see
 *                  ''fm_rawNextHopType'' for more details  
 *
 * \param[in]       value0 is the first 64-bit value in the raw next-hop.
 *
 * \param[in]       value1 is the second 64-bit value in the raw next-hop (for
 *                  raw wide next hop only). 
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if groupId is invalid or
 *                  nextHopList is NULL. If an address described in nextHopList
 *                  cannot be found in the ECMP group's wide next-hop table,
 *                  that next-hop will simply be ignored.
 * \return          FM_ERR_ECMP_GROUP_IN_USE if the group is being used by
 *                  one or more routes and the last next-hop in the group
 *                  is being deleted.
 *
 *****************************************************************************/
fm_status fmDeleteECMPGroupRawNextHop(fm_int                sw,
                                      fm_int                groupId,
                                      fm_ecmpNextHopType    nextHopType,
                                      fm_uint64             value0,
                                      fm_uint64             value1)
{
    fm_status      status;
    fm_ecmpNextHop nextHop;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, groupId = %d, nextHopType = %d, "
                     "value0 = %" FM_FORMAT_64 "X, "
                     "value1 = %" FM_FORMAT_64 "X\n",
                     sw,
                     groupId,
                     nextHopType,
                     value0,
                     value1);

    FM_CLEAR( nextHop );

    nextHop.type = nextHopType;
    
    switch (nextHopType)
    {
        case FM_NEXTHOP_TYPE_RAW_NARROW:
            nextHop.data.rawNarrow.value = value0;
            break;

        case FM_NEXTHOP_TYPE_RAW_WIDE:
            nextHop.data.rawWide.values[0] = value0;
            nextHop.data.rawWide.values[1] = value1;
            break;

        default:
            status = FM_ERR_INVALID_ARGUMENT;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
            break;
    }

    status = fmDeleteECMPGroupNextHopsInternal(sw, groupId, 1, &nextHop);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

ABORT:

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmDeleteECMPGroupRawNextHop */




/*****************************************************************************/
/** fmReplaceECMPGroupNextHopInternal
 * \ingroup intRouterEcmp
 *
 * \desc            Replaces a next-hop in an ECMP group.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupId is the ECMP group ID from a prior call to
 *                  ''fmCreateECMPGroup''.
 *
 * \param[in]       oldNextHop points to the next-hop that is to be replaced.
 *
 * \param[in]       newNextHop points to the next-hop which is to replace
 *                  oldNextHop.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if groupId is invalid.
 * \return          FM_ERR_NO_MEM if memory for the new next-hop cannot be
 *                  allocated.
 * \return          FM_ERR_NOT_FOUND if the old next-hop was not found in the
 *                  ECMP group.
 * \return          FM_ERR_ALREADY_EXISTS if the new next-hop already exists
 *                  in the ECMP group.
 *
 *****************************************************************************/
fm_status fmReplaceECMPGroupNextHopInternal(fm_int          sw,
                                            fm_int          groupId,
                                            fm_ecmpNextHop *oldNextHop,
                                            fm_ecmpNextHop *newNextHop)
{
    fm_status               status;
    fm_switch *             switchPtr;
    fm_intEcmpGroup *       group;
    fm_int                  hopIndex;
    fm_intNextHop *         oldIntNextHop;
    fm_intNextHop *         newIntNextHop;
    fm_bool                 wideHop;

    FM_LOG_ENTRY(FM_LOG_CAT_ROUTING,
                 "sw = %d, groupId = %d, oldNextHop = %p, newNextHop = %p\n",
                 sw,
                 groupId,
                 (void *) oldNextHop,
                 (void *) newNextHop);

    switchPtr = GET_SWITCH_PTR(sw);

    if ( (groupId < 0) || (groupId >= switchPtr->maxArpEntries) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    group = switchPtr->ecmpGroups[groupId];

    if (group == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    oldIntNextHop = FindNextHop(sw, group, oldNextHop, &hopIndex);

    if (oldIntNextHop == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_NOT_FOUND);
    }

    newIntNextHop = FindNextHop(sw, group, newNextHop, NULL);

    if (newIntNextHop != NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_ALREADY_EXISTS);
    }

    switch (oldNextHop->type)
    {
        case FM_NEXTHOP_TYPE_ARP:
        case FM_NEXTHOP_TYPE_DROP:
        case FM_NEXTHOP_TYPE_RAW_NARROW:
            if (group->wideGroup)
            {
                FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_MIXING_NARROW_AND_WIDE_NEXTHOPS);
            }
            wideHop = FALSE;
            break;

        case FM_NEXTHOP_TYPE_RAW_WIDE:
            if (!group->wideGroup)
            {
                FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_MIXING_NARROW_AND_WIDE_NEXTHOPS);
            }
            wideHop = TRUE;
            break;

        default:
            FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
            break;
    }

    switch (newNextHop->type)
    {
        case FM_NEXTHOP_TYPE_ARP:
        case FM_NEXTHOP_TYPE_DROP:
        case FM_NEXTHOP_TYPE_RAW_NARROW:
            if (wideHop)
            {
                FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_MIXING_NARROW_AND_WIDE_NEXTHOPS);
            }
            break;

        case FM_NEXTHOP_TYPE_RAW_WIDE:
            if (!wideHop)
            {
                FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_MIXING_NARROW_AND_WIDE_NEXTHOPS);
            }
            break;

        default:
            FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
            break;
    }

    /* Delete the old next hop from the ARP and interface trees */
    status = fmDeleteArpNextHopFromTrees(sw, oldIntNextHop);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, status);

    newIntNextHop = fmAlloc( sizeof(fm_intNextHop) );
    if (newIntNextHop == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_NO_MEM);
    }

    /* initialize the next hop structure */
    FM_CLEAR(*newIntNextHop);

     /* set nextHop index to unspecified */
    newIntNextHop->hopIndex = FM_NEXTHOP_INDEX_UNSPECIFIED;

    status = fmInitializeNextHop(sw, group, newIntNextHop, newNextHop);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, status);

    /* Store the new nexthop pointer in the next hop table */
    group->nextHops[hopIndex] = newIntNextHop;

    status = ValidateEcmpGroup(sw, group, NULL);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, status);

    FM_API_CALL_FAMILY(status,
                       switchPtr->ReplaceECMPGroupNextHop,
                       sw,
                       group,
                       oldIntNextHop,
                       newIntNextHop);

    if (status == FM_OK)
    {
        fmFree(oldIntNextHop);
    }

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, status);

}   /* end fmReplaceECMPGroupNextHopInternal */




/*****************************************************************************/
/** fmReplaceECMPGroupNextHop
 * \ingroup routerEcmp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Replaces a next-hop in a variable sized ECMP group with
 *                  a new next-hop in a manner that does not affect the
 *                  hashing of the ECMP group using an ''fm_nextHop'' ARP-type 
 *                  next-hop specification. 
 *
 * \note            See ''fmReplaceECMPGroupNextHopV2'' for ''fm_ecmpNextHop'' 
 *                  type next-hop specifications. See ''fmSetECMPGroupNextHops'' 
 *                  for fixed-sized ECMP groups.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupId is the ECMP group ID from a prior call to
 *                  ''fmCreateECMPGroup''.
 *
 * \param[in]       oldNextHop points to the next-hop that is to be replaced.
 *
 * \param[in]       newNextHop points to the next-hop that is to replace
 *                  oldNextHop.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if groupId is invalid.
 * \return          FM_ERR_NO_MEM if memory for the new next-hop cannot be
 *                  allocated.
 * \return          FM_ERR_NOT_FOUND if the old next-hop was not found in the
 *                  ECMP group.
 * \return          FM_ERR_ALREADY_EXISTS if the new next-hop already exists
 *                  in the ECMP group.
 * \return          FM_ERR_UNSUPPORTED if the ECMP group is not a fixed sized
 *                  group.
 *
 *****************************************************************************/
fm_status fmReplaceECMPGroupNextHop(fm_int      sw,
                                    fm_int      groupId,
                                    fm_nextHop *oldNextHop,
                                    fm_nextHop *newNextHop)
{
    fm_status      status;
    fm_switch *    switchPtr;
    fm_bool        routingLockTaken = FALSE;
    fm_ecmpNextHop oldEcmpNextHop;
    fm_ecmpNextHop newEcmpNextHop;

    FM_LOG_ENTRY_API( FM_LOG_CAT_ROUTING,
                      "sw = %d, groupId = %d, oldNextHop = %p, "
                      "newNextHop = %p\n",
                      sw,
                      groupId,
                      (void *) oldNextHop,
                      (void *) newNextHop );

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_CLEAR( oldEcmpNextHop );
    oldEcmpNextHop.type = FM_NEXTHOP_TYPE_ARP;
    FM_MEMCPY_S( &oldEcmpNextHop.data.arp,
                 sizeof(oldEcmpNextHop.data.arp),
                 oldNextHop,
                 sizeof(*oldNextHop) );

    FM_CLEAR( newEcmpNextHop );
    newEcmpNextHop.type = FM_NEXTHOP_TYPE_ARP;
    FM_MEMCPY_S( &newEcmpNextHop.data.arp,
                 sizeof(newEcmpNextHop.data.arp),
                 newNextHop,
                 sizeof(*newNextHop) );

    /* gain exclusive access to routing tables */
    status = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

    routingLockTaken = TRUE;

    status = fmReplaceECMPGroupNextHopInternal(sw,
                                               groupId,
                                               &oldEcmpNextHop,
                                               &newEcmpNextHop);


ABORT:

    if (routingLockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmReplaceECMPGroupNextHop */




/*****************************************************************************/
/** fmReplaceECMPGroupNextHopV2
 * \ingroup routerEcmp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Replaces a next-hop in a variable sized ECMP group with
 *                  a new next-hop in a manner that does not affect the
 *                  hashing of the ECMP group using an ''fm_ecmpNextHop'' type 
 *                  next-hop specification. 
 *
 * \note            See ''fmReplaceECMPGroupNextHop'' for ''fm_nextHop'' 
 *                  type next-hop specifications. See ''fmSetECMPGroupNextHops'' 
 *                  for fixed-sized ECMP groups.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupId is the ECMP group ID from a prior call to
 *                  ''fmCreateECMPGroup''.
 *
 * \param[in]       oldNextHop points to the next-hop that is to be replaced.
 *
 * \param[in]       newNextHop points to the next-hop that is to replace
 *                  oldNextHop.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if groupId is invalid.
 * \return          FM_ERR_NO_MEM if memory for the new next-hop cannot be
 *                  allocated.
 * \return          FM_ERR_NOT_FOUND if the old next-hop was not found in the
 *                  ECMP group.
 * \return          FM_ERR_ALREADY_EXISTS if the new next-hop already exists
 *                  in the ECMP group.
 * \return          FM_ERR_UNSUPPORTED if the ECMP group is not a fixed sized
 *                  group.
 *
 *****************************************************************************/
fm_status fmReplaceECMPGroupNextHopV2(fm_int          sw,
                                      fm_int          groupId,
                                      fm_ecmpNextHop *oldNextHop,
                                      fm_ecmpNextHop *newNextHop)
{
    fm_status  status;
    fm_switch *switchPtr;
    fm_bool    routingLockTaken = FALSE;

    FM_LOG_ENTRY_API( FM_LOG_CAT_ROUTING,
                      "sw = %d, groupId = %d, oldNextHop = %p, "
                      "newNextHop = %p\n",
                      sw,
                      groupId,
                      (void *) oldNextHop,
                      (void *) newNextHop );

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    /* gain exclusive access to routing tables */
    status = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

    routingLockTaken = TRUE;

    status = fmReplaceECMPGroupNextHopInternal(sw,
                                               groupId,
                                               oldNextHop,
                                               newNextHop);


ABORT:

    if (routingLockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmReplaceECMPGroupNextHopV2 */




/*****************************************************************************/
/** fmSetECMPGroupNextHopsInternal
 * \ingroup intRouterEcmp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Stores one or more next-hops for a fixed-size ECMP group.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupId is the ECMP group ID from a prior call to
 *                  ''fmCreateECMPGroup''.
 *
 * \param[in]       firstIndex is the index of the first next-hop in the
 *                  ECMP group which is to be updated.
 *
 * \param[in]       numNextHops contains the number of next-hops to be updated.
 *
 * \param[in]       nextHopList points to an array of next hops to be updated.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if a function parameter is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch,
 *                  if the ECMP group is not a fixed-size ECMP group, or if
 *                  this feature is not supported on the switch.
 *
 *****************************************************************************/
fm_status fmSetECMPGroupNextHopsInternal(fm_int          sw,
                                         fm_int          groupId,
                                         fm_int          firstIndex,
                                         fm_int          numNextHops,
                                         fm_ecmpNextHop *nextHopList)
{
    fm_status          status;
    fm_switch *        switchPtr;
    fm_intEcmpGroup *  group;
    fm_bool            lockTaken;
    fm_bool            wideGroup;
    fm_int             i;

    FM_LOG_ENTRY( FM_LOG_CAT_ROUTING,
                  "sw = %d, groupId = %d, firstIndex = %d, "
                  "numNextHops = %d, nextHopList = %p\n",
                  sw,
                  groupId,
                  firstIndex,
                  numNextHops,
                  (void *) nextHopList );

    switchPtr = GET_SWITCH_PTR(sw);
    lockTaken = FALSE;

    if ( (groupId < 0) || (groupId >= switchPtr->maxArpEntries) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    /* gain exclusive access to routing tables */
    status = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

    lockTaken = TRUE;
    group     = switchPtr->ecmpGroups[groupId];

    if (!group->fixedSize)
    {
        status = FM_ERR_UNSUPPORTED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    if (nextHopList == NULL)
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    if ( (firstIndex < 0) || (firstIndex >= group->maxNextHops) )
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    if ( group->maxNextHops < (firstIndex + numNextHops) )
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    wideGroup = group->wideGroup;

    /* Verify that all next-hops are the same width. */
    for (i = 0 ; i < numNextHops ; i++)
    {
        switch (nextHopList[i].type)
        {
            case FM_NEXTHOP_TYPE_ARP:
            case FM_NEXTHOP_TYPE_RAW_NARROW:
            case FM_NEXTHOP_TYPE_DROP:
                if (wideGroup)
                {
                    status = FM_ERR_MIXING_NARROW_AND_WIDE_NEXTHOPS;
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
                }
                break;

            case FM_NEXTHOP_TYPE_RAW_WIDE:
                if (!wideGroup)
                {
                    status = FM_ERR_MIXING_NARROW_AND_WIDE_NEXTHOPS;
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
                }
                break;

            default:
                status = FM_ERR_INVALID_ARGUMENT;
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
                break;
        }
    }

    /* Change the next-hop information in the hardware. */
    FM_API_CALL_FAMILY(status,
                       switchPtr->SetECMPGroupNextHops,
                       sw,
                       group,
                       firstIndex,
                       numNextHops,
                       nextHopList);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);


ABORT:

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, status);

}   /* end fmSetECMPGroupNextHopsInternal */




/*****************************************************************************/
/** fmSetECMPGroupNextHops
 * \ingroup routerEcmp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Stores one or more next-hops for a fixed-size ECMP group.
 * \desc            Replaces a next-hop in a fixed-sized ECMP group with
 *                  a new next-hop in a manner that does not affect the
 *                  hashing of the ECMP group using an ''fm_ecmpNextHop'' type 
 *                  next-hop specification. 
 *
 * \note            For variable sized ECMP groups, see 
 *                  ''fmReplaceECMPGroupNextHop'' for ''fm_nextHop'' 
 *                  type next-hop specifications and 
 *                  ''fmReplaceECMPGroupNextHopV2'' ''fm_ecmpNextHop'' type 
 *                  next-hop specifications.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupId is the ECMP group ID from a prior call to
 *                  ''fmCreateECMPGroup''.
 *
 * \param[in]       firstIndex is the index of the first next-hop in the
 *                  ECMP group which is to be updated.
 *
 * \param[in]       numNextHops contains the number of next-hops to be updated.
 *
 * \param[in]       nextHopList points to an array of next hops to be updated.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if a function parameter is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch,
 *                  if the ECMP group is not a fixed-size ECMP group, or if
 *                  this feature is not supported on the switch.
 *
 *****************************************************************************/
fm_status fmSetECMPGroupNextHops(fm_int          sw,
                                 fm_int          groupId,
                                 fm_int          firstIndex,
                                 fm_int          numNextHops,
                                 fm_ecmpNextHop *nextHopList)
{
    fm_status status;

    FM_LOG_ENTRY_API( FM_LOG_CAT_ROUTING,
                      "sw = %d, groupId = %d, firstIndex = %d, "
                      "numNextHops = %d, nextHopList = %p\n",
                      sw,
                      groupId,
                      firstIndex,
                      numNextHops,
                      (void *) nextHopList );

    VALIDATE_AND_PROTECT_SWITCH(sw);

    status = fmSetECMPGroupNextHopsInternal(sw,
                                            groupId,
                                            firstIndex,
                                            numNextHops,
                                            nextHopList);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmSetECMPGroupNextHops */




/*****************************************************************************/
/** fmSetECMPGroupRawNextHop
 * \ingroup intRouterEcmp
 *
 * \chips           FM6000
 *
 * \desc            Stores a raw next-hop into an ECMP group.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupId is the ECMP group ID from a prior call to
 *                  ''fmCreateECMPGroup''.
 *
 * \param[in]       index is the index of the next-hop in the ECMP group which
 *                  is to be updated.
 * 
 * \param[in]       nextHopType is the type of raw next hop entry, see
 *                  ''fm_rawNextHopType'' for more details
 *                  
 * \param[in]       value0 is the first 64-bit value in the raw next-hop.
 *
 * \param[in]       value1 is the second 64-bit value in the raw next-hop (for
 *                  raw wide next hop entry only).
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if groupId is invalid.
 *
 *****************************************************************************/
fm_status fmSetECMPGroupRawNextHop(fm_int               sw,
                                   fm_int               groupId,
                                   fm_int               index,
                                   fm_ecmpNextHopType   nextHopType,
                                   fm_uint64            value0,
                                   fm_uint64            value1)
{
    fm_status      status;
    fm_ecmpNextHop nextHop;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, groupId = %d, index = %d, nextHopType = %d, "
                     "value0 = %" FM_FORMAT_64 "X, "
                     "value1 = %" FM_FORMAT_64 "X\n",
                     sw,
                     groupId,
                     index,
                     nextHopType,
                     value0,
                     value1);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    FM_CLEAR( nextHop );

    nextHop.type = nextHopType;
    
    switch (nextHopType)
    {
        case FM_NEXTHOP_TYPE_RAW_NARROW:
            nextHop.data.rawNarrow.value = value0;
            break;

        case FM_NEXTHOP_TYPE_RAW_WIDE:
            nextHop.data.rawWide.values[0] = value0;
            nextHop.data.rawWide.values[1] = value1;
            break;

        default:
            status = FM_ERR_INVALID_ARGUMENT;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
            break;
    }
    
    status = fmSetECMPGroupNextHopsInternal(sw, groupId, index, 1, &nextHop);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

ABORT:

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmSetECMPGroupRawNextHop */




/*****************************************************************************/
/** fmGetECMPGroupFirst
 * \ingroup routerEcmp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Gets the first ECMP group ID.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      firstGroupId points to caller-allocated storage into which
 *                  the first ECMP Group ID will be placed, or -1 if there
 *                  are no ECMP groups.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_NO_MORE if there are no ECMP Groups.
 *
 *****************************************************************************/
fm_status fmGetECMPGroupFirst(fm_int sw, fm_int *firstGroupId)
{
    fm_status        status;
    fm_switch *      switchPtr;
    fm_int           groupId;
    fm_intEcmpGroup *group;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    *firstGroupId = -1;

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxArpEntries > 0)
    {
        status = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

        if (status == FM_OK)
        {
            for (groupId = 0 ; groupId < switchPtr->maxArpEntries ; groupId++)
            {
                group = switchPtr->ecmpGroups[groupId];

                if (group != NULL)
                {
                    break;
                }
            }

            if (groupId < switchPtr->maxArpEntries)
            {
                *firstGroupId = groupId;
                status        = FM_OK;
            }
            else
            {
                status = FM_ERR_NO_MORE;
            }

            fmReleaseReadLock(&switchPtr->routingLock);
        }
    }
    else
    {
        status = FM_ERR_UNSUPPORTED;
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmGetECMPGroupFirst */




/*****************************************************************************/
/** fmGetECMPGroupNext
 * \ingroup routerEcmp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Gets the next ECMP group ID.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       prevGroupId contains the group ID returned by a prior
 *                  call to ''fmGetECMPGroupFirst'' or ''fmGetECMPGroupNext''.
 *
 * \param[out]      nextGroupId points to caller-allocated storage into which
 *                  the next ECMP Group ID will be placed, or -1 if there are
 *                  no more ECMP groups.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_NO_MORE if there are no more ECMP Groups.
 *
 *****************************************************************************/
fm_status fmGetECMPGroupNext(fm_int  sw,
                             fm_int  prevGroupId,
                             fm_int *nextGroupId)
{
    fm_status        status;
    fm_switch *      switchPtr;
    fm_int           groupId;
    fm_intEcmpGroup *group;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    *nextGroupId = -1;

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    status = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    if (status == FM_OK)
    {
        for (groupId = prevGroupId + 1 ;
             groupId < switchPtr->maxArpEntries ;
             groupId++)
        {
            group = switchPtr->ecmpGroups[groupId];

            if (group != NULL)
            {
                break;
            }
        }

        if (groupId < switchPtr->maxArpEntries)
        {
            *nextGroupId = groupId;
            status       = FM_OK;
        }
        else
        {
            status = FM_ERR_NO_MORE;
        }

        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmGetECMPGroupNext */




/*****************************************************************************/
/** fmGetECMPGroupList
 * \ingroup routerEcmp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Returns a list of ECMP Groups.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      numGroups points to caller-allocated storage into which
 *                  will be stored the number of ECMP Groups put into groupList.
 *
 * \param[out]      groupList is an array, max elements in length, that this 
 *                  function will fill with the list of ECMP group IDs.
 *
 * \param[in]       max is the size of groupList, being the maximum number of 
 *                  ECMP group IDs that groupList can hold.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if max is <= 0 or either of the
 *                  pointer arguments is NULL.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_NO_MORE if there are no ECMP Groups.
 * \return          FM_ERR_BUFFER_FULL if max was too small to accommodate
 *                  the entire list of ECMP Groups.
 *
 *****************************************************************************/
fm_status fmGetECMPGroupList(fm_int  sw,
                             fm_int *numGroups,
                             fm_int *groupList,
                             fm_int  max)
{
    fm_status        status;
    fm_switch *      switchPtr;
    fm_int           groupId;
    fm_intEcmpGroup *group;
    fm_int           index;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING, "sw = %d\n", sw);

    if (max <= 0)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    if (numGroups == NULL)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    if (groupList == NULL)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->maxArpEntries > 0)
    {
        status = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

        if (status == FM_OK)
        {
            index = 0;

            for (groupId = 0 ; groupId < switchPtr->maxArpEntries ; groupId++)
            {
                group = switchPtr->ecmpGroups[groupId];

                if (group != NULL)
                {
                    if (index >= max)
                    {
                        status = FM_ERR_BUFFER_FULL;
                        break;
                    }

                    groupList[index] = groupId;
                    index++;
                }
            }

            fmReleaseReadLock(&switchPtr->routingLock);

            if (index == 0)
            {
                status = FM_ERR_NO_MORE;
            }

            *numGroups = index;
        }
    }
    else
    {
        status = FM_ERR_UNSUPPORTED;
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmGetECMPGroupList */




/*****************************************************************************/
/** fmGetECMPGroupNextHopFirst
 * \ingroup routerEcmp
 * 
 * \chips           FM4000, FM6000
 *
 * \desc            Gets the first next-hop for an ECMP group using an
 *                  ''fm_nextHop'' ARP-type next-hop specification. 
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupId is the ECMP group ID from a prior call to
 *                  ''fmCreateECMPGroup''.
 *
 * \param[out]      searchToken points to caller-allocated storage into which
 *                  the function will place a search token used for future
 *                  calls to fmGetECMPGroupNextHopNext.
 *
 * \param[out]      firstNextHop points to caller-allocated storage into which
 *                  the first next-hop address will be placed.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if groupId is not a valid group
 *                  or either firstNextHop or searchToken are NULL.
 * \return          FM_ERR_NO_MORE if there are no next-hops in the ECMP group.
 *
 *****************************************************************************/
fm_status fmGetECMPGroupNextHopFirst(fm_int      sw,
                                     fm_int      groupId,
                                     fm_int *    searchToken,
                                     fm_nextHop *firstNextHop)
{
    fm_status              status;
    fm_switch *            switchPtr;
    fm_intEcmpGroup *      group;
    
    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, groupId = %d, searchToken = %p, "
                     "firstNextHop = %p\n",
                     sw,
                     groupId,
                     (void *) searchToken,
                     (void *) firstNextHop);

    if (searchToken == NULL)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    if (firstNextHop == NULL)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if ( (groupId < 0) || (groupId >= switchPtr->maxArpEntries) )
    {
        status = FM_ERR_INVALID_ARGUMENT;
    }
    else
    {
        status = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

        if (status == FM_OK)
        {
            group = switchPtr->ecmpGroups[groupId];

            if (group == NULL)
            {
                status = FM_ERR_INVALID_ARGUMENT;
            }
            else if (group->nextHopCount <= 0)
            {
                status = FM_ERR_NO_MORE;
            }
            else
            {
                FM_MEMCPY_S( firstNextHop,
                             sizeof(*firstNextHop),
                             &group->nextHops[0]->nextHop.data.arp,
                             sizeof(fm_nextHop) );
                *searchToken = 0;
            }

            fmReleaseReadLock(&switchPtr->routingLock);
        }
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmGetECMPGroupNextHopFirst */




/*****************************************************************************/
/** fmGetECMPGroupNextHopNext
 * \ingroup routerEcmp
 * 
 * \chips           FM4000, FM6000
 *
 * \desc            Gets the next next-hop address for an ECMP group using an
 *                  ''fm_nextHop'' ARP-type next-hop specification.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupId is the ECMP group ID from a prior call to
 *                  ''fmCreateECMPGroup''.
 *
 * \param[out]      searchToken points to caller-allocated storage containing
 *                  a search token provided by an earlier call to
 *                  ''fmGetECMPGroupNextHopFirst'' or 
 *                  ''fmGetECMPGroupNextHopNext''.
 *
 * \param[out]      nextHop points to caller-allocated storage into which
 *                  the next next-hop address will be placed.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if groupId is not a valid group.
 * \return          FM_ERR_NO_MORE if there are no more next-hops in the ECMP 
 *                  group.
 *
 *****************************************************************************/
fm_status fmGetECMPGroupNextHopNext(fm_int      sw,
                                    fm_int      groupId,
                                    fm_int *    searchToken,
                                    fm_nextHop *nextHop)
{
    fm_status              status;
    fm_switch *            switchPtr;
    fm_intEcmpGroup *      group;
    fm_int                 index;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, groupId = %d, searchToken = %p, nextHop = %p\n",
                     sw,
                     groupId,
                     (void *) searchToken,
                     (void *) nextHop);

    if (searchToken == NULL)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    if (nextHop == NULL)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if ( (groupId < 0) || (groupId >= switchPtr->maxArpEntries) )
    {
        status = FM_ERR_INVALID_ARGUMENT;
    }
    else
    {
        status = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

        if (status == FM_OK)
        {
            index = *searchToken + 1;
            group = switchPtr->ecmpGroups[groupId];

            if (group == NULL)
            {
                status = FM_ERR_INVALID_ARGUMENT;
            }
            else if (index < group->nextHopCount)
            {
                FM_MEMCPY_S( nextHop,
                             sizeof(*nextHop),
                             &group->nextHops[index]->nextHop.data.arp,
                             sizeof(fm_nextHop) );
                *searchToken = index;
            }
            else
            {
                status = FM_ERR_NO_MORE;
            }

            fmReleaseReadLock(&switchPtr->routingLock);
        }
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmGetECMPGroupNextHopNext */




/*****************************************************************************/
/** fmGetECMPGroupNextHopListInternal
 * \ingroup intRouterArp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Returns a list of nexthop addresses attached to an ECMP group.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       groupId is the ECMP group id provided by an earlier call
 *                  to fmCreateECMPGroup.
 *
 * \param[out]      numNextHops points to caller-allocated storage into which
 *                  will be stored the number of nexthop addresses put into
 *                  nextHopList.
 *
 * \param[out]      nextHopList points to caller-allocated storage into which
 *                  the list of nexthop addresses will be placed.  The allocated
 *                  storage must have sufficient room for 'max' nexthop
 *                  addresses, or memory corruption may result.
 *
 * \param[in]       max is the maximum number of nexthop addresses that may be
 *                  placed into 'nextHopList'.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if groupId is not a valid group,
 *                  either numNextHops or nextHopList are NULL,
 *                  or max is not >= 1.
 * \return          FM_ERR_NO_MORE if there are no nexthops in the ECMP group.
 * \return          FM_ERR_BUFFER_FULL if max was too small to accommodate
 *                  the entire list of nexthop addresses attached to the
 *                  ECMP group.
 *
 *****************************************************************************/
fm_status fmGetECMPGroupNextHopListInternal(fm_int          sw,
                                            fm_int          groupId,
                                            fm_int *        numNextHops,
                                            fm_ecmpNextHop *nextHopList,
                                            fm_int          max)
{
    fm_status        status;
    fm_switch *      switchPtr;
    fm_intEcmpGroup *group;
    fm_ecmpNextHop * nextHopPtr;
    fm_int           nextHopIndex;
    fm_int           index;

    FM_LOG_ENTRY( FM_LOG_CAT_ROUTING,
                  "sw = %d, groupId = %d, numNexthops = %p, "
                  "nextHopList = %p, max = %d\n",
                  sw,
                  groupId,
                  (void *) numNextHops,
                  (void *) nextHopList,
                  max );

    if (numNextHops == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    if (nextHopList == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    if (max <= 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    switchPtr = GET_SWITCH_PTR(sw);

    if ( (groupId < 0) || (groupId >= switchPtr->maxArpEntries) )
    {
        status = FM_ERR_INVALID_ARGUMENT;
    }
    else
    {
        status = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

        if (status == FM_OK)
        {
            group        = switchPtr->ecmpGroups[groupId];
            nextHopIndex = 0;
            nextHopPtr   = nextHopList;

            for (index = 0 ; index < group->nextHopCount ; index++)
            {
                if (nextHopIndex < max)
                {
                    FM_MEMCPY_S( nextHopPtr,
                                 sizeof(*nextHopPtr),
                                 &group->nextHops[index]->nextHop,
                                 sizeof(fm_ecmpNextHop) );
                    nextHopPtr++;
                    nextHopIndex++;
                }
                else
                {
                    status = FM_ERR_BUFFER_FULL;
                    break;
                }
            }

            fmReleaseReadLock(&switchPtr->routingLock);

            if (nextHopIndex <= 0)
            {
                status = FM_ERR_NO_MORE;
            }

            *numNextHops = nextHopIndex;
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, status);

}   /* end fmGetECMPGroupNextHopListInternal */




/*****************************************************************************/
/** fmGetECMPGroupNextHopList
 * \ingroup routerEcmp
 * 
 * \chips           FM4000, FM6000
 *
 * \desc            Returns a list of next-hop addresses in an ECMP group 
 *                  using an ''fm_nextHop'' ARP-type next-hop specification.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupId is the ECMP group ID from a prior call to
 *                  ''fmCreateECMPGroup''.
 *
 * \param[out]      numNextHops points to caller-allocated storage into which
 *                  will be stored the number of next-hop addresses put into
 *                  nextHopList.
 *
 * \param[out]      nextHopList is an array, max elements in length, that this 
 *                  function will fill with the list of next-hops in the ECMP 
 *                  group.
 *
 * \param[in]       max is the size of nextHopList, being the maximum number of 
 *                  next-hops that nextHopList can hold.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if groupId is not a valid group,
 *                  either numNextHops or nextHopList are NULL, or max is 
 *                  not >= 1.
 * \return          FM_ERR_NO_MORE if there are no next-hops in the ECMP group.
 * \return          FM_ERR_BUFFER_FULL if max was too small to accommodate
 *                  the entire list of next-hop addresses in the ECMP group.
 *
 *****************************************************************************/
fm_status fmGetECMPGroupNextHopList(fm_int      sw,
                                    fm_int      groupId,
                                    fm_int *    numNextHops,
                                    fm_nextHop *nextHopList,
                                    fm_int      max)
{
    fm_status       status;
    fm_ecmpNextHop *ecmpNextHopList;
    fm_int          i;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, groupId = %d, numNexthops = %p, "
                     "nextHopList = %p, max = %d\n",
                     sw,
                     groupId,
                     (void *) numNextHops,
                     (void *) nextHopList,
                     max);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    ecmpNextHopList = fmAlloc( sizeof(fm_ecmpNextHop) * max );

    if (ecmpNextHopList == NULL)
    {
        status = FM_ERR_NO_MEM;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    status = fmGetECMPGroupNextHopListInternal(sw,
                                               groupId,
                                               numNextHops,
                                               ecmpNextHopList,
                                               max);

    if ( (status != FM_OK) && (status != FM_ERR_BUFFER_FULL) )
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    for (i = 0 ; i < *numNextHops ; i++)
    {
        FM_MEMCPY_S( &nextHopList[i],
                     sizeof(nextHopList[i]),
                     &ecmpNextHopList[i].data.arp,
                     sizeof(fm_nextHop) );
    }


ABORT:

    if (ecmpNextHopList != NULL)
    {
        fmFree(ecmpNextHopList);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmGetECMPGroupNextHopList */




/*****************************************************************************/
/** fmGetECMPGroupNextHopUsed
 * \ingroup routerEcmp
 * 
 * \chips           FM4000, FM6000
 *
 * \desc            Retrieves the "used" flag for a next-hop using an
 *                  ''fm_nextHop'' ARP-type next-hop specification.
 * 
 * \note            See ''fmGetECMPGroupNextHopUsedV2'' for ''fm_ecmpNextHop'' 
 *                  type next-hop specifications.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupId is the ECMP group ID from a prior call to
 *                  ''fmCreateECMPGroup''.
 *
 * \param[in]       nextHop points to the next-hop entry to be accessed.
 *
 * \param[out]      used points to caller-allocated storage where this function
 *                  should place the result.
 *
 * \param[in]       resetFlag specifies whether the flag should be reset
 *                  after it is read.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_NOT_FOUND if the specified ARP entry is not
 *                  recognized.
 *
 *****************************************************************************/
fm_status fmGetECMPGroupNextHopUsed(fm_int      sw,
                                    fm_int      groupId,
                                    fm_nextHop *nextHop,
                                    fm_bool *   used,
                                    fm_bool     resetFlag)
{
    fm_status      err;
    fm_ecmpNextHop ecmpNextHop;

    FM_LOG_ENTRY_API( FM_LOG_CAT_ROUTING,
                      "sw=%d, groupId=%d, nextHop=%p, used=%p, resetFlag=%d\n",
                      sw,
                      groupId,
                      (void *) nextHop,
                      (void *) used,
                      resetFlag );

    VALIDATE_AND_PROTECT_SWITCH(sw);

    FM_CLEAR( ecmpNextHop );

    ecmpNextHop.type = FM_NEXTHOP_TYPE_ARP;
    FM_MEMCPY_S( &ecmpNextHop.data.arp,
                 sizeof(ecmpNextHop.data.arp),
                 nextHop,
                 sizeof(*nextHop) );

    err = fmGetECMPGroupNextHopUsedInternal(sw,
                                            groupId,
                                            &ecmpNextHop,
                                            used,
                                            resetFlag);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetECMPGroupNextHopUsed */




/*****************************************************************************/
/** fmGetECMPGroupNextHopUsedV2
 * \ingroup routerEcmp
 * 
 * \chips           FM4000, FM6000
 *
 * \desc            Retrieves the "used" flag for a next-hop using an
 *                  ''fm_ecmpNextHop'' type next-hop specification.
 * 
 * \note            See ''fmGetECMPGroupNextHopUsed'' for ''fm_nextHop'' 
 *                  ARP-type next-hop specifications.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupId is the ECMP group ID from a prior call to
 *                  ''fmCreateECMPGroup''.
 *
 * \param[in]       nextHop points to the next-hop entry to be accessed.
 *
 * \param[out]      used points to caller-allocated storage where this function
 *                  should place the result.
 *
 * \param[in]       resetFlag specifies whether the flag should be reset
 *                  after it is read.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_NOT_FOUND if the specified ARP entry is not
 *                  recognized.
 *
 *****************************************************************************/
fm_status fmGetECMPGroupNextHopUsedV2(fm_int          sw,
                                      fm_int          groupId,
                                      fm_ecmpNextHop *nextHop,
                                      fm_bool *       used,
                                      fm_bool         resetFlag)
{
    fm_status err;

    FM_LOG_ENTRY_API( FM_LOG_CAT_ROUTING,
                      "sw=%d, groupId=%d, nextHop=%p, used=%p, resetFlag=%d\n",
                      sw,
                      groupId,
                      (void *) nextHop,
                      (void *) used,
                      resetFlag );

    VALIDATE_AND_PROTECT_SWITCH(sw);

    err = fmGetECMPGroupNextHopUsedInternal(sw,
                                            groupId,
                                            nextHop,
                                            used,
                                            resetFlag);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetECMPGroupNextHopUsedV2 */




/*****************************************************************************/
/** fmGetECMPGroupNextHopUsedInternal
 * \ingroup intRouterArp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Retrieves the "used" flag for a next hop.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupId is the ECMP group id.
 *
 * \param[in]       nextHop points to the next-hop entry to be accessed.
 *
 * \param[out]      used points to caller-allocated storage where this function
 *                  should place the result.
 *
 * \param[in]       resetFlag specifies whether the flag should be reset
 *                  after it is read.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_NOT_FOUND if the specified ARP entry is not
 *                  recognized.
 *
 *****************************************************************************/
fm_status fmGetECMPGroupNextHopUsedInternal(fm_int          sw,
                                            fm_int          groupId,
                                            fm_ecmpNextHop *nextHop,
                                            fm_bool *       used,
                                            fm_bool         resetFlag)
{
    fm_switch *      switchPtr;
    fm_status        err;
    fm_intEcmpGroup *group;
    fm_intNextHop *  intNextHop;
    fm_bool          nextHopUsed;

    FM_LOG_ENTRY( FM_LOG_CAT_ROUTING,
                  "sw=%d, groupId=%d, nextHop=%p, used=%p, resetFlag=%d\n",
                  sw,
                  groupId,
                  (void *) nextHop,
                  (void *) used,
                  resetFlag );

    switchPtr = GET_SWITCH_PTR(sw);

    *used = FALSE;

    if (switchPtr->maxArpEntries <= 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_UNSUPPORTED);
    }

    if ( (groupId < 0) || (groupId >= switchPtr->maxArpEntries) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    err = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

    if (err != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, err);
    }

    group = switchPtr->ecmpGroups[groupId];

    if (group == NULL)
    {
        err = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    intNextHop = FindNextHop(sw, group, nextHop, NULL);

    if (intNextHop == NULL)
    {
        err = FM_ERR_NOT_FOUND;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, err);
    }

    FM_API_CALL_FAMILY(err,
                       switchPtr->GetNextHopUsed,
                       sw,
                       intNextHop,
                       &nextHopUsed,
                       resetFlag);

    if (err == FM_OK)
    {
        if (nextHopUsed)
        {
            *used = TRUE;
        }
    }

ABORT:

    fmReleaseReadLock(&switchPtr->routingLock);

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, err);

}   /* end fmGetECMPGroupNextHopUsedInternal */




/*****************************************************************************/
/** fmGetECMPGroupRouteCount
 * \ingroup routerEcmp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            Gets the number of routes attached to an ECMP group.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupId is the ECMP group ID from a prior call to
 *                  ''fmCreateECMPGroup''.
 *
 * \param[out]      routeCountPtr points to caller-allocated storage into which
 *                  the function will place the number of routes attached to
 *                  the ECMP group.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if groupId is not a valid group
 *                  or routeCountPtr is NULL.
 *
 *****************************************************************************/
fm_status fmGetECMPGroupRouteCount(fm_int  sw,
                                   fm_int  groupId,
                                   fm_int *routeCountPtr)
{
    fm_status        status;
    fm_switch *      switchPtr;
    fm_intEcmpGroup *group;
    fm_uint          count;
    
    FM_LOG_ENTRY_API(FM_LOG_CAT_ROUTING,
                     "sw = %d, groupId = %d, routeCountPtr = %p\n",
                     sw,
                     groupId,
                     (void *) routeCountPtr);

    if (routeCountPtr == NULL)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, FM_ERR_INVALID_ARGUMENT);
    }

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if ( (groupId < 0) || (groupId >= switchPtr->maxArpEntries) )
    {
        status = FM_ERR_INVALID_ARGUMENT;
    }
    else
    {
        status = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);

        if (status == FM_OK)
        {
            group = switchPtr->ecmpGroups[groupId];

            if (group == NULL)
            {
                status = FM_ERR_INVALID_ARGUMENT;
            }
            else
            {
                count = fmCustomTreeSize(&group->routeTree);

                *routeCountPtr = (fm_int) count;
            }

            fmReleaseReadLock(&switchPtr->routingLock);
        }
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmGetECMPGroupRouteCount */




/*****************************************************************************/
/** fmInitializeNextHop
 * \ingroup intRouterArp
 *
 * \desc            Initializes a next-hop record for use in an ECMP group.
 *
 * \note            This function assumes that the routing lock has already
 *                  been taken.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       group points to the ECMP group.
 *
 * \param[in]       intNextHop points to the user-interface next-hop record to
 *                  be initialized.
 *
 * \param[in]       nextHop optionally points to the application-provided
 *                  fm_ecmpNextHop record. If present, the contents will
 *                  be copied into the intNextHop record. Use NULL if no
 *                  copy is needed.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmInitializeNextHop(fm_int           sw,
                              fm_intEcmpGroup *group,
                              fm_intNextHop *  intNextHop,
                              fm_ecmpNextHop * nextHop)
{
    fm_status                      status;
    fm_switch *                    switchPtr;
    fm_nextHop *                   arpNextHop;
    fm_uint16                      vlan;
    fm_intArpEntry *               arpEntry;
    fm_intIpInterfaceAddressEntry *addrEntry;

    FM_LOG_ENTRY( FM_LOG_CAT_ROUTING,
                  "sw = %d, group = %p (%d), intNextHop = %p, nextHop = %p\n",
                  sw,
                  (void *) group,
                  group->groupId,
                  (void *) intNextHop,
                  (void *) nextHop );

    switchPtr = GET_SWITCH_PTR(sw);

    /* If an ECMP next-hop record was provided, copy it into the internal
     * record. */
    if (nextHop != NULL)
    {
        FM_MEMCPY_S( &intNextHop->nextHop,
                     sizeof(intNextHop->nextHop),
                     nextHop,
                     sizeof(*nextHop) );
    }

    /* Now use the internal copy of the next-hop information */
    nextHop = &intNextHop->nextHop;

    if (nextHop->type == FM_NEXTHOP_TYPE_ARP)
    {
        arpNextHop = &nextHop->data.arp;

        FM_API_CALL_FAMILY(status,
                           switchPtr->ValidateNextHopTrapCode,
                           sw,
                           arpNextHop);

        if (status == FM_ERR_UNSUPPORTED)
        {
            status = FM_OK;
        }
        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, status);

        /* Validate the interface */
        status = fmFindInterfaceAddrEntry(sw,
                                          &arpNextHop->interfaceAddr,
                                          &addrEntry);

        if (status != FM_OK)
        {
            if (status != FM_ERR_INVALID_INTERFACE)
            {
                FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, status);
            }

            /* next-hop specifies an interface IP address that doesn't
             * exist.  This is not an error, but will prevent the next-hop
             * from being used by the hardware until the interface IP address
             * is added to an interface.
             */
            addrEntry = NULL;
        }

        /* Try to get the vlan.  If the interface was specified but the
         * address doesn't exist, the vlan will be set to invalid.  This is
         * not an error, but will prevent the next-hop address from being
         * used by the hardware until the interface IP address is added
         * to an interface.
         */
        vlan = fmGetInterfaceVlan(sw,
                                  &arpNextHop->interfaceAddr,
                                  arpNextHop->vlan);

        /* try to find a matching ARP entry */
        status = fmFindArpEntry(sw,
                                &arpNextHop->addr,
                                vlan,
                                &arpEntry);

        if (status == FM_ERR_NOT_FOUND)
        {
            arpEntry = NULL;
        }
        else if (status != FM_OK)
        {
            FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, status);
        }
    }
    else
    {
        vlan      = 0;
        addrEntry = NULL;
        arpEntry  = NULL;
    }

    intNextHop->sw                    = sw;
    intNextHop->ecmpGroup             = group;
    intNextHop->arp                   = arpEntry;
    intNextHop->vlan                  = vlan;
    intNextHop->oldVlan               = vlan;
    intNextHop->interfaceAddressEntry = addrEntry;
    intNextHop->state                 = FM_NEXT_HOP_STATE_UP;


    if (addrEntry != NULL)
    {
        status = fmCustomTreeInsert(&addrEntry->nextHopTree,
                                    intNextHop,
                                    intNextHop);
        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, status);

        FM_LOG_DEBUG( FM_LOG_CAT_ROUTING,
                      "NextHop %p added to ifEntry %d next-hop-tree\n",
                      (void *) intNextHop,
                      addrEntry->ifEntry->interfaceNum );
    }
    else if ( !fmIsIPAddressEmpty(&intNextHop->nextHop.data.arp.interfaceAddr) )
    {
        status = fmCustomTreeInsert(&switchPtr->noInterfaceNextHops,
                                    intNextHop,
                                    intNextHop);
        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, status);
        FM_LOG_DEBUG( FM_LOG_CAT_ROUTING,
                      "NextHop %p added to no-interface-next-hop-tree\n",
                      (void *) intNextHop );
    }

    if (nextHop->type == FM_NEXTHOP_TYPE_ARP)
    {
        /* If an ARP entry doesn't exist for this next-hop, add the next-hop
         * to the noArpNextHops tree so that it can be quickly found when
         * the ARP is finally added.
         */
        if (arpEntry == NULL)
        {
            status = fmCustomTreeInsert(&switchPtr->noArpNextHops,
                                        intNextHop,
                                        intNextHop);
            FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, status);
            FM_LOG_DEBUG( FM_LOG_CAT_ROUTING,
                          "next hop %p added to noArpNextHops tree\n",
                          (void *) intNextHop );
        }
        else
        {
            /* Add the next-hop to the ARP record's nexthop list */
            status = fmCustomTreeInsert(&arpEntry->nextHopTree,
                                        intNextHop,
                                        intNextHop);
            FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, status);
            FM_LOG_DEBUG( FM_LOG_CAT_ROUTING,
                          "next hop %p added to arp %p next-hop tree\n",
                          (void *) intNextHop,
                          (void *) arpEntry );
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_OK);

}   /* end fmInitializeNextHop */




/*****************************************************************************/
/** fmGetECMPGroupNextHopIndexRange
 * \ingroup intRouterEcmp
 *
 * \chips           FM4000, FM6000
 *
 * \desc            This function returns the hardware next-hop index numbers
 *                  for the first and last next-hop in an ECMP group.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       groupId is the ECMP group ID number.
 *
 * \param[out]      firstIndex points to caller-provided storage into which
 *                  the first hardware record index will be written.
 *                  May be NULL, in which case the first index is not
 *                  returned.
 *
 * \param[out]      lastIndex points to caller-provided storage into which
 *                  the last hardware record index will be written.
 *                  May be NULL, in which case the last index will not be
 *                  returned.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if the groupId is not valid.
 * \return          FM_ERR_UNSUPPORTED if the switch does not support this
 *                  request.
 *
 *****************************************************************************/
fm_status fmGetECMPGroupNextHopIndexRange(fm_int  sw,
                                          fm_int  groupId,
                                          fm_int *firstIndex,
                                          fm_int *lastIndex)
{
    fm_status        status;
    fm_switch *      switchPtr;
    fm_bool          lockTaken;
    fm_intEcmpGroup *group;

    FM_LOG_ENTRY_API( FM_LOG_CAT_ROUTING,
                      "sw = %d, groupId = %d, firstIndex = %p, lastIndex = %p\n",
                      sw,
                      groupId,
                      (void *) firstIndex,
                      (void *) lastIndex );

    lockTaken = FALSE;

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    /* gain read access to routing tables */
    status = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

    lockTaken = TRUE;

    group = switchPtr->ecmpGroups[groupId];

    if (group == NULL)
    {
        status = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);
    }

    FM_API_CALL_FAMILY(status,
                       switchPtr->GetECMPGroupNextHopIndexRange,
                       sw,
                       group,
                       firstIndex,
                       lastIndex);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

ABORT:

    if (lockTaken)
    {
        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmGetECMPGroupNextHopIndexRange */




/*****************************************************************************/
/** fmGetNextHopIndexUsed
 * \ingroup intRouterEcmp
 * 
 * \chips           FM4000, FM6000
 *
 * \desc            Retrieves the "used" flag for a next-hop given the
 *                  index of the next-hop within the hardware next-hop table.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       index is the index number in the hardware next-hop table.
 *
 * \param[out]      used points to caller-allocated storage where this function
 *                  should place the result.
 *
 * \param[in]       resetFlag specifies whether the used flag should be reset
 *                  after it is read.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if routing is not available on the switch.
 * \return          FM_ERR_INVALID_ARGUMENT if the index value is invalid.
 *
 *****************************************************************************/
fm_status fmGetNextHopIndexUsed(fm_int   sw,
                                fm_int   index,
                                fm_bool *used,
                                fm_bool  resetFlag)
{
    fm_status        status;
    fm_switch *      switchPtr;
    fm_bool          lockTaken;

    FM_LOG_ENTRY_API( FM_LOG_CAT_ROUTING,
                      "sw = %d, index = %d, used = %p, resetFlag = %d\n",
                      sw,
                      index,
                      (void *) used,
                      resetFlag );

    lockTaken = FALSE;

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    /* gain read access to routing tables */
    status = fmCaptureReadLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

    lockTaken = TRUE;

    FM_API_CALL_FAMILY(status,
                       switchPtr->GetNextHopIndexUsed,
                       sw,
                       index,
                       used,
                       resetFlag);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ROUTING, status);

ABORT:

    if (lockTaken)
    {
        fmReleaseReadLock(&switchPtr->routingLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ROUTING, status);

}   /* end fmGetNextHopIndexUsed */




/*****************************************************************************/
/** fmDeleteArpNextHopFromTrees
 * \ingroup intRouterArp
 *
 * \desc            Removes an ARP next-hop from the interface and ARP
 *                  custom trees.
 *
 * \note            This function assumes that the routing lock has already
 *                  been taken.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       intNextHop points to the internal next-hop record.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmDeleteArpNextHopFromTrees(fm_int sw, fm_intNextHop *intNextHop)
{
    fm_status  status;
    fm_switch *switchPtr;

    FM_LOG_ENTRY( FM_LOG_CAT_ROUTING,
                  "sw = %d, intNextHop = %p\n",
                  sw,
                  (void *) intNextHop );

    switchPtr = GET_SWITCH_PTR(sw);

    if (intNextHop->nextHop.type != FM_NEXTHOP_TYPE_ARP)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_OK);
    }

    if (intNextHop->interfaceAddressEntry != NULL)
    {
        status = fmCustomTreeRemove(&intNextHop->interfaceAddressEntry->nextHopTree,
                                    intNextHop,
                                    NULL);

        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, status);
        FM_LOG_DEBUG( FM_LOG_CAT_ROUTING,
                      "NextHop %p removed from ifEntry %d next-hop-tree\n",
                      (void *) intNextHop,
                      intNextHop->interfaceAddressEntry->ifEntry->interfaceNum );
    }
    else if ( !fmIsIPAddressEmpty(&intNextHop->nextHop.data.arp.interfaceAddr) )
    {
        status = fmCustomTreeRemove(&switchPtr->noInterfaceNextHops,
                                    intNextHop,
                                    NULL);
        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, status);
        FM_LOG_DEBUG( FM_LOG_CAT_ROUTING,
                      "NextHop %p removed from no-interface-next-hops tree\n",
                      (void *) intNextHop );
    }

    /* If an ARP entry doesn't exist for this next-hop, try to remove the
     * next-hop record from the noArpNextHops tree.  If it isn't there,
     * this is unexpected, but no reason to abort, so simply ignore the
     * returned status.
     */
    if (intNextHop->arp == NULL)
    {
        fmCustomTreeRemove(&switchPtr->noArpNextHops, intNextHop, NULL);
        FM_LOG_DEBUG( FM_LOG_CAT_ROUTING,
                      "next hop %p removed from noArpNextHops tree\n",
                      (void *) intNextHop );
    }
    else
    {
        /* Remove the next-hop from the ARP record's nexthop list */
        status = fmCustomTreeRemove(&intNextHop->arp->nextHopTree,
                                    intNextHop,
                                    NULL);

        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ROUTING, status);
        FM_LOG_DEBUG( FM_LOG_CAT_ROUTING,
                      "next hop %p removed from arp %p next-hop tree\n",
                      (void *) intNextHop,
                      (void *) &intNextHop->arp );
    }

    FM_LOG_EXIT(FM_LOG_CAT_ROUTING, FM_OK);

}   /* end fmDeleteArpNextHopFromTrees */




/*****************************************************************************/
/** fmDbgTestRouteMask
 * \ingroup intRouter
 *
 * \desc            Tests the fmApplyMasksToRoute function.
 *
 * \param[in]       route points to the route string.
 *
 * \param[in]       prefix is the prefix length.
 *
 * \param[in]       routeType is the route type.
 *
 * \param[in]       mcastType is the  multicast type, if route type is multicast.
 *
 * \return          Nothing.
 *
 *****************************************************************************/
void fmDbgTestRouteMask(fm_char *               route,
                        fm_int                  prefix,
                        fm_routeType            routeType,
                        fm_multicastAddressType mcastType)
{
    fm_status     status;
    fm_routeEntry routeEntry;
    fm_char       buf[100];
    fm_ipAddr *   addrPtr;

    FM_CLEAR(routeEntry);

    routeEntry.routeType = routeType;

    switch (routeType)
    {
        case FM_ROUTE_TYPE_UNICAST:
            addrPtr = &routeEntry.data.unicast.dstAddr;
            routeEntry.data.unicast.prefixLength = prefix;
            break;

        case FM_ROUTE_TYPE_UNICAST_ECMP:
            addrPtr = &routeEntry.data.unicastECMP.dstAddr;
            routeEntry.data.unicastECMP.prefixLength = prefix;
            break;

        case FM_ROUTE_TYPE_MULTICAST:
            routeEntry.data.multicast.addressType = mcastType;

            switch (mcastType)
            {
                case FM_MCAST_ADDR_TYPE_L2MAC_VLAN:
                    FM_LOG_PRINT("L2MAC_VLAN mcast invalid\n");
                    return;

                case FM_MCAST_ADDR_TYPE_DSTIP:
                    addrPtr = &routeEntry.data.multicast.info.dstIpRoute.dstAddr;
                    routeEntry.data.multicast.info.dstIpRoute.dstPrefixLength = prefix;
                    break;

                case FM_MCAST_ADDR_TYPE_DSTIP_VLAN:
                    addrPtr = &routeEntry.data.multicast.info.dstIpVlanRoute.dstAddr;
                    routeEntry.data.multicast.info.dstIpVlanRoute.dstPrefixLength = prefix;
                    break;

                case FM_MCAST_ADDR_TYPE_DSTIP_SRCIP:
                    addrPtr = &routeEntry.data.multicast.info.dstSrcIpRoute.dstAddr;
                    routeEntry.data.multicast.info.dstSrcIpRoute.dstPrefixLength = prefix;
                    break;

                case FM_MCAST_ADDR_TYPE_DSTIP_SRCIP_VLAN:
                    addrPtr = &routeEntry.data.multicast.info.dstSrcIpVlanRoute.dstAddr;
                    routeEntry.data.multicast.info.dstSrcIpVlanRoute.dstPrefixLength = prefix;
                    break;

                default:
                    FM_LOG_PRINT("Unknown multicast type %d\n", mcastType);
                    return;
            }
            break;

        default:
            FM_LOG_PRINT("Unknown route type %d\n", routeType);
            return;
    }

    fmDbgConvertStringToIPAddress(route, FALSE, addrPtr);

    status = fmApplyMasksToRoute(&routeEntry);

    if (status != FM_OK)
    {
        FM_LOG_PRINT( "fmApplyMasksToRoute failed: %s\n", fmErrorMsg(status) );
    }
    else
    {
        fmDbgConvertIPAddressToString(addrPtr, buf);
        FM_LOG_PRINT("masked address is: %s\n", buf);
    }

}   /* end fmDbgTestRouteMask*/

