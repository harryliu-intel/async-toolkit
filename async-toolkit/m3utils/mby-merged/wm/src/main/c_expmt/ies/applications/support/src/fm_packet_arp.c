/* vim:et:sw=4:ts=4:tw=79:
 * (No tabs, indent level is 4 spaces, text width is 79)  */

/*****************************************************************************
 * File:            fm_packet_arp.c
 * Creation Date:   October 26, 2007
 * Description:     
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

#include <fm_support.h>
#include <fm_support_int.h>

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

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/**
 * \desc        parses an ARP packet
 *
 * \param[in]   event the packet receive event associated with the ARP packet
 *              to be parsed
 *
 * \param[in]   packet the parsed packet
 *
 * \param[out]  arp points to user-allocated storage in which to store the
 *              information contained in the ARP packet
 *
 * \return      FM_OK if the ARP packet has been parsed successfully
 * \return      FM_ERR_BAD_BUFFER if the packet receive event does not contain
 *              a valid packet buffer
 *
 *****************************************************************************/
static fm_status PacketParseARP(fm_eventPktRecv *event,
                                fm_packet       *packet,
                                fm_packetArp    *arp)
{
    fm_buffer *buffer = (fm_buffer *) event->pkt;
    int       p = packet->layer2;
    int       index, hlen, plen, shift;

    FM_LOG_ENTRY_NOARGS(FM_LOG_CAT_EVENT_PKT_RX);

    if (buffer == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, FM_ERR_BAD_BUFFER);
    }

    memset((void *) arp, 0, sizeof(fm_packetArp));

    /* Set the VLAN ID.  */
    arp->vlanID = event->vlan;

    /* Ignore the hardware type field.  */
    FM_PACKET_ADVANCE_P_HALF_WORD(p);

    /* Parse the protocol type field.  */
    arp->ptype = (fm_uint16) FM_PACKET_GET_HALF_WORD(buffer, p);
    FM_PACKET_ADVANCE_P_HALF_WORD(p);

    /* Parse the hardware length field.  */
    hlen = (int) FM_PACKET_GET_BYTE(buffer, p);
    FM_PACKET_ADVANCE_P_BYTE(p);

    /* Parse the protocol length field.  */
    plen = (int) FM_PACKET_GET_BYTE(buffer, p);
    FM_PACKET_ADVANCE_P_BYTE(p);

    /* Parse the operation field.  */
    arp->oper = (fm_uint16) FM_PACKET_GET_HALF_WORD(buffer, p);
    FM_PACKET_ADVANCE_P_HALF_WORD(p);

    /* Parse the source hardware address.  */
    for (int i = 0; i < hlen; i++)
    {
        shift = (hlen - (i + 1)) << 3;
        arp->sha |= ((fm_macaddr) FM_PACKET_GET_BYTE(buffer, p)) << shift;
        FM_PACKET_ADVANCE_P_BYTE(p);
    }

    /* Parse the source protocol address.  */
    arp->spa.isIPv6 = arp->ptype == FM_PACKET_ETHER_TYPE_IPv6;
    for (int i = 0; i < plen; i++)
    {
        /* The IP address is stored in network byte order. For IPv4 only
         * addr[0] is used. For IPv6 addr[0] contains the least significant 32
         * bits.  */
        index = arp->spa.isIPv6 ? 3 - (i >> 2) : i >> 2;
        shift = (3 - (i % 4)) << 3;
        arp->spa.addr[index] |= (FM_PACKET_GET_BYTE(buffer, p)) << shift;
        FM_PACKET_ADVANCE_P_BYTE(p);
    }

    /* Parse the target hardware address.  */
    for (int i = 0; i < hlen; i++)
    {
        shift = (hlen - (i + 1)) << 3;
        arp->tha |= ((fm_macaddr) FM_PACKET_GET_BYTE(buffer, p)) << shift;
        FM_PACKET_ADVANCE_P_BYTE(p);
    }

    /* Parse the target protocol address.  */
    arp->tpa.isIPv6 = arp->ptype == FM_PACKET_ETHER_TYPE_IPv6;
    for (int i = 0; i < plen; i++)
    {
        /* The IP address is stored in network byte order. For IPv4 only
         * addr[0] is used. For IPv6 addr[0] contains the least significant 32
         * bits.  */
        index = arp->tpa.isIPv6 ? 3 - (i >> 2) : i >> 2;
        shift = (3 - (i % 4)) << 3;
        arp->tpa.addr[index] |= (FM_PACKET_GET_BYTE(buffer, p)) << shift;
        FM_PACKET_ADVANCE_P_BYTE(p);
    }

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, FM_OK);
}

/**
 * \desc        sends an ARP reply packet
 *
 * \param[in]   event the packet receive event associated with the ARP reply
 *              packet to be sent
 *
 * \param[in]   packet the parsed packet
 *
 * \param[in]   arp the ARP reply information
 *
 *****************************************************************************/
static fm_status PacketSendARPReply(fm_eventPktRecv *event,
                                    fm_packet       *packet,
                                    fm_packetArp    *arp)
{
    fm_buffer     *reply;
    fm_packetInfo packetInfo;
    fm_status     status;
    int           p = 0;
    int           index, hlen, plen, shift;

    FM_LOG_ENTRY_NOARGS(FM_LOG_CAT_EVENT_PKT_RX);

    reply = fmDuplicateBufferChain(event->switchNum,
                                   (fm_buffer *) event->pkt);
    if (reply == NULL)
    {
        status = FM_ERR_NO_MEM;
        goto ABORT;
    }

    /* Update the destination MAC address.  */
    FM_PACKET_SET_WORD(reply, p, (fm_uint32) (arp->tha >> 16));
    FM_PACKET_ADVANCE_P_WORD(p);
    FM_PACKET_SET_HALF_WORD(reply, p, (fm_uint32) (arp->tha & 0xFFFFLL));
    FM_PACKET_ADVANCE_P_HALF_WORD(p);

    /* Update the source MAC address.  */
    FM_PACKET_SET_HALF_WORD(reply, p, (fm_uint32) (arp->sha >> 32));
    FM_PACKET_ADVANCE_P_HALF_WORD(p);
    FM_PACKET_SET_WORD(reply, p, (fm_uint32) (arp->sha & 0xFFFFFFFFLL));
    FM_PACKET_ADVANCE_P_WORD(p);

    /* Advance the position pointer to the ARP payload.  */
    p = packet->layer2;

    /* Advance the position pointer past the hardware type field.  */
    FM_PACKET_ADVANCE_P_HALF_WORD(p);

    /* Advance the position pointer past the protocol type field.  */
    FM_PACKET_ADVANCE_P_HALF_WORD(p);

    /* Parse the hardware length field.  */
    hlen = (int) FM_PACKET_GET_BYTE(reply, p);
    FM_PACKET_ADVANCE_P_BYTE(p);

    /* Parse the protocol length field.  */
    plen = (int) FM_PACKET_GET_BYTE(reply, p);
    FM_PACKET_ADVANCE_P_BYTE(p);

    /* Update the operation field.  */
    FM_PACKET_SET_HALF_WORD(reply, p, FM_PACKET_ARP_OPER_REPLY);
    FM_PACKET_ADVANCE_P_HALF_WORD(p);

    /* Update the source hardware address.  */
    for (int i = 0; i < hlen; i++)
    {
        shift = (hlen - (i + 1)) << 3;
        FM_PACKET_SET_BYTE(reply,
                           p,
                           (fm_uint32) ((arp->sha >> shift) & 0xFFLL));
        FM_PACKET_ADVANCE_P_BYTE(p);
    }

    /* Update the source protocol address.  */
    for (int i = 0; i < plen; i++)
    {
        index = arp->spa.isIPv6 ? 3 - (i >> 2) : i >> 2;
        shift = (3 - (i % 4)) << 3;
        FM_PACKET_SET_BYTE(reply,
                           p,
                           (fm_uint32) ((arp->spa.addr[index] >> shift) & 0xFF));
        FM_PACKET_ADVANCE_P_BYTE(p);
    }

    /* Update the target hardware address.  */
    for (int i = 0; i < hlen; i++)
    {
        shift = (hlen - (i + 1)) << 3;
        FM_PACKET_SET_BYTE(reply,
                           p,
                           (fm_uint32) ((arp->tha >> shift) & 0xFFLL));
        FM_PACKET_ADVANCE_P_BYTE(p);
    }

    /* Update the target protocol address.  */
    for (int i = 0; i < plen; i++)
    {
        index = arp->tpa.isIPv6 ? 3 - (i >> 2) : i >> 2;
        shift = (3 - (i % 4)) << 3;
        FM_PACKET_SET_BYTE(reply,
                           p,
                           (fm_uint32) ((arp->tpa.addr[index] >> shift) & 0xFF));
        FM_PACKET_ADVANCE_P_BYTE(p);
    }

    memset((void *) &packetInfo, 0, sizeof(fm_packetInfo));

    packetInfo.vlanId = 0;
    packetInfo.logicalPort = (fm_uint16) event->srcPort;

    status = fmSendPacketExt(event->switchNum, &packetInfo, reply);
    if (status != FM_OK)
    {
        fmFreeBufferChain(event->switchNum, reply);
    }

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, status);
}

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/**
 * \desc        handles an ARP packet
 *
 * \param[in]   event the packet receive event associated with the ARP packet
 *              to be handled
 *
 * \param[in]   packet the parsed packet
 *
 * \return      TRUE if the ARP packet has been handled successfully
 * \return      FALSE otherwise
 *
 *****************************************************************************/
fm_status fmPacketHandleARP(fm_eventPktRecv *event, fm_packet *packet)
{
    fm_arpEntry   arpEntry;
    fm_ipAddr     ipAddr;
    fm_macaddr    macAddr;
    fm_routeEntry routeEntry;
    fm_status     status;
    fm_int        interfaces[FM_PACKET_MAX_INTERFACES];
    fm_int        nInterfaces;
    fm_uint16     vlanID;
    fm_voidptr    searchToken;
    fm_packetArp  arp, reply;

    FM_LOG_ENTRY_NOARGS(FM_LOG_CAT_EVENT_PKT_RX);

    memset((void *) &arpEntry, 0, sizeof(fm_arpEntry));

    status = PacketParseARP(event, packet, &arp);
    FM_SUPPORT_VERIFY(status);

    arpEntry.macAddr = arp.sha;
    arpEntry.ipAddr = arp.spa;

    status = fmGetInterfaceList(event->switchNum,
                                &nInterfaces,
                                interfaces,
                                FM_PACKET_MAX_INTERFACES);
    FM_SUPPORT_VERIFY(status);

    arpEntry.interface = -1;
    for (int i = 0; i < nInterfaces; i++)
    {
        status = fmGetInterfaceAttribute(event->switchNum,
                                         i,
                                         FM_INTERFACE_VLAN,
                                         (void *) &vlanID);
        FM_SUPPORT_VERIFY(status);

        if (vlanID == arp.vlanID)
        {
            arpEntry.interface = i;
            break;
        }
    }

    if (arpEntry.interface == -1)
    {
        status = FM_ERR_INVALID_INTERFACE;
        goto ABORT;
    }

    switch (arp.oper)
    {
        case FM_PACKET_ARP_OPER_REQUEST:
            /***************************************************
             * Check for gratuitous ARP packets.
             **************************************************/
            if (fmCompareIPAddresses(&(arp.spa), &(arp.tpa)) == 0)
            {
                break;
            }

            /***************************************************
             * Send an ARP reply whenever the ARP request packet
             * was directed to one of many interface IP
             * addresses.
             **************************************************/
            status = fmGetInterfaceAddrFirst(event->switchNum,
                                             arpEntry.interface,
                                             &searchToken,
                                             &ipAddr);
            while (status == FM_OK)
            {
                if (fmCompareIPAddresses(&(arp.tpa), &ipAddr) == 0)
                {
                    status = 
                           fmGetRouterAttribute(event->switchNum,
                                                FM_ROUTER_PHYSICAL_MAC_ADDRESS,
                                                &macAddr);
                    FM_SUPPORT_VERIFY(status);

                    reply = arp;
                    reply.sha = macAddr;
                    reply.spa = ipAddr;
                    reply.tha = arp.sha;
                    reply.tpa = arp.spa;

                    status = PacketSendARPReply(event, packet, &reply);
                    FM_SUPPORT_VERIFY(status);

                    break;
                }

                status = fmGetInterfaceAddrNext(event->switchNum,
                                                &searchToken,
                                                &ipAddr);
            }
        case FM_PACKET_ARP_OPER_REPLY:
            break;

        default:
            status = FM_FAIL;
            goto ABORT;
    }


    status = fmAddARPEntry(event->switchNum, &arpEntry);
    if (status == FM_OK)
    {
        memset((void *) &routeEntry, 0, sizeof(fm_routeEntry));

        routeEntry.routeType = FM_ROUTE_TYPE_UNICAST;
        routeEntry.data.unicast.dstAddr = arpEntry.ipAddr;
        routeEntry.data.unicast.prefixLength = packet->ipHdr.isIPv6 ? 128 : 32;
        routeEntry.data.unicast.nextHop = arpEntry.ipAddr;
        routeEntry.data.unicast.vrid = 0;

        status = fmGetInterfaceAddrFirst(event->switchNum, 
                                         arpEntry.interface,
                                         &searchToken,
                                         &(routeEntry.data.unicast.interfaceAddr));
        FM_SUPPORT_VERIFY(status);

        status = fmAddRoute(event->switchNum, &routeEntry, FM_ROUTE_STATE_UP);
    }
    else if (status == FM_ERR_DUPLICATE_ARP_ENTRY)
    {
        fmUpdateARPEntryDMAC(event->switchNum, &arpEntry);
    }

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, status);
}

