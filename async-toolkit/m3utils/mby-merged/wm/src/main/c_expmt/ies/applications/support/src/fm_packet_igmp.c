/* vim:et:sw=4:ts=4:tw=79:
 * (No tabs, indent level is 4 spaces)  */

/*****************************************************************************
 * File:            fm_packet_igmp.c
 * Creation Date:   June 1, 2007  
 * Description:     IGMPv2 snooping agent
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

#define FM_PACKET_IGMP_DMAC_PREFIX  FM_LITERAL_U64(0x01005E)

/*****************************************************************************
 * Global Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

static fm_status PacketMapGroupAddress(fm_ipAddr * groupAddr,
                                       fm_macaddr *destMacAddress);

static fm_status PacketParseIGMP(fm_eventPktRecv *event,
                                 fm_packet       *packet,
                                 fm_packetIgmp   *igmp);

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/* PacketMapGroupAddress
 *
 * \desc        maps an IP version 4 host group address to an Ethernet
 *              multicast address
 *
 * \param[in]   groupAddr points to the IP version 4 host group address that is
 *              to be mapped to an Ethernet multicast address
 *
 * \param[out]  destMacAddress points to caller-allocated storage in which this
 *              function stores the Ethernet multicast address
 *
 * \return      FM_OK if the IP version 4 host group address has been mapped to
 *              an Ethernet multicast address successfully
 * \return      FM_ERR_INVALID_ARGUMENT if an invalid argument has been
 *              specified
 *
 *****************************************************************************/
static fm_status PacketMapGroupAddress(fm_ipAddr * groupAddr,
                                       fm_macaddr *destMacAddress)
{
    fm_status status = FM_OK;

    if (groupAddr->isIPv6)
    {
        status = FM_ERR_INVALID_ARGUMENT;

        goto ABORT;
    }

    /***************************************************
     * RFC 1112, Section 6.4:
     *
     * An IP host group address is mapped to an Ethernet
     * multicast address by placing the low-order
     * 23-bits of the IP address into the low-order 23
     * bits of the Ethernet multicast address
     * 01-00-5E-00-00-00 (hex).
     **************************************************/
    *destMacAddress = FM_PACKET_IGMP_DMAC_PREFIX << 24;

    *destMacAddress |= (fm_macaddr) (groupAddr->addr[0] & ((1 << 23) - 1));

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, status);

}   /* end PacketMapGroupAddress */




/*****************************************************************************/
/* PacketParseIGMP
 *
 * \desc        parses an IGMP packet
 *
 * \param[in]   event points to the packet receive event associated with the
 *              IGMP packet to be parsed
 *
 * \param[in]   packet points to the parsed packet
 *
 * \param[out]  igmp points to caller-allocated storage in which to store the
 *              information contained in the IGMP packet
 *
 * \return      FM_OK if the IGMP packet has been parsed successfully
 * \return      FM_FAIL otherwise
 *
 *****************************************************************************/
static fm_status PacketParseIGMP(fm_eventPktRecv *event,
                                 fm_packet *      packet,
                                 fm_packetIgmp *  igmp)
{
    fm_buffer   *buffer = (fm_buffer *) event->pkt;
    int         p = packet->layer3;
    
    FM_LOG_ENTRY_NOARGS(FM_LOG_CAT_EVENT_PKT_RX);

    /***************************************************
     * Try to counter IGMP DOS attacks by preventing
     * non-multicast IGMP frames from being processed.
     **************************************************/
    if ((packet->dstAddr >> 24) != FM_PACKET_IGMP_DMAC_PREFIX)
    {
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, FM_FAIL);
    }

    memset((void *) igmp, 0, sizeof(fm_packetIgmp));

    /* Parse the IGMP message type field.  */
    igmp->msgType = FM_PACKET_GET_BYTE(buffer, p);
    FM_PACKET_ADVANCE_P_BYTE(p);

    /* Advance the position pointer past the Max Resp Code field.  */
    FM_PACKET_ADVANCE_P_BYTE(p);

    /* Advance the position pointer past the checksum field.  */
    FM_PACKET_ADVANCE_P_HALF_WORD(p);

    if (igmp->msgType == FM_PACKET_IGMP_REPORTv3)
    {
        /***************************************************
         * This is a very limited IGMP V3 parsing
         * It will only grab the first IGMP group
         **************************************************/

        /* Advance the position pointer past the reserved field.  */
        FM_PACKET_ADVANCE_P_HALF_WORD(p);

        /* Advance the position pointer past the number of groups field.  */
        FM_PACKET_ADVANCE_P_HALF_WORD(p);
        
        /* Advance the position pointer past the record type, aux data len and
        *  nb of sources fields.  */
        FM_PACKET_ADVANCE_P_WORD(p);
    }

    /* Parse the group address field.  */
    igmp->groupAddr.isIPv6 = FALSE;
    igmp->groupAddr.addr[0] = FM_PACKET_GET_WORD(buffer, p);
    FM_PACKET_ADVANCE_P_WORD(p);

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, FM_OK);

}   /* end PacketParseIGMP */

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** fmPacketHandleIGMP
 *
 * \desc        handles an IGMP packet
 *
 * \param[in]   event points to the packet receive event associated with the
 *              IGMP packet to be handled
 *
 * \param[in]   packet points to the parsed packet
 *
 * \return      FM_OK if the IGMP packet has been handled successfully
 * \return      FM_FAIL otherwise
 *
 *****************************************************************************/
fm_status fmPacketHandleIGMP(fm_eventPktRecv *event, fm_packet *packet)
{
    fm_multicastAddress  address;
    fm_multicastListener listener;
    fm_status            status;
    fm_uint32            vlanID;
    fm_int               mcastGroup;
    fm_packetIgmp        igmp;

    FM_LOG_ENTRY_NOARGS(FM_LOG_CAT_EVENT_PKT_RX);

    status = PacketParseIGMP(event, packet, &igmp);
    FM_SUPPORT_VERIFY(status);

    memset((void *) &address, 0, sizeof(fm_multicastAddress));

    if (!fmIsMulticastIPAddress(&(igmp.groupAddr)))
    {
        status = FM_FAIL;
        goto ABORT;
    }

    switch (fmRootSupport[event->switchNum].igmpType)
    {
        case FM_MCAST_ADDR_TYPE_DSTIP:
            address.addressType = FM_MCAST_ADDR_TYPE_DSTIP;
            address.info.dstIpRoute.dstAddr.addr[0] = igmp.groupAddr.addr[0];
            address.info.dstIpRoute.dstPrefixLength = 32;
            address.info.dstIpRoute.dstAddr.isIPv6 = FALSE;
            break;

        case FM_MCAST_ADDR_TYPE_DSTIP_VLAN:
            address.addressType = FM_MCAST_ADDR_TYPE_DSTIP_VLAN;
            address.info.dstIpVlanRoute.dstAddr.addr[0] = igmp.groupAddr.addr[0];
            address.info.dstIpVlanRoute.dstPrefixLength = 32;
            address.info.dstIpVlanRoute.dstAddr.isIPv6 = FALSE;
            address.info.dstIpVlanRoute.vlan = (fm_uint16) event->vlan;
            address.info.dstIpVlanRoute.vlanPrefixLength = 12;
            break;

        case FM_MCAST_ADDR_TYPE_DSTIP_SRCIP:
            address.addressType = FM_MCAST_ADDR_TYPE_DSTIP_SRCIP;
            address.info.dstSrcIpRoute.dstAddr.addr[0] = igmp.groupAddr.addr[0];
            address.info.dstSrcIpRoute.dstPrefixLength = 32;
            address.info.dstSrcIpRoute.dstAddr.isIPv6 = FALSE;
            address.info.dstSrcIpRoute.srcAddr.addr[0] = packet->ipHdr.srcAddr.addr[0];
            address.info.dstSrcIpRoute.srcPrefixLength = 32;
            address.info.dstSrcIpRoute.srcAddr.isIPv6 = FALSE;
            break;

        case FM_MCAST_ADDR_TYPE_DSTIP_SRCIP_VLAN:
            address.addressType = FM_MCAST_ADDR_TYPE_DSTIP_SRCIP_VLAN;
            address.info.dstSrcIpVlanRoute.dstAddr.addr[0] = igmp.groupAddr.addr[0];
            address.info.dstSrcIpVlanRoute.dstPrefixLength = 32;
            address.info.dstSrcIpVlanRoute.dstAddr.isIPv6 = FALSE;
            address.info.dstSrcIpVlanRoute.srcAddr.addr[0] = packet->ipHdr.srcAddr.addr[0];
            address.info.dstSrcIpVlanRoute.srcPrefixLength = 32;
            address.info.dstSrcIpVlanRoute.srcAddr.isIPv6 = FALSE;
            address.info.dstSrcIpVlanRoute.vlan = (fm_uint16) event->vlan;
            address.info.dstSrcIpVlanRoute.vlanPrefixLength = 12;
            break;

        case FM_MCAST_ADDR_TYPE_L2MAC_VLAN:
        case FM_MCAST_ADDR_TYPE_UNKNOWN:
        default:
            address.addressType = FM_MCAST_ADDR_TYPE_L2MAC_VLAN;
            address.info.mac.vlan = (fm_uint16) event->vlan;
            address.info.mac.vlan2 = 0;
            status = PacketMapGroupAddress(&(igmp.groupAddr),
                                   &(address.info.mac.destMacAddress));
            break;
    }

    FM_SUPPORT_VERIFY(status);

    memset((void *) &listener, 0, sizeof(fm_multicastListener));

    if (fmGetGlortLogicalPort(event->switchNum, (event->ISLTag[1] >> 16), &listener.port) != FM_OK)
    {
        listener.port = event->srcPort;
    }
    listener.vlan = (fm_uint16) event->vlan;

    if (event->vlan == 0)
    {
        /***************************************************
         * The specially handled IGMP frame was not VLAN
         * tagged on ingress in which case the VLAN ID needs
         * to be retrieved manually.
         **************************************************/
        status = fmGetPortAttribute(event->switchNum,
                                    event->srcPort,
                                    FM_PORT_DEF_VLAN,
                                    &vlanID);

        FM_SUPPORT_VERIFY(status);

        switch (fmRootSupport[event->switchNum].igmpType)
        {
            case FM_MCAST_ADDR_TYPE_DSTIP:
                break;
    
            case FM_MCAST_ADDR_TYPE_DSTIP_VLAN:
                address.info.dstIpVlanRoute.vlan = (fm_uint16) vlanID;
                break;
    
            case FM_MCAST_ADDR_TYPE_DSTIP_SRCIP:
                break;
    
            case FM_MCAST_ADDR_TYPE_DSTIP_SRCIP_VLAN:
                address.info.dstSrcIpVlanRoute.vlan = (fm_uint16) vlanID;
                break;
    
            case FM_MCAST_ADDR_TYPE_L2MAC_VLAN:
            case FM_MCAST_ADDR_TYPE_UNKNOWN:
            default:
                address.info.mac.vlan = (fm_uint16) vlanID;
                break;
        }
        
        listener.vlan = (fm_uint16) vlanID;

    }   /* end if (event->vlan == 0) */

    switch (igmp.msgType)
    {
        case FM_PACKET_IGMP_REPORTv1:
        case FM_PACKET_IGMP_REPORTv2:
        case FM_PACKET_IGMP_REPORTv3:
            /***************************************************
             * Process an IGMP membership report packet, i.e.
             * perform an IGMP join.
             **************************************************/

            status = fmFindMcastGroupByAddress(event->switchNum,
                                               &address,
                                               &mcastGroup);
            if (status != FM_OK)
            {
                if (status == FM_ERR_MCAST_ADDR_NOT_ASSIGNED)
                {
                    /***************************************************
                     * Create a new multicast group, configure the multicast
                     * group address and activate the multicast group.
                     **************************************************/
                    status = fmCreateMcastGroup(event->switchNum, &mcastGroup);

                    FM_SUPPORT_VERIFY(status);

                    address.mcastGroup = mcastGroup;

                    status = fmSetMcastGroupAddress(event->switchNum,
                                                    mcastGroup,
                                                    &address);

                    FM_SUPPORT_VERIFY(status);

                    status = fmActivateMcastGroup(event->switchNum,
                                                  mcastGroup);

                    FM_SUPPORT_VERIFY(status);
                }
                else
                {
                    goto ABORT;
                }
            }

            status = fmAddMcastGroupListener(event->switchNum,
                                             mcastGroup,
                                             &listener);

            FM_SUPPORT_VERIFY(status);

            break;

        case FM_PACKET_IGMP_LEAVEv2:
            /***************************************************
             * Process an IGMP version 2 leave packet.
             **************************************************/

            status = fmFindMcastGroupByAddress(event->switchNum,
                                               &address,
                                               &mcastGroup);

            FM_SUPPORT_VERIFY(status);

            status = fmDeleteMcastGroupListener(event->switchNum,
                                                mcastGroup,
                                                &listener);

            FM_SUPPORT_VERIFY(status);

            memset((void *) &listener, 0, sizeof(fm_multicastListener));

            status = fmGetMcastGroupListenerFirst(event->switchNum,
                                                  mcastGroup,
                                                  &listener);

            if (status != FM_OK)
            {
                if (status == FM_ERR_NO_MORE)
                {
                    /***************************************************
                     * Delete the multicast group, since it is no longer
                     * in use.
                     **************************************************/
                    status = fmDeactivateMcastGroup(event->switchNum,
                                                    mcastGroup);

                    if (status != FM_OK
                        && status != FM_ERR_MCAST_GROUP_NOT_ACTIVE)
                    {
                        goto ABORT;
                    }

                    status = fmDeleteMcastGroup(event->switchNum, mcastGroup);

                    FM_SUPPORT_VERIFY(status);
                }
                else
                {
                    goto ABORT;
                }
            }

            break;

        default:
            status = FM_FAIL;

            break;

    }   /* end switch (igmp.msgType) */

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, status);

}   /* end fmPacketHandleIGMP */

