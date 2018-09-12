/* vim:et:sw=4:ts=4:sw=4:tw=79:
 * (No tabs, indent level is 4 spaces)  */

/*****************************************************************************
 * File:            fm_packet_classify.c
 * Creation Date:   June 1, 2007  
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

#include <fm_proto_lldp.h>

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

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

fm_status fmGetPacketState(fm_int switchNum, fm_int type, fm_bool *state)
{
    TAKE_SUPPORT_LOCK(switchNum);

    *state = (fmRootSupport[switchNum].packetState >> type) & 1
             ? FM_ENABLED
             : FM_DISABLED;

    DROP_SUPPORT_LOCK(switchNum);

    return FM_OK;
}

fm_status fmSetPacketState(fm_int switchNum, fm_int type, fm_bool state)
{
    fm_uint64 *packetState;

    TAKE_SUPPORT_LOCK(switchNum);

    packetState = &(fmRootSupport[switchNum].packetState);
    *packetState = (fmRootSupport[switchNum].packetState & ~(1 << type))
                   | (state << type);

    DROP_SUPPORT_LOCK(switchNum);

    return FM_OK;
}

fm_status fmGetIgmpType(fm_int switchNum, fm_multicastAddressType *type)
{
    TAKE_SUPPORT_LOCK(switchNum);

    *type = fmRootSupport[switchNum].igmpType;

    DROP_SUPPORT_LOCK(switchNum);

    return FM_OK;
}

fm_status fmSetIgmpType(fm_int switchNum, fm_multicastAddressType type)
{
    TAKE_SUPPORT_LOCK(switchNum);

    fmRootSupport[switchNum].igmpType = type;
    
    DROP_SUPPORT_LOCK(switchNum);

    return FM_OK;
}

/** fmPacketClassify
 * \ingroup fmPacket
 *
 * \desc            Parses a packet and calls the appropriate handler if it
 *                  can be handled.
 *
 * \param[in]       pkt is the event for the packet receive.
 *
 * \return          TRUE if the packet was handled
 * \return          FALSE otherwise.
 *
 *****************************************************************************/
fm_bool fmPacketClassify(fm_eventPktRecv *event)
{
    fm_switch *switchPtr = fmRootApi->fmSwitchStateTable[event->switchNum];
    fm_buffer *buffer = (fm_buffer *) event->pkt;
    fm_status status;
    fm_bool   arp;
    fm_bool   igmp;
    fm_bool   lldp;
    fm_packet packet;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_RX, "switchNum = %d", event->switchNum);

    memset((void *) &packet, 0, sizeof(fm_packet));
    status = fmPacketParse(buffer, &packet);
    FM_SUPPORT_VERIFY(status);

    status = fmGetPacketState(event->switchNum, FM_PACKET_TYPE_IGMP, &igmp);
    FM_SUPPORT_VERIFY(status);

    if (igmp &&
        ((switchPtr->switchFamily == FM_SWITCH_FAMILY_FM4000 &&
                event->trapAction == FM4000_PACKET_TRAP_IGMP)
            || (packet.layer3 > 0 &&
                packet.ipHdr.isIPv6 == FALSE &&
                packet.ipHdr.protocol == FM_PACKET_PROTOCOL_IGMP)))
    {
        status = fmPacketHandleIGMP(event, &packet);
        FM_SUPPORT_VERIFY(status);

        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, TRUE);
    }

    status = fmGetPacketState(event->switchNum, FM_PACKET_TYPE_ARP, &arp);
    FM_SUPPORT_VERIFY(status);

    if (arp && packet.etherType == FM_PACKET_ETHER_TYPE_ARP)
    {
        status = fmPacketHandleARP(event, &packet);
        FM_SUPPORT_VERIFY(status);

        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, TRUE);
    }

    status = fmGetPacketState(event->switchNum, FM_PACKET_TYPE_LLDP, &lldp);
    FM_SUPPORT_VERIFY(status);

    if (lldp && packet.etherType == FM_PACKET_ETHER_TYPE_LLDP)
    {
        status = fmPacketHandleLLDP(event);
        FM_SUPPORT_VERIFY(status);

        FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, TRUE);
    }

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, FALSE);
}

/** fmPacketParse
 * \ingroup fmPacket
 *
 * \desc        parses a generic packet buffer
 *
 * \param[in]   buffer points to the packet buffer to be parsed
 *
 * \param[out]  packet points to user-allocated storage in which the parse
 *              results are to be stored
 *
 * \return      FM_OK if successful
 *
 ****************************************************************************/
fm_status fmPacketParse(fm_buffer *buffer, fm_packet *packet)
{
    fm_uint16 etherType;
    fm_uint16 value;
    int       p = 0;

    FM_LOG_ENTRY_NOARGS(FM_LOG_CAT_EVENT_PKT_RX);

    packet->dstAddr = ((fm_macaddr) FM_PACKET_GET_WORD(buffer, p)) << 16;
    FM_PACKET_ADVANCE_P_WORD(p);
    packet->dstAddr |= (fm_macaddr) FM_PACKET_GET_HALF_WORD(buffer, p);
    FM_PACKET_ADVANCE_P_HALF_WORD(p);

    packet->srcAddr = ((fm_macaddr) FM_PACKET_GET_HALF_WORD(buffer, p)) << 32;
    FM_PACKET_ADVANCE_P_HALF_WORD(p);
    packet->srcAddr |= (fm_macaddr) FM_PACKET_GET_WORD(buffer, p);
    FM_PACKET_ADVANCE_P_WORD(p);

    for (int i = 0; i < FM_PACKET_MAX_VLAN_TAGS; i++)
    {
        etherType = (fm_uint16) FM_PACKET_GET_HALF_WORD(buffer, p);
        FM_PACKET_ADVANCE_P_HALF_WORD(p);

        if (etherType == FM_PACKET_ETHER_TYPE_C_VLAN ||
            etherType == FM_PACKET_ETHER_TYPE_S_VLAN_A ||
            etherType == FM_PACKET_ETHER_TYPE_S_VLAN_B)
        {
            value = (fm_uint16) FM_PACKET_GET_HALF_WORD(buffer, p);
            FM_PACKET_ADVANCE_P_HALF_WORD(p);

            packet->vlanTag[i].vlanID = value & 0xFFF;
            packet->vlanTag[i].priority = value >> 12;

            packet->numVlanTags += 1;
        }
        else
        {
            packet->etherType = etherType;

            break;
        }
    }

    packet->layer2 = p;

    switch (packet->etherType)
    {
        case FM_PACKET_ETHER_TYPE_IPv4:
            packet->ipHdr.isIPv6 = FALSE;

            /* Parse the header length field.  */
            packet->layer3 = packet->layer2
                             + 4 * (FM_PACKET_GET_BYTE(buffer, p) & 0xF);
            FM_PACKET_ADVANCE_P_BYTE(p);

            /* Advance the position pointer past the TOS field.  */
            FM_PACKET_ADVANCE_P_BYTE(p);

            /* Parse the total length field.  */
            packet->ipHdr.payloadLength = FM_PACKET_GET_HALF_WORD(buffer, p);
            packet->ipHdr.payloadLength -= packet->layer3 - packet->layer2;
            FM_PACKET_ADVANCE_P_HALF_WORD(p);

            /* Advance the position pointer past the identification, flags and
             * fragment fields.  */
            FM_PACKET_ADVANCE_P_WORD(p);

            /* Advance the position pointer past the time-to-live field.  */
            FM_PACKET_ADVANCE_P_BYTE(p);

            /* Parse the protocol field.  */
            packet->ipHdr.protocol = FM_PACKET_GET_BYTE(buffer, p);
            FM_PACKET_ADVANCE_P_BYTE(p);

            /* Advance past the header checksum field.  */
            FM_PACKET_ADVANCE_P_HALF_WORD(p);

            /* Parse the source IP address.  */
            packet->ipHdr.srcAddr.isIPv6 = FALSE;
            packet->ipHdr.srcAddr.addr[0] = FM_PACKET_GET_WORD(buffer, p);
            FM_PACKET_ADVANCE_P_WORD(p);

            /* Parse the destination IP address.  */
            packet->ipHdr.dstAddr.isIPv6 = FALSE;
            packet->ipHdr.dstAddr.addr[0] = FM_PACKET_GET_WORD(buffer, p);
            FM_PACKET_ADVANCE_P_WORD(p);
            break;
    }

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_RX, FM_OK);
}

