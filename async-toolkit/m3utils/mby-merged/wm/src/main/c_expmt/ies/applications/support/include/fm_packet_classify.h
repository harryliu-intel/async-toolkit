/* vim:et:sw=4:ts=4:tw=79:
 * (No tabs, indent level is 4 spaces, text width is 79)  */

/*****************************************************************************
 * File:            fm_packet_classify.h
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

#ifndef __FM_FM_PACKET_CLASSIFY_H
#define __FM_FM_PACKET_CLASSIFY_H

#define FM_PACKET_ETHER_TYPE_C_VLAN     0x8100
#define FM_PACKET_ETHER_TYPE_S_VLAN_A   0x88A8
#define FM_PACKET_ETHER_TYPE_S_VLAN_B   0x9100
#define FM_PACKET_ETHER_TYPE_IPv4       0x0800
#define FM_PACKET_ETHER_TYPE_IPv6       0x86DD
#define FM_PACKET_ETHER_TYPE_ARP        0x0806
#define FM_PACKET_ETHER_TYPE_LLDP       0x88CC

#define FM_PACKET_PROTOCOL_IGMP         0x2

#define FM_PACKET_TYPE_IGMP             0
#define FM_PACKET_TYPE_ARP              1
#define FM_PACKET_TYPE_LLDP             2

#define FM_PACKET_MAX_VLAN_TAGS     3

typedef struct _fm_vlanTag {
    fm_uint16  vlanID;

    fm_uint16  priority;
} fm_vlanTag;

typedef struct _fm_ipHdr {
    fm_bool   isIPv6;

    /* The length of the Layer 3 payload in bytes  */
    fm_int    payloadLength;

    /* The protocol  */
    fm_uint16 protocol;

    /* The Layer 3 source address  */
    fm_ipAddr srcAddr;

    /* The Layer 3 destination address  */
    fm_ipAddr dstAddr;
} fm_ipHdr;

typedef struct _fm_packet {
    /* The Layer 2 destination address  */
    fm_macaddr dstAddr;

    /* The Layer 2 source address  */
    fm_macaddr srcAddr;

    /* The VLAN tags  */
    fm_vlanTag vlanTag[FM_PACKET_MAX_VLAN_TAGS];

    /* The number of VLAN tags with which the frame is tagged  */
    fm_int     numVlanTags;

    /* The first non-VLAN Ethernet type  */
    fm_uint16  etherType;

    /* The index of the first Layer 2 payload byte  */
    fm_int     layer2;

    /* The index of the first Layer 3 payload byte. A value of zero indicates
     * that the frame does not contain a recognized Layer 3 header  */
    fm_int     layer3;

    /* The IP header  */
    fm_ipHdr   ipHdr;
} fm_packet;

fm_status fmGetPacketState(fm_int switchNum, fm_int type, fm_bool *state);
fm_status fmSetPacketState(fm_int switchNum, fm_int type, fm_bool state);

fm_status fmGetIgmpType(fm_int switchNum, fm_multicastAddressType *type);
fm_status fmSetIgmpType(fm_int switchNum, fm_multicastAddressType type);

fm_bool fmPacketClassify(fm_eventPktRecv *packet);
fm_status fmPacketParse(fm_buffer *buffer, fm_packet *frame);

#endif /* __FM_FM_PACKET_CLASSIFY_H */

