/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_mirror_int.h
 * Creation Date:   2005
 * Description:     Structures and functions for dealing with port mirroring
 *                  groups
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

#ifndef __FM_FM_API_MIRROR_INT_H
#define __FM_FM_API_MIRROR_INT_H


/* holds information about port mirror groups */
typedef struct
{
    /* Group ID */
    fm_int                id;

    /* Port mask for ports being ingress mirrored.
     * One bit per cardinal port. */
    fm_bitArray           ingressPortUsed;

    /* Port mask for ports being egress mirrored.
     * One bit per cardinal port. 
     * This mirror may be RX egress or TX egress, depending upon the
     * mirror type requested. */
    fm_bitArray           egressPortUsed;

    /* Mirror destination port type: either port number or port mask */
    fm_portIdentifierType mirrorPortType;

    /* logical port of a physical port or remote port under stacking */
    fm_int                mirrorLogicalPort;

    /* Port Mask for multiple destination port situations (such as LAGs).
     * One bit per cardinal port. */
    fm_bitArray           mirrorLogicalPortMask;

    /* TRUE if the mirror group is in use */
    fm_bool               used;

    /* Mirror Type (ingress, egress, bidirectional, etc.) */
    fm_mirrorType         mirrorType;

    /* Which mirroring mode is in use (overlay/explicit). Default is overlay.
     * TRUE for overlay, FALSE for explicit. */
    fm_bool               overlayMode;

    /* Egress Traffic Class */
    fm_int                egressPriority;

    /* Truncate Mirrored Frames */
    fm_bool               truncateFrames;

    /* Truncate Others non Mirrored Frames */
    fm_bool               truncateOtherFrames;

    /* Tree of all the configured VLAN1s specific for this group. The key is
     * the vlan while the value refer to the fm_mirrorVlanType direction. */
    fm_tree               vlan1s;

    /* Tree of all the configured VLAN2s specific for this group. The key is
     * the vlan while the value refer to the fm_mirrorVlanType direction. */
    fm_tree               vlan2s;

    /* sample rate used for this group. Defaulted to -1 to indicate that none
     * of the sampler are used for this group. */
    fm_int                sample;

    /* Filter Mirrored Frames using FFU */
    fm_bool               ffuFilter;

    /* Defines the port number being TX mirrored. */
    fm_int                egressSrcPort;

} fm_portMirrorGroup;


fm_status fmAllocatePortMirrorDataStructures(fm_switch *switchPtr);
fm_status fmInitPortMirror(fm_switch *switchPtr);
fm_status fmFreePortMirrorDataStructures(fm_switch *switchPtr);


/* returns the port to which a specified port is being mirrored, or -1 if
 *  the port is not being mirrored */
fm_int fmGetMirrorPortDest(fm_int sw, fm_int port, fm_mirrorType mirrorType);


#endif /* __FM_FM_API_MIRROR_INT_H */
