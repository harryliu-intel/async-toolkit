/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_mirror.h
 * Creation Date:   March 27, 2006
 * Description:     Structures and functions for dealing with port mirroring
 *                  groups.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2012 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM_API_MIRROR_H
#define __FM_FM_API_MIRROR_H

/* Used for the FM_MIRROR_PRIORITY mirror group attribute to indicate that
 * mirrored frames should egress the mirror group's desitnation port with the
 * original priority of the mirrored frame. */
#define FM_MIRROR_PRIORITY_ORIGINAL        -1

/* The default sample rate for mirrored frames. A value of -1 indicates that
 * sampling is disabled. */
#define FM_MIRROR_SAMPLE_RATE_DISABLED     -1

/* Used for the FM_MIRROR_TX_EGRESS_PORT mirror group attribute to defines the
 * port number being TX mirrored as the first logical egress port. */
#define FM_MIRROR_TX_EGRESS_PORT_FIRST     -1


/**************************************************/
/** \ingroup typeEnum
 *  A port mirror can act on ingress or egress.
 *  When acting on ingress, all incoming traffic on
 *  the mirrored port is sent to the mirror
 *  destination port. When acting on egress, all
 *  traffic that is to egress on the mirrored port
 *  is sent to the mirror destination port.
 *                                              \lb\lb
 *  Note that except where noted below, egress mirrored 
 *  traffic will appear at the mirror destination port 
 *  the way it appeared as it ingressed. That is, if the 
 *  switch alters the packet in any way before it egresses,
 *  the mirrored copy of that packet will not include 
 *  the changes to the packet.
 **************************************************/
typedef enum
{
    /** Mirror ingressing traffic on the mirrored port to the mirror
     *  destination port.
     *
     *  \chips  FM2000, FM3000, FM4000, FM6000 */
    FM_MIRROR_TYPE_INGRESS,

    /** Mirror egressing traffic on the mirrored port to the mirror
     *  destination port.
     *
     *  \chips  FM2000, FM3000, FM4000, FM6000 */
    FM_MIRROR_TYPE_EGRESS,

    /** Mirror ingressing and egressing traffic on the mirrored port to the
     *  mirror destination port.
     *
     *  \chips  FM2000, FM3000, FM4000, FM6000 */
    FM_MIRROR_TYPE_BIDIRECTIONAL,

    /** Mirror ingress traffic to the mirror destination port and drop the
     *  traffic ingressing the mirror ports.
     *
     *  \chips  FM2000, FM3000, FM4000, FM6000 */
    FM_MIRROR_TYPE_REDIRECT,

    /** Mirror egressing traffic on the mirrored port to the mirror
     *  destination port.
     *
     *  \chips FM6000 */
    FM_MIRROR_TYPE_TX_EGRESS,

    /** Mirror ingressing and egressing traffic on the mirrored port to the
     *  mirror destination port. The mirrored copy will be identical to the
     *  ingress frame for packets mirrored on ingress and will be identical to
     *  the egress frame for packets mirrored on egress.
     *
     *  \chips FM6000 */
    FM_MIRROR_TYPE_RX_INGRESS_TX_EGRESS

} fm_mirrorType;


/**************************************************/
/** \ingroup typeEnum
 *  Used as an argument to ''fmAddMirrorVlan''.
 *  A VLAN mirror can act on ingress or egress.
 *  When acting on ingress, all incoming traffic on
 *  the mirrored VLAN is sent to the mirror
 *  destination port. When acting on egress, all
 *  traffic that is to egress on the mirrored VLAN
 *  is sent to the mirror destination port.
 **************************************************/
typedef enum
{
    /** VLAN condition only applied for traffic that ingresses with this
     *  specific VLAN.
     *  
     *  \chips FM6000 */
    FM_MIRROR_VLAN_INGRESS,

    /** VLAN condition only applied for traffic that egresses with this
     *  specific VLAN.
     *  
     *  \chips FM6000 */
    FM_MIRROR_VLAN_EGRESS,

    /** VLAN condition applied for traffic that ingresses or egresses with this
     *  specific VLAN.
     *  
     *  \chips FM6000 */
    FM_MIRROR_VLAN_BIDIRECTIONAL

} fm_mirrorVlanType;


/****************************************************************************/
/** \ingroup constMirrorAttr
 *
 * Mirror Attributes, used as an argument to ''fmSetMirrorAttribute'' and
 * ''fmGetMirrorAttribute''.
 *                                                                      \lb\lb
 * For each attribute, the data type of the corresponding attribute value is
 * indicated.
 *                                                                      \lb\lb
 ****************************************************************************/
enum _fm_mirrorAttr
{
    /** Type fm_int: The priority traffic class for the mirrored frame sent
     *  to the mirror destination port. Values range from 0 to 11 with the
     *  default being FM_MIRROR_PRIORITY_ORIGINAL, which indicates that the
     *  priority should be the original priority of the frame being mirrored.
     *
     *  \chips FM6000 */
    FM_MIRROR_PRIORITY,

    /** Type fm_bool: Indicates whether the mirrored frame sent to the mirror
     *  destination port is truncated or not: FM_ENABLED or FM_DISABLED 
     *  (default).
     *
     *  \chips FM6000 */
    FM_MIRROR_TRUNCATE,

    /** Type fm_int: To mirror frames with a particular sample rate expressed
     *  as a positive integer N, indicating that every Nth frame is to be
     *  sampled. The value ranges from 1 to 65535, with the default being
     *  FM_MIRROR_SAMPLE_RATE_DISABLED. Only one sample rate can be
     *  defined for the entire switch and all mirror groups will share the 
     *  same value. The applied sample rate will be the last one specified.
     *  The specified value will be rounded up or down to the nearest value
     *  supported by the hardware so the value read back may not exactly match
     *  the value that was set.
     *
     *  \chips FM6000 */
    FM_MIRROR_SAMPLE_RATE,

    /** Type fm_bool: Indicates whether the mirrored frame is filtered using
     *  an ACL or not: FM_ENABLED or FM_DISABLED (default). If enabled, a frame
     *  will be mirrored only if it matches an ACL with action
     *  ''FM_ACL_ACTIONEXT_MIRROR_GRP'' and which identifies this mirror group 
     *  AND if the mirror group specific conditions are respected (ingress/egress
     *  port, vlan, sample rate).
     *
     *  \chips FM6000 */
    FM_MIRROR_ACL,

    /** Type fm_int: Defines the port number being TX mirrored if multiple
     *  egress ports are parts of this mirror group. Value is the logical
     *  port selected with the default being FM_MIRROR_TX_EGRESS_PORT_FIRST,
     *  which indicates that the selected port is the first egress port of
     *  the mirror group.
     *
     *  \chips FM6000 */
    FM_MIRROR_TX_EGRESS_PORT,

    /** Type fm_bool: Indicates whether the mirrored frame sent to the other
     *  non-mirrored destination ports is truncated or not. FM_ENABLED or
     *  FM_DISABLED (default). The non-mirrored destination ports must be part
     *  of the truncate mask ''FM_MIRROR_TRUNCATE_MASK'' to be resized.
     *
     *  \chips FM6000 */
    FM_MIRROR_TRUNCATE_OTHERS,

    /** Type ''fm_bitArray'': The N-bit egress port mask used to manage
     *  truncation of other non-mirrored destination ports. Each bit position
     *  corresponds to the logical port number of the egress port. This mask
     *  only applies to mirror groups that enable the mirror attribute
     *  ''FM_MIRROR_TRUNCATE_OTHERS''. Only one truncate mask can be
     *  defined for the entire switch, and all mirror groups will share the 
     *  same value. The applied mask will be the last one specified.
     *                                                                  \lb\lb
     *  See ''fm_bitArray'' for a list of helper functions required to
     *  initialize, read, and write the ''fm_bitArray'' structure.
     *
     *  \chips FM6000 */
    FM_MIRROR_TRUNCATE_MASK,

    /** For internal use only. */
    FM_MIRROR_ATTRIBUTE_MAX

};


/** \ingroup macroSynonym
 * @{ */

/** A legacy synonym for ''fmAddMirrorPort''. */
#define fmMirrorAddPort(sw, group, port) \
        fmAddMirrorPort( (sw), (group), (port) )

/** A legacy synonym for ''fmDeleteMirrorPort''. */
#define fmMirrorRemovePort(sw, group, port) \
        fmDeleteMirrorPort( (sw), (group), (port) )

/** A legacy synonym for ''fmGetMirrorPortFirst''. */
#define fmMirrorGetFirstPort(sw, group, firstPort)  \
        fmGetMirrorPortFirst( (sw), (group), (firstPort) )

/** A legacy synonym for ''fmGetMirrorPortNext''. */
#define fmMirrorGetNextPort(sw, group, currentPort, nextPort) \
        fmGetMirrorPortNext( (sw), (group), (currentPort), (nextPort) )

/** A legacy synonym for ''fmGetMirror''. */
#define fmGetMirrorGroup(sw, mirrorGroup, mirrorPort, mirrorType) \
        fmGetMirror( (sw), (mirrorGroup), (mirrorPort), (mirrorType) )

/** A legacy synonym for ''fmGetMirrorFirst''. */
#define fmGetMirrorGroupFirst(sw, firstGroup, mirrorPort, mirrorType) \
        fmGetMirrorFirst( (sw), (firstGroup), (mirrorPort), (mirrorType) )

/** A legacy synonym for ''fmGetMirrorNext''. */
#define fmGetMirrorGroupNext(sw, curGrp, nxtGrp, mirPort, mirType) \
        fmGetMirrorNext( (sw), (curGrp), (nxtGrp), (mirPort), (mirType) )

/** @} (end of Doxygen group) */


/* create and delete port mirroring groups */
fm_status fmCreateMirror(fm_int        sw,
                         fm_int        group,
                         fm_int        mirrorPort,
                         fm_mirrorType mirrorType);

fm_status fmDeleteMirror(fm_int sw, fm_int group);


/* set or change the mirror port after mirror creation */
fm_status fmSetMirrorDestination(fm_int sw, fm_int group, fm_int mirrorPort);
fm_status fmGetMirrorDestination(fm_int sw, fm_int group, fm_int *mirrorPort);
fm_status fmSetMirrorDestinationExt(fm_int             sw,
                                    fm_int             group,
                                    fm_portIdentifier *portId);
fm_status fmGetMirrorDestinationExt(fm_int             sw,
                                    fm_int             group,
                                    fm_portIdentifier *portId);


/* retrieve details about the existing port mirroring groups */
fm_status fmGetMirror(fm_int         sw,
                      fm_int         mirrorGroup,
                      fm_int *       mirrorPort,
                      fm_mirrorType *mirrorType);

fm_status fmGetMirrorFirst(fm_int         sw,
                           fm_int *       firstGroup,
                           fm_int *       mirrorPort,
                           fm_mirrorType *mirrorType);

fm_status fmGetMirrorNext(fm_int         sw,
                          fm_int         currentGroup,
                          fm_int *       nextGroup,
                          fm_int *       mirrorPort,
                          fm_mirrorType *mirrorType);

/* modify the port list for port mirror groups */
fm_status fmAddMirrorPort(fm_int sw, fm_int group, fm_int port);

fm_status fmAddMirrorPortExt(fm_int        sw,
                             fm_int        group,
                             fm_int        port,
                             fm_mirrorType type);

fm_status fmDeleteMirrorPort(fm_int sw, fm_int group, fm_int port);

fm_status fmGetMirrorPortFirst(fm_int sw, fm_int group, fm_int *firstPort);

fm_status fmGetMirrorPortNext(fm_int  sw,
                              fm_int  group,
                              fm_int  currentPort,
                              fm_int *nextPort);

fm_status fmGetMirrorPortFirstV2(fm_int         sw,
                                 fm_int         group,
                                 fm_int *       firstPort,
                                 fm_mirrorType *mirrorType);

fm_status fmGetMirrorPortNextV2(fm_int         sw,
                                fm_int         group,
                                fm_int         currentPort,
                                fm_int *       nextPort,
                                fm_mirrorType *mirrorType);

fm_status fmGetMirrorAttribute(fm_int sw,
                               fm_int group,
                               fm_int attr,
                               void * value);

fm_status fmSetMirrorAttribute(fm_int sw,
                               fm_int group,
                               fm_int attr,
                               void * value);

fm_status fmAddMirrorVlan(fm_int            sw,
                          fm_int            group,
                          fm_uint16         vlanID,
                          fm_mirrorVlanType direction);

fm_status fmAddMirrorVlanExt(fm_int            sw,
                             fm_int            group,
                             fm_vlanSelect     vlanSel,
                             fm_uint16         vlanID,
                             fm_mirrorVlanType direction);

fm_status fmDeleteMirrorVlan(fm_int sw, fm_int group, fm_uint16 vlanID);

fm_status fmDeleteMirrorVlanExt(fm_int        sw, 
                                fm_int        group, 
                                fm_vlanSelect vlanSel,
                                fm_uint16     vlanID);

fm_status fmGetMirrorVlanFirst(fm_int             sw,
                               fm_int             group,
                               fm_uint16 *        firstID,
                               fm_mirrorVlanType *direction);
fm_status fmGetMirrorVlanFirstExt(fm_int             sw,
                                  fm_int             group,
                                  fm_vlanSelect      vlanSel,
                                  fm_uint16 *        firstID,
                                  fm_mirrorVlanType *direction);
fm_status fmGetMirrorVlanNext(fm_int             sw,
                              fm_int             group,
                              fm_uint16          startID,
                              fm_uint16 *        nextID,
                              fm_mirrorVlanType *direction);
fm_status fmGetMirrorVlanNextExt(fm_int             sw,
                                 fm_int             group,
                                 fm_vlanSelect      vlanSel,
                                 fm_uint16          startID,
                                 fm_uint16 *        nextID,
                                 fm_mirrorVlanType *direction);


#endif /* __FM_FM_API_MIRROR_H */
