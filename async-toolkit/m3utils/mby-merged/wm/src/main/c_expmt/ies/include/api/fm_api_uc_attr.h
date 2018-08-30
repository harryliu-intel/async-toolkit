/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_uc_attr.h
 * Creation Date:   2009
 * Last updated:    April 08, 2013
 * Description:     Contains all of the exposed microcode defined 
 *                  attributes.  THIS FILE IS AUTO-GENERATED.  EDIT THE
 *                  MICROCODE TEMPLATE IF YOU NEED TO MODIFY THIS FILE.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2013 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM_API_UC_ATTR_FRAG_H
#define __FM_FM_API_UC_ATTR_FRAG_H

/** \ingroup constPortAttr */
enum _ucPortAttr
{
    /**
     * Type: fm_uint32. The port's default second VLAN (1 - 4095). The
     * default value is 1.                                                  \lb
     *                                                                      \lb
     * This attribute may be set for the CPU interface port.
     *
     * \chips FM6000 */
    FM_PORT_DEF_VLAN2 = FM_PORT_ATTRIBUTE_MAX ,

    /**
     * Type: fm_uint32. The port's default ISL tag user bits value (0 - 255).
     * The default value is 0.
     *
     * \chips FM6000 */
    FM_PORT_DEF_ISL_USER,

    /**
     * Type: fm_uint32. The port's default second 802.1q priority (0 - 7).
     * The default value is 0.                                              \lb
     *                                                                      \lb
     * This attribute may be set for the CPU interface port.
     *
     * \chips FM6000 */
    FM_PORT_DEF_PRI2,

    /**
     * Type: fm_uint32. The port's default second 802.1q CFI (0 - 1). The
     * default value is 0.                                                  \lb
     *                                                                      \lb
     * This attribute may be set for the CPU interface port.
     *
     * \chips FM6000 */
    FM_PORT_DEF_CFI2,

    /**
     * Type: fm_bool. Indicates that the frame parsing logic should parse TRILL
     * encoded frames: FM_ENABLED or FM_DISABLED (default). Should be set on
     * any port that can process ingress TRILL encoded frames.
     *
     * \chips FM6000 */
    FM_PORT_PARSE_TRILL,

    /**
     * Type: fm_bool. Indicates whether or not deep inspection is enabled:
     * FM_ENABLED or FM_DISABLED (default). When enabled, the frame
     * parsing logic parses frames that have one of the following
     * characteristics:                                                     \lb
     * Non IP frame                                                         \lb
     * Unknown L4 protocol                                                  \lb
     * TCP/UDP frames (deep inspection after header)                        \lb
     *                                                                      \lb
     * Note that for Unknown L4 protocol frames, the first 4 bytes will be
     * available in the L4 SRC port and L4 DST port fields respectively.
     * The L4 start index also starts after these SRC/DST port fields.      \lb
     *                                                                      \lb
     * The offset where where deep inspection starts can be controlled
     * with ''FM_SWITCH_DI_L2_START_INDEX'' and
     * ''FM_SWITCH_DI_L4_START_INDEX''.
     *
     * \chips FM6000 */
    FM_PORT_DI_PARSING,

    /**
     * Type: fm_bool. Indicates that the frame parsing logic should parse PAUSE
     * frames: FM_ENABLED (default) or FM_DISABLED.
     *
     * \chips FM6000 */
    FM_PORT_PARSE_PAUSE,

    /**
     * Type: fm_bool. Indicates that the frame parsing logic should parse
     * class-based PAUSE frames: FM_ENABLED (default) or FM_DISABLED.
     *
     * \chips FM6000 */
    FM_PORT_PARSE_CBP_PAUSE,

    /**
     * Type: fm_bool. Enables reflection for L2 switched frames:
     * FM_ENABLED or FM_DISABLED (default). If enabled, reflection will
     * occur only for frames on VLANs with ''FM_VLAN_SWITCHING_REFLECT''
     * enabled.                                                             \lb
     *                                                                      \lb
     * See ''FM_PORT_ROUTING_REFLECT'' for reflection of routed frames.
     *
     * \chips FM6000 */
    FM_PORT_SWITCHING_REFLECT,

    /**
     * Type: fm_bool. Enables reflection for routed frames: FM_ENABLED or
     * FM_DISABLED (default). If enabled, reflection will occur only for
     * frames on VLANs with ''FM_VLAN_ROUTING_REFLECT'' enabled.            \lb
     *                                                                      \lb
     * See ''FM_PORT_SWITCHING_REFLECT'' for reflection of L2 switched
     * frames.
     *
     * \chips FM6000 */
    FM_PORT_ROUTING_REFLECT,

    /**
     * Type: fm_bool. Enables L3 multicast L2L lookup.
     *
     * \chips FM6000 */
    FM_PORT_L3_MCST_L2L_LOOKUP,

    /**
     * Type: fm_bool. Derive VLAN 1 priority from VLAN 2 priority.
     *
     * \chips FM6000 */
    FM_PORT_VPRI1_FROM_VPRI2,

    /**
     * Type: fm_bool. Derive VLAN 2 priority from VLAN 1 priority.
     *
     * \chips FM6000 */
    FM_PORT_VPRI2_FROM_VPRI1,

    /**
     * Type: fm_int. Do not use.
     *
     * \chips FM6000 */
    FM_PORT_EGRESS_VLAN1_ETHERTYPE,

    /**
     * Type: fm_int. Do not use.
     *
     * \chips FM6000 */
    FM_PORT_EGRESS_VLAN2_ETHERTYPE,

    /**
     * Type: fm_bool. Defines RBRIDGE tagging. Should be set on any port which
     * forward RBridge encoded frames, such as TRILL
     *
     * \chips FM6000 */
    FM_PORT_EGRESS_RBRIDGE_TAG,

    /**
     * Type: fm_bool. Enables VLAN tagging for RBridge encoded frames, such as
     * TRILL. VLAN configured as egress designated VLAN will be inserted if
     * enabled.
     *
     * \chips FM6000 */
    FM_PORT_EGRESS_RBRIDGE_VLAN_TAG,

    /**
     * Type: fm_bool. Experimental attribute. For internal use only: FM_ENABLED
     * or FM_DISABLED (default).
     *
     * \chips FM6000 */
    FM_PORT_FORCE_UPDATE_DYNAMIC,

    /**
     * Type: fm_bool. Indicates whether this port should drop frames with
     * invalid SMAC (NULL, Multicast or Broadcast): FM_ENABLED or FM_DISABLED
     * (default).
     *
     * \chips FM6000 */
    FM_PORT_DROP_SMAC_ERR,

    /**
     * Type: fm_bool. Do not flood L3 multicast frames with an unknown DMAC:
     * FM_ENABLED or FM_DISABLED (default).
     *
     * \chips FM6000 */
    FM_PORT_L3_MCST_DENY_FLOODING,

    /**
     * Type: fm_bool. Dropping of VLAN2 untagged frames on ingress: FM_ENABLED
     * or FM_DISABLED (default).
     *
     * \chips FM6000 */
    FM_PORT_DROP_UNTAGGED_VID2,

    /**
     * Type: fm_bool. Dropping frames that are VLAN2 priority tagged (VLAN
     * ID = 0): FM_ENABLED or FM_DISABLED (default).                        \lb
     *                                                                      \lb
     * See ''FM_PORT_DROP_TAGGED_VID2'' for dropping of VLAN2 tagged
     * frames with a non-zero VLAN ID.
     *
     * \chips FM6000 */
    FM_PORT_DROP_PRIORITY_TAGGED_VID2,

    /**
     * Type: fm_bool. Dropping of VLAN2 tagged frames on ingress: FM_ENABLED or
     * FM_DISABLED (default).
     *
     * \chips FM6000 */
    FM_PORT_DROP_TAGGED_VID2,

    /**
     * Type: fm_bool. Dropping frames that are priority tagged (VLAN ID =
     * 0): FM_ENABLED or FM_DISABLED (default).                             \lb
     *                                                                      \lb
     * See ''FM_PORT_DROP_TAGGED'' for dropping of tagged frames with a
     * non- zero VLAN ID.
     *
     * \chips FM6000 */
    FM_PORT_DROP_PRIORITY_TAGGED,

    /**
     * Type: fm_bool. Dropping frames that incur an ingress VLAN boundary
     * violation on VID2: FM_ENABLED or FM_DISABLED (default).
     *
     * \chips FM6000 */
    FM_PORT_DROP_BV_VLAN2,

    /**
     * Type: fm_bool. Experimental attribute. For internal use only: FM_ENABLED
     * or FM_DISABLED (default).
     *
     * \chips FM6000 */
    FM_PORT_SWITCHING_SELECT_VID2,

    /* New attributes begin above this line */

    /** For internal use only. */
    FM_UC_PORT_ATTRIBUTE_MAX
};

/** \ingroup constSwAttr */
enum _ucSwitchAttr
{
    /**
     * Type: fm_uint64. Router 0 MAC address (ingress)
     *
     * \chips FM6000 */
    FM_INGRESS_MAC_ROUTER_0 = FM_SWITCH_ATTRIBUTE_MAX ,

    /**
     * Type: fm_uint32. Defines the first of two valid ether types for
     * indicating an outer VLAN tag. The default value is 0x8100.
     *
     * \chips FM6000 */
    FM_SWITCH_VLAN1_ETHERTYPE_A,

    /**
     * Type: fm_uint32. Defines the second of two valid ether types for
     * indicating an outer VLAN tag. The default value is 0x88A8.
     *
     * \chips FM6000 */
    FM_SWITCH_VLAN1_ETHERTYPE_B,

    /**
     * Type: fm_uint32. Defines the first of two valid ether types for
     * indicating an inner VLAN tag. The default value is 0x8100.
     *
     * \chips FM6000 */
    FM_SWITCH_VLAN2_ETHERTYPE_A,

    /**
     * Type: fm_uint32. Defines the second of two valid ether types for
     * indicating an inner VLAN tag. The default value is 0x88A8.
     *
     * \chips FM6000 */
    FM_SWITCH_VLAN2_ETHERTYPE_B,

    /**
     * Type: fm_uint32. Specifies the Ethernet type used for a Virtual Network
     * endpoint tag.
     *
     * \chips FM6000 */
    FM_SWITCH_VNTAG_TYPE,

    /**
     * Type: fm_uint32. Defines the ether type to recognize as a congestion
     * notification frame.
     *
     * \chips FM6000 */
    FM_SWITCH_CN_TYPE,

    /**
     * Type: fm_uint64. Router 0 MAC address (egress)
     *
     * \chips FM6000 */
    FM_EGRESS_MAC_ROUTER_0,

    /**
     * Type: fm_uint16. Defines the 16-bit RBridge nickname to use for this
     * node in the RBridge network
     *
     * \chips FM6000 */
    FM_SWITCH_RBRIDGE_NICKNAME,

    /* New attributes begin above this line */

    /** For internal use only. */
    FM_UC_SWITCH_ATTRIBUTE_MAX
};

/** \ingroup constVlanAttr */
enum _ucVlanAttr
{
    /**
     * Type: fm_bool. Enables VLAN reflection for L2 switched frames:
     * FM_ENABLED or FM_DISABLED (default). If enabled, reflection will
     * occur only on ports that have ''FM_PORT_SWITCHING_REFLECT''
     * enabled.                                                             \lb
     *                                                                      \lb
     * See ''FM_VLAN_ROUTING_REFLECT'' for reflection of routed frames.     \lb
     *                                                                      \lb
     * Note that setting ''FM_VLAN_REFLECT'' results in this attribute
     * being set to the same value.
     *
     * \chips FM6000 */
    FM_VLAN_SWITCHING_REFLECT = FM_VLAN_ATTRIBUTE_MAX ,

    /**
     * Type: fm_bool. Enables VLAN reflection for routed frames:
     * FM_ENABLED or FM_DISABLED (default). If enabled, reflection will
     * occur only on ports that have ''FM_PORT_ROUTING_REFLECT'' enabled.   \lb
     *                                                                      \lb
     * See ''FM_VLAN_SWITCHING_REFLECT'' for reflection of L2 switched
     * frames.                                                              \lb
     *                                                                      \lb
     * Note that setting ''FM_VLAN_REFLECT'' results in this attribute
     * being set to the same value.
     *
     * \chips FM6000 */
    FM_VLAN_ROUTING_REFLECT,

    /**
     * Type: fm_bool. Defines if an ARP frame received on this VLAN can be
     * logged to CPU or not: FM_ENABLED or FM_DISABLED (default). This
     * parameter works in conjunction with port attribute ''FM_PORT_LOG_ARP''.
     * If the frame is received on a port for which ''FM_PORT_LOG_ARP'' is
     * enabled, the ARP frame will be logged.
     *
     * \chips FM6000 */
    FM_VLAN_ARP_LOGGING,

    /**
     * Type: fm_bool. Defines if a frame received on this VLAN can be IP
     * Tunneled.
     *
     * \chips FM6000 */
    FM_VLAN_IP_TUNNELING,

    /* New attributes begin above this line */

    /** For internal use only. */
    FM_UC_VLAN_ATTRIBUTE_MAX
};

#endif /* __FM_FM_API_UC_ATTR_FRAG_H */
