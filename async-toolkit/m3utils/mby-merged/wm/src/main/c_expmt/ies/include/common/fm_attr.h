/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_attr.h
 * Creation Date:   2006
 * Description:     Structures and functions for dealing with the SDK config
 *                  subsystem.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2006 - 2013 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM_ATTR_H
#define __FM_FM_ATTR_H

#define FM_API_ATTR_TEXT_MAX_LENGTH                 256


/**************************************************/
/** \ingroup typeEnum
 * Used as an argument to ''fmGetApiAttribute'' and
 * ''fmSetApiAttribute'' to identify the data type
 * of the API attribute.
 **************************************************/
typedef enum
{
    /** Attribute type is an integer. */
    FM_API_ATTR_INT = 0,

    /** Attribute type is an integer. */
    FM_API_ATTR_BOOL,

    /** Attribute type is an integer. */
    FM_API_ATTR_FLOAT,

    /** Attribute type is an integer. */
    FM_API_ATTR_TEXT,

    /* -- Add new attributes above this line -- */

    /** For internal use only. */
    FM_API_ATTR_MAX

} fm_apiAttrType;


/*****************************************************************************/
/** API Attributes
 *  \ingroup apiAttrib
 *  \page apiAttributes
 *
 *  Each attribute has a key, a type and a default value. The key strings for
 *  API attributes are organized in a hierarchical dotted format.
 *
 *  Attributes are defined below in two sections. The first section includes 
 *  attributes that are documented and available for customer use and will be 
 *  supported in future versions of the API. The second section is for
 *  for Intel use only and do not appear in the API User Guide. Use of these
 *  attrbitues outside of Intel is not recommended unless guided by Intel
 *  to do so.
 *
 *  The key symbol name begins with AAK_ for API Attribute Key. The key value
 *  is a string.
 * 
 *  The attribute type symbol name begins with AAT_ for API Attribute Type. It
 *  is not typically used by the software, since the helper functions
 *  fmGet*ApiAttribute are per-type, but is required for documentation purposes.
 * 
 *  The default attribute symbol name begins with AAD_ for API Attribute
 *  Default. The default value itself is a function of the attribute type.
 * 
 *  NOTA BENE! Once a key is defined, it must not be changed, as doing so
 *  will break customer applications and render operating systems in the
 *  field to fail. The reason is that API attributes can be read in from
 *  a file at run-time. These files specify the attribute key. If an existing
 *  key becomes unrecognized, the specified attribute value will not be
 *  used by the API.
 *****************************************************************************/

/************************************************************************
 ****                                                                ****
 ****               BEGIN DOCUMENTED ATTRIBUTES                      ****
 ****                                                                ****
 ************************************************************************/
 
/** \ingroup apiAttrib
 * @{ */
 
/** EBI (External Bus Interface) clock speed in MHZ. The EBI is the bus
 *  between the CPU and the switch device. */
#define FM_AAK_API_SYSTEM_EBI_CLOCK                 "api.system.ebi.clock"
#define FM_AAT_API_SYSTEM_EBI_CLOCK                 FM_API_ATTR_INT
#define FM_AAD_API_SYSTEM_EBI_CLOCK                 FM_EBI_MHZ

/** FH (Frame Handler) Ref clock speed in MHZ. */
#define FM_AAK_API_SYSTEM_FH_REF_CLOCK              "api.system.fhRef.clock"
#define FM_AAT_API_SYSTEM_FH_REF_CLOCK              FM_API_ATTR_FLOAT
#ifdef FM_FH_REF_MHZ
#define FM_AAD_API_SYSTEM_FH_REF_CLOCK              FM_FH_REF_MHZ
#else
#define FM_AAD_API_SYSTEM_FH_REF_CLOCK              FM_EBI_MHZ
#endif

/** The default spanning tree state of a port for a VLAN of which it is a
 *  member. See ''Spanning Tree States'' for possible values. This attribute
 *  is also used to set the initial spanning tree state for all ports in all
 *  VLANs in shared spanning tree mode (see the ''FM_SPANNING_TREE_MODE''
 *  switch attribute). */
#define FM_AAK_API_STP_DEF_STATE_VLAN_MEMBER        "api.stp.defaultState.vlanMember"
#define FM_AAT_API_STP_DEF_STATE_VLAN_MEMBER        FM_API_ATTR_INT
#define FM_AAD_API_STP_DEF_STATE_VLAN_MEMBER        FM_STP_STATE_DISABLED

/** The flag indicating whether or not we allow the CPU to send to the CPU in
 *  the directed packet send mode */
#define FM_AAK_API_DIRECT_SEND_TO_CPU               "api.allowDirectSendToCpu"
#define FM_AAT_API_DIRECT_SEND_TO_CPU               FM_API_ATTR_BOOL
#define FM_AAD_API_DIRECT_SEND_TO_CPU               FALSE

/** The default spanning tree state of a port for a VLAN of which it is not
 *  a member. See ''Spanning Tree States'' for possible values. */
#define FM_AAK_API_STP_DEF_STATE_VLAN_NON_MEMBER    "api.stp.defaultState.vlanNonMember"
#define FM_AAT_API_STP_DEF_STATE_VLAN_NON_MEMBER    FM_API_ATTR_INT
#define FM_AAD_API_STP_DEF_STATE_VLAN_NON_MEMBER    FM_STP_STATE_DISABLED

/** For the FM2000, if TRUE, MA Table entries will be written for each VLAN
 *  to defend against learning a source address of all ones or all zeros.
 *  Enabling this attribute defends against denial of service attacks using
 *  these invalid source addresses, but consumes MA Table space that could be
 *  used for learning legitimate addresses. */
#define FM_AAK_API_FM2000_VLAN_WRITEMAC             "api.FM2000.vlan.writeMAC"
#define FM_AAT_API_FM2000_VLAN_WRITEMAC             FM_API_ATTR_BOOL
#define FM_AAD_API_FM2000_VLAN_WRITEMAC             TRUE

/** For the FM2000, setting this attribute to TRUE will cause an error to be
 *  reported by the API if ''api.FM2000.vlan.writeMAC'' is TRUE and there was no
 *  more room in the MA Table. */
#define FM_AAK_API_FM2000_VLAN_REPORT_WR_MAC_ERR    "api.FM2000.vlan.reportWriteMACErrors"
#define FM_AAT_API_FM2000_VLAN_REPORT_WR_MAC_ERR    FM_API_ATTR_BOOL
#define FM_AAD_API_FM2000_VLAN_REPORT_WR_MAC_ERR    FALSE

/** For the FM2000 only, specify the desired frame handler clock speed in MHz.
 *  Use of this attribute is mutually exclusive with use of the three
 *  api.FM2000.boot.frameHandlerPLL attributes. If any of the three
 *  api.FM2000.boot.frameHandlerPLL attributes are specified by the
 *  application, they will override the value of this attribute. */
#define FM_AAK_API_FM2000_BOOT_FHFREQ               "api.FM2000.boot.fhFreq"
#define FM_AAT_API_FM2000_BOOT_FHFREQ               FM_API_ATTR_INT
#ifdef FM_FM2000_FH_PLL_OUT_MHZ
#define FM_AAD_API_FM2000_BOOT_FHFREQ               FM_FM2000_FH_PLL_OUT_MHZ
#else
#define FM_AAD_API_FM2000_BOOT_FHFREQ               363
#endif

/** A factor used to calculate the FM2000 frame handler clock speed according
 *  to the formula:                                                     \lb
 *  ''FM_FH_REF_MHZ'' * M/N/(2**P). */
#define FM_AAK_API_FM2000_BOOT_FH_PLL_P             "api.FM2000.boot.frameHandlerPLL.P"
#define FM_AAT_API_FM2000_BOOT_FH_PLL_P             FM_API_ATTR_INT
#define FM_AAD_API_FM2000_BOOT_FH_PLL_P             0

/** A factor used to calculate the FM2000 frame handler clock speed according
 *  to the formula:                                                         \lb
 *  ''FM_FH_REF_MHZ'' * M/N/(2**P). */
#define FM_AAK_API_FM2000_BOOT_FH_PLL_N             "api.FM2000.boot.frameHandlerPLL.N"
#define FM_AAT_API_FM2000_BOOT_FH_PLL_N             FM_API_ATTR_INT
#define FM_AAD_API_FM2000_BOOT_FH_PLL_N             3

/** A factor used to calculate the FM2000 frame handler clock speed according
 *  to the formula:
 *                                                                      \lb\lb
 *  ''FM_FH_REF_MHZ'' * M/N/(2**P).
 *                                                                      \lb\lb
 *  Note that the default value is calculated from the default value for
 *  ''api.FM2000.boot.frameHandlerPLL.N'' and ''FM_FH_REF_MHZ''.*/
#define FM_AAK_API_FM2000_BOOT_FH_PLL_M             "api.FM2000.boot.frameHandlerPLL.M"
#define FM_AAT_API_FM2000_BOOT_FH_PLL_M             FM_API_ATTR_INT
#define FM_AAD_API_FM2000_BOOT_FH_PLL_M \
    (FM_AAD_API_FM2000_BOOT_FHFREQ      \
     * 1.0                              \
     * FM_AAD_API_FM2000_BOOT_FH_PLL_N  \
     / 33.3)


/** For the FM4000 only, specify the desired frame handler clock speed in MHz.
 *  Use of this attribute is mutually exclusive with use of the three
 *  api.FM4000.boot.frameHandlerPLL attributes. If any of the three
 *  api.FM4000.boot.frameHandlerPLL attributes are specified by the
 *  application, they will override the value of this attribute. */
#define FM_AAK_API_FM4000_BOOT_FHFREQ               "api.FM4000.boot.fhFreq"
#define FM_AAT_API_FM4000_BOOT_FHFREQ               FM_API_ATTR_INT
#ifdef FM_FM4000_FH_PLL_OUT_MHZ
#define FM_AAD_API_FM4000_BOOT_FHFREQ               FM_FM4000_FH_PLL_OUT_MHZ
#else
#define FM_AAD_API_FM4000_BOOT_FHFREQ               375
#endif

/** A factor used to calculate the FM4000 frame handler clock speed according
 *  to the formula:                                                     \lb
 *  ''FM_FH_REF_MHZ'' * M/N/(2**P). */
#define FM_AAK_API_FM4000_BOOT_FH_PLL_P             "api.FM4000.boot.frameHandlerPLL.P"
#define FM_AAT_API_FM4000_BOOT_FH_PLL_P             FM_API_ATTR_INT
#define FM_AAD_API_FM4000_BOOT_FH_PLL_P             0

/** A factor used to calculate the FM4000 frame handler clock speed according
 *  to the formula:                                                         \lb
 *  ''FM_FH_REF_MHZ'' * M/N/(2**P). */
#define FM_AAK_API_FM4000_BOOT_FH_PLL_N             "api.FM4000.boot.frameHandlerPLL.N"
#define FM_AAT_API_FM4000_BOOT_FH_PLL_N             FM_API_ATTR_INT
#define FM_AAD_API_FM4000_BOOT_FH_PLL_N             4

/** A factor used to calculate the FM4000 frame handler clock speed according
 *  to the formula:
 *                                                                      \lb\lb
 *  ''FM_FH_REF_MHZ'' * M/N/(2**P).
 *                                                                      \lb\lb
 *  Note that the default value is calculated from the default value for
 *  ''api.FM4000.boot.frameHandlerPLL.N'' and ''FM_FH_REF_MHZ''.*/
#define FM_AAK_API_FM4000_BOOT_FH_PLL_M             "api.FM4000.boot.frameHandlerPLL.M"
#define FM_AAT_API_FM4000_BOOT_FH_PLL_M             FM_API_ATTR_INT
#define FM_AAD_API_FM4000_BOOT_FH_PLL_M \
    (FM_AAD_API_FM4000_BOOT_FHFREQ      \
     * 1.0                              \
     * FM_AAD_API_FM4000_BOOT_FH_PLL_N  \
     / 33.3)

/** Selects a watermark methodology for FM3000 and FM4000 devices. Valid 
 *  values are:
 *                                                                      \lb\lb
 *  "combined" - Shared watermarks across all ports, except when PAUSE is 
 *  enabled within an SMP, in which case per-port PAUSE is enabled.  
 *  Enabling PAUSE on all ports in an SMP will enable per-SMP PAUSE.  
 *  Supports the ''api.FM4000.splitSMP'' attribute.
 *                                                                      \lb\lb
 *  "shared" - Shared watermarks using only SMP0.
 *                                                                      \lb\lb
 *  "perport_cpu" - Per-port watermarks, but with SMP1 and traffic class 7
 *  reserved for CPU protection.
 *                                                                      \lb\lb
 *  "shared_cpu" - Shared watermarks across all ports, except for the CPU
 *  port, which is protected using SMP1 and traffic class 7. */
#define FM_AAK_API_FM4000_WMSELECT                  "api.FM4000.wmSelect"
#define FM_AAT_API_FM4000_WMSELECT                  FM_API_ATTR_TEXT
#define FM_AAD_API_FM4000_WMSELECT                  "combined"

/** Selects a watermark methodology for FM6000 devices. Valid values are:
 *                                                                      \lb\lb
 *  "disabled" - Watermarks are disabled (except for Global).
 *                                                                      \lb\lb
 *  "lossy" - All memory is dedicated to a lossy watermark configuration.
 *                                                                      \lb\lb
 *  "lossy_lossless" - Memory is split between lossy and lossless 
 *  configurations.
 *                                                                      \lb\lb
 *  "lossless" - All memory is dedicated to a lossless watermark 
 *  configuration. */
#define FM_AAK_API_FM6000_WMSELECT                  "api.FM6000.wmSelect"
#define FM_AAT_API_FM6000_WMSELECT                  FM_API_ATTR_TEXT
#define FM_AAD_API_FM6000_WMSELECT                  "lossy"


/** Specifies the amount of memory (in bytes) to allocate to each port for
 *  RX private (CM_PORT_RXMP_PRIVATE_WM register). */
#define FM_AAK_API_FM6000_CM_RX_RXMP_PRIVATE_BYTES  "api.FM6000.cmRxRxmpPrivateBytes"
#define FM_AAT_API_FM6000_CM_RX_RXMP_PRIVATE_BYTES  FM_API_ATTR_INT
#define FM_AAD_API_FM6000_CM_RX_RXMP_PRIVATE_BYTES  6451

/** Specifies the amount of memory (in bytes) to allocate to each port for
 *  the TX hog watermark. */
#define FM_AAK_API_FM6000_CM_TX_TXMP_HOG_BYTES      "api.FM6000.cmTxTxmpHogBytes"

#define FM_AAT_API_FM6000_CM_TX_TXMP_HOG_BYTES      FM_API_ATTR_INT
#define FM_AAD_API_FM6000_CM_TX_TXMP_HOG_BYTES      1000000

/** Specifies the congestion management RXMP soft drop versus hog watermark
 *  delta to be used in the "lossy" and "lossy_lossless" watermark schemes 
 *  (specified by ''api.FM6000.wmSelect'').  The delta is specified as a
 *  percentage of the soft drop hog watermark. */
#define FM_AAK_API_FM6000_CM_RXMP_SD_VS_HOG_PERCENT "api.FM6000.cmRxmpSdVsHogPercent"
#define FM_AAT_API_FM6000_CM_RXMP_SD_VS_HOG_PERCENT FM_API_ATTR_INT
#define FM_AAD_API_FM6000_CM_RXMP_SD_VS_HOG_PERCENT 80

/** Specifies the number of jitter bits to be used in the congestion management
 *  soft drop algorithm for the "lossy" and "lossy_lossless" watermark schemes
 *  (specified by ''api.FM6000.wmSelect'').  Valid values range from 0 (jitter
 *  disabled) to 7 (the maximum amount of jitter). */
#define FM_AAK_API_FM6000_CM_RXMP_SD_JITTER_BITS    "api.FM6000.cmRxmpSdJitterBits"
#define FM_AAT_API_FM6000_CM_RXMP_SD_JITTER_BITS    FM_API_ATTR_INT
#define FM_AAD_API_FM6000_CM_RXMP_SD_JITTER_BITS    7

/** Specifies whether congestion management should apply a random soft drop
 *  when TX private is exceeded. */
#define FM_AAK_API_FM6000_CM_TX_SD_ON_PRIVATE       "api.FM6000.cmTxSdOnPrivate"
#define FM_AAT_API_FM6000_CM_TX_SD_ON_PRIVATE       FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_CM_TX_SD_ON_PRIVATE       FALSE

/** Specifies whether congestion management should apply a random soft drop 
 *  based on total RXMP usage. */
#define FM_AAK_API_FM6000_CM_TX_SD_ON_RXMP_FREE     "api.FM6000.cmTxSdOnRxmpFree"
#define FM_AAT_API_FM6000_CM_TX_SD_ON_RXMP_FREE     FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_CM_TX_SD_ON_RXMP_FREE     TRUE

/** Specifies the amount of memory (in bytes) to reserve for pause frame
 * response time across a link.  This is used for the "lossless" and
 * "lossy_lossless" watermark scheme (specified by ''api.FM6000.wmSelect''). */
#define FM_AAK_API_FM6000_CM_PAUSE_BUFFER_BYTES     "api.FM6000.cmPauseBufferBytes"
#define FM_AAT_API_FM6000_CM_PAUSE_BUFFER_BYTES     FM_API_ATTR_INT
#define FM_AAD_API_FM6000_CM_PAUSE_BUFFER_BYTES     30720

/** Allows the TX private watermark per traffic class (CM_TX_TC_PRIVATE_WM)
 *  to be set when using "shared_cpu" or "perport_cpu" with the 
 *  ''api.FM4000.wmSelect'' attribute. Note that if ANY port has a 
 *  MAX_FRAME_SIZE > 2048 then the TX private watermark per traffic class is 
 * disabled regardless of this attribute. */
#define FM_AAK_API_FM4000_ENABLE_TXTCPRIV           "api.FM4000.enableTxTcPriv"
#define FM_AAT_API_FM4000_ENABLE_TXTCPRIV           FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_ENABLE_TXTCPRIV           TRUE

/** For watermark selection algorithms that support it, will divide the 
 * available memory between two SMPs. See ''api.FM4000.wmSelect''. */
#define FM_AAK_API_FM4000_ENABLE_SPLITSMP           "api.FM4000.splitSMP"
#define FM_AAT_API_FM4000_ENABLE_SPLITSMP           FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_ENABLE_SPLITSMP           FALSE

/** The first FFU slice on the FM4000 allocated for unicast routing. The value
 *  of this attribute must be less than or equal to the value for
 *  ''api.FM4000.ffu.unicastSliceRangeLast''.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM4000_FFU_UNICAST_SLICE_1ST     "api.FM4000.ffu.unicastSliceRangeFirst"
#define FM_AAT_API_FM4000_FFU_UNICAST_SLICE_1ST     FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_UNICAST_SLICE_1ST     8

/** The last FFU slice on the FM4000 allocated for unicast routing. The value
 *  of this attribute must be greater than or equal to the value for
 *  ''api.FM4000.ffu.unicastSliceRangeFirst'' and less than the value for
 *  ''api.FM4000.ffu.multicastSliceRangeLast''.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM4000_FFU_UNICAST_SLICE_LAST    "api.FM4000.ffu.unicastSliceRangeLast"
#define FM_AAT_API_FM4000_FFU_UNICAST_SLICE_LAST    FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_UNICAST_SLICE_LAST    15

/** The first FFU slice on the FM4000 allocated for multicast routing. The
 *  value of this attribute must be less than or equal to the value for
 *  ''api.FM4000.ffu.multicastSliceRangeLast'' and greater than or equal to the
 *  value for ''api.FM4000.ffu.unicastSliceRangeFirst''.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM4000_FFU_MULTICAST_SLICE_1ST   "api.FM4000.ffu.multicastSliceRangeFirst"
#define FM_AAT_API_FM4000_FFU_MULTICAST_SLICE_1ST   FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_MULTICAST_SLICE_1ST   0

/** The last FFU slice on the FM4000 allocated for multicast routing. The
 *  value of this attribute must be greater than or equal to the value for
 *  ''api.FM4000.ffu.multicastSliceRangeFirst''.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM4000_FFU_MULTICAST_SLICE_LAST  "api.FM4000.ffu.multicastSliceRangeLast"
#define FM_AAT_API_FM4000_FFU_MULTICAST_SLICE_LAST  FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_MULTICAST_SLICE_LAST  7

/** The first FFU slice on the FM4000 allocated for ACLs.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM4000_FFU_ACL_SLICE_1ST         "api.FM4000.ffu.aclSliceRangeFirst"
#define FM_AAT_API_FM4000_FFU_ACL_SLICE_1ST         FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_ACL_SLICE_1ST         16

/** The last FFU slice on the FM4000 allocated for ACLs.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM4000_FFU_ACL_SLICE_LAST        "api.FM4000.ffu.aclSliceRangeLast"
#define FM_AAT_API_FM4000_FFU_ACL_SLICE_LAST        FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_ACL_SLICE_LAST        31

/** The first FFU slice on the FM4000 allocated for CVLANs. By default we do not
 *  pre-allocate any FFU slices for the CVLAN API.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM4000_FFU_CVLAN_SLICE_1ST       "api.FM4000.ffu.cVlanSliceRangeFirst"
#define FM_AAT_API_FM4000_FFU_CVLAN_SLICE_1ST       FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_CVLAN_SLICE_1ST       -1

/** The last FFU slice on the FM4000 allocated for CVLANs.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM4000_FFU_CVLAN_SLICE_LAST      "api.FM4000.ffu.cVlanSliceRangeLast"
#define FM_AAT_API_FM4000_FFU_CVLAN_SLICE_LAST      FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_CVLAN_SLICE_LAST      -1

/** The minimum precedence value allowed for unicast entries. The value of 
 *  this attribute must be less than or equal to the value for
 *  ''api.FM4000.ffu.unicastPrecedenceMax''. This attribute will be used as the
 *  default value for unicast entries. */
#define FM_AAK_API_FM4000_FFU_UNICAST_PRECEDENCE_MIN    "api.FM4000.ffu.unicastPrecedenceMin"
#define FM_AAT_API_FM4000_FFU_UNICAST_PRECEDENCE_MIN    FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_UNICAST_PRECEDENCE_MIN    0

/** The maximum precedence value allowed for unicast entries. The value of this
 *  attribute must be greater than or equal to the value for
 *  ''api.FM4000.ffu.unicastPrecedenceMin''. */
#define FM_AAK_API_FM4000_FFU_UNICAST_PRECEDENCE_MAX    "api.FM4000.ffu.unicastPrecedenceMax"
#define FM_AAT_API_FM4000_FFU_UNICAST_PRECEDENCE_MAX    FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_UNICAST_PRECEDENCE_MAX    7

/** The minimum precedence value allowed for multicast entries. The value of 
 *  this attribute must be less than or equal to the value for
 *  ''api.FM4000.ffu.multicastPrecedenceMax''. This attribute will be used as 
 *  the default value for multicast entries. */
#define FM_AAK_API_FM4000_FFU_MULTICAST_PRECEDENCE_MIN    "api.FM4000.ffu.multicastPrecedenceMin"
#define FM_AAT_API_FM4000_FFU_MULTICAST_PRECEDENCE_MIN    FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_MULTICAST_PRECEDENCE_MIN    0

/** The maximum precedence value allowed for multicast entries. The value of 
 *  this attribute must be greater than or equal to the value for
 *  ''api.FM4000.ffu.multicastPrecedenceMin''. */
#define FM_AAK_API_FM4000_FFU_MULTICAST_PRECEDENCE_MAX    "api.FM4000.ffu.multicastPrecedenceMax"
#define FM_AAT_API_FM4000_FFU_MULTICAST_PRECEDENCE_MAX    FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_MULTICAST_PRECEDENCE_MAX    7

/** The minimum precedence value allowed for ACL rules. The value of this
 *  attribute must be less than or equal to the value for
 *  ''api.FM4000.ffu.ACLPrecedenceMax''. This attribute will be used as the
 *  default value for ACL entries. */
#define FM_AAK_API_FM4000_FFU_ACL_PRECEDENCE_MIN    "api.FM4000.ffu.ACLPrecedenceMin"
#define FM_AAT_API_FM4000_FFU_ACL_PRECEDENCE_MIN    FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_ACL_PRECEDENCE_MIN    5

/** The maximum precedence value allowed for ACL rules. The value of this
 *  attribute must be greater than or equal to the value for
 *  ''api.FM4000.ffu.ACLPrecedenceMin''. */
#define FM_AAK_API_FM4000_FFU_ACL_PRECEDENCE_MAX    "api.FM4000.ffu.ACLPrecedenceMax"
#define FM_AAT_API_FM4000_FFU_ACL_PRECEDENCE_MAX    FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_ACL_PRECEDENCE_MAX    7

/** The first counter/policer bank on the FM4000 allocated for ACLs. */
#define FM_AAK_API_FM4000_FFU_ACL_BANK_1ST         "api.FM4000.ffu.aclBankRangeFirst"
#define FM_AAT_API_FM4000_FFU_ACL_BANK_1ST         FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_ACL_BANK_1ST         0

/** The last counter/policer bank on the FM4000 allocated for ACLs. */
#define FM_AAK_API_FM4000_FFU_ACL_BANK_LAST        "api.FM4000.ffu.aclBankRangeLast"
#define FM_AAT_API_FM4000_FFU_ACL_BANK_LAST        FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_ACL_BANK_LAST        3

/** The first counter/policer bank on the FM4000 allocated for TCAM learning. */
#define FM_AAK_API_FM4000_FFU_LEARNING_BANK_1ST         "api.FM4000.ffu.learningBankRangeFirst"
#define FM_AAT_API_FM4000_FFU_LEARNING_BANK_1ST         FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_LEARNING_BANK_1ST         -1

/** The last counter/policer bank on the FM4000 allocated for TCAM learning. */
#define FM_AAK_API_FM4000_FFU_LEARNING_BANK_LAST        "api.FM4000.ffu.learningBankRangeLast"
#define FM_AAT_API_FM4000_FFU_LEARNING_BANK_LAST        FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_LEARNING_BANK_LAST        -1

/** The FM4000 FFU MAC mapper owner. See ''fm_ffuOwnerType'' for possible
 *  values. */
#define FM_AAK_API_FM4000_FFU_MAPMAC_OWNER          "api.FM4000.ffu.mapMAC.owner"
#define FM_AAT_API_FM4000_FFU_MAPMAC_OWNER          FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_MAPMAC_OWNER          FM_FFU_OWNER_ROUTING

/** The number of entries in the MAC Mapper that are reserved for router
 *  MAC addresses including all the virtual router MAC addresses. The value 
 *  of this attribute must be greater than or equal to 1 if routing is 
 *  enabled. */
#define FM_AAK_API_FM4000_FFU_MAPMAC_ROUTING        "api.FM4000.ffu.mapMAC.reservedForRouting"
#define FM_AAT_API_FM4000_FFU_MAPMAC_ROUTING        FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_MAPMAC_ROUTING        16

/** The FM4000 FFU VLAN mapper owner. See ''fm_ffuOwnerType'' for possible
 *  values. */
#define FM_AAK_API_FM4000_FFU_MAPVLAN_OWNER         "api.FM4000.ffu.mapVlan.owner"
#define FM_AAT_API_FM4000_FFU_MAPVLAN_OWNER         FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_MAPVLAN_OWNER         FM_FFU_OWNER_ACL

/** The FM4000 FFU IP mapper owner. See ''fm_ffuOwnerType'' for possible
 *  values. */
#define FM_AAK_API_FM4000_FFU_MAPIP_OWNER           "api.FM4000.ffu.mapIP.owner"
#define FM_AAT_API_FM4000_FFU_MAPIP_OWNER           FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_MAPIP_OWNER           FM_FFU_OWNER_ACL

/** The FM4000 FFU protocol mapper owner. See ''fm_ffuOwnerType'' for possible
 *  values. */
#define FM_AAK_API_FM4000_FFU_MAPPROT_OWNER         "api.FM4000.ffu.mapProt.owner"
#define FM_AAT_API_FM4000_FFU_MAPPROT_OWNER         FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_MAPPROT_OWNER         FM_FFU_OWNER_ACL

/** The FM4000 FFU L4 address (port) mapper owner. See ''fm_ffuOwnerType'' for
 *  possible values. */
#define FM_AAK_API_FM4000_FFU_MAPL4_OWNER           "api.FM4000.ffu.mapL4.owner"
#define FM_AAT_API_FM4000_FFU_MAPL4_OWNER           FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_MAPL4_OWNER           FM_FFU_OWNER_ACL

/** The FM4000 FFU Ethernet type mapper owner. See ''fm_ffuOwnerType'' for
 *  possible values. */
#define FM_AAK_API_FM4000_FFU_MAPETHTYPE_OWNER      "api.FM4000.ffu.mapEthType.owner"
#define FM_AAT_API_FM4000_FFU_MAPETHTYPE_OWNER      FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_MAPETHTYPE_OWNER      FM_FFU_OWNER_ACL

/** The FM4000 FFU IP packet length mapper owner. See ''fm_ffuOwnerType'' for
 *  possible values. */
#define FM_AAK_API_FM4000_FFU_MAPLENGTH_OWNER       "api.FM4000.ffu.mapLength.owner"
#define FM_AAT_API_FM4000_FFU_MAPLENGTH_OWNER       FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_MAPLENGTH_OWNER       FM_FFU_OWNER_ACL

/** The FM4000 FFU source port mapper owner. See ''fm_ffuOwnerType'' for
 *  possible values. */
#define FM_AAK_API_FM4000_FFU_MAPSRC_OWNER          "api.FM4000.ffu.mapSrc.owner"
#define FM_AAT_API_FM4000_FFU_MAPSRC_OWNER          FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FFU_MAPSRC_OWNER          FM_FFU_OWNER_ACL

/** Initial MaxSegs field of VPD_INFO_1 register on the FM4000. */
#define FM_AAK_API_FM4000_BOOT_NUMSEGMENTS          "api.FM4000.boot.numSegments"
#define FM_AAT_API_FM4000_BOOT_NUMSEGMENTS          FM_API_ATTR_INT
#define FM_AAD_API_FM4000_BOOT_NUMSEGMENTS          FM4000_MAX_MEMORY_SEGMENTS

/** Initial PortDisableMask field of VPD_INFO_1 register on the FM4000. */
#define FM_AAK_API_FM4000_BOOT_PORTDISABLEMASK      "api.FM4000.boot.portDisableMask"
#define FM_AAT_API_FM4000_BOOT_PORTDISABLEMASK      FM_API_ATTR_INT
#define FM_AAD_API_FM4000_BOOT_PORTDISABLEMASK      0

/** Whether to perform a full boot on the FM4000. Set to FALSE if switch has
 *  already been booted via EEPROM. */
#define FM_AAK_API_FM4000_BOOT_FULLBOOT             "api.FM4000.boot.fullBoot"
#define FM_AAT_API_FM4000_BOOT_FULLBOOT             FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_BOOT_FULLBOOT             TRUE

/** Whether ingress mirrors have precedence over egress mirrors. If a
 *  packet can hit on both an ingress mirror and an egress mirror, this
 *  attribute determines which mirror would operate on the packet. */
#define FM_AAK_API_FM4000_INGRESS_MIRROR_PRECEDENCE "api.FM4000.ingressMirrorPrecedence"
#define FM_AAT_API_FM4000_INGRESS_MIRROR_PRECEDENCE FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_INGRESS_MIRROR_PRECEDENCE TRUE

/** Indicate whether to enable the port security using per port triggers on FM4000.*/
#define FM_AAK_API_FM4000_SV_PORT_DETECT           "api.FM4000.svPortDetect"
#define FM_AAT_API_FM4000_SV_PORT_DETECT           FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_SV_PORT_DETECT           FALSE

/** Controls the activation of 2 triggers added as a workaround to the Bali
 *  Errata #45 (Multiple Triggers with Action 'Drop' Cannot be Combined). But
 *  they have the side effect of forcing learning on frames and this can lead to
 *  cases where SMAC multicast or broadcast are being learned.
 *                                                                      \lb\lb
 *  If the customer application requires those 2 triggers to be active to work
 *  around the Bali errata, then this API attribute should be set to TRUE.
 *                                                                      \lb\lb
 *  If the customer application requires a workaround for the errata and
 *  also prevent multicast or broadcast SMAC addresses from being learned, 
 *  then:                                                                   \lb
 *     1 - Set the api.FM4000.enforceLearning attribute to FALSE.           \lb
 *     2 - Create a FFU rule entry that will hit on the reception of a SMAC
 *         having the multicast bit set to 1 and have the action for this 
 *         rule to set a trigger ID.                                        \lb
 *     3 - Create 2 triggers identical to the triggers being controlled by this
 *         new attribute.                                                   \lb
 *     4 - Add a new condition to the triggers that will have them fire only 
 *        if the value of the FFU_ID is different from the one set by the 
 *        FFU rule. */
#define FM_AAK_API_FM4000_ENFORCE_LEARNING          "api.FM4000.enforceLearning"
#define FM_AAT_API_FM4000_ENFORCE_LEARNING          FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_ENFORCE_LEARNING          FALSE

/** Whether to enable the FFU on the FM4000. */
#define FM_AAK_API_FM4000_BOOT_FFUENABLE            "api.FM4000.boot.ffuEnable"
#define FM_AAT_API_FM4000_BOOT_FFUENABLE            FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_BOOT_FFUENABLE            TRUE

/** Initial DeassertionCount field of SERDES_CTRL_3 register on the FM4000. */
#define FM_AAK_API_FM4000_BOOT_SERDESHYSTERESIS     "api.FM4000.boot.serdesHysteresis"
#define FM_AAT_API_FM4000_BOOT_SERDESHYSTERESIS     FM_API_ATTR_INT
#define FM_AAD_API_FM4000_BOOT_SERDESHYSTERESIS     0x1312D

/** Initial LinkCount field of SERDES_CTRL_3 register on the FM4000. */
#define FM_AAK_API_FM4000_BOOT_PCSHYSTERESIS        "api.FM4000.boot.pcsHysteresis"
#define FM_AAT_API_FM4000_BOOT_PCSHYSTERESIS        FM_API_ATTR_INT
#define FM_AAD_API_FM4000_BOOT_PCSHYSTERESIS        0x1

/** Default MAC address table trigger ID on the FM4000.  */
#define FM_AAK_API_FM4000_MA_TABLE_DEFAULTTRIGID    "api.FM4000.MATable.defaultTrigID"
#define FM_AAT_API_FM4000_MA_TABLE_DEFAULTTRIGID    FM_API_ATTR_INT
#define FM_AAD_API_FM4000_MA_TABLE_DEFAULTTRIGID    63

/** Controls whether to enable MAC address table offload support on 
 *  the FM4000. The FFU TCAM is used to offload the MAC table. */
#define FM_AAK_API_FM4000_MA_TABLE_OFFLOADENABLE    "api.FM4000.MATable.offloadEnable"
#define FM_AAT_API_FM4000_MA_TABLE_OFFLOADENABLE    FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_MA_TABLE_OFFLOADENABLE    FALSE

/** Indicates whether the API should automatically identify the switch
 *  device type upon a ''FM_EVENT_SWITCH_INSERTED'' event. */
#define FM_AAK_DEBUG_BOOT_IDENTIFYSWITCH            "debug.boot.identifySwitch"
#define FM_AAT_DEBUG_BOOT_IDENTIFYSWITCH            FM_API_ATTR_BOOL
#define FM_AAD_DEBUG_BOOT_IDENTIFYSWITCH            TRUE

/** Indicates whether to reboot the switch following identification. */
#define FM_AAK_DEBUG_BOOT_RESET                     "debug.boot.reset"
#define FM_AAT_DEBUG_BOOT_RESET                     FM_API_ATTR_BOOL
#define FM_AAD_DEBUG_BOOT_RESET                     TRUE

/** Indicates whether the platform should automatically generate
 *  ''FM_EVENT_SWITCH_INSERTED'' events for all switches known at startup. */
#define FM_AAK_DEBUG_BOOT_AUTOINSERTSWITCH          "debug.boot.autoInsertSwitches"
#define FM_AAT_DEBUG_BOOT_AUTOINSERTSWITCH          FM_API_ATTR_BOOL
#define FM_AAD_DEBUG_BOOT_AUTOINSERTSWITCH          TRUE

/** Indicates the length of time in ns to hold the device in reset
 *  prior to identification */
#define FM_AAK_API_BOOT_RESET_TIME                  "api.boot.deviceResetTime"
#define FM_AAT_API_BOOT_RESET_TIME                  FM_API_ATTR_INT
#define FM_AAD_API_BOOT_RESET_TIME                  200000000

/** Specifies whether Switch-Aggregate links should be auto-enabled
 *  when they are added via ''fmAddSWAGLink''. */
#define FM_AAK_API_AUTO_ENABLE_SWAG_LINKS           "api.swag.autoEnableLinks"
#define FM_AAT_API_AUTO_ENABLE_SWAG_LINKS           FM_API_ATTR_BOOL
#define FM_AAD_API_AUTO_ENABLE_SWAG_LINKS           TRUE

/** Whether to allocate multicast forward-to-CPU triggers on the FM4000. */
#define FM_AAK_API_FM4000_ENABLE_MCAST_CPU_TRIGGERS "api.FM4000.enableMcastCpuTriggers"
#define FM_AAT_API_FM4000_ENABLE_MCAST_CPU_TRIGGERS FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_ENABLE_MCAST_CPU_TRIGGERS FALSE

/** Whether to allocate unicast flooding triggers on the FM4000. */
#define FM_AAK_API_FM4000_ENABLE_UCAST_FLOODING_TRIGGERS "api.FM4000.enableUcastFloodingTriggers"
#define FM_AAT_API_FM4000_ENABLE_UCAST_FLOODING_TRIGGERS FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_ENABLE_UCAST_FLOODING_TRIGGERS TRUE

/** Whether to allocate multicast flooding triggers on the FM4000. */
#define FM_AAK_API_FM4000_ENABLE_MCAST_FLOODING_TRIGGERS "api.FM4000.enableMcastFloodingTriggers"
#define FM_AAT_API_FM4000_ENABLE_MCAST_FLOODING_TRIGGERS FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_ENABLE_MCAST_FLOODING_TRIGGERS TRUE

/** Whether to allocate multicast pruning triggers on the FM4000. */
#define FM_AAK_API_FM4000_ENABLE_MCAST_PRUNING_TRIGGERS "api.FM4000.enableMcastPruningTriggers"
#define FM_AAT_API_FM4000_ENABLE_MCAST_PRUNING_TRIGGERS FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_ENABLE_MCAST_PRUNING_TRIGGERS TRUE

/** Force the switch priority of packet trapped using the
 *  ''FM_PORT_MCAST_FLOODING'' or ''FM_PORT_UCAST_FLOODING'' port attribute.
 *  By default, trapped frames will keep whatever switch priority they are
 *  assigned on ingress, until overridden with this attribute. */
#define FM_AAK_API_FM4000_FLOODING_TRAP_PRIORITY "api.FM4000.floodingTrapPriority"
#define FM_AAT_API_FM4000_FLOODING_TRAP_PRIORITY FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FLOODING_TRAP_PRIORITY -1

/** Force the switch priority of L3 packet trapped using the
 *  ''FM_MCASTGROUP_FWD_TO_CPU'' multicast group attribute. By default, trapped
 *  frames will keep whatever switch priority they are assigned on ingress,
 *  until overridden with this attribute. */
#define FM_AAK_API_FM4000_L3_MCAST_FWD_TO_CPU_PRIORITY "api.FM4000.L3McastFwdToCPUPriority"
#define FM_AAT_API_FM4000_L3_MCAST_FWD_TO_CPU_PRIORITY FM_API_ATTR_INT
#define FM_AAD_API_FM4000_L3_MCAST_FWD_TO_CPU_PRIORITY -1

/** Whether to use the Egress VID table to reflect spanning tree state instead
 *  of relying on the Egress FID table for the FM4000. This attribute must be 
 *  set to TRUE prior to setting a switch to the UP state in order to use any 
 *  of the TRAP values for the ''FM_IP_OPTIONS_DISPOSITION'' switch attribute 
 *  or to use the ''FM_TRAP_MTU_VIOLATIONS'' switch attribute.
 *                                                                      \lb\lb
 *  These TRAP attributes do not distinguish between routed and switched
 *  frames, so triggers are used to untrap switched (non-routed) frames. However,
 *  triggers bypass the Egress FID table, so this API attribute causes the 
 *  Egress VID table to reflect spanning tree state. Note that when this API
 *  attribute is enabled, frames dropped due to egress spanning tree state will
 *  be counted as VLAN egress boundary violations. */
#define FM_AAK_API_FM4000_ENABLE_EGRESS_VID_AS_FID "api.FM4000.enableVidAsFid"
#define FM_AAT_API_FM4000_ENABLE_EGRESS_VID_AS_FID FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_ENABLE_EGRESS_VID_AS_FID FALSE

/** Specifies the limit to block low priority event request when the number of
 *  free event falls below this limit.
 *                                                                      \lb\lb
 *  Note that the Default value will be FM_MAX_EVENTS/24 if FM_MAX_EVENTS is
 *  greater than 512, otherwise it will be set to 24.
 */
#define FM_AAK_API_FREE_EVENT_BLOCK_THRESHOLD       "api.event.blockThreshold"
#define FM_AAT_API_FREE_EVENT_BLOCK_THRESHOLD       FM_API_ATTR_INT
#if (FM_MAX_EVENTS > 512)
#define FM_AAD_API_FREE_EVENT_BLOCK_THRESHOLD       FM_MAX_EVENTS/24
#else
#define FM_AAD_API_FREE_EVENT_BLOCK_THRESHOLD       24
#endif

/** Specifies the limit to unblock low priority event request when the number of
 *  free event exceeds above this limit.
 *                                                                      \lb\lb
 *  Note that the Default value will be
 *  ((FM_MAX_EVENTS/24) + (FM_MAX_EVENTS/192) + 1) if FM_MAX_EVENTS is greater
 *  than 512, otherwise it will be set to 30.
 */
#define FM_AAK_API_FREE_EVENT_UNBLOCK_THRESHOLD     "api.event.unblockThreshold"
#define FM_AAT_API_FREE_EVENT_UNBLOCK_THRESHOLD     FM_API_ATTR_INT
#if (FM_MAX_EVENTS > 512)
#define FM_AAD_API_FREE_EVENT_UNBLOCK_THRESHOLD     ((FM_MAX_EVENTS/24) + (FM_MAX_EVENTS/192) + 1)
#else
#define FM_AAD_API_FREE_EVENT_UNBLOCK_THRESHOLD     30
#endif

/** Specifies the timeout (millisecond) value for the 
 *  semaphore blocking on event free.
 */
#define FM_AAK_API_EVENT_SEM_TIMEOUT                "api.event.semaphoreTimeout"
#define FM_AAT_API_EVENT_SEM_TIMEOUT                FM_API_ATTR_INT
#define FM_AAD_API_EVENT_SEM_TIMEOUT                1000

/** Indicates whether received packets are to be queued directly to the 
 *  application's event handler callback function (see ''fm_eventHandler'')
 *  from the packet receive thread. This will greatly improve the packet
 *  reception rate, but may not be appropriate for all platforms.
 *                                                                      \lb\lb
 *  The thread responsible for packet reception will queue the packet directly.
 *  On some platforms, such as the fm85xxep, the same thread is also 
 *  responsible for other tasks. Hence the application cannot take too long 
 *  in the event handler when processing the receive packet. For platforms
 *  like the fm85xxep, it is not appropriate to enable direct queueing.
 *                                                                      \lb\lb
 *  Another caveat is that with direct queueing, the application's event 
 *  handler can now be called simultaneously for packet receive events as
 *  well as other events. The application must ensure appropriate locking 
 *  around access to any resources shared bewteen the processing of received
 *  packets and other events.
 *                                                                      \lb\lb
 *  This attribute is not currently supported for switch aggregates or
 *  FM2000 family devices.
 */
#define FM_AAK_API_PACKET_RX_DIRECT_ENQUEUEING      "api.packet.rxDirectEnqueueing"
#define FM_AAT_API_PACKET_RX_DIRECT_ENQUEUEING       FM_API_ATTR_BOOL
#define FM_AAD_API_PACKET_RX_DIRECT_ENQUEUEING       FALSE

/** On a switch that was booted from EEPROM (e.g., for subsequent control
 *  via FIBM), indicates which ports have already been brought out of reset 
 *  by the EEPROM. Each bit represents a physical port number (bit N represents
 *  physical port N). If the bit is set to 1, then the corresponding port is
 *  already out of reset. If set to 0, then the API will release the port from
 *  reset.
 *                                                                      \lb\lb
 *  IMPORTANT NOTE: A properly configured EEPROM should bring all ports on the
 *  switches out of reset, so this attribute should generally not be needed
 *  and is deprecated. */ 
#define FM_AAK_API_BOOT_PORT_OUT_OF_RESET_MASK      "api.boot.portOutOfResetMask"
#define FM_AAT_API_BOOT_PORT_OUT_OF_RESET_MASK      FM_API_ATTR_INT
#define FM_AAD_API_BOOT_PORT_OUT_OF_RESET_MASK      0xFFFFFFFF

/** Defines the default remote platform type used by FIBM (fibmNIC only). See
 *  ''fm_platformRemoteType'' for possible values. */
#define FM_AAK_API_PLATFORM_REMOTE_TYPE             "api.platform.remoteType"
#define FM_AAT_API_PLATFORM_REMOTE_TYPE             FM_API_ATTR_INT
#define FM_AAD_API_PLATFORM_REMOTE_TYPE             FM_PLATFORM_TYPE_RENO

/** Indicates whether ''fmDeleteLAGExt'' should wait for the LAG group deletion
 *  to complete, meaning that any required MAC Table purges are finished,
 *  before returning to the caller.  FALSE means that the deletions must
 *  complete prior to function return, TRUE means that the function may
 *  return as soon as the purge is enqueued.  Note that TRUE means that
 *  the application may get ''FM_ERR_NO_FREE_LAG'' if all lag groups are in
 *  the "pending deletion" state when ''fmCreateLAGExt'' is called.
 */
#define FM_AAK_API_ASYNC_LAG_DELETION                "api.lag.asyncDeletion"
#define FM_AAT_API_ASYNC_LAG_DELETION                FM_API_ATTR_BOOL
#define FM_AAD_API_ASYNC_LAG_DELETION                TRUE

/** Indicates whether the API should generate an FM_EVENT_ENTRY_LEARNED or
 *  FM_EVENT_ENTRY_AGED event when the application adds or deletes a static
 *  address to/from the MA table. */
#define FM_AAK_API_MA_EVENT_ON_STATIC_ADDR          "api.ma.eventOnStaticAddr"
#define FM_AAT_API_MA_EVENT_ON_STATIC_ADDR          FM_API_ATTR_BOOL
#define FM_AAD_API_MA_EVENT_ON_STATIC_ADDR          FALSE

/** Indicates whether the API should generate an FM_EVENT_ENTRY_LEARNED or
 *  FM_EVENT_ENTRY_AGED event when the application adds or deletes a dynamic
 *  address to/from the MA table. */
#define FM_AAK_API_MA_EVENT_ON_DYNAMIC_ADDR         "api.ma.eventOnDynamicAddr"
#define FM_AAT_API_MA_EVENT_ON_DYNAMIC_ADDR         FM_API_ATTR_BOOL
#define FM_AAD_API_MA_EVENT_ON_DYNAMIC_ADDR         FALSE

/** Indicates whether the API should flush all the addresses associated with
 *  a port from the MA Table when the port goes down. */
#define FM_AAK_API_MA_FLUSH_ON_PORT_DOWN       "api.ma.flushOnPortDown"
#define FM_AAT_API_MA_FLUSH_ON_PORT_DOWN       FM_API_ATTR_BOOL
#define FM_AAD_API_MA_FLUSH_ON_PORT_DOWN       TRUE

/** Indicates whether the API should flush from the MAC address table
 *  all the addresses associated with:
 *  (1) a VLAN when the VLAN is deleted;
 *  (2) a port/VLAN pair when a port is deleted from the VLAN;
 *  (3) a VLAN when changing the FM_VLAN_FID2_IVL VLAN attribute. */
#define FM_AAK_API_MA_FLUSH_ON_VLAN_CHANGE       "api.ma.flushOnVlanChange"
#define FM_AAT_API_MA_FLUSH_ON_VLAN_CHANGE       FM_API_ATTR_BOOL
#define FM_AAD_API_MA_FLUSH_ON_VLAN_CHANGE       TRUE

/** Indicates whether the API should flush from the MAC address table
 *  all the addresses associated with a port that is being added to or 
 *  deleted from a LAG. */
#define FM_AAK_API_MA_FLUSH_ON_LAG_CHANGE       "api.ma.flushOnLagChange"
#define FM_AAT_API_MA_FLUSH_ON_LAG_CHANGE       FM_API_ATTR_BOOL
#define FM_AAD_API_MA_FLUSH_ON_LAG_CHANGE       TRUE

/** Specifies the maximum number of TCN FIFO entries to be processed in
 *  a single cycle. */
#define FM_AAK_API_MA_TCN_FIFO_BURST_SIZE       "api.ma.tcnFifoBurstSize"
#define FM_AAT_API_MA_TCN_FIFO_BURST_SIZE       FM_API_ATTR_INT
#define FM_AAD_API_MA_TCN_FIFO_BURST_SIZE       512

/** Indicates whether the API should collect VLAN statistics for internal
 *  ports in a switch aggregate. */
#define FM_AAK_API_SWAG_INTERNAL_VLAN_STATS       "api.swag.internalPort.vlanStats"
#define FM_AAT_API_SWAG_INTERNAL_VLAN_STATS       FM_API_ATTR_BOOL
#define FM_AAD_API_SWAG_INTERNAL_VLAN_STATS       FALSE

/** Control whether port attributes should be set on each of the LAG member
 *  ports, as it is with API 2.x, or set on the LAG logical port. See ''Link
 *  Aggregation (LAG) Management'' in the API User Guide for more information.*/
#define FM_AAK_API_PER_LAG_MANAGEMENT               "api.perLagManagement"
#define FM_AAT_API_PER_LAG_MANAGEMENT               FM_API_ATTR_BOOL
#define FM_AAD_API_PER_LAG_MANAGEMENT               TRUE

/** Controls whether the parity sweeper thread is installed or not. The
 *  parity sweeper monitors the state of several memories within the
 *  device and reports any parity errors, as well as correcting those
 *  errors that can be corrected. The rate at which this thread runs
 *  (consuming CPU bandwidth) is controllable via additional attributes. */
#define FM_AAK_API_PARITY_SWEEPER_ENABLE        "api.paritySweeper.enable"
#define FM_AAT_API_PARITY_SWEEPER_ENABLE        FM_API_ATTR_BOOL
#define FM_AAD_API_PARITY_SWEEPER_ENABLE        FALSE

/** When the parity sweeper thread is enabled with the ''api.paritySweeper.enable''
 *  attribute, this attribute specifies the default value for the 
 *  ''FM_PARITY_SWEEPER_CONFIG'' switch attribute and is the number of 
 *  register reads performed by the sweeper between sleep cycles. A larger 
 *  number makes the sweeper scan the entire chip more quickly, resulting in 
 *  faster reporting of detected errors, but consumes more CPU bandwidth
 *  resulting in a negative impact on the perfomance of other API threads. 
 *                                                                      \lb\lb
 *  In single chip systems, this API attribute is all that is needed to
 *  establish the parity sweeper burst size. The ''FM_PARITY_SWEEPER_CONFIG'' 
 *  switch attribute may be used to tune the sweeper's performance on a 
 *  per-switch basis in multi-chip systems. */
#define FM_AAK_API_PARITY_SWEEPER_READ_BURST_SIZE "api.paritySweeper.readBurstSize"
#define FM_AAT_API_PARITY_SWEEPER_READ_BURST_SIZE FM_API_ATTR_INT
#define FM_AAD_API_PARITY_SWEEPER_READ_BURST_SIZE 32

/** When the parity sweeper thread is enabled with the ''api.paritySweeper.enable''
 *  attribute, this attribute spcifies the default value for the 
 *  ''FM_PARITY_SWEEPER_CONFIG'' switch attribute and is the parity sweeper 
 *  sleep period (in ns) between read bursts. A smaller number makes the 
 *  sweeper scan the entire chip more quickly, resulting in faster reporting 
 *  of detected errors, but consumes more CPU bandwidth resulting in a 
 *  negative impact on the perfomance of other API threads. 
 *                                                                      \lb\lb
 *  In single chip systems, this API attribute is all that is needed to
 *  establish the parity sweeper sleep period. The ''FM_PARITY_SWEEPER_CONFIG'' 
 *  switch attribute may be used to tune the sweeper's performance on a 
 *  per-switch basis in multi-chip systems. */
#define FM_AAK_API_PARITY_SWEEPER_SLEEP_PERIOD  "api.paritySweeper.sleepPeriod"
#define FM_AAT_API_PARITY_SWEEPER_SLEEP_PERIOD  FM_API_ATTR_INT
#define FM_AAD_API_PARITY_SWEEPER_SLEEP_PERIOD  50000000

/** Controls whether auto-negotiation events are reported to the application
 *  in a ''FM_EVENT_PORT'' event. */
#define FM_AAK_API_FM4000_AUTONEG_GENERATE_EVENTS   "api.FM4000.autoNeg.generateEvents"
#define FM_AAT_API_FM4000_AUTONEG_GENERATE_EVENTS   FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_AUTONEG_GENERATE_EVENTS   FALSE

/** Controls whether auto-negotiation events are reported to the application
 *  in a ''FM_EVENT_PORT'' event. */
#define FM_AAK_API_FM6000_AUTONEG_GENERATE_EVENTS   "api.FM6000.autoNeg.generateEvents"
#define FM_AAT_API_FM6000_AUTONEG_GENERATE_EVENTS   FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_AUTONEG_GENERATE_EVENTS   FALSE

/** Controls the maximum number of ACL port sets supported on
 *  FM4000 devices.
 *  
 *  \note Deprecated in favor of api.portSets.maxPortSets, if
 *        not set, the default used will be the one from
 *        api.portSets.maxPortSets. */
#define FM_AAK_API_FM4000_MAX_ACL_PORT_SETS         "api.FM4000.maxACLPortSets"
#define FM_AAT_API_FM4000_MAX_ACL_PORT_SETS         FM_API_ATTR_INT
#define FM_AAD_API_FM4000_MAX_ACL_PORT_SETS         2048 

/** Controls the maximum number of ACL port sets supported on a SWAG. */
#define FM_AAK_API_SWAG_MAX_ACL_PORT_SETS         "api.SWAG.maxACLPortSets"
#define FM_AAT_API_SWAG_MAX_ACL_PORT_SETS         FM_API_ATTR_INT
#define FM_AAD_API_SWAG_MAX_ACL_PORT_SETS         2048 

/** Controls the maximum number of port sets supported */
#define FM_AAK_API_MAX_PORT_SETS                  "api.portSets.maxPortSets"
#define FM_AAT_API_MAX_PORT_SETS                  FM_API_ATTR_INT
#define FM_AAD_API_MAX_PORT_SETS                  2048 

/** Controls whether 1G ports use CRM (Counter Rate Monitoring) to monitor the
 *  SerDes signal detect for link down detection.
 *                                                                      \lb\lb
 *  The SerDes signal detect cannot be used to reliably indicate link state 
 *  unless the input signal is at an adequate level and rise time. Such 
 *  conditions may exist when a PHY is used, but cannot be depended upon for 
 *  CX4 or backplane applications. Accordingly, by default the API does not 
 *  rely on this signal for reporting link state. Some PHYs have been 
 *  observed to disable their XAUI TX when there is no transceiver and/or 
 *  cable connected, which under the right conditions can fool the  
 *  device into thinking that link is up when it is not. In these circumstances, 
 *  it may be desirable to monitor signal detect. This capability can be 
 *  enabled by setting this attribute to TRUE. */
#define FM_AAK_API_FM4000_1G_LINK_WITH_CRM          "api.FM4000.1GLinkWithCRM"
#define FM_AAT_API_FM4000_1G_LINK_WITH_CRM          FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_1G_LINK_WITH_CRM          FALSE

/** Controls whether CRM events are reported to the application
 *  in a ''FM_EVENT_CRM'' event. */
#define FM_AAK_API_FM4000_CRM_GENERATE_EVENTS   "api.FM4000.crm.generateEvents"
#define FM_AAT_API_FM4000_CRM_GENERATE_EVENTS   FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_CRM_GENERATE_EVENTS   TRUE

/** Controls whether the down port trigger uses a rate-limiter trigger
 *  (trigger action set to RateLimitAction) instead of trigger with action
 *  set to ForwardingAction=3 (Drop). The down port trigger is used to drop
 *  routed/multicast/special delivery frames which can not be handled by the
 *  PORT_CFG_2 register.
 *                                                                      \lb\lb
 *  When the down port trigger uses a ForwardingAction=3 trigger the drop mask
 *  (TRIGGER_ACTION_DROP.DropMask) is set to 0x01FFFFFF because of errata #45.
 *  This means that when a given frame fires the trigger it will be dropped on
 *  all ports instead of being dropped on the downed ports only. This can be a
 *  problem with IP multicast traffic. If one of the mcast-listener port of a
 *  given multicast group goes down then the traffic will stop on all other
 *  mcast-listener ports until the application removes the downed port from the
 *  multicast group.
 *                                                                      \lb\lb
 *  To work around this problem a rate-limiter trigger with its rate and
 *  capacity set to 0 is used. The drop mask set in TRIGGER_RATE_LIM_CFG_2 is
 *  not affected by errata #45, so it will contain only the downed ports, thus
 *  the traffic won't stop on the mcast-listener ports that are still UP.
 *                                                                      \lb\lb
 *  As this trigger uses a rate limiter, setting this attribute to TRUE reduces
 *  the number storm controllers available to the application from 16 to 15. */                                          
#define FM_AAK_API_FM4000_USE_RATE_LIMITER_TRIGGER  "api.FM4000.useRateLimiterTrigger"
#define FM_AAT_API_FM4000_USE_RATE_LIMITER_TRIGGER  FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_USE_RATE_LIMITER_TRIGGER  FALSE

/** Setting this attribute to TRUE improves the processing time of function
 *  ''fmSetSpanningTreePortState'' as the STP state change will be applied to
 *  the configured VLANs only. But doing so, the ingress boundary violation
 *  cannot be disabled because the spanning tree state of the VLAN for which
 *  the port is not a member will not be the state of the port itself. */
#define FM_AAK_API_FM4000_STP_STATE_ACTIVE_VLAN     "api.FM4000.stpState.activeVlan"
#define FM_AAT_API_FM4000_STP_STATE_ACTIVE_VLAN     FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_STP_STATE_ACTIVE_VLAN     FALSE

/** Controls the number of destination entries associated with
 *   each GLORT CAM entry for multicast groups. The value must be
 *   a multiple of 64 with a maximum value
 *   equal to 256. */
#define FM_AAK_API_FM4000_MCAST_MAX_ENTRIES_PER_CAM     "api.FM4000.mcastMaxEntriesPerCam"
#define FM_AAT_API_FM4000_MCAST_MAX_ENTRIES_PER_CAM     FM_API_ATTR_INT
#define FM_AAD_API_FM4000_MCAST_MAX_ENTRIES_PER_CAM     FM_MCG_MAX_ENTRIES_PER_GLORT

/** The first FFU slice on the FM6000 allocated for unicast routing. The value
 *  of this attribute must be less than or equal to the value for
 *  ''api.FM6000.ffu.unicastSliceRangeLast''.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM6000_FFU_UNICAST_SLICE_1ST     "api.FM6000.ffu.unicastSliceRangeFirst"
#define FM_AAT_API_FM6000_FFU_UNICAST_SLICE_1ST     FM_API_ATTR_INT
#define FM_AAD_API_FM6000_FFU_UNICAST_SLICE_1ST     0

/** The last FFU slice on the FM6000 allocated for unicast routing. The value
 *  of this attribute must be greater than or equal to the value for
 *  ''api.FM6000.ffu.unicastSliceRangeFirst'' and less than the value for
 *  ''api.FM6000.ffu.multicastSliceRangeLast''.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM6000_FFU_UNICAST_SLICE_LAST    "api.FM6000.ffu.unicastSliceRangeLast"
#define FM_AAT_API_FM6000_FFU_UNICAST_SLICE_LAST    FM_API_ATTR_INT
#define FM_AAD_API_FM6000_FFU_UNICAST_SLICE_LAST    11

/** The first FFU slice on the FM6000 allocated for multicast routing. The
 *  value of this attribute must be less than or equal to the value for
 *  ''api.FM6000.ffu.multicastSliceRangeLast'' and greater than or equal to the
 *  value for ''api.FM6000.ffu.unicastSliceRangeFirst''.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM6000_FFU_MULTICAST_SLICE_1ST   "api.FM6000.ffu.multicastSliceRangeFirst"
#define FM_AAT_API_FM6000_FFU_MULTICAST_SLICE_1ST   FM_API_ATTR_INT
#define FM_AAD_API_FM6000_FFU_MULTICAST_SLICE_1ST   0

/** The last FFU slice on the FM6000 allocated for multicast routing. The
 *  value of this attribute must be greater than or equal to the value for
 *  ''api.FM6000.ffu.multicastSliceRangeFirst''.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM6000_FFU_MULTICAST_SLICE_LAST  "api.FM6000.ffu.multicastSliceRangeLast"
#define FM_AAT_API_FM6000_FFU_MULTICAST_SLICE_LAST  FM_API_ATTR_INT
#define FM_AAD_API_FM6000_FFU_MULTICAST_SLICE_LAST  11

/** The first FFU slice on the FM6000 allocated for ACLs.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM6000_FFU_ACL_SLICE_1ST         "api.FM6000.ffu.aclSliceRangeFirst"
#define FM_AAT_API_FM6000_FFU_ACL_SLICE_1ST         FM_API_ATTR_INT
#define FM_AAD_API_FM6000_FFU_ACL_SLICE_1ST         12

/** The last FFU slice on the FM6000 allocated for ACLs.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM6000_FFU_ACL_SLICE_LAST        "api.FM6000.ffu.aclSliceRangeLast"
#define FM_AAT_API_FM6000_FFU_ACL_SLICE_LAST        FM_API_ATTR_INT
#define FM_AAD_API_FM6000_FFU_ACL_SLICE_LAST        23

/** The first FFU slice on the FM6000 allocated for CVLANs. By default we do not
 *  pre-allocate any FFU slices for the CVLAN API.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM6000_FFU_CVLAN_SLICE_1ST       "api.FM6000.ffu.cVlanSliceRangeFirst"
#define FM_AAT_API_FM6000_FFU_CVLAN_SLICE_1ST       FM_API_ATTR_INT
#define FM_AAD_API_FM6000_FFU_CVLAN_SLICE_1ST       -1

/** The last FFU slice on the FM6000 allocated for CVLANs.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM6000_FFU_CVLAN_SLICE_LAST      "api.FM6000.ffu.cVlanSliceRangeLast"
#define FM_AAT_API_FM6000_FFU_CVLAN_SLICE_LAST      FM_API_ATTR_INT
#define FM_AAD_API_FM6000_FFU_CVLAN_SLICE_LAST      -1

/** The first BST slice on the FM6000 allocated for unicast routes.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM6000_BST_ROUTING_SLICE_1ST     "api.FM6000.bst.routingSliceRangeFirst"
#define FM_AAT_API_FM6000_BST_ROUTING_SLICE_1ST     FM_API_ATTR_INT
#define FM_AAD_API_FM6000_BST_ROUTING_SLICE_1ST     -1

/** The last BST slice on the FM6000 allocated for unicast routes.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM6000_BST_ROUTING_SLICE_LAST    "api.FM6000.bst.routingSliceRangeLast"
#define FM_AAT_API_FM6000_BST_ROUTING_SLICE_LAST    FM_API_ATTR_INT
#define FM_AAD_API_FM6000_BST_ROUTING_SLICE_LAST    -1

/** The minimum precedence value allowed for unicast FFU entries. The value of 
 *  this attribute must be less than or equal to the value for
 *  ''api.FM6000.ffu.unicastPrecedenceMax''. This attribute will be used as the
 *  default value for unicast FFU entries. */
#define FM_AAK_API_FM6000_FFU_UNICAST_PRECEDENCE_MIN    "api.FM6000.ffu.unicastPrecedenceMin"
#define FM_AAT_API_FM6000_FFU_UNICAST_PRECEDENCE_MIN    FM_API_ATTR_INT
#define FM_AAD_API_FM6000_FFU_UNICAST_PRECEDENCE_MIN    1

/** The maximum precedence value allowed for unicast FFU entries. The value 
 *  of this attribute must be greater than or equal to the value for
 *  ''api.FM6000.ffu.unicastPrecedenceMin''. */
#define FM_AAK_API_FM6000_FFU_UNICAST_PRECEDENCE_MAX    "api.FM6000.ffu.unicastPrecedenceMax"
#define FM_AAT_API_FM6000_FFU_UNICAST_PRECEDENCE_MAX    FM_API_ATTR_INT
#define FM_AAD_API_FM6000_FFU_UNICAST_PRECEDENCE_MAX    5

/** The minimum precedence value allowed for multicast FFU entries. The value 
 *  of this attribute must be less than or equal to the value for
 *  ''api.FM6000.ffu.multicastPrecedenceMax''. This attribute will be used as 
 *  the default value for multicast FFU entries. */
#define FM_AAK_API_FM6000_FFU_MULTICAST_PRECEDENCE_MIN    "api.FM6000.ffu.multicastPrecedenceMin"
#define FM_AAT_API_FM6000_FFU_MULTICAST_PRECEDENCE_MIN    FM_API_ATTR_INT
#define FM_AAD_API_FM6000_FFU_MULTICAST_PRECEDENCE_MIN    1

/** The maximum precedence value allowed for multicast FFU entries. The value 
 *  of this attribute must be greater than or equal to the value for
 *  ''api.FM6000.ffu.multicastPrecedenceMin''. */
#define FM_AAK_API_FM6000_FFU_MULTICAST_PRECEDENCE_MAX    "api.FM6000.ffu.multicastPrecedenceMax"
#define FM_AAT_API_FM6000_FFU_MULTICAST_PRECEDENCE_MAX    FM_API_ATTR_INT
#define FM_AAD_API_FM6000_FFU_MULTICAST_PRECEDENCE_MAX    7

/** The minimum precedence value allowed for ACL rules. The value of this
 *  attribute must be less than or equal to the value for
 *  ''api.FM6000.ffu.ACLPrecedenceMax''. This attribute will be used as the
 *  default value for ACL entries. */
#define FM_AAK_API_FM6000_FFU_ACL_PRECEDENCE_MIN    "api.FM6000.ffu.ACLPrecedenceMin"
#define FM_AAT_API_FM6000_FFU_ACL_PRECEDENCE_MIN    FM_API_ATTR_INT
#define FM_AAD_API_FM6000_FFU_ACL_PRECEDENCE_MIN    6

/** The maximum precedence value allowed for ACL rules. The value of this
 *  attribute must be greater than or equal to the value for
 *  ''api.FM6000.ffu.ACLPrecedenceMin''. */
#define FM_AAK_API_FM6000_FFU_ACL_PRECEDENCE_MAX    "api.FM6000.ffu.ACLPrecedenceMax"
#define FM_AAT_API_FM6000_FFU_ACL_PRECEDENCE_MAX    FM_API_ATTR_INT
#define FM_AAD_API_FM6000_FFU_ACL_PRECEDENCE_MAX    7

/** Enable strict counter and policer validation, permitting a policer bank to 
 *  be linked to only one ACL. If disabled, every ACL would be able to use the
 *  count and/or police action, however, only the highest precedence rule will
 *  be the one that counts and/or polices if multiple ACLs hit in parallel. */
#define FM_AAK_API_FM6000_FFU_ACL_STRICT_COUNT_POLICE    "api.FM6000.ffu.ACLStrictCountPolice"
#define FM_AAT_API_FM6000_FFU_ACL_STRICT_COUNT_POLICE    FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_FFU_ACL_STRICT_COUNT_POLICE    TRUE

/** Enable the packet receive thread. This attribute must be set in
 *  ''fmPlatformInitialize'' for it to be effective. */
#define FM_AAK_API_PACKET_RECEIVE_ENABLE            "api.packetReceive.enable"
#define FM_AAT_API_PACKET_RECEIVE_ENABLE            FM_API_ATTR_BOOL
#define FM_AAD_API_PACKET_RECEIVE_ENABLE            TRUE 

/** The watermark for MTable garbage collection, expressed in
 *  percentage of the available space in the table */
#define FM_AAK_API_FM6000_MTABLE_CLEANUP_WATERMARK    "api.FM6000.mtable.cleanupWatermark"
#define FM_AAT_API_FM6000_MTABLE_CLEANUP_WATERMARK    FM_API_ATTR_INT
#define FM_AAD_API_FM6000_MTABLE_CLEANUP_WATERMARK    50

/** Enable FM6000 automatic MTable cleanup when out of resources due to needed
 *  garbage collection while executing an MTable operation on behalf of a
 *  caller.  TRUE means that the MTable code will try to perform garbage
 *  collection, including waiting any needed amount of time for that garbage
 *  collection to take place.  FALSE means that the MTable code will return
 *  error code FM_ERR_NO_MCAST_RESOURCES instead. */
#define FM_AAK_API_FM6000_MTABLE_AUTO_CLEANUP         "api.FM6000.mtable.autoCleanup"
#define FM_AAT_API_FM6000_MTABLE_AUTO_CLEANUP         FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_MTABLE_AUTO_CLEANUP         TRUE

/** Enables legacy multicast group restrictions which only allowed a
 *  single multicast address per group, required that the address be
 *  assigned prior to group activation, and automatically determined
 *  whether to use L2 or L3 hardware resources by examining the multicast
 *  address provided. TRUE means use the old restrictions, FALSE means
 *  use the new features. Default is FALSE. */
#define FM_AAK_API_1_ADDR_PER_MCAST_GROUP           "api.multicast.singleAddress"
#define FM_AAT_API_1_ADDR_PER_MCAST_GROUP           FM_API_ATTR_BOOL
#define FM_AAD_API_1_ADDR_PER_MCAST_GROUP           FALSE

/** When adding multicast groups that require MTable entries, if the MTable
 *  is full, but entries are due to expire, this attribute controls whether
 *  the API will suspend itself to wait for the entries to expire so they
 *  can be reused, or return immediately with an error. Setting this
 *  attribute to TRUE causes the API to wait for expiration then reuse the
 *  expired entries. Setting it to FALSE restores the original behavior of the
 *  API, causing an immediate return with error code 
 *  ''FM_ERR_NO_MCAST_RESOURCES''. */
#define FM_AAK_API_FM4000_WAIT_MTABLE_EXPIRY     "api.FM4000.waitMTableExpiry"
#define FM_AAT_API_FM4000_WAIT_MTABLE_EXPIRY     FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_WAIT_MTABLE_EXPIRY     TRUE


/** Multiple instances of the Alta White Model may be logically connected
 *  to simulate multiple-switch environments. Such an environment can utilize
 *  the API's stacking or SWAG support. Multiple switches may be managed by
 *  a single instance of the API (a single "unit") or each switch may be
 *  managed by its own private instance of the API (multiple "units") or a
 *  combination of both. When using a multiple unit environment, each
 *  instance of the API must be able to identify itself by a unit number.
 *  This attribute may be set when initializing an API instance to provide 
 *  the unit number. See ''Multiple Model Environments'' in the  
 *  API User Guide for more information. */
#define FM_AAK_API_PLATFORM_MODEL_POSITION   "api.platform.model.position"
#define FM_AAT_API_PLATFORM_MODEL_POSITION   FM_API_ATTR_INT
#define FM_AAD_API_PLATFORM_MODEL_POSITION   0


/** This API attribute indicates whether or not a port event
 *  with link status ''FM_PORT_STATUS_DFE_COMPLETE'' will be
 *  sent when DFE Tuning is completed (successfully or not) on a
 *  given port. Note that this attribute is overruled by
 *  "api.FM6000.linkDependsOnDfe" when the latter is
 *  set to TRUE. Indeed in that case a port event with link
 *  status ''FM_PORT_STATUS_LINK_DOWN'' or
 *  ''FM_PORT_STATUS_LINK_UP'' will always be generated upon
 *  DFE completion if a status transition has been detected */
#define FM_AAK_API_FM6000_SEND_DFE_TUNING_EVENT "api.FM6000.sendDfeTuningEvent"
#define FM_AAT_API_FM6000_SEND_DFE_TUNING_EVENT FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_SEND_DFE_TUNING_EVENT FALSE


/** Specifies whether the link status is considered dependent on the DFE
 *  Tuning process having completed (successfully or not) on a given port.
 *  Note that when this attribute is set to TRUE and a DFE Mode
 *  configuration change occurs that makes the DFE process
 *  restart, a ''FM_PORT_STATUS_LINK_DOWN'' event will be
 *  generated until DFE completes again. */
#define FM_AAK_API_FM6000_LINK_DEPENDS_ON_DFE     "api.FM6000.linkDependsOnDfe"
#define FM_AAT_API_FM6000_LINK_DEPENDS_ON_DFE     FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_LINK_DEPENDS_ON_DFE     TRUE


/** Specifies the time in seconds the API waits for DFE to complete on a
 *  given port before retrying. */
#define FM_AAK_API_FM6000_DFE_TUNING_TIMEOUT      "api.FM6000.dfeTuningTimeout" 
#define FM_AAT_API_FM6000_DFE_TUNING_TIMEOUT      FM_API_ATTR_INT 
#define FM_AAD_API_FM6000_DFE_TUNING_TIMEOUT      60 

/** Specifies the freelist exclusion mask for A0..B0. */
#define FM_AAK_API_FM6000_B0_FREELIST_EXCL_MASK     "api.FM6000.B0.freelistExclMask"
#define FM_AAT_API_FM6000_B0_FREELIST_EXCL_MASK     FM_API_ATTR_INT
#define FM_AAD_API_FM6000_B0_FREELIST_EXCL_MASK     0x0180

/** Specifies the freelist exclusion value for A0..B0. */
#define FM_AAK_API_FM6000_B0_FREELIST_EXCL_VALUE    "api.FM6000.B0.freelistExclValue"
#define FM_AAT_API_FM6000_B0_FREELIST_EXCL_VALUE    FM_API_ATTR_INT
#define FM_AAD_API_FM6000_B0_FREELIST_EXCL_VALUE    0x0000

/** Enables TRILL support */
#define FM_AAK_API_FM6000_TRILL_ENABLE    "api.FM6000.TRILL.enable"
#define FM_AAT_API_FM6000_TRILL_ENABLE    FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_TRILL_ENABLE    FALSE

/** This API attribute defines how many next-hop records should be reserved for
 *  use with virtual network tunnels. Note that the default value
 *  means that no virtual network tunnels will be supported, so applications
 *  that wish to use virtual networks MUST set the API attribute to the
 *  desired value. */
#define FM_AAK_API_NUM_VN_TUNNEL_NEXTHOPS         "api.vn.numNextHops"
#define FM_AAT_API_NUM_VN_TUNNEL_NEXTHOPS         FM_API_ATTR_INT
#define FM_AAD_API_NUM_VN_TUNNEL_NEXTHOPS         0

/** This API attribute defines how many 64-bit next-hop entries should be
 *  reserved for use as swap and reserved space when packing the next-hop
 *  table. */
#define FM_AAK_API_FM6000_NUM_RESERVED_NEXTHOPS   "api.FM6000.numReservedNextHops"
#define FM_AAT_API_FM6000_NUM_RESERVED_NEXTHOPS   FM_API_ATTR_INT
#define FM_AAD_API_FM6000_NUM_RESERVED_NEXTHOPS   64

/** This API attribute indicates whether route lookups by IP address are
 *  supported. Some API subsystems, such as virtual networking, require this
 *  feature and will force the API attribute to TRUE. */
#define FM_AAK_API_SUPPORT_ROUTE_LOOKUPS          "api.routing.supportRouteLookups"
#define FM_AAT_API_SUPPORT_ROUTE_LOOKUPS          FM_API_ATTR_BOOL
#define FM_AAD_API_SUPPORT_ROUTE_LOOKUPS          FALSE

/** Controls whether the routing maintenance thread is installed (default) or
 *  not. The routing maintenance thread monitors the usage of FFU and BST
 *  resources for routing and moves routes. */
#define FM_AAK_API_ROUTING_MAINTENANCE_ENABLE        "api.routing.maintenance.enable"
#define FM_AAT_API_ROUTING_MAINTENANCE_ENABLE        FM_API_ATTR_BOOL
#define FM_AAD_API_ROUTING_MAINTENANCE_ENABLE        TRUE

/** This API attribute indicates whether automatic vlan2 tagging updates
 *  should take place. The default value is TRUE and will enable normal
 *  operation. Setting this attribute to FALSE enables Virtual-Network tunneling
 *  to function properly. The virtual networking subsystem will force this
 *  attribute to FALSE. */
#define FM_AAK_API_AUTO_VLAN2_TAGGING             "api.vlan.autoVlan2Tagging"
#define FM_AAT_API_AUTO_VLAN2_TAGGING             FM_API_ATTR_BOOL
#define FM_AAD_API_AUTO_VLAN2_TAGGING             TRUE

/** This attribute provides a workaround for an errata in Alta 
 *  chip revisions prior to B2. It specifies whether the ALU13 
 *  must be disabled or not. It is recommended that this
 *  attribute be set to TRUE (the default) for Alta chip revisions 
 *  prior to B2. Note that the ALU13 is used by the following 
 *  features: Storm Controllers, sFlow, VxLAN. Therefore, these features
 *  won't be usable when this attribute is set to TRUE. */
#define FM_AAK_API_FM6000_ALU13_DISABLE     "api.FM6000.alu13.disable"
#define FM_AAT_API_FM6000_ALU13_DISABLE     FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_ALU13_DISABLE     TRUE

/** The name of the first microcode image file the platform layer
 *  will attempt to load. */
#define FM_AAK_API_FM6000_MICROCODE_IMAGE         "api.FM6000.microcodeImage"
#define FM_AAT_API_FM6000_MICROCODE_IMAGE         FM_API_ATTR_TEXT
#define FM_AAD_API_FM6000_MICROCODE_IMAGE         ""

/** The name of the first microcode support library to load. */
#define FM_AAK_API_FM6000_MICROCODE_LIB_1         "api.FM6000.microcodeLib1.Name"
#define FM_AAT_API_FM6000_MICROCODE_LIB_1         FM_API_ATTR_TEXT
#define FM_AAD_API_FM6000_MICROCODE_LIB_1         "libFocalPoint_AWM_switch_VN.so"

/** The name of the first microcode support library's init function. */
#define FM_AAK_API_FM6000_MICROCODE_INIT_FUNC_1   "api.FM6000.microcodeLib1.InitFunc"
#define FM_AAT_API_FM6000_MICROCODE_INIT_FUNC_1   FM_API_ATTR_TEXT
#define FM_AAD_API_FM6000_MICROCODE_INIT_FUNC_1   "fm6000VNUcLibraryInit"

/** The minimum number of route entries required for a batch to be moved
 *  to the BST. */
#define FM_AAK_API_FM6000_BST_MIN_ROUTE_ENTRIES_BATCH     "api.FM6000.bst.minRouteEntriesInBatch"
#define FM_AAT_API_FM6000_BST_MIN_ROUTE_ENTRIES_BATCH     FM_API_ATTR_INT
#define FM_AAD_API_FM6000_BST_MIN_ROUTE_ENTRIES_BATCH     512

/** Additional threshold value to avoid frequent swapping of routes
 *  between the FFU and the BST. */
#define FM_AAK_API_FM6000_ROUTE_SWAPPING_THRESHOLD        "api.FM6000.thresholdValueForRouteSwapping"
#define FM_AAT_API_FM6000_ROUTE_SWAPPING_THRESHOLD        FM_API_ATTR_INT
#define FM_AAD_API_FM6000_ROUTE_SWAPPING_THRESHOLD        128

/** Whether to perform a full boot on the FM10000. Set to FALSE if switch has
 *  already been booted via EEPROM. */
#define FM_AAK_API_FM10000_BOOT_FULLBOOT            "api.FM10000.boot.fullBoot"
#define FM_AAT_API_FM10000_BOOT_FULLBOOT            FM_API_ATTR_BOOL
#define FM_AAD_API_FM10000_BOOT_FULLBOOT            TRUE

/** The number of destination entries associated with each GLORT CAM
 *  entry for multicast groups. The value must be a multiple of 64 with
 *  a maximum value of 256. */
#define FM_AAK_API_FM10000_MCAST_MAX_ENTRIES_PER_CAM    "api.FM10000.mcastMaxEntriesPerCam"
#define FM_AAT_API_FM10000_MCAST_MAX_ENTRIES_PER_CAM    FM_API_ATTR_INT
#define FM_AAD_API_FM10000_MCAST_MAX_ENTRIES_PER_CAM    FM_MCG_MAX_ENTRIES_PER_GLORT

/** The first FFU slice on the FM10000 allocated for unicast routing. The value
 *  of this attribute must be less than or equal to the value for
 *  ''api.FM10000.ffu.unicastSliceRangeLast''.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM10000_FFU_UNICAST_SLICE_1ST     "api.FM10000.ffu.unicastSliceRangeFirst"
#define FM_AAT_API_FM10000_FFU_UNICAST_SLICE_1ST     FM_API_ATTR_INT
#define FM_AAD_API_FM10000_FFU_UNICAST_SLICE_1ST     0

/** The last FFU slice on the FM10000 allocated for unicast routing. The value
 *  of this attribute must be greater than or equal to the value for
 *  ''api.FM10000.ffu.unicastSliceRangeFirst'' and less than the value for
 *  ''api.FM10000.ffu.multicastSliceRangeLast''.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM10000_FFU_UNICAST_SLICE_LAST    "api.FM10000.ffu.unicastSliceRangeLast"
#define FM_AAT_API_FM10000_FFU_UNICAST_SLICE_LAST    FM_API_ATTR_INT
#define FM_AAD_API_FM10000_FFU_UNICAST_SLICE_LAST    15

/** The first FFU slice on the FM10000 allocated for multicast routing. The
 *  value of this attribute must be less than or equal to the value for
 *  ''api.FM10000.ffu.multicastSliceRangeLast'' and greater than or equal to the
 *  value for ''api.FM10000.ffu.unicastSliceRangeFirst''.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM10000_FFU_MULTICAST_SLICE_1ST   "api.FM10000.ffu.multicastSliceRangeFirst"
#define FM_AAT_API_FM10000_FFU_MULTICAST_SLICE_1ST   FM_API_ATTR_INT
#define FM_AAD_API_FM10000_FFU_MULTICAST_SLICE_1ST   0

/** The last FFU slice on the FM10000 allocated for multicast routing. The
 *  value of this attribute must be greater than or equal to the value for
 *  ''api.FM10000.ffu.multicastSliceRangeFirst''.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM10000_FFU_MULTICAST_SLICE_LAST  "api.FM10000.ffu.multicastSliceRangeLast"
#define FM_AAT_API_FM10000_FFU_MULTICAST_SLICE_LAST  FM_API_ATTR_INT
#define FM_AAD_API_FM10000_FFU_MULTICAST_SLICE_LAST  15

/** The first FFU slice on the FM10000 allocated for ACLs.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM10000_FFU_ACL_SLICE_1ST         "api.FM10000.ffu.aclSliceRangeFirst"
#define FM_AAT_API_FM10000_FFU_ACL_SLICE_1ST         FM_API_ATTR_INT
#define FM_AAD_API_FM10000_FFU_ACL_SLICE_1ST         16

/** The last FFU slice on the FM10000 allocated for ACLs.
 *  This attribute has been deprecated in favor of the ''FM_FFU_SLICE_ALLOCATIONS''
 *  switch attribute. */
#define FM_AAK_API_FM10000_FFU_ACL_SLICE_LAST        "api.FM10000.ffu.aclSliceRangeLast"
#define FM_AAT_API_FM10000_FFU_ACL_SLICE_LAST        FM_API_ATTR_INT
#define FM_AAD_API_FM10000_FFU_ACL_SLICE_LAST        31

/** The number of entries in the MAC Mapper that are reserved for router
 *  MAC addresses including all the virtual router MAC addresses. The value 
 *  of this attribute must be greater than or equal to 1 if routing is 
 *  enabled. */
#define FM_AAK_API_FM10000_FFU_MAPMAC_ROUTING        "api.FM10000.ffu.mapMAC.reservedForRouting"
#define FM_AAT_API_FM10000_FFU_MAPMAC_ROUTING        FM_API_ATTR_INT
#define FM_AAD_API_FM10000_FFU_MAPMAC_ROUTING        8

/** The minimum precedence value allowed for ACL rules. The value of this
 *  attribute must be less than or equal to the value for
 *  ''api.FM10000.ffu.ACLPrecedenceMax''. This attribute will be used as the
 *  default value for ACL entries. */
#define FM_AAK_API_FM10000_FFU_ACL_PRECEDENCE_MIN    "api.FM10000.ffu.ACLPrecedenceMin"
#define FM_AAT_API_FM10000_FFU_ACL_PRECEDENCE_MIN    FM_API_ATTR_INT
#define FM_AAD_API_FM10000_FFU_ACL_PRECEDENCE_MIN    1

/** The maximum precedence value allowed for ACL rules. The value of this
 *  attribute must be greater than or equal to the value for
 *  ''api.FM10000.ffu.ACLPrecedenceMin''. */
#define FM_AAK_API_FM10000_FFU_ACL_PRECEDENCE_MAX    "api.FM10000.ffu.ACLPrecedenceMax"
#define FM_AAT_API_FM10000_FFU_ACL_PRECEDENCE_MAX    FM_API_ATTR_INT
#define FM_AAD_API_FM10000_FFU_ACL_PRECEDENCE_MAX    7

/** Enable strict counter and policer validation, permitting a policer bank to 
 *  be linked to set of mutually exclusive ACL/Scenario tuple. If disabled,
 *  every ACL would be able to use the count and/or police action, however,
 *  only the highest precedence rule will be the one that counts and/or
 *  polices if multiple ACLs hit in parallel. */
#define FM_AAK_API_FM10000_FFU_ACL_STRICT_COUNT_POLICE "api.FM10000.ffu.ACLStrictCountPolice"
#define FM_AAT_API_FM10000_FFU_ACL_STRICT_COUNT_POLICE FM_API_ATTR_BOOL
#define FM_AAD_API_FM10000_FFU_ACL_STRICT_COUNT_POLICE TRUE

/* -------- Add new DOCUMENTED api attributes above this line! -------- */

/** @} (end of Doxygen group) */

/************************************************************************
 ****                                                                ****
 ****               END DOCUMENTED API ATTRIBUTES                    ****
 ****                                                                ****
 ****               BEGIN UNDOCUMENTED ATTRIBUTES                    ****
 ****                                                                ****
 ************************************************************************/
 
/************************************************************
 * The following attributes are not documented in the API
 * User Guide as they are intended for Intel Internal Use
 * only.
 ************************************************************/

/* -------- Add new UNDOCUMENTED api attributes below this line! -------- */

/* FM2000 only. Type: fm_bool. Indicates whether the number of scheduler tokens
 * should be adjusted at boot-time or should be left as-is. */
#define FM_AAK_API_FM2000_BOOT_ADJUST_SCHED_TOKENS  "api.FM2000.boot.adjustSchedTokens"
#define FM_AAD_API_FM2000_BOOT_ADJUST_SCHED_TOKENS  TRUE

/* FM4000 only. Type fm_bool. Indicates whether the MA Table should be split
 * into two 8K halves or as a unified 16K table. */
#define FM_AAK_API_FM4000_BOOT_SPLIT_MA_TABLE       "api.FM4000.boot.splitMATable"
#define FM_AAD_API_FM4000_BOOT_SPLIT_MA_TABLE       FALSE

/* FM4000 only. Type fm_bool. Indicates whether the MA Table maintenance
 * thread should reload locked MA Table entries following a purge. 
 * Used by the legacy entry-based scanner only. */
#define FM_AAK_API_FM4000_MA_TABLE_RELOAD_STATIC_ENTRIES_ON_PURGE  "api.FM4000.MATable.reloadStaticEntriesOnPurge"
#define FM_AAD_API_FM4000_MA_TABLE_RELOAD_STATIC_ENTRIES_ON_PURGE  FALSE

/* FM4000 only. Type fm_bool. Indicates whether the API must validate that
 * the MA Table entry match the entry in the update learn event prior to give
 * the event to the application (bz-2717) */
#define FM_AAK_API_FM4000_VALIDATE_LEARN_EVENT      "api.FM4000.validateLearnEvent"
#define FM_AAD_API_FM4000_VALIDATE_LEARN_EVENT      TRUE

/* FM4000 only. Type fm_bool. Specifies whether locked MA Table entries
 * should be restored during Purge Complete processing. */
#define FM_AAK_API_FM4000_PURGE_RESTORE_LOCKED      "api.FM4000.purge.restoreLocked"
#define FM_AAD_API_FM4000_PURGE_RESTORE_LOCKED      TRUE

/* FM4000 only. Type fm_bool. Specifies whether the MA Table maintenance 
 * thread should use the new row-based table scanner instead of the legacy 
 * entry-based scanner. */
#define FM_AAK_API_FM4000_BOOT_USE_SCAN_MA_TABLE    "api.FM4000.boot.useScanMATable"
#define FM_AAD_API_FM4000_BOOT_USE_SCAN_MA_TABLE    TRUE

#define FM_AAK_API_FM4000_BOOT_ADJUST_SCHED_TOKENS  "api.FM4000.boot.adjustSchedTokens"
#define FM_AAD_API_FM4000_BOOT_ADJUST_SCHED_TOKENS  TRUE

#define FM_AAK_API_FM4000_BOOT_LRNTHROTTLERATE      "api.FM4000.boot.learningThrottleRate"
#define FM_AAD_API_FM4000_A0_BOOT_LRNTHROTTLERATE   125
#define FM_AAD_API_FM4000_A2_BOOT_LRNTHROTTLERATE   125

#define FM_AAK_API_FM4000_MULTICAST_EXPIRY          "api.FM4000.multicast.expiration"
#define FM_AAD_API_FM4000_MULTICAST_EXPIRY          3

#define FM_AAK_DEBUG_BOOT_INTERRUPT_HANDLER         "debug.boot.interruptHandler.disable"
#define FM_AAD_DEBUG_BOOT_INTERRUPT_HANDLER         FALSE

/* Type: fm_int. Indicates the base configuration for FM_STATS_CFG. 
 * should be adjusted at boot-time or should be left as-is. */
#define FM_AAK_API_FM4000_STATS_CFG_BASE            "api.FM4000.stats.groupEnableBase"
#define FM_AAD_API_FM4000_STATS_CFG_BASE            0x0

#define FM_AAK_API_FM2000_STATS_CFG_BASE            "api.FM2000.stats.groupEnableBase"
#define FM_AAD_API_FM2000_STATS_CFG_BASE            0x0

/* Enables or disables DMA on the fm85xxep platform (FM4000 only). User 
 * platforms need not support this attribute, but simply hard-code whether 
 * DMA is used. */
#define FM_AAK_API_PLATFORM_DMA                     "api.platform.dma"
#define FM_AAD_API_PLATFORM_DMA                     FALSE

#define FM_AAK_API_FM4000_PURGE_TIMEOUT             "api.FM4000.purgeTimeout"
#define FM_AAD_API_FM4000_PURGE_TIMEOUT             2

/* Enable the MAC table maintenance thread. Should be adjusted at boot time or
 * left as-is. */
#define FM_AAK_API_MA_TABLE_MAINTENENANCE_ENABLE    "api.maTableMaintenance.enable"
#define FM_AAT_API_MA_TABLE_MAINTENENANCE_ENABLE    FM_API_ATTR_BOOL
#define FM_AAD_API_MA_TABLE_MAINTENENANCE_ENABLE    TRUE

/* Enable the fast maintenance thread. The change only takes affect when
 * called in platform initialization. */
#define FM_AAK_API_FAST_MAINTENANCE_ENABLE          "api.fastMaintenance.enable"
#define FM_AAT_API_FAST_MAINTENANCE_ENABLE          FM_API_ATTR_BOOL
#define FM_AAD_API_FAST_MAINTENANCE_ENABLE          FALSE 

/* Fast maintenance thread period (in ns) defaults to 20 ms.
 * The change only takes affect when called in platform initialization. */
#define FM_AAK_API_FAST_MAINTENANCE_PERIOD          "api.fastMaintenance.period"
#define FM_AAT_API_FAST_MAINTENANCE_PERIOD          FM_API_ATTR_INT
#define FM_AAD_API_FAST_MAINTENANCE_PERIOD          20000000
                                                                                          
/* Automatic creation of logical ports for remote glorts. */
#define FM_AAK_API_FM4000_CREATE_REMOTE_LOGICAL_PORTS   "api.FM4000.createRemoteLogicalPorts"
#define FM_AAT_API_FM4000_CREATE_REMOTE_LOGICAL_PORTS   FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_CREATE_REMOTE_LOGICAL_PORTS   FALSE

/* Automatic creation of logical ports for remote glorts. */
#define FM_AAK_API_FM6000_CREATE_REMOTE_LOGICAL_PORTS   "api.FM6000.createRemoteLogicalPorts"
#define FM_AAT_API_FM6000_CREATE_REMOTE_LOGICAL_PORTS   FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_CREATE_REMOTE_LOGICAL_PORTS   FALSE

/* Specifies whether the API should automatically create logical ports
 * for remote glorts. */
#define FM_AAK_API_FM10000_CREATE_REMOTE_LOGICAL_PORTS  "api.FM10000.createRemoteLogicalPorts"
#define FM_AAT_API_FM10000_CREATE_REMOTE_LOGICAL_PORTS  FM_API_ATTR_BOOL
#define FM_AAD_API_FM10000_CREATE_REMOTE_LOGICAL_PORTS  FALSE

/* Redefine the CPU mirror GLORT. Could be used to distinguish mirrored frames
 * from logged frames to the CPU port. Default value of -1 indicates that
 * mirrored frames to the CPU port will use the default CPU GLORT. */
#define FM_AAK_API_FM4000_CPU_MIRROR_GLORT          "api.FM4000.cpuMirrorGlort"
#define FM_AAT_API_FM4000_CPU_MIRROR_GLORT          FM_API_ATTR_INT
#define FM_AAD_API_FM4000_CPU_MIRROR_GLORT          -1

/* Renumber Uplink ports as ports 1-8 instead of 49-56 */
#define FM_AAK_DEBUG_VEGAS_UPLINKS_FIRST            "api.vegas.uplinkPortsFirst"
#define FM_AAT_DEBUG_VEGAS_UPLINKS_FIRST            FM_API_ATTR_BOOL
#define FM_AAD_DEBUG_VEGAS_UPLINKS_FIRST            FALSE

/* Disable the physical port GLORT LAG Filtering. */
#define FM_AAK_API_STRICT_GLORT_PHYSICAL            "api.strict.glortPhysical"
#define FM_AAT_API_STRICT_GLORT_PHYSICAL            FM_API_ATTR_BOOL
#define FM_AAD_API_STRICT_GLORT_PHYSICAL            TRUE

/* Control whether or not resetting the watermark registers when the 
 * FM_AUTO_PAUSE_MODE is set to FM_DISABLED. */
#define FM_AAK_API_RESET_WATERMARK_AT_PAUSE_OFF     "api.autoPauseOff.resetWatermark"
#define FM_AAT_API_RESET_WATERMARK_AT_PAUSE_OFF     FM_API_ATTR_BOOL
#define FM_AAD_API_RESET_WATERMARK_AT_PAUSE_OFF     TRUE

/* Controls the auto-negotiation state machine timeout for clause 37 */
#define FM_AAK_API_FM4000_AUTONEG_CLAUSE_37_TIMEOUT "api.FM4000.autoNeg.clause37.timeout"
#define FM_AAT_API_FM4000_AUTONEG_CLAUSE_37_TIMEOUT FM_API_ATTR_INT
#define FM_AAD_API_FM4000_AUTONEG_CLAUSE_37_TIMEOUT 5000

/* Controls the auto-negotiation state machine link timeout in micro seconds for clause 37 */
#define FM_AAK_API_FM6000_AUTONEG_CLAUSE_37_TIMEOUT "api.FM6000.autoNeg.clause37.timeout"
#define FM_AAT_API_FM6000_AUTONEG_CLAUSE_37_TIMEOUT FM_API_ATTR_INT
#define FM_AAD_API_FM6000_AUTONEG_CLAUSE_37_TIMEOUT 15000

/* Controls the auto-negotiation state machine timeout for SGMII */
#define FM_AAK_API_FM4000_AUTONEG_SGMII_TIMEOUT     "api.FM4000.autoNeg.SGMII.timeout"
#define FM_AAT_API_FM4000_AUTONEG_SGMII_TIMEOUT     FM_API_ATTR_INT
#define FM_AAD_API_FM4000_AUTONEG_SGMII_TIMEOUT     1118

/* Controls the auto-negotiation state machine timeout in micro seconds for SGMII */
#define FM_AAK_API_FM6000_AUTONEG_SGMII_TIMEOUT     "api.FM6000.autoNeg.SGMII.timeout"
#define FM_AAT_API_FM6000_AUTONEG_SGMII_TIMEOUT     FM_API_ATTR_INT
#define FM_AAD_API_FM6000_AUTONEG_SGMII_TIMEOUT     1600

/* Controls the auto-negotiation state machine tx timeout for clause 37 (msec)*/
#define FM_AAK_API_FM4000_AUTONEG_CLAUSE_37_TX_TIMEOUT "api.FM4000.autoNeg.clause37.txTimeout"
#define FM_AAT_API_FM4000_AUTONEG_CLAUSE_37_TX_TIMEOUT FM_API_ATTR_FLOAT
#define FM_AAD_API_FM4000_AUTONEG_CLAUSE_37_TX_TIMEOUT 15.0

/* Controls the auto-negotiation state machine tx timeout for SGMII (msec) */
#define FM_AAK_API_FM4000_AUTONEG_SGMII_TX_TIMEOUT     "api.FM4000.autoNeg.SGMII.txTimeout"
#define FM_AAT_API_FM4000_AUTONEG_SGMII_TX_TIMEOUT     FM_API_ATTR_FLOAT
#define FM_AAD_API_FM4000_AUTONEG_SGMII_TX_TIMEOUT     1.9

/* Controls wether the software restarts auto-negotiation on failure */
#define FM_AAK_API_FM4000_AUTONEG_RESTART_FAIL      "api.FM4000.autoNeg.restartOnFail"
#define FM_AAT_API_FM4000_AUTONEG_RESTART_FAIL      FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_AUTONEG_RESTART_FAIL      TRUE

/* Control whether switch aggregates are to automatically maintain sub-switch
 * state for the application.
 */
#define FM_AAK_API_SWAG_AUTO_SUB_SWITCHES           "api.swag.autoSubSwitches"
#define FM_AAT_API_SWAG_AUTO_SUB_SWITCHES           FM_API_ATTR_BOOL
#define FM_AAD_API_SWAG_AUTO_SUB_SWITCHES           TRUE

/* Control whether switch aggregates are to automatically manage internal
 * (inter-switch) ports for the application.
 */
#define FM_AAK_API_SWAG_AUTO_INTERNAL_PORTS         "api.swag.autoInternalPorts"
#define FM_AAT_API_SWAG_AUTO_INTERNAL_PORTS         FM_API_ATTR_BOOL
#define FM_AAD_API_SWAG_AUTO_INTERNAL_PORTS         TRUE

#define FM_AAK_API_FM4000_FIBM_TEST_RIG             "api.FM4000.FIBM.testRig"
#define FM_AAT_API_FM4000_FIBM_TEST_RIG             FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_FIBM_TEST_RIG             FALSE

#define FM_AAK_API_FM4000_FIBM_TEST_PORT            "api.FM4000.FIBM.testPort"
#define FM_AAT_API_FM4000_FIBM_TEST_PORT            FM_API_ATTR_INT
#define FM_AAD_API_FM4000_FIBM_TEST_PORT            0 

/* Control whether the API read the logical port to physical port map table
 * from a file. */
#define FM_AAK_API_PORT_REMAP_TABLE                "api.portRemapTable"
#define FM_AAT_API_PORT_REMAP_TABLE                FM_API_ATTR_TEXT
#define FM_AAD_API_PORT_REMAP_TABLE                ""

/* A high packet rate directed into a SerDes from the wire while the port is
 * transitioned to an UP state can result in an erroneous link up condition 
 * being reported by the SerDes. This attribute enables a more thorough 
 * validation of link state. */
#define FM_AAK_API_FM4000_1GSERDES_LINK_CHECK      "api.FM4000.enable1GSerdesExtraLinkCheck"
#define FM_AAT_API_FM4000_1GSERDES_LINK_CHECK      FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_1GSERDES_LINK_CHECK      TRUE

/* Specifies whether traffic has to be stopped when calculating the
 * watermarks. */
#define FM_AAK_API_FM2000_STOP_TRAFFIC_FOR_WMSET    "api.FM4000.stopTrafficForWmSet"
#define FM_AAT_API_FM2000_STOP_TRAFFIC_FOR_WMSET    FM_API_ATTR_BOOL
#define FM_AAD_API_FM2000_STOP_TRAFFIC_FOR_WMSET    FALSE

/* Controls whether BPDU frames are trapped when a port is in
 * STP disabled mode on the ingress instance. */
#define FM_AAK_API_FM6000_TRAP_BPDU_ON_STP_DISC   "api.FM6000.trapBpduOnStpDisc"
#define FM_AAT_API_FM6000_TRAP_BPDU_ON_STP_DISC   FM_API_ATTR_BOOL 
#define FM_AAD_API_FM6000_TRAP_BPDU_ON_STP_DISC   FALSE

/* When the parity sweeper thread is enabled with the api.paritySweeper.enable
 * attribute, this attribute indicates whether the parity sweeper is used on 
 * switches accessed via FIBM. Enabling the sweeper on FIBM-accessed switches 
 * greatly increases the amount of FIBM frame traffic. */
#define FM_AAK_API_PARITY_SWEEPER_FIBM_ENABLE   "api.paritySweeper.FibmEnable"
#define FM_AAT_API_PARITY_SWEEPER_FIBM_ENABLE   FM_API_ATTR_BOOL
#define FM_AAD_API_PARITY_SWEEPER_FIBM_ENABLE   FALSE

/* Controls whether the fmYield should be done in fm4000RefreshSpanningTree */
#define FM_AAK_API_FM4000_ENABLE_YIELD_IN_REFRESH_STP   "api.FM4000.enableYield.inRefreshStp"
#define FM_AAT_API_FM4000_ENABLE_YIELD_IN_REFRESH_STP   FM_API_ATTR_BOOL
#define FM_AAD_API_FM4000_ENABLE_YIELD_IN_REFRESH_STP   TRUE

/* Controls whether the platform enables bypass mode on startup.  This only has
 * an effect in platforms that support bypass mode. */
#define FM_AAK_API_PLATFORM_BYPASS_ENABLE   "api.platform.bypassEnable"
#define FM_AAT_API_PLATFORM_BYPASS_ENABLE   FM_API_ATTR_BOOL
#define FM_AAD_API_PLATFORM_BYPASS_ENABLE   FALSE

/* Delete semaphore timeout in seconds. defaults to 5 sec. */
#define FM_AAK_API_LAG_DELETE_SEMAPHORE_TIMEOUT   "api.lag.delSemaphore.timeout"
#define FM_AAT_API_LAG_DELETE_SEMAPHORE_TIMEOUT   FM_API_ATTR_INT
#define FM_AAD_API_LAG_DELETE_SEMAPHORE_TIMEOUT   5

/* Flat text-file based loading of microcode */
#define FM_AAK_API_FM6000_LOAD_MICROCODE_IMAGE    "api.FM6000.loadMicrocodeImage"
#define FM_AAT_API_FM6000_LOAD_MICROCODE_IMAGE    FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_LOAD_MICROCODE_IMAGE    TRUE

/* Controls whether the loaded microcode will be validated on startup */
#define FM_AAK_API_FM6000_VALIDATE_MICROCODE      "api.FM6000.validateMicrocode"
#define FM_AAT_API_FM6000_VALIDATE_MICROCODE      FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_VALIDATE_MICROCODE      FALSE

/* Name of the first microcode support library's white model init function. */
#define FM_AAK_API_FM6000_MICROCODE_MODEL_INIT_FUNC_1   "api.FM6000.microcodeLib1.ModelInitFunc"
#define FM_AAT_API_FM6000_MICROCODE_MODEL_INIT_FUNC_1   FM_API_ATTR_TEXT
#define FM_AAD_API_FM6000_MICROCODE_MODEL_INIT_FUNC_1   "fm6000ModelLibraryInit"

/* Whether to load the SPICO controller on reset. */
#define FM_AAK_API_FM6000_LOAD_SPICO_CODE       "api.FM6000.loadSpicoCode"
#define FM_AAT_API_FM6000_LOAD_SPICO_CODE       FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_LOAD_SPICO_CODE       TRUE

/* Platform register access mode, either EBI or PCIe */
#define FM_AAK_API_FM6000_PLATFORM_REG_ACCESS_PCIE      "api.FM6000.regAccessPcie"
#define FM_AAT_API_FM6000_PLATFORM_REG_ACCESS_PCIE      FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_PLATFORM_REG_ACCESS_PCIE      TRUE

/* Platform register access mode, either EBI or PCIe */
#define FM_AAK_API_FM6000_PLATFORM_REG_ACCESS_I2C      "api.FM6000.regAccessI2C"
#define FM_AAT_API_FM6000_PLATFORM_REG_ACCESS_I2C      FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_PLATFORM_REG_ACCESS_I2C      FALSE

/* Indicates whether chip is booted by eeprom or not */
#define FM_AAK_API_FM6000_PLATFORM_EEPROM_BOOTED      "api.FM6000.eepromBooted"
#define FM_AAT_API_FM6000_PLATFORM_EEPROM_BOOTED      FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_PLATFORM_EEPROM_BOOTED      FALSE

/* Indicates which EEPROM boot image should be selected. Valid values
 * range from 0 to 3. This attribute only applies when switch boots from
 * EEPROM */
#define FM_AAK_API_FM6000_PLATFORM_EEPROM_BOOT_IMAGE  "api.FM6000.eepromBootImage"
#define FM_AAT_API_FM6000_PLATFORM_EEPROM_BOOT_IMAGE  FM_API_ATTR_INT
#define FM_AAD_API_FM6000_PLATFORM_EEPROM_BOOT_IMAGE  0

/* The spanning tree state of internal ports is forced to
 * FM_STP_STATE_FORWARDING to ensure that SWAG links are not broken. To 
 * further protect theses links, the API prevents the STP state from being
 * changed on theses ports. Enabling this attribute will overide this
 * behavior and therefore enable changing the STP state on internal links */
#define FM_AAK_API_STP_ENABLE_INTERNAL_PORT_CTRL    "api.stp.enableInternalPortControl"
#define FM_AAT_API_STP_ENABLE_INTERNAL_PORT_CTRL    FM_API_ATTR_BOOL
#define FM_AAD_API_STP_ENABLE_INTERNAL_PORT_CTRL    FALSE

/* Improvement to latency. This include speeding up the EPL clock to 
 * 350MHZ and reducing the EPL fifos to minimal values. This is
 * experimental until we prove for certainty that those values are 
 * not creating any problem, then the parameter could then be removed. 
 * OWNED BY ALAIN.*/
#define FM_AAK_API_FM6000_ENABLE_LOW_LATENCY_EPL    "api.FM6000.enableLowLatencyEPL"
#define FM_AAT_API_FM6000_ENABLE_LOW_LATENCY_EPL    FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_ENABLE_LOW_LATENCY_EPL    FALSE

/* 40G port - EPL TX FIFO Anti-Bubble Watermark
 * 
 * The Port Channel Anti-Bubble FIFO provides buffering to increase the
 * word-to-word jitter tolerance.  TxAntiBubbleWatermark specifies the
 * number of words that will be stored in the FIFO before a frame will
 * be scheduled for egress.  The minimum setting is 1, the maximum
 * setting is 62.  The settings 0 and 63 should not be used, 
 * the behavior is undefined.
 *
 * The value set in the TxAntiBubbleWatermark will affect the latency.
 * Higher the value is, more increase will be seen on the latency.
 * For example, here are some empirical results:
 * Compare to a value of 1, 8 will increase the latency on average by 20 ns.
 * A value of 12 increase the latency on average by 30 ns.
 * The default value of 4 should add 10 ns on average to the latency. */ 
#define FM_AAK_API_FM6000_40G_ANTI_BUBBLE_WM        "api.FM6000.40G.antiBubbleWm"
#define FM_AAT_API_FM6000_40G_ANTI_BUBBLE_WM        FM_API_ATTR_INT
#define FM_AAD_API_FM6000_40G_ANTI_BUBBLE_WM        4

/* Indicates whether the interrupt will be received from PCIe MSI
 * or not */
#define FM_AAK_API_FM6000_PLATFORM_MSI_ENABLED     "api.FM6000.msiEnabled"
#define FM_AAT_API_FM6000_PLATFORM_MSI_ENABLED      FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_PLATFORM_MSI_ENABLED      FALSE

/* Indicates whether chip is booted by I2C or not */
#define FM_AAK_API_FM6000_PLATFORM_I2C_BOOTED      "api.FM6000.i2cBooted"
#define FM_AAT_API_FM6000_PLATFORM_I2C_BOOTED      FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_PLATFORM_I2C_BOOTED      FALSE
 
/* Indicates whether the platform will be used to generate an EEPROM image */
#define FM_AAK_API_GENERATE_EEPROM_MODE            "api.generateEepromMode"
#define FM_AAT_API_GENERATE_EEPROM_MODE             FM_API_ATTR_INT
#define FM_AAD_API_GENERATE_EEPROM_MODE             0

/* Selects the logical port map to use with the White Model. */
#define FM_AAK_API_PLATFORM_MODEL_PORT_MAP_TYPE     "api.platform.model.portMapType"
#define FM_AAT_API_PLATFORM_MODEL_PORT_MAP_TYPE     FM_API_ATTR_INT
#define FM_AAD_API_PLATFORM_MODEL_PORT_MAP_TYPE     0

/* Specifies the set of switches that are to be modeled in
 * the whiteModel platform. The set consists of one or more comma separated
 * entries. Each entry is formatted as switchNumber-switchFamily. The
 * switchNumber field specifies the switch number associated with the 
 * entry and should be a non-negative integer. The switchFamily field 
 * specifies the switch family to be modeled and is case-insensitive. */ 
#define FM_AAK_API_PLATFORM_MODEL_SWITCH_TYPE       "api.platform.model.switchType"
#define FM_AAT_API_PLATFORM_MODEL_SWITCH_TYPE       FM_API_ATTR_TEXT
#define FM_AAD_API_PLATFORM_MODEL_SWITCH_TYPE       "0-HLP"

/* Specifies whether the white model packet queue interface should send
 * ''FM_MODEL_MSG_PACKET_EOT'' messages after each received packet
 * has been fully processed. */
#define FM_AAK_API_PLATFORM_MODEL_SEND_EOT          "api.platform.model.sendEOT"
#define FM_AAT_API_PLATFORM_MODEL_SEND_EOT          FM_API_ATTR_BOOL
#define FM_AAD_API_PLATFORM_MODEL_SEND_EOT          FALSE

/* Specifies whether routing should set the combination of Next Hop tag bits
 * used to identify an unresolved GLORT-based ARP entry that traps to the
 * CPU port. */
#define FM_AAK_API_FM6000_ENABLE_NEXTHOP_MISS_TAG   "api.FM6000.enableNextHopMissTag"
#define FM_AAT_API_FM6000_ENABLE_NEXTHOP_MISS_TAG   FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_ENABLE_NEXTHOP_MISS_TAG   TRUE
 
/* This API attribute indicates the first switch priority for the first set of
 * special forwarding rules */
#define FM_AAK_API_FM6000_SPECIAL_FWD_RULE_PRI1    "api.FM6000.specialFwdRulePri1" 
#define FM_AAT_API_FM6000_SPECIAL_FWD_RULE_PRI1    FM_API_ATTR_INT 
#define FM_AAD_API_FM6000_SPECIAL_FWD_RULE_PRI1    1 

/* This API attribute indicates the second switch priority for the first set of
 * special forwarding rules */
#define FM_AAK_API_FM6000_SPECIAL_FWD_RULE_PRI2    "api.FM6000.specialFwdRulePri2" 
#define FM_AAT_API_FM6000_SPECIAL_FWD_RULE_PRI2    FM_API_ATTR_INT 
#define FM_AAD_API_FM6000_SPECIAL_FWD_RULE_PRI2    2 

/* This API attribute indicates the logical source port mask for the first set of
 * special forwarding rules */
#define FM_AAK_API_FM6000_SPECIAL_FWD_RULE_SMASK   "api.FM6000.specialFwdRuleSmask"
#define FM_AAT_API_FM6000_SPECIAL_FWD_RULE_SMASK   FM_API_ATTR_TEXT
#define FM_AAD_API_FM6000_SPECIAL_FWD_RULE_SMASK   "0x0"

/* This API attribute indicates the logical destination port mask for the first
 * set of special forwarding rules */
#define FM_AAK_API_FM6000_SPECIAL_FWD_RULE_DMASK   "api.FM6000.specialFwdRuleDmask"
#define FM_AAT_API_FM6000_SPECIAL_FWD_RULE_DMASK   FM_API_ATTR_TEXT
#define FM_AAD_API_FM6000_SPECIAL_FWD_RULE_DMASK   "0x0"

/* This API attribute indicates the first switch priority for the second set of
 * special forwarding rules */
#define FM_AAK_API_FM6000_SPECIAL_FWD_RULE_PRI1_B  "api.FM6000.specialFwdRulePri1B" 
#define FM_AAT_API_FM6000_SPECIAL_FWD_RULE_PRI1_B  FM_API_ATTR_INT 
#define FM_AAD_API_FM6000_SPECIAL_FWD_RULE_PRI1_B  1 

/* This API attribute indicates the second switch priority for the second set of
 * special forwarding rules */
#define FM_AAK_API_FM6000_SPECIAL_FWD_RULE_PRI2_B  "api.FM6000.specialFwdRulePri2B" 
#define FM_AAT_API_FM6000_SPECIAL_FWD_RULE_PRI2_B  FM_API_ATTR_INT 
#define FM_AAD_API_FM6000_SPECIAL_FWD_RULE_PRI2_B  2 

/* This API attribute indicates the logical source port mask for the second set of
 * special forwarding rules */
#define FM_AAK_API_FM6000_SPECIAL_FWD_RULE_SMASK_B "api.FM6000.specialFwdRuleSmaskB"
#define FM_AAT_API_FM6000_SPECIAL_FWD_RULE_SMASK_B FM_API_ATTR_TEXT
#define FM_AAD_API_FM6000_SPECIAL_FWD_RULE_SMASK_B "0x0"

/* This API attribute indicates the logical destination port mask for the second
 * set of special forwarding rules */
#define FM_AAK_API_FM6000_SPECIAL_FWD_RULE_DMASK_B "api.FM6000.specialFwdRuleDmaskB"
#define FM_AAT_API_FM6000_SPECIAL_FWD_RULE_DMASK_B FM_API_ATTR_TEXT
#define FM_AAD_API_FM6000_SPECIAL_FWD_RULE_DMASK_B "0x0"

/* This API attribute indicates if the Vlan Egress Boundary check must be
 * performed at L2F level on L3 Switched Multicast frame */
#define FM_AAK_API_FM6000_MCAST_NO_ROUTE_EVB_CHECK "api.FM6000.noRouteEvbCheck"
#define FM_AAT_API_FM6000_MCAST_NO_ROUTE_EVB_CHECK FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_MCAST_NO_ROUTE_EVB_CHECK FALSE

/* This API attribute is used to debug the initialization of the FM6000
 * scheduler. Enabling this will dump information when scheduler is
 * initialized. */
#define FM_AAK_API_FM6000_DEBUG_SCHEDULER           "api.FM6000.debugScheduler"
#define FM_AAT_API_FM6000_DEBUG_SCHEDULER           FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_DEBUG_SCHEDULER           FALSE

/* This API attribute is for debug purposes. The sync token
 * should always be enabled. */
#define FM_AAK_API_FM6000_ENABLE_SYNC_TOKEN         "api.FM6000.enableSyncToken"
#define FM_AAT_API_FM6000_ENABLE_SYNC_TOKEN         FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_ENABLE_SYNC_TOKEN         TRUE

/* Indicates the mask revision to select, 0 for A0, 1 for B0 */
#define FM_AAK_API_PLATFORM_MODEL_FM6000_REVISION   "api.platform.model.FM6000.revision"
#define FM_AAT_API_PLATFORM_MODEL_FM6000_REVISION   FM_API_ATTR_INT
#define FM_AAD_API_PLATFORM_MODEL_FM6000_REVISION   0

/* Path to freelist file */
#define FM_AAK_API_FM6000_DEBUG_FREELIST            "api.FM6000.debug.freelist"
#define FM_AAT_API_FM6000_DEBUG_FREELIST            FM_API_ATTR_TEXT
#define FM_AAD_API_FM6000_DEBUG_FREELIST            "freelist.txt"

/* Specifies whether the IEEE 1588 reference clock is enabled. */
#define FM_AAK_API_PLATFORM_ENABLE_REF_CLOCK        "api.platform.enableRefClock"
#define FM_AAT_API_PLATFORM_ENABLE_REF_CLOCK        FM_API_ATTR_BOOL
#define FM_AAD_API_PLATFORM_ENABLE_REF_CLOCK        TRUE

/* Specifies whether to initialize the IEEE 1588 reference clock
 * to the system time. */
#define FM_AAK_API_PLATFORM_SET_REF_CLOCK           "api.platform.setRefClock"
#define FM_AAT_API_PLATFORM_SET_REF_CLOCK           FM_API_ATTR_BOOL
#define FM_AAD_API_PLATFORM_SET_REF_CLOCK           FALSE

/* Specifies whether the best-effort management is configured to
 * guarantee management performance under full-load configurations. */
#define FM_AAK_API_FM6000_ENABLE_BEM_PERF_TUNING    "api.FM6000.enableBemPerfTuning"
#define FM_AAT_API_FM6000_ENABLE_BEM_PERF_TUNING    FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_ENABLE_BEM_PERF_TUNING    TRUE

/* This API attribute forces the CM_RXMP_MAP to a value of 12 for
 * each ISL_PRI. This attribute disables some CM features sur as 
 * PAUSE, private RXMP WM and showing per RXMP memory usage */
#define FM_AAK_API_FM6000_OVERRIDE_RXMP             "api.FM6000.overrideRxmp"
#define FM_AAT_API_FM6000_OVERRIDE_RXMP             FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_OVERRIDE_RXMP             FALSE

/* This API attribute selects which action to take if the loaded EEPROM
 * has an incompatibility issue with the API. There are three type of
 * actions: FM6000_EEPROM_MISMATCH_FAIL(0), FM6000_EEPROM_MISMATCH_WARN(1) or
 * FM6000_EEPROM_MISMATCH_NONE(2) */
#define FM_AAK_API_FM6000_EEPROM_MISMATCH_ACTION    "api.FM6000.eepromMismatchAction"
#define FM_AAT_API_FM6000_EEPROM_MISMATCH_ACTION    FM_API_ATTR_INT
#define FM_AAD_API_FM6000_EEPROM_MISMATCH_ACTION    0

/* Specifies whether to initialize voltages using nominal values of switch VDD
 * and VDDS. */
#define FM_AAK_API_FM6000_USE_NOMINAL_VOLTAGES      "api.FM6000.useNominalVoltages"
#define FM_AAT_API_FM6000_USE_NOMINAL_VOLTAGES      FM_API_ATTR_BOOL
#define FM_AAD_API_FM6000_USE_NOMINAL_VOLTAGES      TRUE

/* Specifies the name of the network interface that should be used by
 * the white model to send and receive packets through the CPU port.
 * The default is send/receive through a white model bypass. */
#define FM_AAK_API_PLATFORM_MODEL_PKT_INTERFACE     "api.platform.model.pktInterface"
#define FM_AAT_API_PLATFORM_MODEL_PKT_INTERFACE     FM_API_ATTR_TEXT
#define FM_AAD_API_PLATFORM_MODEL_PKT_INTERFACE     "model-bypass"

/* Specifies the hostname and port number of the model server.
 * This will be used to access the model register via socket
 * instead of direct function calls. */
#define FM_AAK_API_PLATFORM_MODEL_SERVER            "api.platform.model.server"
#define FM_AAT_API_PLATFORM_MODEL_SERVER             FM_API_ATTR_TEXT
#define FM_AAD_API_PLATFORM_MODEL_SERVER             ""


/************************************************************************
 ****                                                                ****
 ****              END UNDOCUMENTED API ATTRIBUTES                   ****
 ****                                                                ****
 ************************************************************************/
 
fm_status fmInitializeApiAttributes(void);

fm_status fmSetApiAttribute(fm_text        key,
                            fm_apiAttrType attrType,
                            void *         value);

fm_status fmGetApiAttribute(fm_text        key,
                            fm_apiAttrType attrType,
                            void *         value);


/* helpers to shortcut getting attributes of a given type */
fm_int fmGetIntApiAttribute(fm_text key, fm_int defaultValue);
fm_bool fmGetBoolApiAttribute(fm_text key, fm_bool defaultValue);
fm_float fmGetFloatApiAttribute(fm_text key, fm_float defaultValue);
fm_text fmGetTextApiAttribute(fm_text key, fm_text defaultValue);

void fmDbgDumpApiAttributes(void);


#endif /* __FM_FM_ATTR_H */
