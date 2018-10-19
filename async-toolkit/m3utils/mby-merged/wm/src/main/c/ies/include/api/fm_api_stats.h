/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_stats.h
 * Creation Date:   June 7, 2005
 * Description:     Structures and functions for dealing with counters
 *                  (statistics)
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

#ifndef __FM_FM_API_STATS_H
#define __FM_FM_API_STATS_H

/**************************************************/
/** Port Statistics Versions
 *  \ingroup constPortStatVersions
 *  \page portsStatVersions
 *
 *  These bit masks represent sets of port statistics,
 *  as reported in a ''fm_portCounters'' structure
 *  returned by ''fmGetPortCounters''. See 
 *  ''fm_portCounters'' for more details.
 **************************************************/
/** \ingroup constPortStatVersions
 * @{ */

/** Indicates that all FM2000 counters in ''fm_portCounters'' are valid. 
 *
 *  \chips  FM2000 */
#define FM2000_STATS_VERSION       (1 << 0)

/** Indicates that all IP statistics counters in ''fm_portCounters'' are valid. 
 *
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_VALID_IP_STATS_VERSION  (1 << 1)

/** Indicates that all FM3000 and FM4000 counters in ''fm_portCounters'' 
 *  are valid. 
 *
 *  \chips  FM3000, FM4000 */
#define FM4000_STATS_VERSION       (1 << 2)

/** Indicates that all FM6000 counters in ''fm_portCounters'' are valid. 
 *
 *  \chips  FM6000 */
#define FM6000_STATS_VERSION       (1 << 3)

/** @} (end of Doxygen group) */


/*****************************************************************************/
/** \ingroup typeStruct typeStructUcode
 *  Per-port statistics returned by ''fmGetPortCounters''.
 *                                                                      \lb\lb
 *  Not all fields in an instance of this structure should always be
 *  considered valid. Fields only valid in certain versions will have a
 *  "Versions: X" comment, where "X" is the name of a version bit mask
 *  (see ''Port Statistics Versions'') that must be set in the cntVersion 
 *  member of this structure for the field to be considered valid. If a field's 
 *  documentation does not indicate any particular version, then the field is 
 *  always valid. If cntVersion does not have the required version bit set 
 *  then any field that requires that version will be set equal to zero.
 *****************************************************************************/
typedef struct _fm_portCounters
{
    /** Identifies the port statistics structure version number. */
    fm_uint64 cntVersion;

    /** Number of valid packets received with unicast L2 DMACs.  */
    fm_uint64 cntRxUcstPkts;

    /** Versions: ''FM_VALID_IP_STATS_VERSION''. Number of valid non-IP 
     *  packets received with unicast L2 DMACS. */
    fm_uint64 cntRxUcstPktsNonIP;

    /** Versions: ''FM_VALID_IP_STATS_VERSION''. Number of valid IPv4 packets 
     *  received with unicast L2 DMACS. */
    fm_uint64 cntRxUcstPktsIPv4;

    /** Versions: ''FM_VALID_IP_STATS_VERSION''. Number of valid IPv6 packets 
     *  received with unicast L2 DMACS. */
    fm_uint64 cntRxUcstPktsIPv6;

    /** Number of valid packets received with broadcast L2 DMACs. */
    fm_uint64 cntRxBcstPkts;

    /** Versions: ''FM_VALID_IP_STATS_VERSION''. Number of valid non-IP 
     *  packets received with broadcast L2 DMACS. */
    fm_uint64 cntRxBcstPktsNonIP;

    /** Versions: ''FM_VALID_IP_STATS_VERSION''. Number of valid IPv4 packets 
     *  received with broadcast L2 DMACS. */
    fm_uint64 cntRxBcstPktsIPv4;

    /** Versions: ''FM_VALID_IP_STATS_VERSION''. Number of valid IPv6 packets 
     *  received with broadcast L2 DMACS. */
    fm_uint64 cntRxBcstPktsIPv6;

    /** Number of valid packets received with multicast L2 DMACs. */
    fm_uint64 cntRxMcstPkts;

    /** Versions: ''FM_VALID_IP_STATS_VERSION''. Number of valid non-IP 
     *  packets received with multicast L2 DMACS. */
    fm_uint64 cntRxMcstPktsNonIP;

    /** Versions: ''FM_VALID_IP_STATS_VERSION''. Number of valid IPv4 packets 
     *  received with multicast L2 DMACS. */
    fm_uint64 cntRxMcstPktsIPv4;

    /** Versions: ''FM_VALID_IP_STATS_VERSION''. Number of valid IPv6 packets 
     *  received with multicast L2 DMACS. */
    fm_uint64 cntRxMcstPktsIPv6;

    /** Versions: ''FM6000_STATS_VERSION'', ''FM_VALID_IP_STATS_VERSION''. 
     * Number of valid non-IP octets received with unicast L2 DMACS. */
    fm_uint64 cntRxUcstOctetsNonIP;

    /** Versions: ''FM6000_STATS_VERSION'', ''FM_VALID_IP_STATS_VERSION''. 
     * Number of valid IPv4 octets received with unicast L2 DMACS. */
    fm_uint64 cntRxUcstOctetsIPv4;

    /** Versions: ''FM6000_STATS_VERSION'', ''FM_VALID_IP_STATS_VERSION''. 
     * Number of valid IPv6 octets received with unicast L2 DMACS. */
    fm_uint64 cntRxUcstOctetsIPv6;

    /** Versions: ''FM6000_STATS_VERSION'', ''FM_VALID_IP_STATS_VERSION''. 
     * Number of valid non-IP octets received with broadcast L2 DMACS. */
    fm_uint64 cntRxBcstOctetsNonIP;

    /** Versions: ''FM6000_STATS_VERSION'', ''FM_VALID_IP_STATS_VERSION''. 
     * Number of valid IPv4 octets received with broadcast L2 DMACS. */
    fm_uint64 cntRxBcstOctetsIPv4;

    /** Versions: ''FM6000_STATS_VERSION'', ''FM_VALID_IP_STATS_VERSION''. 
     * Number of valid IPv6 octets received with broadcast L2 DMACS. */
    fm_uint64 cntRxBcstOctetsIPv6;

    /** Versions: ''FM6000_STATS_VERSION'', ''FM_VALID_IP_STATS_VERSION''. 
     * Number of valid non-IP octets received with multicast L2 DMACS. */
    fm_uint64 cntRxMcstOctetsNonIP;

    /** Versions: ''FM6000_STATS_VERSION'', ''FM_VALID_IP_STATS_VERSION''. 
     * Number of valid IPv4 octets received with multicast L2 DMACS. */
    fm_uint64 cntRxMcstOctetsIPv4;

    /** Versions: ''FM6000_STATS_VERSION''. ''FM_VALID_IP_STATS_VERSION''. 
     * Number of valid IPv6 octets received with multicast L2 DMACS. */
    fm_uint64 cntRxMcstOctetsIPv6;

    /** Number of received valid IEEE 802.3 PAUSE frames. */
    fm_uint64 cntRxPausePkts;

    /** Versions: ''FM6000_STATS_VERSION''.
     * Number of received valid IEEE 802.3 PAUSE octets. */
    fm_uint64 cntRxPauseOctets;

    /** Versions: ''FM6000_STATS_VERSION'', ''FM4000_STATS_VERSION''.
     *  Number of class-based pause packets received. 
     *                                                                  \lb\lb
     *  Note: On FM6000 devices, ''FM_PORT_PARSE_PAUSE'' and 
     *  ''FM_PORT_PARSE_CBP_PAUSE'' must be enabled for this counter to 
     *  work. */
    fm_uint64 cntRxCBPausePkts;

    /** Versions: ''FM6000_STATS_VERSION''.
     *  Number of class-based pause octets received. 
     *                                                                  \lb\lb
     *  Note: ''FM_PORT_PARSE_PAUSE'' and ''FM_PORT_PARSE_CBP_PAUSE'' must 
     *  be enabled for this counter to work. */
    fm_uint64 cntRxCBPauseOctets;

    /** Number of received packets with CRC error, but proper size. */
    fm_uint64 cntRxFCSErrors;

    /** Versions: ''FM6000_STATS_VERSION''. Number of received octets with
     *  CRC error, but proper size. */
    fm_uint64 cntRxFCSErrorsOctets;

    /** Versions: ''FM2000_STATS_VERSION'', ''FM4000_STATS_VERSION''.
     *  Number of received packets with symbol error, but proper size. */
    fm_uint64 cntRxSymbolErrors;

    /** Versions: ''FM4000_STATS_VERSION''. Number of received packets that 
     *  are undersized or oversized. */
    fm_uint64 cntRxFrameSizeErrors;

    /** Number of received valid packets containing 63 or fewer octets
     *  because the minimum frame size is configured to be less than
     *  the Ethernet minimum. */
    fm_uint64 cntRxMinTo63Pkts;

    /** Number of received valid packets containing 64 octets. */
    fm_uint64 cntRx64Pkts;

    /** Number of received valid packets containing 65 to 127 octets. */
    fm_uint64 cntRx65to127Pkts;

    /** Number of received valid packets containing 128 to 255 octets. */
    fm_uint64 cntRx128to255Pkts;

    /** Number of received valid packets containing 256 to 511 octets. */
    fm_uint64 cntRx256to511Pkts;

    /** Number of received valid packets containing 512 to 1023 octets. */
    fm_uint64 cntRx512to1023Pkts;

    /** Number of received valid packets containing 1024 to 1522 octets. */
    fm_uint64 cntRx1024to1522Pkts;

    /** Number of received valid packets containing 1523 to 2047 octets. */
    fm_uint64 cntRx1523to2047Pkts;

    /** Number of received valid packets containing 2048 to 4095 octets. */
    fm_uint64 cntRx2048to4095Pkts;

    /** Number of received valid packets containing 4096 to 8191 octets. */
    fm_uint64 cntRx4096to8191Pkts;

    /** Number of received valid packets containing 8192 to 10239 octets. */
    fm_uint64 cntRx8192to10239Pkts;

    /** Number of received valid packets containing 10240 or more octets
     *  because the maximum frame size is configured to be more than
     *  10240. */
    fm_uint64 cntRx10240toMaxPkts;

    /** Number of received packets smaller than the configured minimum size
     *  with either a CRC or alignment error. For FM6000, this counter is
     *  32 bits*/
    fm_uint64 cntRxFragmentPkts;

    /** Number of received packets smaller than the configured minimum size,
     *  but with a valid CRC. For FM6000, this counter is 32 bits */
    fm_uint64 cntRxUndersizedPkts;

    /** Number of received packets larger than the configured maximum size
     *  with either a CRC or alignment error. For FM2000 and FM4000 
     *  this counter is 16 bits. For FM6000 this counter is 32 bits. */
    fm_uint64 cntRxJabberPkts;

    /** Versions:  ''FM6000_STATS_VERSION''.  Number of received packets 
     *  with any type of framing error (symbol, disparity, etc.). */
    fm_uint64 cntRxFramingErrorPkts;

    /** Versions:  ''FM6000_STATS_VERSION''.  Number of received octets 
     *  with any type of framing error (symbol, disparity, etc.). */
    fm_uint64 cntRxFramingErrorOctets;

    /** Versions:  ''FM6000_STATS_VERSION''.  Number of transmitted packets 
     *  with any type of framing error (symbol, disparity, etc.). */
    fm_uint64 cntTxFramingErrorPkts;

    /** Versions:  ''FM6000_STATS_VERSION''.  Number of transmitted octets 
     *  with any type of framing error (symbol, disparity, etc.). */
    fm_uint64 cntTxFramingErrorOctets;

    /** Versions:  ''FM6000_STATS_VERSION''.  Number of /E/ symbols decoded.
     * Disparity Errors will cause /E/ symbols to be generated. This counter 
     * is 32 bits. */
    fm_uint64 cntCodeErrors;

    /** Number of received packets larger than the configured maximum size.
     *  For FM2000 and FM4000, this applies to packets well-formed or with
     *  either a CRC or alignment error. For FM6000, this applies to packets
     *  that are well-formed. For FM6000 devices, this counter is 32 bits. */
    fm_uint64 cntRxOversizedPkts;

    /** Number of received octets in valid packets. */
    fm_uint64 cntRxGoodOctets;

    /** Versions: ''FM_VALID_IP_STATS_VERSION''. Number of received octets in 
     *  valid Non-IP packets. */
    fm_uint64 cntRxOctetsNonIp;

    /** Versions: ''FM_VALID_IP_STATS_VERSION''. Number of received octets in 
     *  valid IPv4 packets. */
    fm_uint64 cntRxOctetsIPv4;

    /** Versions: ''FM_VALID_IP_STATS_VERSION''. Number of received octets in 
     *  valid IPv6 packets. */
    fm_uint64 cntRxOctetsIPv6;

    /** Versions: ''FM2000_STATS_VERSION'', ''FM4000_STATS_VERSION''.
     *  Number of received octets in bad packets. */
    fm_uint64 cntRxBadOctets;

    /** An array, indexed by frame priority, the entry being the number of
     *  packets recevied at that priority.
     *                                                                  \lb\lb
     *  FM2000, FM3000 and FM4000 devices support only the first 8 priorities 
     *  (0..7) while FM6000 devices support all 16 priorities (0..15). */
    fm_uint64 cntRxPriorityPkts[16];

    /** Versions: ''FM6000_STATS_VERSION''. For frames that are
     *  either invalid on ingress or that get invalidated by the
     *  switch because of a performance issue, the priority is
     *  counted as invalid as this value may not be derived from the
     *  frame (the priority value can be erroneous). */
    fm_uint64 cntRxInvalidPriorityPkts;

    /** An array, indexed by frame priority, the entry being the number of
     *  octets recevied in frames at that priority.
     *                                                                  \lb\lb
     *  FM2000, FM3000 and FM4000 devices support only the first 8 priorities 
     *  (0..7) while FM6000 devices support all 16 priorities (0..15). */
    fm_uint64 cntRxPriorityOctets[16];

    /** Versions: ''FM6000_STATS_VERSION''. For frames that are
     *  either invalid on ingress or that get invalidated by the
     *  switch because of a performance issue, the priority is
     *  counted as invalid as this value may not be derived from the
     *  frame (the priority value can be erroneous). */
    fm_uint64 cntRxInvalidPriorityOctets;

    /** Versions: ''FM6000_STATS_VERSION''.
     * An array, indexed by priority (0..15), each element containing the
     *  number of transmitted octets in frames at that priority. */
    fm_uint64 cntTxPriorityOctets[16];

    /** Number of transmitted valid unicast packets .*/
    fm_uint64 cntTxUcstPkts;

    /** Number of transmitted valid broadcast packets. */
    fm_uint64 cntTxBcstPkts;

    /** Number of transmitted valid multicast packets. */
    fm_uint64 cntTxMcstPkts;

    /** Versions: ''FM6000_STATS_VERSION''.
     *  Number of transmitted valid non-IP unicast packets .*/
    fm_uint64 cntTxUcstPktsNonIP;

    /** Versions: ''FM6000_STATS_VERSION''.
     *  Number of transmitted valid broadcast packets. */
    fm_uint64 cntTxBcstPktsNonIP;

    /** Versions: ''FM6000_STATS_VERSION''.
     *  Number of transmitted valid non-IP multicast packets. */
    fm_uint64 cntTxMcstPktsNonIP;

    /** Versions: ''FM6000_STATS_VERSION''.
     *  Number of transmitted valid IP unicast packets .*/
    fm_uint64 cntTxUcstPktsIP;

    /** Versions: ''FM6000_STATS_VERSION''.
     *  Number of transmitted valid IP broadcast packets. */
    fm_uint64 cntTxBcstPktsIP;

    /** Versions: ''FM6000_STATS_VERSION''.
     *  Number of transmitted valid IP multicast packets. */
    fm_uint64 cntTxMcstPktsIP;

    /** Number of transmitted valid PAUSE frames. For FM2000, FM3000 and
     *  FM4000, this counter is 32 bits. */
    fm_uint64 cntTxPausePkts;

    /** Version: ''FM6000_STATS_VERSION''.  Number of transmitted valid 
     *  PAUSE octets. */
    fm_uint64 cntTxPauseOctets;

    /** Number of transmitted packets with CRC error, but proper size.
     *  For FM2000, FM3000 and FM4000, this counter is 32 bits. */
    fm_uint64 cntTxFCSErroredPkts;

    /** Versions: ''FM6000_STATS_VERSION''.
     *  Number of transmitted octets with CRC error, but proper size. */
    fm_uint64 cntTxFCSErroredOctets;

    /** Versions: ''FM2000_STATS_VERSION'', ''FM4000_STATS_VERSION''.
     *  Number of transmitted packets that were marked on ingress as
     *  erroneous (either due to an CRC or symbol error, or due to under/over
     *  size problems) which the switch element actually managed to discard.
     *  Frames marked as erroneous on ingress which were transmitted
     * (due to cutthrough) will not be included in this counter. */
    fm_uint64 cntTxErrorDropPkts;

    /** Number of frames dropped due to the frame timeout mechanism */
    fm_uint64 cntTxTimeOutPkts;

    /** Versions: ''FM6000_STATS_VERSION''. Number of octets dropped due to 
     *  the frame timeout mechanism. */
    fm_uint64 cntTxTimeOutOctets;

    /** Versions: ''FM6000_STATS_VERSION''. Number of frames that were 
     *  discarded due to no memory in TX. */
    fm_uint64 cntTxOutOfMemErrPkts;

    /** Versions: ''FM6000_STATS_VERSION''. Number of octets that were 
     *  discarded due to no memory in TX. */
    fm_uint64 cntTxOutOfMemErrOctets;

    /** Versions: ''FM6000_STATS_VERSION''. Number of frames that were 
     *  discarded due to unrepairable ECC erors in TX. */
    fm_uint64 cntTxUnrepairEccPkts;

    /** Versions: ''FM6000_STATS_VERSION''. Number of octets that were 
     *  discarded due to unrepairable ECC erors in TX. */
    fm_uint64 cntTxUnrepairEccOctets;

    /** Versions: ''FM6000_STATS_VERSION'', ''FM4000_STATS_VERSION''. 
     *  Number of frames that were discarded due to loopback suppression. */
    fm_uint64 cntTxLoopbackPkts;

    /** Versions: ''FM6000_STATS_VERSION''.
     *  Number of octets that were discarded due to loopback suppression. */
    fm_uint64 cntTxLoopbackOctets;

    /** Versions: ''FM6000_STATS_VERSION''.
     * Number of transmitted valid non-IP unicast packets .*/
    fm_uint64 cntTxUcstOctetsNonIP;

    /** Versions: ''FM6000_STATS_VERSION''.
     * Number of transmitted valid non-IP broadcast packets. */
    fm_uint64 cntTxBcstOctetsNonIP;

    /** Versions: ''FM6000_STATS_VERSION''.
     * Number of transmitted valid non-IP multicast packets. */
    fm_uint64 cntTxMcstOctetsNonIP;

    /** Versions: ''FM6000_STATS_VERSION''.
     * Number of transmitted valid IPv4 unicast packets .*/
    fm_uint64 cntTxUcstOctetsIP;

    /** Versions: ''FM6000_STATS_VERSION''.
     * Number of transmitted valid IPv4 broadcast packets. */
    fm_uint64 cntTxBcstOctetsIP;

    /** Versions: ''FM6000_STATS_VERSION''.
     * Number of transmitted valid IPv4 multicast packets. */
    fm_uint64 cntTxMcstOctetsIP;

    /** Number of transmitted valid packets containing 63 or fewer octets
     *  because the minimum frame size is configured to be less than
     *  the Ethernet minimum. This counter also includes errored frames
     *  that were transmitted anyway because MAC_CFG_2[Min Frame Discard]
     *  (see Tahoe datasheet) was not set. */
    fm_uint64 cntTxMinTo63Pkts;

    /** Number of transmitted valid packets containing 64 octets. */
    fm_uint64 cntTx64Pkts;

    /** Number of transmitted valid packets containing 65 to 127 octets. */
    fm_uint64 cntTx65to127Pkts;

    /** Number of transmitted valid packets containing 128 to 255 octets. */
    fm_uint64 cntTx128to255Pkts;

    /** Number of transmitted valid packets containing 256 to 511 octets. */
    fm_uint64 cntTx256to511Pkts;

    /** Number of transmitted valid packets containing 512 to 1023 octets. */
    fm_uint64 cntTx512to1023Pkts;

    /** Number of transmitted valid packets containing 1024 to 1522 octets. */
    fm_uint64 cntTx1024to1522Pkts;

    /** Number of transmitted valid packets containing 1523 to 2047 octets. */
    fm_uint64 cntTx1523to2047Pkts;

    /** Number of transmitted valid packets containing 2048 to 4095 octets. */
    fm_uint64 cntTx2048to4095Pkts;

    /** Number of transmitted valid packets containing 4096 to 8191 octets. */
    fm_uint64 cntTx4096to8191Pkts;

    /** Number of transmitted valid packets containing 8192 to 10239 octets. */
    fm_uint64 cntTx8192to10239Pkts;

    /** Number of transmitted valid packets containing 10240 or more octets
     *  because the maximum frame size is configured to be more than
     *  10240. */
    fm_uint64 cntTx10240toMaxPkts;

    /** Number of transmitted octets, including CRCs but excluding preambles
     *  and inter-frame characters. */
    fm_uint64 cntTxOctets;

    /** Versions: ''FM4000_STATS_VERSION''. Total octet count of frames 
     *  scheduled for transmission but were dropped due to frame timeout, 
     *  min/max length bound errors, invalid ingress FCS, or phy-level 
     *  errors. */
    fm_uint64 cntTxErrorOctets;

    /** Versions: ''FM2000_STATS_VERSION'', ''FM4000_STATS_VERSION'',
     *  ''FM6000_STATS_VERSION''. Number of transmitted packets dropped
     *  for congestion management. */
    fm_uint64 cntTxCMDropPkts;

    /** Number of frames that were forwarded normally, either unicast or
     *  multicast, as a result of a lookup of a valid entry in the MAC
     *  address table, or a broadcast. Note: This counter does not count
     *  mirrored frames. */
    fm_uint64 cntFIDForwardedPkts;

    /** Number of valid frames that were flooded either because it was a
     *  unicast packet with an unknown destination or an unregistered
     *  multicast packet. */
    fm_uint64 cntFloodForwardedPkts;

    /** Versions: ''FM6000_STATS_VERSION''.  Number of valid frames that were 
     *  switched based on the destination glort. */
    fm_uint64 cntGlortSwitchedPkts;

    /** Versions: ''FM6000_STATS_VERSION''.  Number of valid frames that were 
     *  routed based on the destination glort. */
    fm_uint64 cntGlortRoutedPkts;

    /** Versions: ''FM4000_STATS_VERSION''. Number of frames processed with 
     *  FTYPE==0x2 (Special Delivery). All standard filtering, forwarding, 
     *  and lookup rules are bypassed in this case. */
    fm_uint64 cntSpeciallyHandledPkts;

    /** Versions: ''FM6000_STATS_VERSION'', ''FM4000_STATS_VERSION''.
     *  Number of frames dropped due to header parse errors. */
    fm_uint64 cntParseErrDropPkts;

    /** Versions: ''FM6000_STATS_VERSION'', ''FM4000_STATS_VERSION''.
     *  Number of frames dropped due to memory parity errors encountered in
     *  the Frame Processing pipeline. */
    fm_uint64 cntParityErrorPkts;

    /** Versions: ''FM6000_STATS_VERSION'', ''FM4000_STATS_VERSION''.
     *  Number of frames trapped to the CPU for any reason not covered by
     *  other counters in this group (e.g. Security violations). These include
     *  frames with reserved IEEE multicast addresses (BPDU, 802.1X, LACP, etc.),
     *  trapped IP frames (e.g. ICMP), and programmably trapped frames (due to
     *  the FFU, triggers, etc.). */
    fm_uint64 cntTrappedPkts;

    /** Versions: ''FM4000_STATS_VERSION''. Number of MAC Control frames 
     *  dropped (either standard IEEE pause frames or class-based pause 
     *  frames). */
    fm_uint64 cntPauseDropPkts;

    /** Number of frames that were dropped on ingress because either the
     *  ingress or egress port was not in the forwarding spanning tree state. */
    fm_uint64 cntSTPDropPkts;

    /** Versions: ''FM6000_STATS_VERSION''.
     *  Number of frames that were dropped on ingress because the
     *  ingress port was not in the forwarding spanning tree state. */
    fm_uint64 cntSTPIngressDropsPkts;

    /** Versions: ''FM6000_STATS_VERSION''.
     *  Number of frames that were dropped on egress because the
     *  egress port was not in the forwarding spanning tree state. */
    fm_uint64 cntSTPEgressDropsPkts;

    /** Versions: ''FM2000_STATS_VERSION''. Number of frames that were 
     *  trapped to the CPU and not forwarded normally, as a result of any of 
     *  the three specific trap functions:
     *                                                                  \lb\lb
     *  (1) Destination address = IEEE reserved group address
     *                                                                  \lb\lb
     *  (2) Destination address = CPU MAC address
     *                                                                  \lb\lb
     *  (3) Ether-type = Ether-type trap. */
    fm_uint64 cntReservedTrapPkts;

    /** Number of frames that were dropped or trapped because they were
     *  considered a security violation. */
    fm_uint64 cntSecurityViolationPkts;

    /** Number of frames dropped because the frame was untagged and
     *  the switch is configured to drop untagged frames, or the frame
     *  was tagged and the switch is configured to drop tagged frames. */
    fm_uint64 cntVLANTagDropPkts;

    /** Number of frames dropped for an ingress VLAN boundary violation.
     *  Note: This only applies to 802.1Q; in a port-based VLAN there is no
     *  such thing as an ingress violation. */
    fm_uint64 cntVLANIngressBVPkts;

    /** Number of frames dropped for an egress VLAN boundary violation. */
    fm_uint64 cntVLANEgressBVPkts;

    /** Version: ''FM6000_STATS_VERSION''. Number of frames dropped for 
     *  L2AR loopback suppression. */
    fm_uint64 cntLoopbackDropsPkts;

    /** Versions: ''FM4000_STATS_VERSION''. Number of frames dropped due to 
     *  not finding a matching entry in the GLORT_CAM. */
    fm_uint64 cntGlortMissDropPkts;

    /** Versions: ''FM6000_STATS_VERSION'', ''FM4000_STATS_VERSION''.
     *  Number of frames dropped due to an FFU action. */
    fm_uint64 cntFFUDropPkts;

    /** Versions: ''FM6000_STATS_VERSION''. Number of frames
     *  dropped because the frame was either invalid when parsed
     *  or invalid because the frame processing pipeline was
     *  overloaded and marked this frame as invalid. */
    fm_uint64 cntInvalidDropPkts;

    /** Versions: ''FM6000_STATS_VERSION'', ''FM4000_STATS_VERSION''.
     *  Number of frames dropped due to policer rate limitation. */
    fm_uint64 cntPolicerDropPkts;

    /** Versions: ''FM6000_STATS_VERSION'', ''FM4000_STATS_VERSION''.
     *  Number of IP frames dropped due to TTL less than or equal to 1. */
    fm_uint64 cntTTLDropPkts;

    /** Versions: ''FM6000_STATS_VERSION''. Number of frames dropped due
     *  to the global usage exceeding the global watermark. */
    fm_uint64 cntGlobalWMDropPkts;

    /** Versions: ''FM6000_STATS_VERSION''. Number of frames dropped due
     *  to the RX memory partition usage exceeding the RX memory partition
     *  watermark. */
    fm_uint64 cntRXMPDropPkts;

    /** Versions: ''FM6000_STATS_VERSION''. Number of frames dropped due
     *  to the the RX memory partition's hog watermark exceeded on this port. */
    fm_uint64 cntRxHogDropPkts;

    /** Versions: ''FM6000_STATS_VERSION''. Number of frames dropped due
     *  to the TX memory partition's hog watermarks exceeded on all ports that
     *  the frame was forwarded on. */
    fm_uint64 cntTxHogDropPkts;

    /** Versions: ''FM6000_STATS_VERSION''. Number of frames not counted
     *  by any other group 6 statistic. */
    fm_uint64 cntOtherPkts;

    /** Versions: ''FM6000_STATS_VERSION''. Number of frames dropped 
     *  because of flood control rules. */
    fm_uint64 cntFloodControlDropPkts;

    /** Versions: ''FM4000_STATS_VERSION''. (Congestion Management) Number of 
     *  frames dropped due to the global watermark (CM_GLOBAL_WM) being 
     *  exceeded. */
    fm_uint64 cntCmPrivDropPkts;

    /** Versions: ''FM4000_STATS_VERSION''. (Congestion Management) Number of 
     *  frames dropped due to insufficient memory in shared partition 0. */
    fm_uint64 cntSmp0DropPkts;

    /** Versions: ''FM4000_STATS_VERSION''. (Congestion Management) Number of 
     *  frames dropped due to insufficient memory in shared partition 1. */
    fm_uint64 cntSmp1DropPkts;

    /** Versions: ''FM4000_STATS_VERSION''. (Congestion Management) Number of 
     *  frames dropped due to the SMP 0 RX hog watermark. */
    fm_uint64 cntRxHog0DropPkts;

    /** Versions: ''FM4000_STATS_VERSION''. (Congestion Management) Number of 
     *  frames dropped due to the SMP 1 RX hog watermark. */
    fm_uint64 cntRxHog1DropPkts;

    /** Versions: ''FM4000_STATS_VERSION''. (Congestion Management) Number of 
     *  frames dropped to all egress ports due to the SMP 0 TX hog 
     *  watermark. */
    fm_uint64 cntTxHog0DropPkts;

    /** Versions: ''FM4000_STATS_VERSION''. (Congestion Management) Number of 
     *  frames dropped to all egress ports due to the SMP 1 TX hog 
     *  watermark. */
    fm_uint64 cntTxHog1DropPkts;

    /** Versions: ''FM4000_STATS_VERSION''. (Congestion Management) Number of 
     *  frames dropped due to ingress rate limiting on SMP 0. */
    fm_uint64 cntRateLimit0DropPkts;

    /** Versions: ''FM4000_STATS_VERSION''. (Congestion Management) Number of 
     *  frames dropped due to ingress rate limiting on SMP 1. */
    fm_uint64 cntRateLimit1DropPkts;

    /** Versions: ''FM4000_STATS_VERSION''. (Congestion Management) Number of 
     *  frames dropped due to illegal membership. */
    fm_uint64 cntBadSmpDropPkts;

    /** Versions: ''FM2000_STATS_VERSION'', ''FM4000_STATS_VERSION''.
     *  Number of frames dropped or redirected due to a user-defined trigger. */
    fm_uint64 cntTriggerDropRedirPkts;

    /** Versions: ''FM4000_STATS_VERSION''. Number of frames dropped due to 
     *  a trigger drop action. */
    fm_uint64 cntTriggerDropPkts;

    /** Versions: ''FM4000_STATS_VERSION''. Number of frames redirected due 
     *  to a trigger action. */
    fm_uint64 cntTriggerRedirPkts;

    /** Versions: ''FM2000_STATS_VERSION''. Number of valid frames that were 
     *  mirrored. Note: This counter is only incremented if flooding is 
     *  enabled in the switch. */
    fm_uint64 cntTriggerMirroredPkts;

    /** Versions: ''FM2000_STATS_VERSION''. Number of frames with a broadcast 
     *  destination address dropped because storm control was enabled. */
    fm_uint64 cntBroadcastDropPkts;

    /** Versions: ''FM2000_STATS_VERSION''. Number of unicast and multicast 
     *  frames dropped due to a destination lookup failure when flooding is 
     *  disabled. */
    fm_uint64 cntDLFDropPkts;

    /** Versions: ''FM2000_STATS_VERSION''. Number of received packets dropped 
     *  for exceeding the RX shared watermark. */
    fm_uint64 cntRxCMDropPkts;

    /** Number of frames that were terminated early or dropped due to
     *  underflow during transmission. For FM6000, this counter is 32 bits. */
    fm_uint64 cntUnderrunPkts;

    /** Number of frames that overflowed the receiver and were dropped.
     *  For FM6000, this counter is 32 bits. */
    fm_uint64 cntOverrunPkts;

    /** Versions: ''FM2000_STATS_VERSION'', ''FM4000_STATS_VERSION''.
     *  Number of frames that were corrupted within the switch. When the frame
     *  had a correct CRC on RX but not on TX this counter is incremented. */
    fm_uint64 cntCorruptedPkts;

    /** Versions: ''FM4000_STATS_VERSION''.
     *  Number of counter updates to counter groups 1 through 6 (RX counters)
     *  that were missed due to insufficient counter bandwidth. */
    fm_uint64 cntStatsDropCountTx;

    /** Versions: ''FM4000_STATS_VERSION''. Number of counter updates to 
     *  counter groups 7 through 9 (TX counters) that were missed due to 
     *  insufficient counter bandwidth. */
    fm_uint64 cntStatsDropCountRx;

    /** Versions: ''FM6000_STATS_VERSION''.  Number of frames dropped due
     *  to an ingress FCS error that was dropped by the scheduler prior
     *  to transmission start. */
    fm_uint64 cntTxFCSErrDropPkts;

    /** Versions: ''FM6000_STATS_VERSION''.  Number of octets dropped due
     *  to an ingress FCS error that was dropped by the scheduler prior
     *  to transmission start. */
    fm_uint64 cntTxFCSErrDropOctets;

    /** Versions: ''FM6000_STATS_VERSION''.  Number of mirrored packets
     * transmitted on this port. */ 
    fm_uint64 cntTxMirrorPkts;

    /** Versions: ''FM6000_STATS_VERSION''.  Number of mirrored
     * octets transmitted on this port */ 
    fm_uint64 cntTxMirrorOctets;
    
    /** Versions: ''FM6000_STATS_VERSION''. Time at which the counter read
     *  started in microseconds. May be used on two consecutive reads to
     *  determine packet, octet or error rates. */
    fm_uint64 timestamp;
    
} fm_portCounters;


/**************************************************/
/** \ingroup typeStruct
 * Per VLAN statistics
 **************************************************/
typedef struct _fm_vlanCounters
{
    /** Identifies the VLAN statistics structure version number. */
    fm_uint64 cntVersion;

    /** Versions: ''FM2000_STATS_VERSION'', ''FM4000_STATS_VERSION''.
     * Number of octets received on VLAN in unicast frames. */
    fm_uint64 cntUcstOctets;

    /** Versions: ''FM2000_STATS_VERSION'', ''FM4000_STATS_VERSION''.
     * Number of octets received on VLAN in multiicast and broadcast frames. */
    fm_uint64 cntXcstOctets;

    /** Versions: ''FM2000_STATS_VERSION'', ''FM4000_STATS_VERSION''.
     * Number of received unicast frames on VLAN. */
    fm_uint64 cntUcstPkts;

    /** Versions: ''FM2000_STATS_VERSION'', ''FM4000_STATS_VERSION''.
     * Number of received multicast and broadcast frames on VLAN. */
    fm_uint64 cntXcstPkts;

    /** Versions: ''FM6000_STATS_VERSION''. 
     * Number of received unicast frames on VLAN. */
    fm_uint64 cntRxUcstPkts;

    /** Versions: ''FM6000_STATS_VERSION''. 
     * Number of received multicast frames on VLAN. */
    fm_uint64 cntRxMcstPkts;

    /** Versions: ''FM6000_STATS_VERSION''. 
     * Number of received broadicast frames on VLAN. */
    fm_uint64 cntRxBcstPkts;

    /** Versions: ''FM6000_STATS_VERSION''. 
     * Number of frames dropped on receive on VLAN. */
    fm_uint64 cntRxDropPkts;

    /** Versions: ''FM6000_STATS_VERSION''. 
     * Number of received unicast octets on VLAN. */
    fm_uint64 cntRxUcstOctets;

    /** Versions: ''FM6000_STATS_VERSION''. 
     * Number of received multicast octets on VLAN. */
    fm_uint64 cntRxMcstOctets;

    /** Versions: ''FM6000_STATS_VERSION''. 
     * Number of received broadicast octets on VLAN. */
    fm_uint64 cntRxBcstOctets;

    /** Versions: ''FM6000_STATS_VERSION''. 
     * Number of octets dropped on receive on VLAN. */
    fm_uint64 cntRxDropOctets;

    /** Versions: ''FM6000_STATS_VERSION''. 
     * Number of transmitted unicast frames on VLAN. */
    fm_uint64 cntTxUcstPkts;

    /** Versions: ''FM6000_STATS_VERSION''. 
     * Number of transmitted multicast frames on VLAN. */
    fm_uint64 cntTxMcstPkts;

    /** Versions: ''FM6000_STATS_VERSION''. 
     * Number of transmitted broadicast frames on VLAN. */
    fm_uint64 cntTxBcstPkts;

    /** Versions: ''FM6000_STATS_VERSION''. 
     * Number of frames dropped on transmit on VLAN. */
    fm_uint64 cntTxDropPkts;

    /** Versions: ''FM6000_STATS_VERSION''. 
     * Number of other unclassified packets. */
    fm_uint64 cntTxOtherPkts;

    /** Versions: ''FM6000_STATS_VERSION''. 
     * Number of transmitted unicast octets on VLAN. */
    fm_uint64 cntTxUcstOctets;

    /** Versions: ''FM6000_STATS_VERSION''. 
     * Number of transmitted multicast octets on VLAN. */
    fm_uint64 cntTxMcstOctets;

    /** Versions: ''FM6000_STATS_VERSION''. 
     * Number of transmitted broadicast octets on VLAN. */
    fm_uint64 cntTxBcstOctets;

    /** Versions: ''FM6000_STATS_VERSION''. 
     * Number of octets dropped on transmit on VLAN. */
    fm_uint64 cntTxDropOctets;

    /** Versions: ''FM6000_STATS_VERSION''. 
     * Number of other unclassified octets. */
    fm_uint64 cntTxOtherOctets;

} fm_vlanCounters;


/**************************************************/
/** \ingroup typeStruct
 * Switch-wide global statistics
 * Not all fields in an instance of this structure should always be
 * considered valid. Fields only valid in certain versions will have a
 * "Versions: X" comment; Where "X" is the name of a version bit identifier
 * that must be set in cntVersion for the field to be considered valid.
 * If a field's documentation indicates no version then the field is always
 * valid.
 * If cntVersion does not have the required version bit set then the field is
 * set equal to 0.
 **************************************************/
typedef struct _fm_switchCounters
{
    /** Identifies the switch statistics structure version number. */
    fm_uint64 cntVersion;

    /** Number of frames dropped for congestion management due to the
     *  global low Priority Weighted Discard (PWD) watermark.
     */
    fm_uint64 cntGlobalLowDropPkts;

    /** Number of frames dropped due to the global high Priority Weighted
     *  Discard (PWD) watermark.
     */
    fm_uint64 cntGlobalHighDropPkts;

    /** Number of frames dropped due to the global privilege watermark.
     */
    fm_uint64 cntGlobalPrivilegeDropPkts;

    /** Number of counter updates to counter groups 1 through 6 (RX counters)
     *  that were missed due to insufficient counter bandwidth.
     *  This a global count. FM3000 and FM4000 devices only support a per port 
     *  stat drop counter. */
    fm_uint64 cntStatsDropCountTx;

    /** Number of counter updates to counter groups 7 through 9 (TX counters)
     *  that were missed due to insufficient counter bandwidth.
     *  This a global count. FM3000 and FM4000 devices only support a per port 
     *  stat drop counter. */
    fm_uint64 cntStatsDropCountRx;

} fm_switchCounters;

fm_status fmGetPortCounters(fm_int sw, fm_int port, fm_portCounters *cnt);
fm_status fmResetPortCounters(fm_int sw, fm_int port);
fm_status fmGetVLANCounters(fm_int sw, fm_int vlan, fm_vlanCounters *cnt);
fm_status fmResetVLANCounters(fm_int sw, fm_int vlan);
fm_status fmAllocateVLANCounters(fm_int sw, fm_int vlan);
fm_status fmFreeVLANCounters(fm_int sw, fm_int vlan);
fm_status fmGetSwitchCounters(fm_int sw, fm_switchCounters *cnt);
fm_status fmResetSwitchCounters(fm_int sw);


#endif /* __FM_FM_API_STATS_H */
