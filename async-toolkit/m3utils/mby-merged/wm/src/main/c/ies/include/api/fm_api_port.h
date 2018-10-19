/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_port.h
 * Creation Date:   May 16, 2005
 * Description:     Contains functions dealing with the state of individual
 *                  ports
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

#ifndef __FM_FM_API_PORT_H
#define __FM_FM_API_PORT_H


/**************************************************
 * Flag value for the mac argument of
 * ''fmSetPortAttributeV2'' and 
 * ''fmGetPortAttributeV2''.
 **************************************************/

#define FM_PORT_ACTIVE_MAC                      -1
#define FM_PORT_MAC_ALL                         -2 /* Apply to all macs */
#define FM_PORT_LANE_NA                         -1
#define FM_PORT_LANE_ALL                        -2 /* Apply to all lanes */



/****************************************************************************/
/** \ingroup constPortState
 *
 *  Port States (and modes), used as arguments to ''fmSetPortState'' ,
 *  ''fmSetPortStateV2'', ''fmGetPortState'' and ''fmGetPortStateV2'', as 
 *  well as ''fmSetStackLogicalPortState''.
 ****************************************************************************/
/* NOTE! Order is important.  See the table in fmSetPortState(). */
enum _fm_portState
{
    /** The port is in working order (symbol lock, alignment done) and
     *  ready to receive a frame. */
    FM_PORT_STATE_UP = 0,

    /** The port is administratively set down with the SERDES
     *  transmitting an idle pattern but the receiver disabled.
     *                                                                  \lb\lb
     *  Note: on the FM2000, an errata in the silicon dictates that the RX MAC
     *  must never be disabled, so on the FM2000, a port in this state will
     *  still receive ingressing frames. */
    FM_PORT_STATE_ADMIN_DOWN,

    /** The port is administratively set down with the SERDES
     *  shut down and no signal transmitted. */
    FM_PORT_STATE_ADMIN_PWRDOWN,

    /** The receiver is expecting a Built In Self Test sequence. The subMode
     *  argument to ''fmSetPortState'' or ''fmSetPortStateV2'' is used to 
     *  specify the test pattern (see ''Port State Submodes''). */
    FM_PORT_STATE_BIST,

    /** The SERDES transmits remote fault constantly and the receiver is
     *  disabled.*/
    FM_PORT_STATE_REMOTE_FAULT,

    /** Indicates the absence of any signal on any lane. This state is
     *  "get-only" and cannot be specified in a call to ''fmSetPortState''
     *  or ''fmSetPortStateV2''.*/
    FM_PORT_STATE_DOWN,

    /** Some lanes have either signal or partial synchronization. This state is
     *  "get-only" and cannot be specified in a call to ''fmSetPortState''
     *  or ''fmSetPortStateV2''. */
    FM_PORT_STATE_PARTIALLY_UP,

    /** Indicates continuous reception of a local fault condition, a remote
     *  fault is automatically sent continuously.  This state is
     *  "get-only" and cannot be specified in a call to ''fmSetPortState''
     *  or ''fmSetPortStateV2''.*/
    FM_PORT_STATE_LOCAL_FAULT,

    /** Indicates that DFE tuning is still in progress.  This state is
     *  "get-only" and cannot be specified in a call to ''fmSetPortState''
     *  or ''fmSetPortStateV2''.*/
    FM_PORT_STATE_DFE_TUNING

};


/****************************************************************************/
/** \ingroup constPortSubmode
 *
 *  Port State Submodes for the ''FM_PORT_STATE_BIST'' (Built In Self Test) 
 *  port state, used as arguments to ''fmSetPortState'', ''fmSetPortStateV2'', 
 *  ''fmGetPortState'' and ''fmGetPortStateV2''.
 ****************************************************************************/
enum _fm_portSubmode
{
    /** Psuedo-Random Binary Sequence with polynomial coffecients, 9, 4 and 1,
     *  repeating every 511 cycles. 
     *
     *  \chips  FM2000, FM3000, FM4000 */
    FM_BIST_TX_PRBS_512A = 0,

    /** Psuedo-Random Binary Sequence with polynomial coffecients, 9, 5 and 1,
     *  repeating every 511 cycles. 
     *
     *  \chips  FM2000, FM3000, FM4000, FM6000 */
    FM_BIST_TX_PRBS_512B,

    /** Psuedo-Random Binary Sequence with polynomial coffecients, 10, 3 and 1,
     *  repeating every 1023 cycles. 
     *
     *  \chips  FM2000, FM3000, FM4000 */
    FM_BIST_TX_PRBS_1024,

    /** Generate K28.5 IDLE character for 8B/10B encoding. 
    *
    *  \chips  FM2000, FM3000, FM4000, FM6000 */
    FM_BIST_TX_IDLECHAR,

    /** Generate K28.7 low frequency test data, 0001111100b. 
    *
    *  \chips  FM2000, FM3000, FM4000, FM6000 */
    FM_BIST_TX_TESTCHAR,

    /** Generate K28.7 low frequency test data, 0001111100b. 
    *
    *  \chips  FM3000, FM4000, FM6000 */
    FM_BIST_TX_LOWFREQ,

    /** Generate K28.7 high frequency test data, 0101010101b. 
    *
    *  \chips  FM3000, FM4000, FM6000 */
    FM_BIST_TX_HIGHFREQ,

    /** Generate CJPAT frames.
    *
    *  \chips  FM3000, FM4000, FM6000 */
    FM_BIST_TX_CJPAT,

    /** Psuedo-Random Binary Sequence with polynomial coffecients,
     *  7, 6 and 1, repeating every 127 cycles (ITU V.29)
     *
     *  \chips  FM6000 */
    FM_BIST_TX_PRBS_128,

    /** Psuedo-Random Binary Sequence with polynomial coffecients,
     *  15, 14 and 1, repeating every 32K-1 cycles (ITU O.151)
     *
     *  \chips  FM6000 */
    FM_BIST_TX_PRBS_32K,

    /** Psuedo-Random Binary Sequence with polynomial coffecients,
     *  23, 18 and 1, repeating every 8M-1 cycles (ITU O.151)
     *
     *  \chips  FM6000 */
    FM_BIST_TX_PRBS_8M,

    /** Psuedo-Random Binary Sequence with polynomial coffecients,
     *  31, 28 and 1, repeating every 2G-1 cycles 
     *
     *  \chips  FM6000 */
    FM_BIST_TX_PRBS_2G,

    /** Psuedo-Random Binary Sequence with polynomial coffecients,
     *  11, 9 and 1, repeating every 2047 cycles (IEEE Std
     *  802.3ap-2007)
     *
     *  \chips  FM6000 */
    FM_BIST_TX_PRBS_2048,

    /** 10-bit custom test pattern
     *
     *  \chips  FM6000 */
    FM_BIST_TX_CUSTOM10,

    /** 20-bit pattern as a 10-bit custom pattern followed by its inverse
     *
     *  \chips  FM6000 */
    FM_BIST_TX_CUSTOM20,

    /** 40-bit custom test pattern
     *
     *  \chips  FM6000 */
    FM_BIST_TX_CUSTOM40


};


/****************************************************************************/
/** \ingroup constLaneStatus
 *
 * Port lane status, as reported by ''fmGetPortState'' and ''fmGetPortStateV2''.
 ****************************************************************************/
enum _fm_laneStatus
{
    /** Lane is active and does not have symbol lock. 
    *
    *  \chips  FM2000, FM3000, FM4000, FM6000 */
    FM_PORT_LANE_NOT_LOCKED = 0,

    /** Lane is active and has symbol lock. 
    *
    *  \chips  FM2000, FM3000, FM4000, FM6000 */
    FM_PORT_LANE_LOCKED,

    /** Lane is not active. 
    *
    *  \chips  FM2000, FM3000, FM4000, FM6000 */
    FM_PORT_LANE_UNUSED

};


/****************************************************************************/
/** Port Capabilities
 *  \ingroup constPortCapabilities
 *  \page portCapabilities
 *
 *  The following set of bit masks may be ORed together to identify the
 *  capabilities of a physical port, as returned by the platform API service,
 *  ''fmPlatformGetPortCapabilities''.
 ****************************************************************************/
/** \ingroup constPortCapabilities
 * @{ */

/** Port can be a member of a ling aggregation group. 
 *
 *  \chips  FM2000, FM3000, FM4000, FM6000 */
#define FM_PORT_CAPABILITY_LAG_CAPABLE  (1 << 0)

/** Traffic ingressing on this port can be routed. Note that the
 *  ''FM_PORT_ROUTABLE'' port attribute must be set. 
 *
 *  \chips  FM3000, FM4000, FM6000 */
#define FM_PORT_CAPABILITY_CAN_ROUTE    (1 << 1)

/** Port speed can be set to 10M bits per second using the ''FM_PORT_SPEED''
 *  port attribute. 
 *
 *  \chips  FM2000, FM3000, FM4000, FM6000 */
#define FM_PORT_CAPABILITY_SPEED_10M    (1 << 2)

/** Port speed can be set to 100M bits per second using the ''FM_PORT_SPEED''
 *  port attribute. 
 *
 *  \chips  FM2000, FM3000, FM4000, FM6000 */
#define FM_PORT_CAPABILITY_SPEED_100M   (1 << 3)

/** Port speed can be set to 1G bits per second using the ''FM_PORT_SPEED''
 *  port attribute. 
 *
 *  \chips  FM2000, FM3000, FM4000, FM6000 */
#define FM_PORT_CAPABILITY_SPEED_1G     (1 << 4)

/** Port speed can be set to 2.5G bits per second using the ''FM_PORT_SPEED''
 *  port attribute. 
 *
 *  \chips  FM2000, FM3000, FM4000, FM6000 */
#define FM_PORT_CAPABILITY_SPEED_2PT5G  (1 << 5)

/** Port speed can be set to 5G bits per second using the ''FM_PORT_SPEED''
 *  port attribute. 
 *
 *  \chips  FM2000, FM3000, FM4000, FM6000 */
#define FM_PORT_CAPABILITY_SPEED_5G     (1 << 6)

/** Port speed can be set to 10G bits per second using the ''FM_PORT_SPEED''
 *  port attribute. 
 *
 *  \chips  FM2000, FM3000, FM4000, FM6000 */
#define FM_PORT_CAPABILITY_SPEED_10G    (1 << 7)

/** Port speed can be set to 20G bits per second using the ''FM_PORT_SPEED''
 *  port attribute. 
 *
 *  \chips  FM6000 */
#define FM_PORT_CAPABILITY_SPEED_20G    (1 << 8)

/** Port speed can be set to 40G bits per second using the ''FM_PORT_SPEED''
 *  port attribute. 
 *
 *  \chips  FM6000 */
#define FM_PORT_CAPABILITY_SPEED_40G    (1 << 9)

/** @} (end of Doxygen group) */


/****************************************************************************/
/** Transceiver Signals 
 *  \ingroup constXcvrSignals
 *  \page xcveSignals
 *  
 *  The following definitions represent bitmasks that can be OR-ed
 *  together to notifiy the API of a change of one or more transceiver
 *  signals
 *  *************************************************************************/
/** \ingroup constXcvrSignals
 * @{ */

/** Transceiver module present or not
 *
 *  \chips  FM6000 */
#define FM_PORT_XCVRSIG_MODPRES    (1UL << 0) 

/** Transceiver Loss Of Signal state
 *
 *  \chips  FM6000 */
#define FM_PORT_XCVRSIG_RXLOS      (1UL << 1) 

/** Transmitter fault port attribute.
 *
 *  \chips  FM6000 */
#define FM_PORT_XCVRSIG_TXFAULT    (1UL << 2)

/** @} (end of Doxygen group) */


/****************************************************************************/
/** Dfe Options 
 *  \ingroup constDfeOptions
 *  \page dfeOptions
 *  
 *  The following definitions represent bitmasks that can be OR-ed
 *  together to control some specific dfe options
 *  *************************************************************************/
/** \ingroup constDfeOptions
 * @{ */

/** Stop performing DFE fine tuning, it is only applicable to
 *  ports that have already completed dfe tuning. This flag is
 *  automatically reset when DFE tuning is stopped
 *
 *  \chips  FM6000 */
#define FM_PORT_PAUSE_DFE_FINE_TUNING    (1UL << 0) 

/** Stop reading port eye score, it is only applicable if the
 *  port has completed dfe tuning, this flag is automatically
 *  reset when DFE tuning is stopped
 *
 *  \chips  FM6000 */
#define FM_PORT_STOP_READING_EYE_SCORE  (1UL << 1) 

/** @} (end of Doxygen group) */



/****************************************************************************/
/** \ingroup typeEnum
 *
 *  Indicates the type of port identifier used in an ''fm_portIdentifier'' 
 *  structure.
 ****************************************************************************/
typedef enum
{
    /** Port Number. */
    FM_PORT_IDENTIFIER_PORT_NUMBER = 0,

    /** Port Mask. */
    FM_PORT_IDENTIFIER_PORT_MASK

} fm_portIdentifierType;


/****************************************************************************/
/** Port Identifier
 * \ingroup typeStruct
 *
 *  Structure to identify a port or port mask. Used as an argument to
 *  ''fmSetMirrorDestinationExt'' and ''fmGetMirrorDestinationExt''.
 ****************************************************************************/
typedef struct _fm_portIdentifier
{
    /** Identifies which type of port descriptor is being used. */
    fm_portIdentifierType identifierType;

    /** Port Number */
    fm_int                port;

    /** Port Mask */
    fm_bitArray           portMask;

} fm_portIdentifier;




/* state change functions */
fm_status fmSetPortState(fm_int sw, fm_int port, fm_int mode, fm_int submode);
fm_status fmSetPortStateV2(fm_int sw, fm_int port, fm_int mac, fm_int mode, fm_int submode);
fm_status fmGetPortState(fm_int  sw,
                         fm_int  port,
                         fm_int *mode,
                         fm_int *state,
                         fm_int *info);
fm_status fmGetPortStateV2(fm_int  sw,
                           fm_int  port,
                           fm_int  mac,
                           fm_int *mode,
                           fm_int *state,
                           fm_int *info);
fm_status fmNotifyXcvrChange( fm_int     sw, 
                              fm_int     port, 
                              fm_int     mac, 
                              fm_int     lane, 
                              fm_uint32  xcvrSignals, 
                              void      *xcvrInfo );

/* attribute change functions */
fm_status fmSetPortAttribute(fm_int sw, fm_int port, fm_int attr, void *value);
fm_status fmGetPortAttribute(fm_int sw, fm_int port, fm_int attr, void *value);
fm_status fmSetPortAttributeV2(fm_int sw,
                               fm_int port,
                               fm_int mac,
                               fm_int lane,
                               fm_int attr,
                               void * value);
fm_status fmGetPortAttributeV2(fm_int sw,
                               fm_int port,
                               fm_int mac,
                               fm_int lane,
                               fm_int attr,
                               void * value);
fm_status fmSetPortSecurity(fm_int  sw,
                            fm_int  port,
                            fm_bool enable,
                            fm_bool strict);

fm_status fmGetCpuPort(fm_int sw, fm_int *cpuPort);
fm_status fmSetCpuPort(fm_int sw, fm_int cpuPort);

fm_bool fmIsPerLagPortAttribute(fm_int sw, fm_uint attr);

fm_int fmGetISLTagSize(fm_islTagFormat islTagFormat);
fm_int fmGetPortISLTagSize(fm_int sw, fm_int port);

void fmDbgDumpPortAttributes(fm_int sw, fm_int port);

fm_status fmMapCardinalPort(fm_int   sw,
                            fm_int   portIndex,
                            fm_int * logicalPort,
                            fm_int * physicalPort);

fm_status fmGetCardinalPortList(fm_int   sw,
                                fm_int * numPorts,
                                fm_int * portList,
                                fm_int   maxPorts);

#endif /* __FM_FM_API_PORT_H */
