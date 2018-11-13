// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_RXSTATS_H
#define MBY_RXSTATS_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"

// Defines:

#define MBY_STATS_IN_REGS     mby_ppe_rx_stats_map * const stats_map
#define MBY_STATS_IN_REGS_P                                stats_map

/******** RX_STATS_BASE *******/
#define MBY_RX_STATS_BASE                                       (0x3F18000)
#define MBY_RX_STATS_SIZE                                       (0x0020000)

#define MBY_RX_STATS_BANK_FRAME_WIDTH                           2
#define MBY_RX_STATS_BANK_FRAME_ENTRIES_0                       384
#define MBY_RX_STATS_BANK_FRAME_ENTRIES_1                       4
#define MBY_RX_STATS_BANK_FRAME(index1, index0, word)           ((0x0001000) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0000000) + (MBY_RX_STATS_BASE))

#define MBY_RX_STATS_BANK_FRAME_l_FRAME_COUNTER                 0
#define MBY_RX_STATS_BANK_FRAME_h_FRAME_COUNTER                 47

#define MBY_RX_STATS_BANK_BYTE_WIDTH                            2
#define MBY_RX_STATS_BANK_BYTE_ENTRIES_0                        384
#define MBY_RX_STATS_BANK_BYTE_ENTRIES_1                        4
#define MBY_RX_STATS_BANK_BYTE(index1, index0, word)            ((0x0001000) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0004000) + (MBY_RX_STATS_BASE))

#define MBY_RX_STATS_BANK_BYTE_l_BYTE_COUNTER                   0
#define MBY_RX_STATS_BANK_BYTE_h_BYTE_COUNTER                   55

#define MBY_RX_STATS_VLAN_FRAME_WIDTH                           2
#define MBY_RX_STATS_VLAN_FRAME_ENTRIES                         16384
#define MBY_RX_STATS_VLAN_FRAME(index, word)                    ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0008000) + (MBY_RX_STATS_BASE))

#define MBY_RX_STATS_VLAN_FRAME_l_FRAME_COUNTER                 0
#define MBY_RX_STATS_VLAN_FRAME_h_FRAME_COUNTER                 35

#define MBY_RX_STATS_VLAN_BYTE_WIDTH                            2
#define MBY_RX_STATS_VLAN_BYTE_ENTRIES                          16384
#define MBY_RX_STATS_VLAN_BYTE(index, word)                     ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0028000) + (MBY_RX_STATS_BASE))

#define MBY_RX_STATS_VLAN_BYTE_l_BYTE_COUNTER                   0
#define MBY_RX_STATS_VLAN_BYTE_h_BYTE_COUNTER                   43

#define MBY_SEGMENT_LEN 256

// Enums:

typedef enum mbyRxStatsBk0Enum
{
    STAT_RxUcstPktsNonIP = 0,
    STAT_RxMcstPktsNonIP,
    STAT_RxBcstPktsNonIP,
    STAT_RxUcstPktsIPv4,
    STAT_RxMcstPktsIPv4,
    STAT_RxBcstPktsIPv4,
    STAT_RxUcstPktsIPv6,
    STAT_RxMcstPktsIPv6,
    STAT_RxBcstPktsIPv6

} mbyRxStatsBk0;

typedef enum mbyRxStatsBk1Enum
{
    STAT_RxTC0 = 0,
    STAT_RxTC1,
    STAT_RxTC2,
    STAT_RxTC3,
    STAT_RxTC4,
    STAT_RxTC5,
    STAT_RxTC6,
    STAT_RxTC7

} mbyRxStatsBk1;

typedef enum mbyRxStatsBk2Enum
{
    STAT_FIDForwarded = 0,
    STAT_FloodForwarded,
    STAT_TargetedDeterministicForwarded,
    STAT_ParseErrDrops,
    STAT_ParityErrorDrops,
    STAT_Trapped,
    STAT_CtrlDrops,
    STAT_STPDrops,
    STAT_SecurityViolations,
    STAT_MarkerErrorDrops,
    STAT_VlanIngressDrops,
    STAT_VlanEgressDrops,
    STAT_GlortMissDrops,
    STAT_FFUDrops,
    STAT_TriggerDrops,
    STAT_L3PayloadLengthValidationDrops

} mbyRxStatsBk2;

typedef enum mbyRxStatsBk3Enum
{
    STAT_PolicerDrops = 0,
    STAT_TTLDrops,
    STAT_CMGlobalDrops,
    STAT_SMP0Drops,
    STAT_SMP1Drops,
    STAT_RXHog0Drops,
    STAT_RXHog1Drops,
    STAT_TXHog0Drops,
    STAT_TXHog1Drops,
    STAT_FrameErrorDrops,
    STAT_TriggerRedirects,
    STAT_FloodControlDrops,
    STAT_GlortForwarded,
    STAT_OtherDrops,
    STAT_LoopbackSuppDrops,
    STAT_L4CheckSumValidationDrops

} mbyRxStatsBk3;

// Structs:

typedef struct mbyRxStatsToRxOutStruct
{
    // pass-thru:
    fm_bool                 DROP_TTL;
    fm_byte                 ECN;           ///< ECN value to use in egress packet
    fm_uint16               EDGLORT;       ///< egress destination glort
    fm_uint32               FNMASK;        ///< forwarding normal mask
    fm_bool                 IS_TIMEOUT;
    fm_macaddr              L2_DMAC;       ///< L2 destination MAC address
    fm_uint16               L2_EVID1;      ///< 12-bit egress VLAN ID
    fm_bool                 MARK_ROUTED;
    mbyMirrorType           MIRTYP;        ///< mirror type
    fm_uint32               MOD_IDX;       ///< index into the MODIFY descriptor tables
    fm_byte                 MOD_PROF_IDX;  ///< modify profile index
    fm_bool                 NO_MODIFY;     ///< skip most of modifications in Modifier
    fm_bool                 OOM;           ///< out of memory
    mbyParserInfo           PARSER_INFO;   ///< parser info structure
    mbyParserHdrPtrs        PA_HDR_PTRS;   ///< parser header pointers
    fm_bool                 PM_ERR;        ///< ECC error on PM
    fm_bool                 PM_ERR_NONSOP;
    fm_byte                 QOS_L3_DSCP;   ///< 6-bit QOS Differentiated Services Code Point (DSCP)
    fm_byte               * RX_DATA;       ///< ingress (receive) packet data
    fm_uint32               RX_LENGTH;     ///< ingress packet data length [bytes]
    fm_bool                 SAF_ERROR;     ///< SAF error
    fm_uint64               TAIL_CSUM_LEN; ///< L4 CSUM related information
    fm_bool                 TX_DROP;       ///< flag indicating packet drop
    fm_byte                 TX_TAG;        ///< egress tag
    fm_byte                 XCAST;

} mbyRxStatsToRxOut;

#endif
