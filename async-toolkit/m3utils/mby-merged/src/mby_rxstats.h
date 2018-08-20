// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_RXSTATS_H
#define MBY_RXSTATS_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"

// Defines:

/******** SAF_BASE *******/
#define MBY_SAF_BASE                                            (0x3EB0000)
#define MBY_SAF_SIZE                                            (0x0000100)

#define MBY_SAF_MATRIX_WIDTH                                    2
#define MBY_SAF_MATRIX_ENTRIES                                  24
#define MBY_SAF_MATRIX(index, word)                             ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0000800) + (MBY_SAF_BASE))

#define MBY_SAF_MATRIX_l_ENABLE_SNF                             3
#define MBY_SAF_MATRIX_h_ENABLE_SNF                             26
#define MBY_SAF_MATRIX_l_CUT_THRU_MODE                          1
#define MBY_SAF_MATRIX_h_CUT_THRU_MODE                          2
#define MBY_SAF_MATRIX_b_IGNORE_FRAME_ERROR                     0

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

typedef enum
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

} hlp_modelRxStatsBk3;

// Structs:

typedef struct mbyPolicerToRxStatsStruct
{
    fm_uint32               RX_LENGTH;          // RX packet length
    fm_uint32               RX_PORT;            // RX port number
    fm_bool                 IS_IPV4;            // packet is of type IP v4
    fm_bool                 IS_IPV6;            // packet is of type IP v6
    fm_macaddr              L2_DMAC;            // layer 2 destination address
    fm_uint16               L2_IVLAN1_CNT_INDEX;
    fm_uint64               FNMASK;             // forwarding normal mask
    fm_bool                 SEG_META_ERR;       // segment error
    fm_bool                 allowStateChange;   // allow 
    fm_uint32               ACTION;             // resolved action
    fm_byte                 TC;                 // 3-bit traffic class

} mbyPolicerToRxStats;

typedef struct mbyRxStatsToModifier
{
    fm_bool                 SAF_ERROR;         // SAF error

} mbyRxStatsToModifier;

#endif

