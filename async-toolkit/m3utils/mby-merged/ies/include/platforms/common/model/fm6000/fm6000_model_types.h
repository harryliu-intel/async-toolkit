/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm6000_model_types.h
 * Creation Date:   August 26, 2010
 * Description:     Data structures, definitions and types for the FM6000 model.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2010 - 2012 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM6000_MODEL_TYPES_H
#define __FM6000_MODEL_TYPES_H

#include <platforms/common/model/fm6000/fm6000_model.h>

/**************************************************
 * Macros
 **************************************************/

#define GET_BITS(w, h, l)                                     \
    ( ( ( ( (fm_uint64) FM_LITERAL_64(1) << ( (h) - (l) ) ) << 1 ) - 1 ) & \
     ( ( ( (h) / 64 ) != ( (l) / 64 ) ) ?                     \
      ( ( w[(h) / 64] << ( 64 - ( (l) % 64 ) ) ) |            \
        ( w[(l) / 64] >> ( (l) % 64 ) ) ) :                   \
      ( w[(l) / 64] >> ( (l) % 64 ) ) ) )

#define GET_BIT(w, b) \
    ( ( w[(b) / 64] & ( FM_LITERAL_64(1) << ( (b) % 64 ) ) ) ? 1 : 0 )

#define GET_BIT_FIELD(w, h, l)  ( GET_BITS( (w), (h), (l) ) )

#define TABLE_SIZE(name) (fm_int)(sizeof(name)/sizeof(name[0]))

#define IN_RANGE_1(a,x ) \
    ((a) == (x))

#define IN_RANGE_2(a, x) \
    (((a) >= (x)) && ((a) < ((x) + 2)) && (((a) & 1) == 1))

#define IN_RANGE_3(a, x) \
    (((a) >= (x)) && ((a) < ((x) + 3)) && (((a) & 3) == 2))

#define IN_RANGE_4(a, x) \
    (((a) >= (x)) && ((a) < ((x) + 4)) && (((a) & 3) == 3))

#define IN_RANGE_8(a, x) \
    (((a) >= (x)) && ((a) < ((x) + 8)) && (((a) & 3) == 3))

#define IN_CAM_RANGE(a, x, w) \
    ( (w == 1) ? IN_RANGE_1(a, x) : (w == 2) ? IN_RANGE_2(a, x) \
        : (w == 3) ? IN_RANGE_3(a, x) : (w == 4) ? IN_RANGE_4(a, x) \
        : IN_RANGE_8(a, x) )

#define FM_MODEL_TAKE_LOCK(model)   fmCaptureLock(&model->modelLock, \
                                                  FM_WAIT_FOREVER);

#define FM_MODEL_DROP_LOCK(model)   fmReleaseLock(&model->modelLock);


/**************************************************
 * Renamed constants (avoiding the 6" long names)
 **************************************************/

#define MAX_PORTS                    76
#define MAX_SWITCHES                 100
#define MAX_BST_KEYS                 FM6000_FFU_BST_KEY_ENTRIES_0
#define MAX_BST_PARTITIONS           FM6000_FFU_BST_KEY_ENTRIES_1
#define MAX_BST_SLICES               FM6000_FFU_BST_KEY_ENTRIES_2
#define MAX_BST_SCENARIOS            FM6000_FFU_BST_SCENARIO_CAM_ENTRIES_0
#define MAX_CASES                    4
#define MAX_MIRRORS                  4
#define MAX_SLICE_KEYS               FM6000_FFU_SLICE_CAM_ENTRIES_0
#define MAX_CAM_SLICES               FM6000_FFU_SLICE_CAM_ENTRIES_1
#define MAX_SLICE_SCENARIOS          FM6000_FFU_SLICE_SCENARIO_CAM_ENTRIES_0
#define MAX_HASH_L3_PROFILES         FM6000_HASH_LAYER3_PROFILE_ENTRIES
#define FFU_REMAP_POINT              (MAX_CAM_SLICES>>1)

/* Sweeper rates */
#define SWEEPER_0_PER_PERIOD         1000
#define SWEEPER_1_PER_PERIOD         100

/**************************************************
 * Alta FLAGS usage. This will most likely
 * be auto-gen later on. Meanwhile up-to-date 
 * definitions can be found in router/globals.py 
 **************************************************/

#if 0
/* Define the flags position and lifespan         */
/* --- Bit  0: BEGIN OF PARSER FLAGS -------------------------------- */
/* --- Bit  0: BEGIN OF L3AR FLAGS ---------------------------------- */
/* --- Bit  0: BEGIN OF L2AR FLAGS ---------------------------------- */
/* --- Bit  0: BEGIN OF MOD FLAGS ---------------------------------- */
#define FLAG_FRAME_TYPE0         0       /* PARSER => MODIFY */
#define FLAG_FRAME_TYPE1         1       /* PARSER => MODIFY */
#define FLAG_ISL_TYPE0           2       /* PARSER => MODIFY */
#define FLAG_ISL_TYPE1           3       /* PARSER => MODIFY */
#define FLAG_VLAN1_RX_TAGGED     4       /* PARSER => MODIFY */
#define FLAG_VLAN2_RX_TAGGED     5       /* PARSER => MODIFY */
#define FLAG_VLAN1_CAPTURED      6       /* PARSER => L3AR  */
#define FLAG_NAT_MODE0           6       /* L3AR   => MODIFY */
#define FLAG_VLAN2_CAPTURED      7       /* PARSER => L3AR */
#define FLAG_NAT_MODE1           7       /* L3AR   => MODIFY */
#define FLAG_L3_IP               8       /* PARSER => MODIFY */
#define FLAG_L3_IPV6             9       /* PARSER => MODIFY */
#define FLAG_RX_DGLORT_NON_ZERO  10      /* PARSER => L2AR  */
#define FLAG_EPOCH_TX            10      /* L2AR   => STATS */
#define FLAG_L2_MCST             11      /* PARSER => MODIFY */
#define FLAG_L2_BCST             12      /* PARSER => MODIFY */
#define FLAG_L3_MCST             13      /* PARSER => MODIFY */
#define FLAG_L3_HEAD             14      /* PARSER => L2AR */
#define FLAG_L3_ROUTED           14      /* L3AR   => MODIFY */
#define FLAG_RX_SGLORT_NON_ZERO  15      /* PARSER => L2AR */
#define FLAG_L3_UPDATE_DMAC      15      /* L2AR   => MODIFY */
#define FLAG_V4_IN_V6            16      /* PARSER => L2AR */
#define FLAG_L3_UPDATE_SMAC      16      /* L2AR   => MODIFY */
#define FLAG_TTL_EXPIRED         17      /* PARSER => L2AR  */
#define FLAG_TRILL_RX            18      /* PARSER => MODIFY */
#define FLAG_TRILL_MCST          19      /* PARSER => MODIFY */
#define FLAG_VID1_RX_ZERO        20      /* PARSER => L2AR */
#define FLAG_TRILL_TX            20      /* L2AR   => MODIFY */
#define FLAG_NAT_TCP_HEADER      20      /* L2AR   => MODIFY */
#define FLAG_VID2_RX_ZERO        21      /* PARSER => L2AR */
#define FLAG_TRILL_RELAY         21      /* L2AR   => MODIFY */
#define FLAG_NAT_DEST_ADDRS      21      /* L2AR   => MODIFY */
#define FLAG_TRILL_RX_VLAN       22      /* PARSER => MODIFY */
#define FLAG_CN_TAG_RX           23      /* PARSER => MODIFY */
#define FLAG_L3_UPDATE_DSCP_TTL  23      /* L2AR   => MODIFY */
/* --- Bit 23: END OF MOD FLAGS ------------------------------------- */
/* --- Bit 24: BEGIN OF FORWARD FLAGS ------------------------------- */
#define FLAG_L3_OPTION           24      /* PARSER => L2AR */
#define FLAG_TX_TRUNC            24      /* L2AR   => SCHEDULER */
#define FLAG_ARP                 25      /* PARSER => L2AR */
#define FLAG_L2_VID_SELECT       25      /* L2AR   => SCHEDULER */
#define FLAG_L2_TAG              26      /* L2AR   => SCHEDULER */
#define FLAG_L3_FRAGMENT         27      /* PARSER => L2AR */
#define FLAG_CN_SAMPLE_ELIGIBLE  27      /* L2AR   => SCHEDULER */
#define FLAG_L3_IGMP             28      /* L3AR   => L2AR */
#define FLAG_SNF_OVERRIDE        28      /* L2AR   => SCHEDULER */
#define FLAG_L3_ICMP             29      /* L3AR   => L2AR */
#define FLAG_TX_IGNORE_ERROR     29      /* L2AR   => SCHEDULER */
/* --- Bit 33: END OF FORWARD FLAGS --------------------------------- */
/* --- Bit 34: BEGIN OF TAIL FLAGS ---------------------------------- */
#define FLAG_PAUSE               34      /* PARSER => STATS */
#define FLAG_CLASS_BASED_PAUSE   35      /* PARSER => STATS */
#define FLAG_PARSER_ERROR        36      /* PARSER => STATS */
#define FLAG_CHECKSUM_ERROR      37      /* PARSER => STATS */
#define FLAG_INCOMPLETE_HEADER   38      /* PARSER => STATS */
#define FLAG_PARITY_ERROR        39      /* PARSER => STATS */
#define FLAG_POL1_CREDIT1        36      /* L2AR => POLICER */
#define FLAG_POL1_CREDIT2        37      /* L2AR => POLICER */
#define FLAG_POL3_CREDIT         40      /* L2AR => POLICER */
/* --- Bit 40: END OF PARSER FLAGS ---------------------------------- */
#define FLAG_PORT_ROUTABLE       40      
#define FLAG_MAC_WRITE_BACK      41      /* L2AR   => L2L  */
#define FLAG_MAC_WRITE_NEW       42      /* L2AR   => L2L */
#define FLAG_DMAC_ROUTABLE       42
#define FLAG_VID1_ROUTABLE       43
/* --- Bit 43: END OF TAIL FLAGS ------------------------------------ */
/* --- Bit 44: BEGIN OF STATS FLAGS --------------------------------- */
#define FLAG_STATS_FLOOD         44      /* L2AR   => STATS */
#define FLAG_FFU_SWITCH          45      /* FFU    => L2AR */
#define FLAG_FFU_ROUTE           46      /* FFU    => L2AR */
#define FLAG_STATS_VLAN_ALL      46      /* L2AR   => STATS */
#define FLAG_EVID2_TRANSLATION   47      /* L3AR   => L2AR */
#define FLAG_EPOCH_0             47      /* L2AR   => STATS */
#define FLAG_EPOCH_1             48      /* L2AR   => STATS */
#define FLAG_NEXTHOP_ENTRY_TYPE  49
#define FLAG_NEXTHOP_12AEQVID1   50
#define FLAG_LOOPBACK_SUPPRESS   51      /* L3AR   => L2AR */
/* --- Bit 52: END OF L3AR VISIBLE FLAGS ---------------------------- */ 
#define FLAG_MA2_WRITE_BACK_EN   52      /* L2L    => L2AR */
#define FLAG_COUNT_MAC_UPDATE    52      /* L2AR   => STATS */
#define FLAG_MA2_WRITE_NEW_EN    53      /* L2L    => L2AR */
#define FLAG_COUNT_MAC_NEW       53      /* L2AR   => STATS */
#define FLAG_GLORT_EQUAL         55      /* ALU => L2AR  (ALU1_Zero)  */
#define FLAG_STATS_PAUSE         56      /* L2AR   => STATS */
#define FLAG_MTU_GREATER         60      /* ALU    => L2AR (ALU4_Overflow) */
#define FLAG_MTU_EQUAL           61      /* ALU    => L2AR (ALU4_Zero) */
/* --- Bit 70: END OF L2AR VISIBLE FLAGS ---------------------------- */ 
/* --- Bit 75: END OF STATS FLAGS ----------------------------------- */
#else
/* Define the flags position and lifespan */
/* --- Bit  0: BEGIN OF PARSER FLAGS -------------------------------- */
/* --- Bit  0: BEGIN OF L3AR FLAGS ---------------------------------- */
/* --- Bit  0: BEGIN OF L2AR FLAGS ---------------------------------- */
/* --- Bit  0: BEGIN OF MOD FLAGS ---------------------------------- */
#define FLAG_FRAME_TYPE0         0       /* PARSER => MODIFY */
#define FLAG_FRAME_TYPE1         1       /* PARSER => MODIFY */
#define FLAG_ISL_TYPE0           2       /* PARSER => MODIFY */
#define FLAG_ISL_TYPE1           3       /* PARSER => MODIFY */
#define FLAG_VLAN1_RX_DECODED    4       /* PARSER => MODIFY  Indicate that the VLAN1 has been decoded (from VLAN1 tag or ISL tag) */
#define FLAG_VLAN2_RX_DECODED    5       /* PARSER => MODIFY  Indicate that the VLAN2 has been decoded (from VLAN2 tag) */
#define FLAG_VLAN1_CAPTURED      6       /* PARSER => L3AR   */
#define FLAG_NAT_MODE0           6       /* L3AR   => MODIFY */
#define FLAG_VLAN2_CAPTURED      7       /* PARSER => L3AR   */
#define FLAG_NAT_MODE1           7       /* L3AR   => MODIFY */
#define FLAG_L3_IP               8       /* PARSER => MODIFY */
#define FLAG_L3_IPV6             9       /* PARSER => MODIFY */
#define FLAG_RX_DGLORT_NON_ZERO  10      /* PARSER => L2AR   */
#define FLAG_EPOCH_TX            10      /* L2AR   => STATS  */
#define FLAG_L2_MCST             11      /* PARSER => MODIFY */
#define FLAG_L2_BCST             12      /* PARSER => MODIFY */
#define FLAG_L3_MCST             13      /* PARSER => MODIFY */
#define FLAG_L3_HEAD             14      /* PARSER => L2AR   */
#define FLAG_L3_ROUTED           14      /* L3AR   => MODIFY */
//#define FLAG_RX_SGLORT_NON_ZERO  15      /* PARSER => L2AR   */
#define FLAG_VLAN2_RX_TAGGED     15      /* PARSER => MODIFY  Indicate that a VLAN2 tag is present in the ingress frame regardless if decoded or not */
#define FLAG_V4_IN_V6            16      /* PARSER => L2AR   */
#define FLAG_L3_UPDATE_SMAC      16      /* L2AR   => MODIFY */
#define FLAG_TTL_EXPIRED         17      /* PARSER => L2AR   */
#define FLAG_L3_UPDATE_DMAC      17      /* L2AR   => MODIFY */
#define FLAG_TRILL_RX            18      /* PARSER => MODIFY */
#define FLAG_TRILL_MCST          19      /* PARSER => MODIFY (if FLAG_TRILL_RX is 1) */
#define FLAG_IPTUN_RX            19      /* PARSER => MODIFY (if FLAG_TRILL_RX is 0) */
#define FLAG_VID1_RX_ZERO        20      /* PARSER => L2AR   */
#define FLAG_TRILL_TX            20      /* L2AR   => MODIFY */
#define FLAG_NAT_TCP_HEADER      20      /* L2AR   => MODIFY */
#define FLAG_VID2_RX_ZERO        21      /* PARSER => L2AR   */
#define FLAG_TRILL_RELAY         21      /* L2AR   => MODIFY */
#define FLAG_NAT_DEST_ADDRS      21      /* L2AR   => MODIFY */
#define FLAG_TRILL_RX_VLAN       22      /* PARSER => MODIFY */
#define FLAG_VLAN1_RX_TAGGED     23      /* PARSER => MODIFY  Indicate if the VLAN1 tag is present in the ingress frame regardless if decoded or not */
/* --- Bit 23: END OF MOD FLAGS ------------------------------------- */
/* --- Bit 24: BEGIN OF FORWARD FLAGS ------------------------------- */
#define FLAG_L3_OPTION           24      /* PARSER => L2AR      */
#define FLAG_L2_VID_SELECT       24      /* L2AR   => SCHEDULER */
#define FLAG_CN_TAG_RX           25      /* PARSER => L2AR */
#define FLAG_L3_NO_ARP           25      /* L3AR   => L2AR      */
#define FLAG_L2_TAG              25      /* L2AR   => SCHEDULER */
#define FLAG_ARP                 26      /* PARSER => L2AR      */
#define FLAG_SNF_OVERRIDE        26      /* L2AR   => SCHEDULER */
#define FLAG_L3_FRAGMENT         27      /* PARSER => L2AR      */
#define FLAG_CN_SAMPLE_ELIGIBLE  27      /* L2AR   => SCHEDULER */
#define FLAG_L3_IGMP             28      /* L3AR   => L2AR      */
#define FLAG_TX_TRUNC            28      /* L2AR   => SCHEDULER */
#define FLAG_L3_ICMP             29      /* L3AR   => L2AR      */
#define FLAG_TX_IGNORE_ERROR     29      /* L2AR   => SCHEDULER */
#define FLAG_STRICT_DEST_GLORT   33      /* L3AR   => L2AR      */
/* --- Bit 33: END OF FORWARD FLAGS --------------------------------- */
/* --- Bit 34: BEGIN OF TAIL FLAGS ---------------------------------- */
#define FLAG_PAUSE               34      /* PARSER => STATS */
#define FLAG_CLASS_BASED_PAUSE   35      /* PARSER => STATS */
#define FLAG_PARSER_ERROR        36      /* PARSER => STATS */
#define FLAG_CHECKSUM_ERROR      37      /* PARSER => STATS */
#define FLAG_INCOMPLETE_HEADER   38      /* PARSER => STATS */
#define FLAG_PARITY_ERROR        39      /* PARSER => STATS */
#define FLAG_POL1_CREDIT1        36      /* L2AR => POLICER */
#define FLAG_POL1_CREDIT2        37      /* L2AR => POLICER */
#define FLAG_POL3_CREDIT         40      /* L2AR => POLICER */
/* --- Bit 40: END OF PARSER FLAGS ---------------------------------- */
#define FLAG_PORT_ROUTABLE       40      
#define FLAG_MAC_WRITE_BACK      41      /* L2AR   => L2L */
#define FLAG_MAC_WRITE_NEW       42      /* L2AR   => L2L */
#define FLAG_DMAC_ROUTABLE       42
#define FLAG_VID1_ROUTABLE       43
/* --- Bit 43: END OF TAIL FLAGS ------------------------------------ */
/* --- Bit 44: BEGIN OF STATS FLAGS --------------------------------- */
#define FLAG_STATS_FLOOD         44      /* L2AR   => STATS */
#define FLAG_FFU_SWITCH          45      /* FFU    => L2AR  */
#define FLAG_FFU_ROUTE           46      /* FFU    => L2AR  */
#define FLAG_STATS_VLAN_ALL      46      /* L2AR   => STATS */
#define FLAG_EVID2_TRANSLATION   47      /* L3AR   => L2AR  */
#define FLAG_EPOCH_0             47      /* L2AR   => STATS */
#define FLAG_EPOCH_1             48      /* L2AR   => STATS */
#define FLAG_NEXTHOP_ENTRY_TYPE  49
#define FLAG_NEXTHOP_12AEQVID1   50
#define FLAG_NEXTHOP_12BEQVID1   51
/* --- Bit 52: END OF L3AR VISIBLE FLAGS ---------------------------- */
#define FLAG_MA2_WRITE_BACK_EN   52      /* L2L    => L2AR  */
#define FLAG_COUNT_MAC_UPDATE    52      /* L2AR   => STATS */
#define FLAG_MA2_WRITE_NEW_EN    53      /* L2L    => L2AR  */
#define FLAG_COUNT_MAC_NEW       53      /* L2AR   => STATS */
#define FLAG_GLORT_EQUAL         55      /* ALU => L2AR  (ALU1_Zero) */
#define FLAG_STATS_PAUSE         56      /* L2AR   => STATS */
#define FLAG_MTU_GREATER         60      /* ALU    => L2AR (ALU4_Overflow) */
#define FLAG_MTU_EQUAL           61      /* ALU    => L2AR (ALU4_Zero) */
/* --- Bit 70: END OF L2AR VISIBLE FLAGS ---------------------------- */
/* --- Bit 75: END OF STATS FLAGS ----------------------------------- */

#endif

/* --- Bits not defined in globals.py but needed by WhiteAlta --------*/
#define FLAG_EACL_A              30
#define FLAG_EACL_B              31
#define FLAG_EACL_C              32
#define FLAG_STRICT_DGLORT       33
#define FLAG_SRC_PORT_FLAG1      FLAG_PORT_ROUTABLE
#define FLAG_SRC_PORT_FLAG2      41
#define FLAG_VID2_ROUTABLE       44
#define FLAG_NEXTHOP_12BEQVID2   51
#define FLAG_ALU1_OVERFLOW       54
#define FLAG_ALU1_ZERO           55
#define FLAG_ALU2_OVERFLOW       56
#define FLAG_ALU2_ZERO           57
#define FLAG_ALU3_OVERFLOW       58
#define FLAG_ALU3_ZERO           59
#define FLAG_ALU4_OVERFLOW       60
#define FLAG_ALU4_ZERO           61
#define FLAG_ALU5_OVERFLOW       62
#define FLAG_ALU5_ZERO           63
#define FLAG_ALU6_OVERFLOW       64
#define FLAG_ALU6_ZERO           65


#define fmWhiteModelSetFlag(model, fb, v)                                            \
    {                                                                                \
        (model)->packetState.FLAGS[(fb) / 64] &= ~( FM_LITERAL_64(1) << ( (fb) % 64 ) );          \
        (model)->packetState.FLAGS[(fb) / 64] |= ( ( (v) & FM_LITERAL_64(1) ) << ( (fb) % 64 ) ); \
    }

#define fmWhiteModelGetFlag(model, fb) \
    ( ( (model)->packetState.FLAGS[(fb) / 64] >> ( (fb) % 64 ) ) & FM_LITERAL_64(1) )

/**************************************************
 * Register Structures. These structures
 * mimic the equalivent registers as defined
 * in the specification.
 **************************************************/

typedef struct _fm6000_modelGlortRam
{
    /* Configuration */
    fm_int HashCmd;
    fm_int DMaskBaseIdx;
    fm_int RangeSubIndexA;
    fm_int RangeSubIndexB;
    fm_int DMaskRange;
    fm_int HashRotation;
    fm_int DGlortTag;

    /* Derived */
    fm_int OffsetA;
    fm_int LengthA;
    fm_int OffsetB;
    fm_int LengthB;
    fm_int SelectRange;
    fm_int ShiftRange;

} fm6000_modelGlortRam;

typedef struct _fm6000_modelLagPortTable
{
    fm_int LagSelect;
    fm_int LagShift;
    fm_int Index;
    fm_int HashRotation;

} fm6000_modelLagPortTable;

typedef struct _fm6000_modelL2LMacTableEntry
{
    fm_uint64 MAC;
    fm_int    FID1;
    fm_int    FID2;
    fm_int    Prec;
    fm_int    GLORT;
    fm_int    TAG;
    fm_int    DATA;
    fm_int    Error;
    fm_uint64 key[2];

} fm6000_modelL2LMacTableEntry;

typedef struct _fm6000_modelL2LEvid1Table
{
    fm_int MA1_FID1;
    fm_int MA1_FID2_IVL;
    fm_int ETAG1;
    fm_int ET_IDX;

} fm6000_modelL2LEvid1Table;

typedef struct _fm6000_modelL2LIvid1Table
{
    fm_int MA2_FID1;
    fm_int MA2_FID2_IVL;
    fm_int ITAG1;
    fm_int IT_IDX;

} fm6000_modelL2LIvid1Table;

typedef struct _fm6000_modelL2LEvid2Table
{
    fm_int MA1_FID2;
    fm_int ETAG2;

} fm6000_modelL2LEvid2Table;

typedef struct _fm6000_modelL2LIVid2Table
{
    fm_int MA2_FID2;
    fm_int ITAG2;

} fm6000_modelL2LIVid2Table;

typedef struct _fm6000_modelSweeperCam
{
    fm_uint64 KeyInvert[2];
    fm_uint64 Key[2];

} fm6000_modelSweeperCam;

typedef struct _fm6000_modelSweeperRam
{
    fm_int ReplaceValueGLORT;
    fm_int ReplaceValueDATA;
    fm_int ReplaceValueTAG;
    fm_int ReplaceValuePrec;
    fm_int ReplaceMaskGLORT;
    fm_int ReplaceMaskDATA;
    fm_int ReplaceMaskTAG;
    fm_int ReplaceMaskPrec;
    fm_int DecrementMaskDATA;
    fm_int ReportMatch;

} fm6000_modelSweeperRam;

typedef struct _fm6000_modelSweeperConfig
{
    /* Exposed states */
    fm_uint16 StartIndex;
    fm_uint16 StopIndex;
    fm_bool   Enable;
    fm_bool   SinglePass;
    fm_bool   Blocked;
    fm_uint16 SweeperIndex;
    fm_uint16 SweepCount;

    /* Internal states */
    fm_uint32 tick;
    fm_uint32 period;
    fm_uint32 entriesPerPeriod;

} fm6000_modelSweeperConfig;

typedef struct _fm6000_modelMcastDestTable
{
    fm_uint64 DestMask[2];
    fm_int    MulticastIndex;

} fm6000_modelMcastDestTable;

typedef struct _fm6000_modelMcastMirrorConfig
{
    fm_int    OverlayEnabled;
    fm_int    OverlayDestPort;
    fm_int    OverlayTC;
    fm_int    CanonicalSrcPort;

} fm6000_modelMcastMirrorConfig;

typedef struct _fm6000_modelMcastTxMirrorDest
{
    fm_byte ExplicitDestMask[4];

} fm6000_modelMcastTxMirrorDest;

typedef struct _fm6000_modelCMTxMirrorDest
{
    fm_uint64 ExplicitDestMask[2];
    fm_int    OverlayEnabled;

} fm6000_modelCMTxMirrorDest;

typedef struct _fm6000_modelMcastLoopbackSup
{
    fm_uint16 Glort;
    fm_uint16 Mask;

} fm6000_modelMcastLoopbackSup;

typedef struct _fm6000_modelFFUSliceScenarioCfg
{
    fm_int Byte0Mux;
    fm_int Byte1Mux;
    fm_int Byte2Mux;
    fm_int Byte3Mux;
    fm_int Top4Mux;
    fm_int StartCompare;
    fm_int StartSet;
    fm_int ActionLength;
    fm_int ValidLow;
    fm_int ValidHigh;
    fm_int Kase;

} fm6000_modelFFUSliceScenarioCfg;

typedef struct _fm6000_modelFFUScenarioCam
{
    fm_uint32 KeyInvert;
    fm_uint32 Key;

} fm6000_modelFFUScenarioCam;

typedef struct _fm6000_modelFFUSliceCam
{
    fm_uint64 KeyInvert;
    fm_uint64 Key;

} fm6000_modelFFUSliceCam;

typedef struct _fm6000_modelFFURemapProfile
{
    fm_byte     SelectLabel16;
    fm_byte     SelectLabel8A;
    fm_byte     SelectLabel8B;
    fm_byte     SelectL3HashProfile;
    fm_uint16   ValueHashRotMantissa;
    fm_byte     ValueHashRotExponent;
    fm_byte     SelectHashRotMantissa;
    fm_byte     SelectHashRotExponent; 

} fm6000_modelFFURemapProfile;


typedef struct _fm6000_modelPolicersEntry
{
    fm_byte   Mode;
    fm_byte   Unit;
    fm_uint16 Tag;

} fm6000_modelPolicersEntry;


typedef struct _fm6000_modelFFUEgressACLCam1
{
    fm_uint64 KeyInvert0;
    fm_uint64 Key0;
    fm_uint64 KeyInvert1;
    fm_uint64 Key1;

} fm6000_modelFFUEgressACLCam1;

typedef struct _fm6000_modelFFUEgressACLCam2
{
    fm_uint32 KeyInvert;
    fm_uint32 Key;

} fm6000_modelFFUEgressACLCam2;

typedef struct _fm6000_modelFFUEgressACLAction
{
    fm_int Permit;
    fm_int Count;
    fm_int ActionA;
    fm_int ActionB;
    fm_int ActionC;
    fm_int ActionExt;

} fm6000_modelFFUEgressACLAction;

typedef struct _fm6000_modelBSTScenarioCfg1
{
    fm_int Byte0Mux;
    fm_int Byte1Mux;
    fm_int Byte2Mux;
    fm_int Byte3Mux;
    fm_int Top4Mux;

} fm6000_modelBSTScenarioCfg1;


typedef struct _fm6000_modelBSTScenarioCfg2
{
    fm_int StartCompare;
    fm_int StartSet;
    fm_int ActionLength;
    fm_int RootKeyValid;
    fm_int NybbleMask;

} fm6000_modelBSTScenarioCfg2;


typedef struct _fm6000_modelBSTRootKeys
{
    fm_uint32 Key;
    fm_int    Top4Key;
    fm_int    Top4KeyInvert;
    fm_int    Partition;

} fm6000_modelBSTRootKeys;


typedef struct _fm6000_modelFFULHashL3Profile
{
    fm_uint16   l3DipByteMask;
    fm_uint16   l3SipByteMask;
    fm_byte     l3DSBitMask;
    fm_uint32   l3FlowBitMask;
    fm_byte     ISLUserBitMask;
    fm_byte     l3ProtBitMask;
    fm_uint16   l4SrcBitMask;
    fm_uint16   l4DstBitMask;
    fm_byte     field16ByteMask;
    fm_bool     symmetrizeL3;
    fm_bool     symmetrizeL4;
    fm_bool     usePTable;
    fm_bool     computeHash;
    fm_bool     randomizeNextHop;
    fm_bool     randomizeOther;

} fm6000_modelFFULHashL3Profile;

typedef struct _fm6000_modelStatsLengthBin
{
    fm_uint32 lowerBound;
    fm_uint32 bin;

} fm6000_modelStatsLengthBin;

typedef struct _fm6000_modelLBSCam
{
    fm_uint32 key;
    fm_uint32 keyInvert;

} fm6000_modelLBSCam;

/***************************************************
 * The following state structure holds the
 * pipeline state while a packet is being processed.
 * This state is completely reset at the start of
 * processing for a packet.  Any persistent state
 * should be defined in fm6000_model.
 **************************************************/

typedef struct _fm6000_modelState
{
    /* The packet payload */
    fm_byte *             inData;

    /* The ingress length */
    fm_int                inLength;

    /* Tracks the processing time of the packet */
    fm_timestamp          pktStart;
    fm_timestamp          pktRxEnd;
    fm_timestamp          pktTxEnd;

    /* EPL specific state */
    fm_bool               eplDrop;

    /* Maps to the flags channel */
    fm_uint64             FLAGS[2];

    /* Parser state */
    fm_uint32             parserPayloadIndex;
    fm_uint32             parserState;
    fm_uint32             nextParserState;
    fm_uint64             parserHeaderWord;
    fm_uint32             parserChecksum;
    fm_uint32             parserChecksumVerif;
    fm_uint32             parserLegalPadding;
    fm_uint32             parserNumValidBytes;
    fm_uint32             parserTerminate;
    fm_uint32             parserTerminateAllowed[2];
    fm_uint32             parserShift;

    /* Output channels used by the parser */
    fm_uint32             QOS;
    fm_uint32             L4_ENTRY;
    fm_uint32             L3_FLOW;
    fm_uint32             ISL_FTYPE;
    fm_uint32             FIELD[43];

    /* Channels generated by the parser */
    fm_int                SRC_PORT;
    fm_uint64             L3_DIP[2];
    fm_uint64             L3_SIP[2];
    fm_uint64             L3_DIP_Derived;
    fm_uint64             L2_DMAC;
    fm_uint64             L2_SMAC;
    fm_uint32             L2_VID1;
    fm_uint32             L2_VID2;
    fm_uint32             QOS_L2_VPRI1;
    fm_uint32             QOS_L2_VPRI2;
    fm_uint32             QOS_L3_PRI;
    fm_uint32             QOS_ISL_PRI;
    fm_uint32             QOS_W4;
    fm_uint32             ISL_SGLORT;
    fm_uint32             ISL_DGLORT;
    fm_uint32             ISL_USER;
    fm_uint32             L2_TYPE;
    fm_uint32             L4_SRC;
    fm_uint32             L4_DST;
    fm_uint32             L3_TTL;
    fm_uint32             L3_PROT;
    fm_uint32             FIELD16A;
    fm_uint32             FIELD16B;
    fm_uint32             FIELD16C;
    fm_uint32             FIELD16D;
    fm_uint32             FIELD16E;
    fm_uint32             FIELD16F;
    fm_uint32             FIELD16G;
    fm_uint32             FIELD16H;
    fm_uint32             FIELD16I;
    fm_uint32             L3_LENGTH;

    /* Channels generated by the mapper */
    fm_uint32             SCENARIO;
    fm_uint32             L4_SRC_ID;
    fm_uint32             L4_DST_ID;
    fm_uint32             L2_DMAC_Flag;
    fm_uint32             L2_DMAC_ID1;
    fm_uint32             L2_DMAC_ID2;
    fm_uint32             L2_DMAC_ID3;
    fm_uint32             L2_SMAC_ID1;
    fm_uint32             L2_SMAC_ID2;
    fm_uint32             L2_SMAC_ID3;
    fm_uint32             L2_TYPE_ID1;
    fm_uint32             L2_TYPE_ID2;
    fm_uint32             L3_PROT_ID1;
    fm_uint32             L3_PROT_ID2;
    fm_uint32             L3_DIP_ID1;
    fm_uint32             L3_DIP_ID2;
    fm_uint32             L3_SIP_ID1;
    fm_uint32             L3_SIP_ID2;
    fm_uint32             L3_LENGTH_BIN;
    fm_uint32             ISL_PRI_Source;
    fm_uint32             L2_VPRI1_Source;
    fm_uint32             L2_VPRI2_Source;
    fm_uint32             MAP_VID1;
    fm_uint32             MAP_VID2;
    fm_uint32             VID1_TAG;
    fm_uint32             VID2_TAG;
    fm_uint32             SRC_PORT_ID1;
    fm_uint32             SRC_PORT_ID2;
    fm_uint32             SRC_PORT_ID3;
    fm_uint32             SRC_PORT_ID4;
    fm_uint32             SRC_PORT_QOS_TAG;
    fm_uint32             LABEL8A;
    fm_uint32             LABEL8B;
    fm_uint32             LABEL16;
    
    /* FIXME: weird stuff.... */
    fm_uint32             FIELD16I_4_7;
    fm_uint32             FIELD16I_12_15;
    fm_uint32             L3_W4_Source;
    
    /* Channels generated by the FFU */
    fm_uint32             HASH_PROFILE;
    fm_uint32             L3_HASH;
    fm_uint32             L3_HASH_NEXTHOP;
    fm_uint32             L3_HASH_MANTISSA;
    fm_uint32             L3_HASH_EXPONENT;
    fm_uint32             FFU_DATA_W24_TOP;
    fm_uint32             FFU_DATA_W24;
    fm_uint32             FFU_DATA_TAG1A;
    fm_uint32             FFU_DATA_TAG1B;
    fm_uint32             FFU_DATA_TAG2A;
    fm_uint32             FFU_DATA_TAG2B;
    fm_uint32             FFU_DATA_W16A;
    fm_uint32             FFU_DATA_W16A_TOP;
    fm_uint32             FFU_DATA_W16B;
    fm_uint32             FFU_DATA_W16B_TOP;
    fm_uint32             FFU_DATA_W8A;
    fm_uint32             FFU_DATA_W8B;
    fm_uint32             FFU_DATA_W8C;
    fm_uint32             BST_LABEL16A;
    fm_uint32             BST_LABEL16B;
    fm_int                ffuHitIndex;
    fm_bool               ffuHitSet;
    fm_uint32             ffuPrecW24;
    fm_uint32             ffuPrecTag1A;
    fm_uint32             ffuPrecTag1B;
    fm_uint32             ffuPrecTag2A;
    fm_uint32             ffuPrecTag2B;
    fm_uint32             ffuPrecW16A[4];
    fm_uint32             ffuPrecW16B[4];
    fm_uint32             ffuPrecW8A[8];
    fm_uint32             ffuPrecW8B[8];
    fm_uint32             ffuPrecW8C[8];
    fm_int                ffuEaclHitIndex[32];
    fm_uint32             ffuEaclHit;
    fm_uint64             ffuRawHit[32];
    fm_int                ffuActionLength;

    /* Channels generated by NextHop */
    fm_uint32             NEXTHOP_DATA_W16A;
    fm_uint32             NEXTHOP_DATA_W16B;
    fm_uint32             NEXTHOP_DATA_W16C;
    fm_uint32             NEXTHOP_DATA_W12A;
    fm_uint32             NEXTHOP_TAG;
    fm_uint32             NEXTHOP_DATA_W16D;
    fm_uint32             NEXTHOP_DATA_W8A;
    fm_uint32             NEXTHOP_DATA_W8B;
    fm_uint32             NEXTHOP_DATA_W8C;
    fm_uint32             NEXTHOP_DATA_W8B_W8C;
    fm_uint32             NEXTHOP_DATA_W4A;
    fm_uint32             NEXTHOP_DATA_W4B;
    fm_uint32             NEXTHOP_DATA_W4B_NEXTHOP_DATA_W4A;
    fm_uint32             NEXTHOP_DATA_W12B;
    fm_uint32             NEXTHOP_IDX;

    /* Channels generated by L3AR */
    fm_uint32             L2_HASH_A;
    fm_uint32             L2_HASH_B;
    fm_uint32             IVID1;
    fm_uint32             IVID2;
    fm_uint32             EVID1;
    fm_uint32             EVID2;
    fm_uint32             CSGLORT;
    fm_uint32             DGLORT;
    fm_uint32             OUT_DGLORT;
    fm_uint32             DGLORT_OR_CPU_CODE;
    fm_uint32             SGLORT;
    fm_uint32             MA1_FID1_CMD;
    fm_uint32             MA2_FID1_CMD;
    fm_uint32             MA1_FID2_CMD;
    fm_uint32             MA2_FID2_CMD;
    fm_uint32             MA1_CMD;
    fm_uint32             MA2_CMD;
    fm_uint32             LAG_CmdA;
    fm_uint32             LAG_CmdB;
    fm_uint32             LAG_DropCodeSelect;
    fm_uint32             EACL_CmdA;
    fm_uint32             EACL_CmdB;
    fm_uint32             EACL_DropCodeSelect;
    fm_uint64             LBSCamHit[2];
    fm_uint32             LBS_CmdA;
    fm_uint32             LBS_CmdB;
    fm_uint32             LBS_DropCodeSelect;
    fm_uint32             L2F_CmdA[FM6000_L2F_PROFILE_TABLE_ENTRIES_1];
    fm_uint32             L2F_CmdB[FM6000_L2F_PROFILE_TABLE_ENTRIES_1];
    fm_uint32             L2F_TableSelect[FM6000_L2F_PROFILE_TABLE_ENTRIES_1];
    fm_uint32             L2F_IndexSelect[FM6000_L2F_PROFILE_TABLE_ENTRIES_1];
    fm_uint32             L2F_TableID[FM6000_L2F_PROFILE_TABLE_ENTRIES_1];
    fm_uint32             L2F_CmdLookup[FM6000_L2F_PROFILE_TABLE_ENTRIES_1];
    fm_uint32             L2F_DropCode[FM6000_L2F_PROFILE_TABLE_ENTRIES_1];
    fm_uint32             L2F_DropCodeSelect[FM6000_L2F_PROFILE_TABLE_ENTRIES_1];
    fm_uint32             L3AR_DMASK_IDX1;
    fm_uint32             L3AR_DMASK_IDX2;
    fm_uint32             L3AR_DMASK_IDX3;
    fm_uint64             SMASK[2];
    fm_uint32             ACTION_DATA_W16B;
    fm_uint32             TRAP_HEADER_IDX;
    fm_uint32             TRAP_HEADER_ENABLE;
    fm_uint32             L2F_ISTATE;
    fm_uint64             MA1_MAC;
    fm_uint64             MA2_MAC;
    fm_uint64             MA1_HitPrecMask;
    fm_uint64             MA2_HitPrecMask;
    fm_uint32             MA1_FID1;
    fm_uint32             MA1_FID2;
    fm_uint32             MA2_FID1;
    fm_uint32             MA2_FID2;
    fm_uint64             NEXTHOP_DATA_W16C_W16B_W16A;
    fm_uint32             NEXTHOP_DATA_W8B_W4A;
    fm_uint32             L2L_ETAG1;
    fm_uint32             L2L_ETAG2;
    fm_uint32             DMASK_CMD_PROFILE;
    fm_int                DMASK_IDX3;
    fm_uint32             ALU_X[6];
    fm_uint32             ALU_Y[6];
    fm_uint32             ALU_X_Select[6];
    fm_uint32             ALU_Y_Select[6];
    fm_bool               ALU_Y_MALookup[6];
    fm_uint32             ALU_SwapXY[6];
    fm_uint32             ALU_RollX[6];
    fm_uint32             ALU_RollY[6];
    fm_uint32             ALU_MaskX[6];
    fm_uint32             ALU_MaskY[6];
    fm_uint32             ALU_MapY[6];
    fm_uint32             ALU_Function[6];
    fm_uint32             ALU_OneTwo[6];
    fm_uint32             ALU_OrIntoL2ARKey[6];
    fm_uint32             MA2_DATA;
    fm_uint32             MA1_DATA;
    fm_uint32             ACTION_DATA_W8A;
    fm_uint32             ACTION_DATA_W8B;
    fm_uint32             ACTION_DATA_W8C;
    fm_uint32             ACTION_DATA_W8D;
    fm_uint32             ACTION_DATA_W8E;
    fm_uint32             ACTION_DATA_W8F;
    fm_uint32             ACTION_DATA_W16A;
    fm_uint32             ACTION_DATA_W16C;
    fm_uint32             ACTION_DATA_W16D;
    fm_uint32             ACTION_DATA_W16E;
    fm_uint32             ACTION_DATA_W16F;
    fm_uint32             ACTION_DATA_W16G;
    fm_uint32             ACTION_DATA_W16H;
    fm_uint32             QOS_MAP1_IDX;
    fm_uint32             QOS_MAP2_IDX;
    fm_uint32             OUT_QOS_L3_PRI;
    fm_uint32             OUT_QOS_W4_OUT_QOS_ISL_PRI;
    fm_uint32             OUT_QOS_L2_VPRI1_OUT_QOS_L2_VPRI2;
    fm_uint32             POL1_IDX;
    fm_uint32             POL2_IDX;
    fm_uint32             POL3_IDX;
    fm_uint32             POL3_IDX_PROFILE;

    /* Hash output */
    fm_uint32             L2_HASH_RANDOM;

    /* Channels generated by L2 Lookup */
    fm_int                ISTID;
    fm_int                ESTID;
    fm_int                MA1_HPV;
    fm_int                MA2_HPV;
    fm_int                MA2_MPV;
    fm_int                L2L_IT_IDX;
    fm_int                L2L_ET_IDX;
    fm_int                MA1_TAG;
    fm_int                MA2_TAG;
    fm_int                ACTION_DATA_W16A_11_8_MA2_TAG_7_0;
    fm_uint32             L2L_ITAG1;
    fm_uint32             L2L_ITAG2;
    fm_uint32             MA1_GLORT;
    fm_uint32             MA2_GLORT;

    /* L2 lookup state */
    fm_int                writeBackBank;
    fm_int                writeBackIndex;
    fm_int                writeNewBank;
    fm_int                writeNewIndex;
    fm6000_modelL2LMacTableEntry writeBackEntry;
    fm6000_modelL2LMacTableEntry writeNewEntry;

    /* L2 Mirror State */
    fm_int                MIR_RX_0;
    fm_int                MIR_TX_0;
    fm_int                MIR_TRUNC_0;
    fm_int                MIR_MAP_PRI_0;
    fm_int                MIR_RX_1;
    fm_int                MIR_TX_1;
    fm_int                MIR_TRUNC_1;
    fm_int                MIR_MAP_PRI_1;
    fm_int                MIR_RX_2;
    fm_int                MIR_TX_2;
    fm_int                MIR_TRUNC_2;
    fm_int                MIR_MAP_PRI_2;
    fm_int                MIR_RX_3;
    fm_int                MIR_TX_3;
    fm_int                MIR_TRUNC_3;
    fm_int                MIR_MAP_PRI_3;
    fm_int                MIR_RX_4;
    fm_int                MIR_TX_4;
    fm_int                MIR_TRUNC_4;
    fm_int                MIR_MAP_PRI_4;
    fm_int                MIR_RX_5;
    fm_int                MIR_TX_5;
    fm_int                MIR_TRUNC_5;
    fm_int                MIR_MAP_PRI_5;
    fm_int                MIR_RX_6;
    fm_int                MIR_TX_6;
    fm_int                MIR_TRUNC_6;
    fm_int                MIR_MAP_PRI_6;
    fm_int                MIR_RX_7;
    fm_int                MIR_TX_7;
    fm_int                MIR_TRUNC_7;
    fm_int                MIR_MAP_PRI_7;
    fm_int                MIR_RX_8;
    fm_int                MIR_TX_8;
    fm_int                MIR_TRUNC_8;
    fm_int                MIR_MAP_PRI_8;
    fm_int                MIR_RX_9;
    fm_int                MIR_TX_9;
    fm_int                MIR_TRUNC_9;
    fm_int                MIR_MAP_PRI_9;
    fm_int                MIR_RX_10;
    fm_int                MIR_TX_10;
    fm_int                MIR_TRUNC_10;
    fm_int                MIR_MAP_PRI_10;
    fm_int                MIR_RX_11;
    fm_int                MIR_TX_11;
    fm_int                MIR_TRUNC_11;
    fm_int                MIR_MAP_PRI_11;
    fm_int                MIR_RX_12;
    fm_int                MIR_TX_12;
    fm_int                MIR_TRUNC_12;
    fm_int                MIR_MAP_PRI_12;
    fm_int                MIR_RX_13;
    fm_int                MIR_TX_13;
    fm_int                MIR_TRUNC_13;
    fm_int                MIR_MAP_PRI_13;
    fm_int                MIR_RX_14;
    fm_int                MIR_TX_14;
    fm_int                MIR_TRUNC_14;
    fm_int                MIR_MAP_PRI_14;
    fm_int                MIR_RX_15;
    fm_int                MIR_TX_15;
    fm_int                MIR_TRUNC_15;
    fm_int                MIR_MAP_PRI_15;
    fm_int                MIR_RX_16;
    fm_int                MIR_TX_16;
    fm_int                MIR_TRUNC_16;
    fm_int                MIR_MAP_PRI_16;
    fm_int                MIR_RX_17;
    fm_int                MIR_TX_17;
    fm_int                MIR_TRUNC_17;
    fm_int                MIR_MAP_PRI_17;
    fm_int                MIR_RX_18;
    fm_int                MIR_TX_18;
    fm_int                MIR_TRUNC_18;
    fm_int                MIR_MAP_PRI_18;
    fm_int                MIR_RX_19;
    fm_int                MIR_TX_19;
    fm_int                MIR_TRUNC_19;
    fm_int                MIR_MAP_PRI_19;
    fm_int                MIR_RX_20;
    fm_int                MIR_TX_20;
    fm_int                MIR_TRUNC_20;
    fm_int                MIR_MAP_PRI_20;
    fm_int                MIR_RX_21;
    fm_int                MIR_TX_21;
    fm_int                MIR_TRUNC_21;
    fm_int                MIR_MAP_PRI_21;
    fm_int                MIR_RX_22;
    fm_int                MIR_TX_22;
    fm_int                MIR_TRUNC_22;
    fm_int                MIR_MAP_PRI_22;
    fm_int                MIR_RX_23;
    fm_int                MIR_TX_23;
    fm_int                MIR_TRUNC_23;
    fm_int                MIR_MAP_PRI_23;
    fm_int                MIR_RX_24;
    fm_int                MIR_TX_24;
    fm_int                MIR_TRUNC_24;
    fm_int                MIR_MAP_PRI_24;
    fm_int                MIR_RX_25;
    fm_int                MIR_TX_25;
    fm_int                MIR_TRUNC_25;
    fm_int                MIR_MAP_PRI_25;
    fm_int                MIR_RX_26;
    fm_int                MIR_TX_26;
    fm_int                MIR_TRUNC_26;
    fm_int                MIR_MAP_PRI_26;
    fm_int                MIR_RX_27;
    fm_int                MIR_TX_27;
    fm_int                MIR_TRUNC_27;
    fm_int                MIR_MAP_PRI_27;
    fm_int                MIR_RX_28;
    fm_int                MIR_TX_28;
    fm_int                MIR_TRUNC_28;
    fm_int                MIR_MAP_PRI_28;
    fm_int                MIR_RX_29;
    fm_int                MIR_TX_29;
    fm_int                MIR_TRUNC_29;
    fm_int                MIR_MAP_PRI_29;
    fm_int                MIR_RX_30;
    fm_int                MIR_TX_30;
    fm_int                MIR_TRUNC_30;
    fm_int                MIR_MAP_PRI_30;
    fm_int                MIR_RX_31;
    fm_int                MIR_TX_31;
    fm_int                MIR_TRUNC_31;
    fm_int                MIR_MAP_PRI_31;
    fm_int                MIR_RX_32;
    fm_int                MIR_TX_32;
    fm_int                MIR_TRUNC_32;
    fm_int                MIR_MAP_PRI_32;
    fm_int                MIR_RX_33;
    fm_int                MIR_TX_33;
    fm_int                MIR_TRUNC_33;
    fm_int                MIR_MAP_PRI_33;
    fm_int                MIR_RX_34;
    fm_int                MIR_TX_34;
    fm_int                MIR_TRUNC_34;
    fm_int                MIR_MAP_PRI_34;
    fm_int                MIR_RX_35;
    fm_int                MIR_TX_35;
    fm_int                MIR_TRUNC_35;
    fm_int                MIR_MAP_PRI_35;
    fm_int                MIR_RX_36;
    fm_int                MIR_TX_36;
    fm_int                MIR_TRUNC_36;
    fm_int                MIR_MAP_PRI_36;
    fm_int                MIR_RX_37;
    fm_int                MIR_TX_37;
    fm_int                MIR_TRUNC_37;
    fm_int                MIR_MAP_PRI_37;
    fm_int                MIR_RX_38;
    fm_int                MIR_TX_38;
    fm_int                MIR_TRUNC_38;
    fm_int                MIR_MAP_PRI_38;
    fm_int                MIR_RX_39;
    fm_int                MIR_TX_39;
    fm_int                MIR_TRUNC_39;
    fm_int                MIR_MAP_PRI_39;
    fm_int                MIR_RX_40;
    fm_int                MIR_TX_40;
    fm_int                MIR_TRUNC_40;
    fm_int                MIR_MAP_PRI_40;
    fm_int                MIR_RX_41;
    fm_int                MIR_TX_41;
    fm_int                MIR_TRUNC_41;
    fm_int                MIR_MAP_PRI_41;
    fm_int                MIR_RX_42;
    fm_int                MIR_TX_42;
    fm_int                MIR_TRUNC_42;
    fm_int                MIR_MAP_PRI_42;
    fm_int                MIR_RX_43;
    fm_int                MIR_TX_43;
    fm_int                MIR_TRUNC_43;
    fm_int                MIR_MAP_PRI_43;
    fm_int                MIR_RX_44;
    fm_int                MIR_TX_44;
    fm_int                MIR_TRUNC_44;
    fm_int                MIR_MAP_PRI_44;
    fm_int                MIR_RX_45;
    fm_int                MIR_TX_45;
    fm_int                MIR_TRUNC_45;
    fm_int                MIR_MAP_PRI_45;
    fm_int                MIR_RX_46;
    fm_int                MIR_TX_46;
    fm_int                MIR_TRUNC_46;
    fm_int                MIR_MAP_PRI_46;
    fm_int                MIR_RX_47;
    fm_int                MIR_TX_47;
    fm_int                MIR_TRUNC_47;
    fm_int                MIR_MAP_PRI_47;
    fm_int                MIR_RX_48;
    fm_int                MIR_TX_48;
    fm_int                MIR_TRUNC_48;
    fm_int                MIR_MAP_PRI_48;
    fm_int                MIR_RX_49;
    fm_int                MIR_TX_49;
    fm_int                MIR_TRUNC_49;
    fm_int                MIR_MAP_PRI_49;
    fm_int                MIR_RX_50;
    fm_int                MIR_TX_50;
    fm_int                MIR_TRUNC_50;
    fm_int                MIR_MAP_PRI_50;
    fm_int                MIR_RX_51;
    fm_int                MIR_TX_51;
    fm_int                MIR_TRUNC_51;
    fm_int                MIR_MAP_PRI_51;
    fm_int                MIR_RX_52;
    fm_int                MIR_TX_52;
    fm_int                MIR_TRUNC_52;
    fm_int                MIR_MAP_PRI_52;
    fm_int                MIR_RX_53;
    fm_int                MIR_TX_53;
    fm_int                MIR_TRUNC_53;
    fm_int                MIR_MAP_PRI_53;
    fm_int                MIR_RX_54;
    fm_int                MIR_TX_54;
    fm_int                MIR_TRUNC_54;
    fm_int                MIR_MAP_PRI_54;
    fm_int                MIR_RX_55;
    fm_int                MIR_TX_55;
    fm_int                MIR_TRUNC_55;
    fm_int                MIR_MAP_PRI_55;
    fm_int                MIR_RX_56;
    fm_int                MIR_TX_56;
    fm_int                MIR_TRUNC_56;
    fm_int                MIR_MAP_PRI_56;
    fm_int                MIR_RX_57;
    fm_int                MIR_TX_57;
    fm_int                MIR_TRUNC_57;
    fm_int                MIR_MAP_PRI_57;
    fm_int                MIR_RX_58;
    fm_int                MIR_TX_58;
    fm_int                MIR_TRUNC_58;
    fm_int                MIR_MAP_PRI_58;
    fm_int                MIR_RX_59;
    fm_int                MIR_TX_59;
    fm_int                MIR_TRUNC_59;
    fm_int                MIR_MAP_PRI_59;
    fm_int                MIR_RX_60;
    fm_int                MIR_TX_60;
    fm_int                MIR_TRUNC_60;
    fm_int                MIR_MAP_PRI_60;
    fm_int                MIR_RX_61;
    fm_int                MIR_TX_61;
    fm_int                MIR_TRUNC_61;
    fm_int                MIR_MAP_PRI_61;
    fm_int                MIR_RX_62;
    fm_int                MIR_TX_62;
    fm_int                MIR_TRUNC_62;
    fm_int                MIR_MAP_PRI_62;
    fm_int                MIR_RX_63;
    fm_int                MIR_TX_63;
    fm_int                MIR_TRUNC_63;
    fm_int                MIR_MAP_PRI_63;

    /* Channels generated by GLORT Lookup */
    fm_int                GLORT_DMASK_IDX;
    fm_uint64             LAG_MASK[2];
    fm_int                DGLORT_TAG;

    /* Channels generated by the ALU */
    fm_int                ALU_Z[6];
    fm_uint32             ALU13_Z;
    fm_uint32             ALU46_Z;

    /* Channels generated by L2F */
    fm_uint32             DROP_CODE;
    fm_uint64             DMASK_A[2];
    fm_uint64             DMASK_B[2];

    /* Channels generated by EACL */
    fm_int                eaclHit;
    fm_int                eaclHitIndex[32];
    fm_int                EACL_EXT[32];
    fm_int                EACL_COUNT[32];

    /* Channels generated/used by L2AR */
    fm_uint64             LOCAL_FLAGS;
    fm_uint32             DMT_NEXT_STAGE;
    fm_uint32             MA2_FID1_IVL;
    fm_uint32             MA2_FID2_IVL;
    fm_uint32             QOS_MAP1_FIRST;
    fm_uint32             QOS_MAP1_SECOND;
    fm_uint32             QOS_MAP2_FIRST;
    fm_uint32             QOS_MAP2_SECOND;
    fm_uint32             POL1_TAG1_5_0_QOS_L3_PRI_1_0;
    fm_uint32             POL1_TAG2_5_0_QOS_L3_PRI_1_0;
    fm_uint32             POL2_TAG1_5_0_QOS_L3_PRI_1_0;
    fm_uint32             POL2_TAG2_5_0_QOS_L3_PRI_1_0;
    fm_uint32             POL1_TAG1;
    fm_uint32             POL1_TAG2;
    fm_uint32             POL2_TAG1;
    fm_uint32             POL2_TAG2;
    fm_uint32             POL3_TAG;
    fm_uint32             POL1_TAG1_TOP;
    fm_uint32             POL1_TAG2_TOP;
    fm_uint32             POL2_TAG1_TOP;
    fm_uint32             POL2_TAG2_TOP;
    fm_uint32             POL3_TAG_TOP;
    fm_uint32             MA_WRITEBACK_TAG;
    fm_uint32             MA_WRITEBACK_DATA;
    fm_uint32             MA_WRITEBACK_GLORT;
    fm_uint32             MA_WRITEBACK_PREC;
    fm_uint32             DMASK_IDX;
    fm_uint32             MOD_FLAGS;
    fm_uint32             MOD_DATA_W16A;
    fm_uint32             MOD_DATA_W16B;
    fm_uint32             MOD_DATA_W16C;
    fm_uint32             MOD_DATA_W16D;
    fm_uint32             MOD_DATA_W16E;
    fm_uint32             MOD_DATA_W16F;
    fm_int                MIR_RX[4];
    fm_int                MIR_TX[4];
    fm_int                MIR_TRUNC[4];
    fm_int                MIR_MAP_PRI[4];
    fm_uint32             CPU_CODE;
    fm_uint32             OUT_CPU_CODE;
    fm_uint32             DMT_CmdA[3];
    fm_uint32             DMT_CmdB[3];
    fm_uint32             DMT_ACTION_DROP_CODE[3];
    fm_uint64             DMT_ACTION_DMASK[3][2];
    fm_uint32             TransformDestMask;
    fm_uint32             DMT_PROFILE;
    fm_uint32             SetMirror[4];
    fm_uint32             MA2_LOOKUP;
    fm_uint32             MOD_DATA_W8A;
    fm_uint32             MOD_DATA_W8B;
    fm_uint32             MOD_DATA_W8C;
    fm_uint32             MOD_DATA_W8D;
    fm_uint32             MOD_DATA_W8E;
    fm_uint32             RX_STATS_IDX5A;
    fm_uint32             RX_STATS_IDX5B;
    fm_uint32             RX_STATS_IDX5C;
    fm_uint32             RX_STATS_IDX12A;
    fm_uint32             RX_STATS_IDX12B;
    fm_uint32             RX_STATS_IDX16A;
    fm_uint32             RX_STATS_IDX16B;
    fm_uint32             TX_STATS_IDX4A;
    fm_uint32             TX_STATS_IDX4B;
    fm_uint32             TX_STATS_IDX12A;
    fm_uint32             TX_STATS_IDX12B;
    fm_uint32             OUT_QOS_ISL_PRI;
    fm_uint32             OUT_QOS_L2_VPRI1;
    fm_uint32             OUT_QOS_L2_VPRI2;
    fm_uint32             OUT_QOS_W4;
    fm_uint32             OUT_QOS_TC;
    fm_uint32             OUT_QOS_SMP;
    fm_uint32             flagWriteBack;
    fm_uint32             flagWriteNew;
    fm_bool               flagWriteBackEnabled;
    fm_bool               flagWriteNewEnabled;

    /* Channels generated by MCAST */
    fm_int                mcastIndex;
    fm_bool               mcastRunning;
    fm_int                mcastPortPos;
    fm_int                mcastTxPort;
    fm_int                mcastTxMirror;
    fm_uint32             saved_L2_VID1;
    fm_uint32             saved_L2_VID2;
    fm_uint32             TX_PORT;
    fm_uint32             DST_PORT;
    fm_uint32             MOD_MIR_RX;
    fm_uint32             MOD_MIR_TX;
    fm_uint32             MIR_NUM;
    fm_uint32             TRUNC;
    fm_uint32             MAP_PRI;
    fm_uint32             L2_VID_EQUAL;
    fm_uint32             MCAST_TAG;
    fm_uint32             L2_TAG;
    fm_uint32             TX_DISP;
    fm_uint32             RX_LENGTH;
    fm_uint32             TX_LENGTH;

    /* Stats AR output */
    fm_uint32             RX_VALID;
    fm_uint32             RX_PORT;
    fm_uint32             RX_PORT_TAG;
    fm_uint32             RX_DISP;
    fm_uint32             RX_KEY_MOD_FLAGS;
    fm_uint32             RX_KEY_STATS_FLAGS;
    fm_uint32             RX_KEY_DROP_CODE;
    fm_uint32             RX_KEY_CPU_CODE;
    fm_uint32             RX_KEY_RXMP;
    fm_uint32             RX_KEY_CM_FLAGS;
    fm_uint32             TX_VALID;
    fm_uint32             TX_PORT_TAG;
    fm_uint32             TX_FREE;
    fm_uint32             TX_OOM;
    fm_uint32             TX_DISP_FLAGS;
    fm_uint32             TX_RX_ERR;
    fm_uint64             TX_MOD_KEY;
    fm_uint32             AR_IDX5[6];
    fm_uint32             RX_DELTA;
    fm_uint32             TX_DELTA;
    fm_uint32             DISCRETE_MASK_LO;

    /* Stats AR state */
    fm_uint32             statsIndexValue;

    /* Stats AR Flags output */
    fm_uint64             STATS_KEY;
    fm_uint64             STATS_FLAGS[2];

    /* Remaining modify channels */
    fm_uint32             QOS_W4_ISL_PRI;
    fm_uint32             QOS_L3_DSCP_CU;
    fm_uint32             MOD_DATA_W4;
    fm_uint32             L2_TX_VPRI_VID1;
    fm_uint32             L2_TX_VPRI_VID2;
    fm_uint32             L2_TX_VPRI1_W12A;
    fm_uint32             MOD_MAP_DATA_W8A;
    fm_uint32             MOD_MAP_DATA_W8B;
    fm_uint32             MOD_MAP_DATA_W16A;
    fm_uint32             MOD_MAP_DATA_W16B;
    fm_uint32             MOD_MAP_DATA_W16C;
    fm_uint32             MOD_MAP_DATA_W16D;
    fm_uint32             MOD_MAP_DATA_W16E;
    fm_uint32             MOD_MAP_DATA_W16F;
    fm_uint32             MOD_MAP_DATA_W12A;
    fm_uint32             MOD_DATA_W8D_W8E;

    /* For modify command CAM */
    fm_uint32             L2_VLAN1_TX_Tagged; 
    fm_uint32             L2_VLAN2_TX_Tagged; 
    fm_uint32             TX_PORT_Tag;
    fm_uint32             DST_PORT_Tag;
    fm_uint32             PAUSE;
    fm_uint32             Command;
    fm_uint32             Valid;
    fm_uint32             Jitter;

    /* For modify value CAM */
    fm_uint32             ValA_Type;
    fm_uint32             ValA_Constant;
    fm_uint32             ValA_DataSelect;
    fm_uint32             ValB_Type;
    fm_uint32             ValB_Constant;
    fm_uint32             ValB_DataSelect;
    fm_uint32             ValC_Type;
    fm_uint32             ValC_Constant;
    fm_uint32             ValC_DataSelect;
    fm_uint32             ValD_Type;
    fm_uint32             ValD_Constant;
    fm_uint32             ValD_DataSelect;

    /* Holds the queue fed to the modify engine */
    fm_uint32             modCommandQueue[16];
    fm_uint32             modCommandQueueValid[16];
    fm_uint32             modValueQueue[64];
    fm_uint32             modValueQueueValid[64];
    fm_int                modCommandSlot;
    fm_int                modValueSlot;

    /* Packet bytestream on egress */
    fm_byte *             outData;

    /* Packet length on egress */
    fm_int                outLength;

} fm6000_modelState;

/**************************************************
 * Overall switch state structure. Contains
 * all registers useful used by the model and
 * all persistent internal states.
 **************************************************/

typedef struct _fm6000_model
{
    /* The switch that this white model state maps to */
    fm_int                          sw;

    /* The switch revision of this FM6000 model, 0 = Ax, 1 = Bx */
    fm_int                          rev;

    /* Lock to provide exclusive access to the model state & registers */
    fm_lock                         modelLock;

    /* Map from register address to value */
    fm_tree                         registers;
    
    /* Holds the state of a packet during the pipline processing */
    fm6000_modelState               packetState;

    /* True during the pipeline processing of a packet */
    fm_bool                         processingPacket;

#ifdef SYNC_MODE_ENABLED
    /* If True, SyncCallback function will be called after each register write */
    fm_bool                         syncEnabled;

    /* Pointer to the register synchronization callback function */
    fm_status                       (*SyncCallback)(fm_int sw);
#endif

    /***************************************************
     * The following members hold persistent state
     * for the white model.
     **************************************************/

    fm_uint64                       parserInitState[76];
    /* The A member contains values for fields 0..3 */
    fm_uint32                       parserInitFieldsA[76][4]; 
    /* The B member contains values for fields 8..11 */
    fm_uint32                       parserInitFieldsB[76][4];
    fm_uint16                       defaultVid1[MAX_PORTS];
    fm_uint16                       defaultVid2[MAX_PORTS];
    fm_byte                         defaultVpri1[MAX_PORTS];
    fm_byte                         defaultVpri2[MAX_PORTS];
    fm_uint16                       defaultGlort[MAX_PORTS];
    fm_bool                         decodeF64[MAX_PORTS];

    /* for Mapper */
    fm_uint32                       MAPPER_VID_PROFILE_TABLE[4];
    fm_uint32                       MAPPER_VID1_TABLE[4096];
    fm_uint32                       MAPPER_VID2_TABLE[4096];
    fm_uint64                       MAPPER_SRC_PORT_TABLE[76];
    fm_byte                         MAPPER_SCENARIO_FLAGS_CFG[16];
    fm_uint64                       MAPPER_L4_SRC_COMPARE[FM6000_MAPPER_L4_SRC_COMPARE_ENTRIES];
    fm_uint64                       MAPPER_L4_DST_COMPARE[FM6000_MAPPER_L4_DST_COMPARE_ENTRIES];

    /* for FFU - BST section */
    fm_uint64                       FFU_BST_ACTION[MAX_BST_SLICES]
                                                  [MAX_BST_PARTITIONS]
                                                  [MAX_BST_KEYS];
    fm_uint32                       FFU_BST_KEY[MAX_BST_SLICES]
                                               [MAX_BST_PARTITIONS]
                                               [MAX_BST_KEYS];
    fm6000_modelFFUScenarioCam      FFU_BST_SCENARIO_CAM[MAX_BST_SLICES]
                                                        [MAX_BST_SCENARIOS];
    fm6000_modelBSTScenarioCfg1     FFU_BST_SCENARIO_CFG1[MAX_BST_SLICES]
                                                         [MAX_BST_SCENARIOS];
    fm6000_modelBSTScenarioCfg2     FFU_BST_SCENARIO_CFG2[MAX_BST_SLICES]
                                                         [MAX_BST_SCENARIOS];
    fm6000_modelBSTRootKeys         FFU_BST_ROOT_KEYS[MAX_BST_SLICES]
                                                     [MAX_BST_PARTITIONS];
    fm6000_modelBSTRootKeys         FFU_BST_ROOT_KEYS_SH[MAX_BST_SLICES]
                                                        [MAX_BST_PARTITIONS];
    fm_int                          FFU_BST_MASTER_VALID[MAX_BST_SLICES];
    fm_int                          FFU_BST_MASTER_VALID_SH[MAX_BST_SLICES];
                                                       
    /* for FFU - Slice section */                      
    fm6000_modelFFUSliceCam         FFU_SLICE_CAM[MAX_CAM_SLICES]
                                                 [MAX_SLICE_KEYS];
    fm6000_modelFFUScenarioCam      FFU_SLICE_SCENARIO_CAM[MAX_CAM_SLICES]
                                                          [MAX_SLICE_SCENARIOS];
    fm6000_modelFFUSliceScenarioCfg FFU_SLICE_SCENARIO_CFG[MAX_CAM_SLICES]
                                                          [MAX_SLICE_SCENARIOS];
    fm_uint64                       FFU_SLICE_ACTION[MAX_CAM_SLICES] 
                                                    [MAX_SLICE_KEYS];
    fm_int                          FFU_SLICE_MASTER_VALID[MAX_CAM_SLICES];
    fm_int                          FFU_SLICE_MASTER_VALID_SH[MAX_CAM_SLICES];
    fm6000_modelFFUScenarioCam      FFU_REMAP_SCENARIO_CAM[MAX_SLICE_SCENARIOS];
    fm6000_modelFFURemapProfile     FFU_REMAP_PROFILE[MAX_SLICE_SCENARIOS];
    fm6000_modelFFULHashL3Profile   FFU_HASH_L3_PROFILE[MAX_HASH_L3_PROFILES];
    fm_uint32                       ffuSliceKeyPresent[MAX_CAM_SLICES]
                                                      [MAX_CASES]
                                                      [MAX_SLICE_KEYS/32];
    fm_int                          FFU_ATOMIC_APPLY;
    fm_uint64                       FFU_EACL_CFG;

    /* for NEXTHOP */
    fm_uint64                       NEXTHOP_TABLE[FM6000_NEXTHOP_TABLE_ENTRIES];

    /* for GLORT */
    fm_uint16                       glortCamKey[FM6000_GLORT_CAM_ENTRIES];
    fm_uint16                       glortCamKeyInvert[FM6000_GLORT_CAM_ENTRIES];
    fm_uint32                       glortCamValid[FM6000_GLORT_CAM_ENTRIES / (FM6000_GLORT_CAM_ENTRIES / 32)];
    fm6000_modelGlortRam            glortRam[FM6000_GLORT_CAM_ENTRIES];
    fm6000_modelLagPortTable        LAG_PORT_TABLE[MAX_PORTS];
    fm_uint64                       lagFilteringCamKey[MAX_PORTS];
    fm_uint64                       lagFilteringCamKeyInvert[MAX_PORTS];

    /* for ALU */
    fm_uint16                       ALU_Y_TABLE[6][16];

    /* for Policers */
    fm6000_modelPolicersEntry       POLICER_CFG_4K[2][4096][2];
    fm6000_modelPolicersEntry       POLICER_CFG_1K[1024];

    /* for L2L */
    fm6000_modelL2LMacTableEntry    L2L_MAC_TABLE[16][4096];
    fm6000_modelL2LEvid1Table       L2L_EVID1_TABLE[4096];
    fm6000_modelL2LEvid2Table       L2L_EVID2_TABLE[4096];
    fm6000_modelL2LIvid1Table       L2L_IVID1_TABLE[4096];
    fm6000_modelL2LIVid2Table       L2L_IVID2_TABLE[4096];
    fm_byte                         hashTable[256];
    fm_int                          actionPrecedence[8];
    fm_int                          L2L_SWEEPER_FIFO_HEAD;
    fm_int                          L2L_SWEEPER_FIFO_TAIL;
    fm6000_modelSweeperCam          L2L_SWEEPER_CAM[16];
    fm6000_modelSweeperRam          L2L_SWEEPER_RAM[16];
    fm_uint32                       L2L_SWEEPER_IP;
    fm_uint32                       L2L_SWEEPER_IM;
    fm6000_modelL2LMacTableEntry    L2L_SWEEPER_WRITE_DATA;
    fm6000_modelSweeperConfig       sweeper[2];
    fm_int                          fifoFull;
    fm_int                          lastIndex;

    /* For EACL */
    fm6000_modelFFUEgressACLCam1    EACL_CAM1[32];
    fm6000_modelFFUEgressACLCam2    EACL_CAM2[MAX_PORTS];
    fm6000_modelFFUEgressACLAction  EACL_ACTION_RAM[32][32];

    /* for L2F */
    fm_uint64                       L2F_TABLE_4K[FM6000_L2F_TABLE_4K_ENTRIES_1]
                                                [4096][2];
    fm_uint64                       L2F_TABLE_256[FM6000_L2F_TABLE_256_ENTRIES_1] 
                                                 [256][2];

    /* for MODIFY */
    fm_uint32                       MOD_MAP_DATA_CTRL;
    fm_uint32                       MOD_MAP_DATA_V2T_CTRL;
    fm_uint64                       MOD_L2_VLAN1_TX_TAGGED[4096][2];
    fm_uint64                       MOD_L2_VLAN2_TX_TAGGED[4096][2];
    fm_uint64                       MOD_L2_VPRI1_TX_MAP[76];
    fm_uint64                       MOD_L2_VPRI2_TX_MAP[76];
    fm_uint32                       MOD_MAP_DATA_W8A[76];
    fm_uint32                       MOD_MAP_DATA_W8B[76];
    fm_uint32                       MOD_MAP_DATA_W16A[4096];
    fm_uint32                       MOD_MAP_DATA_W16B[4096];
    fm_uint32                       MOD_MAP_DATA_W16C[4096];
    fm_uint32                       MOD_MAP_DATA_W16D[4096];
    fm_uint32                       MOD_MAP_DATA_W16E[76];
    fm_uint32                       MOD_MAP_DATA_W16F[76];
    fm_uint32                       MOD_MAP_IDX12A[4096];
    fm_uint32                       MOD_TX_MIRROR_SRC;
    fm_uint32                       MOD_TX_PORT_TAG[76];
    fm_uint32                       MOD_DST_PORT_TAG[76];
    fm_uint32                       MOD_MIN_LENGTH[76];
    fm_uint32                       MOD_MAP_DATA_W12A[76];

    /* for MCAST */
    fm6000_modelMcastMirrorConfig   MCAST_MIRROR_CFG[4];
    fm6000_modelMcastTxMirrorDest   MCAST_TX_MIRROR_DEST[10];
    fm6000_modelMcastDestTable      MCAST_DEST_TABLE[4096];
    fm_uint16                       MCAST_VLAN_TABLE[32768];
    fm6000_modelMcastLoopbackSup    MCAST_LOOPBACK_SUPPRESS[MAX_PORTS];
    fm_uint32                       DMASK[MAX_PORTS / 32 + 1];
    fm_uint64                       CM_TX_MIRROR_SRC[4][2];
    fm6000_modelCMTxMirrorDest      CM_TX_MIRROR_DEST[4];

    /* for EPL, note that EPL are indexed 1..24, 0 isn't used. */
    fm_uint32                       EPL_IP[FM6000_NUM_EPLS];
    fm_uint32                       AN_IP[FM6000_NUM_EPLS][FM6000_PORTS_PER_EPL];
    fm_uint32                       AN_IM[FM6000_NUM_EPLS][FM6000_PORTS_PER_EPL];
    fm_uint32                       LINK_IP[FM6000_NUM_EPLS][FM6000_PORTS_PER_EPL];
    fm_uint32                       LINK_IM[FM6000_NUM_EPLS][FM6000_PORTS_PER_EPL];
    fm_uint32                       PORT_STATUS[FM6000_NUM_EPLS][FM6000_PORTS_PER_EPL];
    fm_uint32                       SERDES_IP[FM6000_NUM_EPLS][FM6000_PORTS_PER_EPL];
    fm_uint32                       SERDES_IM[FM6000_NUM_EPLS][FM6000_PORTS_PER_EPL];
    fm_uint64                       SERDES_STATUS[FM6000_NUM_EPLS][FM6000_PORTS_PER_EPL];

    /* Internal port stats */
    fm_int                          linkUp[76];

    /* GLOBAL */
    fm_uint32                       GLOBAL_INTERRUPT_DETECT;
    fm_uint32                       GLOBAL_EPL_INT_DETECT;

    /* SRAM errors [MGMT2] */
    fm_uint32                       SRAM_CORRECTED_IP[4];
    fm_uint32                       SRAM_CORRECTED_IM[4];
    fm_uint32                       SRAM_UNCORRECTABLE_IP[4];
    fm_uint32                       SRAM_UNCORRECTABLE_IM[4];

    /* For statistics */
    fm6000_modelStatsLengthBin      RX_LENGTH_BINS[16];
    fm6000_modelStatsLengthBin      TX_LENGTH_BINS[16];

    /* Stats AR bank configuration */
    fm_byte                         statsCounterType[20];
    fm_byte                         statsAmount[20];
    fm_byte                         statsCounterNum[20];
    fm_byte                         statsPerChannel[20];
    fm_byte                         statsPerLowBit[20];
    fm_byte                         statsPerHighBit[20];
    fm_byte                         statsPerSpanMantissa[20];
    fm_byte                         statsPerSpanExponent[20];
    fm_byte                         statsColorCase[20];
    fm_byte                         statsBankID[20];
    fm_uint64                       statsFlagsPolarity[2];
    fm_uint64                       statsFlagsValue[2];

    /* For LBS */
    fm6000_modelLBSCam              LBS_CAM[FM6000_LBS_CAM_ENTRIES];

    /* Caches MAC table bits */
    fm_uint32                       L2LHashRotation0;
    fm_uint32                       L2LHashRotation1;

    /* For scheduler */
    fm_bool                         isSchedulerInitialized;

} fm6000_model;


/*****************************************************************************
 * Prototypes.
 *****************************************************************************/

/* Direct register write */
fm_status fm6000ModelWriteCSRAbsolute(fm6000_model *model, 
                                      fm_uint32 addr, 
                                      fm_uint32 val);

fm_status fm6000ModelWriteCSRMultAbsolute(fm6000_model *model,
                                          fm_uint32  addr,
                                          fm_int     n,
                                          fm_uint32 *value);

fm_status fm6000ModelWriteCSRAbsolute64(fm6000_model *model,
                                        fm_uint32 addr,
                                        fm_uint64 value);

/* Glort Lookup */
void fm6000ModelGlort(fm6000_model *model);


/* Handles CSR parsing for Glort registers */
fm_status fm6000ModelGlortWriteCSR(fm6000_model *model,
                                   fm_uint32 addr,
                                   fm_uint32 val);


/*  NextHop */
void fm6000ModelNextHop(fm6000_model *model);


/* Handles CSR parsing for NextHop registers */
fm_status fm6000ModelNextHopWriteCSR(fm6000_model *model,
                                     fm_uint32 addr,
                                     fm_uint32 val);


/*  ALU */
void fm6000ModelALU(fm6000_model *model);


/* Handles CSR parsing for ALU registers */
fm_status fm6000ModelALUWriteCSR(fm6000_model *model,
                                 fm_uint32 addr,
                                 fm_uint32 val);


/*  Policer */
void fm6000ModelPolicers(fm6000_model *model);


/* Handles CSR parsing for Policer registers */
fm_status fm6000ModelPolicersWriteCSR(fm6000_model *model,
                                      fm_uint32 addr,
                                      fm_uint32 val);


/*  L2 Lookup */
void fm6000ModelL2L(fm6000_model *model);


/* Called to implement the post L2AR L2L write back */
void fm6000ModelL2LWriteBack(fm6000_model *model);


/* Handles CSR parsing for L2L registers */
fm_status fm6000ModelL2LWriteCSR(fm6000_model *model,
                                 fm_uint32 addr,
                                 fm_uint32 val);


/* Handles the SWEEPER_CFG register (which is in MGMT2). */
fm_status fm6000ModelL2LWriteSweeperCfg(fm6000_model *model, 
                                        fm_uint32     addr, 
                                        fm_uint32     val);


/* Handles Mapper specific initialization */
void fm6000ModelMapperInitialize(fm6000_model *model);

/* Handles L2L specific initialization */
void fm6000ModelL2LInitialize(fm6000_model *model);


/* Periodic tick function to implement the L2L sweepers */
fm_status fm6000ModelL2LTick(fm6000_model *model);

/*  L2 Filtering */
void fm6000ModelL2F(fm6000_model *model);


/* Handles CSR parsing for L2F registers */
fm_status fm6000ModelL2FWriteCSR(fm6000_model *model,
                                 fm_uint32 addr,
                                 fm_uint32 val);

/*  EACL */
void fm6000ModelEACL(fm6000_model *model);


/* Handles CSR parsing for EACL registers */
fm_status fm6000ModelEACLWriteCSR(fm6000_model *model,
                                  fm_uint32 addr,
                                  fm_uint32 val);


/* Implements the DMASK generation in L2F */
void fm6000ModelTransformDMASK(fm6000_model * model,
                               fm_uint64 *mask,
                               fm_int     cmdA,
                               fm_int     cmdB,
                               fm_int     dropSelectCode,
                               fm_int     dropCode);

/* Handles some post L2AR channel generation */
void fm6000ModelL2ARPost(fm6000_model *model);

/* LBS */
void fm6000ModelLBS(fm6000_model *model);

/* Handles CSR parsing for LBS registers */
fm_status fm6000ModelLBSWriteCSR(fm6000_model *model,
                                 fm_uint32 addr,
                                 fm_uint32 val);

/* Multicast */
fm_bool fm6000ModelMcast(fm6000_model *model);


/* Handles CSR parsing for MCAST registers */
fm_status fm6000ModelMcastWriteCSR(fm6000_model *model,
                                   fm_uint32 addr,
                                   fm_uint32 val);


/* Handles CSR parsing for CM registers */
fm_status fm6000ModelCMWriteCSR(fm6000_model *model, 
                                fm_uint32 addr,
                                fm_uint32 val);


/*  Parser */
void fm6000ModelParserPre(fm6000_model *model);
void fm6000ModelParserPost(fm6000_model *model);
void fm6000ModelParserLoadNextPayloadWord(fm6000_model *model);
fm_bool fm6000ModelParserCheckFrameTermination(fm6000_model *model,
                                               fm_uint64    *currentFLAGS,
                                               fm_uint32    sizeOfCurrentFLAGS);


/* Handles CSR parsing for parser registers */
fm_status fm6000ModelParserWriteCSR(fm6000_model *model,
                                    fm_uint32 addr,
                                    fm_uint32 val);


/* Initializes some channel inputs used by the mapper */
void fm6000ModelMapperPre(fm6000_model *model);

/* Process the QOS Mapping tables */
fm_status fm6000ModelMapperQOSMapping(fm6000_model *model);

/* Mapper post processing ahead of the FFU stage */
void fm6000ModelMapperPost(fm6000_model *model);

/* Handles CSR parsing for mapper registers */
fm_status fm6000ModelMapperWriteCSR(fm6000_model *model,
                                    fm_uint32 addr,
                                    fm_uint32 val);

/* Hashing */
void fm6000ModelComputeL2Hash(fm6000_model *model);
void fm6000ModelComputeL3Hash(fm6000_model *model);

/*  FFU */
void fm6000ModelFFU(fm6000_model *model);


/* Handles CSR parsing for FFU registers */
fm_status fm6000ModelFFUWriteCSR(fm6000_model *model,
                                 fm_uint32 addr,
                                 fm_uint32 val);


/* Initializes some STATS related channels prior to processing */
void fm6000ModelStatsPre(fm6000_model *model);


/* Clears the STATS state before each iteration of egress */
void fm6000ModelStatsReset(fm6000_model *model);


/* Computes the results of the STATS AR rules and increments as needed */
void fm6000ModelStatsCompute(fm6000_model *model);


/* Handles CSR parsing for STATS registers */
fm_status fm6000ModelStatsWriteCSR(fm6000_model *model,
                                   fm_uint32 addr,
                                   fm_uint32 val);


/*  Egress Modify */
fm_status fm6000ModelModifyInitialize(fm6000_model *model);
fm_int fm6000ModelModify(fm6000_model *model, fm_byte *packet, fm_int maxPktSize);
fm_status fm6000ModelModifyWriteCSR(fm6000_model *model,
                                    fm_uint32     addr,
                                    fm_uint32     val);
void fm6000ModelEnqueueModifyCommand(fm6000_model *model);
void fm6000ModelEnqueueModifyValue(fm6000_model *model);

/*  EPL */
void fm6000ModelEPL(fm6000_model *model);

/* Handles CSR parsing for EPL registers */
fm_status fm6000ModelEPLWriteCSR(fm6000_model *model,
                                 fm_uint32 addr,
                                 fm_uint32 val);

/* Periodic tick function for the EPL timers. */
fm_status fm6000ModelEPLTick(fm6000_model * model);

/* Captures ingress and egress timestamps. */
fm_status fm6000ModelCaptureTimestamps(fm6000_model *model,
                                       fm_byte *     packet,
                                       fm_int        length);

/* Handles frame padding. */
void fm6000ModelPadFrame(fm6000_model * model,
                         fm_byte *      packet,
                         fm_int *       length);

/* Handles computing the final CRC */
void fm6000ModelComputeEgressCRC(fm_int egressPort, 
                                 fm_int packetLength, 
                                 fm_byte *packet);

/* Handles FIBM processing */
fm_status fm6000ModelProcessFIBM(fm6000_model *model, fm_byte *packet, fm_int *length);

/* For register management */
fm_uint32 fm6000ModelGetRegisterDefault(fm_uint32 addr);
void fm6000ModelGetRegisterAccess(fm_uint32  addr,     
                                  fm_uint32 *rwMask,
                                  fm_uint32 *roMask,
                                  fm_uint32 *cwMask,
                                  fm_uint32 *cw1Mask,
                                  fm_uint32 *rvMask);

fm_status fm6000ModelMgmtWriteCSR(fm6000_model *model, 
                                  fm_uint32     addr, 
                                  fm_uint32     val);

#endif /* __FM6000_MODEL_TYPES_H */
