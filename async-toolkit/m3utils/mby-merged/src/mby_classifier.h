// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_CLASSIFIER_H
#define MBY_CLASSIFIER_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"

// Defines:

#define MBY_FFU_GROUP_BASE                                      (0x3000000)
#define MBY_FFU_GROUP_SIZE                                      (0x0300000)

#define MBY_FFU_TCAM_WIDTH                                      4
#define MBY_FFU_TCAM_ENTRIES_0                                  1024
#define MBY_FFU_TCAM_ENTRIES_1                                  16
#define MBY_FFU_TCAM_ENTRIES_2                                  3
#define MBY_FFU_TCAM(index2, index1, index0, word)              ((0x0100000) * ((index2) - 0)+(0x0004000) * ((index1) - 0) + (0x0000010) * ((index0) - 0) + ((word)*4)+ (0x0000000) + (MBY_FFU_GROUP_BASE))

#define MBY_FFU_TCAM_l__RSVD1_                                  104
#define MBY_FFU_TCAM_h__RSVD1_                                  127
#define MBY_FFU_TCAM_l_KEY_TOP_INVERT                           96
#define MBY_FFU_TCAM_h_KEY_TOP_INVERT                           103
#define MBY_FFU_TCAM_l_KEY_INVERT                               64
#define MBY_FFU_TCAM_h_KEY_INVERT                               95
#define MBY_FFU_TCAM_l__RSVD0_                                  40
#define MBY_FFU_TCAM_h__RSVD0_                                  63
#define MBY_FFU_TCAM_l_KEY_TOP                                  32
#define MBY_FFU_TCAM_h_KEY_TOP                                  39
#define MBY_FFU_TCAM_l_KEY                                      0
#define MBY_FFU_TCAM_h_KEY                                      31

#define MBY_FFU_ACTION_WIDTH                                    2
#define MBY_FFU_ACTION_ENTRIES_0                                1024
#define MBY_FFU_ACTION_ENTRIES_1                                20
#define MBY_FFU_ACTION_ENTRIES_2                                3
#define MBY_FFU_ACTION(index2, index1, index0, word)            ((0x0100000) * ((index2) - 0)+(0x0002000) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0040000) + (MBY_FFU_GROUP_BASE))

#define MBY_FFU_ACTION_l_ACTION1                                32
#define MBY_FFU_ACTION_h_ACTION1                                63
#define MBY_FFU_ACTION_l_ACTION0                                0
#define MBY_FFU_ACTION_h_ACTION0                                31

#define MBY_FFU_TCAM_CFG_WIDTH                                  2
#define MBY_FFU_TCAM_CFG_ENTRIES_0                              64
#define MBY_FFU_TCAM_CFG_ENTRIES_1                              16
#define MBY_FFU_TCAM_CFG_ENTRIES_2                              3
#define MBY_FFU_TCAM_CFG(index2, index1, index0, word)          ((0x0100000) * ((index2) - 0)+(0x0000200) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0080000) + (MBY_FFU_GROUP_BASE))

#define MBY_FFU_TCAM_CFG_l_CHUNK_MASK                           36
#define MBY_FFU_TCAM_CFG_h_CHUNK_MASK                           51
#define MBY_FFU_TCAM_CFG_b_START_COMPARE                        35
#define MBY_FFU_TCAM_CFG_b_START_SET                            34
#define MBY_FFU_TCAM_CFG_l_SELECT_TOP                           28
#define MBY_FFU_TCAM_CFG_h_SELECT_TOP                           33
#define MBY_FFU_TCAM_CFG_l_SELECT0                              21
#define MBY_FFU_TCAM_CFG_h_SELECT0                              27
#define MBY_FFU_TCAM_CFG_l_SELECT1                              14
#define MBY_FFU_TCAM_CFG_h_SELECT1                              20
#define MBY_FFU_TCAM_CFG_l_SELECT2                              7
#define MBY_FFU_TCAM_CFG_h_SELECT2                              13
#define MBY_FFU_TCAM_CFG_l_SELECT3                              0
#define MBY_FFU_TCAM_CFG_h_SELECT3                              6

#define MBY_FFU_ACTION_CFG_WIDTH                                4
#define MBY_FFU_ACTION_CFG_ENTRIES_0                            64
#define MBY_FFU_ACTION_CFG_ENTRIES_1                            3
#define MBY_FFU_ACTION_CFG(index1, index0, word)                ((0x0100000) * ((index1) - 0) + (0x0000010) * ((index0) - 0) + ((word)*4)+ (0x0082000) + (MBY_FFU_GROUP_BASE))

#define MBY_FFU_ACTION_CFG_b_ENABLE_19                          99
#define MBY_FFU_ACTION_CFG_b_ENABLE_18                          98
#define MBY_FFU_ACTION_CFG_b_ENABLE_17                          97
#define MBY_FFU_ACTION_CFG_b_ENABLE_16                          96
#define MBY_FFU_ACTION_CFG_b_ENABLE_15                          95
#define MBY_FFU_ACTION_CFG_b_ENABLE_14                          94
#define MBY_FFU_ACTION_CFG_b_ENABLE_13                          93
#define MBY_FFU_ACTION_CFG_b_ENABLE_12                          92
#define MBY_FFU_ACTION_CFG_b_ENABLE_11                          91
#define MBY_FFU_ACTION_CFG_b_ENABLE_10                          90
#define MBY_FFU_ACTION_CFG_b_ENABLE_9                           89
#define MBY_FFU_ACTION_CFG_b_ENABLE_8                           88
#define MBY_FFU_ACTION_CFG_b_ENABLE_7                           87
#define MBY_FFU_ACTION_CFG_b_ENABLE_6                           86
#define MBY_FFU_ACTION_CFG_b_ENABLE_5                           85
#define MBY_FFU_ACTION_CFG_b_ENABLE_4                           84
#define MBY_FFU_ACTION_CFG_b_ENABLE_3                           83
#define MBY_FFU_ACTION_CFG_b_ENABLE_2                           82
#define MBY_FFU_ACTION_CFG_b_ENABLE_1                           81
#define MBY_FFU_ACTION_CFG_b_ENABLE_0                           80
#define MBY_FFU_ACTION_CFG_l_INDEX_19                           76
#define MBY_FFU_ACTION_CFG_h_INDEX_19                           79
#define MBY_FFU_ACTION_CFG_l_INDEX_18                           72
#define MBY_FFU_ACTION_CFG_h_INDEX_18                           75
#define MBY_FFU_ACTION_CFG_l_INDEX_17                           68
#define MBY_FFU_ACTION_CFG_h_INDEX_17                           71
#define MBY_FFU_ACTION_CFG_l_INDEX_16                           64
#define MBY_FFU_ACTION_CFG_h_INDEX_16                           67
#define MBY_FFU_ACTION_CFG_l_INDEX_15                           60
#define MBY_FFU_ACTION_CFG_h_INDEX_15                           63
#define MBY_FFU_ACTION_CFG_l_INDEX_14                           56
#define MBY_FFU_ACTION_CFG_h_INDEX_14                           59
#define MBY_FFU_ACTION_CFG_l_INDEX_13                           52
#define MBY_FFU_ACTION_CFG_h_INDEX_13                           55
#define MBY_FFU_ACTION_CFG_l_INDEX_12                           48
#define MBY_FFU_ACTION_CFG_h_INDEX_12                           51
#define MBY_FFU_ACTION_CFG_l_INDEX_11                           44
#define MBY_FFU_ACTION_CFG_h_INDEX_11                           47
#define MBY_FFU_ACTION_CFG_l_INDEX_10                           40
#define MBY_FFU_ACTION_CFG_h_INDEX_10                           43
#define MBY_FFU_ACTION_CFG_l_INDEX_9                            36
#define MBY_FFU_ACTION_CFG_h_INDEX_9                            39
#define MBY_FFU_ACTION_CFG_l_INDEX_8                            32
#define MBY_FFU_ACTION_CFG_h_INDEX_8                            35
#define MBY_FFU_ACTION_CFG_l_INDEX_7                            28
#define MBY_FFU_ACTION_CFG_h_INDEX_7                            31
#define MBY_FFU_ACTION_CFG_l_INDEX_6                            24
#define MBY_FFU_ACTION_CFG_h_INDEX_6                            27
#define MBY_FFU_ACTION_CFG_l_INDEX_5                            20
#define MBY_FFU_ACTION_CFG_h_INDEX_5                            23
#define MBY_FFU_ACTION_CFG_l_INDEX_4                            16
#define MBY_FFU_ACTION_CFG_h_INDEX_4                            19
#define MBY_FFU_ACTION_CFG_l_INDEX_3                            12
#define MBY_FFU_ACTION_CFG_h_INDEX_3                            15
#define MBY_FFU_ACTION_CFG_l_INDEX_2                            8
#define MBY_FFU_ACTION_CFG_h_INDEX_2                            11
#define MBY_FFU_ACTION_CFG_l_INDEX_1                            4
#define MBY_FFU_ACTION_CFG_h_INDEX_1                            7
#define MBY_FFU_ACTION_CFG_l_INDEX_0                            0
#define MBY_FFU_ACTION_CFG_h_INDEX_0                            3

#define MBY_FFU_KEY16_BASE                0
#define MBY_FFU_KEY8_BASE                 ( MBY_FFU_KEY16_BASE + MBY_FFU_N_KEY16 )
#define MBY_FFU_KEY32_BASE                ( MBY_FFU_KEY8_BASE + MBY_FFU_N_KEY8 )

#define MBY_FFU_MAX_HASH_ENTRY_SIZE       64
#define MBY_FFU_MAX_HASH_ACTIONS          4
#define MBY_FFU_HASH_CAM_ETY_7_BITS_31_0  0
#define MBY_FFU_HASH_CAM_ETY_7_BITS_63_32 1
#define MBY_FFU_HASH_CAM_ETY_6_BITS_31_0  2
#define MBY_FFU_HASH_CAM_ETY_6_BITS_63_32 3

/* FFU Action entry encoding */
#define MBY_FFU_ACTIONS_PER_ENTRY         2
#define MBY_FFU_ACTION_PREC_WIDTH         3
#define MBY_FFU_ACTION_l_PREC             29
#define MBY_FFU_ACTION_h_PREC             31
#define MBY_FFU_ACTION_l_ENTRYTYPE        24
#define MBY_FFU_ACTION_h_ENTRYTYPE        28
#define MBY_FFU_ACTION_l_SET1_24B_INDEX   24
#define MBY_FFU_ACTION_h_SET1_24B_INDEX   27
#define MBY_FFU_ACTION_l_SET1_24B_VALUE   0
#define MBY_FFU_ACTION_h_SET1_24B_VALUE   23

#define MBY_FFU_ACTION_l_SET3_4B_INDEXC   22
#define MBY_FFU_ACTION_h_SET3_4B_INDEXC   26
#define MBY_FFU_ACTION_l_SET3_4B_INDEXB   17
#define MBY_FFU_ACTION_h_SET3_4B_INDEXB   21
#define MBY_FFU_ACTION_l_SET3_4B_INDEXA   12
#define MBY_FFU_ACTION_h_SET3_4B_INDEXA   16
#define MBY_FFU_ACTION_l_SET3_4B_VALUEC   8
#define MBY_FFU_ACTION_h_SET3_4B_VALUEC   11
#define MBY_FFU_ACTION_l_SET3_4B_VALUEB   4
#define MBY_FFU_ACTION_h_SET3_4B_VALUEB   7
#define MBY_FFU_ACTION_l_SET3_4B_VALUEA   0
#define MBY_FFU_ACTION_h_SET3_4B_VALUEA   3

#define MBY_FFU_ACTION_l_SET3_1B_INDEXC   16
#define MBY_FFU_ACTION_h_SET3_1B_INDEXC   21
#define MBY_FFU_ACTION_b_SET3_1B_VC       22
#define MBY_FFU_ACTION_b_SET3_1B_EC       23
#define MBY_FFU_ACTION_l_SET3_1B_INDEXB   8
#define MBY_FFU_ACTION_h_SET3_1B_INDEXB   13
#define MBY_FFU_ACTION_b_SET3_1B_VB       14
#define MBY_FFU_ACTION_b_SET3_1B_EB       15
#define MBY_FFU_ACTION_l_SET3_1B_INDEXA   0
#define MBY_FFU_ACTION_h_SET3_1B_INDEXA   5
#define MBY_FFU_ACTION_b_SET3_1B_VA       6
#define MBY_FFU_ACTION_b_SET3_1B_EA       7

#define MBY_FFU_ACTION_l_SET8_1B_ENABLE   8 
#define MBY_FFU_ACTION_h_SET8_1B_ENABLE   15
#define MBY_FFU_ACTION_l_SET8_1B_INDEX    16
#define MBY_FFU_ACTION_h_SET8_1B_INDEX    19
#define MBY_FFU_ACTION_l_SET8_1B_VALUE    0
#define MBY_FFU_ACTION_h_SET8_1B_VALUE    7 

#define MBY_FFU_ACTION_l_SET4_4B_ENABLE   20 
#define MBY_FFU_ACTION_h_SET4_4B_ENABLE   23
#define MBY_FFU_ACTION_l_SET4_4B_INDEX    16
#define MBY_FFU_ACTION_h_SET4_4B_INDEX    19
#define MBY_FFU_ACTION_l_SET4_4B_VALUE    0
#define MBY_FFU_ACTION_h_SET4_4B_VALUE    15

/* FFU Remap Entry Encoding */
#define MBY_FFU_REMAP_b_ENTRYTYPE         23
#define MBY_FFU_REMAP_l_SET8_1B_INDEX     16
#define MBY_FFU_REMAP_h_SET8_1B_INDEX     23
#define MBY_FFU_REMAP_l_SET8_1B_VALUE     8 
#define MBY_FFU_REMAP_h_SET8_1B_VALUE     15
#define MBY_FFU_REMAP_l_SET8_1B_MASK      0 
#define MBY_FFU_REMAP_h_SET8_1B_MASK      7

#define MBY_FFU_REMAP_l_SET1_16B_INDEX     16
#define MBY_FFU_REMAP_h_SET1_16B_INDEX     23
#define MBY_FFU_REMAP_l_SET1_16B_VALUE     0 
#define MBY_FFU_REMAP_h_SET1_16B_VALUE     15

#define MBY_FFU_HASH_ENTRY_MODE_32B   0
#define MBY_FFU_HASH_ENTRY_MODE_64B   1

/* Bit number for fields of FFU_SLICE_SRAM.RouteData */
/* Bit numbers when RouteType==GLORT */
#define MBY_FFU_ROUTE_l_DGLORT        0
#define MBY_FFU_ROUTE_h_DGLORT        15
#define MBY_FFU_ROUTE_b_FLOODSET      16

/* Bit numbers when RouteType==ARP */
#define MBY_FFU_ROUTE_l_ARP_INDEX     0
#define MBY_FFU_ROUTE_h_ARP_INDEX     15
#define MBY_FFU_ROUTE_l_GROUP_SIZE    16
#define MBY_FFU_ROUTE_h_GROUP_SIZE    19
#define MBY_FFU_ROUTE_b_GROUP_TYPE    20
#define MBY_FFU_ROUTE_b_ARP_ROUTE     21

/* Bit number for FFU Flags */
#define MBY_FFU_FLAGS_b_DROP          0
#define MBY_FFU_FLAGS_b_TRAP          1
#define MBY_FFU_FLAGS_b_LOG           2
#define MBY_FFU_FLAGS_b_NO_ROUTE      3
#define MBY_FFU_FLAGS_b_RX_MIRROR     4
#define MBY_FFU_FLAGS_b_CAPTURE_TIME  5

#define MBY_N_MA_HASH_KEYS            9

#define MBY_MA_TABLE_CAM_BANK         5
#define MBY_MA_TABLE_CAM_ENTRIES      1024

#define MBY_MA_ENTRY_TYPE_NOT_USED      0
#define MBY_MA_ENTRY_TYPE_PROVISIONAL   1
#define MBY_MA_ENTRY_TYPE_DYNAMIC       2
#define MBY_MA_ENTRY_TYPE_SECURE        3
#define MBY_MA_ENTRY_TYPE_STATIC        4
#define MBY_MA_ENTRY_TYPE_SECURE_STATIC 5

#define MBY_SV_MOVE_DROP_RESERVED     0
#define MBY_SV_MOVE_DROP_PORT         1
#define MBY_SV_MOVE_DROP_ADDR         2
#define MBY_SV_MOVE_DROP_STATIC       3

#define MBY_LOG_TYPE_TRIG_LOG_ACTION  (1 << 0)
#define MBY_LOG_TYPE_FFU              (1 << 1)
#define MBY_LOG_TYPE_RESERVED_MAC     (1 << 2)
#define MBY_LOG_TYPE_ARP_REDIRECT     (1 << 3)
#define MBY_LOG_TYPE_ICMP             (1 << 4)
#define MBY_LOG_TYPE_TTL_IP_MC        (1 << 5)
#define MBY_LOG_TYPE_IP_UCST_L2_MCST  (1 << 7) /* EAC TBR */

/* Trap codes which forms lower 8-bit of CPU-glort. */
/* See RRC Bug 22835.  Changed twice to match RTL change 271047. */
/* cpuCode changed to match RTL (src/rtl/hlp/common/hlp_pkg.vh) : */
#define MBY_CPU_CODE_FFU              0x0;
#define MBY_CPU_CODE_RSVD_MAC         0x1;
#define MBY_CPU_CODE_IGMP             0x2;
#define MBY_CPU_CODE_ICMP             0x3;
#define MBY_CPU_CODE_IP_OPTION        0x4;
#define MBY_CPU_CODE_CPU_ADDRESS      0x5;
#define MBY_CPU_CODE_MTU              0x6;
#define MBY_CPU_CODE_TTL              0x7;
#define MBY_CPU_CODE_MAX              0xF;
//efine MBY_CPU_CODE_FFU              0x80;
//efine MBY_CPU_CODE_RSVD_MAC         0x83;
//efine MBY_CPU_CODE_IGMP             0x86;
//efine MBY_CPU_CODE_ICMP             0x90;
//efine MBY_CPU_CODE_IP_OPTION        0x91;
//efine MBY_CPU_CODE_CPU_ADDRESS      0x92;
//efine MBY_CPU_CODE_MTU              0x94;
//efine MBY_CPU_CODE_TTL              0x96;
//efine MBY_CPU_CODE_MAX              0xFF;

#define MBY_IPV6_OPTION_HOP_BY_HOP     0
#define MBY_IPV6_OPTION_ROUTING       43
#define MBY_IPV6_OPTION_FRAG          44
#define MBY_IPV6_OPTION_DEST          60
#define MBY_IPV6_OPTION_AUTH          51

/* FFU mux selects */
#define MBY_FFU_SELECT_MAP_DIP_MAP_SIP       0
#define MBY_FFU_SELECT_MAP_DMAC_MAP_SMAC     1
#define MBY_FFU_SELECT_MAP_PROT_MAP_LENGTH   2
#define MBY_FFU_SELECT_MAP_SRC_MAP_TYPE      3
#define MBY_FFU_SELECT_USER                  4
#define MBY_FFU_SELECT_FTYPE_SWPRI           5
#define MBY_FFU_SELECT_IPMISC                6
#define MBY_FFU_SELECT_TOS                   7
#define MBY_FFU_SELECT_PROT                  8
#define MBY_FFU_SELECT_TTL                   9
#define MBY_FFU_SELECT_SRC_PORT             10
#define MBY_FFU_SELECT_VPRI_VID_11_8        11
#define MBY_FFU_SELECT_VID_7_0              12
#define MBY_FFU_SELECT_RXTAG                13
#define MBY_FFU_SELECT_L2_DMAC_15_0         14
#define MBY_FFU_SELECT_L2_DMAC_31_16        15
#define MBY_FFU_SELECT_L2_DMAC_47_32        16
#define MBY_FFU_SELECT_L2_SMAC_15_0         17
#define MBY_FFU_SELECT_L2_SMAC_31_16        18
#define MBY_FFU_SELECT_L2_SMAC_47_32        19
#define MBY_FFU_SELECT_DGLORT               20
#define MBY_FFU_SELECT_SGLORT               21
#define MBY_FFU_SELECT_VPRI_VID             22
#define MBY_FFU_SELECT_VPRI2_VID2           23
#define MBY_FFU_SELECT_L2_TYPE              24
#define MBY_FFU_SELECT_L4_DST               25
#define MBY_FFU_SELECT_L4_SRC               26
#define MBY_FFU_SELECT_MAP_L4_DST           27
#define MBY_FFU_SELECT_MAP_L4_SRC           28
#define MBY_FFU_SELECT_L4A                  29
#define MBY_FFU_SELECT_L4B                  30
#define MBY_FFU_SELECT_L4C                  31
#define MBY_FFU_SELECT_L4D                  32
#define MBY_FFU_SELECT_MAP_VPRI1_VID1       33
#define MBY_FFU_SELECT_L3_DIP_31_0          34
#define MBY_FFU_SELECT_L3_DIP_63_32         35
#define MBY_FFU_SELECT_L3_DIP_95_64         36
#define MBY_FFU_SELECT_L3_DIP_127_96        37
#define MBY_FFU_SELECT_L3_SIP_31_0          38
#define MBY_FFU_SELECT_L3_SIP_63_32         39
#define MBY_FFU_SELECT_L3_SIP_95_64         40
#define MBY_FFU_SELECT_L3_SIP_127_96        41

// --------------------------------------------------------------------------------

#define MBY_FGHASH_BASE                                         (0x3400000)
#define MBY_FGHASH_SIZE                                         (0x00C0000)

#define MBY_FFU_HASH_LOOKUP_WIDTH                               4
#define MBY_FFU_HASH_LOOKUP_ENTRIES_0                           8192
#define MBY_FFU_HASH_LOOKUP_ENTRIES_1                           3
#define MBY_FFU_HASH_LOOKUP(index1, index0, word)               ((0x0040000) * ((index1) - 0) + (0x0000010) * ((index0) - 0) + ((word)*4)+ (0x0000000) + (MBY_FGHASH_BASE))

#define MBY_FFU_HASH_LOOKUP_l_PTR                               64
#define MBY_FFU_HASH_LOOKUP_h_PTR                               83
#define MBY_FFU_HASH_LOOKUP_l_RSVD1_                            52
#define MBY_FFU_HASH_LOOKUP_h_RSVD1_                            63
#define MBY_FFU_HASH_LOOKUP_l_SELECT_4                          48
#define MBY_FFU_HASH_LOOKUP_h_SELECT_4                          51
#define MBY_FFU_HASH_LOOKUP_l_SELECT_3                          44
#define MBY_FFU_HASH_LOOKUP_h_SELECT_3                          47
#define MBY_FFU_HASH_LOOKUP_l_SELECT_2                          40
#define MBY_FFU_HASH_LOOKUP_h_SELECT_2                          43
#define MBY_FFU_HASH_LOOKUP_l_SELECT_1                          36
#define MBY_FFU_HASH_LOOKUP_h_SELECT_1                          39
#define MBY_FFU_HASH_LOOKUP_l_SELECT_0                          32
#define MBY_FFU_HASH_LOOKUP_h_SELECT_0                          35
#define MBY_FFU_HASH_LOOKUP_l_MASK                              0
#define MBY_FFU_HASH_LOOKUP_h_MASK                              31

#define MBY_FFU_HASH_CAM_WIDTH                                  2
#define MBY_FFU_HASH_CAM_ENTRIES_0                              8
#define MBY_FFU_HASH_CAM_ENTRIES_1                              32
#define MBY_FFU_HASH_CAM_ENTRIES_2                              3
#define MBY_FFU_HASH_CAM(index2, index1, index0, word)          ((0x0040000) * ((index2) - 0)+(0x0000040) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0020000) + (MBY_FGHASH_BASE))

#define MBY_FFU_HASH_CAM_l_DATA                                 0
#define MBY_FFU_HASH_CAM_h_DATA                                 63

#define MBY_FFU_HASH_CAM_EN_WIDTH                               2
#define MBY_FFU_HASH_CAM_EN_ENTRIES_0                           32
#define MBY_FFU_HASH_CAM_EN_ENTRIES_1                           2
#define MBY_FFU_HASH_CAM_EN_ENTRIES_2                           3
#define MBY_FFU_HASH_CAM_EN(index2, index1, index0, word)       ((0x0040000) * ((index2) - 0)+(0x0000100) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0020800) + (MBY_FGHASH_BASE))

#define MBY_FFU_HASH_CAM_EN_l_MASK                              0
#define MBY_FFU_HASH_CAM_EN_h_MASK                              63

#define MBY_FFU_KEY_MASK0_WIDTH                                 2
#define MBY_FFU_KEY_MASK0_ENTRIES_0                             64
#define MBY_FFU_KEY_MASK0_ENTRIES_1                             2
#define MBY_FFU_KEY_MASK0_ENTRIES_2                             3
#define MBY_FFU_KEY_MASK0(index2, index1, index0, word)         ((0x0040000) * ((index2) - 0)+(0x0000200) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0020C00) + (MBY_FGHASH_BASE))

#define MBY_FFU_KEY_MASK0_l_KEY8_MASK                           0
#define MBY_FFU_KEY_MASK0_h_KEY8_MASK                           63

#define MBY_FFU_KEY_MASK1_WIDTH                                 2
#define MBY_FFU_KEY_MASK1_ENTRIES_0                             64
#define MBY_FFU_KEY_MASK1_ENTRIES_1                             2
#define MBY_FFU_KEY_MASK1_ENTRIES_2                             3
#define MBY_FFU_KEY_MASK1(index2, index1, index0, word)         ((0x0040000) * ((index2) - 0)+(0x0000200) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0021000) + (MBY_FGHASH_BASE))

#define MBY_FFU_KEY_MASK1_l_KEY_SUBMODE1                        50
#define MBY_FFU_KEY_MASK1_h_KEY_SUBMODE1                        51
#define MBY_FFU_KEY_MASK1_l_KEY_SUBMODE0                        48
#define MBY_FFU_KEY_MASK1_h_KEY_SUBMODE0                        49
#define MBY_FFU_KEY_MASK1_l_KEY32_MASK                          32
#define MBY_FFU_KEY_MASK1_h_KEY32_MASK                          47
#define MBY_FFU_KEY_MASK1_l_KEY16_MASK                          0
#define MBY_FFU_KEY_MASK1_h_KEY16_MASK                          31

#define MBY_FFU_KEY_MASK2_WIDTH                                 2
#define MBY_FFU_KEY_MASK2_ENTRIES_0                             64
#define MBY_FFU_KEY_MASK2_ENTRIES_1                             2
#define MBY_FFU_KEY_MASK2_ENTRIES_2                             3
#define MBY_FFU_KEY_MASK2(index2, index1, index0, word)         ((0x0040000) * ((index2) - 0)+(0x0000200) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0021400) + (MBY_FGHASH_BASE))

#define MBY_FFU_KEY_MASK2_l_KEY_SUBMASK1                        32
#define MBY_FFU_KEY_MASK2_h_KEY_SUBMASK1                        63
#define MBY_FFU_KEY_MASK2_l_KEY_SUBMASK0                        0
#define MBY_FFU_KEY_MASK2_h_KEY_SUBMASK0                        31

#define MBY_FFU_HASH_MISS_WIDTH                                 2
#define MBY_FFU_HASH_MISS_ENTRIES_0                             64
#define MBY_FFU_HASH_MISS_ENTRIES_1                             2
#define MBY_FFU_HASH_MISS_ENTRIES_2                             3
#define MBY_FFU_HASH_MISS(index2, index1, index0, word)         ((0x0040000) * ((index2) - 0)+(0x0000200) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0021800) + (MBY_FGHASH_BASE))

#define MBY_FFU_HASH_MISS_l_ACTION1                             32
#define MBY_FFU_HASH_MISS_h_ACTION1                             63
#define MBY_FFU_HASH_MISS_l_ACTION0                             0
#define MBY_FFU_HASH_MISS_h_ACTION0                             31

#define MBY_FFU_HASH_CFG_WIDTH                                  2
#define MBY_FFU_HASH_CFG_ENTRIES_0                              64
#define MBY_FFU_HASH_CFG_ENTRIES_1                              3
#define MBY_FFU_HASH_CFG(index1, index0, word)                  ((0x0040000) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0021C00) + (MBY_FGHASH_BASE))

#define MBY_FFU_HASH_CFG_b_MODE                                 46
#define MBY_FFU_HASH_CFG_l_BASE_PTR_0                           33
#define MBY_FFU_HASH_CFG_h_BASE_PTR_0                           45
#define MBY_FFU_HASH_CFG_l_BASE_PTR_1                           20
#define MBY_FFU_HASH_CFG_h_BASE_PTR_1                           32
#define MBY_FFU_HASH_CFG_l_HASH_SIZE_0                          15
#define MBY_FFU_HASH_CFG_h_HASH_SIZE_0                          19
#define MBY_FFU_HASH_CFG_l_HASH_SIZE_1                          10
#define MBY_FFU_HASH_CFG_h_HASH_SIZE_1                          14
#define MBY_FFU_HASH_CFG_l_ENTRY_SIZE_0                         5
#define MBY_FFU_HASH_CFG_h_ENTRY_SIZE_0                         9
#define MBY_FFU_HASH_CFG_l_ENTRY_SIZE_1                         0
#define MBY_FFU_HASH_CFG_h_ENTRY_SIZE_1                         4

/******** HASH_ENTRY_RAM_BASE *******/
#define MBY_HASH_ENTRY_RAM_BASE                                 (0x3500000)
#define MBY_HASH_ENTRY_RAM_SIZE                                 (0x0080000)

#define MBY_HASH_ENTRY0_WIDTH                                   2
#define MBY_HASH_ENTRY0_ENTRIES                                 65536
#define MBY_HASH_ENTRY0(index, word)                            ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0000000) + (MBY_HASH_ENTRY_RAM_BASE))

#define MBY_HASH_ENTRY0_l_DATA                                  0
#define MBY_HASH_ENTRY0_h_DATA                                  63

#define MBY_HASH_ENTRY1_WIDTH                                   2
#define MBY_HASH_ENTRY1_ENTRIES                                 65536
#define MBY_HASH_ENTRY1(index, word)                            ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0080000) + (MBY_HASH_ENTRY_RAM_BASE))

#define MBY_HASH_ENTRY1_l_DATA                                  0
#define MBY_HASH_ENTRY1_h_DATA                                  63

#define MBY_HASH_ENTRY_RAM_ALLOC_WIDTH                          2
#define MBY_HASH_ENTRY_RAM_ALLOC_ENTRIES                        2
#define MBY_HASH_ENTRY_RAM_ALLOC(index, word)                   ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0100000) + (MBY_HASH_ENTRY_RAM_BASE))

#define MBY_HASH_ENTRY_RAM_ALLOC_l_GP_SEL                       0
#define MBY_HASH_ENTRY_RAM_ALLOC_h_GP_SEL                       7

#define MBY_HASH_ENTRY_RAM_ERR_WRITE_WIDTH                      2
#define MBY_HASH_ENTRY_RAM_ERR_WRITE_ENTRIES_0                  2
#define MBY_HASH_ENTRY_RAM_ERR_WRITE_ENTRIES_1                  2
#define MBY_HASH_ENTRY_RAM_ERR_WRITE(index1, index0, word)      ((0x0000010) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0100020) + (MBY_HASH_ENTRY_RAM_BASE))

#define MBY_HASH_ENTRY_RAM_ERR_WRITE_l_ERR_INJECT               0
#define MBY_HASH_ENTRY_RAM_ERR_WRITE_h_ERR_INJECT               1

#define MBY_HASH_ENTRY_RAM_CERR_READ_WIDTH                      2
#define MBY_HASH_ENTRY_RAM_CERR_READ_ENTRIES_0                  2
#define MBY_HASH_ENTRY_RAM_CERR_READ_ENTRIES_1                  2
#define MBY_HASH_ENTRY_RAM_CERR_READ(index1, index0, word)      ((0x0000010) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0100040) + (MBY_HASH_ENTRY_RAM_BASE))

#define MBY_HASH_ENTRY_RAM_CERR_READ_l_COL                      16
#define MBY_HASH_ENTRY_RAM_CERR_READ_h_COL                      18
#define MBY_HASH_ENTRY_RAM_CERR_READ_l_ADDR                     5
#define MBY_HASH_ENTRY_RAM_CERR_READ_h_ADDR                     15
#define MBY_HASH_ENTRY_RAM_CERR_READ_l_LANE                     3
#define MBY_HASH_ENTRY_RAM_CERR_READ_h_LANE                     4
#define MBY_HASH_ENTRY_RAM_CERR_READ_l_RSVD                     1
#define MBY_HASH_ENTRY_RAM_CERR_READ_h_RSVD                     2
#define MBY_HASH_ENTRY_RAM_CERR_READ_b_ERROR                    0

#define MBY_HASH_ENTRY_RAM_UERR_READ_WIDTH                      2
#define MBY_HASH_ENTRY_RAM_UERR_READ_ENTRIES_0                  2
#define MBY_HASH_ENTRY_RAM_UERR_READ_ENTRIES_1                  2
#define MBY_HASH_ENTRY_RAM_UERR_READ(index1, index0, word)      ((0x0000010) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0100060) + (MBY_HASH_ENTRY_RAM_BASE))

#define MBY_HASH_ENTRY_RAM_UERR_READ_l_COL                      16
#define MBY_HASH_ENTRY_RAM_UERR_READ_h_COL                      18
#define MBY_HASH_ENTRY_RAM_UERR_READ_l_ADDR                     5
#define MBY_HASH_ENTRY_RAM_UERR_READ_h_ADDR                     15
#define MBY_HASH_ENTRY_RAM_UERR_READ_l_LANE                     3
#define MBY_HASH_ENTRY_RAM_UERR_READ_h_LANE                     4
#define MBY_HASH_ENTRY_RAM_UERR_READ_l_RSVD                     1
#define MBY_HASH_ENTRY_RAM_UERR_READ_h_RSVD                     2
#define MBY_HASH_ENTRY_RAM_UERR_READ_b_ERROR                    0

#define MBY_HASH_ENTRY_RAM_CERR_CNT_WIDTH                       2
#define MBY_HASH_ENTRY_RAM_CERR_CNT_ENTRIES_0                   2
#define MBY_HASH_ENTRY_RAM_CERR_CNT_ENTRIES_1                   2
#define MBY_HASH_ENTRY_RAM_CERR_CNT(index1, index0, word)       ((0x0000010) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x01000C0) + (MBY_HASH_ENTRY_RAM_BASE))

#define MBY_HASH_ENTRY_RAM_CERR_CNT_l__RSVD_                    12
#define MBY_HASH_ENTRY_RAM_CERR_CNT_h__RSVD_                    31
#define MBY_HASH_ENTRY_RAM_CERR_CNT_l_COUNTER                   0
#define MBY_HASH_ENTRY_RAM_CERR_CNT_h_COUNTER                   11

#define MBY_HASH_ENTRY_RAM_UERR_CNT_WIDTH                       2
#define MBY_HASH_ENTRY_RAM_UERR_CNT_ENTRIES_0                   2
#define MBY_HASH_ENTRY_RAM_UERR_CNT_ENTRIES_1                   2
#define MBY_HASH_ENTRY_RAM_UERR_CNT(index1, index0, word)       ((0x0000010) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x01000E0) + (MBY_HASH_ENTRY_RAM_BASE))

#define MBY_HASH_ENTRY_RAM_UERR_CNT_l__RSVD_                    12
#define MBY_HASH_ENTRY_RAM_UERR_CNT_h__RSVD_                    31
#define MBY_HASH_ENTRY_RAM_UERR_CNT_l_COUNTER                   0
#define MBY_HASH_ENTRY_RAM_UERR_CNT_h_COUNTER                   11

/******** MPLS_MUX_BASE *******/
#define MBY_MPLS_MUX_BASE                                       (0x37E1000)
#define MBY_MPLS_MUX_SIZE                                       (0x0008000)

#define MBY_MPLS_MUX_EXP_DS_WIDTH                               2
#define MBY_MPLS_MUX_EXP_DS_ENTRIES                             256
#define MBY_MPLS_MUX_EXP_DS(index, word)                        ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0000000) + (MBY_MPLS_MUX_BASE))

#define MBY_MPLS_MUX_EXP_DS_l_DSCP                              6
#define MBY_MPLS_MUX_EXP_DS_h_DSCP                              11
#define MBY_MPLS_MUX_EXP_DS_l_ECN                               4
#define MBY_MPLS_MUX_EXP_DS_h_ECN                               5
#define MBY_MPLS_MUX_EXP_DS_l_TC                                0
#define MBY_MPLS_MUX_EXP_DS_h_TC                                3

#define MBY_MPLS_MUX_DSCP_TC_WIDTH                              2
#define MBY_MPLS_MUX_DSCP_TC_ENTRIES                            64
#define MBY_MPLS_MUX_DSCP_TC(index, word)                       ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0000800) + (MBY_MPLS_MUX_BASE))

#define MBY_MPLS_MUX_DSCP_TC_l_TC                               0
#define MBY_MPLS_MUX_DSCP_TC_h_TC                               3

/******** ENTROPY_BASE *******/
#define MBY_ENTROPY_BASE                                        (0x37E0000)
#define MBY_ENTROPY_SIZE                                        (0x0000100)

#define MBY_ENTROPY_HASH_CFG0_WIDTH                             2
#define MBY_ENTROPY_HASH_CFG0_ENTRIES_0                         64
#define MBY_ENTROPY_HASH_CFG0_ENTRIES_1                         2
#define MBY_ENTROPY_HASH_CFG0(index1, index0, word)             ((0x0000200) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0000000) + (MBY_ENTROPY_BASE))

#define MBY_ENTROPY_HASH_CFG0_l_KEY_MASK8                       0
#define MBY_ENTROPY_HASH_CFG0_h_KEY_MASK8                       63

#define MBY_ENTROPY_HASH_CFG1_WIDTH                             2
#define MBY_ENTROPY_HASH_CFG1_ENTRIES_0                         64
#define MBY_ENTROPY_HASH_CFG1_ENTRIES_1                         2
#define MBY_ENTROPY_HASH_CFG1(index1, index0, word)             ((0x0000200) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0000400) + (MBY_ENTROPY_BASE))

#define MBY_ENTROPY_HASH_CFG1_l_KEY_SUBMODE1                    50
#define MBY_ENTROPY_HASH_CFG1_h_KEY_SUBMODE1                    51
#define MBY_ENTROPY_HASH_CFG1_l_KEY_SUBMODE0                    48
#define MBY_ENTROPY_HASH_CFG1_h_KEY_SUBMODE0                    49
#define MBY_ENTROPY_HASH_CFG1_l_KEY_MASK32                      32
#define MBY_ENTROPY_HASH_CFG1_h_KEY_MASK32                      47
#define MBY_ENTROPY_HASH_CFG1_l_KEY_MASK16                      0
#define MBY_ENTROPY_HASH_CFG1_h_KEY_MASK16                      31

#define MBY_ENTROPY_HASH_CFG2_WIDTH                             2
#define MBY_ENTROPY_HASH_CFG2_ENTRIES_0                         64
#define MBY_ENTROPY_HASH_CFG2_ENTRIES_1                         2
#define MBY_ENTROPY_HASH_CFG2(index1, index0, word)             ((0x0000200) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0000800) + (MBY_ENTROPY_BASE))

#define MBY_ENTROPY_HASH_CFG2_l_KEY_SUBMASK1                    32
#define MBY_ENTROPY_HASH_CFG2_h_KEY_SUBMASK1                    63
#define MBY_ENTROPY_HASH_CFG2_l_KEY_SUBMASK0                    0
#define MBY_ENTROPY_HASH_CFG2_h_KEY_SUBMASK0                    31

#define MBY_ENTROPY_META_CFG_WIDTH                              2
#define MBY_ENTROPY_META_CFG_ENTRIES                            64
#define MBY_ENTROPY_META_CFG(index, word)                       ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0000C00) + (MBY_ENTROPY_BASE))

#define MBY_ENTROPY_META_CFG_l_BYTE_DEFAULTS                    12
#define MBY_ENTROPY_META_CFG_h_BYTE_DEFAULTS                    23
#define MBY_ENTROPY_META_CFG_l_HASH_START                       6
#define MBY_ENTROPY_META_CFG_h_HASH_START                       11
#define MBY_ENTROPY_META_CFG_l_HASH_SIZE                        0
#define MBY_ENTROPY_META_CFG_h_HASH_SIZE                        5

// Enums:

typedef enum mbyClassifierActionEntryTypeEnum
{
    MBY_FFU_ACTION_NOP = 0,
    MBY_FFU_ACTION_SET4_4B,
    MBY_FFU_ACTION_SET8_1B,
    MBY_FFU_ACTION_SET3_1B,
    MBY_FFU_ACTION_SET3_4B,
    MBY_FFU_ACTION_SET1_24B

} mbyClassifierActionEntryType;

typedef enum mbyClassifierAct1Enum
{
    MBY_FFU_ACTION_DROP                   =  0,
    MBY_FFU_ACTION_TRAP                   =  1,
    MBY_FFU_ACTION_LOG                    =  2,
    MBY_FFU_ACTION_NO_ROUTE               =  3,
    MBY_FFU_ACTION_RX_MIRROR              =  4,
    MBY_FFU_ACTION_CAPT_TIME              =  5,
    MBY_FFU_ACTION_TX_TAG0                =  6,
    MBY_FFU_ACTION_TX_TAG1                =  7,
    MBY_FFU_ACTION_TRIGGER0               =  8,
    MBY_FFU_ACTION_TRIGGER1               =  9,
    MBY_FFU_ACTION_TRIGGER2               = 10,
    MBY_FFU_ACTION_TRIGGER3               = 11,
    MBY_FFU_ACTION_TRIGGER4               = 12,
    MBY_FFU_ACTION_TRIGGER5               = 13,
    MBY_FFU_ACTION_TRIGGER6               = 14,
    MBY_FFU_ACTION_TRIGGER7               = 15,
    MBY_FFU_ACTION_SCENARIO0              = 16,
    MBY_FFU_ACTION_SCENARIO1              = 17,
    MBY_FFU_ACTION_SCENARIO2              = 18,
    MBY_FFU_ACTION_SCENARIO3              = 19,
    MBY_FFU_ACTION_SCENARIO4              = 20,
    MBY_FFU_ACTION_SCENARIO5              = 21,
    MBY_FFU_ACTION_LEARN                  = 22,
    MBY_FFU_ACTION_COPY_OTR_VPRI          = 23

} mbyClassifierAct1;

typedef enum mbyClasifierAct4Enum
{
    MBY_FFU_ACTION_DSCP_CTRL              =  0,
    MBY_FFU_ACTION_TTL_CTRL               =  1,
    MBY_FFU_ACTION_TC_CTRL                =  2,
    MBY_FFU_ACTION_ECN_CTRL               =  3,
    MBY_FFU_ACTION_VID_LOW                =  4,
    MBY_FFU_ACTION_VID_MID                =  5,
    MBY_FFU_ACTION_VID_HIGH               =  6,
    MBY_FFU_ACTION_VPRI_LOW               =  7,
    MBY_FFU_ACTION_DSCP_LOW               =  8,
    MBY_FFU_ACTION_DSCP_HIGH              =  9,
    MBY_FFU_ACTION_TC                     = 10,
    // alternate VPRI address when combining DSCP/SWPRI_CTRL/VPRI actions
    MBY_FFU_ACTION_VPRI_HIGH              = 11,     
    MBY_FFU_ACTION_HASH_PROFILE_ECMP_0    = 12,
    MBY_FFU_ACTION_HASH_PROFILE_ECMP_1    = 13,
    MBY_FFU_ACTION_HASH_PROFILE_MOD_0     = 14,
    MBY_FFU_ACTION_HASH_PROFILE_MOD_1     = 15,
    MBY_FFU_ACTION_META0_LOW              = 16,
    MBY_FFU_ACTION_META0_HIGH             = 17,
    MBY_FFU_ACTION_META1_LOW              = 18,
    MBY_FFU_ACTION_META1_HIGH             = 19,
    MBY_FFU_ACTION_META2_LOW              = 20,
    MBY_FFU_ACTION_META2_HIGH             = 21,
    MBY_FFU_ACTION_MPLS_POP               = 22

} mbyClasifierAct4;

typedef enum mbyClassifierAct24Enum
{
    MBY_FFU_ACTION_POLICER0               =  0,
    MBY_FFU_ACTION_POLICER1               =  1,
    MBY_FFU_ACTION_POLICER2               =  2,
    MBY_FFU_ACTION_POLICER3               =  3,
    MBY_FFU_ACTION_FWD                    =  4,
    MBY_FFU_ACTION_MOD_IDX                =  5,
    MBY_FFU_ACTION_REMAP0                 =  6,
    MBY_FFU_ACTION_REMAP1                 =  7,
    MBY_FFU_ACTION_REMAP2                 =  8,
    MBY_FFU_ACTION_REMAP3                 =  9,
    MBY_FFU_ACTION_REMAP4                 = 10,
    MBY_FFU_ACTION_REMAP5                 = 11,
    MBY_FFU_ACTION_REMAP6                 = 12,
    MBY_FFU_ACTION_REMAP7                 = 13

} mbyClassifierAct24;

typedef enum mbyClassifierKey8Enum
{
    MBY_FFU_KEY8_MPLS_LABEL5_0 = 0,
    MBY_FFU_KEY8_MPLS_LABEL5_1 = 1,
    MBY_FFU_KEY8_MPLS_LABEL5_2 = 2,
    MBY_FFU_KEY8_MPLS_LABEL5_3 = 3,
    MBY_FFU_KEY8_MPLS_LABEL6_0 = 4,
    MBY_FFU_KEY8_MPLS_LABEL6_1 = 5,
    MBY_FFU_KEY8_MPLS_LABEL6_2 = 6,
    MBY_FFU_KEY8_MPLS_LABEL6_3 = 7,
    MBY_FFU_KEY8_INNER_TTL     = 8,
    MBY_FFU_KEY8_INNER_PROT    = 9,
    MBY_FFU_KEY8_INNER_LEN     = 10,
    MBY_FFU_KEY8_INNER_DS      = 12,
    MBY_FFU_KEY8_OUTER_TTL     = 20,
    MBY_FFU_KEY8_OUTER_PROT    = 21,
    MBY_FFU_KEY8_OUTER_LEN     = 22,
    MBY_FFU_KEY8_OUTER_DS      = 24

} mbyClassifierKey8;

typedef enum mbyClassifierKey16Enum
{
    MBY_FFU_KEY16_OUTER_VLAN1     = 14,
    MBY_FFU_KEY16_INNER_VLAN1     = 20,
    MBY_FFU_KEY16_MPLS_LABEL1_0   = 24,
    MBY_FFU_KEY16_MPLS_LABEL1_1   = 25,
    MBY_FFU_KEY16_MPLS_LABEL2_0   = 26,
    MBY_FFU_KEY16_MPLS_LABEL2_1   = 27,
    MBY_FFU_KEY16_MPLS_LABEL3_0   = 28,
    MBY_FFU_KEY16_MPLS_LABEL3_1   = 29,
    MBY_FFU_KEY16_MPLS_LABEL4_0   = 30,
    MBY_FFU_KEY16_MPLS_LABEL4_1   = 31

} mbyClassifierKey16;

typedef enum mbyClassifierKey32Enum
{
    MBY_FFU_KEY32_OUTER_SIP_127_96    = 0,
    MBY_FFU_KEY32_OUTER_SIP_95_64     = 1,
    MBY_FFU_KEY32_OUTER_SIP_63_32     = 2,
    MBY_FFU_KEY32_OUTER_SIP_31_0      = 3,
    MBY_FFU_KEY32_OUTER_DIP_127_96    = 4,
    MBY_FFU_KEY32_OUTER_DIP_95_64     = 5,
    MBY_FFU_KEY32_OUTER_DIP_63_32     = 6,
    MBY_FFU_KEY32_OUTER_DIP_31_0      = 7,
    MBY_FFU_KEY32_INNER_SIP_127_96    = 8,
    MBY_FFU_KEY32_INNER_SIP_95_64     = 9,
    MBY_FFU_KEY32_INNER_SIP_63_32     = 10,
    MBY_FFU_KEY32_INNER_SIP_31_0      = 11,
    MBY_FFU_KEY32_INNER_DIP_127_96    = 12,
    MBY_FFU_KEY32_INNER_DIP_95_64     = 13,
    MBY_FFU_KEY32_INNER_DIP_63_32     = 14,
    MBY_FFU_KEY32_INNER_DIP_31_0      = 15

} mbyClassifierKey32;

// Structs:

typedef struct mbyClassifierTcamCfgStruct
{
    fm_uint16               CHUNK_MASK;
    fm_bool                 START_COMPARE;
    fm_bool                 START_SET;
    fm_byte                 SELECT_TOP;
    fm_byte                 SELECT0;
    fm_byte                 SELECT1;
    fm_byte                 SELECT2;
    fm_byte                 SELECT3;

} mbyClassifierTcamCfg;

typedef struct mbyClassifierTcamStruct
{
    fm_uint32               _RSVD1_;
    fm_byte                 KEY_TOP_INVERT;
    fm_uint32               KEY_INVERT;
    fm_uint32               _RSVD0_;
    fm_byte                 KEY_TOP;
    fm_uint32               KEY;

} mbyClassifierTcam;

typedef struct mbyClassifierTcamEntryStruct
{
    fm_uint64               Key;
    fm_uint64               KeyInvert;

} mbyClassifierTcamEntry;

typedef struct mbyClassifierHashLookupStruct
{
    fm_uint32               PTR;
    fm_uint16               RSVD1_;
    fm_byte                 SELECT_4;
    fm_byte                 SELECT_3;
    fm_byte                 SELECT_2;
    fm_byte                 SELECT_1;
    fm_byte                 SELECT_0;
    fm_uint32               MASK;

} mbyClassifierHashLookup;

typedef struct mbyLookupInfoStruct
{
    fm_uint64               key; // 40b field
    fm_uint64               keyInvert; // 40b field
    fm_bool                 rawHits[MBY_FFU_TCAM_ENTRIES_0];
    fm_int                  hitIndex;
    fm_bool                 hitIndexValid;

} mbyLookupInfo;

typedef struct mbyClassifierHitInfoStruct
{
    fm_int                  hitIndex;
    fm_bool                 hitIndexValid;

} mbyClassifierHitInfo;

typedef struct mbyClassifierKeyMaskCfgStruct
{
    fm_uint32               Key16Mask;
    fm_uint64               Key8Mask;
    fm_uint16               Key32Mask;
    fm_byte                 KeySubmode[2]; // 2b field
    fm_uint32               KeySubmask[2];

} mbyClassifierKeyMaskCfg;

typedef struct mbyClassifierHashCfgStruct
{
    fm_bool                 mode;
    fm_uint16               base_ptr  [2]; // 13b field
    fm_byte                 hash_size [2]; // 5b field
    fm_byte                 entry_size[2]; // 5b field

} mbyClassifierHashCfg;

typedef struct mbyMplsMuxExpDsStruct
{
    fm_byte                 DSCP;
    fm_byte                 ECN;
    fm_byte                 TC;

} mbyMplsMuxExpDs;

typedef struct mbyMplsMuxDscpTc
{
    fm_byte                 TC;

} mbyMplsMuxDscpTc;

typedef struct mbyEntropyMetaCfg
{
    fm_uint16               BYTE_DEFAULTS;
    fm_byte                 HASH_START;
    fm_byte                 HASH_SIZE;

} mbyEntropyMetaCfg;

typedef struct mbyClassifierMuxedActionStruct
{    
    fm_byte                 ecn;
    fm_bool                 aqm_mark_en;
    fm_byte                 swpri;
    fm_byte                 ttl_ctrl;
    fm_byte                 ttl01;
    fm_byte                 dscp;
    fm_byte                 vpri;
    fm_bool                 route;

} mbyClassifierMuxedAction;

typedef struct mbyClassifierFlags
{   
    fm_bool                 drop;
    fm_bool                 trap;
    fm_bool                 log;
    fm_bool                 no_route;
    fm_bool                 rx_mirror;
    fm_bool                 capture_time;
    fm_byte                 tx_tag;

} mbyClassifierFlags;

typedef struct mbyIppRxTagStruct
{
    fm_bool                 custom; 
    fm_bool                 mpls;   
    fm_bool                 ipv6;   
    fm_bool                 ipv4;   
    fm_bool                 v2first;
    fm_bool                 vlan2;
    fm_bool                 vlan1; 

} mbyRxTag;

typedef struct mbyClassifierToHashStruct
{
    // Hash value to be used by ARP_TABLE
    fm_uint64               L34_HASH;

    // Ingress L2 domain
    fm_uint16               L2_IDOMAIN;

    // Ingress L3 domain
    fm_byte                 L3_IDOMAIN;

    // Index into the MODIFY descriptor tables
    fm_uint32               MOD_IDX;

    // The 16-bit innermost Ethernet type:
    fm_uint16               L2_ETYPE;

    // # of MPLS labels to pop in Modify:
    fm_byte                 MPLS_POP;

    // Encapsulate flag:
    fm_bool                 ENCAP;

    // Decapsulate flag:
    fm_bool                 DECAP;

    // The 4-bit Switch priority:
    fm_byte                 QOS_SWPRI;

    // The 16-bit source GLORT:
    fm_uint16               SGLORT;

    // The 16-bit ingress destination GLORT:
    fm_uint16               IDGLORT;

    // The Layer 2 destination address:
    fm_macaddr              L2_DMAC;

    // The Layer 2 source address:
    fm_macaddr              L2_SMAC;

    // DMAC embedded in DIP of IPV6 packet:
    fm_macaddr              DMAC_FROM_IPV6;

    // Boolean indicating whether the packet is IPv4:
    fm_bool                 IS_IPV4;

    // Boolean indicating whether the packet is IPv6:
    fm_bool                 IS_IPV6;

    // The 16-bit IPv4 datagram length for outer/inner header:
    fm_uint16               L3_LENGTH;

    // The 16-bit IPv4 datagram length for outer header (including the IPv4
    //header and payload) or 16-bit IPv6 payload length (including any
    // extension headers) in units of octets:
    fm_uint16               OUTER_L3_LENGTH;

    // The 16-bit IPv4 datagram length for Inner header (including the IPv4
    // header and payload) or 16-bit IPv6 payload length (including any
    // extension headers) in units of octets:
    fm_uint16               INNER_L3_LENGTH;

    // Flag to indicate presence of IP options:
    fm_bool                 TRAP_IP_OPTIONS;

    // Packet drop flag depending on TTL value:
    fm_bool                 DROP_TTL;

    // Boolean indicating whether this ICMP packet should be trapped:
    fm_bool                 TRAP_ICMP;

    // Boolean indicating whether this IGMP packet should be trapped:
    fm_bool                 TRAP_IGMP;

    // Controls update of TTL field of egress packet:
    fm_byte                 TTL_CTRL;

    // The 6-bit set of FFU flags. Bits [5:0] contain
    //   {CAPTURE-TIME, RX_MIRROR, NO_ROUTE, LOG, TRAP, DROP}:
    mbyClassifierFlags      FFU_FLAGS;

    // Classifier route:
    fm_uint32               FFU_ROUTE;

    // Boolean indicating no learning:
    fm_bool                 NO_LEARN;

    // The 12-bit ingress VLAN ID:
    fm_uint16               L2_IVID1;

    // Transmit tag:
    fm_byte                 TX_TAG;

    // The 4-bit QOS VLAN priority:
    fm_byte                 QOS_L2_VPRI1;

    // Classifier action triggers:
    fm_byte                 FFU_TRIG;

    // The 6-bit QOS Differentiated Services Code Point (DSCP):
    fm_byte                 QOS_L3_DSCP;

    // Policer action set by FFU to be used policers:
    fm_uint32               POLICER_ACTION[4];

    /* Boolean indicating whether a parity error has been detected in any
     * of the memories while processing this packet. */
    fm_bool                 PARITY_ERROR;

    // ECN value to use in egress packet:
    fm_byte                 ECN;

    // AQM_MARK_EN to use in egress packet:
    fm_byte                 AQM_MARK_EN;

} mbyClassifierToHash;
    
#endif

