// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_MASKGEN_H
#define MBY_MASKGEN_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"

// Defines:

/******** FWD_MISC_BASE *******/
#define MBY_FWD_MISC_BASE                                       (0x3A40000)
#define MBY_FWD_MISC_SIZE                                       (0x0010000)

#define MBY_FWD_PORT_CFG_2_WIDTH                                2
#define MBY_FWD_PORT_CFG_2_ENTRIES                              512
#define MBY_FWD_PORT_CFG_2(index, word)                         ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0000000) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_PORT_CFG_2_l_DESTINATION_MASK                   0
#define MBY_FWD_PORT_CFG_2_h_DESTINATION_MASK                   23

#define MBY_FWD_LAG_CFG_WIDTH                                   2
#define MBY_FWD_LAG_CFG_ENTRIES                                 24
#define MBY_FWD_LAG_CFG(index, word)                            ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0001000) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_LAG_CFG_b_IN_LAG                                9
#define MBY_FWD_LAG_CFG_b_HASH_ROTATION                         8
#define MBY_FWD_LAG_CFG_l_INDEX                                 4
#define MBY_FWD_LAG_CFG_h_INDEX                                 7
#define MBY_FWD_LAG_CFG_l_LAG_SIZE                              0
#define MBY_FWD_LAG_CFG_h_LAG_SIZE                              3

#define MBY_FWD_PORT_CFG_1_WIDTH                                2
#define MBY_FWD_PORT_CFG_1_ENTRIES                              24
#define MBY_FWD_PORT_CFG_1(index, word)                         ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0001100) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_PORT_CFG_1_b_LEARNING_ENABLE                    25
#define MBY_FWD_PORT_CFG_1_b_FILTER_VLAN_INGRESS                24
#define MBY_FWD_PORT_CFG_1_l_DESTINATION_MASK                   0
#define MBY_FWD_PORT_CFG_1_h_DESTINATION_MASK                   23

#define MBY_FWD_SYS_CFG_1_WIDTH                                 2
#define MBY_FWD_SYS_CFG_1(word)                                 (((word)*4) + (0x0001200) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_SYS_CFG_1_b_STORE_TRAP_ACTION                   4
#define MBY_FWD_SYS_CFG_1_b_DROP_MAC_CTRL_ETHERTYPE             3
#define MBY_FWD_SYS_CFG_1_b_DROP_INVALID_SMAC                   2
#define MBY_FWD_SYS_CFG_1_b_ENABLE_TRAP_PLUS_LOG                1
#define MBY_FWD_SYS_CFG_1_b_TRAP_MTU_VIOLATIONS                 0

#define MBY_FWD_CPU_MAC_WIDTH                                   2
#define MBY_FWD_CPU_MAC(word)                                   (((word)*4) + (0x0001208) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_CPU_MAC_l_MAC_ADDR                              0
#define MBY_FWD_CPU_MAC_h_MAC_ADDR                              47

#define MBY_FWD_SYS_CFG_ROUTER_WIDTH                            2
#define MBY_FWD_SYS_CFG_ROUTER(word)                            (((word)*4) + (0x0001210) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_SYS_CFG_ROUTER_b_TRAP_IP_OPTIONS                2
#define MBY_FWD_SYS_CFG_ROUTER_l_TRAP_TTL1                      0
#define MBY_FWD_SYS_CFG_ROUTER_h_TRAP_TTL1                      1

#define MBY_FWD_RX_MIRROR_CFG_WIDTH                             2
#define MBY_FWD_RX_MIRROR_CFG(word)                             (((word)*4) + (0x0001218) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_RX_MIRROR_CFG_l_MIRROR_PROFILE_IDX              0
#define MBY_FWD_RX_MIRROR_CFG_h_MIRROR_PROFILE_IDX              5

#define MBY_FWD_QCN_MIRROR_CFG_WIDTH                            2
#define MBY_FWD_QCN_MIRROR_CFG(word)                            (((word)*4) + (0x0001220) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_QCN_MIRROR_CFG_l_MIRROR_PROFILE_IDX             2
#define MBY_FWD_QCN_MIRROR_CFG_h_MIRROR_PROFILE_IDX             7
#define MBY_FWD_QCN_MIRROR_CFG_l_MIRROR_SESSION                 0
#define MBY_FWD_QCN_MIRROR_CFG_h_MIRROR_SESSION                 1

#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_WIDTH                  4
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION(word)                  (((word)*4) + (0x0001230) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_63            126
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_63            127
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_62            124
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_62            125
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_61            122
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_61            123
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_60            120
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_60            121
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_59            118
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_59            119
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_58            116
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_58            117
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_57            114
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_57            115
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_56            112
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_56            113
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_55            110
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_55            111
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_54            108
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_54            109
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_53            106
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_53            107
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_52            104
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_52            105
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_51            102
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_51            103
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_50            100
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_50            101
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_49            98
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_49            99
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_48            96
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_48            97
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_47            94
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_47            95
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_46            92
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_46            93
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_45            90
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_45            91
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_44            88
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_44            89
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_43            86
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_43            87
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_42            84
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_42            85
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_41            82
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_41            83
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_40            80
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_40            81
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_39            78
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_39            79
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_38            76
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_38            77
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_37            74
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_37            75
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_36            72
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_36            73
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_35            70
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_35            71
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_34            68
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_34            69
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_33            66
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_33            67
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_32            64
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_32            65
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_31            62
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_31            63
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_30            60
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_30            61
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_29            58
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_29            59
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_28            56
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_28            57
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_27            54
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_27            55
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_26            52
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_26            53
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_25            50
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_25            51
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_24            48
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_24            49
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_23            46
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_23            47
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_22            44
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_22            45
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_21            42
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_21            43
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_20            40
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_20            41
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_19            38
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_19            39
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_18            36
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_18            37
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_17            34
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_17            35
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_16            32
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_16            33
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_15            30
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_15            31
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_14            28
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_14            29
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_13            26
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_13            27
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_12            24
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_12            25
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_11            22
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_11            23
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_10            20
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_10            21
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_9             18
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_9             19
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_8             16
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_8             17
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_7             14
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_7             15
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_6             12
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_6             13
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_5             10
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_5             11
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_4             8
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_4             9
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_3             6
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_3             7
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_2             4
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_2             5
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_1             2
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_1             3
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_0             0
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_0             1

#define MBY_FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY_WIDTH           2
#define MBY_FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY(word)           (((word)*4) + (0x0001340) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY_l_SELECT        0
#define MBY_FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY_h_SELECT        63

#define MBY_FWD_IEEE_RESERVED_MAC_CFG_WIDTH                     2
#define MBY_FWD_IEEE_RESERVED_MAC_CFG(word)                     (((word)*4) + (0x0001350) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_IEEE_RESERVED_MAC_CFG_l_TRAP_TC                 0
#define MBY_FWD_IEEE_RESERVED_MAC_CFG_h_TRAP_TC                 2

#define MBY_FWD_IP_WIDTH                                        2
#define MBY_FWD_IP(word)                                        (((word)*4) + (0x0001360) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_IP_b_WB_FIFO_PERR                               4
#define MBY_FWD_IP_b_TRIGGER                                    3
#define MBY_FWD_IP_b_TCAM_ERR                                   2
#define MBY_FWD_IP_b_ENTRY_COUNT                                1
#define MBY_FWD_IP_b_SHELL_CTRL_U_ERR                           0

#define MBY_FWD_IM_WIDTH                                        2
#define MBY_FWD_IM(word)                                        (((word)*4) + (0x0001370) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_IM_b_WB_FIFO_PERR                               4
#define MBY_FWD_IM_b_TRIGGER                                    3
#define MBY_FWD_IM_b_TCAM_ERR                                   2
#define MBY_FWD_IM_b_ENTRY_COUNT                                1
#define MBY_FWD_IM_b_SHELL_CTRL_U_ERR                           0

#define MBY_FWD_MA_TABLE_DIRTY_IP_WIDTH                         2
#define MBY_FWD_MA_TABLE_DIRTY_IP(word)                         (((word)*4) + (0x0001380) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_MA_TABLE_DIRTY_IP_b_PENDING                     0

#define MBY_FWD_MA_TABLE_DIRTY_IM_WIDTH                         2
#define MBY_FWD_MA_TABLE_DIRTY_IM(word)                         (((word)*4) + (0x0001390) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_MA_TABLE_DIRTY_IM_b_MASK                        0

// Action Codes:
#define MBY_ACTION_NORMAL             0   /* forwarded normally */
#define MBY_ACTION_FLOOD              1   /* flooded due to unknown destination */
#define MBY_ACTION_SPECIAL            2   /* forwarded with fixed destination mask */
#define MBY_ACTION_DROP_PARSE         3   /* drop due to parse error */
#define MBY_ACTION_DROP_PARITY        4   /* dropped due to parity error */
#define MBY_ACTION_TRAP               5   /* trap spec. mcast add., MAC CTL & MAC REMAP CTL */
#define MBY_ACTION_DROP_CONTROL       6   /* dropped pause frame */ 
#define MBY_ACTION_DROP_STP           7   /* dropped due to spanning tree*/
#define MBY_ACTION_DROP_SV            8   /* dropped due to security violation */
#define MBY_ACTION_MARKER_ERROR_DROPS 9   /* marker packets dropped due to fatal errors */
#define MBY_ACTION_DROP_IV            10   /* dropped due to vlan ingress violation */
#define MBY_ACTION_DROP_EV            11   /* dropped due to vlan egress violation */
#define MBY_ACTION_DROP_CAM           12
#define MBY_ACTION_DROP_FFU           13   /* dropped due to FFU flag */
#define MBY_ACTION_DROP_TRIG          14   /* dropped due to a trigger action */
#define MBY_ACTION_DROP_L3_PYLD_LEN   15   /* Single segment frames dropped when the L3 payload length does not agree with the actual packet length */
#define MBY_ACTION_DROP_POLICER       16   /* dropped due the policer_drop flag from pre. */
#define MBY_ACTION_DROP_TTL           17  
#define MBY_ACTION_DROP_CM_GLOBAL     18
#define MBY_ACTION_DROP_CM_SMP0       19
#define MBY_ACTION_DROP_CM_SMP1       20
#define MBY_ACTION_DROP_CM_RX_HOG0    21
#define MBY_ACTION_DROP_CM_RX_HOG1    22
#define MBY_ACTION_DROP_CM_TX_HOG0    23
#define MBY_ACTION_DROP_CM_TX_HOG1    24
#define MBY_ACTION_DROP_FRAME_ERR     25
#define MBY_ACTION_REDIRECT_TRIG      26   /* redirected due to trigger match */
#define MBY_ACTION_DROP_DLF           27   /* drop due to flood control of DLF frames */
#define MBY_ACTION_GLORT_FORWARDED    28  /* Glort forwarded. */
#define MBY_ACTION_BANK5_OTHER_DROPS  29  /* Bank 5 Other Drops */
#define MBY_ACTION_DROP_LOOPBACK      30  /* Drop due to port or VLAN reflection disabled. */
#define MBY_ACTION_DROP_L4_CSUM       31  /* Single segment packet drops due to L4 checksum (UDP, TCP, SCTP) failures */

#define MBY_AMASK_DROP_PERR                  ( FM_LITERAL_U64(1) <<  0  )
#define MBY_AMASK_SPECIAL                    ( FM_LITERAL_U64(1) <<  1  )
#define MBY_AMASK_DROP_PARSER_ERR            ( FM_LITERAL_U64(1) <<  2  )
#define MBY_AMASK_TRAP_RESERVED_MAC          ( FM_LITERAL_U64(1) <<  3  )
#define MBY_AMASK_TRAP_RESERVED_MAC_REMAP    ( FM_LITERAL_U64(1) <<  4  )
#define MBY_AMASK_DROP_MAC_CTRL              ( FM_LITERAL_U64(1) <<  7  )
#define MBY_AMASK_DROP_RESERVED_MAC          ( FM_LITERAL_U64(1) <<  8  )
#define MBY_AMASK_DROP_SMAC                  ( FM_LITERAL_U64(1) <<  10 )
#define MBY_AMASK_DROP_SEC_ADDR              ( FM_LITERAL_U64(1) <<  11 )
#define MBY_AMASK_DROP_SEC_PORT              ( FM_LITERAL_U64(1) <<  12 )
#define MBY_AMASK_DROP_STATIC_ADDR           ( FM_LITERAL_U64(1) <<  13 )
#define MBY_AMASK_DROP_PROVISIONAL           ( FM_LITERAL_U64(1) <<  14 )
#define MBY_AMASK_TRAP_CPU_ADDR              ( FM_LITERAL_U64(1) <<  15 )
#define MBY_AMASK_DROP_IV                    ( FM_LITERAL_U64(1) <<  16 )
#define MBY_AMASK_DROP_INGRESS_STP_NON_LEARN ( FM_LITERAL_U64(1) <<  17 )
#define MBY_AMASK_DROP_INGRESS_STP_LEARN     ( FM_LITERAL_U64(1) <<  18 )
#define MBY_AMASK_DROP_FFU                   ( FM_LITERAL_U64(1) <<  19 )
#define MBY_AMASK_TRAP_FFU                   ( FM_LITERAL_U64(1) <<  20 )
#define MBY_AMASK_TRAP_ICMP_TTL              ( FM_LITERAL_U64(1) <<  21 )
#define MBY_AMASK_TRAP_IP_OPTION             ( FM_LITERAL_U64(1) <<  22 )
#define MBY_AMASK_TRAP_MTU_VIO               ( FM_LITERAL_U64(1) <<  23 )
#define MBY_AMASK_TRAP_IGMP                  ( FM_LITERAL_U64(1) <<  24 )
#define MBY_AMASK_TRAP_TTL                   ( FM_LITERAL_U64(1) <<  25 )
#define MBY_AMASK_DROP_TTL                   ( FM_LITERAL_U64(1) <<  26 )
#define MBY_AMASK_DROP_DLF                   ( FM_LITERAL_U64(1) <<  27 )
#define MBY_AMASK_DROP_CAM_MISS              ( FM_LITERAL_U64(1) <<  28 )
#define MBY_AMASK_DROP_NULL_GLORTDEST        ( FM_LITERAL_U64(1) <<  29 )
#define MBY_AMASK_DROP_EV                    ( FM_LITERAL_U64(1) <<  30 )
#define MBY_AMASK_DROP_EGRESS_STP            ( FM_LITERAL_U64(1) <<  32 )
#define MBY_AMASK_DROP_LOOPBACK              ( FM_LITERAL_U64(1) <<  33 )
#define MBY_AMASK_GLORT                      ( FM_LITERAL_U64(1) <<  34 )
#define MBY_AMASK_FLOOD                      ( FM_LITERAL_U64(1) <<  35 )
#define MBY_AMASK_SWITCH_RESERVED_MAC        ( FM_LITERAL_U64(1) <<  36 )
#define MBY_AMASK_FORWARD_NORMAL             ( FM_LITERAL_U64(1) <<  37 )
#define MBY_AMASK_LOG_INGRESS_FFU            ( FM_LITERAL_U64(1) <<  38 )
#define MBY_AMASK_LOG_MAC_CTRL               ( FM_LITERAL_U64(1) <<  39 )
#define MBY_AMASK_LOG_ARP_REDIRECT           ( FM_LITERAL_U64(1) <<  40 )
#define MBY_AMASK_LOG_IP_ICMP                ( FM_LITERAL_U64(1) <<  41 )
#define MBY_AMASK_LOG_IP_TTL                 ( FM_LITERAL_U64(1) <<  42 )
#define MBY_AMASK_MIRROR_INGRESS_FFU         ( FM_LITERAL_U64(1) <<  43 )

#define MBY_LOG_TYPE_TRIG_LOG_ACTION  (1 << 0)
#define MBY_LOG_TYPE_FFU              (1 << 1)
#define MBY_LOG_TYPE_RESERVED_MAC     (1 << 2)
#define MBY_LOG_TYPE_ARP_REDIRECT     (1 << 3)
#define MBY_LOG_TYPE_ICMP             (1 << 4)
#define MBY_LOG_TYPE_TTL_IP_MC        (1 << 5)
#define MBY_LOG_TYPE_IP_UCST_L2_MCST  (1 << 7) /* EAC TBR */

#define MBY_DMAC_IEEE_PREFIX          FM_LITERAL_U64(0x0180c2000000)
#define MBY_SPECIAL_DMASK             0xFFFFFFFFFF00

#define MBY_SV_MOVE_DROP_RESERVED     0
#define MBY_SV_MOVE_DROP_PORT         1
#define MBY_SV_MOVE_DROP_ADDR         2
#define MBY_SV_MOVE_DROP_STATIC       3

// Enums:

typedef enum mbyIeeeReservedMacActionActionEnum
{
    MBY_IEEE_RESERVED_MAC_ACTION_ACTION_SWITCHNORMALLY = 0,
    MBY_IEEE_RESERVED_MAC_ACTION_ACTION_TRAP = 1,
    MBY_IEEE_RESERVED_MAC_ACTION_ACTION_DROP = 2,
    MBY_IEEE_RESERVED_MAC_ACTION_ACTION_LOG = 3

} mbyIeeeReservedMacActionAction;

typedef enum mbyFclassTypeEnum
{
    MBY_FCLASS_UNICAST = 0,
    MBY_FCLASS_MULTICAST,
    MBY_FCLASS_BROADCAST

} mbyFclassType;

// Structs:

typedef struct mbyFwdPortCfg1Struct
{
    fm_bool                 LEARNING_ENABLE;
    fm_bool                 FILTER_VLAN_INGRESS;
    fm_uint32               DESTINATION_MASK;

} mbyFwdPortCfg1;

typedef struct mbyFwdPortCfg2Struct
{
    fm_uint32               DESTINATION_MASK;

} mbyFwdPortCfg2;

typedef struct mbyFwdSysCfg1Struct
{
    fm_bool                 STORE_TRAP_ACTION;
    fm_bool                 DROP_MAC_CTRL_ETHERTYPE;
    fm_bool                 DROP_INVALID_SMAC;
    fm_bool                 ENABLE_TRAP_PLUS_LOG;
    fm_bool                 TRAP_MTU_VIOLATIONS;

} mbyFwdSysCfg1;

typedef struct mbyFwdSysCfgRouterStruct
{
    fm_bool                 TRAP_IP_OPTIONS;
    fm_byte                 TRAP_TTL1;

} mbyFwdSysCfgRouter;

typedef struct mbyMaskGenToTriggersStruct
{
    fm_bool                 LEARNING_ENABLED;    // learning enabled status within action codes
    fm_uint64               AMASK;               // 46-bit action mask
    fm_uint32               DMASK;               // 24-bit destination mask
    fm_uint32               FNMASK;              // 24-bit normal forwarding mask
    fm_byte                 LOG_AMASK;           // 6-bit logging action mask
    fm_bool                 CPU_TRAP;            // flag indicating frame should be sent to CPU
    fm_byte                 OPERATOR_ID;         // 4-bit operator ID
    fm_byte                 QOS_SWPRI;           // 4-bit switch priority
    fm_bool                 STORE_TRAP_ACTION;   // flag indicating whether 4bit trap action code will be stored in metadata
    fm_uint16               IDGLORT;             // 16-bit ingress destination GLORT
    fm_bool                 LOGGING_HIT;         // flag indicating whether logging was hit
    fm_bool                 MCAST_EPOCH;         // flag defining current epoch for multicast garbage collection
    fm_int                  MIRROR0_PORT;        // mirror 0 port
    fm_int                  MIRROR1_PORT;        // mirror 1 port
    fm_byte                 MIRROR0_PROFILE_V;   // mirror 0 profile valid
    fm_byte                 MIRROR1_PROFILE_V;   // mirror 1 profile valid
    fm_int                  MIRROR0_PROFILE_IDX; // mirror 0 profile index
    fm_int                  MIRROR1_PROFILE_IDX; // mirror 1 profile index
    fm_uint32               ACTION;              // resolved action
    fm_byte                 L2_EDOMAIN;          // egress L2 domain
    fm_byte                 L3_EDOMAIN;          // egress L3 domain
    fm_bool                 MAC_MOVED;           // flag indicating that a non-secure MAC was found
    fm_byte                 FCLASS;              // class state (Unicast, Broadcast or Multicast) detected
    fm_byte                 XCAST;               // indicate Unicast, Multicast, or Broadcast

} mbyMaskGenToTriggers;

#endif
