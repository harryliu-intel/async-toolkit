// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_MASKGEN_H
#define MBY_MASKGEN_H

// Includes:

#include <mby_top_map.h> // header file auto-generated from RDL

#include "mby_nxt2msk.h"
#include "mby_msk2trg.h"
#include "mby_maskgen_regs.h"
#include "mby_action_codes.h"
#include "mby_fclass_type.h"
#include "mby_log_type.h"

// Defines:

/******** MST_GLORT_BASE *******/
#define MBY_MST_GLORT_BASE                               (0x3A00000)
#define MBY_MST_GLORT_SIZE                               (0x0010000)

#define MBY_GLORT_DEST_TABLE_WIDTH                       2
#define MBY_GLORT_DEST_TABLE_ENTRIES                     4096
#define MBY_GLORT_DEST_TABLE(index, word)                ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0010000) + (MBY_MST_GLORT_BASE))

#define MBY_GLORT_DEST_TABLE_l_IP_MULTICAST_INDEX        24
#define MBY_GLORT_DEST_TABLE_h_IP_MULTICAST_INDEX        35
#define MBY_GLORT_DEST_TABLE_l_DEST_MASK                 0
#define MBY_GLORT_DEST_TABLE_h_DEST_MASK                 23

#define MBY_GLORT_RAM_WIDTH                              2
#define MBY_GLORT_RAM_ENTRIES                            64
#define MBY_GLORT_RAM(index, word)                       ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0018000) + (MBY_MST_GLORT_BASE))

#define MBY_GLORT_RAM_b_SKIP_DGLORT_DEC                  35
#define MBY_GLORT_RAM_b_HASH_ROTATION                    34
#define MBY_GLORT_RAM_l_DEST_COUNT                       30
#define MBY_GLORT_RAM_h_DEST_COUNT                       33
#define MBY_GLORT_RAM_l_RANGE_SUB_INDEX_B                22
#define MBY_GLORT_RAM_h_RANGE_SUB_INDEX_B                29
#define MBY_GLORT_RAM_l_RANGE_SUB_INDEX_A                14
#define MBY_GLORT_RAM_h_RANGE_SUB_INDEX_A                21
#define MBY_GLORT_RAM_l_DEST_INDEX                       2
#define MBY_GLORT_RAM_h_DEST_INDEX                       13
#define MBY_GLORT_RAM_l_STRICT                           0
#define MBY_GLORT_RAM_h_STRICT                           1

#define MBY_GLORT_CAM_WIDTH                              2
#define MBY_GLORT_CAM_ENTRIES                            64
#define MBY_GLORT_CAM(index, word)                       ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0018400) + (MBY_MST_GLORT_BASE))

#define MBY_GLORT_CAM_l_KEY_INVERT                       16
#define MBY_GLORT_CAM_h_KEY_INVERT                       31
#define MBY_GLORT_CAM_l_KEY                              0
#define MBY_GLORT_CAM_h_KEY                              15

/******** FWD_MISC_BASE *******/
#define MBY_FWD_MISC_BASE                                (0x3A40000)
#define MBY_FWD_MISC_SIZE                                (0x0010000)

#define MBY_FWD_PORT_CFG_2_WIDTH                         2
#define MBY_FWD_PORT_CFG_2_ENTRIES                       512
#define MBY_FWD_PORT_CFG_2(index, word)                  ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0000000) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_PORT_CFG_2_l_DESTINATION_MASK            0
#define MBY_FWD_PORT_CFG_2_h_DESTINATION_MASK            23

#define MBY_FWD_LAG_CFG_WIDTH                            2
#define MBY_FWD_LAG_CFG_ENTRIES                          24
#define MBY_FWD_LAG_CFG(index, word)                     ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0001000) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_LAG_CFG_b_IN_LAG                         9
#define MBY_FWD_LAG_CFG_b_HASH_ROTATION                  8
#define MBY_FWD_LAG_CFG_l_INDEX                          4
#define MBY_FWD_LAG_CFG_h_INDEX                          7
#define MBY_FWD_LAG_CFG_l_LAG_SIZE                       0
#define MBY_FWD_LAG_CFG_h_LAG_SIZE                       3

#define MBY_FWD_PORT_CFG_1_WIDTH                         2
#define MBY_FWD_PORT_CFG_1_ENTRIES                       24
#define MBY_FWD_PORT_CFG_1(index, word)                  ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0001100) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_PORT_CFG_1_b_LEARNING_ENABLE             25
#define MBY_FWD_PORT_CFG_1_b_FILTER_VLAN_INGRESS         24
#define MBY_FWD_PORT_CFG_1_l_DESTINATION_MASK            0
#define MBY_FWD_PORT_CFG_1_h_DESTINATION_MASK            23

#define MBY_FWD_SYS_CFG_1_WIDTH                          2
#define MBY_FWD_SYS_CFG_1(word)                          (((word)*4) + (0x0001200) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_SYS_CFG_1_b_STORE_TRAP_ACTION            4
#define MBY_FWD_SYS_CFG_1_b_DROP_MAC_CTRL_ETHERTYPE      3
#define MBY_FWD_SYS_CFG_1_b_DROP_INVALID_SMAC            2
#define MBY_FWD_SYS_CFG_1_b_ENABLE_TRAP_PLUS_LOG         1
#define MBY_FWD_SYS_CFG_1_b_TRAP_MTU_VIOLATIONS          0

#define MBY_FWD_CPU_MAC_WIDTH                            2
#define MBY_FWD_CPU_MAC(word)                            (((word)*4) + (0x0001208) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_CPU_MAC_l_MAC_ADDR                       0
#define MBY_FWD_CPU_MAC_h_MAC_ADDR                       47

#define MBY_FWD_SYS_CFG_ROUTER_WIDTH                     2
#define MBY_FWD_SYS_CFG_ROUTER(word)                     (((word)*4) + (0x0001210) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_SYS_CFG_ROUTER_b_TRAP_IP_OPTIONS         2
#define MBY_FWD_SYS_CFG_ROUTER_l_TRAP_TTL1               0
#define MBY_FWD_SYS_CFG_ROUTER_h_TRAP_TTL1               1

#define MBY_FWD_RX_MIRROR_CFG_WIDTH                      2
#define MBY_FWD_RX_MIRROR_CFG(word)                      (((word)*4) + (0x0001218) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_RX_MIRROR_CFG_l_MIRROR_PROFILE_IDX       0
#define MBY_FWD_RX_MIRROR_CFG_h_MIRROR_PROFILE_IDX       5

#define MBY_FWD_QCN_MIRROR_CFG_WIDTH                     2
#define MBY_FWD_QCN_MIRROR_CFG(word)                     (((word)*4) + (0x0001220) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_QCN_MIRROR_CFG_l_MIRROR_PROFILE_IDX      2
#define MBY_FWD_QCN_MIRROR_CFG_h_MIRROR_PROFILE_IDX      7
#define MBY_FWD_QCN_MIRROR_CFG_l_MIRROR_SESSION          0
#define MBY_FWD_QCN_MIRROR_CFG_h_MIRROR_SESSION          1

#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_WIDTH           4
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION(word)           (((word)*4) + (0x0001230) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_63     126
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_63     127
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_62     124
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_62     125
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_61     122
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_61     123
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_60     120
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_60     121
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_59     118
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_59     119
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_58     116
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_58     117
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_57     114
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_57     115
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_56     112
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_56     113
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_55     110
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_55     111
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_54     108
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_54     109
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_53     106
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_53     107
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_52     104
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_52     105
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_51     102
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_51     103
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_50     100
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_50     101
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_49     98
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_49     99
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_48     96
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_48     97
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_47     94
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_47     95
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_46     92
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_46     93
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_45     90
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_45     91
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_44     88
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_44     89
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_43     86
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_43     87
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_42     84
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_42     85
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_41     82
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_41     83
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_40     80
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_40     81
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_39     78
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_39     79
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_38     76
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_38     77
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_37     74
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_37     75
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_36     72
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_36     73
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_35     70
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_35     71
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_34     68
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_34     69
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_33     66
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_33     67
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_32     64
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_32     65
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_31     62
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_31     63
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_30     60
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_30     61
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_29     58
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_29     59
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_28     56
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_28     57
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_27     54
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_27     55
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_26     52
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_26     53
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_25     50
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_25     51
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_24     48
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_24     49
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_23     46
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_23     47
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_22     44
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_22     45
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_21     42
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_21     43
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_20     40
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_20     41
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_19     38
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_19     39
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_18     36
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_18     37
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_17     34
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_17     35
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_16     32
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_16     33
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_15     30
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_15     31
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_14     28
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_14     29
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_13     26
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_13     27
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_12     24
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_12     25
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_11     22
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_11     23
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_10     20
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_10     21
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_9      18
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_9      19
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_8      16
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_8      17
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_7      14
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_7      15
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_6      12
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_6      13
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_5      10
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_5      11
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_4      8
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_4      9
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_3      6
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_3      7
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_2      4
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_2      5
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_1      2
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_1      3
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_l_ACTION_0      0
#define MBY_FWD_IEEE_RESERVED_MAC_ACTION_h_ACTION_0      1

#define MBY_FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY_WIDTH    2
#define MBY_FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY(word)    (((word)*4) + (0x0001340) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY_l_SELECT 0
#define MBY_FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY_h_SELECT 63

#define MBY_FWD_IEEE_RESERVED_MAC_CFG_WIDTH              2
#define MBY_FWD_IEEE_RESERVED_MAC_CFG(word)              (((word)*4) + (0x0001350) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_IEEE_RESERVED_MAC_CFG_l_TRAP_TC          0
#define MBY_FWD_IEEE_RESERVED_MAC_CFG_h_TRAP_TC          2

#define MBY_FWD_IP_WIDTH                                 2
#define MBY_FWD_IP(word)                                 (((word)*4) + (0x0001360) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_IP_b_WB_FIFO_PERR                        4
#define MBY_FWD_IP_b_TRIGGER                             3
#define MBY_FWD_IP_b_TCAM_ERR                            2
#define MBY_FWD_IP_b_ENTRY_COUNT                         1
#define MBY_FWD_IP_b_SHELL_CTRL_U_ERR                    0

#define MBY_FWD_IM_WIDTH                                 2
#define MBY_FWD_IM(word)                                 (((word)*4) + (0x0001370) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_IM_b_WB_FIFO_PERR                        4
#define MBY_FWD_IM_b_TRIGGER                             3
#define MBY_FWD_IM_b_TCAM_ERR                            2
#define MBY_FWD_IM_b_ENTRY_COUNT                         1
#define MBY_FWD_IM_b_SHELL_CTRL_U_ERR                    0

#define MBY_FWD_MA_TABLE_DIRTY_IP_WIDTH                  2
#define MBY_FWD_MA_TABLE_DIRTY_IP(word)                  (((word)*4) + (0x0001380) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_MA_TABLE_DIRTY_IP_b_PENDING              0

#define MBY_FWD_MA_TABLE_DIRTY_IM_WIDTH                  2
#define MBY_FWD_MA_TABLE_DIRTY_IM(word)                  (((word)*4) + (0x0001390) + (MBY_FWD_MISC_BASE))

#define MBY_FWD_MA_TABLE_DIRTY_IM_b_MASK                 0

/******** CM_APPLY_BASE *******/
#define MBY_CM_APPLY_BASE                                (0x3A20000)
#define MBY_CM_APPLY_SIZE                                (0x0010000)

#define MBY_CM_APPLY_MIRROR_PROFILE_TABLE_WIDTH          2
#define MBY_CM_APPLY_MIRROR_PROFILE_TABLE_ENTRIES        64
#define MBY_CM_APPLY_MIRROR_PROFILE_TABLE(index, word)   ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0003200) + (MBY_CM_APPLY_BASE))

#define MBY_CM_APPLY_MIRROR_PROFILE_TABLE_l_PORT         0
#define MBY_CM_APPLY_MIRROR_PROFILE_TABLE_h_PORT         4

#define MBY_CM_APPLY_LOOPBACK_SUPPRESS_WIDTH             2
#define MBY_CM_APPLY_LOOPBACK_SUPPRESS_ENTRIES           24
#define MBY_CM_APPLY_LOOPBACK_SUPPRESS(index, word)      ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0003500) + (MBY_CM_APPLY_BASE))

#define MBY_CM_APPLY_LOOPBACK_SUPPRESS_l_GLORT_MASK      16
#define MBY_CM_APPLY_LOOPBACK_SUPPRESS_h_GLORT_MASK      31
#define MBY_CM_APPLY_LOOPBACK_SUPPRESS_l_GLORT           0
#define MBY_CM_APPLY_LOOPBACK_SUPPRESS_h_GLORT           15

/******** SAF_BASE *******/
#define MBY_SAF_BASE                                     (0x3EB0000)
#define MBY_SAF_SIZE                                     (0x0000100)

#define MBY_SAF_MATRIX_WIDTH                             2
#define MBY_SAF_MATRIX_ENTRIES                           24
#define MBY_SAF_MATRIX(index, word)                      ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0000800) + (MBY_SAF_BASE))

#define MBY_SAF_MATRIX_l_ENABLE_SNF                      3
#define MBY_SAF_MATRIX_h_ENABLE_SNF                      26
#define MBY_SAF_MATRIX_l_CUT_THRU_MODE                   1
#define MBY_SAF_MATRIX_h_CUT_THRU_MODE                   2
#define MBY_SAF_MATRIX_b_IGNORE_FRAME_ERROR              0

#define MBY_DMAC_IEEE_PREFIX                 FM_LITERAL_U64(0x0180c2000000)
#define MBY_SPECIAL_DMASK                    0xFFFFFFFFFF00

#define MBY_SV_MOVE_DROP_RESERVED            0
#define MBY_SV_MOVE_DROP_PORT                1
#define MBY_SV_MOVE_DROP_ADDR                2
#define MBY_SV_MOVE_DROP_STATIC              3

#define MBY_DEFAULT_DMASK                    0xFFFFFFFFFFFFFFFF
#define MBY_AMASK_WIDTH                      38

// Enums:

typedef enum mbyStpStateEnum
{
    MBY_STP_STATE_DISABLE = 0,
    MBY_STP_STATE_LISTENING,
    MBY_STP_STATE_LEARNING,
    MBY_STP_STATE_FORWARD

} mbyStpState;

typedef enum mbyIeeeReservedMacActionActionEnum
{
    MBY_IEEE_RESERVED_MAC_ACTION_ACTION_SWITCHNORMALLY = 0,
    MBY_IEEE_RESERVED_MAC_ACTION_ACTION_TRAP           = 1,
    MBY_IEEE_RESERVED_MAC_ACTION_ACTION_DROP           = 2,
    MBY_IEEE_RESERVED_MAC_ACTION_ACTION_LOG            = 3

} mbyIeeeReservedMacActionAction;

// Structs:

// Function prototype:

void MaskGen
(
    mby_ppe_fwd_misc_map  const * const fwd_misc,
    mby_ppe_mst_glort_map const * const glort_map,
    mby_ppe_cm_apply_map  const * const cm_apply,
    mbyNextHopToMaskGen   const * const in,
    mbyMaskGenToTriggers        * const out
);

#endif
