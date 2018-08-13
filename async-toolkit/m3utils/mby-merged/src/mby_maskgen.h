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

// Enums:


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

typedef struct mbyMaskGenToTriggersStruct
{
    fm_bool                 FOO;

} mbyMaskGenToTriggers;

#endif
