// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_MAPPER_H
#define MBY_MAPPER_H

#include "mby_common.h"

// Defines:
#define MBY_MAPPER_BASE                                         (0x3780000)
#define MBY_MAPPER_SIZE                                         (0x0080000)

#define MBY_MAP_PORT_CFG_WIDTH                                  2
#define MBY_MAP_PORT_CFG_ENTRIES                                24
#define MBY_MAP_PORT_CFG(index, word)                           ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0000000) + (MBY_MAPPER_BASE))

#define MBY_MAP_PORT_CFG_l_DEFAULT_SGLORT                       9
#define MBY_MAP_PORT_CFG_h_DEFAULT_SGLORT                       24
#define MBY_MAP_PORT_CFG_b_DEFAULT_SGLORT_EN                    8
#define MBY_MAP_PORT_CFG_l_PORT_PROFILE                         0
#define MBY_MAP_PORT_CFG_h_PORT_PROFILE                         7

#define MBY_MAP_PORT_DEFAULT_WIDTH                              2
#define MBY_MAP_PORT_DEFAULT_ENTRIES_0                          6
#define MBY_MAP_PORT_DEFAULT_ENTRIES_1                          24
#define MBY_MAP_PORT_DEFAULT(index1, index0, word)              ((0x0000040) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0000800) + (MBY_MAPPER_BASE))

#define MBY_MAP_PORT_DEFAULT_l_VALUE                            16
#define MBY_MAP_PORT_DEFAULT_h_VALUE                            31
#define MBY_MAP_PORT_DEFAULT_l_TARGET                           0
#define MBY_MAP_PORT_DEFAULT_h_TARGET                           7

#define MBY_MAP_LEN_LIMIT_WIDTH                                 2
#define MBY_MAP_LEN_LIMIT_ENTRIES                               24
#define MBY_MAP_LEN_LIMIT(index, word)                          ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0001100) + (MBY_MAPPER_BASE))

#define MBY_MAP_LEN_LIMIT_l_OTR_L2_LEN_LIMIT                    9
#define MBY_MAP_LEN_LIMIT_h_OTR_L2_LEN_LIMIT                    11
#define MBY_MAP_LEN_LIMIT_l_INR_L2_LEN_LIMIT                    6
#define MBY_MAP_LEN_LIMIT_h_INR_L2_LEN_LIMIT                    8
#define MBY_MAP_LEN_LIMIT_l_OTR_MPLS_LEN_LIMIT                  3
#define MBY_MAP_LEN_LIMIT_h_OTR_MPLS_LEN_LIMIT                  5
#define MBY_MAP_LEN_LIMIT_l_INR_MPLS_LEN_LIMIT                  0
#define MBY_MAP_LEN_LIMIT_h_INR_MPLS_LEN_LIMIT                  2

#define MBY_MAP_DOMAIN_TCAM_WIDTH                               4
#define MBY_MAP_DOMAIN_TCAM_ENTRIES                             4096
#define MBY_MAP_DOMAIN_TCAM(index, word)                        ((0x0000010) * ((index) - 0) + ((word)*4)+ (0x0010000) + (MBY_MAPPER_BASE))

#define MBY_MAP_DOMAIN_TCAM_l__RSVD1_                           107
#define MBY_MAP_DOMAIN_TCAM_h__RSVD1_                           127
#define MBY_MAP_DOMAIN_TCAM_l_PORT_KEY_INVERT                   90
#define MBY_MAP_DOMAIN_TCAM_h_PORT_KEY_INVERT                   106
#define MBY_MAP_DOMAIN_TCAM_b_VID2_VALID_INVERT                 89
#define MBY_MAP_DOMAIN_TCAM_l_VID2_KEY_INVERT                   77
#define MBY_MAP_DOMAIN_TCAM_h_VID2_KEY_INVERT                   88
#define MBY_MAP_DOMAIN_TCAM_b_VID1_VALID_INVERT                 76
#define MBY_MAP_DOMAIN_TCAM_l_VID1_KEY_INVERT                   64
#define MBY_MAP_DOMAIN_TCAM_h_VID1_KEY_INVERT                   75
#define MBY_MAP_DOMAIN_TCAM_l__RSVD0_                           43
#define MBY_MAP_DOMAIN_TCAM_h__RSVD0_                           63
#define MBY_MAP_DOMAIN_TCAM_l_PORT_KEY                          26
#define MBY_MAP_DOMAIN_TCAM_h_PORT_KEY                          42
#define MBY_MAP_DOMAIN_TCAM_b_VID2_VALID                        25
#define MBY_MAP_DOMAIN_TCAM_l_VID2_KEY                          13
#define MBY_MAP_DOMAIN_TCAM_h_VID2_KEY                          24
#define MBY_MAP_DOMAIN_TCAM_b_VID1_VALID                        12
#define MBY_MAP_DOMAIN_TCAM_l_VID1_KEY                          0
#define MBY_MAP_DOMAIN_TCAM_h_VID1_KEY                          11

#define MBY_MAP_DOMAIN_ACTION0_WIDTH                            2
#define MBY_MAP_DOMAIN_ACTION0_ENTRIES                          4096
#define MBY_MAP_DOMAIN_ACTION0(index, word)                     ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0020000) + (MBY_MAPPER_BASE))

#define MBY_MAP_DOMAIN_ACTION0_l_L2_DOMAIN                      30
#define MBY_MAP_DOMAIN_ACTION0_h_L2_DOMAIN                      37
#define MBY_MAP_DOMAIN_ACTION0_l_L3_DOMAIN                      24
#define MBY_MAP_DOMAIN_ACTION0_h_L3_DOMAIN                      29
#define MBY_MAP_DOMAIN_ACTION0_l_OPERATOR_ID                    20
#define MBY_MAP_DOMAIN_ACTION0_h_OPERATOR_ID                    23
#define MBY_MAP_DOMAIN_ACTION0_b_UPDATE_DOMAINS                 19
#define MBY_MAP_DOMAIN_ACTION0_b_LEARN_EN                       18
#define MBY_MAP_DOMAIN_ACTION0_b_LEARN_MODE                     17
#define MBY_MAP_DOMAIN_ACTION0_l_PRIORITY_PROFILE               12
#define MBY_MAP_DOMAIN_ACTION0_h_PRIORITY_PROFILE               16
#define MBY_MAP_DOMAIN_ACTION0_l_PRI_SOURCE                     4
#define MBY_MAP_DOMAIN_ACTION0_h_PRI_SOURCE                     11
#define MBY_MAP_DOMAIN_ACTION0_b_FORCE_DEFAULT_PRI              3
#define MBY_MAP_DOMAIN_ACTION0_l_DEFAULT_PRI                    0
#define MBY_MAP_DOMAIN_ACTION0_h_DEFAULT_PRI                    2

#define MBY_MAP_DOMAIN_ACTION1_WIDTH                            2
#define MBY_MAP_DOMAIN_ACTION1_ENTRIES                          4096
#define MBY_MAP_DOMAIN_ACTION1(index, word)                     ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0028000) + (MBY_MAPPER_BASE))

#define MBY_MAP_DOMAIN_ACTION1_l_DOMAIN_PROFILE                 36
#define MBY_MAP_DOMAIN_ACTION1_h_DOMAIN_PROFILE                 43
#define MBY_MAP_DOMAIN_ACTION1_l_L2_POLICER                     24
#define MBY_MAP_DOMAIN_ACTION1_h_L2_POLICER                     35
#define MBY_MAP_DOMAIN_ACTION1_l_L3_POLICER                     12
#define MBY_MAP_DOMAIN_ACTION1_h_L3_POLICER                     23
#define MBY_MAP_DOMAIN_ACTION1_l_VLAN_COUNTER                   0
#define MBY_MAP_DOMAIN_ACTION1_h_VLAN_COUNTER                   11

#define MBY_MAP_DOMAIN_PROFILE_WIDTH                            2
#define MBY_MAP_DOMAIN_PROFILE_ENTRIES                          512
#define MBY_MAP_DOMAIN_PROFILE(index, word)                     ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0030000) + (MBY_MAPPER_BASE))

#define MBY_MAP_DOMAIN_PROFILE_l_PRIORITY_PROFILE               0
#define MBY_MAP_DOMAIN_PROFILE_h_PRIORITY_PROFILE               4

#define MBY_MAP_PORT_WIDTH                                      2
#define MBY_MAP_PORT_ENTRIES                                    24
#define MBY_MAP_PORT(index, word)                               ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0031000) + (MBY_MAPPER_BASE))

#define MBY_MAP_PORT_l_MAP_PORT                                 0
#define MBY_MAP_PORT_h_MAP_PORT                                 7

#define MBY_MAP_MAC_WIDTH                                       4
#define MBY_MAP_MAC_ENTRIES                                     96
#define MBY_MAP_MAC(index, word)                                ((0x0000010) * ((index) - 0) + ((word)*4)+ (0x0031800) + (MBY_MAPPER_BASE))

#define MBY_MAP_MAC_b_MAC_ROUTABLE                              66
#define MBY_MAP_MAC_l_MAP_MAC                                   58
#define MBY_MAP_MAC_h_MAP_MAC                                   65
#define MBY_MAP_MAC_l_VALID                                     54
#define MBY_MAP_MAC_h_VALID                                     57
#define MBY_MAP_MAC_l_IGNORE_LENGTH                             48
#define MBY_MAP_MAC_h_IGNORE_LENGTH                             53
#define MBY_MAP_MAC_l_MAC                                       0
#define MBY_MAP_MAC_h_MAC                                       47

#define MBY_MAP_PROT_WIDTH                                      2
#define MBY_MAP_PROT_ENTRIES                                    8
#define MBY_MAP_PROT(index, word)                               ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0032200) + (MBY_MAPPER_BASE))

#define MBY_MAP_PROT_l_MAP_PROT                                 8
#define MBY_MAP_PROT_h_MAP_PROT                                 10
#define MBY_MAP_PROT_l_PROT                                     0
#define MBY_MAP_PROT_h_PROT                                     7

#define MBY_MAP_L4_SRC_WIDTH                                    2
#define MBY_MAP_L4_SRC_ENTRIES                                  64
#define MBY_MAP_L4_SRC(index, word)                             ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0032400) + (MBY_MAPPER_BASE))

#define MBY_MAP_L4_SRC_l_MAP_L4_SRC                             21
#define MBY_MAP_L4_SRC_h_MAP_L4_SRC                             36
#define MBY_MAP_L4_SRC_l_VALID                                  19
#define MBY_MAP_L4_SRC_h_VALID                                  20
#define MBY_MAP_L4_SRC_l_MAP_PROT                               16
#define MBY_MAP_L4_SRC_h_MAP_PROT                               18
#define MBY_MAP_L4_SRC_l_L4_SRC                                 0
#define MBY_MAP_L4_SRC_h_L4_SRC                                 15

#define MBY_MAP_L4_DST_WIDTH                                    2
#define MBY_MAP_L4_DST_ENTRIES                                  64
#define MBY_MAP_L4_DST(index, word)                             ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0032600) + (MBY_MAPPER_BASE))

#define MBY_MAP_L4_DST_l_MAP_L4_DST                             21
#define MBY_MAP_L4_DST_h_MAP_L4_DST                             36
#define MBY_MAP_L4_DST_l_VALID                                  19
#define MBY_MAP_L4_DST_h_VALID                                  20
#define MBY_MAP_L4_DST_l_MAP_PROT                               16
#define MBY_MAP_L4_DST_h_MAP_PROT                               18
#define MBY_MAP_L4_DST_l_L4_DST                                 0
#define MBY_MAP_L4_DST_h_L4_DST                                 15

#define MBY_MAP_EXP_TC_WIDTH                                    2
#define MBY_MAP_EXP_TC_ENTRIES                                  32
#define MBY_MAP_EXP_TC(index, word)                             ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0032800) + (MBY_MAPPER_BASE))

#define MBY_MAP_EXP_TC_l_TC_7                                   21
#define MBY_MAP_EXP_TC_h_TC_7                                   23
#define MBY_MAP_EXP_TC_l_TC_6                                   18
#define MBY_MAP_EXP_TC_h_TC_6                                   20
#define MBY_MAP_EXP_TC_l_TC_5                                   15
#define MBY_MAP_EXP_TC_h_TC_5                                   17
#define MBY_MAP_EXP_TC_l_TC_4                                   12
#define MBY_MAP_EXP_TC_h_TC_4                                   14
#define MBY_MAP_EXP_TC_l_TC_3                                   9
#define MBY_MAP_EXP_TC_h_TC_3                                   11
#define MBY_MAP_EXP_TC_l_TC_2                                   6
#define MBY_MAP_EXP_TC_h_TC_2                                   8
#define MBY_MAP_EXP_TC_l_TC_1                                   3
#define MBY_MAP_EXP_TC_h_TC_1                                   5
#define MBY_MAP_EXP_TC_l_TC_0                                   0
#define MBY_MAP_EXP_TC_h_TC_0                                   2

#define MBY_MAP_DSCP_TC_WIDTH                                   2
#define MBY_MAP_DSCP_TC_ENTRIES                                 2048
#define MBY_MAP_DSCP_TC(index, word)                            ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0034000) + (MBY_MAPPER_BASE))

#define MBY_MAP_DSCP_TC_l_TC                                    6
#define MBY_MAP_DSCP_TC_h_TC                                    8
#define MBY_MAP_DSCP_TC_l_DSCP                                  0
#define MBY_MAP_DSCP_TC_h_DSCP                                  5

#define MBY_MAP_VPRI_TC_WIDTH                                   2
#define MBY_MAP_VPRI_TC_ENTRIES                                 32
#define MBY_MAP_VPRI_TC(index, word)                            ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0038000) + (MBY_MAPPER_BASE))

#define MBY_MAP_VPRI_TC_l_TC_15                                 45
#define MBY_MAP_VPRI_TC_h_TC_15                                 47
#define MBY_MAP_VPRI_TC_l_TC_14                                 42
#define MBY_MAP_VPRI_TC_h_TC_14                                 44
#define MBY_MAP_VPRI_TC_l_TC_13                                 39
#define MBY_MAP_VPRI_TC_h_TC_13                                 41
#define MBY_MAP_VPRI_TC_l_TC_12                                 36
#define MBY_MAP_VPRI_TC_h_TC_12                                 38
#define MBY_MAP_VPRI_TC_l_TC_11                                 33
#define MBY_MAP_VPRI_TC_h_TC_11                                 35
#define MBY_MAP_VPRI_TC_l_TC_10                                 30
#define MBY_MAP_VPRI_TC_h_TC_10                                 32
#define MBY_MAP_VPRI_TC_l_TC_9                                  27
#define MBY_MAP_VPRI_TC_h_TC_9                                  29
#define MBY_MAP_VPRI_TC_l_TC_8                                  24
#define MBY_MAP_VPRI_TC_h_TC_8                                  26
#define MBY_MAP_VPRI_TC_l_TC_7                                  21
#define MBY_MAP_VPRI_TC_h_TC_7                                  23
#define MBY_MAP_VPRI_TC_l_TC_6                                  18
#define MBY_MAP_VPRI_TC_h_TC_6                                  20
#define MBY_MAP_VPRI_TC_l_TC_5                                  15
#define MBY_MAP_VPRI_TC_h_TC_5                                  17
#define MBY_MAP_VPRI_TC_l_TC_4                                  12
#define MBY_MAP_VPRI_TC_h_TC_4                                  14
#define MBY_MAP_VPRI_TC_l_TC_3                                  9
#define MBY_MAP_VPRI_TC_h_TC_3                                  11
#define MBY_MAP_VPRI_TC_l_TC_2                                  6
#define MBY_MAP_VPRI_TC_h_TC_2                                  8
#define MBY_MAP_VPRI_TC_l_TC_1                                  3
#define MBY_MAP_VPRI_TC_h_TC_1                                  5
#define MBY_MAP_VPRI_TC_l_TC_0                                  0
#define MBY_MAP_VPRI_TC_h_TC_0                                  2

#define MBY_MAP_VPRI_WIDTH                                      2
#define MBY_MAP_VPRI_ENTRIES                                    32
#define MBY_MAP_VPRI(index, word)                               ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0038100) + (MBY_MAPPER_BASE))

#define MBY_MAP_VPRI_l_VPRI_15                                  60
#define MBY_MAP_VPRI_h_VPRI_15                                  63
#define MBY_MAP_VPRI_l_VPRI_14                                  56
#define MBY_MAP_VPRI_h_VPRI_14                                  59
#define MBY_MAP_VPRI_l_VPRI_13                                  52
#define MBY_MAP_VPRI_h_VPRI_13                                  55
#define MBY_MAP_VPRI_l_VPRI_12                                  48
#define MBY_MAP_VPRI_h_VPRI_12                                  51
#define MBY_MAP_VPRI_l_VPRI_11                                  44
#define MBY_MAP_VPRI_h_VPRI_11                                  47
#define MBY_MAP_VPRI_l_VPRI_10                                  40
#define MBY_MAP_VPRI_h_VPRI_10                                  43
#define MBY_MAP_VPRI_l_VPRI_9                                   36
#define MBY_MAP_VPRI_h_VPRI_9                                   39
#define MBY_MAP_VPRI_l_VPRI_8                                   32
#define MBY_MAP_VPRI_h_VPRI_8                                   35
#define MBY_MAP_VPRI_l_VPRI_7                                   28
#define MBY_MAP_VPRI_h_VPRI_7                                   31
#define MBY_MAP_VPRI_l_VPRI_6                                   24
#define MBY_MAP_VPRI_h_VPRI_6                                   27
#define MBY_MAP_VPRI_l_VPRI_5                                   20
#define MBY_MAP_VPRI_h_VPRI_5                                   23
#define MBY_MAP_VPRI_l_VPRI_4                                   16
#define MBY_MAP_VPRI_h_VPRI_4                                   19
#define MBY_MAP_VPRI_l_VPRI_3                                   12
#define MBY_MAP_VPRI_h_VPRI_3                                   15
#define MBY_MAP_VPRI_l_VPRI_2                                   8
#define MBY_MAP_VPRI_h_VPRI_2                                   11
#define MBY_MAP_VPRI_l_VPRI_1                                   4
#define MBY_MAP_VPRI_h_VPRI_1                                   7
#define MBY_MAP_VPRI_l_VPRI_0                                   0
#define MBY_MAP_VPRI_h_VPRI_0                                   3

#define MBY_MAP_PROFILE_KEY0_WIDTH                              2
#define MBY_MAP_PROFILE_KEY0_ENTRIES                            96
#define MBY_MAP_PROFILE_KEY0(index, word)                       ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0038400) + (MBY_MAPPER_BASE))

#define MBY_MAP_PROFILE_KEY0_b_PTRS_ERR                         59
#define MBY_MAP_PROFILE_KEY0_l_EX                               56
#define MBY_MAP_PROFILE_KEY0_h_EX                               58
#define MBY_MAP_PROFILE_KEY0_l_CSUM                             54
#define MBY_MAP_PROFILE_KEY0_h_CSUM                             55
#define MBY_MAP_PROFILE_KEY0_l_RSVD0                            50
#define MBY_MAP_PROFILE_KEY0_h_RSVD0                            53
#define MBY_MAP_PROFILE_KEY0_b_IHL_OK                           49
#define MBY_MAP_PROFILE_KEY0_b_IHL_FITS                         48
#define MBY_MAP_PROFILE_KEY0_l_FLAGS                            1
#define MBY_MAP_PROFILE_KEY0_h_FLAGS                            47
#define MBY_MAP_PROFILE_KEY0_b_RSVD                             0

#define MBY_MAP_PROFILE_KEY_INVERT0_WIDTH                       2
#define MBY_MAP_PROFILE_KEY_INVERT0_ENTRIES                     96
#define MBY_MAP_PROFILE_KEY_INVERT0(index, word)                ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0038800) + (MBY_MAPPER_BASE))

#define MBY_MAP_PROFILE_KEY_INVERT0_b_PTRS_ERR                  59
#define MBY_MAP_PROFILE_KEY_INVERT0_l_EX                        56
#define MBY_MAP_PROFILE_KEY_INVERT0_h_EX                        58
#define MBY_MAP_PROFILE_KEY_INVERT0_l_CSUM                      54
#define MBY_MAP_PROFILE_KEY_INVERT0_h_CSUM                      55
#define MBY_MAP_PROFILE_KEY_INVERT0_l_RSVD0                     50
#define MBY_MAP_PROFILE_KEY_INVERT0_h_RSVD0                     53
#define MBY_MAP_PROFILE_KEY_INVERT0_b_IHL_OK                    49
#define MBY_MAP_PROFILE_KEY_INVERT0_b_IHL_FITS                  48
#define MBY_MAP_PROFILE_KEY_INVERT0_l_FLAGS                     1
#define MBY_MAP_PROFILE_KEY_INVERT0_h_FLAGS                     47
#define MBY_MAP_PROFILE_KEY_INVERT0_b_RSVD                      0

#define MBY_MAP_PROFILE_KEY1_WIDTH                              2
#define MBY_MAP_PROFILE_KEY1_ENTRIES                            96
#define MBY_MAP_PROFILE_KEY1(index, word)                       ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0038C00) + (MBY_MAPPER_BASE))

#define MBY_MAP_PROFILE_KEY1_l_PTYPE                            40
#define MBY_MAP_PROFILE_KEY1_h_PTYPE                            49
#define MBY_MAP_PROFILE_KEY1_l_L2_DOMAIN                        32
#define MBY_MAP_PROFILE_KEY1_h_L2_DOMAIN                        39
#define MBY_MAP_PROFILE_KEY1_l_L3_DOMAIN                        26
#define MBY_MAP_PROFILE_KEY1_h_L3_DOMAIN                        31
#define MBY_MAP_PROFILE_KEY1_l_RSVD                             22
#define MBY_MAP_PROFILE_KEY1_h_RSVD                             25
#define MBY_MAP_PROFILE_KEY1_l_PORT_PROFILE                     14
#define MBY_MAP_PROFILE_KEY1_h_PORT_PROFILE                     21
#define MBY_MAP_PROFILE_KEY1_l_DOMAIN_PROFILE                   6
#define MBY_MAP_PROFILE_KEY1_h_DOMAIN_PROFILE                   13
#define MBY_MAP_PROFILE_KEY1_l_MAC_ROUTABLE                     2
#define MBY_MAP_PROFILE_KEY1_h_MAC_ROUTABLE                     5
#define MBY_MAP_PROFILE_KEY1_l_MAC_MBCAST                       0
#define MBY_MAP_PROFILE_KEY1_h_MAC_MBCAST                       1

#define MBY_MAP_PROFILE_KEY_INVERT1_WIDTH                       2
#define MBY_MAP_PROFILE_KEY_INVERT1_ENTRIES                     96
#define MBY_MAP_PROFILE_KEY_INVERT1(index, word)                ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0039000) + (MBY_MAPPER_BASE))

#define MBY_MAP_PROFILE_KEY_INVERT1_l_PTYPE                     40
#define MBY_MAP_PROFILE_KEY_INVERT1_h_PTYPE                     49
#define MBY_MAP_PROFILE_KEY_INVERT1_l_L2_DOMAIN                 32
#define MBY_MAP_PROFILE_KEY_INVERT1_h_L2_DOMAIN                 39
#define MBY_MAP_PROFILE_KEY_INVERT1_l_L3_DOMAIN                 26
#define MBY_MAP_PROFILE_KEY_INVERT1_h_L3_DOMAIN                 31
#define MBY_MAP_PROFILE_KEY_INVERT1_l_RSVD                      22
#define MBY_MAP_PROFILE_KEY_INVERT1_h_RSVD                      25
#define MBY_MAP_PROFILE_KEY_INVERT1_l_PORT_PROFILE              14
#define MBY_MAP_PROFILE_KEY_INVERT1_h_PORT_PROFILE              21
#define MBY_MAP_PROFILE_KEY_INVERT1_l_DOMAIN_PROFILE            6
#define MBY_MAP_PROFILE_KEY_INVERT1_h_DOMAIN_PROFILE            13
#define MBY_MAP_PROFILE_KEY_INVERT1_l_MAC_ROUTABLE              2
#define MBY_MAP_PROFILE_KEY_INVERT1_h_MAC_ROUTABLE              5
#define MBY_MAP_PROFILE_KEY_INVERT1_l_MAC_MBCAST                0
#define MBY_MAP_PROFILE_KEY_INVERT1_h_MAC_MBCAST                1

#define MBY_MAP_PROFILE_ACTION_WIDTH                            2
#define MBY_MAP_PROFILE_ACTION_ENTRIES                          96
#define MBY_MAP_PROFILE_ACTION(index, word)                     ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0039400) + (MBY_MAPPER_BASE))

#define MBY_MAP_PROFILE_ACTION_b_PROFILE_VALID                  34
#define MBY_MAP_PROFILE_ACTION_l_PROFILE                        28
#define MBY_MAP_PROFILE_ACTION_h_PROFILE                        33
#define MBY_MAP_PROFILE_ACTION_l_REWRITE_PROFILE                24
#define MBY_MAP_PROFILE_ACTION_h_REWRITE_PROFILE                27
#define MBY_MAP_PROFILE_ACTION_b_TRIG_VALID                     23
#define MBY_MAP_PROFILE_ACTION_l_PROFILE_TRIG                   15
#define MBY_MAP_PROFILE_ACTION_h_PROFILE_TRIG                   22
#define MBY_MAP_PROFILE_ACTION_b_PARSER_ERROR                   14
#define MBY_MAP_PROFILE_ACTION_l_IP_OPTIONS_MASK                7
#define MBY_MAP_PROFILE_ACTION_h_IP_OPTIONS_MASK                13
#define MBY_MAP_PROFILE_ACTION_b_PRIOS_VALID                    6
#define MBY_MAP_PROFILE_ACTION_l_VPRI_TGT                       3
#define MBY_MAP_PROFILE_ACTION_h_VPRI_TGT                       5
#define MBY_MAP_PROFILE_ACTION_l_DSCP_TGT                       0
#define MBY_MAP_PROFILE_ACTION_h_DSCP_TGT                       2

#define MBY_MAP_DOMAIN_POL_CFG_WIDTH                            2
#define MBY_MAP_DOMAIN_POL_CFG(word)                            (((word)*4) + (0x0039800) + (MBY_MAPPER_BASE))

#define MBY_MAP_DOMAIN_POL_CFG_l_L3_COLOR_CFG                   3
#define MBY_MAP_DOMAIN_POL_CFG_h_L3_COLOR_CFG                   5
#define MBY_MAP_DOMAIN_POL_CFG_l_L2_COLOR_CFG                   0
#define MBY_MAP_DOMAIN_POL_CFG_h_L2_COLOR_CFG                   2

#define MBY_MAP_REWRITE_WIDTH                                   2
#define MBY_MAP_REWRITE_ENTRIES_0                               32
#define MBY_MAP_REWRITE_ENTRIES_1                               16
#define MBY_MAP_REWRITE(index1, index0, word)                   ((0x0000100) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x003A000) + (MBY_MAPPER_BASE))

#define MBY_MAP_REWRITE_l_SRC_ID                                0
#define MBY_MAP_REWRITE_h_SRC_ID                                5

// --------------------------------------------------------------------------------

#define MBY_N_REALIGN_KEYS                80
#define MBY_N_IS_IP_BITS                  2
#define MBY_N_MAC_ROUTABLE_BITS           4
#define MBY_N_VLAN_ROUTER_ID              2
#define MBY_N_MAP8                        8
#define MBY_N_MAP16                       8
#define MBY_N_REWRITE_KEY8_BITS           32
#define MBY_N_REWRITE_KEY16_BITS          16

#define MBY_OTR_L3_LEN_LIMIT              14
#define MBY_OTR_TUN_LEN_LIMIT             18
#define MBY_INR_L3_LEN_LIMIT              14

#define MBY_L2_MIN_SIZE                   14
#define MBY_MPLS_MIN_SIZE                 0
#define MBY_L3_MIN_SIZE                   20
#define MBY_L4_TCP_MIN_SIZE               18
#define MBY_L4_MIN_SIZE                   8

// --------------------------------------------------------------------------------

#define N_REALIGN_KEYS      80

#define TC_SOURCE_VPRI      0
#define TC_SOURCE_MPLS      1
#define TC_SOURCE_DSCP      2
#define TC_SOURCE_META      3

#define SOURCE_NOOP                 0
#define SOURCE_MAP_OUTER_PROT       2
#define SOURCE_MAP_OUTER_DMAC_H     4
#define SOURCE_MAP_OUTER_DMAC_L     5
#define SOURCE_MAP_OUTER_SMAC_H     6
#define SOURCE_MAP_OUTER_SMAC_L     7
#define SOURCE_MAP_PORT_H           8
#define SOURCE_MAP_PORT_L           9
#define SOURCE_MAP_OUTER_L4_SRC_L   12
#define SOURCE_MAP_OUTER_L4_SRC_H   15
#define SOURCE_MAP_OUTER_L4_DST_L   16
#define SOURCE_MAP_OUTER_L4_DST_H   19
#define SOURCE_PA_FLAGS_L           20
#define SOURCE_PA_FLAGS_H           31
#define SOURCE_FFU_PROFILE_L        32
#define SOURCE_FFU_PROFILE_H        33
#define SOURCE_MAP_INNER_PROT       34
#define SOURCE_MAP_INNER_DMAC_H     36
#define SOURCE_MAP_INNER_DMAC_L     37
#define SOURCE_MAP_INNER_SMAC_H     38
#define SOURCE_MAP_INNER_SMAC_L     39
#define SOURCE_PORT_PROFILE_H       40
#define SOURCE_PORT_PROFILE_L       41
#define SOURCE_MAP_INNER_L4_SRC_L   44
#define SOURCE_MAP_INNER_L4_SRC_H   47
#define SOURCE_MAP_INNER_L4_DST_L   48
#define SOURCE_MAP_INNER_L4_DST_H   51
#define SOURCE_EX                   52
#define SOURCE_CSUM                 53
#define SOURCE_IP_INFO              54
#define SOURCE_PTYPE_L              55
#define SOURCE_PTYPE_H              57
// --------------------------------------------------------------------------------

// Enums:

typedef enum mbyParserFlagsEnum
{
    MBY_PA_FLAGS_NOP                =  0,
    MBY_PA_FLAGS_GENERAL_0          =  1,
    MBY_PA_FLAGS_SUP_L4_CSUM_VAL    =  3,
    MBY_PA_FLAGS_OTR_L4_UDP_V       =  4,
    MBY_PA_FLAGS_OTR_L4_TCP_V       =  5,
    MBY_PA_FLAGS_OTR_L4_SCTP_V      =  6,
    MBY_PA_FLAGS_GENERAL_7          =  7,
    MBY_PA_FLAGS_OTR_ESP_V          =  8,
    MBY_PA_FLAGS_WINDOW_PARSE_V     =  9,
    MBY_PA_FLAGS_OTR_HEAD_FRAG_V    = 10,
    MBY_PA_FLAGS_OTR_PAYLOAD_FRAG_V = 11,
    MBY_PA_FLAGS_OTR_ESP_PROT       = 12,
    MBY_PA_FLAGS_OTR_NAT_T          = 13,
    MBY_PA_FLAGS_GENERAL_1          = 14,
    MBY_PA_FLAGS_OTR_L2_VLAN1       = 18,
    MBY_PA_FLAGS_OTR_L2_VLAN2       = 19,
    MBY_PA_FLAGS_OTR_L2_V2FIRST     = 20,
    MBY_PA_FLAGS_OTR_MPLS_V         = 21,
    MBY_PA_FLAGS_OTR_L3_V           = 22,
    MBY_PA_FLAGS_OTR_L4_V           = 23,
    MBY_PA_FLAGS_GENERAL_24         = 24,
    MBY_PA_FLAGS_INR_L2_V           = 25,
    MBY_PA_FLAGS_INR_L2_VLAN1       = 26,
    MBY_PA_FLAGS_INR_L2_VLAN2       = 27,
    MBY_PA_FLAGS_INR_L2_V2FIRST     = 28,
    MBY_PA_FLAGS_INR_MPLS_V         = 29,
    MBY_PA_FLAGS_INR_L3_V           = 30,
    MBY_PA_FLAGS_INR_L4_V           = 31,
    MBY_PA_FLAGS_OTR_OPT_FLAGS      = 32,
    MBY_PA_FLAGS_INR_OPT_FLAGS      = 38,
    MBY_PA_FLAGS_GENERAL_2          = 44

} mbyParserFlags;

typedef enum mbyParserKeysEnum
{
    MBY_PA_KEYS_INNER_DMAC          =  0,
    MBY_PA_KEYS_INNER_SMAC          =  3,
    MBY_PA_KEYS_OUTER_DMAC          =  6,
    MBY_PA_KEYS_OUTER_SMAC          =  9,
    MBY_PA_KEYS_OUTER_ETYPE         = 12,
    MBY_PA_KEYS_OUTER_VLAN1         = 14,
    MBY_PA_KEYS_OUTER_VLAN2         = 15,
    MBY_PA_KEYS_OUTER_L4SRC         = 16,
    MBY_PA_KEYS_OUTER_L4DST         = 17,
    MBY_PA_KEYS_INNER_ETYPE         = 18,
    MBY_PA_KEYS_INNER_VLAN1         = 20,
    MBY_PA_KEYS_INNER_VLAN2         = 21,
    MBY_PA_KEYS_INNER_L4SRC         = 22,
    MBY_PA_KEYS_INNER_L4DST         = 23,
    MBY_PA_KEYS_MPLS                = 24,
    MBY_PA_KEYS_GENERAL             = 32,
    MBY_PA_KEYS_INNER_IP_HEADER     = 36,
    MBY_PA_KEYS_OUTER_IP_HEADER     = 42,
    MBY_PA_KEYS_OUTER_SIPDIP        = 48,
    MBY_PA_KEYS_INNER_SIPDIP        = 64

} mbyParserKeys;

typedef enum mbyParserPtrsIndexEnum
{
    MBY_PA_PTRS_NOP          = 0,
    MBY_PA_PTRS_OTR_MPLS_PTR = 1,
    MBY_PA_PTRS_OTR_L3_PTR   = 2,
    MBY_PA_PTRS_OTR_L4_PTR   = 3,
    MBY_PA_PTRS_INR_L2_PTR   = 4,
    MBY_PA_PTRS_INR_MPLS_PTR = 5,
    MBY_PA_PTRS_INR_L3_PTR   = 6,
    MBY_PA_PTRS_INR_L4_PTR   = 7,
    MBY_PA_PTRS_ESP_PTR      = 7 // Same location as INR_L4

} mbyParserPtrsIndex;

typedef enum mbyParserInfoIndexEnum
{
    MBY_PA_INFO_OTR_L2   = 0,
    MBY_PA_INFO_OTR_MPLS = 1,
    MBY_PA_INFO_OTR_L3   = 2,
    MBY_PA_INFO_OTR_L4   = 3,
    MBY_PA_INFO_INR_L2   = 4,
    MBY_PA_INFO_INR_MPLS = 5,
    MBY_PA_INFO_INR_L3   = 6,
    MBY_PA_INFO_INR_L4   = 7

} mbyParserInfoIndex;

typedef enum mbyRealignKeysEnum
{
    MBY_RE_KEYS_INNER_DMAC          =  0,
    MBY_RE_KEYS_INNER_SMAC          =  3,
    MBY_RE_KEYS_OUTER_DMAC          =  6,
    MBY_RE_KEYS_OUTER_SMAC          =  9,
    MBY_RE_KEYS_OUTER_ETYPE         = 12,
    MBY_RE_KEYS_OUTER_VLAN1         = 14,
    MBY_RE_KEYS_OUTER_VLAN2         = 15,
    MBY_RE_KEYS_OUTER_L4SRC         = 16,
    MBY_RE_KEYS_OUTER_L4DST         = 17,
    MBY_RE_KEYS_INNER_ETYPE         = 18,
    MBY_RE_KEYS_INNER_VLAN1         = 20,
    MBY_RE_KEYS_INNER_VLAN2         = 21,
    MBY_RE_KEYS_INNER_L4SRC         = 22,
    MBY_RE_KEYS_INNER_L4DST         = 23,
    MBY_RE_KEYS_MPLS                = 24,
    MBY_RE_KEYS_GENERAL_8B          = 32,
    MBY_RE_KEYS_INNER_IP_TTL_PROT   = 36,
    MBY_RE_KEYS_INNER_IP_LEN        = 37,
    MBY_RE_KEYS_INNER_IP_DS_FLOW    = 38,
    MBY_RE_KEYS_INNER_IP_FLOW       = 39,
    MBY_RE_KEYS_IP_ISL0_MSB         = 40,
    MBY_RE_KEYS_IP_ISL0_LSB         = 41,
    MBY_RE_KEYS_OUTER_IP_TTL_PROT   = 42,
    MBY_RE_KEYS_OUTER_IP_LEN        = 43,
    MBY_RE_KEYS_OUTER_IP_DS_FLOW    = 44,
    MBY_RE_KEYS_OUTER_IP_FLOW       = 45,
    MBY_RE_KEYS_SGLORT              = 46,
    MBY_RE_KEYS_DGLORT              = 47,
    MBY_RE_KEYS_OUTER_SIP           = 48,
    MBY_RE_KEYS_OUTER_DIP           = 56,
    MBY_RE_KEYS_INNER_SIP           = 64,
    MBY_RE_KEYS_INNER_DIP           = 72

} mbyRealignKeys;

typedef enum mbyPortDefaultsEnum
{
    MBY_DEFAULT_TARGET_KEYS_L       =   0,
    MBY_DEFAULT_TARGET_KEYS_H       =  79,
    MBY_DEFAULT_TARGET_FORCE_KEYS_L =  80,
    MBY_DEFAULT_TARGET_FORCE_KEYS_H =  95,
    MBY_DEFAULT_TARGET_ACT24_L_L    =  96,
    MBY_DEFAULT_TARGET_ACT24_L_H    = 111,
    MBY_DEFAULT_TARGET_ACT24_U_L    = 112,
    MBY_DEFAULT_TARGET_ACT24_U_H    = 127,
    MBY_DEFAULT_TARGET_ACT4_4_L     = 128,
    MBY_DEFAULT_TARGET_ACT4_4_H     = 159,
    MBY_DEFAULT_TARGET_ACT4_2_L     = 160,
    MBY_DEFAULT_TARGET_ACT4_2_H     = 191,
    MBY_DEFAULT_TARGET_ACT4_1_L     = 192,
    MBY_DEFAULT_TARGET_ACT4_1_H     = 223,
    MBY_DEFAULT_TARGET_ACT1_FLAGS   = 224

} mbyPortDefaults;

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

// Structs:

typedef struct mbyMapPortCfgStruct
{
    fm_uint16 DEFAULT_SGLORT;
    fm_bool   DEFAULT_SGLORT_EN;
    fm_byte   PORT_PROFILE;

} mbyMapPortCfg;

typedef struct mbyMapPortDefaultsStruct
{
    fm_uint16 VALUE;
    fm_byte   TARGET;

} mbyMapPortDefaults;

typedef struct mbyMapDomainTcamStruct
{
    fm_uint32 _RSVD1_;
    fm_uint32 PORT_KEY_INVERT;
    fm_bool   VID2_VALID_INVERT;
    fm_uint16 VID2_KEY_INVERT;
    fm_bool   VID1_VALID_INVERT;
    fm_uint16 VID1_KEY_INVERT;
    fm_uint32 _RSVD0_;
    fm_uint32 PORT_KEY;
    fm_bool   VID2_VALID;
    fm_uint16 VID2_KEY;
    fm_bool   VID1_VALID;
    fm_uint16 VID1_KEY;

} mbyMapDomainTcam;

typedef struct mbyMapProfKey0Struct
{
    fm_bool   PTRS_ERR;
    fm_byte   EX;
    fm_byte   CSUM;
    fm_byte   IP_FITS;
    fm_bool   IHL_OK;
    fm_bool   IHL_FITS;
    fm_uint64 FLAGS;

} mbyMapProfKey0;

typedef struct mbyMapProfKey1Struct
{
    fm_uint16 PTYPE;
    fm_byte   L2_DOMAIN;
    fm_byte   L3_DOMAIN;
    fm_byte   IP_SCENARIO;
    fm_byte   PORT_PROFILE;
    fm_byte   DOMAIN_PROFILE;
    fm_byte   MAC_ROUTABLE;
    fm_byte   MAC_MBCAST;

} mbyMapProfKey1;

typedef struct mbyMappedKeyStruct
{
    fm_byte   MAP_OUTER_PROT;  // 4b field
    fm_byte   MAP_OUTER_ETYPE; // 4b field
    fm_byte   MAP_INNER_PROT;  // 4b field
    fm_byte   MAP_INNER_ETYPE; // 4b field
    fm_byte   MAP_OUTER_DMAC;
    fm_byte   MAP_OUTER_SMAC;
    fm_byte   MAP_INNER_DMAC;
    fm_byte   MAP_INNER_SMAC;
    fm_byte   MAP_OUTER_DIP;   // 4b field
    fm_byte   MAP_OUTER_SIP;   // 4b field
    fm_byte   MAP_INNER_DIP;   // 4b field
    fm_byte   MAP_INNER_SIP;   // 4b field
    fm_byte   MAP_PORT;
    fm_uint16 MAP_OUTER_L4_DST;
    fm_uint16 MAP_OUTER_L4_SRC;
    fm_uint16 MAP_INNER_L4_DST;
    fm_uint16 MAP_INNER_L4_SRC;

} mbyMappedKey;

typedef struct mbyMapProfActionStruct
{
    fm_bool   PROFILE_VALID;
    fm_byte   PROFILE;
    fm_byte   REWRITE_PROFILE;
    fm_bool   TRIG_VALID;
    fm_byte   PROFILE_TRIG;
    fm_bool   PARSER_ERROR;
    fm_byte   IP_OPTIONS_MASK;
    fm_bool   PRIOS_VALID;
    fm_byte   VPRI_TGT;
    fm_byte   DSCP_TGT;

} mbyMapProfAction;

typedef struct mbyMapRewriteStruct
{
    fm_byte   SRC_ID;

} mbyMapRewrite;

// Function prototypes:

void Mapper
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyParserToMapper     * const in, 
          mbyMapperToClassifier * const out,
          mbyParserToModifier   * const parser_to_modifier
);

#endif // MBY_MAPPER_H
