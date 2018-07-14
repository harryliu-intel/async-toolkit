// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_PARSER_DEFINES_H
#define MBY_PARSER_DEFINES_H

#define MBY_PARSER_BASE                                         (0x2000000)
#define MBY_PARSER_SIZE                                         (0x0010000)

#define MBY_PARSER_PORT_CFG_WIDTH                               2
#define MBY_PARSER_PORT_CFG_ENTRIES                             24
#define MBY_PARSER_PORT_CFG(index, word)                        ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0001000) + (MBY_PARSER_BASE))

#define MBY_PARSER_PORT_CFG_l_INITIAL_W0_OFFSET                 56
#define MBY_PARSER_PORT_CFG_h_INITIAL_W0_OFFSET                 63
#define MBY_PARSER_PORT_CFG_l_INITIAL_W1_OFFSET                 48
#define MBY_PARSER_PORT_CFG_h_INITIAL_W1_OFFSET                 55
#define MBY_PARSER_PORT_CFG_l_INITIAL_W2_OFFSET                 40
#define MBY_PARSER_PORT_CFG_h_INITIAL_W2_OFFSET                 47
#define MBY_PARSER_PORT_CFG_l_INITIAL_PTR                       32
#define MBY_PARSER_PORT_CFG_h_INITIAL_PTR                       39
#define MBY_PARSER_PORT_CFG_l_INITIAL_STATE                     16
#define MBY_PARSER_PORT_CFG_h_INITIAL_STATE                     31
#define MBY_PARSER_PORT_CFG_l_INITIAL_OP_MASK                   4
#define MBY_PARSER_PORT_CFG_h_INITIAL_OP_MASK                   15
#define MBY_PARSER_PORT_CFG_l_INITIAL_OP_ROT                    0
#define MBY_PARSER_PORT_CFG_h_INITIAL_OP_ROT                    3

#define MBY_PARSER_KEY_W_WIDTH                                  2
#define MBY_PARSER_KEY_W_ENTRIES_0                              16
#define MBY_PARSER_KEY_W_ENTRIES_1                              32
#define MBY_PARSER_KEY_W(index1, index0, word)                  ((0x0000080) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0002000) + (MBY_PARSER_BASE))

#define MBY_PARSER_KEY_W_l_W1_VALUE                             48
#define MBY_PARSER_KEY_W_h_W1_VALUE                             63
#define MBY_PARSER_KEY_W_l_W1_MASK                              32
#define MBY_PARSER_KEY_W_h_W1_MASK                              47
#define MBY_PARSER_KEY_W_l_W0_VALUE                             16
#define MBY_PARSER_KEY_W_h_W0_VALUE                             31
#define MBY_PARSER_KEY_W_l_W0_MASK                              0
#define MBY_PARSER_KEY_W_h_W0_MASK                              15

#define MBY_PARSER_KEY_S_WIDTH                                  2
#define MBY_PARSER_KEY_S_ENTRIES_0                              16
#define MBY_PARSER_KEY_S_ENTRIES_1                              32
#define MBY_PARSER_KEY_S(index1, index0, word)                  ((0x0000080) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0003000) + (MBY_PARSER_BASE))

#define MBY_PARSER_KEY_S_l_STATE_VALUE                          16
#define MBY_PARSER_KEY_S_h_STATE_VALUE                          31
#define MBY_PARSER_KEY_S_l_STATE_MASK                           0
#define MBY_PARSER_KEY_S_h_STATE_MASK                           15

#define MBY_PARSER_ANA_W_WIDTH                                  2
#define MBY_PARSER_ANA_W_ENTRIES_0                              16
#define MBY_PARSER_ANA_W_ENTRIES_1                              32
#define MBY_PARSER_ANA_W(index1, index0, word)                  ((0x0000080) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0004000) + (MBY_PARSER_BASE))

#define MBY_PARSER_ANA_W_l_NEXT_W0_OFFSET                       24
#define MBY_PARSER_ANA_W_h_NEXT_W0_OFFSET                       31
#define MBY_PARSER_ANA_W_l_NEXT_W1_OFFSET                       16
#define MBY_PARSER_ANA_W_h_NEXT_W1_OFFSET                       23
#define MBY_PARSER_ANA_W_l_NEXT_W2_OFFSET                       8
#define MBY_PARSER_ANA_W_h_NEXT_W2_OFFSET                       15
#define MBY_PARSER_ANA_W_l_SKIP                                 0
#define MBY_PARSER_ANA_W_h_SKIP                                 7

#define MBY_PARSER_ANA_S_WIDTH                                  2
#define MBY_PARSER_ANA_S_ENTRIES_0                              16
#define MBY_PARSER_ANA_S_ENTRIES_1                              32
#define MBY_PARSER_ANA_S(index1, index0, word)                  ((0x0000080) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0005000) + (MBY_PARSER_BASE))

#define MBY_PARSER_ANA_S_l_NEXT_STATE                           32
#define MBY_PARSER_ANA_S_h_NEXT_STATE                           47
#define MBY_PARSER_ANA_S_l_NEXT_STATE_MASK                      16
#define MBY_PARSER_ANA_S_h_NEXT_STATE_MASK                      31
#define MBY_PARSER_ANA_S_l_NEXT_OP                              0
#define MBY_PARSER_ANA_S_h_NEXT_OP                              15

#define MBY_PARSER_EXC_WIDTH                                    2
#define MBY_PARSER_EXC_ENTRIES_0                                16
#define MBY_PARSER_EXC_ENTRIES_1                                32
#define MBY_PARSER_EXC(index1, index0, word)                    ((0x0000080) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0006000) + (MBY_PARSER_BASE))

#define MBY_PARSER_EXC_l_EX_OFFSET                              1
#define MBY_PARSER_EXC_h_EX_OFFSET                              8
#define MBY_PARSER_EXC_b_PARSING_DONE                           0

#define MBY_PARSER_EXT_WIDTH                                    2
#define MBY_PARSER_EXT_ENTRIES_0                                32
#define MBY_PARSER_EXT_ENTRIES_1                                32
#define MBY_PARSER_EXT(index1, index0, word)                    ((0x0000100) * ((index1) - 0) + (0x0000008) * ((index0) - 0) + ((word)*4)+ (0x0008000) + (MBY_PARSER_BASE))

#define MBY_PARSER_EXT_l_KEY_START                              25
#define MBY_PARSER_EXT_h_KEY_START                              31
#define MBY_PARSER_EXT_l_KEY_LEN                                18
#define MBY_PARSER_EXT_h_KEY_LEN                                24
#define MBY_PARSER_EXT_l_KEY_OFFSET                             10
#define MBY_PARSER_EXT_h_KEY_OFFSET                             17
#define MBY_PARSER_EXT_l_FLAG_NUM                               4
#define MBY_PARSER_EXT_h_FLAG_NUM                               9
#define MBY_PARSER_EXT_b_FLAG_VALUE                             3
#define MBY_PARSER_EXT_l_PTR_NUM                                0
#define MBY_PARSER_EXT_h_PTR_NUM                                2

#define MBY_PARSER_CSUM_CFG_WIDTH                               2
#define MBY_PARSER_CSUM_CFG_ENTRIES                             24
#define MBY_PARSER_CSUM_CFG(index, word)                        ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0001100) + (MBY_PARSER_BASE))

#define MBY_PARSER_CSUM_CFG_l_VALIDATE_L4_CSUM                  4
#define MBY_PARSER_CSUM_CFG_h_VALIDATE_L4_CSUM                  5
#define MBY_PARSER_CSUM_CFG_b_STORE_L4_PARTIAL_CSUM             3
#define MBY_PARSER_CSUM_CFG_b_COMPUTE_L4_CSUM                   2
#define MBY_PARSER_CSUM_CFG_l_VALIDATE_L3_LENGTH                0
#define MBY_PARSER_CSUM_CFG_h_VALIDATE_L3_LENGTH                1

#endif
