// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_PARSER_H
#define MBY_PARSER_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"

// Defines:

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

#define MBY_PA_MAX_SEG_LEN             192
#define MBY_PA_MAX_PTR_LEN             255
#define MBY_PA_MAX_DATA_SZ             16384
#define MBY_PSEUDOHEADER_SIZE          40
#define MBY_PA_ANA_STAGES              32    // 32 stages
#define MBY_PA_ANA_RULES               16    // 16 rules per stage
#define MBY_OTR_IPHDR_KEY              42    // Note: if IPv6, add 2
#define MBY_OTR_IPADDR_KEY             48
#define MBY_L4CSUM_KEY                 32
#define MBY_L4LEN_KEY                  35

#define MBY_OTR_L3_PTR                 2
#define MBY_OTR_L4_PTR                 3
#define MBY_PA_OTR_L4_UDP_V_FLAG       4     // otr_l4_udp_v flag
#define MBY_PA_OTR_L4_TCP_V_FLAG       5     // otr_l4_tcp_v flag
#define MBY_PA_OTR_HEAD_FRAG_V_FLAG    10    // otr_head_frag_v flag
#define MBY_PA_OTR_PAYLOAD_FRAG_V_FLAG 11    // otr_payload_frag_v flag
#define MBY_PA_OTR_L3_V_FLAG           22    // otr_l3_v flag
#define MBY_PA_ANA_OP_MASK_BITS        0xFFF
#define MBY_PA_ANA_OP_ROT_BITS         0x0F
#define MBY_PA_ANA_OP_ROT_SHIFT        0xC

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

typedef struct mbyMacToParserStruct
{
    // The ingress packet data
    fm_byte                *RX_DATA;

    // The ingress packet data length in units of bytes
    fm_uint32               RX_LENGTH;

    // The ingress port
    fm_int                  RX_PORT;

    /* Packet Meta Data. */
    fm_byte                 PKT_META[32];

} mbyMacToParser;

typedef struct mbyParserToMapperStruct
{
    // The ingress port (pass-thru):
    fm_int                  RX_PORT;

    // Packet meta data (pass-thru):
    fm_byte                 PKT_META[32];

    /* The 4-bit set of RX EPL flags. Bits [7:3] are reserved and always
     * set to zero. For bits [2:0] see also ''mbyRxFlags''. */
    fm_byte                 RX_FLAGS;

    /* Pkt Seg Meta Err. Valid on EOP. */
//  fm_bool                 SEG_META_ERR;

    /* Adjusted segment length. */
    fm_uint16               PA_ADJ_SEG_LEN;

    /* 16-bit Parser keys extracted from packet. */
    fm_uint16               PA_KEYS[84];

    /* Boolean valid bits to match Parser keys assigned by extract actions for packet. */
    fm_bool                 PA_KEYS_VALID[84];

    /* Parser flag values assigned per bit by extract actions. */
    fm_bool                 PA_FLAGS[48];

    /* Byte offsets of interest within packet resulting from Parser extract
     * actions. */
    fm_byte                 PA_PTRS[8];

    /* Boolean valid bits to match pointers of interest within packet from
     * Parser extract actions. */
    fm_bool                 PA_PTRS_VALID[8];

    /* Checksum OK result for outer (bit 0) and inner (bit 1) IPv4 headers. */
    fm_byte                 PA_CSUM_OK;

    /* Parser analyzer stage where exception was reached. */
    fm_byte                 PA_EX_STAGE;

    /* Parser stopped: EOS exception and segment was not EOP. */
    fm_bool                 PA_EX_DEPTH_EXCEED;

    /* Parser stopped: EOS exception and segment was EOP. */
    fm_bool                 PA_EX_TRUNC_HEADER;

    /* Parser stopped: Parsing Done exception. */
    fm_bool                 PA_EX_PARSING_DONE;

    /* Checksum validation error, drop pkt in Tail. */
    fm_bool                 PA_DROP;

    /* L3 length error. */
    fm_bool                 PA_L3LEN_ERR;

    /* packet type (added for MBY) */
    fm_byte                 PA_PACKET_TYPE;

} mbyParserToMapper;

#endif // MBY_PARSER_H
