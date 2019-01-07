// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_PARSER_H
#define MBY_PARSER_H

// Includes:

#include "mby_bitfield.h"

// Defines:

//efine MBY_PA_ANA_STAGES              32    // 32 stages
#define MBY_PA_ANA_STAGES              mby_ppe_parser_map_PARSER_ANA_S__nd

//efine MBY_PA_ANA_RULES               16    // 16 rules per stage
#define MBY_PA_ANA_RULES               parser_ana_s_rf_PARSER_ANA_S__nd

#define MBY_PA_PTYPE_ENTRIES           parser_ptype_tcam_rf_PARSER_PTYPE_TCAM__nd

#define MBY_PA_MAX_SEG_LEN             192
#define MBY_PA_MAX_PTR_LEN             255
#define MBY_PA_MAX_DATA_SZ             16384
#define MBY_PSEUDOHEADER_SIZE          40

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
#define MBY_PA_PROT_ID_NOP             0xFF

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

typedef struct mbyRxMacToParserStruct
{
    fm_uint32        RX_PORT;             ///< Ingress port
    fm_uint32        RX_LENGTH;
    fm_byte          SEG_DATA[MBY_PA_MAX_SEG_LEN];
} mbyRxMacToParser;

typedef struct mbyParserToMapperStruct
{
    fm_uint16        PA_ADJ_SEG_LEN;                   ///< Adjusted segment length
    fm_byte          PA_CSUM_OK;                       ///< Checksum OK result for outer (bit 0) and inner (bit 1) IPv4 headers
    fm_bool          PA_DROP;                          ///< Checksum validation error, drop pkt in tail
    fm_bool          PA_EX_DEPTH_EXCEED;               ///< Parser stopped: EOS exception and segment was not EOP
    fm_bool          PA_EX_PARSING_DONE;               ///< Parser stopped: Parsing Done exception
    fm_byte          PA_EX_STAGE;                      ///< Analyzer stage where exception occurred
    fm_bool          PA_EX_TRUNC_HEADER;               ///< Parser stopped: EOS exception and segment was EOP:
    fm_bool          PA_FLAGS     [MBY_N_PARSER_FLGS]; ///< Parser flags assigned by extract
    fm_uint16        PA_KEYS      [MBY_N_PARSER_KEYS]; ///< 16-bit parser keys
    fm_bool          PA_KEYS_VALID[MBY_N_PARSER_KEYS]; ///< Parser keys valid flags
    fm_bool          PA_L3LEN_ERR;                     ///< L3 length error
    fm_uint16        PA_PACKET_TYPE;                   ///< Packet type (new for MBY)
    mbyParserHdrPtrs PA_HDR_PTRS;                      ///< Parser header pointers
    fm_uint32        RX_PORT;                          ///< Ingress port
    fm_uint32        RX_LENGTH;
} mbyParserToMapper;

void Parser
(
    mby_ppe_parser_map    const * const parser_map,
    mbyRxMacToParser      const * const in,
    mbyParserToMapper           * const out
);


#endif
