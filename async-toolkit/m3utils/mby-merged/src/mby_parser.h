// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_PARSER_H
#define MBY_PARSER_H

#include "mby_common.h"
#include "mby_parser_defines.h"

// Structs:

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

typedef struct mbyParserInfoStruct
{
    fm_byte                 otr_l2_len;    // 3b field 
    fm_bool                 otr_l2_vlan1;
    fm_bool                 otr_l2_vlan2;
    fm_bool                 otr_l2_v2first;
    fm_byte                 otr_mpls_len;  // 3b field
    fm_byte                 otr_l3_len;    // 4b field
    fm_bool                 otr_l3_v6;
    fm_bool                 otr_l4_udp;
    fm_bool                 otr_l4_tcp;
    fm_byte                 otr_tun_len;   // 5b field
    fm_byte                 inr_l2_len;    // 3b field
    fm_bool                 inr_l2_vlan1;
    fm_bool                 inr_l2_vlan2;
    fm_bool                 inr_l2_v2first;
    fm_byte                 inr_mpls_len;  // 3b field
    fm_byte                 inr_l3_len;    // 4b field
    fm_bool                 inr_l3_v6;
    fm_bool                 inr_l4_udp;
    fm_bool                 inr_l4_tcp;  

} mbyParserInfo;

typedef struct mbyParserToModifierStruct
{
    /* parser_info to be used for modify */
    mbyParserInfo           PARSER_INFO;

} mbyParserToModifier;

// Function prototypes:

void Parser
(
    fm_uint32                       regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyMacToParser    * const in, 
          mbyParserToMapper * const out
);

#endif // MBY_PARSER_H
