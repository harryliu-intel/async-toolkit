// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_COMMON_H
#define MBY_COMMON_H

// Includes:
#include "mby_parser_defines.h"
#include "mby_classifier_defines.h"

#define MBY_N_PARSER_KEYS                 80
#define MBY_N_PARSER_FLAGS                48
#define MBY_N_PARSER_PTRS                 8

// Defines:
#define FM_LITERAL_U64(x) (x ## ULL)

// Get a named field of 2-32 bits within a 32-bit value
#define FM_GET_FIELD(rvalue, regname, fieldname) \
    ( (rvalue >> regname ## _l_ ## fieldname) &  \
     ( ( 2U << (regname ## _h_ ## fieldname - regname ## _l_ ## fieldname) ) - 1 ) )

// Set a named field of 2-32 bits within a 32-bit value
#define FM_SET_FIELD(lvalue, regname, fieldname, fieldvalue)                                   \
    (lvalue ^= ( ( (lvalue >> regname ## _l_ ## fieldname) ^ fieldvalue ) &                    \
                ( ( 2U << (regname ## _h_ ## fieldname - regname ## _l_ ## fieldname) ) - 1 ) ) \
               << regname ## _l_ ## fieldname)

// Get a named field of 2-64 bits within a 64-bit value.
#define FM_GET_FIELD64(rvalue, regname, fieldname)                                            \
    ( (rvalue >> regname ## _l_ ## fieldname) &                                               \
     ( ( FM_LITERAL_U64(2) << (regname ## _h_ ## fieldname - regname ## _l_ ## fieldname) ) - \
      FM_LITERAL_U64(1) ) )


// Set a named field of 2-64 bits within a 64-bit value
#define FM_SET_FIELD64(lvalue, regname, fieldname, fieldvalue)                                           \
    (lvalue ^= ( ( (lvalue >> regname ## _l_ ## fieldname) ^ fieldvalue ) &                              \
                ( ( FM_LITERAL_U64(2) << (regname ## _h_ ## fieldname - regname ## _l_ ## fieldname) ) - \
                 FM_LITERAL_U64(1) ) ) << regname ## _l_ ## fieldname)

// Extract a field of 32 or fewer bits from an unnamed 32-bit value
#define FM_GET_UNNAMED_FIELD(lvalue, start, len) \
    ((lvalue >> (start)) & ((1 << (len)) - 1))

// Set a field of 32 or fewer bits for an unnamed 32-bit value
#define FM_SET_UNNAMED_FIELD(lvalue, start, len, value) \
    lvalue &= ~(((1 << (len)) - 1) << (start)); \
    lvalue |= ((value) & ((1 << (len)) - 1)) << (start); 

// Extract a field of 64 or fewer bits from an unnamed 64-bit value.
#define FM_GET_UNNAMED_FIELD64(lvalue, start, len) \
    ((lvalue >> (start)) & ((FM_LITERAL_U64(1) << (len)) - FM_LITERAL_U64(1))) 

// Set a field of 64 or fewer bits for an unnamed 64-bit value
#define FM_SET_UNNAMED_FIELD64(lvalue, start, len, value) \
    lvalue &= ~(((FM_LITERAL_U64(1) << (len)) - FM_LITERAL_U64(1)) << (start)); \
    lvalue |= ((value) & ((FM_LITERAL_U64(1) << (len)) - FM_LITERAL_U64(1))) << (start); 

// Get a named field of 2-32 bits within a >64-bit value
#define FM_ARRAY_GET_FIELD(array, regname, fieldname)            \
    fmMultiWordBitfieldGet32(array, regname ## _h_ ## fieldname, \
                             regname ## _l_ ## fieldname)

// Set a named field of 2-32 bits within a >64-bit value
#define FM_ARRAY_SET_FIELD(array, regname, fieldname, fieldvalue) \
    fmMultiWordBitfieldSet32(array, regname ## _h_ ## fieldname,  \
                             regname ## _l_ ## fieldname, fieldvalue)

// Get a named field of 2-32 bits within an array of 64-bit values
#define FM_ARRAY_GET_FIELD64(array, regname, fieldname)        \
    fmMultiWordBitfieldGet64(array, regname ## _h_ ## fieldname, \
                             regname ## _l_ ## fieldname)

// Set a named field of 33-64 bits within a >64-bit value
#define FM_ARRAY_SET_FIELD64(array, regname, fieldname, fieldvalue) \
    fmMultiWordBitfieldSet64(array, regname ## _h_ ## fieldname,    \
                             regname ## _l_ ## fieldname, fieldvalue)

// Extract an unnamed field of 32 or fewer bits from a >64-bit value
#define FM_ARRAY_GET_UNNAMED_FIELD(array, start, len) \
    fmMultiWordBitfieldGet32((array), (start) + (len) - 1, (start))

// Set an unnamed field of 32 or fewer bits within a >64-bit value
#define FM_ARRAY_SET_UNNAMED_FIELD(array, start, len, value) \
    fmMultiWordBitfieldSet32((array), (start) + (len) - 1, (start), (value))

// Get a named field of 1 bit within a 32-bit value
#define FM_GET_BIT(rvalue, regname, bitname) \
    ( (rvalue >> regname ## _b_ ## bitname) & 1 )

// Set a named field of 1 bit within a 32-bit value
#define FM_SET_BIT(lvalue, regname, bitname, bitvalue)          \
    ( lvalue = ( lvalue & ~(1 << regname ## _b_ ## bitname) ) | \
               ( (bitvalue & 1) << regname ## _b_ ## bitname ) )

// Get a named field of 1 bit within a 64-bit value
#define FM_GET_BIT64(rvalue, regname, bitname) \
    ( (rvalue >> regname ## _b_ ## bitname) & 1 )

// Set a named field of 1 bit within a 64-bit value.
#define FM_SET_BIT64(lvalue, regname, bitname, bitvalue)                      \
    ( lvalue = ( lvalue & ~(FM_LITERAL_U64(1) << regname ## _b_ ## bitname) ) \
               | ( ( bitvalue & FM_LITERAL_U64(1) ) << regname ## _b_ ## bitname ) )

// Get a named field of 1 bit within a >64-bit value
#define FM_ARRAY_GET_BIT(array, regname, bitname)              \
    fmMultiWordBitfieldGet32(array, regname ## _b_ ## bitname, \
                             regname ## _b_ ## bitname)

// Set a named field of 1 bit within a >64-bit value
#define FM_ARRAY_SET_BIT(array, regname, bitname, bitvalue)    \
    fmMultiWordBitfieldSet32(array, regname ## _b_ ## bitname, \
                             regname ## _b_ ## bitname, bitvalue)

// Extract an unnamed bit from a >64-bit value
#define FM_ARRAY_GET_UNNAMED_BIT(array, bit) \
    fmMultiWordBitfieldGet32((array), (bit), (bit))

// Set an unnamed field of 32 or fewer bits within a >64-bit value
#define FM_ARRAY_SET_UNNAMED_BIT(array, bit, value) \
    fmMultiWordBitfieldSet32((array), (bit), (bit), (value))

#define MBY_REGISTER_ARRAY_SIZE 0x1800000

// Basic Data Types:
typedef char                  fm_char;
typedef short                 fm_int16;
typedef int                   fm_int32;
typedef long long             fm_int64;
typedef int                   fm_int;

typedef unsigned char         fm_bool;
typedef unsigned char         fm_byte;
typedef unsigned int          fm_uint;
typedef unsigned short        fm_uint16;
typedef unsigned int          fm_uint32;
typedef unsigned long long    fm_uint64;

typedef char                 *fm_text;

// FM Data Types:
typedef fm_int                fm_status;
typedef unsigned long long    fm_macaddr;

// Constants:
const fm_status  FM_OK   = 0;
const fm_status  FM_FAIL = 1;
const fm_bool    TRUE    = 1;
const fm_bool    FALSE   = 0;

// External function prototypes:

fm_uint32 fmMultiWordBitfieldGet32(const fm_uint32 *array, fm_int hiBit, fm_int loBit);
fm_uint64 fmMultiWordBitfieldGet64(const fm_uint32 *array, fm_int hiBit, fm_int loBit);

void      fmMultiWordBitfieldSet32(fm_uint32 *array, fm_int hiBit, fm_int loBit, fm_uint32 value);
void      fmMultiWordBitfieldSet64(fm_uint32 *array, fm_int hiBit, fm_int loBit, fm_uint64 value);

fm_bool   fmModelIsMulticastMacAddress(fm_macaddr keyMac);
fm_bool   fmModelIsBroadcastMacAddress(fm_macaddr keyMac);

fm_status mbyModelReadCSR(fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
                          const fm_uint32 byte_addr,
                          fm_uint32 *value);

fm_status mbyModelReadCSRMult(fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
                              const fm_uint32 byte_addr,
                              const fm_int len,
                              fm_uint32 *value);

// Enums:

typedef enum mbyFfuKey8Enum
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

} mbyFfuKey8;

typedef enum mbyFfuKey16Enum
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

} mbyFfuKey16;

// Structs:

typedef struct _mbyMacToParser
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

typedef struct _mbyParserToMapper
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

typedef struct mbyClassifierKeysStruct
{
    fm_uint32               key32[MBY_FFU_N_KEY32];
    fm_uint16               key16[MBY_FFU_N_KEY16];
    fm_byte                 key8 [MBY_FFU_N_KEY8 ];

} mbyClassifierKeys;

typedef struct mbyPrecValStruct
{
    fm_byte                 prec; // 3b field
    // act24.val is 24b, act4.val is 4b, act1.val is 1b
    fm_uint32               val;  

} mbyPrecVal;

typedef struct mbyClassifierActionsStruct
{
    mbyPrecVal              act24[MBY_FFU_N_ACT24];
    mbyPrecVal              act4 [MBY_FFU_N_ACT4 ];
    mbyPrecVal              act1 [MBY_FFU_N_ACT1 ];

} mbyClassifierActions;

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

typedef struct mbyMapperToClassifierStruct
{
    /* Boolean indicating whether a header parse error has occurred. */
    fm_bool                 PARSER_ERROR;

    /* Keys to be used for FFU TCAM lookup */
    mbyClassifierKeys       FFU_KEYS;

    /* Actions generated by FFU lookup */
    mbyClassifierActions    FFU_ACTIONS;

    /* The 6-bit FFU scenario. */
    fm_byte                 FFU_SCENARIO;

    /* The 4-bit FFU vrid. */
    fm_byte                 FFU_VRID;

    /* ip_option to be used for counting trap_ip_iptions */
    fm_bool                 IP_OPTION[2];

    /* The 5-bit FFU priority profile. */
    fm_byte                 PRIORITY_PROFILE;

    /* Mapper decision for using default priority */
    fm_bool                 NO_PRI_ENC;

    /* FFU Group Keys feeding to next group. Per FFU Group data to be used by DV. */
//  mbyClassifierKeys       FFU_GRP_KEYS[MBY_FFU_TCAM_ENTRIES_2-1];

    /* FFU Group Actions going to next group Per FFU Group data to be used by DV. */
//  mbyClassifierActions    FFU_GRP_ACTIONS[MBY_FFU_TCAM_ENTRIES_2-1];

    /* Fghash Actions going to next group Per Fghash data to be used DV. */
//  mbyFghashActions        FGHASH_ACTIONS[MBY_FFU_HASH_CFG_ENTRIES_1];

    /* The 6-bit FFU scenario to be used by next FFU group. Per FFU Group data to be used by DV. */
//  fm_byte                 FFU_GRP_SCENARIO[MBY_FFU_TCAM_ENTRIES_2-1];

    /* ECN/SWPRI/TTL01/DSCP and merged VPRI */
//  mbyClassifierMuxedAction FFU_MUXED_ACTION;

    // 0 for Shared Vlan Learning (SVL), 1 for Independent Vlan Learning (IVL)
    fm_bool                 LEARN_MODE;

    // Ingress VLAN counter
    fm_uint16               L2_IVLAN1_CNT_INDEX;

} mbyMapperToClassifier;


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

#endif // MBY_COMMON_H
