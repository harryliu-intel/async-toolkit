// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_MAPPER_H
#define MBY_MAPPER_H

#include "mby_common.h"
#include "mby_parser.h"
#include "mby_classifier.h"
#include "mby_mapper_defines.h"

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

// Function prototypes:

void Mapper
(
    fm_uint32                           regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyParserToMapper     * const in, 
          mbyMapperToClassifier * const out,
          mbyParserToModifier   * const parser_to_modifier
);

#endif // MBY_MAPPER_H
