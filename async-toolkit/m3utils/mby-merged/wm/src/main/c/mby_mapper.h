// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_MAPPER_H
#define MBY_MAPPER_H

// Includes:

#include "mby_common.h"
#include "mby_parser.h"

// Defines:

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

#define MBY_MAP_OUTER_DIP_VALID           0x1
#define MBY_MAP_OUTER_SIP_VALID           0x2
#define MBY_MAP_INNER_DIP_VALID           0x4
#define MBY_MAP_INNER_SIP_VALID           0x8

#define MBY_MAP_IP_PROF_OUT_SIP_BIT       0
#define MBY_MAP_IP_PROF_OUT_DIP_BIT       2
#define MBY_MAP_IP_PROF_INN_SIP_BIT       4
#define MBY_MAP_IP_PROF_INN_DIP_BIT       6
#define MBY_MAP_IP_PROF_BITS_PER_IP       2

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
#define SOURCE_PACKET_PROFILE_L     32
#define SOURCE_PACKET_PROFILE_H     33
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

// Enums:

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

// Structs:

typedef struct mbyMapPortCfgStruct
{
    fm_uint16 DEFAULT_SGLORT;
    fm_bool   DEFAULT_SGLORT_EN;
    fm_byte   PORT_PROFILE;

} mbyMapPortCfg;

typedef struct mbyMapProtStruct
{
    fm_byte   MAP_PROT; // 3b field
    fm_byte   PROT;     // 8b

} mbyMapProt;

typedef struct mbyMapPortDefaultsStruct
{
    fm_uint16 VALUE;
    fm_byte   TARGET;

} mbyMapPortDefaults;


typedef struct mbyMapDomainProfileStruct
{
    fm_byte PRIORITY_PROFILE;  //  5b field

} mbyMapDomainProfile;

typedef struct mbyMapDomainAction0Struct
{
    fm_byte L2_DOMAIN;         //  8b field
    fm_byte L3_DOMAIN;         //  6b
    fm_byte NAD;               //  4b
    fm_bool UPDATE_DOMAINS;    //  1b
    fm_bool LEARN_EN;          //  1b
    fm_bool LEARN_MODE;        //  1b
    fm_byte PRIORITY_PROFILE;  //  5b
    fm_byte PRI_SOURCE;        //  8b
    fm_bool FORCE_DEFAULT_PRI; //  1b
    fm_byte DEFAULT_PRI;       //  3b

} mbyMapDomainAction0;

typedef struct mbyMapDomainAction1Struct
{
    fm_byte   DOMAIN_PROFILE;  //  8b
    fm_uint16 L2_POLICER;      // 12b
    fm_uint16 L3_POLICER;      // 12b
    fm_uint16 VLAN_COUNTER;    // 12b

} mbyMapDomainAction1;

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

typedef struct mbyMapIpCfgStruct
{
    fm_byte   MATCH_LENGTH;
    fm_byte   VALID;
    fm_byte   MAP_IP;
    fm_byte   IP_PROFILE;
    fm_bool   IS_IPV6;

} mbyMapIpCfg;

typedef struct mbyMapIpLoStruct
{
    fm_uint64 IP_LO;

} mbyMapIpLo;

typedef struct mbyMapIpHiStruct
{
    fm_uint64 IP_HI;

} mbyMapIpHi;

typedef struct mbyMapProfKey0Struct
{
    fm_bool   PTRS_ERR;
    fm_byte   EX;
    fm_byte   CSUM;
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

typedef struct mbyMapMacStruct
{
    fm_bool    MAC_ROUTABLE;   //  1b field
    fm_byte    MAP_MAC;        //  8b
    fm_byte    VALID;          //  4b
    fm_byte    IGNORE_LENGTH;  //  6b
    fm_macaddr MAC;            // 48b

} mbyMapMac;

typedef struct mbyMapRewriteStruct
{
    fm_byte   SRC_ID;

} mbyMapRewrite;

typedef struct mbyMapperToClassifierStruct
{
    mbyClassifierActions    CLASSIFIER_ACTIONS;///< classifier actions
    mbyClassifierKeys       CLASSIFIER_KEYS;   ///< classifier TCAM lookup keys
    fm_byte                 PACKET_PROFILE;    ///< 6-bit packet profile ID
    fm_bool                 IP_OPTION[2];      ///< trap_ip_iptions count
    fm_uint16               L2_IDOMAIN;        ///< L2 ingress domain
    fm_uint16               L2_IVLAN1_CNT;     ///< ingress VLAN counter
    fm_byte                 L3_IDOMAIN;        ///< L3 ingress domain
    fm_bool                 LEARN_MODE;        ///< learning mode: 0=SVL, 1=IVL
    fm_bool                 NO_PRI_ENC;        ///< mapper priority encoding
    fm_bool                 NAD;               ///< NAD (former operator ID)
    fm_bool                 OTR_MPLS_V;        ///< parser outer MPLS packet valid
    fm_bool                 PARSER_ERROR;      ///< header parse error
    mbyParserInfo           PARSER_INFO;       ///< parser info structure
    fm_byte                 PRIORITY_PROFILE;  ///< 5-bit classifier priority profile
    fm_uint32               RX_PORT;           ///< ingress port
    fm_byte                 TRAFFIC_CLASS;     ///< 3-bit traffic class
    // pass-thru:
    fm_bool                 PARITY_ERROR;      ///< parity error detected flag
    fm_bool                 PA_DROP;           ///< checksum validation error, drop pkt in tail
    mbyParserHdrPtrs        PA_HDR_PTRS;       ///< parser header pointers
    fm_bool                 PA_L3LEN_ERR;      ///< l3 length error
  fm_uint32 RX_LENGTH;

} mbyMapperToClassifier;

void Mapper
(
    mby_ppe_mapper_map    const * const mapper_map,
    mbyParserToMapper     const * const in,
    mbyMapperToClassifier       * const out
);

#endif // MBY_MAPPER_H
