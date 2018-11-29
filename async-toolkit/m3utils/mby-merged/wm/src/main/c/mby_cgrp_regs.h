// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_CLASSIFIER_REGS_H
#define MBY_CLASSIFIER_REGS_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"

// Defines:

/* --------------------------------------------------------------------- */
/* The following are written manually based on the specs - do not delete */

/* FFU Action set entry encoding */
#define MBY_CGRP_ACTIONS_PER_ENTRY         2
#define MBY_CGRP_ACTION_PREC_WIDTH         3
#define MBY_CGRP_ACTION_l_PREC             29
#define MBY_CGRP_ACTION_h_PREC             31
#define MBY_CGRP_ACTION_l_ENTRYTYPE        24
#define MBY_CGRP_ACTION_h_ENTRYTYPE        28
#define MBY_CGRP_ACTION_l_SET1_24B_INDEX   24
#define MBY_CGRP_ACTION_h_SET1_24B_INDEX   27
#define MBY_CGRP_ACTION_l_SET1_24B_VALUE   0
#define MBY_CGRP_ACTION_h_SET1_24B_VALUE   23

#define MBY_CGRP_ACTION_l_SET3_4B_INDEXC   22
#define MBY_CGRP_ACTION_h_SET3_4B_INDEXC   26
#define MBY_CGRP_ACTION_l_SET3_4B_INDEXB   17
#define MBY_CGRP_ACTION_h_SET3_4B_INDEXB   21
#define MBY_CGRP_ACTION_l_SET3_4B_INDEXA   12
#define MBY_CGRP_ACTION_h_SET3_4B_INDEXA   16
#define MBY_CGRP_ACTION_l_SET3_4B_VALUEC   8
#define MBY_CGRP_ACTION_h_SET3_4B_VALUEC   11
#define MBY_CGRP_ACTION_l_SET3_4B_VALUEB   4
#define MBY_CGRP_ACTION_h_SET3_4B_VALUEB   7
#define MBY_CGRP_ACTION_l_SET3_4B_VALUEA   0
#define MBY_CGRP_ACTION_h_SET3_4B_VALUEA   3

#define MBY_CGRP_ACTION_l_SET3_1B_INDEXC   16
#define MBY_CGRP_ACTION_h_SET3_1B_INDEXC   21
#define MBY_CGRP_ACTION_b_SET3_1B_VC       22
#define MBY_CGRP_ACTION_b_SET3_1B_EC       23
#define MBY_CGRP_ACTION_l_SET3_1B_INDEXB   8
#define MBY_CGRP_ACTION_h_SET3_1B_INDEXB   13
#define MBY_CGRP_ACTION_b_SET3_1B_VB       14
#define MBY_CGRP_ACTION_b_SET3_1B_EB       15
#define MBY_CGRP_ACTION_l_SET3_1B_INDEXA   0
#define MBY_CGRP_ACTION_h_SET3_1B_INDEXA   5
#define MBY_CGRP_ACTION_b_SET3_1B_VA       6
#define MBY_CGRP_ACTION_b_SET3_1B_EA       7

#define MBY_CGRP_ACTION_l_SET8_1B_ENABLE   8
#define MBY_CGRP_ACTION_h_SET8_1B_ENABLE   15
#define MBY_CGRP_ACTION_l_SET8_1B_INDEX    16
#define MBY_CGRP_ACTION_h_SET8_1B_INDEX    19
#define MBY_CGRP_ACTION_l_SET8_1B_VALUE    0
#define MBY_CGRP_ACTION_h_SET8_1B_VALUE    7

#define MBY_CGRP_ACTION_l_SET4_4B_ENABLE   20
#define MBY_CGRP_ACTION_h_SET4_4B_ENABLE   23
#define MBY_CGRP_ACTION_l_SET4_4B_INDEX    16
#define MBY_CGRP_ACTION_h_SET4_4B_INDEX    19
#define MBY_CGRP_ACTION_l_SET4_4B_VALUE    0
#define MBY_CGRP_ACTION_h_SET4_4B_VALUE    15

/* FFU Remap Entry Encoding */
#define MBY_CGRP_REMAP_b_ENTRYTYPE         23
#define MBY_CGRP_REMAP_l_SET8_1B_INDEX     16
#define MBY_CGRP_REMAP_h_SET8_1B_INDEX     23
#define MBY_CGRP_REMAP_l_SET8_1B_VALUE     8
#define MBY_CGRP_REMAP_h_SET8_1B_VALUE     15
#define MBY_CGRP_REMAP_l_SET8_1B_MASK      0
#define MBY_CGRP_REMAP_h_SET8_1B_MASK      7

#define MBY_CGRP_REMAP_l_SET1_16B_INDEX     16
#define MBY_CGRP_REMAP_h_SET1_16B_INDEX     23
#define MBY_CGRP_REMAP_l_SET1_16B_VALUE     0
#define MBY_CGRP_REMAP_h_SET1_16B_VALUE     15

#define MBY_CGRP_HASH_ENTRY_MODE_32B   0
#define MBY_CGRP_HASH_ENTRY_MODE_64B   1

/* Bit number for fields of FFU_SLICE_SRAM.RouteData */
/* Bit numbers when RouteType==GLORT */
#define MBY_CGRP_ROUTE_l_DGLORT        0
#define MBY_CGRP_ROUTE_h_DGLORT        15
#define MBY_CGRP_ROUTE_b_FLOODSET      16

/* Bit numbers when RouteType==ARP */
#define MBY_CGRP_ROUTE_l_ARP_INDEX     0
#define MBY_CGRP_ROUTE_h_ARP_INDEX     15
#define MBY_CGRP_ROUTE_l_GROUP_SIZE    16
#define MBY_CGRP_ROUTE_h_GROUP_SIZE    19
#define MBY_CGRP_ROUTE_b_GROUP_TYPE    20
#define MBY_CGRP_ROUTE_b_ARP_ROUTE     21

/* Bit number for FFU Flags */
#define MBY_CGRP_FLAGS_b_DROP          0
#define MBY_CGRP_FLAGS_b_TRAP          1
#define MBY_CGRP_FLAGS_b_LOG           2
#define MBY_CGRP_FLAGS_b_NO_ROUTE      3
#define MBY_CGRP_FLAGS_b_RX_MIRROR     4

#define MBY_N_MA_HASH_KEYS            9

#define MBY_SV_MOVE_DROP_RESERVED     0
#define MBY_SV_MOVE_DROP_PORT         1
#define MBY_SV_MOVE_DROP_ADDR         2
#define MBY_SV_MOVE_DROP_STATIC       3

#define MBY_LOG_TYPE_TRIG_LOG_ACTION  (1 << 0)
#define MBY_LOG_TYPE_FFU              (1 << 1)
#define MBY_LOG_TYPE_RESERVED_MAC     (1 << 2)
#define MBY_LOG_TYPE_ARP_REDIRECT     (1 << 3)
#define MBY_LOG_TYPE_ICMP             (1 << 4)
#define MBY_LOG_TYPE_TTL_IP_MC        (1 << 5)
#define MBY_LOG_TYPE_IP_UCST_L2_MCST  (1 << 7) /* EAC TBR */

/* Trap codes which forms lower 8-bit of CPU-glort. */
/* See RRC Bug 22835.  Changed twice to match RTL change 271047. */
/* cpuCode changed to match RTL (src/rtl/hlp/common/hlp_pkg.vh) : */
#define MBY_CPU_CODE_FFU              0x0;
#define MBY_CPU_CODE_RSVD_MAC         0x1;
#define MBY_CPU_CODE_IGMP             0x2;
#define MBY_CPU_CODE_ICMP             0x3;
#define MBY_CPU_CODE_IP_OPTION        0x4;
#define MBY_CPU_CODE_CPU_ADDRESS      0x5;
#define MBY_CPU_CODE_MTU              0x6;
#define MBY_CPU_CODE_TTL              0x7;
#define MBY_CPU_CODE_MAX              0xF;
//efine MBY_CPU_CODE_FFU              0x80;
//efine MBY_CPU_CODE_RSVD_MAC         0x83;
//efine MBY_CPU_CODE_IGMP             0x86;
//efine MBY_CPU_CODE_ICMP             0x90;
//efine MBY_CPU_CODE_IP_OPTION        0x91;
//efine MBY_CPU_CODE_CPU_ADDRESS      0x92;
//efine MBY_CPU_CODE_MTU              0x94;
//efine MBY_CPU_CODE_TTL              0x96;
//efine MBY_CPU_CODE_MAX              0xFF;

#define MBY_IPV6_OPTION_HOP_BY_HOP     0
#define MBY_IPV6_OPTION_ROUTING       43
#define MBY_IPV6_OPTION_FRAG          44
#define MBY_IPV6_OPTION_DEST          60
#define MBY_IPV6_OPTION_AUTH          51

/* FFU mux selects */
#define MBY_CGRP_SELECT_MAP_DIP_MAP_SIP       0
#define MBY_CGRP_SELECT_MAP_DMAC_MAP_SMAC     1
#define MBY_CGRP_SELECT_MAP_PROT_MAP_LENGTH   2
#define MBY_CGRP_SELECT_MAP_SRC_MAP_TYPE      3
#define MBY_CGRP_SELECT_USER                  4
#define MBY_CGRP_SELECT_FTYPE_TC              5
#define MBY_CGRP_SELECT_IPMISC                6
#define MBY_CGRP_SELECT_TOS                   7
#define MBY_CGRP_SELECT_PROT                  8
#define MBY_CGRP_SELECT_TTL                   9
#define MBY_CGRP_SELECT_SRC_PORT             10
#define MBY_CGRP_SELECT_VPRI_VID_11_8        11
#define MBY_CGRP_SELECT_VID_7_0              12
#define MBY_CGRP_SELECT_RXTAG                13
#define MBY_CGRP_SELECT_L2_DMAC_15_0         14
#define MBY_CGRP_SELECT_L2_DMAC_31_16        15
#define MBY_CGRP_SELECT_L2_DMAC_47_32        16
#define MBY_CGRP_SELECT_L2_SMAC_15_0         17
#define MBY_CGRP_SELECT_L2_SMAC_31_16        18
#define MBY_CGRP_SELECT_L2_SMAC_47_32        19
#define MBY_CGRP_SELECT_DGLORT               20
#define MBY_CGRP_SELECT_SGLORT               21
#define MBY_CGRP_SELECT_VPRI_VID             22
#define MBY_CGRP_SELECT_VPRI2_VID2           23
#define MBY_CGRP_SELECT_L2_TYPE              24
#define MBY_CGRP_SELECT_L4_DST               25
#define MBY_CGRP_SELECT_L4_SRC               26
#define MBY_CGRP_SELECT_MAP_L4_DST           27
#define MBY_CGRP_SELECT_MAP_L4_SRC           28
#define MBY_CGRP_SELECT_L4A                  29
#define MBY_CGRP_SELECT_L4B                  30
#define MBY_CGRP_SELECT_L4C                  31
#define MBY_CGRP_SELECT_L4D                  32
#define MBY_CGRP_SELECT_MAP_VPRI1_VID1       33
#define MBY_CGRP_SELECT_L3_DIP_31_0          34
#define MBY_CGRP_SELECT_L3_DIP_63_32         35
#define MBY_CGRP_SELECT_L3_DIP_95_64         36
#define MBY_CGRP_SELECT_L3_DIP_127_96        37
#define MBY_CGRP_SELECT_L3_SIP_31_0          38
#define MBY_CGRP_SELECT_L3_SIP_63_32         39
#define MBY_CGRP_SELECT_L3_SIP_95_64         40
#define MBY_CGRP_SELECT_L3_SIP_127_96        41

#define MBY_WCM_ACTION_CFG_INDEX_WIDTH        5

// Enums:

typedef enum mbyClassifierActionEntryTypeEnum
{
    MBY_CGRP_ACTION_NOP = 0,
    MBY_CGRP_ACTION_SET4_4B,
    MBY_CGRP_ACTION_SET8_1B,
    MBY_CGRP_ACTION_SET3_1B,
    MBY_CGRP_ACTION_SET3_4B,
    MBY_CGRP_ACTION_SET1_24B

} mbyClassifierActionEntryType;

typedef enum mbyClassifierAct1Enum
{
    MBY_CGRP_ACTION_DROP                   =  0,
    MBY_CGRP_ACTION_TRAP                   =  1,
    MBY_CGRP_ACTION_LOG                    =  2,
    MBY_CGRP_ACTION_NO_ROUTE               =  3,
    MBY_CGRP_ACTION_RX_MIRROR              =  4,
    // MBY_CGRP_ACTION_RESERVED               =  5,
    MBY_CGRP_ACTION_TX_TAG0                =  6,
    MBY_CGRP_ACTION_TX_TAG1                =  7,
    MBY_CGRP_ACTION_TRIGGER0               =  8,
    MBY_CGRP_ACTION_TRIGGER1               =  9,
    MBY_CGRP_ACTION_TRIGGER2               = 10,
    MBY_CGRP_ACTION_TRIGGER3               = 11,
    MBY_CGRP_ACTION_TRIGGER4               = 12,
    MBY_CGRP_ACTION_TRIGGER5               = 13,
    MBY_CGRP_ACTION_TRIGGER6               = 14,
    MBY_CGRP_ACTION_TRIGGER7               = 15,
    MBY_CGRP_ACTION_PROFILE0               = 16,
    MBY_CGRP_ACTION_PROFILE1               = 17,
    MBY_CGRP_ACTION_PROFILE2               = 18,
    MBY_CGRP_ACTION_PROFILE3               = 19,
    MBY_CGRP_ACTION_PROFILE4               = 20,
    MBY_CGRP_ACTION_PROFILE5               = 21,
    MBY_CGRP_ACTION_LEARN_NOTIFY           = 22,
    MBY_CGRP_ACTION_COPY_OTR_VPRI          = 23  // Removed in MBY. Reserved for future use. REVISIT!!!

} mbyClassifierAct1;

typedef enum mbyClasifierAct4Enum
{
    MBY_CGRP_ACTION_DSCP_CTRL              =  0,
    MBY_CGRP_ACTION_TTL_CTRL               =  1,
    MBY_CGRP_ACTION_TC_CTRL                =  2,
    MBY_CGRP_ACTION_ECN_CTRL               =  3,
    MBY_CGRP_ACTION_VID_LOW                =  4,
    MBY_CGRP_ACTION_VID_MID                =  5,
    MBY_CGRP_ACTION_VID_HIGH               =  6,
    MBY_CGRP_ACTION_VPRI_LOW               =  7,
    MBY_CGRP_ACTION_DSCP_LOW               =  8,
    MBY_CGRP_ACTION_DSCP_HIGH              =  9,
    MBY_CGRP_ACTION_TC                     = 10,
    // alternate VPRI address when combining DSCP/TC_CTRL/VPRI actions
    MBY_CGRP_ACTION_VPRI_HIGH              = 11,
    MBY_CGRP_ACTION_HASH_PROFILE_ECMP_LOW  = 12,
    MBY_CGRP_ACTION_HASH_PROFILE_ECMP_HIGH = 13,
    MBY_CGRP_ACTION_HASH_PROFILE_MOD_LOW   = 14,
    MBY_CGRP_ACTION_HASH_PROFILE_MOD_HIGH  = 15,
    MBY_CGRP_ACTION_META0_LOW              = 16,
    MBY_CGRP_ACTION_META0_HIGH             = 17,
    MBY_CGRP_ACTION_META1_LOW              = 18,
    MBY_CGRP_ACTION_META1_HIGH             = 19,
    MBY_CGRP_ACTION_META2_LOW              = 20,
    MBY_CGRP_ACTION_META2_HIGH             = 21,
    MBY_CGRP_ACTION_META3_LOW              = 22,
    MBY_CGRP_ACTION_META3_HIGH             = 23,
    MBY_CGRP_ACTION_HASH_PROFILE_LAG_LOW   = 24,
    MBY_CGRP_ACTION_HASH_PROFILE_LAG_HIGH  = 25,
    MBY_CGRP_ACTION_MPLS_POP               = 26 // Removed in MBY <-- REVISIT!!!

} mbyClasifierAct4;

typedef enum mbyClassifierAct24Enum
{
    MBY_CGRP_ACTION_POLICER0               =  0,
    MBY_CGRP_ACTION_POLICER1               =  1,
    MBY_CGRP_ACTION_POLICER2               =  2,
    MBY_CGRP_ACTION_POLICER3               =  3,
    MBY_CGRP_ACTION_FWD                    =  4,
    MBY_CGRP_ACTION_MOD_PROFILE            =  5,
    MBY_CGRP_ACTION_REMAP0                 =  6,
    MBY_CGRP_ACTION_REMAP1                 =  7,
    MBY_CGRP_ACTION_REMAP2                 =  8,
    MBY_CGRP_ACTION_REMAP3                 =  9,
    MBY_CGRP_ACTION_REMAP4                 = 10,
    MBY_CGRP_ACTION_REMAP5                 = 11,
    MBY_CGRP_ACTION_REMAP6                 = 12,
    MBY_CGRP_ACTION_REMAP7                 = 13,
    MBY_CGRP_ACTION_USED                   = 14

} mbyClassifierAct24;

typedef enum mbyClassifierKey8Enum
{
    MBY_CGRP_KEY8_MPLS_LABEL5_0 = 0,
    MBY_CGRP_KEY8_MPLS_LABEL5_1 = 1,
    MBY_CGRP_KEY8_MPLS_LABEL5_2 = 2,
    MBY_CGRP_KEY8_MPLS_LABEL5_3 = 3,
    MBY_CGRP_KEY8_MPLS_LABEL6_0 = 4,
    MBY_CGRP_KEY8_MPLS_LABEL6_1 = 5,
    MBY_CGRP_KEY8_MPLS_LABEL6_2 = 6,
    MBY_CGRP_KEY8_MPLS_LABEL6_3 = 7,
    MBY_CGRP_KEY8_INNER_TTL     = 8,
    MBY_CGRP_KEY8_INNER_PROT    = 9,
    MBY_CGRP_KEY8_INNER_LEN     = 10,
    MBY_CGRP_KEY8_INNER_DS      = 12,
    MBY_CGRP_KEY8_OUTER_TTL     = 20,
    MBY_CGRP_KEY8_OUTER_PROT    = 21,
    MBY_CGRP_KEY8_OUTER_LEN     = 22,
    MBY_CGRP_KEY8_OUTER_DS      = 24

} mbyClassifierKey8;

typedef enum mbyClassifierKey16Enum
{
    MBY_CGRP_KEY16_OUTER_VLAN1     = 14,
    MBY_CGRP_KEY16_INNER_VLAN1     = 20,
    MBY_CGRP_KEY16_MPLS_LABEL1_0   = 24,
    MBY_CGRP_KEY16_MPLS_LABEL1_1   = 25,
    MBY_CGRP_KEY16_MPLS_LABEL2_0   = 26,
    MBY_CGRP_KEY16_MPLS_LABEL2_1   = 27,
    MBY_CGRP_KEY16_MPLS_LABEL3_0   = 28,
    MBY_CGRP_KEY16_MPLS_LABEL3_1   = 29,
    MBY_CGRP_KEY16_MPLS_LABEL4_0   = 30,
    MBY_CGRP_KEY16_MPLS_LABEL4_1   = 31

} mbyClassifierKey16;

typedef enum mbyClassifierKey32Enum
{
    MBY_CGRP_KEY32_OUTER_SIP_127_96    = 0,
    MBY_CGRP_KEY32_OUTER_SIP_95_64     = 1,
    MBY_CGRP_KEY32_OUTER_SIP_63_32     = 2,
    MBY_CGRP_KEY32_OUTER_SIP_31_0      = 3,
    MBY_CGRP_KEY32_OUTER_DIP_127_96    = 4,
    MBY_CGRP_KEY32_OUTER_DIP_95_64     = 5,
    MBY_CGRP_KEY32_OUTER_DIP_63_32     = 6,
    MBY_CGRP_KEY32_OUTER_DIP_31_0      = 7,
    MBY_CGRP_KEY32_INNER_SIP_127_96    = 8,
    MBY_CGRP_KEY32_INNER_SIP_95_64     = 9,
    MBY_CGRP_KEY32_INNER_SIP_63_32     = 10,
    MBY_CGRP_KEY32_INNER_SIP_31_0      = 11,
    MBY_CGRP_KEY32_INNER_DIP_127_96    = 12,
    MBY_CGRP_KEY32_INNER_DIP_95_64     = 13,
    MBY_CGRP_KEY32_INNER_DIP_63_32     = 14,
    MBY_CGRP_KEY32_INNER_DIP_31_0      = 15

} mbyClassifierKey32;

// Structs:

typedef struct mbyClassifierTcamCfgStruct
{
    fm_uint16 CHUNK_MASK;
    fm_bool   START_COMPARE;
    fm_bool   START_SET;
    fm_byte   SELECT_TOP;
    fm_byte   SELECT0;
    fm_byte   SELECT1;
    fm_byte   SELECT2;
    fm_byte   SELECT3;

} mbyClassifierTcamCfg;

typedef struct mbyClassifierTcamStruct
{
    fm_uint32 _RSVD1_;
    fm_byte   KEY_TOP_INVERT;
    fm_uint32 KEY_INVERT;
    fm_uint32 _RSVD0_;
    fm_byte   KEY_TOP;
    fm_uint32 KEY;

} mbyClassifierTcam;

// TODO is this ever used?
typedef struct mbyClassifierTcamEntryStruct
{
    fm_uint64 KEY;
    fm_uint64 KEY_INVERT;

} mbyClassifierTcamEntry;

typedef struct mbyClassifierHashLookupStruct
{
    fm_uint32 PTR;
    fm_uint16 RSVD1_;
    fm_byte   SELECT_4;
    fm_byte   SELECT_3;
    fm_byte   SELECT_2;
    fm_byte   SELECT_1;
    fm_byte   SELECT_0;
    fm_uint32 MASK;

} mbyClassifierHashLookup;

typedef struct mbyClassifierKeyMaskCfgStruct
{
    fm_uint32 KEY8_MASK;    // 32b field <-- Should it be 64b field? REVISIT!!!
    fm_uint32 KEY16_MASK;   // 32b
    fm_uint16 KEY32_MASK;   // 16b
    fm_byte   KEY_MASK_SEL; //  4b

} mbyClassifierKeyMaskCfg;

typedef struct mbyClassifierEntropyCfgStruct
{
    fm_uint32 KEY8_MASK;        // 32b field
    fm_uint32 KEY16_MASK;       // 32b
    fm_uint16 KEY32_MASK;       // 16b
    fm_byte   KEY_MASK_PROFILE; //  4b
    fm_byte   SYM_PROFILE;      //  2b
    fm_bool   SYMMETRIC;        //  1b

} mbyClassifierEntropyCfg;

typedef struct mbyClassifierHashCfgStruct
{
    fm_bool   mode;
    fm_uint16 base_ptr  [2]; // 2 x 13b field
    fm_byte   hash_size [2]; // 2 x  5b
    fm_byte   entry_size[2]; // 2 x  5b

} mbyClassifierHashCfg;

typedef struct mbyMplsMuxExpDsStruct
{
    fm_byte   DSCP;
    fm_byte   ECN;
    fm_byte   TC;

} mbyMplsMuxExpDs;

typedef struct mbyMplsMuxDscpTc
{
    fm_byte   TC;

} mbyMplsMuxDscpTc;

typedef struct mbyClassifierActionCfgStruct
{
    fm_bool   enable; // 1b field
    fm_byte   slice;  // 4b

} mbyClassifierActionCfg;

typedef struct mbyClassifierMuxedActionStruct
{
    fm_byte   ecn;
    fm_bool   aqm_mark_en;
    fm_byte   tc;
    fm_byte   ttl_ctrl;
    fm_byte   ttl01;
    fm_byte   dscp;
    fm_byte   vpri;
    fm_bool   route;

} mbyClassifierMuxedAction;

typedef struct mbyClassifierFlags
{
    fm_bool   drop;
    fm_bool   trap;
    fm_bool   log;
    fm_bool   no_route;
    fm_bool   rx_mirror;
    fm_byte   tx_tag;

} mbyClassifierFlags;

typedef struct mbyIppRxTagStruct
{
    fm_bool   custom;
    fm_bool   mpls;
    fm_bool   ipv6;
    fm_bool   ipv4;
    fm_bool   v2first;
    fm_bool   vlan2;
    fm_bool   vlan1;

} mbyRxTag;


// Functions:

mbyClassifierHashLookup mbyClsGetEmHashLookupEntry
(
    em_hash_lookup_r * const em_hash_lookup_reg,
    fm_uint16          const lookup_ptr
);

mbyClassifierKeyMaskCfg mbyClsGetEmKeyMaskCfg
(
    mby_ppe_cgrp_em_map * const cgrp_em_map,
    fm_byte               const hash_num,
    fm_byte               const scenario
);

mbyClassifierHashCfg mbyClsGetEmHashCfg
(
    mby_ppe_cgrp_em_map * const cgrp_em_map,
    fm_byte               const scenario
);


fm_uint64 mbyClsGetEmHashCamEntry
(
    mby_ppe_cgrp_em_map * const cgrp_em_map,
    fm_uint32             const entry,
    fm_uint32             const word
);

fm_uint64 mbyClsGetEmHashCamMask
(
    mby_ppe_cgrp_em_map * const cgrp_em_map,
    fm_uint32             const row,
    fm_uint32             const rule
);

fm_uint64 mbyClsGetEmAShmEntry // <-- REVISIT!!!
(
    mby_shm_map * const shm_map,
    fm_uint16     const block,
    fm_uint16     const cell
);

fm_uint64 mbyClsGetEmBShmEntry
(
    mby_shm_map * const shm_map,
    fm_uint16     const block,
    fm_uint16     const cell
);

void mbyClsGetEmHashMissActions
(
    mby_ppe_cgrp_em_map * const cgrp_em_map,
    mbyClassifierHashCfg  const hash_cfg,
    fm_uint32             const hash_num,
    fm_byte               const scenario,
    fm_uint32                 * hash_actions
);

mbyClassifierTcamCfg mbyClsGetWcmTcamCfg
(
    mby_ppe_cgrp_b_nested_map * const cgrp_b_map,
    fm_byte                     const slice,
    fm_byte                     const scenario
);

mbyClassifierTcamEntry mbyClsGetWcmTcamEntry
(
    mby_ppe_cgrp_b_nested_map * const cgrp_b_map,
    fm_byte                     const slice,
    fm_uint16                   const index
);

mbyClassifierActionCfg mbyClsGetWcmActionCfg
(
    mby_ppe_cgrp_b_nested_map * const cgrp_b_map,
    fm_byte                     const scenario,
    fm_byte                     const ram_num
);

fm_uint32 mbyClsGetWcmActionEntry
(
    mby_ppe_cgrp_b_nested_map * const cgrp_b_map,
    fm_byte                     const ram_num,
    fm_uint32                   const hit_index,
    fm_uint32                   const action
);

void mbyClsConvertKeysToBytes
(
    mbyClassifierKeys const keys,
    fm_byte                 bytes[MBY_CGRP_HASH_KEYS]
);

#endif /* MBY_CLASSIFIER_H */
