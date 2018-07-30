// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_CLASSIFIER_H
#define MBY_CLASSIFIER_H

// Includes:

#include "mby_common.h"

// Defines:

#define MBY_FFU_KEY16_BASE                0
#define MBY_FFU_KEY8_BASE                 ( MBY_FFU_KEY16_BASE + MBY_FFU_N_KEY16 )
#define MBY_FFU_KEY32_BASE                ( MBY_FFU_KEY8_BASE + MBY_FFU_N_KEY8 )

#define MBY_FFU_MAX_HASH_ENTRY_SIZE       64
#define MBY_FFU_MAX_HASH_ACTIONS          4
#define MBY_FFU_HASH_CAM_ETY_7_BITS_31_0  0
#define MBY_FFU_HASH_CAM_ETY_7_BITS_63_32 1
#define MBY_FFU_HASH_CAM_ETY_6_BITS_31_0  2
#define MBY_FFU_HASH_CAM_ETY_6_BITS_63_32 3

/* FFU Action entry encoding */
#define MBY_FFU_ACTIONS_PER_ENTRY         2
#define MBY_FFU_ACTION_PREC_WIDTH         3
#define MBY_FFU_ACTION_l_PREC             29
#define MBY_FFU_ACTION_h_PREC             31
#define MBY_FFU_ACTION_l_ENTRYTYPE        24
#define MBY_FFU_ACTION_h_ENTRYTYPE        28
#define MBY_FFU_ACTION_l_SET1_24B_INDEX   24
#define MBY_FFU_ACTION_h_SET1_24B_INDEX   27
#define MBY_FFU_ACTION_l_SET1_24B_VALUE   0
#define MBY_FFU_ACTION_h_SET1_24B_VALUE   23

#define MBY_FFU_ACTION_l_SET3_4B_INDEXC   22
#define MBY_FFU_ACTION_h_SET3_4B_INDEXC   26
#define MBY_FFU_ACTION_l_SET3_4B_INDEXB   17
#define MBY_FFU_ACTION_h_SET3_4B_INDEXB   21
#define MBY_FFU_ACTION_l_SET3_4B_INDEXA   12
#define MBY_FFU_ACTION_h_SET3_4B_INDEXA   16
#define MBY_FFU_ACTION_l_SET3_4B_VALUEC   8
#define MBY_FFU_ACTION_h_SET3_4B_VALUEC   11
#define MBY_FFU_ACTION_l_SET3_4B_VALUEB   4
#define MBY_FFU_ACTION_h_SET3_4B_VALUEB   7
#define MBY_FFU_ACTION_l_SET3_4B_VALUEA   0
#define MBY_FFU_ACTION_h_SET3_4B_VALUEA   3

#define MBY_FFU_ACTION_l_SET3_1B_INDEXC   16
#define MBY_FFU_ACTION_h_SET3_1B_INDEXC   21
#define MBY_FFU_ACTION_b_SET3_1B_VC       22
#define MBY_FFU_ACTION_b_SET3_1B_EC       23
#define MBY_FFU_ACTION_l_SET3_1B_INDEXB   8
#define MBY_FFU_ACTION_h_SET3_1B_INDEXB   13
#define MBY_FFU_ACTION_b_SET3_1B_VB       14
#define MBY_FFU_ACTION_b_SET3_1B_EB       15
#define MBY_FFU_ACTION_l_SET3_1B_INDEXA   0
#define MBY_FFU_ACTION_h_SET3_1B_INDEXA   5
#define MBY_FFU_ACTION_b_SET3_1B_VA       6
#define MBY_FFU_ACTION_b_SET3_1B_EA       7

#define MBY_FFU_ACTION_l_SET8_1B_ENABLE   8 
#define MBY_FFU_ACTION_h_SET8_1B_ENABLE   15
#define MBY_FFU_ACTION_l_SET8_1B_INDEX    16
#define MBY_FFU_ACTION_h_SET8_1B_INDEX    19
#define MBY_FFU_ACTION_l_SET8_1B_VALUE    0
#define MBY_FFU_ACTION_h_SET8_1B_VALUE    7 

#define MBY_FFU_ACTION_l_SET4_4B_ENABLE   20 
#define MBY_FFU_ACTION_h_SET4_4B_ENABLE   23
#define MBY_FFU_ACTION_l_SET4_4B_INDEX    16
#define MBY_FFU_ACTION_h_SET4_4B_INDEX    19
#define MBY_FFU_ACTION_l_SET4_4B_VALUE    0
#define MBY_FFU_ACTION_h_SET4_4B_VALUE    15

/* FFU Remap Entry Encoding */
#define MBY_FFU_REMAP_b_ENTRYTYPE         23
#define MBY_FFU_REMAP_l_SET8_1B_INDEX     16
#define MBY_FFU_REMAP_h_SET8_1B_INDEX     23
#define MBY_FFU_REMAP_l_SET8_1B_VALUE     8 
#define MBY_FFU_REMAP_h_SET8_1B_VALUE     15
#define MBY_FFU_REMAP_l_SET8_1B_MASK      0 
#define MBY_FFU_REMAP_h_SET8_1B_MASK      7

#define MBY_FFU_REMAP_l_SET1_16B_INDEX     16
#define MBY_FFU_REMAP_h_SET1_16B_INDEX     23
#define MBY_FFU_REMAP_l_SET1_16B_VALUE     0 
#define MBY_FFU_REMAP_h_SET1_16B_VALUE     15

#define MBY_FFU_HASH_ENTRY_MODE_32B   0
#define MBY_FFU_HASH_ENTRY_MODE_64B   1

/* Bit number for fields of FFU_SLICE_SRAM.RouteData */
/* Bit numbers when RouteType==GLORT */
#define MBY_FFU_ROUTE_l_DGLORT        0
#define MBY_FFU_ROUTE_h_DGLORT        15
#define MBY_FFU_ROUTE_b_FLOODSET      16

/* Bit numbers when RouteType==ARP */
#define MBY_FFU_ROUTE_l_ARP_INDEX     0
#define MBY_FFU_ROUTE_h_ARP_INDEX     15
#define MBY_FFU_ROUTE_l_GROUP_SIZE    16
#define MBY_FFU_ROUTE_h_GROUP_SIZE    19
#define MBY_FFU_ROUTE_b_GROUP_TYPE    20
#define MBY_FFU_ROUTE_b_ARP_ROUTE     21

/* Bit number for FFU Flags */
#define MBY_FFU_FLAGS_b_DROP          0
#define MBY_FFU_FLAGS_b_TRAP          1
#define MBY_FFU_FLAGS_b_LOG           2
#define MBY_FFU_FLAGS_b_NO_ROUTE      3
#define MBY_FFU_FLAGS_b_RX_MIRROR     4
#define MBY_FFU_FLAGS_b_CAPTURE_TIME  5

#define MBY_N_MA_HASH_KEYS            9

#define MBY_MA_TABLE_CAM_BANK         5
#define MBY_MA_TABLE_CAM_ENTRIES      1024

#define MBY_MA_ENTRY_TYPE_NOT_USED      0
#define MBY_MA_ENTRY_TYPE_PROVISIONAL   1
#define MBY_MA_ENTRY_TYPE_DYNAMIC       2
#define MBY_MA_ENTRY_TYPE_SECURE        3
#define MBY_MA_ENTRY_TYPE_STATIC        4
#define MBY_MA_ENTRY_TYPE_SECURE_STATIC 5

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

/* Frame types */
#define MBY_ETYPE_IPv4                 0x0800
#define MBY_ETYPE_IPv6                 0x86DD
#define MBY_ETYPE_MAC_CONTROL          0x8808

#define MBY_IPV6_OPTION_HOP_BY_HOP     0
#define MBY_IPV6_OPTION_ROUTING       43
#define MBY_IPV6_OPTION_FRAG          44
#define MBY_IPV6_OPTION_DEST          60
#define MBY_IPV6_OPTION_AUTH          51

/* FFU mux selects */
#define MBY_FFU_SELECT_MAP_DIP_MAP_SIP       0
#define MBY_FFU_SELECT_MAP_DMAC_MAP_SMAC     1
#define MBY_FFU_SELECT_MAP_PROT_MAP_LENGTH   2
#define MBY_FFU_SELECT_MAP_SRC_MAP_TYPE      3
#define MBY_FFU_SELECT_USER                  4
#define MBY_FFU_SELECT_FTYPE_SWPRI           5
#define MBY_FFU_SELECT_IPMISC                6
#define MBY_FFU_SELECT_TOS                   7
#define MBY_FFU_SELECT_PROT                  8
#define MBY_FFU_SELECT_TTL                   9
#define MBY_FFU_SELECT_SRC_PORT             10
#define MBY_FFU_SELECT_VPRI_VID_11_8        11
#define MBY_FFU_SELECT_VID_7_0              12
#define MBY_FFU_SELECT_RXTAG                13
#define MBY_FFU_SELECT_L2_DMAC_15_0         14
#define MBY_FFU_SELECT_L2_DMAC_31_16        15
#define MBY_FFU_SELECT_L2_DMAC_47_32        16
#define MBY_FFU_SELECT_L2_SMAC_15_0         17
#define MBY_FFU_SELECT_L2_SMAC_31_16        18
#define MBY_FFU_SELECT_L2_SMAC_47_32        19
#define MBY_FFU_SELECT_DGLORT               20
#define MBY_FFU_SELECT_SGLORT               21
#define MBY_FFU_SELECT_VPRI_VID             22
#define MBY_FFU_SELECT_VPRI2_VID2           23
#define MBY_FFU_SELECT_L2_TYPE              24
#define MBY_FFU_SELECT_L4_DST               25
#define MBY_FFU_SELECT_L4_SRC               26
#define MBY_FFU_SELECT_MAP_L4_DST           27
#define MBY_FFU_SELECT_MAP_L4_SRC           28
#define MBY_FFU_SELECT_L4A                  29
#define MBY_FFU_SELECT_L4B                  30
#define MBY_FFU_SELECT_L4C                  31
#define MBY_FFU_SELECT_L4D                  32
#define MBY_FFU_SELECT_MAP_VPRI1_VID1       33
#define MBY_FFU_SELECT_L3_DIP_31_0          34
#define MBY_FFU_SELECT_L3_DIP_63_32         35
#define MBY_FFU_SELECT_L3_DIP_95_64         36
#define MBY_FFU_SELECT_L3_DIP_127_96        37
#define MBY_FFU_SELECT_L3_SIP_31_0          38
#define MBY_FFU_SELECT_L3_SIP_63_32         39
#define MBY_FFU_SELECT_L3_SIP_95_64         40
#define MBY_FFU_SELECT_L3_SIP_127_96        41

// Enums:

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

typedef struct mbyClassifierKeysStruct
{
    fm_uint32               key32[MBY_FFU_N_KEY32];
    fm_uint16               key16[MBY_FFU_N_KEY16];
    fm_byte                 key8 [MBY_FFU_N_KEY8 ];

} mbyClassifierKeys;

typedef struct mbyPrecValStruct
{
    fm_byte                 prec; // 3b field
    fm_uint32               val;  // act24.val is 24b, act4.val is 4b, act1.val is 1b

} mbyPrecVal;

typedef struct mbyClassifierActionsStruct
{
    mbyPrecVal              act24[MBY_FFU_N_ACT24];
    mbyPrecVal              act4 [MBY_FFU_N_ACT4 ];
    mbyPrecVal              act1 [MBY_FFU_N_ACT1 ];

} mbyClassifierActions;

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

    // 0 for Shared Vlan Learning (SVL), 1 for Independent Vlan Learning (IVL)
    fm_bool                 LEARN_MODE;

    // Ingress VLAN counter
    fm_uint16               L2_IVLAN1_CNT_INDEX;

} mbyMapperToClassifier;

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

typedef struct mbyClassifierToHashStruct
{
    // The 6-bit FFU scenario.
    fm_byte                 FFU_SCENARIO;

    // FFU Group Keys feeding to next group. Per FFU Group data to be used by DV.
    mbyClassifierKeys       FFU_GRP_KEYS[1];     // note: [1] for now

    // FFU Group Actions going to next group Per FFU Group data to be used by DV.
    mbyClassifierActions    FFU_GRP_ACTIONS[1];  // note: [1] for now

    // The 6-bit FFU scenario to be used by next FFU group. Per FFU Group data to be used by DV.
    fm_byte                 FFU_GRP_SCENARIO[1]; // note: [1] for now

    /* Fghash Actions going to next group Per Fghash data to be used DV. */
//  mbyFghashActions        FGHASH_ACTIONS[MBY_FFU_HASH_CFG_ENTRIES_1];

    /* ECN/SWPRI/TTL01/DSCP and merged VPRI */
//  mbyClassifierMuxedAction FFU_MUXED_ACTION;

} mbyClassifierToHash;
    
#endif // MBY_CLASSIFIER_H
