// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_COMMON_H
#define MBY_COMMON_H

// Macros:

#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))

#define isBroadcastMacAddress(addr) ( (addr) == FM_LITERAL_U64(0xFFFFFFFFFFFF))
#define   isUnicastMacAddress(addr) (((addr) &  FM_LITERAL_U64(0x010000000000)) == 0)
#define isMulticastMacAddress(addr) (!isUnicastMacAddress(addr) && !isBroadcastMacAddress(addr))

#define FM_NOT_USED(a) (void) (a)

// Defines:

#define MBY_PORTS_COUNT          16 // <-- REVISIT!!!
#define MBY_FABRIC_LOG_PORTS     MBY_PORTS_COUNT

#define MBY_REGISTER_ARRAY_SIZE  0x1800000

#define MBY_MAX_PACKET_SIZE      32767

#define MBY_SEGMENT_LEN          256

#define MBY_N_PARSER_KEYS        80
#define MBY_N_PARSER_FLGS        48
#define MBY_N_PARSER_PTRS         8

// Changes to these constants must be reflected also in mbyLpmKeySels
#define MBY_CGRP_KEY8            64
#define MBY_CGRP_KEY16           32
#define MBY_CGRP_KEY32           16

#define MBY_CGRP_KEY16_BASE      0
#define MBY_CGRP_KEY8_BASE       ( MBY_CGRP_KEY16_BASE + MBY_CGRP_KEY16 )
#define MBY_CGRP_KEY32_BASE      ( MBY_CGRP_KEY8_BASE  + MBY_CGRP_KEY8 )

#define MBY_CGRP_KEYS            ( MBY_CGRP_KEY8 + MBY_CGRP_KEY16   + MBY_CGRP_KEY32   )
#define MBY_CGRP_HASH_KEYS       ( MBY_CGRP_KEY8 + MBY_CGRP_KEY16*2 + MBY_CGRP_KEY32*4 )
#define MBY_CGRP_ACT24           16
#define MBY_CGRP_ACT4            26
#define MBY_CGRP_ACT1            24
#define MBY_CGRP_REMAP_ACTIONS    8
#define MBY_CGRP_POL_ACTIONS      4  // MBY_CGRP_ACTION_POLICER[0..3]

#define MBY_PROT_TCP              6
#define MBY_PROT_UDP             17
#define MBY_PROT_ICMPv4           1
#define MBY_PROT_ICMPv6          58
#define MBY_PROT_IGMP             2

// Ethernet Frame types
#define MBY_ETYPE_IPv4           0x0800
#define MBY_ETYPE_IPv6           0x86DD
#define MBY_ETYPE_MAC_CONTROL    0x8808

#define MAC_ADDR_BYTES           6

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

#define FM_OK   0
#define FM_FAIL 1
#define TRUE    1
#define FALSE   0

// Enums:

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

typedef enum mbyClassifierGroupEnum
{
    MBY_CLA_GROUP_A = 0,
    MBY_CLA_GROUP_B = 1

} mbyClassifierGroup;

typedef enum mbyMirrorTypeEnum
{
    MBY_MIRTYPE_NORMAL = 0,
    MBY_MIRTYPE_MIR0,
    MBY_MIRTYPE_MIR1

} mbyMirrorType;

typedef enum mbyStpStateEnum
{
    MBY_STP_STATE_DISABLE = 0,
    MBY_STP_STATE_LISTENING,
    MBY_STP_STATE_LEARNING,
    MBY_STP_STATE_FORWARD

} mbyStpState;

typedef enum mbyMaLookupEntryTypeEnum
{
    MBY_MA_LOOKUP_ENTRY_TYPE_NOTUSED      = 0,
    MBY_MA_LOOKUP_ENTRY_TYPE_PROVISIONAL  = 1,
    MBY_MA_LOOKUP_ENTRY_TYPE_DYNAMIC      = 2,
    MBY_MA_LOOKUP_ENTRY_TYPE_SECURE       = 3,
    MBY_MA_LOOKUP_ENTRY_TYPE_STATIC       = 4,
    MBY_MA_LOOKUP_ENTRY_TYPE_SECURESTATIC = 5

} mbyMaLookupEntryType;

typedef enum mbyTriggerActionForwardingEnum
{
    MBY_TRIG_ACTION_FORWARDING_AS_IS = 0,
    MBY_TRIG_ACTION_FORWARDING_FORWARD,
    MBY_TRIG_ACTION_FORWARDING_REDIRECT,
    MBY_TRIG_ACTION_FORWARDING_DROP

} mbyTriggerActionForwarding;

typedef enum mbyTriggerActionTrapEnum
{
    MBY_TRIG_ACTION_TRAP_AS_IS = 0,
    MBY_TRIG_ACTION_TRAP_TRAP,
    MBY_TRIG_ACTION_TRAP_LOG,
    MBY_TRIG_ACTION_TRAP_REVERT

} mbyTriggerActionTrap;

typedef enum mbyTriggerActionMirroringEnum
{
    MBY_TRIG_ACTION_MIRRORING_AS_IS = 0,
    MBY_TRIG_ACTION_MIRRORING_MIRROR,
    MBY_TRIG_ACTION_MIRRORING_CANCEL

} mbyTriggerActionMirroring;

typedef enum mbyTriggerActionTCEnum
{
    MBY_TRIG_ACTION_TC_AS_IS = 0,
    MBY_TRIG_ACTION_TC_REASSIGN

} mbyTriggerActionTC;

typedef enum mbyTriggerActionVlanEnum
{
    MBY_TRIG_ACTION_VLAN_AS_IS = 0,
    MBY_TRIG_ACTION_VLAN_REASSIGN

} mbyTriggerActionVlan;

typedef enum mbyTriggerActionLearningEnum
{
    MBY_TRIG_ACTION_LEARNING_AS_IS = 0,
    MBY_TRIG_ACTION_LEARNING_DONT_LEARN,
    MBY_TRIG_ACTION_LEARNING_FORCE_LEARN

} mbyTriggerActionLearning;

// Structs:

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

typedef struct mbyParserHdrPtrsStruct
{
    fm_byte                 OFFSET      [MBY_N_PARSER_PTRS]; // offsets to data of interest within packet
    fm_bool                 OFFSET_VALID[MBY_N_PARSER_PTRS]; // parser offset valid flags
    fm_byte                 PROT_ID     [MBY_N_PARSER_PTRS]; // parser protocol IDs
} mbyParserHdrPtrs;

typedef struct mbyClassifierKeysStruct
{
    fm_uint32               key32[MBY_CGRP_KEY32];
    fm_uint16               key16[MBY_CGRP_KEY16];
    fm_byte                 key8 [MBY_CGRP_KEY8 ];

} mbyClassifierKeys;

typedef struct mbyActionPrecValStruct
{
    fm_byte                 prec; // 3b field
    fm_uint32               val;  // act24.val is 24b, act4.val is 4b, act1.val is 1b

} mbyActionPrecVal;

typedef struct mbyClassifierActionsStruct
{
    mbyActionPrecVal        act24[MBY_CGRP_ACT24];
    mbyActionPrecVal        act4 [MBY_CGRP_ACT4 ];
    mbyActionPrecVal        act1 [MBY_CGRP_ACT1 ];

} mbyClassifierActions;

typedef struct mbyMaTableStruct
{
    fm_byte                 _RSVD5_;
    fm_byte                 OLD_PORT;
    fm_byte                 NEW_PORT;
    mbyMaLookupEntryType    ENTRY_TYPE;
    fm_byte                 _RSVD3_;
    fm_byte                 TRIG_ID;
    fm_uint16               S_GLORT;
    fm_uint16               D_GLORT;
    fm_byte                 _RSVD2_;
    fm_bool                 _RSVD1_;
    fm_uint16               L2_DOMAIN;
    fm_uint16               VID;
    fm_uint64               MAC_ADDRESS;

} mbyMaTable;

typedef struct mbyTriggerResultsStruct
{
    fm_uint32                           action;
    mbyTriggerActionForwarding          forwardingAction;
    fm_uint16                           destGlort;
    fm_uint64                           destMask;
    fm_bool                             filterDestMask;
    mbyTriggerActionTrap                trapAction;
    fm_byte                             cpuCode;
    fm_byte                             trapCode;
    fm_bool                             logAction;
    mbyTriggerActionMirroring           mirroringAction0;
    mbyTriggerActionMirroring           mirroringAction1;
    fm_bool                             rxMirror;
    fm_byte                             mirrorProfileIndex0;
    fm_byte                             mirrorProfileIndex1;
    fm_bool                             mirror0ProfileV;
    fm_bool                             mirror1ProfileV;
    fm_byte                             mirror0ProfileIdx;
    fm_byte                             mirror1ProfileIdx;
    mbyTriggerActionTC                  TCAction;
    fm_byte                             TC;
    mbyTriggerActionVlan                vlanAction;
    fm_uint16                           vlan;
    mbyTriggerActionLearning            learningAction;
    fm_bool                             rateLimitAction;
    fm_byte                             rateLimitNum;
    fm_int                              metadataTrigNum[4];
    fm_byte                             metadataAction[4];
    fm_byte                             egressL2DomainAction;
    fm_byte                             egressL3DomainAction;
    fm_byte                             qcnValid0;
    fm_byte                             qcnValid1;
    fm_byte                             policerAction;
    fm_byte                             noModifyAction;

} mbyTriggerResults;

// External function prototypes:

fm_status mbyModelReadCSR
(
    fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint32 byte_addr,
    fm_uint32 * const value
);

fm_status mbyModelReadCSRMult
(
    fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint32 byte_addr,
    const fm_int len,
    fm_uint32 * const value
);

fm_status mbyModelReadCSR64
(
 	fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
	const fm_uint32 byte_addr,
	fm_uint64 * const value
);

fm_status mbyModelWriteCSR64
(
    fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint32 byte_addr,
    const fm_uint64 new_value
);

fm_status mbyModelWriteCSRMult
(
    fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint32 byte_addr,
    const fm_int len,
    const fm_uint32 * new_value
);

#endif // MBY_COMMON_H
