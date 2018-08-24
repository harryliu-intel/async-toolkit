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

#define MBY_PORTS_COUNT          24 // <-- REVISIT!!!
#define MBY_FABRIC_LOG_PORTS     MBY_PORTS_COUNT

#define MBY_REGISTER_ARRAY_SIZE  0x1800000

#define MBY_N_PARSER_KEYS        80
#define MBY_N_PARSER_FLAGS       48
#define MBY_N_PARSER_PTRS         8

#define MBY_FFU_N_KEY8           64
#define MBY_FFU_N_KEY16          32
#define MBY_FFU_N_KEY32          16
#define MBY_FFU_N_KEYS           ( MBY_FFU_N_KEY8 + MBY_FFU_N_KEY16   + MBY_FFU_N_KEY32   )
#define MBY_FFU_N_HASH_KEYS      ( MBY_FFU_N_KEY8 + MBY_FFU_N_KEY16*2 + MBY_FFU_N_KEY32*4 )
#define MBY_FFU_N_ACT24          16
#define MBY_FFU_N_ACT4           23
#define MBY_FFU_N_ACT1           24
#define MBY_FFU_N_REMAP_ACTIONS   8

#define MBY_PROT_TCP              6
#define MBY_PROT_UDP             17
#define MBY_PROT_ICMPv4           1
#define MBY_PROT_ICMPv6          58
#define MBY_PROT_IGMP             2

// Ethernet Frame types
#define MBY_ETYPE_IPv4           0x0800
#define MBY_ETYPE_IPv6           0x86DD
#define MBY_ETYPE_MAC_CONTROL    0x8808

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

typedef enum mbyMirrorTypeEnum
{
    MBY_MIRTYPE_NORMAL = 0,
    MBY_MIRTYPE_MIR0,
    MBY_MIRTYPE_MIR1

} mbyMirrorType;

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

// External function prototypes:

fm_bool   fmModelIsMulticastMacAddress(fm_macaddr keyMac);
fm_bool   fmModelIsBroadcastMacAddress(fm_macaddr keyMac);

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
