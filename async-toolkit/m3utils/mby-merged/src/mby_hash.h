// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_HASH_H
#define MBY_HASH_H

// Includes:

#include "mby_common.h"

// Defines:

#define MBY_ARP_VLAN_BASE                                       (0x3700000)
#define MBY_ARP_VLAN_SIZE                                       (0x0020000)

#define MBY_FWD_HASHING_CFG_WIDTH                               2
#define MBY_FWD_HASHING_CFG(word)                               (((word)*4) + (0x0051880) + (MBY_ARP_VLAN_BASE))

#define MBY_FWD_HASHING_CFG_b_USE_METADATA                      13
#define MBY_FWD_HASHING_CFG_b_ECMP_ROTATION                     12
#define MBY_FWD_HASHING_CFG_l_ROTATION_B                        10
#define MBY_FWD_HASHING_CFG_h_ROTATION_B                        11
#define MBY_FWD_HASHING_CFG_l_ROTATION_A                        8
#define MBY_FWD_HASHING_CFG_h_ROTATION_A                        9
#define MBY_FWD_HASHING_CFG_b_USE_VID                           7
#define MBY_FWD_HASHING_CFG_b_USE_VPRI                          6
#define MBY_FWD_HASHING_CFG_b_USE_TYPE                          5
#define MBY_FWD_HASHING_CFG_b_USE_SMAC                          4
#define MBY_FWD_HASHING_CFG_b_USE_DMAC                          3
#define MBY_FWD_HASHING_CFG_b_SYMMETRIC                         2
#define MBY_FWD_HASHING_CFG_b_USE_L34                           1
#define MBY_FWD_HASHING_CFG_b_USE_L2_IF_IP                      0

// Enums:

// Structs:

typedef struct mbyFwdHashingCfgStruct
{
    fm_bool                 USE_METADATA;
    fm_bool                 ECMP_ROTATION;
    fm_byte                 ROTATION_B;
    fm_byte                 ROTATION_A;
    fm_bool                 USE_VID;
    fm_bool                 USE_VPRI;
    fm_bool                 USE_TYPE;
    fm_bool                 USE_SMAC;
    fm_bool                 USE_DMAC;
    fm_bool                 SYMMETRIC;
    fm_bool                 USE_L34;
    fm_bool                 USE_L2_IF_IP;

} mbyFwdHashingCfg;

typedef struct mbyHashKeysStruct
{
    fm_uint64 crc34;
    fm_uint64 crc234;
    fm_byte   l234Key[17];
    fm_bool   zeroL2;
    fm_bool   zeroL34;
    fm_bool   useL34;
    fm_byte   rotA;
    fm_byte   rotB;
    fm_bool   arpEcmpCfg;

} mbyHashKeys;

typedef struct mbyHashToNextHopStruct
{
    mbyHashKeys             HASH_KEYS;
    fm_uint16               HASH_ROT_A_PTABLE_INDEX;
    fm_uint16               HASH_ROT_B_PTABLE_INDEX;
    fm_uint32               HASH_ROT_A;
    fm_uint32               HASH_ROT_B;
    fm_uint16               RAW_HASH;
    fm_byte                 ARP_HASH[16];

    // pass-thru from Classifier:
    mbyClassifierFlags      FFU_FLAGS;
    fm_uint32               FFU_ROUTE;
    fm_bool                 ENCAP;
    fm_bool                 DECAP;
    fm_macaddr              L2_SMAC;
    fm_macaddr              L2_DMAC;
    fm_macaddr              DMAC_FROM_IPV6;
    fm_uint16               L2_IDOMAIN;
    fm_byte                 L3_IDOMAIN;
    fm_uint16               L2_IVID1;
    fm_bool                 LEARN_MODE;
} mbyHashToNextHop;

#endif
