#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <mby_top_map.h>

#include <mby_common.h>
#include <mby_pipeline.h>

#define COLOR_RED     "\x1b[31m"
#define COLOR_GREEN   "\x1b[32m"
#define COLOR_RESET   "\x1b[0m"

// --------------------------------------------------------------------------------
// EM_KEY_SEL{0,1} : define the input key
// --------------------------------------------------------------------------------

void setEmKeySel0
(
    mby_ppe_cgrp_em_map * const cgrp_em_map,
    fm_uint32             const lookup,  // a.k.a. hash number
    fm_byte               const profile, // a.k.a. packet profile ID
    fm_uint32             const key8_mask
)
{
    assert (lookup  < mby_ppe_cgrp_em_map_KEY_SEL0__n); // [0 ..  1]
    assert (profile <   em_key_sel0_rf_EM_KEY_SEL0__n); // [0 .. 63]

    em_key_sel0_r * const em_key_sel0 = &(cgrp_em_map->KEY_SEL0[lookup][profile]);

    em_key_sel0->KEY8_MASK = key8_mask; // 32 bits
}

void setEmKeySel1
(
    mby_ppe_cgrp_em_map * const cgrp_em_map,
    fm_uint32             const lookup,  // a.k.a. hash number
    fm_byte               const profile, // a.k.a. packet profile ID
    fm_uint32             const key16_mask,
    fm_uint16             const key32_mask,
    fm_byte               const key_mask_sel
)
{
    assert (lookup       <        mby_ppe_cgrp_em_map_KEY_SEL1__n); // [0 ..  1]
    assert (profile      <          em_key_sel1_rf_EM_KEY_SEL1__n); // [0 .. 63]
    assert (key32_mask   <  (1uL << em_key_sel1_r_KEY32_MASK__n));
    assert (key_mask_sel <  (1uL << em_key_sel1_r_KEY_MASK_SEL__n));

    em_key_sel1_r * const em_key_sel1 = &(cgrp_em_map->KEY_SEL1[lookup][profile]);

    em_key_sel1->KEY16_MASK   = key16_mask;   // 32 bits
    em_key_sel1->KEY32_MASK   = key32_mask;   // 16 bits
    em_key_sel1->KEY_MASK_SEL = key_mask_sel; //  4 bits
}

// --------------------------------------------------------------------------------
// EM_HASH_CFG : configure lookup mode, base pointers, and entry sizes
// --------------------------------------------------------------------------------

void setEmHashCfg
(
    mby_ppe_cgrp_em_map * const cgrp_em_map,
    fm_byte               const profile, // a.k.a. packet profile ID
    fm_byte               const mode,
    fm_uint16             const base_ptrs  [2],
    fm_byte               const hash_sizes [2],
    fm_byte               const entry_sizes[2]
)
{
    assert (profile < em_key_sel0_rf_EM_KEY_SEL0__n);
    assert (mode    < (1uL << em_hash_cfg_r_MODE__n));
    for (fm_uint i = 0; i < 2; i++) {
        assert (base_ptrs  [i] < (1uL << em_hash_cfg_r_BASE_PTR_0__n));
        assert (hash_sizes [i] < (1uL << em_hash_cfg_r_HASH_SIZE_0__n));
        assert (entry_sizes[i] < (1uL << em_hash_cfg_r_ENTRY_SIZE_0__n));
    }

    em_hash_cfg_r * const em_hash_cfg = &(cgrp_em_map->HASH_CFG[profile]);

    em_hash_cfg->MODE         = mode;
    em_hash_cfg->BASE_PTR_0   = base_ptrs  [0];
    em_hash_cfg->BASE_PTR_1   = base_ptrs  [1];
    em_hash_cfg->HASH_SIZE_0  = hash_sizes [0];
    em_hash_cfg->HASH_SIZE_1  = hash_sizes [1];
    em_hash_cfg->ENTRY_SIZE_0 = entry_sizes[0];
    em_hash_cfg->ENTRY_SIZE_1 = entry_sizes[1];
}

// --------------------------------------------------------------------------------
// EM_HASH_CAM_EN : enables hash CAM profiles (a.k.a. scenarios)
// --------------------------------------------------------------------------------
void setEmHashCamEn
(
    mby_ppe_cgrp_em_map * const cgrp_em_map,
    fm_byte               const profile,  // a.k.a. packet profile ID
    fm_uint32             const row,
    fm_uint32             const rule
)
{
    assert (row     <  mby_ppe_cgrp_em_map_HASH_CAM_EN__n); // [0 ..  1]
    assert (rule    < em_hash_cam_en_rf_EM_HASH_CAM_EN__n); // [0 .. 31]
    assert (profile < 64);                                  // [0 .. 63]

    em_hash_cam_en_r * const em_hash_cam_en = &(cgrp_em_map->HASH_CAM_EN[row][rule]);

    fm_uint64 mask = (1uLL << profile);

//  em_hash_cam_en->MASK  = mask; // one-hot
    em_hash_cam_en->MASK |= mask; // read-modify-write
}

// --------------------------------------------------------------------------------
// EM_HASH_CAM : stores hash CAM entries for hash overflow.
// --------------------------------------------------------------------------------
void setEmHashCam
(
    mby_ppe_cgrp_em_map * const cgrp_em_map,
    fm_byte               const entry_num,
    fm_byte               const word_num,
    fm_uint64             const data
)
{
    // EM_B_HASH_CAM[0..31,0..7]
    assert (entry_num < mby_ppe_cgrp_em_map_HASH_CAM__n); // [0 .. 31]
    assert (word_num  <   em_hash_cam_rf_EM_HASH_CAM__n); // [0 ..  7]

    em_hash_cam_r * const em_hash_cam = &(cgrp_em_map->HASH_CAM[entry_num][word_num]);

    em_hash_cam->DATA = data;
}

// --------------------------------------------------------------------------------
// EM_HASH_MISS : define up to 4 actions to occur if there is a miss,
//                i.e. no matching entry in neither the hash lookup table
//                nor the overflow CAM.
// --------------------------------------------------------------------------------
void setEmHashMiss
(
    mby_ppe_cgrp_em_map * const cgrp_em_map,
    fm_uint32             const row,        // row = [0 .. 1]
    fm_byte               const profile,    // a.k.a. packet profile ID
    fm_uint32             const actions[2]  // 2 actions per row
)
{
    assert (row     < mby_ppe_cgrp_em_map_HASH_MISS__n); // [0 ..  1]
    assert (profile <  em_hash_miss_rf_EM_HASH_MISS__n); // [0 .. 63]

    em_hash_miss_r * const em_hash_miss = &(cgrp_em_map->HASH_MISS[row][profile]);

    em_hash_miss->ACTION0 = actions[0]; // 32 bits
    em_hash_miss->ACTION1 = actions[1]; // 32 bits
}

// --------------------------------------------------------------------------------
// EM_A_HASH_LOOKUP : stores exact match (a.k.a. hash lookup) bucket state
// --------------------------------------------------------------------------------
void setEmAHashLookup
(
    mby_ppe_cgrp_a_nested_map * const cgrp_a_ns_map,
    fm_uint32                   const bucket,     // hash bucket
    fm_uint32                   const pointer,    // 20 bit pointer
    fm_byte                     const selects[5], //  4 bit selects
    fm_uint32                   const mask        // 32 bit mask
)
{
    assert (bucket < mby_ppe_cgrp_a_nested_map_EM_HASH_LOOKUP__n); // [0 .. 8191]
    assert ((pointer & 0xfff00000uL) == 0uL); // 20 bits
    for (fm_uint i = 0; i < 5; i++)
        assert ((selects[i] & 0xf0) == 0); // 4 bits

    em_hash_lookup_r * const em_hash_lookup = &(cgrp_a_ns_map->EM_HASH_LOOKUP[bucket]);

    em_hash_lookup->PTR      = pointer;
    em_hash_lookup->SELECT_4 = selects[4];
    em_hash_lookup->SELECT_3 = selects[3];
    em_hash_lookup->SELECT_2 = selects[2];
    em_hash_lookup->SELECT_1 = selects[1];
    em_hash_lookup->SELECT_0 = selects[0];
    em_hash_lookup->MASK     = mask;
}

// --------------------------------------------------------------------------------
// EM_B_HASH_LOOKUP : stores exact match (a.k.a. hash lookup) bucket state
// --------------------------------------------------------------------------------
void setEmBHashLookup
(
    mby_ppe_cgrp_b_nested_map * const cgrp_b_ns_map,
    fm_uint32                   const bucket,     // hash bucket
    fm_uint32                   const pointer,    // 20 bit pointer
    fm_byte                     const selects[5], //  4 bit selects
    fm_uint32                   const mask        // 32 bit mask
)
{
    assert (bucket < mby_ppe_cgrp_b_nested_map_EM_HASH_LOOKUP__n); // [0 .. 8191]
    assert ((pointer & 0xfff00000uL) == 0uL); // 20 bits
    for (fm_uint i = 0; i < 5; i++)
        assert ((selects[i] & 0xf0) == 0); // 4 bits

    em_hash_lookup_r * const em_hash_lookup = &(cgrp_b_ns_map->EM_HASH_LOOKUP[bucket]);

    em_hash_lookup->PTR      = pointer;
    em_hash_lookup->SELECT_4 = selects[4];
    em_hash_lookup->SELECT_3 = selects[3];
    em_hash_lookup->SELECT_2 = selects[2];
    em_hash_lookup->SELECT_1 = selects[1];
    em_hash_lookup->SELECT_0 = selects[0];
    em_hash_lookup->MASK     = mask;
}
// --------------------------------------------------------------------------------
// FWD_TABLE1 : define up to 4 actions to occur if there is a miss,
//              i.e. no matching entry in neither the hash lookup table
// --------------------------------------------------------------------------------
void setSharedTableMem
(
    mby_shm_map * const shm_map,
    fm_uint32     const lookup, // a.k.a. hash number, a.k.a. entry number
    fm_uint32     const rx_port,
    fm_uint32     const data
)
{
    // FWD_TABLE1[0..511][0..255]
    //      lookup_/  port_/

    fwd_table1_r * const fwd_table1 = &(shm_map->FWD_TABLE1[lookup][rx_port]);

    fwd_table1->DATA = data; // 64 bits
}

// --------------------------------------------------------------------------------

void initRegs
(
    mby_ppe_cgrp_a_map  * const cgrp_a_map,
    mby_ppe_cgrp_b_map  * const cgrp_b_map,
    mby_ppe_entropy_map * const entropy_map
)
{
    // <initialize registers here>
}

void initDefaultInputs
(
    mbyMapperToClassifier * const map2cla
)
{
    mbyClassifierActions * const actions_in  = &(map2cla->FFU_ACTIONS);
    mbyClassifierKeys    * const keys        = &(map2cla->FFU_KEYS);
    fm_byte              * const scenario_in = &(map2cla->FFU_PROFILE);
    fm_bool              * const ip_option   =   map2cla->IP_OPTION;
    mbyParserInfo        * const parser_info = &(map2cla->PARSER_INFO);
    fm_byte              * const pri_profile = &(map2cla->PRIORITY_PROFILE);

    // mbyClassifierActions:
    for (fm_uint i = 0; i < MBY_CGRP_ACT24; i++) {
        actions_in->act24[i].prec = 0;
        actions_in->act24[i].val  = 0;
    }
    for (fm_uint i = 0; i < MBY_CGRP_ACT4; i++) {
        actions_in->act4[i].prec  = 0;
        actions_in->act4[i].val   = 0;
    }
    for (fm_uint i = 0; i < MBY_CGRP_ACT1; i++) {
        actions_in->act1[i].prec  = 0;
        actions_in->act1[i].val   = 0;
    }

    // mbyClassifierKeys:
    for (fm_uint i = 0; i < MBY_CGRP_KEY32; i++)
        keys->key32[i] = 0;
    for (fm_uint i = 0; i < MBY_CGRP_KEY16; i++)
        keys->key16[i] = 0;
    for (fm_uint i = 0; i < MBY_CGRP_KEY8;  i++)
        keys->key8 [i] = 0;

    // fm_byte:
    *scenario_in = 0;

    // fm_bool:
    *ip_option = FALSE;

    // mbyParserInfo:
    parser_info->otr_l2_len     = 0;
    parser_info->otr_l2_vlan1   = 0;
    parser_info->otr_l2_vlan2   = 0;
    parser_info->otr_l2_v2first = 0;
    parser_info->otr_mpls_len   = 0;
    parser_info->otr_l3_len     = 0;
    parser_info->otr_l3_v6      = 0;
    parser_info->otr_l4_udp     = 0;
    parser_info->otr_l4_tcp     = 0;
    parser_info->otr_tun_len    = 0;
    parser_info->inr_l2_len     = 0;
    parser_info->inr_l2_vlan1   = 0;
    parser_info->inr_l2_vlan2   = 0;
    parser_info->inr_l2_v2first = 0;
    parser_info->inr_mpls_len   = 0;
    parser_info->inr_l3_len     = 0;
    parser_info->inr_l3_v6      = 0;
    parser_info->inr_l4_udp     = 0;
    parser_info->inr_l4_tcp     = 0;

    // fm_byte
    *pri_profile                = 0;
}

void initInputs
(
    fm_uint32               const test_num,
    mbyMapperToClassifier * const map2cla
)
{
    mbyClassifierActions * const actions_in  = &(map2cla->FFU_ACTIONS);
    mbyClassifierKeys    * const keys        = &(map2cla->FFU_KEYS);
    fm_byte              * const scenario_in = &(map2cla->FFU_PROFILE);
    fm_bool              * const ip_option   =   map2cla->IP_OPTION;
    mbyParserInfo        * const parser_info = &(map2cla->PARSER_INFO);
    fm_byte              * const pri_profile = &(map2cla->PRIORITY_PROFILE);

    switch (test_num)
    {
    case  0:
        initDefaultInputs(map2cla);
        break;
    case  1:
        initDefaultInputs(map2cla);
        break;
    default:
        printf("Unsupported scenario -- exiting!\n");
        exit(-1);
    }
}

fm_status checkOutputs
(
    fm_uint32             const test_num,
    mbyClassifierToHash * const cla2hsh
)
{
    fm_status test_status = FM_OK;

    switch (test_num)
    {
    default:
    case  0: test_status = FM_FAIL; break;
    case  1: test_status = FM_OK;   break;
    }

    return test_status;
}

void allocMem
(
    mby_ppe_cgrp_a_map  **cgrp_a_map,
    mby_ppe_cgrp_b_map  **cgrp_b_map,
    mby_ppe_entropy_map **entropy_map,
    mby_shm_map         **shm_map
)
{
    *cgrp_a_map = malloc(sizeof(mby_ppe_cgrp_a_map));
    if (*cgrp_a_map == NULL) {
        printf("Could not allocate heap memory for classifier A map -- exiting!\n");
        exit(-1);
    }

    *cgrp_b_map = malloc(sizeof(mby_ppe_cgrp_b_map));
    if (*cgrp_b_map == NULL) {
        printf("Could not allocate heap memory for classifier B map -- exiting!\n");
        exit(-1);
    }

    *entropy_map = malloc(sizeof(mby_ppe_entropy_map));
    if (*entropy_map == NULL) {
        printf("Could not allocate heap memory for entropy map -- exiting!\n");
        exit(-1);
    }

    *shm_map = malloc(sizeof(mby_shm_map));
    if (*shm_map == NULL) {
        printf("Could not allocate heap memory for shared memory map -- exiting!\n");
        exit(-1);
    }
}

void freeMem
(
    mby_ppe_cgrp_a_map  * const cgrp_a_map,
    mby_ppe_cgrp_b_map  * const cgrp_b_map,
    mby_ppe_entropy_map * const entropy_map,
    mby_shm_map         * const shm_map
)
{
    free( cgrp_a_map);
    free( cgrp_b_map);
    free(entropy_map);
    free(    shm_map);
}


void updateTestStats
(
    char      * const test_name,
    fm_status         test_status,
    fm_uint32 * const num_tests,
    fm_uint32 * const num_passed
)
{
    if (test_status == FM_OK)
        printf(COLOR_GREEN "[pass]" COLOR_RESET);
    else
        printf(COLOR_RED   "[FAIL]" COLOR_RESET);

    fm_uint32 test_num = *num_tests;

    printf(" Test number %3d:  %s\n", test_num, test_name);

    (*num_tests)++;

    if (test_status == FM_OK)
        (*num_passed)++;
}

fm_bool reportTestStats
(
    fm_uint32 const num_tests,
    fm_uint32 const num_passed
)
{
    printf("--------------------------------------------------------------------------------\n");

    fm_bool tests_passed = (num_passed == num_tests);

    if (tests_passed)
        printf(COLOR_GREEN "[pass]");
    else
        printf(COLOR_RED   "[FAIL]");

    printf("  %3d/%3d - Classifier tests\n" COLOR_RESET, num_passed, num_tests);

    printf("--------------------------------------------------------------------------------\n");

    return tests_passed;
}

fm_status testWildCardMatch
(
    mby_ppe_cgrp_a_map    * const cgrp_a_map,
    mby_ppe_cgrp_b_map    * const cgrp_b_map,
    mby_ppe_entropy_map   * const entropy_map,
    mby_shm_map           * const shm_map,
    mbyMapperToClassifier * const in,
    mbyClassifierToHash   * const out
)
{
    // Initialize inputs:

    initDefaultInputs(in);

    mbyClassifierActions * const actions_in  = &(in->FFU_ACTIONS);
    mbyClassifierKeys    * const keys        = &(in->FFU_KEYS);
    fm_byte              * const scenario_in = &(in->FFU_PROFILE);
    fm_bool              * const ip_option   =   in->IP_OPTION;
    mbyParserInfo        * const parser_info = &(in->PARSER_INFO);
    fm_byte              * const pri_profile = &(in->PRIORITY_PROFILE);

    // Initialize registers:

    // <...>

    Classifier
    (
        cgrp_a_map,
        cgrp_b_map,
        entropy_map,
        shm_map,
        in,
        out
    );

    // Check outputs:

    fm_status test_status = FM_FAIL; // check for real <-- FIXME!!!

    return test_status;
}

fm_status testExactMatch
(
    mby_ppe_cgrp_a_map    * const cgrp_a_map,
    mby_ppe_cgrp_b_map    * const cgrp_b_map,
    mby_ppe_entropy_map   * const entropy_map,
    mby_shm_map           * const shm_map,
    mbyMapperToClassifier * const in,
    mbyClassifierToHash   * const out
)
{
    // L2 Lookup Test:
    //
    // The PPE provides the functionality to support basic MAC address management
    // by employing the Exact Match unit of CGRP_B, operating in split mode.
    //
    // A MAC address table is implemented by programming entries into the exact
    // match table in CGRP_B. Split mode is used so that a lookup may be performed
    // on the packet's SMAC and DMAC simultaneously.
    //
    // The DMAC lookup provides the destination port (DGLORT) out of which
    // the packet should be sent.
    //
    // The SMAC lookup is used to determine if we need to learn the destination
    // port for the sending host, so that when it becomes the destination in
    // a subsequent response packet, we will have a DMAC entry ready.

    // --------------------------------------------------------------------------------
    // Initialize inputs:
    // --------------------------------------------------------------------------------

    initDefaultInputs(in);

    mbyClassifierActions * const actions_in  = &(in->FFU_ACTIONS);
    mbyClassifierKeys    * const keys        = &(in->FFU_KEYS);
    fm_byte              * const scenario_in = &(in->FFU_PROFILE);
    fm_bool              * const ip_option   =   in->IP_OPTION;
    mbyParserInfo        * const parser_info = &(in->PARSER_INFO);
    fm_byte              * const pri_profile = &(in->PRIORITY_PROFILE);

    // --------------------------------------------------------------------------------
    // Initialize registers:
    // --------------------------------------------------------------------------------

    // Init key selects:
    fm_uint32 lookup       = 0; // a.k.a. hash number
    fm_uint32 profile      = 0; // a.k.a. packet profile ID
    fm_uint32 key8_mask    = 0;
    fm_uint32 key16_mask   = 0;
    fm_uint16 key32_mask   = 0;
    fm_byte   key_mask_sel = 0;

    // Pointer to EM B register map:
    mby_ppe_cgrp_em_map       * const cgrp_b_em_map = &(cgrp_b_map->EM); // common to A & B
    mby_ppe_cgrp_b_nested_map * const cgrp_b_ns_map = &(cgrp_b_map->B);  // specific to B

    setEmKeySel0 (cgrp_b_em_map, lookup, profile, key8_mask);
    setEmKeySel1 (cgrp_b_em_map, lookup, profile, key16_mask, key32_mask, key_mask_sel);

    // Init hash config:
    fm_byte   mode            =   0;   // lookup mode = split mode, width = 2 x 32 bytes
    fm_uint16 base_ptrs   [2] = { 0 }; // start of hash tables (2 lookups per packet)
    fm_byte   hash_sizes  [2] = { 0 };
    fm_byte   entry_sizes [2] = { 0 };

    setEmHashCfg (cgrp_b_em_map, profile, mode, base_ptrs, hash_sizes, entry_sizes);

    // Init hash CAM enable:
    fm_uint32 row  = 0;
    fm_uint32 rule = 0;

    setEmHashCamEn (cgrp_b_em_map, profile, row, rule);

    // Init hash CAM:
    fm_byte   entry_num = 0;
    fm_byte   word_num  = 0;
    fm_uint64 data      = 0;

    setEmHashCam (cgrp_b_em_map, entry_num, word_num, data);

    // Init hash miss:
    fm_uint32 actions[2] = { 0 };

    setEmHashMiss (cgrp_b_em_map, lookup, profile, actions);

    // Init hash lookup (i.e. bucket state):
    fm_uint32 bucket     =   0;   // hash bucket
    fm_uint32 pointer    =   0;   // 20 bit pointer
    fm_byte   selects[5] = { 0 }; //  4 bit selects
    fm_uint32 mask       =   0;   // 32 bit mask

    setEmBHashLookup (cgrp_b_ns_map, bucket, pointer, selects, mask);

    // Init shared table memory (shm):
    // - pointer to forwarding table for EM_B:
    fm_uint32 rx_port  = 0;
    fm_uint64 shm_data = 0;

    setSharedTableMem (shm_map, lookup, rx_port, shm_data);

    // --------------------------------------------------------------------------------
    // Call DUT:
    // --------------------------------------------------------------------------------

    Classifier
    (
        cgrp_a_map,
        cgrp_b_map,
        entropy_map,
        shm_map,
        in,
        out
    );

    // --------------------------------------------------------------------------------
    // Check outputs:
    // --------------------------------------------------------------------------------

    fm_status test_status = FM_FAIL; // check for real <-- FIXME!!!

    return test_status;
}

int main (void)
{
    mby_ppe_cgrp_a_map   *cgrp_a_map = NULL;
    mby_ppe_cgrp_b_map   *cgrp_b_map = NULL;
    mby_ppe_entropy_map *entropy_map = NULL;
    mby_shm_map             *shm_map = NULL;

    allocMem(&cgrp_a_map, &cgrp_b_map, &entropy_map, &shm_map);

    mbyMapperToClassifier map2cla = { 0 };
    mbyClassifierToHash   cla2hsh = { 0 };

    fm_uint32 num_tests   = 0;
    fm_uint32 num_passed  = 0;
    fm_status test_status = FM_OK;

    printf("--------------------------------------------------------------------------------\n");

    // --------------------------------------------------------------------------------
    // Wild Card Match (WCM) Test
    // --------------------------------------------------------------------------------
    test_status = testWildCardMatch(cgrp_a_map, cgrp_b_map, entropy_map, shm_map, &map2cla, &cla2hsh);
    updateTestStats("Wild Card Match (WCM)", test_status, &num_tests, &num_passed);

    // --------------------------------------------------------------------------------
    // Exact Match (EM) Test
    // --------------------------------------------------------------------------------
    test_status = testExactMatch(cgrp_a_map, cgrp_b_map, entropy_map, shm_map, &map2cla, &cla2hsh);
    updateTestStats("Exact Match (EM)", test_status, &num_tests, &num_passed);

    // --------------------------------------------------------------------------------
    // Longest Prefix Match (LPM) Test
    // --------------------------------------------------------------------------------
    test_status = testWildCardMatch(cgrp_a_map, cgrp_b_map, entropy_map, shm_map, &map2cla, &cla2hsh);
    updateTestStats("Longest Prefix Match (LPM)", test_status, &num_tests, &num_passed);

    // --------------------------------------------------------------------------------

    fm_bool tests_passed = reportTestStats(num_tests, num_passed);

    // Free up memory:
    freeMem(cgrp_a_map, cgrp_b_map, entropy_map, shm_map);

    int rv = (tests_passed) ? 0 : -1;

    return rv;
}
