/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include <uint.h>
#include <string.h>
#include <assert.h>
#include <mby_exactmatch.h>
#include <mby_mapper.h>

#include <mby_top_map.h>
#include <tst_model_c_write.h> // write_field()
#include <mby_exactmatch.h>

#define COLOR_RED     "\x1b[31m"
#define COLOR_GREEN   "\x1b[32m"
#define COLOR_RESET   "\x1b[0m"

#define SIMPLE_EXACTMATCH_TEST(name, fails) {if (!run_on_simple_exactmatch(simple_exactmatch_ ## name ## _test_setup, \
                simple_exactmatch_ ## name ## _test_check)) pass(#name); else {++fails; fail(#name);} }

typedef void(*run_on_simple_exactmatch_setup_fn)
(
    mby_ppe_cgrp_a_map__addr * const cgrp_a_map_w,
    mby_ppe_cgrp_b_map__addr * const cgrp_b_map_w,
    mby_shm_map__addr        * const shm_map_w,
    mbyMapperToClassifier    * const map2cla
);

typedef int(*run_on_simple_exactmatch_check_fn)
(
    fm_uint32 actions[MBY_EM_MAX_ACTIONS_NUM]
);

static void pass(const char* name)
{
    printf(COLOR_GREEN "[pass]" COLOR_RESET " %s\n", name);
}

static void fail(const char* name)
{
    printf(COLOR_RED   "[FAIL]" COLOR_RESET " %s\n", name );
}

static void allocMem
(
    mby_ppe_cgrp_a_map       **cgrp_a_map,
    mby_ppe_cgrp_a_map__addr **cgrp_a_map_w,
    mby_ppe_cgrp_b_map       **cgrp_b_map,
    mby_ppe_cgrp_b_map__addr **cgrp_b_map_w,
    mby_shm_map              **shm_map,
    mby_shm_map__addr        **shm_map_w
)
{
    *cgrp_a_map = malloc(sizeof(mby_ppe_cgrp_a_map));
    *cgrp_a_map_w = malloc(sizeof(mby_ppe_cgrp_a_map__addr));
    *cgrp_b_map = malloc(sizeof(mby_ppe_cgrp_b_map));
    *cgrp_b_map_w = malloc(sizeof(mby_ppe_cgrp_b_map__addr));
    *shm_map    = malloc(sizeof(mby_shm_map));
    *shm_map_w    = malloc(sizeof(mby_shm_map__addr));
    if (*cgrp_a_map == NULL) {
        printf("Could not allocate heap memory for classifier A map -- exiting!\n");
        exit(-1);
    }
    if (*cgrp_b_map == NULL) {
        printf("Could not allocate heap memory for classifier B map -- exiting!\n");
        exit(-1);
    }
    if (*shm_map == NULL) {
        printf("Could not allocate heap memory for shared memory map -- exiting!\n");
        exit(-1);
    }
}

static void freeMem
(
    mby_ppe_cgrp_a_map       * const cgrp_a_map,
    mby_ppe_cgrp_a_map__addr * const cgrp_a_map_w,
    mby_ppe_cgrp_b_map       * const cgrp_b_map,
    mby_ppe_cgrp_b_map__addr * const cgrp_b_map_w,
    mby_shm_map              * const shm_map,
    mby_shm_map__addr        * const shm_map_w
)
{
    free(cgrp_a_map);
    free(cgrp_a_map_w);
    free(cgrp_b_map);
    free(cgrp_b_map_w);
    free(shm_map);
    free(shm_map_w);
}

// EM_X_HASH_LOOKUP

static void set_EM_HASH_LOOKUP
(
    em_hash_lookup_r__addr * const em_hash_lookup_reg_w,
    fm_uint                  const bucket,
    uint20                   const ptr,
    uint4                    const select_4,
    uint4                    const select_3,
    uint4                    const select_2,
    uint4                    const select_1,
    uint4                    const select_0,
    uint32                   const mask
)
{
    em_hash_lookup_r__addr * const em_hash_lookup_w = &(em_hash_lookup_reg_w[bucket]);

    write_field(em_hash_lookup_w->PTR,      ptr     );
    write_field(em_hash_lookup_w->SELECT_4, select_4);
    write_field(em_hash_lookup_w->SELECT_3, select_3);
    write_field(em_hash_lookup_w->SELECT_2, select_2);
    write_field(em_hash_lookup_w->SELECT_1, select_1);
    write_field(em_hash_lookup_w->SELECT_0, select_0);
    write_field(em_hash_lookup_w->MASK,     mask    );
}

// EM_HASH_CAM

static void set_EM_HASH_CAM
(
    mby_ppe_cgrp_em_map__addr * const cgrp_em_map_w,
    fm_byte                     const entry,
    fm_byte                     const word,
    fm_uint64                   const data
)
{
    em_hash_cam_r__addr * const em_hash_cam_w = &(cgrp_em_map_w->HASH_CAM[entry][word]);

    write_field(em_hash_cam_w->DATA, data);
}

// EM_HASH_CAM_EN

static void set_EM_HASH_CAM_EN
(
    mby_ppe_cgrp_em_map__addr * const cgrp_em_map_w,
    fm_byte                     const row,
    fm_byte                     const rule,
    fm_uint64                   const mask
)
{
    em_hash_cam_en_r__addr * const em_hash_cam_en = &(cgrp_em_map_w->HASH_CAM_EN[row][rule]);

    write_field(em_hash_cam_en->MASK, mask);
}

// EM_KEY_SEL0

static void set_EM_KEY_SEL0
(
    mby_ppe_cgrp_em_map__addr * const cgrp_em_map_w,
    fm_bool                     const hash,
    fm_byte                     const profile,
    fm_uint32                   const key8_mask
)
{
    em_key_sel0_r__addr * const em_key_sel0 = &(cgrp_em_map_w->KEY_SEL0[hash][profile]);

    write_field(em_key_sel0->KEY8_MASK, key8_mask);
}


// EM_KEY_SEL1

static void set_EM_KEY_SEL1
(
    mby_ppe_cgrp_em_map__addr * const cgrp_em_map_w,
    fm_bool                     const hash,
    fm_byte                     const profile,
    uint4                       const key_mask_sel,
    uint16                      const key32_mask,
    uint32                      const key16_mask
)
{
    em_key_sel1_r__addr * const em_key_sel1 = &(cgrp_em_map_w->KEY_SEL1[hash][profile]);

    write_field(em_key_sel1->KEY_MASK_SEL, key_mask_sel);
    write_field(em_key_sel1->KEY32_MASK,   key32_mask  );
    write_field(em_key_sel1->KEY16_MASK,   key16_mask  );
}

// EM_KEY_MASK

static void set_EM_KEY_MASK
(
    mby_ppe_cgrp_em_map__addr * const cgrp_em_map_w,
    fm_bool                     const hash,
    fm_byte                     const key_mask_sel,
    fm_byte                     const dw,
    uint64                      const mask
)
{
    fm_byte mask_id = (key_mask_sel * 2) + dw;

    em_key_mask_r__addr * const em_key_mask = &(cgrp_em_map_w->KEY_MASK[hash][mask_id]);

    write_field(em_key_mask->MASK, mask);
}

// EM_HASH_MISS

static void set_EM_HASH_MISS
(
    mby_ppe_cgrp_em_map__addr * const cgrp_em_map_w,
    fm_bool                     const hash,
    fm_byte                     const profile,
    fm_uint32                   const action1,
    fm_uint32                   const action0
)
{
    em_hash_miss_r__addr * const em_hash_miss = &(cgrp_em_map_w->HASH_MISS[hash][profile]);

    write_field(em_hash_miss->ACTION1, action1);
    write_field(em_hash_miss->ACTION0, action0);
}

// EM_HASH_CFG

static void set_EM_HASH_CFG
(
    mby_ppe_cgrp_em_map__addr * const cgrp_em_map_w,
    fm_byte                     const profile,
    fm_bool                     const mode,
    fm_uint16                   const base_ptr_0,
    fm_uint16                   const base_ptr_1,
    fm_byte                     const hash_size_0,
    fm_byte                     const hash_size_1,
    fm_byte                     const entry_size_0,
    fm_byte                     const entry_size_1
)
{
    em_hash_cfg_r__addr * const em_hash_cfg = &(cgrp_em_map_w->HASH_CFG[profile]);

    write_field(em_hash_cfg->MODE,         mode        );
    write_field(em_hash_cfg->BASE_PTR_0,   base_ptr_0  );
    write_field(em_hash_cfg->BASE_PTR_1,   base_ptr_1  );
    write_field(em_hash_cfg->HASH_SIZE_0,  hash_size_0 );
    write_field(em_hash_cfg->HASH_SIZE_1,  hash_size_1 );
    write_field(em_hash_cfg->ENTRY_SIZE_0, entry_size_0);
    write_field(em_hash_cfg->ENTRY_SIZE_1, entry_size_1);
}

// FWD_TABLE0

static void set_FWD_TABLE0
(
    mby_shm_map__addr * const shm_map_w,
    fm_uint             const i,
    fm_uint             const j,
    fm_uint64           const data
)
{
    fwd_table0_r__addr * const fwd_table0_w = &(shm_map_w->FWD_TABLE0[i][j]);

    write_field(fwd_table0_w->DATA, data);
}

// FWD_TABLE1

static void set_FWD_TABLE1
(
    mby_shm_map__addr * const shm_map_w,
    fm_uint             const i,
    fm_uint             const j,
    fm_uint64           const data
)
{
    fwd_table1_r__addr * const fwd_table1_w = &(shm_map_w->FWD_TABLE1[i][j]);

    write_field(fwd_table1_w->DATA, data);
}

static void cpy_actions
(
    mbyClassifierActions * const in,
    mbyClassifierActions * const out
)
{
    for (fm_uint i = 0; i < MBY_CGRP_ACT24; i++) {
        out->act24[i].prec = in->act24[i].prec;
        out->act24[i].val = in->act24[i].val;
    }

    for (fm_uint i = 0; i < MBY_CGRP_ACT4; i++) {
        out->act4[i].prec = in->act4[i].prec;
        out->act4[i].val = in->act4[i].val;
    }

    for (fm_uint i = 0; i < MBY_CGRP_ACT1; i++) {
        out->act1[i].prec = in->act1[i].prec;
        out->act1[i].val = in->act1[i].val;
    }
}

static void init_actions(mbyClassifierActions * const actions_in)
{
    for (fm_uint i = 0; i < MBY_CGRP_ACT24; i++) {
        actions_in->act24[i].prec = 1;
        actions_in->act24[i].val  = 0;
    }

    for (fm_uint i = 0; i < MBY_CGRP_ACT4; i++) {
        actions_in->act4[i].prec = 1;
        actions_in->act4[i].val = 0;
    }

    for (fm_uint i = 0; i < MBY_CGRP_ACT1; i++) {
        actions_in->act1[i].prec = 1;
        actions_in->act1[i].val = 0;
    }
}

static void init_keys(mbyClassifierKeys * const keys)
{
    for (fm_uint i = 0; i < MBY_CGRP_KEY32; i++)
        keys->key32[i] = 0;

    for (fm_uint i = 0; i < MBY_CGRP_KEY16; i++)
        keys->key16[i] = 0;

    for (fm_uint i = 0; i < MBY_CGRP_KEY8;  i++)
        keys->key8 [i] = 0;
}

static void initInputs
(
    mbyMapperToClassifier * const map2cla
)
{
    mbyClassifierActions * const actions_in     = &(map2cla->CLASSIFIER_ACTIONS);
    mbyClassifierKeys    * const keys           = &(map2cla->CLASSIFIER_KEYS);
    fm_byte              * const packet_profile = &(map2cla->PACKET_PROFILE);

    init_actions(actions_in);
    init_keys(keys);
    *packet_profile = 0;
}

static void setRegs_basic
(
    mby_ppe_cgrp_a_map__addr * const cgrp_a_map_w,
    mby_ppe_cgrp_b_map__addr * const cgrp_b_map_w,
    mby_shm_map__addr        * const shm_map_w
)
{
    mby_ppe_cgrp_a_nested_map__addr * cgrp_a_nested_map_w = &(cgrp_a_map_w->A);
    mby_ppe_cgrp_em_map__addr       * cgrp_em_map_w       = &(cgrp_a_map_w->EM);

    // EM_A_KEY_SEL0[hash][profile])
    set_EM_KEY_SEL0(cgrp_em_map_w,
        0,      // hash
        17,     // packet profile
        0x20    // key8_mask  (0000 0000 0000 0000 0000 0000 0010 0000 == KEY8[5] )
    );

    // EM_A_KEY_SEL1[hash][profile])
    set_EM_KEY_SEL1(cgrp_em_map_w,
        0,      // hash
        17,     // packet profile
        0x0,    // key_mask_sel <-- Might need to REVISIT!!!
        0x10,   // key32_mask (0000 0000 0001 0000                     == KEY32[4])
        0x0     // key16_mask (0000 0000 0000 0000 0000 0000 0000 0000 == NONE    )
    );

    // EM_A_HASH_CFG[profile]
    set_EM_HASH_CFG(cgrp_em_map_w,
        17,     // packet profile
        1,      // mode (64B == non-split mode)
        0x0,    // base_ptr_0
        0x0,    // base_ptr_1
        0x6,    // hash_size_0
        0x0,    // hash_size_1
        0x3,    // entry_size_0
        0       // entry_size_1
    );

    // EM_A_HASH_LOOKUP[bucket]
    set_EM_HASH_LOOKUP(cgrp_a_nested_map_w->EM_HASH_LOOKUP,
        0xc, // bucket
        0x0,    // ptr
        0x4,    // select_4
        0x3,    // select_3
        0x2,    // select_2
        0x1,    // select_1
        0x0,    // select_0
        0x20000 // mask (0000 0000 0000 0010 0000 0000 0000 0000 == 17)
    );

    // FWD_TABLE0[block][cell]
    set_FWD_TABLE0(shm_map_w,
        0,  // block
        0,  // cell
        0x12341234ULL << 32 // hash_entry_keys
    );

    // FWD_TABLE0[block][cell]
    set_FWD_TABLE0(shm_map_w,
        0,  // block
        1,  // cell
        0x54210000ULL << 32 // hash_actions[0]=0x54210000
    );
}

static void setInputs_basic
(
    mbyMapperToClassifier * const map2cla
)
{
    /*
    set based on the SQA test

    Frame Header Data:
    00  11  22  33  44  55  00  01  01  01  01  01  81  00  00  02
    08  00  45  00  00  14  00  00  00  00  ff  06  cc  45  b0  0a
    1b  2c  12  34  12  34
    */
    map2cla->CLASSIFIER_KEYS.key32[0]  = 0xB00A1B2C;
    map2cla->CLASSIFIER_KEYS.key32[1]  = 0x12341234;
    map2cla->CLASSIFIER_KEYS.key32[4]  = 0x12341234;
    map2cla->CLASSIFIER_KEYS.key16[6]  = 0x0011;
    map2cla->CLASSIFIER_KEYS.key16[7]  = 0x2233;
    map2cla->CLASSIFIER_KEYS.key16[8]  = 0x4455;
    map2cla->CLASSIFIER_KEYS.key16[9]  = 0x0001;
    map2cla->CLASSIFIER_KEYS.key16[10] = 0x0101;
    map2cla->CLASSIFIER_KEYS.key16[11] = 0x0101;
    map2cla->CLASSIFIER_KEYS.key16[12] = 0x0800;
    map2cla->CLASSIFIER_KEYS.key16[13] = 0x0001;
    map2cla->CLASSIFIER_KEYS.key16[14] = 0x0001;
    map2cla->CLASSIFIER_KEYS.key16[15] = 0x0001;
    map2cla->CLASSIFIER_KEYS.key16[16] = 0x0001;
    map2cla->CLASSIFIER_KEYS.key16[17] = 0x0203;
    map2cla->CLASSIFIER_KEYS.key16[19] = 0x0203;
    map2cla->CLASSIFIER_KEYS.key16[31] = 0x0002;
    map2cla->CLASSIFIER_KEYS.key8[3]   = 0x11;
    map2cla->CLASSIFIER_KEYS.key8[6]   = 0x40;
    map2cla->CLASSIFIER_KEYS.key8[20]  = 0xff;
    map2cla->CLASSIFIER_KEYS.key8[21]  = 0x06;
    map2cla->CLASSIFIER_KEYS.key8[23]  = 0x14;
    map2cla->CLASSIFIER_KEYS.key8[25]  = 0x40;
    map2cla->CLASSIFIER_KEYS.key8[28]  = 0x01;
    map2cla->CLASSIFIER_KEYS.key8[29]  = 0x02;
    map2cla->CLASSIFIER_KEYS.key8[43]  = 0x60;
    map2cla->CLASSIFIER_KEYS.key8[45]  = 0x02;
    map2cla->CLASSIFIER_KEYS.key8[55]  = 0xa1;
    map2cla->CLASSIFIER_KEYS.key8[56]  = 0x07;
    map2cla->CLASSIFIER_KEYS.key8[57]  = 0xf6;
    map2cla->CLASSIFIER_KEYS.key8[58]  = 0xe5;
    map2cla->CLASSIFIER_KEYS.key8[59]  = 0xd4;

    map2cla->CLASSIFIER_ACTIONS.act4[4].val  = 1;
    map2cla->CLASSIFIER_ACTIONS.act1[20].val = 1;
    map2cla->CLASSIFIER_ACTIONS.act1[22].val = 1;

    map2cla->PACKET_PROFILE = 0x11; // 17
}

static void simple_exactmatch_basic_test_setup
(
    mby_ppe_cgrp_a_map__addr * const cgrp_a_map_w,
    mby_ppe_cgrp_b_map__addr * const cgrp_b_map_w,
    mby_shm_map__addr        * const shm_map_w,
    mbyMapperToClassifier    * const map2cla
)
{
    setRegs_basic(cgrp_a_map_w, cgrp_b_map_w, shm_map_w);

    initInputs(map2cla);

    setInputs_basic(map2cla);
}

static int simple_exactmatch_basic_test_check
(
    fm_uint32 actions[MBY_EM_MAX_ACTIONS_NUM]
)
{

    // There should be only one action
    if (actions[0] != (fm_uint32) 0x54210000)
        return 1;

    // Verify other actions are empty
    for (fm_uint i = 1; i < MBY_EM_MAX_ACTIONS_NUM; i++) {
        if(actions[i] != 0x0)
            return 1;
    }

    return 0;
}

static int run_on_simple_exactmatch
(
    run_on_simple_exactmatch_setup_fn setup,
    run_on_simple_exactmatch_check_fn check
)
{
    mby_ppe_cgrp_a_map *cgrp_a_map = NULL;
    mby_ppe_cgrp_b_map *cgrp_b_map = NULL;
    mby_shm_map        *shm_map    = NULL;

    mby_ppe_cgrp_a_map__addr *cgrp_a_map_w = NULL;
    mby_ppe_cgrp_b_map__addr *cgrp_b_map_w = NULL;
    mby_shm_map__addr        *shm_map_w    = NULL;

    allocMem(&cgrp_a_map, &cgrp_a_map_w, &cgrp_b_map, &cgrp_b_map_w, &shm_map, &shm_map_w);

    mbyMapperToClassifier map2cla = { 0 };
    fm_uint32             actions[MBY_EM_MAX_ACTIONS_NUM] = { 0 };

    mbyMapperToClassifier const * const in          = &map2cla;
    mbyClassifierActions        * const actions_out = &(map2cla.CLASSIFIER_ACTIONS);

    mby_ppe_cgrp_a_map__init(cgrp_a_map, cgrp_a_map_w, mby_field_init_cb);
    // No need to initalialize maps that are not used
    // mby_ppe_cgrp_b_map__init(cgrp_b_map, cgrp_b_map_w, mby_field_init_cb);
    mby_shm_map__init(shm_map, shm_map_w, mby_field_init_cb);

    setup(cgrp_a_map_w, cgrp_b_map_w, shm_map_w, &map2cla);

    mbyMatchExact
    (
        cgrp_a_map->A.EM_HASH_LOOKUP,
        &(cgrp_a_map->EM),
        shm_map,
        &in->CLASSIFIER_KEYS,
        in->PACKET_PROFILE,
        MBY_CGRP_A,
        actions
    );

    int ret = check(actions);

    // Free up memory:
    freeMem(cgrp_a_map, cgrp_a_map_w, cgrp_b_map, cgrp_b_map_w, shm_map, shm_map_w);

    return ret;
}


int main(void)
{
    printf("--------------------------------------------------------------------------------\n");

    fm_uint tests = 0;
    fm_uint fails = 0;

    SIMPLE_EXACTMATCH_TEST(basic, fails); tests++;

    fm_uint passes = (tests > fails) ? tests - fails : 0;

    printf("--------------------------------------------------------------------------------\n");

    if (fails == 0)
        printf(COLOR_GREEN "[pass]");
    else
        printf(COLOR_RED   "[FAIL]");

    printf(" %2d/%2d - EXACTMATCH tests\n" COLOR_RESET, passes, tests);

    printf("--------------------------------------------------------------------------------\n");

    int rv = (fails == 0) ? 0 : -1;

    return rv;
}
