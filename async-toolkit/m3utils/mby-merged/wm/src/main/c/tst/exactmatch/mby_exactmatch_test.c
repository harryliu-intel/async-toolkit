#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include <uint.h>
#include <string.h>
#include <assert.h>
#include <mby_exactmatch.h>
#include <mby_mapper.h>

#include <mby_top_map.h>

#include <mby_common.h>
#include <mby_pipeline.h>

#define COLOR_RED     "\x1b[31m"
#define COLOR_GREEN   "\x1b[32m"
#define COLOR_RESET   "\x1b[0m"


#define SIMPLE_EXACTMATCH_TEST(name, fails) {if (!run_on_simple_exactmatch(simple_exactmatch_ ## name ## _test_setup, \
                simple_exactmatch_ ## name ## _test_check)) pass(#name); else {++fails; fail(#name);} }

typedef void(*run_on_simple_exactmatch_setup_fn)
(
    mby_ppe_cgrp_a_map    * const cgrp_a_map,
    mby_ppe_cgrp_b_map    * const cgrp_b_map,
    mby_shm_map           * const shm_map,
    mbyMapperToClassifier * const map2cla
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
    mby_ppe_cgrp_a_map  **cgrp_a_map,
    mby_ppe_cgrp_b_map  **cgrp_b_map,
    mby_shm_map         **shm_map
)
{
    *cgrp_a_map = malloc(sizeof(mby_ppe_cgrp_a_map));
    *cgrp_b_map = malloc(sizeof(mby_ppe_cgrp_b_map));
    *shm_map    = malloc(sizeof(mby_shm_map));
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
    mby_ppe_cgrp_a_map * const cgrp_a_map,
    mby_ppe_cgrp_b_map * const cgrp_b_map,
    mby_shm_map        * const shm_map
)
{
    free(cgrp_a_map);
    free(cgrp_b_map);
    free(shm_map);
}

// EM_A_HASH_LOOKUP

static void set_EM_A_HASH_LOOKUP
(
    mby_ppe_cgrp_a_map * const cgrp_a_map,
    fm_uint bucket,
    uint20 ptr,
    uint4 select_4,
    uint4 select_3,
    uint4 select_2,
    uint4 select_1,
    uint4 select_0,
    uint32 mask
)
{
    em_a_hash_lookup_r * const em_a_hash_lookup = &(cgrp_a_map->EM_A_HASH_LOOKUP[bucket]);
    em_a_hash_lookup->PTR       = ptr;
    em_a_hash_lookup->SELECT_4  = select_4;
    em_a_hash_lookup->SELECT_3  = select_3;
    em_a_hash_lookup->SELECT_2  = select_2;
    em_a_hash_lookup->SELECT_1  = select_1;
    em_a_hash_lookup->SELECT_0  = select_0;
    em_a_hash_lookup->MASK      = mask;
}

static void init_EM_A_HASH_LOOKUP_REG
(
    mby_ppe_cgrp_a_map  * const cgrp_a_map
)
{
    for (fm_uint bucket = 0 ; bucket < MBY_EM_A_HASH_LOOKUP_ENTRIES ; bucket++)
    {
        set_EM_A_HASH_LOOKUP(cgrp_a_map,
            bucket,
            0x00000, // 20b field
            0x0,
            0x0,
            0x0,
            0x0,
            0x0,
            0x00000000);
    }
}

// EM_A_HASH_CAM

static void set_EM_A_HASH_CAM
(
    mby_ppe_cgrp_a_map * const cgrp_a_map,
    fm_byte entry,
    fm_byte word,
    fm_uint64 DATA
)
{
    em_a_hash_cam_r * const em_a_hash_cam = &(cgrp_a_map->EM_A_HASH_CAM[entry][word]);
    em_a_hash_cam->DATA = DATA;
}

static void init_EM_A_HASH_CAM_REG
(
    mby_ppe_cgrp_a_map  * const cgrp_a_map
)
{
    for (fm_uint entry = 0 ; entry < MBY_FFU_HASH_CAM_ENTRIES_1 ; entry++)
    {
        for(fm_uint word = 0 ; word < MBY_FFU_HASH_CAM_ENTRIES_0 ; word++)
        {
            set_EM_A_HASH_CAM(cgrp_a_map, entry, word, (fm_uint64)0x0000000000000000);
        }
    }
}

// EM_A_HASH_CAM_EN

static void set_EM_A_HASH_CAM_EN
(
    mby_ppe_cgrp_a_map * const cgrp_a_map,
    fm_byte row,
    fm_byte rule,
    fm_byte select
)
{
    em_a_hash_cam_en_r * const em_a_hash_cam_en = &(cgrp_a_map->EM_A_HASH_CAM_EN[rule][row]);
    em_a_hash_cam_en->MASK |= 1 << select;
}

static void init_EM_A_HASH_CAM_EN_REG
(
    mby_ppe_cgrp_a_map  * const cgrp_a_map
)
{
    for (fm_uint row = 0 ; row < MBY_FFU_HASH_CAM_EN_ENTRIES_1 ; row++)
    {
        for(fm_uint rule = 0 ; rule < MBY_FFU_HASH_CAM_EN_ENTRIES_0 ; rule++)
        {
            set_EM_A_HASH_CAM_EN(cgrp_a_map, row, rule, 0x00);
        }
    }
}

// EM_A_KEY_SEL0

static void set_EM_A_KEY_SEL0
(
    mby_ppe_cgrp_a_map * const cgrp_a_map,
    fm_bool hash,
    fm_byte scenario,
    fm_uint32 key8_mask
)
{
    em_a_key_sel0_r * const em_a_key_sel0 = &(cgrp_a_map->EM_A_KEY_SEL0[hash][scenario]);
    em_a_key_sel0->KEY8_MASK = key8_mask;
}

static void init_EM_A_KEY_SEL0_REG
(
    mby_ppe_cgrp_a_map  * const cgrp_a_map
)
{
    for (fm_uint hash = 0 ; hash < MBY_FFU_KEY_MASK0_ENTRIES_1 ; hash++)
    {
        for(fm_uint scenario = 0 ; scenario < MBY_FFU_KEY_MASK0_ENTRIES_0 ; scenario++)
        {
            set_EM_A_KEY_SEL0(cgrp_a_map, hash, scenario, 0x00000000);
        }
    }
}

// EM_A_KEY_SEL1

static void set_EM_A_KEY_SEL1
(
    mby_ppe_cgrp_a_map * const cgrp_a_map,
    fm_bool hash,
    fm_byte scenario,
    uint4 key_mask_sel,
    uint16 key32_mask,
    uint32 key16_mask
)
{
    em_a_key_sel1_r * const em_a_key_sel1 = &(cgrp_a_map->EM_A_KEY_SEL1[hash][scenario]);
    em_a_key_sel1->KEY_MASK_SEL = key_mask_sel;
    em_a_key_sel1->KEY32_MASK   = key32_mask;
    em_a_key_sel1->KEY16_MASK   = key16_mask;
}

static void init_EM_A_KEY_SEL1_REG
(
    mby_ppe_cgrp_a_map  * const cgrp_a_map
)
{
    for (fm_uint hash = 0 ; hash < MBY_FFU_KEY_MASK0_ENTRIES_1 ; hash++)
    {
        for (fm_uint scenario = 0 ; scenario < MBY_FFU_KEY_MASK0_ENTRIES_0 ; scenario++)
        {
            set_EM_A_KEY_SEL1(cgrp_a_map, hash, scenario,
                (uint4)0x0,
                (uint16)0x0000,
                (uint32)0x00000000);
        }
    }
}

// EM_A_KEY_MASK

static void set_EM_A_KEY_MASK
(
    mby_ppe_cgrp_a_map * const cgrp_a_map,
    fm_bool hash,
    fm_byte key_mask_sel,
    fm_byte dw,
    uint64 mask
)
{
    fm_byte mask_id = (key_mask_sel * 2) + dw;
    em_a_key_mask_r * const em_a_key_mask = &(cgrp_a_map->EM_A_KEY_MASK[hash][mask_id]);
    em_a_key_mask->MASK = mask;
}

static void init_EM_A_KEY_MASK_REG
(
    mby_ppe_cgrp_a_map  * const cgrp_a_map
)
{
    for (fm_uint hash = 0 ; hash < MBY_FFU_KEY_MASK0_ENTRIES_1 ; hash++)
    {
        for (fm_uint key_mask_sel = 0 ; key_mask_sel < 16 ; key_mask_sel++) // <-- 16?? hash key profiles number REVISIT!!!
        {
            for (fm_uint dw = 0 ; dw < 2 ; dw++)
            {
                set_EM_A_KEY_MASK(cgrp_a_map, hash, key_mask_sel, dw, (uint64)0x00000000);
            }
        }
    }
}

// EM_A_HASH_MISS

static void set_EM_A_HASH_MISS
(
    mby_ppe_cgrp_a_map * const cgrp_a_map,
    fm_bool hash,
    fm_byte profile,
    fm_uint32 action1,
    fm_uint32 action0
)
{
    em_a_hash_miss_r * const em_a_hash_miss = &(cgrp_a_map->EM_A_HASH_MISS[hash][profile]);
    em_a_hash_miss->ACTION1 = action1;
    em_a_hash_miss->ACTION0 = action0;
}

static void init_EM_A_HASH_MISS_REG
(
    mby_ppe_cgrp_a_map  * const cgrp_a_map
)
{
    for (fm_uint hash = 0 ; hash < MBY_FFU_HASH_MISS_ENTRIES_1 ; hash++)
    {
        for (fm_uint profile = 0 ; profile < MBY_FFU_HASH_MISS_ENTRIES_0 ; profile++)
        {
            set_EM_A_HASH_MISS(cgrp_a_map, hash, profile, (uint32)0x0000, (uint32)0x0000);
        }
    }
}

// EM_A_HASH_CFG

static void set_EM_A_HASH_CFG
(
    mby_ppe_cgrp_a_map * const cgrp_a_map,
    fm_byte profile,
    fm_bool mode,
    fm_uint16 base_ptr_0,
    fm_uint16 base_ptr_1,
    fm_byte hash_size_0,
    fm_byte hash_size_1,
    fm_byte entry_size_0,
    fm_byte entry_size_1
)
{
    em_a_hash_cfg_r * const em_a_hash_cfg = &(cgrp_a_map->EM_A_HASH_CFG[profile]);
    em_a_hash_cfg->MODE         = mode;
    em_a_hash_cfg->BASE_PTR_0   = base_ptr_0;
    em_a_hash_cfg->BASE_PTR_1   = base_ptr_1;
    em_a_hash_cfg->HASH_SIZE_0  = hash_size_0;
    em_a_hash_cfg->HASH_SIZE_1  = hash_size_1;
    em_a_hash_cfg->ENTRY_SIZE_0 = entry_size_0;
    em_a_hash_cfg->ENTRY_SIZE_1 = entry_size_1;
}

static void init_EM_A_HASH_CFG_REG
(
    mby_ppe_cgrp_a_map  * const cgrp_a_map
)
{
    for (fm_uint profile = 0 ; profile < MBY_FFU_HASH_CFG_ENTRIES_0 ; profile++)
    {
        set_EM_A_HASH_CFG(cgrp_a_map, profile,
            FALSE,
            (uint13)0x00,
            (uint13)0x00,
            (uint5)0x0,
            (uint5)0x0,
            (uint5)0x0,
            (uint5)0x0
            );
    }
}

// EM_B_HASH_LOOKUP

static void set_EM_B_HASH_LOOKUP
(
    mby_ppe_cgrp_b_map * const cgrp_b_map,
    fm_uint bucket,
    uint20 ptr,
    uint4 select_4,
    uint4 select_3,
    uint4 select_2,
    uint4 select_1,
    uint4 select_0,
    uint32 mask
)
{
    em_b_hash_lookup_r * const em_b_hash_lookup = &(cgrp_b_map->EM_B_HASH_LOOKUP[bucket]);
    em_b_hash_lookup->PTR       = ptr;
    em_b_hash_lookup->SELECT_4  = select_4;
    em_b_hash_lookup->SELECT_3  = select_3;
    em_b_hash_lookup->SELECT_2  = select_2;
    em_b_hash_lookup->SELECT_1  = select_1;
    em_b_hash_lookup->SELECT_0  = select_0;
    em_b_hash_lookup->MASK      = mask;
}

static void init_EM_B_HASH_LOOKUP_REG
(
    mby_ppe_cgrp_b_map * const cgrp_b_map
)
{
    for (fm_uint bucket = 0 ; bucket < MBY_EM_B_HASH_LOOKUP_ENTRIES ; bucket++)
    {
        set_EM_B_HASH_LOOKUP(cgrp_b_map,
            bucket,
            0x00000, // 20b field
            0x0,
            0x0,
            0x0,
            0x0,
            0x0,
            0x00000000);
    }
}

// EM_B_HASH_CAM

static void set_EM_B_HASH_CAM
(
    mby_ppe_cgrp_b_map * const cgrp_b_map,
    fm_byte entry,
    fm_byte word,
    fm_uint64 DATA
)
{
    em_b_hash_cam_r * const em_b_hash_cam = &(cgrp_b_map->EM_B_HASH_CAM[entry][word]);
    em_b_hash_cam->DATA = DATA;
}

static void init_EM_B_HASH_CAM_REG
(
    mby_ppe_cgrp_b_map * const cgrp_b_map
)
{
    for (fm_uint entry = 0 ; entry < MBY_FFU_HASH_CAM_ENTRIES_1 ; entry++)
    {
        for(fm_uint word = 0 ; word < MBY_FFU_HASH_CAM_ENTRIES_0 ; word++)
        {
            set_EM_B_HASH_CAM(cgrp_b_map, entry, word, (fm_uint64)0x0000000000000000);
        }
    }
}

// EM_B_HASH_CAM_EN

static void set_EM_B_HASH_CAM_EN
(
    mby_ppe_cgrp_b_map * const cgrp_b_map,
    fm_byte row,
    fm_byte rule,
    fm_byte select
)
{
    em_b_hash_cam_en_r * const em_b_hash_cam_en = &(cgrp_b_map->EM_B_HASH_CAM_EN[rule][row]);
    em_b_hash_cam_en->MASK |= 1 << select;
}

static void init_EM_B_HASH_CAM_EN_REG
(
    mby_ppe_cgrp_b_map * const cgrp_b_map
)
{
    for (fm_uint row = 0 ; row < MBY_FFU_HASH_CAM_EN_ENTRIES_1 ; row++)
    {
        for(fm_uint rule = 0 ; rule < MBY_FFU_HASH_CAM_EN_ENTRIES_0 ; rule++)
        {
            set_EM_B_HASH_CAM_EN(cgrp_b_map, row, rule, 0x00);
        }
    }
}

// EM_B_KEY_SEL0

static void set_EM_B_KEY_SEL0
(
    mby_ppe_cgrp_b_map * const cgrp_b_map,
    fm_bool hash,
    fm_byte scenario,
    fm_uint32 key8_mask
)
{
    em_b_key_sel0_r * const em_b_key_sel0 = &(cgrp_b_map->EM_B_KEY_SEL0[hash][scenario]);
    em_b_key_sel0->KEY8_MASK = key8_mask;
}

static void init_EM_B_KEY_SEL0_REG
(
    mby_ppe_cgrp_b_map * const cgrp_b_map
)
{
    for (fm_uint hash = 0 ; hash < MBY_FFU_KEY_MASK0_ENTRIES_1 ; hash++)
    {
        for(fm_uint scenario = 0 ; scenario < MBY_FFU_KEY_MASK0_ENTRIES_0 ; scenario++)
        {
            set_EM_B_KEY_SEL0(cgrp_b_map, hash, scenario, 0x00000000);
        }
    }
}

// EM_B_KEY_SEL1

static void set_EM_B_KEY_SEL1
(
    mby_ppe_cgrp_b_map * const cgrp_b_map,
    fm_bool hash,
    fm_byte scenario,
    uint4 key_mask_sel,
    uint16 key32_mask,
    uint32 key16_mask
)
{
    em_b_key_sel1_r * const em_b_key_sel1 = &(cgrp_b_map->EM_B_KEY_SEL1[hash][scenario]);
    em_b_key_sel1->KEY_MASK_SEL = key_mask_sel;
    em_b_key_sel1->KEY32_MASK   = key32_mask;
    em_b_key_sel1->KEY16_MASK   = key16_mask;
}

static void init_EM_B_KEY_SEL1_REG
(
    mby_ppe_cgrp_b_map * const cgrp_b_map
)
{
    for (fm_uint hash = 0 ; hash < MBY_FFU_KEY_MASK0_ENTRIES_1 ; hash++)
    {
        for (fm_uint scenario = 0 ; scenario < MBY_FFU_KEY_MASK0_ENTRIES_0 ; scenario++)
        {
            set_EM_B_KEY_SEL1(cgrp_b_map, hash, scenario,
                (uint4)0x0,
                (uint16)0x0000,
                (uint32)0x00000000);
        }
    }
}

// EM_B_KEY_MASK

static void set_EM_B_KEY_MASK
(
    mby_ppe_cgrp_b_map * const cgrp_b_map,
    fm_bool hash,
    fm_byte key_mask_sel,
    fm_byte dw,
    uint64 mask
)
{
    fm_byte mask_id = (key_mask_sel * 2) + dw;
    em_b_key_mask_r * const em_b_key_mask = &(cgrp_b_map->EM_B_KEY_MASK[hash][mask_id]);
    em_b_key_mask->MASK = mask;
}

static void init_EM_B_KEY_MASK_REG
(
    mby_ppe_cgrp_b_map * const cgrp_b_map
)
{
    for (fm_uint hash = 0 ; hash < MBY_FFU_KEY_MASK0_ENTRIES_1 ; hash++)
    {
        for (fm_uint key_mask_sel = 0 ; key_mask_sel < 16 ; key_mask_sel++) // <-- 16?? hash key profiles number REVISIT!!!
        {
            for (fm_uint dw = 0 ; dw < 2 ; dw++)
            {
                set_EM_B_KEY_MASK(cgrp_b_map, hash, key_mask_sel, dw, (uint64)0x00000000);
            }
        }
    }
}

// EM_B_HASH_MISS

static void set_EM_B_HASH_MISS
(
    mby_ppe_cgrp_b_map * const cgrp_b_map,
    fm_bool hash,
    fm_byte profile,
    fm_uint32 action1,
    fm_uint32 action0
)
{
    em_b_hash_miss_r * const em_b_hash_miss = &(cgrp_b_map->EM_B_HASH_MISS[hash][profile]);
    em_b_hash_miss->ACTION1 = action1;
    em_b_hash_miss->ACTION0 = action0;
}

static void init_EM_B_HASH_MISS_REG
(
    mby_ppe_cgrp_b_map * const cgrp_b_map
)
{
    for (fm_uint hash = 0 ; hash < MBY_FFU_HASH_MISS_ENTRIES_1 ; hash++)
    {
        for (fm_uint profile = 0 ; profile < MBY_FFU_HASH_MISS_ENTRIES_0 ; profile++)
        {
            set_EM_B_HASH_MISS(cgrp_b_map, hash, profile, (uint32)0x0000, (uint32)0x0000);
        }
    }
}

// EM_B_HASH_CFG

static void set_EM_B_HASH_CFG
(
    mby_ppe_cgrp_b_map * const cgrp_b_map,
    fm_byte profile,
    fm_bool mode,
    fm_uint16 base_ptr_0,
    fm_uint16 base_ptr_1,
    fm_byte hash_size_0,
    fm_byte hash_size_1,
    fm_byte entry_size_0,
    fm_byte entry_size_1
)
{
    em_b_hash_cfg_r * const em_b_hash_cfg = &(cgrp_b_map->EM_B_HASH_CFG[profile]);
    em_b_hash_cfg->MODE = mode;
    em_b_hash_cfg->BASE_PTR_0 = base_ptr_0;
    em_b_hash_cfg->BASE_PTR_1 = base_ptr_1;
    em_b_hash_cfg->HASH_SIZE_0 = hash_size_0;
    em_b_hash_cfg->HASH_SIZE_1 = hash_size_1;
    em_b_hash_cfg->ENTRY_SIZE_0 = entry_size_0;
    em_b_hash_cfg->ENTRY_SIZE_1 = entry_size_1;
}

static void init_EM_B_HASH_CFG_REG
(
    mby_ppe_cgrp_b_map * const cgrp_b_map
)
{
    for (fm_uint profile = 0 ; profile < MBY_FFU_HASH_CFG_ENTRIES_0 ; profile++)
    {
        set_EM_B_HASH_CFG(cgrp_b_map, profile,
            FALSE,
            (uint13)0x00,
            (uint13)0x00,
            (uint5)0x0,
            (uint5)0x0,
            (uint5)0x0,
            (uint5)0x0
            );
    }
}

// FWD_TABLE0

static void set_FWD_TABLE0
(
    mby_shm_map * const shm_map,
    fm_uint i,
    fm_uint j,
    fm_uint64 data
)
{
    fwd_table0_r * const fwd_table0 = &(shm_map->FWD_TABLE0[i][j]);
    fwd_table0->DATA = data;
}

static void init_FWD_TABLE0_REG
(
    mby_shm_map * const shm_map
)
{
    for (fm_uint i = 0 ; i < 3072 ; i++)
    {
        for (fm_uint j = 0 ; j < 256 ; j++)
        {
            fm_uint64 data = 0x0000000000000000;
            set_FWD_TABLE0(shm_map, i, j, data);
        }
    }
}

// FWD_TABLE1

static void set_FWD_TABLE1
(
    mby_shm_map * const shm_map,
    fm_uint i,
    fm_uint j,
    fm_uint64 data
)
{
    fwd_table1_r * const fwd_table1 = &(shm_map->FWD_TABLE1[i][j]);
    fwd_table1->DATA = data;
}

static void init_FWD_TABLE1_REG
(
    mby_shm_map * const shm_map
)
{
    for (fm_uint i = 0 ; i < 512 ; i++)
    {
        for (fm_uint j = 0 ; j < 256 ; j++)
        {
            fm_uint64 data = 0x0000000000000000;
            set_FWD_TABLE1(shm_map, i, j, data);
        }
    }
}

static void cpy_actions
(
    mbyClassifierActions * const in,
    mbyClassifierActions * const out
)
{
    for (fm_uint i = 0; i < MBY_FFU_ACT24; i++) {
        out->act24[i].prec = in->act24[i].prec;
        out->act24[i].val = in->act24[i].val;
    }

    for (fm_uint i = 0; i < MBY_FFU_ACT4; i++) {
        out->act4[i].prec = in->act4[i].prec;
        out->act4[i].val = in->act4[i].val;
    }

    for (fm_uint i = 0; i < MBY_FFU_ACT1; i++) {
        out->act1[i].prec = in->act1[i].prec;
        out->act1[i].val = in->act1[i].val;
    }
}

static void init_actions(mbyClassifierActions * const actions_in)
{
    for (fm_uint i = 0; i < MBY_FFU_ACT24; i++) {
        actions_in->act24[i].prec = 1;
        actions_in->act24[i].val  = 0;
    }

    for (fm_uint i = 0; i < MBY_FFU_ACT4; i++) {
        actions_in->act4[i].prec = 1;
        actions_in->act4[i].val = 0;
    }

    for (fm_uint i = 0; i < MBY_FFU_ACT1; i++) {
        actions_in->act1[i].prec = 1;
        actions_in->act1[i].val = 0;
    }
}

static void init_keys(mbyClassifierKeys * const keys)
{
    for (fm_uint i = 0; i < MBY_FFU_KEY32; i++)
        keys->key32[i] = 0;

    for (fm_uint i = 0; i < MBY_FFU_KEY16; i++)
        keys->key16[i] = 0;

    for (fm_uint i = 0; i < MBY_FFU_KEY8;  i++)
        keys->key8 [i] = 0;
}

static void initRegs
(
    mby_ppe_cgrp_a_map * const cgrp_a_map,
    mby_ppe_cgrp_b_map * const cgrp_b_map,
    mby_shm_map        * const shm_map
)
{
    // Clasifier A regs
    init_EM_A_HASH_LOOKUP_REG(cgrp_a_map);
    init_EM_A_HASH_CAM_REG(cgrp_a_map);
    init_EM_A_HASH_CAM_EN_REG(cgrp_a_map);
    init_EM_A_KEY_SEL0_REG(cgrp_a_map);
    init_EM_A_KEY_SEL1_REG(cgrp_a_map);
    init_EM_A_KEY_MASK_REG(cgrp_a_map);
    init_EM_A_HASH_MISS_REG(cgrp_a_map);
    init_EM_A_HASH_CFG_REG(cgrp_a_map);
    // Clasifier B regs
    init_EM_B_HASH_LOOKUP_REG(cgrp_b_map);
    init_EM_B_HASH_CAM_REG(cgrp_b_map);
    init_EM_B_HASH_CAM_EN_REG(cgrp_b_map);
    init_EM_B_KEY_SEL0_REG(cgrp_b_map);
    init_EM_B_KEY_SEL1_REG(cgrp_b_map);
    init_EM_B_KEY_MASK_REG(cgrp_b_map);
    init_EM_B_HASH_MISS_REG(cgrp_b_map);
    init_EM_B_HASH_CFG_REG(cgrp_b_map);
    // Shared memory
    init_FWD_TABLE0_REG(shm_map);
    init_FWD_TABLE1_REG(shm_map);
}

static void initInputs
(
    mbyMapperToClassifier * const map2cla
)
{
    mbyClassifierActions * const actions_in  = &(map2cla->FFU_ACTIONS);
    mbyClassifierKeys    * const keys        = &(map2cla->FFU_KEYS);
    fm_byte              * const scenario_in = &(map2cla->FFU_SCENARIO);

    init_actions(actions_in);
    init_keys(keys);
    *scenario_in = 0;
}

static void setRegs_basic
(
    mby_ppe_cgrp_a_map    * const cgrp_a_map,
    mby_ppe_cgrp_b_map    * const cgrp_b_map,
    mby_shm_map           * const shm_map
)
{
    // EM_A_KEY_SEL0[hash][scenario])
    set_EM_A_KEY_SEL0(cgrp_a_map,
        0,  // hash
        17, // scenario <-- profile? REVISIT!!!
        0x20    // key8_mask  (0000 0000 0000 0000 0000 0000 0010 0000 == KEY8[5] )
    );

    // EM_A_KEY_SEL1[hash][scenario])
    set_EM_A_KEY_SEL1(cgrp_a_map,
        0,  // hash
        17, // scenario <-- profile? REVISIT!!!
        0x0,    // key_mask_sel <-- Might need to REVISIT!!!
        0x10,   // key32_mask (0000 0000 0001 0000                     == KEY32[4])
        0x0     // key16_mask (0000 0000 0000 0000 0000 0000 0000 0000 == NONE    )
    );

    // EM_A_HASH_CFG[scenario]
    set_EM_A_HASH_CFG(cgrp_a_map,
        17, // scenario <-- profile? REVISIT!!!
        1,      // mode (64B == non-split mode)
        0x0,    // base_ptr_0
        0x0,    // base_ptr_1
        0x6,    // hash_size_0
        0x0,    // hash_size_1
        0x3,    // entry_size_0
        0       // entry_size_1
    );

    // EM_A_HASH_LOOKUP[bucket]
    set_EM_A_HASH_LOOKUP(cgrp_a_map,
        0xc, // bucket
        0x0,    // ptr
        0x4,    // select_4
        0x3,    // select_3
        0x2,    // select_2
        0x1,    // select_1
        0x0,    // select_0
        0x20000 // mask (0000 0000 0000 0010 0000 0000 0000 0000 == 17)
    );

    // FWD_TABLE0[entry_idx][port_id]
    set_FWD_TABLE0(shm_map,
        0,  // entry_idx
        0,  // port_id ? <-- REVISIT!
        0x12341234ULL << 32 // hash_entry_keys
    );

    // FWD_TABLE0[entry_idx][port_id]
    set_FWD_TABLE0(shm_map,
        1,  // entry_idx
        0,  // port_id ? <-- REVISIT!
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
    map2cla->FFU_KEYS.key32[0]  = 0xB00A1B2C;
    map2cla->FFU_KEYS.key32[1]  = 0x12341234;
    map2cla->FFU_KEYS.key32[4]  = 0x12341234;
    map2cla->FFU_KEYS.key16[6]  = 0x0011;
    map2cla->FFU_KEYS.key16[7]  = 0x2233;
    map2cla->FFU_KEYS.key16[8]  = 0x4455;
    map2cla->FFU_KEYS.key16[9]  = 0x0001;
    map2cla->FFU_KEYS.key16[10] = 0x0101;
    map2cla->FFU_KEYS.key16[11] = 0x0101;
    map2cla->FFU_KEYS.key16[12] = 0x0800;
    map2cla->FFU_KEYS.key16[13] = 0x0001;
    map2cla->FFU_KEYS.key16[14] = 0x0001;
    map2cla->FFU_KEYS.key16[15] = 0x0001;
    map2cla->FFU_KEYS.key16[16] = 0x0001;
    map2cla->FFU_KEYS.key16[17] = 0x0203;
    map2cla->FFU_KEYS.key16[19] = 0x0203;
    map2cla->FFU_KEYS.key16[31] = 0x0002;
    map2cla->FFU_KEYS.key8[3]   = 0x11;
    map2cla->FFU_KEYS.key8[6]   = 0x40;
    map2cla->FFU_KEYS.key8[20]  = 0xff;
    map2cla->FFU_KEYS.key8[21]  = 0x06;
    map2cla->FFU_KEYS.key8[23]  = 0x14;
    map2cla->FFU_KEYS.key8[25]  = 0x40;
    map2cla->FFU_KEYS.key8[28]  = 0x01;
    map2cla->FFU_KEYS.key8[29]  = 0x02;
    map2cla->FFU_KEYS.key8[43]  = 0x60;
    map2cla->FFU_KEYS.key8[45]  = 0x02;
    map2cla->FFU_KEYS.key8[55]  = 0xa1;
    map2cla->FFU_KEYS.key8[56]  = 0x07;
    map2cla->FFU_KEYS.key8[57]  = 0xf6;
    map2cla->FFU_KEYS.key8[58]  = 0xe5;
    map2cla->FFU_KEYS.key8[59]  = 0xd4;

    map2cla->FFU_ACTIONS.act4[4].val  = 1;
    map2cla->FFU_ACTIONS.act1[20].val = 1;
    map2cla->FFU_ACTIONS.act1[22].val = 1;

    map2cla->FFU_SCENARIO = 0x11; // 17
}

static void simple_exactmatch_basic_test_setup
(
    mby_ppe_cgrp_a_map    * const cgrp_a_map,
    mby_ppe_cgrp_b_map    * const cgrp_b_map,
    mby_shm_map           * const shm_map,
    mbyMapperToClassifier * const map2cla
)
{
    initRegs(cgrp_a_map, cgrp_b_map, shm_map);

    setRegs_basic(cgrp_a_map, cgrp_b_map, shm_map);

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
    mby_ppe_cgrp_a_map   *cgrp_a_map = NULL;
    mby_ppe_cgrp_b_map   *cgrp_b_map = NULL;
    mby_shm_map          *shm_map = NULL;
    allocMem(&cgrp_a_map, &cgrp_b_map, &shm_map);
    initRegs(cgrp_a_map, cgrp_b_map, shm_map);


    mbyMapperToClassifier map2cla = { 0 };

    mbyMapperToClassifier const * const in  = &map2cla;
    mbyClassifierActions * const actions_out = &(map2cla.FFU_ACTIONS);
    setup(cgrp_a_map, cgrp_b_map, shm_map, &map2cla);
    fm_uint32 actions[MBY_EM_MAX_ACTIONS_NUM] = { 0 };

    mbyMatchExact
    (
        cgrp_a_map,
        cgrp_b_map,
        shm_map,
        &(in->FFU_KEYS),
        in->FFU_SCENARIO,
        MBY_CLA_GROUP_A,
        actions
    );

    int ret = check(actions);

    // Free up memory:
    freeMem(cgrp_a_map, cgrp_b_map, shm_map);

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