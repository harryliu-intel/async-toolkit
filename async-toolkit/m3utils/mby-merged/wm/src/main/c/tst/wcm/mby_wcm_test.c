#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include <string.h>
#include <assert.h>
#include <mby_wcm.h>
#include <mby_mapper.h>

#include <mby_top_map.h>

#include <mby_common.h>
#include <mby_pipeline.h>

#define COLOR_RED     "\x1b[31m"
#define COLOR_GREEN   "\x1b[32m"
#define COLOR_RESET   "\x1b[0m"


#define SIMPLE_WCM_TEST(name, fails) {if (!run_on_simple_wcm(simple_wcm_ ## name ## _test_setup, \
                simple_wcm_ ## name ## _test_check)) pass(#name); else {++fails; fail(#name);} }

typedef void(*run_on_simple_wcm_setup_fn)
(
    mby_ppe_cgrp_b_map  * const cgrp_b_map,
    mbyMapperToClassifier * const map2cla
);
typedef int(*run_on_simple_wcm_check_fn)(fm_uint32 actions[MBY_WCM_MAX_ACTIONS_NUM]);

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
    mby_ppe_cgrp_b_map  **cgrp_b_map
)
{
    *cgrp_b_map = malloc(sizeof(mby_ppe_cgrp_b_map));
    if (*cgrp_b_map == NULL) {
        printf("Could not allocate heap memory for classifier B map -- exiting!\n");
        exit(-1);
    }
}

static void freeMem
(
    mby_ppe_cgrp_b_map  * const cgrp_b_map
)
{
    free( cgrp_b_map);
}

static void set_WCM_ACTION_REG
(
    mby_ppe_cgrp_b_map  * const cgrp_b_map,
    fm_uint i,
    fm_uint j,
    fm_uint nr_action,
    fm_uint32 value
)
{
    wcm_action_r * const em_b_wcm_action = &(cgrp_b_map->WCM_ACTION[i][j]);

    switch(nr_action)
    {
        case 0: em_b_wcm_action->ACTION0 = value; break;
        case 1: em_b_wcm_action->ACTION1 = value; break;
        default:
        {
            em_b_wcm_action->ACTION0 = 0x0;
            em_b_wcm_action->ACTION1 = 0x0;
        }
    }
}

static void init_WCM_ACTION_REG
(
    mby_ppe_cgrp_b_map  * const cgrp_b_map
)
{
    for (fm_uint i = 0; i < MBY_FFU_ACTION_ENTRIES_1; i++)
    {
        for(fm_uint j = 0; j < MBY_FFU_TCAM_ENTRIES_0; j++)
        {
            set_WCM_ACTION_REG(cgrp_b_map, i, j, -1, 0x0);
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

static void set_WCM_TCAM_REG
(
    mby_ppe_cgrp_b_map  * const cgrp_b_map,
    fm_uint i,
    fm_uint j,
    fm_byte key_top_invert,
    fm_uint32 key_invert,
    fm_byte key_top,
    fm_uint32 key
)
{
    wcm_tcam_r * const em_b_wcm_tcam = &(cgrp_b_map->WCM_TCAM[i][j]);
    em_b_wcm_tcam->KEY_TOP_INVERT = key_top_invert;
    em_b_wcm_tcam->KEY_INVERT = key_invert;
    em_b_wcm_tcam->KEY_TOP = key_top;
    em_b_wcm_tcam->KEY = key;
}

static void init_WCM_TCAM_REG
(
    mby_ppe_cgrp_b_map  * const cgrp_b_map
)
{
    for (fm_uint i = 0; i < MBY_FFU_TCAM_CFG_ENTRIES_1; i++)
    {
        for(fm_uint j = 0; j < MBY_FFU_TCAM_ENTRIES_0; j++)
        {
            set_WCM_TCAM_REG(cgrp_b_map, i, j, 0xff, 0xffffffff, 0xff, 0xffffffff);
        }
    }
}

static void set_index_WCM_ACTION_REG
(
    mby_ppe_cgrp_b_map  * const cgrp_b_map,
    fm_uint i,
    fm_uint index_nr,
    fm_byte value
)
{
    wcm_action_cfg_r * const em_b_wcm_action_cfg = &(cgrp_b_map->WCM_ACTION_CFG[i]);
    switch(index_nr)
    {
        case 0: em_b_wcm_action_cfg->INDEX_0 = value; break;
        case 1: em_b_wcm_action_cfg->INDEX_1 = value; break;
        case 2: em_b_wcm_action_cfg->INDEX_2 = value; break;
        case 3: em_b_wcm_action_cfg->INDEX_3 = value; break;
        case 4: em_b_wcm_action_cfg->INDEX_4 = value; break;
        case 5: em_b_wcm_action_cfg->INDEX_5 = value; break;
        case 6: em_b_wcm_action_cfg->INDEX_6 = value; break;
        case 7: em_b_wcm_action_cfg->INDEX_7 = value; break;
        case 8: em_b_wcm_action_cfg->INDEX_8 = value; break;
        case 9: em_b_wcm_action_cfg->INDEX_9 = value; break;
        case 10: em_b_wcm_action_cfg->INDEX_10 = value; break;
        case 11: em_b_wcm_action_cfg->INDEX_11 = value; break;
        case 12: em_b_wcm_action_cfg->INDEX_12 = value; break;
        case 13: em_b_wcm_action_cfg->INDEX_13 = value; break;
        case 14: em_b_wcm_action_cfg->INDEX_14 = value; break;
        case 15: em_b_wcm_action_cfg->INDEX_15 = value; break;
        case 16: em_b_wcm_action_cfg->INDEX_16 = value; break;
        case 17: em_b_wcm_action_cfg->INDEX_17 = value; break;
        case 18: em_b_wcm_action_cfg->INDEX_18 = value; break;
        case 19: em_b_wcm_action_cfg->INDEX_19 = value; break;
        default:
        {
            em_b_wcm_action_cfg->INDEX_0 = 0x0;
            em_b_wcm_action_cfg->INDEX_1 = 0x0;
            em_b_wcm_action_cfg->INDEX_2 = 0x0;
            em_b_wcm_action_cfg->INDEX_3 = 0x0;
            em_b_wcm_action_cfg->INDEX_4 = 0x0;
            em_b_wcm_action_cfg->INDEX_5 = 0x0;
            em_b_wcm_action_cfg->INDEX_6 = 0x0;
            em_b_wcm_action_cfg->INDEX_7 = 0x0;
            em_b_wcm_action_cfg->INDEX_8 = 0x0;
            em_b_wcm_action_cfg->INDEX_9 = 0x0;
            em_b_wcm_action_cfg->INDEX_10 = 0x0;
            em_b_wcm_action_cfg->INDEX_11 = 0x0;
            em_b_wcm_action_cfg->INDEX_12 = 0x0;
            em_b_wcm_action_cfg->INDEX_13 = 0x0;
            em_b_wcm_action_cfg->INDEX_14 = 0x0;
            em_b_wcm_action_cfg->INDEX_15 = 0x0;
            em_b_wcm_action_cfg->INDEX_16 = 0x0;
            em_b_wcm_action_cfg->INDEX_17 = 0x0;
            em_b_wcm_action_cfg->INDEX_18 = 0x0;
            em_b_wcm_action_cfg->INDEX_19 = 0x0;
        }
    }
}

static void set_enable_WCM_ACTION_REG
(
    mby_ppe_cgrp_b_map  * const cgrp_b_map,
    fm_uint i,
    fm_uint enable_nr,
    fm_byte value
)
{
    wcm_action_cfg_r * const em_b_wcm_action_cfg = &(cgrp_b_map->WCM_ACTION_CFG[i]);
    switch(enable_nr)
    {
        case 0: em_b_wcm_action_cfg->ENABLE_0 = value; break;
        case 1: em_b_wcm_action_cfg->ENABLE_1 = value; break;
        case 2: em_b_wcm_action_cfg->ENABLE_2 = value; break;
        case 3: em_b_wcm_action_cfg->ENABLE_3 = value; break;
        case 4: em_b_wcm_action_cfg->ENABLE_4 = value; break;
        case 5: em_b_wcm_action_cfg->ENABLE_5 = value; break;
        case 6: em_b_wcm_action_cfg->ENABLE_6 = value; break;
        case 7: em_b_wcm_action_cfg->ENABLE_7 = value; break;
        case 8: em_b_wcm_action_cfg->ENABLE_8 = value; break;
        case 9: em_b_wcm_action_cfg->ENABLE_9 = value; break;
        case 10: em_b_wcm_action_cfg->ENABLE_10 = value; break;
        case 11: em_b_wcm_action_cfg->ENABLE_11 = value; break; 
        case 12:em_b_wcm_action_cfg->ENABLE_12 = value; break;
        case 13: em_b_wcm_action_cfg->ENABLE_13 = value; break;
        case 14: em_b_wcm_action_cfg->ENABLE_14 = value; break;
        case 15: em_b_wcm_action_cfg->ENABLE_15 = value; break;
        case 16: em_b_wcm_action_cfg->ENABLE_16 = value; break;
        case 17: em_b_wcm_action_cfg->ENABLE_17 = value; break;
        case 18: em_b_wcm_action_cfg->ENABLE_18 = value; break;
        case 19: em_b_wcm_action_cfg->ENABLE_19 = value; break;
        default:
        {
            em_b_wcm_action_cfg->ENABLE_0 = 0x0;
            em_b_wcm_action_cfg->ENABLE_1 = 0x0;
            em_b_wcm_action_cfg->ENABLE_2 = 0x0;
            em_b_wcm_action_cfg->ENABLE_3 = 0x0;
            em_b_wcm_action_cfg->ENABLE_4 = 0x0;
            em_b_wcm_action_cfg->ENABLE_5 = 0x0;
            em_b_wcm_action_cfg->ENABLE_6 = 0x0;
            em_b_wcm_action_cfg->ENABLE_7 = 0x0;
            em_b_wcm_action_cfg->ENABLE_8 = 0x0;
            em_b_wcm_action_cfg->ENABLE_9 = 0x0;
            em_b_wcm_action_cfg->ENABLE_10 = 0x0;
            em_b_wcm_action_cfg->ENABLE_11 = 0x0;
            em_b_wcm_action_cfg->ENABLE_12 = 0x0;
            em_b_wcm_action_cfg->ENABLE_13 = 0x0;
            em_b_wcm_action_cfg->ENABLE_14 = 0x0;
            em_b_wcm_action_cfg->ENABLE_15 = 0x0;
            em_b_wcm_action_cfg->ENABLE_16 = 0x0;
            em_b_wcm_action_cfg->ENABLE_17 = 0x0;
            em_b_wcm_action_cfg->ENABLE_18 = 0x0;
            em_b_wcm_action_cfg->ENABLE_19 = 0x0;
        }
    }
}
static void init_WCM_ACTION_CFG_REG
(
    mby_ppe_cgrp_b_map  * const cgrp_b_map
)
{
    for(fm_uint i = 0; i < 64; i++)
    {
        set_index_WCM_ACTION_REG(cgrp_b_map, i, -1, 0x0);
        set_enable_WCM_ACTION_REG(cgrp_b_map, i, -1, 0x0);
    }
}

static void set_WCM_TCAM_CFG_REG
(
    mby_ppe_cgrp_b_map  * const cgrp_b_map,
    fm_uint i,
    fm_uint j,
    uint16 chunk_mask,
    uint8 start_compare,
    uint8 start_set,
    uint6 select_top,
    uint7 select0,
    uint7 select1,
    uint7 select2,
    uint7 select3
)
{
    wcm_tcam_cfg_r * const em_b_wcm_tcam_cfg = &(cgrp_b_map->WCM_TCAM_CFG[i][j]);
    em_b_wcm_tcam_cfg->CHUNK_MASK = chunk_mask;
    em_b_wcm_tcam_cfg->START_COMPARE = start_compare;
    em_b_wcm_tcam_cfg->START_SET = start_set;
    em_b_wcm_tcam_cfg->SELECT_TOP = select_top;
    em_b_wcm_tcam_cfg->SELECT0 = select0;
    em_b_wcm_tcam_cfg->SELECT1 = select1;
    em_b_wcm_tcam_cfg->SELECT2 = select2;
    em_b_wcm_tcam_cfg->SELECT3 = select3;
}

static void init_WCM_TCAM_CFG_REG
(
    mby_ppe_cgrp_b_map  * const cgrp_b_map
)
{
    for (fm_uint i = 0; i < MBY_FFU_TCAM_CFG_ENTRIES_1; i++)
    {
        for(fm_uint j = 0; j < 64; j++)
        {
            set_WCM_TCAM_CFG_REG(cgrp_b_map, i, j, 0x0, 0x1, 0x1, 0x0, 0x0, 0x0, 0x0, 0x0);
        }
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

static void init_keys(mbyClassifierKeys    * const keys)
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
    mby_ppe_cgrp_b_map  * const cgrp_b_map
)
{
    init_WCM_ACTION_REG(cgrp_b_map);
    init_WCM_TCAM_REG(cgrp_b_map);
    init_WCM_ACTION_CFG_REG(cgrp_b_map);
    init_WCM_TCAM_CFG_REG(cgrp_b_map);

}

static void initInputs
(
    mbyMapperToClassifier * const map2cla
)
{
    mbyClassifierActions * const actions_in        = &(map2cla->FFU_ACTIONS);
    mbyClassifierKeys    * const keys              = &(map2cla->FFU_KEYS);
    fm_byte              * const packet_profile_in = &(map2cla->FFU_PROFILE);

    init_actions(actions_in);
    init_keys(keys);
    *packet_profile_in = 0;
}

static void setInputs_ipv4_frame
(
    mbyMapperToClassifier * const map2cla
)
{
    /*
    set based on the SQA test

    Frame Header Data:
    00  12  34  56  78  90  00  07  08  09  0a  0b  08  00  45  00
    00  14  00  00  00  00  05  11  70  ca  aa  bb  cc  dd  11  aa
    bb  cc
    */
    map2cla->FFU_KEYS.key32[0] = 0xAABBCCDD;
    map2cla->FFU_KEYS.key32[1] = 0x11AABBCC;
    map2cla->FFU_KEYS.key32[4] = 0x11AABBCC;
    map2cla->FFU_KEYS.key16[6] = 0x12;
    map2cla->FFU_KEYS.key16[7] = 0x3456;
    map2cla->FFU_KEYS.key16[8] = 0x7890;
    map2cla->FFU_KEYS.key16[9] = 0x7;
    map2cla->FFU_KEYS.key16[10] = 0x809;
    map2cla->FFU_KEYS.key16[11] = 0xA0B;
    map2cla->FFU_KEYS.key16[12] = 0x800;
    map2cla->FFU_KEYS.key16[13] = 0x1;
    map2cla->FFU_KEYS.key16[14] = 0x1;
    map2cla->FFU_KEYS.key16[15] = 0x1;
    map2cla->FFU_KEYS.key16[16] = 0x1;
    map2cla->FFU_KEYS.key16[17] = 0x203;
    map2cla->FFU_KEYS.key16[19] = 0x203;
    map2cla->FFU_KEYS.key8[3] = 0x10;
    map2cla->FFU_KEYS.key8[6] = 0x4;
    map2cla->FFU_KEYS.key8[7] = 0x5;
    map2cla->FFU_KEYS.key8[20] = 0x5;
    map2cla->FFU_KEYS.key8[21] = 0x11;
    map2cla->FFU_KEYS.key8[23] = 0x14;
    map2cla->FFU_KEYS.key8[25] = 0x40;
    map2cla->FFU_KEYS.key8[28] = 0x1;
    map2cla->FFU_KEYS.key8[29] = 0x1;
    map2cla->FFU_KEYS.key8[45] = 0x1;
    map2cla->FFU_KEYS.key8[55] = 0xa1;
    map2cla->FFU_KEYS.key8[56] = 0x7;
    map2cla->FFU_KEYS.key8[57] = 0xf6;
    map2cla->FFU_KEYS.key8[58] = 0xe5;
    map2cla->FFU_KEYS.key8[59] = 0xd4;

    map2cla->FFU_ACTIONS.act4[4].val = 1;
    map2cla->FFU_ACTIONS.act1[20].val = 1;
    map2cla->FFU_ACTIONS.act1[22].val = 1;

    map2cla->FFU_PROFILE = 16;
}

static void simple_wcm_basic_test_setup
(
    mby_ppe_cgrp_b_map  * const cgrp_b_map,
    mbyMapperToClassifier * const map2cla
)
{
    initRegs(cgrp_b_map);
    initInputs(map2cla);
}

static int simple_wcm_basic_test_check
(
    fm_uint32 actions[MBY_WCM_MAX_ACTIONS_NUM]
)
{
    for (fm_uint i = 0; i < MBY_WCM_MAX_ACTIONS_NUM; i++) {
        if(actions[i] != 0x0)
            return 1;
    }

    return 0;
}

static void simple_wcm_dip_ipv4_test_setup
(
    mby_ppe_cgrp_b_map  * const cgrp_b_map,
    mbyMapperToClassifier * const map2cla
)
{
    /*
    set based on the SQA test

    tbl.acl_instance          = IES_ACL_NO_INSTANCE;
    tbl.location              = IES_ACL_FFU_GROUP_0;
    tbl.scenario              = IES_ACL_SCENARIO_INGRS_IPV4;
    tbl.condition             = IES_ACL_MATCH_DIP;
    tbl.mask.dip.addr[0]      = htonl(0xffffffff);
    tbl.mask.dip.is_ipv6      = false;
    tbl.min_entries           = 1;
    tbl.max_entries           = 1024;

    cond.val.dip.addr[0] = htonl(0x11223344);
    cond.val.dip.is_ipv6 = false;

    cond.mask.dip.addr[0] = htonl(0xffffffff);
    cond.mask.dip.is_ipv6 = false;

    rule.precedence = 512;
    rule.condition = tbl.condition;
    rule.cond = cond;
    rule.action = IES_ACL_ACTION_PERMIT;
    rule.act_val = act_val;
    rule.state = IES_ACL_RULE_ENTRY_STATE_VALID;
    */
    initRegs(cgrp_b_map);

    set_WCM_TCAM_REG(cgrp_b_map, 0, 512, 0x0, 0xee554433, 0x0, 0x11aabbcc);
    set_WCM_TCAM_REG(cgrp_b_map, 1, 512, 0x0, 0xee554433, 0x0, 0x11aabbcc);

    set_WCM_TCAM_CFG_REG(cgrp_b_map, 0, 16, 0xffff, 0x1, 0x1, 0x0, 0x64, 0x64, 0x64, 0x64);
    set_WCM_TCAM_CFG_REG(cgrp_b_map, 1, 16, 0xffff, 0x1, 0x1, 0x0, 0x64, 0x64, 0x64, 0x64);

    set_WCM_ACTION_REG(cgrp_b_map, 0, 512, 0, 0x640000c1);
    set_WCM_ACTION_REG(cgrp_b_map, 1, 512, 0, 0x640000c1);

    for (fm_uint i = 0; i < 63; i++) {
        set_index_WCM_ACTION_REG(cgrp_b_map, i, 0, 0x1);
        set_enable_WCM_ACTION_REG(cgrp_b_map, i, 0, 0x1);
    }

    initInputs(map2cla);
    setInputs_ipv4_frame(map2cla);
}

static int simple_wcm_dip_ipv4_test_check
(
    fm_uint32 actions[MBY_WCM_MAX_ACTIONS_NUM]
)
{
    for (fm_uint i = 0; i < MBY_WCM_MAX_ACTIONS_NUM; i++) {
        if (actions[i] != 0x0 && i != 0)
            return 1;
        // act24[4].prec = 3, act24[4].val = 259 -> FWD
        if (actions[i] != 0x640000c1 && i == 0)
            return 1;
    }

    return 0;
}

static void simple_wcm_mac_test_setup
(
    mby_ppe_cgrp_b_map  * const cgrp_b_map,
    mbyMapperToClassifier * const map2cla
)
{
    /*
    set based on the SQA test

    tbl.acl_instance          = IES_ACL_NO_INSTANCE;
    tbl.location              = IES_ACL_FFU_GROUP_0;
    tbl.scenario              = IES_ACL_SCENARIO_INGRS_IPV4;
    tbl.condition             = IES_ACL_MATCH_DMAC;
	tbl.mask.dmac             = htonl(0xffffffffffff);
    tbl.min_entries           = 1;
    tbl.max_entries           = 1024;

    cond.val.dmac = htonl(0x001234567890);
    cond.mask.dmac = htonl(0xffffffffffff);

    rule.precedence = 512;
    rule.condition = tbl.condition;
    rule.cond = cond;
    rule.action = IES_LITERAL_U64(2);
    rule.act_val = act_val;
    rule.state = IES_ACL_RULE_ENTRY_STATE_VALID;
    */
    initRegs(cgrp_b_map);

    set_WCM_TCAM_REG(cgrp_b_map, 0, 512, 0x0, 0x90785634, 0x0, 0x6f87a9cb);
    set_WCM_TCAM_REG(cgrp_b_map, 1, 512, 0x0, 0x0, 0x0, 0x0);

    set_WCM_TCAM_CFG_REG(cgrp_b_map, 0, 16, 0xffff, 0x1, 0x1, 0x0, 0x8, 0x8, 0x7, 0x7);
    set_WCM_TCAM_CFG_REG(cgrp_b_map, 1, 16, 0xffff, 0x1, 0x1, 0x0, 0x8, 0x8, 0x7, 0x7);

    set_WCM_ACTION_REG(cgrp_b_map, 0, 512, 0, 0x640000c1);
    set_WCM_ACTION_REG(cgrp_b_map, 1, 512, 0, 0x74000103);

    for (fm_uint i = 0; i < 63; i++) {
        set_index_WCM_ACTION_REG(cgrp_b_map, i, 1, 0x1);
        set_enable_WCM_ACTION_REG(cgrp_b_map, i, 1, 0x1);
    }

    initInputs(map2cla);
    setInputs_ipv4_frame(map2cla);
}

static int simple_wcm_mac_test_check
(
    fm_uint32 actions[MBY_WCM_MAX_ACTIONS_NUM]
)
{
    for (fm_uint i = 0; i < MBY_WCM_MAX_ACTIONS_NUM; i++) {
        if (actions[i] != 0x0 && i != 2)
            return 1;
        // act1[1].prec = 3, act1[1].val = 1 -> TRAP
        if (actions[i] != 0x74000103 && i == 2)
            return 1;
    }

    return 0;
}

static int run_on_simple_wcm
(
    run_on_simple_wcm_setup_fn setup,
    run_on_simple_wcm_check_fn check
)
{

    mby_ppe_cgrp_b_map   *cgrp_b_map = NULL;
    allocMem(&cgrp_b_map);
    initRegs(cgrp_b_map);


    mbyMapperToClassifier map2cla = { 0 };
  
    mbyMapperToClassifier const * const in  = &map2cla;
    mbyClassifierActions * const actions_out = &(map2cla.FFU_ACTIONS);
    setup(cgrp_b_map, &map2cla);
    fm_uint32 actions[MBY_WCM_MAX_ACTIONS_NUM] = { 0 };

    mbyMatchWildcard
    (
        cgrp_b_map,
        &(in->FFU_KEYS),
        in->FFU_PROFILE,
        0,
        actions
    );

    int ret = check(actions);

    // Free up memory:
    freeMem(cgrp_b_map);

    return ret;
}


int main(void)
{
    printf("--------------------------------------------------------------------------------\n");

    fm_uint tests = 0;
    fm_uint fails = 0;
    // default values
    SIMPLE_WCM_TEST(basic,                fails); tests++;
    // case with 1 slace cascade
    SIMPLE_WCM_TEST(dip_ipv4,             fails); tests++;
    // case with 2 slace cascade
    SIMPLE_WCM_TEST(mac,                  fails); tests++;

    fm_uint passes = (tests > fails) ? tests - fails : 0;

    printf("--------------------------------------------------------------------------------\n");

    if (fails == 0)
        printf(COLOR_GREEN "[pass]");
    else
        printf(COLOR_RED   "[FAIL]");

    printf(" %2d/%2d - WCM tests\n" COLOR_RESET, passes, tests);

    printf("--------------------------------------------------------------------------------\n");

    int rv = (fails == 0) ? 0 : -1;

    return rv;
}
