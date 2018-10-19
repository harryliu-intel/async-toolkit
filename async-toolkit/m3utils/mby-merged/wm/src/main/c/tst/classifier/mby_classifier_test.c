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

void initRegs
(
    mby_ppe_cgrp_a_map  * const cgrp_a_map,
    mby_ppe_cgrp_b_map  * const cgrp_b_map,
    mby_ppe_entropy_map * const entropy_map
)
{
    // <initialize registers here>

    // --------------------------------------------------------------------------------
    // EM_B_HASH_CAM
    // --------------------------------------------------------------------------------
    fm_uint32 entry = 0;
    fm_uint32 word  = 0;
    fm_uint64 data  = 0xDeadBeefBaadC0deuLL;

    em_b_hash_cam_r * const em_b_hash_cam = &(cgrp_b_map->EM_B_HASH_CAM[entry][word]);
    em_b_hash_cam->DATA = data;
}

void initDefaultInputs
(
    mbyMapperToClassifier * const map2cla
)
{
    mbyClassifierActions * const actions_in  = &(map2cla->FFU_ACTIONS);
    mbyClassifierKeys    * const keys        = &(map2cla->FFU_KEYS);
    fm_byte              * const scenario_in = &(map2cla->FFU_SCENARIO);
    fm_bool              * const ip_option   =   map2cla->IP_OPTION;
    mbyParserInfo        * const parser_info = &(map2cla->PARSER_INFO);
    fm_byte              * const pri_profile = &(map2cla->PRIORITY_PROFILE);

    // mbyClassifierActions:
    for (fm_uint i = 0; i < MBY_FFU_ACT24; i++) {
        actions_in->act24[i].prec = 0;
        actions_in->act24[i].val  = 0;
    }
    for (fm_uint i = 0; i < MBY_FFU_ACT4; i++) {
        actions_in->act4[i].prec  = 0;
        actions_in->act4[i].val   = 0;
    }
    for (fm_uint i = 0; i < MBY_FFU_ACT1; i++) {
        actions_in->act1[i].prec  = 0;
        actions_in->act1[i].val   = 0;
    }

    // mbyClassifierKeys:
    for (fm_uint i = 0; i < MBY_FFU_KEY32; i++)
        keys->key32[i] = 0;
    for (fm_uint i = 0; i < MBY_FFU_KEY16; i++)
        keys->key16[i] = 0;
    for (fm_uint i = 0; i < MBY_FFU_KEY8;  i++)
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
    fm_byte              * const scenario_in = &(map2cla->FFU_SCENARIO);
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
        printf("Unsupported test scenario -- exiting!\n");
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
    fm_byte              * const scenario_in = &(in->FFU_SCENARIO);
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
    test_status = testWildCardMatch(cgrp_a_map, cgrp_b_map, entropy_map, shm_map, &map2cla, &cla2hsh);
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
