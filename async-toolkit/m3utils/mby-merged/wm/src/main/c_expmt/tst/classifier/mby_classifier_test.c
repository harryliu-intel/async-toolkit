#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#ifdef USE_NEW_CSRS
#include <mby_top_map.h>
#endif

#include <mby_common.h>
#include <mby_pipeline.h>

#define COLOR_RED     "\x1b[31m"
#define COLOR_GREEN   "\x1b[32m"
#define COLOR_RESET   "\x1b[0m"

typedef struct mbyTestResultsStruct
{
    fm_uint32 num_tests;
    fm_uint32 num_passed;
    fm_uint32 num_failed;

} mbyTestResults;

void initRegs
(
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_a_map  cgrp_a_map,
    mby_ppe_cgrp_b_map  cgrp_b_map,
    mby_ppe_entropy_map entropy_map
#else
    fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE]
#endif
)
{
    // <initialize registers here>

    // --------------------------------------------------------------------------------
    // EM_B_HASH_CAM
    // --------------------------------------------------------------------------------
    fm_uint32 entry = 0;
    fm_uint32 word  = 0;
    fm_uint64 data  = 0xDeadBeefBaadC0deuLL;

#ifdef USE_NEW_CSRS
    em_b_hash_cam_r * const em_b_hash_cam = &(cgrp_b_map.EM_B_HASH_CAM[entry][word]);
    em_b_hash_cam->DATA = data;
#else
    fm_byte const group = 1; // Exact Match "B"
    fm_uint64 em_b_hash_cam_reg = data;
    mbyModelWriteCSR64(regs, MBY_FFU_HASH_CAM(group, entry, word, 0), em_b_hash_cam_reg);
    fm_uint64 em_b_hash_cam_tmp = 0;
    mbyModelReadCSR64(regs, MBY_FFU_HASH_CAM(group, entry, word, 0), &em_b_hash_cam_tmp);
    assert(em_b_hash_cam_tmp == em_b_hash_cam_reg);
#endif
}

void initInputs
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

    // <initialize inputs here>

}

mbyTestResults checkOutputs
(
    mbyClassifierToHash * const cla2hsh
)
{
    mbyTestResults test_results = { 0 };

    test_results.num_tests  = 1;
    test_results.num_passed = 0;
    test_results.num_failed = test_results.num_tests - test_results.num_passed;

    return test_results;
}

int main (void)
{
#ifdef USE_NEW_CSRS
    mby_ppe_cgrp_a_map  cgrp_a_map;
    mby_ppe_cgrp_b_map  cgrp_b_map;
    mby_ppe_entropy_map entropy_map;
#else
    // Allocate storage for registers on the heap:
    fm_uint32 *regs = malloc(MBY_REGISTER_ARRAY_SIZE * sizeof(fm_uint32));
    if (regs == NULL) {
        printf("Could not allocate heap memory for register buffer -- exiting!\n");
        exit(-1);
    }
#endif

    mbyMapperToClassifier map2cla = { 0 };
    mbyClassifierToHash   cla2hsh = { 0 };

    initInputs(&map2cla);

    mbyMapperToClassifier const * const in  = &map2cla;
    mbyClassifierToHash         * const out = &cla2hsh;

    Classifier
    (
#ifdef USE_NEW_CSRS
        &cgrp_a_map,
        &cgrp_b_map,
        &entropy_map,
#else
        regs,
#endif
        in,
        out
   );

    mbyTestResults test_results = checkOutputs(&cla2hsh);

    printf("--------------------------------------------------------------------------------\n");

    fm_bool tests_passed = (test_results.num_passed == test_results.num_tests);

    if (tests_passed)
        printf(COLOR_GREEN "[pass]");
    else
        printf(COLOR_RED   "[FAIL]");

    printf("  %3d/%3d - Classifier tests\n"
           COLOR_RESET, test_results.num_passed, test_results.num_tests);

    printf("--------------------------------------------------------------------------------\n");

    // Free up memory:
#ifndef USE_NEW_CSRS
    free(regs);
#endif

    int rv = (tests_passed) ? 0 : -1;

    return rv;
}
