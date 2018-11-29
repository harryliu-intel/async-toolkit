#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include <string.h>
#include <assert.h>

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
//typedef int(*run_on_simple_wcm_check_fn)(fm_uint32 actions[MBY_WCM_MAX_ACTIONS_NUM]);

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

static int run_on_simple_wcm
(
    run_on_simple_wcm_setup_fn setup
    //run_on_simple_wcm_check_fn check
)
{
    /*mby_ppe_fwd_misc_map   *fwd_misc_map = NULL;
    mby_ppe_trig_apply_map *trig_apply_map = NULL;
    mby_ppe_trig_apply_misc_map *trig_apply_misc_map = NULL;
    mby_ppe_trig_usage_map *trig_usage_map = NULL;

    allocMem(&fwd_misc_map);
    allocMem(&trig_apply_map);
    allocMem(&trig_apply_misc_map);
    allocMem(&trig_usage_map);*/

    //initRegs(cgrp_b_map);

    mbyMaskGenToTriggers gen2trig = { 0 };
    mbyMaskGenToTriggers const * const in = &gen2trig;
    mbyTriggersToCongMgmt * const out;
    //mbyMapperToClassifier map2cla = { 0 };
  
    //mbyMapperToClassifier const * const in  = &map2cla;
    //mbyClassifierActions * const actions_out = &(map2cla.FFU_ACTIONS);
    //setup(cgrp_b_map, &map2cla);
    ///fm_uint32 actions[MBY_WCM_MAX_ACTIONS_NUM] = { 0 };
 
    /*mbyTriggers
    (
        fwd_misc_map,
        MBY_TRG_IN_REGS_P,
        in,
        out
    );*/

    //int ret = check(actions);

    // Free up memory:
    /*freeMem(fwd_misc_map);
    freeMem(trig_apply_map);
    freeMem(trig_apply_misc_map);
    freeMem(trig_usage_map);*/

    return 0;
}


int main(void)
{
    printf("--------------------------------------------------------------------------------\n");

    fm_uint tests = 0;
    fm_uint fails = 0;
    // default values
    //SIMPLE_TRIGGERS_TEST(basic,                fails); tests++;

    fm_uint passes = (tests > fails) ? tests - fails : 0;

    printf("--------------------------------------------------------------------------------\n");

    if (fails == 0)
        printf(COLOR_GREEN "[pass]");
    else
        printf(COLOR_RED   "[FAIL]");

    printf(" %2d/%2d - Triggers tests\n" COLOR_RESET, passes, tests);

    printf("--------------------------------------------------------------------------------\n");

    int rv = (fails == 0) ? 0 : -1;

    return rv;
}