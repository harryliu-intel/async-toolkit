#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include <string.h>
#include <assert.h>

#include <mby_top_map.h>

#include <mby_common.h>
#include <mby_pipeline.h>

#define MBY_PHYSICAL_SOURCE_PORTS         17

//An example of input for the triggers (output from maskgen) for TC = 0 and egress_vlan_id = 7
#define MASK_GEN_ACTION                   0x1
#define MASK_GEN_AMASK                    0x000800000000
#define MASK_GEN_GLORT_DMASK              0x1fffff
#define MASK_GEN_IDGLORT                  0x0400
#define MASK_GEN_IP_MCAST_IDX             0x1
#define MASK_GEN_IS_IPV4                  0x1
#define MASK_GEN_L2_DMAC                  9988776655
#define MASK_GEN_L2_EVID1                 0x7
#define MASK_GEN_L2_SMAC                  101010101
#define MASK_GEN_LEARNING_ENABLED         0x1
#define MASK_GEN_MARK_ROUTED              0x1
#define MASK_GEN_PRE_RESOLVE_ACTION       0x1
#define MASK_GEN_PRE_RESOLVE_DGLORT       0x400
#define MASK_GEN_DMASK_0                  0x4
#define MASK_GEN_PRE_RESOLVE_DMASK_0      0x4
#define MASK_GEN_RX_LENGTH                128
#define MASK_GEN_RX_PORT                  0x1
#define MASK_GEN_STORE_TRAP_ACTION        0x1
#define MASK_GEN_STRICT_GLORT_ROUTING     0x1
#define MASK_GEN_L2_IVLAN1_CNT            0x3
#define MASK_GEN_MOD_IDX                  0x1
#define MASK_GEN_PARSER_INFO_OTR_L2_LEN   0x2
#define MASK_GEN_PARSER_INFO_OTR_L2_VLAN1 0x1
#define MASK_GEN_PARSER_INFO_OTR_L3_LEN   0x5
#define MASK_GEN_TAIL_CSUM_LEN            0x800000

#define TRIG_NEW_VLAN                     0x2
#define TRIG_NEW_TC                       0x7

#define COLOR_RED     "\x1b[31m"
#define COLOR_GREEN   "\x1b[32m"
#define COLOR_RESET   "\x1b[0m"


#define SIMPLE_TRIGGER_TEST(name, fails) {if (!runOnSimpleTrigger(simpleTrigger ## name ## TestSetup, \
                simpleTrigger ## name ## TestCheck)) pass(#name); else {++fails; fail(#name);} }

typedef void(*run_on_simple_trigger_setup_fn)
(
    mby_ppe_trig_apply_map      * const trig_apply_map,
    mby_ppe_trig_apply_misc_map * const trig_apply_misc_map,
    mby_ppe_fwd_misc_map        * const fwd_misc_map,
    mbyMaskGenToTriggers        * const gen2trig
);
typedef int(*run_on_simple_trigger_check_fn)
(
    mbyMaskGenToTriggers  const * const in,
    mbyTriggersToCongMgmt const * const out
);

static void pass(const char* name)
{
    printf(COLOR_GREEN "[pass]" COLOR_RESET " %s\n", name);
}

static void fail(const char* name)
{
    printf(COLOR_RED   "[FAIL]" COLOR_RESET " %s\n", name );
}

static void setMbyTrigCondCfg
(
    mby_ppe_trig_apply_map * const trig_apply_map,
    fm_byte                  const trig,
    fm_byte                  const mtch_learn,
    fm_byte                  const mtch_vlan,
    fm_byte                  const mtch_cgrp,
    fm_byte                  const mtch_tc,
    fm_byte                  const mtch_dglort,
    fm_byte                  const mtch_edomain,
    fm_byte                  const mtch_precedence,
    fm_byte                  const mtch_rand_num,
    fm_byte                  const mtch_rand_if_less,
    fm_byte                  const mtch_rand_threshold,
    fm_byte                  const mtch_tx
)
{
    trigger_condition_cfg_r * const trigger_condition_cfg = &(trig_apply_map->TRIGGER_CONDITION_CFG[trig]);

    trigger_condition_cfg->LEARN                  = mtch_learn;
    trigger_condition_cfg->MATCH_VLAN             = mtch_vlan;
    trigger_condition_cfg->MATCH_CGRP             = mtch_cgrp;
    trigger_condition_cfg->MATCH_TC               = mtch_tc;
    trigger_condition_cfg->MATCH_DEST_GLORT       = mtch_dglort;
    trigger_condition_cfg->MATCH_EGRESS_DOMAIN    = mtch_edomain;
    trigger_condition_cfg->MATCH_BY_PRECEDENCE    = mtch_precedence;
    trigger_condition_cfg->MATCH_RANDOM_NUMBER    = mtch_rand_num;
    trigger_condition_cfg->MATCH_RANDOM_IF_LESS   = mtch_rand_if_less;
    trigger_condition_cfg->MATCH_RANDOM_THRESHOLD = mtch_rand_threshold;
    trigger_condition_cfg->MATCH_TX               = mtch_tx;
}

static void setMbyTrigCondParam
(
    mby_ppe_trig_apply_map * const trig_apply_map,
    fm_byte                  const trig,
    fm_byte                  const vid_id,
    fm_byte                  const tc,
    fm_byte                  const fra_cla_mask,
    fm_byte                  const routed_mask,
    fm_uint16                const edomain_value,
    fm_uint16                const edomain_mask
)
{
    trigger_condition_param_r * const trigger_condition_param = &(trig_apply_map->TRIGGER_CONDITION_PARAM[trig]);

    trigger_condition_param->VID_ID              = vid_id;
    trigger_condition_param->TC                  = tc;
    trigger_condition_param->FRAME_CLASS_MASK    = fra_cla_mask;
    trigger_condition_param->ROUTED_MASK         = routed_mask;
    trigger_condition_param->EGRESS_DOMAIN_VALUE = edomain_value;
    trigger_condition_param->EGRESS_DOMAIN_MASK  = edomain_mask;
}

static void setMbyTrigCondCgrp
(
    mby_ppe_trig_apply_map * const trig_apply_map,
    fm_byte                  const trig,
    fm_byte                  const cgrp_id,
    fm_byte                  const cgrp_mask
)
{
    trigger_condition_cgrp_r * const trigger_condition_cgrp = &(trig_apply_map->TRIGGER_CONDITION_CGRP[trig]);

    trigger_condition_cgrp->CGRP_ID   = cgrp_id;
    trigger_condition_cgrp->CGRP_MASK = cgrp_mask;
}

static void setMbyTrigCondGlort
(
    mby_ppe_trig_apply_map * const trig_apply_map,
    fm_byte                  const trig,
    fm_uint16                const dest_glort,
    fm_uint16                const glort_mask
)
{
    trigger_condition_glort_r * const trigger_condition_glort = &(trig_apply_map->TRIGGER_CONDITION_GLORT[trig]);

    trigger_condition_glort->DEST_GLORT = dest_glort;
    trigger_condition_glort->GLORT_MASK = glort_mask;
}

static void setMbyTrigCondRx
(
    mby_ppe_trig_apply_map * const trig_apply_map,
    fm_byte                  const trig,
    fm_uint32                const src_port_mask
)
{
    trigger_condition_rx_r * const trigger_condition_rx = &(trig_apply_map->TRIGGER_CONDITION_RX[trig]);

    trigger_condition_rx->SRC_PORT_MASK = src_port_mask;
}

static void setMbyTrigCondAmask1
(
    mby_ppe_trig_apply_map * const trig_apply_map,
    fm_byte                  const trig,
    fm_uint32                const han_act_mask
)
{
    trigger_condition_amask_1_r * const trigger_condition_amask_1 = &(trig_apply_map->TRIGGER_CONDITION_AMASK_1[trig]);

    trigger_condition_amask_1->HANDLER_ACTION_MASK = han_act_mask;
}

static void setMbyTrigCondAmask2
(
    mby_ppe_trig_apply_map * const trig_apply_map,
    fm_byte                  const trig,
    fm_uint16                const han_act_mask
)
{
    trigger_condition_amask_2_r * const trigger_condition_amask_2 = &(trig_apply_map->TRIGGER_CONDITION_AMASK_2[trig]);

    trigger_condition_amask_2->HANDLER_ACTION_MASK = han_act_mask;
}

static void setMbyTrigDirMapCtx
(
    mby_ppe_trig_apply_map * const trig_apply_map,
    fm_uint64                const dest_port_mask_0,
    fm_uint64                const dest_port_mask_1,
    fm_uint64                const dest_port_mask_2,
    fm_uint64                const dest_port_mask_3,
    fm_uint64                const dest_port_mask_4
)
{
    trig_apply_map->TRIGGER_DIRECT_MAP_CTX0.DEST_PORT_MASK = dest_port_mask_0;
    trig_apply_map->TRIGGER_DIRECT_MAP_CTX1.DEST_PORT_MASK = dest_port_mask_1;
    trig_apply_map->TRIGGER_DIRECT_MAP_CTX2.DEST_PORT_MASK = dest_port_mask_2;
    trig_apply_map->TRIGGER_DIRECT_MAP_CTX3.DEST_PORT_MASK = dest_port_mask_3;
    trig_apply_map->TRIGGER_DIRECT_MAP_CTX4.DEST_PORT_MASK = dest_port_mask_4;
}

static void setMbyTrigDirMapAdm
(
    mby_ppe_trig_apply_map * const trig_apply_map,
    fm_uint64                const new_dest_mask_0,
    fm_uint64                const new_dest_mask_1,
    fm_uint64                const new_dest_mask_2,
    fm_uint64                const new_dest_mask_3,
    fm_byte                  const new_dest_mask_4,
    fm_byte                  const filter_dest_mask
)
{
    trig_apply_map->TRIGGER_DIRECT_MAP_ADM0.NEW_DEST_MASK    = new_dest_mask_0;
    trig_apply_map->TRIGGER_DIRECT_MAP_ADM1.NEW_DEST_MASK    = new_dest_mask_1;
    trig_apply_map->TRIGGER_DIRECT_MAP_ADM2.NEW_DEST_MASK    = new_dest_mask_2;
    trig_apply_map->TRIGGER_DIRECT_MAP_ADM3.NEW_DEST_MASK    = new_dest_mask_3;
    trig_apply_map->TRIGGER_DIRECT_MAP_ADM4.NEW_DEST_MASK    = new_dest_mask_4;
    trig_apply_map->TRIGGER_DIRECT_MAP_ADM4.FILTER_DEST_MASK = filter_dest_mask;
}

static void setMbyTrigDirMapAdr
(
    mby_ppe_trig_apply_map * const trig_apply_map,
    fm_uint64                const drop_mask_0,
    fm_uint64                const drop_mask_1,
    fm_uint64                const drop_mask_2,
    fm_uint64                const drop_mask_3,
    fm_byte                  const drop_mask_4
)
{
    trig_apply_map->TRIGGER_DIRECT_MAP_ADR0.DROP_MASK = drop_mask_0;
    trig_apply_map->TRIGGER_DIRECT_MAP_ADR1.DROP_MASK = drop_mask_1;
    trig_apply_map->TRIGGER_DIRECT_MAP_ADR2.DROP_MASK = drop_mask_2;
    trig_apply_map->TRIGGER_DIRECT_MAP_ADR3.DROP_MASK = drop_mask_3;
    trig_apply_map->TRIGGER_DIRECT_MAP_ADR4.DROP_MASK = drop_mask_4;
}

static void setMbyTrigAction
(
    mby_ppe_trig_apply_map       * const trig_apply_map,
    fm_byte                        const trig,
    mbyTriggerActions      const * const trig_actions
)
{
    trig_apply_map->TRIGGER_ACTION_CFG_1[trig].FORWARDING_ACTION       = trig_actions->forwardingAction;
    trig_apply_map->TRIGGER_ACTION_CFG_1[trig].TRAP_ACTION             = trig_actions->trapAction;
    trig_apply_map->TRIGGER_ACTION_CFG_1[trig].TC_ACTION               = trig_actions->TCAction;
    trig_apply_map->TRIGGER_ACTION_CFG_1[trig].VLAN_ACTION             = trig_actions->vlanAction;
    trig_apply_map->TRIGGER_ACTION_CFG_1[trig].LEARNING_ACTION         = trig_actions->learningAction;
    trig_apply_map->TRIGGER_ACTION_CFG_1[trig].RATE_LIMIT_ACTION       = trig_actions->rateLimitAction;
    trig_apply_map->TRIGGER_ACTION_CFG_1[trig].EGRESS_L2_DOMAIN_ACTION = trig_actions->egressL2DomainAction;
    trig_apply_map->TRIGGER_ACTION_CFG_1[trig].EGRESS_L3_DOMAIN_ACTION = trig_actions->egressL3DomainAction;
    trig_apply_map->TRIGGER_ACTION_CFG_1[trig].POLICER_ACTION          = trig_actions->policerAction;
    trig_apply_map->TRIGGER_ACTION_CFG_1[trig].MIRRORING_ACTION0       = trig_actions->mirroringAction0;
    trig_apply_map->TRIGGER_ACTION_CFG_1[trig].MIRRORING_ACTION1       = trig_actions->mirroringAction1;
    trig_apply_map->TRIGGER_ACTION_CFG_1[trig].MIRRORING_ACTION2       = trig_actions->mirroringAction2;
    trig_apply_map->TRIGGER_ACTION_CFG_1[trig].MIRRORING_ACTION3       = trig_actions->mirroringAction3;

    trig_apply_map->TRIGGER_ACTION_CFG_2[trig].NEW_TC                  = trig_actions->newTC;
    trig_apply_map->TRIGGER_ACTION_CFG_2[trig].NEW_EVID                = trig_actions->newVlan;
    trig_apply_map->TRIGGER_ACTION_CFG_2[trig].RATE_LIMIT_NUM          = trig_actions->newRateLimitNum;
    trig_apply_map->TRIGGER_ACTION_CFG_2[trig].TRAP_CODE               = trig_actions->trapCode;

    trig_apply_map->TRIGGER_ACTION_GLORT[trig].NEW_DEST_GLORT          = trig_actions->newDestGlort;
    trig_apply_map->TRIGGER_ACTION_GLORT[trig].NEW_DEST_GLORT_MASK     = trig_actions->newDestGlortMask;
    trig_apply_map->TRIGGER_ACTION_MIRROR[trig].MIRROR_PROFILE_INDEX0  = trig_actions->mirroringAction0;
    trig_apply_map->TRIGGER_ACTION_MIRROR[trig].MIRROR_PROFILE_INDEX1  = trig_actions->mirroringAction1;
    trig_apply_map->TRIGGER_ACTION_MIRROR[trig].MIRROR_PROFILE_INDEX2  = trig_actions->mirroringAction2;
    trig_apply_map->TRIGGER_ACTION_MIRROR[trig].MIRROR_PROFILE_INDEX3  = trig_actions->mirroringAction3;
}

static void initMbyTrigRegisters
(
    mby_ppe_trig_apply_map      * const trig_apply_map,
    mby_ppe_trig_apply_misc_map * const trig_apply_misc_map,
    mby_ppe_fwd_misc_map        * const fwd_misc_map
)
{
    mbyTriggerActions trig_actions = { 0 };

    for (fm_uint i = 0; i < MBY_TRIGGERS_COUNT; i++)
    {
        setMbyTrigCondCfg(trig_apply_map, i, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0);
        setMbyTrigCondParam(trig_apply_map, i, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0);
        setMbyTrigCondCgrp(trig_apply_map, i, 0x0, 0x0);
        setMbyTrigCondGlort(trig_apply_map, i, 0x0, 0x0);
        setMbyTrigCondRx(trig_apply_map, i, 0x0);
        setMbyTrigCondAmask1(trig_apply_map, i, 0x0);
        setMbyTrigCondAmask2(trig_apply_map, i, 0x0);
        setMbyTrigDirMapCtx(trig_apply_map, i, 0x0, 0x0, 0x0, 0x0);
        setMbyTrigDirMapAdm(trig_apply_map, i, 0x0, 0x0, 0x0, 0x0, 0x0);
        setMbyTrigDirMapAdr(trig_apply_map, i, 0x0, 0x0, 0x0, 0x0);
        setMbyTrigAction(trig_apply_map, i, &trig_actions);
    }

    trig_apply_misc_map->TRIGGER_IM[0].MASK           = 0xffffffffffff;
    trig_apply_misc_map->TRIGGER_IM[1].MASK           = 0xffffffffffff;
    trig_apply_misc_map->TRIGGER_RATE_LIM_EMPTY.EMPTY = 0xffff;
    trig_apply_misc_map->MA_TCN_IM.PENDING_EVENTS     = 0x1;
    trig_apply_misc_map->MA_TCN_IM.TCN_OVERFLOW       = 0x1;
    for (fm_uint i = 0; i < MBY_PHYSICAL_SOURCE_PORTS; i++)
        trig_apply_misc_map->MA_TCN_WM[i].WM          = 0x1e;

    fwd_misc_map->FWD_IM.TRIGGER = 0x1;
}

static void initMbyTrigInputs
(
    mbyMaskGenToTriggers * const msg2trig
)
{
    msg2trig->ACTION                   = MASK_GEN_ACTION;
    msg2trig->AMASK                    = MASK_GEN_AMASK;
    msg2trig->GLORT_DMASK              = MASK_GEN_GLORT_DMASK;
    msg2trig->IDGLORT                  = MASK_GEN_IDGLORT;
    msg2trig->IP_MCAST_IDX             = MASK_GEN_IP_MCAST_IDX;
    msg2trig->IS_IPV4                  = MASK_GEN_IS_IPV4;
    msg2trig->L2_DMAC                  = MASK_GEN_L2_DMAC;
    msg2trig->L2_EVID1                 = MASK_GEN_L2_EVID1;
    msg2trig->L2_SMAC                  = MASK_GEN_L2_SMAC;
    msg2trig->LEARNING_ENABLED         = MASK_GEN_LEARNING_ENABLED;
    msg2trig->MARK_ROUTED              = MASK_GEN_MARK_ROUTED;
    msg2trig->PRE_RESOLVE_ACTION       = MASK_GEN_PRE_RESOLVE_ACTION;
    msg2trig->PRE_RESOLVE_DGLORT       = MASK_GEN_PRE_RESOLVE_DGLORT;
    msg2trig->DMASK[0]                 = MASK_GEN_DMASK_0;
    msg2trig->PRE_RESOLVE_DMASK[0]     = MASK_GEN_PRE_RESOLVE_DMASK_0;
    msg2trig->RX_LENGTH                = MASK_GEN_RX_LENGTH;
    msg2trig->RX_PORT                  = MASK_GEN_RX_PORT;
    msg2trig->STORE_TRAP_ACTION        = MASK_GEN_STORE_TRAP_ACTION;
    msg2trig->STRICT_GLORT_ROUTING     = MASK_GEN_STRICT_GLORT_ROUTING;
    msg2trig->L2_IVLAN1_CNT            = MASK_GEN_L2_IVLAN1_CNT;
    msg2trig->MOD_IDX                  = MASK_GEN_MOD_IDX;
    msg2trig->PARSER_INFO.otr_l2_len   = MASK_GEN_PARSER_INFO_OTR_L2_LEN;
    msg2trig->PARSER_INFO.otr_l2_vlan1 = MASK_GEN_PARSER_INFO_OTR_L2_VLAN1;
    msg2trig->PARSER_INFO.otr_l3_len   = MASK_GEN_PARSER_INFO_OTR_L3_LEN;
    msg2trig->TAIL_CSUM_LEN            = MASK_GEN_TAIL_CSUM_LEN;
}

static void simpleTriggerBasicTestSetup
(
    mby_ppe_trig_apply_map      * const trig_apply_map,
    mby_ppe_trig_apply_misc_map * const trig_apply_misc_map,
    mby_ppe_fwd_misc_map        * const fwd_misc_map,
    mbyMaskGenToTriggers        * const gen2trig
)
{
    initMbyTrigRegisters
    (
        trig_apply_map,
        trig_apply_misc_map,
        fwd_misc_map
    );
    initMbyTrigInputs(gen2trig);
}

static int simpleTriggerBasicTestCheck
(
    mbyMaskGenToTriggers  const * const in,
    mbyTriggersToCongMgmt const * const out
)
{
    if(in->ACTION != out->ACTION || in->IDGLORT != out->IDGLORT || in->L2_EVID1 != out->L2_EVID1 ||
       in->MIRROR0_PROFILE_IDX != out->MIRROR0_PROFILE_IDX || in->MIRROR0_PROFILE_V != out->MIRROR0_PROFILE_V ||
       in->MIRROR1_PROFILE_IDX != out->MIRROR1_PROFILE_IDX || in->MIRROR1_PROFILE_V != out->MIRROR1_PROFILE_V ||
       in->QOS_TC != out->QOS_TC)
        return 1;

    for (fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
    {
        if(in->DMASK[i] != out->DMASK[i])
            return 1;
    }

    return 0;
}

static void simpleTriggerMatchOnVlanTCTestSetup
(
    mby_ppe_trig_apply_map      * const trig_apply_map,
    mby_ppe_trig_apply_misc_map * const trig_apply_misc_map,
    mby_ppe_fwd_misc_map        * const fwd_misc_map,
    mbyMaskGenToTriggers        * const gen2trig
)
{
    mbyTriggerActions trig_actions = { 0 };

    initMbyTrigRegisters
    (
        trig_apply_map,
        trig_apply_misc_map,
        fwd_misc_map
    );

    for (fm_uint i = 0; i < MBY_TRIGGERS_COUNT; i++)
    {
        setMbyTrigCondCfg(trig_apply_map, i, 0x1, 0x2, 0x2, 0x2, 0x2, 0x2, 0x0, 0x0, 0x0, 0x12, 0x0);
        setMbyTrigCondParam(trig_apply_map, i, 0x0, 0x0, 0x7, 0x3, 0x0, 0x0);
    }

    setMbyTrigCondCgrp(trig_apply_map, 2, 0x1, 0x1);
    setMbyTrigCondCgrp(trig_apply_map, 5, 0x8, 0x0);
    setMbyTrigCondCgrp(trig_apply_map, 7, 0x2, 0x2);
    setMbyTrigCondCgrp(trig_apply_map, 8, 0x2, 0x2);
    setMbyTrigCondCgrp(trig_apply_map, 9, 0x4, 0x4);
    setMbyTrigCondCgrp(trig_apply_map, 10, 0x4, 0x4);
    setMbyTrigCondCgrp(trig_apply_map, 11, 0x6e, 0x5c);

    setMbyTrigCondGlort(trig_apply_map, 0, 0x8da1, 0x0);
    setMbyTrigCondGlort(trig_apply_map, 1, 0x8da1, 0x0);
    setMbyTrigCondGlort(trig_apply_map, 5, 0x1000, 0xf800);
    setMbyTrigCondGlort(trig_apply_map, 7, 0x1000, 0xf800);
    setMbyTrigCondGlort(trig_apply_map, 8, 0x1000, 0xf800);
    setMbyTrigCondGlort(trig_apply_map, 9, 0x1000, 0xf800);
    setMbyTrigCondGlort(trig_apply_map, 10, 0x1000, 0xf800);
    setMbyTrigCondGlort(trig_apply_map, 11, 0x81ff, 0x0);

    setMbyTrigCondRx(trig_apply_map, 5, 0x7FFFFF);
    setMbyTrigCondRx(trig_apply_map, 6, 0xFFFFF);
    setMbyTrigCondRx(trig_apply_map, 9, 0x4FFFFF);
    setMbyTrigCondRx(trig_apply_map, 10, 0x4FFFFF);
    setMbyTrigCondRx(trig_apply_map, 11, 0x7FFFFF);

    for (fm_uint i = 0; i < 11; i++)
    {
        setMbyTrigCondCfg(trig_apply_map, 0, 0x1, 0x2, 0x2, 0x2, 0x2, 0x2, 0x0, 0x1, 0x0, 0x12, 0x0);
        setMbyTrigCondAmask1(trig_apply_map, i, 0x7FFFFD9F);
        setMbyTrigCondAmask2(trig_apply_map, i, 0xFFF);
    }

    setMbyTrigDirMapCtx(trig_apply_map, 1, 0x700000, 0x0, 0x0, 0x0);

    setMbyTrigDirMapAdm(trig_apply_map, 11, 0x0, 0x0, 0x0, 0x0, 0x1);

    trig_actions.vlanAction = 0x1;
    trig_actions.newVlan    = TRIG_NEW_VLAN;
    trig_actions.TCAction   = 0x1;
    trig_actions.newTC      = TRIG_NEW_TC;
    for (fm_uint i = 0; i < MBY_TRIGGERS_COUNT; i++)
        setMbyTrigAction(trig_apply_map, i, &trig_actions);

    initMbyTrigInputs(gen2trig);
}

static int simpleTriggerMatchOnVlanTCTestCheck
(
    mbyMaskGenToTriggers  const * const in,
    mbyTriggersToCongMgmt const * const out
)
{
    if(in->ACTION != out->ACTION || in->IDGLORT != out->IDGLORT || in->L2_EVID1 == out->L2_EVID1 ||
       in->MIRROR0_PROFILE_IDX != out->MIRROR0_PROFILE_IDX || in->MIRROR0_PROFILE_V != out->MIRROR0_PROFILE_V ||
       in->MIRROR1_PROFILE_IDX != out->MIRROR1_PROFILE_IDX || in->MIRROR1_PROFILE_V != out->MIRROR1_PROFILE_V ||
       in->QOS_TC == out->QOS_TC)
        return 1;

    for (fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
    {
        if(in->DMASK[i] != out->DMASK[i])
            return 1;
    }

    if(out->L2_EVID1 != TRIG_NEW_VLAN || out->QOS_TC != TRIG_NEW_TC)
        return 1;

    return 0;
}

static int runOnSimpleTrigger
(
    run_on_simple_trigger_setup_fn setup,
    run_on_simple_trigger_check_fn check
)
{
    mby_ppe_trig_apply_map      trig_apply_map      = { 0 };
    mby_ppe_trig_apply_misc_map trig_apply_misc_map = { 0 };
    mby_ppe_fwd_misc_map        fwd_misc_map        = { 0 };
    mby_ppe_mapper_map          mapper_map          = { 0 };

    mbyMaskGenToTriggers  gen2trig          = { 0 };
    mbyTriggersToCongMgmt trig2con          = { 0 };
    mbyMaskGenToTriggers  const * const in  = &gen2trig;
    mbyTriggersToCongMgmt       * const out = &trig2con;

    setup
    (
        &trig_apply_map,
        &trig_apply_misc_map,
        &fwd_misc_map,
        &gen2trig
    );

    Triggers
    (
        &trig_apply_map,
        &trig_apply_misc_map,
        &fwd_misc_map,
        &mapper_map,
        in,
        out
    );

    int ret = check(in, out);

    return ret;
}


int main(void)
{
    printf("--------------------------------------------------------------------------------\n");

    fm_uint tests = 0;
    fm_uint fails = 0;
    // default values
    SIMPLE_TRIGGER_TEST(Basic,           fails); tests++;
    SIMPLE_TRIGGER_TEST(MatchOnVlanTC,   fails); tests++;

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