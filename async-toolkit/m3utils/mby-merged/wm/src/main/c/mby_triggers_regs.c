/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_triggers_regs.h"

mbyTriggerConditionCfg mbyTrigGetConditionCfg
(
    mby_ppe_trig_apply_map const * const trig_apply_map,
    fm_byte                        const trig
)
{
    mbyTriggerConditionCfg cond_cfg;

    cond_cfg.MATCH_TX               = trig_apply_map->TRIGGER_CONDITION_CFG[trig].MATCH_TX;
    cond_cfg.MATCH_RANDOM_THRESHOLD = trig_apply_map->TRIGGER_CONDITION_CFG[trig].MATCH_RANDOM_THRESHOLD;
    cond_cfg.MATCH_RANDOM_IF_LESS   = trig_apply_map->TRIGGER_CONDITION_CFG[trig].MATCH_RANDOM_IF_LESS;
    cond_cfg.MATCH_RANDOM_NUMBER    = trig_apply_map->TRIGGER_CONDITION_CFG[trig].MATCH_RANDOM_NUMBER;
    cond_cfg.MATCH_BY_PRECEDENCE    = trig_apply_map->TRIGGER_CONDITION_CFG[trig].MATCH_BY_PRECEDENCE;
    cond_cfg.MATCH_EGRESS_DOMAIN    = trig_apply_map->TRIGGER_CONDITION_CFG[trig].MATCH_EGRESS_DOMAIN;
    cond_cfg.MATCH_DEST_GLORT       = trig_apply_map->TRIGGER_CONDITION_CFG[trig].MATCH_DEST_GLORT;
    cond_cfg.MATCH_TC               = trig_apply_map->TRIGGER_CONDITION_CFG[trig].MATCH_TC;
    cond_cfg.MATCH_CGRP             = trig_apply_map->TRIGGER_CONDITION_CFG[trig].MATCH_CGRP;
    cond_cfg.MATCH_VLAN             = trig_apply_map->TRIGGER_CONDITION_CFG[trig].MATCH_VLAN;
    cond_cfg.LEARN                  = trig_apply_map->TRIGGER_CONDITION_CFG[trig].LEARN;

    return cond_cfg;
}

mbyTriggerConditionParam mbyTrigGetConditionParam
(
    mby_ppe_trig_apply_map const * const trig_apply_map,
    fm_byte                        const trig
)
{
    mbyTriggerConditionParam param_cfg;

    param_cfg.EGRESS_DOMAIN_MASK  = trig_apply_map->TRIGGER_CONDITION_PARAM[trig].EGRESS_DOMAIN_MASK;
    param_cfg.EGRESS_DOMAIN_VALUE = trig_apply_map->TRIGGER_CONDITION_PARAM[trig].EGRESS_DOMAIN_VALUE;
    param_cfg.ROUTED_MASK         = trig_apply_map->TRIGGER_CONDITION_PARAM[trig].ROUTED_MASK;
    param_cfg.FRAME_CLASS_MASK    = trig_apply_map->TRIGGER_CONDITION_PARAM[trig].FRAME_CLASS_MASK;
    param_cfg.TC                  = trig_apply_map->TRIGGER_CONDITION_PARAM[trig].TC;
    param_cfg.VID_ID              = trig_apply_map->TRIGGER_CONDITION_PARAM[trig].VID_ID;

    return param_cfg;
}

mbyTriggerConditionCGRP mbyTrigGetConditionCGRP
(
    mby_ppe_trig_apply_map const * const trig_apply_map,
    fm_byte                        const trig
)
{
    mbyTriggerConditionCGRP cgrp_cond;

    cgrp_cond.CGRP_MASK = trig_apply_map->TRIGGER_CONDITION_CGRP[trig].CGRP_MASK;
    cgrp_cond.CGRP_ID   = trig_apply_map->TRIGGER_CONDITION_CGRP[trig].CGRP_ID;

    return cgrp_cond;
}

mbyTriggerConditionGlort mbyTrigGetConditionGlort
(
    mby_ppe_trig_apply_map const * const trig_apply_map,
    fm_byte                        const trig
)
{
    mbyTriggerConditionGlort glort_cond;

    glort_cond.GLORT_MASK = trig_apply_map->TRIGGER_CONDITION_GLORT[trig].GLORT_MASK;
    glort_cond.DEST_GLORT = trig_apply_map->TRIGGER_CONDITION_GLORT[trig].DEST_GLORT;

    return glort_cond;
}

mbyTriggerConditionRx mbyTrigGetConditionRx
(
    mby_ppe_trig_apply_map const * const trig_apply_map,
    fm_byte                        const trig
)
{
    mbyTriggerConditionRx rx_cond;

    rx_cond.SRC_PORT_MASK = trig_apply_map->TRIGGER_CONDITION_RX[trig].SRC_PORT_MASK;

    return rx_cond;
}

mbyTriggerConditionAmask1 mbyTrigGetConditionAmask1
(
    mby_ppe_trig_apply_map const * const trig_apply_map,
    fm_byte                        const trig
)
{
    mbyTriggerConditionAmask1 amask1_cond;

    amask1_cond.HANDLER_ACTION_MASK = trig_apply_map->TRIGGER_CONDITION_AMASK_1[trig].HANDLER_ACTION_MASK;

    return amask1_cond;
}

mbyTriggerConditionAmask2 mbyTrigGetConditionAmask2
(
    mby_ppe_trig_apply_map const * const trig_apply_map,
    fm_byte                        const trig
)
{
    mbyTriggerConditionAmask2 amask2_cond;

    amask2_cond.HANDLER_ACTION_MASK = trig_apply_map->TRIGGER_CONDITION_AMASK_2[trig].HANDLER_ACTION_MASK;

    return amask2_cond;
}

mbyTriggerDirectMapCtx mbyTrigGetDirectMapCtrlCtx
(
    mby_ppe_trig_apply_map const * const trig_apply_map,
    fm_byte                        const trig
)
{
    mbyTriggerDirectMapCtx direct_map_ctx;

    direct_map_ctx.DEST_PORT_MASK[4] = trig_apply_map->TRIGGER_DIRECT_MAP_CTX4.DEST_PORT_MASK;
    direct_map_ctx.DEST_PORT_MASK[3] = trig_apply_map->TRIGGER_DIRECT_MAP_CTX3.DEST_PORT_MASK;
    direct_map_ctx.DEST_PORT_MASK[2] = trig_apply_map->TRIGGER_DIRECT_MAP_CTX2.DEST_PORT_MASK;
    direct_map_ctx.DEST_PORT_MASK[1] = trig_apply_map->TRIGGER_DIRECT_MAP_CTX1.DEST_PORT_MASK;
    direct_map_ctx.DEST_PORT_MASK[0] = trig_apply_map->TRIGGER_DIRECT_MAP_CTX0.DEST_PORT_MASK;

    return direct_map_ctx;
}

static mbyTriggerDirectMapAdm mbyTrigGetDirectMapCtrlAdm
(
    mby_ppe_trig_apply_map const * const trig_apply_map,
    fm_byte                        const trig
)
{
    mbyTriggerDirectMapAdm direct_map_adm;

    direct_map_adm.NEW_DEST_MASK[4] = trig_apply_map->TRIGGER_DIRECT_MAP_ADM4.NEW_DEST_MASK;
    direct_map_adm.NEW_DEST_MASK[3] = trig_apply_map->TRIGGER_DIRECT_MAP_ADM3.NEW_DEST_MASK;
    direct_map_adm.NEW_DEST_MASK[2] = trig_apply_map->TRIGGER_DIRECT_MAP_ADM2.NEW_DEST_MASK;
    direct_map_adm.NEW_DEST_MASK[1] = trig_apply_map->TRIGGER_DIRECT_MAP_ADM1.NEW_DEST_MASK;
    direct_map_adm.NEW_DEST_MASK[0] = trig_apply_map->TRIGGER_DIRECT_MAP_ADM0.NEW_DEST_MASK;
    direct_map_adm.FILTER_DEST_MASK = trig_apply_map->TRIGGER_DIRECT_MAP_ADM4.FILTER_DEST_MASK;

    return direct_map_adm;
}

static mbyTriggerDirectMapAdr mbyTrigGetDirectMapCtrlAdr
(
    mby_ppe_trig_apply_map const * const trig_apply_map,
    fm_byte                        const trig
)
{
    mbyTriggerDirectMapAdr direct_map_adr;

    direct_map_adr.DROP_MASK[4] = trig_apply_map->TRIGGER_DIRECT_MAP_ADR4.DROP_MASK;
    direct_map_adr.DROP_MASK[3] = trig_apply_map->TRIGGER_DIRECT_MAP_ADR3.DROP_MASK;
    direct_map_adr.DROP_MASK[2] = trig_apply_map->TRIGGER_DIRECT_MAP_ADR2.DROP_MASK;
    direct_map_adr.DROP_MASK[1] = trig_apply_map->TRIGGER_DIRECT_MAP_ADR1.DROP_MASK;
    direct_map_adr.DROP_MASK[0] = trig_apply_map->TRIGGER_DIRECT_MAP_ADR0.DROP_MASK;

    return direct_map_adr;
}

mbyTriggerActions mbyTriggerGetActions
(
    mby_ppe_trig_apply_map const * const trig_apply_map,
    fm_byte                        const trig
)
{
    mbyTriggerActions trig_actions = { 0 };

    trig_actions.forwardingAction     = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].FORWARDING_ACTION;
    trig_actions.trapAction           = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].TRAP_ACTION;
    trig_actions.TCAction             = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].TC_ACTION;
    trig_actions.vlanAction           = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].VLAN_ACTION;
    trig_actions.learningAction       = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].LEARNING_ACTION;
    trig_actions.rateLimitAction      = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].RATE_LIMIT_ACTION;
    trig_actions.egressL2DomainAction = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].EGRESS_L2_DOMAIN_ACTION;
    trig_actions.egressL3DomainAction = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].EGRESS_L3_DOMAIN_ACTION;
    trig_actions.policerAction        = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].POLICER_ACTION;
    trig_actions.mirroringAction0     = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].MIRRORING_ACTION0;
    trig_actions.mirroringAction1     = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].MIRRORING_ACTION1;
    trig_actions.mirroringAction2     = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].MIRRORING_ACTION2;
    trig_actions.mirroringAction3     = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].MIRRORING_ACTION3;

    trig_actions.newTC                = trig_apply_map->TRIGGER_ACTION_CFG_2[trig].NEW_TC;
    trig_actions.newVlan              = trig_apply_map->TRIGGER_ACTION_CFG_2[trig].NEW_EVID;
    trig_actions.newRateLimitNum      = trig_apply_map->TRIGGER_ACTION_CFG_2[trig].RATE_LIMIT_NUM;
    trig_actions.trapCode             = trig_apply_map->TRIGGER_ACTION_CFG_2[trig].TRAP_CODE;
    trig_actions.filterDestMask       = trig_apply_map->TRIGGER_DIRECT_MAP_ADM4.FILTER_DEST_MASK;

    mbyTriggerDirectMapAdm direct_map_adm  = mbyTrigGetDirectMapCtrlAdm(trig_apply_map, trig);
    if (trig_apply_map->TRIGGER_DIRECT_MAP_CTRL.GO_COMPL == 0 && trig_apply_map->TRIGGER_DIRECT_MAP_CTRL.STATUS == 0)
    {
        trig_actions.filterDestMask = direct_map_adm.FILTER_DEST_MASK;
        for(fm_int i = 0; i < 5; i++)
            trig_actions.newDestMask[i] = direct_map_adm.NEW_DEST_MASK[i];
    }

    trig_actions.newDestGlort         = trig_apply_map->TRIGGER_ACTION_GLORT[trig].NEW_DEST_GLORT;
    trig_actions.newDestGlortMask     = trig_apply_map->TRIGGER_ACTION_GLORT[trig].NEW_DEST_GLORT_MASK;
    trig_actions.mirrorProfileIndex0  = trig_apply_map->TRIGGER_ACTION_MIRROR[trig].MIRROR_PROFILE_INDEX0;
    trig_actions.mirrorProfileIndex1  = trig_apply_map->TRIGGER_ACTION_MIRROR[trig].MIRROR_PROFILE_INDEX1;
    trig_actions.mirrorProfileIndex2  = trig_apply_map->TRIGGER_ACTION_MIRROR[trig].MIRROR_PROFILE_INDEX2;
    trig_actions.mirrorProfileIndex3  = trig_apply_map->TRIGGER_ACTION_MIRROR[trig].MIRROR_PROFILE_INDEX3;

    mbyTriggerDirectMapAdr direct_map_adr  = mbyTrigGetDirectMapCtrlAdr(trig_apply_map, trig);
    if (trig_apply_map->TRIGGER_DIRECT_MAP_CTRL.GO_COMPL == 0 && trig_apply_map->TRIGGER_DIRECT_MAP_CTRL.STATUS == 0)
    {
        for(fm_int i = 0; i < 5; i++)
            trig_actions.dropMask[i] = direct_map_adr.DROP_MASK[i];
    }

    return trig_actions;
}
