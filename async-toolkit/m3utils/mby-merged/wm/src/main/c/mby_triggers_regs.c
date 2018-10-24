// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_triggers_regs.h"

mbyTriggerConditionCfg mbyTrigGetConditionCfg
(
    MBY_TRG_IN_REGS,
    fm_byte            const trig
)
{
    mbyTriggerConditionCfg cond_cfg;

    cond_cfg.MATCH_TX = trig_apply_map->TRIGGER_CONDITION_CFG[trig].MATCH_TX;                             // [35:34]
    cond_cfg.MATCH_RANDOM_THRESHOLD = trig_apply_map->TRIGGER_CONDITION_CFG[trig].MATCH_RANDOM_THRESHOLD; // [33:29]
    cond_cfg.MATCH_RANDOM_IF_LESS = trig_apply_map->TRIGGER_CONDITION_CFG[trig].MATCH_RANDOM_IF_LESS;     // [28:28]
    cond_cfg.MATCH_RANDOM_NUMBER = trig_apply_map->TRIGGER_CONDITION_CFG[trig].MATCH_RANDOM_NUMBER;       // [27:27]
    cond_cfg.MATCH_BY_PRECEDENCE = trig_apply_map->TRIGGER_CONDITION_CFG[trig].MATCH_BY_PRECEDENCE;       // [26:26]
    cond_cfg.MATCH_EGRESS_DOMAIN = trig_apply_map->TRIGGER_CONDITION_CFG[trig].MATCH_EGRESS_DOMAIN;       // [21:20]
    cond_cfg.MATCH_DEST_GLORT = trig_apply_map->TRIGGER_CONDITION_CFG[trig].MATCH_DEST_GLORT;             // [19:18]
    cond_cfg.MATCH_TC = trig_apply_map->TRIGGER_CONDITION_CFG[trig].MATCH_TC;                             // [15:14]
    cond_cfg.MATCH_VLAN = trig_apply_map->TRIGGER_CONDITION_CFG[trig].MATCH_VLAN;                         // [11:10]
    cond_cfg.LEARN = trig_apply_map->TRIGGER_CONDITION_CFG[trig].LEARN;                                                   // [ 5: 4]

    // <--- REVISIT!!!!
    //MATCH_METADATA1;
    //MATCH_METADATA0;
    //MATCH_ETHER_TYPE;
    //MATCH_HIT_SADA;
    //MATCH_HIT_DA;
    //MATCH_HIT_SA;
    //MATCH_DA;
    //MATCH_SA;

    return cond_cfg;
}

mbyTriggerConditionParam mbyTrigGetConditionParam
(   
    MBY_TRG_IN_REGS,
    fm_byte            const trig
)
{
    mbyTriggerConditionParam param_cfg;
    
    param_cfg.EGRESS_DOMAIN_MASK = trig_apply_map->TRIGGER_CONDITION_PARAM[trig].EGRESS_DOMAIN_MASK;   // [59:45]
    param_cfg.EGRESS_DOMAIN_VALUE = trig_apply_map->TRIGGER_CONDITION_PARAM[trig].EGRESS_DOMAIN_VALUE; // [44:30]
    param_cfg.ROUTED_MASK = trig_apply_map->TRIGGER_CONDITION_PARAM[trig].ROUTED_MASK;                 // [25:24]
    param_cfg.FRAME_CLASS_MASK = trig_apply_map->TRIGGER_CONDITION_PARAM[trig].FRAME_CLASS_MASK;       // [23:21]
    param_cfg.TC = trig_apply_map->TRIGGER_CONDITION_PARAM[trig].TC;                                   // [20:18]
    param_cfg.VID_ID = trig_apply_map->TRIGGER_CONDITION_PARAM[trig].VID_ID;                           // [17:12]

    // <--- REVISIT!!!!
    //FTYPE_MASK
    //DA_ID
    //SA_ID

    return param_cfg;
}

mbyTriggerConditionCGRP mbyTrigGetConditionCGRP
(
    MBY_TRG_IN_REGS,
    fm_byte            const trig
)
{
    mbyTriggerConditionCGRP cgrp_cond;

    cgrp_cond.CGRP_MASK = trig_apply_map->TRIGGER_CONDITION_CGRP[trig].CGRP_MASK; // [15:8]
    cgrp_cond.CGRP_ID = trig_apply_map->TRIGGER_CONDITION_CGRP[trig].CGRP_ID;     // [ 7:0]

    return cgrp_cond;
}

mbyTriggerConditionGlort mbyTrigGetConditionGlort
(
    MBY_TRG_IN_REGS,
    fm_byte            const trig
)
{
    mbyTriggerConditionGlort glort_cond;

    glort_cond.GLORT_MASK = trig_apply_map->TRIGGER_CONDITION_GLORT[trig].GLORT_MASK;   // [31:16]
    glort_cond.DEST_GLORT = trig_apply_map->TRIGGER_CONDITION_GLORT[trig].DEST_GLORT;   // [15: 0]

    return glort_cond;
}

mbyTriggerConditionRx mbyTrigGetConditionRx
(
    MBY_TRG_IN_REGS,
    fm_byte            const trig
)
{
    mbyTriggerConditionRx rx_cond;

    rx_cond.SRC_PORT_MASK = trig_apply_map->TRIGGER_CONDITION_RX[trig].SRC_PORT_MASK;   // [17: 0]

    return rx_cond;
}

mbyTriggerConditionAmask1 mbyTrigGetConditionAmask1
(
    MBY_TRG_IN_REGS,
    fm_byte            const trig
)
{
    mbyTriggerConditionAmask1 amask1_cond;

    amask1_cond.HANDLER_ACTION_MASK = trig_apply_map->TRIGGER_CONDITION_AMASK_1[trig].HANDLER_ACTION_MASK;   // [31: 0]

    return amask1_cond;
}

mbyTriggerConditionAmask2 mbyTrigGetConditionAmask2
(
    MBY_TRG_IN_REGS,
    fm_byte            const trig
)
{
    mbyTriggerConditionAmask2 amask2_cond;

    amask2_cond.HANDLER_ACTION_MASK = trig_apply_map->TRIGGER_CONDITION_AMASK_2[trig].HANDLER_ACTION_MASK;   // [31: 0]

    return amask2_cond;
}

mbyTriggerActions mbyTriggerGetActions
(
    MBY_TRG_IN_REGS,
    fm_byte            const trig
)
{
    mbyTriggerActions trig_actions;

    trig_actions.forwardingAction = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].FORWARDING_ACTION;          // [ 1: 0]
    trig_actions.trapAction = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].TRAP_ACTION;                      // [ 3: 2]
    trig_actions.TCAction = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].TC_ACTION;                          // [ 8: 8]
    trig_actions.vlanAction = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].VLAN_ACTION;                       // [ 9: 9]
    trig_actions.learningAction = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].LEARNING_ACTION;               // [11:10]
    trig_actions.rateLimitAction = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].RATE_LIMIT_ACTION;            // [12:12]
    trig_actions.egressL2DomainAction = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].EGRESS_L2_DOMAIN_ACTION; // [17:17]
    trig_actions.egressL3DomainAction = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].EGRESS_L3_DOMAIN_ACTION; // [18:18]
    trig_actions.policerAction = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].POLICER_ACTION;                 // [19:19]
    trig_actions.noModifyAction = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].NO_MODIFY_ACTION;              // [20:20]
    trig_actions.mirroringAction0 = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].MIRRORING_ACTION0;           // [22:21]
    trig_actions.mirroringAction1 = trig_apply_map->TRIGGER_ACTION_CFG_1[trig].MIRRORING_ACTION1;           // [24:23]
    // <--- REVISIT!!!!
    //MIRRORING_ACTION2
    //MIRRORING_ACTION3
    //METADATA_ACTION_SLOT removed

    trig_actions.newTC = trig_apply_map->TRIGGER_ACTION_CFG_2[trig].NEW_TC;                   // [ 2: 0]
    trig_actions.newVlan = trig_apply_map->TRIGGER_ACTION_CFG_2[trig].NEW_EVID;               // [14: 3]
    trig_actions.newRateLimitNum = trig_apply_map->TRIGGER_ACTION_CFG_2[trig].RATE_LIMIT_NUM; // [18:15]
    trig_actions.trapCode = trig_apply_map->TRIGGER_ACTION_CFG_2[trig].TRAP_CODE;              // [21:19]

    trig_actions.filterDestMask = trig_apply_map->TRIGGER_DIRECT_MAP_ADM4.FILTER_DEST_MASK; // [ 1: 0]
    trig_actions.newDestMask = trig_apply_map->TRIGGER_DIRECT_MAP_ADM4.NEW_DEST_MASK;        // [ 2: 2]
    // <--- REVISIT!!!!
    //TRIGGER_DIRECT_MAP_ADM0
    //TRIGGER_DIRECT_MAP_ADM1
    //TRIGGER_DIRECT_MAP_ADM2
    //TRIGGER_DIRECT_MAP_ADM3

    trig_actions.newDestGlort = trig_apply_map->TRIGGER_ACTION_GLORT[trig].NEW_DEST_GLORT;          // [15: 0]
    trig_actions.newDestGlortMask = trig_apply_map->TRIGGER_ACTION_GLORT[trig].NEW_DEST_GLORT_MASK; // [16:31]

    trig_actions.mirrorProfileIndex0 = trig_apply_map->TRIGGER_ACTION_MIRROR[trig].MIRROR_PROFILE_INDEX0; // [ 5: 0]
    trig_actions.mirrorProfileIndex1 = trig_apply_map->TRIGGER_ACTION_MIRROR[trig].MIRROR_PROFILE_INDEX1; // [11: 6]
    // <--- REVISIT!!!!
    //MIRROR_PROFILE_INDEX2
    //MIRROR_PROFILE_INDEX3

    trig_actions.dropMask = trig_apply_map->TRIGGER_DIRECT_MAP_ADR4.DROP_MASK; // [ 1: 0]
    // <--- REVISIT!!!!
    //TRIGGER_DIRECT_MAP_ADR0
    //TRIGGER_DIRECT_MAP_ADR1
    //TRIGGER_DIRECT_MAP_ADR2
    //TRIGGER_DIRECT_MAP_ADR3
   
   return trig_actions;
}