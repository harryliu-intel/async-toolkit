// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_bitfield.h"
#include "mby_common.h"
#include "mby_params.h"
#include "mby_maskgen_regs.h"
#include "mby_triggers_regs.h"
#include "mby_action_codes.h"
#include "mby_fclass_type.h"
#include "mby_log_type.h"
#include "mby_triggers.h"

// TODO is there a better way to include this? REVISIT
#include "../m3/model_server/src/model_c_write.h" // pull in write_field

static void dmaskClear
(
    fm_uint64 * const dmask_in
)
{
    for(fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
        dmask_in[i] = 0;
}

static void dmaskCopy
(
    fm_uint64 const * const dmask_in,
    fm_uint64       * const dmask_out
)
{
    for(fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
        dmask_out[i] = dmask_in[i];
}

static fm_bool isDmask
(
    fm_uint64 const * const dmask
)
{
    fm_bool is_dmask = FALSE;

    for(fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
    {
        is_dmask = (dmask[i] != 0);
        if(is_dmask) break;
    }

    return is_dmask;
}

static fm_bool isActionDropTrig
(
    fm_uint64 const * const dmask
)
{
    fm_bool is_action_drop_trig = TRUE;

    for(fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
    {
        is_action_drop_trig = (dmask[i] == 0);
        if(!is_action_drop_trig) break;
    }

    return is_action_drop_trig;
}

static inline void incrementTrigCounter
(
    mby_ppe_trig_apply_map       const * const trig_apply_map,
    mby_ppe_trig_apply_map__addr const * const trig_apply_map_w,
    fm_byte                              const trig
)
{
    fm_uint64 trig_count = trig_apply_map->TRIGGER_STATS[trig].COUNT; // [63:0]
    trig_count = (trig_count == FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF)) ? 0 : trig_count + 1;

    write_field(trig_apply_map_w->TRIGGER_STATS[trig].COUNT, trig_count);

}

/**
 * Triggers Match Conditions and random matching
 * Specified [match conditions] (https://securewiki.ith.intel.com/display/25T/RX-PPE+Triggers#RX-PPETriggers-TriggerMatchConditions)
 * and [random matching] (https://securewiki.ith.intel.com/display/25T/RX-PPE+Triggers#RX-PPETriggers-RandomMatching)
 *
 * @param[in] trig_apply_map Trigger Registers map
 * @param[in] trig           Trigger number
 * @param[in] learn_en       LEARNING_ENABLED coming from MaskGen stage
 * @param[in] l2_evid1       L2_EVID1 coming from MaskGen stage
 * @param[in] cgrp_trig      CGRP_TRIG coming from MaskGen stage
 * @param[in] qos_tc         QOS_TC coming from MaskGen stage
 * @param[in] idglort        IDGLORT coming from MaskGen stage
 * @param[in] l3_edomain     L3_SMAC coming from MaskGen stage
 * @param[in] l2_edomain     L2_SMAC coming from MaskGen stage
 * @param[in] fclass         FCLASS coming from MaskGen stage
 * @param[in] mark_routed    MARK_ROUTED coming from MaskGen stage
 * @param[in] dmask          DMASK coming from MaskGen stage
 * @param[in] rx_port        RX_PORT coming from MaskGen stage
 * @param[in] amask          AMASK coming from MaskGen stage
 *
 * \return True if all of the conditions are matched, false if not
 */
static fm_bool evaluateTrigger
(
    mby_ppe_trig_apply_map const * const trig_apply_map,
    fm_int                         const trig,
    fm_bool                        const learn_en,
    fm_uint16                      const l2_evid1,
    fm_byte                        const cgrp_trig,
    fm_byte                        const qos_tc,
    fm_uint16                      const idglort,
    fm_byte                        const l3_edomain,
    fm_byte                        const l2_edomain,
    fm_byte                        const fclass,
    fm_bool                        const mark_routed,
    fm_uint64              const * const dmask,
    fm_uint32                      const rx_port,
    fm_uint64                      const amask
)
{
    mbyTriggerConditionCfg      cond_cfg;
    mbyTriggerConditionCGRP     cond_cgrp;
    mbyTriggerConditionParam    cond_param;
    mbyTriggerConditionGlort    cond_glort;
    mbyTriggerConditionRx       cond_rx;
    mbyTriggerConditionAmask1   cond_amask1;
    mbyTriggerConditionAmask2   cond_amask2;
    mbyTriggerDirectMapCtx      direct_map_ctx;
    fm_uint16                   egress_domain = 0;
    fm_bool                     cgrp_hit;
    fm_bool                     dglort_hit;
    fm_bool                     egress_domain_hit;
    fm_bool                     r_hit;
    fm_bool                     hit = TRUE;
    fm_uint32                   trig_lfsr0 = 0;
    fm_uint32                   trig_lfsr1 = 0;
    fm_uint64                   dest_port_mask;
    fm_uint32                   src_port_mask;
    fm_uint64                   action_mask;
    fm_uint64                   action_mask2;

    cond_cfg       = mbyTrigGetConditionCfg    (trig_apply_map, trig);
    cond_cgrp      = mbyTrigGetConditionCGRP   (trig_apply_map, trig);
    cond_param     = mbyTrigGetConditionParam  (trig_apply_map, trig);
    cond_glort     = mbyTrigGetConditionGlort  (trig_apply_map, trig);
    cond_rx        = mbyTrigGetConditionRx     (trig_apply_map, trig);
    direct_map_ctx = mbyTrigGetDirectMapCtrlCtx(trig_apply_map, trig);
    cond_amask1    = mbyTrigGetConditionAmask1 (trig_apply_map, trig);
    cond_amask2    = mbyTrigGetConditionAmask2 (trig_apply_map, trig);

    //This code doesn't match RTL. So DV should not enable WM for matchRandom tests
    trig_lfsr0 = FM_GET_UNNAMED_FIELD(fmRand(), 0, 24);
    trig_lfsr1 = FM_GET_UNNAMED_FIELD(fmRand(), 0, 24);

    /* Match on the LEARN NOTIFY. */
    hit &= ( ( !learn_en && cond_cfg.LEARN == 0 ) ||
             (  learn_en && cond_cfg.LEARN == 1 ) ||
             (              cond_cfg.LEARN == 2 ) );

    /* Match on the egress VLAN ID. */
    hit &= ( ( l2_evid1 != cond_param.VID_ID && cond_cfg.MATCH_VLAN == 0 ) ||
             ( l2_evid1 == cond_param.VID_ID && cond_cfg.MATCH_VLAN == 1 ) ||
             (                                  cond_cfg.MATCH_VLAN == 2 ) );

    /* Match on the CGRP lookup result. */
    cgrp_hit = ( (cgrp_trig         & cond_cgrp.CGRP_MASK) ==
                 (cond_cgrp.CGRP_ID & cond_cgrp.CGRP_MASK) );
    hit &= ( ( !cgrp_hit && cond_cfg.MATCH_CGRP == 0 ) ||
             (  cgrp_hit && cond_cfg.MATCH_CGRP == 1 ) ||
             (              cond_cfg.MATCH_CGRP == 2 ) );

    /* Match on the traffic class (TC). */
    hit &= ( ( qos_tc != cond_param.TC && cond_cfg.MATCH_TC == 0 ) ||
             ( qos_tc == cond_param.TC && cond_cfg.MATCH_TC == 1 ) ||
             (                            cond_cfg.MATCH_TC == 2 ) );

    /* Match on the destination GLORT. */
    dglort_hit = ( ( idglort               & cond_glort.GLORT_MASK) ==
                   ( cond_glort.DEST_GLORT & cond_glort.GLORT_MASK) );
    hit &= ( ( !dglort_hit && cond_cfg.MATCH_DEST_GLORT == 0 ) ||
             (  dglort_hit && cond_cfg.MATCH_DEST_GLORT == 1 ) ||
             (                cond_cfg.MATCH_DEST_GLORT == 2 ) );

    /* Match on EgressDomainValue. */
    egress_domain = ((l3_edomain & 0x3f) << 8) | (l2_edomain & 0xff);
    egress_domain_hit = ( (egress_domain                  & cond_param.EGRESS_DOMAIN_MASK) ==
                          (cond_param.EGRESS_DOMAIN_VALUE & cond_param.EGRESS_DOMAIN_MASK) );
    hit &= ( ( !egress_domain_hit && cond_cfg.MATCH_EGRESS_DOMAIN == 0 ) ||
             (  egress_domain_hit && cond_cfg.MATCH_EGRESS_DOMAIN == 1 ) ||
             (                       cond_cfg.MATCH_EGRESS_DOMAIN == 2 ) );

    /* Match on the packet class (Layer 2 unicast, broadcast or multicast). */
    hit &= ( ( (fclass == MBY_FCLASS_UNICAST) &&
               ( ( cond_param.FRAME_CLASS_MASK & 0x1 ) != 0 ) ) ||
             ( (fclass == MBY_FCLASS_BROADCAST) &&
               ( ( cond_param.FRAME_CLASS_MASK & 0x2 ) != 0 ) ) ||
             ( (fclass == MBY_FCLASS_MULTICAST) &&
               ( ( cond_param.FRAME_CLASS_MASK & 0x4 ) != 0 ) ) );

    /* Match on the ingress port. */
    src_port_mask = cond_rx.SRC_PORT_MASK;
    hit &= ( (src_port_mask & (1 << rx_port)) != 0 );

    /* Match on the (set of) egress port(s). */
    for(fm_int i = 0; i < MBY_DMASK_REGISTERS; i++)
    {
        dest_port_mask = direct_map_ctx.DEST_PORT_MASK[i];
        switch (  cond_cfg.MATCH_TX)
        {
            case MBY_TRIGGER_CONDITION_CFG_MATCH_TX_MASK_Z:
                hit &= ( (dmask[i] & dest_port_mask) == 0);
                break;
            case MBY_TRIGGER_CONDITION_CFG_MATCH_TX_MASK_NZ:
                hit &= ( (dmask[i] & dest_port_mask) != 0);
                break;
            case MBY_TRIGGER_CONDITION_CFG_MATCH_TX_EXACT_EQ:
                hit &= (  dmask[i] == dest_port_mask );
                break;
            case MBY_TRIGGER_CONDITION_CFG_MATCH_TX_EXACT_NE:
                hit &= (  dmask[i] != dest_port_mask );
                break;
            default:
                hit = 0;
                break;
        }
    }

    /* Match on the packet's route status. */
    hit &= ( ( !mark_routed && ( (cond_param.ROUTED_MASK & 0x1) != 0 ) ) ||
             (  mark_routed && ( (cond_param.ROUTED_MASK & 0x2) != 0 ) ) );

    /* Match on one or more bits of the frame handler action mask. */
    action_mask  = cond_amask1.HANDLER_ACTION_MASK;
    action_mask2 = cond_amask2.HANDLER_ACTION_MASK;
    FM_SET_UNNAMED_FIELD64(action_mask, 32, 13, action_mask2);
    hit &= ( (amask & action_mask) != 0 );

    /* Random Matching */
    r_hit = ( ( cond_cfg.MATCH_RANDOM_NUMBER  == 0 ) &&
              ( cond_cfg.MATCH_RANDOM_IF_LESS == 1 ) &&
              ( trig_lfsr0 <= (FM_LITERAL_U64(1) << cond_cfg.MATCH_RANDOM_THRESHOLD ) ) ) ||
            ( ( cond_cfg.MATCH_RANDOM_NUMBER  == 0 ) &&
              ( cond_cfg.MATCH_RANDOM_IF_LESS == 0 ) &&
              ( trig_lfsr0 >  (FM_LITERAL_U64(1) << cond_cfg.MATCH_RANDOM_THRESHOLD) ) ) ||
            ( ( cond_cfg.MATCH_RANDOM_NUMBER  == 1 ) &&
              ( cond_cfg.MATCH_RANDOM_IF_LESS == 1 ) &&
              ( trig_lfsr1 <= (FM_LITERAL_U64(1) << cond_cfg.MATCH_RANDOM_THRESHOLD) ) ) ||
            ( ( cond_cfg.MATCH_RANDOM_NUMBER  == 1 ) &&
              ( cond_cfg.MATCH_RANDOM_IF_LESS == 0 ) &&
              ( trig_lfsr1 >  (FM_LITERAL_U64(1) << cond_cfg.MATCH_RANDOM_THRESHOLD) ) );

        hit &= r_hit;

    return hit;
}

static void applyPrecedenceResolution
(
    mbyTriggerActions * const lo,
    fm_int              const trig,
    mbyTriggerActions * const hi
)
{

    if ( ( lo->forwardingAction == MBY_TRIG_ACTION_FORWARDING_DROP ) &&
         ( hi->forwardingAction == MBY_TRIG_ACTION_FORWARDING_DROP ) )
    {
        for (fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
            lo->dropMask[i] = lo->dropMask[i] | hi->dropMask[i];
    }

    else if ( hi->forwardingAction > lo->forwardingAction )
    {
        for (fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
        {
            lo->dropMask[i]    = hi->dropMask[i];
            lo->newDestMask[i] = hi->newDestMask[i];
        }
        lo->forwardingAction = hi->forwardingAction;
        lo->newDestGlort     = hi->newDestGlort;
        lo->newDestGlortMask = hi->newDestGlortMask;
        lo->filterDestMask   = hi->filterDestMask;
    }

    if ( hi->trapAction > lo->trapAction )
    {
        lo->trapAction = hi->trapAction;
        if(hi->trapAction == MBY_TRIG_ACTION_TRAP_TRAP)
            lo->trapCode = hi->trapCode;
    }

    if ( hi->mirroringAction0 > lo->mirroringAction0 )
    {
        lo->mirroringAction0    = hi->mirroringAction0;
        lo->mirrorProfileIndex0 = hi->mirrorProfileIndex0;
    }

    if ( hi->mirroringAction1 > lo->mirroringAction1 )
    {
        lo->mirroringAction1    = hi->mirroringAction1;
        lo->mirrorProfileIndex1 = hi->mirrorProfileIndex1;
    }

    if ( hi->mirroringAction2 > lo->mirroringAction2 )
    {
        lo->mirroringAction2    = hi->mirroringAction2;
        lo->mirrorProfileIndex2 = hi->mirrorProfileIndex2;
    }

    if ( hi->mirroringAction3 > lo->mirroringAction3 )
    {
        lo->mirroringAction3    = hi->mirroringAction3;
        lo->mirrorProfileIndex3 = hi->mirrorProfileIndex3;
    }

    if ( hi->TCAction > lo->TCAction )
    {
        lo->TCAction = hi->TCAction;
        lo->newTC    = hi->newTC;
    }

    if ( hi->vlanAction > lo->vlanAction )
    {
        lo->vlanAction = hi->vlanAction;
        lo->newVlan    = hi->newVlan;
    }

    if ( hi->learningAction > lo->learningAction )
        lo->learningAction = hi->learningAction;

    if ( hi->rateLimitAction > lo->rateLimitAction )
    {
        lo->rateLimitAction = hi->rateLimitAction;
        lo->newRateLimitNum = hi->newRateLimitNum;
    }

    if ( hi->policerAction > lo->policerAction )
        lo->policerAction = hi->policerAction;

    if ( hi->egressL3DomainAction > lo->egressL3DomainAction )
        lo->egressL3DomainAction = hi->egressL3DomainAction;

    if ( hi->egressL2DomainAction > lo->egressL2DomainAction )
        lo->egressL2DomainAction = hi->egressL2DomainAction;
}

/**
 * Configurable Trigger Precedence and action resolution
 * Specified [trigger precedence] (https://securewiki.ith.intel.com/display/25T/RX-PPE+Triggers#RX-PPETriggers-ConfigurableTriggerPrecedence)
 * and [action resolution] (https://securewiki.ith.intel.com/display/25T/RX-PPE+Triggers#RX-PPETriggers-TriggerActionResolution)
 *
 * @param[in]  trig_apply_map            Trigger Registers map
 * @param[in]  hit_mask_hi               Matched trigges from 64 - 95
 * @param[in]  hit_mask_lo               Matched trigges from 0  - 63
 * @param[out] trig_hit_mask_resolved_lo Output with fired triggers from 0  - 63
 * @param[out] trig_hit_mask_resolved_hi Output with fired triggers from 64 - 95
 * @param[out] lo                        Output with actions
 */
static void resolveTriggers
(
    mby_ppe_trig_apply_map const * const trig_apply_map,
    fm_uint64                      const hit_mask_hi,
    fm_uint64                      const hit_mask_lo,
    fm_uint64                    * const trig_hit_mask_resolved_lo,
    fm_int64                     * const trig_hit_mask_resolved_hi,
    mbyTriggerActions            * const lo
)
{
    mbyTriggerActions hi;
    fm_bool           prec_winner = FALSE;

    *trig_hit_mask_resolved_hi = hit_mask_hi;
    *trig_hit_mask_resolved_lo = hit_mask_lo;

    for (fm_int i = 0; i < MBY_TRIGGERS_COUNT; i++)
    {
        mbyTriggerConditionCfg cond_cfg  = mbyTrigGetConditionCfg(trig_apply_map, i);
        prec_winner                     &= cond_cfg.MATCH_BY_PRECEDENCE;

        if (i < 64)
        {
            if ( ( (hit_mask_lo & (FM_LITERAL_U64(1) << i)) != 0 ) && !prec_winner )
            {
                prec_winner = TRUE;
                hi          = mbyTriggerGetActions(trig_apply_map, i);
                applyPrecedenceResolution(lo, i, &hi);
            }
            else if ( ( (hit_mask_lo & (FM_LITERAL_U64(1) << i)) != 0 ) && prec_winner )
                *trig_hit_mask_resolved_lo &= ~(FM_LITERAL_U64(1) << i);

        }
        else
        {
            if ( ( (hit_mask_hi & (FM_LITERAL_U64(1) << (i - 64))) != 0 ) && !prec_winner )
            {
                prec_winner = TRUE;
                hi          = mbyTriggerGetActions(trig_apply_map, i);
                applyPrecedenceResolution(lo, i, &hi);
            }
            else if ( ( (hit_mask_hi & (FM_LITERAL_U64(1) << (i - 64))) != 0 ) && prec_winner )
                *trig_hit_mask_resolved_hi &= ~(FM_LITERAL_U64(1) << (i - 64));
        }
    }
}

/**
 * Trigger Actions
 * Specified [here] (https://securewiki.ith.intel.com/display/25T/RX-PPE+Triggers#RX-PPETriggers-TriggerActions)
 *
 * @param[in]  actions               Actions that are the result of the resolveTriggers function
 * @param[out] results               Actions to apply to the frame
 * @param[in]  action                ACTION coming from MaskGen stage
 * @param[in]  dmask                 Pointer to array with DMASK coming from MaskGen stage
 * @param[in]  pre_resolve_action    PRE_RESOLVE_ACTION coming from MaskGen stage
 * @param[in]  pre_resolve_dmask     Pointer to array with PRE_RESOLVE_DMASK coming from MaskGen stage
 * @param[in]  pre_resolve_dglort    PRE_RESOLVE_DGLORT coming from MaskGen stage
 * @param[in]  idglort               IDGLORT coming from MaskGen stage
 * @param[in]  qcn_mirror0_profile_v QCN_MIRROR0_PROFILE_V coming from MaskGen stage
 * @param[in]  qcn_mirror1_profile_v QCN_MIRROR1_PROFILE_V coming from MaskGen stage
 * @param[in]  mirror0_profile_idx   MIRROR0_PROFILE_IDX coming from MaskGen stage
 * @param[in]  mirror0_profile_v     MIRROR0_PROFILE_V coming from MaskGen stage
 * @param[in]  mirror1_profile_idx   MIRROR1_PROFILE_IDX coming from MaskGen stage
 * @param[in]  mirror1_profile_v     MIRROR1_PROFILE_V coming from MaskGen stage
 * @param[in]  qos_tc                QOS_TC coming from MaskGen stage
 * @param[in]  l2_evid1              L2_EVID1 coming from MaskGen stage
 * @param[out] learning_enabled      Output depends on learning action
 */
static void applyTriggers
(
    mbyTriggerActions       * const actions,
    mbyTriggerResults       * const results,
    fm_uint32                 const action,
    fm_uint64         const * const dmask,
    fm_uint32                 const pre_resolve_action,
    fm_uint64         const * const pre_resolve_dmask,
    fm_uint32                 const pre_resolve_dglort,
    fm_uint16                 const idglort,
    fm_bool                   const qcn_mirror0_profile_v,
    fm_bool                   const qcn_mirror1_profile_v,
    fm_uint32                 const mirror0_profile_idx,
    fm_byte                   const mirror0_profile_v,
    fm_uint32                 const mirror1_profile_idx,
    fm_byte                   const mirror1_profile_v,
    fm_byte                   const qos_tc,
    fm_uint16                 const l2_evid1,
    fm_bool                 * const learning_enabled
)
{
    fm_uint16                dglort;
    fm_bool                  is_pre_resolve_dmask = FALSE;
    fm_bool                  is_action_drop_trig  = TRUE;

    results->filterDestMask = TRUE;

    /* store the action in the triggerResults channel */
    results->forwardingAction = actions->forwardingAction;

    switch (actions->forwardingAction)
    {
        case MBY_TRIG_ACTION_FORWARDING_FORWARD:
            is_pre_resolve_dmask = isDmask(pre_resolve_dmask);

            if ( is_pre_resolve_dmask )
            {
                dglort = (pre_resolve_dglort    & ~actions->newDestGlortMask) |
                         (actions->newDestGlort & actions->newDestGlortMask);
                results->destGlort = dglort;
                dmaskCopy(pre_resolve_dmask, results->destMask);
                results->action    = pre_resolve_action;
            }
            else
            {
                results->destGlort = idglort;
                dmaskCopy(dmask, results->destMask);
                results->action    = action;
            }
            break;

        case MBY_TRIG_ACTION_FORWARDING_REDIRECT:
            dglort = (idglort & ~actions->newDestGlortMask) |
                     (actions->newDestGlort & actions->newDestGlortMask);
            results->destGlort      = dglort;
            dmaskCopy(actions->newDestMask, results->destMask);
                is_action_drop_trig = isActionDropTrig(results->destMask);
            results->filterDestMask = actions->filterDestMask;
            results->action         = (is_action_drop_trig) ? MBY_ACTION_DROP_TRIG : MBY_ACTION_REDIRECT_TRIG;
            break;

        case MBY_TRIG_ACTION_FORWARDING_DROP:
            results->destGlort = idglort;
            for(fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
            {
                results->destMask[i]  = dmask[i] & ~actions->dropMask[i];
                is_action_drop_trig      = is_action_drop_trig ? ( ( dmask[i] != 0 ) && ( results->destMask[i] == 0 ) ) : is_action_drop_trig;
            }
            results->action = (is_action_drop_trig) ? MBY_ACTION_DROP_TRIG : action;
            break;

        default:
            results->destGlort = idglort;
            dmaskCopy(dmask, results->destMask);
            results->action    = action;
            break;
    }

    /* store trapAction in the triggerResults channel */
    results->trapAction = actions->trapAction;
    results->logAction  = 0;
    switch (actions->trapAction)
    {
        case MBY_TRIG_ACTION_TRAP_TRAP:
            results->cpuCode = actions->trapCode;
            break;

        case MBY_TRIG_ACTION_TRAP_LOG:
            results->logAction = 1;
            break;

        case MBY_TRIG_ACTION_TRAP_REVERT: /* do not trap or log */
            if ( action == MBY_ACTION_TRAP )
            {
                results->action = pre_resolve_action;
                dmaskCopy(pre_resolve_dmask, results->destMask);
                is_action_drop_trig = isActionDropTrig(results->destMask);

                if ( actions->forwardingAction == MBY_TRIG_ACTION_FORWARDING_REDIRECT )
                    dmaskCopy(actions->newDestMask, results->destMask);
                else if ( actions->forwardingAction ==
                    MBY_TRIG_ACTION_FORWARDING_DROP )
                    for(fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
                        results->destMask[i] &= ~actions->dropMask[i];
                if ( is_action_drop_trig )
                    results->action = MBY_ACTION_DROP_TRIG;
            }
            break;

        default:
            break;
    }

    /* store the action in the triggerResults channel */
    results->qcnValid0 = qcn_mirror0_profile_v;
    results->qcnValid1 = qcn_mirror1_profile_v;

    results->mirroringAction0 = actions->mirroringAction0;
    results->mirroringAction1 = actions->mirroringAction1;

    if (actions->mirroringAction0 == MBY_TRIG_ACTION_MIRRORING_MIRROR) /* actions for mirrors */
    {
        results->mirror0ProfileV   = 1;
        results->mirror0ProfileIdx = actions->mirrorProfileIndex0;
        results->qcnValid0 = 0;
    }
    else if (actions->mirroringAction0 == MBY_TRIG_ACTION_MIRRORING_CANCEL) /* existing mirror canceled*/
        results->mirror0ProfileV = 0;

    else/* no change in mirroring disposition; leave mirrors as they are */
    {
        results->mirror0ProfileV   = mirror0_profile_v;
        results->mirror0ProfileIdx = mirror0_profile_idx;
    }

    if (actions->mirroringAction1 == MBY_TRIG_ACTION_MIRRORING_MIRROR) /* actions for mirrors */
    {
        results->mirror1ProfileV   = 1;
        results->mirror1ProfileIdx = actions->mirrorProfileIndex1;
        results->qcnValid1 = 0;
    }
    else if (actions->mirroringAction1 == MBY_TRIG_ACTION_MIRRORING_CANCEL) /* existing mirror canceled*/
        results->mirror1ProfileV = 0;

    else/* no change in mirroring disposition; leave mirrors as they are */
    {
        results->mirror1ProfileV   = mirror1_profile_v;
        results->mirror1ProfileIdx = mirror1_profile_idx;
    }

    /* store the action in the triggerResults channel */
    results->TCAction = actions->TCAction;

    results->TC = qos_tc;

    if ( actions->TCAction == MBY_TRIG_ACTION_TC_REASSIGN )
        results->TC = actions->newTC;

    /* XXX Real silicon determines the traffic class and the SMP membership
     * here. The HLP white model has moved this logic to the CM stage. */

    /* store the action in the triggerResults channel */
    results->vlanAction = actions->vlanAction;

    results->vlan = l2_evid1;

    if ( actions->vlanAction == MBY_TRIG_ACTION_VLAN_REASSIGN )
        results->vlan = actions->newVlan;

    /* store the action in the triggerResults channel */
    results->learningAction = actions->learningAction;

    if(actions->learningAction == MBY_TRIG_ACTION_LEARNING_DONT_LEARN)
        *learning_enabled = 0;
    else if(actions->learningAction == MBY_TRIG_ACTION_LEARNING_FORCE_LEARN)
        *learning_enabled = 1;

    /* store the action in the triggerResults channel */
    results->rateLimitAction = actions->rateLimitAction;

    if( actions->rateLimitAction == MBY_TRIG_ACTION_RATE_LIMIT_APPLY)
        results->rateLimitNum = actions->newRateLimitNum;

    /* store the action in the triggerResults channel */
    results->egressL3DomainAction = actions->egressL3DomainAction;

    if( actions->egressL3DomainAction == MBY_TRIG_ACTION_EGRESS_L3DOMAIN_ALWAYS_UPDATE)
        results->update_l3_domain = 1;

    /* store the action in the triggerResults channel */
    results->egressL2DomainAction = actions->egressL2DomainAction;

    if( actions->egressL2DomainAction == MBY_TRIG_ACTION_EGRESS_L2DOMAIN_ALWAYS_UPDATE)
        results->update_l2_domain = 1;

    /* store the action in the triggerResults channel */
    results->policerAction = actions->policerAction;
}

/**
 * Trigger Counters
 * Specified [here] (https://securewiki.ith.intel.com/display/25T/RX-PPE+Triggers#RX-PPETriggers-TriggerCountersandInterrupts)
 *
 * @param[in]     trig_apply_map            Trigger Registers map
 * @param[output] trig_apply_map_w          Trigger Registers write map
 * @param[in]     trig_hit_mask_resolved_lo Fired triggers from 0  - 63
 * @param[in]     trig_hit_mask_resolved_hi Fired triggers from 64 - 95
 * @param[in]     hitMaskHi                 Matched trigges from 64 - 95
 * @param[in]     hitMaskLo                 Matched trigges from 0  - 63
 */
static void triggersStatsUpdate
(
    mby_ppe_trig_apply_map       const * const trig_apply_map,
    mby_ppe_trig_apply_map__addr const * const trig_apply_map_w,
    fm_uint64                          * const trig_hit_mask_resolved_lo,
    fm_int64                           * const trig_hit_mask_resolved_hi,
    fm_uint64                            const hitMaskHi,
    fm_uint64                            const hitMaskLo
)
{
    for (fm_int i = 0; i < MBY_TRIGGERS_COUNT; i++)
    {
        if (i < 64)
        {
            if ( (hitMaskLo & (FM_LITERAL_U64(1) << i)) != 0 )
                if ( (*trig_hit_mask_resolved_lo & (FM_LITERAL_U64(1) << i)) )
                    incrementTrigCounter(trig_apply_map, trig_apply_map_w, i);
        } else
            if ( (hitMaskHi & (FM_LITERAL_U64(1) << (i - 64))) != 0 )
                if ( (*trig_hit_mask_resolved_hi & (FM_LITERAL_U64(1) << (i - 64))) )
                    incrementTrigCounter(trig_apply_map, trig_apply_map_w, i);
    }
}

/**
 * TCN FIFO
 * Specified [here] (https://securewiki.ith.intel.com/display/25T/MBY+PP+TCN+FIFO)
 *
 * @param[in]     trig_apply_misc_map   Trigger Registers map
 * @param[output] trig_apply_misc_map_w Trigger Registers write map
 * @param[in]     fwd_misc_map          Miscellaneous Registers in FWD map
 * @param[out]    fwd_misc_map_w        Miscellaneous Registers in FWD write map
 * @param[in]     mapper_map            Mapper Registers Set map
 * @param[in]     learn_en              LEARNING_ENABLED coming from MaskGen stage
 * @param[in]     l2_smac               L2_SMAC coming from MaskGen stage
 * @param[in]     l2_evid1              L2_EVID1 coming from MaskGen stage
 * @param[in]     l2_edomain            L2_EDOMAIN coming from MaskGen stage
 * @param[in]     rx_port               RX_PORT coming from MaskGen stage
 */
static void tcnFifo
(
    mby_ppe_trig_apply_misc_map       const * const trig_apply_misc_map,
    mby_ppe_trig_apply_misc_map__addr const * const trig_apply_misc_map_w,
    mby_ppe_fwd_misc_map              const * const fwd_misc_map,
    mby_ppe_fwd_misc_map__addr        const * const fwd_misc_map_w,
    mby_ppe_mapper_map                const * const mapper_map,
    fm_bool                                   const learning_enabled,
    fm_macaddr                                const l2_smac,
    fm_uint16                                 const l2_evid1,
    fm_byte                                   const l2_edomain,
    fm_uint32                                 const rx_port
)
{
    fm_uint64 head           = trig_apply_misc_map->MA_TCN_PTR_HEAD.HEAD;
    fm_uint64 tail           = trig_apply_misc_map->MA_TCN_PTR_TAIL.TAIL;
    fm_bool   port_learning  = fwd_misc_map->FWD_PORT_CFG_1[rx_port].LEARNING_ENABLE;
    fm_bool   port_l2_domain = mapper_map->MAP_DOMAIN_ACTION0[l2_edomain].LEARN_EN;
    fm_uint64 usage          = trig_apply_misc_map->MA_TCN_USAGE[rx_port].USAGE;
    fm_uint64 wm             = trig_apply_misc_map->MA_TCN_WM[rx_port].WM;
    fm_bool   dequeue        = trig_apply_misc_map->MA_TCN_DEQUEUE.READY;

    if(learning_enabled && port_learning && port_l2_domain && usage < wm)
    {
        if((tail + 1) % (MBY_MA_TCN_FIFO_CAPACITY + 1) == head)
        {
            write_field(trig_apply_misc_map_w->MA_TCN_IP.TCN_OVERFLOW, 1);
        }
        else
        {
            write_field(trig_apply_misc_map_w->MA_TCN_FIFO_0[tail].MAC_ADDRESS, l2_smac);
            write_field(trig_apply_misc_map_w->MA_TCN_FIFO_0[tail].PORT,        rx_port);
            write_field(trig_apply_misc_map_w->MA_TCN_FIFO_1[tail].L2_DOMAIN,   l2_edomain);
            write_field(trig_apply_misc_map_w->MA_TCN_FIFO_1[tail].VID,         l2_evid1);

            tail = (tail + 1) % (MBY_MA_TCN_FIFO_CAPACITY + 1);
            write_field(trig_apply_misc_map_w->MA_TCN_PTR_TAIL.TAIL,     tail);
            write_field(trig_apply_misc_map_w->MA_TCN_IP.PENDING_EVENTS, 1);
        }
    }

    if(dequeue)
    {
        write_field(trig_apply_misc_map_w->MA_TCN_DATA_0.MAC_ADDRESS, trig_apply_misc_map->MA_TCN_FIFO_0[head].MAC_ADDRESS);
        write_field(trig_apply_misc_map_w->MA_TCN_DATA_0.PORT,        trig_apply_misc_map->MA_TCN_FIFO_0[head].PORT);
        write_field(trig_apply_misc_map_w->MA_TCN_DATA_1.L2_DOMAIN,   trig_apply_misc_map->MA_TCN_FIFO_1[head].L2_DOMAIN);
        write_field(trig_apply_misc_map_w->MA_TCN_DATA_1.VID,         trig_apply_misc_map->MA_TCN_FIFO_1[head].VID);

        head = (head + 1) % (MBY_MA_TCN_FIFO_CAPACITY + 1);
        write_field(trig_apply_misc_map_w->MA_TCN_DEQUEUE.READY, 0);
    }

    if ((trig_apply_misc_map->MA_TCN_IP.PENDING_EVENTS & ~trig_apply_misc_map->MA_TCN_IM.PENDING_EVENTS) ||
        (trig_apply_misc_map->MA_TCN_IP.TCN_OVERFLOW   & ~trig_apply_misc_map->MA_TCN_IM.TCN_OVERFLOW  ))
        write_field(fwd_misc_map_w->FWD_IP.MA_TCN, 1);
}

static void glortLookupDeterministicMode
(
    mby_ppe_fwd_misc_map const * const fwd_misc,
    fm_bool                      const targeted_deterministic,
    fm_uint32                    const hash_rot_a,
    fm_uint32                    const hash_rot_b,
    mbyTriggerResults    const * const triggers,
    fm_uint64                  * const dmask_o,
    fm_uint32                  * const action_o
)
{
    // GLORT Lookup - strict targeted deterministic mode:
    fm_bool skip_lookup
        = (((*action_o == MBY_ACTION_REDIRECT_TRIG) && !triggers->filterDestMask) ||
           ((*action_o != MBY_ACTION_REDIRECT_TRIG) &&  targeted_deterministic));

    if (!skip_lookup && isDmask(dmask_o))
    {
        for (fm_uint i = 0; i < MBY_MAX_FABRIC_LAG_PORT; i++) // REVISIT !!!! MBY_MAX_FABRIC_LAG_PORT ???
        {
            mbyFwdLagCfg lag_cfg;
            lag_cfg = getLagCfg(fwd_misc, i);
            if (!lag_cfg.IN_LAG)
                continue;

            fm_uint32 hash = (lag_cfg.HASH_ROTATION) ? hash_rot_b : hash_rot_a;
            hash %= (lag_cfg.LAG_SIZE == 0) ? 16 : lag_cfg.LAG_SIZE; //REVISIT !!!! 0..16 LAG porta???
            if (hash != lag_cfg.INDEX)
                dmask_o[i / 64] &= ~(FM_LITERAL_U64(1) << (i % 64)); //!!!REVISIT DMAS 257bit
        }

        if (!isDmask(dmask_o))
            *action_o = MBY_ACTION_BANK5_OTHER_DROPS;
    }
}

static void loopbackSuppressionFiltering
(
    mby_ppe_cm_apply_map const * const cm_apply,
    fm_bool                      const targeted_deterministic,
    fm_bool                      const mark_routed,
    fm_uint16                    const csglort,
    fm_uint32                  * const action_o,
    fm_uint64                  * const dmask_o
)
{
    fm_bool skip_suppress = targeted_deterministic || mark_routed || (!isDmask(dmask_o));

    //REVISIT!!! should be MBY_DEST_PORTS_COUNT
    // (MBY_DEST_PORTS_COUNT - 1) It does not need to be [0..256] because the CPU port is never a member of a LAG
    for (fm_uint i = 0; !skip_suppress && (i < MBY_FABRIC_LOG_PORTS - 1); i++)
    {
        cm_apply_loopback_suppress_r const * const lpbk_sup = &(cm_apply->CM_APPLY_LOOPBACK_SUPPRESS[i]);

        fm_uint16 lpbk_glort_mask = lpbk_sup->GLORT_MASK;
        fm_uint16 lpbk_glort      = lpbk_sup->GLORT;
        if ((csglort & lpbk_glort_mask) == lpbk_glort)
            dmask_o[i / 64] &= ~(FM_LITERAL_U64(1) << (i % 64));
    }

    if (!isDmask(dmask_o))
        *action_o = MBY_ACTION_DROP_LOOPBACK; // dropping frame
}

static void handleTraps
(
    mby_ppe_cm_apply_map const * const cm_apply,
    fm_byte                      const operator_id,
    fm_bool                      const store_trap_action,
    mbyTriggerResults    const * const triggers,
    fm_uint64                  * const dmask_o,
    fm_uint16                  * const idglort_o,
    fm_byte                    * const cpu_code_o,
    fm_uint32                  * const action_o,
    fm_bool                    * const mark_routed_o,
    fm_uint16                  * const ip_mcast_idx_o
)
{
    *idglort_o = cm_apply->CM_APPLY_TRAP_GLORT[operator_id].TRAP_GLORT;

    if(triggers->trapAction == MBY_TRIG_ACTION_TRAP_TRAP)
        *cpu_code_o = MBY_TRIGGER_TRAP_ACTION_CODE | triggers->cpuCode;

    *action_o       = MBY_ACTION_TRAP;
    *mark_routed_o  = FALSE;
    *ip_mcast_idx_o = 0;

    //!!! REVISIT RDL changes needed here
    for(fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
        dmask_o[i] = cm_apply->CM_APPLY_CPU_TRAP_MASK.DEST_MASK;

    if(!isDmask(dmask_o))
        *action_o = MBY_ACTION_BANK5_OTHER_DROPS;
}

static void handleLogging
(
    mby_ppe_cm_apply_map const * const cm_apply,
    mbyTriggerResults    const * const triggers,
    fm_byte                    * const log_amask_o,
    fm_bool                    * const logging_hit_o,
    fm_uint32                  * const mirror0_profile_idx_o,
    fm_byte                    * const mirror0_profile_v_o
)
{
    fm_uint64 log_mask                = 0;
    fm_uint32 log_mirror0_profile_idx = 0;

    if(triggers->logAction)
        *log_amask_o |= MBY_LOG_TYPE_TRIG_LOG_ACTION;

    for (fm_uint i = 0; i < MBY_AMASK_WIDTH; i++) {
        log_mask = FM_LITERAL_U64(1) << i;
        if (*log_amask_o & log_mask) {
            *log_amask_o = log_mask;
            break;
        }
    }

    if(log_mask != 0)
    {
        *logging_hit_o = TRUE;

        switch(log_mask)
        {
            case MBY_LOG_TYPE_TRIG_LOG_ACTION:
                log_mirror0_profile_idx = cm_apply->CM_APPLY_LOG_MIRROR_PROFILE.TRIGGER;
                break;
            case MBY_LOG_TYPE_CGRP:
                log_mirror0_profile_idx = cm_apply->CM_APPLY_LOG_MIRROR_PROFILE.FFU;
                break;
            case MBY_LOG_TYPE_RESERVED_MAC:
                log_mirror0_profile_idx = cm_apply->CM_APPLY_LOG_MIRROR_PROFILE.RESERVED_MAC;
                break;
            case MBY_LOG_TYPE_ARP_REDIRECT:
                log_mirror0_profile_idx = cm_apply->CM_APPLY_LOG_MIRROR_PROFILE.ARP_REDIRECT;
                break;
            case MBY_LOG_TYPE_ICMP:
                log_mirror0_profile_idx = cm_apply->CM_APPLY_LOG_MIRROR_PROFILE.ICMP;
                break;
            default:
                *logging_hit_o = FALSE;
        }
    }

    if((!triggers->mirror0ProfileV || (triggers->mirror0ProfileV && triggers->qcnValid0)) && *logging_hit_o)
    {
        *mirror0_profile_v_o   = TRUE;
        *mirror0_profile_idx_o = log_mirror0_profile_idx;
    }
}

static void trap
(
    mby_ppe_cm_apply_map const * const cm_apply,
    mbyFwdSysCfg1        const * const sys_cfg1,
    fm_byte                      const operator_id,
    fm_bool                      const store_trap_action,
    mbyTriggerResults    const * const triggers,
    fm_uint64                  * const dmask,
    fm_byte                    * const log_amask_o,
    fm_bool                    * const logging_hit_o,
    fm_bool                    * const cpu_trap_o,
    fm_byte                    * const l2_edomain_o,
    fm_byte                    * const l3_edomain_o,
    fm_uint32                  * const mirror0_profile_idx_o,
    fm_byte                    * const mirror0_profile_v_o,
    fm_uint64                  * const fnmask_o,
    fm_uint16                  * const idglort_o,
    fm_byte                    * const cpu_code_o,
    fm_uint32                  * const action_o,
    fm_bool                    * const mark_routed_o,
    fm_uint16                  * const ip_mcast_idx_o
)
{
    fm_bool cpu_trap     = (*action_o == MBY_ACTION_TRAP);

    fm_bool trap_trap    = (triggers->trapAction == MBY_TRIG_ACTION_TRAP_TRAP);
    fm_bool trap_revert  = (triggers->trapAction == MBY_TRIG_ACTION_TRAP_REVERT);
    fm_bool trap_log     = (triggers->trapAction == MBY_TRIG_ACTION_TRAP_LOG);

    *l2_edomain_o = (triggers->egressL2DomainAction == 0) ? *l2_edomain_o : 0;
    *l3_edomain_o = (triggers->egressL3DomainAction == 0) ? *l3_edomain_o : 0;

    if ((cpu_trap || trap_trap) && !trap_revert)
        handleTraps
        (
            cm_apply,
            operator_id,
            store_trap_action,
            triggers,
            dmask,
            idglort_o,
            cpu_code_o,
            action_o,
            mark_routed_o,
            ip_mcast_idx_o
        );

    if ((sys_cfg1->ENABLE_TRAP_PLUS_LOG || !cpu_trap || trap_trap || trap_log) && !trap_revert)
        handleLogging
        (
            cm_apply,
            triggers,
            log_amask_o,
            logging_hit_o,
            mirror0_profile_idx_o,
            mirror0_profile_v_o
        );

    dmaskCopy(fnmask_o, dmask);

    // output values
    *cpu_trap_o   = cpu_trap;
}

static void tailProcessing
(
    fm_uint32   const rx_length,
    fm_uint32   const mirror0_port,
    fm_uint32   const mirror1_port,
    fm_bool     const pa_drop,
    fm_bool     const pa_l3len_err,
    fm_bool   * const mirror0_profile_v_o,
    fm_bool   * const mirror1_profile_v_o,
    fm_byte   * const seg_meta_err_o,
    fm_bool   * const saf_error_o,
    fm_bool   * const tx_drop_o,
    fm_uint32 * const action_o,
    fm_uint64 * const dmask_o,
    fm_uint64 * const fnmask_o
)
{
    // remove FCS bytes from array storage:
    fm_uint32 pkt_len  = (rx_length < 4) ? 0 : (rx_length - 4);
    fm_uint32 num_segs = 0;

    // First segment is 192 bytes
    if (pkt_len <= 192) {
        pkt_len   = 0;
        num_segs  = 1;
    } else {
        pkt_len  -= 192;
        num_segs += 1 + (pkt_len + MBY_SEGMENT_LEN - 1) / MBY_SEGMENT_LEN; // check <-- REVISIT!!!
    }

    dmaskCopy(fnmask_o, dmask_o);

    if (*mirror0_profile_v_o)
        dmask_o[mirror0_port / 64] |= (FM_LITERAL_U64(1) << (mirror0_port % 64));


    if (*mirror1_profile_v_o)
        dmask_o[mirror1_port / 64] |= (FM_LITERAL_U64(1) << (mirror1_port % 64));

    for(fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
        dmask_o[i] &= ( 0xFFFFFFFFFFFF ); // 48 bit port mask ????

    fm_bool saf_error    = FALSE;
    fm_bool tx_drop      = FALSE;

    // Update Action code for CSUM and L3 Length errors
    // Applies to only single segment packets. Multi-segment packets are handled by Modify
    if (rx_length <= 192)
    {
        if (*action_o == MBY_ACTION_NORMAL            ||
            *action_o == MBY_ACTION_FLOOD             ||
            *action_o == MBY_ACTION_GLORT_FORWARDED   ||
            *action_o == MBY_ACTION_TRAP              ||
            *action_o == MBY_ACTION_SPECIAL           ||
            *action_o == MBY_ACTION_REDIRECT_TRIG     ||
            *action_o == MBY_ACTION_DROP_CONTROL      ||
            *action_o == MBY_ACTION_DROP_IV           ||
            *action_o == MBY_ACTION_DROP_EV           ||
            *action_o == MBY_ACTION_DROP_STP          ||
            *action_o == MBY_ACTION_DROP_CAM          ||
            *action_o == MBY_ACTION_DROP_CGRP         ||
            *action_o == MBY_ACTION_DROP_TRIG         ||
            *action_o == MBY_ACTION_DROP_TTL          ||
            *action_o == MBY_ACTION_DROP_DLF          ||
            *action_o == MBY_ACTION_BANK5_OTHER_DROPS ||
            *action_o == MBY_ACTION_DROP_SV)
        {
            if (pa_drop && pa_l3len_err)
                *action_o = MBY_ACTION_DROP_L3_PYLD_LEN;
        }

        // Drop single-segment packets with l4csum error /l3 length error:
        if (*action_o == MBY_ACTION_DROP_L3_PYLD_LEN ||
            *action_o == MBY_ACTION_DROP_L4_CSUM)
        {
            dmaskClear(fnmask_o);
            *mirror1_profile_v_o = 0;
            *mirror0_profile_v_o = 0;
        }
    }
    else if (pa_drop && pa_l3len_err)
        *seg_meta_err_o = 2; // framing error in multi-segment packet

    // output values
    *saf_error_o    = saf_error;
    *tx_drop_o      = tx_drop;
}

static void maskGenUpdate
(
    //in
    mby_ppe_fwd_misc_map const * const fwd_misc,
    mby_ppe_cm_apply_map const * const cm_apply,
    fm_bool                      const targeted_deterministic,
    fm_uint32                    const hash_rot_a,
    fm_uint32                    const hash_rot_b,
    fm_uint16                    const csglort,
    fm_uint32                    const rx_length,
    fm_uint32                    const mirror0_port,
    fm_uint32                    const mirror1_port,
    fm_bool                      const pa_drop,
    fm_bool                      const pa_l3len_err,
    fm_byte                      const operator_id,
    fm_bool                      const store_trap_action,
    mbyTriggerResults    const * const triggers,
    //in-out
    fm_uint64                  * const dmask,
    fm_uint32                  * const action,
    fm_byte                    * const cpu_code,
    fm_uint16                  * const idglort,
    fm_uint64                  * const fnmask,
    fm_byte                    * const log_amask,
    fm_bool                    * const logging_hit,
    fm_byte                    * const seg_meta_err,
    fm_bool                    * const mark_routed,
    fm_uint16                  * const ip_mcast_idx,
    fm_uint32                  * const mirror0_profile_idx,
    fm_bool                    * const mirror0_profile_v,
    fm_bool                    * const mirror1_profile_v,
    //out
    fm_bool                    * const cpu_trap,
    fm_byte                    * const l2_edomain,
    fm_byte                    * const l3_edomain,
    fm_bool                    * const saf_error,
    fm_bool                    * const tx_drop
)
{
    mbyFwdSysCfg1 sys_cfg1;
    sys_cfg1 = getSysCfg1(fwd_misc);
    dmaskCopy(dmask, fnmask);

    // --------------------------------------------------------------------------------
    // LAG (Lag Filtering):
    // GLORT Lookup - strict targeted deterministic mode:
    glortLookupDeterministicMode
    (
        fwd_misc,
        targeted_deterministic,
        hash_rot_a,
        hash_rot_b,
        triggers,
        dmask,
        action
    );

    // --------------------------------------------------------------------------------
    // Loopback Suppression Filtering (Loopback_suppress(2)):
    loopbackSuppressionFiltering
    (
        cm_apply,
        targeted_deterministic,
        *mark_routed,
        csglort,
        action,
        dmask
    );

    // --------------------------------------------------------------------------------
    // Trap (Trapping):
    trap
    (
        cm_apply,
        &sys_cfg1,
        operator_id,
        store_trap_action,
        triggers,
        dmask,
        log_amask,
        logging_hit,
        cpu_trap,
        l2_edomain,
        l3_edomain,
        mirror0_profile_idx,
        mirror0_profile_v,
        fnmask,
        idglort,
        cpu_code,
        action,
        mark_routed,
        ip_mcast_idx
    );

    // --------------------------------------------------------------------------------
    // Tail processing (From Stats block):
    tailProcessing
    (
        rx_length,
        mirror0_port,
        mirror1_port,
        pa_drop,
        pa_l3len_err,
        mirror0_profile_v,
        mirror1_profile_v,
        seg_meta_err,
        saf_error,
        tx_drop,
        action,
        dmask,
        fnmask
    );
}

void Triggers
(
    mby_ppe_trig_apply_map            const * const trig_apply_map,
    mby_ppe_trig_apply_map__addr      const * const trig_apply_map_w,
    mby_ppe_trig_apply_misc_map       const * const trig_apply_misc_map,
    mby_ppe_trig_apply_misc_map__addr const * const trig_apply_misc_map_w,
    mby_ppe_fwd_misc_map              const * const fwd_misc_map,
    mby_ppe_fwd_misc_map__addr        const * const fwd_misc_map_w,
    mby_ppe_cm_apply_map              const * const cm_apply_map,
    mby_ppe_mapper_map                const * const mapper_map,
    mbyMaskGenToTriggers              const * const in,
    mbyTriggersToCongMgmt                   * const out
)
{
    // Read inputs from the MaskGen:
    fm_bool           const learn_en                  = in->LEARNING_ENABLED;
    fm_uint16         const l2_evid1                  = in->L2_EVID1;
    fm_byte           const cgrp_trig                 = in->CGRP_TRIG;
    fm_byte                 cpu_code                  = in->CPU_CODE;
    fm_bool                 cpu_trap                  = in->CPU_TRAP;
    fm_uint16         const csglort                   = in->CSGLORT;
    fm_uint32         const hash_rot_a                = in->HASH_ROT_A;
    fm_uint32         const hash_rot_b                = in->HASH_ROT_B;
    fm_byte           const qos_tc                    = in->QOS_TC;
    fm_uint16               idglort                   = in->IDGLORT;
    fm_byte           const operator_id               = in->OPERATOR_ID;
    fm_byte                 log_amask                 = in->LOG_AMASK;
    fm_bool                 logging_hit               = in->LOGGING_HIT;
    fm_byte                 l3_edomain                = in->L3_EDOMAIN;
    fm_byte                 l2_edomain                = in->L2_EDOMAIN;
    fm_uint16               ip_mcast_idx              = in->IP_MCAST_IDX;
    fm_byte           const fclass                    = in->FCLASS;
    fm_bool                 mark_routed               = in->MARK_ROUTED;
    fm_uint64 const * const dmask                     = in->DMASK;
    fm_macaddr        const l2_smac                   = in->L2_SMAC;
    fm_uint32         const rx_port                   = in->RX_PORT;
    fm_uint32         const rx_length                 = in->RX_LENGTH;
    fm_uint64         const amask                     = in->AMASK;
    fm_uint32         const action                    = in->ACTION;
    fm_uint32         const pre_resolve_action        = in->PRE_RESOLVE_ACTION;
    fm_uint16         const pre_resolve_dglort        = in->PRE_RESOLVE_DGLORT;
    fm_uint64 const * const pre_resolve_dmask         = in->PRE_RESOLVE_DMASK;
    fm_bool           const qcn_mirror0_profile_v     = in->QCN_MIRROR0_PROFILE_V;
    fm_bool           const qcn_mirror1_profile_v     = in->QCN_MIRROR1_PROFILE_V;
    fm_bool           const store_trap_action         = in->STORE_TRAP_ACTION;
    fm_uint32               mirror0_profile_idx       = in->MIRROR0_PROFILE_IDX;
    fm_byte                 mirror0_profile_v         = in->MIRROR0_PROFILE_V;
    fm_uint32         const mirror1_profile_idx       = in->MIRROR1_PROFILE_IDX;
    fm_byte                 mirror1_profile_v         = in->MIRROR1_PROFILE_V;
    fm_bool                 learning_enabled          = in->LEARNING_ENABLED;
    fm_bool           const targeted_deterministic    = in->TARGETED_DETERMINISTIC;
    fm_uint32         const mirror0_port              = in->MIRROR0_PORT;
    fm_uint32         const mirror1_port              = in->MIRROR1_PORT;
    fm_bool           const pa_drop                   = in->PA_DROP;
    fm_bool           const pa_l3len_err              = in->PA_L3LEN_ERR;
    fm_bool                 seg_meta_err              = in->SEG_META_ERR;

    /* no_modify action comes from triggers. */
    fm_bool                 hit;
    fm_uint64               hit_mask_hi               = FM_LITERAL_U64(0);
    fm_uint64               hit_mask_lo               = FM_LITERAL_U64(0);
    fm_uint64               trig_hit_mask_resolved_lo = FM_LITERAL_U64(0);
    fm_int64                trig_hit_mask_resolved_hi = FM_LITERAL_U64(0);
    mbyTriggerActions       actions                   = { 0 };
    mbyTriggerResults       results                   = { 0 };

    /* maskgen variables */
    fm_uint64               fnmask[MBY_DMASK_REGISTERS] = { 0 };
    fm_bool                 saf_error                   = FALSE;
    fm_bool                 tx_drop                     = FALSE;

    /* EvaluateTrigger first */
    for (fm_int i = 0; i < MBY_TRIGGERS_COUNT; i++)
    {
        hit =   evaluateTrigger
                (
                    trig_apply_map,
                    i,
                    learn_en,
                    l2_evid1,
                    cgrp_trig,
                    qos_tc,
                    idglort,
                    l3_edomain,
                    l2_edomain,
                    fclass,
                    mark_routed,
                    dmask,
                    rx_port,
                    amask
                );

        if (hit)
        {
            if (i < 64)
                hit_mask_lo |= FM_LITERAL_U64(1) << i;
            else
                hit_mask_hi |= FM_LITERAL_U64(1) << (i - 64);
        }
    }

    resolveTriggers
    (
        trig_apply_map,
        hit_mask_hi,
        hit_mask_lo,
        &trig_hit_mask_resolved_lo,
        &trig_hit_mask_resolved_hi,
        &actions
    );

    applyTriggers
    (
        &actions,
        &results,
        action,
        dmask,
        pre_resolve_action,
        pre_resolve_dmask,
        pre_resolve_dglort,
        idglort,
        qcn_mirror0_profile_v,
        qcn_mirror1_profile_v,
        mirror0_profile_idx,
        mirror0_profile_v,
        mirror1_profile_idx,
        mirror1_profile_v,
        qos_tc,
        l2_evid1,
        &learning_enabled
    );

    triggersStatsUpdate
    (
        trig_apply_map,
        trig_apply_map_w,
        &trig_hit_mask_resolved_lo,
        &trig_hit_mask_resolved_hi,
        hit_mask_hi,
        hit_mask_lo
    );

    // events and interrupts
    fm_uint64 trig_ip_lo = trig_apply_misc_map->TRIGGER_IP[0].PENDING;
    trig_ip_lo          |= ((FM_LITERAL_U64(1) << 48) - 1) & trig_hit_mask_resolved_lo;
    write_field(trig_apply_misc_map_w->TRIGGER_IP[0].PENDING, trig_ip_lo);

    fm_uint64 trig_ip_hi     = trig_apply_misc_map->TRIGGER_IP[1].PENDING;
    fm_uint64 new_trig_ip_hi = 0;

    /*take upper 16 bits of resolved_hit_lo*/
    FM_SET_UNNAMED_FIELD64(new_trig_ip_hi, 0, 16, trig_hit_mask_resolved_lo >> 48);
    /*take lower 36 bits of resolved_hit_hi*/
    FM_SET_UNNAMED_FIELD64(new_trig_ip_hi, (64 - 48), 36,
                           ((FM_LITERAL_U64(1) << 36) - 1) & trig_hit_mask_resolved_hi);
    trig_ip_hi |= new_trig_ip_hi;
    write_field(trig_apply_misc_map_w->TRIGGER_IP[1].PENDING, trig_ip_hi);

    /* propagate trigger interrupts to FWD_IP and GLOBAL_INTERRUPT regs */
    fm_uint64 trig_im_lo = FM_LITERAL_U64(0);
    fm_uint64 trig_im_hi = FM_LITERAL_U64(0);
    trig_im_lo           = trig_apply_misc_map->TRIGGER_IM[0].MASK;
    trig_im_hi           = trig_apply_misc_map->TRIGGER_IM[1].MASK;

    write_field(fwd_misc_map_w->FWD_IP.TRIGGER,
                (((trig_ip_hi & ~trig_im_hi) != 0) | ((trig_ip_lo & ~trig_im_lo) != 0)));

    // <--- REVISIT!!!! global_interrupt
    /*
    fm_bool fwd_ip_trig        = fwd_misc_map->FWD_IP.TRIGGER;
    fm_bool fwd_ip_entry_count = fwd_misc_map->FWD_IP.ENTRY_COUNT;

    fm_bool fwd_im_trig        = fwd_misc_map->FWD_IM.TRIGGER;
    fm_bool fwd_im_entry_count = fwd_misc_map->FWD_IM.ENTRY_COUNT;

    fm_byte global_int_fwd = 0;

    if ((fwd_ip_entry_count & ~fwd_im_entry_count) || (fwd_ip_trig & ~fwd_im_trig))
        global_int_fwd = 1;

    if ((fwd_ip_entry_count & ~fwd_im_entry_count) == 0 && (fwd_ip_trig & ~fwd_im_trig) == 0)
        global_int_fwd = 0;
    */
    //regPtr = FM_MODEL_GET_REG_PTR(model, HLP_GLOBAL_INTERRUPT(0));
    //FM_ARRAY_SET_BIT(regPtr, HLP_GLOBAL_INTERRUPT, FWD, globalIntFwd);

    tcnFifo
    (
        trig_apply_misc_map,
        trig_apply_misc_map_w,
        fwd_misc_map,
        fwd_misc_map_w,
        mapper_map,
        learning_enabled,
        l2_smac,
        l2_evid1,
        l2_edomain,
        rx_port
    );

    fm_uint32 msk_gen_action = results.action;

    maskGenUpdate
    (
        fwd_misc_map,
        cm_apply_map,
        targeted_deterministic,
        hash_rot_a,
        hash_rot_b,
        csglort,
        rx_length,
        mirror0_port,
        mirror1_port,
        pa_drop,
        pa_l3len_err,
        operator_id,
        store_trap_action,
        &results,
        results.destMask,
        &msk_gen_action,
        &cpu_code,
        &idglort,
        fnmask,
        &log_amask,
        &logging_hit,
        &seg_meta_err,
        &mark_routed,
        &ip_mcast_idx,
        &mirror0_profile_idx,
        &mirror0_profile_v,
        &mirror1_profile_v,
        &cpu_trap,
        &l2_edomain,
        &l3_edomain,
        &saf_error,
        &tx_drop
    );

    // --------------------------------------------------------------------------------
    // Write outputs:

    for(fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
        out->DMASK [i] = results.destMask[i];

    out->ACTION              = msk_gen_action;
    out->CPU_CODE            = cpu_code;
    out->CPU_TRAP            = cpu_trap;
    out->IDGLORT             = results.destGlort;
    out->FNMASK              = fnmask[0]; //!!! REVISIT temporary solution
    out->L2_EVID1            = results.vlan;
    out->LEARNING_ENABLED    = learning_enabled;
    out->MARK_ROUTED         = mark_routed;
    out->MIRROR0_PROFILE_IDX = results.mirror0ProfileIdx;
    out->MIRROR0_PROFILE_V   = results.mirror0ProfileV;
    out->MIRROR1_PROFILE_IDX = results.mirror1ProfileIdx;
    out->MIRROR1_PROFILE_V   = results.mirror1ProfileV;
    out->RX_LENGTH           = rx_length;
    out->SAF_ERROR           = saf_error;
    out->TRAP_CODE           = results.trapCode;
    out->TX_DROP             = tx_drop;
    out->QOS_TC              = results.TC;
    out->UPDATE_L2_DOMAIN    = results.update_l2_domain;
    out->UPDATE_L3_DOMAIN    = results.update_l3_domain;
    // Pass thru:
    out->CONTENT_ADDR        = in->CONTENT_ADDR;
    out->DROP_TTL            = in->DROP_TTL;
    out->ECN                 = in->ECN;
    out->EDGLORT             = in->EDGLORT;
    out->IS_IPV4             = in->IS_IPV4;
    out->IS_IPV6             = in->IS_IPV6;
    out->IS_TIMEOUT          = in->IS_TIMEOUT;
    out->L2_DMAC             = in->L2_DMAC;
    out->L2_IVLAN1_CNT       = in->L2_IVLAN1_CNT;
    out->MCAST_EPOCH         = in->MCAST_EPOCH;
    out->MIRTYP              = in->MIRTYP;
    out->MOD_IDX             = in->MOD_IDX;
    out->MOD_PROF_IDX        = in->MOD_PROF_IDX;
    out->OOM                 = in->OOM;
    out->PARSER_INFO         = in->PARSER_INFO;
    out->PA_HDR_PTRS         = in->PA_HDR_PTRS;
    out->PM_ERR              = in->PM_ERR;
    out->PM_ERR_NONSOP       = in->PM_ERR_NONSOP;
    out->QOS_L3_DSCP         = in->QOS_L3_DSCP;
    out->RX_LENGTH           = in->RX_LENGTH;
    out->RX_PORT             = in->RX_PORT;
    out->SEG_META_ERR        = in->SEG_META_ERR;
    out->TAIL_CSUM_LEN       = in->TAIL_CSUM_LEN;
    out->TRAFFIC_CLASS       = in->TRAFFIC_CLASS;
    out->TX_TAG              = in->TX_TAG;
    out->XCAST               = in->XCAST;
    out->RX_LENGTH           = in->RX_LENGTH;
}
