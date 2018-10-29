// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_maskgen.h"
#include "mby_triggers.h"

static inline void incrementTrigCounter
(
    mby_ppe_trig_apply_map * const trig_apply_map,
    fm_byte                  const trig
)
{
    mbyTriggerStats trig_stats;

    trig_stats.COUNT = trig_apply_map->TRIGGER_STATS[trig].COUNT; // [63:0]
    trig_stats.COUNT = (trig_stats.COUNT == FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF)) ? 0 : trig_stats.COUNT + 1;
    trig_apply_map->TRIGGER_STATS[trig].COUNT = trig_stats.COUNT;
}

static fm_bool evaluateTrigger
(
    mby_ppe_trig_apply_map      * const trig_apply_map,
//  mby_ppe_trig_apply_misc_map * const trig_apply_misc_map,
//  mby_ppe_trig_usage_map      * const trig_usage_map,
    fm_int                        const trig,
    fm_uint16                   * const l2_evid1,
    fm_byte                     * const qos_swpri,
    fm_uint16                   * const idglort,
    fm_byte                     * const l2_edomain,
    fm_byte                     * const fclass,
    fm_bool                     * const mark_routed,
    fm_uint32                   * const dmask,
    fm_uint32                   * const rx_port,
    fm_uint64                   * const amask
)
{
    mbyTriggerConditionCfg          cond_cfg;
    mbyTriggerConditionCGRP         cond_cgrp;
    mbyTriggerConditionParam        cond_param;
    mbyTriggerConditionGlort        cond_glort;
    mbyTriggerConditionRx           cond_rx;
    mbyTriggerConditionAmask1       cond_amask1;
    mbyTriggerConditionAmask2       cond_amask2;
    fm_uint16                       egress_domain = 0;
    fm_bool                         cgrp_hit;
    fm_bool                         dglort_hit;
    fm_bool                         egress_domain_hit;
    fm_bool                         r_hit;
    fm_bool                         hit = TRUE;
    fm_uint32                       trig_lfsr0 = 0;
    fm_uint32                       trig_lfsr1 = 0;

    fm_uint32                       src_port_mask;
    fm_uint32                       dest_port_mask = 0;
    fm_uint64                       action_mask;
    fm_uint64                       action_mask2;

    cond_cfg   = mbyTrigGetConditionCfg   (trig_apply_map, trig);
    cond_cgrp  = mbyTrigGetConditionCGRP  (trig_apply_map, trig);
    cond_param = mbyTrigGetConditionParam (trig_apply_map, trig);
    cond_glort = mbyTrigGetConditionGlort (trig_apply_map, trig);

    //GetConditionType(model, trig, &condType); <- it's removed
    //GetConditionMetadata(model, trig, &condMetadata); <- it's removed

    //This code doesn't match RTL. So DV should not enable WM for matchRandom
    //tests
    // <--- REVISIT!!!!
    //trig_lfsr0 = FM_GET_UNNAMED_FIELD(fmRand(), 0, 24);
    //trig_lfsr1 = FM_GET_UNNAMED_FIELD(fmRand(), 0, 24);

    /* Match on the source MAC address lookup result. */
    /* MATCH_SA - removed from TRIGGER_CONDITION_CFG register */

    /* Match on a source MAC address lookup hit. */
    /* MATCH_HIT_SA - removed from TRIGGER_CONDITION_CFG register */

    /* RRC bug 23763 */
    /* MATCH_HIT_DA - removed from TRIGGER_CONDITION_CFG register */

    /* Match on the destination MAC address lookup result. */
    /* MATCH_DA - removed from TRIGGER_CONDITION_CFG and TRIGGER_CONDITION_PARAM register */

    /* Match on a destination MAC address lookup hit. */
    /* MATCH_HIT_DA - removed from TRIGGER_CONDITION_CFG */

    /* Match on a destination or source MAC address lookup hit. */
    /* MATCH_HIT_SADA - removed from TRIGGER_CONDITION_CFG */

    /* Match on the egress VLAN ID. */
    hit &= ( ( ( *l2_evid1 !=   cond_param.VID_ID ) &&
               (   cond_cfg.MATCH_VLAN == 0 ) ) ||
             ( ( *l2_evid1 ==   cond_param.VID_ID ) &&
               (   cond_cfg.MATCH_VLAN == 1 ) ) ||
             (   cond_cfg.MATCH_VLAN == 2 ) );

    /* Match on the FFU lookup result. */
    // <--- REVISIT!!!!
    //state->FFU_TRIG &
    cgrp_hit = ( (cond_cgrp.CGRP_MASK) ==
               (  cond_cgrp.CGRP_ID & cond_cgrp.CGRP_MASK) );
    hit &= ( ( !cgrp_hit && (   cond_cfg.MATCH_CGRP == 0 ) ) ||
             ( cgrp_hit && (   cond_cfg.MATCH_CGRP == 1 ) ) ||
             (   cond_cfg.MATCH_CGRP == 2 ) );

    /* Match on the traffic class (TC). */
    hit &= ( ( ( *qos_swpri !=   cond_param.TC ) &&
               (   cond_cfg.MATCH_TC == 0 ) ) ||
             ( ( *qos_swpri ==   cond_param.TC ) &&
               (   cond_cfg.MATCH_TC == 1 ) ) ||
             (   cond_cfg.MATCH_TC == 2 ) );

    /* Match on the Ethernet type. */
    /* removed register */

    /* Match on the destination GLORT. */
    dglort_hit = ( (*idglort &   cond_glort.GLORT_MASK) ==
                  (  cond_glort.DEST_GLORT & cond_glort.GLORT_MASK) );
    hit &= ( ( !dglort_hit && (   cond_cfg.MATCH_DEST_GLORT == 0 ) ) ||
             ( dglort_hit && (   cond_cfg.MATCH_DEST_GLORT == 1 ) ) ||
             (   cond_cfg.MATCH_DEST_GLORT == 2 ) );

    /* Match on EgressDomainValue. */
    egress_domain = ((*l2_edomain & 0x3f) << 9) | (*l2_edomain & 0x1ff);
    egress_domain_hit = ( (egress_domain &   cond_param.EGRESS_DOMAIN_MASK) ==
                            (  cond_param.EGRESS_DOMAIN_VALUE & cond_param.EGRESS_DOMAIN_MASK) );
    hit &= ( ( !egress_domain_hit && (   cond_cfg.MATCH_EGRESS_DOMAIN == 0 ) ) ||
             ( egress_domain_hit && (   cond_cfg.MATCH_EGRESS_DOMAIN == 1 ) ) ||
             (   cond_cfg.MATCH_EGRESS_DOMAIN == 2 ) );

    /* Match on MetadataValue0. */
    /* MATCH_METADATA0 - removed from TRIGGER_CONDITION_CFG register */

    /* Match on MetadataValue1. */
    /* MATCH_METADATA1 - removed from TRIGGER_CONDITION_CFG register */

    r_hit = ( (  cond_cfg.MATCH_RANDOM_NUMBER == 0) &&
             (  cond_cfg.MATCH_RANDOM_IF_LESS == 1) &&
             (trig_lfsr0 <= (FM_LITERAL_U64(1) <<   cond_cfg.MATCH_RANDOM_THRESHOLD) ) ) ||
           ( (  cond_cfg.MATCH_RANDOM_NUMBER == 0) &&
             (  cond_cfg.MATCH_RANDOM_IF_LESS == 0) &&
             (trig_lfsr0 > (FM_LITERAL_U64(1) <<   cond_cfg.MATCH_RANDOM_THRESHOLD) ) ) ||
           ( (  cond_cfg.MATCH_RANDOM_NUMBER == 1) &&
             (  cond_cfg.MATCH_RANDOM_IF_LESS == 1) &&
             (trig_lfsr1 <= (FM_LITERAL_U64(1) <<   cond_cfg.MATCH_RANDOM_THRESHOLD) ) ) ||
           ( (  cond_cfg.MATCH_RANDOM_NUMBER == 1) &&
             (  cond_cfg.MATCH_RANDOM_IF_LESS == 0) &&
             (trig_lfsr1 > (FM_LITERAL_U64(1) <<   cond_cfg.MATCH_RANDOM_THRESHOLD) ) );

        hit &= r_hit;

    /* Match on the packet class (Layer 2 unicast, broadcast or multicast). */
    hit &= ( ( (*fclass ==  MBY_FCLASS_UNICAST) &&
               ( (  cond_param.FRAME_CLASS_MASK & 0x1) != 0 ) ) ||
             ( (*fclass ==  MBY_FCLASS_BROADCAST) &&
               ( (  cond_param.FRAME_CLASS_MASK & 0x2) != 0 ) ) ||
             ( (*fclass ==  MBY_FCLASS_MULTICAST) &&
               ( (  cond_param.FRAME_CLASS_MASK & 0x4) != 0 ) ) );

    /* Match on the ingress port. */
    cond_rx = mbyTrigGetConditionRx(trig_apply_map, trig);

    src_port_mask = cond_rx.SRC_PORT_MASK;

    hit &= ( (src_port_mask & (1 << *rx_port)) != 0 );

    /* Match on the (set of) egress port(s). */
    // <--- REVISIT!!!!
    //regPtr = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_CONDITION_TX(trig, 0));
    //destPortMask = FM_GET_FIELD(*regPtr, HLP_TRIGGER_CONDITION_TX, DEST_PORT_MASK);

    switch (  cond_cfg.MATCH_TX)
    {
        case MBY_TRIGGER_CONDITION_CFG_MATCH_TX_MASK_Z:
            hit &= ( (*dmask & dest_port_mask) == 0);
            break;
        case MBY_TRIGGER_CONDITION_CFG_MATCH_TX_MASK_NZ:
            hit &= ( (*dmask & dest_port_mask) != 0);
            break;
        case MBY_TRIGGER_CONDITION_CFG_MATCH_TX_EXACT_EQ:
            hit &= (  *dmask == dest_port_mask );
            break;
        case MBY_TRIGGER_CONDITION_CFG_MATCH_TX_EXACT_NE:
            hit &= (  *dmask != dest_port_mask );
            break;
        default:
            hit = 0;
            break;
    }

    /* Match on the packet's route status. */
    hit &= ( ( !*mark_routed &&
               ( (  cond_param.ROUTED_MASK & 0x1) != 0 ) ) ||
             ( *mark_routed &&
               ( (  cond_param.ROUTED_MASK & 0x2) != 0 ) ) );

    /* Match on one or more bits of the frame handler action mask. */
    cond_amask1 = mbyTrigGetConditionAmask1(trig_apply_map, trig);
    action_mask = cond_amask1.HANDLER_ACTION_MASK;

    cond_amask2 = mbyTrigGetConditionAmask2(trig_apply_map, trig);
    action_mask2 = cond_amask2.HANDLER_ACTION_MASK;
    FM_SET_UNNAMED_FIELD64(action_mask,
                         32,
                         13,
                         action_mask2);

    hit &= ( (*amask & action_mask) != 0 );

    return hit;
}

static void applyPrecedenceResolution
(
    mbyTriggerActions       * const lo,
    fm_int                    const trig,
    mbyTriggerActions       * const hi
)
{

    if ( ( lo->forwardingAction == MBY_TRIG_ACTION_FORWARDING_DROP ) &&
         ( hi->forwardingAction == MBY_TRIG_ACTION_FORWARDING_DROP ) )
        lo->dropMask = lo->dropMask | hi->dropMask;
    else if ( hi->forwardingAction > lo->forwardingAction )
    {
        lo->forwardingAction = hi->forwardingAction;
        lo->dropMask = hi->dropMask;
        lo->newDestGlort = hi->newDestGlort;
        lo->newDestGlortMask = hi->newDestGlortMask;
        lo->newDestMask = hi->newDestMask;
        lo->filterDestMask = hi->filterDestMask;
    }

    if ( hi->trapAction > lo->trapAction )
    {
        lo->trapAction = hi->trapAction;
        if(hi->trapAction == MBY_TRIG_ACTION_TRAP_TRAP) {
            lo->trapCode = hi->trapCode;
        }
    }

    if ( hi->mirroringAction0 > lo->mirroringAction0 )
    {
        lo->mirroringAction0 = hi->mirroringAction0;
        lo->mirrorProfileIndex0 = hi->mirrorProfileIndex0;
    }

    if ( hi->mirroringAction1 > lo->mirroringAction1 )
    {
        lo->mirroringAction1 = hi->mirroringAction1;
        lo->mirrorProfileIndex1 = hi->mirrorProfileIndex1;
    }

    if ( hi->TCAction > lo->TCAction )
    {
        lo->TCAction = hi->TCAction;
        lo->newTC = hi->newTC;
    }

    if ( hi->vlanAction > lo->vlanAction )
    {
        lo->vlanAction = hi->vlanAction;
        lo->newVlan = hi->newVlan;
    }

    if ( hi->learningAction > lo->learningAction )
    {
        lo->learningAction = hi->learningAction;
    }

    if ( hi->rateLimitAction > lo->rateLimitAction )
    {
        lo->rateLimitAction = hi->rateLimitAction;
        lo->newRateLimitNum = hi->newRateLimitNum;
    }

    if ( hi->policerAction > lo->policerAction )
    {
        lo->policerAction = hi->policerAction;
    }

    if ( hi->egressL3DomainAction > lo->egressL3DomainAction )
    {
        lo->egressL3DomainAction = hi->egressL3DomainAction;
    }

    if ( hi->egressL2DomainAction > lo->egressL2DomainAction )
    {
        lo->egressL2DomainAction = hi->egressL2DomainAction;
    }

    if ( hi->metadataAction[hi->metadataActionSlot] > lo->metadataAction[hi->metadataActionSlot] )
    {
        lo->metadataTrigNum[hi->metadataActionSlot] = trig;
        lo->metadataAction[hi->metadataActionSlot] = hi->metadataAction[hi->metadataActionSlot];
        lo->metadataMask[hi->metadataActionSlot] = hi->metadataMask[hi->metadataActionSlot];
        lo->metadataValue[hi->metadataActionSlot] = hi->metadataValue[hi->metadataActionSlot];
        lo->metadataSource[hi->metadataActionSlot] = hi->metadataSource[hi->metadataActionSlot];
        lo->metadataOffset[hi->metadataActionSlot] = hi->metadataOffset[hi->metadataActionSlot];
    }

    if ( hi->noModifyAction > lo->noModifyAction)
    {
        lo->noModifyAction = hi->noModifyAction;
        lo->metadataMaskSel = hi ->metadataMaskSel;
    }
}

static void resolveTriggers
(
    mby_ppe_trig_apply_map      * const trig_apply_map,
//  mby_ppe_trig_apply_misc_map * const trig_apply_misc_map,
//  mby_ppe_trig_usage_map      * const trig_usage_map,
    fm_uint64                     const hit_mask_hi,
    fm_uint64                     const hit_mask_lo,
    fm_uint64                   * const trig_hit_mask_resolved_lo,
    fm_int64                    * const trig_hit_mask_resolved_hi,
    mbyTriggerActions           * const lo
)
{
    mbyTriggerActions           hi;
    fm_bool                     prec_winner = FALSE;

    *trig_hit_mask_resolved_hi = hit_mask_hi;
    *trig_hit_mask_resolved_lo = hit_mask_lo;

    //FM_CLEAR(*lo);

    for (fm_int i = 0; i < MBY_TRIGGERS_COUNT; i++)
    {
        mbyTriggerConditionCfg cond_cfg = mbyTrigGetConditionCfg(trig_apply_map, i);
        prec_winner &=   cond_cfg.MATCH_BY_PRECEDENCE;

        if (i < 64)
        {
            if ( ( (hit_mask_lo & (FM_LITERAL_U64(1) << i)) != 0 ) && !prec_winner )
            {
                prec_winner = TRUE;
                hi = mbyTriggerGetActions(trig_apply_map, i);
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
                hi = mbyTriggerGetActions(trig_apply_map, i);
                applyPrecedenceResolution(lo, i, &hi);
            }
            else if ( ( (hit_mask_hi & (FM_LITERAL_U64(1) << (i - 64))) != 0 ) && prec_winner )
                *trig_hit_mask_resolved_hi &= ~(FM_LITERAL_U64(1) << (i - 64));
        }

    }
}

static void applyTriggers
(
    mbyTriggerActions * const actions,
    mbyTriggerResults * const results,
    fm_uint32         * const action,
    fm_uint32         * const dmask,
    fm_uint16         * const idglort,
    fm_bool           * const qcn_mirror0_profile_v,
    fm_bool           * const qcn_mirror1_profile_v,
    fm_uint32         * const mirror0_profile_idx,
    fm_byte           * const mirror0_profile_v,
    fm_uint32         * const mirror1_profile_idx,
    fm_byte           * const mirror1_profile_v,
    fm_byte           * const qos_swpri,
    fm_uint16         * const l2_evid1,
    fm_bool           * const learning_enabled
)
{
    //hlp_modelState           *state = &model->packetState;
    fm_uint16                dglort;
    fm_uint16                pkt_meta_16;
    fm_uint16                pkt_meta_16_src;
    mbyTriggerActionMetadata actionMetadata;
    fm_byte                  PKT_META_CPY[32];
    fm_uint32                *actionMetadataMask;

    results->filterDestMask = TRUE;

    /* store the action in the triggerResults channel */
    results->forwardingAction = actions->forwardingAction;

    switch (actions->forwardingAction)
    {
        case MBY_TRIG_ACTION_FORWARDING_FORWARD:
        // <--- REVISIT!!!!
            /*if ( state->PRE_RESOLVE_DMASK != 0 )
            {
                dglort = (state->PRE_RESOLVE_DGLORT & ~actions->newDestGlortMask) |
                         (actions->newDestGlort & actions->newDestGlortMask);
                results->destGlort = dglort;
                results->destMask = state->PRE_RESOLVE_DMASK;
                results->action = *action;
            }
            else
            {*/
                results->destGlort = *idglort;
                results->destMask = *dmask;
                results->action = *action;
            //}
            break;

        case MBY_TRIG_ACTION_FORWARDING_REDIRECT:
            dglort = (*idglort & ~actions->newDestGlortMask) |
                     (actions->newDestGlort & actions->newDestGlortMask);
            results->destGlort = dglort;
            results->destMask = actions->newDestMask;
            results->filterDestMask = actions->filterDestMask;

            if ( results->destMask == 0)
                results->action = MBY_ACTION_DROP_TRIG;

            else
                results->action = MBY_ACTION_REDIRECT_TRIG;

            break;

        case MBY_TRIG_ACTION_FORWARDING_DROP:
            results->destGlort = *idglort;
            results->destMask = *dmask & ~actions->dropMask;

            if ( ( *dmask != 0 ) && ( results->destMask == 0 ) )
                results->action = MBY_ACTION_DROP_TRIG;
            else
                results->action = *action;
            break;

        default:
            results->destGlort = *idglort;
            results->destMask = *dmask;
            results->action = *action;
            break;
    }

    /* store trapAction in the triggerResults channel */
    results->trapAction = actions->trapAction;
    results->logAction = 0;
    switch (actions->trapAction)
    {
        case MBY_TRIG_ACTION_TRAP_TRAP:
            results->cpuCode = actions->trapCode;
            break;

        case MBY_TRIG_ACTION_TRAP_LOG:
            results->logAction = 1;
            break;

        case MBY_TRIG_ACTION_TRAP_REVERT: /* do not trap or log */
            if ( *action == MBY_ACTION_TRAP )
            {
                // <--- REVISIT!!!!
                //results->destMask = state->PRE_RESOLVE_DMASK;
                //results->action = state->PRE_RESOLVE_ACTION;
                if ( actions->forwardingAction ==
                     MBY_TRIG_ACTION_FORWARDING_REDIRECT )
                    results->destMask = actions->newDestMask;
                else if ( actions->forwardingAction ==
                     MBY_TRIG_ACTION_FORWARDING_DROP )
                    results->destMask &= ~actions->dropMask;
                if ( results->destMask == 0 )
                    results->action = MBY_ACTION_DROP_TRIG;
            }
            break;

        default:
            break;
    }

    /* store the action in the triggerResults channel */
    results->qcnValid0 = *qcn_mirror0_profile_v;
    results->qcnValid1 = *qcn_mirror1_profile_v;

    results->mirroringAction0 = actions->mirroringAction0;
    results->mirroringAction1 = actions->mirroringAction1;

    if (actions->mirroringAction0 == MBY_TRIG_ACTION_MIRRORING_MIRROR) /* actions for mirrors */
    {
        results->mirror0ProfileV   = 1;
        results->mirror0ProfileIdx = actions->mirrorProfileIndex0;
        results->qcnValid0 = 0;
    }
    else if (actions->mirroringAction0 == MBY_TRIG_ACTION_MIRRORING_CANCEL) /* existing mirror canceled*/
          results->mirror0ProfileV   = 0;

    else/* no change in mirroring disposition; leave mirrors as they are */
    {
        results->mirror0ProfileV   = *mirror0_profile_v;
        results->mirror0ProfileIdx = *mirror0_profile_idx;
    }

    if (actions->mirroringAction1 == MBY_TRIG_ACTION_MIRRORING_MIRROR) /* actions for mirrors */
    {
        results->mirror1ProfileV   = 1;
        results->mirror1ProfileIdx = actions->mirrorProfileIndex1;
        results->qcnValid1 = 0;
    }
    else if (actions->mirroringAction1 == MBY_TRIG_ACTION_MIRRORING_CANCEL) /* existing mirror canceled*/
        results->mirror1ProfileV   = 0;
    else/* no change in mirroring disposition; leave mirrors as they are */
    {
        results->mirror1ProfileV   = *mirror1_profile_v;
        results->mirror1ProfileIdx = *mirror1_profile_idx;
    }

    /* store the action in the triggerResults channel */
    results->TCAction = actions->TCAction;

    results->TC = *qos_swpri;

    if ( actions->TCAction ==
         MBY_TRIG_ACTION_TC_REASSIGN )
        results->TC = actions->newTC;

    /* XXX Real silicon determines the traffic class and the SMP membership
     * here. The HLP white model has moved this logic to the CM stage. */

    /* store the action in the triggerResults channel */
    results->vlanAction = actions->vlanAction;

    results->vlan = *l2_evid1;

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

    /* Metadata removed    // <--- REVISIT!!!! */

    /* store the action in the triggerResults channel */
   results->egressL3DomainAction = actions->egressL3DomainAction;

    /* store the action in the triggerResults channel */
    results->egressL2DomainAction = actions->egressL2DomainAction;

    /* store the action in the triggerResults channel */
    results->policerAction = actions->policerAction;

    /* Metadata removed    // <--- REVISIT!!!! */
    results->noModifyAction = actions->noModifyAction;

}   /* end ApplyTriggers */

static void triggersStatsUpdate
(
    mby_ppe_trig_apply_map * const trig_apply_map,
    fm_uint64              * const trig_hit_mask_resolved_lo,
    fm_int64               * const trig_hit_mask_resolved_hi,
    fm_uint64                const hitMaskHi,
    fm_uint64                const hitMaskLo
)
{
    for (fm_int i = 0; i < MBY_TRIGGERS_COUNT; i++)
    {
        if (i < 64)
        {
            if ( (hitMaskLo & (FM_LITERAL_U64(1) << i)) != 0 )
                if ( (*trig_hit_mask_resolved_lo & (FM_LITERAL_U64(1) << i)) )
                    incrementTrigCounter(trig_apply_map, i);
        } else
            if ( (hitMaskHi & (FM_LITERAL_U64(1) << (i - 64))) != 0 )
                if ( (*trig_hit_mask_resolved_hi & (FM_LITERAL_U64(1) << (i - 64))) )
                    incrementTrigCounter(trig_apply_map, i);
    }
}

void Triggers
(
    mby_ppe_trig_apply_map      * const trig_apply_map,
    mby_ppe_trig_apply_misc_map * const trig_apply_misc_map,
    mby_ppe_trig_usage_map      * const trig_usage_map,
//  mby_ppe_fwd_misc_map        * const fwd_misc_map,
    mbyMaskGenToTriggers  const * const in,
    mbyTriggersToCongMgmt       * const out
)
{
    // Read inputs from the MaskGen:
    /*fm_uint16 l2_evid1    = in->L2_EVID1;
    fm_byte   qos_swpri   = in->QOS_SWPRI;
    fm_uint16 idglort     = in->IDGLORT;
    fm_byte   l2_edomain  = in->L2_EDOMAIN;
    fm_byte   fclass      = in->FCLASS;
    fm_bool   mark_routed = in->MARK_ROUTED;
    fm_uint32 dmask       = in->DMASK;
    fm_uint32 rx_port     = in->RX_PORT;
    fm_uint32 action      = in->ACTION;
    fm_uint64 amask       = in->AMASK;
    fm_bool   qcn_mirror0_profile_v = in->QCN_MIRROR0_PROFILE_V;
    fm_bool   qcn_mirror1_profile_v = in->QCN_MIRROR1_PROFILE_V;
    fm_uint32 mirror0_profile_idx = in->MIRROR0_PROFILE_IDX;
    fm_byte   mirror0_profile_v = in->MIRROR0_PROFILE_V;
    fm_uint32 mirror1_profile_idx = in->MIRROR1_PROFILE_IDX;
    fm_byte   mirror1_profile_v = in->MIRROR1_PROFILE_V;
    fm_bool   learning_enabled = in->LEARNING_ENABLED;

    mbyTriggerActions           actions;
    fm_bool                     hit;
    mbyTriggerResults   results;
    fm_uint64           trig_hit_mask_resolved_lo = FM_LITERAL_U64(0);
    fm_int64            trig_hit_mask_resolved_hi = FM_LITERAL_U64(0);
    fm_uint64                   hit_mask_hi = FM_LITERAL_U64(0);
    fm_uint64                   hit_mask_lo = FM_LITERAL_U64(0);*/
    // Note: this stage is empty for now. Will be coded up in the next sprint. <-- REVISIT!!!

    /* no_modify action comes from triggers. */
    fm_bool no_modify = FALSE;

    /* EvaluateTrigger first */
    /*for (fm_int i = 0; i < MBY_TRIGGERS_COUNT; i++)
    {
        hit = evaluateTrigger(
                MBY_TRG_IN_REGS_P,
                i,
                &l2_evid1,
                &qos_swpri,
                &idglort,
                &l2_edomain,
                &fclass,
                &mark_routed,
                &dmask,
                &rx_port,
                &amask
                );

        if (i < 64)
        {
            if (hit)
                hit_mask_lo |= FM_LITERAL_U64(1) << i;
        }
        else
            if (hit)
                hit_mask_hi |= FM_LITERAL_U64(1) << (i - 64);
    }

    resolveTriggers
    (
        trig_apply_map,
     // trig_apply_misc_map,
     // trig_usage_map,
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
        &action,
        &dmask,
        &idglort,
        &qcn_mirror0_profile_v,
        &qcn_mirror1_profile_v,
        &mirror0_profile_idx,
        &mirror0_profile_v,
        &mirror1_profile_idx,
        &mirror1_profile_v,
        &qos_swpri,
        &l2_evid1,
        &learning_enabled
    );

    triggersStatsUpdate
    (
        trig_apply_map,
     // trig_apply_misc_map,
     // trig_usage_map,
        &trig_hit_mask_resolved_lo,
        &trig_hit_mask_resolved_hi,
        hit_mask_hi,
        hit_mask_lo
    );

    fm_uint64 trig_ip_lo;
    fm_uint64 trig_ip_hi;
    fm_uint64 new_trig_ip_hi;
    trig_ip_lo = trig_apply_misc_map->TRIGGER_IP[0].PENDING;
    trig_ip_lo |= ((FM_LITERAL_U64(1)<<48)-1) & trig_hit_mask_resolved_lo;
    trig_apply_misc_map->TRIGGER_IP[0].PENDING = trig_ip_lo;

    trig_ip_hi = trig_apply_misc_map->TRIGGER_IP[1].PENDING;
    new_trig_ip_hi = 0;*/

    /*take upper 16 bits of resolved_hit_lo*/
    /*FM_SET_UNNAMED_FIELD64(new_trig_ip_hi,
                            0,
                            16,
                            trig_hit_mask_resolved_lo >> 48);*/
    /*take lower 36 bits of resolved_hit_hi*/
    /*FM_SET_UNNAMED_FIELD64(new_trig_ip_hi,
                            (64-48),
                            36,
                            ((FM_LITERAL_U64(1)<<36)-1) & trig_hit_mask_resolved_hi);
    trig_ip_hi |= new_trig_ip_hi;
    trig_apply_misc_map->TRIGGER_IP[1].PENDING = trig_ip_hi;*/

    /* propagate trigger interrupts to FWD_IP and GLOBAL_INTERRUPT regs */

    /*fm_uint64 trig_im_lo = FM_LITERAL_U64(0);
    fm_uint64 trig_im_hi = FM_LITERAL_U64(0);
    trig_ip_lo = trig_apply_misc_map->TRIGGER_IM[0].MASK;
    trig_ip_hi = trig_apply_misc_map->TRIGGER_IM[1].MASK;

    fm_bool fwd_ip_trig;
    fm_bool fwd_ip_entry_count;
    fwd_misc_map->FWD_IP.TRIGGER = (((trig_ip_hi & ~trig_im_hi) != 0) |
                                                  ((trig_ip_lo & ~trig_im_lo) != 0));

    fwd_ip_trig = fwd_misc_map->FWD_IP.TRIGGER;
    fwd_ip_entry_count = fwd_misc_map->FWD_IP.ENTRY_COUNT;

    fm_bool fwd_im_trig;
    fm_bool fwd_im_entry_count;
    fwd_im_trig = fwd_misc_map->FWD_IM.TRIGGER;
    fwd_im_entry_count = fwd_misc_map->FWD_IM.ENTRY_COUNT;

    fm_byte global_int_fwd = 0;*/



     // <--- REVISIT!!!!
    /*if ((fwd_ip_entry_count & ~fwd_im_entry_count) || (fwd_ip_trig & ~fwd_im_trig))
        global_int_fwd = 1;
        if ((fwd_im_entry_count & ~fwd_im_entry_count) == 0 && (fwd_ip_trig & ~fwd_im_trig) == 0)
                global_int_fwd = 0;
    */
    // <--- REVISIT!!!!
    //regPtr = FM_MODEL_GET_REG_PTR(model, HLP_GLOBAL_INTERRUPT(0));
    //FM_ARRAY_SET_BIT(regPtr, HLP_GLOBAL_INTERRUPT, FWD, globalIntFwd);

    // <--- REVISIT!!!!
    //state->QOS_SWPRI = state->TRIGGERS.TC;
    //state->DMASK = state->TRIGGERS.destMask;
    //state->IDGLORT = state->TRIGGERS.destGlort;

    //state->MIRROR1_PROFILE_V = state->TRIGGERS.mirror1ProfileV;
    //state->MIRROR1_PROFILE_IDX = state->TRIGGERS.mirror1ProfileIdx;
    //state->MIRROR0_PROFILE_V = state->TRIGGERS.mirror0ProfileV;
    //state->MIRROR0_PROFILE_IDX = state->TRIGGERS.mirror0ProfileIdx;






    // Pass thru:
    out->ACTION            = in->ACTION; //results.action; //in->ACTION;
    out->DROP_TTL          = in->DROP_TTL;
    out->ECN               = in->ECN;
    out->EDGLORT           = in->EDGLORT;
    out->FNMASK            = in->FNMASK;
    out->IS_IPV4           = in->IS_IPV4;
    out->IS_IPV6           = in->IS_IPV6;
    out->IS_TIMEOUT        = in->IS_TIMEOUT;
    out->L2_DMAC           = in->L2_DMAC;
    out->L2_EVID1          = in->L2_EVID1; //results.vlan; //in->L2_EVID1;
    out->L2_IVLAN1_CNT     = in->L2_IVLAN1_CNT;
    out->MARK_ROUTED       = in->MARK_ROUTED;
    out->MIRTYP            = in->MIRTYP;
    out->MOD_IDX           = in->MOD_IDX;
    out->MOD_PROF_IDX      = in->MOD_PROF_IDX;
    out->NO_MODIFY         = no_modify; //results.noModifyAction; //no_modify;
    out->OOM               = in->OOM;
    out->PARSER_INFO       = in->PARSER_INFO;
    out->PA_HDR_PTRS       = in->PA_HDR_PTRS;
    out->PM_ERR            = in->PM_ERR;
    out->PM_ERR_NONSOP     = in->PM_ERR_NONSOP;
    out->QOS_L3_DSCP       = in->QOS_L3_DSCP;
    out->RX_DATA           = in->RX_DATA;
    out->RX_LENGTH         = in->RX_LENGTH;
    out->RX_PORT           = in->RX_PORT;
    out->SAF_ERROR         = in->SAF_ERROR;
    out->SEG_META_ERR      = in->SEG_META_ERR;
    out->TAIL_CSUM_LEN     = in->TAIL_CSUM_LEN;
    out->TRAFFIC_CLASS     = in->TRAFFIC_CLASS;
    out->TX_DROP           = in->TX_DROP;
    out->TX_TAG            = in->TX_TAG;
    out->XCAST             = in->XCAST;
}
