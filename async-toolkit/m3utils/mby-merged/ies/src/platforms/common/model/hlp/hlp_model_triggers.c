/* vim:et:sw=4:ts=4:sw=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_triggers.c
 * Creation Date:   August 16, 2012
 * Description:     TRIGGERS stage of HLP white model
 *
 * INTEL CONFIDENTIAL
 * Copyright 2012 - 2017 Intel Corporation. All Rights Reserved.
 *
 * The source code contained or described herein and all documents related
 * to the source code ("Material") are owned by Intel Corporation or its
 * suppliers or licensors. Title to the Material remains with Intel
 * Corporation or its suppliers and licensors. The Material contains trade
 * secrets and proprietary and confidential information of Intel or its
 * suppliers and licensors. The Material is protected by worldwide copyright
 * and trade secret laws and treaty provisions. No part of the Material may
 * be used, copied, reproduced, modified, published, uploaded, posted,
 * transmitted, distributed, or disclosed in any way without Intel's prior
 * express written permission.
 *
 * No license under any patent, copyright, trade secret or other intellectual
 * property right is granted to or conferred upon you by disclosure or
 * delivery of the Materials, either expressly, by implication, inducement,
 * estoppel or otherwise. Any license under such intellectual property rights
 * must be express and approved by Intel in writing.
 *****************************************************************************/

#include <fm_sdk_hlp_int.h>
#include <platforms/common/model/hlp/hlp_model_types.h>
#include <platforms/common/model/hlp/debug/hlp_model_debug.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

/*****************************************************************************
 * Global Variables
 *****************************************************************************/
fm_int triggersDisplayVerbose = 1;

/*****************************************************************************
 * Local Function Prototypes
 *****************************************************************************/

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** GetConditionCfg
 * \ingroup intModel
 *
 * \desc            Reads TRIGGER_CONDITION_CFG register data from register cache 
 *                  and populates corresponding register struct
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       trig points to the trigger for which register data needs to
 *                  be fetched.
 *
 * \param[in,out]   condCfg is the struct for TRIGGER_CONDITION_CFG that
 *                  is populated with data from register cache
 *
 *****************************************************************************/
static void GetConditionCfg(hlp_model               *model,
                            fm_byte                 trig,
                            hlpTriggerConditionCfg  *condCfg)
{
    fm_uint32       *regPtr;
    
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_CONDITION_CFG(trig, 0));

    condCfg->MATCH_TX = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_CFG, MATCH_TX);
    condCfg->MATCH_RANDOM_THRESHOLD = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_CFG, MATCH_RANDOM_THRESHOLD);
    condCfg->MATCH_RANDOM_IF_LESS = FM_GET_BIT(*regPtr, HLP_TRIGGER_CONDITION_CFG, MATCH_RANDOM_IF_LESS);
    condCfg->MATCH_RANDOM_NUMBER = FM_GET_BIT(*regPtr, HLP_TRIGGER_CONDITION_CFG, MATCH_RANDOM_NUMBER);    
    condCfg->MATCH_BY_PRECEDENCE = FM_GET_BIT(*regPtr, HLP_TRIGGER_CONDITION_CFG, MATCH_BY_PRECEDENCE);
    condCfg->MATCH_METADATA1 = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_CFG, MATCH_METADATA1);
    condCfg->MATCH_METADATA0 = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_CFG, MATCH_METADATA0);
    condCfg->MATCH_EGRESS_DOMAIN = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_CFG, MATCH_EGRESS_DOMAIN);
    condCfg->MATCH_DEST_GLORT = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_CFG, MATCH_DEST_GLORT);
    condCfg->MATCH_ETHER_TYPE = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_CFG, MATCH_ETHER_TYPE);
    condCfg->MATCH_TC = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_CFG, MATCH_TC);
    condCfg->MATCH_FFU = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_CFG, MATCH_FFU);
    condCfg->MATCH_VLAN = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_CFG, MATCH_VLAN);
    condCfg->MATCH_HIT_SADA = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_CFG, MATCH_HIT_SADA);
    condCfg->MATCH_HIT_DA = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_CFG, MATCH_HIT_DA);
    condCfg->MATCH_HIT_SA = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_CFG, MATCH_HIT_SA);
    condCfg->MATCH_DA = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_CFG, MATCH_DA);
    condCfg->MATCH_SA = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_CFG, MATCH_SA);

   	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS, "MatchMetadata1=%1d, MatchMetadata0=%1d\n", condCfg->MATCH_METADATA1, condCfg->MATCH_METADATA0);
} /* GetConditionCfg */

/*****************************************************************************/
/** GetConditionParam
 * \ingroup intModel
 *
 * \desc            Reads TRIGGER_CONDITION_PARAM register data from register cache 
 *                  and populates corresponding register struct
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       trig points to the trigger for which register data needs to
 *                  be fetched.
 *
 * \param[in,out]   condParam is the struct for TRIGGER_CONDITION_PARAM that
 *                  is populated with data from register cache
 *
 *****************************************************************************/
static void GetConditionParam(hlp_model                   *model,
                            fm_byte                     trig,
                            hlpTriggerConditionParam    *condParam)
{
    fm_uint32       *regPtr;
    
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_CONDITION_PARAM(trig, 0));

    condParam->EGRESS_DOMAIN_MASK = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_PARAM, EGRESS_DOMAIN_MASK);
    condParam->EGRESS_DOMAIN_VALUE = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_PARAM, EGRESS_DOMAIN_VALUE);
    condParam->FTYPE_MASK = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_PARAM, FTYPE_MASK);
    condParam->ROUTED_MASK = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_PARAM, ROUTED_MASK);
    condParam->FRAME_CLASS_MASK = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_PARAM, FRAME_CLASS_MASK);
    condParam->TC = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_PARAM, TC);
    condParam->VID_ID = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_PARAM, VID_ID);
    condParam->DA_ID = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_PARAM, DA_ID);
    condParam->SA_ID = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_PARAM, SA_ID);
} /* GetConditionParam */

/*****************************************************************************/
/** GetConditionFfu
 * \ingroup intModel
 *
 * \desc            Reads TRIGGER_CONDITION_FFU register data from register cache 
 *                  and populates corresponding register struct
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       trig points to the trigger for which register data needs to
 *                  be fetched.
 *
 * \param[in,out]   condFfu is the struct for TRIGGER_CONDITION_FFU that
 *                  is populated with data from register cache
 *
 *****************************************************************************/
static void GetConditionFfu(hlp_model               *model,
                            fm_byte                 trig,
                            hlpTriggerConditionFfu  *condFfu)
{
    fm_uint32       *regPtr;
    
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_CONDITION_FFU(trig, 0));

    condFfu->FFU_MASK = FM_GET_FIELD(*regPtr, HLP_TRIGGER_CONDITION_FFU, FFU_MASK);
    condFfu->FFU_ID = FM_GET_FIELD(*regPtr, HLP_TRIGGER_CONDITION_FFU, FFU_ID);
} /* GetConditionFfu */

/*****************************************************************************/
/** GetConditionType
 * \ingroup intModel
 *
 * \desc            Reads TRIGGER_CONDITION_TYPE register data from register cache 
 *                  and populates corresponding register struct
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       trig points to the trigger for which register data needs to
 *                  be fetched.
 *
 * \param[in,out]   condType is the struct for TRIGGER_CONDITION_TYPE that
 *                  is populated with data from register cache
 *
 *****************************************************************************/
static void GetConditionType(hlp_model               *model,
                            fm_byte                  trig,
                            hlpTriggerConditionType  *condType)
{
    fm_uint32       *regPtr;
    
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_CONDITION_TYPE(trig, 0));

    condType->ETHER_TYPE_MASK = FM_GET_FIELD(*regPtr, HLP_TRIGGER_CONDITION_TYPE, ETHER_TYPE_MASK);
    condType->ETHER_TYPE = FM_GET_FIELD(*regPtr, HLP_TRIGGER_CONDITION_TYPE, ETHER_TYPE);
} /* GetConditionType */

/*****************************************************************************/
/** GetConditionGlort
 * \ingroup intModel
 *
 * \desc            Reads TRIGGER_CONDITION_GLORT register data from register cache 
 *                  and populates corresponding register struct
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       trig points to the trigger for which register data needs to
 *                  be fetched.
 *
 * \param[in,out]   condGlort is the struct for TRIGGER_CONDITION_GLORT that
 *                  is populated with data from register cache
 *
 *****************************************************************************/
static void GetConditionGlort(hlp_model                 *model,
                              fm_byte                   trig,
                              hlpTriggerConditionGlort  *condGlort)
{
    fm_uint32       *regPtr;
    
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_CONDITION_GLORT(trig, 0));

    condGlort->GLORT_MASK = FM_GET_FIELD(*regPtr, HLP_TRIGGER_CONDITION_GLORT, GLORT_MASK);
    condGlort->DEST_GLORT = FM_GET_FIELD(*regPtr, HLP_TRIGGER_CONDITION_GLORT, DEST_GLORT);
} /* GetConditionGlort */

/*****************************************************************************/
/** GetConditionMetadata
 * \ingroup intModel
 *
 * \desc            Reads TRIGGER_CONDITION_METADATA register data from register cache
 *                  and populates corresponding register struct
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in]       trig points to the trigger for which register data needs to
 *                  be fetched.
 *
 * \param[in,out]   condMetadata is the struct for TRIGGER_CONDITION_METADATA that
 *                  is populated with data from register cache
 *
 *****************************************************************************/
static void GetConditionMetadata(hlp_model                   *model,
                                 fm_byte                     trig,
								 hlpTriggerConditionMetadata *condMetadata)
{
    fm_uint32 *regPtr;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_CONDITION_METADATA(trig, 0));

    condMetadata->METADATA_SOURCE1 = FM_ARRAY_GET_BIT(regPtr, HLP_TRIGGER_CONDITION_METADATA, METADATA_SOURCE1);
    condMetadata->METADATA_MASK1 = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_METADATA, METADATA_MASK1);
    condMetadata->METADATA_VALUE1 = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_METADATA, METADATA_VALUE1);
    condMetadata->METADATA_SEL1 = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_METADATA, METADATA_SEL1);
    condMetadata->METADATA_SOURCE0 = FM_GET_BIT(*regPtr, HLP_TRIGGER_CONDITION_METADATA, METADATA_SOURCE0);
    condMetadata->METADATA_MASK0 = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_METADATA, METADATA_MASK0);
    condMetadata->METADATA_VALUE0 = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_METADATA, METADATA_VALUE0);
    condMetadata->METADATA_SEL0 = FM_ARRAY_GET_FIELD(regPtr, HLP_TRIGGER_CONDITION_METADATA, METADATA_SEL0);

	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS, "MetadataSource1=%1d, MetadataSel1=%d, MetadataMask1=0x%x, MetadataValue1=0x%x\n",
									  condMetadata->METADATA_SOURCE1, condMetadata->METADATA_SEL1, condMetadata->METADATA_MASK1, condMetadata->METADATA_VALUE1);
	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS, "MetadataSource0=%1d, MetadataSel0=%d, MetadataMask0=0x%x, MetadataValue0=0x%x\n",
									  condMetadata->METADATA_SOURCE0, condMetadata->METADATA_SEL0, condMetadata->METADATA_MASK0, condMetadata->METADATA_VALUE0);
} /* GetConditionMetadata */


/*****************************************************************************/
/** IncrementTrigCounter        
 * \ingroup intModel
 *
 * \desc            Increments the specified statistics counter by one, 
 *                  wrapping around to zero if necessary.
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       trig is trigger for which statistics counter needs to be
 *                  incremented.
 *
 * \return          
 *
 *****************************************************************************/
static inline fm_status IncrementTrigCounter(hlp_model *model,
                                             fm_byte   trig)
{
    fm_uint32 *     regPtr;
    fm_uint64       trigStat;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_STATS(trig, 0));

    trigStat = FM_ARRAY_GET_FIELD64(regPtr, HLP_TRIGGER_STATS, COUNT);

	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
				 " IncTrigCtr (pre_write): TRIGGER_STATS[%d]=0x%llx\n",
				trig, trigStat);

    /* Ensure portability of overflow operation (wrap arround) */
    if (trigStat == FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF))
    {
        trigStat = 0;
    }
    else
    {
        trigStat++;
    }

    FM_ARRAY_SET_FIELD64(regPtr, HLP_TRIGGER_STATS, COUNT, trigStat);

	trigStat = FM_ARRAY_GET_FIELD64(regPtr, HLP_TRIGGER_STATS, COUNT);
	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
				" IncTrigCtr (post_write): TRIGGER_STATS[%d]=0x%llx\n",
				trig, trigStat);

    return FM_OK;
}   /* end IncrementTrigCounter */

static void ApplyPrecedenceResolution(hlp_modelTriggerActions *lo,
                                      fm_int                  trig,
                                      hlp_modelTriggerActions *hi)
{

    if ( ( lo->forwardingAction == HLP_MODEL_TRIG_ACTION_FORWARDING_DROP ) &&
         ( hi->forwardingAction == HLP_MODEL_TRIG_ACTION_FORWARDING_DROP ) )
    {
        /* Changed to bitwise OR per RRC bug 22752 */
        lo->dropMask = lo->dropMask | hi->dropMask;
    }
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
        //lo->cpuCode = hi->cpuCode;
        if(hi->trapAction == HLP_MODEL_TRIG_ACTION_TRAP_TRAP) {
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

        WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
					" hi->metadataActionSlot=%1d, lo->metadataAction=%1d\n",
					hi->metadataActionSlot, lo->metadataAction[hi->metadataActionSlot]);
		for(int i=0; i<4; i++) {
			WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
						" lo->metadataAction[%d]=%1d\n", i, lo->metadataAction[i]);
		}
		WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS, " hi->metadataActionSlot=%1d\n", hi->metadataActionSlot);
		for(int i=0; i<4; i++) {
			WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
						" hi->metadataAction[%d]=%1d\n", i, hi->metadataAction[i]);
		}

        lo->metadataAction[hi->metadataActionSlot] = hi->metadataAction[hi->metadataActionSlot];
        lo->metadataMask[hi->metadataActionSlot] = hi->metadataMask[hi->metadataActionSlot];
        lo->metadataValue[hi->metadataActionSlot] = hi->metadataValue[hi->metadataActionSlot];
        lo->metadataSource[hi->metadataActionSlot] = hi->metadataSource[hi->metadataActionSlot];
        lo->metadataOffset[hi->metadataActionSlot] = hi->metadataOffset[hi->metadataActionSlot];

		for(int i=0; i<4; i++) {
			WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
						" lo->metadataAction[%d]=%1d, lo->metadataTrigNum[%d]=%2d\n", i, lo->metadataAction[i], i, lo->metadataTrigNum[i]);
		}
    } 

    if ( hi->noModifyAction > lo->noModifyAction)
    {
        lo->noModifyAction = hi->noModifyAction;
        lo->metadataMaskSel = hi ->metadataMaskSel;
    }
}   /* end ApplyPrecedenceResolution */

/* TODO: not used here ? :: TRIGGER_RATE_LIM_CFG_2 [0..15] . DropMask 24b [23:0] = Rate limiter configuration (Drop mask) */

static void ApplyTriggers(hlp_model *              model,
                          hlp_modelTriggerActions *actions)
{
    hlp_modelState           *state = &model->packetState;
    fm_uint16                dglort;
    fm_uint16                pkt_meta_16;
    fm_uint16                pkt_meta_16_src;
    hlpTriggerActionMetadata actionMetadata;
    fm_uint32                *regPtr;
    fm_byte                  storeTrapAction;
    fm_byte                  PKT_META_CPY[32];
    fm_uint32                *actionMetadataMask;

    state->TRIGGERS.filterDestMask = TRUE;
    
    /* store the action in the triggerResults channel */
    state->TRIGGERS.forwardingAction = actions->forwardingAction;

    switch (actions->forwardingAction)
    {
        case HLP_MODEL_TRIG_ACTION_FORWARDING_FORWARD:
            if ( state->PRE_RESOLVE_DMASK != 0 )
            {    
                dglort = (state->PRE_RESOLVE_DGLORT & ~actions->newDestGlortMask) |
                         (actions->newDestGlort & actions->newDestGlortMask);
                state->TRIGGERS.destGlort = dglort;
                state->TRIGGERS.destMask = state->PRE_RESOLVE_DMASK;
                state->TRIGGERS.action = state->PRE_RESOLVE_ACTION;
            } 
            else
            {
                state->TRIGGERS.destGlort = state->IDGLORT;
                state->TRIGGERS.destMask = state->DMASK;
                state->TRIGGERS.action = state->ACTION;
            }    
            break;

        case HLP_MODEL_TRIG_ACTION_FORWARDING_REDIRECT:
            dglort = (state->IDGLORT & ~actions->newDestGlortMask) |
                     (actions->newDestGlort & actions->newDestGlortMask);
            state->TRIGGERS.destGlort = dglort;
            state->TRIGGERS.destMask = actions->newDestMask;
            state->TRIGGERS.filterDestMask = actions->filterDestMask;

            if ( state->TRIGGERS.destMask == 0)
            {
                state->TRIGGERS.action = HLP_MODEL_ACTION_DROP_TRIG;
            }
            else {
                state->TRIGGERS.action = HLP_MODEL_ACTION_REDIRECT_TRIG;
            }
            break;

        case HLP_MODEL_TRIG_ACTION_FORWARDING_DROP:
            state->TRIGGERS.destGlort = state->IDGLORT;
            state->TRIGGERS.destMask = state->DMASK & ~actions->dropMask;
            
            if ( ( state->DMASK != 0 ) && ( state->TRIGGERS.destMask == 0 ) )
            {
                state->TRIGGERS.action = HLP_MODEL_ACTION_DROP_TRIG;
            } 
            else
            {
                state->TRIGGERS.action = state->ACTION;
            }    
            break;

        default:
            state->TRIGGERS.destGlort = state->IDGLORT;
            state->TRIGGERS.destMask = state->DMASK;
            state->TRIGGERS.action = state->ACTION;
            break;
    }
    
    /* store trapAction in the triggerResults channel */
    state->TRIGGERS.trapAction = actions->trapAction;
    state->TRIGGERS.logAction = 0;
    switch (actions->trapAction)
    {
        case HLP_MODEL_TRIG_ACTION_TRAP_TRAP:
            state->TRIGGERS.cpuCode = actions->trapCode;
            break;

        case HLP_MODEL_TRIG_ACTION_TRAP_LOG:
            state->TRIGGERS.logAction = 1;
            break;

        case HLP_MODEL_TRIG_ACTION_TRAP_REVERT: /* do not trap or log */
            if ( state->ACTION == HLP_MODEL_ACTION_TRAP )
            {
                state->TRIGGERS.destMask = state->PRE_RESOLVE_DMASK;
                state->TRIGGERS.action = state->PRE_RESOLVE_ACTION;
                if ( actions->forwardingAction ==
                     HLP_MODEL_TRIG_ACTION_FORWARDING_REDIRECT )
                {
                    state->TRIGGERS.destMask = actions->newDestMask;
                }
                else if ( actions->forwardingAction ==
                     HLP_MODEL_TRIG_ACTION_FORWARDING_DROP )
                {
                    state->TRIGGERS.destMask &= ~actions->dropMask;
                }
                if ( state->TRIGGERS.destMask == 0 )
                {
                    state->TRIGGERS.action = HLP_MODEL_ACTION_DROP_TRIG;
                }
            }
            break;

        default:
            break;
    }

    /* store the action in the triggerResults channel */
    state->TRIGGERS.qcnValid0 = state->QCN_MIRROR0_PROFILE_V;
    state->TRIGGERS.qcnValid1 = state->QCN_MIRROR1_PROFILE_V;

    state->TRIGGERS.mirroringAction0 = actions->mirroringAction0;
    state->TRIGGERS.mirroringAction1 = actions->mirroringAction1;

    if (actions->mirroringAction0 == HLP_MODEL_TRIG_ACTION_MIRRORING_MIRROR) /* actions for mirrors */
    {
        state->TRIGGERS.mirror0ProfileV   = 1;
        state->TRIGGERS.mirror0ProfileIdx = actions->mirrorProfileIndex0;
        state->TRIGGERS.qcnValid0 = 0;
    }
    else if (actions->mirroringAction0 == HLP_MODEL_TRIG_ACTION_MIRRORING_CANCEL) /* existing mirror canceled*/
    {       
        state->TRIGGERS.mirror0ProfileV   = 0;
    }
    else/* no change in mirroring disposition; leave mirrors as they are */
    {
        state->TRIGGERS.mirror0ProfileV   = state->MIRROR0_PROFILE_V;
        state->TRIGGERS.mirror0ProfileIdx = state->MIRROR0_PROFILE_IDX;
    }

    if (actions->mirroringAction1 == HLP_MODEL_TRIG_ACTION_MIRRORING_MIRROR) /* actions for mirrors */
    {
        state->TRIGGERS.mirror1ProfileV   = 1;
        state->TRIGGERS.mirror1ProfileIdx = actions->mirrorProfileIndex1;
        state->TRIGGERS.qcnValid1 = 0;
    }
    else if (actions->mirroringAction1 == HLP_MODEL_TRIG_ACTION_MIRRORING_CANCEL) /* existing mirror canceled*/
    {       
        state->TRIGGERS.mirror1ProfileV   = 0;
    }
    else/* no change in mirroring disposition; leave mirrors as they are */
    {
        state->TRIGGERS.mirror1ProfileV   = state->MIRROR1_PROFILE_V;
        state->TRIGGERS.mirror1ProfileIdx = state->MIRROR1_PROFILE_IDX;
    }

    /* store the action in the triggerResults channel */
    state->TRIGGERS.TCAction = actions->TCAction;

    state->TRIGGERS.TC = state->QOS_SWPRI;

    if ( actions->TCAction ==
         HLP_MODEL_TRIG_ACTION_TC_REASSIGN )
    {
        state->TRIGGERS.TC = actions->newTC;
    }

    /* XXX Real silicon determines the traffic class and the SMP membership
     * here. The HLP white model has moved this logic to the CM stage. */

    /* store the action in the triggerResults channel */
    state->TRIGGERS.vlanAction = actions->vlanAction;

    state->TRIGGERS.vlan = state->L2_EVID1;

    if ( actions->vlanAction == HLP_MODEL_TRIG_ACTION_VLAN_REASSIGN )
    {
        state->TRIGGERS.vlan = actions->newVlan;
    }

    /* store the action in the triggerResults channel */
    state->TRIGGERS.learningAction = actions->learningAction;
    
    if(actions->learningAction == HLP_MODEL_TRIG_ACTION_LEARNING_DONT_LEARN)
    {
        state->LEARNING_ENABLED = 0;
//        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, " state->LEARNING_ENABLED = 0 due to actions->learningAction == HLP_MODEL_TRIG_ACTION_LEARNING_DONT_LEARN. \n");
    }
    else if(actions->learningAction == HLP_MODEL_TRIG_ACTION_LEARNING_FORCE_LEARN)
    {
        state->LEARNING_ENABLED = 1;
//        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, " state->LEARNING_ENABLED = 1 due to actions->learningAction == HLP_MODEL_TRIG_ACTION_LEARNING_FORCE_LEARN. \n");
    }

    /* store the action in the triggerResults channel */
    state->TRIGGERS.rateLimitAction = actions->rateLimitAction;
    
    if( actions->rateLimitAction == HLP_MODEL_TRIG_ACTION_RATE_LIMIT_APPLY)
    {
        state->TRIGGERS.rateLimitNum = actions->newRateLimitNum;
    }    

    /* store the action in the triggerResults channel */
    for(int i=0; i< 32; i++)
    {
        PKT_META_CPY[i] = state->PKT_META[i];
    }
    for(int i=0; i<4; i++) 
    {

		WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
					" pkt_meta[%d]=0x%x, pkt_meta[%d]=0x%x\n",
					actions->metadataOffset[i]+1, state->PKT_META[actions->metadataOffset[i]+1],
					actions->metadataOffset[i], state->PKT_META[actions->metadataOffset[i]]);

        state->TRIGGERS.metadataTrigNum[i] = actions->metadataTrigNum[i];
        state->TRIGGERS.metadataAction[i] = actions->metadataAction[i];
        switch (actions->metadataAction[i])
        {
            case HLP_MODEL_TRIG_ACTION_METADATA_MASKED_16_SET: /* METADATA set */
				WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
							" i=%d, action->metadataOffset=0x%x, action->metadataMask=0x%x, action->metadataValue=0x%x.\n",
							i, actions->metadataOffset[i], actions->metadataMask[i], actions->metadataValue[i]);
                pkt_meta_16 = ((state->PKT_META[actions->metadataOffset[i] + 1]) << 8) | state->PKT_META[actions->metadataOffset[i]];
                pkt_meta_16 &= ~actions->metadataMask[i];
                pkt_meta_16 |= actions->metadataValue[i];
                state->PKT_META[actions->metadataOffset[i] + 1] = (pkt_meta_16 >> 8);
                state->PKT_META[actions->metadataOffset[i]] = (pkt_meta_16 & 0xff);
				WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
							" pkt_meta[%d]=0x%x, pkt_meta[%d]=0x%x\n",
							actions->metadataOffset[i]+1, state->PKT_META[actions->metadataOffset[i]+1],
							actions->metadataOffset[i], state->PKT_META[actions->metadataOffset[i]]);
                break;

            case HLP_MODEL_TRIG_ACTION_METADATA_MASKED_16_COPY: /* METADATA copy */
                for( int i=0; i< 32; i+=8 ) {
                	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
                                " PKT_META_CPY[%2d:%2d] = 0x%02x, 0x%02x, 0x%02x, 0x%02x, 0x%02x, 0x%02x, 0x%02x, 0x%02x.\n", 
                                i, i+7, 
                                PKT_META_CPY[i], PKT_META_CPY[i+1], PKT_META_CPY[i+2], PKT_META_CPY[i+3],
                                PKT_META_CPY[i+4], PKT_META_CPY[i+5], PKT_META_CPY[i+6], PKT_META_CPY[i+7]); 
                    }
                WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
                                " i=%d, action->metadataOffset=0x%x, action->metadataMask=0x%x, action->metadataSource=0x%x.\n", 
                                i, actions->metadataOffset[i], actions->metadataMask[i], actions->metadataSource[i]);
                pkt_meta_16 = ((state->PKT_META[actions->metadataOffset[i] + 1]) << 8) | state->PKT_META[actions->metadataOffset[i]];
                pkt_meta_16_src = ((PKT_META_CPY[actions->metadataSource[i] + 1]) << 8) | PKT_META_CPY[actions->metadataSource[i]];
                pkt_meta_16 &= ~actions->metadataMask[i];
                pkt_meta_16 |= (pkt_meta_16_src & actions->metadataMask[i]);
                state->PKT_META[actions->metadataOffset[i] + 1] = (pkt_meta_16 >> 8);
                state->PKT_META[actions->metadataOffset[i]] = (pkt_meta_16 & 0xff);
				WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
							" pkt_meta[%d]=0x%x, pkt_meta[%d]=0x%x\n",
							actions->metadataOffset[i]+1, state->PKT_META[actions->metadataOffset[i]+1],
							actions->metadataOffset[i], state->PKT_META[actions->metadataOffset[i]]);
                break;

            default:
                break;
        }
    }

    /* store the action in the triggerResults channel */
    state->TRIGGERS.egressL3DomainAction = actions->egressL3DomainAction;

    /* store the action in the triggerResults channel */
    state->TRIGGERS.egressL2DomainAction = actions->egressL2DomainAction;

    /* store the action in the triggerResults channel */
    state->TRIGGERS.policerAction = actions->policerAction;

    for( int i=0; i< 32; i+=8 ) {
    	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
                    " PARSER_PKT_META[%2d:%2d] = 0x%02x, 0x%02x, 0x%02x, 0x%02x, 0x%02x, 0x%02x, 0x%02x, 0x%02x.\n", 
                    i, i+7, 
                    state->PARSER_PKT_META[i], state->PARSER_PKT_META[i+1], state->PARSER_PKT_META[i+2], state->PARSER_PKT_META[i+3],
                    state->PARSER_PKT_META[i+4], state->PARSER_PKT_META[i+5], state->PARSER_PKT_META[i+6], state->PARSER_PKT_META[i+7]); 
    }

    if ( actions->noModifyAction == HLP_MODEL_TRIG_ACTION_NOMODIFY_DO_NOT_MODIFY)
    {
    	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS, "metadataMaskSel=%1d\n", actions->metadataMaskSel);
        for(int i=0; i<4; i++)
        {
            actionMetadataMask = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_ACTION_METADATA_MASK(actions->metadataMaskSel, i, 0));
            actions->metadataRevMask[i] = FM_ARRAY_GET_FIELD64(actionMetadataMask, HLP_TRIGGER_ACTION_METADATA_MASK, METADATA_MASK);
            WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS, "metadataRevMask=%llx\n", actions->metadataRevMask[i]);
            for(int j=0; j<8; j++)
            {
                //fm_byte octect_mask = ((actions->metadataRevMask[i] >> (8*j)) & 0xff);
                fm_byte octect_mask = FM_GET_UNNAMED_FIELD64(actions->metadataRevMask[i], j*8, 8);
                state->PKT_META[i*8+j] = (state->PKT_META[i*8+j] & ~octect_mask) |(state->PARSER_PKT_META[i*8+j] & octect_mask);
            }
        }
    }
    state->TRIGGERS.noModifyAction = actions->noModifyAction;

   	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
        "actions->forwardingAction =0x%x, actions->trapAction=0x%x, actions->mirroringAction0 =0x%x, actions->mirroringAction1 =0x%x,actions->mirrorProfileIndex0=0x%x, actions->mirrorProfileIndex1=0x%x\n",
        actions->forwardingAction, actions->trapAction, 
        actions->mirroringAction0, actions->mirroringAction1,
        actions->mirrorProfileIndex0, actions->mirrorProfileIndex1);
}   /* end ApplyTriggers */

/* FIXME Add support for random matching. */
static fm_bool EvaluateTrigger(hlp_model *model, fm_int trig)
{
    hlp_modelState *                state = &model->packetState;
    hlpTriggerConditionCfg          condCfg;
    hlpTriggerConditionFfu          condFfu;
    hlpTriggerConditionMetadata     condMetadata;
    hlpTriggerConditionGlort        condGlort;
    hlpTriggerConditionParam        condParam;
    hlpTriggerConditionType         condType;
    fm_uint32                       *regPtr;
    fm_bool                         learningEnable;
    fm_byte                         saId;
    fm_bool                         dglortHit;
    fm_uint16                       EgressDomain = 0;
    fm_bool                         EgressDomainHit;
    fm_bool                         MetadataValue0Hit;
    fm_bool                         MetadataValue1Hit;
    fm_bool                         etypeHit;
    fm_bool                         ffuHit;
    fm_bool                         hit = TRUE;
    fm_bool                         rhit;
    fm_byte                         dst_trig;
    fm_uint32                       trig_lfsr0 = 0;
    fm_uint32                       trig_lfsr1 = 0;
    fm_uint32                       srcPortMask;
    fm_uint32                       destPortMask;
    fm_uint64                       actionMask;
    fm_byte                         pktMetaSel[2];
    
    GetConditionCfg(model, trig, &condCfg);
    GetConditionFfu(model, trig, &condFfu);
    GetConditionParam(model, trig, &condParam);
    GetConditionType(model, trig, &condType);
    GetConditionGlort(model, trig, &condGlort);
    GetConditionMetadata(model, trig, &condMetadata);
      
    //This code doesn't match RTL. So DV should not enable WM for matchRandom
    //tests
    trig_lfsr0 = FM_GET_UNNAMED_FIELD(fmRand(), 0, 24);
    trig_lfsr1 = FM_GET_UNNAMED_FIELD(fmRand(), 0, 24);
    
	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
				 " [trig=0x%x] trig_lfsr0 = 0x%x, trig_lfsr1 = 0x%x \n",
				 trig, trig_lfsr0, trig_lfsr1);

    /* Match on the source MAC address lookup result. */

    saId = 0;
    learningEnable = 0;
    
//    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_PORT_CFG_3(state->RX_PORT));
//    learningEnable = FM_GET_BIT(*regPtr, HLP_PORT_CFG_3, LEARNING_ENABLE);

    /* No src lookup */
//    if (learningEnable == 0)
//    {
//        saId = 0;
//    }
//    /* src miss */
    if (state->SA_HIT == 0)
    {
        saId = 63;
    }
    else
    {
        saId = state->SA_RESULT.TRIG_ID;
    }

   	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
        " saID: port_cfg_3->LearningEnable=%1d; state->SA_HIT=%1d; saID=0x%x.\n", 
        learningEnable, state->SA_HIT, saId); 

    hit &= ( ( ( saId !=   condParam.SA_ID ) &&
               (   condCfg.MATCH_SA == 0 ) ) ||
             ( ( saId ==   condParam.SA_ID ) &&
               (   condCfg.MATCH_SA == 1 ) ) ||
             (   condCfg.MATCH_SA == 2 ) );

	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
		" [trig=0x%x] match_sa (hit = 0x%x;  saId = 0x%x)\n",
		trig, hit, saId);

    /* Match on a source MAC address lookup hit. */
    hit &= ( ( !state->SA_HIT && (   condCfg.MATCH_HIT_SA == 0 ) ) ||
             ( state->SA_HIT && (   condCfg.MATCH_HIT_SA == 1 ) ) ||
             (   condCfg.MATCH_HIT_SA == 2 ) );

	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
		" [trig=0x%x] match_hit_sa (hit = %x)\n",
		trig, hit);

    /* RRC bug 23763 */
    if ( !state->DA_HIT )
    {
        dst_trig = 0x3f; 
		WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
					 "!DA_HIT - dst_trig to 1's: dst_trig=0x%x]\n",
					 dst_trig);
    }
    else
    {
        dst_trig = state->DA_RESULT.TRIG_ID;
    }

    /* Match on the destination MAC address lookup result. */
    hit &= ( ( ( dst_trig !=   condParam.DA_ID ) &&
               (   condCfg.MATCH_DA == 0 ) ) ||
             ( ( dst_trig ==   condParam.DA_ID ) &&
               (   condCfg.MATCH_DA == 1 ) ) ||
             (   condCfg.MATCH_DA == 2 ) );

	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
		" [trig=0x%x] match_da (hit = %x)\n",
		trig, hit);

    /* Match on a destination MAC address lookup hit. */
    hit &= ( ( !state->DA_HIT && (   condCfg.MATCH_HIT_DA == 0 ) ) ||
             ( state->DA_HIT && (   condCfg.MATCH_HIT_DA == 1 ) ) ||
             (   condCfg.MATCH_HIT_DA == 2 ) );

	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
		" [trig=0x%x] match_hit_da (hit = %x)\n",
		trig, hit);

    /* Match on a destination or source MAC address lookup hit. */
    hit &= ( ( ( !state->DA_HIT || !state->SA_HIT ) &&
               (   condCfg.MATCH_HIT_SADA == 0 ) ) ||
             ( ( state->DA_HIT || state->SA_HIT ) &&
               (   condCfg.MATCH_HIT_SADA == 1 ) ) ||
             (   condCfg.MATCH_HIT_SADA == 2 ) );

	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
		" [trig=%-2d] (state->DA_HIT=%d, state->SA_HIT=%d,   condCfg.MATCH_HIT_SADA=%d), match_hit_sa_da) hit = %d\n",
		trig, state->DA_HIT, state->SA_HIT,
		  condCfg.MATCH_HIT_SADA, hit);

    /* Match on the egress VLAN ID. */
    hit &= ( ( ( state->L2_EVLAN1_TRIG !=   condParam.VID_ID ) &&
               (   condCfg.MATCH_VLAN == 0 ) ) ||
             ( ( state->L2_EVLAN1_TRIG ==   condParam.VID_ID ) &&
               (   condCfg.MATCH_VLAN == 1 ) ) ||
             (   condCfg.MATCH_VLAN == 2 ) );

	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
		" [trig=0x%x] match_vlan (hit = %x)\n",
		trig, hit);

    /* Match on the FFU lookup result. */
    ffuHit = ( (state->FFU_TRIG &   condFfu.FFU_MASK) ==
               (  condFfu.FFU_ID & condFfu.FFU_MASK) );
    hit &= ( ( !ffuHit && (   condCfg.MATCH_FFU == 0 ) ) ||
             ( ffuHit && (   condCfg.MATCH_FFU == 1 ) ) ||
             (   condCfg.MATCH_FFU == 2 ) );

	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
		" [trig=0x%x] match_ffu (hit = %x)\n",
		trig, hit);

    /* Match on the traffic class (TC). */
    hit &= ( ( ( state->QOS_SWPRI !=   condParam.TC ) &&
               (   condCfg.MATCH_TC == 0 ) ) ||
             ( ( state->QOS_SWPRI ==   condParam.TC ) &&
               (   condCfg.MATCH_TC == 1 ) ) ||
             (   condCfg.MATCH_TC == 2 ) );

	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
		" [trig=0x%x] match_tc (hit = %x)\n",
		trig, hit);

    /* Match on the Ethernet type. */
    etypeHit = ( (state->L2_ETYPE &   condType.ETHER_TYPE_MASK) ==
                 (  condType.ETHER_TYPE & condType.ETHER_TYPE_MASK) );
    hit &= ( ( !etypeHit && (   condCfg.MATCH_ETHER_TYPE == 0 ) ) ||
             ( etypeHit && (   condCfg.MATCH_ETHER_TYPE == 1 ) ) ||
             (   condCfg.MATCH_ETHER_TYPE == 2 ) );

	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
		" [trig=0x%x] match_ethertype (hit = %x)\n",
		trig, hit);

    /* Match on the destination GLORT. */
    dglortHit = ( (state->IDGLORT &   condGlort.GLORT_MASK) ==
                  (  condGlort.DEST_GLORT & condGlort.GLORT_MASK) );
    hit &= ( ( !dglortHit && (   condCfg.MATCH_DEST_GLORT == 0 ) ) ||
             ( dglortHit && (   condCfg.MATCH_DEST_GLORT == 1 ) ) ||
             (   condCfg.MATCH_DEST_GLORT == 2 ) );

	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
		" [trig=0x%x] match_dst_glort (hit = %x)\n",
		trig, hit);

    /* Match on EgressDomainValue. */
    EgressDomain = ((state->L3_EDOMAIN & 0x3f) << 9) | (state->L2_EDOMAIN & 0x1ff);
    EgressDomainHit = ( (EgressDomain &   condParam.EGRESS_DOMAIN_MASK) ==
    		            (  condParam.EGRESS_DOMAIN_VALUE & condParam.EGRESS_DOMAIN_MASK) );
    hit &= ( ( !EgressDomainHit && (   condCfg.MATCH_EGRESS_DOMAIN == 0 ) ) ||
             ( EgressDomainHit && (   condCfg.MATCH_EGRESS_DOMAIN == 1 ) ) ||
             (   condCfg.MATCH_EGRESS_DOMAIN == 2 ) );
	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
		" [trig=0x%x] match_egress_domain (hit = %x) \n",
		trig, hit);

    /* Match on MetadataValue0. */
    pktMetaSel[0] = (  condMetadata.METADATA_SOURCE0==0) ? state->PARSER_PKT_META[condMetadata.METADATA_SEL0] : state->PKT_META[condMetadata.METADATA_SEL0];
    MetadataValue0Hit = ( (pktMetaSel[0] &   condMetadata.METADATA_MASK0) ==
                          (  condMetadata.METADATA_VALUE0 & condMetadata.METADATA_MASK0));
    hit &= ( ( !MetadataValue0Hit && (   condCfg.MATCH_METADATA0 == 0 ) ) ||
             ( MetadataValue0Hit && (   condCfg.MATCH_METADATA0 == 1 ) ) ||
             (   condCfg.MATCH_METADATA0 == 2 ) );
	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
		" [trig=0x%x] match_metadata_value0 (hit = %x) \n",
		trig, hit);

    /* Match on MetadataValue1. */
    pktMetaSel[1] = (  condMetadata.METADATA_SOURCE1==0) ? state->PARSER_PKT_META[condMetadata.METADATA_SEL1] : state->PKT_META[condMetadata.METADATA_SEL1];
    MetadataValue1Hit = ( (pktMetaSel[1] &   condMetadata.METADATA_MASK1) ==
                          (  condMetadata.METADATA_VALUE1 & condMetadata.METADATA_MASK1));
    hit &= ( ( !MetadataValue1Hit && (   condCfg.MATCH_METADATA1 == 0 ) ) ||
             ( MetadataValue1Hit && (   condCfg.MATCH_METADATA1 == 1 ) ) ||
             (   condCfg.MATCH_METADATA1 == 2 ) );
	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
		" [trig=0x%x] match_metadata_value1 (hit = %x) \n",
		trig, hit);

    rhit = ( (  condCfg.MATCH_RANDOM_NUMBER == 0) &&
             (  condCfg.MATCH_RANDOM_IF_LESS == 1) &&
             (trig_lfsr0 <= (FM_LITERAL_U64(1) <<   condCfg.MATCH_RANDOM_THRESHOLD) ) ) ||
           ( (  condCfg.MATCH_RANDOM_NUMBER == 0) &&
             (  condCfg.MATCH_RANDOM_IF_LESS == 0) &&
             (trig_lfsr0 > (FM_LITERAL_U64(1) <<   condCfg.MATCH_RANDOM_THRESHOLD) ) ) ||
           ( (  condCfg.MATCH_RANDOM_NUMBER == 1) &&
             (  condCfg.MATCH_RANDOM_IF_LESS == 1) &&
             (trig_lfsr1 <= (FM_LITERAL_U64(1) <<   condCfg.MATCH_RANDOM_THRESHOLD) ) ) ||
           ( (  condCfg.MATCH_RANDOM_NUMBER == 1) &&
             (  condCfg.MATCH_RANDOM_IF_LESS == 0) &&
             (trig_lfsr1 > (FM_LITERAL_U64(1) <<   condCfg.MATCH_RANDOM_THRESHOLD) ) );

	hit &= rhit;

	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
		" [trig=0x%x] match_random: (hit = %x)\n",
		trig, hit);
	/*
	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
		" [trig=0x%x] match_random: IF_LESS=%d, THRESH=%d, LFSR=0x%6x, RAND_HIT = %d, (hit = %x)\n",
		trig, condCfg.MATCH_RANDOM_IF_LESS, condCfg.MATCH_RANDOM_THRESHOLD,
		condCfg.MATCH_RANDOM_NUMBER ? trig_lfsr1 : trig_lfsr0, rhit, hit);
    */
    /* Match on the packet class (Layer 2 unicast, broadcast or multicast). */
    hit &= ( ( (state->FCLASS ==  HLP_MODEL_FCLASS_UNICAST) &&
               ( (  condParam.FRAME_CLASS_MASK & 0x1) != 0 ) ) ||
             ( (state->FCLASS ==  HLP_MODEL_FCLASS_BROADCAST) &&
               ( (  condParam.FRAME_CLASS_MASK & 0x2) != 0 ) ) ||
             ( (state->FCLASS ==  HLP_MODEL_FCLASS_MULTICAST) &&
               ( (  condParam.FRAME_CLASS_MASK & 0x4) != 0 ) ) );

	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
		" [trig=0x%x] match_class (hit = %x)\n",
		trig, hit);

    /* Match on the ingress port. */
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_CONDITION_RX(trig, 0));
    srcPortMask = FM_GET_FIELD(*regPtr, HLP_TRIGGER_CONDITION_RX, SRC_PORT_MASK);

    hit &= ( (srcPortMask & (1 << state->RX_PORT)) != 0 );

	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
		" [trig=0x%x] match_src_port (hit = %x) (port = 0x%x) \n",
		trig, hit, state->RX_PORT);

    /* Match on the (set of) egress port(s). */
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_CONDITION_TX(trig, 0));
    destPortMask = FM_GET_FIELD(*regPtr, HLP_TRIGGER_CONDITION_TX, DEST_PORT_MASK);

    switch (  condCfg.MATCH_TX)
    {
        case HLP_TRIGGER_CONDITION_CFG_MATCH_TX_MASK_Z:
            hit &= ( (state->DMASK & destPortMask) == 0); 
            break;
        case HLP_TRIGGER_CONDITION_CFG_MATCH_TX_MASK_NZ:
            hit &= ( (state->DMASK & destPortMask) != 0); 
            break;
        case HLP_TRIGGER_CONDITION_CFG_MATCH_TX_EXACT_EQ:
            hit &= (  state->DMASK == destPortMask ); 
            break;
        case HLP_TRIGGER_CONDITION_CFG_MATCH_TX_EXACT_NE:
            hit &= (  state->DMASK != destPortMask ); 
            break;
        default:
            hit = 0;
            break;
    }

	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
		" [trig=0x%x] match_dst_port (hit = %x) (DMASK=0x%x) \n",
		trig, hit, state->DMASK);

    /* Match on the packet's route status. */
    hit &= ( ( !state->MARK_ROUTED &&
               ( (  condParam.ROUTED_MASK & 0x1) != 0 ) ) ||
             ( state->MARK_ROUTED &&
               ( (  condParam.ROUTED_MASK & 0x2) != 0 ) ) );

	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
		" [trig=0x%x] match_routed (hit = %x)\n",
		trig, hit);

    /* Match on one or more bits of the frame handler action mask. */
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_CONDITION_AMASK_1(trig, 0));
    actionMask = FM_GET_FIELD(*regPtr, HLP_TRIGGER_CONDITION_AMASK_1, HANDLER_ACTION_MASK);

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_CONDITION_AMASK_2(trig, 0));
    FM_SET_UNNAMED_FIELD64(actionMask, 
                         32, 
                         13, 
                         FM_GET_FIELD(*regPtr, 
                                      HLP_TRIGGER_CONDITION_AMASK_2, 
                                      HANDLER_ACTION_MASK));

    hit &= ( (state->AMASK & actionMask) != 0 );

	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
		" [trig=0x%x] match_amask (RETURN:  hit = %x) (AMASK=0x%llx)  << RETURN >> \n",
		trig, hit, state->AMASK);

    return hit;
}   /* end EvaluateTrigger */

/* populate actions. */
static hlp_modelTriggerActions * ExtractFields(hlp_model                *model,
                                               fm_int                   trig,
                                               hlp_modelTriggerActions  *actions)
{
    fm_uint32   *actionCfg1;
    fm_uint32   *actionCfg2;
    fm_uint32   *actionDmask;
    fm_uint32   *actionGlort;
    fm_uint32   *actionMirror;
    fm_uint32   *actionDrop;
    fm_uint32   *actionMetadata;
    fm_int       trigSlot;

    actionCfg1 = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_ACTION_CFG_1(trig, 0));
    actionCfg2 = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_ACTION_CFG_2(trig, 0));
    actionDmask = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_ACTION_DMASK(trig, 0));
    actionGlort = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_ACTION_GLORT(trig, 0));
    actionMirror = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_ACTION_MIRROR(trig, 0));
    actionDrop = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_ACTION_DROP(trig, 0));
    actionMetadata = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_ACTION_METADATA(trig, 0));

    actions->forwardingAction = FM_GET_FIELD(*actionCfg1, HLP_TRIGGER_ACTION_CFG_1, FORWARDING_ACTION);
    actions->newDestGlort = FM_GET_FIELD(*actionGlort, HLP_TRIGGER_ACTION_GLORT, NEW_DEST_GLORT);
    actions->newDestGlortMask = FM_GET_FIELD(*actionGlort, HLP_TRIGGER_ACTION_GLORT, NEW_DEST_GLORT_MASK);
    actions->newDestMask = FM_GET_FIELD(*actionDmask, HLP_TRIGGER_ACTION_DMASK, NEW_DEST_MASK);
    actions->filterDestMask = FM_GET_BIT(*actionDmask, HLP_TRIGGER_ACTION_DMASK, FILTER_DEST_MASK);
    actions->dropMask = FM_GET_FIELD(*actionDrop, HLP_TRIGGER_ACTION_DROP, DROP_MASK);
    actions->trapAction = FM_GET_FIELD(*actionCfg1, HLP_TRIGGER_ACTION_CFG_1, TRAP_ACTION);
    actions->mirroringAction0 = FM_GET_FIELD(*actionCfg1, HLP_TRIGGER_ACTION_CFG_1, MIRRORING_ACTION0);
    actions->mirroringAction1 = FM_GET_FIELD(*actionCfg1, HLP_TRIGGER_ACTION_CFG_1, MIRRORING_ACTION1);
    actions->mirrorProfileIndex0 = FM_GET_FIELD(*actionMirror, HLP_TRIGGER_ACTION_MIRROR, MIRROR_PROFILE_INDEX0);
    actions->mirrorProfileIndex1 = FM_GET_FIELD(*actionMirror, HLP_TRIGGER_ACTION_MIRROR, MIRROR_PROFILE_INDEX1);
    actions->TCAction = FM_GET_BIT(*actionCfg1, HLP_TRIGGER_ACTION_CFG_1, TC_ACTION);
    actions->newTC = FM_GET_FIELD(*actionCfg2, HLP_TRIGGER_ACTION_CFG_2, NEW_TC);
    actions->vlanAction = FM_GET_BIT(*actionCfg1, HLP_TRIGGER_ACTION_CFG_1, VLAN_ACTION);
    actions->newVlan = FM_GET_FIELD(*actionCfg2, HLP_TRIGGER_ACTION_CFG_2, NEW_EVID);
    actions->learningAction = FM_GET_FIELD(*actionCfg1, HLP_TRIGGER_ACTION_CFG_1, LEARNING_ACTION);
    actions->rateLimitAction = FM_GET_BIT(*actionCfg1, HLP_TRIGGER_ACTION_CFG_1, RATE_LIMIT_ACTION);
    actions->newRateLimitNum = FM_GET_FIELD(*actionCfg2, HLP_TRIGGER_ACTION_CFG_2, RATE_LIMIT_NUM);

    actions->policerAction = FM_GET_BIT(*actionCfg1, HLP_TRIGGER_ACTION_CFG_1, POLICER_ACTION);
    actions->egressL3DomainAction = FM_GET_BIT(*actionCfg1, HLP_TRIGGER_ACTION_CFG_1, EGRESS_L3_DOMAIN_ACTION);
    actions->egressL2DomainAction = FM_GET_BIT(*actionCfg1, HLP_TRIGGER_ACTION_CFG_1, EGRESS_L2_DOMAIN_ACTION);

    actions->metadataActionSlot = FM_GET_FIELD(*actionCfg1, HLP_TRIGGER_ACTION_CFG_1, METADATA_ACTION_SLOT);
    for(int i=0; i<4; i++)
    {
        if(i==actions->metadataActionSlot)
        {
            actions->metadataAction[i] = FM_GET_FIELD(*actionCfg1, HLP_TRIGGER_ACTION_CFG_1, METADATA_ACTION);
            actions->metadataMask[i] = FM_ARRAY_GET_FIELD(actionMetadata, HLP_TRIGGER_ACTION_METADATA, MASK);
            actions->metadataValue[i] = FM_ARRAY_GET_FIELD(actionMetadata, HLP_TRIGGER_ACTION_METADATA, VALUE);
            actions->metadataSource[i] = FM_ARRAY_GET_FIELD(actionMetadata, HLP_TRIGGER_ACTION_METADATA, SOURCE);
            actions->metadataOffset[i] = FM_ARRAY_GET_FIELD(actionMetadata, HLP_TRIGGER_ACTION_METADATA, OFFSET);
        } else {
            actions->metadataAction[i] = 0;
            actions->metadataMask[i] = 0;
            actions->metadataValue[i] = 0;
            actions->metadataSource[i] = 0;
            actions->metadataOffset[i] = 0;
        }
    }

    actions->noModifyAction = FM_GET_BIT(*actionCfg1, HLP_TRIGGER_ACTION_CFG_1, NO_MODIFY_ACTION);
    actions->metadataMaskSel = FM_GET_BIT(*actionCfg2, HLP_TRIGGER_ACTION_CFG_2, METADATA_MASK_SEL);

    actions->trapCode = FM_GET_FIELD(*actionCfg2, HLP_TRIGGER_ACTION_CFG_2, TRAP_CODE);

    /*
    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
        "trig=%d mirroringAction0=%d mirroringAction1=%d mirrorProfileIndex0=%d mirrorProfileIndex1=%d\n",
        trig, actions->mirroringAction0, actions->mirroringAction1,
        actions->mirrorProfileIndex0, actions->mirrorProfileIndex1);
    */

    return actions;
}   /* end ExtractFields */


static void ResolveTriggers(hlp_model *             model,
                            fm_uint64               hitMaskHi,
							fm_uint64               hitMaskLo,
                            hlp_modelTriggerActions *lo)
{
    hlp_modelState *            state = &model->packetState;
    hlp_modelTriggerActions     hi;
    hlpTriggerConditionCfg      condCfg;
    fm_int                      i;
    fm_bool                     precWinner = FALSE;

    state->TRIG_HIT_MASK_RESOLVED_HI = hitMaskHi;
    state->TRIG_HIT_MASK_RESOLVED_LO = hitMaskLo;

    FM_CLEAR(*lo);

    for (i = 0; i < HLP_MODEL_TRIGGERS_COUNT; i++)
    {
        GetConditionCfg(model, i, &condCfg);
        precWinner &=   condCfg.MATCH_BY_PRECEDENCE;

        /*                     */
       	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
            "precWinner[i=%2d]=%1d, MatchByPrecedence=%1d; hitMaskHi=0x%llx hitMaskLo=0x%llx; (Before) state->TRIG_HIT_MASK_RESOLVED_HI=0x%llx state->TRIG_HIT_MASK_RESOLVED_LO=0x%llx\n",
            i, precWinner,   condCfg.MATCH_BY_PRECEDENCE, hitMaskHi, hitMaskLo,
			state->TRIG_HIT_MASK_RESOLVED_HI,
			state->TRIG_HIT_MASK_RESOLVED_LO);
        /*                     */

        if (i < 64)
        {
            if ( ( (hitMaskLo & (FM_LITERAL_U64(1) << i)) != 0 ) && !precWinner )
            {
            	precWinner = TRUE;
                ApplyPrecedenceResolution(lo, i, ExtractFields(model, i, &hi));
            }
            else if ( ( (hitMaskLo & (FM_LITERAL_U64(1) << i)) != 0 ) && precWinner )
            {
                state->TRIG_HIT_MASK_RESOLVED_LO &= ~(FM_LITERAL_U64(1) << i);
            }
        }
        else
        {
            if ( ( (hitMaskHi & (FM_LITERAL_U64(1) << (i - 64))) != 0 ) && !precWinner )
            {
            	precWinner = TRUE;
                ApplyPrecedenceResolution(lo, i, ExtractFields(model, i, &hi));
            }
            else if ( ( (hitMaskHi & (FM_LITERAL_U64(1) << (i - 64))) != 0 ) && precWinner )
            {
                state->TRIG_HIT_MASK_RESOLVED_HI &= ~(FM_LITERAL_U64(1) << (i - 64));
            }
        }

       	WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
            "(After) state->TRIG_HIT_MASK_RESOLVED_HI[i=%2d]=0x%llx state->TRIG_HIT_MASK_RESOLVED_LO[i=%2d]=0x%llx\n",
            i, state->TRIG_HIT_MASK_RESOLVED_HI,
			i, state->TRIG_HIT_MASK_RESOLVED_LO);
    }
}   /* end ResolveTriggers */

/*****************************************************************************/
/** TriggersStatsUpdate
 *
 *****************************************************************************/

static void TriggersStatsUpdate(hlp_model *              model,
                                fm_uint64                hitMaskHi,
								fm_uint64                hitMaskLo,
                                hlp_modelTriggerActions *actions)
{
    hlp_modelState *state = &model->packetState;
    fm_status      status = FM_OK;
    fm_int         i;

    FM_NOT_USED(actions);

    for (i = 0; i < HLP_MODEL_TRIGGERS_COUNT; i++)
    {
        if (i < 64)
        {
            if ( (hitMaskLo & (FM_LITERAL_U64(1) << i)) != 0 )
            {
                if ( (state->TRIG_HIT_MASK_RESOLVED_LO & (FM_LITERAL_U64(1) << i)) )
                {
                    status = IncrementTrigCounter(model, i);
                }
            }
        }
        else
        {
            if ( (hitMaskHi & (FM_LITERAL_U64(1) << (i - 64))) != 0 )
            {
                if ( (state->TRIG_HIT_MASK_RESOLVED_HI & (FM_LITERAL_U64(1) << (i - 64))) )
                {
                    status = IncrementTrigCounter(model, i);
                }
            }
        }
    }
}   /* end TriggersStatsUpdate */

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

fm_status hlpModelTriggers(hlp_model *model)
{
    hlp_modelState *        state = &model->packetState;
    hlp_modelTriggerActions actions;
    fm_status                   status = FM_OK;
    fm_uint64                   hitMaskHi = FM_LITERAL_U64(0);
    fm_uint64                   hitMaskLo = FM_LITERAL_U64(0);
    fm_int                      i;
    fm_uint32 *                 regPtr;
    fm_byte 					fwdIpEntryCount, fwdImEntryCount;
    fm_uint64 					fwdIpTrigger, fwdImTrigger;
    fm_byte 					globalIntFwd = 0;
    fm_uint64                   triggerIpHi, newTriggerIpHi;
    fm_uint64                   triggerIpLo;
    fm_uint64                   triggerImHi;
    fm_uint64                   triggerImLo;

    if(testPlusArgs("HLP_TRIGGERS_WM_PRINT_VERBOSE") >= 0)
        triggersDisplayVerbose = testPlusArgs("HLP_TRIGGERS_WM_PRINT_VERBOSE");
    WM_DISPLAY(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS, "triggersDisplayVerbose=%0d\n", triggersDisplayVerbose)

    /* EvaluateTrigger first */
    for (i = 0; i < HLP_MODEL_TRIGGERS_COUNT; i++)
    {
        if (i < 64)
        {
            if (EvaluateTrigger(model, i))
            {
                hitMaskLo |= FM_LITERAL_U64(1) << i;
            }
        }
        else
        {
            if (EvaluateTrigger(model, i))
            {
                hitMaskHi |= FM_LITERAL_U64(1) << (i - 64);
            }
        }
    }

    ResolveTriggers(model, hitMaskHi, hitMaskLo, &actions);

    ApplyTriggers(model, &actions);

    TriggersStatsUpdate(model, hitMaskHi, hitMaskLo, &actions);

    if (model->allowStateChange)
    {
        /*****************************************************
         * Inform the application of a new TRIGGERS hit event.
         ****************************************************/
        /* TRIGGER_IP[0]/[1] - each of 48bits*/
        regPtr = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_IP(0, 0));
        triggerIpLo = FM_ARRAY_GET_FIELD64(regPtr, HLP_TRIGGER_IP, PENDING);
		WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
					"(pre_write) TRIGGER_IP[0]=0x%llx\n",
					triggerIpLo);
        triggerIpLo |= ((FM_LITERAL_U64(1)<<48)-1) & state->TRIG_HIT_MASK_RESOLVED_LO;
        FM_ARRAY_SET_FIELD64(regPtr, HLP_TRIGGER_IP, PENDING, triggerIpLo);
		WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
					"(post_write) TRIGGER_IP[0]=0x%llx\n",
					triggerIpLo);

        regPtr = FM_MODEL_GET_REG_PTR(model, HLP_TRIGGER_IP(1, 0));
        triggerIpHi = FM_ARRAY_GET_FIELD64(regPtr, HLP_TRIGGER_IP, PENDING);
		WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
					"(pre_write) TRIGGER_IP[1]=0x%llx\n",
					triggerIpHi);
        newTriggerIpHi = 0;
        /*take upper 16 bits of resolved_hit_lo*/
        FM_SET_UNNAMED_FIELD64(newTriggerIpHi, 
                               0, 
                               16, 
                               state->TRIG_HIT_MASK_RESOLVED_LO >> 48);
        /*take lower 36 bits of resolved_hit_hi*/
        FM_SET_UNNAMED_FIELD64(newTriggerIpHi, 
                               (64-48), 
                               36, 
                               ((FM_LITERAL_U64(1)<<36)-1) & state->TRIG_HIT_MASK_RESOLVED_HI);
        triggerIpHi |= newTriggerIpHi;
        FM_ARRAY_SET_FIELD64(regPtr, HLP_TRIGGER_IP, PENDING, triggerIpHi);
		WM_DISPLAY3(triggersDisplayVerbose, FM_LOG_CAT_MODEL_TRIGGERS,
					"(post_write) TRIGGER_IP[1]=0x%llx\n",
					triggerIpHi);

        /* propagate trigger interrupts to FWD_IP and GLOBAL_INTERRUPT regs */

        regPtr      = FM_MODEL_GET_REG_PTR(model,  HLP_TRIGGER_IM(0, 0));
        triggerImLo = FM_ARRAY_GET_FIELD64(regPtr, HLP_TRIGGER_IM, MASK);

        regPtr      = FM_MODEL_GET_REG_PTR(model,  HLP_TRIGGER_IM(1, 0));
        triggerImHi = FM_ARRAY_GET_FIELD64(regPtr, HLP_TRIGGER_IM, MASK);
        
        regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FWD_IP(0));
        FM_ARRAY_SET_BIT(regPtr, HLP_FWD_IP, TRIGGER,
                         (((triggerIpHi & ~triggerImHi) != 0) |
						  ((triggerIpLo & ~triggerImLo) != 0))
						);
        fwdIpTrigger = FM_GET_BIT(*regPtr, HLP_FWD_IP, TRIGGER);
        fwdIpEntryCount = FM_GET_BIT(*regPtr, HLP_FWD_IP, ENTRY_COUNT);

        regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FWD_IM(0));
        fwdImTrigger = FM_GET_BIT(*regPtr, HLP_FWD_IM, TRIGGER);
        fwdImEntryCount = FM_GET_BIT(*regPtr, HLP_FWD_IM, ENTRY_COUNT);

        regPtr = FM_MODEL_GET_REG_PTR(model, HLP_GLOBAL_INTERRUPT(0));
//        FM_ARRAY_SET_BIT(regPtr, HLP_GLOBAL_INTERRUPT, FWD,
//                         ((fwdIpTrigger & ~fwdImTrigger) != 0));
        if ((fwdIpEntryCount & ~fwdImEntryCount) || (fwdIpTrigger & ~fwdImTrigger))
        {
        	globalIntFwd = 1;
		}
		if ((fwdIpEntryCount & ~fwdImEntryCount) == 0 && (fwdIpTrigger & ~fwdImTrigger) == 0)
		{
			globalIntFwd = 0;
		}
        FM_ARRAY_SET_BIT(regPtr, HLP_GLOBAL_INTERRUPT, FWD, globalIntFwd);
//FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "aaverchedebug: hlpModelTriggers: HLP_GLOBAL_INTERRUPT.FWD=%0d. \n", globalIntFwd);
    }

    state->ACTION = state->TRIGGERS.action;
    state->L2_EVID1 = state->TRIGGERS.vlan;
    state->QOS_SWPRI = state->TRIGGERS.TC;
    state->DMASK = state->TRIGGERS.destMask;
    state->IDGLORT = state->TRIGGERS.destGlort;
    state->NO_MODIFY = state->TRIGGERS.noModifyAction;
    
    state->MIRROR1_PROFILE_V = state->TRIGGERS.mirror1ProfileV;
    state->MIRROR1_PROFILE_IDX = state->TRIGGERS.mirror1ProfileIdx;
    state->MIRROR0_PROFILE_V = state->TRIGGERS.mirror0ProfileV;
    state->MIRROR0_PROFILE_IDX = state->TRIGGERS.mirror0ProfileIdx;
    
    if (!state->DISABLE_DBG_DUMP)
        HLP_MODEL_DBG_DUMP(model, hlpModelDbgDumpTriggers);

    return status;
}   /* end hlpModelTriggers */

