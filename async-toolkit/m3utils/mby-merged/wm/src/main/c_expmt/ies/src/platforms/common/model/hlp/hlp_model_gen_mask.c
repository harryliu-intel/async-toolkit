/* vim:et:sw=4:ts=4:sw=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_gen_mask.c
 * Creation Date:   June 25, 2012
 * Description:     GEN_MASK1 and GEN_MASK2 stages of HLP white model
 *
 * INTEL CONFIDENTIAL
 * Copyright 2012 Intel Corporation. All Rights Reserved.
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
fm_int genMaskDisplayVerbose = 1;

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Function Prototypes
 *****************************************************************************/

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** GetLagCfg 
 * \ingroup intModel
 *
 * \desc            Reads FWD_LAG_CFG register data from register cache and
 *                  populates corresponding register struct
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in,out]   lagCfg is the struct for LAG_CFG that
 *                  is populated with data from register cache
 *
 *****************************************************************************/
static void GetLagCfg(hlp_model        *model,
                       hlpFwdLagCfg     *lagCfg)
{
    fm_uint32       *regPtr;
    hlp_modelState  *state;
    
    state = &model->packetState;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FWD_LAG_CFG(state->RX_PORT, 0));

    lagCfg->IN_LAG = FM_GET_BIT(*regPtr, HLP_FWD_LAG_CFG, IN_LAG);
    lagCfg->HASH_ROTATION = FM_GET_BIT(*regPtr, HLP_FWD_LAG_CFG, HASH_ROTATION);
    lagCfg->INDEX = FM_GET_FIELD(*regPtr, HLP_FWD_LAG_CFG, INDEX);
    lagCfg->LAG_SIZE = FM_GET_FIELD(*regPtr, HLP_FWD_LAG_CFG, LAG_SIZE);
} /* GetLagCfg */

/*****************************************************************************/
/** GetPortCfg1 
 * \ingroup intModel
 *
 * \desc            Reads PORT_CFG_1 register data from register cache and
 *                  populates corresponding register struct
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in,out]   portCfg is the struct for PORT_CFG_1 that
 *                  is populated with data from register cache
 *
 *****************************************************************************/
static void GetPortCfg1(hlp_model       *model,
                        hlpFwdPortCfg1  *portCfg)
{
    fm_uint32       *regPtr;
    hlp_modelState  *state;
    
    state = &model->packetState;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FWD_PORT_CFG_1(state->RX_PORT, 0));

    portCfg->LEARNING_ENABLE     = FM_GET_BIT(*regPtr, HLP_FWD_PORT_CFG_1, LEARNING_ENABLE);
    portCfg->FILTER_VLAN_INGRESS  = FM_GET_BIT(*regPtr, HLP_FWD_PORT_CFG_1, FILTER_VLAN_INGRESS);
    portCfg->DESTINATION_MASK    = FM_GET_FIELD(*regPtr, HLP_FWD_PORT_CFG_1, DESTINATION_MASK);

} /* GetPortCfg1 */

/*****************************************************************************/
/** GetPortCfg2
 * \ingroup intModel
 *
 * \desc            Reads PORT_CFG_2 register data from register cache and
 *                  populates corresponding register struct
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in,out]   portCfg is the struct for PORT_CFG_2 that
 *                  is populated with data from register cache
 *
 *****************************************************************************/
static void GetPortCfg2(hlp_model       *model,
                        hlpFwdPortCfg2  *portCfg)
{
    fm_uint32       *regPtr;
    hlp_modelState  *state;
    
    state = &model->packetState;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FWD_PORT_CFG_2(state->L2_EDOMAIN, 0));

    portCfg->DESTINATION_MASK     = FM_GET_FIELD(*regPtr, HLP_FWD_PORT_CFG_2, DESTINATION_MASK);

} /* GetPortCfg2 */


/*****************************************************************************/
/** GetSysCfg1 
 * \ingroup intModel
 *
 * \desc            Reads SYS_CFG_1 register data from register cache and
 *                  populates corresponding register struct
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in,out]   sysCfg is the struct for SYS_CFG_1 that
 *                  is populated with data from register cache
 *
 *****************************************************************************/
static void GetSysCfg1(hlp_model       *model,
                        hlpFwdSysCfg1  *sysCfg)
{
    fm_uint32       *regPtr;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FWD_SYS_CFG_1(0));

    sysCfg->STORE_TRAP_ACTION = FM_GET_BIT(*regPtr, HLP_FWD_SYS_CFG_1, STORE_TRAP_ACTION);
    sysCfg->DROP_MAC_CTRL_ETHERTYPE = FM_GET_BIT(*regPtr, HLP_FWD_SYS_CFG_1, DROP_MAC_CTRL_ETHERTYPE);
    sysCfg->DROP_INVALID_SMAC = FM_GET_BIT(*regPtr, HLP_FWD_SYS_CFG_1, DROP_INVALID_SMAC);
    sysCfg->ENABLE_TRAP_PLUS_LOG = FM_GET_BIT(*regPtr, HLP_FWD_SYS_CFG_1, ENABLE_TRAP_PLUS_LOG);
    sysCfg->TRAP_MTU_VIOLATIONS = FM_GET_BIT(*regPtr, HLP_FWD_SYS_CFG_1, TRAP_MTU_VIOLATIONS);
    
} /* GetSysCfg1 */

/*****************************************************************************/
/** GetSysCfgRouter 
 * \ingroup intModel
 *
 * \desc            Reads SYS_CFG_ROUTER register data from register cache and
 *                  populates corresponding register struct
 *
 * \param[in]       model points to the switch data structure.
 *
 * \param[in,out]   sysCfgRouter is the struct for SYS_CFG_ROUTER that
 *                  is populated with data from register cache
 *
 *****************************************************************************/
static void GetSysCfgRouter(hlp_model           *model,
                            hlpFwdSysCfgRouter  *sysCfgRouter)
{
    fm_uint32       *regPtr;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FWD_SYS_CFG_ROUTER(0));

    sysCfgRouter->TRAP_IP_OPTIONS = FM_GET_BIT(*regPtr, HLP_FWD_SYS_CFG_ROUTER, TRAP_IP_OPTIONS);
    sysCfgRouter->TRAP_TTL1 = FM_GET_FIELD(*regPtr, HLP_FWD_SYS_CFG_ROUTER, TRAP_TTL1);

} /* GetSysCfgRouter */



/*****************************************************************************/
/** fmModelIsCpuMacAddress
 * \ingroup intModel
 *
 * \desc           Utility function to determine whether the current frame 
 *                 in the pipeline has a CPU MAC address.  It is made public
 *                 since it is used in these stages:
 *                   - gen_mask compares a destination mac address .
 *                   - learning compares a source mac address .
 *
 * \param[in]      model points to the switch model state.
 *
 * \param[in]      macAddr is the address of the current frame.
 *
 * \return         Comparison of macAddrIn and address field of CPU_MAC
 *
 *****************************************************************************/
fm_bool fmModelIsCpuMacAddress(hlp_model *model, fm_macaddr macAddr)
{
    fm_uint32   *regPtr;
 
    fm_macaddr  cpuMac;
 
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FWD_CPU_MAC(0));
    cpuMac = FM_ARRAY_GET_FIELD64(regPtr, HLP_FWD_CPU_MAC, MAC_ADDR);
    
    return (macAddr == cpuMac);
}   /* end fmModelIsCpuMacAddress */


/*****************************************************************************/
/** IsIeeeFrame
 * \ingroup intModel
 *
 * \desc           To determine whether the current frame in pipeline has
 *                 IEEE reserved mac address as destination mac address .
 *
 * \param[in]      model points to the switch model state.
 *
 * \return         TRUE if DMAC is a IEEE reserved MAC address.
 * \return         FALSE otherwise.
 *
 *****************************************************************************/
static fm_bool IsIeeeFrame(hlp_model *model)
{
    hlp_modelState  *state;
    fm_uint64       lsb;
    fm_uint64       prefix;
    
    state = &model->packetState;
    prefix = state->L2_DMAC & ~FM_LITERAL_U64(0xFF);

    if (prefix == HLP_MODEL_DMAC_IEEE_PREFIX)
    {
        lsb = state->L2_DMAC & FM_LITERAL_U64(0xFF);

        if ( ( (lsb >= 0x04) && (lsb <= 0x2F) ) &&
             !( (lsb == 0x20) || (lsb == 0x21) ) )
        {
            return TRUE;
        }
    }
    return FALSE;

}   /* end IsIeeeFrame */


/*****************************************************************************/
/** UpdateActionMask
 * \ingroup intModel
 *
 * \desc            Updates the action mask with extra bits for the TRIGGERS
 *                  stage.
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       logIpMcTtl is a boolean indicating whether IP multicast
 *                  frames whose time-to-live field is equal to or less than 1
 *                  are to be logged to the CPU.
 *
 *****************************************************************************/
static void UpdateActionMask(hlp_model *model, fm_bool logIpMcTtl)
{
    hlp_modelState *state = &model->packetState;

    FM_NOT_USED(logIpMcTtl);

    if (state->LOG_AMASK & HLP_MODEL_LOG_TYPE_FFU)
    {
        state->AMASK |= HLP_MODEL_AMASK_LOG_INGRESS_FFU;
    }

    if (state->LOG_AMASK & HLP_MODEL_LOG_TYPE_RESERVED_MAC)
    {
        state->AMASK |= HLP_MODEL_AMASK_LOG_MAC_CTRL;
    }

    if (state->LOG_AMASK & HLP_MODEL_LOG_TYPE_ARP_REDIRECT)
    {
        state->AMASK |= HLP_MODEL_AMASK_LOG_ARP_REDIRECT;
    }

    if (state->LOG_AMASK & HLP_MODEL_LOG_TYPE_ICMP)
    {
        state->AMASK |= HLP_MODEL_AMASK_LOG_IP_ICMP;
    }

    if (state->LOG_AMASK & HLP_MODEL_LOG_TYPE_TTL_IP_MC)
    {
        state->AMASK |= HLP_MODEL_AMASK_LOG_IP_TTL;
    }

    if (state->RX_MIRROR)
    {
        state->AMASK |= HLP_MODEL_AMASK_MIRROR_INGRESS_FFU;
    }


}   /* end UpdateActionMask */


/*****************************************************************************/
/** ProcessEgressSTPState
 * \ingroup intModel
 *
 * \desc           Updated destination mask based on egress STP state.
 *
 * \param[in]      model points to the switch model state.
 *
 *****************************************************************************/
static void ProcessEgressStpState(hlp_model *model)
{
    hlp_modelState *state = &model->packetState;

    if ( ( (state->AMASK & HLP_MODEL_AMASK_SPECIAL) == 0) &&
         (state->DMASK != 0) )
    {
        state->DMASK &= state->L2_EFID1_STATE;
        WM_DISPLAY2(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                   "STP egress filtering: DMASK[0x%x]\n",
                      state->DMASK);

        if (state->DMASK == 0)
        {
            /* Egress STP violation */
            state->AMASK |= HLP_MODEL_AMASK_DROP_EGRESS_STP;
            state->ACTION = HLP_MODEL_ACTION_DROP_STP;
            WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                         "STP egress violation: Dropping frame\n");
        }
    }

}   /* end ProcessEgressStpState */


/*****************************************************************************/
/** ProcessFiltering
 * \ingroup intModel
 *
 * \desc           Updates the destination mask and action mask by processing
 *                 packet type and Ingress VLAN membership.
 *
 * \param[in]      model points to the switch model state.
 *
 *****************************************************************************/
static void ProcessFiltering(hlp_model *model)
{
    hlp_modelState  *state;
    hlpFwdPortCfg1  portCfg1;
    hlpFwdPortCfg2  portCfg2;

    
    state = &model->packetState;
    
    GetPortCfg1(model, &portCfg1); //HLP_FWD_PORT_CFG_1
    GetPortCfg2(model, &portCfg2); //HLP_FWD_PORT_CFG_2

    /* Added in RRC for setting FCLASS */
    if ( fmModelIsBroadcastMacAddress(state->L2_DMAC) )
    { 
        state->FCLASS = HLP_MODEL_FCLASS_BROADCAST;
        state->XCAST = 2;
    }
    else if ( fmModelIsMulticastMacAddress(state->L2_DMAC) )
    { 
        state->FCLASS = HLP_MODEL_FCLASS_MULTICAST;
        state->XCAST = 1;

    }
    else if ( fmModelIsUnicastMacAddress(state->L2_DMAC) )
    { 
        state->FCLASS = HLP_MODEL_FCLASS_UNICAST;
        state->XCAST = 0;
    }

    state->DMASK = HLP_MODEL_DEFAULT_DMASK;
    
    if (state->FLOOD_FORWARDED)
    {
        state->AMASK |= HLP_MODEL_AMASK_FLOOD;
    }

    if (state->GLORT_FORWARDED)
    {
        state->AMASK |= HLP_MODEL_AMASK_GLORT;
    }

    WM_DISPLAY3(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                 "Init. to 1's, AND'd in next stage: DMASK[ox%x]\n",
                 state->DMASK);

    /* Perform port based filtering for switched packets. */
    state->DMASK &= portCfg1.DESTINATION_MASK;
    WM_DISPLAY2(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                 "Port based filtering: DMASK[0x%x]\n",
                 state->DMASK);
    
    state->DMASK &= portCfg2.DESTINATION_MASK;
    WM_DISPLAY2(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                 "Egress L2 Domain based port filtering: DMASK[0x%x]\n",
                 state->DMASK);

    /* Ingress VLAN reflection check. */
    if ( !state->MARK_ROUTED && 
         !state->L2_IVLAN1_REFLECT &&
         !state->TARGETED_DETERMINISTIC)
    {
        state->DMASK &= ~(FM_LITERAL_U64(1) << state->RX_PORT);
        WM_DISPLAY2(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                     "VLAN reflection filtering: DMASK[0x%x]\n",
                      state->DMASK);
    }

    if ( state->DMASK == 0 )
    {
        state->AMASK |= HLP_MODEL_AMASK_DROP_LOOPBACK;
        WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                     "Port based filtering & VLAN reflection prevention : Dropping frame (AMASK_DROP_LB, DMASK[0x%x])\n",
                     state->DMASK);
    }

    state->PRE_RESOLVE_DMASK = state->DMASK & state->GLORT_DMASK;  

    WM_DISPLAY2(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                 "GLORT based filtering: dstMaskACL[0x%x]\n",
                 state->PRE_RESOLVE_DMASK);

    if (state->PRE_RESOLVE_DMASK == 0)
    {
        /* Egress VLAN membership check */
        if (state->GLORT_DMASK == 0)
        {
            state->AMASK |= HLP_MODEL_AMASK_DROP_NULL_GLORTDEST;
            WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                            "Null Glort Dest: Dropping frame\n");

            if (state->FLOOD_FORWARDED == 1)
            {
                state->AMASK |= HLP_MODEL_AMASK_DROP_DLF;
                WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                            "Null Glort Dest & Flood Forwarded: Dropping frame (DLF)\n");
            }
        }
        else
        {
            /* Don't set amask loopback when also setting null dest. (rtl 276931) */
            state->AMASK |= HLP_MODEL_AMASK_DROP_LOOPBACK;
            WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                         "Loopback (port or VLAN refl. dis.): Dropping frame [PRE_RESOLVE_DMASK=0x%x]\n",
                         state->PRE_RESOLVE_DMASK);
        }
    }
    else if (!state->TARGETED_DETERMINISTIC)
    {
        state->PRE_RESOLVE_DMASK &= state->L2_EVLAN1_MEMBERSHIP;
        WM_DISPLAY2(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                     "VLAN egress filtering: dstMaskACL &= L2_EVLAN1_MEMBERSHIP [0x%x]\n",
                      state->PRE_RESOLVE_DMASK);
        if (state->PRE_RESOLVE_DMASK == 0)
        {
            state->AMASK |= HLP_MODEL_AMASK_DROP_EV;
            WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                         "VLAN egress violation: Dropping frame\n");
        }
        
    }

}   /* end ProcessFiltering */


/*****************************************************************************/
/** ProcessPortSecurity
 * \ingroup intModel
 *
 * \desc           Port and MAC security Set the appropriate bits in the action
 *                 mask.
 *
 * \param[in]      model points to the switch model state.
 *
 *****************************************************************************/
static void ProcessPortSecurity(hlp_model *model)
{
    hlp_modelState  *state;
    hlpFwdPortCfg1  portCfg1;   
    
    state = &model->packetState;
    
    GetPortCfg1(model, &portCfg1); //HLP_FWD_PORT_CFG_1

    /* Ingress VLAN membership check. */
    if ( !state->L2_IVLAN1_MEMBERSHIP && portCfg1.FILTER_VLAN_INGRESS )
    {
        state->AMASK |= HLP_MODEL_AMASK_DROP_IV;
        WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                     "VLAN ingress violation: Dropping frame\n");
    }
    
    if (state->SA_HIT && (state->SA_RESULT.S_GLORT != state->CSGLORT))
    {
        //if (!state->SA_RESULT->secure)
        //{
            state->MAC_MOVED = TRUE;
        //}

        //if (state->SA_RESULT->secure)
        //{
        //    state->AMASK |= HLP_MODEL_AMASK_DROP_SEC_PORT;

        //    /* eac. **FIXME**:  double-check per spec. & bug changes. */
        //    /* state->TCN_FIFO_MASK |= HLP_MODEL_TCN_EVENT_PORT_SV; */

        //    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
        //                 "Port security violation: Dropping frame\n");
        //}
    }

    switch (state->SV_DROP)
    {
    	case HLP_MODEL_SV_MOVE_DROP_ADDR:
    			state->AMASK |= HLP_MODEL_AMASK_DROP_SEC_ADDR;
    			WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
    						 "secure address violation: Dropping frame\n");
    			break;
    	case HLP_MODEL_SV_MOVE_DROP_PORT:
    			state->AMASK |= HLP_MODEL_AMASK_DROP_SEC_PORT;
    			WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
    						 "secure port violation: Dropping frame\n");
    			break;
    	case HLP_MODEL_SV_MOVE_DROP_STATIC:
    			state->AMASK |= HLP_MODEL_AMASK_DROP_STATIC_ADDR;
    			WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
    						 "static address violation: Dropping frame\n");
    			break;
    	default:
    		break;
    }

    /* Ingress spanning tree check. */
    switch (state->L2_IFID1_STATE)
    {
        case HLP_MODEL_STP_STATE_DISABLE:
        case HLP_MODEL_STP_STATE_LISTENING:
                state->AMASK |= HLP_MODEL_AMASK_DROP_INGRESS_STP_NON_LEARN;
                WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                             "STP ingress (non-learning) violation: Dropping frame\n");
                break;

        case HLP_MODEL_STP_STATE_LEARNING:
                state->AMASK |= HLP_MODEL_AMASK_DROP_INGRESS_STP_LEARN;
                WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                             "STP ingress (learning) violation: Dropping frame\n");
                break;

        default:
            break;
    }

}   /* end ProcessPortSecurity */


/*****************************************************************************/
/** ProcessFfuFlags
 * \ingroup intModel
 *
 * \desc           Process FFU flags and updates appropriate states in the
 *                 packet state.
 *
 * \param[in]      model points to the switch model state.
 *
 *****************************************************************************/
static void ProcessFfuFlags(hlp_model *model)
{
    hlp_modelState  *state;
    fm_uint32       *regPtr;

    state = &model->packetState;

    if (state->FFU_FLAGS.drop)
    {
        state->AMASK |= HLP_MODEL_AMASK_DROP_FFU;
        WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                     "FFU flag processing: Dropping frame\n");
    }
    if (state->FFU_FLAGS.trap)
    {
        state->AMASK |= HLP_MODEL_AMASK_TRAP_FFU;
        WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                     "FFU flag processing: Trapping frame\n");
    }
    if (state->FFU_FLAGS.log)
    {
        state->LOG_AMASK |= HLP_MODEL_LOG_TYPE_FFU;
        WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                     "FFU flag processing: Logging frame\n");
    }
    if (state->FFU_FLAGS.rx_mirror)
    {
        state->RX_MIRROR = TRUE;
        /* Set in ActionRes RTL, but set up for Trig here. */
        state->MIRROR1_PROFILE_V   = state->RX_MIRROR;
        regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FWD_RX_MIRROR_CFG(0));

        state->MIRROR1_PROFILE_IDX = FM_GET_FIELD(*regPtr, HLP_FWD_RX_MIRROR_CFG, MIRROR_PROFILE_IDX);
        WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                     "FFU flag processing: RX mirroring frame\n");
    }

}   /* end ProcessFfuFlags */


/*****************************************************************************/
/** ProcessTraps
 * \ingroup intModel
 *
 * \desc            Update action mask of the packet state based on the reason
 *                  for trapped frame.
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[out]      logIpMcTtl points to caller-allocated storage where this
 *                  function places the decision whether IP multicast frames
 *                  whose time-to-live field is equal to or less than 1 are to
 *                  be logged to the CPU.
 *
 *****************************************************************************/
static void ProcessTraps(hlp_model *model, fm_bool *logIpMcTtl)
{
    hlp_modelState      *state;
    hlpFwdSysCfg1       sysCfg1;
    hlpFwdSysCfgRouter  sysCfgRouter;
    fm_byte             rmc_idx;
    fm_byte             action;
    fm_uint32           *rmcPtr;
    fm_uint32           *rmtpPtr;
    fm_bool             isCpuMac;
    
    state = &model->packetState;
    
    GetSysCfg1(model, &sysCfg1);
    GetSysCfgRouter(model, &sysCfgRouter);

    rmcPtr = FM_MODEL_GET_REG_PTR(model, HLP_FWD_IEEE_RESERVED_MAC_ACTION(0));
    rmtpPtr = FM_MODEL_GET_REG_PTR(model, HLP_FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY(0));

    isCpuMac = fmModelIsCpuMacAddress(model, state->L2_DMAC);
//    if (isCpuMac == TRUE)
    if ((isCpuMac == TRUE) && (!((state->L2_DMAC == 0) && state->PARSER_INFO.window_parse_v)))
    {
        state->AMASK |= HLP_MODEL_AMASK_TRAP_CPU_ADDR;
        WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,"Trapping CPU addressed frame\n");
    }

    /* Special Packet Handling */

    if ( (state->L2_DMAC & HLP_MODEL_SPECIAL_DMASK) ==
         (HLP_MODEL_DMAC_IEEE_PREFIX & HLP_MODEL_SPECIAL_DMASK) )
    {
        rmc_idx = (fm_byte) ( state->L2_DMAC & 0x00000000003F );

        action = 0xFF;

        if (rmc_idx < 64)
        {
            action = FM_ARRAY_GET_UNNAMED_FIELD(rmcPtr, 2 * rmc_idx, 2);  
        }
        else if (rmc_idx > 64)
        {
        	WM_DISPLAY3(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,"Index to IEEE_RESERVED_MAC_ACTION > 64\n");
        }

        switch (action)
        {
            case HLP_IEEE_RESERVED_MAC_ACTION_ACTION_SWITCHNORMALLY:
                state->AMASK |= HLP_MODEL_AMASK_SWITCH_RESERVED_MAC;
                break;

            case HLP_IEEE_RESERVED_MAC_ACTION_ACTION_TRAP:
                state->AMASK |= ( FM_ARRAY_GET_UNNAMED_BIT(rmtpPtr, rmc_idx) ?
                                  HLP_MODEL_AMASK_TRAP_RESERVED_MAC_REMAP :
                                  HLP_MODEL_AMASK_TRAP_RESERVED_MAC );
                break;

            case HLP_IEEE_RESERVED_MAC_ACTION_ACTION_DROP:
                state->AMASK |= HLP_MODEL_AMASK_DROP_RESERVED_MAC;
                break;

            case HLP_IEEE_RESERVED_MAC_ACTION_ACTION_LOG:
                state->AMASK |= HLP_MODEL_AMASK_LOG_MAC_CTRL;
                state->LOG_AMASK |= HLP_MODEL_LOG_TYPE_RESERVED_MAC;
                break;

            default:
            	WM_DISPLAY3(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,"Illegal IEEE_RESERVED_MAC action.\n");
                break;
        }
    }


    if (state->MARK_ROUTED)
    {
        if (state->IP_MCAST_IDX == 0)
        {
            if ( state->L2_IVID1 == ( state->L2_EVID1 & 0xFFF) )
            {
                state->LOG_AMASK |= HLP_MODEL_LOG_TYPE_ARP_REDIRECT;
            }
        }
    }

    if(state->DROP_TTL)
    {
        if (state->IP_MCAST_IDX == 0)
        {
            if ( sysCfgRouter.TRAP_TTL1 == 0 )
            {
                state->AMASK |= HLP_MODEL_AMASK_DROP_TTL;
            }
            else if ( sysCfgRouter.TRAP_TTL1 == 1 )
            {
                state->AMASK |= ( state->TRAP_ICMP ?
                                HLP_MODEL_AMASK_TRAP_ICMP_TTL :
                                HLP_MODEL_AMASK_DROP_TTL );
            }
            else if ( sysCfgRouter.TRAP_TTL1 == 2 )
            {
                state->AMASK |= ( state->TRAP_ICMP ?
                                HLP_MODEL_AMASK_TRAP_ICMP_TTL :
                                HLP_MODEL_AMASK_TRAP_TTL );
            }
        }
        else /* Frame is IP multicast */
        {
            if ( ( sysCfgRouter.TRAP_TTL1 == 1 ) && state->TRAP_ICMP )
            {
                state->LOG_AMASK |= HLP_MODEL_LOG_TYPE_ICMP;
                *logIpMcTtl = TRUE;
            }
            else if (sysCfgRouter.TRAP_TTL1 == 2 )
            {
                state->LOG_AMASK |= ( state->TRAP_ICMP ?
                                      HLP_MODEL_LOG_TYPE_ICMP :
                                      HLP_MODEL_LOG_TYPE_TTL_IP_MC );
                *logIpMcTtl = TRUE;
            }
        }
    }
    


    if ( state->TRAP_IP_OPTIONS &&
         ( state->IS_IPV6 || state->IS_IPV4 ) &&
         sysCfgRouter.TRAP_IP_OPTIONS )
    {
        state->AMASK |= HLP_MODEL_AMASK_TRAP_IP_OPTION;
    }

    if ( state->MTU_VIOLATION && sysCfg1.TRAP_MTU_VIOLATIONS )
    {
        if (state->MARK_ROUTED)
        {
            state->AMASK |= HLP_MODEL_AMASK_TRAP_MTU_VIO;
            WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,"MTU violation: Trapping frame\n");
        }
    }
}   /* end ProcessTraps */


/*****************************************************************************/
/** ResolveMask
 * \ingroup intModel
 *
 * \desc            Retrieves a bitmask representing the index of the highest
 *                  priority bit (lowest numbered bit) that is set in the
 *                  specified bitmask.
 *
 * \param[in]       mask is the bitmask to be scanned.
 *
 * \param[in]       n is the number of bits to scan.
 *
 * \return          A bitmask representing the index of the highest priority
 *                  bit.
 * \return          0 otherwise.
 *
 *****************************************************************************/
static fm_uint64 ResolveMask(fm_uint64 mask, fm_int n)
{
    fm_uint64 priMask;
    fm_int    i;

    for (i = 0; i < n; i++)
    {
        priMask = FM_LITERAL_U64(1) << i;
        if (mask & priMask)
        {
            return priMask;
        }
    }
    return FM_LITERAL_U64(0);

}   /* end ResolveMask */

/*****************************************************************************/
/** AnalyzeMaskBits
 * \ingroup intModel
 *
 * \desc            takes in amask(after resolution) to printout what might be
 *                  the reason for genMask to block learning
 *
 * \param[in]       amask
 *
 * \param[in]       model points to switch model state
 *
 *****************************************************************************/
static void AnalyzeMaskBits(fm_uint64 amask, hlp_model   *model)
{
    hlp_modelState  *state = &model->packetState;
    fm_uint64   blockingLearningBits;
    fm_uint64   AMASK_BLOCKING_LEARNING_MASK = 0x000180B7D87;//43bits
    static const char *amask_bit_string[] = {
                "HLP_MODEL_AMASK_DROP_PERR",
                "HLP_MODEL_AMASK_SPECIAL",
                "HLP_MODEL_AMASK_DROP_PARSER_ERR",
                "HLP_MODEL_AMASK_TRAP_RESERVED_MAC",
                "HLP_MODEL_AMASK_TRAP_RESERVED_MAC_REMAP",
				"unused",
				"unused",
                "HLP_MODEL_AMASK_DROP_MAC_CTRL",
                "HLP_MODEL_AMASK_DROP_RESERVED_MAC",
				"unused",
                "HLP_MODEL_AMASK_DROP_SMAC",
                "HLP_MODEL_AMASK_DROP_SEC_ADDR",
                "HLP_MODEL_AMASK_DROP_SEC_PORT",
                "HLP_MODEL_AMASK_DROP_STATIC_ADDR",
                "HLP_MODEL_AMASK_DROP_PROVISIONAL",
                "HLP_MODEL_AMASK_TRAP_CPU_ADDR",
                "HLP_MODEL_AMASK_DROP_IV",
                "HLP_MODEL_AMASK_DROP_INGRESS_STP_NON_LEARN",
                "HLP_MODEL_AMASK_DROP_INGRESS_STP_LEARN",
                "HLP_MODEL_AMASK_DROP_FFU",
                "HLP_MODEL_AMASK_TRAP_FFU",
                "HLP_MODEL_AMASK_TRAP_ICMP_TTL",
                "HLP_MODEL_AMASK_TRAP_IP_OPTION",
                "HLP_MODEL_AMASK_TRAP_MTU_VIO",
                "HLP_MODEL_AMASK_TRAP_IGMP",
                "HLP_MODEL_AMASK_TRAP_TTL",
                "HLP_MODEL_AMASK_DROP_TTL",
                "HLP_MODEL_AMASK_DROP_DLF",
                "HLP_MODEL_AMASK_DROP_CAM_MISS",
                "HLP_MODEL_AMASK_DROP_NULL_GLORTDEST",
                "HLP_MODEL_AMASK_DROP_EV",
				"unused",
                "HLP_MODEL_AMASK_DROP_EGRESS_STP",
                "HLP_MODEL_AMASK_DROP_LOOPBACK",
                "HLP_MODEL_AMASK_GLORT",
                "HLP_MODEL_AMASK_FLOOD",
                "HLP_MODEL_AMASK_SWITCH_RESERVED_MAC",
                "HLP_MODEL_AMASK_FORWARD_NORMAL",
                "HLP_MODEL_AMASK_LOG_INGRESS_FFU",
                "HLP_MODEL_AMASK_LOG_RESERVED_MAC",
                "HLP_MODEL_AMASK_LOG_ARP_REDIRECT",
                "HLP_MODEL_AMASK_LOG_IP_ICMP",
                "HLP_MODEL_AMASK_LOG_IP_TTL",
                "HLP_MODEL_AMASK_MIRROR_INGRESS_FFU"
    };
    
    FM_NOT_USED(amask);

    blockingLearningBits = state->AMASK & AMASK_BLOCKING_LEARNING_MASK;
    if(blockingLearningBits != 0)
    {
        WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK, "These amask bits may possibly cause LEARNING_ENABLE=0.\n");
        for (fm_int i = 0; i < HLP_MODEL_AMASK_WIDTH_IND_UNUSED; i++)
        {
            fm_uint64 maskThisRound = FM_LITERAL_U64(1) << i;
            if (blockingLearningBits & maskThisRound)
            {
                WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                              "%s\n", amask_bit_string[i]);
            }
        }
    }


}

/*****************************************************************************/
/** ResolveAction
 * \ingroup intModel
 *
 * \desc           Updates Destination Mask,Action Type and Trap code based on
 *                 high priority action from the Action Mask.
 *
 * \param[in]      model points to the switch model state.
 *
 *****************************************************************************/
static void ResolveAction(hlp_model *model)
{
    hlp_modelState  *state = &model->packetState;
    fm_uint64       amask;
    fm_uint32       *regPtr;
    fm_byte         trapTC;
    
    state = &model->packetState;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FWD_IEEE_RESERVED_MAC_CFG(0));
    trapTC = FM_GET_FIELD(*regPtr, HLP_FWD_IEEE_RESERVED_MAC_CFG, TRAP_TC);

    if (state->AMASK & HLP_MODEL_AMASK_SPECIAL)
    {
        state->PRE_RESOLVE_ACTION = HLP_MODEL_ACTION_SPECIAL;
        state->PRE_RESOLVE_DMASK = state->GLORT_DMASK;
        state->DMASK = state->PRE_RESOLVE_DMASK;
    }
    else
    {
        if (state->AMASK & HLP_MODEL_AMASK_FLOOD)
        {
            state->PRE_RESOLVE_ACTION = HLP_MODEL_ACTION_FLOOD;
        }
        else
        {
            state->PRE_RESOLVE_ACTION = HLP_MODEL_ACTION_NORMAL;
        }
        /*  PRE_RESOLVE_DMASK now a function of DST_MASK_ACL  *
         *  which is last handled in ProcessFfuEgressActions. */  
    }
    state->PRE_RESOLVE_DGLORT = state->IDGLORT;

   	WM_DISPLAY3(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,"pre-ResolveMask: AMASK=0x%llx, ", state->AMASK);

    amask = ResolveMask(state->AMASK, HLP_MODEL_AMASK_WIDTH);

    AnalyzeMaskBits(amask, model);

  	WM_DISPLAY3(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,"post-ResolveMask: amask=0x%llx\n", amask);

    //state->DMASK = state->PRE_RESOLVE_DMASK;

    switch (amask)
    {
        case FM_LITERAL_U64(0):
            state->AMASK |= HLP_MODEL_AMASK_FORWARD_NORMAL;
            state->ACTION = HLP_MODEL_ACTION_NORMAL;
            break;

        case HLP_MODEL_AMASK_DROP_PERR:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_DROP_PARITY;
            state->LEARNING_ENABLED = 0;
            WM_DISPLAY3(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK," ResolveAction: state->LEARNING_ENABLED = 0 due to amask = HLP_MODEL_AMASK_DROP_PERR. \n");
            break;

        case HLP_MODEL_AMASK_SPECIAL:
            state->DMASK = state->GLORT_DMASK;
            state->ACTION = HLP_MODEL_ACTION_SPECIAL;
            state->LEARNING_ENABLED = 0;
            WM_DISPLAY3(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK," ResolveAction: state->LEARNING_ENABLED = 0 due to amask = HLP_MODEL_AMASK_SPECIAL. \n"); //1
            break;

        case HLP_MODEL_AMASK_DROP_PARSER_ERR:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_DROP_PARSE;
            state->LEARNING_ENABLED = 0;
            WM_DISPLAY3(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK," ResolveAction: state->LEARNING_ENABLED = 0 due to amask = HLP_MODEL_AMASK_DROP_PARSER_ERR. \n");
            break;

        case HLP_MODEL_AMASK_TRAP_RESERVED_MAC:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_TRAP;
            state->CPU_CODE = HLP_MODEL_CPU_CODE_RSVD_MAC;
            break;

        case HLP_MODEL_AMASK_TRAP_RESERVED_MAC_REMAP:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_TRAP;
            state->CPU_CODE = HLP_MODEL_CPU_CODE_RSVD_MAC;
            state->QOS_SWPRI = trapTC; /* FIXME */
            break;

        case HLP_MODEL_AMASK_DROP_MAC_CTRL:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_DROP_CONTROL;
            state->LEARNING_ENABLED = 0;
            WM_DISPLAY3(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK," ResolveAction: state->LEARNING_ENABLED = 0 due to amask = HLP_MODEL_AMASK_DROP_MAC_CTRL. \n");
            break;

        case HLP_MODEL_AMASK_DROP_RESERVED_MAC:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_BANK5_OTHER_DROPS;
            state->LEARNING_ENABLED = 0;
            WM_DISPLAY3(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK," ResolveAction: state->LEARNING_ENABLED = 0 due to amask = HLP_MODEL_AMASK_DROP_RESERVED_MAC. \n");
            break;

        case HLP_MODEL_AMASK_DROP_SMAC:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_BANK5_OTHER_DROPS;
            state->LEARNING_ENABLED = 0;
            WM_DISPLAY3(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK," ResolveAction: state->LEARNING_ENABLED = 0 due to amask = HLP_MODEL_AMASK_DROP_SMAC. \n");
            break;

        case HLP_MODEL_AMASK_DROP_SEC_ADDR :
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_DROP_SV;
            break;

        case HLP_MODEL_AMASK_DROP_SEC_PORT :
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_DROP_SV;
            break;

        case HLP_MODEL_AMASK_DROP_STATIC_ADDR :
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_DROP_SV;
            break;

        case HLP_MODEL_AMASK_DROP_PROVISIONAL:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_BANK5_OTHER_DROPS;
            break;

        case HLP_MODEL_AMASK_TRAP_CPU_ADDR:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_TRAP;
            state->CPU_CODE = HLP_MODEL_CPU_CODE_CPU_ADDRESS;
            break;
/*
 * Removed per Spec bug 22752.  WIll it return?
 *
        case HLP_MODEL_AMASK_TRAP_REMAP_CPU:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_TRAP;
            state->CPU_CODE = HLP_MODEL_CPU_CODE_CPU_ADDRESS;
            state->QOS_SWPRI = 15;
            break;
*/

        case HLP_MODEL_AMASK_DROP_IV:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_DROP_IV;
            state->LEARNING_ENABLED = 0;
            WM_DISPLAY3(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK," ResolveAction: state->LEARNING_ENABLED = 0 due to amask = HLP_MODEL_AMASK_DROP_IV. \n");
            break;

        case HLP_MODEL_AMASK_DROP_INGRESS_STP_NON_LEARN:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_DROP_STP;
            state->LEARNING_ENABLED = 0;
            WM_DISPLAY3(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK," ResolveAction: state->LEARNING_ENABLED = 0 due to amask = HLP_MODEL_AMASK_DROP_INGRESS_STP_NON_LEARN. \n");
            break;

        case HLP_MODEL_AMASK_DROP_INGRESS_STP_LEARN:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_DROP_STP;
            break;

        case HLP_MODEL_AMASK_DROP_FFU:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_DROP_FFU;
            state->LEARNING_ENABLED = 0;
            WM_DISPLAY3(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK," ResolveAction: state->LEARNING_ENABLED = 0 due to amask = HLP_MODEL_AMASK_DROP_FFU. \n");
            break;

        case HLP_MODEL_AMASK_TRAP_FFU:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_TRAP;
            state->CPU_CODE = HLP_MODEL_CPU_CODE_FFU;
            break;

        case HLP_MODEL_AMASK_TRAP_ICMP_TTL:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_TRAP;
            state->CPU_CODE = HLP_MODEL_CPU_CODE_ICMP;
            break;

        case HLP_MODEL_AMASK_TRAP_IP_OPTION:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_TRAP;
            state->CPU_CODE = HLP_MODEL_CPU_CODE_IP_OPTION;
            break;

        case HLP_MODEL_AMASK_TRAP_MTU_VIO:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_TRAP;
            state->CPU_CODE = HLP_MODEL_CPU_CODE_MTU;
            break;

        case HLP_MODEL_AMASK_TRAP_IGMP:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_TRAP;
            state->CPU_CODE = HLP_MODEL_CPU_CODE_IGMP;
            break;

        case HLP_MODEL_AMASK_TRAP_TTL:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_TRAP;
            state->CPU_CODE = HLP_MODEL_CPU_CODE_TTL;
            break;

        case HLP_MODEL_AMASK_DROP_TTL:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_DROP_TTL;
            break;

        case HLP_MODEL_AMASK_DROP_NULL_GLORTDEST:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_BANK5_OTHER_DROPS;
            break;

        case HLP_MODEL_AMASK_DROP_EV:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_DROP_EV;
            break;

        case HLP_MODEL_AMASK_DROP_DLF:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_DROP_DLF;
            state->LEARNING_ENABLED = 0;
            WM_DISPLAY3(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK," ResolveAction: state->LEARNING_ENABLED = 0 due to amask = HLP_MODEL_AMASK_DROP_DLF. \n");
            break;

        case HLP_MODEL_AMASK_DROP_CAM_MISS:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_DROP_CAM;
            state->LEARNING_ENABLED = 0;
            WM_DISPLAY3(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK," ResolveAction: state->LEARNING_ENABLED = 0 due to amask = HLP_MODEL_AMASK_DROP_CAM_MISS. \n");
            break;

//        case HLP_MODEL_AMASK_DROP_POLICER:
//            state->DMASK = 0;
//            state->ACTION = HLP_MODEL_ACTION_DROP_POLICER;
//            break;

        case HLP_MODEL_AMASK_DROP_EGRESS_STP:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_DROP_STP;
            break;

        case HLP_MODEL_AMASK_DROP_LOOPBACK:
            state->DMASK = 0;
            state->ACTION = HLP_MODEL_ACTION_DROP_LOOPBACK;
            break;

        case HLP_MODEL_AMASK_GLORT:
            state->ACTION = HLP_MODEL_ACTION_GLORT_FORWARDED;
            break;

        case HLP_MODEL_AMASK_FLOOD:
            state->ACTION = HLP_MODEL_ACTION_FLOOD;
            break;

        case HLP_MODEL_AMASK_SWITCH_RESERVED_MAC:
            /* To Stats Bin, pending Bug 23032 */
            state->ACTION = HLP_MODEL_ACTION_NORMAL;
            break;

        default:
            state->DMASK = 0;//TODO: tmp fix, to match with rtl
            state->ACTION = HLP_MODEL_ACTION_BANK5_OTHER_DROPS;
            break;
    }

	WM_DISPLAY2(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
		"end ResolveAction: state->ACTION=%-2d\n",
		state->ACTION);

}   /* end ResolveAction */


/*****************************************************************************/
/** HandleLAG
 * \ingroup intModel
 *
 * \desc           Handles LAG and updates DMASK appropriately
 *
 * \param[in]      model points to the switch model state.
 *
 *****************************************************************************/
static void HandleLAG(hlp_model *model)
{
    hlp_modelState      * state = &model->packetState;
    fm_uint32           hash;
    fm_int              i;
    hlpFwdLagCfg        lagCfg;
    fm_uint32           *regPtr;

    /*Trigger Redirect ForwardActin offers a FilterNewDestMask config*/
    /*which, when set to 0b, bypasses LAG filtering*/
    /*+ GLORT LOOKUP-strict mode-targeted deterministic*/
    if ( ( ( state->ACTION == HLP_MODEL_ACTION_REDIRECT_TRIG ) &&
           !state->TRIGGERS.filterDestMask ) ||
         ( ( state->ACTION != HLP_MODEL_ACTION_REDIRECT_TRIG ) &&
           state->TARGETED_DETERMINISTIC ) )
    {
        return;
    }

    if (state->DMASK != 0)
    {
        for (i = 0; i <= HLP_MAX_FABRIC_LOG_PORT; i++)
        {
            regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FWD_LAG_CFG(i, 0));

            lagCfg.IN_LAG = FM_GET_BIT(*regPtr, HLP_FWD_LAG_CFG, IN_LAG);
            lagCfg.HASH_ROTATION = FM_GET_BIT(*regPtr, HLP_FWD_LAG_CFG, HASH_ROTATION);
            lagCfg.INDEX = FM_GET_FIELD(*regPtr, HLP_FWD_LAG_CFG, INDEX);
            lagCfg.LAG_SIZE = FM_GET_FIELD(*regPtr, HLP_FWD_LAG_CFG, LAG_SIZE);
            //FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, " LAG filtering - lagCfg.IN_LAG=0x%llx, lagCfg.HashRotation=0x%llx, lagCfg.LagSize=0x%llx, lagCfg.Index=0x%llx\n",
            //             (fm_uint64)lagCfg.IN_LAG, (fm_uint64)lagCfg.HashRotation, (fm_uint64)lagCfg.LagSize, (fm_uint64)lagCfg.Index);
            if (!lagCfg.IN_LAG)
            {
                continue;
            }
    
            hash = lagCfg.HASH_ROTATION ? state->HASH_ROT_B : state->HASH_ROT_A;
            switch (lagCfg.LAG_SIZE)
            {
                case 0:  hash %= 16;               break;
                default: hash %= lagCfg.LAG_SIZE; break;
            }
    
            if (hash != lagCfg.INDEX)
            {
                state->DMASK &= ~(FM_LITERAL_U64(1) << i);
            }
        }

        if (state->DMASK == 0)
        {
            state->ACTION = HLP_MODEL_ACTION_BANK5_OTHER_DROPS;
            WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                         "In LAG, DMASK=0: Dropping frame\n");

        }
    }

    WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                 "LAG filtering: DMASK[0x%x]\n",
                 state->DMASK);

}   /* end HandleLAG */


/*****************************************************************************/
/** HandleLoopbackSuppress
 * \ingroup intModel
 *
 * \desc            Handles loopback suppression and update the destination mask
 *                  appropriately.
 *
 * \param[in]       model points to the switch model state.
 *
 *****************************************************************************/
static void HandleLoopbackSuppress(hlp_model *model)
{
    //hlpCmApplyLoopbackSuppress      *loopback_suppress;
    hlp_modelState                  *state = &model->packetState;
    fm_int                          i;
    fm_uint32                       *regPtr;
    fm_uint16                       lpbk_glort;
    fm_uint16                       lpbk_glort_mask;
    
    /*
    if (  (state->ISL_IFTYPE == HLP_MODEL_FTYPE_SPECIAL) ||
          state->MARK_ROUTED      ||
         (state->DMASK == 0)        )
    {
        return;
    }
    */

    if ( state->TARGETED_DETERMINISTIC || state->MARK_ROUTED || state->DMASK == 0)
    {
        return;
    }

    for (i = 0; i <= HLP_MAX_FABRIC_LOG_PORT; i++)
    {

        regPtr = FM_MODEL_GET_REG_PTR(model, HLP_CM_APPLY_LOOPBACK_SUPPRESS(i, 0));
        lpbk_glort_mask = FM_ARRAY_GET_FIELD64(regPtr, HLP_CM_APPLY_LOOPBACK_SUPPRESS, GLORT_MASK); 
        lpbk_glort = FM_ARRAY_GET_FIELD64(regPtr, HLP_CM_APPLY_LOOPBACK_SUPPRESS, GLORT); 

        if ( (state->CSGLORT & lpbk_glort_mask) == lpbk_glort )
        {
            state->DMASK &= ~(FM_LITERAL_U64(1) << i);
        }
        //FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "Loopback suppression filtering: loopback_suppress->glort_mask[%d] = 0x%0x\n", i, lpbk_glort_mask);
        //FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "Loopback suppression filtering: loopback_suppress->glort[%d] = 0x%0x\n", i, lpbk_glort);
    }

    WM_DISPLAY2(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                 "Loopback suppression filtering: DMASK[0x%x]\n",
                 state->DMASK);

    if ( state->DMASK == 0 )
    {
        state->ACTION = HLP_MODEL_ACTION_DROP_LOOPBACK;
        WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
            "Loopback suppression: Dropping frame, DMASK[0x%x])\n",
                             state->DMASK);
    }

}   /* end HandleLoopbackSuppress */


/*****************************************************************************/
/** HandleTraps
 * \ingroup intModel
 *
 * \desc            Traps the packet to the CPU.
 *
 * \param[in]       model points to the switch model state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 * \note            Trapping packets to the CPU is a low occurrence event. There
 *                  is therefore no need to cache the TRAP_GLORT and
 *                  CPU_TRAP_MASK register state.
 *
 *****************************************************************************/
static fm_status HandleTraps(hlp_model *model)
{
    hlp_modelState  *state = &model->packetState;
    fm_status       err = FM_OK;
    fm_uint32       *regPtr;

    state->OPERATOR_ID = state->PKT_META[20] & 0x0f; 
    
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_CM_APPLY_TRAP_GLORT(state->OPERATOR_ID, 0));

    state->IDGLORT = FM_GET_FIELD(*regPtr, HLP_CM_APPLY_TRAP_GLORT, TRAP_GLORT);

    if(state->TRIGGERS.trapAction ==
             HLP_MODEL_TRIG_ACTION_TRAP_TRAP)
    {
        state->CPU_CODE = (1 << 3) | state->TRIGGERS.cpuCode;
    }

    if ( state->STORE_TRAP_ACTION == 1)
    {
    	WM_DISPLAY3(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,"pre-store_trap_action: PKT_META[19]=0x%x cpu_code=0x%x\n", state->PKT_META[19], state->CPU_CODE);
        FM_SET_UNNAMED_FIELD(state->PKT_META[19], 4, 4, state->CPU_CODE);// assign bits [159:156] in pkt_meta
        FM_SET_UNNAMED_FIELD(state->PKT_META[18], 7, 1, state->STORE_TRAP_ACTION);// set bit 151 (presence bit) in pkt_meta
        WM_DISPLAY3(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,"post-store_trap_action: PKT_META[19]=0x%x \n", state->PKT_META[19]);
    }

    state->ACTION = HLP_MODEL_ACTION_TRAP;
    //state->ISL_IFTYPE = HLP_MODEL_FTYPE_SPECIAL;
    state->MARK_ROUTED = FALSE;
    state->IP_MCAST_IDX = 0;
    
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_CM_APPLY_CPU_TRAP_MASK(0));
    state->DMASK = FM_GET_FIELD(*regPtr, HLP_CM_APPLY_CPU_TRAP_MASK, DEST_MASK);

    if (state->DMASK == 0)
    {
        state->ACTION = HLP_MODEL_ACTION_BANK5_OTHER_DROPS;
        WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                     "In CPU Trap Mask, DMASK=0: Dropping frame\n");
    }

    WM_DISPLAY2(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                 "End of HandleTraps: DMASK[0x%x], IDGLORT=0x%x, IP_MCAST_IDX=0x%x \n",
                 state->DMASK, state->IDGLORT, state->IP_MCAST_IDX);
ABORT:
    return err;

}   /* end HandleTraps */


/*****************************************************************************/
/** HandleLogging
 * \ingroup intModel
 *
 * \desc            Logs the packet to the CPU.
 *
 * \param[in]       model points to the switch model state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 * \note            Logging packets to the CPU is a low occurrence event. There
 *                  is therefore no need to cache the CPU_LOG_MASK_FH register
 *                  state.
 *
 *****************************************************************************/
static fm_status HandleLogging(hlp_model *model)
{
    hlp_modelState      *state = &model->packetState;
    fm_status           err = FM_OK;
    fm_uint64           logMask;
    fm_uint32           *regPtr;

    if (state->TRIGGERS.logAction == 1)
    {
        state->LOG_AMASK |= HLP_MODEL_LOG_TYPE_TRIG_LOG_ACTION;
    }

	WM_DISPLAY3(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,"pre-Resolve: LOG_AMASK=0x%x\n",
										state->LOG_AMASK);

    logMask = ResolveMask(state->LOG_AMASK, HLP_MODEL_LOG_MASK_WIDTH);

	WM_DISPLAY2(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,"post-Resolve: LOG_AMASK=0x%x, logmask = 0x%llx\n",
										state->LOG_AMASK, logMask);

    /* Per RRC Spec. bug 22835 and RTL change 271047. */
    if(logMask != 0)
    {
        state->LOGGING_HIT = 1;
        
        regPtr = FM_MODEL_GET_REG_PTR(model, HLP_CM_APPLY_LOG_MIRROR_PROFILE(0));

        switch (logMask)
        {
            case HLP_MODEL_LOG_TYPE_TRIG_LOG_ACTION:
                state->LOG_MIRROR_PROFILE_IDX = FM_ARRAY_GET_FIELD(regPtr, HLP_CM_APPLY_LOG_MIRROR_PROFILE, TRIGGER);
                break;
            case HLP_MODEL_LOG_TYPE_FFU:
                state->LOG_MIRROR_PROFILE_IDX = FM_ARRAY_GET_FIELD(regPtr, HLP_CM_APPLY_LOG_MIRROR_PROFILE, FFU);            
                break;
            case HLP_MODEL_LOG_TYPE_RESERVED_MAC:
                state->LOG_MIRROR_PROFILE_IDX = FM_ARRAY_GET_FIELD(regPtr, HLP_CM_APPLY_LOG_MIRROR_PROFILE, RESERVED_MAC);            
                break;
            case HLP_MODEL_LOG_TYPE_ARP_REDIRECT:
                state->LOG_MIRROR_PROFILE_IDX = FM_ARRAY_GET_FIELD(regPtr, HLP_CM_APPLY_LOG_MIRROR_PROFILE, ARP_REDIRECT);;            
                break;
            case HLP_MODEL_LOG_TYPE_ICMP:
                state->LOG_MIRROR_PROFILE_IDX = FM_ARRAY_GET_FIELD(regPtr, HLP_CM_APPLY_LOG_MIRROR_PROFILE, ICMP);;            
                break;
            case HLP_MODEL_LOG_TYPE_TTL_IP_MC:
                state->LOG_MIRROR_PROFILE_IDX = FM_ARRAY_GET_FIELD(regPtr, HLP_CM_APPLY_LOG_MIRROR_PROFILE, TTL);;            
                break;
            case 0:
                break;
            default:
                state->LOGGING_HIT = 0;
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err = FM_ERR_UNSUPPORTED);
                break;
        }
    }

   	WM_DISPLAY2(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,"post-case: LOG_MIRROR_PROFILE_IDX=0x%x, TRIGGERS.mirror0ProfileV=%1d\n",
                state->LOG_MIRROR_PROFILE_IDX, state->TRIGGERS.mirror0ProfileV);

    if ((!state->TRIGGERS.mirror0ProfileV || (state->TRIGGERS.mirror0ProfileV && state->TRIGGERS.qcnValid0)) && state->LOGGING_HIT == 1)
    {
        state->MIRROR0_PROFILE_V = 1;
        state->MIRROR0_PROFILE_IDX = state->LOG_MIRROR_PROFILE_IDX;
    }

ABORT:
    return err;

}   /* end HandleLogging */


/*****************************************************************************/
/** ProcessQCN
 * \ingroup intModel
 *
 * \desc           Gets QCN info and updates appropriate packet states.
 *
 * \param[in]      model points to the switch model state.
 *
 *****************************************************************************/
static void ProcessQCN(hlp_model *model)
{
    hlp_modelState *state;
    fm_uint32      *regPtr;
    fm_byte        mirrorProfileIdx;
    fm_byte        mirrorSession;

    state = &model->packetState;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_FWD_QCN_MIRROR_CFG(0));
    mirrorProfileIdx = FM_GET_FIELD(*regPtr, HLP_FWD_QCN_MIRROR_CFG, MIRROR_PROFILE_IDX);
    mirrorSession = FM_GET_FIELD(*regPtr, HLP_FWD_QCN_MIRROR_CFG, MIRROR_SESSION);

    if ( ( state->RX_MIRROR == 0 ) && ( mirrorSession == 2 ) )
    {
    	state->QCN_MIRROR1_PROFILE_V = 1;
        state->MIRROR1_PROFILE_V = 1;
    	state->MIRROR1_PROFILE_IDX = mirrorProfileIdx;
    }
    if ( mirrorSession == 1 )
    {
    	state->QCN_MIRROR0_PROFILE_V = 1;
        state->MIRROR0_PROFILE_V = 1;
    	state->MIRROR0_PROFILE_IDX = mirrorProfileIdx;
    }
}   /* end ProcessQCN */


/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** hlpModelGenMask1
 * \ingroup intModel
 *
 * \desc            Models the first half of the HLP white model GEN_MASK
 *                  stage (GEN_MASK1).
 *
 * \param[in]       model points to the switch model state.
 *
 *****************************************************************************/
void hlpModelGenMask1(hlp_model *model)
{
    hlp_modelState      *state;
    fm_bool             logIpMcTtl = FALSE;
    fm_byte             learning_enabled_snapshot;
    hlpFwdPortCfg1      portCfg1;
    hlpFwdSysCfg1       sysCfg1;
    
    if(testPlusArgs("HLP_GEN_MASK_WM_PRINT_VERBOSE") >= 0)
        genMaskDisplayVerbose = testPlusArgs("HLP_GEN_MASK_WM_PRINT_VERBOSE");
    WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,"genMaskDisplayVerbose=%0d\n", genMaskDisplayVerbose)

    state = &model->packetState;

    GetPortCfg1(model, &portCfg1); //HLP_FWD_PORT_CFG_1
    GetSysCfg1(model, &sysCfg1);

    //the initial state->LEARNING_ENABLED assignment:
    state->LEARNING_ENABLED = 
        portCfg1.LEARNING_ENABLE &&
        (!state->NO_LEARN) &&
        ((state->L2_IFID1_STATE == HLP_MODEL_STP_STATE_LEARNING) ||
         (state->L2_IFID1_STATE == HLP_MODEL_STP_STATE_FORWARD));
    WM_DISPLAY3(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
    		" hlpModelGenMask1: INITIAL state->LEARNING_ENABLED = %d due to : HLP_FWD_PORT_CFG_1.LEARNING_ENABLE=%d, state->NO_LEARN=%d, state->L2_IFID1_STATE=%d. \n",
			state->LEARNING_ENABLED, portCfg1.LEARNING_ENABLE, state->NO_LEARN, state->L2_IFID1_STATE);

    if (state->GLORT_CAM_MISS)
    {
        state->AMASK |= HLP_MODEL_AMASK_DROP_CAM_MISS;
        WM_DISPLAY2(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                     "Glort CAM Miss\n");
    }
    
    //if (state->ISL_IFTYPE == HLP_MODEL_FTYPE_SPECIAL)
    if(state->TARGETED_DETERMINISTIC)
    {
        state->AMASK |= HLP_MODEL_AMASK_SPECIAL;
        WM_DISPLAY2(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                     "Marking frame as special delivery\n");
    }
    
    /*
    if ( state->DROP_TAGGED )
    {
        state->AMASK |= HLP_MODEL_AMASK_DROP_TAG;
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                     "ISL/VLAN tagging error: Dropping frame\n");
    }
    */

    /*
    if( state->POLICER_DROP )
    {
        state->AMASK |= HLP_MODEL_AMASK_DROP_POLICER;
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                     "Policer dropping frame\n");
    }
    */

    if (state->PARSER_ERROR)
    {
        state->AMASK |= HLP_MODEL_AMASK_DROP_PARSER_ERR;
        WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                     "Parse error: Dropping frame\n");
    }

    if (state->TRAP_IGMP)
    {
        state->AMASK |= HLP_MODEL_AMASK_TRAP_IGMP;
        WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,"Trapping IGMP frame\n");
    }
    
    
    if (state->PARITY_ERROR)
    {
        state->AMASK |= HLP_MODEL_AMASK_DROP_PERR;
        WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                     "Parity error: Dropping frame\n");
    }
    

    if ( (sysCfg1.DROP_MAC_CTRL_ETHERTYPE == TRUE) &&
         (state->L2_ETYPE == HLP_MODEL_ETYPE_MAC_CONTROL) )
    {
        state->AMASK |= HLP_MODEL_AMASK_DROP_MAC_CTRL;
        WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                     "MAC CTRL Ethertype 0x8808 Frame: Dropping frame\n");
    }

    if ( (sysCfg1.DROP_INVALID_SMAC == TRUE) &&
         ( ((state->L2_SMAC == 0) && (state->PARSER_INFO.window_parse_v == 0)) ||
           (fmModelIsBroadcastMacAddress(state->L2_SMAC))                      ||
           (fmModelIsMulticastMacAddress(state->L2_SMAC))   
         )
       )
    {
        state->AMASK |= HLP_MODEL_AMASK_DROP_SMAC;
        WM_DISPLAY(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
                     "SMAC Frame: Dropping frame (state->L2_SMAC=0x%llx)\n", state->L2_SMAC);
    }
    
    if(fmModelIsCpuMacAddress(model, state->L2_SMAC))
    {
        state->LEARNING_ENABLED = 0;
        WM_DISPLAY2(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK," hlpModelGenMask1: state->LEARNING_ENABLED = 0 due to fmModelIsCpuMacAddress() = 1. \n");
    }
    if(state->L2_SMAC == 0)
    {
        state->LEARNING_ENABLED = 0;
        WM_DISPLAY2(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK," hlpModelGenMask1: state->LEARNING_ENABLED = 0 due to L2_SMAC == 0. \n");
    }

    ProcessTraps(model, &logIpMcTtl);

    ProcessPortSecurity(model);

    ProcessFiltering(model);

    ProcessFfuFlags(model);

    ProcessQCN(model);

    state->DMASK = state->PRE_RESOLVE_DMASK;

    ProcessEgressStpState(model);

    learning_enabled_snapshot = state->LEARNING_ENABLED;
    ResolveAction(model);
    if (learning_enabled_snapshot != state->LEARNING_ENABLED) /*debug*/
    {
    	WM_DISPLAY2(genMaskDisplayVerbose, FM_LOG_CAT_MODEL_GEN_MASK,
    			" hlpModelGenMask1: before ResolveAction(): state->LEARNING_ENABLED = %d; after ResolveAction(): state->LEARNING_ENABLED = %d. \n",
				learning_enabled_snapshot, state->LEARNING_ENABLED);
    }

    UpdateActionMask(model, logIpMcTtl);

//    ProcessDOS(model);
//    state->LEARNING_ENABLED &= (!state->NO_LEARN); //state->NO_LEARN may be updated by ProcessDOS
    
    state->CPU_TRAP = (state->ACTION == HLP_MODEL_ACTION_TRAP);

    state->OPERATOR_ID = state->PKT_META[20] & 0x0f; 

    state->QOS_SWPRI &= 0x7; /*TODO: swpri 4bits, tc 3bits; to coordinate with rtl in act_res*/

    state->STORE_TRAP_ACTION = sysCfg1.STORE_TRAP_ACTION;

    if (!state->DISABLE_DBG_DUMP)
        HLP_MODEL_DBG_DUMP(model, hlpModelDbgDumpGenMask1);

}   /* end hlpModelGenMask1 */


/*****************************************************************************/
/** hlpModelGenMask2
 * \ingroup intModel
 *
 * \desc            Models the second half of the HLP white model GEN_MASK
 *                  stage (GEN_MASK2).
 *
 * \param[in]       model points to the switch model state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status hlpModelGenMask2(hlp_model *model)
{
    hlp_modelState  *state;
    fm_status       err = FM_OK;
    fm_bool         doNotTrapOrLog;
    fm_uint32       *regPtr;
    hlpFwdSysCfg1   sysCfg1;

    state = &model->packetState;
    
    GetSysCfg1(model, &sysCfg1);

    /* Apply the TRIGGERS stage results. */
/*    state->RX_MIRROR = state->TRIGGERS.rxMirror;
 *
    state->ACTION = state->TRIGGERS.action;
    state->L2_EVID1 = state->TRIGGERS.vlan;
    state->QOS_SWPRI = state->TRIGGERS.TC;
    state->DMASK = state->TRIGGERS.destMask;
    state->IDGLORT = state->TRIGGERS.destGlort;
    
    state->MIRROR1_PROFILE_V = state->TRIGGERS.mirror1ProfileV;
    state->MIRROR1_PROFILE_IDX = state->TRIGGERS.mirror1ProfileIdx;
    state->MIRROR0_PROFILE_V = state->TRIGGERS.mirror0ProfileV;
    state->MIRROR0_PROFILE_IDX = state->TRIGGERS.mirror0ProfileIdx;
*/

    HandleLAG(model);

    HandleLoopbackSuppress(model);

    doNotTrapOrLog = ( state->TRIGGERS.trapAction ==
                       HLP_MODEL_TRIG_ACTION_TRAP_REVERT );

    /* Trap to CPU */
    state->IDGLORT = state->TRIGGERS.destGlort;

    if ( ( state->CPU_TRAP ||
           ( state->TRIGGERS.trapAction ==
             HLP_MODEL_TRIG_ACTION_TRAP_TRAP ) ) &&
         !doNotTrapOrLog )
    {
        err = HandleTraps(model);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
    } else {//TODO: not !trap - should be default
        if(state->TRIGGERS.egressL3DomainAction == 0) {
            FM_SET_UNNAMED_FIELD64(state->PKT_META[21], 5, 3, state->L3_EDOMAIN & 0x07);
            FM_SET_UNNAMED_FIELD64(state->PKT_META[22], 0, 3, state->L3_EDOMAIN >> 3);
        }
        if(state->TRIGGERS.egressL2DomainAction == 0) {
            FM_SET_UNNAMED_FIELD64(state->PKT_META[20], 4, 4, state->L2_EDOMAIN & 0x00F);
            FM_SET_UNNAMED_FIELD64(state->PKT_META[21], 0, 5, state->L2_EDOMAIN >> 4);
        }
    }

    state->LOGGING_HIT = 0;

    if ( ( sysCfg1.ENABLE_TRAP_PLUS_LOG || 
         (state->CPU_TRAP  == 0) ||  
           ( state->TRIGGERS.trapAction == HLP_MODEL_TRIG_ACTION_TRAP_TRAP) || 
           ( state->TRIGGERS.trapAction == HLP_MODEL_TRIG_ACTION_TRAP_LOG) ) &&
         !doNotTrapOrLog )
    {
        err = HandleLogging(model);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
    }
    
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_CM_APPLY_MCAST_EPOCH(0));
    state->MCAST_EPOCH = FM_GET_BIT(*regPtr, HLP_CM_APPLY_MCAST_EPOCH, CURRENT);

    //state->MTMASK = state->GLORT_DMASK & model->INTERNAL_PORT_MASK;

    regPtr = FM_MODEL_GET_REG_PTR(model, 
                                  HLP_CM_APPLY_MIRROR_PROFILE_TABLE(state->MIRROR0_PROFILE_IDX, 0));
    state->MIRROR0_PORT = FM_GET_FIELD(*regPtr, HLP_CM_APPLY_MIRROR_PROFILE_TABLE, PORT);

    regPtr = FM_MODEL_GET_REG_PTR(model, 
                                  HLP_CM_APPLY_MIRROR_PROFILE_TABLE(state->MIRROR1_PROFILE_IDX, 0));
    state->MIRROR1_PORT = FM_GET_FIELD(*regPtr, HLP_CM_APPLY_MIRROR_PROFILE_TABLE, PORT);

    state->MIRROR0_PROFILE_V &= state->MIRROR0_PORT < HLP_MODEL_PORTS_COUNT;
    state->MIRROR1_PROFILE_V &= state->MIRROR1_PORT < HLP_MODEL_PORTS_COUNT;

    state->FNMASK = state->DMASK;

    // partial process of marker drop, can be further modified in cm_apply_checker based on RateLimit
    if((state->PKT_META[HLP_MODEL_META_TYPE_OFF] == HLP_MODEL_META_TYPE_MARKER) && (state->FNMASK == 0))
    {
        state->ACTION = HLP_MODEL_ACTION_MARKER_ERROR_DROPS;
    }

    


ABORT:
    if (!state->DISABLE_DBG_DUMP)
        HLP_MODEL_DBG_DUMP(model, hlpModelDbgDumpGenMask2);

    return err;

}  /* end hlpModelGenMask2 */
