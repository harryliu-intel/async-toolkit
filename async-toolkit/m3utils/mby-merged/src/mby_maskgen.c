// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_nexthop.h"
#include "mby_maskgen.h"

static void getPortCfg1
(
    fm_uint32              regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint32        port, // RX port
    mbyFwdPortCfg1 * const cfg
)
{
    fm_uint32 fwd_port_cfg1_vals[MBY_FWD_PORT_CFG_1_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_FWD_PORT_CFG_1(port, 0), MBY_FWD_PORT_CFG_1_WIDTH, fwd_port_cfg1_vals);

    cfg->LEARNING_ENABLE     = FM_ARRAY_GET_BIT  (fwd_port_cfg1_vals, MBY_FWD_PORT_CFG_1, LEARNING_ENABLE);
    cfg->FILTER_VLAN_INGRESS = FM_ARRAY_GET_BIT  (fwd_port_cfg1_vals, MBY_FWD_PORT_CFG_1, FILTER_VLAN_INGRESS);
    cfg->DESTINATION_MASK    = FM_ARRAY_GET_FIELD(fwd_port_cfg1_vals, MBY_FWD_PORT_CFG_1, DESTINATION_MASK);
}

static void getPortCfg2
(
    fm_uint32              regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint32        port, // RX port
    mbyFwdPortCfg2 * const cfg
)
{
    fm_uint32 fwd_port_cfg2_vals[MBY_FWD_PORT_CFG_2_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_FWD_PORT_CFG_2(port, 0), MBY_FWD_PORT_CFG_2_WIDTH, fwd_port_cfg2_vals);

    cfg->DESTINATION_MASK = FM_ARRAY_GET_FIELD(fwd_port_cfg2_vals, MBY_FWD_PORT_CFG_2, DESTINATION_MASK);
}

static void getSysCfg1
(
    fm_uint32              regs[MBY_REGISTER_ARRAY_SIZE],
    mbyFwdSysCfg1  * const cfg
)
{
    fm_uint32 fwd_sys_cfg1_vals[MBY_FWD_SYS_CFG_1_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_FWD_SYS_CFG_1(0), MBY_FWD_SYS_CFG_1_WIDTH, fwd_sys_cfg1_vals);

    cfg->STORE_TRAP_ACTION       = FM_ARRAY_GET_BIT(fwd_sys_cfg1_vals, MBY_FWD_SYS_CFG_1, STORE_TRAP_ACTION);
    cfg->DROP_MAC_CTRL_ETHERTYPE = FM_ARRAY_GET_BIT(fwd_sys_cfg1_vals, MBY_FWD_SYS_CFG_1, DROP_MAC_CTRL_ETHERTYPE);
    cfg->DROP_INVALID_SMAC       = FM_ARRAY_GET_BIT(fwd_sys_cfg1_vals, MBY_FWD_SYS_CFG_1, DROP_INVALID_SMAC);
    cfg->ENABLE_TRAP_PLUS_LOG    = FM_ARRAY_GET_BIT(fwd_sys_cfg1_vals, MBY_FWD_SYS_CFG_1, ENABLE_TRAP_PLUS_LOG);
    cfg->TRAP_MTU_VIOLATIONS     = FM_ARRAY_GET_BIT(fwd_sys_cfg1_vals, MBY_FWD_SYS_CFG_1, TRAP_MTU_VIOLATIONS);
}

void MaskGen
(
    fm_uint32                          regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyNextHopToMaskGen  * const in,
          mbyMaskGenToTriggers * const out
)
{
    fm_uint32 rx_port = 0; // REVISIT !!!!
    
    mbyFwdPortCfg1 port_cfg1;
    getPortCfg1(regs, rx_port, &port_cfg1);

    mbyFwdPortCfg2 port_cfg2;
    getPortCfg2(regs, rx_port, &port_cfg2);

    mbyFwdSysCfg1 sys_cfg1;
    getSysCfg1 (regs, &sys_cfg1);

#if 0
    // initialize learning flag:
    state->LEARNING_ENABLED = 
        portCfg1.LEARNING_ENABLE &&
        (!state->NO_LEARN) &&
        ((state->L2_IFID1_STATE == HLP_MODEL_STP_STATE_LEARNING) ||
         (state->L2_IFID1_STATE == HLP_MODEL_STP_STATE_FORWARD));

    if (state->GLORT_CAM_MISS)
        state->AMASK |= HLP_MODEL_AMASK_DROP_CAM_MISS; // CAM miss -> drop frame
    
    if (state->TARGETED_DETERMINISTIC)
        state->AMASK |= HLP_MODEL_AMASK_SPECIAL; // frame is special delivery
    
    if (state->PARSER_ERROR)
        state->AMASK |= HLP_MODEL_AMASK_DROP_PARSER_ERR; // parser err -> drop frame

    if (state->TRAP_IGMP)
        state->AMASK |= HLP_MODEL_AMASK_TRAP_IGMP; // trap IGMP frame
    
    if (state->PARITY_ERROR)
        state->AMASK |= HLP_MODEL_AMASK_DROP_PERR; // parity err -> drop frame 

    if ((sysCfg1.DROP_MAC_CTRL_ETHERTYPE == TRUE) && (state->L2_ETYPE == HLP_MODEL_ETYPE_MAC_CONTROL))
        state->AMASK |= HLP_MODEL_AMASK_DROP_MAC_CTRL; // MAC CTRL Eth type 0x8808 Frame -> drop frame

    if ((sysCfg1.DROP_INVALID_SMAC == TRUE) &&
        (((state->L2_SMAC == 0) && (state->PARSER_INFO.window_parse_v == 0)) ||
         (fmModelIsBroadcastMacAddress(state->L2_SMAC)) ||
         (fmModelIsMulticastMacAddress(state->L2_SMAC))))
        state->AMASK |= HLP_MODEL_AMASK_DROP_SMAC; // SMAC frame -> drop frame
    
    if (fmModelIsCpuMacAddress(model, state->L2_SMAC))
        state->LEARNING_ENABLED = 0; // L2_SMAC = CPU MAC addr -> disable learning

    if (state->L2_SMAC == 0)
        state->LEARNING_ENABLED = 0; // L2_SMAC = 0 -> disable learning

    fm_bool logIpMcTtl = FALSE;
    ProcessTraps(model, &logIpMcTtl);

    ProcessPortSecurity(model);

    ProcessFiltering(model);

    ProcessFfuFlags(model);

    ProcessQCN(model);

    state->DMASK = state->PRE_RESOLVE_DMASK;

    ProcessEgressStpState(model);

    fm_byte learning_enabled_snapshot = state->LEARNING_ENABLED;

    ResolveAction(model);

    UpdateActionMask(model, logIpMcTtl);

    state->CPU_TRAP = (state->ACTION == HLP_MODEL_ACTION_TRAP);

    state->OPERATOR_ID = state->PKT_META[20] & 0x0f; 

    state->QOS_SWPRI &= 0x7; /*TODO: swpri 4bits, tc 3bits; to coordinate with rtl in act_res*/

    state->STORE_TRAP_ACTION = sysCfg1.STORE_TRAP_ACTION;

    // part 2

    HandleLAG(model);

    HandleLoopbackSuppress(model);

    fm_bool doNotTrapOrLog = (state->TRIGGERS.trapAction == HLP_MODEL_TRIG_ACTION_TRAP_REVERT);

    /* Trap to CPU */
    state->IDGLORT = state->TRIGGERS.destGlort;

    if ((state->CPU_TRAP || (state->TRIGGERS.trapAction == HLP_MODEL_TRIG_ACTION_TRAP_TRAP)) && !doNotTrapOrLog)
    {
        err = HandleTraps(model);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
    } else { //TODO: not !trap - should be default
        if (state->TRIGGERS.egressL3DomainAction == 0) {
            FM_SET_UNNAMED_FIELD64(state->PKT_META[21], 5, 3, state->L3_EDOMAIN & 0x07);
            FM_SET_UNNAMED_FIELD64(state->PKT_META[22], 0, 3, state->L3_EDOMAIN >> 3);
        }
        if (state->TRIGGERS.egressL2DomainAction == 0) {
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
        state->ACTION = HLP_MODEL_ACTION_MARKER_ERROR_DROPS;
#endif
}

