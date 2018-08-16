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
    fm_uint32             regs[MBY_REGISTER_ARRAY_SIZE],
    mbyFwdSysCfg1 * const cfg
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

static void getSysCfgRouter
(
    fm_uint32                  regs[MBY_REGISTER_ARRAY_SIZE],
    mbyFwdSysCfgRouter * const cfg
)
{
    fm_uint32 fwd_sys_cfg_router_vals[MBY_FWD_SYS_CFG_ROUTER_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_FWD_SYS_CFG_ROUTER(0), MBY_FWD_SYS_CFG_ROUTER_WIDTH, fwd_sys_cfg_router_vals);

    cfg->TRAP_IP_OPTIONS = FM_ARRAY_GET_BIT  (fwd_sys_cfg_router_vals, MBY_FWD_SYS_CFG_ROUTER, TRAP_IP_OPTIONS);
    cfg->TRAP_TTL1       = FM_ARRAY_GET_FIELD(fwd_sys_cfg_router_vals, MBY_FWD_SYS_CFG_ROUTER, TRAP_TTL1);
}

static fm_bool isCpuMacAddress
(
    fm_uint32        regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_macaddr mac_addr
)
{
    fm_uint32 fwd_cpu_mac_vals[MBY_FWD_CPU_MAC_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_FWD_CPU_MAC(0), MBY_FWD_CPU_MAC_WIDTH, fwd_cpu_mac_vals);
    fm_macaddr cpu_mac_addr = FM_ARRAY_GET_FIELD64(fwd_cpu_mac_vals, MBY_FWD_CPU_MAC, MAC_ADDR);

    fm_bool result = (mac_addr == cpu_mac_addr);
    return result;
}

void MaskGen
(
    fm_uint32                          regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyNextHopToMaskGen  * const in,
          mbyMaskGenToTriggers * const out
)
{
    // Get inputs:
    fm_uint32   rx_port         = in->RX_PORT;
    fm_bool     parser_window_v = in->PARSER_WINDOW_V; // was: in->PARSER_INFO.window_parse_v;
    fm_macaddr  l2_smac         = in->L2_SMAC;
    fm_macaddr  l2_dmac         = in->L2_DMAC;
    mbySTPState l2_ifid1_state  = in->L2_IFID1_STATE;
    fm_bool     no_learn        = in->NO_LEARN;
    
    // Configurations:
    mbyFwdPortCfg1 port_cfg1;
    getPortCfg1(regs, rx_port, &port_cfg1);

    mbyFwdPortCfg2 port_cfg2;
    getPortCfg2(regs, rx_port, &port_cfg2);

    mbyFwdSysCfg1 sys_cfg1;
    getSysCfg1 (regs, &sys_cfg1);

    mbyFwdSysCfgRouter sys_cfg_router;
    getSysCfgRouter(regs, &sys_cfg_router);

    // Logging:
    fm_bool logging_hit = FALSE;
    
    // --------------------------------------------------------------------------------
    // Learning:

    fm_bool l2_ifid1_learn   = ((l2_ifid1_state == MBY_STP_STATE_LEARNING) ||
                                (l2_ifid1_state == MBY_STP_STATE_FORWARD));
    fm_bool learning_allowed = port_cfg1.LEARNING_ENABLE && !no_learn && l2_ifid1_learn;
    fm_bool l2_smac_is_cpu   = isCpuMacAddress(regs, l2_smac);
    fm_bool l2_smac_is_zero  = (l2_smac == 0);
    fm_bool learning_enabled = learning_allowed && !l2_smac_is_cpu && !l2_smac_is_zero;

    // --------------------------------------------------------------------------------
    // Action Masks:

    fm_uint64 amask = 0;
    fm_byte log_amask = 0;
    
    if (in->GLORT_CAM_MISS)
        amask |= MBY_AMASK_DROP_CAM_MISS;   // CAM miss -> drop frame
    
    if (in->TARGETED_DETERMINISTIC)
        amask |= MBY_AMASK_SPECIAL;         // frame is special delivery
    
    if (in->PARSER_ERROR)
        amask |= MBY_AMASK_DROP_PARSER_ERR; // parser err -> drop frame

    if (in->TRAP_IGMP)
        amask |= MBY_AMASK_TRAP_IGMP;       // trap IGMP frame
    
    if (in->PARITY_ERROR)
        amask |= MBY_AMASK_DROP_PERR;       // parity err -> drop frame 

    if ((sys_cfg1.DROP_MAC_CTRL_ETHERTYPE == TRUE) && (in->L2_ETYPE == MBY_ETYPE_MAC_CONTROL))
        amask |= MBY_AMASK_DROP_MAC_CTRL;   // MAC CTRL Eth type 0x8808 Frame -> drop frame

    fm_bool drop_invalid_smac  = sys_cfg1.DROP_INVALID_SMAC;
    fm_bool l2_smac_is_invalid = l2_smac_is_zero && !parser_window_v;
    fm_bool l2_smac_is_broad   = isBroadcastMacAddress(l2_smac);
    fm_bool l2_smac_is_multi   = isMulticastMacAddress(l2_smac);

    if (drop_invalid_smac && (l2_smac_is_invalid || l2_smac_is_broad || l2_smac_is_multi))
        amask |= MBY_AMASK_DROP_SMAC;       // SMAC frame -> drop frame
    
    // --------------------------------------------------------------------------------
    // Traps:

    fm_bool log_ip_mc_ttl = FALSE;
    fm_bool l2_dmac_cpu   = isCpuMacAddress(regs, l2_dmac);
    fm_bool l2_dmac_zero  = (l2_dmac == 0);

    if (l2_dmac_cpu && !(l2_dmac_zero && parser_window_v))
        amask |= MBY_AMASK_TRAP_CPU_ADDR; // Trapping CPU addressed frame

    // Special packet handling:
    if ((l2_dmac & MBY_SPECIAL_DMASK) == (MBY_DMAC_IEEE_PREFIX & MBY_SPECIAL_DMASK))
    {
        fm_uint32 fwd_rsvd_mac_action_vals[MBY_FWD_IEEE_RESERVED_MAC_ACTION_WIDTH] = { 0 };
        mbyModelReadCSRMult(regs, MBY_FWD_IEEE_RESERVED_MAC_ACTION(0), MBY_FWD_IEEE_RESERVED_MAC_ACTION_WIDTH, fwd_rsvd_mac_action_vals);

        fm_uint32 fwd_rsvd_mac_trap_pri_vals[MBY_FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY_WIDTH] = { 0 };
        mbyModelReadCSRMult(regs, MBY_FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY(0), MBY_FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY_WIDTH, fwd_rsvd_mac_trap_pri_vals);

        fm_byte rmc_idx  = l2_dmac & 0x3F; // 6 bits (0..63)
        fm_byte action   = FM_ARRAY_GET_UNNAMED_FIELD(fwd_rsvd_mac_action_vals, 2 * rmc_idx, 2); // 2-bit action
        fm_bool trap_pri = FM_ARRAY_GET_UNNAMED_BIT  (fwd_rsvd_mac_trap_pri_vals, rmc_idx);

        switch (action)
        {
            case MBY_IEEE_RESERVED_MAC_ACTION_ACTION_SWITCHNORMALLY:
                amask |= MBY_AMASK_SWITCH_RESERVED_MAC;
                break;

            case MBY_IEEE_RESERVED_MAC_ACTION_ACTION_TRAP:
                amask |= (trap_pri) ? MBY_AMASK_TRAP_RESERVED_MAC_REMAP : MBY_AMASK_TRAP_RESERVED_MAC;
                break;

            case MBY_IEEE_RESERVED_MAC_ACTION_ACTION_DROP:
                amask |= MBY_AMASK_DROP_RESERVED_MAC;
                break;

            default:
            case MBY_IEEE_RESERVED_MAC_ACTION_ACTION_LOG:
                amask |= MBY_AMASK_LOG_MAC_CTRL;
                log_amask |= MBY_LOG_TYPE_RESERVED_MAC;
                break;
        }
    }

    if (in->MARK_ROUTED && (in->IP_MCAST_IDX == 0) && (in->L2_IVID1 == (in->L2_EVID1 & 0xFFF)))
        log_amask |= MBY_LOG_TYPE_ARP_REDIRECT;

    if (in->DROP_TTL)
    {
        if (in->IP_MCAST_IDX == 0)
        {
            if      (sys_cfg_router.TRAP_TTL1 == 0)
                amask |= MBY_AMASK_DROP_TTL;
            else if (sys_cfg_router.TRAP_TTL1 == 1 )
                amask |= (in->TRAP_ICMP) ? MBY_AMASK_TRAP_ICMP_TTL : MBY_AMASK_DROP_TTL;
            else if (sys_cfg_router.TRAP_TTL1 == 2 )
                amask |= (in->TRAP_ICMP) ? MBY_AMASK_TRAP_ICMP_TTL : MBY_AMASK_TRAP_TTL;
        }
        else // Frame is IP multicast
        {
            if ((sys_cfg_router.TRAP_TTL1 == 1) && in->TRAP_ICMP) {
                log_amask |= MBY_LOG_TYPE_ICMP;
                log_ip_mc_ttl = TRUE;
            } else if (sys_cfg_router.TRAP_TTL1 == 2) {
                log_amask |= (in->TRAP_ICMP) ? MBY_LOG_TYPE_ICMP : MBY_LOG_TYPE_TTL_IP_MC;
                log_ip_mc_ttl = TRUE;
            }
        }
    }

    if (sys_cfg_router.TRAP_IP_OPTIONS && in->TRAP_IP_OPTIONS && (in->IS_IPV6 || in->IS_IPV4))
        amask |= MBY_AMASK_TRAP_IP_OPTION;

    if (sys_cfg1.TRAP_MTU_VIOLATIONS && in->MTU_VIOLATION && in->MARK_ROUTED)
        amask |= MBY_AMASK_TRAP_MTU_VIO; // MTU violation -> trapping frame

    // --------------------------------------------------------------------------------
    // Port Security:

    if (port_cfg1.FILTER_VLAN_INGRESS && !in->L2_IVLAN1_MEMBERSHIP)
        amask |= MBY_AMASK_DROP_IV; // VLAN ingress violation -> dropping frame
    
    fm_bool mac_moved = (in->SA_HIT && (in->SA_RESULT.S_GLORT != in->CSGLORT));

    switch (in->SV_DROP)
    {
        case MBY_SV_MOVE_DROP_ADDR:   // secure addr violation -> dropping frame
            amask |= MBY_AMASK_DROP_SEC_ADDR; break;
        case MBY_SV_MOVE_DROP_PORT:   // secure port violation -> dropping frame
            amask |= MBY_AMASK_DROP_SEC_PORT; break;
        case MBY_SV_MOVE_DROP_STATIC: // static addr violation -> dropping frame
            amask |= MBY_AMASK_DROP_STATIC_ADDR; break;
        default: break;
    }

    // Ingress spanning tree check:
    switch (in->L2_IFID1_STATE)
    {
        case MBY_STP_STATE_DISABLE:
        case MBY_STP_STATE_LISTENING: // STP ingress (non-learning) violation -> dropping frame
            amask |= MBY_AMASK_DROP_INGRESS_STP_NON_LEARN; break; 
        case MBY_STP_STATE_LEARNING:  // STP ingress (learning) violation -> dropping frame
            amask |= MBY_AMASK_DROP_INGRESS_STP_LEARN; break;
        default: break;
    }

    // --------------------------------------------------------------------------------
    // Filtering:

    fm_bool l2_dmac_is_broad = isBroadcastMacAddress(l2_dmac);
    fm_bool l2_dmac_is_multi = isMulticastMacAddress(l2_dmac);
    fm_bool l2_dmac_is_uni   =   isUnicastMacAddress(l2_dmac);

    fm_byte fclass = 0;
    fm_byte xcast  = 0;
    
    if        (l2_dmac_is_broad) { 
        fclass = MBY_FCLASS_BROADCAST;
        xcast  = 2;
    } else if (l2_dmac_is_multi) { 
        fclass = MBY_FCLASS_MULTICAST;
        xcast  = 1;
    } else if (l2_dmac_is_uni) { 
        fclass = MBY_FCLASS_UNICAST;
        xcast  = 0;
    }

    fm_uint32 dmask = MBY_DEFAULT_DMASK;

    if (in->FLOOD_FORWARDED)
        amask |= MBY_AMASK_FLOOD;

    if (in->GLORT_FORWARDED)
        amask |= MBY_AMASK_GLORT;

    // Perform port-based filtering for switched packets
    dmask &= port_cfg1.DESTINATION_MASK;
    dmask &= port_cfg2.DESTINATION_MASK;

    // Ingress VLAN reflection check:
    if ( !in->MARK_ROUTED && !in->L2_IVLAN1_REFLECT && !in->TARGETED_DETERMINISTIC)
        dmask &= ~(FM_LITERAL_U64(1) << in->RX_PORT);

    // Prevent reflection: drop frame
    if (dmask == 0)
        amask |= MBY_AMASK_DROP_LOOPBACK;

    // Save pre-resolve dmask for later:
    fm_uint32 pre_resolve_dmask = dmask & in->GLORT_DMASK;

    if (pre_resolve_dmask == 0)
    {
        // Egress VLAN membership check:
        if (in->GLORT_DMASK == 0) { // Null Glort Dest: dropping frame
            amask |= MBY_AMASK_DROP_NULL_GLORTDEST;
            if (in->FLOOD_FORWARDED == 1) // Null Glort Dest & Flood Forwarded: dropping frame (DLF)
                amask |= MBY_AMASK_DROP_DLF;
        }
        else { // do not set amask loopback when also setting null dest.
            amask |= MBY_AMASK_DROP_LOOPBACK; // Loopback (port or VLAN refl. dis.): dropping frame
        }
    }
    else if (!in->TARGETED_DETERMINISTIC)
    {
        pre_resolve_dmask &= in->L2_EVLAN1_MEMBERSHIP; // VLAN egress filtering
        if (pre_resolve_dmask == 0)
            amask |= MBY_AMASK_DROP_EV; // VLAN egress violation: dropping frame
    }

    // --------------------------------------------------------------------------------
    
#if 0
    ProcessFfuFlags(model);

    ProcessQCN(model);
#endif

    dmask = pre_resolve_dmask; // 24-bit destination mask

#if 0
    ProcessEgressStpState(model);

    ResolveAction(model);

    UpdateActionMask(model, logIpMcTtl);
#endif

    fm_bool cpu_trap = (in->ACTION == MBY_ACTION_TRAP);

    out->CPU_TRAP          = cpu_trap;
    out->OPERATOR_ID       = in->OPERATOR_ID;
    out->QOS_SWPRI         = in->QOS_SWPRI;
    out->STORE_TRAP_ACTION = sys_cfg1.STORE_TRAP_ACTION;

#if 0
    HandleLAG(model);

    HandleLoopbackSuppress(model);
#endif
    out->IDGLORT = in->TRIGGERS.destGlort;

    // Trap:
    fm_bool trap_trap   = (in->TRIGGERS.trapAction == MBY_TRIG_ACTION_TRAP_TRAP);
    fm_bool trap_revert = (in->TRIGGERS.trapAction == MBY_TRIG_ACTION_TRAP_REVERT);
    fm_bool trap_log    = (in->TRIGGERS.trapAction == MBY_TRIG_ACTION_TRAP_LOG);

    fm_byte l3_edomain  = (in->TRIGGERS.egressL3DomainAction == 0) ? in->L3_EDOMAIN : 0;
    fm_byte l2_edomain  = (in->TRIGGERS.egressL2DomainAction == 0) ? in->L2_EDOMAIN : 0;

#if 0
    if ((cpu_trap || trap_trap) && !trap_revert)
        err = HandleTraps(model);
            
    if ((sys_cfg1.ENABLE_TRAP_PLUS_LOG || !cpu_trap || trap_trap || trap_log) && !trap_revert)
        err = HandleLogging(model);
#endif

//T regPtr = FM_MODEL_GET_REG_PTR(model, HLP_CM_APPLY_MCAST_EPOCH(0));
    fm_bool mcast_epoch = FALSE; //T FM_GET_BIT(*regPtr, HLP_CM_APPLY_MCAST_EPOCH, CURRENT);

//T regPtr = FM_MODEL_GET_REG_PTR(model, HLP_CM_APPLY_MIRROR_PROFILE_TABLE(in->MIRROR0_PROFILE_IDX, 0));
    fm_int mirror0_port = 0; //T FM_GET_FIELD(*regPtr, HLP_CM_APPLY_MIRROR_PROFILE_TABLE, PORT);

//T regPtr = FM_MODEL_GET_REG_PTR(model, HLP_CM_APPLY_MIRROR_PROFILE_TABLE(in>MIRROR1_PROFILE_IDX, 0));
    fm_int mirror1_port = 0; //T FM_GET_FIELD(*regPtr, HLP_CM_APPLY_MIRROR_PROFILE_TABLE, PORT);

    fm_uint64 fnmask = dmask; // normal forwarding mask

    // partial process of marker drop, can be further modified in cm_apply_checker based on RateLimit
    fm_uint32 action = 0;
#if 0
    if ((in->PKT_META[MBY_META_TYPE_OFF] == MBY_META_TYPE_MARKER) && (fnmask == 0))
        action = MBY_ACTION_MARKER_ERROR_DROPS;
#endif

    out->LEARNING_ENABLED  = learning_enabled;
    out->MCAST_EPOCH       = mcast_epoch;
    out->MIRROR0_PORT      = mirror0_port;
    out->MIRROR1_PORT      = mirror1_port;
    out->MIRROR0_PROFILE_V = (mirror0_port < MBY_PORTS_COUNT);
    out->MIRROR1_PROFILE_V = (mirror1_port < MBY_PORTS_COUNT);
    out->AMASK             = amask;
    out->FNMASK            = fnmask;
    out->DMASK             = dmask;
    out->LOG_AMASK         = log_amask;
    out->L3_EDOMAIN        = l3_edomain;
    out->L2_EDOMAIN        = l2_edomain;
    out->LOGGING_HIT       = logging_hit;
    out->ACTION            = action;
    out->MAC_MOVED         = mac_moved;
    out->FCLASS            = fclass;
    out->XCAST             = xcast;
}
