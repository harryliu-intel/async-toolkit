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

static void getLagCfg
(
    fm_uint32            regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint32      port,
    mbyFwdLagCfg * const cfg
)
{
    fm_uint32 fwd_lag_cfg_vals[MBY_FWD_LAG_CFG_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_FWD_LAG_CFG(port, 0), MBY_FWD_LAG_CFG_WIDTH, fwd_lag_cfg_vals);

    cfg->IN_LAG        = FM_ARRAY_GET_BIT  (fwd_lag_cfg_vals, MBY_FWD_LAG_CFG, IN_LAG);
    cfg->HASH_ROTATION = FM_ARRAY_GET_BIT  (fwd_lag_cfg_vals, MBY_FWD_LAG_CFG, HASH_ROTATION);
    cfg->INDEX         = FM_ARRAY_GET_FIELD(fwd_lag_cfg_vals, MBY_FWD_LAG_CFG, INDEX);
    cfg->LAG_SIZE      = FM_ARRAY_GET_FIELD(fwd_lag_cfg_vals, MBY_FWD_LAG_CFG, LAG_SIZE);
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

static void resolveAction
(
    // inputs:
    const fm_uint64   pri_amask,
    const fm_uint32   glort_dmask,
    const fm_byte     trap_tc,
    // inouts:
    fm_uint64 * const amask,
    fm_uint32 * const dmask,
    fm_bool   * const learning_enabled,
    fm_byte   * const cpu_code,
    fm_byte   * const qos_swpri,
    // output
    fm_uint32 * const action
)
{
    switch (pri_amask)
    {
        case FM_LITERAL_U64(0):
            *amask           |= MBY_AMASK_FORWARD_NORMAL;
            *action           = MBY_ACTION_NORMAL;
            break;

        case MBY_AMASK_DROP_PERR:
            *dmask            = 0;
            *action           = MBY_ACTION_DROP_PARITY;
            *learning_enabled = FALSE;
            break;

        case MBY_AMASK_SPECIAL:
            *dmask            = glort_dmask;
            *action           = MBY_ACTION_SPECIAL;
            *learning_enabled = FALSE;
            break;

        case MBY_AMASK_DROP_PARSER_ERR:
            *dmask            = 0;
            *action           = MBY_ACTION_DROP_PARSE;
            *learning_enabled = FALSE;
            break;

        case MBY_AMASK_TRAP_RESERVED_MAC:
            *dmask            = 0;
            *action           = MBY_ACTION_TRAP;
            *cpu_code         = MBY_CPU_CODE_RSVD_MAC;
            break;

        case MBY_AMASK_TRAP_RESERVED_MAC_REMAP:
            *dmask            = 0;
            *action           = MBY_ACTION_TRAP;
            *cpu_code         = MBY_CPU_CODE_RSVD_MAC;
            *qos_swpri        = trap_tc; // <-- REVISIT!!!
            break;
        
        case MBY_AMASK_DROP_MAC_CTRL:
            *dmask            = 0;
            *action           = MBY_ACTION_DROP_CONTROL;
            *learning_enabled = FALSE;
            break;

        case MBY_AMASK_DROP_RESERVED_MAC:
        case MBY_AMASK_DROP_SMAC:
        case MBY_AMASK_DROP_PROVISIONAL:
            *dmask            = 0;
            *action           = MBY_ACTION_BANK5_OTHER_DROPS;
            *learning_enabled = FALSE;
            break;

        case MBY_AMASK_DROP_SEC_ADDR :
        case MBY_AMASK_DROP_SEC_PORT :
        case MBY_AMASK_DROP_STATIC_ADDR :
            *dmask            = 0;
            *action           = MBY_ACTION_DROP_SV;
            break;

        case MBY_AMASK_TRAP_CPU_ADDR:
            *dmask            = 0;
            *action           = MBY_ACTION_TRAP;
            *cpu_code         = MBY_CPU_CODE_CPU_ADDRESS;
            break;

        case MBY_AMASK_DROP_IV:
            *dmask            = 0;
            *action           = MBY_ACTION_DROP_IV;
            *learning_enabled = FALSE;
            break;

        case MBY_AMASK_DROP_INGRESS_STP_NON_LEARN:
            *dmask            = 0;
            *action           = MBY_ACTION_DROP_STP;
            *learning_enabled = FALSE;
            break;

        case MBY_AMASK_DROP_INGRESS_STP_LEARN:
            *dmask            = 0;
            *action           = MBY_ACTION_DROP_STP;
            break;

        case MBY_AMASK_DROP_FFU:
            *dmask            = 0;
            *action           = MBY_ACTION_DROP_FFU;
            *learning_enabled = FALSE;
            break;

        case MBY_AMASK_TRAP_FFU:
            *dmask            = 0;
            *action           = MBY_ACTION_TRAP;
            *cpu_code         = MBY_CPU_CODE_FFU;
            break;

        case MBY_AMASK_TRAP_ICMP_TTL:
            *dmask            = 0;
            *action           = MBY_ACTION_TRAP;
            *cpu_code         = MBY_CPU_CODE_ICMP;
            break;

        case MBY_AMASK_TRAP_IP_OPTION:
            *dmask            = 0;
            *action           = MBY_ACTION_TRAP;
            *cpu_code         = MBY_CPU_CODE_IP_OPTION;
            break;

        case MBY_AMASK_TRAP_MTU_VIO:
            *dmask            = 0;
            *action           = MBY_ACTION_TRAP;
            *cpu_code         = MBY_CPU_CODE_MTU;
            break;

        case MBY_AMASK_TRAP_IGMP:
            *dmask            = 0;
            *action           = MBY_ACTION_TRAP;
            *cpu_code         = MBY_CPU_CODE_IGMP;
            break;

        case MBY_AMASK_TRAP_TTL:
            *dmask             = 0;
            *action            = MBY_ACTION_TRAP;
            *cpu_code         = MBY_CPU_CODE_TTL;
            break;

        case MBY_AMASK_DROP_TTL:
            *dmask             = 0;
            *action            = MBY_ACTION_DROP_TTL;
            break;

        case MBY_AMASK_DROP_NULL_GLORTDEST:
            *dmask             = 0;
            *action            = MBY_ACTION_BANK5_OTHER_DROPS;
            break;

        case MBY_AMASK_DROP_EV:
            *dmask             = 0;
            *action            = MBY_ACTION_DROP_EV;
            break;

        case MBY_AMASK_DROP_DLF:
            *dmask            = 0;
            *action           = MBY_ACTION_DROP_DLF;
            *learning_enabled = FALSE;
            break;

        case MBY_AMASK_DROP_CAM_MISS:
            *dmask            = 0;
            *action           = MBY_ACTION_DROP_CAM;
            *learning_enabled = 0;
            break;

        case MBY_AMASK_DROP_EGRESS_STP:
            *dmask            = 0;
            *action           = MBY_ACTION_DROP_STP;
            break;

        case MBY_AMASK_DROP_LOOPBACK:
            *dmask            = 0;
            *action           = MBY_ACTION_DROP_LOOPBACK;
            break;

        case MBY_AMASK_GLORT:
            *action           = MBY_ACTION_GLORT_FORWARDED;
            break;

        case MBY_AMASK_FLOOD:
            *action           = MBY_ACTION_FLOOD;
            break;

        case MBY_AMASK_SWITCH_RESERVED_MAC:
            *action           = MBY_ACTION_NORMAL;
            break;

        default:
            *dmask            = 0;
            *action           = MBY_ACTION_BANK5_OTHER_DROPS;
            break;
    }
}

void MaskGen
(
    fm_uint32                          regs[MBY_REGISTER_ARRAY_SIZE],
    const mbyNextHopToMaskGen  * const in,
          mbyMaskGenToTriggers * const out
)
{
    // Read inputs:
    fm_uint32   rx_port         = in->RX_PORT;
    fm_bool     parser_window_v = in->PARSER_WINDOW_V; // was: in->PARSER_INFO.window_parse_v;
    fm_macaddr  l2_smac         = in->L2_SMAC;
    fm_macaddr  l2_dmac         = in->L2_DMAC;
    mbySTPState l2_ifid1_state  = in->L2_IFID1_STATE;
    fm_bool     no_learn        = in->NO_LEARN;
    fm_uint16   idglort         = in->IDGLORT; // in->TRIGGERS.destGlort; // <-- REVISIT!!!
    fm_uint32   glort_dmask     = in->GLORT_DMASK;
    fm_byte     qos_swpri       = in->QOS_SWPRI;
    fm_byte     operator_id     = in->OPERATOR_ID;
    fm_bool     rx_mirror_in    = in->RX_MIRROR;
    fm_bool     mark_routed     = in->MARK_ROUTED;
    fm_uint16   csglort         = in->CSGLORT;

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

        fm_byte rmc_idx    = l2_dmac & 0x3F; // 6 bits (0..63)
        fm_byte mac_action = FM_ARRAY_GET_UNNAMED_FIELD(fwd_rsvd_mac_action_vals, 2 * rmc_idx, 2); // 2-bit action
        fm_bool trap_pri   = FM_ARRAY_GET_UNNAMED_BIT  (fwd_rsvd_mac_trap_pri_vals, rmc_idx);

        switch (mac_action)
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

    if (mark_routed && (in->IP_MCAST_IDX == 0) && (in->L2_IVID1 == (in->L2_EVID1 & 0xFFF)))
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

    if (sys_cfg1.TRAP_MTU_VIOLATIONS && in->MTU_VIOLATION && mark_routed)
        amask |= MBY_AMASK_TRAP_MTU_VIO; // MTU violation -> trapping frame

    // --------------------------------------------------------------------------------
    // Port Security:

    if (port_cfg1.FILTER_VLAN_INGRESS && !in->L2_IVLAN1_MEMBERSHIP)
        amask |= MBY_AMASK_DROP_IV; // VLAN ingress violation -> dropping frame
    
    fm_bool mac_moved = (in->SA_HIT && (in->SA_RESULT.S_GLORT != csglort));

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

    switch (in->L2_IFID1_STATE) // Ingress spanning tree check
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
    if ( !mark_routed && !in->L2_IVLAN1_REFLECT && !in->TARGETED_DETERMINISTIC)
        dmask &= ~(FM_LITERAL_U64(1) << rx_port);

    // Prevent reflection: drop frame
    if (dmask == 0)
        amask |= MBY_AMASK_DROP_LOOPBACK;

    // Save pre-resolve dmask for later:
    fm_uint32 pre_resolve_dmask = dmask & glort_dmask;

    if (pre_resolve_dmask == 0)
    {
        // Egress VLAN membership check:
        if (glort_dmask == 0) { // Null Glort Dest: dropping frame
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
    // Classifier Flags:    

    if (in->FFU_FLAGS.drop)
        amask |= MBY_AMASK_DROP_FFU; // dropping frame

    if (in->FFU_FLAGS.trap)
        amask |= MBY_AMASK_TRAP_FFU; // trapping frame

    if (in->FFU_FLAGS.log)
        log_amask |= MBY_LOG_TYPE_FFU; // logging fram

    fm_bool rx_mirror_out       = FALSE;
    fm_bool mirror0_profile_v   = FALSE;
    fm_bool mirror1_profile_v   = FALSE;
    fm_byte mirror0_profile_idx = 0;
    fm_byte mirror1_profile_idx = 0;

    if (in->FFU_FLAGS.rx_mirror)
    {
        rx_mirror_out     = TRUE;  // RX mirroring frame
        mirror1_profile_v = rx_mirror_in;

        fm_uint32 fwd_rx_mirror_cfg_vals[MBY_FWD_RX_MIRROR_CFG_WIDTH] = { 0 };
        mbyModelReadCSRMult(regs, MBY_FWD_RX_MIRROR_CFG(0), MBY_FWD_RX_MIRROR_CFG_WIDTH, fwd_rx_mirror_cfg_vals);

        mirror1_profile_idx = FM_ARRAY_GET_FIELD(fwd_rx_mirror_cfg_vals, MBY_FWD_RX_MIRROR_CFG, MIRROR_PROFILE_IDX);
    }

    // --------------------------------------------------------------------------------
    // QCN:

    fm_uint32 fwd_qcn_mirror_cfg_vals[MBY_FWD_QCN_MIRROR_CFG_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_FWD_QCN_MIRROR_CFG(0), MBY_FWD_QCN_MIRROR_CFG_WIDTH, fwd_qcn_mirror_cfg_vals);

    fm_byte mirror_profile_idx = FM_ARRAY_GET_FIELD(fwd_qcn_mirror_cfg_vals, MBY_FWD_QCN_MIRROR_CFG, MIRROR_PROFILE_IDX);
    fm_byte mirror_session     = FM_ARRAY_GET_FIELD(fwd_qcn_mirror_cfg_vals, MBY_FWD_QCN_MIRROR_CFG, MIRROR_SESSION);

    fm_uint32 cm_mirror0_profile_vals[MBY_CM_APPLY_MIRROR_PROFILE_TABLE_WIDTH] = { 0 };
    fm_uint32 cm_mirror1_profile_vals[MBY_CM_APPLY_MIRROR_PROFILE_TABLE_WIDTH] = { 0 };

    mbyModelReadCSRMult(regs, MBY_CM_APPLY_MIRROR_PROFILE_TABLE(mirror0_profile_idx, 0), MBY_CM_APPLY_MIRROR_PROFILE_TABLE_WIDTH, cm_mirror0_profile_vals);
    mbyModelReadCSRMult(regs, MBY_CM_APPLY_MIRROR_PROFILE_TABLE(mirror1_profile_idx, 0), MBY_CM_APPLY_MIRROR_PROFILE_TABLE_WIDTH, cm_mirror1_profile_vals);

    fm_uint32 mirror0_port = FM_ARRAY_GET_FIELD(cm_mirror0_profile_vals, MBY_CM_APPLY_MIRROR_PROFILE_TABLE, PORT);
    fm_uint32 mirror1_port = FM_ARRAY_GET_FIELD(cm_mirror1_profile_vals, MBY_CM_APPLY_MIRROR_PROFILE_TABLE, PORT);

    fm_bool qcn_mirror0_profile_v   = FALSE;
    fm_bool qcn_mirror1_profile_v   = FALSE;

    if (rx_mirror_in && (mirror_session == 2)) {
    	qcn_mirror1_profile_v = TRUE;
        mirror1_profile_v     = (mirror0_port < MBY_PORTS_COUNT);
    	mirror1_profile_idx   = mirror_profile_idx;
    }

    if (mirror_session == 1) {
    	qcn_mirror0_profile_v = TRUE;
        mirror0_profile_v     = (mirror0_port < MBY_PORTS_COUNT);
    	mirror0_profile_idx   = mirror_profile_idx;
    }

    // --------------------------------------------------------------------------------
    // Egress STP state:
    
    dmask = pre_resolve_dmask; // 24-bit destination mask
    if (((amask & MBY_AMASK_SPECIAL) == 0) && (dmask != 0)) {
        dmask &= in->L2_EFID1_STATE; // STP egress filtering: DMASK
        if (dmask == 0)
            amask |= MBY_AMASK_DROP_EGRESS_STP; // STP egress violation
    }

    // --------------------------------------------------------------------------------
    // Pre-Resolve:

    fm_uint32 pre_resolve_action = 0; // How does this affect 'action'? <-- REVISIT!!!
    
    if (amask & MBY_AMASK_SPECIAL) {
        pre_resolve_action = MBY_ACTION_SPECIAL;
        pre_resolve_dmask  = glort_dmask;
        dmask              = glort_dmask;
    }
    else if (amask & MBY_AMASK_FLOOD)
        pre_resolve_action = MBY_ACTION_FLOOD;
    else
        pre_resolve_action = MBY_ACTION_NORMAL;

    fm_uint16 pre_resolve_dglort = idglort; // Ditto for 'dglort' <-- REVISIT!!!

    // --------------------------------------------------------------------------------
    // Resolve Action:

    fm_uint64 pri_amask = 0; // bitmask representing the index of the highest priority bit
    for (fm_uint i = 0; i < MBY_AMASK_WIDTH; i++) {
        pri_amask = FM_LITERAL_U64(1) << i;
        if (amask & pri_amask)
            break;
    }

    fm_uint32 action = 0;
    fm_byte   cpu_code = 0;
    
    fm_uint32 fwd_rsvd_mac_cfg_vals[MBY_FWD_IEEE_RESERVED_MAC_CFG_WIDTH] = { 0 };
    mbyModelReadCSRMult(regs, MBY_FWD_IEEE_RESERVED_MAC_CFG(0), MBY_FWD_IEEE_RESERVED_MAC_CFG_WIDTH, fwd_rsvd_mac_cfg_vals);
    fm_byte trap_tc = FM_ARRAY_GET_FIELD(fwd_rsvd_mac_cfg_vals, MBY_FWD_IEEE_RESERVED_MAC_CFG, TRAP_TC);

    resolveAction
    (
        pri_amask,
        glort_dmask,
        trap_tc,
        &amask,
        &dmask,
        &learning_enabled,
        &cpu_code,
        &qos_swpri,
        &action
    );

    fm_uint32 fnmask = dmask; // normal forwarding mask - is this correct? <-- REVISIT!!!

    // --------------------------------------------------------------------------------
    // Update Action Mask

    if (log_amask & MBY_LOG_TYPE_FFU)
        amask |= MBY_AMASK_LOG_INGRESS_FFU;

    if (log_amask & MBY_LOG_TYPE_RESERVED_MAC)
        amask |= MBY_AMASK_LOG_MAC_CTRL;

    if (log_amask & MBY_LOG_TYPE_ARP_REDIRECT)
        amask |= MBY_AMASK_LOG_ARP_REDIRECT;

    if (log_amask & MBY_LOG_TYPE_ICMP)
        amask |= MBY_AMASK_LOG_IP_ICMP;

    if (log_amask & MBY_LOG_TYPE_TTL_IP_MC)
        amask |= MBY_AMASK_LOG_IP_TTL;

    if (rx_mirror_in)
        amask |= MBY_AMASK_MIRROR_INGRESS_FFU;

    // --------------------------------------------------------------------------------    
    // LAG:

    // GLORT Lookup - strict targeted deterministic mode:
    fm_bool skip_lookup
        = (((action == MBY_ACTION_REDIRECT_TRIG) && !in->TRIGGERS.filterDestMask) ||
           ((action != MBY_ACTION_REDIRECT_TRIG) &&  in->TARGETED_DETERMINISTIC));

    if (!skip_lookup && (dmask != 0))
    {
        for (fm_uint i = 0; i < MBY_FABRIC_LOG_PORTS; i++)
        {
            mbyFwdLagCfg lag_cfg;
            getLagCfg(regs, i, &lag_cfg);
            if (!lag_cfg.IN_LAG)
                continue;
    
            fm_uint32 hash = (lag_cfg.HASH_ROTATION) ? in->HASH_ROT_B : in->HASH_ROT_A;
            hash %= (lag_cfg.LAG_SIZE == 0) ? 16 : lag_cfg.LAG_SIZE;
            if (hash != lag_cfg.INDEX)
                dmask &= ~(FM_LITERAL_U64(1) << i);
        }

        if (dmask == 0)
            action = MBY_ACTION_BANK5_OTHER_DROPS;
    }

    // --------------------------------------------------------------------------------
    // Loopback Suppression Filtering:

    fm_bool skip_suppress = in->TARGETED_DETERMINISTIC || mark_routed || (dmask == 0);

    for (fm_uint i = 0; !skip_suppress && (i < MBY_FABRIC_LOG_PORTS); i++)
    {
        fm_uint32 cm_lpbk_suppress_vals[MBY_CM_APPLY_LOOPBACK_SUPPRESS_WIDTH] = { 0 };
        mbyModelReadCSRMult(regs, MBY_CM_APPLY_LOOPBACK_SUPPRESS(i, 0), MBY_CM_APPLY_LOOPBACK_SUPPRESS_WIDTH, cm_lpbk_suppress_vals);

        fm_uint16 lpbk_glort_mask = FM_ARRAY_GET_FIELD64(cm_lpbk_suppress_vals, MBY_CM_APPLY_LOOPBACK_SUPPRESS, GLORT_MASK);
        fm_uint16 lpbk_glort      = FM_ARRAY_GET_FIELD64(cm_lpbk_suppress_vals, MBY_CM_APPLY_LOOPBACK_SUPPRESS, GLORT); 

        if ((csglort & lpbk_glort_mask) == lpbk_glort)
            dmask &= ~(FM_LITERAL_U64(1) << i);
    }

    if (dmask == 0 )
        action = MBY_ACTION_DROP_LOOPBACK; // dropping frame

    // --------------------------------------------------------------------------------

    // Trap:
    fm_bool cpu_trap    = (action == MBY_ACTION_TRAP);

    fm_bool trap_trap   = (in->TRIGGERS.trapAction == MBY_TRIG_ACTION_TRAP_TRAP);
    fm_bool trap_revert = (in->TRIGGERS.trapAction == MBY_TRIG_ACTION_TRAP_REVERT);
    fm_bool trap_log    = (in->TRIGGERS.trapAction == MBY_TRIG_ACTION_TRAP_LOG);

    fm_byte l3_edomain  = (in->TRIGGERS.egressL3DomainAction == 0) ? in->L3_EDOMAIN : 0;
    fm_byte l2_edomain  = (in->TRIGGERS.egressL2DomainAction == 0) ? in->L2_EDOMAIN : 0;

    if ((cpu_trap || trap_trap) && !trap_revert)
    { /* err = HandleTraps(model) */; }
            
    if ((sys_cfg1.ENABLE_TRAP_PLUS_LOG || !cpu_trap || trap_trap || trap_log) && !trap_revert)
    { /* err = HandleLogging(model) */; }

    // Write outputs:
    out->LEARNING_ENABLED      = learning_enabled;
    out->MIRROR0_PORT          = mirror0_port;
    out->MIRROR1_PORT          = mirror1_port;
    out->MIRROR0_PROFILE_V     = mirror0_profile_v;
    out->MIRROR1_PROFILE_V     = mirror1_profile_v;
    out->MIRROR0_PROFILE_IDX   = mirror0_profile_idx;
    out->MIRROR1_PROFILE_IDX   = mirror1_profile_idx;
    out->QCN_MIRROR0_PROFILE_V = qcn_mirror0_profile_v;
    out->QCN_MIRROR1_PROFILE_V = qcn_mirror1_profile_v;
    out->AMASK                 = amask;
    out->FNMASK                = fnmask;
    out->DMASK                 = dmask;
    out->LOG_AMASK             = log_amask;
    out->L3_EDOMAIN            = l3_edomain;
    out->L2_EDOMAIN            = l2_edomain;
    out->LOGGING_HIT           = logging_hit;
    out->ACTION                = action;
    out->MAC_MOVED             = mac_moved;
    out->FCLASS                = fclass;
    out->XCAST                 = xcast;
    out->IDGLORT               = idglort;
    out->CPU_CODE              = cpu_code;
    out->QOS_SWPRI             = qos_swpri;
    out->CPU_TRAP              = cpu_trap;
    out->OPERATOR_ID           = operator_id;
    out->STORE_TRAP_ACTION     = sys_cfg1.STORE_TRAP_ACTION;
    out->RX_MIRROR             = rx_mirror_out;
}
