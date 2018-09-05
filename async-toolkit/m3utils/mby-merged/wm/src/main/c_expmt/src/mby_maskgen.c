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
    fm_uint64 fwd_port_cfg1_reg = 0;
    mbyModelReadCSR64(regs, MBY_FWD_PORT_CFG_1(port, 0), &fwd_port_cfg1_reg);

    cfg->LEARNING_ENABLE     = FM_GET_BIT64  (fwd_port_cfg1_reg, MBY_FWD_PORT_CFG_1, LEARNING_ENABLE);
    cfg->FILTER_VLAN_INGRESS = FM_GET_BIT64  (fwd_port_cfg1_reg, MBY_FWD_PORT_CFG_1, FILTER_VLAN_INGRESS);
    cfg->DESTINATION_MASK    = FM_GET_FIELD64(fwd_port_cfg1_reg, MBY_FWD_PORT_CFG_1, DESTINATION_MASK);
}

static void getPortCfg2
(
    fm_uint32              regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint32        port, // RX port
    mbyFwdPortCfg2 * const cfg
)
{
    fm_uint64 fwd_port_cfg2_reg = 0;
    mbyModelReadCSR64(regs, MBY_FWD_PORT_CFG_2(port, 0), &fwd_port_cfg2_reg);

    cfg->DESTINATION_MASK = FM_GET_FIELD64(fwd_port_cfg2_reg, MBY_FWD_PORT_CFG_2, DESTINATION_MASK);
}

static void getSysCfg1
(
    fm_uint32             regs[MBY_REGISTER_ARRAY_SIZE],
    mbyFwdSysCfg1 * const cfg
)
{
    fm_uint64 fwd_sys_cfg1_reg = 0;
    mbyModelReadCSR64(regs, MBY_FWD_SYS_CFG_1(0), &fwd_sys_cfg1_reg);

    cfg->STORE_TRAP_ACTION       = FM_GET_BIT64(fwd_sys_cfg1_reg, MBY_FWD_SYS_CFG_1, STORE_TRAP_ACTION);
    cfg->DROP_MAC_CTRL_ETHERTYPE = FM_GET_BIT64(fwd_sys_cfg1_reg, MBY_FWD_SYS_CFG_1, DROP_MAC_CTRL_ETHERTYPE);
    cfg->DROP_INVALID_SMAC       = FM_GET_BIT64(fwd_sys_cfg1_reg, MBY_FWD_SYS_CFG_1, DROP_INVALID_SMAC);
    cfg->ENABLE_TRAP_PLUS_LOG    = FM_GET_BIT64(fwd_sys_cfg1_reg, MBY_FWD_SYS_CFG_1, ENABLE_TRAP_PLUS_LOG);
    cfg->TRAP_MTU_VIOLATIONS     = FM_GET_BIT64(fwd_sys_cfg1_reg, MBY_FWD_SYS_CFG_1, TRAP_MTU_VIOLATIONS);
}

static void getSysCfgRouter
(
    fm_uint32                  regs[MBY_REGISTER_ARRAY_SIZE],
    mbyFwdSysCfgRouter * const cfg
)
{
    fm_uint64 fwd_sys_cfg_router_reg = 0;
    mbyModelReadCSR64(regs, MBY_FWD_SYS_CFG_ROUTER(0), &fwd_sys_cfg_router_reg);

    cfg->TRAP_IP_OPTIONS = FM_GET_BIT64  (fwd_sys_cfg_router_reg, MBY_FWD_SYS_CFG_ROUTER, TRAP_IP_OPTIONS);
    cfg->TRAP_TTL1       = FM_GET_FIELD64(fwd_sys_cfg_router_reg, MBY_FWD_SYS_CFG_ROUTER, TRAP_TTL1);
}

static void getLagCfg
(
    fm_uint32            regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint32      port,
    mbyFwdLagCfg * const cfg
)
{
    fm_uint64 fwd_lag_cfg_reg = 0;
    mbyModelReadCSR64(regs, MBY_FWD_LAG_CFG(port, 0), &fwd_lag_cfg_reg);

    cfg->IN_LAG        = FM_GET_BIT64  (fwd_lag_cfg_reg, MBY_FWD_LAG_CFG, IN_LAG);
    cfg->HASH_ROTATION = FM_GET_BIT64  (fwd_lag_cfg_reg, MBY_FWD_LAG_CFG, HASH_ROTATION);
    cfg->INDEX         = FM_GET_FIELD64(fwd_lag_cfg_reg, MBY_FWD_LAG_CFG, INDEX);
    cfg->LAG_SIZE      = FM_GET_FIELD64(fwd_lag_cfg_reg, MBY_FWD_LAG_CFG, LAG_SIZE);
}

static void getGlortCamEntry
(
    fm_uint32           regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_byte       cam_index,
    mbyGlortCam * const cam_entry
)
{
    fm_uint64 glort_cam_reg = 0;
    mbyModelReadCSR64(regs, MBY_GLORT_CAM(cam_index, 0), &glort_cam_reg);

    cam_entry->KEY_INVERT = FM_GET_FIELD64(glort_cam_reg, MBY_GLORT_CAM, KEY_INVERT);
    cam_entry->KEY        = FM_GET_FIELD64(glort_cam_reg, MBY_GLORT_CAM, KEY);
}

static void getGlortDestTableEntry
(
    fm_uint32                 regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint16           table_index,
    mbyGlortDestTable * const table_entry
)
{
    fm_uint64 glort_dest_table_reg = 0;
    mbyModelReadCSR64(regs, MBY_GLORT_DEST_TABLE(table_index, 0), &glort_dest_table_reg);

    table_entry->IP_MULTICAST_INDEX = FM_GET_FIELD64(glort_dest_table_reg, MBY_GLORT_DEST_TABLE, IP_MULTICAST_INDEX);
    table_entry->DEST_MASK          = FM_GET_FIELD64(glort_dest_table_reg, MBY_GLORT_DEST_TABLE, DEST_MASK);
}

static void getGlortRamEntry
(
    fm_uint32           regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_byte       ram_index,
    mbyGlortRam * const ram_entry
)
{
    fm_uint64 glort_ram_reg = 0;
    mbyModelReadCSR64(regs, MBY_GLORT_RAM(ram_index, 0), &glort_ram_reg);

    ram_entry->SKIP_DGLORT_DEC   = FM_GET_BIT64  (glort_ram_reg, MBY_GLORT_RAM, SKIP_DGLORT_DEC);
    ram_entry->HASH_ROTATION     = FM_GET_BIT64  (glort_ram_reg, MBY_GLORT_RAM, HASH_ROTATION);
    ram_entry->DEST_COUNT        = FM_GET_FIELD64(glort_ram_reg, MBY_GLORT_RAM, DEST_COUNT);
    ram_entry->RANGE_SUB_INDEX_A = FM_GET_FIELD64(glort_ram_reg, MBY_GLORT_RAM, RANGE_SUB_INDEX_A);
    ram_entry->RANGE_SUB_INDEX_B = FM_GET_FIELD64(glort_ram_reg, MBY_GLORT_RAM, RANGE_SUB_INDEX_B);
    ram_entry->DEST_INDEX        = FM_GET_FIELD64(glort_ram_reg, MBY_GLORT_RAM, DEST_INDEX);
    ram_entry->STRICT            = FM_GET_FIELD64(glort_ram_reg, MBY_GLORT_RAM, STRICT);
}

static fm_status lookUpRamEntry
(
    fm_uint32           regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint16     idglort,
    mbyGlortRam * const glort_ram
)
{
    fm_bool cam_hit = FALSE;

    // The highest numbered GLORT_CAM entry has highest precendence:
    for (fm_int i = MBY_GLORT_CAM_ENTRIES - 1; i >= 0; i--)
    {
        fm_byte index = i;

        mbyGlortCam glort_cam;
        getGlortCamEntry(regs, index, &glort_cam);

        fm_uint16 mask    = glort_cam.KEY ^ glort_cam.KEY_INVERT;
        fm_uint16 key     = glort_cam.KEY;
        fm_uint16 key_inv = glort_cam.KEY_INVERT;

        if (((key & key_inv) == 0) && ((idglort & mask) == (key & mask)))
        {
            getGlortRamEntry(regs, index, glort_ram);
            cam_hit = TRUE;
            break;
        }
    }

    if (!cam_hit)  // if glort_cam is not hit then zero out glort_ram_entry
    {
        glort_ram->SKIP_DGLORT_DEC   = 0;
        glort_ram->HASH_ROTATION     = 0;
        glort_ram->DEST_COUNT        = 0;
        glort_ram->RANGE_SUB_INDEX_A = 0;
        glort_ram->RANGE_SUB_INDEX_B = 0;
        glort_ram->DEST_INDEX        = 0;
        glort_ram->STRICT            = 0;
    }

    fm_status sts = (cam_hit) ? FM_OK : FM_FAIL;
    return sts;
}

static void lookUpDestEntry
(
    fm_uint32                 regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_uint16           idglort,
    const fm_bool             strict_glort_routing,
    const fm_uint32           hash_rot_a,
    const fm_uint32           hash_rot_b,
    const mbyGlortRam         glort_ram,
    mbyGlortDestTable * const glort_dest_table
)
{
    fm_uint16 length_a   = (glort_ram.RANGE_SUB_INDEX_A >> 4) & 0xF;
    fm_uint16 length_b   = (glort_ram.RANGE_SUB_INDEX_B >> 4) & 0xF;
    fm_uint16 offset_a   =  glort_ram.RANGE_SUB_INDEX_A & 0xF;
    fm_uint16 offset_b   =  glort_ram.RANGE_SUB_INDEX_B & 0xF;
    fm_uint16 glort_a    = (idglort >> offset_a) & ((1 << length_a) - 1);
    fm_uint16 glort_b    = (idglort >> offset_b) & ((1 << length_b) - 1);
    fm_uint16 dest_index = 0;

    if (strict_glort_routing) {
        dest_index = glort_ram.DEST_INDEX + (glort_b << length_a) + glort_a;
    } else {
        // TODO Clarify calculation precedence for '%' and '?'.
        fm_uint32 hash = ((glort_ram.HASH_ROTATION) ? hash_rot_b : hash_rot_a)
            % (glort_ram.DEST_COUNT == 0) ? 16 : glort_ram.DEST_COUNT;
        dest_index = glort_ram.DEST_INDEX + (hash << length_a) + glort_a;
    }

    dest_index &= 0xFFF; // remove carry bit

    getGlortDestTableEntry(regs, dest_index, glort_dest_table);
}

static fm_bool isCpuMacAddress
(
    fm_uint32        regs[MBY_REGISTER_ARRAY_SIZE],
    const fm_macaddr mac_addr
)
{
    fm_uint64 fwd_cpu_mac_reg = 0;
    mbyModelReadCSR64(regs, MBY_FWD_CPU_MAC(0), &fwd_cpu_mac_reg);
    fm_macaddr cpu_mac_addr = FM_GET_FIELD64(fwd_cpu_mac_reg, MBY_FWD_CPU_MAC, MAC_ADDR);

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
    const fm_uint64          amask_in               = in->AMASK;
    const fm_uint16          csglort                = in->CSGLORT;
    const fm_bool            da_hit                 = in->DA_HIT;
    const fm_bool            drop_ttl               = in->DROP_TTL;
    const mbyClassifierFlags ffu_flags              = in->FFU_FLAGS;
    const fm_bool            flood_forwarded        = in->FLOOD_FORWARDED;
    const fm_uint32          glort_dmask_in         = in->GLORT_DMASK;
    const fm_bool            glort_forwarded        = in->GLORT_FORWARDED;
    const fm_uint32          hash_rot_a             = in->HASH_ROT_A;
    const fm_uint32          hash_rot_b             = in->HASH_ROT_A;
    const fm_uint16          idglort                = in->IDGLORT;         // in->TRIGGERS.destGlort; // <-- REVISIT!!!
    const fm_bool            is_ipv4                = in->IS_IPV4;
    const fm_bool            is_ipv6                = in->IS_IPV6;
    const fm_macaddr         l2_dmac                = in->L2_DMAC;
    const fm_uint16          l2_edomain_in          = in->L2_EDOMAIN;
    const fm_uint32          l2_efid1_state         = in->L2_EFID1_STATE;
    const fm_uint16          l2_etype               = in->L2_ETYPE;
    const fm_uint16          l2_evid1               = in->L2_EVID1;
    const fm_uint32          l2_evlan1_membership   = in->L2_EVLAN1_MEMBERSHIP;
    const mbyStpState        l2_ifid1_state         = in->L2_IFID1_STATE;
    const fm_uint16          l2_ivid1               = in->L2_IVID1;
    const fm_bool            l2_ivlan1_membership   = in->L2_IVLAN1_MEMBERSHIP;
    const fm_bool            l2_ivlan1_reflect      = in->L2_IVLAN1_REFLECT;
    const fm_macaddr         l2_smac                = in->L2_SMAC;
    const fm_byte            l3_edomain_in          = in->L3_EDOMAIN;
    const fm_bool            mark_routed            = in->MARK_ROUTED;
    const fm_bool            mtu_violation          = in->MTU_VIOLATION;
    const fm_bool            no_learn               = in->NO_LEARN;
    const fm_byte            operator_id            = in->OPERATOR_ID;
    const fm_bool            parity_error           = in->PARITY_ERROR;
    const fm_bool            parser_window_v        = in->PARSER_WINDOW_V;
    const fm_bool            parser_error           = in->PARSER_ERROR;
    const fm_bool            pa_drop                = in->PA_DROP;
    const fm_bool            pa_l3len_err           = in->PA_L3LEN_ERR;
    const fm_byte            qos_swpri_in           = in->QOS_SWPRI;
    const fm_uint32          rx_length              = in->RX_LENGTH;
    const fm_bool            rx_mirror_in           = in->RX_MIRROR;
    const fm_uint32          rx_port                = in->RX_PORT;
    const fm_bool            sa_hit                 = in->SA_HIT;
    const mbyMaTable         sa_result              = in->SA_RESULT;
    const fm_byte            seg_meta_err_in        = in->SEG_META_ERR;
    const fm_byte            sv_drop                = in->SV_DROP;
    const fm_bool            trap_icmp              = in->TRAP_ICMP;
    const fm_bool            trap_igmp              = in->TRAP_IGMP;
    const fm_bool            trap_ip_options        = in->TRAP_IP_OPTIONS;
    const mbyTriggerResults  triggers               = in->TRIGGERS;

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
    // GLORT:

    mbyGlortRam glort_ram;
    fm_bool glort_cam_miss = FALSE;
    if (lookUpRamEntry(regs, idglort, &glort_ram) != FM_OK)
        glort_cam_miss = TRUE; // GLORT CAM miss

    // GLORT CAM hit:
    fm_bool strict_glort_routing   = (glort_ram.STRICT == 2) || (glort_ram.STRICT == 3);
    fm_bool targeted_deterministic = (glort_ram.STRICT == 2);

    fm_bool   skip_dglort_dec = FALSE;
    fm_uint32 glort_dmask     = glort_dmask_in;
    fm_uint16 ip_mcast_idx    = 0;

    if (!glort_cam_miss)
    {
        mbyGlortDestTable glort_dest_table;
        lookUpDestEntry(regs, idglort, strict_glort_routing, hash_rot_a, hash_rot_b, glort_ram, &glort_dest_table);

        skip_dglort_dec = glort_ram.SKIP_DGLORT_DEC;
        glort_dmask     = glort_dest_table.DEST_MASK;
        ip_mcast_idx    = glort_dest_table.IP_MULTICAST_INDEX;
    }

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

    fm_uint64 amask     = amask_in; // from lookUpL2() in NextHop
    fm_byte   log_amask = 0;

    if (glort_cam_miss)
        amask |= MBY_AMASK_DROP_CAM_MISS;   // CAM miss -> drop frame

    if (targeted_deterministic)
        amask |= MBY_AMASK_SPECIAL;         // frame is special delivery

    if (parser_error)
        amask |= MBY_AMASK_DROP_PARSER_ERR; // parser err -> drop frame

    if (trap_igmp)
        amask |= MBY_AMASK_TRAP_IGMP;       // trap IGMP frame

    if (parity_error)
        amask |= MBY_AMASK_DROP_PERR;       // parity err -> drop frame

    if ((sys_cfg1.DROP_MAC_CTRL_ETHERTYPE == TRUE) && (l2_etype == MBY_ETYPE_MAC_CONTROL))
        amask |= MBY_AMASK_DROP_MAC_CTRL;   // MAC CTRL Eth type 0x8808 Frame -> drop frame

    fm_bool drop_invalid_smac  = sys_cfg1.DROP_INVALID_SMAC;
    fm_bool l2_smac_is_invalid = l2_smac_is_zero && !parser_window_v;
    fm_bool l2_smac_is_broad   = isBroadcastMacAddress(l2_smac);
    fm_bool l2_smac_is_multi   = isMulticastMacAddress(l2_smac);

    if (drop_invalid_smac && (l2_smac_is_invalid || l2_smac_is_broad || l2_smac_is_multi))
        amask |= MBY_AMASK_DROP_SMAC;       // SMAC frame -> drop frame

    // --------------------------------------------------------------------------------
    // Traps:

    fm_bool store_trap_action = sys_cfg1.STORE_TRAP_ACTION;
    // TODO Variable 'log_ip_mc_ttl' is assigned a value that is never used.
    fm_bool log_ip_mc_ttl     = FALSE;
    fm_bool l2_dmac_cpu       = isCpuMacAddress(regs, l2_dmac);
    fm_bool l2_dmac_zero      = (l2_dmac == 0);

    if (l2_dmac_cpu && !(l2_dmac_zero && parser_window_v))
        amask |= MBY_AMASK_TRAP_CPU_ADDR; // Trapping CPU addressed frame

    // Special packet handling:
    if ((l2_dmac & MBY_SPECIAL_DMASK) == (MBY_DMAC_IEEE_PREFIX & MBY_SPECIAL_DMASK))
    {
        fm_uint32 fwd_rsvd_mac_action_regs[MBY_FWD_IEEE_RESERVED_MAC_ACTION_WIDTH] = { 0 };
        mbyModelReadCSRMult(regs, MBY_FWD_IEEE_RESERVED_MAC_ACTION(0), MBY_FWD_IEEE_RESERVED_MAC_ACTION_WIDTH, fwd_rsvd_mac_action_regs);

        fm_uint64 fwd_rsvd_mac_trap_pri_reg = 0;
        mbyModelReadCSR64(regs, MBY_FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY(0), &fwd_rsvd_mac_trap_pri_reg);

        fm_byte rmc_idx    = l2_dmac & 0x3F; // 6 bits (0..63)
        fm_byte mac_action = FM_ARRAY_GET_UNNAMED_FIELD(fwd_rsvd_mac_action_regs, 2 * rmc_idx, 2); // 2-bit action
        fm_bool trap_pri   = FM_GET_UNNAMED_BIT64      (fwd_rsvd_mac_trap_pri_reg, rmc_idx);

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

    if (mark_routed && (ip_mcast_idx == 0) && (l2_ivid1 == (l2_evid1 & 0xFFF)))
        log_amask |= MBY_LOG_TYPE_ARP_REDIRECT;

    if (drop_ttl)
    {
        if (ip_mcast_idx == 0)
        {
            if      (sys_cfg_router.TRAP_TTL1 == 0)
                amask |= MBY_AMASK_DROP_TTL;
            else if (sys_cfg_router.TRAP_TTL1 == 1 )
                amask |= (trap_icmp) ? MBY_AMASK_TRAP_ICMP_TTL : MBY_AMASK_DROP_TTL;
            else if (sys_cfg_router.TRAP_TTL1 == 2 )
                amask |= (trap_icmp) ? MBY_AMASK_TRAP_ICMP_TTL : MBY_AMASK_TRAP_TTL;
        }
        else // Frame is IP multicast
        {
            if ((sys_cfg_router.TRAP_TTL1 == 1) && trap_icmp) {
                log_amask |= MBY_LOG_TYPE_ICMP;
                log_ip_mc_ttl = TRUE;
            } else if (sys_cfg_router.TRAP_TTL1 == 2) {
                log_amask |= (trap_icmp) ? MBY_LOG_TYPE_ICMP : MBY_LOG_TYPE_TTL_IP_MC;
                log_ip_mc_ttl = TRUE;
            }
        }
    }

    if (sys_cfg_router.TRAP_IP_OPTIONS && trap_ip_options && (is_ipv4 || is_ipv6))
        amask |= MBY_AMASK_TRAP_IP_OPTION;

    if (sys_cfg1.TRAP_MTU_VIOLATIONS && mtu_violation && mark_routed)
        amask |= MBY_AMASK_TRAP_MTU_VIO; // MTU violation -> trapping frame

    // --------------------------------------------------------------------------------
    // Port Security:

    if (port_cfg1.FILTER_VLAN_INGRESS && !l2_ivlan1_membership)
        amask |= MBY_AMASK_DROP_IV; // VLAN ingress violation -> dropping frame

    fm_bool mac_moved = (sa_hit && (sa_result.S_GLORT != csglort));

    switch (sv_drop)
    {
        case MBY_SV_MOVE_DROP_ADDR:   // secure addr violation -> dropping frame
            amask |= MBY_AMASK_DROP_SEC_ADDR; break;
        case MBY_SV_MOVE_DROP_PORT:   // secure port violation -> dropping frame
            amask |= MBY_AMASK_DROP_SEC_PORT; break;
        case MBY_SV_MOVE_DROP_STATIC: // static addr violation -> dropping frame
            amask |= MBY_AMASK_DROP_STATIC_ADDR; break;
        default: break;
    }

    switch (l2_ifid1_state) // Ingress spanning tree check
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

    if (flood_forwarded)
        amask |= MBY_AMASK_FLOOD;

    if (glort_forwarded)
        amask |= MBY_AMASK_GLORT;

    // Perform port-based filtering for switched packets
    dmask &= port_cfg1.DESTINATION_MASK;
    dmask &= port_cfg2.DESTINATION_MASK;

    // Ingress VLAN reflection check:
    if ( !mark_routed && !l2_ivlan1_reflect && !targeted_deterministic)
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
            if (flood_forwarded) // Null Glort Dest & Flood Forwarded: dropping frame (DLF)
                amask |= MBY_AMASK_DROP_DLF;
        }
        else { // do not set amask loopback when also setting null dest.
            amask |= MBY_AMASK_DROP_LOOPBACK; // Loopback (port or VLAN refl. dis.): dropping frame
        }
    }
    else if (!targeted_deterministic)
    {
        pre_resolve_dmask &= l2_evlan1_membership; // VLAN egress filtering
        if (pre_resolve_dmask == 0)
            amask |= MBY_AMASK_DROP_EV; // VLAN egress violation: dropping frame
    }

    // --------------------------------------------------------------------------------
    // Classifier Flags:

    if (ffu_flags.drop)
        amask |= MBY_AMASK_DROP_FFU; // dropping frame

    if (ffu_flags.trap)
        amask |= MBY_AMASK_TRAP_FFU; // trapping frame

    if (ffu_flags.log)
        log_amask |= MBY_LOG_TYPE_FFU; // logging fram

    fm_bool rx_mirror           = rx_mirror_in;
    fm_bool mirror0_profile_v   = FALSE;
    fm_bool mirror1_profile_v   = FALSE;
    fm_byte mirror0_profile_idx = 0;
    fm_byte mirror1_profile_idx = 0;

    if (ffu_flags.rx_mirror)
    {
        rx_mirror         = TRUE;  // RX mirroring frame
        mirror1_profile_v = rx_mirror;

        fm_uint64 fwd_rx_mirror_cfg_reg = 0;
        mbyModelReadCSR64(regs, MBY_FWD_RX_MIRROR_CFG(0), &fwd_rx_mirror_cfg_reg);

        mirror1_profile_idx = FM_GET_FIELD64(fwd_rx_mirror_cfg_reg, MBY_FWD_RX_MIRROR_CFG, MIRROR_PROFILE_IDX);
    }

    // --------------------------------------------------------------------------------
    // QCN:

    fm_uint64 fwd_qcn_mirror_cfg_reg = 0;
    mbyModelReadCSR64(regs, MBY_FWD_QCN_MIRROR_CFG(0), &fwd_qcn_mirror_cfg_reg);

    fm_byte mirror_profile_idx = FM_GET_FIELD64(fwd_qcn_mirror_cfg_reg, MBY_FWD_QCN_MIRROR_CFG, MIRROR_PROFILE_IDX);
    fm_byte mirror_session     = FM_GET_FIELD64(fwd_qcn_mirror_cfg_reg, MBY_FWD_QCN_MIRROR_CFG, MIRROR_SESSION);

    fm_uint64 cm_mirror0_profile_reg = 0;
    fm_uint64 cm_mirror1_profile_reg = 0;

    mbyModelReadCSR64(regs, MBY_CM_APPLY_MIRROR_PROFILE_TABLE(mirror0_profile_idx, 0), &cm_mirror0_profile_reg);
    mbyModelReadCSR64(regs, MBY_CM_APPLY_MIRROR_PROFILE_TABLE(mirror1_profile_idx, 0), &cm_mirror1_profile_reg);

    fm_uint32 mirror0_port = FM_GET_FIELD64(cm_mirror0_profile_reg, MBY_CM_APPLY_MIRROR_PROFILE_TABLE, PORT);
    fm_uint32 mirror1_port = FM_GET_FIELD64(cm_mirror1_profile_reg, MBY_CM_APPLY_MIRROR_PROFILE_TABLE, PORT);

    fm_bool qcn_mirror0_profile_v   = FALSE;
    fm_bool qcn_mirror1_profile_v   = FALSE;

    if (rx_mirror && (mirror_session == 2)) {
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
        dmask &= l2_efid1_state; // STP egress filtering: DMASK
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

    fm_uint64 fwd_rsvd_mac_cfg_reg = 0;
    mbyModelReadCSR64(regs, MBY_FWD_IEEE_RESERVED_MAC_CFG(0), &fwd_rsvd_mac_cfg_reg);

    fm_byte   trap_tc   = FM_GET_FIELD64(fwd_rsvd_mac_cfg_reg, MBY_FWD_IEEE_RESERVED_MAC_CFG, TRAP_TC);
    fm_byte   cpu_code  = 0;
    fm_byte   qos_swpri = qos_swpri_in;
    fm_uint32 action    = 0;

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

    if (rx_mirror)
        amask |= MBY_AMASK_MIRROR_INGRESS_FFU;

    // --------------------------------------------------------------------------------
    // LAG:

    // GLORT Lookup - strict targeted deterministic mode:
    fm_bool skip_lookup
        = (((action == MBY_ACTION_REDIRECT_TRIG) && !triggers.filterDestMask) ||
           ((action != MBY_ACTION_REDIRECT_TRIG) &&  targeted_deterministic));

    if (!skip_lookup && (dmask != 0))
    {
        for (fm_uint i = 0; i < MBY_FABRIC_LOG_PORTS; i++)
        {
            mbyFwdLagCfg lag_cfg;
            getLagCfg(regs, i, &lag_cfg);
            if (!lag_cfg.IN_LAG)
                continue;

            fm_uint32 hash = (lag_cfg.HASH_ROTATION) ? hash_rot_b : hash_rot_a;
            hash %= (lag_cfg.LAG_SIZE == 0) ? 16 : lag_cfg.LAG_SIZE;
            if (hash != lag_cfg.INDEX)
                dmask &= ~(FM_LITERAL_U64(1) << i);
        }

        if (dmask == 0)
            action = MBY_ACTION_BANK5_OTHER_DROPS;
    }

    // --------------------------------------------------------------------------------
    // Loopback Suppression Filtering:

    fm_bool skip_suppress = targeted_deterministic || mark_routed || (dmask == 0);

    for (fm_uint i = 0; !skip_suppress && (i < MBY_FABRIC_LOG_PORTS); i++)
    {
        fm_uint64 cm_lpbk_suppress_reg = 0;
        mbyModelReadCSR64(regs, MBY_CM_APPLY_LOOPBACK_SUPPRESS(i, 0), &cm_lpbk_suppress_reg);

        fm_uint16 lpbk_glort_mask = FM_GET_FIELD64(cm_lpbk_suppress_reg, MBY_CM_APPLY_LOOPBACK_SUPPRESS, GLORT_MASK);
        fm_uint16 lpbk_glort      = FM_GET_FIELD64(cm_lpbk_suppress_reg, MBY_CM_APPLY_LOOPBACK_SUPPRESS, GLORT);

        if ((csglort & lpbk_glort_mask) == lpbk_glort)
            dmask &= ~(FM_LITERAL_U64(1) << i);
    }

    if (dmask == 0 )
        action = MBY_ACTION_DROP_LOOPBACK; // dropping frame

    // --------------------------------------------------------------------------------

    // Trap:
    fm_bool cpu_trap    = (action == MBY_ACTION_TRAP);

    fm_bool trap_trap   = (triggers.trapAction == MBY_TRIG_ACTION_TRAP_TRAP);
    fm_bool trap_revert = (triggers.trapAction == MBY_TRIG_ACTION_TRAP_REVERT);
    fm_bool trap_log    = (triggers.trapAction == MBY_TRIG_ACTION_TRAP_LOG);

    fm_uint16 l2_edomain = (triggers.egressL2DomainAction == 0) ? l2_edomain_in : 0;
    fm_byte   l3_edomain = (triggers.egressL3DomainAction == 0) ? l3_edomain_in : 0;

    if ((cpu_trap || trap_trap) && !trap_revert)
    { /* err = HandleTraps(model) */; }

    if ((sys_cfg1.ENABLE_TRAP_PLUS_LOG || !cpu_trap || trap_trap || trap_log) && !trap_revert)
    { /* err = HandleLogging(model) */; }

    // --------------------------------------------------------------------------------
    // Tail processing:

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

    fm_uint64 saf_matrix_reg = 0;
    mbyModelReadCSR64(regs, MBY_SAF_MATRIX(rx_port, 0), &saf_matrix_reg);

    fm_uint64 enable_snf    = FM_GET_FIELD64(saf_matrix_reg, MBY_SAF_MATRIX, ENABLE_SNF);
    fm_uint32 cut_thru_mode = FM_GET_FIELD64(saf_matrix_reg, MBY_SAF_MATRIX, CUT_THRU_MODE);
    fm_bool   ign_frame_err = FM_GET_BIT64  (saf_matrix_reg, MBY_SAF_MATRIX, IGNORE_FRAME_ERROR);

    dmask = fnmask; // recall stored mask <-- REVISIT!!!!

    if (mirror0_profile_v)
        dmask |= (FM_LITERAL_U64(1) << mirror0_port);

    if (mirror1_profile_v)
        dmask |= (FM_LITERAL_U64(1) << mirror1_port);

    dmask &= ( 0xFFFFFFFFFFFF ); // 48 bit port mask
    enable_snf = (enable_snf & dmask) & 0xFFFFFF;

    // Decide whether frame is SAF:
    fm_bool is_saf = FALSE;
    if (enable_snf)
        is_saf = TRUE;
    else if ((cut_thru_mode == 0) || (cut_thru_mode == 1))
        is_saf = (num_segs == 1);
    else if (cut_thru_mode == 2)
        is_saf = (num_segs <= 2);
    else
        is_saf = TRUE; // end-of-frame

    // SAF ERROR in tail processing:
    fm_bool saf_error    = FALSE;
    fm_bool tx_drop      = FALSE;
    fm_byte seg_meta_err = seg_meta_err_in;

    if ((seg_meta_err == 1) || (seg_meta_err == 2))
    {
        if (is_saf || (dmask == 0)) {
            saf_error = !ign_frame_err;
            if (!enable_snf && (cut_thru_mode == 0) && (dmask != 0))
                saf_error = FALSE;
        }
    }

    if (saf_error)
    {
        if ((num_segs == 1) || (dmask == 0)) // single-segment
        {
            action            = MBY_ACTION_DROP_FRAME_ERR;
            fnmask            = 0;
            mirror1_profile_v = 0;
            mirror0_profile_v = 0;
        }
        else // multi-segment
            tx_drop = TRUE;
    }

    // Update Action code for CSUM and L3 Length errors
    // Applies to only single segment packets. Multi-segment packets are handled by Modify
    if (rx_length <= 192)
    {
        if (action == MBY_ACTION_NORMAL ||
            action == MBY_ACTION_FLOOD ||
            action == MBY_ACTION_GLORT_FORWARDED ||
            action == MBY_ACTION_TRAP ||
            action == MBY_ACTION_SPECIAL ||
            action == MBY_ACTION_REDIRECT_TRIG ||
            action == MBY_ACTION_DROP_CONTROL ||
            action == MBY_ACTION_DROP_IV ||
            action == MBY_ACTION_DROP_EV ||
            action == MBY_ACTION_DROP_STP ||
            action == MBY_ACTION_DROP_CAM ||
            action == MBY_ACTION_DROP_FFU ||
            action == MBY_ACTION_DROP_TRIG ||
            action == MBY_ACTION_DROP_TTL ||
            action == MBY_ACTION_DROP_DLF ||
            action == MBY_ACTION_BANK5_OTHER_DROPS ||
            action == MBY_ACTION_DROP_SV)
        {
            if (pa_drop && pa_l3len_err)
                action = MBY_ACTION_DROP_L3_PYLD_LEN;
        }

        // Drop single-segment packets with l4csum error /l3 length error:
        if ((action == MBY_ACTION_DROP_L3_PYLD_LEN) || (action == MBY_ACTION_DROP_L4_CSUM))
        {
            fnmask            = 0;
            mirror1_profile_v = 0;
            mirror0_profile_v = 0;
        }
    }
    else if (pa_drop && pa_l3len_err)
        seg_meta_err = 2; // framing error in multi-segment packet

    // --------------------------------------------------------------------------------

    // Write outputs:
    out->ACTION                 = action;
    out->AMASK                  = amask;
    out->CPU_CODE               = cpu_code;
    out->CPU_TRAP               = cpu_trap;
    out->DA_HIT                 = da_hit;
    out->DMASK                  = dmask;
    out->DROP_TTL               = drop_ttl;
    out->FCLASS                 = fclass;
    out->FNMASK                 = fnmask;
    out->GLORT_CAM_MISS         = glort_cam_miss;
    out->GLORT_DMASK            = glort_dmask;
    out->IDGLORT                = idglort;
    out->IP_MCAST_IDX           = ip_mcast_idx;
    out->IS_IPV4                = is_ipv4;
    out->IS_IPV6                = is_ipv6;
    out->L2_DMAC                = l2_dmac;
    out->L2_EDOMAIN             = l2_edomain;
    out->L2_EVID1               = l2_evid1;
    out->L3_EDOMAIN             = l3_edomain;
    out->LEARNING_ENABLED       = learning_enabled;
    out->LOGGING_HIT            = logging_hit;
    out->LOG_AMASK              = log_amask;
    out->MAC_MOVED              = mac_moved;
    out->MARK_ROUTED            = mark_routed;
    out->MIRROR0_PORT           = mirror0_port;
    out->MIRROR0_PROFILE_IDX    = mirror0_profile_idx;
    out->MIRROR0_PROFILE_V      = mirror0_profile_v;
    out->MIRROR1_PORT           = mirror1_port;
    out->MIRROR1_PROFILE_IDX    = mirror1_profile_idx;
    out->MIRROR1_PROFILE_V      = mirror1_profile_v;
    out->OPERATOR_ID            = operator_id;
    out->QCN_MIRROR0_PROFILE_V  = qcn_mirror0_profile_v;
    out->QCN_MIRROR1_PROFILE_V  = qcn_mirror1_profile_v;
    out->QOS_SWPRI              = qos_swpri;
    out->RX_MIRROR              = rx_mirror;
    out->SAF_ERROR              = saf_error;
    out->SEG_META_ERR           = seg_meta_err;
    out->SKIP_DGLORT_DEC        = skip_dglort_dec;
    out->STORE_TRAP_ACTION      = store_trap_action;
    out->STRICT_GLORT_ROUTING   = strict_glort_routing;
    out->TARGETED_DETERMINISTIC = targeted_deterministic;
    out->TX_DROP                = tx_drop;
    out->XCAST                  = xcast;

    // Pass thru:
    out->ECN                    = in->ECN;
    out->EDGLORT                = in->EDGLORT;
    out->IS_TIMEOUT             = in->IS_TIMEOUT;
    out->L2_IVLAN1_CNT          = in->L2_IVLAN1_CNT;
    out->MIRTYP                 = in->MIRTYP;
    out->MOD_IDX                = in->MOD_IDX;
    out->NO_MODIFY              = in->NO_MODIFY;
    out->OOM                    = in->OOM;
    out->PARSER_INFO            = in->PARSER_INFO;
    out->PM_ERR                 = in->PM_ERR;
    out->PM_ERR_NONSOP          = in->PM_ERR_NONSOP;
    out->QOS_L3_DSCP            = in->QOS_L3_DSCP;
    out->RX_DATA                = in->RX_DATA;
    out->RX_LENGTH              = in->RX_LENGTH;
    out->RX_PORT                = in->RX_PORT;
    out->TAIL_CSUM_LEN          = in->TAIL_CSUM_LEN;
    out->TRAFFIC_CLASS          = in->TRAFFIC_CLASS;
    out->TX_TAG                 = in->TX_TAG;
}
