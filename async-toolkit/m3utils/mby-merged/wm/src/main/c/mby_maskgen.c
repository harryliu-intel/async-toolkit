/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_bitfield.h"
#include "mby_params.h"
#include "mby_common.h"
#include "mby_eth_types.h"
#include "mby_cgrp_regs.h"
#include "mby_maskgen.h"

static fm_status lookUpRamEntry
(
    mby_ppe_mst_glort_map const * const glort_map,
    fm_uint16                     const idglort,
    mbyGlortRam                 * const glort_ram
)
{
    fm_bool cam_hit = FALSE;

    // The highest numbered GLORT_CAM entry has highest precendence:
    for (fm_int i = mby_ppe_mst_glort_map_GLORT_CAM__nd - 1; i >= 0; i--)
    {
        fm_byte index = i;

        mbyGlortCam glort_cam;
        glort_cam = getGlortCamEntry(glort_map, index);

        fm_uint16 mask    = glort_cam.KEY ^ glort_cam.KEY_INVERT;
        fm_uint16 key     = glort_cam.KEY;
        fm_uint16 key_inv = glort_cam.KEY_INVERT;

        if (((key & key_inv) == 0) && ((idglort & mask) == (key & mask)))
        {
            *glort_ram = getGlortRamEntry(glort_map, index);
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
    mby_ppe_mst_glort_map const * const glort_map,
    fm_uint16                     const idglort,
    fm_bool                       const strict_glort_routing,
    fm_uint32                     const hash_rot_a,
    fm_uint32                     const hash_rot_b,
    mbyGlortRam                   const glort_ram,
    mbyGlortDestTable           * const glort_dest_table
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
    *glort_dest_table = getGlortDestTableEntry(glort_map, dest_index);
}

static fm_bool isCpuMacAddress
(
    mby_ppe_fwd_misc_map const * const fwd_misc,
    fm_macaddr                   const mac_addr
)
{
    fm_macaddr cpu_mac_addr = 0;

    fwd_cpu_mac_r const * const cpu_mac = &(fwd_misc->FWD_CPU_MAC);

    cpu_mac_addr = cpu_mac->MAC_ADDR;

    fm_bool result = (mac_addr == cpu_mac_addr);
    return result;
}

static void dmaskClear
(
    fm_uint64 * const dmask
)
{
    for(fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
        dmask[i] = 0;
}

static void dmaskCopy
(
    fm_uint64 * const dmask_in,
    fm_uint64 * const dmask_out
)
{
    for(fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
        dmask_out[i] = dmask_in[i];
}

static fm_bool isDmask
(
    fm_uint64 * const dmask
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

static void resolveAction
(
    // inputs:
    fm_uint64   const pri_amask,
    fm_uint64 * const glort_dmask,
    fm_byte     const trap_tc,
    // inouts:
    fm_uint64 * const amask,
    fm_uint64 * const dmask,
    fm_bool   * const learning_enabled,
    fm_byte   * const cpu_code,
    fm_byte   * const qos_tc,
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
            dmaskClear(dmask);
            *action           = MBY_ACTION_DROP_PARITY;
            *learning_enabled = FALSE;
            break;

        case MBY_AMASK_SPECIAL:
            dmaskCopy(glort_dmask, dmask);
            *action           = MBY_ACTION_SPECIAL;
            *learning_enabled = FALSE;
            break;

        case MBY_AMASK_DROP_PARSER_ERR:
            dmaskClear(dmask);
            *action           = MBY_ACTION_DROP_PARSE;
            *learning_enabled = FALSE;
            break;

        case MBY_AMASK_TRAP_RESERVED_MAC:
            dmaskClear(dmask);
            *action           = MBY_ACTION_TRAP;
            *cpu_code         = MBY_CPU_CODE_RSVD_MAC;
            break;

        case MBY_AMASK_TRAP_RESERVED_MAC_REMAP:
            dmaskClear(dmask);
            *action           = MBY_ACTION_TRAP;
            *cpu_code         = MBY_CPU_CODE_RSVD_MAC;
            *qos_tc           = trap_tc; // <-- REVISIT!!!
            break;

        case MBY_AMASK_DROP_MAC_CTRL:
            dmaskClear(dmask);
            *action           = MBY_ACTION_DROP_CONTROL;
            *learning_enabled = FALSE;
            break;

        case MBY_AMASK_DROP_RESERVED_MAC:
        case MBY_AMASK_DROP_SMAC:
            dmaskClear(dmask);
            *action           = MBY_ACTION_BANK5_OTHER_DROPS;
            *learning_enabled = FALSE;
            break;
        //In MBY Functional Specification Reserved <- REVISIT !!!!
        /*case MBY_AMASK_DROP_PROVISIONAL:
            dmaskClear(dmask);
            *action           = MBY_ACTION_BANK5_OTHER_DROPS;
            *learning_enabled = FALSE;
            break;

        case MBY_AMASK_DROP_SEC_ADDR:
        case MBY_AMASK_DROP_SEC_PORT:
        case MBY_AMASK_DROP_STATIC_ADDR:
            dmaskClear(dmask);
            *action           = MBY_ACTION_DROP_SV;
            break;
        */
        case MBY_AMASK_TRAP_CPU_ADDR:
            dmaskClear(dmask);
            *action           = MBY_ACTION_TRAP;
            *cpu_code         = MBY_CPU_CODE_CPU_ADDRESS;
            break;

        case MBY_AMASK_DROP_IV:
            dmaskClear(dmask);
            *action           = MBY_ACTION_DROP_IV;
            *learning_enabled = FALSE;
            break;

        case MBY_AMASK_DROP_INGRESS_STP_NON_LEARN:
            dmaskClear(dmask);
            *action           = MBY_ACTION_DROP_STP;
            *learning_enabled = FALSE;
            break;

        case MBY_AMASK_DROP_INGRESS_STP_LEARN:
            dmaskClear(dmask);
            *action           = MBY_ACTION_DROP_STP;
            break;

        case MBY_AMASK_DROP_CGRP:
            dmaskClear(dmask);
            *action           = MBY_ACTION_DROP_CGRP;
            *learning_enabled = FALSE;
            break;

        case MBY_AMASK_TRAP_CGRP:
            dmaskClear(dmask);
            *action           = MBY_ACTION_TRAP;
            *cpu_code         = MBY_CPU_CODE_CGRP;
            break;

        case MBY_AMASK_TRAP_ICMP_TTL:
            dmaskClear(dmask);
            *action           = MBY_ACTION_TRAP;
            *cpu_code         = MBY_CPU_CODE_ICMP;
            break;

        case MBY_AMASK_TRAP_IP_OPTION:
            dmaskClear(dmask);
            *action           = MBY_ACTION_TRAP;
            *cpu_code         = MBY_CPU_CODE_IP_OPTION;
            break;

        case MBY_AMASK_TRAP_MTU_VIO:
            dmaskClear(dmask);
            *action           = MBY_ACTION_TRAP;
            *cpu_code         = MBY_CPU_CODE_MTU;
            break;

        case MBY_AMASK_TRAP_IGMP:
            dmaskClear(dmask);
            *action           = MBY_ACTION_TRAP;
            *cpu_code         = MBY_CPU_CODE_IGMP;
            break;

        case MBY_AMASK_TRAP_TTL:
            dmaskClear(dmask);
            *action            = MBY_ACTION_TRAP;
            *cpu_code          = MBY_CPU_CODE_TTL;
            break;

        case MBY_AMASK_DROP_TTL:
            dmaskClear(dmask);
            *action            = MBY_ACTION_DROP_TTL;
            break;

        case MBY_AMASK_DROP_NULL_GLORTDEST:
            dmaskClear(dmask);
            *action            = MBY_ACTION_BANK5_OTHER_DROPS;
            break;

        case MBY_AMASK_DROP_EV:
            dmaskClear(dmask);
            *action            = MBY_ACTION_DROP_EV;
            break;

        case MBY_AMASK_DROP_DLF:
            dmaskClear(dmask);
            *action           = MBY_ACTION_DROP_DLF;
            *learning_enabled = FALSE;
            break;

        case MBY_AMASK_DROP_CAM_MISS:
            dmaskClear(dmask);
            *action           = MBY_ACTION_DROP_CAM;
            *learning_enabled = 0;
            break;

        case MBY_AMASK_DROP_EGRESS_STP:
            dmaskClear(dmask);
            *action           = MBY_ACTION_DROP_STP;
            break;

        case MBY_AMASK_DROP_LOOPBACK:
            dmaskClear(dmask);
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
            dmaskClear(dmask);
            *action           = MBY_ACTION_BANK5_OTHER_DROPS;
            break;
    }
}

static fm_byte getIfid1
(
    fm_byte                           rx_port,
    ingress_mst_table_r const * const ingress_mst_table
)
{
    switch (rx_port)
    {
        case 0:  return ingress_mst_table->STP_STATE_0;
        case 1:  return ingress_mst_table->STP_STATE_1;
        case 2:  return ingress_mst_table->STP_STATE_2;
        case 3:  return ingress_mst_table->STP_STATE_3;
        case 4:  return ingress_mst_table->STP_STATE_4;
        case 5:  return ingress_mst_table->STP_STATE_5;
        case 6:  return ingress_mst_table->STP_STATE_6;
        case 7:  return ingress_mst_table->STP_STATE_7;
        case 8:  return ingress_mst_table->STP_STATE_8;
        case 9:  return ingress_mst_table->STP_STATE_9;
        case 10: return ingress_mst_table->STP_STATE_10;
        case 11: return ingress_mst_table->STP_STATE_11;
        case 12: return ingress_mst_table->STP_STATE_12;
        case 13: return ingress_mst_table->STP_STATE_13;
        case 14: return ingress_mst_table->STP_STATE_14;
        case 15: return ingress_mst_table->STP_STATE_15;
        case 16: return ingress_mst_table->STP_STATE_16;
        case 17: return ingress_mst_table->STP_STATE_17;
        default: return ingress_mst_table->STP_STATE_0;
    }
}

static fm_byte getMacAction
(
    fm_byte                                      action_index,
    fwd_ieee_reserved_mac_action_r const * const mac_action
)
{
    switch (action_index)
    {
        case 0:  return mac_action->ACTION_0;
        case 1:  return mac_action->ACTION_1;
        case 2:  return mac_action->ACTION_2;
        case 3:  return mac_action->ACTION_3;
        case 4:  return mac_action->ACTION_4;
        case 5:  return mac_action->ACTION_5;
        case 6:  return mac_action->ACTION_6;
        case 7:  return mac_action->ACTION_7;
        case 8:  return mac_action->ACTION_8;
        case 9:  return mac_action->ACTION_9;
        case 10: return mac_action->ACTION_10;
        case 11: return mac_action->ACTION_11;
        case 12: return mac_action->ACTION_12;
        case 13: return mac_action->ACTION_13;
        case 14: return mac_action->ACTION_14;
        case 15: return mac_action->ACTION_15;
        case 16: return mac_action->ACTION_16;
        case 17: return mac_action->ACTION_17;
        case 18: return mac_action->ACTION_18;
        case 19: return mac_action->ACTION_19;
        case 20: return mac_action->ACTION_20;
        case 21: return mac_action->ACTION_21;
        case 22: return mac_action->ACTION_22;
        case 23: return mac_action->ACTION_23;
        case 24: return mac_action->ACTION_24;
        case 25: return mac_action->ACTION_25;
        case 26: return mac_action->ACTION_26;
        case 27: return mac_action->ACTION_27;
        case 28: return mac_action->ACTION_28;
        case 29: return mac_action->ACTION_29;
        case 30: return mac_action->ACTION_30;
        case 31: return mac_action->ACTION_31;
        case 32: return mac_action->ACTION_32;
        case 33: return mac_action->ACTION_33;
        case 34: return mac_action->ACTION_34;
        case 35: return mac_action->ACTION_35;
        case 36: return mac_action->ACTION_36;
        case 37: return mac_action->ACTION_37;
        case 38: return mac_action->ACTION_38;
        case 39: return mac_action->ACTION_39;
        case 40: return mac_action->ACTION_40;
        case 41: return mac_action->ACTION_41;
        case 42: return mac_action->ACTION_42;
        case 43: return mac_action->ACTION_43;
        case 44: return mac_action->ACTION_44;
        case 45: return mac_action->ACTION_45;
        case 46: return mac_action->ACTION_46;
        case 47: return mac_action->ACTION_47;
        case 48: return mac_action->ACTION_48;
        case 49: return mac_action->ACTION_49;
        case 50: return mac_action->ACTION_50;
        case 51: return mac_action->ACTION_51;
        case 52: return mac_action->ACTION_52;
        case 53: return mac_action->ACTION_53;
        case 54: return mac_action->ACTION_54;
        case 55: return mac_action->ACTION_55;
        case 56: return mac_action->ACTION_56;
        case 57: return mac_action->ACTION_57;
        case 58: return mac_action->ACTION_58;
        case 59: return mac_action->ACTION_59;
        case 60: return mac_action->ACTION_60;
        case 61: return mac_action->ACTION_61;
        case 62: return mac_action->ACTION_62;
        case 63: return mac_action->ACTION_63;
        default: return MBY_IEEE_RESERVED_MAC_ACTION_ACTION_DROP;
    }
}

static void glortLookup
(
    mby_ppe_mst_glort_map const * const glort_map,
    fm_uint16                     const idglort,
    fm_uint32                     const hash_rot_a,
    fm_uint32                     const hash_rot_b,
    fm_uint64             const * const glort_dmask_in,
    fm_bool                     * const glort_cam_miss_o,
    fm_bool                     * const strict_glort_routing_o,
    fm_bool                     * const targeted_deterministic_o,
    fm_bool                     * const skip_dglort_dec_o,
    fm_uint64                   * const glort_dmask_o,
    fm_uint16                   * const ip_mcast_idx_o,
    mbyGlortDestTable           * const glort_dest_table_o
)
{
    mbyGlortRam glort_ram;
    fm_bool glort_cam_miss = FALSE;

    if (lookUpRamEntry(glort_map, idglort, &glort_ram) != FM_OK)
        glort_cam_miss = TRUE; // GLORT CAM miss

    // GLORT CAM hit:
    fm_bool strict_glort_routing   = (glort_ram.STRICT == 2) || (glort_ram.STRICT == 3);
    fm_bool targeted_deterministic = (glort_ram.STRICT == 2);

    fm_bool skip_dglort_dec = FALSE;

    fm_uint16 ip_mcast_idx    = 0;
    for(fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
        glort_dmask_o[i] = glort_dmask_in[i];

    if (!glort_cam_miss)
    {
        lookUpDestEntry(glort_map, idglort, strict_glort_routing, hash_rot_a, hash_rot_b, glort_ram, glort_dest_table_o);

        skip_dglort_dec = glort_ram.SKIP_DGLORT_DEC;
        ip_mcast_idx    = glort_dest_table_o->IP_MULTICAST_INDEX;
        dmaskCopy(glort_dest_table_o->DEST_MASK, glort_dmask_o);
    }

    // output values
    *glort_cam_miss_o         = glort_cam_miss;
    *strict_glort_routing_o   = strict_glort_routing;
    *targeted_deterministic_o = targeted_deterministic;
    *skip_dglort_dec_o        = skip_dglort_dec;
    *ip_mcast_idx_o           = ip_mcast_idx;
}


static void learningEnabledCheck
(
    mby_ppe_mst_glort_map const * const glort_map,
    mby_ppe_fwd_misc_map  const * const fwd_misc,
    mbyFwdPortCfg         const * const port_cfg,
    fm_uint32                     const rx_port,
    fm_uint16                     const l2_ivid1,
    fm_bool                       const learn_notify,
    fm_macaddr                    const l2_smac,
    fm_bool                     * const l2_smac_is_zero_o,
    fm_bool                     * const learning_enabled_o,
    mbyStpState                 * const l2_ifid1_state_o
)
{
    // Learning:
    ///> Learning might need to be moved to Triggers <-- REVISIT!!!

    /* Perform ingress forwarding ID lookup. */
    mbyStpState l2_ifid1_state = MBY_STP_STATE_DISABLE;
    ingress_mst_table_r const * const ingress_mst_table = &(glort_map->INGRESS_MST_TABLE[l2_ivid1]);
    l2_ifid1_state = getIfid1(rx_port, ingress_mst_table);

    fm_bool l2_ifid1_learn   = ((l2_ifid1_state == MBY_STP_STATE_LEARNING) ||
                                (l2_ifid1_state == MBY_STP_STATE_FORWARD));

    // If a frame is received with an SMAC that matches the value in CPU_MAC, there will be no SMAC learning notification.
    fm_bool learning_allowed = port_cfg->LEARNING_ENABLE && !learn_notify && l2_ifid1_learn;
    fm_bool l2_smac_is_cpu   = isCpuMacAddress(fwd_misc, l2_smac);
    fm_bool l2_smac_is_zero  = (l2_smac == 0);
    fm_bool learning_enabled = learning_allowed && !l2_smac_is_cpu && !l2_smac_is_zero;

    // output values
    *l2_smac_is_zero_o  = l2_smac_is_zero;
    *learning_enabled_o = learning_enabled;
    *l2_ifid1_state_o   = l2_ifid1_state;
}

static void actionMaskUpdate
(
    mbyFwdSysCfg1 const * const sys_cfg1,
    fm_uint64             const amask_in,
    fm_bool               const parser_error,
    fm_bool               const trap_igmp,
    fm_bool               const parity_error,
    fm_uint16             const l2_etype,
    fm_macaddr            const l2_smac,
    fm_bool               const parser_window_v,
    fm_bool               const glort_cam_miss,
    fm_bool               const targeted_deterministic,
    fm_bool               const l2_smac_is_zero,
    fm_uint64           * const amask_o
)
{
    fm_uint64 amask     = amask_in; // from lookUpL2() in NextHop

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

    if ((sys_cfg1->DROP_MAC_CTRL_ETHERTYPE == TRUE) && (l2_etype == MBY_ETYPE_MAC_CONTROL))
        amask |= MBY_AMASK_DROP_MAC_CTRL;   // MAC CTRL Eth type 0x8808 Frame -> drop frame

    fm_bool drop_invalid_smac  = sys_cfg1->DROP_INVALID_SMAC;
    fm_bool l2_smac_is_invalid = l2_smac_is_zero && !parser_window_v;
    fm_bool l2_smac_is_broad   = isBroadcastMacAddress(l2_smac);
    fm_bool l2_smac_is_multi   = isMulticastMacAddress(l2_smac);

    if (drop_invalid_smac && (l2_smac_is_invalid || l2_smac_is_broad || l2_smac_is_multi))
        amask |= MBY_AMASK_DROP_SMAC;       // SMAC frame -> drop frame

    // output values
    *amask_o = amask;
}

static void specialPacketHandlingTtlTrapDropLog
(
    mby_ppe_fwd_misc_map const * const fwd_misc,
    mbyFwdSysCfg1        const * const sys_cfg1,
    mbyFwdSysCfgRouter   const * const sys_cfg_router,
    fm_macaddr                   const l2_dmac,
    fm_bool                      const parser_window_v,
    fm_bool                      const drop_ttl,
    fm_bool                      const trap_icmp,
    fm_uint16                    const ip_mcast_idx,
    fm_uint16                    const l2_ivid1,
    fm_uint16                    const l2_evid1,
    fm_bool                      const routed,
    fm_bool                      const trap_ip_options,
    fm_bool                      const is_ipv4,
    fm_bool                      const is_ipv6,
    fm_bool                      const mtu_violation,
    fm_uint64                  * const amask_o,
    fm_byte                    * const log_amask_o,
    fm_bool                    * const store_trap_action_o
)
{
    fm_byte log_amask         = 0;
    fm_bool store_trap_action = sys_cfg1->STORE_TRAP_ACTION;
    // TODO Variable 'log_ip_mc_ttl' is assigned a value that is never used.
    fm_bool log_ip_mc_ttl     = FALSE;
    fm_bool l2_dmac_cpu       = isCpuMacAddress(fwd_misc, l2_dmac);
    fm_bool l2_dmac_zero      = (l2_dmac == 0);

    if (l2_dmac_cpu && !(l2_dmac_zero && parser_window_v))
        *amask_o |= MBY_AMASK_TRAP_CPU_ADDR; // Trapping CPU addressed frame

    // Special packet handling:
    if ((l2_dmac & MBY_SPECIAL_DMASK) == (MBY_DMAC_IEEE_PREFIX & MBY_SPECIAL_DMASK))
    {
        fm_byte rmc_idx = l2_dmac & 0x3F; // 6 bits (0..63)
        fm_byte mac_action;
        fm_bool trap_pri;

        fwd_ieee_reserved_mac_action_r const * const reserved_mac_action = &(fwd_misc->FWD_IEEE_RESERVED_MAC_ACTION);
        mac_action = getMacAction(rmc_idx, reserved_mac_action);

        fwd_ieee_reserved_mac_trap_priority_r const * const mac_trap_pri = &(fwd_misc->FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY);
        trap_pri = FM_GET_UNNAMED_BIT64(mac_trap_pri->SELECT, rmc_idx);

        switch (mac_action)
        {
            case MBY_IEEE_RESERVED_MAC_ACTION_ACTION_SWITCHNORMALLY:
                *amask_o |= MBY_AMASK_SWITCH_RESERVED_MAC;
                break;

            case MBY_IEEE_RESERVED_MAC_ACTION_ACTION_TRAP:
                *amask_o |= (trap_pri) ? MBY_AMASK_TRAP_RESERVED_MAC_REMAP : MBY_AMASK_TRAP_RESERVED_MAC;
                break;

            case MBY_IEEE_RESERVED_MAC_ACTION_ACTION_DROP:
                *amask_o |= MBY_AMASK_DROP_RESERVED_MAC;
                break;

            default:
            case MBY_IEEE_RESERVED_MAC_ACTION_ACTION_LOG:
                *amask_o  |= MBY_AMASK_LOG_RESERVED_MAC;
                log_amask |= MBY_LOG_TYPE_RESERVED_MAC;
                break;
        }
    }

    if (routed && (ip_mcast_idx == 0) && (l2_ivid1 == (l2_evid1 & 0xFFF)))
        log_amask |= MBY_LOG_TYPE_ARP_REDIRECT;

    if (drop_ttl)
    {
        if (ip_mcast_idx == 0)
        {
            if (sys_cfg_router->TRAP_TTL1 == 0)
                *amask_o |= MBY_AMASK_DROP_TTL;
            else if (sys_cfg_router->TRAP_TTL1 == 1 )
                *amask_o |= (trap_icmp) ? MBY_AMASK_TRAP_ICMP_TTL : MBY_AMASK_DROP_TTL;
            else if (sys_cfg_router->TRAP_TTL1 == 2 )
                *amask_o |= (trap_icmp) ? MBY_AMASK_TRAP_ICMP_TTL : MBY_AMASK_TRAP_TTL;
        }
        else // Frame is IP multicast
        {
            if ((sys_cfg_router->TRAP_TTL1 == 1) && trap_icmp) {
                log_amask |= MBY_LOG_TYPE_ICMP;
                log_ip_mc_ttl = TRUE;
            } else if (sys_cfg_router->TRAP_TTL1 == 2) {
                log_amask |= (trap_icmp) ? MBY_LOG_TYPE_ICMP : MBY_LOG_TYPE_TTL_IP_MC;
                log_ip_mc_ttl = TRUE;
            }
        }
    }

    if (sys_cfg_router->TRAP_IP_OPTIONS && trap_ip_options && (is_ipv4 || is_ipv6))
        *amask_o |= MBY_AMASK_TRAP_IP_OPTION;

    if (sys_cfg1->TRAP_MTU_VIOLATIONS && mtu_violation && routed)
        *amask_o |= MBY_AMASK_TRAP_MTU_VIO; // MTU violation -> trapping frame

    // output values
    *log_amask_o         = log_amask;
    *store_trap_action_o = store_trap_action;
}

static void ingressSpanningTreeCheck
(
    mbyFwdPortCfg      const * const port_cfg,
    fm_bool                    const l2_ivlan1_membership,
    fm_bool                    const sa_hit,
    fm_uint16                  const csglort,
    fm_byte                    const sv_drop,
    mbyStpState                const l2_ifid1_state,
    mbyMaTable                 const sa_result,
    fm_uint64                * const amask,
    fm_bool                  * const mac_moved_o
)
{
    if (port_cfg->FILTER_VLAN_INGRESS && !l2_ivlan1_membership)
        *amask |= MBY_AMASK_DROP_IV; // VLAN ingress violation -> dropping frame

    fm_bool mac_moved = (sa_hit && (sa_result.S_GLORT != csglort));

    //In MBY Functional Specification Reserved <- REVISIT !!!!
    /*switch (sv_drop)
    {
        case MBY_SV_MOVE_DROP_ADDR:   // secure addr violation -> dropping frame
            *amask |= MBY_AMASK_DROP_SEC_ADDR; break;
        case MBY_SV_MOVE_DROP_PORT:   // secure port violation -> dropping frame
            *amask |= MBY_AMASK_DROP_SEC_PORT; break;
        case MBY_SV_MOVE_DROP_STATIC: // static addr violation -> dropping frame
            *amask |= MBY_AMASK_DROP_STATIC_ADDR; break;
        default: break;
    }*/

    switch (l2_ifid1_state) // Ingress spanning tree check
    {
        case MBY_STP_STATE_DISABLE:
        case MBY_STP_STATE_LISTENING: // STP ingress (non-learning) violation -> dropping frame
            *amask |= MBY_AMASK_DROP_INGRESS_STP_NON_LEARN;
            break;
        case MBY_STP_STATE_LEARNING:  // STP ingress (learning) violation -> dropping frame
            *amask |= MBY_AMASK_DROP_INGRESS_STP_LEARN;
            break;
        default: break;
    }

    // output values
    *mac_moved_o = mac_moved;

}

static void filtering
(
    mby_ppe_mst_glort_map const * const glort_map,
    mbyFwdPortCfg         const * const port_cfg,
    mbyFwdPortCfg2        const * const port_cfg2,
    fm_macaddr                    const l2_dmac,
    fm_bool                       const flood_forwarded,
    fm_bool                       const glort_forwarded,
    fm_bool                       const targeted_deterministic,
    fm_bool                       const routed,
    fm_bool                       const l2_ivlan1_reflect,
    fm_uint64                   * const glort_dmask,
    fm_uint32                     const rx_port,
    fm_uint16                     const l2_evid1,
    fm_uint64                   * const amask_o,
    fm_uint64                   * const pre_resolve_dmask_o,
    fm_uint64                   * const dmask_o,
    fm_byte                     * const fclass_o,
    fm_byte                     * const xcast_o
)
{
    fm_bool l2_dmac_is_broad = isBroadcastMacAddress(l2_dmac);
    fm_bool l2_dmac_is_multi = isMulticastMacAddress(l2_dmac);
    fm_bool l2_dmac_is_uni   = isUnicastMacAddress  (l2_dmac);

    fm_byte fclass               = 0;
    fm_byte xcast                = 0;
    fm_bool is_pre_resolve_dmask = FALSE;
    fm_bool is_dmask             = FALSE;
    fm_bool is_glort_dmask       = FALSE;

    if (l2_dmac_is_broad) {
        fclass = MBY_FCLASS_BROADCAST;
        xcast  = 2;
    } else if (l2_dmac_is_multi) {
        fclass = MBY_FCLASS_MULTICAST;
        xcast  = 1;
    } else if (l2_dmac_is_uni) {
        fclass = MBY_FCLASS_UNICAST;
        xcast  = 0;
    }

    if (flood_forwarded)
        *amask_o |= MBY_AMASK_FLOOD;

    if (glort_forwarded)
        *amask_o |= MBY_AMASK_GLORT;

    // Perform port-based filtering for switched packets
    for(fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
    {
        dmask_o[i] &= port_cfg->DESTINATION_MASK[i];
        dmask_o[0] &= port_cfg2->DESTINATION_MASK; //!!!REVISIT RDL changes needed here
    }

    // Ingress VLAN reflection check:
    if ( !routed && !l2_ivlan1_reflect && !targeted_deterministic)
        dmask_o[rx_port / 64] &= ~(FM_LITERAL_U64(1) << (rx_port % 64)); //!!!REVISIT replace rx_port with the port specified by ([0..7][0..1][0..15])+1CPU

    // Save pre-resolve dmask for later:
    for(fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
        pre_resolve_dmask_o[i] = dmask_o[i] & glort_dmask[i];

    is_pre_resolve_dmask = isDmask(pre_resolve_dmask_o);
    is_dmask             = isDmask(dmask_o);
    is_glort_dmask       = isDmask(glort_dmask);

    // Prevent reflection: drop frame
    if (!is_dmask)
        *amask_o |= MBY_AMASK_DROP_LOOPBACK;

    if (!is_pre_resolve_dmask)
    {
        // Egress VLAN membership check:
        if (!is_glort_dmask) { // Null Glort Dest: dropping frame
            *amask_o |= MBY_AMASK_DROP_NULL_GLORTDEST;
            if (flood_forwarded) // Null Glort Dest & Flood Forwarded: dropping frame (DLF)
                *amask_o |= MBY_AMASK_DROP_DLF;
        }
        else { // do not set amask loopback when also setting null dest.
            *amask_o |= MBY_AMASK_DROP_LOOPBACK; // Loopback (port or VLAN refl. dis.): dropping frame
        }
    }
    else if (!targeted_deterministic)
    {
        mbyEgressVidTableCfg evidTable = getEvidTableCfgEntry(glort_map, l2_evid1);
        for(fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
            pre_resolve_dmask_o[i] &= evidTable.MEMBERSHIP[i]; // VLAN egress filtering
        if (!is_pre_resolve_dmask)
            *amask_o |= MBY_AMASK_DROP_EV; // VLAN egress violation: dropping frame
    }

    // output values
    *fclass_o = fclass;
    *xcast_o  = xcast;
}

static void classifierFlags
(
    mby_ppe_fwd_misc_map const * const fwd_misc,
    mbyClassifierFlags   const * const cgrp_flags,
    fm_uint64                  * const amask,
    fm_byte                    * const log_amask,
    fm_bool                      const rx_mirror_in,
    fm_bool                    * const rx_mirror_o,
    fm_bool                    * const mirror1_profile_v_o,
    fm_byte                    * const mirror1_profile_idx_o
)
{
    if (cgrp_flags->drop)
        *amask |= MBY_AMASK_DROP_CGRP; // dropping frame

    if (cgrp_flags->trap)
        *amask |= MBY_AMASK_TRAP_CGRP; // trapping frame

    if (cgrp_flags->log)
        *log_amask |= MBY_LOG_TYPE_CGRP; // logging frame

    fm_bool rx_mirror           = rx_mirror_in;
    fm_bool mirror1_profile_v   = FALSE;
    fm_byte mirror1_profile_idx = 0;

    if (cgrp_flags->rx_mirror)
    {
        rx_mirror           = TRUE;  // RX mirroring frame
        mirror1_profile_v   = rx_mirror;
        mirror1_profile_idx = fwd_misc->FWD_RX_MIRROR_CFG.MIRROR_PROFILE_IDX;
    }

    // output values
    *rx_mirror_o           = rx_mirror;
    *mirror1_profile_v_o   = mirror1_profile_v;
    *mirror1_profile_idx_o = mirror1_profile_idx;
}

static void qcnMirroring
(
    mby_ppe_fwd_misc_map const * const fwd_misc,
    mby_ppe_cm_apply_map const * const cm_apply,
    fm_bool                      const rx_mirror,
    fm_bool                    * const mcst_epoch_o,
    fm_byte                    * const mirror0_profile_idx_o,
    fm_bool                    * const mirror0_profile_v_o,
    fm_byte                    * const mirror1_profile_idx_o,
    fm_bool                    * const mirror1_profile_v_o,
    fm_uint32                  * const mirror0_port_o,
    fm_uint32                  * const mirror1_port_o,
    fm_bool                    * const qcn_mirror0_profile_v_o,
    fm_bool                    * const qcn_mirror1_profile_v_o
)
{
    fwd_qcn_mirror_cfg_r const * const qcn_mirror_cfg = &(fwd_misc->FWD_QCN_MIRROR_CFG);

    fm_byte mirror_profile_idx = qcn_mirror_cfg->MIRROR_PROFILE_IDX;
    fm_byte mirror_session     = qcn_mirror_cfg->MIRROR_SESSION;

    //!!!REVISIT How should I get the port from DMASK???
    mbyMirrorEcmpDmask mirror_ecmp_dmask_0;
    mirror_ecmp_dmask_0 = getMirrorEcmpDmask(cm_apply, *mirror0_profile_idx_o);

    mbyMirrorEcmpDmask mirror_ecmp_dmask_1;
    mirror_ecmp_dmask_1 = getMirrorEcmpDmask(cm_apply, *mirror1_profile_idx_o);

    fm_uint32 mirror0_port = mirror_ecmp_dmask_0.Mirror_port_mask[0];  //!!!REVISIT How should I get the port from DMASK???
    fm_uint32 mirror1_port = mirror_ecmp_dmask_1.Mirror_port_mask[0];  //!!!REVISIT How should I get the port from DMASK???

    cm_apply_mcast_epoch_r const * const mcast_epoch = &(cm_apply->CM_APPLY_MCAST_EPOCH);
    fm_bool mcst_epoch = mcast_epoch->CURRENT;

    fm_bool mirror0_profile_v     = FALSE;
    fm_byte mirror0_profile_idx   = 0;
    fm_bool qcn_mirror0_profile_v = FALSE;
    fm_bool qcn_mirror1_profile_v = FALSE;

    if (rx_mirror && (mirror_session == 2)) {
        qcn_mirror1_profile_v  = TRUE;
        *mirror1_profile_v_o   = (mirror0_port < MBY_PORTS_COUNT); //!!!REVISIT Probably should be 257 after change mirror_ecmp_dmask0..4 RDL changes needed
        *mirror1_profile_idx_o = mirror_profile_idx;
    }

    if (mirror_session == 1) {
        qcn_mirror0_profile_v  = TRUE;
        mirror0_profile_v      = (mirror1_port < MBY_PORTS_COUNT); //!!!REVISIT Probably should be 257 after change mirror_ecmp_dmask0..4 RDL changes needed
        mirror0_profile_idx    = mirror_profile_idx;
    }

    // output values
    *mcst_epoch_o            = mcst_epoch;
    *mirror0_port_o          = mirror0_port;
    *mirror1_port_o          = mirror1_port;
    *mirror0_profile_v_o     = mirror0_profile_v;
    *mirror0_profile_idx_o   = mirror0_profile_idx;
    *qcn_mirror0_profile_v_o = qcn_mirror0_profile_v;
    *qcn_mirror1_profile_v_o = qcn_mirror1_profile_v;
}

static void egressSTP
(
    mby_ppe_mst_glort_map const * const glort_map,
    fm_uint64                   * const amask,
    fm_uint16                     const l2_evid1,
    fm_uint64                   * const pre_resolve_dmask,
    fm_uint64                   * const dmask_o
)
{
    fm_bool egress_stp         = FALSE;
    fm_bool egress_stp_changed = FALSE;

    /* Perform egress forwarding ID lookup. */
    for(fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
    {
        dmask_o[i] = pre_resolve_dmask[i]; // 257-bit destination mask
        fm_uint64 l2_efid1_state = glort_map->EGRESS_MST_TABLE[l2_evid1][i].FORWARDING;
        if (((*amask & MBY_AMASK_SPECIAL) == 0) && (dmask_o[i] != 0))
        {
            if(!egress_stp_changed)
            {
                egress_stp         = TRUE;
                egress_stp_changed = TRUE;
            }
            dmask_o[i] &= l2_efid1_state; // STP egress filtering: DMASK
            if(dmask_o[i] != 0 && egress_stp)
                egress_stp = FALSE;
        }
    }

    if (egress_stp)
        *amask |= MBY_AMASK_DROP_EGRESS_STP; // STP egress violation
}

static void preResolveActionDmaskDglort
(
    fm_uint64   const amask,
    fm_uint64 * const glort_dmask,
    fm_uint16   const idglort,
    fm_uint64 * const dmask_o,
    fm_uint64 * const pre_resolve_dmask_o,
    fm_uint16 * const pre_resolve_dglort_o,
    fm_uint32 * const pre_resolve_action_o
)
{
    fm_uint32 pre_resolve_action = 0; // How does this affect 'action'? <-- REVISIT!!!

    if (amask & MBY_AMASK_SPECIAL) {
        pre_resolve_action = MBY_ACTION_SPECIAL;
        dmaskCopy(glort_dmask, pre_resolve_dmask_o);
        dmaskCopy(glort_dmask, dmask_o);
    }
    else if (amask & MBY_AMASK_FLOOD)
        pre_resolve_action = MBY_ACTION_FLOOD;
    else
        pre_resolve_action = MBY_ACTION_NORMAL;

    // Output values
    *pre_resolve_dglort_o = idglort;
    *pre_resolve_action_o = pre_resolve_action;
}

static void actionResolution
(
    mby_ppe_fwd_misc_map const * const fwd_misc,
    fm_uint64                  * const amask,
    fm_byte                      const qos_tc_in,
    fm_uint64                  * const glort_dmask,
    fm_bool                    * const learning_enabled,
    fm_uint64                  * const dmask_o,
    fm_byte                    * const qos_tc_o,
    fm_uint32                  * const action_o,
    fm_byte                    * const cpu_code_o
)
{
    fm_uint64 pri_amask = 0; // bitmask representing the index of the highest priority bit
    for (fm_uint i = 0; i < MBY_AMASK_WIDTH; i++) {
        pri_amask = FM_LITERAL_U64(1) << i;
        if (*amask & pri_amask) {
            *amask = pri_amask;
            break;
        }
    }

    fwd_ieee_reserved_mac_cfg_r const * const res_mac_cfg = &(fwd_misc->FWD_IEEE_RESERVED_MAC_CFG);

    fm_byte   trap_tc  = res_mac_cfg->TRAP_TC;
    fm_byte   qos_tc   = qos_tc_in;
    fm_byte   cpu_code = 0;
    fm_uint32 action   = 0;

    resolveAction
    (
        *amask,
        glort_dmask,
        trap_tc,
        amask,
        dmask_o,
        learning_enabled,
        &cpu_code,
        &qos_tc,
        &action
    );

    // output values
    *action_o   = action;
    *qos_tc_o   = qos_tc;
    *cpu_code_o = cpu_code;
}

static void updateActionMask
(
    fm_uint64   const log_amask,
    fm_bool     const rx_mirror,
    fm_uint64 * const amask_o
)
{
    if (log_amask & MBY_LOG_TYPE_CGRP)
        *amask_o |= MBY_AMASK_LOG_INGRESS_CGRP;

    if (log_amask & MBY_LOG_TYPE_RESERVED_MAC)
        *amask_o |= MBY_AMASK_LOG_RESERVED_MAC;

    if (log_amask & MBY_LOG_TYPE_ARP_REDIRECT)
        *amask_o |= MBY_AMASK_LOG_ARP_REDIRECT;

    if (log_amask & MBY_LOG_TYPE_ICMP)
        *amask_o |= MBY_AMASK_LOG_IP_ICMP;

    if (log_amask & MBY_LOG_TYPE_TTL_IP_MC)
        *amask_o |= MBY_AMASK_LOG_IP_TTL;

    if (rx_mirror)
        *amask_o |= MBY_AMASK_MIRROR_INGRESS_CGRP;
}

void MaskGen
(
    mby_ppe_fwd_misc_map  const * const fwd_misc,
    mby_ppe_mst_glort_map const * const glort_map,
    mby_ppe_cm_apply_map  const * const cm_apply,
    mbyNextHopToMaskGen   const * const in,
    mbyMaskGenToTriggers        * const out
)
{
    // Read inputs:
    fm_uint16                  const csglort              = in->CSGLORT;
    fm_bool                    const drop_ttl             = in->DROP_TTL;
    mbyClassifierFlags         const cgrp_flags           = in->CGRP_FLAGS;
    fm_bool                    const flood_forwarded      = in->FLOOD_FORWARDED;
    fm_uint64          const * const glort_dmask_in       = in->GLORT_DMASK;
    fm_bool                    const glort_forwarded      = in->GLORT_FORWARDED;
    fm_uint32                  const hash_rot_a           = in->HASH_ROT_A;
    fm_uint32                  const hash_rot_b           = in->HASH_ROT_B;
    fm_uint16                  const idglort              = in->IDGLORT;
    fm_bool                    const is_ipv4              = in->IS_IPV4;
    fm_bool                    const is_ipv6              = in->IS_IPV6;
    fm_macaddr                 const l2_dmac              = in->L2_DMAC;
    fm_uint16                  const l2_edomain_in        = in->L2_EDOMAIN;
    fm_uint16                  const l2_etype             = in->L2_ETYPE;
    fm_uint16                  const l2_evid1             = in->L2_EVID1;
    fm_uint16                  const l2_ivid1             = in->L2_IVID1;
    fm_bool                    const l2_ivlan1_membership = in->L2_IVLAN1_MEMBERSHIP;
    fm_bool                    const l2_ivlan1_reflect    = in->L2_IVLAN1_REFLECT;
    fm_macaddr                 const l2_smac              = in->L2_SMAC;
    fm_bool                    const routed               = in->ROUTED;
    fm_bool                    const mtu_violation        = in->MTU_VIOLATION;
    fm_bool                    const learn_notify         = in->LEARN_NOTIFY;
    fm_bool                    const parity_error         = in->PARITY_ERROR;
    fm_bool                    const parser_window_v      = in->PARSER_WINDOW_V;
    fm_bool                    const parser_error         = in->PARSER_ERROR;
    fm_byte                    const qos_tc_in            = in->QOS_TC;
    fm_bool                    const rx_mirror_in         = in->RX_MIRROR;
    fm_uint32                  const rx_port              = in->RX_PORT;
    fm_bool                    const sa_hit               = in->SA_HIT;
    mbyMaTable                 const sa_result            = in->SA_RESULT;
    fm_byte                    const sv_drop              = in->SV_DROP;
    fm_bool                    const trap_icmp            = in->TRAP_ICMP;
    fm_bool                    const trap_igmp            = in->TRAP_IGMP;
    fm_bool                    const trap_ip_options      = in->TRAP_IP_OPTIONS;

    // Configurations:
    mbyFwdPortCfg port_cfg;
    port_cfg = getPortCfg(fwd_misc, rx_port);

    mbyFwdPortCfg2 port_cfg2;
    port_cfg2 = getPortCfg2(fwd_misc, l2_edomain_in);

    mbyFwdSysCfg1 sys_cfg1;
    sys_cfg1 = getSysCfg1(fwd_misc);

    mbyFwdSysCfgRouter sys_cfg_router;
    sys_cfg_router = getSysCfgRouter(fwd_misc);

    // Logging:
    fm_bool logging_hit = FALSE;

    // --------------------------------------------------------------------------------
    // GLORT:
    fm_bool glort_cam_miss                     = FALSE;
    fm_bool strict_glort_routing               = FALSE;
    fm_bool targeted_deterministic             = FALSE;
    fm_bool skip_dglort_dec                    = FALSE;
    fm_uint16 ip_mcast_idx                     =   0  ;
    mbyGlortDestTable glort_dest_table         = { 0 };
    fm_uint64 glort_dmask[MBY_DMASK_REGISTERS] = { 0 };
    glortLookup
    (
        glort_map,
        idglort,
        hash_rot_a,
        hash_rot_b,
        glort_dmask_in,
        &glort_cam_miss,
        &strict_glort_routing,
        &targeted_deterministic,
        &skip_dglort_dec,
        glort_dmask,
        &ip_mcast_idx,
        &glort_dest_table
    );

    // --------------------------------------------------------------------------------
    // Learning (Ingress VLAN and STP Filters):
    fm_bool learning_enabled   = FALSE;
    fm_bool l2_smac_is_zero    = FALSE;
    mbyStpState l2_ifid1_state = MBY_STP_STATE_DISABLE;
    learningEnabledCheck
    (
        glort_map,
        fwd_misc,
        &port_cfg,
        rx_port,
        l2_ivid1,
        learn_notify,
        l2_smac,
        &l2_smac_is_zero,
        &learning_enabled,
        &l2_ifid1_state
    );

    // --------------------------------------------------------------------------------
    // Action Mask:
    fm_uint64 amask_in = 0;
    fm_uint64 amask    = 0;
    actionMaskUpdate
    (
        &sys_cfg1,
        amask_in,
        parser_error,
        trap_igmp,
        parity_error,
        l2_etype,
        l2_smac,
        parser_window_v,
        glort_cam_miss,
        targeted_deterministic,
        l2_smac_is_zero,
        &amask
    );

    // --------------------------------------------------------------------------------
    // Special Packet Handling and Ttl Trap Log Drop (ActionMask set):
    fm_byte log_amask         = 0;
    fm_bool store_trap_action = FALSE;
    specialPacketHandlingTtlTrapDropLog
    (
        fwd_misc,
        &sys_cfg1,
        &sys_cfg_router,
        l2_dmac, parser_window_v,
        drop_ttl, trap_icmp,
        ip_mcast_idx,
        l2_ivid1,
        l2_evid1,
        routed,
        trap_ip_options,
        is_ipv4,
        is_ipv6,
        mtu_violation,
        &amask,
        &log_amask,
        &store_trap_action
    );

    // --------------------------------------------------------------------------------
    // Ingress spanning tree check (Ingress VLAN and STP Filters):
    fm_bool mac_moved = FALSE;
    ingressSpanningTreeCheck
    (
        &port_cfg,
        l2_ivlan1_membership,
        sa_hit,
        csglort,
        sv_drop,
        l2_ifid1_state,
        sa_result,
        &amask,
        &mac_moved
    );

    // --------------------------------------------------------------------------------
    // Filtering (PreResolution DMASK for targeted_deterministic, Loopback_suppress(1)
    //            STP Filtering, PreResolution DMASK):
    fm_uint64 pre_resolve_dmask[MBY_DMASK_REGISTERS] = { 0,                };
    fm_uint64 dmask[MBY_DMASK_REGISTERS]             = { MBY_DEFAULT_DMASK };
    fm_byte fclass                                   =   0;
    fm_byte xcast                                    =   0;
    filtering
    (
        glort_map,
        &port_cfg,
        &port_cfg2,
        l2_dmac,
        flood_forwarded,
        glort_forwarded,
        targeted_deterministic,
        routed,
        l2_ivlan1_reflect,
        glort_dmask,
        rx_port,
        l2_evid1,
        &amask,
        pre_resolve_dmask,
        dmask,
        &fclass,
        &xcast
    );

    // --------------------------------------------------------------------------------
    // Classifier Flags (Trapping, Logging, Mirroring):
    fm_bool rx_mirror           = FALSE;
    fm_bool mirror1_profile_v   = FALSE;
    fm_byte mirror1_profile_idx = 0    ;
    classifierFlags
    (
        fwd_misc,
        &cgrp_flags,
        &amask,
        &log_amask,
        rx_mirror_in,
        &rx_mirror,
        &mirror1_profile_v,
        &mirror1_profile_idx
    );

    // --------------------------------------------------------------------------------
    // QCN Mirroring:
    fm_bool   mcst_epoch              = FALSE;
    fm_bool   mirror0_profile_v       = FALSE;
    fm_byte   mirror0_profile_idx     = 0    ;
    fm_uint32 mirror0_port            = 0    ;
    fm_uint32 mirror1_port            = 0    ;
    fm_bool   qcn_mirror0_profile_v   = FALSE;
    fm_bool   qcn_mirror1_profile_v   = FALSE;
    qcnMirroring
    (
        fwd_misc,
        cm_apply,
        rx_mirror,
        &mcst_epoch,
        &mirror0_profile_idx,
        &mirror0_profile_v,
        &mirror1_profile_idx,
        &mirror1_profile_v,
        &mirror0_port,
        &mirror1_port,
        &qcn_mirror0_profile_v,
        &qcn_mirror1_profile_v
    );

    // --------------------------------------------------------------------------------
    // Egress STP state (Egress VLAN Filtering):
    egressSTP
    (
        glort_map,
        &amask,
        l2_evid1,
        pre_resolve_dmask,
        dmask
    );

    // --------------------------------------------------------------------------------
    // Pre-Resolve (PreResolution Action, PreResolution DMASK, PreResolution DGLORT):
    fm_uint16 pre_resolve_dglort = 0;
    fm_uint32 pre_reslove_action = 0;
    preResolveActionDmaskDglort
    (
        amask,
        glort_dmask,
        idglort,
        dmask,
        pre_resolve_dmask,
        &pre_resolve_dglort,
        &pre_reslove_action
    );

    // --------------------------------------------------------------------------------
    // Resolve Action (Action Resolution):
    fm_uint32 action   = 0;
    fm_byte   qos_tc   = 0;
    fm_byte   cpu_code = 0;
    actionResolution
    (
        fwd_misc,
        &amask,
        qos_tc_in,
        glort_dmask,
        &learning_enabled,
        dmask,
        &qos_tc,
        &action,
        &cpu_code
    );

    // --------------------------------------------------------------------------------
    // Update Action Mask (Action Mask)
    updateActionMask
    (
        log_amask,
        rx_mirror,
        &amask
    );

    // --------------------------------------------------------------------------------
    // Write outputs:

    for(fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
    {
        //REVISIT!!!
        out->PRE_RESOLVE_DMASK[i] = pre_resolve_dmask[i];
        out->DMASK            [i] = dmask            [i];
        out->GLORT_DMASK      [i] = glort_dmask      [i];
    }

    out->ACTION                 = action;
    out->AMASK                  = amask;
    out->CPU_CODE               = cpu_code;
    out->DROP_TTL               = drop_ttl;
    out->FCLASS                 = fclass;
    out->GLORT_CAM_MISS         = glort_cam_miss;
    out->HASH_ROT_A             = hash_rot_a;
    out->HASH_ROT_B             = hash_rot_b;
    out->IDGLORT                = idglort;
    out->IP_MCAST_IDX           = ip_mcast_idx;
    out->IS_IPV4                = is_ipv4;
    out->IS_IPV6                = is_ipv6;
    out->L2_DMAC                = l2_dmac;
    out->L2_EDOMAIN             = l2_edomain_in;
    out->L2_EVID1               = l2_evid1;
    out->L2_SMAC                = l2_smac;
    out->LEARNING_ENABLED       = learning_enabled;
    out->LOGGING_HIT            = logging_hit;
    out->LOG_AMASK              = log_amask;
    out->MAC_MOVED              = mac_moved;
    out->ROUTED                 = routed;
    out->MCAST_EPOCH            = mcst_epoch;
    out->MIRROR0_PORT           = mirror0_port;
    out->MIRROR0_PROFILE_IDX    = mirror0_profile_idx;
    out->MIRROR0_PROFILE_V      = mirror0_profile_v;
    out->MIRROR1_PORT           = mirror1_port;
    out->MIRROR1_PROFILE_IDX    = mirror1_profile_idx;
    out->MIRROR1_PROFILE_V      = mirror1_profile_v;
    out->PRE_RESOLVE_ACTION     = pre_reslove_action;
    out->PRE_RESOLVE_DGLORT     = pre_resolve_dglort;
    out->QCN_MIRROR0_PROFILE_V  = qcn_mirror0_profile_v;
    out->QCN_MIRROR1_PROFILE_V  = qcn_mirror1_profile_v;
    out->QOS_TC                 = qos_tc;
    out->RX_MIRROR              = rx_mirror;
    out->RX_PORT                = rx_port;
    out->SKIP_DGLORT_DEC        = skip_dglort_dec;
    out->STORE_TRAP_ACTION      = store_trap_action;
    out->STRICT_GLORT_ROUTING   = strict_glort_routing;
    out->TARGETED_DETERMINISTIC = targeted_deterministic;
    out->XCAST                  = xcast;

    // Pass thru:
    out->CGRP_TRIG              = in->CGRP_TRIG;
    out->CONTENT_ADDR           = in->CONTENT_ADDR;
    out->CPU_TRAP               = in->CPU_TRAP;
    out->ECN                    = in->ECN;
    out->EDGLORT                = in->EDGLORT;
    out->IS_TIMEOUT             = in->IS_TIMEOUT;
    out->L2_IVLAN1_CNT          = in->L2_IVLAN1_CNT;
    out->L3_EDOMAIN             = in->L3_EDOMAIN;
    out->MIRTYP                 = in->MIRTYP;
    out->MOD_IDX                = in->MOD_IDX;
    out->MOD_PROF_IDX           = in->MOD_PROF_IDX;
    out->OOM                    = in->OOM;
    out->NAD                    = in->NAD;
    out->PA_DROP                = in->PA_DROP;
    out->PARSER_INFO            = in->PARSER_INFO;
    out->PA_HDR_PTRS            = in->PA_HDR_PTRS;
    out->PM_ERR                 = in->PM_ERR;
    out->PM_ERR_NONSOP          = in->PM_ERR_NONSOP;
    out->PA_L3LEN_ERR           = in->PA_L3LEN_ERR;
    out->QOS_L3_DSCP            = in->QOS_L3_DSCP;
    out->RX_LENGTH              = in->RX_LENGTH;
    out->SEG_META_ERR           = in->SEG_META_ERR;
    out->TAIL_CSUM_LEN          = in->TAIL_CSUM_LEN;
    out->TRAFFIC_CLASS          = in->TRAFFIC_CLASS;
    out->TX_TAG                 = in->TX_TAG;
    out->RX_LENGTH              = in->RX_LENGTH;
}
