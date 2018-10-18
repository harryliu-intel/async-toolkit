#include "mby_basic_fwd_init.h"

#include <mby_bitfield.h>
#include <mby_classifier.h>
#include <mby_common.h>
#include <mby_congmgmt.h>
#include <mby_crc32.h>
#include <mby_hash.h>
#include <mby_mapper.h>
#include <mby_maskgen.h>
#include <mby_model.h>
#include <mby_modifier.h>
#include <mby_nexthop.h>
#include <mby_parser.h>
#include <mby_pipeline.h>
#include <mby_reg_ctrl.h>
#include <mby_rxstats.h>
#include <mby_triggers.h>
#include <mby_txstats.h>

void basic_fwd_init
(
#ifdef USE_NEW_CSRS
    mby_ppe_rx_top_map * const rx_top_map,
#endif
    fm_uint32 fwd_port,
    fm_macaddr dmac
)
{
#ifdef USE_NEW_CSRS
    ingress_vid_table_r * const ivid_table = &(rx_top_map->nexthop.INGRESS_VID_TABLE[1]);

    ivid_table->TRAP_IGMP  = 0;
    ivid_table->REFLECT    = 0;
    ivid_table->MEMBERSHIP = 0x3ffff;

    egress_vid_table_r * const evid_table = &(rx_top_map->mst_glort.EGRESS_VID_TABLE[1][0]);

    evid_table->MEMBERSHIP = 0x3ffff;

    ingress_mst_table_r * const ingress_mst_table = &(rx_top_map->mst_glort.INGRESS_MST_TABLE[1]);

    ingress_mst_table->STP_STATE_0 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_1 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_2 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_3 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_4 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_5 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_6 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_7 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_8 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_9 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_10 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_11 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_12 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_13 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_14 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_15 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_16 = MBY_STP_STATE_FORWARD;
    ingress_mst_table->STP_STATE_16 = MBY_STP_STATE_FORWARD;

    egress_mst_table_r * const egress_mst_table = &(rx_top_map->mst_glort.EGRESS_MST_TABLE[1][0]);

    egress_mst_table->FORWARDING = 0x3ffff;

    glort_direct_map_dst0_r * const map_dst0 = &(rx_top_map->mst_glort.GLORT_DIRECT_MAP_DST0);
    glort_direct_map_dst4_r * const map_dst4 = &(rx_top_map->mst_glort.GLORT_DIRECT_MAP_DST4);
    map_dst4->IP_MULTICAST_INDEX             = 0x1;
    map_dst0->DEST_MASK                      = 0x20;

    /* This part is to add DMAC to WCM. */
    map_profile_key0_r * const map_prof_key0 = &(rx_top_map->mapper.MAP_PROFILE_KEY0[1]);
    map_prof_key0->CSUM = 0x3;

    fm_uint slice   = 0;
    fm_uint profile = 1;
    fm_uint index   = 1022;
    fm_uint ram_num = 0;

    map_profile_action_r * const prof_action = &(rx_top_map->mapper.MAP_PROFILE_ACTION[1]);
    prof_action->PROFILE_VALID = 1;
    prof_action->PROFILE       = profile;

    wcm_tcam_cfg_r * wcm_tcam_cfg = &(rx_top_map->cgrp_b.WCM_TCAM_CFG[slice][profile]);

    wcm_tcam_cfg->START_COMPARE = 1;
    wcm_tcam_cfg->SELECT_TOP    = 0x0;
    wcm_tcam_cfg->SELECT0       = 0x8;
    wcm_tcam_cfg->SELECT1       = 0x8;
    wcm_tcam_cfg->CHUNK_MASK    = 0xffff;

    wcm_tcam_cfg = &(rx_top_map->cgrp_b.WCM_TCAM_CFG[slice+1][profile]);
    wcm_tcam_cfg->SELECT_TOP    = 0x0;
    wcm_tcam_cfg->SELECT0       = 0x7;
    wcm_tcam_cfg->SELECT1       = 0x7;
    wcm_tcam_cfg->SELECT2       = 0x6;
    wcm_tcam_cfg->SELECT3       = 0x6;
    wcm_tcam_cfg->CHUNK_MASK    = 0xffff;

    wcm_tcam_cfg = &(rx_top_map->cgrp_b.WCM_TCAM_CFG[slice+2][profile]);
    wcm_tcam_cfg->START_COMPARE = 1;

    /* Set KEYS to DMAC. */
    wcm_tcam_r * wcm_tcam_entry = &(rx_top_map->cgrp_b.WCM_TCAM[slice][index]);
    wcm_tcam_entry->KEY            = dmac & 0xffff;
    wcm_tcam_entry->KEY_INVERT     = ~wcm_tcam_entry->KEY;
    wcm_tcam_entry->KEY_TOP        = 0x0;
    wcm_tcam_entry->KEY_TOP_INVERT = 0x0;

    wcm_tcam_entry = &(rx_top_map->cgrp_b.WCM_TCAM[slice+1][index]);
    wcm_tcam_entry->KEY            = (dmac >> 16) & 0xffffffff;
    wcm_tcam_entry->KEY_INVERT     = ~wcm_tcam_entry->KEY;
    wcm_tcam_entry->KEY_TOP        = 0x0;
    wcm_tcam_entry->KEY_TOP_INVERT = 0x0;

    wcm_action_cfg_r * const wcm_action_cfg = &(rx_top_map->cgrp_b.WCM_ACTION_CFG[ram_num]);
    wcm_action_cfg->ENABLE_0 = TRUE;
    wcm_action_cfg->INDEX_0  = slice + 1;

    wcm_action_r * const wcm_action = &(rx_top_map->cgrp_b.WCM_ACTION[ram_num][index]);
    FM_SET_FIELD(wcm_action->ACTION0, MBY_FFU_ACTION, SET1_24B_INDEX, 4);
    FM_SET_FIELD(wcm_action->ACTION0, MBY_FFU_ACTION, SET1_24B_VALUE, (0x100 + fwd_port));
    FM_SET_FIELD(wcm_action->ACTION0, MBY_FFU_ACTION, PREC, 1);
    FM_SET_UNNAMED_FIELD(wcm_action->ACTION0, 28, 1, 1);

#else
    /* Add L2 entry to match on. */
    fm_uint32 ma_table_regs[MBY_MA_TABLE_WIDTH] = { 0 };

    FM_ARRAY_SET_FIELD(ma_table_regs, MBY_MA_TABLE, OLD_PORT, 0);
    FM_ARRAY_SET_FIELD(ma_table_regs, MBY_MA_TABLE, NEW_PORT, 0);
    FM_ARRAY_SET_FIELD(ma_table_regs, MBY_MA_TABLE, ENTRY_TYPE, MBY_MA_LOOKUP_ENTRY_TYPE_STATIC);
    FM_ARRAY_SET_FIELD(ma_table_regs, MBY_MA_TABLE, TRIG_ID, 0);
    FM_ARRAY_SET_FIELD(ma_table_regs, MBY_MA_TABLE, S_GLORT, 0);
    FM_ARRAY_SET_FIELD(ma_table_regs, MBY_MA_TABLE, D_GLORT, 0x100 + fwd_port);
    FM_ARRAY_SET_FIELD(ma_table_regs, MBY_MA_TABLE, L2_DOMAIN, 0);
    FM_ARRAY_SET_FIELD(ma_table_regs, MBY_MA_TABLE, VID, 0);
    FM_ARRAY_SET_FIELD64(ma_table_regs, MBY_MA_TABLE, MAC_ADDRESS, dmac);

    fm_uint16 cam_index = MBY_MA_TABLE_TCAM_SIZE - 1;
    fm_byte   bank      = MBY_MAC_ADDR_BANK_COUNT - 1;
    mbyWriteRegMult(0, MBY_MA_TABLE(bank, cam_index, 0), MBY_MA_TABLE_WIDTH, ma_table_regs);

    // TODO uncomment once these regs exist
    mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_INGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_EGRESS_VID_TABLE(0x1, 0x0), 0x1fffff);
    //mbyWriteReg(0, MBY_MOD_VLAN_TAG(0x1, 0x0), 0x0);
    //mbyWriteReg(0, MBY_INGRESS_MST_TABLE(0x1, 0x0), 0xffffffffffff);
    //mbyWriteReg(0, MBY_EGRESS_MST_TABLE(0x1, 0x0), 0x1fffff);
#endif
}
