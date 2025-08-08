// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include "mby_maskgen_regs.h"

mbyGlortRam getGlortRamEntry
(
    mby_ppe_mst_glort_map const * const glort_map,
    fm_byte                       const ram_index
)
{
    mbyGlortRam ram_entry;

    glort_ram_r const * const glort_ram = &(glort_map->GLORT_RAM[ram_index]);

    ram_entry.SKIP_DGLORT_DEC   = glort_ram->SKIP_DGLORT_DEC;
    ram_entry.HASH_ROTATION     = glort_ram->HASH_ROTATION;
    ram_entry.DEST_COUNT        = glort_ram->DEST_COUNT;
    ram_entry.RANGE_SUB_INDEX_A = glort_ram->RANGE_SUB_INDEX_A;
    ram_entry.RANGE_SUB_INDEX_B = glort_ram->RANGE_SUB_INDEX_B;
    ram_entry.DEST_INDEX        = glort_ram->DEST_INDEX;
    ram_entry.STRICT            = glort_ram->STRICT;

    return ram_entry;
}

mbyGlortCam getGlortCamEntry
(
    mby_ppe_mst_glort_map const * const glort_map,
    fm_byte                             cam_index
)
{
    mbyGlortCam cam_entry;

    glort_cam_r const * const glort_cam = &(glort_map->GLORT_CAM[cam_index]);

    cam_entry.KEY_INVERT = glort_cam->KEY_INVERT;
    cam_entry.KEY        = glort_cam->KEY;

    return cam_entry;
}

mbyGlortDestTable getGlortDestTableEntry
(
    mby_ppe_mst_glort_map const * const glort_map,
    fm_uint16                           table_index
)
{
    mbyGlortDestTable table_entry;

    //!!! REVISIT RDL changes needed here, indexed by table_index
    glort_direct_map_dst0_r const * const map_dst0 = &(glort_map->GLORT_DIRECT_MAP_DST0);
    glort_direct_map_dst1_r const * const map_dst1 = &(glort_map->GLORT_DIRECT_MAP_DST1);
    glort_direct_map_dst2_r const * const map_dst2 = &(glort_map->GLORT_DIRECT_MAP_DST2);
    glort_direct_map_dst3_r const * const map_dst3 = &(glort_map->GLORT_DIRECT_MAP_DST3);
    glort_direct_map_dst4_r const * const map_dst4 = &(glort_map->GLORT_DIRECT_MAP_DST4);

    table_entry.IP_MULTICAST_INDEX = map_dst4->IP_MULTICAST_INDEX;
    table_entry.DEST_MASK[4]       = map_dst4->DEST_MASK;
    table_entry.DEST_MASK[3]       = map_dst3->DEST_MASK;
    table_entry.DEST_MASK[2]       = map_dst2->DEST_MASK;
    table_entry.DEST_MASK[1]       = map_dst1->DEST_MASK;
    table_entry.DEST_MASK[0]       = map_dst0->DEST_MASK;

    return table_entry;
}

mbyEgressVidTableCfg getEvidTableCfgEntry
(
    mby_ppe_mst_glort_map const * const mst_glort,
    fm_uint16                           vid
)
{
    mbyEgressVidTableCfg entry;

    egress_vid_table_r const * const vid_table0 = &(mst_glort->EGRESS_VID_TABLE[vid][0]);
    egress_vid_table_r const * const vid_table1 = &(mst_glort->EGRESS_VID_TABLE[vid][1]);
    egress_vid_table_r const * const vid_table2 = &(mst_glort->EGRESS_VID_TABLE[vid][2]);
    egress_vid_table_r const * const vid_table3 = &(mst_glort->EGRESS_VID_TABLE[vid][3]);
    egress_vid_table_r const * const vid_table4 = &(mst_glort->EGRESS_VID_TABLE[vid][4]);
    egress_vid_cfg_r   const * const vid_cfg    = &(mst_glort->EGRESS_VID_CFG  [vid]   );

    entry.MEMBERSHIP[0] = vid_table0->MEMBERSHIP;
    entry.MEMBERSHIP[1] = vid_table1->MEMBERSHIP;
    entry.MEMBERSHIP[2] = vid_table2->MEMBERSHIP;
    entry.MEMBERSHIP[3] = vid_table3->MEMBERSHIP;
    entry.MEMBERSHIP[4] = vid_table4->MEMBERSHIP;
    entry.TRIG_ID       = vid_cfg   ->TRIG_ID;

    return entry;
}

mbyFwdPortCfg getPortCfg
(
    mby_ppe_fwd_misc_map const * const fwd_misc,
    fm_uint32                    const port // RX port
)
{
    mbyFwdPortCfg cfg;

    fwd_port_cfg_0_r   const * const port_cfg_0   = &(fwd_misc->FWD_PORT_CFG_0[port]);
    fwd_port_cfg_1_0_r const * const port_cfg_1_0 = &(fwd_misc->FWD_PORT_CFG_1_0[port]);
    fwd_port_cfg_1_1_r const * const port_cfg_1_1 = &(fwd_misc->FWD_PORT_CFG_1_1[port]);
    fwd_port_cfg_1_2_r const * const port_cfg_1_2 = &(fwd_misc->FWD_PORT_CFG_1_2[port]);
    fwd_port_cfg_1_3_r const * const port_cfg_1_3 = &(fwd_misc->FWD_PORT_CFG_1_3[port]);
    fwd_port_cfg_1_4_r const * const port_cfg_1_4 = &(fwd_misc->FWD_PORT_CFG_1_4[port]);

    cfg.LEARNING_ENABLE     = port_cfg_0->LEARNING_ENABLE;
    cfg.FILTER_VLAN_INGRESS = port_cfg_0->FILTER_VLAN_INGRESS;

    cfg.DESTINATION_MASK[4] = port_cfg_1_4->DESTINATION_MASK;
    cfg.DESTINATION_MASK[3] = port_cfg_1_3->DESTINATION_MASK;
    cfg.DESTINATION_MASK[2] = port_cfg_1_2->DESTINATION_MASK;
    cfg.DESTINATION_MASK[1] = port_cfg_1_1->DESTINATION_MASK;
    cfg.DESTINATION_MASK[0] = port_cfg_1_0->DESTINATION_MASK;

    return cfg;
}

mbyFwdPortCfg2 getPortCfg2
(
    mby_ppe_fwd_misc_map const * const fwd_misc,
    fm_uint16                    const l2_edomain
)
{
    mbyFwdPortCfg2 cfg;

    fwd_port_cfg_2_r const * const port_cfg_2 = &(fwd_misc->FWD_PORT_CFG_2[l2_edomain]);

    cfg.DESTINATION_MASK = port_cfg_2->DESTINATION_MASK;

    return cfg;
}

mbyFwdSysCfg1 getSysCfg1
(
    mby_ppe_fwd_misc_map const * const fwd_misc
)
{
    mbyFwdSysCfg1 cfg;

    fwd_sys_cfg_1_r const * const sys_cfg_1 = &(fwd_misc->FWD_SYS_CFG_1);

    cfg.STORE_TRAP_ACTION       = sys_cfg_1->STORE_TRAP_ACTION;
    cfg.DROP_MAC_CTRL_ETHERTYPE = sys_cfg_1->DROP_MAC_CTRL_ETHERTYPE;
    cfg.DROP_INVALID_SMAC       = sys_cfg_1->DROP_INVALID_SMAC;
    cfg.ENABLE_TRAP_PLUS_LOG    = sys_cfg_1->ENABLE_TRAP_PLUS_LOG;
    cfg.TRAP_MTU_VIOLATIONS     = sys_cfg_1->TRAP_MTU_VIOLATIONS;

    return cfg;
}

mbyFwdSysCfgRouter getSysCfgRouter
(
    mby_ppe_fwd_misc_map const * const fwd_misc
)
{
    mbyFwdSysCfgRouter cfg;

    fwd_sys_cfg_router_r const * const sys_cfg_router = &(fwd_misc->FWD_SYS_CFG_ROUTER);

    cfg.TRAP_IP_OPTIONS = sys_cfg_router->TRAP_IP_OPTIONS;
    cfg.TRAP_TTL1       = sys_cfg_router->TRAP_TTL1;

    return cfg;
}

mbyFwdLagCfg getLagCfg
(
    mby_ppe_fwd_misc_map const * const fwd_misc,
    fm_uint32                    const port
)
{
    mbyFwdLagCfg cfg;

    fwd_lag_cfg_r const * const lag_cfg = &(fwd_misc->FWD_LAG_CFG[port]);

    cfg.IN_LAG        = lag_cfg->IN_LAG;
    cfg.HASH_ROTATION = lag_cfg->HASH_ROTATION;
    cfg.INDEX         = lag_cfg->INDEX;
    cfg.LAG_SIZE      = lag_cfg->LAG_SIZE;

    return cfg;
}

mbyCmApplyLoopbackSuppress getLoopbackSuppress
(
    mby_ppe_cm_apply_map const * const cm_apply,
    fm_uint32                    const port // logical port
)
{
    mbyCmApplyLoopbackSuppress loopSupp;

    cm_apply_loopback_suppress_r const * const loop_supp = &(cm_apply->CM_APPLY_LOOPBACK_SUPPRESS[port]);

    loopSupp.GLORT_MASK = loop_supp->GLORT_MASK;
    loopSupp.GLORT      = loop_supp->GLORT;

    return loopSupp;
}

mbyCpuTrapMask getCpuTrapMask
(
    mby_ppe_cm_apply_map const * const cm_apply
)
{
    mbyCpuTrapMask cpu_trap_mask;

    cm_apply_cpu_trap_mask_0_r const * const cpu_trap_mask_0 = &(cm_apply->CM_APPLY_CPU_TRAP_0_MASK);
    cm_apply_cpu_trap_mask_1_r const * const cpu_trap_mask_1 = &(cm_apply->CM_APPLY_CPU_TRAP_1_MASK);
    cm_apply_cpu_trap_mask_2_r const * const cpu_trap_mask_2 = &(cm_apply->CM_APPLY_CPU_TRAP_2_MASK);
    cm_apply_cpu_trap_mask_3_r const * const cpu_trap_mask_3 = &(cm_apply->CM_APPLY_CPU_TRAP_3_MASK);
    cm_apply_cpu_trap_mask_4_r const * const cpu_trap_mask_4 = &(cm_apply->CM_APPLY_CPU_TRAP_4_MASK);

    cpu_trap_mask.DEST_MASK[4] = cpu_trap_mask_4->DEST_MASK;
    cpu_trap_mask.DEST_MASK[3] = cpu_trap_mask_3->DEST_MASK;
    cpu_trap_mask.DEST_MASK[2] = cpu_trap_mask_2->DEST_MASK;
    cpu_trap_mask.DEST_MASK[1] = cpu_trap_mask_1->DEST_MASK;
    cpu_trap_mask.DEST_MASK[0] = cpu_trap_mask_0->DEST_MASK;

    return cpu_trap_mask;
}

mbyMirrorEcmpDmask getMirrorEcmpDmask
(
    mby_ppe_cm_apply_map const * const cm_apply,
    fm_uint16                    const mirror_profile_idx
)
{
    mbyMirrorEcmpDmask mirror_ecmp_dmask;

    cm_apply_mirror_ecmp_dmask0_r const * const mirror_ecmp_dmask_0 = &(cm_apply->CM_APPLY_MIRROR_ECMP_DMASK0[mirror_profile_idx]);
    cm_apply_mirror_ecmp_dmask1_r const * const mirror_ecmp_dmask_1 = &(cm_apply->CM_APPLY_MIRROR_ECMP_DMASK1[mirror_profile_idx]);
    cm_apply_mirror_ecmp_dmask2_r const * const mirror_ecmp_dmask_2 = &(cm_apply->CM_APPLY_MIRROR_ECMP_DMASK2[mirror_profile_idx]);
    cm_apply_mirror_ecmp_dmask3_r const * const mirror_ecmp_dmask_3 = &(cm_apply->CM_APPLY_MIRROR_ECMP_DMASK3[mirror_profile_idx]);
    cm_apply_mirror_ecmp_dmask4_r const * const mirror_ecmp_dmask_4 = &(cm_apply->CM_APPLY_MIRROR_ECMP_DMASK4[mirror_profile_idx]);

    mirror_ecmp_dmask.Mirror_port_mask[4] = mirror_ecmp_dmask_4->Mirror_port_mask4;
    mirror_ecmp_dmask.Mirror_port_mask[3] = mirror_ecmp_dmask_3->Mirror_port_mask3;
    mirror_ecmp_dmask.Mirror_port_mask[2] = mirror_ecmp_dmask_2->Mirror_port_mask2;
    mirror_ecmp_dmask.Mirror_port_mask[1] = mirror_ecmp_dmask_1->Mirror_port_mask1;
    mirror_ecmp_dmask.Mirror_port_mask[0] = mirror_ecmp_dmask_0->Mirror_port_mask0;

    return mirror_ecmp_dmask;
}