#include <stdlib.h>
#include <stdio.h>

#include <mby_maskgen_test.h>
#include <mby_pipeline.h>
#include <mby_reg_ctrl.h>

#include <mby_top_map.h>

#define COLOR_RED     "\x1b[31m"
#define COLOR_GREEN   "\x1b[32m"
#define COLOR_RESET   "\x1b[0m"

fm_uint tests = 0;
fm_uint fails = 0;
fm_uint passes = 0;

void test_pass(const char * name)
{
    printf(COLOR_GREEN "[pass]" COLOR_RESET " %s\n", name);
    passes++;
    tests++;
}

void test_fail(const char * name)
{
    printf(COLOR_RED   "[FAIL]" COLOR_RESET " %s\n", name );
    fails++;
    tests++;
}

static void maskgen_default_reg_setup
(
    mby_ppe_fwd_misc_map  * const fwd_misc,
    mby_ppe_mst_glort_map * const glort_map,
    mby_ppe_cm_apply_map  * const cm_apply
)
{
    for (fm_uint i = 0; i < mby_ppe_mst_glort_map_GLORT_CAM__nd; i++)
    {
        glort_cam_r * const glort_cam = &(glort_map->GLORT_CAM[i]);
        glort_cam->KEY_INVERT = 0xFFFF;
        glort_cam->KEY        = 0xFFFF;
    }
}

static void maskgen_test_setup
(
    mby_ppe_fwd_misc_map     * const fwd_misc,
    mby_ppe_mst_glort_map    * const glort_map,
    mby_ppe_cm_apply_map     * const cm_apply,
    mbyNextHopToMaskGen      * const nexthopToMaskgen,
    mby_maskgen_test_data_in * const test_in
)
{
    /* Set nexthopToMaskgen. */
    nexthopToMaskgen->RX_PORT              = test_in->rx_port;
    nexthopToMaskgen->L2_SMAC              = test_in->l2_smac;
    nexthopToMaskgen->L2_DMAC              = test_in->l2_dmac;
    nexthopToMaskgen->IDGLORT              = test_in->idglort;
    nexthopToMaskgen->GLORT_DMASK          = test_in->glort_dmask_in;
    nexthopToMaskgen->L2_IVID1             = test_in->l2_ivid1;
    nexthopToMaskgen->L2_EVID1             = test_in->l2_evid1;
    nexthopToMaskgen->AMASK                = test_in->amask;
    nexthopToMaskgen->L2_IVLAN1_MEMBERSHIP = test_in->l2_ivlan1_membership;
    nexthopToMaskgen->L2_EDOMAIN           = test_in->l2_edomain_in;
    nexthopToMaskgen->MARK_ROUTED          = test_in->mark_routed;
    nexthopToMaskgen->PARSER_ERROR         = test_in->parser_error;
    nexthopToMaskgen->TRAP_IGMP            = test_in->trap_igmp;
    nexthopToMaskgen->PARITY_ERROR         = test_in->parity_error;
    nexthopToMaskgen->MTU_VIOLATION        = test_in->mtu_violation;
    nexthopToMaskgen->CSGLORT              = test_in->csglort;

    /* Set FWD_PORT_CFG_1 register. */
    fwd_port_cfg_1_r * const port_cfg_1 = &(fwd_misc->FWD_PORT_CFG_1[test_in->rx_port]);

    port_cfg_1->LEARNING_ENABLE     = test_in->port_cfg_1.learning_enable;
    port_cfg_1->FILTER_VLAN_INGRESS = test_in->port_cfg_1.filter_vlan_ingress;
    port_cfg_1->DESTINATION_MASK    = test_in->port_cfg_1.destination_mask;

    /* Set FWD_PORT_CFG_2 register. */
    fwd_port_cfg_2_r * const port_cfg_2 = &(fwd_misc->FWD_PORT_CFG_2[test_in->l2_edomain_in]);

    port_cfg_2->DESTINATION_MASK = test_in->port_cfg_2.destination_mask;

    /* Set EGRESS_VID_TABLE register. */
    egress_vid_table_r * const vid_table = &(glort_map->EGRESS_VID_TABLE[test_in->l2_evid1][0]);

    vid_table->MEMBERSHIP = test_in->evid_table.membership;

    /* Set FWD_SYS_CFG_1 register. */
    fwd_sys_cfg_1_r * const sys_cfg_1 = &(fwd_misc->FWD_SYS_CFG_1);

    sys_cfg_1->STORE_TRAP_ACTION       = test_in->sys_cfg_1.store_trap_action;
    sys_cfg_1->DROP_MAC_CTRL_ETHERTYPE = test_in->sys_cfg_1.drop_mac_ctrl_ethertype;
    sys_cfg_1->DROP_INVALID_SMAC       = test_in->sys_cfg_1.drop_invalid_smac;
    sys_cfg_1->ENABLE_TRAP_PLUS_LOG    = test_in->sys_cfg_1.enable_trap_plus_log;
    sys_cfg_1->TRAP_MTU_VIOLATIONS     = test_in->sys_cfg_1.trap_mtu_violations;

    /* Set FWD_CPU_MAC register. */
    fwd_cpu_mac_r * const cpu_mac = &(fwd_misc->FWD_CPU_MAC);

    cpu_mac->MAC_ADDR = test_in->fwd_cpu_mac.cpu_mac_addr;

    /* Set FWD_LAG_CFG register. */
    for (fm_uint i = 0; i < MBY_FABRIC_LOG_PORTS; i++)
    {
        fwd_lag_cfg_r * const lag_cfg = &(fwd_misc->FWD_LAG_CFG[i]);

        lag_cfg->IN_LAG        = test_in->fwd_lag_cfg.in_lag;
        lag_cfg->HASH_ROTATION = test_in->fwd_lag_cfg.hash_rotation;
        lag_cfg->INDEX         = test_in->fwd_lag_cfg.index;
        lag_cfg->LAG_SIZE      = test_in->fwd_lag_cfg.lag_size;
    }

    /* Set CM_APPLY_LOOPBACK_SUPPRESS register. */
    for (fm_uint i = 0; i < MBY_FABRIC_LOG_PORTS; i++)
    {
        cm_apply_loopback_suppress_r * const lpbk_sup = &(cm_apply->CM_APPLY_LOOPBACK_SUPPRESS[i]);

        lpbk_sup->GLORT_MASK = test_in->lpbk_suppress.glort_mask;
        lpbk_sup->GLORT      = test_in->lpbk_suppress.glort;
    }
    /* Set GLORT_CAM/GLORT_RAM registers. */
    int index = 1;
    glort_cam_r * const glort_cam = &(glort_map->GLORT_CAM[index]);
    glort_cam->KEY_INVERT               = test_in->glort_cam_ram.key_invert;
    glort_cam->KEY                      = test_in->glort_cam_ram.key;

    glort_ram_r * const glort_ram = &(glort_map->GLORT_RAM[index]);
    glort_ram->SKIP_DGLORT_DEC          = test_in->glort_cam_ram.skip_dglort_dec;
    glort_ram->HASH_ROTATION            = test_in->glort_cam_ram.hash_rotation;
    glort_ram->DEST_COUNT               = test_in->glort_cam_ram.dest_count;
    glort_ram->RANGE_SUB_INDEX_A        = test_in->glort_cam_ram.range_sub_index_a;
    glort_ram->RANGE_SUB_INDEX_B        = test_in->glort_cam_ram.range_sub_index_b;
    glort_ram->DEST_INDEX               = test_in->glort_cam_ram.dest_index;
    glort_ram->STRICT                   = test_in->glort_cam_ram.strict;

    /* Set GLORT_DIRECT_MAP registers. */
    /* Calculate dest_index. */
    fm_uint16 length_a   = (test_in->glort_cam_ram.range_sub_index_a >> 4) & 0xF;
    fm_uint16 length_b   = (test_in->glort_cam_ram.range_sub_index_b >> 4) & 0xF;
    fm_uint16 offset_a   =  test_in->glort_cam_ram.range_sub_index_a & 0xF;
    fm_uint16 offset_b   =  test_in->glort_cam_ram.range_sub_index_b & 0xF;
    fm_uint16 glort_a    = (test_in->glort_cam_ram.key >> offset_a) & ((1 << length_a) - 1);
    fm_uint16 glort_b    = (test_in->glort_cam_ram.key >> offset_b) & ((1 << length_b) - 1);
    fm_uint16 dest_index = 0;
    fm_bool strict_glort_routing =
        (test_in->glort_cam_ram.strict == MBY_GLORT_RAM_STRICT_TARGETED_DETERMINISTIC) ||
        (test_in->glort_cam_ram.strict == MBY_GLORT_RAM_STRICT_DETERMINISTIC);

    if (strict_glort_routing) {
        dest_index = test_in->glort_cam_ram.dest_index + (glort_b << length_a) + glort_a;
    } else {
        // TODO Clarify calculation precedence for '%' and '?'.
        fm_uint32 hash = ((test_in->glort_cam_ram.hash_rotation) ? test_in->hash_rot_b : test_in->hash_rot_a)
                    % (test_in->glort_cam_ram.dest_count == 0) ? 16 : test_in->glort_cam_ram.dest_count;
        dest_index = test_in->glort_cam_ram.dest_index + (hash << length_a) + glort_a;
    }

    /* Temporary use only two proxy registers
     * - GLORT_DIRECT_MAP_DST0 - for DEST_MASK
     * - GLORT_DIRECT_MAP_DST4 - for IP_MULTICAST_INDEX
     * <-- REVISIT!!!
     */
    glort_direct_map_dst0_r * const map_dst0 = &(glort_map->GLORT_DIRECT_MAP_DST0);
    glort_direct_map_dst4_r * const map_dst4 = &(glort_map->GLORT_DIRECT_MAP_DST4);

    map_dst4->IP_MULTICAST_INDEX = test_in->glort_map.ip_multicast_index;
    map_dst0->DEST_MASK          = test_in->glort_map.dest_mask;

    /* Set INGRESS_MST_TABLE register. */
    ingress_mst_table_r * const ingress_mst_table = &(glort_map->INGRESS_MST_TABLE[test_in->l2_ivid1]);

    ingress_mst_table->STP_STATE_0 = test_in->ingress_mst_table.stp_state_0;
    ingress_mst_table->STP_STATE_1 = test_in->ingress_mst_table.stp_state_1;
    ingress_mst_table->STP_STATE_2 = test_in->ingress_mst_table.stp_state_2;
    ingress_mst_table->STP_STATE_3 = test_in->ingress_mst_table.stp_state_3;
    ingress_mst_table->STP_STATE_4 = test_in->ingress_mst_table.stp_state_4;
    ingress_mst_table->STP_STATE_5 = test_in->ingress_mst_table.stp_state_5;
    ingress_mst_table->STP_STATE_6 = test_in->ingress_mst_table.stp_state_6;
    ingress_mst_table->STP_STATE_7 = test_in->ingress_mst_table.stp_state_7;
    ingress_mst_table->STP_STATE_8 = test_in->ingress_mst_table.stp_state_8;
    ingress_mst_table->STP_STATE_9 = test_in->ingress_mst_table.stp_state_9;
    ingress_mst_table->STP_STATE_10 = test_in->ingress_mst_table.stp_state_10;
    ingress_mst_table->STP_STATE_11 = test_in->ingress_mst_table.stp_state_11;
    ingress_mst_table->STP_STATE_12 = test_in->ingress_mst_table.stp_state_12;
    ingress_mst_table->STP_STATE_13 = test_in->ingress_mst_table.stp_state_13;
    ingress_mst_table->STP_STATE_14 = test_in->ingress_mst_table.stp_state_14;
    ingress_mst_table->STP_STATE_15 = test_in->ingress_mst_table.stp_state_15;
    ingress_mst_table->STP_STATE_16 = test_in->ingress_mst_table.stp_state_16;
    ingress_mst_table->STP_STATE_17 = test_in->ingress_mst_table.stp_state_17;

    /* Set EGRESS_MST_TABLE register. */
    egress_mst_table_r * const egress_mst_table = &(glort_map->EGRESS_MST_TABLE[test_in->l2_evid1][0]);

    egress_mst_table->FORWARDING = test_in->egress_mst_table.forwarding;
}

static fm_bool maskgen_test_verify
(
    mbyMaskGenToTriggers      * const maskgenToTriggers,
    mby_maskgen_test_data_out * const test_data_out
)
{
    if (maskgenToTriggers->GLORT_CAM_MISS != test_data_out->glort_cam_miss)
        return FALSE;

    if (maskgenToTriggers->STRICT_GLORT_ROUTING != test_data_out->strict_glort_routing)
        return FALSE;

    if (maskgenToTriggers->TARGETED_DETERMINISTIC != test_data_out->targeted_deterministic)
        return FALSE;

    if (maskgenToTriggers->AMASK != test_data_out->amask)
        return FALSE;

    if (maskgenToTriggers->SKIP_DGLORT_DEC != test_data_out->skip_dglort_dec)
        return FALSE;

    if (maskgenToTriggers->IP_MCAST_IDX != test_data_out->ip_mcast_idx)
        return FALSE;

    if (maskgenToTriggers->LEARNING_ENABLED != test_data_out->learning_enabled)
        return FALSE;

    if (maskgenToTriggers->LOG_AMASK != test_data_out->log_amask)
        return FALSE;

    if (maskgenToTriggers->STORE_TRAP_ACTION != test_data_out->store_trap_action)
        return FALSE;

    //REVISIT!!!! dmask is changed, we need to fix test
    for(fm_uint i = 0; i < MBY_DMASK_REGISTERS; i++)
        if (maskgenToTriggers->DMASK[i] != test_data_out->dmask)
            return FALSE;

    if (maskgenToTriggers->ACTION != test_data_out->action)
        return FALSE;

    return TRUE;
}

static void maskgen_run_test(maskgen_test_data * const test_data)
{
    mby_ppe_fwd_misc_map  fwd_misc;
    mby_ppe_mst_glort_map glort_map;
    mby_ppe_cm_apply_map  cm_apply;

    mbyNextHopToMaskGen  nexthopToMaskgen = { 0 };
    mbyMaskGenToTriggers out              = { 0 };

    maskgen_default_reg_setup(&fwd_misc, &glort_map, &cm_apply);

    maskgen_test_setup(&fwd_misc, &glort_map, &cm_apply, &nexthopToMaskgen, &(test_data->in));

    MaskGen(&fwd_misc, &glort_map, &cm_apply, &nexthopToMaskgen, &out);


    fm_bool pass = maskgen_test_verify(&out, &(test_data->out));

    if (pass)
        test_pass(test_data->name);
    else
        test_fail(test_data->name);
}

int main()
{
    printf("--------------------------------------------------------------------------------\n");

    int err;

    fm_uint tests_num = sizeof(maskgen_tests) / sizeof(maskgen_test_data);

    for (fm_uint test_num = 0; test_num < tests_num; test_num++)
        maskgen_run_test(&maskgen_tests[test_num]);

    printf("--------------------------------------------------------------------------------\n");

    if (fails == 0)
        printf(COLOR_GREEN "[pass]");
    else
        printf(COLOR_RED   "[FAIL]");

    printf(" %2d/%2d - Maskgen tests\n" COLOR_RESET, passes, tests);

    printf("--------------------------------------------------------------------------------\n");

    int rv = (fails == 0) ? 0 : -1;
    return rv;
}
