#include <stdlib.h>
#include <stdio.h>

#include <mby_maskgen_test.h>
#include <mby_pipeline.h>
#include <mby_reg_ctrl.h>

#ifdef USE_NEW_CSRS
#include <mby_top_map.h>
#endif

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

#ifdef USE_NEW_CSRS
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
#endif

static void maskgen_test_setup
(
#ifdef USE_NEW_CSRS
    mby_ppe_fwd_misc_map     * const fwd_misc,
    mby_ppe_mst_glort_map    * const glort_map,
    mby_ppe_cm_apply_map     * const cm_apply,
#else
    fm_uint32                        regs[MBY_REGISTER_ARRAY_SIZE],
#endif
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
    nexthopToMaskgen->L2_EVLAN1_MEMBERSHIP = test_in->l2_evlan1_membership;
    nexthopToMaskgen->L2_IFID1_STATE       = test_in->l2_ifid1_state;
    nexthopToMaskgen->L2_EFID1_STATE       = test_in->l2_efid1_state;
    nexthopToMaskgen->L2_EDOMAIN           = test_in->l2_edomain_in;
    nexthopToMaskgen->MARK_ROUTED          = test_in->mark_routed;
    nexthopToMaskgen->PARSER_ERROR         = test_in->parser_error;
    nexthopToMaskgen->TRAP_IGMP            = test_in->trap_igmp;
    nexthopToMaskgen->PARITY_ERROR         = test_in->parity_error;
    nexthopToMaskgen->MTU_VIOLATION        = test_in->mtu_violation;
    nexthopToMaskgen->CSGLORT              = test_in->csglort;

    /* Set FWD_PORT_CFG_1 register. */
#ifdef USE_NEW_CSRS
    fwd_port_cfg_1_r * const port_cfg_1 = &(fwd_misc->FWD_PORT_CFG_1[test_in->rx_port]);

    port_cfg_1->LEARNING_ENABLE     = test_in->port_cfg_1.learning_enable;
    port_cfg_1->FILTER_VLAN_INGRESS = test_in->port_cfg_1.filter_vlan_ingress;
    port_cfg_1->DESTINATION_MASK    = test_in->port_cfg_1.destination_mask;
#else
    fm_uint64 fwd_port_cfg1_reg = 0;

    FM_SET_BIT64  (fwd_port_cfg1_reg, MBY_FWD_PORT_CFG_1, LEARNING_ENABLE, test_in->port_cfg_1.learning_enable);
    FM_SET_BIT64  (fwd_port_cfg1_reg, MBY_FWD_PORT_CFG_1, FILTER_VLAN_INGRESS, test_in->port_cfg_1.filter_vlan_ingress);
    FM_SET_FIELD64(fwd_port_cfg1_reg, MBY_FWD_PORT_CFG_1, DESTINATION_MASK, test_in->port_cfg_1.destination_mask);
    mbyModelWriteCSR64(regs, MBY_FWD_PORT_CFG_1(test_in->rx_port, 0), fwd_port_cfg1_reg);
#endif

    /* Set FWD_PORT_CFG_2 register. */
#ifdef USE_NEW_CSRS
    fwd_port_cfg_2_r * const port_cfg_2 = &(fwd_misc->FWD_PORT_CFG_2[test_in->l2_edomain_in]);

    port_cfg_2->DESTINATION_MASK = test_in->port_cfg_2.destination_mask;
#else
    fm_uint64 fwd_port_cfg2_reg = 0;

    FM_SET_FIELD64(fwd_port_cfg2_reg, MBY_FWD_PORT_CFG_2, DESTINATION_MASK, test_in->port_cfg_2.destination_mask);
    mbyModelWriteCSR64(regs, MBY_FWD_PORT_CFG_2(test_in->l2_edomain_in, 0), fwd_port_cfg2_reg);
#endif

    /* Set FWD_SYS_CFG_1 register. */
#ifdef USE_NEW_CSRS
    fwd_sys_cfg_1_r * const sys_cfg_1 = &(fwd_misc->FWD_SYS_CFG_1);

    sys_cfg_1->STORE_TRAP_ACTION       = test_in->sys_cfg_1.store_trap_action;
    sys_cfg_1->DROP_MAC_CTRL_ETHERTYPE = test_in->sys_cfg_1.drop_mac_ctrl_ethertype;
    sys_cfg_1->DROP_INVALID_SMAC       = test_in->sys_cfg_1.drop_invalid_smac;
    sys_cfg_1->ENABLE_TRAP_PLUS_LOG    = test_in->sys_cfg_1.enable_trap_plus_log;
    sys_cfg_1->TRAP_MTU_VIOLATIONS     = test_in->sys_cfg_1.trap_mtu_violations;
#else
    fm_uint64 fwd_sys_cfg1_reg = 0;
    mbyModelReadCSR64(regs, MBY_FWD_SYS_CFG_1(0), &fwd_sys_cfg1_reg);

    FM_SET_BIT64(fwd_sys_cfg1_reg, MBY_FWD_SYS_CFG_1, STORE_TRAP_ACTION, test_in->sys_cfg_1.store_trap_action);
    FM_SET_BIT64(fwd_sys_cfg1_reg, MBY_FWD_SYS_CFG_1, DROP_MAC_CTRL_ETHERTYPE, test_in->sys_cfg_1.drop_mac_ctrl_ethertype);
    FM_SET_BIT64(fwd_sys_cfg1_reg, MBY_FWD_SYS_CFG_1, DROP_INVALID_SMAC, test_in->sys_cfg_1.drop_invalid_smac);
    FM_SET_BIT64(fwd_sys_cfg1_reg, MBY_FWD_SYS_CFG_1, ENABLE_TRAP_PLUS_LOG, test_in->sys_cfg_1.enable_trap_plus_log);
    FM_SET_BIT64(fwd_sys_cfg1_reg, MBY_FWD_SYS_CFG_1, TRAP_MTU_VIOLATIONS, test_in->sys_cfg_1.trap_mtu_violations);
#endif

    /* Set FWD_CPU_MAC register. */
#ifdef USE_NEW_CSRS
    fwd_cpu_mac_r * const cpu_mac = &(fwd_misc->FWD_CPU_MAC);

    cpu_mac->MAC_ADDR = test_in->fwd_cpu_mac.cpu_mac_addr;
#else
    fm_uint64 fwd_cpu_mac_reg = 0;

    FM_SET_FIELD64(fwd_cpu_mac_reg, MBY_FWD_CPU_MAC, MAC_ADDR, test_in->fwd_cpu_mac.cpu_mac_addr);
    mbyModelWriteCSR64(regs, MBY_FWD_CPU_MAC(0), fwd_cpu_mac_reg);
#endif

    /* Set FWD_LAG_CFG register. */
    for (fm_uint i = 0; i < MBY_FABRIC_LOG_PORTS; i++)
    {
#ifdef USE_NEW_CSRS
        fwd_lag_cfg_r * const lag_cfg = &(fwd_misc->FWD_LAG_CFG[i]);

        lag_cfg->IN_LAG        = test_in->fwd_lag_cfg.in_lag;
        lag_cfg->HASH_ROTATION = test_in->fwd_lag_cfg.hash_rotation;
        lag_cfg->INDEX         = test_in->fwd_lag_cfg.index;
        lag_cfg->LAG_SIZE      = test_in->fwd_lag_cfg.lag_size;
#else
        fm_uint64 fwd_lag_cfg_reg = 0;

        FM_SET_BIT64  (fwd_lag_cfg_reg, MBY_FWD_LAG_CFG, IN_LAG, test_in->fwd_lag_cfg.in_lag);
        FM_SET_BIT64  (fwd_lag_cfg_reg, MBY_FWD_LAG_CFG, HASH_ROTATION, test_in->fwd_lag_cfg.hash_rotation);
        FM_SET_FIELD64(fwd_lag_cfg_reg, MBY_FWD_LAG_CFG, INDEX, test_in->fwd_lag_cfg.index);
        FM_SET_FIELD64(fwd_lag_cfg_reg, MBY_FWD_LAG_CFG, LAG_SIZE, test_in->fwd_lag_cfg.lag_size);

        mbyModelWriteCSR64(regs, MBY_FWD_LAG_CFG(i, 0), fwd_lag_cfg_reg);
#endif
    }

    /* Set CM_APPLY_LOOPBACK_SUPPRESS register. */
    for (fm_uint i = 0; i < MBY_FABRIC_LOG_PORTS; i++)
    {
#ifdef USE_NEW_CSRS
        cm_apply_loopback_suppress_r * const lpbk_sup = &(cm_apply->CM_APPLY_LOOPBACK_SUPPRESS[i]);

        lpbk_sup->GLORT_MASK = test_in->lpbk_suppress.glort_mask;
        lpbk_sup->GLORT      = test_in->lpbk_suppress.glort;
#else
        fm_uint64 cm_lpbk_suppress_reg = 0;

        FM_SET_FIELD64(cm_lpbk_suppress_reg, MBY_CM_APPLY_LOOPBACK_SUPPRESS, GLORT_MASK, test_in->lpbk_suppress.glort_mask);
        FM_SET_FIELD64(cm_lpbk_suppress_reg, MBY_CM_APPLY_LOOPBACK_SUPPRESS, GLORT, test_in->lpbk_suppress.glort);

        mbyModelWriteCSR64(regs, MBY_CM_APPLY_LOOPBACK_SUPPRESS(i, 0), cm_lpbk_suppress_reg);
#endif
    }
    /* Set GLORT_CAM/GLORT_RAM registers. */
    int index = 1;
#ifdef USE_NEW_CSRS
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
#else
    fm_uint64 glort_cam_reg = 0;
    FM_SET_FIELD64(glort_cam_reg, MBY_GLORT_CAM, KEY_INVERT, test_in->glort_cam_ram.key_invert);
    FM_SET_FIELD64(glort_cam_reg, MBY_GLORT_CAM, KEY, test_in->glort_cam_ram.key);
    mbyModelWriteCSR64(regs, MBY_GLORT_CAM(index, 0), glort_cam_reg);

    fm_uint64 glort_ram_reg = 0;
    FM_SET_BIT64  (glort_ram_reg, MBY_GLORT_RAM, SKIP_DGLORT_DEC, test_in->glort_cam_ram.skip_dglort_dec);
    FM_SET_BIT64  (glort_ram_reg, MBY_GLORT_RAM, HASH_ROTATION, test_in->glort_cam_ram.hash_rotation);
    FM_SET_FIELD64(glort_ram_reg, MBY_GLORT_RAM, DEST_COUNT, test_in->glort_cam_ram.dest_count);
    FM_SET_FIELD64(glort_ram_reg, MBY_GLORT_RAM, RANGE_SUB_INDEX_A, test_in->glort_cam_ram.range_sub_index_a);
    FM_SET_FIELD64(glort_ram_reg, MBY_GLORT_RAM, RANGE_SUB_INDEX_B, test_in->glort_cam_ram.range_sub_index_b);
    FM_SET_FIELD64(glort_ram_reg, MBY_GLORT_RAM, DEST_INDEX, test_in->glort_cam_ram.dest_index);
    FM_SET_FIELD64(glort_ram_reg, MBY_GLORT_RAM, STRICT, test_in->glort_cam_ram.strict);
    mbyModelWriteCSR64(regs, MBY_GLORT_RAM(index, 0), glort_ram_reg);
#endif

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

#ifdef USE_NEW_CSRS
    /* Temporary use only two proxy registers
     * - GLORT_DIRECT_MAP_DST0 - for DEST_MASK
     * - GLORT_DIRECT_MAP_DST4 - for IP_MULTICAST_INDEX
     * <-- REVISIT!!!
     */
    glort_direct_map_dst0_r * const map_dst0 = &(glort_map->GLORT_DIRECT_MAP_DST0);
    glort_direct_map_dst4_r * const map_dst4 = &(glort_map->GLORT_DIRECT_MAP_DST4);

    map_dst4->IP_MULTICAST_INDEX = test_in->glort_map.ip_multicast_index;
    map_dst0->DEST_MASK          = test_in->glort_map.dest_mask;
#else
    fm_uint64 glort_dest_table_reg = 0;
    FM_SET_FIELD64(glort_dest_table_reg, MBY_GLORT_DEST_TABLE, IP_MULTICAST_INDEX, test_in->glort_map.ip_multicast_index);
    FM_SET_FIELD64(glort_dest_table_reg, MBY_GLORT_DEST_TABLE, DEST_MASK, test_in->glort_map.dest_mask);
    mbyModelWriteCSR64(regs, MBY_GLORT_DEST_TABLE(dest_index, 0), glort_dest_table_reg);
#endif
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

    if (maskgenToTriggers->DMASK != test_data_out->dmask)
        return FALSE;

    if (maskgenToTriggers->ACTION != test_data_out->action)
        return FALSE;

    return TRUE;
}

static void maskgen_run_test(maskgen_test_data * const test_data)
{
#ifdef USE_NEW_CSRS
    mby_ppe_fwd_misc_map  fwd_misc;
    mby_ppe_mst_glort_map glort_map;
    mby_ppe_cm_apply_map  cm_apply;
#else
    fm_uint32 *           regs;
#endif

    mbyNextHopToMaskGen  nexthopToMaskgen = { 0 };
    mbyMaskGenToTriggers out              = { 0 };

#ifdef USE_NEW_CSRS
    maskgen_default_reg_setup(&fwd_misc, &glort_map, &cm_apply);

    maskgen_test_setup(&fwd_misc, &glort_map, &cm_apply, &nexthopToMaskgen, &(test_data->in));

    MaskGen(&fwd_misc, &glort_map, &cm_apply, &nexthopToMaskgen, &out);

#else
    regs = calloc(MBY_REGISTER_ARRAY_SIZE, sizeof(fm_uint32));

    mbyModelLoadDefaults(regs);

    maskgen_test_setup(regs, &nexthopToMaskgen, &(test_data->in));

    MaskGen(regs, &nexthopToMaskgen, &out);

    free(regs);
#endif

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
