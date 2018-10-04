#ifndef MBY_NEXTHOP_TEST_H
#define MBY_NEXTHOP_TEST_H

#include "mby_maskgen.h"

typedef struct port_cfg_1_struct
{
    fm_bool           learning_enable;
    fm_bool           filter_vlan_ingress;
    fm_uint32         destination_mask;

} port_cfg_1;

typedef struct port_cfg_2_struct
{
    fm_uint32         destination_mask;

} port_cfg_2;

typedef struct sys_cfg_1_struct
{
    fm_bool           store_trap_action;
    fm_bool           drop_mac_ctrl_ethertype;
    fm_bool           drop_invalid_smac;
    fm_bool           enable_trap_plus_log;
    fm_bool           trap_mtu_violations;

} sys_cfg_1;

typedef struct fwd_cpu_mac_struct
{
    fm_macaddr        cpu_mac_addr;
} fwd_cpu_mac;

typedef struct fwd_lag_cfg_struct
{
    fm_bool           in_lag;
    fm_bool           hash_rotation;
    fm_byte           index;
    fm_byte           lag_size;
} fwd_lag_cfg;

typedef struct lpbk_suppress_struct
{
    fm_uint16 glort_mask;
    fm_uint16 glort;
} lpbk_suppress;

typedef struct glort_cam_ram_struct
{
    fm_uint16         key_invert;
    fm_uint16         key;
    fm_bool           skip_dglort_dec;
    fm_bool           hash_rotation;
    fm_byte           dest_count;
    fm_byte           range_sub_index_b;
    fm_byte           range_sub_index_a;
    fm_uint16         dest_index;
    mbyGlortRamStrict strict;

} glort_cam_ram;

typedef struct glort_map_struct
{
    fm_uint16         ip_multicast_index;
    fm_uint32         dest_mask;
} glort_map;

typedef struct mby_maskgen_test_data_in_struct
{
    /* Maskgen input data. */
    fm_macaddr        l2_smac;
    fm_macaddr        l2_dmac;
    fm_uint16         idglort;
    fm_uint32         glort_dmask_in;
    fm_uint16         l2_ivid1;
    fm_uint16         l2_evid1;
    fm_bool           l2_ivlan1_membership;
    fm_uint32         l2_evlan1_membership;
    mbyStpState       l2_ifid1_state;
    fm_uint32         l2_efid1_state;
    fm_uint16         l2_edomain_in;
    fm_uint64         amask;
    fm_bool           mark_routed;
    fm_uint32         hash_rot_a;
    fm_uint32         hash_rot_b;
    fm_uint32         rx_port;
    fm_bool           parser_error;
    fm_bool           trap_igmp;
    fm_bool           parity_error;
    fm_bool           mtu_violation;
    fm_uint16         csglort;

    /* Input data for FWD_PORT_CFG_1 register. */
    port_cfg_1        port_cfg_1;

    /* Input data for FWD_PORT_CFG_2 register. */
    port_cfg_2        port_cfg_2;

    /* Input data for FWD_SYS_CFG_1 register. */
    sys_cfg_1         sys_cfg_1;

    /* Input data for FWD_CPU_MAC register. */
    fwd_cpu_mac       fwd_cpu_mac;

    /* Input data for CM_APPLY_LOOPBACK_SUPPRESS register. */
    lpbk_suppress     lpbk_suppress;

    /* Input data for FWD_LAG_CFG register. */
    fwd_lag_cfg       fwd_lag_cfg;

    /* Input data for GLORT_CAM/GLORT_RAM registers. */
    glort_cam_ram     glort_cam_ram;

    /* GLORT_DIRECT_MAP register. */
    glort_map         glort_map;

} mby_maskgen_test_data_in;

typedef struct mby_maskgen_test_data_out_struct
{
    fm_bool   glort_cam_miss;
    fm_bool   strict_glort_routing;
    fm_bool   targeted_deterministic;
    fm_bool   skip_dglort_dec;
    fm_uint16 ip_mcast_idx;
    fm_bool   learning_enabled;
    fm_uint64 amask;
    fm_byte   log_amask;
    fm_bool   store_trap_action;
    fm_uint32 dmask;
    fm_uint32 action;

} mby_maskgen_test_data_out;

typedef struct maskgen_test_data_struct
{
    mby_maskgen_test_data_in  in;
    mby_maskgen_test_data_out out;
    char *                    name;

} maskgen_test_data;

maskgen_test_data maskgen_tests[] =
{
    {
        .in =
        {
            .l2_smac                      = 0x001122334455,
            .l2_dmac                      = 0x0022446688AA,
            .idglort                      = 0x100,
            .glort_dmask_in               = 0xFFFF,
            .l2_ivid1                     = 1,
            .l2_evid1                     = 1,
            .l2_ivlan1_membership         = TRUE,
            .l2_evlan1_membership         = 0xFFFF,
            .l2_ifid1_state               = MBY_STP_STATE_FORWARD,
            .l2_efid1_state               = 0xFFFF,
            .l2_edomain_in                = 0,
            .amask                        = 0,
            .mark_routed                  = TRUE,
            .port_cfg_1 =
            {
                .learning_enable          = TRUE,
                .filter_vlan_ingress      = TRUE,
                .destination_mask         = 0xFFFF,
            },
            .port_cfg_2 =
            {
                .destination_mask         = 0xFFFF,
            },
            .sys_cfg_1 =
             {
                 .store_trap_action       = TRUE,
                 .drop_mac_ctrl_ethertype = TRUE,
                 .drop_invalid_smac       = TRUE,
                 .enable_trap_plus_log    = TRUE,
                 .trap_mtu_violations     = TRUE,
             },
             .lpbk_suppress =
             {
                 .glort_mask              = 0xFFFF,
                 .glort                   = 0x50,
             },
             .glort_cam_ram =
             {
                 .key                     = 0xFFFF,
                 .key_invert              = 0xFFFF,
                 .skip_dglort_dec         = FALSE,
                 .strict                  = MBY_GLORT_RAM_STRICT_DETERMINISTIC,
             },
        },
        .out =
        {
            .glort_cam_miss               = TRUE,
            .strict_glort_routing         = FALSE,
            .targeted_deterministic       = FALSE,
            .skip_dglort_dec              = FALSE,
            .ip_mcast_idx                 = 0,
            .learning_enabled             = FALSE,
            .amask                        = ( MBY_AMASK_DROP_CAM_MISS |
                                              MBY_AMASK_LOG_ARP_REDIRECT ),
            .log_amask                    = MBY_LOG_TYPE_ARP_REDIRECT,
            .store_trap_action            = TRUE,
            .dmask                        = 0,
            .action                       = MBY_ACTION_DROP_LOOPBACK,
        },
        .name = "GLORT CAM miss log_amask ARP_REDIRECT"
    },
    {
        .in =
        {
            .rx_port                      = 1,
            .l2_smac                      = 0x001122334455,
            .l2_dmac                      = 0x0022446688AA,
            .idglort                      = 0x200,
            .glort_dmask_in               = 0,
            .l2_ivid1                     = 1,
            .l2_evid1                     = 1,
            .l2_ivlan1_membership         = TRUE,
            .l2_evlan1_membership         = 0xFFFF,
            .l2_ifid1_state               = MBY_STP_STATE_FORWARD,
            .l2_efid1_state               = 0xFFFF,
            .l2_edomain_in                = 0,
            .amask                        = 0,
            .mark_routed                  = FALSE,
            .hash_rot_a                   = 0,
            .hash_rot_b                   = 0,
            .port_cfg_1 =
            {
                .learning_enable          = TRUE,
                .filter_vlan_ingress      = TRUE,
                .destination_mask         = 0xFFFF,
            },
            .port_cfg_2 =
            {
                .destination_mask         = 0xFFFF,
            },
            .sys_cfg_1 =
            {
                .store_trap_action       = TRUE,
                .drop_mac_ctrl_ethertype = TRUE,
                .drop_invalid_smac       = TRUE,
                .enable_trap_plus_log    = TRUE,
                .trap_mtu_violations     = TRUE,
            },
            .lpbk_suppress =
            {
                .glort_mask              = 0xFFFF,
                .glort                   = 0x50,
            },
            .glort_cam_ram =
            {
                .key                     = 0x200,
                .key_invert              = ~0x200,
                .dest_index              = 1,
                .skip_dglort_dec         = TRUE,
            },
            .glort_map =
            {
                .dest_mask               = 0xFFFF,
            },
        },
        .out =
        {
            .glort_cam_miss              = FALSE,
            .strict_glort_routing        = FALSE,
            .targeted_deterministic      = FALSE,
            .skip_dglort_dec             = TRUE,
            .ip_mcast_idx                = 0,
            .learning_enabled            = TRUE,
            .amask                       = MBY_AMASK_FORWARD_NORMAL,
            .log_amask                   = 0,
            .store_trap_action           = TRUE,
            .dmask                       = 0xFFFD,
            .action                      = MBY_ACTION_NORMAL,
        },
        .name = "GLORT CAM hit mask FORWARD_NORMAL"
    },
    {
        .in =
        {
            .rx_port                      = 1,
            .l2_smac                      = 0x001122334455,
            .l2_dmac                      = 0x0022446688AA,
            .idglort                      = 0x200,
            .glort_dmask_in               = 0,
            .l2_ivid1                     = 1,
            .l2_evid1                     = 1,
            .l2_ivlan1_membership         = TRUE,
            .l2_evlan1_membership         = 0xFFFF,
            .l2_ifid1_state               = MBY_STP_STATE_FORWARD,
            .l2_efid1_state               = 0xFFFF,
            .l2_edomain_in                = 0,
            .amask                        = 0,
            .mark_routed                  = FALSE,
            .hash_rot_a                   = 0,
            .hash_rot_b                   = 0,
            .port_cfg_1 =
            {
                .learning_enable          = TRUE,
                .filter_vlan_ingress      = TRUE,
                .destination_mask         = 0x0,
            },
            .port_cfg_2 =
            {
                .destination_mask         = 0x0,
            },
            .sys_cfg_1 =
            {
                .store_trap_action       = TRUE,
                .drop_mac_ctrl_ethertype = TRUE,
                .drop_invalid_smac       = TRUE,
                .enable_trap_plus_log    = TRUE,
                .trap_mtu_violations     = TRUE,
            },
            .lpbk_suppress =
            {
                .glort_mask              = 0xFFFF,
                .glort                   = 0x50,
            },
            .glort_cam_ram =
            {
                .key                     = 0x200,
                .key_invert              = ~0x200,
                .dest_index              = 1,
            },
            .glort_map =
            {
                .dest_mask               = 0xFFFF,
            },
        },
        .out =
        {
            .glort_cam_miss              = FALSE,
            .strict_glort_routing        = FALSE,
            .targeted_deterministic      = FALSE,
            .ip_mcast_idx                = 0,
            .learning_enabled            = TRUE,
            .amask                       = MBY_AMASK_DROP_LOOPBACK,
            .log_amask                   = 0,
            .store_trap_action           = TRUE,
            .dmask                       = 0,
            .action                      = MBY_ACTION_DROP_LOOPBACK,
        },
        .name = "GLORT CAM hit dmask 0 amask DROP_LOOPBACK"
    },
    {
        .in =
        {
            .rx_port                      = 1,
            .l2_smac                      = 0x001122334455,
            .l2_dmac                      = 0x0022446688AA,
            .idglort                      = 0x200,
            .glort_dmask_in               = 0,
            .l2_ivid1                     = 1,
            .l2_evid1                     = 1,
            .l2_ivlan1_membership         = TRUE,
            .l2_evlan1_membership         = 0xFFFF,
            .l2_ifid1_state               = MBY_STP_STATE_FORWARD,
            .l2_efid1_state               = 0xFFFF,
            .l2_edomain_in                = 0,
            .amask                        = 0,
            .mark_routed                  = FALSE,
            .hash_rot_a                   = 0,
            .hash_rot_b                   = 0,
            .port_cfg_1 =
            {
                .learning_enable          = TRUE,
                .filter_vlan_ingress      = TRUE,
                .destination_mask         = 0xFFFF,
            },
            .port_cfg_2 =
            {
                .destination_mask         = 0xFFFF,
            },
            .sys_cfg_1 =
            {
                .store_trap_action       = TRUE,
                .drop_mac_ctrl_ethertype = TRUE,
                .drop_invalid_smac       = TRUE,
                .enable_trap_plus_log    = TRUE,
                .trap_mtu_violations     = TRUE,
            },
            .lpbk_suppress =
            {
                .glort_mask              = 0xFFFF,
                .glort                   = 0x50,
            },
            .glort_cam_ram =
            {
                .key                     = 0x200,
                .key_invert              = ~0x200,
                .dest_index              = 1,
                .strict                  = MBY_GLORT_RAM_STRICT_TARGETED_DETERMINISTIC,
            },
            .glort_map =
            {
                .dest_mask               = 0xFFFF,
                .ip_multicast_index      = 1,
            },
        },
        .out =
        {
            .glort_cam_miss              = FALSE,
            .strict_glort_routing        = TRUE,
            .targeted_deterministic      = TRUE,
            .ip_mcast_idx                = 1,
            .learning_enabled            = FALSE,
            .amask                       = MBY_AMASK_SPECIAL,
            .log_amask                   = 0,
            .store_trap_action           = TRUE,
            .dmask                       = 0xFFFF,
            .action                      = MBY_ACTION_SPECIAL,
        },
        .name = "GLORT CAM hit TARGETED_DETERMINISTIC 0 amask SPECIAL"
    },
    {
        .in =
        {
            .rx_port                      = 1,
            .l2_smac                      = 0x001122334455,
            .l2_dmac                      = 0x0022446688AA,
            .idglort                      = 0x200,
            .l2_ivid1                     = 1,
            .l2_evid1                     = 1,
            .l2_ivlan1_membership         = TRUE,
            .l2_evlan1_membership         = 0xFFFF,
            .l2_ifid1_state               = MBY_STP_STATE_FORWARD,
            .l2_efid1_state               = 0xFFFF,
            .l2_edomain_in                = 0,
            .glort_dmask_in               = 0,
            .parser_error                 = TRUE,
            .port_cfg_1 =
            {
                .learning_enable          = TRUE,
                .filter_vlan_ingress      = TRUE,
                .destination_mask         = 0xFFFF,
            },
            .port_cfg_2 =
            {
                .destination_mask         = 0xFFFF,
            },
            .sys_cfg_1 =
            {
                .store_trap_action       = TRUE,
                .drop_mac_ctrl_ethertype = TRUE,
                .drop_invalid_smac       = TRUE,
                .enable_trap_plus_log    = TRUE,
                .trap_mtu_violations     = TRUE,
            },
            .lpbk_suppress =
            {
                .glort_mask              = 0xFFFF,
                .glort                   = 0x50,
            },
            .glort_cam_ram =
            {
                .key                     = 0xFFFF,
                .key_invert              = 0xFFFF,
                .skip_dglort_dec         = FALSE,
            },
        },
        .out =
        {
            .glort_cam_miss              = TRUE,
            .targeted_deterministic      = FALSE,
            .learning_enabled            = FALSE,
            .amask                       = MBY_AMASK_DROP_PARSER_ERR,
            .log_amask                   = 0,
            .store_trap_action           = TRUE,
            .dmask                       = 0,
            .action                      = MBY_ACTION_DROP_LOOPBACK,
        },
        .name = "GLORT CAM miss PARSER_ERROR"
    },
    {
        .in =
        {
            .rx_port                      = 1,
            .l2_smac                      = 0x001122334455,
            .l2_dmac                      = 0x0022446688AA,
            .idglort                      = 0x200,
            .l2_ivid1                     = 1,
            .l2_evid1                     = 1,
            .l2_ivlan1_membership         = TRUE,
            .l2_evlan1_membership         = 0xFFFF,
            .l2_ifid1_state               = MBY_STP_STATE_FORWARD,
            .l2_efid1_state               = 0xFFFF,
            .l2_edomain_in                = 0,
            .glort_dmask_in               = 0,
            .parser_error                 = FALSE,
            .trap_igmp                    = TRUE,
            .port_cfg_1 =
            {
                .learning_enable          = TRUE,
                .filter_vlan_ingress      = TRUE,
                .destination_mask         = 0xFFFF,
            },
            .port_cfg_2 =
            {
                .destination_mask         = 0xFFFF,
            },
            .sys_cfg_1 =
            {
                .store_trap_action       = TRUE,
                .drop_mac_ctrl_ethertype = TRUE,
                .drop_invalid_smac       = TRUE,
                .enable_trap_plus_log    = TRUE,
                .trap_mtu_violations     = TRUE,
            },
            .lpbk_suppress =
            {
                .glort_mask              = 0xFFFF,
                .glort                   = 0x50,
            },
            .glort_cam_ram =
            {
                .key                     = 0x200,
                .key_invert              = ~0x200,
                .dest_index              = 1,
                .strict                  = MBY_GLORT_RAM_STRICT_DETERMINISTIC,
            },
            .glort_map =
            {
                .dest_mask               = 0xFFFF,
            },
        },
        .out =
        {
            .glort_cam_miss              = FALSE,
            .strict_glort_routing        = TRUE,
            .targeted_deterministic      = FALSE,
            .learning_enabled            = TRUE,
            .amask                       = MBY_AMASK_TRAP_IGMP,
            .log_amask                   = 0,
            .store_trap_action           = TRUE,
            .dmask                       = 0,
            .action                      = MBY_ACTION_DROP_LOOPBACK,
        },
        .name = "GLORT CAM hit TRAP_IGMP"
    },
    {
        .in =
        {
            .rx_port                      = 1,
            .l2_smac                      = 0x001122334455,
            .l2_dmac                      = 0x0022446688AA,
            .idglort                      = 0x200,
            .l2_ivid1                     = 1,
            .l2_evid1                     = 1,
            .l2_ivlan1_membership         = TRUE,
            .l2_evlan1_membership         = 0xFFFF,
            .l2_ifid1_state               = MBY_STP_STATE_FORWARD,
            .l2_efid1_state               = 0xFFFF,
            .l2_edomain_in                = 0,
            .glort_dmask_in               = 0,
            .parser_error                 = FALSE,
            .parity_error                 = TRUE,
            .port_cfg_1 =
            {
                .learning_enable          = TRUE,
                .filter_vlan_ingress      = TRUE,
                .destination_mask         = 0xFFFF,
            },
            .port_cfg_2 =
            {
                .destination_mask         = 0xFFFF,
            },
            .sys_cfg_1 =
            {
                .store_trap_action       = TRUE,
                .drop_mac_ctrl_ethertype = TRUE,
                .drop_invalid_smac       = TRUE,
                .enable_trap_plus_log    = TRUE,
                .trap_mtu_violations     = TRUE,
            },
            .lpbk_suppress =
            {
                .glort_mask              = 0xFFFF,
                .glort                   = 0x50,
            },
            .glort_cam_ram =
            {
                .key                     = 0x200,
                .key_invert              = ~0x200,
                .dest_index              = 1,
                .strict                  = MBY_GLORT_RAM_STRICT_DETERMINISTIC,
            },
            .glort_map =
            {
                .dest_mask               = 0xFFFF,
            },
        },
        .out =
        {
            .glort_cam_miss              = FALSE,
            .strict_glort_routing        = TRUE,
            .targeted_deterministic      = FALSE,
            .learning_enabled            = FALSE,
            .amask                       = MBY_AMASK_DROP_PERR,
            .log_amask                   = 0,
            .store_trap_action           = TRUE,
            .dmask                       = 0,
            .action                      = MBY_ACTION_DROP_LOOPBACK,
        },
        .name = "GLORT CAM hit DROP_PARITY_ERR"
    },
    {
        .in =
        {
            .rx_port                      = 1,
            .l2_smac                      = 0x111122334455,
            .l2_dmac                      = 0x0022446688AA,
            .idglort                      = 0x200,
            .l2_ivid1                     = 1,
            .l2_evid1                     = 1,
            .l2_ivlan1_membership         = TRUE,
            .l2_evlan1_membership         = 0xFFFF,
            .l2_ifid1_state               = MBY_STP_STATE_FORWARD,
            .l2_efid1_state               = 0xFFFF,
            .l2_edomain_in                = 0,
            .glort_dmask_in               = 0,
            .parser_error                 = FALSE,
            .port_cfg_1 =
            {
                .learning_enable          = TRUE,
                .filter_vlan_ingress      = TRUE,
                .destination_mask         = 0xFFFF,
            },
            .port_cfg_2 =
            {
                .destination_mask         = 0xFFFF,
            },
            .sys_cfg_1 =
            {
                .store_trap_action       = TRUE,
                .drop_mac_ctrl_ethertype = TRUE,
                .drop_invalid_smac       = TRUE,
                .enable_trap_plus_log    = TRUE,
                .trap_mtu_violations     = TRUE,
            },
            .lpbk_suppress =
            {
                .glort_mask              = 0xFFFF,
                .glort                   = 0x50,
            },
            .glort_cam_ram =
            {
                .key                     = 0x200,
                .key_invert              = ~0x200,
                .dest_index              = 1,
                .strict                  = MBY_GLORT_RAM_STRICT_DETERMINISTIC,
            },
            .glort_map =
            {
                .dest_mask               = 0xFFFF,
            },
        },
        .out =
        {
            .glort_cam_miss              = FALSE,
            .strict_glort_routing        = TRUE,
            .targeted_deterministic      = FALSE,
            .learning_enabled            = FALSE,
            .amask                       = MBY_AMASK_DROP_SMAC,
            .log_amask                   = 0,
            .store_trap_action           = TRUE,
            .dmask                       = 0,
            .action                      = MBY_ACTION_DROP_LOOPBACK,
        },
        .name = "GLORT CAM hit DROP_INVALID_SMAC"
    },
    {
        .in =
        {
            .rx_port                      = 1,
            .l2_smac                      = 0x001122334455,
            .l2_dmac                      = 0x0022446688AA,
            .idglort                      = 0x200,
            .l2_ivid1                     = 1,
            .l2_evid1                     = 1,
            .l2_ivlan1_membership         = TRUE,
            .l2_evlan1_membership         = 0xFFFF,
            .l2_ifid1_state               = MBY_STP_STATE_FORWARD,
            .l2_efid1_state               = 0xFFFF,
            .l2_edomain_in                = 0,
            .glort_dmask_in               = 0,
            .parser_error                 = FALSE,
            .port_cfg_1 =
            {
                .learning_enable          = TRUE,
                .filter_vlan_ingress      = TRUE,
                .destination_mask         = 0xFFFF,
            },
            .port_cfg_2 =
            {
                .destination_mask         = 0xFFFF,
            },
            .sys_cfg_1 =
            {
                .store_trap_action       = TRUE,
                .drop_mac_ctrl_ethertype = TRUE,
                .drop_invalid_smac       = TRUE,
                .enable_trap_plus_log    = TRUE,
                .trap_mtu_violations     = TRUE,
            },
            .fwd_cpu_mac =
            {
                .cpu_mac_addr             = 0x0022446688AA,
            },
            .lpbk_suppress =
            {
                .glort_mask              = 0xFFFF,
                .glort                   = 0x50,
            },
            .glort_cam_ram =
            {
                .key                     = 0x200,
                .key_invert              = ~0x200,
                .dest_index              = 1,
                .strict                  = MBY_GLORT_RAM_STRICT_DETERMINISTIC,
            },
            .glort_map =
            {
                .dest_mask               = 0xFFFF,
            },
        },
        .out =
        {
            .glort_cam_miss              = FALSE,
            .strict_glort_routing        = TRUE,
            .targeted_deterministic      = FALSE,
            .learning_enabled            = TRUE,
            .amask                       = MBY_AMASK_TRAP_CPU_ADDR,
            .log_amask                   = 0,
            .store_trap_action           = TRUE,
            .dmask                       = 0,
            .action                      = MBY_ACTION_DROP_LOOPBACK,
        },
        .name = "GLORT CAM hit TRAP_CPU"
    },
    {
        .in =
        {
            .rx_port                      = 1,
            .l2_smac                      = 0x001122334455,
            .l2_dmac                      = 0x0022446688AA,
            .idglort                      = 0x200,
            .l2_ivid1                     = 1,
            .l2_evid1                     = 1,
            .l2_ivlan1_membership         = TRUE,
            .l2_evlan1_membership         = 0xFFFF,
            .l2_ifid1_state               = MBY_STP_STATE_FORWARD,
            .l2_efid1_state               = 0xFFFF,
            .l2_edomain_in                = 0,
            .glort_dmask_in               = 0,
            .parser_error                 = FALSE,
            .mark_routed                  = TRUE,
            .mtu_violation                = TRUE,
            .port_cfg_1 =
            {
                .learning_enable          = TRUE,
                .filter_vlan_ingress      = TRUE,
                .destination_mask         = 0xFFFF,
            },
            .port_cfg_2 =
            {
                .destination_mask         = 0xFFFF,
            },
            .sys_cfg_1 =
            {
                .store_trap_action       = TRUE,
                .drop_mac_ctrl_ethertype = TRUE,
                .drop_invalid_smac       = TRUE,
                .enable_trap_plus_log    = TRUE,
                .trap_mtu_violations     = TRUE,
            },
            .lpbk_suppress =
            {
                .glort_mask              = 0xFFFF,
                .glort                   = 0x50,
            },
            .glort_cam_ram =
            {
                .key                     = 0x200,
                .key_invert              = ~0x200,
                .dest_index              = 1,
                .strict                  = MBY_GLORT_RAM_STRICT_DETERMINISTIC,
            },
            .glort_map =
            {
                .dest_mask               = 0xFFFF,
                .ip_multicast_index      = 1,
            },
        },
        .out =
        {
            .glort_cam_miss              = FALSE,
            .strict_glort_routing        = TRUE,
            .targeted_deterministic      = FALSE,
            .learning_enabled            = TRUE,
            .amask                       = MBY_AMASK_TRAP_MTU_VIO,
            .log_amask                   = 0,
            .store_trap_action           = TRUE,
            .dmask                       = 0,
            .action                      = MBY_ACTION_DROP_LOOPBACK,
            .ip_mcast_idx                = 1,
        },
        .name = "GLORT CAM hit TRAP_MTU_VIOLATION"
    },

};
#endif
