#ifndef MBY_TRIGGERS_TEST_H
#define MBY_TRIGGERS_TEST_H

#include <fm_types.h>
#include <mby_dmask_regs.h>
#include <mby_log_type.h>

typedef struct sys_cfg_1_struct
{
    fm_bool           store_trap_action;
    fm_bool           drop_mac_ctrl_ethertype;
    fm_bool           drop_invalid_smac;
    fm_bool           enable_trap_plus_log;
    fm_bool           trap_mtu_violations;

} sys_cfg_1;

typedef struct fwd_lag_cfg_struct
{
    fm_bool           in_lag;
    fm_bool           hash_rotation;
    fm_byte           index;
    fm_byte           lag_size;
} fwd_lag_cfg;

typedef struct lpbk_suppress_struct
{
    fm_uint16         glort_mask;
    fm_uint16         glort;
} lpbk_suppress;


typedef struct mby_maskgen_test_data_in_struct
{
    /* Triggers input data */
    fm_uint32     action;
    fm_uint64     amask;
    //CPU_CODE
    //CPU_TRAP
    fm_uint16     csglort;
    //DA_HIT
    fm_uint64     dmask[MBY_DMASK_REGISTERS];
    //DROP_TTL
    //FCLASS
    fm_bool       glort_cam_miss;
    fm_uint64     glort_dmask_in[MBY_DMASK_REGISTERS];
    fm_uint32     hash_rot_a;
    fm_uint32     hash_rot_b;
    fm_uint16     idglort;
    fm_uint16     ip_mcast_idx;
    //IS_IPV4
    //IS_IPV6
    fm_macaddr    l2_dmac;
    fm_uint16     l2_edomain_in;
    fm_uint16     l2_evid1;
    fm_macaddr    l2_smac;
    //L3_EDOMAIN
    fm_bool       learning_enabled;
    //LOGGING_HIT
    fm_byte       log_amask;
    //MAC_MOVED
    fm_bool       mark_routed;
    //MCAST_EPOCH
    //MIRROR0_PORT
    //MIRROR0_PROFILE_IDX
    //MIRROR0_PROFILE_V
    //MIRROR1_PORT
    //MIRROR1_PROFILE_IDX
    //MIRROR1_PROFILE_V
    //OPERATOR_ID
    //PA_L3LEN_ERR;
    //PRE_RESOLVE_ACTION;
    //PRE_RESOLVE_DGLORT;
    //PRE_RESOLVE_DMASK[MBY_DMASK_REGISTERS];
    //QCN_MIRROR0_PROFILE_V;
    //QCN_MIRROR1_PROFILE_V;
    //QOS_TC;
    //RX_LENGTH;
    //RX_MIRROR;
    fm_uint32     rx_port;
    //SEG_META_ERR;
    //SKIP_DGLORT_DEC;
    //STORE_TRAP_ACTION;
    //STRICT_GLORT_ROUTING;
    fm_bool       targeted_deterministic;
    //XCAST;

    /* Input data for FWD_SYS_CFG_1 register. */
    sys_cfg_1     sys_cfg_1;

    /* Input data for CM_APPLY_LOOPBACK_SUPPRESS register. */
    lpbk_suppress lpbk_suppress;

    /* Input data for FWD_LAG_CFG register. */
    fwd_lag_cfg   fwd_lag_cfg;

} mby_triggers_test_data_in;

typedef struct mby_maskgen_test_data_out_struct
{
    fm_uint32 action;

    fm_uint64 dmask[MBY_DMASK_REGISTERS];

    fm_bool   learning_enabled;

} mby_triggers_test_data_out;

typedef struct maskgen_test_data_struct
{
    mby_triggers_test_data_in  in;
    mby_triggers_test_data_out out;
    char *                     name;

} triggers_test_data;

triggers_test_data triggers_tests[] =
{
    {
        .in =
        {
            .action                       = MBY_ACTION_DROP_CAM,
            .amask                        = ( MBY_AMASK_DROP_CAM_MISS |
                                              MBY_AMASK_LOG_ARP_REDIRECT ),
            .csglort                      = 0,
            .dmask                        = { 0x0 },
            .glort_dmask_in               = { 0xFFFF },
            .idglort                      = 0x100,
            .ip_mcast_idx                 = 0,
            .l2_dmac                      = 0x0022446688AA,
            .l2_edomain_in                = 0,
            .l2_evid1                     = 1,
            .l2_smac                      = 0x001122334455,
            .learning_enabled             = FALSE,
            .log_amask                    = MBY_LOG_TYPE_ARP_REDIRECT,
            .mark_routed                  = TRUE,
            .targeted_deterministic       = FALSE,
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
        },
        .out =
        {
            .action                       = MBY_ACTION_DROP_LOOPBACK,
            .dmask                        = { 0x0 },
            .learning_enabled             = FALSE,

        },
        .name = "GLORT CAM miss log_amask ARP_REDIRECT"
    },
    {
        .in =
        {
            .action                       = MBY_ACTION_NORMAL,
            .amask                        = MBY_AMASK_FORWARD_NORMAL,
            .csglort                      = 0,
            .dmask                        = { 0xFFFD },
            .glort_dmask_in               = { 0 },
            .hash_rot_a                   = 0,
            .hash_rot_b                   = 0,
            .idglort                      = 0x200,
            .ip_mcast_idx                 = 0,
            .l2_dmac                      = 0x0022446688AA,
            .l2_edomain_in                = 0,
            .l2_evid1                     = 1,
            .l2_smac                      = 0x001122334455,
            .learning_enabled             = TRUE,
            .log_amask                    = 0,
            .mark_routed                  = FALSE,
            .rx_port                      = 1,
            .targeted_deterministic       = FALSE,
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
        },
        .out =
        {
            .action                       = MBY_ACTION_NORMAL,
            .dmask                        = { 0xFFFD },
            .learning_enabled             = TRUE,
        },
        .name = "GLORT CAM hit mask FORWARD_NORMAL"
    },
    {
        .in =
        {
            .action                       = MBY_ACTION_DROP_LOOPBACK,
            .amask                        = MBY_AMASK_DROP_LOOPBACK,
            .csglort                      = 0,
            .dmask                        = { 0 },
            .glort_dmask_in               = { 0 },
            .hash_rot_a                   = 0,
            .hash_rot_b                   = 0,
            .idglort                      = 0x200,
            .ip_mcast_idx                 = 0,
            .l2_dmac                      = 0x0022446688AA,
            .l2_edomain_in                = 0,
            .l2_evid1                     = 1,
            .l2_smac                      = 0x001122334455,
            .learning_enabled             = TRUE,
            .log_amask                    = 0,
            .mark_routed                  = FALSE,
            .rx_port                      = 1,
            .targeted_deterministic       = FALSE,
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
        },
        .out =
        {
            .action                      = MBY_ACTION_DROP_LOOPBACK,
            .dmask                       = { 0 },
            .learning_enabled            = TRUE,
        },
        .name = "GLORT CAM hit dmask 0 amask DROP_LOOPBACK"
    },
    {
        .in =
        {
            .action                       = MBY_ACTION_SPECIAL,
            .amask                        = MBY_AMASK_SPECIAL,
            .csglort                      = 0,
            .dmask                        = { 0xFFFF },
            .glort_dmask_in               = { 0 },
            .hash_rot_a                   = 0,
            .hash_rot_b                   = 0,
            .idglort                      = 0x200,
            .ip_mcast_idx                 = 1,
            .l2_dmac                      = 0x0022446688AA,
            .l2_edomain_in                = 0,
            .l2_evid1                     = 1,
            .l2_smac                      = 0x001122334455,
            .learning_enabled             = FALSE,
            .log_amask                    = 0,
            .mark_routed                  = FALSE,
            .rx_port                      = 1,
            .targeted_deterministic      = TRUE,
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
        },
        .out =
        {
            .action                      = MBY_ACTION_SPECIAL,
            .dmask                       = { 0xFFFF },
            .learning_enabled            = FALSE,
        },
        .name = "GLORT CAM hit TARGETED_DETERMINISTIC 0 amask SPECIAL"
    },
    {
        .in =
        {
            .action                       = MBY_ACTION_DROP_PARSE,
            .amask                        = MBY_AMASK_DROP_PARSER_ERR,
            .csglort                      = 0,
            .dmask                        = { 0 },
            .glort_dmask_in               = { 0 },
            .idglort                      = 0x200,
            .l2_dmac                      = 0x0022446688AA,
            .l2_edomain_in                = 0,
            .l2_evid1                     = 1,
            .l2_smac                      = 0x001122334455,
            .learning_enabled             = FALSE,
            .log_amask                    = 0,
            .rx_port                      = 1,
            .targeted_deterministic       = FALSE,
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
        },
        .out =
        {
            .action                      = MBY_ACTION_DROP_LOOPBACK,
            .dmask                       = { 0 },
            .learning_enabled            = FALSE,
        },
        .name = "GLORT CAM miss PARSER_ERROR"
    },
    {
        .in =
        {
            .action                       = MBY_ACTION_TRAP,
            .amask                        = MBY_AMASK_TRAP_IGMP,
            .csglort                      = 0,
            .dmask                        = { 0 },
            .glort_dmask_in               = { 0 },
            .idglort                      = 0x200,
            .l2_dmac                      = 0x0022446688AA,
            .l2_edomain_in                = 0,
            .l2_evid1                     = 1,
            .l2_smac                      = 0x001122334455,
            .learning_enabled            = TRUE,
            .log_amask                    = 0,
            .rx_port                      = 1,
            .targeted_deterministic       = FALSE,
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
        },
        .out =
        {
            .action                      = MBY_ACTION_DROP_LOOPBACK,
            .dmask                       = { 0 },
            .learning_enabled            = TRUE,
        },
        .name = "GLORT CAM hit TRAP_IGMP"
    },
    {
        .in =
        {
            .action                       = MBY_ACTION_DROP_PARITY,
            .amask                        = MBY_AMASK_DROP_PERR,
            .csglort                      = 0,
            .dmask                        = { 0 },
            .glort_dmask_in               = { 0 },
            .idglort                      = 0x200,
            .l2_dmac                      = 0x0022446688AA,
            .l2_edomain_in                = 0,
            .l2_evid1                     = 1,
            .l2_smac                      = 0x001122334455,
            .learning_enabled             = FALSE,
            .log_amask                    = 0,
            .rx_port                      = 1,
            .targeted_deterministic       = FALSE,
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
        },
        .out =
        {
            .action                      = MBY_ACTION_DROP_LOOPBACK,
            .dmask                       = { 0 },
            .learning_enabled            = FALSE,
        },
        .name = "GLORT CAM hit DROP_PARITY_ERR"
    },
    {
        .in =
        {
            .action                       = MBY_ACTION_BANK5_OTHER_DROPS,
            .amask                        = MBY_AMASK_DROP_SMAC,
            .csglort                      = 0,
            .dmask                        = { 0 },
            .glort_dmask_in               = { 0 },
            .idglort                      = 0x200,
            .l2_dmac                      = 0x0022446688AA,
            .l2_evid1                     = 1,
            .l2_edomain_in                = 0,
            .l2_smac                      = 0x111122334455,
            .learning_enabled             = FALSE,
            .log_amask                    = 0,
            .rx_port                      = 1,
            .targeted_deterministic       = FALSE,
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
        },
        .out =
        {
            .action                      = MBY_ACTION_DROP_LOOPBACK,
            .dmask                       = { 0 },
            .learning_enabled            = FALSE,
        },
        .name = "GLORT CAM hit DROP_INVALID_SMAC"
    },
    {
        .in =
        {
            .action                       = MBY_ACTION_TRAP,
            .amask                        = MBY_AMASK_TRAP_CPU_ADDR,
            .csglort                      = 0,
            .dmask                        = { 0 },
            .glort_dmask_in               = { 0 },
            .idglort                      = 0x200,
            .l2_dmac                      = 0x0022446688AA,
            .l2_edomain_in                = 0,
            .l2_evid1                     = 1,
            .l2_smac                      = 0x001122334455,
            .learning_enabled             = TRUE,
            .log_amask                    = 0,
            .rx_port                      = 1,
            .targeted_deterministic      = FALSE,
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
        },
        .out =
        {
            .action                      = MBY_ACTION_DROP_LOOPBACK,
            .dmask                       = { 0 },
            .learning_enabled            = TRUE,
        },
        .name = "GLORT CAM hit TRAP_CPU"
    },
    {
        .in =
        {
            .action                       = MBY_ACTION_TRAP,
            .amask                        = MBY_AMASK_TRAP_MTU_VIO,
            .csglort                      = 0,
            .dmask                        = { 0 },
            .glort_dmask_in               = { 0 },
            .idglort                      = 0x200,
            .ip_mcast_idx                 = 1,
            .l2_dmac                      = 0x0022446688AA,
            .l2_edomain_in                = 0,
            .l2_evid1                     = 1,
            .l2_smac                      = 0x001122334455,
            .learning_enabled             = TRUE,
            .log_amask                    = 0,
            .mark_routed                  = TRUE,
            .rx_port                      = 1,
            .targeted_deterministic       = FALSE,
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
        },
        .out =
        {
            .action                      = MBY_ACTION_DROP_LOOPBACK,
            .dmask                       = { 0 },
            .learning_enabled            = TRUE,
        },
        .name = "GLORT CAM hit TRAP_MTU_VIOLATION"
    },
};

#endif
