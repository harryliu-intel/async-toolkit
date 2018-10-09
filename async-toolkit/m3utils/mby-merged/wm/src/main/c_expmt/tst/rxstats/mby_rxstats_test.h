#ifndef MBY_RXSTATS_TEST_H
#define MBY_RXSTATS_TEST_H

#include "mby_rxstats.h"
#include "mby_maskgen.h" // action codes

typedef struct mby_rxstats_test_data_in_struct
{
    /* RX stats input data. */
    fm_uint32  rx_length;
    fm_uint32  rx_port;
    fm_bool    is_ipv4;
    fm_bool    is_ipv6;
    fm_macaddr l2_dmac;
    fm_uint16  l2_ivlan1_cnt;
    fm_byte    traffic_class;
    fm_uint    action;

} mby_rxstats_test_data_in;

typedef struct mby_rxstats_test_data_out_struct
{
    fm_uint16 index0;
    fm_uint64 exp_val0;
    fm_uint16 index1;
    fm_uint64 exp_val1;
    fm_uint16 index2;
    fm_uint64 exp_val2;
    fm_uint16 index3;
    fm_uint64 exp_val3;
    fm_uint16 vlan_index;
    fm_uint64 exp_vlan_val;

} mby_rxstats_test_data_out;

typedef struct rxstats_test_data_struct
{
    mby_rxstats_test_data_in  in;
    mby_rxstats_test_data_out out;
    char *                    name;

} rxstats_test_data;

rxstats_test_data rxstats_tests[] =
{
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 1,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_NORMAL,
        },
        .out =
        {
            .index0        = 16,
            .exp_val0      = 1,
            .index1        = 16,
            .exp_val1      = 1,
            .index2        = 16,
            .exp_val2      = 1,
            .index3        = 0,
            .exp_val3      = 0,
            .vlan_index    = 4,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_NORMAL"
    },
    {
        .in =
        {
            .rx_length     = 128,
            .rx_port       = 2,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x011122334455,
            .l2_ivlan1_cnt = 2,
            .traffic_class = 2,
            .action        = MBY_ACTION_FLOOD,
        },
        .out =
        {
            .index0        = 33,
            .exp_val0      = 1,
            .index1        = 34,
            .exp_val1      = 1,
            .index2        = 33,
            .exp_val2      = 1,
            .index3        = 0,
            .exp_val3      = 0,
            .vlan_index    = 9,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP mcast TC 2 ACTION_FLOOD"
    },
    {
        .in =
        {
            .rx_length     = 256,
            .rx_port       = 3,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0xFFFFFFFFFFFF,
            .l2_ivlan1_cnt = 4,
            .traffic_class = 4,
            .action        = MBY_ACTION_SPECIAL,
        },
        .out =
        {
            .index0        = 50,
            .exp_val0      = 1,
            .index1        = 52,
            .exp_val1      = 1,
            .index2        = 50,
            .exp_val2      = 1,
            .index3        = 0,
            .exp_val3      = 0,
            .vlan_index    = 18,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP bcast TC 4 ACTION_SPECIAL"
    },
    {
        .in =
        {
            .rx_length     = 512,
            .rx_port       = 4,
            .is_ipv4       = TRUE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 6,
            .traffic_class = 6,
            .action        = MBY_ACTION_DROP_PARITY,
        },
        .out =
        {
            .index0        = 67,
            .exp_val0      = 1,
            .index1        = 70,
            .exp_val1      = 1,
            .index2        = 68,
            .exp_val2      = 1,
            .index3        = 0,
            .exp_val3      = 0,
            .vlan_index    = 27,
            .exp_vlan_val  = 1,
        },
        .name = "IPv4   ucast TC 6 ACTION_DROP_PARITY"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 4,
            .is_ipv4       = TRUE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x011122334455,
            .l2_ivlan1_cnt = 2,
            .traffic_class = 1,
            .action        = MBY_ACTION_DROP_CONTROL,
        },
        .out =
        {
            .index0        = 68,
            .exp_val0      = 1,
            .index1        = 65,
            .exp_val1      = 1,
            .index2        = 70,
            .exp_val2      = 1,
            .index3        = 0,
            .exp_val3      = 0,
            .vlan_index    = 11,
            .exp_vlan_val  = 1,
        },
        .name = "IPv4   mcast TC 1 ACTION_DROP_CONTROL"
    },
    {
        .in =
        {
            .rx_length     = 128,
            .rx_port       = 3,
            .is_ipv4       = TRUE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0xFFFFFFFFFFFF,
            .l2_ivlan1_cnt = 2,
            .traffic_class = 3,
            .action        = MBY_ACTION_MARKER_ERROR_DROPS,
        },
        .out =
        {
            .index0        = 53,
            .exp_val0      = 1,
            .index1        = 51,
            .exp_val1      = 1,
            .index2        = 57,
            .exp_val2      = 1,
            .index3        = 0,
            .exp_val3      = 0,
            .vlan_index    = 11,
            .exp_vlan_val  = 1,
        },
        .name = "IPv4   bcast TC 3 ACTION_MARKER_ERROR_DROPS"
    },
    {
        .in =
        {
            .rx_length     = 1024,
            .rx_port       = 7,
            .is_ipv4       = FALSE,
            .is_ipv6       = TRUE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 15,
            .traffic_class = 7,
            .action        = MBY_ACTION_DROP_IV,
        },
        .out =
        {
            .index0        = 118,
            .exp_val0      = 1,
            .index1        = 119,
            .exp_val1      = 1,
            .index2        = 122,
            .exp_val2      = 1,
            .index3        = 0,
            .exp_val3      = 0,
            .vlan_index    = 63,
            .exp_vlan_val  = 1,
        },
        .name = "IPv6   ucast TC 7 ACTION_DROP_IV"
    },
    {
        .in =
        {
            .rx_length     = 1024,
            .rx_port       = 5,
            .is_ipv4       = FALSE,
            .is_ipv6       = TRUE,
            .l2_dmac       = 0xFFFFFFFFFFFF,
            .l2_ivlan1_cnt = 8,
            .traffic_class = 5,
            .action        = MBY_ACTION_TRAP,
        },
        .out =
        {
            .index0        = 88,
            .exp_val0      = 1,
            .index1        = 85,
            .exp_val1      = 1,
            .index2        = 85,
            .exp_val2      = 1,
            .index3        = 0,
            .exp_val3      = 0,
            .vlan_index    = 34,
            .exp_vlan_val  = 1,
        },
        .name = "IPv6   bcast TC 5 ACTION_TRAP"
    },
    {
        .in =
        {
            .rx_length     = 1536,
            .rx_port       = 6,
            .is_ipv4       = FALSE,
            .is_ipv6       = TRUE,
            .l2_dmac       = 0x010203040506,
            .l2_ivlan1_cnt = 10,
            .traffic_class = 0,
            .action        = MBY_ACTION_DROP_TRIG,
        },
        .out =
        {
            .index0        = 103,
            .exp_val0      = 1,
            .index1        = 96,
            .exp_val1      = 1,
            .index2        = 110,
            .exp_val2      = 1,
            .index3        = 0,
            .exp_val3      = 0,
            .vlan_index    = 43,
            .exp_vlan_val  = 1,
        },
        .name = "IPv6   mcast TC 0 ACTION_DROP_TRIG"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 1,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_DROP_PARSE,
        },
        .out =
        {
            .index0        = 16,
            .exp_val0      = 1,
            .index1        = 16,
            .exp_val1      = 1,
            .index2        = 19,
            .exp_val2      = 1,
            .index3        = 0,
            .exp_val3      = 0,
            .vlan_index    = 7,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_DROP_PARSE"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 1,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_DROP_STP,
        },
        .out =
        {
            .index0        = 16,
            .exp_val0      = 1,
            .index1        = 16,
            .exp_val1      = 1,
            .index2        = 23,
            .exp_val2      = 1,
            .index3        = 0,
            .exp_val3      = 0,
            .vlan_index    = 7,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_DROP_STP"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 1,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_DROP_SV,
        },
        .out =
        {
            .index0        = 16,
            .exp_val0      = 1,
            .index1        = 16,
            .exp_val1      = 1,
            .index2        = 24,
            .exp_val2      = 1,
            .index3        = 0,
            .exp_val3      = 0,
            .vlan_index    = 7,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_DROP_SV"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 1,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_DROP_EV,
        },
        .out =
        {
            .index0        = 16,
            .exp_val0      = 1,
            .index1        = 16,
            .exp_val1      = 1,
            .index2        = 27,
            .exp_val2      = 1,
            .index3        = 0,
            .exp_val3      = 0,
            .vlan_index    = 7,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_DROP_EV"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 1,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_DROP_CAM,
        },
        .out =
        {
            .index0        = 16,
            .exp_val0      = 1,
            .index1        = 16,
            .exp_val1      = 1,
            .index2        = 28,
            .exp_val2      = 1,
            .index3        = 0,
            .exp_val3      = 0,
            .vlan_index    = 7,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_DROP_CAM"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 1,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_DROP_FFU,
        },
        .out =
        {
            .index0        = 16,
            .exp_val0      = 1,
            .index1        = 16,
            .exp_val1      = 1,
            .index2        = 29,
            .exp_val2      = 1,
            .index3        = 0,
            .exp_val3      = 0,
            .vlan_index    = 7,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_DROP_FFU"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 1,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_DROP_L3_PYLD_LEN,
        },
        .out =
        {
            .index0        = 16,
            .exp_val0      = 1,
            .index1        = 16,
            .exp_val1      = 1,
            .index2        = 31,
            .exp_val2      = 1,
            .index3        = 0,
            .exp_val3      = 0,
            .vlan_index    = 7,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_DROP_L3_PYLD_LEN"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 0,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_DROP_POLICER,
        },
        .out =
        {
            .index0        = 0,
            .exp_val0      = 1,
            .index1        = 0,
            .exp_val1      = 1,
            .index2        = 0,
            .exp_val2      = 0,
            .index3        = 0,
            .exp_val3      = 1,
            .vlan_index    = 7,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_DROP_POLICER"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 0,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_DROP_TTL,
        },
        .out =
        {
            .index0        = 0,
            .exp_val0      = 1,
            .index1        = 0,
            .exp_val1      = 1,
            .index2        = 0,
            .exp_val2      = 0,
            .index3        = 1,
            .exp_val3      = 1,
            .vlan_index    = 7,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_DROP_TTL"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 0,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_DROP_CM_GLOBAL,
        },
        .out =
        {
            .index0        = 0,
            .exp_val0      = 1,
            .index1        = 0,
            .exp_val1      = 1,
            .index2        = 0,
            .exp_val2      = 0,
            .index3        = 2,
            .exp_val3      = 1,
            .vlan_index    = 7,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_DROP_CM_GLOBAL"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 0,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_DROP_CM_SMP0,
        },
        .out =
        {
            .index0        = 0,
            .exp_val0      = 1,
            .index1        = 0,
            .exp_val1      = 1,
            .index2        = 0,
            .exp_val2      = 0,
            .index3        = 3,
            .exp_val3      = 1,
            .vlan_index    = 7,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_DROP_CM_SMP0"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 0,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_DROP_CM_SMP1,
        },
        .out =
        {
            .index0        = 0,
            .exp_val0      = 1,
            .index1        = 0,
            .exp_val1      = 1,
            .index2        = 0,
            .exp_val2      = 0,
            .index3        = 4,
            .exp_val3      = 1,
            .vlan_index    = 7,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_DROP_CM_SMP1"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 0,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_DROP_CM_RX_HOG0,
        },
        .out =
        {
            .index0        = 0,
            .exp_val0      = 1,
            .index1        = 0,
            .exp_val1      = 1,
            .index2        = 0,
            .exp_val2      = 0,
            .index3        = 5,
            .exp_val3      = 1,
            .vlan_index    = 7,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_DROP_CM_RX_HOG0"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 0,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_DROP_CM_RX_HOG1,
        },
        .out =
        {
            .index0        = 0,
            .exp_val0      = 1,
            .index1        = 0,
            .exp_val1      = 1,
            .index2        = 0,
            .exp_val2      = 0,
            .index3        = 6,
            .exp_val3      = 1,
            .vlan_index    = 7,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_DROP_CM_RX_HOG1"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 0,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_DROP_CM_TX_HOG0,
        },
        .out =
        {
            .index0        = 0,
            .exp_val0      = 1,
            .index1        = 0,
            .exp_val1      = 1,
            .index2        = 0,
            .exp_val2      = 0,
            .index3        = 7,
            .exp_val3      = 1,
            .vlan_index    = 7,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_DROP_CM_TX_HOG0"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 0,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_DROP_CM_TX_HOG1,
        },
        .out =
        {
            .index0        = 0,
            .exp_val0      = 1,
            .index1        = 0,
            .exp_val1      = 1,
            .index2        = 0,
            .exp_val2      = 0,
            .index3        = 8,
            .exp_val3      = 1,
            .vlan_index    = 7,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_DROP_CM_TX_HOG1"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 0,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_DROP_FRAME_ERR,
        },
        .out =
        {
            .index0        = 0,
            .exp_val0      = 1,
            .index1        = 0,
            .exp_val1      = 1,
            .index2        = 0,
            .exp_val2      = 0,
            .index3        = 9,
            .exp_val3      = 1,
            .vlan_index    = 7,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_DROP_FRAME_ERR"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 0,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_REDIRECT_TRIG,
        },
        .out =
        {
            .index0        = 0,
            .exp_val0      = 1,
            .index1        = 0,
            .exp_val1      = 1,
            .index2        = 0,
            .exp_val2      = 0,
            .index3        = 10,
            .exp_val3      = 1,
            .vlan_index    = 4,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_REDIRECT_TRIG"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 0,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_DROP_DLF,
        },
        .out =
        {
            .index0        = 0,
            .exp_val0      = 1,
            .index1        = 0,
            .exp_val1      = 1,
            .index2        = 0,
            .exp_val2      = 0,
            .index3        = 11,
            .exp_val3      = 1,
            .vlan_index    = 7,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_DROP_DLF"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 0,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_GLORT_FORWARDED,
        },
        .out =
        {
            .index0        = 0,
            .exp_val0      = 1,
            .index1        = 0,
            .exp_val1      = 1,
            .index2        = 0,
            .exp_val2      = 0,
            .index3        = 12,
            .exp_val3      = 1,
            .vlan_index    = 4,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_GLORT_FORWARDED"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 0,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_BANK5_OTHER_DROPS,
        },
        .out =
        {
            .index0        = 0,
            .exp_val0      = 1,
            .index1        = 0,
            .exp_val1      = 1,
            .index2        = 0,
            .exp_val2      = 0,
            .index3        = 13,
            .exp_val3      = 1,
            .vlan_index    = 7,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_BANK5_OTHER_DROPS"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 0,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_DROP_LOOPBACK,
        },
        .out =
        {
            .index0        = 0,
            .exp_val0      = 1,
            .index1        = 0,
            .exp_val1      = 1,
            .index2        = 0,
            .exp_val2      = 0,
            .index3        = 14,
            .exp_val3      = 1,
            .vlan_index    = 7,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_DROP_LOOPBACK"
    },
    {
        .in =
        {
            .rx_length     = 64,
            .rx_port       = 0,
            .is_ipv4       = FALSE,
            .is_ipv6       = FALSE,
            .l2_dmac       = 0x001122334455,
            .l2_ivlan1_cnt = 1,
            .traffic_class = 0,
            .action        = MBY_ACTION_DROP_L4_CSUM,
        },
        .out =
        {
            .index0        = 0,
            .exp_val0      = 1,
            .index1        = 0,
            .exp_val1      = 1,
            .index2        = 0,
            .exp_val2      = 0,
            .index3        = 15,
            .exp_val3      = 1,
            .vlan_index    = 7,
            .exp_vlan_val  = 1,
        },
        .name = "Non-IP ucast TC 0 ACTION_DROP_L4_CSUM"
    },
};
#endif
