# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Chip/FM2000/Registers.pm
# Creation Date:    09/26/07
# Description:      
#
# INTEL CONFIDENTIAL
# Copyright 2007 - 2011 Intel Corporation. All Rights Reserved. 
#
# The source code contained or described herein and all documents related
# to the source code ("Material") are owned by Intel Corporation or its
# suppliers or licensors. Title to the Material remains with Intel
# Corporation or its suppliers and licensors. The Material contains trade
# secrets and proprietary and confidential information of Intel or its
# suppliers and licensors. The Material is protected by worldwide copyright
# and trade secret laws and treaty provisions. No part of the Material may
# be used, copied, reproduced, modified, published, uploaded, posted,
# transmitted, distributed, or disclosed in any way without Intel's prior
# express written permission.
#
# No license under any patent, copyright, trade secret or other intellectual
# property right is granted to or conferred upon you by disclosure or 
# delivery of the Materials, either expressly, by implication, inducement,
# estoppel or otherwise. Any license under such intellectual property rights
# must be express and approved by Intel in writing.
###############################################################################

package Chip::FM2000::Registers;
use strict;
use warnings;

use base qw(Exporter);

our @EXPORT = qw(
	FM_APL_ERR_CNT
	FM_BOOT_STATUS
	FM_CHIP_MODE
	FM_CLK_MULT_1
	FM_CM_PRI_MAP_1
	FM_CM_PRI_MAP_2
	FM_CONTEPL_CTRLSTAT
	FM_EGRESS_SCHEDULE_1
	FM_EGRESS_SCHEDULE_2
	FM_EPL_INT_DETECT
	FM_EPL_LED_STATUS
	FM_EPL_PACE_RATE
	FM_EPL_PACE_STAT
	FM_EPL_PACE_WM
	FM_FID_TABLE
	FM_FRAME_CTRL_IM
	FM_FRAME_CTRL_IP
	FM_FRAME_TIME_OUT
	FM_FUSEBOX_1
	FM_FUSEBOX_2
	FM_FUSEBOX_3
	FM_FUSEBOX_4
	FM_GLOBAL_EPL_INT_DETECT
	FM_GLOBAL_PAUSE_WM
	FM_HEADER_MASK
	FM_INTERRUPT_DETECT
	FM_JITTER_WATERMARK
	FM_LCI_CFG
	FM_LCI_IM
	FM_LCI_IP
	FM_LCI_RX_FIFO
	FM_LCI_STATUS
	FM_LCI_TX_FIFO
	FM_LFSR_CFG
	FM_MAC_CFG_1
	FM_MAC_CFG_2
	FM_MAC_CFG_3
	FM_MAC_CFG_4
	FM_MAC_CFG_5
	FM_MAC_CFG_6
	FM_MAC_IM
	FM_MAC_IP
	FM_MAC_STATUS
	FM_MA_TABLE
	FM_MA_TABLE_CFG
	FM_MA_TABLE_STATUS_1
	FM_MA_TABLE_STATUS_2
	FM_MA_TABLE_STATUS_3
	FM_MGR_IM
	FM_MGR_IP
	FM_PCS_CFG_1
	FM_PCS_CFG_2
	FM_PCS_CFG_3
	FM_PCS_CFG_4
	FM_PCS_CFG_5
	FM_PCS_IM
	FM_PCS_IP
	FM_PERR_DEBUG
	FM_PERR_IM
	FM_PERR_IP
	FM_PLL_FH_CTRL
	FM_PLL_FH_STAT
	FM_PORT_CFG_1
	FM_PORT_CFG_2
	FM_PORT_CLK_SEL
	FM_PORT_MAC_SEC_IM
	FM_PORT_MAC_SEC_IP
	FM_PORT_RESET
	FM_PORT_VLAN_IM_1
	FM_PORT_VLAN_IM_2
	FM_PORT_VLAN_IP_1
	FM_PORT_VLAN_IP_2
	FM_QUEUE_CFG_1
	FM_QUEUE_CFG_2
	FM_QUEUE_CFG_3
	FM_QUEUE_CFG_4
	FM_QUEUE_CFG_5
	FM_RMON_RX_JABBER
	FM_RMON_TX_CRC
	FM_RMON_TX_PAUSE
	FM_RX_PAUSE_WM
	FM_RX_PRI_MAP
	FM_SAF_MATRIX
	FM_SCAN_CTRL
	FM_SCAN_DATA_IN
	FM_SCAN_DATA_OUT
	FM_SCAN_FREQ_MULT
	FM_SCAN_SEL
	FM_SCHED_PRI_MAP
	FM_SERDES_BIST_CNT
	FM_SERDES_BIST_ERR_CNT
	FM_SERDES_CTRL_1
	FM_SERDES_CTRL_2
	FM_SERDES_CTRL_3
	FM_SERDES_IM
	FM_SERDES_IP
	FM_SERDES_STATUS
	FM_SERDES_TEST_MODE
	FM_SHADOW_FUSEBOX_1
	FM_SHADOW_FUSEBOX_2
	FM_SHADOW_FUSEBOX_3
	FM_SHADOW_FUSEBOX_4
	FM_SOFT_RESET
	FM_STATS_CFG
	FM_STATS_DROP_COUNT
	FM_STAT_BROADCAST_DROPS
	FM_STAT_CMRX_DROPS
	FM_STAT_DLFDROPS
	FM_STAT_FIDFORWARDED
	FM_STAT_FLOOD_FORWARDED
	FM_STAT_GLOBAL_HIGH_DROP
	FM_STAT_GLOBAL_LOW_DROP
	FM_STAT_GLOBAL_PRIV_DROP
	FM_STAT_RESERVED_TRAPS
	FM_STAT_RX_10240_TO_MAX
	FM_STAT_RX_1024_TO_1522
	FM_STAT_RX_128_TO_255
	FM_STAT_RX_1523_TO_2047
	FM_STAT_RX_2048_TO_4095
	FM_STAT_RX_256_TO_511
	FM_STAT_RX_4096_TO_8191
	FM_STAT_RX_512_TO_1023
	FM_STAT_RX_64_PKTS
	FM_STAT_RX_65_TO_127
	FM_STAT_RX_8192_TO_10239
	FM_STAT_RX_BAD_OCTETS
	FM_STAT_RX_BCST_PKTS
	FM_STAT_RX_FCSERRORS
	FM_STAT_RX_FRAGMENTS
	FM_STAT_RX_GOOD_OCTETS
	FM_STAT_RX_JABBERS
	FM_STAT_RX_MCST_PKTS
	FM_STAT_RX_MINTO_63
	FM_STAT_RX_OCTETS_P0
	FM_STAT_RX_OCTETS_P1
	FM_STAT_RX_OCTETS_P2
	FM_STAT_RX_OCTETS_P3
	FM_STAT_RX_OCTETS_P4
	FM_STAT_RX_OCTETS_P5
	FM_STAT_RX_OCTETS_P6
	FM_STAT_RX_OCTETS_P7
	FM_STAT_RX_OVERSIZED
	FM_STAT_RX_P0
	FM_STAT_RX_P1
	FM_STAT_RX_P2
	FM_STAT_RX_P3
	FM_STAT_RX_P4
	FM_STAT_RX_P5
	FM_STAT_RX_P6
	FM_STAT_RX_P7
	FM_STAT_RX_PAUSE_PKTS
	FM_STAT_RX_SYMBOL_ERRORS
	FM_STAT_RX_UCST_PKTS
	FM_STAT_RX_UNDERSIZED
	FM_STAT_SECURITY_VIOLATIONS
	FM_STAT_STPDROPS
	FM_STAT_TRIG
	FM_STAT_TRIGGER_DROPS
	FM_STAT_TRIGGER_MIRRORED
	FM_STAT_TX_10240_TO_MAX
	FM_STAT_TX_1024_TO_1522
	FM_STAT_TX_128_TO_255
	FM_STAT_TX_1523_TO_2047
	FM_STAT_TX_2048_TO_4095
	FM_STAT_TX_256_TO_511
	FM_STAT_TX_4096_TO_8191
	FM_STAT_TX_512_TO_1023
	FM_STAT_TX_64_PKTS
	FM_STAT_TX_65_TO_127
	FM_STAT_TX_8192_TO_10239
	FM_STAT_TX_BCST_PKTS
	FM_STAT_TX_BYTECOUNT
	FM_STAT_TX_DROP_PORT
	FM_STAT_TX_ERROR_DROP
	FM_STAT_TX_MCST_PKTS
	FM_STAT_TX_MINTO_63
	FM_STAT_TX_OCTETS
	FM_STAT_TX_TIME_OUT_DROP
	FM_STAT_TX_UCST_PKTS
	FM_STAT_VLAN_EGRESS_BV
	FM_STAT_VLAN_INGRESS_BV
	FM_STAT_VLAN_TAG_DROPS
	FM_STAT_VLAN_UCST_OCTETS
	FM_STAT_VLAN_UCST_PKTS
	FM_STAT_VLAN_XCST_OCTETS
	FM_STAT_VLAN_XCST_PKTS
	FM_STREAM_STATUS_1
	FM_STREAM_STATUS_2
	FM_SWITCH_ERR_CNT
	FM_SYS_CFG_1
	FM_SYS_CFG_2
	FM_SYS_CFG_3
	FM_SYS_CFG_4
	FM_SYS_CFG_6
	FM_SYS_CFG_7
	FM_TRIGGER_CFG
	FM_TRIGGER_IM
	FM_TRIGGER_IP
	FM_TRIGGER_PRI
	FM_TRIGGER_RX
	FM_TRIGGER_TX
	FM_TRUNK_CANONICAL
	FM_TRUNK_GROUP_1
	FM_TRUNK_GROUP_2
	FM_TRUNK_GROUP_3
	FM_TRUNK_HASH_MASK
	FM_TRUNK_PORT_MAP
	FM_TX_PRI_MAP_1
	FM_TX_PRI_MAP_2
	FM_VID_TABLE
	FM_VITAL_PRODUCT_DATA

);

our $FM_APL_ERR_CNT_OFFSET = 1;
sub FM_APL_ERR_CNT_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_APL_ERR_CNT_WIDTH = 1;
sub FM_APL_ERR_CNT
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8025 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_BOOT_STATUS_WIDTH = 1;
sub FM_BOOT_STATUS
{
    my ($self) = @_;
    return (0x0);
}

our $FM_CHIP_MODE_WIDTH = 1;
sub FM_CHIP_MODE
{
    my ($self) = @_;
    return (0x301);
}

our $FM_CLK_MULT_1_WIDTH = 1;
sub FM_CLK_MULT_1
{
    my ($self) = @_;
    return (0x302);
}

our $FM_CM_PRI_MAP_1_WIDTH = 1;
sub FM_CM_PRI_MAP_1
{
    my ($self) = @_;
    return (0x64000);
}

our $FM_CM_PRI_MAP_2_WIDTH = 1;
sub FM_CM_PRI_MAP_2
{
    my ($self) = @_;
    return (0x64001);
}

our $FM_CONTEPL_CTRLSTAT_WIDTH = 1;
sub FM_CONTEPL_CTRLSTAT
{
    my ($self) = @_;
    return (0x311);
}

our $FM_EGRESS_SCHEDULE_1_OFFSET = 0;
sub FM_EGRESS_SCHEDULE_1_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_EGRESS_SCHEDULE_1_WIDTH = 1;
sub FM_EGRESS_SCHEDULE_1
{
    my ($self, $index) = @_;
    return (( 0x2040 + ( 2 * $index ) ));
}

our $FM_EGRESS_SCHEDULE_2_OFFSET = 0;
sub FM_EGRESS_SCHEDULE_2_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_EGRESS_SCHEDULE_2_WIDTH = 1;
sub FM_EGRESS_SCHEDULE_2
{
    my ($self, $index) = @_;
    return (( 0x2041 + ( 2 * $index ) ));
}

our $FM_EPL_INT_DETECT_OFFSET = 1;
sub FM_EPL_INT_DETECT_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_EPL_INT_DETECT_WIDTH = 1;
sub FM_EPL_INT_DETECT
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x802b * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_EPL_LED_STATUS_OFFSET = 1;
sub FM_EPL_LED_STATUS_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_EPL_LED_STATUS_WIDTH = 1;
sub FM_EPL_LED_STATUS
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x802a * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_EPL_PACE_RATE_OFFSET = 1;
sub FM_EPL_PACE_RATE_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_EPL_PACE_RATE_WIDTH = 1;
sub FM_EPL_PACE_RATE
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8018 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_EPL_PACE_STAT_OFFSET = 1;
sub FM_EPL_PACE_STAT_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_EPL_PACE_STAT_WIDTH = 1;
sub FM_EPL_PACE_STAT
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8019 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_EPL_PACE_WM_OFFSET_1 = 0;
sub FM_EPL_PACE_WM_ENTRIES_1
{
    my ($self) = @_;
    return 8;
}
our $FM_EPL_PACE_WM_OFFSET_0 = 0;
sub FM_EPL_PACE_WM_ENTRIES_0
{
    my ($self) = @_;
    return 24;
}
our $FM_EPL_PACE_WM_WIDTH = 1;
sub FM_EPL_PACE_WM
{
    my ($self, $n, $index) = @_;
    return (( 0x0 + ( 0x8010 + $n ) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_FID_TABLE_OFFSET = 0;
sub FM_FID_TABLE_ENTRIES
{
    my ($self) = @_;
    return 4096;
}
our $FM_FID_TABLE_WIDTH = 2;
sub FM_FID_TABLE
{
    my ($self, $index, $word) = @_;
    return (( 0x52000 + ( $index * 2 ) + $word ));
}

our $FM_FRAME_CTRL_IM_WIDTH = 1;
sub FM_FRAME_CTRL_IM
{
    my ($self) = @_;
    return (0x30e);
}

our $FM_FRAME_CTRL_IP_WIDTH = 1;
sub FM_FRAME_CTRL_IP
{
    my ($self) = @_;
    return (0x30d);
}

our $FM_FRAME_TIME_OUT_WIDTH = 1;
sub FM_FRAME_TIME_OUT
{
    my ($self) = @_;
    return (0x303);
}

our $FM_FUSEBOX_1_WIDTH = 1;
sub FM_FUSEBOX_1
{
    my ($self) = @_;
    return (0x305);
}

our $FM_FUSEBOX_2_WIDTH = 1;
sub FM_FUSEBOX_2
{
    my ($self) = @_;
    return (0x306);
}

our $FM_FUSEBOX_3_WIDTH = 1;
sub FM_FUSEBOX_3
{
    my ($self) = @_;
    return (0x307);
}

our $FM_FUSEBOX_4_WIDTH = 1;
sub FM_FUSEBOX_4
{
    my ($self) = @_;
    return (0x308);
}

our $FM_GLOBAL_EPL_INT_DETECT_WIDTH = 1;
sub FM_GLOBAL_EPL_INT_DETECT
{
    my ($self) = @_;
    return (0x30a);
}

our $FM_GLOBAL_PAUSE_WM_OFFSET = 0;
sub FM_GLOBAL_PAUSE_WM_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_GLOBAL_PAUSE_WM_WIDTH = 1;
sub FM_GLOBAL_PAUSE_WM
{
    my ($self, $index) = @_;
    return (( 0x64080 + $index ));
}

our $FM_HEADER_MASK_OFFSET = -1;
sub FM_HEADER_MASK_ENTRIES
{
    my ($self) = @_;
    return -1;
}
our $FM_HEADER_MASK_WIDTH = 4;
sub FM_HEADER_MASK
{
    my ($self, $index) = @_;
    return (( 0x58110 + ( $index * 1 ) ));
}

our $FM_INTERRUPT_DETECT_WIDTH = 1;
sub FM_INTERRUPT_DETECT
{
    my ($self) = @_;
    return (0x309);
}

our $FM_JITTER_WATERMARK_WIDTH = 1;
sub FM_JITTER_WATERMARK
{
    my ($self) = @_;
    return (0x20fc);
}

our $FM_LCI_CFG_WIDTH = 1;
sub FM_LCI_CFG
{
    my ($self) = @_;
    return (0x4005);
}

our $FM_LCI_IM_WIDTH = 1;
sub FM_LCI_IM
{
    my ($self) = @_;
    return (0x4003);
}

our $FM_LCI_IP_WIDTH = 1;
sub FM_LCI_IP
{
    my ($self) = @_;
    return (0x4002);
}

our $FM_LCI_RX_FIFO_WIDTH = 1;
sub FM_LCI_RX_FIFO
{
    my ($self) = @_;
    return (0x4000);
}

our $FM_LCI_STATUS_WIDTH = 1;
sub FM_LCI_STATUS
{
    my ($self) = @_;
    return (0x4004);
}

our $FM_LCI_TX_FIFO_WIDTH = 1;
sub FM_LCI_TX_FIFO
{
    my ($self) = @_;
    return (0x4001);
}

our $FM_LFSR_CFG_WIDTH = 1;
sub FM_LFSR_CFG
{
    my ($self) = @_;
    return (0x64002);
}

our $FM_MAC_CFG_1_OFFSET = 1;
sub FM_MAC_CFG_1_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_MAC_CFG_1_WIDTH = 1;
sub FM_MAC_CFG_1
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x801a * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_MAC_CFG_2_OFFSET = 1;
sub FM_MAC_CFG_2_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_MAC_CFG_2_WIDTH = 1;
sub FM_MAC_CFG_2
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x801b * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_MAC_CFG_3_OFFSET = 1;
sub FM_MAC_CFG_3_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_MAC_CFG_3_WIDTH = 1;
sub FM_MAC_CFG_3
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x801c * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_MAC_CFG_4_OFFSET = 1;
sub FM_MAC_CFG_4_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_MAC_CFG_4_WIDTH = 1;
sub FM_MAC_CFG_4
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x801d * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_MAC_CFG_5_OFFSET = 1;
sub FM_MAC_CFG_5_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_MAC_CFG_5_WIDTH = 1;
sub FM_MAC_CFG_5
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x801e * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_MAC_CFG_6_OFFSET = 1;
sub FM_MAC_CFG_6_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_MAC_CFG_6_WIDTH = 1;
sub FM_MAC_CFG_6
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x801f * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_MAC_IM_OFFSET = 1;
sub FM_MAC_IM_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_MAC_IM_WIDTH = 1;
sub FM_MAC_IM
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8024 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_MAC_IP_OFFSET = 1;
sub FM_MAC_IP_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_MAC_IP_WIDTH = 1;
sub FM_MAC_IP
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8023 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_MAC_STATUS_OFFSET = 1;
sub FM_MAC_STATUS_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_MAC_STATUS_WIDTH = 1;
sub FM_MAC_STATUS
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8022 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_MA_TABLE_OFFSET = 0;
sub FM_MA_TABLE_ENTRIES
{
    my ($self) = @_;
    return 16384;
}
our $FM_MA_TABLE_WIDTH = 3;
sub FM_MA_TABLE
{
    my ($self, $index, $word) = @_;
    return (( 0x10000 + ( $index * 4 ) + $word ));
}

our $FM_MA_TABLE_CFG_WIDTH = 1;
sub FM_MA_TABLE_CFG
{
    my ($self) = @_;
    return (0x58120);
}

our $FM_MA_TABLE_STATUS_1_WIDTH = 1;
sub FM_MA_TABLE_STATUS_1
{
    my ($self) = @_;
    return (0x58000);
}

our $FM_MA_TABLE_STATUS_2_WIDTH = 1;
sub FM_MA_TABLE_STATUS_2
{
    my ($self) = @_;
    return (0x58001);
}

our $FM_MA_TABLE_STATUS_3_WIDTH = 1;
sub FM_MA_TABLE_STATUS_3
{
    my ($self) = @_;
    return (0x310);
}

our $FM_MGR_IM_WIDTH = 1;
sub FM_MGR_IM
{
    my ($self) = @_;
    return (0x30c);
}

our $FM_MGR_IP_WIDTH = 1;
sub FM_MGR_IP
{
    my ($self) = @_;
    return (0x30b);
}

our $FM_PCS_CFG_1_OFFSET = 1;
sub FM_PCS_CFG_1_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_PCS_CFG_1_WIDTH = 1;
sub FM_PCS_CFG_1
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8009 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_PCS_CFG_2_OFFSET = 1;
sub FM_PCS_CFG_2_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_PCS_CFG_2_WIDTH = 1;
sub FM_PCS_CFG_2
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x800a * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_PCS_CFG_3_OFFSET = 1;
sub FM_PCS_CFG_3_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_PCS_CFG_3_WIDTH = 1;
sub FM_PCS_CFG_3
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x800b * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_PCS_CFG_4_OFFSET = 1;
sub FM_PCS_CFG_4_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_PCS_CFG_4_WIDTH = 1;
sub FM_PCS_CFG_4
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x800c * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_PCS_CFG_5_OFFSET = 1;
sub FM_PCS_CFG_5_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_PCS_CFG_5_WIDTH = 1;
sub FM_PCS_CFG_5
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x800d * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_PCS_IM_OFFSET = 1;
sub FM_PCS_IM_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_PCS_IM_WIDTH = 1;
sub FM_PCS_IM
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x800f * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_PCS_IP_OFFSET = 1;
sub FM_PCS_IP_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_PCS_IP_WIDTH = 1;
sub FM_PCS_IP
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x800e * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_PERR_DEBUG_WIDTH = 1;
sub FM_PERR_DEBUG
{
    my ($self) = @_;
    return (0x314);
}

our $FM_PERR_IM_WIDTH = 1;
sub FM_PERR_IM
{
    my ($self) = @_;
    return (0x313);
}

our $FM_PERR_IP_WIDTH = 1;
sub FM_PERR_IP
{
    my ($self) = @_;
    return (0x312);
}

our $FM_PLL_FH_CTRL_WIDTH = 1;
sub FM_PLL_FH_CTRL
{
    my ($self) = @_;
    return (0x315);
}

our $FM_PLL_FH_STAT_WIDTH = 1;
sub FM_PLL_FH_STAT
{
    my ($self) = @_;
    return (0x316);
}

our $FM_PORT_CFG_1_OFFSET = 0;
sub FM_PORT_CFG_1_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_PORT_CFG_1_WIDTH = 1;
sub FM_PORT_CFG_1
{
    my ($self, $index) = @_;
    return (( 0x54000 + $index ));
}

our $FM_PORT_CFG_2_OFFSET = 0;
sub FM_PORT_CFG_2_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_PORT_CFG_2_WIDTH = 1;
sub FM_PORT_CFG_2
{
    my ($self, $index) = @_;
    return (( 0x60060 + $index ));
}

our $FM_PORT_CLK_SEL_WIDTH = 1;
sub FM_PORT_CLK_SEL
{
    my ($self) = @_;
    return (0x317);
}

our $FM_PORT_MAC_SEC_IM_WIDTH = 1;
sub FM_PORT_MAC_SEC_IM
{
    my ($self) = @_;
    return (0x640c5);
}

our $FM_PORT_MAC_SEC_IP_WIDTH = 1;
sub FM_PORT_MAC_SEC_IP
{
    my ($self) = @_;
    return (0x640c4);
}

our $FM_PORT_RESET_WIDTH = 1;
sub FM_PORT_RESET
{
    my ($self) = @_;
    return (0x318);
}

our $FM_PORT_VLAN_IM_1_WIDTH = 1;
sub FM_PORT_VLAN_IM_1
{
    my ($self) = @_;
    return (0x640c3);
}

our $FM_PORT_VLAN_IM_2_WIDTH = 1;
sub FM_PORT_VLAN_IM_2
{
    my ($self) = @_;
    return (0x640c1);
}

our $FM_PORT_VLAN_IP_1_WIDTH = 1;
sub FM_PORT_VLAN_IP_1
{
    my ($self) = @_;
    return (0x640c2);
}

our $FM_PORT_VLAN_IP_2_WIDTH = 1;
sub FM_PORT_VLAN_IP_2
{
    my ($self) = @_;
    return (0x640c0);
}

our $FM_QUEUE_CFG_1_OFFSET = 0;
sub FM_QUEUE_CFG_1_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_QUEUE_CFG_1_WIDTH = 1;
sub FM_QUEUE_CFG_1
{
    my ($self, $index) = @_;
    return (( 0x64020 + $index ));
}

our $FM_QUEUE_CFG_2_OFFSET = 0;
sub FM_QUEUE_CFG_2_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_QUEUE_CFG_2_WIDTH = 1;
sub FM_QUEUE_CFG_2
{
    my ($self, $index) = @_;
    return (( 0x64040 + $index ));
}

our $FM_QUEUE_CFG_3_WIDTH = 1;
sub FM_QUEUE_CFG_3
{
    my ($self) = @_;
    return (0x65000);
}

our $FM_QUEUE_CFG_4_WIDTH = 1;
sub FM_QUEUE_CFG_4
{
    my ($self) = @_;
    return (0x64003);
}

our $FM_QUEUE_CFG_5_WIDTH = 1;
sub FM_QUEUE_CFG_5
{
    my ($self) = @_;
    return (0x64004);
}

our $FM_RMON_RX_JABBER_OFFSET = 1;
sub FM_RMON_RX_JABBER_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_RMON_RX_JABBER_WIDTH = 1;
sub FM_RMON_RX_JABBER
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8029 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_RMON_TX_CRC_OFFSET = 1;
sub FM_RMON_TX_CRC_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_RMON_TX_CRC_WIDTH = 1;
sub FM_RMON_TX_CRC
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8027 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_RMON_TX_PAUSE_OFFSET = 1;
sub FM_RMON_TX_PAUSE_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_RMON_TX_PAUSE_WIDTH = 1;
sub FM_RMON_TX_PAUSE
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8026 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_RX_PAUSE_WM_OFFSET = 0;
sub FM_RX_PAUSE_WM_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_RX_PAUSE_WM_WIDTH = 1;
sub FM_RX_PAUSE_WM
{
    my ($self, $index) = @_;
    return (( 0x640a0 + $index ));
}

our $FM_RX_PRI_MAP_OFFSET = 0;
sub FM_RX_PRI_MAP_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_RX_PRI_MAP_WIDTH = 1;
sub FM_RX_PRI_MAP
{
    my ($self, $index) = @_;
    return (( 0x60040 + $index ));
}

our $FM_SAF_MATRIX_OFFSET = 0;
sub FM_SAF_MATRIX_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_SAF_MATRIX_WIDTH = 1;
sub FM_SAF_MATRIX
{
    my ($self, $index) = @_;
    return (( 0x650c0 + $index ));
}

our $FM_SCAN_CTRL_WIDTH = 1;
sub FM_SCAN_CTRL
{
    my ($self) = @_;
    return (0x101);
}

our $FM_SCAN_DATA_IN_WIDTH = 1;
sub FM_SCAN_DATA_IN
{
    my ($self) = @_;
    return (0x103);
}

our $FM_SCAN_DATA_OUT_WIDTH = 1;
sub FM_SCAN_DATA_OUT
{
    my ($self) = @_;
    return (0x104);
}

our $FM_SCAN_FREQ_MULT_WIDTH = 1;
sub FM_SCAN_FREQ_MULT
{
    my ($self) = @_;
    return (0x100);
}

our $FM_SCAN_SEL_WIDTH = 1;
sub FM_SCAN_SEL
{
    my ($self) = @_;
    return (0x102);
}

our $FM_SCHED_PRI_MAP_WIDTH = 1;
sub FM_SCHED_PRI_MAP
{
    my ($self) = @_;
    return (0x65001);
}

our $FM_SERDES_BIST_CNT_OFFSET = 1;
sub FM_SERDES_BIST_CNT_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_SERDES_BIST_CNT_WIDTH = 1;
sub FM_SERDES_BIST_CNT
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8007 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_SERDES_BIST_ERR_CNT_OFFSET = 1;
sub FM_SERDES_BIST_ERR_CNT_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_SERDES_BIST_ERR_CNT_WIDTH = 1;
sub FM_SERDES_BIST_ERR_CNT
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8008 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_SERDES_CTRL_1_OFFSET = 1;
sub FM_SERDES_CTRL_1_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_SERDES_CTRL_1_WIDTH = 1;
sub FM_SERDES_CTRL_1
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8000 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_SERDES_CTRL_2_OFFSET = 1;
sub FM_SERDES_CTRL_2_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_SERDES_CTRL_2_WIDTH = 1;
sub FM_SERDES_CTRL_2
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8001 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_SERDES_CTRL_3_OFFSET = 1;
sub FM_SERDES_CTRL_3_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_SERDES_CTRL_3_WIDTH = 1;
sub FM_SERDES_CTRL_3
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8002 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_SERDES_IM_OFFSET = 1;
sub FM_SERDES_IM_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_SERDES_IM_WIDTH = 1;
sub FM_SERDES_IM
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8006 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_SERDES_IP_OFFSET = 1;
sub FM_SERDES_IP_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_SERDES_IP_WIDTH = 1;
sub FM_SERDES_IP
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8005 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_SERDES_STATUS_OFFSET = 1;
sub FM_SERDES_STATUS_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_SERDES_STATUS_WIDTH = 1;
sub FM_SERDES_STATUS
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8004 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_SERDES_TEST_MODE_OFFSET = 1;
sub FM_SERDES_TEST_MODE_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_SERDES_TEST_MODE_WIDTH = 1;
sub FM_SERDES_TEST_MODE
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8003 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_SHADOW_FUSEBOX_1_WIDTH = 1;
sub FM_SHADOW_FUSEBOX_1
{
    my ($self) = @_;
    return (0x319);
}

our $FM_SHADOW_FUSEBOX_2_WIDTH = 1;
sub FM_SHADOW_FUSEBOX_2
{
    my ($self) = @_;
    return (0x31a);
}

our $FM_SHADOW_FUSEBOX_3_WIDTH = 1;
sub FM_SHADOW_FUSEBOX_3
{
    my ($self) = @_;
    return (0x31b);
}

our $FM_SHADOW_FUSEBOX_4_WIDTH = 1;
sub FM_SHADOW_FUSEBOX_4
{
    my ($self) = @_;
    return (0x31c);
}

our $FM_SOFT_RESET_WIDTH = 1;
sub FM_SOFT_RESET
{
    my ($self) = @_;
    return (0x300);
}

our $FM_STATS_CFG_WIDTH = 1;
sub FM_STATS_CFG
{
    my ($self) = @_;
    return (0x66200);
}

our $FM_STATS_DROP_COUNT_WIDTH = 1;
sub FM_STATS_DROP_COUNT
{
    my ($self) = @_;
    return (0x66202);
}

our $FM_STAT_BROADCAST_DROPS_OFFSET = 0;
sub FM_STAT_BROADCAST_DROPS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_BROADCAST_DROPS_WIDTH = 2;
sub FM_STAT_BROADCAST_DROPS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x8b * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_CMRX_DROPS_OFFSET = 0;
sub FM_STAT_CMRX_DROPS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_CMRX_DROPS_WIDTH = 2;
sub FM_STAT_CMRX_DROPS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x8c * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_DLFDROPS_OFFSET = 0;
sub FM_STAT_DLFDROPS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_DLFDROPS_WIDTH = 2;
sub FM_STAT_DLFDROPS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x8a * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_FIDFORWARDED_OFFSET = 0;
sub FM_STAT_FIDFORWARDED_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_FIDFORWARDED_WIDTH = 2;
sub FM_STAT_FIDFORWARDED
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x80 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_FLOOD_FORWARDED_OFFSET = 0;
sub FM_STAT_FLOOD_FORWARDED_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_FLOOD_FORWARDED_WIDTH = 2;
sub FM_STAT_FLOOD_FORWARDED
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x81 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_GLOBAL_HIGH_DROP_WIDTH = 2;
sub FM_STAT_GLOBAL_HIGH_DROP
{
    my ($self, $word) = @_;
    return (( 0x66002 + $word ));
}

our $FM_STAT_GLOBAL_LOW_DROP_WIDTH = 2;
sub FM_STAT_GLOBAL_LOW_DROP
{
    my ($self, $word) = @_;
    return (( 0x66000 + $word ));
}

our $FM_STAT_GLOBAL_PRIV_DROP_WIDTH = 2;
sub FM_STAT_GLOBAL_PRIV_DROP
{
    my ($self, $word) = @_;
    return (( 0x66004 + $word ));
}

our $FM_STAT_RESERVED_TRAPS_OFFSET = 0;
sub FM_STAT_RESERVED_TRAPS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RESERVED_TRAPS_WIDTH = 2;
sub FM_STAT_RESERVED_TRAPS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x83 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_10240_TO_MAX_OFFSET = 0;
sub FM_STAT_RX_10240_TO_MAX_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_10240_TO_MAX_WIDTH = 2;
sub FM_STAT_RX_10240_TO_MAX
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x4b * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_1024_TO_1522_OFFSET = 0;
sub FM_STAT_RX_1024_TO_1522_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_1024_TO_1522_WIDTH = 2;
sub FM_STAT_RX_1024_TO_1522
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x46 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_128_TO_255_OFFSET = 0;
sub FM_STAT_RX_128_TO_255_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_128_TO_255_WIDTH = 2;
sub FM_STAT_RX_128_TO_255
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x43 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_1523_TO_2047_OFFSET = 0;
sub FM_STAT_RX_1523_TO_2047_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_1523_TO_2047_WIDTH = 2;
sub FM_STAT_RX_1523_TO_2047
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x47 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_2048_TO_4095_OFFSET = 0;
sub FM_STAT_RX_2048_TO_4095_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_2048_TO_4095_WIDTH = 2;
sub FM_STAT_RX_2048_TO_4095
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x48 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_256_TO_511_OFFSET = 0;
sub FM_STAT_RX_256_TO_511_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_256_TO_511_WIDTH = 2;
sub FM_STAT_RX_256_TO_511
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x44 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_4096_TO_8191_OFFSET = 0;
sub FM_STAT_RX_4096_TO_8191_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_4096_TO_8191_WIDTH = 2;
sub FM_STAT_RX_4096_TO_8191
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x49 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_512_TO_1023_OFFSET = 0;
sub FM_STAT_RX_512_TO_1023_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_512_TO_1023_WIDTH = 2;
sub FM_STAT_RX_512_TO_1023
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x45 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_64_PKTS_OFFSET = 0;
sub FM_STAT_RX_64_PKTS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_64_PKTS_WIDTH = 2;
sub FM_STAT_RX_64_PKTS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x41 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_65_TO_127_OFFSET = 0;
sub FM_STAT_RX_65_TO_127_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_65_TO_127_WIDTH = 2;
sub FM_STAT_RX_65_TO_127
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x42 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_8192_TO_10239_OFFSET = 0;
sub FM_STAT_RX_8192_TO_10239_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_8192_TO_10239_WIDTH = 2;
sub FM_STAT_RX_8192_TO_10239
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x4a * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_BAD_OCTETS_OFFSET = 0;
sub FM_STAT_RX_BAD_OCTETS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_BAD_OCTETS_WIDTH = 2;
sub FM_STAT_RX_BAD_OCTETS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x51 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_BCST_PKTS_OFFSET = 0;
sub FM_STAT_RX_BCST_PKTS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_BCST_PKTS_WIDTH = 2;
sub FM_STAT_RX_BCST_PKTS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x1 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_FCSERRORS_OFFSET = 0;
sub FM_STAT_RX_FCSERRORS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_FCSERRORS_WIDTH = 2;
sub FM_STAT_RX_FCSERRORS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x4 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_FRAGMENTS_OFFSET = 0;
sub FM_STAT_RX_FRAGMENTS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_FRAGMENTS_WIDTH = 2;
sub FM_STAT_RX_FRAGMENTS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x4e * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_GOOD_OCTETS_OFFSET = 0;
sub FM_STAT_RX_GOOD_OCTETS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_GOOD_OCTETS_WIDTH = 2;
sub FM_STAT_RX_GOOD_OCTETS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x50 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_JABBERS_OFFSET = 0;
sub FM_STAT_RX_JABBERS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_JABBERS_WIDTH = 2;
sub FM_STAT_RX_JABBERS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x4f * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_MCST_PKTS_OFFSET = 0;
sub FM_STAT_RX_MCST_PKTS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_MCST_PKTS_WIDTH = 2;
sub FM_STAT_RX_MCST_PKTS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x2 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_MINTO_63_OFFSET = 0;
sub FM_STAT_RX_MINTO_63_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_MINTO_63_WIDTH = 2;
sub FM_STAT_RX_MINTO_63
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x40 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_OCTETS_P0_OFFSET = 0;
sub FM_STAT_RX_OCTETS_P0_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_OCTETS_P0_WIDTH = 2;
sub FM_STAT_RX_OCTETS_P0
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x90 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_OCTETS_P1_OFFSET = 0;
sub FM_STAT_RX_OCTETS_P1_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_OCTETS_P1_WIDTH = 2;
sub FM_STAT_RX_OCTETS_P1
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x91 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_OCTETS_P2_OFFSET = 0;
sub FM_STAT_RX_OCTETS_P2_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_OCTETS_P2_WIDTH = 2;
sub FM_STAT_RX_OCTETS_P2
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x92 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_OCTETS_P3_OFFSET = 0;
sub FM_STAT_RX_OCTETS_P3_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_OCTETS_P3_WIDTH = 2;
sub FM_STAT_RX_OCTETS_P3
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x93 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_OCTETS_P4_OFFSET = 0;
sub FM_STAT_RX_OCTETS_P4_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_OCTETS_P4_WIDTH = 2;
sub FM_STAT_RX_OCTETS_P4
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x94 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_OCTETS_P5_OFFSET = 0;
sub FM_STAT_RX_OCTETS_P5_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_OCTETS_P5_WIDTH = 2;
sub FM_STAT_RX_OCTETS_P5
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x95 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_OCTETS_P6_OFFSET = 0;
sub FM_STAT_RX_OCTETS_P6_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_OCTETS_P6_WIDTH = 2;
sub FM_STAT_RX_OCTETS_P6
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x96 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_OCTETS_P7_OFFSET = 0;
sub FM_STAT_RX_OCTETS_P7_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_OCTETS_P7_WIDTH = 2;
sub FM_STAT_RX_OCTETS_P7
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x97 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_OVERSIZED_OFFSET = 0;
sub FM_STAT_RX_OVERSIZED_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_OVERSIZED_WIDTH = 2;
sub FM_STAT_RX_OVERSIZED
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x4d * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_P0_OFFSET = 0;
sub FM_STAT_RX_P0_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_P0_WIDTH = 2;
sub FM_STAT_RX_P0
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x8 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_P1_OFFSET = 0;
sub FM_STAT_RX_P1_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_P1_WIDTH = 2;
sub FM_STAT_RX_P1
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x9 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_P2_OFFSET = 0;
sub FM_STAT_RX_P2_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_P2_WIDTH = 2;
sub FM_STAT_RX_P2
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0xa * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_P3_OFFSET = 0;
sub FM_STAT_RX_P3_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_P3_WIDTH = 2;
sub FM_STAT_RX_P3
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0xb * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_P4_OFFSET = 0;
sub FM_STAT_RX_P4_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_P4_WIDTH = 2;
sub FM_STAT_RX_P4
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0xc * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_P5_OFFSET = 0;
sub FM_STAT_RX_P5_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_P5_WIDTH = 2;
sub FM_STAT_RX_P5
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0xd * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_P6_OFFSET = 0;
sub FM_STAT_RX_P6_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_P6_WIDTH = 2;
sub FM_STAT_RX_P6
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0xe * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_P7_OFFSET = 0;
sub FM_STAT_RX_P7_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_P7_WIDTH = 2;
sub FM_STAT_RX_P7
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0xf * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_PAUSE_PKTS_OFFSET = 0;
sub FM_STAT_RX_PAUSE_PKTS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_PAUSE_PKTS_WIDTH = 2;
sub FM_STAT_RX_PAUSE_PKTS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x3 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_SYMBOL_ERRORS_OFFSET = 0;
sub FM_STAT_RX_SYMBOL_ERRORS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_SYMBOL_ERRORS_WIDTH = 2;
sub FM_STAT_RX_SYMBOL_ERRORS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x5 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_UCST_PKTS_OFFSET = 0;
sub FM_STAT_RX_UCST_PKTS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_UCST_PKTS_WIDTH = 2;
sub FM_STAT_RX_UCST_PKTS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x0 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_RX_UNDERSIZED_OFFSET = 0;
sub FM_STAT_RX_UNDERSIZED_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_RX_UNDERSIZED_WIDTH = 2;
sub FM_STAT_RX_UNDERSIZED
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x4c * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_SECURITY_VIOLATIONS_OFFSET = 0;
sub FM_STAT_SECURITY_VIOLATIONS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_SECURITY_VIOLATIONS_WIDTH = 2;
sub FM_STAT_SECURITY_VIOLATIONS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x84 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_STPDROPS_OFFSET = 0;
sub FM_STAT_STPDROPS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_STPDROPS_WIDTH = 2;
sub FM_STAT_STPDROPS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x82 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_TRIG_OFFSET = 0;
sub FM_STAT_TRIG_ENTRIES
{
    my ($self) = @_;
    return 17;
}
our $FM_STAT_TRIG_WIDTH = 2;
sub FM_STAT_TRIG
{
    my ($self, $index, $word) = @_;
    return (( 0x660c0 + ( $index * 2 ) + $word ));
}

our $FM_STAT_TRIGGER_DROPS_OFFSET = 0;
sub FM_STAT_TRIGGER_DROPS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_TRIGGER_DROPS_WIDTH = 2;
sub FM_STAT_TRIGGER_DROPS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x88 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_TRIGGER_MIRRORED_OFFSET = 0;
sub FM_STAT_TRIGGER_MIRRORED_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_TRIGGER_MIRRORED_WIDTH = 2;
sub FM_STAT_TRIGGER_MIRRORED
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x89 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_TX_10240_TO_MAX_OFFSET = 0;
sub FM_STAT_TX_10240_TO_MAX_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_TX_10240_TO_MAX_WIDTH = 2;
sub FM_STAT_TX_10240_TO_MAX
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0xa5 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_TX_1024_TO_1522_OFFSET = 0;
sub FM_STAT_TX_1024_TO_1522_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_TX_1024_TO_1522_WIDTH = 2;
sub FM_STAT_TX_1024_TO_1522
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0xa0 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_TX_128_TO_255_OFFSET = 0;
sub FM_STAT_TX_128_TO_255_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_TX_128_TO_255_WIDTH = 2;
sub FM_STAT_TX_128_TO_255
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x9d * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_TX_1523_TO_2047_OFFSET = 0;
sub FM_STAT_TX_1523_TO_2047_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_TX_1523_TO_2047_WIDTH = 2;
sub FM_STAT_TX_1523_TO_2047
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0xa1 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_TX_2048_TO_4095_OFFSET = 0;
sub FM_STAT_TX_2048_TO_4095_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_TX_2048_TO_4095_WIDTH = 2;
sub FM_STAT_TX_2048_TO_4095
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0xa2 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_TX_256_TO_511_OFFSET = 0;
sub FM_STAT_TX_256_TO_511_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_TX_256_TO_511_WIDTH = 2;
sub FM_STAT_TX_256_TO_511
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x9e * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_TX_4096_TO_8191_OFFSET = 0;
sub FM_STAT_TX_4096_TO_8191_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_TX_4096_TO_8191_WIDTH = 2;
sub FM_STAT_TX_4096_TO_8191
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0xa3 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_TX_512_TO_1023_OFFSET = 0;
sub FM_STAT_TX_512_TO_1023_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_TX_512_TO_1023_WIDTH = 2;
sub FM_STAT_TX_512_TO_1023
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x9f * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_TX_64_PKTS_OFFSET = 0;
sub FM_STAT_TX_64_PKTS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_TX_64_PKTS_WIDTH = 2;
sub FM_STAT_TX_64_PKTS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x9b * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_TX_65_TO_127_OFFSET = 0;
sub FM_STAT_TX_65_TO_127_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_TX_65_TO_127_WIDTH = 2;
sub FM_STAT_TX_65_TO_127
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x9c * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_TX_8192_TO_10239_OFFSET = 0;
sub FM_STAT_TX_8192_TO_10239_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_TX_8192_TO_10239_WIDTH = 2;
sub FM_STAT_TX_8192_TO_10239
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0xa4 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_TX_BCST_PKTS_OFFSET = 0;
sub FM_STAT_TX_BCST_PKTS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_TX_BCST_PKTS_WIDTH = 2;
sub FM_STAT_TX_BCST_PKTS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x11 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_TX_BYTECOUNT_OFFSET = 1;
sub FM_STAT_TX_BYTECOUNT_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_STAT_TX_BYTECOUNT_WIDTH = 2;
sub FM_STAT_TX_BYTECOUNT
{
    my ($self, $index, $word) = @_;
    return ((0x0 + (0x802c * 1) + ( 0x400 * ( $index - 1 ) ) + $word));
}

our $FM_STAT_TX_DROP_PORT_OFFSET = 0;
sub FM_STAT_TX_DROP_PORT_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_TX_DROP_PORT_WIDTH = 2;
sub FM_STAT_TX_DROP_PORT
{
    my ($self, $index, $word) = @_;
    return (( 0x66080 + ( $index * 2 ) + $word ));
}

our $FM_STAT_TX_ERROR_DROP_OFFSET = 0;
sub FM_STAT_TX_ERROR_DROP_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_TX_ERROR_DROP_WIDTH = 2;
sub FM_STAT_TX_ERROR_DROP
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x14 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_TX_MCST_PKTS_OFFSET = 0;
sub FM_STAT_TX_MCST_PKTS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_TX_MCST_PKTS_WIDTH = 2;
sub FM_STAT_TX_MCST_PKTS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x12 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_TX_MINTO_63_OFFSET = 0;
sub FM_STAT_TX_MINTO_63_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_TX_MINTO_63_WIDTH = 2;
sub FM_STAT_TX_MINTO_63
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x9a * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_TX_OCTETS_OFFSET = 0;
sub FM_STAT_TX_OCTETS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_TX_OCTETS_WIDTH = 2;
sub FM_STAT_TX_OCTETS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x98 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_TX_TIME_OUT_DROP_OFFSET = 0;
sub FM_STAT_TX_TIME_OUT_DROP_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_TX_TIME_OUT_DROP_WIDTH = 2;
sub FM_STAT_TX_TIME_OUT_DROP
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x13 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_TX_UCST_PKTS_OFFSET = 0;
sub FM_STAT_TX_UCST_PKTS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_TX_UCST_PKTS_WIDTH = 2;
sub FM_STAT_TX_UCST_PKTS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x10 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_VLAN_EGRESS_BV_OFFSET = 0;
sub FM_STAT_VLAN_EGRESS_BV_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_VLAN_EGRESS_BV_WIDTH = 2;
sub FM_STAT_VLAN_EGRESS_BV
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x87 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_VLAN_INGRESS_BV_OFFSET = 0;
sub FM_STAT_VLAN_INGRESS_BV_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_VLAN_INGRESS_BV_WIDTH = 2;
sub FM_STAT_VLAN_INGRESS_BV
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x86 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_VLAN_TAG_DROPS_OFFSET = 0;
sub FM_STAT_VLAN_TAG_DROPS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_VLAN_TAG_DROPS_WIDTH = 2;
sub FM_STAT_VLAN_TAG_DROPS
{
    my ($self, $index, $word) = @_;
    return ((0x70000 + (0x85 * 2) + ( 0x200 * $index ) + $word));
}

our $FM_STAT_VLAN_UCST_OCTETS_OFFSET = 0;
sub FM_STAT_VLAN_UCST_OCTETS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_VLAN_UCST_OCTETS_WIDTH = 2;
sub FM_STAT_VLAN_UCST_OCTETS
{
    my ($self, $index, $word) = @_;
    return (( 0x66180 + ( $index * 2 ) + $word ));
}

our $FM_STAT_VLAN_UCST_PKTS_OFFSET = 0;
sub FM_STAT_VLAN_UCST_PKTS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_VLAN_UCST_PKTS_WIDTH = 2;
sub FM_STAT_VLAN_UCST_PKTS
{
    my ($self, $index, $word) = @_;
    return (( 0x66100 + ( $index * 2 ) + $word ));
}

our $FM_STAT_VLAN_XCST_OCTETS_OFFSET = 0;
sub FM_STAT_VLAN_XCST_OCTETS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_VLAN_XCST_OCTETS_WIDTH = 2;
sub FM_STAT_VLAN_XCST_OCTETS
{
    my ($self, $index, $word) = @_;
    return (( 0x661c0 + ( $index * 2 ) + $word ));
}

our $FM_STAT_VLAN_XCST_PKTS_OFFSET = 0;
sub FM_STAT_VLAN_XCST_PKTS_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STAT_VLAN_XCST_PKTS_WIDTH = 2;
sub FM_STAT_VLAN_XCST_PKTS
{
    my ($self, $index, $word) = @_;
    return (( 0x66140 + ( $index * 2 ) + $word ));
}

our $FM_STREAM_STATUS_1_OFFSET = 0;
sub FM_STREAM_STATUS_1_ENTRIES
{
    my ($self) = @_;
    return 25;
}
our $FM_STREAM_STATUS_1_WIDTH = 1;
sub FM_STREAM_STATUS_1
{
    my ($self, $index) = @_;
    return (( 0x64060 + $index ));
}

our $FM_STREAM_STATUS_2_WIDTH = 1;
sub FM_STREAM_STATUS_2
{
    my ($self) = @_;
    return (0x64008);
}

our $FM_SWITCH_ERR_CNT_OFFSET = 1;
sub FM_SWITCH_ERR_CNT_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_SWITCH_ERR_CNT_WIDTH = 1;
sub FM_SWITCH_ERR_CNT
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8028 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_SYS_CFG_1_WIDTH = 1;
sub FM_SYS_CFG_1
{
    my ($self) = @_;
    return (0x60001);
}

our $FM_SYS_CFG_2_WIDTH = 1;
sub FM_SYS_CFG_2
{
    my ($self) = @_;
    return (0x58121);
}

our $FM_SYS_CFG_3_WIDTH = 1;
sub FM_SYS_CFG_3
{
    my ($self) = @_;
    return (0x60002);
}

our $FM_SYS_CFG_4_WIDTH = 1;
sub FM_SYS_CFG_4
{
    my ($self) = @_;
    return (0x60003);
}

our $FM_SYS_CFG_6_WIDTH = 1;
sub FM_SYS_CFG_6
{
    my ($self) = @_;
    return (0x60004);
}

our $FM_SYS_CFG_7_WIDTH = 1;
sub FM_SYS_CFG_7
{
    my ($self) = @_;
    return (0x30f);
}

our $FM_TRIGGER_CFG_OFFSET = 0;
sub FM_TRIGGER_CFG_ENTRIES
{
    my ($self) = @_;
    return 15;
}
our $FM_TRIGGER_CFG_WIDTH = 1;
sub FM_TRIGGER_CFG
{
    my ($self, $index) = @_;
    return (( 0x62020 + ( $index * 1 ) ));
}

our $FM_TRIGGER_IM_WIDTH = 1;
sub FM_TRIGGER_IM
{
    my ($self) = @_;
    return (0x640c7);
}

our $FM_TRIGGER_IP_WIDTH = 1;
sub FM_TRIGGER_IP
{
    my ($self) = @_;
    return (0x640c6);
}

our $FM_TRIGGER_PRI_OFFSET = 0;
sub FM_TRIGGER_PRI_ENTRIES
{
    my ($self) = @_;
    return 15;
}
our $FM_TRIGGER_PRI_WIDTH = 1;
sub FM_TRIGGER_PRI
{
    my ($self, $index) = @_;
    return (( 0x62040 + ( $index * 1 ) ));
}

our $FM_TRIGGER_RX_OFFSET = 0;
sub FM_TRIGGER_RX_ENTRIES
{
    my ($self) = @_;
    return 15;
}
our $FM_TRIGGER_RX_WIDTH = 1;
sub FM_TRIGGER_RX
{
    my ($self, $index) = @_;
    return (( 0x62060 + ( $index * 1 ) ));
}

our $FM_TRIGGER_TX_OFFSET = 0;
sub FM_TRIGGER_TX_ENTRIES
{
    my ($self) = @_;
    return 15;
}
our $FM_TRIGGER_TX_WIDTH = 1;
sub FM_TRIGGER_TX
{
    my ($self, $index) = @_;
    return (( 0x62080 + ( $index * 1 ) ));
}

our $FM_TRUNK_CANONICAL_OFFSET = 1;
sub FM_TRUNK_CANONICAL_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_TRUNK_CANONICAL_WIDTH = 1;
sub FM_TRUNK_CANONICAL
{
    my ($self, $index) = @_;
    return (( 0x60020 + $index ));
}

our $FM_TRUNK_GROUP_1_OFFSET = 0;
sub FM_TRUNK_GROUP_1_ENTRIES
{
    my ($self) = @_;
    return 12;
}
our $FM_TRUNK_GROUP_1_WIDTH = 1;
sub FM_TRUNK_GROUP_1
{
    my ($self, $index) = @_;
    return (( 0x63020 + ( $index * 1 ) ));
}

our $FM_TRUNK_GROUP_2_OFFSET = 0;
sub FM_TRUNK_GROUP_2_ENTRIES
{
    my ($self) = @_;
    return 12;
}
our $FM_TRUNK_GROUP_2_WIDTH = 1;
sub FM_TRUNK_GROUP_2
{
    my ($self, $index) = @_;
    return (( 0x63040 + ( $index * 1 ) ));
}

our $FM_TRUNK_GROUP_3_OFFSET = 0;
sub FM_TRUNK_GROUP_3_ENTRIES
{
    my ($self) = @_;
    return 12;
}
our $FM_TRUNK_GROUP_3_WIDTH = 1;
sub FM_TRUNK_GROUP_3
{
    my ($self, $index) = @_;
    return (( 0x63060 + ( $index * 1 ) ));
}

our $FM_TRUNK_HASH_MASK_WIDTH = 1;
sub FM_TRUNK_HASH_MASK
{
    my ($self) = @_;
    return (0x61000);
}

our $FM_TRUNK_PORT_MAP_OFFSET = 1;
sub FM_TRUNK_PORT_MAP_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_TRUNK_PORT_MAP_WIDTH = 1;
sub FM_TRUNK_PORT_MAP
{
    my ($self, $index) = @_;
    return (( 0x63000 + $index ));
}

our $FM_TX_PRI_MAP_1_OFFSET = 1;
sub FM_TX_PRI_MAP_1_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_TX_PRI_MAP_1_WIDTH = 1;
sub FM_TX_PRI_MAP_1
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8020 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_TX_PRI_MAP_2_OFFSET = 1;
sub FM_TX_PRI_MAP_2_ENTRIES
{
    my ($self) = @_;
    return 24;
}
our $FM_TX_PRI_MAP_2_WIDTH = 1;
sub FM_TX_PRI_MAP_2
{
    my ($self, $index) = @_;
    return (( 0x0 + (0x8021 * 1) + ( 0x400 * ( $index - 1 ) ) ));
}

our $FM_VID_TABLE_OFFSET = 0;
sub FM_VID_TABLE_ENTRIES
{
    my ($self) = @_;
    return 4096;
}
our $FM_VID_TABLE_WIDTH = 2;
sub FM_VID_TABLE
{
    my ($self, $index, $word) = @_;
    return (( 0x50000 + ( $index * 2 ) + $word ));
}

our $FM_VITAL_PRODUCT_DATA_WIDTH = 1;
sub FM_VITAL_PRODUCT_DATA
{
    my ($self) = @_;
    return (0x304);
}

1;
