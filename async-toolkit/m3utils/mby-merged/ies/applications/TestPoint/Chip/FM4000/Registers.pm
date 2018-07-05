# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Chip/FM4000/Registers.pm
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

package Chip::FM4000::Registers;
use strict;
use warnings;

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(
    FM_AGING_SERVE_WM
    FM_AN_CTL
    FM_AN_RX_CW_Hi
    FM_AN_RX_CW_Lo
    FM_AN_RX_LAST_PAGE
    FM_AN_RX_MSG0
    FM_AN_RX_MSG1
    FM_AN_STATUS
    FM_AN_TIMEOUT
    FM_AN_TX_CW_Hi
    FM_AN_TX_CW_Lo
    FM_AN_TX_MSG0
    FM_AN_TX_MSG1
    FM_AN_TX_TIMER
    FM_AN_TimeoutValue
    FM_ARP_IM
    FM_ARP_IP
    FM_ARP_REDIRECT_DIP
    FM_ARP_REDIRECT_SIP
    FM_ARP_TABLE
    FM_ARP_TABLE_REPAIR_0
    FM_ARP_TABLE_REPAIR_1
    FM_ARP_TABLE_REPAIR_2
    FM_ARP_TABLE_REPAIR_3
    FM_ARP_USED
    FM_ARP_USED_REPAIR
    FM_ASYNC_SCAN_CFG
    FM_ASYNC_SCAN_CMD
    FM_ASYNC_SCAN_IN
    FM_ASYNC_SCAN_OUT
    FM_BOOT_CTRL
    FM_BOOT_STATUS
    FM_CANONICAL_GLORT_CAM
    FM_CHIP_MODE
    FM_CLK_MULT_1
    FM_CM_CPU_PORT_STATUS
    FM_CM_GLOBAL_CFG
    FM_CM_GLOBAL_USAGE
    FM_CM_GLOBAL_WM
    FM_CM_IM
    FM_CM_IP
    FM_CM_PAUSE_DECIMATION
    FM_CM_PAUSE_RESEND_INTERVAL
    FM_CM_PORT_CFG
    FM_CM_PORT_LIST
    FM_CM_RX_SHARED_SMP_HOG_WM
    FM_CM_RX_SHARED_SMP_USAGE
    FM_CM_RX_SHARED_WM
    FM_CM_RX_SMP_HOG_WM
    FM_CM_RX_SMP_PAUSE_WM
    FM_CM_RX_SMP_PRIVATE_WM
    FM_CM_RX_SMP_USAGE
    FM_CM_RX_USAGE
    FM_CM_SHARED_SMP_PAUSE_WM
    FM_CM_SHARED_SMP_USAGE
    FM_CM_SHARED_WM
    FM_CM_SMP_MEMBERSHIP
    FM_CM_TX_HOG_MAP
    FM_CM_TX_SHARED_HOG_WM
    FM_CM_TX_SHARED_SMP_USAGE
    FM_CM_TX_SMP_HOG_WM
    FM_CM_TX_SMP_PRIVATE_WM
    FM_CM_TX_SMP_USAGE
    FM_CM_TX_TC_PRIVATE_WM
    FM_CM_TX_TC_USAGE
    FM_CN_BACKOFF_BYTETIME
    FM_CN_CPID_MASK
    FM_CN_CPID_TABLE
    FM_CN_FB_CFG
    FM_CN_FORWARD_MASK
    FM_CN_FRAME_CFG_1
    FM_CN_FRAME_CFG_2
    FM_CN_GLOBAL_CFG
    FM_CN_GLOBAL_CFG_1
    FM_CN_GLOBAL_CFG_2
    FM_CN_RATE_ACTION_MASK
    FM_CN_RATE_LIM_CFG
    FM_CN_RATE_LIM_CPID
    FM_CN_SAMPLE_CFG
    FM_CN_SMP_CFG
    FM_CN_SMP_THRESHOLD
    FM_CN_STATS
    FM_CN_STATS_CFG
    FM_CN_VCN_DMAC_1
    FM_CN_VCN_DMAC_2
    FM_CONTEPL_CTRLSTAT
    FM_CPID_0
    FM_CPID_1
    FM_CPID_2
    FM_CPID_3
    FM_CPID_4
    FM_CPID_5
    FM_CPID_6
    FM_CPID_7
    FM_CPU_GLORT
    FM_CPU_LOG_MASK_FH
    FM_CPU_MASK
    FM_CPU_TRAP_MASK_FH
    FM_CRM_CFG
    FM_CRM_CFG_COUNTER
    FM_CRM_CFG_LIMIT
    FM_CRM_CFG_WINDOW
    FM_CRM_EXCEED_COUNT
    FM_CRM_IM
    FM_CRM_INT_DETECT
    FM_CRM_IP
    FM_CRM_LAST_COUNT
    FM_CROSSRING_RESET
    FM_DEBUG_RX_MAC
    FM_DEBUG_RX_RS
    FM_DEBUG_SYNCBUF
    FM_DEBUG_TX_MAC
    FM_DI_CFG
    FM_DSCP_PRI_MAP
    FM_EBI
    FM_EGRESS_FID_TABLE
    FM_EGRESS_FID_TABLE_REPAIR
    FM_EGRESS_FRAME_COUNTER
    FM_EGRESS_VID_TABLE
    FM_EGRESS_VID_TABLE_REPAIR
    FM_EPL_INT_DETECT
    FM_EPL_LED_STATUS
    FM_EPL_PORT_CTRL
    FM_FATAL_COUNT
    FM_FFU_EGRESS_ACTIONS
    FM_FFU_EGRESS_CHUNK_CFG
    FM_FFU_EGRESS_CHUNK_VALID
    FM_FFU_INIT_SLICE
    FM_FFU_MAP_IP_CFG
    FM_FFU_MAP_IP_HI
    FM_FFU_MAP_IP_LO
    FM_FFU_MAP_L4_DST
    FM_FFU_MAP_L4_SRC
    FM_FFU_MAP_LENGTH
    FM_FFU_MAP_MAC
    FM_FFU_MAP_PROT
    FM_FFU_MAP_SRC
    FM_FFU_MAP_TYPE
    FM_FFU_MAP_VLAN
    FM_FFU_MAP_VLAN_REPAIR
    FM_FFU_MASTER_VALID
    FM_FFU_SLICE_CASCADE_ACTION
    FM_FFU_SLICE_CASE
    FM_FFU_SLICE_CASE_CFG
    FM_FFU_SLICE_SRAM
    FM_FFU_SLICE_TCAM
    FM_FFU_SLICE_VALID
    FM_FH_INT_DETECT
    FM_FH_LOOPBACK_SUPPRESS
    FM_FID_TABLE
    FM_FRAME_SERVE_WM
    FM_FRAME_TIME_OUT
    FM_FUSEBOX
    FM_FUSEBOX_CFG
    FM_FUSEBOX_INFO
    FM_FUSEBOX_REPAIR_ASYNC
    FM_FUSEBOX_REPAIR_SYNC_ADDR
    FM_FUSEBOX_REPAIR_SYNC_VALUE
    FM_FUSEBOX_SERIAL_NUMBER
    FM_FUSEBOX_UNUSED
    FM_FUSE_PORT
    FM_FUSE_SEG
    FM_GLOBAL_EPL_INT_DETECT
    FM_GLORT_CAM
    FM_GLORT_DEST_TABLE
    FM_GLORT_DEST_TABLE_REPAIR
    FM_GLORT_RAM
    FM_GLORT_RAM_TABLE_REPAIR
    FM_GPIO_CFG
    FM_GPIO_DATA
    FM_GPIO_IM
    FM_GPIO_IP
    FM_HEAD_TAIL_SPACING_1
    FM_HEAD_TAIL_SPACING_2
    FM_HEAD_TAIL_SPACING_3
    FM_I2C_CFG
    FM_I2C_CTRL
    FM_I2C_DATA
    FM_INGRESS_FID_TABLE
    FM_INGRESS_FID_TABLE_REPAIR
    FM_INGRESS_VID_TABLE
    FM_INGRESS_VID_TABLE_REPAIR
    FM_INTERNAL_PORT_MASK
    FM_INTERRUPT_DETECT
    FM_IP_MULTICAST_TABLE
    FM_JITTER_TIMER
    FM_L234_HASH_CFG
    FM_L34_FLOW_HASH_CFG_1
    FM_L34_FLOW_HASH_CFG_2
    FM_L34_HASH_CFG
    FM_L4PROT1_WD_MASK_HI
    FM_L4PROT1_WD_MASK_LO
    FM_L4PROT2_WD_MASK_HI
    FM_L4PROT2_WD_MASK_LO
    FM_L4_CTL
    FM_LAG_CFG
    FM_LAST_FATAL_CODE
    FM_LCI_CFG
    FM_LCI_IM
    FM_LCI_IP
    FM_LCI_RX_FIFO
    FM_LCI_STATUS
    FM_LCI_TX_FIFO
    FM_LEARNING_SERVE_WM
    FM_LED_CFG
    FM_LOG_MASK
    FM_LOOPBACK_SUPPRESS
    FM_LSM_INT_DETECT
    FM_MAC_BORDER_CFG
    FM_MAC_CFG_1
    FM_MAC_CFG_2
    FM_MAC_CFG_3
    FM_MAC_CFG_4
    FM_MAC_CFG_5
    FM_MAC_CFG_6
    FM_MAC_IM
    FM_MAC_IP
    FM_MAC_STATUS
    FM_MAC_VLAN_ETYPE
    FM_MAC_VLAN_ETYPE_1
    FM_MAC_VLAN_ETYPE_2
    FM_MA_PURGE
    FM_MA_TABLE
    FM_MA_TABLE_CFG_1
    FM_MA_TABLE_CFG_2
    FM_MA_TABLE_CFG_3
    FM_MA_TABLE_STATUS_3
    FM_MA_TCN_CFG_1
    FM_MA_TCN_CFG_2
    FM_MA_TCN_FIFO
    FM_MA_TCN_IM
    FM_MA_TCN_IP
    FM_MA_TCN_PTR
    FM_MDIO_CFG
    FM_MDIO_CTRL
    FM_MDIO_DATA
    FM_MGMT_CLK_COUNTER
    FM_MGMT_FFU_CLK_COUNTER
    FM_MGR_IM
    FM_MGR_IP
    FM_MIRROR_GLORTS
    FM_MSB_CFG
    FM_MSB_CREDITS
    FM_MSB_IBM_GLORT
    FM_MSB_IBM_INT
    FM_MSB_IM
    FM_MSB_INTR_CTR_0
    FM_MSB_INTR_CTR_1
    FM_MSB_INTR_CTR_2
    FM_MSB_INTR_CTR_3
    FM_MSB_INTR_CTR_4
    FM_MSB_INTR_CTR_5
    FM_MSB_INT_FRAME
    FM_MSB_IP
    FM_MSB_RX_EPL_RATE
    FM_MSB_SCRATCH_0
    FM_MSB_SCRATCH_1
    FM_MSB_SCRATCH_10
    FM_MSB_SCRATCH_11
    FM_MSB_SCRATCH_12
    FM_MSB_SCRATCH_13
    FM_MSB_SCRATCH_14
    FM_MSB_SCRATCH_15
    FM_MSB_SCRATCH_2
    FM_MSB_SCRATCH_3
    FM_MSB_SCRATCH_4
    FM_MSB_SCRATCH_5
    FM_MSB_SCRATCH_6
    FM_MSB_SCRATCH_7
    FM_MSB_SCRATCH_8
    FM_MSB_SCRATCH_9
    FM_MSB_SRAM_REPAIR_0
    FM_MSB_SRAM_REPAIR_1
    FM_MSB_STATS_0
    FM_MSB_STATS_1
    FM_MSB_STATS_2
    FM_MSB_TS
    FM_MTU_TABLE
    FM_PACING_PRI_WM
    FM_PACING_RATE
    FM_PACING_STAT
    FM_PARITY_IM
    FM_PARITY_IP
    FM_PARSE_CFG
    FM_PARSE_RLT_1
    FM_PARSE_RLT_2
    FM_PCS_CFG_1
    FM_PCS_CFG_2
    FM_PCS_CFG_3
    FM_PCS_CFG_4
    FM_PCS_CFG_5
    FM_PCS_IM
    FM_PCS_IP
    FM_PERR_IM
    FM_PERR_IP
    FM_PLL_FH_CTRL
    FM_PLL_FH_STAT
    FM_POLICER_CFG
    FM_POLICER_DSCP_DOWN_MAP
    FM_POLICER_IM
    FM_POLICER_IP
    FM_POLICER_REPAIR
    FM_POLICER_STATUS
    FM_POLICER_SWPRI_DOWN_MAP
    FM_POLICER_TABLE
    FM_POLICER_TS_DIV
    FM_PORT_CFG_1
    FM_PORT_CFG_2
    FM_PORT_CFG_3
    FM_PORT_CFG_ISL
    FM_PORT_MAC_SEC_IM
    FM_PORT_MAC_SEC_IP
    FM_PORT_VLAN_IM_1
    FM_PORT_VLAN_IM_2
    FM_PORT_VLAN_IP_1
    FM_PORT_VLAN_IP_2
    FM_PREFETCH_CFG
    FM_PRE_TO_MID_FIFO_CFG
    FM_QDM_CFG
    FM_QDM_FRAME_CNT
    FM_QDM_INSTRUMENT
    FM_RX_MIRROR_CFG
    FM_RX_PRI_MAP
    FM_RX_RATE_LIM_CFG
    FM_RX_RATE_LIM_THRESHOLD
    FM_RX_RATE_LIM_USAGE
    FM_RX_VPRI_MAP
    FM_SAF_MATRIX
    FM_SCAN_CTRL
    FM_SCAN_DATA_IN
    FM_SCAN_DATA_OUT
    FM_SCAN_FREQ_MULT
    FM_SCAN_SEL
    FM_SCHED_DRR_Q
    FM_SCHED_FH_GROUP_CFG
    FM_SCHED_GROUP_CFG
    FM_SCHED_SHAPING_GROUP_CFG
    FM_SERDES_BIST_ERR_CNT
    FM_SERDES_CTRL_1
    FM_SERDES_CTRL_2
    FM_SERDES_CTRL_3
    FM_SERDES_IM
    FM_SERDES_IP
    FM_SERDES_STATUS
    FM_SERDES_TEST_MODE
    FM_SHADOW_FUSEBOX_INFO
    FM_SHADOW_FUSEBOX_REPAIR_ASYNC
    FM_SHADOW_FUSEBOX_REPAIR_SYNC_ADDR
    FM_SHADOW_FUSEBOX_REPAIR_SYNC_VALUE
    FM_SHADOW_FUSEBOX_SERIAL_NUMBER
    FM_SHADOW_FUSEBOX_UNUSED
    FM_SOFT_RESET
    FM_SOT_CFG
    FM_SRC_MAC_HI
    FM_SRC_MAC_LO
    FM_SRC_MAC_VIRTUAL_HI
    FM_SRC_MAC_VIRTUAL_LO
    FM_STATS_CFG
    FM_STATS_DROP_COUNT_RX
    FM_STATS_DROP_COUNT_TX
    FM_STAT_BadSMPDrops
    FM_STAT_CMPrivDrops
    FM_STAT_EPL_ERROR1
    FM_STAT_EPL_ERROR2
    FM_STAT_EgressACLOctets
    FM_STAT_EgressACLPkts
    FM_STAT_FFUDrops
    FM_STAT_FIDForwarded
    FM_STAT_FloodForwarded
    FM_STAT_GlobalHighDrop
    FM_STAT_GlobalLowDrop
    FM_STAT_GlobalPrivDrop
    FM_STAT_GlortMissDrops
    FM_STAT_Others
    FM_STAT_ParityError
    FM_STAT_ParseErrDrops
    FM_STAT_PauseDrops
    FM_STAT_PolicerDrops
    FM_STAT_RX_JABBER
    FM_STAT_RateLimit0Drops
    FM_STAT_RateLimit1Drops
    FM_STAT_Rx10240toMax
    FM_STAT_Rx1024to1522
    FM_STAT_Rx128to255
    FM_STAT_Rx1523to2047
    FM_STAT_Rx2048to4095
    FM_STAT_Rx256to511
    FM_STAT_Rx4096to8191
    FM_STAT_Rx512to1023
    FM_STAT_Rx64Pkts
    FM_STAT_Rx65to127
    FM_STAT_Rx8192to10239
    FM_STAT_RxBcstPktsIPv4
    FM_STAT_RxBcstPktsIPv6
    FM_STAT_RxBcstPktsNonIP
    FM_STAT_RxCBPausePkts
    FM_STAT_RxFCSErrors
    FM_STAT_RxFragments
    FM_STAT_RxFrameSizeErrors
    FM_STAT_RxHog0Drops
    FM_STAT_RxHog1Drops
    FM_STAT_RxMcstPktsIPv4
    FM_STAT_RxMcstPktsIPv6
    FM_STAT_RxMcstPktsNonIP
    FM_STAT_RxMinto63
    FM_STAT_RxOctetsError
    FM_STAT_RxOctetsIPv4
    FM_STAT_RxOctetsIPv6
    FM_STAT_RxOctetsNonIP
    FM_STAT_RxOctetsP0
    FM_STAT_RxOctetsP1
    FM_STAT_RxOctetsP2
    FM_STAT_RxOctetsP3
    FM_STAT_RxOctetsP4
    FM_STAT_RxOctetsP5
    FM_STAT_RxOctetsP6
    FM_STAT_RxOctetsP7
    FM_STAT_RxOversized
    FM_STAT_RxP0
    FM_STAT_RxP1
    FM_STAT_RxP2
    FM_STAT_RxP3
    FM_STAT_RxP4
    FM_STAT_RxP5
    FM_STAT_RxP6
    FM_STAT_RxP7
    FM_STAT_RxPausePkts
    FM_STAT_RxSymbolErrors
    FM_STAT_RxUcstPktsIPv4
    FM_STAT_RxUcstPktsIPv6
    FM_STAT_RxUcstPktsNonIP
    FM_STAT_RxUndersized
    FM_STAT_SMP0Drops
    FM_STAT_SMP1Drops
    FM_STAT_STPDrops
    FM_STAT_SecurityViolations
    FM_STAT_SpeciallyHandled
    FM_STAT_TTLDrops
    FM_STAT_TX_BYTECOUNT
    FM_STAT_TX_CRC
    FM_STAT_TX_PAUSE
    FM_STAT_Trapped
    FM_STAT_Trigger
    FM_STAT_TriggerDrops
    FM_STAT_TriggerRedirects
    FM_STAT_Tx10240toMax
    FM_STAT_Tx1024to1522
    FM_STAT_Tx128to255
    FM_STAT_Tx1523to2047
    FM_STAT_Tx2048to4095
    FM_STAT_Tx256to511
    FM_STAT_Tx4096to8191
    FM_STAT_Tx512to1023
    FM_STAT_Tx64Pkts
    FM_STAT_Tx65to127
    FM_STAT_Tx8192to10239
    FM_STAT_TxBcstPkts
    FM_STAT_TxDropPort
    FM_STAT_TxErrorDrop
    FM_STAT_TxErrorOctets
    FM_STAT_TxErrors
    FM_STAT_TxHog0Drops
    FM_STAT_TxHog1Drops
    FM_STAT_TxLoopbackDrop
    FM_STAT_TxMcstPkts
    FM_STAT_TxMinto63
    FM_STAT_TxOctets
    FM_STAT_TxPort0
    FM_STAT_TxPort1
    FM_STAT_TxPort10
    FM_STAT_TxPort11
    FM_STAT_TxPort12
    FM_STAT_TxPort13
    FM_STAT_TxPort14
    FM_STAT_TxPort15
    FM_STAT_TxPort16
    FM_STAT_TxPort17
    FM_STAT_TxPort18
    FM_STAT_TxPort19
    FM_STAT_TxPort2
    FM_STAT_TxPort20
    FM_STAT_TxPort21
    FM_STAT_TxPort22
    FM_STAT_TxPort23
    FM_STAT_TxPort24
    FM_STAT_TxPort3
    FM_STAT_TxPort4
    FM_STAT_TxPort5
    FM_STAT_TxPort6
    FM_STAT_TxPort7
    FM_STAT_TxPort8
    FM_STAT_TxPort9
    FM_STAT_TxTimeOutDrop
    FM_STAT_TxUcstPkts
    FM_STAT_VlanEgressDrops
    FM_STAT_VlanIngressDrops
    FM_STAT_VlanTagDrops
    FM_STAT_VlanUcstOctets
    FM_STAT_VlanUcstPkts
    FM_STAT_VlanXcstOctets
    FM_STAT_VlanXcstPkts
    FM_SWITCH_PRI_TO_CLASS_1
    FM_SWITCH_PRI_TO_CLASS_2
    FM_SW_IM
    FM_SW_IP
    FM_SYNCBUF_CFG
    FM_SYS_CFG_1
    FM_SYS_CFG_2
    FM_SYS_CFG_3
    FM_SYS_CFG_4
    FM_SYS_CFG_5
    FM_SYS_CFG_7
    FM_SYS_CFG_8
    FM_SYS_CFG_ROUTER
    FM_TCN_FIFO_REPAIR
    FM_TCP_WD_MASK_HI
    FM_TCP_WD_MASK_LO
    FM_TRAFFIC_CLASS_TO_SCHED_PRI
    FM_TRAP_GLORT
    FM_TRIGGER_ACTION_CFG_1
    FM_TRIGGER_ACTION_CFG_2
    FM_TRIGGER_ACTION_DMASK
    FM_TRIGGER_ACTION_DROP
    FM_TRIGGER_ACTION_GLORT
    FM_TRIGGER_ACTION_MIRROR
    FM_TRIGGER_CFG
    FM_TRIGGER_CONDITION_AMASK
    FM_TRIGGER_CONDITION_AMASK_1
    FM_TRIGGER_CONDITION_AMASK_2
    FM_TRIGGER_CONDITION_CFG
    FM_TRIGGER_CONDITION_FFU
    FM_TRIGGER_CONDITION_GLORT
    FM_TRIGGER_CONDITION_PARAM
    FM_TRIGGER_CONDITION_RX
    FM_TRIGGER_CONDITION_TX
    FM_TRIGGER_CONDITION_TYPE
    FM_TRIGGER_IM
    FM_TRIGGER_IP
    FM_TRIGGER_RATE_LIM_CFG_1
    FM_TRIGGER_RATE_LIM_CFG_2
    FM_TRIGGER_RATE_LIM_USAGE
    FM_TRIGGER_RX
    FM_TRIGGER_TX
    FM_TX_MIRROR
    FM_TX_MIRROR_FH
    FM_TX_PRI_MAP_1
    FM_TX_PRI_MAP_2
    FM_TX_RATE_LIM_CFG
    FM_TX_RATE_LIM_USAGE
    FM_TX_TRUNC
    FM_TX_VPRI_MAP_1
    FM_TX_VPRI_MAP_2
    FM_UDP_WD_MASK_HI
    FM_UDP_WD_MASK_LO
    FM_VID_TABLE
    FM_VITAL_PRODUCT_DATA
    FM_VLANTAG_TABLE
    FM_VPD_ASYNC_RAM_REPAIR
    FM_VPD_INFO_1
    FM_VPD_INFO_2
    FM_VPD_SYNC_RAM_REPAIR
    FM_VPRI_PRI_MAP
    FM_WATCHDOG_CFG
);

sub _traceback($)
{
    my ($message) = @_;

    my $indent = "  ";
    my $level;
    for ($level=1 ; ; $level++)
    {
        my ($package, $file, $line, $subroutine) = caller($level);
        if (!defined($subroutine)
            || ($subroutine eq "(eval)"))
        {
            last;
        }
        $file = pop(@{[split(/te2\/+/, $file)]});
        printf("%s%s at %s line %d:\n",
               join('', ($indent) x ($level - 1)), $subroutine, $file, $line);
    }
    printf("%s %s\n", join('', ($indent) x ($level - 1)), $message);
}

sub _validatePrototype(\@$;$)
{
    my ($arguments, $m, $n) = @_;

    my ($package, $file, $line, $subroutine) = caller(1);
    if (scalar(@{$arguments}) < $m)
    {
        _traceback(sprintf("Not enough arguments for %s::%s",
                           $package, $subroutine));
        die;
    }
    elsif ((defined($n) ? $n != -1 : 1)
            && scalar(@{$arguments}) > (defined($n) ? $n : $m))
    {
        _traceback(sprintf("Too many arguments for %s::%s",
                           $package, $subroutine));
        die;
    }
    for (my $i=0 ; $i<$n ; $i++)
    {
        if ($arguments->[$i] != int($arguments->[$i]))
        {
            _traceback(sprintf("%s::%s: Argument %d: Type mismatch",
                               $package, $subroutine, $i));
            die;
        }
    }
}

our $FM_HSM_BASE                                  =  0x00000; # // 0x00000 top level
our $FM_HSM_SIZE                                  =  0x40000; #
our $FM_LSM_BASE                                  =  0x40000; # // 0x40000 top level
our $FM_LSM_SIZE                                  =  0x40000; #
our $FM_EPL_BASE                                  =  (0x10000 + $FM_LSM_BASE); # // 0x50000 LSM_BASE
our $FM_EPL_SIZE                                  =  0x10000; #
our $FM_SCHED2_BASE                               =  (0x20000 + $FM_LSM_BASE); # // 0x60000 LSM_BASE
our $FM_SCHED2_SIZE                               =  0x10000; #
our $FM_MSB_BASE                                  =  0x80000; # // 0x80000 top level
our $FM_MSB_SIZE                                  =  0x10000; #
our $FM_PORT_STATS_BASE                           =  0x90000; # // 0x90000 top level
our $FM_PORT_STATS_SIZE                           =  0x10000; #
our $FM_STATS_RX_TYPE_BASE                        =  (0x00000 + $FM_PORT_STATS_BASE); # // 0x90000 PORT_STATS_BASE
our $FM_STATS_RX_TYPE_SIZE                        =  0x04000; #
our $FM_STATS_RX_PKT_PRI_BASE                     =  (0x00310 + $FM_PORT_STATS_BASE); # // 0x90310 PORT_STATS_BASE
our $FM_STATS_RX_PKT_PRI_SIZE                     =  0x04000; #
our $FM_STATS_TX_TYPE_BASE                        =  (0x004D0 + $FM_PORT_STATS_BASE); # // 0x904D0 PORT_STATS_BASE
our $FM_STATS_TX_TYPE_SIZE                        =  0x04000; #
our $FM_STATS_TX_INGRESS_BASE                     =  (0x00620 + $FM_PORT_STATS_BASE); # // 0x90620 PORT_STATS_BASE
our $FM_STATS_TX_INGRESS_SIZE                     =  0x04000; #
our $FM_STATS_RX_BIN_BASE                         =  (0x01000 + $FM_PORT_STATS_BASE); # // 0x91000 PORT_STATS_BASE
our $FM_STATS_RX_BIN_SIZE                         =  0x04000; #
our $FM_STATS_RX_OCTET_BASE                       =  (0x01380 + $FM_PORT_STATS_BASE); # // 0x91380 PORT_STATS_BASE
our $FM_STATS_RX_OCTET_SIZE                       =  0x04000; #
our $FM_STATS_TX_BIN_BASE                         =  (0x01460 + $FM_PORT_STATS_BASE); # // 0x91460 PORT_STATS_BASE
our $FM_STATS_TX_BIN_SIZE                         =  0x04000; #
our $FM_STATS_RX_ACTION_BASE                      =  (0x02000 + $FM_PORT_STATS_BASE); # // 0x92000 PORT_STATS_BASE
our $FM_STATS_RX_ACTION_SIZE                      =  0x04000; #
our $FM_STATS_RX_OCT_PRI_BASE                     =  (0x02700 + $FM_PORT_STATS_BASE); # // 0x92700 PORT_STATS_BASE
our $FM_STATS_RX_OCT_PRI_SIZE                     =  0x04000; #
our $FM_STATS_TX_OCTET_BASE                       =  (0x028C0 + $FM_PORT_STATS_BASE); # // 0x928C0 PORT_STATS_BASE
our $FM_STATS_TX_OCTET_SIZE                       =  0x04000; #
our $FM_MTABLE_BASE                               =  0xC0000; # // 0xC0000 top level
our $FM_MTABLE_SIZE                               =  0x10000; #
our $FM_HANDLER_BASE                              =  0x100000; # // 0x100000 top level
our $FM_HANDLER_SIZE                              =  0x100000; #
our $FM_ASSOC_BASE                                =  (0x01000 + $FM_HANDLER_BASE); # // 0x101000 HANDLER_BASE
our $FM_ASSOC_SIZE                                =  0x01000; #
our $FM_CM_BASE                                   =  (0x03000 + $FM_HANDLER_BASE); # // 0x103000 HANDLER_BASE
our $FM_CM_SIZE                                   =  0x01000; #
our $FM_LAG_BASE                                  =  (0x04000 + $FM_HANDLER_BASE); # // 0x104000 HANDLER_BASE
our $FM_LAG_SIZE                                  =  0x01000; #
our $FM_TRIG_BASE                                 =  (0x06000 + $FM_HANDLER_BASE); # // 0x106000 HANDLER_BASE
our $FM_TRIG_SIZE                                 =  0x01000; #
our $FM_GLORT_BASE                                =  (0x08000 + $FM_HANDLER_BASE); # // 0x108000 HANDLER_BASE
our $FM_GLORT_SIZE                                =  0x04000; #
our $FM_POLICER_BASE                              =  (0x0C000 + $FM_HANDLER_BASE); # // 0x10C000 HANDLER_BASE
our $FM_POLICER_SIZE                              =  0x04000; #
our $FM_ARP_BASE                                  =  (0x20000 + $FM_HANDLER_BASE); # // 0x120000 HANDLER_BASE
our $FM_ARP_SIZE                                  =  0x20000; #
our $FM_FFU_BASE                                  =  (0x40000 + $FM_HANDLER_BASE); # // 0x140000 HANDLER_BASE
our $FM_FFU_SIZE                                  =  0x40000; #
our $FM_L2LOOKUP_BASE                             =  (0x80000 + $FM_HANDLER_BASE); # // 0x180000 HANDLER_BASE
our $FM_L2LOOKUP_SIZE                             =  0x20000; #
our $FM_STATS_BASE                                =  (0xA0000 + $FM_HANDLER_BASE); # // 0x1A0000 HANDLER_BASE
our $FM_STATS_SIZE                                =  0x10000; #
our $FM_STATS_RX_CM_BASE                          =  (0x00000 + $FM_STATS_BASE); # // 0x1A0000 STATS_BASE
our $FM_STATS_RX_CM_SIZE                          =  0x00100; #
our $FM_STATS_VLAN_PKT_BASE                       =  (0x00100 + $FM_STATS_BASE); # // 0x1A0100 STATS_BASE
our $FM_STATS_VLAN_PKT_SIZE                       =  0x00100; #
our $FM_STATS_VLAN_OCT_BASE                       =  (0x00200 + $FM_STATS_BASE); # // 0x1A0200 STATS_BASE
our $FM_STATS_VLAN_OCT_SIZE                       =  0x00100; #
our $FM_STATS_TRIG_BASE                           =  (0x00300 + $FM_STATS_BASE); # // 0x1A0300 STATS_BASE
our $FM_STATS_TRIG_SIZE                           =  0x00100; #
our $FM_STATS_EGRESS_ACL_BASE                     =  (0x00400 + $FM_STATS_BASE); # // 0x1A0400 STATS_BASE
our $FM_STATS_EGRESS_ACL_SIZE                     =  0x00100; #
our $FM_STATS_CTRL_BASE                           =  (0x00500 + $FM_STATS_BASE); # // 0x1A0500 STATS_BASE
our $FM_STATS_CTRL_SIZE                           =  0x00100; #

our $FM_LEARNING_SERVE_WM                         =  (0x1E80A + $FM_L2LOOKUP_BASE); # // 0x19E80A L2LOOKUP_BASE

sub FM_LEARNING_SERVE_WM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x1E80A + $FM_L2LOOKUP_BASE); # 0x19E80A L2LOOKUP_BASE
}


sub FM_LEARNING_SERVE_WM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_LEARNING_SERVE_WM_WIDTH                   =  1; #
our $FM_LEARNING_SERVE_WM_BITS                    =  31; #

sub FM_TX_PRI_MAP_1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00020 + $FM_EPL_BASE); # 0x50020 EPL_BASE
}


sub FM_TX_PRI_MAP_1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0xECA86420;
}

our $FM_TX_PRI_MAP_1_WIDTH                        =  1; #
our $FM_TX_PRI_MAP_1_BITS                         =  32; #

sub FM_TX_PRI_MAP_1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_TX_PRI_MAP_1_STRIDE                       =  1024; #

sub FM_AN_RX_LAST_PAGE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index) - 0) + 0x0007F + $FM_EPL_BASE + ($word)); # 0x5007F EPL_BASE
}


sub FM_AN_RX_LAST_PAGE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_AN_RX_LAST_PAGE_WIDTH                     =  2; #
our $FM_AN_RX_LAST_PAGE_BITS                      =  48; #

sub FM_AN_RX_LAST_PAGE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_AN_RX_LAST_PAGE_STRIDE                    =  1024; #
our $FM_EGRESS_FRAME_COUNTER                      =  (0x00081 + $FM_HANDLER_BASE); # // 0x100081 HANDLER_BASE

sub FM_EGRESS_FRAME_COUNTER {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00081 + $FM_HANDLER_BASE); # 0x100081 HANDLER_BASE
}


sub FM_EGRESS_FRAME_COUNTER_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_EGRESS_FRAME_COUNTER_WIDTH                =  1; #
our $FM_EGRESS_FRAME_COUNTER_BITS                 =  32; #

sub FM_TRIGGER_TX {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00540 + $FM_TRIG_BASE); # 0x106540 TRIG_BASE
}


sub FM_TRIGGER_TX_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_TRIGGER_TX_WIDTH                          =  1; #
our $FM_TRIGGER_TX_BITS                           =  25; #

sub FM_TRIGGER_TX_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}

our $FM_TRIGGER_TX_STRIDE                         =  1; #

sub FM_STAT_GlobalHighDrop {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00002 + $FM_STATS_RX_CM_BASE + ($word)); # 0x1A0002 STATS_RX_CM_BASE
}


sub FM_STAT_GlobalHighDrop_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return 0;
}

our $FM_STAT_GlobalHighDrop_WIDTH                 =  2; #
our $FM_STAT_GlobalHighDrop_BITS                  =  64; #

sub FM_FUSEBOX_REPAIR_SYNC_VALUE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x02016 + $FM_LSM_BASE); # 0x42016 LSM_BASE
}


sub FM_FUSEBOX_REPAIR_SYNC_VALUE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_FUSEBOX_REPAIR_SYNC_VALUE_WIDTH           =  1; #
our $FM_FUSEBOX_REPAIR_SYNC_VALUE_BITS            =  32; #

sub FM_FUSEBOX_REPAIR_SYNC_VALUE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 13;
}

our $FM_FUSEBOX_REPAIR_SYNC_VALUE_STRIDE          =  1; #
our $FM_EBI                                       =  (0x00000 + $FM_HSM_BASE); # // 0x00000 HSM_BASE

sub FM_EBI {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00000 + $FM_HSM_BASE); # 0x00000 HSM_BASE
}


sub FM_EBI_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_EBI_WIDTH                                 =  1; #
our $FM_EBI_BITS                                  =  32; #
our $FM_CPU_GLORT                                 =  (0x00035 + $FM_HANDLER_BASE); # // 0x100035 HANDLER_BASE

sub FM_CPU_GLORT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00035 + $FM_HANDLER_BASE); # 0x100035 HANDLER_BASE
}


sub FM_CPU_GLORT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_CPU_GLORT_WIDTH                           =  1; #
our $FM_CPU_GLORT_BITS                            =  16; #

sub FM_STAT_TxDropPort {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00080 + $FM_STATS_RX_CM_BASE + ($word)); # 0x1A0080 STATS_RX_CM_BASE
}


sub FM_STAT_TxDropPort_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxDropPort_WIDTH                     =  2; #
our $FM_STAT_TxDropPort_BITS                      =  64; #

sub FM_STAT_TxDropPort_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxDropPort_STRIDE                    =  2; #

sub FM_L4_CTL {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00067 + $FM_EPL_BASE); # 0x50067 EPL_BASE
}


sub FM_L4_CTL_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00800000;
}

our $FM_L4_CTL_WIDTH                              =  1; #
our $FM_L4_CTL_BITS                               =  30; #

sub FM_L4_CTL_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_L4_CTL_STRIDE                             =  1024; #
our $FM_FRAME_SERVE_WM                            =  (0x1E80B + $FM_L2LOOKUP_BASE); # // 0x19E80B L2LOOKUP_BASE

sub FM_FRAME_SERVE_WM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x1E80B + $FM_L2LOOKUP_BASE); # 0x19E80B L2LOOKUP_BASE
}


sub FM_FRAME_SERVE_WM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FRAME_SERVE_WM_WIDTH                      =  1; #
our $FM_FRAME_SERVE_WM_BITS                       =  31; #

sub FM_CM_TX_SHARED_SMP_USAGE {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00340 + $FM_CM_BASE); # 0x103340 CM_BASE
}


sub FM_CM_TX_SHARED_SMP_USAGE_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_CM_TX_SHARED_SMP_USAGE_WIDTH              =  1; #
our $FM_CM_TX_SHARED_SMP_USAGE_BITS               =  14; #

sub FM_CM_TX_SHARED_SMP_USAGE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}


sub FM_CM_TX_SHARED_SMP_USAGE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CM_TX_SHARED_SMP_USAGE_STRIDE_0           =  1; #
our $FM_CM_TX_SHARED_SMP_USAGE_STRIDE_1           =  2; #

sub FM_PACING_PRI_WM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00001 * (($index1) - 0) + 0x00400 * (($index0) - 0) + 0x00010 + $FM_EPL_BASE); # 0x50010 EPL_BASE
}


sub FM_PACING_PRI_WM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_PACING_PRI_WM_WIDTH                       =  1; #
our $FM_PACING_PRI_WM_BITS                        =  25; #

sub FM_PACING_PRI_WM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}


sub FM_PACING_PRI_WM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}

our $FM_PACING_PRI_WM_STRIDE_0                    =  1024; #
our $FM_PACING_PRI_WM_STRIDE_1                    =  1; #
our $FM_MGR_IM                                    =  (0x02001 + $FM_LSM_BASE); # // 0x42001 LSM_BASE

sub FM_MGR_IM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x02001 + $FM_LSM_BASE); # 0x42001 LSM_BASE
}


sub FM_MGR_IM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0xFFFFFFFF;
}

our $FM_MGR_IM_WIDTH                              =  1; #
our $FM_MGR_IM_BITS                               =  32; #

sub FM_TRIGGER_RX {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00560 + $FM_TRIG_BASE); # 0x106560 TRIG_BASE
}


sub FM_TRIGGER_RX_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_TRIGGER_RX_WIDTH                          =  1; #
our $FM_TRIGGER_RX_BITS                           =  25; #

sub FM_TRIGGER_RX_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}

our $FM_TRIGGER_RX_STRIDE                         =  1; #
our $FM_CPU_MASK                                  =  (0x00001 + $FM_MTABLE_BASE); # // 0xC0001 MTABLE_BASE

sub FM_CPU_MASK {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00001 + $FM_MTABLE_BASE); # 0xC0001 MTABLE_BASE
}


sub FM_CPU_MASK_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_CPU_MASK_WIDTH                            =  1; #
our $FM_CPU_MASK_BITS                             =  25; #

sub FM_CM_TX_SHARED_HOG_WM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00200 + $FM_CM_BASE); # 0x103200 CM_BASE
}


sub FM_CM_TX_SHARED_HOG_WM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00001FFF;
}

our $FM_CM_TX_SHARED_HOG_WM_WIDTH                 =  1; #
our $FM_CM_TX_SHARED_HOG_WM_BITS                  =  13; #

sub FM_CM_TX_SHARED_HOG_WM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}


sub FM_CM_TX_SHARED_HOG_WM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CM_TX_SHARED_HOG_WM_STRIDE_0              =  1; #
our $FM_CM_TX_SHARED_HOG_WM_STRIDE_1              =  4; #

sub FM_AN_RX_CW_Lo {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index) - 0) + 0x00074 + $FM_EPL_BASE + ($word)); # 0x50074 EPL_BASE
}


sub FM_AN_RX_CW_Lo_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_AN_RX_CW_Lo_WIDTH                         =  2; #
our $FM_AN_RX_CW_Lo_BITS                          =  48; #

sub FM_AN_RX_CW_Lo_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_AN_RX_CW_Lo_STRIDE                        =  1024; #

sub FM_MAC_BORDER_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00019 + $FM_EPL_BASE); # 0x50019 EPL_BASE
}


sub FM_MAC_BORDER_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000006;
}

our $FM_MAC_BORDER_CFG_WIDTH                      =  1; #
our $FM_MAC_BORDER_CFG_BITS                       =  3; #

sub FM_MAC_BORDER_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_MAC_BORDER_CFG_STRIDE                     =  1024; #

sub FM_FUSEBOX_REPAIR_SYNC_ADDR {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x02009 + $FM_LSM_BASE); # 0x42009 LSM_BASE
}


sub FM_FUSEBOX_REPAIR_SYNC_ADDR_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_FUSEBOX_REPAIR_SYNC_ADDR_WIDTH            =  1; #
our $FM_FUSEBOX_REPAIR_SYNC_ADDR_BITS             =  24; #

sub FM_FUSEBOX_REPAIR_SYNC_ADDR_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 13;
}

our $FM_FUSEBOX_REPAIR_SYNC_ADDR_STRIDE           =  1; #

sub FM_PACING_STAT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00019 + $FM_EPL_BASE); # 0x50019 EPL_BASE
}


sub FM_PACING_STAT_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_PACING_STAT_WIDTH                         =  1; #
our $FM_PACING_STAT_BITS                          =  25; #

sub FM_PACING_STAT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_PACING_STAT_STRIDE                        =  1024; #

sub FM_RX_PRI_MAP {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00080 + $FM_ASSOC_BASE + ($word)); # 0x101080 ASSOC_BASE
}


sub FM_RX_PRI_MAP_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x33221100;}
    if($word == 1) {return 0x77665544;}
}

our $FM_RX_PRI_MAP_WIDTH                          =  2; #
our $FM_RX_PRI_MAP_BITS                           =  64; #

sub FM_RX_PRI_MAP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_RX_PRI_MAP_STRIDE                         =  2; #

sub FM_PACING_RATE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00018 + $FM_EPL_BASE); # 0x50018 EPL_BASE
}


sub FM_PACING_RATE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00002800;
}

our $FM_PACING_RATE_WIDTH                         =  1; #
our $FM_PACING_RATE_BITS                          =  14; #

sub FM_PACING_RATE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_PACING_RATE_STRIDE                        =  1024; #
our $FM_SHADOW_FUSEBOX_SERIAL_NUMBER              =  (0x02024 + $FM_LSM_BASE); # // 0x42024 LSM_BASE

sub FM_SHADOW_FUSEBOX_SERIAL_NUMBER {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x02024 + $FM_LSM_BASE); # 0x42024 LSM_BASE
}


sub FM_SHADOW_FUSEBOX_SERIAL_NUMBER_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SHADOW_FUSEBOX_SERIAL_NUMBER_WIDTH        =  1; #
our $FM_SHADOW_FUSEBOX_SERIAL_NUMBER_BITS         =  32; #

sub FM_SHADOW_FUSEBOX_REPAIR_SYNC_ADDR {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x02029 + $FM_LSM_BASE); # 0x42029 LSM_BASE
}


sub FM_SHADOW_FUSEBOX_REPAIR_SYNC_ADDR_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SHADOW_FUSEBOX_REPAIR_SYNC_ADDR_WIDTH     =  1; #
our $FM_SHADOW_FUSEBOX_REPAIR_SYNC_ADDR_BITS      =  24; #

sub FM_SHADOW_FUSEBOX_REPAIR_SYNC_ADDR_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 13;
}

our $FM_SHADOW_FUSEBOX_REPAIR_SYNC_ADDR_STRIDE    =  1; #
our $FM_FUSEBOX_INFO                              =  (0x02005 + $FM_LSM_BASE); # // 0x42005 LSM_BASE

sub FM_FUSEBOX_INFO {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x02005 + $FM_LSM_BASE); # 0x42005 LSM_BASE
}


sub FM_FUSEBOX_INFO_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0xC020018C;
}

our $FM_FUSEBOX_INFO_WIDTH                        =  1; #
our $FM_FUSEBOX_INFO_BITS                         =  32; #

sub FM_FID_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x16000 + $FM_L2LOOKUP_BASE + ($word)); # 0x196000 L2LOOKUP_BASE
}


sub FM_FID_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_FID_TABLE_WIDTH                           =  2; #
our $FM_FID_TABLE_BITS                            =  50; #

sub FM_FID_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}

our $FM_FID_TABLE_STRIDE                          =  2; #

sub FM_SHADOW_FUSEBOX_REPAIR_ASYNC {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x02026 + $FM_LSM_BASE); # 0x42026 LSM_BASE
}


sub FM_SHADOW_FUSEBOX_REPAIR_ASYNC_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0xFFFFFFFF;
}

our $FM_SHADOW_FUSEBOX_REPAIR_ASYNC_WIDTH         =  1; #
our $FM_SHADOW_FUSEBOX_REPAIR_ASYNC_BITS          =  32; #

sub FM_SHADOW_FUSEBOX_REPAIR_ASYNC_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 3;
}

our $FM_SHADOW_FUSEBOX_REPAIR_ASYNC_STRIDE        =  1; #
our $FM_CROSSRING_RESET                           =  (0x00400 + $FM_HSM_BASE); # // 0x00400 HSM_BASE

sub FM_CROSSRING_RESET {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00400 + $FM_HSM_BASE); # 0x00400 HSM_BASE
}


sub FM_CROSSRING_RESET_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0000001C;
}

our $FM_CROSSRING_RESET_WIDTH                     =  1; #
our $FM_CROSSRING_RESET_BITS                      =  5; #

sub FM_MAC_VLAN_ETYPE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0003F + $FM_EPL_BASE); # 0x5003F EPL_BASE
}


sub FM_MAC_VLAN_ETYPE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x910088A8;
}

our $FM_MAC_VLAN_ETYPE_WIDTH                      =  1; #
our $FM_MAC_VLAN_ETYPE_BITS                       =  32; #

sub FM_MAC_VLAN_ETYPE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_MAC_VLAN_ETYPE_STRIDE                     =  1024; #

sub FM_SHADOW_FUSEBOX_REPAIR_SYNC_VALUE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x02036 + $FM_LSM_BASE); # 0x42036 LSM_BASE
}


sub FM_SHADOW_FUSEBOX_REPAIR_SYNC_VALUE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SHADOW_FUSEBOX_REPAIR_SYNC_VALUE_WIDTH    =  1; #
our $FM_SHADOW_FUSEBOX_REPAIR_SYNC_VALUE_BITS     =  32; #

sub FM_SHADOW_FUSEBOX_REPAIR_SYNC_VALUE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 13;
}

our $FM_SHADOW_FUSEBOX_REPAIR_SYNC_VALUE_STRIDE   =  1; #
our $FM_SYS_CFG_5                                 =  (0x00004 + $FM_HANDLER_BASE); # // 0x100004 HANDLER_BASE

sub FM_SYS_CFG_5 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00004 + $FM_HANDLER_BASE); # 0x100004 HANDLER_BASE
}


sub FM_SYS_CFG_5_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SYS_CFG_5_WIDTH                           =  1; #
our $FM_SYS_CFG_5_BITS                            =  31; #
our $FM_CM_IM                                     =  (0x0069B + $FM_CM_BASE); # // 0x10369B CM_BASE

sub FM_CM_IM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0069B + $FM_CM_BASE); # 0x10369B CM_BASE
}


sub FM_CM_IM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0xFFFFFFFF;
}

our $FM_CM_IM_WIDTH                               =  1; #
our $FM_CM_IM_BITS                                =  32; #
our $FM_FUSEBOX_SERIAL_NUMBER                     =  (0x02004 + $FM_LSM_BASE); # // 0x42004 LSM_BASE

sub FM_FUSEBOX_SERIAL_NUMBER {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x02004 + $FM_LSM_BASE); # 0x42004 LSM_BASE
}


sub FM_FUSEBOX_SERIAL_NUMBER_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FUSEBOX_SERIAL_NUMBER_WIDTH               =  1; #
our $FM_FUSEBOX_SERIAL_NUMBER_BITS                =  32; #
our $FM_FUSEBOX_UNUSED                            =  (0x02023 + $FM_LSM_BASE); # // 0x42023 LSM_BASE

sub FM_FUSEBOX_UNUSED {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x02023 + $FM_LSM_BASE); # 0x42023 LSM_BASE
}


sub FM_FUSEBOX_UNUSED_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FUSEBOX_UNUSED_WIDTH                      =  1; #
our $FM_FUSEBOX_UNUSED_BITS                       =  32; #

sub FM_STAT_GlobalLowDrop {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00000 + $FM_STATS_RX_CM_BASE + ($word)); # 0x1A0000 STATS_RX_CM_BASE
}


sub FM_STAT_GlobalLowDrop_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return 0;
}

our $FM_STAT_GlobalLowDrop_WIDTH                  =  2; #
our $FM_STAT_GlobalLowDrop_BITS                   =  64; #

sub FM_TX_PRI_MAP_2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00021 + $FM_EPL_BASE); # 0x50021 EPL_BASE
}


sub FM_TX_PRI_MAP_2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0xECA86420;
}

our $FM_TX_PRI_MAP_2_WIDTH                        =  1; #
our $FM_TX_PRI_MAP_2_BITS                         =  32; #

sub FM_TX_PRI_MAP_2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_TX_PRI_MAP_2_STRIDE                       =  1024; #

sub FM_AN_RX_CW_Hi {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index) - 0) + 0x00076 + $FM_EPL_BASE + ($word)); # 0x50076 EPL_BASE
}


sub FM_AN_RX_CW_Hi_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_AN_RX_CW_Hi_WIDTH                         =  2; #
our $FM_AN_RX_CW_Hi_BITS                          =  48; #

sub FM_AN_RX_CW_Hi_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_AN_RX_CW_Hi_STRIDE                        =  1024; #
our $FM_CN_GLOBAL_CFG                             =  (0x00692 + $FM_CM_BASE); # // 0x103692 CM_BASE

sub FM_CN_GLOBAL_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00692 + $FM_CM_BASE); # 0x103692 CM_BASE
}


sub FM_CN_GLOBAL_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x10010028;
}

our $FM_CN_GLOBAL_CFG_WIDTH                       =  1; #
our $FM_CN_GLOBAL_CFG_BITS                        =  32; #
our $FM_SHADOW_FUSEBOX_UNUSED                     =  (0x02043 + $FM_LSM_BASE); # // 0x42043 LSM_BASE

sub FM_SHADOW_FUSEBOX_UNUSED {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x02043 + $FM_LSM_BASE); # 0x42043 LSM_BASE
}


sub FM_SHADOW_FUSEBOX_UNUSED_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SHADOW_FUSEBOX_UNUSED_WIDTH               =  1; #
our $FM_SHADOW_FUSEBOX_UNUSED_BITS                =  32; #

sub FM_AN_TX_CW_Lo {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index) - 0) + 0x00070 + $FM_EPL_BASE + ($word)); # 0x50070 EPL_BASE
}


sub FM_AN_TX_CW_Lo_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_AN_TX_CW_Lo_WIDTH                         =  2; #
our $FM_AN_TX_CW_Lo_BITS                          =  48; #

sub FM_AN_TX_CW_Lo_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_AN_TX_CW_Lo_STRIDE                        =  1024; #

sub FM_CM_RX_SHARED_SMP_HOG_WM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00400 + $FM_CM_BASE); # 0x103400 CM_BASE
}


sub FM_CM_RX_SHARED_SMP_HOG_WM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00001FFF;
}

our $FM_CM_RX_SHARED_SMP_HOG_WM_WIDTH             =  1; #
our $FM_CM_RX_SHARED_SMP_HOG_WM_BITS              =  13; #

sub FM_CM_RX_SHARED_SMP_HOG_WM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}


sub FM_CM_RX_SHARED_SMP_HOG_WM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CM_RX_SHARED_SMP_HOG_WM_STRIDE_0          =  1; #
our $FM_CM_RX_SHARED_SMP_HOG_WM_STRIDE_1          =  2; #

sub FM_STAT_GlobalPrivDrop {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00004 + $FM_STATS_RX_CM_BASE + ($word)); # 0x1A0004 STATS_RX_CM_BASE
}


sub FM_STAT_GlobalPrivDrop_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return 0;
}

our $FM_STAT_GlobalPrivDrop_WIDTH                 =  2; #
our $FM_STAT_GlobalPrivDrop_BITS                  =  64; #
our $FM_AGING_SERVE_WM                            =  (0x1E809 + $FM_L2LOOKUP_BASE); # // 0x19E809 L2LOOKUP_BASE

sub FM_AGING_SERVE_WM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x1E809 + $FM_L2LOOKUP_BASE); # 0x19E809 L2LOOKUP_BASE
}


sub FM_AGING_SERVE_WM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_AGING_SERVE_WM_WIDTH                      =  1; #
our $FM_AGING_SERVE_WM_BITS                       =  31; #
our $FM_CM_IP                                     =  (0x0069A + $FM_CM_BASE); # // 0x10369A CM_BASE

sub FM_CM_IP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0069A + $FM_CM_BASE); # 0x10369A CM_BASE
}


sub FM_CM_IP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0xFFFFFFFF;
}

our $FM_CM_IP_WIDTH                               =  1; #
our $FM_CM_IP_BITS                                =  32; #

sub FM_TRIGGER_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00550 + $FM_TRIG_BASE); # 0x106550 TRIG_BASE
}


sub FM_TRIGGER_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000003;
}

our $FM_TRIGGER_CFG_WIDTH                         =  1; #
our $FM_TRIGGER_CFG_BITS                          =  32; #

sub FM_TRIGGER_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}

our $FM_TRIGGER_CFG_STRIDE                        =  1; #
our $FM_MGR_IP                                    =  (0x02000 + $FM_LSM_BASE); # // 0x42000 LSM_BASE

sub FM_MGR_IP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x02000 + $FM_LSM_BASE); # 0x42000 LSM_BASE
}


sub FM_MGR_IP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MGR_IP_WIDTH                              =  1; #
our $FM_MGR_IP_BITS                               =  32; #

sub FM_MAC_CFG_4 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0001D + $FM_EPL_BASE); # 0x5001D EPL_BASE
}


sub FM_MAC_CFG_4_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x0000FFFF;
}

our $FM_MAC_CFG_4_WIDTH                           =  1; #
our $FM_MAC_CFG_4_BITS                            =  16; #

sub FM_MAC_CFG_4_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_MAC_CFG_4_STRIDE                          =  1024; #

sub FM_CM_RX_SHARED_WM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x004E0 + $FM_CM_BASE); # 0x1034E0 CM_BASE
}


sub FM_CM_RX_SHARED_WM_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x0000021C;
}

our $FM_CM_RX_SHARED_WM_WIDTH                     =  1; #
our $FM_CM_RX_SHARED_WM_BITS                      =  13; #

sub FM_CM_RX_SHARED_WM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}

our $FM_CM_RX_SHARED_WM_STRIDE                    =  1; #
our $FM_SHADOW_FUSEBOX_INFO                       =  (0x02025 + $FM_LSM_BASE); # // 0x42025 LSM_BASE

sub FM_SHADOW_FUSEBOX_INFO {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x02025 + $FM_LSM_BASE); # 0x42025 LSM_BASE
}


sub FM_SHADOW_FUSEBOX_INFO_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0xC020018C;
}

our $FM_SHADOW_FUSEBOX_INFO_WIDTH                 =  1; #
our $FM_SHADOW_FUSEBOX_INFO_BITS                  =  32; #

sub FM_AN_TX_CW_Hi {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index) - 0) + 0x00072 + $FM_EPL_BASE + ($word)); # 0x50072 EPL_BASE
}


sub FM_AN_TX_CW_Hi_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_AN_TX_CW_Hi_WIDTH                         =  2; #
our $FM_AN_TX_CW_Hi_BITS                          =  48; #

sub FM_AN_TX_CW_Hi_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_AN_TX_CW_Hi_STRIDE                        =  1024; #
our $FM_CONTEPL_CTRLSTAT                          =  (0x00402 + $FM_HSM_BASE); # // 0x00402 HSM_BASE

sub FM_CONTEPL_CTRLSTAT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00402 + $FM_HSM_BASE); # 0x00402 HSM_BASE
}


sub FM_CONTEPL_CTRLSTAT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_CONTEPL_CTRLSTAT_WIDTH                    =  1; #
our $FM_CONTEPL_CTRLSTAT_BITS                     =  2; #

sub FM_CM_RX_SHARED_SMP_USAGE {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00440 + $FM_CM_BASE); # 0x103440 CM_BASE
}


sub FM_CM_RX_SHARED_SMP_USAGE_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_CM_RX_SHARED_SMP_USAGE_WIDTH              =  1; #
our $FM_CM_RX_SHARED_SMP_USAGE_BITS               =  14; #

sub FM_CM_RX_SHARED_SMP_USAGE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}


sub FM_CM_RX_SHARED_SMP_USAGE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CM_RX_SHARED_SMP_USAGE_STRIDE_0           =  1; #
our $FM_CM_RX_SHARED_SMP_USAGE_STRIDE_1           =  2; #

sub FM_FUSEBOX_REPAIR_ASYNC {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x02006 + $FM_LSM_BASE); # 0x42006 LSM_BASE
}


sub FM_FUSEBOX_REPAIR_ASYNC_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0xFFFFFFFF;
}

our $FM_FUSEBOX_REPAIR_ASYNC_WIDTH                =  1; #
our $FM_FUSEBOX_REPAIR_ASYNC_BITS                 =  32; #

sub FM_FUSEBOX_REPAIR_ASYNC_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 3;
}

our $FM_FUSEBOX_REPAIR_ASYNC_STRIDE               =  1; #
our $FM_SYS_CFG_2                                 =  (0x00001 + $FM_HANDLER_BASE); # // 0x100001 HANDLER_BASE

sub FM_SYS_CFG_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00001 + $FM_HANDLER_BASE); # 0x100001 HANDLER_BASE
}


sub FM_SYS_CFG_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SYS_CFG_2_WIDTH                           =  1; #
our $FM_SYS_CFG_2_BITS                            =  4; #

sub FM_TRIGGER_CONDITION_AMASK {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x002C0 + $FM_TRIG_BASE); # 0x1062C0 TRIG_BASE
}


sub FM_TRIGGER_CONDITION_AMASK_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_TRIGGER_CONDITION_AMASK_WIDTH             =  1; #
our $FM_TRIGGER_CONDITION_AMASK_BITS              =  32; #

sub FM_TRIGGER_CONDITION_AMASK_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_TRIGGER_CONDITION_AMASK_STRIDE            =  1; #
our $FM_CHIP_MODE                                 =  (0x00102 + $FM_LSM_BASE); # // 0x40102 LSM_BASE

sub FM_CHIP_MODE {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00102 + $FM_LSM_BASE); # 0x40102 LSM_BASE
}


sub FM_CHIP_MODE_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_CHIP_MODE_WIDTH                           =  1; #
our $FM_CHIP_MODE_BITS                            =  21; #

sub FM_AN_TimeoutValue {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0007A + $FM_EPL_BASE); # 0x5007A EPL_BASE
}


sub FM_AN_TimeoutValue_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_AN_TimeoutValue_WIDTH                     =  1; #
our $FM_AN_TimeoutValue_BITS                      =  16; #

sub FM_AN_TimeoutValue_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_AN_TimeoutValue_STRIDE                    =  1024; #

sub FM_VID_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x10000 + $FM_L2LOOKUP_BASE + ($word)); # 0x190000 L2LOOKUP_BASE
}


sub FM_VID_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_VID_TABLE_WIDTH                           =  3; #
our $FM_VID_TABLE_BITS                            =  78; #

sub FM_VID_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}

our $FM_VID_TABLE_STRIDE                          =  4; #

our $FM_LCI_CFG                                   =  (0x00001 + $FM_HSM_BASE); # // 0x00001 HSM_BASE

sub FM_LCI_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00001 + $FM_HSM_BASE); # 0x00001 HSM_BASE
}


sub FM_LCI_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000004;
}

our $FM_LCI_CFG_WIDTH                             =  1; #
our $FM_LCI_CFG_BITS                              =  17; #
our $FM_LCI_RX_FIFO                               =  (0x00002 + $FM_HSM_BASE); # // 0x00002 HSM_BASE

sub FM_LCI_RX_FIFO {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00002 + $FM_HSM_BASE); # 0x00002 HSM_BASE
}


sub FM_LCI_RX_FIFO_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_LCI_RX_FIFO_WIDTH                         =  1; #
our $FM_LCI_RX_FIFO_BITS                          =  32; #
our $FM_LCI_TX_FIFO                               =  (0x00003 + $FM_HSM_BASE); # // 0x00003 HSM_BASE

sub FM_LCI_TX_FIFO {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00003 + $FM_HSM_BASE); # 0x00003 HSM_BASE
}


sub FM_LCI_TX_FIFO_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_LCI_TX_FIFO_WIDTH                         =  1; #
our $FM_LCI_TX_FIFO_BITS                          =  32; #
our $FM_LCI_IP                                    =  (0x00004 + $FM_HSM_BASE); # // 0x00004 HSM_BASE

sub FM_LCI_IP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00004 + $FM_HSM_BASE); # 0x00004 HSM_BASE
}


sub FM_LCI_IP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_LCI_IP_WIDTH                              =  1; #
our $FM_LCI_IP_BITS                               =  8; #
our $FM_LCI_IM                                    =  (0x00005 + $FM_HSM_BASE); # // 0x00005 HSM_BASE

sub FM_LCI_IM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00005 + $FM_HSM_BASE); # 0x00005 HSM_BASE
}


sub FM_LCI_IM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000006;
}

our $FM_LCI_IM_WIDTH                              =  1; #
our $FM_LCI_IM_BITS                               =  8; #
our $FM_LCI_STATUS                                =  (0x00006 + $FM_HSM_BASE); # // 0x00006 HSM_BASE

sub FM_LCI_STATUS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00006 + $FM_HSM_BASE); # 0x00006 HSM_BASE
}


sub FM_LCI_STATUS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000001;
}

our $FM_LCI_STATUS_WIDTH                          =  1; #
our $FM_LCI_STATUS_BITS                           =  16; #
our $FM_SCAN_DATA_OUT                             =  (0x00100 + $FM_HSM_BASE); # // 0x00100 HSM_BASE

sub FM_SCAN_DATA_OUT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00100 + $FM_HSM_BASE); # 0x00100 HSM_BASE
}


sub FM_SCAN_DATA_OUT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SCAN_DATA_OUT_WIDTH                       =  1; #
our $FM_SCAN_DATA_OUT_BITS                        =  32; #
our $FM_SCAN_SEL                                  =  (0x00101 + $FM_HSM_BASE); # // 0x00101 HSM_BASE

sub FM_SCAN_SEL {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00101 + $FM_HSM_BASE); # 0x00101 HSM_BASE
}


sub FM_SCAN_SEL_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SCAN_SEL_WIDTH                            =  1; #
our $FM_SCAN_SEL_BITS                             =  32; #
our $FM_SCAN_FREQ_MULT                            =  (0x00102 + $FM_HSM_BASE); # // 0x00102 HSM_BASE

sub FM_SCAN_FREQ_MULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00102 + $FM_HSM_BASE); # 0x00102 HSM_BASE
}


sub FM_SCAN_FREQ_MULT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SCAN_FREQ_MULT_WIDTH                      =  1; #
our $FM_SCAN_FREQ_MULT_BITS                       =  8; #
our $FM_SCAN_CTRL                                 =  (0x00103 + $FM_HSM_BASE); # // 0x00103 HSM_BASE

sub FM_SCAN_CTRL {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00103 + $FM_HSM_BASE); # 0x00103 HSM_BASE
}


sub FM_SCAN_CTRL_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SCAN_CTRL_WIDTH                           =  1; #
our $FM_SCAN_CTRL_BITS                            =  7; #
our $FM_SCAN_DATA_IN                              =  (0x00104 + $FM_HSM_BASE); # // 0x00104 HSM_BASE

sub FM_SCAN_DATA_IN {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00104 + $FM_HSM_BASE); # 0x00104 HSM_BASE
}


sub FM_SCAN_DATA_IN_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SCAN_DATA_IN_WIDTH                        =  1; #
our $FM_SCAN_DATA_IN_BITS                         =  32; #
our $FM_LAST_FATAL_CODE                           =  (0x00200 + $FM_HSM_BASE); # // 0x00200 HSM_BASE

sub FM_LAST_FATAL_CODE {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00200 + $FM_HSM_BASE); # 0x00200 HSM_BASE
}


sub FM_LAST_FATAL_CODE_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_LAST_FATAL_CODE_WIDTH                     =  1; #
our $FM_LAST_FATAL_CODE_BITS                      =  8; #
our $FM_INTERRUPT_DETECT                          =  (0x00201 + $FM_HSM_BASE); # // 0x00201 HSM_BASE

sub FM_INTERRUPT_DETECT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00201 + $FM_HSM_BASE); # 0x00201 HSM_BASE
}


sub FM_INTERRUPT_DETECT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_INTERRUPT_DETECT_WIDTH                    =  1; #
our $FM_INTERRUPT_DETECT_BITS                     =  32; #
our $FM_FATAL_COUNT                               =  (0x00202 + $FM_HSM_BASE); # // 0x00202 HSM_BASE

sub FM_FATAL_COUNT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00202 + $FM_HSM_BASE); # 0x00202 HSM_BASE
}


sub FM_FATAL_COUNT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FATAL_COUNT_WIDTH                         =  1; #
our $FM_FATAL_COUNT_BITS                          =  8; #
our $FM_SOFT_RESET                                =  (0x00203 + $FM_HSM_BASE); # // 0x00203 HSM_BASE

sub FM_SOFT_RESET {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00203 + $FM_HSM_BASE); # 0x00203 HSM_BASE
}


sub FM_SOFT_RESET_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000008;
}

our $FM_SOFT_RESET_WIDTH                          =  1; #
our $FM_SOFT_RESET_BITS                           =  4; #
our $FM_WATCHDOG_CFG                              =  (0x00204 + $FM_HSM_BASE); # // 0x00204 HSM_BASE

sub FM_WATCHDOG_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00204 + $FM_HSM_BASE); # 0x00204 HSM_BASE
}


sub FM_WATCHDOG_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_WATCHDOG_CFG_WIDTH                        =  1; #
our $FM_WATCHDOG_CFG_BITS                         =  1; #
our $FM_PLL_FH_STAT                               =  (0x00205 + $FM_HSM_BASE); # // 0x00205 HSM_BASE

sub FM_PLL_FH_STAT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00205 + $FM_HSM_BASE); # 0x00205 HSM_BASE
}


sub FM_PLL_FH_STAT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PLL_FH_STAT_WIDTH                         =  1; #
our $FM_PLL_FH_STAT_BITS                          =  1; #
our $FM_PLL_FH_CTRL                               =  (0x00206 + $FM_HSM_BASE); # // 0x00206 HSM_BASE

sub FM_PLL_FH_CTRL {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00206 + $FM_HSM_BASE); # 0x00206 HSM_BASE
}


sub FM_PLL_FH_CTRL_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00002142;
}

our $FM_PLL_FH_CTRL_WIDTH                         =  1; #
our $FM_PLL_FH_CTRL_BITS                          =  17; #
our $FM_VITAL_PRODUCT_DATA                        =  (0x00304 + $FM_HSM_BASE); # // 0x00304 HSM_BASE

sub FM_VITAL_PRODUCT_DATA {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00304 + $FM_HSM_BASE); # 0x00304 HSM_BASE
}


sub FM_VITAL_PRODUCT_DATA_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0AE19000;
}

our $FM_VITAL_PRODUCT_DATA_WIDTH                  =  1; #
our $FM_VITAL_PRODUCT_DATA_BITS                   =  28; #
our $FM_PREFETCH_CFG                              =  (0x00401 + $FM_HSM_BASE); # // 0x00401 HSM_BASE

sub FM_PREFETCH_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00401 + $FM_HSM_BASE); # 0x00401 HSM_BASE
}


sub FM_PREFETCH_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000008;
}

our $FM_PREFETCH_CFG_WIDTH                        =  1; #
our $FM_PREFETCH_CFG_BITS                         =  4; #
our $FM_LSM_INT_DETECT                            =  (0x00000 + $FM_LSM_BASE); # // 0x40000 LSM_BASE

sub FM_LSM_INT_DETECT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00000 + $FM_LSM_BASE); # 0x40000 LSM_BASE
}


sub FM_LSM_INT_DETECT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_LSM_INT_DETECT_WIDTH                      =  1; #
our $FM_LSM_INT_DETECT_BITS                       =  6; #
our $FM_GLOBAL_EPL_INT_DETECT                     =  (0x00001 + $FM_LSM_BASE); # // 0x40001 LSM_BASE

sub FM_GLOBAL_EPL_INT_DETECT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00001 + $FM_LSM_BASE); # 0x40001 LSM_BASE
}


sub FM_GLOBAL_EPL_INT_DETECT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_GLOBAL_EPL_INT_DETECT_WIDTH               =  1; #
our $FM_GLOBAL_EPL_INT_DETECT_BITS                =  24; #
our $FM_PERR_IP                                   =  (0x00002 + $FM_LSM_BASE); # // 0x40002 LSM_BASE

sub FM_PERR_IP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00002 + $FM_LSM_BASE); # 0x40002 LSM_BASE
}


sub FM_PERR_IP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PERR_IP_WIDTH                             =  1; #
our $FM_PERR_IP_BITS                              =  16; #
our $FM_PERR_IM                                   =  (0x00003 + $FM_LSM_BASE); # // 0x40003 LSM_BASE

sub FM_PERR_IM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00003 + $FM_LSM_BASE); # 0x40003 LSM_BASE
}


sub FM_PERR_IM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0000FFFF;
}

our $FM_PERR_IM_WIDTH                             =  1; #
our $FM_PERR_IM_BITS                              =  16; #
our $FM_SW_IP                                     =  (0x00004 + $FM_LSM_BASE); # // 0x40004 LSM_BASE

sub FM_SW_IP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00004 + $FM_LSM_BASE); # 0x40004 LSM_BASE
}


sub FM_SW_IP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SW_IP_WIDTH                               =  1; #
our $FM_SW_IP_BITS                                =  32; #
our $FM_SW_IM                                     =  (0x00005 + $FM_LSM_BASE); # // 0x40005 LSM_BASE

sub FM_SW_IM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00005 + $FM_LSM_BASE); # 0x40005 LSM_BASE
}


sub FM_SW_IM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0xFFFFFFFF;
}

our $FM_SW_IM_WIDTH                               =  1; #
our $FM_SW_IM_BITS                                =  32; #
our $FM_FRAME_TIME_OUT                            =  (0x00101 + $FM_LSM_BASE); # // 0x40101 LSM_BASE

sub FM_FRAME_TIME_OUT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00101 + $FM_LSM_BASE); # 0x40101 LSM_BASE
}


sub FM_FRAME_TIME_OUT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x000088B8;
}

our $FM_FRAME_TIME_OUT_WIDTH                      =  1; #
our $FM_FRAME_TIME_OUT_BITS                       =  28; #
our $FM_BOOT_CTRL                                 =  (0x00102 + $FM_LSM_BASE); # // 0x40102 LSM_BASE

sub FM_BOOT_CTRL {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00102 + $FM_LSM_BASE); # 0x40102 LSM_BASE
}


sub FM_BOOT_CTRL_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_BOOT_CTRL_WIDTH                           =  1; #
our $FM_BOOT_CTRL_BITS                            =  21; #
our $FM_BOOT_STATUS                               =  (0x00103 + $FM_LSM_BASE); # // 0x40103 LSM_BASE

sub FM_BOOT_STATUS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00103 + $FM_LSM_BASE); # 0x40103 LSM_BASE
}


sub FM_BOOT_STATUS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_BOOT_STATUS_WIDTH                         =  1; #
our $FM_BOOT_STATUS_BITS                          =  1; #
our $FM_CLK_MULT_1                                =  (0x00104 + $FM_LSM_BASE); # // 0x40104 LSM_BASE

sub FM_CLK_MULT_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00104 + $FM_LSM_BASE); # 0x40104 LSM_BASE
}


sub FM_CLK_MULT_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000019;
}

our $FM_CLK_MULT_1_WIDTH                          =  1; #
our $FM_CLK_MULT_1_BITS                           =  8; #
our $FM_FUSEBOX_CFG                               =  (0x00105 + $FM_LSM_BASE); # // 0x40105 LSM_BASE

sub FM_FUSEBOX_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00105 + $FM_LSM_BASE); # 0x40105 LSM_BASE
}


sub FM_FUSEBOX_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000040;
}

our $FM_FUSEBOX_CFG_WIDTH                         =  1; #
our $FM_FUSEBOX_CFG_BITS                          =  8; #

sub FM_VPD_INFO_1 {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00108 + $FM_LSM_BASE + ($word)); # 0x40108 LSM_BASE
}


sub FM_VPD_INFO_1_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00900000;}
}

our $FM_VPD_INFO_1_WIDTH                          =  2; #
our $FM_VPD_INFO_1_BITS                           =  64; #

sub FM_VPD_INFO_2 {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x0010A + $FM_LSM_BASE + ($word)); # 0x4010A LSM_BASE
}


sub FM_VPD_INFO_2_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000215;}
    if($word == 1) {return 0x00000000;}
}

our $FM_VPD_INFO_2_WIDTH                          =  2; #
our $FM_VPD_INFO_2_BITS                           =  11; #

sub FM_VPD_ASYNC_RAM_REPAIR {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0010C + $FM_LSM_BASE); # 0x4010C LSM_BASE
}


sub FM_VPD_ASYNC_RAM_REPAIR_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0xFFFFFFFF;
}

our $FM_VPD_ASYNC_RAM_REPAIR_WIDTH                =  1; #
our $FM_VPD_ASYNC_RAM_REPAIR_BITS                 =  32; #

sub FM_VPD_ASYNC_RAM_REPAIR_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}

our $FM_VPD_ASYNC_RAM_REPAIR_STRIDE               =  1; #

sub FM_VPD_SYNC_RAM_REPAIR {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00110 + $FM_LSM_BASE + ($word)); # 0x40110 LSM_BASE
}


sub FM_VPD_SYNC_RAM_REPAIR_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_VPD_SYNC_RAM_REPAIR_WIDTH                 =  2; #
our $FM_VPD_SYNC_RAM_REPAIR_BITS                  =  56; #

sub FM_VPD_SYNC_RAM_REPAIR_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}

our $FM_VPD_SYNC_RAM_REPAIR_STRIDE                =  2; #
our $FM_GPIO_CFG                                  =  (0x00134 + $FM_LSM_BASE); # // 0x40134 LSM_BASE

sub FM_GPIO_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00134 + $FM_LSM_BASE); # 0x40134 LSM_BASE
}


sub FM_GPIO_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_GPIO_CFG_WIDTH                            =  1; #
our $FM_GPIO_CFG_BITS                             =  32; #
our $FM_GPIO_DATA                                 =  (0x00135 + $FM_LSM_BASE); # // 0x40135 LSM_BASE

sub FM_GPIO_DATA {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00135 + $FM_LSM_BASE); # 0x40135 LSM_BASE
}


sub FM_GPIO_DATA_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_GPIO_DATA_WIDTH                           =  1; #
our $FM_GPIO_DATA_BITS                            =  32; #
our $FM_GPIO_IP                                   =  (0x00136 + $FM_LSM_BASE); # // 0x40136 LSM_BASE

sub FM_GPIO_IP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00136 + $FM_LSM_BASE); # 0x40136 LSM_BASE
}


sub FM_GPIO_IP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_GPIO_IP_WIDTH                             =  1; #
our $FM_GPIO_IP_BITS                              =  32; #
our $FM_GPIO_IM                                   =  (0x00137 + $FM_LSM_BASE); # // 0x40137 LSM_BASE

sub FM_GPIO_IM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00137 + $FM_LSM_BASE); # 0x40137 LSM_BASE
}


sub FM_GPIO_IM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0xFFFFFFFF;
}

our $FM_GPIO_IM_WIDTH                             =  1; #
our $FM_GPIO_IM_BITS                              =  32; #
our $FM_I2C_CFG                                   =  (0x00138 + $FM_LSM_BASE); # // 0x40138 LSM_BASE

sub FM_I2C_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00138 + $FM_LSM_BASE); # 0x40138 LSM_BASE
}


sub FM_I2C_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x29010081;
}

our $FM_I2C_CFG_WIDTH                             =  1; #
our $FM_I2C_CFG_BITS                              =  32; #

sub FM_I2C_DATA {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00139 + $FM_LSM_BASE); # 0x40139 LSM_BASE
}


sub FM_I2C_DATA_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_I2C_DATA_WIDTH                            =  1; #
our $FM_I2C_DATA_BITS                             =  32; #

sub FM_I2C_DATA_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}

our $FM_I2C_DATA_STRIDE                           =  1; #
our $FM_I2C_CTRL                                  =  (0x0013B + $FM_LSM_BASE); # // 0x4013B LSM_BASE

sub FM_I2C_CTRL {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0013B + $FM_LSM_BASE); # 0x4013B LSM_BASE
}


sub FM_I2C_CTRL_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00780000;
}

our $FM_I2C_CTRL_WIDTH                            =  1; #
our $FM_I2C_CTRL_BITS                             =  24; #
our $FM_MDIO_CFG                                  =  (0x0013C + $FM_LSM_BASE); # // 0x4013C LSM_BASE

sub FM_MDIO_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0013C + $FM_LSM_BASE); # 0x4013C LSM_BASE
}


sub FM_MDIO_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00003010;
}

our $FM_MDIO_CFG_WIDTH                            =  1; #
our $FM_MDIO_CFG_BITS                             =  32; #
our $FM_MDIO_DATA                                 =  (0x0013D + $FM_LSM_BASE); # // 0x4013D LSM_BASE

sub FM_MDIO_DATA {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0013D + $FM_LSM_BASE); # 0x4013D LSM_BASE
}


sub FM_MDIO_DATA_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MDIO_DATA_WIDTH                           =  1; #
our $FM_MDIO_DATA_BITS                            =  32; #
our $FM_MDIO_CTRL                                 =  (0x0013E + $FM_LSM_BASE); # // 0x4013E LSM_BASE

sub FM_MDIO_CTRL {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0013E + $FM_LSM_BASE); # 0x4013E LSM_BASE
}


sub FM_MDIO_CTRL_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MDIO_CTRL_WIDTH                           =  1; #
our $FM_MDIO_CTRL_BITS                            =  32; #
our $FM_LED_CFG                                   =  (0x0013F + $FM_LSM_BASE); # // 0x4013F LSM_BASE

sub FM_LED_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0013F + $FM_LSM_BASE); # 0x4013F LSM_BASE
}


sub FM_LED_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_LED_CFG_WIDTH                             =  1; #
our $FM_LED_CFG_BITS                              =  20; #

sub FM_EPL_PORT_CTRL {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00160 + $FM_LSM_BASE); # 0x40160 LSM_BASE
}


sub FM_EPL_PORT_CTRL_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_EPL_PORT_CTRL_WIDTH                       =  1; #
our $FM_EPL_PORT_CTRL_BITS                        =  4; #

sub FM_EPL_PORT_CTRL_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_EPL_PORT_CTRL_STRIDE                      =  1; #

sub FM_FUSEBOX {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00180 + $FM_LSM_BASE); # 0x40180 LSM_BASE
}


sub FM_FUSEBOX_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_FUSEBOX_WIDTH                             =  1; #
our $FM_FUSEBOX_BITS                              =  8; #

sub FM_FUSEBOX_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}

our $FM_FUSEBOX_STRIDE                            =  1; #
our $FM_ASYNC_SCAN_CFG                            =  (0x00200 + $FM_LSM_BASE); # // 0x40200 LSM_BASE

sub FM_ASYNC_SCAN_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00200 + $FM_LSM_BASE); # 0x40200 LSM_BASE
}


sub FM_ASYNC_SCAN_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00010000;
}

our $FM_ASYNC_SCAN_CFG_WIDTH                      =  1; #
our $FM_ASYNC_SCAN_CFG_BITS                       =  32; #
our $FM_ASYNC_SCAN_CMD                            =  (0x00201 + $FM_LSM_BASE); # // 0x40201 LSM_BASE

sub FM_ASYNC_SCAN_CMD {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00201 + $FM_LSM_BASE); # 0x40201 LSM_BASE
}


sub FM_ASYNC_SCAN_CMD_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_ASYNC_SCAN_CMD_WIDTH                      =  1; #
our $FM_ASYNC_SCAN_CMD_BITS                       =  16; #
our $FM_ASYNC_SCAN_IN                             =  (0x00202 + $FM_LSM_BASE); # // 0x40202 LSM_BASE

sub FM_ASYNC_SCAN_IN {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00202 + $FM_LSM_BASE); # 0x40202 LSM_BASE
}


sub FM_ASYNC_SCAN_IN_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_ASYNC_SCAN_IN_WIDTH                       =  1; #
our $FM_ASYNC_SCAN_IN_BITS                        =  16; #
our $FM_ASYNC_SCAN_OUT                            =  (0x00203 + $FM_LSM_BASE); # // 0x40203 LSM_BASE

sub FM_ASYNC_SCAN_OUT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00203 + $FM_LSM_BASE); # 0x40203 LSM_BASE
}


sub FM_ASYNC_SCAN_OUT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_ASYNC_SCAN_OUT_WIDTH                      =  1; #
our $FM_ASYNC_SCAN_OUT_BITS                       =  16; #

sub FM_CRM_CFG_COUNTER {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x01200 + $FM_LSM_BASE); # 0x41200 LSM_BASE
}


sub FM_CRM_CFG_COUNTER_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CRM_CFG_COUNTER_WIDTH                     =  1; #
our $FM_CRM_CFG_COUNTER_BITS                      =  25; #

sub FM_CRM_CFG_COUNTER_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 256;
}

our $FM_CRM_CFG_COUNTER_STRIDE                    =  1; #

sub FM_CRM_CFG_WINDOW {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x01300 + $FM_LSM_BASE); # 0x41300 LSM_BASE
}


sub FM_CRM_CFG_WINDOW_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CRM_CFG_WINDOW_WIDTH                      =  1; #
our $FM_CRM_CFG_WINDOW_BITS                       =  24; #

sub FM_CRM_CFG_WINDOW_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 256;
}

our $FM_CRM_CFG_WINDOW_STRIDE                     =  1; #

sub FM_CRM_CFG_LIMIT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x01400 + $FM_LSM_BASE); # 0x41400 LSM_BASE
}


sub FM_CRM_CFG_LIMIT_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CRM_CFG_LIMIT_WIDTH                       =  1; #
our $FM_CRM_CFG_LIMIT_BITS                        =  32; #

sub FM_CRM_CFG_LIMIT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 256;
}

our $FM_CRM_CFG_LIMIT_STRIDE                      =  1; #

sub FM_CRM_LAST_COUNT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x01500 + $FM_LSM_BASE); # 0x41500 LSM_BASE
}


sub FM_CRM_LAST_COUNT_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CRM_LAST_COUNT_WIDTH                      =  1; #
our $FM_CRM_LAST_COUNT_BITS                       =  32; #

sub FM_CRM_LAST_COUNT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 256;
}

our $FM_CRM_LAST_COUNT_STRIDE                     =  1; #

sub FM_CRM_EXCEED_COUNT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x01600 + $FM_LSM_BASE); # 0x41600 LSM_BASE
}


sub FM_CRM_EXCEED_COUNT_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CRM_EXCEED_COUNT_WIDTH                    =  1; #
our $FM_CRM_EXCEED_COUNT_BITS                     =  32; #

sub FM_CRM_EXCEED_COUNT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 256;
}

our $FM_CRM_EXCEED_COUNT_STRIDE                   =  1; #
our $FM_CRM_CFG                                   =  (0x01700 + $FM_LSM_BASE); # // 0x41700 LSM_BASE

sub FM_CRM_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01700 + $FM_LSM_BASE); # 0x41700 LSM_BASE
}


sub FM_CRM_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000001;
}

our $FM_CRM_CFG_WIDTH                             =  1; #
our $FM_CRM_CFG_BITS                              =  16; #
our $FM_CRM_INT_DETECT                            =  (0x01701 + $FM_LSM_BASE); # // 0x41701 LSM_BASE

sub FM_CRM_INT_DETECT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01701 + $FM_LSM_BASE); # 0x41701 LSM_BASE
}


sub FM_CRM_INT_DETECT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_CRM_INT_DETECT_WIDTH                      =  1; #
our $FM_CRM_INT_DETECT_BITS                       =  8; #

sub FM_CRM_IP {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x01710 + $FM_LSM_BASE); # 0x41710 LSM_BASE
}


sub FM_CRM_IP_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CRM_IP_WIDTH                              =  1; #
our $FM_CRM_IP_BITS                               =  32; #

sub FM_CRM_IP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}

our $FM_CRM_IP_STRIDE                             =  1; #

sub FM_CRM_IM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x01720 + $FM_LSM_BASE); # 0x41720 LSM_BASE
}


sub FM_CRM_IM_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0xFFFFFFFF;
}

our $FM_CRM_IM_WIDTH                              =  1; #
our $FM_CRM_IM_BITS                               =  32; #

sub FM_CRM_IM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}

our $FM_CRM_IM_STRIDE                             =  1; #
our $FM_SOT_CFG                                   =  (0x01800 + $FM_LSM_BASE); # // 0x41800 LSM_BASE

sub FM_SOT_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01800 + $FM_LSM_BASE); # 0x41800 LSM_BASE
}


sub FM_SOT_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SOT_CFG_WIDTH                             =  1; #
our $FM_SOT_CFG_BITS                              =  1; #

sub FM_SERDES_CTRL_1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00000 + $FM_EPL_BASE); # 0x50000 EPL_BASE
}


sub FM_SERDES_CTRL_1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SERDES_CTRL_1_WIDTH                       =  1; #
our $FM_SERDES_CTRL_1_BITS                        =  32; #

sub FM_SERDES_CTRL_1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_SERDES_CTRL_1_STRIDE                      =  1024; #

sub FM_SERDES_CTRL_2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00001 + $FM_EPL_BASE); # 0x50001 EPL_BASE
}


sub FM_SERDES_CTRL_2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x0003FF00;
}

our $FM_SERDES_CTRL_2_WIDTH                       =  1; #
our $FM_SERDES_CTRL_2_BITS                        =  27; #

sub FM_SERDES_CTRL_2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_SERDES_CTRL_2_STRIDE                      =  1024; #

sub FM_SERDES_CTRL_3 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00002 + $FM_EPL_BASE); # 0x50002 EPL_BASE
}


sub FM_SERDES_CTRL_3_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x1321312D;
}

our $FM_SERDES_CTRL_3_WIDTH                       =  1; #
our $FM_SERDES_CTRL_3_BITS                        =  32; #

sub FM_SERDES_CTRL_3_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_SERDES_CTRL_3_STRIDE                      =  1024; #

sub FM_SERDES_TEST_MODE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00003 + $FM_EPL_BASE); # 0x50003 EPL_BASE
}


sub FM_SERDES_TEST_MODE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000060;
}

our $FM_SERDES_TEST_MODE_WIDTH                    =  1; #
our $FM_SERDES_TEST_MODE_BITS                     =  8; #

sub FM_SERDES_TEST_MODE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_SERDES_TEST_MODE_STRIDE                   =  1024; #

sub FM_SERDES_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00004 + $FM_EPL_BASE); # 0x50004 EPL_BASE
}


sub FM_SERDES_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SERDES_STATUS_WIDTH                       =  1; #
our $FM_SERDES_STATUS_BITS                        =  5; #

sub FM_SERDES_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_SERDES_STATUS_STRIDE                      =  1024; #

sub FM_SERDES_IP {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00005 + $FM_EPL_BASE); # 0x50005 EPL_BASE
}


sub FM_SERDES_IP_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SERDES_IP_WIDTH                           =  1; #
our $FM_SERDES_IP_BITS                            =  32; #

sub FM_SERDES_IP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_SERDES_IP_STRIDE                          =  1024; #

sub FM_SERDES_IM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00006 + $FM_EPL_BASE); # 0x50006 EPL_BASE
}


sub FM_SERDES_IM_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000FFF;
}

our $FM_SERDES_IM_WIDTH                           =  1; #
our $FM_SERDES_IM_BITS                            =  12; #

sub FM_SERDES_IM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_SERDES_IM_STRIDE                          =  1024; #

sub FM_SERDES_BIST_ERR_CNT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00007 + $FM_EPL_BASE); # 0x50007 EPL_BASE
}


sub FM_SERDES_BIST_ERR_CNT_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SERDES_BIST_ERR_CNT_WIDTH                 =  1; #
our $FM_SERDES_BIST_ERR_CNT_BITS                  =  32; #

sub FM_SERDES_BIST_ERR_CNT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_SERDES_BIST_ERR_CNT_STRIDE                =  1024; #

sub FM_PCS_CFG_1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00009 + $FM_EPL_BASE); # 0x50009 EPL_BASE
}


sub FM_PCS_CFG_1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x80000C0A;
}

our $FM_PCS_CFG_1_WIDTH                           =  1; #
our $FM_PCS_CFG_1_BITS                            =  32; #

sub FM_PCS_CFG_1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_PCS_CFG_1_STRIDE                          =  1024; #

sub FM_PCS_CFG_2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0000A + $FM_EPL_BASE); # 0x5000A EPL_BASE
}


sub FM_PCS_CFG_2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000001;
}

our $FM_PCS_CFG_2_WIDTH                           =  1; #
our $FM_PCS_CFG_2_BITS                            =  24; #

sub FM_PCS_CFG_2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_PCS_CFG_2_STRIDE                          =  1024; #

sub FM_PCS_CFG_3 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0000B + $FM_EPL_BASE); # 0x5000B EPL_BASE
}


sub FM_PCS_CFG_3_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000002;
}

our $FM_PCS_CFG_3_WIDTH                           =  1; #
our $FM_PCS_CFG_3_BITS                            =  24; #

sub FM_PCS_CFG_3_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_PCS_CFG_3_STRIDE                          =  1024; #

sub FM_PCS_CFG_4 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0000C + $FM_EPL_BASE); # 0x5000C EPL_BASE
}


sub FM_PCS_CFG_4_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_PCS_CFG_4_WIDTH                           =  1; #
our $FM_PCS_CFG_4_BITS                            =  24; #

sub FM_PCS_CFG_4_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_PCS_CFG_4_STRIDE                          =  1024; #

sub FM_PCS_CFG_5 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0000D + $FM_EPL_BASE); # 0x5000D EPL_BASE
}


sub FM_PCS_CFG_5_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_PCS_CFG_5_WIDTH                           =  1; #
our $FM_PCS_CFG_5_BITS                            =  24; #

sub FM_PCS_CFG_5_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_PCS_CFG_5_STRIDE                          =  1024; #

sub FM_PCS_IP {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0000E + $FM_EPL_BASE); # 0x5000E EPL_BASE
}


sub FM_PCS_IP_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_PCS_IP_WIDTH                              =  1; #
our $FM_PCS_IP_BITS                               =  18; #

sub FM_PCS_IP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_PCS_IP_STRIDE                             =  1024; #

sub FM_PCS_IM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0000F + $FM_EPL_BASE); # 0x5000F EPL_BASE
}


sub FM_PCS_IM_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x0003FFFF;
}

our $FM_PCS_IM_WIDTH                              =  1; #
our $FM_PCS_IM_BITS                               =  18; #

sub FM_PCS_IM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_PCS_IM_STRIDE                             =  1024; #

sub FM_SYNCBUF_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00018 + $FM_EPL_BASE); # 0x50018 EPL_BASE
}


sub FM_SYNCBUF_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00002800;
}

our $FM_SYNCBUF_CFG_WIDTH                         =  1; #
our $FM_SYNCBUF_CFG_BITS                          =  14; #

sub FM_SYNCBUF_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_SYNCBUF_CFG_STRIDE                        =  1024; #

sub FM_MAC_CFG_1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0001A + $FM_EPL_BASE); # 0x5001A EPL_BASE
}


sub FM_MAC_CFG_1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x10180000;
}

our $FM_MAC_CFG_1_WIDTH                           =  1; #
our $FM_MAC_CFG_1_BITS                            =  30; #

sub FM_MAC_CFG_1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_MAC_CFG_1_STRIDE                          =  1024; #

sub FM_MAC_CFG_2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0001B + $FM_EPL_BASE); # 0x5001B EPL_BASE
}


sub FM_MAC_CFG_2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x0007217C;
}

our $FM_MAC_CFG_2_WIDTH                           =  1; #
our $FM_MAC_CFG_2_BITS                            =  32; #

sub FM_MAC_CFG_2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_MAC_CFG_2_STRIDE                          =  1024; #

sub FM_MAC_CFG_3 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0001C + $FM_EPL_BASE); # 0x5001C EPL_BASE
}


sub FM_MAC_CFG_3_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x4010FFFF;
}

our $FM_MAC_CFG_3_WIDTH                           =  1; #
our $FM_MAC_CFG_3_BITS                            =  32; #

sub FM_MAC_CFG_3_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_MAC_CFG_3_STRIDE                          =  1024; #

sub FM_MAC_CFG_5 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0001E + $FM_EPL_BASE); # 0x5001E EPL_BASE
}


sub FM_MAC_CFG_5_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAC_CFG_5_WIDTH                           =  1; #
our $FM_MAC_CFG_5_BITS                            =  32; #

sub FM_MAC_CFG_5_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_MAC_CFG_5_STRIDE                          =  1024; #

sub FM_MAC_CFG_6 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0001F + $FM_EPL_BASE); # 0x5001F EPL_BASE
}


sub FM_MAC_CFG_6_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAC_CFG_6_WIDTH                           =  1; #
our $FM_MAC_CFG_6_BITS                            =  16; #

sub FM_MAC_CFG_6_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_MAC_CFG_6_STRIDE                          =  1024; #

sub FM_TX_VPRI_MAP_1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00020 + $FM_EPL_BASE); # 0x50020 EPL_BASE
}


sub FM_TX_VPRI_MAP_1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0xECA86420;
}

our $FM_TX_VPRI_MAP_1_WIDTH                       =  1; #
our $FM_TX_VPRI_MAP_1_BITS                        =  32; #

sub FM_TX_VPRI_MAP_1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_TX_VPRI_MAP_1_STRIDE                      =  1024; #

sub FM_TX_VPRI_MAP_2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00021 + $FM_EPL_BASE); # 0x50021 EPL_BASE
}


sub FM_TX_VPRI_MAP_2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0xECA86420;
}

our $FM_TX_VPRI_MAP_2_WIDTH                       =  1; #
our $FM_TX_VPRI_MAP_2_BITS                        =  32; #

sub FM_TX_VPRI_MAP_2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_TX_VPRI_MAP_2_STRIDE                      =  1024; #

sub FM_MAC_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00022 + $FM_EPL_BASE); # 0x50022 EPL_BASE
}


sub FM_MAC_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAC_STATUS_WIDTH                          =  1; #
our $FM_MAC_STATUS_BITS                           =  2; #

sub FM_MAC_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_MAC_STATUS_STRIDE                         =  1024; #

sub FM_MAC_IP {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00023 + $FM_EPL_BASE); # 0x50023 EPL_BASE
}


sub FM_MAC_IP_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAC_IP_WIDTH                              =  1; #
our $FM_MAC_IP_BITS                               =  12; #

sub FM_MAC_IP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_MAC_IP_STRIDE                             =  1024; #

sub FM_MAC_IM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00024 + $FM_EPL_BASE); # 0x50024 EPL_BASE
}


sub FM_MAC_IM_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000FFF;
}

our $FM_MAC_IM_WIDTH                              =  1; #
our $FM_MAC_IM_BITS                               =  12; #

sub FM_MAC_IM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_MAC_IM_STRIDE                             =  1024; #

sub FM_STAT_EPL_ERROR1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00025 + $FM_EPL_BASE); # 0x50025 EPL_BASE
}


sub FM_STAT_EPL_ERROR1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_STAT_EPL_ERROR1_WIDTH                     =  1; #
our $FM_STAT_EPL_ERROR1_BITS                      =  16; #

sub FM_STAT_EPL_ERROR1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_EPL_ERROR1_STRIDE                    =  1024; #

sub FM_STAT_TX_PAUSE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00026 + $FM_EPL_BASE); # 0x50026 EPL_BASE
}


sub FM_STAT_TX_PAUSE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_STAT_TX_PAUSE_WIDTH                       =  1; #
our $FM_STAT_TX_PAUSE_BITS                        =  32; #

sub FM_STAT_TX_PAUSE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TX_PAUSE_STRIDE                      =  1024; #

sub FM_STAT_TX_CRC {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00027 + $FM_EPL_BASE); # 0x50027 EPL_BASE
}


sub FM_STAT_TX_CRC_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_STAT_TX_CRC_WIDTH                         =  1; #
our $FM_STAT_TX_CRC_BITS                          =  32; #

sub FM_STAT_TX_CRC_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TX_CRC_STRIDE                        =  1024; #

sub FM_STAT_EPL_ERROR2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00028 + $FM_EPL_BASE); # 0x50028 EPL_BASE
}


sub FM_STAT_EPL_ERROR2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_STAT_EPL_ERROR2_WIDTH                     =  1; #
our $FM_STAT_EPL_ERROR2_BITS                      =  16; #

sub FM_STAT_EPL_ERROR2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_EPL_ERROR2_STRIDE                    =  1024; #

sub FM_STAT_RX_JABBER {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00029 + $FM_EPL_BASE); # 0x50029 EPL_BASE
}


sub FM_STAT_RX_JABBER_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_STAT_RX_JABBER_WIDTH                      =  1; #
our $FM_STAT_RX_JABBER_BITS                       =  16; #

sub FM_STAT_RX_JABBER_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RX_JABBER_STRIDE                     =  1024; #

sub FM_EPL_LED_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0002A + $FM_EPL_BASE); # 0x5002A EPL_BASE
}


sub FM_EPL_LED_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_EPL_LED_STATUS_WIDTH                      =  1; #
our $FM_EPL_LED_STATUS_BITS                       =  5; #

sub FM_EPL_LED_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_EPL_LED_STATUS_STRIDE                     =  1024; #

sub FM_EPL_INT_DETECT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0002B + $FM_EPL_BASE); # 0x5002B EPL_BASE
}


sub FM_EPL_INT_DETECT_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_EPL_INT_DETECT_WIDTH                      =  1; #
our $FM_EPL_INT_DETECT_BITS                       =  3; #

sub FM_EPL_INT_DETECT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_EPL_INT_DETECT_STRIDE                     =  1024; #

sub FM_STAT_TX_BYTECOUNT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index) - 0) + 0x0002C + $FM_EPL_BASE + ($word)); # 0x5002C EPL_BASE
}


sub FM_STAT_TX_BYTECOUNT_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_STAT_TX_BYTECOUNT_WIDTH                   =  2; #
our $FM_STAT_TX_BYTECOUNT_BITS                    =  64; #

sub FM_STAT_TX_BYTECOUNT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TX_BYTECOUNT_STRIDE                  =  1024; #

sub FM_DEBUG_RX_MAC {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0002E + $FM_EPL_BASE); # 0x5002E EPL_BASE
}


sub FM_DEBUG_RX_MAC_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_DEBUG_RX_MAC_WIDTH                        =  1; #
our $FM_DEBUG_RX_MAC_BITS                         =  30; #

sub FM_DEBUG_RX_MAC_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_DEBUG_RX_MAC_STRIDE                       =  1024; #

sub FM_DEBUG_RX_RS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0002F + $FM_EPL_BASE); # 0x5002F EPL_BASE
}


sub FM_DEBUG_RX_RS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_DEBUG_RX_RS_WIDTH                         =  1; #
our $FM_DEBUG_RX_RS_BITS                          =  14; #

sub FM_DEBUG_RX_RS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_DEBUG_RX_RS_STRIDE                        =  1024; #

sub FM_DEBUG_SYNCBUF {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00030 + $FM_EPL_BASE); # 0x50030 EPL_BASE
}


sub FM_DEBUG_SYNCBUF_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_DEBUG_SYNCBUF_WIDTH                       =  1; #
our $FM_DEBUG_SYNCBUF_BITS                        =  26; #

sub FM_DEBUG_SYNCBUF_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_DEBUG_SYNCBUF_STRIDE                      =  1024; #

sub FM_DEBUG_TX_MAC {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00031 + $FM_EPL_BASE); # 0x50031 EPL_BASE
}


sub FM_DEBUG_TX_MAC_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_DEBUG_TX_MAC_WIDTH                        =  1; #
our $FM_DEBUG_TX_MAC_BITS                         =  30; #

sub FM_DEBUG_TX_MAC_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_DEBUG_TX_MAC_STRIDE                       =  1024; #

sub FM_SRC_MAC_LO {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00033 + $FM_EPL_BASE); # 0x50033 EPL_BASE
}


sub FM_SRC_MAC_LO_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SRC_MAC_LO_WIDTH                          =  1; #
our $FM_SRC_MAC_LO_BITS                           =  32; #

sub FM_SRC_MAC_LO_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_SRC_MAC_LO_STRIDE                         =  1024; #

sub FM_SRC_MAC_HI {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00034 + $FM_EPL_BASE); # 0x50034 EPL_BASE
}


sub FM_SRC_MAC_HI_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SRC_MAC_HI_WIDTH                          =  1; #
our $FM_SRC_MAC_HI_BITS                           =  16; #

sub FM_SRC_MAC_HI_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_SRC_MAC_HI_STRIDE                         =  1024; #

sub FM_SRC_MAC_VIRTUAL_LO {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00035 + $FM_EPL_BASE); # 0x50035 EPL_BASE
}


sub FM_SRC_MAC_VIRTUAL_LO_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SRC_MAC_VIRTUAL_LO_WIDTH                  =  1; #
our $FM_SRC_MAC_VIRTUAL_LO_BITS                   =  32; #

sub FM_SRC_MAC_VIRTUAL_LO_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_SRC_MAC_VIRTUAL_LO_STRIDE                 =  1024; #

sub FM_SRC_MAC_VIRTUAL_HI {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00036 + $FM_EPL_BASE); # 0x50036 EPL_BASE
}


sub FM_SRC_MAC_VIRTUAL_HI_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SRC_MAC_VIRTUAL_HI_WIDTH                  =  1; #
our $FM_SRC_MAC_VIRTUAL_HI_BITS                   =  16; #

sub FM_SRC_MAC_VIRTUAL_HI_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_SRC_MAC_VIRTUAL_HI_STRIDE                 =  1024; #

sub FM_JITTER_TIMER {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0003D + $FM_EPL_BASE); # 0x5003D EPL_BASE
}


sub FM_JITTER_TIMER_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x001C1000;
}

our $FM_JITTER_TIMER_WIDTH                        =  1; #
our $FM_JITTER_TIMER_BITS                         =  22; #

sub FM_JITTER_TIMER_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_JITTER_TIMER_STRIDE                       =  1024; #

sub FM_PARSE_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0003E + $FM_EPL_BASE); # 0x5003E EPL_BASE
}


sub FM_PARSE_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_PARSE_CFG_WIDTH                           =  1; #
our $FM_PARSE_CFG_BITS                            =  32; #

sub FM_PARSE_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_PARSE_CFG_STRIDE                          =  1024; #

sub FM_MAC_VLAN_ETYPE_2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0003F + $FM_EPL_BASE); # 0x5003F EPL_BASE
}


sub FM_MAC_VLAN_ETYPE_2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x910088A8;
}

our $FM_MAC_VLAN_ETYPE_2_WIDTH                    =  1; #
our $FM_MAC_VLAN_ETYPE_2_BITS                     =  32; #

sub FM_MAC_VLAN_ETYPE_2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_MAC_VLAN_ETYPE_2_STRIDE                   =  1024; #

sub FM_PARSE_RLT_1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00040 + $FM_EPL_BASE); # 0x50040 EPL_BASE
}


sub FM_PARSE_RLT_1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_PARSE_RLT_1_WIDTH                         =  1; #
our $FM_PARSE_RLT_1_BITS                          =  32; #

sub FM_PARSE_RLT_1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_PARSE_RLT_1_STRIDE                        =  1024; #

sub FM_PARSE_RLT_2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00041 + $FM_EPL_BASE); # 0x50041 EPL_BASE
}


sub FM_PARSE_RLT_2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_PARSE_RLT_2_WIDTH                         =  1; #
our $FM_PARSE_RLT_2_BITS                          =  32; #

sub FM_PARSE_RLT_2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_PARSE_RLT_2_STRIDE                        =  1024; #

sub FM_TX_TRUNC {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00042 + $FM_EPL_BASE); # 0x50042 EPL_BASE
}


sub FM_TX_TRUNC_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00160016;
}

our $FM_TX_TRUNC_WIDTH                            =  1; #
our $FM_TX_TRUNC_BITS                             =  32; #

sub FM_TX_TRUNC_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_TX_TRUNC_STRIDE                           =  1024; #

sub FM_CPID_0 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index) - 0) + 0x00050 + $FM_EPL_BASE + ($word)); # 0x50050 EPL_BASE
}


sub FM_CPID_0_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000016;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CPID_0_WIDTH                              =  2; #
our $FM_CPID_0_BITS                               =  64; #

sub FM_CPID_0_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CPID_0_STRIDE                             =  1024; #

sub FM_CPID_1 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index) - 0) + 0x00052 + $FM_EPL_BASE + ($word)); # 0x50052 EPL_BASE
}


sub FM_CPID_1_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000016;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CPID_1_WIDTH                              =  2; #
our $FM_CPID_1_BITS                               =  64; #

sub FM_CPID_1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CPID_1_STRIDE                             =  1024; #

sub FM_CPID_2 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index) - 0) + 0x00054 + $FM_EPL_BASE + ($word)); # 0x50054 EPL_BASE
}


sub FM_CPID_2_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000016;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CPID_2_WIDTH                              =  2; #
our $FM_CPID_2_BITS                               =  64; #

sub FM_CPID_2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CPID_2_STRIDE                             =  1024; #

sub FM_CPID_3 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index) - 0) + 0x00056 + $FM_EPL_BASE + ($word)); # 0x50056 EPL_BASE
}


sub FM_CPID_3_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000016;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CPID_3_WIDTH                              =  2; #
our $FM_CPID_3_BITS                               =  64; #

sub FM_CPID_3_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CPID_3_STRIDE                             =  1024; #

sub FM_CPID_4 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index) - 0) + 0x00058 + $FM_EPL_BASE + ($word)); # 0x50058 EPL_BASE
}


sub FM_CPID_4_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000016;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CPID_4_WIDTH                              =  2; #
our $FM_CPID_4_BITS                               =  64; #

sub FM_CPID_4_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CPID_4_STRIDE                             =  1024; #

sub FM_CPID_5 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index) - 0) + 0x0005A + $FM_EPL_BASE + ($word)); # 0x5005A EPL_BASE
}


sub FM_CPID_5_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000016;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CPID_5_WIDTH                              =  2; #
our $FM_CPID_5_BITS                               =  64; #

sub FM_CPID_5_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CPID_5_STRIDE                             =  1024; #

sub FM_CPID_6 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index) - 0) + 0x0005C + $FM_EPL_BASE + ($word)); # 0x5005C EPL_BASE
}


sub FM_CPID_6_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000016;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CPID_6_WIDTH                              =  2; #
our $FM_CPID_6_BITS                               =  64; #

sub FM_CPID_6_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CPID_6_STRIDE                             =  1024; #

sub FM_CPID_7 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index) - 0) + 0x0005E + $FM_EPL_BASE + ($word)); # 0x5005E EPL_BASE
}


sub FM_CPID_7_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000016;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CPID_7_WIDTH                              =  2; #
our $FM_CPID_7_BITS                               =  64; #

sub FM_CPID_7_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CPID_7_STRIDE                             =  1024; #

sub FM_MAC_VLAN_ETYPE_1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00066 + $FM_EPL_BASE); # 0x50066 EPL_BASE
}


sub FM_MAC_VLAN_ETYPE_1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0xF3208100;
}

our $FM_MAC_VLAN_ETYPE_1_WIDTH                    =  1; #
our $FM_MAC_VLAN_ETYPE_1_BITS                     =  32; #

sub FM_MAC_VLAN_ETYPE_1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_MAC_VLAN_ETYPE_1_STRIDE                   =  1024; #

sub FM_DI_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00067 + $FM_EPL_BASE); # 0x50067 EPL_BASE
}


sub FM_DI_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00800000;
}

our $FM_DI_CFG_WIDTH                              =  1; #
our $FM_DI_CFG_BITS                               =  30; #

sub FM_DI_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_DI_CFG_STRIDE                             =  1024; #

sub FM_TCP_WD_MASK_LO {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00068 + $FM_EPL_BASE); # 0x50068 EPL_BASE
}


sub FM_TCP_WD_MASK_LO_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_TCP_WD_MASK_LO_WIDTH                      =  1; #
our $FM_TCP_WD_MASK_LO_BITS                       =  32; #

sub FM_TCP_WD_MASK_LO_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_TCP_WD_MASK_LO_STRIDE                     =  1024; #

sub FM_TCP_WD_MASK_HI {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00069 + $FM_EPL_BASE); # 0x50069 EPL_BASE
}


sub FM_TCP_WD_MASK_HI_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_TCP_WD_MASK_HI_WIDTH                      =  1; #
our $FM_TCP_WD_MASK_HI_BITS                       =  16; #

sub FM_TCP_WD_MASK_HI_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_TCP_WD_MASK_HI_STRIDE                     =  1024; #

sub FM_UDP_WD_MASK_LO {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0006A + $FM_EPL_BASE); # 0x5006A EPL_BASE
}


sub FM_UDP_WD_MASK_LO_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_UDP_WD_MASK_LO_WIDTH                      =  1; #
our $FM_UDP_WD_MASK_LO_BITS                       =  32; #

sub FM_UDP_WD_MASK_LO_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_UDP_WD_MASK_LO_STRIDE                     =  1024; #

sub FM_UDP_WD_MASK_HI {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0006B + $FM_EPL_BASE); # 0x5006B EPL_BASE
}


sub FM_UDP_WD_MASK_HI_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_UDP_WD_MASK_HI_WIDTH                      =  1; #
our $FM_UDP_WD_MASK_HI_BITS                       =  16; #

sub FM_UDP_WD_MASK_HI_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_UDP_WD_MASK_HI_STRIDE                     =  1024; #

sub FM_L4PROT1_WD_MASK_LO {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0006C + $FM_EPL_BASE); # 0x5006C EPL_BASE
}


sub FM_L4PROT1_WD_MASK_LO_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L4PROT1_WD_MASK_LO_WIDTH                  =  1; #
our $FM_L4PROT1_WD_MASK_LO_BITS                   =  32; #

sub FM_L4PROT1_WD_MASK_LO_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_L4PROT1_WD_MASK_LO_STRIDE                 =  1024; #

sub FM_L4PROT1_WD_MASK_HI {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0006D + $FM_EPL_BASE); # 0x5006D EPL_BASE
}


sub FM_L4PROT1_WD_MASK_HI_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L4PROT1_WD_MASK_HI_WIDTH                  =  1; #
our $FM_L4PROT1_WD_MASK_HI_BITS                   =  16; #

sub FM_L4PROT1_WD_MASK_HI_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_L4PROT1_WD_MASK_HI_STRIDE                 =  1024; #

sub FM_L4PROT2_WD_MASK_LO {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0006E + $FM_EPL_BASE); # 0x5006E EPL_BASE
}


sub FM_L4PROT2_WD_MASK_LO_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L4PROT2_WD_MASK_LO_WIDTH                  =  1; #
our $FM_L4PROT2_WD_MASK_LO_BITS                   =  32; #

sub FM_L4PROT2_WD_MASK_LO_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_L4PROT2_WD_MASK_LO_STRIDE                 =  1024; #

sub FM_L4PROT2_WD_MASK_HI {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0006F + $FM_EPL_BASE); # 0x5006F EPL_BASE
}


sub FM_L4PROT2_WD_MASK_HI_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L4PROT2_WD_MASK_HI_WIDTH                  =  1; #
our $FM_L4PROT2_WD_MASK_HI_BITS                   =  16; #

sub FM_L4PROT2_WD_MASK_HI_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_L4PROT2_WD_MASK_HI_STRIDE                 =  1024; #

sub FM_AN_TX_MSG0 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index) - 0) + 0x00070 + $FM_EPL_BASE + ($word)); # 0x50070 EPL_BASE
}


sub FM_AN_TX_MSG0_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_AN_TX_MSG0_WIDTH                          =  2; #
our $FM_AN_TX_MSG0_BITS                           =  48; #

sub FM_AN_TX_MSG0_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_AN_TX_MSG0_STRIDE                         =  1024; #

sub FM_AN_TX_MSG1 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index) - 0) + 0x00072 + $FM_EPL_BASE + ($word)); # 0x50072 EPL_BASE
}


sub FM_AN_TX_MSG1_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_AN_TX_MSG1_WIDTH                          =  2; #
our $FM_AN_TX_MSG1_BITS                           =  48; #

sub FM_AN_TX_MSG1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_AN_TX_MSG1_STRIDE                         =  1024; #

sub FM_AN_RX_MSG0 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index) - 0) + 0x00074 + $FM_EPL_BASE + ($word)); # 0x50074 EPL_BASE
}


sub FM_AN_RX_MSG0_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_AN_RX_MSG0_WIDTH                          =  2; #
our $FM_AN_RX_MSG0_BITS                           =  48; #

sub FM_AN_RX_MSG0_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_AN_RX_MSG0_STRIDE                         =  1024; #

sub FM_AN_RX_MSG1 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index) - 0) + 0x00076 + $FM_EPL_BASE + ($word)); # 0x50076 EPL_BASE
}


sub FM_AN_RX_MSG1_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_AN_RX_MSG1_WIDTH                          =  2; #
our $FM_AN_RX_MSG1_BITS                           =  48; #

sub FM_AN_RX_MSG1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_AN_RX_MSG1_STRIDE                         =  1024; #

sub FM_AN_CTL {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00078 + $FM_EPL_BASE); # 0x50078 EPL_BASE
}


sub FM_AN_CTL_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_AN_CTL_WIDTH                              =  1; #
our $FM_AN_CTL_BITS                               =  5; #

sub FM_AN_CTL_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_AN_CTL_STRIDE                             =  1024; #

sub FM_AN_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00079 + $FM_EPL_BASE); # 0x50079 EPL_BASE
}


sub FM_AN_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_AN_STATUS_WIDTH                           =  1; #
our $FM_AN_STATUS_BITS                            =  5; #

sub FM_AN_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_AN_STATUS_STRIDE                          =  1024; #

sub FM_AN_TIMEOUT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0007A + $FM_EPL_BASE); # 0x5007A EPL_BASE
}


sub FM_AN_TIMEOUT_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_AN_TIMEOUT_WIDTH                          =  1; #
our $FM_AN_TIMEOUT_BITS                           =  16; #

sub FM_AN_TIMEOUT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_AN_TIMEOUT_STRIDE                         =  1024; #

sub FM_AN_TX_TIMER {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0007B + $FM_EPL_BASE); # 0x5007B EPL_BASE
}


sub FM_AN_TX_TIMER_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_AN_TX_TIMER_WIDTH                         =  1; #
our $FM_AN_TX_TIMER_BITS                          =  24; #

sub FM_AN_TX_TIMER_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_AN_TX_TIMER_STRIDE                        =  1024; #

sub FM_VLANTAG_TABLE {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00001 * (($index0) - 0) + 0x00400 * (($index1) - 0) + 0x00080 + $FM_EPL_BASE); # 0x50080 EPL_BASE
}


sub FM_VLANTAG_TABLE_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_VLANTAG_TABLE_WIDTH                       =  1; #
our $FM_VLANTAG_TABLE_BITS                        =  32; #

sub FM_VLANTAG_TABLE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}

sub FM_VLANTAG_TABLE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_VLANTAG_TABLE_STRIDE_0                    =  1; #
our $FM_VLANTAG_TABLE_STRIDE_1                    =  1024; #

sub FM_SCHED_GROUP_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00002 * (($index) - 0) + 0x00040 + $FM_SCHED2_BASE); # 0x60040 SCHED2_BASE
}


sub FM_SCHED_GROUP_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00FFFFFF;
}

our $FM_SCHED_GROUP_CFG_WIDTH                     =  1; #
our $FM_SCHED_GROUP_CFG_BITS                      =  24; #

sub FM_SCHED_GROUP_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_SCHED_GROUP_CFG_STRIDE                    =  2; #
our $FM_FUSE_PORT                                 =  (0x000FE + $FM_SCHED2_BASE); # // 0x600FE SCHED2_BASE

sub FM_FUSE_PORT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x000FE + $FM_SCHED2_BASE); # 0x600FE SCHED2_BASE
}


sub FM_FUSE_PORT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FUSE_PORT_WIDTH                           =  1; #
our $FM_FUSE_PORT_BITS                            =  32; #
our $FM_FUSE_SEG                                  =  (0x000FF + $FM_SCHED2_BASE); # // 0x600FF SCHED2_BASE

sub FM_FUSE_SEG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x000FF + $FM_SCHED2_BASE); # 0x600FF SCHED2_BASE
}


sub FM_FUSE_SEG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FUSE_SEG_WIDTH                            =  1; #
our $FM_FUSE_SEG_BITS                             =  32; #
our $FM_MSB_CFG                                   =  (0x00000 + $FM_MSB_BASE); # // 0x80000 MSB_BASE

sub FM_MSB_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00000 + $FM_MSB_BASE); # 0x80000 MSB_BASE
}


sub FM_MSB_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0000000F;
}

our $FM_MSB_CFG_WIDTH                             =  1; #
our $FM_MSB_CFG_BITS                              =  6; #
our $FM_MSB_IBM_GLORT                             =  (0x00001 + $FM_MSB_BASE); # // 0x80001 MSB_BASE

sub FM_MSB_IBM_GLORT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00001 + $FM_MSB_BASE); # 0x80001 MSB_BASE
}


sub FM_MSB_IBM_GLORT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_IBM_GLORT_WIDTH                       =  1; #
our $FM_MSB_IBM_GLORT_BITS                        =  16; #
our $FM_MSB_IBM_INT                               =  (0x00002 + $FM_MSB_BASE); # // 0x80002 MSB_BASE

sub FM_MSB_IBM_INT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00002 + $FM_MSB_BASE); # 0x80002 MSB_BASE
}


sub FM_MSB_IBM_INT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00FF0000;
}

our $FM_MSB_IBM_INT_WIDTH                         =  1; #
our $FM_MSB_IBM_INT_BITS                          =  32; #
our $FM_MSB_INT_FRAME                             =  (0x00003 + $FM_MSB_BASE); # // 0x80003 MSB_BASE

sub FM_MSB_INT_FRAME {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00003 + $FM_MSB_BASE); # 0x80003 MSB_BASE
}


sub FM_MSB_INT_FRAME_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x000FFFFF;
}

our $FM_MSB_INT_FRAME_WIDTH                       =  1; #
our $FM_MSB_INT_FRAME_BITS                        =  28; #
our $FM_MSB_STATS_0                               =  (0x00004 + $FM_MSB_BASE); # // 0x80004 MSB_BASE

sub FM_MSB_STATS_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00004 + $FM_MSB_BASE); # 0x80004 MSB_BASE
}


sub FM_MSB_STATS_0_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_STATS_0_WIDTH                         =  1; #
our $FM_MSB_STATS_0_BITS                          =  32; #
our $FM_MSB_STATS_1                               =  (0x00005 + $FM_MSB_BASE); # // 0x80005 MSB_BASE

sub FM_MSB_STATS_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00005 + $FM_MSB_BASE); # 0x80005 MSB_BASE
}


sub FM_MSB_STATS_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_STATS_1_WIDTH                         =  1; #
our $FM_MSB_STATS_1_BITS                          =  32; #
our $FM_MSB_STATS_2                               =  (0x00006 + $FM_MSB_BASE); # // 0x80006 MSB_BASE

sub FM_MSB_STATS_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00006 + $FM_MSB_BASE); # 0x80006 MSB_BASE
}


sub FM_MSB_STATS_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_STATS_2_WIDTH                         =  1; #
our $FM_MSB_STATS_2_BITS                          =  32; #
our $FM_MSB_INTR_CTR_0                            =  (0x00007 + $FM_MSB_BASE); # // 0x80007 MSB_BASE

sub FM_MSB_INTR_CTR_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00007 + $FM_MSB_BASE); # 0x80007 MSB_BASE
}


sub FM_MSB_INTR_CTR_0_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_INTR_CTR_0_WIDTH                      =  1; #
our $FM_MSB_INTR_CTR_0_BITS                       =  32; #
our $FM_MSB_INTR_CTR_1                            =  (0x00008 + $FM_MSB_BASE); # // 0x80008 MSB_BASE

sub FM_MSB_INTR_CTR_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00008 + $FM_MSB_BASE); # 0x80008 MSB_BASE
}


sub FM_MSB_INTR_CTR_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_INTR_CTR_1_WIDTH                      =  1; #
our $FM_MSB_INTR_CTR_1_BITS                       =  32; #
our $FM_MSB_INTR_CTR_2                            =  (0x00009 + $FM_MSB_BASE); # // 0x80009 MSB_BASE

sub FM_MSB_INTR_CTR_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00009 + $FM_MSB_BASE); # 0x80009 MSB_BASE
}


sub FM_MSB_INTR_CTR_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_INTR_CTR_2_WIDTH                      =  1; #
our $FM_MSB_INTR_CTR_2_BITS                       =  32; #
our $FM_MSB_INTR_CTR_3                            =  (0x0000A + $FM_MSB_BASE); # // 0x8000A MSB_BASE

sub FM_MSB_INTR_CTR_3 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0000A + $FM_MSB_BASE); # 0x8000A MSB_BASE
}


sub FM_MSB_INTR_CTR_3_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_INTR_CTR_3_WIDTH                      =  1; #
our $FM_MSB_INTR_CTR_3_BITS                       =  32; #
our $FM_MSB_INTR_CTR_4                            =  (0x0000B + $FM_MSB_BASE); # // 0x8000B MSB_BASE

sub FM_MSB_INTR_CTR_4 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0000B + $FM_MSB_BASE); # 0x8000B MSB_BASE
}


sub FM_MSB_INTR_CTR_4_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_INTR_CTR_4_WIDTH                      =  1; #
our $FM_MSB_INTR_CTR_4_BITS                       =  32; #
our $FM_MSB_INTR_CTR_5                            =  (0x0000C + $FM_MSB_BASE); # // 0x8000C MSB_BASE

sub FM_MSB_INTR_CTR_5 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0000C + $FM_MSB_BASE); # 0x8000C MSB_BASE
}


sub FM_MSB_INTR_CTR_5_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_INTR_CTR_5_WIDTH                      =  1; #
our $FM_MSB_INTR_CTR_5_BITS                       =  32; #
our $FM_MSB_IP                                    =  (0x0000D + $FM_MSB_BASE); # // 0x8000D MSB_BASE

sub FM_MSB_IP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0000D + $FM_MSB_BASE); # 0x8000D MSB_BASE
}


sub FM_MSB_IP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_IP_WIDTH                              =  1; #
our $FM_MSB_IP_BITS                               =  4; #
our $FM_MSB_IM                                    =  (0x0000E + $FM_MSB_BASE); # // 0x8000E MSB_BASE

sub FM_MSB_IM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0000E + $FM_MSB_BASE); # 0x8000E MSB_BASE
}


sub FM_MSB_IM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0000000F;
}

our $FM_MSB_IM_WIDTH                              =  1; #
our $FM_MSB_IM_BITS                               =  4; #
our $FM_MSB_RX_EPL_RATE                           =  (0x0000F + $FM_MSB_BASE); # // 0x8000F MSB_BASE

sub FM_MSB_RX_EPL_RATE {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0000F + $FM_MSB_BASE); # 0x8000F MSB_BASE
}


sub FM_MSB_RX_EPL_RATE_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000605;
}

our $FM_MSB_RX_EPL_RATE_WIDTH                     =  1; #
our $FM_MSB_RX_EPL_RATE_BITS                      =  12; #
our $FM_MSB_SCRATCH_0                             =  (0x00010 + $FM_MSB_BASE); # // 0x80010 MSB_BASE

sub FM_MSB_SCRATCH_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00010 + $FM_MSB_BASE); # 0x80010 MSB_BASE
}


sub FM_MSB_SCRATCH_0_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_SCRATCH_0_WIDTH                       =  1; #
our $FM_MSB_SCRATCH_0_BITS                        =  32; #
our $FM_MSB_SCRATCH_1                             =  (0x00011 + $FM_MSB_BASE); # // 0x80011 MSB_BASE

sub FM_MSB_SCRATCH_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00011 + $FM_MSB_BASE); # 0x80011 MSB_BASE
}


sub FM_MSB_SCRATCH_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_SCRATCH_1_WIDTH                       =  1; #
our $FM_MSB_SCRATCH_1_BITS                        =  32; #
our $FM_MSB_SCRATCH_2                             =  (0x00012 + $FM_MSB_BASE); # // 0x80012 MSB_BASE

sub FM_MSB_SCRATCH_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00012 + $FM_MSB_BASE); # 0x80012 MSB_BASE
}


sub FM_MSB_SCRATCH_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_SCRATCH_2_WIDTH                       =  1; #
our $FM_MSB_SCRATCH_2_BITS                        =  32; #
our $FM_MSB_SCRATCH_3                             =  (0x00013 + $FM_MSB_BASE); # // 0x80013 MSB_BASE

sub FM_MSB_SCRATCH_3 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00013 + $FM_MSB_BASE); # 0x80013 MSB_BASE
}


sub FM_MSB_SCRATCH_3_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_SCRATCH_3_WIDTH                       =  1; #
our $FM_MSB_SCRATCH_3_BITS                        =  32; #
our $FM_MSB_SCRATCH_4                             =  (0x00014 + $FM_MSB_BASE); # // 0x80014 MSB_BASE

sub FM_MSB_SCRATCH_4 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00014 + $FM_MSB_BASE); # 0x80014 MSB_BASE
}


sub FM_MSB_SCRATCH_4_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_SCRATCH_4_WIDTH                       =  1; #
our $FM_MSB_SCRATCH_4_BITS                        =  32; #
our $FM_MSB_SCRATCH_5                             =  (0x00015 + $FM_MSB_BASE); # // 0x80015 MSB_BASE

sub FM_MSB_SCRATCH_5 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00015 + $FM_MSB_BASE); # 0x80015 MSB_BASE
}


sub FM_MSB_SCRATCH_5_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_SCRATCH_5_WIDTH                       =  1; #
our $FM_MSB_SCRATCH_5_BITS                        =  32; #
our $FM_MSB_SCRATCH_6                             =  (0x00016 + $FM_MSB_BASE); # // 0x80016 MSB_BASE

sub FM_MSB_SCRATCH_6 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00016 + $FM_MSB_BASE); # 0x80016 MSB_BASE
}


sub FM_MSB_SCRATCH_6_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_SCRATCH_6_WIDTH                       =  1; #
our $FM_MSB_SCRATCH_6_BITS                        =  32; #
our $FM_MSB_SCRATCH_7                             =  (0x00017 + $FM_MSB_BASE); # // 0x80017 MSB_BASE

sub FM_MSB_SCRATCH_7 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00017 + $FM_MSB_BASE); # 0x80017 MSB_BASE
}


sub FM_MSB_SCRATCH_7_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_SCRATCH_7_WIDTH                       =  1; #
our $FM_MSB_SCRATCH_7_BITS                        =  32; #
our $FM_MSB_SCRATCH_8                             =  (0x00018 + $FM_MSB_BASE); # // 0x80018 MSB_BASE

sub FM_MSB_SCRATCH_8 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00018 + $FM_MSB_BASE); # 0x80018 MSB_BASE
}


sub FM_MSB_SCRATCH_8_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_SCRATCH_8_WIDTH                       =  1; #
our $FM_MSB_SCRATCH_8_BITS                        =  32; #
our $FM_MSB_SCRATCH_9                             =  (0x00019 + $FM_MSB_BASE); # // 0x80019 MSB_BASE

sub FM_MSB_SCRATCH_9 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00019 + $FM_MSB_BASE); # 0x80019 MSB_BASE
}


sub FM_MSB_SCRATCH_9_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_SCRATCH_9_WIDTH                       =  1; #
our $FM_MSB_SCRATCH_9_BITS                        =  32; #
our $FM_MSB_SCRATCH_10                            =  (0x0001A + $FM_MSB_BASE); # // 0x8001A MSB_BASE

sub FM_MSB_SCRATCH_10 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0001A + $FM_MSB_BASE); # 0x8001A MSB_BASE
}


sub FM_MSB_SCRATCH_10_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_SCRATCH_10_WIDTH                      =  1; #
our $FM_MSB_SCRATCH_10_BITS                       =  32; #
our $FM_MSB_SCRATCH_11                            =  (0x0001B + $FM_MSB_BASE); # // 0x8001B MSB_BASE

sub FM_MSB_SCRATCH_11 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0001B + $FM_MSB_BASE); # 0x8001B MSB_BASE
}


sub FM_MSB_SCRATCH_11_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_SCRATCH_11_WIDTH                      =  1; #
our $FM_MSB_SCRATCH_11_BITS                       =  32; #
our $FM_MSB_SCRATCH_12                            =  (0x0001C + $FM_MSB_BASE); # // 0x8001C MSB_BASE

sub FM_MSB_SCRATCH_12 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0001C + $FM_MSB_BASE); # 0x8001C MSB_BASE
}


sub FM_MSB_SCRATCH_12_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_SCRATCH_12_WIDTH                      =  1; #
our $FM_MSB_SCRATCH_12_BITS                       =  32; #
our $FM_MSB_SCRATCH_13                            =  (0x0001D + $FM_MSB_BASE); # // 0x8001D MSB_BASE

sub FM_MSB_SCRATCH_13 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0001D + $FM_MSB_BASE); # 0x8001D MSB_BASE
}


sub FM_MSB_SCRATCH_13_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_SCRATCH_13_WIDTH                      =  1; #
our $FM_MSB_SCRATCH_13_BITS                       =  32; #
our $FM_MSB_SCRATCH_14                            =  (0x0001E + $FM_MSB_BASE); # // 0x8001E MSB_BASE

sub FM_MSB_SCRATCH_14 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0001E + $FM_MSB_BASE); # 0x8001E MSB_BASE
}


sub FM_MSB_SCRATCH_14_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_SCRATCH_14_WIDTH                      =  1; #
our $FM_MSB_SCRATCH_14_BITS                       =  32; #
our $FM_MSB_SCRATCH_15                            =  (0x0001F + $FM_MSB_BASE); # // 0x8001F MSB_BASE

sub FM_MSB_SCRATCH_15 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0001F + $FM_MSB_BASE); # 0x8001F MSB_BASE
}


sub FM_MSB_SCRATCH_15_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_SCRATCH_15_WIDTH                      =  1; #
our $FM_MSB_SCRATCH_15_BITS                       =  32; #
our $FM_MSB_TS                                    =  (0x00020 + $FM_MSB_BASE); # // 0x80020 MSB_BASE

sub FM_MSB_TS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00020 + $FM_MSB_BASE); # 0x80020 MSB_BASE
}


sub FM_MSB_TS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_TS_WIDTH                              =  1; #
our $FM_MSB_TS_BITS                               =  8; #
our $FM_MSB_CREDITS                               =  (0x00021 + $FM_MSB_BASE); # // 0x80021 MSB_BASE

sub FM_MSB_CREDITS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00021 + $FM_MSB_BASE); # 0x80021 MSB_BASE
}


sub FM_MSB_CREDITS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000003;
}

our $FM_MSB_CREDITS_WIDTH                         =  1; #
our $FM_MSB_CREDITS_BITS                          =  2; #
our $FM_MSB_SRAM_REPAIR_0                         =  (0x00022 + $FM_MSB_BASE); # // 0x80022 MSB_BASE

sub FM_MSB_SRAM_REPAIR_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00022 + $FM_MSB_BASE); # 0x80022 MSB_BASE
}


sub FM_MSB_SRAM_REPAIR_0_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_SRAM_REPAIR_0_WIDTH                   =  1; #
our $FM_MSB_SRAM_REPAIR_0_BITS                    =  12; #
our $FM_MSB_SRAM_REPAIR_1                         =  (0x00023 + $FM_MSB_BASE); # // 0x80023 MSB_BASE

sub FM_MSB_SRAM_REPAIR_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00023 + $FM_MSB_BASE); # 0x80023 MSB_BASE
}


sub FM_MSB_SRAM_REPAIR_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_SRAM_REPAIR_1_WIDTH                   =  1; #
our $FM_MSB_SRAM_REPAIR_1_BITS                    =  12; #

sub FM_STAT_RxUcstPktsNonIP {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00000 + $FM_STATS_RX_TYPE_BASE + ($word)); # 0x90000 STATS_RX_TYPE_BASE
}


sub FM_STAT_RxUcstPktsNonIP_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxUcstPktsNonIP_WIDTH                =  2; #
our $FM_STAT_RxUcstPktsNonIP_BITS                 =  64; #

sub FM_STAT_RxUcstPktsNonIP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxUcstPktsNonIP_STRIDE               =  4; #

sub FM_STAT_RxBcstPktsNonIP {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00002 + $FM_STATS_RX_TYPE_BASE + ($word)); # 0x90002 STATS_RX_TYPE_BASE
}


sub FM_STAT_RxBcstPktsNonIP_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxBcstPktsNonIP_WIDTH                =  2; #
our $FM_STAT_RxBcstPktsNonIP_BITS                 =  64; #

sub FM_STAT_RxBcstPktsNonIP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxBcstPktsNonIP_STRIDE               =  4; #

sub FM_STAT_RxMcstPktsNonIP {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00070 + $FM_STATS_RX_TYPE_BASE + ($word)); # 0x90070 STATS_RX_TYPE_BASE
}


sub FM_STAT_RxMcstPktsNonIP_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxMcstPktsNonIP_WIDTH                =  2; #
our $FM_STAT_RxMcstPktsNonIP_BITS                 =  64; #

sub FM_STAT_RxMcstPktsNonIP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxMcstPktsNonIP_STRIDE               =  4; #

sub FM_STAT_RxUcstPktsIPv4 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00072 + $FM_STATS_RX_TYPE_BASE + ($word)); # 0x90072 STATS_RX_TYPE_BASE
}


sub FM_STAT_RxUcstPktsIPv4_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxUcstPktsIPv4_WIDTH                 =  2; #
our $FM_STAT_RxUcstPktsIPv4_BITS                  =  64; #

sub FM_STAT_RxUcstPktsIPv4_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxUcstPktsIPv4_STRIDE                =  4; #

sub FM_STAT_RxBcstPktsIPv4 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x000E0 + $FM_STATS_RX_TYPE_BASE + ($word)); # 0x900E0 STATS_RX_TYPE_BASE
}


sub FM_STAT_RxBcstPktsIPv4_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxBcstPktsIPv4_WIDTH                 =  2; #
our $FM_STAT_RxBcstPktsIPv4_BITS                  =  64; #

sub FM_STAT_RxBcstPktsIPv4_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxBcstPktsIPv4_STRIDE                =  4; #

sub FM_STAT_RxMcstPktsIPv4 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x000E2 + $FM_STATS_RX_TYPE_BASE + ($word)); # 0x900E2 STATS_RX_TYPE_BASE
}


sub FM_STAT_RxMcstPktsIPv4_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxMcstPktsIPv4_WIDTH                 =  2; #
our $FM_STAT_RxMcstPktsIPv4_BITS                  =  64; #

sub FM_STAT_RxMcstPktsIPv4_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxMcstPktsIPv4_STRIDE                =  4; #

sub FM_STAT_RxUcstPktsIPv6 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00150 + $FM_STATS_RX_TYPE_BASE + ($word)); # 0x90150 STATS_RX_TYPE_BASE
}


sub FM_STAT_RxUcstPktsIPv6_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxUcstPktsIPv6_WIDTH                 =  2; #
our $FM_STAT_RxUcstPktsIPv6_BITS                  =  64; #

sub FM_STAT_RxUcstPktsIPv6_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxUcstPktsIPv6_STRIDE                =  4; #

sub FM_STAT_RxBcstPktsIPv6 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00152 + $FM_STATS_RX_TYPE_BASE + ($word)); # 0x90152 STATS_RX_TYPE_BASE
}


sub FM_STAT_RxBcstPktsIPv6_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxBcstPktsIPv6_WIDTH                 =  2; #
our $FM_STAT_RxBcstPktsIPv6_BITS                  =  64; #

sub FM_STAT_RxBcstPktsIPv6_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxBcstPktsIPv6_STRIDE                =  4; #

sub FM_STAT_RxMcstPktsIPv6 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x001C0 + $FM_STATS_RX_TYPE_BASE + ($word)); # 0x901C0 STATS_RX_TYPE_BASE
}


sub FM_STAT_RxMcstPktsIPv6_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxMcstPktsIPv6_WIDTH                 =  2; #
our $FM_STAT_RxMcstPktsIPv6_BITS                  =  64; #

sub FM_STAT_RxMcstPktsIPv6_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxMcstPktsIPv6_STRIDE                =  4; #

sub FM_STAT_RxPausePkts {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x001C2 + $FM_STATS_RX_TYPE_BASE + ($word)); # 0x901C2 STATS_RX_TYPE_BASE
}


sub FM_STAT_RxPausePkts_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxPausePkts_WIDTH                    =  2; #
our $FM_STAT_RxPausePkts_BITS                     =  64; #

sub FM_STAT_RxPausePkts_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxPausePkts_STRIDE                   =  4; #

sub FM_STAT_RxCBPausePkts {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00230 + $FM_STATS_RX_TYPE_BASE + ($word)); # 0x90230 STATS_RX_TYPE_BASE
}


sub FM_STAT_RxCBPausePkts_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxCBPausePkts_WIDTH                  =  2; #
our $FM_STAT_RxCBPausePkts_BITS                   =  64; #

sub FM_STAT_RxCBPausePkts_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxCBPausePkts_STRIDE                 =  4; #

sub FM_STAT_RxSymbolErrors {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00232 + $FM_STATS_RX_TYPE_BASE + ($word)); # 0x90232 STATS_RX_TYPE_BASE
}


sub FM_STAT_RxSymbolErrors_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxSymbolErrors_WIDTH                 =  2; #
our $FM_STAT_RxSymbolErrors_BITS                  =  64; #

sub FM_STAT_RxSymbolErrors_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxSymbolErrors_STRIDE                =  4; #

sub FM_STAT_RxFCSErrors {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x002A0 + $FM_STATS_RX_TYPE_BASE + ($word)); # 0x902A0 STATS_RX_TYPE_BASE
}


sub FM_STAT_RxFCSErrors_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxFCSErrors_WIDTH                    =  2; #
our $FM_STAT_RxFCSErrors_BITS                     =  64; #

sub FM_STAT_RxFCSErrors_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxFCSErrors_STRIDE                   =  4; #

sub FM_STAT_RxFrameSizeErrors {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x002A2 + $FM_STATS_RX_TYPE_BASE + ($word)); # 0x902A2 STATS_RX_TYPE_BASE
}


sub FM_STAT_RxFrameSizeErrors_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxFrameSizeErrors_WIDTH              =  2; #
our $FM_STAT_RxFrameSizeErrors_BITS               =  64; #

sub FM_STAT_RxFrameSizeErrors_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxFrameSizeErrors_STRIDE             =  4; #

sub FM_STAT_RxP0 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00000 + $FM_STATS_RX_PKT_PRI_BASE + ($word)); # 0x90310 STATS_RX_PKT_PRI_BASE
}


sub FM_STAT_RxP0_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxP0_WIDTH                           =  2; #
our $FM_STAT_RxP0_BITS                            =  64; #

sub FM_STAT_RxP0_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxP0_STRIDE                          =  4; #

sub FM_STAT_RxP1 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00002 + $FM_STATS_RX_PKT_PRI_BASE + ($word)); # 0x90312 STATS_RX_PKT_PRI_BASE
}


sub FM_STAT_RxP1_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxP1_WIDTH                           =  2; #
our $FM_STAT_RxP1_BITS                            =  64; #

sub FM_STAT_RxP1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxP1_STRIDE                          =  4; #

sub FM_STAT_RxP2 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00070 + $FM_STATS_RX_PKT_PRI_BASE + ($word)); # 0x90380 STATS_RX_PKT_PRI_BASE
}


sub FM_STAT_RxP2_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxP2_WIDTH                           =  2; #
our $FM_STAT_RxP2_BITS                            =  64; #

sub FM_STAT_RxP2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxP2_STRIDE                          =  4; #

sub FM_STAT_RxP3 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00072 + $FM_STATS_RX_PKT_PRI_BASE + ($word)); # 0x90382 STATS_RX_PKT_PRI_BASE
}


sub FM_STAT_RxP3_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxP3_WIDTH                           =  2; #
our $FM_STAT_RxP3_BITS                            =  64; #

sub FM_STAT_RxP3_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxP3_STRIDE                          =  4; #

sub FM_STAT_RxP4 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x000E0 + $FM_STATS_RX_PKT_PRI_BASE + ($word)); # 0x903F0 STATS_RX_PKT_PRI_BASE
}


sub FM_STAT_RxP4_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxP4_WIDTH                           =  2; #
our $FM_STAT_RxP4_BITS                            =  64; #

sub FM_STAT_RxP4_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxP4_STRIDE                          =  4; #

sub FM_STAT_RxP5 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x000E2 + $FM_STATS_RX_PKT_PRI_BASE + ($word)); # 0x903F2 STATS_RX_PKT_PRI_BASE
}


sub FM_STAT_RxP5_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxP5_WIDTH                           =  2; #
our $FM_STAT_RxP5_BITS                            =  64; #

sub FM_STAT_RxP5_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxP5_STRIDE                          =  4; #

sub FM_STAT_RxP6 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00150 + $FM_STATS_RX_PKT_PRI_BASE + ($word)); # 0x90460 STATS_RX_PKT_PRI_BASE
}


sub FM_STAT_RxP6_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxP6_WIDTH                           =  2; #
our $FM_STAT_RxP6_BITS                            =  64; #

sub FM_STAT_RxP6_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxP6_STRIDE                          =  4; #

sub FM_STAT_RxP7 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00152 + $FM_STATS_RX_PKT_PRI_BASE + ($word)); # 0x90462 STATS_RX_PKT_PRI_BASE
}


sub FM_STAT_RxP7_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxP7_WIDTH                           =  2; #
our $FM_STAT_RxP7_BITS                            =  64; #

sub FM_STAT_RxP7_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxP7_STRIDE                          =  4; #

sub FM_STAT_TxUcstPkts {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00000 + $FM_STATS_TX_TYPE_BASE + ($word)); # 0x904D0 STATS_TX_TYPE_BASE
}


sub FM_STAT_TxUcstPkts_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxUcstPkts_WIDTH                     =  2; #
our $FM_STAT_TxUcstPkts_BITS                      =  64; #

sub FM_STAT_TxUcstPkts_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxUcstPkts_STRIDE                    =  4; #

sub FM_STAT_TxBcstPkts {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00002 + $FM_STATS_TX_TYPE_BASE + ($word)); # 0x904D2 STATS_TX_TYPE_BASE
}


sub FM_STAT_TxBcstPkts_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxBcstPkts_WIDTH                     =  2; #
our $FM_STAT_TxBcstPkts_BITS                      =  64; #

sub FM_STAT_TxBcstPkts_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxBcstPkts_STRIDE                    =  4; #

sub FM_STAT_TxMcstPkts {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00070 + $FM_STATS_TX_TYPE_BASE + ($word)); # 0x90540 STATS_TX_TYPE_BASE
}


sub FM_STAT_TxMcstPkts_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxMcstPkts_WIDTH                     =  2; #
our $FM_STAT_TxMcstPkts_BITS                      =  64; #

sub FM_STAT_TxMcstPkts_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxMcstPkts_STRIDE                    =  4; #

sub FM_STAT_TxTimeOutDrop {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00072 + $FM_STATS_TX_TYPE_BASE + ($word)); # 0x90542 STATS_TX_TYPE_BASE
}


sub FM_STAT_TxTimeOutDrop_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxTimeOutDrop_WIDTH                  =  2; #
our $FM_STAT_TxTimeOutDrop_BITS                   =  64; #

sub FM_STAT_TxTimeOutDrop_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxTimeOutDrop_STRIDE                 =  4; #

sub FM_STAT_TxErrorDrop {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x000E0 + $FM_STATS_TX_TYPE_BASE + ($word)); # 0x905B0 STATS_TX_TYPE_BASE
}


sub FM_STAT_TxErrorDrop_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxErrorDrop_WIDTH                    =  2; #
our $FM_STAT_TxErrorDrop_BITS                     =  64; #

sub FM_STAT_TxErrorDrop_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxErrorDrop_STRIDE                   =  4; #

sub FM_STAT_TxLoopbackDrop {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x000E2 + $FM_STATS_TX_TYPE_BASE + ($word)); # 0x905B2 STATS_TX_TYPE_BASE
}


sub FM_STAT_TxLoopbackDrop_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxLoopbackDrop_WIDTH                 =  2; #
our $FM_STAT_TxLoopbackDrop_BITS                  =  64; #

sub FM_STAT_TxLoopbackDrop_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxLoopbackDrop_STRIDE                =  4; #

sub FM_STAT_TxPort0 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00000 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x90620 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort0_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort0_WIDTH                        =  2; #
our $FM_STAT_TxPort0_BITS                         =  64; #

sub FM_STAT_TxPort0_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort0_STRIDE                       =  4; #

sub FM_STAT_TxPort1 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00002 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x90622 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort1_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort1_WIDTH                        =  2; #
our $FM_STAT_TxPort1_BITS                         =  64; #

sub FM_STAT_TxPort1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort1_STRIDE                       =  4; #

sub FM_STAT_TxPort2 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00070 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x90690 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort2_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort2_WIDTH                        =  2; #
our $FM_STAT_TxPort2_BITS                         =  64; #

sub FM_STAT_TxPort2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort2_STRIDE                       =  4; #

sub FM_STAT_TxPort3 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00072 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x90692 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort3_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort3_WIDTH                        =  2; #
our $FM_STAT_TxPort3_BITS                         =  64; #

sub FM_STAT_TxPort3_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort3_STRIDE                       =  4; #

sub FM_STAT_TxPort4 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x000E0 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x90700 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort4_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort4_WIDTH                        =  2; #
our $FM_STAT_TxPort4_BITS                         =  64; #

sub FM_STAT_TxPort4_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort4_STRIDE                       =  4; #

sub FM_STAT_TxPort5 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x000E2 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x90702 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort5_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort5_WIDTH                        =  2; #
our $FM_STAT_TxPort5_BITS                         =  64; #

sub FM_STAT_TxPort5_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort5_STRIDE                       =  4; #

sub FM_STAT_TxPort6 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00150 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x90770 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort6_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort6_WIDTH                        =  2; #
our $FM_STAT_TxPort6_BITS                         =  64; #

sub FM_STAT_TxPort6_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort6_STRIDE                       =  4; #

sub FM_STAT_TxPort7 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00152 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x90772 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort7_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort7_WIDTH                        =  2; #
our $FM_STAT_TxPort7_BITS                         =  64; #

sub FM_STAT_TxPort7_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort7_STRIDE                       =  4; #

sub FM_STAT_TxPort8 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x001C0 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x907E0 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort8_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort8_WIDTH                        =  2; #
our $FM_STAT_TxPort8_BITS                         =  64; #

sub FM_STAT_TxPort8_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort8_STRIDE                       =  4; #

sub FM_STAT_TxPort9 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x001C2 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x907E2 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort9_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort9_WIDTH                        =  2; #
our $FM_STAT_TxPort9_BITS                         =  64; #

sub FM_STAT_TxPort9_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort9_STRIDE                       =  4; #

sub FM_STAT_TxPort10 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00230 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x90850 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort10_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort10_WIDTH                       =  2; #
our $FM_STAT_TxPort10_BITS                        =  64; #

sub FM_STAT_TxPort10_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort10_STRIDE                      =  4; #

sub FM_STAT_TxPort11 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00232 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x90852 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort11_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort11_WIDTH                       =  2; #
our $FM_STAT_TxPort11_BITS                        =  64; #

sub FM_STAT_TxPort11_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort11_STRIDE                      =  4; #

sub FM_STAT_TxPort12 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x002A0 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x908C0 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort12_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort12_WIDTH                       =  2; #
our $FM_STAT_TxPort12_BITS                        =  64; #

sub FM_STAT_TxPort12_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort12_STRIDE                      =  4; #

sub FM_STAT_TxPort13 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x002A2 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x908C2 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort13_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort13_WIDTH                       =  2; #
our $FM_STAT_TxPort13_BITS                        =  64; #

sub FM_STAT_TxPort13_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort13_STRIDE                      =  4; #

sub FM_STAT_TxPort14 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00310 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x90930 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort14_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort14_WIDTH                       =  2; #
our $FM_STAT_TxPort14_BITS                        =  64; #

sub FM_STAT_TxPort14_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort14_STRIDE                      =  4; #

sub FM_STAT_TxPort15 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00312 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x90932 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort15_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort15_WIDTH                       =  2; #
our $FM_STAT_TxPort15_BITS                        =  64; #

sub FM_STAT_TxPort15_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort15_STRIDE                      =  4; #

sub FM_STAT_TxPort16 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00380 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x909A0 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort16_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort16_WIDTH                       =  2; #
our $FM_STAT_TxPort16_BITS                        =  64; #

sub FM_STAT_TxPort16_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort16_STRIDE                      =  4; #

sub FM_STAT_TxPort17 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00382 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x909A2 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort17_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort17_WIDTH                       =  2; #
our $FM_STAT_TxPort17_BITS                        =  64; #

sub FM_STAT_TxPort17_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort17_STRIDE                      =  4; #

sub FM_STAT_TxPort18 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x003F0 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x90A10 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort18_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort18_WIDTH                       =  2; #
our $FM_STAT_TxPort18_BITS                        =  64; #

sub FM_STAT_TxPort18_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort18_STRIDE                      =  4; #

sub FM_STAT_TxPort19 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x003F2 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x90A12 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort19_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort19_WIDTH                       =  2; #
our $FM_STAT_TxPort19_BITS                        =  64; #

sub FM_STAT_TxPort19_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort19_STRIDE                      =  4; #

sub FM_STAT_TxPort20 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00460 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x90A80 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort20_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort20_WIDTH                       =  2; #
our $FM_STAT_TxPort20_BITS                        =  64; #

sub FM_STAT_TxPort20_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort20_STRIDE                      =  4; #

sub FM_STAT_TxPort21 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00462 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x90A82 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort21_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort21_WIDTH                       =  2; #
our $FM_STAT_TxPort21_BITS                        =  64; #

sub FM_STAT_TxPort21_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort21_STRIDE                      =  4; #

sub FM_STAT_TxPort22 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x004D0 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x90AF0 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort22_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort22_WIDTH                       =  2; #
our $FM_STAT_TxPort22_BITS                        =  64; #

sub FM_STAT_TxPort22_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort22_STRIDE                      =  4; #

sub FM_STAT_TxPort23 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x004D2 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x90AF2 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort23_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort23_WIDTH                       =  2; #
our $FM_STAT_TxPort23_BITS                        =  64; #

sub FM_STAT_TxPort23_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort23_STRIDE                      =  4; #

sub FM_STAT_TxPort24 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00540 + $FM_STATS_TX_INGRESS_BASE + ($word)); # 0x90B60 STATS_TX_INGRESS_BASE
}


sub FM_STAT_TxPort24_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxPort24_WIDTH                       =  2; #
our $FM_STAT_TxPort24_BITS                        =  64; #

sub FM_STAT_TxPort24_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxPort24_STRIDE                      =  4; #

sub FM_STAT_RxMinto63 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00000 + $FM_STATS_RX_BIN_BASE + ($word)); # 0x91000 STATS_RX_BIN_BASE
}


sub FM_STAT_RxMinto63_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxMinto63_WIDTH                      =  2; #
our $FM_STAT_RxMinto63_BITS                       =  64; #

sub FM_STAT_RxMinto63_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxMinto63_STRIDE                     =  4; #

sub FM_STAT_Rx64Pkts {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00002 + $FM_STATS_RX_BIN_BASE + ($word)); # 0x91002 STATS_RX_BIN_BASE
}


sub FM_STAT_Rx64Pkts_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Rx64Pkts_WIDTH                       =  2; #
our $FM_STAT_Rx64Pkts_BITS                        =  64; #

sub FM_STAT_Rx64Pkts_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Rx64Pkts_STRIDE                      =  4; #

sub FM_STAT_Rx65to127 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00070 + $FM_STATS_RX_BIN_BASE + ($word)); # 0x91070 STATS_RX_BIN_BASE
}


sub FM_STAT_Rx65to127_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Rx65to127_WIDTH                      =  2; #
our $FM_STAT_Rx65to127_BITS                       =  64; #

sub FM_STAT_Rx65to127_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Rx65to127_STRIDE                     =  4; #

sub FM_STAT_Rx128to255 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00072 + $FM_STATS_RX_BIN_BASE + ($word)); # 0x91072 STATS_RX_BIN_BASE
}


sub FM_STAT_Rx128to255_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Rx128to255_WIDTH                     =  2; #
our $FM_STAT_Rx128to255_BITS                      =  64; #

sub FM_STAT_Rx128to255_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Rx128to255_STRIDE                    =  4; #

sub FM_STAT_Rx256to511 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x000E0 + $FM_STATS_RX_BIN_BASE + ($word)); # 0x910E0 STATS_RX_BIN_BASE
}


sub FM_STAT_Rx256to511_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Rx256to511_WIDTH                     =  2; #
our $FM_STAT_Rx256to511_BITS                      =  64; #

sub FM_STAT_Rx256to511_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Rx256to511_STRIDE                    =  4; #

sub FM_STAT_Rx512to1023 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x000E2 + $FM_STATS_RX_BIN_BASE + ($word)); # 0x910E2 STATS_RX_BIN_BASE
}


sub FM_STAT_Rx512to1023_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Rx512to1023_WIDTH                    =  2; #
our $FM_STAT_Rx512to1023_BITS                     =  64; #

sub FM_STAT_Rx512to1023_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Rx512to1023_STRIDE                   =  4; #

sub FM_STAT_Rx1024to1522 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00150 + $FM_STATS_RX_BIN_BASE + ($word)); # 0x91150 STATS_RX_BIN_BASE
}


sub FM_STAT_Rx1024to1522_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Rx1024to1522_WIDTH                   =  2; #
our $FM_STAT_Rx1024to1522_BITS                    =  64; #

sub FM_STAT_Rx1024to1522_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Rx1024to1522_STRIDE                  =  4; #

sub FM_STAT_Rx1523to2047 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00152 + $FM_STATS_RX_BIN_BASE + ($word)); # 0x91152 STATS_RX_BIN_BASE
}


sub FM_STAT_Rx1523to2047_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Rx1523to2047_WIDTH                   =  2; #
our $FM_STAT_Rx1523to2047_BITS                    =  64; #

sub FM_STAT_Rx1523to2047_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Rx1523to2047_STRIDE                  =  4; #

sub FM_STAT_Rx2048to4095 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x001C0 + $FM_STATS_RX_BIN_BASE + ($word)); # 0x911C0 STATS_RX_BIN_BASE
}


sub FM_STAT_Rx2048to4095_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Rx2048to4095_WIDTH                   =  2; #
our $FM_STAT_Rx2048to4095_BITS                    =  64; #

sub FM_STAT_Rx2048to4095_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Rx2048to4095_STRIDE                  =  4; #

sub FM_STAT_Rx4096to8191 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x001C2 + $FM_STATS_RX_BIN_BASE + ($word)); # 0x911C2 STATS_RX_BIN_BASE
}


sub FM_STAT_Rx4096to8191_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Rx4096to8191_WIDTH                   =  2; #
our $FM_STAT_Rx4096to8191_BITS                    =  64; #

sub FM_STAT_Rx4096to8191_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Rx4096to8191_STRIDE                  =  4; #

sub FM_STAT_Rx8192to10239 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00230 + $FM_STATS_RX_BIN_BASE + ($word)); # 0x91230 STATS_RX_BIN_BASE
}


sub FM_STAT_Rx8192to10239_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Rx8192to10239_WIDTH                  =  2; #
our $FM_STAT_Rx8192to10239_BITS                   =  64; #

sub FM_STAT_Rx8192to10239_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Rx8192to10239_STRIDE                 =  4; #

sub FM_STAT_Rx10240toMax {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00232 + $FM_STATS_RX_BIN_BASE + ($word)); # 0x91232 STATS_RX_BIN_BASE
}


sub FM_STAT_Rx10240toMax_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Rx10240toMax_WIDTH                   =  2; #
our $FM_STAT_Rx10240toMax_BITS                    =  64; #

sub FM_STAT_Rx10240toMax_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Rx10240toMax_STRIDE                  =  4; #

sub FM_STAT_RxFragments {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x002A0 + $FM_STATS_RX_BIN_BASE + ($word)); # 0x912A0 STATS_RX_BIN_BASE
}


sub FM_STAT_RxFragments_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxFragments_WIDTH                    =  2; #
our $FM_STAT_RxFragments_BITS                     =  64; #

sub FM_STAT_RxFragments_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxFragments_STRIDE                   =  4; #

sub FM_STAT_RxUndersized {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x002A2 + $FM_STATS_RX_BIN_BASE + ($word)); # 0x912A2 STATS_RX_BIN_BASE
}


sub FM_STAT_RxUndersized_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxUndersized_WIDTH                   =  2; #
our $FM_STAT_RxUndersized_BITS                    =  64; #

sub FM_STAT_RxUndersized_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxUndersized_STRIDE                  =  4; #

sub FM_STAT_RxOversized {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00310 + $FM_STATS_RX_BIN_BASE + ($word)); # 0x91310 STATS_RX_BIN_BASE
}


sub FM_STAT_RxOversized_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxOversized_WIDTH                    =  2; #
our $FM_STAT_RxOversized_BITS                     =  64; #

sub FM_STAT_RxOversized_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxOversized_STRIDE                   =  4; #

sub FM_STAT_RxOctetsNonIP {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00000 + $FM_STATS_RX_OCTET_BASE + ($word)); # 0x91380 STATS_RX_OCTET_BASE
}


sub FM_STAT_RxOctetsNonIP_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxOctetsNonIP_WIDTH                  =  2; #
our $FM_STAT_RxOctetsNonIP_BITS                   =  64; #

sub FM_STAT_RxOctetsNonIP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxOctetsNonIP_STRIDE                 =  4; #

sub FM_STAT_RxOctetsIPv4 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00002 + $FM_STATS_RX_OCTET_BASE + ($word)); # 0x91382 STATS_RX_OCTET_BASE
}


sub FM_STAT_RxOctetsIPv4_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxOctetsIPv4_WIDTH                   =  2; #
our $FM_STAT_RxOctetsIPv4_BITS                    =  64; #

sub FM_STAT_RxOctetsIPv4_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxOctetsIPv4_STRIDE                  =  4; #

sub FM_STAT_RxOctetsIPv6 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00070 + $FM_STATS_RX_OCTET_BASE + ($word)); # 0x913F0 STATS_RX_OCTET_BASE
}


sub FM_STAT_RxOctetsIPv6_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxOctetsIPv6_WIDTH                   =  2; #
our $FM_STAT_RxOctetsIPv6_BITS                    =  64; #

sub FM_STAT_RxOctetsIPv6_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxOctetsIPv6_STRIDE                  =  4; #

sub FM_STAT_RxOctetsError {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00072 + $FM_STATS_RX_OCTET_BASE + ($word)); # 0x913F2 STATS_RX_OCTET_BASE
}


sub FM_STAT_RxOctetsError_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxOctetsError_WIDTH                  =  2; #
our $FM_STAT_RxOctetsError_BITS                   =  64; #

sub FM_STAT_RxOctetsError_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxOctetsError_STRIDE                 =  4; #

sub FM_STAT_TxMinto63 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00000 + $FM_STATS_TX_BIN_BASE + ($word)); # 0x91460 STATS_TX_BIN_BASE
}


sub FM_STAT_TxMinto63_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxMinto63_WIDTH                      =  2; #
our $FM_STAT_TxMinto63_BITS                       =  64; #

sub FM_STAT_TxMinto63_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxMinto63_STRIDE                     =  4; #

sub FM_STAT_Tx64Pkts {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00002 + $FM_STATS_TX_BIN_BASE + ($word)); # 0x91462 STATS_TX_BIN_BASE
}


sub FM_STAT_Tx64Pkts_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Tx64Pkts_WIDTH                       =  2; #
our $FM_STAT_Tx64Pkts_BITS                        =  64; #

sub FM_STAT_Tx64Pkts_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Tx64Pkts_STRIDE                      =  4; #

sub FM_STAT_Tx65to127 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00070 + $FM_STATS_TX_BIN_BASE + ($word)); # 0x914D0 STATS_TX_BIN_BASE
}


sub FM_STAT_Tx65to127_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Tx65to127_WIDTH                      =  2; #
our $FM_STAT_Tx65to127_BITS                       =  64; #

sub FM_STAT_Tx65to127_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Tx65to127_STRIDE                     =  4; #

sub FM_STAT_Tx128to255 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00072 + $FM_STATS_TX_BIN_BASE + ($word)); # 0x914D2 STATS_TX_BIN_BASE
}


sub FM_STAT_Tx128to255_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Tx128to255_WIDTH                     =  2; #
our $FM_STAT_Tx128to255_BITS                      =  64; #

sub FM_STAT_Tx128to255_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Tx128to255_STRIDE                    =  4; #

sub FM_STAT_Tx256to511 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x000E0 + $FM_STATS_TX_BIN_BASE + ($word)); # 0x91540 STATS_TX_BIN_BASE
}


sub FM_STAT_Tx256to511_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Tx256to511_WIDTH                     =  2; #
our $FM_STAT_Tx256to511_BITS                      =  64; #

sub FM_STAT_Tx256to511_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Tx256to511_STRIDE                    =  4; #

sub FM_STAT_Tx512to1023 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x000E2 + $FM_STATS_TX_BIN_BASE + ($word)); # 0x91542 STATS_TX_BIN_BASE
}


sub FM_STAT_Tx512to1023_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Tx512to1023_WIDTH                    =  2; #
our $FM_STAT_Tx512to1023_BITS                     =  64; #

sub FM_STAT_Tx512to1023_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Tx512to1023_STRIDE                   =  4; #

sub FM_STAT_Tx1024to1522 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00150 + $FM_STATS_TX_BIN_BASE + ($word)); # 0x915B0 STATS_TX_BIN_BASE
}


sub FM_STAT_Tx1024to1522_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Tx1024to1522_WIDTH                   =  2; #
our $FM_STAT_Tx1024to1522_BITS                    =  64; #

sub FM_STAT_Tx1024to1522_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Tx1024to1522_STRIDE                  =  4; #

sub FM_STAT_Tx1523to2047 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00152 + $FM_STATS_TX_BIN_BASE + ($word)); # 0x915B2 STATS_TX_BIN_BASE
}


sub FM_STAT_Tx1523to2047_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Tx1523to2047_WIDTH                   =  2; #
our $FM_STAT_Tx1523to2047_BITS                    =  64; #

sub FM_STAT_Tx1523to2047_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Tx1523to2047_STRIDE                  =  4; #

sub FM_STAT_Tx2048to4095 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x001C0 + $FM_STATS_TX_BIN_BASE + ($word)); # 0x91620 STATS_TX_BIN_BASE
}


sub FM_STAT_Tx2048to4095_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Tx2048to4095_WIDTH                   =  2; #
our $FM_STAT_Tx2048to4095_BITS                    =  64; #

sub FM_STAT_Tx2048to4095_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Tx2048to4095_STRIDE                  =  4; #

sub FM_STAT_Tx4096to8191 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x001C2 + $FM_STATS_TX_BIN_BASE + ($word)); # 0x91622 STATS_TX_BIN_BASE
}


sub FM_STAT_Tx4096to8191_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Tx4096to8191_WIDTH                   =  2; #
our $FM_STAT_Tx4096to8191_BITS                    =  64; #

sub FM_STAT_Tx4096to8191_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Tx4096to8191_STRIDE                  =  4; #

sub FM_STAT_Tx8192to10239 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00230 + $FM_STATS_TX_BIN_BASE + ($word)); # 0x91690 STATS_TX_BIN_BASE
}


sub FM_STAT_Tx8192to10239_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Tx8192to10239_WIDTH                  =  2; #
our $FM_STAT_Tx8192to10239_BITS                   =  64; #

sub FM_STAT_Tx8192to10239_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Tx8192to10239_STRIDE                 =  4; #

sub FM_STAT_Tx10240toMax {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00232 + $FM_STATS_TX_BIN_BASE + ($word)); # 0x91692 STATS_TX_BIN_BASE
}


sub FM_STAT_Tx10240toMax_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Tx10240toMax_WIDTH                   =  2; #
our $FM_STAT_Tx10240toMax_BITS                    =  64; #

sub FM_STAT_Tx10240toMax_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Tx10240toMax_STRIDE                  =  4; #

sub FM_STAT_TxErrors {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x002A0 + $FM_STATS_TX_BIN_BASE + ($word)); # 0x91700 STATS_TX_BIN_BASE
}


sub FM_STAT_TxErrors_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxErrors_WIDTH                       =  2; #
our $FM_STAT_TxErrors_BITS                        =  64; #

sub FM_STAT_TxErrors_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxErrors_STRIDE                      =  4; #

sub FM_STAT_FIDForwarded {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00000 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x92000 STATS_RX_ACTION_BASE
}


sub FM_STAT_FIDForwarded_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_FIDForwarded_WIDTH                   =  2; #
our $FM_STAT_FIDForwarded_BITS                    =  64; #

sub FM_STAT_FIDForwarded_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_FIDForwarded_STRIDE                  =  4; #

sub FM_STAT_FloodForwarded {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00002 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x92002 STATS_RX_ACTION_BASE
}


sub FM_STAT_FloodForwarded_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_FloodForwarded_WIDTH                 =  2; #
our $FM_STAT_FloodForwarded_BITS                  =  64; #

sub FM_STAT_FloodForwarded_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_FloodForwarded_STRIDE                =  4; #

sub FM_STAT_SpeciallyHandled {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00070 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x92070 STATS_RX_ACTION_BASE
}


sub FM_STAT_SpeciallyHandled_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_SpeciallyHandled_WIDTH               =  2; #
our $FM_STAT_SpeciallyHandled_BITS                =  64; #

sub FM_STAT_SpeciallyHandled_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_SpeciallyHandled_STRIDE              =  4; #

sub FM_STAT_ParseErrDrops {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00072 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x92072 STATS_RX_ACTION_BASE
}


sub FM_STAT_ParseErrDrops_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_ParseErrDrops_WIDTH                  =  2; #
our $FM_STAT_ParseErrDrops_BITS                   =  64; #

sub FM_STAT_ParseErrDrops_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_ParseErrDrops_STRIDE                 =  4; #

sub FM_STAT_ParityError {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x000E0 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x920E0 STATS_RX_ACTION_BASE
}


sub FM_STAT_ParityError_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_ParityError_WIDTH                    =  2; #
our $FM_STAT_ParityError_BITS                     =  64; #

sub FM_STAT_ParityError_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_ParityError_STRIDE                   =  4; #

sub FM_STAT_Trapped {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x000E2 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x920E2 STATS_RX_ACTION_BASE
}


sub FM_STAT_Trapped_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Trapped_WIDTH                        =  2; #
our $FM_STAT_Trapped_BITS                         =  64; #

sub FM_STAT_Trapped_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Trapped_STRIDE                       =  4; #

sub FM_STAT_PauseDrops {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00150 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x92150 STATS_RX_ACTION_BASE
}


sub FM_STAT_PauseDrops_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_PauseDrops_WIDTH                     =  2; #
our $FM_STAT_PauseDrops_BITS                      =  64; #

sub FM_STAT_PauseDrops_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_PauseDrops_STRIDE                    =  4; #

sub FM_STAT_STPDrops {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00152 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x92152 STATS_RX_ACTION_BASE
}


sub FM_STAT_STPDrops_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_STPDrops_WIDTH                       =  2; #
our $FM_STAT_STPDrops_BITS                        =  64; #

sub FM_STAT_STPDrops_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_STPDrops_STRIDE                      =  4; #

sub FM_STAT_SecurityViolations {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x001C0 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x921C0 STATS_RX_ACTION_BASE
}


sub FM_STAT_SecurityViolations_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_SecurityViolations_WIDTH             =  2; #
our $FM_STAT_SecurityViolations_BITS              =  64; #

sub FM_STAT_SecurityViolations_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_SecurityViolations_STRIDE            =  4; #

sub FM_STAT_VlanTagDrops {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x001C2 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x921C2 STATS_RX_ACTION_BASE
}


sub FM_STAT_VlanTagDrops_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_VlanTagDrops_WIDTH                   =  2; #
our $FM_STAT_VlanTagDrops_BITS                    =  64; #

sub FM_STAT_VlanTagDrops_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_VlanTagDrops_STRIDE                  =  4; #

sub FM_STAT_VlanIngressDrops {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00230 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x92230 STATS_RX_ACTION_BASE
}


sub FM_STAT_VlanIngressDrops_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_VlanIngressDrops_WIDTH               =  2; #
our $FM_STAT_VlanIngressDrops_BITS                =  64; #

sub FM_STAT_VlanIngressDrops_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_VlanIngressDrops_STRIDE              =  4; #

sub FM_STAT_VlanEgressDrops {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00232 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x92232 STATS_RX_ACTION_BASE
}


sub FM_STAT_VlanEgressDrops_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_VlanEgressDrops_WIDTH                =  2; #
our $FM_STAT_VlanEgressDrops_BITS                 =  64; #

sub FM_STAT_VlanEgressDrops_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_VlanEgressDrops_STRIDE               =  4; #

sub FM_STAT_GlortMissDrops {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x002A0 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x922A0 STATS_RX_ACTION_BASE
}


sub FM_STAT_GlortMissDrops_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_GlortMissDrops_WIDTH                 =  2; #
our $FM_STAT_GlortMissDrops_BITS                  =  64; #

sub FM_STAT_GlortMissDrops_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_GlortMissDrops_STRIDE                =  4; #

sub FM_STAT_FFUDrops {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x002A2 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x922A2 STATS_RX_ACTION_BASE
}


sub FM_STAT_FFUDrops_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_FFUDrops_WIDTH                       =  2; #
our $FM_STAT_FFUDrops_BITS                        =  64; #

sub FM_STAT_FFUDrops_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_FFUDrops_STRIDE                      =  4; #

sub FM_STAT_TriggerDrops {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00310 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x92310 STATS_RX_ACTION_BASE
}


sub FM_STAT_TriggerDrops_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TriggerDrops_WIDTH                   =  2; #
our $FM_STAT_TriggerDrops_BITS                    =  64; #

sub FM_STAT_TriggerDrops_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TriggerDrops_STRIDE                  =  4; #

sub FM_STAT_PolicerDrops {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00312 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x92312 STATS_RX_ACTION_BASE
}


sub FM_STAT_PolicerDrops_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_PolicerDrops_WIDTH                   =  2; #
our $FM_STAT_PolicerDrops_BITS                    =  64; #

sub FM_STAT_PolicerDrops_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_PolicerDrops_STRIDE                  =  4; #

sub FM_STAT_TTLDrops {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00380 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x92380 STATS_RX_ACTION_BASE
}


sub FM_STAT_TTLDrops_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TTLDrops_WIDTH                       =  2; #
our $FM_STAT_TTLDrops_BITS                        =  64; #

sub FM_STAT_TTLDrops_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TTLDrops_STRIDE                      =  4; #

sub FM_STAT_CMPrivDrops {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00382 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x92382 STATS_RX_ACTION_BASE
}


sub FM_STAT_CMPrivDrops_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_CMPrivDrops_WIDTH                    =  2; #
our $FM_STAT_CMPrivDrops_BITS                     =  64; #

sub FM_STAT_CMPrivDrops_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_CMPrivDrops_STRIDE                   =  4; #

sub FM_STAT_SMP0Drops {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x003F0 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x923F0 STATS_RX_ACTION_BASE
}


sub FM_STAT_SMP0Drops_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_SMP0Drops_WIDTH                      =  2; #
our $FM_STAT_SMP0Drops_BITS                       =  64; #

sub FM_STAT_SMP0Drops_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_SMP0Drops_STRIDE                     =  4; #

sub FM_STAT_SMP1Drops {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x003F2 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x923F2 STATS_RX_ACTION_BASE
}


sub FM_STAT_SMP1Drops_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_SMP1Drops_WIDTH                      =  2; #
our $FM_STAT_SMP1Drops_BITS                       =  64; #

sub FM_STAT_SMP1Drops_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_SMP1Drops_STRIDE                     =  4; #

sub FM_STAT_RxHog0Drops {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00460 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x92460 STATS_RX_ACTION_BASE
}


sub FM_STAT_RxHog0Drops_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxHog0Drops_WIDTH                    =  2; #
our $FM_STAT_RxHog0Drops_BITS                     =  64; #

sub FM_STAT_RxHog0Drops_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxHog0Drops_STRIDE                   =  4; #

sub FM_STAT_RxHog1Drops {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00462 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x92462 STATS_RX_ACTION_BASE
}


sub FM_STAT_RxHog1Drops_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxHog1Drops_WIDTH                    =  2; #
our $FM_STAT_RxHog1Drops_BITS                     =  64; #

sub FM_STAT_RxHog1Drops_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxHog1Drops_STRIDE                   =  4; #

sub FM_STAT_TxHog0Drops {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x004D0 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x924D0 STATS_RX_ACTION_BASE
}


sub FM_STAT_TxHog0Drops_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxHog0Drops_WIDTH                    =  2; #
our $FM_STAT_TxHog0Drops_BITS                     =  64; #

sub FM_STAT_TxHog0Drops_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxHog0Drops_STRIDE                   =  4; #

sub FM_STAT_TxHog1Drops {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x004D2 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x924D2 STATS_RX_ACTION_BASE
}


sub FM_STAT_TxHog1Drops_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxHog1Drops_WIDTH                    =  2; #
our $FM_STAT_TxHog1Drops_BITS                     =  64; #

sub FM_STAT_TxHog1Drops_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxHog1Drops_STRIDE                   =  4; #

sub FM_STAT_RateLimit0Drops {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00540 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x92540 STATS_RX_ACTION_BASE
}


sub FM_STAT_RateLimit0Drops_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RateLimit0Drops_WIDTH                =  2; #
our $FM_STAT_RateLimit0Drops_BITS                 =  64; #

sub FM_STAT_RateLimit0Drops_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RateLimit0Drops_STRIDE               =  4; #

sub FM_STAT_RateLimit1Drops {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00542 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x92542 STATS_RX_ACTION_BASE
}


sub FM_STAT_RateLimit1Drops_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RateLimit1Drops_WIDTH                =  2; #
our $FM_STAT_RateLimit1Drops_BITS                 =  64; #

sub FM_STAT_RateLimit1Drops_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RateLimit1Drops_STRIDE               =  4; #

sub FM_STAT_BadSMPDrops {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x005B0 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x925B0 STATS_RX_ACTION_BASE
}


sub FM_STAT_BadSMPDrops_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_BadSMPDrops_WIDTH                    =  2; #
our $FM_STAT_BadSMPDrops_BITS                     =  64; #

sub FM_STAT_BadSMPDrops_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_BadSMPDrops_STRIDE                   =  4; #

sub FM_STAT_TriggerRedirects {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x005B2 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x925B2 STATS_RX_ACTION_BASE
}


sub FM_STAT_TriggerRedirects_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TriggerRedirects_WIDTH               =  2; #
our $FM_STAT_TriggerRedirects_BITS                =  64; #

sub FM_STAT_TriggerRedirects_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TriggerRedirects_STRIDE              =  4; #

sub FM_STAT_Others {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00620 + $FM_STATS_RX_ACTION_BASE + ($word)); # 0x92620 STATS_RX_ACTION_BASE
}


sub FM_STAT_Others_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Others_WIDTH                         =  2; #
our $FM_STAT_Others_BITS                          =  64; #

sub FM_STAT_Others_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_Others_STRIDE                        =  4; #

sub FM_STAT_RxOctetsP0 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00000 + $FM_STATS_RX_OCT_PRI_BASE + ($word)); # 0x92700 STATS_RX_OCT_PRI_BASE
}


sub FM_STAT_RxOctetsP0_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxOctetsP0_WIDTH                     =  2; #
our $FM_STAT_RxOctetsP0_BITS                      =  64; #

sub FM_STAT_RxOctetsP0_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxOctetsP0_STRIDE                    =  4; #

sub FM_STAT_RxOctetsP1 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00002 + $FM_STATS_RX_OCT_PRI_BASE + ($word)); # 0x92702 STATS_RX_OCT_PRI_BASE
}


sub FM_STAT_RxOctetsP1_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxOctetsP1_WIDTH                     =  2; #
our $FM_STAT_RxOctetsP1_BITS                      =  64; #

sub FM_STAT_RxOctetsP1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxOctetsP1_STRIDE                    =  4; #

sub FM_STAT_RxOctetsP2 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00070 + $FM_STATS_RX_OCT_PRI_BASE + ($word)); # 0x92770 STATS_RX_OCT_PRI_BASE
}


sub FM_STAT_RxOctetsP2_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxOctetsP2_WIDTH                     =  2; #
our $FM_STAT_RxOctetsP2_BITS                      =  64; #

sub FM_STAT_RxOctetsP2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxOctetsP2_STRIDE                    =  4; #

sub FM_STAT_RxOctetsP3 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00072 + $FM_STATS_RX_OCT_PRI_BASE + ($word)); # 0x92772 STATS_RX_OCT_PRI_BASE
}


sub FM_STAT_RxOctetsP3_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxOctetsP3_WIDTH                     =  2; #
our $FM_STAT_RxOctetsP3_BITS                      =  64; #

sub FM_STAT_RxOctetsP3_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxOctetsP3_STRIDE                    =  4; #

sub FM_STAT_RxOctetsP4 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x000E0 + $FM_STATS_RX_OCT_PRI_BASE + ($word)); # 0x927E0 STATS_RX_OCT_PRI_BASE
}


sub FM_STAT_RxOctetsP4_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxOctetsP4_WIDTH                     =  2; #
our $FM_STAT_RxOctetsP4_BITS                      =  64; #

sub FM_STAT_RxOctetsP4_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxOctetsP4_STRIDE                    =  4; #

sub FM_STAT_RxOctetsP5 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x000E2 + $FM_STATS_RX_OCT_PRI_BASE + ($word)); # 0x927E2 STATS_RX_OCT_PRI_BASE
}


sub FM_STAT_RxOctetsP5_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxOctetsP5_WIDTH                     =  2; #
our $FM_STAT_RxOctetsP5_BITS                      =  64; #

sub FM_STAT_RxOctetsP5_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxOctetsP5_STRIDE                    =  4; #

sub FM_STAT_RxOctetsP6 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00150 + $FM_STATS_RX_OCT_PRI_BASE + ($word)); # 0x92850 STATS_RX_OCT_PRI_BASE
}


sub FM_STAT_RxOctetsP6_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxOctetsP6_WIDTH                     =  2; #
our $FM_STAT_RxOctetsP6_BITS                      =  64; #

sub FM_STAT_RxOctetsP6_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxOctetsP6_STRIDE                    =  4; #

sub FM_STAT_RxOctetsP7 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00152 + $FM_STATS_RX_OCT_PRI_BASE + ($word)); # 0x92852 STATS_RX_OCT_PRI_BASE
}


sub FM_STAT_RxOctetsP7_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_RxOctetsP7_WIDTH                     =  2; #
our $FM_STAT_RxOctetsP7_BITS                      =  64; #

sub FM_STAT_RxOctetsP7_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_RxOctetsP7_STRIDE                    =  4; #

sub FM_STAT_TxOctets {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00000 + $FM_STATS_TX_OCTET_BASE + ($word)); # 0x928C0 STATS_TX_OCTET_BASE
}


sub FM_STAT_TxOctets_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxOctets_WIDTH                       =  2; #
our $FM_STAT_TxOctets_BITS                        =  64; #

sub FM_STAT_TxOctets_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxOctets_STRIDE                      =  4; #

sub FM_STAT_TxErrorOctets {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00002 + $FM_STATS_TX_OCTET_BASE + ($word)); # 0x928C2 STATS_TX_OCTET_BASE
}


sub FM_STAT_TxErrorOctets_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_TxErrorOctets_WIDTH                  =  2; #
our $FM_STAT_TxErrorOctets_BITS                   =  64; #

sub FM_STAT_TxErrorOctets_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_TxErrorOctets_STRIDE                 =  4; #
our $FM_TX_MIRROR                                 =  (0x00000 + $FM_MTABLE_BASE); # // 0xC0000 MTABLE_BASE

sub FM_TX_MIRROR {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00000 + $FM_MTABLE_BASE); # 0xC0000 MTABLE_BASE
}


sub FM_TX_MIRROR_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_TX_MIRROR_WIDTH                           =  1; #
our $FM_TX_MIRROR_BITS                            =  14; #
our $FM_LOG_MASK                                  =  (0x00001 + $FM_MTABLE_BASE); # // 0xC0001 MTABLE_BASE

sub FM_LOG_MASK {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00001 + $FM_MTABLE_BASE); # 0xC0001 MTABLE_BASE
}


sub FM_LOG_MASK_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_LOG_MASK_WIDTH                            =  1; #
our $FM_LOG_MASK_BITS                             =  25; #

sub FM_LOOPBACK_SUPPRESS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x04000 + $FM_MTABLE_BASE); # 0xC4000 MTABLE_BASE
}


sub FM_LOOPBACK_SUPPRESS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x0000FFFF;
}

our $FM_LOOPBACK_SUPPRESS_WIDTH                   =  1; #
our $FM_LOOPBACK_SUPPRESS_BITS                    =  32; #

sub FM_LOOPBACK_SUPPRESS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_LOOPBACK_SUPPRESS_STRIDE                  =  1; #
our $FM_MIRROR_GLORTS                             =  (0x04019 + $FM_MTABLE_BASE); # // 0xC4019 MTABLE_BASE

sub FM_MIRROR_GLORTS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x04019 + $FM_MTABLE_BASE); # 0xC4019 MTABLE_BASE
}


sub FM_MIRROR_GLORTS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0000FF00;
}

our $FM_MIRROR_GLORTS_WIDTH                       =  1; #
our $FM_MIRROR_GLORTS_BITS                        =  32; #

sub FM_IP_MULTICAST_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x08000 + $FM_MTABLE_BASE); # 0xC8000 MTABLE_BASE
}


sub FM_IP_MULTICAST_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_IP_MULTICAST_TABLE_WIDTH                  =  1; #
our $FM_IP_MULTICAST_TABLE_BITS                   =  30; #

sub FM_IP_MULTICAST_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16384;
}

our $FM_IP_MULTICAST_TABLE_STRIDE                 =  1; #
our $FM_SYS_CFG_1                                 =  (0x00000 + $FM_HANDLER_BASE); # // 0x100000 HANDLER_BASE

sub FM_SYS_CFG_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00000 + $FM_HANDLER_BASE); # 0x100000 HANDLER_BASE
}


sub FM_SYS_CFG_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00001CFF;
}

our $FM_SYS_CFG_1_WIDTH                           =  1; #
our $FM_SYS_CFG_1_BITS                            =  16; #
our $FM_SYS_CFG_3                                 =  (0x00002 + $FM_HANDLER_BASE); # // 0x100002 HANDLER_BASE

sub FM_SYS_CFG_3 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00002 + $FM_HANDLER_BASE); # 0x100002 HANDLER_BASE
}


sub FM_SYS_CFG_3_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SYS_CFG_3_WIDTH                           =  1; #
our $FM_SYS_CFG_3_BITS                            =  16; #
our $FM_SYS_CFG_4                                 =  (0x00003 + $FM_HANDLER_BASE); # // 0x100003 HANDLER_BASE

sub FM_SYS_CFG_4 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00003 + $FM_HANDLER_BASE); # 0x100003 HANDLER_BASE
}


sub FM_SYS_CFG_4_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SYS_CFG_4_WIDTH                           =  1; #
our $FM_SYS_CFG_4_BITS                            =  32; #
our $FM_SYS_CFG_7                                 =  (0x00006 + $FM_HANDLER_BASE); # // 0x100006 HANDLER_BASE

sub FM_SYS_CFG_7 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00006 + $FM_HANDLER_BASE); # 0x100006 HANDLER_BASE
}


sub FM_SYS_CFG_7_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x80007530;
}

our $FM_SYS_CFG_7_WIDTH                           =  1; #
our $FM_SYS_CFG_7_BITS                            =  32; #
our $FM_SYS_CFG_8                                 =  (0x00007 + $FM_HANDLER_BASE); # // 0x100007 HANDLER_BASE

sub FM_SYS_CFG_8 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00007 + $FM_HANDLER_BASE); # 0x100007 HANDLER_BASE
}


sub FM_SYS_CFG_8_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SYS_CFG_8_WIDTH                           =  1; #
our $FM_SYS_CFG_8_BITS                            =  2; #
our $FM_PORT_VLAN_IP_1                            =  (0x00012 + $FM_HANDLER_BASE); # // 0x100012 HANDLER_BASE

sub FM_PORT_VLAN_IP_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00012 + $FM_HANDLER_BASE); # 0x100012 HANDLER_BASE
}


sub FM_PORT_VLAN_IP_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PORT_VLAN_IP_1_WIDTH                      =  1; #
our $FM_PORT_VLAN_IP_1_BITS                       =  32; #
our $FM_PORT_VLAN_IM_1                            =  (0x00013 + $FM_HANDLER_BASE); # // 0x100013 HANDLER_BASE

sub FM_PORT_VLAN_IM_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00013 + $FM_HANDLER_BASE); # 0x100013 HANDLER_BASE
}


sub FM_PORT_VLAN_IM_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x01FFFFFE;
}

our $FM_PORT_VLAN_IM_1_WIDTH                      =  1; #
our $FM_PORT_VLAN_IM_1_BITS                       =  32; #
our $FM_PORT_VLAN_IP_2                            =  (0x00014 + $FM_HANDLER_BASE); # // 0x100014 HANDLER_BASE

sub FM_PORT_VLAN_IP_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00014 + $FM_HANDLER_BASE); # 0x100014 HANDLER_BASE
}


sub FM_PORT_VLAN_IP_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PORT_VLAN_IP_2_WIDTH                      =  1; #
our $FM_PORT_VLAN_IP_2_BITS                       =  32; #
our $FM_PORT_VLAN_IM_2                            =  (0x00015 + $FM_HANDLER_BASE); # // 0x100015 HANDLER_BASE

sub FM_PORT_VLAN_IM_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00015 + $FM_HANDLER_BASE); # 0x100015 HANDLER_BASE
}


sub FM_PORT_VLAN_IM_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x01FFFFFE;
}

our $FM_PORT_VLAN_IM_2_WIDTH                      =  1; #
our $FM_PORT_VLAN_IM_2_BITS                       =  32; #
our $FM_PORT_MAC_SEC_IP                           =  (0x00016 + $FM_HANDLER_BASE); # // 0x100016 HANDLER_BASE

sub FM_PORT_MAC_SEC_IP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00016 + $FM_HANDLER_BASE); # 0x100016 HANDLER_BASE
}


sub FM_PORT_MAC_SEC_IP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PORT_MAC_SEC_IP_WIDTH                     =  1; #
our $FM_PORT_MAC_SEC_IP_BITS                      =  32; #
our $FM_PORT_MAC_SEC_IM                           =  (0x00017 + $FM_HANDLER_BASE); # // 0x100017 HANDLER_BASE

sub FM_PORT_MAC_SEC_IM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00017 + $FM_HANDLER_BASE); # 0x100017 HANDLER_BASE
}


sub FM_PORT_MAC_SEC_IM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x01FFFFFE;
}

our $FM_PORT_MAC_SEC_IM_WIDTH                     =  1; #
our $FM_PORT_MAC_SEC_IM_BITS                      =  25; #
our $FM_FH_INT_DETECT                             =  (0x00018 + $FM_HANDLER_BASE); # // 0x100018 HANDLER_BASE

sub FM_FH_INT_DETECT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00018 + $FM_HANDLER_BASE); # 0x100018 HANDLER_BASE
}


sub FM_FH_INT_DETECT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FH_INT_DETECT_WIDTH                       =  1; #
our $FM_FH_INT_DETECT_BITS                        =  9; #
our $FM_SYS_CFG_ROUTER                            =  (0x00019 + $FM_HANDLER_BASE); # // 0x100019 HANDLER_BASE

sub FM_SYS_CFG_ROUTER {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00019 + $FM_HANDLER_BASE); # 0x100019 HANDLER_BASE
}


sub FM_SYS_CFG_ROUTER_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SYS_CFG_ROUTER_WIDTH                      =  1; #
our $FM_SYS_CFG_ROUTER_BITS                       =  3; #
our $FM_L34_HASH_CFG                              =  (0x00020 + $FM_HANDLER_BASE); # // 0x100020 HANDLER_BASE

sub FM_L34_HASH_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00020 + $FM_HANDLER_BASE); # 0x100020 HANDLER_BASE
}


sub FM_L34_HASH_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0101033E;
}

our $FM_L34_HASH_CFG_WIDTH                        =  1; #
our $FM_L34_HASH_CFG_BITS                         =  32; #
our $FM_L34_FLOW_HASH_CFG_1                       =  (0x00021 + $FM_HANDLER_BASE); # // 0x100021 HANDLER_BASE

sub FM_L34_FLOW_HASH_CFG_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00021 + $FM_HANDLER_BASE); # 0x100021 HANDLER_BASE
}


sub FM_L34_FLOW_HASH_CFG_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_L34_FLOW_HASH_CFG_1_WIDTH                 =  1; #
our $FM_L34_FLOW_HASH_CFG_1_BITS                  =  16; #
our $FM_L34_FLOW_HASH_CFG_2                       =  (0x00022 + $FM_HANDLER_BASE); # // 0x100022 HANDLER_BASE

sub FM_L34_FLOW_HASH_CFG_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00022 + $FM_HANDLER_BASE); # 0x100022 HANDLER_BASE
}


sub FM_L34_FLOW_HASH_CFG_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_L34_FLOW_HASH_CFG_2_WIDTH                 =  1; #
our $FM_L34_FLOW_HASH_CFG_2_BITS                  =  20; #
our $FM_L234_HASH_CFG                             =  (0x00023 + $FM_HANDLER_BASE); # // 0x100023 HANDLER_BASE

sub FM_L234_HASH_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00023 + $FM_HANDLER_BASE); # 0x100023 HANDLER_BASE
}


sub FM_L234_HASH_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0000E5FB;
}

our $FM_L234_HASH_CFG_WIDTH                       =  1; #
our $FM_L234_HASH_CFG_BITS                        =  16; #
our $FM_TX_MIRROR_FH                              =  (0x0002E + $FM_HANDLER_BASE); # // 0x10002E HANDLER_BASE

sub FM_TX_MIRROR_FH {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0002E + $FM_HANDLER_BASE); # 0x10002E HANDLER_BASE
}


sub FM_TX_MIRROR_FH_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00003F00;
}

our $FM_TX_MIRROR_FH_WIDTH                        =  1; #
our $FM_TX_MIRROR_FH_BITS                         =  14; #
our $FM_CPU_TRAP_MASK_FH                          =  (0x0002F + $FM_HANDLER_BASE); # // 0x10002F HANDLER_BASE

sub FM_CPU_TRAP_MASK_FH {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0002F + $FM_HANDLER_BASE); # 0x10002F HANDLER_BASE
}


sub FM_CPU_TRAP_MASK_FH_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000001;
}

our $FM_CPU_TRAP_MASK_FH_WIDTH                    =  1; #
our $FM_CPU_TRAP_MASK_FH_BITS                     =  25; #
our $FM_CPU_LOG_MASK_FH                           =  (0x00030 + $FM_HANDLER_BASE); # // 0x100030 HANDLER_BASE

sub FM_CPU_LOG_MASK_FH {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00030 + $FM_HANDLER_BASE); # 0x100030 HANDLER_BASE
}


sub FM_CPU_LOG_MASK_FH_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000001;
}

our $FM_CPU_LOG_MASK_FH_WIDTH                     =  1; #
our $FM_CPU_LOG_MASK_FH_BITS                      =  25; #
our $FM_RX_MIRROR_CFG                             =  (0x00031 + $FM_HANDLER_BASE); # // 0x100031 HANDLER_BASE

sub FM_RX_MIRROR_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00031 + $FM_HANDLER_BASE); # 0x100031 HANDLER_BASE
}


sub FM_RX_MIRROR_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_RX_MIRROR_CFG_WIDTH                       =  1; #
our $FM_RX_MIRROR_CFG_BITS                        =  21; #
our $FM_PARITY_IP                                 =  (0x00032 + $FM_HANDLER_BASE); # // 0x100032 HANDLER_BASE

sub FM_PARITY_IP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00032 + $FM_HANDLER_BASE); # 0x100032 HANDLER_BASE
}


sub FM_PARITY_IP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PARITY_IP_WIDTH                           =  1; #
our $FM_PARITY_IP_BITS                            =  10; #
our $FM_PARITY_IM                                 =  (0x00033 + $FM_HANDLER_BASE); # // 0x100033 HANDLER_BASE

sub FM_PARITY_IM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00033 + $FM_HANDLER_BASE); # 0x100033 HANDLER_BASE
}


sub FM_PARITY_IM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x000001FF;
}

our $FM_PARITY_IM_WIDTH                           =  1; #
our $FM_PARITY_IM_BITS                            =  10; #
our $FM_TRAP_GLORT                                =  (0x00035 + $FM_HANDLER_BASE); # // 0x100035 HANDLER_BASE

sub FM_TRAP_GLORT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00035 + $FM_HANDLER_BASE); # 0x100035 HANDLER_BASE
}


sub FM_TRAP_GLORT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_TRAP_GLORT_WIDTH                          =  1; #
our $FM_TRAP_GLORT_BITS                           =  16; #

sub FM_SAF_MATRIX {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00040 + $FM_HANDLER_BASE); # 0x100040 HANDLER_BASE
}


sub FM_SAF_MATRIX_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

  if($index == 0) {
    return 0x01FFFFFF;
  }
  if($index == 1) {
    return 0x00000001;
  }
  if($index == 2) {
    return 0x00000001;
  }
  if($index == 3) {
    return 0x00000001;
  }
  if($index == 4) {
    return 0x00000001;
  }
  if($index == 5) {
    return 0x00000001;
  }
  if($index == 6) {
    return 0x00000001;
  }
  if($index == 7) {
    return 0x00000001;
  }
  if($index == 8) {
    return 0x00000001;
  }
  if($index == 9) {
    return 0x00000001;
  }
  if($index == 10) {
    return 0x00000001;
  }
  if($index == 11) {
    return 0x00000001;
  }
  if($index == 12) {
    return 0x00000001;
  }
  if($index == 13) {
    return 0x00000001;
  }
  if($index == 14) {
    return 0x00000001;
  }
  if($index == 15) {
    return 0x00000001;
  }
  if($index == 16) {
    return 0x00000001;
  }
  if($index == 17) {
    return 0x00000001;
  }
  if($index == 18) {
    return 0x00000001;
  }
  if($index == 19) {
    return 0x00000001;
  }
  if($index == 20) {
    return 0x00000001;
  }
  if($index == 21) {
    return 0x00000001;
  }
  if($index == 22) {
    return 0x00000001;
  }
  if($index == 23) {
    return 0x00000001;
  }
  if($index == 24) {
    return 0x00000001;
  }
}

our $FM_SAF_MATRIX_WIDTH                          =  1; #
our $FM_SAF_MATRIX_BITS                           =  25; #

sub FM_SAF_MATRIX_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_SAF_MATRIX_STRIDE                         =  1; #

sub FM_FH_LOOPBACK_SUPPRESS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00060 + $FM_HANDLER_BASE); # 0x100060 HANDLER_BASE
}


sub FM_FH_LOOPBACK_SUPPRESS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x0000FFFF;
}

our $FM_FH_LOOPBACK_SUPPRESS_WIDTH                =  1; #
our $FM_FH_LOOPBACK_SUPPRESS_BITS                 =  32; #

sub FM_FH_LOOPBACK_SUPPRESS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_FH_LOOPBACK_SUPPRESS_STRIDE               =  1; #
our $FM_INTERNAL_PORT_MASK                        =  (0x00080 + $FM_HANDLER_BASE); # // 0x100080 HANDLER_BASE

sub FM_INTERNAL_PORT_MASK {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00080 + $FM_HANDLER_BASE); # 0x100080 HANDLER_BASE
}


sub FM_INTERNAL_PORT_MASK_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x01FFFFFF;
}

our $FM_INTERNAL_PORT_MASK_WIDTH                  =  1; #
our $FM_INTERNAL_PORT_MASK_BITS                   =  25; #
our $FM_MGMT_CLK_COUNTER                          =  (0x00082 + $FM_HANDLER_BASE); # // 0x100082 HANDLER_BASE

sub FM_MGMT_CLK_COUNTER {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00082 + $FM_HANDLER_BASE); # 0x100082 HANDLER_BASE
}


sub FM_MGMT_CLK_COUNTER_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000004;
}

our $FM_MGMT_CLK_COUNTER_WIDTH                    =  1; #
our $FM_MGMT_CLK_COUNTER_BITS                     =  21; #
our $FM_MGMT_FFU_CLK_COUNTER                      =  (0x00083 + $FM_HANDLER_BASE); # // 0x100083 HANDLER_BASE

sub FM_MGMT_FFU_CLK_COUNTER {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00083 + $FM_HANDLER_BASE); # 0x100083 HANDLER_BASE
}


sub FM_MGMT_FFU_CLK_COUNTER_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000014;
}

our $FM_MGMT_FFU_CLK_COUNTER_WIDTH                =  1; #
our $FM_MGMT_FFU_CLK_COUNTER_BITS                 =  21; #
our $FM_HEAD_TAIL_SPACING_1                       =  (0x00084 + $FM_HANDLER_BASE); # // 0x100084 HANDLER_BASE

sub FM_HEAD_TAIL_SPACING_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00084 + $FM_HANDLER_BASE); # 0x100084 HANDLER_BASE
}


sub FM_HEAD_TAIL_SPACING_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000041;
}

our $FM_HEAD_TAIL_SPACING_1_WIDTH                 =  1; #
our $FM_HEAD_TAIL_SPACING_1_BITS                  =  8; #
our $FM_HEAD_TAIL_SPACING_2                       =  (0x00085 + $FM_HANDLER_BASE); # // 0x100085 HANDLER_BASE

sub FM_HEAD_TAIL_SPACING_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00085 + $FM_HANDLER_BASE); # 0x100085 HANDLER_BASE
}


sub FM_HEAD_TAIL_SPACING_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000284;
}

our $FM_HEAD_TAIL_SPACING_2_WIDTH                 =  1; #
our $FM_HEAD_TAIL_SPACING_2_BITS                  =  12; #
our $FM_HEAD_TAIL_SPACING_3                       =  (0x00086 + $FM_HANDLER_BASE); # // 0x100086 HANDLER_BASE

sub FM_HEAD_TAIL_SPACING_3 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00086 + $FM_HANDLER_BASE); # 0x100086 HANDLER_BASE
}


sub FM_HEAD_TAIL_SPACING_3_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000284;
}

our $FM_HEAD_TAIL_SPACING_3_WIDTH                 =  1; #
our $FM_HEAD_TAIL_SPACING_3_BITS                  =  12; #
our $FM_PRE_TO_MID_FIFO_CFG                       =  (0x00087 + $FM_HANDLER_BASE); # // 0x100087 HANDLER_BASE

sub FM_PRE_TO_MID_FIFO_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00087 + $FM_HANDLER_BASE); # 0x100087 HANDLER_BASE
}


sub FM_PRE_TO_MID_FIFO_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x000001C6;
}

our $FM_PRE_TO_MID_FIFO_CFG_WIDTH                 =  1; #
our $FM_PRE_TO_MID_FIFO_CFG_BITS                  =  12; #

sub FM_PORT_CFG_1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00000 + $FM_ASSOC_BASE); # 0x101000 ASSOC_BASE
}


sub FM_PORT_CFG_1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x29000001;
}

our $FM_PORT_CFG_1_WIDTH                          =  1; #
our $FM_PORT_CFG_1_BITS                           =  32; #

sub FM_PORT_CFG_1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_PORT_CFG_1_STRIDE                         =  1; #

sub FM_PORT_CFG_2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00020 + $FM_ASSOC_BASE); # 0x101020 ASSOC_BASE
}


sub FM_PORT_CFG_2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x01FFFFFF;
}

our $FM_PORT_CFG_2_WIDTH                          =  1; #
our $FM_PORT_CFG_2_BITS                           =  25; #

sub FM_PORT_CFG_2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_PORT_CFG_2_STRIDE                         =  1; #

sub FM_PORT_CFG_3 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00040 + $FM_ASSOC_BASE); # 0x101040 ASSOC_BASE
}


sub FM_PORT_CFG_3_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000022;
}

our $FM_PORT_CFG_3_WIDTH                          =  1; #
our $FM_PORT_CFG_3_BITS                           =  7; #

sub FM_PORT_CFG_3_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_PORT_CFG_3_STRIDE                         =  1; #

sub FM_PORT_CFG_ISL {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00060 + $FM_ASSOC_BASE); # 0x101060 ASSOC_BASE
}


sub FM_PORT_CFG_ISL_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_PORT_CFG_ISL_WIDTH                        =  1; #
our $FM_PORT_CFG_ISL_BITS                         =  28; #

sub FM_PORT_CFG_ISL_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_PORT_CFG_ISL_STRIDE                       =  1; #

sub FM_RX_VPRI_MAP {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00080 + $FM_ASSOC_BASE + ($word)); # 0x101080 ASSOC_BASE
}


sub FM_RX_VPRI_MAP_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x33221100;}
    if($word == 1) {return 0x77665544;}
}

our $FM_RX_VPRI_MAP_WIDTH                         =  2; #
our $FM_RX_VPRI_MAP_BITS                          =  64; #

sub FM_RX_VPRI_MAP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_RX_VPRI_MAP_STRIDE                        =  2; #

sub FM_DSCP_PRI_MAP {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x000C0 + $FM_ASSOC_BASE); # 0x1010C0 ASSOC_BASE
}


sub FM_DSCP_PRI_MAP_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_DSCP_PRI_MAP_WIDTH                        =  1; #
our $FM_DSCP_PRI_MAP_BITS                         =  4; #

sub FM_DSCP_PRI_MAP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_DSCP_PRI_MAP_STRIDE                       =  1; #

sub FM_VPRI_PRI_MAP {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00100 + $FM_ASSOC_BASE); # 0x101100 ASSOC_BASE
}


sub FM_VPRI_PRI_MAP_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

  if($index == 0) {
    return 0x00000000;
  }
  if($index == 1) {
    return 0x00000001;
  }
  if($index == 2) {
    return 0x00000002;
  }
  if($index == 3) {
    return 0x00000003;
  }
  if($index == 4) {
    return 0x00000004;
  }
  if($index == 5) {
    return 0x00000005;
  }
  if($index == 6) {
    return 0x00000006;
  }
  if($index == 7) {
    return 0x00000007;
  }
  if($index == 8) {
    return 0x00000008;
  }
  if($index == 9) {
    return 0x00000009;
  }
  if($index == 10) {
    return 0x0000000A;
  }
  if($index == 11) {
    return 0x0000000B;
  }
  if($index == 12) {
    return 0x0000000C;
  }
  if($index == 13) {
    return 0x0000000D;
  }
  if($index == 14) {
    return 0x0000000E;
  }
  if($index == 15) {
    return 0x0000000F;
  }
}

our $FM_VPRI_PRI_MAP_WIDTH                        =  1; #
our $FM_VPRI_PRI_MAP_BITS                         =  4; #

sub FM_VPRI_PRI_MAP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}

our $FM_VPRI_PRI_MAP_STRIDE                       =  1; #

sub FM_CM_TX_TC_PRIVATE_WM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00008 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00000 + $FM_CM_BASE); # 0x103000 CM_BASE
}


sub FM_CM_TX_TC_PRIVATE_WM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00001FFF;
}

our $FM_CM_TX_TC_PRIVATE_WM_WIDTH                 =  1; #
our $FM_CM_TX_TC_PRIVATE_WM_BITS                  =  13; #

sub FM_CM_TX_TC_PRIVATE_WM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}


sub FM_CM_TX_TC_PRIVATE_WM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CM_TX_TC_PRIVATE_WM_STRIDE_0              =  1; #
our $FM_CM_TX_TC_PRIVATE_WM_STRIDE_1              =  8; #

sub FM_CM_TX_TC_USAGE {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00008 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00100 + $FM_CM_BASE); # 0x103100 CM_BASE
}


sub FM_CM_TX_TC_USAGE_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_CM_TX_TC_USAGE_WIDTH                      =  1; #
our $FM_CM_TX_TC_USAGE_BITS                       =  14; #

sub FM_CM_TX_TC_USAGE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}


sub FM_CM_TX_TC_USAGE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CM_TX_TC_USAGE_STRIDE_0                   =  1; #
our $FM_CM_TX_TC_USAGE_STRIDE_1                   =  8; #

sub FM_CM_TX_SMP_HOG_WM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00200 + $FM_CM_BASE); # 0x103200 CM_BASE
}


sub FM_CM_TX_SMP_HOG_WM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00001FFF;
}

our $FM_CM_TX_SMP_HOG_WM_WIDTH                    =  1; #
our $FM_CM_TX_SMP_HOG_WM_BITS                     =  13; #

sub FM_CM_TX_SMP_HOG_WM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}


sub FM_CM_TX_SMP_HOG_WM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CM_TX_SMP_HOG_WM_STRIDE_0                 =  1; #
our $FM_CM_TX_SMP_HOG_WM_STRIDE_1                 =  4; #

sub FM_CM_RX_SMP_PAUSE_WM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00280 + $FM_CM_BASE); # 0x103280 CM_BASE
}


sub FM_CM_RX_SMP_PAUSE_WM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x1FFF1FFF;
}

our $FM_CM_RX_SMP_PAUSE_WM_WIDTH                  =  1; #
our $FM_CM_RX_SMP_PAUSE_WM_BITS                   =  29; #

sub FM_CM_RX_SMP_PAUSE_WM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}


sub FM_CM_RX_SMP_PAUSE_WM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CM_RX_SMP_PAUSE_WM_STRIDE_0               =  1; #
our $FM_CM_RX_SMP_PAUSE_WM_STRIDE_1               =  2; #

sub FM_CM_RX_SMP_PRIVATE_WM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x002C0 + $FM_CM_BASE); # 0x1032C0 CM_BASE
}


sub FM_CM_RX_SMP_PRIVATE_WM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00001FFF;
}

our $FM_CM_RX_SMP_PRIVATE_WM_WIDTH                =  1; #
our $FM_CM_RX_SMP_PRIVATE_WM_BITS                 =  13; #

sub FM_CM_RX_SMP_PRIVATE_WM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}


sub FM_CM_RX_SMP_PRIVATE_WM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CM_RX_SMP_PRIVATE_WM_STRIDE_0             =  1; #
our $FM_CM_RX_SMP_PRIVATE_WM_STRIDE_1             =  2; #

sub FM_CM_RX_SMP_USAGE {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00300 + $FM_CM_BASE); # 0x103300 CM_BASE
}


sub FM_CM_RX_SMP_USAGE_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_CM_RX_SMP_USAGE_WIDTH                     =  1; #
our $FM_CM_RX_SMP_USAGE_BITS                      =  14; #

sub FM_CM_RX_SMP_USAGE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}


sub FM_CM_RX_SMP_USAGE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CM_RX_SMP_USAGE_STRIDE_0                  =  1; #
our $FM_CM_RX_SMP_USAGE_STRIDE_1                  =  2; #

sub FM_CM_TX_SMP_USAGE {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00380 + $FM_CM_BASE); # 0x103380 CM_BASE
}


sub FM_CM_TX_SMP_USAGE_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_CM_TX_SMP_USAGE_WIDTH                     =  1; #
our $FM_CM_TX_SMP_USAGE_BITS                      =  14; #

sub FM_CM_TX_SMP_USAGE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}


sub FM_CM_TX_SMP_USAGE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CM_TX_SMP_USAGE_STRIDE_0                  =  1; #
our $FM_CM_TX_SMP_USAGE_STRIDE_1                  =  2; #

sub FM_CM_TX_SMP_PRIVATE_WM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x003C0 + $FM_CM_BASE); # 0x1033C0 CM_BASE
}


sub FM_CM_TX_SMP_PRIVATE_WM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00001FFF;
}

our $FM_CM_TX_SMP_PRIVATE_WM_WIDTH                =  1; #
our $FM_CM_TX_SMP_PRIVATE_WM_BITS                 =  13; #

sub FM_CM_TX_SMP_PRIVATE_WM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}


sub FM_CM_TX_SMP_PRIVATE_WM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CM_TX_SMP_PRIVATE_WM_STRIDE_0             =  1; #
our $FM_CM_TX_SMP_PRIVATE_WM_STRIDE_1             =  2; #

sub FM_CM_RX_SMP_HOG_WM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00400 + $FM_CM_BASE); # 0x103400 CM_BASE
}


sub FM_CM_RX_SMP_HOG_WM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00001FFF;
}

our $FM_CM_RX_SMP_HOG_WM_WIDTH                    =  1; #
our $FM_CM_RX_SMP_HOG_WM_BITS                     =  13; #

sub FM_CM_RX_SMP_HOG_WM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}


sub FM_CM_RX_SMP_HOG_WM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CM_RX_SMP_HOG_WM_STRIDE_0                 =  1; #
our $FM_CM_RX_SMP_HOG_WM_STRIDE_1                 =  2; #

sub FM_CM_PAUSE_RESEND_INTERVAL {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00480 + $FM_CM_BASE); # 0x103480 CM_BASE
}


sub FM_CM_PAUSE_RESEND_INTERVAL_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000100;
}

our $FM_CM_PAUSE_RESEND_INTERVAL_WIDTH            =  1; #
our $FM_CM_PAUSE_RESEND_INTERVAL_BITS             =  16; #

sub FM_CM_PAUSE_RESEND_INTERVAL_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CM_PAUSE_RESEND_INTERVAL_STRIDE           =  1; #

sub FM_CM_PORT_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x004A0 + $FM_CM_BASE); # 0x1034A0 CM_BASE
}


sub FM_CM_PORT_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x0000FF00;
}

our $FM_CM_PORT_CFG_WIDTH                         =  1; #
our $FM_CM_PORT_CFG_BITS                          =  21; #

sub FM_CM_PORT_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CM_PORT_CFG_STRIDE                        =  1; #

sub FM_CM_RX_USAGE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x004C0 + $FM_CM_BASE); # 0x1034C0 CM_BASE
}


sub FM_CM_RX_USAGE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CM_RX_USAGE_WIDTH                         =  1; #
our $FM_CM_RX_USAGE_BITS                          =  14; #

sub FM_CM_RX_USAGE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CM_RX_USAGE_STRIDE                        =  1; #

sub FM_CM_SHARED_WM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x004E0 + $FM_CM_BASE); # 0x1034E0 CM_BASE
}


sub FM_CM_SHARED_WM_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x0000021C;
}

our $FM_CM_SHARED_WM_WIDTH                        =  1; #
our $FM_CM_SHARED_WM_BITS                         =  13; #

sub FM_CM_SHARED_WM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}

our $FM_CM_SHARED_WM_STRIDE                       =  1; #

sub FM_CM_PAUSE_DECIMATION {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x004F0 + $FM_CM_BASE); # 0x1034F0 CM_BASE
}


sub FM_CM_PAUSE_DECIMATION_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x001A4000;
}

our $FM_CM_PAUSE_DECIMATION_WIDTH                 =  1; #
our $FM_CM_PAUSE_DECIMATION_BITS                  =  32; #

sub FM_CM_PAUSE_DECIMATION_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}

our $FM_CM_PAUSE_DECIMATION_STRIDE                =  1; #

sub FM_CM_SHARED_SMP_PAUSE_WM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x004F8 + $FM_CM_BASE); # 0x1034F8 CM_BASE
}


sub FM_CM_SHARED_SMP_PAUSE_WM_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x1FFF1FFF;
}

our $FM_CM_SHARED_SMP_PAUSE_WM_WIDTH              =  1; #
our $FM_CM_SHARED_SMP_PAUSE_WM_BITS               =  29; #

sub FM_CM_SHARED_SMP_PAUSE_WM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}

our $FM_CM_SHARED_SMP_PAUSE_WM_STRIDE             =  1; #

sub FM_CM_SHARED_SMP_USAGE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x004FA + $FM_CM_BASE); # 0x1034FA CM_BASE
}


sub FM_CM_SHARED_SMP_USAGE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CM_SHARED_SMP_USAGE_WIDTH                 =  1; #
our $FM_CM_SHARED_SMP_USAGE_BITS                  =  14; #

sub FM_CM_SHARED_SMP_USAGE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}

our $FM_CM_SHARED_SMP_USAGE_STRIDE                =  1; #
our $FM_CM_GLOBAL_USAGE                           =  (0x004FC + $FM_CM_BASE); # // 0x1034FC CM_BASE

sub FM_CM_GLOBAL_USAGE {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x004FC + $FM_CM_BASE); # 0x1034FC CM_BASE
}


sub FM_CM_GLOBAL_USAGE_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_CM_GLOBAL_USAGE_WIDTH                     =  1; #
our $FM_CM_GLOBAL_USAGE_BITS                      =  14; #
our $FM_CM_GLOBAL_WM                              =  (0x004FD + $FM_CM_BASE); # // 0x1034FD CM_BASE

sub FM_CM_GLOBAL_WM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x004FD + $FM_CM_BASE); # 0x1034FD CM_BASE
}


sub FM_CM_GLOBAL_WM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000FA0;
}

our $FM_CM_GLOBAL_WM_WIDTH                        =  1; #
our $FM_CM_GLOBAL_WM_BITS                         =  13; #
our $FM_CM_SMP_MEMBERSHIP                         =  (0x004FE + $FM_CM_BASE); # // 0x1034FE CM_BASE

sub FM_CM_SMP_MEMBERSHIP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x004FE + $FM_CM_BASE); # 0x1034FE CM_BASE
}


sub FM_CM_SMP_MEMBERSHIP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_CM_SMP_MEMBERSHIP_WIDTH                   =  1; #
our $FM_CM_SMP_MEMBERSHIP_BITS                    =  32; #
our $FM_CM_TX_HOG_MAP                             =  (0x004FF + $FM_CM_BASE); # // 0x1034FF CM_BASE

sub FM_CM_TX_HOG_MAP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x004FF + $FM_CM_BASE); # 0x1034FF CM_BASE
}


sub FM_CM_TX_HOG_MAP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_CM_TX_HOG_MAP_WIDTH                       =  1; #
our $FM_CM_TX_HOG_MAP_BITS                        =  16; #
our $FM_CN_CPID_MASK                              =  (0x00500 + $FM_CM_BASE); # // 0x103500 CM_BASE

sub FM_CN_CPID_MASK {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00500 + $FM_CM_BASE); # 0x103500 CM_BASE
}


sub FM_CN_CPID_MASK_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_CN_CPID_MASK_WIDTH                        =  1; #
our $FM_CN_CPID_MASK_BITS                         =  16; #

sub FM_CN_VCN_DMAC_2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00540 + $FM_CM_BASE); # 0x103540 CM_BASE
}


sub FM_CN_VCN_DMAC_2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x0000EEFF;
}

our $FM_CN_VCN_DMAC_2_WIDTH                       =  1; #
our $FM_CN_VCN_DMAC_2_BITS                        =  16; #

sub FM_CN_VCN_DMAC_2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CN_VCN_DMAC_2_STRIDE                      =  1; #

sub FM_CN_RATE_LIM_CPID {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00580 + $FM_CM_BASE); # 0x103580 CM_BASE
}


sub FM_CN_RATE_LIM_CPID_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x0000000F;
}

our $FM_CN_RATE_LIM_CPID_WIDTH                    =  1; #
our $FM_CN_RATE_LIM_CPID_BITS                     =  4; #

sub FM_CN_RATE_LIM_CPID_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}


sub FM_CN_RATE_LIM_CPID_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CN_RATE_LIM_CPID_STRIDE_0                 =  1; #
our $FM_CN_RATE_LIM_CPID_STRIDE_1                 =  2; #

sub FM_CN_SMP_CFG {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x005C0 + $FM_CM_BASE); # 0x1035C0 CM_BASE
}


sub FM_CN_SMP_CFG_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_CN_SMP_CFG_WIDTH                          =  1; #
our $FM_CN_SMP_CFG_BITS                           =  32; #

sub FM_CN_SMP_CFG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}


sub FM_CN_SMP_CFG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CN_SMP_CFG_STRIDE_0                       =  1; #
our $FM_CN_SMP_CFG_STRIDE_1                       =  2; #

sub FM_CN_SMP_THRESHOLD {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00600 + $FM_CM_BASE); # 0x103600 CM_BASE
}


sub FM_CN_SMP_THRESHOLD_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_CN_SMP_THRESHOLD_WIDTH                    =  1; #
our $FM_CN_SMP_THRESHOLD_BITS                     =  32; #

sub FM_CN_SMP_THRESHOLD_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}


sub FM_CN_SMP_THRESHOLD_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CN_SMP_THRESHOLD_STRIDE_0                 =  1; #
our $FM_CN_SMP_THRESHOLD_STRIDE_1                 =  2; #
our $FM_CN_SAMPLE_CFG                             =  (0x00640 + $FM_CM_BASE); # // 0x103640 CM_BASE

sub FM_CN_SAMPLE_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00640 + $FM_CM_BASE); # 0x103640 CM_BASE
}


sub FM_CN_SAMPLE_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x000F0064;
}

our $FM_CN_SAMPLE_CFG_WIDTH                       =  1; #
our $FM_CN_SAMPLE_CFG_BITS                        =  20; #

sub FM_CN_RATE_LIM_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00660 + $FM_CM_BASE); # 0x103660 CM_BASE
}


sub FM_CN_RATE_LIM_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x15151501;
}

our $FM_CN_RATE_LIM_CFG_WIDTH                     =  1; #
our $FM_CN_RATE_LIM_CFG_BITS                      =  31; #

sub FM_CN_RATE_LIM_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CN_RATE_LIM_CFG_STRIDE                    =  1; #

sub FM_CN_CPID_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00680 + $FM_CM_BASE + ($word)); # 0x103680 CM_BASE
}


sub FM_CN_CPID_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CN_CPID_TABLE_WIDTH                       =  2; #
our $FM_CN_CPID_TABLE_BITS                        =  64; #

sub FM_CN_CPID_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}

our $FM_CN_CPID_TABLE_STRIDE                      =  2; #
our $FM_CN_GLOBAL_CFG_1                           =  (0x00692 + $FM_CM_BASE); # // 0x103692 CM_BASE

sub FM_CN_GLOBAL_CFG_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00692 + $FM_CM_BASE); # 0x103692 CM_BASE
}


sub FM_CN_GLOBAL_CFG_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x10010028;
}

our $FM_CN_GLOBAL_CFG_1_WIDTH                     =  1; #
our $FM_CN_GLOBAL_CFG_1_BITS                      =  32; #
our $FM_CN_GLOBAL_CFG_2                           =  (0x00693 + $FM_CM_BASE); # // 0x103693 CM_BASE

sub FM_CN_GLOBAL_CFG_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00693 + $FM_CM_BASE); # 0x103693 CM_BASE
}


sub FM_CN_GLOBAL_CFG_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x04080000;
}

our $FM_CN_GLOBAL_CFG_2_WIDTH                     =  1; #
our $FM_CN_GLOBAL_CFG_2_BITS                      =  27; #
our $FM_CN_FB_CFG                                 =  (0x00694 + $FM_CM_BASE); # // 0x103694 CM_BASE

sub FM_CN_FB_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00694 + $FM_CM_BASE); # 0x103694 CM_BASE
}


sub FM_CN_FB_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0008010C;
}

our $FM_CN_FB_CFG_WIDTH                           =  1; #
our $FM_CN_FB_CFG_BITS                            =  30; #
our $FM_CN_FORWARD_MASK                           =  (0x00695 + $FM_CM_BASE); # // 0x103695 CM_BASE

sub FM_CN_FORWARD_MASK {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00695 + $FM_CM_BASE); # 0x103695 CM_BASE
}


sub FM_CN_FORWARD_MASK_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_CN_FORWARD_MASK_WIDTH                     =  1; #
our $FM_CN_FORWARD_MASK_BITS                      =  25; #
our $FM_CN_RATE_ACTION_MASK                       =  (0x00696 + $FM_CM_BASE); # // 0x103696 CM_BASE

sub FM_CN_RATE_ACTION_MASK {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00696 + $FM_CM_BASE); # 0x103696 CM_BASE
}


sub FM_CN_RATE_ACTION_MASK_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_CN_RATE_ACTION_MASK_WIDTH                 =  1; #
our $FM_CN_RATE_ACTION_MASK_BITS                  =  25; #
our $FM_CN_BACKOFF_BYTETIME                       =  (0x00697 + $FM_CM_BASE); # // 0x103697 CM_BASE

sub FM_CN_BACKOFF_BYTETIME {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00697 + $FM_CM_BASE); # 0x103697 CM_BASE
}


sub FM_CN_BACKOFF_BYTETIME_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00001388;
}

our $FM_CN_BACKOFF_BYTETIME_WIDTH                 =  1; #
our $FM_CN_BACKOFF_BYTETIME_BITS                  =  23; #
our $FM_CN_FRAME_CFG_1                            =  (0x00698 + $FM_CM_BASE); # // 0x103698 CM_BASE

sub FM_CN_FRAME_CFG_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00698 + $FM_CM_BASE); # 0x103698 CM_BASE
}


sub FM_CN_FRAME_CFG_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_CN_FRAME_CFG_1_WIDTH                      =  1; #
our $FM_CN_FRAME_CFG_1_BITS                       =  32; #
our $FM_CN_FRAME_CFG_2                            =  (0x00699 + $FM_CM_BASE); # // 0x103699 CM_BASE

sub FM_CN_FRAME_CFG_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00699 + $FM_CM_BASE); # 0x103699 CM_BASE
}


sub FM_CN_FRAME_CFG_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_CN_FRAME_CFG_2_WIDTH                      =  1; #
our $FM_CN_FRAME_CFG_2_BITS                       =  32; #
our $FM_CM_CPU_PORT_STATUS                        =  (0x0069C + $FM_CM_BASE); # // 0x10369C CM_BASE

sub FM_CM_CPU_PORT_STATUS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0069C + $FM_CM_BASE); # 0x10369C CM_BASE
}


sub FM_CM_CPU_PORT_STATUS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_CM_CPU_PORT_STATUS_WIDTH                  =  1; #
our $FM_CM_CPU_PORT_STATUS_BITS                   =  8; #
our $FM_CN_VCN_DMAC_1                             =  (0x0069D + $FM_CM_BASE); # // 0x10369D CM_BASE

sub FM_CN_VCN_DMAC_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0069D + $FM_CM_BASE); # 0x10369D CM_BASE
}


sub FM_CN_VCN_DMAC_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00EFFEFF;
}

our $FM_CN_VCN_DMAC_1_WIDTH                       =  1; #
our $FM_CN_VCN_DMAC_1_BITS                        =  32; #

sub FM_TX_RATE_LIM_CFG {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00008 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00700 + $FM_CM_BASE); # 0x103700 CM_BASE
}


sub FM_TX_RATE_LIM_CFG_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0xDFFE0100;
}

our $FM_TX_RATE_LIM_CFG_WIDTH                     =  1; #
our $FM_TX_RATE_LIM_CFG_BITS                      =  32; #

sub FM_TX_RATE_LIM_CFG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}


sub FM_TX_RATE_LIM_CFG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_TX_RATE_LIM_CFG_STRIDE_0                  =  1; #
our $FM_TX_RATE_LIM_CFG_STRIDE_1                  =  8; #

sub FM_TX_RATE_LIM_USAGE {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00008 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00800 + $FM_CM_BASE); # 0x103800 CM_BASE
}


sub FM_TX_RATE_LIM_USAGE_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000200;
}

our $FM_TX_RATE_LIM_USAGE_WIDTH                   =  1; #
our $FM_TX_RATE_LIM_USAGE_BITS                    =  32; #

sub FM_TX_RATE_LIM_USAGE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}


sub FM_TX_RATE_LIM_USAGE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_TX_RATE_LIM_USAGE_STRIDE_0                =  1; #
our $FM_TX_RATE_LIM_USAGE_STRIDE_1                =  8; #

sub FM_RX_RATE_LIM_CFG {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00900 + $FM_CM_BASE); # 0x103900 CM_BASE
}


sub FM_RX_RATE_LIM_CFG_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0xDFFE0100;
}

our $FM_RX_RATE_LIM_CFG_WIDTH                     =  1; #
our $FM_RX_RATE_LIM_CFG_BITS                      =  32; #

sub FM_RX_RATE_LIM_CFG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}


sub FM_RX_RATE_LIM_CFG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_RX_RATE_LIM_CFG_STRIDE_0                  =  1; #
our $FM_RX_RATE_LIM_CFG_STRIDE_1                  =  2; #

sub FM_RX_RATE_LIM_USAGE {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00940 + $FM_CM_BASE); # 0x103940 CM_BASE
}


sub FM_RX_RATE_LIM_USAGE_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000200;
}

our $FM_RX_RATE_LIM_USAGE_WIDTH                   =  1; #
our $FM_RX_RATE_LIM_USAGE_BITS                    =  32; #

sub FM_RX_RATE_LIM_USAGE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}


sub FM_RX_RATE_LIM_USAGE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_RX_RATE_LIM_USAGE_STRIDE_0                =  1; #
our $FM_RX_RATE_LIM_USAGE_STRIDE_1                =  2; #

sub FM_RX_RATE_LIM_THRESHOLD {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00980 + $FM_CM_BASE); # 0x103980 CM_BASE
}


sub FM_RX_RATE_LIM_THRESHOLD_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_RX_RATE_LIM_THRESHOLD_WIDTH               =  1; #
our $FM_RX_RATE_LIM_THRESHOLD_BITS                =  32; #

sub FM_RX_RATE_LIM_THRESHOLD_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}


sub FM_RX_RATE_LIM_THRESHOLD_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_RX_RATE_LIM_THRESHOLD_STRIDE_0            =  1; #
our $FM_RX_RATE_LIM_THRESHOLD_STRIDE_1            =  2; #

sub FM_CM_PORT_LIST {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x009C0 + $FM_CM_BASE); # 0x1039C0 CM_BASE
}


sub FM_CM_PORT_LIST_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

  if($index == 0) {
    return 0x00000000;
  }
  if($index == 1) {
    return 0x00000001;
  }
  if($index == 2) {
    return 0x00000002;
  }
  if($index == 3) {
    return 0x00000003;
  }
  if($index == 4) {
    return 0x00000004;
  }
  if($index == 5) {
    return 0x00000005;
  }
  if($index == 6) {
    return 0x00000006;
  }
  if($index == 7) {
    return 0x00000007;
  }
  if($index == 8) {
    return 0x00000008;
  }
  if($index == 9) {
    return 0x00000009;
  }
  if($index == 10) {
    return 0x0000000A;
  }
  if($index == 11) {
    return 0x0000000B;
  }
  if($index == 12) {
    return 0x0000000C;
  }
  if($index == 13) {
    return 0x0000000D;
  }
  if($index == 14) {
    return 0x0000000E;
  }
  if($index == 15) {
    return 0x0000000F;
  }
  if($index == 16) {
    return 0x00000010;
  }
  if($index == 17) {
    return 0x00000011;
  }
  if($index == 18) {
    return 0x00000012;
  }
  if($index == 19) {
    return 0x00000013;
  }
  if($index == 20) {
    return 0x00000014;
  }
  if($index == 21) {
    return 0x00000015;
  }
  if($index == 22) {
    return 0x00000016;
  }
  if($index == 23) {
    return 0x00000017;
  }
  if($index == 24) {
    return 0x00000038;
  }
}

our $FM_CM_PORT_LIST_WIDTH                        =  1; #
our $FM_CM_PORT_LIST_BITS                         =  6; #

sub FM_CM_PORT_LIST_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_CM_PORT_LIST_STRIDE                       =  1; #

sub FM_SCHED_DRR_Q {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00008 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00A00 + $FM_CM_BASE); # 0x103A00 CM_BASE
}


sub FM_SCHED_DRR_Q_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000800;
}

our $FM_SCHED_DRR_Q_WIDTH                         =  1; #
our $FM_SCHED_DRR_Q_BITS                          =  24; #

sub FM_SCHED_DRR_Q_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}


sub FM_SCHED_DRR_Q_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_SCHED_DRR_Q_STRIDE_0                      =  1; #
our $FM_SCHED_DRR_Q_STRIDE_1                      =  8; #

sub FM_SCHED_SHAPING_GROUP_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00B00 + $FM_CM_BASE); # 0x103B00 CM_BASE
}


sub FM_SCHED_SHAPING_GROUP_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SCHED_SHAPING_GROUP_CFG_WIDTH             =  1; #
our $FM_SCHED_SHAPING_GROUP_CFG_BITS              =  32; #

sub FM_SCHED_SHAPING_GROUP_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_SCHED_SHAPING_GROUP_CFG_STRIDE            =  1; #
our $FM_SWITCH_PRI_TO_CLASS_1                     =  (0x00B20 + $FM_CM_BASE); # // 0x103B20 CM_BASE

sub FM_SWITCH_PRI_TO_CLASS_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00B20 + $FM_CM_BASE); # 0x103B20 CM_BASE
}


sub FM_SWITCH_PRI_TO_CLASS_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00FAC688;
}

our $FM_SWITCH_PRI_TO_CLASS_1_WIDTH               =  1; #
our $FM_SWITCH_PRI_TO_CLASS_1_BITS                =  24; #
our $FM_SWITCH_PRI_TO_CLASS_2                     =  (0x00B21 + $FM_CM_BASE); # // 0x103B21 CM_BASE

sub FM_SWITCH_PRI_TO_CLASS_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00B21 + $FM_CM_BASE); # 0x103B21 CM_BASE
}


sub FM_SWITCH_PRI_TO_CLASS_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00FAC688;
}

our $FM_SWITCH_PRI_TO_CLASS_2_WIDTH               =  1; #
our $FM_SWITCH_PRI_TO_CLASS_2_BITS                =  24; #
our $FM_CM_GLOBAL_CFG                             =  (0x00B22 + $FM_CM_BASE); # // 0x103B22 CM_BASE

sub FM_CM_GLOBAL_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00B22 + $FM_CM_BASE); # 0x103B22 CM_BASE
}


sub FM_CM_GLOBAL_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000014;
}

our $FM_CM_GLOBAL_CFG_WIDTH                       =  1; #
our $FM_CM_GLOBAL_CFG_BITS                        =  18; #
our $FM_TRAFFIC_CLASS_TO_SCHED_PRI                =  (0x00B23 + $FM_CM_BASE); # // 0x103B23 CM_BASE

sub FM_TRAFFIC_CLASS_TO_SCHED_PRI {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00B23 + $FM_CM_BASE); # 0x103B23 CM_BASE
}


sub FM_TRAFFIC_CLASS_TO_SCHED_PRI_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x76543210;
}

our $FM_TRAFFIC_CLASS_TO_SCHED_PRI_WIDTH          =  1; #
our $FM_TRAFFIC_CLASS_TO_SCHED_PRI_BITS           =  32; #

sub FM_CN_STATS_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00C00 + $FM_CM_BASE); # 0x103C00 CM_BASE
}


sub FM_CN_STATS_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CN_STATS_CFG_WIDTH                        =  1; #
our $FM_CN_STATS_CFG_BITS                         =  9; #

sub FM_CN_STATS_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}

our $FM_CN_STATS_CFG_STRIDE                       =  1; #

sub FM_CN_STATS {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00008 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00D00 + $FM_CM_BASE); # 0x103D00 CM_BASE
}


sub FM_CN_STATS_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_CN_STATS_WIDTH                            =  1; #
our $FM_CN_STATS_BITS                             =  32; #

sub FM_CN_STATS_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}


sub FM_CN_STATS_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}

our $FM_CN_STATS_STRIDE_0                         =  1; #
our $FM_CN_STATS_STRIDE_1                         =  8; #

sub FM_SCHED_FH_GROUP_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00F20 + $FM_CM_BASE); # 0x103F20 CM_BASE
}


sub FM_SCHED_FH_GROUP_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00FF00FF;
}

our $FM_SCHED_FH_GROUP_CFG_WIDTH                  =  1; #
our $FM_SCHED_FH_GROUP_CFG_BITS                   =  24; #

sub FM_SCHED_FH_GROUP_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_SCHED_FH_GROUP_CFG_STRIDE                 =  1; #

sub FM_QDM_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00F40 + $FM_CM_BASE); # 0x103F40 CM_BASE
}


sub FM_QDM_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000401;
}

our $FM_QDM_CFG_WIDTH                             =  1; #
our $FM_QDM_CFG_BITS                              =  15; #

sub FM_QDM_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}

our $FM_QDM_CFG_STRIDE                            =  1; #

sub FM_QDM_FRAME_CNT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00F42 + $FM_CM_BASE); # 0x103F42 CM_BASE
}


sub FM_QDM_FRAME_CNT_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_QDM_FRAME_CNT_WIDTH                       =  1; #
our $FM_QDM_FRAME_CNT_BITS                        =  16; #

sub FM_QDM_FRAME_CNT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}

our $FM_QDM_FRAME_CNT_STRIDE                      =  1; #

sub FM_QDM_INSTRUMENT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00F44 + $FM_CM_BASE); # 0x103F44 CM_BASE
}


sub FM_QDM_INSTRUMENT_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_QDM_INSTRUMENT_WIDTH                      =  1; #
our $FM_QDM_INSTRUMENT_BITS                       =  32; #

sub FM_QDM_INSTRUMENT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}

our $FM_QDM_INSTRUMENT_STRIDE                     =  1; #

sub FM_LAG_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00000 + $FM_LAG_BASE); # 0x104000 LAG_BASE
}


sub FM_LAG_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_LAG_CFG_WIDTH                             =  1; #
our $FM_LAG_CFG_BITS                              =  10; #

sub FM_LAG_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_LAG_CFG_STRIDE                            =  1; #

sub FM_CANONICAL_GLORT_CAM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00020 + $FM_LAG_BASE); # 0x104020 LAG_BASE
}


sub FM_CANONICAL_GLORT_CAM_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CANONICAL_GLORT_CAM_WIDTH                 =  1; #
our $FM_CANONICAL_GLORT_CAM_BITS                  =  23; #

sub FM_CANONICAL_GLORT_CAM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}

our $FM_CANONICAL_GLORT_CAM_STRIDE                =  1; #

sub FM_TRIGGER_CONDITION_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00100 + $FM_TRIG_BASE); # 0x106100 TRIG_BASE
}


sub FM_TRIGGER_CONDITION_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x0C4AAAAA;
}

our $FM_TRIGGER_CONDITION_CFG_WIDTH               =  1; #
our $FM_TRIGGER_CONDITION_CFG_BITS                =  28; #

sub FM_TRIGGER_CONDITION_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_TRIGGER_CONDITION_CFG_STRIDE              =  1; #

sub FM_TRIGGER_CONDITION_PARAM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00140 + $FM_TRIG_BASE); # 0x106140 TRIG_BASE
}


sub FM_TRIGGER_CONDITION_PARAM_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x1FC00000;
}

our $FM_TRIGGER_CONDITION_PARAM_WIDTH             =  1; #
our $FM_TRIGGER_CONDITION_PARAM_BITS              =  32; #

sub FM_TRIGGER_CONDITION_PARAM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_TRIGGER_CONDITION_PARAM_STRIDE            =  1; #

sub FM_TRIGGER_CONDITION_FFU {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00180 + $FM_TRIG_BASE); # 0x106180 TRIG_BASE
}


sub FM_TRIGGER_CONDITION_FFU_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_TRIGGER_CONDITION_FFU_WIDTH               =  1; #
our $FM_TRIGGER_CONDITION_FFU_BITS                =  16; #

sub FM_TRIGGER_CONDITION_FFU_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_TRIGGER_CONDITION_FFU_STRIDE              =  1; #

sub FM_TRIGGER_CONDITION_TYPE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x001C0 + $FM_TRIG_BASE); # 0x1061C0 TRIG_BASE
}


sub FM_TRIGGER_CONDITION_TYPE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_TRIGGER_CONDITION_TYPE_WIDTH              =  1; #
our $FM_TRIGGER_CONDITION_TYPE_BITS               =  32; #

sub FM_TRIGGER_CONDITION_TYPE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_TRIGGER_CONDITION_TYPE_STRIDE             =  1; #

sub FM_TRIGGER_CONDITION_GLORT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00200 + $FM_TRIG_BASE); # 0x106200 TRIG_BASE
}


sub FM_TRIGGER_CONDITION_GLORT_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_TRIGGER_CONDITION_GLORT_WIDTH             =  1; #
our $FM_TRIGGER_CONDITION_GLORT_BITS              =  32; #

sub FM_TRIGGER_CONDITION_GLORT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_TRIGGER_CONDITION_GLORT_STRIDE            =  1; #

sub FM_TRIGGER_CONDITION_RX {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00240 + $FM_TRIG_BASE); # 0x106240 TRIG_BASE
}


sub FM_TRIGGER_CONDITION_RX_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_TRIGGER_CONDITION_RX_WIDTH                =  1; #
our $FM_TRIGGER_CONDITION_RX_BITS                 =  25; #

sub FM_TRIGGER_CONDITION_RX_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_TRIGGER_CONDITION_RX_STRIDE               =  1; #

sub FM_TRIGGER_CONDITION_TX {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00280 + $FM_TRIG_BASE); # 0x106280 TRIG_BASE
}


sub FM_TRIGGER_CONDITION_TX_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x01FFFFFF;
}

our $FM_TRIGGER_CONDITION_TX_WIDTH                =  1; #
our $FM_TRIGGER_CONDITION_TX_BITS                 =  25; #

sub FM_TRIGGER_CONDITION_TX_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_TRIGGER_CONDITION_TX_STRIDE               =  1; #

sub FM_TRIGGER_CONDITION_AMASK_1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x002C0 + $FM_TRIG_BASE); # 0x1062C0 TRIG_BASE
}


sub FM_TRIGGER_CONDITION_AMASK_1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_TRIGGER_CONDITION_AMASK_1_WIDTH           =  1; #
our $FM_TRIGGER_CONDITION_AMASK_1_BITS            =  32; #

sub FM_TRIGGER_CONDITION_AMASK_1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_TRIGGER_CONDITION_AMASK_1_STRIDE          =  1; #

sub FM_TRIGGER_CONDITION_AMASK_2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00300 + $FM_TRIG_BASE); # 0x106300 TRIG_BASE
}


sub FM_TRIGGER_CONDITION_AMASK_2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000030;
}

our $FM_TRIGGER_CONDITION_AMASK_2_WIDTH           =  1; #
our $FM_TRIGGER_CONDITION_AMASK_2_BITS            =  13; #

sub FM_TRIGGER_CONDITION_AMASK_2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_TRIGGER_CONDITION_AMASK_2_STRIDE          =  1; #

sub FM_TRIGGER_ACTION_CFG_1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00340 + $FM_TRIG_BASE); # 0x106340 TRIG_BASE
}


sub FM_TRIGGER_ACTION_CFG_1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_TRIGGER_ACTION_CFG_1_WIDTH                =  1; #
our $FM_TRIGGER_ACTION_CFG_1_BITS                 =  12; #

sub FM_TRIGGER_ACTION_CFG_1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_TRIGGER_ACTION_CFG_1_STRIDE               =  1; #

sub FM_TRIGGER_ACTION_CFG_2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00380 + $FM_TRIG_BASE); # 0x106380 TRIG_BASE
}


sub FM_TRIGGER_ACTION_CFG_2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_TRIGGER_ACTION_CFG_2_WIDTH                =  1; #
our $FM_TRIGGER_ACTION_CFG_2_BITS                 =  22; #

sub FM_TRIGGER_ACTION_CFG_2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_TRIGGER_ACTION_CFG_2_STRIDE               =  1; #

sub FM_TRIGGER_ACTION_GLORT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x003C0 + $FM_TRIG_BASE); # 0x1063C0 TRIG_BASE
}


sub FM_TRIGGER_ACTION_GLORT_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_TRIGGER_ACTION_GLORT_WIDTH                =  1; #
our $FM_TRIGGER_ACTION_GLORT_BITS                 =  32; #

sub FM_TRIGGER_ACTION_GLORT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_TRIGGER_ACTION_GLORT_STRIDE               =  1; #

sub FM_TRIGGER_ACTION_DMASK {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00400 + $FM_TRIG_BASE); # 0x106400 TRIG_BASE
}


sub FM_TRIGGER_ACTION_DMASK_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x02000000;
}

our $FM_TRIGGER_ACTION_DMASK_WIDTH                =  1; #
our $FM_TRIGGER_ACTION_DMASK_BITS                 =  26; #

sub FM_TRIGGER_ACTION_DMASK_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_TRIGGER_ACTION_DMASK_STRIDE               =  1; #

sub FM_TRIGGER_ACTION_MIRROR {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00440 + $FM_TRIG_BASE); # 0x106440 TRIG_BASE
}


sub FM_TRIGGER_ACTION_MIRROR_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_TRIGGER_ACTION_MIRROR_WIDTH               =  1; #
our $FM_TRIGGER_ACTION_MIRROR_BITS                =  21; #

sub FM_TRIGGER_ACTION_MIRROR_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_TRIGGER_ACTION_MIRROR_STRIDE              =  1; #

sub FM_TRIGGER_ACTION_DROP {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00480 + $FM_TRIG_BASE); # 0x106480 TRIG_BASE
}


sub FM_TRIGGER_ACTION_DROP_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_TRIGGER_ACTION_DROP_WIDTH                 =  1; #
our $FM_TRIGGER_ACTION_DROP_BITS                  =  25; #

sub FM_TRIGGER_ACTION_DROP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_TRIGGER_ACTION_DROP_STRIDE                =  1; #

sub FM_TRIGGER_RATE_LIM_CFG_1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x004C0 + $FM_TRIG_BASE); # 0x1064C0 TRIG_BASE
}


sub FM_TRIGGER_RATE_LIM_CFG_1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_TRIGGER_RATE_LIM_CFG_1_WIDTH              =  1; #
our $FM_TRIGGER_RATE_LIM_CFG_1_BITS               =  26; #

sub FM_TRIGGER_RATE_LIM_CFG_1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}

our $FM_TRIGGER_RATE_LIM_CFG_1_STRIDE             =  1; #

sub FM_TRIGGER_RATE_LIM_CFG_2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x004D0 + $FM_TRIG_BASE); # 0x1064D0 TRIG_BASE
}


sub FM_TRIGGER_RATE_LIM_CFG_2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_TRIGGER_RATE_LIM_CFG_2_WIDTH              =  1; #
our $FM_TRIGGER_RATE_LIM_CFG_2_BITS               =  25; #

sub FM_TRIGGER_RATE_LIM_CFG_2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}

our $FM_TRIGGER_RATE_LIM_CFG_2_STRIDE             =  1; #

sub FM_TRIGGER_IP {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x004E0 + $FM_TRIG_BASE); # 0x1064E0 TRIG_BASE
}


sub FM_TRIGGER_IP_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

  if($index == 0) {
    return 0x00000000;
  }
  if($index == 1) {
    return 0x00000000;
  }
}

our $FM_TRIGGER_IP_WIDTH                          =  1; #
our $FM_TRIGGER_IP_BITS                           =  32; #

sub FM_TRIGGER_IP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}

our $FM_TRIGGER_IP_STRIDE                         =  1; #

sub FM_TRIGGER_IM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x004E4 + $FM_TRIG_BASE); # 0x1064E4 TRIG_BASE
}


sub FM_TRIGGER_IM_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

  if($index == 0) {
    return 0xFFFFFFFF;
  }
  if($index == 1) {
    return 0xFFFFFFFF;
  }
}

our $FM_TRIGGER_IM_WIDTH                          =  1; #
our $FM_TRIGGER_IM_BITS                           =  32; #

sub FM_TRIGGER_IM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}

our $FM_TRIGGER_IM_STRIDE                         =  1; #

sub FM_TRIGGER_RATE_LIM_USAGE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00580 + $FM_TRIG_BASE); # 0x106580 TRIG_BASE
}


sub FM_TRIGGER_RATE_LIM_USAGE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_TRIGGER_RATE_LIM_USAGE_WIDTH              =  1; #
our $FM_TRIGGER_RATE_LIM_USAGE_BITS               =  27; #

sub FM_TRIGGER_RATE_LIM_USAGE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}

our $FM_TRIGGER_RATE_LIM_USAGE_STRIDE             =  1; #

sub FM_GLORT_DEST_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00000 + $FM_GLORT_BASE + ($word)); # 0x108000 GLORT_BASE
}


sub FM_GLORT_DEST_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_GLORT_DEST_TABLE_WIDTH                    =  2; #
our $FM_GLORT_DEST_TABLE_BITS                     =  40; #

sub FM_GLORT_DEST_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}

our $FM_GLORT_DEST_TABLE_STRIDE                   =  2; #

sub FM_GLORT_CAM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x02000 + $FM_GLORT_BASE); # 0x10A000 GLORT_BASE
}


sub FM_GLORT_CAM_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_GLORT_CAM_WIDTH                           =  1; #
our $FM_GLORT_CAM_BITS                            =  32; #

sub FM_GLORT_CAM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 256;
}

our $FM_GLORT_CAM_STRIDE                          =  1; #

sub FM_GLORT_RAM {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x02200 + $FM_GLORT_BASE + ($word)); # 0x10A200 GLORT_BASE
}


sub FM_GLORT_RAM_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_GLORT_RAM_WIDTH                           =  2; #
our $FM_GLORT_RAM_BITS                            =  36; #

sub FM_GLORT_RAM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 256;
}

our $FM_GLORT_RAM_STRIDE                          =  2; #
our $FM_GLORT_RAM_TABLE_REPAIR                    =  (0x02500 + $FM_GLORT_BASE); # // 0x10A500 GLORT_BASE

sub FM_GLORT_RAM_TABLE_REPAIR {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x02500 + $FM_GLORT_BASE); # 0x10A500 GLORT_BASE
}


sub FM_GLORT_RAM_TABLE_REPAIR_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_GLORT_RAM_TABLE_REPAIR_WIDTH              =  1; #
our $FM_GLORT_RAM_TABLE_REPAIR_BITS               =  8; #
our $FM_GLORT_DEST_TABLE_REPAIR                   =  (0x02501 + $FM_GLORT_BASE); # // 0x10A501 GLORT_BASE

sub FM_GLORT_DEST_TABLE_REPAIR {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x02501 + $FM_GLORT_BASE); # 0x10A501 GLORT_BASE
}


sub FM_GLORT_DEST_TABLE_REPAIR_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_GLORT_DEST_TABLE_REPAIR_WIDTH             =  1; #
our $FM_GLORT_DEST_TABLE_REPAIR_BITS              =  12; #

sub FM_POLICER_TABLE {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00800 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x00000 + $FM_POLICER_BASE + ($word)); # 0x10C000 POLICER_BASE
}


sub FM_POLICER_TABLE_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_POLICER_TABLE_WIDTH                       =  4; #
our $FM_POLICER_TABLE_BITS                        =  128; #

sub FM_POLICER_TABLE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 512;
}


sub FM_POLICER_TABLE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}

our $FM_POLICER_TABLE_STRIDE_0                    =  4; #
our $FM_POLICER_TABLE_STRIDE_1                    =  2048; #

sub FM_POLICER_REPAIR {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x02000 + $FM_POLICER_BASE); # 0x10E000 POLICER_BASE
}


sub FM_POLICER_REPAIR_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_POLICER_REPAIR_WIDTH                      =  1; #
our $FM_POLICER_REPAIR_BITS                       =  4; #

sub FM_POLICER_REPAIR_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}


sub FM_POLICER_REPAIR_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}

our $FM_POLICER_REPAIR_STRIDE_0                   =  1; #
our $FM_POLICER_REPAIR_STRIDE_1                   =  2; #

sub FM_POLICER_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x02008 + $FM_POLICER_BASE); # 0x10E008 POLICER_BASE
}


sub FM_POLICER_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_POLICER_CFG_WIDTH                         =  1; #
our $FM_POLICER_CFG_BITS                          =  25; #

sub FM_POLICER_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}

our $FM_POLICER_CFG_STRIDE                        =  1; #
our $FM_POLICER_IP                                =  (0x0200C + $FM_POLICER_BASE); # // 0x10E00C POLICER_BASE

sub FM_POLICER_IP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0200C + $FM_POLICER_BASE); # 0x10E00C POLICER_BASE
}


sub FM_POLICER_IP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_POLICER_IP_WIDTH                          =  1; #
our $FM_POLICER_IP_BITS                           =  4; #
our $FM_POLICER_IM                                =  (0x0200D + $FM_POLICER_BASE); # // 0x10E00D POLICER_BASE

sub FM_POLICER_IM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0200D + $FM_POLICER_BASE); # 0x10E00D POLICER_BASE
}


sub FM_POLICER_IM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0000000F;
}

our $FM_POLICER_IM_WIDTH                          =  1; #
our $FM_POLICER_IM_BITS                           =  4; #
our $FM_POLICER_STATUS                            =  (0x0200E + $FM_POLICER_BASE); # // 0x10E00E POLICER_BASE

sub FM_POLICER_STATUS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0200E + $FM_POLICER_BASE); # 0x10E00E POLICER_BASE
}


sub FM_POLICER_STATUS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_POLICER_STATUS_WIDTH                      =  1; #
our $FM_POLICER_STATUS_BITS                       =  16; #
our $FM_POLICER_TS_DIV                            =  (0x0200F + $FM_POLICER_BASE); # // 0x10E00F POLICER_BASE

sub FM_POLICER_TS_DIV {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0200F + $FM_POLICER_BASE); # 0x10E00F POLICER_BASE
}


sub FM_POLICER_TS_DIV_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_POLICER_TS_DIV_WIDTH                      =  1; #
our $FM_POLICER_TS_DIV_BITS                       =  4; #

sub FM_POLICER_DSCP_DOWN_MAP {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x02100 + $FM_POLICER_BASE); # 0x10E100 POLICER_BASE
}


sub FM_POLICER_DSCP_DOWN_MAP_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_POLICER_DSCP_DOWN_MAP_WIDTH               =  1; #
our $FM_POLICER_DSCP_DOWN_MAP_BITS                =  6; #

sub FM_POLICER_DSCP_DOWN_MAP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_POLICER_DSCP_DOWN_MAP_STRIDE              =  1; #

sub FM_POLICER_SWPRI_DOWN_MAP {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x02140 + $FM_POLICER_BASE); # 0x10E140 POLICER_BASE
}


sub FM_POLICER_SWPRI_DOWN_MAP_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_POLICER_SWPRI_DOWN_MAP_WIDTH              =  1; #
our $FM_POLICER_SWPRI_DOWN_MAP_BITS               =  4; #

sub FM_POLICER_SWPRI_DOWN_MAP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}

our $FM_POLICER_SWPRI_DOWN_MAP_STRIDE             =  1; #

sub FM_ARP_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00000 + $FM_ARP_BASE + ($word)); # 0x120000 ARP_BASE
}


sub FM_ARP_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_ARP_TABLE_WIDTH                           =  3; #
our $FM_ARP_TABLE_BITS                            =  72; #

sub FM_ARP_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16384;
}

our $FM_ARP_TABLE_STRIDE                          =  4; #

sub FM_ARP_USED {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x10000 + $FM_ARP_BASE); # 0x130000 ARP_BASE
}


sub FM_ARP_USED_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_ARP_USED_WIDTH                            =  1; #
our $FM_ARP_USED_BITS                             =  32; #

sub FM_ARP_USED_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 512;
}

our $FM_ARP_USED_STRIDE                           =  1; #
our $FM_ARP_IP                                    =  (0x10201 + $FM_ARP_BASE); # // 0x130201 ARP_BASE

sub FM_ARP_IP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x10201 + $FM_ARP_BASE); # 0x130201 ARP_BASE
}


sub FM_ARP_IP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_ARP_IP_WIDTH                              =  1; #
our $FM_ARP_IP_BITS                               =  1; #
our $FM_ARP_IM                                    =  (0x10202 + $FM_ARP_BASE); # // 0x130202 ARP_BASE

sub FM_ARP_IM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x10202 + $FM_ARP_BASE); # 0x130202 ARP_BASE
}


sub FM_ARP_IM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000001;
}

our $FM_ARP_IM_WIDTH                              =  1; #
our $FM_ARP_IM_BITS                               =  1; #

sub FM_ARP_REDIRECT_SIP {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x10204 + $FM_ARP_BASE + ($word)); # 0x130204 ARP_BASE
}


sub FM_ARP_REDIRECT_SIP_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_ARP_REDIRECT_SIP_WIDTH                    =  4; #
our $FM_ARP_REDIRECT_SIP_BITS                     =  128; #

sub FM_ARP_REDIRECT_DIP {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x10208 + $FM_ARP_BASE + ($word)); # 0x130208 ARP_BASE
}


sub FM_ARP_REDIRECT_DIP_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_ARP_REDIRECT_DIP_WIDTH                    =  4; #
our $FM_ARP_REDIRECT_DIP_BITS                     =  128; #
our $FM_ARP_TABLE_REPAIR_0                        =  (0x10210 + $FM_ARP_BASE); # // 0x130210 ARP_BASE

sub FM_ARP_TABLE_REPAIR_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x10210 + $FM_ARP_BASE); # 0x130210 ARP_BASE
}


sub FM_ARP_TABLE_REPAIR_0_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_ARP_TABLE_REPAIR_0_WIDTH                  =  1; #
our $FM_ARP_TABLE_REPAIR_0_BITS                   =  12; #
our $FM_ARP_TABLE_REPAIR_1                        =  (0x10211 + $FM_ARP_BASE); # // 0x130211 ARP_BASE

sub FM_ARP_TABLE_REPAIR_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x10211 + $FM_ARP_BASE); # 0x130211 ARP_BASE
}


sub FM_ARP_TABLE_REPAIR_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_ARP_TABLE_REPAIR_1_WIDTH                  =  1; #
our $FM_ARP_TABLE_REPAIR_1_BITS                   =  12; #
our $FM_ARP_TABLE_REPAIR_2                        =  (0x10212 + $FM_ARP_BASE); # // 0x130212 ARP_BASE

sub FM_ARP_TABLE_REPAIR_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x10212 + $FM_ARP_BASE); # 0x130212 ARP_BASE
}


sub FM_ARP_TABLE_REPAIR_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_ARP_TABLE_REPAIR_2_WIDTH                  =  1; #
our $FM_ARP_TABLE_REPAIR_2_BITS                   =  12; #
our $FM_ARP_TABLE_REPAIR_3                        =  (0x10213 + $FM_ARP_BASE); # // 0x130213 ARP_BASE

sub FM_ARP_TABLE_REPAIR_3 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x10213 + $FM_ARP_BASE); # 0x130213 ARP_BASE
}


sub FM_ARP_TABLE_REPAIR_3_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_ARP_TABLE_REPAIR_3_WIDTH                  =  1; #
our $FM_ARP_TABLE_REPAIR_3_BITS                   =  12; #
our $FM_ARP_USED_REPAIR                           =  (0x10214 + $FM_ARP_BASE); # // 0x130214 ARP_BASE

sub FM_ARP_USED_REPAIR {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x10214 + $FM_ARP_BASE); # 0x130214 ARP_BASE
}


sub FM_ARP_USED_REPAIR_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_ARP_USED_REPAIR_WIDTH                     =  1; #
our $FM_ARP_USED_REPAIR_BITS                      =  9; #

sub FM_FFU_SLICE_TCAM {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x01000 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x00000 + $FM_FFU_BASE + ($word)); # 0x140000 FFU_BASE
}


sub FM_FFU_SLICE_TCAM_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000200;}
}

our $FM_FFU_SLICE_TCAM_WIDTH                      =  3; #
our $FM_FFU_SLICE_TCAM_BITS                       =  74; #

sub FM_FFU_SLICE_TCAM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 512;
}


sub FM_FFU_SLICE_TCAM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}

our $FM_FFU_SLICE_TCAM_STRIDE_0                   =  4; #
our $FM_FFU_SLICE_TCAM_STRIDE_1                   =  4096; #

sub FM_FFU_SLICE_SRAM {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x01000 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x00800 + $FM_FFU_BASE + ($word)); # 0x140800 FFU_BASE
}


sub FM_FFU_SLICE_SRAM_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_FFU_SLICE_SRAM_WIDTH                      =  2; #
our $FM_FFU_SLICE_SRAM_BITS                       =  40; #

sub FM_FFU_SLICE_SRAM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 512;
}


sub FM_FFU_SLICE_SRAM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}

our $FM_FFU_SLICE_SRAM_STRIDE_0                   =  2; #
our $FM_FFU_SLICE_SRAM_STRIDE_1                   =  4096; #

sub FM_FFU_SLICE_VALID {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x01000 * (($index) - 0) + 0x00C00 + $FM_FFU_BASE); # 0x140C00 FFU_BASE
}


sub FM_FFU_SLICE_VALID_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_FFU_SLICE_VALID_WIDTH                     =  1; #
our $FM_FFU_SLICE_VALID_BITS                      =  32; #

sub FM_FFU_SLICE_VALID_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}

our $FM_FFU_SLICE_VALID_STRIDE                    =  4096; #

sub FM_FFU_SLICE_CASE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x01000 * (($index) - 0) + 0x00C02 + $FM_FFU_BASE); # 0x140C02 FFU_BASE
}


sub FM_FFU_SLICE_CASE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_FFU_SLICE_CASE_WIDTH                      =  1; #
our $FM_FFU_SLICE_CASE_BITS                       =  32; #

sub FM_FFU_SLICE_CASE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}

our $FM_FFU_SLICE_CASE_STRIDE                     =  4096; #

sub FM_FFU_SLICE_CASCADE_ACTION {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x01000 * (($index) - 0) + 0x00C04 + $FM_FFU_BASE); # 0x140C04 FFU_BASE
}


sub FM_FFU_SLICE_CASCADE_ACTION_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_FFU_SLICE_CASCADE_ACTION_WIDTH            =  1; #
our $FM_FFU_SLICE_CASCADE_ACTION_BITS             =  32; #

sub FM_FFU_SLICE_CASCADE_ACTION_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}

our $FM_FFU_SLICE_CASCADE_ACTION_STRIDE           =  4096; #

sub FM_FFU_SLICE_CASE_CFG {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x01000 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00C08 + $FM_FFU_BASE); # 0x140C08 FFU_BASE
}


sub FM_FFU_SLICE_CASE_CFG_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_FFU_SLICE_CASE_CFG_WIDTH                  =  1; #
our $FM_FFU_SLICE_CASE_CFG_BITS                   =  32; #

sub FM_FFU_SLICE_CASE_CFG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}


sub FM_FFU_SLICE_CASE_CFG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}

our $FM_FFU_SLICE_CASE_CFG_STRIDE_0               =  1; #
our $FM_FFU_SLICE_CASE_CFG_STRIDE_1               =  4096; #

sub FM_FFU_MASTER_VALID {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x3A000 + $FM_FFU_BASE + ($word)); # 0x17A000 FFU_BASE
}


sub FM_FFU_MASTER_VALID_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0xFFFFFFFF;}
    if($word == 1) {return 0xFFFFFFFF;}
}

our $FM_FFU_MASTER_VALID_WIDTH                    =  2; #
our $FM_FFU_MASTER_VALID_BITS                     =  64; #

sub FM_FFU_MAP_SRC {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x3B000 + $FM_FFU_BASE); # 0x17B000 FFU_BASE
}


sub FM_FFU_MAP_SRC_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_FFU_MAP_SRC_WIDTH                         =  1; #
our $FM_FFU_MAP_SRC_BITS                          =  5; #

sub FM_FFU_MAP_SRC_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_FFU_MAP_SRC_STRIDE                        =  1; #

sub FM_FFU_MAP_MAC {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x3B100 + $FM_FFU_BASE + ($word)); # 0x17B100 FFU_BASE
}


sub FM_FFU_MAP_MAC_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_FFU_MAP_MAC_WIDTH                         =  2; #
our $FM_FFU_MAP_MAC_BITS                          =  63; #

sub FM_FFU_MAP_MAC_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}

our $FM_FFU_MAP_MAC_STRIDE                        =  2; #
our $FM_FFU_MAP_VLAN_REPAIR                       =  (0x3B180 + $FM_FFU_BASE); # // 0x17B180 FFU_BASE

sub FM_FFU_MAP_VLAN_REPAIR {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x3B180 + $FM_FFU_BASE); # 0x17B180 FFU_BASE
}


sub FM_FFU_MAP_VLAN_REPAIR_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FFU_MAP_VLAN_REPAIR_WIDTH                 =  1; #
our $FM_FFU_MAP_VLAN_REPAIR_BITS                  =  12; #

sub FM_FFU_MAP_TYPE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x3B200 + $FM_FFU_BASE); # 0x17B200 FFU_BASE
}


sub FM_FFU_MAP_TYPE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_FFU_MAP_TYPE_WIDTH                        =  1; #
our $FM_FFU_MAP_TYPE_BITS                         =  20; #

sub FM_FFU_MAP_TYPE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}

our $FM_FFU_MAP_TYPE_STRIDE                       =  1; #

sub FM_FFU_MAP_LENGTH {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x3B300 + $FM_FFU_BASE); # 0x17B300 FFU_BASE
}


sub FM_FFU_MAP_LENGTH_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_FFU_MAP_LENGTH_WIDTH                      =  1; #
our $FM_FFU_MAP_LENGTH_BITS                       =  20; #

sub FM_FFU_MAP_LENGTH_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}

our $FM_FFU_MAP_LENGTH_STRIDE                     =  1; #

sub FM_FFU_MAP_IP_LO {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x3B400 + $FM_FFU_BASE + ($word)); # 0x17B400 FFU_BASE
}


sub FM_FFU_MAP_IP_LO_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_FFU_MAP_IP_LO_WIDTH                       =  2; #
our $FM_FFU_MAP_IP_LO_BITS                        =  64; #

sub FM_FFU_MAP_IP_LO_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}

our $FM_FFU_MAP_IP_LO_STRIDE                      =  2; #

sub FM_FFU_MAP_IP_HI {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x3B420 + $FM_FFU_BASE + ($word)); # 0x17B420 FFU_BASE
}


sub FM_FFU_MAP_IP_HI_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_FFU_MAP_IP_HI_WIDTH                       =  2; #
our $FM_FFU_MAP_IP_HI_BITS                        =  64; #

sub FM_FFU_MAP_IP_HI_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}

our $FM_FFU_MAP_IP_HI_STRIDE                      =  2; #

sub FM_FFU_MAP_IP_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x3B440 + $FM_FFU_BASE); # 0x17B440 FFU_BASE
}


sub FM_FFU_MAP_IP_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_FFU_MAP_IP_CFG_WIDTH                      =  1; #
our $FM_FFU_MAP_IP_CFG_BITS                       =  14; #

sub FM_FFU_MAP_IP_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}

our $FM_FFU_MAP_IP_CFG_STRIDE                     =  1; #

sub FM_FFU_MAP_PROT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x3B500 + $FM_FFU_BASE); # 0x17B500 FFU_BASE
}


sub FM_FFU_MAP_PROT_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_FFU_MAP_PROT_WIDTH                        =  1; #
our $FM_FFU_MAP_PROT_BITS                         =  11; #

sub FM_FFU_MAP_PROT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}

our $FM_FFU_MAP_PROT_STRIDE                       =  1; #

sub FM_FFU_MAP_L4_SRC {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x3B600 + $FM_FFU_BASE + ($word)); # 0x17B600 FFU_BASE
}


sub FM_FFU_MAP_L4_SRC_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_FFU_MAP_L4_SRC_WIDTH                      =  2; #
our $FM_FFU_MAP_L4_SRC_BITS                       =  48; #

sub FM_FFU_MAP_L4_SRC_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_FFU_MAP_L4_SRC_STRIDE                     =  2; #

sub FM_FFU_MAP_L4_DST {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x3B700 + $FM_FFU_BASE + ($word)); # 0x17B700 FFU_BASE
}


sub FM_FFU_MAP_L4_DST_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_FFU_MAP_L4_DST_WIDTH                      =  2; #
our $FM_FFU_MAP_L4_DST_BITS                       =  48; #

sub FM_FFU_MAP_L4_DST_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_FFU_MAP_L4_DST_STRIDE                     =  2; #
our $FM_FFU_INIT_SLICE                            =  (0x3B800 + $FM_FFU_BASE); # // 0x17B800 FFU_BASE

sub FM_FFU_INIT_SLICE {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x3B800 + $FM_FFU_BASE); # 0x17B800 FFU_BASE
}


sub FM_FFU_INIT_SLICE_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0003FFE0;
}

our $FM_FFU_INIT_SLICE_WIDTH                      =  1; #
our $FM_FFU_INIT_SLICE_BITS                       =  18; #

sub FM_FFU_MAP_VLAN {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x3C000 + $FM_FFU_BASE); # 0x17C000 FFU_BASE
}


sub FM_FFU_MAP_VLAN_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_FFU_MAP_VLAN_WIDTH                        =  1; #
our $FM_FFU_MAP_VLAN_BITS                         =  14; #

sub FM_FFU_MAP_VLAN_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}

our $FM_FFU_MAP_VLAN_STRIDE                       =  1; #

sub FM_FFU_EGRESS_CHUNK_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x3D000 + $FM_FFU_BASE); # 0x17D000 FFU_BASE
}


sub FM_FFU_EGRESS_CHUNK_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_FFU_EGRESS_CHUNK_CFG_WIDTH                =  1; #
our $FM_FFU_EGRESS_CHUNK_CFG_BITS                 =  32; #

sub FM_FFU_EGRESS_CHUNK_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}

our $FM_FFU_EGRESS_CHUNK_CFG_STRIDE               =  1; #

sub FM_FFU_EGRESS_CHUNK_VALID {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x3D100 + $FM_FFU_BASE); # 0x17D100 FFU_BASE
}


sub FM_FFU_EGRESS_CHUNK_VALID_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0xFFFFFFFF;
}

our $FM_FFU_EGRESS_CHUNK_VALID_WIDTH              =  1; #
our $FM_FFU_EGRESS_CHUNK_VALID_BITS               =  32; #

sub FM_FFU_EGRESS_CHUNK_VALID_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}

our $FM_FFU_EGRESS_CHUNK_VALID_STRIDE             =  1; #

sub FM_FFU_EGRESS_ACTIONS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x3D200 + $FM_FFU_BASE); # 0x17D200 FFU_BASE
}


sub FM_FFU_EGRESS_ACTIONS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_FFU_EGRESS_ACTIONS_WIDTH                  =  1; #
our $FM_FFU_EGRESS_ACTIONS_BITS                   =  3; #

sub FM_FFU_EGRESS_ACTIONS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 512;
}

our $FM_FFU_EGRESS_ACTIONS_STRIDE                 =  1; #

sub FM_MA_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00000 + $FM_L2LOOKUP_BASE + ($word)); # 0x180000 L2LOOKUP_BASE
}


sub FM_MA_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_MA_TABLE_WIDTH                            =  3; #
our $FM_MA_TABLE_BITS                             =  96; #

sub FM_MA_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16384;
}

our $FM_MA_TABLE_STRIDE                           =  4; #

sub FM_INGRESS_VID_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x10000 + $FM_L2LOOKUP_BASE + ($word)); # 0x190000 L2LOOKUP_BASE
}


sub FM_INGRESS_VID_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_INGRESS_VID_TABLE_WIDTH                   =  3; #
our $FM_INGRESS_VID_TABLE_BITS                    =  78; #

sub FM_INGRESS_VID_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}

our $FM_INGRESS_VID_TABLE_STRIDE                  =  4; #

sub FM_EGRESS_VID_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x14000 + $FM_L2LOOKUP_BASE + ($word)); # 0x194000 L2LOOKUP_BASE
}


sub FM_EGRESS_VID_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_EGRESS_VID_TABLE_WIDTH                    =  2; #
our $FM_EGRESS_VID_TABLE_BITS                     =  50; #

sub FM_EGRESS_VID_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}

our $FM_EGRESS_VID_TABLE_STRIDE                   =  2; #

sub FM_INGRESS_FID_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x16000 + $FM_L2LOOKUP_BASE + ($word)); # 0x196000 L2LOOKUP_BASE
}


sub FM_INGRESS_FID_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_INGRESS_FID_TABLE_WIDTH                   =  2; #
our $FM_INGRESS_FID_TABLE_BITS                    =  50; #

sub FM_INGRESS_FID_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}

our $FM_INGRESS_FID_TABLE_STRIDE                  =  2; #

sub FM_EGRESS_FID_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x18000 + $FM_L2LOOKUP_BASE); # 0x198000 L2LOOKUP_BASE
}


sub FM_EGRESS_FID_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_EGRESS_FID_TABLE_WIDTH                    =  1; #
our $FM_EGRESS_FID_TABLE_BITS                     =  25; #

sub FM_EGRESS_FID_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}

our $FM_EGRESS_FID_TABLE_STRIDE                   =  1; #

sub FM_MA_TCN_FIFO {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x1E000 + $FM_L2LOOKUP_BASE + ($word)); # 0x19E000 L2LOOKUP_BASE
}


sub FM_MA_TCN_FIFO_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_MA_TCN_FIFO_WIDTH                         =  4; #
our $FM_MA_TCN_FIFO_BITS                          =  115; #

sub FM_MA_TCN_FIFO_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 512;
}

our $FM_MA_TCN_FIFO_STRIDE                        =  4; #
our $FM_MA_TCN_PTR                                =  (0x1E800 + $FM_L2LOOKUP_BASE); # // 0x19E800 L2LOOKUP_BASE

sub FM_MA_TCN_PTR {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x1E800 + $FM_L2LOOKUP_BASE); # 0x19E800 L2LOOKUP_BASE
}


sub FM_MA_TCN_PTR_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MA_TCN_PTR_WIDTH                          =  1; #
our $FM_MA_TCN_PTR_BITS                           =  18; #
our $FM_MA_TCN_IP                                 =  (0x1E801 + $FM_L2LOOKUP_BASE); # // 0x19E801 L2LOOKUP_BASE

sub FM_MA_TCN_IP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x1E801 + $FM_L2LOOKUP_BASE); # 0x19E801 L2LOOKUP_BASE
}


sub FM_MA_TCN_IP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MA_TCN_IP_WIDTH                           =  1; #
our $FM_MA_TCN_IP_BITS                            =  6; #
our $FM_MA_TCN_IM                                 =  (0x1E802 + $FM_L2LOOKUP_BASE); # // 0x19E802 L2LOOKUP_BASE

sub FM_MA_TCN_IM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x1E802 + $FM_L2LOOKUP_BASE); # 0x19E802 L2LOOKUP_BASE
}


sub FM_MA_TCN_IM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0000003F;
}

our $FM_MA_TCN_IM_WIDTH                           =  1; #
our $FM_MA_TCN_IM_BITS                            =  6; #
our $FM_MA_TABLE_STATUS_3                         =  (0x1E803 + $FM_L2LOOKUP_BASE); # // 0x19E803 L2LOOKUP_BASE

sub FM_MA_TABLE_STATUS_3 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x1E803 + $FM_L2LOOKUP_BASE); # 0x19E803 L2LOOKUP_BASE
}


sub FM_MA_TABLE_STATUS_3_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MA_TABLE_STATUS_3_WIDTH                   =  1; #
our $FM_MA_TABLE_STATUS_3_BITS                    =  32; #
our $FM_MA_TABLE_CFG_1                            =  (0x1E804 + $FM_L2LOOKUP_BASE); # // 0x19E804 L2LOOKUP_BASE

sub FM_MA_TABLE_CFG_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x1E804 + $FM_L2LOOKUP_BASE); # 0x19E804 L2LOOKUP_BASE
}


sub FM_MA_TABLE_CFG_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x03C10040;
}

our $FM_MA_TABLE_CFG_1_WIDTH                      =  1; #
our $FM_MA_TABLE_CFG_1_BITS                       =  28; #
our $FM_MA_TABLE_CFG_2                            =  (0x1E805 + $FM_L2LOOKUP_BASE); # // 0x19E805 L2LOOKUP_BASE

sub FM_MA_TABLE_CFG_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x1E805 + $FM_L2LOOKUP_BASE); # 0x19E805 L2LOOKUP_BASE
}


sub FM_MA_TABLE_CFG_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MA_TABLE_CFG_2_WIDTH                      =  1; #
our $FM_MA_TABLE_CFG_2_BITS                       =  32; #
our $FM_MA_TABLE_CFG_3                            =  (0x1E806 + $FM_L2LOOKUP_BASE); # // 0x19E806 L2LOOKUP_BASE

sub FM_MA_TABLE_CFG_3 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x1E806 + $FM_L2LOOKUP_BASE); # 0x19E806 L2LOOKUP_BASE
}


sub FM_MA_TABLE_CFG_3_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MA_TABLE_CFG_3_WIDTH                      =  1; #
our $FM_MA_TABLE_CFG_3_BITS                       =  8; #
our $FM_MA_TCN_CFG_1                              =  (0x1E807 + $FM_L2LOOKUP_BASE); # // 0x19E807 L2LOOKUP_BASE

sub FM_MA_TCN_CFG_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x1E807 + $FM_L2LOOKUP_BASE); # 0x19E807 L2LOOKUP_BASE
}


sub FM_MA_TCN_CFG_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x07FFFFFF;
}

our $FM_MA_TCN_CFG_1_WIDTH                        =  1; #
our $FM_MA_TCN_CFG_1_BITS                         =  28; #
our $FM_MA_TCN_CFG_2                              =  (0x1E808 + $FM_L2LOOKUP_BASE); # // 0x19E808 L2LOOKUP_BASE

sub FM_MA_TCN_CFG_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x1E808 + $FM_L2LOOKUP_BASE); # 0x19E808 L2LOOKUP_BASE
}


sub FM_MA_TCN_CFG_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0007FFFF;
}

our $FM_MA_TCN_CFG_2_WIDTH                        =  1; #
our $FM_MA_TCN_CFG_2_BITS                         =  19; #
our $FM_MA_PURGE                                  =  (0x1E809 + $FM_L2LOOKUP_BASE); # // 0x19E809 L2LOOKUP_BASE

sub FM_MA_PURGE {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x1E809 + $FM_L2LOOKUP_BASE); # 0x19E809 L2LOOKUP_BASE
}


sub FM_MA_PURGE_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MA_PURGE_WIDTH                            =  1; #
our $FM_MA_PURGE_BITS                             =  31; #

sub FM_MTU_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x1E810 + $FM_L2LOOKUP_BASE); # 0x19E810 L2LOOKUP_BASE
}


sub FM_MTU_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MTU_TABLE_WIDTH                           =  1; #
our $FM_MTU_TABLE_BITS                            =  14; #

sub FM_MTU_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}

our $FM_MTU_TABLE_STRIDE                          =  1; #
our $FM_INGRESS_VID_TABLE_REPAIR                  =  (0x1E820 + $FM_L2LOOKUP_BASE); # // 0x19E820 L2LOOKUP_BASE

sub FM_INGRESS_VID_TABLE_REPAIR {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x1E820 + $FM_L2LOOKUP_BASE); # 0x19E820 L2LOOKUP_BASE
}


sub FM_INGRESS_VID_TABLE_REPAIR_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_INGRESS_VID_TABLE_REPAIR_WIDTH            =  1; #
our $FM_INGRESS_VID_TABLE_REPAIR_BITS             =  12; #
our $FM_EGRESS_VID_TABLE_REPAIR                   =  (0x1E821 + $FM_L2LOOKUP_BASE); # // 0x19E821 L2LOOKUP_BASE

sub FM_EGRESS_VID_TABLE_REPAIR {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x1E821 + $FM_L2LOOKUP_BASE); # 0x19E821 L2LOOKUP_BASE
}


sub FM_EGRESS_VID_TABLE_REPAIR_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_EGRESS_VID_TABLE_REPAIR_WIDTH             =  1; #
our $FM_EGRESS_VID_TABLE_REPAIR_BITS              =  12; #
our $FM_INGRESS_FID_TABLE_REPAIR                  =  (0x1E822 + $FM_L2LOOKUP_BASE); # // 0x19E822 L2LOOKUP_BASE

sub FM_INGRESS_FID_TABLE_REPAIR {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x1E822 + $FM_L2LOOKUP_BASE); # 0x19E822 L2LOOKUP_BASE
}


sub FM_INGRESS_FID_TABLE_REPAIR_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_INGRESS_FID_TABLE_REPAIR_WIDTH            =  1; #
our $FM_INGRESS_FID_TABLE_REPAIR_BITS             =  12; #
our $FM_EGRESS_FID_TABLE_REPAIR                   =  (0x1E823 + $FM_L2LOOKUP_BASE); # // 0x19E823 L2LOOKUP_BASE

sub FM_EGRESS_FID_TABLE_REPAIR {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x1E823 + $FM_L2LOOKUP_BASE); # 0x19E823 L2LOOKUP_BASE
}


sub FM_EGRESS_FID_TABLE_REPAIR_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_EGRESS_FID_TABLE_REPAIR_WIDTH             =  1; #
our $FM_EGRESS_FID_TABLE_REPAIR_BITS              =  12; #

sub FM_TCN_FIFO_REPAIR {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x1E824 + $FM_L2LOOKUP_BASE); # 0x19E824 L2LOOKUP_BASE
}


sub FM_TCN_FIFO_REPAIR_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_TCN_FIFO_REPAIR_WIDTH                     =  1; #
our $FM_TCN_FIFO_REPAIR_BITS                      =  9; #

sub FM_TCN_FIFO_REPAIR_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}

our $FM_TCN_FIFO_REPAIR_STRIDE                    =  1; #

sub FM_STAT_VlanUcstPkts {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00000 + $FM_STATS_VLAN_PKT_BASE + ($word)); # 0x1A0100 STATS_VLAN_PKT_BASE
}


sub FM_STAT_VlanUcstPkts_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_VlanUcstPkts_WIDTH                   =  2; #
our $FM_STAT_VlanUcstPkts_BITS                    =  64; #

sub FM_STAT_VlanUcstPkts_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_STAT_VlanUcstPkts_STRIDE                  =  2; #

sub FM_STAT_VlanXcstPkts {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00080 + $FM_STATS_VLAN_PKT_BASE + ($word)); # 0x1A0180 STATS_VLAN_PKT_BASE
}


sub FM_STAT_VlanXcstPkts_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_VlanXcstPkts_WIDTH                   =  2; #
our $FM_STAT_VlanXcstPkts_BITS                    =  64; #

sub FM_STAT_VlanXcstPkts_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_STAT_VlanXcstPkts_STRIDE                  =  2; #

sub FM_STAT_VlanUcstOctets {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00000 + $FM_STATS_VLAN_OCT_BASE + ($word)); # 0x1A0200 STATS_VLAN_OCT_BASE
}


sub FM_STAT_VlanUcstOctets_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_VlanUcstOctets_WIDTH                 =  2; #
our $FM_STAT_VlanUcstOctets_BITS                  =  64; #

sub FM_STAT_VlanUcstOctets_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_STAT_VlanUcstOctets_STRIDE                =  2; #

sub FM_STAT_VlanXcstOctets {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00080 + $FM_STATS_VLAN_OCT_BASE + ($word)); # 0x1A0280 STATS_VLAN_OCT_BASE
}


sub FM_STAT_VlanXcstOctets_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_VlanXcstOctets_WIDTH                 =  2; #
our $FM_STAT_VlanXcstOctets_BITS                  =  64; #

sub FM_STAT_VlanXcstOctets_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_STAT_VlanXcstOctets_STRIDE                =  2; #

sub FM_STAT_Trigger {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00000 + $FM_STATS_TRIG_BASE + ($word)); # 0x1A0300 STATS_TRIG_BASE
}


sub FM_STAT_Trigger_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_Trigger_WIDTH                        =  2; #
our $FM_STAT_Trigger_BITS                         =  64; #

sub FM_STAT_Trigger_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}

our $FM_STAT_Trigger_STRIDE                       =  2; #

sub FM_STAT_EgressACLPkts {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00000 + $FM_STATS_EGRESS_ACL_BASE + ($word)); # 0x1A0400 STATS_EGRESS_ACL_BASE
}


sub FM_STAT_EgressACLPkts_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_EgressACLPkts_WIDTH                  =  2; #
our $FM_STAT_EgressACLPkts_BITS                   =  64; #

sub FM_STAT_EgressACLPkts_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_EgressACLPkts_STRIDE                 =  2; #

sub FM_STAT_EgressACLOctets {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00080 + $FM_STATS_EGRESS_ACL_BASE + ($word)); # 0x1A0480 STATS_EGRESS_ACL_BASE
}


sub FM_STAT_EgressACLOctets_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return 0;
}

our $FM_STAT_EgressACLOctets_WIDTH                =  2; #
our $FM_STAT_EgressACLOctets_BITS                 =  64; #

sub FM_STAT_EgressACLOctets_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STAT_EgressACLOctets_STRIDE               =  2; #

sub FM_STATS_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00002 * (($index) - 0) + 0x00000 + $FM_STATS_CTRL_BASE); # 0x1A0500 STATS_CTRL_BASE
}


sub FM_STATS_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x000079FE;
}

our $FM_STATS_CFG_WIDTH                           =  1; #
our $FM_STATS_CFG_BITS                            =  16; #

sub FM_STATS_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STATS_CFG_STRIDE                          =  2; #

sub FM_STATS_DROP_COUNT_RX {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00002 * (($index) - 0) + 0x00040 + $FM_STATS_CTRL_BASE); # 0x1A0540 STATS_CTRL_BASE
}


sub FM_STATS_DROP_COUNT_RX_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_STATS_DROP_COUNT_RX_WIDTH                 =  1; #
our $FM_STATS_DROP_COUNT_RX_BITS                  =  32; #

sub FM_STATS_DROP_COUNT_RX_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STATS_DROP_COUNT_RX_STRIDE                =  2; #

sub FM_STATS_DROP_COUNT_TX {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00002 * (($index) - 0) + 0x00080 + $FM_STATS_CTRL_BASE); # 0x1A0580 STATS_CTRL_BASE
}


sub FM_STATS_DROP_COUNT_TX_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_STATS_DROP_COUNT_TX_WIDTH                 =  1; #
our $FM_STATS_DROP_COUNT_TX_BITS                  =  32; #

sub FM_STATS_DROP_COUNT_TX_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}

our $FM_STATS_DROP_COUNT_TX_STRIDE                =  2; #




our $FM_LEARNING_SERVE_WM_l_LEARNING_SERVE_WM     =  0; #
our $FM_LEARNING_SERVE_WM_h_LEARNING_SERVE_WM     =  30; #

our $FM_TX_PRI_MAP_1_l_PRI3                       =  12; #
our $FM_TX_PRI_MAP_1_h_PRI3                       =  15; #
our $FM_TX_PRI_MAP_1_l_PRI7                       =  28; #
our $FM_TX_PRI_MAP_1_h_PRI7                       =  31; #
our $FM_TX_PRI_MAP_1_l_PRI5                       =  20; #
our $FM_TX_PRI_MAP_1_h_PRI5                       =  23; #
our $FM_TX_PRI_MAP_1_l_PRI1                       =  4; #
our $FM_TX_PRI_MAP_1_h_PRI1                       =  7; #
our $FM_TX_PRI_MAP_1_l_PRI4                       =  16; #
our $FM_TX_PRI_MAP_1_h_PRI4                       =  19; #
our $FM_TX_PRI_MAP_1_l_PRI6                       =  24; #
our $FM_TX_PRI_MAP_1_h_PRI6                       =  27; #
our $FM_TX_PRI_MAP_1_l_PRI0                       =  0; #
our $FM_TX_PRI_MAP_1_h_PRI0                       =  3; #
our $FM_TX_PRI_MAP_1_l_PRI2                       =  8; #
our $FM_TX_PRI_MAP_1_h_PRI2                       =  11; #

our $FM_AN_RX_LAST_PAGE_l_LastPage                =  0; #
our $FM_AN_RX_LAST_PAGE_h_LastPage                =  47; #

our $FM_EGRESS_FRAME_COUNTER_l_EGRESS_FRAME_COUNTER  =  0; #
our $FM_EGRESS_FRAME_COUNTER_h_EGRESS_FRAME_COUNTER  =  31; #

our $FM_TRIGGER_TX_l_destinationPortMask          =  1; #
our $FM_TRIGGER_TX_h_destinationPortMask          =  24; #


our $FM_FUSEBOX_REPAIR_SYNC_VALUE_l_all           =  0; #
our $FM_FUSEBOX_REPAIR_SYNC_VALUE_h_all           =  31; #

our $FM_EBI_l_bootState                           =  0; #
our $FM_EBI_h_bootState                           =  31; #

our $FM_CPU_GLORT_l_trapGlort                     =  0; #
our $FM_CPU_GLORT_h_trapGlort                     =  15; #


our $FM_L4_CTL_b_CaptureTCPFlags                  =  16; #
our $FM_L4_CTL_l_OtherL3Length                    =  18; #
our $FM_L4_CTL_h_OtherL3Length                    =  23; #
our $FM_L4_CTL_b_CaptureTCP_HL_RSV_OPTIONS_Fields  =  16; #
our $FM_L4_CTL_l_CustomProtocol1                  =  0; #
our $FM_L4_CTL_h_CustomProtocol1                  =  7; #
our $FM_L4_CTL_l_OtherProtocolLength              =  24; #
our $FM_L4_CTL_h_OtherProtocolLength              =  29; #
our $FM_L4_CTL_l_CustomProtocol2                  =  8; #
our $FM_L4_CTL_h_CustomProtocol2                  =  15; #

our $FM_FRAME_SERVE_WM_l_FRAME_SERVE_WM           =  0; #
our $FM_FRAME_SERVE_WM_h_FRAME_SERVE_WM           =  30; #

our $FM_CM_TX_SHARED_SMP_USAGE_l_count            =  0; #
our $FM_CM_TX_SHARED_SMP_USAGE_h_count            =  13; #

our $FM_PACING_PRI_WM_l_PacingWatermark           =  0; #
our $FM_PACING_PRI_WM_h_PacingWatermark           =  24; #

our $FM_MGR_IM_l_reserved                         =  0; #
our $FM_MGR_IM_h_reserved                         =  31; #

our $FM_TRIGGER_RX_l_sourcePortMask               =  1; #
our $FM_TRIGGER_RX_h_sourcePortMask               =  24; #

our $FM_CPU_MASK_l_DestMask                       =  0; #
our $FM_CPU_MASK_h_DestMask                       =  24; #

our $FM_CM_TX_SHARED_HOG_WM_l_watermark           =  0; #
our $FM_CM_TX_SHARED_HOG_WM_h_watermark           =  12; #

our $FM_AN_RX_CW_Lo_l_message                     =  0; #
our $FM_AN_RX_CW_Lo_h_message                     =  47; #

our $FM_MAC_BORDER_CFG_l_ReadyToks                =  0; #
our $FM_MAC_BORDER_CFG_h_ReadyToks                =  2; #

our $FM_FUSEBOX_REPAIR_SYNC_ADDR_l_all            =  0; #
our $FM_FUSEBOX_REPAIR_SYNC_ADDR_h_all            =  23; #

our $FM_PACING_STAT_l_IFGS                        =  0; #
our $FM_PACING_STAT_h_IFGS                        =  24; #

our $FM_RX_PRI_MAP_l_pri3                         =  12; #
our $FM_RX_PRI_MAP_h_pri3                         =  15; #
our $FM_RX_PRI_MAP_l_pri10                        =  40; #
our $FM_RX_PRI_MAP_h_pri10                        =  43; #
our $FM_RX_PRI_MAP_l_pri7                         =  28; #
our $FM_RX_PRI_MAP_h_pri7                         =  31; #
our $FM_RX_PRI_MAP_l_pri13                        =  52; #
our $FM_RX_PRI_MAP_h_pri13                        =  55; #
our $FM_RX_PRI_MAP_l_pri4                         =  16; #
our $FM_RX_PRI_MAP_h_pri4                         =  19; #
our $FM_RX_PRI_MAP_l_pri0                         =  0; #
our $FM_RX_PRI_MAP_h_pri0                         =  3; #
our $FM_RX_PRI_MAP_l_pri14                        =  56; #
our $FM_RX_PRI_MAP_h_pri14                        =  59; #
our $FM_RX_PRI_MAP_l_pri9                         =  36; #
our $FM_RX_PRI_MAP_h_pri9                         =  39; #
our $FM_RX_PRI_MAP_l_pri12                        =  48; #
our $FM_RX_PRI_MAP_h_pri12                        =  51; #
our $FM_RX_PRI_MAP_l_pri5                         =  20; #
our $FM_RX_PRI_MAP_h_pri5                         =  23; #
our $FM_RX_PRI_MAP_l_pri15                        =  60; #
our $FM_RX_PRI_MAP_h_pri15                        =  63; #
our $FM_RX_PRI_MAP_l_pri1                         =  4; #
our $FM_RX_PRI_MAP_h_pri1                         =  7; #
our $FM_RX_PRI_MAP_l_pri8                         =  32; #
our $FM_RX_PRI_MAP_h_pri8                         =  35; #
our $FM_RX_PRI_MAP_l_pri6                         =  24; #
our $FM_RX_PRI_MAP_h_pri6                         =  27; #
our $FM_RX_PRI_MAP_l_pri11                        =  44; #
our $FM_RX_PRI_MAP_h_pri11                        =  47; #
our $FM_RX_PRI_MAP_l_pri2                         =  8; #
our $FM_RX_PRI_MAP_h_pri2                         =  11; #

our $FM_PACING_RATE_b_UNH_A_Behavior              =  13; #
our $FM_PACING_RATE_b_DeleteAllR                  =  12; #
our $FM_PACING_RATE_l_SyncBufferWatermark         =  8; #
our $FM_PACING_RATE_h_SyncBufferWatermark         =  11; #
our $FM_PACING_RATE_l_PacingRate                  =  0; #
our $FM_PACING_RATE_h_PacingRate                  =  7; #

our $FM_SHADOW_FUSEBOX_SERIAL_NUMBER_l_all        =  0; #
our $FM_SHADOW_FUSEBOX_SERIAL_NUMBER_h_all        =  31; #

our $FM_SHADOW_FUSEBOX_REPAIR_SYNC_ADDR_l_all     =  0; #
our $FM_SHADOW_FUSEBOX_REPAIR_SYNC_ADDR_h_all     =  23; #

our $FM_FUSEBOX_INFO_l_maxPorts                   =  27; #
our $FM_FUSEBOX_INFO_h_maxPorts                   =  31; #
our $FM_FUSEBOX_INFO_l_repairedBanks              =  6; #
our $FM_FUSEBOX_INFO_h_repairedBanks              =  8; #
our $FM_FUSEBOX_INFO_l_maxSegs                    =  9; #
our $FM_FUSEBOX_INFO_h_maxSegs                    =  22; #
our $FM_FUSEBOX_INFO_l_packageVersion             =  0; #
our $FM_FUSEBOX_INFO_h_packageVersion             =  3; #
our $FM_FUSEBOX_INFO_l_chipVersion                =  23; #
our $FM_FUSEBOX_INFO_h_chipVersion                =  26; #

our $FM_FID_TABLE_b_parityError                   =  0; #
our $FM_FID_TABLE_l_STPState                      =  2; #
our $FM_FID_TABLE_h_STPState                      =  49; #

our $FM_SHADOW_FUSEBOX_REPAIR_ASYNC_l_repair1     =  0; #
our $FM_SHADOW_FUSEBOX_REPAIR_ASYNC_h_repair1     =  15; #
our $FM_SHADOW_FUSEBOX_REPAIR_ASYNC_l_repair2     =  16; #
our $FM_SHADOW_FUSEBOX_REPAIR_ASYNC_h_repair2     =  31; #

our $FM_CROSSRING_RESET_b_MSB                     =  1; #
our $FM_CROSSRING_RESET_b_MTABLE                  =  3; #
our $FM_CROSSRING_RESET_b_FrameStat               =  4; #
our $FM_CROSSRING_RESET_b_FrameControl            =  0; #
our $FM_CROSSRING_RESET_b_LSM                     =  2; #

our $FM_MAC_VLAN_ETYPE_l_VLANEtherTypeA           =  0; #
our $FM_MAC_VLAN_ETYPE_h_VLANEtherTypeA           =  15; #
our $FM_MAC_VLAN_ETYPE_l_STagTypeB                =  16; #
our $FM_MAC_VLAN_ETYPE_h_STagTypeB                =  31; #
our $FM_MAC_VLAN_ETYPE_l_VLANEtherTypeB           =  16; #
our $FM_MAC_VLAN_ETYPE_h_VLANEtherTypeB           =  31; #
our $FM_MAC_VLAN_ETYPE_l_STagTypeA                =  0; #
our $FM_MAC_VLAN_ETYPE_h_STagTypeA                =  15; #

our $FM_SHADOW_FUSEBOX_REPAIR_SYNC_VALUE_l_all    =  0; #
our $FM_SHADOW_FUSEBOX_REPAIR_SYNC_VALUE_h_all    =  31; #

our $FM_SYS_CFG_5_l_reserved                      =  0; #
our $FM_SYS_CFG_5_h_reserved                      =  30; #

our $FM_CM_IM_l_reserved                          =  0; #
our $FM_CM_IM_h_reserved                          =  31; #

our $FM_FUSEBOX_SERIAL_NUMBER_l_all               =  0; #
our $FM_FUSEBOX_SERIAL_NUMBER_h_all               =  31; #

our $FM_FUSEBOX_UNUSED_l_all                      =  0; #
our $FM_FUSEBOX_UNUSED_h_all                      =  31; #


our $FM_TX_PRI_MAP_2_l_PRI12                      =  16; #
our $FM_TX_PRI_MAP_2_h_PRI12                      =  19; #
our $FM_TX_PRI_MAP_2_l_PRI9                       =  4; #
our $FM_TX_PRI_MAP_2_h_PRI9                       =  7; #
our $FM_TX_PRI_MAP_2_l_PRI10                      =  8; #
our $FM_TX_PRI_MAP_2_h_PRI10                      =  11; #
our $FM_TX_PRI_MAP_2_l_PRI15                      =  28; #
our $FM_TX_PRI_MAP_2_h_PRI15                      =  31; #
our $FM_TX_PRI_MAP_2_l_PRI8                       =  0; #
our $FM_TX_PRI_MAP_2_h_PRI8                       =  3; #
our $FM_TX_PRI_MAP_2_l_PRI13                      =  20; #
our $FM_TX_PRI_MAP_2_h_PRI13                      =  23; #
our $FM_TX_PRI_MAP_2_l_PRI11                      =  12; #
our $FM_TX_PRI_MAP_2_h_PRI11                      =  15; #
our $FM_TX_PRI_MAP_2_l_PRI14                      =  24; #
our $FM_TX_PRI_MAP_2_h_PRI14                      =  27; #

our $FM_AN_RX_CW_Hi_l_message                     =  0; #
our $FM_AN_RX_CW_Hi_h_message                     =  47; #

our $FM_CN_GLOBAL_CFG_l_EtherType                 =  16; #
our $FM_CN_GLOBAL_CFG_h_EtherType                 =  31; #
our $FM_CN_GLOBAL_CFG_l_SamplePeriod              =  2; #
our $FM_CN_GLOBAL_CFG_h_SamplePeriod              =  11; #
our $FM_CN_GLOBAL_CFG_l_CongestionMode            =  0; #
our $FM_CN_GLOBAL_CFG_h_CongestionMode            =  1; #
our $FM_CN_GLOBAL_CFG_l_Mode                      =  0; #
our $FM_CN_GLOBAL_CFG_h_Mode                      =  1; #

our $FM_SHADOW_FUSEBOX_UNUSED_l_all               =  0; #
our $FM_SHADOW_FUSEBOX_UNUSED_h_all               =  31; #

our $FM_AN_TX_CW_Lo_l_message                     =  0; #
our $FM_AN_TX_CW_Lo_h_message                     =  47; #

our $FM_CM_RX_SHARED_SMP_HOG_WM_l_watermark       =  0; #
our $FM_CM_RX_SHARED_SMP_HOG_WM_h_watermark       =  12; #


our $FM_AGING_SERVE_WM_l_AGING_SERVE_WM           =  0; #
our $FM_AGING_SERVE_WM_h_AGING_SERVE_WM           =  30; #

our $FM_CM_IP_l_reserved                          =  0; #
our $FM_CM_IP_h_reserved                          =  31; #

our $FM_TRIGGER_CFG_l_action                      =  16; #
our $FM_TRIGGER_CFG_h_action                      =  18; #
our $FM_TRIGGER_CFG_b_destMACMatch                =  2; #
our $FM_TRIGGER_CFG_b_unicast                     =  7; #
our $FM_TRIGGER_CFG_b_srcMACMiss                  =  1; #
our $FM_TRIGGER_CFG_b_destPortMatch               =  5; #
our $FM_TRIGGER_CFG_b_VLAN                        =  6; #
our $FM_TRIGGER_CFG_b_srcPortMatch                =  4; #
our $FM_TRIGGER_CFG_b_broadcast                   =  8; #
our $FM_TRIGGER_CFG_l_trigID                      =  28; #
our $FM_TRIGGER_CFG_h_trigID                      =  31; #
our $FM_TRIGGER_CFG_l_outputPriority              =  24; #
our $FM_TRIGGER_CFG_h_outputPriority              =  27; #
our $FM_TRIGGER_CFG_b_srcMACMatch                 =  0; #
our $FM_TRIGGER_CFG_b_destMACMiss                 =  3; #
our $FM_TRIGGER_CFG_l_outputPort                  =  19; #
our $FM_TRIGGER_CFG_h_outputPort                  =  23; #
our $FM_TRIGGER_CFG_b_destOrSrcMatch              =  11; #
our $FM_TRIGGER_CFG_b_multicast                   =  9; #
our $FM_TRIGGER_CFG_b_priority                    =  10; #

our $FM_MGR_IP_l_reserved                         =  0; #
our $FM_MGR_IP_h_reserved                         =  31; #

our $FM_MAC_CFG_4_l_PauseInterval                 =  0; #
our $FM_MAC_CFG_4_h_PauseInterval                 =  15; #

our $FM_CM_RX_SHARED_WM_l_watermark               =  0; #
our $FM_CM_RX_SHARED_WM_h_watermark               =  12; #

our $FM_SHADOW_FUSEBOX_INFO_l_maxPorts            =  27; #
our $FM_SHADOW_FUSEBOX_INFO_h_maxPorts            =  31; #
our $FM_SHADOW_FUSEBOX_INFO_l_repairedBanks       =  6; #
our $FM_SHADOW_FUSEBOX_INFO_h_repairedBanks       =  8; #
our $FM_SHADOW_FUSEBOX_INFO_l_maxSegs             =  9; #
our $FM_SHADOW_FUSEBOX_INFO_h_maxSegs             =  22; #
our $FM_SHADOW_FUSEBOX_INFO_l_packageVersion      =  0; #
our $FM_SHADOW_FUSEBOX_INFO_h_packageVersion      =  3; #
our $FM_SHADOW_FUSEBOX_INFO_l_chipVersion         =  23; #
our $FM_SHADOW_FUSEBOX_INFO_h_chipVersion         =  26; #

our $FM_AN_TX_CW_Hi_l_message                     =  0; #
our $FM_AN_TX_CW_Hi_h_message                     =  47; #

our $FM_CONTEPL_CTRLSTAT_b_continuityEnable       =  1; #
our $FM_CONTEPL_CTRLSTAT_b_continuityStatus       =  0; #

our $FM_CM_RX_SHARED_SMP_USAGE_l_count            =  0; #
our $FM_CM_RX_SHARED_SMP_USAGE_h_count            =  13; #

our $FM_FUSEBOX_REPAIR_ASYNC_l_repair1            =  0; #
our $FM_FUSEBOX_REPAIR_ASYNC_h_repair1            =  15; #
our $FM_FUSEBOX_REPAIR_ASYNC_l_repair2            =  16; #
our $FM_FUSEBOX_REPAIR_ASYNC_h_repair2            =  31; #

our $FM_SYS_CFG_2_b_enableVLAN                    =  2; #
our $FM_SYS_CFG_2_b_VLANUniTunnel                 =  0; #
our $FM_SYS_CFG_2_b_multipleSPT                   =  3; #
our $FM_SYS_CFG_2_b_VLANMultiTunnel               =  1; #

our $FM_TRIGGER_CONDITION_AMASK_l_HandlerActionMask  =  0; #
our $FM_TRIGGER_CONDITION_AMASK_h_HandlerActionMask  =  31; #

our $FM_CHIP_MODE_l_Command                       =  0; #
our $FM_CHIP_MODE_h_Command                       =  3; #
our $FM_CHIP_MODE_b_useShadow                     =  20; #
our $FM_CHIP_MODE_b_execFuse                      =  19; #
our $FM_CHIP_MODE_b_execMem                       =  18; #
our $FM_CHIP_MODE_b_execAsyncRamRepair            =  0; #
our $FM_CHIP_MODE_b_execEeprom                    =  0; #

our $FM_AN_TimeoutValue_l_AN_TimeoutValue         =  0; #
our $FM_AN_TimeoutValue_h_AN_TimeoutValue         =  15; #

our $FM_VID_TABLE_b_TrapIGMP                      =  77; #
our $FM_VID_TABLE_l_CounterIndex                  =  2; #
our $FM_VID_TABLE_h_CounterIndex                  =  7; #
our $FM_VID_TABLE_b_reflect                       =  1; #
our $FM_VID_TABLE_b_parityError                   =  0; #
our $FM_VID_TABLE_l_FID                           =  64; #
our $FM_VID_TABLE_h_FID                           =  75; #
our $FM_VID_TABLE_l_trigId                        =  8; #
our $FM_VID_TABLE_h_trigId                        =  13; #
our $FM_VID_TABLE_l_membership                    =  14; #
our $FM_VID_TABLE_h_membership                    =  63; #
our $FM_VID_TABLE_l_vcnt                          =  2; #
our $FM_VID_TABLE_h_vcnt                          =  7; #


our $FM_LCI_CFG_b_rxEnable                        =  0; #
our $FM_LCI_CFG_b_txComputeCRC                    =  2; #
our $FM_LCI_CFG_b_hostPadding                     =  4; #
our $FM_LCI_CFG_l_crcOffset                       =  11; #
our $FM_LCI_CFG_h_crcOffset                       =  16; #
our $FM_LCI_CFG_l_hdrOffset                       =  5; #
our $FM_LCI_CFG_h_hdrOffset                       =  10; #
our $FM_LCI_CFG_b_endianness                      =  3; #

our $FM_LCI_RX_FIFO_l_all                         =  0; #
our $FM_LCI_RX_FIFO_h_all                         =  31; #

our $FM_LCI_TX_FIFO_l_all                         =  0; #
our $FM_LCI_TX_FIFO_h_all                         =  31; #

our $FM_LCI_IP_b_LCI_RXTailerror                  =  5; #
our $FM_LCI_IP_b_LCI_TXOverrun                    =  7; #
our $FM_LCI_IP_b_newFrameRecv                     =  1; #
our $FM_LCI_IP_b_pauseStateChange                 =  0; #
our $FM_LCI_IP_b_endOfFrameSend                   =  2; #
our $FM_LCI_IP_b_LCI_RXInternalError              =  4; #
our $FM_LCI_IP_b_LCI_RXUnderflow                  =  6; #
our $FM_LCI_IP_b_LCI_RXError                      =  3; #

our $FM_LCI_IM_b_LCI_RXTailerror                  =  5; #
our $FM_LCI_IM_b_LCI_TXOverrun                    =  7; #
our $FM_LCI_IM_b_newFrameRecv                     =  1; #
our $FM_LCI_IM_b_pauseStateChange                 =  0; #
our $FM_LCI_IM_b_endOfFrameSend                   =  2; #
our $FM_LCI_IM_b_LCI_RXInternalError              =  4; #
our $FM_LCI_IM_b_LCI_RXUnderflow                  =  6; #
our $FM_LCI_IM_b_LCI_RXError                      =  3; #

our $FM_LCI_STATUS_b_rxReady                      =  1; #
our $FM_LCI_STATUS_b_txReady                      =  0; #
our $FM_LCI_STATUS_b_testAndSetLock               =  3; #
our $FM_LCI_STATUS_l_pendingTxWords               =  4; #
our $FM_LCI_STATUS_h_pendingTxWords               =  15; #
our $FM_LCI_STATUS_b_rxEndOfFrame                 =  2; #

our $FM_SCAN_DATA_OUT_l_all                       =  0; #
our $FM_SCAN_DATA_OUT_h_all                       =  31; #

our $FM_SCAN_SEL_l_all                            =  0; #
our $FM_SCAN_SEL_h_all                            =  31; #

our $FM_SCAN_FREQ_MULT_l_MGMT2SCAN                =  0; #
our $FM_SCAN_FREQ_MULT_h_MGMT2SCAN                =  7; #

our $FM_SCAN_CTRL_l_shiftCnt                      =  2; #
our $FM_SCAN_CTRL_h_shiftCnt                      =  6; #
our $FM_SCAN_CTRL_b_testMode                      =  1; #
our $FM_SCAN_CTRL_b_enableCapture                 =  0; #

our $FM_SCAN_DATA_IN_l_all                        =  0; #
our $FM_SCAN_DATA_IN_h_all                        =  31; #

our $FM_LAST_FATAL_CODE_l_FatalCode               =  0; #
our $FM_LAST_FATAL_CODE_h_FatalCode               =  7; #

our $FM_INTERRUPT_DETECT_b_ARP                    =  17; #
our $FM_INTERRUPT_DETECT_b_Trigger                =  13; #
our $FM_INTERRUPT_DETECT_b_GlobalInterruptMask    =  31; #
our $FM_INTERRUPT_DETECT_b_VLAN_Egress            =  10; #
our $FM_INTERRUPT_DETECT_b_TCN                    =  15; #
our $FM_INTERRUPT_DETECT_b_VLAN_Ingress           =  11; #
our $FM_INTERRUPT_DETECT_b_POLICER                =  18; #
our $FM_INTERRUPT_DETECT_b_RxFIFO                 =  20; #
our $FM_INTERRUPT_DETECT_b_CM                     =  16; #
our $FM_INTERRUPT_DETECT_b_EPL                    =  0; #
our $FM_INTERRUPT_DETECT_b_GPIO                   =  3; #
our $FM_INTERRUPT_DETECT_b_I2C                    =  4; #
our $FM_INTERRUPT_DETECT_b_SOFT                   =  1; #
our $FM_INTERRUPT_DETECT_b_MSB                    =  9; #
our $FM_INTERRUPT_DETECT_b_CRM                    =  6; #
our $FM_INTERRUPT_DETECT_b_Security               =  12; #
our $FM_INTERRUPT_DETECT_b_PERR_FrameHandler      =  14; #
our $FM_INTERRUPT_DETECT_b_PERR                   =  2; #
our $FM_INTERRUPT_DETECT_b_MDIO                   =  5; #
our $FM_INTERRUPT_DETECT_b_TxFIFO                 =  21; #

our $FM_FATAL_COUNT_l_ResetCount                  =  0; #
our $FM_FATAL_COUNT_h_ResetCount                  =  7; #

our $FM_SOFT_RESET_b_MgmtReset                    =  2; #
our $FM_SOFT_RESET_b_FH_Reset                     =  3; #
our $FM_SOFT_RESET_b_SwitchReset                  =  0; #
our $FM_SOFT_RESET_b_CoreReset                    =  1; #

our $FM_WATCHDOG_CFG_b_Enable                     =  0; #

our $FM_PLL_FH_STAT_b_locked                      =  0; #

our $FM_PLL_FH_CTRL_b_powerDown                   =  1; #
our $FM_PLL_FH_CTRL_b_PLLoutEnable                =  15; #
our $FM_PLL_FH_CTRL_b_enGracefulDegradation       =  16; #
our $FM_PLL_FH_CTRL_l_M                           =  4; #
our $FM_PLL_FH_CTRL_h_M                           =  10; #
our $FM_PLL_FH_CTRL_b_bypass                      =  0; #
our $FM_PLL_FH_CTRL_l_P                           =  2; #
our $FM_PLL_FH_CTRL_h_P                           =  3; #
our $FM_PLL_FH_CTRL_l_N                           =  11; #
our $FM_PLL_FH_CTRL_h_N                           =  14; #

our $FM_VITAL_PRODUCT_DATA_l_PartNumber           =  12; #
our $FM_VITAL_PRODUCT_DATA_h_PartNumber           =  27; #

our $FM_PREFETCH_CFG_l_PrefetchLength             =  0; #
our $FM_PREFETCH_CFG_h_PrefetchLength             =  3; #

our $FM_LSM_INT_DETECT_b_CRM                      =  5; #
our $FM_LSM_INT_DETECT_b_SW                       =  0; #
our $FM_LSM_INT_DETECT_b_MDIO                     =  4; #
our $FM_LSM_INT_DETECT_b_PERR                     =  1; #
our $FM_LSM_INT_DETECT_b_GPIO                     =  2; #
our $FM_LSM_INT_DETECT_b_I2C                      =  3; #

our $FM_GLOBAL_EPL_INT_DETECT_b_port3             =  3; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port15            =  15; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port7             =  7; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port4             =  4; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port21            =  21; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port23            =  23; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port10            =  10; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port12            =  12; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port16            =  16; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port1             =  1; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port8             =  8; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port9             =  9; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port13            =  13; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port20            =  20; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port5             =  5; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port19            =  19; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port17            =  17; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port22            =  22; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port14            =  14; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port2             =  2; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port6             =  6; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port11            =  11; #
our $FM_GLOBAL_EPL_INT_DETECT_b_port18            =  18; #

our $FM_PERR_IP_b_ReferenceCountError             =  5; #
our $FM_PERR_IP_b_MTableCumulativeError           =  10; #
our $FM_PERR_IP_b_TxQueueError                    =  7; #
our $FM_PERR_IP_b_MTableTransientError            =  2; #
our $FM_PERR_IP_b_SubSegmentError                 =  9; #
our $FM_PERR_IP_b_TxQueueFreeList                 =  8; #
our $FM_PERR_IP_b_TxQueueLink                     =  13; #
our $FM_PERR_IP_b_RxQueueLinke                    =  12; #
our $FM_PERR_IP_b_PacketHeaderParityError         =  0; #
our $FM_PERR_IP_b_RxQueuePointerError             =  1; #
our $FM_PERR_IP_b_FreeListError                   =  4; #
our $FM_PERR_IP_b_MTableTableError                =  15; #
our $FM_PERR_IP_b_HeadPolarityError               =  6; #
our $FM_PERR_IP_b_Reserved                        =  14; #

our $FM_PERR_IM_l_mask                            =  0; #
our $FM_PERR_IM_h_mask                            =  15; #

our $FM_SW_IP_l_SoftIP                            =  0; #
our $FM_SW_IP_h_SoftIP                            =  31; #

our $FM_SW_IM_l_SoftIM                            =  0; #
our $FM_SW_IM_h_SoftIM                            =  31; #

our $FM_FRAME_TIME_OUT_l_timeOutMult              =  0; #
our $FM_FRAME_TIME_OUT_h_timeOutMult              =  27; #

our $FM_BOOT_CTRL_l_Command                       =  0; #
our $FM_BOOT_CTRL_h_Command                       =  3; #
our $FM_BOOT_CTRL_b_useShadow                     =  20; #
our $FM_BOOT_CTRL_b_execFuse                      =  19; #
our $FM_BOOT_CTRL_b_execMem                       =  18; #
our $FM_BOOT_CTRL_b_execAsyncRamRepair            =  0; #
our $FM_BOOT_CTRL_b_execEeprom                    =  0; #

our $FM_BOOT_STATUS_b_CommandDone                 =  0; #

our $FM_CLK_MULT_1_l_SPIMult                      =  0; #
our $FM_CLK_MULT_1_h_SPIMult                      =  7; #
our $FM_CLK_MULT_1_l_LEDMult                      =  0; #
our $FM_CLK_MULT_1_h_LEDMult                      =  7; #

our $FM_FUSEBOX_CFG_l_ProgPulseWidth              =  0; #
our $FM_FUSEBOX_CFG_h_ProgPulseWidth              =  7; #

our $FM_VPD_INFO_1_b_IBM_EN                       =  55; #
our $FM_VPD_INFO_1_l_MaskVersion                  =  32; #
our $FM_VPD_INFO_1_h_MaskVersion                  =  39; #
our $FM_VPD_INFO_1_l_MaxSegs                      =  40; #
our $FM_VPD_INFO_1_h_MaxSegs                      =  53; #
our $FM_VPD_INFO_1_l_PortDisableMask              =  1; #
our $FM_VPD_INFO_1_h_PortDisableMask              =  24; #
our $FM_VPD_INFO_1_b_Lock                         =  0; #
our $FM_VPD_INFO_1_l_RepairedSyncBanks            =  60; #
our $FM_VPD_INFO_1_h_RepairedSyncBanks            =  63; #
our $FM_VPD_INFO_1_l_RepairedAsyncBanks           =  56; #
our $FM_VPD_INFO_1_h_RepairedAsyncBanks           =  59; #
our $FM_VPD_INFO_1_b_Reserved1                    =  54; #
our $FM_VPD_INFO_1_b_Valid                        =  0; #
our $FM_VPD_INFO_1_l_Package                      =  25; #
our $FM_VPD_INFO_1_h_Package                      =  31; #

our $FM_VPD_INFO_2_l_jtagID                       =  0; #
our $FM_VPD_INFO_2_h_jtagID                       =  10; #

our $FM_VPD_ASYNC_RAM_REPAIR_l_Repair1            =  0; #
our $FM_VPD_ASYNC_RAM_REPAIR_h_Repair1            =  15; #
our $FM_VPD_ASYNC_RAM_REPAIR_l_Repair2            =  16; #
our $FM_VPD_ASYNC_RAM_REPAIR_h_Repair2            =  31; #

our $FM_VPD_SYNC_RAM_REPAIR_l_Data                =  0; #
our $FM_VPD_SYNC_RAM_REPAIR_h_Data                =  31; #
our $FM_VPD_SYNC_RAM_REPAIR_l_Address             =  32; #
our $FM_VPD_SYNC_RAM_REPAIR_h_Address             =  55; #

our $FM_GPIO_CFG_l_OpenDrain                      =  16; #
our $FM_GPIO_CFG_h_OpenDrain                      =  31; #
our $FM_GPIO_CFG_l_Dir                            =  0; #
our $FM_GPIO_CFG_h_Dir                            =  15; #

our $FM_GPIO_DATA_l_data                          =  0; #
our $FM_GPIO_DATA_h_data                          =  15; #
our $FM_GPIO_DATA_l_reserved                      =  16; #
our $FM_GPIO_DATA_h_reserved                      =  31; #

our $FM_GPIO_IP_l_detectLow                       =  16; #
our $FM_GPIO_IP_h_detectLow                       =  31; #
our $FM_GPIO_IP_l_detectHigh                      =  0; #
our $FM_GPIO_IP_h_detectHigh                      =  15; #

our $FM_GPIO_IM_l_detectLowMask                   =  16; #
our $FM_GPIO_IM_h_detectLowMask                   =  31; #
our $FM_GPIO_IM_l_detectHighMask                  =  0; #
our $FM_GPIO_IM_h_detectHighMask                  =  15; #

our $FM_I2C_CFG_l_Divider                         =  8; #
our $FM_I2C_CFG_h_Divider                         =  19; #
our $FM_I2C_CFG_b_InterruptEnable                 =  24; #
our $FM_I2C_CFG_l_Addr                            =  1; #
our $FM_I2C_CFG_h_Addr                            =  7; #
our $FM_I2C_CFG_l_DebounceFilterCountLimit        =  25; #
our $FM_I2C_CFG_h_DebounceFilterCountLimit        =  31; #
our $FM_I2C_CFG_b_InterruptMask                   =  24; #
our $FM_I2C_CFG_b_Enable                          =  0; #

our $FM_I2C_DATA_l_Data                           =  0; #
our $FM_I2C_DATA_h_Data                           =  31; #

our $FM_I2C_CTRL_l_Command                        =  8; #
our $FM_I2C_CTRL_h_Command                        =  9; #
our $FM_I2C_CTRL_l_LengthR                        =  13; #
our $FM_I2C_CTRL_h_LengthR                        =  15; #
our $FM_I2C_CTRL_l_LengthSent                     =  16; #
our $FM_I2C_CTRL_h_LengthSent                     =  18; #
our $FM_I2C_CTRL_l_LengthW                        =  10; #
our $FM_I2C_CTRL_h_LengthW                        =  12; #
our $FM_I2C_CTRL_l_Addr                           =  0; #
our $FM_I2C_CTRL_h_Addr                           =  7; #
our $FM_I2C_CTRL_l_CommandCompleted               =  19; #
our $FM_I2C_CTRL_h_CommandCompleted               =  22; #
our $FM_I2C_CTRL_b_InterruptPending               =  23; #

our $FM_MDIO_CFG_l_Divider                        =  0; #
our $FM_MDIO_CFG_h_Divider                        =  11; #
our $FM_MDIO_CFG_b_Preamble                       =  12; #
our $FM_MDIO_CFG_b_InterruptEnable                =  13; #
our $FM_MDIO_CFG_b_InterruptMask                  =  13; #
our $FM_MDIO_CFG_l_Reserved                       =  14; #
our $FM_MDIO_CFG_h_Reserved                       =  31; #

our $FM_MDIO_DATA_l_Data                          =  0; #
our $FM_MDIO_DATA_h_Data                          =  15; #
our $FM_MDIO_DATA_l_Reserved                      =  16; #
our $FM_MDIO_DATA_h_Reserved                      =  31; #

our $FM_MDIO_CTRL_l_Device                        =  16; #
our $FM_MDIO_CTRL_h_Device                        =  20; #
our $FM_MDIO_CTRL_l_Command                       =  26; #
our $FM_MDIO_CTRL_h_Command                       =  27; #
our $FM_MDIO_CTRL_l_PhyAddress                    =  21; #
our $FM_MDIO_CTRL_h_PhyAddress                    =  25; #
our $FM_MDIO_CTRL_l_Status                        =  29; #
our $FM_MDIO_CTRL_h_Status                        =  30; #
our $FM_MDIO_CTRL_b_DeviceType                    =  28; #
our $FM_MDIO_CTRL_b_InterruptPending              =  31; #
our $FM_MDIO_CTRL_l_Register                      =  0; #
our $FM_MDIO_CTRL_h_Register                      =  15; #

our $FM_LED_CFG_b_LEDInvert                       =  17; #
our $FM_LED_CFG_l_LEDMode                         =  18; #
our $FM_LED_CFG_h_LEDMode                         =  19; #
our $FM_LED_CFG_l_LEDFreq                         =  0; #
our $FM_LED_CFG_h_LEDFreq                         =  15; #
our $FM_LED_CFG_b_LEDEnable                       =  16; #

our $FM_EPL_PORT_CTRL_b_InitializeN               =  3; #
our $FM_EPL_PORT_CTRL_b_SinkMgmtReq               =  1; #
our $FM_EPL_PORT_CTRL_b_BorderResetN              =  1; #
our $FM_EPL_PORT_CTRL_b_ResetN                    =  0; #
our $FM_EPL_PORT_CTRL_b_ClockSelect               =  2; #

our $FM_FUSEBOX_l_Data                            =  0; #
our $FM_FUSEBOX_h_Data                            =  7; #

our $FM_ASYNC_SCAN_CFG_b_ChainSelect              =  29; #
our $FM_ASYNC_SCAN_CFG_l_ScanClockDivisor         =  16; #
our $FM_ASYNC_SCAN_CFG_h_ScanClockDivisor         =  27; #
our $FM_ASYNC_SCAN_CFG_b_TestMode                 =  31; #
our $FM_ASYNC_SCAN_CFG_b_ModeSelect               =  30; #
our $FM_ASYNC_SCAN_CFG_l_ChainEnable              =  0; #
our $FM_ASYNC_SCAN_CFG_h_ChainEnable              =  15; #

our $FM_ASYNC_SCAN_CMD_l_Command                  =  0; #
our $FM_ASYNC_SCAN_CMD_h_Command                  =  15; #

our $FM_ASYNC_SCAN_IN_l_DataIn                    =  0; #
our $FM_ASYNC_SCAN_IN_h_DataIn                    =  15; #

our $FM_ASYNC_SCAN_OUT_l_DataOut                  =  0; #
our $FM_ASYNC_SCAN_OUT_h_DataOut                  =  15; #

our $FM_CRM_CFG_COUNTER_l_CounterSize             =  1; #
our $FM_CRM_CFG_COUNTER_h_CounterSize             =  2; #
our $FM_CRM_CFG_COUNTER_l_CounterAddress          =  3; #
our $FM_CRM_CFG_COUNTER_h_CounterAddress          =  24; #
our $FM_CRM_CFG_COUNTER_b_Enabled                 =  0; #

our $FM_CRM_CFG_WINDOW_l_RateWindow               =  0; #
our $FM_CRM_CFG_WINDOW_h_RateWindow               =  23; #

our $FM_CRM_CFG_LIMIT_l_RateLimit                 =  0; #
our $FM_CRM_CFG_LIMIT_h_RateLimit                 =  31; #

our $FM_CRM_LAST_COUNT_l_LastCount                =  0; #
our $FM_CRM_LAST_COUNT_h_LastCount                =  31; #

our $FM_CRM_EXCEED_COUNT_l_ExceedCount            =  0; #
our $FM_CRM_EXCEED_COUNT_h_ExceedCount            =  31; #

our $FM_CRM_CFG_l_PollingPeriod                   =  0; #
our $FM_CRM_CFG_h_PollingPeriod                   =  15; #

our $FM_CRM_INT_DETECT_l_IntDetect                =  0; #
our $FM_CRM_INT_DETECT_h_IntDetect                =  7; #

our $FM_CRM_IP_l_InterruptPending                 =  0; #
our $FM_CRM_IP_h_InterruptPending                 =  31; #

our $FM_CRM_IM_l_InterruptMask                    =  0; #
our $FM_CRM_IM_h_InterruptMask                    =  31; #

our $FM_SOT_CFG_b_ObsSelect                       =  0; #

our $FM_SERDES_CTRL_1_l_DEQ_LaneC                 =  24; #
our $FM_SERDES_CTRL_1_h_DEQ_LaneC                 =  27; #
our $FM_SERDES_CTRL_1_l_DEQ_LaneB                 =  20; #
our $FM_SERDES_CTRL_1_h_DEQ_LaneB                 =  23; #
our $FM_SERDES_CTRL_1_l_DEQ_LaneD                 =  28; #
our $FM_SERDES_CTRL_1_h_DEQ_LaneD                 =  31; #
our $FM_SERDES_CTRL_1_l_DTX_LaneA                 =  0; #
our $FM_SERDES_CTRL_1_h_DTX_LaneA                 =  3; #
our $FM_SERDES_CTRL_1_l_DTX_LaneC                 =  8; #
our $FM_SERDES_CTRL_1_h_DTX_LaneC                 =  11; #
our $FM_SERDES_CTRL_1_l_DTX_LaneB                 =  4; #
our $FM_SERDES_CTRL_1_h_DTX_LaneB                 =  7; #
our $FM_SERDES_CTRL_1_l_DEQ_LaneA                 =  16; #
our $FM_SERDES_CTRL_1_h_DEQ_LaneA                 =  19; #
our $FM_SERDES_CTRL_1_l_DTX_LaneD                 =  12; #
our $FM_SERDES_CTRL_1_h_DTX_LaneD                 =  15; #

our $FM_SERDES_CTRL_2_l_LaneReset                 =  8; #
our $FM_SERDES_CTRL_2_h_LaneReset                 =  11; #
our $FM_SERDES_CTRL_2_l_HighDrive                 =  4; #
our $FM_SERDES_CTRL_2_h_HighDrive                 =  7; #
our $FM_SERDES_CTRL_2_l_RX_PolarityReversal       =  18; #
our $FM_SERDES_CTRL_2_h_RX_PolarityReversal       =  21; #
our $FM_SERDES_CTRL_2_b_PLLResetAB                =  16; #
our $FM_SERDES_CTRL_2_b_LookForAllCommas          =  26; #
our $FM_SERDES_CTRL_2_b_PLLResetCD                =  17; #
our $FM_SERDES_CTRL_2_l_TX_PolarityReversal       =  22; #
our $FM_SERDES_CTRL_2_h_TX_PolarityReversal       =  25; #
our $FM_SERDES_CTRL_2_l_LowDrive                  =  0; #
our $FM_SERDES_CTRL_2_h_LowDrive                  =  3; #
our $FM_SERDES_CTRL_2_l_LanePowerDown             =  12; #
our $FM_SERDES_CTRL_2_h_LanePowerDown             =  15; #

our $FM_SERDES_CTRL_3_l_LinkCount                 =  20; #
our $FM_SERDES_CTRL_3_h_LinkCount                 =  31; #
our $FM_SERDES_CTRL_3_l_DeassertionCount          =  0; #
our $FM_SERDES_CTRL_3_h_DeassertionCount          =  19; #

our $FM_SERDES_TEST_MODE_b_BIST_Sync              =  5; #
our $FM_SERDES_TEST_MODE_b_FramerEnable           =  6; #
our $FM_SERDES_TEST_MODE_l_BIST_Mode              =  0; #
our $FM_SERDES_TEST_MODE_h_BIST_Mode              =  2; #
our $FM_SERDES_TEST_MODE_l_TestMode               =  3; #
our $FM_SERDES_TEST_MODE_h_TestMode               =  4; #
our $FM_SERDES_TEST_MODE_b_TransmitCJPAT          =  7; #

our $FM_SERDES_STATUS_l_SymbolLock                =  0; #
our $FM_SERDES_STATUS_h_SymbolLock                =  3; #
our $FM_SERDES_STATUS_b_SignalDetect              =  4; #

our $FM_SERDES_IP_b_LOS2                          =  6; #
our $FM_SERDES_IP_b_OBC3                          =  10; #
our $FM_SERDES_IP_b_OBC0                          =  1; #
our $FM_SERDES_IP_b_LOS0                          =  0; #
our $FM_SERDES_IP_b_OBC2                          =  7; #
our $FM_SERDES_IP_b_LOS3                          =  9; #
our $FM_SERDES_IP_b_OBC1                          =  4; #
our $FM_SERDES_IP_l_PhyErrorCount                 =  12; #
our $FM_SERDES_IP_h_PhyErrorCount                 =  31; #
our $FM_SERDES_IP_b_DE3                           =  11; #
our $FM_SERDES_IP_b_DE1                           =  5; #
our $FM_SERDES_IP_b_DE2                           =  8; #
our $FM_SERDES_IP_b_DE0                           =  2; #
our $FM_SERDES_IP_b_LOS1                          =  3; #

our $FM_SERDES_IM_l_SERDES_IM                     =  0; #
our $FM_SERDES_IM_h_SERDES_IM                     =  11; #

our $FM_SERDES_BIST_ERR_CNT_l_BEC3                =  24; #
our $FM_SERDES_BIST_ERR_CNT_h_BEC3                =  31; #
our $FM_SERDES_BIST_ERR_CNT_l_BEC2                =  16; #
our $FM_SERDES_BIST_ERR_CNT_h_BEC2                =  23; #
our $FM_SERDES_BIST_ERR_CNT_l_BEC1                =  8; #
our $FM_SERDES_BIST_ERR_CNT_h_BEC1                =  15; #
our $FM_SERDES_BIST_ERR_CNT_l_BEC0                =  0; #
our $FM_SERDES_BIST_ERR_CNT_h_BEC0                =  7; #

our $FM_PCS_CFG_1_b_IgnoreAllRX_Errors            =  14; #
our $FM_PCS_CFG_1_b_DisableTransmitRS             =  26; #
our $FM_PCS_CFG_1_b_ForceLF                       =  23; #
our $FM_PCS_CFG_1_l_MuxSelect                     =  6; #
our $FM_PCS_CFG_1_h_MuxSelect                     =  7; #
our $FM_PCS_CFG_1_b_IgnoreDataErrors              =  15; #
our $FM_PCS_CFG_1_b_IgnorePreambleErrors          =  16; #
our $FM_PCS_CFG_1_b_EnableSendingRF               =  22; #
our $FM_PCS_CFG_1_l_LFSR_Seed                     =  0; #
our $FM_PCS_CFG_1_h_LFSR_Seed                     =  3; #
our $FM_PCS_CFG_1_l_IFG                           =  8; #
our $FM_PCS_CFG_1_h_IFG                           =  13; #
our $FM_PCS_CFG_1_b_DisableReceiverRS             =  27; #
our $FM_PCS_CFG_1_b_ForceRF                       =  24; #
our $FM_PCS_CFG_1_b_EnableLF_Response             =  21; #
our $FM_PCS_CFG_1_l_DatapathSelect                =  29; #
our $FM_PCS_CFG_1_h_DatapathSelect                =  30; #
our $FM_PCS_CFG_1_b_ShortPreamble                 =  4; #
our $FM_PCS_CFG_1_b_IgnoreIFGErrors               =  17; #
our $FM_PCS_CFG_1_b_EnableDIC                     =  18; #
our $FM_PCS_CFG_1_b_CheckEndEnable                =  31; #
our $FM_PCS_CFG_1_b_ForceFSIG                     =  25; #
our $FM_PCS_CFG_1_b_NibbleDetect                  =  5; #
our $FM_PCS_CFG_1_b_EnableLinkUpNotification      =  28; #
our $FM_PCS_CFG_1_b_InvertRX_LaneOrdering         =  20; #
our $FM_PCS_CFG_1_b_InvertTX_LaneOrdering         =  19; #

our $FM_PCS_CFG_2_l_LF_TX                         =  0; #
our $FM_PCS_CFG_2_h_LF_TX                         =  23; #

our $FM_PCS_CFG_3_l_RF_TX                         =  0; #
our $FM_PCS_CFG_3_h_RF_TX                         =  23; #

our $FM_PCS_CFG_4_l_FSIG_TX                       =  0; #
our $FM_PCS_CFG_4_h_FSIG_TX                       =  23; #

our $FM_PCS_CFG_5_l_FSIG_RX                       =  0; #
our $FM_PCS_CFG_5_h_FSIG_RX                       =  23; #

our $FM_PCS_IP_b_LinkUp                           =  13; #
our $FM_PCS_IP_b_RF_Sent                          =  4; #
our $FM_PCS_IP_b_AN_RX_MSG                        =  17; #
our $FM_PCS_IP_b_AN_Fail                          =  16; #
our $FM_PCS_IP_l_PCS_FIFO_Overflow                =  7; #
our $FM_PCS_IP_h_PCS_FIFO_Overflow                =  10; #
our $FM_PCS_IP_b_RF_Detected                      =  1; #
our $FM_PCS_IP_b_LinkUpDown                       =  11; #
our $FM_PCS_IP_b_LanesMisaligned                  =  6; #
our $FM_PCS_IP_b_LinkDownUp                       =  12; #
our $FM_PCS_IP_b_AN_Done                          =  15; #
our $FM_PCS_IP_b_LF_Sent                          =  3; #
our $FM_PCS_IP_b_FSIG_Sent                        =  5; #
our $FM_PCS_IP_b_FSIG_Detected                    =  2; #
our $FM_PCS_IP_b_LF_Detected                      =  0; #
our $FM_PCS_IP_b_FaultChange                      =  14; #

our $FM_PCS_IM_b_LinkUp                           =  13; #
our $FM_PCS_IM_b_RF_Sent                          =  4; #
our $FM_PCS_IM_b_AN_RX_MSG                        =  17; #
our $FM_PCS_IM_b_AN_Fail                          =  16; #
our $FM_PCS_IM_l_PCS_FIFO_Overflow                =  7; #
our $FM_PCS_IM_h_PCS_FIFO_Overflow                =  10; #
our $FM_PCS_IM_b_RF_Detected                      =  1; #
our $FM_PCS_IM_b_LinkUpDown                       =  11; #
our $FM_PCS_IM_b_LanesMisaligned                  =  6; #
our $FM_PCS_IM_b_LinkDownUp                       =  12; #
our $FM_PCS_IM_b_AN_Done                          =  15; #
our $FM_PCS_IM_b_LF_Sent                          =  3; #
our $FM_PCS_IM_b_FSIG_Sent                        =  5; #
our $FM_PCS_IM_b_FSIG_Detected                    =  2; #
our $FM_PCS_IM_b_LF_Detected                      =  0; #
our $FM_PCS_IM_b_FaultChange                      =  14; #

our $FM_SYNCBUF_CFG_b_UNH_A_Behavior              =  13; #
our $FM_SYNCBUF_CFG_b_DeleteAllR                  =  12; #
our $FM_SYNCBUF_CFG_l_SyncBufferWatermark         =  8; #
our $FM_SYNCBUF_CFG_h_SyncBufferWatermark         =  11; #
our $FM_SYNCBUF_CFG_l_PacingRate                  =  0; #
our $FM_SYNCBUF_CFG_h_PacingRate                  =  7; #

our $FM_MAC_CFG_1_l_CRC_Offset                    =  6; #
our $FM_MAC_CFG_1_h_CRC_Offset                    =  11; #
our $FM_MAC_CFG_1_l_MinFrameSize                  =  24; #
our $FM_MAC_CFG_1_h_MinFrameSize                  =  29; #
our $FM_MAC_CFG_1_l_HeaderOffset                  =  0; #
our $FM_MAC_CFG_1_h_HeaderOffset                  =  4; #
our $FM_MAC_CFG_1_l_MaxFrameSize                  =  12; #
our $FM_MAC_CFG_1_h_MaxFrameSize                  =  23; #

our $FM_MAC_CFG_2_b_DisableTX_MAC                 =  1; #
our $FM_MAC_CFG_2_b_EnableVCfiUpdate              =  12; #
our $FM_MAC_CFG_2_l_VLAN_EtherType                =  16; #
our $FM_MAC_CFG_2_h_VLAN_EtherType                =  31; #
our $FM_MAC_CFG_2_b_EnableTTLDecrement            =  17; #
our $FM_MAC_CFG_2_b_EnableVEtherTypeUpdate        =  13; #
our $FM_MAC_CFG_2_b_PadRuntFrames                 =  8; #
our $FM_MAC_CFG_2_l_VLAN_EType                    =  13; #
our $FM_MAC_CFG_2_h_VLAN_EType                    =  14; #
our $FM_MAC_CFG_2_b_EnableRouting                 =  16; #
our $FM_MAC_CFG_2_b_SerdesErrorDiscard            =  7; #
our $FM_MAC_CFG_2_b_RX_CRC_Discard                =  4; #
our $FM_MAC_CFG_2_b_EnableDSCPModification        =  18; #
our $FM_MAC_CFG_2_b_StripRltOnEgress              =  15; #
our $FM_MAC_CFG_2_b_EnableVPriUpdate              =  11; #
our $FM_MAC_CFG_2_b_EnableSourcePortPause         =  9; #
our $FM_MAC_CFG_2_b_PhyErrorDiscard               =  6; #
our $FM_MAC_CFG_2_b_DrainTX                       =  9; #
our $FM_MAC_CFG_2_b_LengthCheckDiscard            =  10; #
our $FM_MAC_CFG_2_b_DisableRX_Pause               =  2; #
our $FM_MAC_CFG_2_b_DisableRX_MAC                 =  0; #
our $FM_MAC_CFG_2_b_RuntFrameDiscard              =  3; #
our $FM_MAC_CFG_2_b_RX_OversizeDiscard            =  5; #
our $FM_MAC_CFG_2_b_CheckParseError               =  2; #

our $FM_MAC_CFG_3_l_MinEventRate                  =  16; #
our $FM_MAC_CFG_3_h_MinEventRate                  =  21; #
our $FM_MAC_CFG_3_l_TX_PauseValue                 =  0; #
our $FM_MAC_CFG_3_h_TX_PauseValue                 =  15; #
our $FM_MAC_CFG_3_l_MaxParsingDepth               =  24; #
our $FM_MAC_CFG_3_h_MaxParsingDepth               =  31; #

our $FM_MAC_CFG_5_l_PortMACID_Lo                  =  0; #
our $FM_MAC_CFG_5_h_PortMACID_Lo                  =  31; #

our $FM_MAC_CFG_6_l_PortMACID_Hi                  =  0; #
our $FM_MAC_CFG_6_h_PortMACID_Hi                  =  15; #

our $FM_TX_VPRI_MAP_1_l_PRI3                      =  12; #
our $FM_TX_VPRI_MAP_1_h_PRI3                      =  15; #
our $FM_TX_VPRI_MAP_1_l_PRI7                      =  28; #
our $FM_TX_VPRI_MAP_1_h_PRI7                      =  31; #
our $FM_TX_VPRI_MAP_1_l_PRI5                      =  20; #
our $FM_TX_VPRI_MAP_1_h_PRI5                      =  23; #
our $FM_TX_VPRI_MAP_1_l_PRI1                      =  4; #
our $FM_TX_VPRI_MAP_1_h_PRI1                      =  7; #
our $FM_TX_VPRI_MAP_1_l_PRI4                      =  16; #
our $FM_TX_VPRI_MAP_1_h_PRI4                      =  19; #
our $FM_TX_VPRI_MAP_1_l_PRI6                      =  24; #
our $FM_TX_VPRI_MAP_1_h_PRI6                      =  27; #
our $FM_TX_VPRI_MAP_1_l_PRI0                      =  0; #
our $FM_TX_VPRI_MAP_1_h_PRI0                      =  3; #
our $FM_TX_VPRI_MAP_1_l_PRI2                      =  8; #
our $FM_TX_VPRI_MAP_1_h_PRI2                      =  11; #

our $FM_TX_VPRI_MAP_2_l_PRI12                     =  16; #
our $FM_TX_VPRI_MAP_2_h_PRI12                     =  19; #
our $FM_TX_VPRI_MAP_2_l_PRI9                      =  4; #
our $FM_TX_VPRI_MAP_2_h_PRI9                      =  7; #
our $FM_TX_VPRI_MAP_2_l_PRI10                     =  8; #
our $FM_TX_VPRI_MAP_2_h_PRI10                     =  11; #
our $FM_TX_VPRI_MAP_2_l_PRI15                     =  28; #
our $FM_TX_VPRI_MAP_2_h_PRI15                     =  31; #
our $FM_TX_VPRI_MAP_2_l_PRI8                      =  0; #
our $FM_TX_VPRI_MAP_2_h_PRI8                      =  3; #
our $FM_TX_VPRI_MAP_2_l_PRI13                     =  20; #
our $FM_TX_VPRI_MAP_2_h_PRI13                     =  23; #
our $FM_TX_VPRI_MAP_2_l_PRI11                     =  12; #
our $FM_TX_VPRI_MAP_2_h_PRI11                     =  15; #
our $FM_TX_VPRI_MAP_2_l_PRI14                     =  24; #
our $FM_TX_VPRI_MAP_2_h_PRI14                     =  27; #

our $FM_MAC_STATUS_b_RX_Status                    =  1; #
our $FM_MAC_STATUS_b_TX_Status                    =  0; #

our $FM_MAC_IP_b_RX_PhyError                      =  5; #
our $FM_MAC_IP_b_TX_CRC_Error2                    =  7; #
our $FM_MAC_IP_b_RX_RuntError                     =  0; #
our $FM_MAC_IP_b_VLANTAG_TABLE_ParityErr          =  9; #
our $FM_MAC_IP_b_RX_PauseEnableDeAsserted         =  9; #
our $FM_MAC_IP_b_RX_S2A_Overflow                  =  1; #
our $FM_MAC_IP_b_Rx_PauseOverflow                 =  4; #
our $FM_MAC_IP_b_RX_OversizeError                 =  3; #
our $FM_MAC_IP_b_TX_Underflow                     =  8; #
our $FM_MAC_IP_b_RX_CRC_Error                     =  2; #
our $FM_MAC_IP_b_TX_CRC_Error                     =  6; #
our $FM_MAC_IP_b_CheckEndError                    =  11; #
our $FM_MAC_IP_b_RXFabricRequestEnableDeAsserted  =  10; #

our $FM_MAC_IM_b_RX_PhyError                      =  5; #
our $FM_MAC_IM_b_TX_CRC_Error2                    =  7; #
our $FM_MAC_IM_b_RX_RuntError                     =  0; #
our $FM_MAC_IM_b_VLANTAG_TABLE_ParityErr          =  9; #
our $FM_MAC_IM_b_RX_PauseEnableDeAsserted         =  9; #
our $FM_MAC_IM_b_RX_S2A_Overflow                  =  1; #
our $FM_MAC_IM_b_Rx_PauseOverflow                 =  4; #
our $FM_MAC_IM_b_RX_OversizeError                 =  3; #
our $FM_MAC_IM_b_TX_Underflow                     =  8; #
our $FM_MAC_IM_b_RX_CRC_Error                     =  2; #
our $FM_MAC_IM_b_TX_CRC_Error                     =  6; #
our $FM_MAC_IM_b_CheckEndError                    =  11; #
our $FM_MAC_IM_b_RXFabricRequestEnableDeAsserted  =  10; #

our $FM_STAT_EPL_ERROR1_l_UnderflowCount          =  0; #
our $FM_STAT_EPL_ERROR1_h_UnderflowCount          =  7; #
our $FM_STAT_EPL_ERROR1_l_OverflowCount           =  8; #
our $FM_STAT_EPL_ERROR1_h_OverflowCount           =  15; #

our $FM_STAT_TX_PAUSE_l_TX_Pause                  =  0; #
our $FM_STAT_TX_PAUSE_h_TX_Pause                  =  31; #

our $FM_STAT_TX_CRC_l_TX_CRC_Errors               =  0; #
our $FM_STAT_TX_CRC_h_TX_CRC_Errors               =  31; #

our $FM_STAT_EPL_ERROR2_l_InternalCRC_ErrorCount  =  0; #
our $FM_STAT_EPL_ERROR2_h_InternalCRC_ErrorCount  =  15; #

our $FM_STAT_RX_JABBER_l_RX_Jabber                =  0; #
our $FM_STAT_RX_JABBER_h_RX_Jabber                =  15; #

our $FM_EPL_LED_STATUS_b_RX_Receiving             =  3; #
our $FM_EPL_LED_STATUS_b_TX_Transmitting          =  4; #
our $FM_EPL_LED_STATUS_b_RX_Recieving             =  3; #
our $FM_EPL_LED_STATUS_b_PortStatus               =  2; #
our $FM_EPL_LED_STATUS_b_PortError                =  0; #
our $FM_EPL_LED_STATUS_b_RF                       =  1; #

our $FM_EPL_INT_DETECT_b_MAC_IP                   =  2; #
our $FM_EPL_INT_DETECT_b_PCS_IP                   =  1; #
our $FM_EPL_INT_DETECT_b_SERDES_IP                =  0; #

our $FM_STAT_TX_BYTECOUNT_l_ByteCount             =  0; #
our $FM_STAT_TX_BYTECOUNT_h_ByteCount             =  63; #

our $FM_DEBUG_RX_MAC_l_RX_STATE                   =  0; #
our $FM_DEBUG_RX_MAC_h_RX_STATE                   =  5; #
our $FM_DEBUG_RX_MAC_b_badcrc_found               =  7; #
our $FM_DEBUG_RX_MAC_b_len_morethan_max           =  9; #
our $FM_DEBUG_RX_MAC_l_FRM_LEN_OUTPUT             =  16; #
our $FM_DEBUG_RX_MAC_h_FRM_LEN_OUTPUT             =  29; #
our $FM_DEBUG_RX_MAC_b_MACRX_CHECK_END_ERR        =  13; #
our $FM_DEBUG_RX_MAC_b_MACRX_OVERFLOW_ERR         =  8; #
our $FM_DEBUG_RX_MAC_b_discard_frame              =  15; #
our $FM_DEBUG_RX_MAC_b_MACRX_BYTESTRM_ERR         =  12; #
our $FM_DEBUG_RX_MAC_b_bad_length                 =  14; #
our $FM_DEBUG_RX_MAC_b_MACRX_PHYFRM_ERR           =  11; #
our $FM_DEBUG_RX_MAC_b_MACRX_RUNT_ERR             =  10; #

our $FM_DEBUG_RX_RS_l_RX_STATE                    =  0; #
our $FM_DEBUG_RX_RS_h_RX_STATE                    =  2; #
our $FM_DEBUG_RX_RS_l_BYTESTREAM_ERROR            =  6; #
our $FM_DEBUG_RX_RS_h_BYTESTREAM_ERROR            =  9; #
our $FM_DEBUG_RX_RS_b_check_end_termination_fail  =  5; #
our $FM_DEBUG_RX_RS_l_EOP_found                   =  10; #
our $FM_DEBUG_RX_RS_h_EOP_found                   =  13; #
our $FM_DEBUG_RX_RS_b_check_end_2ndcol_fail       =  4; #

our $FM_DEBUG_SYNCBUF_b_Delete_Idle               =  23; #
our $FM_DEBUG_SYNCBUF_l_watermark_lane3           =  12; #
our $FM_DEBUG_SYNCBUF_h_watermark_lane3           =  15; #
our $FM_DEBUG_SYNCBUF_l_Align_CNT                 =  20; #
our $FM_DEBUG_SYNCBUF_h_Align_CNT                 =  22; #
our $FM_DEBUG_SYNCBUF_b_sfdn_detect               =  24; #
our $FM_DEBUG_SYNCBUF_l_watermark_lane0           =  0; #
our $FM_DEBUG_SYNCBUF_h_watermark_lane0           =  3; #
our $FM_DEBUG_SYNCBUF_l_level_lane0               =  0; #
our $FM_DEBUG_SYNCBUF_h_level_lane0               =  3; #
our $FM_DEBUG_SYNCBUF_l_watermark_lane1           =  4; #
our $FM_DEBUG_SYNCBUF_h_watermark_lane1           =  7; #
our $FM_DEBUG_SYNCBUF_b_Switch_DataNibble         =  25; #
our $FM_DEBUG_SYNCBUF_l_Delete_R_R                =  16; #
our $FM_DEBUG_SYNCBUF_h_Delete_R_R                =  19; #
our $FM_DEBUG_SYNCBUF_l_level_lane3               =  12; #
our $FM_DEBUG_SYNCBUF_h_level_lane3               =  15; #
our $FM_DEBUG_SYNCBUF_l_level_lane1               =  4; #
our $FM_DEBUG_SYNCBUF_h_level_lane1               =  7; #
our $FM_DEBUG_SYNCBUF_l_watermark_lane2           =  8; #
our $FM_DEBUG_SYNCBUF_h_watermark_lane2           =  11; #
our $FM_DEBUG_SYNCBUF_l_level_lane2               =  8; #
our $FM_DEBUG_SYNCBUF_h_level_lane2               =  11; #

our $FM_DEBUG_TX_MAC_b_frame_error_forfsm         =  27; #
our $FM_DEBUG_TX_MAC_l_TXMAC_STATE                =  0; #
our $FM_DEBUG_TX_MAC_h_TXMAC_STATE                =  3; #
our $FM_DEBUG_TX_MAC_b_pad_frame                  =  28; #
our $FM_DEBUG_TX_MAC_l_data_slack_avail           =  4; #
our $FM_DEBUG_TX_MAC_h_data_slack_avail           =  10; #
our $FM_DEBUG_TX_MAC_b_pad_misalign               =  6; #
our $FM_DEBUG_TX_MAC_b_overflow                   =  11; #
our $FM_DEBUG_TX_MAC_b_frame_error                =  7; #
our $FM_DEBUG_TX_MAC_l_TXDP_STATE                 =  4; #
our $FM_DEBUG_TX_MAC_h_TXDP_STATE                 =  5; #
our $FM_DEBUG_TX_MAC_b_real_frame_error           =  26; #
our $FM_DEBUG_TX_MAC_l_reg_EXTRA                  =  8; #
our $FM_DEBUG_TX_MAC_h_reg_EXTRA                  =  25; #
our $FM_DEBUG_TX_MAC_b_err_tail                   =  29; #
our $FM_DEBUG_TX_MAC_l_ready_toks_avail           =  0; #
our $FM_DEBUG_TX_MAC_h_ready_toks_avail           =  2; #

our $FM_SRC_MAC_LO_l_SrcMacLo                     =  0; #
our $FM_SRC_MAC_LO_h_SrcMacLo                     =  31; #

our $FM_SRC_MAC_HI_l_SrcMacHi                     =  0; #
our $FM_SRC_MAC_HI_h_SrcMacHi                     =  15; #

our $FM_SRC_MAC_VIRTUAL_LO_l_SrcMacLo             =  0; #
our $FM_SRC_MAC_VIRTUAL_LO_h_SrcMacLo             =  31; #

our $FM_SRC_MAC_VIRTUAL_HI_l_SrcMacHi             =  0; #
our $FM_SRC_MAC_VIRTUAL_HI_h_SrcMacHi             =  15; #

our $FM_JITTER_TIMER_l_TXJitterSS                 =  0; #
our $FM_JITTER_TIMER_h_TXJitterSS                 =  5; #
our $FM_JITTER_TIMER_l_TXJitterSF                 =  8; #
our $FM_JITTER_TIMER_h_TXJitterSF                 =  13; #
our $FM_JITTER_TIMER_l_TXJitterCT                 =  16; #
our $FM_JITTER_TIMER_h_TXJitterCT                 =  21; #

our $FM_PARSE_CFG_b_ParseL4                       =  6; #
our $FM_PARSE_CFG_b_FlagIPv4Options               =  7; #
our $FM_PARSE_CFG_l_ISLTag                        =  0; #
our $FM_PARSE_CFG_h_ISLTag                        =  2; #
our $FM_PARSE_CFG_b_FlagIPv6Dest                  =  11; #
our $FM_PARSE_CFG_b_Send32B                       =  4; #
our $FM_PARSE_CFG_b_FlagIPv6Frag                  =  10; #
our $FM_PARSE_CFG_b_FlagIPv6Auth                  =  12; #
our $FM_PARSE_CFG_b_ParseL3                       =  5; #
our $FM_PARSE_CFG_b_SendOtherL3                   =  4; #
our $FM_PARSE_CFG_b_FlagIPv6Routing               =  9; #
our $FM_PARSE_CFG_b_FlagIPv6HopByHop              =  8; #
our $FM_PARSE_CFG_l_CNType                        =  16; #
our $FM_PARSE_CFG_h_CNType                        =  31; #

our $FM_MAC_VLAN_ETYPE_2_l_VLANEtherTypeA         =  0; #
our $FM_MAC_VLAN_ETYPE_2_h_VLANEtherTypeA         =  15; #
our $FM_MAC_VLAN_ETYPE_2_l_STagTypeB              =  16; #
our $FM_MAC_VLAN_ETYPE_2_h_STagTypeB              =  31; #
our $FM_MAC_VLAN_ETYPE_2_l_VLANEtherTypeB         =  16; #
our $FM_MAC_VLAN_ETYPE_2_h_VLANEtherTypeB         =  31; #
our $FM_MAC_VLAN_ETYPE_2_l_STagTypeA              =  0; #
our $FM_MAC_VLAN_ETYPE_2_h_STagTypeA              =  15; #

our $FM_PARSE_RLT_1_l_CPIDLo                      =  0; #
our $FM_PARSE_RLT_1_h_CPIDLo                      =  31; #

our $FM_PARSE_RLT_2_l_CPIDHi                      =  0; #
our $FM_PARSE_RLT_2_h_CPIDHi                      =  15; #
our $FM_PARSE_RLT_2_l_RLTType                     =  16; #
our $FM_PARSE_RLT_2_h_RLTType                     =  31; #

our $FM_TX_TRUNC_l_RSVD                           =  28; #
our $FM_TX_TRUNC_h_RSVD                           =  31; #
our $FM_TX_TRUNC_l_MirrorTruncationLen            =  16; #
our $FM_TX_TRUNC_h_MirrorTruncationLen            =  27; #
our $FM_TX_TRUNC_l_CpuTruncationLen               =  0; #
our $FM_TX_TRUNC_h_CpuTruncationLen               =  11; #

our $FM_CPID_0_l_CPID                             =  0; #
our $FM_CPID_0_h_CPID                             =  63; #

our $FM_CPID_1_l_CPID                             =  0; #
our $FM_CPID_1_h_CPID                             =  63; #

our $FM_CPID_2_l_CPID                             =  0; #
our $FM_CPID_2_h_CPID                             =  63; #

our $FM_CPID_3_l_CPID                             =  0; #
our $FM_CPID_3_h_CPID                             =  63; #

our $FM_CPID_4_l_CPID                             =  0; #
our $FM_CPID_4_h_CPID                             =  63; #

our $FM_CPID_5_l_CPID                             =  0; #
our $FM_CPID_5_h_CPID                             =  63; #

our $FM_CPID_6_l_CPID                             =  0; #
our $FM_CPID_6_h_CPID                             =  63; #

our $FM_CPID_7_l_CPID                             =  0; #
our $FM_CPID_7_h_CPID                             =  63; #

our $FM_MAC_VLAN_ETYPE_1_l_F32Type                =  16; #
our $FM_MAC_VLAN_ETYPE_1_h_F32Type                =  31; #
our $FM_MAC_VLAN_ETYPE_1_l_CTagType               =  0; #
our $FM_MAC_VLAN_ETYPE_1_h_CTagType               =  15; #

our $FM_DI_CFG_b_CaptureTCPFlags                  =  16; #
our $FM_DI_CFG_l_OtherL3Length                    =  18; #
our $FM_DI_CFG_h_OtherL3Length                    =  23; #
our $FM_DI_CFG_b_CaptureTCP_HL_RSV_OPTIONS_Fields  =  16; #
our $FM_DI_CFG_l_CustomProtocol1                  =  0; #
our $FM_DI_CFG_h_CustomProtocol1                  =  7; #
our $FM_DI_CFG_l_OtherProtocolLength              =  24; #
our $FM_DI_CFG_h_OtherProtocolLength              =  29; #
our $FM_DI_CFG_l_CustomProtocol2                  =  8; #
our $FM_DI_CFG_h_CustomProtocol2                  =  15; #

our $FM_TCP_WD_MASK_LO_l_TCP_WD_Mask_Lo           =  0; #
our $FM_TCP_WD_MASK_LO_h_TCP_WD_Mask_Lo           =  31; #

our $FM_TCP_WD_MASK_HI_l_TCP_WD_Mask_Hi           =  0; #
our $FM_TCP_WD_MASK_HI_h_TCP_WD_Mask_Hi           =  15; #
our $FM_TCP_WD_MASK_HI_l_TCP_WD_Mask_Lo           =  0; #
our $FM_TCP_WD_MASK_HI_h_TCP_WD_Mask_Lo           =  15; #

our $FM_UDP_WD_MASK_LO_l_UDP_WD_Mask_Lo           =  0; #
our $FM_UDP_WD_MASK_LO_h_UDP_WD_Mask_Lo           =  31; #

our $FM_UDP_WD_MASK_HI_l_UDP_WD_Mask_Lo           =  0; #
our $FM_UDP_WD_MASK_HI_h_UDP_WD_Mask_Lo           =  15; #
our $FM_UDP_WD_MASK_HI_l_UDP_WD_Mask_Hi           =  0; #
our $FM_UDP_WD_MASK_HI_h_UDP_WD_Mask_Hi           =  15; #

our $FM_L4PROT1_WD_MASK_LO_l_Prot1_WD_Mask_Lo     =  0; #
our $FM_L4PROT1_WD_MASK_LO_h_Prot1_WD_Mask_Lo     =  31; #
our $FM_L4PROT1_WD_MASK_LO_l_Prot_WD_Mask_Lo      =  0; #
our $FM_L4PROT1_WD_MASK_LO_h_Prot_WD_Mask_Lo      =  31; #

our $FM_L4PROT1_WD_MASK_HI_l_Prot1_WD_Mask_Hi     =  0; #
our $FM_L4PROT1_WD_MASK_HI_h_Prot1_WD_Mask_Hi     =  15; #
our $FM_L4PROT1_WD_MASK_HI_l_GRE_WD_Mask_Lo       =  0; #
our $FM_L4PROT1_WD_MASK_HI_h_GRE_WD_Mask_Lo       =  15; #

our $FM_L4PROT2_WD_MASK_LO_l_Custom_WD_Mask_Lo    =  0; #
our $FM_L4PROT2_WD_MASK_LO_h_Custom_WD_Mask_Lo    =  31; #
our $FM_L4PROT2_WD_MASK_LO_l_Prot2_WD_Mask_Lo     =  0; #
our $FM_L4PROT2_WD_MASK_LO_h_Prot2_WD_Mask_Lo     =  31; #

our $FM_L4PROT2_WD_MASK_HI_l_Custom_WD_Mask_Lo    =  0; #
our $FM_L4PROT2_WD_MASK_HI_h_Custom_WD_Mask_Lo    =  15; #
our $FM_L4PROT2_WD_MASK_HI_l_Prot2_WD_Mask_Hi     =  0; #
our $FM_L4PROT2_WD_MASK_HI_h_Prot2_WD_Mask_Hi     =  15; #

our $FM_AN_TX_MSG0_l_message                      =  0; #
our $FM_AN_TX_MSG0_h_message                      =  47; #

our $FM_AN_TX_MSG1_l_message                      =  0; #
our $FM_AN_TX_MSG1_h_message                      =  47; #

our $FM_AN_RX_MSG0_l_message                      =  0; #
our $FM_AN_RX_MSG0_h_message                      =  47; #

our $FM_AN_RX_MSG1_l_message                      =  0; #
our $FM_AN_RX_MSG1_h_message                      =  47; #

our $FM_AN_CTL_l_AN_Mode                          =  3; #
our $FM_AN_CTL_h_AN_Mode                          =  4; #
our $FM_AN_CTL_b_AN_Enable                        =  0; #
our $FM_AN_CTL_b_AN_Select                        =  2; #
our $FM_AN_CTL_b_AN_RestartRX_Check               =  1; #

our $FM_AN_STATUS_l_AN_State                      =  0; #
our $FM_AN_STATUS_h_AN_State                      =  3; #
our $FM_AN_STATUS_b_AN_Msg_Select                 =  4; #

our $FM_AN_TIMEOUT_l_AN_TimeoutValue              =  0; #
our $FM_AN_TIMEOUT_h_AN_TimeoutValue              =  15; #

our $FM_AN_TX_TIMER_l_TimerValue                  =  0; #
our $FM_AN_TX_TIMER_h_TimerValue                  =  23; #

our $FM_VLANTAG_TABLE_l_TagEnable                 =  0; #
our $FM_VLANTAG_TABLE_h_TagEnable                 =  31; #

our $FM_SCHED_GROUP_CFG_l_StrictPriority          =  16; #
our $FM_SCHED_GROUP_CFG_h_StrictPriority          =  23; #
our $FM_SCHED_GROUP_CFG_l_PrioritySetBoundary     =  8; #
our $FM_SCHED_GROUP_CFG_h_PrioritySetBoundary     =  15; #
our $FM_SCHED_GROUP_CFG_l_SchedulingGroupBoundary  =  0; #
our $FM_SCHED_GROUP_CFG_h_SchedulingGroupBoundary  =  7; #

our $FM_FUSE_PORT_l_all                           =  0; #
our $FM_FUSE_PORT_h_all                           =  31; #

our $FM_FUSE_SEG_l_all                            =  0; #
our $FM_FUSE_SEG_h_all                            =  31; #

our $FM_MSB_CFG_b_interruptGlortEn                =  5; #
our $FM_MSB_CFG_b_attachedCpu                     =  1; #
our $FM_MSB_CFG_b_disableIbm                      =  0; #
our $FM_MSB_CFG_b_padHsmFrame                     =  2; #
our $FM_MSB_CFG_b_ibmGlortEn                      =  4; #
our $FM_MSB_CFG_b_padIbmResponse                  =  3; #

our $FM_MSB_IBM_GLORT_l_ibmGlort                  =  0; #
our $FM_MSB_IBM_GLORT_h_ibmGlort                  =  15; #

our $FM_MSB_IBM_INT_l_interruptGlort              =  0; #
our $FM_MSB_IBM_INT_h_interruptGlort              =  15; #
our $FM_MSB_IBM_INT_l_interruptInterval           =  16; #
our $FM_MSB_IBM_INT_h_interruptInterval           =  31; #

our $FM_MSB_INT_FRAME_l_etherType                 =  0; #
our $FM_MSB_INT_FRAME_h_etherType                 =  15; #
our $FM_MSB_INT_FRAME_l_islSysPri                 =  16; #
our $FM_MSB_INT_FRAME_h_islSysPri                 =  19; #
our $FM_MSB_INT_FRAME_l_islUserBits               =  20; #
our $FM_MSB_INT_FRAME_h_islUserBits               =  27; #

our $FM_MSB_STATS_0_l_ibmFrameCtr                 =  0; #
our $FM_MSB_STATS_0_h_ibmFrameCtr                 =  31; #

our $FM_MSB_STATS_1_l_nonIbmFrameCtr              =  0; #
our $FM_MSB_STATS_1_h_nonIbmFrameCtr              =  31; #

our $FM_MSB_STATS_2_l_errorFrameCtr               =  0; #
our $FM_MSB_STATS_2_h_errorFrameCtr               =  31; #

our $FM_MSB_INTR_CTR_0_l_interruptCtr             =  0; #
our $FM_MSB_INTR_CTR_0_h_interruptCtr             =  31; #

our $FM_MSB_INTR_CTR_1_l_interruptSeqCtr          =  0; #
our $FM_MSB_INTR_CTR_1_h_interruptSeqCtr          =  31; #

our $FM_MSB_INTR_CTR_2_l_ibmCrcErrorCtr           =  0; #
our $FM_MSB_INTR_CTR_2_h_ibmCrcErrorCtr           =  31; #

our $FM_MSB_INTR_CTR_3_l_nonIbmCrcErrorCtr        =  0; #
our $FM_MSB_INTR_CTR_3_h_nonIbmCrcErrorCtr        =  31; #

our $FM_MSB_INTR_CTR_4_l_frameAndNoCpuCtr         =  0; #
our $FM_MSB_INTR_CTR_4_h_frameAndNoCpuCtr         =  31; #

our $FM_MSB_INTR_CTR_5_l_invalidIbmOpCtr          =  0; #
our $FM_MSB_INTR_CTR_5_h_invalidIbmOpCtr          =  31; #

our $FM_MSB_IP_b_ipNonIbmCrcError                 =  1; #
our $FM_MSB_IP_b_ipInvalidIbmOp                   =  3; #
our $FM_MSB_IP_b_ipFrameAndNoCpu                  =  2; #
our $FM_MSB_IP_b_ipIbmCrcError                    =  0; #

our $FM_MSB_IM_b_imFrameAndNoCpu                  =  2; #
our $FM_MSB_IM_b_imIbmCrcError                    =  0; #
our $FM_MSB_IM_b_imNonIbmCrcError                 =  1; #
our $FM_MSB_IM_b_imInvalidIbmOp                   =  3; #

our $FM_MSB_RX_EPL_RATE_l_interFrameGap           =  8; #
our $FM_MSB_RX_EPL_RATE_h_interFrameGap           =  11; #
our $FM_MSB_RX_EPL_RATE_l_numCycles               =  0; #
our $FM_MSB_RX_EPL_RATE_h_numCycles               =  7; #

our $FM_MSB_SCRATCH_0_l_scratch0                  =  0; #
our $FM_MSB_SCRATCH_0_h_scratch0                  =  31; #

our $FM_MSB_SCRATCH_1_l_scratch1                  =  0; #
our $FM_MSB_SCRATCH_1_h_scratch1                  =  31; #

our $FM_MSB_SCRATCH_2_l_scratch2                  =  0; #
our $FM_MSB_SCRATCH_2_h_scratch2                  =  31; #

our $FM_MSB_SCRATCH_3_l_scratch3                  =  0; #
our $FM_MSB_SCRATCH_3_h_scratch3                  =  31; #

our $FM_MSB_SCRATCH_4_l_scratch4                  =  0; #
our $FM_MSB_SCRATCH_4_h_scratch4                  =  31; #

our $FM_MSB_SCRATCH_5_l_scratch5                  =  0; #
our $FM_MSB_SCRATCH_5_h_scratch5                  =  31; #

our $FM_MSB_SCRATCH_6_l_scratch6                  =  0; #
our $FM_MSB_SCRATCH_6_h_scratch6                  =  31; #

our $FM_MSB_SCRATCH_7_l_scratch7                  =  0; #
our $FM_MSB_SCRATCH_7_h_scratch7                  =  31; #

our $FM_MSB_SCRATCH_8_l_scratch8                  =  0; #
our $FM_MSB_SCRATCH_8_h_scratch8                  =  31; #

our $FM_MSB_SCRATCH_9_l_scratch9                  =  0; #
our $FM_MSB_SCRATCH_9_h_scratch9                  =  31; #

our $FM_MSB_SCRATCH_10_l_scratch10                =  0; #
our $FM_MSB_SCRATCH_10_h_scratch10                =  31; #

our $FM_MSB_SCRATCH_11_l_scratch11                =  0; #
our $FM_MSB_SCRATCH_11_h_scratch11                =  31; #

our $FM_MSB_SCRATCH_12_l_scratch12                =  0; #
our $FM_MSB_SCRATCH_12_h_scratch12                =  31; #

our $FM_MSB_SCRATCH_13_l_scratch13                =  0; #
our $FM_MSB_SCRATCH_13_h_scratch13                =  31; #

our $FM_MSB_SCRATCH_14_l_scratch14                =  0; #
our $FM_MSB_SCRATCH_14_h_scratch14                =  31; #

our $FM_MSB_SCRATCH_15_l_scratch15                =  0; #
our $FM_MSB_SCRATCH_15_h_scratch15                =  31; #

our $FM_MSB_TS_b_ibmHasFbuf                       =  7; #
our $FM_MSB_TS_b_intHasPort0                      =  5; #
our $FM_MSB_TS_b_fsHasPort0                       =  3; #
our $FM_MSB_TS_b_fh0HasPort0                      =  2; #
our $FM_MSB_TS_b_hsmHasFbuf                       =  6; #
our $FM_MSB_TS_b_schHasPort0                      =  4; #
our $FM_MSB_TS_b_lsmHasPort0                      =  1; #
our $FM_MSB_TS_b_fbufHasPort0                     =  0; #

our $FM_MSB_CREDITS_l_hsmCredits                  =  0; #
our $FM_MSB_CREDITS_h_hsmCredits                  =  1; #

our $FM_MSB_SRAM_REPAIR_0_b_bank0LockBit          =  11; #
our $FM_MSB_SRAM_REPAIR_0_l_bank0RepairAddr       =  0; #
our $FM_MSB_SRAM_REPAIR_0_h_bank0RepairAddr       =  10; #

our $FM_MSB_SRAM_REPAIR_1_l_bank1RepairAddr       =  0; #
our $FM_MSB_SRAM_REPAIR_1_h_bank1RepairAddr       =  10; #
our $FM_MSB_SRAM_REPAIR_1_b_bank1LockBit          =  11; #





























































































































our $FM_TX_MIRROR_l_srcport                       =  8; #
our $FM_TX_MIRROR_h_srcport                       =  13; #
our $FM_TX_MIRROR_l_dstport                       =  0; #
our $FM_TX_MIRROR_h_dstport                       =  5; #

our $FM_LOG_MASK_l_DestMask                       =  0; #
our $FM_LOG_MASK_h_DestMask                       =  24; #

our $FM_LOOPBACK_SUPPRESS_l_glort                 =  0; #
our $FM_LOOPBACK_SUPPRESS_h_glort                 =  15; #
our $FM_LOOPBACK_SUPPRESS_l_mask                  =  16; #
our $FM_LOOPBACK_SUPPRESS_h_mask                  =  31; #

our $FM_MIRROR_GLORTS_l_txMirrorGlort             =  16; #
our $FM_MIRROR_GLORTS_h_txMirrorGlort             =  31; #
our $FM_MIRROR_GLORTS_l_logGlort                  =  0; #
our $FM_MIRROR_GLORTS_h_logGlort                  =  15; #
our $FM_MIRROR_GLORTS_l_cpuGlort                  =  0; #
our $FM_MIRROR_GLORTS_h_cpuGlort                  =  15; #

our $FM_IP_MULTICAST_TABLE_b_tail                 =  1; #
our $FM_IP_MULTICAST_TABLE_b_skip                 =  3; #
our $FM_IP_MULTICAST_TABLE_b_parity               =  0; #
our $FM_IP_MULTICAST_TABLE_b_novlan               =  2; #
our $FM_IP_MULTICAST_TABLE_l_nxtptr               =  16; #
our $FM_IP_MULTICAST_TABLE_h_nxtptr               =  29; #
our $FM_IP_MULTICAST_TABLE_l_vlan                 =  4; #
our $FM_IP_MULTICAST_TABLE_h_vlan                 =  15; #

our $FM_SYS_CFG_1_b_trapIGMP                      =  4; #
our $FM_SYS_CFG_1_b_trapMTUViolations             =  11; #
our $FM_SYS_CFG_1_b_remapCPUSP15                  =  8; #
our $FM_SYS_CFG_1_b_trapSlow                      =  0; #
our $FM_SYS_CFG_1_b_broadcastCPU                  =  6; #
our $FM_SYS_CFG_1_b_broadcastDisable              =  15; #
our $FM_SYS_CFG_1_b_trap802_1x                    =  5; #
our $FM_SYS_CFG_1_b_remapIEEESP15                 =  7; #
our $FM_SYS_CFG_1_b_floodControlMulticast         =  14; #
our $FM_SYS_CFG_1_b_trapBPDU                      =  2; #
our $FM_SYS_CFG_1_b_trapLACP                      =  1; #
our $FM_SYS_CFG_1_b_dropPause                     =  10; #
our $FM_SYS_CFG_1_b_floodControlUnicast           =  13; #
our $FM_SYS_CFG_1_b_trapGARP                      =  3; #
our $FM_SYS_CFG_1_b_enableTrapPlusLog             =  12; #
our $FM_SYS_CFG_1_b_remapEtherTrapSP15            =  9; #

our $FM_SYS_CFG_3_l_CPUMAMSB                      =  0; #
our $FM_SYS_CFG_3_h_CPUMAMSB                      =  15; #

our $FM_SYS_CFG_4_l_all                           =  0; #
our $FM_SYS_CFG_4_h_all                           =  31; #

our $FM_SYS_CFG_7_l_ageTime                       =  0; #
our $FM_SYS_CFG_7_h_ageTime                       =  30; #
our $FM_SYS_CFG_7_b_disableAging                  =  31; #

our $FM_SYS_CFG_8_b_enableFFU                     =  0; #
our $FM_SYS_CFG_8_b_allowQTagPause                =  1; #

our $FM_PORT_VLAN_IP_1_l_all                      =  0; #
our $FM_PORT_VLAN_IP_1_h_all                      =  31; #

our $FM_PORT_VLAN_IM_1_b_Reserved1                =  0; #
our $FM_PORT_VLAN_IM_1_l_mask                     =  1; #
our $FM_PORT_VLAN_IM_1_h_mask                     =  24; #
our $FM_PORT_VLAN_IM_1_l_Reserved2                =  25; #
our $FM_PORT_VLAN_IM_1_h_Reserved2                =  31; #

our $FM_PORT_VLAN_IP_2_l_all                      =  0; #
our $FM_PORT_VLAN_IP_2_h_all                      =  31; #

our $FM_PORT_VLAN_IM_2_b_Reserved1                =  0; #
our $FM_PORT_VLAN_IM_2_l_mask                     =  1; #
our $FM_PORT_VLAN_IM_2_h_mask                     =  24; #
our $FM_PORT_VLAN_IM_2_l_Reserved2                =  25; #
our $FM_PORT_VLAN_IM_2_h_Reserved2                =  31; #

our $FM_PORT_MAC_SEC_IP_l_all                     =  0; #
our $FM_PORT_MAC_SEC_IP_h_all                     =  31; #

our $FM_PORT_MAC_SEC_IM_l_mask                    =  1; #
our $FM_PORT_MAC_SEC_IM_h_mask                    =  24; #

our $FM_FH_INT_DETECT_b_ARP_IP                    =  7; #
our $FM_FH_INT_DETECT_b_Port_Vlan_IP_1            =  0; #
our $FM_FH_INT_DETECT_b_Port_Vlan_IP_2            =  1; #
our $FM_FH_INT_DETECT_b_CM_IP                     =  6; #
our $FM_FH_INT_DETECT_b_Port_Mac_Sec_IP           =  2; #
our $FM_FH_INT_DETECT_b_POLICER_IP                =  8; #
our $FM_FH_INT_DETECT_b_MA_TCN_IP                 =  5; #
our $FM_FH_INT_DETECT_b_PARITY_IP                 =  4; #
our $FM_FH_INT_DETECT_b_Triggers                  =  3; #

our $FM_SYS_CFG_ROUTER_l_trapTTL1                 =  0; #
our $FM_SYS_CFG_ROUTER_h_trapTTL1                 =  1; #
our $FM_SYS_CFG_ROUTER_l_TTLdisposal              =  0; #
our $FM_SYS_CFG_ROUTER_h_TTLdisposal              =  1; #
our $FM_SYS_CFG_ROUTER_b_trapIPOptions            =  2; #

our $FM_L34_HASH_CFG_b_Symmetric                  =  0; #
our $FM_L34_HASH_CFG_b_UsePROT                    =  3; #
our $FM_L34_HASH_CFG_l_PROT2                      =  24; #
our $FM_L34_HASH_CFG_h_PROT2                      =  31; #
our $FM_L34_HASH_CFG_b_UseSIP                     =  1; #
our $FM_L34_HASH_CFG_b_UseL4SRC                   =  8; #
our $FM_L34_HASH_CFG_l_ECMP_Rotation              =  10; #
our $FM_L34_HASH_CFG_h_ECMP_Rotation              =  11; #
our $FM_L34_HASH_CFG_b_UseTCP                     =  4; #
our $FM_L34_HASH_CFG_l_PROT1                      =  16; #
our $FM_L34_HASH_CFG_h_PROT1                      =  23; #
our $FM_L34_HASH_CFG_b_UsePROT1                   =  6; #
our $FM_L34_HASH_CFG_l_RSVD                       =  12; #
our $FM_L34_HASH_CFG_h_RSVD                       =  15; #
our $FM_L34_HASH_CFG_b_UseL4DST                   =  9; #
our $FM_L34_HASH_CFG_b_UseDIP                     =  2; #
our $FM_L34_HASH_CFG_b_UsePROT2                   =  7; #
our $FM_L34_HASH_CFG_b_UseUDP                     =  5; #

our $FM_L34_FLOW_HASH_CFG_1_l_UserMask            =  8; #
our $FM_L34_FLOW_HASH_CFG_1_h_UserMask            =  15; #
our $FM_L34_FLOW_HASH_CFG_1_l_DiffServMask        =  0; #
our $FM_L34_FLOW_HASH_CFG_1_h_DiffServMask        =  5; #

our $FM_L34_FLOW_HASH_CFG_2_l_FlowLabelMask       =  0; #
our $FM_L34_FLOW_HASH_CFG_2_h_FlowLabelMask       =  19; #

our $FM_L234_HASH_CFG_b_UseVPRI                   =  6; #
our $FM_L234_HASH_CFG_b_Symmetric                 =  2; #
our $FM_L234_HASH_CFG_b_UseSMAC                   =  4; #
our $FM_L234_HASH_CFG_b_TahoeCompatible           =  12; #
our $FM_L234_HASH_CFG_b_UseL2ifIP                 =  0; #
our $FM_L234_HASH_CFG_b_UseVID                    =  7; #
our $FM_L234_HASH_CFG_b_UseDMAC                   =  3; #
our $FM_L234_HASH_CFG_b_UseL34                    =  1; #
our $FM_L234_HASH_CFG_b_UseRoutedSMAC             =  14; #
our $FM_L234_HASH_CFG_l_RotationB                 =  10; #
our $FM_L234_HASH_CFG_h_RotationB                 =  11; #
our $FM_L234_HASH_CFG_b_UseRoutedDMAC             =  13; #
our $FM_L234_HASH_CFG_b_UseType                   =  5; #
our $FM_L234_HASH_CFG_b_UseHash34                 =  15; #
our $FM_L234_HASH_CFG_l_RotationA                 =  8; #
our $FM_L234_HASH_CFG_h_RotationA                 =  9; #

our $FM_TX_MIRROR_FH_l_srcport                    =  8; #
our $FM_TX_MIRROR_FH_h_srcport                    =  13; #
our $FM_TX_MIRROR_FH_l_dstport                    =  0; #
our $FM_TX_MIRROR_FH_h_dstport                    =  5; #

our $FM_CPU_TRAP_MASK_FH_l_DestMask               =  0; #
our $FM_CPU_TRAP_MASK_FH_h_DestMask               =  24; #

our $FM_CPU_LOG_MASK_FH_l_DestMask                =  0; #
our $FM_CPU_LOG_MASK_FH_h_DestMask                =  24; #

our $FM_RX_MIRROR_CFG_l_rxMirrorPort              =  16; #
our $FM_RX_MIRROR_CFG_h_rxMirrorPort              =  20; #
our $FM_RX_MIRROR_CFG_l_rxMirrorGlort             =  0; #
our $FM_RX_MIRROR_CFG_h_rxMirrorGlort             =  15; #

our $FM_PARITY_IP_b_parIpFid                      =  1; #
our $FM_PARITY_IP_b_parIpMA                       =  4; #
our $FM_PARITY_IP_b_parGlrtTable                  =  6; #
our $FM_PARITY_IP_b_parIpEgressVid                =  2; #
our $FM_PARITY_IP_b_parIpVid                      =  0; #
our $FM_PARITY_IP_b_parTCN                        =  8; #
our $FM_PARITY_IP_b_parIpGlrtRam                  =  5; #
our $FM_PARITY_IP_b_parIpEgressFid                =  3; #
our $FM_PARITY_IP_b_parIpIngressVid               =  0; #
our $FM_PARITY_IP_b_parARP                        =  9; #
our $FM_PARITY_IP_b_parIpIngressFid               =  1; #
our $FM_PARITY_IP_b_parFFUramTable                =  7; #

our $FM_PARITY_IM_b_parIpFid                      =  1; #
our $FM_PARITY_IM_b_parIpMA                       =  4; #
our $FM_PARITY_IM_b_parGlrtTable                  =  6; #
our $FM_PARITY_IM_b_parIpEgressVid                =  2; #
our $FM_PARITY_IM_b_parIpVid                      =  0; #
our $FM_PARITY_IM_b_parTCN                        =  8; #
our $FM_PARITY_IM_b_parIpGlrtRam                  =  5; #
our $FM_PARITY_IM_b_parIpEgressFid                =  3; #
our $FM_PARITY_IM_b_parIpIngressVid               =  0; #
our $FM_PARITY_IM_b_parARP                        =  9; #
our $FM_PARITY_IM_b_parIpIngressFid               =  1; #
our $FM_PARITY_IM_b_parFFUramTable                =  7; #

our $FM_TRAP_GLORT_l_trapGlort                    =  0; #
our $FM_TRAP_GLORT_h_trapGlort                    =  15; #

our $FM_SAF_MATRIX_b_port7                        =  7; #
our $FM_SAF_MATRIX_b_port4                        =  4; #
our $FM_SAF_MATRIX_b_port0                        =  0; #
our $FM_SAF_MATRIX_b_port21                       =  21; #
our $FM_SAF_MATRIX_b_port12                       =  12; #
our $FM_SAF_MATRIX_b_port1                        =  1; #
our $FM_SAF_MATRIX_b_port24                       =  24; #
our $FM_SAF_MATRIX_b_port9                        =  9; #
our $FM_SAF_MATRIX_b_port19                       =  19; #
our $FM_SAF_MATRIX_b_port17                       =  17; #
our $FM_SAF_MATRIX_b_port14                       =  14; #
our $FM_SAF_MATRIX_b_port2                        =  2; #
our $FM_SAF_MATRIX_b_port11                       =  11; #
our $FM_SAF_MATRIX_b_port3                        =  3; #
our $FM_SAF_MATRIX_b_port15                       =  15; #
our $FM_SAF_MATRIX_b_port10                       =  10; #
our $FM_SAF_MATRIX_b_port23                       =  23; #
our $FM_SAF_MATRIX_b_port16                       =  16; #
our $FM_SAF_MATRIX_b_port8                        =  8; #
our $FM_SAF_MATRIX_b_port13                       =  13; #
our $FM_SAF_MATRIX_b_port5                        =  5; #
our $FM_SAF_MATRIX_b_port20                       =  20; #
our $FM_SAF_MATRIX_b_port22                       =  22; #
our $FM_SAF_MATRIX_b_port6                        =  6; #
our $FM_SAF_MATRIX_b_port18                       =  18; #

our $FM_FH_LOOPBACK_SUPPRESS_l_glort              =  0; #
our $FM_FH_LOOPBACK_SUPPRESS_h_glort              =  15; #
our $FM_FH_LOOPBACK_SUPPRESS_l_mask               =  16; #
our $FM_FH_LOOPBACK_SUPPRESS_h_mask               =  31; #

our $FM_INTERNAL_PORT_MASK_l_mask                 =  0; #
our $FM_INTERNAL_PORT_MASK_h_mask                 =  24; #

our $FM_MGMT_CLK_COUNTER_l_MGMT_CLK_COUNTER       =  0; #
our $FM_MGMT_CLK_COUNTER_h_MGMT_CLK_COUNTER       =  20; #

our $FM_MGMT_FFU_CLK_COUNTER_l_MGMT_FFU_CLK_COUNTER  =  0; #
our $FM_MGMT_FFU_CLK_COUNTER_h_MGMT_FFU_CLK_COUNTER  =  20; #

our $FM_HEAD_TAIL_SPACING_1_l_headTailWait        =  0; #
our $FM_HEAD_TAIL_SPACING_1_h_headTailWait        =  3; #
our $FM_HEAD_TAIL_SPACING_1_l_tailHeadWait        =  4; #
our $FM_HEAD_TAIL_SPACING_1_h_tailHeadWait        =  7; #

our $FM_HEAD_TAIL_SPACING_2_l_headTailSpacing     =  0; #
our $FM_HEAD_TAIL_SPACING_2_h_headTailSpacing     =  3; #
our $FM_HEAD_TAIL_SPACING_2_l_tailHeadSpacing     =  4; #
our $FM_HEAD_TAIL_SPACING_2_h_tailHeadSpacing     =  7; #
our $FM_HEAD_TAIL_SPACING_2_l_mgmtSpacing         =  8; #
our $FM_HEAD_TAIL_SPACING_2_h_mgmtSpacing         =  11; #

our $FM_HEAD_TAIL_SPACING_3_l_headTailSpacing     =  0; #
our $FM_HEAD_TAIL_SPACING_3_h_headTailSpacing     =  3; #
our $FM_HEAD_TAIL_SPACING_3_l_tailHeadSpacing     =  4; #
our $FM_HEAD_TAIL_SPACING_3_h_tailHeadSpacing     =  7; #
our $FM_HEAD_TAIL_SPACING_3_l_mgmtSpacing         =  8; #
our $FM_HEAD_TAIL_SPACING_3_h_mgmtSpacing         =  11; #

our $FM_PRE_TO_MID_FIFO_CFG_l_stallInputThreshold  =  6; #
our $FM_PRE_TO_MID_FIFO_CFG_h_stallInputThreshold  =  11; #
our $FM_PRE_TO_MID_FIFO_CFG_l_srcLookupThreshold  =  0; #
our $FM_PRE_TO_MID_FIFO_CFG_h_srcLookupThreshold  =  5; #

our $FM_PORT_CFG_1_l_defaultVID                   =  0; #
our $FM_PORT_CFG_1_h_defaultVID                   =  11; #
our $FM_PORT_CFG_1_b_dropUntagged                 =  23; #
our $FM_PORT_CFG_1_l_defaultDSCP                  =  16; #
our $FM_PORT_CFG_1_h_defaultDSCP                  =  21; #
our $FM_PORT_CFG_1_l_defaultVPRI                  =  12; #
our $FM_PORT_CFG_1_h_defaultVPRI                  =  15; #
our $FM_PORT_CFG_1_b_EncapsulateVLAN              =  31; #
our $FM_PORT_CFG_1_b_dropTagged                   =  22; #
our $FM_PORT_CFG_1_b_SwitchPriorityFromVLAN       =  27; #
our $FM_PORT_CFG_1_b_SwitchPriorityPrefersDSCP    =  30; #
our $FM_PORT_CFG_1_b_AssumeUntagged               =  31; #
our $FM_PORT_CFG_1_b_useDefaultVLAN               =  25; #
our $FM_PORT_CFG_1_b_useDefaultDSCP               =  26; #
our $FM_PORT_CFG_1_b_dropMgmtISL                  =  24; #
our $FM_PORT_CFG_1_b_SwitchPriorityFromDSCP       =  28; #
our $FM_PORT_CFG_1_b_SwitchPriorityFromISL        =  29; #

our $FM_PORT_CFG_2_l_destinationMask              =  0; #
our $FM_PORT_CFG_2_h_destinationMask              =  24; #

our $FM_PORT_CFG_3_b_remapSecuritySP15            =  5; #
our $FM_PORT_CFG_3_b_LearningEnable               =  1; #
our $FM_PORT_CFG_3_l_RelaxSourceLookup            =  5; #
our $FM_PORT_CFG_3_h_RelaxSourceLookup            =  6; #
our $FM_PORT_CFG_3_b_SecurityTrap                 =  4; #
our $FM_PORT_CFG_3_l_SecurityType                 =  2; #
our $FM_PORT_CFG_3_h_SecurityType                 =  3; #
our $FM_PORT_CFG_3_b_filterVLANIngress            =  0; #

our $FM_PORT_CFG_ISL_l_srcGlort                   =  0; #
our $FM_PORT_CFG_ISL_h_srcGlort                   =  15; #
our $FM_PORT_CFG_ISL_l_defaultPriority            =  24; #
our $FM_PORT_CFG_ISL_h_defaultPriority            =  27; #
our $FM_PORT_CFG_ISL_l_USR                        =  16; #
our $FM_PORT_CFG_ISL_h_USR                        =  23; #

our $FM_RX_VPRI_MAP_l_pri3                        =  12; #
our $FM_RX_VPRI_MAP_h_pri3                        =  15; #
our $FM_RX_VPRI_MAP_l_pri10                       =  40; #
our $FM_RX_VPRI_MAP_h_pri10                       =  43; #
our $FM_RX_VPRI_MAP_l_pri7                        =  28; #
our $FM_RX_VPRI_MAP_h_pri7                        =  31; #
our $FM_RX_VPRI_MAP_l_pri13                       =  52; #
our $FM_RX_VPRI_MAP_h_pri13                       =  55; #
our $FM_RX_VPRI_MAP_l_pri4                        =  16; #
our $FM_RX_VPRI_MAP_h_pri4                        =  19; #
our $FM_RX_VPRI_MAP_l_pri0                        =  0; #
our $FM_RX_VPRI_MAP_h_pri0                        =  3; #
our $FM_RX_VPRI_MAP_l_pri14                       =  56; #
our $FM_RX_VPRI_MAP_h_pri14                       =  59; #
our $FM_RX_VPRI_MAP_l_pri9                        =  36; #
our $FM_RX_VPRI_MAP_h_pri9                        =  39; #
our $FM_RX_VPRI_MAP_l_pri12                       =  48; #
our $FM_RX_VPRI_MAP_h_pri12                       =  51; #
our $FM_RX_VPRI_MAP_l_pri5                        =  20; #
our $FM_RX_VPRI_MAP_h_pri5                        =  23; #
our $FM_RX_VPRI_MAP_l_pri15                       =  60; #
our $FM_RX_VPRI_MAP_h_pri15                       =  63; #
our $FM_RX_VPRI_MAP_l_pri1                        =  4; #
our $FM_RX_VPRI_MAP_h_pri1                        =  7; #
our $FM_RX_VPRI_MAP_l_pri8                        =  32; #
our $FM_RX_VPRI_MAP_h_pri8                        =  35; #
our $FM_RX_VPRI_MAP_l_pri6                        =  24; #
our $FM_RX_VPRI_MAP_h_pri6                        =  27; #
our $FM_RX_VPRI_MAP_l_pri11                       =  44; #
our $FM_RX_VPRI_MAP_h_pri11                       =  47; #
our $FM_RX_VPRI_MAP_l_pri2                        =  8; #
our $FM_RX_VPRI_MAP_h_pri2                        =  11; #

our $FM_DSCP_PRI_MAP_l_pri                        =  0; #
our $FM_DSCP_PRI_MAP_h_pri                        =  3; #

our $FM_VPRI_PRI_MAP_l_pri                        =  0; #
our $FM_VPRI_PRI_MAP_h_pri                        =  3; #

our $FM_CM_TX_TC_PRIVATE_WM_l_watermark           =  0; #
our $FM_CM_TX_TC_PRIVATE_WM_h_watermark           =  12; #

our $FM_CM_TX_TC_USAGE_l_count                    =  0; #
our $FM_CM_TX_TC_USAGE_h_count                    =  13; #

our $FM_CM_TX_SMP_HOG_WM_l_watermark              =  0; #
our $FM_CM_TX_SMP_HOG_WM_h_watermark              =  12; #

our $FM_CM_RX_SMP_PAUSE_WM_l_PauseOff             =  16; #
our $FM_CM_RX_SMP_PAUSE_WM_h_PauseOff             =  28; #
our $FM_CM_RX_SMP_PAUSE_WM_l_PauseOn              =  0; #
our $FM_CM_RX_SMP_PAUSE_WM_h_PauseOn              =  12; #

our $FM_CM_RX_SMP_PRIVATE_WM_l_watermark          =  0; #
our $FM_CM_RX_SMP_PRIVATE_WM_h_watermark          =  12; #

our $FM_CM_RX_SMP_USAGE_l_count                   =  0; #
our $FM_CM_RX_SMP_USAGE_h_count                   =  13; #

our $FM_CM_TX_SMP_USAGE_l_count                   =  0; #
our $FM_CM_TX_SMP_USAGE_h_count                   =  13; #

our $FM_CM_TX_SMP_PRIVATE_WM_l_watermark          =  0; #
our $FM_CM_TX_SMP_PRIVATE_WM_h_watermark          =  12; #

our $FM_CM_RX_SMP_HOG_WM_l_watermark              =  0; #
our $FM_CM_RX_SMP_HOG_WM_h_watermark              =  12; #

our $FM_CM_PAUSE_RESEND_INTERVAL_l_INTERVAL       =  0; #
our $FM_CM_PAUSE_RESEND_INTERVAL_h_INTERVAL       =  15; #

our $FM_CM_PORT_CFG_l_DISASSOCIATION_MASK         =  8; #
our $FM_CM_PORT_CFG_h_DISASSOCIATION_MASK         =  15; #
our $FM_CM_PORT_CFG_b_SMP1_RL_ACTION              =  20; #
our $FM_CM_PORT_CFG_b_SMP0_RL_ACTION              =  16; #
our $FM_CM_PORT_CFG_b_PAUSE_TYPE                  =  3; #
our $FM_CM_PORT_CFG_b_SMP1_RL_PAUSE               =  20; #
our $FM_CM_PORT_CFG_l_DECIMATOR                   =  0; #
our $FM_CM_PORT_CFG_h_DECIMATOR                   =  2; #
our $FM_CM_PORT_CFG_b_SMP0_RL_PAUSE               =  16; #

our $FM_CM_RX_USAGE_l_count                       =  0; #
our $FM_CM_RX_USAGE_h_count                       =  13; #

our $FM_CM_SHARED_WM_l_watermark                  =  0; #
our $FM_CM_SHARED_WM_h_watermark                  =  12; #

our $FM_CM_PAUSE_DECIMATION_l_K                   =  0; #
our $FM_CM_PAUSE_DECIMATION_h_K                   =  13; #
our $FM_CM_PAUSE_DECIMATION_l_M                   =  19; #
our $FM_CM_PAUSE_DECIMATION_h_M                   =  31; #
our $FM_CM_PAUSE_DECIMATION_l_CP                  =  14; #
our $FM_CM_PAUSE_DECIMATION_h_CP                  =  18; #
our $FM_CM_PAUSE_DECIMATION_l_UP                  =  0; #
our $FM_CM_PAUSE_DECIMATION_h_UP                  =  13; #
our $FM_CM_PAUSE_DECIMATION_l_N                   =  14; #
our $FM_CM_PAUSE_DECIMATION_h_N                   =  18; #
our $FM_CM_PAUSE_DECIMATION_l_CV                  =  19; #
our $FM_CM_PAUSE_DECIMATION_h_CV                  =  31; #

our $FM_CM_SHARED_SMP_PAUSE_WM_l_PauseOff         =  16; #
our $FM_CM_SHARED_SMP_PAUSE_WM_h_PauseOff         =  28; #
our $FM_CM_SHARED_SMP_PAUSE_WM_l_PauseOn          =  0; #
our $FM_CM_SHARED_SMP_PAUSE_WM_h_PauseOn          =  12; #

our $FM_CM_SHARED_SMP_USAGE_l_count               =  0; #
our $FM_CM_SHARED_SMP_USAGE_h_count               =  13; #

our $FM_CM_GLOBAL_USAGE_l_count                   =  0; #
our $FM_CM_GLOBAL_USAGE_h_count                   =  13; #

our $FM_CM_GLOBAL_WM_l_watermark                  =  0; #
our $FM_CM_GLOBAL_WM_h_watermark                  =  12; #

our $FM_CM_SMP_MEMBERSHIP_l_tc4                   =  16; #
our $FM_CM_SMP_MEMBERSHIP_h_tc4                   =  19; #
our $FM_CM_SMP_MEMBERSHIP_l_tc5                   =  20; #
our $FM_CM_SMP_MEMBERSHIP_h_tc5                   =  23; #
our $FM_CM_SMP_MEMBERSHIP_l_tc0                   =  0; #
our $FM_CM_SMP_MEMBERSHIP_h_tc0                   =  3; #
our $FM_CM_SMP_MEMBERSHIP_l_tc2                   =  8; #
our $FM_CM_SMP_MEMBERSHIP_h_tc2                   =  11; #
our $FM_CM_SMP_MEMBERSHIP_l_tc3                   =  12; #
our $FM_CM_SMP_MEMBERSHIP_h_tc3                   =  15; #
our $FM_CM_SMP_MEMBERSHIP_l_tc1                   =  4; #
our $FM_CM_SMP_MEMBERSHIP_h_tc1                   =  7; #
our $FM_CM_SMP_MEMBERSHIP_l_tc7                   =  28; #
our $FM_CM_SMP_MEMBERSHIP_h_tc7                   =  31; #
our $FM_CM_SMP_MEMBERSHIP_l_tc6                   =  24; #
our $FM_CM_SMP_MEMBERSHIP_h_tc6                   =  27; #

our $FM_CM_TX_HOG_MAP_l_tc4                       =  8; #
our $FM_CM_TX_HOG_MAP_h_tc4                       =  9; #
our $FM_CM_TX_HOG_MAP_l_tc5                       =  10; #
our $FM_CM_TX_HOG_MAP_h_tc5                       =  11; #
our $FM_CM_TX_HOG_MAP_l_tc0                       =  0; #
our $FM_CM_TX_HOG_MAP_h_tc0                       =  1; #
our $FM_CM_TX_HOG_MAP_l_tc2                       =  4; #
our $FM_CM_TX_HOG_MAP_h_tc2                       =  5; #
our $FM_CM_TX_HOG_MAP_l_tc3                       =  6; #
our $FM_CM_TX_HOG_MAP_h_tc3                       =  7; #
our $FM_CM_TX_HOG_MAP_l_tc1                       =  2; #
our $FM_CM_TX_HOG_MAP_h_tc1                       =  3; #
our $FM_CM_TX_HOG_MAP_l_tc7                       =  14; #
our $FM_CM_TX_HOG_MAP_h_tc7                       =  15; #
our $FM_CM_TX_HOG_MAP_l_tc6                       =  12; #
our $FM_CM_TX_HOG_MAP_h_tc6                       =  13; #

our $FM_CN_CPID_MASK_l_Mask                       =  0; #
our $FM_CN_CPID_MASK_h_Mask                       =  15; #

our $FM_CN_VCN_DMAC_2_l_MAC                       =  0; #
our $FM_CN_VCN_DMAC_2_h_MAC                       =  15; #

our $FM_CN_RATE_LIM_CPID_l_index                  =  0; #
our $FM_CN_RATE_LIM_CPID_h_index                  =  3; #

our $FM_CN_SMP_CFG_l_EQ                           =  0; #
our $FM_CN_SMP_CFG_h_EQ                           =  11; #
our $FM_CN_SMP_CFG_l_SC                           =  16; #
our $FM_CN_SMP_CFG_h_SC                           =  27; #

our $FM_CN_SMP_THRESHOLD_l_Min                    =  0; #
our $FM_CN_SMP_THRESHOLD_h_Min                    =  11; #
our $FM_CN_SMP_THRESHOLD_l_Max                    =  16; #
our $FM_CN_SMP_THRESHOLD_h_Max                    =  27; #

our $FM_CN_SAMPLE_CFG_l_SampleJitter              =  16; #
our $FM_CN_SAMPLE_CFG_h_SampleJitter              =  19; #
our $FM_CN_SAMPLE_CFG_l_SampleMin                 =  0; #
our $FM_CN_SAMPLE_CFG_h_SampleMin                 =  15; #

our $FM_CN_RATE_LIM_CFG_l_MaxFrac                 =  16; #
our $FM_CN_RATE_LIM_CFG_h_MaxFrac                 =  23; #
our $FM_CN_RATE_LIM_CFG_l_Min                     =  8; #
our $FM_CN_RATE_LIM_CFG_h_Min                     =  15; #
our $FM_CN_RATE_LIM_CFG_l_Unit                    =  0; #
our $FM_CN_RATE_LIM_CFG_h_Unit                    =  7; #
our $FM_CN_RATE_LIM_CFG_l_Max                     =  24; #
our $FM_CN_RATE_LIM_CFG_h_Max                     =  30; #

our $FM_CN_CPID_TABLE_l_CPID                      =  0; #
our $FM_CN_CPID_TABLE_h_CPID                      =  63; #

our $FM_CN_GLOBAL_CFG_1_l_EtherType               =  16; #
our $FM_CN_GLOBAL_CFG_1_h_EtherType               =  31; #
our $FM_CN_GLOBAL_CFG_1_l_SamplePeriod            =  2; #
our $FM_CN_GLOBAL_CFG_1_h_SamplePeriod            =  11; #
our $FM_CN_GLOBAL_CFG_1_l_CongestionMode          =  0; #
our $FM_CN_GLOBAL_CFG_1_h_CongestionMode          =  1; #
our $FM_CN_GLOBAL_CFG_1_l_Mode                    =  0; #
our $FM_CN_GLOBAL_CFG_1_h_Mode                    =  1; #

our $FM_CN_GLOBAL_CFG_2_b_cpidCache               =  16; #
our $FM_CN_GLOBAL_CFG_2_b_ignoreMatch             =  25; #
our $FM_CN_GLOBAL_CFG_2_b_localReaction           =  22; #
our $FM_CN_GLOBAL_CFG_2_b_fbQDelta                =  19; #
our $FM_CN_GLOBAL_CFG_2_l_CNSrcGlort              =  0; #
our $FM_CN_GLOBAL_CFG_2_h_CNSrcGlort              =  15; #
our $FM_CN_GLOBAL_CFG_2_b_rlDecrease              =  18; #
our $FM_CN_GLOBAL_CFG_2_b_rlIncrease              =  17; #
our $FM_CN_GLOBAL_CFG_2_b_reqFwd                  =  26; #
our $FM_CN_GLOBAL_CFG_2_l_backoffMode             =  20; #
our $FM_CN_GLOBAL_CFG_2_h_backoffMode             =  21; #
our $FM_CN_GLOBAL_CFG_2_b_qEqSaturate             =  24; #
our $FM_CN_GLOBAL_CFG_2_b_sendSample              =  23; #

our $FM_CN_FB_CFG_l_DriftMultExp                  =  0; #
our $FM_CN_FB_CFG_h_DriftMultExp                  =  4; #
our $FM_CN_FB_CFG_l_BCN_GI                        =  24; #
our $FM_CN_FB_CFG_h_BCN_GI                        =  29; #
our $FM_CN_FB_CFG_l_FCN_BF                        =  24; #
our $FM_CN_FB_CFG_h_FCN_BF                        =  27; #
our $FM_CN_FB_CFG_l_BCN_GD                        =  16; #
our $FM_CN_FB_CFG_h_BCN_GD                        =  21; #
our $FM_CN_FB_CFG_l_BCN_W                         =  8; #
our $FM_CN_FB_CFG_h_BCN_W                         =  9; #
our $FM_CN_FB_CFG_l_BCN_DG                        =  16; #
our $FM_CN_FB_CFG_h_BCN_DG                        =  21; #

our $FM_CN_FORWARD_MASK_l_Mask                    =  0; #
our $FM_CN_FORWARD_MASK_h_Mask                    =  24; #

our $FM_CN_RATE_ACTION_MASK_l_Mask                =  0; #
our $FM_CN_RATE_ACTION_MASK_h_Mask                =  24; #

our $FM_CN_BACKOFF_BYTETIME_l_RandShift           =  0; #
our $FM_CN_BACKOFF_BYTETIME_h_RandShift           =  2; #
our $FM_CN_BACKOFF_BYTETIME_l_Time                =  0; #
our $FM_CN_BACKOFF_BYTETIME_h_Time                =  22; #

our $FM_CN_FRAME_CFG_1_l_MA                       =  0; #
our $FM_CN_FRAME_CFG_1_h_MA                       =  31; #
our $FM_CN_FRAME_CFG_1_l_MA_CPID                  =  0; #
our $FM_CN_FRAME_CFG_1_h_MA_CPID                  =  31; #

our $FM_CN_FRAME_CFG_2_l_MA                       =  0; #
our $FM_CN_FRAME_CFG_2_h_MA                       =  15; #
our $FM_CN_FRAME_CFG_2_l_VLAN                     =  16; #
our $FM_CN_FRAME_CFG_2_h_VLAN                     =  27; #
our $FM_CN_FRAME_CFG_2_l_MA_CPID                  =  0; #
our $FM_CN_FRAME_CFG_2_h_MA_CPID                  =  15; #
our $FM_CN_FRAME_CFG_2_l_Priority                 =  28; #
our $FM_CN_FRAME_CFG_2_h_Priority                 =  31; #

our $FM_CM_CPU_PORT_STATUS_l_Congestion           =  0; #
our $FM_CM_CPU_PORT_STATUS_h_Congestion           =  7; #

our $FM_CN_VCN_DMAC_1_l_MAC                       =  0; #
our $FM_CN_VCN_DMAC_1_h_MAC                       =  31; #

our $FM_TX_RATE_LIM_CFG_l_Capacity                =  0; #
our $FM_TX_RATE_LIM_CFG_h_Capacity                =  16; #
our $FM_TX_RATE_LIM_CFG_l_RateUnit                =  25; #
our $FM_TX_RATE_LIM_CFG_h_RateUnit                =  31; #
our $FM_TX_RATE_LIM_CFG_l_RateFrac                =  17; #
our $FM_TX_RATE_LIM_CFG_h_RateFrac                =  24; #

our $FM_TX_RATE_LIM_USAGE_l_frac                  =  0; #
our $FM_TX_RATE_LIM_USAGE_h_frac                  =  7; #
our $FM_TX_RATE_LIM_USAGE_l_units                 =  8; #
our $FM_TX_RATE_LIM_USAGE_h_units                 =  31; #

our $FM_RX_RATE_LIM_CFG_l_Capacity                =  0; #
our $FM_RX_RATE_LIM_CFG_h_Capacity                =  16; #
our $FM_RX_RATE_LIM_CFG_l_RateUnit                =  25; #
our $FM_RX_RATE_LIM_CFG_h_RateUnit                =  31; #
our $FM_RX_RATE_LIM_CFG_l_RateFrac                =  17; #
our $FM_RX_RATE_LIM_CFG_h_RateFrac                =  24; #

our $FM_RX_RATE_LIM_USAGE_l_frac                  =  0; #
our $FM_RX_RATE_LIM_USAGE_h_frac                  =  7; #
our $FM_RX_RATE_LIM_USAGE_l_units                 =  8; #
our $FM_RX_RATE_LIM_USAGE_h_units                 =  31; #

our $FM_RX_RATE_LIM_THRESHOLD_l_drop              =  16; #
our $FM_RX_RATE_LIM_THRESHOLD_h_drop              =  31; #
our $FM_RX_RATE_LIM_THRESHOLD_l_off               =  0; #
our $FM_RX_RATE_LIM_THRESHOLD_h_off               =  15; #

our $FM_CM_PORT_LIST_b_last                       =  5; #
our $FM_CM_PORT_LIST_l_port                       =  0; #
our $FM_CM_PORT_LIST_h_port                       =  4; #

our $FM_SCHED_DRR_Q_l_Weight                      =  0; #
our $FM_SCHED_DRR_Q_h_Weight                      =  23; #

our $FM_SCHED_SHAPING_GROUP_CFG_l_pri3            =  12; #
our $FM_SCHED_SHAPING_GROUP_CFG_h_pri3            =  14; #
our $FM_SCHED_SHAPING_GROUP_CFG_l_pri7            =  28; #
our $FM_SCHED_SHAPING_GROUP_CFG_h_pri7            =  30; #
our $FM_SCHED_SHAPING_GROUP_CFG_b_reserved3       =  15; #
our $FM_SCHED_SHAPING_GROUP_CFG_b_reserved1       =  7; #
our $FM_SCHED_SHAPING_GROUP_CFG_l_pri4            =  16; #
our $FM_SCHED_SHAPING_GROUP_CFG_h_pri4            =  18; #
our $FM_SCHED_SHAPING_GROUP_CFG_b_reserved6       =  27; #
our $FM_SCHED_SHAPING_GROUP_CFG_l_pri0            =  0; #
our $FM_SCHED_SHAPING_GROUP_CFG_h_pri0            =  2; #
our $FM_SCHED_SHAPING_GROUP_CFG_b_reserved4       =  19; #
our $FM_SCHED_SHAPING_GROUP_CFG_b_reserved0       =  3; #
our $FM_SCHED_SHAPING_GROUP_CFG_b_reserved7       =  31; #
our $FM_SCHED_SHAPING_GROUP_CFG_b_reserved5       =  23; #
our $FM_SCHED_SHAPING_GROUP_CFG_l_pri5            =  20; #
our $FM_SCHED_SHAPING_GROUP_CFG_h_pri5            =  22; #
our $FM_SCHED_SHAPING_GROUP_CFG_l_pri1            =  4; #
our $FM_SCHED_SHAPING_GROUP_CFG_h_pri1            =  6; #
our $FM_SCHED_SHAPING_GROUP_CFG_l_pri6            =  24; #
our $FM_SCHED_SHAPING_GROUP_CFG_h_pri6            =  26; #
our $FM_SCHED_SHAPING_GROUP_CFG_b_reserved2       =  11; #
our $FM_SCHED_SHAPING_GROUP_CFG_l_pri2            =  8; #
our $FM_SCHED_SHAPING_GROUP_CFG_h_pri2            =  10; #

our $FM_SWITCH_PRI_TO_CLASS_1_l_pri3              =  9; #
our $FM_SWITCH_PRI_TO_CLASS_1_h_pri3              =  11; #
our $FM_SWITCH_PRI_TO_CLASS_1_l_pri7              =  21; #
our $FM_SWITCH_PRI_TO_CLASS_1_h_pri7              =  23; #
our $FM_SWITCH_PRI_TO_CLASS_1_l_pri5              =  15; #
our $FM_SWITCH_PRI_TO_CLASS_1_h_pri5              =  17; #
our $FM_SWITCH_PRI_TO_CLASS_1_l_pri1              =  3; #
our $FM_SWITCH_PRI_TO_CLASS_1_h_pri1              =  5; #
our $FM_SWITCH_PRI_TO_CLASS_1_l_pri4              =  12; #
our $FM_SWITCH_PRI_TO_CLASS_1_h_pri4              =  14; #
our $FM_SWITCH_PRI_TO_CLASS_1_l_pri6              =  18; #
our $FM_SWITCH_PRI_TO_CLASS_1_h_pri6              =  20; #
our $FM_SWITCH_PRI_TO_CLASS_1_l_pri0              =  0; #
our $FM_SWITCH_PRI_TO_CLASS_1_h_pri0              =  2; #
our $FM_SWITCH_PRI_TO_CLASS_1_l_pri2              =  6; #
our $FM_SWITCH_PRI_TO_CLASS_1_h_pri2              =  8; #

our $FM_SWITCH_PRI_TO_CLASS_2_l_pri12             =  12; #
our $FM_SWITCH_PRI_TO_CLASS_2_h_pri12             =  14; #
our $FM_SWITCH_PRI_TO_CLASS_2_l_pri9              =  3; #
our $FM_SWITCH_PRI_TO_CLASS_2_h_pri9              =  5; #
our $FM_SWITCH_PRI_TO_CLASS_2_l_pri10             =  6; #
our $FM_SWITCH_PRI_TO_CLASS_2_h_pri10             =  8; #
our $FM_SWITCH_PRI_TO_CLASS_2_l_pri15             =  21; #
our $FM_SWITCH_PRI_TO_CLASS_2_h_pri15             =  23; #
our $FM_SWITCH_PRI_TO_CLASS_2_l_pri8              =  0; #
our $FM_SWITCH_PRI_TO_CLASS_2_h_pri8              =  2; #
our $FM_SWITCH_PRI_TO_CLASS_2_l_pri13             =  15; #
our $FM_SWITCH_PRI_TO_CLASS_2_h_pri13             =  17; #
our $FM_SWITCH_PRI_TO_CLASS_2_l_pri11             =  9; #
our $FM_SWITCH_PRI_TO_CLASS_2_h_pri11             =  11; #
our $FM_SWITCH_PRI_TO_CLASS_2_l_pri14             =  18; #
our $FM_SWITCH_PRI_TO_CLASS_2_h_pri14             =  20; #

our $FM_CM_GLOBAL_CFG_l_sharedPauseInterval       =  8; #
our $FM_CM_GLOBAL_CFG_h_sharedPauseInterval       =  15; #
our $FM_CM_GLOBAL_CFG_b_forcePauseOn              =  16; #
our $FM_CM_GLOBAL_CFG_b_forcePauseOff             =  17; #
our $FM_CM_GLOBAL_CFG_l_ifgPenalty                =  0; #
our $FM_CM_GLOBAL_CFG_h_ifgPenalty                =  7; #

our $FM_TRAFFIC_CLASS_TO_SCHED_PRI_l_pri3         =  12; #
our $FM_TRAFFIC_CLASS_TO_SCHED_PRI_h_pri3         =  14; #
our $FM_TRAFFIC_CLASS_TO_SCHED_PRI_b_reserved7    =  31; #
our $FM_TRAFFIC_CLASS_TO_SCHED_PRI_l_pri7         =  28; #
our $FM_TRAFFIC_CLASS_TO_SCHED_PRI_h_pri7         =  30; #
our $FM_TRAFFIC_CLASS_TO_SCHED_PRI_l_pri5         =  20; #
our $FM_TRAFFIC_CLASS_TO_SCHED_PRI_h_pri5         =  22; #
our $FM_TRAFFIC_CLASS_TO_SCHED_PRI_l_pri1         =  4; #
our $FM_TRAFFIC_CLASS_TO_SCHED_PRI_h_pri1         =  6; #
our $FM_TRAFFIC_CLASS_TO_SCHED_PRI_l_pri4         =  16; #
our $FM_TRAFFIC_CLASS_TO_SCHED_PRI_h_pri4         =  18; #
our $FM_TRAFFIC_CLASS_TO_SCHED_PRI_l_pri6         =  24; #
our $FM_TRAFFIC_CLASS_TO_SCHED_PRI_h_pri6         =  26; #
our $FM_TRAFFIC_CLASS_TO_SCHED_PRI_l_pri0         =  0; #
our $FM_TRAFFIC_CLASS_TO_SCHED_PRI_h_pri0         =  2; #
our $FM_TRAFFIC_CLASS_TO_SCHED_PRI_l_pri2         =  8; #
our $FM_TRAFFIC_CLASS_TO_SCHED_PRI_h_pri2         =  10; #

our $FM_CN_STATS_CFG_l_tx                         =  0; #
our $FM_CN_STATS_CFG_h_tx                         =  4; #
our $FM_CN_STATS_CFG_b_smp                        =  8; #
our $FM_CN_STATS_CFG_b_tc                         =  8; #

our $FM_CN_STATS_l_Count                          =  0; #
our $FM_CN_STATS_h_Count                          =  31; #

our $FM_SCHED_FH_GROUP_CFG_l_StrictPriority       =  16; #
our $FM_SCHED_FH_GROUP_CFG_h_StrictPriority       =  23; #
our $FM_SCHED_FH_GROUP_CFG_l_SchedulingGroupBoundary  =  0; #
our $FM_SCHED_FH_GROUP_CFG_h_SchedulingGroupBoundary  =  7; #
our $FM_SCHED_FH_GROUP_CFG_l_Unimplemented        =  8; #
our $FM_SCHED_FH_GROUP_CFG_h_Unimplemented        =  22; #

our $FM_QDM_CFG_l_tx                              =  0; #
our $FM_QDM_CFG_h_tx                              =  4; #
our $FM_QDM_CFG_l_w                               =  10; #
our $FM_QDM_CFG_h_w                               =  13; #
our $FM_QDM_CFG_b_mode                            =  14; #
our $FM_QDM_CFG_l_tc                              =  6; #
our $FM_QDM_CFG_h_tc                              =  8; #

our $FM_QDM_FRAME_CNT_l_cnt                       =  0; #
our $FM_QDM_FRAME_CNT_h_cnt                       =  15; #

our $FM_QDM_INSTRUMENT_b_spill                    =  31; #
our $FM_QDM_INSTRUMENT_l_ts                       =  0; #
our $FM_QDM_INSTRUMENT_h_ts                       =  30; #

our $FM_LAG_CFG_l_Index                           =  4; #
our $FM_LAG_CFG_h_Index                           =  7; #
our $FM_LAG_CFG_b_InLAG                           =  9; #
our $FM_LAG_CFG_b_HashRotation                    =  8; #
our $FM_LAG_CFG_l_LagSize                         =  0; #
our $FM_LAG_CFG_h_LagSize                         =  3; #

our $FM_CANONICAL_GLORT_CAM_l_LagGlort            =  0; #
our $FM_CANONICAL_GLORT_CAM_h_LagGlort            =  15; #
our $FM_CANONICAL_GLORT_CAM_l_MaskSize            =  16; #
our $FM_CANONICAL_GLORT_CAM_h_MaskSize            =  19; #
our $FM_CANONICAL_GLORT_CAM_l_PortFieldSize       =  20; #
our $FM_CANONICAL_GLORT_CAM_h_PortFieldSize       =  22; #

our $FM_TRIGGER_CONDITION_CFG_l_MatchEtherType    =  16; #
our $FM_TRIGGER_CONDITION_CFG_h_MatchEtherType    =  17; #
our $FM_TRIGGER_CONDITION_CFG_l_MatchHitSADA      =  8; #
our $FM_TRIGGER_CONDITION_CFG_h_MatchHitSADA      =  9; #
our $FM_TRIGGER_CONDITION_CFG_l_MatchDA           =  2; #
our $FM_TRIGGER_CONDITION_CFG_h_MatchDA           =  3; #
our $FM_TRIGGER_CONDITION_CFG_l_MatchSwitchPri    =  14; #
our $FM_TRIGGER_CONDITION_CFG_h_MatchSwitchPri    =  15; #
our $FM_TRIGGER_CONDITION_CFG_l_MatchHitDA        =  6; #
our $FM_TRIGGER_CONDITION_CFG_h_MatchHitDA        =  7; #
our $FM_TRIGGER_CONDITION_CFG_b_MatchByPrecedence  =  20; #
our $FM_TRIGGER_CONDITION_CFG_l_MatchSA           =  0; #
our $FM_TRIGGER_CONDITION_CFG_h_MatchSA           =  1; #
our $FM_TRIGGER_CONDITION_CFG_l_MatchVlan         =  10; #
our $FM_TRIGGER_CONDITION_CFG_h_MatchVlan         =  11; #
our $FM_TRIGGER_CONDITION_CFG_l_MatchFFU          =  12; #
our $FM_TRIGGER_CONDITION_CFG_h_MatchFFU          =  13; #
our $FM_TRIGGER_CONDITION_CFG_l_MatchDestGlort    =  18; #
our $FM_TRIGGER_CONDITION_CFG_h_MatchDestGlort    =  19; #
our $FM_TRIGGER_CONDITION_CFG_b_MatchRandomNumber  =  21; #
our $FM_TRIGGER_CONDITION_CFG_b_MatchRandomIfLess  =  22; #
our $FM_TRIGGER_CONDITION_CFG_l_MatchHitSA        =  4; #
our $FM_TRIGGER_CONDITION_CFG_h_MatchHitSA        =  5; #
our $FM_TRIGGER_CONDITION_CFG_l_MatchRandomThreshold  =  23; #
our $FM_TRIGGER_CONDITION_CFG_h_MatchRandomThreshold  =  27; #

our $FM_TRIGGER_CONDITION_PARAM_l_VID_ID          =  12; #
our $FM_TRIGGER_CONDITION_PARAM_h_VID_ID          =  17; #
our $FM_TRIGGER_CONDITION_PARAM_b_MatchDroppedFrames  =  31; #
our $FM_TRIGGER_CONDITION_PARAM_l_FrameClassMask  =  22; #
our $FM_TRIGGER_CONDITION_PARAM_h_FrameClassMask  =  24; #
our $FM_TRIGGER_CONDITION_PARAM_l_FtypeMask       =  27; #
our $FM_TRIGGER_CONDITION_PARAM_h_FtypeMask       =  30; #
our $FM_TRIGGER_CONDITION_PARAM_l_DA_ID           =  6; #
our $FM_TRIGGER_CONDITION_PARAM_h_DA_ID           =  11; #
our $FM_TRIGGER_CONDITION_PARAM_l_SwitchPri       =  18; #
our $FM_TRIGGER_CONDITION_PARAM_h_SwitchPri       =  21; #
our $FM_TRIGGER_CONDITION_PARAM_l_SA_ID           =  0; #
our $FM_TRIGGER_CONDITION_PARAM_h_SA_ID           =  5; #
our $FM_TRIGGER_CONDITION_PARAM_l_RoutedMask      =  25; #
our $FM_TRIGGER_CONDITION_PARAM_h_RoutedMask      =  26; #

our $FM_TRIGGER_CONDITION_FFU_l_FFU_Mask          =  8; #
our $FM_TRIGGER_CONDITION_FFU_h_FFU_Mask          =  15; #
our $FM_TRIGGER_CONDITION_FFU_l_FFU_ID            =  0; #
our $FM_TRIGGER_CONDITION_FFU_h_FFU_ID            =  7; #

our $FM_TRIGGER_CONDITION_TYPE_l_EtherType        =  0; #
our $FM_TRIGGER_CONDITION_TYPE_h_EtherType        =  15; #
our $FM_TRIGGER_CONDITION_TYPE_l_EtherTypeMask    =  16; #
our $FM_TRIGGER_CONDITION_TYPE_h_EtherTypeMask    =  31; #

our $FM_TRIGGER_CONDITION_GLORT_l_DestGlort       =  0; #
our $FM_TRIGGER_CONDITION_GLORT_h_DestGlort       =  15; #
our $FM_TRIGGER_CONDITION_GLORT_l_GlortMask       =  16; #
our $FM_TRIGGER_CONDITION_GLORT_h_GlortMask       =  31; #

our $FM_TRIGGER_CONDITION_RX_l_SrcPortMask        =  0; #
our $FM_TRIGGER_CONDITION_RX_h_SrcPortMask        =  24; #

our $FM_TRIGGER_CONDITION_TX_l_DestPortMask       =  0; #
our $FM_TRIGGER_CONDITION_TX_h_DestPortMask       =  24; #

our $FM_TRIGGER_CONDITION_AMASK_1_l_HandlerActionMask  =  0; #
our $FM_TRIGGER_CONDITION_AMASK_1_h_HandlerActionMask  =  31; #

our $FM_TRIGGER_CONDITION_AMASK_2_l_HandlerActionMask  =  0; #
our $FM_TRIGGER_CONDITION_AMASK_2_h_HandlerActionMask  =  12; #

our $FM_TRIGGER_ACTION_CFG_1_b_SetActionBit       =  11; #
our $FM_TRIGGER_ACTION_CFG_1_b_VlanAction         =  6; #
our $FM_TRIGGER_ACTION_CFG_1_b_ClearActionBit     =  10; #
our $FM_TRIGGER_ACTION_CFG_1_l_TrapAction         =  2; #
our $FM_TRIGGER_ACTION_CFG_1_h_TrapAction         =  3; #
our $FM_TRIGGER_ACTION_CFG_1_l_ForwardingAction   =  0; #
our $FM_TRIGGER_ACTION_CFG_1_h_ForwardingAction   =  1; #
our $FM_TRIGGER_ACTION_CFG_1_b_MirroringAction    =  4; #
our $FM_TRIGGER_ACTION_CFG_1_l_LearningAction     =  7; #
our $FM_TRIGGER_ACTION_CFG_1_h_LearningAction     =  8; #
our $FM_TRIGGER_ACTION_CFG_1_b_RateLimitAction    =  9; #
our $FM_TRIGGER_ACTION_CFG_1_b_SwitchPriAction    =  5; #

our $FM_TRIGGER_ACTION_CFG_2_l_NewSwitchPri       =  2; #
our $FM_TRIGGER_ACTION_CFG_2_h_NewSwitchPri       =  5; #
our $FM_TRIGGER_ACTION_CFG_2_l_NewVlan            =  6; #
our $FM_TRIGGER_ACTION_CFG_2_h_NewVlan            =  17; #
our $FM_TRIGGER_ACTION_CFG_2_b_CpuTruncate        =  0; #
our $FM_TRIGGER_ACTION_CFG_2_l_RateLimitNum       =  18; #
our $FM_TRIGGER_ACTION_CFG_2_h_RateLimitNum       =  21; #
our $FM_TRIGGER_ACTION_CFG_2_b_MirrorTruncate     =  1; #

our $FM_TRIGGER_ACTION_GLORT_l_NewDestGlortMask   =  16; #
our $FM_TRIGGER_ACTION_GLORT_h_NewDestGlortMask   =  31; #
our $FM_TRIGGER_ACTION_GLORT_l_NewDestGlort       =  0; #
our $FM_TRIGGER_ACTION_GLORT_h_NewDestGlort       =  15; #

our $FM_TRIGGER_ACTION_DMASK_l_NewDestMask        =  0; #
our $FM_TRIGGER_ACTION_DMASK_h_NewDestMask        =  24; #
our $FM_TRIGGER_ACTION_DMASK_b_FilterDestMask     =  25; #

our $FM_TRIGGER_ACTION_MIRROR_l_MirrorPort        =  0; #
our $FM_TRIGGER_ACTION_MIRROR_h_MirrorPort        =  4; #
our $FM_TRIGGER_ACTION_MIRROR_l_MirrorGlort       =  5; #
our $FM_TRIGGER_ACTION_MIRROR_h_MirrorGlort       =  20; #

our $FM_TRIGGER_ACTION_DROP_l_DropMask            =  0; #
our $FM_TRIGGER_ACTION_DROP_h_DropMask            =  24; #

our $FM_TRIGGER_RATE_LIM_CFG_1_l_Capacity         =  0; #
our $FM_TRIGGER_RATE_LIM_CFG_1_h_Capacity         =  11; #
our $FM_TRIGGER_RATE_LIM_CFG_1_l_RateMantissa     =  12; #
our $FM_TRIGGER_RATE_LIM_CFG_1_h_RateMantissa     =  23; #
our $FM_TRIGGER_RATE_LIM_CFG_1_l_RateExponent     =  24; #
our $FM_TRIGGER_RATE_LIM_CFG_1_h_RateExponent     =  25; #

our $FM_TRIGGER_RATE_LIM_CFG_2_l_DropMask         =  0; #
our $FM_TRIGGER_RATE_LIM_CFG_2_h_DropMask         =  24; #

our $FM_TRIGGER_IP_l_Pending                      =  0; #
our $FM_TRIGGER_IP_h_Pending                      =  31; #

our $FM_TRIGGER_IM_l_Mask                         =  0; #
our $FM_TRIGGER_IM_h_Mask                         =  31; #

our $FM_TRIGGER_RATE_LIM_USAGE_l_Level            =  0; #
our $FM_TRIGGER_RATE_LIM_USAGE_h_Level            =  26; #

our $FM_GLORT_DEST_TABLE_l_DestMask               =  1; #
our $FM_GLORT_DEST_TABLE_h_DestMask               =  25; #
our $FM_GLORT_DEST_TABLE_b_ParityError            =  0; #
our $FM_GLORT_DEST_TABLE_l_IP_MulticastIndex      =  26; #
our $FM_GLORT_DEST_TABLE_h_IP_MulticastIndex      =  39; #

our $FM_GLORT_CAM_l_value                         =  0; #
our $FM_GLORT_CAM_h_value                         =  15; #
our $FM_GLORT_CAM_l_mask                          =  16; #
our $FM_GLORT_CAM_h_mask                          =  31; #

our $FM_GLORT_RAM_l_RangeSubIndexB                =  23; #
our $FM_GLORT_RAM_h_RangeSubIndexB                =  30; #
our $FM_GLORT_RAM_l_DestCount                     =  31; #
our $FM_GLORT_RAM_h_DestCount                     =  34; #
our $FM_GLORT_RAM_l_DestIndex                     =  3; #
our $FM_GLORT_RAM_h_DestIndex                     =  14; #
our $FM_GLORT_RAM_b_ParityError                   =  0; #
our $FM_GLORT_RAM_l_RangeSubIndexA                =  15; #
our $FM_GLORT_RAM_h_RangeSubIndexA                =  22; #
our $FM_GLORT_RAM_b_HashRotation                  =  35; #
our $FM_GLORT_RAM_l_Strict                        =  1; #
our $FM_GLORT_RAM_h_Strict                        =  2; #

our $FM_GLORT_RAM_TABLE_REPAIR_l_index            =  0; #
our $FM_GLORT_RAM_TABLE_REPAIR_h_index            =  7; #

our $FM_GLORT_DEST_TABLE_REPAIR_l_index           =  0; #
our $FM_GLORT_DEST_TABLE_REPAIR_h_index           =  11; #

our $FM_POLICER_TABLE_l_PolicerExcessIndex        =  94; #
our $FM_POLICER_TABLE_h_PolicerExcessIndex        =  101; #
our $FM_POLICER_TABLE_l_PolicerExcessRateExponent  =  102; #
our $FM_POLICER_TABLE_h_PolicerExcessRateExponent  =  105; #
our $FM_POLICER_TABLE_l_FrameCount                =  0; #
our $FM_POLICER_TABLE_h_FrameCount                =  63; #
our $FM_POLICER_TABLE_l_PolicerTimestamp          =  106; #
our $FM_POLICER_TABLE_h_PolicerTimestamp          =  121; #
our $FM_POLICER_TABLE_b_PolicerCommittedAction    =  55; #
our $FM_POLICER_TABLE_l_PolicerExcessLimit        =  81; #
our $FM_POLICER_TABLE_h_PolicerExcessLimit        =  92; #
our $FM_POLICER_TABLE_l_PolicerCommittedRateMantissa  =  56; #
our $FM_POLICER_TABLE_h_PolicerCommittedRateMantissa  =  63; #
our $FM_POLICER_TABLE_l_PolicerCommittedIndex     =  56; #
our $FM_POLICER_TABLE_h_PolicerCommittedIndex     =  63; #
our $FM_POLICER_TABLE_l_ByteCount                 =  64; #
our $FM_POLICER_TABLE_h_ByteCount                 =  127; #
our $FM_POLICER_TABLE_l_PolicerExcessCurrent      =  28; #
our $FM_POLICER_TABLE_h_PolicerExcessCurrent      =  54; #
our $FM_POLICER_TABLE_l_PolicerCommittedLimit     =  68; #
our $FM_POLICER_TABLE_h_PolicerCommittedLimit     =  80; #
our $FM_POLICER_TABLE_l_PolicerCommittedCurrent   =  0; #
our $FM_POLICER_TABLE_h_PolicerCommittedCurrent   =  27; #
our $FM_POLICER_TABLE_b_PolicerExcessAction       =  93; #
our $FM_POLICER_TABLE_l_PolicerCommittedRateExponent  =  64; #
our $FM_POLICER_TABLE_h_PolicerCommittedRateExponent  =  67; #
our $FM_POLICER_TABLE_l_PolicerExcessRateMantissa  =  94; #
our $FM_POLICER_TABLE_h_PolicerExcessRateMantissa  =  101; #

our $FM_POLICER_REPAIR_l_REPAIR_ADDR              =  0; #
our $FM_POLICER_REPAIR_h_REPAIR_ADDR              =  3; #

our $FM_POLICER_CFG_b_MarkDSCP                    =  22; #
our $FM_POLICER_CFG_l_IngressColorSource          =  20; #
our $FM_POLICER_CFG_h_IngressColorSource          =  21; #
our $FM_POLICER_CFG_b_TimestampWriteEnable        =  24; #
our $FM_POLICER_CFG_b_MarkSwitchPri               =  23; #
our $FM_POLICER_CFG_l_IndexLastCountNoInt         =  10; #
our $FM_POLICER_CFG_h_IndexLastCountNoInt         =  19; #
our $FM_POLICER_CFG_l_IndexLastPolicer            =  0; #
our $FM_POLICER_CFG_h_IndexLastPolicer            =  9; #

our $FM_POLICER_IP_l_InterruptPending             =  0; #
our $FM_POLICER_IP_h_InterruptPending             =  3; #

our $FM_POLICER_IM_l_InterruptMask                =  0; #
our $FM_POLICER_IM_h_InterruptMask                =  3; #

our $FM_POLICER_STATUS_l_Timestamp                =  0; #
our $FM_POLICER_STATUS_h_Timestamp                =  15; #

our $FM_POLICER_TS_DIV_l_div                      =  0; #
our $FM_POLICER_TS_DIV_h_div                      =  3; #

our $FM_POLICER_DSCP_DOWN_MAP_l_NewDSCP           =  0; #
our $FM_POLICER_DSCP_DOWN_MAP_h_NewDSCP           =  5; #

our $FM_POLICER_SWPRI_DOWN_MAP_l_NewSwitchPriority  =  0; #
our $FM_POLICER_SWPRI_DOWN_MAP_h_NewSwitchPriority  =  3; #

our $FM_ARP_TABLE_l_DMAC                          =  0; #
our $FM_ARP_TABLE_h_DMAC                          =  47; #
our $FM_ARP_TABLE_l_type                          =  60; #
our $FM_ARP_TABLE_h_type                          =  61; #
our $FM_ARP_TABLE_l_VRID                          =  64; #
our $FM_ARP_TABLE_h_VRID                          =  71; #
our $FM_ARP_TABLE_l_VLAN                          =  48; #
our $FM_ARP_TABLE_h_VLAN                          =  59; #
our $FM_ARP_TABLE_b_parityError                   =  63; #
our $FM_ARP_TABLE_l_dstGlort                      =  0; #
our $FM_ARP_TABLE_h_dstGlort                      =  15; #
our $FM_ARP_TABLE_l_MTU_Index                     =  16; #
our $FM_ARP_TABLE_h_MTU_Index                     =  18; #
our $FM_ARP_TABLE_b_reserved                      =  62; #

our $FM_ARP_USED_l_used                           =  0; #
our $FM_ARP_USED_h_used                           =  31; #

our $FM_ARP_IP_b_Redirect                         =  0; #

our $FM_ARP_IM_b_Mask                             =  0; #

our $FM_ARP_REDIRECT_SIP_l_SIP                    =  0; #
our $FM_ARP_REDIRECT_SIP_h_SIP                    =  127; #

our $FM_ARP_REDIRECT_DIP_l_SIP                    =  0; #
our $FM_ARP_REDIRECT_DIP_h_SIP                    =  127; #

our $FM_ARP_TABLE_REPAIR_0_l_index                =  0; #
our $FM_ARP_TABLE_REPAIR_0_h_index                =  11; #

our $FM_ARP_TABLE_REPAIR_1_l_index                =  0; #
our $FM_ARP_TABLE_REPAIR_1_h_index                =  11; #

our $FM_ARP_TABLE_REPAIR_2_l_index                =  0; #
our $FM_ARP_TABLE_REPAIR_2_h_index                =  11; #

our $FM_ARP_TABLE_REPAIR_3_l_index                =  0; #
our $FM_ARP_TABLE_REPAIR_3_h_index                =  11; #

our $FM_ARP_USED_REPAIR_l_index                   =  0; #
our $FM_ARP_USED_REPAIR_h_index                   =  8; #

our $FM_FFU_SLICE_TCAM_l_KeyLow                   =  0; #
our $FM_FFU_SLICE_TCAM_h_KeyLow                   =  31; #
our $FM_FFU_SLICE_TCAM_b_Case                     =  73; #
our $FM_FFU_SLICE_TCAM_b_Valid                    =  72; #
our $FM_FFU_SLICE_TCAM_l_MaskLow                  =  32; #
our $FM_FFU_SLICE_TCAM_h_MaskLow                  =  63; #
our $FM_FFU_SLICE_TCAM_l_MaskTop                  =  68; #
our $FM_FFU_SLICE_TCAM_h_MaskTop                  =  71; #
our $FM_FFU_SLICE_TCAM_l_KeyTop                   =  64; #
our $FM_FFU_SLICE_TCAM_h_KeyTop                   =  67; #

our $FM_FFU_SLICE_SRAM_l_DSCP                     =  0; #
our $FM_FFU_SLICE_SRAM_h_DSCP                     =  5; #
our $FM_FFU_SLICE_SRAM_b_SetPRI                   =  19; #
our $FM_FFU_SLICE_SRAM_l_CounterIndex             =  24; #
our $FM_FFU_SLICE_SRAM_h_CounterIndex             =  33; #
our $FM_FFU_SLICE_SRAM_l_ByteData                 =  8; #
our $FM_FFU_SLICE_SRAM_h_ByteData                 =  15; #
our $FM_FFU_SLICE_SRAM_l_Precedence               =  36; #
our $FM_FFU_SLICE_SRAM_h_Precedence               =  38; #
our $FM_FFU_SLICE_SRAM_b_SetDSCP                  =  16; #
our $FM_FFU_SLICE_SRAM_l_SubCommand               =  16; #
our $FM_FFU_SLICE_SRAM_h_SubCommand               =  21; #
our $FM_FFU_SLICE_SRAM_l_VLAN                     =  0; #
our $FM_FFU_SLICE_SRAM_h_VLAN                     =  11; #
our $FM_FFU_SLICE_SRAM_b_ParityError              =  39; #
our $FM_FFU_SLICE_SRAM_l_CounterBank              =  34; #
our $FM_FFU_SLICE_SRAM_h_CounterBank              =  35; #
our $FM_FFU_SLICE_SRAM_l_Command                  =  22; #
our $FM_FFU_SLICE_SRAM_h_Command                  =  23; #
our $FM_FFU_SLICE_SRAM_l_ShortData                =  0; #
our $FM_FFU_SLICE_SRAM_h_ShortData                =  15; #
our $FM_FFU_SLICE_SRAM_b_SetVPRI                  =  18; #
our $FM_FFU_SLICE_SRAM_l_ByteMask                 =  0; #
our $FM_FFU_SLICE_SRAM_h_ByteMask                 =  7; #
our $FM_FFU_SLICE_SRAM_l_RouteData                =  0; #
our $FM_FFU_SLICE_SRAM_h_RouteData                =  21; #
our $FM_FFU_SLICE_SRAM_b_SetVLAN                  =  17; #
our $FM_FFU_SLICE_SRAM_l_PRI                      =  12; #
our $FM_FFU_SLICE_SRAM_h_PRI                      =  15; #

our $FM_FFU_SLICE_VALID_l_Valid                   =  0; #
our $FM_FFU_SLICE_VALID_h_Valid                   =  31; #

our $FM_FFU_SLICE_CASE_l_Case                     =  0; #
our $FM_FFU_SLICE_CASE_h_Case                     =  31; #

our $FM_FFU_SLICE_CASCADE_ACTION_l_CascadeAction  =  0; #
our $FM_FFU_SLICE_CASCADE_ACTION_h_CascadeAction  =  31; #

our $FM_FFU_SLICE_CASE_CFG_l_SelectTop            =  24; #
our $FM_FFU_SLICE_CASE_CFG_h_SelectTop            =  29; #
our $FM_FFU_SLICE_CASE_CFG_l_Select3              =  18; #
our $FM_FFU_SLICE_CASE_CFG_h_Select3              =  23; #
our $FM_FFU_SLICE_CASE_CFG_l_Select0              =  0; #
our $FM_FFU_SLICE_CASE_CFG_h_Select0              =  5; #
our $FM_FFU_SLICE_CASE_CFG_b_StartCompare         =  30; #
our $FM_FFU_SLICE_CASE_CFG_l_Select1              =  6; #
our $FM_FFU_SLICE_CASE_CFG_h_Select1              =  11; #
our $FM_FFU_SLICE_CASE_CFG_l_Select2              =  12; #
our $FM_FFU_SLICE_CASE_CFG_h_Select2              =  17; #
our $FM_FFU_SLICE_CASE_CFG_b_StartAction          =  31; #

our $FM_FFU_MASTER_VALID_l_ChunkValid             =  32; #
our $FM_FFU_MASTER_VALID_h_ChunkValid             =  63; #
our $FM_FFU_MASTER_VALID_l_SliceValid             =  0; #
our $FM_FFU_MASTER_VALID_h_SliceValid             =  31; #

our $FM_FFU_MAP_SRC_l_MAP_SRC                     =  0; #
our $FM_FFU_MAP_SRC_h_MAP_SRC                     =  3; #
our $FM_FFU_MAP_SRC_b_Routable                    =  4; #

our $FM_FFU_MAP_MAC_b_validSMAC                   =  56; #
our $FM_FFU_MAP_MAC_b_Router                      =  62; #
our $FM_FFU_MAP_MAC_b_validDMAC                   =  57; #
our $FM_FFU_MAP_MAC_l_MAP_MAC                     =  58; #
our $FM_FFU_MAP_MAC_h_MAP_MAC                     =  61; #
our $FM_FFU_MAP_MAC_l_IgnoreLength                =  48; #
our $FM_FFU_MAP_MAC_h_IgnoreLength                =  55; #
our $FM_FFU_MAP_MAC_l_MAC                         =  0; #
our $FM_FFU_MAP_MAC_h_MAC                         =  47; #

our $FM_FFU_MAP_VLAN_REPAIR_l_REPAIR_ADDR         =  0; #
our $FM_FFU_MAP_VLAN_REPAIR_h_REPAIR_ADDR         =  11; #

our $FM_FFU_MAP_TYPE_l_TYPE                       =  0; #
our $FM_FFU_MAP_TYPE_h_TYPE                       =  15; #
our $FM_FFU_MAP_TYPE_l_MAP_TYPE                   =  16; #
our $FM_FFU_MAP_TYPE_h_MAP_TYPE                   =  19; #

our $FM_FFU_MAP_LENGTH_l_MAP_LENGTH               =  16; #
our $FM_FFU_MAP_LENGTH_h_MAP_LENGTH               =  19; #
our $FM_FFU_MAP_LENGTH_l_LENGTH                   =  0; #
our $FM_FFU_MAP_LENGTH_h_LENGTH                   =  15; #

our $FM_FFU_MAP_IP_LO_l_IP_LO                     =  0; #
our $FM_FFU_MAP_IP_LO_h_IP_LO                     =  63; #

our $FM_FFU_MAP_IP_HI_l_IP_HI                     =  0; #
our $FM_FFU_MAP_IP_HI_h_IP_HI                     =  63; #

our $FM_FFU_MAP_IP_CFG_b_validDIP                 =  9; #
our $FM_FFU_MAP_IP_CFG_l_MAP_IP                   =  10; #
our $FM_FFU_MAP_IP_CFG_h_MAP_IP                   =  13; #
our $FM_FFU_MAP_IP_CFG_b_validSIP                 =  8; #
our $FM_FFU_MAP_IP_CFG_l_IgnoreLength             =  0; #
our $FM_FFU_MAP_IP_CFG_h_IgnoreLength             =  7; #

our $FM_FFU_MAP_PROT_l_PROT                       =  0; #
our $FM_FFU_MAP_PROT_h_PROT                       =  7; #
our $FM_FFU_MAP_PROT_l_MAP_PROT                   =  8; #
our $FM_FFU_MAP_PROT_h_MAP_PROT                   =  10; #

our $FM_FFU_MAP_L4_SRC_l_L4SRC                    =  0; #
our $FM_FFU_MAP_L4_SRC_h_L4SRC                    =  15; #
our $FM_FFU_MAP_L4_SRC_l_MAP_L4SRC                =  32; #
our $FM_FFU_MAP_L4_SRC_h_MAP_L4SRC                =  47; #
our $FM_FFU_MAP_L4_SRC_l_MAP_PROT                 =  16; #
our $FM_FFU_MAP_L4_SRC_h_MAP_PROT                 =  18; #
our $FM_FFU_MAP_L4_SRC_b_VALID                    =  19; #

our $FM_FFU_MAP_L4_DST_l_MAP_PROT                 =  16; #
our $FM_FFU_MAP_L4_DST_h_MAP_PROT                 =  18; #
our $FM_FFU_MAP_L4_DST_l_L4DST                    =  0; #
our $FM_FFU_MAP_L4_DST_h_L4DST                    =  15; #
our $FM_FFU_MAP_L4_DST_l_MAP_L4DST                =  32; #
our $FM_FFU_MAP_L4_DST_h_MAP_L4DST                =  47; #
our $FM_FFU_MAP_L4_DST_b_VALID                    =  19; #

our $FM_FFU_INIT_SLICE_l_Repair1                  =  6; #
our $FM_FFU_INIT_SLICE_h_Repair1                  =  11; #
our $FM_FFU_INIT_SLICE_l_Repair2                  =  12; #
our $FM_FFU_INIT_SLICE_h_Repair2                  =  17; #
our $FM_FFU_INIT_SLICE_l_ActiveSlices             =  0; #
our $FM_FFU_INIT_SLICE_h_ActiveSlices             =  5; #

our $FM_FFU_MAP_VLAN_l_MAP_VLAN                   =  0; #
our $FM_FFU_MAP_VLAN_h_MAP_VLAN                   =  11; #
our $FM_FFU_MAP_VLAN_b_Routable                   =  12; #
our $FM_FFU_MAP_VLAN_b_ParityError                =  13; #

our $FM_FFU_EGRESS_CHUNK_CFG_b_StartCascade       =  31; #
our $FM_FFU_EGRESS_CHUNK_CFG_l_DstPortMask        =  0; #
our $FM_FFU_EGRESS_CHUNK_CFG_h_DstPortMask        =  24; #

our $FM_FFU_EGRESS_CHUNK_VALID_l_Valid            =  0; #
our $FM_FFU_EGRESS_CHUNK_VALID_h_Valid            =  31; #

our $FM_FFU_EGRESS_ACTIONS_b_Drop                 =  0; #
our $FM_FFU_EGRESS_ACTIONS_b_Log                  =  1; #
our $FM_FFU_EGRESS_ACTIONS_b_Count                =  2; #

our $FM_MA_TABLE_l_destMask                       =  64; #
our $FM_MA_TABLE_h_destMask                       =  88; #
our $FM_MA_TABLE_l_MACAddress                     =  0; #
our $FM_MA_TABLE_h_MACAddress                     =  47; #
our $FM_MA_TABLE_l_destGlort                      =  64; #
our $FM_MA_TABLE_h_destGlort                      =  79; #
our $FM_MA_TABLE_b_parityError                    =  62; #
our $FM_MA_TABLE_l_state                          =  60; #
our $FM_MA_TABLE_h_state                          =  61; #
our $FM_MA_TABLE_l_FID                            =  48; #
our $FM_MA_TABLE_h_FID                            =  59; #
our $FM_MA_TABLE_l_trigId                         =  90; #
our $FM_MA_TABLE_h_trigId                         =  95; #
our $FM_MA_TABLE_b_destType                       =  89; #
our $FM_MA_TABLE_b_reserved                       =  63; #

our $FM_INGRESS_VID_TABLE_b_TrapIGMP              =  77; #
our $FM_INGRESS_VID_TABLE_l_CounterIndex          =  2; #
our $FM_INGRESS_VID_TABLE_h_CounterIndex          =  7; #
our $FM_INGRESS_VID_TABLE_b_reflect               =  1; #
our $FM_INGRESS_VID_TABLE_b_parityError           =  0; #
our $FM_INGRESS_VID_TABLE_l_FID                   =  64; #
our $FM_INGRESS_VID_TABLE_h_FID                   =  75; #
our $FM_INGRESS_VID_TABLE_l_trigId                =  8; #
our $FM_INGRESS_VID_TABLE_h_trigId                =  13; #
our $FM_INGRESS_VID_TABLE_l_membership            =  14; #
our $FM_INGRESS_VID_TABLE_h_membership            =  63; #
our $FM_INGRESS_VID_TABLE_l_vcnt                  =  2; #
our $FM_INGRESS_VID_TABLE_h_vcnt                  =  7; #

our $FM_EGRESS_VID_TABLE_b_parityError            =  0; #
our $FM_EGRESS_VID_TABLE_l_FID                    =  32; #
our $FM_EGRESS_VID_TABLE_h_FID                    =  43; #
our $FM_EGRESS_VID_TABLE_l_TrigID                 =  44; #
our $FM_EGRESS_VID_TABLE_h_TrigID                 =  49; #
our $FM_EGRESS_VID_TABLE_l_MTU_Index              =  1; #
our $FM_EGRESS_VID_TABLE_h_MTU_Index              =  3; #
our $FM_EGRESS_VID_TABLE_l_membership             =  4; #
our $FM_EGRESS_VID_TABLE_h_membership             =  28; #

our $FM_INGRESS_FID_TABLE_b_parityError           =  0; #
our $FM_INGRESS_FID_TABLE_l_STPState              =  2; #
our $FM_INGRESS_FID_TABLE_h_STPState              =  49; #

our $FM_EGRESS_FID_TABLE_b_parityError            =  0; #
our $FM_EGRESS_FID_TABLE_l_Forwarding             =  1; #
our $FM_EGRESS_FID_TABLE_h_Forwarding             =  24; #

our $FM_MA_TCN_FIFO_l_Index                       =  96; #
our $FM_MA_TCN_FIFO_h_Index                       =  107; #
our $FM_MA_TCN_FIFO_l_Type                        =  111; #
our $FM_MA_TCN_FIFO_h_Type                        =  113; #
our $FM_MA_TCN_FIFO_l_Set                         =  108; #
our $FM_MA_TCN_FIFO_h_Set                         =  110; #
our $FM_MA_TCN_FIFO_l_Entry                       =  0; #
our $FM_MA_TCN_FIFO_h_Entry                       =  95; #
our $FM_MA_TCN_FIFO_b_ParityError                 =  114; #

our $FM_MA_TCN_PTR_l_Tail                         =  9; #
our $FM_MA_TCN_PTR_h_Tail                         =  17; #
our $FM_MA_TCN_PTR_l_Head                         =  0; #
our $FM_MA_TCN_PTR_h_Head                         =  8; #

our $FM_MA_TCN_IP_b_BinFullOverflow               =  4; #
our $FM_MA_TCN_IP_b_SecurityViolationOverflow     =  5; #
our $FM_MA_TCN_IP_b_PendingEvents                 =  0; #
our $FM_MA_TCN_IP_b_EventParityErrorOverflow      =  3; #
our $FM_MA_TCN_IP_b_EventLearnedOverflow          =  2; #
our $FM_MA_TCN_IP_b_EventAgedOverflow             =  1; #

our $FM_MA_TCN_IM_b_BinFullOverflow               =  4; #
our $FM_MA_TCN_IM_b_SecurityViolationOverflow     =  5; #
our $FM_MA_TCN_IM_b_PendingEvents                 =  0; #
our $FM_MA_TCN_IM_b_LearnedOverflow               =  2; #
our $FM_MA_TCN_IM_b_AgedOverflow                  =  1; #
our $FM_MA_TCN_IM_b_ParityErrorOverflow           =  3; #

our $FM_MA_TABLE_STATUS_3_l_skipSACount           =  0; #
our $FM_MA_TABLE_STATUS_3_h_skipSACount           =  15; #
our $FM_MA_TABLE_STATUS_3_l_skipLRNCount          =  16; #
our $FM_MA_TABLE_STATUS_3_h_skipLRNCount          =  31; #

our $FM_MA_TABLE_CFG_1_b_tablePartition           =  3; #
our $FM_MA_TABLE_CFG_1_l_RateSALookupInterval     =  22; #
our $FM_MA_TABLE_CFG_1_h_RateSALookupInterval     =  27; #
our $FM_MA_TABLE_CFG_1_b_SALookupMode             =  0; #
our $FM_MA_TABLE_CFG_1_l_RateSALookupCapacity     =  4; #
our $FM_MA_TABLE_CFG_1_h_RateSALookupCapacity     =  9; #
our $FM_MA_TABLE_CFG_1_l_RateSALookupCurrent      =  10; #
our $FM_MA_TABLE_CFG_1_h_RateSALookupCurrent      =  15; #
our $FM_MA_TABLE_CFG_1_l_hashRotation             =  1; #
our $FM_MA_TABLE_CFG_1_h_hashRotation             =  2; #
our $FM_MA_TABLE_CFG_1_l_RateSALookupIncrement    =  16; #
our $FM_MA_TABLE_CFG_1_h_RateSALookupIncrement    =  21; #

our $FM_MA_TABLE_CFG_2_l_FloodGlort               =  0; #
our $FM_MA_TABLE_CFG_2_h_FloodGlort               =  15; #
our $FM_MA_TABLE_CFG_2_l_GlortFlood               =  0; #
our $FM_MA_TABLE_CFG_2_h_GlortFlood               =  15; #
our $FM_MA_TABLE_CFG_2_l_GlortBroadcast           =  16; #
our $FM_MA_TABLE_CFG_2_h_GlortBroadcast           =  31; #
our $FM_MA_TABLE_CFG_2_l_XcastGlort               =  16; #
our $FM_MA_TABLE_CFG_2_h_XcastGlort               =  31; #

our $FM_MA_TABLE_CFG_3_b_softLearning             =  1; #
our $FM_MA_TABLE_CFG_3_l_TrigIdDefault            =  2; #
our $FM_MA_TABLE_CFG_3_h_TrigIdDefault            =  7; #
our $FM_MA_TABLE_CFG_3_b_softAging                =  0; #

our $FM_MA_TCN_CFG_1_b_FlowControlEnable          =  27; #
our $FM_MA_TCN_CFG_1_l_LearnedEventsThreshold     =  9; #
our $FM_MA_TCN_CFG_1_h_LearnedEventsThreshold     =  17; #
our $FM_MA_TCN_CFG_1_l_AgedEventsThreshold        =  0; #
our $FM_MA_TCN_CFG_1_h_AgedEventsThreshold        =  8; #
our $FM_MA_TCN_CFG_1_l_ErrorEventsLimit           =  18; #
our $FM_MA_TCN_CFG_1_h_ErrorEventsLimit           =  26; #

our $FM_MA_TCN_CFG_2_l_InterruptTimeout           =  9; #
our $FM_MA_TCN_CFG_2_h_InterruptTimeout           =  18; #
our $FM_MA_TCN_CFG_2_l_InterruptThreshold         =  0; #
our $FM_MA_TCN_CFG_2_h_InterruptThreshold         =  8; #

our $FM_MA_PURGE_b_startPurge                     =  30; #
our $FM_MA_PURGE_b_matchFID                       =  28; #
our $FM_MA_PURGE_l_glort                          =  0; #
our $FM_MA_PURGE_h_glort                          =  15; #
our $FM_MA_PURGE_b_matchGlort                     =  29; #
our $FM_MA_PURGE_l_FID                            =  16; #
our $FM_MA_PURGE_h_FID                            =  27; #

our $FM_MTU_TABLE_l_mtu                           =  0; #
our $FM_MTU_TABLE_h_mtu                           =  13; #

our $FM_INGRESS_VID_TABLE_REPAIR_l_index          =  0; #
our $FM_INGRESS_VID_TABLE_REPAIR_h_index          =  11; #

our $FM_EGRESS_VID_TABLE_REPAIR_l_index           =  0; #
our $FM_EGRESS_VID_TABLE_REPAIR_h_index           =  11; #

our $FM_INGRESS_FID_TABLE_REPAIR_l_index          =  0; #
our $FM_INGRESS_FID_TABLE_REPAIR_h_index          =  11; #

our $FM_EGRESS_FID_TABLE_REPAIR_l_index           =  0; #
our $FM_EGRESS_FID_TABLE_REPAIR_h_index           =  11; #

our $FM_TCN_FIFO_REPAIR_l_index                   =  0; #
our $FM_TCN_FIFO_REPAIR_h_index                   =  8; #








our $FM_STATS_CFG_b_EnableGroup13                 =  13; #
our $FM_STATS_CFG_b_EnableGroup3                  =  3; #
our $FM_STATS_CFG_b_EnableGroup8                  =  8; #
our $FM_STATS_CFG_b_EnableGroup10                 =  10; #
our $FM_STATS_CFG_b_EnableGroup2                  =  2; #
our $FM_STATS_CFG_b_EnableGroup14                 =  14; #
our $FM_STATS_CFG_b_EnableGroup11                 =  11; #
our $FM_STATS_CFG_b_EnableGroup5                  =  5; #
our $FM_STATS_CFG_b_PrioritySelect                =  15; #
our $FM_STATS_CFG_b_EnableGroup1                  =  1; #
our $FM_STATS_CFG_b_EnableGroup6                  =  6; #
our $FM_STATS_CFG_b_EnableGroup9                  =  9; #
our $FM_STATS_CFG_b_EnableGroup12                 =  12; #
our $FM_STATS_CFG_b_EnableGroup4                  =  4; #
our $FM_STATS_CFG_b_EnableGroup7                  =  7; #

our $FM_STATS_DROP_COUNT_RX_l_DropCount           =  0; #
our $FM_STATS_DROP_COUNT_RX_h_DropCount           =  31; #

our $FM_STATS_DROP_COUNT_TX_l_DropCount           =  0; #
our $FM_STATS_DROP_COUNT_TX_h_DropCount           =  31; #
1;

