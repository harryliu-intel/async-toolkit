# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Chip/FM6000/Registers.pm
# Creation Date:    December 18, 2008
# Last Updated:     September 26, 2012
# Description:      Perl register utility variables and subroutines.
#
# INTEL CONFIDENTIAL
# Copyright 2008 - 2012 Intel Corporation. All Rights Reserved. 
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

package Chip::FM6000::Registers;
use strict;
use warnings;

use base qw(Exporter);

our @EXPORT = qw(
    FM_AN_37_BASE_PAGE_TX
    FM_AN_37_BASE_PAGE_RX
    FM_MULTI_PURPOSE_ERROR_COUNTER
    FM_AN_73_BASE_PAGE_TX
    FM_SGMII_AN_TX_CONFIG
    FM_AN_37_NEXT_PAGE_TX
    FM_SGMII_AN_TIMER_CFG
    FM_PCS_40GBASER_RX_BIP_STATUS
    FM_PCS_10GBASER_RX_BER_STATUS
    FM_AN_37_TIMER_CFG
    FM_AN_73_PAGE_RX_STATUS
    FM_NEXTHOP_WIDE
    FM_NEXTHOP_NARROW_UNICAST
    FM_FFU_SLICE_ACTION_MISC
    FM_FFU_SLICE_ACTION
    FM_AN_37_NEXT_PAGE_RX
    FM_FFU_BST_ACTION_ROUTE
    FM_AN_37_PAGE_RX_STATUS
    FM_AN_73_BASE_PAGE_RX
    FM_EPL_PORT_MULTI_PURPOSE_CFG_A
    FM_AN_73_NEXT_PAGE_TX
    FM_FFU_BST_ACTION_MISC
    FM_MOD_L2_VLAN2_TX_TAGGED
    FM_NEXTHOP_NARROW_MULTICAST
    FM_LCI_CFG
    FM_LCI_RX_FIFO
    FM_LCI_TX_FIFO
    FM_LCI_IP
    FM_LCI_IM
    FM_LCI_STATUS
    FM_FATAL_CODE
    FM_LAST_FATAL_CODE
    FM_FATAL_COUNT
    FM_SOFT_RESET
    FM_RESET_CFG
    FM_WATCHDOG_CFG
    FM_MGMT_SCRATCH
    FM_VITAL_PRODUCT_DATA
    FM_PCI_CFG_ID
    FM_PCI_CFG_CMD
    FM_PCI_CFG_1
    FM_PCI_CFG_2
    FM_PCI_CFG_BAR0
    FM_PCI_CFG_BAR1
    FM_PCI_CFG_BAR2
    FM_PCI_CFG_BAR3
    FM_PCI_CFG_BAR4
    FM_PCI_CFG_BAR5
    FM_PCI_CFG_CARDBUS
    FM_PCI_CFG_SUBID
    FM_PCI_CFG_EXP_ROM
    FM_PCI_CFG_CAP_PTR
    FM_PCI_CFG_RSVD
    FM_PCI_CFG_INT
    FM_PCI_CFG_PM_CAP
    FM_PCI_CFG_PM_CTRL
    FM_PCI_CFG_MSI_CAP
    FM_PCI_CFG_MSI_ADDR_LO
    FM_PCI_CFG_MSI_ADDR_HI
    FM_PCI_CFG_MSI_DATA
    FM_PCI_CFG_MSI_MASK
    FM_PCI_CFG_MSI_PENDING
    FM_PCI_CFG_PCIE_CAP
    FM_PCI_CFG_PCI_DEV_CAP
    FM_PCI_CFG_PCIE_DEV
    FM_PCI_CFG_PCI_LINK_CAP
    FM_PCI_CFG_PCI_LINK_CTRL
    FM_PCI_CFG_DEV_CAP2
    FM_PCI_CFG_DEV_CTRL2
    FM_PCI_CFG_PCI_LINK_CAP2
    FM_PCI_CFG_PCI_LINK_CTRL2
    FM_PCI_ENDIANISM
    FM_PCI_COMMAND
    FM_PCI_STATUS
    FM_PCI_COALESCING
    FM_PCI_RX_BD_BASE
    FM_PCI_RX_BD_END
    FM_PCI_TX_BD_BASE
    FM_PCI_TX_BD_END
    FM_PCI_IP
    FM_PCI_IM
    FM_PCI_CURRENT_TX_DATA_PTR
    FM_PCI_CURRENT_RX_DATA_PTR
    FM_PCI_CURRENT_TX_BD_PTR
    FM_PCI_CURRENT_RX_BD_PTR
    FM_PCI_TX_FRAME_LEN
    FM_PCI_SIZE
    FM_PCI_DMA_CFG
    FM_PCI_FRAME_TIMEOUT
    FM_PCI_STAT_COUNTER
    FM_PCI_STAT_NUM_PKTS
    FM_PCI_DEBUG
    FM_PCI_CORE_CTRL_1
    FM_PCI_CORE_CTRL_2
    FM_PCI_CORE_DEBUG_1
    FM_PCI_CORE_DEBUG_2
    FM_PCI_CORE_DEBUG_3
    FM_PCI_CORE_DEBUG_4
    FM_PCI_CORE_DEBUG_5
    FM_PCI_CORE_DEBUG_6
    FM_PCI_CORE_DEBUG_7
    FM_PCI_CORE_DEBUG_8
    FM_PCI_CORE_DEBUG_9
    FM_PCI_CORE_DEBUG_10
    FM_PCI_CORE_DEBUG_11
    FM_PCI_CORE_DEBUG_12
    FM_PCI_CORE_DEBUG_13
    FM_PCI_CORE_DEBUG_14
    FM_PCI_CORE_DEBUG_15
    FM_PCI_CORE_DEBUG_16
    FM_PCI_CORE_DEBUG_17
    FM_PCI_CORE_DEBUG_18
    FM_PCI_CORE_DEBUG_19
    FM_PCI_CORE_DEBUG_20
    FM_PCI_CORE_DEBUG_21
    FM_PCI_CORE_DEBUG_22
    FM_PCI_SERDES_CTRL_1
    FM_PCI_SERDES_CTRL_2
    FM_PCI_SERDES_CTRL_3
    FM_PCI_SERDES_CTRL_4
    FM_PCI_SERDES_DEBUG_1
    FM_PCI_SERDES_DEBUG_2
    FM_PCI_SERDES_DEBUG_3
    FM_ESCHED_CFG_1
    FM_ESCHED_CFG_2
    FM_ESCHED_CFG_3
    FM_ESCHED_DRR_Q
    FM_ESCHED_DRR_CFG
    FM_ESCHED_DRR_DC_INIT
    FM_MSB_CFG
    FM_MSB_RX_RATE
    FM_MSB_TRUNC
    FM_MSB_TX_CRC_ERROR
    FM_MSB_TX_FRAME_ERROR
    FM_MSB_TX_TIMEOUT
    FM_MSB_TX_VALID_FRAMES
    FM_MSB_TX_INVALID_FRAMES
    FM_MSB_RX_VALID_FRAMES
    FM_FIBM_CFG
    FM_FIBM_SGLORT
    FM_FIBM_INT
    FM_FIBM_INT_FRAME
    FM_FIBM_INT_FRAME_DMAC
    FM_FIBM_INT_FRAME_SMAC
    FM_FIBM_REQUEST_CTR
    FM_FIBM_DROP_CTR
    FM_FIBM_RESPONSE_CTR
    FM_FIBM_INTR_CTR_0
    FM_FIBM_INTR_CTR_1
    FM_FIBM_SCRATCH_0
    FM_FIBM_SCRATCH_1
    FM_FIBM_SCRATCH_2
    FM_FIBM_SCRATCH_3
    FM_FIBM_SCRATCH_4
    FM_FIBM_SCRATCH_5
    FM_FIBM_SCRATCH_6
    FM_FIBM_SCRATCH_7
    FM_FIBM_SCRATCH_8
    FM_FIBM_SCRATCH_9
    FM_FIBM_SCRATCH_10
    FM_FIBM_SCRATCH_11
    FM_FIBM_SCRATCH_12
    FM_FIBM_SCRATCH_13
    FM_FIBM_SCRATCH_14
    FM_FIBM_SCRATCH_15
    FM_EACL_CAM1
    FM_EACL_CAM2
    FM_EACL_ACTION_RAM
    FM_EACL_PROFILE_TABLE
    FM_LAG_PORT_TABLE
    FM_LAG_PROFILE_TABLE
    FM_LAG_FILTERING_CAM
    FM_SSCHED_TX_NEXT_PORT
    FM_SSCHED_TX_INIT_TOKEN
    FM_SSCHED_TX_INIT_COMPLETE
    FM_SSCHED_TX_REPLACE_TOKEN
    FM_SSCHED_RX_NEXT_PORT
    FM_SSCHED_RX_INIT_TOKEN
    FM_SSCHED_RX_INIT_COMPLETE
    FM_SSCHED_RX_REPLACE_TOKEN
    FM_SSCHED_RX_SLOW_PORT
    FM_SSCHED_RXQ_FREELIST_INIT
    FM_SSCHED_RXQ_FREELIST_INIT_DONE
    FM_SSCHED_TXQ_FREELIST_INIT
    FM_SSCHED_TXQ_FREELIST_INIT_DONE
    FM_SSCHED_HS_FREELIST_INIT
    FM_SSCHED_HS_FREELIST_INIT_DONE
    FM_SSCHED_FREELIST_INIT
    FM_SSCHED_FREELIST_INIT_DONE
    FM_HASH_LAYER2_KEY_PROFILE
    FM_HASH_LAYER2_FUNC_PROFILE
    FM_HASH_LAYER2_ROTA_PTABLE
    FM_HASH_LAYER2_ROTB_PTABLE
    FM_ALU_CMD_TABLE
    FM_ALU_Y_TABLE
    FM_L2L_SWEEPER_TIMER_CFG
    FM_L2L_SWEEPER_TIMER_STATUS
    FM_L2L_SWEEPER_CAM
    FM_L2L_SWEEPER_RAM
    FM_L2L_SWEEPER_FIFO
    FM_L2L_SWEEPER_FIFO_HEAD
    FM_L2L_SWEEPER_FIFO_TAIL
    FM_L2L_SWEEPER_IP
    FM_L2L_SWEEPER_IM
    FM_L2L_SWEEPER_WRITE_COMMAND
    FM_L2L_SWEEPER_WRITE_DATA
    FM_GLORT_CAM
    FM_GLORT_RAM
    FM_SBUS_CFG
    FM_SBUS_COMMAND
    FM_SBUS_REQUEST
    FM_SBUS_RESPONSE
    FM_SBUS_SPICO
    FM_SBUS_IP
    FM_SBUS_IM
    FM_SPICO_BIST
    FM_TAP_FSM_STATE
    FM_TAP_EFUSE_CFG
    FM_TAP_EFUSE
    FM_SSCHED_TICK_CFG
    FM_L3AR_CAM
    FM_L3AR_SLICE_CFG
    FM_L3AR_KEY_CFG
    FM_L3AR_ACTION_CFG
    FM_L3AR_RAM1
    FM_L3AR_RAM2
    FM_L3AR_RAM3
    FM_L3AR_RAM4
    FM_L3AR_RAM5
    FM_L3AR_DGLORT_PROFILE_TABLE
    FM_L3AR_SGLORT_PROFILE_TABLE
    FM_L3AR_W8ABCD_PROFILE_TABLE
    FM_L3AR_W8E_PROFILE_TABLE
    FM_L3AR_W8F_PROFILE_TABLE
    FM_L3AR_MA1_MAC_PROFILE_TABLE
    FM_L3AR_MA2_MAC_PROFILE_TABLE
    FM_L3AR_VID_PROFILE_TABLE
    FM_L3AR_MA_FID_PROFILE_TABLE
    FM_L3AR_CSGLORT_PROFILE_TABLE
    FM_L3AR_W16ABC_PROFILE_TABLE
    FM_L3AR_W16DEF_PROFILE_TABLE
    FM_L3AR_W16GH_PROFILE_TABLE
    FM_L3AR_HASH_ROT_PROFILE_TABLE
    FM_L3AR_ALU13_OP_PROFILE_TABLE
    FM_L3AR_ALU46_OP_PROFILE_TABLE
    FM_L3AR_POL1_IDX_PROFILE_TABLE
    FM_L3AR_POL2_IDX_PROFILE_TABLE
    FM_L3AR_POL3_IDX_PROFILE_TABLE
    FM_L3AR_QOS_PROFILE_TABLE
    FM_L3AR_TRAP_HEADER_RULE
    FM_L3AR_TRAP_HEADER_DATA
    FM_L3AR_IP
    FM_L3AR_IM
    FM_LBS_CAM
    FM_LBS_PROFILE_TABLE
    FM_STATS_AR_IDX_CAM
    FM_STATS_AR_IDX_RAM
    FM_STATS_AR_FLAGS_CAM1
    FM_STATS_AR_FLAGS_CAM2
    FM_STATS_AR_FLAGS_CAM2_POLARITY
    FM_STATS_AR_FLAGS_CAM2_VALUE
    FM_STATS_AR_RX_PORT_MAP
    FM_STATS_AR_TX_PORT_MAP
    FM_STATS_AR_RX_LENGTH_COMPARE
    FM_STATS_AR_TX_LENGTH_COMPARE
    FM_STATS_AR_BANK_CFG1
    FM_STATS_AR_BANK_CFG2
    FM_STATS_DISCRETE_COUNTER_FRAME
    FM_STATS_DISCRETE_COUNTER_BYTE
    FM_GLOBAL_INTERRUPT_DETECT
    FM_INTERRUPT_MASK_INT
    FM_INTERRUPT_MASK_PCIE
    FM_INTERRUPT_MASK_FIBM
    FM_GLOBAL_EPL_INT_DETECT
    FM_SRAM_CORRECTED_IP
    FM_SRAM_CORRECTED_IM
    FM_SRAM_UNCORRECTABLE_IP
    FM_SRAM_UNCORRECTABLE_IM
    FM_SRAM_UNCORRECTABLE_FATAL
    FM_SW_IP
    FM_SW_IM
    FM_SW_TEST_AND_SET
    FM_FRAME_TIME_OUT
    FM_CHIP_VERSION
    FM_PIN_STRAP_STAT
    FM_BOOT_CTRL
    FM_BOOT_ARGS
    FM_GPIO_CFG
    FM_GPIO_DATA
    FM_GPIO_IP
    FM_GPIO_IM
    FM_I2C_CFG
    FM_I2C_DATA
    FM_I2C_CTRL
    FM_MDIO_CFG
    FM_MDIO_DATA
    FM_MDIO_CTRL
    FM_SPI_TX_DATA
    FM_SPI_RX_DATA
    FM_SPI_HEADER
    FM_SPI_CTRL
    FM_LED_CFG
    FM_SCAN_CONTROL
    FM_SCAN_CONFIG_DATA_IN
    FM_SCAN_CHAIN_DATA_IN
    FM_SCAN_DATA_OUT
    FM_SCAN_STATUS
    FM_ETHCLK_CFG
    FM_ETHCLK_RATIO
    FM_PLL_CTRL
    FM_DLL_CTRL
    FM_PLL_STAT
    FM_SWEEPER_CFG
    FM_S2A_EN_STATUS
    FM_RO_CFG
    FM_TESTCTRL_MOD_CFG
    FM_TESTCTRL_TICK_CFG
    FM_TESTCTRL_TICK_CNT
    FM_TESTCTRL_CLK_CNT
    FM_TESTCTRL_START
    FM_FUSEBOX
    FM_BM_MARCH_SEQUENCE
    FM_BM_GENERAL_CONFIG
    FM_BM_TXQ_HS_SEGMENTS
    FM_BM_RXQ_PAGES
    FM_BM_MODEL_INFO
    FM_BM_FFU_SLICE_MASK
    FM_BM_VRM
    FM_BM_MAX_ALLOWED_REPAIRS
    FM_BM_FUSEBOX_SUMMARY
    FM_BM_IP
    FM_BM_IM
    FM_BM_ENGINE_STATUS
    FM_BM_FUSEBOX_BURN_TIME
    FM_BM_FUSEBOX_APPEND
    FM_BM_START_OPERATION
    FM_BM_DEBUG_STATUS_1
    FM_BM_DEBUG_STATUS_2
    FM_TEN_T_BIST_MARCH_CONFIG
    FM_TEN_T_BIST_GENERAL_CONFIG
    FM_TEN_T_BIST_STATUS
    FM_TEN_T_BIST_USER_CHECKER_MASK
    FM_TEN_T_BIST_IP
    FM_TEN_T_BIST_IM
    FM_TEN_T_BIST_USER_OP
    FM_TEN_T_BIST_PAIRED_READ
    FM_TEN_T_BIST_CURRENT_ADDR
    FM_TEN_T_BIST_START_SEQUENCE
    FM_TEN_T_BIST_SAVED_ADDRESS
    FM_TEN_T_BIST_DEBUG_STATUS_1
    FM_TEN_T_BIST_DEBUG_STATUS_2
    FM_TEN_T_BIST_CHAIN_LATENCY
    FM_TEN_T_BIST_CHAIN_CDC
    FM_CDP_BIST_REPAIR
    FM_CDP_BIST_MARCH_CONFIG
    FM_CDP_BIST_DEFECT_MAP
    FM_CDP_BIST_GENERAL_CONFIG
    FM_CDP_BIST_STATUS
    FM_CDP_BIST_USER_CHECKER_MASKS
    FM_CDP_BIST_IP
    FM_CDP_BIST_IM
    FM_CDP_BIST_CLEAR_REPAIRS
    FM_CDP_BIST_ADD_REPAIR
    FM_CDP_BIST_USER_OP
    FM_CDP_BIST_START_SEQUENCE
    FM_CDP_BIST_BLOCK_HALFCHUNK
    FM_CDP_BIST_SEGMENT_COUNT
    FM_CDP_BIST_NEXT_SEGMENT
    FM_CDP_BIST_NEXT_NEW_REPAIR
    FM_CDP_BIST_SAVED_ADDRESS
    FM_CDP_BIST_DEBUG_STATUS_1
    FM_CDP_BIST_DEBUG_STATUS_2
    FM_CDP_BIST_CHAIN_GENERAL_CONFIG
    FM_CDP_BIST_CHAIN_LATENCY
    FM_CDP_BIST_CHAIN_CDC
    FM_SPDP_BIST_MARCH_CONFIG
    FM_SPDP_BIST_GENERAL_CONFIG
    FM_SPDP_BIST_STATUS
    FM_SPDP_BIST_USER_CHECKER_MASKS
    FM_SPDP_BIST_IP
    FM_SPDP_BIST_IM
    FM_SPDP_BIST_MAX_ADDR
    FM_SPDP_BIST_USER_OP
    FM_SPDP_BIST_START_SEQUENCE
    FM_SPDP_BIST_SAVED_ADDRESS
    FM_SPDP_BIST_DEBUG_STATUS_1
    FM_SPDP_BIST_DEBUG_STATUS_2
    FM_SPDP_BIST_CHAIN_GENERAL_CONFIG
    FM_SPDP_BIST_CHAIN_LATENCY
    FM_SPDP_BIST_CHAIN_CDC
    FM_SRBM_REPAIR
    FM_SRBM_MARCH_SEQUENCE
    FM_SRBM_GENERAL_CONFIG
    FM_SRBM_STATUS
    FM_SRBM_IP
    FM_SRBM_IM
    FM_SRBM_CLEAR_REPAIRS
    FM_SRBM_ADD_REPAIR
    FM_SRBM_NEXT_NEW_REPAIR
    FM_SRBM_LAST_NR_DEFECT
    FM_CRM_DATA
    FM_CRM_CTRL
    FM_CRM_STATUS
    FM_CRM_TIME
    FM_CRM_IP
    FM_CRM_IM
    FM_CRM_COMMAND
    FM_CRM_REGISTER
    FM_CRM_PERIOD
    FM_CRM_PARAM
    FM_CM_PORT_TXMP_USAGE
    FM_CM_PORT_TXMP_IP_WM
    FM_CM_PORT_TXMP_SAMPLING_PERIOD
    FM_CM_PORT_TXMP_SAMPLING_STATE
    FM_CM_PORT_TXMP_ABOVE_IP
    FM_CM_PORT_TXMP_BELOW_IP
    FM_CM_PORT_TXMP_ABOVE_IM
    FM_CM_PORT_TXMP_BELOW_IM
    FM_CM_INTERRUPT_DETECT
    FM_FC_MRL_MGMT_CYCLES
    FM_FC_MRL_SWEEP_CYCLES
    FM_FC_MRL_SWEEP_PERIOD
    FM_FC_MRL_UNROLL_ITER
    FM_FC_MRL_TOKEN_LIMIT
    FM_FC_MRL_FC_TOKEN_LIMIT
    FM_FC_MRL_ALIGN_TX_STATS
    FM_FC_MRL_RATE_LIMITER
    FM_L2L_MAC_TABLE_CFG
    FM_L2L_CMD_PROFILE_TABLE
    FM_L2L_LOCK_TABLE
    FM_L2L_EVID1_TABLE
    FM_L2L_IVID1_TABLE
    FM_L2L_EVID2_TABLE
    FM_L2L_IVID2_TABLE
    FM_SAF_MATRIX
    FM_PORT_STATUS
    FM_AN_IM
    FM_LINK_IM
    FM_AN_IP
    FM_LINK_IP
    FM_SGMII_AN_TX_CONFIG_LOOPBACK
    FM_SGMII_AN_RX_CONFIG
    FM_AN_73_NEXT_PAGE_RX
    FM_LINK_RULES
    FM_MAC_CFG
    FM_TX_SEQUENCE
    FM_RX_SEQUENCE
    FM_MAC_1588_STATUS
    FM_MAC_OVERSIZE_COUNTER
    FM_MAC_JABBER_COUNTER
    FM_MAC_UNDERSIZE_COUNTER
    FM_MAC_RUNT_COUNTER
    FM_MAC_OVERRUN_COUNTER
    FM_MAC_UNDERRUN_COUNTER
    FM_MAC_CODE_ERROR_COUNTER
    FM_MAC_LINK_COUNTER
    FM_PCS_1000BASEX_CFG
    FM_PCS_1000BASEX_RX_STATUS
    FM_PCS_1000BASEX_TX_STATUS
    FM_PCS_10GBASER_CFG
    FM_PCS_10GBASER_RX_STATUS
    FM_PCS_10GBASER_TX_STATUS
    FM_AN_37_CFG
    FM_AN_37_STATUS
    FM_AN_73_CFG
    FM_AN_73_TIMER_CFG
    FM_AN_73_STATUS
    FM_AN_73_TX_LCW_DEBUG
    FM_AN_73_RX_ABILITY_MATCH_LCW_DEBUG
    FM_SERDES_CFG
    FM_DISPARITY_ERROR_8B10B
    FM_LANE_CFG
    FM_LANE_STATUS
    FM_SERDES_RX_CFG
    FM_SERDES_TX_CFG
    FM_SERDES_SIGNAL_DETECT
    FM_SERDES_STATUS
    FM_SERDES_IM
    FM_SERDES_IP
    FM_LANE_DEBUG
    FM_EPL_IP
    FM_EPL_CFG_A
    FM_EPL_CFG_B
    FM_EPL_LED_STATUS
    FM_EPL_FIFO_ERROR_STATUS
    FM_EPL_TX_FIFO_READ_PTR_STATUS
    FM_EPL_TX_FIFO_WRITE_PTR_STATUS
    FM_EPL_TX_FIFO_A_PTR_STATUS
    FM_EPL_TX_FIFO_B_PTR_STATUS
    FM_EPL_TX_FIFO_C_PTR_STATUS
    FM_EPL_TX_FIFO_D_PTR_STATUS
    FM_EPL_RX_FIFO_READ_PTR_STATUS
    FM_EPL_RX_FIFO_WRITE_PTR_STATUS
    FM_EPL_RX_FIFO_A_PTR_STATUS
    FM_EPL_RX_FIFO_B_PTR_STATUS
    FM_EPL_RX_FIFO_C_PTR_STATUS
    FM_EPL_RX_FIFO_D_PTR_STATUS
    FM_PCS_10GBASEX_CFG
    FM_PCS_10GBASEX_RX_STATUS
    FM_PCS_10GBASEX_TX_STATUS
    FM_PCS_40GBASER_CFG
    FM_PCS_40GBASER_RX_STATUS
    FM_EPL_1588_TIMER_STATUS
    FM_PARSER_CAM
    FM_PARSER_RAM
    FM_PARSER_INIT_STATE
    FM_PARSER_INIT_FIELDS
    FM_CM_RXMP_MAP
    FM_CM_TXMP_MAP
    FM_CM_TC_MAP
    FM_CM_BSG_MAP
    FM_CM_GLOBAL_USAGE
    FM_CM_RXMP_USAGE
    FM_CM_SHARED_RXMP_USAGE
    FM_CM_PORT_RXMP_USAGE
    FM_CM_GLOBAL_TXMP_USAGE
    FM_CM_PORT_TX_DROP_COUNT
    FM_CM_GLOBAL_WM
    FM_CM_SHARED_RXMP_WM
    FM_CM_PORT_RXMP_PRIVATE_WM
    FM_CM_PORT_RXMP_HOG_WM
    FM_CM_PORT_TXMP_PRIVATE_WM
    FM_CM_PORT_TXMP_HOG_WM
    FM_CM_RXMP_SOFT_DROP_WM
    FM_CM_PORT_RXMP_PAUSE_ON_WM
    FM_CM_PORT_RXMP_PAUSE_OFF_WM
    FM_CM_SHARED_RXMP_PAUSE_ON_WM
    FM_CM_SHARED_RXMP_PAUSE_OFF_WM
    FM_CM_TC_PC_MAP
    FM_CM_PC_RXMP_MAP
    FM_CM_PAUSE_CFG
    FM_CM_PAUSE_PACING_CFG
    FM_CM_PAUSE_RCV_STATE
    FM_CM_ESCHED_STATE
    FM_CM_PAUSE_GEN_STATE
    FM_CM_PAUSE_PACING_STATE
    FM_CM_TX_MIRROR_DEST
    FM_CM_TX_MIRROR_SRC
    FM_CM_SAMPLING_MIRROR_CFG
    FM_ERL_CFG
    FM_ERL_CFG_IFG
    FM_ERL_USAGE
    FM_CM_QUEUE_STATE_INIT
    FM_MAPPER_VID_PROFILE_TABLE
    FM_MAPPER_VID1_TABLE
    FM_MAPPER_VID2_TABLE
    FM_MAPPER_SRC_PORT_TABLE
    FM_MAPPER_DMAC_CAM1
    FM_MAPPER_DMAC_CAM2
    FM_MAPPER_DMAC_CAM3
    FM_MAPPER_DMAC_RAM1
    FM_MAPPER_DMAC_RAM2
    FM_MAPPER_DMAC_RAM3
    FM_MAPPER_SMAC_CAM1
    FM_MAPPER_SMAC_CAM3
    FM_MAPPER_SMAC_RAM1
    FM_MAPPER_SMAC_RAM3
    FM_MAPPER_TYPE_CAM1
    FM_MAPPER_TYPE_CAM2
    FM_MAPPER_TYPE_RAM1
    FM_MAPPER_TYPE_RAM2
    FM_MAPPER_DIP_CAM1
    FM_MAPPER_DIP_CAM2
    FM_MAPPER_DIP_CAM3
    FM_MAPPER_SIP_CAM1
    FM_MAPPER_SIP_CAM2
    FM_MAPPER_SIP_CAM3
    FM_MAPPER_DIP_RAM1
    FM_MAPPER_DIP_RAM2
    FM_MAPPER_DIP_RAM3
    FM_MAPPER_SIP_RAM1
    FM_MAPPER_SIP_RAM2
    FM_MAPPER_SIP_RAM3
    FM_MAPPER_PROT_CAM1
    FM_MAPPER_PROT_CAM2
    FM_MAPPER_PROT_RAM1
    FM_MAPPER_PROT_RAM2
    FM_MAPPER_LENGTH_COMPARE
    FM_MAPPER_L4_SRC_COMPARE
    FM_MAPPER_L4_DST_COMPARE
    FM_MAPPER_SCENARIO_FLAGS_CFG
    FM_MAPPER_FFU_INIT
    FM_MAPPER_QOS_PER_PORT_VPRI1
    FM_MAPPER_QOS_PER_PORT_VPRI2
    FM_MAPPER_QOS_PER_PORT_W4
    FM_MAPPER_QOS_L2_VPRI1_TO_ISL
    FM_MAPPER_QOS_L2_VPRI2_TO_ISL
    FM_MAPPER_QOS_W4_TO_ISL
    FM_MAPPER_QOS_L3_PRI_TO_ISL
    FM_MAPPER_QOS_CAM1
    FM_MAPPER_QOS_CAM2
    FM_MAPPER_QOS_RAM1
    FM_MAPPER_QOS_RAM2
    FM_POLICER_CFG_4K
    FM_POLICER_CFG_1K
    FM_POLICER_STATE_4K
    FM_POLICER_STATE_1K
    FM_POLICER_QOS_MAP1
    FM_POLICER_QOS_MAP2
    FM_L2AR_CAM
    FM_L2AR_CAM_DMASK
    FM_L2AR_SLICE_CFG
    FM_L2AR_EACL_EXT
    FM_L2AR_RAM
    FM_L2AR_FLAGS_CAM_POLARITY
    FM_L2AR_FLAGS_CAM_VALUE
    FM_L2AR_FLAGS_CAM
    FM_L2AR_ACTION_DMT
    FM_L2AR_ACTION_CPU_CODE
    FM_L2AR_ACTION_TRAP_HEADER
    FM_L2AR_ACTION_MIRROR
    FM_L2AR_ACTION_QOS
    FM_L2AR_ACTION_MA_WRITEBACK
    FM_L2AR_ACTION_DGLORT
    FM_L2AR_ACTION_W16AB
    FM_L2AR_ACTION_W16CDEF
    FM_L2AR_ACTION_W8ABCDE
    FM_L2AR_ACTION_W4
    FM_L2AR_ACTION_VID
    FM_L2AR_ACTION_DMASK_IDX
    FM_L2AR_ACTION_STATS_IDX5AB
    FM_L2AR_ACTION_STATS_IDX5C
    FM_L2AR_ACTION_STATS_IDX12A
    FM_L2AR_ACTION_STATS_IDX12B
    FM_L2AR_ACTION_STATS_IDX16A
    FM_L2AR_ACTION_STATS_IDX16B
    FM_L2AR_DGLORT_PROFILE_TABLE
    FM_L2AR_W16AB_PROFILE_TABLE
    FM_L2AR_W16CDEF_PROFILE_TABLE
    FM_L2AR_W8ABCDE_PROFILE_TABLE
    FM_L2AR_W4_PROFILE_TABLE
    FM_L2AR_VID_PROFILE_TABLE
    FM_L2AR_DMASK_IDX_PROFILE_TABLE
    FM_L2AR_QOS_PROFILE_TABLE
    FM_L2AR_MA_WRITEBACK_PROFILE_TABLE
    FM_L2AR_STATS_IDX5AB_PROFILE_TABLE
    FM_L2AR_STATS_IDX5C_PROFILE_TABLE
    FM_L2AR_STATS_IDX12A_PROFILE_TABLE
    FM_L2AR_STATS_IDX12B_PROFILE_TABLE
    FM_L2AR_STATS_IDX16A_PROFILE_TABLE
    FM_L2AR_STATS_IDX16B_PROFILE_TABLE
    FM_L2AR_TRAP_HEADER_RULE
    FM_L2AR_TRAP_HEADER_DATA
    FM_L2AR_IP
    FM_L2AR_IM
    FM_MOD_L2_VLAN1_TX_TAGGED
    FM_MOD_MAP_DATA_V2T
    FM_MOD_CAM
    FM_MOD_COMMAND_RAM
    FM_MOD_VALUE_RAM
    FM_MOD_MAP_IDX12A
    FM_MOD_MAP_DATA_W16A
    FM_MOD_MAP_DATA_W16B
    FM_MOD_MAP_DATA_W16C
    FM_MOD_MAP_DATA_W16D
    FM_MOD_MAP_DATA_W16E
    FM_MOD_MAP_DATA_W16F
    FM_MOD_MAP_DATA_W12A
    FM_MOD_MAP_DATA_W8A
    FM_MOD_MAP_DATA_W8B
    FM_MOD_TX_PORT_TAG
    FM_MOD_DST_PORT_TAG
    FM_MOD_L2_VPRI1_TX_MAP
    FM_MOD_L2_VPRI2_TX_MAP
    FM_MOD_TX_PAUSE_QUANTA
    FM_MOD_MIN_LENGTH
    FM_MCAST_LOOPBACK_SUPPRESS
    FM_MOD_MAP_DATA_CTRL
    FM_MOD_MAP_DATA_V2T_CTRL
    FM_MOD_TX_MIRROR_SRC
    FM_MOD_TRANSMIT_MODE
    FM_NEXTHOP_TABLE
    FM_L2F_TABLE_4K
    FM_L2F_TABLE_256
    FM_L2F_PROFILE_TABLE
    FM_STATS_BANK_COUNTER
    FM_MCAST_DEST_TABLE
    FM_MCAST_TX_MIRROR_DEST
    FM_MCAST_MIRROR_CFG
    FM_MCAST_TX_TRUNC_MASK
    FM_MCAST_PRIVATE_WM
    FM_MCAST_HOG_WM
    FM_MCAST_LIMITED_SKEW_MULTICAST
    FM_MCAST_VLAN_TABLE
    FM_L2L_MAC_TABLE
    FM_L2L_MAC_TABLE_SWEEPER
    FM_FFU_BST_ACTION
    FM_FFU_BST_KEY
    FM_FFU_BST_SCENARIO_CAM
    FM_FFU_BST_SCENARIO_CFG1
    FM_FFU_BST_SCENARIO_CFG2
    FM_FFU_BST_ROOT_KEYS
    FM_FFU_BST_MASTER_VALID
    FM_FFU_SLICE_CAM
    FM_FFU_SLICE_ACTION_ROUTE
    FM_FFU_SLICE_SCENARIO_CAM
    FM_FFU_SLICE_SCENARIO_CFG
    FM_FFU_SLICE_MASTER_VALID
    FM_FFU_ATOMIC_APPLY
    FM_FFU_REMAP_SCENARIO_CAM
    FM_FFU_REMAP_PROFILE
    FM_FFU_EACL_CFG
    FM_HASH_LAYER3_PROFILE
    FM_HASH_LAYER3_PTABLE
);
our @EXPORT_FAIL = qw(_panic _validatePrototype);

###############################################################################
# Local Variables
###############################################################################

use constant INDENT => "  "; 

###############################################################################
# Local Functions
###############################################################################

sub _panic($)
{
    my ($format, @arguments) = @_;

    my ($level);

    open(STDOUT, ">&STDERR");
    for ($level = 1; ; $level++)
    {
        my ($package, $file, $line, $subroutine) = caller($level);
        if (!defined($subroutine) || $subroutine eq "(eval)")
        {
            last;
        }
        $file =~ s,(perl|te2)/+,,;
        printf("%s$subroutine at $file line $line:\n", INDENT x ($level - 1));
    }
    printf("%s$format\n", INDENT x ($level - 1), @arguments);
    die();
}

sub _validatePrototype(\@$;$)
{
    my ($arguments, $m, $n) = @_;

	$n = defined($n) ? $n : $m;

    my ($package, $file, $line, $subroutine) = caller(1);
    if (scalar(@{$arguments}) < $m)
    {
        _panic("Not enough arguments for $subroutine");
    }
	elsif ($n != -1 && scalar(@{$arguments}) > $n) 
    {
        _panic("Too many arguments for $subroutine");
    }
    for (my $i = 0; $i < $n; $i++)
    {
        if (!defined($arguments->[$i]))
    {
            _panic("$subroutine: argument $i not defined");
        }
        elsif ($arguments->[$i] != int($arguments->[$i]))
        {
            _panic("$subroutine: argument $i not an integer");
        }
    }
}

###############################################################################
# Public Variables & Functions
###############################################################################

our $FM_MGMT1_BASE                                =  0x00000; # // 0x00000 top level
our $FM_MGMT1_SIZE                                =  0x01000; #
our $FM_PCIE_BASE                                 =  0x01000; # // 0x01000 top level
our $FM_PCIE_SIZE                                 =  0x01000; #
our $FM_ESCHED_BASE                               =  0x02000; # // 0x02000 top level
our $FM_ESCHED_SIZE                               =  0x01000; #
our $FM_MONITOR_BASE                              =  0x03000; # // 0x03000 top level
our $FM_MONITOR_SIZE                              =  0x01000; #
our $FM_MSB_BASE                                  =  0x04000; # // 0x04000 top level
our $FM_MSB_SIZE                                  =  0x01000; #
our $FM_FIBM_BASE                                 =  0x05000; # // 0x05000 top level
our $FM_FIBM_SIZE                                 =  0x01000; #
our $FM_EACL_BASE                                 =  0x06000; # // 0x06000 top level
our $FM_EACL_SIZE                                 =  0x01000; #
our $FM_LAG_BASE                                  =  0x07000; # // 0x07000 top level
our $FM_LAG_SIZE                                  =  0x01000; #
our $FM_SSCHED_BASE                               =  0x08000; # // 0x08000 top level
our $FM_SSCHED_SIZE                               =  0x01000; #
our $FM_HASH_BASE                                 =  0x0B000; # // 0x0B000 top level
our $FM_HASH_SIZE                                 =  0x01000; #
our $FM_ALU_BASE                                  =  0x0C000; # // 0x0C000 top level
our $FM_ALU_SIZE                                  =  0x01000; #
our $FM_L2L_SWEEPER_BASE                          =  0x0D000; # // 0x0D000 top level
our $FM_L2L_SWEEPER_SIZE                          =  0x01000; #
our $FM_GLORT_BASE                                =  0x0E000; # // 0x0E000 top level
our $FM_GLORT_SIZE                                =  0x01000; #
our $FM_JSS_BASE                                  =  0x0F000; # // 0x0F000 top level
our $FM_JSS_SIZE                                  =  0x01000; #
our $FM_L3AR_BASE                                 =  0x10000; # // 0x10000 top level
our $FM_L3AR_SIZE                                 =  0x04000; #
our $FM_LBS_BASE                                  =  0x14000; # // 0x14000 top level
our $FM_LBS_SIZE                                  =  0x01000; #
our $FM_STATS_AR_BASE                             =  0x18000; # // 0x18000 top level
our $FM_STATS_AR_SIZE                             =  0x02000; #
our $FM_STATS_DISCRETE_BASE                       =  0x1A000; # // 0x1A000 top level
our $FM_STATS_DISCRETE_SIZE                       =  0x01000; #
our $FM_MGMT2_BASE                                =  0x1C000; # // 0x1C000 top level
our $FM_MGMT2_SIZE                                =  0x04000; #
our $FM_CMM_BASE                                  =  0x20000; # // 0x20000 top level
our $FM_CMM_SIZE                                  =  0x08000; #
our $FM_FC_BEM_BASE                               =  0x28000; # // 0x28000 top level
our $FM_FC_BEM_SIZE                               =  0x01000; #
our $FM_L2L_BASE                                  =  0x30000; # // 0x30000 top level
our $FM_L2L_SIZE                                  =  0x08000; #
our $FM_SAF_BASE                                  =  0xA0000; # // 0xA0000 top level
our $FM_SAF_SIZE                                  =  0x01000; #
our $FM_EPL_BASE                                  =  0xE0000; # // 0xE0000 top level
our $FM_EPL_SIZE                                  =  0x20000; #
our $FM_PARSER_BASE                               =  0x100000; # // 0x100000 top level
our $FM_PARSER_SIZE                               =  0x10000; #
our $FM_CM_BASE                                   =  0x110000; # // 0x110000 top level
our $FM_CM_SIZE                                   =  0x10000; #
our $FM_MAPPER_BASE                               =  0x120000; # // 0x120000 top level
our $FM_MAPPER_SIZE                               =  0x10000; #
our $FM_POLICERS_BASE                             =  0x130000; # // 0x130000 top level
our $FM_POLICERS_SIZE                             =  0x10000; #
our $FM_L2AR_BASE                                 =  0x140000; # // 0x140000 top level
our $FM_L2AR_SIZE                                 =  0x10000; #
our $FM_MOD_BASE                                  =  0x150000; # // 0x150000 top level
our $FM_MOD_SIZE                                  =  0x10000; #
our $FM_NEXTHOP_BASE                              =  0x160000; # // 0x160000 top level
our $FM_NEXTHOP_SIZE                              =  0x20000; #
our $FM_L2F_BASE                                  =  0x180000; # // 0x180000 top level
our $FM_L2F_SIZE                                  =  0x80000; #
our $FM_STATS_BANK_BASE                           =  0x200000; # // 0x200000 top level
our $FM_STATS_BANK_SIZE                           =  0x40000; #
our $FM_MCAST_MID_BASE                            =  0x240000; # // 0x240000 top level
our $FM_MCAST_MID_SIZE                            =  0x20000; #
our $FM_MCAST_POST_BASE                           =  0x260000; # // 0x260000 top level
our $FM_MCAST_POST_SIZE                           =  0x20000; #
our $FM_L2L_MAC_BASE                              =  0x280000; # // 0x280000 top level
our $FM_L2L_MAC_SIZE                              =  0x80000; #
our $FM_FFU_BASE                                  =  0x300000; # // 0x300000 top level
our $FM_FFU_SIZE                                  =  0x100000; #

# Structs
our $FM_LAG_CAM_KEYS_WIDTH                        =  2; #
our $FM_LAG_CAM_KEYS_BITS                         =  64; #

our $FM_LAG_CAM_KEYS_l_HashAModulo9               =  16; #
our $FM_LAG_CAM_KEYS_h_HashAModulo9               =  19; #
our $FM_LAG_CAM_KEYS_l_HashBValue                 =  32; #
our $FM_LAG_CAM_KEYS_h_HashBValue                 =  39; #
our $FM_LAG_CAM_KEYS_l_HashBModulo13              =  56; #
our $FM_LAG_CAM_KEYS_h_HashBModulo13              =  59; #
our $FM_LAG_CAM_KEYS_l_HashBModulo15              =  60; #
our $FM_LAG_CAM_KEYS_h_HashBModulo15              =  63; #
our $FM_LAG_CAM_KEYS_l_HashBModulo11              =  52; #
our $FM_LAG_CAM_KEYS_h_HashBModulo11              =  55; #
our $FM_LAG_CAM_KEYS_l_HashAValue                 =  0; #
our $FM_LAG_CAM_KEYS_h_HashAValue                 =  7; #
our $FM_LAG_CAM_KEYS_l_HashAModulo13              =  24; #
our $FM_LAG_CAM_KEYS_h_HashAModulo13              =  27; #
our $FM_LAG_CAM_KEYS_l_HashBModulo7               =  45; #
our $FM_LAG_CAM_KEYS_h_HashBModulo7               =  47; #
our $FM_LAG_CAM_KEYS_l_HashAModulo11              =  20; #
our $FM_LAG_CAM_KEYS_h_HashAModulo11              =  23; #
our $FM_LAG_CAM_KEYS_l_HashBModulo9               =  48; #
our $FM_LAG_CAM_KEYS_h_HashBModulo9               =  51; #
our $FM_LAG_CAM_KEYS_l_HashAModulo3               =  8; #
our $FM_LAG_CAM_KEYS_h_HashAModulo3               =  9; #
our $FM_LAG_CAM_KEYS_l_HashBModulo3               =  40; #
our $FM_LAG_CAM_KEYS_h_HashBModulo3               =  41; #
our $FM_LAG_CAM_KEYS_l_HashAModulo5               =  10; #
our $FM_LAG_CAM_KEYS_h_HashAModulo5               =  12; #
our $FM_LAG_CAM_KEYS_l_HashAModulo15              =  28; #
our $FM_LAG_CAM_KEYS_h_HashAModulo15              =  31; #
our $FM_LAG_CAM_KEYS_l_HashBModulo5               =  42; #
our $FM_LAG_CAM_KEYS_h_HashBModulo5               =  44; #
our $FM_LAG_CAM_KEYS_l_HashAModulo7               =  13; #
our $FM_LAG_CAM_KEYS_h_HashAModulo7               =  15; #


our $FM_L2L_SWEEPER_CAM_KEYS_WIDTH                =  4; #
our $FM_L2L_SWEEPER_CAM_KEYS_BITS                 =  112; #

our $FM_L2L_SWEEPER_CAM_KEYS_b_Error              =  111; #
our $FM_L2L_SWEEPER_CAM_KEYS_b_Timer              =  110; #
our $FM_L2L_SWEEPER_CAM_KEYS_l_TAG                =  96; #
our $FM_L2L_SWEEPER_CAM_KEYS_h_TAG                =  107; #
our $FM_L2L_SWEEPER_CAM_KEYS_l_DATA               =  88; #
our $FM_L2L_SWEEPER_CAM_KEYS_h_DATA               =  95; #
our $FM_L2L_SWEEPER_CAM_KEYS_l_FID1               =  48; #
our $FM_L2L_SWEEPER_CAM_KEYS_h_FID1               =  59; #
our $FM_L2L_SWEEPER_CAM_KEYS_l_MAC                =  0; #
our $FM_L2L_SWEEPER_CAM_KEYS_h_MAC                =  47; #
our $FM_L2L_SWEEPER_CAM_KEYS_l_GLORT              =  72; #
our $FM_L2L_SWEEPER_CAM_KEYS_h_GLORT              =  87; #
our $FM_L2L_SWEEPER_CAM_KEYS_l_FID2               =  60; #
our $FM_L2L_SWEEPER_CAM_KEYS_h_FID2               =  71; #
our $FM_L2L_SWEEPER_CAM_KEYS_l_Prec               =  108; #
our $FM_L2L_SWEEPER_CAM_KEYS_h_Prec               =  109; #


our $FM_L3AR_CAM_KEYS_WIDTH                       =  8; #
our $FM_L3AR_CAM_KEYS_BITS                        =  252; #

our $FM_L3AR_CAM_KEYS_l_FFU_DATA_W24_TOP          =  164; #
our $FM_L3AR_CAM_KEYS_h_FFU_DATA_W24_TOP          =  171; #
our $FM_L3AR_CAM_KEYS_l_QOS_ISL_PRI               =  83; #
our $FM_L3AR_CAM_KEYS_h_QOS_ISL_PRI               =  86; #
our $FM_L3AR_CAM_KEYS_l_SRC_PORT                  =  52; #
our $FM_L3AR_CAM_KEYS_h_SRC_PORT                  =  58; #
our $FM_L3AR_CAM_KEYS_l_FFU_DATA_W8A              =  140; #
our $FM_L3AR_CAM_KEYS_h_FFU_DATA_W8A              =  147; #
our $FM_L3AR_CAM_KEYS_l_L2_TYPE_ID2               =  87; #
our $FM_L3AR_CAM_KEYS_h_L2_TYPE_ID2               =  90; #
our $FM_L3AR_CAM_KEYS_l_FFU_DATA_W8B              =  148; #
our $FM_L3AR_CAM_KEYS_h_FFU_DATA_W8B              =  155; #
our $FM_L3AR_CAM_KEYS_l_FFU_DATA_W8C              =  228; #
our $FM_L3AR_CAM_KEYS_h_FFU_DATA_W8C              =  235; #
our $FM_L3AR_CAM_KEYS_l_ACTION_FLAGS              =  0; #
our $FM_L3AR_CAM_KEYS_h_ACTION_FLAGS              =  51; #
our $FM_L3AR_CAM_KEYS_l_ISL_SGLORT                =  67; #
our $FM_L3AR_CAM_KEYS_h_ISL_SGLORT                =  82; #
our $FM_L3AR_CAM_KEYS_l_L2_SMAC_ID3               =  96; #
our $FM_L3AR_CAM_KEYS_h_L2_SMAC_ID3               =  100; #
our $FM_L3AR_CAM_KEYS_l_FFU_DATA_W16B_TOP         =  160; #
our $FM_L3AR_CAM_KEYS_h_FFU_DATA_W16B_TOP         =  163; #
our $FM_L3AR_CAM_KEYS_l_L3_SIP_ID3                =  106; #
our $FM_L3AR_CAM_KEYS_h_L3_SIP_ID3                =  110; #
our $FM_L3AR_CAM_KEYS_l_FFU_DATA_TAG1A            =  180; #
our $FM_L3AR_CAM_KEYS_h_FFU_DATA_TAG1A            =  191; #
our $FM_L3AR_CAM_KEYS_l_FFU_DATA_TAG1B            =  192; #
our $FM_L3AR_CAM_KEYS_h_FFU_DATA_TAG1B            =  203; #
our $FM_L3AR_CAM_KEYS_l_L2_DMAC_ID3               =  91; #
our $FM_L3AR_CAM_KEYS_h_L2_DMAC_ID3               =  95; #
our $FM_L3AR_CAM_KEYS_l_L3_DIP_ID3                =  101; #
our $FM_L3AR_CAM_KEYS_h_L3_DIP_ID3                =  105; #
our $FM_L3AR_CAM_KEYS_l_NEXTHOP_TAG               =  172; #
our $FM_L3AR_CAM_KEYS_h_NEXTHOP_TAG               =  179; #
our $FM_L3AR_CAM_KEYS_l_L3_HASH                   =  236; #
our $FM_L3AR_CAM_KEYS_h_L3_HASH                   =  251; #
our $FM_L3AR_CAM_KEYS_l_MAP_VID2                  =  128; #
our $FM_L3AR_CAM_KEYS_h_MAP_VID2                  =  139; #
our $FM_L3AR_CAM_KEYS_l_MAP_VID1                  =  115; #
our $FM_L3AR_CAM_KEYS_h_MAP_VID1                  =  126; #
our $FM_L3AR_CAM_KEYS_l_L3_PROT_ID2               =  111; #
our $FM_L3AR_CAM_KEYS_h_L3_PROT_ID2               =  114; #
our $FM_L3AR_CAM_KEYS_l_SRC_PORT_ID4              =  59; #
our $FM_L3AR_CAM_KEYS_h_SRC_PORT_ID4              =  66; #
our $FM_L3AR_CAM_KEYS_l_FFU_DATA_W16A_TOP         =  156; #
our $FM_L3AR_CAM_KEYS_h_FFU_DATA_W16A_TOP         =  159; #
our $FM_L3AR_CAM_KEYS_l_FFU_DATA_TAG2A            =  204; #
our $FM_L3AR_CAM_KEYS_h_FFU_DATA_TAG2A            =  215; #
our $FM_L3AR_CAM_KEYS_l_FFU_DATA_TAG2B            =  216; #
our $FM_L3AR_CAM_KEYS_h_FFU_DATA_TAG2B            =  227; #


our $FM_STATS_AR_CAM_KEYS_WIDTH                   =  4; #
our $FM_STATS_AR_CAM_KEYS_BITS                    =  128; #

our $FM_STATS_AR_CAM_KEYS_l_RX_KEY_DROP_CODE      =  45; #
our $FM_STATS_AR_CAM_KEYS_h_RX_KEY_DROP_CODE      =  52; #
our $FM_STATS_AR_CAM_KEYS_l_RX_PORT_TAG           =  1; #
our $FM_STATS_AR_CAM_KEYS_h_RX_PORT_TAG           =  8; #
our $FM_STATS_AR_CAM_KEYS_b_RX_VALID              =  0; #
our $FM_STATS_AR_CAM_KEYS_l_TX_PORT_TAG           =  72; #
our $FM_STATS_AR_CAM_KEYS_h_TX_PORT_TAG           =  79; #
our $FM_STATS_AR_CAM_KEYS_b_TX_VALID              =  71; #
our $FM_STATS_AR_CAM_KEYS_l_RX_KEY_CPU_CODE       =  53; #
our $FM_STATS_AR_CAM_KEYS_h_RX_KEY_CPU_CODE       =  60; #
our $FM_STATS_AR_CAM_KEYS_l_RX_KEY_CM_FLAGS       =  65; #
our $FM_STATS_AR_CAM_KEYS_h_RX_KEY_CM_FLAGS       =  70; #
our $FM_STATS_AR_CAM_KEYS_l_RX_DISP               =  9; #
our $FM_STATS_AR_CAM_KEYS_h_RX_DISP               =  10; #
our $FM_STATS_AR_CAM_KEYS_l_RX_KEY_STATS_FLAGS    =  29; #
our $FM_STATS_AR_CAM_KEYS_h_RX_KEY_STATS_FLAGS    =  44; #
our $FM_STATS_AR_CAM_KEYS_l_RX_KEY_RXMP           =  61; #
our $FM_STATS_AR_CAM_KEYS_h_RX_KEY_RXMP           =  64; #
our $FM_STATS_AR_CAM_KEYS_l_TX_MOD_KEY            =  88; #
our $FM_STATS_AR_CAM_KEYS_h_TX_MOD_KEY            =  127; #
our $FM_STATS_AR_CAM_KEYS_l_RX_KEY_MOD_FLAGS      =  11; #
our $FM_STATS_AR_CAM_KEYS_h_RX_KEY_MOD_FLAGS      =  28; #
our $FM_STATS_AR_CAM_KEYS_l_TX_DISP_FLAGS         =  80; #
our $FM_STATS_AR_CAM_KEYS_h_TX_DISP_FLAGS         =  87; #


our $FM_PARSER_CAM_KEYS_WIDTH                     =  2; #
our $FM_PARSER_CAM_KEYS_BITS                      =  64; #

our $FM_PARSER_CAM_KEYS_l_State1                  =  40; #
our $FM_PARSER_CAM_KEYS_h_State1                  =  47; #
our $FM_PARSER_CAM_KEYS_l_State2                  =  48; #
our $FM_PARSER_CAM_KEYS_h_State2                  =  55; #
our $FM_PARSER_CAM_KEYS_l_State3                  =  56; #
our $FM_PARSER_CAM_KEYS_h_State3                  =  63; #
our $FM_PARSER_CAM_KEYS_l_State0                  =  32; #
our $FM_PARSER_CAM_KEYS_h_State0                  =  39; #
our $FM_PARSER_CAM_KEYS_l_FrameByte1              =  16; #
our $FM_PARSER_CAM_KEYS_h_FrameByte1              =  23; #
our $FM_PARSER_CAM_KEYS_l_FrameByte2              =  8; #
our $FM_PARSER_CAM_KEYS_h_FrameByte2              =  15; #
our $FM_PARSER_CAM_KEYS_l_FrameByte3              =  0; #
our $FM_PARSER_CAM_KEYS_h_FrameByte3              =  7; #
our $FM_PARSER_CAM_KEYS_l_FrameByte0              =  24; #
our $FM_PARSER_CAM_KEYS_h_FrameByte0              =  31; #


our $FM_POLICER_CFG_ENTRY_WIDTH                   =  1; #
our $FM_POLICER_CFG_ENTRY_BITS                    =  32; #

our $FM_POLICER_CFG_ENTRY_l_CapacityExponent      =  15; #
our $FM_POLICER_CFG_ENTRY_h_CapacityExponent      =  19; #
our $FM_POLICER_CFG_ENTRY_b_Unit                  =  1; #
our $FM_POLICER_CFG_ENTRY_l_TAG                   =  20; #
our $FM_POLICER_CFG_ENTRY_h_TAG                   =  31; #
our $FM_POLICER_CFG_ENTRY_l_RateExponent          =  6; #
our $FM_POLICER_CFG_ENTRY_h_RateExponent          =  10; #
our $FM_POLICER_CFG_ENTRY_l_RateMantissa          =  2; #
our $FM_POLICER_CFG_ENTRY_h_RateMantissa          =  5; #
our $FM_POLICER_CFG_ENTRY_l_CapacityMantissa      =  11; #
our $FM_POLICER_CFG_ENTRY_h_CapacityMantissa      =  14; #
our $FM_POLICER_CFG_ENTRY_b_Mode                  =  0; #


our $FM_L2AR_CAM_KEYS_WIDTH                       =  12; #
our $FM_L2AR_CAM_KEYS_BITS                        =  384; #

our $FM_L2AR_CAM_KEYS_l_L2L_ETAG1                 =  192; #
our $FM_L2AR_CAM_KEYS_h_L2L_ETAG1                 =  203; #
our $FM_L2AR_CAM_KEYS_l_L2L_ETAG2                 =  204; #
our $FM_L2AR_CAM_KEYS_h_L2L_ETAG2                 =  215; #
our $FM_L2AR_CAM_KEYS_l_POL2_TAG2_TOP             =  290; #
our $FM_L2AR_CAM_KEYS_h_POL2_TAG2_TOP             =  293; #
our $FM_L2AR_CAM_KEYS_l_L2F_ISTATE                =  152; #
our $FM_L2AR_CAM_KEYS_h_L2F_ISTATE                =  164; #
our $FM_L2AR_CAM_KEYS_l_DROP_CODE                 =  336; #
our $FM_L2AR_CAM_KEYS_h_DROP_CODE                 =  343; #
our $FM_L2AR_CAM_KEYS_l_MA2_TAG                   =  178; #
our $FM_L2AR_CAM_KEYS_h_MA2_TAG                   =  189; #
our $FM_L2AR_CAM_KEYS_b_DGLORT_TAG                =  165; #
our $FM_L2AR_CAM_KEYS_l_DGLORT                    =  298; #
our $FM_L2AR_CAM_KEYS_h_DGLORT                    =  313; #
our $FM_L2AR_CAM_KEYS_l_L2_TYPE_ID2               =  274; #
our $FM_L2AR_CAM_KEYS_h_L2_TYPE_ID2               =  277; #
our $FM_L2AR_CAM_KEYS_l_POL1_TAG1_TOP             =  278; #
our $FM_L2AR_CAM_KEYS_h_POL1_TAG1_TOP             =  281; #
our $FM_L2AR_CAM_KEYS_l_POL2_TAG1_TOP             =  286; #
our $FM_L2AR_CAM_KEYS_h_POL2_TAG1_TOP             =  289; #
our $FM_L2AR_CAM_KEYS_l_MA2_HPV                   =  352; #
our $FM_L2AR_CAM_KEYS_h_MA2_HPV                   =  355; #
our $FM_L2AR_CAM_KEYS_l_ACTION_FLAGS              =  76; #
our $FM_L2AR_CAM_KEYS_h_ACTION_FLAGS              =  151; #
our $FM_L2AR_CAM_KEYS_l_L2_SMAC_ID3               =  269; #
our $FM_L2AR_CAM_KEYS_h_L2_SMAC_ID3               =  273; #
our $FM_L2AR_CAM_KEYS_l_ACTION_DATA_W8F           =  256; #
our $FM_L2AR_CAM_KEYS_h_ACTION_DATA_W8F           =  263; #
our $FM_L2AR_CAM_KEYS_l_POL3_TAG_TOP              =  294; #
our $FM_L2AR_CAM_KEYS_h_POL3_TAG_TOP              =  297; #
our $FM_L2AR_CAM_KEYS_b_MA1_LOOKUP                =  350; #
our $FM_L2AR_CAM_KEYS_l_ALU46_Z                   =  232; #
our $FM_L2AR_CAM_KEYS_h_ALU46_Z                   =  247; #
our $FM_L2AR_CAM_KEYS_l_L2_DMAC_ID3               =  264; #
our $FM_L2AR_CAM_KEYS_h_L2_DMAC_ID3               =  268; #
our $FM_L2AR_CAM_KEYS_l_ALU13_Z                   =  216; #
our $FM_L2AR_CAM_KEYS_h_ALU13_Z                   =  231; #
our $FM_L2AR_CAM_KEYS_l_SGLORT                    =  320; #
our $FM_L2AR_CAM_KEYS_h_SGLORT                    =  335; #
our $FM_L2AR_CAM_KEYS_l_SMASK                     =  0; #
our $FM_L2AR_CAM_KEYS_h_SMASK                     =  75; #
our $FM_L2AR_CAM_KEYS_l_MA1_HPV                   =  344; #
our $FM_L2AR_CAM_KEYS_h_MA1_HPV                   =  347; #
our $FM_L2AR_CAM_KEYS_l_MA2_MPV                   =  356; #
our $FM_L2AR_CAM_KEYS_h_MA2_MPV                   =  359; #
our $FM_L2AR_CAM_KEYS_b_MA2_LOOKUP                =  351; #
our $FM_L2AR_CAM_KEYS_l_L2L_ITAG1                 =  360; #
our $FM_L2AR_CAM_KEYS_h_L2L_ITAG1                 =  371; #
our $FM_L2AR_CAM_KEYS_l_MA1_TAG                   =  166; #
our $FM_L2AR_CAM_KEYS_h_MA1_TAG                   =  177; #
our $FM_L2AR_CAM_KEYS_l_POL1_TAG2_TOP             =  282; #
our $FM_L2AR_CAM_KEYS_h_POL1_TAG2_TOP             =  285; #
our $FM_L2AR_CAM_KEYS_b_MA2_FID2_IVL              =  349; #
our $FM_L2AR_CAM_KEYS_l_ISL_USER                  =  248; #
our $FM_L2AR_CAM_KEYS_h_ISL_USER                  =  255; #
our $FM_L2AR_CAM_KEYS_b_MA1_FID2_IVL              =  348; #
our $FM_L2AR_CAM_KEYS_l_L2L_ITAG2                 =  372; #
our $FM_L2AR_CAM_KEYS_h_L2L_ITAG2                 =  383; #


our $FM_MOD_CAM_KEYS_WIDTH                        =  2; #
our $FM_MOD_CAM_KEYS_BITS                         =  48; #

our $FM_MOD_CAM_KEYS_b_L2_VID_EQUAL               =  8; #
our $FM_MOD_CAM_KEYS_b_TRUNC                      =  6; #
our $FM_MOD_CAM_KEYS_l_DST_PORT_Tag               =  12; #
our $FM_MOD_CAM_KEYS_h_DST_PORT_Tag               =  21; #
our $FM_MOD_CAM_KEYS_l_MOD_FLAGS                  =  24; #
our $FM_MOD_CAM_KEYS_h_MOD_FLAGS                  =  47; #
our $FM_MOD_CAM_KEYS_b_MIR_RX                     =  1; #
our $FM_MOD_CAM_KEYS_l_TX_PORT_Tag                =  10; #
our $FM_MOD_CAM_KEYS_h_TX_PORT_Tag                =  11; #
our $FM_MOD_CAM_KEYS_b_L2_TAG                     =  7; #
our $FM_MOD_CAM_KEYS_b_L2_VLAN1_TX_Tagged         =  22; #
our $FM_MOD_CAM_KEYS_b_PAUSE                      =  0; #
our $FM_MOD_CAM_KEYS_b_MIR_TX                     =  2; #
our $FM_MOD_CAM_KEYS_b_MAP_PRI                    =  5; #
our $FM_MOD_CAM_KEYS_l_MIR_NUM                    =  3; #
our $FM_MOD_CAM_KEYS_h_MIR_NUM                    =  4; #
our $FM_MOD_CAM_KEYS_b_MCAST_TAG                  =  9; #
our $FM_MOD_CAM_KEYS_b_L2_VLAN2_TX_Tagged         =  23; #


our $FM_MCAST_VLAN_ENTRY_WIDTH                    =  1; #
our $FM_MCAST_VLAN_ENTRY_BITS                     =  15; #

our $FM_MCAST_VLAN_ENTRY_b_Last                   =  14; #
our $FM_MCAST_VLAN_ENTRY_l_VID                    =  0; #
our $FM_MCAST_VLAN_ENTRY_h_VID                    =  11; #
our $FM_MCAST_VLAN_ENTRY_b_L2_TAG                 =  13; #
our $FM_MCAST_VLAN_ENTRY_b_MCAST_TAG              =  12; #


our $FM_FFU_CAM_KEYS_WIDTH                        =  2; #
our $FM_FFU_CAM_KEYS_BITS                         =  38; #

our $FM_FFU_CAM_KEYS_l_Case                       =  36; #
our $FM_FFU_CAM_KEYS_h_Case                       =  37; #
our $FM_FFU_CAM_KEYS_l_Key                        =  0; #
our $FM_FFU_CAM_KEYS_h_Key                        =  35; #


our $FM_FFU_SCENARIO_KEYS_WIDTH                   =  1; #
our $FM_FFU_SCENARIO_KEYS_BITS                    =  32; #

our $FM_FFU_SCENARIO_KEYS_l_VID1_TAG              =  28; #
our $FM_FFU_SCENARIO_KEYS_h_VID1_TAG              =  29; #
our $FM_FFU_SCENARIO_KEYS_l_VID2_TAG              =  30; #
our $FM_FFU_SCENARIO_KEYS_h_VID2_TAG              =  31; #
our $FM_FFU_SCENARIO_KEYS_l_SCENARIO_FLAGS        =  0; #
our $FM_FFU_SCENARIO_KEYS_h_SCENARIO_FLAGS        =  15; #
our $FM_FFU_SCENARIO_KEYS_l_SRC_PORT_ID3          =  16; #
our $FM_FFU_SCENARIO_KEYS_h_SRC_PORT_ID3          =  23; #
our $FM_FFU_SCENARIO_KEYS_l_L2_DMAC_ID1           =  24; #
our $FM_FFU_SCENARIO_KEYS_h_L2_DMAC_ID1           =  27; #



# Registers

sub FM_AN_37_BASE_PAGE_TX {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00006 + $FM_EPL_BASE + ($word)); # 0xE0006 EPL_BASE
}

sub FM_AN_37_BASE_PAGE_TX_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_AN_37_BASE_PAGE_TX_WIDTH                  =  2; #
our $FM_AN_37_BASE_PAGE_TX_BITS                   =  16; #

sub FM_AN_37_BASE_PAGE_TX_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_AN_37_BASE_PAGE_TX_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_AN_37_BASE_PAGE_TX_STRIDE_0               =  128; #
our $FM_AN_37_BASE_PAGE_TX_STRIDE_1               =  1024; #


sub FM_AN_37_BASE_PAGE_RX {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00008 + $FM_EPL_BASE); # 0xE0008 EPL_BASE
}

sub FM_AN_37_BASE_PAGE_RX_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_AN_37_BASE_PAGE_RX_WIDTH                  =  1; #
our $FM_AN_37_BASE_PAGE_RX_BITS                   =  16; #

sub FM_AN_37_BASE_PAGE_RX_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_AN_37_BASE_PAGE_RX_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_AN_37_BASE_PAGE_RX_STRIDE_0               =  128; #
our $FM_AN_37_BASE_PAGE_RX_STRIDE_1               =  1024; #


sub FM_MULTI_PURPOSE_ERROR_COUNTER {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00036 + $FM_EPL_BASE); # 0xE0036 EPL_BASE
}

sub FM_MULTI_PURPOSE_ERROR_COUNTER_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_MULTI_PURPOSE_ERROR_COUNTER_WIDTH         =  1; #
our $FM_MULTI_PURPOSE_ERROR_COUNTER_BITS          =  32; #

sub FM_MULTI_PURPOSE_ERROR_COUNTER_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_MULTI_PURPOSE_ERROR_COUNTER_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_MULTI_PURPOSE_ERROR_COUNTER_STRIDE_0      =  128; #
our $FM_MULTI_PURPOSE_ERROR_COUNTER_STRIDE_1      =  1024; #


sub FM_AN_73_BASE_PAGE_TX {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00006 + $FM_EPL_BASE + ($word)); # 0xE0006 EPL_BASE
}

sub FM_AN_73_BASE_PAGE_TX_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_AN_73_BASE_PAGE_TX_WIDTH                  =  2; #
our $FM_AN_73_BASE_PAGE_TX_BITS                   =  48; #

sub FM_AN_73_BASE_PAGE_TX_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_AN_73_BASE_PAGE_TX_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_AN_73_BASE_PAGE_TX_STRIDE_0               =  128; #
our $FM_AN_73_BASE_PAGE_TX_STRIDE_1               =  1024; #


sub FM_SGMII_AN_TX_CONFIG {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00006 + $FM_EPL_BASE + ($word)); # 0xE0006 EPL_BASE
}

sub FM_SGMII_AN_TX_CONFIG_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_SGMII_AN_TX_CONFIG_WIDTH                  =  2; #
our $FM_SGMII_AN_TX_CONFIG_BITS                   =  16; #

sub FM_SGMII_AN_TX_CONFIG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_SGMII_AN_TX_CONFIG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_SGMII_AN_TX_CONFIG_STRIDE_0               =  128; #
our $FM_SGMII_AN_TX_CONFIG_STRIDE_1               =  1024; #


sub FM_AN_37_NEXT_PAGE_TX {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00006 + $FM_EPL_BASE + ($word)); # 0xE0006 EPL_BASE
}

sub FM_AN_37_NEXT_PAGE_TX_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_AN_37_NEXT_PAGE_TX_WIDTH                  =  2; #
our $FM_AN_37_NEXT_PAGE_TX_BITS                   =  16; #

sub FM_AN_37_NEXT_PAGE_TX_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_AN_37_NEXT_PAGE_TX_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_AN_37_NEXT_PAGE_TX_STRIDE_0               =  128; #
our $FM_AN_37_NEXT_PAGE_TX_STRIDE_1               =  1024; #


sub FM_SGMII_AN_TIMER_CFG {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x0002C + $FM_EPL_BASE); # 0xE002C EPL_BASE
}

sub FM_SGMII_AN_TIMER_CFG_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_SGMII_AN_TIMER_CFG_WIDTH                  =  1; #
our $FM_SGMII_AN_TIMER_CFG_BITS                   =  11; #

sub FM_SGMII_AN_TIMER_CFG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_SGMII_AN_TIMER_CFG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_SGMII_AN_TIMER_CFG_STRIDE_0               =  128; #
our $FM_SGMII_AN_TIMER_CFG_STRIDE_1               =  1024; #


sub FM_PCS_40GBASER_RX_BIP_STATUS {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00036 + $FM_EPL_BASE); # 0xE0036 EPL_BASE
}

sub FM_PCS_40GBASER_RX_BIP_STATUS_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_PCS_40GBASER_RX_BIP_STATUS_WIDTH          =  1; #
our $FM_PCS_40GBASER_RX_BIP_STATUS_BITS           =  32; #

sub FM_PCS_40GBASER_RX_BIP_STATUS_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_PCS_40GBASER_RX_BIP_STATUS_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_PCS_40GBASER_RX_BIP_STATUS_STRIDE_0       =  128; #
our $FM_PCS_40GBASER_RX_BIP_STATUS_STRIDE_1       =  1024; #


sub FM_PCS_10GBASER_RX_BER_STATUS {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00036 + $FM_EPL_BASE); # 0xE0036 EPL_BASE
}

sub FM_PCS_10GBASER_RX_BER_STATUS_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_PCS_10GBASER_RX_BER_STATUS_WIDTH          =  1; #
our $FM_PCS_10GBASER_RX_BER_STATUS_BITS           =  32; #

sub FM_PCS_10GBASER_RX_BER_STATUS_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_PCS_10GBASER_RX_BER_STATUS_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_PCS_10GBASER_RX_BER_STATUS_STRIDE_0       =  128; #
our $FM_PCS_10GBASER_RX_BER_STATUS_STRIDE_1       =  1024; #


sub FM_AN_37_TIMER_CFG {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x0002C + $FM_EPL_BASE); # 0xE002C EPL_BASE
}

sub FM_AN_37_TIMER_CFG_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_AN_37_TIMER_CFG_WIDTH                     =  1; #
our $FM_AN_37_TIMER_CFG_BITS                      =  11; #

sub FM_AN_37_TIMER_CFG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_AN_37_TIMER_CFG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_AN_37_TIMER_CFG_STRIDE_0                  =  128; #
our $FM_AN_37_TIMER_CFG_STRIDE_1                  =  1024; #


sub FM_AN_73_PAGE_RX_STATUS {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x0000A + $FM_EPL_BASE + ($word)); # 0xE000A EPL_BASE
}

sub FM_AN_73_PAGE_RX_STATUS_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_AN_73_PAGE_RX_STATUS_WIDTH                =  2; #
our $FM_AN_73_PAGE_RX_STATUS_BITS                 =  48; #

sub FM_AN_73_PAGE_RX_STATUS_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_AN_73_PAGE_RX_STATUS_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_AN_73_PAGE_RX_STATUS_STRIDE_0             =  128; #
our $FM_AN_73_PAGE_RX_STATUS_STRIDE_1             =  1024; #


sub FM_NEXTHOP_WIDE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00000 + $FM_NEXTHOP_BASE + ($word)); # 0x160000 NEXTHOP_BASE
}

sub FM_NEXTHOP_WIDE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_NEXTHOP_WIDE_WIDTH                        =  4; #
our $FM_NEXTHOP_WIDE_BITS                         =  128; #

sub FM_NEXTHOP_WIDE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32768;
}
our $FM_NEXTHOP_WIDE_STRIDE                       =  4; #


sub FM_NEXTHOP_NARROW_UNICAST {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00000 + $FM_NEXTHOP_BASE + ($word)); # 0x160000 NEXTHOP_BASE
}

sub FM_NEXTHOP_NARROW_UNICAST_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_NEXTHOP_NARROW_UNICAST_WIDTH              =  2; #
our $FM_NEXTHOP_NARROW_UNICAST_BITS               =  64; #

sub FM_NEXTHOP_NARROW_UNICAST_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 65536;
}
our $FM_NEXTHOP_NARROW_UNICAST_STRIDE             =  2; #


sub FM_FFU_SLICE_ACTION_MISC {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x04000 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x81000 + $FM_FFU_BASE + ($word)); # 0x381000 FFU_BASE
}

sub FM_FFU_SLICE_ACTION_MISC_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_FFU_SLICE_ACTION_MISC_WIDTH               =  2; #
our $FM_FFU_SLICE_ACTION_MISC_BITS                =  44; #

sub FM_FFU_SLICE_ACTION_MISC_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 1024;
}
sub FM_FFU_SLICE_ACTION_MISC_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 24;
}
our $FM_FFU_SLICE_ACTION_MISC_STRIDE_0            =  2; #
our $FM_FFU_SLICE_ACTION_MISC_STRIDE_1            =  16384; #


sub FM_FFU_SLICE_ACTION {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x04000 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x81000 + $FM_FFU_BASE + ($word)); # 0x381000 FFU_BASE
}

sub FM_FFU_SLICE_ACTION_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_FFU_SLICE_ACTION_WIDTH                    =  2; #
our $FM_FFU_SLICE_ACTION_BITS                     =  44; #

sub FM_FFU_SLICE_ACTION_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 1024;
}
sub FM_FFU_SLICE_ACTION_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 24;
}
our $FM_FFU_SLICE_ACTION_STRIDE_0                 =  2; #
our $FM_FFU_SLICE_ACTION_STRIDE_1                 =  16384; #


sub FM_AN_37_NEXT_PAGE_RX {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00008 + $FM_EPL_BASE); # 0xE0008 EPL_BASE
}

sub FM_AN_37_NEXT_PAGE_RX_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_AN_37_NEXT_PAGE_RX_WIDTH                  =  1; #
our $FM_AN_37_NEXT_PAGE_RX_BITS                   =  16; #

sub FM_AN_37_NEXT_PAGE_RX_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_AN_37_NEXT_PAGE_RX_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_AN_37_NEXT_PAGE_RX_STRIDE_0               =  128; #
our $FM_AN_37_NEXT_PAGE_RX_STRIDE_1               =  1024; #


sub FM_FFU_BST_ACTION_ROUTE {
    my ($self, $index2, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 5, -1);

    return (0x10000 * (($index2) - 0) + 0x00800 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x00000 + $FM_FFU_BASE + ($word)); # 0x300000 FFU_BASE
}

sub FM_FFU_BST_ACTION_ROUTE_DEFAULT {
    my ($self, $index2, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 5, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_FFU_BST_ACTION_ROUTE_WIDTH                =  2; #
our $FM_FFU_BST_ACTION_ROUTE_BITS                 =  50; #

sub FM_FFU_BST_ACTION_ROUTE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 1024;
}
sub FM_FFU_BST_ACTION_ROUTE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
sub FM_FFU_BST_ACTION_ROUTE_ENTRIES_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_FFU_BST_ACTION_ROUTE_STRIDE_0             =  2; #
our $FM_FFU_BST_ACTION_ROUTE_STRIDE_1             =  2048; #
our $FM_FFU_BST_ACTION_ROUTE_STRIDE_2             =  65536; #


sub FM_AN_37_PAGE_RX_STATUS {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00008 + $FM_EPL_BASE); # 0xE0008 EPL_BASE
}

sub FM_AN_37_PAGE_RX_STATUS_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_AN_37_PAGE_RX_STATUS_WIDTH                =  1; #
our $FM_AN_37_PAGE_RX_STATUS_BITS                 =  16; #

sub FM_AN_37_PAGE_RX_STATUS_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_AN_37_PAGE_RX_STATUS_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_AN_37_PAGE_RX_STATUS_STRIDE_0             =  128; #
our $FM_AN_37_PAGE_RX_STATUS_STRIDE_1             =  1024; #


sub FM_AN_73_BASE_PAGE_RX {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x0000A + $FM_EPL_BASE + ($word)); # 0xE000A EPL_BASE
}

sub FM_AN_73_BASE_PAGE_RX_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_AN_73_BASE_PAGE_RX_WIDTH                  =  2; #
our $FM_AN_73_BASE_PAGE_RX_BITS                   =  48; #

sub FM_AN_73_BASE_PAGE_RX_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_AN_73_BASE_PAGE_RX_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_AN_73_BASE_PAGE_RX_STRIDE_0               =  128; #
our $FM_AN_73_BASE_PAGE_RX_STRIDE_1               =  1024; #


sub FM_EPL_PORT_MULTI_PURPOSE_CFG_A {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00006 + $FM_EPL_BASE + ($word)); # 0xE0006 EPL_BASE
}

sub FM_EPL_PORT_MULTI_PURPOSE_CFG_A_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_EPL_PORT_MULTI_PURPOSE_CFG_A_WIDTH        =  2; #
our $FM_EPL_PORT_MULTI_PURPOSE_CFG_A_BITS         =  48; #

sub FM_EPL_PORT_MULTI_PURPOSE_CFG_A_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_EPL_PORT_MULTI_PURPOSE_CFG_A_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_EPL_PORT_MULTI_PURPOSE_CFG_A_STRIDE_0     =  128; #
our $FM_EPL_PORT_MULTI_PURPOSE_CFG_A_STRIDE_1     =  1024; #


sub FM_AN_73_NEXT_PAGE_TX {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00006 + $FM_EPL_BASE + ($word)); # 0xE0006 EPL_BASE
}

sub FM_AN_73_NEXT_PAGE_TX_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_AN_73_NEXT_PAGE_TX_WIDTH                  =  2; #
our $FM_AN_73_NEXT_PAGE_TX_BITS                   =  48; #

sub FM_AN_73_NEXT_PAGE_TX_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_AN_73_NEXT_PAGE_TX_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_AN_73_NEXT_PAGE_TX_STRIDE_0               =  128; #
our $FM_AN_73_NEXT_PAGE_TX_STRIDE_1               =  1024; #


sub FM_FFU_BST_ACTION_MISC {
    my ($self, $index2, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 5, -1);

    return (0x10000 * (($index2) - 0) + 0x00800 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x00000 + $FM_FFU_BASE + ($word)); # 0x300000 FFU_BASE
}

sub FM_FFU_BST_ACTION_MISC_DEFAULT {
    my ($self, $index2, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 5, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_FFU_BST_ACTION_MISC_WIDTH                 =  2; #
our $FM_FFU_BST_ACTION_MISC_BITS                  =  50; #

sub FM_FFU_BST_ACTION_MISC_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 1024;
}
sub FM_FFU_BST_ACTION_MISC_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
sub FM_FFU_BST_ACTION_MISC_ENTRIES_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_FFU_BST_ACTION_MISC_STRIDE_0              =  2; #
our $FM_FFU_BST_ACTION_MISC_STRIDE_1              =  2048; #
our $FM_FFU_BST_ACTION_MISC_STRIDE_2              =  65536; #


sub FM_MOD_L2_VLAN2_TX_TAGGED {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x04000 + $FM_MOD_BASE + ($word)); # 0x154000 MOD_BASE
}

sub FM_MOD_L2_VLAN2_TX_TAGGED_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_MOD_L2_VLAN2_TX_TAGGED_WIDTH              =  3; #
our $FM_MOD_L2_VLAN2_TX_TAGGED_BITS               =  76; #

sub FM_MOD_L2_VLAN2_TX_TAGGED_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}
our $FM_MOD_L2_VLAN2_TX_TAGGED_STRIDE             =  4; #


sub FM_NEXTHOP_NARROW_MULTICAST {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00000 + $FM_NEXTHOP_BASE + ($word)); # 0x160000 NEXTHOP_BASE
}

sub FM_NEXTHOP_NARROW_MULTICAST_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_NEXTHOP_NARROW_MULTICAST_WIDTH            =  2; #
our $FM_NEXTHOP_NARROW_MULTICAST_BITS             =  64; #

sub FM_NEXTHOP_NARROW_MULTICAST_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 65536;
}
our $FM_NEXTHOP_NARROW_MULTICAST_STRIDE           =  2; #


sub FM_LCI_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00000 + $FM_MGMT1_BASE); # 0x00000 MGMT1_BASE
}

sub FM_LCI_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_LCI_CFG_WIDTH                             =  1; #
our $FM_LCI_CFG_BITS                              =  2; #



sub FM_LCI_RX_FIFO {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00001 + $FM_MGMT1_BASE); # 0x00001 MGMT1_BASE
}

sub FM_LCI_RX_FIFO_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_LCI_RX_FIFO_WIDTH                         =  1; #
our $FM_LCI_RX_FIFO_BITS                          =  32; #



sub FM_LCI_TX_FIFO {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00002 + $FM_MGMT1_BASE); # 0x00002 MGMT1_BASE
}

sub FM_LCI_TX_FIFO_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_LCI_TX_FIFO_WIDTH                         =  1; #
our $FM_LCI_TX_FIFO_BITS                          =  32; #



sub FM_LCI_IP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00003 + $FM_MGMT1_BASE); # 0x00003 MGMT1_BASE
}

sub FM_LCI_IP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_LCI_IP_WIDTH                              =  1; #
our $FM_LCI_IP_BITS                               =  2; #



sub FM_LCI_IM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00004 + $FM_MGMT1_BASE); # 0x00004 MGMT1_BASE
}

sub FM_LCI_IM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000003;
}

our $FM_LCI_IM_WIDTH                              =  1; #
our $FM_LCI_IM_BITS                               =  2; #



sub FM_LCI_STATUS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00005 + $FM_MGMT1_BASE); # 0x00005 MGMT1_BASE
}

sub FM_LCI_STATUS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000001;
}

our $FM_LCI_STATUS_WIDTH                          =  1; #
our $FM_LCI_STATUS_BITS                           =  15; #



sub FM_FATAL_CODE {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00006 + $FM_MGMT1_BASE); # 0x00006 MGMT1_BASE
}

sub FM_FATAL_CODE_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FATAL_CODE_WIDTH                          =  1; #
our $FM_FATAL_CODE_BITS                           =  8; #



sub FM_LAST_FATAL_CODE {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00007 + $FM_MGMT1_BASE); # 0x00007 MGMT1_BASE
}

sub FM_LAST_FATAL_CODE_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_LAST_FATAL_CODE_WIDTH                     =  1; #
our $FM_LAST_FATAL_CODE_BITS                      =  8; #



sub FM_FATAL_COUNT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00008 + $FM_MGMT1_BASE); # 0x00008 MGMT1_BASE
}

sub FM_FATAL_COUNT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FATAL_COUNT_WIDTH                         =  1; #
our $FM_FATAL_COUNT_BITS                          =  8; #



sub FM_SOFT_RESET {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00009 + $FM_MGMT1_BASE); # 0x00009 MGMT1_BASE
}

sub FM_SOFT_RESET_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0000001f;
}

our $FM_SOFT_RESET_WIDTH                          =  1; #
our $FM_SOFT_RESET_BITS                           =  5; #



sub FM_RESET_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0000A + $FM_MGMT1_BASE); # 0x0000A MGMT1_BASE
}

sub FM_RESET_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00004010;
}

our $FM_RESET_CFG_WIDTH                           =  1; #
our $FM_RESET_CFG_BITS                            =  16; #



sub FM_WATCHDOG_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0000B + $FM_MGMT1_BASE); # 0x0000B MGMT1_BASE
}

sub FM_WATCHDOG_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_WATCHDOG_CFG_WIDTH                        =  1; #
our $FM_WATCHDOG_CFG_BITS                         =  1; #



sub FM_MGMT_SCRATCH {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0000C + $FM_MGMT1_BASE); # 0x0000C MGMT1_BASE
}

sub FM_MGMT_SCRATCH_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MGMT_SCRATCH_WIDTH                        =  1; #
our $FM_MGMT_SCRATCH_BITS                         =  32; #

sub FM_MGMT_SCRATCH_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
our $FM_MGMT_SCRATCH_STRIDE                       =  1; #


sub FM_VITAL_PRODUCT_DATA {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00304 + $FM_MGMT1_BASE); # 0x00304 MGMT1_BASE
}

sub FM_VITAL_PRODUCT_DATA_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0000ae20;
}

our $FM_VITAL_PRODUCT_DATA_WIDTH                  =  1; #
our $FM_VITAL_PRODUCT_DATA_BITS                   =  16; #



sub FM_PCI_CFG_ID {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00000 + $FM_PCIE_BASE); # 0x01000 PCIE_BASE
}

sub FM_PCI_CFG_ID_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x17701823;
}

our $FM_PCI_CFG_ID_WIDTH                          =  1; #
our $FM_PCI_CFG_ID_BITS                           =  32; #



sub FM_PCI_CFG_CMD {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00001 + $FM_PCIE_BASE); # 0x01001 PCIE_BASE
}

sub FM_PCI_CFG_CMD_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00100000;
}

our $FM_PCI_CFG_CMD_WIDTH                         =  1; #
our $FM_PCI_CFG_CMD_BITS                          =  32; #



sub FM_PCI_CFG_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00002 + $FM_PCIE_BASE); # 0x01002 PCIE_BASE
}

sub FM_PCI_CFG_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CFG_1_WIDTH                           =  1; #
our $FM_PCI_CFG_1_BITS                            =  32; #



sub FM_PCI_CFG_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00003 + $FM_PCIE_BASE); # 0x01003 PCIE_BASE
}

sub FM_PCI_CFG_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CFG_2_WIDTH                           =  1; #
our $FM_PCI_CFG_2_BITS                            =  32; #



sub FM_PCI_CFG_BAR0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00004 + $FM_PCIE_BASE); # 0x01004 PCIE_BASE
}

sub FM_PCI_CFG_BAR0_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000004;
}

our $FM_PCI_CFG_BAR0_WIDTH                        =  1; #
our $FM_PCI_CFG_BAR0_BITS                         =  32; #



sub FM_PCI_CFG_BAR1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00005 + $FM_PCIE_BASE); # 0x01005 PCIE_BASE
}

sub FM_PCI_CFG_BAR1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CFG_BAR1_WIDTH                        =  1; #
our $FM_PCI_CFG_BAR1_BITS                         =  32; #



sub FM_PCI_CFG_BAR2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00006 + $FM_PCIE_BASE); # 0x01006 PCIE_BASE
}

sub FM_PCI_CFG_BAR2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CFG_BAR2_WIDTH                        =  1; #
our $FM_PCI_CFG_BAR2_BITS                         =  32; #



sub FM_PCI_CFG_BAR3 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00007 + $FM_PCIE_BASE); # 0x01007 PCIE_BASE
}

sub FM_PCI_CFG_BAR3_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CFG_BAR3_WIDTH                        =  1; #
our $FM_PCI_CFG_BAR3_BITS                         =  32; #



sub FM_PCI_CFG_BAR4 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00008 + $FM_PCIE_BASE); # 0x01008 PCIE_BASE
}

sub FM_PCI_CFG_BAR4_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CFG_BAR4_WIDTH                        =  1; #
our $FM_PCI_CFG_BAR4_BITS                         =  32; #



sub FM_PCI_CFG_BAR5 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00009 + $FM_PCIE_BASE); # 0x01009 PCIE_BASE
}

sub FM_PCI_CFG_BAR5_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CFG_BAR5_WIDTH                        =  1; #
our $FM_PCI_CFG_BAR5_BITS                         =  32; #



sub FM_PCI_CFG_CARDBUS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0000A + $FM_PCIE_BASE); # 0x0100A PCIE_BASE
}

sub FM_PCI_CFG_CARDBUS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CFG_CARDBUS_WIDTH                     =  1; #
our $FM_PCI_CFG_CARDBUS_BITS                      =  32; #



sub FM_PCI_CFG_SUBID {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0000B + $FM_PCIE_BASE); # 0x0100B PCIE_BASE
}

sub FM_PCI_CFG_SUBID_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CFG_SUBID_WIDTH                       =  1; #
our $FM_PCI_CFG_SUBID_BITS                        =  32; #



sub FM_PCI_CFG_EXP_ROM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0000C + $FM_PCIE_BASE); # 0x0100C PCIE_BASE
}

sub FM_PCI_CFG_EXP_ROM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CFG_EXP_ROM_WIDTH                     =  1; #
our $FM_PCI_CFG_EXP_ROM_BITS                      =  32; #



sub FM_PCI_CFG_CAP_PTR {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0000D + $FM_PCIE_BASE); # 0x0100D PCIE_BASE
}

sub FM_PCI_CFG_CAP_PTR_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000040;
}

our $FM_PCI_CFG_CAP_PTR_WIDTH                     =  1; #
our $FM_PCI_CFG_CAP_PTR_BITS                      =  32; #



sub FM_PCI_CFG_RSVD {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0000E + $FM_PCIE_BASE); # 0x0100E PCIE_BASE
}

sub FM_PCI_CFG_RSVD_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CFG_RSVD_WIDTH                        =  1; #
our $FM_PCI_CFG_RSVD_BITS                         =  32; #



sub FM_PCI_CFG_INT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0000F + $FM_PCIE_BASE); # 0x0100F PCIE_BASE
}

sub FM_PCI_CFG_INT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x000002ff;
}

our $FM_PCI_CFG_INT_WIDTH                         =  1; #
our $FM_PCI_CFG_INT_BITS                          =  32; #



sub FM_PCI_CFG_PM_CAP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00010 + $FM_PCIE_BASE); # 0x01010 PCIE_BASE
}

sub FM_PCI_CFG_PM_CAP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0xc9c35001;
}

our $FM_PCI_CFG_PM_CAP_WIDTH                      =  1; #
our $FM_PCI_CFG_PM_CAP_BITS                       =  32; #



sub FM_PCI_CFG_PM_CTRL {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00011 + $FM_PCIE_BASE); # 0x01011 PCIE_BASE
}

sub FM_PCI_CFG_PM_CTRL_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00008000;
}

our $FM_PCI_CFG_PM_CTRL_WIDTH                     =  1; #
our $FM_PCI_CFG_PM_CTRL_BITS                      =  32; #



sub FM_PCI_CFG_MSI_CAP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00014 + $FM_PCIE_BASE); # 0x01014 PCIE_BASE
}

sub FM_PCI_CFG_MSI_CAP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00807005;
}

our $FM_PCI_CFG_MSI_CAP_WIDTH                     =  1; #
our $FM_PCI_CFG_MSI_CAP_BITS                      =  25; #



sub FM_PCI_CFG_MSI_ADDR_LO {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00015 + $FM_PCIE_BASE); # 0x01015 PCIE_BASE
}

sub FM_PCI_CFG_MSI_ADDR_LO_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CFG_MSI_ADDR_LO_WIDTH                 =  1; #
our $FM_PCI_CFG_MSI_ADDR_LO_BITS                  =  32; #



sub FM_PCI_CFG_MSI_ADDR_HI {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00016 + $FM_PCIE_BASE); # 0x01016 PCIE_BASE
}

sub FM_PCI_CFG_MSI_ADDR_HI_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CFG_MSI_ADDR_HI_WIDTH                 =  1; #
our $FM_PCI_CFG_MSI_ADDR_HI_BITS                  =  32; #



sub FM_PCI_CFG_MSI_DATA {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00017 + $FM_PCIE_BASE); # 0x01017 PCIE_BASE
}

sub FM_PCI_CFG_MSI_DATA_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CFG_MSI_DATA_WIDTH                    =  1; #
our $FM_PCI_CFG_MSI_DATA_BITS                     =  16; #



sub FM_PCI_CFG_MSI_MASK {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00018 + $FM_PCIE_BASE); # 0x01018 PCIE_BASE
}

sub FM_PCI_CFG_MSI_MASK_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CFG_MSI_MASK_WIDTH                    =  1; #
our $FM_PCI_CFG_MSI_MASK_BITS                     =  32; #



sub FM_PCI_CFG_MSI_PENDING {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00019 + $FM_PCIE_BASE); # 0x01019 PCIE_BASE
}

sub FM_PCI_CFG_MSI_PENDING_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CFG_MSI_PENDING_WIDTH                 =  1; #
our $FM_PCI_CFG_MSI_PENDING_BITS                  =  32; #



sub FM_PCI_CFG_PCIE_CAP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0001C + $FM_PCIE_BASE); # 0x0101C PCIE_BASE
}

sub FM_PCI_CFG_PCIE_CAP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00020010;
}

our $FM_PCI_CFG_PCIE_CAP_WIDTH                    =  1; #
our $FM_PCI_CFG_PCIE_CAP_BITS                     =  30; #



sub FM_PCI_CFG_PCI_DEV_CAP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0001D + $FM_PCIE_BASE); # 0x0101D PCIE_BASE
}

sub FM_PCI_CFG_PCI_DEV_CAP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00008701;
}

our $FM_PCI_CFG_PCI_DEV_CAP_WIDTH                 =  1; #
our $FM_PCI_CFG_PCI_DEV_CAP_BITS                  =  32; #



sub FM_PCI_CFG_PCIE_DEV {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0001E + $FM_PCIE_BASE); # 0x0101E PCIE_BASE
}

sub FM_PCI_CFG_PCIE_DEV_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00102010;
}

our $FM_PCI_CFG_PCIE_DEV_WIDTH                    =  1; #
our $FM_PCI_CFG_PCIE_DEV_BITS                     =  22; #



sub FM_PCI_CFG_PCI_LINK_CAP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0001F + $FM_PCIE_BASE); # 0x0101F PCIE_BASE
}

sub FM_PCI_CFG_PCI_LINK_CAP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00036c42;
}

our $FM_PCI_CFG_PCI_LINK_CAP_WIDTH                =  1; #
our $FM_PCI_CFG_PCI_LINK_CAP_BITS                 =  32; #



sub FM_PCI_CFG_PCI_LINK_CTRL {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00020 + $FM_PCIE_BASE); # 0x01020 PCIE_BASE
}

sub FM_PCI_CFG_PCI_LINK_CTRL_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x10110000;
}

our $FM_PCI_CFG_PCI_LINK_CTRL_WIDTH               =  1; #
our $FM_PCI_CFG_PCI_LINK_CTRL_BITS                =  32; #



sub FM_PCI_CFG_DEV_CAP2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00025 + $FM_PCIE_BASE); # 0x01025 PCIE_BASE
}

sub FM_PCI_CFG_DEV_CAP2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0000001f;
}

our $FM_PCI_CFG_DEV_CAP2_WIDTH                    =  1; #
our $FM_PCI_CFG_DEV_CAP2_BITS                     =  5; #



sub FM_PCI_CFG_DEV_CTRL2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00026 + $FM_PCIE_BASE); # 0x01026 PCIE_BASE
}

sub FM_PCI_CFG_DEV_CTRL2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CFG_DEV_CTRL2_WIDTH                   =  1; #
our $FM_PCI_CFG_DEV_CTRL2_BITS                    =  5; #



sub FM_PCI_CFG_PCI_LINK_CAP2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00027 + $FM_PCIE_BASE); # 0x01027 PCIE_BASE
}

sub FM_PCI_CFG_PCI_LINK_CAP2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000006;
}

our $FM_PCI_CFG_PCI_LINK_CAP2_WIDTH               =  1; #
our $FM_PCI_CFG_PCI_LINK_CAP2_BITS                =  9; #



sub FM_PCI_CFG_PCI_LINK_CTRL2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00028 + $FM_PCIE_BASE); # 0x01028 PCIE_BASE
}

sub FM_PCI_CFG_PCI_LINK_CTRL2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00010002;
}

our $FM_PCI_CFG_PCI_LINK_CTRL2_WIDTH              =  1; #
our $FM_PCI_CFG_PCI_LINK_CTRL2_BITS               =  22; #



sub FM_PCI_ENDIANISM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00400 + $FM_PCIE_BASE); # 0x01400 PCIE_BASE
}

sub FM_PCI_ENDIANISM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_ENDIANISM_WIDTH                       =  1; #
our $FM_PCI_ENDIANISM_BITS                        =  32; #



sub FM_PCI_COMMAND {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00401 + $FM_PCIE_BASE); # 0x01401 PCIE_BASE
}

sub FM_PCI_COMMAND_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_COMMAND_WIDTH                         =  1; #
our $FM_PCI_COMMAND_BITS                          =  32; #



sub FM_PCI_STATUS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00402 + $FM_PCIE_BASE); # 0x01402 PCIE_BASE
}

sub FM_PCI_STATUS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_STATUS_WIDTH                          =  1; #
our $FM_PCI_STATUS_BITS                           =  6; #



sub FM_PCI_COALESCING {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00403 + $FM_PCIE_BASE); # 0x01403 PCIE_BASE
}

sub FM_PCI_COALESCING_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_COALESCING_WIDTH                      =  1; #
our $FM_PCI_COALESCING_BITS                       =  26; #



sub FM_PCI_RX_BD_BASE {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00404 + $FM_PCIE_BASE + ($word)); # 0x01404 PCIE_BASE
}

sub FM_PCI_RX_BD_BASE_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_PCI_RX_BD_BASE_WIDTH                      =  2; #
our $FM_PCI_RX_BD_BASE_BITS                       =  64; #



sub FM_PCI_RX_BD_END {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00406 + $FM_PCIE_BASE + ($word)); # 0x01406 PCIE_BASE
}

sub FM_PCI_RX_BD_END_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_PCI_RX_BD_END_WIDTH                       =  2; #
our $FM_PCI_RX_BD_END_BITS                        =  64; #



sub FM_PCI_TX_BD_BASE {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00408 + $FM_PCIE_BASE + ($word)); # 0x01408 PCIE_BASE
}

sub FM_PCI_TX_BD_BASE_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_PCI_TX_BD_BASE_WIDTH                      =  2; #
our $FM_PCI_TX_BD_BASE_BITS                       =  64; #



sub FM_PCI_TX_BD_END {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x0040A + $FM_PCIE_BASE + ($word)); # 0x0140A PCIE_BASE
}

sub FM_PCI_TX_BD_END_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_PCI_TX_BD_END_WIDTH                       =  2; #
our $FM_PCI_TX_BD_END_BITS                        =  64; #



sub FM_PCI_IP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0040C + $FM_PCIE_BASE); # 0x0140C PCIE_BASE
}

sub FM_PCI_IP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_IP_WIDTH                              =  1; #
our $FM_PCI_IP_BITS                               =  32; #



sub FM_PCI_IM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0040D + $FM_PCIE_BASE); # 0x0140D PCIE_BASE
}

sub FM_PCI_IM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0xffffffff;
}

our $FM_PCI_IM_WIDTH                              =  1; #
our $FM_PCI_IM_BITS                               =  32; #



sub FM_PCI_CURRENT_TX_DATA_PTR {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x0040E + $FM_PCIE_BASE + ($word)); # 0x0140E PCIE_BASE
}

sub FM_PCI_CURRENT_TX_DATA_PTR_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_PCI_CURRENT_TX_DATA_PTR_WIDTH             =  2; #
our $FM_PCI_CURRENT_TX_DATA_PTR_BITS              =  64; #



sub FM_PCI_CURRENT_RX_DATA_PTR {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00410 + $FM_PCIE_BASE + ($word)); # 0x01410 PCIE_BASE
}

sub FM_PCI_CURRENT_RX_DATA_PTR_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_PCI_CURRENT_RX_DATA_PTR_WIDTH             =  2; #
our $FM_PCI_CURRENT_RX_DATA_PTR_BITS              =  64; #



sub FM_PCI_CURRENT_TX_BD_PTR {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00412 + $FM_PCIE_BASE + ($word)); # 0x01412 PCIE_BASE
}

sub FM_PCI_CURRENT_TX_BD_PTR_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_PCI_CURRENT_TX_BD_PTR_WIDTH               =  2; #
our $FM_PCI_CURRENT_TX_BD_PTR_BITS                =  64; #



sub FM_PCI_CURRENT_RX_BD_PTR {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00414 + $FM_PCIE_BASE + ($word)); # 0x01414 PCIE_BASE
}

sub FM_PCI_CURRENT_RX_BD_PTR_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_PCI_CURRENT_RX_BD_PTR_WIDTH               =  2; #
our $FM_PCI_CURRENT_RX_BD_PTR_BITS                =  64; #



sub FM_PCI_TX_FRAME_LEN {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00416 + $FM_PCIE_BASE); # 0x01416 PCIE_BASE
}

sub FM_PCI_TX_FRAME_LEN_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00404000;
}

our $FM_PCI_TX_FRAME_LEN_WIDTH                    =  1; #
our $FM_PCI_TX_FRAME_LEN_BITS                     =  32; #



sub FM_PCI_SIZE {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00417 + $FM_PCIE_BASE); # 0x01417 PCIE_BASE
}

sub FM_PCI_SIZE_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0000071f;
}

our $FM_PCI_SIZE_WIDTH                            =  1; #
our $FM_PCI_SIZE_BITS                             =  11; #



sub FM_PCI_DMA_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00418 + $FM_PCIE_BASE); # 0x01418 PCIE_BASE
}

sub FM_PCI_DMA_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000075;
}

our $FM_PCI_DMA_CFG_WIDTH                         =  1; #
our $FM_PCI_DMA_CFG_BITS                          =  8; #



sub FM_PCI_FRAME_TIMEOUT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00419 + $FM_PCIE_BASE); # 0x01419 PCIE_BASE
}

sub FM_PCI_FRAME_TIMEOUT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x003ffff0;
}

our $FM_PCI_FRAME_TIMEOUT_WIDTH                   =  1; #
our $FM_PCI_FRAME_TIMEOUT_BITS                    =  22; #



sub FM_PCI_STAT_COUNTER {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0041A + $FM_PCIE_BASE); # 0x0141A PCIE_BASE
}

sub FM_PCI_STAT_COUNTER_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_STAT_COUNTER_WIDTH                    =  1; #
our $FM_PCI_STAT_COUNTER_BITS                     =  24; #



sub FM_PCI_STAT_NUM_PKTS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0041B + $FM_PCIE_BASE); # 0x0141B PCIE_BASE
}

sub FM_PCI_STAT_NUM_PKTS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_STAT_NUM_PKTS_WIDTH                   =  1; #
our $FM_PCI_STAT_NUM_PKTS_BITS                    =  32; #



sub FM_PCI_DEBUG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0041C + $FM_PCIE_BASE); # 0x0141C PCIE_BASE
}

sub FM_PCI_DEBUG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000003;
}

our $FM_PCI_DEBUG_WIDTH                           =  1; #
our $FM_PCI_DEBUG_BITS                            =  4; #



sub FM_PCI_CORE_CTRL_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0041D + $FM_PCIE_BASE); # 0x0141D PCIE_BASE
}

sub FM_PCI_CORE_CTRL_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0000c01f;
}

our $FM_PCI_CORE_CTRL_1_WIDTH                     =  1; #
our $FM_PCI_CORE_CTRL_1_BITS                      =  19; #



sub FM_PCI_CORE_CTRL_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0041E + $FM_PCIE_BASE); # 0x0141E PCIE_BASE
}

sub FM_PCI_CORE_CTRL_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_CTRL_2_WIDTH                     =  1; #
our $FM_PCI_CORE_CTRL_2_BITS                      =  7; #



sub FM_PCI_CORE_DEBUG_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0041F + $FM_PCIE_BASE); # 0x0141F PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_1_WIDTH                    =  1; #
our $FM_PCI_CORE_DEBUG_1_BITS                     =  28; #



sub FM_PCI_CORE_DEBUG_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00420 + $FM_PCIE_BASE); # 0x01420 PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_2_WIDTH                    =  1; #
our $FM_PCI_CORE_DEBUG_2_BITS                     =  32; #



sub FM_PCI_CORE_DEBUG_3 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00421 + $FM_PCIE_BASE); # 0x01421 PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_3_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_3_WIDTH                    =  1; #
our $FM_PCI_CORE_DEBUG_3_BITS                     =  32; #



sub FM_PCI_CORE_DEBUG_4 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00422 + $FM_PCIE_BASE); # 0x01422 PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_4_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_4_WIDTH                    =  1; #
our $FM_PCI_CORE_DEBUG_4_BITS                     =  32; #



sub FM_PCI_CORE_DEBUG_5 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00423 + $FM_PCIE_BASE); # 0x01423 PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_5_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_5_WIDTH                    =  1; #
our $FM_PCI_CORE_DEBUG_5_BITS                     =  32; #



sub FM_PCI_CORE_DEBUG_6 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00424 + $FM_PCIE_BASE); # 0x01424 PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_6_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_6_WIDTH                    =  1; #
our $FM_PCI_CORE_DEBUG_6_BITS                     =  32; #



sub FM_PCI_CORE_DEBUG_7 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00425 + $FM_PCIE_BASE); # 0x01425 PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_7_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_7_WIDTH                    =  1; #
our $FM_PCI_CORE_DEBUG_7_BITS                     =  32; #



sub FM_PCI_CORE_DEBUG_8 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00426 + $FM_PCIE_BASE); # 0x01426 PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_8_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_8_WIDTH                    =  1; #
our $FM_PCI_CORE_DEBUG_8_BITS                     =  32; #



sub FM_PCI_CORE_DEBUG_9 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00427 + $FM_PCIE_BASE); # 0x01427 PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_9_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_9_WIDTH                    =  1; #
our $FM_PCI_CORE_DEBUG_9_BITS                     =  32; #



sub FM_PCI_CORE_DEBUG_10 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00428 + $FM_PCIE_BASE); # 0x01428 PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_10_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_10_WIDTH                   =  1; #
our $FM_PCI_CORE_DEBUG_10_BITS                    =  32; #



sub FM_PCI_CORE_DEBUG_11 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00429 + $FM_PCIE_BASE); # 0x01429 PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_11_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_11_WIDTH                   =  1; #
our $FM_PCI_CORE_DEBUG_11_BITS                    =  32; #



sub FM_PCI_CORE_DEBUG_12 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0042A + $FM_PCIE_BASE); # 0x0142A PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_12_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_12_WIDTH                   =  1; #
our $FM_PCI_CORE_DEBUG_12_BITS                    =  32; #



sub FM_PCI_CORE_DEBUG_13 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0042B + $FM_PCIE_BASE); # 0x0142B PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_13_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_13_WIDTH                   =  1; #
our $FM_PCI_CORE_DEBUG_13_BITS                    =  32; #



sub FM_PCI_CORE_DEBUG_14 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0042C + $FM_PCIE_BASE); # 0x0142C PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_14_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_14_WIDTH                   =  1; #
our $FM_PCI_CORE_DEBUG_14_BITS                    =  32; #



sub FM_PCI_CORE_DEBUG_15 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0042D + $FM_PCIE_BASE); # 0x0142D PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_15_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_15_WIDTH                   =  1; #
our $FM_PCI_CORE_DEBUG_15_BITS                    =  32; #



sub FM_PCI_CORE_DEBUG_16 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0042E + $FM_PCIE_BASE); # 0x0142E PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_16_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_16_WIDTH                   =  1; #
our $FM_PCI_CORE_DEBUG_16_BITS                    =  32; #



sub FM_PCI_CORE_DEBUG_17 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0042F + $FM_PCIE_BASE); # 0x0142F PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_17_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_17_WIDTH                   =  1; #
our $FM_PCI_CORE_DEBUG_17_BITS                    =  5; #



sub FM_PCI_CORE_DEBUG_18 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00430 + $FM_PCIE_BASE); # 0x01430 PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_18_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_18_WIDTH                   =  1; #
our $FM_PCI_CORE_DEBUG_18_BITS                    =  32; #



sub FM_PCI_CORE_DEBUG_19 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00431 + $FM_PCIE_BASE); # 0x01431 PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_19_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_19_WIDTH                   =  1; #
our $FM_PCI_CORE_DEBUG_19_BITS                    =  32; #



sub FM_PCI_CORE_DEBUG_20 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00432 + $FM_PCIE_BASE); # 0x01432 PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_20_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_20_WIDTH                   =  1; #
our $FM_PCI_CORE_DEBUG_20_BITS                    =  32; #



sub FM_PCI_CORE_DEBUG_21 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00433 + $FM_PCIE_BASE); # 0x01433 PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_21_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_21_WIDTH                   =  1; #
our $FM_PCI_CORE_DEBUG_21_BITS                    =  32; #



sub FM_PCI_CORE_DEBUG_22 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00434 + $FM_PCIE_BASE); # 0x01434 PCIE_BASE
}

sub FM_PCI_CORE_DEBUG_22_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_CORE_DEBUG_22_WIDTH                   =  1; #
our $FM_PCI_CORE_DEBUG_22_BITS                    =  32; #



sub FM_PCI_SERDES_CTRL_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00435 + $FM_PCIE_BASE); # 0x01435 PCIE_BASE
}

sub FM_PCI_SERDES_CTRL_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0f121f34;
}

our $FM_PCI_SERDES_CTRL_1_WIDTH                   =  1; #
our $FM_PCI_SERDES_CTRL_1_BITS                    =  31; #



sub FM_PCI_SERDES_CTRL_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00436 + $FM_PCIE_BASE); # 0x01436 PCIE_BASE
}

sub FM_PCI_SERDES_CTRL_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000008;
}

our $FM_PCI_SERDES_CTRL_2_WIDTH                   =  1; #
our $FM_PCI_SERDES_CTRL_2_BITS                    =  14; #



sub FM_PCI_SERDES_CTRL_3 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00437 + $FM_PCIE_BASE); # 0x01437 PCIE_BASE
}

sub FM_PCI_SERDES_CTRL_3_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x1401aaaa;
}

our $FM_PCI_SERDES_CTRL_3_WIDTH                   =  1; #
our $FM_PCI_SERDES_CTRL_3_BITS                    =  32; #



sub FM_PCI_SERDES_CTRL_4 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00438 + $FM_PCIE_BASE); # 0x01438 PCIE_BASE
}

sub FM_PCI_SERDES_CTRL_4_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00006000;
}

our $FM_PCI_SERDES_CTRL_4_WIDTH                   =  1; #
our $FM_PCI_SERDES_CTRL_4_BITS                    =  15; #



sub FM_PCI_SERDES_DEBUG_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00439 + $FM_PCIE_BASE); # 0x01439 PCIE_BASE
}

sub FM_PCI_SERDES_DEBUG_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_SERDES_DEBUG_1_WIDTH                  =  1; #
our $FM_PCI_SERDES_DEBUG_1_BITS                   =  32; #



sub FM_PCI_SERDES_DEBUG_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0043A + $FM_PCIE_BASE); # 0x0143A PCIE_BASE
}

sub FM_PCI_SERDES_DEBUG_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_SERDES_DEBUG_2_WIDTH                  =  1; #
our $FM_PCI_SERDES_DEBUG_2_BITS                   =  32; #



sub FM_PCI_SERDES_DEBUG_3 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0043B + $FM_PCIE_BASE); # 0x0143B PCIE_BASE
}

sub FM_PCI_SERDES_DEBUG_3_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PCI_SERDES_DEBUG_3_WIDTH                  =  1; #
our $FM_PCI_SERDES_DEBUG_3_BITS                   =  28; #



sub FM_ESCHED_CFG_1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00000 + $FM_ESCHED_BASE); # 0x02000 ESCHED_BASE
}

sub FM_ESCHED_CFG_1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00ffffff;
}

our $FM_ESCHED_CFG_1_WIDTH                        =  1; #
our $FM_ESCHED_CFG_1_BITS                         =  24; #

sub FM_ESCHED_CFG_1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_ESCHED_CFG_1_STRIDE                       =  1; #


sub FM_ESCHED_CFG_2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00080 + $FM_ESCHED_BASE); # 0x02080 ESCHED_BASE
}

sub FM_ESCHED_CFG_2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00ffffff;
}

our $FM_ESCHED_CFG_2_WIDTH                        =  1; #
our $FM_ESCHED_CFG_2_BITS                         =  24; #

sub FM_ESCHED_CFG_2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_ESCHED_CFG_2_STRIDE                       =  1; #


sub FM_ESCHED_CFG_3 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00100 + $FM_ESCHED_BASE); # 0x02100 ESCHED_BASE
}

sub FM_ESCHED_CFG_3_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_ESCHED_CFG_3_WIDTH                        =  1; #
our $FM_ESCHED_CFG_3_BITS                         =  12; #

sub FM_ESCHED_CFG_3_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_ESCHED_CFG_3_STRIDE                       =  1; #


sub FM_ESCHED_DRR_Q {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00010 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00000 + $FM_MONITOR_BASE); # 0x03000 MONITOR_BASE
}

sub FM_ESCHED_DRR_Q_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_ESCHED_DRR_Q_WIDTH                        =  1; #
our $FM_ESCHED_DRR_Q_BITS                         =  24; #

sub FM_ESCHED_DRR_Q_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
sub FM_ESCHED_DRR_Q_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_ESCHED_DRR_Q_STRIDE_0                     =  1; #
our $FM_ESCHED_DRR_Q_STRIDE_1                     =  16; #


sub FM_ESCHED_DRR_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00800 + $FM_MONITOR_BASE); # 0x03800 MONITOR_BASE
}

sub FM_ESCHED_DRR_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00ffffff;
}

our $FM_ESCHED_DRR_CFG_WIDTH                      =  1; #
our $FM_ESCHED_DRR_CFG_BITS                       =  32; #

sub FM_ESCHED_DRR_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_ESCHED_DRR_CFG_STRIDE                     =  1; #


sub FM_ESCHED_DRR_DC_INIT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00C00 + $FM_MONITOR_BASE); # 0x03C00 MONITOR_BASE
}

sub FM_ESCHED_DRR_DC_INIT_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_ESCHED_DRR_DC_INIT_WIDTH                  =  1; #
our $FM_ESCHED_DRR_DC_INIT_BITS                   =  24; #

sub FM_ESCHED_DRR_DC_INIT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 1024;
}
our $FM_ESCHED_DRR_DC_INIT_STRIDE                 =  1; #


sub FM_MSB_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00000 + $FM_MSB_BASE); # 0x04000 MSB_BASE
}

sub FM_MSB_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x03000003;
}

our $FM_MSB_CFG_WIDTH                             =  1; #
our $FM_MSB_CFG_BITS                              =  28; #



sub FM_MSB_RX_RATE {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00001 + $FM_MSB_BASE); # 0x04001 MSB_BASE
}

sub FM_MSB_RX_RATE_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000605;
}

our $FM_MSB_RX_RATE_WIDTH                         =  1; #
our $FM_MSB_RX_RATE_BITS                          =  12; #



sub FM_MSB_TRUNC {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00002 + $FM_MSB_BASE); # 0x04002 MSB_BASE
}

sub FM_MSB_TRUNC_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_TRUNC_WIDTH                           =  1; #
our $FM_MSB_TRUNC_BITS                            =  32; #



sub FM_MSB_TX_CRC_ERROR {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00003 + $FM_MSB_BASE); # 0x04003 MSB_BASE
}

sub FM_MSB_TX_CRC_ERROR_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_TX_CRC_ERROR_WIDTH                    =  1; #
our $FM_MSB_TX_CRC_ERROR_BITS                     =  32; #



sub FM_MSB_TX_FRAME_ERROR {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00004 + $FM_MSB_BASE); # 0x04004 MSB_BASE
}

sub FM_MSB_TX_FRAME_ERROR_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_TX_FRAME_ERROR_WIDTH                  =  1; #
our $FM_MSB_TX_FRAME_ERROR_BITS                   =  32; #



sub FM_MSB_TX_TIMEOUT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00005 + $FM_MSB_BASE); # 0x04005 MSB_BASE
}

sub FM_MSB_TX_TIMEOUT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_TX_TIMEOUT_WIDTH                      =  1; #
our $FM_MSB_TX_TIMEOUT_BITS                       =  32; #



sub FM_MSB_TX_VALID_FRAMES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00006 + $FM_MSB_BASE); # 0x04006 MSB_BASE
}

sub FM_MSB_TX_VALID_FRAMES_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_TX_VALID_FRAMES_WIDTH                 =  1; #
our $FM_MSB_TX_VALID_FRAMES_BITS                  =  32; #



sub FM_MSB_TX_INVALID_FRAMES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00007 + $FM_MSB_BASE); # 0x04007 MSB_BASE
}

sub FM_MSB_TX_INVALID_FRAMES_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_TX_INVALID_FRAMES_WIDTH               =  1; #
our $FM_MSB_TX_INVALID_FRAMES_BITS                =  32; #



sub FM_MSB_RX_VALID_FRAMES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00008 + $FM_MSB_BASE); # 0x04008 MSB_BASE
}

sub FM_MSB_RX_VALID_FRAMES_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MSB_RX_VALID_FRAMES_WIDTH                 =  1; #
our $FM_MSB_RX_VALID_FRAMES_BITS                  =  32; #



sub FM_FIBM_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00000 + $FM_FIBM_BASE); # 0x05000 FIBM_BASE
}

sub FM_FIBM_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00048883;
}

our $FM_FIBM_CFG_WIDTH                            =  1; #
our $FM_FIBM_CFG_BITS                             =  23; #



sub FM_FIBM_SGLORT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00001 + $FM_FIBM_BASE); # 0x05001 FIBM_BASE
}

sub FM_FIBM_SGLORT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_SGLORT_WIDTH                         =  1; #
our $FM_FIBM_SGLORT_BITS                          =  16; #



sub FM_FIBM_INT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00002 + $FM_FIBM_BASE); # 0x05002 FIBM_BASE
}

sub FM_FIBM_INT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00ff0000;
}

our $FM_FIBM_INT_WIDTH                            =  1; #
our $FM_FIBM_INT_BITS                             =  32; #



sub FM_FIBM_INT_FRAME {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00003 + $FM_FIBM_BASE); # 0x05003 FIBM_BASE
}

sub FM_FIBM_INT_FRAME_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x000fffff;
}

our $FM_FIBM_INT_FRAME_WIDTH                      =  1; #
our $FM_FIBM_INT_FRAME_BITS                       =  28; #



sub FM_FIBM_INT_FRAME_DMAC {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00004 + $FM_FIBM_BASE + ($word)); # 0x05004 FIBM_BASE
}

sub FM_FIBM_INT_FRAME_DMAC_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x0000ffff;}
    if($word == 1) {return 0x00000000;}
}

our $FM_FIBM_INT_FRAME_DMAC_WIDTH                 =  2; #
our $FM_FIBM_INT_FRAME_DMAC_BITS                  =  48; #



sub FM_FIBM_INT_FRAME_SMAC {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00006 + $FM_FIBM_BASE + ($word)); # 0x05006 FIBM_BASE
}

sub FM_FIBM_INT_FRAME_SMAC_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x0000ffff;}
    if($word == 1) {return 0x00000000;}
}

our $FM_FIBM_INT_FRAME_SMAC_WIDTH                 =  2; #
our $FM_FIBM_INT_FRAME_SMAC_BITS                  =  48; #



sub FM_FIBM_REQUEST_CTR {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00008 + $FM_FIBM_BASE); # 0x05008 FIBM_BASE
}

sub FM_FIBM_REQUEST_CTR_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_REQUEST_CTR_WIDTH                    =  1; #
our $FM_FIBM_REQUEST_CTR_BITS                     =  32; #



sub FM_FIBM_DROP_CTR {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00009 + $FM_FIBM_BASE); # 0x05009 FIBM_BASE
}

sub FM_FIBM_DROP_CTR_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_DROP_CTR_WIDTH                       =  1; #
our $FM_FIBM_DROP_CTR_BITS                        =  32; #



sub FM_FIBM_RESPONSE_CTR {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0000A + $FM_FIBM_BASE); # 0x0500A FIBM_BASE
}

sub FM_FIBM_RESPONSE_CTR_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_RESPONSE_CTR_WIDTH                   =  1; #
our $FM_FIBM_RESPONSE_CTR_BITS                    =  32; #



sub FM_FIBM_INTR_CTR_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0000B + $FM_FIBM_BASE); # 0x0500B FIBM_BASE
}

sub FM_FIBM_INTR_CTR_0_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_INTR_CTR_0_WIDTH                     =  1; #
our $FM_FIBM_INTR_CTR_0_BITS                      =  32; #



sub FM_FIBM_INTR_CTR_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0000C + $FM_FIBM_BASE); # 0x0500C FIBM_BASE
}

sub FM_FIBM_INTR_CTR_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_INTR_CTR_1_WIDTH                     =  1; #
our $FM_FIBM_INTR_CTR_1_BITS                      =  32; #



sub FM_FIBM_SCRATCH_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0000D + $FM_FIBM_BASE); # 0x0500D FIBM_BASE
}

sub FM_FIBM_SCRATCH_0_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_SCRATCH_0_WIDTH                      =  1; #
our $FM_FIBM_SCRATCH_0_BITS                       =  32; #



sub FM_FIBM_SCRATCH_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0000E + $FM_FIBM_BASE); # 0x0500E FIBM_BASE
}

sub FM_FIBM_SCRATCH_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_SCRATCH_1_WIDTH                      =  1; #
our $FM_FIBM_SCRATCH_1_BITS                       =  32; #



sub FM_FIBM_SCRATCH_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0000F + $FM_FIBM_BASE); # 0x0500F FIBM_BASE
}

sub FM_FIBM_SCRATCH_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_SCRATCH_2_WIDTH                      =  1; #
our $FM_FIBM_SCRATCH_2_BITS                       =  32; #



sub FM_FIBM_SCRATCH_3 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00010 + $FM_FIBM_BASE); # 0x05010 FIBM_BASE
}

sub FM_FIBM_SCRATCH_3_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_SCRATCH_3_WIDTH                      =  1; #
our $FM_FIBM_SCRATCH_3_BITS                       =  32; #



sub FM_FIBM_SCRATCH_4 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00011 + $FM_FIBM_BASE); # 0x05011 FIBM_BASE
}

sub FM_FIBM_SCRATCH_4_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_SCRATCH_4_WIDTH                      =  1; #
our $FM_FIBM_SCRATCH_4_BITS                       =  32; #



sub FM_FIBM_SCRATCH_5 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00012 + $FM_FIBM_BASE); # 0x05012 FIBM_BASE
}

sub FM_FIBM_SCRATCH_5_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_SCRATCH_5_WIDTH                      =  1; #
our $FM_FIBM_SCRATCH_5_BITS                       =  32; #



sub FM_FIBM_SCRATCH_6 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00013 + $FM_FIBM_BASE); # 0x05013 FIBM_BASE
}

sub FM_FIBM_SCRATCH_6_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_SCRATCH_6_WIDTH                      =  1; #
our $FM_FIBM_SCRATCH_6_BITS                       =  32; #



sub FM_FIBM_SCRATCH_7 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00014 + $FM_FIBM_BASE); # 0x05014 FIBM_BASE
}

sub FM_FIBM_SCRATCH_7_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_SCRATCH_7_WIDTH                      =  1; #
our $FM_FIBM_SCRATCH_7_BITS                       =  32; #



sub FM_FIBM_SCRATCH_8 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00015 + $FM_FIBM_BASE); # 0x05015 FIBM_BASE
}

sub FM_FIBM_SCRATCH_8_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_SCRATCH_8_WIDTH                      =  1; #
our $FM_FIBM_SCRATCH_8_BITS                       =  32; #



sub FM_FIBM_SCRATCH_9 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00016 + $FM_FIBM_BASE); # 0x05016 FIBM_BASE
}

sub FM_FIBM_SCRATCH_9_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_SCRATCH_9_WIDTH                      =  1; #
our $FM_FIBM_SCRATCH_9_BITS                       =  32; #



sub FM_FIBM_SCRATCH_10 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00017 + $FM_FIBM_BASE); # 0x05017 FIBM_BASE
}

sub FM_FIBM_SCRATCH_10_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_SCRATCH_10_WIDTH                     =  1; #
our $FM_FIBM_SCRATCH_10_BITS                      =  32; #



sub FM_FIBM_SCRATCH_11 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00018 + $FM_FIBM_BASE); # 0x05018 FIBM_BASE
}

sub FM_FIBM_SCRATCH_11_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_SCRATCH_11_WIDTH                     =  1; #
our $FM_FIBM_SCRATCH_11_BITS                      =  32; #



sub FM_FIBM_SCRATCH_12 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00019 + $FM_FIBM_BASE); # 0x05019 FIBM_BASE
}

sub FM_FIBM_SCRATCH_12_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_SCRATCH_12_WIDTH                     =  1; #
our $FM_FIBM_SCRATCH_12_BITS                      =  32; #



sub FM_FIBM_SCRATCH_13 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0001A + $FM_FIBM_BASE); # 0x0501A FIBM_BASE
}

sub FM_FIBM_SCRATCH_13_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_SCRATCH_13_WIDTH                     =  1; #
our $FM_FIBM_SCRATCH_13_BITS                      =  32; #



sub FM_FIBM_SCRATCH_14 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0001B + $FM_FIBM_BASE); # 0x0501B FIBM_BASE
}

sub FM_FIBM_SCRATCH_14_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_SCRATCH_14_WIDTH                     =  1; #
our $FM_FIBM_SCRATCH_14_BITS                      =  32; #



sub FM_FIBM_SCRATCH_15 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0001C + $FM_FIBM_BASE); # 0x0501C FIBM_BASE
}

sub FM_FIBM_SCRATCH_15_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FIBM_SCRATCH_15_WIDTH                     =  1; #
our $FM_FIBM_SCRATCH_15_BITS                      =  32; #



sub FM_EACL_CAM1 {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00008 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x00000 + $FM_EACL_BASE + ($word)); # 0x06000 EACL_BASE
}

sub FM_EACL_CAM1_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_EACL_CAM1_WIDTH                           =  3; #
our $FM_EACL_CAM1_BITS                            =  76; #

sub FM_EACL_CAM1_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
sub FM_EACL_CAM1_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_EACL_CAM1_STRIDE_0                        =  4; #
our $FM_EACL_CAM1_STRIDE_1                        =  8; #


sub FM_EACL_CAM2 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00100 + $FM_EACL_BASE + ($word)); # 0x06100 EACL_BASE
}

sub FM_EACL_CAM2_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_EACL_CAM2_WIDTH                           =  2; #
our $FM_EACL_CAM2_BITS                            =  64; #

sub FM_EACL_CAM2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_EACL_CAM2_STRIDE                          =  2; #


sub FM_EACL_ACTION_RAM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00020 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00400 + $FM_EACL_BASE); # 0x06400 EACL_BASE
}

sub FM_EACL_ACTION_RAM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_EACL_ACTION_RAM_WIDTH                     =  1; #
our $FM_EACL_ACTION_RAM_BITS                      =  6; #

sub FM_EACL_ACTION_RAM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
sub FM_EACL_ACTION_RAM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_EACL_ACTION_RAM_STRIDE_0                  =  1; #
our $FM_EACL_ACTION_RAM_STRIDE_1                  =  32; #


sub FM_EACL_PROFILE_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00800 + $FM_EACL_BASE); # 0x06800 EACL_BASE
}

sub FM_EACL_PROFILE_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_EACL_PROFILE_TABLE_WIDTH                  =  1; #
our $FM_EACL_PROFILE_TABLE_BITS                   =  8; #

sub FM_EACL_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_EACL_PROFILE_TABLE_STRIDE                 =  1; #


sub FM_LAG_PORT_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00000 + $FM_LAG_BASE); # 0x07000 LAG_BASE
}

sub FM_LAG_PORT_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_LAG_PORT_TABLE_WIDTH                      =  1; #
our $FM_LAG_PORT_TABLE_BITS                       =  14; #

sub FM_LAG_PORT_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_LAG_PORT_TABLE_STRIDE                     =  1; #


sub FM_LAG_PROFILE_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00080 + $FM_LAG_BASE); # 0x07080 LAG_BASE
}

sub FM_LAG_PROFILE_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_LAG_PROFILE_TABLE_WIDTH                   =  1; #
our $FM_LAG_PROFILE_TABLE_BITS                    =  8; #

sub FM_LAG_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_LAG_PROFILE_TABLE_STRIDE                  =  1; #


sub FM_LAG_FILTERING_CAM {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00200 + $FM_LAG_BASE + ($word)); # 0x07200 LAG_BASE
}

sub FM_LAG_FILTERING_CAM_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_LAG_FILTERING_CAM_WIDTH                   =  4; #
our $FM_LAG_FILTERING_CAM_BITS                    =  128; #

sub FM_LAG_FILTERING_CAM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_LAG_FILTERING_CAM_STRIDE                  =  4; #


sub FM_SSCHED_TX_NEXT_PORT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00000 + $FM_SSCHED_BASE); # 0x08000 SSCHED_BASE
}

sub FM_SSCHED_TX_NEXT_PORT_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SSCHED_TX_NEXT_PORT_WIDTH                 =  1; #
our $FM_SSCHED_TX_NEXT_PORT_BITS                  =  32; #

sub FM_SSCHED_TX_NEXT_PORT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_SSCHED_TX_NEXT_PORT_STRIDE                =  1; #


sub FM_SSCHED_TX_INIT_TOKEN {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00020 + $FM_SSCHED_BASE); # 0x08020 SSCHED_BASE
}

sub FM_SSCHED_TX_INIT_TOKEN_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SSCHED_TX_INIT_TOKEN_WIDTH                =  1; #
our $FM_SSCHED_TX_INIT_TOKEN_BITS                 =  11; #



sub FM_SSCHED_TX_INIT_COMPLETE {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00021 + $FM_SSCHED_BASE); # 0x08021 SSCHED_BASE
}

sub FM_SSCHED_TX_INIT_COMPLETE_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SSCHED_TX_INIT_COMPLETE_WIDTH             =  1; #
our $FM_SSCHED_TX_INIT_COMPLETE_BITS              =  1; #



sub FM_SSCHED_TX_REPLACE_TOKEN {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00022 + $FM_SSCHED_BASE); # 0x08022 SSCHED_BASE
}

sub FM_SSCHED_TX_REPLACE_TOKEN_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SSCHED_TX_REPLACE_TOKEN_WIDTH             =  1; #
our $FM_SSCHED_TX_REPLACE_TOKEN_BITS              =  32; #



sub FM_SSCHED_RX_NEXT_PORT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00040 + $FM_SSCHED_BASE); # 0x08040 SSCHED_BASE
}

sub FM_SSCHED_RX_NEXT_PORT_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SSCHED_RX_NEXT_PORT_WIDTH                 =  1; #
our $FM_SSCHED_RX_NEXT_PORT_BITS                  =  32; #

sub FM_SSCHED_RX_NEXT_PORT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_SSCHED_RX_NEXT_PORT_STRIDE                =  1; #


sub FM_SSCHED_RX_INIT_TOKEN {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00060 + $FM_SSCHED_BASE); # 0x08060 SSCHED_BASE
}

sub FM_SSCHED_RX_INIT_TOKEN_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SSCHED_RX_INIT_TOKEN_WIDTH                =  1; #
our $FM_SSCHED_RX_INIT_TOKEN_BITS                 =  10; #



sub FM_SSCHED_RX_INIT_COMPLETE {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00061 + $FM_SSCHED_BASE); # 0x08061 SSCHED_BASE
}

sub FM_SSCHED_RX_INIT_COMPLETE_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SSCHED_RX_INIT_COMPLETE_WIDTH             =  1; #
our $FM_SSCHED_RX_INIT_COMPLETE_BITS              =  1; #



sub FM_SSCHED_RX_REPLACE_TOKEN {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00062 + $FM_SSCHED_BASE); # 0x08062 SSCHED_BASE
}

sub FM_SSCHED_RX_REPLACE_TOKEN_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SSCHED_RX_REPLACE_TOKEN_WIDTH             =  1; #
our $FM_SSCHED_RX_REPLACE_TOKEN_BITS              =  22; #



sub FM_SSCHED_RX_SLOW_PORT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00070 + $FM_SSCHED_BASE); # 0x08070 SSCHED_BASE
}

sub FM_SSCHED_RX_SLOW_PORT_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SSCHED_RX_SLOW_PORT_WIDTH                 =  1; #
our $FM_SSCHED_RX_SLOW_PORT_BITS                  =  16; #

sub FM_SSCHED_RX_SLOW_PORT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_SSCHED_RX_SLOW_PORT_STRIDE                =  1; #


sub FM_SSCHED_RXQ_FREELIST_INIT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x000F0 + $FM_SSCHED_BASE); # 0x080F0 SSCHED_BASE
}

sub FM_SSCHED_RXQ_FREELIST_INIT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SSCHED_RXQ_FREELIST_INIT_WIDTH            =  1; #
our $FM_SSCHED_RXQ_FREELIST_INIT_BITS             =  16; #



sub FM_SSCHED_RXQ_FREELIST_INIT_DONE {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x000F1 + $FM_SSCHED_BASE); # 0x080F1 SSCHED_BASE
}

sub FM_SSCHED_RXQ_FREELIST_INIT_DONE_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SSCHED_RXQ_FREELIST_INIT_DONE_WIDTH       =  1; #
our $FM_SSCHED_RXQ_FREELIST_INIT_DONE_BITS        =  16; #



sub FM_SSCHED_TXQ_FREELIST_INIT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x000F4 + $FM_SSCHED_BASE); # 0x080F4 SSCHED_BASE
}

sub FM_SSCHED_TXQ_FREELIST_INIT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SSCHED_TXQ_FREELIST_INIT_WIDTH            =  1; #
our $FM_SSCHED_TXQ_FREELIST_INIT_BITS             =  16; #



sub FM_SSCHED_TXQ_FREELIST_INIT_DONE {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x000F5 + $FM_SSCHED_BASE); # 0x080F5 SSCHED_BASE
}

sub FM_SSCHED_TXQ_FREELIST_INIT_DONE_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SSCHED_TXQ_FREELIST_INIT_DONE_WIDTH       =  1; #
our $FM_SSCHED_TXQ_FREELIST_INIT_DONE_BITS        =  16; #



sub FM_SSCHED_HS_FREELIST_INIT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x000F8 + $FM_SSCHED_BASE); # 0x080F8 SSCHED_BASE
}

sub FM_SSCHED_HS_FREELIST_INIT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SSCHED_HS_FREELIST_INIT_WIDTH             =  1; #
our $FM_SSCHED_HS_FREELIST_INIT_BITS              =  16; #



sub FM_SSCHED_HS_FREELIST_INIT_DONE {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x000F9 + $FM_SSCHED_BASE); # 0x080F9 SSCHED_BASE
}

sub FM_SSCHED_HS_FREELIST_INIT_DONE_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SSCHED_HS_FREELIST_INIT_DONE_WIDTH        =  1; #
our $FM_SSCHED_HS_FREELIST_INIT_DONE_BITS         =  16; #



sub FM_SSCHED_FREELIST_INIT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x000FC + $FM_SSCHED_BASE); # 0x080FC SSCHED_BASE
}

sub FM_SSCHED_FREELIST_INIT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SSCHED_FREELIST_INIT_WIDTH                =  1; #
our $FM_SSCHED_FREELIST_INIT_BITS                 =  16; #



sub FM_SSCHED_FREELIST_INIT_DONE {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x000FD + $FM_SSCHED_BASE); # 0x080FD SSCHED_BASE
}

sub FM_SSCHED_FREELIST_INIT_DONE_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SSCHED_FREELIST_INIT_DONE_WIDTH           =  1; #
our $FM_SSCHED_FREELIST_INIT_DONE_BITS            =  16; #



sub FM_HASH_LAYER2_KEY_PROFILE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00008 * (($index) - 0) + 0x00000 + $FM_HASH_BASE + ($word)); # 0x0B000 HASH_BASE
}

sub FM_HASH_LAYER2_KEY_PROFILE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
    if($word == 4) {return 0x00000000;}
}

our $FM_HASH_LAYER2_KEY_PROFILE_WIDTH             =  5; #
our $FM_HASH_LAYER2_KEY_PROFILE_BITS              =  150; #

sub FM_HASH_LAYER2_KEY_PROFILE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_HASH_LAYER2_KEY_PROFILE_STRIDE            =  8; #


sub FM_HASH_LAYER2_FUNC_PROFILE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00080 + $FM_HASH_BASE); # 0x0B080 HASH_BASE
}

sub FM_HASH_LAYER2_FUNC_PROFILE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000001;
}

our $FM_HASH_LAYER2_FUNC_PROFILE_WIDTH            =  1; #
our $FM_HASH_LAYER2_FUNC_PROFILE_BITS             =  28; #

sub FM_HASH_LAYER2_FUNC_PROFILE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_HASH_LAYER2_FUNC_PROFILE_STRIDE           =  1; #


sub FM_HASH_LAYER2_ROTA_PTABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00400 + $FM_HASH_BASE); # 0x0B400 HASH_BASE
}

sub FM_HASH_LAYER2_ROTA_PTABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_HASH_LAYER2_ROTA_PTABLE_WIDTH             =  1; #
our $FM_HASH_LAYER2_ROTA_PTABLE_BITS              =  24; #

sub FM_HASH_LAYER2_ROTA_PTABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 1024;
}
our $FM_HASH_LAYER2_ROTA_PTABLE_STRIDE            =  1; #


sub FM_HASH_LAYER2_ROTB_PTABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00800 + $FM_HASH_BASE); # 0x0B800 HASH_BASE
}

sub FM_HASH_LAYER2_ROTB_PTABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_HASH_LAYER2_ROTB_PTABLE_WIDTH             =  1; #
our $FM_HASH_LAYER2_ROTB_PTABLE_BITS              =  24; #

sub FM_HASH_LAYER2_ROTB_PTABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 1024;
}
our $FM_HASH_LAYER2_ROTB_PTABLE_STRIDE            =  1; #


sub FM_ALU_CMD_TABLE {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00040 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x00000 + $FM_ALU_BASE + ($word)); # 0x0C000 ALU_BASE
}

sub FM_ALU_CMD_TABLE_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000800;}
}

our $FM_ALU_CMD_TABLE_WIDTH                       =  2; #
our $FM_ALU_CMD_TABLE_BITS                        =  46; #

sub FM_ALU_CMD_TABLE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
sub FM_ALU_CMD_TABLE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 6;
}
our $FM_ALU_CMD_TABLE_STRIDE_0                    =  2; #
our $FM_ALU_CMD_TABLE_STRIDE_1                    =  64; #


sub FM_ALU_Y_TABLE {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00010 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00200 + $FM_ALU_BASE); # 0x0C200 ALU_BASE
}

sub FM_ALU_Y_TABLE_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_ALU_Y_TABLE_WIDTH                         =  1; #
our $FM_ALU_Y_TABLE_BITS                          =  16; #

sub FM_ALU_Y_TABLE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
sub FM_ALU_Y_TABLE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 6;
}
our $FM_ALU_Y_TABLE_STRIDE_0                      =  1; #
our $FM_ALU_Y_TABLE_STRIDE_1                      =  16; #


sub FM_L2L_SWEEPER_TIMER_CFG {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00000 + $FM_L2L_SWEEPER_BASE + ($word)); # 0x0D000 L2L_SWEEPER_BASE
}

sub FM_L2L_SWEEPER_TIMER_CFG_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L2L_SWEEPER_TIMER_CFG_WIDTH               =  2; #
our $FM_L2L_SWEEPER_TIMER_CFG_BITS                =  34; #

sub FM_L2L_SWEEPER_TIMER_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
our $FM_L2L_SWEEPER_TIMER_CFG_STRIDE              =  2; #


sub FM_L2L_SWEEPER_TIMER_STATUS {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00004 + $FM_L2L_SWEEPER_BASE + ($word)); # 0x0D004 L2L_SWEEPER_BASE
}

sub FM_L2L_SWEEPER_TIMER_STATUS_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L2L_SWEEPER_TIMER_STATUS_WIDTH            =  2; #
our $FM_L2L_SWEEPER_TIMER_STATUS_BITS             =  33; #

sub FM_L2L_SWEEPER_TIMER_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
our $FM_L2L_SWEEPER_TIMER_STATUS_STRIDE           =  2; #


sub FM_L2L_SWEEPER_CAM {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00008 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x00080 + $FM_L2L_SWEEPER_BASE + ($word)); # 0x0D080 L2L_SWEEPER_BASE
}

sub FM_L2L_SWEEPER_CAM_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_L2L_SWEEPER_CAM_WIDTH                     =  4; #
our $FM_L2L_SWEEPER_CAM_BITS                      =  128; #

sub FM_L2L_SWEEPER_CAM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
sub FM_L2L_SWEEPER_CAM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_L2L_SWEEPER_CAM_STRIDE_0                  =  4; #
our $FM_L2L_SWEEPER_CAM_STRIDE_1                  =  8; #


sub FM_L2L_SWEEPER_RAM {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00100 + $FM_L2L_SWEEPER_BASE + ($word)); # 0x0D100 L2L_SWEEPER_BASE
}

sub FM_L2L_SWEEPER_RAM_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_L2L_SWEEPER_RAM_WIDTH                     =  3; #
our $FM_L2L_SWEEPER_RAM_BITS                      =  85; #

sub FM_L2L_SWEEPER_RAM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_L2L_SWEEPER_RAM_STRIDE                    =  4; #


sub FM_L2L_SWEEPER_FIFO {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00200 + $FM_L2L_SWEEPER_BASE); # 0x0D200 L2L_SWEEPER_BASE
}

sub FM_L2L_SWEEPER_FIFO_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2L_SWEEPER_FIFO_WIDTH                    =  1; #
our $FM_L2L_SWEEPER_FIFO_BITS                     =  32; #

sub FM_L2L_SWEEPER_FIFO_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 512;
}
our $FM_L2L_SWEEPER_FIFO_STRIDE                   =  1; #


sub FM_L2L_SWEEPER_FIFO_HEAD {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00400 + $FM_L2L_SWEEPER_BASE); # 0x0D400 L2L_SWEEPER_BASE
}

sub FM_L2L_SWEEPER_FIFO_HEAD_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_L2L_SWEEPER_FIFO_HEAD_WIDTH               =  1; #
our $FM_L2L_SWEEPER_FIFO_HEAD_BITS                =  9; #



sub FM_L2L_SWEEPER_FIFO_TAIL {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00401 + $FM_L2L_SWEEPER_BASE); # 0x0D401 L2L_SWEEPER_BASE
}

sub FM_L2L_SWEEPER_FIFO_TAIL_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_L2L_SWEEPER_FIFO_TAIL_WIDTH               =  1; #
our $FM_L2L_SWEEPER_FIFO_TAIL_BITS                =  9; #



sub FM_L2L_SWEEPER_IP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00402 + $FM_L2L_SWEEPER_BASE); # 0x0D402 L2L_SWEEPER_BASE
}

sub FM_L2L_SWEEPER_IP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_L2L_SWEEPER_IP_WIDTH                      =  1; #
our $FM_L2L_SWEEPER_IP_BITS                       =  3; #



sub FM_L2L_SWEEPER_IM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00403 + $FM_L2L_SWEEPER_BASE); # 0x0D403 L2L_SWEEPER_BASE
}

sub FM_L2L_SWEEPER_IM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_L2L_SWEEPER_IM_WIDTH                      =  1; #
our $FM_L2L_SWEEPER_IM_BITS                       =  3; #



sub FM_L2L_SWEEPER_WRITE_COMMAND {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00404 + $FM_L2L_SWEEPER_BASE); # 0x0D404 L2L_SWEEPER_BASE
}

sub FM_L2L_SWEEPER_WRITE_COMMAND_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_L2L_SWEEPER_WRITE_COMMAND_WIDTH           =  1; #
our $FM_L2L_SWEEPER_WRITE_COMMAND_BITS            =  17; #



sub FM_L2L_SWEEPER_WRITE_DATA {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00408 + $FM_L2L_SWEEPER_BASE + ($word)); # 0x0D408 L2L_SWEEPER_BASE
}

sub FM_L2L_SWEEPER_WRITE_DATA_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_L2L_SWEEPER_WRITE_DATA_WIDTH              =  4; #
our $FM_L2L_SWEEPER_WRITE_DATA_BITS               =  116; #



sub FM_GLORT_CAM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00000 + $FM_GLORT_BASE); # 0x0E000 GLORT_BASE
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

    return 1024;
}
our $FM_GLORT_CAM_STRIDE                          =  1; #


sub FM_GLORT_RAM {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00800 + $FM_GLORT_BASE + ($word)); # 0x0E800 GLORT_BASE
}

sub FM_GLORT_RAM_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_GLORT_RAM_WIDTH                           =  2; #
our $FM_GLORT_RAM_BITS                            =  43; #

sub FM_GLORT_RAM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 1024;
}
our $FM_GLORT_RAM_STRIDE                          =  2; #


sub FM_SBUS_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00000 + $FM_JSS_BASE); # 0x0F000 JSS_BASE
}

sub FM_SBUS_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000001;
}

our $FM_SBUS_CFG_WIDTH                            =  1; #
our $FM_SBUS_CFG_BITS                             =  1; #



sub FM_SBUS_COMMAND {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00001 + $FM_JSS_BASE); # 0x0F001 JSS_BASE
}

sub FM_SBUS_COMMAND_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SBUS_COMMAND_WIDTH                        =  1; #
our $FM_SBUS_COMMAND_BITS                         =  29; #



sub FM_SBUS_REQUEST {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00002 + $FM_JSS_BASE); # 0x0F002 JSS_BASE
}

sub FM_SBUS_REQUEST_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SBUS_REQUEST_WIDTH                        =  1; #
our $FM_SBUS_REQUEST_BITS                         =  32; #



sub FM_SBUS_RESPONSE {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00003 + $FM_JSS_BASE); # 0x0F003 JSS_BASE
}

sub FM_SBUS_RESPONSE_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SBUS_RESPONSE_WIDTH                       =  1; #
our $FM_SBUS_RESPONSE_BITS                        =  32; #



sub FM_SBUS_SPICO {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00004 + $FM_JSS_BASE); # 0x0F004 JSS_BASE
}

sub FM_SBUS_SPICO_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000001;
}

our $FM_SBUS_SPICO_WIDTH                          =  1; #
our $FM_SBUS_SPICO_BITS                           =  23; #



sub FM_SBUS_IP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00005 + $FM_JSS_BASE); # 0x0F005 JSS_BASE
}

sub FM_SBUS_IP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SBUS_IP_WIDTH                             =  1; #
our $FM_SBUS_IP_BITS                              =  20; #



sub FM_SBUS_IM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00006 + $FM_JSS_BASE); # 0x0F006 JSS_BASE
}

sub FM_SBUS_IM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SBUS_IM_WIDTH                             =  1; #
our $FM_SBUS_IM_BITS                              =  20; #



sub FM_SPICO_BIST {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00007 + $FM_JSS_BASE); # 0x0F007 JSS_BASE
}

sub FM_SPICO_BIST_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SPICO_BIST_WIDTH                          =  1; #
our $FM_SPICO_BIST_BITS                           =  6; #



sub FM_TAP_FSM_STATE {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00008 + $FM_JSS_BASE); # 0x0F008 JSS_BASE
}

sub FM_TAP_FSM_STATE_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000001;
}

our $FM_TAP_FSM_STATE_WIDTH                       =  1; #
our $FM_TAP_FSM_STATE_BITS                        =  9; #



sub FM_TAP_EFUSE_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00009 + $FM_JSS_BASE); # 0x0F009 JSS_BASE
}

sub FM_TAP_EFUSE_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_TAP_EFUSE_CFG_WIDTH                       =  1; #
our $FM_TAP_EFUSE_CFG_BITS                        =  2; #



sub FM_TAP_EFUSE {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x0000C + $FM_JSS_BASE + ($word)); # 0x0F00C JSS_BASE
}

sub FM_TAP_EFUSE_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_TAP_EFUSE_WIDTH                           =  4; #
our $FM_TAP_EFUSE_BITS                            =  128; #



sub FM_SSCHED_TICK_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00010 + $FM_JSS_BASE); # 0x0F010 JSS_BASE
}

sub FM_SSCHED_TICK_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SSCHED_TICK_CFG_WIDTH                     =  1; #
our $FM_SSCHED_TICK_CFG_BITS                      =  8; #



sub FM_L3AR_CAM {
    my ($self, $index2, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 5, -1);

    return (0x00200 * (($index2) - 0) + 0x00010 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x00000 + $FM_L3AR_BASE + ($word)); # 0x10000 L3AR_BASE
}

sub FM_L3AR_CAM_DEFAULT {
    my ($self, $index2, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 5, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_L3AR_CAM_WIDTH                            =  4; #
our $FM_L3AR_CAM_BITS                             =  128; #

sub FM_L3AR_CAM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_L3AR_CAM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
sub FM_L3AR_CAM_ENTRIES_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_L3AR_CAM_STRIDE_0                         =  4; #
our $FM_L3AR_CAM_STRIDE_1                         =  16; #
our $FM_L3AR_CAM_STRIDE_2                         =  512; #


sub FM_L3AR_SLICE_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01000 + $FM_L3AR_BASE); # 0x11000 L3AR_BASE
}

sub FM_L3AR_SLICE_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_L3AR_SLICE_CFG_WIDTH                      =  1; #
our $FM_L3AR_SLICE_CFG_BITS                       =  10; #



sub FM_L3AR_KEY_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01001 + $FM_L3AR_BASE); # 0x11001 L3AR_BASE
}

sub FM_L3AR_KEY_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0000000f;
}

our $FM_L3AR_KEY_CFG_WIDTH                        =  1; #
our $FM_L3AR_KEY_CFG_BITS                         =  4; #



sub FM_L3AR_ACTION_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x01008 + $FM_L3AR_BASE); # 0x11008 L3AR_BASE
}

sub FM_L3AR_ACTION_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L3AR_ACTION_CFG_WIDTH                     =  1; #
our $FM_L3AR_ACTION_CFG_BITS                      =  5; #

sub FM_L3AR_ACTION_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_L3AR_ACTION_CFG_STRIDE                    =  1; #


sub FM_L3AR_RAM1 {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00040 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x01200 + $FM_L3AR_BASE + ($word)); # 0x11200 L3AR_BASE
}

sub FM_L3AR_RAM1_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L3AR_RAM1_WIDTH                           =  2; #
our $FM_L3AR_RAM1_BITS                            =  58; #

sub FM_L3AR_RAM1_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
sub FM_L3AR_RAM1_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_L3AR_RAM1_STRIDE_0                        =  2; #
our $FM_L3AR_RAM1_STRIDE_1                        =  64; #


sub FM_L3AR_RAM2 {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00040 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x01400 + $FM_L3AR_BASE + ($word)); # 0x11400 L3AR_BASE
}

sub FM_L3AR_RAM2_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L3AR_RAM2_WIDTH                           =  2; #
our $FM_L3AR_RAM2_BITS                            =  58; #

sub FM_L3AR_RAM2_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
sub FM_L3AR_RAM2_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_L3AR_RAM2_STRIDE_0                        =  2; #
our $FM_L3AR_RAM2_STRIDE_1                        =  64; #


sub FM_L3AR_RAM3 {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00020 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x01600 + $FM_L3AR_BASE); # 0x11600 L3AR_BASE
}

sub FM_L3AR_RAM3_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_L3AR_RAM3_WIDTH                           =  1; #
our $FM_L3AR_RAM3_BITS                            =  32; #

sub FM_L3AR_RAM3_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
sub FM_L3AR_RAM3_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_L3AR_RAM3_STRIDE_0                        =  1; #
our $FM_L3AR_RAM3_STRIDE_1                        =  32; #


sub FM_L3AR_RAM4 {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00040 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x01800 + $FM_L3AR_BASE + ($word)); # 0x11800 L3AR_BASE
}

sub FM_L3AR_RAM4_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L3AR_RAM4_WIDTH                           =  2; #
our $FM_L3AR_RAM4_BITS                            =  55; #

sub FM_L3AR_RAM4_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
sub FM_L3AR_RAM4_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_L3AR_RAM4_STRIDE_0                        =  2; #
our $FM_L3AR_RAM4_STRIDE_1                        =  64; #


sub FM_L3AR_RAM5 {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00040 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x01A00 + $FM_L3AR_BASE + ($word)); # 0x11A00 L3AR_BASE
}

sub FM_L3AR_RAM5_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L3AR_RAM5_WIDTH                           =  2; #
our $FM_L3AR_RAM5_BITS                            =  59; #

sub FM_L3AR_RAM5_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
sub FM_L3AR_RAM5_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_L3AR_RAM5_STRIDE_0                        =  2; #
our $FM_L3AR_RAM5_STRIDE_1                        =  64; #


sub FM_L3AR_DGLORT_PROFILE_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x01C00 + $FM_L3AR_BASE + ($word)); # 0x11C00 L3AR_BASE
}

sub FM_L3AR_DGLORT_PROFILE_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L3AR_DGLORT_PROFILE_TABLE_WIDTH           =  2; #
our $FM_L3AR_DGLORT_PROFILE_TABLE_BITS            =  35; #

sub FM_L3AR_DGLORT_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L3AR_DGLORT_PROFILE_TABLE_STRIDE          =  2; #


sub FM_L3AR_SGLORT_PROFILE_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x01C40 + $FM_L3AR_BASE + ($word)); # 0x11C40 L3AR_BASE
}

sub FM_L3AR_SGLORT_PROFILE_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L3AR_SGLORT_PROFILE_TABLE_WIDTH           =  2; #
our $FM_L3AR_SGLORT_PROFILE_TABLE_BITS            =  35; #

sub FM_L3AR_SGLORT_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L3AR_SGLORT_PROFILE_TABLE_STRIDE          =  2; #


sub FM_L3AR_W8ABCD_PROFILE_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x01C80 + $FM_L3AR_BASE + ($word)); # 0x11C80 L3AR_BASE
}

sub FM_L3AR_W8ABCD_PROFILE_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_L3AR_W8ABCD_PROFILE_TABLE_WIDTH           =  3; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_BITS            =  72; #

sub FM_L3AR_W8ABCD_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L3AR_W8ABCD_PROFILE_TABLE_STRIDE          =  4; #


sub FM_L3AR_W8E_PROFILE_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x01D00 + $FM_L3AR_BASE); # 0x11D00 L3AR_BASE
}

sub FM_L3AR_W8E_PROFILE_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L3AR_W8E_PROFILE_TABLE_WIDTH              =  1; #
our $FM_L3AR_W8E_PROFILE_TABLE_BITS               =  19; #

sub FM_L3AR_W8E_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L3AR_W8E_PROFILE_TABLE_STRIDE             =  1; #


sub FM_L3AR_W8F_PROFILE_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x01D20 + $FM_L3AR_BASE); # 0x11D20 L3AR_BASE
}

sub FM_L3AR_W8F_PROFILE_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L3AR_W8F_PROFILE_TABLE_WIDTH              =  1; #
our $FM_L3AR_W8F_PROFILE_TABLE_BITS               =  18; #

sub FM_L3AR_W8F_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L3AR_W8F_PROFILE_TABLE_STRIDE             =  1; #


sub FM_L3AR_MA1_MAC_PROFILE_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x01D40 + $FM_L3AR_BASE + ($word)); # 0x11D40 L3AR_BASE
}

sub FM_L3AR_MA1_MAC_PROFILE_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L3AR_MA1_MAC_PROFILE_TABLE_WIDTH          =  2; #
our $FM_L3AR_MA1_MAC_PROFILE_TABLE_BITS           =  51; #

sub FM_L3AR_MA1_MAC_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L3AR_MA1_MAC_PROFILE_TABLE_STRIDE         =  2; #


sub FM_L3AR_MA2_MAC_PROFILE_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x01D80 + $FM_L3AR_BASE + ($word)); # 0x11D80 L3AR_BASE
}

sub FM_L3AR_MA2_MAC_PROFILE_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L3AR_MA2_MAC_PROFILE_TABLE_WIDTH          =  2; #
our $FM_L3AR_MA2_MAC_PROFILE_TABLE_BITS           =  50; #

sub FM_L3AR_MA2_MAC_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L3AR_MA2_MAC_PROFILE_TABLE_STRIDE         =  2; #


sub FM_L3AR_VID_PROFILE_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x01DC0 + $FM_L3AR_BASE + ($word)); # 0x11DC0 L3AR_BASE
}

sub FM_L3AR_VID_PROFILE_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L3AR_VID_PROFILE_TABLE_WIDTH              =  2; #
our $FM_L3AR_VID_PROFILE_TABLE_BITS               =  60; #

sub FM_L3AR_VID_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L3AR_VID_PROFILE_TABLE_STRIDE             =  2; #


sub FM_L3AR_MA_FID_PROFILE_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x01E00 + $FM_L3AR_BASE + ($word)); # 0x11E00 L3AR_BASE
}

sub FM_L3AR_MA_FID_PROFILE_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L3AR_MA_FID_PROFILE_TABLE_WIDTH           =  2; #
our $FM_L3AR_MA_FID_PROFILE_TABLE_BITS            =  56; #

sub FM_L3AR_MA_FID_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L3AR_MA_FID_PROFILE_TABLE_STRIDE          =  2; #


sub FM_L3AR_CSGLORT_PROFILE_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x01E40 + $FM_L3AR_BASE + ($word)); # 0x11E40 L3AR_BASE
}

sub FM_L3AR_CSGLORT_PROFILE_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L3AR_CSGLORT_PROFILE_TABLE_WIDTH          =  2; #
our $FM_L3AR_CSGLORT_PROFILE_TABLE_BITS           =  34; #

sub FM_L3AR_CSGLORT_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L3AR_CSGLORT_PROFILE_TABLE_STRIDE         =  2; #


sub FM_L3AR_W16ABC_PROFILE_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x01E80 + $FM_L3AR_BASE + ($word)); # 0x11E80 L3AR_BASE
}

sub FM_L3AR_W16ABC_PROFILE_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L3AR_W16ABC_PROFILE_TABLE_WIDTH           =  2; #
our $FM_L3AR_W16ABC_PROFILE_TABLE_BITS            =  57; #

sub FM_L3AR_W16ABC_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L3AR_W16ABC_PROFILE_TABLE_STRIDE          =  2; #


sub FM_L3AR_W16DEF_PROFILE_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x01EC0 + $FM_L3AR_BASE + ($word)); # 0x11EC0 L3AR_BASE
}

sub FM_L3AR_W16DEF_PROFILE_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L3AR_W16DEF_PROFILE_TABLE_WIDTH           =  2; #
our $FM_L3AR_W16DEF_PROFILE_TABLE_BITS            =  57; #

sub FM_L3AR_W16DEF_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L3AR_W16DEF_PROFILE_TABLE_STRIDE          =  2; #


sub FM_L3AR_W16GH_PROFILE_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x01F00 + $FM_L3AR_BASE + ($word)); # 0x11F00 L3AR_BASE
}

sub FM_L3AR_W16GH_PROFILE_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L3AR_W16GH_PROFILE_TABLE_WIDTH            =  2; #
our $FM_L3AR_W16GH_PROFILE_TABLE_BITS             =  38; #

sub FM_L3AR_W16GH_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L3AR_W16GH_PROFILE_TABLE_STRIDE           =  2; #


sub FM_L3AR_HASH_ROT_PROFILE_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x01F40 + $FM_L3AR_BASE); # 0x11F40 L3AR_BASE
}

sub FM_L3AR_HASH_ROT_PROFILE_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L3AR_HASH_ROT_PROFILE_TABLE_WIDTH         =  1; #
our $FM_L3AR_HASH_ROT_PROFILE_TABLE_BITS          =  24; #

sub FM_L3AR_HASH_ROT_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_L3AR_HASH_ROT_PROFILE_TABLE_STRIDE        =  1; #


sub FM_L3AR_ALU13_OP_PROFILE_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x01F80 + $FM_L3AR_BASE + ($word)); # 0x11F80 L3AR_BASE
}

sub FM_L3AR_ALU13_OP_PROFILE_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_L3AR_ALU13_OP_PROFILE_TABLE_WIDTH         =  4; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_BITS          =  120; #

sub FM_L3AR_ALU13_OP_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_STRIDE        =  4; #


sub FM_L3AR_ALU46_OP_PROFILE_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x02000 + $FM_L3AR_BASE + ($word)); # 0x12000 L3AR_BASE
}

sub FM_L3AR_ALU46_OP_PROFILE_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_L3AR_ALU46_OP_PROFILE_TABLE_WIDTH         =  4; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_BITS          =  120; #

sub FM_L3AR_ALU46_OP_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_STRIDE        =  4; #


sub FM_L3AR_POL1_IDX_PROFILE_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x02080 + $FM_L3AR_BASE); # 0x12080 L3AR_BASE
}

sub FM_L3AR_POL1_IDX_PROFILE_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L3AR_POL1_IDX_PROFILE_TABLE_WIDTH         =  1; #
our $FM_L3AR_POL1_IDX_PROFILE_TABLE_BITS          =  16; #

sub FM_L3AR_POL1_IDX_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_L3AR_POL1_IDX_PROFILE_TABLE_STRIDE        =  1; #


sub FM_L3AR_POL2_IDX_PROFILE_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x02090 + $FM_L3AR_BASE); # 0x12090 L3AR_BASE
}

sub FM_L3AR_POL2_IDX_PROFILE_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L3AR_POL2_IDX_PROFILE_TABLE_WIDTH         =  1; #
our $FM_L3AR_POL2_IDX_PROFILE_TABLE_BITS          =  16; #

sub FM_L3AR_POL2_IDX_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_L3AR_POL2_IDX_PROFILE_TABLE_STRIDE        =  1; #


sub FM_L3AR_POL3_IDX_PROFILE_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x020A0 + $FM_L3AR_BASE); # 0x120A0 L3AR_BASE
}

sub FM_L3AR_POL3_IDX_PROFILE_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L3AR_POL3_IDX_PROFILE_TABLE_WIDTH         =  1; #
our $FM_L3AR_POL3_IDX_PROFILE_TABLE_BITS          =  23; #

sub FM_L3AR_POL3_IDX_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_L3AR_POL3_IDX_PROFILE_TABLE_STRIDE        =  1; #


sub FM_L3AR_QOS_PROFILE_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x020C0 + $FM_L3AR_BASE + ($word)); # 0x120C0 L3AR_BASE
}

sub FM_L3AR_QOS_PROFILE_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L3AR_QOS_PROFILE_TABLE_WIDTH              =  2; #
our $FM_L3AR_QOS_PROFILE_TABLE_BITS               =  61; #

sub FM_L3AR_QOS_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L3AR_QOS_PROFILE_TABLE_STRIDE             =  2; #


sub FM_L3AR_TRAP_HEADER_RULE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x02100 + $FM_L3AR_BASE); # 0x12100 L3AR_BASE
}

sub FM_L3AR_TRAP_HEADER_RULE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L3AR_TRAP_HEADER_RULE_WIDTH               =  1; #
our $FM_L3AR_TRAP_HEADER_RULE_BITS                =  10; #

sub FM_L3AR_TRAP_HEADER_RULE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
our $FM_L3AR_TRAP_HEADER_RULE_STRIDE              =  1; #


sub FM_L3AR_TRAP_HEADER_DATA {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00010 * (($index) - 0) + 0x02120 + $FM_L3AR_BASE + ($word)); # 0x12120 L3AR_BASE
}

sub FM_L3AR_TRAP_HEADER_DATA_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
    if($word == 4) {return 0x00000000;}
    if($word == 5) {return 0x00000000;}
    if($word == 6) {return 0x00000000;}
    if($word == 7) {return 0x00000000;}
    if($word == 8) {return 0x00000000;}
    if($word == 9) {return 0x00000000;}
    if($word == 10) {return 0x00000000;}
}

our $FM_L3AR_TRAP_HEADER_DATA_WIDTH               =  11; #
our $FM_L3AR_TRAP_HEADER_DATA_BITS                =  344; #

sub FM_L3AR_TRAP_HEADER_DATA_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
our $FM_L3AR_TRAP_HEADER_DATA_STRIDE              =  16; #


sub FM_L3AR_IP {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x02140 + $FM_L3AR_BASE); # 0x12140 L3AR_BASE
}

sub FM_L3AR_IP_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L3AR_IP_WIDTH                             =  1; #
our $FM_L3AR_IP_BITS                              =  32; #

sub FM_L3AR_IP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_L3AR_IP_STRIDE                            =  1; #


sub FM_L3AR_IM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x02148 + $FM_L3AR_BASE); # 0x12148 L3AR_BASE
}

sub FM_L3AR_IM_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L3AR_IM_WIDTH                             =  1; #
our $FM_L3AR_IM_BITS                              =  32; #

sub FM_L3AR_IM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_L3AR_IM_STRIDE                            =  1; #


sub FM_LBS_CAM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00000 + $FM_LBS_BASE); # 0x14000 LBS_BASE
}

sub FM_LBS_CAM_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_LBS_CAM_WIDTH                             =  1; #
our $FM_LBS_CAM_BITS                              =  32; #

sub FM_LBS_CAM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_LBS_CAM_STRIDE                            =  1; #


sub FM_LBS_PROFILE_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00080 + $FM_LBS_BASE); # 0x14080 LBS_BASE
}

sub FM_LBS_PROFILE_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_LBS_PROFILE_TABLE_WIDTH                   =  1; #
our $FM_LBS_PROFILE_TABLE_BITS                    =  8; #

sub FM_LBS_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_LBS_PROFILE_TABLE_STRIDE                  =  1; #


sub FM_STATS_AR_IDX_CAM {
    my ($self, $index2, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 5, -1);

    return (0x00100 * (($index2) - 0) + 0x00008 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x00000 + $FM_STATS_AR_BASE + ($word)); # 0x18000 STATS_AR_BASE
}

sub FM_STATS_AR_IDX_CAM_DEFAULT {
    my ($self, $index2, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 5, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_STATS_AR_IDX_CAM_WIDTH                    =  4; #
our $FM_STATS_AR_IDX_CAM_BITS                     =  128; #

sub FM_STATS_AR_IDX_CAM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
sub FM_STATS_AR_IDX_CAM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
sub FM_STATS_AR_IDX_CAM_ENTRIES_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 6;
}
our $FM_STATS_AR_IDX_CAM_STRIDE_0                 =  4; #
our $FM_STATS_AR_IDX_CAM_STRIDE_1                 =  8; #
our $FM_STATS_AR_IDX_CAM_STRIDE_2                 =  256; #


sub FM_STATS_AR_IDX_RAM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00020 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00800 + $FM_STATS_AR_BASE); # 0x18800 STATS_AR_BASE
}

sub FM_STATS_AR_IDX_RAM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_STATS_AR_IDX_RAM_WIDTH                    =  1; #
our $FM_STATS_AR_IDX_RAM_BITS                     =  5; #

sub FM_STATS_AR_IDX_RAM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
sub FM_STATS_AR_IDX_RAM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 6;
}
our $FM_STATS_AR_IDX_RAM_STRIDE_0                 =  1; #
our $FM_STATS_AR_IDX_RAM_STRIDE_1                 =  32; #


sub FM_STATS_AR_FLAGS_CAM1 {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00008 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x00A00 + $FM_STATS_AR_BASE + ($word)); # 0x18A00 STATS_AR_BASE
}

sub FM_STATS_AR_FLAGS_CAM1_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_STATS_AR_FLAGS_CAM1_WIDTH                 =  4; #
our $FM_STATS_AR_FLAGS_CAM1_BITS                  =  128; #

sub FM_STATS_AR_FLAGS_CAM1_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
sub FM_STATS_AR_FLAGS_CAM1_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}
our $FM_STATS_AR_FLAGS_CAM1_STRIDE_0              =  4; #
our $FM_STATS_AR_FLAGS_CAM1_STRIDE_1              =  8; #


sub FM_STATS_AR_FLAGS_CAM2 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00C00 + $FM_STATS_AR_BASE + ($word)); # 0x18C00 STATS_AR_BASE
}

sub FM_STATS_AR_FLAGS_CAM2_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_STATS_AR_FLAGS_CAM2_WIDTH                 =  4; #
our $FM_STATS_AR_FLAGS_CAM2_BITS                  =  128; #

sub FM_STATS_AR_FLAGS_CAM2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 90;
}
our $FM_STATS_AR_FLAGS_CAM2_STRIDE                =  4; #


sub FM_STATS_AR_FLAGS_CAM2_POLARITY {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00E00 + $FM_STATS_AR_BASE + ($word)); # 0x18E00 STATS_AR_BASE
}

sub FM_STATS_AR_FLAGS_CAM2_POLARITY_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_STATS_AR_FLAGS_CAM2_POLARITY_WIDTH        =  3; #
our $FM_STATS_AR_FLAGS_CAM2_POLARITY_BITS         =  90; #



sub FM_STATS_AR_FLAGS_CAM2_VALUE {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00E04 + $FM_STATS_AR_BASE + ($word)); # 0x18E04 STATS_AR_BASE
}

sub FM_STATS_AR_FLAGS_CAM2_VALUE_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_STATS_AR_FLAGS_CAM2_VALUE_WIDTH           =  3; #
our $FM_STATS_AR_FLAGS_CAM2_VALUE_BITS            =  90; #



sub FM_STATS_AR_RX_PORT_MAP {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00E80 + $FM_STATS_AR_BASE); # 0x18E80 STATS_AR_BASE
}

sub FM_STATS_AR_RX_PORT_MAP_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_STATS_AR_RX_PORT_MAP_WIDTH                =  1; #
our $FM_STATS_AR_RX_PORT_MAP_BITS                 =  13; #

sub FM_STATS_AR_RX_PORT_MAP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_STATS_AR_RX_PORT_MAP_STRIDE               =  1; #


sub FM_STATS_AR_TX_PORT_MAP {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00F00 + $FM_STATS_AR_BASE); # 0x18F00 STATS_AR_BASE
}

sub FM_STATS_AR_TX_PORT_MAP_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_STATS_AR_TX_PORT_MAP_WIDTH                =  1; #
our $FM_STATS_AR_TX_PORT_MAP_BITS                 =  13; #

sub FM_STATS_AR_TX_PORT_MAP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_STATS_AR_TX_PORT_MAP_STRIDE               =  1; #


sub FM_STATS_AR_RX_LENGTH_COMPARE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00F80 + $FM_STATS_AR_BASE); # 0x18F80 STATS_AR_BASE
}

sub FM_STATS_AR_RX_LENGTH_COMPARE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_STATS_AR_RX_LENGTH_COMPARE_WIDTH          =  1; #
our $FM_STATS_AR_RX_LENGTH_COMPARE_BITS           =  18; #

sub FM_STATS_AR_RX_LENGTH_COMPARE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_STATS_AR_RX_LENGTH_COMPARE_STRIDE         =  1; #


sub FM_STATS_AR_TX_LENGTH_COMPARE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00F90 + $FM_STATS_AR_BASE); # 0x18F90 STATS_AR_BASE
}

sub FM_STATS_AR_TX_LENGTH_COMPARE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_STATS_AR_TX_LENGTH_COMPARE_WIDTH          =  1; #
our $FM_STATS_AR_TX_LENGTH_COMPARE_BITS           =  18; #

sub FM_STATS_AR_TX_LENGTH_COMPARE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_STATS_AR_TX_LENGTH_COMPARE_STRIDE         =  1; #


sub FM_STATS_AR_BANK_CFG1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00FA0 + $FM_STATS_AR_BASE); # 0x18FA0 STATS_AR_BASE
}

sub FM_STATS_AR_BANK_CFG1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_STATS_AR_BANK_CFG1_WIDTH                  =  1; #
our $FM_STATS_AR_BANK_CFG1_BITS                   =  8; #

sub FM_STATS_AR_BANK_CFG1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_STATS_AR_BANK_CFG1_STRIDE                 =  1; #


sub FM_STATS_AR_BANK_CFG2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00FB0 + $FM_STATS_AR_BASE); # 0x18FB0 STATS_AR_BASE
}

sub FM_STATS_AR_BANK_CFG2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_STATS_AR_BANK_CFG2_WIDTH                  =  1; #
our $FM_STATS_AR_BANK_CFG2_BITS                   =  27; #

sub FM_STATS_AR_BANK_CFG2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_STATS_AR_BANK_CFG2_STRIDE                 =  1; #


sub FM_STATS_DISCRETE_COUNTER_FRAME {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00000 + $FM_STATS_DISCRETE_BASE + ($word)); # 0x1A000 STATS_DISCRETE_BASE
}

sub FM_STATS_DISCRETE_COUNTER_FRAME_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_STATS_DISCRETE_COUNTER_FRAME_WIDTH        =  2; #
our $FM_STATS_DISCRETE_COUNTER_FRAME_BITS         =  64; #

sub FM_STATS_DISCRETE_COUNTER_FRAME_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}
our $FM_STATS_DISCRETE_COUNTER_FRAME_STRIDE       =  2; #


sub FM_STATS_DISCRETE_COUNTER_BYTE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00080 + $FM_STATS_DISCRETE_BASE + ($word)); # 0x1A080 STATS_DISCRETE_BASE
}

sub FM_STATS_DISCRETE_COUNTER_BYTE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_STATS_DISCRETE_COUNTER_BYTE_WIDTH         =  2; #
our $FM_STATS_DISCRETE_COUNTER_BYTE_BITS          =  64; #

sub FM_STATS_DISCRETE_COUNTER_BYTE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}
our $FM_STATS_DISCRETE_COUNTER_BYTE_STRIDE        =  2; #


sub FM_GLOBAL_INTERRUPT_DETECT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00000 + $FM_MGMT2_BASE); # 0x1C000 MGMT2_BASE
}

sub FM_GLOBAL_INTERRUPT_DETECT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_GLOBAL_INTERRUPT_DETECT_WIDTH             =  1; #
our $FM_GLOBAL_INTERRUPT_DETECT_BITS              =  15; #



sub FM_INTERRUPT_MASK_INT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00001 + $FM_MGMT2_BASE); # 0x1C001 MGMT2_BASE
}

sub FM_INTERRUPT_MASK_INT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00007fff;
}

our $FM_INTERRUPT_MASK_INT_WIDTH                  =  1; #
our $FM_INTERRUPT_MASK_INT_BITS                   =  15; #



sub FM_INTERRUPT_MASK_PCIE {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00002 + $FM_MGMT2_BASE); # 0x1C002 MGMT2_BASE
}

sub FM_INTERRUPT_MASK_PCIE_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00007fff;
}

our $FM_INTERRUPT_MASK_PCIE_WIDTH                 =  1; #
our $FM_INTERRUPT_MASK_PCIE_BITS                  =  15; #



sub FM_INTERRUPT_MASK_FIBM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00003 + $FM_MGMT2_BASE); # 0x1C003 MGMT2_BASE
}

sub FM_INTERRUPT_MASK_FIBM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00007fff;
}

our $FM_INTERRUPT_MASK_FIBM_WIDTH                 =  1; #
our $FM_INTERRUPT_MASK_FIBM_BITS                  =  15; #



sub FM_GLOBAL_EPL_INT_DETECT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00004 + $FM_MGMT2_BASE); # 0x1C004 MGMT2_BASE
}

sub FM_GLOBAL_EPL_INT_DETECT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_GLOBAL_EPL_INT_DETECT_WIDTH               =  1; #
our $FM_GLOBAL_EPL_INT_DETECT_BITS                =  25; #



sub FM_SRAM_CORRECTED_IP {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00008 + $FM_MGMT2_BASE + ($word)); # 0x1C008 MGMT2_BASE
}

sub FM_SRAM_CORRECTED_IP_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_SRAM_CORRECTED_IP_WIDTH                   =  4; #
our $FM_SRAM_CORRECTED_IP_BITS                    =  128; #



sub FM_SRAM_CORRECTED_IM {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x0000C + $FM_MGMT2_BASE + ($word)); # 0x1C00C MGMT2_BASE
}

sub FM_SRAM_CORRECTED_IM_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_SRAM_CORRECTED_IM_WIDTH                   =  4; #
our $FM_SRAM_CORRECTED_IM_BITS                    =  128; #



sub FM_SRAM_UNCORRECTABLE_IP {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00010 + $FM_MGMT2_BASE + ($word)); # 0x1C010 MGMT2_BASE
}

sub FM_SRAM_UNCORRECTABLE_IP_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_SRAM_UNCORRECTABLE_IP_WIDTH               =  4; #
our $FM_SRAM_UNCORRECTABLE_IP_BITS                =  128; #



sub FM_SRAM_UNCORRECTABLE_IM {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00014 + $FM_MGMT2_BASE + ($word)); # 0x1C014 MGMT2_BASE
}

sub FM_SRAM_UNCORRECTABLE_IM_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_SRAM_UNCORRECTABLE_IM_WIDTH               =  4; #
our $FM_SRAM_UNCORRECTABLE_IM_BITS                =  128; #



sub FM_SRAM_UNCORRECTABLE_FATAL {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00018 + $FM_MGMT2_BASE + ($word)); # 0x1C018 MGMT2_BASE
}

sub FM_SRAM_UNCORRECTABLE_FATAL_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_SRAM_UNCORRECTABLE_FATAL_WIDTH            =  4; #
our $FM_SRAM_UNCORRECTABLE_FATAL_BITS             =  128; #



sub FM_SW_IP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0001C + $FM_MGMT2_BASE); # 0x1C01C MGMT2_BASE
}

sub FM_SW_IP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SW_IP_WIDTH                               =  1; #
our $FM_SW_IP_BITS                                =  32; #



sub FM_SW_IM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0001D + $FM_MGMT2_BASE); # 0x1C01D MGMT2_BASE
}

sub FM_SW_IM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0xffffffff;
}

our $FM_SW_IM_WIDTH                               =  1; #
our $FM_SW_IM_BITS                                =  32; #



sub FM_SW_TEST_AND_SET {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0001E + $FM_MGMT2_BASE); # 0x1C01E MGMT2_BASE
}

sub FM_SW_TEST_AND_SET_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SW_TEST_AND_SET_WIDTH                     =  1; #
our $FM_SW_TEST_AND_SET_BITS                      =  32; #



sub FM_FRAME_TIME_OUT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0001F + $FM_MGMT2_BASE); # 0x1C01F MGMT2_BASE
}

sub FM_FRAME_TIME_OUT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00002fa8;
}

our $FM_FRAME_TIME_OUT_WIDTH                      =  1; #
our $FM_FRAME_TIME_OUT_BITS                       =  28; #



sub FM_CHIP_VERSION {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00020 + $FM_MGMT2_BASE); # 0x1C020 MGMT2_BASE
}

sub FM_CHIP_VERSION_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_CHIP_VERSION_WIDTH                        =  1; #
our $FM_CHIP_VERSION_BITS                         =  7; #



sub FM_PIN_STRAP_STAT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00021 + $FM_MGMT2_BASE); # 0x1C021 MGMT2_BASE
}

sub FM_PIN_STRAP_STAT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PIN_STRAP_STAT_WIDTH                      =  1; #
our $FM_PIN_STRAP_STAT_BITS                       =  15; #



sub FM_BOOT_CTRL {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00022 + $FM_MGMT2_BASE); # 0x1C022 MGMT2_BASE
}

sub FM_BOOT_CTRL_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_BOOT_CTRL_WIDTH                           =  1; #
our $FM_BOOT_CTRL_BITS                            =  32; #



sub FM_BOOT_ARGS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00023 + $FM_MGMT2_BASE); # 0x1C023 MGMT2_BASE
}

sub FM_BOOT_ARGS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_BOOT_ARGS_WIDTH                           =  1; #
our $FM_BOOT_ARGS_BITS                            =  32; #



sub FM_GPIO_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00024 + $FM_MGMT2_BASE); # 0x1C024 MGMT2_BASE
}

sub FM_GPIO_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_GPIO_CFG_WIDTH                            =  1; #
our $FM_GPIO_CFG_BITS                             =  32; #



sub FM_GPIO_DATA {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00025 + $FM_MGMT2_BASE); # 0x1C025 MGMT2_BASE
}

sub FM_GPIO_DATA_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_GPIO_DATA_WIDTH                           =  1; #
our $FM_GPIO_DATA_BITS                            =  16; #



sub FM_GPIO_IP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00026 + $FM_MGMT2_BASE); # 0x1C026 MGMT2_BASE
}

sub FM_GPIO_IP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_GPIO_IP_WIDTH                             =  1; #
our $FM_GPIO_IP_BITS                              =  32; #



sub FM_GPIO_IM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00027 + $FM_MGMT2_BASE); # 0x1C027 MGMT2_BASE
}

sub FM_GPIO_IM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0xffffffff;
}

our $FM_GPIO_IM_WIDTH                             =  1; #
our $FM_GPIO_IM_BITS                              =  32; #



sub FM_I2C_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00028 + $FM_MGMT2_BASE); # 0x1C028 MGMT2_BASE
}

sub FM_I2C_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x02900c81;
}

our $FM_I2C_CFG_WIDTH                             =  1; #
our $FM_I2C_CFG_BITS                              =  28; #



sub FM_I2C_DATA {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0002C + $FM_MGMT2_BASE); # 0x1C02C MGMT2_BASE
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

    return 3;
}
our $FM_I2C_DATA_STRIDE                           =  1; #


sub FM_I2C_CTRL {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00030 + $FM_MGMT2_BASE); # 0x1C030 MGMT2_BASE
}

sub FM_I2C_CTRL_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x03c00000;
}

our $FM_I2C_CTRL_WIDTH                            =  1; #
our $FM_I2C_CTRL_BITS                             =  27; #



sub FM_MDIO_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00031 + $FM_MGMT2_BASE); # 0x1C031 MGMT2_BASE
}

sub FM_MDIO_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00003010;
}

our $FM_MDIO_CFG_WIDTH                            =  1; #
our $FM_MDIO_CFG_BITS                             =  14; #



sub FM_MDIO_DATA {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00032 + $FM_MGMT2_BASE); # 0x1C032 MGMT2_BASE
}

sub FM_MDIO_DATA_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MDIO_DATA_WIDTH                           =  1; #
our $FM_MDIO_DATA_BITS                            =  16; #



sub FM_MDIO_CTRL {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00033 + $FM_MGMT2_BASE); # 0x1C033 MGMT2_BASE
}

sub FM_MDIO_CTRL_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MDIO_CTRL_WIDTH                           =  1; #
our $FM_MDIO_CTRL_BITS                            =  32; #



sub FM_SPI_TX_DATA {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00034 + $FM_MGMT2_BASE); # 0x1C034 MGMT2_BASE
}

sub FM_SPI_TX_DATA_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SPI_TX_DATA_WIDTH                         =  1; #
our $FM_SPI_TX_DATA_BITS                          =  32; #



sub FM_SPI_RX_DATA {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00035 + $FM_MGMT2_BASE); # 0x1C035 MGMT2_BASE
}

sub FM_SPI_RX_DATA_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SPI_RX_DATA_WIDTH                         =  1; #
our $FM_SPI_RX_DATA_BITS                          =  32; #



sub FM_SPI_HEADER {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00036 + $FM_MGMT2_BASE); # 0x1C036 MGMT2_BASE
}

sub FM_SPI_HEADER_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SPI_HEADER_WIDTH                          =  1; #
our $FM_SPI_HEADER_BITS                           =  32; #



sub FM_SPI_CTRL {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00037 + $FM_MGMT2_BASE); # 0x1C037 MGMT2_BASE
}

sub FM_SPI_CTRL_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0000003e;
}

our $FM_SPI_CTRL_WIDTH                            =  1; #
our $FM_SPI_CTRL_BITS                             =  23; #



sub FM_LED_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00038 + $FM_MGMT2_BASE); # 0x1C038 MGMT2_BASE
}

sub FM_LED_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0001e848;
}

our $FM_LED_CFG_WIDTH                             =  1; #
our $FM_LED_CFG_BITS                              =  25; #



sub FM_SCAN_CONTROL {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00039 + $FM_MGMT2_BASE); # 0x1C039 MGMT2_BASE
}

sub FM_SCAN_CONTROL_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000010;
}

our $FM_SCAN_CONTROL_WIDTH                        =  1; #
our $FM_SCAN_CONTROL_BITS                         =  5; #



sub FM_SCAN_CONFIG_DATA_IN {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0003A + $FM_MGMT2_BASE); # 0x1C03A MGMT2_BASE
}

sub FM_SCAN_CONFIG_DATA_IN_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SCAN_CONFIG_DATA_IN_WIDTH                 =  1; #
our $FM_SCAN_CONFIG_DATA_IN_BITS                  =  32; #



sub FM_SCAN_CHAIN_DATA_IN {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0003B + $FM_MGMT2_BASE); # 0x1C03B MGMT2_BASE
}

sub FM_SCAN_CHAIN_DATA_IN_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SCAN_CHAIN_DATA_IN_WIDTH                  =  1; #
our $FM_SCAN_CHAIN_DATA_IN_BITS                   =  32; #



sub FM_SCAN_DATA_OUT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0003C + $FM_MGMT2_BASE); # 0x1C03C MGMT2_BASE
}

sub FM_SCAN_DATA_OUT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SCAN_DATA_OUT_WIDTH                       =  1; #
our $FM_SCAN_DATA_OUT_BITS                        =  32; #



sub FM_SCAN_STATUS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0003D + $FM_MGMT2_BASE); # 0x1C03D MGMT2_BASE
}

sub FM_SCAN_STATUS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SCAN_STATUS_WIDTH                         =  1; #
our $FM_SCAN_STATUS_BITS                          =  10; #



sub FM_ETHCLK_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0003E + $FM_MGMT2_BASE); # 0x1C03E MGMT2_BASE
}

sub FM_ETHCLK_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_ETHCLK_CFG_WIDTH                          =  1; #
our $FM_ETHCLK_CFG_BITS                           =  18; #

sub FM_ETHCLK_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
our $FM_ETHCLK_CFG_STRIDE                         =  1; #


sub FM_ETHCLK_RATIO {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00040 + $FM_MGMT2_BASE); # 0x1C040 MGMT2_BASE
}

sub FM_ETHCLK_RATIO_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_ETHCLK_RATIO_WIDTH                        =  1; #
our $FM_ETHCLK_RATIO_BITS                         =  24; #

sub FM_ETHCLK_RATIO_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
our $FM_ETHCLK_RATIO_STRIDE                       =  1; #


sub FM_PLL_CTRL {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00042 + $FM_MGMT2_BASE + ($word)); # 0x1C042 MGMT2_BASE
}

sub FM_PLL_CTRL_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x20841436;}
    if($word == 1) {return 0x00005560;}
}

our $FM_PLL_CTRL_WIDTH                            =  2; #
our $FM_PLL_CTRL_BITS                             =  49; #



sub FM_DLL_CTRL {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00044 + $FM_MGMT2_BASE + ($word)); # 0x1C044 MGMT2_BASE
}

sub FM_DLL_CTRL_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x08011b05;}
    if($word == 1) {return 0x00000000;}
}

our $FM_DLL_CTRL_WIDTH                            =  2; #
our $FM_DLL_CTRL_BITS                             =  34; #



sub FM_PLL_STAT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00046 + $FM_MGMT2_BASE); # 0x1C046 MGMT2_BASE
}

sub FM_PLL_STAT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_PLL_STAT_WIDTH                            =  1; #
our $FM_PLL_STAT_BITS                             =  4; #



sub FM_SWEEPER_CFG {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00048 + $FM_MGMT2_BASE + ($word)); # 0x1C048 MGMT2_BASE
}

sub FM_SWEEPER_CFG_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
    if($word == 4) {return 0x00000000;}
}

our $FM_SWEEPER_CFG_WIDTH                         =  5; #
our $FM_SWEEPER_CFG_BITS                          =  144; #



sub FM_S2A_EN_STATUS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00050 + $FM_MGMT2_BASE); # 0x1C050 MGMT2_BASE
}

sub FM_S2A_EN_STATUS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_S2A_EN_STATUS_WIDTH                       =  1; #
our $FM_S2A_EN_STATUS_BITS                        =  10; #



sub FM_RO_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00052 + $FM_MGMT2_BASE); # 0x1C052 MGMT2_BASE
}

sub FM_RO_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_RO_CFG_WIDTH                              =  1; #
our $FM_RO_CFG_BITS                               =  2; #

sub FM_RO_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
our $FM_RO_CFG_STRIDE                             =  1; #


sub FM_TESTCTRL_MOD_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00054 + $FM_MGMT2_BASE); # 0x1C054 MGMT2_BASE
}

sub FM_TESTCTRL_MOD_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_TESTCTRL_MOD_CFG_WIDTH                    =  1; #
our $FM_TESTCTRL_MOD_CFG_BITS                     =  1; #



sub FM_TESTCTRL_TICK_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00055 + $FM_MGMT2_BASE); # 0x1C055 MGMT2_BASE
}

sub FM_TESTCTRL_TICK_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_TESTCTRL_TICK_CFG_WIDTH                   =  1; #
our $FM_TESTCTRL_TICK_CFG_BITS                    =  16; #



sub FM_TESTCTRL_TICK_CNT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00056 + $FM_MGMT2_BASE); # 0x1C056 MGMT2_BASE
}

sub FM_TESTCTRL_TICK_CNT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_TESTCTRL_TICK_CNT_WIDTH                   =  1; #
our $FM_TESTCTRL_TICK_CNT_BITS                    =  16; #



sub FM_TESTCTRL_CLK_CNT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00057 + $FM_MGMT2_BASE); # 0x1C057 MGMT2_BASE
}

sub FM_TESTCTRL_CLK_CNT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_TESTCTRL_CLK_CNT_WIDTH                    =  1; #
our $FM_TESTCTRL_CLK_CNT_BITS                     =  32; #



sub FM_TESTCTRL_START {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00058 + $FM_MGMT2_BASE); # 0x1C058 MGMT2_BASE
}

sub FM_TESTCTRL_START_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_TESTCTRL_START_WIDTH                      =  1; #
our $FM_TESTCTRL_START_BITS                       =  1; #



sub FM_FUSEBOX {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x01000 + $FM_MGMT2_BASE); # 0x1D000 MGMT2_BASE
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


sub FM_BM_MARCH_SEQUENCE {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x01080 + $FM_MGMT2_BASE + ($word)); # 0x1D080 MGMT2_BASE
}

sub FM_BM_MARCH_SEQUENCE_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0xfca952b9;}
    if($word == 1) {return 0xa9fca99e;}
    if($word == 2) {return 0xb16529ed;}
    if($word == 3) {return 0x7430fdb9;}
}

our $FM_BM_MARCH_SEQUENCE_WIDTH                   =  4; #
our $FM_BM_MARCH_SEQUENCE_BITS                    =  128; #



sub FM_BM_GENERAL_CONFIG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01084 + $FM_MGMT2_BASE); # 0x1D084 MGMT2_BASE
}

sub FM_BM_GENERAL_CONFIG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000080;
}

our $FM_BM_GENERAL_CONFIG_WIDTH                   =  1; #
our $FM_BM_GENERAL_CONFIG_BITS                    =  8; #



sub FM_BM_TXQ_HS_SEGMENTS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01085 + $FM_MGMT2_BASE); # 0x1D085 MGMT2_BASE
}

sub FM_BM_TXQ_HS_SEGMENTS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00003fff;
}

our $FM_BM_TXQ_HS_SEGMENTS_WIDTH                  =  1; #
our $FM_BM_TXQ_HS_SEGMENTS_BITS                   =  15; #



sub FM_BM_RXQ_PAGES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01086 + $FM_MGMT2_BASE); # 0x1D086 MGMT2_BASE
}

sub FM_BM_RXQ_PAGES_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000400;
}

our $FM_BM_RXQ_PAGES_WIDTH                        =  1; #
our $FM_BM_RXQ_PAGES_BITS                         =  11; #



sub FM_BM_MODEL_INFO {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01087 + $FM_MGMT2_BASE); # 0x1D087 MGMT2_BASE
}

sub FM_BM_MODEL_INFO_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x0000bfff;
}

our $FM_BM_MODEL_INFO_WIDTH                       =  1; #
our $FM_BM_MODEL_INFO_BITS                        =  24; #



sub FM_BM_FFU_SLICE_MASK {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01088 + $FM_MGMT2_BASE); # 0x1D088 MGMT2_BASE
}

sub FM_BM_FFU_SLICE_MASK_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_BM_FFU_SLICE_MASK_WIDTH                   =  1; #
our $FM_BM_FFU_SLICE_MASK_BITS                    =  24; #



sub FM_BM_VRM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01089 + $FM_MGMT2_BASE); # 0x1D089 MGMT2_BASE
}

sub FM_BM_VRM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_BM_VRM_WIDTH                              =  1; #
our $FM_BM_VRM_BITS                               =  8; #



sub FM_BM_MAX_ALLOWED_REPAIRS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0108A + $FM_MGMT2_BASE); # 0x1D08A MGMT2_BASE
}

sub FM_BM_MAX_ALLOWED_REPAIRS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000014;
}

our $FM_BM_MAX_ALLOWED_REPAIRS_WIDTH              =  1; #
our $FM_BM_MAX_ALLOWED_REPAIRS_BITS               =  6; #



sub FM_BM_FUSEBOX_SUMMARY {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0108B + $FM_MGMT2_BASE); # 0x1D08B MGMT2_BASE
}

sub FM_BM_FUSEBOX_SUMMARY_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_BM_FUSEBOX_SUMMARY_WIDTH                  =  1; #
our $FM_BM_FUSEBOX_SUMMARY_BITS                   =  18; #



sub FM_BM_IP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0108C + $FM_MGMT2_BASE); # 0x1D08C MGMT2_BASE
}

sub FM_BM_IP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_BM_IP_WIDTH                               =  1; #
our $FM_BM_IP_BITS                                =  20; #



sub FM_BM_IM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0108D + $FM_MGMT2_BASE); # 0x1D08D MGMT2_BASE
}

sub FM_BM_IM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_BM_IM_WIDTH                               =  1; #
our $FM_BM_IM_BITS                                =  20; #



sub FM_BM_ENGINE_STATUS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0108E + $FM_MGMT2_BASE); # 0x1D08E MGMT2_BASE
}

sub FM_BM_ENGINE_STATUS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_BM_ENGINE_STATUS_WIDTH                    =  1; #
our $FM_BM_ENGINE_STATUS_BITS                     =  11; #



sub FM_BM_FUSEBOX_BURN_TIME {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0108F + $FM_MGMT2_BASE); # 0x1D08F MGMT2_BASE
}

sub FM_BM_FUSEBOX_BURN_TIME_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000271;
}

our $FM_BM_FUSEBOX_BURN_TIME_WIDTH                =  1; #
our $FM_BM_FUSEBOX_BURN_TIME_BITS                 =  10; #



sub FM_BM_FUSEBOX_APPEND {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01090 + $FM_MGMT2_BASE); # 0x1D090 MGMT2_BASE
}

sub FM_BM_FUSEBOX_APPEND_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_BM_FUSEBOX_APPEND_WIDTH                   =  1; #
our $FM_BM_FUSEBOX_APPEND_BITS                    =  32; #



sub FM_BM_START_OPERATION {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01091 + $FM_MGMT2_BASE); # 0x1D091 MGMT2_BASE
}

sub FM_BM_START_OPERATION_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_BM_START_OPERATION_WIDTH                  =  1; #
our $FM_BM_START_OPERATION_BITS                   =  3; #



sub FM_BM_DEBUG_STATUS_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01092 + $FM_MGMT2_BASE); # 0x1D092 MGMT2_BASE
}

sub FM_BM_DEBUG_STATUS_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_BM_DEBUG_STATUS_1_WIDTH                   =  1; #
our $FM_BM_DEBUG_STATUS_1_BITS                    =  24; #



sub FM_BM_DEBUG_STATUS_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01093 + $FM_MGMT2_BASE); # 0x1D093 MGMT2_BASE
}

sub FM_BM_DEBUG_STATUS_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_BM_DEBUG_STATUS_2_WIDTH                   =  1; #
our $FM_BM_DEBUG_STATUS_2_BITS                    =  16; #



sub FM_TEN_T_BIST_MARCH_CONFIG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x01100 + $FM_MGMT2_BASE); # 0x1D100 MGMT2_BASE
}

sub FM_TEN_T_BIST_MARCH_CONFIG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_TEN_T_BIST_MARCH_CONFIG_WIDTH             =  1; #
our $FM_TEN_T_BIST_MARCH_CONFIG_BITS              =  19; #

sub FM_TEN_T_BIST_MARCH_CONFIG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_TEN_T_BIST_MARCH_CONFIG_STRIDE            =  1; #


sub FM_TEN_T_BIST_GENERAL_CONFIG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01104 + $FM_MGMT2_BASE); # 0x1D104 MGMT2_BASE
}

sub FM_TEN_T_BIST_GENERAL_CONFIG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_TEN_T_BIST_GENERAL_CONFIG_WIDTH           =  1; #
our $FM_TEN_T_BIST_GENERAL_CONFIG_BITS            =  1; #



sub FM_TEN_T_BIST_STATUS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01105 + $FM_MGMT2_BASE); # 0x1D105 MGMT2_BASE
}

sub FM_TEN_T_BIST_STATUS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_TEN_T_BIST_STATUS_WIDTH                   =  1; #
our $FM_TEN_T_BIST_STATUS_BITS                    =  2; #



sub FM_TEN_T_BIST_USER_CHECKER_MASK {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01106 + $FM_MGMT2_BASE); # 0x1D106 MGMT2_BASE
}

sub FM_TEN_T_BIST_USER_CHECKER_MASK_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_TEN_T_BIST_USER_CHECKER_MASK_WIDTH        =  1; #
our $FM_TEN_T_BIST_USER_CHECKER_MASK_BITS         =  16; #



sub FM_TEN_T_BIST_IP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01107 + $FM_MGMT2_BASE); # 0x1D107 MGMT2_BASE
}

sub FM_TEN_T_BIST_IP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_TEN_T_BIST_IP_WIDTH                       =  1; #
our $FM_TEN_T_BIST_IP_BITS                        =  4; #



sub FM_TEN_T_BIST_IM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01108 + $FM_MGMT2_BASE); # 0x1D108 MGMT2_BASE
}

sub FM_TEN_T_BIST_IM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_TEN_T_BIST_IM_WIDTH                       =  1; #
our $FM_TEN_T_BIST_IM_BITS                        =  4; #



sub FM_TEN_T_BIST_USER_OP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01109 + $FM_MGMT2_BASE); # 0x1D109 MGMT2_BASE
}

sub FM_TEN_T_BIST_USER_OP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_TEN_T_BIST_USER_OP_WIDTH                  =  1; #
our $FM_TEN_T_BIST_USER_OP_BITS                   =  24; #



sub FM_TEN_T_BIST_PAIRED_READ {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0110A + $FM_MGMT2_BASE); # 0x1D10A MGMT2_BASE
}

sub FM_TEN_T_BIST_PAIRED_READ_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_TEN_T_BIST_PAIRED_READ_WIDTH              =  1; #
our $FM_TEN_T_BIST_PAIRED_READ_BITS               =  19; #



sub FM_TEN_T_BIST_CURRENT_ADDR {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0110B + $FM_MGMT2_BASE); # 0x1D10B MGMT2_BASE
}

sub FM_TEN_T_BIST_CURRENT_ADDR_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_TEN_T_BIST_CURRENT_ADDR_WIDTH             =  1; #
our $FM_TEN_T_BIST_CURRENT_ADDR_BITS              =  16; #



sub FM_TEN_T_BIST_START_SEQUENCE {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0110C + $FM_MGMT2_BASE); # 0x1D10C MGMT2_BASE
}

sub FM_TEN_T_BIST_START_SEQUENCE_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_TEN_T_BIST_START_SEQUENCE_WIDTH           =  1; #
our $FM_TEN_T_BIST_START_SEQUENCE_BITS            =  1; #



sub FM_TEN_T_BIST_SAVED_ADDRESS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0110D + $FM_MGMT2_BASE); # 0x1D10D MGMT2_BASE
}

sub FM_TEN_T_BIST_SAVED_ADDRESS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_TEN_T_BIST_SAVED_ADDRESS_WIDTH            =  1; #
our $FM_TEN_T_BIST_SAVED_ADDRESS_BITS             =  16; #



sub FM_TEN_T_BIST_DEBUG_STATUS_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0110E + $FM_MGMT2_BASE); # 0x1D10E MGMT2_BASE
}

sub FM_TEN_T_BIST_DEBUG_STATUS_1_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_TEN_T_BIST_DEBUG_STATUS_1_WIDTH           =  1; #
our $FM_TEN_T_BIST_DEBUG_STATUS_1_BITS            =  18; #



sub FM_TEN_T_BIST_DEBUG_STATUS_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0110F + $FM_MGMT2_BASE); # 0x1D10F MGMT2_BASE
}

sub FM_TEN_T_BIST_DEBUG_STATUS_2_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_TEN_T_BIST_DEBUG_STATUS_2_WIDTH           =  1; #
our $FM_TEN_T_BIST_DEBUG_STATUS_2_BITS            =  16; #



sub FM_TEN_T_BIST_CHAIN_LATENCY {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01140 + $FM_MGMT2_BASE); # 0x1D140 MGMT2_BASE
}

sub FM_TEN_T_BIST_CHAIN_LATENCY_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000002;
}

our $FM_TEN_T_BIST_CHAIN_LATENCY_WIDTH            =  1; #
our $FM_TEN_T_BIST_CHAIN_LATENCY_BITS             =  5; #



sub FM_TEN_T_BIST_CHAIN_CDC {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01141 + $FM_MGMT2_BASE); # 0x1D141 MGMT2_BASE
}

sub FM_TEN_T_BIST_CHAIN_CDC_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_TEN_T_BIST_CHAIN_CDC_WIDTH                =  1; #
our $FM_TEN_T_BIST_CHAIN_CDC_BITS                 =  12; #



sub FM_CDP_BIST_REPAIR {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00080 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x01200 + $FM_MGMT2_BASE); # 0x1D200 MGMT2_BASE
}

sub FM_CDP_BIST_REPAIR_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_CDP_BIST_REPAIR_WIDTH                     =  1; #
our $FM_CDP_BIST_REPAIR_BITS                      =  28; #

sub FM_CDP_BIST_REPAIR_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 10;
}
sub FM_CDP_BIST_REPAIR_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_REPAIR_STRIDE_0                  =  1; #
our $FM_CDP_BIST_REPAIR_STRIDE_1                  =  128; #


sub FM_CDP_BIST_MARCH_CONFIG {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00080 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x01210 + $FM_MGMT2_BASE); # 0x1D210 MGMT2_BASE
}

sub FM_CDP_BIST_MARCH_CONFIG_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_CDP_BIST_MARCH_CONFIG_WIDTH               =  1; #
our $FM_CDP_BIST_MARCH_CONFIG_BITS                =  22; #

sub FM_CDP_BIST_MARCH_CONFIG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_CDP_BIST_MARCH_CONFIG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_MARCH_CONFIG_STRIDE_0            =  1; #
our $FM_CDP_BIST_MARCH_CONFIG_STRIDE_1            =  128; #


sub FM_CDP_BIST_DEFECT_MAP {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00080 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x01214 + $FM_MGMT2_BASE); # 0x1D214 MGMT2_BASE
}

sub FM_CDP_BIST_DEFECT_MAP_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_CDP_BIST_DEFECT_MAP_WIDTH                 =  1; #
our $FM_CDP_BIST_DEFECT_MAP_BITS                  =  32; #

sub FM_CDP_BIST_DEFECT_MAP_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_CDP_BIST_DEFECT_MAP_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_DEFECT_MAP_STRIDE_0              =  1; #
our $FM_CDP_BIST_DEFECT_MAP_STRIDE_1              =  128; #


sub FM_CDP_BIST_GENERAL_CONFIG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x01218 + $FM_MGMT2_BASE); # 0x1D218 MGMT2_BASE
}

sub FM_CDP_BIST_GENERAL_CONFIG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CDP_BIST_GENERAL_CONFIG_WIDTH             =  1; #
our $FM_CDP_BIST_GENERAL_CONFIG_BITS              =  9; #

sub FM_CDP_BIST_GENERAL_CONFIG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_GENERAL_CONFIG_STRIDE            =  128; #


sub FM_CDP_BIST_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x01219 + $FM_MGMT2_BASE); # 0x1D219 MGMT2_BASE
}

sub FM_CDP_BIST_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CDP_BIST_STATUS_WIDTH                     =  1; #
our $FM_CDP_BIST_STATUS_BITS                      =  12; #

sub FM_CDP_BIST_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_STATUS_STRIDE                    =  128; #


sub FM_CDP_BIST_USER_CHECKER_MASKS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x0121A + $FM_MGMT2_BASE); # 0x1D21A MGMT2_BASE
}

sub FM_CDP_BIST_USER_CHECKER_MASKS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CDP_BIST_USER_CHECKER_MASKS_WIDTH         =  1; #
our $FM_CDP_BIST_USER_CHECKER_MASKS_BITS          =  14; #

sub FM_CDP_BIST_USER_CHECKER_MASKS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_USER_CHECKER_MASKS_STRIDE        =  128; #


sub FM_CDP_BIST_IP {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x0121B + $FM_MGMT2_BASE); # 0x1D21B MGMT2_BASE
}

sub FM_CDP_BIST_IP_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CDP_BIST_IP_WIDTH                         =  1; #
our $FM_CDP_BIST_IP_BITS                          =  4; #

sub FM_CDP_BIST_IP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_IP_STRIDE                        =  128; #


sub FM_CDP_BIST_IM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x0121C + $FM_MGMT2_BASE); # 0x1D21C MGMT2_BASE
}

sub FM_CDP_BIST_IM_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CDP_BIST_IM_WIDTH                         =  1; #
our $FM_CDP_BIST_IM_BITS                          =  4; #

sub FM_CDP_BIST_IM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_IM_STRIDE                        =  128; #


sub FM_CDP_BIST_CLEAR_REPAIRS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x0121D + $FM_MGMT2_BASE); # 0x1D21D MGMT2_BASE
}

sub FM_CDP_BIST_CLEAR_REPAIRS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CDP_BIST_CLEAR_REPAIRS_WIDTH              =  1; #
our $FM_CDP_BIST_CLEAR_REPAIRS_BITS               =  1; #

sub FM_CDP_BIST_CLEAR_REPAIRS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_CLEAR_REPAIRS_STRIDE             =  128; #


sub FM_CDP_BIST_ADD_REPAIR {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x0121E + $FM_MGMT2_BASE); # 0x1D21E MGMT2_BASE
}

sub FM_CDP_BIST_ADD_REPAIR_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CDP_BIST_ADD_REPAIR_WIDTH                 =  1; #
our $FM_CDP_BIST_ADD_REPAIR_BITS                  =  28; #

sub FM_CDP_BIST_ADD_REPAIR_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_ADD_REPAIR_STRIDE                =  128; #


sub FM_CDP_BIST_USER_OP {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x0121F + $FM_MGMT2_BASE); # 0x1D21F MGMT2_BASE
}

sub FM_CDP_BIST_USER_OP_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CDP_BIST_USER_OP_WIDTH                    =  1; #
our $FM_CDP_BIST_USER_OP_BITS                     =  32; #

sub FM_CDP_BIST_USER_OP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_USER_OP_STRIDE                   =  128; #


sub FM_CDP_BIST_START_SEQUENCE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x01220 + $FM_MGMT2_BASE); # 0x1D220 MGMT2_BASE
}

sub FM_CDP_BIST_START_SEQUENCE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CDP_BIST_START_SEQUENCE_WIDTH             =  1; #
our $FM_CDP_BIST_START_SEQUENCE_BITS              =  3; #

sub FM_CDP_BIST_START_SEQUENCE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_START_SEQUENCE_STRIDE            =  128; #


sub FM_CDP_BIST_BLOCK_HALFCHUNK {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x01221 + $FM_MGMT2_BASE); # 0x1D221 MGMT2_BASE
}

sub FM_CDP_BIST_BLOCK_HALFCHUNK_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CDP_BIST_BLOCK_HALFCHUNK_WIDTH            =  1; #
our $FM_CDP_BIST_BLOCK_HALFCHUNK_BITS             =  11; #

sub FM_CDP_BIST_BLOCK_HALFCHUNK_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_BLOCK_HALFCHUNK_STRIDE           =  128; #


sub FM_CDP_BIST_SEGMENT_COUNT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x01222 + $FM_MGMT2_BASE); # 0x1D222 MGMT2_BASE
}

sub FM_CDP_BIST_SEGMENT_COUNT_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CDP_BIST_SEGMENT_COUNT_WIDTH              =  1; #
our $FM_CDP_BIST_SEGMENT_COUNT_BITS               =  19; #

sub FM_CDP_BIST_SEGMENT_COUNT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_SEGMENT_COUNT_STRIDE             =  128; #


sub FM_CDP_BIST_NEXT_SEGMENT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x01223 + $FM_MGMT2_BASE); # 0x1D223 MGMT2_BASE
}

sub FM_CDP_BIST_NEXT_SEGMENT_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CDP_BIST_NEXT_SEGMENT_WIDTH               =  1; #
our $FM_CDP_BIST_NEXT_SEGMENT_BITS                =  20; #

sub FM_CDP_BIST_NEXT_SEGMENT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_NEXT_SEGMENT_STRIDE              =  128; #


sub FM_CDP_BIST_NEXT_NEW_REPAIR {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x01224 + $FM_MGMT2_BASE); # 0x1D224 MGMT2_BASE
}

sub FM_CDP_BIST_NEXT_NEW_REPAIR_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CDP_BIST_NEXT_NEW_REPAIR_WIDTH            =  1; #
our $FM_CDP_BIST_NEXT_NEW_REPAIR_BITS             =  30; #

sub FM_CDP_BIST_NEXT_NEW_REPAIR_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_NEXT_NEW_REPAIR_STRIDE           =  128; #


sub FM_CDP_BIST_SAVED_ADDRESS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x01225 + $FM_MGMT2_BASE); # 0x1D225 MGMT2_BASE
}

sub FM_CDP_BIST_SAVED_ADDRESS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CDP_BIST_SAVED_ADDRESS_WIDTH              =  1; #
our $FM_CDP_BIST_SAVED_ADDRESS_BITS               =  19; #

sub FM_CDP_BIST_SAVED_ADDRESS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_SAVED_ADDRESS_STRIDE             =  128; #


sub FM_CDP_BIST_DEBUG_STATUS_1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x01226 + $FM_MGMT2_BASE); # 0x1D226 MGMT2_BASE
}

sub FM_CDP_BIST_DEBUG_STATUS_1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CDP_BIST_DEBUG_STATUS_1_WIDTH             =  1; #
our $FM_CDP_BIST_DEBUG_STATUS_1_BITS              =  18; #

sub FM_CDP_BIST_DEBUG_STATUS_1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_DEBUG_STATUS_1_STRIDE            =  128; #


sub FM_CDP_BIST_DEBUG_STATUS_2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x01227 + $FM_MGMT2_BASE); # 0x1D227 MGMT2_BASE
}

sub FM_CDP_BIST_DEBUG_STATUS_2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CDP_BIST_DEBUG_STATUS_2_WIDTH             =  1; #
our $FM_CDP_BIST_DEBUG_STATUS_2_BITS              =  32; #

sub FM_CDP_BIST_DEBUG_STATUS_2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_DEBUG_STATUS_2_STRIDE            =  128; #


sub FM_CDP_BIST_CHAIN_GENERAL_CONFIG {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00080 * (($index1) - 0) + 0x00020 * (($index0) - 0) + 0x01240 + $FM_MGMT2_BASE); # 0x1D240 MGMT2_BASE
}

sub FM_CDP_BIST_CHAIN_GENERAL_CONFIG_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_CDP_BIST_CHAIN_GENERAL_CONFIG_WIDTH       =  1; #
our $FM_CDP_BIST_CHAIN_GENERAL_CONFIG_BITS        =  1; #

sub FM_CDP_BIST_CHAIN_GENERAL_CONFIG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
sub FM_CDP_BIST_CHAIN_GENERAL_CONFIG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_CHAIN_GENERAL_CONFIG_STRIDE_0    =  32; #
our $FM_CDP_BIST_CHAIN_GENERAL_CONFIG_STRIDE_1    =  128; #


sub FM_CDP_BIST_CHAIN_LATENCY {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00080 * (($index1) - 0) + 0x00020 * (($index0) - 0) + 0x01241 + $FM_MGMT2_BASE); # 0x1D241 MGMT2_BASE
}

sub FM_CDP_BIST_CHAIN_LATENCY_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000002;
}

our $FM_CDP_BIST_CHAIN_LATENCY_WIDTH              =  1; #
our $FM_CDP_BIST_CHAIN_LATENCY_BITS               =  5; #

sub FM_CDP_BIST_CHAIN_LATENCY_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
sub FM_CDP_BIST_CHAIN_LATENCY_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_CHAIN_LATENCY_STRIDE_0           =  32; #
our $FM_CDP_BIST_CHAIN_LATENCY_STRIDE_1           =  128; #


sub FM_CDP_BIST_CHAIN_CDC {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00080 * (($index1) - 0) + 0x00020 * (($index0) - 0) + 0x01242 + $FM_MGMT2_BASE); # 0x1D242 MGMT2_BASE
}

sub FM_CDP_BIST_CHAIN_CDC_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_CDP_BIST_CHAIN_CDC_WIDTH                  =  1; #
our $FM_CDP_BIST_CHAIN_CDC_BITS                   =  20; #

sub FM_CDP_BIST_CHAIN_CDC_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
sub FM_CDP_BIST_CHAIN_CDC_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CDP_BIST_CHAIN_CDC_STRIDE_0               =  32; #
our $FM_CDP_BIST_CHAIN_CDC_STRIDE_1               =  128; #


sub FM_SPDP_BIST_MARCH_CONFIG {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00080 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x01400 + $FM_MGMT2_BASE); # 0x1D400 MGMT2_BASE
}

sub FM_SPDP_BIST_MARCH_CONFIG_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_SPDP_BIST_MARCH_CONFIG_WIDTH              =  1; #
our $FM_SPDP_BIST_MARCH_CONFIG_BITS               =  22; #

sub FM_SPDP_BIST_MARCH_CONFIG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_SPDP_BIST_MARCH_CONFIG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_SPDP_BIST_MARCH_CONFIG_STRIDE_0           =  1; #
our $FM_SPDP_BIST_MARCH_CONFIG_STRIDE_1           =  128; #


sub FM_SPDP_BIST_GENERAL_CONFIG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x01404 + $FM_MGMT2_BASE); # 0x1D404 MGMT2_BASE
}

sub FM_SPDP_BIST_GENERAL_CONFIG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SPDP_BIST_GENERAL_CONFIG_WIDTH            =  1; #
our $FM_SPDP_BIST_GENERAL_CONFIG_BITS             =  5; #

sub FM_SPDP_BIST_GENERAL_CONFIG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_SPDP_BIST_GENERAL_CONFIG_STRIDE           =  128; #


sub FM_SPDP_BIST_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x01405 + $FM_MGMT2_BASE); # 0x1D405 MGMT2_BASE
}

sub FM_SPDP_BIST_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SPDP_BIST_STATUS_WIDTH                    =  1; #
our $FM_SPDP_BIST_STATUS_BITS                     =  2; #

sub FM_SPDP_BIST_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_SPDP_BIST_STATUS_STRIDE                   =  128; #


sub FM_SPDP_BIST_USER_CHECKER_MASKS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x01406 + $FM_MGMT2_BASE); # 0x1D406 MGMT2_BASE
}

sub FM_SPDP_BIST_USER_CHECKER_MASKS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SPDP_BIST_USER_CHECKER_MASKS_WIDTH        =  1; #
our $FM_SPDP_BIST_USER_CHECKER_MASKS_BITS         =  14; #

sub FM_SPDP_BIST_USER_CHECKER_MASKS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_SPDP_BIST_USER_CHECKER_MASKS_STRIDE       =  128; #


sub FM_SPDP_BIST_IP {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x01407 + $FM_MGMT2_BASE); # 0x1D407 MGMT2_BASE
}

sub FM_SPDP_BIST_IP_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SPDP_BIST_IP_WIDTH                        =  1; #
our $FM_SPDP_BIST_IP_BITS                         =  4; #

sub FM_SPDP_BIST_IP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_SPDP_BIST_IP_STRIDE                       =  128; #


sub FM_SPDP_BIST_IM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x01408 + $FM_MGMT2_BASE); # 0x1D408 MGMT2_BASE
}

sub FM_SPDP_BIST_IM_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SPDP_BIST_IM_WIDTH                        =  1; #
our $FM_SPDP_BIST_IM_BITS                         =  4; #

sub FM_SPDP_BIST_IM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_SPDP_BIST_IM_STRIDE                       =  128; #


sub FM_SPDP_BIST_MAX_ADDR {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x01409 + $FM_MGMT2_BASE); # 0x1D409 MGMT2_BASE
}

sub FM_SPDP_BIST_MAX_ADDR_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SPDP_BIST_MAX_ADDR_WIDTH                  =  1; #
our $FM_SPDP_BIST_MAX_ADDR_BITS                   =  16; #

sub FM_SPDP_BIST_MAX_ADDR_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_SPDP_BIST_MAX_ADDR_STRIDE                 =  128; #


sub FM_SPDP_BIST_USER_OP {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x0140A + $FM_MGMT2_BASE); # 0x1D40A MGMT2_BASE
}

sub FM_SPDP_BIST_USER_OP_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SPDP_BIST_USER_OP_WIDTH                   =  1; #
our $FM_SPDP_BIST_USER_OP_BITS                    =  32; #

sub FM_SPDP_BIST_USER_OP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_SPDP_BIST_USER_OP_STRIDE                  =  128; #


sub FM_SPDP_BIST_START_SEQUENCE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x0140B + $FM_MGMT2_BASE); # 0x1D40B MGMT2_BASE
}

sub FM_SPDP_BIST_START_SEQUENCE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SPDP_BIST_START_SEQUENCE_WIDTH            =  1; #
our $FM_SPDP_BIST_START_SEQUENCE_BITS             =  2; #

sub FM_SPDP_BIST_START_SEQUENCE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_SPDP_BIST_START_SEQUENCE_STRIDE           =  128; #


sub FM_SPDP_BIST_SAVED_ADDRESS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x0140C + $FM_MGMT2_BASE); # 0x1D40C MGMT2_BASE
}

sub FM_SPDP_BIST_SAVED_ADDRESS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SPDP_BIST_SAVED_ADDRESS_WIDTH             =  1; #
our $FM_SPDP_BIST_SAVED_ADDRESS_BITS              =  17; #

sub FM_SPDP_BIST_SAVED_ADDRESS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_SPDP_BIST_SAVED_ADDRESS_STRIDE            =  128; #


sub FM_SPDP_BIST_DEBUG_STATUS_1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x0140D + $FM_MGMT2_BASE); # 0x1D40D MGMT2_BASE
}

sub FM_SPDP_BIST_DEBUG_STATUS_1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SPDP_BIST_DEBUG_STATUS_1_WIDTH            =  1; #
our $FM_SPDP_BIST_DEBUG_STATUS_1_BITS             =  18; #

sub FM_SPDP_BIST_DEBUG_STATUS_1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_SPDP_BIST_DEBUG_STATUS_1_STRIDE           =  128; #


sub FM_SPDP_BIST_DEBUG_STATUS_2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00080 * (($index) - 0) + 0x0140E + $FM_MGMT2_BASE); # 0x1D40E MGMT2_BASE
}

sub FM_SPDP_BIST_DEBUG_STATUS_2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SPDP_BIST_DEBUG_STATUS_2_WIDTH            =  1; #
our $FM_SPDP_BIST_DEBUG_STATUS_2_BITS             =  32; #

sub FM_SPDP_BIST_DEBUG_STATUS_2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_SPDP_BIST_DEBUG_STATUS_2_STRIDE           =  128; #


sub FM_SPDP_BIST_CHAIN_GENERAL_CONFIG {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00080 * (($index1) - 0) + 0x00020 * (($index0) - 0) + 0x01440 + $FM_MGMT2_BASE); # 0x1D440 MGMT2_BASE
}

sub FM_SPDP_BIST_CHAIN_GENERAL_CONFIG_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_SPDP_BIST_CHAIN_GENERAL_CONFIG_WIDTH      =  1; #
our $FM_SPDP_BIST_CHAIN_GENERAL_CONFIG_BITS       =  3; #

sub FM_SPDP_BIST_CHAIN_GENERAL_CONFIG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
sub FM_SPDP_BIST_CHAIN_GENERAL_CONFIG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_SPDP_BIST_CHAIN_GENERAL_CONFIG_STRIDE_0   =  32; #
our $FM_SPDP_BIST_CHAIN_GENERAL_CONFIG_STRIDE_1   =  128; #


sub FM_SPDP_BIST_CHAIN_LATENCY {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00080 * (($index1) - 0) + 0x00020 * (($index0) - 0) + 0x01441 + $FM_MGMT2_BASE); # 0x1D441 MGMT2_BASE
}

sub FM_SPDP_BIST_CHAIN_LATENCY_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000002;
}

our $FM_SPDP_BIST_CHAIN_LATENCY_WIDTH             =  1; #
our $FM_SPDP_BIST_CHAIN_LATENCY_BITS              =  5; #

sub FM_SPDP_BIST_CHAIN_LATENCY_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
sub FM_SPDP_BIST_CHAIN_LATENCY_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_SPDP_BIST_CHAIN_LATENCY_STRIDE_0          =  32; #
our $FM_SPDP_BIST_CHAIN_LATENCY_STRIDE_1          =  128; #


sub FM_SPDP_BIST_CHAIN_CDC {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00080 * (($index1) - 0) + 0x00020 * (($index0) - 0) + 0x01442 + $FM_MGMT2_BASE); # 0x1D442 MGMT2_BASE
}

sub FM_SPDP_BIST_CHAIN_CDC_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_SPDP_BIST_CHAIN_CDC_WIDTH                 =  1; #
our $FM_SPDP_BIST_CHAIN_CDC_BITS                  =  12; #

sub FM_SPDP_BIST_CHAIN_CDC_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
sub FM_SPDP_BIST_CHAIN_CDC_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 5;
}
our $FM_SPDP_BIST_CHAIN_CDC_STRIDE_0              =  32; #
our $FM_SPDP_BIST_CHAIN_CDC_STRIDE_1              =  128; #


sub FM_SRBM_REPAIR {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x01700 + $FM_MGMT2_BASE); # 0x1D700 MGMT2_BASE
}

sub FM_SRBM_REPAIR_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_SRBM_REPAIR_WIDTH                         =  1; #
our $FM_SRBM_REPAIR_BITS                          =  28; #

sub FM_SRBM_REPAIR_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}
our $FM_SRBM_REPAIR_STRIDE                        =  1; #


sub FM_SRBM_MARCH_SEQUENCE {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x01708 + $FM_MGMT2_BASE + ($word)); # 0x1D708 MGMT2_BASE
}

sub FM_SRBM_MARCH_SEQUENCE_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0xfca952b9;}
    if($word == 1) {return 0xa9fca99e;}
    if($word == 2) {return 0xb16529ed;}
    if($word == 3) {return 0x7430fdb9;}
}

our $FM_SRBM_MARCH_SEQUENCE_WIDTH                 =  4; #
our $FM_SRBM_MARCH_SEQUENCE_BITS                  =  128; #



sub FM_SRBM_GENERAL_CONFIG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0170C + $FM_MGMT2_BASE); # 0x1D70C MGMT2_BASE
}

sub FM_SRBM_GENERAL_CONFIG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000200;
}

our $FM_SRBM_GENERAL_CONFIG_WIDTH                 =  1; #
our $FM_SRBM_GENERAL_CONFIG_BITS                  =  10; #



sub FM_SRBM_STATUS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0170D + $FM_MGMT2_BASE); # 0x1D70D MGMT2_BASE
}

sub FM_SRBM_STATUS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SRBM_STATUS_WIDTH                         =  1; #
our $FM_SRBM_STATUS_BITS                          =  8; #



sub FM_SRBM_IP {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0170E + $FM_MGMT2_BASE); # 0x1D70E MGMT2_BASE
}

sub FM_SRBM_IP_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SRBM_IP_WIDTH                             =  1; #
our $FM_SRBM_IP_BITS                              =  4; #



sub FM_SRBM_IM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0170F + $FM_MGMT2_BASE); # 0x1D70F MGMT2_BASE
}

sub FM_SRBM_IM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SRBM_IM_WIDTH                             =  1; #
our $FM_SRBM_IM_BITS                              =  4; #



sub FM_SRBM_CLEAR_REPAIRS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01710 + $FM_MGMT2_BASE); # 0x1D710 MGMT2_BASE
}

sub FM_SRBM_CLEAR_REPAIRS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SRBM_CLEAR_REPAIRS_WIDTH                  =  1; #
our $FM_SRBM_CLEAR_REPAIRS_BITS                   =  1; #



sub FM_SRBM_ADD_REPAIR {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01711 + $FM_MGMT2_BASE); # 0x1D711 MGMT2_BASE
}

sub FM_SRBM_ADD_REPAIR_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SRBM_ADD_REPAIR_WIDTH                     =  1; #
our $FM_SRBM_ADD_REPAIR_BITS                      =  28; #



sub FM_SRBM_NEXT_NEW_REPAIR {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01712 + $FM_MGMT2_BASE); # 0x1D712 MGMT2_BASE
}

sub FM_SRBM_NEXT_NEW_REPAIR_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SRBM_NEXT_NEW_REPAIR_WIDTH                =  1; #
our $FM_SRBM_NEXT_NEW_REPAIR_BITS                 =  30; #



sub FM_SRBM_LAST_NR_DEFECT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x01713 + $FM_MGMT2_BASE); # 0x1D713 MGMT2_BASE
}

sub FM_SRBM_LAST_NR_DEFECT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_SRBM_LAST_NR_DEFECT_WIDTH                 =  1; #
our $FM_SRBM_LAST_NR_DEFECT_BITS                  =  29; #



sub FM_CRM_DATA {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x02000 + $FM_MGMT2_BASE); # 0x1E000 MGMT2_BASE
}

sub FM_CRM_DATA_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_CRM_DATA_WIDTH                            =  1; #
our $FM_CRM_DATA_BITS                             =  32; #

sub FM_CRM_DATA_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
sub FM_CRM_DATA_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2048;
}
our $FM_CRM_DATA_STRIDE_0                         =  1; #
our $FM_CRM_DATA_STRIDE_1                         =  2; #


sub FM_CRM_CTRL {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x03000 + $FM_MGMT2_BASE); # 0x1F000 MGMT2_BASE
}

sub FM_CRM_CTRL_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_CRM_CTRL_WIDTH                            =  1; #
our $FM_CRM_CTRL_BITS                             =  24; #



sub FM_CRM_STATUS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x03001 + $FM_MGMT2_BASE); # 0x1F001 MGMT2_BASE
}

sub FM_CRM_STATUS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_CRM_STATUS_WIDTH                          =  1; #
our $FM_CRM_STATUS_BITS                           =  7; #



sub FM_CRM_TIME {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x03002 + $FM_MGMT2_BASE); # 0x1F002 MGMT2_BASE
}

sub FM_CRM_TIME_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_CRM_TIME_WIDTH                            =  1; #
our $FM_CRM_TIME_BITS                             =  32; #



sub FM_CRM_IP {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x03004 + $FM_MGMT2_BASE + ($word)); # 0x1F004 MGMT2_BASE
}

sub FM_CRM_IP_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CRM_IP_WIDTH                              =  2; #
our $FM_CRM_IP_BITS                               =  64; #



sub FM_CRM_IM {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x03006 + $FM_MGMT2_BASE + ($word)); # 0x1F006 MGMT2_BASE
}

sub FM_CRM_IM_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0xffffffff;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CRM_IM_WIDTH                              =  2; #
our $FM_CRM_IM_BITS                               =  64; #



sub FM_CRM_COMMAND {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x03080 + $FM_MGMT2_BASE + ($word)); # 0x1F080 MGMT2_BASE
}

sub FM_CRM_COMMAND_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CRM_COMMAND_WIDTH                         =  2; #
our $FM_CRM_COMMAND_BITS                          =  34; #

sub FM_CRM_COMMAND_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}
our $FM_CRM_COMMAND_STRIDE                        =  2; #


sub FM_CRM_REGISTER {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x03100 + $FM_MGMT2_BASE + ($word)); # 0x1F100 MGMT2_BASE
}

sub FM_CRM_REGISTER_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CRM_REGISTER_WIDTH                        =  2; #
our $FM_CRM_REGISTER_BITS                         =  40; #

sub FM_CRM_REGISTER_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}
our $FM_CRM_REGISTER_STRIDE                       =  2; #


sub FM_CRM_PERIOD {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x03180 + $FM_MGMT2_BASE + ($word)); # 0x1F180 MGMT2_BASE
}

sub FM_CRM_PERIOD_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CRM_PERIOD_WIDTH                          =  2; #
our $FM_CRM_PERIOD_BITS                           =  64; #

sub FM_CRM_PERIOD_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}
our $FM_CRM_PERIOD_STRIDE                         =  2; #


sub FM_CRM_PARAM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x03200 + $FM_MGMT2_BASE); # 0x1F200 MGMT2_BASE
}

sub FM_CRM_PARAM_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CRM_PARAM_WIDTH                           =  1; #
our $FM_CRM_PARAM_BITS                            =  32; #

sub FM_CRM_PARAM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}
our $FM_CRM_PARAM_STRIDE                          =  1; #


sub FM_CM_PORT_TXMP_USAGE {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00010 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00000 + $FM_CMM_BASE); # 0x20000 CMM_BASE
}

sub FM_CM_PORT_TXMP_USAGE_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_CM_PORT_TXMP_USAGE_WIDTH                  =  1; #
our $FM_CM_PORT_TXMP_USAGE_BITS                   =  16; #

sub FM_CM_PORT_TXMP_USAGE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
sub FM_CM_PORT_TXMP_USAGE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 80;
}
our $FM_CM_PORT_TXMP_USAGE_STRIDE_0               =  1; #
our $FM_CM_PORT_TXMP_USAGE_STRIDE_1               =  16; #


sub FM_CM_PORT_TXMP_IP_WM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00010 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x00800 + $FM_CMM_BASE); # 0x20800 CMM_BASE
}

sub FM_CM_PORT_TXMP_IP_WM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0xffffffff;
}

our $FM_CM_PORT_TXMP_IP_WM_WIDTH                  =  1; #
our $FM_CM_PORT_TXMP_IP_WM_BITS                   =  32; #

sub FM_CM_PORT_TXMP_IP_WM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
sub FM_CM_PORT_TXMP_IP_WM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 80;
}
our $FM_CM_PORT_TXMP_IP_WM_STRIDE_0               =  1; #
our $FM_CM_PORT_TXMP_IP_WM_STRIDE_1               =  16; #


sub FM_CM_PORT_TXMP_SAMPLING_PERIOD {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00010 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x01000 + $FM_CMM_BASE); # 0x21000 CMM_BASE
}

sub FM_CM_PORT_TXMP_SAMPLING_PERIOD_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_CM_PORT_TXMP_SAMPLING_PERIOD_WIDTH        =  1; #
our $FM_CM_PORT_TXMP_SAMPLING_PERIOD_BITS         =  21; #

sub FM_CM_PORT_TXMP_SAMPLING_PERIOD_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
sub FM_CM_PORT_TXMP_SAMPLING_PERIOD_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 80;
}
our $FM_CM_PORT_TXMP_SAMPLING_PERIOD_STRIDE_0     =  1; #
our $FM_CM_PORT_TXMP_SAMPLING_PERIOD_STRIDE_1     =  16; #


sub FM_CM_PORT_TXMP_SAMPLING_STATE {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00010 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x01800 + $FM_CMM_BASE); # 0x21800 CMM_BASE
}

sub FM_CM_PORT_TXMP_SAMPLING_STATE_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_CM_PORT_TXMP_SAMPLING_STATE_WIDTH         =  1; #
our $FM_CM_PORT_TXMP_SAMPLING_STATE_BITS          =  32; #

sub FM_CM_PORT_TXMP_SAMPLING_STATE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
sub FM_CM_PORT_TXMP_SAMPLING_STATE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 80;
}
our $FM_CM_PORT_TXMP_SAMPLING_STATE_STRIDE_0      =  1; #
our $FM_CM_PORT_TXMP_SAMPLING_STATE_STRIDE_1      =  16; #


sub FM_CM_PORT_TXMP_ABOVE_IP {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x02000 + $FM_CMM_BASE + ($word)); # 0x22000 CMM_BASE
}

sub FM_CM_PORT_TXMP_ABOVE_IP_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_CM_PORT_TXMP_ABOVE_IP_WIDTH               =  3; #
our $FM_CM_PORT_TXMP_ABOVE_IP_BITS                =  80; #

sub FM_CM_PORT_TXMP_ABOVE_IP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
our $FM_CM_PORT_TXMP_ABOVE_IP_STRIDE              =  4; #


sub FM_CM_PORT_TXMP_BELOW_IP {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x02040 + $FM_CMM_BASE + ($word)); # 0x22040 CMM_BASE
}

sub FM_CM_PORT_TXMP_BELOW_IP_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_CM_PORT_TXMP_BELOW_IP_WIDTH               =  3; #
our $FM_CM_PORT_TXMP_BELOW_IP_BITS                =  80; #

sub FM_CM_PORT_TXMP_BELOW_IP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
our $FM_CM_PORT_TXMP_BELOW_IP_STRIDE              =  4; #


sub FM_CM_PORT_TXMP_ABOVE_IM {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x02080 + $FM_CMM_BASE + ($word)); # 0x22080 CMM_BASE
}

sub FM_CM_PORT_TXMP_ABOVE_IM_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0xffffffff;}
    if($word == 1) {return 0xffffffff;}
    if($word == 2) {return 0x0000ffff;}
}

our $FM_CM_PORT_TXMP_ABOVE_IM_WIDTH               =  3; #
our $FM_CM_PORT_TXMP_ABOVE_IM_BITS                =  80; #

sub FM_CM_PORT_TXMP_ABOVE_IM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
our $FM_CM_PORT_TXMP_ABOVE_IM_STRIDE              =  4; #


sub FM_CM_PORT_TXMP_BELOW_IM {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x020C0 + $FM_CMM_BASE + ($word)); # 0x220C0 CMM_BASE
}

sub FM_CM_PORT_TXMP_BELOW_IM_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0xffffffff;}
    if($word == 1) {return 0xffffffff;}
    if($word == 2) {return 0x0000ffff;}
}

our $FM_CM_PORT_TXMP_BELOW_IM_WIDTH               =  3; #
our $FM_CM_PORT_TXMP_BELOW_IM_BITS                =  80; #

sub FM_CM_PORT_TXMP_BELOW_IM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
our $FM_CM_PORT_TXMP_BELOW_IM_STRIDE              =  4; #


sub FM_CM_INTERRUPT_DETECT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x02100 + $FM_CMM_BASE); # 0x22100 CMM_BASE
}

sub FM_CM_INTERRUPT_DETECT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_CM_INTERRUPT_DETECT_WIDTH                 =  1; #
our $FM_CM_INTERRUPT_DETECT_BITS                  =  24; #



sub FM_FC_MRL_MGMT_CYCLES {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00000 + $FM_FC_BEM_BASE); # 0x28000 FC_BEM_BASE
}

sub FM_FC_MRL_MGMT_CYCLES_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00008208;
}

our $FM_FC_MRL_MGMT_CYCLES_WIDTH                  =  1; #
our $FM_FC_MRL_MGMT_CYCLES_BITS                   =  20; #

sub FM_FC_MRL_MGMT_CYCLES_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_FC_MRL_MGMT_CYCLES_STRIDE                 =  1; #


sub FM_FC_MRL_SWEEP_CYCLES {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00010 + $FM_FC_BEM_BASE); # 0x28010 FC_BEM_BASE
}

sub FM_FC_MRL_SWEEP_CYCLES_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000008;
}

our $FM_FC_MRL_SWEEP_CYCLES_WIDTH                 =  1; #
our $FM_FC_MRL_SWEEP_CYCLES_BITS                  =  6; #

sub FM_FC_MRL_SWEEP_CYCLES_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 3;
}
our $FM_FC_MRL_SWEEP_CYCLES_STRIDE                =  1; #


sub FM_FC_MRL_SWEEP_PERIOD {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00014 + $FM_FC_BEM_BASE); # 0x28014 FC_BEM_BASE
}

sub FM_FC_MRL_SWEEP_PERIOD_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000050;
}

our $FM_FC_MRL_SWEEP_PERIOD_WIDTH                 =  1; #
our $FM_FC_MRL_SWEEP_PERIOD_BITS                  =  8; #



sub FM_FC_MRL_UNROLL_ITER {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00015 + $FM_FC_BEM_BASE); # 0x28015 FC_BEM_BASE
}

sub FM_FC_MRL_UNROLL_ITER_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FC_MRL_UNROLL_ITER_WIDTH                  =  1; #
our $FM_FC_MRL_UNROLL_ITER_BITS                   =  12; #



sub FM_FC_MRL_TOKEN_LIMIT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00018 + $FM_FC_BEM_BASE); # 0x28018 FC_BEM_BASE
}

sub FM_FC_MRL_TOKEN_LIMIT_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000010;
}

our $FM_FC_MRL_TOKEN_LIMIT_WIDTH                  =  1; #
our $FM_FC_MRL_TOKEN_LIMIT_BITS                   =  18; #

sub FM_FC_MRL_TOKEN_LIMIT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 6;
}
our $FM_FC_MRL_TOKEN_LIMIT_STRIDE                 =  1; #


sub FM_FC_MRL_FC_TOKEN_LIMIT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00020 + $FM_FC_BEM_BASE); # 0x28020 FC_BEM_BASE
}

sub FM_FC_MRL_FC_TOKEN_LIMIT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x000003ff;
}

our $FM_FC_MRL_FC_TOKEN_LIMIT_WIDTH               =  1; #
our $FM_FC_MRL_FC_TOKEN_LIMIT_BITS                =  31; #



sub FM_FC_MRL_ALIGN_TX_STATS {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00021 + $FM_FC_BEM_BASE); # 0x28021 FC_BEM_BASE
}

sub FM_FC_MRL_ALIGN_TX_STATS_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FC_MRL_ALIGN_TX_STATS_WIDTH               =  1; #
our $FM_FC_MRL_ALIGN_TX_STATS_BITS                =  1; #



sub FM_FC_MRL_RATE_LIMITER {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00022 + $FM_FC_BEM_BASE); # 0x28022 FC_BEM_BASE
}

sub FM_FC_MRL_RATE_LIMITER_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000128;
}

our $FM_FC_MRL_RATE_LIMITER_WIDTH                 =  1; #
our $FM_FC_MRL_RATE_LIMITER_BITS                  =  11; #



sub FM_L2L_MAC_TABLE_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x00000 + $FM_L2L_BASE); # 0x30000 L2L_BASE
}

sub FM_L2L_MAC_TABLE_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00003000;
}

our $FM_L2L_MAC_TABLE_CFG_WIDTH                   =  1; #
our $FM_L2L_MAC_TABLE_CFG_BITS                    =  15; #



sub FM_L2L_CMD_PROFILE_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00010 + $FM_L2L_BASE); # 0x30010 L2L_BASE
}

sub FM_L2L_CMD_PROFILE_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00088000;
}

our $FM_L2L_CMD_PROFILE_TABLE_WIDTH               =  1; #
our $FM_L2L_CMD_PROFILE_TABLE_BITS                =  20; #

sub FM_L2L_CMD_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_L2L_CMD_PROFILE_TABLE_STRIDE              =  1; #


sub FM_L2L_LOCK_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00800 + $FM_L2L_BASE + ($word)); # 0x30800 L2L_BASE
}

sub FM_L2L_LOCK_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L2L_LOCK_TABLE_WIDTH                      =  2; #
our $FM_L2L_LOCK_TABLE_BITS                       =  64; #

sub FM_L2L_LOCK_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 1024;
}
our $FM_L2L_LOCK_TABLE_STRIDE                     =  2; #


sub FM_L2L_EVID1_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x02000 + $FM_L2L_BASE + ($word)); # 0x32000 L2L_BASE
}

sub FM_L2L_EVID1_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L2L_EVID1_TABLE_WIDTH                     =  2; #
our $FM_L2L_EVID1_TABLE_BITS                      =  33; #

sub FM_L2L_EVID1_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}
our $FM_L2L_EVID1_TABLE_STRIDE                    =  2; #


sub FM_L2L_IVID1_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x04000 + $FM_L2L_BASE + ($word)); # 0x34000 L2L_BASE
}

sub FM_L2L_IVID1_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L2L_IVID1_TABLE_WIDTH                     =  2; #
our $FM_L2L_IVID1_TABLE_BITS                      =  33; #

sub FM_L2L_IVID1_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}
our $FM_L2L_IVID1_TABLE_STRIDE                    =  2; #


sub FM_L2L_EVID2_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06000 + $FM_L2L_BASE); # 0x36000 L2L_BASE
}

sub FM_L2L_EVID2_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2L_EVID2_TABLE_WIDTH                     =  1; #
our $FM_L2L_EVID2_TABLE_BITS                      =  24; #

sub FM_L2L_EVID2_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}
our $FM_L2L_EVID2_TABLE_STRIDE                    =  1; #


sub FM_L2L_IVID2_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x07000 + $FM_L2L_BASE); # 0x37000 L2L_BASE
}

sub FM_L2L_IVID2_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2L_IVID2_TABLE_WIDTH                     =  1; #
our $FM_L2L_IVID2_TABLE_BITS                      =  24; #

sub FM_L2L_IVID2_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}
our $FM_L2L_IVID2_TABLE_STRIDE                    =  1; #


sub FM_SAF_MATRIX {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00000 + $FM_SAF_BASE + ($word)); # 0xA0000 SAF_BASE
}

sub FM_SAF_MATRIX_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_SAF_MATRIX_WIDTH                          =  3; #
our $FM_SAF_MATRIX_BITS                           =  82; #

sub FM_SAF_MATRIX_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_SAF_MATRIX_STRIDE                         =  4; #


sub FM_PORT_STATUS {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00000 + $FM_EPL_BASE); # 0xE0000 EPL_BASE
}

sub FM_PORT_STATUS_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_PORT_STATUS_WIDTH                         =  1; #
our $FM_PORT_STATUS_BITS                          =  12; #

sub FM_PORT_STATUS_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_PORT_STATUS_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_PORT_STATUS_STRIDE_0                      =  128; #
our $FM_PORT_STATUS_STRIDE_1                      =  1024; #


sub FM_AN_IM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00001 + $FM_EPL_BASE); # 0xE0001 EPL_BASE
}

sub FM_AN_IM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x0007ffff;
}

our $FM_AN_IM_WIDTH                               =  1; #
our $FM_AN_IM_BITS                                =  19; #

sub FM_AN_IM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_AN_IM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_AN_IM_STRIDE_0                            =  128; #
our $FM_AN_IM_STRIDE_1                            =  1024; #


sub FM_LINK_IM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00002 + $FM_EPL_BASE); # 0xE0002 EPL_BASE
}

sub FM_LINK_IM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x07ffffff;
}

our $FM_LINK_IM_WIDTH                             =  1; #
our $FM_LINK_IM_BITS                              =  27; #

sub FM_LINK_IM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_LINK_IM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_LINK_IM_STRIDE_0                          =  128; #
our $FM_LINK_IM_STRIDE_1                          =  1024; #


sub FM_AN_IP {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00003 + $FM_EPL_BASE); # 0xE0003 EPL_BASE
}

sub FM_AN_IP_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_AN_IP_WIDTH                               =  1; #
our $FM_AN_IP_BITS                                =  19; #

sub FM_AN_IP_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_AN_IP_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_AN_IP_STRIDE_0                            =  128; #
our $FM_AN_IP_STRIDE_1                            =  1024; #


sub FM_LINK_IP {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00004 + $FM_EPL_BASE); # 0xE0004 EPL_BASE
}

sub FM_LINK_IP_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_LINK_IP_WIDTH                             =  1; #
our $FM_LINK_IP_BITS                              =  27; #

sub FM_LINK_IP_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_LINK_IP_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_LINK_IP_STRIDE_0                          =  128; #
our $FM_LINK_IP_STRIDE_1                          =  1024; #


sub FM_SGMII_AN_TX_CONFIG_LOOPBACK {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00006 + $FM_EPL_BASE + ($word)); # 0xE0006 EPL_BASE
}

sub FM_SGMII_AN_TX_CONFIG_LOOPBACK_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_SGMII_AN_TX_CONFIG_LOOPBACK_WIDTH         =  2; #
our $FM_SGMII_AN_TX_CONFIG_LOOPBACK_BITS          =  16; #

sub FM_SGMII_AN_TX_CONFIG_LOOPBACK_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_SGMII_AN_TX_CONFIG_LOOPBACK_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_SGMII_AN_TX_CONFIG_LOOPBACK_STRIDE_0      =  128; #
our $FM_SGMII_AN_TX_CONFIG_LOOPBACK_STRIDE_1      =  1024; #


sub FM_SGMII_AN_RX_CONFIG {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00008 + $FM_EPL_BASE); # 0xE0008 EPL_BASE
}

sub FM_SGMII_AN_RX_CONFIG_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_SGMII_AN_RX_CONFIG_WIDTH                  =  1; #
our $FM_SGMII_AN_RX_CONFIG_BITS                   =  16; #

sub FM_SGMII_AN_RX_CONFIG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_SGMII_AN_RX_CONFIG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_SGMII_AN_RX_CONFIG_STRIDE_0               =  128; #
our $FM_SGMII_AN_RX_CONFIG_STRIDE_1               =  1024; #


sub FM_AN_73_NEXT_PAGE_RX {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x0000A + $FM_EPL_BASE + ($word)); # 0xE000A EPL_BASE
}

sub FM_AN_73_NEXT_PAGE_RX_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_AN_73_NEXT_PAGE_RX_WIDTH                  =  2; #
our $FM_AN_73_NEXT_PAGE_RX_BITS                   =  48; #

sub FM_AN_73_NEXT_PAGE_RX_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_AN_73_NEXT_PAGE_RX_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_AN_73_NEXT_PAGE_RX_STRIDE_0               =  128; #
our $FM_AN_73_NEXT_PAGE_RX_STRIDE_1               =  1024; #


sub FM_LINK_RULES {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x0000C + $FM_EPL_BASE); # 0xE000C EPL_BASE
}

sub FM_LINK_RULES_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_LINK_RULES_WIDTH                          =  1; #
our $FM_LINK_RULES_BITS                           =  22; #

sub FM_LINK_RULES_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_LINK_RULES_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_LINK_RULES_STRIDE_0                       =  128; #
our $FM_LINK_RULES_STRIDE_1                       =  1024; #


sub FM_MAC_CFG {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00010 + $FM_EPL_BASE + ($word)); # 0xE0010 EPL_BASE
}

sub FM_MAC_CFG_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x40000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00005381;}
}

our $FM_MAC_CFG_WIDTH                             =  4; #
our $FM_MAC_CFG_BITS                              =  126; #

sub FM_MAC_CFG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_MAC_CFG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_MAC_CFG_STRIDE_0                          =  128; #
our $FM_MAC_CFG_STRIDE_1                          =  1024; #


sub FM_TX_SEQUENCE {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00014 + $FM_EPL_BASE + ($word)); # 0xE0014 EPL_BASE
}

sub FM_TX_SEQUENCE_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_TX_SEQUENCE_WIDTH                         =  2; #
our $FM_TX_SEQUENCE_BITS                          =  36; #

sub FM_TX_SEQUENCE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_TX_SEQUENCE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_TX_SEQUENCE_STRIDE_0                      =  128; #
our $FM_TX_SEQUENCE_STRIDE_1                      =  1024; #


sub FM_RX_SEQUENCE {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00016 + $FM_EPL_BASE + ($word)); # 0xE0016 EPL_BASE
}

sub FM_RX_SEQUENCE_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_RX_SEQUENCE_WIDTH                         =  2; #
our $FM_RX_SEQUENCE_BITS                          =  36; #

sub FM_RX_SEQUENCE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_RX_SEQUENCE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_RX_SEQUENCE_STRIDE_0                      =  128; #
our $FM_RX_SEQUENCE_STRIDE_1                      =  1024; #


sub FM_MAC_1588_STATUS {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00018 + $FM_EPL_BASE + ($word)); # 0xE0018 EPL_BASE
}

sub FM_MAC_1588_STATUS_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_MAC_1588_STATUS_WIDTH                     =  2; #
our $FM_MAC_1588_STATUS_BITS                      =  63; #

sub FM_MAC_1588_STATUS_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_MAC_1588_STATUS_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_MAC_1588_STATUS_STRIDE_0                  =  128; #
our $FM_MAC_1588_STATUS_STRIDE_1                  =  1024; #


sub FM_MAC_OVERSIZE_COUNTER {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x0001A + $FM_EPL_BASE); # 0xE001A EPL_BASE
}

sub FM_MAC_OVERSIZE_COUNTER_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_MAC_OVERSIZE_COUNTER_WIDTH                =  1; #
our $FM_MAC_OVERSIZE_COUNTER_BITS                 =  32; #

sub FM_MAC_OVERSIZE_COUNTER_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_MAC_OVERSIZE_COUNTER_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_MAC_OVERSIZE_COUNTER_STRIDE_0             =  128; #
our $FM_MAC_OVERSIZE_COUNTER_STRIDE_1             =  1024; #


sub FM_MAC_JABBER_COUNTER {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x0001B + $FM_EPL_BASE); # 0xE001B EPL_BASE
}

sub FM_MAC_JABBER_COUNTER_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_MAC_JABBER_COUNTER_WIDTH                  =  1; #
our $FM_MAC_JABBER_COUNTER_BITS                   =  32; #

sub FM_MAC_JABBER_COUNTER_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_MAC_JABBER_COUNTER_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_MAC_JABBER_COUNTER_STRIDE_0               =  128; #
our $FM_MAC_JABBER_COUNTER_STRIDE_1               =  1024; #


sub FM_MAC_UNDERSIZE_COUNTER {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x0001C + $FM_EPL_BASE); # 0xE001C EPL_BASE
}

sub FM_MAC_UNDERSIZE_COUNTER_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_MAC_UNDERSIZE_COUNTER_WIDTH               =  1; #
our $FM_MAC_UNDERSIZE_COUNTER_BITS                =  32; #

sub FM_MAC_UNDERSIZE_COUNTER_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_MAC_UNDERSIZE_COUNTER_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_MAC_UNDERSIZE_COUNTER_STRIDE_0            =  128; #
our $FM_MAC_UNDERSIZE_COUNTER_STRIDE_1            =  1024; #


sub FM_MAC_RUNT_COUNTER {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x0001D + $FM_EPL_BASE); # 0xE001D EPL_BASE
}

sub FM_MAC_RUNT_COUNTER_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_MAC_RUNT_COUNTER_WIDTH                    =  1; #
our $FM_MAC_RUNT_COUNTER_BITS                     =  32; #

sub FM_MAC_RUNT_COUNTER_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_MAC_RUNT_COUNTER_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_MAC_RUNT_COUNTER_STRIDE_0                 =  128; #
our $FM_MAC_RUNT_COUNTER_STRIDE_1                 =  1024; #


sub FM_MAC_OVERRUN_COUNTER {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x0001E + $FM_EPL_BASE); # 0xE001E EPL_BASE
}

sub FM_MAC_OVERRUN_COUNTER_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_MAC_OVERRUN_COUNTER_WIDTH                 =  1; #
our $FM_MAC_OVERRUN_COUNTER_BITS                  =  32; #

sub FM_MAC_OVERRUN_COUNTER_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_MAC_OVERRUN_COUNTER_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_MAC_OVERRUN_COUNTER_STRIDE_0              =  128; #
our $FM_MAC_OVERRUN_COUNTER_STRIDE_1              =  1024; #


sub FM_MAC_UNDERRUN_COUNTER {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x0001F + $FM_EPL_BASE); # 0xE001F EPL_BASE
}

sub FM_MAC_UNDERRUN_COUNTER_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_MAC_UNDERRUN_COUNTER_WIDTH                =  1; #
our $FM_MAC_UNDERRUN_COUNTER_BITS                 =  32; #

sub FM_MAC_UNDERRUN_COUNTER_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_MAC_UNDERRUN_COUNTER_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_MAC_UNDERRUN_COUNTER_STRIDE_0             =  128; #
our $FM_MAC_UNDERRUN_COUNTER_STRIDE_1             =  1024; #


sub FM_MAC_CODE_ERROR_COUNTER {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00020 + $FM_EPL_BASE); # 0xE0020 EPL_BASE
}

sub FM_MAC_CODE_ERROR_COUNTER_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_MAC_CODE_ERROR_COUNTER_WIDTH              =  1; #
our $FM_MAC_CODE_ERROR_COUNTER_BITS               =  32; #

sub FM_MAC_CODE_ERROR_COUNTER_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_MAC_CODE_ERROR_COUNTER_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_MAC_CODE_ERROR_COUNTER_STRIDE_0           =  128; #
our $FM_MAC_CODE_ERROR_COUNTER_STRIDE_1           =  1024; #


sub FM_MAC_LINK_COUNTER {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00021 + $FM_EPL_BASE); # 0xE0021 EPL_BASE
}

sub FM_MAC_LINK_COUNTER_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_MAC_LINK_COUNTER_WIDTH                    =  1; #
our $FM_MAC_LINK_COUNTER_BITS                     =  32; #

sub FM_MAC_LINK_COUNTER_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_MAC_LINK_COUNTER_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_MAC_LINK_COUNTER_STRIDE_0                 =  128; #
our $FM_MAC_LINK_COUNTER_STRIDE_1                 =  1024; #


sub FM_PCS_1000BASEX_CFG {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00022 + $FM_EPL_BASE); # 0xE0022 EPL_BASE
}

sub FM_PCS_1000BASEX_CFG_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_PCS_1000BASEX_CFG_WIDTH                   =  1; #
our $FM_PCS_1000BASEX_CFG_BITS                    =  2; #

sub FM_PCS_1000BASEX_CFG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_PCS_1000BASEX_CFG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_PCS_1000BASEX_CFG_STRIDE_0                =  128; #
our $FM_PCS_1000BASEX_CFG_STRIDE_1                =  1024; #


sub FM_PCS_1000BASEX_RX_STATUS {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00023 + $FM_EPL_BASE); # 0xE0023 EPL_BASE
}

sub FM_PCS_1000BASEX_RX_STATUS_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_PCS_1000BASEX_RX_STATUS_WIDTH             =  1; #
our $FM_PCS_1000BASEX_RX_STATUS_BITS              =  7; #

sub FM_PCS_1000BASEX_RX_STATUS_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_PCS_1000BASEX_RX_STATUS_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_PCS_1000BASEX_RX_STATUS_STRIDE_0          =  128; #
our $FM_PCS_1000BASEX_RX_STATUS_STRIDE_1          =  1024; #


sub FM_PCS_1000BASEX_TX_STATUS {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00024 + $FM_EPL_BASE); # 0xE0024 EPL_BASE
}

sub FM_PCS_1000BASEX_TX_STATUS_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_PCS_1000BASEX_TX_STATUS_WIDTH             =  1; #
our $FM_PCS_1000BASEX_TX_STATUS_BITS              =  11; #

sub FM_PCS_1000BASEX_TX_STATUS_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_PCS_1000BASEX_TX_STATUS_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_PCS_1000BASEX_TX_STATUS_STRIDE_0          =  128; #
our $FM_PCS_1000BASEX_TX_STATUS_STRIDE_1          =  1024; #


sub FM_PCS_10GBASER_CFG {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00025 + $FM_EPL_BASE); # 0xE0025 EPL_BASE
}

sub FM_PCS_10GBASER_CFG_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_PCS_10GBASER_CFG_WIDTH                    =  1; #
our $FM_PCS_10GBASER_CFG_BITS                     =  4; #

sub FM_PCS_10GBASER_CFG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_PCS_10GBASER_CFG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_PCS_10GBASER_CFG_STRIDE_0                 =  128; #
our $FM_PCS_10GBASER_CFG_STRIDE_1                 =  1024; #


sub FM_PCS_10GBASER_RX_STATUS {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00026 + $FM_EPL_BASE); # 0xE0026 EPL_BASE
}

sub FM_PCS_10GBASER_RX_STATUS_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_PCS_10GBASER_RX_STATUS_WIDTH              =  1; #
our $FM_PCS_10GBASER_RX_STATUS_BITS               =  10; #

sub FM_PCS_10GBASER_RX_STATUS_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_PCS_10GBASER_RX_STATUS_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_PCS_10GBASER_RX_STATUS_STRIDE_0           =  128; #
our $FM_PCS_10GBASER_RX_STATUS_STRIDE_1           =  1024; #


sub FM_PCS_10GBASER_TX_STATUS {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00027 + $FM_EPL_BASE); # 0xE0027 EPL_BASE
}

sub FM_PCS_10GBASER_TX_STATUS_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_PCS_10GBASER_TX_STATUS_WIDTH              =  1; #
our $FM_PCS_10GBASER_TX_STATUS_BITS               =  3; #

sub FM_PCS_10GBASER_TX_STATUS_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_PCS_10GBASER_TX_STATUS_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_PCS_10GBASER_TX_STATUS_STRIDE_0           =  128; #
our $FM_PCS_10GBASER_TX_STATUS_STRIDE_1           =  1024; #


sub FM_AN_37_CFG {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00028 + $FM_EPL_BASE); # 0xE0028 EPL_BASE
}

sub FM_AN_37_CFG_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_AN_37_CFG_WIDTH                           =  1; #
our $FM_AN_37_CFG_BITS                            =  4; #

sub FM_AN_37_CFG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_AN_37_CFG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_AN_37_CFG_STRIDE_0                        =  128; #
our $FM_AN_37_CFG_STRIDE_1                        =  1024; #


sub FM_AN_37_STATUS {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x0002A + $FM_EPL_BASE); # 0xE002A EPL_BASE
}

sub FM_AN_37_STATUS_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_AN_37_STATUS_WIDTH                        =  1; #
our $FM_AN_37_STATUS_BITS                         =  28; #

sub FM_AN_37_STATUS_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_AN_37_STATUS_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_AN_37_STATUS_STRIDE_0                     =  128; #
our $FM_AN_37_STATUS_STRIDE_1                     =  1024; #


sub FM_AN_73_CFG {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x0002B + $FM_EPL_BASE); # 0xE002B EPL_BASE
}

sub FM_AN_73_CFG_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_AN_73_CFG_WIDTH                           =  1; #
our $FM_AN_73_CFG_BITS                            =  5; #

sub FM_AN_73_CFG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_AN_73_CFG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_AN_73_CFG_STRIDE_0                        =  128; #
our $FM_AN_73_CFG_STRIDE_1                        =  1024; #


sub FM_AN_73_TIMER_CFG {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x0002C + $FM_EPL_BASE); # 0xE002C EPL_BASE
}

sub FM_AN_73_TIMER_CFG_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_AN_73_TIMER_CFG_WIDTH                     =  1; #
our $FM_AN_73_TIMER_CFG_BITS                      =  20; #

sub FM_AN_73_TIMER_CFG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_AN_73_TIMER_CFG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_AN_73_TIMER_CFG_STRIDE_0                  =  128; #
our $FM_AN_73_TIMER_CFG_STRIDE_1                  =  1024; #


sub FM_AN_73_STATUS {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x0002E + $FM_EPL_BASE); # 0xE002E EPL_BASE
}

sub FM_AN_73_STATUS_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_AN_73_STATUS_WIDTH                        =  1; #
our $FM_AN_73_STATUS_BITS                         =  31; #

sub FM_AN_73_STATUS_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_AN_73_STATUS_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_AN_73_STATUS_STRIDE_0                     =  128; #
our $FM_AN_73_STATUS_STRIDE_1                     =  1024; #


sub FM_AN_73_TX_LCW_DEBUG {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00030 + $FM_EPL_BASE + ($word)); # 0xE0030 EPL_BASE
}

sub FM_AN_73_TX_LCW_DEBUG_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_AN_73_TX_LCW_DEBUG_WIDTH                  =  2; #
our $FM_AN_73_TX_LCW_DEBUG_BITS                   =  48; #

sub FM_AN_73_TX_LCW_DEBUG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_AN_73_TX_LCW_DEBUG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_AN_73_TX_LCW_DEBUG_STRIDE_0               =  128; #
our $FM_AN_73_TX_LCW_DEBUG_STRIDE_1               =  1024; #


sub FM_AN_73_RX_ABILITY_MATCH_LCW_DEBUG {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00032 + $FM_EPL_BASE + ($word)); # 0xE0032 EPL_BASE
}

sub FM_AN_73_RX_ABILITY_MATCH_LCW_DEBUG_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_AN_73_RX_ABILITY_MATCH_LCW_DEBUG_WIDTH    =  2; #
our $FM_AN_73_RX_ABILITY_MATCH_LCW_DEBUG_BITS     =  48; #

sub FM_AN_73_RX_ABILITY_MATCH_LCW_DEBUG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_AN_73_RX_ABILITY_MATCH_LCW_DEBUG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_AN_73_RX_ABILITY_MATCH_LCW_DEBUG_STRIDE_0 =  128; #
our $FM_AN_73_RX_ABILITY_MATCH_LCW_DEBUG_STRIDE_1 =  1024; #


sub FM_SERDES_CFG {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00034 + $FM_EPL_BASE + ($word)); # 0xE0034 EPL_BASE
}

sub FM_SERDES_CFG_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x0aaaa005;}
    if($word == 1) {return 0x00000000;}
}

our $FM_SERDES_CFG_WIDTH                          =  2; #
our $FM_SERDES_CFG_BITS                           =  34; #

sub FM_SERDES_CFG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_SERDES_CFG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_SERDES_CFG_STRIDE_0                       =  128; #
our $FM_SERDES_CFG_STRIDE_1                       =  1024; #


sub FM_DISPARITY_ERROR_8B10B {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00036 + $FM_EPL_BASE); # 0xE0036 EPL_BASE
}

sub FM_DISPARITY_ERROR_8B10B_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_DISPARITY_ERROR_8B10B_WIDTH               =  1; #
our $FM_DISPARITY_ERROR_8B10B_BITS                =  32; #

sub FM_DISPARITY_ERROR_8B10B_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_DISPARITY_ERROR_8B10B_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_DISPARITY_ERROR_8B10B_STRIDE_0            =  128; #
our $FM_DISPARITY_ERROR_8B10B_STRIDE_1            =  1024; #


sub FM_LANE_CFG {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00037 + $FM_EPL_BASE); # 0xE0037 EPL_BASE
}

sub FM_LANE_CFG_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000001;
}

our $FM_LANE_CFG_WIDTH                            =  1; #
our $FM_LANE_CFG_BITS                             =  26; #

sub FM_LANE_CFG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_LANE_CFG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_LANE_CFG_STRIDE_0                         =  128; #
our $FM_LANE_CFG_STRIDE_1                         =  1024; #


sub FM_LANE_STATUS {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00038 + $FM_EPL_BASE); # 0xE0038 EPL_BASE
}

sub FM_LANE_STATUS_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_LANE_STATUS_WIDTH                         =  1; #
our $FM_LANE_STATUS_BITS                          =  17; #

sub FM_LANE_STATUS_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_LANE_STATUS_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_LANE_STATUS_STRIDE_0                      =  128; #
our $FM_LANE_STATUS_STRIDE_1                      =  1024; #


sub FM_SERDES_RX_CFG {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00039 + $FM_EPL_BASE); # 0xE0039 EPL_BASE
}

sub FM_SERDES_RX_CFG_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_SERDES_RX_CFG_WIDTH                       =  1; #
our $FM_SERDES_RX_CFG_BITS                        =  32; #

sub FM_SERDES_RX_CFG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_SERDES_RX_CFG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_SERDES_RX_CFG_STRIDE_0                    =  128; #
our $FM_SERDES_RX_CFG_STRIDE_1                    =  1024; #


sub FM_SERDES_TX_CFG {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x0003A + $FM_EPL_BASE + ($word)); # 0xE003A EPL_BASE
}

sub FM_SERDES_TX_CFG_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_SERDES_TX_CFG_WIDTH                       =  2; #
our $FM_SERDES_TX_CFG_BITS                        =  44; #

sub FM_SERDES_TX_CFG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_SERDES_TX_CFG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_SERDES_TX_CFG_STRIDE_0                    =  128; #
our $FM_SERDES_TX_CFG_STRIDE_1                    =  1024; #


sub FM_SERDES_SIGNAL_DETECT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x0003C + $FM_EPL_BASE); # 0xE003C EPL_BASE
}

sub FM_SERDES_SIGNAL_DETECT_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_SERDES_SIGNAL_DETECT_WIDTH                =  1; #
our $FM_SERDES_SIGNAL_DETECT_BITS                 =  20; #

sub FM_SERDES_SIGNAL_DETECT_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_SERDES_SIGNAL_DETECT_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_SERDES_SIGNAL_DETECT_STRIDE_0             =  128; #
our $FM_SERDES_SIGNAL_DETECT_STRIDE_1             =  1024; #


sub FM_SERDES_STATUS {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x0003E + $FM_EPL_BASE + ($word)); # 0xE003E EPL_BASE
}

sub FM_SERDES_STATUS_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_SERDES_STATUS_WIDTH                       =  2; #
our $FM_SERDES_STATUS_BITS                        =  43; #

sub FM_SERDES_STATUS_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_SERDES_STATUS_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_SERDES_STATUS_STRIDE_0                    =  128; #
our $FM_SERDES_STATUS_STRIDE_1                    =  1024; #


sub FM_SERDES_IM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00040 + $FM_EPL_BASE); # 0xE0040 EPL_BASE
}

sub FM_SERDES_IM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00003fff;
}

our $FM_SERDES_IM_WIDTH                           =  1; #
our $FM_SERDES_IM_BITS                            =  14; #

sub FM_SERDES_IM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_SERDES_IM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_SERDES_IM_STRIDE_0                        =  128; #
our $FM_SERDES_IM_STRIDE_1                        =  1024; #


sub FM_SERDES_IP {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00041 + $FM_EPL_BASE); # 0xE0041 EPL_BASE
}

sub FM_SERDES_IP_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_SERDES_IP_WIDTH                           =  1; #
our $FM_SERDES_IP_BITS                            =  14; #

sub FM_SERDES_IP_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_SERDES_IP_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_SERDES_IP_STRIDE_0                        =  128; #
our $FM_SERDES_IP_STRIDE_1                        =  1024; #


sub FM_LANE_DEBUG {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00400 * (($index1) - 0) + 0x00080 * (($index0) - 0) + 0x00042 + $FM_EPL_BASE); # 0xE0042 EPL_BASE
}

sub FM_LANE_DEBUG_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_LANE_DEBUG_WIDTH                          =  1; #
our $FM_LANE_DEBUG_BITS                           =  10; #

sub FM_LANE_DEBUG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
sub FM_LANE_DEBUG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_LANE_DEBUG_STRIDE_0                       =  128; #
our $FM_LANE_DEBUG_STRIDE_1                       =  1024; #


sub FM_EPL_IP {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00300 + $FM_EPL_BASE); # 0xE0300 EPL_BASE
}

sub FM_EPL_IP_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_EPL_IP_WIDTH                              =  1; #
our $FM_EPL_IP_BITS                               =  12; #

sub FM_EPL_IP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_EPL_IP_STRIDE                             =  1024; #


sub FM_EPL_CFG_A {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00301 + $FM_EPL_BASE); # 0xE0301 EPL_BASE
}

sub FM_EPL_CFG_A_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x0c7d7899;
}

our $FM_EPL_CFG_A_WIDTH                           =  1; #
our $FM_EPL_CFG_A_BITS                            =  31; #

sub FM_EPL_CFG_A_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_EPL_CFG_A_STRIDE                          =  1024; #


sub FM_EPL_CFG_B {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00302 + $FM_EPL_BASE); # 0xE0302 EPL_BASE
}

sub FM_EPL_CFG_B_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00080000;
}

our $FM_EPL_CFG_B_WIDTH                           =  1; #
our $FM_EPL_CFG_B_BITS                            =  24; #

sub FM_EPL_CFG_B_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_EPL_CFG_B_STRIDE                          =  1024; #


sub FM_EPL_LED_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00303 + $FM_EPL_BASE); # 0xE0303 EPL_BASE
}

sub FM_EPL_LED_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_EPL_LED_STATUS_WIDTH                      =  1; #
our $FM_EPL_LED_STATUS_BITS                       =  24; #

sub FM_EPL_LED_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_EPL_LED_STATUS_STRIDE                     =  1024; #


sub FM_EPL_FIFO_ERROR_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00304 + $FM_EPL_BASE); # 0xE0304 EPL_BASE
}

sub FM_EPL_FIFO_ERROR_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_EPL_FIFO_ERROR_STATUS_WIDTH               =  1; #
our $FM_EPL_FIFO_ERROR_STATUS_BITS                =  8; #

sub FM_EPL_FIFO_ERROR_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_EPL_FIFO_ERROR_STATUS_STRIDE              =  1024; #


sub FM_EPL_TX_FIFO_READ_PTR_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00305 + $FM_EPL_BASE); # 0xE0305 EPL_BASE
}

sub FM_EPL_TX_FIFO_READ_PTR_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_EPL_TX_FIFO_READ_PTR_STATUS_WIDTH         =  1; #
our $FM_EPL_TX_FIFO_READ_PTR_STATUS_BITS          =  16; #

sub FM_EPL_TX_FIFO_READ_PTR_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_EPL_TX_FIFO_READ_PTR_STATUS_STRIDE        =  1024; #


sub FM_EPL_TX_FIFO_WRITE_PTR_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00306 + $FM_EPL_BASE); # 0xE0306 EPL_BASE
}

sub FM_EPL_TX_FIFO_WRITE_PTR_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_EPL_TX_FIFO_WRITE_PTR_STATUS_WIDTH        =  1; #
our $FM_EPL_TX_FIFO_WRITE_PTR_STATUS_BITS         =  16; #

sub FM_EPL_TX_FIFO_WRITE_PTR_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_EPL_TX_FIFO_WRITE_PTR_STATUS_STRIDE       =  1024; #


sub FM_EPL_TX_FIFO_A_PTR_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00307 + $FM_EPL_BASE); # 0xE0307 EPL_BASE
}

sub FM_EPL_TX_FIFO_A_PTR_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_EPL_TX_FIFO_A_PTR_STATUS_WIDTH            =  1; #
our $FM_EPL_TX_FIFO_A_PTR_STATUS_BITS             =  8; #

sub FM_EPL_TX_FIFO_A_PTR_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_EPL_TX_FIFO_A_PTR_STATUS_STRIDE           =  1024; #


sub FM_EPL_TX_FIFO_B_PTR_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00308 + $FM_EPL_BASE); # 0xE0308 EPL_BASE
}

sub FM_EPL_TX_FIFO_B_PTR_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_EPL_TX_FIFO_B_PTR_STATUS_WIDTH            =  1; #
our $FM_EPL_TX_FIFO_B_PTR_STATUS_BITS             =  8; #

sub FM_EPL_TX_FIFO_B_PTR_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_EPL_TX_FIFO_B_PTR_STATUS_STRIDE           =  1024; #


sub FM_EPL_TX_FIFO_C_PTR_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00309 + $FM_EPL_BASE); # 0xE0309 EPL_BASE
}

sub FM_EPL_TX_FIFO_C_PTR_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_EPL_TX_FIFO_C_PTR_STATUS_WIDTH            =  1; #
our $FM_EPL_TX_FIFO_C_PTR_STATUS_BITS             =  8; #

sub FM_EPL_TX_FIFO_C_PTR_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_EPL_TX_FIFO_C_PTR_STATUS_STRIDE           =  1024; #


sub FM_EPL_TX_FIFO_D_PTR_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0030A + $FM_EPL_BASE); # 0xE030A EPL_BASE
}

sub FM_EPL_TX_FIFO_D_PTR_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_EPL_TX_FIFO_D_PTR_STATUS_WIDTH            =  1; #
our $FM_EPL_TX_FIFO_D_PTR_STATUS_BITS             =  8; #

sub FM_EPL_TX_FIFO_D_PTR_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_EPL_TX_FIFO_D_PTR_STATUS_STRIDE           =  1024; #


sub FM_EPL_RX_FIFO_READ_PTR_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0030B + $FM_EPL_BASE); # 0xE030B EPL_BASE
}

sub FM_EPL_RX_FIFO_READ_PTR_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_EPL_RX_FIFO_READ_PTR_STATUS_WIDTH         =  1; #
our $FM_EPL_RX_FIFO_READ_PTR_STATUS_BITS          =  24; #

sub FM_EPL_RX_FIFO_READ_PTR_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_EPL_RX_FIFO_READ_PTR_STATUS_STRIDE        =  1024; #


sub FM_EPL_RX_FIFO_WRITE_PTR_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0030C + $FM_EPL_BASE); # 0xE030C EPL_BASE
}

sub FM_EPL_RX_FIFO_WRITE_PTR_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_EPL_RX_FIFO_WRITE_PTR_STATUS_WIDTH        =  1; #
our $FM_EPL_RX_FIFO_WRITE_PTR_STATUS_BITS         =  24; #

sub FM_EPL_RX_FIFO_WRITE_PTR_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_EPL_RX_FIFO_WRITE_PTR_STATUS_STRIDE       =  1024; #


sub FM_EPL_RX_FIFO_A_PTR_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0030D + $FM_EPL_BASE); # 0xE030D EPL_BASE
}

sub FM_EPL_RX_FIFO_A_PTR_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_EPL_RX_FIFO_A_PTR_STATUS_WIDTH            =  1; #
our $FM_EPL_RX_FIFO_A_PTR_STATUS_BITS             =  12; #

sub FM_EPL_RX_FIFO_A_PTR_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_EPL_RX_FIFO_A_PTR_STATUS_STRIDE           =  1024; #


sub FM_EPL_RX_FIFO_B_PTR_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0030E + $FM_EPL_BASE); # 0xE030E EPL_BASE
}

sub FM_EPL_RX_FIFO_B_PTR_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_EPL_RX_FIFO_B_PTR_STATUS_WIDTH            =  1; #
our $FM_EPL_RX_FIFO_B_PTR_STATUS_BITS             =  12; #

sub FM_EPL_RX_FIFO_B_PTR_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_EPL_RX_FIFO_B_PTR_STATUS_STRIDE           =  1024; #


sub FM_EPL_RX_FIFO_C_PTR_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x0030F + $FM_EPL_BASE); # 0xE030F EPL_BASE
}

sub FM_EPL_RX_FIFO_C_PTR_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_EPL_RX_FIFO_C_PTR_STATUS_WIDTH            =  1; #
our $FM_EPL_RX_FIFO_C_PTR_STATUS_BITS             =  12; #

sub FM_EPL_RX_FIFO_C_PTR_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_EPL_RX_FIFO_C_PTR_STATUS_STRIDE           =  1024; #


sub FM_EPL_RX_FIFO_D_PTR_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00310 + $FM_EPL_BASE); # 0xE0310 EPL_BASE
}

sub FM_EPL_RX_FIFO_D_PTR_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_EPL_RX_FIFO_D_PTR_STATUS_WIDTH            =  1; #
our $FM_EPL_RX_FIFO_D_PTR_STATUS_BITS             =  12; #

sub FM_EPL_RX_FIFO_D_PTR_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_EPL_RX_FIFO_D_PTR_STATUS_STRIDE           =  1024; #


sub FM_PCS_10GBASEX_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00311 + $FM_EPL_BASE); # 0xE0311 EPL_BASE
}

sub FM_PCS_10GBASEX_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_PCS_10GBASEX_CFG_WIDTH                    =  1; #
our $FM_PCS_10GBASEX_CFG_BITS                     =  5; #

sub FM_PCS_10GBASEX_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_PCS_10GBASEX_CFG_STRIDE                   =  1024; #


sub FM_PCS_10GBASEX_RX_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00312 + $FM_EPL_BASE); # 0xE0312 EPL_BASE
}

sub FM_PCS_10GBASEX_RX_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_PCS_10GBASEX_RX_STATUS_WIDTH              =  1; #
our $FM_PCS_10GBASEX_RX_STATUS_BITS               =  7; #

sub FM_PCS_10GBASEX_RX_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_PCS_10GBASEX_RX_STATUS_STRIDE             =  1024; #


sub FM_PCS_10GBASEX_TX_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00313 + $FM_EPL_BASE); # 0xE0313 EPL_BASE
}

sub FM_PCS_10GBASEX_TX_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_PCS_10GBASEX_TX_STATUS_WIDTH              =  1; #
our $FM_PCS_10GBASEX_TX_STATUS_BITS               =  14; #

sub FM_PCS_10GBASEX_TX_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_PCS_10GBASEX_TX_STATUS_STRIDE             =  1024; #


sub FM_PCS_40GBASER_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00314 + $FM_EPL_BASE); # 0xE0314 EPL_BASE
}

sub FM_PCS_40GBASER_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x000001ff;
}

our $FM_PCS_40GBASER_CFG_WIDTH                    =  1; #
our $FM_PCS_40GBASER_CFG_BITS                     =  30; #

sub FM_PCS_40GBASER_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_PCS_40GBASER_CFG_STRIDE                   =  1024; #


sub FM_PCS_40GBASER_RX_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00315 + $FM_EPL_BASE); # 0xE0315 EPL_BASE
}

sub FM_PCS_40GBASER_RX_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_PCS_40GBASER_RX_STATUS_WIDTH              =  1; #
our $FM_PCS_40GBASER_RX_STATUS_BITS               =  30; #

sub FM_PCS_40GBASER_RX_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_PCS_40GBASER_RX_STATUS_STRIDE             =  1024; #


sub FM_EPL_1588_TIMER_STATUS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00400 * (($index) - 0) + 0x00316 + $FM_EPL_BASE); # 0xE0316 EPL_BASE
}

sub FM_EPL_1588_TIMER_STATUS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_EPL_1588_TIMER_STATUS_WIDTH               =  1; #
our $FM_EPL_1588_TIMER_STATUS_BITS                =  31; #

sub FM_EPL_1588_TIMER_STATUS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 25;
}
our $FM_EPL_1588_TIMER_STATUS_STRIDE              =  1024; #


sub FM_PARSER_CAM {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x00000 + $FM_PARSER_BASE + ($word)); # 0x100000 PARSER_BASE
}

sub FM_PARSER_CAM_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_PARSER_CAM_WIDTH                          =  4; #
our $FM_PARSER_CAM_BITS                           =  128; #

sub FM_PARSER_CAM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}
sub FM_PARSER_CAM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 28;
}
our $FM_PARSER_CAM_STRIDE_0                       =  4; #
our $FM_PARSER_CAM_STRIDE_1                       =  1024; #


sub FM_PARSER_RAM {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x00200 + $FM_PARSER_BASE + ($word)); # 0x100200 PARSER_BASE
}

sub FM_PARSER_RAM_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_PARSER_RAM_WIDTH                          =  4; #
our $FM_PARSER_RAM_BITS                           =  110; #

sub FM_PARSER_RAM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}
sub FM_PARSER_RAM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 28;
}
our $FM_PARSER_RAM_STRIDE_0                       =  4; #
our $FM_PARSER_RAM_STRIDE_1                       =  1024; #


sub FM_PARSER_INIT_STATE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x08000 + $FM_PARSER_BASE + ($word)); # 0x108000 PARSER_BASE
}

sub FM_PARSER_INIT_STATE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_PARSER_INIT_STATE_WIDTH                   =  2; #
our $FM_PARSER_INIT_STATE_BITS                    =  34; #

sub FM_PARSER_INIT_STATE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_PARSER_INIT_STATE_STRIDE                  =  2; #


sub FM_PARSER_INIT_FIELDS {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00004 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x08200 + $FM_PARSER_BASE + ($word)); # 0x108200 PARSER_BASE
}

sub FM_PARSER_INIT_FIELDS_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_PARSER_INIT_FIELDS_WIDTH                  =  2; #
our $FM_PARSER_INIT_FIELDS_BITS                   =  64; #

sub FM_PARSER_INIT_FIELDS_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
sub FM_PARSER_INIT_FIELDS_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_PARSER_INIT_FIELDS_STRIDE_0               =  2; #
our $FM_PARSER_INIT_FIELDS_STRIDE_1               =  4; #


sub FM_CM_RXMP_MAP {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00000 + $FM_CM_BASE + ($word)); # 0x110000 CM_BASE
}

sub FM_CM_RXMP_MAP_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CM_RXMP_MAP_WIDTH                         =  2; #
our $FM_CM_RXMP_MAP_BITS                          =  64; #



sub FM_CM_TXMP_MAP {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00002 + $FM_CM_BASE + ($word)); # 0x110002 CM_BASE
}

sub FM_CM_TXMP_MAP_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CM_TXMP_MAP_WIDTH                         =  2; #
our $FM_CM_TXMP_MAP_BITS                          =  64; #



sub FM_CM_TC_MAP {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00004 + $FM_CM_BASE + ($word)); # 0x110004 CM_BASE
}

sub FM_CM_TC_MAP_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CM_TC_MAP_WIDTH                           =  2; #
our $FM_CM_TC_MAP_BITS                            =  64; #



sub FM_CM_BSG_MAP {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00100 + $FM_CM_BASE + ($word)); # 0x110100 CM_BASE
}

sub FM_CM_BSG_MAP_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CM_BSG_MAP_WIDTH                          =  2; #
our $FM_CM_BSG_MAP_BITS                           =  48; #

sub FM_CM_BSG_MAP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_CM_BSG_MAP_STRIDE                         =  2; #


sub FM_CM_GLOBAL_USAGE {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00200 + $FM_CM_BASE + ($word)); # 0x110200 CM_BASE
}

sub FM_CM_GLOBAL_USAGE_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CM_GLOBAL_USAGE_WIDTH                     =  2; #
our $FM_CM_GLOBAL_USAGE_BITS                      =  34; #



sub FM_CM_RXMP_USAGE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00210 + $FM_CM_BASE); # 0x110210 CM_BASE
}

sub FM_CM_RXMP_USAGE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CM_RXMP_USAGE_WIDTH                       =  1; #
our $FM_CM_RXMP_USAGE_BITS                        =  16; #

sub FM_CM_RXMP_USAGE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
our $FM_CM_RXMP_USAGE_STRIDE                      =  1; #


sub FM_CM_SHARED_RXMP_USAGE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00220 + $FM_CM_BASE + ($word)); # 0x110220 CM_BASE
}

sub FM_CM_SHARED_RXMP_USAGE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CM_SHARED_RXMP_USAGE_WIDTH                =  2; #
our $FM_CM_SHARED_RXMP_USAGE_BITS                 =  34; #

sub FM_CM_SHARED_RXMP_USAGE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
our $FM_CM_SHARED_RXMP_USAGE_STRIDE               =  2; #


sub FM_CM_PORT_RXMP_USAGE {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00020 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x01000 + $FM_CM_BASE + ($word)); # 0x111000 CM_BASE
}

sub FM_CM_PORT_RXMP_USAGE_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CM_PORT_RXMP_USAGE_WIDTH                  =  2; #
our $FM_CM_PORT_RXMP_USAGE_BITS                   =  35; #

sub FM_CM_PORT_RXMP_USAGE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
sub FM_CM_PORT_RXMP_USAGE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_CM_PORT_RXMP_USAGE_STRIDE_0               =  2; #
our $FM_CM_PORT_RXMP_USAGE_STRIDE_1               =  32; #


sub FM_CM_GLOBAL_TXMP_USAGE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x02000 + $FM_CM_BASE); # 0x112000 CM_BASE
}

sub FM_CM_GLOBAL_TXMP_USAGE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CM_GLOBAL_TXMP_USAGE_WIDTH                =  1; #
our $FM_CM_GLOBAL_TXMP_USAGE_BITS                 =  16; #

sub FM_CM_GLOBAL_TXMP_USAGE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
our $FM_CM_GLOBAL_TXMP_USAGE_STRIDE               =  1; #


sub FM_CM_PORT_TX_DROP_COUNT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x02100 + $FM_CM_BASE + ($word)); # 0x112100 CM_BASE
}

sub FM_CM_PORT_TX_DROP_COUNT_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CM_PORT_TX_DROP_COUNT_WIDTH               =  2; #
our $FM_CM_PORT_TX_DROP_COUNT_BITS                =  64; #

sub FM_CM_PORT_TX_DROP_COUNT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 80;
}
our $FM_CM_PORT_TX_DROP_COUNT_STRIDE              =  2; #


sub FM_CM_GLOBAL_WM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x02200 + $FM_CM_BASE); # 0x112200 CM_BASE
}

sub FM_CM_GLOBAL_WM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x3e00bcc0;
}

our $FM_CM_GLOBAL_WM_WIDTH                        =  1; #
our $FM_CM_GLOBAL_WM_BITS                         =  32; #



sub FM_CM_SHARED_RXMP_WM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x02210 + $FM_CM_BASE); # 0x112210 CM_BASE
}

sub FM_CM_SHARED_RXMP_WM_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0xffffffff;
}

our $FM_CM_SHARED_RXMP_WM_WIDTH                   =  1; #
our $FM_CM_SHARED_RXMP_WM_BITS                    =  32; #

sub FM_CM_SHARED_RXMP_WM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_CM_SHARED_RXMP_WM_STRIDE                  =  1; #


sub FM_CM_PORT_RXMP_PRIVATE_WM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00010 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x02800 + $FM_CM_BASE); # 0x112800 CM_BASE
}

sub FM_CM_PORT_RXMP_PRIVATE_WM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0xffffffff;
}

our $FM_CM_PORT_RXMP_PRIVATE_WM_WIDTH             =  1; #
our $FM_CM_PORT_RXMP_PRIVATE_WM_BITS              =  32; #

sub FM_CM_PORT_RXMP_PRIVATE_WM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
sub FM_CM_PORT_RXMP_PRIVATE_WM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_CM_PORT_RXMP_PRIVATE_WM_STRIDE_0          =  1; #
our $FM_CM_PORT_RXMP_PRIVATE_WM_STRIDE_1          =  16; #


sub FM_CM_PORT_RXMP_HOG_WM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00010 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x03000 + $FM_CM_BASE); # 0x113000 CM_BASE
}

sub FM_CM_PORT_RXMP_HOG_WM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0xffffffff;
}

our $FM_CM_PORT_RXMP_HOG_WM_WIDTH                 =  1; #
our $FM_CM_PORT_RXMP_HOG_WM_BITS                  =  32; #

sub FM_CM_PORT_RXMP_HOG_WM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
sub FM_CM_PORT_RXMP_HOG_WM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_CM_PORT_RXMP_HOG_WM_STRIDE_0              =  1; #
our $FM_CM_PORT_RXMP_HOG_WM_STRIDE_1              =  16; #


sub FM_CM_PORT_TXMP_PRIVATE_WM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00010 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x03800 + $FM_CM_BASE); # 0x113800 CM_BASE
}

sub FM_CM_PORT_TXMP_PRIVATE_WM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00003fff;
}

our $FM_CM_PORT_TXMP_PRIVATE_WM_WIDTH             =  1; #
our $FM_CM_PORT_TXMP_PRIVATE_WM_BITS              =  16; #

sub FM_CM_PORT_TXMP_PRIVATE_WM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
sub FM_CM_PORT_TXMP_PRIVATE_WM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 80;
}
our $FM_CM_PORT_TXMP_PRIVATE_WM_STRIDE_0          =  1; #
our $FM_CM_PORT_TXMP_PRIVATE_WM_STRIDE_1          =  16; #


sub FM_CM_PORT_TXMP_HOG_WM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00010 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x04000 + $FM_CM_BASE); # 0x114000 CM_BASE
}

sub FM_CM_PORT_TXMP_HOG_WM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00003fff;
}

our $FM_CM_PORT_TXMP_HOG_WM_WIDTH                 =  1; #
our $FM_CM_PORT_TXMP_HOG_WM_BITS                  =  14; #

sub FM_CM_PORT_TXMP_HOG_WM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
sub FM_CM_PORT_TXMP_HOG_WM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 80;
}
our $FM_CM_PORT_TXMP_HOG_WM_STRIDE_0              =  1; #
our $FM_CM_PORT_TXMP_HOG_WM_STRIDE_1              =  16; #


sub FM_CM_RXMP_SOFT_DROP_WM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x04800 + $FM_CM_BASE); # 0x114800 CM_BASE
}

sub FM_CM_RXMP_SOFT_DROP_WM_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x7ffe3fff;
}

our $FM_CM_RXMP_SOFT_DROP_WM_WIDTH                =  1; #
our $FM_CM_RXMP_SOFT_DROP_WM_BITS                 =  31; #

sub FM_CM_RXMP_SOFT_DROP_WM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_CM_RXMP_SOFT_DROP_WM_STRIDE               =  1; #


sub FM_CM_PORT_RXMP_PAUSE_ON_WM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00010 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x05000 + $FM_CM_BASE); # 0x115000 CM_BASE
}

sub FM_CM_PORT_RXMP_PAUSE_ON_WM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0xffffffff;
}

our $FM_CM_PORT_RXMP_PAUSE_ON_WM_WIDTH            =  1; #
our $FM_CM_PORT_RXMP_PAUSE_ON_WM_BITS             =  32; #

sub FM_CM_PORT_RXMP_PAUSE_ON_WM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
sub FM_CM_PORT_RXMP_PAUSE_ON_WM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_CM_PORT_RXMP_PAUSE_ON_WM_STRIDE_0         =  1; #
our $FM_CM_PORT_RXMP_PAUSE_ON_WM_STRIDE_1         =  16; #


sub FM_CM_PORT_RXMP_PAUSE_OFF_WM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00010 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x05800 + $FM_CM_BASE); # 0x115800 CM_BASE
}

sub FM_CM_PORT_RXMP_PAUSE_OFF_WM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0xffffffff;
}

our $FM_CM_PORT_RXMP_PAUSE_OFF_WM_WIDTH           =  1; #
our $FM_CM_PORT_RXMP_PAUSE_OFF_WM_BITS            =  32; #

sub FM_CM_PORT_RXMP_PAUSE_OFF_WM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
sub FM_CM_PORT_RXMP_PAUSE_OFF_WM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_CM_PORT_RXMP_PAUSE_OFF_WM_STRIDE_0        =  1; #
our $FM_CM_PORT_RXMP_PAUSE_OFF_WM_STRIDE_1        =  16; #


sub FM_CM_SHARED_RXMP_PAUSE_ON_WM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06000 + $FM_CM_BASE); # 0x116000 CM_BASE
}

sub FM_CM_SHARED_RXMP_PAUSE_ON_WM_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0xffffffff;
}

our $FM_CM_SHARED_RXMP_PAUSE_ON_WM_WIDTH          =  1; #
our $FM_CM_SHARED_RXMP_PAUSE_ON_WM_BITS           =  32; #

sub FM_CM_SHARED_RXMP_PAUSE_ON_WM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
our $FM_CM_SHARED_RXMP_PAUSE_ON_WM_STRIDE         =  1; #


sub FM_CM_SHARED_RXMP_PAUSE_OFF_WM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06010 + $FM_CM_BASE); # 0x116010 CM_BASE
}

sub FM_CM_SHARED_RXMP_PAUSE_OFF_WM_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0xffffffff;
}

our $FM_CM_SHARED_RXMP_PAUSE_OFF_WM_WIDTH         =  1; #
our $FM_CM_SHARED_RXMP_PAUSE_OFF_WM_BITS          =  32; #

sub FM_CM_SHARED_RXMP_PAUSE_OFF_WM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
our $FM_CM_SHARED_RXMP_PAUSE_OFF_WM_STRIDE        =  1; #


sub FM_CM_TC_PC_MAP {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x06100 + $FM_CM_BASE + ($word)); # 0x116100 CM_BASE
}

sub FM_CM_TC_PC_MAP_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CM_TC_PC_MAP_WIDTH                        =  2; #
our $FM_CM_TC_PC_MAP_BITS                         =  36; #

sub FM_CM_TC_PC_MAP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_CM_TC_PC_MAP_STRIDE                       =  2; #


sub FM_CM_PC_RXMP_MAP {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06200 + $FM_CM_BASE); # 0x116200 CM_BASE
}

sub FM_CM_PC_RXMP_MAP_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x88888888;
}

our $FM_CM_PC_RXMP_MAP_WIDTH                      =  1; #
our $FM_CM_PC_RXMP_MAP_BITS                       =  32; #

sub FM_CM_PC_RXMP_MAP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_CM_PC_RXMP_MAP_STRIDE                     =  1; #


sub FM_CM_PAUSE_CFG {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x06400 + $FM_CM_BASE + ($word)); # 0x116400 CM_BASE
}

sub FM_CM_PAUSE_CFG_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0xc1e613ff;}
    if($word == 1) {return 0x003fffff;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_CM_PAUSE_CFG_WIDTH                        =  4; #
our $FM_CM_PAUSE_CFG_BITS                         =  76; #

sub FM_CM_PAUSE_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_CM_PAUSE_CFG_STRIDE                       =  4; #


sub FM_CM_PAUSE_PACING_CFG {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x06600 + $FM_CM_BASE + ($word)); # 0x116600 CM_BASE
}

sub FM_CM_PAUSE_PACING_CFG_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_CM_PAUSE_PACING_CFG_WIDTH                 =  4; #
our $FM_CM_PAUSE_PACING_CFG_BITS                  =  128; #

sub FM_CM_PAUSE_PACING_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_CM_PAUSE_PACING_CFG_STRIDE                =  4; #


sub FM_CM_PAUSE_RCV_STATE {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00008 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x06800 + $FM_CM_BASE); # 0x116800 CM_BASE
}

sub FM_CM_PAUSE_RCV_STATE_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_CM_PAUSE_RCV_STATE_WIDTH                  =  1; #
our $FM_CM_PAUSE_RCV_STATE_BITS                   =  25; #

sub FM_CM_PAUSE_RCV_STATE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}
sub FM_CM_PAUSE_RCV_STATE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_CM_PAUSE_RCV_STATE_STRIDE_0               =  1; #
our $FM_CM_PAUSE_RCV_STATE_STRIDE_1               =  8; #


sub FM_CM_ESCHED_STATE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06C00 + $FM_CM_BASE); # 0x116C00 CM_BASE
}

sub FM_CM_ESCHED_STATE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CM_ESCHED_STATE_WIDTH                     =  1; #
our $FM_CM_ESCHED_STATE_BITS                      =  24; #

sub FM_CM_ESCHED_STATE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_CM_ESCHED_STATE_STRIDE                    =  1; #


sub FM_CM_PAUSE_GEN_STATE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x06D00 + $FM_CM_BASE + ($word)); # 0x116D00 CM_BASE
}

sub FM_CM_PAUSE_GEN_STATE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_CM_PAUSE_GEN_STATE_WIDTH                  =  2; #
our $FM_CM_PAUSE_GEN_STATE_BITS                   =  44; #

sub FM_CM_PAUSE_GEN_STATE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_CM_PAUSE_GEN_STATE_STRIDE                 =  2; #


sub FM_CM_PAUSE_PACING_STATE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06E00 + $FM_CM_BASE); # 0x116E00 CM_BASE
}

sub FM_CM_PAUSE_PACING_STATE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CM_PAUSE_PACING_STATE_WIDTH               =  1; #
our $FM_CM_PAUSE_PACING_STATE_BITS                =  8; #

sub FM_CM_PAUSE_PACING_STATE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_CM_PAUSE_PACING_STATE_STRIDE              =  1; #


sub FM_CM_TX_MIRROR_DEST {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x06E80 + $FM_CM_BASE + ($word)); # 0x116E80 CM_BASE
}

sub FM_CM_TX_MIRROR_DEST_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_CM_TX_MIRROR_DEST_WIDTH                   =  3; #
our $FM_CM_TX_MIRROR_DEST_BITS                    =  77; #

sub FM_CM_TX_MIRROR_DEST_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CM_TX_MIRROR_DEST_STRIDE                  =  4; #


sub FM_CM_TX_MIRROR_SRC {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x06E90 + $FM_CM_BASE + ($word)); # 0x116E90 CM_BASE
}

sub FM_CM_TX_MIRROR_SRC_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_CM_TX_MIRROR_SRC_WIDTH                    =  3; #
our $FM_CM_TX_MIRROR_SRC_BITS                     =  80; #

sub FM_CM_TX_MIRROR_SRC_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_CM_TX_MIRROR_SRC_STRIDE                   =  4; #


sub FM_CM_SAMPLING_MIRROR_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06EA0 + $FM_CM_BASE); # 0x116EA0 CM_BASE
}

sub FM_CM_SAMPLING_MIRROR_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_CM_SAMPLING_MIRROR_CFG_WIDTH              =  1; #
our $FM_CM_SAMPLING_MIRROR_CFG_BITS               =  6; #

sub FM_CM_SAMPLING_MIRROR_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
our $FM_CM_SAMPLING_MIRROR_CFG_STRIDE             =  1; #


sub FM_ERL_CFG {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00010 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x07000 + $FM_CM_BASE); # 0x117000 CM_BASE
}

sub FM_ERL_CFG_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x20001000;
}

our $FM_ERL_CFG_WIDTH                             =  1; #
our $FM_ERL_CFG_BITS                              =  32; #

sub FM_ERL_CFG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
sub FM_ERL_CFG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_ERL_CFG_STRIDE_0                          =  1; #
our $FM_ERL_CFG_STRIDE_1                          =  16; #


sub FM_ERL_CFG_IFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x07800 + $FM_CM_BASE); # 0x117800 CM_BASE
}

sub FM_ERL_CFG_IFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000014;
}

our $FM_ERL_CFG_IFG_WIDTH                         =  1; #
our $FM_ERL_CFG_IFG_BITS                          =  8; #

sub FM_ERL_CFG_IFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_ERL_CFG_IFG_STRIDE                        =  1; #


sub FM_ERL_USAGE {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00010 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x08000 + $FM_CM_BASE); # 0x118000 CM_BASE
}

sub FM_ERL_USAGE_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x003fffff;
}

our $FM_ERL_USAGE_WIDTH                           =  1; #
our $FM_ERL_USAGE_BITS                            =  23; #

sub FM_ERL_USAGE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
sub FM_ERL_USAGE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_ERL_USAGE_STRIDE_0                        =  1; #
our $FM_ERL_USAGE_STRIDE_1                        =  16; #


sub FM_CM_QUEUE_STATE_INIT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00010 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x08800 + $FM_CM_BASE); # 0x118800 CM_BASE
}

sub FM_CM_QUEUE_STATE_INIT_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_CM_QUEUE_STATE_INIT_WIDTH                 =  1; #
our $FM_CM_QUEUE_STATE_INIT_BITS                  =  1; #

sub FM_CM_QUEUE_STATE_INIT_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
sub FM_CM_QUEUE_STATE_INIT_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 80;
}
our $FM_CM_QUEUE_STATE_INIT_STRIDE_0              =  1; #
our $FM_CM_QUEUE_STATE_INIT_STRIDE_1              =  16; #


sub FM_MAPPER_VID_PROFILE_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00000 + $FM_MAPPER_BASE); # 0x120000 MAPPER_BASE
}

sub FM_MAPPER_VID_PROFILE_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000002;
}

our $FM_MAPPER_VID_PROFILE_TABLE_WIDTH            =  1; #
our $FM_MAPPER_VID_PROFILE_TABLE_BITS             =  5; #

sub FM_MAPPER_VID_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_MAPPER_VID_PROFILE_TABLE_STRIDE           =  1; #


sub FM_MAPPER_VID1_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x01000 + $FM_MAPPER_BASE); # 0x121000 MAPPER_BASE
}

sub FM_MAPPER_VID1_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_VID1_TABLE_WIDTH                   =  1; #
our $FM_MAPPER_VID1_TABLE_BITS                    =  17; #

sub FM_MAPPER_VID1_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}
our $FM_MAPPER_VID1_TABLE_STRIDE                  =  1; #


sub FM_MAPPER_VID2_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x02000 + $FM_MAPPER_BASE); # 0x122000 MAPPER_BASE
}

sub FM_MAPPER_VID2_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_VID2_TABLE_WIDTH                   =  1; #
our $FM_MAPPER_VID2_TABLE_BITS                    =  17; #

sub FM_MAPPER_VID2_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}
our $FM_MAPPER_VID2_TABLE_STRIDE                  =  1; #


sub FM_MAPPER_SRC_PORT_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x03000 + $FM_MAPPER_BASE + ($word)); # 0x123000 MAPPER_BASE
}

sub FM_MAPPER_SRC_PORT_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_MAPPER_SRC_PORT_TABLE_WIDTH               =  2; #
our $FM_MAPPER_SRC_PORT_TABLE_BITS                =  39; #

sub FM_MAPPER_SRC_PORT_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_MAPPER_SRC_PORT_TABLE_STRIDE              =  2; #


sub FM_MAPPER_DMAC_CAM1 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x03100 + $FM_MAPPER_BASE + ($word)); # 0x123100 MAPPER_BASE
}

sub FM_MAPPER_DMAC_CAM1_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_MAPPER_DMAC_CAM1_WIDTH                    =  3; #
our $FM_MAPPER_DMAC_CAM1_BITS                     =  96; #

sub FM_MAPPER_DMAC_CAM1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_DMAC_CAM1_STRIDE                   =  4; #


sub FM_MAPPER_DMAC_CAM2 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x03140 + $FM_MAPPER_BASE + ($word)); # 0x123140 MAPPER_BASE
}

sub FM_MAPPER_DMAC_CAM2_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_MAPPER_DMAC_CAM2_WIDTH                    =  3; #
our $FM_MAPPER_DMAC_CAM2_BITS                     =  96; #

sub FM_MAPPER_DMAC_CAM2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_DMAC_CAM2_STRIDE                   =  4; #


sub FM_MAPPER_DMAC_CAM3 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x03180 + $FM_MAPPER_BASE + ($word)); # 0x123180 MAPPER_BASE
}

sub FM_MAPPER_DMAC_CAM3_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_MAPPER_DMAC_CAM3_WIDTH                    =  3; #
our $FM_MAPPER_DMAC_CAM3_BITS                     =  96; #

sub FM_MAPPER_DMAC_CAM3_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_MAPPER_DMAC_CAM3_STRIDE                   =  4; #


sub FM_MAPPER_DMAC_RAM1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x03200 + $FM_MAPPER_BASE); # 0x123200 MAPPER_BASE
}

sub FM_MAPPER_DMAC_RAM1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_DMAC_RAM1_WIDTH                    =  1; #
our $FM_MAPPER_DMAC_RAM1_BITS                     =  5; #

sub FM_MAPPER_DMAC_RAM1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_DMAC_RAM1_STRIDE                   =  1; #


sub FM_MAPPER_DMAC_RAM2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x03210 + $FM_MAPPER_BASE); # 0x123210 MAPPER_BASE
}

sub FM_MAPPER_DMAC_RAM2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_DMAC_RAM2_WIDTH                    =  1; #
our $FM_MAPPER_DMAC_RAM2_BITS                     =  4; #

sub FM_MAPPER_DMAC_RAM2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_DMAC_RAM2_STRIDE                   =  1; #


sub FM_MAPPER_DMAC_RAM3 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x03220 + $FM_MAPPER_BASE); # 0x123220 MAPPER_BASE
}

sub FM_MAPPER_DMAC_RAM3_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_DMAC_RAM3_WIDTH                    =  1; #
our $FM_MAPPER_DMAC_RAM3_BITS                     =  5; #

sub FM_MAPPER_DMAC_RAM3_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_MAPPER_DMAC_RAM3_STRIDE                   =  1; #


sub FM_MAPPER_SMAC_CAM1 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x03300 + $FM_MAPPER_BASE + ($word)); # 0x123300 MAPPER_BASE
}

sub FM_MAPPER_SMAC_CAM1_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_MAPPER_SMAC_CAM1_WIDTH                    =  3; #
our $FM_MAPPER_SMAC_CAM1_BITS                     =  96; #

sub FM_MAPPER_SMAC_CAM1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_SMAC_CAM1_STRIDE                   =  4; #


sub FM_MAPPER_SMAC_CAM3 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x03380 + $FM_MAPPER_BASE + ($word)); # 0x123380 MAPPER_BASE
}

sub FM_MAPPER_SMAC_CAM3_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_MAPPER_SMAC_CAM3_WIDTH                    =  3; #
our $FM_MAPPER_SMAC_CAM3_BITS                     =  96; #

sub FM_MAPPER_SMAC_CAM3_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_MAPPER_SMAC_CAM3_STRIDE                   =  4; #


sub FM_MAPPER_SMAC_RAM1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x03400 + $FM_MAPPER_BASE); # 0x123400 MAPPER_BASE
}

sub FM_MAPPER_SMAC_RAM1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_SMAC_RAM1_WIDTH                    =  1; #
our $FM_MAPPER_SMAC_RAM1_BITS                     =  4; #

sub FM_MAPPER_SMAC_RAM1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_SMAC_RAM1_STRIDE                   =  1; #


sub FM_MAPPER_SMAC_RAM3 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x03420 + $FM_MAPPER_BASE); # 0x123420 MAPPER_BASE
}

sub FM_MAPPER_SMAC_RAM3_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_SMAC_RAM3_WIDTH                    =  1; #
our $FM_MAPPER_SMAC_RAM3_BITS                     =  5; #

sub FM_MAPPER_SMAC_RAM3_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_MAPPER_SMAC_RAM3_STRIDE                   =  1; #


sub FM_MAPPER_TYPE_CAM1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x03440 + $FM_MAPPER_BASE); # 0x123440 MAPPER_BASE
}

sub FM_MAPPER_TYPE_CAM1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_TYPE_CAM1_WIDTH                    =  1; #
our $FM_MAPPER_TYPE_CAM1_BITS                     =  32; #

sub FM_MAPPER_TYPE_CAM1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_TYPE_CAM1_STRIDE                   =  1; #


sub FM_MAPPER_TYPE_CAM2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x03450 + $FM_MAPPER_BASE); # 0x123450 MAPPER_BASE
}

sub FM_MAPPER_TYPE_CAM2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_TYPE_CAM2_WIDTH                    =  1; #
our $FM_MAPPER_TYPE_CAM2_BITS                     =  32; #

sub FM_MAPPER_TYPE_CAM2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_TYPE_CAM2_STRIDE                   =  1; #


sub FM_MAPPER_TYPE_RAM1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x03460 + $FM_MAPPER_BASE); # 0x123460 MAPPER_BASE
}

sub FM_MAPPER_TYPE_RAM1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_TYPE_RAM1_WIDTH                    =  1; #
our $FM_MAPPER_TYPE_RAM1_BITS                     =  4; #

sub FM_MAPPER_TYPE_RAM1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_TYPE_RAM1_STRIDE                   =  1; #


sub FM_MAPPER_TYPE_RAM2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x03470 + $FM_MAPPER_BASE); # 0x123470 MAPPER_BASE
}

sub FM_MAPPER_TYPE_RAM2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_TYPE_RAM2_WIDTH                    =  1; #
our $FM_MAPPER_TYPE_RAM2_BITS                     =  4; #

sub FM_MAPPER_TYPE_RAM2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_TYPE_RAM2_STRIDE                   =  1; #


sub FM_MAPPER_DIP_CAM1 {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00008 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x03800 + $FM_MAPPER_BASE + ($word)); # 0x123800 MAPPER_BASE
}

sub FM_MAPPER_DIP_CAM1_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_MAPPER_DIP_CAM1_WIDTH                     =  4; #
our $FM_MAPPER_DIP_CAM1_BITS                      =  128; #

sub FM_MAPPER_DIP_CAM1_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
sub FM_MAPPER_DIP_CAM1_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_DIP_CAM1_STRIDE_0                  =  4; #
our $FM_MAPPER_DIP_CAM1_STRIDE_1                  =  8; #


sub FM_MAPPER_DIP_CAM2 {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00008 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x03880 + $FM_MAPPER_BASE + ($word)); # 0x123880 MAPPER_BASE
}

sub FM_MAPPER_DIP_CAM2_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_MAPPER_DIP_CAM2_WIDTH                     =  4; #
our $FM_MAPPER_DIP_CAM2_BITS                      =  128; #

sub FM_MAPPER_DIP_CAM2_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
sub FM_MAPPER_DIP_CAM2_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_DIP_CAM2_STRIDE_0                  =  4; #
our $FM_MAPPER_DIP_CAM2_STRIDE_1                  =  8; #


sub FM_MAPPER_DIP_CAM3 {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00008 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x03900 + $FM_MAPPER_BASE + ($word)); # 0x123900 MAPPER_BASE
}

sub FM_MAPPER_DIP_CAM3_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_MAPPER_DIP_CAM3_WIDTH                     =  4; #
our $FM_MAPPER_DIP_CAM3_BITS                      =  128; #

sub FM_MAPPER_DIP_CAM3_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
sub FM_MAPPER_DIP_CAM3_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_MAPPER_DIP_CAM3_STRIDE_0                  =  4; #
our $FM_MAPPER_DIP_CAM3_STRIDE_1                  =  8; #


sub FM_MAPPER_SIP_CAM1 {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00008 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x03A00 + $FM_MAPPER_BASE + ($word)); # 0x123A00 MAPPER_BASE
}

sub FM_MAPPER_SIP_CAM1_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_MAPPER_SIP_CAM1_WIDTH                     =  4; #
our $FM_MAPPER_SIP_CAM1_BITS                      =  128; #

sub FM_MAPPER_SIP_CAM1_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
sub FM_MAPPER_SIP_CAM1_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_SIP_CAM1_STRIDE_0                  =  4; #
our $FM_MAPPER_SIP_CAM1_STRIDE_1                  =  8; #


sub FM_MAPPER_SIP_CAM2 {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00008 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x03A80 + $FM_MAPPER_BASE + ($word)); # 0x123A80 MAPPER_BASE
}

sub FM_MAPPER_SIP_CAM2_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_MAPPER_SIP_CAM2_WIDTH                     =  4; #
our $FM_MAPPER_SIP_CAM2_BITS                      =  128; #

sub FM_MAPPER_SIP_CAM2_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
sub FM_MAPPER_SIP_CAM2_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_SIP_CAM2_STRIDE_0                  =  4; #
our $FM_MAPPER_SIP_CAM2_STRIDE_1                  =  8; #


sub FM_MAPPER_SIP_CAM3 {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00008 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x03B00 + $FM_MAPPER_BASE + ($word)); # 0x123B00 MAPPER_BASE
}

sub FM_MAPPER_SIP_CAM3_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_MAPPER_SIP_CAM3_WIDTH                     =  4; #
our $FM_MAPPER_SIP_CAM3_BITS                      =  128; #

sub FM_MAPPER_SIP_CAM3_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
sub FM_MAPPER_SIP_CAM3_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_MAPPER_SIP_CAM3_STRIDE_0                  =  4; #
our $FM_MAPPER_SIP_CAM3_STRIDE_1                  =  8; #


sub FM_MAPPER_DIP_RAM1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x03C00 + $FM_MAPPER_BASE); # 0x123C00 MAPPER_BASE
}

sub FM_MAPPER_DIP_RAM1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_DIP_RAM1_WIDTH                     =  1; #
our $FM_MAPPER_DIP_RAM1_BITS                      =  4; #

sub FM_MAPPER_DIP_RAM1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_DIP_RAM1_STRIDE                    =  1; #


sub FM_MAPPER_DIP_RAM2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x03C10 + $FM_MAPPER_BASE); # 0x123C10 MAPPER_BASE
}

sub FM_MAPPER_DIP_RAM2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_DIP_RAM2_WIDTH                     =  1; #
our $FM_MAPPER_DIP_RAM2_BITS                      =  4; #

sub FM_MAPPER_DIP_RAM2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_DIP_RAM2_STRIDE                    =  1; #


sub FM_MAPPER_DIP_RAM3 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x03C20 + $FM_MAPPER_BASE); # 0x123C20 MAPPER_BASE
}

sub FM_MAPPER_DIP_RAM3_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_DIP_RAM3_WIDTH                     =  1; #
our $FM_MAPPER_DIP_RAM3_BITS                      =  5; #

sub FM_MAPPER_DIP_RAM3_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_MAPPER_DIP_RAM3_STRIDE                    =  1; #


sub FM_MAPPER_SIP_RAM1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x03C40 + $FM_MAPPER_BASE); # 0x123C40 MAPPER_BASE
}

sub FM_MAPPER_SIP_RAM1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_SIP_RAM1_WIDTH                     =  1; #
our $FM_MAPPER_SIP_RAM1_BITS                      =  4; #

sub FM_MAPPER_SIP_RAM1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_SIP_RAM1_STRIDE                    =  1; #


sub FM_MAPPER_SIP_RAM2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x03C50 + $FM_MAPPER_BASE); # 0x123C50 MAPPER_BASE
}

sub FM_MAPPER_SIP_RAM2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_SIP_RAM2_WIDTH                     =  1; #
our $FM_MAPPER_SIP_RAM2_BITS                      =  4; #

sub FM_MAPPER_SIP_RAM2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_SIP_RAM2_STRIDE                    =  1; #


sub FM_MAPPER_SIP_RAM3 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x03C60 + $FM_MAPPER_BASE); # 0x123C60 MAPPER_BASE
}

sub FM_MAPPER_SIP_RAM3_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_SIP_RAM3_WIDTH                     =  1; #
our $FM_MAPPER_SIP_RAM3_BITS                      =  5; #

sub FM_MAPPER_SIP_RAM3_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_MAPPER_SIP_RAM3_STRIDE                    =  1; #


sub FM_MAPPER_PROT_CAM1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x03C80 + $FM_MAPPER_BASE); # 0x123C80 MAPPER_BASE
}

sub FM_MAPPER_PROT_CAM1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_PROT_CAM1_WIDTH                    =  1; #
our $FM_MAPPER_PROT_CAM1_BITS                     =  16; #

sub FM_MAPPER_PROT_CAM1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_PROT_CAM1_STRIDE                   =  1; #


sub FM_MAPPER_PROT_CAM2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x03C90 + $FM_MAPPER_BASE); # 0x123C90 MAPPER_BASE
}

sub FM_MAPPER_PROT_CAM2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_PROT_CAM2_WIDTH                    =  1; #
our $FM_MAPPER_PROT_CAM2_BITS                     =  16; #

sub FM_MAPPER_PROT_CAM2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_PROT_CAM2_STRIDE                   =  1; #


sub FM_MAPPER_PROT_RAM1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x03CA0 + $FM_MAPPER_BASE); # 0x123CA0 MAPPER_BASE
}

sub FM_MAPPER_PROT_RAM1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_PROT_RAM1_WIDTH                    =  1; #
our $FM_MAPPER_PROT_RAM1_BITS                     =  4; #

sub FM_MAPPER_PROT_RAM1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_PROT_RAM1_STRIDE                   =  1; #


sub FM_MAPPER_PROT_RAM2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x03CB0 + $FM_MAPPER_BASE); # 0x123CB0 MAPPER_BASE
}

sub FM_MAPPER_PROT_RAM2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_PROT_RAM2_WIDTH                    =  1; #
our $FM_MAPPER_PROT_RAM2_BITS                     =  4; #

sub FM_MAPPER_PROT_RAM2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_PROT_RAM2_STRIDE                   =  1; #


sub FM_MAPPER_LENGTH_COMPARE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x03CC0 + $FM_MAPPER_BASE); # 0x123CC0 MAPPER_BASE
}

sub FM_MAPPER_LENGTH_COMPARE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_LENGTH_COMPARE_WIDTH               =  1; #
our $FM_MAPPER_LENGTH_COMPARE_BITS                =  20; #

sub FM_MAPPER_LENGTH_COMPARE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_LENGTH_COMPARE_STRIDE              =  1; #


sub FM_MAPPER_L4_SRC_COMPARE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x03D00 + $FM_MAPPER_BASE + ($word)); # 0x123D00 MAPPER_BASE
}

sub FM_MAPPER_L4_SRC_COMPARE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_MAPPER_L4_SRC_COMPARE_WIDTH               =  2; #
our $FM_MAPPER_L4_SRC_COMPARE_BITS                =  41; #

sub FM_MAPPER_L4_SRC_COMPARE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}
our $FM_MAPPER_L4_SRC_COMPARE_STRIDE              =  2; #


sub FM_MAPPER_L4_DST_COMPARE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x03D80 + $FM_MAPPER_BASE + ($word)); # 0x123D80 MAPPER_BASE
}

sub FM_MAPPER_L4_DST_COMPARE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_MAPPER_L4_DST_COMPARE_WIDTH               =  2; #
our $FM_MAPPER_L4_DST_COMPARE_BITS                =  41; #

sub FM_MAPPER_L4_DST_COMPARE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}
our $FM_MAPPER_L4_DST_COMPARE_STRIDE              =  2; #


sub FM_MAPPER_SCENARIO_FLAGS_CFG {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x03E00 + $FM_MAPPER_BASE + ($word)); # 0x123E00 MAPPER_BASE
}

sub FM_MAPPER_SCENARIO_FLAGS_CFG_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x03020100;}
    if($word == 1) {return 0x07060504;}
    if($word == 2) {return 0x0b0a0908;}
    if($word == 3) {return 0x0f0e0d0c;}
}

our $FM_MAPPER_SCENARIO_FLAGS_CFG_WIDTH           =  4; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_BITS            =  126; #



sub FM_MAPPER_FFU_INIT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x03E04 + $FM_MAPPER_BASE); # 0x123E04 MAPPER_BASE
}

sub FM_MAPPER_FFU_INIT_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MAPPER_FFU_INIT_WIDTH                     =  1; #
our $FM_MAPPER_FFU_INIT_BITS                      =  18; #



sub FM_MAPPER_QOS_PER_PORT_VPRI1 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x03F00 + $FM_MAPPER_BASE + ($word)); # 0x123F00 MAPPER_BASE
}

sub FM_MAPPER_QOS_PER_PORT_VPRI1_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x33221100;}
    if($word == 1) {return 0x77665544;}
}

our $FM_MAPPER_QOS_PER_PORT_VPRI1_WIDTH           =  2; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_BITS            =  64; #

sub FM_MAPPER_QOS_PER_PORT_VPRI1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_MAPPER_QOS_PER_PORT_VPRI1_STRIDE          =  2; #


sub FM_MAPPER_QOS_PER_PORT_VPRI2 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x04000 + $FM_MAPPER_BASE + ($word)); # 0x124000 MAPPER_BASE
}

sub FM_MAPPER_QOS_PER_PORT_VPRI2_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x33221100;}
    if($word == 1) {return 0x77665544;}
}

our $FM_MAPPER_QOS_PER_PORT_VPRI2_WIDTH           =  2; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_BITS            =  64; #

sub FM_MAPPER_QOS_PER_PORT_VPRI2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_MAPPER_QOS_PER_PORT_VPRI2_STRIDE          =  2; #


sub FM_MAPPER_QOS_PER_PORT_W4 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x04100 + $FM_MAPPER_BASE + ($word)); # 0x124100 MAPPER_BASE
}

sub FM_MAPPER_QOS_PER_PORT_W4_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x33221100;}
    if($word == 1) {return 0x77665544;}
}

our $FM_MAPPER_QOS_PER_PORT_W4_WIDTH              =  2; #
our $FM_MAPPER_QOS_PER_PORT_W4_BITS               =  64; #

sub FM_MAPPER_QOS_PER_PORT_W4_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_MAPPER_QOS_PER_PORT_W4_STRIDE             =  2; #


sub FM_MAPPER_QOS_L2_VPRI1_TO_ISL {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x04200 + $FM_MAPPER_BASE); # 0x124200 MAPPER_BASE
}

sub FM_MAPPER_QOS_L2_VPRI1_TO_ISL_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_QOS_L2_VPRI1_TO_ISL_WIDTH          =  1; #
our $FM_MAPPER_QOS_L2_VPRI1_TO_ISL_BITS           =  4; #

sub FM_MAPPER_QOS_L2_VPRI1_TO_ISL_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_QOS_L2_VPRI1_TO_ISL_STRIDE         =  1; #


sub FM_MAPPER_QOS_L2_VPRI2_TO_ISL {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x04210 + $FM_MAPPER_BASE); # 0x124210 MAPPER_BASE
}

sub FM_MAPPER_QOS_L2_VPRI2_TO_ISL_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_QOS_L2_VPRI2_TO_ISL_WIDTH          =  1; #
our $FM_MAPPER_QOS_L2_VPRI2_TO_ISL_BITS           =  4; #

sub FM_MAPPER_QOS_L2_VPRI2_TO_ISL_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_QOS_L2_VPRI2_TO_ISL_STRIDE         =  1; #


sub FM_MAPPER_QOS_W4_TO_ISL {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x04220 + $FM_MAPPER_BASE); # 0x124220 MAPPER_BASE
}

sub FM_MAPPER_QOS_W4_TO_ISL_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_QOS_W4_TO_ISL_WIDTH                =  1; #
our $FM_MAPPER_QOS_W4_TO_ISL_BITS                 =  4; #

sub FM_MAPPER_QOS_W4_TO_ISL_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_QOS_W4_TO_ISL_STRIDE               =  1; #


sub FM_MAPPER_QOS_L3_PRI_TO_ISL {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x04230 + $FM_MAPPER_BASE); # 0x124230 MAPPER_BASE
}

sub FM_MAPPER_QOS_L3_PRI_TO_ISL_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_QOS_L3_PRI_TO_ISL_WIDTH            =  1; #
our $FM_MAPPER_QOS_L3_PRI_TO_ISL_BITS             =  16; #

sub FM_MAPPER_QOS_L3_PRI_TO_ISL_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_MAPPER_QOS_L3_PRI_TO_ISL_STRIDE           =  1; #


sub FM_MAPPER_QOS_CAM1 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x04280 + $FM_MAPPER_BASE + ($word)); # 0x124280 MAPPER_BASE
}

sub FM_MAPPER_QOS_CAM1_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_MAPPER_QOS_CAM1_WIDTH                     =  2; #
our $FM_MAPPER_QOS_CAM1_BITS                      =  64; #

sub FM_MAPPER_QOS_CAM1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_MAPPER_QOS_CAM1_STRIDE                    =  2; #


sub FM_MAPPER_QOS_CAM2 {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x042C0 + $FM_MAPPER_BASE + ($word)); # 0x1242C0 MAPPER_BASE
}

sub FM_MAPPER_QOS_CAM2_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_MAPPER_QOS_CAM2_WIDTH                     =  2; #
our $FM_MAPPER_QOS_CAM2_BITS                      =  64; #

sub FM_MAPPER_QOS_CAM2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_MAPPER_QOS_CAM2_STRIDE                    =  2; #


sub FM_MAPPER_QOS_RAM1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x04300 + $FM_MAPPER_BASE); # 0x124300 MAPPER_BASE
}

sub FM_MAPPER_QOS_RAM1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_QOS_RAM1_WIDTH                     =  1; #
our $FM_MAPPER_QOS_RAM1_BITS                      =  6; #

sub FM_MAPPER_QOS_RAM1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_MAPPER_QOS_RAM1_STRIDE                    =  1; #


sub FM_MAPPER_QOS_RAM2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x04320 + $FM_MAPPER_BASE); # 0x124320 MAPPER_BASE
}

sub FM_MAPPER_QOS_RAM2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MAPPER_QOS_RAM2_WIDTH                     =  1; #
our $FM_MAPPER_QOS_RAM2_BITS                      =  3; #

sub FM_MAPPER_QOS_RAM2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_MAPPER_QOS_RAM2_STRIDE                    =  1; #


sub FM_POLICER_CFG_4K {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x02000 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x00000 + $FM_POLICERS_BASE + ($word)); # 0x130000 POLICERS_BASE
}

sub FM_POLICER_CFG_4K_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_POLICER_CFG_4K_WIDTH                      =  2; #
our $FM_POLICER_CFG_4K_BITS                       =  64; #

sub FM_POLICER_CFG_4K_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}
sub FM_POLICER_CFG_4K_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
our $FM_POLICER_CFG_4K_STRIDE_0                   =  2; #
our $FM_POLICER_CFG_4K_STRIDE_1                   =  8192; #


sub FM_POLICER_CFG_1K {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x04000 + $FM_POLICERS_BASE); # 0x134000 POLICERS_BASE
}

sub FM_POLICER_CFG_1K_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_POLICER_CFG_1K_WIDTH                      =  1; #
our $FM_POLICER_CFG_1K_BITS                       =  32; #

sub FM_POLICER_CFG_1K_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 1024;
}
our $FM_POLICER_CFG_1K_STRIDE                     =  1; #


sub FM_POLICER_STATE_4K {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x02000 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x08000 + $FM_POLICERS_BASE + ($word)); # 0x138000 POLICERS_BASE
}

sub FM_POLICER_STATE_4K_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_POLICER_STATE_4K_WIDTH                    =  2; #
our $FM_POLICER_STATE_4K_BITS                     =  64; #

sub FM_POLICER_STATE_4K_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}
sub FM_POLICER_STATE_4K_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
our $FM_POLICER_STATE_4K_STRIDE_0                 =  2; #
our $FM_POLICER_STATE_4K_STRIDE_1                 =  8192; #


sub FM_POLICER_STATE_1K {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0C000 + $FM_POLICERS_BASE); # 0x13C000 POLICERS_BASE
}

sub FM_POLICER_STATE_1K_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_POLICER_STATE_1K_WIDTH                    =  1; #
our $FM_POLICER_STATE_1K_BITS                     =  32; #

sub FM_POLICER_STATE_1K_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 1024;
}
our $FM_POLICER_STATE_1K_STRIDE                   =  1; #


sub FM_POLICER_QOS_MAP1 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0C400 + $FM_POLICERS_BASE); # 0x13C400 POLICERS_BASE
}

sub FM_POLICER_QOS_MAP1_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_POLICER_QOS_MAP1_WIDTH                    =  1; #
our $FM_POLICER_QOS_MAP1_BITS                     =  8; #

sub FM_POLICER_QOS_MAP1_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_POLICER_QOS_MAP1_STRIDE                   =  1; #


sub FM_POLICER_QOS_MAP2 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0C440 + $FM_POLICERS_BASE); # 0x13C440 POLICERS_BASE
}

sub FM_POLICER_QOS_MAP2_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_POLICER_QOS_MAP2_WIDTH                    =  1; #
our $FM_POLICER_QOS_MAP2_BITS                     =  16; #

sub FM_POLICER_QOS_MAP2_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}
our $FM_POLICER_QOS_MAP2_STRIDE                   =  1; #


sub FM_L2AR_CAM {
    my ($self, $index2, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 5, -1);

    return (0x00800 * (($index2) - 0) + 0x00020 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x00000 + $FM_L2AR_BASE + ($word)); # 0x140000 L2AR_BASE
}

sub FM_L2AR_CAM_DEFAULT {
    my ($self, $index2, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 5, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_L2AR_CAM_WIDTH                            =  4; #
our $FM_L2AR_CAM_BITS                             =  128; #

sub FM_L2AR_CAM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 6;
}
sub FM_L2AR_CAM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}
sub FM_L2AR_CAM_ENTRIES_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}
our $FM_L2AR_CAM_STRIDE_0                         =  4; #
our $FM_L2AR_CAM_STRIDE_1                         =  32; #
our $FM_L2AR_CAM_STRIDE_2                         =  2048; #


sub FM_L2AR_CAM_DMASK {
    my ($self, $index2, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 5, -1);

    return (0x00200 * (($index2) - 0) + 0x00008 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x04000 + $FM_L2AR_BASE + ($word)); # 0x144000 L2AR_BASE
}

sub FM_L2AR_CAM_DMASK_DEFAULT {
    my ($self, $index2, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 5, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_L2AR_CAM_DMASK_WIDTH                      =  4; #
our $FM_L2AR_CAM_DMASK_BITS                       =  102; #

sub FM_L2AR_CAM_DMASK_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
sub FM_L2AR_CAM_DMASK_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}
sub FM_L2AR_CAM_DMASK_ENTRIES_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}
our $FM_L2AR_CAM_DMASK_STRIDE_0                   =  4; #
our $FM_L2AR_CAM_DMASK_STRIDE_1                   =  8; #
our $FM_L2AR_CAM_DMASK_STRIDE_2                   =  512; #


sub FM_L2AR_SLICE_CFG {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x05000 + $FM_L2AR_BASE); # 0x145000 L2AR_BASE
}

sub FM_L2AR_SLICE_CFG_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_L2AR_SLICE_CFG_WIDTH                      =  1; #
our $FM_L2AR_SLICE_CFG_BITS                       =  16; #



sub FM_L2AR_EACL_EXT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x05010 + $FM_L2AR_BASE + ($word)); # 0x145010 L2AR_BASE
}

sub FM_L2AR_EACL_EXT_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L2AR_EACL_EXT_WIDTH                       =  2; #
our $FM_L2AR_EACL_EXT_BITS                        =  64; #

sub FM_L2AR_EACL_EXT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}
our $FM_L2AR_EACL_EXT_STRIDE                      =  2; #


sub FM_L2AR_RAM {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00080 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x05400 + $FM_L2AR_BASE + ($word)); # 0x145400 L2AR_BASE
}

sub FM_L2AR_RAM_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L2AR_RAM_WIDTH                            =  2; #
our $FM_L2AR_RAM_BITS                             =  39; #

sub FM_L2AR_RAM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 64;
}
sub FM_L2AR_RAM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}
our $FM_L2AR_RAM_STRIDE_0                         =  2; #
our $FM_L2AR_RAM_STRIDE_1                         =  128; #


sub FM_L2AR_FLAGS_CAM_POLARITY {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x05800 + $FM_L2AR_BASE + ($word)); # 0x145800 L2AR_BASE
}

sub FM_L2AR_FLAGS_CAM_POLARITY_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_L2AR_FLAGS_CAM_POLARITY_WIDTH             =  3; #
our $FM_L2AR_FLAGS_CAM_POLARITY_BITS              =  76; #

sub FM_L2AR_FLAGS_CAM_POLARITY_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
our $FM_L2AR_FLAGS_CAM_POLARITY_STRIDE            =  4; #


sub FM_L2AR_FLAGS_CAM_VALUE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x05808 + $FM_L2AR_BASE + ($word)); # 0x145808 L2AR_BASE
}

sub FM_L2AR_FLAGS_CAM_VALUE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_L2AR_FLAGS_CAM_VALUE_WIDTH                =  3; #
our $FM_L2AR_FLAGS_CAM_VALUE_BITS                 =  76; #

sub FM_L2AR_FLAGS_CAM_VALUE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
our $FM_L2AR_FLAGS_CAM_VALUE_STRIDE               =  4; #


sub FM_L2AR_FLAGS_CAM {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00200 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x05C00 + $FM_L2AR_BASE + ($word)); # 0x145C00 L2AR_BASE
}

sub FM_L2AR_FLAGS_CAM_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_L2AR_FLAGS_CAM_WIDTH                      =  4; #
our $FM_L2AR_FLAGS_CAM_BITS                       =  128; #

sub FM_L2AR_FLAGS_CAM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
sub FM_L2AR_FLAGS_CAM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
our $FM_L2AR_FLAGS_CAM_STRIDE_0                   =  4; #
our $FM_L2AR_FLAGS_CAM_STRIDE_1                   =  512; #


sub FM_L2AR_ACTION_DMT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00080 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x06000 + $FM_L2AR_BASE + ($word)); # 0x146000 L2AR_BASE
}

sub FM_L2AR_ACTION_DMT_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_L2AR_ACTION_DMT_WIDTH                     =  3; #
our $FM_L2AR_ACTION_DMT_BITS                      =  96; #

sub FM_L2AR_ACTION_DMT_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
sub FM_L2AR_ACTION_DMT_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 3;
}
our $FM_L2AR_ACTION_DMT_STRIDE_0                  =  4; #
our $FM_L2AR_ACTION_DMT_STRIDE_1                  =  128; #


sub FM_L2AR_ACTION_CPU_CODE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06200 + $FM_L2AR_BASE); # 0x146200 L2AR_BASE
}

sub FM_L2AR_ACTION_CPU_CODE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_ACTION_CPU_CODE_WIDTH                =  1; #
our $FM_L2AR_ACTION_CPU_CODE_BITS                 =  32; #

sub FM_L2AR_ACTION_CPU_CODE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}
our $FM_L2AR_ACTION_CPU_CODE_STRIDE               =  1; #


sub FM_L2AR_ACTION_TRAP_HEADER {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06280 + $FM_L2AR_BASE); # 0x146280 L2AR_BASE
}

sub FM_L2AR_ACTION_TRAP_HEADER_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_ACTION_TRAP_HEADER_WIDTH             =  1; #
our $FM_L2AR_ACTION_TRAP_HEADER_BITS              =  8; #

sub FM_L2AR_ACTION_TRAP_HEADER_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}
our $FM_L2AR_ACTION_TRAP_HEADER_STRIDE            =  1; #


sub FM_L2AR_ACTION_MIRROR {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00080 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x06400 + $FM_L2AR_BASE); # 0x146400 L2AR_BASE
}

sub FM_L2AR_ACTION_MIRROR_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_L2AR_ACTION_MIRROR_WIDTH                  =  1; #
our $FM_L2AR_ACTION_MIRROR_BITS                   =  16; #

sub FM_L2AR_ACTION_MIRROR_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}
sub FM_L2AR_ACTION_MIRROR_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_L2AR_ACTION_MIRROR_STRIDE_0               =  1; #
our $FM_L2AR_ACTION_MIRROR_STRIDE_1               =  128; #


sub FM_L2AR_ACTION_QOS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06600 + $FM_L2AR_BASE); # 0x146600 L2AR_BASE
}

sub FM_L2AR_ACTION_QOS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_ACTION_QOS_WIDTH                     =  1; #
our $FM_L2AR_ACTION_QOS_BITS                      =  20; #

sub FM_L2AR_ACTION_QOS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}
our $FM_L2AR_ACTION_QOS_STRIDE                    =  1; #


sub FM_L2AR_ACTION_MA_WRITEBACK {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06680 + $FM_L2AR_BASE); # 0x146680 L2AR_BASE
}

sub FM_L2AR_ACTION_MA_WRITEBACK_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_ACTION_MA_WRITEBACK_WIDTH            =  1; #
our $FM_L2AR_ACTION_MA_WRITEBACK_BITS             =  20; #

sub FM_L2AR_ACTION_MA_WRITEBACK_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}
our $FM_L2AR_ACTION_MA_WRITEBACK_STRIDE           =  1; #


sub FM_L2AR_ACTION_DGLORT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06700 + $FM_L2AR_BASE); # 0x146700 L2AR_BASE
}

sub FM_L2AR_ACTION_DGLORT_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_ACTION_DGLORT_WIDTH                  =  1; #
our $FM_L2AR_ACTION_DGLORT_BITS                   =  20; #

sub FM_L2AR_ACTION_DGLORT_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}
our $FM_L2AR_ACTION_DGLORT_STRIDE                 =  1; #


sub FM_L2AR_ACTION_W16AB {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06780 + $FM_L2AR_BASE); # 0x146780 L2AR_BASE
}

sub FM_L2AR_ACTION_W16AB_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_ACTION_W16AB_WIDTH                   =  1; #
our $FM_L2AR_ACTION_W16AB_BITS                    =  20; #

sub FM_L2AR_ACTION_W16AB_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}
our $FM_L2AR_ACTION_W16AB_STRIDE                  =  1; #


sub FM_L2AR_ACTION_W16CDEF {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06800 + $FM_L2AR_BASE); # 0x146800 L2AR_BASE
}

sub FM_L2AR_ACTION_W16CDEF_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_ACTION_W16CDEF_WIDTH                 =  1; #
our $FM_L2AR_ACTION_W16CDEF_BITS                  =  20; #

sub FM_L2AR_ACTION_W16CDEF_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}
our $FM_L2AR_ACTION_W16CDEF_STRIDE                =  1; #


sub FM_L2AR_ACTION_W8ABCDE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06880 + $FM_L2AR_BASE); # 0x146880 L2AR_BASE
}

sub FM_L2AR_ACTION_W8ABCDE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_ACTION_W8ABCDE_WIDTH                 =  1; #
our $FM_L2AR_ACTION_W8ABCDE_BITS                  =  20; #

sub FM_L2AR_ACTION_W8ABCDE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}
our $FM_L2AR_ACTION_W8ABCDE_STRIDE                =  1; #


sub FM_L2AR_ACTION_W4 {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06900 + $FM_L2AR_BASE); # 0x146900 L2AR_BASE
}

sub FM_L2AR_ACTION_W4_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_ACTION_W4_WIDTH                      =  1; #
our $FM_L2AR_ACTION_W4_BITS                       =  20; #

sub FM_L2AR_ACTION_W4_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}
our $FM_L2AR_ACTION_W4_STRIDE                     =  1; #


sub FM_L2AR_ACTION_VID {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06980 + $FM_L2AR_BASE); # 0x146980 L2AR_BASE
}

sub FM_L2AR_ACTION_VID_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_ACTION_VID_WIDTH                     =  1; #
our $FM_L2AR_ACTION_VID_BITS                      =  20; #

sub FM_L2AR_ACTION_VID_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}
our $FM_L2AR_ACTION_VID_STRIDE                    =  1; #


sub FM_L2AR_ACTION_DMASK_IDX {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06A00 + $FM_L2AR_BASE); # 0x146A00 L2AR_BASE
}

sub FM_L2AR_ACTION_DMASK_IDX_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_ACTION_DMASK_IDX_WIDTH               =  1; #
our $FM_L2AR_ACTION_DMASK_IDX_BITS                =  20; #

sub FM_L2AR_ACTION_DMASK_IDX_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}
our $FM_L2AR_ACTION_DMASK_IDX_STRIDE              =  1; #


sub FM_L2AR_ACTION_STATS_IDX5AB {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06A80 + $FM_L2AR_BASE); # 0x146A80 L2AR_BASE
}

sub FM_L2AR_ACTION_STATS_IDX5AB_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_ACTION_STATS_IDX5AB_WIDTH            =  1; #
our $FM_L2AR_ACTION_STATS_IDX5AB_BITS             =  20; #

sub FM_L2AR_ACTION_STATS_IDX5AB_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}
our $FM_L2AR_ACTION_STATS_IDX5AB_STRIDE           =  1; #


sub FM_L2AR_ACTION_STATS_IDX5C {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06B00 + $FM_L2AR_BASE); # 0x146B00 L2AR_BASE
}

sub FM_L2AR_ACTION_STATS_IDX5C_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_ACTION_STATS_IDX5C_WIDTH             =  1; #
our $FM_L2AR_ACTION_STATS_IDX5C_BITS              =  20; #

sub FM_L2AR_ACTION_STATS_IDX5C_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}
our $FM_L2AR_ACTION_STATS_IDX5C_STRIDE            =  1; #


sub FM_L2AR_ACTION_STATS_IDX12A {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06B80 + $FM_L2AR_BASE); # 0x146B80 L2AR_BASE
}

sub FM_L2AR_ACTION_STATS_IDX12A_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_ACTION_STATS_IDX12A_WIDTH            =  1; #
our $FM_L2AR_ACTION_STATS_IDX12A_BITS             =  20; #

sub FM_L2AR_ACTION_STATS_IDX12A_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}
our $FM_L2AR_ACTION_STATS_IDX12A_STRIDE           =  1; #


sub FM_L2AR_ACTION_STATS_IDX12B {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06C00 + $FM_L2AR_BASE); # 0x146C00 L2AR_BASE
}

sub FM_L2AR_ACTION_STATS_IDX12B_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_ACTION_STATS_IDX12B_WIDTH            =  1; #
our $FM_L2AR_ACTION_STATS_IDX12B_BITS             =  20; #

sub FM_L2AR_ACTION_STATS_IDX12B_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}
our $FM_L2AR_ACTION_STATS_IDX12B_STRIDE           =  1; #


sub FM_L2AR_ACTION_STATS_IDX16A {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06C80 + $FM_L2AR_BASE); # 0x146C80 L2AR_BASE
}

sub FM_L2AR_ACTION_STATS_IDX16A_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_ACTION_STATS_IDX16A_WIDTH            =  1; #
our $FM_L2AR_ACTION_STATS_IDX16A_BITS             =  20; #

sub FM_L2AR_ACTION_STATS_IDX16A_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}
our $FM_L2AR_ACTION_STATS_IDX16A_STRIDE           =  1; #


sub FM_L2AR_ACTION_STATS_IDX16B {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06D00 + $FM_L2AR_BASE); # 0x146D00 L2AR_BASE
}

sub FM_L2AR_ACTION_STATS_IDX16B_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_ACTION_STATS_IDX16B_WIDTH            =  1; #
our $FM_L2AR_ACTION_STATS_IDX16B_BITS             =  20; #

sub FM_L2AR_ACTION_STATS_IDX16B_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 128;
}
our $FM_L2AR_ACTION_STATS_IDX16B_STRIDE           =  1; #


sub FM_L2AR_DGLORT_PROFILE_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06D80 + $FM_L2AR_BASE); # 0x146D80 L2AR_BASE
}

sub FM_L2AR_DGLORT_PROFILE_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_DGLORT_PROFILE_TABLE_WIDTH           =  1; #
our $FM_L2AR_DGLORT_PROFILE_TABLE_BITS            =  32; #

sub FM_L2AR_DGLORT_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L2AR_DGLORT_PROFILE_TABLE_STRIDE          =  1; #


sub FM_L2AR_W16AB_PROFILE_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x06DC0 + $FM_L2AR_BASE + ($word)); # 0x146DC0 L2AR_BASE
}

sub FM_L2AR_W16AB_PROFILE_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L2AR_W16AB_PROFILE_TABLE_WIDTH            =  2; #
our $FM_L2AR_W16AB_PROFILE_TABLE_BITS             =  37; #

sub FM_L2AR_W16AB_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L2AR_W16AB_PROFILE_TABLE_STRIDE           =  2; #


sub FM_L2AR_W16CDEF_PROFILE_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x06E00 + $FM_L2AR_BASE + ($word)); # 0x146E00 L2AR_BASE
}

sub FM_L2AR_W16CDEF_PROFILE_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_L2AR_W16CDEF_PROFILE_TABLE_WIDTH          =  3; #
our $FM_L2AR_W16CDEF_PROFILE_TABLE_BITS           =  76; #

sub FM_L2AR_W16CDEF_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L2AR_W16CDEF_PROFILE_TABLE_STRIDE         =  4; #


sub FM_L2AR_W8ABCDE_PROFILE_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x06E80 + $FM_L2AR_BASE + ($word)); # 0x146E80 L2AR_BASE
}

sub FM_L2AR_W8ABCDE_PROFILE_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_L2AR_W8ABCDE_PROFILE_TABLE_WIDTH          =  3; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_BITS           =  94; #

sub FM_L2AR_W8ABCDE_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_STRIDE         =  4; #


sub FM_L2AR_W4_PROFILE_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06F00 + $FM_L2AR_BASE); # 0x146F00 L2AR_BASE
}

sub FM_L2AR_W4_PROFILE_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_W4_PROFILE_TABLE_WIDTH               =  1; #
our $FM_L2AR_W4_PROFILE_TABLE_BITS                =  25; #

sub FM_L2AR_W4_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_L2AR_W4_PROFILE_TABLE_STRIDE              =  1; #


sub FM_L2AR_VID_PROFILE_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06F20 + $FM_L2AR_BASE); # 0x146F20 L2AR_BASE
}

sub FM_L2AR_VID_PROFILE_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_VID_PROFILE_TABLE_WIDTH              =  1; #
our $FM_L2AR_VID_PROFILE_TABLE_BITS               =  28; #

sub FM_L2AR_VID_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L2AR_VID_PROFILE_TABLE_STRIDE             =  1; #


sub FM_L2AR_DMASK_IDX_PROFILE_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x06F40 + $FM_L2AR_BASE); # 0x146F40 L2AR_BASE
}

sub FM_L2AR_DMASK_IDX_PROFILE_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_DMASK_IDX_PROFILE_TABLE_WIDTH        =  1; #
our $FM_L2AR_DMASK_IDX_PROFILE_TABLE_BITS         =  14; #

sub FM_L2AR_DMASK_IDX_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L2AR_DMASK_IDX_PROFILE_TABLE_STRIDE       =  1; #


sub FM_L2AR_QOS_PROFILE_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x06F80 + $FM_L2AR_BASE + ($word)); # 0x146F80 L2AR_BASE
}

sub FM_L2AR_QOS_PROFILE_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L2AR_QOS_PROFILE_TABLE_WIDTH              =  2; #
our $FM_L2AR_QOS_PROFILE_TABLE_BITS               =  59; #

sub FM_L2AR_QOS_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L2AR_QOS_PROFILE_TABLE_STRIDE             =  2; #


sub FM_L2AR_MA_WRITEBACK_PROFILE_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x07000 + $FM_L2AR_BASE + ($word)); # 0x147000 L2AR_BASE
}

sub FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_WIDTH     =  3; #
our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_BITS      =  80; #

sub FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_STRIDE    =  4; #


sub FM_L2AR_STATS_IDX5AB_PROFILE_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x07080 + $FM_L2AR_BASE); # 0x147080 L2AR_BASE
}

sub FM_L2AR_STATS_IDX5AB_PROFILE_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_STATS_IDX5AB_PROFILE_TABLE_WIDTH     =  1; #
our $FM_L2AR_STATS_IDX5AB_PROFILE_TABLE_BITS      =  18; #

sub FM_L2AR_STATS_IDX5AB_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L2AR_STATS_IDX5AB_PROFILE_TABLE_STRIDE    =  1; #


sub FM_L2AR_STATS_IDX5C_PROFILE_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x070A0 + $FM_L2AR_BASE); # 0x1470A0 L2AR_BASE
}

sub FM_L2AR_STATS_IDX5C_PROFILE_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_STATS_IDX5C_PROFILE_TABLE_WIDTH      =  1; #
our $FM_L2AR_STATS_IDX5C_PROFILE_TABLE_BITS       =  9; #

sub FM_L2AR_STATS_IDX5C_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L2AR_STATS_IDX5C_PROFILE_TABLE_STRIDE     =  1; #


sub FM_L2AR_STATS_IDX12A_PROFILE_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x070C0 + $FM_L2AR_BASE); # 0x1470C0 L2AR_BASE
}

sub FM_L2AR_STATS_IDX12A_PROFILE_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_STATS_IDX12A_PROFILE_TABLE_WIDTH     =  1; #
our $FM_L2AR_STATS_IDX12A_PROFILE_TABLE_BITS      =  17; #

sub FM_L2AR_STATS_IDX12A_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L2AR_STATS_IDX12A_PROFILE_TABLE_STRIDE    =  1; #


sub FM_L2AR_STATS_IDX12B_PROFILE_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x070E0 + $FM_L2AR_BASE); # 0x1470E0 L2AR_BASE
}

sub FM_L2AR_STATS_IDX12B_PROFILE_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_STATS_IDX12B_PROFILE_TABLE_WIDTH     =  1; #
our $FM_L2AR_STATS_IDX12B_PROFILE_TABLE_BITS      =  17; #

sub FM_L2AR_STATS_IDX12B_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_L2AR_STATS_IDX12B_PROFILE_TABLE_STRIDE    =  1; #


sub FM_L2AR_STATS_IDX16A_PROFILE_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x07100 + $FM_L2AR_BASE); # 0x147100 L2AR_BASE
}

sub FM_L2AR_STATS_IDX16A_PROFILE_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_STATS_IDX16A_PROFILE_TABLE_WIDTH     =  1; #
our $FM_L2AR_STATS_IDX16A_PROFILE_TABLE_BITS      =  16; #

sub FM_L2AR_STATS_IDX16A_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_L2AR_STATS_IDX16A_PROFILE_TABLE_STRIDE    =  1; #


sub FM_L2AR_STATS_IDX16B_PROFILE_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x07110 + $FM_L2AR_BASE); # 0x147110 L2AR_BASE
}

sub FM_L2AR_STATS_IDX16B_PROFILE_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_STATS_IDX16B_PROFILE_TABLE_WIDTH     =  1; #
our $FM_L2AR_STATS_IDX16B_PROFILE_TABLE_BITS      =  16; #

sub FM_L2AR_STATS_IDX16B_PROFILE_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_L2AR_STATS_IDX16B_PROFILE_TABLE_STRIDE    =  1; #


sub FM_L2AR_TRAP_HEADER_RULE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x07120 + $FM_L2AR_BASE); # 0x147120 L2AR_BASE
}

sub FM_L2AR_TRAP_HEADER_RULE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_L2AR_TRAP_HEADER_RULE_WIDTH               =  1; #
our $FM_L2AR_TRAP_HEADER_RULE_BITS                =  12; #

sub FM_L2AR_TRAP_HEADER_RULE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
our $FM_L2AR_TRAP_HEADER_RULE_STRIDE              =  1; #


sub FM_L2AR_TRAP_HEADER_DATA {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x07128 + $FM_L2AR_BASE + ($word)); # 0x147128 L2AR_BASE
}

sub FM_L2AR_TRAP_HEADER_DATA_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_L2AR_TRAP_HEADER_DATA_WIDTH               =  4; #
our $FM_L2AR_TRAP_HEADER_DATA_BITS                =  110; #

sub FM_L2AR_TRAP_HEADER_DATA_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2;
}
our $FM_L2AR_TRAP_HEADER_DATA_STRIDE              =  4; #


sub FM_L2AR_IP {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x07130 + $FM_L2AR_BASE + ($word)); # 0x147130 L2AR_BASE
}

sub FM_L2AR_IP_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L2AR_IP_WIDTH                             =  2; #
our $FM_L2AR_IP_BITS                              =  64; #

sub FM_L2AR_IP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}
our $FM_L2AR_IP_STRIDE                            =  2; #


sub FM_L2AR_IM {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x07140 + $FM_L2AR_BASE + ($word)); # 0x147140 L2AR_BASE
}

sub FM_L2AR_IM_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_L2AR_IM_WIDTH                             =  2; #
our $FM_L2AR_IM_BITS                              =  64; #

sub FM_L2AR_IM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}
our $FM_L2AR_IM_STRIDE                            =  2; #


sub FM_MOD_L2_VLAN1_TX_TAGGED {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00000 + $FM_MOD_BASE + ($word)); # 0x150000 MOD_BASE
}

sub FM_MOD_L2_VLAN1_TX_TAGGED_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_MOD_L2_VLAN1_TX_TAGGED_WIDTH              =  3; #
our $FM_MOD_L2_VLAN1_TX_TAGGED_BITS               =  76; #

sub FM_MOD_L2_VLAN1_TX_TAGGED_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}
our $FM_MOD_L2_VLAN1_TX_TAGGED_STRIDE             =  4; #


sub FM_MOD_MAP_DATA_V2T {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x04000 + $FM_MOD_BASE + ($word)); # 0x154000 MOD_BASE
}

sub FM_MOD_MAP_DATA_V2T_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_MOD_MAP_DATA_V2T_WIDTH                    =  3; #
our $FM_MOD_MAP_DATA_V2T_BITS                     =  72; #

sub FM_MOD_MAP_DATA_V2T_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}
our $FM_MOD_MAP_DATA_V2T_STRIDE                   =  4; #


sub FM_MOD_CAM {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00080 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x08000 + $FM_MOD_BASE + ($word)); # 0x158000 MOD_BASE
}

sub FM_MOD_CAM_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_MOD_CAM_WIDTH                             =  4; #
our $FM_MOD_CAM_BITS                              =  112; #

sub FM_MOD_CAM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
sub FM_MOD_CAM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_MOD_CAM_STRIDE_0                          =  4; #
our $FM_MOD_CAM_STRIDE_1                          =  128; #


sub FM_MOD_COMMAND_RAM {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00020 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x09000 + $FM_MOD_BASE); # 0x159000 MOD_BASE
}

sub FM_MOD_COMMAND_RAM_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_MOD_COMMAND_RAM_WIDTH                     =  1; #
our $FM_MOD_COMMAND_RAM_BITS                      =  15; #

sub FM_MOD_COMMAND_RAM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
sub FM_MOD_COMMAND_RAM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 20;
}
our $FM_MOD_COMMAND_RAM_STRIDE_0                  =  1; #
our $FM_MOD_COMMAND_RAM_STRIDE_1                  =  32; #


sub FM_MOD_VALUE_RAM {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00040 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x09400 + $FM_MOD_BASE + ($word)); # 0x159400 MOD_BASE
}

sub FM_MOD_VALUE_RAM_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_MOD_VALUE_RAM_WIDTH                       =  2; #
our $FM_MOD_VALUE_RAM_BITS                        =  64; #

sub FM_MOD_VALUE_RAM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
sub FM_MOD_VALUE_RAM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 15;
}
our $FM_MOD_VALUE_RAM_STRIDE_0                    =  2; #
our $FM_MOD_VALUE_RAM_STRIDE_1                    =  64; #


sub FM_MOD_MAP_IDX12A {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0A000 + $FM_MOD_BASE); # 0x15A000 MOD_BASE
}

sub FM_MOD_MAP_IDX12A_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MOD_MAP_IDX12A_WIDTH                      =  1; #
our $FM_MOD_MAP_IDX12A_BITS                       =  12; #

sub FM_MOD_MAP_IDX12A_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}
our $FM_MOD_MAP_IDX12A_STRIDE                     =  1; #


sub FM_MOD_MAP_DATA_W16A {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0B000 + $FM_MOD_BASE); # 0x15B000 MOD_BASE
}

sub FM_MOD_MAP_DATA_W16A_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MOD_MAP_DATA_W16A_WIDTH                   =  1; #
our $FM_MOD_MAP_DATA_W16A_BITS                    =  16; #

sub FM_MOD_MAP_DATA_W16A_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}
our $FM_MOD_MAP_DATA_W16A_STRIDE                  =  1; #


sub FM_MOD_MAP_DATA_W16B {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0C000 + $FM_MOD_BASE); # 0x15C000 MOD_BASE
}

sub FM_MOD_MAP_DATA_W16B_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MOD_MAP_DATA_W16B_WIDTH                   =  1; #
our $FM_MOD_MAP_DATA_W16B_BITS                    =  16; #

sub FM_MOD_MAP_DATA_W16B_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}
our $FM_MOD_MAP_DATA_W16B_STRIDE                  =  1; #


sub FM_MOD_MAP_DATA_W16C {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0D000 + $FM_MOD_BASE); # 0x15D000 MOD_BASE
}

sub FM_MOD_MAP_DATA_W16C_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MOD_MAP_DATA_W16C_WIDTH                   =  1; #
our $FM_MOD_MAP_DATA_W16C_BITS                    =  16; #

sub FM_MOD_MAP_DATA_W16C_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}
our $FM_MOD_MAP_DATA_W16C_STRIDE                  =  1; #


sub FM_MOD_MAP_DATA_W16D {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0E000 + $FM_MOD_BASE); # 0x15E000 MOD_BASE
}

sub FM_MOD_MAP_DATA_W16D_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MOD_MAP_DATA_W16D_WIDTH                   =  1; #
our $FM_MOD_MAP_DATA_W16D_BITS                    =  16; #

sub FM_MOD_MAP_DATA_W16D_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}
our $FM_MOD_MAP_DATA_W16D_STRIDE                  =  1; #


sub FM_MOD_MAP_DATA_W16E {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0F000 + $FM_MOD_BASE); # 0x15F000 MOD_BASE
}

sub FM_MOD_MAP_DATA_W16E_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MOD_MAP_DATA_W16E_WIDTH                   =  1; #
our $FM_MOD_MAP_DATA_W16E_BITS                    =  16; #

sub FM_MOD_MAP_DATA_W16E_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_MOD_MAP_DATA_W16E_STRIDE                  =  1; #


sub FM_MOD_MAP_DATA_W16F {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0F080 + $FM_MOD_BASE); # 0x15F080 MOD_BASE
}

sub FM_MOD_MAP_DATA_W16F_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MOD_MAP_DATA_W16F_WIDTH                   =  1; #
our $FM_MOD_MAP_DATA_W16F_BITS                    =  16; #

sub FM_MOD_MAP_DATA_W16F_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_MOD_MAP_DATA_W16F_STRIDE                  =  1; #


sub FM_MOD_MAP_DATA_W12A {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0F100 + $FM_MOD_BASE); # 0x15F100 MOD_BASE
}

sub FM_MOD_MAP_DATA_W12A_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MOD_MAP_DATA_W12A_WIDTH                   =  1; #
our $FM_MOD_MAP_DATA_W12A_BITS                    =  12; #

sub FM_MOD_MAP_DATA_W12A_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_MOD_MAP_DATA_W12A_STRIDE                  =  1; #


sub FM_MOD_MAP_DATA_W8A {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0F180 + $FM_MOD_BASE); # 0x15F180 MOD_BASE
}

sub FM_MOD_MAP_DATA_W8A_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MOD_MAP_DATA_W8A_WIDTH                    =  1; #
our $FM_MOD_MAP_DATA_W8A_BITS                     =  8; #

sub FM_MOD_MAP_DATA_W8A_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_MOD_MAP_DATA_W8A_STRIDE                   =  1; #


sub FM_MOD_MAP_DATA_W8B {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0F200 + $FM_MOD_BASE); # 0x15F200 MOD_BASE
}

sub FM_MOD_MAP_DATA_W8B_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MOD_MAP_DATA_W8B_WIDTH                    =  1; #
our $FM_MOD_MAP_DATA_W8B_BITS                     =  8; #

sub FM_MOD_MAP_DATA_W8B_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_MOD_MAP_DATA_W8B_STRIDE                   =  1; #


sub FM_MOD_TX_PORT_TAG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0F280 + $FM_MOD_BASE); # 0x15F280 MOD_BASE
}

sub FM_MOD_TX_PORT_TAG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MOD_TX_PORT_TAG_WIDTH                     =  1; #
our $FM_MOD_TX_PORT_TAG_BITS                      =  4; #

sub FM_MOD_TX_PORT_TAG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_MOD_TX_PORT_TAG_STRIDE                    =  1; #


sub FM_MOD_DST_PORT_TAG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0F300 + $FM_MOD_BASE); # 0x15F300 MOD_BASE
}

sub FM_MOD_DST_PORT_TAG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MOD_DST_PORT_TAG_WIDTH                    =  1; #
our $FM_MOD_DST_PORT_TAG_BITS                     =  10; #

sub FM_MOD_DST_PORT_TAG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_MOD_DST_PORT_TAG_STRIDE                   =  1; #


sub FM_MOD_L2_VPRI1_TX_MAP {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x0F400 + $FM_MOD_BASE + ($word)); # 0x15F400 MOD_BASE
}

sub FM_MOD_L2_VPRI1_TX_MAP_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x76543210;}
    if($word == 1) {return 0xfedcba98;}
}

our $FM_MOD_L2_VPRI1_TX_MAP_WIDTH                 =  2; #
our $FM_MOD_L2_VPRI1_TX_MAP_BITS                  =  64; #

sub FM_MOD_L2_VPRI1_TX_MAP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_MOD_L2_VPRI1_TX_MAP_STRIDE                =  2; #


sub FM_MOD_L2_VPRI2_TX_MAP {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x0F500 + $FM_MOD_BASE + ($word)); # 0x15F500 MOD_BASE
}

sub FM_MOD_L2_VPRI2_TX_MAP_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x76543210;}
    if($word == 1) {return 0xfedcba98;}
}

our $FM_MOD_L2_VPRI2_TX_MAP_WIDTH                 =  2; #
our $FM_MOD_L2_VPRI2_TX_MAP_BITS                  =  64; #

sub FM_MOD_L2_VPRI2_TX_MAP_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_MOD_L2_VPRI2_TX_MAP_STRIDE                =  2; #


sub FM_MOD_TX_PAUSE_QUANTA {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0F600 + $FM_MOD_BASE); # 0x15F600 MOD_BASE
}

sub FM_MOD_TX_PAUSE_QUANTA_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0xffffffff;
}

our $FM_MOD_TX_PAUSE_QUANTA_WIDTH                 =  1; #
our $FM_MOD_TX_PAUSE_QUANTA_BITS                  =  32; #

sub FM_MOD_TX_PAUSE_QUANTA_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_MOD_TX_PAUSE_QUANTA_STRIDE                =  1; #


sub FM_MOD_MIN_LENGTH {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0F680 + $FM_MOD_BASE); # 0x15F680 MOD_BASE
}

sub FM_MOD_MIN_LENGTH_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000040;
}

our $FM_MOD_MIN_LENGTH_WIDTH                      =  1; #
our $FM_MOD_MIN_LENGTH_BITS                       =  8; #

sub FM_MOD_MIN_LENGTH_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_MOD_MIN_LENGTH_STRIDE                     =  1; #


sub FM_MCAST_LOOPBACK_SUPPRESS {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x0F700 + $FM_MOD_BASE); # 0x15F700 MOD_BASE
}

sub FM_MCAST_LOOPBACK_SUPPRESS_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x0000ffff;
}

our $FM_MCAST_LOOPBACK_SUPPRESS_WIDTH             =  1; #
our $FM_MCAST_LOOPBACK_SUPPRESS_BITS              =  32; #

sub FM_MCAST_LOOPBACK_SUPPRESS_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 76;
}
our $FM_MCAST_LOOPBACK_SUPPRESS_STRIDE            =  1; #


sub FM_MOD_MAP_DATA_CTRL {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0F800 + $FM_MOD_BASE); # 0x15F800 MOD_BASE
}

sub FM_MOD_MAP_DATA_CTRL_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MOD_MAP_DATA_CTRL_WIDTH                   =  1; #
our $FM_MOD_MAP_DATA_CTRL_BITS                    =  22; #



sub FM_MOD_MAP_DATA_V2T_CTRL {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0F801 + $FM_MOD_BASE); # 0x15F801 MOD_BASE
}

sub FM_MOD_MAP_DATA_V2T_CTRL_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000006;
}

our $FM_MOD_MAP_DATA_V2T_CTRL_WIDTH               =  1; #
our $FM_MOD_MAP_DATA_V2T_CTRL_BITS                =  4; #



sub FM_MOD_TX_MIRROR_SRC {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0F802 + $FM_MOD_BASE); # 0x15F802 MOD_BASE
}

sub FM_MOD_TX_MIRROR_SRC_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MOD_TX_MIRROR_SRC_WIDTH                   =  1; #
our $FM_MOD_TX_MIRROR_SRC_BITS                    =  28; #



sub FM_MOD_TRANSMIT_MODE {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x0F803 + $FM_MOD_BASE); # 0x15F803 MOD_BASE
}

sub FM_MOD_TRANSMIT_MODE_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MOD_TRANSMIT_MODE_WIDTH                   =  1; #
our $FM_MOD_TRANSMIT_MODE_BITS                    =  19; #



sub FM_NEXTHOP_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0x00000 + $FM_NEXTHOP_BASE + ($word)); # 0x160000 NEXTHOP_BASE
}

sub FM_NEXTHOP_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_NEXTHOP_TABLE_WIDTH                       =  2; #
our $FM_NEXTHOP_TABLE_BITS                        =  64; #

sub FM_NEXTHOP_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 65536;
}
our $FM_NEXTHOP_TABLE_STRIDE                      =  2; #


sub FM_L2F_TABLE_4K {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x04000 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x00000 + $FM_L2F_BASE + ($word)); # 0x180000 L2F_BASE
}

sub FM_L2F_TABLE_4K_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_L2F_TABLE_4K_WIDTH                        =  3; #
our $FM_L2F_TABLE_4K_BITS                         =  76; #

sub FM_L2F_TABLE_4K_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}
sub FM_L2F_TABLE_4K_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 8;
}
our $FM_L2F_TABLE_4K_STRIDE_0                     =  4; #
our $FM_L2F_TABLE_4K_STRIDE_1                     =  16384; #


sub FM_L2F_TABLE_256 {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x00400 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x20000 + $FM_L2F_BASE + ($word)); # 0x1A0000 L2F_BASE
}

sub FM_L2F_TABLE_256_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_L2F_TABLE_256_WIDTH                       =  3; #
our $FM_L2F_TABLE_256_BITS                        =  76; #

sub FM_L2F_TABLE_256_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 256;
}
sub FM_L2F_TABLE_256_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_L2F_TABLE_256_STRIDE_0                    =  4; #
our $FM_L2F_TABLE_256_STRIDE_1                    =  1024; #


sub FM_L2F_PROFILE_TABLE {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00010 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x21000 + $FM_L2F_BASE); # 0x1A1000 L2F_BASE
}

sub FM_L2F_PROFILE_TABLE_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_L2F_PROFILE_TABLE_WIDTH                   =  1; #
our $FM_L2F_PROFILE_TABLE_BITS                    =  24; #

sub FM_L2F_PROFILE_TABLE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
sub FM_L2F_PROFILE_TABLE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
our $FM_L2F_PROFILE_TABLE_STRIDE_0                =  1; #
our $FM_L2F_PROFILE_TABLE_STRIDE_1                =  16; #


sub FM_STATS_BANK_COUNTER {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x01000 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x00000 + $FM_STATS_BANK_BASE + ($word)); # 0x200000 STATS_BANK_BASE
}

sub FM_STATS_BANK_COUNTER_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_STATS_BANK_COUNTER_WIDTH                  =  2; #
our $FM_STATS_BANK_COUNTER_BITS                   =  64; #

sub FM_STATS_BANK_COUNTER_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 2048;
}
sub FM_STATS_BANK_COUNTER_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_STATS_BANK_COUNTER_STRIDE_0               =  2; #
our $FM_STATS_BANK_COUNTER_STRIDE_1               =  4096; #


sub FM_MCAST_DEST_TABLE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0x00000 + $FM_MCAST_MID_BASE + ($word)); # 0x240000 MCAST_MID_BASE
}

sub FM_MCAST_DEST_TABLE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
}

our $FM_MCAST_DEST_TABLE_WIDTH                    =  3; #
our $FM_MCAST_DEST_TABLE_BITS                     =  92; #

sub FM_MCAST_DEST_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}
our $FM_MCAST_DEST_TABLE_STRIDE                   =  4; #


sub FM_MCAST_TX_MIRROR_DEST {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x04000 + $FM_MCAST_MID_BASE); # 0x244000 MCAST_MID_BASE
}

sub FM_MCAST_TX_MIRROR_DEST_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MCAST_TX_MIRROR_DEST_WIDTH                =  1; #
our $FM_MCAST_TX_MIRROR_DEST_BITS                 =  32; #

sub FM_MCAST_TX_MIRROR_DEST_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 10;
}
our $FM_MCAST_TX_MIRROR_DEST_STRIDE               =  1; #


sub FM_MCAST_MIRROR_CFG {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x04010 + $FM_MCAST_MID_BASE); # 0x244010 MCAST_MID_BASE
}

sub FM_MCAST_MIRROR_CFG_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MCAST_MIRROR_CFG_WIDTH                    =  1; #
our $FM_MCAST_MIRROR_CFG_BITS                     =  19; #

sub FM_MCAST_MIRROR_CFG_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_MCAST_MIRROR_CFG_STRIDE                   =  1; #


sub FM_MCAST_TX_TRUNC_MASK {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x04020 + $FM_MCAST_MID_BASE); # 0x244020 MCAST_MID_BASE
}

sub FM_MCAST_TX_TRUNC_MASK_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MCAST_TX_TRUNC_MASK_WIDTH                 =  1; #
our $FM_MCAST_TX_TRUNC_MASK_BITS                  =  8; #

sub FM_MCAST_TX_TRUNC_MASK_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 10;
}
our $FM_MCAST_TX_TRUNC_MASK_STRIDE                =  1; #


sub FM_MCAST_PRIVATE_WM {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x04030 + $FM_MCAST_MID_BASE); # 0x244030 MCAST_MID_BASE
}

sub FM_MCAST_PRIVATE_WM_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MCAST_PRIVATE_WM_WIDTH                    =  1; #
our $FM_MCAST_PRIVATE_WM_BITS                     =  14; #

sub FM_MCAST_PRIVATE_WM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 12;
}
our $FM_MCAST_PRIVATE_WM_STRIDE                   =  1; #


sub FM_MCAST_HOG_WM {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x04040 + $FM_MCAST_MID_BASE); # 0x244040 MCAST_MID_BASE
}

sub FM_MCAST_HOG_WM_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00003fff;
}

our $FM_MCAST_HOG_WM_WIDTH                        =  1; #
our $FM_MCAST_HOG_WM_BITS                         =  14; #



sub FM_MCAST_LIMITED_SKEW_MULTICAST {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0x04041 + $FM_MCAST_MID_BASE); # 0x244041 MCAST_MID_BASE
}

sub FM_MCAST_LIMITED_SKEW_MULTICAST_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_MCAST_LIMITED_SKEW_MULTICAST_WIDTH        =  1; #
our $FM_MCAST_LIMITED_SKEW_MULTICAST_BITS         =  12; #



sub FM_MCAST_VLAN_TABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0x00000 + $FM_MCAST_POST_BASE); # 0x260000 MCAST_POST_BASE
}

sub FM_MCAST_VLAN_TABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_MCAST_VLAN_TABLE_WIDTH                    =  1; #
our $FM_MCAST_VLAN_TABLE_BITS                     =  16; #

sub FM_MCAST_VLAN_TABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32768;
}
our $FM_MCAST_VLAN_TABLE_STRIDE                   =  1; #


sub FM_L2L_MAC_TABLE {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x04000 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x00000 + $FM_L2L_MAC_BASE + ($word)); # 0x280000 L2L_MAC_BASE
}

sub FM_L2L_MAC_TABLE_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_L2L_MAC_TABLE_WIDTH                       =  4; #
our $FM_L2L_MAC_TABLE_BITS                        =  117; #

sub FM_L2L_MAC_TABLE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}
sub FM_L2L_MAC_TABLE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_L2L_MAC_TABLE_STRIDE_0                    =  4; #
our $FM_L2L_MAC_TABLE_STRIDE_1                    =  16384; #


sub FM_L2L_MAC_TABLE_SWEEPER {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x04000 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x40000 + $FM_L2L_MAC_BASE + ($word)); # 0x2C0000 L2L_MAC_BASE
}

sub FM_L2L_MAC_TABLE_SWEEPER_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_L2L_MAC_TABLE_SWEEPER_WIDTH               =  4; #
our $FM_L2L_MAC_TABLE_SWEEPER_BITS                =  117; #

sub FM_L2L_MAC_TABLE_SWEEPER_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4096;
}
sub FM_L2L_MAC_TABLE_SWEEPER_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_L2L_MAC_TABLE_SWEEPER_STRIDE_0            =  4; #
our $FM_L2L_MAC_TABLE_SWEEPER_STRIDE_1            =  16384; #


sub FM_FFU_BST_ACTION {
    my ($self, $index2, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 5, -1);

    return (0x10000 * (($index2) - 0) + 0x00800 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x00000 + $FM_FFU_BASE + ($word)); # 0x300000 FFU_BASE
}

sub FM_FFU_BST_ACTION_DEFAULT {
    my ($self, $index2, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 5, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_FFU_BST_ACTION_WIDTH                      =  2; #
our $FM_FFU_BST_ACTION_BITS                       =  50; #

sub FM_FFU_BST_ACTION_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 1024;
}
sub FM_FFU_BST_ACTION_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
sub FM_FFU_BST_ACTION_ENTRIES_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_FFU_BST_ACTION_STRIDE_0                   =  2; #
our $FM_FFU_BST_ACTION_STRIDE_1                   =  2048; #
our $FM_FFU_BST_ACTION_STRIDE_2                   =  65536; #


sub FM_FFU_BST_KEY {
    my ($self, $index2, $index1, $index0) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x10000 * (($index2) - 0) + 0x00400 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x08000 + $FM_FFU_BASE); # 0x308000 FFU_BASE
}

sub FM_FFU_BST_KEY_DEFAULT {
    my ($self, $index2, $index1, $index0) = @_;
    _validatePrototype(@_, 4, -1);

    return 0x00000000;
}

our $FM_FFU_BST_KEY_WIDTH                         =  1; #
our $FM_FFU_BST_KEY_BITS                          =  32; #

sub FM_FFU_BST_KEY_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 1024;
}
sub FM_FFU_BST_KEY_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
sub FM_FFU_BST_KEY_ENTRIES_2 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_FFU_BST_KEY_STRIDE_0                      =  1; #
our $FM_FFU_BST_KEY_STRIDE_1                      =  1024; #
our $FM_FFU_BST_KEY_STRIDE_2                      =  65536; #


sub FM_FFU_BST_SCENARIO_CAM {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x10000 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x0C000 + $FM_FFU_BASE + ($word)); # 0x30C000 FFU_BASE
}

sub FM_FFU_BST_SCENARIO_CAM_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_FFU_BST_SCENARIO_CAM_WIDTH                =  2; #
our $FM_FFU_BST_SCENARIO_CAM_BITS                 =  64; #

sub FM_FFU_BST_SCENARIO_CAM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
sub FM_FFU_BST_SCENARIO_CAM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_FFU_BST_SCENARIO_CAM_STRIDE_0             =  2; #
our $FM_FFU_BST_SCENARIO_CAM_STRIDE_1             =  65536; #


sub FM_FFU_BST_SCENARIO_CFG1 {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x10000 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x0C040 + $FM_FFU_BASE); # 0x30C040 FFU_BASE
}

sub FM_FFU_BST_SCENARIO_CFG1_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_FFU_BST_SCENARIO_CFG1_WIDTH               =  1; #
our $FM_FFU_BST_SCENARIO_CFG1_BITS                =  32; #

sub FM_FFU_BST_SCENARIO_CFG1_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
sub FM_FFU_BST_SCENARIO_CFG1_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_FFU_BST_SCENARIO_CFG1_STRIDE_0            =  1; #
our $FM_FFU_BST_SCENARIO_CFG1_STRIDE_1            =  65536; #


sub FM_FFU_BST_SCENARIO_CFG2 {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x10000 * (($index1) - 0) + 0x00001 * (($index0) - 0) + 0x0C060 + $FM_FFU_BASE); # 0x30C060 FFU_BASE
}

sub FM_FFU_BST_SCENARIO_CFG2_DEFAULT {
    my ($self, $index1, $index0) = @_;
    _validatePrototype(@_, 3, -1);

    return 0x00000000;
}

our $FM_FFU_BST_SCENARIO_CFG2_WIDTH               =  1; #
our $FM_FFU_BST_SCENARIO_CFG2_BITS                =  29; #

sub FM_FFU_BST_SCENARIO_CFG2_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
sub FM_FFU_BST_SCENARIO_CFG2_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_FFU_BST_SCENARIO_CFG2_STRIDE_0            =  1; #
our $FM_FFU_BST_SCENARIO_CFG2_STRIDE_1            =  65536; #


sub FM_FFU_BST_ROOT_KEYS {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x10000 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x0C080 + $FM_FFU_BASE + ($word)); # 0x30C080 FFU_BASE
}

sub FM_FFU_BST_ROOT_KEYS_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_FFU_BST_ROOT_KEYS_WIDTH                   =  2; #
our $FM_FFU_BST_ROOT_KEYS_BITS                    =  44; #

sub FM_FFU_BST_ROOT_KEYS_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
sub FM_FFU_BST_ROOT_KEYS_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_FFU_BST_ROOT_KEYS_STRIDE_0                =  2; #
our $FM_FFU_BST_ROOT_KEYS_STRIDE_1                =  65536; #


sub FM_FFU_BST_MASTER_VALID {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x10000 * (($index) - 0) + 0x0C0A0 + $FM_FFU_BASE); # 0x30C0A0 FFU_BASE
}

sub FM_FFU_BST_MASTER_VALID_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_FFU_BST_MASTER_VALID_WIDTH                =  1; #
our $FM_FFU_BST_MASTER_VALID_BITS                 =  1; #

sub FM_FFU_BST_MASTER_VALID_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 4;
}
our $FM_FFU_BST_MASTER_VALID_STRIDE               =  65536; #


sub FM_FFU_SLICE_CAM {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x04000 * (($index1) - 0) + 0x00004 * (($index0) - 0) + 0x80000 + $FM_FFU_BASE + ($word)); # 0x380000 FFU_BASE
}

sub FM_FFU_SLICE_CAM_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_FFU_SLICE_CAM_WIDTH                       =  4; #
our $FM_FFU_SLICE_CAM_BITS                        =  102; #

sub FM_FFU_SLICE_CAM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 1024;
}
sub FM_FFU_SLICE_CAM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 24;
}
our $FM_FFU_SLICE_CAM_STRIDE_0                    =  4; #
our $FM_FFU_SLICE_CAM_STRIDE_1                    =  16384; #


sub FM_FFU_SLICE_ACTION_ROUTE {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x04000 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x81000 + $FM_FFU_BASE + ($word)); # 0x381000 FFU_BASE
}

sub FM_FFU_SLICE_ACTION_ROUTE_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_FFU_SLICE_ACTION_ROUTE_WIDTH              =  2; #
our $FM_FFU_SLICE_ACTION_ROUTE_BITS               =  44; #

sub FM_FFU_SLICE_ACTION_ROUTE_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 1024;
}
sub FM_FFU_SLICE_ACTION_ROUTE_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 24;
}
our $FM_FFU_SLICE_ACTION_ROUTE_STRIDE_0           =  2; #
our $FM_FFU_SLICE_ACTION_ROUTE_STRIDE_1           =  16384; #


sub FM_FFU_SLICE_SCENARIO_CAM {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x04000 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x81800 + $FM_FFU_BASE + ($word)); # 0x381800 FFU_BASE
}

sub FM_FFU_SLICE_SCENARIO_CAM_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_FFU_SLICE_SCENARIO_CAM_WIDTH              =  2; #
our $FM_FFU_SLICE_SCENARIO_CAM_BITS               =  64; #

sub FM_FFU_SLICE_SCENARIO_CAM_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
sub FM_FFU_SLICE_SCENARIO_CAM_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 24;
}
our $FM_FFU_SLICE_SCENARIO_CAM_STRIDE_0           =  2; #
our $FM_FFU_SLICE_SCENARIO_CAM_STRIDE_1           =  16384; #


sub FM_FFU_SLICE_SCENARIO_CFG {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    return (0x04000 * (($index1) - 0) + 0x00002 * (($index0) - 0) + 0x81840 + $FM_FFU_BASE + ($word)); # 0x381840 FFU_BASE
}

sub FM_FFU_SLICE_SCENARIO_CFG_DEFAULT {
    my ($self, $index1, $index0, $word) = @_;
    _validatePrototype(@_, 4, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_FFU_SLICE_SCENARIO_CFG_WIDTH              =  2; #
our $FM_FFU_SLICE_SCENARIO_CFG_BITS               =  38; #

sub FM_FFU_SLICE_SCENARIO_CFG_ENTRIES_0 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
sub FM_FFU_SLICE_SCENARIO_CFG_ENTRIES_1 {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 24;
}
our $FM_FFU_SLICE_SCENARIO_CFG_STRIDE_0           =  2; #
our $FM_FFU_SLICE_SCENARIO_CFG_STRIDE_1           =  16384; #


sub FM_FFU_SLICE_MASTER_VALID {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x04000 * (($index) - 0) + 0x81880 + $FM_FFU_BASE); # 0x381880 FFU_BASE
}

sub FM_FFU_SLICE_MASTER_VALID_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_FFU_SLICE_MASTER_VALID_WIDTH              =  1; #
our $FM_FFU_SLICE_MASTER_VALID_BITS               =  2; #

sub FM_FFU_SLICE_MASTER_VALID_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 24;
}
our $FM_FFU_SLICE_MASTER_VALID_STRIDE             =  16384; #


sub FM_FFU_ATOMIC_APPLY {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return (0xF0000 + $FM_FFU_BASE); # 0x3F0000 FFU_BASE
}

sub FM_FFU_ATOMIC_APPLY_DEFAULT {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 0x00000000;
}

our $FM_FFU_ATOMIC_APPLY_WIDTH                    =  1; #
our $FM_FFU_ATOMIC_APPLY_BITS                     =  2; #



sub FM_FFU_REMAP_SCENARIO_CAM {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00002 * (($index) - 0) + 0xF8000 + $FM_FFU_BASE + ($word)); # 0x3F8000 FFU_BASE
}

sub FM_FFU_REMAP_SCENARIO_CAM_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_FFU_REMAP_SCENARIO_CAM_WIDTH              =  2; #
our $FM_FFU_REMAP_SCENARIO_CAM_BITS               =  64; #

sub FM_FFU_REMAP_SCENARIO_CAM_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_FFU_REMAP_SCENARIO_CAM_STRIDE             =  2; #


sub FM_FFU_REMAP_PROFILE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0xF8040 + $FM_FFU_BASE); # 0x3F8040 FFU_BASE
}

sub FM_FFU_REMAP_PROFILE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x0c000040;
}

our $FM_FFU_REMAP_PROFILE_WIDTH                   =  1; #
our $FM_FFU_REMAP_PROFILE_BITS                    =  28; #

sub FM_FFU_REMAP_PROFILE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 32;
}
our $FM_FFU_REMAP_PROFILE_STRIDE                  =  1; #


sub FM_FFU_EACL_CFG {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    return (0xFC000 + $FM_FFU_BASE + ($word)); # 0x3FC000 FFU_BASE
}

sub FM_FFU_EACL_CFG_DEFAULT {
    my ($self, $word) = @_;
    _validatePrototype(@_, 2, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
}

our $FM_FFU_EACL_CFG_WIDTH                        =  2; #
our $FM_FFU_EACL_CFG_BITS                         =  64; #



sub FM_HASH_LAYER3_PROFILE {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    return (0x00004 * (($index) - 0) + 0xFC040 + $FM_FFU_BASE + ($word)); # 0x3FC040 FFU_BASE
}

sub FM_HASH_LAYER3_PROFILE_DEFAULT {
    my ($self, $index, $word) = @_;
    _validatePrototype(@_, 3, -1);

    if($word == 0) {return 0x00000000;}
    if($word == 1) {return 0x00000000;}
    if($word == 2) {return 0x00000000;}
    if($word == 3) {return 0x00000000;}
}

our $FM_HASH_LAYER3_PROFILE_WIDTH                 =  4; #
our $FM_HASH_LAYER3_PROFILE_BITS                  =  122; #

sub FM_HASH_LAYER3_PROFILE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 16;
}
our $FM_HASH_LAYER3_PROFILE_STRIDE                =  4; #


sub FM_HASH_LAYER3_PTABLE {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return (0x00001 * (($index) - 0) + 0xFC400 + $FM_FFU_BASE); # 0x3FC400 FFU_BASE
}

sub FM_HASH_LAYER3_PTABLE_DEFAULT {
    my ($self, $index) = @_;
    _validatePrototype(@_, 2, -1);

    return 0x00000000;
}

our $FM_HASH_LAYER3_PTABLE_WIDTH                  =  1; #
our $FM_HASH_LAYER3_PTABLE_BITS                   =  24; #

sub FM_HASH_LAYER3_PTABLE_ENTRIES {
    my ($self) = @_;
    _validatePrototype(@_, 1, -1);

    return 1024;
}
our $FM_HASH_LAYER3_PTABLE_STRIDE                 =  1; #


our $FM_AN_37_BASE_PAGE_TX_b_Pause                =  7; #
our $FM_AN_37_BASE_PAGE_TX_b_FullDuplex           =  5; #
our $FM_AN_37_BASE_PAGE_TX_l_RemoteFault          =  12; #
our $FM_AN_37_BASE_PAGE_TX_h_RemoteFault          =  13; #
our $FM_AN_37_BASE_PAGE_TX_b_NP                   =  15; #
our $FM_AN_37_BASE_PAGE_TX_b_ACK                  =  14; #
our $FM_AN_37_BASE_PAGE_TX_b_AsmDir               =  8; #
our $FM_AN_37_BASE_PAGE_TX_l_Rsrvd4to0            =  0; #
our $FM_AN_37_BASE_PAGE_TX_h_Rsrvd4to0            =  4; #
our $FM_AN_37_BASE_PAGE_TX_b_HalfDuplex           =  6; #
our $FM_AN_37_BASE_PAGE_TX_l_Rsrvd11to9           =  9; #
our $FM_AN_37_BASE_PAGE_TX_h_Rsrvd11to9           =  11; #

our $FM_AN_37_BASE_PAGE_RX_b_Pause                =  7; #
our $FM_AN_37_BASE_PAGE_RX_b_FullDuplex           =  5; #
our $FM_AN_37_BASE_PAGE_RX_l_RemoteFault          =  12; #
our $FM_AN_37_BASE_PAGE_RX_h_RemoteFault          =  13; #
our $FM_AN_37_BASE_PAGE_RX_b_NP                   =  15; #
our $FM_AN_37_BASE_PAGE_RX_b_ACK                  =  14; #
our $FM_AN_37_BASE_PAGE_RX_b_AsmDir               =  8; #
our $FM_AN_37_BASE_PAGE_RX_l_Rsrvd4to0            =  0; #
our $FM_AN_37_BASE_PAGE_RX_h_Rsrvd4to0            =  4; #
our $FM_AN_37_BASE_PAGE_RX_b_HalfDuplex           =  6; #
our $FM_AN_37_BASE_PAGE_RX_l_Rsrvd11to9           =  9; #
our $FM_AN_37_BASE_PAGE_RX_h_Rsrvd11to9           =  11; #

our $FM_MULTI_PURPOSE_ERROR_COUNTER_l_ErrorCnt    =  0; #
our $FM_MULTI_PURPOSE_ERROR_COUNTER_h_ErrorCnt    =  31; #

our $FM_AN_73_BASE_PAGE_TX_l_F                    =  46; #
our $FM_AN_73_BASE_PAGE_TX_h_F                    =  47; #
our $FM_AN_73_BASE_PAGE_TX_l_T                    =  16; #
our $FM_AN_73_BASE_PAGE_TX_h_T                    =  20; #
our $FM_AN_73_BASE_PAGE_TX_l_E                    =  5; #
our $FM_AN_73_BASE_PAGE_TX_h_E                    =  9; #
our $FM_AN_73_BASE_PAGE_TX_l_S                    =  0; #
our $FM_AN_73_BASE_PAGE_TX_h_S                    =  4; #
our $FM_AN_73_BASE_PAGE_TX_b_NP                   =  15; #
our $FM_AN_73_BASE_PAGE_TX_l_C                    =  10; #
our $FM_AN_73_BASE_PAGE_TX_h_C                    =  12; #
our $FM_AN_73_BASE_PAGE_TX_b_ACK                  =  14; #
our $FM_AN_73_BASE_PAGE_TX_l_A                    =  21; #
our $FM_AN_73_BASE_PAGE_TX_h_A                    =  45; #
our $FM_AN_73_BASE_PAGE_TX_b_RF                   =  13; #

our $FM_SGMII_AN_TX_CONFIG_l_Rsrvd9_1             =  1; #
our $FM_SGMII_AN_TX_CONFIG_h_Rsrvd9_1             =  9; #
our $FM_SGMII_AN_TX_CONFIG_b_B0                   =  0; #
our $FM_SGMII_AN_TX_CONFIG_b_Rsrvd15              =  15; #
our $FM_SGMII_AN_TX_CONFIG_b_OneB14               =  14; #
our $FM_SGMII_AN_TX_CONFIG_b_Rsrvd13              =  13; #
our $FM_SGMII_AN_TX_CONFIG_b_Rsrvd12              =  12; #
our $FM_SGMII_AN_TX_CONFIG_l_Rsrvd11to10          =  10; #
our $FM_SGMII_AN_TX_CONFIG_h_Rsrvd11to10          =  11; #

our $FM_AN_37_NEXT_PAGE_TX_b_MP                   =  13; #
our $FM_AN_37_NEXT_PAGE_TX_b_T                    =  11; #
our $FM_AN_37_NEXT_PAGE_TX_b_NP                   =  15; #
our $FM_AN_37_NEXT_PAGE_TX_b_ACK                  =  14; #
our $FM_AN_37_NEXT_PAGE_TX_b_ACK2                 =  12; #
our $FM_AN_37_NEXT_PAGE_TX_l_MU                   =  0; #
our $FM_AN_37_NEXT_PAGE_TX_h_MU                   =  10; #

our $FM_SGMII_AN_TIMER_CFG_l_TimeScale            =  0; #
our $FM_SGMII_AN_TIMER_CFG_h_TimeScale            =  3; #
our $FM_SGMII_AN_TIMER_CFG_l_LinkTimerTimeout     =  4; #
our $FM_SGMII_AN_TIMER_CFG_h_LinkTimerTimeout     =  10; #

our $FM_PCS_40GBASER_RX_BIP_STATUS_l_BipCnt       =  0; #
our $FM_PCS_40GBASER_RX_BIP_STATUS_h_BipCnt       =  31; #

our $FM_PCS_10GBASER_RX_BER_STATUS_l_BerCnt       =  0; #
our $FM_PCS_10GBASER_RX_BER_STATUS_h_BerCnt       =  31; #

our $FM_AN_37_TIMER_CFG_l_TimeScale               =  0; #
our $FM_AN_37_TIMER_CFG_h_TimeScale               =  3; #
our $FM_AN_37_TIMER_CFG_l_LinkTimerTimeout        =  4; #
our $FM_AN_37_TIMER_CFG_h_LinkTimerTimeout        =  10; #

our $FM_AN_73_PAGE_RX_STATUS_l_GeneralPurposeField  =  0; #
our $FM_AN_73_PAGE_RX_STATUS_h_GeneralPurposeField  =  47; #

our $FM_NEXTHOP_WIDE_l_NextHop                    =  0; #
our $FM_NEXTHOP_WIDE_h_NextHop                    =  127; #

our $FM_NEXTHOP_NARROW_UNICAST_l_W12A             =  48; #
our $FM_NEXTHOP_NARROW_UNICAST_h_W12A             =  59; #
our $FM_NEXTHOP_NARROW_UNICAST_l_W16A             =  0; #
our $FM_NEXTHOP_NARROW_UNICAST_h_W16A             =  15; #
our $FM_NEXTHOP_NARROW_UNICAST_l_TAG              =  60; #
our $FM_NEXTHOP_NARROW_UNICAST_h_TAG              =  63; #
our $FM_NEXTHOP_NARROW_UNICAST_l_W16B             =  16; #
our $FM_NEXTHOP_NARROW_UNICAST_h_W16B             =  31; #
our $FM_NEXTHOP_NARROW_UNICAST_l_W16C             =  32; #
our $FM_NEXTHOP_NARROW_UNICAST_h_W16C             =  47; #

our $FM_FFU_SLICE_ACTION_MISC_l_Parity            =  42; #
our $FM_FFU_SLICE_ACTION_MISC_h_Parity            =  43; #
our $FM_FFU_SLICE_ACTION_MISC_l_TagCmd            =  36; #
our $FM_FFU_SLICE_ACTION_MISC_h_TagCmd            =  37; #
our $FM_FFU_SLICE_ACTION_MISC_l_Precedence        =  39; #
our $FM_FFU_SLICE_ACTION_MISC_h_Precedence        =  41; #
our $FM_FFU_SLICE_ACTION_MISC_b_Route             =  38; #
our $FM_FFU_SLICE_ACTION_MISC_l_TagData           =  24; #
our $FM_FFU_SLICE_ACTION_MISC_h_TagData           =  35; #
our $FM_FFU_SLICE_ACTION_MISC_l_Command           =  20; #
our $FM_FFU_SLICE_ACTION_MISC_h_Command           =  22; #
our $FM_FFU_SLICE_ACTION_MISC_l_Data              =  0; #
our $FM_FFU_SLICE_ACTION_MISC_h_Data              =  19; #
our $FM_FFU_SLICE_ACTION_MISC_b_Switch            =  23; #

our $FM_FFU_SLICE_ACTION_l_Parity                 =  42; #
our $FM_FFU_SLICE_ACTION_h_Parity                 =  43; #
our $FM_FFU_SLICE_ACTION_l_TagCmd                 =  36; #
our $FM_FFU_SLICE_ACTION_h_TagCmd                 =  37; #
our $FM_FFU_SLICE_ACTION_l_Precedence             =  39; #
our $FM_FFU_SLICE_ACTION_h_Precedence             =  41; #
our $FM_FFU_SLICE_ACTION_b_Route                  =  38; #
our $FM_FFU_SLICE_ACTION_l_TagData                =  24; #
our $FM_FFU_SLICE_ACTION_h_TagData                =  35; #
our $FM_FFU_SLICE_ACTION_l_ActionData             =  0; #
our $FM_FFU_SLICE_ACTION_h_ActionData             =  23; #

our $FM_AN_37_NEXT_PAGE_RX_b_MP                   =  13; #
our $FM_AN_37_NEXT_PAGE_RX_b_T                    =  11; #
our $FM_AN_37_NEXT_PAGE_RX_b_NP                   =  15; #
our $FM_AN_37_NEXT_PAGE_RX_b_ACK                  =  14; #
our $FM_AN_37_NEXT_PAGE_RX_b_ACK2                 =  12; #
our $FM_AN_37_NEXT_PAGE_RX_l_MU                   =  0; #
our $FM_AN_37_NEXT_PAGE_RX_h_MU                   =  10; #

our $FM_FFU_BST_ACTION_ROUTE_b_NextHopEntryType   =  23; #
our $FM_FFU_BST_ACTION_ROUTE_l_LPM                =  24; #
our $FM_FFU_BST_ACTION_ROUTE_h_LPM                =  31; #
our $FM_FFU_BST_ACTION_ROUTE_l_TagCmd             =  44; #
our $FM_FFU_BST_ACTION_ROUTE_h_TagCmd             =  45; #
our $FM_FFU_BST_ACTION_ROUTE_l_NextHopBaseIndex   =  0; #
our $FM_FFU_BST_ACTION_ROUTE_h_NextHopBaseIndex   =  15; #
our $FM_FFU_BST_ACTION_ROUTE_l_Precedence         =  47; #
our $FM_FFU_BST_ACTION_ROUTE_h_Precedence         =  49; #
our $FM_FFU_BST_ACTION_ROUTE_b_Route              =  46; #
our $FM_FFU_BST_ACTION_ROUTE_l_TagData            =  32; #
our $FM_FFU_BST_ACTION_ROUTE_h_TagData            =  43; #
our $FM_FFU_BST_ACTION_ROUTE_l_NextHopRange       =  16; #
our $FM_FFU_BST_ACTION_ROUTE_h_NextHopRange       =  22; #

our $FM_AN_37_PAGE_RX_STATUS_l_GeneralPurposeField  =  0; #
our $FM_AN_37_PAGE_RX_STATUS_h_GeneralPurposeField  =  15; #

our $FM_AN_73_BASE_PAGE_RX_l_F                    =  46; #
our $FM_AN_73_BASE_PAGE_RX_h_F                    =  47; #
our $FM_AN_73_BASE_PAGE_RX_l_T                    =  16; #
our $FM_AN_73_BASE_PAGE_RX_h_T                    =  20; #
our $FM_AN_73_BASE_PAGE_RX_l_E                    =  5; #
our $FM_AN_73_BASE_PAGE_RX_h_E                    =  9; #
our $FM_AN_73_BASE_PAGE_RX_l_S                    =  0; #
our $FM_AN_73_BASE_PAGE_RX_h_S                    =  4; #
our $FM_AN_73_BASE_PAGE_RX_b_NP                   =  15; #
our $FM_AN_73_BASE_PAGE_RX_l_C                    =  10; #
our $FM_AN_73_BASE_PAGE_RX_h_C                    =  12; #
our $FM_AN_73_BASE_PAGE_RX_b_ACK                  =  14; #
our $FM_AN_73_BASE_PAGE_RX_l_A                    =  21; #
our $FM_AN_73_BASE_PAGE_RX_h_A                    =  45; #
our $FM_AN_73_BASE_PAGE_RX_b_RF                   =  13; #

our $FM_EPL_PORT_MULTI_PURPOSE_CFG_A_l_GeneralPurposeField  =  0; #
our $FM_EPL_PORT_MULTI_PURPOSE_CFG_A_h_GeneralPurposeField  =  47; #

our $FM_AN_73_NEXT_PAGE_TX_l_U                    =  16; #
our $FM_AN_73_NEXT_PAGE_TX_h_U                    =  47; #
our $FM_AN_73_NEXT_PAGE_TX_b_MP                   =  13; #
our $FM_AN_73_NEXT_PAGE_TX_b_T                    =  11; #
our $FM_AN_73_NEXT_PAGE_TX_b_NP                   =  15; #
our $FM_AN_73_NEXT_PAGE_TX_b_ACK                  =  14; #
our $FM_AN_73_NEXT_PAGE_TX_b_ACK2                 =  12; #
our $FM_AN_73_NEXT_PAGE_TX_l_MU                   =  0; #
our $FM_AN_73_NEXT_PAGE_TX_h_MU                   =  10; #

our $FM_FFU_BST_ACTION_MISC_l_LPM                 =  24; #
our $FM_FFU_BST_ACTION_MISC_h_LPM                 =  31; #
our $FM_FFU_BST_ACTION_MISC_l_TagCmd              =  44; #
our $FM_FFU_BST_ACTION_MISC_h_TagCmd              =  45; #
our $FM_FFU_BST_ACTION_MISC_l_Precedence          =  47; #
our $FM_FFU_BST_ACTION_MISC_h_Precedence          =  49; #
our $FM_FFU_BST_ACTION_MISC_b_Route               =  46; #
our $FM_FFU_BST_ACTION_MISC_l_TagData             =  32; #
our $FM_FFU_BST_ACTION_MISC_h_TagData             =  43; #
our $FM_FFU_BST_ACTION_MISC_l_Command             =  20; #
our $FM_FFU_BST_ACTION_MISC_h_Command             =  22; #
our $FM_FFU_BST_ACTION_MISC_l_Data                =  0; #
our $FM_FFU_BST_ACTION_MISC_h_Data                =  19; #
our $FM_FFU_BST_ACTION_MISC_b_Switch              =  23; #

our $FM_MOD_L2_VLAN2_TX_TAGGED_l_PortMask         =  0; #
our $FM_MOD_L2_VLAN2_TX_TAGGED_h_PortMask         =  75; #

our $FM_NEXTHOP_NARROW_MULTICAST_l_W12A           =  48; #
our $FM_NEXTHOP_NARROW_MULTICAST_h_W12A           =  59; #
our $FM_NEXTHOP_NARROW_MULTICAST_l_W4B            =  44; #
our $FM_NEXTHOP_NARROW_MULTICAST_h_W4B            =  47; #
our $FM_NEXTHOP_NARROW_MULTICAST_l_W4A            =  40; #
our $FM_NEXTHOP_NARROW_MULTICAST_h_W4A            =  43; #
our $FM_NEXTHOP_NARROW_MULTICAST_l_W16A           =  0; #
our $FM_NEXTHOP_NARROW_MULTICAST_h_W16A           =  15; #
our $FM_NEXTHOP_NARROW_MULTICAST_l_TAG            =  60; #
our $FM_NEXTHOP_NARROW_MULTICAST_h_TAG            =  63; #
our $FM_NEXTHOP_NARROW_MULTICAST_l_W8A            =  16; #
our $FM_NEXTHOP_NARROW_MULTICAST_h_W8A            =  23; #
our $FM_NEXTHOP_NARROW_MULTICAST_l_W8B            =  24; #
our $FM_NEXTHOP_NARROW_MULTICAST_h_W8B            =  31; #
our $FM_NEXTHOP_NARROW_MULTICAST_l_W8C            =  32; #
our $FM_NEXTHOP_NARROW_MULTICAST_h_W8C            =  39; #

our $FM_LCI_CFG_b_endianness                      =  0; #
our $FM_LCI_CFG_b_hostPadding                     =  1; #

our $FM_LCI_RX_FIFO_l_data                        =  0; #
our $FM_LCI_RX_FIFO_h_data                        =  31; #

our $FM_LCI_TX_FIFO_l_data                        =  0; #
our $FM_LCI_TX_FIFO_h_data                        =  31; #

our $FM_LCI_IP_b_newFrameRecv                     =  0; #
our $FM_LCI_IP_b_endOfFrameSend                   =  1; #

our $FM_LCI_IM_b_newFrameRecv                     =  0; #
our $FM_LCI_IM_b_endOfFrameSend                   =  1; #

our $FM_LCI_STATUS_b_rxEndOfFrame                 =  2; #
our $FM_LCI_STATUS_b_rxReady                      =  1; #
our $FM_LCI_STATUS_b_txReady                      =  0; #
our $FM_LCI_STATUS_l_pendingTXwords               =  3; #
our $FM_LCI_STATUS_h_pendingTXwords               =  14; #

our $FM_FATAL_CODE_l_FatalCode                    =  0; #
our $FM_FATAL_CODE_h_FatalCode                    =  6; #
our $FM_FATAL_CODE_b_FatalType                    =  7; #

our $FM_LAST_FATAL_CODE_l_FatalCode               =  0; #
our $FM_LAST_FATAL_CODE_h_FatalCode               =  7; #

our $FM_FATAL_COUNT_l_ResetCount                  =  0; #
our $FM_FATAL_COUNT_h_ResetCount                  =  7; #

our $FM_SOFT_RESET_b_FIBMReset                    =  2; #
our $FM_SOFT_RESET_b_JSSReset                     =  3; #
our $FM_SOFT_RESET_b_EPLReset                     =  4; #
our $FM_SOFT_RESET_b_PCIeReset                    =  0; #
our $FM_SOFT_RESET_b_MSBReset                     =  1; #

our $FM_RESET_CFG_l_MasterResetAssertCount        =  0; #
our $FM_RESET_CFG_h_MasterResetAssertCount        =  7; #
our $FM_RESET_CFG_l_MasterResetDelayCount         =  8; #
our $FM_RESET_CFG_h_MasterResetDelayCount         =  15; #

our $FM_WATCHDOG_CFG_b_FatalResetEnable           =  0; #

our $FM_MGMT_SCRATCH_l_Data                       =  0; #
our $FM_MGMT_SCRATCH_h_Data                       =  31; #

our $FM_VITAL_PRODUCT_DATA_l_PartNumber           =  0; #
our $FM_VITAL_PRODUCT_DATA_h_PartNumber           =  15; #

our $FM_PCI_CFG_ID_l_VendorID                     =  0; #
our $FM_PCI_CFG_ID_h_VendorID                     =  15; #
our $FM_PCI_CFG_ID_l_DeviceID                     =  16; #
our $FM_PCI_CFG_ID_h_DeviceID                     =  31; #

our $FM_PCI_CFG_CMD_b_DetectParErr                =  31; #
our $FM_PCI_CFG_CMD_b_RcvTgtAbort                 =  28; #
our $FM_PCI_CFG_CMD_b_INTAssertDis                =  10; #
our $FM_PCI_CFG_CMD_b_ParErrResp                  =  6; #
our $FM_PCI_CFG_CMD_b_MemWrInv                    =  4; #
our $FM_PCI_CFG_CMD_b_SERRnEn                     =  8; #
our $FM_PCI_CFG_CMD_b_MstrDataParErr              =  24; #
our $FM_PCI_CFG_CMD_b_INTStat                     =  19; #
our $FM_PCI_CFG_CMD_b_VGAPaletteSnp               =  5; #
our $FM_PCI_CFG_CMD_b_BusMasterEn                 =  2; #
our $FM_PCI_CFG_CMD_b_CapList                     =  20; #
our $FM_PCI_CFG_CMD_b_MemSpaceEn                  =  1; #
our $FM_PCI_CFG_CMD_b_SigTgtAbort                 =  27; #
our $FM_PCI_CFG_CMD_b_IdselStep                   =  7; #
our $FM_PCI_CFG_CMD_b_FastB2BEn                   =  9; #
our $FM_PCI_CFG_CMD_b_Cap66Mhz                    =  21; #
our $FM_PCI_CFG_CMD_b_RcvMstAbort                 =  29; #
our $FM_PCI_CFG_CMD_l_DevselTiming                =  25; #
our $FM_PCI_CFG_CMD_h_DevselTiming                =  26; #
our $FM_PCI_CFG_CMD_b_CapFastB2B                  =  23; #
our $FM_PCI_CFG_CMD_b_SpecialCycEn                =  3; #
our $FM_PCI_CFG_CMD_b_SigSysErr                   =  30; #
our $FM_PCI_CFG_CMD_b_IOSpaceEn                   =  0; #

our $FM_PCI_CFG_1_l_RevisionID                    =  0; #
our $FM_PCI_CFG_1_h_RevisionID                    =  7; #
our $FM_PCI_CFG_1_l_Class                         =  8; #
our $FM_PCI_CFG_1_h_Class                         =  31; #

our $FM_PCI_CFG_2_l_BIST                          =  24; #
our $FM_PCI_CFG_2_h_BIST                          =  31; #
our $FM_PCI_CFG_2_l_HeaderType                    =  16; #
our $FM_PCI_CFG_2_h_HeaderType                    =  23; #
our $FM_PCI_CFG_2_l_CacheLineSize                 =  0; #
our $FM_PCI_CFG_2_h_CacheLineSize                 =  7; #
our $FM_PCI_CFG_2_l_LatencyTimer                  =  8; #
our $FM_PCI_CFG_2_h_LatencyTimer                  =  15; #

our $FM_PCI_CFG_BAR0_l_AddressSize                =  1; #
our $FM_PCI_CFG_BAR0_h_AddressSize                =  2; #
our $FM_PCI_CFG_BAR0_l_Address                    =  4; #
our $FM_PCI_CFG_BAR0_h_Address                    =  31; #
our $FM_PCI_CFG_BAR0_b_Type                       =  0; #
our $FM_PCI_CFG_BAR0_b_Prefetchable               =  3; #

our $FM_PCI_CFG_BAR1_l_AddressHigh                =  0; #
our $FM_PCI_CFG_BAR1_h_AddressHigh                =  31; #

our $FM_PCI_CFG_BAR2_l_Reserved                   =  0; #
our $FM_PCI_CFG_BAR2_h_Reserved                   =  31; #

our $FM_PCI_CFG_BAR3_l_Reserved                   =  0; #
our $FM_PCI_CFG_BAR3_h_Reserved                   =  31; #

our $FM_PCI_CFG_BAR4_l_Reserved                   =  0; #
our $FM_PCI_CFG_BAR4_h_Reserved                   =  31; #

our $FM_PCI_CFG_BAR5_l_Reserved                   =  0; #
our $FM_PCI_CFG_BAR5_h_Reserved                   =  31; #

our $FM_PCI_CFG_CARDBUS_l_CardbusPtr              =  0; #
our $FM_PCI_CFG_CARDBUS_h_CardbusPtr              =  31; #

our $FM_PCI_CFG_SUBID_l_SubDeviceID               =  16; #
our $FM_PCI_CFG_SUBID_h_SubDeviceID               =  31; #
our $FM_PCI_CFG_SUBID_l_SubVendorID               =  0; #
our $FM_PCI_CFG_SUBID_h_SubVendorID               =  15; #

our $FM_PCI_CFG_EXP_ROM_l_Address                 =  11; #
our $FM_PCI_CFG_EXP_ROM_h_Address                 =  31; #
our $FM_PCI_CFG_EXP_ROM_b_Enable                  =  0; #

our $FM_PCI_CFG_CAP_PTR_l_CapabilityPtr           =  0; #
our $FM_PCI_CFG_CAP_PTR_h_CapabilityPtr           =  7; #
our $FM_PCI_CFG_CAP_PTR_l_Reserved                =  8; #
our $FM_PCI_CFG_CAP_PTR_h_Reserved                =  31; #

our $FM_PCI_CFG_RSVD_l_Reserved                   =  0; #
our $FM_PCI_CFG_RSVD_h_Reserved                   =  31; #

our $FM_PCI_CFG_INT_l_MaxLatency                  =  24; #
our $FM_PCI_CFG_INT_h_MaxLatency                  =  31; #
our $FM_PCI_CFG_INT_l_InterruptPin                =  8; #
our $FM_PCI_CFG_INT_h_InterruptPin                =  15; #
our $FM_PCI_CFG_INT_l_MinGrant                    =  16; #
our $FM_PCI_CFG_INT_h_MinGrant                    =  23; #
our $FM_PCI_CFG_INT_l_InterruptLine               =  0; #
our $FM_PCI_CFG_INT_h_InterruptLine               =  7; #

our $FM_PCI_CFG_PM_CAP_l_NextCapPtr               =  8; #
our $FM_PCI_CFG_PM_CAP_h_NextCapPtr               =  15; #
our $FM_PCI_CFG_PM_CAP_l_PowerManagementCapabilities  =  16; #
our $FM_PCI_CFG_PM_CAP_h_PowerManagementCapabilities  =  31; #
our $FM_PCI_CFG_PM_CAP_l_CapID                    =  0; #
our $FM_PCI_CFG_PM_CAP_h_CapID                    =  7; #

our $FM_PCI_CFG_PM_CTRL_b_PMEEn                   =  8; #
our $FM_PCI_CFG_PM_CTRL_b_PMEEvent                =  15; #
our $FM_PCI_CFG_PM_CTRL_b_B2B3Support             =  22; #
our $FM_PCI_CFG_PM_CTRL_l_PwrState                =  0; #
our $FM_PCI_CFG_PM_CTRL_h_PwrState                =  1; #
our $FM_PCI_CFG_PM_CTRL_b_NoSoftRst               =  3; #
our $FM_PCI_CFG_PM_CTRL_l_Reserved0               =  9; #
our $FM_PCI_CFG_PM_CTRL_h_Reserved0               =  14; #
our $FM_PCI_CFG_PM_CTRL_l_Reserved1               =  24; #
our $FM_PCI_CFG_PM_CTRL_h_Reserved1               =  31; #
our $FM_PCI_CFG_PM_CTRL_b_BusPwr                  =  23; #

our $FM_PCI_CFG_MSI_CAP_l_NextCapPtr              =  8; #
our $FM_PCI_CFG_MSI_CAP_h_NextCapPtr              =  15; #
our $FM_PCI_CFG_MSI_CAP_b_IsPVMCapable            =  24; #
our $FM_PCI_CFG_MSI_CAP_l_MultipleMessageEnable   =  20; #
our $FM_PCI_CFG_MSI_CAP_h_MultipleMessageEnable   =  22; #
our $FM_PCI_CFG_MSI_CAP_b_MSIEnable               =  16; #
our $FM_PCI_CFG_MSI_CAP_l_CapID                   =  0; #
our $FM_PCI_CFG_MSI_CAP_h_CapID                   =  7; #
our $FM_PCI_CFG_MSI_CAP_b_Is64BitCapable          =  23; #
our $FM_PCI_CFG_MSI_CAP_l_MultipleMessageCapable  =  17; #
our $FM_PCI_CFG_MSI_CAP_h_MultipleMessageCapable  =  19; #

our $FM_PCI_CFG_MSI_ADDR_LO_l_Addr                =  2; #
our $FM_PCI_CFG_MSI_ADDR_LO_h_Addr                =  31; #
our $FM_PCI_CFG_MSI_ADDR_LO_l_Reserved            =  0; #
our $FM_PCI_CFG_MSI_ADDR_LO_h_Reserved            =  1; #

our $FM_PCI_CFG_MSI_ADDR_HI_l_Addr                =  0; #
our $FM_PCI_CFG_MSI_ADDR_HI_h_Addr                =  31; #

our $FM_PCI_CFG_MSI_DATA_l_Data                   =  0; #
our $FM_PCI_CFG_MSI_DATA_h_Data                   =  15; #

our $FM_PCI_CFG_MSI_MASK_l_Mask                   =  0; #
our $FM_PCI_CFG_MSI_MASK_h_Mask                   =  31; #

our $FM_PCI_CFG_MSI_PENDING_l_Pending             =  0; #
our $FM_PCI_CFG_MSI_PENDING_h_Pending             =  31; #

our $FM_PCI_CFG_PCIE_CAP_l_InterrruptMessageNumber  =  25; #
our $FM_PCI_CFG_PCIE_CAP_h_InterrruptMessageNumber  =  29; #
our $FM_PCI_CFG_PCIE_CAP_b_SlotImplemented        =  24; #
our $FM_PCI_CFG_PCIE_CAP_l_DevicePortType         =  20; #
our $FM_PCI_CFG_PCIE_CAP_h_DevicePortType         =  23; #
our $FM_PCI_CFG_PCIE_CAP_l_NextCapPtr             =  8; #
our $FM_PCI_CFG_PCIE_CAP_h_NextCapPtr             =  15; #
our $FM_PCI_CFG_PCIE_CAP_l_CapabilityVersion      =  16; #
our $FM_PCI_CFG_PCIE_CAP_h_CapabilityVersion      =  19; #
our $FM_PCI_CFG_PCIE_CAP_l_CapID                  =  0; #
our $FM_PCI_CFG_PCIE_CAP_h_CapID                  =  7; #

our $FM_PCI_CFG_PCI_DEV_CAP_l_DeviceCapabilities  =  0; #
our $FM_PCI_CFG_PCI_DEV_CAP_h_DeviceCapabilities  =  31; #

our $FM_PCI_CFG_PCIE_DEV_l_DeviceStatus1          =  20; #
our $FM_PCI_CFG_PCIE_DEV_h_DeviceStatus1          =  21; #
our $FM_PCI_CFG_PCIE_DEV_l_DeviceStatus0          =  16; #
our $FM_PCI_CFG_PCIE_DEV_h_DeviceStatus0          =  19; #
our $FM_PCI_CFG_PCIE_DEV_l_DeviceControl          =  0; #
our $FM_PCI_CFG_PCIE_DEV_h_DeviceControl          =  15; #

our $FM_PCI_CFG_PCI_LINK_CAP_l_LinkCapabilities   =  0; #
our $FM_PCI_CFG_PCI_LINK_CAP_h_LinkCapabilities   =  31; #

our $FM_PCI_CFG_PCI_LINK_CTRL_b_LinkBandwdMgmtIntEn  =  10; #
our $FM_PCI_CFG_PCI_LINK_CTRL_b_CommonClkCfg      =  6; #
our $FM_PCI_CFG_PCI_LINK_CTRL_l_LinkStatus        =  16; #
our $FM_PCI_CFG_PCI_LINK_CTRL_h_LinkStatus        =  31; #
our $FM_PCI_CFG_PCI_LINK_CTRL_b_LinkAutoBandwdIntEn  =  11; #
our $FM_PCI_CFG_PCI_LINK_CTRL_b_HWAutoWidthDis    =  9; #
our $FM_PCI_CFG_PCI_LINK_CTRL_l_ActStateLinkPM    =  0; #
our $FM_PCI_CFG_PCI_LINK_CTRL_h_ActStateLinkPM    =  1; #
our $FM_PCI_CFG_PCI_LINK_CTRL_b_RdCplBoundary     =  3; #
our $FM_PCI_CFG_PCI_LINK_CTRL_b_EnClkPM           =  8; #
our $FM_PCI_CFG_PCI_LINK_CTRL_l_Reserved          =  4; #
our $FM_PCI_CFG_PCI_LINK_CTRL_h_Reserved          =  5; #
our $FM_PCI_CFG_PCI_LINK_CTRL_b_ExtSync           =  7; #

our $FM_PCI_CFG_DEV_CAP2_b_CplTimeoutDisSuppt     =  4; #
our $FM_PCI_CFG_DEV_CAP2_l_Reserved               =  0; #
our $FM_PCI_CFG_DEV_CAP2_h_Reserved               =  3; #

our $FM_PCI_CFG_DEV_CTRL2_l_CplTimeoutVal         =  0; #
our $FM_PCI_CFG_DEV_CTRL2_h_CplTimeoutVal         =  3; #
our $FM_PCI_CFG_DEV_CTRL2_b_CplTimeoutDis         =  4; #

our $FM_PCI_CFG_PCI_LINK_CAP2_b_CrosslinkSuppt    =  8; #
our $FM_PCI_CFG_PCI_LINK_CAP2_b_Reserved          =  0; #
our $FM_PCI_CFG_PCI_LINK_CAP2_l_SupptLinkSpeed    =  1; #
our $FM_PCI_CFG_PCI_LINK_CAP2_h_SupptLinkSpeed    =  7; #

our $FM_PCI_CFG_PCI_LINK_CTRL2_l_CompDeEmph       =  12; #
our $FM_PCI_CFG_PCI_LINK_CTRL2_h_CompDeEmph       =  15; #
our $FM_PCI_CFG_PCI_LINK_CTRL2_b_ComplianceSOS    =  11; #
our $FM_PCI_CFG_PCI_LINK_CTRL2_b_EnterCompliance  =  4; #
our $FM_PCI_CFG_PCI_LINK_CTRL2_b_HWAutoSpeedDis   =  5; #
our $FM_PCI_CFG_PCI_LINK_CTRL2_b_LinkEqReq        =  21; #
our $FM_PCI_CFG_PCI_LINK_CTRL2_l_TgtLinkSpeed     =  0; #
our $FM_PCI_CFG_PCI_LINK_CTRL2_h_TgtLinkSpeed     =  3; #
our $FM_PCI_CFG_PCI_LINK_CTRL2_b_EnterModCompliance  =  10; #
our $FM_PCI_CFG_PCI_LINK_CTRL2_b_Reserved0        =  6; #
our $FM_PCI_CFG_PCI_LINK_CTRL2_l_Reserved1        =  16; #
our $FM_PCI_CFG_PCI_LINK_CTRL2_h_Reserved1        =  20; #
our $FM_PCI_CFG_PCI_LINK_CTRL2_l_TxMargin         =  7; #
our $FM_PCI_CFG_PCI_LINK_CTRL2_h_TxMargin         =  9; #

our $FM_PCI_ENDIANISM_l_BigEndian                 =  0; #
our $FM_PCI_ENDIANISM_h_BigEndian                 =  31; #

our $FM_PCI_COMMAND_l_Argument                    =  16; #
our $FM_PCI_COMMAND_h_Argument                    =  31; #
our $FM_PCI_COMMAND_l_Command                     =  0; #
our $FM_PCI_COMMAND_h_Command                     =  15; #

our $FM_PCI_STATUS_l_RxState                      =  3; #
our $FM_PCI_STATUS_h_RxState                      =  5; #
our $FM_PCI_STATUS_l_TxState                      =  0; #
our $FM_PCI_STATUS_h_TxState                      =  2; #

our $FM_PCI_COALESCING_l_CoalescingTimeout        =  0; #
our $FM_PCI_COALESCING_h_CoalescingTimeout        =  15; #
our $FM_PCI_COALESCING_l_CoalescingCnt            =  16; #
our $FM_PCI_COALESCING_h_CoalescingCnt            =  23; #
our $FM_PCI_COALESCING_b_CoalescingRx             =  25; #
our $FM_PCI_COALESCING_b_CoalescingTx             =  24; #

our $FM_PCI_RX_BD_BASE_l_Address                  =  0; #
our $FM_PCI_RX_BD_BASE_h_Address                  =  63; #

our $FM_PCI_RX_BD_END_l_Address                   =  0; #
our $FM_PCI_RX_BD_END_h_Address                   =  63; #

our $FM_PCI_TX_BD_BASE_l_Address                  =  0; #
our $FM_PCI_TX_BD_BASE_h_Address                  =  63; #

our $FM_PCI_TX_BD_END_l_Address                   =  0; #
our $FM_PCI_TX_BD_END_h_Address                   =  63; #

our $FM_PCI_IP_b_Pause                            =  3; #
our $FM_PCI_IP_b_LinkDnToUp                       =  5; #
our $FM_PCI_IP_b_LinkUpToDn                       =  4; #
our $FM_PCI_IP_b_RxPackets                        =  1; #
our $FM_PCI_IP_b_TxPackets                        =  0; #
our $FM_PCI_IP_b_Coalescing                       =  2; #
our $FM_PCI_IP_b_TLP_Err                          =  6; #
our $FM_PCI_IP_l_Reserved                         =  9; #
our $FM_PCI_IP_h_Reserved                         =  31; #
our $FM_PCI_IP_b_ECRC_Err                         =  8; #
our $FM_PCI_IP_b_Cmpl_Timout                      =  7; #

our $FM_PCI_IM_l_Mask                             =  0; #
our $FM_PCI_IM_h_Mask                             =  31; #

our $FM_PCI_CURRENT_TX_DATA_PTR_l_Address         =  0; #
our $FM_PCI_CURRENT_TX_DATA_PTR_h_Address         =  63; #

our $FM_PCI_CURRENT_RX_DATA_PTR_l_Address         =  0; #
our $FM_PCI_CURRENT_RX_DATA_PTR_h_Address         =  63; #

our $FM_PCI_CURRENT_TX_BD_PTR_l_Address           =  0; #
our $FM_PCI_CURRENT_TX_BD_PTR_h_Address           =  63; #

our $FM_PCI_CURRENT_RX_BD_PTR_l_Address           =  0; #
our $FM_PCI_CURRENT_RX_BD_PTR_h_Address           =  63; #

our $FM_PCI_TX_FRAME_LEN_l_MinLen                 =  16; #
our $FM_PCI_TX_FRAME_LEN_h_MinLen                 =  22; #
our $FM_PCI_TX_FRAME_LEN_l_Reserved               =  23; #
our $FM_PCI_TX_FRAME_LEN_h_Reserved               =  31; #
our $FM_PCI_TX_FRAME_LEN_l_MaxLen                 =  0; #
our $FM_PCI_TX_FRAME_LEN_h_MaxLen                 =  15; #

our $FM_PCI_SIZE_l_ChunkSize                      =  0; #
our $FM_PCI_SIZE_h_ChunkSize                      =  5; #
our $FM_PCI_SIZE_l_FetchLimit                     =  8; #
our $FM_PCI_SIZE_h_FetchLimit                     =  10; #

our $FM_PCI_DMA_CFG_l_TagsToKeep                  =  0; #
our $FM_PCI_DMA_CFG_h_TagsToKeep                  =  1; #
our $FM_PCI_DMA_CFG_l_DMAEn                       =  4; #
our $FM_PCI_DMA_CFG_h_DMAEn                       =  5; #
our $FM_PCI_DMA_CFG_l_LinkDnAction                =  6; #
our $FM_PCI_DMA_CFG_h_LinkDnAction                =  7; #
our $FM_PCI_DMA_CFG_b_TagPosition                 =  2; #
our $FM_PCI_DMA_CFG_b_Debug                       =  3; #

our $FM_PCI_FRAME_TIMEOUT_l_Timeout               =  0; #
our $FM_PCI_FRAME_TIMEOUT_h_Timeout               =  21; #

our $FM_PCI_STAT_COUNTER_l_RcplBadStatus          =  4; #
our $FM_PCI_STAT_COUNTER_h_RcplBadStatus          =  7; #
our $FM_PCI_STAT_COUNTER_l_RxModeErr              =  20; #
our $FM_PCI_STAT_COUNTER_h_RxModeErr              =  23; #
our $FM_PCI_STAT_COUNTER_l_RcplEcrc               =  0; #
our $FM_PCI_STAT_COUNTER_h_RcplEcrc               =  3; #
our $FM_PCI_STAT_COUNTER_l_RcplTimeout            =  8; #
our $FM_PCI_STAT_COUNTER_h_RcplTimeout            =  11; #
our $FM_PCI_STAT_COUNTER_l_RxTimeout              =  16; #
our $FM_PCI_STAT_COUNTER_h_RxTimeout              =  19; #
our $FM_PCI_STAT_COUNTER_l_TxTimeout              =  12; #
our $FM_PCI_STAT_COUNTER_h_TxTimeout              =  15; #

our $FM_PCI_STAT_NUM_PKTS_l_TxNumPkts             =  0; #
our $FM_PCI_STAT_NUM_PKTS_h_TxNumPkts             =  15; #
our $FM_PCI_STAT_NUM_PKTS_l_RxNumPkts             =  16; #
our $FM_PCI_STAT_NUM_PKTS_h_RxNumPkts             =  31; #

our $FM_PCI_DEBUG_l_debug                         =  0; #
our $FM_PCI_DEBUG_h_debug                         =  3; #

our $FM_PCI_CORE_CTRL_1_b_DbiTarget               =  17; #
our $FM_PCI_CORE_CTRL_1_b_AuxPwrDet               =  15; #
our $FM_PCI_CORE_CTRL_1_b_D3Wakeup                =  14; #
our $FM_PCI_CORE_CTRL_1_b_CoreEnable              =  16; #
our $FM_PCI_CORE_CTRL_1_b_DbiBarMaskAcc           =  18; #
our $FM_PCI_CORE_CTRL_1_b_LegacyIntEnable         =  12; #
our $FM_PCI_CORE_CTRL_1_b_SysInt                  =  13; #
our $FM_PCI_CORE_CTRL_1_l_CfgSpaceMask            =  0; #
our $FM_PCI_CORE_CTRL_1_h_CfgSpaceMask            =  11; #

our $FM_PCI_CORE_CTRL_2_l_DiagBusInx              =  0; #
our $FM_PCI_CORE_CTRL_2_h_DiagBusInx              =  3; #
our $FM_PCI_CORE_CTRL_2_l_DiagCtrl                =  4; #
our $FM_PCI_CORE_CTRL_2_h_DiagCtrl                =  6; #

our $FM_PCI_CORE_DEBUG_1_l_PostedHdrCrdts         =  0; #
our $FM_PCI_CORE_DEBUG_1_h_PostedHdrCrdts         =  7; #
our $FM_PCI_CORE_DEBUG_1_l_PostedDataCrdts        =  8; #
our $FM_PCI_CORE_DEBUG_1_h_PostedDataCrdts        =  19; #
our $FM_PCI_CORE_DEBUG_1_l_NonpostedHdrCrdts      =  20; #
our $FM_PCI_CORE_DEBUG_1_h_NonpostedHdrCrdts      =  27; #

our $FM_PCI_CORE_DEBUG_2_l_CplHdrCrdts            =  12; #
our $FM_PCI_CORE_DEBUG_2_h_CplHdrCrdts            =  19; #
our $FM_PCI_CORE_DEBUG_2_l_CplDataCrdts           =  20; #
our $FM_PCI_CORE_DEBUG_2_h_CplDataCrdts           =  31; #
our $FM_PCI_CORE_DEBUG_2_l_NonpostedDataCrdts     =  0; #
our $FM_PCI_CORE_DEBUG_2_h_NonpostedDataCrdts     =  11; #

our $FM_PCI_CORE_DEBUG_3_l_Bar0StartLo            =  0; #
our $FM_PCI_CORE_DEBUG_3_h_Bar0StartLo            =  31; #

our $FM_PCI_CORE_DEBUG_4_l_Bar0StartHi            =  0; #
our $FM_PCI_CORE_DEBUG_4_h_Bar0StartHi            =  31; #

our $FM_PCI_CORE_DEBUG_5_l_Bar1StartLo            =  0; #
our $FM_PCI_CORE_DEBUG_5_h_Bar1StartLo            =  31; #

our $FM_PCI_CORE_DEBUG_6_l_Bar1StartHi            =  0; #
our $FM_PCI_CORE_DEBUG_6_h_Bar1StartHi            =  31; #

our $FM_PCI_CORE_DEBUG_7_l_Bar0LimitLo            =  0; #
our $FM_PCI_CORE_DEBUG_7_h_Bar0LimitLo            =  31; #

our $FM_PCI_CORE_DEBUG_8_l_Bar0LimitHi            =  0; #
our $FM_PCI_CORE_DEBUG_8_h_Bar0LimitHi            =  31; #

our $FM_PCI_CORE_DEBUG_9_l_Bar1LimitLo            =  0; #
our $FM_PCI_CORE_DEBUG_9_h_Bar1LimitLo            =  31; #

our $FM_PCI_CORE_DEBUG_10_l_Bar1LimitHi           =  0; #
our $FM_PCI_CORE_DEBUG_10_h_Bar1LimitHi           =  31; #

our $FM_PCI_CORE_DEBUG_11_l_ExpRomStart           =  0; #
our $FM_PCI_CORE_DEBUG_11_h_ExpRomStart           =  31; #

our $FM_PCI_CORE_DEBUG_12_l_ExpRomLimit           =  0; #
our $FM_PCI_CORE_DEBUG_12_h_ExpRomLimit           =  31; #

our $FM_PCI_CORE_DEBUG_13_b_ClkReqN               =  31; #
our $FM_PCI_CORE_DEBUG_13_b_VndMsg                =  21; #
our $FM_PCI_CORE_DEBUG_13_b_MemSpaceEn            =  1; #
our $FM_PCI_CORE_DEBUG_13_b_PmNoSoftRst           =  9; #
our $FM_PCI_CORE_DEBUG_13_b_BusMstrEn             =  0; #
our $FM_PCI_CORE_DEBUG_13_b_PmStat                =  17; #
our $FM_PCI_CORE_DEBUG_13_b_MsgUnlk               =  20; #
our $FM_PCI_CORE_DEBUG_13_b_PhyLinkUp             =  23; #
our $FM_PCI_CORE_DEBUG_13_b_AuxPmEn               =  19; #
our $FM_PCI_CORE_DEBUG_13_l_MaxRdRqSz             =  2; #
our $FM_PCI_CORE_DEBUG_13_h_MaxRdRqSz             =  4; #
our $FM_PCI_CORE_DEBUG_13_b_PmPmeEn               =  16; #
our $FM_PCI_CORE_DEBUG_13_b_Wake                  =  30; #
our $FM_PCI_CORE_DEBUG_13_l_LtssmSt               =  24; #
our $FM_PCI_CORE_DEBUG_13_h_LtssmSt               =  29; #
our $FM_PCI_CORE_DEBUG_13_l_PmDSt                 =  13; #
our $FM_PCI_CORE_DEBUG_13_h_PmDSt                 =  15; #
our $FM_PCI_CORE_DEBUG_13_l_PmCurrSt              =  10; #
our $FM_PCI_CORE_DEBUG_13_h_PmCurrSt              =  12; #
our $FM_PCI_CORE_DEBUG_13_l_MaxPldSz              =  5; #
our $FM_PCI_CORE_DEBUG_13_h_MaxPldSz              =  7; #
our $FM_PCI_CORE_DEBUG_13_b_PmOff                 =  22; #
our $FM_PCI_CORE_DEBUG_13_b_Rcb                   =  8; #
our $FM_PCI_CORE_DEBUG_13_b_PmBlkTlp              =  18; #

our $FM_PCI_CORE_DEBUG_14_l_MsgPld                =  0; #
our $FM_PCI_CORE_DEBUG_14_h_MsgPld                =  31; #

our $FM_PCI_CORE_DEBUG_15_l_DbgInfoLo             =  0; #
our $FM_PCI_CORE_DEBUG_15_h_DbgInfoLo             =  31; #

our $FM_PCI_CORE_DEBUG_16_l_DbgInfoHi             =  0; #
our $FM_PCI_CORE_DEBUG_16_h_DbgInfoHi             =  31; #

our $FM_PCI_CORE_DEBUG_17_b_ParErrRcv             =  2; #
our $FM_PCI_CORE_DEBUG_17_b_LinkRqRstN            =  4; #
our $FM_PCI_CORE_DEBUG_17_b_DllLinkUp             =  3; #
our $FM_PCI_CORE_DEBUG_17_l_ParErrXmt             =  0; #
our $FM_PCI_CORE_DEBUG_17_h_ParErrXmt             =  1; #

our $FM_PCI_CORE_DEBUG_18_l_DiagBus               =  0; #
our $FM_PCI_CORE_DEBUG_18_h_DiagBus               =  31; #

our $FM_PCI_CORE_DEBUG_19_l_DiagBus               =  0; #
our $FM_PCI_CORE_DEBUG_19_h_DiagBus               =  31; #

our $FM_PCI_CORE_DEBUG_20_l_DiagBus               =  0; #
our $FM_PCI_CORE_DEBUG_20_h_DiagBus               =  31; #

our $FM_PCI_CORE_DEBUG_21_l_DiagBus               =  0; #
our $FM_PCI_CORE_DEBUG_21_h_DiagBus               =  31; #

our $FM_PCI_CORE_DEBUG_22_l_DiagBus               =  0; #
our $FM_PCI_CORE_DEBUG_22_h_DiagBus               =  31; #

our $FM_PCI_SERDES_CTRL_1_b_TxPhaseMasterEn       =  21; #
our $FM_PCI_SERDES_CTRL_1_l_TxSlew                =  18; #
our $FM_PCI_SERDES_CTRL_1_h_TxSlew                =  19; #
our $FM_PCI_SERDES_CTRL_1_b_RxPolarityInv         =  7; #
our $FM_PCI_SERDES_CTRL_1_l_TxEqPost              =  14; #
our $FM_PCI_SERDES_CTRL_1_h_TxEqPost              =  17; #
our $FM_PCI_SERDES_CTRL_1_b_TxOutputEn            =  20; #
our $FM_PCI_SERDES_CTRL_1_l_EbufThreshold         =  0; #
our $FM_PCI_SERDES_CTRL_1_h_EbufThreshold         =  2; #
our $FM_PCI_SERDES_CTRL_1_b_TxPolarityInv         =  28; #
our $FM_PCI_SERDES_CTRL_1_l_ElecIdleThreshold     =  3; #
our $FM_PCI_SERDES_CTRL_1_h_ElecIdleThreshold     =  6; #
our $FM_PCI_SERDES_CTRL_1_l_TxEqPre               =  12; #
our $FM_PCI_SERDES_CTRL_1_h_TxEqPre               =  13; #
our $FM_PCI_SERDES_CTRL_1_l_TxPhaseMaster         =  22; #
our $FM_PCI_SERDES_CTRL_1_h_TxPhaseMaster         =  23; #
our $FM_PCI_SERDES_CTRL_1_l_TxPhaseIn             =  24; #
our $FM_PCI_SERDES_CTRL_1_h_TxPhaseIn             =  27; #
our $FM_PCI_SERDES_CTRL_1_l_TxOutputAmpl          =  8; #
our $FM_PCI_SERDES_CTRL_1_h_TxOutputAmpl          =  11; #
our $FM_PCI_SERDES_CTRL_1_b_RXLaneReverse         =  29; #
our $FM_PCI_SERDES_CTRL_1_b_TXLaneReverse         =  30; #

our $FM_PCI_SERDES_CTRL_2_l_TxOutputCtrl          =  10; #
our $FM_PCI_SERDES_CTRL_2_h_TxOutputCtrl          =  13; #
our $FM_PCI_SERDES_CTRL_2_b_TxInjectDispErr       =  6; #
our $FM_PCI_SERDES_CTRL_2_b_FarLoopbackEn         =  4; #
our $FM_PCI_SERDES_CTRL_2_b_NearLoopbackEn        =  5; #
our $FM_PCI_SERDES_CTRL_2_l_TxPatternGenSel       =  8; #
our $FM_PCI_SERDES_CTRL_2_h_TxPatternGenSel       =  9; #
our $FM_PCI_SERDES_CTRL_2_l_RxPatternCmpSel       =  1; #
our $FM_PCI_SERDES_CTRL_2_h_RxPatternCmpSel       =  2; #
our $FM_PCI_SERDES_CTRL_2_b_TxPatternGenEn        =  7; #
our $FM_PCI_SERDES_CTRL_2_b_RxSigStrengthEn       =  3; #
our $FM_PCI_SERDES_CTRL_2_b_RxPatternCmpEn        =  0; #

our $FM_PCI_SERDES_CTRL_3_l_LowNoiseFilterCfg     =  0; #
our $FM_PCI_SERDES_CTRL_3_h_LowNoiseFilterCfg     =  15; #
our $FM_PCI_SERDES_CTRL_3_l_RefSel                =  25; #
our $FM_PCI_SERDES_CTRL_3_h_RefSel                =  30; #
our $FM_PCI_SERDES_CTRL_3_l_RxRateSel             =  19; #
our $FM_PCI_SERDES_CTRL_3_h_RxRateSel             =  24; #
our $FM_PCI_SERDES_CTRL_3_b_RxDropoutCharEn       =  18; #
our $FM_PCI_SERDES_CTRL_3_b_TxPhaseSlip           =  31; #
our $FM_PCI_SERDES_CTRL_3_b_OverrideProcDflt      =  16; #
our $FM_PCI_SERDES_CTRL_3_b_RxCx4LosMode          =  17; #

our $FM_PCI_SERDES_CTRL_4_b_TxPhaseCalEn          =  14; #
our $FM_PCI_SERDES_CTRL_4_l_TxRateSel             =  6; #
our $FM_PCI_SERDES_CTRL_4_h_TxRateSel             =  11; #
our $FM_PCI_SERDES_CTRL_4_b_Tx20bEn               =  12; #
our $FM_PCI_SERDES_CTRL_4_l_RxActSigMin           =  0; #
our $FM_PCI_SERDES_CTRL_4_h_RxActSigMin           =  5; #
our $FM_PCI_SERDES_CTRL_4_b_K30ErrEn              =  13; #

our $FM_PCI_SERDES_DEBUG_1_l_AnalogToCore         =  0; #
our $FM_PCI_SERDES_DEBUG_1_h_AnalogToCore         =  31; #

our $FM_PCI_SERDES_DEBUG_2_l_AnalogToCore         =  0; #
our $FM_PCI_SERDES_DEBUG_2_h_AnalogToCore         =  31; #

our $FM_PCI_SERDES_DEBUG_3_l_PatternCmpPass       =  24; #
our $FM_PCI_SERDES_DEBUG_3_h_PatternCmpPass       =  27; #
our $FM_PCI_SERDES_DEBUG_3_l_RxSigStrength        =  16; #
our $FM_PCI_SERDES_DEBUG_3_h_RxSigStrength        =  23; #
our $FM_PCI_SERDES_DEBUG_3_l_TxPhaseOut           =  0; #
our $FM_PCI_SERDES_DEBUG_3_h_TxPhaseOut           =  15; #

our $FM_ESCHED_CFG_1_l_tcGroupBoundary            =  12; #
our $FM_ESCHED_CFG_1_h_tcGroupBoundary            =  23; #
our $FM_ESCHED_CFG_1_l_prioritySetBoundary        =  0; #
our $FM_ESCHED_CFG_1_h_prioritySetBoundary        =  11; #

our $FM_ESCHED_CFG_2_l_tcEnable                   =  12; #
our $FM_ESCHED_CFG_2_h_tcEnable                   =  23; #
our $FM_ESCHED_CFG_2_l_strictPriority             =  0; #
our $FM_ESCHED_CFG_2_h_strictPriority             =  11; #

our $FM_ESCHED_CFG_3_l_tcInnerPriority            =  0; #
our $FM_ESCHED_CFG_3_h_tcInnerPriority            =  11; #

our $FM_ESCHED_DRR_Q_l_q                          =  0; #
our $FM_ESCHED_DRR_Q_h_q                          =  23; #

our $FM_ESCHED_DRR_CFG_l_groupBoundary            =  12; #
our $FM_ESCHED_DRR_CFG_h_groupBoundary            =  23; #
our $FM_ESCHED_DRR_CFG_l_zeroLength               =  0; #
our $FM_ESCHED_DRR_CFG_h_zeroLength               =  11; #
our $FM_ESCHED_DRR_CFG_l_ifgPenalty               =  24; #
our $FM_ESCHED_DRR_CFG_h_ifgPenalty               =  31; #

our $FM_ESCHED_DRR_DC_INIT_l_dc                   =  0; #
our $FM_ESCHED_DRR_DC_INIT_h_dc                   =  23; #

our $FM_MSB_CFG_b_Enable                          =  0; #
our $FM_MSB_CFG_b_PadHsmFrame                     =  1; #
our $FM_MSB_CFG_l_debug                           =  24; #
our $FM_MSB_CFG_h_debug                           =  27; #
our $FM_MSB_CFG_l_TimeOut                         =  2; #
our $FM_MSB_CFG_h_TimeOut                         =  23; #

our $FM_MSB_RX_RATE_l_NumCycles                   =  0; #
our $FM_MSB_RX_RATE_h_NumCycles                   =  7; #
our $FM_MSB_RX_RATE_l_InterFrameGap               =  8; #
our $FM_MSB_RX_RATE_h_InterFrameGap               =  11; #

our $FM_MSB_TRUNC_l_RXNumWords                    =  0; #
our $FM_MSB_TRUNC_h_RXNumWords                    =  15; #
our $FM_MSB_TRUNC_l_TXNumWords                    =  16; #
our $FM_MSB_TRUNC_h_TXNumWords                    =  31; #

our $FM_MSB_TX_CRC_ERROR_l_Counter                =  0; #
our $FM_MSB_TX_CRC_ERROR_h_Counter                =  31; #

our $FM_MSB_TX_FRAME_ERROR_l_Counter              =  0; #
our $FM_MSB_TX_FRAME_ERROR_h_Counter              =  31; #

our $FM_MSB_TX_TIMEOUT_l_Counter                  =  0; #
our $FM_MSB_TX_TIMEOUT_h_Counter                  =  31; #

our $FM_MSB_TX_VALID_FRAMES_l_Counter             =  0; #
our $FM_MSB_TX_VALID_FRAMES_h_Counter             =  31; #

our $FM_MSB_TX_INVALID_FRAMES_l_Counter           =  0; #
our $FM_MSB_TX_INVALID_FRAMES_h_Counter           =  31; #

our $FM_MSB_RX_VALID_FRAMES_l_Counter             =  0; #
our $FM_MSB_RX_VALID_FRAMES_h_Counter             =  31; #

our $FM_FIBM_CFG_b_append4bytes                   =  16; #
our $FM_FIBM_CFG_b_padIbmResponse                 =  1; #
our $FM_FIBM_CFG_b_ibmGlortEn                     =  2; #
our $FM_FIBM_CFG_b_interruptGlortEn               =  3; #
our $FM_FIBM_CFG_l_responseType                   =  8; #
our $FM_FIBM_CFG_h_responseType                   =  11; #
our $FM_FIBM_CFG_b_tagPosition                    =  18; #
our $FM_FIBM_CFG_l_interruptType                  =  12; #
our $FM_FIBM_CFG_h_interruptType                  =  15; #
our $FM_FIBM_CFG_l_debug                          =  19; #
our $FM_FIBM_CFG_h_debug                          =  22; #
our $FM_FIBM_CFG_b_ignoreLast4Bytes               =  17; #
our $FM_FIBM_CFG_l_requestType                    =  4; #
our $FM_FIBM_CFG_h_requestType                    =  7; #
our $FM_FIBM_CFG_b_disableIbm                     =  0; #

our $FM_FIBM_SGLORT_l_SGlort                      =  0; #
our $FM_FIBM_SGLORT_h_SGlort                      =  15; #

our $FM_FIBM_INT_l_interruptGlort                 =  0; #
our $FM_FIBM_INT_h_interruptGlort                 =  15; #
our $FM_FIBM_INT_l_interruptInterval              =  16; #
our $FM_FIBM_INT_h_interruptInterval              =  31; #

our $FM_FIBM_INT_FRAME_l_islUserBits              =  20; #
our $FM_FIBM_INT_FRAME_h_islUserBits              =  27; #
our $FM_FIBM_INT_FRAME_l_islSysPri                =  16; #
our $FM_FIBM_INT_FRAME_h_islSysPri                =  19; #
our $FM_FIBM_INT_FRAME_l_etherType                =  0; #
our $FM_FIBM_INT_FRAME_h_etherType                =  15; #

our $FM_FIBM_INT_FRAME_DMAC_l_MacAddress          =  0; #
our $FM_FIBM_INT_FRAME_DMAC_h_MacAddress          =  47; #

our $FM_FIBM_INT_FRAME_SMAC_l_MacAddress          =  0; #
our $FM_FIBM_INT_FRAME_SMAC_h_MacAddress          =  47; #

our $FM_FIBM_REQUEST_CTR_l_Counter                =  0; #
our $FM_FIBM_REQUEST_CTR_h_Counter                =  31; #

our $FM_FIBM_DROP_CTR_l_Counter                   =  0; #
our $FM_FIBM_DROP_CTR_h_Counter                   =  31; #

our $FM_FIBM_RESPONSE_CTR_l_Counter               =  0; #
our $FM_FIBM_RESPONSE_CTR_h_Counter               =  31; #

our $FM_FIBM_INTR_CTR_0_l_Counter                 =  0; #
our $FM_FIBM_INTR_CTR_0_h_Counter                 =  31; #

our $FM_FIBM_INTR_CTR_1_l_Counter                 =  0; #
our $FM_FIBM_INTR_CTR_1_h_Counter                 =  31; #

our $FM_FIBM_SCRATCH_0_l_scratch0                 =  0; #
our $FM_FIBM_SCRATCH_0_h_scratch0                 =  31; #

our $FM_FIBM_SCRATCH_1_l_scratch1                 =  0; #
our $FM_FIBM_SCRATCH_1_h_scratch1                 =  31; #

our $FM_FIBM_SCRATCH_2_l_scratch2                 =  0; #
our $FM_FIBM_SCRATCH_2_h_scratch2                 =  31; #

our $FM_FIBM_SCRATCH_3_l_scratch3                 =  0; #
our $FM_FIBM_SCRATCH_3_h_scratch3                 =  31; #

our $FM_FIBM_SCRATCH_4_l_scratch4                 =  0; #
our $FM_FIBM_SCRATCH_4_h_scratch4                 =  31; #

our $FM_FIBM_SCRATCH_5_l_scratch5                 =  0; #
our $FM_FIBM_SCRATCH_5_h_scratch5                 =  31; #

our $FM_FIBM_SCRATCH_6_l_scratch6                 =  0; #
our $FM_FIBM_SCRATCH_6_h_scratch6                 =  31; #

our $FM_FIBM_SCRATCH_7_l_scratch7                 =  0; #
our $FM_FIBM_SCRATCH_7_h_scratch7                 =  31; #

our $FM_FIBM_SCRATCH_8_l_scratch8                 =  0; #
our $FM_FIBM_SCRATCH_8_h_scratch8                 =  31; #

our $FM_FIBM_SCRATCH_9_l_scratch9                 =  0; #
our $FM_FIBM_SCRATCH_9_h_scratch9                 =  31; #

our $FM_FIBM_SCRATCH_10_l_scratch10               =  0; #
our $FM_FIBM_SCRATCH_10_h_scratch10               =  31; #

our $FM_FIBM_SCRATCH_11_l_scratch11               =  0; #
our $FM_FIBM_SCRATCH_11_h_scratch11               =  31; #

our $FM_FIBM_SCRATCH_12_l_scratch12               =  0; #
our $FM_FIBM_SCRATCH_12_h_scratch12               =  31; #

our $FM_FIBM_SCRATCH_13_l_scratch13               =  0; #
our $FM_FIBM_SCRATCH_13_h_scratch13               =  31; #

our $FM_FIBM_SCRATCH_14_l_scratch14               =  0; #
our $FM_FIBM_SCRATCH_14_h_scratch14               =  31; #

our $FM_FIBM_SCRATCH_15_l_scratch15               =  0; #
our $FM_FIBM_SCRATCH_15_h_scratch15               =  31; #

our $FM_EACL_CAM1_l_Key                           =  38; #
our $FM_EACL_CAM1_h_Key                           =  75; #
our $FM_EACL_CAM1_l_KeyInvert                     =  0; #
our $FM_EACL_CAM1_h_KeyInvert                     =  37; #

our $FM_EACL_CAM2_l_Key                           =  32; #
our $FM_EACL_CAM2_h_Key                           =  63; #
our $FM_EACL_CAM2_l_KeyInvert                     =  0; #
our $FM_EACL_CAM2_h_KeyInvert                     =  31; #

our $FM_EACL_ACTION_RAM_b_Count                   =  1; #
our $FM_EACL_ACTION_RAM_b_Permit                  =  0; #
our $FM_EACL_ACTION_RAM_b_ActionExt               =  5; #
our $FM_EACL_ACTION_RAM_b_ActionA                 =  2; #
our $FM_EACL_ACTION_RAM_b_ActionB                 =  3; #
our $FM_EACL_ACTION_RAM_b_ActionC                 =  4; #

our $FM_EACL_PROFILE_TABLE_l_CmdB                 =  4; #
our $FM_EACL_PROFILE_TABLE_h_CmdB                 =  6; #
our $FM_EACL_PROFILE_TABLE_b_DropCodeSelect       =  7; #
our $FM_EACL_PROFILE_TABLE_l_CmdA                 =  0; #
our $FM_EACL_PROFILE_TABLE_h_CmdA                 =  3; #

our $FM_LAG_PORT_TABLE_l_LagSelect                =  0; #
our $FM_LAG_PORT_TABLE_h_LagSelect                =  2; #
our $FM_LAG_PORT_TABLE_l_Index                    =  6; #
our $FM_LAG_PORT_TABLE_h_Index                    =  12; #
our $FM_LAG_PORT_TABLE_b_HashRotation             =  13; #
our $FM_LAG_PORT_TABLE_l_LagShift                 =  3; #
our $FM_LAG_PORT_TABLE_h_LagShift                 =  5; #

our $FM_LAG_PROFILE_TABLE_l_CmdB                  =  4; #
our $FM_LAG_PROFILE_TABLE_h_CmdB                  =  6; #
our $FM_LAG_PROFILE_TABLE_b_DropCodeSelect        =  7; #
our $FM_LAG_PROFILE_TABLE_l_CmdA                  =  0; #
our $FM_LAG_PROFILE_TABLE_h_CmdA                  =  3; #

our $FM_LAG_FILTERING_CAM_l_Key                   =  64; #
our $FM_LAG_FILTERING_CAM_h_Key                   =  127; #
our $FM_LAG_FILTERING_CAM_l_KeyInvert             =  0; #
our $FM_LAG_FILTERING_CAM_h_KeyInvert             =  63; #

our $FM_SSCHED_TX_NEXT_PORT_l_NextPort            =  0; #
our $FM_SSCHED_TX_NEXT_PORT_h_NextPort            =  31; #

our $FM_SSCHED_TX_INIT_TOKEN_l_Port               =  0; #
our $FM_SSCHED_TX_INIT_TOKEN_h_Port               =  6; #
our $FM_SSCHED_TX_INIT_TOKEN_b_Synchronized       =  10; #
our $FM_SSCHED_TX_INIT_TOKEN_b_Locked             =  9; #

our $FM_SSCHED_TX_INIT_COMPLETE_b_Data            =  0; #

our $FM_SSCHED_TX_REPLACE_TOKEN_b_RwTok           =  28; #
our $FM_SSCHED_TX_REPLACE_TOKEN_b_NewLocked       =  19; #
our $FM_SSCHED_TX_REPLACE_TOKEN_b_FoundReadyCnt   =  31; #
our $FM_SSCHED_TX_REPLACE_TOKEN_l_OldReadyCnt     =  20; #
our $FM_SSCHED_TX_REPLACE_TOKEN_h_OldReadyCnt     =  23; #
our $FM_SSCHED_TX_REPLACE_TOKEN_b_NewIdle         =  18; #
our $FM_SSCHED_TX_REPLACE_TOKEN_b_RwCnt           =  29; #
our $FM_SSCHED_TX_REPLACE_TOKEN_b_OldLocked       =  9; #
our $FM_SSCHED_TX_REPLACE_TOKEN_l_NewReadyCnt     =  24; #
our $FM_SSCHED_TX_REPLACE_TOKEN_h_NewReadyCnt     =  27; #
our $FM_SSCHED_TX_REPLACE_TOKEN_l_NewPort         =  10; #
our $FM_SSCHED_TX_REPLACE_TOKEN_h_NewPort         =  16; #
our $FM_SSCHED_TX_REPLACE_TOKEN_l_OldPort         =  0; #
our $FM_SSCHED_TX_REPLACE_TOKEN_h_OldPort         =  6; #
our $FM_SSCHED_TX_REPLACE_TOKEN_b_FoundTok        =  30; #
our $FM_SSCHED_TX_REPLACE_TOKEN_b_OldIdle         =  8; #

our $FM_SSCHED_RX_NEXT_PORT_l_NextPort            =  0; #
our $FM_SSCHED_RX_NEXT_PORT_h_NextPort            =  31; #

our $FM_SSCHED_RX_INIT_TOKEN_l_Port               =  0; #
our $FM_SSCHED_RX_INIT_TOKEN_h_Port               =  6; #
our $FM_SSCHED_RX_INIT_TOKEN_b_Locked             =  9; #

our $FM_SSCHED_RX_INIT_COMPLETE_b_Data            =  0; #

our $FM_SSCHED_RX_REPLACE_TOKEN_b_NewLocked       =  19; #
our $FM_SSCHED_RX_REPLACE_TOKEN_b_NewIdle         =  18; #
our $FM_SSCHED_RX_REPLACE_TOKEN_b_OldLocked       =  9; #
our $FM_SSCHED_RX_REPLACE_TOKEN_l_NewPort         =  10; #
our $FM_SSCHED_RX_REPLACE_TOKEN_h_NewPort         =  16; #
our $FM_SSCHED_RX_REPLACE_TOKEN_l_OldPort         =  0; #
our $FM_SSCHED_RX_REPLACE_TOKEN_h_OldPort         =  6; #
our $FM_SSCHED_RX_REPLACE_TOKEN_b_Rw              =  20; #
our $FM_SSCHED_RX_REPLACE_TOKEN_b_OldIdle         =  8; #
our $FM_SSCHED_RX_REPLACE_TOKEN_b_Found           =  21; #

our $FM_SSCHED_RX_SLOW_PORT_l_SlowPort            =  0; #
our $FM_SSCHED_RX_SLOW_PORT_h_SlowPort            =  15; #

our $FM_SSCHED_RXQ_FREELIST_INIT_l_Address        =  0; #
our $FM_SSCHED_RXQ_FREELIST_INIT_h_Address        =  15; #

our $FM_SSCHED_RXQ_FREELIST_INIT_DONE_l_Address   =  0; #
our $FM_SSCHED_RXQ_FREELIST_INIT_DONE_h_Address   =  15; #

our $FM_SSCHED_TXQ_FREELIST_INIT_l_Address        =  0; #
our $FM_SSCHED_TXQ_FREELIST_INIT_h_Address        =  15; #

our $FM_SSCHED_TXQ_FREELIST_INIT_DONE_l_Address   =  0; #
our $FM_SSCHED_TXQ_FREELIST_INIT_DONE_h_Address   =  15; #

our $FM_SSCHED_HS_FREELIST_INIT_l_Address         =  0; #
our $FM_SSCHED_HS_FREELIST_INIT_h_Address         =  15; #

our $FM_SSCHED_HS_FREELIST_INIT_DONE_l_Address    =  0; #
our $FM_SSCHED_HS_FREELIST_INIT_DONE_h_Address    =  15; #

our $FM_SSCHED_FREELIST_INIT_l_Address            =  0; #
our $FM_SSCHED_FREELIST_INIT_h_Address            =  15; #

our $FM_SSCHED_FREELIST_INIT_DONE_l_Address       =  0; #
our $FM_SSCHED_FREELIST_INIT_DONE_h_Address       =  15; #

our $FM_HASH_LAYER2_KEY_PROFILE_l_QOS_L2_VPRI2_EVID2__BitMask  =  128; #
our $FM_HASH_LAYER2_KEY_PROFILE_h_QOS_L2_VPRI2_EVID2__BitMask  =  143; #
our $FM_HASH_LAYER2_KEY_PROFILE_b_UseL3A          =  148; #
our $FM_HASH_LAYER2_KEY_PROFILE_b_ACTION_DATA_W16C_HalfWordMask  =  146; #
our $FM_HASH_LAYER2_KEY_PROFILE_b_UseL3B          =  149; #
our $FM_HASH_LAYER2_KEY_PROFILE_l_MA2_MAC_BitMask  =  48; #
our $FM_HASH_LAYER2_KEY_PROFILE_h_MA2_MAC_BitMask  =  95; #
our $FM_HASH_LAYER2_KEY_PROFILE_l_MA1_MAC_BitMask  =  0; #
our $FM_HASH_LAYER2_KEY_PROFILE_h_MA1_MAC_BitMask  =  47; #
our $FM_HASH_LAYER2_KEY_PROFILE_b_ACTION_DATA_W16A_HalfWordMask  =  144; #
our $FM_HASH_LAYER2_KEY_PROFILE_b_ACTION_DATA_W16B_HalfWordMask  =  145; #
our $FM_HASH_LAYER2_KEY_PROFILE_b_SymmetrizeMAC   =  147; #
our $FM_HASH_LAYER2_KEY_PROFILE_l_QOS_L2_VPRI1_EVID1__BitMask  =  112; #
our $FM_HASH_LAYER2_KEY_PROFILE_h_QOS_L2_VPRI1_EVID1__BitMask  =  127; #
our $FM_HASH_LAYER2_KEY_PROFILE_l_L2_TYPE_BitMask  =  96; #
our $FM_HASH_LAYER2_KEY_PROFILE_h_L2_TYPE_BitMask  =  111; #

our $FM_HASH_LAYER2_FUNC_PROFILE_b_ComputeRotA    =  22; #
our $FM_HASH_LAYER2_FUNC_PROFILE_b_RandomizeRotB  =  27; #
our $FM_HASH_LAYER2_FUNC_PROFILE_b_UsePTableRotB  =  25; #
our $FM_HASH_LAYER2_FUNC_PROFILE_b_ComputeRotB    =  23; #
our $FM_HASH_LAYER2_FUNC_PROFILE_b_RandomizeRotA  =  26; #
our $FM_HASH_LAYER2_FUNC_PROFILE_b_Select_HASH_ROT_B  =  21; #
our $FM_HASH_LAYER2_FUNC_PROFILE_l_Value_HASH_ROT  =  0; #
our $FM_HASH_LAYER2_FUNC_PROFILE_h_Value_HASH_ROT  =  19; #
our $FM_HASH_LAYER2_FUNC_PROFILE_b_UsePTableRotA  =  24; #
our $FM_HASH_LAYER2_FUNC_PROFILE_b_Select_HASH_ROT_A  =  20; #

our $FM_HASH_LAYER2_ROTA_PTABLE_l_Value_0         =  0; #
our $FM_HASH_LAYER2_ROTA_PTABLE_h_Value_0         =  5; #
our $FM_HASH_LAYER2_ROTA_PTABLE_l_Value_1         =  6; #
our $FM_HASH_LAYER2_ROTA_PTABLE_h_Value_1         =  11; #
our $FM_HASH_LAYER2_ROTA_PTABLE_l_Value_2         =  12; #
our $FM_HASH_LAYER2_ROTA_PTABLE_h_Value_2         =  17; #
our $FM_HASH_LAYER2_ROTA_PTABLE_l_Value_3         =  18; #
our $FM_HASH_LAYER2_ROTA_PTABLE_h_Value_3         =  23; #

our $FM_HASH_LAYER2_ROTB_PTABLE_l_Value_0         =  0; #
our $FM_HASH_LAYER2_ROTB_PTABLE_h_Value_0         =  5; #
our $FM_HASH_LAYER2_ROTB_PTABLE_l_Value_1         =  6; #
our $FM_HASH_LAYER2_ROTB_PTABLE_h_Value_1         =  11; #
our $FM_HASH_LAYER2_ROTB_PTABLE_l_Value_2         =  12; #
our $FM_HASH_LAYER2_ROTB_PTABLE_h_Value_2         =  17; #
our $FM_HASH_LAYER2_ROTB_PTABLE_l_Value_3         =  18; #
our $FM_HASH_LAYER2_ROTB_PTABLE_h_Value_3         =  23; #

our $FM_ALU_CMD_TABLE_l_RollX                     =  1; #
our $FM_ALU_CMD_TABLE_h_RollX                     =  4; #
our $FM_ALU_CMD_TABLE_l_MaskY                     =  25; #
our $FM_ALU_CMD_TABLE_h_MaskY                     =  40; #
our $FM_ALU_CMD_TABLE_l_MaskX                     =  5; #
our $FM_ALU_CMD_TABLE_h_MaskX                     =  20; #
our $FM_ALU_CMD_TABLE_b_OrIntoL2ARKey             =  45; #
our $FM_ALU_CMD_TABLE_l_RollY                     =  21; #
our $FM_ALU_CMD_TABLE_h_RollY                     =  24; #
our $FM_ALU_CMD_TABLE_b_OneTwo                    =  44; #
our $FM_ALU_CMD_TABLE_b_SwapXY                    =  0; #
our $FM_ALU_CMD_TABLE_l_Function                  =  42; #
our $FM_ALU_CMD_TABLE_h_Function                  =  43; #
our $FM_ALU_CMD_TABLE_b_MapY                      =  41; #

our $FM_ALU_Y_TABLE_l_Value                       =  0; #
our $FM_ALU_Y_TABLE_h_Value                       =  15; #

our $FM_L2L_SWEEPER_TIMER_CFG_l_StartIndex        =  0; #
our $FM_L2L_SWEEPER_TIMER_CFG_h_StartIndex        =  15; #
our $FM_L2L_SWEEPER_TIMER_CFG_b_SinglePass        =  33; #
our $FM_L2L_SWEEPER_TIMER_CFG_b_Enable            =  32; #
our $FM_L2L_SWEEPER_TIMER_CFG_l_StopIndex         =  16; #
our $FM_L2L_SWEEPER_TIMER_CFG_h_StopIndex         =  31; #

our $FM_L2L_SWEEPER_TIMER_STATUS_l_SweepCount     =  16; #
our $FM_L2L_SWEEPER_TIMER_STATUS_h_SweepCount     =  31; #
our $FM_L2L_SWEEPER_TIMER_STATUS_b_Blocked        =  32; #
our $FM_L2L_SWEEPER_TIMER_STATUS_l_SweeperIndex   =  0; #
our $FM_L2L_SWEEPER_TIMER_STATUS_h_SweeperIndex   =  15; #

our $FM_L2L_SWEEPER_CAM_l_Key                     =  64; #
our $FM_L2L_SWEEPER_CAM_h_Key                     =  127; #
our $FM_L2L_SWEEPER_CAM_l_KeyInvert               =  0; #
our $FM_L2L_SWEEPER_CAM_h_KeyInvert               =  63; #

our $FM_L2L_SWEEPER_RAM_l_DecrementMaskDATA       =  76; #
our $FM_L2L_SWEEPER_RAM_h_DecrementMaskDATA       =  83; #
our $FM_L2L_SWEEPER_RAM_l_ReplaceMaskGLORT        =  38; #
our $FM_L2L_SWEEPER_RAM_h_ReplaceMaskGLORT        =  53; #
our $FM_L2L_SWEEPER_RAM_l_ReplaceValuePrec        =  36; #
our $FM_L2L_SWEEPER_RAM_h_ReplaceValuePrec        =  37; #
our $FM_L2L_SWEEPER_RAM_b_ReportMatch             =  84; #
our $FM_L2L_SWEEPER_RAM_l_ReplaceMaskTAG          =  62; #
our $FM_L2L_SWEEPER_RAM_h_ReplaceMaskTAG          =  73; #
our $FM_L2L_SWEEPER_RAM_l_ReplaceMaskPrec         =  74; #
our $FM_L2L_SWEEPER_RAM_h_ReplaceMaskPrec         =  75; #
our $FM_L2L_SWEEPER_RAM_l_ReplaceValueDATA        =  16; #
our $FM_L2L_SWEEPER_RAM_h_ReplaceValueDATA        =  23; #
our $FM_L2L_SWEEPER_RAM_l_ReplaceMaskDATA         =  54; #
our $FM_L2L_SWEEPER_RAM_h_ReplaceMaskDATA         =  61; #
our $FM_L2L_SWEEPER_RAM_l_ReplaceValueTAG         =  24; #
our $FM_L2L_SWEEPER_RAM_h_ReplaceValueTAG         =  35; #
our $FM_L2L_SWEEPER_RAM_l_ReplaceValueGLORT       =  0; #
our $FM_L2L_SWEEPER_RAM_h_ReplaceValueGLORT       =  15; #

our $FM_L2L_SWEEPER_FIFO_l_Entry                  =  0; #
our $FM_L2L_SWEEPER_FIFO_h_Entry                  =  15; #
our $FM_L2L_SWEEPER_FIFO_l_SweeperReportMask      =  16; #
our $FM_L2L_SWEEPER_FIFO_h_SweeperReportMask      =  31; #

our $FM_L2L_SWEEPER_FIFO_HEAD_l_Pointer           =  0; #
our $FM_L2L_SWEEPER_FIFO_HEAD_h_Pointer           =  8; #

our $FM_L2L_SWEEPER_FIFO_TAIL_l_Pointer           =  0; #
our $FM_L2L_SWEEPER_FIFO_TAIL_h_Pointer           =  8; #

our $FM_L2L_SWEEPER_IP_l_SweepComplete            =  0; #
our $FM_L2L_SWEEPER_IP_h_SweepComplete            =  1; #
our $FM_L2L_SWEEPER_IP_b_FifoNonEmpty             =  2; #

our $FM_L2L_SWEEPER_IM_l_Mask                     =  0; #
our $FM_L2L_SWEEPER_IM_h_Mask                     =  2; #

our $FM_L2L_SWEEPER_WRITE_COMMAND_b_Valid         =  16; #
our $FM_L2L_SWEEPER_WRITE_COMMAND_l_Index         =  0; #
our $FM_L2L_SWEEPER_WRITE_COMMAND_h_Index         =  15; #

our $FM_L2L_SWEEPER_WRITE_DATA_l_TAG              =  96; #
our $FM_L2L_SWEEPER_WRITE_DATA_h_TAG              =  107; #
our $FM_L2L_SWEEPER_WRITE_DATA_l_DATA             =  108; #
our $FM_L2L_SWEEPER_WRITE_DATA_h_DATA             =  115; #
our $FM_L2L_SWEEPER_WRITE_DATA_l_FID1             =  48; #
our $FM_L2L_SWEEPER_WRITE_DATA_h_FID1             =  59; #
our $FM_L2L_SWEEPER_WRITE_DATA_l_MAC              =  0; #
our $FM_L2L_SWEEPER_WRITE_DATA_h_MAC              =  47; #
our $FM_L2L_SWEEPER_WRITE_DATA_l_GLORT            =  80; #
our $FM_L2L_SWEEPER_WRITE_DATA_h_GLORT            =  95; #
our $FM_L2L_SWEEPER_WRITE_DATA_l_FID2             =  60; #
our $FM_L2L_SWEEPER_WRITE_DATA_h_FID2             =  71; #
our $FM_L2L_SWEEPER_WRITE_DATA_l_Prec             =  72; #
our $FM_L2L_SWEEPER_WRITE_DATA_h_Prec             =  73; #

our $FM_GLORT_CAM_l_Key                           =  16; #
our $FM_GLORT_CAM_h_Key                           =  31; #
our $FM_GLORT_CAM_l_KeyInvert                     =  0; #
our $FM_GLORT_CAM_h_KeyInvert                     =  15; #

our $FM_GLORT_RAM_l_HashCmd                       =  0; #
our $FM_GLORT_RAM_h_HashCmd                       =  1; #
our $FM_GLORT_RAM_l_DMaskRange                    =  34; #
our $FM_GLORT_RAM_h_DMaskRange                    =  40; #
our $FM_GLORT_RAM_l_RangeSubIndexA                =  18; #
our $FM_GLORT_RAM_h_RangeSubIndexA                =  25; #
our $FM_GLORT_RAM_l_RangeSubIndexB                =  26; #
our $FM_GLORT_RAM_h_RangeSubIndexB                =  33; #
our $FM_GLORT_RAM_l_DMaskBaseIdx                  =  2; #
our $FM_GLORT_RAM_h_DMaskBaseIdx                  =  17; #
our $FM_GLORT_RAM_b_HashRotation                  =  41; #
our $FM_GLORT_RAM_l_StrictCmd                     =  0; #
our $FM_GLORT_RAM_h_StrictCmd                     =  1; #
our $FM_GLORT_RAM_b_DGlortTag                     =  42; #

our $FM_SBUS_CFG_b_SBUS_ControllerReset           =  0; #

our $FM_SBUS_COMMAND_l_Register                   =  0; #
our $FM_SBUS_COMMAND_h_Register                   =  7; #
our $FM_SBUS_COMMAND_b_Execute                    =  24; #
our $FM_SBUS_COMMAND_l_Op                         =  16; #
our $FM_SBUS_COMMAND_h_Op                         =  23; #
our $FM_SBUS_COMMAND_l_Address                    =  8; #
our $FM_SBUS_COMMAND_h_Address                    =  15; #
our $FM_SBUS_COMMAND_b_Busy                       =  25; #
our $FM_SBUS_COMMAND_l_ResultCode                 =  26; #
our $FM_SBUS_COMMAND_h_ResultCode                 =  28; #

our $FM_SBUS_REQUEST_l_Data                       =  0; #
our $FM_SBUS_REQUEST_h_Data                       =  31; #

our $FM_SBUS_RESPONSE_l_Data                      =  0; #
our $FM_SBUS_RESPONSE_h_Data                      =  31; #

our $FM_SBUS_SPICO_l_DataIn                       =  13; #
our $FM_SBUS_SPICO_h_DataIn                       =  22; #
our $FM_SBUS_SPICO_b_Enable                       =  1; #
our $FM_SBUS_SPICO_b_Reset                        =  0; #
our $FM_SBUS_SPICO_b_Interrupt                    =  2; #
our $FM_SBUS_SPICO_l_DataOut                      =  3; #
our $FM_SBUS_SPICO_h_DataOut                      =  12; #

our $FM_SBUS_IP_l_DataDetectLow                   =  10; #
our $FM_SBUS_IP_h_DataDetectLow                   =  19; #
our $FM_SBUS_IP_l_DataDetectHigh                  =  0; #
our $FM_SBUS_IP_h_DataDetectHigh                  =  9; #

our $FM_SBUS_IM_l_MaskDataDetectLow               =  10; #
our $FM_SBUS_IM_h_MaskDataDetectLow               =  19; #
our $FM_SBUS_IM_l_MaskDataDetectHigh              =  0; #
our $FM_SBUS_IM_h_MaskDataDetectHigh              =  9; #

our $FM_SPICO_BIST_b_BIST_RUN                     =  1; #
our $FM_SPICO_BIST_b_BIST_DONE_FAIL_OUT           =  3; #
our $FM_SPICO_BIST_b_BIST_DONE_PASS_OUT           =  2; #
our $FM_SPICO_BIST_b_BIST_RST                     =  0; #
our $FM_SPICO_BIST_b_BIST_DONE_PASS_OUT_IMEM      =  4; #
our $FM_SPICO_BIST_b_BIST_DONE_FAIL_OUT_IMEM      =  5; #

our $FM_TAP_FSM_STATE_b_SIR                       =  7; #
our $FM_TAP_FSM_STATE_b_UDR                       =  4; #
our $FM_TAP_FSM_STATE_b_SDR                       =  3; #
our $FM_TAP_FSM_STATE_b_CIR                       =  6; #
our $FM_TAP_FSM_STATE_b_TLR                       =  0; #
our $FM_TAP_FSM_STATE_b_CDR                       =  2; #
our $FM_TAP_FSM_STATE_b_E1DR                      =  5; #
our $FM_TAP_FSM_STATE_b_UIR                       =  8; #
our $FM_TAP_FSM_STATE_b_RTI                       =  1; #

our $FM_TAP_EFUSE_CFG_b_Read                      =  1; #
our $FM_TAP_EFUSE_CFG_b_ClkEnable                 =  0; #

our $FM_TAP_EFUSE_l_DATA                          =  0; #
our $FM_TAP_EFUSE_h_DATA                          =  127; #

our $FM_SSCHED_TICK_CFG_l_Period                  =  0; #
our $FM_SSCHED_TICK_CFG_h_Period                  =  7; #

our $FM_L3AR_CAM_l_Key                            =  64; #
our $FM_L3AR_CAM_h_Key                            =  127; #
our $FM_L3AR_CAM_l_KeyInvert                      =  0; #
our $FM_L3AR_CAM_h_KeyInvert                      =  63; #

our $FM_L3AR_SLICE_CFG_l_SliceDisable             =  5; #
our $FM_L3AR_SLICE_CFG_h_SliceDisable             =  9; #
our $FM_L3AR_SLICE_CFG_l_ChainedPrecedence        =  0; #
our $FM_L3AR_SLICE_CFG_h_ChainedPrecedence        =  4; #

our $FM_L3AR_KEY_CFG_b_EnableKeySet_0             =  0; #
our $FM_L3AR_KEY_CFG_b_EnableKeySet_3             =  3; #
our $FM_L3AR_KEY_CFG_b_EnableKeySet_1             =  1; #
our $FM_L3AR_KEY_CFG_b_EnableKeySet_2             =  2; #

our $FM_L3AR_ACTION_CFG_b_EnableRAM1              =  0; #
our $FM_L3AR_ACTION_CFG_b_EnableRAM2              =  1; #
our $FM_L3AR_ACTION_CFG_b_EnableRAM3              =  2; #
our $FM_L3AR_ACTION_CFG_b_EnableRAM4              =  3; #
our $FM_L3AR_ACTION_CFG_b_EnableRAM5              =  4; #

our $FM_L3AR_RAM1_l_Mask_ACTION_FLAGS_LO          =  0; #
our $FM_L3AR_RAM1_h_Mask_ACTION_FLAGS_LO          =  25; #
our $FM_L3AR_RAM1_l_Set_ACTION_FLAGS_LO           =  32; #
our $FM_L3AR_RAM1_h_Set_ACTION_FLAGS_LO           =  57; #

our $FM_L3AR_RAM2_l_Mask_ACTION_FLAGS_HI          =  0; #
our $FM_L3AR_RAM2_h_Mask_ACTION_FLAGS_HI          =  25; #
our $FM_L3AR_RAM2_l_Set_ACTION_FLAGS_HI           =  32; #
our $FM_L3AR_RAM2_h_Set_ACTION_FLAGS_HI           =  57; #

our $FM_L3AR_RAM3_l_VID_PROFILE                   =  21; #
our $FM_L3AR_RAM3_h_VID_PROFILE                   =  25; #
our $FM_L3AR_RAM3_b_TRAP_HEADER_IDX               =  2; #
our $FM_L3AR_RAM3_b_SetTrapHeaderCmd              =  0; #
our $FM_L3AR_RAM3_b_MuxOutput_VID                 =  20; #
our $FM_L3AR_RAM3_b_SetL2LookupCmdProfile         =  3; #
our $FM_L3AR_RAM3_l_MA_FID_PROFILE                =  27; #
our $FM_L3AR_RAM3_h_MA_FID_PROFILE                =  31; #
our $FM_L3AR_RAM3_l_L2L_CMD_PROFILE               =  4; #
our $FM_L3AR_RAM3_h_L2L_CMD_PROFILE               =  7; #
our $FM_L3AR_RAM3_b_MuxOutput_MA1_MAC             =  8; #
our $FM_L3AR_RAM3_l_MA1_MAC_PROFILE               =  9; #
our $FM_L3AR_RAM3_h_MA1_MAC_PROFILE               =  13; #
our $FM_L3AR_RAM3_b_MuxOutput_MA_FID              =  26; #
our $FM_L3AR_RAM3_b_TRAP_HEADER_ENABLE            =  1; #
our $FM_L3AR_RAM3_l_MA2_MAC_PROFILE               =  15; #
our $FM_L3AR_RAM3_h_MA2_MAC_PROFILE               =  19; #
our $FM_L3AR_RAM3_b_MuxOutput_MA2_MAC             =  14; #

our $FM_L3AR_RAM4_b_SetHashProfile                =  0; #
our $FM_L3AR_RAM4_l_POL3_IDX_PROFILE              =  45; #
our $FM_L3AR_RAM4_h_POL3_IDX_PROFILE              =  48; #
our $FM_L3AR_RAM4_b_MuxOutput_POL3_IDX            =  44; #
our $FM_L3AR_RAM4_b_MuxOutput_POL1_IDX            =  34; #
our $FM_L3AR_RAM4_l_POL2_IDX_PROFILE              =  40; #
our $FM_L3AR_RAM4_h_POL2_IDX_PROFILE              =  43; #
our $FM_L3AR_RAM4_l_ALU13_OP_PROFILE              =  23; #
our $FM_L3AR_RAM4_h_ALU13_OP_PROFILE              =  27; #
our $FM_L3AR_RAM4_b_SetAlu46CmdProfile            =  16; #
our $FM_L3AR_RAM4_l_ALU46_CMD_PROFILE             =  17; #
our $FM_L3AR_RAM4_h_ALU46_CMD_PROFILE             =  21; #
our $FM_L3AR_RAM4_l_HASH_ROT_PROFILE              =  6; #
our $FM_L3AR_RAM4_h_HASH_ROT_PROFILE              =  9; #
our $FM_L3AR_RAM4_b_MuxOutput_ALU46_OP            =  28; #
our $FM_L3AR_RAM4_b_MuxOutput_QOS                 =  49; #
our $FM_L3AR_RAM4_b_SetAlu13CmdProfile            =  10; #
our $FM_L3AR_RAM4_l_ALU13_CMD_PROFILE             =  11; #
our $FM_L3AR_RAM4_h_ALU13_CMD_PROFILE             =  15; #
our $FM_L3AR_RAM4_b_MuxOutput_HASH_ROT            =  5; #
our $FM_L3AR_RAM4_b_MuxOutput_ALU13_OP            =  22; #
our $FM_L3AR_RAM4_l_HASH_PROFILE                  =  1; #
our $FM_L3AR_RAM4_h_HASH_PROFILE                  =  4; #
our $FM_L3AR_RAM4_l_ALU46_OP_PROFILE              =  29; #
our $FM_L3AR_RAM4_h_ALU46_OP_PROFILE              =  33; #
our $FM_L3AR_RAM4_l_QOS_PROFILE                   =  50; #
our $FM_L3AR_RAM4_h_QOS_PROFILE                   =  54; #
our $FM_L3AR_RAM4_b_MuxOutput_POL2_IDX            =  39; #
our $FM_L3AR_RAM4_l_POL1_IDX_PROFILE              =  35; #
our $FM_L3AR_RAM4_h_POL1_IDX_PROFILE              =  38; #

our $FM_L3AR_RAM5_b_SetDestMaskCmdProfile         =  18; #
our $FM_L3AR_RAM5_l_W8ABCD_PROFILE                =  24; #
our $FM_L3AR_RAM5_h_W8ABCD_PROFILE                =  28; #
our $FM_L3AR_RAM5_l_W16DEF_PROFILE                =  48; #
our $FM_L3AR_RAM5_h_W16DEF_PROFILE                =  52; #
our $FM_L3AR_RAM5_l_W16GH_PROFILE                 =  54; #
our $FM_L3AR_RAM5_h_W16GH_PROFILE                 =  58; #
our $FM_L3AR_RAM5_l_CSGLORT_PROFILE               =  13; #
our $FM_L3AR_RAM5_h_CSGLORT_PROFILE               =  17; #
our $FM_L3AR_RAM5_b_MuxOutput_W16DEF              =  47; #
our $FM_L3AR_RAM5_b_MuxOutput_W16ABC              =  41; #
our $FM_L3AR_RAM5_l_W8F_PROFILE                   =  36; #
our $FM_L3AR_RAM5_h_W8F_PROFILE                   =  40; #
our $FM_L3AR_RAM5_l_DGLORT_PROFILE                =  1; #
our $FM_L3AR_RAM5_h_DGLORT_PROFILE                =  5; #
our $FM_L3AR_RAM5_l_W16ABC_PROFILE                =  42; #
our $FM_L3AR_RAM5_h_W16ABC_PROFILE                =  46; #
our $FM_L3AR_RAM5_l_SGLORT_PROFILE                =  7; #
our $FM_L3AR_RAM5_h_SGLORT_PROFILE                =  11; #
our $FM_L3AR_RAM5_b_MuxOutput_CSGLORT             =  12; #
our $FM_L3AR_RAM5_b_MuxOutput_W8F                 =  35; #
our $FM_L3AR_RAM5_b_MuxOutput_W8E                 =  29; #
our $FM_L3AR_RAM5_l_DMASK_CMD_PROFILE             =  19; #
our $FM_L3AR_RAM5_h_DMASK_CMD_PROFILE             =  22; #
our $FM_L3AR_RAM5_b_MuxOutput_SGLORT              =  6; #
our $FM_L3AR_RAM5_l_W8E_PROFILE                   =  30; #
our $FM_L3AR_RAM5_h_W8E_PROFILE                   =  34; #
our $FM_L3AR_RAM5_b_MuxOutput_DGLORT              =  0; #
our $FM_L3AR_RAM5_b_MuxOutput_W8ABCD              =  23; #
our $FM_L3AR_RAM5_b_MuxOutput_W16GH               =  53; #

our $FM_L3AR_DGLORT_PROFILE_TABLE_l_Value_DGLORT  =  0; #
our $FM_L3AR_DGLORT_PROFILE_TABLE_h_Value_DGLORT  =  15; #
our $FM_L3AR_DGLORT_PROFILE_TABLE_l_Select_DGLORT  =  32; #
our $FM_L3AR_DGLORT_PROFILE_TABLE_h_Select_DGLORT  =  34; #
our $FM_L3AR_DGLORT_PROFILE_TABLE_l_Mask_DGLORT   =  16; #
our $FM_L3AR_DGLORT_PROFILE_TABLE_h_Mask_DGLORT   =  31; #

our $FM_L3AR_SGLORT_PROFILE_TABLE_l_Mask_SGLORT   =  16; #
our $FM_L3AR_SGLORT_PROFILE_TABLE_h_Mask_SGLORT   =  31; #
our $FM_L3AR_SGLORT_PROFILE_TABLE_l_Value_SGLORT  =  0; #
our $FM_L3AR_SGLORT_PROFILE_TABLE_h_Value_SGLORT  =  15; #
our $FM_L3AR_SGLORT_PROFILE_TABLE_l_Select_SGLORT  =  32; #
our $FM_L3AR_SGLORT_PROFILE_TABLE_h_Select_SGLORT  =  34; #

our $FM_L3AR_W8ABCD_PROFILE_TABLE_l_Value_ACTION_DATA_W8C  =  16; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_h_Value_ACTION_DATA_W8C  =  23; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_l_Mask_ACTION_DATA_W8D  =  56; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_h_Mask_ACTION_DATA_W8D  =  63; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_l_Value_ACTION_DATA_W8D  =  24; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_h_Value_ACTION_DATA_W8D  =  31; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_l_Select_ACTION_DATA_W8D  =  70; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_h_Select_ACTION_DATA_W8D  =  71; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_l_Select_ACTION_DATA_W8C  =  68; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_h_Select_ACTION_DATA_W8C  =  69; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_l_Mask_ACTION_DATA_W8A  =  32; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_h_Mask_ACTION_DATA_W8A  =  39; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_l_Select_ACTION_DATA_W8B  =  66; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_h_Select_ACTION_DATA_W8B  =  67; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_l_Select_ACTION_DATA_W8A  =  64; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_h_Select_ACTION_DATA_W8A  =  65; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_l_Mask_ACTION_DATA_W8C  =  48; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_h_Mask_ACTION_DATA_W8C  =  55; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_l_Mask_ACTION_DATA_W8B  =  40; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_h_Mask_ACTION_DATA_W8B  =  47; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_l_Value_ACTION_DATA_W8A  =  0; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_h_Value_ACTION_DATA_W8A  =  7; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_l_Value_ACTION_DATA_W8B  =  8; #
our $FM_L3AR_W8ABCD_PROFILE_TABLE_h_Value_ACTION_DATA_W8B  =  15; #

our $FM_L3AR_W8E_PROFILE_TABLE_l_Mask_ACTION_DATA_W8E  =  8; #
our $FM_L3AR_W8E_PROFILE_TABLE_h_Mask_ACTION_DATA_W8E  =  15; #
our $FM_L3AR_W8E_PROFILE_TABLE_l_Select_ACTION_DATA_W8E  =  16; #
our $FM_L3AR_W8E_PROFILE_TABLE_h_Select_ACTION_DATA_W8E  =  18; #
our $FM_L3AR_W8E_PROFILE_TABLE_l_Value_ACTION_DATA_W8E  =  0; #
our $FM_L3AR_W8E_PROFILE_TABLE_h_Value_ACTION_DATA_W8E  =  7; #

our $FM_L3AR_W8F_PROFILE_TABLE_l_Select_ACTION_DATA_W8F  =  16; #
our $FM_L3AR_W8F_PROFILE_TABLE_h_Select_ACTION_DATA_W8F  =  17; #
our $FM_L3AR_W8F_PROFILE_TABLE_l_Mask_ACTION_DATA_W8F  =  8; #
our $FM_L3AR_W8F_PROFILE_TABLE_h_Mask_ACTION_DATA_W8F  =  15; #
our $FM_L3AR_W8F_PROFILE_TABLE_l_Value_ACTION_DATA_W8F  =  0; #
our $FM_L3AR_W8F_PROFILE_TABLE_h_Value_ACTION_DATA_W8F  =  7; #

our $FM_L3AR_MA1_MAC_PROFILE_TABLE_l_Value_MA1_MAC  =  0; #
our $FM_L3AR_MA1_MAC_PROFILE_TABLE_h_Value_MA1_MAC  =  47; #
our $FM_L3AR_MA1_MAC_PROFILE_TABLE_l_Select_MA1_MAC  =  48; #
our $FM_L3AR_MA1_MAC_PROFILE_TABLE_h_Select_MA1_MAC  =  50; #

our $FM_L3AR_MA2_MAC_PROFILE_TABLE_l_Select_MA2_MAC  =  48; #
our $FM_L3AR_MA2_MAC_PROFILE_TABLE_h_Select_MA2_MAC  =  49; #
our $FM_L3AR_MA2_MAC_PROFILE_TABLE_l_Value_MA2_MAC  =  0; #
our $FM_L3AR_MA2_MAC_PROFILE_TABLE_h_Value_MA2_MAC  =  47; #

our $FM_L3AR_VID_PROFILE_TABLE_l_Value_EVID2      =  36; #
our $FM_L3AR_VID_PROFILE_TABLE_h_Value_EVID2      =  47; #
our $FM_L3AR_VID_PROFILE_TABLE_l_Value_IVID1      =  0; #
our $FM_L3AR_VID_PROFILE_TABLE_h_Value_IVID1      =  11; #
our $FM_L3AR_VID_PROFILE_TABLE_l_Select_IVID2     =  51; #
our $FM_L3AR_VID_PROFILE_TABLE_h_Select_IVID2     =  53; #
our $FM_L3AR_VID_PROFILE_TABLE_l_Select_IVID1     =  48; #
our $FM_L3AR_VID_PROFILE_TABLE_h_Select_IVID1     =  50; #
our $FM_L3AR_VID_PROFILE_TABLE_l_Value_EVID1      =  24; #
our $FM_L3AR_VID_PROFILE_TABLE_h_Value_EVID1      =  35; #
our $FM_L3AR_VID_PROFILE_TABLE_l_Select_EVID1     =  54; #
our $FM_L3AR_VID_PROFILE_TABLE_h_Select_EVID1     =  56; #
our $FM_L3AR_VID_PROFILE_TABLE_l_Select_EVID2     =  57; #
our $FM_L3AR_VID_PROFILE_TABLE_h_Select_EVID2     =  59; #
our $FM_L3AR_VID_PROFILE_TABLE_l_Value_IVID2      =  12; #
our $FM_L3AR_VID_PROFILE_TABLE_h_Value_IVID2      =  23; #

our $FM_L3AR_MA_FID_PROFILE_TABLE_l_Value_MA2_FID2  =  12; #
our $FM_L3AR_MA_FID_PROFILE_TABLE_h_Value_MA2_FID2  =  23; #
our $FM_L3AR_MA_FID_PROFILE_TABLE_l_Select_MA1_FID1  =  52; #
our $FM_L3AR_MA_FID_PROFILE_TABLE_h_Select_MA1_FID1  =  53; #
our $FM_L3AR_MA_FID_PROFILE_TABLE_l_Value_MA1_FID1  =  24; #
our $FM_L3AR_MA_FID_PROFILE_TABLE_h_Value_MA1_FID1  =  35; #
our $FM_L3AR_MA_FID_PROFILE_TABLE_l_Value_MA1_FID2  =  36; #
our $FM_L3AR_MA_FID_PROFILE_TABLE_h_Value_MA1_FID2  =  47; #
our $FM_L3AR_MA_FID_PROFILE_TABLE_l_Select_MA1_FID2  =  54; #
our $FM_L3AR_MA_FID_PROFILE_TABLE_h_Select_MA1_FID2  =  55; #
our $FM_L3AR_MA_FID_PROFILE_TABLE_l_Select_MA2_FID1  =  48; #
our $FM_L3AR_MA_FID_PROFILE_TABLE_h_Select_MA2_FID1  =  49; #
our $FM_L3AR_MA_FID_PROFILE_TABLE_l_Select_MA2_FID2  =  50; #
our $FM_L3AR_MA_FID_PROFILE_TABLE_h_Select_MA2_FID2  =  51; #
our $FM_L3AR_MA_FID_PROFILE_TABLE_l_Value_MA2_FID1  =  0; #
our $FM_L3AR_MA_FID_PROFILE_TABLE_h_Value_MA2_FID1  =  11; #

our $FM_L3AR_CSGLORT_PROFILE_TABLE_l_Value_CSGLORT  =  0; #
our $FM_L3AR_CSGLORT_PROFILE_TABLE_h_Value_CSGLORT  =  15; #
our $FM_L3AR_CSGLORT_PROFILE_TABLE_l_Mask_CSGLORT  =  16; #
our $FM_L3AR_CSGLORT_PROFILE_TABLE_h_Mask_CSGLORT  =  31; #
our $FM_L3AR_CSGLORT_PROFILE_TABLE_l_Select_CSGLORT  =  32; #
our $FM_L3AR_CSGLORT_PROFILE_TABLE_h_Select_CSGLORT  =  33; #

our $FM_L3AR_W16ABC_PROFILE_TABLE_l_Select_ACTION_DATA_W16B  =  51; #
our $FM_L3AR_W16ABC_PROFILE_TABLE_h_Select_ACTION_DATA_W16B  =  53; #
our $FM_L3AR_W16ABC_PROFILE_TABLE_l_Select_ACTION_DATA_W16A  =  48; #
our $FM_L3AR_W16ABC_PROFILE_TABLE_h_Select_ACTION_DATA_W16A  =  50; #
our $FM_L3AR_W16ABC_PROFILE_TABLE_l_Select_ACTION_DATA_W16C  =  54; #
our $FM_L3AR_W16ABC_PROFILE_TABLE_h_Select_ACTION_DATA_W16C  =  56; #
our $FM_L3AR_W16ABC_PROFILE_TABLE_l_Value_ACTION_DATA_W16A  =  0; #
our $FM_L3AR_W16ABC_PROFILE_TABLE_h_Value_ACTION_DATA_W16A  =  15; #
our $FM_L3AR_W16ABC_PROFILE_TABLE_l_Value_ACTION_DATA_W16C  =  32; #
our $FM_L3AR_W16ABC_PROFILE_TABLE_h_Value_ACTION_DATA_W16C  =  47; #
our $FM_L3AR_W16ABC_PROFILE_TABLE_l_Value_ACTION_DATA_W16B  =  16; #
our $FM_L3AR_W16ABC_PROFILE_TABLE_h_Value_ACTION_DATA_W16B  =  31; #

our $FM_L3AR_W16DEF_PROFILE_TABLE_l_Select_ACTION_DATA_W16D  =  48; #
our $FM_L3AR_W16DEF_PROFILE_TABLE_h_Select_ACTION_DATA_W16D  =  50; #
our $FM_L3AR_W16DEF_PROFILE_TABLE_l_Value_ACTION_DATA_W16F  =  32; #
our $FM_L3AR_W16DEF_PROFILE_TABLE_h_Value_ACTION_DATA_W16F  =  47; #
our $FM_L3AR_W16DEF_PROFILE_TABLE_l_Select_ACTION_DATA_W16F  =  54; #
our $FM_L3AR_W16DEF_PROFILE_TABLE_h_Select_ACTION_DATA_W16F  =  56; #
our $FM_L3AR_W16DEF_PROFILE_TABLE_l_Select_ACTION_DATA_W16E  =  51; #
our $FM_L3AR_W16DEF_PROFILE_TABLE_h_Select_ACTION_DATA_W16E  =  53; #
our $FM_L3AR_W16DEF_PROFILE_TABLE_l_Value_ACTION_DATA_W16E  =  16; #
our $FM_L3AR_W16DEF_PROFILE_TABLE_h_Value_ACTION_DATA_W16E  =  31; #
our $FM_L3AR_W16DEF_PROFILE_TABLE_l_Value_ACTION_DATA_W16D  =  0; #
our $FM_L3AR_W16DEF_PROFILE_TABLE_h_Value_ACTION_DATA_W16D  =  15; #

our $FM_L3AR_W16GH_PROFILE_TABLE_l_Value_ACTION_DATA_W16G  =  0; #
our $FM_L3AR_W16GH_PROFILE_TABLE_h_Value_ACTION_DATA_W16G  =  15; #
our $FM_L3AR_W16GH_PROFILE_TABLE_l_Value_ACTION_DATA_W16H  =  16; #
our $FM_L3AR_W16GH_PROFILE_TABLE_h_Value_ACTION_DATA_W16H  =  31; #
our $FM_L3AR_W16GH_PROFILE_TABLE_l_Select_ACTION_DATA_W16H  =  35; #
our $FM_L3AR_W16GH_PROFILE_TABLE_h_Select_ACTION_DATA_W16H  =  37; #
our $FM_L3AR_W16GH_PROFILE_TABLE_l_Select_ACTION_DATA_W16G  =  32; #
our $FM_L3AR_W16GH_PROFILE_TABLE_h_Select_ACTION_DATA_W16G  =  34; #

our $FM_L3AR_HASH_ROT_PROFILE_TABLE_l_Value_HASH_ROT_EXPONENT  =  18; #
our $FM_L3AR_HASH_ROT_PROFILE_TABLE_h_Value_HASH_ROT_EXPONENT  =  21; #
our $FM_L3AR_HASH_ROT_PROFILE_TABLE_l_Value_HASH_ROT_MANTISSA  =  0; #
our $FM_L3AR_HASH_ROT_PROFILE_TABLE_h_Value_HASH_ROT_MANTISSA  =  15; #
our $FM_L3AR_HASH_ROT_PROFILE_TABLE_l_Select_HASH_ROT_EXPONENT  =  22; #
our $FM_L3AR_HASH_ROT_PROFILE_TABLE_h_Select_HASH_ROT_EXPONENT  =  23; #
our $FM_L3AR_HASH_ROT_PROFILE_TABLE_l_Select_HASH_ROT_MANTISSA  =  16; #
our $FM_L3AR_HASH_ROT_PROFILE_TABLE_h_Select_HASH_ROT_MANTISSA  =  17; #

our $FM_L3AR_ALU13_OP_PROFILE_TABLE_l_Select_ALU1_X  =  96; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_h_Select_ALU1_X  =  99; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_l_Select_ALU2_Y  =  112; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_h_Select_ALU2_Y  =  115; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_l_Value_ALU2_Y  =  64; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_h_Value_ALU2_Y  =  79; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_l_Select_ALU2_X  =  100; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_h_Select_ALU2_X  =  103; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_l_Select_ALU1_Y  =  108; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_h_Select_ALU1_Y  =  111; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_l_Value_ALU1_Y  =  48; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_h_Value_ALU1_Y  =  63; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_l_Value_ALU1_X  =  0; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_h_Value_ALU1_X  =  15; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_l_Value_ALU2_X  =  16; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_h_Value_ALU2_X  =  31; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_l_Select_ALU3_Y  =  116; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_h_Select_ALU3_Y  =  119; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_l_Select_ALU3_X  =  104; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_h_Select_ALU3_X  =  107; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_l_Value_ALU3_X  =  32; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_h_Value_ALU3_X  =  47; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_l_Value_ALU3_Y  =  80; #
our $FM_L3AR_ALU13_OP_PROFILE_TABLE_h_Value_ALU3_Y  =  95; #

our $FM_L3AR_ALU46_OP_PROFILE_TABLE_l_Select_ALU5_X  =  100; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_h_Select_ALU5_X  =  103; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_l_Value_ALU4_X  =  0; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_h_Value_ALU4_X  =  15; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_l_Value_ALU4_Y  =  48; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_h_Value_ALU4_Y  =  63; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_l_Select_ALU6_X  =  104; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_h_Select_ALU6_X  =  107; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_l_Value_ALU6_Y  =  80; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_h_Value_ALU6_Y  =  95; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_l_Select_ALU6_Y  =  116; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_h_Select_ALU6_Y  =  119; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_l_Value_ALU5_Y  =  64; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_h_Value_ALU5_Y  =  79; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_l_Value_ALU5_X  =  16; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_h_Value_ALU5_X  =  31; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_l_Select_ALU5_Y  =  112; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_h_Select_ALU5_Y  =  115; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_l_Select_ALU4_Y  =  108; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_h_Select_ALU4_Y  =  111; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_l_Select_ALU4_X  =  96; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_h_Select_ALU4_X  =  99; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_l_Value_ALU6_X  =  32; #
our $FM_L3AR_ALU46_OP_PROFILE_TABLE_h_Value_ALU6_X  =  47; #

our $FM_L3AR_POL1_IDX_PROFILE_TABLE_l_Value_POL1_IDX  =  0; #
our $FM_L3AR_POL1_IDX_PROFILE_TABLE_h_Value_POL1_IDX  =  11; #
our $FM_L3AR_POL1_IDX_PROFILE_TABLE_l_Select_POL1_IDX  =  12; #
our $FM_L3AR_POL1_IDX_PROFILE_TABLE_h_Select_POL1_IDX  =  15; #

our $FM_L3AR_POL2_IDX_PROFILE_TABLE_l_Select_POL2_IDX  =  12; #
our $FM_L3AR_POL2_IDX_PROFILE_TABLE_h_Select_POL2_IDX  =  15; #
our $FM_L3AR_POL2_IDX_PROFILE_TABLE_l_Value_POL2_IDX  =  0; #
our $FM_L3AR_POL2_IDX_PROFILE_TABLE_h_Value_POL2_IDX  =  11; #

our $FM_L3AR_POL3_IDX_PROFILE_TABLE_l_Mask_POL3_IDX  =  10; #
our $FM_L3AR_POL3_IDX_PROFILE_TABLE_h_Mask_POL3_IDX  =  19; #
our $FM_L3AR_POL3_IDX_PROFILE_TABLE_l_Select_POL3_IDX  =  20; #
our $FM_L3AR_POL3_IDX_PROFILE_TABLE_h_Select_POL3_IDX  =  22; #
our $FM_L3AR_POL3_IDX_PROFILE_TABLE_l_Value_POL3_IDX  =  0; #
our $FM_L3AR_POL3_IDX_PROFILE_TABLE_h_Value_POL3_IDX  =  9; #

our $FM_L3AR_QOS_PROFILE_TABLE_l_Mask_QOS_ISL_PRI  =  20; #
our $FM_L3AR_QOS_PROFILE_TABLE_h_Mask_QOS_ISL_PRI  =  23; #
our $FM_L3AR_QOS_PROFILE_TABLE_l_Value_QOS_L2_VPRI1  =  4; #
our $FM_L3AR_QOS_PROFILE_TABLE_h_Value_QOS_L2_VPRI1  =  7; #
our $FM_L3AR_QOS_PROFILE_TABLE_l_Value_QOS_L3_PRI  =  12; #
our $FM_L3AR_QOS_PROFILE_TABLE_h_Value_QOS_L3_PRI  =  19; #
our $FM_L3AR_QOS_PROFILE_TABLE_l_Value_QOS_ISL_PRI  =  0; #
our $FM_L3AR_QOS_PROFILE_TABLE_h_Value_QOS_ISL_PRI  =  3; #
our $FM_L3AR_QOS_PROFILE_TABLE_l_Select_QOS_MAP2_IDX  =  58; #
our $FM_L3AR_QOS_PROFILE_TABLE_h_Select_QOS_MAP2_IDX  =  60; #
our $FM_L3AR_QOS_PROFILE_TABLE_l_Mask_QOS_L2_VPRI1  =  24; #
our $FM_L3AR_QOS_PROFILE_TABLE_h_Mask_QOS_L2_VPRI1  =  27; #
our $FM_L3AR_QOS_PROFILE_TABLE_l_Mask_QOS_L2_VPRI2  =  28; #
our $FM_L3AR_QOS_PROFILE_TABLE_h_Mask_QOS_L2_VPRI2  =  31; #
our $FM_L3AR_QOS_PROFILE_TABLE_l_Select_QOS_W4    =  52; #
our $FM_L3AR_QOS_PROFILE_TABLE_h_Select_QOS_W4    =  54; #
our $FM_L3AR_QOS_PROFILE_TABLE_l_Select_QOS_L3_PRI  =  49; #
our $FM_L3AR_QOS_PROFILE_TABLE_h_Select_QOS_L3_PRI  =  51; #
our $FM_L3AR_QOS_PROFILE_TABLE_l_Mask_QOS_L3_PRI  =  32; #
our $FM_L3AR_QOS_PROFILE_TABLE_h_Mask_QOS_L3_PRI  =  39; #
our $FM_L3AR_QOS_PROFILE_TABLE_l_Select_QOS_ISL_PRI  =  40; #
our $FM_L3AR_QOS_PROFILE_TABLE_h_Select_QOS_ISL_PRI  =  42; #
our $FM_L3AR_QOS_PROFILE_TABLE_l_Value_QOS_L2_VPRI2  =  8; #
our $FM_L3AR_QOS_PROFILE_TABLE_h_Value_QOS_L2_VPRI2  =  11; #
our $FM_L3AR_QOS_PROFILE_TABLE_l_Select_QOS_MAP1_IDX  =  55; #
our $FM_L3AR_QOS_PROFILE_TABLE_h_Select_QOS_MAP1_IDX  =  57; #
our $FM_L3AR_QOS_PROFILE_TABLE_l_Select_QOS_L2_VPRI2  =  46; #
our $FM_L3AR_QOS_PROFILE_TABLE_h_Select_QOS_L2_VPRI2  =  48; #
our $FM_L3AR_QOS_PROFILE_TABLE_l_Select_QOS_L2_VPRI1  =  43; #
our $FM_L3AR_QOS_PROFILE_TABLE_h_Select_QOS_L2_VPRI1  =  45; #

our $FM_L3AR_TRAP_HEADER_RULE_l_Rule              =  4; #
our $FM_L3AR_TRAP_HEADER_RULE_h_Rule              =  9; #
our $FM_L3AR_TRAP_HEADER_RULE_l_Slice             =  0; #
our $FM_L3AR_TRAP_HEADER_RULE_h_Slice             =  3; #

our $FM_L3AR_TRAP_HEADER_DATA_l_L3_PROT           =  288; #
our $FM_L3AR_TRAP_HEADER_DATA_h_L3_PROT           =  295; #
our $FM_L3AR_TRAP_HEADER_DATA_l_L4_SRC            =  256; #
our $FM_L3AR_TRAP_HEADER_DATA_h_L4_SRC            =  271; #
our $FM_L3AR_TRAP_HEADER_DATA_l_IVID1             =  320; #
our $FM_L3AR_TRAP_HEADER_DATA_h_IVID1             =  331; #
our $FM_L3AR_TRAP_HEADER_DATA_l_EVID1             =  332; #
our $FM_L3AR_TRAP_HEADER_DATA_h_EVID1             =  343; #
our $FM_L3AR_TRAP_HEADER_DATA_l_L3_DIP            =  128; #
our $FM_L3AR_TRAP_HEADER_DATA_h_L3_DIP            =  255; #
our $FM_L3AR_TRAP_HEADER_DATA_l_L4_DST            =  272; #
our $FM_L3AR_TRAP_HEADER_DATA_h_L4_DST            =  287; #
our $FM_L3AR_TRAP_HEADER_DATA_l_NEXTHOP_IDX       =  296; #
our $FM_L3AR_TRAP_HEADER_DATA_h_NEXTHOP_IDX       =  311; #
our $FM_L3AR_TRAP_HEADER_DATA_l_L3_SIP            =  0; #
our $FM_L3AR_TRAP_HEADER_DATA_h_L3_SIP            =  127; #

our $FM_L3AR_IP_l_Pending                         =  0; #
our $FM_L3AR_IP_h_Pending                         =  31; #

our $FM_L3AR_IM_l_Mask                            =  0; #
our $FM_L3AR_IM_h_Mask                            =  31; #

our $FM_LBS_CAM_l_Key                             =  16; #
our $FM_LBS_CAM_h_Key                             =  31; #
our $FM_LBS_CAM_l_KeyInvert                       =  0; #
our $FM_LBS_CAM_h_KeyInvert                       =  15; #

our $FM_LBS_PROFILE_TABLE_l_CmdB                  =  4; #
our $FM_LBS_PROFILE_TABLE_h_CmdB                  =  6; #
our $FM_LBS_PROFILE_TABLE_b_DropCodeSelect        =  7; #
our $FM_LBS_PROFILE_TABLE_l_CmdA                  =  0; #
our $FM_LBS_PROFILE_TABLE_h_CmdA                  =  3; #

our $FM_STATS_AR_IDX_CAM_l_Key                    =  64; #
our $FM_STATS_AR_IDX_CAM_h_Key                    =  127; #
our $FM_STATS_AR_IDX_CAM_l_KeyInvert              =  0; #
our $FM_STATS_AR_IDX_CAM_h_KeyInvert              =  63; #

our $FM_STATS_AR_IDX_RAM_l_IndexValue             =  0; #
our $FM_STATS_AR_IDX_RAM_h_IndexValue             =  4; #

our $FM_STATS_AR_FLAGS_CAM1_l_Key                 =  64; #
our $FM_STATS_AR_FLAGS_CAM1_h_Key                 =  127; #
our $FM_STATS_AR_FLAGS_CAM1_l_KeyInvert           =  0; #
our $FM_STATS_AR_FLAGS_CAM1_h_KeyInvert           =  63; #

our $FM_STATS_AR_FLAGS_CAM2_l_Key                 =  64; #
our $FM_STATS_AR_FLAGS_CAM2_h_Key                 =  127; #
our $FM_STATS_AR_FLAGS_CAM2_l_KeyInvert           =  0; #
our $FM_STATS_AR_FLAGS_CAM2_h_KeyInvert           =  63; #

our $FM_STATS_AR_FLAGS_CAM2_POLARITY_l_Polarity   =  0; #
our $FM_STATS_AR_FLAGS_CAM2_POLARITY_h_Polarity   =  89; #

our $FM_STATS_AR_FLAGS_CAM2_VALUE_l_Value         =  0; #
our $FM_STATS_AR_FLAGS_CAM2_VALUE_h_Value         =  89; #

our $FM_STATS_AR_RX_PORT_MAP_l_RxPortDelta        =  8; #
our $FM_STATS_AR_RX_PORT_MAP_h_RxPortDelta        =  12; #
our $FM_STATS_AR_RX_PORT_MAP_l_Tag                =  0; #
our $FM_STATS_AR_RX_PORT_MAP_h_Tag                =  7; #

our $FM_STATS_AR_TX_PORT_MAP_l_Tag                =  0; #
our $FM_STATS_AR_TX_PORT_MAP_h_Tag                =  7; #
our $FM_STATS_AR_TX_PORT_MAP_l_TxPortDelta        =  8; #
our $FM_STATS_AR_TX_PORT_MAP_h_TxPortDelta        =  12; #

our $FM_STATS_AR_RX_LENGTH_COMPARE_l_LowerBound   =  0; #
our $FM_STATS_AR_RX_LENGTH_COMPARE_h_LowerBound   =  13; #
our $FM_STATS_AR_RX_LENGTH_COMPARE_l_Bin          =  14; #
our $FM_STATS_AR_RX_LENGTH_COMPARE_h_Bin          =  17; #

our $FM_STATS_AR_TX_LENGTH_COMPARE_l_LowerBound   =  0; #
our $FM_STATS_AR_TX_LENGTH_COMPARE_h_LowerBound   =  13; #
our $FM_STATS_AR_TX_LENGTH_COMPARE_l_Bin          =  14; #
our $FM_STATS_AR_TX_LENGTH_COMPARE_h_Bin          =  17; #

our $FM_STATS_AR_BANK_CFG1_l_Select_PerChannel    =  4; #
our $FM_STATS_AR_BANK_CFG1_h_Select_PerChannel    =  7; #
our $FM_STATS_AR_BANK_CFG1_l_Select_CounterNum    =  0; #
our $FM_STATS_AR_BANK_CFG1_h_Select_CounterNum    =  3; #

our $FM_STATS_AR_BANK_CFG2_l_Amount               =  1; #
our $FM_STATS_AR_BANK_CFG2_h_Amount               =  2; #
our $FM_STATS_AR_BANK_CFG2_b_CounterType          =  0; #
our $FM_STATS_AR_BANK_CFG2_l_BankID               =  22; #
our $FM_STATS_AR_BANK_CFG2_h_BankID               =  26; #
our $FM_STATS_AR_BANK_CFG2_l_ColorCase            =  20; #
our $FM_STATS_AR_BANK_CFG2_h_ColorCase            =  21; #
our $FM_STATS_AR_BANK_CFG2_l_PerSpanMantissa      =  11; #
our $FM_STATS_AR_BANK_CFG2_h_PerSpanMantissa      =  15; #
our $FM_STATS_AR_BANK_CFG2_l_PerSpanExponent      =  16; #
our $FM_STATS_AR_BANK_CFG2_h_PerSpanExponent      =  19; #
our $FM_STATS_AR_BANK_CFG2_l_PerHighBit           =  7; #
our $FM_STATS_AR_BANK_CFG2_h_PerHighBit           =  10; #
our $FM_STATS_AR_BANK_CFG2_l_PerLowBit            =  3; #
our $FM_STATS_AR_BANK_CFG2_h_PerLowBit            =  6; #

our $FM_STATS_DISCRETE_COUNTER_FRAME_l_FrameCount  =  0; #
our $FM_STATS_DISCRETE_COUNTER_FRAME_h_FrameCount  =  63; #

our $FM_STATS_DISCRETE_COUNTER_BYTE_l_ByteCount   =  0; #
our $FM_STATS_DISCRETE_COUNTER_BYTE_h_ByteCount   =  63; #

our $FM_GLOBAL_INTERRUPT_DETECT_b_PCI             =  14; #
our $FM_GLOBAL_INTERRUPT_DETECT_b_BIST            =  8; #
our $FM_GLOBAL_INTERRUPT_DETECT_b_GPIO            =  4; #
our $FM_GLOBAL_INTERRUPT_DETECT_b_I2C             =  5; #
our $FM_GLOBAL_INTERRUPT_DETECT_b_SWEEPER         =  12; #
our $FM_GLOBAL_INTERRUPT_DETECT_b_L3AR            =  10; #
our $FM_GLOBAL_INTERRUPT_DETECT_b_SOFT            =  3; #
our $FM_GLOBAL_INTERRUPT_DETECT_b_CM              =  13; #
our $FM_GLOBAL_INTERRUPT_DETECT_b_MDIO            =  6; #
our $FM_GLOBAL_INTERRUPT_DETECT_b_SRAM_C_ERR      =  2; #
our $FM_GLOBAL_INTERRUPT_DETECT_b_LCI             =  9; #
our $FM_GLOBAL_INTERRUPT_DETECT_b_EPL             =  0; #
our $FM_GLOBAL_INTERRUPT_DETECT_b_CRM             =  7; #
our $FM_GLOBAL_INTERRUPT_DETECT_b_SRAM_U_ERR      =  1; #
our $FM_GLOBAL_INTERRUPT_DETECT_b_L2AR            =  11; #

our $FM_INTERRUPT_MASK_INT_b_MaskLCIInterrupt     =  9; #
our $FM_INTERRUPT_MASK_INT_b_MaskSOFTInterrupt    =  3; #
our $FM_INTERRUPT_MASK_INT_b_MaskL2ARInterrupt    =  11; #
our $FM_INTERRUPT_MASK_INT_b_MaskPCIInterrupt     =  14; #
our $FM_INTERRUPT_MASK_INT_b_MaskSRAM_U_ERRInterrupt  =  1; #
our $FM_INTERRUPT_MASK_INT_b_MaskGPIOInterrupt    =  4; #
our $FM_INTERRUPT_MASK_INT_b_MaskSWEEPERInterrupt  =  12; #
our $FM_INTERRUPT_MASK_INT_b_MaskMDIOInterrupt    =  6; #
our $FM_INTERRUPT_MASK_INT_b_MaskBISTInterrupt    =  8; #
our $FM_INTERRUPT_MASK_INT_b_MaskCMInterrupt      =  13; #
our $FM_INTERRUPT_MASK_INT_b_MaskL3ARInterrupt    =  10; #
our $FM_INTERRUPT_MASK_INT_b_MaskI2CInterrupt     =  5; #
our $FM_INTERRUPT_MASK_INT_b_MaskEPLInterrupt     =  0; #
our $FM_INTERRUPT_MASK_INT_b_MaskSRAM_C_ERRInterrupt  =  2; #
our $FM_INTERRUPT_MASK_INT_b_MaskCRMInterrupt     =  7; #

our $FM_INTERRUPT_MASK_PCIE_b_MaskLCIInterrupt    =  9; #
our $FM_INTERRUPT_MASK_PCIE_b_MaskSOFTInterrupt   =  3; #
our $FM_INTERRUPT_MASK_PCIE_b_MaskL2ARInterrupt   =  11; #
our $FM_INTERRUPT_MASK_PCIE_b_MaskPCIInterrupt    =  14; #
our $FM_INTERRUPT_MASK_PCIE_b_MaskSRAM_U_ERRInterrupt  =  1; #
our $FM_INTERRUPT_MASK_PCIE_b_MaskGPIOInterrupt   =  4; #
our $FM_INTERRUPT_MASK_PCIE_b_MaskSWEEPERInterrupt  =  12; #
our $FM_INTERRUPT_MASK_PCIE_b_MaskMDIOInterrupt   =  6; #
our $FM_INTERRUPT_MASK_PCIE_b_MaskBISTInterrupt   =  8; #
our $FM_INTERRUPT_MASK_PCIE_b_MaskCMInterrupt     =  13; #
our $FM_INTERRUPT_MASK_PCIE_b_MaskL3ARInterrupt   =  10; #
our $FM_INTERRUPT_MASK_PCIE_b_MaskI2CInterrupt    =  5; #
our $FM_INTERRUPT_MASK_PCIE_b_MaskEPLInterrupt    =  0; #
our $FM_INTERRUPT_MASK_PCIE_b_MaskSRAM_C_ERRInterrupt  =  2; #
our $FM_INTERRUPT_MASK_PCIE_b_MaskCRMInterrupt    =  7; #

our $FM_INTERRUPT_MASK_FIBM_b_MaskLCIInterrupt    =  9; #
our $FM_INTERRUPT_MASK_FIBM_b_MaskSOFTInterrupt   =  3; #
our $FM_INTERRUPT_MASK_FIBM_b_MaskL2ARInterrupt   =  11; #
our $FM_INTERRUPT_MASK_FIBM_b_MaskPCIInterrupt    =  14; #
our $FM_INTERRUPT_MASK_FIBM_b_MaskSRAM_U_ERRInterrupt  =  1; #
our $FM_INTERRUPT_MASK_FIBM_b_MaskGPIOInterrupt   =  4; #
our $FM_INTERRUPT_MASK_FIBM_b_MaskSWEEPERInterrupt  =  12; #
our $FM_INTERRUPT_MASK_FIBM_b_MaskMDIOInterrupt   =  6; #
our $FM_INTERRUPT_MASK_FIBM_b_MaskBISTInterrupt   =  8; #
our $FM_INTERRUPT_MASK_FIBM_b_MaskCMInterrupt     =  13; #
our $FM_INTERRUPT_MASK_FIBM_b_MaskL3ARInterrupt   =  10; #
our $FM_INTERRUPT_MASK_FIBM_b_MaskI2CInterrupt    =  5; #
our $FM_INTERRUPT_MASK_FIBM_b_MaskEPLInterrupt    =  0; #
our $FM_INTERRUPT_MASK_FIBM_b_MaskSRAM_C_ERRInterrupt  =  2; #
our $FM_INTERRUPT_MASK_FIBM_b_MaskCRMInterrupt    =  7; #

our $FM_GLOBAL_EPL_INT_DETECT_l_InterruptDetect   =  1; #
our $FM_GLOBAL_EPL_INT_DETECT_h_InterruptDetect   =  24; #

our $FM_SRAM_CORRECTED_IP_l_Interrupt             =  0; #
our $FM_SRAM_CORRECTED_IP_h_Interrupt             =  127; #

our $FM_SRAM_CORRECTED_IM_l_Mask                  =  0; #
our $FM_SRAM_CORRECTED_IM_h_Mask                  =  127; #

our $FM_SRAM_UNCORRECTABLE_IP_l_Interrupt         =  0; #
our $FM_SRAM_UNCORRECTABLE_IP_h_Interrupt         =  127; #

our $FM_SRAM_UNCORRECTABLE_IM_l_Mask              =  0; #
our $FM_SRAM_UNCORRECTABLE_IM_h_Mask              =  127; #

our $FM_SRAM_UNCORRECTABLE_FATAL_l_Mask           =  0; #
our $FM_SRAM_UNCORRECTABLE_FATAL_h_Mask           =  127; #

our $FM_SW_IP_l_SoftIP                            =  0; #
our $FM_SW_IP_h_SoftIP                            =  31; #

our $FM_SW_IM_l_SoftIM                            =  0; #
our $FM_SW_IM_h_SoftIM                            =  31; #

our $FM_SW_TEST_AND_SET_l_Data                    =  0; #
our $FM_SW_TEST_AND_SET_h_Data                    =  15; #
our $FM_SW_TEST_AND_SET_l_Mask                    =  16; #
our $FM_SW_TEST_AND_SET_h_Mask                    =  31; #

our $FM_FRAME_TIME_OUT_l_timeOutMult              =  0; #
our $FM_FRAME_TIME_OUT_h_timeOutMult              =  27; #

our $FM_CHIP_VERSION_l_Version                    =  0; #
our $FM_CHIP_VERSION_h_Version                    =  6; #

our $FM_PIN_STRAP_STAT_b_dtackInv                 =  0; #
our $FM_PIN_STRAP_STAT_b_bootMode0                =  7; #
our $FM_PIN_STRAP_STAT_b_rwInv                    =  1; #
our $FM_PIN_STRAP_STAT_b_dataHold                 =  14; #
our $FM_PIN_STRAP_STAT_b_testClkTerm              =  6; #
our $FM_PIN_STRAP_STAT_b_bootMode2                =  9; #
our $FM_PIN_STRAP_STAT_b_bootMode1                =  8; #
our $FM_PIN_STRAP_STAT_b_pcieClkTerm              =  5; #
our $FM_PIN_STRAP_STAT_b_ethClkTerm               =  4; #
our $FM_PIN_STRAP_STAT_b_i2cAddr1                 =  12; #
our $FM_PIN_STRAP_STAT_b_i2cAddr2                 =  13; #
our $FM_PIN_STRAP_STAT_b_csn                      =  3; #
our $FM_PIN_STRAP_STAT_b_i2cAddr0                 =  11; #
our $FM_PIN_STRAP_STAT_b_ignPar                   =  2; #
our $FM_PIN_STRAP_STAT_b_parityEven               =  10; #

our $FM_BOOT_CTRL_b_CommandDone                   =  4; #
our $FM_BOOT_CTRL_l_EepromAddr                    =  8; #
our $FM_BOOT_CTRL_h_EepromAddr                    =  31; #
our $FM_BOOT_CTRL_b_EepromLoadDone                =  5; #
our $FM_BOOT_CTRL_l_Command                       =  0; #
our $FM_BOOT_CTRL_h_Command                       =  3; #
our $FM_BOOT_CTRL_b_EepromEnable                  =  7; #
our $FM_BOOT_CTRL_b_EepromError                   =  6; #

our $FM_BOOT_ARGS_l_Data                          =  0; #
our $FM_BOOT_ARGS_h_Data                          =  31; #

our $FM_GPIO_CFG_l_Dir                            =  0; #
our $FM_GPIO_CFG_h_Dir                            =  15; #
our $FM_GPIO_CFG_l_OpenDrain                      =  16; #
our $FM_GPIO_CFG_h_OpenDrain                      =  31; #

our $FM_GPIO_DATA_l_data                          =  0; #
our $FM_GPIO_DATA_h_data                          =  15; #

our $FM_GPIO_IP_l_detectLow                       =  16; #
our $FM_GPIO_IP_h_detectLow                       =  31; #
our $FM_GPIO_IP_l_detectHigh                      =  0; #
our $FM_GPIO_IP_h_detectHigh                      =  15; #

our $FM_GPIO_IM_l_detectLowMask                   =  16; #
our $FM_GPIO_IM_h_detectLowMask                   =  31; #
our $FM_GPIO_IM_l_detectHighMask                  =  0; #
our $FM_GPIO_IM_h_detectHighMask                  =  15; #

our $FM_I2C_CFG_b_InterruptMask                   =  20; #
our $FM_I2C_CFG_b_Enable                          =  0; #
our $FM_I2C_CFG_l_Divider                         =  8; #
our $FM_I2C_CFG_h_Divider                         =  19; #
our $FM_I2C_CFG_l_DebounceFilterCountLimit        =  21; #
our $FM_I2C_CFG_h_DebounceFilterCountLimit        =  27; #
our $FM_I2C_CFG_l_Addr                            =  1; #
our $FM_I2C_CFG_h_Addr                            =  7; #

our $FM_I2C_DATA_l_Data                           =  0; #
our $FM_I2C_DATA_h_Data                           =  31; #

our $FM_I2C_CTRL_l_LengthSent                     =  18; #
our $FM_I2C_CTRL_h_LengthSent                     =  21; #
our $FM_I2C_CTRL_l_LengthW                        =  10; #
our $FM_I2C_CTRL_h_LengthW                        =  13; #
our $FM_I2C_CTRL_l_CommandCompleted               =  22; #
our $FM_I2C_CTRL_h_CommandCompleted               =  25; #
our $FM_I2C_CTRL_l_LengthR                        =  14; #
our $FM_I2C_CTRL_h_LengthR                        =  17; #
our $FM_I2C_CTRL_l_Command                        =  8; #
our $FM_I2C_CTRL_h_Command                        =  9; #
our $FM_I2C_CTRL_b_InterruptPending               =  26; #
our $FM_I2C_CTRL_l_Addr                           =  0; #
our $FM_I2C_CTRL_h_Addr                           =  7; #

our $FM_MDIO_CFG_b_InterruptMask                  =  13; #
our $FM_MDIO_CFG_b_Preamble                       =  12; #
our $FM_MDIO_CFG_l_Divider                        =  0; #
our $FM_MDIO_CFG_h_Divider                        =  11; #

our $FM_MDIO_DATA_l_Data                          =  0; #
our $FM_MDIO_DATA_h_Data                          =  15; #

our $FM_MDIO_CTRL_l_Register                      =  0; #
our $FM_MDIO_CTRL_h_Register                      =  15; #
our $FM_MDIO_CTRL_l_Status                        =  29; #
our $FM_MDIO_CTRL_h_Status                        =  30; #
our $FM_MDIO_CTRL_l_PhyAddress                    =  21; #
our $FM_MDIO_CTRL_h_PhyAddress                    =  25; #
our $FM_MDIO_CTRL_l_Command                       =  26; #
our $FM_MDIO_CTRL_h_Command                       =  27; #
our $FM_MDIO_CTRL_l_Device                        =  16; #
our $FM_MDIO_CTRL_h_Device                        =  20; #
our $FM_MDIO_CTRL_b_InterruptPending              =  31; #
our $FM_MDIO_CTRL_b_DeviceType                    =  28; #

our $FM_SPI_TX_DATA_l_Data                        =  0; #
our $FM_SPI_TX_DATA_h_Data                        =  31; #

our $FM_SPI_RX_DATA_l_Data                        =  0; #
our $FM_SPI_RX_DATA_h_Data                        =  31; #

our $FM_SPI_HEADER_l_Data                         =  0; #
our $FM_SPI_HEADER_h_Data                         =  31; #

our $FM_SPI_CTRL_l_DataSize                       =  17; #
our $FM_SPI_CTRL_h_DataSize                       =  18; #
our $FM_SPI_CTRL_b_Selected                       =  22; #
our $FM_SPI_CTRL_l_Freq                           =  0; #
our $FM_SPI_CTRL_h_Freq                           =  9; #
our $FM_SPI_CTRL_l_Command                        =  11; #
our $FM_SPI_CTRL_h_Command                        =  14; #
our $FM_SPI_CTRL_b_Enable                         =  10; #
our $FM_SPI_CTRL_b_Busy                           =  21; #
our $FM_SPI_CTRL_l_HeaderSize                     =  15; #
our $FM_SPI_CTRL_h_HeaderSize                     =  16; #
our $FM_SPI_CTRL_l_DataShiftMethod                =  19; #
our $FM_SPI_CTRL_h_DataShiftMethod                =  20; #

our $FM_LED_CFG_l_LEDFreq                         =  0; #
our $FM_LED_CFG_h_LEDFreq                         =  23; #
our $FM_LED_CFG_b_LEDEnable                       =  24; #

our $FM_SCAN_CONTROL_b_PermitScanOperations       =  4; #
our $FM_SCAN_CONTROL_l_GroupEnable                =  0; #
our $FM_SCAN_CONTROL_h_GroupEnable                =  3; #

our $FM_SCAN_CONFIG_DATA_IN_l_Address             =  24; #
our $FM_SCAN_CONFIG_DATA_IN_h_Address             =  29; #
our $FM_SCAN_CONFIG_DATA_IN_l_DataIn              =  0; #
our $FM_SCAN_CONFIG_DATA_IN_h_DataIn              =  23; #
our $FM_SCAN_CONFIG_DATA_IN_b_IsWrite             =  31; #
our $FM_SCAN_CONFIG_DATA_IN_b_reserved            =  30; #

our $FM_SCAN_CHAIN_DATA_IN_l_DataIn               =  0; #
our $FM_SCAN_CHAIN_DATA_IN_h_DataIn               =  31; #

our $FM_SCAN_DATA_OUT_l_DataOut                   =  0; #
our $FM_SCAN_DATA_OUT_h_DataOut                   =  31; #

our $FM_SCAN_STATUS_b_CountsAreEqual              =  8; #
our $FM_SCAN_STATUS_b_RequestDropped              =  9; #
our $FM_SCAN_STATUS_l_ReqCount                    =  0; #
our $FM_SCAN_STATUS_h_ReqCount                    =  3; #
our $FM_SCAN_STATUS_l_RespCount                   =  4; #
our $FM_SCAN_STATUS_h_RespCount                   =  7; #

our $FM_ETHCLK_CFG_b_EnableDivider                =  16; #
our $FM_ETHCLK_CFG_l_Divider                      =  0; #
our $FM_ETHCLK_CFG_h_Divider                      =  15; #
our $FM_ETHCLK_CFG_b_EnableClockOut               =  17; #

our $FM_ETHCLK_RATIO_l_N                          =  12; #
our $FM_ETHCLK_RATIO_h_N                          =  23; #
our $FM_ETHCLK_RATIO_l_M                          =  0; #
our $FM_ETHCLK_RATIO_h_M                          =  11; #

our $FM_PLL_CTRL_l_Divider1                       =  10; #
our $FM_PLL_CTRL_h_Divider1                       =  17; #
our $FM_PLL_CTRL_l_PLL_CLKOUT_Select              =  47; #
our $FM_PLL_CTRL_h_PLL_CLKOUT_Select              =  48; #
our $FM_PLL_CTRL_l_Divider2                       =  29; #
our $FM_PLL_CTRL_h_Divider2                       =  36; #
our $FM_PLL_CTRL_l_EPL_ClkSelect                  =  42; #
our $FM_PLL_CTRL_h_EPL_ClkSelect                  =  43; #
our $FM_PLL_CTRL_b_PCIE_ClkSelect                 =  46; #
our $FM_PLL_CTRL_l_SBUS_ClkSelect                 =  44; #
our $FM_PLL_CTRL_h_SBUS_ClkSelect                 =  45; #
our $FM_PLL_CTRL_l_MSB_ClkSelect                  =  38; #
our $FM_PLL_CTRL_h_MSB_ClkSelect                  =  39; #
our $FM_PLL_CTRL_l_FIBM_ClkSelect                 =  40; #
our $FM_PLL_CTRL_h_FIBM_ClkSelect                 =  41; #
our $FM_PLL_CTRL_l_Multiplier2                    =  19; #
our $FM_PLL_CTRL_h_Multiplier2                    =  28; #
our $FM_PLL_CTRL_b_Enable1                        =  18; #
our $FM_PLL_CTRL_b_Enable2                        =  37; #
our $FM_PLL_CTRL_l_Multiplier1                    =  0; #
our $FM_PLL_CTRL_h_Multiplier1                    =  9; #

our $FM_DLL_CTRL_l_RefDiv1                        =  0; #
our $FM_DLL_CTRL_h_RefDiv1                        =  7; #
our $FM_DLL_CTRL_b_Enable1                        =  32; #
our $FM_DLL_CTRL_b_Enable2                        =  33; #
our $FM_DLL_CTRL_l_FbDiv2                         =  24; #
our $FM_DLL_CTRL_h_FbDiv2                         =  31; #
our $FM_DLL_CTRL_l_RefDiv2                        =  16; #
our $FM_DLL_CTRL_h_RefDiv2                        =  23; #
our $FM_DLL_CTRL_l_FbDiv1                         =  8; #
our $FM_DLL_CTRL_h_FbDiv1                         =  15; #

our $FM_PLL_STAT_b_Locked2                        =  1; #
our $FM_PLL_STAT_b_DllLocked2                     =  3; #
our $FM_PLL_STAT_b_Locked1                        =  0; #
our $FM_PLL_STAT_b_DllLocked1                     =  2; #

our $FM_SWEEPER_CFG_l_CmMonitorTickPeriod         =  136; #
our $FM_SWEEPER_CFG_h_CmMonitorTickPeriod         =  143; #
our $FM_SWEEPER_CFG_l_L2LookupWritebackPeriod     =  64; #
our $FM_SWEEPER_CFG_h_L2LookupWritebackPeriod     =  95; #
our $FM_SWEEPER_CFG_l_L2LookupPeriod0             =  0; #
our $FM_SWEEPER_CFG_h_L2LookupPeriod0             =  31; #
our $FM_SWEEPER_CFG_l_PolicerPeriod               =  96; #
our $FM_SWEEPER_CFG_h_PolicerPeriod               =  111; #
our $FM_SWEEPER_CFG_l_SchedPeriod                 =  128; #
our $FM_SWEEPER_CFG_h_SchedPeriod                 =  135; #
our $FM_SWEEPER_CFG_l_PausePeriod                 =  112; #
our $FM_SWEEPER_CFG_h_PausePeriod                 =  127; #
our $FM_SWEEPER_CFG_l_L2LookupPeriod1             =  32; #
our $FM_SWEEPER_CFG_h_L2LookupPeriod1             =  63; #

our $FM_S2A_EN_STATUS_b_SchedTickEn               =  6; #
our $FM_S2A_EN_STATUS_b_CmMonitorTickEn           =  7; #
our $FM_S2A_EN_STATUS_b_L2LTick0En                =  1; #
our $FM_S2A_EN_STATUS_b_L2LWbTickEn               =  3; #
our $FM_S2A_EN_STATUS_b_RoCfg1En                  =  9; #
our $FM_S2A_EN_STATUS_b_TimeoutTickEn             =  0; #
our $FM_S2A_EN_STATUS_b_PolicerTickEn             =  4; #
our $FM_S2A_EN_STATUS_b_RoCfg0En                  =  8; #
our $FM_S2A_EN_STATUS_b_PauseTickEn               =  5; #
our $FM_S2A_EN_STATUS_b_L2LTick1En                =  2; #

our $FM_RO_CFG_l_Value                            =  0; #
our $FM_RO_CFG_h_Value                            =  1; #

our $FM_TESTCTRL_MOD_CFG_b_Module                 =  0; #

our $FM_TESTCTRL_TICK_CFG_l_Ticks                 =  0; #
our $FM_TESTCTRL_TICK_CFG_h_Ticks                 =  15; #

our $FM_TESTCTRL_TICK_CNT_l_Count                 =  0; #
our $FM_TESTCTRL_TICK_CNT_h_Count                 =  15; #

our $FM_TESTCTRL_CLK_CNT_l_Count                  =  0; #
our $FM_TESTCTRL_CLK_CNT_h_Count                  =  31; #

our $FM_TESTCTRL_START_b_Start                    =  0; #

our $FM_FUSEBOX_l_Data                            =  0; #
our $FM_FUSEBOX_h_Data                            =  7; #

our $FM_BM_MARCH_SEQUENCE_l_MarchOperations       =  0; #
our $FM_BM_MARCH_SEQUENCE_h_MarchOperations       =  127; #

our $FM_BM_GENERAL_CONFIG_b_FuseboxWriteEnable    =  6; #
our $FM_BM_GENERAL_CONFIG_b_GPIOReturnToZero      =  4; #
our $FM_BM_GENERAL_CONFIG_b_PadTXQFreelist        =  7; #
our $FM_BM_GENERAL_CONFIG_l_GPIOExtraHoldCycles   =  0; #
our $FM_BM_GENERAL_CONFIG_h_GPIOExtraHoldCycles   =  3; #
our $FM_BM_GENERAL_CONFIG_b_GPIODefectReportingEnable  =  5; #

our $FM_BM_TXQ_HS_SEGMENTS_l_Segments             =  0; #
our $FM_BM_TXQ_HS_SEGMENTS_h_Segments             =  14; #

our $FM_BM_RXQ_PAGES_l_Pages                      =  0; #
our $FM_BM_RXQ_PAGES_h_Pages                      =  10; #

our $FM_BM_MODEL_INFO_l_Model                     =  16; #
our $FM_BM_MODEL_INFO_h_Model                     =  23; #
our $FM_BM_MODEL_INFO_l_Segments                  =  0; #
our $FM_BM_MODEL_INFO_h_Segments                  =  15; #

our $FM_BM_FFU_SLICE_MASK_l_SliceMask             =  0; #
our $FM_BM_FFU_SLICE_MASK_h_SliceMask             =  23; #

our $FM_BM_VRM_l_Value                            =  0; #
our $FM_BM_VRM_h_Value                            =  7; #

our $FM_BM_MAX_ALLOWED_REPAIRS_l_MaxRepairs       =  0; #
our $FM_BM_MAX_ALLOWED_REPAIRS_h_MaxRepairs       =  5; #

our $FM_BM_FUSEBOX_SUMMARY_b_FirstEmptyIndexValid  =  16; #
our $FM_BM_FUSEBOX_SUMMARY_b_SummaryValid         =  17; #
our $FM_BM_FUSEBOX_SUMMARY_l_FirstEmptyIndex      =  8; #
our $FM_BM_FUSEBOX_SUMMARY_h_FirstEmptyIndex      =  12; #
our $FM_BM_FUSEBOX_SUMMARY_l_RepairCount          =  0; #
our $FM_BM_FUSEBOX_SUMMARY_h_RepairCount          =  5; #
our $FM_BM_FUSEBOX_SUMMARY_l_reserved2            =  13; #
our $FM_BM_FUSEBOX_SUMMARY_h_reserved2            =  15; #
our $FM_BM_FUSEBOX_SUMMARY_l_reserved1            =  6; #
our $FM_BM_FUSEBOX_SUMMARY_h_reserved1            =  7; #

our $FM_BM_IP_b_HStorEngineError                  =  2; #
our $FM_BM_IP_b_SPDPEngine2Error                  =  7; #
our $FM_BM_IP_b_SubMasterError                    =  10; #
our $FM_BM_IP_b_SPDPEngine4Error                  =  9; #
our $FM_BM_IP_b_TenTEngineError                   =  0; #
our $FM_BM_IP_b_InvalidSegmentCount               =  16; #
our $FM_BM_IP_b_TXQEngineError                    =  3; #
our $FM_BM_IP_b_SequencerBusy                     =  18; #
our $FM_BM_IP_b_FuseboxParityError                =  11; #
our $FM_BM_IP_b_NotEnoughEmptyFBEntries           =  13; #
our $FM_BM_IP_b_ArrayEngineError                  =  1; #
our $FM_BM_IP_b_SoftRepairLimitExceeded           =  14; #
our $FM_BM_IP_b_OperationRequestedWhileBusy       =  19; #
our $FM_BM_IP_b_SPDPEngine3Error                  =  8; #
our $FM_BM_IP_b_InsufficientSegments              =  17; #
our $FM_BM_IP_b_SPDPEngine0Error                  =  5; #
our $FM_BM_IP_b_CannotReadRepairFromEngine        =  15; #
our $FM_BM_IP_b_RXQEngineError                    =  4; #
our $FM_BM_IP_b_FuseboxSummaryInvalid             =  12; #
our $FM_BM_IP_b_SPDPEngine1Error                  =  6; #

our $FM_BM_IM_l_MASK                              =  0; #
our $FM_BM_IM_h_MASK                              =  19; #

our $FM_BM_ENGINE_STATUS_b_SPDPEngine3Busy        =  8; #
our $FM_BM_ENGINE_STATUS_b_ArrayEngineBusy        =  1; #
our $FM_BM_ENGINE_STATUS_b_SPDPEngine0Busy        =  5; #
our $FM_BM_ENGINE_STATUS_b_TenTEngineBusy         =  0; #
our $FM_BM_ENGINE_STATUS_b_SPDPEngine4Busy        =  9; #
our $FM_BM_ENGINE_STATUS_b_TXQEngineBusy          =  3; #
our $FM_BM_ENGINE_STATUS_b_HStorEngineBusy        =  2; #
our $FM_BM_ENGINE_STATUS_b_RXQEngineBusy          =  4; #
our $FM_BM_ENGINE_STATUS_b_SPDPEngine2Busy        =  7; #
our $FM_BM_ENGINE_STATUS_b_SPDPEngine1Busy        =  6; #
our $FM_BM_ENGINE_STATUS_b_BISTMasterBusy         =  10; #

our $FM_BM_FUSEBOX_BURN_TIME_l_BurnClocks         =  0; #
our $FM_BM_FUSEBOX_BURN_TIME_h_BurnClocks         =  9; #

our $FM_BM_FUSEBOX_APPEND_b_Parity                =  27; #
our $FM_BM_FUSEBOX_APPEND_l_Type                  =  28; #
our $FM_BM_FUSEBOX_APPEND_h_Type                  =  31; #
our $FM_BM_FUSEBOX_APPEND_l_Info                  =  0; #
our $FM_BM_FUSEBOX_APPEND_h_Info                  =  26; #

our $FM_BM_START_OPERATION_l_Operation            =  0; #
our $FM_BM_START_OPERATION_h_Operation            =  2; #

our $FM_BM_DEBUG_STATUS_1_l_AuxState3             =  18; #
our $FM_BM_DEBUG_STATUS_1_h_AuxState3             =  23; #
our $FM_BM_DEBUG_STATUS_1_l_AuxState1             =  6; #
our $FM_BM_DEBUG_STATUS_1_h_AuxState1             =  11; #
our $FM_BM_DEBUG_STATUS_1_l_AuxState2             =  12; #
our $FM_BM_DEBUG_STATUS_1_h_AuxState2             =  17; #
our $FM_BM_DEBUG_STATUS_1_l_State                 =  0; #
our $FM_BM_DEBUG_STATUS_1_h_State                 =  5; #

our $FM_BM_DEBUG_STATUS_2_l_RepairPool            =  7; #
our $FM_BM_DEBUG_STATUS_2_h_RepairPool            =  9; #
our $FM_BM_DEBUG_STATUS_2_b_FboxLoad              =  10; #
our $FM_BM_DEBUG_STATUS_2_l_BootCmdType           =  4; #
our $FM_BM_DEBUG_STATUS_2_h_BootCmdType           =  6; #
our $FM_BM_DEBUG_STATUS_2_l_WordIndex             =  11; #
our $FM_BM_DEBUG_STATUS_2_h_WordIndex             =  15; #
our $FM_BM_DEBUG_STATUS_2_l_SRState               =  0; #
our $FM_BM_DEBUG_STATUS_2_h_SRState               =  3; #

our $FM_TEN_T_BIST_MARCH_CONFIG_b_DelayedWrites   =  15; #
our $FM_TEN_T_BIST_MARCH_CONFIG_b_Enabled         =  18; #
our $FM_TEN_T_BIST_MARCH_CONFIG_l_ValueType       =  12; #
our $FM_TEN_T_BIST_MARCH_CONFIG_h_ValueType       =  13; #
our $FM_TEN_T_BIST_MARCH_CONFIG_l_FirstMarchSeqIndex  =  2; #
our $FM_TEN_T_BIST_MARCH_CONFIG_h_FirstMarchSeqIndex  =  6; #
our $FM_TEN_T_BIST_MARCH_CONFIG_b_UseBypass       =  17; #
our $FM_TEN_T_BIST_MARCH_CONFIG_b_SelectBW        =  14; #
our $FM_TEN_T_BIST_MARCH_CONFIG_l_LastMarchSeqIndex  =  7; #
our $FM_TEN_T_BIST_MARCH_CONFIG_h_LastMarchSeqIndex  =  11; #
our $FM_TEN_T_BIST_MARCH_CONFIG_l_Value           =  0; #
our $FM_TEN_T_BIST_MARCH_CONFIG_h_Value           =  1; #
our $FM_TEN_T_BIST_MARCH_CONFIG_b_DoPairing       =  16; #

our $FM_TEN_T_BIST_GENERAL_CONFIG_b_StopOnDefect  =  0; #

our $FM_TEN_T_BIST_STATUS_b_FoundDefect           =  1; #
our $FM_TEN_T_BIST_STATUS_b_LastOpHadDefect       =  0; #

our $FM_TEN_T_BIST_USER_CHECKER_MASK_l_CheckerMask  =  0; #
our $FM_TEN_T_BIST_USER_CHECKER_MASK_h_CheckerMask  =  15; #

our $FM_TEN_T_BIST_IP_b_FoundDefect               =  2; #
our $FM_TEN_T_BIST_IP_b_CDCTimeout                =  0; #
our $FM_TEN_T_BIST_IP_b_StoppedOnDefect           =  3; #
our $FM_TEN_T_BIST_IP_b_OperationRequestedWhileBusy  =  1; #

our $FM_TEN_T_BIST_IM_l_MASK                      =  0; #
our $FM_TEN_T_BIST_IM_h_MASK                      =  3; #

our $FM_TEN_T_BIST_USER_OP_l_Operation            =  16; #
our $FM_TEN_T_BIST_USER_OP_h_Operation            =  17; #
our $FM_TEN_T_BIST_USER_OP_b_DoPairedRead         =  20; #
our $FM_TEN_T_BIST_USER_OP_b_ClearPairingInfo     =  23; #
our $FM_TEN_T_BIST_USER_OP_l_Address              =  0; #
our $FM_TEN_T_BIST_USER_OP_h_Address              =  15; #
our $FM_TEN_T_BIST_USER_OP_b_UpdatePairedRead     =  21; #
our $FM_TEN_T_BIST_USER_OP_b_UpdateUseBypass      =  22; #
our $FM_TEN_T_BIST_USER_OP_l_DataValue            =  18; #
our $FM_TEN_T_BIST_USER_OP_h_DataValue            =  19; #

our $FM_TEN_T_BIST_PAIRED_READ_b_Valid            =  18; #
our $FM_TEN_T_BIST_PAIRED_READ_l_Address          =  0; #
our $FM_TEN_T_BIST_PAIRED_READ_h_Address          =  15; #
our $FM_TEN_T_BIST_PAIRED_READ_l_DataValue        =  16; #
our $FM_TEN_T_BIST_PAIRED_READ_h_DataValue        =  17; #

our $FM_TEN_T_BIST_CURRENT_ADDR_l_Address         =  0; #
our $FM_TEN_T_BIST_CURRENT_ADDR_h_Address         =  15; #

our $FM_TEN_T_BIST_START_SEQUENCE_b_SequenceType  =  0; #

our $FM_TEN_T_BIST_SAVED_ADDRESS_l_Address        =  0; #
our $FM_TEN_T_BIST_SAVED_ADDRESS_h_Address        =  15; #

our $FM_TEN_T_BIST_DEBUG_STATUS_1_l_BISTIndex     =  6; #
our $FM_TEN_T_BIST_DEBUG_STATUS_1_h_BISTIndex     =  10; #
our $FM_TEN_T_BIST_DEBUG_STATUS_1_l_BISTIndexME   =  11; #
our $FM_TEN_T_BIST_DEBUG_STATUS_1_h_BISTIndexME   =  15; #
our $FM_TEN_T_BIST_DEBUG_STATUS_1_l_State         =  0; #
our $FM_TEN_T_BIST_DEBUG_STATUS_1_h_State         =  5; #
our $FM_TEN_T_BIST_DEBUG_STATUS_1_l_ChainWait     =  16; #
our $FM_TEN_T_BIST_DEBUG_STATUS_1_h_ChainWait     =  17; #

our $FM_TEN_T_BIST_DEBUG_STATUS_2_l_Controller    =  0; #
our $FM_TEN_T_BIST_DEBUG_STATUS_2_h_Controller    =  15; #

our $FM_TEN_T_BIST_CHAIN_LATENCY_l_InjectDrainSpacing  =  0; #
our $FM_TEN_T_BIST_CHAIN_LATENCY_h_InjectDrainSpacing  =  4; #

our $FM_TEN_T_BIST_CHAIN_CDC_b_Valid              =  8; #
our $FM_TEN_T_BIST_CHAIN_CDC_b_Inject             =  10; #
our $FM_TEN_T_BIST_CHAIN_CDC_b_Enable             =  9; #
our $FM_TEN_T_BIST_CHAIN_CDC_l_Data               =  0; #
our $FM_TEN_T_BIST_CHAIN_CDC_h_Data               =  7; #
our $FM_TEN_T_BIST_CHAIN_CDC_b_Drain              =  11; #

our $FM_CDP_BIST_REPAIR_b_RepairValid             =  27; #
our $FM_CDP_BIST_REPAIR_l_CompressedMask          =  16; #
our $FM_CDP_BIST_REPAIR_h_CompressedMask          =  26; #
our $FM_CDP_BIST_REPAIR_l_RepairAddress           =  0; #
our $FM_CDP_BIST_REPAIR_h_RepairAddress           =  15; #

our $FM_CDP_BIST_MARCH_CONFIG_b_DelayedWrites     =  20; #
our $FM_CDP_BIST_MARCH_CONFIG_b_Enabled           =  21; #
our $FM_CDP_BIST_MARCH_CONFIG_l_ValueType         =  18; #
our $FM_CDP_BIST_MARCH_CONFIG_h_ValueType         =  19; #
our $FM_CDP_BIST_MARCH_CONFIG_l_FirstMarchSeqIndex  =  8; #
our $FM_CDP_BIST_MARCH_CONFIG_h_FirstMarchSeqIndex  =  12; #
our $FM_CDP_BIST_MARCH_CONFIG_l_LastMarchSeqIndex  =  13; #
our $FM_CDP_BIST_MARCH_CONFIG_h_LastMarchSeqIndex  =  17; #
our $FM_CDP_BIST_MARCH_CONFIG_l_Value             =  0; #
our $FM_CDP_BIST_MARCH_CONFIG_h_Value             =  7; #

our $FM_CDP_BIST_DEFECT_MAP_l_DefectRow           =  0; #
our $FM_CDP_BIST_DEFECT_MAP_h_DefectRow           =  31; #

our $FM_CDP_BIST_GENERAL_CONFIG_b_PruneDefectReporting  =  2; #
our $FM_CDP_BIST_GENERAL_CONFIG_b_SkipReadBadAddr  =  4; #
our $FM_CDP_BIST_GENERAL_CONFIG_b_ResetAfterRead  =  0; #
our $FM_CDP_BIST_GENERAL_CONFIG_b_FlushAfterReset  =  8; #
our $FM_CDP_BIST_GENERAL_CONFIG_b_DoRepairAnalysis  =  1; #
our $FM_CDP_BIST_GENERAL_CONFIG_b_StopOnDefect    =  6; #
our $FM_CDP_BIST_GENERAL_CONFIG_b_FlushDelayedWriteBeforeRead  =  7; #
our $FM_CDP_BIST_GENERAL_CONFIG_b_ChunkwiseTesting  =  3; #
our $FM_CDP_BIST_GENERAL_CONFIG_b_SkipWriteBadAddr  =  5; #

our $FM_CDP_BIST_STATUS_b_HaveEnoughSegments      =  8; #
our $FM_CDP_BIST_STATUS_b_FoundDefect             =  11; #
our $FM_CDP_BIST_STATUS_b_LastOpHadDefect         =  10; #
our $FM_CDP_BIST_STATUS_l_FirstEmptyRepair        =  4; #
our $FM_CDP_BIST_STATUS_h_FirstEmptyRepair        =  7; #
our $FM_CDP_BIST_STATUS_l_FirstNewRepair          =  0; #
our $FM_CDP_BIST_STATUS_h_FirstNewRepair          =  3; #
our $FM_CDP_BIST_STATUS_b_SegmentCountValid       =  9; #

our $FM_CDP_BIST_USER_CHECKER_MASKS_l_HighHalfChunkMask  =  7; #
our $FM_CDP_BIST_USER_CHECKER_MASKS_h_HighHalfChunkMask  =  13; #
our $FM_CDP_BIST_USER_CHECKER_MASKS_l_LowHalfChunkMask  =  0; #
our $FM_CDP_BIST_USER_CHECKER_MASKS_h_LowHalfChunkMask  =  6; #

our $FM_CDP_BIST_IP_b_CDCTimeout                  =  1; #
our $FM_CDP_BIST_IP_b_StoppedOnDefect             =  3; #
our $FM_CDP_BIST_IP_b_RepairOverflow              =  0; #
our $FM_CDP_BIST_IP_b_OperationRequestedWhileBusy  =  2; #

our $FM_CDP_BIST_IM_l_MASK                        =  0; #
our $FM_CDP_BIST_IM_h_MASK                        =  3; #

our $FM_CDP_BIST_CLEAR_REPAIRS_b_ClearRepairs     =  0; #

our $FM_CDP_BIST_ADD_REPAIR_b_UpdateFNR           =  27; #
our $FM_CDP_BIST_ADD_REPAIR_l_CompressedMask      =  16; #
our $FM_CDP_BIST_ADD_REPAIR_h_CompressedMask      =  26; #
our $FM_CDP_BIST_ADD_REPAIR_l_RepairAddress       =  0; #
our $FM_CDP_BIST_ADD_REPAIR_h_RepairAddress       =  15; #

our $FM_CDP_BIST_USER_OP_l_Operation              =  12; #
our $FM_CDP_BIST_USER_OP_h_Operation              =  13; #
our $FM_CDP_BIST_USER_OP_l_Index                  =  16; #
our $FM_CDP_BIST_USER_OP_h_Index                  =  22; #
our $FM_CDP_BIST_USER_OP_l_Block                  =  10; #
our $FM_CDP_BIST_USER_OP_h_Block                  =  11; #
our $FM_CDP_BIST_USER_OP_l_HalfChunk              =  23; #
our $FM_CDP_BIST_USER_OP_h_HalfChunk              =  31; #
our $FM_CDP_BIST_USER_OP_b_reserved               =  14; #
our $FM_CDP_BIST_USER_OP_l_DataValue              =  0; #
our $FM_CDP_BIST_USER_OP_h_DataValue              =  7; #
our $FM_CDP_BIST_USER_OP_b_RAMCoreReset           =  15; #

our $FM_CDP_BIST_START_SEQUENCE_l_SequenceType    =  0; #
our $FM_CDP_BIST_START_SEQUENCE_h_SequenceType    =  2; #

our $FM_CDP_BIST_BLOCK_HALFCHUNK_l_Block          =  9; #
our $FM_CDP_BIST_BLOCK_HALFCHUNK_h_Block          =  10; #
our $FM_CDP_BIST_BLOCK_HALFCHUNK_l_HalfChunk      =  0; #
our $FM_CDP_BIST_BLOCK_HALFCHUNK_h_HalfChunk      =  8; #

our $FM_CDP_BIST_SEGMENT_COUNT_b_HaveEnoughSegments  =  17; #
our $FM_CDP_BIST_SEGMENT_COUNT_b_SegmentCountValid  =  16; #
our $FM_CDP_BIST_SEGMENT_COUNT_l_GoodSegments     =  0; #
our $FM_CDP_BIST_SEGMENT_COUNT_h_GoodSegments     =  15; #
our $FM_CDP_BIST_SEGMENT_COUNT_b_ReachedLastSegment  =  18; #

our $FM_CDP_BIST_NEXT_SEGMENT_b_HaveEnoughSegments  =  17; #
our $FM_CDP_BIST_NEXT_SEGMENT_b_IsGood            =  16; #
our $FM_CDP_BIST_NEXT_SEGMENT_l_Segment           =  0; #
our $FM_CDP_BIST_NEXT_SEGMENT_h_Segment           =  15; #
our $FM_CDP_BIST_NEXT_SEGMENT_b_SequencerBusy     =  19; #
our $FM_CDP_BIST_NEXT_SEGMENT_b_ReachedLastSegment  =  18; #

our $FM_CDP_BIST_NEXT_NEW_REPAIR_b_IsNewRepair    =  28; #
our $FM_CDP_BIST_NEXT_NEW_REPAIR_l_CompressedMask  =  16; #
our $FM_CDP_BIST_NEXT_NEW_REPAIR_h_CompressedMask  =  26; #
our $FM_CDP_BIST_NEXT_NEW_REPAIR_b_RepairEntryValid  =  27; #
our $FM_CDP_BIST_NEXT_NEW_REPAIR_b_NoMoreRepairs  =  29; #
our $FM_CDP_BIST_NEXT_NEW_REPAIR_l_RepairAddress  =  0; #
our $FM_CDP_BIST_NEXT_NEW_REPAIR_h_RepairAddress  =  15; #

our $FM_CDP_BIST_SAVED_ADDRESS_l_Address          =  0; #
our $FM_CDP_BIST_SAVED_ADDRESS_h_Address          =  15; #
our $FM_CDP_BIST_SAVED_ADDRESS_l_CurrentBlock     =  16; #
our $FM_CDP_BIST_SAVED_ADDRESS_h_CurrentBlock     =  17; #
our $FM_CDP_BIST_SAVED_ADDRESS_b_WasKnownBad      =  18; #

our $FM_CDP_BIST_DEBUG_STATUS_1_l_BISTIndex       =  6; #
our $FM_CDP_BIST_DEBUG_STATUS_1_h_BISTIndex       =  10; #
our $FM_CDP_BIST_DEBUG_STATUS_1_l_BISTIndexME     =  11; #
our $FM_CDP_BIST_DEBUG_STATUS_1_h_BISTIndexME     =  15; #
our $FM_CDP_BIST_DEBUG_STATUS_1_l_State           =  0; #
our $FM_CDP_BIST_DEBUG_STATUS_1_h_State           =  5; #
our $FM_CDP_BIST_DEBUG_STATUS_1_l_ChainWait       =  16; #
our $FM_CDP_BIST_DEBUG_STATUS_1_h_ChainWait       =  17; #

our $FM_CDP_BIST_DEBUG_STATUS_2_l_Controller1     =  16; #
our $FM_CDP_BIST_DEBUG_STATUS_2_h_Controller1     =  31; #
our $FM_CDP_BIST_DEBUG_STATUS_2_l_Controller0     =  0; #
our $FM_CDP_BIST_DEBUG_STATUS_2_h_Controller0     =  15; #

our $FM_CDP_BIST_CHAIN_GENERAL_CONFIG_b_DisableChainOps  =  0; #

our $FM_CDP_BIST_CHAIN_LATENCY_l_InjectDrainSpacing  =  0; #
our $FM_CDP_BIST_CHAIN_LATENCY_h_InjectDrainSpacing  =  4; #

our $FM_CDP_BIST_CHAIN_CDC_b_Valid                =  8; #
our $FM_CDP_BIST_CHAIN_CDC_b_Inject               =  10; #
our $FM_CDP_BIST_CHAIN_CDC_b_Enable               =  9; #
our $FM_CDP_BIST_CHAIN_CDC_l_Data                 =  0; #
our $FM_CDP_BIST_CHAIN_CDC_h_Data                 =  7; #
our $FM_CDP_BIST_CHAIN_CDC_l_ExtraData            =  12; #
our $FM_CDP_BIST_CHAIN_CDC_h_ExtraData            =  19; #
our $FM_CDP_BIST_CHAIN_CDC_b_Drain                =  11; #

our $FM_SPDP_BIST_MARCH_CONFIG_b_DelayedWrites    =  20; #
our $FM_SPDP_BIST_MARCH_CONFIG_b_Enabled          =  21; #
our $FM_SPDP_BIST_MARCH_CONFIG_l_ValueType        =  18; #
our $FM_SPDP_BIST_MARCH_CONFIG_h_ValueType        =  19; #
our $FM_SPDP_BIST_MARCH_CONFIG_l_FirstMarchSeqIndex  =  8; #
our $FM_SPDP_BIST_MARCH_CONFIG_h_FirstMarchSeqIndex  =  12; #
our $FM_SPDP_BIST_MARCH_CONFIG_l_LastMarchSeqIndex  =  13; #
our $FM_SPDP_BIST_MARCH_CONFIG_h_LastMarchSeqIndex  =  17; #
our $FM_SPDP_BIST_MARCH_CONFIG_l_Value            =  0; #
our $FM_SPDP_BIST_MARCH_CONFIG_h_Value            =  7; #

our $FM_SPDP_BIST_GENERAL_CONFIG_b_ResetAfterRead  =  0; #
our $FM_SPDP_BIST_GENERAL_CONFIG_b_FlushAfterReset  =  4; #
our $FM_SPDP_BIST_GENERAL_CONFIG_b_StopOnDefect   =  1; #
our $FM_SPDP_BIST_GENERAL_CONFIG_b_FlushDelayedWriteBeforeRead  =  2; #
our $FM_SPDP_BIST_GENERAL_CONFIG_b_AddressFormat  =  3; #

our $FM_SPDP_BIST_STATUS_b_FoundDefect            =  1; #
our $FM_SPDP_BIST_STATUS_b_LastOpHadDefect        =  0; #

our $FM_SPDP_BIST_USER_CHECKER_MASKS_l_HighHalfChunkMask  =  7; #
our $FM_SPDP_BIST_USER_CHECKER_MASKS_h_HighHalfChunkMask  =  13; #
our $FM_SPDP_BIST_USER_CHECKER_MASKS_l_LowHalfChunkMask  =  0; #
our $FM_SPDP_BIST_USER_CHECKER_MASKS_h_LowHalfChunkMask  =  6; #

our $FM_SPDP_BIST_IP_b_CDCTimeout                 =  0; #
our $FM_SPDP_BIST_IP_b_FoundDefectInNonRepairableBank  =  3; #
our $FM_SPDP_BIST_IP_b_StoppedOnDefect            =  2; #
our $FM_SPDP_BIST_IP_b_OperationRequestedWhileBusy  =  1; #

our $FM_SPDP_BIST_IM_l_MASK                       =  0; #
our $FM_SPDP_BIST_IM_h_MASK                       =  3; #

our $FM_SPDP_BIST_MAX_ADDR_l_MaxAddr              =  0; #
our $FM_SPDP_BIST_MAX_ADDR_h_MaxAddr              =  15; #

our $FM_SPDP_BIST_USER_OP_l_Operation             =  12; #
our $FM_SPDP_BIST_USER_OP_h_Operation             =  13; #
our $FM_SPDP_BIST_USER_OP_l_Address               =  16; #
our $FM_SPDP_BIST_USER_OP_h_Address               =  31; #
our $FM_SPDP_BIST_USER_OP_b_Block                 =  10; #
our $FM_SPDP_BIST_USER_OP_b_reserved              =  14; #
our $FM_SPDP_BIST_USER_OP_l_DataValue             =  0; #
our $FM_SPDP_BIST_USER_OP_h_DataValue             =  7; #
our $FM_SPDP_BIST_USER_OP_b_RAMCoreReset          =  15; #

our $FM_SPDP_BIST_START_SEQUENCE_b_OnlyOneBlock   =  1; #
our $FM_SPDP_BIST_START_SEQUENCE_b_StartingBlock  =  0; #

our $FM_SPDP_BIST_SAVED_ADDRESS_l_Address         =  0; #
our $FM_SPDP_BIST_SAVED_ADDRESS_h_Address         =  15; #
our $FM_SPDP_BIST_SAVED_ADDRESS_b_CurrentBlock    =  16; #

our $FM_SPDP_BIST_DEBUG_STATUS_1_l_BISTIndex      =  6; #
our $FM_SPDP_BIST_DEBUG_STATUS_1_h_BISTIndex      =  10; #
our $FM_SPDP_BIST_DEBUG_STATUS_1_l_BISTIndexME    =  11; #
our $FM_SPDP_BIST_DEBUG_STATUS_1_h_BISTIndexME    =  15; #
our $FM_SPDP_BIST_DEBUG_STATUS_1_l_State          =  0; #
our $FM_SPDP_BIST_DEBUG_STATUS_1_h_State          =  5; #
our $FM_SPDP_BIST_DEBUG_STATUS_1_l_ChainWait      =  16; #
our $FM_SPDP_BIST_DEBUG_STATUS_1_h_ChainWait      =  17; #

our $FM_SPDP_BIST_DEBUG_STATUS_2_l_Controller1    =  16; #
our $FM_SPDP_BIST_DEBUG_STATUS_2_h_Controller1    =  31; #
our $FM_SPDP_BIST_DEBUG_STATUS_2_l_Controller0    =  0; #
our $FM_SPDP_BIST_DEBUG_STATUS_2_h_Controller0    =  15; #

our $FM_SPDP_BIST_CHAIN_GENERAL_CONFIG_b_DisableChainOps  =  1; #
our $FM_SPDP_BIST_CHAIN_GENERAL_CONFIG_b_ReportNonRepairableDetails  =  2; #
our $FM_SPDP_BIST_CHAIN_GENERAL_CONFIG_b_IsPostRepair  =  0; #

our $FM_SPDP_BIST_CHAIN_LATENCY_l_InjectDrainSpacing  =  0; #
our $FM_SPDP_BIST_CHAIN_LATENCY_h_InjectDrainSpacing  =  4; #

our $FM_SPDP_BIST_CHAIN_CDC_b_Valid               =  8; #
our $FM_SPDP_BIST_CHAIN_CDC_b_Inject              =  10; #
our $FM_SPDP_BIST_CHAIN_CDC_b_Enable              =  9; #
our $FM_SPDP_BIST_CHAIN_CDC_l_Data                =  0; #
our $FM_SPDP_BIST_CHAIN_CDC_h_Data                =  7; #
our $FM_SPDP_BIST_CHAIN_CDC_b_Drain               =  11; #

our $FM_SRBM_REPAIR_l_BadAddress                  =  0; #
our $FM_SRBM_REPAIR_h_BadAddress                  =  15; #
our $FM_SRBM_REPAIR_b_RepairValid                 =  27; #
our $FM_SRBM_REPAIR_l_MemBankID                   =  16; #
our $FM_SRBM_REPAIR_h_MemBankID                   =  23; #
our $FM_SRBM_REPAIR_l_MemChain                    =  24; #
our $FM_SRBM_REPAIR_h_MemChain                    =  26; #

our $FM_SRBM_MARCH_SEQUENCE_l_MarchOperations     =  0; #
our $FM_SRBM_MARCH_SEQUENCE_h_MarchOperations     =  127; #

our $FM_SRBM_GENERAL_CONFIG_b_PruneDefectReporting  =  8; #
our $FM_SRBM_GENERAL_CONFIG_b_AppendToRepairTable  =  9; #
our $FM_SRBM_GENERAL_CONFIG_l_RepairChainDisableMask  =  0; #
our $FM_SRBM_GENERAL_CONFIG_h_RepairChainDisableMask  =  7; #

our $FM_SRBM_STATUS_l_FirstEmptyRepair            =  4; #
our $FM_SRBM_STATUS_h_FirstEmptyRepair            =  7; #
our $FM_SRBM_STATUS_l_FirstNewRepair              =  0; #
our $FM_SRBM_STATUS_h_FirstNewRepair              =  3; #

our $FM_SRBM_IP_b_SliceNumberS2AError             =  3; #
our $FM_SRBM_IP_b_BankRepairS2AError              =  2; #
our $FM_SRBM_IP_b_RepairOverflow                  =  1; #
our $FM_SRBM_IP_b_TwoDefectsInOneBank             =  0; #

our $FM_SRBM_IM_l_MASK                            =  0; #
our $FM_SRBM_IM_h_MASK                            =  3; #

our $FM_SRBM_CLEAR_REPAIRS_b_ClearRepairs         =  0; #

our $FM_SRBM_ADD_REPAIR_l_BadAddress              =  0; #
our $FM_SRBM_ADD_REPAIR_h_BadAddress              =  15; #
our $FM_SRBM_ADD_REPAIR_b_UpdateFNR               =  27; #
our $FM_SRBM_ADD_REPAIR_l_MemBankID               =  16; #
our $FM_SRBM_ADD_REPAIR_h_MemBankID               =  23; #
our $FM_SRBM_ADD_REPAIR_l_MemChain                =  24; #
our $FM_SRBM_ADD_REPAIR_h_MemChain                =  26; #

our $FM_SRBM_NEXT_NEW_REPAIR_l_BadAddress         =  0; #
our $FM_SRBM_NEXT_NEW_REPAIR_h_BadAddress         =  15; #
our $FM_SRBM_NEXT_NEW_REPAIR_b_IsNewRepair        =  28; #
our $FM_SRBM_NEXT_NEW_REPAIR_l_MemBankID          =  16; #
our $FM_SRBM_NEXT_NEW_REPAIR_h_MemBankID          =  23; #
our $FM_SRBM_NEXT_NEW_REPAIR_b_RepairEntryValid   =  27; #
our $FM_SRBM_NEXT_NEW_REPAIR_b_NoMoreRepairs      =  29; #
our $FM_SRBM_NEXT_NEW_REPAIR_l_MemChain           =  24; #
our $FM_SRBM_NEXT_NEW_REPAIR_h_MemChain           =  26; #

our $FM_SRBM_LAST_NR_DEFECT_l_BadAddress          =  0; #
our $FM_SRBM_LAST_NR_DEFECT_h_BadAddress          =  15; #
our $FM_SRBM_LAST_NR_DEFECT_b_DefectValid         =  28; #
our $FM_SRBM_LAST_NR_DEFECT_b_BankType            =  27; #
our $FM_SRBM_LAST_NR_DEFECT_l_MemBankID           =  16; #
our $FM_SRBM_LAST_NR_DEFECT_h_MemBankID           =  23; #
our $FM_SRBM_LAST_NR_DEFECT_l_MemChain            =  24; #
our $FM_SRBM_LAST_NR_DEFECT_h_MemChain            =  26; #

our $FM_CRM_DATA_l_Data                           =  0; #
our $FM_CRM_DATA_h_Data                           =  31; #

our $FM_CRM_CTRL_b_Continuous                     =  13; #
our $FM_CRM_CTRL_l_TimeoutPrescale                =  19; #
our $FM_CRM_CTRL_h_TimeoutPrescale                =  23; #
our $FM_CRM_CTRL_l_TickPrescale                   =  14; #
our $FM_CRM_CTRL_h_TickPrescale                   =  18; #
our $FM_CRM_CTRL_l_FirstCommandIndex              =  1; #
our $FM_CRM_CTRL_h_FirstCommandIndex              =  6; #
our $FM_CRM_CTRL_b_Run                            =  0; #
our $FM_CRM_CTRL_l_LastCommandIndex               =  7; #
our $FM_CRM_CTRL_h_LastCommandIndex               =  12; #

our $FM_CRM_STATUS_l_CommandIndex                 =  1; #
our $FM_CRM_STATUS_h_CommandIndex                 =  6; #
our $FM_CRM_STATUS_b_Running                      =  0; #

our $FM_CRM_TIME_l_Tick                           =  0; #
our $FM_CRM_TIME_h_Tick                           =  31; #

our $FM_CRM_IP_l_InterruptPending                 =  0; #
our $FM_CRM_IP_h_InterruptPending                 =  63; #

our $FM_CRM_IM_l_InterruptMask                    =  0; #
our $FM_CRM_IM_h_InterruptMask                    =  63; #

our $FM_CRM_COMMAND_l_Count                       =  14; #
our $FM_CRM_COMMAND_h_Count                       =  33; #
our $FM_CRM_COMMAND_l_Command                     =  0; #
our $FM_CRM_COMMAND_h_Command                     =  2; #
our $FM_CRM_COMMAND_l_DataIndex                   =  3; #
our $FM_CRM_COMMAND_h_DataIndex                   =  13; #

our $FM_CRM_REGISTER_l_Stride2Shift               =  36; #
our $FM_CRM_REGISTER_h_Stride2Shift               =  39; #
our $FM_CRM_REGISTER_l_BlockSize2Shift            =  32; #
our $FM_CRM_REGISTER_h_BlockSize2Shift            =  35; #
our $FM_CRM_REGISTER_l_BlockSize1Shift            =  24; #
our $FM_CRM_REGISTER_h_BlockSize1Shift            =  27; #
our $FM_CRM_REGISTER_l_Stride1Shift               =  28; #
our $FM_CRM_REGISTER_h_Stride1Shift               =  31; #
our $FM_CRM_REGISTER_l_BaseAddress                =  0; #
our $FM_CRM_REGISTER_h_BaseAddress                =  21; #
our $FM_CRM_REGISTER_l_Size                       =  22; #
our $FM_CRM_REGISTER_h_Size                       =  23; #

our $FM_CRM_PERIOD_l_Interval                     =  0; #
our $FM_CRM_PERIOD_h_Interval                     =  31; #
our $FM_CRM_PERIOD_l_LastTick                     =  32; #
our $FM_CRM_PERIOD_h_LastTick                     =  63; #

our $FM_CRM_PARAM_l_Data                          =  0; #
our $FM_CRM_PARAM_h_Data                          =  31; #

our $FM_CM_PORT_TXMP_USAGE_l_SegmentCount         =  0; #
our $FM_CM_PORT_TXMP_USAGE_h_SegmentCount         =  15; #

our $FM_CM_PORT_TXMP_IP_WM_l_MaxSegmentLimit      =  0; #
our $FM_CM_PORT_TXMP_IP_WM_h_MaxSegmentLimit      =  15; #
our $FM_CM_PORT_TXMP_IP_WM_l_MinSegmentLimit      =  16; #
our $FM_CM_PORT_TXMP_IP_WM_h_MinSegmentLimit      =  31; #

our $FM_CM_PORT_TXMP_SAMPLING_PERIOD_l_SegmentPeriod  =  0; #
our $FM_CM_PORT_TXMP_SAMPLING_PERIOD_h_SegmentPeriod  =  15; #
our $FM_CM_PORT_TXMP_SAMPLING_PERIOD_b_SamplingEnabled  =  20; #
our $FM_CM_PORT_TXMP_SAMPLING_PERIOD_l_SegmentJitter  =  16; #
our $FM_CM_PORT_TXMP_SAMPLING_PERIOD_h_SegmentJitter  =  19; #

our $FM_CM_PORT_TXMP_SAMPLING_STATE_l_LastSample  =  0; #
our $FM_CM_PORT_TXMP_SAMPLING_STATE_h_LastSample  =  15; #
our $FM_CM_PORT_TXMP_SAMPLING_STATE_l_CurrentPeriod  =  16; #
our $FM_CM_PORT_TXMP_SAMPLING_STATE_h_CurrentPeriod  =  31; #

our $FM_CM_PORT_TXMP_ABOVE_IP_l_Pending           =  0; #
our $FM_CM_PORT_TXMP_ABOVE_IP_h_Pending           =  79; #

our $FM_CM_PORT_TXMP_BELOW_IP_l_Pending           =  0; #
our $FM_CM_PORT_TXMP_BELOW_IP_h_Pending           =  79; #

our $FM_CM_PORT_TXMP_ABOVE_IM_l_Mask              =  0; #
our $FM_CM_PORT_TXMP_ABOVE_IM_h_Mask              =  79; #

our $FM_CM_PORT_TXMP_BELOW_IM_l_Mask              =  0; #
our $FM_CM_PORT_TXMP_BELOW_IM_h_Mask              =  79; #

our $FM_CM_INTERRUPT_DETECT_l_Below               =  12; #
our $FM_CM_INTERRUPT_DETECT_h_Below               =  23; #
our $FM_CM_INTERRUPT_DETECT_l_Above               =  0; #
our $FM_CM_INTERRUPT_DETECT_h_Above               =  11; #

our $FM_FC_MRL_MGMT_CYCLES_l_Cycles               =  0; #
our $FM_FC_MRL_MGMT_CYCLES_h_Cycles               =  5; #
our $FM_FC_MRL_MGMT_CYCLES_l_ReadInterval         =  12; #
our $FM_FC_MRL_MGMT_CYCLES_h_ReadInterval         =  19; #
our $FM_FC_MRL_MGMT_CYCLES_l_WriteInterval        =  6; #
our $FM_FC_MRL_MGMT_CYCLES_h_WriteInterval        =  11; #

our $FM_FC_MRL_SWEEP_CYCLES_l_Cycles              =  0; #
our $FM_FC_MRL_SWEEP_CYCLES_h_Cycles              =  5; #

our $FM_FC_MRL_SWEEP_PERIOD_l_Period              =  0; #
our $FM_FC_MRL_SWEEP_PERIOD_h_Period              =  7; #

our $FM_FC_MRL_UNROLL_ITER_l_Cycles               =  0; #
our $FM_FC_MRL_UNROLL_ITER_h_Cycles               =  11; #

our $FM_FC_MRL_TOKEN_LIMIT_l_Limit                =  0; #
our $FM_FC_MRL_TOKEN_LIMIT_h_Limit                =  5; #
our $FM_FC_MRL_TOKEN_LIMIT_l_MaxLevel             =  12; #
our $FM_FC_MRL_TOKEN_LIMIT_h_MaxLevel             =  17; #
our $FM_FC_MRL_TOKEN_LIMIT_l_Level                =  6; #
our $FM_FC_MRL_TOKEN_LIMIT_h_Level                =  11; #

our $FM_FC_MRL_FC_TOKEN_LIMIT_l_Limit             =  0; #
our $FM_FC_MRL_FC_TOKEN_LIMIT_h_Limit             =  9; #
our $FM_FC_MRL_FC_TOKEN_LIMIT_l_MaxLevel          =  20; #
our $FM_FC_MRL_FC_TOKEN_LIMIT_h_MaxLevel          =  29; #
our $FM_FC_MRL_FC_TOKEN_LIMIT_l_Level             =  10; #
our $FM_FC_MRL_FC_TOKEN_LIMIT_h_Level             =  19; #
our $FM_FC_MRL_FC_TOKEN_LIMIT_b_Enable            =  30; #

our $FM_FC_MRL_ALIGN_TX_STATS_b_AlignTXStats      =  0; #

our $FM_FC_MRL_RATE_LIMITER_b_Enable              =  10; #
our $FM_FC_MRL_RATE_LIMITER_l_Period              =  0; #
our $FM_FC_MRL_RATE_LIMITER_h_Period              =  9; #

our $FM_L2L_MAC_TABLE_CFG_b_Ma2SuppressMode       =  14; #
our $FM_L2L_MAC_TABLE_CFG_l_HashRotation0         =  1; #
our $FM_L2L_MAC_TABLE_CFG_h_HashRotation0         =  4; #
our $FM_L2L_MAC_TABLE_CFG_l_HashRotation1         =  5; #
our $FM_L2L_MAC_TABLE_CFG_h_HashRotation1         =  8; #
our $FM_L2L_MAC_TABLE_CFG_b_FullTableMode         =  0; #
our $FM_L2L_MAC_TABLE_CFG_b_HashMode              =  9; #
our $FM_L2L_MAC_TABLE_CFG_l_LookupThreshold       =  10; #
our $FM_L2L_MAC_TABLE_CFG_h_LookupThreshold       =  13; #

our $FM_L2L_CMD_PROFILE_TABLE_l_MA2_HitPrecMask   =  16; #
our $FM_L2L_CMD_PROFILE_TABLE_h_MA2_HitPrecMask   =  19; #
our $FM_L2L_CMD_PROFILE_TABLE_b_MA1_FID1_CMD      =  0; #
our $FM_L2L_CMD_PROFILE_TABLE_l_MA2_FID2_CMD      =  7; #
our $FM_L2L_CMD_PROFILE_TABLE_h_MA2_FID2_CMD      =  8; #
our $FM_L2L_CMD_PROFILE_TABLE_b_MA2_FID1_CMD      =  6; #
our $FM_L2L_CMD_PROFILE_TABLE_l_MA1_FID2_CMD      =  1; #
our $FM_L2L_CMD_PROFILE_TABLE_h_MA1_FID2_CMD      =  2; #
our $FM_L2L_CMD_PROFILE_TABLE_l_MA1_CMD           =  3; #
our $FM_L2L_CMD_PROFILE_TABLE_h_MA1_CMD           =  5; #
our $FM_L2L_CMD_PROFILE_TABLE_l_MA2_CMD           =  9; #
our $FM_L2L_CMD_PROFILE_TABLE_h_MA2_CMD           =  11; #
our $FM_L2L_CMD_PROFILE_TABLE_l_MA1_HitPrecMask   =  12; #
our $FM_L2L_CMD_PROFILE_TABLE_h_MA1_HitPrecMask   =  15; #

our $FM_L2L_LOCK_TABLE_b_Locked_26                =  26; #
our $FM_L2L_LOCK_TABLE_b_Locked_25                =  25; #
our $FM_L2L_LOCK_TABLE_b_Locked_28                =  28; #
our $FM_L2L_LOCK_TABLE_b_Locked_27                =  27; #
our $FM_L2L_LOCK_TABLE_b_Locked_29                =  29; #
our $FM_L2L_LOCK_TABLE_b_Locked_8                 =  8; #
our $FM_L2L_LOCK_TABLE_b_Locked_7                 =  7; #
our $FM_L2L_LOCK_TABLE_b_Locked_20                =  20; #
our $FM_L2L_LOCK_TABLE_b_Locked_6                 =  6; #
our $FM_L2L_LOCK_TABLE_b_Locked_5                 =  5; #
our $FM_L2L_LOCK_TABLE_b_Locked_22                =  22; #
our $FM_L2L_LOCK_TABLE_b_Locked_21                =  21; #
our $FM_L2L_LOCK_TABLE_b_Locked_24                =  24; #
our $FM_L2L_LOCK_TABLE_b_Locked_23                =  23; #
our $FM_L2L_LOCK_TABLE_b_Locked_9                 =  9; #
our $FM_L2L_LOCK_TABLE_b_Locked_39                =  39; #
our $FM_L2L_LOCK_TABLE_b_Locked_38                =  38; #
our $FM_L2L_LOCK_TABLE_b_Locked_37                =  37; #
our $FM_L2L_LOCK_TABLE_b_Locked_36                =  36; #
our $FM_L2L_LOCK_TABLE_b_Locked_31                =  31; #
our $FM_L2L_LOCK_TABLE_b_Locked_30                =  30; #
our $FM_L2L_LOCK_TABLE_b_Locked_35                =  35; #
our $FM_L2L_LOCK_TABLE_b_Locked_34                =  34; #
our $FM_L2L_LOCK_TABLE_b_Locked_33                =  33; #
our $FM_L2L_LOCK_TABLE_b_Locked_32                =  32; #
our $FM_L2L_LOCK_TABLE_b_Locked_48                =  48; #
our $FM_L2L_LOCK_TABLE_b_Locked_47                =  47; #
our $FM_L2L_LOCK_TABLE_b_Locked_49                =  49; #
our $FM_L2L_LOCK_TABLE_b_Locked_44                =  44; #
our $FM_L2L_LOCK_TABLE_b_Locked_43                =  43; #
our $FM_L2L_LOCK_TABLE_b_Locked_46                =  46; #
our $FM_L2L_LOCK_TABLE_b_Locked_45                =  45; #
our $FM_L2L_LOCK_TABLE_b_Locked_40                =  40; #
our $FM_L2L_LOCK_TABLE_b_Locked_42                =  42; #
our $FM_L2L_LOCK_TABLE_b_Locked_41                =  41; #
our $FM_L2L_LOCK_TABLE_b_Locked_1                 =  1; #
our $FM_L2L_LOCK_TABLE_b_Locked_2                 =  2; #
our $FM_L2L_LOCK_TABLE_b_Locked_3                 =  3; #
our $FM_L2L_LOCK_TABLE_b_Locked_4                 =  4; #
our $FM_L2L_LOCK_TABLE_b_Locked_0                 =  0; #
our $FM_L2L_LOCK_TABLE_b_Locked_63                =  63; #
our $FM_L2L_LOCK_TABLE_b_Locked_61                =  61; #
our $FM_L2L_LOCK_TABLE_b_Locked_62                =  62; #
our $FM_L2L_LOCK_TABLE_b_Locked_60                =  60; #
our $FM_L2L_LOCK_TABLE_b_Locked_12                =  12; #
our $FM_L2L_LOCK_TABLE_b_Locked_13                =  13; #
our $FM_L2L_LOCK_TABLE_b_Locked_10                =  10; #
our $FM_L2L_LOCK_TABLE_b_Locked_11                =  11; #
our $FM_L2L_LOCK_TABLE_b_Locked_59                =  59; #
our $FM_L2L_LOCK_TABLE_b_Locked_58                =  58; #
our $FM_L2L_LOCK_TABLE_b_Locked_57                =  57; #
our $FM_L2L_LOCK_TABLE_b_Locked_56                =  56; #
our $FM_L2L_LOCK_TABLE_b_Locked_55                =  55; #
our $FM_L2L_LOCK_TABLE_b_Locked_18                =  18; #
our $FM_L2L_LOCK_TABLE_b_Locked_54                =  54; #
our $FM_L2L_LOCK_TABLE_b_Locked_19                =  19; #
our $FM_L2L_LOCK_TABLE_b_Locked_53                =  53; #
our $FM_L2L_LOCK_TABLE_b_Locked_16                =  16; #
our $FM_L2L_LOCK_TABLE_b_Locked_52                =  52; #
our $FM_L2L_LOCK_TABLE_b_Locked_17                =  17; #
our $FM_L2L_LOCK_TABLE_b_Locked_51                =  51; #
our $FM_L2L_LOCK_TABLE_b_Locked_14                =  14; #
our $FM_L2L_LOCK_TABLE_b_Locked_50                =  50; #
our $FM_L2L_LOCK_TABLE_b_Locked_15                =  15; #

our $FM_L2L_EVID1_TABLE_l_MA1_FID1                =  0; #
our $FM_L2L_EVID1_TABLE_h_MA1_FID1                =  11; #
our $FM_L2L_EVID1_TABLE_l_ETAG1                   =  13; #
our $FM_L2L_EVID1_TABLE_h_ETAG1                   =  24; #
our $FM_L2L_EVID1_TABLE_l_ET_IDX                  =  25; #
our $FM_L2L_EVID1_TABLE_h_ET_IDX                  =  32; #
our $FM_L2L_EVID1_TABLE_b_MA1_FID2_IVL            =  12; #

our $FM_L2L_IVID1_TABLE_l_IT_IDX                  =  25; #
our $FM_L2L_IVID1_TABLE_h_IT_IDX                  =  32; #
our $FM_L2L_IVID1_TABLE_b_MA2_FID2_IVL            =  12; #
our $FM_L2L_IVID1_TABLE_l_ITAG1                   =  13; #
our $FM_L2L_IVID1_TABLE_h_ITAG1                   =  24; #
our $FM_L2L_IVID1_TABLE_l_MA2_FID1                =  0; #
our $FM_L2L_IVID1_TABLE_h_MA2_FID1                =  11; #

our $FM_L2L_EVID2_TABLE_l_MA1_FID2                =  0; #
our $FM_L2L_EVID2_TABLE_h_MA1_FID2                =  11; #
our $FM_L2L_EVID2_TABLE_l_ETAG2                   =  12; #
our $FM_L2L_EVID2_TABLE_h_ETAG2                   =  23; #

our $FM_L2L_IVID2_TABLE_l_ITAG2                   =  12; #
our $FM_L2L_IVID2_TABLE_h_ITAG2                   =  23; #
our $FM_L2L_IVID2_TABLE_l_MA2_FID2                =  0; #
our $FM_L2L_IVID2_TABLE_h_MA2_FID2                =  11; #

our $FM_SAF_MATRIX_l_CutThruMode                  =  80; #
our $FM_SAF_MATRIX_h_CutThruMode                  =  81; #
our $FM_SAF_MATRIX_l_EnableSNF                    =  0; #
our $FM_SAF_MATRIX_h_EnableSNF                    =  79; #

our $FM_PORT_STATUS_b_Transmitting                =  9; #
our $FM_PORT_STATUS_l_LinkFaultDebounced          =  0; #
our $FM_PORT_STATUS_h_LinkFaultDebounced          =  1; #
our $FM_PORT_STATUS_b_SerXmit                     =  11; #
our $FM_PORT_STATUS_l_LinkFaultRx                 =  4; #
our $FM_PORT_STATUS_h_LinkFaultRx                 =  5; #
our $FM_PORT_STATUS_b_RxLinkUp                    =  6; #
our $FM_PORT_STATUS_b_HiBer                       =  8; #
our $FM_PORT_STATUS_b_Receiving                   =  10; #
our $FM_PORT_STATUS_b_HeartbeatOk                 =  7; #
our $FM_PORT_STATUS_l_LinkFaultMac                =  2; #
our $FM_PORT_STATUS_h_LinkFaultMac                =  3; #

our $FM_AN_IM_b_An37AcknowledgeDetect             =  13; #
our $FM_AN_IM_b_An37AnDisableLinkOk               =  11; #
our $FM_AN_IM_b_An37NextPageWait                  =  15; #
our $FM_AN_IM_b_An73AbilityDetect                 =  1; #
our $FM_AN_IM_b_An37MrPageRx                      =  18; #
our $FM_AN_IM_b_An37LinkOk                        =  17; #
our $FM_AN_IM_b_An73TransmitDisable               =  0; #
our $FM_AN_IM_b_An37AnEnable                      =  9; #
our $FM_AN_IM_b_An37IdleDetect                    =  16; #
our $FM_AN_IM_b_An73AnGoodCheck                   =  5; #
our $FM_AN_IM_b_An37CompleteAcknowledge           =  14; #
our $FM_AN_IM_b_An73NextPageWait                  =  4; #
our $FM_AN_IM_b_An37AnRestart                     =  10; #
our $FM_AN_IM_b_An37AbilityDetect                 =  12; #
our $FM_AN_IM_b_An73AcknowledgeDetect             =  2; #
our $FM_AN_IM_b_An73ReceiveIdle                   =  7; #
our $FM_AN_IM_b_An73MrPageRx                      =  8; #
our $FM_AN_IM_b_An73CompleteAcknowledge           =  3; #
our $FM_AN_IM_b_An73AnGood                        =  6; #

our $FM_LINK_IM_b_MacRxFrame                      =  11; #
our $FM_LINK_IM_b_ErrorRxFcs                      =  17; #
our $FM_LINK_IM_b_ErrorTxAlign                    =  24; #
our $FM_LINK_IM_b_Pcs1000basexSyncStatus          =  3; #
our $FM_LINK_IM_b_PcsBaserHiBer                   =  8; #
our $FM_LINK_IM_b_LinkFaultDebounced              =  0; #
our $FM_LINK_IM_b_ErrorRxPreamble                 =  14; #
our $FM_LINK_IM_b_EgressTimeStamp                 =  26; #
our $FM_LINK_IM_b_LinkFaultRx                     =  1; #
our $FM_LINK_IM_b_ErrorRxCode                     =  15; #
our $FM_LINK_IM_b_ErrorRxUndersize                =  20; #
our $FM_LINK_IM_b_Pcs10gbasexSyncStatus           =  5; #
our $FM_LINK_IM_b_Pcs10gbaserBlockLock            =  7; #
our $FM_LINK_IM_b_Pcs10gbasexDeskewError          =  6; #
our $FM_LINK_IM_b_Pcs10gbasexAlignStatus          =  4; #
our $FM_LINK_IM_b_MacRxFaultSequence              =  12; #
our $FM_LINK_IM_b_ErrorRxOversize                 =  18; #
our $FM_LINK_IM_b_ErrorTxUnderrun                 =  25; #
our $FM_LINK_IM_b_ErrorRxAlignOverflow            =  23; #
our $FM_LINK_IM_b_RxLinkUp                        =  2; #
our $FM_LINK_IM_b_ErrorRxOverrun                  =  22; #
our $FM_LINK_IM_b_Pcs40gbaserBlockLockAllLanes    =  9; #
our $FM_LINK_IM_b_ErrorRxFrame                    =  16; #
our $FM_LINK_IM_b_ErrorRxJabber                   =  19; #
our $FM_LINK_IM_b_Pcs40gbaserAlignStatus          =  10; #
our $FM_LINK_IM_b_ErrorRxRunt                     =  21; #
our $FM_LINK_IM_b_ErrorTxFcs                      =  13; #

our $FM_AN_IP_b_An37AcknowledgeDetect             =  13; #
our $FM_AN_IP_b_An37AnDisableLinkOk               =  11; #
our $FM_AN_IP_b_An37NextPageWait                  =  15; #
our $FM_AN_IP_b_An73AbilityDetect                 =  1; #
our $FM_AN_IP_b_An37MrPageRx                      =  18; #
our $FM_AN_IP_b_An37LinkOk                        =  17; #
our $FM_AN_IP_b_An73TransmitDisable               =  0; #
our $FM_AN_IP_b_An37AnEnable                      =  9; #
our $FM_AN_IP_b_An37IdleDetect                    =  16; #
our $FM_AN_IP_b_An73AnGoodCheck                   =  5; #
our $FM_AN_IP_b_An37CompleteAcknowledge           =  14; #
our $FM_AN_IP_b_An73NextPageWait                  =  4; #
our $FM_AN_IP_b_An37AnRestart                     =  10; #
our $FM_AN_IP_b_An37AbilityDetect                 =  12; #
our $FM_AN_IP_b_An73AcknowledgeDetect             =  2; #
our $FM_AN_IP_b_An73ReceiveIdle                   =  7; #
our $FM_AN_IP_b_An73MrPageRx                      =  8; #
our $FM_AN_IP_b_An73CompleteAcknowledge           =  3; #
our $FM_AN_IP_b_An73AnGood                        =  6; #

our $FM_LINK_IP_b_MacRxFrame                      =  11; #
our $FM_LINK_IP_b_ErrorRxFcs                      =  17; #
our $FM_LINK_IP_b_ErrorTxAlign                    =  24; #
our $FM_LINK_IP_b_Pcs1000basexSyncStatus          =  3; #
our $FM_LINK_IP_b_MacRxSequenceOrFsig             =  12; #
our $FM_LINK_IP_b_PcsBaserHiBer                   =  8; #
our $FM_LINK_IP_b_LinkFaultDebounced              =  0; #
our $FM_LINK_IP_b_ErrorRxPreamble                 =  14; #
our $FM_LINK_IP_b_EgressTimeStamp                 =  26; #
our $FM_LINK_IP_b_LinkFaultRx                     =  1; #
our $FM_LINK_IP_b_ErrorRxCode                     =  15; #
our $FM_LINK_IP_b_ErrorRxUndersize                =  20; #
our $FM_LINK_IP_b_Pcs10gbasexSyncStatus           =  5; #
our $FM_LINK_IP_b_Pcs10gbaserBlockLock            =  7; #
our $FM_LINK_IP_b_Pcs10gbasexDeskewError          =  6; #
our $FM_LINK_IP_b_Pcs10gbasexAlignStatus          =  4; #
our $FM_LINK_IP_b_ErrorRxOversize                 =  18; #
our $FM_LINK_IP_b_ErrorTxUnderrun                 =  25; #
our $FM_LINK_IP_b_ErrorRxAlignOverflow            =  23; #
our $FM_LINK_IP_b_RxLinkUp                        =  2; #
our $FM_LINK_IP_b_ErrorRxOverrun                  =  22; #
our $FM_LINK_IP_b_Pcs40gbaserBlockLockAllLanes    =  9; #
our $FM_LINK_IP_b_ErrorRxFrame                    =  16; #
our $FM_LINK_IP_b_ErrorRxJabber                   =  19; #
our $FM_LINK_IP_b_Pcs40gbaserAlignStatus          =  10; #
our $FM_LINK_IP_b_ErrorRxRunt                     =  21; #
our $FM_LINK_IP_b_ErrorTxFcs                      =  13; #

our $FM_SGMII_AN_TX_CONFIG_LOOPBACK_l_Speed       =  10; #
our $FM_SGMII_AN_TX_CONFIG_LOOPBACK_h_Speed       =  11; #
our $FM_SGMII_AN_TX_CONFIG_LOOPBACK_l_Rsrvd9to1   =  1; #
our $FM_SGMII_AN_TX_CONFIG_LOOPBACK_h_Rsrvd9to1   =  9; #
our $FM_SGMII_AN_TX_CONFIG_LOOPBACK_b_Ack         =  14; #
our $FM_SGMII_AN_TX_CONFIG_LOOPBACK_b_B0          =  0; #
our $FM_SGMII_AN_TX_CONFIG_LOOPBACK_b_Link        =  15; #
our $FM_SGMII_AN_TX_CONFIG_LOOPBACK_b_Rsrvd13     =  13; #
our $FM_SGMII_AN_TX_CONFIG_LOOPBACK_b_Duplex      =  12; #

our $FM_SGMII_AN_RX_CONFIG_l_Speed                =  10; #
our $FM_SGMII_AN_RX_CONFIG_h_Speed                =  11; #
our $FM_SGMII_AN_RX_CONFIG_l_Rsrvd9to1            =  1; #
our $FM_SGMII_AN_RX_CONFIG_h_Rsrvd9to1            =  9; #
our $FM_SGMII_AN_RX_CONFIG_b_Ack                  =  14; #
our $FM_SGMII_AN_RX_CONFIG_b_B0                   =  0; #
our $FM_SGMII_AN_RX_CONFIG_b_Link                 =  15; #
our $FM_SGMII_AN_RX_CONFIG_b_Rsrvd13              =  13; #
our $FM_SGMII_AN_RX_CONFIG_b_Duplex               =  12; #

our $FM_AN_73_NEXT_PAGE_RX_l_U                    =  16; #
our $FM_AN_73_NEXT_PAGE_RX_h_U                    =  47; #
our $FM_AN_73_NEXT_PAGE_RX_b_MP                   =  13; #
our $FM_AN_73_NEXT_PAGE_RX_b_T                    =  11; #
our $FM_AN_73_NEXT_PAGE_RX_b_NP                   =  15; #
our $FM_AN_73_NEXT_PAGE_RX_b_ACK                  =  14; #
our $FM_AN_73_NEXT_PAGE_RX_b_ACK2                 =  12; #
our $FM_AN_73_NEXT_PAGE_RX_l_MU                   =  0; #
our $FM_AN_73_NEXT_PAGE_RX_h_MU                   =  10; #

our $FM_LINK_RULES_l_HeartbeatTimeScale           =  18; #
our $FM_LINK_RULES_h_HeartbeatTimeScale           =  21; #
our $FM_LINK_RULES_l_FaultTicksDown               =  13; #
our $FM_LINK_RULES_h_FaultTicksDown               =  17; #
our $FM_LINK_RULES_l_FaultTicksUp                 =  4; #
our $FM_LINK_RULES_h_FaultTicksUp                 =  8; #
our $FM_LINK_RULES_l_FaultTimeScaleDown           =  9; #
our $FM_LINK_RULES_h_FaultTimeScaleDown           =  12; #
our $FM_LINK_RULES_l_FaultTimeScaleUp             =  0; #
our $FM_LINK_RULES_h_FaultTimeScaleUp             =  3; #

our $FM_MAC_CFG_b_TxPadEnable                     =  37; #
our $FM_MAC_CFG_b_Ieee1588Enable                  =  0; #
our $FM_MAC_CFG_l_RxMinFrameLength                =  56; #
our $FM_MAC_CFG_h_RxMinFrameLength                =  63; #
our $FM_MAC_CFG_l_TxFifoManualTokenShift          =  115; #
our $FM_MAC_CFG_h_TxFifoManualTokenShift          =  117; #
our $FM_MAC_CFG_b_RxFcsForceBad                   =  81; #
our $FM_MAC_CFG_l_TxIdleMinIfgBytes               =  6; #
our $FM_MAC_CFG_h_TxIdleMinIfgBytes               =  11; #
our $FM_MAC_CFG_b_TxFifoManual                    =  111; #
our $FM_MAC_CFG_b_TxClockCompensationEnable       =  12; #
our $FM_MAC_CFG_b_RxIgnoreFcsErrors               =  85; #
our $FM_MAC_CFG_l_RxMaxFrameLength                =  64; #
our $FM_MAC_CFG_h_RxMaxFrameLength                =  79; #
our $FM_MAC_CFG_b_LinkFaultDisable                =  95; #
our $FM_MAC_CFG_b_Ieee1588LastBit                 =  1; #
our $FM_MAC_CFG_b_TxFifoPcBubbleDisable           =  102; #
our $FM_MAC_CFG_b_RxIgnoreCodeErrors              =  82; #
our $FM_MAC_CFG_b_RxIgnoreUndersizeErrors         =  83; #
our $FM_MAC_CFG_b_RxIgnoreOversizeErrors          =  84; #
our $FM_MAC_CFG_l_TxFaultMode                     =  32; #
our $FM_MAC_CFG_h_TxFaultMode                     =  34; #
our $FM_MAC_CFG_b_CjpatEnable                     =  35; #
our $FM_MAC_CFG_b_RxIgnoreIFGErrors               =  87; #
our $FM_MAC_CFG_b_PreambleMode                    =  44; #
our $FM_MAC_CFG_b_SelTrafficPath                  =  31; #
our $FM_MAC_CFG_l_RxMinEventRate                  =  88; #
our $FM_MAC_CFG_h_RxMinEventRate                  =  94; #
our $FM_MAC_CFG_b_FcsStart                        =  45; #
our $FM_MAC_CFG_l_TxFifoHighWatermark             =  107; #
our $FM_MAC_CFG_h_TxFifoHighWatermark             =  110; #
our $FM_MAC_CFG_l_TxDrainMode                     =  29; #
our $FM_MAC_CFG_h_TxDrainMode                     =  30; #
our $FM_MAC_CFG_b_RxIgnorePreambleErrors          =  86; #
our $FM_MAC_CFG_l_TxAntiBubbleWatermark           =  96; #
our $FM_MAC_CFG_h_TxAntiBubbleWatermark           =  101; #
our $FM_MAC_CFG_b_CjpatUseIdle                    =  36; #
our $FM_MAC_CFG_b_CounterWrap                     =  54; #
our $FM_MAC_CFG_l_TxFifoManualTokPreference       =  124; #
our $FM_MAC_CFG_h_TxFifoManualTokPreference       =  125; #
our $FM_MAC_CFG_l_TxFifoManualHistory             =  121; #
our $FM_MAC_CFG_h_TxFifoManualHistory             =  123; #
our $FM_MAC_CFG_l_TxFcsMode                       =  2; #
our $FM_MAC_CFG_h_TxFcsMode                       =  4; #
our $FM_MAC_CFG_b_RxFcsRemove                     =  80; #
our $FM_MAC_CFG_b_RxDrain                         =  55; #
our $FM_MAC_CFG_l_TxFifoLowWatermark              =  103; #
our $FM_MAC_CFG_h_TxFifoLowWatermark              =  106; #
our $FM_MAC_CFG_l_TxFifoManualLevelShift          =  118; #
our $FM_MAC_CFG_h_TxFifoManualLevelShift          =  120; #
our $FM_MAC_CFG_l_TxMinColumns                    =  38; #
our $FM_MAC_CFG_h_TxMinColumns                    =  43; #
our $FM_MAC_CFG_l_TxClockCompensationTimeout      =  13; #
our $FM_MAC_CFG_h_TxClockCompensationTimeout      =  28; #
our $FM_MAC_CFG_l_TxFifoManualBubbleShift         =  112; #
our $FM_MAC_CFG_h_TxFifoManualBubbleShift         =  114; #
our $FM_MAC_CFG_l_StartCharD                      =  46; #
our $FM_MAC_CFG_h_StartCharD                      =  53; #
our $FM_MAC_CFG_b_TxIdleEnableDic                 =  5; #

our $FM_TX_SEQUENCE_l_TxSequenceD                 =  0; #
our $FM_TX_SEQUENCE_h_TxSequenceD                 =  31; #
our $FM_TX_SEQUENCE_l_TxSequenceK                 =  32; #
our $FM_TX_SEQUENCE_h_TxSequenceK                 =  35; #

our $FM_RX_SEQUENCE_l_RxSequenceD                 =  0; #
our $FM_RX_SEQUENCE_h_RxSequenceD                 =  31; #
our $FM_RX_SEQUENCE_l_RxSequenceK                 =  32; #
our $FM_RX_SEQUENCE_h_RxSequenceK                 =  35; #

our $FM_MAC_1588_STATUS_l_EgressTimeStamp         =  0; #
our $FM_MAC_1588_STATUS_h_EgressTimeStamp         =  30; #
our $FM_MAC_1588_STATUS_l_IngressTimeStamp        =  32; #
our $FM_MAC_1588_STATUS_h_IngressTimeStamp        =  62; #

our $FM_MAC_OVERSIZE_COUNTER_l_FrameErrCntOversize  =  0; #
our $FM_MAC_OVERSIZE_COUNTER_h_FrameErrCntOversize  =  31; #

our $FM_MAC_JABBER_COUNTER_l_FrameErrCntJabber    =  0; #
our $FM_MAC_JABBER_COUNTER_h_FrameErrCntJabber    =  31; #

our $FM_MAC_UNDERSIZE_COUNTER_l_FrameErrCntUndersize  =  0; #
our $FM_MAC_UNDERSIZE_COUNTER_h_FrameErrCntUndersize  =  31; #

our $FM_MAC_RUNT_COUNTER_l_FrameErrCntRunt        =  0; #
our $FM_MAC_RUNT_COUNTER_h_FrameErrCntRunt        =  31; #

our $FM_MAC_OVERRUN_COUNTER_l_FrameErrCntOverrun  =  0; #
our $FM_MAC_OVERRUN_COUNTER_h_FrameErrCntOverrun  =  31; #

our $FM_MAC_UNDERRUN_COUNTER_l_UnderrunCnt        =  0; #
our $FM_MAC_UNDERRUN_COUNTER_h_UnderrunCnt        =  31; #

our $FM_MAC_CODE_ERROR_COUNTER_l_CodeErrorCnt     =  0; #
our $FM_MAC_CODE_ERROR_COUNTER_h_CodeErrorCnt     =  31; #

our $FM_MAC_LINK_COUNTER_l_RemoteFault            =  22; #
our $FM_MAC_LINK_COUNTER_h_RemoteFault            =  31; #
our $FM_MAC_LINK_COUNTER_l_NoFault                =  0; #
our $FM_MAC_LINK_COUNTER_h_NoFault                =  11; #
our $FM_MAC_LINK_COUNTER_l_LocalFault             =  12; #
our $FM_MAC_LINK_COUNTER_h_LocalFault             =  21; #

our $FM_PCS_1000BASEX_CFG_b_LsMaskSeenI1orI2      =  0; #
our $FM_PCS_1000BASEX_CFG_b_DisableCheckEnd       =  1; #

our $FM_PCS_1000BASEX_RX_STATUS_b_SyncStatus      =  6; #
our $FM_PCS_1000BASEX_RX_STATUS_b_DecodeCarrierDetect  =  5; #
our $FM_PCS_1000BASEX_RX_STATUS_l_State           =  0; #
our $FM_PCS_1000BASEX_RX_STATUS_h_State           =  4; #

our $FM_PCS_1000BASEX_TX_STATUS_l_DebugTxOsetState  =  4; #
our $FM_PCS_1000BASEX_TX_STATUS_h_DebugTxOsetState  =  7; #
our $FM_PCS_1000BASEX_TX_STATUS_l_DebugCodeGrpState  =  0; #
our $FM_PCS_1000BASEX_TX_STATUS_h_DebugCodeGrpState  =  3; #
our $FM_PCS_1000BASEX_TX_STATUS_l_DebugTxOset     =  8; #
our $FM_PCS_1000BASEX_TX_STATUS_h_DebugTxOset     =  10; #

our $FM_PCS_10GBASER_CFG_b_ScramblerBypass        =  0; #
our $FM_PCS_10GBASER_CFG_b_DescramblerBypass      =  1; #
our $FM_PCS_10GBASER_CFG_b_LsMaskHiBer            =  3; #
our $FM_PCS_10GBASER_CFG_b_LsMaskSeenI            =  2; #

our $FM_PCS_10GBASER_RX_STATUS_b_BlockLock        =  0; #
our $FM_PCS_10GBASER_RX_STATUS_b_HiBer            =  1; #
our $FM_PCS_10GBASER_RX_STATUS_l_BerCnt           =  2; #
our $FM_PCS_10GBASER_RX_STATUS_h_BerCnt           =  9; #

our $FM_PCS_10GBASER_TX_STATUS_l_DebugState       =  0; #
our $FM_PCS_10GBASER_TX_STATUS_h_DebugState       =  2; #

our $FM_AN_37_CFG_b_MrNpAble                      =  1; #
our $FM_AN_37_CFG_b_MrAnEnable                    =  0; #
our $FM_AN_37_CFG_b_NpEnable                      =  2; #
our $FM_AN_37_CFG_b_ToggleNpLoaded                =  3; #

our $FM_AN_37_STATUS_l_CurrentLcw                 =  0; #
our $FM_AN_37_STATUS_h_CurrentLcw                 =  15; #
our $FM_AN_37_STATUS_b_AbilityMatch               =  21; #
our $FM_AN_37_STATUS_b_IdleMatch                  =  24; #
our $FM_AN_37_STATUS_b_MrAnComplete               =  16; #
our $FM_AN_37_STATUS_l_State                      =  17; #
our $FM_AN_37_STATUS_h_State                      =  20; #
our $FM_AN_37_STATUS_b_AcknowledgeMatch           =  22; #
our $FM_AN_37_STATUS_b_LinkTimerDone              =  25; #
our $FM_AN_37_STATUS_l_Rudi                       =  26; #
our $FM_AN_37_STATUS_h_Rudi                       =  27; #
our $FM_AN_37_STATUS_b_ConsistencyMatch           =  23; #

our $FM_AN_73_CFG_b_TxNextPageLoaded              =  4; #
our $FM_AN_73_CFG_b_ClrNonceCnt                   =  3; #
our $FM_AN_73_CFG_b_MrAutonegEnable               =  0; #
our $FM_AN_73_CFG_b_IncompatibleLink              =  2; #
our $FM_AN_73_CFG_b_IgnoreNonceMatch              =  1; #

our $FM_AN_73_TIMER_CFG_l_TimeScale               =  0; #
our $FM_AN_73_TIMER_CFG_h_TimeScale               =  3; #
our $FM_AN_73_TIMER_CFG_l_BreakLinkTimeout        =  4; #
our $FM_AN_73_TIMER_CFG_h_BreakLinkTimeout        =  10; #
our $FM_AN_73_TIMER_CFG_l_LinkFailInibitTimeout   =  11; #
our $FM_AN_73_TIMER_CFG_h_LinkFailInibitTimeout   =  19; #

our $FM_AN_73_STATUS_l_State                      =  6; #
our $FM_AN_73_STATUS_h_State                      =  8; #
our $FM_AN_73_STATUS_b_AckNonceMatch              =  15; #
our $FM_AN_73_STATUS_b_ToggleRx                   =  26; #
our $FM_AN_73_STATUS_b_AnLinkGood                 =  17; #
our $FM_AN_73_STATUS_b_ToggleTx                   =  27; #
our $FM_AN_73_STATUS_b_TransmitAbility            =  28; #
our $FM_AN_73_STATUS_b_ConsistencyMatch           =  21; #
our $FM_AN_73_STATUS_b_TransmitDisable            =  30; #
our $FM_AN_73_STATUS_b_AbilityMatch               =  14; #
our $FM_AN_73_STATUS_b_MrLpAutonegAble            =  23; #
our $FM_AN_73_STATUS_b_AnReceiveIdle              =  18; #
our $FM_AN_73_STATUS_b_BasePage                   =  19; #
our $FM_AN_73_STATUS_b_NonceMatch                 =  24; #
our $FM_AN_73_STATUS_l_RxPageCount                =  0; #
our $FM_AN_73_STATUS_h_RxPageCount                =  5; #
our $FM_AN_73_STATUS_b_NpRx                       =  25; #
our $FM_AN_73_STATUS_b_AcknowledgeMatch           =  16; #
our $FM_AN_73_STATUS_b_CompleteAck                =  20; #
our $FM_AN_73_STATUS_l_TxNonce                    =  9; #
our $FM_AN_73_STATUS_h_TxNonce                    =  13; #
our $FM_AN_73_STATUS_b_MrAutonegComplete          =  22; #
our $FM_AN_73_STATUS_b_TransmitAck                =  29; #

our $FM_AN_73_TX_LCW_DEBUG_l_TxLinkCodeWord       =  0; #
our $FM_AN_73_TX_LCW_DEBUG_h_TxLinkCodeWord       =  47; #

our $FM_AN_73_RX_ABILITY_MATCH_LCW_DEBUG_l_AbilityMatchWord  =  0; #
our $FM_AN_73_RX_ABILITY_MATCH_LCW_DEBUG_h_AbilityMatchWord  =  47; #

our $FM_SERDES_CFG_l_PowerDown                    =  30; #
our $FM_SERDES_CFG_h_PowerDown                    =  31; #
our $FM_SERDES_CFG_b_FarLoopbackEn                =  29; #
our $FM_SERDES_CFG_l_RefSel                       =  6; #
our $FM_SERDES_CFG_h_RefSel                       =  11; #
our $FM_SERDES_CFG_b_KrRestartTraining            =  32; #
our $FM_SERDES_CFG_b_NearLoopbackEn               =  28; #
our $FM_SERDES_CFG_l_CoreToCntl                   =  12; #
our $FM_SERDES_CFG_h_CoreToCntl                   =  27; #
our $FM_SERDES_CFG_b_KrTrainingEn                 =  33; #
our $FM_SERDES_CFG_l_RxFifoWatermark              =  0; #
our $FM_SERDES_CFG_h_RxFifoWatermark              =  5; #

our $FM_DISPARITY_ERROR_8B10B_l_ErrorCnt          =  0; #
our $FM_DISPARITY_ERROR_8B10B_h_ErrorCnt          =  31; #

our $FM_LANE_CFG_b_RxTokLimitTok                  =  10; #
our $FM_LANE_CFG_l_RxTokMaxBubbleRun              =  2; #
our $FM_LANE_CFG_h_RxTokMaxBubbleRun              =  5; #
our $FM_LANE_CFG_l_RxTokMinBubbleRun              =  6; #
our $FM_LANE_CFG_h_RxTokMinBubbleRun              =  9; #
our $FM_LANE_CFG_l_RxTokMaxTokRun                 =  11; #
our $FM_LANE_CFG_h_RxTokMaxTokRun                 =  16; #
our $FM_LANE_CFG_b_DisableAn73FreqCheck           =  25; #
our $FM_LANE_CFG_b_RxTokLimitBubble               =  1; #
our $FM_LANE_CFG_b_RxTokAutomatic                 =  0; #
our $FM_LANE_CFG_b_ResetRx                        =  23; #
our $FM_LANE_CFG_b_ResetTx                        =  24; #
our $FM_LANE_CFG_l_RxTokMinTokRun                 =  17; #
our $FM_LANE_CFG_h_RxTokMinTokRun                 =  22; #

our $FM_LANE_STATUS_b_Pcs40gbaserAmLock           =  4; #
our $FM_LANE_STATUS_b_PcsBasexEnableCgalign       =  3; #
our $FM_LANE_STATUS_l_RxSignalDetectSample        =  13; #
our $FM_LANE_STATUS_h_RxSignalDetectSample        =  16; #
our $FM_LANE_STATUS_b_Pcs40gbaserDeskewLock       =  5; #
our $FM_LANE_STATUS_b_Pcs1000basexSyncStatus      =  0; #
our $FM_LANE_STATUS_b_Pcs10gbasexSyncStatus       =  2; #
our $FM_LANE_STATUS_b_PcsBaserBlockLock           =  6; #
our $FM_LANE_STATUS_b_Pcs10gbasexDeskewLock       =  1; #
our $FM_LANE_STATUS_l_RxRate                      =  7; #
our $FM_LANE_STATUS_h_RxRate                      =  12; #

our $FM_SERDES_RX_CFG_b_RxPolarityInvEn           =  25; #
our $FM_SERDES_RX_CFG_l_RxElecIdleThreshold       =  26; #
our $FM_SERDES_RX_CFG_h_RxElecIdleThreshold       =  29; #
our $FM_SERDES_RX_CFG_b_Rx_8b10bAlignEn           =  10; #
our $FM_SERDES_RX_CFG_l_RxRateSel                 =  1; #
our $FM_SERDES_RX_CFG_h_RxRateSel                 =  7; #
our $FM_SERDES_RX_CFG_b_RxDropoutCharEn           =  13; #
our $FM_SERDES_RX_CFG_b_RxCx4LosMode              =  12; #
our $FM_SERDES_RX_CFG_b_RxSigStrengthStrongEn     =  30; #
our $FM_SERDES_RX_CFG_b_Rx_8b10bDecodeEn          =  11; #
our $FM_SERDES_RX_CFG_l_RxPatternCmpSel           =  23; #
our $FM_SERDES_RX_CFG_h_RxPatternCmpSel           =  24; #
our $FM_SERDES_RX_CFG_b_K28_7CommaDetEn           =  8; #
our $FM_SERDES_RX_CFG_b_RxSigStrengthEn           =  20; #
our $FM_SERDES_RX_CFG_b_RxSelClk20                =  21; #
our $FM_SERDES_RX_CFG_b_RxPatternCmpEn            =  22; #
our $FM_SERDES_RX_CFG_b_RxEn                      =  0; #
our $FM_SERDES_RX_CFG_l_RxActiveSigMin            =  14; #
our $FM_SERDES_RX_CFG_h_RxActiveSigMin            =  19; #
our $FM_SERDES_RX_CFG_b_IgnoreDisparityError      =  31; #
our $FM_SERDES_RX_CFG_b_Rx_20b10bEn               =  9; #

our $FM_SERDES_TX_CFG_b_Tx_8b10bInjectDisparityErr  =  36; #
our $FM_SERDES_TX_CFG_b_Tx_20bF20En               =  33; #
our $FM_SERDES_TX_CFG_b_TxEn                      =  0; #
our $FM_SERDES_TX_CFG_b_TxK30_7ErrEn              =  43; #
our $FM_SERDES_TX_CFG_l_TxRateSel                 =  1; #
our $FM_SERDES_TX_CFG_h_TxRateSel                 =  7; #
our $FM_SERDES_TX_CFG_b_TxPhaseCalEn              =  27; #
our $FM_SERDES_TX_CFG_b_TxOutputEn                =  42; #
our $FM_SERDES_TX_CFG_b_Tx_8b10bEncodeEn          =  35; #
our $FM_SERDES_TX_CFG_b_TxPhaseSlip               =  29; #
our $FM_SERDES_TX_CFG_b_TxPhaseBeacon_20b10bEn    =  26; #
our $FM_SERDES_TX_CFG_l_TxOutputEqPost            =  8; #
our $FM_SERDES_TX_CFG_h_TxOutputEqPost            =  11; #
our $FM_SERDES_TX_CFG_l_TxOutputAmplitude         =  37; #
our $FM_SERDES_TX_CFG_h_TxOutputAmplitude         =  41; #
our $FM_SERDES_TX_CFG_l_TxPatternGenSel           =  21; #
our $FM_SERDES_TX_CFG_h_TxPatternGenSel           =  22; #
our $FM_SERDES_TX_CFG_b_TxPatternGenEn            =  23; #
our $FM_SERDES_TX_CFG_b_TxPolarityInvEn           =  30; #
our $FM_SERDES_TX_CFG_b_Tx_8b10bDisparityReset    =  34; #
our $FM_SERDES_TX_CFG_l_TxPhaseIn                 =  17; #
our $FM_SERDES_TX_CFG_h_TxPhaseIn                 =  20; #
our $FM_SERDES_TX_CFG_b_TxPhaseMaster             =  28; #
our $FM_SERDES_TX_CFG_b_TxDetectRx                =  24; #
our $FM_SERDES_TX_CFG_b_Tx_20b10bEn               =  32; #
our $FM_SERDES_TX_CFG_b_TxSelClk20                =  31; #
our $FM_SERDES_TX_CFG_l_TxOutputEqPre             =  12; #
our $FM_SERDES_TX_CFG_h_TxOutputEqPre             =  14; #
our $FM_SERDES_TX_CFG_b_TxElecIdle                =  25; #
our $FM_SERDES_TX_CFG_l_TxOutputSlew              =  15; #
our $FM_SERDES_TX_CFG_h_TxOutputSlew              =  16; #

our $FM_SERDES_SIGNAL_DETECT_b_MaskRxRdy          =  9; #
our $FM_SERDES_SIGNAL_DETECT_l_MaxFalseCount      =  10; #
our $FM_SERDES_SIGNAL_DETECT_h_MaxFalseCount      =  14; #
our $FM_SERDES_SIGNAL_DETECT_b_MaskKrSignalDetect  =  7; #
our $FM_SERDES_SIGNAL_DETECT_l_RxSigStrengthOrMask  =  0; #
our $FM_SERDES_SIGNAL_DETECT_h_RxSigStrengthOrMask  =  1; #
our $FM_SERDES_SIGNAL_DETECT_l_MaxTrueCount       =  15; #
our $FM_SERDES_SIGNAL_DETECT_h_MaxTrueCount       =  19; #
our $FM_SERDES_SIGNAL_DETECT_b_MaskRxSigStrength  =  4; #
our $FM_SERDES_SIGNAL_DETECT_l_RxSigStrengthMatchPattern  =  2; #
our $FM_SERDES_SIGNAL_DETECT_h_RxSigStrengthMatchPattern  =  3; #
our $FM_SERDES_SIGNAL_DETECT_b_MaskKrRxTrained    =  6; #
our $FM_SERDES_SIGNAL_DETECT_b_MaskKrTrainingFailure  =  8; #
our $FM_SERDES_SIGNAL_DETECT_b_MaskKrRxFault      =  5; #

our $FM_SERDES_STATUS_b_RxElecIdleDetect          =  28; #
our $FM_SERDES_STATUS_b_TxRdy                     =  37; #
our $FM_SERDES_STATUS_b_KrTrainingFailure         =  35; #
our $FM_SERDES_STATUS_b_KrSignalDetect            =  34; #
our $FM_SERDES_STATUS_b_KrRxFault                 =  32; #
our $FM_SERDES_STATUS_b_KrRxTrained               =  33; #
our $FM_SERDES_STATUS_l_Rx_8b10bSlipInProgress    =  22; #
our $FM_SERDES_STATUS_h_Rx_8b10bSlipInProgress    =  23; #
our $FM_SERDES_STATUS_l_Rx_8b10bDisparityErr      =  18; #
our $FM_SERDES_STATUS_h_Rx_8b10bDisparityErr      =  19; #
our $FM_SERDES_STATUS_l_TxPhaseOut                =  39; #
our $FM_SERDES_STATUS_h_TxPhaseOut                =  42; #
our $FM_SERDES_STATUS_b_RxRdy                     =  38; #
our $FM_SERDES_STATUS_b_KrPmdFault                =  31; #
our $FM_SERDES_STATUS_l_RxPatternCmpPass          =  24; #
our $FM_SERDES_STATUS_h_RxPatternCmpPass          =  25; #
our $FM_SERDES_STATUS_l_AnalogToCore              =  0; #
our $FM_SERDES_STATUS_h_AnalogToCore              =  15; #
our $FM_SERDES_STATUS_b_TxDetectRxComplete        =  29; #
our $FM_SERDES_STATUS_l_Rx_8b10bCommaDet          =  16; #
our $FM_SERDES_STATUS_h_Rx_8b10bCommaDet          =  17; #
our $FM_SERDES_STATUS_l_Rx_8b10bOutOfBandErr      =  20; #
our $FM_SERDES_STATUS_h_Rx_8b10bOutOfBandErr      =  21; #
our $FM_SERDES_STATUS_b_KrTxFault                 =  36; #
our $FM_SERDES_STATUS_l_RxSigStrength             =  26; #
our $FM_SERDES_STATUS_h_RxSigStrength             =  27; #
our $FM_SERDES_STATUS_b_TxDetectRxResult          =  30; #

our $FM_SERDES_IM_b_RxElecIdleDetect              =  13; #
our $FM_SERDES_IM_b_TxRdy                         =  6; #
our $FM_SERDES_IM_b_KrRxFault                     =  0; #
our $FM_SERDES_IM_b_KrRxTrained                   =  1; #
our $FM_SERDES_IM_b_KrSignalDetect                =  2; #
our $FM_SERDES_IM_b_KrTrainingFailure             =  11; #
our $FM_SERDES_IM_b_RxRdy                         =  7; #
our $FM_SERDES_IM_b_RxSignalDetect                =  5; #
our $FM_SERDES_IM_b_Rx8b10bCommaDet               =  8; #
our $FM_SERDES_IM_b_RxSignalDetectIn              =  4; #
our $FM_SERDES_IM_b_Rx8b10bOutOfBandErr           =  10; #
our $FM_SERDES_IM_b_TxDetectRxComplete            =  12; #
our $FM_SERDES_IM_b_Rx8b10bDisparityErr           =  9; #
our $FM_SERDES_IM_b_KrTxFault                     =  3; #

our $FM_SERDES_IP_b_RxElecIdleDetect              =  13; #
our $FM_SERDES_IP_b_TxRdy                         =  6; #
our $FM_SERDES_IP_b_KrRxFault                     =  0; #
our $FM_SERDES_IP_b_KrRxTrained                   =  1; #
our $FM_SERDES_IP_b_KrSignalDetect                =  2; #
our $FM_SERDES_IP_b_KrTrainingFailure             =  11; #
our $FM_SERDES_IP_b_RxRdy                         =  7; #
our $FM_SERDES_IP_b_RxSignalDetect                =  5; #
our $FM_SERDES_IP_b_Rx8b10bCommaDet               =  8; #
our $FM_SERDES_IP_b_RxSignalDetectIn              =  4; #
our $FM_SERDES_IP_b_Rx8b10bOutOfBandErr           =  10; #
our $FM_SERDES_IP_b_TxDetectRxComplete            =  12; #
our $FM_SERDES_IP_b_Rx8b10bDisparityErr           =  9; #
our $FM_SERDES_IP_b_KrTxFault                     =  3; #

our $FM_LANE_DEBUG_l_LastBad10bCode               =  0; #
our $FM_LANE_DEBUG_h_LastBad10bCode               =  9; #

our $FM_EPL_IP_l_AnPortInterrupt                  =  0; #
our $FM_EPL_IP_h_AnPortInterrupt                  =  3; #
our $FM_EPL_IP_l_LinkPortInterrupt                =  4; #
our $FM_EPL_IP_h_LinkPortInterrupt                =  7; #
our $FM_EPL_IP_l_SerdesInterrupt                  =  8; #
our $FM_EPL_IP_h_SerdesInterrupt                  =  11; #

our $FM_EPL_CFG_A_b_Active_0                      =  19; #
our $FM_EPL_CFG_A_b_Port0ReverseRxLanes           =  9; #
our $FM_EPL_CFG_A_l_RefClockBSource               =  5; #
our $FM_EPL_CFG_A_h_RefClockBSource               =  7; #
our $FM_EPL_CFG_A_l_SkewTolerance                 =  25; #
our $FM_EPL_CFG_A_h_SkewTolerance                 =  30; #
our $FM_EPL_CFG_A_b_Ieee1588Tick                  =  24; #
our $FM_EPL_CFG_A_l_Timeout1Us                    =  11; #
our $FM_EPL_CFG_A_h_Timeout1Us                    =  18; #
our $FM_EPL_CFG_A_b_Active_3                      =  22; #
our $FM_EPL_CFG_A_b_Active_2                      =  21; #
our $FM_EPL_CFG_A_b_Active_1                      =  20; #
our $FM_EPL_CFG_A_b_Port0ReverseTxLanes           =  8; #
our $FM_EPL_CFG_A_b_RefClockBDiv                  =  4; #
our $FM_EPL_CFG_A_b_MapXauiToCh1                  =  23; #
our $FM_EPL_CFG_A_l_RefClockASource               =  1; #
our $FM_EPL_CFG_A_h_RefClockASource               =  3; #
our $FM_EPL_CFG_A_b_SpeedUp                       =  10; #
our $FM_EPL_CFG_A_b_RefClockADiv                  =  0; #

our $FM_EPL_CFG_B_l_Port2PcsSel                   =  8; #
our $FM_EPL_CFG_B_h_Port2PcsSel                   =  11; #
our $FM_EPL_CFG_B_b_MlTokAuto                     =  19; #
our $FM_EPL_CFG_B_l_Port1PcsSel                   =  4; #
our $FM_EPL_CFG_B_h_Port1PcsSel                   =  7; #
our $FM_EPL_CFG_B_b_MlTokMinRunEnable             =  20; #
our $FM_EPL_CFG_B_l_QplMode                       =  16; #
our $FM_EPL_CFG_B_h_QplMode                       =  18; #
our $FM_EPL_CFG_B_l_Port3PcsSel                   =  12; #
our $FM_EPL_CFG_B_h_Port3PcsSel                   =  15; #
our $FM_EPL_CFG_B_l_MlTokMinRun                   =  21; #
our $FM_EPL_CFG_B_h_MlTokMinRun                   =  23; #
our $FM_EPL_CFG_B_l_Port0PcsSel                   =  0; #
our $FM_EPL_CFG_B_h_Port0PcsSel                   =  3; #

our $FM_EPL_LED_STATUS_b_Port2Reset               =  12; #
our $FM_EPL_LED_STATUS_b_Port3LocalFault          =  20; #
our $FM_EPL_LED_STATUS_b_Port0RemoteFault         =  3; #
our $FM_EPL_LED_STATUS_b_Port3Reset               =  18; #
our $FM_EPL_LED_STATUS_b_Port2LocalFault          =  14; #
our $FM_EPL_LED_STATUS_b_Port2Receiving           =  17; #
our $FM_EPL_LED_STATUS_b_Port0Receiving           =  5; #
our $FM_EPL_LED_STATUS_b_Port1Transmitting        =  10; #
our $FM_EPL_LED_STATUS_b_Port0Transmitting        =  4; #
our $FM_EPL_LED_STATUS_b_Port1LinkUp              =  7; #
our $FM_EPL_LED_STATUS_b_Port3Receiving           =  23; #
our $FM_EPL_LED_STATUS_b_Port1RemoteFault         =  9; #
our $FM_EPL_LED_STATUS_b_Port0LocalFault          =  2; #
our $FM_EPL_LED_STATUS_b_Port1Receiving           =  11; #
our $FM_EPL_LED_STATUS_b_Port0LinkUp              =  1; #
our $FM_EPL_LED_STATUS_b_Port2RemoteFault         =  15; #
our $FM_EPL_LED_STATUS_b_Port3Transmitting        =  22; #
our $FM_EPL_LED_STATUS_b_Port3LinkUp              =  19; #
our $FM_EPL_LED_STATUS_b_Port1LocalFault          =  8; #
our $FM_EPL_LED_STATUS_b_Port3RemoteFault         =  21; #
our $FM_EPL_LED_STATUS_b_Port0Reset               =  0; #
our $FM_EPL_LED_STATUS_b_Port2Transmitting        =  16; #
our $FM_EPL_LED_STATUS_b_Port2LinkUp              =  13; #
our $FM_EPL_LED_STATUS_b_Port1Reset               =  6; #

our $FM_EPL_FIFO_ERROR_STATUS_b_TxFifoErrorA      =  0; #
our $FM_EPL_FIFO_ERROR_STATUS_b_TxFifoErrorB      =  1; #
our $FM_EPL_FIFO_ERROR_STATUS_b_TxFifoErrorC      =  2; #
our $FM_EPL_FIFO_ERROR_STATUS_b_TxFifoErrorD      =  3; #
our $FM_EPL_FIFO_ERROR_STATUS_b_RxFifoErrorA      =  4; #
our $FM_EPL_FIFO_ERROR_STATUS_b_RxFifoErrorB      =  5; #
our $FM_EPL_FIFO_ERROR_STATUS_b_RxFifoErrorC      =  6; #
our $FM_EPL_FIFO_ERROR_STATUS_b_RxFifoErrorD      =  7; #

our $FM_EPL_TX_FIFO_READ_PTR_STATUS_l_ReadPtrD    =  12; #
our $FM_EPL_TX_FIFO_READ_PTR_STATUS_h_ReadPtrD    =  15; #
our $FM_EPL_TX_FIFO_READ_PTR_STATUS_l_ReadPtrB    =  4; #
our $FM_EPL_TX_FIFO_READ_PTR_STATUS_h_ReadPtrB    =  7; #
our $FM_EPL_TX_FIFO_READ_PTR_STATUS_l_ReadPtrC    =  8; #
our $FM_EPL_TX_FIFO_READ_PTR_STATUS_h_ReadPtrC    =  11; #
our $FM_EPL_TX_FIFO_READ_PTR_STATUS_l_ReadPtrA    =  0; #
our $FM_EPL_TX_FIFO_READ_PTR_STATUS_h_ReadPtrA    =  3; #

our $FM_EPL_TX_FIFO_WRITE_PTR_STATUS_l_WritePtrD  =  12; #
our $FM_EPL_TX_FIFO_WRITE_PTR_STATUS_h_WritePtrD  =  15; #
our $FM_EPL_TX_FIFO_WRITE_PTR_STATUS_l_WritePtrC  =  8; #
our $FM_EPL_TX_FIFO_WRITE_PTR_STATUS_h_WritePtrC  =  11; #
our $FM_EPL_TX_FIFO_WRITE_PTR_STATUS_l_WritePtrB  =  4; #
our $FM_EPL_TX_FIFO_WRITE_PTR_STATUS_h_WritePtrB  =  7; #
our $FM_EPL_TX_FIFO_WRITE_PTR_STATUS_l_WritePtrA  =  0; #
our $FM_EPL_TX_FIFO_WRITE_PTR_STATUS_h_WritePtrA  =  3; #

our $FM_EPL_TX_FIFO_A_PTR_STATUS_l_WritePtrA      =  4; #
our $FM_EPL_TX_FIFO_A_PTR_STATUS_h_WritePtrA      =  7; #
our $FM_EPL_TX_FIFO_A_PTR_STATUS_l_ReadPtrA       =  0; #
our $FM_EPL_TX_FIFO_A_PTR_STATUS_h_ReadPtrA       =  3; #

our $FM_EPL_TX_FIFO_B_PTR_STATUS_l_WritePtrB      =  4; #
our $FM_EPL_TX_FIFO_B_PTR_STATUS_h_WritePtrB      =  7; #
our $FM_EPL_TX_FIFO_B_PTR_STATUS_l_ReadPtrB       =  0; #
our $FM_EPL_TX_FIFO_B_PTR_STATUS_h_ReadPtrB       =  3; #

our $FM_EPL_TX_FIFO_C_PTR_STATUS_l_WritePtrC      =  4; #
our $FM_EPL_TX_FIFO_C_PTR_STATUS_h_WritePtrC      =  7; #
our $FM_EPL_TX_FIFO_C_PTR_STATUS_l_ReadPtrC       =  0; #
our $FM_EPL_TX_FIFO_C_PTR_STATUS_h_ReadPtrC       =  3; #

our $FM_EPL_TX_FIFO_D_PTR_STATUS_l_WritePtrD      =  4; #
our $FM_EPL_TX_FIFO_D_PTR_STATUS_h_WritePtrD      =  7; #
our $FM_EPL_TX_FIFO_D_PTR_STATUS_l_ReadPtrD       =  0; #
our $FM_EPL_TX_FIFO_D_PTR_STATUS_h_ReadPtrD       =  3; #

our $FM_EPL_RX_FIFO_READ_PTR_STATUS_l_ReadPtrD    =  18; #
our $FM_EPL_RX_FIFO_READ_PTR_STATUS_h_ReadPtrD    =  23; #
our $FM_EPL_RX_FIFO_READ_PTR_STATUS_l_ReadPtrB    =  6; #
our $FM_EPL_RX_FIFO_READ_PTR_STATUS_h_ReadPtrB    =  11; #
our $FM_EPL_RX_FIFO_READ_PTR_STATUS_l_ReadPtrC    =  12; #
our $FM_EPL_RX_FIFO_READ_PTR_STATUS_h_ReadPtrC    =  17; #
our $FM_EPL_RX_FIFO_READ_PTR_STATUS_l_ReadPtrA    =  0; #
our $FM_EPL_RX_FIFO_READ_PTR_STATUS_h_ReadPtrA    =  5; #

our $FM_EPL_RX_FIFO_WRITE_PTR_STATUS_l_WritePtrD  =  18; #
our $FM_EPL_RX_FIFO_WRITE_PTR_STATUS_h_WritePtrD  =  23; #
our $FM_EPL_RX_FIFO_WRITE_PTR_STATUS_l_WritePtrC  =  12; #
our $FM_EPL_RX_FIFO_WRITE_PTR_STATUS_h_WritePtrC  =  17; #
our $FM_EPL_RX_FIFO_WRITE_PTR_STATUS_l_WritePtrB  =  6; #
our $FM_EPL_RX_FIFO_WRITE_PTR_STATUS_h_WritePtrB  =  11; #
our $FM_EPL_RX_FIFO_WRITE_PTR_STATUS_l_WritePtrA  =  0; #
our $FM_EPL_RX_FIFO_WRITE_PTR_STATUS_h_WritePtrA  =  5; #

our $FM_EPL_RX_FIFO_A_PTR_STATUS_l_WritePtrA      =  6; #
our $FM_EPL_RX_FIFO_A_PTR_STATUS_h_WritePtrA      =  11; #
our $FM_EPL_RX_FIFO_A_PTR_STATUS_l_ReadPtrA       =  0; #
our $FM_EPL_RX_FIFO_A_PTR_STATUS_h_ReadPtrA       =  5; #

our $FM_EPL_RX_FIFO_B_PTR_STATUS_l_WritePtrB      =  6; #
our $FM_EPL_RX_FIFO_B_PTR_STATUS_h_WritePtrB      =  11; #
our $FM_EPL_RX_FIFO_B_PTR_STATUS_l_ReadPtrB       =  0; #
our $FM_EPL_RX_FIFO_B_PTR_STATUS_h_ReadPtrB       =  5; #

our $FM_EPL_RX_FIFO_C_PTR_STATUS_l_WritePtrC      =  6; #
our $FM_EPL_RX_FIFO_C_PTR_STATUS_h_WritePtrC      =  11; #
our $FM_EPL_RX_FIFO_C_PTR_STATUS_l_ReadPtrC       =  0; #
our $FM_EPL_RX_FIFO_C_PTR_STATUS_h_ReadPtrC       =  5; #

our $FM_EPL_RX_FIFO_D_PTR_STATUS_l_WritePtrD      =  6; #
our $FM_EPL_RX_FIFO_D_PTR_STATUS_h_WritePtrD      =  11; #
our $FM_EPL_RX_FIFO_D_PTR_STATUS_l_ReadPtrD       =  0; #
our $FM_EPL_RX_FIFO_D_PTR_STATUS_h_ReadPtrD       =  5; #

our $FM_PCS_10GBASEX_CFG_b_LsMaskSeenK            =  2; #
our $FM_PCS_10GBASEX_CFG_b_TxPrbsSel              =  0; #
our $FM_PCS_10GBASEX_CFG_b_LsMaskSeenR            =  3; #
our $FM_PCS_10GBASEX_CFG_b_LsMaskSeenA            =  1; #
our $FM_PCS_10GBASEX_CFG_b_RxDisableCheckEnd      =  4; #

our $FM_PCS_10GBASEX_RX_STATUS_b_SyncStatus       =  1; #
our $FM_PCS_10GBASEX_RX_STATUS_b_DeskewError      =  6; #
our $FM_PCS_10GBASEX_RX_STATUS_l_LnSyncStatus     =  2; #
our $FM_PCS_10GBASEX_RX_STATUS_h_LnSyncStatus     =  5; #
our $FM_PCS_10GBASEX_RX_STATUS_b_AlignStatus      =  0; #

our $FM_PCS_10GBASEX_TX_STATUS_b_DebugNextIdle    =  12; #
our $FM_PCS_10GBASEX_TX_STATUS_l_DebugACnt        =  7; #
our $FM_PCS_10GBASEX_TX_STATUS_h_DebugACnt        =  11; #
our $FM_PCS_10GBASEX_TX_STATUS_b_DebugQDet        =  13; #
our $FM_PCS_10GBASEX_TX_STATUS_l_DebugState       =  0; #
our $FM_PCS_10GBASEX_TX_STATUS_h_DebugState       =  2; #
our $FM_PCS_10GBASEX_TX_STATUS_l_DebugLaneDisparity  =  3; #
our $FM_PCS_10GBASEX_TX_STATUS_h_DebugLaneDisparity  =  6; #

our $FM_PCS_40GBASER_CFG_b_ScramblerBypass        =  25; #
our $FM_PCS_40GBASER_CFG_b_ManualLaneSel          =  24; #
our $FM_PCS_40GBASER_CFG_l_Lane2Sel               =  20; #
our $FM_PCS_40GBASER_CFG_h_Lane2Sel               =  21; #
our $FM_PCS_40GBASER_CFG_b_DescramblerBypass      =  26; #
our $FM_PCS_40GBASER_CFG_b_LsMaskHiBer            =  28; #
our $FM_PCS_40GBASER_CFG_b_LsMaskSeenI            =  27; #
our $FM_PCS_40GBASER_CFG_l_Lane1Sel               =  18; #
our $FM_PCS_40GBASER_CFG_h_Lane1Sel               =  19; #
our $FM_PCS_40GBASER_CFG_l_Lane0Sel               =  16; #
our $FM_PCS_40GBASER_CFG_h_Lane0Sel               =  17; #
our $FM_PCS_40GBASER_CFG_l_Lane3Sel               =  22; #
our $FM_PCS_40GBASER_CFG_h_Lane3Sel               =  23; #
our $FM_PCS_40GBASER_CFG_l_AmTimeout              =  0; #
our $FM_PCS_40GBASER_CFG_h_AmTimeout              =  15; #
our $FM_PCS_40GBASER_CFG_b_Kr2AmEncodingSelect    =  29; #

our $FM_PCS_40GBASER_RX_STATUS_l_Lane2Sel         =  8; #
our $FM_PCS_40GBASER_RX_STATUS_h_Lane2Sel         =  11; #
our $FM_PCS_40GBASER_RX_STATUS_l_Lane1Sel         =  4; #
our $FM_PCS_40GBASER_RX_STATUS_h_Lane1Sel         =  7; #
our $FM_PCS_40GBASER_RX_STATUS_b_BlockLockLaneD   =  19; #
our $FM_PCS_40GBASER_RX_STATUS_b_HiBer            =  28; #
our $FM_PCS_40GBASER_RX_STATUS_l_Lane0Sel         =  0; #
our $FM_PCS_40GBASER_RX_STATUS_h_Lane0Sel         =  3; #
our $FM_PCS_40GBASER_RX_STATUS_l_Lane3Sel         =  12; #
our $FM_PCS_40GBASER_RX_STATUS_h_Lane3Sel         =  15; #
our $FM_PCS_40GBASER_RX_STATUS_b_BlockLockLaneA   =  16; #
our $FM_PCS_40GBASER_RX_STATUS_b_AlignStatus      =  29; #
our $FM_PCS_40GBASER_RX_STATUS_b_BlockLockLaneC   =  18; #
our $FM_PCS_40GBASER_RX_STATUS_l_BerCnt           =  20; #
our $FM_PCS_40GBASER_RX_STATUS_h_BerCnt           =  27; #
our $FM_PCS_40GBASER_RX_STATUS_b_BlockLockLaneB   =  17; #

our $FM_EPL_1588_TIMER_STATUS_l_Timer             =  0; #
our $FM_EPL_1588_TIMER_STATUS_h_Timer             =  30; #

our $FM_PARSER_CAM_l_Key                          =  64; #
our $FM_PARSER_CAM_h_Key                          =  127; #
our $FM_PARSER_CAM_l_KeyInvert                    =  0; #
our $FM_PARSER_CAM_h_KeyInvert                    =  63; #

our $FM_PARSER_RAM_b_Terminate                    =  105; #
our $FM_PARSER_RAM_b_Byte3Enable                  =  57; #
our $FM_PARSER_RAM_b_Halfword0Add                 =  58; #
our $FM_PARSER_RAM_b_Byte0Enable                  =  54; #
our $FM_PARSER_RAM_l_ShiftNextSlice               =  106; #
our $FM_PARSER_RAM_h_ShiftNextSlice               =  108; #
our $FM_PARSER_RAM_l_StateFrameRot                =  100; #
our $FM_PARSER_RAM_h_StateFrameRot                =  101; #
our $FM_PARSER_RAM_b_Byte1Enable                  =  55; #
our $FM_PARSER_RAM_l_Halfword1Rot                 =  52; #
our $FM_PARSER_RAM_h_Halfword1Rot                 =  53; #
our $FM_PARSER_RAM_l_StateValue1                  =  72; #
our $FM_PARSER_RAM_h_StateValue1                  =  79; #
our $FM_PARSER_RAM_l_StateValue0                  =  62; #
our $FM_PARSER_RAM_h_StateValue0                  =  69; #
our $FM_PARSER_RAM_l_StateOp0                     =  60; #
our $FM_PARSER_RAM_h_StateOp0                     =  61; #
our $FM_PARSER_RAM_l_StateValue3                  =  92; #
our $FM_PARSER_RAM_h_StateValue3                  =  99; #
our $FM_PARSER_RAM_l_StateOp1                     =  70; #
our $FM_PARSER_RAM_h_StateOp1                     =  71; #
our $FM_PARSER_RAM_l_StateValue2                  =  82; #
our $FM_PARSER_RAM_h_StateValue2                  =  89; #
our $FM_PARSER_RAM_l_Halfword1Dest                =  44; #
our $FM_PARSER_RAM_h_Halfword1Dest                =  49; #
our $FM_PARSER_RAM_l_StateOp3                     =  90; #
our $FM_PARSER_RAM_h_StateOp3                     =  91; #
our $FM_PARSER_RAM_l_StateOp2                     =  80; #
our $FM_PARSER_RAM_h_StateOp2                     =  81; #
our $FM_PARSER_RAM_b_TerminateAllowed             =  104; #
our $FM_PARSER_RAM_l_LegalPadding                 =  102; #
our $FM_PARSER_RAM_h_LegalPadding                 =  103; #
our $FM_PARSER_RAM_b_rsvd                         =  109; #
our $FM_PARSER_RAM_b_Byte2Enable                  =  56; #
our $FM_PARSER_RAM_l_Halfword0Rot                 =  50; #
our $FM_PARSER_RAM_h_Halfword0Rot                 =  51; #
our $FM_PARSER_RAM_b_Halfword1Add                 =  59; #
our $FM_PARSER_RAM_l_SetFlags                     =  0; #
our $FM_PARSER_RAM_h_SetFlags                     =  37; #
our $FM_PARSER_RAM_l_Halfword0Dest                =  38; #
our $FM_PARSER_RAM_h_Halfword0Dest                =  43; #

our $FM_PARSER_INIT_STATE_l_State1                =  8; #
our $FM_PARSER_INIT_STATE_h_State1                =  15; #
our $FM_PARSER_INIT_STATE_l_State2                =  16; #
our $FM_PARSER_INIT_STATE_h_State2                =  23; #
our $FM_PARSER_INIT_STATE_b_Fields1Valid          =  33; #
our $FM_PARSER_INIT_STATE_l_State3                =  24; #
our $FM_PARSER_INIT_STATE_h_State3                =  31; #
our $FM_PARSER_INIT_STATE_l_State0                =  0; #
our $FM_PARSER_INIT_STATE_h_State0                =  7; #
our $FM_PARSER_INIT_STATE_b_rsvd                  =  32; #

our $FM_PARSER_INIT_FIELDS_l_Data                 =  0; #
our $FM_PARSER_INIT_FIELDS_h_Data                 =  63; #

our $FM_CM_RXMP_MAP_l_RXMP_9                      =  36; #
our $FM_CM_RXMP_MAP_h_RXMP_9                      =  39; #
our $FM_CM_RXMP_MAP_l_RXMP_8                      =  32; #
our $FM_CM_RXMP_MAP_h_RXMP_8                      =  35; #
our $FM_CM_RXMP_MAP_l_RXMP_7                      =  28; #
our $FM_CM_RXMP_MAP_h_RXMP_7                      =  31; #
our $FM_CM_RXMP_MAP_l_RXMP_6                      =  24; #
our $FM_CM_RXMP_MAP_h_RXMP_6                      =  27; #
our $FM_CM_RXMP_MAP_l_RXMP_14                     =  56; #
our $FM_CM_RXMP_MAP_h_RXMP_14                     =  59; #
our $FM_CM_RXMP_MAP_l_RXMP_13                     =  52; #
our $FM_CM_RXMP_MAP_h_RXMP_13                     =  55; #
our $FM_CM_RXMP_MAP_l_RXMP_15                     =  60; #
our $FM_CM_RXMP_MAP_h_RXMP_15                     =  63; #
our $FM_CM_RXMP_MAP_l_RXMP_10                     =  40; #
our $FM_CM_RXMP_MAP_h_RXMP_10                     =  43; #
our $FM_CM_RXMP_MAP_l_RXMP_12                     =  48; #
our $FM_CM_RXMP_MAP_h_RXMP_12                     =  51; #
our $FM_CM_RXMP_MAP_l_RXMP_11                     =  44; #
our $FM_CM_RXMP_MAP_h_RXMP_11                     =  47; #
our $FM_CM_RXMP_MAP_l_RXMP_0                      =  0; #
our $FM_CM_RXMP_MAP_h_RXMP_0                      =  3; #
our $FM_CM_RXMP_MAP_l_RXMP_1                      =  4; #
our $FM_CM_RXMP_MAP_h_RXMP_1                      =  7; #
our $FM_CM_RXMP_MAP_l_RXMP_2                      =  8; #
our $FM_CM_RXMP_MAP_h_RXMP_2                      =  11; #
our $FM_CM_RXMP_MAP_l_RXMP_3                      =  12; #
our $FM_CM_RXMP_MAP_h_RXMP_3                      =  15; #
our $FM_CM_RXMP_MAP_l_RXMP_4                      =  16; #
our $FM_CM_RXMP_MAP_h_RXMP_4                      =  19; #
our $FM_CM_RXMP_MAP_l_RXMP_5                      =  20; #
our $FM_CM_RXMP_MAP_h_RXMP_5                      =  23; #

our $FM_CM_TXMP_MAP_l_TXMP_15                     =  60; #
our $FM_CM_TXMP_MAP_h_TXMP_15                     =  63; #
our $FM_CM_TXMP_MAP_l_TXMP_14                     =  56; #
our $FM_CM_TXMP_MAP_h_TXMP_14                     =  59; #
our $FM_CM_TXMP_MAP_l_TXMP_13                     =  52; #
our $FM_CM_TXMP_MAP_h_TXMP_13                     =  55; #
our $FM_CM_TXMP_MAP_l_TXMP_9                      =  36; #
our $FM_CM_TXMP_MAP_h_TXMP_9                      =  39; #
our $FM_CM_TXMP_MAP_l_TXMP_12                     =  48; #
our $FM_CM_TXMP_MAP_h_TXMP_12                     =  51; #
our $FM_CM_TXMP_MAP_l_TXMP_8                      =  32; #
our $FM_CM_TXMP_MAP_h_TXMP_8                      =  35; #
our $FM_CM_TXMP_MAP_l_TXMP_11                     =  44; #
our $FM_CM_TXMP_MAP_h_TXMP_11                     =  47; #
our $FM_CM_TXMP_MAP_l_TXMP_10                     =  40; #
our $FM_CM_TXMP_MAP_h_TXMP_10                     =  43; #
our $FM_CM_TXMP_MAP_l_TXMP_4                      =  16; #
our $FM_CM_TXMP_MAP_h_TXMP_4                      =  19; #
our $FM_CM_TXMP_MAP_l_TXMP_5                      =  20; #
our $FM_CM_TXMP_MAP_h_TXMP_5                      =  23; #
our $FM_CM_TXMP_MAP_l_TXMP_6                      =  24; #
our $FM_CM_TXMP_MAP_h_TXMP_6                      =  27; #
our $FM_CM_TXMP_MAP_l_TXMP_7                      =  28; #
our $FM_CM_TXMP_MAP_h_TXMP_7                      =  31; #
our $FM_CM_TXMP_MAP_l_TXMP_0                      =  0; #
our $FM_CM_TXMP_MAP_h_TXMP_0                      =  3; #
our $FM_CM_TXMP_MAP_l_TXMP_1                      =  4; #
our $FM_CM_TXMP_MAP_h_TXMP_1                      =  7; #
our $FM_CM_TXMP_MAP_l_TXMP_2                      =  8; #
our $FM_CM_TXMP_MAP_h_TXMP_2                      =  11; #
our $FM_CM_TXMP_MAP_l_TXMP_3                      =  12; #
our $FM_CM_TXMP_MAP_h_TXMP_3                      =  15; #

our $FM_CM_TC_MAP_l_TC_2                          =  8; #
our $FM_CM_TC_MAP_h_TC_2                          =  11; #
our $FM_CM_TC_MAP_l_TC_3                          =  12; #
our $FM_CM_TC_MAP_h_TC_3                          =  15; #
our $FM_CM_TC_MAP_l_TC_0                          =  0; #
our $FM_CM_TC_MAP_h_TC_0                          =  3; #
our $FM_CM_TC_MAP_l_TC_1                          =  4; #
our $FM_CM_TC_MAP_h_TC_1                          =  7; #
our $FM_CM_TC_MAP_l_TC_6                          =  24; #
our $FM_CM_TC_MAP_h_TC_6                          =  27; #
our $FM_CM_TC_MAP_l_TC_7                          =  28; #
our $FM_CM_TC_MAP_h_TC_7                          =  31; #
our $FM_CM_TC_MAP_l_TC_4                          =  16; #
our $FM_CM_TC_MAP_h_TC_4                          =  19; #
our $FM_CM_TC_MAP_l_TC_5                          =  20; #
our $FM_CM_TC_MAP_h_TC_5                          =  23; #
our $FM_CM_TC_MAP_l_TC_8                          =  32; #
our $FM_CM_TC_MAP_h_TC_8                          =  35; #
our $FM_CM_TC_MAP_l_TC_9                          =  36; #
our $FM_CM_TC_MAP_h_TC_9                          =  39; #
our $FM_CM_TC_MAP_l_TC_10                         =  40; #
our $FM_CM_TC_MAP_h_TC_10                         =  43; #
our $FM_CM_TC_MAP_l_TC_12                         =  48; #
our $FM_CM_TC_MAP_h_TC_12                         =  51; #
our $FM_CM_TC_MAP_l_TC_11                         =  44; #
our $FM_CM_TC_MAP_h_TC_11                         =  47; #
our $FM_CM_TC_MAP_l_TC_14                         =  56; #
our $FM_CM_TC_MAP_h_TC_14                         =  59; #
our $FM_CM_TC_MAP_l_TC_13                         =  52; #
our $FM_CM_TC_MAP_h_TC_13                         =  55; #
our $FM_CM_TC_MAP_l_TC_15                         =  60; #
our $FM_CM_TC_MAP_h_TC_15                         =  63; #

our $FM_CM_BSG_MAP_l_BSG_0                        =  0; #
our $FM_CM_BSG_MAP_h_BSG_0                        =  3; #
our $FM_CM_BSG_MAP_l_BSG_11                       =  44; #
our $FM_CM_BSG_MAP_h_BSG_11                       =  47; #
our $FM_CM_BSG_MAP_l_BSG_10                       =  40; #
our $FM_CM_BSG_MAP_h_BSG_10                       =  43; #
our $FM_CM_BSG_MAP_l_BSG_9                        =  36; #
our $FM_CM_BSG_MAP_h_BSG_9                        =  39; #
our $FM_CM_BSG_MAP_l_BSG_7                        =  28; #
our $FM_CM_BSG_MAP_h_BSG_7                        =  31; #
our $FM_CM_BSG_MAP_l_BSG_8                        =  32; #
our $FM_CM_BSG_MAP_h_BSG_8                        =  35; #
our $FM_CM_BSG_MAP_l_BSG_5                        =  20; #
our $FM_CM_BSG_MAP_h_BSG_5                        =  23; #
our $FM_CM_BSG_MAP_l_BSG_6                        =  24; #
our $FM_CM_BSG_MAP_h_BSG_6                        =  27; #
our $FM_CM_BSG_MAP_l_BSG_3                        =  12; #
our $FM_CM_BSG_MAP_h_BSG_3                        =  15; #
our $FM_CM_BSG_MAP_l_BSG_4                        =  16; #
our $FM_CM_BSG_MAP_h_BSG_4                        =  19; #
our $FM_CM_BSG_MAP_l_BSG_1                        =  4; #
our $FM_CM_BSG_MAP_h_BSG_1                        =  7; #
our $FM_CM_BSG_MAP_l_BSG_2                        =  8; #
our $FM_CM_BSG_MAP_h_BSG_2                        =  11; #

our $FM_CM_GLOBAL_USAGE_l_SegmentCount            =  0; #
our $FM_CM_GLOBAL_USAGE_h_SegmentCount            =  16; #
our $FM_CM_GLOBAL_USAGE_l_FrameCount              =  17; #
our $FM_CM_GLOBAL_USAGE_h_FrameCount              =  33; #

our $FM_CM_RXMP_USAGE_l_SegmentCount              =  0; #
our $FM_CM_RXMP_USAGE_h_SegmentCount              =  15; #

our $FM_CM_SHARED_RXMP_USAGE_l_SegmentCount       =  0; #
our $FM_CM_SHARED_RXMP_USAGE_h_SegmentCount       =  16; #
our $FM_CM_SHARED_RXMP_USAGE_l_FrameCount         =  17; #
our $FM_CM_SHARED_RXMP_USAGE_h_FrameCount         =  33; #

our $FM_CM_PORT_RXMP_USAGE_l_SegmentCount         =  0; #
our $FM_CM_PORT_RXMP_USAGE_h_SegmentCount         =  16; #
our $FM_CM_PORT_RXMP_USAGE_l_FrameCount           =  18; #
our $FM_CM_PORT_RXMP_USAGE_h_FrameCount           =  34; #
our $FM_CM_PORT_RXMP_USAGE_b_RSVD                 =  17; #

our $FM_CM_GLOBAL_TXMP_USAGE_l_SegmentCount       =  0; #
our $FM_CM_GLOBAL_TXMP_USAGE_h_SegmentCount       =  15; #

our $FM_CM_PORT_TX_DROP_COUNT_l_FrameCount        =  0; #
our $FM_CM_PORT_TX_DROP_COUNT_h_FrameCount        =  63; #

our $FM_CM_GLOBAL_WM_l_FrameLimit                 =  16; #
our $FM_CM_GLOBAL_WM_h_FrameLimit                 =  31; #
our $FM_CM_GLOBAL_WM_l_SegmentLimit               =  0; #
our $FM_CM_GLOBAL_WM_h_SegmentLimit               =  15; #

our $FM_CM_SHARED_RXMP_WM_l_FrameLimit            =  16; #
our $FM_CM_SHARED_RXMP_WM_h_FrameLimit            =  31; #
our $FM_CM_SHARED_RXMP_WM_l_SegmentLimit          =  0; #
our $FM_CM_SHARED_RXMP_WM_h_SegmentLimit          =  15; #

our $FM_CM_PORT_RXMP_PRIVATE_WM_l_FrameLimit      =  16; #
our $FM_CM_PORT_RXMP_PRIVATE_WM_h_FrameLimit      =  31; #
our $FM_CM_PORT_RXMP_PRIVATE_WM_l_SegmentLimit    =  0; #
our $FM_CM_PORT_RXMP_PRIVATE_WM_h_SegmentLimit    =  15; #

our $FM_CM_PORT_RXMP_HOG_WM_l_FrameLimit          =  16; #
our $FM_CM_PORT_RXMP_HOG_WM_h_FrameLimit          =  31; #
our $FM_CM_PORT_RXMP_HOG_WM_l_SegmentLimit        =  0; #
our $FM_CM_PORT_RXMP_HOG_WM_h_SegmentLimit        =  15; #

our $FM_CM_PORT_TXMP_PRIVATE_WM_l_PrivateSegmentLimit  =  0; #
our $FM_CM_PORT_TXMP_PRIVATE_WM_h_PrivateSegmentLimit  =  13; #
our $FM_CM_PORT_TXMP_PRIVATE_WM_b_SoftDropOnRxmpFree  =  15; #
our $FM_CM_PORT_TXMP_PRIVATE_WM_b_SoftDropOnPrivate  =  14; #

our $FM_CM_PORT_TXMP_HOG_WM_l_HogSegmentLimit     =  0; #
our $FM_CM_PORT_TXMP_HOG_WM_h_HogSegmentLimit     =  13; #

our $FM_CM_RXMP_SOFT_DROP_WM_l_SoftDropSegmentLimit  =  0; #
our $FM_CM_RXMP_SOFT_DROP_WM_h_SoftDropSegmentLimit  =  13; #
our $FM_CM_RXMP_SOFT_DROP_WM_l_SoftDropSegmentLimitJitterBits  =  14; #
our $FM_CM_RXMP_SOFT_DROP_WM_h_SoftDropSegmentLimitJitterBits  =  16; #
our $FM_CM_RXMP_SOFT_DROP_WM_l_HogSegmentLimit    =  17; #
our $FM_CM_RXMP_SOFT_DROP_WM_h_HogSegmentLimit    =  30; #

our $FM_CM_PORT_RXMP_PAUSE_ON_WM_l_FrameLimit     =  16; #
our $FM_CM_PORT_RXMP_PAUSE_ON_WM_h_FrameLimit     =  31; #
our $FM_CM_PORT_RXMP_PAUSE_ON_WM_l_SegmentLimit   =  0; #
our $FM_CM_PORT_RXMP_PAUSE_ON_WM_h_SegmentLimit   =  15; #

our $FM_CM_PORT_RXMP_PAUSE_OFF_WM_l_FrameLimit    =  16; #
our $FM_CM_PORT_RXMP_PAUSE_OFF_WM_h_FrameLimit    =  31; #
our $FM_CM_PORT_RXMP_PAUSE_OFF_WM_l_SegmentLimit  =  0; #
our $FM_CM_PORT_RXMP_PAUSE_OFF_WM_h_SegmentLimit  =  15; #

our $FM_CM_SHARED_RXMP_PAUSE_ON_WM_l_FrameLimit   =  16; #
our $FM_CM_SHARED_RXMP_PAUSE_ON_WM_h_FrameLimit   =  31; #
our $FM_CM_SHARED_RXMP_PAUSE_ON_WM_l_SegmentLimit  =  0; #
our $FM_CM_SHARED_RXMP_PAUSE_ON_WM_h_SegmentLimit  =  15; #

our $FM_CM_SHARED_RXMP_PAUSE_OFF_WM_l_FrameLimit  =  16; #
our $FM_CM_SHARED_RXMP_PAUSE_OFF_WM_h_FrameLimit  =  31; #
our $FM_CM_SHARED_RXMP_PAUSE_OFF_WM_l_SegmentLimit  =  0; #
our $FM_CM_SHARED_RXMP_PAUSE_OFF_WM_h_SegmentLimit  =  15; #

our $FM_CM_TC_PC_MAP_l_PauseClass_0               =  0; #
our $FM_CM_TC_PC_MAP_h_PauseClass_0               =  2; #
our $FM_CM_TC_PC_MAP_l_PauseClass_4               =  12; #
our $FM_CM_TC_PC_MAP_h_PauseClass_4               =  14; #
our $FM_CM_TC_PC_MAP_l_PauseClass_3               =  9; #
our $FM_CM_TC_PC_MAP_h_PauseClass_3               =  11; #
our $FM_CM_TC_PC_MAP_l_PauseClass_11              =  33; #
our $FM_CM_TC_PC_MAP_h_PauseClass_11              =  35; #
our $FM_CM_TC_PC_MAP_l_PauseClass_2               =  6; #
our $FM_CM_TC_PC_MAP_h_PauseClass_2               =  8; #
our $FM_CM_TC_PC_MAP_l_PauseClass_10              =  30; #
our $FM_CM_TC_PC_MAP_h_PauseClass_10              =  32; #
our $FM_CM_TC_PC_MAP_l_PauseClass_1               =  3; #
our $FM_CM_TC_PC_MAP_h_PauseClass_1               =  5; #
our $FM_CM_TC_PC_MAP_l_PauseClass_8               =  24; #
our $FM_CM_TC_PC_MAP_h_PauseClass_8               =  26; #
our $FM_CM_TC_PC_MAP_l_PauseClass_7               =  21; #
our $FM_CM_TC_PC_MAP_h_PauseClass_7               =  23; #
our $FM_CM_TC_PC_MAP_l_PauseClass_6               =  18; #
our $FM_CM_TC_PC_MAP_h_PauseClass_6               =  20; #
our $FM_CM_TC_PC_MAP_l_PauseClass_5               =  15; #
our $FM_CM_TC_PC_MAP_h_PauseClass_5               =  17; #
our $FM_CM_TC_PC_MAP_l_PauseClass_9               =  27; #
our $FM_CM_TC_PC_MAP_h_PauseClass_9               =  29; #

our $FM_CM_PC_RXMP_MAP_l_RXMP_7                   =  28; #
our $FM_CM_PC_RXMP_MAP_h_RXMP_7                   =  31; #
our $FM_CM_PC_RXMP_MAP_l_RXMP_6                   =  24; #
our $FM_CM_PC_RXMP_MAP_h_RXMP_6                   =  27; #
our $FM_CM_PC_RXMP_MAP_l_RXMP_0                   =  0; #
our $FM_CM_PC_RXMP_MAP_h_RXMP_0                   =  3; #
our $FM_CM_PC_RXMP_MAP_l_RXMP_1                   =  4; #
our $FM_CM_PC_RXMP_MAP_h_RXMP_1                   =  7; #
our $FM_CM_PC_RXMP_MAP_l_RXMP_2                   =  8; #
our $FM_CM_PC_RXMP_MAP_h_RXMP_2                   =  11; #
our $FM_CM_PC_RXMP_MAP_l_RXMP_3                   =  12; #
our $FM_CM_PC_RXMP_MAP_h_RXMP_3                   =  15; #
our $FM_CM_PC_RXMP_MAP_l_RXMP_4                   =  16; #
our $FM_CM_PC_RXMP_MAP_h_RXMP_4                   =  19; #
our $FM_CM_PC_RXMP_MAP_l_RXMP_5                   =  20; #
our $FM_CM_PC_RXMP_MAP_h_RXMP_5                   =  23; #

our $FM_CM_PAUSE_CFG_l_PauseQuantaMultiplierMantissa  =  12; #
our $FM_CM_PAUSE_CFG_h_PauseQuantaMultiplierMantissa  =  16; #
our $FM_CM_PAUSE_CFG_l_PausePacingMask            =  54; #
our $FM_CM_PAUSE_CFG_h_PausePacingMask            =  61; #
our $FM_CM_PAUSE_CFG_l_PauseResendInterval        =  30; #
our $FM_CM_PAUSE_CFG_h_PauseResendInterval        =  53; #
our $FM_CM_PAUSE_CFG_b_SharedPauseEnable_5        =  69; #
our $FM_CM_PAUSE_CFG_b_SharedPauseEnable_6        =  70; #
our $FM_CM_PAUSE_CFG_b_SharedPauseEnable_3        =  67; #
our $FM_CM_PAUSE_CFG_b_SharedPauseEnable_4        =  68; #
our $FM_CM_PAUSE_CFG_b_SharedPauseEnable_9        =  73; #
our $FM_CM_PAUSE_CFG_b_SharedPauseEnable_7        =  71; #
our $FM_CM_PAUSE_CFG_b_SharedPauseEnable_10       =  74; #
our $FM_CM_PAUSE_CFG_l_PauseQuantaMultiplierExponent  =  17; #
our $FM_CM_PAUSE_CFG_h_PauseQuantaMultiplierExponent  =  20; #
our $FM_CM_PAUSE_CFG_b_SharedPauseEnable_8        =  72; #
our $FM_CM_PAUSE_CFG_b_SharedPauseEnable_11       =  75; #
our $FM_CM_PAUSE_CFG_b_PauseType                  =  29; #
our $FM_CM_PAUSE_CFG_b_SharedPauseEnable_1        =  65; #
our $FM_CM_PAUSE_CFG_b_SharedPauseEnable_2        =  66; #
our $FM_CM_PAUSE_CFG_l_PauseMask                  =  0; #
our $FM_CM_PAUSE_CFG_h_PauseMask                  =  11; #
our $FM_CM_PAUSE_CFG_l_PauseQuantaDivisor         =  21; #
our $FM_CM_PAUSE_CFG_h_PauseQuantaDivisor         =  28; #
our $FM_CM_PAUSE_CFG_b_SharedPauseEnable_0        =  64; #

our $FM_CM_PAUSE_PACING_CFG_l_PausePacingTime_1   =  16; #
our $FM_CM_PAUSE_PACING_CFG_h_PausePacingTime_1   =  31; #
our $FM_CM_PAUSE_PACING_CFG_l_PausePacingTime_0   =  0; #
our $FM_CM_PAUSE_PACING_CFG_h_PausePacingTime_0   =  15; #
our $FM_CM_PAUSE_PACING_CFG_l_PausePacingTime_3   =  48; #
our $FM_CM_PAUSE_PACING_CFG_h_PausePacingTime_3   =  63; #
our $FM_CM_PAUSE_PACING_CFG_l_PausePacingTime_2   =  32; #
our $FM_CM_PAUSE_PACING_CFG_h_PausePacingTime_2   =  47; #
our $FM_CM_PAUSE_PACING_CFG_l_PausePacingTime_5   =  80; #
our $FM_CM_PAUSE_PACING_CFG_h_PausePacingTime_5   =  95; #
our $FM_CM_PAUSE_PACING_CFG_l_PausePacingTime_4   =  64; #
our $FM_CM_PAUSE_PACING_CFG_h_PausePacingTime_4   =  79; #
our $FM_CM_PAUSE_PACING_CFG_l_PausePacingTime_7   =  112; #
our $FM_CM_PAUSE_PACING_CFG_h_PausePacingTime_7   =  127; #
our $FM_CM_PAUSE_PACING_CFG_l_PausePacingTime_6   =  96; #
our $FM_CM_PAUSE_PACING_CFG_h_PausePacingTime_6   =  111; #

our $FM_CM_PAUSE_RCV_STATE_l_PauseTime            =  0; #
our $FM_CM_PAUSE_RCV_STATE_h_PauseTime            =  24; #

our $FM_CM_ESCHED_STATE_l_ERLState                =  12; #
our $FM_CM_ESCHED_STATE_h_ERLState                =  23; #
our $FM_CM_ESCHED_STATE_l_SchedState              =  0; #
our $FM_CM_ESCHED_STATE_h_SchedState              =  11; #

our $FM_CM_PAUSE_GEN_STATE_l_PausePortStateRXMP   =  32; #
our $FM_CM_PAUSE_GEN_STATE_h_PausePortStateRXMP   =  43; #
our $FM_CM_PAUSE_GEN_STATE_l_PauseStatePC         =  24; #
our $FM_CM_PAUSE_GEN_STATE_h_PauseStatePC         =  31; #
our $FM_CM_PAUSE_GEN_STATE_l_PauseResendTime      =  0; #
our $FM_CM_PAUSE_GEN_STATE_h_PauseResendTime      =  23; #

our $FM_CM_PAUSE_PACING_STATE_b_Paused_0          =  0; #
our $FM_CM_PAUSE_PACING_STATE_b_Paused_7          =  7; #
our $FM_CM_PAUSE_PACING_STATE_b_Paused_6          =  6; #
our $FM_CM_PAUSE_PACING_STATE_b_Paused_5          =  5; #
our $FM_CM_PAUSE_PACING_STATE_b_Paused_4          =  4; #
our $FM_CM_PAUSE_PACING_STATE_b_Paused_3          =  3; #
our $FM_CM_PAUSE_PACING_STATE_b_Paused_2          =  2; #
our $FM_CM_PAUSE_PACING_STATE_b_Paused_1          =  1; #

our $FM_CM_TX_MIRROR_DEST_b_OverlayEnabled        =  76; #
our $FM_CM_TX_MIRROR_DEST_l_ExplicitDestMask      =  0; #
our $FM_CM_TX_MIRROR_DEST_h_ExplicitDestMask      =  75; #

our $FM_CM_TX_MIRROR_SRC_l_SrcMask                =  0; #
our $FM_CM_TX_MIRROR_SRC_h_SrcMask                =  79; #

our $FM_CM_SAMPLING_MIRROR_CFG_b_MirrorMapPriority  =  5; #
our $FM_CM_SAMPLING_MIRROR_CFG_b_MirrorTX         =  3; #
our $FM_CM_SAMPLING_MIRROR_CFG_b_MirrorTruncate   =  4; #
our $FM_CM_SAMPLING_MIRROR_CFG_l_MirrorNumber     =  0; #
our $FM_CM_SAMPLING_MIRROR_CFG_h_MirrorNumber     =  1; #
our $FM_CM_SAMPLING_MIRROR_CFG_b_MirrorRX         =  2; #

our $FM_ERL_CFG_l_Rate                            =  16; #
our $FM_ERL_CFG_h_Rate                            =  31; #
our $FM_ERL_CFG_l_Capacity                        =  0; #
our $FM_ERL_CFG_h_Capacity                        =  15; #

our $FM_ERL_CFG_IFG_l_IfgPenalty                  =  0; #
our $FM_ERL_CFG_IFG_h_IfgPenalty                  =  7; #

our $FM_ERL_USAGE_l_Credits                       =  0; #
our $FM_ERL_USAGE_h_Credits                       =  22; #

our $FM_CM_QUEUE_STATE_INIT_b_Init                =  0; #

our $FM_MAPPER_VID_PROFILE_TABLE_b_TranslateVID2  =  3; #
our $FM_MAPPER_VID_PROFILE_TABLE_b_SelectIndex1   =  0; #
our $FM_MAPPER_VID_PROFILE_TABLE_b_TranslateVID1  =  2; #
our $FM_MAPPER_VID_PROFILE_TABLE_b_MapValid       =  4; #
our $FM_MAPPER_VID_PROFILE_TABLE_b_SelectIndex2   =  1; #

our $FM_MAPPER_VID1_TABLE_l_VID1_TAG              =  12; #
our $FM_MAPPER_VID1_TABLE_h_VID1_TAG              =  13; #
our $FM_MAPPER_VID1_TABLE_l_MAP_VID1              =  0; #
our $FM_MAPPER_VID1_TABLE_h_MAP_VID1              =  11; #
our $FM_MAPPER_VID1_TABLE_b_VID1_Flag             =  16; #
our $FM_MAPPER_VID1_TABLE_l_VID1_MuxSelect        =  14; #
our $FM_MAPPER_VID1_TABLE_h_VID1_MuxSelect        =  15; #

our $FM_MAPPER_VID2_TABLE_l_VID2_TAG              =  12; #
our $FM_MAPPER_VID2_TABLE_h_VID2_TAG              =  13; #
our $FM_MAPPER_VID2_TABLE_l_MAP_VID2              =  0; #
our $FM_MAPPER_VID2_TABLE_h_MAP_VID2              =  11; #
our $FM_MAPPER_VID2_TABLE_b_VID2_Flag             =  16; #
our $FM_MAPPER_VID2_TABLE_l_VID2_MuxSelect        =  14; #
our $FM_MAPPER_VID2_TABLE_h_VID2_MuxSelect        =  15; #

our $FM_MAPPER_SRC_PORT_TABLE_l_SRC_PORT_QOS_TAG  =  34; #
our $FM_MAPPER_SRC_PORT_TABLE_h_SRC_PORT_QOS_TAG  =  38; #
our $FM_MAPPER_SRC_PORT_TABLE_b_SRC_PORT_Flag2    =  33; #
our $FM_MAPPER_SRC_PORT_TABLE_b_SRC_PORT_Flag1    =  32; #
our $FM_MAPPER_SRC_PORT_TABLE_l_SRC_PORT_ID2      =  8; #
our $FM_MAPPER_SRC_PORT_TABLE_h_SRC_PORT_ID2      =  15; #
our $FM_MAPPER_SRC_PORT_TABLE_l_SRC_PORT_ID3      =  16; #
our $FM_MAPPER_SRC_PORT_TABLE_h_SRC_PORT_ID3      =  23; #
our $FM_MAPPER_SRC_PORT_TABLE_l_SRC_PORT_ID4      =  24; #
our $FM_MAPPER_SRC_PORT_TABLE_h_SRC_PORT_ID4      =  31; #
our $FM_MAPPER_SRC_PORT_TABLE_l_SRC_PORT_ID1      =  0; #
our $FM_MAPPER_SRC_PORT_TABLE_h_SRC_PORT_ID1      =  7; #

our $FM_MAPPER_DMAC_CAM1_l_Key                    =  48; #
our $FM_MAPPER_DMAC_CAM1_h_Key                    =  95; #
our $FM_MAPPER_DMAC_CAM1_l_KeyInvert              =  0; #
our $FM_MAPPER_DMAC_CAM1_h_KeyInvert              =  47; #

our $FM_MAPPER_DMAC_CAM2_l_Key                    =  48; #
our $FM_MAPPER_DMAC_CAM2_h_Key                    =  95; #
our $FM_MAPPER_DMAC_CAM2_l_KeyInvert              =  0; #
our $FM_MAPPER_DMAC_CAM2_h_KeyInvert              =  47; #

our $FM_MAPPER_DMAC_CAM3_l_Key                    =  48; #
our $FM_MAPPER_DMAC_CAM3_h_Key                    =  95; #
our $FM_MAPPER_DMAC_CAM3_l_KeyInvert              =  0; #
our $FM_MAPPER_DMAC_CAM3_h_KeyInvert              =  47; #

our $FM_MAPPER_DMAC_RAM1_l_ID                     =  0; #
our $FM_MAPPER_DMAC_RAM1_h_ID                     =  3; #
our $FM_MAPPER_DMAC_RAM1_b_L2_DMAC_Flag           =  4; #

our $FM_MAPPER_DMAC_RAM2_l_ID                     =  0; #
our $FM_MAPPER_DMAC_RAM2_h_ID                     =  3; #

our $FM_MAPPER_DMAC_RAM3_l_ID                     =  0; #
our $FM_MAPPER_DMAC_RAM3_h_ID                     =  4; #

our $FM_MAPPER_SMAC_CAM1_l_Key                    =  48; #
our $FM_MAPPER_SMAC_CAM1_h_Key                    =  95; #
our $FM_MAPPER_SMAC_CAM1_l_KeyInvert              =  0; #
our $FM_MAPPER_SMAC_CAM1_h_KeyInvert              =  47; #

our $FM_MAPPER_SMAC_CAM3_l_Key                    =  48; #
our $FM_MAPPER_SMAC_CAM3_h_Key                    =  95; #
our $FM_MAPPER_SMAC_CAM3_l_KeyInvert              =  0; #
our $FM_MAPPER_SMAC_CAM3_h_KeyInvert              =  47; #

our $FM_MAPPER_SMAC_RAM1_l_ID                     =  0; #
our $FM_MAPPER_SMAC_RAM1_h_ID                     =  3; #

our $FM_MAPPER_SMAC_RAM3_l_ID                     =  0; #
our $FM_MAPPER_SMAC_RAM3_h_ID                     =  4; #

our $FM_MAPPER_TYPE_CAM1_l_Key                    =  16; #
our $FM_MAPPER_TYPE_CAM1_h_Key                    =  31; #
our $FM_MAPPER_TYPE_CAM1_l_KeyInvert              =  0; #
our $FM_MAPPER_TYPE_CAM1_h_KeyInvert              =  15; #

our $FM_MAPPER_TYPE_CAM2_l_Key                    =  16; #
our $FM_MAPPER_TYPE_CAM2_h_Key                    =  31; #
our $FM_MAPPER_TYPE_CAM2_l_KeyInvert              =  0; #
our $FM_MAPPER_TYPE_CAM2_h_KeyInvert              =  15; #

our $FM_MAPPER_TYPE_RAM1_l_ID                     =  0; #
our $FM_MAPPER_TYPE_RAM1_h_ID                     =  3; #

our $FM_MAPPER_TYPE_RAM2_l_ID                     =  0; #
our $FM_MAPPER_TYPE_RAM2_h_ID                     =  3; #

our $FM_MAPPER_DIP_CAM1_l_Key                     =  64; #
our $FM_MAPPER_DIP_CAM1_h_Key                     =  127; #
our $FM_MAPPER_DIP_CAM1_l_KeyInvert               =  0; #
our $FM_MAPPER_DIP_CAM1_h_KeyInvert               =  63; #

our $FM_MAPPER_DIP_CAM2_l_Key                     =  64; #
our $FM_MAPPER_DIP_CAM2_h_Key                     =  127; #
our $FM_MAPPER_DIP_CAM2_l_KeyInvert               =  0; #
our $FM_MAPPER_DIP_CAM2_h_KeyInvert               =  63; #

our $FM_MAPPER_DIP_CAM3_l_Key                     =  64; #
our $FM_MAPPER_DIP_CAM3_h_Key                     =  127; #
our $FM_MAPPER_DIP_CAM3_l_KeyInvert               =  0; #
our $FM_MAPPER_DIP_CAM3_h_KeyInvert               =  63; #

our $FM_MAPPER_SIP_CAM1_l_Key                     =  64; #
our $FM_MAPPER_SIP_CAM1_h_Key                     =  127; #
our $FM_MAPPER_SIP_CAM1_l_KeyInvert               =  0; #
our $FM_MAPPER_SIP_CAM1_h_KeyInvert               =  63; #

our $FM_MAPPER_SIP_CAM2_l_Key                     =  64; #
our $FM_MAPPER_SIP_CAM2_h_Key                     =  127; #
our $FM_MAPPER_SIP_CAM2_l_KeyInvert               =  0; #
our $FM_MAPPER_SIP_CAM2_h_KeyInvert               =  63; #

our $FM_MAPPER_SIP_CAM3_l_Key                     =  64; #
our $FM_MAPPER_SIP_CAM3_h_Key                     =  127; #
our $FM_MAPPER_SIP_CAM3_l_KeyInvert               =  0; #
our $FM_MAPPER_SIP_CAM3_h_KeyInvert               =  63; #

our $FM_MAPPER_DIP_RAM1_l_ID                      =  0; #
our $FM_MAPPER_DIP_RAM1_h_ID                      =  3; #

our $FM_MAPPER_DIP_RAM2_l_ID                      =  0; #
our $FM_MAPPER_DIP_RAM2_h_ID                      =  3; #

our $FM_MAPPER_DIP_RAM3_l_ID                      =  0; #
our $FM_MAPPER_DIP_RAM3_h_ID                      =  4; #

our $FM_MAPPER_SIP_RAM1_l_ID                      =  0; #
our $FM_MAPPER_SIP_RAM1_h_ID                      =  3; #

our $FM_MAPPER_SIP_RAM2_l_ID                      =  0; #
our $FM_MAPPER_SIP_RAM2_h_ID                      =  3; #

our $FM_MAPPER_SIP_RAM3_l_ID                      =  0; #
our $FM_MAPPER_SIP_RAM3_h_ID                      =  4; #

our $FM_MAPPER_PROT_CAM1_l_Key                    =  8; #
our $FM_MAPPER_PROT_CAM1_h_Key                    =  15; #
our $FM_MAPPER_PROT_CAM1_l_KeyInvert              =  0; #
our $FM_MAPPER_PROT_CAM1_h_KeyInvert              =  7; #

our $FM_MAPPER_PROT_CAM2_l_Key                    =  8; #
our $FM_MAPPER_PROT_CAM2_h_Key                    =  15; #
our $FM_MAPPER_PROT_CAM2_l_KeyInvert              =  0; #
our $FM_MAPPER_PROT_CAM2_h_KeyInvert              =  7; #

our $FM_MAPPER_PROT_RAM1_l_ID                     =  0; #
our $FM_MAPPER_PROT_RAM1_h_ID                     =  3; #

our $FM_MAPPER_PROT_RAM2_l_ID                     =  0; #
our $FM_MAPPER_PROT_RAM2_h_ID                     =  3; #

our $FM_MAPPER_LENGTH_COMPARE_l_LowerBound        =  0; #
our $FM_MAPPER_LENGTH_COMPARE_h_LowerBound        =  15; #
our $FM_MAPPER_LENGTH_COMPARE_l_Bin               =  16; #
our $FM_MAPPER_LENGTH_COMPARE_h_Bin               =  19; #

our $FM_MAPPER_L4_SRC_COMPARE_l_ID                =  25; #
our $FM_MAPPER_L4_SRC_COMPARE_h_ID                =  40; #
our $FM_MAPPER_L4_SRC_COMPARE_b_Valid             =  0; #
our $FM_MAPPER_L4_SRC_COMPARE_l_Protocol          =  1; #
our $FM_MAPPER_L4_SRC_COMPARE_h_Protocol          =  8; #
our $FM_MAPPER_L4_SRC_COMPARE_l_LowerBound        =  9; #
our $FM_MAPPER_L4_SRC_COMPARE_h_LowerBound        =  24; #

our $FM_MAPPER_L4_DST_COMPARE_l_ID                =  25; #
our $FM_MAPPER_L4_DST_COMPARE_h_ID                =  40; #
our $FM_MAPPER_L4_DST_COMPARE_b_Valid             =  0; #
our $FM_MAPPER_L4_DST_COMPARE_l_Protocol          =  1; #
our $FM_MAPPER_L4_DST_COMPARE_h_Protocol          =  8; #
our $FM_MAPPER_L4_DST_COMPARE_l_LowerBound        =  9; #
our $FM_MAPPER_L4_DST_COMPARE_h_LowerBound        =  24; #

our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_Flag8_MuxSelect  =  64; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_Flag8_MuxSelect  =  69; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_Flag7_MuxSelect  =  56; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_Flag7_MuxSelect  =  61; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_reserved10    =  86; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_reserved10    =  87; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_reserved9     =  78; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_reserved9     =  79; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_reserved8     =  70; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_reserved8     =  71; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_reserved14    =  118; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_reserved14    =  119; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_reserved13    =  110; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_reserved13    =  111; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_Flag14_MuxSelect  =  112; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_Flag14_MuxSelect  =  117; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_reserved12    =  102; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_reserved12    =  103; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_reserved11    =  94; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_reserved11    =  95; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_Flag6_MuxSelect  =  48; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_Flag6_MuxSelect  =  53; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_Flag1_MuxSelect  =  8; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_Flag1_MuxSelect  =  13; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_Flag12_MuxSelect  =  96; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_Flag12_MuxSelect  =  101; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_Flag3_MuxSelect  =  24; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_Flag3_MuxSelect  =  29; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_Flag11_MuxSelect  =  88; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_Flag11_MuxSelect  =  93; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_Flag2_MuxSelect  =  16; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_Flag2_MuxSelect  =  21; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_reserved6     =  54; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_reserved6     =  55; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_reserved7     =  62; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_reserved7     =  63; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_reserved4     =  38; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_reserved4     =  39; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_reserved5     =  46; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_reserved5     =  47; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_reserved2     =  22; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_reserved2     =  23; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_reserved3     =  30; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_reserved3     =  31; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_Flag13_MuxSelect  =  104; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_Flag13_MuxSelect  =  109; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_reserved0     =  6; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_reserved0     =  7; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_reserved1     =  14; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_reserved1     =  15; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_Flag9_MuxSelect  =  72; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_Flag9_MuxSelect  =  77; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_Flag15_MuxSelect  =  120; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_Flag15_MuxSelect  =  125; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_Flag0_MuxSelect  =  0; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_Flag0_MuxSelect  =  5; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_Flag10_MuxSelect  =  80; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_Flag10_MuxSelect  =  85; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_Flag4_MuxSelect  =  32; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_Flag4_MuxSelect  =  37; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_l_Flag5_MuxSelect  =  40; #
our $FM_MAPPER_SCENARIO_FLAGS_CFG_h_Flag5_MuxSelect  =  45; #

our $FM_MAPPER_FFU_INIT_l_W16A_MuxSelect2         =  5; #
our $FM_MAPPER_FFU_INIT_h_W16A_MuxSelect2         =  6; #
our $FM_MAPPER_FFU_INIT_l_W16B_MuxSelect2         =  10; #
our $FM_MAPPER_FFU_INIT_h_W16B_MuxSelect2         =  11; #
our $FM_MAPPER_FFU_INIT_l_W16B_MuxSelect1         =  7; #
our $FM_MAPPER_FFU_INIT_h_W16B_MuxSelect1         =  9; #
our $FM_MAPPER_FFU_INIT_l_W8A_MuxSelect           =  12; #
our $FM_MAPPER_FFU_INIT_h_W8A_MuxSelect           =  13; #
our $FM_MAPPER_FFU_INIT_l_W8C_MuxSelect           =  16; #
our $FM_MAPPER_FFU_INIT_h_W8C_MuxSelect           =  17; #
our $FM_MAPPER_FFU_INIT_l_W24_MuxSelect           =  0; #
our $FM_MAPPER_FFU_INIT_h_W24_MuxSelect           =  1; #
our $FM_MAPPER_FFU_INIT_l_W8B_MuxSelect           =  14; #
our $FM_MAPPER_FFU_INIT_h_W8B_MuxSelect           =  15; #
our $FM_MAPPER_FFU_INIT_l_W16A_MuxSelect1         =  2; #
our $FM_MAPPER_FFU_INIT_h_W16A_MuxSelect1         =  4; #

our $FM_MAPPER_QOS_PER_PORT_VPRI1_l_Pri15         =  60; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_h_Pri15         =  63; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_l_Pri14         =  56; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_h_Pri14         =  59; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_l_Pri13         =  52; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_h_Pri13         =  55; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_l_Pri12         =  48; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_h_Pri12         =  51; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_l_Pri7          =  28; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_h_Pri7          =  31; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_l_Pri8          =  32; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_h_Pri8          =  35; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_l_Pri9          =  36; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_h_Pri9          =  39; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_l_Pri11         =  44; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_h_Pri11         =  47; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_l_Pri10         =  40; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_h_Pri10         =  43; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_l_Pri0          =  0; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_h_Pri0          =  3; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_l_Pri2          =  8; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_h_Pri2          =  11; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_l_Pri1          =  4; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_h_Pri1          =  7; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_l_Pri4          =  16; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_h_Pri4          =  19; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_l_Pri3          =  12; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_h_Pri3          =  15; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_l_Pri6          =  24; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_h_Pri6          =  27; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_l_Pri5          =  20; #
our $FM_MAPPER_QOS_PER_PORT_VPRI1_h_Pri5          =  23; #

our $FM_MAPPER_QOS_PER_PORT_VPRI2_l_Pri15         =  60; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_h_Pri15         =  63; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_l_Pri14         =  56; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_h_Pri14         =  59; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_l_Pri13         =  52; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_h_Pri13         =  55; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_l_Pri12         =  48; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_h_Pri12         =  51; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_l_Pri7          =  28; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_h_Pri7          =  31; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_l_Pri8          =  32; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_h_Pri8          =  35; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_l_Pri9          =  36; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_h_Pri9          =  39; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_l_Pri11         =  44; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_h_Pri11         =  47; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_l_Pri10         =  40; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_h_Pri10         =  43; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_l_Pri0          =  0; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_h_Pri0          =  3; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_l_Pri2          =  8; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_h_Pri2          =  11; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_l_Pri1          =  4; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_h_Pri1          =  7; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_l_Pri4          =  16; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_h_Pri4          =  19; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_l_Pri3          =  12; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_h_Pri3          =  15; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_l_Pri6          =  24; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_h_Pri6          =  27; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_l_Pri5          =  20; #
our $FM_MAPPER_QOS_PER_PORT_VPRI2_h_Pri5          =  23; #

our $FM_MAPPER_QOS_PER_PORT_W4_l_Pri15            =  60; #
our $FM_MAPPER_QOS_PER_PORT_W4_h_Pri15            =  63; #
our $FM_MAPPER_QOS_PER_PORT_W4_l_Pri14            =  56; #
our $FM_MAPPER_QOS_PER_PORT_W4_h_Pri14            =  59; #
our $FM_MAPPER_QOS_PER_PORT_W4_l_Pri13            =  52; #
our $FM_MAPPER_QOS_PER_PORT_W4_h_Pri13            =  55; #
our $FM_MAPPER_QOS_PER_PORT_W4_l_Pri12            =  48; #
our $FM_MAPPER_QOS_PER_PORT_W4_h_Pri12            =  51; #
our $FM_MAPPER_QOS_PER_PORT_W4_l_Pri7             =  28; #
our $FM_MAPPER_QOS_PER_PORT_W4_h_Pri7             =  31; #
our $FM_MAPPER_QOS_PER_PORT_W4_l_Pri8             =  32; #
our $FM_MAPPER_QOS_PER_PORT_W4_h_Pri8             =  35; #
our $FM_MAPPER_QOS_PER_PORT_W4_l_Pri9             =  36; #
our $FM_MAPPER_QOS_PER_PORT_W4_h_Pri9             =  39; #
our $FM_MAPPER_QOS_PER_PORT_W4_l_Pri11            =  44; #
our $FM_MAPPER_QOS_PER_PORT_W4_h_Pri11            =  47; #
our $FM_MAPPER_QOS_PER_PORT_W4_l_Pri10            =  40; #
our $FM_MAPPER_QOS_PER_PORT_W4_h_Pri10            =  43; #
our $FM_MAPPER_QOS_PER_PORT_W4_l_Pri0             =  0; #
our $FM_MAPPER_QOS_PER_PORT_W4_h_Pri0             =  3; #
our $FM_MAPPER_QOS_PER_PORT_W4_l_Pri2             =  8; #
our $FM_MAPPER_QOS_PER_PORT_W4_h_Pri2             =  11; #
our $FM_MAPPER_QOS_PER_PORT_W4_l_Pri1             =  4; #
our $FM_MAPPER_QOS_PER_PORT_W4_h_Pri1             =  7; #
our $FM_MAPPER_QOS_PER_PORT_W4_l_Pri4             =  16; #
our $FM_MAPPER_QOS_PER_PORT_W4_h_Pri4             =  19; #
our $FM_MAPPER_QOS_PER_PORT_W4_l_Pri3             =  12; #
our $FM_MAPPER_QOS_PER_PORT_W4_h_Pri3             =  15; #
our $FM_MAPPER_QOS_PER_PORT_W4_l_Pri6             =  24; #
our $FM_MAPPER_QOS_PER_PORT_W4_h_Pri6             =  27; #
our $FM_MAPPER_QOS_PER_PORT_W4_l_Pri5             =  20; #
our $FM_MAPPER_QOS_PER_PORT_W4_h_Pri5             =  23; #

our $FM_MAPPER_QOS_L2_VPRI1_TO_ISL_l_Pri          =  0; #
our $FM_MAPPER_QOS_L2_VPRI1_TO_ISL_h_Pri          =  3; #

our $FM_MAPPER_QOS_L2_VPRI2_TO_ISL_l_Pri          =  0; #
our $FM_MAPPER_QOS_L2_VPRI2_TO_ISL_h_Pri          =  3; #

our $FM_MAPPER_QOS_W4_TO_ISL_l_Pri                =  0; #
our $FM_MAPPER_QOS_W4_TO_ISL_h_Pri                =  3; #

our $FM_MAPPER_QOS_L3_PRI_TO_ISL_l_Pri0           =  0; #
our $FM_MAPPER_QOS_L3_PRI_TO_ISL_h_Pri0           =  3; #
our $FM_MAPPER_QOS_L3_PRI_TO_ISL_l_Pri2           =  8; #
our $FM_MAPPER_QOS_L3_PRI_TO_ISL_h_Pri2           =  11; #
our $FM_MAPPER_QOS_L3_PRI_TO_ISL_l_Pri1           =  4; #
our $FM_MAPPER_QOS_L3_PRI_TO_ISL_h_Pri1           =  7; #
our $FM_MAPPER_QOS_L3_PRI_TO_ISL_l_Pri3           =  12; #
our $FM_MAPPER_QOS_L3_PRI_TO_ISL_h_Pri3           =  15; #

our $FM_MAPPER_QOS_CAM1_l_Key                     =  32; #
our $FM_MAPPER_QOS_CAM1_h_Key                     =  63; #
our $FM_MAPPER_QOS_CAM1_l_KeyInvert               =  0; #
our $FM_MAPPER_QOS_CAM1_h_KeyInvert               =  31; #

our $FM_MAPPER_QOS_CAM2_l_Key                     =  32; #
our $FM_MAPPER_QOS_CAM2_h_Key                     =  63; #
our $FM_MAPPER_QOS_CAM2_l_KeyInvert               =  0; #
our $FM_MAPPER_QOS_CAM2_h_KeyInvert               =  31; #

our $FM_MAPPER_QOS_RAM1_l_L2_VPRI2_Source         =  2; #
our $FM_MAPPER_QOS_RAM1_h_L2_VPRI2_Source         =  3; #
our $FM_MAPPER_QOS_RAM1_l_L2_VPRI1_Source         =  0; #
our $FM_MAPPER_QOS_RAM1_h_L2_VPRI1_Source         =  1; #
our $FM_MAPPER_QOS_RAM1_l_W4_Source               =  4; #
our $FM_MAPPER_QOS_RAM1_h_W4_Source               =  5; #

our $FM_MAPPER_QOS_RAM2_l_ISL_PRI_Source          =  0; #
our $FM_MAPPER_QOS_RAM2_h_ISL_PRI_Source          =  2; #

our $FM_POLICER_CFG_4K_l_Entry1                   =  0; #
our $FM_POLICER_CFG_4K_h_Entry1                   =  31; #
our $FM_POLICER_CFG_4K_l_Entry2                   =  32; #
our $FM_POLICER_CFG_4K_h_Entry2                   =  63; #

our $FM_POLICER_CFG_1K_l_Entry                    =  0; #
our $FM_POLICER_CFG_1K_h_Entry                    =  31; #

our $FM_POLICER_STATE_4K_l_Count2                 =  32; #
our $FM_POLICER_STATE_4K_h_Count2                 =  63; #
our $FM_POLICER_STATE_4K_l_Count1                 =  0; #
our $FM_POLICER_STATE_4K_h_Count1                 =  31; #

our $FM_POLICER_STATE_1K_l_Count                  =  0; #
our $FM_POLICER_STATE_1K_h_Count                  =  31; #

our $FM_POLICER_QOS_MAP1_l_Second                 =  4; #
our $FM_POLICER_QOS_MAP1_h_Second                 =  7; #
our $FM_POLICER_QOS_MAP1_l_First                  =  0; #
our $FM_POLICER_QOS_MAP1_h_First                  =  3; #

our $FM_POLICER_QOS_MAP2_l_Second                 =  8; #
our $FM_POLICER_QOS_MAP2_h_Second                 =  15; #
our $FM_POLICER_QOS_MAP2_l_First                  =  0; #
our $FM_POLICER_QOS_MAP2_h_First                  =  7; #

our $FM_L2AR_CAM_l_Key                            =  64; #
our $FM_L2AR_CAM_h_Key                            =  127; #
our $FM_L2AR_CAM_l_KeyInvert                      =  0; #
our $FM_L2AR_CAM_h_KeyInvert                      =  63; #

our $FM_L2AR_CAM_DMASK_l_Key                      =  64; #
our $FM_L2AR_CAM_DMASK_h_Key                      =  101; #
our $FM_L2AR_CAM_DMASK_l_KeyInvert                =  0; #
our $FM_L2AR_CAM_DMASK_h_KeyInvert                =  37; #

our $FM_L2AR_SLICE_CFG_l_SliceDisable             =  8; #
our $FM_L2AR_SLICE_CFG_h_SliceDisable             =  15; #
our $FM_L2AR_SLICE_CFG_l_ChainedPrecedence        =  0; #
our $FM_L2AR_SLICE_CFG_h_ChainedPrecedence        =  7; #

our $FM_L2AR_EACL_EXT_l_ExtHitConditioned         =  0; #
our $FM_L2AR_EACL_EXT_h_ExtHitConditioned         =  63; #

our $FM_L2AR_RAM_b_SetMirror_0                    =  20; #
our $FM_L2AR_RAM_b_SetMirror_1                    =  21; #
our $FM_L2AR_RAM_b_SetMirror_2                    =  22; #
our $FM_L2AR_RAM_b_SetMirror_3                    =  23; #
our $FM_L2AR_RAM_l_FLAGS_TAG                      =  0; #
our $FM_L2AR_RAM_h_FLAGS_TAG                      =  7; #
our $FM_L2AR_RAM_l_DMT_PROFILE                    =  8; #
our $FM_L2AR_RAM_h_DMT_PROFILE                    =  12; #
our $FM_L2AR_RAM_b_TransformDestMask              =  16; #
our $FM_L2AR_RAM_b_MuxOutput_W8ABCDE              =  29; #
our $FM_L2AR_RAM_b_MuxOutput_STATS_IDX5C          =  34; #
our $FM_L2AR_RAM_b_MuxOutput_W16CDEF              =  28; #
our $FM_L2AR_RAM_b_MuxOutput_STATS_IDX5AB         =  33; #
our $FM_L2AR_RAM_b_SetTrapHeader                  =  19; #
our $FM_L2AR_RAM_b_MuxOutput_MA_WRITEBACK         =  25; #
our $FM_L2AR_RAM_b_MuxOutput_W4                   =  30; #
our $FM_L2AR_RAM_b_MuxOutput_VID                  =  31; #
our $FM_L2AR_RAM_b_DMT_NEXT_STAGE                 =  17; #
our $FM_L2AR_RAM_b_MuxOutput_QOS                  =  24; #
our $FM_L2AR_RAM_b_MuxOutput_STATS_IDX16B         =  38; #
our $FM_L2AR_RAM_b_MuxOutput_STATS_IDX16A         =  37; #
our $FM_L2AR_RAM_b_MuxOutput_W16AB                =  27; #
our $FM_L2AR_RAM_b_MuxOutput_STATS_IDX12A         =  35; #
our $FM_L2AR_RAM_b_SetCpuCode                     =  18; #
our $FM_L2AR_RAM_b_MuxOutput_STATS_IDX12B         =  36; #
our $FM_L2AR_RAM_b_MuxOutput_DMASK_IDX            =  32; #
our $FM_L2AR_RAM_b_MuxOutput_DGLORT               =  26; #

our $FM_L2AR_FLAGS_CAM_POLARITY_l_Polarity        =  0; #
our $FM_L2AR_FLAGS_CAM_POLARITY_h_Polarity        =  75; #

our $FM_L2AR_FLAGS_CAM_VALUE_l_Value              =  0; #
our $FM_L2AR_FLAGS_CAM_VALUE_h_Value              =  75; #

our $FM_L2AR_FLAGS_CAM_l_Key                      =  64; #
our $FM_L2AR_FLAGS_CAM_h_Key                      =  127; #
our $FM_L2AR_FLAGS_CAM_l_KeyInvert                =  0; #
our $FM_L2AR_FLAGS_CAM_h_KeyInvert                =  63; #

our $FM_L2AR_ACTION_DMT_l_CmdB                    =  12; #
our $FM_L2AR_ACTION_DMT_h_CmdB                    =  14; #
our $FM_L2AR_ACTION_DMT_l_ACTION_DMASK            =  20; #
our $FM_L2AR_ACTION_DMT_h_ACTION_DMASK            =  95; #
our $FM_L2AR_ACTION_DMT_l_ACTION_DROP_CODE        =  0; #
our $FM_L2AR_ACTION_DMT_h_ACTION_DROP_CODE        =  7; #
our $FM_L2AR_ACTION_DMT_l_CmdA                    =  8; #
our $FM_L2AR_ACTION_DMT_h_CmdA                    =  11; #

our $FM_L2AR_ACTION_CPU_CODE_l_CPU_CODE_3         =  24; #
our $FM_L2AR_ACTION_CPU_CODE_h_CPU_CODE_3         =  31; #
our $FM_L2AR_ACTION_CPU_CODE_l_CPU_CODE_0         =  0; #
our $FM_L2AR_ACTION_CPU_CODE_h_CPU_CODE_0         =  7; #
our $FM_L2AR_ACTION_CPU_CODE_l_CPU_CODE_1         =  8; #
our $FM_L2AR_ACTION_CPU_CODE_h_CPU_CODE_1         =  15; #
our $FM_L2AR_ACTION_CPU_CODE_l_CPU_CODE_2         =  16; #
our $FM_L2AR_ACTION_CPU_CODE_h_CPU_CODE_2         =  23; #

our $FM_L2AR_ACTION_TRAP_HEADER_b_TRAP_HEADER_IDX_2  =  6; #
our $FM_L2AR_ACTION_TRAP_HEADER_b_TRAP_HEADER_IDX_3  =  7; #
our $FM_L2AR_ACTION_TRAP_HEADER_b_TRAP_HEADER_ENABLE_2  =  2; #
our $FM_L2AR_ACTION_TRAP_HEADER_b_TRAP_HEADER_ENABLE_3  =  3; #
our $FM_L2AR_ACTION_TRAP_HEADER_b_TRAP_HEADER_ENABLE_0  =  0; #
our $FM_L2AR_ACTION_TRAP_HEADER_b_TRAP_HEADER_ENABLE_1  =  1; #
our $FM_L2AR_ACTION_TRAP_HEADER_b_TRAP_HEADER_IDX_1  =  5; #
our $FM_L2AR_ACTION_TRAP_HEADER_b_TRAP_HEADER_IDX_0  =  4; #

our $FM_L2AR_ACTION_MIRROR_b_MIR_MAP_PRI_3        =  15; #
our $FM_L2AR_ACTION_MIRROR_b_MIR_MAP_PRI_2        =  14; #
our $FM_L2AR_ACTION_MIRROR_b_MIR_MAP_PRI_1        =  13; #
our $FM_L2AR_ACTION_MIRROR_b_MIR_MAP_PRI_0        =  12; #
our $FM_L2AR_ACTION_MIRROR_b_MIR_TX_3             =  7; #
our $FM_L2AR_ACTION_MIRROR_b_MIR_TX_2             =  6; #
our $FM_L2AR_ACTION_MIRROR_b_MIR_TRUNC_3          =  11; #
our $FM_L2AR_ACTION_MIRROR_b_MIR_TRUNC_2          =  10; #
our $FM_L2AR_ACTION_MIRROR_b_MIR_TRUNC_1          =  9; #
our $FM_L2AR_ACTION_MIRROR_b_MIR_TRUNC_0          =  8; #
our $FM_L2AR_ACTION_MIRROR_b_MIR_RX_2             =  2; #
our $FM_L2AR_ACTION_MIRROR_b_MIR_RX_3             =  3; #
our $FM_L2AR_ACTION_MIRROR_b_MIR_RX_0             =  0; #
our $FM_L2AR_ACTION_MIRROR_b_MIR_RX_1             =  1; #
our $FM_L2AR_ACTION_MIRROR_b_MIR_TX_1             =  5; #
our $FM_L2AR_ACTION_MIRROR_b_MIR_TX_0             =  4; #

our $FM_L2AR_ACTION_QOS_l_QOS_PROFILE_2           =  10; #
our $FM_L2AR_ACTION_QOS_h_QOS_PROFILE_2           =  14; #
our $FM_L2AR_ACTION_QOS_l_QOS_PROFILE_3           =  15; #
our $FM_L2AR_ACTION_QOS_h_QOS_PROFILE_3           =  19; #
our $FM_L2AR_ACTION_QOS_l_QOS_PROFILE_0           =  0; #
our $FM_L2AR_ACTION_QOS_h_QOS_PROFILE_0           =  4; #
our $FM_L2AR_ACTION_QOS_l_QOS_PROFILE_1           =  5; #
our $FM_L2AR_ACTION_QOS_h_QOS_PROFILE_1           =  9; #

our $FM_L2AR_ACTION_MA_WRITEBACK_l_MA_WRITEBACK_PROFILE_2  =  10; #
our $FM_L2AR_ACTION_MA_WRITEBACK_h_MA_WRITEBACK_PROFILE_2  =  14; #
our $FM_L2AR_ACTION_MA_WRITEBACK_l_MA_WRITEBACK_PROFILE_3  =  15; #
our $FM_L2AR_ACTION_MA_WRITEBACK_h_MA_WRITEBACK_PROFILE_3  =  19; #
our $FM_L2AR_ACTION_MA_WRITEBACK_l_MA_WRITEBACK_PROFILE_0  =  0; #
our $FM_L2AR_ACTION_MA_WRITEBACK_h_MA_WRITEBACK_PROFILE_0  =  4; #
our $FM_L2AR_ACTION_MA_WRITEBACK_l_MA_WRITEBACK_PROFILE_1  =  5; #
our $FM_L2AR_ACTION_MA_WRITEBACK_h_MA_WRITEBACK_PROFILE_1  =  9; #

our $FM_L2AR_ACTION_DGLORT_l_DGLORT_PROFILE_2     =  10; #
our $FM_L2AR_ACTION_DGLORT_h_DGLORT_PROFILE_2     =  14; #
our $FM_L2AR_ACTION_DGLORT_l_DGLORT_PROFILE_1     =  5; #
our $FM_L2AR_ACTION_DGLORT_h_DGLORT_PROFILE_1     =  9; #
our $FM_L2AR_ACTION_DGLORT_l_DGLORT_PROFILE_3     =  15; #
our $FM_L2AR_ACTION_DGLORT_h_DGLORT_PROFILE_3     =  19; #
our $FM_L2AR_ACTION_DGLORT_l_DGLORT_PROFILE_0     =  0; #
our $FM_L2AR_ACTION_DGLORT_h_DGLORT_PROFILE_0     =  4; #

our $FM_L2AR_ACTION_W16AB_l_W16AB_PROFILE_0       =  0; #
our $FM_L2AR_ACTION_W16AB_h_W16AB_PROFILE_0       =  4; #
our $FM_L2AR_ACTION_W16AB_l_W16AB_PROFILE_1       =  5; #
our $FM_L2AR_ACTION_W16AB_h_W16AB_PROFILE_1       =  9; #
our $FM_L2AR_ACTION_W16AB_l_W16AB_PROFILE_2       =  10; #
our $FM_L2AR_ACTION_W16AB_h_W16AB_PROFILE_2       =  14; #
our $FM_L2AR_ACTION_W16AB_l_W16AB_PROFILE_3       =  15; #
our $FM_L2AR_ACTION_W16AB_h_W16AB_PROFILE_3       =  19; #

our $FM_L2AR_ACTION_W16CDEF_l_W16CDEF_PROFILE_3   =  15; #
our $FM_L2AR_ACTION_W16CDEF_h_W16CDEF_PROFILE_3   =  19; #
our $FM_L2AR_ACTION_W16CDEF_l_W16CDEF_PROFILE_2   =  10; #
our $FM_L2AR_ACTION_W16CDEF_h_W16CDEF_PROFILE_2   =  14; #
our $FM_L2AR_ACTION_W16CDEF_l_W16CDEF_PROFILE_1   =  5; #
our $FM_L2AR_ACTION_W16CDEF_h_W16CDEF_PROFILE_1   =  9; #
our $FM_L2AR_ACTION_W16CDEF_l_W16CDEF_PROFILE_0   =  0; #
our $FM_L2AR_ACTION_W16CDEF_h_W16CDEF_PROFILE_0   =  4; #

our $FM_L2AR_ACTION_W8ABCDE_l_W8ABCDE_PROFILE_3   =  15; #
our $FM_L2AR_ACTION_W8ABCDE_h_W8ABCDE_PROFILE_3   =  19; #
our $FM_L2AR_ACTION_W8ABCDE_l_W8ABCDE_PROFILE_1   =  5; #
our $FM_L2AR_ACTION_W8ABCDE_h_W8ABCDE_PROFILE_1   =  9; #
our $FM_L2AR_ACTION_W8ABCDE_l_W8ABCDE_PROFILE_2   =  10; #
our $FM_L2AR_ACTION_W8ABCDE_h_W8ABCDE_PROFILE_2   =  14; #
our $FM_L2AR_ACTION_W8ABCDE_l_W8ABCDE_PROFILE_0   =  0; #
our $FM_L2AR_ACTION_W8ABCDE_h_W8ABCDE_PROFILE_0   =  4; #

our $FM_L2AR_ACTION_W4_l_W4_PROFILE_0             =  0; #
our $FM_L2AR_ACTION_W4_h_W4_PROFILE_0             =  4; #
our $FM_L2AR_ACTION_W4_l_W4_PROFILE_1             =  5; #
our $FM_L2AR_ACTION_W4_h_W4_PROFILE_1             =  9; #
our $FM_L2AR_ACTION_W4_l_W4_PROFILE_2             =  10; #
our $FM_L2AR_ACTION_W4_h_W4_PROFILE_2             =  14; #
our $FM_L2AR_ACTION_W4_l_W4_PROFILE_3             =  15; #
our $FM_L2AR_ACTION_W4_h_W4_PROFILE_3             =  19; #

our $FM_L2AR_ACTION_VID_l_VID_PROFILE_0           =  0; #
our $FM_L2AR_ACTION_VID_h_VID_PROFILE_0           =  4; #
our $FM_L2AR_ACTION_VID_l_VID_PROFILE_1           =  5; #
our $FM_L2AR_ACTION_VID_h_VID_PROFILE_1           =  9; #
our $FM_L2AR_ACTION_VID_l_VID_PROFILE_2           =  10; #
our $FM_L2AR_ACTION_VID_h_VID_PROFILE_2           =  14; #
our $FM_L2AR_ACTION_VID_l_VID_PROFILE_3           =  15; #
our $FM_L2AR_ACTION_VID_h_VID_PROFILE_3           =  19; #

our $FM_L2AR_ACTION_DMASK_IDX_l_DMASK_IDX_PROFILE_3  =  15; #
our $FM_L2AR_ACTION_DMASK_IDX_h_DMASK_IDX_PROFILE_3  =  19; #
our $FM_L2AR_ACTION_DMASK_IDX_l_DMASK_IDX_PROFILE_2  =  10; #
our $FM_L2AR_ACTION_DMASK_IDX_h_DMASK_IDX_PROFILE_2  =  14; #
our $FM_L2AR_ACTION_DMASK_IDX_l_DMASK_IDX_PROFILE_1  =  5; #
our $FM_L2AR_ACTION_DMASK_IDX_h_DMASK_IDX_PROFILE_1  =  9; #
our $FM_L2AR_ACTION_DMASK_IDX_l_DMASK_IDX_PROFILE_0  =  0; #
our $FM_L2AR_ACTION_DMASK_IDX_h_DMASK_IDX_PROFILE_0  =  4; #

our $FM_L2AR_ACTION_STATS_IDX5AB_l_STATS_IDX5AB_PROFILE_1  =  5; #
our $FM_L2AR_ACTION_STATS_IDX5AB_h_STATS_IDX5AB_PROFILE_1  =  9; #
our $FM_L2AR_ACTION_STATS_IDX5AB_l_STATS_IDX5AB_PROFILE_0  =  0; #
our $FM_L2AR_ACTION_STATS_IDX5AB_h_STATS_IDX5AB_PROFILE_0  =  4; #
our $FM_L2AR_ACTION_STATS_IDX5AB_l_STATS_IDX5AB_PROFILE_3  =  15; #
our $FM_L2AR_ACTION_STATS_IDX5AB_h_STATS_IDX5AB_PROFILE_3  =  19; #
our $FM_L2AR_ACTION_STATS_IDX5AB_l_STATS_IDX5AB_PROFILE_2  =  10; #
our $FM_L2AR_ACTION_STATS_IDX5AB_h_STATS_IDX5AB_PROFILE_2  =  14; #

our $FM_L2AR_ACTION_STATS_IDX5C_l_STATS_IDX5C_PROFILE_0  =  0; #
our $FM_L2AR_ACTION_STATS_IDX5C_h_STATS_IDX5C_PROFILE_0  =  4; #
our $FM_L2AR_ACTION_STATS_IDX5C_l_STATS_IDX5C_PROFILE_1  =  5; #
our $FM_L2AR_ACTION_STATS_IDX5C_h_STATS_IDX5C_PROFILE_1  =  9; #
our $FM_L2AR_ACTION_STATS_IDX5C_l_STATS_IDX5C_PROFILE_2  =  10; #
our $FM_L2AR_ACTION_STATS_IDX5C_h_STATS_IDX5C_PROFILE_2  =  14; #
our $FM_L2AR_ACTION_STATS_IDX5C_l_STATS_IDX5C_PROFILE_3  =  15; #
our $FM_L2AR_ACTION_STATS_IDX5C_h_STATS_IDX5C_PROFILE_3  =  19; #

our $FM_L2AR_ACTION_STATS_IDX12A_l_STATS_IDX12A_PROFILE_1  =  5; #
our $FM_L2AR_ACTION_STATS_IDX12A_h_STATS_IDX12A_PROFILE_1  =  9; #
our $FM_L2AR_ACTION_STATS_IDX12A_l_STATS_IDX12A_PROFILE_0  =  0; #
our $FM_L2AR_ACTION_STATS_IDX12A_h_STATS_IDX12A_PROFILE_0  =  4; #
our $FM_L2AR_ACTION_STATS_IDX12A_l_STATS_IDX12A_PROFILE_3  =  15; #
our $FM_L2AR_ACTION_STATS_IDX12A_h_STATS_IDX12A_PROFILE_3  =  19; #
our $FM_L2AR_ACTION_STATS_IDX12A_l_STATS_IDX12A_PROFILE_2  =  10; #
our $FM_L2AR_ACTION_STATS_IDX12A_h_STATS_IDX12A_PROFILE_2  =  14; #

our $FM_L2AR_ACTION_STATS_IDX12B_l_STATS_IDX12B_PROFILE_2  =  10; #
our $FM_L2AR_ACTION_STATS_IDX12B_h_STATS_IDX12B_PROFILE_2  =  14; #
our $FM_L2AR_ACTION_STATS_IDX12B_l_STATS_IDX12B_PROFILE_1  =  5; #
our $FM_L2AR_ACTION_STATS_IDX12B_h_STATS_IDX12B_PROFILE_1  =  9; #
our $FM_L2AR_ACTION_STATS_IDX12B_l_STATS_IDX12B_PROFILE_3  =  15; #
our $FM_L2AR_ACTION_STATS_IDX12B_h_STATS_IDX12B_PROFILE_3  =  19; #
our $FM_L2AR_ACTION_STATS_IDX12B_l_STATS_IDX12B_PROFILE_0  =  0; #
our $FM_L2AR_ACTION_STATS_IDX12B_h_STATS_IDX12B_PROFILE_0  =  4; #

our $FM_L2AR_ACTION_STATS_IDX16A_l_STATS_IDX16A_PROFILE_1  =  5; #
our $FM_L2AR_ACTION_STATS_IDX16A_h_STATS_IDX16A_PROFILE_1  =  9; #
our $FM_L2AR_ACTION_STATS_IDX16A_l_STATS_IDX16A_PROFILE_0  =  0; #
our $FM_L2AR_ACTION_STATS_IDX16A_h_STATS_IDX16A_PROFILE_0  =  4; #
our $FM_L2AR_ACTION_STATS_IDX16A_l_STATS_IDX16A_PROFILE_3  =  15; #
our $FM_L2AR_ACTION_STATS_IDX16A_h_STATS_IDX16A_PROFILE_3  =  19; #
our $FM_L2AR_ACTION_STATS_IDX16A_l_STATS_IDX16A_PROFILE_2  =  10; #
our $FM_L2AR_ACTION_STATS_IDX16A_h_STATS_IDX16A_PROFILE_2  =  14; #

our $FM_L2AR_ACTION_STATS_IDX16B_l_STATS_IDX16B_PROFILE_2  =  10; #
our $FM_L2AR_ACTION_STATS_IDX16B_h_STATS_IDX16B_PROFILE_2  =  14; #
our $FM_L2AR_ACTION_STATS_IDX16B_l_STATS_IDX16B_PROFILE_1  =  5; #
our $FM_L2AR_ACTION_STATS_IDX16B_h_STATS_IDX16B_PROFILE_1  =  9; #
our $FM_L2AR_ACTION_STATS_IDX16B_l_STATS_IDX16B_PROFILE_3  =  15; #
our $FM_L2AR_ACTION_STATS_IDX16B_h_STATS_IDX16B_PROFILE_3  =  19; #
our $FM_L2AR_ACTION_STATS_IDX16B_l_STATS_IDX16B_PROFILE_0  =  0; #
our $FM_L2AR_ACTION_STATS_IDX16B_h_STATS_IDX16B_PROFILE_0  =  4; #

our $FM_L2AR_DGLORT_PROFILE_TABLE_l_Value_DGLORT  =  0; #
our $FM_L2AR_DGLORT_PROFILE_TABLE_h_Value_DGLORT  =  15; #
our $FM_L2AR_DGLORT_PROFILE_TABLE_l_Mask_DGLORT   =  16; #
our $FM_L2AR_DGLORT_PROFILE_TABLE_h_Mask_DGLORT   =  31; #

our $FM_L2AR_W16AB_PROFILE_TABLE_l_Mask_MOD_DATA_W16A  =  16; #
our $FM_L2AR_W16AB_PROFILE_TABLE_h_Mask_MOD_DATA_W16A  =  31; #
our $FM_L2AR_W16AB_PROFILE_TABLE_l_Select_MOD_DATA_W16A  =  32; #
our $FM_L2AR_W16AB_PROFILE_TABLE_h_Select_MOD_DATA_W16A  =  34; #
our $FM_L2AR_W16AB_PROFILE_TABLE_l_Select_MOD_DATA_W16B  =  35; #
our $FM_L2AR_W16AB_PROFILE_TABLE_h_Select_MOD_DATA_W16B  =  36; #
our $FM_L2AR_W16AB_PROFILE_TABLE_l_Value_MOD_DATA_W16A  =  0; #
our $FM_L2AR_W16AB_PROFILE_TABLE_h_Value_MOD_DATA_W16A  =  15; #

our $FM_L2AR_W16CDEF_PROFILE_TABLE_l_Select_MOD_DATA_W16E  =  70; #
our $FM_L2AR_W16CDEF_PROFILE_TABLE_h_Select_MOD_DATA_W16E  =  72; #
our $FM_L2AR_W16CDEF_PROFILE_TABLE_l_Select_MOD_DATA_W16F  =  73; #
our $FM_L2AR_W16CDEF_PROFILE_TABLE_h_Select_MOD_DATA_W16F  =  75; #
our $FM_L2AR_W16CDEF_PROFILE_TABLE_l_Select_MOD_DATA_W16C  =  64; #
our $FM_L2AR_W16CDEF_PROFILE_TABLE_h_Select_MOD_DATA_W16C  =  66; #
our $FM_L2AR_W16CDEF_PROFILE_TABLE_l_Select_MOD_DATA_W16D  =  67; #
our $FM_L2AR_W16CDEF_PROFILE_TABLE_h_Select_MOD_DATA_W16D  =  69; #
our $FM_L2AR_W16CDEF_PROFILE_TABLE_l_Value_MOD_DATA_W16C  =  0; #
our $FM_L2AR_W16CDEF_PROFILE_TABLE_h_Value_MOD_DATA_W16C  =  15; #
our $FM_L2AR_W16CDEF_PROFILE_TABLE_l_Value_MOD_DATA_W16E  =  32; #
our $FM_L2AR_W16CDEF_PROFILE_TABLE_h_Value_MOD_DATA_W16E  =  47; #
our $FM_L2AR_W16CDEF_PROFILE_TABLE_l_Value_MOD_DATA_W16D  =  16; #
our $FM_L2AR_W16CDEF_PROFILE_TABLE_h_Value_MOD_DATA_W16D  =  31; #
our $FM_L2AR_W16CDEF_PROFILE_TABLE_l_Value_MOD_DATA_W16F  =  48; #
our $FM_L2AR_W16CDEF_PROFILE_TABLE_h_Value_MOD_DATA_W16F  =  63; #

our $FM_L2AR_W8ABCDE_PROFILE_TABLE_l_Value_MOD_DATA_W8D  =  24; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_h_Value_MOD_DATA_W8D  =  31; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_l_Value_MOD_DATA_W8C  =  16; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_h_Value_MOD_DATA_W8C  =  23; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_l_Value_MOD_DATA_W8E  =  32; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_h_Value_MOD_DATA_W8E  =  39; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_l_Select_MOD_DATA_W8B  =  82; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_h_Select_MOD_DATA_W8B  =  84; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_l_Select_MOD_DATA_W8C  =  85; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_h_Select_MOD_DATA_W8C  =  87; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_l_Select_MOD_DATA_W8D  =  88; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_h_Select_MOD_DATA_W8D  =  90; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_l_Select_MOD_DATA_W8E  =  91; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_h_Select_MOD_DATA_W8E  =  93; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_l_Mask_MOD_DATA_W8A  =  40; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_h_Mask_MOD_DATA_W8A  =  47; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_l_Mask_MOD_DATA_W8B  =  48; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_h_Mask_MOD_DATA_W8B  =  55; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_l_Mask_MOD_DATA_W8C  =  56; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_h_Mask_MOD_DATA_W8C  =  63; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_l_Value_MOD_DATA_W8B  =  8; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_h_Value_MOD_DATA_W8B  =  15; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_l_Mask_MOD_DATA_W8D  =  64; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_h_Mask_MOD_DATA_W8D  =  71; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_l_Value_MOD_DATA_W8A  =  0; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_h_Value_MOD_DATA_W8A  =  7; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_l_Mask_MOD_DATA_W8E  =  72; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_h_Mask_MOD_DATA_W8E  =  79; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_l_Select_MOD_DATA_W8A  =  80; #
our $FM_L2AR_W8ABCDE_PROFILE_TABLE_h_Select_MOD_DATA_W8A  =  81; #

our $FM_L2AR_W4_PROFILE_TABLE_l_Select_QOS_W4     =  20; #
our $FM_L2AR_W4_PROFILE_TABLE_h_Select_QOS_W4     =  24; #
our $FM_L2AR_W4_PROFILE_TABLE_l_Select_MOD_DATA_W4  =  16; #
our $FM_L2AR_W4_PROFILE_TABLE_h_Select_MOD_DATA_W4  =  19; #
our $FM_L2AR_W4_PROFILE_TABLE_l_Value_QOS_W4      =  4; #
our $FM_L2AR_W4_PROFILE_TABLE_h_Value_QOS_W4      =  7; #
our $FM_L2AR_W4_PROFILE_TABLE_l_Mask_MOD_DATA_W4  =  8; #
our $FM_L2AR_W4_PROFILE_TABLE_h_Mask_MOD_DATA_W4  =  11; #
our $FM_L2AR_W4_PROFILE_TABLE_l_Value_MOD_DATA_W4  =  0; #
our $FM_L2AR_W4_PROFILE_TABLE_h_Value_MOD_DATA_W4  =  3; #
our $FM_L2AR_W4_PROFILE_TABLE_l_Mask_QOS_W4       =  12; #
our $FM_L2AR_W4_PROFILE_TABLE_h_Mask_QOS_W4       =  15; #

our $FM_L2AR_VID_PROFILE_TABLE_l_Select_L2_VID1   =  24; #
our $FM_L2AR_VID_PROFILE_TABLE_h_Select_L2_VID1   =  25; #
our $FM_L2AR_VID_PROFILE_TABLE_l_Select_L2_VID2   =  26; #
our $FM_L2AR_VID_PROFILE_TABLE_h_Select_L2_VID2   =  27; #
our $FM_L2AR_VID_PROFILE_TABLE_l_Value_L2_VID1    =  0; #
our $FM_L2AR_VID_PROFILE_TABLE_h_Value_L2_VID1    =  11; #
our $FM_L2AR_VID_PROFILE_TABLE_l_Value_L2_VID2    =  12; #
our $FM_L2AR_VID_PROFILE_TABLE_h_Value_L2_VID2    =  23; #

our $FM_L2AR_DMASK_IDX_PROFILE_TABLE_l_Value_DMASK_IDX  =  0; #
our $FM_L2AR_DMASK_IDX_PROFILE_TABLE_h_Value_DMASK_IDX  =  11; #
our $FM_L2AR_DMASK_IDX_PROFILE_TABLE_l_Select_DMASK_IDX  =  12; #
our $FM_L2AR_DMASK_IDX_PROFILE_TABLE_h_Select_DMASK_IDX  =  13; #

our $FM_L2AR_QOS_PROFILE_TABLE_l_Mask_QOS_L2_VPRI1  =  24; #
our $FM_L2AR_QOS_PROFILE_TABLE_h_Mask_QOS_L2_VPRI1  =  27; #
our $FM_L2AR_QOS_PROFILE_TABLE_l_Mask_QOS_L2_VPRI2  =  28; #
our $FM_L2AR_QOS_PROFILE_TABLE_h_Mask_QOS_L2_VPRI2  =  31; #
our $FM_L2AR_QOS_PROFILE_TABLE_l_Select_QOS_L3_PRI  =  55; #
our $FM_L2AR_QOS_PROFILE_TABLE_h_Select_QOS_L3_PRI  =  58; #
our $FM_L2AR_QOS_PROFILE_TABLE_l_Mask_QOS_ISL_PRI  =  20; #
our $FM_L2AR_QOS_PROFILE_TABLE_h_Mask_QOS_ISL_PRI  =  23; #
our $FM_L2AR_QOS_PROFILE_TABLE_l_Value_QOS_L2_VPRI1  =  4; #
our $FM_L2AR_QOS_PROFILE_TABLE_h_Value_QOS_L2_VPRI1  =  7; #
our $FM_L2AR_QOS_PROFILE_TABLE_l_Select_QOS_ISL_PRI  =  40; #
our $FM_L2AR_QOS_PROFILE_TABLE_h_Select_QOS_ISL_PRI  =  44; #
our $FM_L2AR_QOS_PROFILE_TABLE_l_Mask_QOS_L3_PRI  =  32; #
our $FM_L2AR_QOS_PROFILE_TABLE_h_Mask_QOS_L3_PRI  =  39; #
our $FM_L2AR_QOS_PROFILE_TABLE_l_Value_QOS_L3_PRI  =  12; #
our $FM_L2AR_QOS_PROFILE_TABLE_h_Value_QOS_L3_PRI  =  19; #
our $FM_L2AR_QOS_PROFILE_TABLE_l_Value_QOS_L2_VPRI2  =  8; #
our $FM_L2AR_QOS_PROFILE_TABLE_h_Value_QOS_L2_VPRI2  =  11; #
our $FM_L2AR_QOS_PROFILE_TABLE_l_Select_QOS_L2_VPRI2  =  50; #
our $FM_L2AR_QOS_PROFILE_TABLE_h_Select_QOS_L2_VPRI2  =  54; #
our $FM_L2AR_QOS_PROFILE_TABLE_l_Value_QOS_ISL_PRI  =  0; #
our $FM_L2AR_QOS_PROFILE_TABLE_h_Value_QOS_ISL_PRI  =  3; #
our $FM_L2AR_QOS_PROFILE_TABLE_l_Select_QOS_L2_VPRI1  =  45; #
our $FM_L2AR_QOS_PROFILE_TABLE_h_Select_QOS_L2_VPRI1  =  49; #

our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_l_Select_MA_WRITEBACK_GLORT  =  74; #
our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_h_Select_MA_WRITEBACK_GLORT  =  75; #
our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_l_Value_MA_WRITEBACK_DATA  =  30; #
our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_h_Value_MA_WRITEBACK_DATA  =  37; #
our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_l_Value_MA_WRITEBACK_GLORT  =  2; #
our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_h_Value_MA_WRITEBACK_GLORT  =  17; #
our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_l_Select_MA_WRITEBACK_DATA  =  78; #
our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_h_Select_MA_WRITEBACK_DATA  =  79; #
our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_l_Value_MA_WRITEBACK_PREC  =  0; #
our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_h_Value_MA_WRITEBACK_PREC  =  1; #
our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_l_Mask_MA_WRITEBACK_GLORT  =  38; #
our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_h_Mask_MA_WRITEBACK_GLORT  =  53; #
our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_l_Mask_MA_WRITEBACK_TAG  =  54; #
our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_h_Mask_MA_WRITEBACK_TAG  =  65; #
our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_l_Value_MA_WRITEBACK_TAG  =  18; #
our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_h_Value_MA_WRITEBACK_TAG  =  29; #
our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_l_Select_MA_WRITEBACK_TAG  =  76; #
our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_h_Select_MA_WRITEBACK_TAG  =  77; #
our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_l_Mask_MA_WRITEBACK_DATA  =  66; #
our $FM_L2AR_MA_WRITEBACK_PROFILE_TABLE_h_Mask_MA_WRITEBACK_DATA  =  73; #

our $FM_L2AR_STATS_IDX5AB_PROFILE_TABLE_l_Value_RX_STATS_IDX5A  =  0; #
our $FM_L2AR_STATS_IDX5AB_PROFILE_TABLE_h_Value_RX_STATS_IDX5A  =  4; #
our $FM_L2AR_STATS_IDX5AB_PROFILE_TABLE_l_Select_RX_STATS_IDX5B  =  14; #
our $FM_L2AR_STATS_IDX5AB_PROFILE_TABLE_h_Select_RX_STATS_IDX5B  =  17; #
our $FM_L2AR_STATS_IDX5AB_PROFILE_TABLE_l_Value_RX_STATS_IDX5B  =  5; #
our $FM_L2AR_STATS_IDX5AB_PROFILE_TABLE_h_Value_RX_STATS_IDX5B  =  9; #
our $FM_L2AR_STATS_IDX5AB_PROFILE_TABLE_l_Select_RX_STATS_IDX5A  =  10; #
our $FM_L2AR_STATS_IDX5AB_PROFILE_TABLE_h_Select_RX_STATS_IDX5A  =  13; #

our $FM_L2AR_STATS_IDX5C_PROFILE_TABLE_l_Value_RX_STATS_IDX5C  =  0; #
our $FM_L2AR_STATS_IDX5C_PROFILE_TABLE_h_Value_RX_STATS_IDX5C  =  4; #
our $FM_L2AR_STATS_IDX5C_PROFILE_TABLE_l_Select_RX_STATS_IDX5C  =  5; #
our $FM_L2AR_STATS_IDX5C_PROFILE_TABLE_h_Select_RX_STATS_IDX5C  =  8; #

our $FM_L2AR_STATS_IDX12A_PROFILE_TABLE_l_Select_RX_STATS_IDX12A  =  12; #
our $FM_L2AR_STATS_IDX12A_PROFILE_TABLE_h_Select_RX_STATS_IDX12A  =  16; #
our $FM_L2AR_STATS_IDX12A_PROFILE_TABLE_l_Value_RX_STATS_IDX12A  =  0; #
our $FM_L2AR_STATS_IDX12A_PROFILE_TABLE_h_Value_RX_STATS_IDX12A  =  11; #

our $FM_L2AR_STATS_IDX12B_PROFILE_TABLE_l_Select_RX_STATS_IDX12B  =  12; #
our $FM_L2AR_STATS_IDX12B_PROFILE_TABLE_h_Select_RX_STATS_IDX12B  =  16; #
our $FM_L2AR_STATS_IDX12B_PROFILE_TABLE_l_Value_RX_STATS_IDX12B  =  0; #
our $FM_L2AR_STATS_IDX12B_PROFILE_TABLE_h_Value_RX_STATS_IDX12B  =  11; #

our $FM_L2AR_STATS_IDX16A_PROFILE_TABLE_l_Select_RX_STATS_IDX16A  =  12; #
our $FM_L2AR_STATS_IDX16A_PROFILE_TABLE_h_Select_RX_STATS_IDX16A  =  15; #
our $FM_L2AR_STATS_IDX16A_PROFILE_TABLE_l_Value_RX_STATS_IDX16A  =  0; #
our $FM_L2AR_STATS_IDX16A_PROFILE_TABLE_h_Value_RX_STATS_IDX16A  =  11; #

our $FM_L2AR_STATS_IDX16B_PROFILE_TABLE_l_Select_RX_STATS_IDX16B  =  12; #
our $FM_L2AR_STATS_IDX16B_PROFILE_TABLE_h_Select_RX_STATS_IDX16B  =  15; #
our $FM_L2AR_STATS_IDX16B_PROFILE_TABLE_l_Value_RX_STATS_IDX16B  =  0; #
our $FM_L2AR_STATS_IDX16B_PROFILE_TABLE_h_Value_RX_STATS_IDX16B  =  11; #

our $FM_L2AR_TRAP_HEADER_RULE_l_Rule              =  0; #
our $FM_L2AR_TRAP_HEADER_RULE_h_Rule              =  5; #
our $FM_L2AR_TRAP_HEADER_RULE_l_Slice             =  6; #
our $FM_L2AR_TRAP_HEADER_RULE_h_Slice             =  11; #

our $FM_L2AR_TRAP_HEADER_DATA_l_MA_WRITEBACK_TAG  =  90; #
our $FM_L2AR_TRAP_HEADER_DATA_h_MA_WRITEBACK_TAG  =  101; #
our $FM_L2AR_TRAP_HEADER_DATA_l_MA_WRITEBACK_GLORT  =  74; #
our $FM_L2AR_TRAP_HEADER_DATA_h_MA_WRITEBACK_GLORT  =  89; #
our $FM_L2AR_TRAP_HEADER_DATA_l_MA2_MAC           =  0; #
our $FM_L2AR_TRAP_HEADER_DATA_h_MA2_MAC           =  47; #
our $FM_L2AR_TRAP_HEADER_DATA_l_MA_WRITEBACK_DATA  =  102; #
our $FM_L2AR_TRAP_HEADER_DATA_h_MA_WRITEBACK_DATA  =  109; #
our $FM_L2AR_TRAP_HEADER_DATA_l_MA_WRITEBACK_PREC  =  72; #
our $FM_L2AR_TRAP_HEADER_DATA_h_MA_WRITEBACK_PREC  =  73; #
our $FM_L2AR_TRAP_HEADER_DATA_l_MA2_FID2          =  60; #
our $FM_L2AR_TRAP_HEADER_DATA_h_MA2_FID2          =  71; #
our $FM_L2AR_TRAP_HEADER_DATA_l_MA2_FID1          =  48; #
our $FM_L2AR_TRAP_HEADER_DATA_h_MA2_FID1          =  59; #

our $FM_L2AR_IP_l_Pending                         =  0; #
our $FM_L2AR_IP_h_Pending                         =  63; #

our $FM_L2AR_IM_l_Mask                            =  0; #
our $FM_L2AR_IM_h_Mask                            =  63; #

our $FM_MOD_L2_VLAN1_TX_TAGGED_l_PortMask         =  0; #
our $FM_MOD_L2_VLAN1_TX_TAGGED_h_PortMask         =  75; #

our $FM_MOD_MAP_DATA_V2T_b_W8D_Enable             =  70; #
our $FM_MOD_MAP_DATA_V2T_l_W16A_Enable            =  64; #
our $FM_MOD_MAP_DATA_V2T_h_W16A_Enable            =  65; #
our $FM_MOD_MAP_DATA_V2T_l_W16A                   =  0; #
our $FM_MOD_MAP_DATA_V2T_h_W16A                   =  15; #
our $FM_MOD_MAP_DATA_V2T_l_W16B                   =  16; #
our $FM_MOD_MAP_DATA_V2T_h_W16B                   =  31; #
our $FM_MOD_MAP_DATA_V2T_l_W8B                    =  32; #
our $FM_MOD_MAP_DATA_V2T_h_W8B                    =  39; #
our $FM_MOD_MAP_DATA_V2T_b_W8C_Enable             =  69; #
our $FM_MOD_MAP_DATA_V2T_l_W8C                    =  40; #
our $FM_MOD_MAP_DATA_V2T_h_W8C                    =  47; #
our $FM_MOD_MAP_DATA_V2T_l_W8D                    =  48; #
our $FM_MOD_MAP_DATA_V2T_h_W8D                    =  55; #
our $FM_MOD_MAP_DATA_V2T_l_W8E                    =  56; #
our $FM_MOD_MAP_DATA_V2T_h_W8E                    =  63; #
our $FM_MOD_MAP_DATA_V2T_b_W8E_Enable             =  71; #
our $FM_MOD_MAP_DATA_V2T_b_W8B_Enable             =  68; #
our $FM_MOD_MAP_DATA_V2T_l_W16B_Enable            =  66; #
our $FM_MOD_MAP_DATA_V2T_h_W16B_Enable            =  67; #

our $FM_MOD_CAM_l_Key                             =  64; #
our $FM_MOD_CAM_h_Key                             =  111; #
our $FM_MOD_CAM_l_KeyInvert                       =  0; #
our $FM_MOD_CAM_h_KeyInvert                       =  47; #

our $FM_MOD_COMMAND_RAM_b_Valid                   =  14; #
our $FM_MOD_COMMAND_RAM_l_Command                 =  0; #
our $FM_MOD_COMMAND_RAM_h_Command                 =  7; #
our $FM_MOD_COMMAND_RAM_l_Jitter                  =  8; #
our $FM_MOD_COMMAND_RAM_h_Jitter                  =  13; #

our $FM_MOD_VALUE_RAM_l_ValC_Type                 =  45; #
our $FM_MOD_VALUE_RAM_h_ValC_Type                 =  47; #
our $FM_MOD_VALUE_RAM_l_ValD_DataSelect           =  32; #
our $FM_MOD_VALUE_RAM_h_ValD_DataSelect           =  36; #
our $FM_MOD_VALUE_RAM_l_ValB_Type                 =  53; #
our $FM_MOD_VALUE_RAM_h_ValB_Type                 =  55; #
our $FM_MOD_VALUE_RAM_l_ValB_DataSelect           =  48; #
our $FM_MOD_VALUE_RAM_h_ValB_DataSelect           =  52; #
our $FM_MOD_VALUE_RAM_l_ValC_DataSelect           =  40; #
our $FM_MOD_VALUE_RAM_h_ValC_DataSelect           =  44; #
our $FM_MOD_VALUE_RAM_l_ValC_Constant             =  8; #
our $FM_MOD_VALUE_RAM_h_ValC_Constant             =  15; #
our $FM_MOD_VALUE_RAM_l_ValA_Type                 =  61; #
our $FM_MOD_VALUE_RAM_h_ValA_Type                 =  63; #
our $FM_MOD_VALUE_RAM_l_ValA_Constant             =  24; #
our $FM_MOD_VALUE_RAM_h_ValA_Constant             =  31; #
our $FM_MOD_VALUE_RAM_l_ValD_Constant             =  0; #
our $FM_MOD_VALUE_RAM_h_ValD_Constant             =  7; #
our $FM_MOD_VALUE_RAM_l_ValA_DataSelect           =  56; #
our $FM_MOD_VALUE_RAM_h_ValA_DataSelect           =  60; #
our $FM_MOD_VALUE_RAM_l_ValD_Type                 =  37; #
our $FM_MOD_VALUE_RAM_h_ValD_Type                 =  39; #
our $FM_MOD_VALUE_RAM_l_ValB_Constant             =  16; #
our $FM_MOD_VALUE_RAM_h_ValB_Constant             =  23; #

our $FM_MOD_MAP_IDX12A_l_Data                     =  0; #
our $FM_MOD_MAP_IDX12A_h_Data                     =  11; #

our $FM_MOD_MAP_DATA_W16A_l_Data                  =  0; #
our $FM_MOD_MAP_DATA_W16A_h_Data                  =  15; #

our $FM_MOD_MAP_DATA_W16B_l_Data                  =  0; #
our $FM_MOD_MAP_DATA_W16B_h_Data                  =  15; #

our $FM_MOD_MAP_DATA_W16C_l_Data                  =  0; #
our $FM_MOD_MAP_DATA_W16C_h_Data                  =  15; #

our $FM_MOD_MAP_DATA_W16D_l_Data                  =  0; #
our $FM_MOD_MAP_DATA_W16D_h_Data                  =  15; #

our $FM_MOD_MAP_DATA_W16E_l_Data                  =  0; #
our $FM_MOD_MAP_DATA_W16E_h_Data                  =  15; #

our $FM_MOD_MAP_DATA_W16F_l_Data                  =  0; #
our $FM_MOD_MAP_DATA_W16F_h_Data                  =  15; #

our $FM_MOD_MAP_DATA_W12A_l_Data                  =  0; #
our $FM_MOD_MAP_DATA_W12A_h_Data                  =  11; #

our $FM_MOD_MAP_DATA_W8A_l_Data                   =  0; #
our $FM_MOD_MAP_DATA_W8A_h_Data                   =  7; #

our $FM_MOD_MAP_DATA_W8B_l_Data                   =  0; #
our $FM_MOD_MAP_DATA_W8B_h_Data                   =  7; #

our $FM_MOD_TX_PORT_TAG_l_PAUSE_Tag               =  2; #
our $FM_MOD_TX_PORT_TAG_h_PAUSE_Tag               =  3; #
our $FM_MOD_TX_PORT_TAG_l_Tag                     =  0; #
our $FM_MOD_TX_PORT_TAG_h_Tag                     =  1; #

our $FM_MOD_DST_PORT_TAG_l_Tag                    =  0; #
our $FM_MOD_DST_PORT_TAG_h_Tag                    =  9; #

our $FM_MOD_L2_VPRI1_TX_MAP_l_VPri1_13            =  52; #
our $FM_MOD_L2_VPRI1_TX_MAP_h_VPri1_13            =  55; #
our $FM_MOD_L2_VPRI1_TX_MAP_l_VPri1_12            =  48; #
our $FM_MOD_L2_VPRI1_TX_MAP_h_VPri1_12            =  51; #
our $FM_MOD_L2_VPRI1_TX_MAP_l_VPri1_15            =  60; #
our $FM_MOD_L2_VPRI1_TX_MAP_h_VPri1_15            =  63; #
our $FM_MOD_L2_VPRI1_TX_MAP_l_VPri1_14            =  56; #
our $FM_MOD_L2_VPRI1_TX_MAP_h_VPri1_14            =  59; #
our $FM_MOD_L2_VPRI1_TX_MAP_l_VPri1_4             =  16; #
our $FM_MOD_L2_VPRI1_TX_MAP_h_VPri1_4             =  19; #
our $FM_MOD_L2_VPRI1_TX_MAP_l_VPri1_10            =  40; #
our $FM_MOD_L2_VPRI1_TX_MAP_h_VPri1_10            =  43; #
our $FM_MOD_L2_VPRI1_TX_MAP_l_VPri1_3             =  12; #
our $FM_MOD_L2_VPRI1_TX_MAP_h_VPri1_3             =  15; #
our $FM_MOD_L2_VPRI1_TX_MAP_l_VPri1_11            =  44; #
our $FM_MOD_L2_VPRI1_TX_MAP_h_VPri1_11            =  47; #
our $FM_MOD_L2_VPRI1_TX_MAP_l_VPri1_6             =  24; #
our $FM_MOD_L2_VPRI1_TX_MAP_h_VPri1_6             =  27; #
our $FM_MOD_L2_VPRI1_TX_MAP_l_VPri1_5             =  20; #
our $FM_MOD_L2_VPRI1_TX_MAP_h_VPri1_5             =  23; #
our $FM_MOD_L2_VPRI1_TX_MAP_l_VPri1_0             =  0; #
our $FM_MOD_L2_VPRI1_TX_MAP_h_VPri1_0             =  3; #
our $FM_MOD_L2_VPRI1_TX_MAP_l_VPri1_2             =  8; #
our $FM_MOD_L2_VPRI1_TX_MAP_h_VPri1_2             =  11; #
our $FM_MOD_L2_VPRI1_TX_MAP_l_VPri1_1             =  4; #
our $FM_MOD_L2_VPRI1_TX_MAP_h_VPri1_1             =  7; #
our $FM_MOD_L2_VPRI1_TX_MAP_l_VPri1_8             =  32; #
our $FM_MOD_L2_VPRI1_TX_MAP_h_VPri1_8             =  35; #
our $FM_MOD_L2_VPRI1_TX_MAP_l_VPri1_7             =  28; #
our $FM_MOD_L2_VPRI1_TX_MAP_h_VPri1_7             =  31; #
our $FM_MOD_L2_VPRI1_TX_MAP_l_VPri1_9             =  36; #
our $FM_MOD_L2_VPRI1_TX_MAP_h_VPri1_9             =  39; #

our $FM_MOD_L2_VPRI2_TX_MAP_l_VPri2_15            =  60; #
our $FM_MOD_L2_VPRI2_TX_MAP_h_VPri2_15            =  63; #
our $FM_MOD_L2_VPRI2_TX_MAP_l_VPri2_14            =  56; #
our $FM_MOD_L2_VPRI2_TX_MAP_h_VPri2_14            =  59; #
our $FM_MOD_L2_VPRI2_TX_MAP_l_VPri2_13            =  52; #
our $FM_MOD_L2_VPRI2_TX_MAP_h_VPri2_13            =  55; #
our $FM_MOD_L2_VPRI2_TX_MAP_l_VPri2_0             =  0; #
our $FM_MOD_L2_VPRI2_TX_MAP_h_VPri2_0             =  3; #
our $FM_MOD_L2_VPRI2_TX_MAP_l_VPri2_1             =  4; #
our $FM_MOD_L2_VPRI2_TX_MAP_h_VPri2_1             =  7; #
our $FM_MOD_L2_VPRI2_TX_MAP_l_VPri2_2             =  8; #
our $FM_MOD_L2_VPRI2_TX_MAP_h_VPri2_2             =  11; #
our $FM_MOD_L2_VPRI2_TX_MAP_l_VPri2_3             =  12; #
our $FM_MOD_L2_VPRI2_TX_MAP_h_VPri2_3             =  15; #
our $FM_MOD_L2_VPRI2_TX_MAP_l_VPri2_4             =  16; #
our $FM_MOD_L2_VPRI2_TX_MAP_h_VPri2_4             =  19; #
our $FM_MOD_L2_VPRI2_TX_MAP_l_VPri2_5             =  20; #
our $FM_MOD_L2_VPRI2_TX_MAP_h_VPri2_5             =  23; #
our $FM_MOD_L2_VPRI2_TX_MAP_l_VPri2_6             =  24; #
our $FM_MOD_L2_VPRI2_TX_MAP_h_VPri2_6             =  27; #
our $FM_MOD_L2_VPRI2_TX_MAP_l_VPri2_7             =  28; #
our $FM_MOD_L2_VPRI2_TX_MAP_h_VPri2_7             =  31; #
our $FM_MOD_L2_VPRI2_TX_MAP_l_VPri2_8             =  32; #
our $FM_MOD_L2_VPRI2_TX_MAP_h_VPri2_8             =  35; #
our $FM_MOD_L2_VPRI2_TX_MAP_l_VPri2_9             =  36; #
our $FM_MOD_L2_VPRI2_TX_MAP_h_VPri2_9             =  39; #
our $FM_MOD_L2_VPRI2_TX_MAP_l_VPri2_10            =  40; #
our $FM_MOD_L2_VPRI2_TX_MAP_h_VPri2_10            =  43; #
our $FM_MOD_L2_VPRI2_TX_MAP_l_VPri2_11            =  44; #
our $FM_MOD_L2_VPRI2_TX_MAP_h_VPri2_11            =  47; #
our $FM_MOD_L2_VPRI2_TX_MAP_l_VPri2_12            =  48; #
our $FM_MOD_L2_VPRI2_TX_MAP_h_VPri2_12            =  51; #

our $FM_MOD_TX_PAUSE_QUANTA_l_PAUSE_TIME2         =  16; #
our $FM_MOD_TX_PAUSE_QUANTA_h_PAUSE_TIME2         =  31; #
our $FM_MOD_TX_PAUSE_QUANTA_l_PAUSE_TIME1         =  0; #
our $FM_MOD_TX_PAUSE_QUANTA_h_PAUSE_TIME1         =  15; #

our $FM_MOD_MIN_LENGTH_l_MinLength                =  0; #
our $FM_MOD_MIN_LENGTH_h_MinLength                =  7; #

our $FM_MCAST_LOOPBACK_SUPPRESS_l_Mask            =  16; #
our $FM_MCAST_LOOPBACK_SUPPRESS_h_Mask            =  31; #
our $FM_MCAST_LOOPBACK_SUPPRESS_l_Glort           =  0; #
our $FM_MCAST_LOOPBACK_SUPPRESS_h_Glort           =  15; #

our $FM_MOD_MAP_DATA_CTRL_l_W12A_Index            =  16; #
our $FM_MOD_MAP_DATA_CTRL_h_W12A_Index            =  17; #
our $FM_MOD_MAP_DATA_CTRL_l_W8A_Index             =  18; #
our $FM_MOD_MAP_DATA_CTRL_h_W8A_Index             =  19; #
our $FM_MOD_MAP_DATA_CTRL_l_W8B_Index             =  20; #
our $FM_MOD_MAP_DATA_CTRL_h_W8B_Index             =  21; #
our $FM_MOD_MAP_DATA_CTRL_l_W16A_Index            =  0; #
our $FM_MOD_MAP_DATA_CTRL_h_W16A_Index            =  2; #
our $FM_MOD_MAP_DATA_CTRL_l_W16B_Index            =  3; #
our $FM_MOD_MAP_DATA_CTRL_h_W16B_Index            =  5; #
our $FM_MOD_MAP_DATA_CTRL_l_W16F_Index            =  14; #
our $FM_MOD_MAP_DATA_CTRL_h_W16F_Index            =  15; #
our $FM_MOD_MAP_DATA_CTRL_l_W16D_Index            =  9; #
our $FM_MOD_MAP_DATA_CTRL_h_W16D_Index            =  11; #
our $FM_MOD_MAP_DATA_CTRL_l_W16E_Index            =  12; #
our $FM_MOD_MAP_DATA_CTRL_h_W16E_Index            =  13; #
our $FM_MOD_MAP_DATA_CTRL_l_W16C_Index            =  6; #
our $FM_MOD_MAP_DATA_CTRL_h_W16C_Index            =  8; #

our $FM_MOD_MAP_DATA_V2T_CTRL_l_V2T_Index         =  0; #
our $FM_MOD_MAP_DATA_V2T_CTRL_h_V2T_Index         =  2; #
our $FM_MOD_MAP_DATA_V2T_CTRL_b_V2T_Enable        =  3; #

our $FM_MOD_TX_MIRROR_SRC_l_CanonicalPort_0       =  0; #
our $FM_MOD_TX_MIRROR_SRC_h_CanonicalPort_0       =  6; #
our $FM_MOD_TX_MIRROR_SRC_l_CanonicalPort_1       =  7; #
our $FM_MOD_TX_MIRROR_SRC_h_CanonicalPort_1       =  13; #
our $FM_MOD_TX_MIRROR_SRC_l_CanonicalPort_2       =  14; #
our $FM_MOD_TX_MIRROR_SRC_h_CanonicalPort_2       =  20; #
our $FM_MOD_TX_MIRROR_SRC_l_CanonicalPort_3       =  21; #
our $FM_MOD_TX_MIRROR_SRC_h_CanonicalPort_3       =  27; #

our $FM_MOD_TRANSMIT_MODE_b_Group40G_4            =  4; #
our $FM_MOD_TRANSMIT_MODE_b_Group40G_3            =  3; #
our $FM_MOD_TRANSMIT_MODE_b_Group40G_6            =  6; #
our $FM_MOD_TRANSMIT_MODE_b_Group40G_5            =  5; #
our $FM_MOD_TRANSMIT_MODE_b_Group40G_8            =  8; #
our $FM_MOD_TRANSMIT_MODE_b_Group40G_7            =  7; #
our $FM_MOD_TRANSMIT_MODE_b_Group40G_9            =  9; #
our $FM_MOD_TRANSMIT_MODE_b_Group40G_18           =  18; #
our $FM_MOD_TRANSMIT_MODE_b_Group40G_1            =  1; #
our $FM_MOD_TRANSMIT_MODE_b_Group40G_2            =  2; #
our $FM_MOD_TRANSMIT_MODE_b_Group40G_16           =  16; #
our $FM_MOD_TRANSMIT_MODE_b_Group40G_17           =  17; #
our $FM_MOD_TRANSMIT_MODE_b_Group40G_0            =  0; #
our $FM_MOD_TRANSMIT_MODE_b_Group40G_10           =  10; #
our $FM_MOD_TRANSMIT_MODE_b_Group40G_11           =  11; #
our $FM_MOD_TRANSMIT_MODE_b_Group40G_14           =  14; #
our $FM_MOD_TRANSMIT_MODE_b_Group40G_15           =  15; #
our $FM_MOD_TRANSMIT_MODE_b_Group40G_12           =  12; #
our $FM_MOD_TRANSMIT_MODE_b_Group40G_13           =  13; #

our $FM_NEXTHOP_TABLE_l_NextHop                   =  0; #
our $FM_NEXTHOP_TABLE_h_NextHop                   =  63; #

our $FM_L2F_TABLE_4K_l_M                          =  0; #
our $FM_L2F_TABLE_4K_h_M                          =  75; #

our $FM_L2F_TABLE_256_l_M                         =  0; #
our $FM_L2F_TABLE_256_h_M                         =  75; #

our $FM_L2F_PROFILE_TABLE_l_TableSelect           =  4; #
our $FM_L2F_PROFILE_TABLE_h_TableSelect           =  6; #
our $FM_L2F_PROFILE_TABLE_l_CmdB                  =  16; #
our $FM_L2F_PROFILE_TABLE_h_CmdB                  =  18; #
our $FM_L2F_PROFILE_TABLE_b_CmdLookup             =  11; #
our $FM_L2F_PROFILE_TABLE_l_TableID               =  7; #
our $FM_L2F_PROFILE_TABLE_h_TableID               =  10; #
our $FM_L2F_PROFILE_TABLE_b_DropCodeSelect        =  23; #
our $FM_L2F_PROFILE_TABLE_l_CmdA                  =  12; #
our $FM_L2F_PROFILE_TABLE_h_CmdA                  =  15; #
our $FM_L2F_PROFILE_TABLE_l_IndexSelect           =  0; #
our $FM_L2F_PROFILE_TABLE_h_IndexSelect           =  3; #
our $FM_L2F_PROFILE_TABLE_l_DropCode              =  19; #
our $FM_L2F_PROFILE_TABLE_h_DropCode              =  22; #

our $FM_STATS_BANK_COUNTER_l_Count                =  0; #
our $FM_STATS_BANK_COUNTER_h_Count                =  63; #

our $FM_MCAST_DEST_TABLE_l_MulticastIndex         =  76; #
our $FM_MCAST_DEST_TABLE_h_MulticastIndex         =  91; #
our $FM_MCAST_DEST_TABLE_l_DestMask               =  0; #
our $FM_MCAST_DEST_TABLE_h_DestMask               =  75; #

our $FM_MCAST_TX_MIRROR_DEST_l_ExplicitDestMask_3  =  24; #
our $FM_MCAST_TX_MIRROR_DEST_h_ExplicitDestMask_3  =  31; #
our $FM_MCAST_TX_MIRROR_DEST_l_ExplicitDestMask_2  =  16; #
our $FM_MCAST_TX_MIRROR_DEST_h_ExplicitDestMask_2  =  23; #
our $FM_MCAST_TX_MIRROR_DEST_l_ExplicitDestMask_1  =  8; #
our $FM_MCAST_TX_MIRROR_DEST_h_ExplicitDestMask_1  =  15; #
our $FM_MCAST_TX_MIRROR_DEST_l_ExplicitDestMask_0  =  0; #
our $FM_MCAST_TX_MIRROR_DEST_h_ExplicitDestMask_0  =  7; #

our $FM_MCAST_MIRROR_CFG_l_OverlayTC              =  8; #
our $FM_MCAST_MIRROR_CFG_h_OverlayTC              =  11; #
our $FM_MCAST_MIRROR_CFG_b_OverlayEnabled         =  0; #
our $FM_MCAST_MIRROR_CFG_l_CanonicalSrcPort       =  12; #
our $FM_MCAST_MIRROR_CFG_h_CanonicalSrcPort       =  18; #
our $FM_MCAST_MIRROR_CFG_l_OverlayDestPort        =  1; #
our $FM_MCAST_MIRROR_CFG_h_OverlayDestPort        =  7; #

our $FM_MCAST_TX_TRUNC_MASK_l_Mask                =  0; #
our $FM_MCAST_TX_TRUNC_MASK_h_Mask                =  7; #

our $FM_MCAST_PRIVATE_WM_l_Credits                =  0; #
our $FM_MCAST_PRIVATE_WM_h_Credits                =  13; #

our $FM_MCAST_HOG_WM_l_Credits                    =  0; #
our $FM_MCAST_HOG_WM_h_Credits                    =  13; #

our $FM_MCAST_LIMITED_SKEW_MULTICAST_l_LimitedSkew  =  0; #
our $FM_MCAST_LIMITED_SKEW_MULTICAST_h_LimitedSkew  =  11; #

our $FM_MCAST_VLAN_TABLE_l_Data                   =  0; #
our $FM_MCAST_VLAN_TABLE_h_Data                   =  14; #
our $FM_MCAST_VLAN_TABLE_b_Type                   =  15; #

our $FM_L2L_MAC_TABLE_b_Error                     =  116; #
our $FM_L2L_MAC_TABLE_l_TAG                       =  96; #
our $FM_L2L_MAC_TABLE_h_TAG                       =  107; #
our $FM_L2L_MAC_TABLE_l_DATA                      =  108; #
our $FM_L2L_MAC_TABLE_h_DATA                      =  115; #
our $FM_L2L_MAC_TABLE_l_FID1                      =  48; #
our $FM_L2L_MAC_TABLE_h_FID1                      =  59; #
our $FM_L2L_MAC_TABLE_l_MAC                       =  0; #
our $FM_L2L_MAC_TABLE_h_MAC                       =  47; #
our $FM_L2L_MAC_TABLE_l_GLORT                     =  80; #
our $FM_L2L_MAC_TABLE_h_GLORT                     =  95; #
our $FM_L2L_MAC_TABLE_l_FID2                      =  60; #
our $FM_L2L_MAC_TABLE_h_FID2                      =  71; #
our $FM_L2L_MAC_TABLE_l_Prec                      =  72; #
our $FM_L2L_MAC_TABLE_h_Prec                      =  73; #

our $FM_L2L_MAC_TABLE_SWEEPER_b_Error             =  116; #
our $FM_L2L_MAC_TABLE_SWEEPER_l_TAG               =  96; #
our $FM_L2L_MAC_TABLE_SWEEPER_h_TAG               =  107; #
our $FM_L2L_MAC_TABLE_SWEEPER_l_DATA              =  108; #
our $FM_L2L_MAC_TABLE_SWEEPER_h_DATA              =  115; #
our $FM_L2L_MAC_TABLE_SWEEPER_l_FID1              =  48; #
our $FM_L2L_MAC_TABLE_SWEEPER_h_FID1              =  59; #
our $FM_L2L_MAC_TABLE_SWEEPER_l_MAC               =  0; #
our $FM_L2L_MAC_TABLE_SWEEPER_h_MAC               =  47; #
our $FM_L2L_MAC_TABLE_SWEEPER_l_GLORT             =  80; #
our $FM_L2L_MAC_TABLE_SWEEPER_h_GLORT             =  95; #
our $FM_L2L_MAC_TABLE_SWEEPER_l_FID2              =  60; #
our $FM_L2L_MAC_TABLE_SWEEPER_h_FID2              =  71; #
our $FM_L2L_MAC_TABLE_SWEEPER_l_Prec              =  72; #
our $FM_L2L_MAC_TABLE_SWEEPER_h_Prec              =  73; #

our $FM_FFU_BST_ACTION_l_LPM                      =  24; #
our $FM_FFU_BST_ACTION_h_LPM                      =  31; #
our $FM_FFU_BST_ACTION_l_TagCmd                   =  44; #
our $FM_FFU_BST_ACTION_h_TagCmd                   =  45; #
our $FM_FFU_BST_ACTION_l_Precedence               =  47; #
our $FM_FFU_BST_ACTION_h_Precedence               =  49; #
our $FM_FFU_BST_ACTION_b_Route                    =  46; #
our $FM_FFU_BST_ACTION_l_TagData                  =  32; #
our $FM_FFU_BST_ACTION_h_TagData                  =  43; #
our $FM_FFU_BST_ACTION_l_ActionData               =  0; #
our $FM_FFU_BST_ACTION_h_ActionData               =  23; #

our $FM_FFU_BST_KEY_l_Value                       =  0; #
our $FM_FFU_BST_KEY_h_Value                       =  31; #

our $FM_FFU_BST_SCENARIO_CAM_l_Key                =  32; #
our $FM_FFU_BST_SCENARIO_CAM_h_Key                =  63; #
our $FM_FFU_BST_SCENARIO_CAM_l_KeyInvert          =  0; #
our $FM_FFU_BST_SCENARIO_CAM_h_KeyInvert          =  31; #

our $FM_FFU_BST_SCENARIO_CFG1_l_ByteMux_2         =  12; #
our $FM_FFU_BST_SCENARIO_CFG1_h_ByteMux_2         =  17; #
our $FM_FFU_BST_SCENARIO_CFG1_l_ByteMux_1         =  6; #
our $FM_FFU_BST_SCENARIO_CFG1_h_ByteMux_1         =  11; #
our $FM_FFU_BST_SCENARIO_CFG1_l_ByteMux_0         =  0; #
our $FM_FFU_BST_SCENARIO_CFG1_h_ByteMux_0         =  5; #
our $FM_FFU_BST_SCENARIO_CFG1_l_Top4Mux           =  24; #
our $FM_FFU_BST_SCENARIO_CFG1_h_Top4Mux           =  28; #
our $FM_FFU_BST_SCENARIO_CFG1_l_ByteMux_3         =  18; #
our $FM_FFU_BST_SCENARIO_CFG1_h_ByteMux_3         =  23; #
our $FM_FFU_BST_SCENARIO_CFG1_l_Reserved          =  29; #
our $FM_FFU_BST_SCENARIO_CFG1_h_Reserved          =  31; #

our $FM_FFU_BST_SCENARIO_CFG2_b_StartSet          =  28; #
our $FM_FFU_BST_SCENARIO_CFG2_b_RootKeyValid_10   =  10; #
our $FM_FFU_BST_SCENARIO_CFG2_b_StartCompare      =  26; #
our $FM_FFU_BST_SCENARIO_CFG2_b_NybbleMask_7      =  23; #
our $FM_FFU_BST_SCENARIO_CFG2_b_NybbleMask_2      =  18; #
our $FM_FFU_BST_SCENARIO_CFG2_b_NybbleMask_1      =  17; #
our $FM_FFU_BST_SCENARIO_CFG2_b_NybbleMask_0      =  16; #
our $FM_FFU_BST_SCENARIO_CFG2_b_NybbleMask_6      =  22; #
our $FM_FFU_BST_SCENARIO_CFG2_b_Reserved          =  27; #
our $FM_FFU_BST_SCENARIO_CFG2_b_NybbleMask_5      =  21; #
our $FM_FFU_BST_SCENARIO_CFG2_b_NybbleMask_4      =  20; #
our $FM_FFU_BST_SCENARIO_CFG2_b_NybbleMask_3      =  19; #
our $FM_FFU_BST_SCENARIO_CFG2_b_RootKeyValid_5    =  5; #
our $FM_FFU_BST_SCENARIO_CFG2_b_RootKeyValid_4    =  4; #
our $FM_FFU_BST_SCENARIO_CFG2_b_RootKeyValid_7    =  7; #
our $FM_FFU_BST_SCENARIO_CFG2_b_RootKeyValid_6    =  6; #
our $FM_FFU_BST_SCENARIO_CFG2_b_RootKeyValid_1    =  1; #
our $FM_FFU_BST_SCENARIO_CFG2_b_RootKeyValid_0    =  0; #
our $FM_FFU_BST_SCENARIO_CFG2_b_RootKeyValid_3    =  3; #
our $FM_FFU_BST_SCENARIO_CFG2_l_ActionLength      =  24; #
our $FM_FFU_BST_SCENARIO_CFG2_h_ActionLength      =  25; #
our $FM_FFU_BST_SCENARIO_CFG2_b_RootKeyValid_2    =  2; #
our $FM_FFU_BST_SCENARIO_CFG2_b_RootKeyValid_13   =  13; #
our $FM_FFU_BST_SCENARIO_CFG2_b_RootKeyValid_14   =  14; #
our $FM_FFU_BST_SCENARIO_CFG2_b_RootKeyValid_11   =  11; #
our $FM_FFU_BST_SCENARIO_CFG2_b_RootKeyValid_12   =  12; #
our $FM_FFU_BST_SCENARIO_CFG2_b_RootKeyValid_9    =  9; #
our $FM_FFU_BST_SCENARIO_CFG2_b_RootKeyValid_8    =  8; #
our $FM_FFU_BST_SCENARIO_CFG2_b_RootKeyValid_15   =  15; #

our $FM_FFU_BST_ROOT_KEYS_l_Partition             =  40; #
our $FM_FFU_BST_ROOT_KEYS_h_Partition             =  43; #
our $FM_FFU_BST_ROOT_KEYS_l_Top4KeyInvert         =  32; #
our $FM_FFU_BST_ROOT_KEYS_h_Top4KeyInvert         =  35; #
our $FM_FFU_BST_ROOT_KEYS_l_Top4Key               =  36; #
our $FM_FFU_BST_ROOT_KEYS_h_Top4Key               =  39; #
our $FM_FFU_BST_ROOT_KEYS_l_Key                   =  0; #
our $FM_FFU_BST_ROOT_KEYS_h_Key                   =  31; #

our $FM_FFU_BST_MASTER_VALID_b_Valid              =  0; #

our $FM_FFU_SLICE_CAM_l_Key                       =  64; #
our $FM_FFU_SLICE_CAM_h_Key                       =  101; #
our $FM_FFU_SLICE_CAM_l_KeyInvert                 =  0; #
our $FM_FFU_SLICE_CAM_h_KeyInvert                 =  37; #

our $FM_FFU_SLICE_ACTION_ROUTE_b_NextHopEntryType  =  23; #
our $FM_FFU_SLICE_ACTION_ROUTE_l_Parity           =  42; #
our $FM_FFU_SLICE_ACTION_ROUTE_h_Parity           =  43; #
our $FM_FFU_SLICE_ACTION_ROUTE_l_TagCmd           =  36; #
our $FM_FFU_SLICE_ACTION_ROUTE_h_TagCmd           =  37; #
our $FM_FFU_SLICE_ACTION_ROUTE_l_NextHopBaseIndex  =  0; #
our $FM_FFU_SLICE_ACTION_ROUTE_h_NextHopBaseIndex  =  15; #
our $FM_FFU_SLICE_ACTION_ROUTE_l_Precedence       =  39; #
our $FM_FFU_SLICE_ACTION_ROUTE_h_Precedence       =  41; #
our $FM_FFU_SLICE_ACTION_ROUTE_b_Route            =  38; #
our $FM_FFU_SLICE_ACTION_ROUTE_l_TagData          =  24; #
our $FM_FFU_SLICE_ACTION_ROUTE_h_TagData          =  35; #
our $FM_FFU_SLICE_ACTION_ROUTE_l_NextHopRange     =  16; #
our $FM_FFU_SLICE_ACTION_ROUTE_h_NextHopRange     =  22; #

our $FM_FFU_SLICE_SCENARIO_CAM_l_Key              =  32; #
our $FM_FFU_SLICE_SCENARIO_CAM_h_Key              =  63; #
our $FM_FFU_SLICE_SCENARIO_CAM_l_KeyInvert        =  0; #
our $FM_FFU_SLICE_SCENARIO_CAM_h_KeyInvert        =  31; #

our $FM_FFU_SLICE_SCENARIO_CFG_l_ByteMux_2        =  12; #
our $FM_FFU_SLICE_SCENARIO_CFG_h_ByteMux_2        =  17; #
our $FM_FFU_SLICE_SCENARIO_CFG_l_ByteMux_1        =  6; #
our $FM_FFU_SLICE_SCENARIO_CFG_h_ByteMux_1        =  11; #
our $FM_FFU_SLICE_SCENARIO_CFG_l_ByteMux_0        =  0; #
our $FM_FFU_SLICE_SCENARIO_CFG_h_ByteMux_0        =  5; #
our $FM_FFU_SLICE_SCENARIO_CFG_l_Top4Mux          =  24; #
our $FM_FFU_SLICE_SCENARIO_CFG_h_Top4Mux          =  28; #
our $FM_FFU_SLICE_SCENARIO_CFG_l_ActionLength     =  32; #
our $FM_FFU_SLICE_SCENARIO_CFG_h_ActionLength     =  33; #
our $FM_FFU_SLICE_SCENARIO_CFG_l_ByteMux_3        =  18; #
our $FM_FFU_SLICE_SCENARIO_CFG_h_ByteMux_3        =  23; #
our $FM_FFU_SLICE_SCENARIO_CFG_b_StartSet         =  30; #
our $FM_FFU_SLICE_SCENARIO_CFG_b_ValidLow         =  34; #
our $FM_FFU_SLICE_SCENARIO_CFG_b_ValidHigh        =  35; #
our $FM_FFU_SLICE_SCENARIO_CFG_l_Case             =  36; #
our $FM_FFU_SLICE_SCENARIO_CFG_h_Case             =  37; #
our $FM_FFU_SLICE_SCENARIO_CFG_b_StartCompare     =  29; #

our $FM_FFU_SLICE_MASTER_VALID_b_LookupValid      =  0; #
our $FM_FFU_SLICE_MASTER_VALID_b_ActionValid      =  1; #

our $FM_FFU_ATOMIC_APPLY_b_CAM_Slices             =  0; #
our $FM_FFU_ATOMIC_APPLY_b_BST_Slices             =  1; #

our $FM_FFU_REMAP_SCENARIO_CAM_l_Key              =  32; #
our $FM_FFU_REMAP_SCENARIO_CAM_h_Key              =  63; #
our $FM_FFU_REMAP_SCENARIO_CAM_l_KeyInvert        =  0; #
our $FM_FFU_REMAP_SCENARIO_CAM_h_KeyInvert        =  31; #

our $FM_FFU_REMAP_PROFILE_b_Select_HASH_PROFILE   =  5; #
our $FM_FFU_REMAP_PROFILE_l_Value_HASH_ROT_EXPONENT  =  22; #
our $FM_FFU_REMAP_PROFILE_h_Value_HASH_ROT_EXPONENT  =  25; #
our $FM_FFU_REMAP_PROFILE_l_Value_HASH_ROT_MANTISSA  =  6; #
our $FM_FFU_REMAP_PROFILE_h_Value_HASH_ROT_MANTISSA  =  21; #
our $FM_FFU_REMAP_PROFILE_l_Select_LABEL8B        =  3; #
our $FM_FFU_REMAP_PROFILE_h_Select_LABEL8B        =  4; #
our $FM_FFU_REMAP_PROFILE_l_Select_LABEL8A        =  1; #
our $FM_FFU_REMAP_PROFILE_h_Select_LABEL8A        =  2; #
our $FM_FFU_REMAP_PROFILE_b_Select_LABEL16        =  0; #
our $FM_FFU_REMAP_PROFILE_b_Select_HASH_ROT_EXPONENT  =  27; #
our $FM_FFU_REMAP_PROFILE_b_Select_HASH_ROT_MANTISSA  =  26; #

our $FM_FFU_EACL_CFG_l_Valid                      =  0; #
our $FM_FFU_EACL_CFG_h_Valid                      =  31; #
our $FM_FFU_EACL_CFG_l_StartList                  =  32; #
our $FM_FFU_EACL_CFG_h_StartList                  =  63; #

our $FM_HASH_LAYER3_PROFILE_l_L3_SIP_ByteMask     =  0; #
our $FM_HASH_LAYER3_PROFILE_h_L3_SIP_ByteMask     =  15; #
our $FM_HASH_LAYER3_PROFILE_l_L4_DST_BitMask      =  48; #
our $FM_HASH_LAYER3_PROFILE_h_L4_DST_BitMask      =  63; #
our $FM_HASH_LAYER3_PROFILE_b_SymmetrizeL3        =  116; #
our $FM_HASH_LAYER3_PROFILE_b_SymmetrizeL4        =  117; #
our $FM_HASH_LAYER3_PROFILE_l_L3_FLOW_BitMask     =  96; #
our $FM_HASH_LAYER3_PROFILE_h_L3_FLOW_BitMask     =  115; #
our $FM_HASH_LAYER3_PROFILE_b_ComputeHash         =  119; #
our $FM_HASH_LAYER3_PROFILE_l_ISL_USER_BitMask    =  72; #
our $FM_HASH_LAYER3_PROFILE_h_ISL_USER_BitMask    =  79; #
our $FM_HASH_LAYER3_PROFILE_l_FIELD16_ByteMask    =  88; #
our $FM_HASH_LAYER3_PROFILE_h_FIELD16_ByteMask    =  95; #
our $FM_HASH_LAYER3_PROFILE_b_UsePTable           =  118; #
our $FM_HASH_LAYER3_PROFILE_l_L3_PROT_BitMask     =  80; #
our $FM_HASH_LAYER3_PROFILE_h_L3_PROT_BitMask     =  87; #
our $FM_HASH_LAYER3_PROFILE_l_L3_DS_BitMask       =  64; #
our $FM_HASH_LAYER3_PROFILE_h_L3_DS_BitMask       =  71; #
our $FM_HASH_LAYER3_PROFILE_l_L4_SRC_BitMask      =  32; #
our $FM_HASH_LAYER3_PROFILE_h_L4_SRC_BitMask      =  47; #
our $FM_HASH_LAYER3_PROFILE_l_L3_DIP_ByteMask     =  16; #
our $FM_HASH_LAYER3_PROFILE_h_L3_DIP_ByteMask     =  31; #
our $FM_HASH_LAYER3_PROFILE_b_RandomizeNextHop    =  120; #
our $FM_HASH_LAYER3_PROFILE_b_RandomizeOther      =  121; #

our $FM_HASH_LAYER3_PTABLE_l_Value_0              =  0; #
our $FM_HASH_LAYER3_PTABLE_h_Value_0              =  5; #
our $FM_HASH_LAYER3_PTABLE_l_Value_1              =  6; #
our $FM_HASH_LAYER3_PTABLE_h_Value_1              =  11; #
our $FM_HASH_LAYER3_PTABLE_l_Value_2              =  12; #
our $FM_HASH_LAYER3_PTABLE_h_Value_2              =  17; #
our $FM_HASH_LAYER3_PTABLE_l_Value_3              =  18; #
our $FM_HASH_LAYER3_PTABLE_h_Value_3              =  23; #


1;
