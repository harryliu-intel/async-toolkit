# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FM6000/FunctionCore.pm
# Creation Date:    June. 6, 2011
# Description:      Handles FM6000 specific code
#
# INTEL CONFIDENTIAL
# Copyright 2011 - 2012 Intel Corporation. All Rights Reserved. 
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

package Applications::TestPoint::Common::FM6000::FunctionCore;
use strict;
use warnings;

use base qw(
    Exporter
);

use SDKScalars;




my %fm6000_port_attr_map = (
    'capture_tcp_flags'      => $FM_PORT_CAPTURE_TCP_FLAGS,
    'capture_l4_entry'       => $FM_PORT_CAPTURE_L4_ENTRY,
    'active_mac'             => $FM_PORT_SELECT_ACTIVE_MAC,
    'autoneg_inhb_timer'     => $FM_PORT_AUTONEG_LINK_INHB_TIMER,
    'autoneg_inhb_timer_kx'  => $FM_PORT_AUTONEG_LINK_INHB_TIMER_KX,
    'autoneg_nextpages'      => $FM_PORT_AUTONEG_NEXTPAGES,
    'autoneg_partner_nxtpgs' => $FM_PORT_AUTONEG_PARTNER_NEXTPAGES,
    'autoneg_ignore_nonce'   => $FM_PORT_AUTONEG_IGNORE_NONCE,
    'bcast_flooding'         => $FM_PORT_BCAST_FLOODING,
    'cfi2'                   => $FM_PORT_DEF_CFI2,
    'check_vid2_learning'    => $FM_PORT_CHECK_LEARNING_VID2,
    'cjpat'                  => $FM_PORT_TRANSMIT_CJPAT,
    'dfe_mode'               => $FM_PORT_DFE_MODE,
    'dfe_params'             => $FM_PORT_DFE_PARAMETERS,
    'di_parsing'             => $FM_PORT_DI_PARSING,
    'drop_smac_err'          => $FM_PORT_DROP_SMAC_ERR,
    'eth_mode'               => $FM_PORT_ETHERNET_INTERFACE_MODE,
    'pc_map'                 => -1,          # multiple values to select between
    'pri2'                   => $FM_PORT_DEF_PRI2,
    'pvid2'                  => $FM_PORT_DEF_VLAN2,
    'isl_user'               => $FM_PORT_DEF_ISL_USER,
    'shaper_ifg_penalty'     => $FM_QOS_SHAPING_GROUP_IFG_PENALTY,
    'signal_threshold'       => $FM_PORT_SIGNAL_THRESHOLD,
    'slew'                   => $FM_PORT_SLEW_RATE,
    'tc_map'                 => -1,          # multiple values to select between
    'tx_pad_size'            => $FM_PORT_TX_PAD_SIZE,
    'up_debounce_time'       => $FM_PORT_UP_DEBOUNCE_TIME,
    'down_debounce_time'     => $FM_PORT_DOWN_DEBOUNCE_TIME,
    'low_eyescore_mode'      => $FM_PORT_LOW_EYE_SCORE_RECOVERY,
    'low_eyescore_threshold' => $FM_PORT_LOW_EYE_SCORE_THRESHOLD,
    'low_eyescore_timeout'   => $FM_PORT_LOW_EYE_SCORE_TIMEOUT,
    'trap_bpdu'              => $FM_PORT_TRAP_IEEE_BPDU,
    'trap_garp'              => $FM_PORT_TRAP_IEEE_GARP,
    'trap_lacp'              => $FM_PORT_TRAP_IEEE_LACP,
    'trap_other'             => $FM_PORT_TRAP_IEEE_OTHER,
    'trap_security'          => $FM_PORT_TRAP_IEEE_8021X,
    'pri2_from_pri1'         => $FM_PORT_VPRI2_FROM_VPRI1,
    'pri1_from_pri2'         => $FM_PORT_VPRI1_FROM_VPRI2,
    'trill_hop_count'        => 0,
    'trill_parsing'          => $FM_PORT_PARSE_TRILL,
    'trill_tagging'          => $FM_PORT_EGRESS_RBRIDGE_TAG,
    'trill_vlan_tagging'     => $FM_PORT_EGRESS_RBRIDGE_VLAN_TAG,
    'vlan_translation'       => $FM_PORT_APPLY_VLAN_TRANSLATION,
    'select_vid2'            => $FM_PORT_SWITCHING_SELECT_VID2,
    'select_vid2_next'       => $FM_PORT_SWITCHING_NEXTHOP_VID,
    'timestamp_generate'     => $FM_PORT_TIMESTAMP_GENERATION,
    'timestamp_events'       => $FM_PORT_EGRESS_TIMESTAMP_EVENTS,
    'tx_fcs_mode'            => $FM_PORT_TX_FCS_MODE,
);

my %fm6000_sw_attr_map = (
    'di_l2_start_index'  => $FM_SWITCH_DI_L2_START_INDEX,
    'di_l4_start_index'  => $FM_SWITCH_DI_L4_START_INDEX,
    'eth_mode'           => $FM_PORT_ETHERNET_INTERFACE_MODE,
    'learn_drop_frame'   => $FM_LEARN_DROP_FRAME,
    'mirror_counters'    => $FM_MIRROR_COUNTERS,
    'shared-fid'         => $FM_VLAN_LEARNING_SHARED_VLAN,
    'stats_config'       => $FM_STAT_GROUP_ENABLE,
    'vlan-learning'      => $FM_VLAN_LEARNING_MODE,
    'vlan1_eth_a'        => $FM_SWITCH_VLAN1_ETHERTYPE_A,
    'vlan1_eth_b'        => $FM_SWITCH_VLAN1_ETHERTYPE_B,
    'vlan2_eth_a'        => $FM_SWITCH_VLAN2_ETHERTYPE_A,
    'vlan2_eth_b'        => $FM_SWITCH_VLAN2_ETHERTYPE_B,
    'pol_bank_drop_mask' => $FM_POLICER_BANK_DROP_MASK,
    'rbridge_nick'       => $FM_SWITCH_RBRIDGE_NICKNAME,
    'rbridge_mac'        => $FM_SWITCH_RBRIDGE_MAC,
    'rbridge_eth'        => $FM_SWITCH_RBRIDGE_ETHERNET_TYPE,
    'drop_mtu_violation' => $FM_DROP_MTU_VIOLATIONS,
);

my %fm6000_sw_trap_attr_map = (
    'trap_cpu_mac'      => $FM_CPU_MAC_TRAP_DISP,
);




##@cmethod public FM6000::FunctionCore& new()
#
# @desc         Instantiates a Perl
#               Applications::TestPoint::Common::FM6000::FunctionCore object
#
# @return       A Perl reference to an instantiated
#               Applications::TestPoint::Common::FM6000::FunctionCore object
sub new
{
    my $class = shift;
    my $self = {};
    
    bless $self, $class;
    return $self;
}




##@cmethod public fcGetPortAttributes(void)
#
# @desc         Returns a list of port attributes that are FM6000 specific
#               
# @return       A hash reference to a list of port attributes
sub fcGetPortAttributes
{
    return \%fm6000_port_attr_map;
}




##@cmethod public fcGetSwitchAttributes(void)
#
# @desc         Returns a list of switch attributes that are FM6000 specific
#               
# @return       A hash reference to a list of switch attributes
sub fcGetSwitchAttributes
{
    return \%fm6000_sw_attr_map;
}

##@cmethod public fcGetTrappingAttributes(void)
#
# @desc         Returns a list of switch trapping attributes that are 
#               FM6000 specific
#               
# @return       A hash reference to a list of trapping attributes
sub fcGetTrappingAttributes
{
    return \%fm6000_sw_trap_attr_map;
}

1;

