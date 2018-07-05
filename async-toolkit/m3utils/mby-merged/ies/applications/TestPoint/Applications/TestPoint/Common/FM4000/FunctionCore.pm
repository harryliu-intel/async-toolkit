# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FM4000/FunctionCore.pm
# Creation Date:    June. 6, 2011
# Description:      Handles FM4000 specific code
#
# INTEL CONFIDENTIAL
# Copyright 2011  Intel Corporation. All Rights Reserved. 
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

package Applications::TestPoint::Common::FM4000::FunctionCore;
use strict;
use warnings;

use base qw(
    Exporter
);

use SDKScalars;




my %fm4000_port_attr_map = (
    'capture_tcp_flags' => $FM_PORT_CAPTURE_TCP_FLAGS,
    'deep_protocol_a'   => $FM_PORT_PARSER_PROTOCOL_A,
    'deep_protocol_b'   => $FM_PORT_PARSER_PROTOCOL_B,
    'l3_deep_bytes'     => $FM_PORT_PARSER_NOT_IP_PAYLOAD,
    'l4_deep_bytes'     => $FM_PORT_PARSER_UNKNOWN_L4_PAYLOAD,
    'parse_non_ip'      => $FM_PORT_PARSER_ENABLE_NOT_IP,
    'parse_tcp_payload' => $FM_PORT_PARSER_TCP_PAYLOAD,
    'parse_udp_payload' => $FM_PORT_PARSER_UDP_PAYLOAD,
    'parse_a_payload'   => $FM_PORT_PARSER_PROT_A_PAYLOAD,
    'parse_b_payload'   => $FM_PORT_PARSER_PROT_B_PAYLOAD,
    'stats_config'      => $FM_PORT_STAT_GROUP_ENABLE,
    'store_and_fwd'     => $FM_PORT_SAF,
    'vlan_tag_a'        => $FM_PORT_PARSER_VLAN_TAG_A,
    'vlan_tag_b'        => $FM_PORT_PARSER_VLAN_TAG_B,
);

my %fm4000_sw_attr_map = (
    
);

my %fm4000_sw_trap_attr_map = (
    
);




##@cmethod public FM4000::FunctionCore& new()
#
# @desc         Instantiates a Perl
#               Applications::TestPoint::Common::FM4000::FunctionCore object
#
# @return       A Perl reference to an instantiated
#               Applications::TestPoint::Common::FM4000::FunctionCore object
sub new
{
    my $class = shift;
    my $self = {};
    
    bless $self, $class;
    return $self;
}




##@cmethod public fcGetPortAttributes(void)
#
# @desc         Returns a list of port attributes that are FM4000 specific
#               
# @return       A hash reference to a list of port attributes
sub fcGetPortAttributes
{
    return \%fm4000_port_attr_map;
}




##@cmethod public fcGetSwitchAttributes(void)
#
# @desc         Returns a list of switch attributes that are FM4000 specific
#               
# @return       A hash reference to a list of switch attributes
sub fcGetSwitchAttributes
{
    return \%fm4000_sw_attr_map;
}




##@cmethod public fcGetTrappingAttributes(void)
#
# @desc         Returns a list of switch trapping attributes that are 
#               FM4000 specific
#               
# @return       A hash reference to a list of trapping attributes
sub fcGetTrappingAttributes
{
    return \%fm4000_sw_trap_attr_map;
}

1;

