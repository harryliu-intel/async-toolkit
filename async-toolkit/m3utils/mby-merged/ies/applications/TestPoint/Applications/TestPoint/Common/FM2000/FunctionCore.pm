# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FM2000/FunctionCore.pm
# Creation Date:    June. 6, 2011
# Description:      Handles FM2000 specific code
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

package Applications::TestPoint::Common::FM2000::FunctionCore;
use strict;
use warnings;

use base qw(
    Exporter
);

use SDK qw(/^\$/);




my %fm2000_port_attr_map = (
    'security_pri'      => $FM_PORT_SECURITY_PRIORITY,
    'ether_type'        => $FM_PORT_VLAN_ETHER_TYPE,
    'store_and_fwd'     => $FM_PORT_SAF,
);

my %fm2000_sw_attr_map = (
    'stats_config'      => $FM_STAT_GROUP_ENABLE,
);

my %fm2000_sw_trap_attr_map = (
    'trap_ether'        => $FM_TRAP_ETH_TYPE,
);




##@cmethod public FM2000::FunctionCore& new()
#
# @desc         Instantiates a Perl
#               Applications::TestPoint::Common::FM2000::FunctionCore object
#
# @return       A Perl reference to an instantiated
#               Applications::TestPoint::Common::FM2000::FunctionCore object
sub new
{
    my $class = shift;
    my $self = {};
    
    bless $self, $class;
    return $self;
}




##@cmethod public fcGetPortAttributes(void)
#
# @desc         Returns a list of port attributes that are FM2000 specific
#               
# @return       A hash reference to a list of port attributes
sub fcGetPortAttributes
{
    return \%fm2000_port_attr_map;
}




##@cmethod public fcGetSwitchAttributes(void)
#
# @desc         Returns a list of switch attributes that are FM2000 specific
#               
# @return       A hash reference to a list of switch attributes
sub fcGetSwitchAttributes
{
    return \%fm2000_sw_attr_map;
}




##@cmethod public fcGetTrappingAttributes(void)
#
# @desc         Returns a list of switch trapping attributes that are 
#               FM2000 specific
#               
# @return       A hash reference to a list of trapping attributes
sub fcGetTrappingAttributes
{
    return \%fm2000_sw_trap_attr_map;
}

1;

