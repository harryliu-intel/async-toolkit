# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/LoadBalancingCore.pm
# Creation Date:    11/29/2007
# Description:      TestPoint commands for load-balancing groups.
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

package Applications::TestPoint::Common::LoadBalancingCore;
use strict;
use warnings;

#Loaded when chip family is detected
#use base qw(
#    Applications::TestPoint::Common::FM4000::LoadBalancingCore
#);

use List::Util qw(sum);
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;


sub tpHandleSetLoadBalancing
{
    my ($self, @args) = @_;
    my $chip = $self->{CHIP};

    print "tpHandleSetLoadBalancing\n";

    my ($firstSwitch, $lastSwitch) = $self->tpPlatformGetSwitchRange();

    if ($firstSwitch != $lastSwitch)
    {
        print "multiswitch system not supported\n";
        return $FM_FAIL;
    }

    my $resp = $FM_OK;
    for (my $sw = $firstSwitch; $sw <= $lastSwitch; $sw++)
    {
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000)
        {
            my $ret = $self->tpFM4000HandleSetLoadBalancing(@args);
            if($ret != $FM_OK && $resp == $FM_OK)
            {
                $resp = $ret;
            }
        }
        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000)
        {
            #TODO ???
        }
    }
    return $resp;

}   # end tpHandleSetLoadBalancing


sub tpHandleShowLoadBalancing
{
    my ($self, @args) = @_;
    my $chip = $self->{CHIP};

    print "tpHandleLoadBalancing\n";

    my ($firstSwitch, $lastSwitch) = $self->tpPlatformGetSwitchRange();

    if ($firstSwitch != $lastSwitch)
    {
        print "multiswitch system not supported\n";
        return $FM_FAIL;
    }

    my $resp = $FM_OK;
    for (my $sw = $firstSwitch; $sw <= $lastSwitch; $sw++)
    {
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000)
        {
            my $ret = $self->tpFM4000HandleShowLoadBalancing(@args);
            if($ret != $FM_OK && $resp == $FM_OK)
            {
                $resp = $ret;
            }
        }
        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000)
        {
            #TODO ???
        }
    }
    return $resp;

}   # end tpHandleShowLoadBalancing

1;
