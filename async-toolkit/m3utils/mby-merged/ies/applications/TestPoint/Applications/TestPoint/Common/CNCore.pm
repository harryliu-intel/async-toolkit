# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/CNCore.pm
# Creation Date:    11/20/2007
# Description:      Congestion Notification TestPoint Interfaces
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

package Applications::TestPoint::Common::CNCore;
use strict;
use warnings;

#Dynamically loaded at chip family detection
#use base qw(
#    Applications::TestPoint::Common::FM4000::CNCore
#);

use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;


##@cmethod public void tpHandleSetCN()
# @brief        Set global congestion notification properties
# 
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleSetCN
{
    my ($self, @args) = @_;
    my $chip = $self->{CHIP};
    my $status = $FM_OK;

    print "tpHandleSetCN\n";

    foreach my $sw ($self->tpGetSwitches)
    {
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);
    
        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000)
        {
            $status = $self->tpFM4000HandleSetCN(@args);
        }
        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000)
        {
            print "feature not supported for FM2000\n";
        }
    }
    
    return $status;
}

##@cmethod public void tpHandleShowCN
# @brief        Show global congestion notification properties
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleShowCN
{
    my ($self, @args) = @_;
    my $chip = $self->{CHIP};
    my $status = $FM_OK;

    print "tpHandleShowCN\n";

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $sw ($self->tpGetSwitches)
    {
        if ($switchCount > 1)
        {
            print "Switch $sw:\n";
        }
        
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);
    
        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000)
        {
            $status = $self->tpFM4000HandleShowCN(@args);
        }
        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000)
        {
            print "feature not supported for FM2000\n";
        }
    }
    
    return $status;
}


1;
