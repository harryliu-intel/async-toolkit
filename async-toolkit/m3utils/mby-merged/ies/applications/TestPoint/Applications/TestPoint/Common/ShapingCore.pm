# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/ShapingCore.pm
# Creation Date:    11/20/07
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

package Applications::TestPoint::Common::ShapingCore;
use strict;
use warnings;

#Loaded when chip family is detected
#use base qw(
#    Exporter
#    Applications::TestPoint::Common::FM4000::ShapingCore
#    Applications::TestPoint::Common::FM6000::ShapingCore
#);

use List::Util qw(sum);
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

our @EXPORT = qw(
    tpHandleShapingNumGroups
    tpHandleShapingTrafficClass
    tpHandleShapingBandwidth
    tpHandleShapingCapacity
    tpHandleShapingApply
    tpHandleShowShaping
    tpHandleIngressRateLimiterBandwidth
    tpHandleIngressRateLimiterCapacity
    tpHandleIngressRateLimiterPauseThreshold
    tpHandleIngressRateLimiterDropThreshold
    tpHandleShowIngressRateLimiter
);

# Wrapper for the chip specific creation function
sub tpCreateShapingConfigInternal
{
    my ($self, $sw) = @_;
    my $chip = $self->{CHIP};

    # Get switch information from SDK
    my $info = new SDK::fm_switchInfo();
    $chip->fmGetSwitchInfo($sw, $info);
        
    if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000)
    {
        return $self->tpFM4000CreateShapingConfig();
    }
    elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
    {
        return $self->tpFM6000CreateShapingConfig();
    }
    else
    {
        return undef;
    }
}

sub tpHandleShapingApplyInternal
{
    my ($self, $sw, $logPort, $port) = @_;
    my $chip = $self->{CHIP};

    # Get switch information from SDK
    my $info = new SDK::fm_switchInfo();
    $chip->fmGetSwitchInfo($sw, $info);
        
    if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000)
    {
        return $self->tpFM4000HandleShapingApply($sw, $logPort, $port);
    }
    elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
    {
        return $self->tpFM6000HandleShapingApply($sw, $logPort, $port);
    }
    else
    {
        return undef;
    }
}

sub tpHandleShowShapingInternal
{
    my ($self, $sw, $logPort, $port, $config) = @_;
    my $chip = $self->{CHIP};

    # Get switch information from SDK
    my $info = new SDK::fm_switchInfo();
    $chip->fmGetSwitchInfo($sw, $info);
        
    if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000)
    {
        return $self->tpFM4000HandleShowShaping($sw, $logPort, $port, $config);
    }
    elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
    {
        return $self->tpFM6000HandleShowShaping($sw, $logPort, $port, $config);
    }
    else
    {
        return undef;
    }
}

##@method void tpHandleShapingNumGroups
# Configure the number of shaping groups
#
sub tpHandleShapingNumGroups
{
    my ($self, $port, $numGroups) = @_;
    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_FAIL;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);
            
        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
        {
            next;
        }

        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            
            if(not defined $self->{SHAPING}->{$sw}->{$logPort})
            {
                $self->{SHAPING}->{$sw}->{$logPort} = $self->tpCreateShapingConfigInternal($sw);
            }

            $self->{SHAPING}->{$sw}->{$logPort}->{num_groups} = $numGroups; 
        }
    }
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $FM_OK;

}

##@method void tpHandleShapingTrafficClass
# Assign TC to shaping groups
#
sub tpHandleShapingTrafficClass
{
    my ($self, $port, $group_num, $tc) = @_;
    my $chip = $self->{CHIP};
    my @tcList;
    my $maxTc;

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_FAIL;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);
            
        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
        {
            $maxTc = 12;
        }
        else
        {
            $maxTc = 8;
        }

        @tcList = $self->validateList($tc, 0, $maxTc - 1);
        if(!defined($tc) || scalar(@tcList) == 0)
        {
            printf("Invalid traffic class(es) $tc, please provide a number between 0 and $maxTc\n");
            return $FM_FAIL;
        }
    
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            
            if(not defined $self->{SHAPING}->{$sw}->{$logPort})
            {
                $self->{SHAPING}->{$sw}->{$logPort} = $self->tpCreateShapingConfigInternal($sw);
            }

            my @groupList = $self->validateList($group_num, 0,
                                $self->{SHAPING}->{$sw}->{$logPort}->{num_groups}-1,
                                $FALSE);


            if(scalar(@groupList) == 0)
            {
                printf("Group(s) $group_num not valid on port $port, switch $sw\n");
                next;
            }

            foreach my $g (@groupList)
            {
                foreach my $t (@tcList)
                {
                    # 2) find and remove the tc from any existing groups
                    for ( my $i = 0; $i < $maxTc ; $i++ )
                    {
                        for ( my $j = 0; 
                              $j < scalar(@{$self->{SHAPING}->{$sw}->{$logPort}->{tc}->[$i]}) ;
                              $j++ )
                        {
                            if($self->{SHAPING}->{$sw}->{$logPort}->{tc}->[$i]->[$j] == $t)
                            {
                                splice(@{$self->{SHAPING}->{$sw}->{$logPort}->{tc}->[$i]}, $j, 1);
                            } 
                        }
                    }
    
                    # 3) Add the tc to the specified group number
                    push(@{$self->{SHAPING}->{$sw}->{$logPort}->{tc}->[$g]},$t); 
    
                }   # for each tc
            }   # for each group
        }   # for each port
    }   # for each switch
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $FM_OK;

}

##@method void tpHandleShapingBandwidth
# Set the bandwidth of a shaping group
#
sub tpHandleShapingBandwidth
{
    my ($self, $port, $group_num, $bw) = @_;
    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_FAIL;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            
            if(not defined $self->{SHAPING}->{$sw}->{$logPort})
            {
                $self->{SHAPING}->{$sw}->{$logPort} = $self->tpCreateShapingConfigInternal($sw);
            }

            my @groupList = $self->validateList($group_num, 0,
                                $self->{SHAPING}->{$sw}->{$logPort}->{num_groups}-1);

            if(scalar(@groupList) == 0)
            {
                printf("Group(s) $group_num not valid on port $port, switch $sw\n");
                next;
            }

            foreach my $g (@groupList)
            {
                $self->{SHAPING}->{$sw}->{$logPort}->{bandwidth}->[$g] = $bw;
            }
        }
    }
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $FM_OK;
    
}

##@method void tpHandleShapingCapacity
# Set the shaping groups capacity
#
sub tpHandleShapingCapacity
{
    my ($self, $port, $group_num, $capacity) = @_;
    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_FAIL;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            
            if(not defined $self->{SHAPING}->{$sw}->{$logPort})
            {
                $self->{SHAPING}->{$sw}->{$logPort} = $self->tpCreateShapingConfigInternal($sw);
            }

            my @groupList = $self->validateList($group_num, 0,
                                $self->{SHAPING}->{$sw}->{$logPort}->{num_groups}-1);

            if(scalar(@groupList) == 0)
            {
                printf("Group(s) $group_num not valid on port $port, switch $sw\n");
                next;
            }

            foreach my $g (@groupList)
            {
                $self->{SHAPING}->{$sw}->{$logPort}->{capacity}->[$g] = $capacity;
            }
        }
    }
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $FM_OK;

}

##@method void tpHandleShapingApply
# Make a scheduler group higher priority
#
sub tpHandleShapingApply
{
    my ($self, $port) = @_;
    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            
            $self->tpHandleShapingApplyInternal($sw, $logPort, $port);
        }
    }
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    
}


sub tpHandleShowShaping
{
    my ($self, $config, $port) = @_;

    if(!defined $config || 
        !($config eq "active" || $config eq "new"))
    {
        printf("Unknown argument $config\n");
        return;
    }

    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        if ($switchCount > 1)
        {
            printf("Switch $sw:\n");
        }

        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            $self->tpHandleShowShapingInternal($sw, $logPort, $port, $config);
        }
    }
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);

}

##@method void tpHandleIngressRateLimiterBandwidth
# Set the bandwidth of an ingress rate limiter
#
sub tpHandleIngressRateLimiterBandwidth
{
    my ($self, $port, $smp_num, $bw) = @_;
    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_FAIL;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));

        if (scalar(@portList) == 0)
        {
            next;
        }

        push(@affectedPortList, @portList);

        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);

            my @smpList = $self->validateList($smp_num, 0, 1);

            if(scalar(@smpList) == 0)
            {
                printf("smp not valid on port $port, switch $switchNum\n");
                next;
            }

            foreach my $smp (@smpList)
            {
                $self->tpFM4000HandleIngressRateLimiterBandwidth($switchNum, $logPort, $smp, $bw);
            }

        }
    }

    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $FM_OK;
 
}

##@method void tpHandleShapingCapacity
# Set the ingress rate limiter capacity
#
sub tpHandleIngressRateLimiterCapacity
{
    my ($self, $port, $smp_num, $capacity) = @_;
    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_FAIL;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));

        if (scalar(@portList) == 0)
        {
            next;
        }

        push(@affectedPortList, @portList);

        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);

            my @smpList = $self->validateList($smp_num, 0, 1);

            if(scalar(@smpList) == 0)
            {
                printf("smp not valid on port $port, switch $switchNum\n");
                next;
            }

            foreach my $smp (@smpList)
            {
                $self->tpFM4000HandleIngressRateLimiterCapacity($switchNum, $logPort, $smp, $capacity);
            }
        }
    }

    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $FM_OK;

}

##@method void tpHandleIngressRateLimiterPauseThreshold
# Set the ingress rate limiter tocken bucket level at which
# pause off will be sent
#
sub tpHandleIngressRateLimiterPauseThreshold
{
    my ($self, $port, $smp_num, $threshold) = @_;
    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_FAIL;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));

        if (scalar(@portList) == 0)
        {
            next;
        }

        push(@affectedPortList, @portList);

        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);

            my @smpList = $self->validateList($smp_num, 0, 1);

            if(scalar(@smpList) == 0)
            {
                printf("smp not valid on port $port, switch $switchNum\n");
                next;
            }

            foreach my $smp (@smpList)
            {
                $self->tpFM4000HandleIngressRateLimiterPauseThreshold($switchNum, $logPort, $smp, $threshold);
            }
        }
    }

    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $FM_OK;

}

##@method void tpHandleIngressRateLimiterDropThreshold
# Set the ingress rate limiter tocken bucket level at which
# frames will start to get dropped
#
sub tpHandleIngressRateLimiterDropThreshold
{
    my ($self, $port, $smp_num, $threshold) = @_;
    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_FAIL;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));

        if (scalar(@portList) == 0)
        {
            next;
        }

        push(@affectedPortList, @portList);

        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);

            my @smpList = $self->validateList($smp_num, 0, 1);

            if(scalar(@smpList) == 0)
            {
                printf("smp not valid on port $port, switch $switchNum\n");
                next;
            }

            foreach my $smp (@smpList)
            {
                $self->tpFM4000HandleIngressRateLimiterDropThreshold($switchNum, $logPort, $smp, $threshold);
            }
        }
    }

    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $FM_OK;

}

##@method void tpHandleShowIngressPortLimiter
# Show the ingress rate limiter configuration for n ports
#
sub tpHandleShowIngressRateLimiter
{
    my ($self, $port) = @_;
    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));

        if (scalar(@portList) == 0)
        {
            next;
        }

        push(@affectedPortList, @portList);

        if ($switchCount > 1)
        {
            printf("Switch $sw:\n");
        }

        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            $self->tpFM4000HandleShowIngressRateLimiter($sw, $logPort, $port);
        }
    }

    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);


    return $FM_OK;

}

1;
