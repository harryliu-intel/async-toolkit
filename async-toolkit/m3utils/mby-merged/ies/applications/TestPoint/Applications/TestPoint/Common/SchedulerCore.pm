# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/SchedulerCore.pm
# Creation Date:    10/30/07
# Description:      
#
# INTEL CONFIDENTIAL
# Copyright 2007 - 2012 Intel Corporation. All Rights Reserved. 
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

package Applications::TestPoint::Common::SchedulerCore;
use strict;
use warnings;

#Loaded when chip family is detected
#use base qw(
#    Exporter
#    Applications::TestPoint::Common::FM4000::SchedulerCore
#    Applications::TestPoint::Common::FM6000::SchedulerCore
#);

use List::Util qw(sum);
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

our @EXPORT = qw(
    tpHandleSchedNumGroups
    tpHandleSchedTrafficClass
    tpHandleSchedStrict
    tpHandleSchedPriority
    tpHandleSchedWeight
    tpHandleSchedApply
    tpHandleShowSched
);


##@method void tpHandleSchedNumGroups
# Configure the number of scheduler groups
#
sub tpHandleSchedNumGroups
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

    if(!defined($numGroups))
    {
        print("Invalid argument. Must specify the number of scheduler groups!\n"); 
        return $FM_FAIL;
    }
    
    if ($numGroups == 0)
    {
        print("Invalid number of scheduler groups 0!\n");
        return $FM_FAIL;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));
        my $maxNumGroups = 0;
        
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);

        if (scalar(@portList) == 0)
        {
            next;
        }

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000 ||
            $info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
        {
            $maxNumGroups = 8;
        }
        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
        {
            $maxNumGroups = 12; 
        }

        if ($numGroups > $maxNumGroups)
        {
            print("Invalid number of scheduler groups $numGroups!\n");
            return $FM_FAIL;
        }
    
        push(@affectedPortList, @portList);

        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            
            if(not defined $self->{SCHEDULER}->{$sw}->{$logPort})
            {
                if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000 ||
                    $info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
                {
                    $self->{SCHEDULER}->{$sw}->{$logPort} =
                                    $self->tpFM4000CreateSchedulerConfig();
                }
                elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
                {
                    $self->{SCHEDULER}->{$sw}->{$logPort} =
                                    $self->tpFM6000CreateSchedulerConfig();
 
                }
            }

            $self->{SCHEDULER}->{$sw}->{$logPort}->{num_groups} = $numGroups; 
        }
    }
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $FM_OK
}

##@method void tpHandleSchedTrafficClass
# Assign TC to scheduler groups
#
sub tpHandleSchedTrafficClass
{
    my ($self, $port, $group_num, $tc) = @_;
    my $chip = $self->{CHIP};

    if(!defined($tc))
    {
        printf("Invalid traffic class(es) $tc\n");
        return $FM_FAIL;
    }
    
    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_FAIL;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my $tcMax = 7;
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
        {
            $tcMax = 11;
        }

        my @tcList = $self->validateList($tc, 0, $tcMax);
        if(scalar(@tcList) == 0)
        {
            printf("Invalid traffic class(es) $tc, please provide a number
between 0 and $tcMax \n");
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
            
            if(not defined $self->{SCHEDULER}->{$sw}->{$logPort})
            {
                if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000 ||
                    $info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
                {
                    $self->{SCHEDULER}->{$sw}->{$logPort} =
                                    $self->tpFM4000CreateSchedulerConfig();
                }
                elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
                {
                    $self->{SCHEDULER}->{$sw}->{$logPort} =
                                    $self->tpFM6000CreateSchedulerConfig();
                }

            }

            my @groupList = $self->validateList($group_num, 0,
                                $self->{SCHEDULER}->{$sw}->{$logPort}->{num_groups}-1,
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
                    # Find and remove the tc from any existing groups
                    for ( my $i = 0; $i < $tcMax + 1 ; $i++ )
                    {
                        for ( my $j = 0; 
                              $j < scalar(@{$self->{SCHEDULER}->{$sw}->{$logPort}->{tc}->[$i]}) ;
                              $j++ )
                        {
                            if($self->{SCHEDULER}->{$sw}->{$logPort}->{tc}->[$i]->[$j] == $t)
                            {
                                splice(@{$self->{SCHEDULER}->{$sw}->{$logPort}->{tc}->[$i]},
                                    $j,1);
                            } 
                        }
                    }
    
                    # Add the tc to the specified group number
                    push(@{$self->{SCHEDULER}->{$sw}->{$logPort}->{tc}->[$g]},$t); 
    
                }   # for each tc
            }   # for each group
        }   # for each port
    }   # for each switch
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $FM_OK;
    
}

##@method void tpHandleSchedStrict
# Make a scheduler group strict
#
sub tpHandleSchedStrict
{
    my ($self, $port, $group_num, $strict) = @_;
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
            
            if(not defined $self->{SCHEDULER}->{$sw}->{$logPort})
            {
                # Get switch information from SDK
                my $info = new SDK::fm_switchInfo();
                $chip->fmGetSwitchInfo(0, $info);

                if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000 ||
                    $info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
                {
                    $self->{SCHEDULER}->{$sw}->{$logPort} =
                                    $self->tpFM4000CreateSchedulerConfig();
                }
                elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
                {
                    $self->{SCHEDULER}->{$sw}->{$logPort} =
                                    $self->tpFM6000CreateSchedulerConfig();
                }
            }

            my @groupList = $self->validateList($group_num, 0,
                                $self->{SCHEDULER}->{$sw}->{$logPort}->{num_groups}-1);

            if(scalar(@groupList) == 0)
            {
                printf("Group(s) $group_num not valid on port $port, switch $sw\n");
                next;
            }

            foreach my $g (@groupList)
            {
                my $strict_bit = ( lc($strict) eq "on" ) ? 1 : 0;
                                   
                $self->{SCHEDULER}->{$sw}->{$logPort}->{strict} &= ~(1 << $g);
                $self->{SCHEDULER}->{$sw}->{$logPort}->{strict} |= ($strict_bit << $g);

            }
        }
    }
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $FM_OK;

}

##@method void tpHandleSchedPriority
# Make a scheduler group higher priority
#
sub tpHandleSchedPriority
{
    my ($self, $port, $group_num, $priority) = @_;
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
            
            if(not defined $self->{SCHEDULER}->{$sw}->{$logPort})
            {
                # Get switch information from SDK
                my $info = new SDK::fm_switchInfo();
                $chip->fmGetSwitchInfo(0, $info);

                if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000 ||
                    $info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
                {
                    $self->{SCHEDULER}->{$sw}->{$logPort} =
                                    $self->tpFM4000CreateSchedulerConfig();
                }
                elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
                {
                    $self->{SCHEDULER}->{$sw}->{$logPort} =
                                    $self->tpFM6000CreateSchedulerConfig();
                }

            }

            my @groupList = $self->validateList($group_num, 0,
                                $self->{SCHEDULER}->{$sw}->{$logPort}->{num_groups}-1,
                                $FALSE);

            my $maxPriority = $self->{SCHEDULER}->{$sw}->{$logPort}->{num_groups} - 1;
            if($priority < 0 || $priority > $maxPriority)
            {
                printf("Priority must be 0 to $maxPriority on port $port, switch $sw\n");
                next;
            }

            if(scalar(@groupList) == 0)
            {
                printf("Group(s) $group_num not valid on port $port, switch $sw\n");
                next;
            }

            foreach my $g (@groupList)
            {
                $self->{SCHEDULER}->{$sw}->{$logPort}->{priority}->[$g] = $priority;
            }
        }
    }
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    
}

##@method void tpHandleSchedWeight
# Make a scheduler group higher priority
#
sub tpHandleSchedWeight
{
    my ($self, $port, $group_num, $weight) = @_;
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
            
            if(not defined $self->{SCHEDULER}->{$sw}->{$logPort})
            {
                # Get switch information from SDK
                my $info = new SDK::fm_switchInfo();
                $chip->fmGetSwitchInfo(0, $info);

                if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000 ||
                    $info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
                {
                    $self->{SCHEDULER}->{$sw}->{$logPort} = 
                                    $self->tpFM4000CreateSchedulerConfig();
                }
                elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
                {
                    $self->{SCHEDULER}->{$sw}->{$logPort} = 
                                    $self->tpFM6000CreateSchedulerConfig();
                }
            }

            my @groupList = $self->validateList($group_num, 0,
                                $self->{SCHEDULER}->{$sw}->{$logPort}->{num_groups}-1,
                                $FALSE);

            if(scalar(@groupList) == 0)
            {
                printf("Group(s) $group_num not valid on port $port, switch $sw\n");
                next;
            }

            foreach my $g (@groupList)
            {
                $self->{SCHEDULER}->{$sw}->{$logPort}->{weight}->[$g] = $weight;
            }
        }
    }
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $FM_OK;
    
}

##@method void tpHandleSchedApply
# Make a scheduler group higher priority
#
sub tpHandleSchedApply
{
    my ($self, $port) = @_;
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

        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);

        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);

            if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000 ||
                $info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
            {
                $self->tpFM4000HandleSchedApply($sw, $logPort, $port);
            }
            elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
            {
                $self->tpFM6000HandleSchedApply($sw, $logPort, $port);
            }
        }
    }
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $FM_OK;

}


sub tpHandleShowSched
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

        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);
 
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
            if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000 ||
                $info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
            {
                $self->tpFM4000HandleShowSched($sw, $logPort, $port,
                $config);
            }
            elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
            {
                $self->tpFM6000HandleShowSched($sw, $logPort, $port,
                $config);
            }
        }
    }
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);

}

1;
