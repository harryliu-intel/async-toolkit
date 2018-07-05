# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FM4000/SchedulerCore.pm
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

package Applications::TestPoint::Common::FM4000::SchedulerCore;
use strict;
use warnings;

use base qw(Exporter);

use List::Util qw(sum);
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

our @EXPORT = qw(
    tpFM4000HandleSchedNumGroups
    tpFM4000HandleSchedTrafficClass
    tpFM4000HandleSchedStrict
    tpFM4000HandleSchedPriority
    tpFM4000HandleSchedWeight
    tpFM4000HandleSchedApply
    tpFM4000HandleShowSched
);

# internal function, creates an scheduler config data structure
sub tpFM4000CreateSchedulerConfig
{
    my ($self) = @_;

    # Default chip configuration:
    #   8 groups, 1 tc per group, strict dequeue all groups
    my %scheduler = (
                num_groups => 8,
                tc => [ [], [], [], [], [], [], [], [] ], 
                strict => 0xff,
                priority => [0,0,0,0,0,0,0,0],
                weight => [ 0, 0, 0, 0, 0, 0, 0, 0],
            );

    return \%scheduler;
}

sub tpFM4000SortTCList
{
    my ($self, $direction, $schedConfig) = @_;

    for(my $grp = 0; $grp < $schedConfig->{num_groups} ; $grp++)
    {
        my @tcList = @{$schedConfig->{tc}->[$grp]};

        if($direction == 1)
        {
            @tcList = sort {$b <=> $a} @tcList;
        }else
        {
            @tcList = sort {$a <=> $b} @tcList;
        }

        $schedConfig->{tc}->[$grp] = \@tcList;
    }
}

# Internal function that applies a scheduler configuration
sub tpFM4000ProgramScheduler1
{
    my ($self, $p) = @_;
    my $chip = $self->{CHIP};

    my ($sw, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($p);
    my %scheduler = %{$self->{SCHEDULER}->{$p}};

    my %val = ( type=>"fm_uint32", value=>0);

    # 1) First sort the tc's in each scheduler group
    $self->tpFM4000SortTCList(1, $self->{SCHEDULER}->{$p});

    # 2) Program the group mask
    $val{value} = 0x80;
    for(my $grp = 0; $grp < $scheduler{num_groups} ; $grp++)
    {
        $val{value} |= 1 << ( $scheduler{tc}->[$grp]->[0] ) ;
    }
    $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_SCHED_GROUPS, 0, \%val);

    # 3) Set Weights
    for(my $grp = 0; $grp < $scheduler{num_groups} ; $grp++)
    {
        $val{value} = $scheduler{weight}->[$grp];
        $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_TC_GROUP_WEIGHT, 
                    $scheduler{tc}->[$grp]->[0], \%val);
    }

    # 4) Clear out any old priority/strict configuration
    for(my $tc = 0 ; $tc < 8 ; $tc++)
    {
        $val{value} = $FM_QOS_TC_GROUP_MODE_BALANCED;
        $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_TC_GROUP_MODE, $tc, \%val);
    }

    # 5) Set Strict mask
    for(my $grp = 0; $grp < $scheduler{num_groups} ; $grp++)
    {
        $val{value} = (($scheduler{strict} >> $grp) & 0x1) ?
                            $FM_QOS_TC_GROUP_MODE_STRICT : 
                            $FM_QOS_TC_GROUP_MODE_BALANCED;

        # Strict is programmed by TC, not by group head
        foreach my $tc ( @{$scheduler{tc}->[$grp]})
        {
            $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_TC_GROUP_MODE, 
                        $tc, \%val);
        }
    }

    # 6) Set Priority mask
    $val{value} = 0x80;
    for(my $grp = 0; $grp < $scheduler{num_groups}-1 ; $grp++)
    {
        if($scheduler{priority}->[$grp] != $scheduler{priority}->[$grp+1])
        {
            $val{value} |= 1 << ( $scheduler{tc}->[$grp]->[0] ) ;
        }
    }
    $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_SCHED_PRI_SETS, 0, \%val);
}

# Internal function that applies a scheduler configuration
sub tpFM4000ProgramScheduler
{
    my ($self, $sw, $logPort) = @_;
    my $chip = $self->{CHIP};

    my %scheduler = %{$self->{SCHEDULER}->{$sw}->{$logPort}};

    my %val = ( type=>"fm_uint32", value=>0);

    # 1) First sort the tc's in each scheduler group
    $self->tpFM4000SortTCList(1, $self->{SCHEDULER}->{$sw}->{$logPort});

    # 2) Program the group mask
    $val{value} = 0x80;
    for(my $grp = 0; $grp < $scheduler{num_groups} ; $grp++)
    {
        $val{value} |= 1 << ( $scheduler{tc}->[$grp]->[0] ) ;
    }
    $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_SCHED_GROUPS, 0, \%val);

    # 3) Set Weights
    for(my $grp = 0; $grp < $scheduler{num_groups} ; $grp++)
    {
        $val{value} = $scheduler{weight}->[$grp];
        $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_TC_GROUP_WEIGHT, 
                    $scheduler{tc}->[$grp]->[0], \%val);
    }

    # 4) Clear out any old priority/strict configuration
    for(my $tc = 0 ; $tc < 8 ; $tc++)
    {
        $val{value} = $FM_QOS_TC_GROUP_MODE_BALANCED;
        $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_TC_GROUP_MODE, $tc, \%val);
    }

    # 5) Set Strict mask
    for(my $grp = 0; $grp < $scheduler{num_groups} ; $grp++)
    {
        $val{value} = (($scheduler{strict} >> $grp) & 0x1) ?
                            $FM_QOS_TC_GROUP_MODE_STRICT : 
                            $FM_QOS_TC_GROUP_MODE_BALANCED;

        # Strict is programmed by TC, not by group head
        foreach my $tc ( @{$scheduler{tc}->[$grp]})
        {
            $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_TC_GROUP_MODE, 
                        $tc, \%val);
        }
    }

    # 6) Set Priority mask
    $val{value} = 0x80;
    for(my $grp = 0; $grp < $scheduler{num_groups}-1 ; $grp++)
    {
        if($scheduler{priority}->[$grp] != $scheduler{priority}->[$grp+1])
        {
            $val{value} |= 1 << ( $scheduler{tc}->[$grp]->[0] ) ;
        }
    }
    $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_SCHED_PRI_SETS, 0, \%val);
}

##@cmethod public int tpFM4000VerifySchedulerConfig(int sw, int logPort, int port)
#
# @desc         internal function, verify the scheduler configuration
#
# @param[in]    sw is the switch(es) on which the port appears
#
# @param[in]    logPort is the switch's logical port number for the port.
#
# @param[in]    port is the system global port number for the port (as the
#               user specified it).
#
# @return       FM_OK if configuration is valid, otherwise FM_FAIL
sub tpFM4000VerifySchedulerConfig
{
    my ($self, $sw, $logPort, $port) = @_;

    # Reverse sort the TC group arrays
    $self->tpFM4000SortTCList(0, $self->{SCHEDULER}->{$sw}->{$logPort});

    # Make sure all TC are in a group
    my $tc = 0;
    for(my $grp = 0 ; $grp < $self->{SCHEDULER}->{$sw}->{$logPort}->{num_groups} ; $grp++)
    {
        foreach my $t ( @{$self->{SCHEDULER}->{$sw}->{$logPort}->{tc}->[$grp]})
        {
            $tc |= 1<< $t;
        }

    }
    if($tc != 0xff)
    {
        my @list;
        for(my $i = 0 ; $i< 8 ; $i++)
        {
            if(!(($tc >> $i) & 0x1))
            {
                push(@list, $i);
            }
        }

        printf("Invalid config: port $port, switch $sw, traffic classes");
        printf(" not assigned to a scheduling group: %s\n", join(",", @list));
        return $FM_FAIL; 
    }

    my $last = $self->{SCHEDULER}->{$sw}->{$logPort}->{tc}->[0]->[0]; 
    # Make sure tc are consecutive within and between groups
    for(my $grp = 0 ; $grp < $self->{SCHEDULER}->{$sw}->{$logPort}->{num_groups} ; $grp++)
    {
        for(my $i = 0;
            $i < scalar(@{$self->{SCHEDULER}->{$sw}->{$logPort}->{tc}->[$grp]}) ;
            $i++)
        {
            if($i == 0 && $grp == 0)
            {
                # Skip the very first tc
                next;
            }

            if ($last+1 != $self->{SCHEDULER}->{$sw}->{$logPort}->{tc}->[$grp]->[$i])
            {
                printf("Invalid config: port $port, switch $sw, group $grp:");
                printf(" traffic classes must be consecutive\n");
                return $FM_FAIL;
            }

            $last = $self->{SCHEDULER}->{$sw}->{$logPort}->{tc}->[$grp]->[$i];
        }
    }

    # Make sure there is only one consecutive set of DRR groups
    my $cnt = 0;
    my $strict = -1;
    for(my $grp = 0 ; $grp < $self->{SCHEDULER}->{$sw}->{$logPort}->{num_groups} ; $grp++)
    {
        if($strict == -1)
        {
            $strict = ($self->{SCHEDULER}->{$sw}->{$logPort}->{strict} >> $grp) & 0x1;
        }else
        {
            if($strict != (($self->{SCHEDULER}->{$sw}->{$logPort}->{strict} >> $grp) & 0x1))
            {
                $cnt++;
            }
            $strict = ($self->{SCHEDULER}->{$sw}->{$logPort}->{strict} >> $grp) & 0x1;
        }
    }

    if($cnt > 2)
    {
        printf("Invalid config: port $port, switch $sw, only one consecutive set of");
        printf(" DRR groups is allowed\n");
        return $FM_FAIL;
    }

    # Verify the priority is consecutive
    my $pri = 0;
    for(my $grp = 0 ; $grp < $self->{SCHEDULER}->{$sw}->{$logPort}->{num_groups}; $grp++)
    {
        if($self->{SCHEDULER}->{$sw}->{$logPort}->{priority}->[$grp] != $pri)
        {
            if($self->{SCHEDULER}->{$sw}->{$logPort}->{priority}->[$grp] != $pri+1)
            {
                printf("Invalid config: port $port, switch $sw, group $grp is");
                printf(" not in an equal or consecutivly numbered priority\n");
                return $FM_FAIL;
            }else
            {
                $pri++;
            }
        }
    }

    # Verify: Strict priority must be assigned consistently throughout a
    # priority set.
    for(my $grp = 0; $grp < $self->{SCHEDULER}->{$sw}->{$logPort}->{num_groups}-1 ; $grp++)
    {
        if($self->{SCHEDULER}->{$sw}->{$logPort}->{priority}->[$grp] ==
            $self->{SCHEDULER}->{$sw}->{$logPort}->{priority}->[$grp+1])
        {
            if( (($self->{SCHEDULER}->{$sw}->{$logPort}->{strict} >> $grp) & 0x1) !=
                (($self->{SCHEDULER}->{$sw}->{$logPort}->{strict} >> ($grp+1)) & 0x1) )
            {
                printf("Invalid config: port $port, switch $sw, groups $grp");
                printf(" and %d: strict must be consistent within a", $grp+1);
                printf(" priority set\n");
                return $FM_FAIL;
            }
        }
    }

    return $FM_OK;
}

##@method void tpFM4000HandleSchedNumGroups
# FM4000 Handler for configuring the number of scheduler groups
#   Note calling this function clears any new configuration
#
#   @param[in] port The port or ports being configured
#   @param[in] numGroups The number of scheduler groups

sub tpFM4000HandleSchedNumGroups
{
    my ($self, $sw, $port, $numGroups) = @_;

    my @portList = $self->validateList($port, $self->tpPlatformGetPortRange());

    if(!defined $port || scalar(@portList) == 0)
    {
        printf("Must specifiy a valid port or port list\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    foreach my $p (@portList)
    {
       # Map port number for the benefit of non-SWAG Vegas. Ignore the
       # returned sw2 since it should match sw and may actually be
       # wrong for some platforms.
        my ($sw2, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($p);
        if(not defined $self->{SCHEDULER}->{$p})
        {
            $self->{SCHEDULER}->{$p} = $self->tpFM4000CreateSchedulerConfig();
        }

        $self->{SCHEDULER}->{$p}->{num_groups} = $numGroups; 
    } 
    return $FM_OK;
}

##@method void tpFM4000HandleSchedTrafficClass
# Assign a tc to a scheduler group
#
#   @param[in] sw           The switch number
#   @param[in] port         The port number
#   @param[in] group_num    The scheduler group
#   @param[in] tc           The traffic class being assigned
sub tpFM4000HandleSchedTrafficClass
{
    my ($self, $sw, $port, $group_num, $tc) = @_;

    my @portList = $self->validateList($port, $self->tpPlatformGetPortRange());
    my @tcList = $self->validateList($tc, 0, 7);

    if(!defined $port || scalar(@portList) == 0)
    {
        printf("Must specifiy a valid port or port list\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if(!defined($tc) || scalar(@tcList) == 0)
    {
        printf("Invalid tc, please provide a number between 0 and 7\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    foreach my $p (@portList)
    {
       # Map port number for the benefit of non-SWAG Vegas. Ignore the
       # returned sw2 since it should match sw and may actually be
       # wrong for some platforms.
        my ($sw2, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($p);

        if(not defined $self->{SCHEDULER}->{$p})
        {
            $self->{SCHEDULER}->{$p} = $self->tpFM4000CreateSchedulerConfig();
        }

        if($group_num >= $self->{SCHEDULER}->{$p}->{num_groups})
        {
            printf("Invalid group number $group_num, please ".
                   "specify a value between 0 and %d or change ".
                   "the total number of groups\n", 
                    $self->{SCHEDULER}->{$p}->{num_groups}-1);
            return $FM_ERR_INVALID_ARGUMENT;
        }

        foreach my $t (@tcList)
        {
            if($t < 0 || $t > 7)
            {
                printf("Invalid tc, please provide a number between 0 and 7\n");
                return $FM_ERR_INVALID_ARGUMENT;
            }

            # 2) find and remove the tc from any existing groups
            for ( my $i = 0; $i < 8 ; $i++ )
            {
                for ( my $j = 0; 
                      $j < scalar(@{$self->{SCHEDULER}->{$p}->{tc}->[$i]}) ;
                      $j++ )
                {
                    if($self->{SCHEDULER}->{$p}->{tc}->[$i]->[$j] == $t)
                    {
                        splice(@{$self->{SCHEDULER}->{$p}->{tc}->[$i]}, $j);
                    } 
                }
            }

            # 3) Add the tc to the specified group number
            push(@{$self->{SCHEDULER}->{$p}->{tc}->[$group_num]},$t); 

        }
    } 

    return $FM_OK;
}

##@method void tpFM4000HandleSchedStrict
# Mark a scheduler group as strict
#
#   @param[in] sw           The switch number
#   @param[in] port         The port number
#   @param[in] group_num    The scheduler group
#   @param[in] strict       "on" or "off
sub tpFM4000HandleSchedStrict
{
    my ($self, $sw, $port, $group_num, $strict) = @_;

    my @portList = $self->validateList($port, $self->tpPlatformGetPortRange());

    if(!defined $port || scalar(@portList) == 0)
    {
        printf("Must specifiy a valid port or port list\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    foreach my $p (@portList)
    {
       # Map port number for the benefit of non-SWAG Vegas. Ignore the
       # returned sw2 since it should match sw and may actually be
       # wrong for some platforms.
        my ($sw2, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($p);
        if(not defined $self->{SCHEDULER}->{$p})
        {
            $self->{SCHEDULER}->{$p} = $self->tpFM4000CreateSchedulerConfig();
        }

        my @groupList = $self->validateList($group_num, 0,
                            $self->{SCHEDULER}->{$p}->{num_groups}-1);

        if(scalar(@groupList) == 0)
        {
            printf("Invalid group, please provide a number between 0 and 7\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }

        foreach my $g (@groupList)
        {
            my $strict_bit = ( lc($strict) eq "on" ) ? 1 : 0;
                               
            $self->{SCHEDULER}->{$p}->{strict} &= ~(1 << $g);
            $self->{SCHEDULER}->{$p}->{strict} |= ($strict_bit << $g);

        }
    }
    return $FM_OK;
}

##@method void tpFM4000HandleSchedPriority
# Mark a scheduler group as higher priority
#
#   @param[in] sw           The switch number
#   @param[in] port         The port number
#   @param[in] group_num    The scheduler group
#   @param[in] priority     The priority number
sub tpFM4000HandleSchedPriority
{
    my ($self, $sw, $port, $group_num, $priority) = @_;

    my @portList = $self->validateList($port, $self->tpPlatformGetPortRange());

    if(!defined $port || scalar(@portList) == 0)
    {
        printf("Must specifiy a valid port or port list\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    foreach my $p (@portList)
    {
        # Map port number for the benefit of non-SWAG Vegas. Ignore the
        # returned sw2 since it should match sw and may actually be
        # wrong for some platforms.
        my ($sw2, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($p);
        if(not defined $self->{SCHEDULER}->{$p})
        {
            $self->{SCHEDULER}->{$p} = $self->tpFM4000CreateSchedulerConfig();
        }

        my @groupList = $self->validateList($group_num, 0,
                            $self->{SCHEDULER}->{$p}->{num_groups}-1);

        if($priority < 0 || $priority > $self->{SCHEDULER}->{$p}->{num_groups})
        {
            printf("Must specify a valid priority (0 to num_groups)\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }

        if(scalar(@groupList) == 0)
        {
            printf("Invalid group, please provide a number between 0 and 7\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }

        foreach my $g (@groupList)
        {
            $self->{SCHEDULER}->{$p}->{priority}->[$g] = $priority;
        }
    }
    return $FM_OK;
}

sub tpFM4000HandleSchedWeight
{
    my ($self, $sw, $port, $group_num, $weight) = @_;

    my @portList = $self->validateList($port, $self->tpPlatformGetPortRange());

    if(!defined $port || scalar(@portList) == 0)
    {
        printf("Must specifiy a valid port or port list\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    foreach my $p (@portList)
    {
        # Map port number for the benefit of non-SWAG Vegas. Ignore the
        # returned sw2 since it should match sw and may actually be
        # wrong for some platforms.
        my ($sw2, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($p);
        if(not defined $self->{SCHEDULER}->{$p})
        {
            $self->{SCHEDULER}->{$p} = $self->tpFM4000CreateSchedulerConfig();
        }

        my @groupList = $self->validateList($group_num, 0,
                            $self->{SCHEDULER}->{$p}->{num_groups}-1);

        if(scalar(@groupList) == 0)
        {
            printf("Invalid group, please provide a number between 0 and 7\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }

        foreach my $g (@groupList)
        {
            $self->{SCHEDULER}->{$p}->{weight}->[$g] = $weight;
        }
    }
    return $FM_OK;
}

##@cmethod public int tpFM4000HandleSchedApply(int sw, int logPort, int port)
#
# @desc         Handles applying the scheduler configuration to a port
#
# @param[in]    sw is the switch(es) on which the port appears
#
# @param[in]    logPort is the switch's logical port number for the port.
#
# @param[in]    port is the system global port number for the port (as the
#               user specified it).
sub tpFM4000HandleSchedApply
{
    my ($self, $sw, $logPort, $port) = @_;

    if(not defined $self->{SCHEDULER}->{$sw}->{$logPort})
    {
        printf("Port $port, switch $sw has no scheduler configuration to apply\n");
        return;
    }

    # 1) Validate the configuration
    my $valid = $self->tpFM4000VerifySchedulerConfig($sw, $logPort, $port);

    if($valid != $FM_OK)
    {
        return;
    } 

    # 2) Pass the configuration to the API
    $self->tpFM4000ProgramScheduler($sw, $logPort);

}

# Internal Function: Reads out the current scheduler config 
# from the API and creates a scheduler configuration data
# structure
sub tpFM4000LoadSchedConfig
{
    my ($self, $sw, $logPort) = @_;
    my $chip = $self->{CHIP};

    my %val = (type=>"fm_uint32", value=>0);

    my $scheduler = {
                num_groups => 0,
                tc => [ [], [], [], [], [], [], [], [] ], 
                strict => 0xff,
                priority => [ 0, 0, 0, 0, 0, 0, 0, 0],
                weight => [ 0, 0, 0, 0, 0, 0, 0, 0]
            };

    # Get num groups and tc map
    $chip->fmGetPortQOS($sw, $logPort, $FM_QOS_SCHED_GROUPS, 0, \%val);
   
    # Walk through the group map, and count how many groups there are
    # Also setup the tc arrays while we're at it
    my $group_num = 0;
    for(my $tc = 0 ; $tc < 8 ; $tc++)
    {
        push(@{$scheduler->{tc}->[$group_num]}, $tc);

        if(($val{value} >> $tc) & 0x1)
        {
            # Head of a group
            $group_num++;
        }
    } 
    $scheduler->{num_groups} = $group_num;

    # Reverse sort the tc arrays
    $self->tpFM4000SortTCList(1, $scheduler);

    # Get the weights
    for(my $grp = 0; $grp < $scheduler->{num_groups} ; $grp++)
    {
        $val{value} = 0;
        $chip->fmGetPortQOS($sw, $logPort, $FM_QOS_TC_GROUP_WEIGHT, 
                    $scheduler->{tc}->[$grp]->[0], \%val);

        $scheduler->{weight}->[$grp] = $val{value};
    }

    # Get the strict bitmask
    $scheduler->{strict} = 0; 
    for(my $grp = 0; $grp < $scheduler->{num_groups} ; $grp++)
    {
        $chip->fmGetPortQOS($sw, $logPort, $FM_QOS_TC_GROUP_MODE, 
                    $scheduler->{tc}->[$grp]->[0], \%val);
    
        my $strict = ($val{value} == $FM_QOS_TC_GROUP_MODE_STRICT) ? 1 : 0;
        $scheduler->{strict} |= $strict << $grp;
    }

    # Get the priorities
    $chip->fmGetPortQOS($sw, $logPort, $FM_QOS_SCHED_PRI_SETS, 0, \%val);
    my $pri = 0;
    for(my $grp = 0; $grp < $scheduler->{num_groups} ; $grp++)
    {
        $scheduler->{priority}->[$grp] = $pri;
        if( ($val{value} >> $scheduler->{tc}->[$grp]->[0]) & 0x1 )
        {
            $pri++;
        }
    }

    return $scheduler;
}

sub tpFM4000SchedPrintConfig
{
    my ($self, $port, $schedConfig) = @_;

    $self->tpFM4000SortTCList(1, $schedConfig);

    printf("Port $port\n");
    for(my $grp = 0 ; $grp < $schedConfig->{num_groups} ; $grp++)
    {
        printf("\tGroup $grp\n");
        printf("\t\tTraffic Class: %s\n", join(",",
                        @{$schedConfig->{tc}->[$grp]}));
        printf("\t\tStrict: %s\n", 
                    (($schedConfig->{strict} >> $grp) & 0x1) ? 
                        "on" : "off");
        printf("\t\tPriority: %s\n", 
                    $schedConfig->{priority}->[$grp]);
        printf("\t\tWeight: %s\n", 
                    ($schedConfig->{weight}->[$grp]));
    }
}


sub tpFM4000HandleShowSched
{
    my ($self, $sw, $logPort, $port, $config) = @_;

    my $schedConfig;

    if($config eq "new")
    {
        if(!defined($self->{SCHEDULER}->{$sw}->{$logPort}))
        {
            printf("A new configuration has not been started on port $port, switch $sw\n");
            return;
        }

        $schedConfig = $self->{SCHEDULER}->{$sw}->{$logPort};
    }
    elsif($config eq "active")
    {
        $schedConfig = $self->tpFM4000LoadSchedConfig($sw, $logPort);
    }
    else
    {
        printf("Must specify active or new configuration\n");
        return; 
    }

    $self->tpFM4000SchedPrintConfig($port, $schedConfig);

    return;
}


1;
