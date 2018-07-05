# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FM6000/ShapingCore.pm
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

package Applications::TestPoint::Common::FM6000::ShapingCore;
use strict;
use warnings;

use base qw(Exporter);

use List::Util qw(sum);
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

our @EXPORT = qw(
    tpFM6000CreateShapingConfig
    tpFM6000HandleShapingApply
    tpFM6000HandleShowShaping
);

# internal function, creates an shaping config data structure
sub tpFM6000CreateShapingConfig
{
    my ($self) = @_;

    # Default chip configuration:
    #   1 groups, all TC's, 10Gb/s 
    my %shaping = (
                num_groups => 12,
                tc => [ [0..11], [], [], [], [], [], [], [], [], [], [], [] ], 
                bandwidth => [10000,0,0,0,0,0,0,0,0,0,0,0,],
                capacity => [ 262144, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
            );

    return \%shaping;
}

sub tpFM6000ShapingSortTCList
{
    my ($self, $direction, $shapeConfig) = @_;

    for(my $grp = 0; $grp < $shapeConfig->{num_groups} ; $grp++)
    {
        my @tcList = @{$shapeConfig->{tc}->[$grp]};

        if($direction == 1)
        {
            @tcList = sort {$b <=> $a} @tcList;
        }else
        {
            @tcList = sort {$a <=> $b} @tcList;
        }

        $shapeConfig->{tc}->[$grp] = \@tcList;
    }
}


# Internal Function: Reads out the current shaping config 
# from the API and creates a shaping configuration data
# structure
sub tpFM6000LoadShapingConfig
{
    my ($self, $sw, $logPort) = @_;
    my $chip = $self->{CHIP};

    my %val = (type=>"fm_uint32", value=>0);

    my $shaping = {
                num_groups => 0,
                tc => [ [], [], [], [], [], [], [], [], [], [], [], [] ], 
                bandwidth => [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
                capacity => [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
            };

    # Get num groups and tc map
   
    # Setup the group map
    my $highest_group = 0;
    for(my $tc = 0 ; $tc < 12 ; $tc++)
    {
        $val{value}=0;
        $chip->fmGetPortQOS($sw, $logPort, $FM_QOS_TC_SHAPING_GROUP_MAP, $tc, \%val);

        my $group_num = $val{value};
        if($group_num > $highest_group)
        {
            $highest_group = $group_num;
        }
        push(@{$shaping->{tc}->[$group_num]}, $tc);
    } 
    $shaping->{num_groups} = $highest_group+1;

    # Reverse sort the tc arrays
    $self->tpFM6000ShapingSortTCList(1, $shaping);

    # Get the Bandwidth
    for(my $grp = 0; $grp < $shaping->{num_groups} ; $grp++)
    {
        $val{type} = "fm_uint64";
        $val{value} = new Math::BigInt(0);
        $chip->fmGetPortQOS($sw, $logPort, $FM_QOS_SHAPING_GROUP_RATE, 
                    $grp, \%val);
        $val{value}->bdiv(new Math::BigInt(1e6));
        $shaping->{bandwidth}->[$grp] = $val{value}->numify();
    }

    # Get the capacity
    for(my $grp = 0; $grp < $shaping->{num_groups} ; $grp++)
    {
        $val{type} = "fm_uint64";
        $val{value} = new Math::BigInt(0);
        $chip->fmGetPortQOS($sw, $logPort, $FM_QOS_SHAPING_GROUP_MAX_BURST, 
                    $grp, \%val);
        $shaping->{capacity}->[$grp] = $val{value}->numify()/8;
    }

    return $shaping;
}


sub tpFM6000ShapingPrintConfig
{
    my ($self, $port, $shapeConfig) = @_;

    $self->tpFM6000ShapingSortTCList(1, $shapeConfig);

    printf("Port $port\n");
    for(my $grp = 0 ; $grp < $shapeConfig->{num_groups} ; $grp++)
    {
        if (scalar(@{$shapeConfig->{tc}->[$grp]}) > 0)
        {
            printf("\tGroup $grp\n");
            printf("\t\tTraffic Class: %s\n", join(",",
                            @{$shapeConfig->{tc}->[$grp]}));
            printf("\t\tBandwidth: %s Mb/s\n", 
                        $shapeConfig->{bandwidth}->[$grp]);
            printf("\t\tCapacity: %s Bytes\n", 
                        ($shapeConfig->{capacity}->[$grp]));
        }
    }
}



sub tpFM6000HandleShowShaping
{
    my ($self, $sw, $logPort, $port, $config) = @_;

    my $shapeConfig;

    if($config eq "new")
    {
        if(!defined($self->{SHAPING}->{$sw}->{$logPort}))
        {
            printf("A new configuration has not been started on port $port, switch $sw\n");
            return;
        }

        $shapeConfig = $self->{SHAPING}->{$sw}->{$logPort};
    }
    elsif($config eq "active")
    {
        $shapeConfig = $self->tpFM6000LoadShapingConfig($sw, $logPort);
    }
    else
    {
        printf("Must specify active or new configuration\n");
        return; 
    }

    $self->tpFM6000ShapingPrintConfig($port, $shapeConfig);
    return;
}

# Internal function that applies a shaping configuration
sub tpFM6000ProgramShaping
{
    my ($self, $sw, $logPort) = @_;
    my $chip = $self->{CHIP};

    my %shaping = %{$self->{SHAPING}->{$sw}->{$logPort}};

    my %val = ( type=>"fm_uint32", value=>0);

    # 1) First sort the tc's in each shaping  group
    $self->tpFM6000ShapingSortTCList(1, $self->{SHAPING}->{$sw}->{$logPort});

    # 2) Program the group mapping
    for(my $grp = 0; $grp < $shaping{num_groups} ; $grp++)
    {
        foreach my $t ( @{$shaping{tc}->[$grp]})
        {
            $val{value} = $grp;
            $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_TC_SHAPING_GROUP_MAP, $t, \%val);
        }
    }
    
    # 3) Set capacity
    for(my $grp = 0; $grp < $shaping{num_groups} ; $grp++)
    {
        $val{type} = "fm_uint64";
        $val{value} = new Math::BigInt($shaping{capacity}->[$grp] * 8);
        $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_SHAPING_GROUP_MAX_BURST, 
                    $grp, \%val);
    }

    # 4) Set bandwidth
    for(my $grp = 0; $grp < $shaping{num_groups} ; $grp++)
    {
        $val{type} = "fm_uint64";
        $val{value} = new Math::BigInt($shaping{bandwidth}->[$grp] * 1E6);
        $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_SHAPING_GROUP_RATE, 
                    $grp, \%val);
    }
}


sub tpFM6000HandleShapingApply
{
    my ($self, $sw, $logPort, $port) = @_;

    if(not defined $self->{SHAPING}->{$sw}->{$logPort})
    {
        printf("Port $port, switch $sw has no shaping configuration to apply\n");
        return;
    }

    # 1) Validate the configuration
    # my $valid = $self->tpFM6000VerifyShapingConfig($sw, $logPort, $port);
    #
    # if($valid != $FM_OK)
    # {
    #     return;
    # } 

    # 2) Pass the configuration to the API
    $self->tpFM6000ProgramShaping($sw, $logPort);
}

1;
