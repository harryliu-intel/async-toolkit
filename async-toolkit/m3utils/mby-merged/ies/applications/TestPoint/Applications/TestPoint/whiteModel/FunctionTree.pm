# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/whiteModel/FunctionTree.pm
# Creation Date:    June 6, 2012
# Description:      Manages the menu tree for the FMC Client application 
#
# INTEL CONFIDENTIAL
# Copyright 2012 Intel Corporation. All Rights Reserved. 
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

package Applications::TestPoint::whiteModel::FunctionTree;
use strict;
use warnings;

use base qw(
    Exporter
    Applications::TestPoint::Common::FunctionTreeCore
    Applications::TestPoint::whiteModel::Functions
);

use Config;
use SDK qw(/^\$/);
use Chip::FocalPoint;
require Chip::FocalPoint::Configurations::Native;

# Export all public SDK constants to the TestPoint application to allow access
# to these constants when TestPoint is run in EXPERT mode.
our @EXPORT = @Chip::FocalPoint::EXPORT;
push(@EXPORT, qw(getCPUMacAddress));

##@cmethod public FunctionTree& new(char                    *platform,
#                                   Applications::TestPoint &testpoint)
#
# @desc         Instantiates a Perl
#               Applications::TestPoint::whiteModel::FunctionTree object
#
# @param[in]    platform The platform identifier
#
# @param[in]    testpoint A Perl reference to an instantiated Perl
#               Applications::TestPoint object
#
# @return       A Perl reference to an instantiated
#               Applications::TestPoint::whiteModel::FunctionTree object
sub new
{
    my ($class, $platform, $testpoint) = @_;

    $class = ref($class) || $class;
    my $self = 
             new Applications::TestPoint::Common::FunctionTreeCore("whiteModel",
                                                                   $testpoint);
    bless($self, $class);
    my $configuration = Chip::FocalPoint::Configurations::Native->new();
    $configuration->debug($testpoint->{DEBUG});

    # The whiteModel platform is built using one chip.
    $configuration->switchCount(1);

    $self->{CHIP} =
              Chip::FocalPoint->new(0, "FocalPoint", "Native", $configuration);

    # Retrieve all switch numbers associated with physical switches.
    $self->tpSetSwitches();

    # Assert that 1 physical switch is known to the SDK.
    verifySwitches($self->tpGetPhysicalSwitches());

    # In debug mode the state of the switch should NOT be changed. Only ``reg''
    # commands are guaranteed to function properly.
    if (!$testpoint->{DEBUG})
    {
        # Set the state of all switches to UP.
        foreach my $switchNum ($self->tpGetPhysicalSwitches())
        {
            $self->{CHIP}->fmSetSwitchState($switchNum, $FM_ENABLED);
        }
        $self->handleSetSwitchConfig("cpu_mac", getCPUMacAddress());
    }

    # Initialize the white model interface
    $self->{CHIP}->fmModelTestPointIntfInit();

    $self->{CHIP}->disableErrors();
    my $switchType = $self->{CHIP}->fmGetTextApiAttribute(
                                       $FM_AAK_API_PLATFORM_MODEL_SWITCH_TYPE,
                                       $FM_AAD_API_PLATFORM_MODEL_SWITCH_TYPE);
    $self->{CHIP}->enableErrors();

    my @switchTypes = split(/,/, $switchType);
    if ($switchTypes[0] =~ /0-(.*)/)
    {
        if (uc($1) eq "FM4000")
        {
            $self->{maxPort} = 24;
        }
        elsif (uc($1) eq "FM6000")
        {
            $self->{maxPort} = 72;
        }
        elsif (uc($1) eq "HLP")
        {
            $self->{maxPort} = 32;
        }
        else
        {
            printf("whiteModel/FunctionTree.pm: Unknown switch type: $1. Force 32 ports");
$self->{maxPort} = 32;
        }
    }
    else
    {
        die("$switchType: Invalid \"api.platform.model.switchType\" format");
    }

    return $self;
}

##@method private char* getCPUMacAddress(void)
#
# @desc         Retrieves the CPU MAC address
#
# @return       The CPU MAC address
sub getCPUMacAddress
{
    my $address = "00:00:00:00:00:00";

    my ($ifname, $lladdr);

    if ($Config{'archname'} =~ /darwin/)
    {
        $ifname = "en0";
        $lladdr = "ether";
    }
    else
    {
        $ifname = "eth0";
        $lladdr = "HWaddr";
    }
    my @output = split(/[\n\r]/, readpipe("/sbin/ifconfig $ifname"));
    foreach my $line (@output)
    {
        chomp($line);
        if ($line =~ m/\Q$lladdr\E/)
        {
            $address = $line;
            $address =~ s/.*$lladdr\s+([A-Za-z0-9:]+).*/$1/;
        }
    } 
    return $address;
}

##@method char **getPackageListForSymbolLoading()
#   Returns a list of package names to load symbols from
#
#   @return Array of package names
sub getPackageListForSymbolLoading
{
    return qw(Chip::FocalPoint);
}

##@method private void verifySwitches(int switches[])
#
# @desc         Verifies that 1 physical switch with switch number 0
#               is known to the SDK
sub verifySwitches
{
    my (@switches) = @_;

    map {
        die if $switches[$_] != $_;
    } (0)
}

1;
