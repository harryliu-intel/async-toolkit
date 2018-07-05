# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

##############################################################################
# File:             Chip/FocalPoint.pm
# Creation Date:    08/30/07
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
##############################################################################

##@namespace Chip::FocalPoint
# The Chip::FocalPoint package provides access to the FM2000 and FM4000
# register address subroutines and to all Fulcum Microsystems Software
# Development Kit (SDK) functions.
package Chip::FocalPoint;
use strict;
use warnings;

use base qw(
    Exporter
    Base::Chip
);

our @EXPORT = qw(@SYMBOLS);
our @ISA;
our @SYMBOLS = qw();

# Import all public SDK constants, but prevent import of the @SYMBOLS symbol to
# prevent accidental modification of this symbol.
use Chip::FocalPoint::Environments::NativeEnv qw(!@SYMBOLS);

# Export all public SDK constants and allow access to the Bali register and
# SDK symbols to allow TestPoint to perform symbol completion.
BEGIN
{
    push(@EXPORT, @Chip::FocalPoint::Environments::NativeEnv::EXPORT);
}
push(@SYMBOLS, @Chip::FocalPoint::Environments::NativeEnv::SYMBOLS);

#To be loaded when chip family is detected
#BEGIN
#{
#    require Chip::FM2000::Registers;
#    require Chip::FM4000::Registers;
#    require Chip::FM6000::Registers;
#}

##@method public Chip::FocalPoint&
#                new(int                                        id,
#                    char                                       *type,
#                    char                                       *environment,
#                    Chip::FocalPoint::Configurations::Native   &configuration)
#
# @desc         Instantiates a Perl Chip::FocalPoint object
#
# @param[in]    id The global identifier for the Chip::FocalPoint object to be
#               created
#
# @param[in]    type the chip type identifier
#
# @param[in]    environment The environment identifier
#
# @param[in]    configuration An instantiated
#               Chip::FocalPoint::Configurations::Native Perl object containing
#               information with which the Chip::FocalPoint Perl object is to
#               be initialized
#
# @return       An instantiated Chip::FocalPoint Perl object
sub new
{
    my ($class, $id, $type, $environment, $configuration) = @_;

    my $result;

    $class = ref($class) || $class;
    if (lc($environment) eq "native")
    {
        $environment =
           Chip::FocalPoint::Environments::NativeEnv->new($id, $configuration);
    }
    else
    {
        die(sprintf("%s: Unknown environment", $environment));
    }

    sleep(1);

    my $self = Base::Chip->new($id, $type, undef, $environment);
    bless($self, $class);
    $self->SetConfiguration($configuration);

    if (!$configuration->debug && !$configuration->debugPlatform)
    {
        # Set the state of the switch to UP.
        foreach my $switchNum ($self->GetSwitches)
        {
            $result = $self->fmSetSwitchState($switchNum, $FM_ENABLED);
            if ($result != $FM_OK)
            {
                print("Failed to bring switch up - exiting TestPoint ...\n");
                exit(1);
            }
        }
    }

    # Retrieve the register information.
    $self->SetRegisters;

    return $self;
}

##@cmethod private Chip::FocalPoint::Configurations::Native&
#                  GetConfiguration(void)
#
# @desc         Retrieves the SDK configuration
#
# @return       The SDK configuration
sub GetConfiguration
{
    my ($self) = @_;

    return $self->{CONFIGURATION};
}

##@cmethod private int[] GetSwitches(void)
#
# @desc         Retrieves the switch numbers for all physical switches
#
# @return       The switch number for all physical switches
sub GetSwitches
{
    my ($self) = @_;

    if (!exists($self->{SWITCHES}) || !defined($self->{SWITCHES}))
    {
        $self->SetSwitches()
    }

    return @{$self->{SWITCHES}};
}

##@cmethod private int[] GetSelectedSwitches(void)
#
# @desc         Retrieves the switch numbers for all selected switches
#
# @return       The switch numbers for all selected switches
sub GetSelectedSwitches
{
    my ($self) = @_;

    if (!exists($self->{SELECTED_SWITCHES}) || !defined($self->{SELECTED_SWITCHES}))
    {
        # Default to all available physical switches
        $self->SetSelectedSwitches($self->GetSwitches);
    }

    return @{$self->{SELECTED_SWITCHES}};
}

##@cmethod private SDK::fm_switchInfo[] GetSwitchInfo(void)
#
# @desc         Retrieves identification information for all physical switches
#
# @return       The identification information for all physical switches
sub GetSwitchInfo
{
    my ($self) = @_;

    if (!exists($self->{INFORMATION}) || !defined($self->{INFORMATION}))
    {
        $self->SetSwitchInfo();
    }

    return @{$self->{INFORMATION}};
}

##@cmethod private void SetConfiguration(
#                      Chip::FocalPoint::Configurations::Native &configuration)
#
# @desc         Sets the SDK configuration
#
# @param[in]    configuration The SDK configuration
sub SetConfiguration
{
    my ($self, $configuration) = @_;

    $self->{CONFIGURATION} = $configuration;
}

##@cmethod private void SetRegisters(void)
#
# @decs         Populates an internal data structure with register information
sub SetRegisters
{
    my ($self) = @_;

    my ($registers);

    # Perform switch family identification for all physical switches.
    SWITCH: foreach my $switchNum ($self->GetSwitches)
    {
        my ($currentFamily, $previousFamily);

        $currentFamily = ($self->GetSwitchInfo)[$switchNum]->{'switchFamily'};
        CASE: for ($currentFamily)
        {
            ($_ == $FM_SWITCH_FAMILY_FM2000) && do
            {
                my $chipISA = 'Chip::FM2000::Registers';
                eval("require $chipISA");
                die($@) if ($@);
                if (not grep {$_ eq $chipISA} @ISA)
                {
                    push(@ISA, $chipISA);
                    push(@SYMBOLS, @Chip::FM2000::Registers::EXPORT);
                }
                $registers = \%Chip::FM2000::Registers::;
                last CASE;
            };

            ( ( $_ == $FM_SWITCH_FAMILY_FM4000 ) ||
              ( $_ == $FM_SWITCH_FAMILY_REMOTE_FM4000 ) ) && do
            {
                my $chipISA = 'Chip::FM4000::Registers';
                eval("require $chipISA");
                die($@) if ($@);
                if (not grep {$_ eq $chipISA} @ISA) 
                {
                    push(@ISA, $chipISA);
                    push(@SYMBOLS, @Chip::FM4000::Registers::EXPORT);
                }
                $registers = \%Chip::FM4000::Registers::;
                last CASE;
            };

            ($_ == $FM_SWITCH_FAMILY_FM6000) && do
            {
                my $chipISA = 'Chip::FM6000::Registers';
                eval("require $chipISA");
                die($@) if ($@);
                if (not grep {$_ eq $chipISA} @ISA) 
                {
                    push(@ISA, $chipISA);
                    push(@SYMBOLS, @Chip::FM6000::Registers::EXPORT);
                }
                $registers = \%Chip::FM6000::Registers::;
                last CASE;
            };

            ($_ == $FM_SWITCH_FAMILY_HLP) && do
            {
                # No need reg support, done in API debug code
                last CASE;
            };

            ($_ == $FM_SWITCH_FAMILY_SWAG) && do
            {
                # Do nothing for SWAG chips; only physical switches
                # get register support
                last CASE;
            };

            ($_ == $FM_SWITCH_FAMILY_UNKNOWN) && do
            {
                $registers = undef;
                warn(  "Cannot detect switch family!\n"
                     . "Name based register access will be disabled");
                last CASE;
            };

            die("$_: Unknown switch family");
        }

        if (defined($previousFamily) && $previousFamily != $currentFamily)
        {
            die("Mixed chip environments are not supported");
        }
        $previousFamily = $currentFamily;
    }
    $self->SUPER::SetRegisters($registers);
}

##@cmethod private void SetSwitches(void)
#
# @desc         Populates an internal data structure with all physical switches
sub SetSwitches
{
    my ($self) = @_;

    $self->{SWITCHES} = [];
    my $n = 0;
    my ($currentSwitch, $nextSwitch, $status);
    do
    {
        $self->disableErrors();
        if ($n == 0)
        {
            $status = $self->fmGetSwitchFirst(\$nextSwitch);
        }
        else
        {
            $status = $self->fmGetSwitchNext($currentSwitch, \$nextSwitch); 
        }
        $self->enableErrors();
        push(@{$self->{SWITCHES}}, $nextSwitch);
        $currentSwitch = $nextSwitch;
    } while ($status == $FM_OK && ++$n);
    pop(@{$self->{SWITCHES}});
    # Numerically sort the switch numbers.
    @{$self->{SWITCHES}} = sort {$a <=> $b} @{$self->{SWITCHES}};
}

##@cmethod private void SetSelectedSwitches(void)
#
# @desc         Populates an internal data structure with the switch numbers
#               of all switches selected for subsequent configuration.
#
# @param[in]    selectedSwitches is an array of switch numbers that have been
#               selected.
sub SetSelectedSwitches
{
    my ($self, @selectedSwitches) = @_;

    @{$self->{SELECTED_SWITCHES}} = sort {$a <=> $b} @selectedSwitches;
}

##@cmethod public void SetSwitchInfo(void)
#
# @desc         Populates an internal data structure with identification
#               information for all physical switches
sub SetSwitchInfo
{
    my ($self) = @_;

    my $configuration = $self->GetConfiguration;

    $self->{INFORMATION} = [];
    foreach my $switchNum ($self->GetSwitches)
    {
        my $infoW = SDK::fm_switchInfo->new();
        if (!$configuration->debug && !$configuration->debugPlatform)
        {
            $self->fmGetSwitchInfo($switchNum, $infoW);
        }
        elsif ($configuration->debug && !$configuration->debugPlatform)
        {
            my ($value);

            my $status = $self->fmReadUINT32($switchNum, 0x304, \$value);
            if ($status != $FM_OK)
            {
                die("Cannot read VITAL_PRODUCT_DATA register");
            }
            my $partNumber = ($value >> 12) & 0xffff;
            SWITCH: for ($partNumber)
            {
                $_ == 0xae18 && do
                {
                    $infoW->{'switchFamily'} = $FM_SWITCH_FAMILY_FM2000;
                    last SWITCH;
                };

                $_ == 0xae19 && do
                {
                    $infoW->{'switchFamily'} = $FM_SWITCH_FAMILY_FM4000;
                    last SWITCH;
                };

                /^/ && do
                {
                    die(sprintf("%#x: Unknown part number", $_));
                };
            }
        }
        elsif ($configuration->debugPlatform)
        {
            $infoW->{'switchFamily'} = $FM_SWITCH_FAMILY_UNKNOWN;
        }
        $self->{INFORMATION}->[$switchNum] = $infoW;
    }
}

1;
