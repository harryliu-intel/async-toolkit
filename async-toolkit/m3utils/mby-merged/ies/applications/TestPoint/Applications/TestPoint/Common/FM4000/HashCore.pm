# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FM4000/HashCore.pm
# Creation Date:    09/26/07
# Description:      Hashing code calls to be shared by all platform
#                   implementations for TestPoint.
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

package Applications::TestPoint::Common::FM4000::HashCore;
use strict;
use warnings;

use List::Util qw(sum);
use Math::BigFloat;
use Time::HiRes qw(gettimeofday);
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;


sub tp4000HandleSetHashingL234Compatable
{
    my ($self, $state) = @_;
    my $chip = $self->{CHIP};
    my $set;

    if(!defined $state)
    {
        print "State not defined.\n";
        return;
    }
    if ($state eq "on")
    {
        $set = $FM_LAG_HASH_COMPATIBILITY_FM2000;
    }
    elsif ($state eq "off")
    {
        $set = 0;
    }
    else
    {
        printf("%s is not a valid state!\n", $state);
        return;
    }

    print "Setting Compatable $state.\n";

    my %val = (type => "fm_uint32", value => $set);
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status = $chip->fmSetLAGAttributeExt($switchNum, $FM_LAG_HASH_COMPATIBILITY, 0, \%val);
        if ($status != $FM_OK)
        {
            print "ERROR: status $status does not equal FM_OK $FM_OK.\n";
        }
    }
}

my %L34Fields = (
                 "use_dip"   => $FM_ROUTING_HASH_DIP,
                 "use_sip"   => $FM_ROUTING_HASH_SIP,
                 "use_prot"  => $FM_ROUTING_HASH_PROT,
                 "use_tcp"   => $FM_ROUTING_HASH_TCP,
                 "use_udp"   => $FM_ROUTING_HASH_UDP,
                 "use_prot1" => $FM_ROUTING_HASH_PROT1,
                 "use_prot2" => $FM_ROUTING_HASH_PROT2,
                 "use_l4src" => $FM_ROUTING_HASH_L4SRC,
                 "use_l4dst" => $FM_ROUTING_HASH_L4DST,
                 "symmetric" => $FM_ROUTING_HASH_SYM
);

sub GetL34FieldBit
{
    my ($field) = @_;

    my $bit = $L34Fields{$field};

    if (!defined $bit)
    {
        printf("%s is not a valid field!\n", $field);
        return -1;
    }
    return $bit;
}

##@method void handleSetHashingL34()
# Handler for 
#
#   @param[in] field 
#
#   @param[in] state The state string, "on" or "off"
#
sub tp4000HandleSetHashingL34
{
    my ($self, $field, $state) = @_;
    my $chip = $self->{CHIP};
    my $set = 0;
    my $bit = 0;

    if (!defined $field)
    {
        print "Field not defined\n";
        return;
    }
    if(!defined $state)
    {
        print "State not defined.\n";
        return;
    }

    print "Setting hash $field $state\n";

    if ($state eq "on")
    {
        $set = 1;
    }
    elsif ($state eq "off")
    {
        $set = 0;
    }
    else
    {
        printf("%s is not a valid state!\n", $state);
        return;
    }

    $bit = GetL34FieldBit($field);
    if ($bit == -1)
    {
        return;
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my %val = (type => "fm_uint32", value => 0);
        my $status = $chip->fmGetSwitchAttribute($switchNum, $FM_ROUTING_HASH, \%val);
        if ($status != $FM_OK)
        {
            print "ERROR: status $status does not equal FM_OK $FM_OK.\n";
        }
        print "status = $status, val = " . $val{"value"} . "\n";

        if ($set)
        {
            $val{"value"} |= $bit;
        }
        else
        {
            $val{"value"} &= ~$bit;
        }

        $status = $chip->fmSetSwitchAttribute($switchNum, $FM_ROUTING_HASH, \%val);
        if ($status != $FM_OK)
        {
            print "ERROR: status $status does not equal FM_OK $FM_OK.\n";
        }
    }
}

sub tp4000HandleSetHashingL34Rotation
{
    my ($self, $rotationNumber) = @_;
    my $chip = $self->{CHIP};
    my $attribute;

    if(!defined $rotationNumber)
    {
        print "Rotation number not defined.\n";
        return;
    }
    if(($rotationNumber != 0) && ($rotationNumber != 1) && ($rotationNumber != 2))
    {
        print "Rotation $rotationNumber should be 0-2.\n";
        return;
    }

    print "Setting ECMP rotation to $rotationNumber.\n";

    my %val = (type => "fm_uint32", value => $rotationNumber);
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status = $chip->fmSetSwitchAttribute($switchNum, $FM_ROUTING_HASH_ROTATION, \%val);
        if ($status != $FM_OK)
        {
            print "ERROR: status $status does not equal FM_OK $FM_OK.\n";
        }
    }
}

sub tp4000HandleSetHashingL34Prot
{
    my ($self, $ab, $protocol) = @_;
    my $chip = $self->{CHIP};
    my $attribute;

    if(!defined $ab)
    {
        print "Protocol number (1/2) not defined.\n";
        return;
    }
    if ($ab eq "1")
    {
        $attribute = $FM_ROUTING_HASH_PROT_1;
    }
    elsif ($ab eq "2")
    {
        $attribute = $FM_ROUTING_HASH_PROT_2;
    }
    else
    {
        printf("%s is not a valid protocol number (1/2)!\n", $ab);
        return;
    }

    $protocol = $self->str2intnum($protocol);
    if(!defined $protocol)
    {
        print "Protocol not defined.\n";
        return;
    }
    printf("Setting PROT%d to 0x%X.\n", $ab, $protocol);

    my %val = (type => "fm_uint32", value => $protocol);
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status = $chip->fmSetSwitchAttribute($switchNum, $attribute, \%val);
        if ($status != $FM_OK)
        {
            print "ERROR: status $status does not equal FM_OK $FM_OK.\n";
        }
    }
}


sub HandleSetHashingL34Mask
{
    my ($self, $attribute, $maskSize, $name, $mask) = @_;
    my $chip = $self->{CHIP};

    $mask = $self->str2intnum($mask);
    if(!defined $mask)
    {
        print "Mask not defined.\n";
        return;
    }
    $mask &= (1 << $maskSize) - 1;

    printf("Setting the %s mask to 0x%X.\n", $name, $mask);

    my %val = (type => "fm_uint32", value => $mask);
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status = $chip->fmSetSwitchAttribute($switchNum, $attribute, \%val);
        if ($status != $FM_OK)
        {
            print "ERROR: status $status does not equal FM_OK $FM_OK.\n";
        }
    }
}

sub tp4000HandleSetHashingL34DiffServMask
{
    my ($self, $mask) = @_;

    HandleSetHashingL34Mask($self, $FM_ROUTING_HASH_FLOW_DIFFSERV_MASK, 6, "DiffServ", $mask);
}

sub tp4000HandleSetHashingL34UserMask
{
    my ($self, $mask) = @_;

    HandleSetHashingL34Mask($self, $FM_ROUTING_HASH_FLOW_USER_MASK, 8, "ISL User", $mask);
}

sub tp4000HandleSetHashingL34FlowLabelMask
{
    my ($self, $mask) = @_;

    HandleSetHashingL34Mask($self, $FM_ROUTING_HASH_FLOW_LABEL_MASK, 20, "Flow Label", $mask);
}

sub tp4000HandleShowHashingL34
{
    my ($self) = @_;
    my $chip = $self->{CHIP};
    my $format = " %-18s %s\n";

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        if ($switchCount > 1)
        {
            print "Switch $switchNum:\n";
        }

        my %val = (type => "fm_uint32", value => 0);

        my $status = $chip->fmGetSwitchAttribute($switchNum, $FM_ROUTING_HASH, \%val);
        if ($status != $FM_OK)
        {
            print "ERROR: status $status does not equal FM_OK $FM_OK.\n";
        }
        foreach my $field (sort keys %L34Fields)
        {
            my $value = (($L34Fields{$field} & $val{"value"}) != 0) ? "on" : "off";
            printf($format, $field, $value);
        }

        $status = $chip->fmGetSwitchAttribute($switchNum, $FM_ROUTING_HASH_ROTATION, \%val);
        if ($status != $FM_OK)
        {
            print "ERROR: status $status does not equal FM_OK $FM_OK.\n";
        }
        printf($format, "Routing rotation", $val{"value"});

        $status = $chip->fmGetSwitchAttribute($switchNum, $FM_ROUTING_HASH_PROT_1, \%val);
        if ($status != $FM_OK)
        {
            print "ERROR: status $status does not equal FM_OK $FM_OK.\n";
        }
        printf($format, "Protocol 1", sprintf("0x%X", $val{"value"}));

        $status = $chip->fmGetSwitchAttribute($switchNum, $FM_ROUTING_HASH_PROT_2, \%val);
        if ($status != $FM_OK)
        {
            print "ERROR: status $status does not equal FM_OK $FM_OK.\n";
        }
        printf($format, "Protocol 2", sprintf("0x%X", $val{"value"}));

        $status = $chip->fmGetSwitchAttribute($switchNum, $FM_ROUTING_HASH_FLOW_DIFFSERV_MASK, \%val);
        if ($status != $FM_OK)
        {
            print "ERROR: status $status does not equal FM_OK $FM_OK.\n";
        }
        printf($format, "DiffServ mask", sprintf("0x%X", $val{"value"}));

        $status = $chip->fmGetSwitchAttribute($switchNum, $FM_ROUTING_HASH_FLOW_USER_MASK, \%val);
        if ($status != $FM_OK)
        {
            print "ERROR: status $status does not equal FM_OK $FM_OK.\n";
        }
        printf($format, "ISL User mask", sprintf("0x%X", $val{"value"}));

        $status = $chip->fmGetSwitchAttribute($switchNum, $FM_ROUTING_HASH_FLOW_LABEL_MASK, \%val);
        if ($status != $FM_OK)
        {
            print "ERROR: status $status does not equal FM_OK $FM_OK.\n";
        }
        printf($format, "Flow Label mask", sprintf("0x%X", $val{"value"}));
    }
}

1;
