# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/HashCore.pm
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

package Applications::TestPoint::Common::HashCore;
use strict;
use warnings;

use List::Util qw(sum);
use Math::BigFloat;
use Time::HiRes qw(gettimeofday);
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

my %hashRotation = (
                  "L2_hash_rot" => $FM_LAG_HASH_L2_HASH_ROTATION,
                  "L3_hash_rot" => $FM_LAG_HASH_L3_HASH_ROTATION,
                  "random"      => $FM_LAG_HASH_RANDOM_HASH_VALUE
);

my %rotationSelect = (
                  "a" => $FM_LAG_HASH_ROTATION_A,
                  "b" => $FM_LAG_HASH_ROTATION_B,
);

my %L234Fields = (
                  "use_dmac"     => $FM_LAG_HASH_DA,
                  "use_smac"     => $FM_LAG_HASH_SA,
                  "use_type"     => $FM_LAG_HASH_TYPE,
                  "use_srcport"  => $FM_LAG_HASH_SRCPORT,
                  "use_vlanid"   => $FM_LAG_HASH_VLANID,
                  "use_vlanpri"  => $FM_LAG_HASH_VLANPRI,
                  "use_l2_if_ip" => $FM_LAG_HASH_L2IFIP,
                  "use_l34"      => $FM_LAG_HASH_L34,
                  "symmetric"    => $FM_LAG_HASH_SYM
);

sub GetL234FieldBit
{
    my ($field) = @_;

    my $bit = $L234Fields{$field};

    if (!defined $bit)
    {
        printf("%s is not a valid field!\n", $field);
        return -1;
    }
    return $bit;
}

##@method void handleSetHashingL234()
# Handler for 
#
#   @param[in] field 
#
#   @param[in] state The state string, "on" or "off"
#
sub tpHandleSetHashingL234
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

    $bit = GetL234FieldBit($field);
    if ($bit == -1)
    {
        return;
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my %val = (type => "fm_uint32", value => 0);
        my $status = $chip->fmGetLAGAttributeExt($switchNum, $FM_LAG_HASH, 0, \%val);
        if ($status != $FM_OK)
        {
            print "ERROR: status $status does not equal FM_OK $FM_OK.\n";
        }

        if ($set)
        {
            $val{"value"} |= $bit;
        }
        else
        {
            $val{"value"} &= ~$bit;
        }

        $status = $chip->fmSetLAGAttributeExt($switchNum, $FM_LAG_HASH, 0, \%val);
        if ($status != $FM_OK)
        {
            print "ERROR: status $status does not equal FM_OK $FM_OK.\n";
        }
    }
}

sub tpHandleSetHashingL234Rotation
{
    my ($self, $ab, $rotationNumber) = @_;
    my $chip = $self->{CHIP};
    my $attribute;

    if(!defined $ab)
    {
        print "Rotation (A/B) not defined.\n";
        return;
    }
    if ($ab eq "a")
    {
        $attribute = $FM_LAG_HASH_ROTATION_A;
    }
    elsif ($ab eq "b")
    {
        $attribute = $FM_LAG_HASH_ROTATION_B;
    }
    else
    {
        printf("%s is not a valid Rotation (a/b)!\n", $ab);
        return;
    }

    if(!defined $rotationNumber)
    {
        print "Rotation number not defined.\n";
        return;
    }
    if(($rotationNumber != 0) && ($rotationNumber != 1) && ($rotationNumber != 2) && ($rotationNumber != 3))
    {
        print "Rotation $rotationNumber should be 0-3.\n";
        return;
    }

    print "Setting rotation $ab to $rotationNumber.\n";

    my %val = (type => "fm_uint32", value => $rotationNumber);
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status = $chip->fmSetLAGAttributeExt($switchNum, $attribute, 0, \%val);
        if ($status != $FM_OK)
        {
            print "ERROR: status $status does not equal FM_OK $FM_OK.\n";
        }
    }
}

sub tpHandleShowHashingL234
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

        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($switchNum, $info);

        my %val = (type => "fm_uint32", value => 0);

        my $status = $chip->fmGetLAGAttributeExt($switchNum, $FM_LAG_HASH, 0, \%val);
        if ($status != $FM_OK)
        {
            print "ERROR: status $status does not equal FM_OK $FM_OK.\n";
        }
        foreach my $field (sort keys %L234Fields)
        {
            if (($info->{'switchFamily'} != $FM_SWITCH_FAMILY_FM6000) ||
                (($field ne "use_srcport") && ($field ne "use_l2_if_ip")))
            {
                my $value = (($L234Fields{$field} & $val{"value"}) != 0) ? "on" : "off";
                printf($format, $field, $value);
            }
        }

        if ($info->{'switchFamily'} != $FM_SWITCH_FAMILY_FM6000)
        {
            $status = $chip->fmGetLAGAttributeExt($switchNum, $FM_LAG_HASH_COMPATIBILITY, 0, \%val);
            if ($status != $FM_OK)
            {
                print "ERROR: status $status does not equal FM_OK $FM_OK.\n";
            }
            my $value = (($FM_LAG_HASH_COMPATIBILITY_FM2000 & $val{"value"}) != 0) ? "on" : "off";
            printf($format, "fm2000_compatable", $value);

            $status = $chip->fmGetLAGAttributeExt($switchNum, $FM_LAG_HASH_ROTATION_A, 0, \%val);
            if ($status != $FM_OK)
            {
                print "ERROR: status $status does not equal FM_OK $FM_OK.\n";
            }
            printf($format, "Rotation A", $val{"value"});
    
            $status = $chip->fmGetLAGAttributeExt($switchNum, $FM_LAG_HASH_ROTATION_B, 0, \%val);
            if ($status != $FM_OK)
            {
                print "ERROR: status $status does not equal FM_OK $FM_OK.\n";
            }
            printf($format, "Rotation B", $val{"value"});
        }
    }
}

1;
