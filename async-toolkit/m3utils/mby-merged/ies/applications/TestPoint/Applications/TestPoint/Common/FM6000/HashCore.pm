# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FM6000/HashCore.pm
# Creation Date:    08/25/11
# Description:      Hashing code calls for FM6000 switch family.
#
# INTEL CONFIDENTIAL
# Copyright 2011  Intel Corporation. All Rights Reserved. 
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

package Applications::TestPoint::Common::FM6000::HashCore;
use strict;
use warnings;

use List::Util qw(sum);
use Math::BigFloat;
use Time::HiRes qw(gettimeofday);
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;


sub tp6000HandleSetHashingProf
{
    my ($self, $profile, $attrib, $field, $value, $extValue) = @_;
    my $chip = $self->{CHIP};

    if(!defined $value)
    {
        print "Value not defined.\n";
        return;
    }

    if ($attrib eq "l2Key")
    {
        my $l2HashKey = new SDK::fm_L2HashKey();
        $l2HashKey->{'profileIndex'} = $profile;

        my $switchCount = scalar(@{[$self->tpGetSwitches]});
        foreach my $switchNum ($self->tpGetSwitches)
        {
            my %val = (type => "fm_L2HashKey", value => $l2HashKey);
            my $status = $chip->fmGetSwitchAttribute($switchNum, $FM_L2_HASH_KEY, \%val);
            if ($status != $FM_OK)
            {
                print "ERROR: Can't retreive the L2 Hash Key configuration\n";
                return $status;
            }

            if ($field eq "smac")
            {
                # validate the mac
                if ($self->validateL2Address($value) != $FM_OK)
                {
                    print($TP_MSG_ERR_MAC_INVALID_ADDRESS);
                    return $FM_ERR_INVALID_ARGUMENT;
                }

                $l2HashKey->{'SMACMask'} = $value;
            }
            elsif ($field eq "dmac")
            {
                # validate the mac
                if ($self->validateL2Address($value) != $FM_OK)
                {
                    print($TP_MSG_ERR_MAC_INVALID_ADDRESS);
                    return $FM_ERR_INVALID_ARGUMENT;
                }

                $l2HashKey->{'DMACMask'} = $value;
            }
            elsif ($field eq "ethtype")
            {
                $l2HashKey->{'etherTypeMask'} = $value;
            }
            elsif ($field eq "vlan1")
            {
                $l2HashKey->{'vlanId1Mask'} = $value;
            }
            elsif ($field eq "vlan1-pri")
            {
                $l2HashKey->{'vlanPri1Mask'} = $value;
            }
            elsif ($field eq "vlan2")
            {
                $l2HashKey->{'vlanId2Mask'} = $value;
            }
            elsif ($field eq "vlan2-pri")
            {
                $l2HashKey->{'vlanPri2Mask'} = $value;
            }
            elsif ($field eq "def-rot")
            {
                $l2HashKey->{'defaultHashRot'}->{'mantissa'} = $value;
                $l2HashKey->{'defaultHashRot'}->{'exponent'} = $extValue;
            }
            elsif ($field eq "symmetrize")
            {
                if ($value eq 'on')
                {
                    $l2HashKey->{'symmetrizeMAC'} = $TRUE;
                }
                else
                {
                    $l2HashKey->{'symmetrizeMAC'} = $FALSE;
                }
            }
            else
            {
                print "ERROR: Wrong Input Data\n";
                return $FM_FAIL;
            }

            $status = $chip->fmSetSwitchAttribute($switchNum, $FM_L2_HASH_KEY, \%val);
            if ($status != $FM_OK)
            {
                print "ERROR: Can't push the L2 Hash Key configuration\n";
                return $status;
            }
        }
    }
    elsif (($attrib eq "rotA") || $attrib eq "rotB")
    {
        my $l2HashRot = new SDK::fm_L2HashRot();
        $l2HashRot->{'profileIndex'} = $profile;

        my $attribute = 0;
        if ($attrib eq "rotA")
        {
            $attribute = $FM_L2_HASH_ROT_A;
        }
        else
        {
            $attribute = $FM_L2_HASH_ROT_B;
        }

        my $switchCount = scalar(@{[$self->tpGetSwitches]});
        foreach my $switchNum ($self->tpGetSwitches)
        {
            my %val = (type => "fm_L2HashRot", value => $l2HashRot);
            my $status = $chip->fmGetSwitchAttribute($switchNum, $attribute, \%val);
            if ($status != $FM_OK)
            {
                print "ERROR: Can't retreive the L2 Hash Rotation configuration\n";
                return $status;
            }

            if ($field eq "useL3HashKey")
            {
                if ($value eq 'on')
                {
                    $l2HashRot->{'useL3HashKey'} = $TRUE;
                }
                else
                {
                    $l2HashRot->{'useL3HashKey'} = $FALSE;
                }
            }
            elsif ($field eq "useL3HashRot")
            {
                if ($value eq 'on')
                {
                    $l2HashRot->{'useL3HashRot'} = $TRUE;
                }
                else
                {
                    $l2HashRot->{'useL3HashRot'} = $FALSE;
                }
            }
            elsif ($field eq "usePTable")
            {
                if ($value eq 'on')
                {
                    $l2HashRot->{'usePTable'} = $TRUE;
                }
                else
                {
                    $l2HashRot->{'usePTable'} = $FALSE;
                }
            }
            elsif ($field eq "randomSelection")
            {
                if ($value eq 'on')
                {
                    $l2HashRot->{'randomSelection'} = $TRUE;
                }
                else
                {
                    $l2HashRot->{'randomSelection'} = $FALSE;
                }
            }
            else
            {
                print "ERROR: Wrong Input Data\n";
                return $FM_FAIL;
            }

            $status = $chip->fmSetSwitchAttribute($switchNum, $attribute, \%val);
            if ($status != $FM_OK)
            {
                print "ERROR: Can't push the L2 Hash Rotation configuration\n";
                return $status;
            }
        }
    }
    elsif ($attrib eq "l3Key")
    {
        my $l3HashKey = new SDK::fm_L3HashConfig();
        $l3HashKey->{'profileIndex'} = $profile;

        my $switchCount = scalar(@{[$self->tpGetSwitches]});
        foreach my $switchNum ($self->tpGetSwitches)
        {
            my %val = (type => "fm_L3HashConfig", value => $l3HashKey);
            my $status = $chip->fmGetSwitchAttribute($switchNum, $FM_L3_HASH_CONFIG, \%val);
            if ($status != $FM_OK)
            {
                print "ERROR: Can't retreive the L3 Hash Key configuration\n";
                return $status;
            }

            if ($field eq "sip")
            {
                $l3HashKey->{'SIPMask'} = $value;
            }
            elsif ($field eq "dip")
            {
                $l3HashKey->{'DIPMask'} = $value;
            }
            elsif ($field eq "l4-src-port")
            {
                $l3HashKey->{'L4SrcMask'} = $value;
            }
            elsif ($field eq "l4-dst-port")
            {
                $l3HashKey->{'L4DstMask'} = $value;
            }
            elsif ($field eq "dscp")
            {
                $l3HashKey->{'DSCPMask'} = $value;
            }
            elsif ($field eq "isl-user")
            {
                $l3HashKey->{'ISLUserMask'} = $value;
            }
            elsif ($field eq "protocol")
            {
                $l3HashKey->{'protocolMask'} = $value;
            }
            elsif ($field eq "custom")
            {
                $l3HashKey->{'customFieldMask'} = $value;
            }
            elsif ($field eq "flow")
            {
                $l3HashKey->{'flowMask'} = $value;
            }
            elsif ($field eq "symmetrizeL3")
            {
                if ($value eq 'on')
                {
                    $l3HashKey->{'symmetrizeL3Fields'} = $TRUE;
                }
                else
                {
                    $l3HashKey->{'symmetrizeL3Fields'} = $FALSE;
                }
            }
            elsif ($field eq "symmetrizeL4")
            {
                if ($value eq 'on')
                {
                    $l3HashKey->{'symmetrizeL4Fields'} = $TRUE;
                }
                else
                {
                    $l3HashKey->{'symmetrizeL4Fields'} = $FALSE;
                }
            }
            elsif ($field eq "usePTable")
            {
                if ($value eq 'on')
                {
                    $l3HashKey->{'usePTable'} = $TRUE;
                }
                else
                {
                    $l3HashKey->{'usePTable'} = $FALSE;
                }
            }
            elsif ($field eq "random-nexthop")
            {
                if ($value eq 'on')
                {
                    $l3HashKey->{'randomNextHop'} = $TRUE;
                }
                else
                {
                    $l3HashKey->{'randomNextHop'} = $FALSE;
                }
            }
            elsif ($field eq "random-other")
            {
                if ($value eq 'on')
                {
                    $l3HashKey->{'randomOther'} = $TRUE;
                }
                else
                {
                    $l3HashKey->{'randomOther'} = $FALSE;
                }
            }
            elsif ($field eq "disable")
            {
                if ($value eq 'on')
                {
                    $l3HashKey->{'disableAllButRandom'} = $TRUE;
                }
                else
                {
                    $l3HashKey->{'disableAllButRandom'} = $FALSE;
                }
            }
            else
            {
                print "ERROR: Wrong Input Data\n";
                return $FM_FAIL;
            }

            $status = $chip->fmSetSwitchAttribute($switchNum, $FM_L3_HASH_CONFIG, \%val);
            if ($status != $FM_OK)
            {
                print "ERROR: Can't push the L3 Hash Key configuration\n";
                return $status;
            }
        }
    }
    else
    {
        print "ERROR: Wrong Input Data\n";
        return $FM_FAIL;
    }

    return $FM_OK;
}

sub tp6000HandleShowHashingProf
{
    my ($self, $profile, $attrib) = @_;
    my $chip = $self->{CHIP};
    my $format = " %-22s %s\n";

    if(!defined $attrib)
    {
        print "Attribute not defined.\n";
        return;
    }

    if ($attrib eq "l2Key")
    {
        my $l2HashKey = new SDK::fm_L2HashKey();
        $l2HashKey->{'profileIndex'} = $profile;

        my $switchCount = scalar(@{[$self->tpGetSwitches]});
        foreach my $switchNum ($self->tpGetSwitches)
        {
            if ($switchCount > 1)
            {
                print "Switch $switchNum:\n";
            }

            my %val = (type => "fm_L2HashKey", value => $l2HashKey);
            my $status = $chip->fmGetSwitchAttribute($switchNum, $FM_L2_HASH_KEY, \%val);
            if ($status != $FM_OK)
            {
                print "ERROR: Can't retreive the L2 Hash Key configuration\n";
                return $status;
            }

            printf($format, "Profile Index", $l2HashKey->{'profileIndex'});
            printf($format, "SMAC Mask", $l2HashKey->{'SMACMask'});
            printf($format, "DMAC Mask", $l2HashKey->{'DMACMask'});
            printf($format, "EtherType Mask", sprintf("0x%04x", $l2HashKey->{'etherTypeMask'}));
            printf($format, "Vlan1 Mask", sprintf("0x%03x", $l2HashKey->{'vlanId1Mask'}));
            printf($format, "Vlan1 Pri Mask", sprintf("0x%01x", $l2HashKey->{'vlanPri1Mask'}));
            printf($format, "Vlan2 Mask", sprintf("0x%03x", $l2HashKey->{'vlanId2Mask'}));
            printf($format, "Vlan2 Pri Mask", sprintf("0x%01x", $l2HashKey->{'vlanPri2Mask'}));
            printf($format, "Hash Rotation Mantissa", sprintf("0x%04x", $l2HashKey->{'defaultHashRot'}->{'mantissa'}));
            printf($format, "Hash Rotation Exponent", sprintf("0x%01x", $l2HashKey->{'defaultHashRot'}->{'exponent'}));
            printf($format, "Symmetrize MAC", ($l2HashKey->{'symmetrizeMAC'} != 0) ? "on" : "off");
        }
    }
    elsif (($attrib eq "rotA") || ($attrib eq "rotB"))
    {
        my $l2HashRot = new SDK::fm_L2HashRot();
        $l2HashRot->{'profileIndex'} = $profile;

        my $attribute = 0;
        if ($attrib eq "rotA")
        {
            $attribute = $FM_L2_HASH_ROT_A;
        }
        else
        {
            $attribute = $FM_L2_HASH_ROT_B;
        }

        my $switchCount = scalar(@{[$self->tpGetSwitches]});
        foreach my $switchNum ($self->tpGetSwitches)
        {
            if ($switchCount > 1)
            {
                print "Switch $switchNum:\n";
            }

            my %val = (type => "fm_L2HashRot", value => $l2HashRot);
            my $status = $chip->fmGetSwitchAttribute($switchNum, $attribute, \%val);
            if ($status != $FM_OK)
            {
                print "ERROR: Can't retreive the L2 Hash Rotation configuration\n";
                return $status;
            }

            printf($format, "Profile Index", $l2HashRot->{'profileIndex'});
            printf($format, "Use L3 Hash Key", ($l2HashRot->{'useL3HashKey'} != 0) ? "on" : "off");
            printf($format, "Use L3 Hash Rotation", ($l2HashRot->{'useL3HashRot'} != 0) ? "on" : "off");
            printf($format, "Use PTable", ($l2HashRot->{'usePTable'} != 0) ? "on" : "off");
            printf($format, "Use Random Selection", ($l2HashRot->{'randomSelection'} != 0) ? "on" : "off");
        }
    }
    elsif ($attrib eq "l3Key")
    {
        my $l3HashKey = new SDK::fm_L3HashConfig();
        $l3HashKey->{'profileIndex'} = $profile;

        my $switchCount = scalar(@{[$self->tpGetSwitches]});
        foreach my $switchNum ($self->tpGetSwitches)
        {
            if ($switchCount > 1)
            {
                print "Switch $switchNum:\n";
            }

            my %val = (type => "fm_L3HashConfig", value => $l3HashKey);
            my $status = $chip->fmGetSwitchAttribute($switchNum, $FM_L3_HASH_CONFIG, \%val);
            if ($status != $FM_OK)
            {
                print "ERROR: Can't retreive the L3 Hash Key configuration\n";
                return $status;
            }

            printf($format, "Profile Index", $l3HashKey->{'profileIndex'});
            printf($format, "SIP Mask", sprintf("0x%04x", $l3HashKey->{'SIPMask'}));
            printf($format, "DIP Mask", sprintf("0x%04x", $l3HashKey->{'DIPMask'}));
            printf($format, "L4 Src Mask", sprintf("0x%04x", $l3HashKey->{'L4SrcMask'}));
            printf($format, "L4 Dst Mask", sprintf("0x%04x", $l3HashKey->{'L4DstMask'}));
            printf($format, "DSCP Mask", sprintf("0x%02x", $l3HashKey->{'DSCPMask'}));
            printf($format, "ISL User Mask", sprintf("0x%02x", $l3HashKey->{'ISLUserMask'}));
            printf($format, "Protocol Mask", sprintf("0x%02x", $l3HashKey->{'protocolMask'}));
            printf($format, "Custom Mask", sprintf("0x%02x", $l3HashKey->{'customFieldMask'}));
            printf($format, "Flow Mask", sprintf("0x%05x", $l3HashKey->{'flowMask'}));
            printf($format, "Symmetrize L3", ($l3HashKey->{'symmetrizeL3Fields'} != 0) ? "on" : "off");
            printf($format, "Symmetrize L4", ($l3HashKey->{'symmetrizeL4Fields'} != 0) ? "on" : "off");
            printf($format, "Use PTable", ($l3HashKey->{'usePTable'} != 0) ? "on" : "off");
            printf($format, "Random NextHop", ($l3HashKey->{'randomNextHop'} != 0) ? "on" : "off");
            printf($format, "Random Other", ($l3HashKey->{'randomOther'} != 0) ? "on" : "off");
            printf($format, "L3 Hash Disabled", ($l3HashKey->{'disableAllButRandom'} != 0) ? "on" : "off");
        }
    }
    else
    {
        print "ERROR: Wrong Input Data\n";
        return $FM_FAIL;
    }

    return $FM_OK;
}

1;
