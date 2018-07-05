# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FM4000/AclSimpleCore.pm
# Creation Date:    10/25/07
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

package Applications::TestPoint::Common::FM4000::AclSimpleCore;
use strict;
use warnings;

use List::Util qw(sum);
use Scalar::Util qw(blessed);
use Math::BigInt;
use Math::BigFloat;
use Time::HiRes qw(gettimeofday);
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;
use Types::tp_ipAddr;
use Types::tp_l4Port;
use Types::tp_Protocol;
use Types::tp_macAddress;
use Types::tp_ethertype;
use Types::tp_aclSimple;
use Types::tp_aclSimpleRule;

my $FIRST_SLICE = 16;
my @available_slices = (16..31);
my @available_acls = (0..3);

my %acls = ();

# Select0 ... SelectTop
my %selects = ("sip" => [32,32,32,32,32], # sip
               "dip" => [0,0,0,0,32], # dip
               "l34" => [32,32,32,32,32,  0,0,0,0,31,  
                         12,12,11,11,31,  42,31,31,31,31], # dip, sip, l4src,l4dst
               "smac-ethertype" => [39,39, 38,38, 32,  37,37,37,37, 31], # type, smac
               "dmac-ethertype" => [39,39, 6,6, 32,  5,5,5,5, 31], # type, dmac
              );

my %actions = ("permit" => $FM_FFU_FLAG_CLEAR, 
               "deny" => $FM_FFU_FLAG_SET,
               "switch" => $FM_FFU_FLAG_NOP,
               );

my $SWITCH_NUM = 0;

###############################################################################
# Types::tp_aclSimple ACL functions
###############################################################################
sub PrintAclSimple
{
    my ($self, $acl, $number) = @_;

    
    print "ACL-Simple " . $acl->{"type"} . " $number";
    
    if ($self->IsAclAssociated($acl))
    {
        print " Association: ";
        foreach my $port (@{$acl->{"ports"}})
        {
            printf("%3d", $port);
        }
        print "\n";
    }
    else
    {
        print " Not associated\n";
    }

    foreach my $seqNum (sort numerical keys %{$acl->{"rules"}})
    {
        $self->PrintRule($acl->{"rules"}->{$seqNum}, $seqNum, $acl);
    }
}

sub IsAclAssociated
{
    my ($self, $acl) = @_;
    return (defined $acl->{"ports"}) && (scalar(@{$acl->{"ports"}}) != 0);
}

sub IsAclAssociatedPort
{
    my ($self, $acl, $port) = @_;

    if (!$self->IsAclAssociated($acl))
    {
        return $FALSE;
    }

    my @portList = $self->validateList($port, $self->tpPlatformGetPortRange);
    if (!defined($port) || (scalar(@portList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FALSE;
    }

    foreach my $assocPort (@{$acl->{"ports"}})
    {
        foreach my $argPort (@portList)
        {
            if ($argPort == $assocPort)
            {
                return $TRUE;
            }
        }
    }

    return $FALSE;
}

sub UnsetAclSimple
{
    my ($self, $acl, $number) = @_;
    my $chip = $self->{CHIP};

    if (!$self->IsAclAssociated($acl))
    {
        print "ACL is already not associated\n";
        return $FM_FAIL;
    }

    my $status;
    my $sliceInfo = $acl->{"sliceInfo"};

    foreach my $switchNum ($self->tpGetSwitches)
    {
        foreach my $seqNum (sort numerical keys %{$acl->{"rules"}})
        {
            my $index = $acl->{"rules"}->{$seqNum}->{"index"};
            $status = $chip->fm4000FFUSetRuleValid($switchNum, $sliceInfo, $index, $FALSE, $TRUE);
            if ($status != $FM_OK)
            {
                print "ERROR $status in fm4000FFUSetRuleValid\n";
                return $status;
            }
            $acl->{"rules"}->{$seqNum}->{"index"} = undef;
        }

        $status = $chip->fm4000FFUUnconfigureSlice($switchNum, $sliceInfo, $TRUE);
        if ($status != $FM_OK)
        {
            print "ERROR $status in fm4000FFUUnconfigureSlice\n";
            return $status;
        }
    }

    $self->FreeSlices($sliceInfo->{"keyStart"}, $sliceInfo->{"keyEnd"});
    $self->UnSetPortMap($acl->{"associationNumber"}, @{$acl->{"ports"}});
    unshift @available_acls, $acl->{"associationNumber"};
    $acl->{"ports"} = [];
    return $FM_OK;
}

sub DeleteAclSimple
{
    my ($self, $acl, $number) = @_;

    if ($self->IsAclAssociated($acl))
    {
        print "ERROR: ACL is associated. Can not delete.\n";
        return $FM_FAIL;
    }

    delete $acls{$acl->{"type"}}->{$number};
    return $FM_OK;
}

###############################################################################
# Types::tp_aclSimpleRule ACL rule functions
###############################################################################
sub PrintRule
{
    my ($self, $rule, $seqNum, $acl) = @_;
    my $chip = $self->{CHIP};
    my $type = $acl->{"type"};
    my @params = @{$rule->{"params"}};
    my $switchNum = $SWITCH_NUM;

    printf("  seq %3d %-4s %-6s", $seqNum, $type, $rule->{"action"});

    if (($type eq "sip") or ($type eq "dip"))
    {
        my $ip;
        my $status = $self->InetNToP($params[0], \$ip);
        if ($status != $FM_OK)
        {
            print "IP conversion ERROR\n";
        }
        printf(" %-18s",  $ip);
    }
    elsif ($type eq "l34")
    {
        my ($sip, $dip, $sport, $dport, $prot);
        my $status = $self->InetNToP($params[0], \$sip);
        if ($status != $FM_OK)
        {
            print "IP conversion ERROR\n";
        }

        $status = $self->InetNToP($params[1], \$dip);
        if ($status != $FM_OK)
        {
            print "IP conversion ERROR\n";
        }

        $status = $self->L4PortNToP($params[2], \$sport);
        if ($status != $FM_OK)
        {
            print "port conversion ERROR\n";
        }

        $status = $self->L4PortNToP($params[3], \$dport);
        if ($status != $FM_OK)
        {
            print "port conversion ERROR\n";
        }

        $status = $self->ProtNToP($params[4], \$prot);
        if ($status != $FM_OK)
        {
            print "protocol conversion ERROR\n";
        }

        printf(" %-18s %-18s 0x%-9s 0x%-9s 0x%-5s", $sip, $dip, $sport, $dport, $prot);
    }
    elsif(($type eq "smac-ethertype") or ($type eq "dmac-ethertype"))
    {
        my ($mac, $ethertype);
        my $status = $self->MacAddressNToP($params[0], \$mac);
        if ($status != $FM_OK)
        {
            print "MAC Address conversion ERROR\n";
        }

        $status = $self->EthertypeNToP($params[1], \$ethertype);
        if ($status != $FM_OK)
        {
            print "EtherType conversion ERROR\n";
        }

        printf(" %-18s %-9s", $mac, $ethertype);
    }
    else
    {
        print "ERROR: unknown type";
    }

    if ($rule->{"count"})
    {
        if ($rule->{"action"} ne "switch")
        {
            print " count";
        } else {
            print " Addr: " . $rule->{"count"};
        }
        if (defined $rule->{"index"})
        {
            my $frames = 0;
            my $bytes = 0;

            my $bank = $acl->{"associationNumber"};

            my $status = $chip->fm4000FFUGetCounter($switchNum, $bank, $rule->{"index"},
                                                \$frames, \$bytes);
            if ($status == $FM_OK)
            {
                print " ($frames frames, $bytes bytes)";
            }
            else
            {
                print " (ERROR reading count)\n";
            }
        }
    }

    print "\n";
}

###############################################################################
# Types::tp_ipAddr functions
###############################################################################

sub Inet4NToFfu
{
    my $self                    = shift(@_);
    my Types::tp_ipAddr $ipAddr = shift(@_);
    my $value                   = shift(@_);
    my $mask                    = shift(@_);

    if (!blessed($ipAddr) || !$ipAddr->isa('Types::tp_ipAddr'))
    {
        print "Inet4NToFfu ERROR: not a Types::tp_ipAddr\n";
        return $FM_FAIL;
    }
    push(@$value, Math::BigInt->new($ipAddr->{"address"}->[0]));
    push(@$mask, Math::BigInt->new((0xffffffff << (32-$ipAddr->{"prefix"})) & 0xffffffff));

    return $FM_OK;
}


###############################################################################
# Types::tp_l4Port functions
###############################################################################

sub L4PortNToFfu
{
    my $self                    = shift(@_);
    my Types::tp_l4Port $l4Port = shift(@_);
    my $value                   = shift(@_);
    my $mask                    = shift(@_);

    if (!blessed($l4Port) || !$l4Port->isa('Types::tp_l4Port'))
    {
        print "L4PortNToFfu ERROR: not a Types::tp_l4Port\n";
        return $FM_FAIL;
    }
    push(@$value, Math::BigInt->new($l4Port->{"port"}));
    push(@$mask, Math::BigInt->new($l4Port->{"mask"}));

    return $FM_OK;
}

##@cmethod private int L4PortPToN(char *portStr, Types::tp_l4Port &l4Port)
#
# @desc         Converts a layer 4 port & mask to network format
#
# @param[in]    port The port/mask string to be converted
#
# @param[out]   l4Port Points to a Types::tp_l4Port Perl data structure in
#               which the network representation of the layer 4 port is
#               to be stored
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub L4PortPToN
{
    my $self                    = shift(@_);
    my $portStr                 = shift(@_);
    my Types::tp_l4Port $l4Port = shift(@_);

    # Remove leading and trailing whitespace.
    $portStr =~ s/^\s+(.*)\s+$//;

    $l4Port->{"port"} = $portStr;
    # strip off mask if it is there
    if ($l4Port->{"port"} =~ m/\//)
    {
        $l4Port->{"port"} =~ s/^([^\/]*)\/.*$/$1/;
    }
    $l4Port->{"port"} = $self->str2intnum($l4Port->{"port"});
    if (!defined($l4Port->{"port"}) or 
        ($l4Port->{"port"} < 0) or ($l4Port->{"port"} > 0xffff))
    {
        return $FM_FAIL;
    }

    $l4Port->{"mask"} = 0xffff;
    if ($portStr =~ m/\//)
    {
        $portStr =~ s/^[^\/]*\/(.*)$/$1/;
        $l4Port->{"mask"} = $self->str2intnum($portStr);
        if (!defined($l4Port->{"mask"}) or 
            ($l4Port->{"mask"} < 0) or ($l4Port->{"mask"} > 0xffff))
        {
            return $FM_FAIL;
        }
    }

    return $FM_OK;
}

##@cmethod private int L4PortNToP(Types::tp_l4Port &l4Port, char **portStr)
#
# @desc         Converts a layer 4 port structure into a layer 4 port
#               character string
#
# @param[in]    l4Port Points to a Types::tp_l4Port Perl data structure to be
#               converted
#
# @param[out]   portStr Points to user-allocated storage in which the layer 4
#               port character string is to be stored
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub L4PortNToP
{
    my $self                    = shift(@_);
    my Types::tp_l4Port $l4Port = shift(@_);
    my $portStr                 = shift(@_);

    if (!blessed($l4Port) || !$l4Port->isa('Types::tp_l4Port'))
    {
        return $FM_FAIL;
    }
    
    $$portStr = sprintf("%x", $l4Port->{"port"});

    if ($l4Port->{"mask"} != 0xffff)
    {
        $$portStr .= sprintf("/%x", $l4Port->{"mask"});
    }

    return $FM_OK;
}

###############################################################################
# Types::tp_Protocol functions
###############################################################################

sub ProtNToFfu
{
    my $self                    = shift(@_);
    my Types::tp_Protocol $Prot = shift(@_);
    my $value                   = shift(@_);
    my $mask                    = shift(@_);

    if (!blessed($Prot) || !$Prot->isa('Types::tp_Protocol'))
    {
        print "ProtNToFfu ERROR: not a Types::tp_Protocol\n";
        return $FM_FAIL;
    }
    push(@$value, Math::BigInt->new($Prot->{"prot"}));
    push(@$mask, Math::BigInt->new($Prot->{"mask"}));

    return $FM_OK;
}

##@cmethod private int ProtPToN(char *protStr, Types::tp_Protocol &Prot)
#
# @desc         Converts a protocol & mask to network format
#
# @param[in]    prot The protocol/mask string to be converted
#
# @param[out]   Prot Points to a Types::tp_Protocol Perl data structure in
#               which the network representation of the protocol is
#               to be stored
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub ProtPToN
{
    my $self                    = shift(@_);
    my $protStr                 = shift(@_);
    my Types::tp_Protocol $Prot = shift(@_);

    # Remove leading and trailing whitespace.
    $protStr =~ s/^\s+(.*)\s+$//;

    $Prot->{"prot"} = $protStr;
    # strip off mask if it is there
    if ($Prot->{"prot"} =~ m/\//)
    {
        $Prot->{"prot"} =~ s/^([^\/]*)\/.*$/$1/;
    }
    $Prot->{"prot"} = $self->str2intnum($Prot->{"prot"});
    if (!defined($Prot->{"prot"}) or 
        ($Prot->{"prot"} < 0) or ($Prot->{"prot"} > 0xffff))
    {
        return $FM_FAIL;
    }

    $Prot->{"mask"} = 0xffff;
    if ($protStr =~ m/\//)
    {
        $protStr =~ s/^[^\/]*\/(.*)$/$1/;
        $Prot->{"mask"} = $self->str2intnum($protStr);
        if (!defined($Prot->{"mask"}) or 
            ($Prot->{"mask"} < 0) or ($Prot->{"mask"} > 0xffff))
        {
            return $FM_FAIL;
        }
    }

    return $FM_OK;
}

##@cmethod private int ProtNToP(Types::tp_Protocol &Prot, char **protStr)
#
# @desc         Converts an IP protocol structure into a protocol
#               character string
#
# @param[in]    Prot Points to a Types::tp_Protocol Perl data structure to be
#               converted
#
# @param[out]   protStr Points to user-allocated storage in which the
#               protocol character string is to be stored
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub ProtNToP
{
    my $self                    = shift(@_);
    my Types::tp_Protocol $Prot = shift(@_);
    my $protStr                 = shift(@_);

    if (!blessed($Prot) || !$Prot->isa('Types::tp_Protocol'))
    {
        return $FM_FAIL;
    }
    
    $$protStr = sprintf("%x", $Prot->{"prot"});

    if ($Prot->{"mask"} != 0xffff)
    {
        $$protStr .= sprintf("/%x", $Prot->{"mask"});
    }

    return $FM_OK;
}


###############################################################################
# Types::tp_macAddress functions
###############################################################################

# note: this expects that something has already been stored in the lower 16 bits of the last key
sub MacAddressNToFfu
{
    my $self                            = shift(@_);
    my Types::tp_macAddress $macAddress = shift(@_);
    my $value                           = shift(@_);
    my $mask                            = shift(@_);

    if (!blessed($macAddress) || !$macAddress->isa('Types::tp_macAddress'))
    {
        print "MacAddressNToFfu ERROR: not a Types::tp_macAddress\n";
        return $FM_FAIL;
    }

    push(@$value,  pop(@$value) | $macAddress->{"macAddress"}->copy()->band(0xffff)->blsft(16));
    push(@$mask,  pop(@$mask) | $macAddress->{"mask"}->copy()->band(0xffff)->blsft(16));

    push(@$value, $macAddress->{"macAddress"}->copy()->brsft(16)->band(0xffffffff));
    push(@$mask, $macAddress->{"mask"}->copy()->brsft(16)->band(0xffffffff));

    return $FM_OK;
}

##@cmethod private int MacAddressSinglePToN(char *macAddressStr)
#
# @desc         Converts a single macAddress to network format
#
# @param[in]    macAddressStr The macAddress string to be converted
#
# @return       Math::BigInt if successful
# @return       undef otherwise
sub MacAddressSinglePToN
{
    my $self           = shift(@_);
    my $macAddressStr  = shift(@_);
    my $output         = undef;

    my @address = split(/:/, $macAddressStr);

    # Ensure that all parts of the CIDR style IP address are hexadecimal
    # numbers.
    foreach my $part (@address)
    {
        $part = $self->str2hexnum($part);
        if (!defined($part))
        {
            return undef;
        }
    }

    # Ensure that 6 8-bit parts have been specified.
    if (scalar(@address) != 6
        || sum(map {int($_ >= 0x0 && $_ <= 0xFF)} @address) != 6)
    {
        return undef;
    }

    $output = Math::BigInt->new(0);
    foreach my $part (@address)
    {
        $output->blsft(8)->bior($part);
    }

    return $output;
}

##@cmethod private int MacAddressPToN(char *macAddressStr, Types::tp_macAddress &macAddress)
#
# @desc         Converts a macAddress to network format
#
# @param[in]    macAddress The macAddress string to be converted
#
# @param[out]   macAddress Points to a Types::tp_macAddress Perl data structure in
#               which the network representation of the macAddress is
#               to be stored
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub MacAddressPToN
{
    my $self                            = shift(@_);
    my $macAddressStr                   = shift(@_);
    my Types::tp_macAddress $macAddress = shift(@_);

    # Remove leading and trailing whitespace.
    $macAddressStr =~ s/^\s+(.*)\s+$//;

    $macAddress->{"macAddress"} = $macAddressStr;
    # strip off mask if it is there
    if ($macAddress->{"macAddress"} =~ m/\//)
    {
        $macAddress->{"macAddress"} =~ s/^([^\/]*)\/.*$/$1/;
    }

    $macAddress->{"macAddress"} = $self->MacAddressSinglePToN($macAddress->{"macAddress"});

    if (!defined($macAddress->{"macAddress"}))
    {
        return $FM_FAIL;
    }

    $macAddress->{"mask"} = Math::BigInt->new("0xffffffffffff");
    if ($macAddressStr =~ m/\//)
    {
        $macAddressStr =~ s/^[^\/]*\/(.*)$/$1/;
        $macAddress->{"mask"} = $self->MacAddressSinglePToN($macAddressStr);
        if (!defined($macAddress->{"mask"}))
        {
            return $FM_FAIL;
        }
    }

    return $FM_OK;
}

##@cmethod private int MacAddressNToP(Types::tp_macAddress &macAddress, char **macAddressStr)
#
# @desc         Converts a macAddress structure into a character string
#
# @param[in]    macAddress Points to a Types::tp_macAddress Perl data structure to be
#               converted
#
# @param[out]   cidr Points to user-allocated storage in which the macAddress
#               character string is to be stored
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub MacAddressNToP
{
    my $self                            = shift(@_);
    my Types::tp_macAddress $macAddress = shift(@_);
    my $macAddressStr                   = shift(@_);

    if (!blessed($macAddress) || !$macAddress->isa('Types::tp_macAddress'))
    {
        return $FM_FAIL;
    }
    
    $$macAddressStr = join(':', map {
        sprintf("%02x", $macAddress->{"macAddress"}->copy()->brsft(8 * $_)->band(0xff)->as_number());
    } reverse(0 .. 5));

    if ($macAddress->{"mask"} != Math::BigInt->new("0xffffffffffff"))
    {
        $$macAddressStr .= "/" . join(':', map {
            sprintf("%02x", $macAddress->{"mask"}->copy()->brsft(8 * $_)->band(0xff)->as_number());
        } reverse(0 .. 5));
    }

    return $FM_OK;
}


###############################################################################
# Types::tp_ethertype functions
###############################################################################

sub EthertypeNToFfu
{
    my $self                          = shift(@_);
    my Types::tp_ethertype $ethertype = shift(@_);
    my $value                         = shift(@_);
    my $mask                          = shift(@_);

    if (!blessed($ethertype) || !$ethertype->isa('Types::tp_ethertype'))
    {
        print "EthertypeNToFfu ERROR: not a Types::tp_ethertype\n";
        return $FM_FAIL;
    }
    push(@$value, Math::BigInt->new($ethertype->{"ethertype"}));
    push(@$mask, Math::BigInt->new($ethertype->{"mask"}));

    return $FM_OK;
}

##@cmethod private int EthertypePToN(char *ethertypeStr, Types::tp_ethertype &ethertype)
#
# @desc         Converts a ethertype to network format
#
# @param[in]    ethertype The ethertype string to be converted
#
# @param[out]   ethertype Points to a Types::tp_ethertype Perl data structure in
#               which the network representation of the ethertype is
#               to be stored
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub EthertypePToN
{
    my $self                          = shift(@_);
    my $ethertypeStr                  = shift(@_);
    my Types::tp_ethertype $ethertype = shift(@_);

    # Remove leading and trailing whitespace.
    $ethertypeStr =~ s/^\s+(.*)\s+$//;

    $ethertype->{"ethertype"} = $ethertypeStr;
    # strip off mask if it is there
    if ($ethertype->{"ethertype"} =~ m/\//)
    {
        $ethertype->{"ethertype"} =~ s/^([^\/]*)\/.*$/$1/;
    }
    $ethertype->{"ethertype"} = $self->str2intnum($ethertype->{"ethertype"});
    if (!defined($ethertype->{"ethertype"}) or 
        ($ethertype->{"ethertype"} < 0) or ($ethertype->{"ethertype"} > 0xffff))
    {
        return $FM_FAIL;
    }

    $ethertype->{"mask"} = 0xffff;
    if ($ethertypeStr =~ m/\//)
    {
        $ethertypeStr =~ s/^[^\/]*\/(.*)$/$1/;
        $ethertype->{"mask"} = $self->str2intnum($ethertypeStr);
        if (!defined($ethertype->{"mask"}) or 
            ($ethertype->{"mask"} < 0) or ($ethertype->{"mask"} > 0xffff))
        {
            return $FM_FAIL;
        }
    }

    return $FM_OK;
}

##@cmethod private int EthertypeNToP(Types::tp_ethertype &ethertype, char **ethertypeStr)
#
# @desc         Converts a ethertype structure into a character string
#
# @param[in]    ethertype Points to a Types::tp_ethertype Perl data structure to be
#               converted
#
# @param[out]   cidr Points to user-allocated storage in which the ethertype
#               character string is to be stored
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub EthertypeNToP
{
    my $self                          = shift(@_);
    my Types::tp_ethertype $ethertype = shift(@_);
    my $ethertypeStr                  = shift(@_);

    if (!blessed($ethertype) || !$ethertype->isa('Types::tp_ethertype'))
    {
        return $FM_FAIL;
    }
    
    $$ethertypeStr = sprintf("%x", $ethertype->{"ethertype"});

    if ($ethertype->{"mask"} != 0xffff)
    {
        $$ethertypeStr .= sprintf("/%x", $ethertype->{"mask"});
    }

    return $FM_OK;
}


###############################################################################
# helper functions
###############################################################################

sub numerical { $a <=> $b }

sub CheckValidType
{
    my ($self, $type) = @_;

    if ((!defined $type) or (!defined $selects{$type}))
    {
        print "Type $type is not a valid type.\n";
        return $FM_FAIL;
    }
    return $FM_OK;
}

sub CheckValidAction
{
    my ($self, $action) = @_;

    if ((defined $action) and 
       (($action eq "permit") or ($action eq "deny") or ($action eq "switch")))
    {
        return $FM_OK;
    }
    my $printact = "";
    if (!defined $action)
    {
        $printact = $action;
    }
    print "Action $action is not valid (permit/deny/switch).\n";
    return $FM_FAIL;
}

sub FreeSlices
{
    my ($self, $startSlice, $endSlice) = @_;

    push @available_slices, $startSlice .. $endSlice;
}

# find the first block of $numSlices slices and use them
sub GetSlices
{
    my ($self, $numSlices) = @_;

    if(scalar(@available_slices) == 0)
    {
        return undef;
    }

    my $start   = $available_slices[0];
    my $current = $start;

    foreach my $slice (sort numerical @available_slices)
    {
        if ($slice != $current + 1)
        {
            $start = $slice;
        }
        $current = $slice;

        if ($slice - $start + 1 == $numSlices)
        {
            # we're done. mark slices as used.
            my @unused_slices = ();
            foreach my $unused (sort numerical @available_slices)
            {
                if ($unused < $start or $unused > $slice)
                {
                    push @unused_slices, $unused;
                }
            }
            @available_slices = @unused_slices;

            # return the first slice in the series
            return $start;
        }
    }

    return undef;
}

# private: to be called by SetPortMap/UnSetPortMap
sub SetPortMapValue
{
    my ($self, $associationNumber, $value, @portlist) = @_;

    my $chip = $self->{CHIP};

    my $status;
    my $mapSrc = 0;
    my $routable = 0;

    foreach my $port (@portlist)
    {
        my $switchNum;
        my $pport;
        $chip->fmPlatformMapLogicalPortToPhysical($SWITCH_NUM, $port, \$switchNum, \$pport);

        $status = $chip->fm4000FFUGetSourceMapper($switchNum, $pport,
                                              \$mapSrc, \$routable, $TRUE);

        if ($status != $FM_OK)
        {
            print "Error $status in fm4000FFUGetSourceMapper\n";
            return $status;
        }

        printf("SetPortMap port %d (phys port %d): was %x", $port, $pport, $mapSrc);
        if ($value)
        {
            $mapSrc |= 1 << $associationNumber;
        }
        else
        {
            $mapSrc &= ~(1 << $associationNumber);
        }
        printf(", now %x\n", $mapSrc);

        $status = $chip->fm4000FFUSetSourceMapperMapSrc($SWITCH_NUM, $pport, $mapSrc, $TRUE);
        
        if ($status != $FM_OK)
        {
            print "Error $status in fm4000FFUGetSourceMapper\n";
            return $status;
        }
    }

    return $FM_OK;
}

sub UnSetPortMap
{
    my ($self, $associationNumber, @portlist) = @_;

    return $self->SetPortMapValue($associationNumber, 0, @portlist);
}

sub SetPortMap
{
    my ($self, $associationNumber, @portlist) = @_;
    
    return $self->SetPortMapValue($associationNumber, 1, @portlist);
}

###############################################################################
# TestPoint functions
###############################################################################
sub tp4000HandleCreateAclSimple
{
    my ($self, $type, $number) = @_;

    if ($self->CheckValidType($type) != $FM_OK)
    {
        print "Invalid ACL type\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (defined $acls{$type}->{$number})
    {
        print "ERROR: That ACL already exists\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }   

    my Types::tp_aclSimple $acl = Types::tp_aclSimple->new($type);
    $acls{$type}->{$number} = $acl;
    print "ACL added $acl\n";

    return $FM_OK;
}

sub HandleAddAclSimpleRuleIp
{
    my ($self, $number, $seqNum, $sip, $action, $count, $type) = @_;

    if (!defined $acls{$type}->{$number})
    {
        print "That ACL does not exist. Create the ACL before adding rules.\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $acl = $acls{$type}->{$number};
    if ($self->IsAclAssociated($acl))
    {
        print "Can not add rules to an associated ACL.\n";
        return $FM_FAIL;
    }

    if (defined $acl->{"rules"}->{$seqNum})
    {
        print "ERROR: That ACL rule already exists\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }   

    my Types::tp_ipAddr $ipAddr = Types::tp_ipAddr->new();
    my $status = $self->InetPToN($sip, $ipAddr);
    if ($status != $FM_OK)
    {
        print "invalid IP addresss $sip\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if($self->CheckValidAction($action) != $FM_OK)
    {
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my Types::tp_aclSimpleRule $rule = Types::tp_aclSimpleRule->new($action, $count, $ipAddr);
    if ($action eq "switch")
    {
        # HACK!  So that I didn't have to rip up the current command 
        #        structure, I borrowed the count variable 
        $rule->setGlort($count);
    }
    $acl->{"rules"}->{$seqNum} = $rule;

    print "ACL rule added " . $acl->{"rules"}->{$seqNum} . "\n";

    return $FM_OK;
}

sub tp4000HandleAddAclSimpleRuleSip
{
    my ($self, $number, $seqNum, $sip, $action, $count) = @_;

    return $self->HandleAddAclSimpleRuleIp($number, $seqNum, $sip, $action, $count, "sip");
}

sub tp4000HandleAddAclSimpleRuleDip
{
    my ($self, $number, $seqNum, $sip, $action, $count) = @_;

    return $self->HandleAddAclSimpleRuleIp($number, $seqNum, $sip, $action, $count, "dip");
}

sub tp4000HandleAddAclSimpleRuleL34
{
    my ($self, $number, $seqNum, 
        $sip, $dip, $l4src, $l4dst, $prot,
        $action, $count) = @_;
    my $type = "l34";

    if (!defined $acls{$type}->{$number})
    {
        print "That ACL does not exist. Create the ACL before adding rules.\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $acl = $acls{$type}->{$number};
    if ($self->IsAclAssociated($acl))
    {
        print "Can not add rules to an associated ACL.\n";
        return $FM_FAIL;
    }

    my Types::tp_ipAddr $ipAddrSip = Types::tp_ipAddr->new();
    my $status = $self->InetPToN($sip, $ipAddrSip);
    if ($status != $FM_OK)
    {
        print "invalid SIP addresss $sip\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my Types::tp_ipAddr $ipAddrDip = Types::tp_ipAddr->new();
    $status = $self->InetPToN($dip, $ipAddrDip);
    if ($status != $FM_OK)
    {
        print "invalid DIP addresss $dip\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my Types::tp_l4Port $l4PortSrc = Types::tp_l4Port->new();
    $status = $self->L4PortPToN($l4src, $l4PortSrc);
    if ($status != $FM_OK)
    {
        print "invalid source port $l4src\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my Types::tp_l4Port $l4PortDst = Types::tp_l4Port->new();
    $status = $self->L4PortPToN($l4dst, $l4PortDst);
    if ($status != $FM_OK)
    {
        print "invalid dest port $l4dst\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my Types::tp_Protocol $Prot = Types::tp_Protocol->new();
    $status = $self->ProtPToN($prot, $Prot);
    if ($status != $FM_OK)
    {
        print "invalid protocol $prot\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if($self->CheckValidAction($action) != $FM_OK)
    {
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my Types::tp_aclSimpleRule $rule = 
        Types::tp_aclSimpleRule->new($action, $count, $ipAddrSip, $ipAddrDip,
                $l4PortSrc, $l4PortDst, $Prot);
    if ($action eq "switch")
    {
        # HACK!  So that I didn't have to rip up the current command 
        #        structure, I borrowed the count variable 
        $rule->setGlort($count);
    }
    $acl->{"rules"}->{$seqNum} = $rule;

    print "ACL rule added " . $acl->{"rules"}->{$seqNum} . "\n";

    return $FM_OK;
}

sub HandleAddAclSimpleRuleMacEthertype
{
    my ($self, $number, $seqNum, $mac, $etype, $action, $count, $type) = @_;

    if (!defined $acls{$type}->{$number})
    {
        print "That ACL does not exist. Create the ACL before adding rules.\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $acl = $acls{$type}->{$number};
    if ($self->IsAclAssociated($acl))
    {
        print "Can not add rules to an associated ACL.\n";
        return $FM_FAIL;
    }

    my Types::tp_macAddress $macAddress = Types::tp_macAddress->new();
    my $status = $self->MacAddressPToN($mac, $macAddress);
    if ($status != $FM_OK)
    {
        print "invalid MAC address $mac\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my Types::tp_ethertype $ethertype = Types::tp_ethertype->new();
    $status = $self->EthertypePToN($etype, $ethertype);
    if ($status != $FM_OK)
    {
        print "invalid ethertype $etype\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if($self->CheckValidAction($action) != $FM_OK)
    {
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my Types::tp_aclSimpleRule $rule = 
        Types::tp_aclSimpleRule->new($action, $count, $macAddress, $ethertype);
    $acl->{"rules"}->{$seqNum} = $rule;

    if ($action eq "switch")
    {
        # HACK!  So that I didn't have to rip up the current command 
        #        structure, I borrowed the count variable 
        $rule->setGlort($count);
    }
    print "ACL rule added " . $acl->{"rules"}->{$seqNum} . "\n";

    return $FM_OK;
}

sub tp4000HandleAddAclSimpleRuleSmacEthertype
{
    my ($self, $number, $seqNum, $smac, $ethertype, $action, $count) = @_;

    return $self->HandleAddAclSimpleRuleMacEthertype($number, $seqNum, 
                                                     $smac, $ethertype, 
                                                     $action, $count, 
                                                     "smac-ethertype");
}

sub tp4000HandleAddAclSimpleRuleDmacEthertype
{
    my ($self, $number, $seqNum, $smac, $ethertype, $action, $count) = @_;

    return $self->HandleAddAclSimpleRuleMacEthertype($number, $seqNum, 
                                                     $smac, $ethertype, 
                                                     $action, $count, 
                                                     "dmac-ethertype");
}

sub tp4000HandleRemoveAclSimpleRule
{
    my ($self, $type, $number, $seqNum) = @_;

    if ($self->CheckValidType($type) != $FM_OK)
    {
        print "Invalid ACL type\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }
    
    if (!defined $acls{$type}->{$number})
    {
        print "That ACL does not exist. Create the ACL before adding rules.\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $acl = $acls{$type}->{$number};
    if ($self->IsAclAssociated($acl))
    {
        print "Can not remove rules from an associated ACL.\n";
        return $FM_FAIL;
    }

    if (!defined $acl->{"rules"}->{$seqNum})
    {
        print "That rule does not exist\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    delete $acl->{"rules"}->{$seqNum};
    print "Rule removed\n";

    return $FM_OK;
}

sub tp4000HandleApplyAclSimple
{
    my ($self, $type, $number, $port) = @_;
    my $chip = $self->{CHIP};

    if ($self->CheckValidType($type) != $FM_OK)
    {
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (!defined $acls{$type}->{$number})
    {
        print "That ACL does not exist. Create the ACL first.\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (scalar(keys %{$acls{$type}->{$number}->{"rules"}}) == 0)
    {
        print "That ACL has no rules.\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if ($self->IsAclAssociated($acls{$type}->{$number}))
    {
        print "That ACL has already been associated.\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @portList = $self->validateList($port, $self->tpPlatformGetPortRange);
    if (!defined($port) || (scalar(@portList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $acl = $acls{$type}->{$number};
    my $numSlices = scalar(@{$selects{$type}}) / 5;

    if(scalar(@available_acls) == 0)
    {
        print "No free resources to apply ACL (max=4).\n";
        return $FM_FAIL;
    }

    my $sliceNum = $self->GetSlices($numSlices);
    if (!defined $sliceNum)
    {
        print "No free resources to apply ACL (Num slices req = $numSlices).\n";
        return $FM_FAIL;
    }

    print "OK\n";

    $acl->{"associationNumber"} = shift @available_acls;

    $acl->{"ports"} = \@portList;
    $self->SetPortMap($acl->{"associationNumber"}, @portList);

    my $sliceInfo = SDK::fm_ffuSliceInfo->new();
    $sliceInfo->{"validScenarios"} = 0xffffffff;
    $sliceInfo->{"kase"} = 0;
    $sliceInfo->{"keyStart"} = $sliceNum;
    $sliceInfo->{"keyEnd"} = $sliceInfo->{"keyStart"} + $numSlices - 1;
    $sliceInfo->{"actionEnd"} = $sliceInfo->{"keyEnd"};
    $sliceInfo->{"selects"} = $selects{$type};
    $acl->{"sliceInfo"} = $sliceInfo;

    foreach my $switchNum ($self->tpGetSwitches)
    {
        print "fm4000FFUConfigureSlice($switchNum, sliceInfo ($sliceNum), TRUE)\n";
        my $status = $chip->fm4000FFUConfigureSlice($switchNum, $sliceInfo, $TRUE);
        if ($status != $FM_OK)
        {
            print "ERROR $status in fm4000FFUConfigureSlice\n";
            return $status;
        }

        my $index = 1;
        
        foreach my $seqNum (sort numerical keys %{$acl->{"rules"}})
        {
            my Types::tp_aclSimpleRule $rule = $acl->{"rules"}->{$seqNum};
            my $value = [];
            my $mask = [];
            if (($acl->{"type"} eq "sip") or ($acl->{"type"} eq "dip"))
            {
                $self->Inet4NToFfu($rule->{"params"}->[0], $value, $mask);
            }
            elsif ($acl->{"type"} eq "l34")
            {
                $self->Inet4NToFfu($rule->{"params"}->[0], $value, $mask);
                $self->Inet4NToFfu($rule->{"params"}->[1], $value, $mask);
                my $portValue = [];
                my $portMask = [];
                $self->L4PortNToFfu($rule->{"params"}->[2], $portValue, $portMask);
                $self->L4PortNToFfu($rule->{"params"}->[3], $portValue, $portMask);
                push(@$value, $portValue->[0] | ($portValue->[1] << 16));
                push(@$mask, $portMask->[0] | ($portMask->[1] << 16));
                $self->ProtNToFfu($rule->{"params"}->[4], $value, $mask);
            }
            elsif (($acl->{"type"} eq "smac-ethertype") or ($acl->{"type"} eq "dmac-ethertype"))
            {
                $self->EthertypeNToFfu($rule->{"params"}->[1], $value, $mask);
                $self->MacAddressNToFfu($rule->{"params"}->[0], $value, $mask);
            }
            else
            {
                print "unknown type.\n";
                return $FM_FAIL;
            }

            if ($port ne "all")
            {
                printf("  value[%02d]=%08x mask=%08x\n", 0, $value->[0], $mask->[0]);
                $value->[0] = Math::BigInt->new($value->[0]);
                $value->[0]->bior(Math::BigInt->new(1)->blsft(32 + $acl->{"associationNumber"}));
                $mask->[0] = Math::BigInt->new($mask->[0]);
                $mask->[0]->bior(Math::BigInt->new(1)->blsft(32 + $acl->{"associationNumber"}));
                printf("  value[%02d]=%08x mask=%08x\n", 0, $value->[0], $mask->[0]);
                printf("  value[%02d]=%s mask=%s\n", 0, $value->[0]->as_hex(), $mask->[0]->as_hex());
            }

            my $ffuAction = SDK::fm_ffuAction->new();
            $ffuAction->{"precedence"} = 0;
            $ffuAction->{"counter"}    = $rule->{"count"} ? $index : 0;
            $ffuAction->{"bank"}       = $acl->{"associationNumber"};
            $ffuAction->{"data"}       = SDK::fm_ffuActionData->new();
            $ffuAction->{"data"}->{"flags"} = SDK::fm_ffuActionFlags->new();
            $ffuAction->{"data"}->{"flags"}->{"trap"}     = $FM_FFU_FLAG_NOP;
            $ffuAction->{"data"}->{"flags"}->{"log"}      = $FM_FFU_FLAG_NOP;
            $ffuAction->{"data"}->{"flags"}->{"noRoute"}  = $FM_FFU_FLAG_NOP;
            $ffuAction->{"data"}->{"flags"}->{"rxMirror"} = $FM_FFU_FLAG_NOP;
            if ($rule->{"action"} eq "switch")
            {
                $ffuAction->{"action"}     = $FM_FFU_ACTION_ROUTE_LOGICAL_PORT;
                $ffuAction->{"data"}->{"flags"}->{"drop"}     = 
                                             $FM_FFU_FLAG_NOP;
                $ffuAction->{"data"}->{"logicalPort"} =
                                             $rule->{"glort"};
                print "Switching to glort " . $rule->{"glort"} . "\n";
            } else {
                $ffuAction->{"action"}     = $FM_FFU_ACTION_SET_FLAGS;
                $ffuAction->{"data"}->{"flags"}->{"drop"}     = 
                                             $actions{$rule->{"action"}};
            }
            print "fm4000FFUSetRule($switchNum, sliceInfo ($sliceNum), $index, TRUE, $value, $mask, ffuAction, FALSE, TRUE)\n";
            for (my $i = 0 ; $i < $numSlices ; $i++)
            {
                printf("  value[%02d]=%08x mask=%08x\n", $i, $value->[$i], $mask->[$i]);
            }
            $status = $chip->fm4000FFUSetRule(
                                          $switchNum, $sliceInfo, $index, $TRUE, 
                                          $value, $mask, $ffuAction, $FALSE, $TRUE);
            if ($status != $FM_OK)
            {
                print "ERROR $status in fm4000FFUSetRule\n";
                return $status;
            }
            else
            {
                print "wrote rule to index $index\n";
                $rule->{"index"} = $index;
            }

            my $newFrameCount = Math::BigInt->new(0);
            my $newByteCount  = Math::BigInt->new(0);
            my $status = $chip->fm4000FFUSetCounter($switchNum, $ffuAction->{"bank"}, $index, 
                                                $newFrameCount, $newByteCount);
            if ($status != $FM_OK)
            {
                print "ERROR $status in fm4000FFUSetCounter\n";
                return $status;
            }

            $index++;
        }
    }

    return $FM_OK;
}

sub tp4000HandleShowAclSimple
{
    my ($self, $type, $number) = @_;
 
    foreach my $aclType (sort keys %acls)
    {
        if ((!defined $type) or ($type eq "") or ($type eq $aclType))
        {
            foreach my $aclNumber (sort numerical keys %{$acls{$aclType}})
            {
                if ((!defined $number) or ($number eq "") or ($number eq $aclNumber))
                {
                    $self->PrintAclSimple($acls{$aclType}->{$aclNumber}, $aclNumber);
                }
            }
        }
    }

    return $FM_OK;
}

sub tp4000HandleShowAclSimplePort
{
    my ($self, $port) = @_;
    my $found = $FALSE;

    print "ACLs on port $port:\n";
 
    foreach my $type (sort keys %acls)
    {
        foreach my $number (sort numerical keys %{$acls{$type}})
        {
            if ($self->IsAclAssociatedPort($acls{$type}->{$number}, $port))
            {
                $self->PrintAclSimple($acls{$type}->{$number}, $number);
                $found = $TRUE;
            }
        }
    }
    if (!$found)
    {
        print "No matching ACL found.\n";
        return $FM_FAIL;
    }

    return $FM_OK;
}

# removes ACL association on chip
sub tp4000HandleResetAclSimple
{
    my ($self, $type, $number) = @_;
    my $found = $FALSE;
 
    foreach my $aclType (sort keys %acls)
    {
        if ((!defined $type) or ($type eq "") or ($type eq $aclType))
        {
            foreach my $aclNumber (sort numerical keys %{$acls{$aclType}})
            {
                if ((!defined $number) or ($number eq "") or ($number eq $aclNumber))
                {
                    $self->UnsetAclSimple($acls{$aclType}->{$aclNumber}, $aclNumber);
                    $found = $TRUE;
                }
            }
        }
    }
    if (!$found)
    {
        print "No matching ACL found.\n";
        return $FM_FAIL;
    }

    return $FM_OK;
}

# removes ACL from perl
# must already be unassociated
sub tp4000HandleDeleteAclSimple
{
    my ($self, $type, $number) = @_;
    my $found = $FALSE;
 
    foreach my $aclType (sort keys %acls)
    {
        if ((!defined $type) or ($type eq "") or ($type eq $aclType))
        {
            foreach my $aclNumber (sort numerical keys %{$acls{$aclType}})
            {
                if ((!defined $number) or ($number eq "") or ($number eq $aclNumber))
                {
                    $self->DeleteAclSimple($acls{$aclType}->{$aclNumber}, $aclNumber);
                    $found = $TRUE;
                }
            }
        }
    }
    if (!$found)
    {
        print "No matching ACL found.\n";
        return $FM_FAIL;
    }

    return $FM_OK;
}



1;
