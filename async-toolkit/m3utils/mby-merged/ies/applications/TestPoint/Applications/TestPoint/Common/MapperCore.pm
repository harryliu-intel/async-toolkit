# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/MapperCore.pm
# Creation Date:    November 4, 2010
# Description:      TestPoint interface to policers
#
# INTEL CONFIDENTIAL
# Copyright 2010 - 2011 Intel Corporation. All Rights Reserved. 
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

package Applications::TestPoint::Common::MapperCore;
use strict;
use warnings;

use SDKScalars;
use Math::BigInt;

##@cmethod private int tpHandleAddMapper
#
# @desc         
#
# @param[in]    
#
# @param[in]    
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleAddMapper
{
    my $nargs = $#_;
    my $self = shift @_;
    my $api = $self->{'CHIP'};

    my $type = shift @_;

    foreach my $switchNum ($self->tpGetSwitches)
    {
        if ($type eq "source")
        {
            my $port = shift @_;
            my $map = shift @_;
            my $status;

            my $srcMap = new SDK::fm_sourceMapperValue();
            $srcMap->{'sourcePort'} = $port;
            $srcMap->{'mappedSourcePortValue'} = $map;

            my %void = (type => "fm_sourceMapperValue", value => $srcMap);
            $status = $api->fmAddMapperEntry($switchNum, $FM_MAPPER_SOURCE, \%void, $FM_MAPPER_ENTRY_MODE_CACHE);
            if ($status != $FM_OK)
            {
                return $status;
            }
        }
        elsif ($type eq "protocol")
        {
            my $protocol = shift @_;
            my $map = shift @_;
            my $status;

            my $value;
            my $mask;
            if ($protocol =~ m%^([^/]+)/([^/]+)$%)
            {
                ($value, $mask) = ($1, $2);
            }
            else
            {
                $value = $protocol;
                $mask = 0xff;
            }

            my $protocolMap = new SDK::fm_protocolMapperValue();
            $protocolMap->{'protocol'} = $value;
            $protocolMap->{'protocolMask'} = $mask;
            $protocolMap->{'mappedProtocolValue'} = $map;

            my %void = (type => "fm_protocolMapperValue", value => $protocolMap);
            $status = $api->fmAddMapperEntry($switchNum, $FM_MAPPER_PROTOCOL, \%void, $FM_MAPPER_ENTRY_MODE_CACHE);
            if ($status != $FM_OK)
            {
                return $status;
            }
        }
        elsif (($type eq "l4src") ||
               ($type eq "l4dst"))
        {
            my $start = shift @_;
            my $end = shift @_;
            my $protocol = shift @_;
            my $map = shift @_;
            my $status;

            my $l4PortMap = new SDK::fm_l4PortMapperValue();
            $l4PortMap->{'l4PortStart'} = $start;
            $l4PortMap->{'l4PortEnd'} = $end;
            $l4PortMap->{'mappedProtocol'} = $protocol;
            $l4PortMap->{'protocol'} = $protocol;
            $l4PortMap->{'mappedL4PortValue'} = $map;

            my %void = (type => "fm_l4PortMapperValue", value => $l4PortMap);

            my $mapType;
            if ($type eq "l4src")
            {
                $mapType = $FM_MAPPER_L4_SRC;
            }
            else
            {
                $mapType = $FM_MAPPER_L4_DST;
            }
            $status = $api->fmAddMapperEntry($switchNum, $mapType, \%void, $FM_MAPPER_ENTRY_MODE_CACHE);
            if ($status != $FM_OK)
            {
                return $status;
            }
        }
        elsif ($type eq "mac")
        {
            my $direction = shift @_;
            my $mac = shift @_;
            my $map = shift @_;
            my $status;

            my $value;
            my $mask;
            if ($mac =~ m%^([^/]+)/([^/]+)$%)
            {
                ($value, $mask) = ($1, $2);
            }
            else
            {
                $value = $mac;
                $mask = "ff:ff:ff:ff:ff:ff";
            }            
            my $addr;
            foreach $addr ($value, $mask)
            {
                if ($self->validateL2Address($addr) != $FM_OK)
                {
                    print "'$addr' is not a legal MAC address\n";
                    return $FM_FAIL;
                }
            }

            my $MACMap = new SDK::fm_macMapperValue();
            $MACMap->{'mac'} = $value;
            $MACMap->{'macMask'} = $mask;
            $MACMap->{'ignoreLength'} = 0;
            $MACMap->{'mappedMac'} = $map;

            if ($direction eq "source")
            {
                $MACMap->{'validSrcMac'} = $TRUE;
                $MACMap->{'validDstMac'} = $FALSE;
            }
            else
            {
                $MACMap->{'validSrcMac'} = $FALSE;
                $MACMap->{'validDstMac'} = $TRUE;
            }

            my %void = (type => "fm_macMapperValue", value => $MACMap);
            $status = $api->fmAddMapperEntry($switchNum, $FM_MAPPER_MAC, \%void, $FM_MAPPER_ENTRY_MODE_CACHE);
            if ($status != $FM_OK)
            {
                return $status;
            }
        }
        elsif ($type eq "ethtype")
        {
            my $ethType = shift @_;
            my $map = shift @_;
            my $status;

            my $value;
            my $mask;
            if ($ethType =~ m%^([^/]+)/([^/]+)$%)
            {
                ($value, $mask) = ($1, $2);
            }
            else
            {
                $value = $ethType;
                $mask = 0xff;
            }

            my $ethTypeMap = new SDK::fm_ethTypeValue();
            $ethTypeMap->{'ethType'} = $value;
            $ethTypeMap->{'ethTypeMask'} = $mask;
            $ethTypeMap->{'mappedEthType'} = $map;

            my %void = (type => "fm_ethTypeValue", value => $ethTypeMap);
            $status = $api->fmAddMapperEntry($switchNum, $FM_MAPPER_ETH_TYPE, \%void, $FM_MAPPER_ENTRY_MODE_CACHE);
            if ($status != $FM_OK)
            {
                return $status;
            }
        }
        elsif ($type eq "iplength")
        {
            my $start = shift @_;
            my $end = shift @_;
            my $map = shift @_;
            my $status;

            my $ipLengthMap = new SDK::fm_ipLengthMapperValue();
            $ipLengthMap->{'ipLengthStart'} = $start;
            $ipLengthMap->{'ipLengthEnd'} = $end;
            $ipLengthMap->{'mappedIpLength'} = $map;

            my %void = (type => "fm_ipLengthMapperValue", value => $ipLengthMap);
            $status = $api->fmAddMapperEntry($switchNum, $FM_MAPPER_IP_LENGTH, \%void, $FM_MAPPER_ENTRY_MODE_CACHE);
            if ($status != $FM_OK)
            {
                return $status;
            }
        }
        elsif ($type eq "ipaddr")
        {
            my $direction = shift @_;
            my $ip = shift @_;
            my $map = shift @_;

            my $value;
            my $mask;
            if ($ip =~ m%^([^/]+)/([^/]+)$%)
            {
                ($value, $mask) = ($1, $2);
            }
            else
            {
                $value = $ip;
                $mask = undef;
            }            
            my $ipAddr = Types::tp_ipAddr->new();
            my $status = $self->InetPToN($value, $ipAddr);
            if ($status != $FM_OK)
            {
                print "'$value' is not a legal IP address\n";
                return $status;
            }
            $value = SDK::fm_ipAddr->new();
            $status = $self->SetIpAddr($ipAddr, $value);
            if ($status != $FM_OK)
            {
                print "Trouble converting IP address\n";
                return $status;
            }
            if (defined $mask and $mask !~ /^\d+$/)
            {
                print "'$mask' is not a legal number\n";
                return $FM_OK;
            }
            my $maskBits;
            if (not defined $mask)
            {
                $maskBits = 128;
            }
            elsif ($value->{'isIPv6'})
            {
                $maskBits = $mask;
            }
            else
            {
                $maskBits = $mask + 96;
            }

            if ($maskBits > 128)
            {
                print "'$mask' is too big\n";
                return $FM_OK;
            }

            $mask = SDK::fm_ipAddr->new();
            $mask->{'isIPv6'} = $value->{'isIPv6'};
            
            my $bigMask = Math::BigInt->bone();
            $bigMask->blsft(Math::BigInt->new(128 - $maskBits));
            $bigMask->bsub(Math::BigInt->bone());
            $bigMask->bxor(Math::BigInt->new("0xffffffffffffffffffffffffffffffff"));

            my @addrArray = (0) x 4;

            for (my $i = 0 ; $i < 4 ; $i++)
            {
                my $x = Math::BigInt->new("0xffffffff");
                $x->band($bigMask);
                $addrArray[$i] = Applications::TestPoint::Common::RoutingCore::htonl($x->numify());
                $bigMask->brsft(Math::BigInt->new("32"));
            }

            $mask->{'addr'} = \@addrArray;

            my $ipAddrMap = new SDK::fm_ipAddrMapperValue();
            $ipAddrMap->{'ipAddr'} = $value;
            $ipAddrMap->{'ipAddrMask'} = $mask;
            $ipAddrMap->{'ignoreLength'} = 0;
            $ipAddrMap->{'mappedIp'} = $map;

            if ($direction eq "source")
            {
                $ipAddrMap->{'validSrcIp'} = $TRUE;
                $ipAddrMap->{'validDstIp'} = $FALSE;
            }
            else
            {
                $ipAddrMap->{'validSrcIp'} = $FALSE;
                $ipAddrMap->{'validDstIp'} = $TRUE;
            }

            my %void = (type => "fm_ipAddrMapperValue", value => $ipAddrMap);
            $status = $api->fmAddMapperEntry($switchNum, $FM_MAPPER_IP_ADDR, \%void, $FM_MAPPER_ENTRY_MODE_CACHE);
            if ($status != $FM_OK)
            {
                return $status;
            }
        }
        elsif ($type eq "vlan")
        {
            my $vlanId = shift @_;
            my $map = shift @_;
            my $status;

            my $vlanMap = new SDK::fm_vlanMapperValue();
            $vlanMap->{'vlanId'} = $vlanId;
            $vlanMap->{'mappedVlanId'} = $map;

            my %void = (type => "fm_vlanMapperValue", value => $vlanMap);
            $status = $api->fmAddMapperEntry($switchNum, $FM_MAPPER_VLAN, \%void, $FM_MAPPER_ENTRY_MODE_CACHE);
            if ($status != $FM_OK)
            {
                return $status;
            }
        }
        else
        {
            print "Unrecognized mapper type!\n";
            return $FM_FAIL;
        }
    }

    my $status = $FM_OK;

    return $status;

}   # end tpHandleAddMapper


sub tpHandleDelMapper
{
    my $nargs = $#_;
    my $self = shift @_;
    my $api = $self->{'CHIP'};

    my $type = shift @_;

    foreach my $switchNum ($self->tpGetSwitches)
    {
        if ($type eq "source")
        {
            my $port = shift @_;
            my $status;

            my $srcMap = new SDK::fm_sourceMapperValue();
            $srcMap->{'sourcePort'} = $port;

            my %void = (type => "fm_sourceMapperValue", value => $srcMap);
            $status = $api->fmDeleteMapperEntry($switchNum, $FM_MAPPER_SOURCE, \%void, $FM_MAPPER_ENTRY_MODE_CACHE);
            if ($status != $FM_OK)
            {
                return $status;
            }
        }
        elsif ($type eq "protocol")
        {
            my $protocol = shift @_;
            my $map = shift @_;
            my $status;

            my $value;
            my $mask;
            if ($protocol =~ m%^([^/]+)/([^/]+)$%)
            {
                ($value, $mask) = ($1, $2);
            }
            else
            {
                $value = $protocol;
                $mask = 0xff;
            }

            my $protocolMap = new SDK::fm_protocolMapperValue();
            $protocolMap->{'protocol'} = $value;
            $protocolMap->{'protocolMask'} = $mask;
            $protocolMap->{'mappedProtocolValue'} = $map;

            my %void = (type => "fm_protocolMapperValue", value => $protocolMap);
            $status = $api->fmDeleteMapperEntry($switchNum, $FM_MAPPER_PROTOCOL, \%void, $FM_MAPPER_ENTRY_MODE_CACHE);
            if ($status != $FM_OK)
            {
                return $status;
            }
        }
        elsif (($type eq "l4src") ||
               ($type eq "l4dst"))
        {
            my $start = shift @_;
            my $end = shift @_;
            my $protocol = shift @_;
            my $map = shift @_;
            my $status;

            my $l4PortMap = new SDK::fm_l4PortMapperValue();
            $l4PortMap->{'l4PortStart'} = $start;
            $l4PortMap->{'l4PortEnd'} = $end;
            $l4PortMap->{'mappedProtocol'} = $protocol;
            $l4PortMap->{'protocol'} = $protocol;
            $l4PortMap->{'mappedL4PortValue'} = $map;

            my %void = (type => "fm_l4PortMapperValue", value => $l4PortMap);

            my $mapType;
            if ($type eq "l4src")
            {
                $mapType = $FM_MAPPER_L4_SRC;
            }
            else
            {
                $mapType = $FM_MAPPER_L4_DST;
            }
            $status = $api->fmDeleteMapperEntry($switchNum, $mapType, \%void, $FM_MAPPER_ENTRY_MODE_CACHE);
            if ($status != $FM_OK)
            {
                return $status;
            }
        }
        elsif ($type eq "mac")
        {
            my $direction = shift @_;
            my $mac = shift @_;
            my $map = shift @_;
            my $status;

            my $value;
            my $mask;
            if ($mac =~ m%^([^/]+)/([^/]+)$%)
            {
                ($value, $mask) = ($1, $2);
            }
            else
            {
                $value = $mac;
                $mask = "ff:ff:ff:ff:ff:ff";
            }            
            my $addr;
            foreach $addr ($value, $mask)
            {
                if ($self->validateL2Address($addr) != $FM_OK)
                {
                    print "'$addr' is not a legal MAC address\n";
                    return $FM_FAIL;
                }
            }

            my $MACMap = new SDK::fm_macMapperValue();
            $MACMap->{'mac'} = $value;
            $MACMap->{'macMask'} = $mask;
            $MACMap->{'ignoreLength'} = 0;
            $MACMap->{'mappedMac'} = $map;

            if ($direction eq "source")
            {
                $MACMap->{'validSrcMac'} = $TRUE;
                $MACMap->{'validDstMac'} = $FALSE;
            }
            else
            {
                $MACMap->{'validSrcMac'} = $FALSE;
                $MACMap->{'validDstMac'} = $TRUE;
            }

            my %void = (type => "fm_macMapperValue", value => $MACMap);
            $status = $api->fmDeleteMapperEntry($switchNum, $FM_MAPPER_MAC, \%void, $FM_MAPPER_ENTRY_MODE_CACHE);
            if ($status != $FM_OK)
            {
                return $status;
            }
        }
        elsif ($type eq "ethtype")
        {
            my $ethType = shift @_;
            my $map = shift @_;
            my $status;

            my $value;
            my $mask;
            if ($ethType =~ m%^([^/]+)/([^/]+)$%)
            {
                ($value, $mask) = ($1, $2);
            }
            else
            {
                $value = $ethType;
                $mask = 0xff;
            }

            my $ethTypeMap = new SDK::fm_ethTypeValue();
            $ethTypeMap->{'ethType'} = $value;
            $ethTypeMap->{'ethTypeMask'} = $mask;
            $ethTypeMap->{'mappedEthType'} = $map;

            my %void = (type => "fm_ethTypeValue", value => $ethTypeMap);
            $status = $api->fmDeleteMapperEntry($switchNum, $FM_MAPPER_ETH_TYPE, \%void, $FM_MAPPER_ENTRY_MODE_CACHE);
            if ($status != $FM_OK)
            {
                return $status;
            }
        }
        elsif ($type eq "iplength")
        {
            my $start = shift @_;
            my $end = shift @_;
            my $map = shift @_;
            my $status;

            my $ipLengthMap = new SDK::fm_ipLengthMapperValue();
            $ipLengthMap->{'ipLengthStart'} = $start;
            $ipLengthMap->{'ipLengthEnd'} = $end;
            $ipLengthMap->{'mappedIpLength'} = $map;

            my %void = (type => "fm_ipLengthMapperValue", value => $ipLengthMap);
            $status = $api->fmDeleteMapperEntry($switchNum, $FM_MAPPER_IP_LENGTH, \%void, $FM_MAPPER_ENTRY_MODE_CACHE);
            if ($status != $FM_OK)
            {
                return $status;
            }
        }
        elsif ($type eq "ipaddr")
        {
            my $direction = shift @_;
            my $ip = shift @_;
            my $map = shift @_;

            my $value;
            my $mask;
            if ($ip =~ m%^([^/]+)/([^/]+)$%)
            {
                ($value, $mask) = ($1, $2);
            }
            else
            {
                $value = $ip;
                $mask = undef;
            }            
            my $ipAddr = Types::tp_ipAddr->new();
            my $status = $self->InetPToN($value, $ipAddr);
            if ($status != $FM_OK)
            {
                print "'$value' is not a legal IP address\n";
                return $status;
            }
            $value = SDK::fm_ipAddr->new();
            $status = $self->SetIpAddr($ipAddr, $value);
            if ($status != $FM_OK)
            {
                print "Trouble converting IP address\n";
                return $status;
            }
            if (defined $mask and $mask !~ /^\d+$/)
            {
                print "'$mask' is not a legal number\n";
                return $FM_OK;
            }
            my $maskBits;
            if (not defined $mask)
            {
                $maskBits = 128;
            }
            elsif ($value->{'isIPv6'})
            {
                $maskBits = $mask;
            }
            else
            {
                $maskBits = $mask + 96;
            }

            if ($maskBits > 128)
            {
                print "'$mask' is too big\n";
                return $FM_OK;
            }

            $mask = SDK::fm_ipAddr->new();
            $mask->{'isIPv6'} = $value->{'isIPv6'};
            
            my $bigMask = Math::BigInt->bone();
            $bigMask->blsft(Math::BigInt->new(128 - $maskBits));
            $bigMask->bsub(Math::BigInt->bone());
            $bigMask->bxor(Math::BigInt->new("0xffffffffffffffffffffffffffffffff"));

            my @addrArray = (0) x 4;

            for (my $i = 0 ; $i < 4 ; $i++)
            {
                my $x = Math::BigInt->new("0xffffffff");
                $x->band($bigMask);
                $addrArray[$i] = Applications::TestPoint::Common::RoutingCore::htonl($x->numify());
                $bigMask->brsft(Math::BigInt->new("32"));
            }

            $mask->{'addr'} = \@addrArray;

            my $ipAddrMap = new SDK::fm_ipAddrMapperValue();
            $ipAddrMap->{'ipAddr'} = $value;
            $ipAddrMap->{'ipAddrMask'} = $mask;
            $ipAddrMap->{'ignoreLength'} = 0;
            $ipAddrMap->{'mappedIp'} = $map;

            if ($direction eq "source")
            {
                $ipAddrMap->{'validSrcIp'} = $TRUE;
                $ipAddrMap->{'validDstIp'} = $FALSE;
            }
            else
            {
                $ipAddrMap->{'validSrcIp'} = $FALSE;
                $ipAddrMap->{'validDstIp'} = $TRUE;
            }

            my %void = (type => "fm_ipAddrMapperValue", value => $ipAddrMap);
            $status = $api->fmDeleteMapperEntry($switchNum, $FM_MAPPER_IP_ADDR, \%void, $FM_MAPPER_ENTRY_MODE_CACHE);
            if ($status != $FM_OK)
            {
                return $status;
            }
        }
        elsif ($type eq "vlan")
        {
            my $vlanId = shift @_;
            my $status;

            my $vlanMap = new SDK::fm_vlanMapperValue();
            $vlanMap->{'vlanId'} = $vlanId;

            my %void = (type => "fm_vlanMapperValue", value => $vlanMap);
            $status = $api->fmDeleteMapperEntry($switchNum, $FM_MAPPER_VLAN, \%void, $FM_MAPPER_ENTRY_MODE_CACHE);
            if ($status != $FM_OK)
            {
                return $status;
            }
        }
        else
        {
            print "Unrecognized mapper type!\n";
            return $FM_FAIL;
        }
    }

    my $status = $FM_OK;

    return $status;

}   # end tpHandleDelMapper


sub tpHandleShowMapper
{
    my $nargs = $#_;
    my $self = shift @_;
    my $api = $self->{'CHIP'};

    my $type = shift @_;

    my $countLeading = sub {
        my $x = $_[0];
        my @bits = split(//, sprintf('%032b', Applications::TestPoint::Common::RoutingCore::ntohl($x)));
        my $result = 0;
        while ($#bits >= 0 and shift(@bits) eq '1')
        {
            $result++;
        }
        return $result;

    };  # end $countLeading

    my $countLeadingIPv6 = sub {
        my $result = 0;
        for (my $i = 3 ; $i >= 0 ; $i--)
        {
            return $result + $countLeading->($_[$i]) if ($_[$i] != 0xffffffff);
            $result += 32;
        }
        return $result;

    };  # end $countLeadingIPv6

    foreach my $switchNum ($self->tpGetSwitches)
    {
        if ($type eq "source")
        {
            my $status;

            my $nEntries = 0;
            my $maxEntries = 76;
            my $entriesW = [map {SDK::fm_sourceMapperValue->new()} (1 .. 76)];

            my %void = (type => "fm_sourceMapperValue[]", value => $entriesW);
            $status = $api->fmGetMapper($switchNum, $FM_MAPPER_SOURCE, \$nEntries, \%void, $maxEntries);
            if ($status != $FM_OK)
            {
                return $status;
            }
            for (my $entry = 0 ; $entry < $nEntries ; $entry++)
            {
                my $srcMap = $entriesW->[$entry];
                my $port = $srcMap->{'sourcePort'};
                my $map = $srcMap->{'mappedSourcePortValue'};

                print("Logical Port:$port ==> Map:$map\n");
            }
        }
        elsif ($type eq "protocol")
        {
            my $status;

            my $nEntries = 0;
            my $maxEntries = 16;
            my $entriesW = [map {SDK::fm_protocolMapperValue->new()} (1 .. 16)];

            my %void = (type => "fm_protocolMapperValue[]", value => $entriesW);
            $status = $api->fmGetMapper($switchNum, $FM_MAPPER_PROTOCOL, \$nEntries, \%void, $maxEntries);
            if ($status != $FM_OK)
            {
                return $status;
            }
            for (my $entry = 0 ; $entry < $nEntries ; $entry++)
            {
                my $protMap = $entriesW->[$entry];
                my $protocol = $protMap->{'protocol'};
                my $mask = $protMap->{'protocolMask'};
                my $map = $protMap->{'mappedProtocolValue'};

                print("Protocol:$protocol/$mask ==> Map:$map\n");
            }
        }
        elsif (($type eq "l4src") ||
               ($type eq "l4dst"))
        {
            my $status;

            my $nEntries = 0;
            my $maxEntries = 32;
            my $entriesW = [map {SDK::fm_l4PortMapperValue->new()} (1 .. 32)];

            my %void = (type => "fm_l4PortMapperValue[]", value => $entriesW);

            my $mapType;
            if ($type eq "l4src")
            {
                $mapType = $FM_MAPPER_L4_SRC;
            }
            else
            {
                $mapType = $FM_MAPPER_L4_DST;
            }

            $status = $api->fmGetMapper($switchNum, $mapType, \$nEntries, \%void, $maxEntries);
            if ($status != $FM_OK)
            {
                return $status;
            }
            for (my $entry = 0 ; $entry < $nEntries ; $entry++)
            {
                my $l4PortMap = $entriesW->[$entry];
                my $start = $l4PortMap->{'l4PortStart'};
                my $end = $l4PortMap->{'l4PortEnd'};
                my $protocol = $l4PortMap->{'protocol'};
                my $map = $l4PortMap->{'mappedL4PortValue'};

                print("L4Port:$start-$end With Protocol:$protocol ==> Map:$map\n");
            }
        }
        elsif ($type eq "mac")
        {
            my $status;

            my $nEntries = 0;
            my $maxEntries = 32;
            my $entriesW = [map {SDK::fm_macMapperValue->new()} (1 .. 32)];

            my %void = (type => "fm_macMapperValue[]", value => $entriesW);
            $status = $api->fmGetMapper($switchNum, $FM_MAPPER_MAC, \$nEntries, \%void, $maxEntries);
            if ($status != $FM_OK)
            {
                return $status;
            }
            for (my $entry = 0 ; $entry < $nEntries ; $entry++)
            {
                my $MACMap = $entriesW->[$entry];
                my $mac = $MACMap->{'mac'};
                my $mask = $MACMap->{'macMask'};
                my $map = $MACMap->{'mappedMac'};
                my $validSrc = $MACMap->{'validSrcMac'};
                my $validDst = $MACMap->{'validDstMac'};

                if ($validSrc == $TRUE)
                {
                    print("Source MAC:$mac/$mask ==> Map:$map\n");
                }
                else
                {
                    print("Destination MAC:$mac/$mask ==> Map:$map\n");
                }
            }
        }
        elsif ($type eq "ethtype")
        {
            my $status;

            my $nEntries = 0;
            my $maxEntries = 16;
            my $entriesW = [map {SDK::fm_ethTypeValue->new()} (1 .. 16)];

            my %void = (type => "fm_ethTypeValue[]", value => $entriesW);
            $status = $api->fmGetMapper($switchNum, $FM_MAPPER_ETH_TYPE, \$nEntries, \%void, $maxEntries);
            if ($status != $FM_OK)
            {
                return $status;
            }
            for (my $entry = 0 ; $entry < $nEntries ; $entry++)
            {
                my $ethTypeMap = $entriesW->[$entry];
                my $ethType = $ethTypeMap->{'ethType'};
                my $mask = $ethTypeMap->{'ethTypeMask'};
                my $map = $ethTypeMap->{'mappedEthType'};

                print("EtherType:$ethType/$mask ==> Map:$map\n");
            }
        }
        elsif ($type eq "iplength")
        {
            my $status;

            my $nEntries = 0;
            my $maxEntries = 8;
            my $entriesW = [map {SDK::fm_ipLengthMapperValue->new()} (1 .. 8)];

            my %void = (type => "fm_ipLengthMapperValue[]", value => $entriesW);
            $status = $api->fmGetMapper($switchNum, $FM_MAPPER_IP_LENGTH, \$nEntries, \%void, $maxEntries);
            if ($status != $FM_OK)
            {
                return $status;
            }
            for (my $entry = 0 ; $entry < $nEntries ; $entry++)
            {
                my $ipLengthMap = $entriesW->[$entry];
                my $start = $ipLengthMap->{'ipLengthStart'};
                my $end = $ipLengthMap->{'ipLengthEnd'};
                my $map = $ipLengthMap->{'mappedIpLength'};

                print("IP Length:$start-$end ==> Map:$map\n");
            }
        }
        elsif ($type eq "ipaddr")
        {
            my $status;

            my $nEntries = 0;
            my $maxEntries = 32;
            my $entriesW = [map {SDK::fm_ipAddrMapperValue->new()} (1 .. 32)];

            my %void = (type => "fm_ipAddrMapperValue[]", value => $entriesW);
            $status = $api->fmGetMapper($switchNum, $FM_MAPPER_IP_ADDR, \$nEntries, \%void, $maxEntries);
            if ($status != $FM_OK)
            {
                return $status;
            }
            for (my $entry = 0 ; $entry < $nEntries ; $entry++)
            {
                my $ipAddrMap = $entriesW->[$entry];
                my $val = $ipAddrMap->{'ipAddr'};
                my $mask = $ipAddrMap->{'ipAddrMask'};
                my $map = $ipAddrMap->{'mappedIp'};
                my $validSrc = $ipAddrMap->{'validSrcIp'};
                my $validDst = $ipAddrMap->{'validDstIp'};

                my $tpip = Types::tp_ipAddr->new();
                my $status = $self->GetIpAddr($val, $tpip);
                return $status unless ($status == $FM_OK);
                my $ipStr;
                $status = $self->InetNToP($tpip, \$ipStr);
                return $status unless ($status == $FM_OK);
                my $txt = $ipStr;
                # InetNToP prints IPv6 addresses in decimal (bug 11103)
                # and we would prefer the more traditional hexadecimal,
                # so convert IPv6 addresses on our own.
                $txt = join(':',
                            reverse map { sprintf('%04x:%04x',
                                                  $_ >> 16,
                                                  $_ & 0xffff) }
                            @{$val->{'addr'}}) if ($val->{'isIPv6'});
                my $prefix;
                my $maxPrefix;
                if ($mask->{'isIPv6'})
                {
                    $maxPrefix = 128;
                    $prefix = $countLeadingIPv6->(@{$mask->{'addr'}});
                }
                else
                {
                    $maxPrefix = 32;
                    $prefix = $countLeading->($mask->{'addr'}->[0]);
                }
                $txt .= "/$prefix" unless ($prefix == $maxPrefix);

                if ($validSrc == $TRUE)
                {
                    print("Source IP:$txt ==> Map:$map\n");
                }
                else
                {
                    print("Destination IP:$txt ==> Map:$map\n");
                }
            }
        }
        elsif ($type eq "vlan")
        {
            my $status;

            my $nEntries = 0;
            my $maxEntries = 4096;
            my $entriesW = [map {SDK::fm_vlanMapperValue->new()} (1 .. 4096)];

            my %void = (type => "fm_vlanMapperValue[]", value => $entriesW);
            $status = $api->fmGetMapper($switchNum, $FM_MAPPER_VLAN, \$nEntries, \%void, $maxEntries);
            if ($status != $FM_OK)
            {
                return $status;
            }
            for (my $entry = 0 ; $entry < $nEntries ; $entry++)
            {
                my $vlanMap = $entriesW->[$entry];
                my $vlanId = $vlanMap->{'vlanId'};
                my $map = $vlanMap->{'mappedVlanId'};

                print("Vlan:$vlanId ==> Map:$map\n");
            }
        }
        else
        {
            print "Unrecognized mapper type!\n";
            return $FM_FAIL;
        }
    }

    my $status = $FM_OK;

    return $status;

}   # end tpHandleShowMapper

1;
