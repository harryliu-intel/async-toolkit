# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/RoutingCore.pm
# Creation Date:    09/14/07
# Description:      FocalPoint routing support code
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

##@class Applications::TestPoint::Common::RoutingCore
package Applications::TestPoint::Common::RoutingCore;
use strict;
use warnings;

use Config;
use List::Util qw(max sum);
use Scalar::Util qw(blessed);
use Math::BigInt;
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Base::Const;
use Types::tp_ipAddr;
use Types::tp_vpPair;
use Scripts::Utilities;

###############################################################################
#
#                           CONSTANTS
#
###############################################################################

use constant TP_ROUTE_ALL   => -1;

###############################################################################
#
#                           LOCAL VARIABLES
#
###############################################################################

##@var private SDK::fm_arpEntry** __arpEntries
#
# @brief        Statically allocated array large enough to hold the maximum
#               number of possible ARP entries (FM_MAX_ARPS)
our $__arpEntries = [map {SDK::fm_arpEntry->new()} (1 .. $FM_MAX_ARPS)];

##@var private SDK::fm_routeEntry** __routeEntries
#
# @brief        Statically allocated array large enough to hold the maximum
#               number of possible route entries (FM_MAX_ARP)
our $__routeEntries = [map {SDK::fm_routeEntry->new()} (1 .. $FM_MAX_ARPS)];

##@var private <char*,int> __routerAttrs
#
# @brief        Perl HASH containing all recognized router attributes
our %__routerAttrs =
(
    'mac'           => $FM_ROUTER_PHYSICAL_MAC_ADDRESS,
    'ttl-disposal'  => $FM_ROUTER_TRAP_TTL1,
    'trap-ip-opts'  => $FM_ROUTER_TRAP_IP_OPTIONS,
    'virtual-mac'   => $FM_ROUTER_VIRTUAL_MAC_ADDRESS,
);

##@var private <char*,int> __routerTTL1Attrs
our %__routerTTL1Attrs =
(
    'drop-all'      => $FM_ROUTER_TTL1_DROP_ALL,
    'trap-icmp'     => $FM_ROUTER_TTL1_TRAP_ICMP,
    'trap-all'      => $FM_ROUTER_TTL1_TRAP_ALL,
);

###############################################################################
#
#                           LOCAL FUNCTIONS
#
###############################################################################

sub htonl
{
    my ($value) = @_;

    if ($Config{'byteorder'} =~ m/1234(|5678)/o)
    {
        # This is a little-endian host.
        return unpack('N', pack('L', $value));
    }
    return $value;
}

sub ntohl
{
    my ($value) = @_;

    if ($Config{'byteorder'} =~ m/1234(|5678)/o)
    {
        # This is a little-endian host.
        return unpack('L', pack('N', $value));
    }
    return $value;
}

##@cmethod private int GetIpAddr(SDK::fm_ipAddr   &source,
#                                Types::tp_ipAddr &destination)
#
# @brief        Converts a SDK::fm_ipAddr network address structure into a
#               Types::tp_ipAddr network address data structure
#
# @param[out]   source Points to a SDK::fm_ipAddr Perl data structure
#
# @param[in]    destination Points to a Types::tp_ipAddr Perl data structure
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub GetIpAddr
{
    my $self                         = shift(@_);
    my $source                       = shift(@_);
    my Types::tp_ipAddr $destination = shift(@_);

    if (!blessed($source)
        || !$source->isa('SDK::fm_ipAddr')
        || !blessed($destination)
        || !$destination->isa('Types::tp_ipAddr'))
    {
        return $FM_FAIL;
    }
    if (arrayCopy($destination->{'address'}, $source->{'addr'}) != FM_SUCCESS)
    {
        return $FM_FAIL;
    }
    $destination->{'af'} = $source->{'isIPv6'} ? TP_AF_INET6 : TP_AF_INET;
    return $FM_OK;
}

##@cmethod private int InetNToP(Types::tp_ipAddr &ipAddr, char **cidr)
#
# @brief        Converts a network address structure into a CIDR style IP
#               address character string
#
# @param[in]    ipAddr Points to a Types::tp_ipAddr Perl data structure to be
#               converted
#
# @param[out]   cidr Points to user-allocated storage in which the CIDR style
#               IP address character string is to be stored
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub InetNToP
{
    my $self                    = shift(@_);
    my Types::tp_ipAddr $ipAddr = shift(@_);
    my $cidr                    = shift(@_);

    if (!blessed($ipAddr) || !$ipAddr->isa('Types::tp_ipAddr'))
    {
        return $FM_FAIL;
    }
    if ($ipAddr->{'af'} == TP_AF_INET6)
    {
        return $self->Inet6NToP($ipAddr, $cidr);
    }
    else
    {
        return $self->Inet4NToP($ipAddr, $cidr);
    }
}

##@cmethod private int Inet4NToP(Types::tp_ipAddr &ipAddr, char **cidr)
#
# @brief        Converts a network address structure into a CIDR style IP
#               version 4 address character string
#
# @param[in]    ipAddr Points to a Types::tp_ipAddr Perl data structure to be
#               converted
#
# @param[out]   cidr Points to user-allocated storage in which the CIDR style
#               IP version 4 address character string is to be stored
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub Inet4NToP
{
    my $self                    = shift(@_);
    my Types::tp_ipAddr $ipAddr = shift(@_);
    my $cidr                    = shift(@_);

    # Convert the IP address to host byte order.
    my $value = ntohl($ipAddr->{'address'}->[0]);
    ${$cidr} = join('.', map {($value >> (8 * $_)) & 0xFF} reverse(0 .. 3));
    if (defined($ipAddr->{'prefix'}))
    {
        ${$cidr} .= sprintf("/%d", $ipAddr->{'prefix'});
    }
    return $FM_OK;
}

##@cmethod private int Inet6NToP(Types::tp_ipAddr &ipAddr, char **cidr)
#
# @brief        Converts a network address structure into a CIDR style IP
#               version 6 address character string
#
# @param[in]    ipAddr Points to a Types::tp_ipAddr Perl data structure to be
#               converted
#
# @param[out]   cidr Points to user-allocated storage in which the CIDR style
#               IP version 6 address character string is to be stored
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub Inet6NToP
{
    my $self                    = shift(@_);
    my Types::tp_ipAddr $ipAddr = shift(@_);
    my $cidr                    = shift(@_);

    # Convert the IP address to host byte order.
    my @value = map {ntohl($ipAddr->{'address'}->[$_])} 0 .. 3;
    ${$cidr} = join(':', map {
        my $index = int($_ / 2);
        my $shift = 16 * ($_ % 2);
        sprintf("%x", ($value[$index] >> $shift) & 0xFFFF)
    } reverse(0 .. 7));
    if (defined($ipAddr->{'prefix'}))
    {
        ${$cidr} .= sprintf("/%d", $ipAddr->{'prefix'});
    }
    return $FM_OK;
}

##@cmethod private int InetPToN(char *cidr, Types::tp_ipAddr &ipAddr)
#
# @brief        Converts a CIDR style IP address character string into a
#               network address structure
#
# @param[in]    cidr The CIDR style IP address to be converted
#
# @param[out]   ipAddr Points to a Types::tp_ipAddr Perl data structure in
#               which the network representation of the IP address is to be
#               stored
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub InetPToN
{
    my ($self, $cidr, $ipAddr) = @_;

    if (!blessed($ipAddr) || !$ipAddr->isa('Types::tp_ipAddr'))
    {
        return $FM_FAIL;
    }
    if ($cidr =~ m/:/)
    {
        return $self->Inet6PToN($cidr, $ipAddr);
    }
    else
    {
        return $self->Inet4PToN($cidr, $ipAddr);
    }
}

##@cmethod private int Inet4PToN(char *cidr, Types::tp_ipAddr &ipAddr)
#
# @brief        Converts a CIDR style IP version 4 address to network format
#
# @param[in]    cidr The CIDR style IP version 4 address to be converted
#
# @param[out]   ipAddr Points to a Types::tp_ipAddr Perl data structure in
#               which the network representation of the IP version 4 address is
#               to be stored
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub Inet4PToN
{
    my $self                    = shift(@_);
    my $cidr                    = shift(@_);
    my Types::tp_ipAddr $ipAddr = shift(@_);

    # Mark the CIDR style address as a CIDR style IP version 4 address.
    $ipAddr->{'af'} = TP_AF_INET;

    $ipAddr->{'address'} = $cidr;
    # Remove leading and trailing whitespace.
    $ipAddr->{'address'} =~ s/^\s+(.*)\s+$//;
    if ($ipAddr->{'address'} =~ m/\//)
    {
        $ipAddr->{'address'} =~ s/^([^\/]*)\/.*$/$1/;
    }
    # Ensure that the last part of the CIDR style IP address is a number.
    if ($ipAddr->{'address'} =~ m/[^0-9]$/)
    {
        return $FM_FAIL;
    }
    my @address = split(/\./, $ipAddr->{'address'});
    # Ensure that all parts of the CIDR style IP address are numbers.
    #
    # Note: The loop variable in a foreach construct is aliased to each element
    # in the array being looped over. As a consequence, modifying the loop
    # variable will modify the array element the loop variable is refering to.
    foreach my $part (@address)
    {
        $part = $self->str2decnum($part);
        if (!defined($part))
        {
            return $FM_FAIL;
        }
    }
    # At least 1 byte and at most 4 bytes of data must be specified.
    if (scalar(@address) < 1 || scalar(@address) > 4
        || sum(map {int($_ >= 0x0 && $_ <= 0xFF)} @address) != scalar(@address))
    {
        return $FM_FAIL;
    }
    $ipAddr->{'address'} = [(0) x 4];
    my $value = sum(map {$address[$_] << (8 * (3 - $_))} 0 .. $#address);
    # Convert the IP address to network byte order.
    $ipAddr->{'address'}->[0] = htonl($value);

    $ipAddr->{'prefix'} = 32;
    if ($cidr =~ m/\//)
    {
        $ipAddr->{'prefix'} = $cidr;
        $ipAddr->{'prefix'} =~ s/^[^\/]*\/(.*)$/$1/;
        $ipAddr->{'prefix'} = $self->str2decnum($ipAddr->{'prefix'});
        if (!defined($ipAddr->{'prefix'})
            || $ipAddr->{'prefix'} < 0 || $ipAddr->{'prefix'} > 32)
        {
            return $FM_FAIL;
        }
    }

    return $FM_OK;
}

##@cmethod private int Inet6PToN(char *cidr, Types::tp_ipAddr &ipAddr)
#
# @brief        Converts a CIDR style IP version 6 address to network format
#
# @param[in]    cidr The CIDR style IP version 6 address to be converted
#
# @param[out]   ipAddr Points to a Types::tp_ipAddr Perl data structure in
#               which the network representation of the IP version 6 address is
#               to be stored
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub Inet6PToN
{
    my $self                    = shift(@_);
    my $cidr                    = shift(@_);
    my Types::tp_ipAddr $ipAddr = shift(@_);

    # Mark the CIDR style address as a CIDR style IP version 6 address.
    $ipAddr->{'af'} = TP_AF_INET6;

    $ipAddr->{'address'} = $cidr;
    # Remove leading and trailing whitespace and, if provided, the prefix
    # length.
    $ipAddr->{'address'} =~ s/^\s*([^\/]*).*\s*$/$1/;
    if ($ipAddr->{'address'} =~ m/::/)
    {
        my @address = split(/::/, $ipAddr->{'address'});
        # Compression of zeros can only occur once.
        if (scalar(@address) > 2)
        {
            return $FM_FAIL;
        }
        # Ensure that the @address ARRAY contains 2 non-empty entries.
        my $n = scalar(@address);
        SWITCH: for ($n)
        {
            /0/ && do
            {
                # Handle the unspecified address "::".
                @address = (0) x 2;
                last SWITCH;
            };

            /1/ && do
            {
                # Handle addresses of the form "...:x::".
                if ($ipAddr->{'address'} =~ m/::$/)
                {
                    push(@address, 0);
                }
                last SWITCH;
            };

            /2/ && do
            {
                # Handle address of the form "::x:...".
                if ($ipAddr->{'address'} =~ m/^::/)
                {
                    # Replace the first (empty) element by 0.
                    splice(@address, 0, 1, 0);
                }
                last SWITCH;
            };
        }
        # Detect all ":" characters and use this information to create a
        # decompressed IP version 6 address.
        my @separators = map {sum(map {int($_ eq ":")} split(//, $_))} @address;
        # Take into account mixed IP version 4 and IP version 6 addresses.
        if ($ipAddr->{'address'} =~ m/\./)
        {
            $separators[$#separators]++;
        }
        # At least one zero will be inserted into the IP version 6 address to
        # allow detection of improper use of the compression operator.
        # my $zeros = max(1, 8 - sum(@separators) - 2);
        my $zeros = max(1, 8 - sum(@separators) - 2);
        $ipAddr->{'address'} =
                    sprintf("%s:%s:%s",
                            $address[0], join(":", (0) x $zeros), $address[1]);
    }

    my @address = split(/:/, $ipAddr->{'address'});
    if ($ipAddr->{'address'} =~ m/\./)
    {
        # The address is a mixed IP version 4 and IP version 6 address. The
        # first 6 16-bit parts of the address are in IP version 6 notation,
        # while the last 2 16-bit parts are in IP version 4 notation, i.e.
        # a.b.c.d.

        # Ensure that the last part of the CIDR style IP address is a number.
        if ($ipAddr->{'address'} =~ m/[^0-9]$/)
        {
            return $FM_FAIL;
        }
        # Ensure that the first 6 16-bit parts are not in IP version 4
        # notation. Furthermore, ensure that these parts are all hexadecimal
        # numbers.
        for (my $i = 0; $i < $#address; $i++)
        {
            if ($address[$i] =~ m/\./
                || !defined($address[$i] = $self->str2hexnum($address[$i])))
            {
                return $FM_FAIL;
            }
        }
        # Ensure that the last 2 16-bit parts are in IP version 4 notation.
        # Furthermore, ensure that these parts are all 8-bit decimal numbers.
        my @inet4 = split(/\./, $address[$#address]);
        if (scalar(@inet4) != 4)
        {
            return $FM_FAIL;
        }
        foreach my $part (@inet4)
        {
            $part = $self->str2decnum($part);
            if (!defined($part) || ($part < 0x0 || $part > 0xFF))
            {
                return $FM_FAIL;
            }
        }
        pop(@address);
        push(@address, $inet4[0] << 8 | $inet4[1]);
        push(@address, $inet4[2] << 8 | $inet4[3]);
    }
    else
    {
        # Ensure that all parts of the CIDR style IP address are hexadecimal
        # numbers.
        foreach my $part (@address)
        {
            $part = $self->str2hexnum($part);
            if (!defined($part))
            {
                return $FM_FAIL;
            }
        }
    }
    # Ensure that 8 16-bit parts have been specified.
    if (scalar(@address) != 8
        || sum(map {int($_ >= 0x0 && $_ <= 0xFFFF)} @address) != 8)
    {
        return $FM_FAIL;
    }
    my @value = (0) x 4;
    map {
        my $index = 3 - int($_ / 2);
        my $shift = $_ % 2 == 0 ? 16 : 0;
        $value[$index] |= $address[$_] << $shift;
    } (0 .. 7);
    # Convert the IP address to network byte order.
    $ipAddr->{'address'} = [map {htonl($value[$_])} 0 .. 3];

    $ipAddr->{'prefix'} = 128;
    if ($cidr =~ m/\//)
    {
        $ipAddr->{'prefix'} = $cidr;
        $ipAddr->{'prefix'} =~ s/^[^\/]*\/(.*)$/$1/;
        $ipAddr->{'prefix'} = $self->str2decnum($ipAddr->{'prefix'});
        if (!defined($ipAddr->{'prefix'})
            || $ipAddr->{'prefix'} < 0 || $ipAddr->{'prefix'} > 128)
        {
            return $FM_FAIL;
        }
    }

    return $FM_OK;
}

##@cmethod private bool IsIPMulticast(Types::tp_ipAddr &ipAddr)
#
# @brief        Determines whether an IP version 4 or IP version 6 address is
#               an IP multicast address
#
# @param[out]   ipAddr Points to a Types::tp_ipAddr Perl data structure whose
#               IP class is to be determined
#
# @return       TRUE if the IP address is an IP multicast address
# @return       FALSE otherwise
sub IsIPMulticast
{
    my $self                    = shift(@_);
    my Types::tp_ipAddr $ipAddr = shift(@_);

    if ($self->IsIPv6($ipAddr))
    {
        if (($ipAddr->{'address'}->[3] & htonl(0xFF000000)) == htonl(0xFF000000))
        {
            return $TRUE;
        }
    }
    else
    {
        if (($ipAddr->{'address'}->[0] & htonl(0xF0000000)) == htonl(0xE0000000))
        {
            return $TRUE;
        }
    }
    return $FALSE;
}

##@cmethod private bool IsIPv6(Types::tp_ipAddr &ipAddr)
#
# @brief        Determines whether an IP address is an IP version 6 address or
#               an IP version 4 address
#
# @param[in]    ipAddr Points to a Types::tp_ipAddr Perl data structure whose
#               IP version is to be determined
#
# @return       TRUE if the IP address is an IP version 6 address
# @return       FALSE otherwise
sub IsIPv6
{
    my ($self, $ipAddr) = @_;

    return $ipAddr->{'af'} == TP_AF_INET6 ? $TRUE : $FALSE;
}


##
# @brief        Parses the standard arp-entry command parameters.
# 
# @param[in]    args is the argument list (@_) passed to the function.
# 
# @return       ($result, $arpEntry)
# 
sub ParseArpEntryArgs
{
    my ($self, @args) = @_;

#   printf("ParseArpEntryArgs: [ %s ]\n", join(' ', @args));

    my $ipAddress = shift(@args);
    if (defined($ipAddress) && $ipAddress eq "all")
    {
        return ($FM_OK, "all");
    }

    if (!defined($ipAddress))
    {
        print("Must specify <ip address>\n");
        return ($FM_ERR_INVALID_ARGUMENT);
    }

    my Types::tp_ipAddr $ipAddr = Types::tp_ipAddr->new();
    if ($self->InetPToN($ipAddress, $ipAddr) != $FM_OK)
    {
        print($TP_MSG_ERR_ROUTING_INVALID_ADDR);
        return ($FM_ERR_INVALID_ARGUMENT);
    }

    if ($ipAddr->{'af'} == TP_AF_INET && $ipAddr->{'prefix'} != 32)
    {
        print($TP_MSG_ERR_ROUTING_INVALID_ADDR);
        return ($FM_ERR_INVALID_ARGUMENT);
    }

    my $interface = -1;
    my $vlan = 0;

    my $mode = shift(@args);

    if (!defined($mode))
    {
        print "Insufficient arguments\n";
        return ($FM_ERR_INVALID_ARGUMENT);
    }
    elsif ($mode eq "interface")
    {
        $interface = shift(@args);
        if (!defined($interface))
        {
            print "Insufficient arguments\n";
            return ($FM_ERR_INVALID_ARGUMENT);
        }
    }
    elsif ($mode eq "vlan")
    {
        $vlan = shift(@args);
        if (!defined($vlan))
        {
            print "Insufficient arguments\n";
            return ($FM_ERR_INVALID_ARGUMENT);
        }
    }
    else
    {
        print "Invalid argument\n";
        return ($FM_ERR_INVALID_ARGUMENT);
    }

    my $arpEntry = SDK::fm_arpEntry->new();
    $self->SetIpAddr($ipAddr, $arpEntry->{'ipAddr'});
    $arpEntry->{'interface'} = $interface;
    $arpEntry->{'vlan'} = $vlan;

    return ($FM_OK, $arpEntry);

}   # end ParseArpEntryArgs


##@cmethod private int SetIpAddr(Types::tp_ipAddr &source,
#                                SDK::fm_ipAddr   &destination)
#
# @brief        Converts a Types::tp_ipAddr network address structure into a
#               SDK::fm_ipAddr network address data structure
#
# @param[in]    source Points to a Types::tp_ipAddr Perl data structure
#
# @param[out]   destination Points to a SDK::fm_ipAddr Perl data structure
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub SetIpAddr
{
    my $self                    = shift(@_);
    my Types::tp_ipAddr $source = shift(@_);
    my $destination             = shift(@_);

    if (!blessed($source)
        || !$source->isa('Types::tp_ipAddr')
        || !blessed($destination)
        || !$destination->isa('SDK::fm_ipAddr'))
    {
        return $FM_FAIL;
    }
    $destination->{'addr'} = $source->{'address'};
    $destination->{'isIPv6'} = $source->{'af'} == TP_AF_INET6;
    return $FM_OK;
}

##@cmethod private int SetRouteEntry(Types::tp_ipAddr   &ipAddr,
#                                    Types::tp_ipAddr   &nextHop,
#                                    Types::tp_ipAddr   &interface,
#                                    int                vrid,
#                                    SDK::fm_routeEntry &routeEntry)
#
# @brief        Populates a SDK::fm_routeEntry data structure with a unicast
#               route entry
#
# @param[in]    ipAddr The unicast destination IP address
#
# @param[in]    nextHop The gateway IP address
#
# @param[in]    interface The interface identifier
#
# @param[in]    vrid The virtual router ID. Set to 0 to use the physical router
#
# @param[out]   routeEntry The SDK::fm_routeEntry data structure to be
#               populated
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub SetRouteEntry
{
    my $self                        = shift(@_);
    my Types::tp_ipAddr $ipAddr     = shift(@_);
    my Types::tp_ipAddr $nextHop    = shift(@_);
    my Types::tp_ipAddr $interface  = shift(@_);
    my $vrid                        = shift(@_);
    my $routeEntry                  = shift(@_);

    if (!blessed($ipAddr) || !$ipAddr->isa('Types::tp_ipAddr')
        || !blessed($nextHop) || !$nextHop->isa('Types::tp_ipAddr')
        || !blessed($interface) || !$interface->isa('Types::tp_ipAddr')
        || !blessed($routeEntry) || !$routeEntry->isa('SDK::fm_routeEntry'))
    {
        return $FM_FAIL;
    }

    $routeEntry->{'routeType'} = $FM_ROUTE_TYPE_UNICAST;
    my $unicastRouteEntry = $routeEntry->{'data'}->{'unicast'};
    $self->SetIpAddr($ipAddr, $unicastRouteEntry->{'dstAddr'});
    $unicastRouteEntry->{'prefixLength'} = $ipAddr->{'prefix'};
    $self->SetIpAddr($nextHop, $unicastRouteEntry->{'nextHop'});
    $self->SetIpAddr($interface, $unicastRouteEntry->{'interfaceAddr'});
    $unicastRouteEntry->{'vrid'} = $vrid;

    return $FM_OK;
}

##@cmethod private int ValidateInterface(char *identifier, int *interface)
#
# @brief        Validates an interface identifier
#
# @param[in]    identifier The interface identifier to be validated
#
# @param[out]   interface points to user-allocated storage in which the
#               interface number is to be stored
#
# @note         It is assumed that on a multi-chip system the routing table on
#               all chips are in sync.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub ValidateInterface
{
    my ($self, $identifier, $interface) = @_;

    my $chip = $self->{CHIP};

    my Types::tp_ipAddr $ipAddr = Types::tp_ipAddr->new();
    if ($self->InetPToN($identifier, $ipAddr) != $FM_OK)
    {
        return $FM_FAIL;
    }
    # Ensure that a unique IP version 4 or 6 address is provided.
    if (($ipAddr->{'af'} == TP_AF_INET && $ipAddr->{'prefix'} != 32)
        || ($ipAddr->{'af'} == TP_AF_INET6 && $ipAddr->{'prefix'} != 128))
    {
        return $FM_FAIL;
    }

    my ($currentInterface, $nextInterface, $status);

    my $switchNum = shift(@{[$self->tpGetSwitches]});
    $chip->disableErrors();
    $status = $chip->fmGetInterfaceFirst($switchNum, \$currentInterface);
    if ($status == $FM_ERR_NO_MORE)
    {
        return $FM_FAIL;
    }
    INTERFACE: while ($status == $FM_OK)
    {
        my ($identifier, $searchToken);

        $identifier = SDK::fm_ipAddr->new();
        $searchToken = SDK::fm_voidptrW->new();
        $status = $chip->fmGetInterfaceAddrFirst($switchNum, $currentInterface,
                                                 $searchToken, $identifier);
        IDENTIFIER: while ($status == $FM_OK)
        {
            my ($match);

            if ($identifier->{'isIPv6'})
            {
                $match = arrayCompare($identifier->{'addr'},
                                      $ipAddr->{'address'});
            }
            else
            {
                $match =
                       $identifier->{'addr'}->[0] == $ipAddr->{'address'}->[0];
            }
            if ($match)
            {
                ${$interface} = $currentInterface;
                last INTERFACE;
            }

            $status = $chip->fmGetInterfaceAddrNext($switchNum, $searchToken,
                                                    $identifier);
        }

        $status = $chip->fmGetInterfaceNext($switchNum, $currentInterface,
                                            \$nextInterface);
        $currentInterface = $nextInterface;
    }
    $chip->enableErrors();
    return $status == $FM_OK || $status == $FM_ERR_NO_MORE ? $FM_OK : $FM_FAIL;
}

##@cmethod private int ValidateVRID(int vrid)
#
# @brief        Validates a virtual router ID
#
# @param[in]    vrid The virtual router ID to be validated
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub ValidateVRID
{
    my ($self, $vrid) = @_;

    my $chip = $self->{'CHIP'};

    if (!defined($vrid = $self->str2intnum($vrid)))
    {
        return $FM_FAIL;
    }

    my ($currentVrid, $nextVrid, $status);

    my $switchNum = shift(@{[$self->tpGetSwitches]});
    $chip->disableErrors();
    $status = $chip->fmGetVirtualRouterFirst($switchNum, \$currentVrid);
    VRID: while ($status == $FM_OK)
    {
        if ($currentVrid == $vrid)
        {
            last VRID;
        }

        $status = $chip->fmGetVirtualRouterNext($switchNum, $currentVrid,
                                                \$nextVrid);
        $currentVrid = $nextVrid;
    }
    $chip->enableErrors();
    return $status == $FM_OK ? $FM_OK : $FM_FAIL;
}

###############################################################################
#
#                           PUBLIC FUNCTIONS
#
###############################################################################

##@cmethod public int tpHandleAddArpEntry(char *ipAddress,
#                                         char *DMAC,
#                                         char *interface)
#
# @brief        Handles adding ARP entries
#
# @param[in]    ipAddress The IP version 4 or IP version 6 address for which an
#               ARP entry is to be added
#
# @param[in]    DMAC The destination MAC address associated with the IP address
#
# @param[in]    interface The interface to be associated with the ARP entry
#               that is to be added
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleAddArpEntry
{
    my ($self, $ipAddress, $DMAC, $interface) = @_;

    my $chip = $self->{CHIP};

    if (!defined($ipAddress) || !defined($DMAC) || !defined($interface))
    {
        print("Must specify <ip address> <DMAC> <interface>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my Types::tp_ipAddr $ipAddr = Types::tp_ipAddr->new();
    if ($self->InetPToN($ipAddress, $ipAddr) != $FM_OK)
    {
        print($TP_MSG_ERR_ROUTING_INVALID_ADDR);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    # Ensure that a unique IP version 4 or 6 address is provided.
    if (($ipAddr->{'af'} == TP_AF_INET && $ipAddr->{'prefix'} != 32)
        || ($ipAddr->{'af'} == TP_AF_INET6 && $ipAddr->{'prefix'} != 128))
    {
        print($TP_MSG_ERR_ROUTING_INVALID_ADDR);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if ($DMAC ne "stateless")
    {
        if ($self->validateL2Address($DMAC) != $FM_OK)
        {
            print($TP_MSG_ERR_MAC_INVALID_ADDRESS);
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }
    else
    {
        # $FM_MAC_STATELESS_AUTOCONFIG
        $DMAC = "00:15:ed:00:00:00";
    }

    if ($self->ValidateInterface($interface, \$interface) != $FM_OK)
    {
        print($TP_MSG_ERR_ROUTING_INVALID_INTERFACE);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    # Populate the ARP entry to be added to the ARP table.
    my $arpEntry = SDK::fm_arpEntry->new();
    $self->SetIpAddr($ipAddr, $arpEntry->{'ipAddr'});
    $arpEntry->{'interface'} = $interface;
    $arpEntry->{'macAddr'} = $DMAC;

    $chip->disableErrors();
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status = $chip->fmAddARPEntry($switchNum, $arpEntry);
        if ($status != $FM_OK)
        {
            goto ABORT;
        }
    }
    $chip->enableErrors();

    return $FM_OK;

ABORT:
    printf("ARP entry for %s cannot be created!\n",
           $arpEntry->{'ipAddr'}->{'addr'});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        $chip->fmDeleteARPEntry($switchNum, $arpEntry);
    }
    return $FM_FAIL;
}

##@cmethod public int tpHandleAddInterface(char *identifier, int vlan)
#
# @brief        Handles adding a routing interface
#
# @param[in]    identifier The interface identifier specified as a CIDR style
#               IP version 4 or IP version 6 address
#
# @param[in]    vlan The VLAN the interface is to be associated with
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleAddInterface
{
    my ($self, $identifier, $vlan) = @_;

    my $chip = $self->{CHIP};

    if (!defined($identifier) || !defined($vlan))
    {
        print("Must specify <interface> <vlan>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my Types::tp_ipAddr $ipAddr = Types::tp_ipAddr->new();
    if ($self->InetPToN($identifier, $ipAddr) != $FM_OK)
    {
        print($TP_MSG_ERR_ROUTING_INVALID_INTERFACE);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    # Ensure that a unique IP version 4 address is provided.
    if ($ipAddr->{'af'} == TP_AF_INET && $ipAddr->{'prefix'} != 32)
    {
        print($TP_MSG_ERR_ROUTING_INVALID_INTERFACE);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (!defined($vlan = $self->str2intnum($vlan)))
    {
        print($TP_MSG_ERR_VLAN_INVALID_SCALAR);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    if (!$self->_tpIsValidVLAN($vlan))
    {
        print($TP_MSG_ERR_VLAN_INVALID_ID);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $interfaces = [];
    my $previousInterface = undef;
    $chip->disableErrors();
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my ($status);

        $status = $chip->fmCreateInterface($switchNum,
                                           \$interfaces->[$switchNum]);
        if ($status != $FM_OK
            || (defined($previousInterface)
                && $interfaces->[$switchNum] != $previousInterface))
        {
            if ($status == $FM_ERR_TABLE_FULL)
            {
                print("Out of interfaces!\n");
            }
            goto ABORT;
        }

        my $address = SDK::fm_ipAddr->new();
        $self->SetIpAddr($ipAddr, $address);
        $status = $chip->fmAddInterfaceAddr($switchNum,
                                            $interfaces->[$switchNum],
                                            $address);
        if ($status != $FM_OK)
        {
            goto ABORT;
        }

        my %void = (type => "fm_uint16", value => $vlan);
        $status = $chip->fmSetInterfaceAttribute($switchNum,
                                                 $interfaces->[$switchNum],
                                                 $FM_INTERFACE_VLAN, \%void);
        if ($status != $FM_OK)
        {
            printf("Cannot associate VLAN %d with interface %s!\n",
                   $vlan, $identifier);
            goto ABORT;
        }

        $previousInterface = $interfaces->[$switchNum];
    }
    $chip->enableErrors();

    return $FM_OK;

ABORT:
    printf("Interface %s cannot be created!\n", $identifier);
    foreach my $switchNum ($self->tpGetSwitches)
    {
        if (defined($interfaces->[$switchNum]))
        {
            $chip->fmDeleteInterface($switchNum, $interfaces->[$switchNum]);
        }
    }
    $chip->enableErrors();
    return $FM_FAIL;
}

##@cmethod public int tpHandleAddRoute(char *subnet,
#                                      char *gateway
#                                      char *interface,
#                                      int  vrid)
#
# @brief        Handles adding a route
#
# @param[in]    subnet The host or subnet to add a route for
#
# @param[in]    gateway The gateway
#
# @param[in]    identifier The interface identifier
#
# @param[in]    vrid @optional The virtual router ID in the range 1-255
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleAddRoute
{
    my ($self, $subnet, $gateway, $identifier, $vrid) = @_;

    my $chip = $self->{CHIP};

    if (!defined($subnet) || !defined($gateway) || !defined($identifier))
    {
        print("Must specify <subnet> <gateway> <interface>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my Types::tp_ipAddr $ipAddr = Types::tp_ipAddr->new();
    if ($self->InetPToN($subnet, $ipAddr) != $FM_OK)
    {
        print($TP_MSG_ERR_ROUTING_INVALID_ADDR);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (defined($vrid))
    {
        if ($self->ValidateVRID($vrid) != $FM_OK)
        {
            print($TP_MSG_ERR_ROUTING_INVALID_VRID);
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }
    else
    {
        $vrid = 0;
    }

    my $routeEntry = SDK::fm_routeEntry->new();

    if ($gateway ne "ECMP")
    {
        my Types::tp_ipAddr $nextHop = Types::tp_ipAddr->new();
        if ($self->InetPToN($gateway, $nextHop) != $FM_OK)
        {
            print($TP_MSG_ERR_ROUTING_INVALID_NEXTHOP);
            return $FM_ERR_INVALID_ARGUMENT;
        }
        # Ensure that a unique IP version 4 address is provided.
        if ($nextHop->{'af'} == TP_AF_INET && $nextHop->{'prefix'} != 32)
        {
            print($TP_MSG_ERR_ROUTING_INVALID_NEXTHOP);
            return $FM_ERR_INVALID_ARGUMENT;
        }
    
        my $temporary;
        if ($self->ValidateInterface($identifier, \$temporary) != $FM_OK)
        {
            print($TP_MSG_ERR_ROUTING_INVALID_INTERFACE);
            return $FM_ERR_INVALID_ARGUMENT;
        }
        my Types::tp_ipAddr $interface = Types::tp_ipAddr->new();
        $self->InetPToN($identifier, $interface);

        $self->SetRouteEntry($ipAddr, $nextHop, $interface, $vrid, $routeEntry);
    }
    else
    {
        $routeEntry->{'routeType'} = $FM_ROUTE_TYPE_UNICAST_ECMP;
        my $unicastRouteEntry = $routeEntry->{'data'}->{'unicastECMP'};
        $self->SetIpAddr($ipAddr, $unicastRouteEntry->{'dstAddr'});
        $unicastRouteEntry->{'prefixLength'} = $ipAddr->{'prefix'};
        $unicastRouteEntry->{'ecmpGroup'} = $identifier;
        $unicastRouteEntry->{'vrid'} = $vrid;
    }

    $chip->disableErrors();
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status = $chip->fmAddRoute($switchNum, $routeEntry,
                                       $FM_ROUTE_STATE_UP);
        if ($status != $FM_OK)
        {
            goto ABORT;
        }
    }
    $chip->enableErrors();

    return $FM_OK;

ABORT:
    printf("Cannot add route for %s via %s\n", $subnet, $gateway);
    foreach my $switchNum ($self->tpGetSwitches)
    {
        $chip->fmDeleteRoute($switchNum, $routeEntry);
    }
    $chip->enableErrors();
    return $FM_FAIL;
}

##@cmethod public int tpHandleCreateVirtualRouter(int vrid)
#
# @brief        Handles creating a virtual router
#
# @param[in]    vrid The virtual router ID in the range 1-255
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleCreateVirtualRouter
{
    my ($self, $vrid) = @_;

    my $chip = $self->{'CHIP'};

    if (!defined($vrid))
    {
        print("Must specify <vrid>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if ($self->ValidateVRID($vrid) == $FM_OK)
    {
        print($TP_MSG_ERR_ROUTING_INVALID_VRID);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    $chip->disableErrors();
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status = $chip->fmCreateVirtualRouter($switchNum, $vrid);
        if ($status != $FM_OK)
        {
            goto ABORT;
        }
    }
    $chip->enableErrors();

    return $FM_OK;

ABORT:
    printf("Cannot create virtual router %d!\n", $vrid);
    foreach my $switchNum ($self->tpGetSwitches)
    {
        $chip->fmDeleteVirtualRouter($switchNum, $vrid);
    }
    $chip->enableErrors();
    return $FM_FAIL;
}


##@cmethod public int tpHandleDelArpEntry(char *ipAddress)
#
# @brief        Handles deleting an ARP entry
#
# @param[in]    ipAddress The CIDR style IP address pointing to the ARP entry
#               to be deleted
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleDelArpEntry
{
    my ($self, $ipAddress, @args) = @_;

    my $chip = $self->{CHIP};

    my $result = $FM_OK;
    if (defined($ipAddress) && $ipAddress eq "all")
    {
        my ($status);

        my $switchCount = scalar(@{[$self->tpGetSwitches]});
        SWITCH: foreach my $switchNum ($self->tpGetSwitches)
        {
            my ($numEntries);

            $status = $chip->fmGetARPEntryList($switchNum, \$numEntries,
                                               $__arpEntries, $FM_MAX_ARPS);
            if ($status != $FM_OK)
            {
                $result = $FM_FAIL;
                printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                print("ARP entries cannot be deleted!\n");
                next SWITCH;
            }

            ARP: for (my $i = 0; $i < $numEntries; $i++)
            {
                $status = $chip->fmDeleteARPEntry($switchNum,
                                                  $__arpEntries->[$i]);
                if ($status != $FM_OK)
                {
                    my ($cidr);

                    $result = $FM_FAIL;
                    printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                    my Types::tp_ipAddr $ipAddr = Types::tp_ipAddr->new();
                    $self->GetIpAddr($__arpEntries->[$i]->{'ipAddr'}, $ipAddr);
                    $ipAddr->{'prefix'} = 32;
                    $self->InetNToP($ipAddr, \$cidr);
                    printf("Cannot delete ARP entry for %s\n", $cidr);
                    next ARP;
                }
            }
        }
    }
    else
    {
        if (!defined($ipAddress))
        {
            print("Must specify <ip address>!\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }

        my Types::tp_ipAddr $ipAddr = Types::tp_ipAddr->new();
        if ($self->InetPToN($ipAddress, $ipAddr) != $FM_OK)
        {
            print($TP_MSG_ERR_ROUTING_INVALID_ADDR);
            return $FM_ERR_INVALID_ARGUMENT;
        }
        if ($ipAddr->{'af'} == TP_AF_INET && $ipAddr->{'prefix'} != 32)
        {
            print($TP_MSG_ERR_ROUTING_INVALID_ADDR);
            return $FM_ERR_INVALID_ARGUMENT;
        }

        my $interface = -1;
        my $vlan = 0;

        my $mode = shift(@args);

        if (!defined($mode))
        {
            print "Insufficient arguments\n";
            return $FM_ERR_INVALID_ARGUMENT;
        }
        elsif ($mode eq "interface")
        {
            $interface = shift(@args);
            if (!defined($interface))
            {
                print "Insufficient arguments\n";
                return $FM_ERR_INVALID_ARGUMENT;
            }
        }
        elsif ($mode eq "vlan")
        {
            $vlan = shift(@args);
            if (!defined($vlan))
            {
                print "Insufficient arguments\n";
                return $FM_ERR_INVALID_ARGUMENT;
            }
        }
        else
        {
            print "Invalid argument\n";
            return $FM_ERR_INVALID_ARGUMENT;
        }

        my $switchCount = scalar(@{[$self->tpGetSwitches]});
        SWITCH: foreach my $switchNum ($self->tpGetSwitches)
        {
            my $entryW = SDK::fm_arpEntry->new();
            $self->SetIpAddr($ipAddr, $entryW->{'ipAddr'});

            $entryW->{'interface'} = $interface;
            $entryW->{'vlan'} = $vlan;

            my $status = $chip->fmDeleteARPEntry($switchNum, $entryW);
            if ($status != $FM_OK)
            {
                $result = $FM_FAIL;
                printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                printf("Cannot delete ARP entry for %s!\n", $ipAddress);
                next SWITCH;
            }
        }
    }
    return $result;

}   # end tpHandleDelArpEntry


##@cmethod public in tpHandleDelInterface(char *identifier)
#
# @brief        Handles deleting an interface
#
# @param[in]    identifier The interface identifier
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleDelInterface
{
    my ($self, $identifier) = @_;

    my $chip = $self->{CHIP};

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    if (defined($identifier) && $identifier eq 'all')
    {
        SWITCH: foreach my $switchNum ($self->tpGetSwitches)
        {
            my ($numInterfaces);

            my $interfaces = [(0) x $FM_MAX_IP_INTERFACES];
            my $status = $chip->fmGetInterfaceList($switchNum,
                                                   \$numInterfaces,
                                                   $interfaces,
                                                   $FM_MAX_IP_INTERFACES);
            if ($status != $FM_OK)
            {
                $result = $FM_FAIL;
                printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                print("Interfaces cannot be deleted!\n");
                next SWITCH;
            }

            IF: for (my $i = 0; $i < $numInterfaces; $i++)
            {
                my $status = $chip->fmDeleteInterface($switchNum,
                                                      $interfaces->[$i]);
                if ($status != $FM_OK)
                {
                    $result = $FM_FAIL;
                    printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                    printf("Cannot delete interface %d!\n", $interfaces->[$i]);
                    next IF;
                }
            }
        }
    }
    else
    {
        my ($interface);

        if (!defined($identifier))
        {
            print("Must specify <ip address>!\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }

        if ($self->ValidateInterface($identifier, \$interface) != $FM_OK)
        {
            print($TP_MSG_ERR_ROUTING_INVALID_INTERFACE);
            return $FM_ERR_INVALID_ARGUMENT;
        }

        SWITCH: foreach my $switchNum ($self->tpGetSwitches)
        {
            my $status = $chip->fmDeleteInterface($switchNum, $interface);
            if ($status != $FM_OK)
            {
                $result = $FM_FAIL;
                printf("Switch %d: ", $switchNum) if ($switchNum > 1);
                printf("Interface %s cannot be deleted!\n", $identifier);
                next SWITCH;
            }
        }
    }
    return $result;
}

##@cmethod public int tpHandleDelRoute(char *subnet,
#                                      char *gateway,
#                                      char *identifier,
#                                      int  vrid)
#
# @brief        Handles deleting a route or set of routes
#
# @param[in]    subnet
#
# @param[in]    gateway @optional
#
# @param[in]    identifier The interface identifier
#
# @param[in]    vrid @optional The virtual router ID in the range 1-255
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleDelRoute
{
    my ($self, $subnet, $gateway, $identifier, $vrid) = @_;

    my $chip = $self->{CHIP};

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    if (defined($subnet) && $subnet eq "all")
    {
        $chip->disableErrors();
        SWITCH: foreach my $switchNum ($self->tpGetSwitches)
        {
            my ($numRoutes);

            my $status = $chip->fmGetRouteList($switchNum, \$numRoutes,
                                               $__routeEntries, $FM_MAX_ARPS);
            if ($status != $FM_OK)
            {
                $result = $FM_FAIL;
                printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                print("Routes cannot be deleted!\n");
                next SWITCH;
            }

            ROUTE: for (my $i = 0; $i < $numRoutes; $i++)
            {
                if ($chip->fmIsRouteEntryUnicast($__routeEntries->[$i]))
                {
                    my $status = $chip->fmDeleteRoute($switchNum,
                                                      $__routeEntries->[$i]);
                    if ($status != $FM_OK)
                    {
                        my ($cidr);

                        $result = $FM_FAIL;
                        printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                        my Types::tp_ipAddr $ipAddr = Types::tp_ipAddr->new();
                        $self->GetIpAddr($__routeEntries->[$i]->{'dstAddr'},
                                         $ipAddr);
                        $ipAddr->{'prefix'} =
                                       $__routeEntries->[$i]->{'prefixLength'};
                        $self->InetNToP($ipAddr, \$cidr);
                        printf("Cannot delete route for %s\n", $cidr);
                        next ROUTE;
                    }
                }
            }
        }
        $chip->enableErrors();
    }
    else
    {
        if (!defined($subnet) || !defined($identifier))
        {
            print("Must specify <subnet> <interface>!\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }

        my Types::tp_ipAddr $ipAddr = Types::tp_ipAddr->new();
        if ($self->InetPToN($subnet, $ipAddr) != $FM_OK)
        {
            print($TP_MSG_ERR_ROUTING_INVALID_ADDR);
            return $FM_ERR_INVALID_ARGUMENT;
        }

        if (defined($vrid))
        {
            if ($self->ValidateVRID($vrid) != $FM_OK)
            {
                print($TP_MSG_ERR_ROUTING_INVALID_VRID);
                return $FM_ERR_INVALID_ARGUMENT;
            }
        }
        else
        {
            $vrid = 0;
        }

        my $routeEntry = SDK::fm_routeEntry->new();

        if ($gateway ne "ECMP")
        {
            my Types::tp_ipAddr $nextHop = Types::tp_ipAddr->new();
            if (defined($gateway))
            {
                if ($self->InetPToN($gateway, $nextHop) != $FM_OK)
                {
                    print($TP_MSG_ERR_ROUTING_INVALID_NEXTHOP);
                    return $FM_ERR_INVALID_ARGUMENT;
                }
                if ($nextHop->{'af'} == TP_AF_INET && $nextHop->{'prefix'} != 32)
                {
                    print($TP_MSG_ERR_ROUTING_INVALID_NEXTHOP);
                    return $FM_ERR_INVALID_ARGUMENT;
                }
            }
            else
            {
                $nextHop->{'af'} = TP_AF_INET;
                $nextHop->{'address'}->[0] = 0xFFFFFFFF;
            }

            my $temporary;
            if ($self->ValidateInterface($identifier, \$temporary) != $FM_OK)
            {
                print($TP_MSG_ERR_ROUTING_INVALID_INTERFACE);
                return $FM_ERR_INVALID_ARGUMENT;
            }
            my Types::tp_ipAddr $interface = Types::tp_ipAddr->new();
            $self->InetPToN($identifier, $interface);

            $self->SetRouteEntry($ipAddr, $nextHop, $interface, $vrid,
                                 $routeEntry);
        }
        else
        {
            $routeEntry->{'routeType'} = $FM_ROUTE_TYPE_UNICAST_ECMP;
            my $unicastRouteEntry = $routeEntry->{'data'}->{'unicastECMP'};
            $self->SetIpAddr($ipAddr, $unicastRouteEntry->{'dstAddr'});
            $unicastRouteEntry->{'prefixLength'} = $ipAddr->{'prefix'};
            $unicastRouteEntry->{'ecmpGroup'} = $identifier;
            $unicastRouteEntry->{'vrid'} = $vrid;
        }

        $chip->disableErrors();
        SWITCH: foreach my $switchNum ($self->tpGetSwitches)
        {
            my $status = $chip->fmDeleteRoute($switchNum, $routeEntry);
            if ($status != $FM_OK)
            {
                $result = $FM_FAIL;
                printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                my $format = "Cannot delete route for %s";
                if (defined($gateway))
                {
                    $format .= " via %s";
                }
                printf($format . "\n", $subnet, $gateway);
                next SWITCH;
            }
        }
        $chip->enableErrors();
    }
    return $result;
}

##@cmethod public int tpHandleDelVirtualRouter(int vrid)
sub tpHandleDelVirtualRouter
{
    my ($self, $vrid) = @_;

    my $chip = $self->{'CHIP'};

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    if (defined($vrid) && $vrid eq 'all')
    {
        my $routers = [(0) x $FM_MAX_VIRTUAL_ROUTERS];
        $chip->disableErrors();
        SWITCH: foreach my $switchNum ($self->tpGetSwitches)
        {
            my ($numRouters);

            my $status = $chip->fmGetVirtualRouterList($switchNum,
                                                       \$numRouters,
                                                       $routers,
                                                       $FM_MAX_VIRTUAL_ROUTERS);
            if ($status != $FM_OK)
            {
                $result = $FM_FAIL;
                printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                print("Virtual routers cannot be deleted!\n");
                next SWITCH;
            }

            VRID: for (my $i = 0; $i < $numRouters; $i++)
            {
                my $status = $chip->fmDeleteVirtualRouter($switchNum,
                                                          $routers->[$i]);
                if ($status != $FM_OK)
                {
                    $result = $FM_FAIL;
                    printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                    printf("Cannot delete virtual router %d!\n",
                           $routers->[$i]);
                    next VRID;
                }
            }
        }
        $chip->enableErrors();
    }
    else
    {
        if ($self->ValidateVRID($vrid) != $FM_OK)
        {
            print($TP_MSG_ERR_ROUTING_INVALID_VRID);
            return $FM_ERR_INVALID_ARGUMENT;
        }

        $chip->disableErrors();
        SWITCH: foreach my $switchNum ($self->tpGetSwitches)
        {
            my $status = $chip->fmDeleteVirtualRouter($switchNum, $vrid);
            if ($status != $FM_OK)
            {
                $result = $FM_FAIL;
                printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                printf("Cannot delete virtual router %d!\n", $vrid);
                next SWITCH;
            }
        }
        $chip->enableErrors();
    }
    return $result;
}


##
# 
# 
sub tpHandleResetArpEntryUsed
{
    my ($self, @args) = @_;

#   printf("tpHandleResetArpEntryUsed: [ %s ]\n", join(' ', @args));

    my $chip = $self->{CHIP};

    my ($result, $arpEntry) = $self->ParseArpEntryArgs(@args);

    if ($result != $FM_OK)
    {
        return $result;
    }

    if ($arpEntry eq "all")
    {
        print "Invalid argument: $arpEntry\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $switchNum = shift(@{[$self->tpGetSwitches]});
    my $used = 0;

    $chip->disableErrors();
    my $status = $chip->fmGetARPEntryUsed($switchNum, $arpEntry, \$used, $TRUE);
    $chip->enableErrors();

    return $result;

}   # end tpHandleResetArpEntryUsed


##@cmethod public int tpHandleSetRouterAttribute(char *attribute,
#                                                char *value)
#
# @brief        Handles setting a router attribute
#
# @param[in]    attribute The router attribute to be set
#
# @param[in]    value The value to set the attribute to
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
# 
sub tpHandleSetRouterAttribute
{
    my ($self, $attribute, $value) = @_;

    my $chip = $self->{CHIP};

    if (!defined($attribute) || !exists($__routerAttrs{$attribute}))
    {
        print("Invalid command!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $attrType;
    my $attrValue;

    if ($attribute eq 'mac' || $attribute eq 'virtual-mac')
    {
        if (!defined($value)
            || $self->validateL2Address($value) != $FM_OK)
        {
            print($TP_MSG_ERR_MAC_INVALID_ADDRESS);
            return $FM_ERR_INVALID_ARGUMENT;
        }

        $attrType = "fm_macaddr";
        $attrValue = $value;
    }
    elsif ($attribute eq 'ttl-disposal')
    {
        if (!defined($value) || !defined($__routerTTL1Attrs{$value}))
        {
            print "Invalid ttl-disposal attribute\n";
            return $FM_ERR_INVALID_ARGUMENT;
        }

        $attrType = "fm_int";
        $attrValue = $__routerTTL1Attrs{$value};
    }
    elsif ($attribute eq 'trap-ip-opts')
    {
        if (!defined($value))
        {
            print "Invalid argument\n";
            return $FM_ERR_INVALID_ARGUMENT;
        }

        $attrType = "fm_bool";
        if ($value eq "on" || $value eq "enable")
        {
            $attrValue = $FM_ENABLED;
        }
        elsif ($value eq "off" || $value eq "disable")
        {
            $attrValue = $FM_DISABLED;
        }
        else
        {
            print "Invalid argument\n";
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }

    $chip->disableErrors();
    SWITCH: foreach my $switchNum ($self->tpGetSwitches)
    {
        my %void = (type => $attrType, value => $attrValue);
        my $status = $chip->fmSetRouterAttribute($switchNum,
                                                 $__routerAttrs{$attribute},
                                                 \%void);
        if ($status != $FM_OK)
        {
            goto ABORT;
        }

        if ($attribute eq 'mac')
        {
            $chip->fmSetRouterState($switchNum, 0, $FM_ROUTE_STATE_UP);
        }
    }
    $chip->enableErrors();

    return $FM_OK;

ABORT:
    printf("Cannot set %s router attribute!\n", $attribute);
    $chip->enableErrors();
    return $FM_FAIL;
}

##@cmethod public int tpHandleShowArpEntry(void)
#
# @brief        Handles showing ARP entries
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleShowArpEntry
{
    my ($self, @args) = @_;

#   printf("tpHandleShowArpEntry: [ %s ]\n", join(' ', @args));

    my $chip = $self->{CHIP};

    my ($result, $arpEntry) = $self->ParseArpEntryArgs(@args);
    return $result if $result != $FM_OK;

    if ($arpEntry eq "all")
    {
        my $switchCount = scalar(@{[$self->tpGetSwitches]});
        my @numEntries = ();

        $chip->disableErrors();
        foreach my $switchNum ($self->tpGetSwitches)
        {
            my $status = $chip->fmGetARPEntryList($switchNum,
                                                  \$numEntries[$switchNum],
                                                  $__arpEntries, $FM_MAX_ARPS);
            if ($status != $FM_OK)
            {
                $result = $FM_FAIL;
                printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                print("ARP table cannot be shown!\n");
                next SWITCH;
            }
    
            for (my $i = 0; $i < $numEntries[$switchNum]; $i++)
            {
                my ($cidr);
    
                if (($i % 40) == 0)
                {
                    print("\n");
                    printf("Switch %d\n", $switchNum) if ($switchCount > 1);
                    printf("%-43s %-17s VLAN\n", "IP address", "MAC address");
                    printf("%s\n", '-' x 78);
                }
    
                my $entry = $__arpEntries->[$i];
                my %void = (type => "fm_uint16", value => 0);
                my $status = $chip->fmGetInterfaceAttribute($switchNum,
                                                            $entry->{'interface'},
                                                            $FM_INTERFACE_VLAN,
                                                            \%void);
                if ($status != $FM_OK)
                {
                    $void{'value'} = -1;
                }
                my Types::tp_ipAddr $ipAddr = Types::tp_ipAddr->new();
                $self->GetIpAddr($entry->{'ipAddr'}, $ipAddr);
                $self->InetNToP($ipAddr, \$cidr);

                # $FM_MAC_STATELESS_AUTOCONFIG
                if ($entry->{'macAddr'} eq "00:15:ed:00:00:00")
                {
                    printf("%-43s %-17s %4d\n",
                           $cidr, "Auto-Config MAC", $void{'value'});
                }
                else
                {
                    printf("%-43s %-17s %4d\n",
                           $cidr, $entry->{'macAddr'}, $void{'value'});
                }
            }
        }
        $chip->enableErrors();
    
        foreach my $switchNum ($self->tpGetSwitches)
        {
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            printf("\nARP table contains %d entries\n", $numEntries[$switchNum]);
        }
    }
    else
    {
        my $switchNum = shift(@{[$self->tpGetSwitches]});

        my $arpInfo = SDK::fm_arpEntryInfo->new();

        $chip->disableErrors();
        my $status = $chip->fmGetARPEntryInfo($switchNum, $arpEntry, $arpInfo);
        $chip->enableErrors();

        if ($status != $FM_OK)
        {
            return $FM_FAIL;
        }

        my $arp = $arpInfo->{'arp'};
        my $arpIpAddr;

        my Types::tp_ipAddr $ipAddr = Types::tp_ipAddr->new();
        $self->GetIpAddr($arp->{'ipAddr'}, $ipAddr);
        $self->InetNToP($ipAddr, \$arpIpAddr);

        printf("ipAddr      %s\n", $arpIpAddr);
        printf("interface   %s\n", $arp->{'interface'});
        printf("vlan        %s\n", $arp->{'vlan'});
        printf("macAddr     %s\n", $arp->{'macAddr'});
        printf("used        %s\n", $arpInfo->{'used'} ? "yes" : "no");

    } # else

    return $result;

}   # end tpHandleShowArpEntry


##@cmethod public int tpHandleShowInterface(void)
#
# @note         It is assumed that on a multi-chip system the routing table on
#               all chips are in sync.
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
sub tpHandleShowInterface
{
    my ($self) = @_;

    my $chip = $self->{CHIP};

    print("\n");
    printf("Interface# %-43s VLAN\n", 'Identifier');
    printf("%s\n", '-' x 60);

    my ($currentInterface, $nextInterface, $status);

    my $switchNum = shift(@{[$self->tpGetSwitches]});
    $chip->disableErrors();
    $status = $chip->fmGetInterfaceFirst($switchNum, \$currentInterface);
    while ($status == $FM_OK)
    {
        my ($identifier, $searchToken);

        my @addresses = ();
        $identifier = SDK::fm_ipAddr->new();
        $searchToken = SDK::fm_voidptrW->new();
        $status = $chip->fmGetInterfaceAddrFirst($switchNum, $currentInterface,
                                                 $searchToken, $identifier);
        while ($status == $FM_OK)
        {
            my ($cidr);

            my Types::tp_ipAddr $ipAddr = Types::tp_ipAddr->new();
            $self->GetIpAddr($identifier, $ipAddr);
            $self->InetNToP($ipAddr, \$cidr);
            push(@addresses, $cidr);

            $status = $chip->fmGetInterfaceAddrNext($switchNum, $searchToken,
                                                    $identifier);
        }

        my %void = (type => "fm_uint16", value => 0);
        $status = $chip->fmGetInterfaceAttribute($switchNum, $currentInterface,
                                                 $FM_INTERFACE_VLAN, \%void);

        foreach my $address (@addresses)
        {
            printf("%s %4d %-43s %4d\n",
                   ' ' x 5, $currentInterface, $address, $void{'value'});
        }

        $status = $chip->fmGetInterfaceNext($switchNum, $currentInterface,
                                            \$nextInterface);
        $currentInterface = $nextInterface;
    }
    $chip->enableErrors();

    print("\n");

    return $FM_OK;
}

##@cmethod public int tpHandleShowRoute(int vrid)
#
# @brief        Handles showing route entries
#
# @param[in]    vrid @optional The virtual router ID in the range 1-255
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleShowRoute
{
    my ($self, $vrid) = @_;

    my $chip = $self->{CHIP};

    if (!defined($vrid))
    {
        print("Invalid command!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }
    else
    {
        SWITCH: for ($vrid)
        {
            $_ eq 'physical' && do
            {
                $vrid = 0;
                last SWITCH;
            };

            $_ eq 'all' && do
            {
                $vrid = TP_ROUTE_ALL;
                last SWITCH;
            };

            if ($self->ValidateVRID($vrid) != $FM_OK)
            {
                print($TP_MSG_ERR_ROUTING_INVALID_VRID);
                return $FM_ERR_INVALID_ARGUMENT;
            }
        }
    }

    my ($numRoutes);

    my $switchNum = shift(@{[$self->tpGetSwitches]});
    $chip->disableErrors();
    my $status = $chip->fmGetRouteList($switchNum, \$numRoutes,
                                       $__routeEntries, $FM_MAX_ARPS);
    $chip->enableErrors();
    if ($status != $FM_OK)
    {
        print("Routing table cannot be shown!\n");
        return $FM_FAIL;
    }

    my $numUcast = 0;
    my $numMatches = 0;
    $chip->disableErrors();
    for (my $i = 0; $i < $numRoutes; $i++)
    {
        my $routeEntry = $__routeEntries->[$i];
        if ($chip->fmIsRouteEntryUnicast($routeEntry))
        {
            my ($gateway, $subnet);

            my $unicastRouteEntry = $routeEntry->{'data'}->{'unicast'};
            if ($vrid == TP_ROUTE_ALL || $unicastRouteEntry->{'vrid'} == $vrid)
            {
                if (($numMatches % 40) == 0)
                {
                    print("\n");
                    printf("VRID %-43s %s\n", 'IP address', 'gateway');
                    printf("%s\n", '-' x 78);
                }

                my Types::tp_ipAddr $ipAddr = Types::tp_ipAddr->new();
                $self->GetIpAddr($unicastRouteEntry->{'dstAddr'}, $ipAddr);
                $ipAddr->{'prefix'} = $unicastRouteEntry->{'prefixLength'};
                $self->InetNToP($ipAddr, \$subnet);
                $self->GetIpAddr($unicastRouteEntry->{'nextHop'}, $ipAddr);
                $ipAddr->{'prefix'} = undef;
                $self->InetNToP($ipAddr, \$gateway);
                printf("%4d %-43s %s\n",
                       $unicastRouteEntry->{'vrid'}, $subnet, $gateway);

                $numMatches += 1;
            }

            $numUcast += 1;
        }
    }
    $chip->enableErrors();

    my $numMcast = 0;
    $chip->disableErrors();
    ROUTE: for (my $i = 0; $i < $numRoutes; $i++)
    {
        my $routeEntry = $__routeEntries->[$i];
        if ($chip->fmIsRouteEntryMulticast($routeEntry))
        {
            if ($vrid == TP_ROUTE_ALL || $vrid == 0)
            {
                my ($destination, $source, $vlan);

                if (($numMcast % 40) == 0)
                {
                    print("\n");
                    printf("%-6s %-7s %-43s %s\n",
                           'Group#', 'VLAN', 'Source Address', 'Group Address');
                    printf("%s\n", '-' x 78);
                }

                my $multicastAddress = $routeEntry->{'data'}->{'multicast'};
                my $group = $multicastAddress->{'mcastGroup'};
                TYPE: for ($multicastAddress->{'addressType'})
                {

                    my $info = $multicastAddress->{'info'};
                    my Types::tp_ipAddr $ipAddr = Types::tp_ipAddr->new();
                    my Types::tp_vpPair $vlanID = Types::tp_vpPair->new(12);
               
                    $_ == $FM_MCAST_ADDR_TYPE_DSTIP && do
                    {
                        my $mcastInfo = $info->{'dstIpRoute'};
                        $self->GetIpAddr($mcastInfo->{'dstAddr'}, $ipAddr);
                        $ipAddr->{'prefix'} = $mcastInfo->{'dstPrefixLength'};
                        $self->InetNToP($ipAddr, \$destination);
                        $source = $self->IsIPv6($ipAddr) ? '::/0' : '0.0.0.0/0';
                        $vlan = '0/0';
                        last TYPE;
                    };

                    $_ == $FM_MCAST_ADDR_TYPE_DSTIP_VLAN && do
                    {
                        my $mcastInfo = $info->{'dstIpVlanRoute'};
                        $self->GetIpAddr($mcastInfo->{'dstAddr'}, $ipAddr);
                        $ipAddr->{'prefix'} = $mcastInfo->{'dstPrefixLength'};
                        $self->InetNToP($ipAddr, \$destination);
                        $source = $self->IsIPv6($ipAddr) ? '::/0' : '0.0.0.0/0';
                        $vlanID->{'value'} = $mcastInfo->{'vlan'};
                        $vlanID->{'prefix'} = $mcastInfo->{'vlanPrefixLength'};
                        $self->VPPairNToP($vlanID, \$vlan);
                        last TYPE;
                    };

                    $_ == $FM_MCAST_ADDR_TYPE_DSTIP_SRCIP && do
                    {
                        my $mcastInfo = $info->{'dstSrcIpRoute'};
                        $self->GetIpAddr($mcastInfo->{'dstAddr'}, $ipAddr);
                        $ipAddr->{'prefix'} = $mcastInfo->{'dstPrefixLength'};
                        $self->InetNToP($ipAddr, \$destination);
                        $self->GetIpAddr($mcastInfo->{'srcAddr'}, $ipAddr);
                        $ipAddr->{'prefix'} = $mcastInfo->{'srcPrefixLength'};
                        $self->InetNToP($ipAddr, \$source);
                        $vlan = '0/0';
                        last TYPE;
                    };

                    $_ == $FM_MCAST_ADDR_TYPE_DSTIP_SRCIP_VLAN && do
                    {
                        my $mcastInfo = $info->{'dstSrcIpVlanRoute'};
                        $self->GetIpAddr($mcastInfo->{'dstAddr'}, $ipAddr);
                        $ipAddr->{'prefix'} = $mcastInfo->{'dstPrefixLength'};
                        $self->InetNToP($ipAddr, \$destination);
                        $self->GetIpAddr($mcastInfo->{'srcAddr'}, $ipAddr);
                        $ipAddr->{'prefix'} = $mcastInfo->{'srcPrefixLength'};
                        $self->InetNToP($ipAddr, \$source);
                        $vlanID->{'value'} = $mcastInfo->{'vlan'};
                        $vlanID->{'prefix'} = $mcastInfo->{'vlanPrefixLength'};
                        $self->VPPairNToP($vlanID, \$vlan);
                        last TYPE;
                    };

                    do
                    {
                        printf("%d: Unknown multicast route type!\n",
                               $multicastAddress->{'addressType'});
                        next ROUTE;
                    };
                }

                printf("%6d %-7s %-43s %s\n",
                       $group, $vlan, $source, $destination);
            }

            $numMcast += 1;
        }
    }
    $chip->enableErrors();

    printf("\nRouting table contains %d entries (%d unicast, %d multicast)\n",
           $numRoutes, $numUcast, $numMcast);

    return $FM_OK;

}   # end tpHandleShowRoute


##@cmethod public int  pHandleShowRouter()
#
# @brief        Displays the global router attributes.
# 
# @param        None.
# 
# @return       FM_OK if successfull.
# @return       FM_FAIL otherwise.
# 
sub tpHandleShowRouter
{
    my ($self) = @_;

    my $chip = $self->{CHIP};

    my $getPhysicalMac = sub {
        my ($switchNum) = @_;
        my %void = (type => 'fm_macaddr', value => "");
        my $status = $chip->fmGetRouterAttribute(
                $switchNum,
                $FM_ROUTER_PHYSICAL_MAC_ADDRESS,
                \%void);
        return ($status == $FM_OK) ? $void{value} : "<invalid>";

    };  # end $getPhysicalMac

    my $getVirtualMac = sub {
        my ($switchNum) = @_;
        my %void = (type => 'fm_macaddr', value => "");
        my $status = $chip->fmGetRouterAttribute(
                $switchNum,
                $FM_ROUTER_VIRTUAL_MAC_ADDRESS,
                \%void);
        return ($status == $FM_OK) ? $void{value} : "<invalid>";

    };  # end $getVirtualMac

    my $getTTLDisposal = sub {
        my ($switchNum) = shift @_;
        my %void = (type => 'fm_int', value => 0);
        my $status = $chip->fmGetRouterAttribute(
                $switchNum,
                $FM_ROUTER_TRAP_TTL1,
                \%void);
        if ($status != $FM_OK)
        {
            return "<invalid>";
        }
        for my $key (keys(%__routerTTL1Attrs))
        {
            return $key if $void{value} == $__routerTTL1Attrs{$key};
        }
        return sprintf("unknown (%d)", $void{value});

    };  # end $getTTLDisposal

    my $getTrapRedirectEvent = sub {
        my ($switchNum) = shift @_;
        my %void = (type => 'fm_bool', value => 0);
        my $status = $chip->fmGetRouterAttribute(
                $switchNum,
                $FM_ROUTER_TRAP_REDIRECT_EVENT,
                \%void);
        if ($status != $FM_OK)
        {
            return "<invalid>";
        }
        elsif ($void{value})
        {
            return "enabled";
        }
        else
        {
            return "disabled";
        }
    };  # end $getTrapRedirectEvent

    my $getTrapIpOptions = sub {
        my ($switchNum) = shift @_;
        my %void = (type => 'fm_bool', value => 0);
        my $status = $chip->fmGetRouterAttribute(
                $switchNum,
                $FM_ROUTER_TRAP_IP_OPTIONS,
                \%void);
        if ($status != $FM_OK)
        {
            return "<invalid>";
        }
        elsif ($void{value})
        {
            return "enabled";
        }
        else
        {
            return "disabled";
        }
    };  # end $getTrapIpOptions

    my @switchList = $self->tpGetSwitches;

    foreach my $switchNum (@switchList)
    {
        print "\n";
        if ($#switchList > 1)
        {
            print "Switch $switchNum:\n";
            print "----------\n";
        }

        printf("physical-mac    %s\n", $getPhysicalMac->($switchNum));
        printf("ttl-disposal    %s\n", $getTTLDisposal->($switchNum));
        printf("trap-ip-opts    %s\n", $getTrapIpOptions->($switchNum));
        printf("virtual-mac     %s\n", $getVirtualMac->($switchNum));
    }

}   # end tpHandleShowRouter


##@cmethod public int tpHandleCreateEcmpGroup()
#
# @brief        Handles creating an ECMP Group
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleCreateEcmpGroup
{
    my ($self, $wideNextHop) = @_;
    my $groupId;
    my $status;
    my $nextHop = SDK::fm_nextHop->new();

    my $chip = $self->{'CHIP'};

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    $chip->disableErrors();
    foreach my $switchNum ($self->tpGetSwitches)
    {
        if (!defined($wideNextHop))
        {
            $status = $chip->fmCreateECMPGroup($switchNum, \$groupId, 0, $nextHop);
        }
        else
        {
            my $info = SDK::fm_ecmpGroupInfo->new();
            $info->{wideNextHops} = 1;
            $info->{numFixedEntries} = 1;
            $status = $chip->fmCreateECMPGroupV2($switchNum, \$groupId, $info);
        }

        if ($status != $FM_OK)
        {
            goto ABORT;
        }
        print("Create ECMP Group $groupId on switch $switchNum \n");
    }
    $chip->enableErrors();

    return $FM_OK;

ABORT:
    printf("Cannot create ECMP Group!\n");
    foreach my $switchNum ($self->tpGetSwitches)
    {
        $chip->fmDeleteECMPGroup($switchNum, $groupId);
    }
    $chip->enableErrors();
    return $FM_FAIL;
}


##@cmethod public int tpHandleDeleteEcmpGroup()
#
# @brief        Handles that delete an ECMP Group
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleDeleteEcmpGroup
{
    my ($self, $groupId) = @_;

    my $chip = $self->{'CHIP'};

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    $chip->disableErrors();
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status = $chip->fmDeleteECMPGroup($switchNum, $groupId);
        if ($status != $FM_OK)
        {
            goto ABORT;
        }
    }
    $chip->enableErrors();

    return $FM_OK;

ABORT:
    printf("Cannot Delete ECMP Group %d!\n", $groupId);

    $chip->enableErrors();
    return $FM_FAIL;
}


##@cmethod public int tpAddEcmpGroupNexthop()
#
# @brief        Add next hop to an ECMP Group
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpAddEcmpGroupNexthop
{
    my ($self, $groupId, $nextHopAddr, $interface, $trapCode) = @_;
    my $chip = $self->{'CHIP'};

    my $nextHop = SDK::fm_nextHop->new();

    my Types::tp_ipAddr $ipAddr = Types::tp_ipAddr->new();
    if ($self->InetPToN($nextHopAddr, $ipAddr) != $FM_OK)
    {
        print($TP_MSG_ERR_ROUTING_INVALID_ADDR);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    # Ensure that a unique IP version 4 or 6 address is provided.
    if (($ipAddr->{'af'} == TP_AF_INET && $ipAddr->{'prefix'} != 32)
        || ($ipAddr->{'af'} == TP_AF_INET6 && $ipAddr->{'prefix'} != 128))
    {
        print($TP_MSG_ERR_ROUTING_INVALID_ADDR);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    $self->SetIpAddr($ipAddr, $nextHop->{'addr'});

    my $temporary;
    if ($self->ValidateInterface($interface, \$temporary) != $FM_OK)
    {
        print($TP_MSG_ERR_ROUTING_INVALID_INTERFACE);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my Types::tp_ipAddr $interfaceAddr = Types::tp_ipAddr->new();
    $self->InetPToN($interface, $interfaceAddr);
    $self->SetIpAddr($interfaceAddr, $nextHop->{'interfaceAddr'});

    $nextHop->{'vlan'} = 0;

    if (defined $trapCode)
    {
        $nextHop->{'trapCode'} = $trapCode;
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    $chip->disableErrors();
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status = $chip->fmAddECMPGroupNextHops($switchNum, $groupId, 1, $nextHop);
        if ($status != $FM_OK)
        {
            goto ABORT;
        }
    }
    $chip->enableErrors();

    return $FM_OK;

ABORT:
    printf("Cannot add this next hop to this ECMP Group!\n");
    $chip->enableErrors();
    return $FM_FAIL;
}


##@cmethod public int tpAddEcmpGroupRawNexthop()
#
# @brief        Add raw next hop to an ECMP Group
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpAddEcmpGroupRawNexthop
{
    my ($self, $groupId, $nextHopType, $firstChk, $secondChk) = @_;
    my $chip = $self->{'CHIP'};

    my $first64 = Math::BigInt->new(0);
    my $second64 = Math::BigInt->new(0);

    if ($nextHopType eq "narrow")
    {
        $nextHopType = $FM_NEXTHOP_TYPE_RAW_NARROW;
    }
    elsif ($nextHopType eq "wide")
    {
        $nextHopType = $FM_NEXTHOP_TYPE_RAW_WIDE;
    }
    else
    {
        printf("Raw type must either be narrow or wide!\n");
        return $FM_FAIL;
    }

    if (!(defined $firstChk))
    {
        printf("You must define the a raw 64bit value!\n");
        return $FM_FAIL;
    }
    $first64 = Math::BigInt->new($firstChk);

    if ($nextHopType == $FM_NEXTHOP_TYPE_RAW_WIDE)
    {
        if (!(defined $secondChk))
        {
            printf("You must define the second raw 64bit value!\n");
            return $FM_FAIL;
        }
        $second64 = Math::BigInt->new($secondChk);
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    $chip->disableErrors();
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status = $chip->fmAddECMPGroupRawNextHop($switchNum, 
                                                     $groupId, 
                                                     $nextHopType,
                                                     $first64, 
                                                     $second64);
        if ($status != $FM_OK)
        {
            goto ABORT;
        }
    }
    $chip->enableErrors();

    return $FM_OK;

ABORT:
    printf("Cannot add this next hop entry to this ECMP Group!\n");
    $chip->enableErrors();
    return $FM_FAIL;
}


##@cmethod public int tpHandleAddEcmpGroupNexthop()
#
# @brief        Handles to add next hop to an ECMP Group
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleAddEcmpGroupNexthop
{
    my ($self, $groupId, $nextHop, @args) = @_;
    my $retVal;

    if ($nextHop eq "raw")
    {
        $retVal = $self->tpAddEcmpGroupRawNexthop($groupId, @args);
    }
    else
    {
        $retVal = $self->tpAddEcmpGroupNexthop($groupId, $nextHop, @args);
    }

    return $retVal;
}


##@cmethod public int tpDelEcmpGroupNexthop()
#
# @brief        Delete next hop from an ECMP Group
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpDelEcmpGroupNexthop
{
    my ($self, $groupId, $nextHopAddr, $interface) = @_;
    my $chip = $self->{'CHIP'};

    my $nextHop = SDK::fm_nextHop->new();

    my Types::tp_ipAddr $ipAddr = Types::tp_ipAddr->new();
    if ($self->InetPToN($nextHopAddr, $ipAddr) != $FM_OK)
    {
        print($TP_MSG_ERR_ROUTING_INVALID_ADDR);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    # Ensure that a unique IP version 4 or 6 address is provided.
    if (($ipAddr->{'af'} == TP_AF_INET && $ipAddr->{'prefix'} != 32)
        || ($ipAddr->{'af'} == TP_AF_INET6 && $ipAddr->{'prefix'} != 128))
    {
        print($TP_MSG_ERR_ROUTING_INVALID_ADDR);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    $self->SetIpAddr($ipAddr, $nextHop->{'addr'});

    my $temporary;
    if ($self->ValidateInterface($interface, \$temporary) != $FM_OK)
    {
        print($TP_MSG_ERR_ROUTING_INVALID_INTERFACE);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my Types::tp_ipAddr $interfaceAddr = Types::tp_ipAddr->new();
    $self->InetPToN($interface, $interfaceAddr);
    $self->SetIpAddr($interfaceAddr, $nextHop->{'interfaceAddr'});

    $nextHop->{'vlan'} = 0;

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    $chip->disableErrors();
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status = $chip->fmDeleteECMPGroupNextHops($switchNum, $groupId, 1, $nextHop);
        if ($status != $FM_OK)
        {
            goto ABORT;
        }
    }
    $chip->enableErrors();

    return $FM_OK;

ABORT:
    printf("Cannot delete this next hop from this ECMP Group!\n");
    $chip->enableErrors();
    return $FM_FAIL;
}


##@cmethod public int tpDelEcmpGroupNexthop()
#
# @brief        Delete a raw next hop from an ECMP Group
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpDelEcmpGroupRawNexthop
{
    my ($self, $groupId, $nextHopType, $firstChk, $secondChk) = @_;
    my $chip = $self->{'CHIP'};

    my $first64 = Math::BigInt->new(0);
    my $second64 = Math::BigInt->new(0);

    if ($nextHopType eq "narrow")
    {
        $nextHopType = $FM_NEXTHOP_TYPE_RAW_NARROW;
    }
    elsif ($nextHopType eq "wide")
    {
        $nextHopType = $FM_NEXTHOP_TYPE_RAW_WIDE;
    }
    else
    {
        printf("Raw type must either be narrow or wide!\n");
        return $FM_FAIL;
    }

    if (!(defined $firstChk))
    {
        printf("You must define the a raw 64bit value!\n");
        return $FM_FAIL;
    }
    $first64 = Math::BigInt->new($firstChk);

    if ($nextHopType == $FM_NEXTHOP_TYPE_RAW_WIDE)
    {
        if (!(defined $secondChk))
        {
            printf("You must define the second raw 64bit value!\n");
            return $FM_FAIL;
        }
        $second64 = Math::BigInt->new($secondChk);
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    $chip->disableErrors();
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status = $chip->fmDeleteECMPGroupRawNextHop($switchNum, 
                                                        $groupId, 
                                                        $nextHopType,
                                                        $first64, 
                                                        $second64);
        if ($status != $FM_OK)
        {
            goto ABORT;
        }
    }
    $chip->enableErrors();

    return $FM_OK;

ABORT:
    printf("Cannot delete this next hop entry from this ECMP Group!\n");
    $chip->enableErrors();
    return $FM_FAIL;
}


##@cmethod public int tpHandleDelEcmpGroupNexthop()
#
# @brief        Handles to delete next hop from an ECMP Group
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleDelEcmpGroupNexthop
{
    my ($self, $groupId, $nextHop, @args) = @_;
    my $retVal;

    if ($nextHop eq "raw")
    {
        $retVal = $self->tpDelEcmpGroupRawNexthop($groupId, @args);
    }
    else
    {
        $retVal = $self->tpDelEcmpGroupNexthop($groupId, $nextHop, @args);
    }

    return $retVal;
}

1;
