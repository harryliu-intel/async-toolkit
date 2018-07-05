# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Scripts/GenFrame/Utilities.pm
# Creation Date:    11/01/06
# Description:      Support routines for the GenFrame framework.
#
# INTEL CONFIDENTIAL
# Copyright 2006 - 2011 Intel Corporation. All Rights Reserved. 
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

package Scripts::GenFrame::Utilities;
use strict;
use warnings;

require Exporter;
use POSIX qw(ceil floor);
use Scripts::Utilities;
our @ISA = qw(Exporter);
our @EXPORT = qw(
    getIPv4Address
    getIPv6Address
    getMacAddress
    setIPv4Address
    setIPv6Address
    setMacAddress
    INDEX_MA_HI
    INDEX_MA_LO
);
our @EXPORT_FAIL = qw(
    getIPAddress
    setIPAddress
);

use constant INDEX_MA_HI    => 0;
use constant INDEX_MA_LO    => 1;

##@method private int[] getIPAddress(char *address, int length, char *separator)
# Convert the string representation of an IP address into a Perl array of
# 32-bit words
# @param[in] address The IP address to be parsed
# @param[in] length The number of bytes the specified IP address should consist
# of
# @param[in] separator The separator
# @return A Perl reference to a Perl array containing the parsed IP address
# ordered as <tt>(LSW .. MSW)</tt>
sub getIPAddress($$$)
{
    my ($address, $length, $separator) = @_;

    my @bytes = split(/$separator/o, $address);
    if (scalar(@bytes) != $length)
    {
        return undef;
    }
    my $words = [(0x0) x ceil($length / 4)];
    for (my $i=0 ; $i<$length ; $i++)
    {
        $words->[floor($i / 4)] |= (pop(@bytes) << (8 * ($i % 4)));
    }
    return $words;
}

##@method public int getIPv4Address(char *address)
# Convert the string representation of an IP version 4 address into a 32-bit
# word
# @param[in] address The IP version 4 address to be parsed
# @return The parsed IP version 4 address
sub getIPv4Address($)
{
    my ($address) = @_;

    return shift(@{getIPAddress($address, 4, "\\.")});
}

##@method public int[] getIPv6Address(char *address)
# Convert the string representation of an IP version 6 address into a Perl
# array of 32-bit words
# @param[in] address The IP version 6 address to be parsed
# @return A Perl reference to a Perl array containing the parsed IP version 6
# address ordered as <tt>(LSW .. MSW)</tt>
sub getIPv6Address($)
{
    my ($address) = @_;

    $_ = $address;
    our $n = 0;
    no strict 'vars';
    m/
        (?{ local $i = 0; })
        (\w+: (?{ $i = $i + 1; }))*
        (?{ $n = $i; })
    /x;
    if ($n == 7)
    {
        ##@method private int8_t[] x2d(int16_t x)
        # Split a 16-bit integer into its top and bottom 8-bits
        # @param[in] x A 16-bit integer
        # @return A Perl array containing the top and bottom 8-bits of @a x in
        # the order <i>(lo, hi)</i>
        sub x2d($)
        {
            $x = hex($_);
            return (($x >> 8) & 0xff, $x & 0xff);
        }
        $address = join(".", map {x2d($_)} split(/:/, $address));
    }

    return getIPAddress($address, 16, "\\.");
}

##@method public int* getMacAddress(char *address)
# Convert the string representation of a MAC address into two 32-bit words
# @param[in] address The MAC address to be parsed
# @return The parsed MAC address (<i> Since perl does not support 64-bit
# integers, the MAC address will be returned as two 32-bit integers in the
# order (hi, lo) </i>)
sub getMacAddress($)
{
    no warnings 'uninitialized';

    my ($address) = @_;

    my @bytes = split(/:/, $address);
    if (scalar(@bytes) != 6)
    {
        return (undef, undef);
    }
    my @data = ();
    for (my $i=0 ; $i<6 ; $i++)
    {
        $data[($i < 4 ? INDEX_MA_LO : INDEX_MA_HI)] +=
                                          (hex(pop(@bytes)) << (8 * ($i % 4)));
    }
    return @data;
}

##@method private char* setIPAddress(int[] address, int length, char *separator)
# Convert the integer representation of an IP address into a string
# @param[in] address A Perl reference to a Perl array containing the IP address
# to be parsed ordered as <tt>(LSW .. MSW)</tt>
# @param[in] length The number of bytes the specified IP address should consist
# of
# @param[in] separator The separator
# @return The string representation of the specified IP version address
sub setIPAddress($$$)
{
    my ($address, $length, $separator) = @_;

    my @bytes = ();
    for (my $i=0 ; $i<$length ; $i++)
    {
        my $byte = (($address->[floor($i / 4)] >> (8 * ($i % 4))) & 0xff);
        unshift(@bytes, sprintf("%d", $byte));
    }
    return join($separator, @bytes);
}

##@method public char* setIPv4Address(int address)
# Convert the integer representation of an IP version 4 address into a string
# @param[in] address The IP version 4 address to be parsed
# @return The string representation of the specified IP version 4 address
sub setIPv4Address($)
{
    my ($address) = @_;

    return setIPAddress([$address], 4, ".");
}

##@method public char* setIPv6Address(int address)
# Convert the integer representation of an IP version 6 address into a string
# @param[in] address A Perl reference to an array containing the IP version 6
# address to be parsed ordered as <tt>(LSW .. MSW)</tt>
# @return The string representation of the specified IP version 6 address
sub setIPv6Address($)
{
    my ($address) = @_;

    return setIPAddress($address, 16, ".");
}

##@method public char* setMacAddress(int mac_lo, int mac_hi)
# Convert the integer representation a MAC address into a string
# @param[in] mac_lo The low part of the MAC address
# @param[in] mac_hi The high part of the MAC address
# @return The string representation of the MAC address
sub setMacAddress(@)
{
    validatePrototype(@_, 2);
    my ($mac_lo, $mac_hi) = @_;

    my @bytes = ();
    for (my $i=0 ; $i<6 ; $i++)
    {
	    my $byte = ((($i < 4 ? $mac_lo : $mac_hi) >> (8 * ($i % 4))) & 0xff);
        unshift(@bytes, sprintf("%02X", $byte));
    }
    return join(":", @bytes);
}

1;
