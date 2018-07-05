# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Scripts/GenFrame/L3.pm
# Creation Date:    03/01/07
# Description:      Layer 3 frame generation support for Test Engine 2 tests
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

package Scripts::GenFrame::L3;
use strict;
use warnings;

require Exporter;
use Base::Const;
use Scripts::GenFrame::L2;
use Scripts::Utilities;
our @ISA = qw(Exporter);
our @EXPORT = qw(
    addL3header
    addL3Payload
    assembleIPV4Header
    assembleIPV6Header
    corruptL3Ver
    genIPheader
    getIP
    getIPv6LastNxtHdrIdx
    getIPv6OptionLength
    getL3Para
    getL3PayloadLen
    getL4Index
    oneComplementAdd
    randIp
    setIPLen
    setL3Para
    updateIPv4Chksum
    updateL3PayloadLen
    updateL3Protocol
);
our %EXPORT_TAGS = (
    CONSTANTS   => [qw(
        PROTOCOL_TCP
        PROTOCOL_UDP

        HEADER_HOP_BY_HOP
        HEADER_ROUTING
        HEADER_FRAGMENTATION
        HEADER_ESP
        HEADER_AUTHENTICATION
        HEADER_DESTINATION
    )]
);
Exporter::export_tags('CONSTANTS');

use constant PROTOCOL_TCP               => 6;
use constant PROTOCOL_UDP               => 17;

use constant HEADER_HOP_BY_HOP          => 0;
use constant HEADER_ROUTING             => 43;
use constant HEADER_FRAGMENTATION       => 44;
use constant HEADER_ESP                 => 50;
use constant HEADER_AUTHENTICATION      => 51;
use constant HEADER_DESTINATION         => 60;

##@method public char** addL3header(char* SIP, char *DIP, char **pDataArray)
# Adds a Layer 3 header to the frame and initializes the proper fields for
# either IP version 4 or IP version 6 depending on the form of @a SIP and @a
# DIP. If @a pDataArray contains a Layer 2 header, the IP header will be placed
# after the last VLAN tag. If however @a pDataArray does not contain a Layer 2
# header, an IP header will be placed at byte 12 and the first 12 bytes of the
# frame will be filled with zeros
# @param[in] SIP The Layer 3 source address
# @param[in] DIP The Layer 3 destination address
# @param[in,out] pDataArray A Perl reference to an array containing the packet
# data
# @note For IP version 4 @a SIP and @a DIP should be strings in the form @c
# "a.b.c.d", while for IP version 6 @a SIP and @a DIP should be in the form @c
# "a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p", where @c a -- @c p are decimal numbers
# between 0 and 255. Note that for #addL3header to modify @a pDataArray the
# form of @a SIP and @a DIP should match
# @return A Perl reference to an array containing the modified packet data
sub addL3header($$$)
{
    my ($SIP, $DIP, $pDataArray) = @_;

    my @sipArray = split(/\./, $SIP); 
    my @dipArray = split(/\./, $DIP); 

    # Check if SIP and DIP are correct
    my $error = FM_FALSE;
    for (my $i=0 ; $i<scalar(@sipArray) ; $i++)
    {
        if ($sipArray[$i]<0 || $sipArray[$i]>255)
        {
            $error = FM_TRUE;
        }
    }
    for (my $i=0 ; $i<scalar(@dipArray) ; $i++)
    {
        if ($dipArray[$i]<0 || $dipArray[$i]>255)
        {
            $error = FM_TRUE;
        }
    }
    if ((scalar(@sipArray) != scalar(@dipArray))
        || (scalar(@sipArray) != 4 && scalar(@sipArray) != 16)
        || (scalar(@dipArray) != 4 && scalar(@dipArray) != 16))
    {
        $error = FM_TRUE;
    }
    if ($error == FM_TRUE)
    {
        return $pDataArray;
    }

    # Determine at which byte the Layer 2 payload starts.
    my $l3Index = getL3Index($pDataArray);

    my $nxtHdr = 2;
    my $tos = 0;
    my $ttl = 15;
    
    my $pHdr;
    # IP version 4
    if (scalar(@sipArray) == 4)
    {
        my $hdrLen = 20 / 4;
        my $pktLen = $hdrLen * 4;
        my $id = 0;
        my $flags = 0;
        my $fragOffset = 0;

        # Write the Ethernet type to the Layer 2 length / type field.
        $pDataArray->[$l3Index - 2] = 0x8;
        $pDataArray->[$l3Index - 1] = 0x0;
        
        $pHdr = assembleIPV4Header($hdrLen, 
                                   $tos,
                                   $pktLen,
                                   $id,
                                   $flags,
                                   $fragOffset,
                                   $ttl,
                                   $nxtHdr,
                                   \@sipArray,
                                   \@dipArray);
    }
    elsif (scalar(@sipArray) == 16)
    {
        my $flowLabel = 0;
        my $payloadLen = 0;

        # Write the Ethernet type to the Layer 2 length / type field.
        $pDataArray->[$l3Index - 2] = 0x86;
        $pDataArray->[$l3Index - 1] = 0xdd;
        
        $pHdr = assembleIPV6Header($tos,
                                   $flowLabel,
                                   $payloadLen,
                                   $nxtHdr,
                                   $ttl,
                                   \@sipArray,
                                   \@dipArray);
    }

    # Append the Layer 3 header to the Layer 2 header.
    for (my $i=0; $i<scalar(@{$pHdr}); $i++)
    {
        $pDataArray->[$l3Index + $i] = $pHdr->[$i];
    }

    return $pDataArray;
}

##@method public char** addL3Payload(char **pPayloadArray, char **pDataArray)
# Adds a Layer 3 payload to the frame
# @param[in] pPayloadArray A Perl reference to an array containing the Layer 3
# payload
# @param[in,out] pDataArray A Perl reference to an array containing a Layer 3
# header
# @return A Perl reference to an array containing the modified Layer 3 header
sub addL3Payload($$)
{
    validatePrototype(@_, 2);
    my ($pPayloadArray, $pDataArray) = @_;

    # Remove the default UDP payload.
    my $l4Index = getL4Index($pDataArray);
    splice(@{$pDataArray}, $l4Index);

    my $length = scalar(@{$pPayloadArray});
    for (my $i=0 ; $i<$length ; $i++)
    {
        $pDataArray->[$l4Index + $i] = $pPayloadArray->[$i];
    }
    return $pDataArray;
}

##@method public char** assembleIPV4Header(int headerLength, int TOS, int totalLength, int id, int flags, int fragmentOffset, int TTL, int protocol, char **SIP, char **DIP)
# Assembles an IP version 4 header
# @param[in] headerLength The IP version 4 header length
# @param[in] TOS The type of service (TOS) field
# @param[in] totalLength The total length of the IP datagram in bytes
# @param[in] id The 16-bit identification field
# @param[in] flags
# @param[in] fragmentOffset
# @param[in] TTL
# @param[in] protocol
# @param[in] SIP
# @param[in] DIP
# @return A Perl reference to an array containg the IP version 4 header
sub assembleIPV4Header($$$$$$$$$$)
{
    my ($headerLength, $TOS, $totalLength, $id, $flags, $fragmentOffset, $TTL, $protocol, $SIP, $DIP) = @_;

    my $pHdrArray = [];
    my $pWordArray = [];

    $pWordArray->[0] = ((4 << 28)
                        | ($headerLength << 24)
                        | ($TOS << 16)
                        | $totalLength);
    $pWordArray->[1] = (($id << 16)
                        | ($flags << 13)
                        | ($fragmentOffset));
    $pWordArray->[2] = (($TTL << 24)
                        | ($protocol << 16));
    # Copy the words contained in the array pointed to by $pWordArray to the
    # byte array pointed to by $pHdrArray.
    for (my $i=0 ; $i<scalar(@{$pWordArray}) ; $i++)
    {
        for (my $j=0; $j<4; $j++)
        {
            $pHdrArray->[4 * $i + $j] = 
                                 (($pWordArray->[$i] >> (24 - 8 * $j)) & 0xff);
        }
    }
    foreach my $byte (@{$SIP})
    {
        push(@{$pHdrArray}, $byte);
    }
    foreach my $byte (@{$DIP})
    {
        push(@{$pHdrArray}, $byte);
    }
    # Compute the IP version 4 header checksum.
    my $checkSum = oneComplementAdd($pHdrArray);
    $pHdrArray->[10] = (($checkSum >> 8) & 0xff);
    $pHdrArray->[11] = ($checkSum & 0xff);
    return $pHdrArray;
}

##@method public char** assembleIPV6Header(int tc, int flowLabel, int payloadLength, int nextHeader, int hopLimit, int[] SIP, int[] DIP)
# Assemble an IP version 6 header
# @param[in] tc The 8-bit Traffic Class value
# @param[in] flowLabel The 20-bit Flow Label
# @param[in] payloadLength The 16-bit length of the IP version 6 payload
# @param[in] nextHeader The 8-bit Next Header value
# @param[in] hopLimit The 8-bit Hop Limit value
# @param[in] SIP A Perl reference to an 16-entry byte array containing the
# Layer 3 source address
# @param[in] DIP A Perl reference to an 16-entry byte array containing the
# Layer 3 destination address
# @return A Perl reference to an array containing an IP version 6 header
sub assembleIPV6Header($$$$$$$)
{
    my ($tc, $flowLabel, $payloadLength, $nextHeader, $hopLimit, $SIP, $DIP) = @_;

    my $pHdrArray = [];
    my $pWordArray = [];
    $pWordArray->[0] = ((6 << 28)
                        | (($tc & 0xff) << 24)
                        | ($flowLabel & 0xfffff));
    $pWordArray->[1] = ((($payloadLength & 0xffff) << 16)
                        | (($nextHeader & 0xff) << 8)
                        | ($hopLimit & 0xff));
    # Copy the words contained in the array pointed to by $pWordArray to the
    # byte array pointed to by $pHdrArray.
    for (my $i=0 ; $i<scalar(@{$pWordArray}) ; $i++)
    {
        for (my $j=0 ; $j<4 ; $j++)
        {
            $pHdrArray->[4 * $i + $j] = 
                                 (($pWordArray->[$i] >> (24 - 8 * $j)) & 0xff);
        }
    }
    foreach my $byte (@{$SIP})
    {
        push(@{$pHdrArray}, $byte);
    }
    foreach my $byte (@{$DIP})
    {
        push(@{$pHdrArray}, $byte);
    }
    return $pHdrArray;
}

##@method public char** corruptL4Ver(int version, char **pDataArray)
# Corrupts the Layer 3 version field
# @param[in] version The version value with which to corrupt the Layer 3
# version field
# @param[in,out] pDataArray A Perl reference to an array containing a Layer 3
# header
# @return A Perl reference to an array containing the modified Layer 3 header
sub corruptL3Ver($$)
{
    my ($version, $pDataArray) = @_;

    my $l3Index = getL3Index($pDataArray);
    $pDataArray->[$l3Index] = (($pDataArray->[$l3Index] & 0xf)
                               | (($version & 0xf) << 4));
    updateIPv4Chksum($pDataArray);
    return $pDataArray;
}

##@method public char** genIPheader(char* da, char *sa, char *SIP, char *DIP)
# Generates an IP version 4 or IP version 6 frame based on the format of the @a
# SIP and @a DIP arguments. If either @a SIP or @a DIP are not formatted as IP
# version 4 or IP version 6 addresses, a basic Layer 2 packet is returned
# @param[in] da Layer 2 destination address
# @param[in] sa Layer 2 source address
# @param[in] SIP Layer 3 source address (either IP version 4 or IP version 6)
# @param[in] DIP Layer 3 destination address (either IP version 4 or IP version
# 6)
# @return A Perl reference to an array containing a basic Layer 3 packet
sub genIPheader($$$$)
{
    my($da, $sa, $SIP, $DIP) = @_;

    my $pDataArray = genL2header($da, $sa);
    addL3header($SIP, $DIP, $pDataArray);
    # By default add a UDP header with all fields zeroed out as the Layer 3
    # payload. Note that since the UDP checksum is optional (see TCP/IP
    # Illustrated, Volume 1 pp. 144 -- 147), the UDP checksum will not be
    # computed.
    setL3Para(0, 15, PROTOCOL_UDP, $pDataArray);
    updateL3PayloadLen($pDataArray, 8);
    my $l4Index = getL4Index($pDataArray);
    for (my $i=0 ; $i<8 ; $i++)
    {
        $pDataArray->[$l4Index + $i] = 0x0;
    }
    $pDataArray->[$l4Index + 5] = 8;
    return $pDataArray;
}

##@method public int[] getIP(char *IP, int version)
# Convert the string representation of an IP address, e.g. "192.0.0.1" into its
# integer representation
# @param[in] IP The IP address in the form @c "a.b.c.d" for IP version 4
# addresses or @c "a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p" for IP version 6 addresses,
# where @c a -- @c p are decimal numbers between 0 and 255
# @param[in] version @optional
# @return A Perl array containing the integer representation of the specified
# IP address
sub getIP($;$)
{
    validatePrototype(@_, 1, 2);
    my ($IP, $version) = @_;

    $version = (defined($version) ? $version : 4);
  
    my @bytes = split(/\./, $IP); 
    my $length = scalar(@bytes);
    my @result = (0x0) x 4;
    for (my $i=0 ; $i<$length ; $i++)
    {
        $result[int($i / 4)] |= (pop(@bytes) << (8 * ($i % 4)));
    }
    return @result;
}

##@method public int[] getIPv6LastNxtHdrIdx(char** pDataArray, int ISLTag)
# Determine the index within @a pDataArray of the last Next Header field of an
# IP version 6 frame
# @param[in,out] pDataArray A Perl reference to an array containing an IP
# version 6 header
# @param[in] ISLTag @optional The ISL tag to be expected. See @link
# Scripts::GenFrame::L2::getL3Index() getL3Index @endlink for a list of valid
# choices. The default is @c Base::Enum::ISL_NONE()
# @return A Perl array containing the index of the last Next Header field and
# the offset of the Layer 3 payload with respect to this last Next Header field
# in the order <tt>(nxtHdrIdx, payloadOffset)</tt>
sub getIPv6LastNxtHdrIdx($;$)
{
    my($pDataArray, $ISLTag) = @_;

    my $l3Index = getL3Index($pDataArray, $ISLTag);
    my $nxtHdrIdx = $l3Index + 6;
    my $payloadOffset = 34;
    # index is the start index of the current Next Header field
    my $index = $nxtHdrIdx + $payloadOffset;
    while ($index < scalar(@{$pDataArray}))
    {
        my $headerLength = getIPv6OptionLength($pDataArray->[$nxtHdrIdx],
                                               $index, $pDataArray);
        if ($headerLength == -1)
        {
            last;
        }
        $nxtHdrIdx = $index;
        $payloadOffset = $headerLength;
        $index += $headerLength;
    }
    return ($nxtHdrIdx, $payloadOffset);
}

##@method public int getIPv6OptionLength(int nextHeader, int optionIndex, char **pDataArray)
# Determine the length of an IP version 6 extension header
# @param[in] nextHeader The Next Header value of the IP version 6 extension
# header whose length is being determined
# @param[in] optionIndex The index within @a pDataArray at which the IP version
# 6 extension header, whose length is being determined, starts
# @param[in,out] pDataArray A Perl reference to an array containing an IP
# version 6 header
# @return The length of the specified IP version 6 extension header, or -1 if
# @a nextHeader does not contain a Next Header value corresponding with a known
# IP version 6 extension header
sub getIPv6OptionLength($$$)
{
    my ($nextHeader, $optionIndex, $pDataArray) = @_;

    my $headerLength;
    # Authentication Header
    if ($nextHeader == HEADER_AUTHENTICATION)
    {
        $headerLength = 4 * $pDataArray->[$optionIndex + 1] + 8;
    }
    # Fragmentation Header, Encapsulating Security Payload Header
    elsif ($nextHeader == HEADER_FRAGMENTATION
           || $nextHeader == HEADER_ESP) 
    {
        $headerLength = 8;
    }
    # Hop-by-Hop Options Header, Routing Header, Destination Options Header
    elsif ($nextHeader == HEADER_HOP_BY_HOP
           || $nextHeader == HEADER_ROUTING
           || $nextHeader == HEADER_DESTINATION)
    {
        $headerLength = 8 * $pDataArray->[$optionIndex + 1] + 8;
    }
    else
    {
        $headerLength = -1;
    }
    return $headerLength;
}

##@method public list getL3Para(char* pDataArray, int ISLTag)
# @param[in,out] pDataArray The packet data to analyze
# @param[in] ISLTag @optional The ISL tag to be expected. See @link
# Scripts::GenFrame::L2::getL3Index() getL3Index @endlink for a list of valid
# choices. The default is @c Base::Enum::ISL_NONE()
# @return Depending on the IP version, the following fields are returned:
# <ol>
#   <li value="4">  (TOS, TTL, protocol)
#   <li value="6">  (priority, hop limit, next header)
#   <li value="0">  (undef, undef, undef) in all other cases
# </ol>
# Note that in the case of an IP version 6 packet the first Next Header field
# will be returned
sub getL3Para($;$)
{
    my ($pDataArray, $ISLTag) = @_;

    my ($field0, $field1, $field2) = (undef, undef, undef);

    my $l3Index = getL3Index($pDataArray, $ISLTag);
    my $ipVersion = (($pDataArray->[$l3Index] >> 4) & 0xf);
    if ($ipVersion == 4)
    {
        $field0 = ($pDataArray->[$l3Index + 1] & 0xff);
        $field1 = ($pDataArray->[$l3Index + 8] & 0xff);
        $field2 = ($pDataArray->[$l3Index + 9] & 0xff);
    }
    elsif ($ipVersion == 6)
    {
        $field0 = ($pDataArray->[$l3Index] & 0xf);
        $field1 = ($pDataArray->[$l3Index + 6] & 0xff);
        $field2 = ($pDataArray->[$l3Index + 7] & 0xff);
    }
    return ($field0, $field1, $field2);
}

##@method public int getL3PayloadLen(char **pDataArray, int ISLTag)
# Retrieve the Layer 3 payload length
# @param[in] pDataArray A Perl reference to an array containing packet data
# @param[in] ISLTag @optional The ISL tag to be expected. See @link
# Scripts::GenFrame::L2::getL3Index() getL3Index @endlink for a list of valid
# choices. The default is @c Base::Enum::ISL_NONE()
# @return On success the Layer 3 payload length, -1 otherwise
sub getL3PayloadLen($;$)
{
    my ($pDataArray, $ISLTag) = @_;

    my $payloadLength = -1;

    my $l3Index = getL3Index($pDataArray, $ISLTag);
    my $ipVersion = (($pDataArray->[$l3Index] >> 4) & 0xf);
    if ($ipVersion == 4)
    {
        $payloadLength = (($pDataArray->[$l3Index + 2] << 8)
                          | $pDataArray->[$l3Index + 3]);
        $payloadLength -= 4 * ($pDataArray->[$l3Index] & 0xf);
    }
    elsif ($ipVersion == 6)
    {
        $payloadLength = (($pDataArray->[$l3Index + 4] << 8)
                          | $pDataArray->[$l3Index + 5]);
    }
    return $payloadLength;
}

##@method public int getL4Index(char** pDataArray, int ISLTag)
# Retrieve the starting index of Layer 4 data
# @param[in] pDataArray A Perl reference to an array containing packet data
# @param[in] ISLTag @optional The ISL tag to be expected. See @link
# Scripts::GenFrame::L2::getL3Index() getL3Index @endlink for a list of valid
# choices. The default is @c Base::Enum::ISL_NONE()
# @return Upon success the starting index of the Layer 4 data, -1 otherwise
sub getL4Index($;$)
{
    my ($pDataArray, $ISLTag) = @_;

    my $l3Index = getL3Index($pDataArray, $ISLTag);
    my $ipVersion = (($pDataArray->[$l3Index] >> 4) & 0xf);
    if ($ipVersion == 4)
    {
        return $l3Index + 4 * ($pDataArray->[$l3Index] & 0xf);
    }
    elsif ($ipVersion == 6)
    {
        my ($nxtHdrIdx, $payloadOffset) = getIPv6LastNxtHdrIdx($pDataArray);
        return $nxtHdrIdx + $payloadOffset;
    }
    else
    {
        return -1;
    }
}

##@method public int16_t oneComplementAdd(char** pHdrArray)
# Compute the 16-bit one's complement of the one's complement sum of the header
# pointed to by @a pHdrArray
# @param[in] pHdrArray A Perl reference to a header array
# @return The 16-bit one's complement of the one's complement sum of the
# specified header
sub oneComplementAdd($)
{
    my ($pHdrArray) = @_;

    my $sum = 0;
    for (my $i=0 ; $i<scalar(@{$pHdrArray}) ; $i+=2)
    {
        $sum += (($pHdrArray->[$i] << 8) | $pHdrArray->[$i + 1]);
        $sum = ($sum & 0xffff) + (($sum >> 16) & 0xffff);
    }
    return ((~$sum) & 0xffff);
}

##@method public char* randIp(bool isIPv6)
# Generate a random Layer 3 address
# @param[in] isIPv6 @optional A boolean indicating whether to generate an IP
# version 4 address (@a isIPv6 equals @c FM_FALSE) or an IP version 6
# address (@a isIPv6 equals @c FM_TRUE). Default is @c FM_FALSE
# @return A random Layer 3 address in the form @c "a.b.c.d" for IP version 4
# addresses or @c "a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p" for IP version 6 addresses,
# where @c a -- @c p are decimal numbers between 0 and 255
sub randIp(;$)
{
    my ($isIPv6) = @_;

    my $length = (defined($isIPv6) && $isIPv6 ? 16 : 4);
    my $address = sprintf("%d", int(rand(256)) & 0xff);
    for (my $i=0 ; $i<($length - 1) ; $i++)
    {
        $address = sprintf("%s\.%d", $address, int(rand(256)) & 0xff);
    }
    return $address;
}

##@method public char** setIPLen(int packetLength, char **pDataArray)
# Set the IP version 4 packet length field or the IP version 6 payload length
# field
# @param[in] packetLength The new packet length
# @param[in,out] pDataArray A Perl reference to an array containing a Layer 3
# header
# @return A Perl reference to an array containing the modified Layer 3 header
sub setIPLen($$)
{
    validatePrototype(@_, 2);
    my ($packetLength, $pDataArray) = @_;

    my $l3Index = getL3Index($pDataArray);
    my $ipVersion = (($pDataArray->[$l3Index] >> 4) & 0xf);
    if ($ipVersion == 4)
    {
        $pDataArray->[$l3Index + 2] = (($packetLength >> 8) & 0xff);
        $pDataArray->[$l3Index + 3] = ($packetLength & 0xff);
        updateIPv4Chksum($pDataArray);
    }
    elsif ($ipVersion == 6)
    {
        $pDataArray->[$l3Index + 4] = (($packetLength >> 8) & 0xff);
        $pDataArray->[$l3Index + 5] = ($packetLength & 0xff);
        updateIPv4Chksum($pDataArray);
    }
    return $pDataArray;
}

##@method public char** setL3Para(int tos, int ttl, int protocol, char **pDataArray, int ISLTag)
# If the packet pointed to by @a pDataArray is either an IP version 4 or IP
# version 6 frame, the packet is updated, otherwise it is returned unaltered
# @param[in] tos The TOS field for IPv4 or the Traffic Class field for IPv6.
# Set to undef to leave the TOS or Traffic Class field unchanged
# @param[in] ttl The time-to-live (TTL) field for IPv4 or the Hop Limit field
# for IPv6. Set to undef to leave the time-to-live or Hop Limit field unchanged
# @param[in] protocol The protocol field for IPv4 or the Next Header field for
# IPv6. Set to undef to leave the protocol or Next Header field unchanged
# @param[in,out] pDataArray A Perl reference to an array containing the packet
# data
# @param[in] ISLTag @optional The ISL tag to be expected. See @link
# Scripts::GenFrame::L2::getL3Index() getL3Index @endlink for a list of valid
# choices. The default is @c Base::Enum::ISL_NONE()
# @return A Perl reference to an array containing the altered packet data
sub setL3Para($$$$;$)
{
    my ($tos, $ttl, $protocol, $pDataArray, $ISLTag) = @_;

    my $l3Index = getL3Index($pDataArray, $ISLTag);

    my $ipVersion = (($pDataArray->[$l3Index] >> 4) & 0xf);
    if ($ipVersion == 4)
    {
        if (defined($tos))
        {
            $pDataArray->[$l3Index + 1] = $tos;
        }
        if (defined($ttl))
        {
            $pDataArray->[$l3Index + 8] = $ttl;
        }
        if (defined($protocol))
        {
            $pDataArray->[$l3Index + 9] = $protocol;
        }
        updateIPv4Chksum($pDataArray, $ISLTag)
    }
    elsif ($ipVersion == 6)
    {
        if (defined($tos))
        {
            $pDataArray->[$l3Index] = (($pDataArray->[$l3Index] & 0xf0)
                                       |  (($tos >> 4) & 0xf));
            $pDataArray->[$l3Index + 1] = (($pDataArray->[$l3Index + 1] & 0x0f)
                                           | (($tos & 0xf) << 4));
        }
        if (defined($protocol))
        {
            $pDataArray->[$l3Index + 6] = $protocol;
        }
        if (defined($ttl))
        {
            $pDataArray->[$l3Index + 7] = $ttl;
        }
    }
    return $pDataArray;
}

##@method public char** updateIPv4Chksum(char ** pDataArray, int ISLTag)
# Update the IP version 4 header checksum field
# @param[in,out] pDataArray A Perl reference to an array containing a Layer 3
# header
# @param[in] ISLTag @optional The ISL tag to be expected. See @link
# Scripts::GenFrame::L2::getL3Index() getL3Index @endlink for a list of valid
# choices. The default is @c Base::Enum::ISL_NONE()
# @return A Perl reference to an array containing the modified Layer 3 header
sub updateIPv4Chksum($;$)
{
    my ($pDataArray, $ISLTag) = @_;

    my $l3Index = getL3Index($pDataArray, $ISLTag);

    my $ipVersion = (($pDataArray->[$l3Index] >> 4) & 0xf);
    if ($ipVersion != 4)
    {
        return $pDataArray;
    }
    # Zero out the checksum field before computing the IP version 4 header
    # checksum.
    $pDataArray->[$l3Index + 10] = 0;
    $pDataArray->[$l3Index + 11] = 0;
    # Copy the IP version 4 header to a newly created array and compute the
    # checksum of this array.
    my $headerLength = 4 * ($pDataArray->[$l3Index] & 0xf);
    my $pHdrArray = [];
    for (my $i=0 ; $i<$headerLength ; $i++)
    {
        $pHdrArray->[$i] = $pDataArray->[$l3Index + $i];
    }
    my $checkSum = oneComplementAdd($pHdrArray);
    # Update the IP version 4 header checksum field with the newly computed
    # checksum.
    $pDataArray->[$l3Index + 10] = (($checkSum >> 8) & 0xff);
    $pDataArray->[$l3Index + 11] = ($checkSum & 0xff);
    return $pDataArray;
}

##@method public char** updateL3PayloadLen(char** pDataArray, int payloadLength, int ISLTag)
# Update the IP version 4 total length field or the IP version 6 Payload Length
# field
# @param[in,out] pDataArray A Perl reference to an array containing a Layer 3
# header
# @param[in] payloadLength The new payload length
# @param[in] ISLTag @optional The ISL tag to be expected. See @link
# Scripts::GenFrame::L2::getL3Index() getL3Index @endlink for a list of valid
# choices. The default is @c Base::Enum::ISL_NONE()
# @return A Perl reference to an array containing the modified Layer 3 header
sub updateL3PayloadLen($$;$)
{
    validatePrototype(@_, 2, 3);
    my ($pDataArray, $payloadLength, $ISLTag) = @_;

    my $l3Index = getL3Index($pDataArray, $ISLTag);
    my $ipVersion = (($pDataArray->[$l3Index] >> 4) & 0xf);
    if ($ipVersion == 4)
    {
        my $length = 4 * ($pDataArray->[$l3Index] & 0xf) + $payloadLength;
        $pDataArray->[$l3Index + 2] = (($length >> 8) & 0xff);
        $pDataArray->[$l3Index + 3] = ($length & 0xff);
        updateIPv4Chksum($pDataArray, $ISLTag);
    }
    elsif ($ipVersion == 6)
    {
        $pDataArray->[$l3Index + 4] = (($payloadLength >> 8) & 0xff);
        $pDataArray->[$l3Index + 5] = ($payloadLength & 0xff);
    }
    return $pDataArray;
}

##@method public char** updateL3Protocol(char** pDataArray, int protocol)
# Update either the IP version 4 protocol field or the IP version 6 Next Header
# field 
# @param[in,out] pDataArray A Perl reference to an array containing a Layer 3
# header
# @param[in] protocol The new protocol
# @return A Perl reference to an array containing the modified Layer 3 header
sub updateL3Protocol($$)
{
    my ($pDataArray, $protocol) = @_;

    my $l3Index = getL3Index($pDataArray);
    my $ipVersion = (($pDataArray->[$l3Index] >> 4) & 0xf);
    if ($ipVersion == 4)
    {
        $pDataArray->[$l3Index + 9] = $protocol;
        updateIPv4Chksum($pDataArray);
    }
    elsif ($ipVersion == 6)
    {
         my ($nxtHdrIdx) = getIPv6LastNxtHdrIdx($pDataArray);
         $pDataArray->[$nxtHdrIdx] = $protocol;
    }
    return $pDataArray;
}

1;
