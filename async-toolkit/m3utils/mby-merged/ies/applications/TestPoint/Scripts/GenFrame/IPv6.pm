# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Scripts/GenFrame/IPv6.pm
# Creation Date:    03/01/07
# Description:      IP version 6 frame generation support for Test Engine 2
#                   tests
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

package Scripts::GenFrame::IPv6;
use strict;
use warnings;

require Exporter;
use POSIX qw(floor);
use Scripts::GenFrame::L2;
use Scripts::GenFrame::L3;
use Scripts::GenFrame::Utilities;
use Scripts::Utilities;
our @ISA = qw(Exporter);
our @EXPORT = qw(
    addIPv6Option
    genICMPv6Header
    genIPv6Option
    genIPv6PseudoHeader
    genMLDv2Frame
    setIPv6Para
    updateICMPv6Chksum
);
our %EXPORT_TAGS = (
    CONSTANTS   => [qw(
        PROTOCOL_ICMPv6

        MLD_QUERY
        MLD_REPORT
        MLD_DONE
    )]
);
Exporter::export_tags('CONSTANTS');

use constant PROTOCOL_ICMPv6    => 58;

use constant MLD_QUERY          => 130;
use constant MLD_REPORT         => 131;
use constant MLD_DONE           => 132;

##@method public char** addIPv6Option(int nextHeader, char **option, char **pDataArray)
# Adds a new option to an existing IP version 6 header
# @param[in] nextHeader The Next Header value for the option to be added (the
# Next Header field of either the previous option or the main IP version 6
# header will be modified using this value)
# @param[in] option A Perl reference to an array containing an IP version 6
# option
# @param[in,out] pDataArray A Perl reference to an array containing an IP
# version 6 header
# @return A Perl reference to an array containing the modified IP version 6
# header
sub addIPv6Option
{
    my ($nextHeader, $option, $pDataArray) = @_;

    my $l3Index = getL3Index($pDataArray);
    my $ipVersion = (($pDataArray->[$l3Index] >> 4) & 0xf);
    if ($ipVersion != 6)
    {
        return $pDataArray;
    }
    my ($nxtHdrIdx, $payloadOffset) = getIPv6LastNxtHdrIdx($pDataArray);
    my $length = scalar(@{$pDataArray});
    my $optionLength = scalar(@{$option});
    my $headerLength = $length + $optionLength;
    my $optionOffset = $nxtHdrIdx + $payloadOffset;
    # Shift the payload to allow the specified option header to be inserted.
    my $payloadLength = $length - $optionOffset;
    for (my $i=$payloadLength ; $i>0 ; $i--)
    {
        $pDataArray->[$headerLength - $i] = $pDataArray->[$length - $i];
    }
    # Insert the option header into the IP version 6 header.
    my $protocol = $pDataArray->[$nxtHdrIdx];
    $pDataArray->[$nxtHdrIdx] = $nextHeader;
    $pDataArray->[$optionOffset] = $protocol;
    for (my $i=1 ; $i<$optionLength ; $i++)
    {
        $pDataArray->[$optionOffset + $i] = $option->[$i];
    }
    # Update the payload length.
    updateL3PayloadLen($pDataArray,
                       getL3PayloadLen($pDataArray) + $optionLength);
    return $pDataArray;
}

##@method public char** genICMPv6Header(char *da, char *sa, char *SIP, char *DIP, int messageType)
# Generate an ICMP for IP version 6 header. The ICMPv6 message body of the
# generated header is empty
# @param[in] da The Layer 2 destination address
# @param[in] sa The Layer 2 source address
# @param[in] SIP The IP version 6 source address
# @param[in] DIP The IP version 6 destination address
# @param[in] messageType The 16-bit ICMPv6 message type consisting of the 8-bit
# type field (bits 0..7) and the 8-bit code field (bits 8..15)
# @return A Perl reference to an array containing the ICMPv6 header
sub genICMPv6Header($$$$$)
{
    my ($da, $sa, $SIP, $DIP, $messageType) = @_;

    my $pDataArray = genIPheader($da, $sa, $SIP, $DIP);

    # Ensure that the Layer 3 header is an IP version 6 header.
    my $l3Index = getL3Index($pDataArray);
    my $ipVersion = (($pDataArray->[$l3Index] >> 4) & 0xf);
    if ($ipVersion != 6)
    {
        traceback("Warning: Either SIP or DIP is not an IP version 6 address");
        return undef;
    }

    # Remove the default UDP payload.
    my $l4Index = getL4Index($pDataArray);
    splice(@{$pDataArray}, $l4Index);

    # Create an ICMPv6 header.
    my $pICMPv6Array = [(0x0) x 8];
    $pICMPv6Array->[0] = ($messageType & 0xff);
    $pICMPv6Array->[1] = (($messageType >> 8) & 0xff);

    # Add the ICMPv6 header to the IP version 6 header.
    updateL3Protocol($pDataArray, PROTOCOL_ICMPv6);
    my $messageLength = scalar(@{$pICMPv6Array});
    updateL3PayloadLen($pDataArray, $messageLength);
    for (my $i=0 ; $i<$messageLength ; $i++)
    {
        $pDataArray->[$l4Index + $i] = $pICMPv6Array->[$i];
    }
    updateICMPv6Chksum($pDataArray);
    return $pDataArray;
}

##@method public char** genIPv6Option(int nextHeader, int fragOffset, int length)
# Generates a new basic extension header for an existing IP version 6 header.
# Note that since Bali only parses the Next Header fields to determine what
# type of extension headers an IP version 6 frame contains, #genIPv6Option does
# not generate fully populated extension headers
# @param[in] nextHeader The Next Header value identifying the newly generated
# extension header. Valid values are:
# <ol>
#   <li value="0"> Hop-by-Hop options header
#   <li value="43"> %Routing header
#   <li value="44"> Fragment header
#   <li value="60"> Destination options header
#   <li value="51"> Authentication header
#   <li value="50"> Encapsulating Security Payload header
# </ol>
# @param[in] fragOffset The offset value for the fragment option
# @param[in] length The option length
# @return A Perl reference to an array containing an IP version 6 option
sub genIPv6Option($;$$)
{
    my ($nextHeader, $fragOffset, $length) = @_;

    my $option = [];
    $option->[0] = $nextHeader;
    $option->[2] = 0;
    $option->[3] = 0;
    # Authentication header
    if ($nextHeader == HEADER_AUTHENTICATION)
    {
        $option->[1] = ($length - 8) / 4;
    }
    # Fragmentation header
    elsif ($nextHeader == HEADER_FRAGMENTATION)
    {
        $option->[1] = 0;
        $length = 8;
        $option->[2] = (($fragOffset >> 5) & 0xff);
        $option->[3] = (($fragOffset & 0x1f)<<3);
    }
    elsif ($nextHeader == HEADER_ESP)
    {
        $length = 8;
    }
    # Hop-by-Hop Options Header, Routing Header, Destination Options Header
    elsif ($nextHeader == HEADER_HOP_BY_HOP
           || $nextHeader == HEADER_ROUTING
           || $nextHeader == HEADER_DESTINATION)
    { 
        $option->[1] = ($length - 8) / 8;
    }
    else
    {
        traceback(sprintf("Warning: %d: Unhandled IP version 6 option",
                          $nextHeader));
        return undef;
    }
    for (my $i=4 ; $i<$length ; $i++)
    {
        $option->[$i] = ($i & 0xff);
    }
    return $option;
}

##@method public char** genIPv6PseudoHeader(char **pDataArray, int nextHeader, int length, int ISLTag)
# Generate an IP version 6 pseudo header for the specified Layer 3 header which
# can be used to update the upper-layer header checksum
# @param[in] pDataArray A Perl reference to an array containing a Layer 3
# header
# @param[in] nextHeader The upper-layer protocol
# @param[in] length The upper-layer packet length. In most cases (e.g. ICMPv6
# and TCP) this is the length of the upper-layer header and data, while in some
# cases (e.g. UDP) it is the length carried in the upper-layer header
# @param[in] ISLTag @optional The ISL tag to be expected. See @link
# Scripts::GenFrame::L2::getL3Index() getL3Index @endlink for a list of valid
# choices. The default is @c Base::Enum::ISL_NONE()
# @return A Perl reference to an array containing the 40-byte IP version 6
# pseudo header
sub genIPv6PseudoHeader($$$;$)
{
    my ($pDataArray, $nextHeader, $length, $ISLTag) = @_;

    # Copy the IP version 6 source and destination address to the pseudo
    # header.
    my $l3Index = getL3Index($pDataArray, $ISLTag);
    my $pPseudoHeader = [];
    for (my $i=0 ; $i<32 ; $i++)
    {
        $pPseudoHeader->[$i] = $pDataArray->[$l3Index + 8 + $i];
    }
    for (my $i=0 ; $i<4 ; $i++)
    {
        $pPseudoHeader->[32 + $i] = (($length >> (24 - 8 * $i)) & 0xff);
    }
    for (my $i=0 ; $i<3 ; $i++)
    {
        $pPseudoHeader->[36 + $i] = 0x0;
    }
    $pPseudoHeader->[39] = $nextHeader;
    return $pPseudoHeader;
}

##@method public char** genMLDv2Frame(char *da, char *sa, char *SIP, char *DIP, int type, char *mcastAddress, int delay)
# Generate a multicast listener discovery (MLD) version 2 frame
# @param[in] da The Layer 2 destination address
# @param[in] sa The Layer 2 source address
# @param[in] SIP The IP version 6 source address
# @param[in] DIP The IP version 6 destination address
# @param[in] type The MLD message type. Valid values are:
# <ul>
#   <li> @c GenFrame::MLD_QUERY : Multicast listener query message
#   <li> @c GenFrame::MLD_REPORT : Multicast listener report message
#   <li> @c GenFrame::MLD_DONE : Multicast lister done message
# </ul>
# @param[in] mcastAddress The IP version 6 multicast address the sender is
# interested in
# @param[in] delay @optional The 16-bit maximum response delay. This field is
# only meaningful in multicast listener query messages
# @return A Perl reference to a Perl array containing a MLDv2 frame
sub genMLDv2Frame($$$$$$;$)
{
    validatePrototype(@_, 6, 7);
    my ($da, $sa, $SIP, $DIP, $type, $mcastAddress, $delay) = @_;

    my $types = {
        MLD_QUERY()     => undef,
        MLD_REPORT()    => undef,
        MLD_DONE()      => undef,
    };
    if (!exists($types->{+$type}))
    {
        return undef;
    }
    my $pDataArray = genICMPv6Header($da, $sa, $SIP, $DIP, $type);

    # Generate the MLDv2 message body.
    my $pMLDv2Array = [];
    if ($type == MLD_QUERY)
    {
        $pMLDv2Array->[0] = (($delay >> 8) & 0xff);
        $pMLDv2Array->[1] = ($delay & 0xff);
        $pMLDv2Array->[2] = 0x0;
        $pMLDv2Array->[3] = 0x0;
    }
    else
    {
        for (my $i=0 ; $i<4 ; $i++)
        {
            $pMLDv2Array->[$i] = 0x0;
        }
    }
    my @address = @{getIPv6Address($mcastAddress)};
    for (my $i=0 ; $i<16 ; $i++)
    {
        $pMLDv2Array->[19 - $i] =
                          (($address[floor($i / 4)] >> (8 * ($i % 4))) & 0xff);
    }

    # Add the MLDv2 message body to the ICMPv6 header.
    my $l4Index = getL4Index($pDataArray);
    my $bodyLength = scalar(@{$pMLDv2Array});
    updateL3PayloadLen($pDataArray, $bodyLength + 4);
    for (my $i=0 ; $i<$bodyLength ; $i++)
    {
        $pDataArray->[$l4Index + 4 + $i] = $pMLDv2Array->[$i];
    }
    updateICMPv6Chksum($pDataArray);

    # Add 4 bytes to the frame to allow the frame check sequence to be added to
    # the frame.
    push(@{$pDataArray}, (0x0) x 4);

    # Update the Layer 2 checksum.
    updateL2Chksum($pDataArray);
    return $pDataArray;
}

##@method public char** setIPv6Para(int flowLabel, char **pDataArray)
# Update several IP version 6 parameters
# @param[in] flowLabel flowLabel  (20bit)
# @param[in,out] pDataArray A Perl reference to a Perl array containing an IP
# version 6 header
# @return A Perl reference to an array containing the modified IPv6 header
# @note For now only the flowLabel field is supported
sub setIPv6Para($$)
{
    my ($flowLabel, $pDataArray) = @_;

    my $l3Index=getL3Index($pDataArray);
    $pDataArray->[$l3Index + 1]= (($pDataArray->[$l3Index + 1] & 0xf0)
                                  | (($flowLabel >> 16) & 0xf));
    $pDataArray->[$l3Index + 2] = (($flowLabel >> 8) & 0xff);
    $pDataArray->[$l3Index + 3] = ($flowLabel & 0xff);
    return $pDataArray;
}

##@method public char** updateICMPv6Chksum(char **pDataArray, int ISLTag)
# Update the ICMP for IP version 6 header checksum field
# @param[in,out] pDataArray A Perl reference to an array containing an ICMPv6
# header
# @param[in] ISLTag @optional The ISL tag to be expected. See @link
# Scripts::GenFrame::L2::getL3Index() getL3Index @endlink for a list of valid
# choices. The default is @c Base::Enum::ISL_NONE()
# @return A Perl reference to an array containing the modified ICMPv6 header
sub updateICMPv6Chksum($;$)
{
    my ($pDataArray, $ISLTag) = @_;

    my ($nxtHdrIdx, $payloadOffset) = getIPv6LastNxtHdrIdx($pDataArray);
    $payloadOffset += $nxtHdrIdx;
    my $nextHeader = $pDataArray->[$nxtHdrIdx];
    my $length = scalar(@{$pDataArray});
    my $messageLength = $length - $payloadOffset;
    my $pICMPv6Array = genIPv6PseudoHeader($pDataArray, PROTOCOL_ICMPv6,
                                           $messageLength, $ISLTag);
    for (my $i=0 ; $i<$messageLength ; $i++)
    {
        $pICMPv6Array->[40 + $i] = $pDataArray->[$payloadOffset + $i];
    }
    # Set the checksum field to zero.
    $pICMPv6Array->[42] = 0x0;
    $pICMPv6Array->[43] = 0x0;
    # Calculate the message checksum.
    my $checksum = oneComplementAdd($pICMPv6Array);
    $pDataArray->[$payloadOffset + 2] = (($checksum >> 8) & 0xff);
    $pDataArray->[$payloadOffset + 3] = ($checksum & 0xff);
    return $pDataArray;
}

1;
