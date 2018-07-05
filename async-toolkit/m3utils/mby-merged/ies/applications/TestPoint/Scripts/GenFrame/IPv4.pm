# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Scripts/GenFrame/IPv4.pm
# Creation Date:    03/01/07
# Description:      IP version 4 frame generation support for Test Engine 2
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

package Scripts::GenFrame::IPv4;
use strict;
use warnings;

require Exporter;
use POSIX qw(ceil);
use Scripts::GenFrame::L2;
use Scripts::GenFrame::L3;
use Scripts::GenFrame::Utilities;
use Scripts::Utilities;
our @ISA = qw(Exporter);
our @EXPORT = qw(
    addIPv4Option
    corruptIPv4Chksum
    genARPFrame
    genICMPv4Header
    genIGMPv3Frame
    genIPv4PseudoHeader
    setIPv4Frag
    shortenIPv4Hdr
);
our @EXPORT_FAIL = qw(
    updateIPv4HdrLen
);
our %EXPORT_TAGS = (
    CONSTANTS   => [qw(
        PROTOCOL_ICMP
        PROTOCOL_IGMP
    )]
);
Exporter::export_tags('CONSTANTS');

use constant PROTOCOL_ICMP      => 1;
use constant PROTOCOL_IGMP      => 2;

##@method public char** addIPv4Option(char** option, char **pDataArray)
# Adds a new option to an existing IP version 4 header
# @param[in] option A Perl reference to an array containing an IP version 4
# option
# @param[in,out] pDataArray A Perl reference to an array containing an IP
# version 4 header
# @return A Perl reference to an array containing the modified IP version 4
# header
sub addIPv4Option($$)
{
    my ($option, $pDataArray) = @_;

    my $l3Index = getL3Index($pDataArray);
    # Perform an IP version check.
    my $ipVersion = (($pDataArray->[$l3Index] >> 4) & 0xF);
    if ($ipVersion != 4)
    {
        return $pDataArray;
    }
    my $headerLength = 4 * ($pDataArray->[$l3Index] & 0xf);
    # Ensure that the options field always ends on a 32-bit boundary.
    my $optionLength = 4 * ceil(scalar(@{$option}) / 4);
    if (($headerLength + $optionLength) > 60)
    {
        return $pDataArray;
    }
    my @array = splice(@{$pDataArray}, $l3Index + $headerLength);
    push(@{$pDataArray}, @{$option});
    # Pad the options field with zeros if necessary.
    push(@{$pDataArray}, (0x0) x ($optionLength - scalar(@{$option})));
    push(@{$pDataArray}, @array);
    $headerLength = ($headerLength + $optionLength) / 4;
    updateIPv4HdrLen($headerLength, $pDataArray);
    return $pDataArray;
}

##@method public char** corruptIPv4Chksum(char** pDataArray)
# Corrupts the IP version 4 checksum of an existing Layer 3 header
# @param[in,out] pDataArray A Perl reference to an array containing a Layer 3
# header
# @return A Perl reference to an array containing the modified Layer 3 header
sub corruptIPv4Chksum($)
{
    my ($pDataArray) = @_;

    my $l3Index = getL3Index($pDataArray);
    # Perform an IP version check.
    my $ipVersion = (($pDataArray->[$l3Index] >> 4) & 0xF);
    if ($ipVersion != 4)
    {
        return $pDataArray;
    }
    $pDataArray->[$l3Index + 10] = (($pDataArray->[$l3Index + 10] + 1) & 0xff);
    $pDataArray->[$l3Index + 11] = (($pDataArray->[$l3Index + 11] + 1) & 0xff);
    return $pDataArray;
}

## @method byte[] genARPFrame(struct parameters)
# Generate an arp request (or gratuitous request) frame
# @param[in] parameters A hash of parameters used to construct the ARP frame
# <ol>
#   <li> smac (src mac in "00:12:34:56:78:9a" format, MUST BE DEFINED)</li>
#   <li> dmac (dest mac, defaults to "FF:FF:FF:FF:FF:FF")</li>
#   <li> vlan (set to undef or -1 for untagged)</li>
#   <li> sip (src ip string, eg: "192.168.0.1")</li>
#   <li> dutmac ( set to undef or "ff:ff:ff:ff:ff:ff" for gratuitous arp )</li>
#   <li> dutip  ( set to undef or sip for gratuitous arp ) </li>
# </ol>
sub genARPFrame($)
{
    my ($parameters) = @_;
   
    my $frame = [];

    if(defined $parameters->{dmac}) 
    {
        $frame = GenFrame::setL2da($parameters->{dmac}, $frame);
    }
    else
    {
        $frame = GenFrame::setL2da("FF:FF:FF:FF:FF:FF", $frame);
    }

    
    if(defined $parameters->{smac})
    {
        $frame = GenFrame::setL2sa($parameters->{smac}, $frame); 
    }
    else
    {
        printf("ERROR: GenFrame::genARPFrame hash argument must define smac\n");
        return -1;
    }

    $frame = GenFrame::setL2type(0x0806, $frame);            # ARP Type

    if(defined $parameters->{vlan} && $parameters->{vlan} != -1)
    {
        $frame = GenFrame::addAlternateVLANheader(
                                0x8100,
                                $parameters->{vlan},
                                0, 0,       #Vpri and CFI
                                $frame);
    }

    # hardware type ethernet
    foreach my $byte ( 0x00, 0x01 ) {
        push( @{$frame}, $byte);
    }
    
    # Protocol IP 
    foreach my $byte ( 0x08, 0x00 ) {
        push( @{$frame}, $byte);
    }

    # addr len = 0x06
    # protocol len = 0x04
    # opcode request = 0x00 0x01
    foreach my $byte ( 0x06, 0x04, 0x00, 0x01) {
        push( @{$frame}, $byte);
    }

    # Add mac
    my ($macHi, $macLo) = GenFrame::getMAC($parameters->{smac});
    push( @{$frame}, ($macHi >> 8) & 0xff);
    push( @{$frame}, $macHi & 0xff);
    push( @{$frame}, ($macLo >> 24) & 0xff);
    push( @{$frame}, ($macLo >> 16) & 0xff);
    push( @{$frame}, ($macLo >> 8)  & 0xff);
    push( @{$frame}, $macLo & 0xff);

    # Add ip
    my @bytes = split(/\./, $parameters->{sip});
    foreach my $b ( @bytes)
    {
        push( @{$frame}, $b);
    }

    # Add DUT mac
    my $tMac = "FF:FF:FF:FF:FF:FF";
    if(defined $parameters->{dutmac})
    {
        $tMac = $parameters->{dutmac}; 
    }

    foreach my $b ( split(/:/, $tMac) )
    {
        push( @{$frame}, hex($b));
    }

    # Add DUT IP
    # in this case it's gratuitous, so add the host's ip
    my $tIP = $parameters->{sip};
    if(defined $parameters->{dutip})
    {
        $tIP = $parameters->{dutip};
    }

    foreach my $b ( split(/\./, $tIP))
    {
        push( @{$frame}, $b);
    }

    return $frame;
}

##@method public char** genICMPv4Header(char *da, char *sa, char *SIP,
#                                       char *DIP, int messageType)
# Generate an ICMP for IP version 4 header
# @param[in] da The Layer 2 destination address
# @param[in] sa The Layer 2 source address
# @param[in] SIP The IP version 4 source address
# @param[in] DIP The IP version 4 destination address
# @param[in] messageType The 16-bit ICMP message type consisting of the 8-bit
# type field (bits 0..7) and the 8-bit code field (bits 8..15)
# @return A Perl reference to an array containing the ICMP header
sub genICMPv4Header($$$$$)
{
    my ($da, $sa, $SIP, $DIP, $messageType) = @_;
    
    my $pDataArray = genIPheader($da, $sa, $SIP, $DIP);

    # Ensure that the Layer 3 header is an IP version 4 header.
    my $l3Index = getL3Index($pDataArray);
    my $ipVersion = (($pDataArray->[$l3Index] >> 4) & 0xf);
    if ($ipVersion != 4)
    {
        traceback("Warning: Either SIP or DIP is not an IP version 4 address");
        return undef;
    }

    # Remove the default UDP payload.
    my $l4Index = getL4Index($pDataArray);
    splice(@{$pDataArray}, $l4Index);

    # Create an ICMP header.
    my $icmpByteArray = [(0x0) x 8];
    $icmpByteArray->[0] = ($messageType & 0xff);
    $icmpByteArray->[1] = (($messageType >> 8) & 0xff);
    my $checkSum = oneComplementAdd($icmpByteArray);
    $icmpByteArray->[2] = (($checkSum >> 8) & 0xff);
    $icmpByteArray->[3] = ($checkSum & 0xff);

    # Change the protocol field of the Layer 3 header, update the payload
    # length field and recalculate the IP version 4 header checksum.
    updateL3Protocol($pDataArray, PROTOCOL_ICMP);
    updateL3PayloadLen($pDataArray, scalar(@{$icmpByteArray}));
    updateIPv4Chksum($pDataArray);

    # Append the ICMP header to the Layer 3 header.
    for (my $i=0 ; $i<scalar(@{$icmpByteArray}) ; $i++)
    {
        $pDataArray->[$l4Index + $i] = $icmpByteArray->[$i];
    }
    return $pDataArray;
}

##@method public char** genIGMPv3Frame(char* sa, char *SIP, char *DIP, int type)
# Generate an IGMP version 3 header
# @param[in] sa The Layer 2 source address
# @param[in] SIP The IP version 4 source address
# @param[in] DIP The IP version 4 multicast destination address. Valid values
# are in the range between 224.0.0.0 and 239.255.255.255
# @param[in] type The IGMP version 3 message type. Valid values are:
# <ol>
#   <li value="17"> Membership Query
#   <li value="34"> Version 3 Membership Report
# </ol>
# @return A Perl reference to an array containing the IGMPv3 header
sub genIGMPv3Frame($$$$)
{
    validatePrototype(@_, 4);
    my ($sa, $SIP, $DIP, $type) = @_;

    my $pDataArray = [];

    # Check whether an IP version 4 source and an IP version 4 multicast
    # destination address have been provided.
    my $sip = getIPv4Address($SIP);
    my $dip = getIPv4Address($DIP);
    if (!defined($sip)
        || (!defined($dip)
            || ($dip < 0xE0000000 || $dip > 0xEFFFFFFF)))
    {
        return $pDataArray;
    }

    # Construct a multicast Layer 2 destination address based on the Layer 2
    # multicast prefix of 0x01005E and the lower 23 bits of the Layer 3
    # multicast destination address.
    my $da = setMacAddress(((0x5E << 24) | ($dip & 0x7FFFFF)), 0x0100);
    $pDataArray = genIPheader($da, $sa, $SIP, $DIP);

    # Remove the default UDP payload.
    my $l4Index = getL4Index($pDataArray);
    splice(@{$pDataArray}, $l4Index);

    my $ttl = undef;
    # The IP version 4 address range between 224.0.0.0 and 224.0.0.255 is
    # intended for applications that never need to multicast further than one
    # hop.
    if ($dip >= 0xE0000000 && $dip <= 0xE00000FF)
    {
        $ttl = 1;
    }
    setL3Para(undef, $ttl, PROTOCOL_IGMP, $pDataArray);

    my $message = [];
    if ($type == 0x11)
    {
        # FIXME: Not yet implemented.
    }
    elsif ($type == 0x22)
    {
        # IGMP version 3 message type.
        $message->[0] = 0x22;
        # Reserved; this field is set to zero on transmission.
        $message->[1] = 0x0;
        # IGMP version 3 checksum field; this field is set to zero for
        # computing the checksum.
        $message->[2] = 0x0;
        $message->[3] = 0x0;
        # Reserved; this field is set to zero on transmission.
        $message->[4] = 0x0;
        $message->[5] = 0x0;

        # Number of Group Records
        # FIXME: For the time being, the Number of Group Records field is set
        # to zero and the IGMP version 3 message is padded with zeros to create
        # a 22 byte IP payload.
        $message->[6] = 0x0;
        $message->[7] = 0x0;
        for (my $i=8 ; $i<22 ; $i++)
        {
            $message->[$i] = 0x0;
        }
    }
    elsif ($type == 0x17)
    {
        # IGMP version 3 message type.
        $message->[0] = 0x17;
        # Reserved; this field is set to zero on transmission.
        $message->[1] = 0x0;
        # IGMP version 3 checksum field; this field is set to zero for
        # computing the checksum.
        $message->[2] = 0x0;
        $message->[3] = 0x0;
        # Reserved; this field is set to zero on transmission.
        $message->[4] = 0x0;
        $message->[5] = 0x0;

        # Number of Group Records
        # FIXME: For the time being, the Number of Group Records field is set
        # to zero and the IGMP version 3 message is padded with zeros to create
        # a 22 byte IP payload.
        $message->[6] = 0x0;
        $message->[7] = 0x0;
        for (my $i=8 ; $i<22 ; $i++)
        {
            $message->[$i] = 0x0;
        }
    }
    # Compute the IGMP version 3 message checksum and update the checksum
    # field.
    my $checksum = oneComplementAdd($message);
    $message->[2] = (($checksum >> 8) & 0xff);
    $message->[3] = ($checksum & 0xff);

    # Add the IGMP version 3 message as IP payload to the frame.
    for (my $i=0 ; $i<scalar(@{$message}) ; $i++)
    {
        $pDataArray->[$l4Index + $i] = $message->[$i];
    }

    return $pDataArray;
}

##@method public char** genIPv4PseudoHeader(char **pDataArray, int protocol,
#                                           int length, int ISLTag)
# Generate an IP version 4 pseudo header for the specified Layer 3 header which
# can be used to update the upper-layer header checksum
# @param[in] pDataArray A Perl reference to an array containing an IP version 4
# header
# @param[in] protocol The upper-layer protocol
# @param[in] length The upper-layer packet length. In most cases (e.g. TCP)
# this is the length of the upper-layer header and data, while in some cases
# (e.g. UDP) it is the length carried in the upper-layer header
# @param[in] ISLTag @optional The ISL tag to be expected. See @link
# Scripts::GenFrame::L2::getL3Index() getL3Index @endlink for a list of valid
# choices. The default is @c Base::Enum::ISL_NONE()
# @return A Perl reference to an array containing the 12-byte IP version 4
# pseudo header
sub genIPv4PseudoHeader($$$;$)
{
    my ($pDataArray, $protocol, $length, $ISLTag) = @_;

    # Copy the IP version 4 source and destination address to the pseudo
    # header. 
    my $l3Index = getL3Index($pDataArray, $ISLTag);
    my $pPseudoHeader = [];
    for (my $i=0 ; $i<8 ; $i++)
    {
        $pPseudoHeader->[$i] = $pDataArray->[$l3Index + 12 + $i];
    }
    $pPseudoHeader->[8] = 0x0;
    $pPseudoHeader->[9] = $protocol;
    $pPseudoHeader->[10] = (($length >> 8) & 0xff);
    $pPseudoHeader->[11] = ($length & 0xff);
    return $pPseudoHeader;
}

##@method public char** setIPv4Frag(int id, int flag, int offset,
#                                   char **pDataArray)
# @param[in] id
# @param[in] flag
# @param[in] offset
# @param[in,out] pDataArray
# @return
sub setIPv4Frag($$$$)
{
    validatePrototype(@_, 4);
    my ($id, $flag, $offset, $pDataArray) = @_;

    my $l3Index = getL3Index($pDataArray);
    # Perform an IP version check.
    my $ipVersion = (($pDataArray->[$l3Index] >> 4) & 0xF);
    if ($ipVersion != 4)
    {
        return $pDataArray;
    }
    $pDataArray->[$l3Index + 4] = (($id >> 8) & 0xff);
    $pDataArray->[$l3Index + 5] = ($id & 0xff);
    $pDataArray->[$l3Index + 6] = (($flag << 5) | (($offset >> 8) & 0x1f));
    $pDataArray->[$l3Index + 7] = ($offset & 0xff);
    updateIPv4Chksum($pDataArray);
    return $pDataArray;
}

##@method public char** shortenIPv4Hdr(int headerLength, char **pDataArray)
# Shortens the IP version 4 header
# @param[in] headerLength The new IP version 4 header length in units of 4-byte
# @param[in,out] pDataArray A Perl reference to an array containing an IP
# version 4 header
# @return A Perl reference to an array containing the shortened IP version 4
# header
sub shortenIPv4Hdr($$)
{
    validatePrototype(@_, 2);
    my ($headerLength, $pDataArray) = @_;

    my $l3Index = getL3Index($pDataArray);
    # Perform an IP version check.
    my $ipVersion = (($pDataArray->[$l3Index] >> 4) & 0xF);
    if ($ipVersion != 4)
    {
        return $pDataArray;
    }
    # Shorten the IP version 4 header to the specified length.
    splice(@{$pDataArray},
           $l3Index + 4 * $headerLength,
           4 * (($pDataArray->[$l3Index] & 0xF) - $headerLength));
    if ($headerLength >= 5)
    {
        updateIPv4HdrLen($headerLength, $pDataArray);
    }
    else
    {
        # Since the total length field and the checksum field are not
        # guaranteed to exists, only update the header length field.
        $pDataArray->[$l3Index] = (($pDataArray->[$l3Index] & 0xf0)
                                   | $headerLength);
    }
    return $pDataArray;
}

##@method private char** updateIPv4HdrLen(int headerLength, char **pDataArray)
# Updates the header length and total length fields of the specified IP version
# 4 header with the specified header length
# @param[in] headerLength The new IP version 4 header length in units of
# 4-byte
# @param[in,out] pDataArray A Perl reference to an array containing an IP
# version 4 header
sub updateIPv4HdrLen($$)
{
    validatePrototype(@_, 2);
    my ($headerLength, $pDataArray) = @_;

    my $l3Index = getL3Index($pDataArray);
    # Perform an IP version check.
    my $ipVersion = (($pDataArray->[$l3Index] >> 4) & 0xF);
    if ($ipVersion != 4)
    {
        return $pDataArray;
    }
    my $totalLength = (($pDataArray->[$l3Index + 2] << 8)
                       | $pDataArray->[$l3Index + 3]);
    $totalLength -= 4 * ($pDataArray->[$l3Index] & 0xF);
    $totalLength += 4 * $headerLength;
    $pDataArray->[$l3Index] = (($ipVersion << 4) | ($headerLength & 0xF));
    $pDataArray->[$l3Index + 2] = (($totalLength >> 8) & 0xFF);
    $pDataArray->[$l3Index + 3] = ($totalLength & 0xFF);
    updateIPv4Chksum($pDataArray);
    return $pDataArray;
}

1;
