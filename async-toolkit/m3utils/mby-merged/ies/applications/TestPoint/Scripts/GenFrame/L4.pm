# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Scripts/GenFrame/L4.pm
# Creation Date:    03/01/07
# Description:      Layer 4 frame generation support for Test Engine 2 tests
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

package Scripts::GenFrame::L4;
use strict;
use warnings;

require Exporter;
use Scripts::GenFrame::IPv4;
use Scripts::GenFrame::IPv6;
use Scripts::GenFrame::L2;
use Scripts::GenFrame::L3;
our @ISA = qw(Exporter);
our @EXPORT = qw(
    addL4header
    updateTCPChksum
    updateUDPChksum
);

##@method public char** addL4header(int protocol, int sport, int dport, char **pDataArray, int ISLTag)
# Adds a new Layer 4 header to an existing Layer 3 header and updates the
# protocol field in case of an IP version 4 frame or the last Next Header
# field in case of an IP version 6 frame
# @param[in] protocol The Layer 4 protocol. Valid values are:
# <ul>
#   <li> @c PROTOCOL_TCP
#   <li> @c PROTOCOL_UDP
# </ul>
# @param[in] sport The Layer 4 source port
# @param[in] dport The Layer 4 destination port
# @param[in,out] pDataArray A Perl reference to an array containing a Layer 3
# header
# @param[in] ISLTag @optional The ISL tag to be expected. See @link
# Scripts::GenFrame::L2::getL3Index() getL3Index @endlink for a list of valid
# choices. The default is @c Base::Enum::ISL_NONE()
# @return A Perl reference to an array containing a Layer 4 header
sub addL4header($$$$;$)
{
    my ($protocol, $sport, $dport, $pDataArray, $ISLTag) = @_;

    # Retrieve the current Layer 3 payload length.
    my $payloadLength = getL3PayloadLen($pDataArray, $ISLTag);

    # Remove the default UDP payload and adjust the current Layer 3 payload
    # length.
    my $l4Index = getL4Index($pDataArray, $ISLTag);
    my @payload = splice(@{$pDataArray}, $l4Index);
    $payloadLength -= scalar(@payload);

    # Create l4HdrArray to store Layer 4 header bytes to calculate checksum
    my $dwArray = [];
    if ($protocol == PROTOCOL_TCP)
    {
        $dwArray->[0] = (($sport << 16) | $dport);
        $dwArray->[1] = 0;
        $dwArray->[2] = 0;
        # Generate a randomized TCP flags field
        $dwArray->[3] = (0x5 << 28)+((int(rand(0x40)))<<16);
        $dwArray->[4] = 0;
    }
    elsif ($protocol == PROTOCOL_UDP)
    {
        $dwArray->[0] = (($sport << 16) | $dport);
        $dwArray->[1] = ((0x8 << 16) | 0x0);
    }
    else
    {
        $dwArray->[0] = (($sport << 16) | $dport);
    }
    my $l4HdrArray = [];
    for (my $i=0 ; $i<scalar(@{$dwArray}) ; $i++)
    {
        for (my $j=0 ; $j<4 ; $j++)
        {
            $l4HdrArray->[4 * $i + $j] =
                                    (($dwArray->[$i] >> (24 - 8 * $j)) & 0xff);
        }
    }

    # Add the Layer 4 header to an existing Layer 3 header.
    for (my $i=0 ; $i<scalar(@{$l4HdrArray}) ; $i++)
    {
        $pDataArray->[$l4Index + $i] = $l4HdrArray->[$i];
    }

    # Update the protocol and payload length fields.
    updateL3Protocol($pDataArray, $protocol);
    $payloadLength += scalar(@{$l4HdrArray});
    updateL3PayloadLen($pDataArray, $payloadLength, $ISLTag);

    # Update the Layer 4 checksum field.
    if ($protocol == PROTOCOL_TCP)
    {
        updateTCPChksum($pDataArray, $ISLTag);
    }
    elsif ($protocol == PROTOCOL_UDP)
    {
        updateUDPChksum($pDataArray, $ISLTag);
    }
    else
    {
        # Nothing to do.
    }

    # For IP version 4 frames update the checksum field
    my $l3Index = getL3Index($pDataArray, $ISLTag);
    my $ipVersion = (($pDataArray->[$l3Index] >> 4) & 0xf);
    if ($ipVersion == 4)
    {
        updateIPv4Chksum($pDataArray);
    }
    return $pDataArray;
}

##@method private char** updateL4Chksum(char **pDataArray, int protocol, int chksumIdx, int ISLTag)
# Update the Layer 4 checksum field using a checksum procedure as used in %TCP
# @param[in,out] pDataArray A Perl reference to an array containing a Layer 4
# header
# @param[in] protocol The Layer 4 protocol
# @param[in] chksumIdx
# @param[in] ISLTag @optional The ISL tag to be expected. See @link
# Scripts::GenFrame::L2::getL3Index() getL3Index @endlink for a list of valid
# choices. The default is @c Base::Enum::ISL_NONE()
# @return A Perl reference to an array containing the modified Layer 4 header
sub updateL4Chksum($$$;$)
{
    my ($pDataArray, $protocol, $chksumIdx, $ISLTag) = @_;

    my $l3Index = getL3Index($pDataArray, $ISLTag);
    my $ipVersion = (($pDataArray->[$l3Index] >> 4) & 0xf);
    my $l4Index = getL4Index($pDataArray, $ISLTag);
    # It is assumed that $pDataArray points to a Layer 4 header that does not
    # have the Layer 2 frame check sequence (FCS) appended to it.
    my $payloadLength = scalar(@{$pDataArray}) - $l4Index;

    my $nxtHdrIdx;
    my $pPayloadArray;
    if ($ipVersion == 4)
    {
        $nxtHdrIdx = $l3Index + 9;
        if ($pDataArray->[$nxtHdrIdx] != $protocol)
        {
            return $pDataArray;
        }

        # Construct a pseudo header.
        $pPayloadArray = genIPv4PseudoHeader($pDataArray, $protocol,
                                             $payloadLength, $ISLTag);
    }
    elsif ($ipVersion == 6)
    {
        ($nxtHdrIdx) = getIPv6LastNxtHdrIdx($pDataArray, $ISLTag);
        if ($pDataArray->[$nxtHdrIdx] != $protocol)
        {
            return $pDataArray;
        }

        # Construct a pseudo header.
        $pPayloadArray = genIPv6PseudoHeader($pDataArray, $protocol,
                                             $payloadLength, $ISLTag);
    }
    else
    {
        # Unrecognized IP version.
        return $pDataArray;
    }

    my $length = scalar(@{$pPayloadArray});
    for (my $i=0 ; $i<$payloadLength ; $i++)
    {
        if ($i == $chksumIdx || $i == ($chksumIdx + 1))
        {
            # Zero out the checksum.
            $pPayloadArray->[$length + $i] = 0x0;
        }
        else
        {
            $pPayloadArray->[$length + $i] = $pDataArray->[$l4Index + $i];
        }
    }
    if ($payloadLength % 2)
    {
        # Pad the segment with zeros to create a 16-bit word for checksumming
        # purposes.
        $pPayloadArray->[$length + $payloadLength] = 0x0;
    }
    my $checksum = oneComplementAdd($pPayloadArray);
    $pDataArray->[$l4Index + $chksumIdx] = (($checksum >> 8) & 0xff);
    $pDataArray->[$l4Index + $chksumIdx + 1] = ($checksum & 0xff);
    return $pDataArray;
}


##@method public char** updateTCPChksum(char **pDataArray, int ISLTag)
# Update the %TCP segment checksum field
# @param[in,out] pDataArray A Perl reference to an array containing a %TCP
# header
# @param[in] ISLTag @optional The ISL tag to be expected. See @link
# Scripts::GenFrame::L2::getL3Index() getL3Index @endlink for a list of valid
# choices. The default is @c Base::Enum::ISL_NONE()
# @return A Perl reference to an array containing the modified %TCP header
sub updateTCPChksum($;$)
{
    my ($pDataArray, $ISLTag) = @_;

    return updateL4Chksum($pDataArray, PROTOCOL_TCP, 16, $ISLTag);
}

##@method public char** updateUDPChksum(char **pDataArray, int ISLTag)
# Update the %UDP checksum field
# @param[in,out] pDataArray A Perl reference to an array containing a %UDP
# header
# @param[in] ISLTag @optional The ISL tag to be expected. See @link
# Scripts::GenFrame::L2::getL3Index() getL3Index @endlink for a list of valid
# choices. The default is @c Base::Enum::ISL_NONE()
# @return A Perl reference to an array containing the modified %UDP header
sub updateUDPChksum($;$)
{
    my ($pDataArray, $ISLTag) = @_;

    return updateL4Chksum($pDataArray, PROTOCOL_UDP, 6, $ISLTag);
}

1;
