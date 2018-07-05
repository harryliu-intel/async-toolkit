# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Scripts/GenFrame.pm
# Creation Date:    08/02/06
# Description:      Frame generation support for Test Engine 2 tests.
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

##@namespace Scripts::GenFrame
# XXX

##@class GenFrame
# @code
#   use Scripts::GenFrame qw(:CONSTANTS);
# @endcode
package GenFrame;
use strict;
use warnings;

require Exporter;
use Scripts::GenFrame::IPv4;
use Scripts::GenFrame::IPv6;
use Scripts::GenFrame::ISL;
use Scripts::GenFrame::L2;
use Scripts::GenFrame::L3;
use Scripts::GenFrame::L4;
use Scripts::GenFrame::TCP;
use Scripts::GenFrame::UDP;
use Scripts::GenFrame::Utilities;
use Scripts::Utilities;
our %EXPORT_TAGS = (
    DA          => [qw(
        DA_BPDU
        DA_LACP
        DA_8021x
        DA_GMRP
        DA_GVRP
    )],
    PROTOCOL    => [qw(
        PROTOCOL_TCP
        PROTOCOL_UDP
        PROTOCOL_ICMP
        PROTOCOL_ICMPv6
        PROTOCOL_IGMP
    )],
    IPv6        => [qw(
        HEADER_HOP_BY_HOP
        HEADER_ROUTING
        HEADER_FRAGMENTATION
        HEADER_ESP
        HEADER_AUTHENTICATION
        HEADER_DESTINATION
    )],
    MLD         => [qw(
        MLD_QUERY
        MLD_REPORT
        MLD_DONE
    )],
    UTILITIES   => [qw(
        INDEX_MA_HI
        INDEX_MA_LO
    )]
);
our @ISA = qw(
    Exporter
    Scripts::GenFrame::IPv4
    Scripts::GenFrame::IPv6
    Scripts::GenFrame::ISL
    Scripts::GenFrame::L2
    Scripts::GenFrame::L3
    Scripts::GenFrame::L4
    Scripts::GenFrame::TCP
    Scripts::GenFrame::UDP
    Scripts::GenFrame::Utilities
);

# Add all tag classes to the :CONSTANTS class and add all classes to @EXPORT.
{
    my %seen = ();
    foreach my $key (keys(%EXPORT_TAGS))
    {
        if (!$seen{$key}++)
        {
            Exporter::export_tags($key);
            push(@{$EXPORT_TAGS{CONSTANTS}}, @{$EXPORT_TAGS{$key}});
        }
    }
    Exporter::export_tags('CONSTANTS');
}

# Create a Scripts::GenFrame symbol table that only contains constants defined
# in the %EXPORT_TAGS hash.
{
    no strict 'refs';
    my %seen = ();
    *Scripts::GenFrame::EXPORT = $GenFrame::{EXPORT};
    *Scripts::GenFrame::EXPORT_TAGS = $GenFrame::{EXPORT_TAGS};
    *Scripts::GenFrame::ISA = $GenFrame::{ISA};
    foreach my $key (keys(%EXPORT_TAGS))
    {
        foreach my $symbol (@{$EXPORT_TAGS{$key}})
        {
            if (!$seen{$symbol})
            {
                *{"Scripts::GenFrame::" . $symbol} = $GenFrame::{$symbol};
            }
        }
    }
}

our $PROTO_TCP      = PROTOCOL_TCP;
our $PROTO_UDP      = PROTOCOL_UDP;
our $PROTO_ICMP     = PROTOCOL_ICMP;
our $PROTO_ICMPv6   = PROTOCOL_ICMPv6;
our $PROTO_IGMP     = PROTOCOL_IGMP;

##@method void printFrame(char **pDataArray)
# Print a frame a byte at a time
# @param[in] pDataArray A Perl reference to the packet data which is to be
# printed
sub printFrame($)
{
    validatePrototype(@_, 1);
    my ($pDataArray) = @_;

    printf("%s\n", toString($pDataArray));
}

##@method public char* toString(char **pDataArray)
# Convert the specified packet data into its string representation
# @param[in] pDataArray A Perl reference to a Perl array containing packet data
# @return The string representation of the specified packet data
sub toString($)
{
    validatePrototype(@_, 1);
    my ($pDataArray) = @_;

    my @indices = ();
    my $string = "";
    for (my $i=0 ; $i<scalar(@{$pDataArray}) ; $i++)
    {
        if ($i > 0 && ($i % 16) == 0)
        {
            $string .= sprintf("\n");
        }
        my $data = $pDataArray->[$i];
        # Provide a way to detect frame format errors.
        if (!defined($data))
        {
            $data = 0x0;
            push(@indices, $i);
        }
        $string .= sprintf(" %02x ", $data);
    }
    if (scalar(@indices) > 0)
    {
        traceback(sprintf("Warning: Frame format errors at byte %s",
                          join(", ", @indices)));
    }
    return $string;
}

1;
