# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Scripts/GenFrame/UDP.pm
# Creation Date:    08/22/07
# Description:      UDP frame generation support 
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

package Scripts::GenFrame::UDP;
use strict;
use warnings;

require Exporter;
use Scripts::GenFrame::L2;
use Scripts::GenFrame::L3;
use Scripts::GenFrame::L4;
our @ISA = qw(Exporter);
our @EXPORT = qw(
    genIPv4UDPFrame
);

sub genIPv4UDPFrame($)
{
    my ($parameters) = @_;

    my $headerLen = 14+20;     # dmac, smac, type, +20B ipv4 header

    #######################################################
    # Fill in default values for what the user hasn't defined
    #######################################################
    if(not defined $parameters->{frameLength})
    {
        $parameters->{frameLength} = 64;
    }

    if(not defined $parameters->{dmac}) 
    {
        $parameters->{dmac} = GenFrame::randMAC();
    }
    if(not defined $parameters->{smac}) 
    {
        $parameters->{smac} = GenFrame::randMAC();
    }
    if(not defined $parameters->{vid})
    {
        $parameters->{vid} = -1;
    }else
    {
        $headerLen += 4;
    }

    # User can specify either vpri4b, or vpri and/or cfi
    if(not defined $parameters->{vpri4b})
    {
        $parameters->{vpri4b} = 0;
    }
    if(defined $parameters->{vpri})
    {
        $parameters->{vpri4b} = $parameters->{vpri} << 1;
    }
    if(defined $parameters->{cfi})
    {
        $parameters->{vpri4b} |= $parameters->{cfi} & 0x1;
    }

    if(not defined $parameters->{sip})
    {
        $parameters->{sip} = "192.168.0.1";
    }
    if(not defined $parameters->{dip})
    {
        $parameters->{sip} = "192.168.0.2";
    }
    if(not defined $parameters->{tos})
    {
        $parameters->{tos} = 0;
    }
    if(not defined $parameters->{ttl})
    {
        $parameters->{ttl} = 255;
    }

    if(not defined $parameters->{sL4port})
    {
        # This is a random source port between 1024 and 65535
        $parameters->{sL4port} = int(rand(65535-1024))+1024;
    }

    if(not defined $parameters->{dL4port})
    {
        $parameters->{dL4port} = 0x0007;      # UDP echo
    }

    #######################################################
    # Construct the frame
    #######################################################

    my $frame = [];

    $frame = GenFrame::genL2header($parameters->{dmac}, $parameters->{smac});

    if($parameters->{vid} != -1)
    {
        $frame = GenFrame::setAlternateVLANheader(0x8100, $parameters->{vid},
                            $parameters->{vpri4b} >> 1, $parameters->{vpri4b} & 0x1,
                            $frame);
    }

    $frame = GenFrame::addL3header($parameters->{sip}, $parameters->{dip}, $frame); 
    $frame = GenFrame::setL3Para($parameters->{tos}, $parameters->{ttl}, 
                                 17, $frame);
    $frame = GenFrame::updateL3PayloadLen($frame, $parameters->{frameLength} - $headerLen - 4);

    my $l4Header = [];
    $l4Header->[0] = ($parameters->{sL4port} >> 8) & 0xff;
    $l4Header->[1] = $parameters->{sL4port} & 0xff;

    $l4Header->[2] = ($parameters->{dL4port} >> 8) & 0xff;
    $l4Header->[3] = $parameters->{dL4port} & 0xff;

    # UDP message Length
    # frameLength - L2 header - CRC -20B IPv4
    my $l4Len = $parameters->{frameLength} - $headerLen - 4 - 20;
    $l4Header->[4] = ($l4Len >> 8 ) & 0xff;
    $l4Header->[5] = $l4Len & 0xff;

    # UDP checksum, note because the TG could fill in the payload
    # the checksum is not used and set to 0
    $l4Header->[6] = 0;
    $l4Header->[7] = 0;

    foreach my $b (@{$l4Header})
    {
        push(@{$frame}, $b);
    }

    # GenFrame::printFrame($frame);

    return $frame;
}

1;
