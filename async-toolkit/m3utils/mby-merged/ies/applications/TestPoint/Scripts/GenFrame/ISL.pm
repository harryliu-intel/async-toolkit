# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Scripts/GenFrame/ISL.pm
# Creation Date:    10/10/06
# Description:      ISL tag generation and parsing support for Test Engine 2
#                   tests.
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

package Scripts::GenFrame::ISL;
use strict;

require Exporter;
use List::Util qw(min);
our @ISA = qw(Exporter);
our @EXPORT = qw(
    addISLheader
    genF32ISLheader
    genF64ISLheader
    genF96ISLheader
    setISLheader
);

##@method char *frame addISLheader(char *islTag, char *frame)
# Add an ISL header (4B, 8B or 12B) to a frame. If the frame data array has a
# length greater than 12, the ISL header will be inserted at byte 13 shifting
# the rest of the array.
# @param islTag The ISL tag which is to be tagged onto the specified frame
# @param frame The frame onto which the ISL tag will be tagged
# @return An ISL tagged frame
sub addISLheader($$)
{
    my ($islTag, $frame) = @_;

    my $length = scalar(@{$frame});
    my $islLength = scalar(@{$islTag});

    my $data = [];
    for (my $i=0 ; $i<min($length - 12, $islLength) ; $i++)
    {
        $data->[$i] = $frame->[$i + 12];
    }
    setISLheader($islTag, $frame);
    for (my $i=$length - 1 ; $i>=12 + $islLength ; $i--)
    {
        $frame->[$i + $islLength] = $frame->[$i];
    }
    for (my $i=0 ; $i<min($length - 12, $islLength) ; $i++)
    {
        $frame->[$i + 12 + $islLength] = $data->[$i];
    }
    return $frame;
}


## @method char* genF32ISLheader(hash isl_info)
# Generate a Fulcrum Microsystems 32B (F32) ISL tag. The F32 ISL tag has the
# following format:
# @code
#     7            MSB            0   7            LSB            0
#   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
#   |               Special (SRCGLORT) Ethernet Type                |
#   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
#   |                         Source GLORT                          |
#   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
# @endcode
# @param isl_info A perl hash with the following entries.  The default
# values for all entries are:
# @code
#   %isl_info = (
#       user        => 0,   # The user bits (high bit set => add srcglort tag)
#       srcGlort    => 0,   # The 16 bit source global resource tag (GLORT)
#       F32EtypeSG   => 0xF320, # Ethertype for SRCGLORT
#   )
# @endcode
# @return An F32 ISL tag (may be an empty list reference)
sub genF32ISLheader(\%)
{
    my ($isl_info)= @_;

    my $tag = []; # list reference, may be returned empty

    # USER: default is to not attach the srcglort info
    my $user = (exists($isl_info->{user}) ? $isl_info->{user} : 0);
    # Source GLORT: by default let Bali set the source GLORT
    my $srcGlort = (exists($isl_info->{srcGlort}) ? $isl_info->{srcGlort} : 0);

    my $etype_sg   = $isl_info->{"F32EtypeSG"};
    $etype_sg   = 0xF320 if not defined $etype_sg;

    if ($user&0x80) {
        $tag->[0] = (($etype_sg >> 8) & 0xff);
        $tag->[1] = ($etype_sg & 0xff);
        $tag->[2] = (($srcGlort >> 8) & 0xff);
        $tag->[3] = ($srcGlort & 0xff);
    }
    return $tag;
}

## @method char* genF64ISLheader(hash isl_info)
# Generate a Fulcrum Microsystems 64B (F64) ISL tag. The F64 ISL tag has the
# following format:
# @code
#     7            MSB            0   7            LSB            0
#   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
#   | FTYPE | VTYPE |     SYSPRI    |          USER                 |
#   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
#   | VLAN_PRI  |CFI|                      VLAN                     |
#   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
#   |                         Source GLORT                          |
#   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
#   |                      Destination GLORT                        |
#   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
# @endcode
# @param isl_info A perl hash with at most 9 entries. The default values for
# all entries are:
# @code
#   %isl_info = (
#       ftype       => 0,   # The frame type
#       vtype       => 0,   # The VLAN type
#       syspri      => 0,   # The 8 bit switch priority
#       user        => 0,   # The user bits
#       vpri        => 0,   # The 3 bit VLAN priority
#       cfi         => 0,   # The 1 bit VLAN CFI priority; interpreted by the
#                           # design as VLAN priority bit 0
#       vid         => 0,   # The 12 bit VLAN ID
#       srcGlort    => 0,   # The 16 bit source global resource tag (GLORT)
#       dstGlort    => 0    # The 16 bit destination GLORT
#   )
# @endcode
# @return A F64 ISL tag
sub genF64ISLheader(\%)
{
    my ($isl_info)= @_;

    my $tag;

    # FTYPE: default is switched traffic
    my $ftype = (exists($isl_info->{ftype}) ? $isl_info->{ftype} : 0x0);
    # VTYPE: default is a frame without a VLAN tag or a frame whose VLAN tag
    # was ignored, unless one or more of the VLAN_PRI, CFI or VID entries are
    # exists in which case the default is a VLAN tag of type 0x8100.
    my $vtype = (exists($isl_info->{vtype})
                 ? $isl_info->{vtype}
                 : (exists($isl_info->{vpri})
                    || exists($isl_info->{cfi})
                    || exists($isl_info->{vid}) ? 0x1 : 0x0));
    # SYSPRI: default is switch priority 0, which by default coincides with
    # traffic class 0
    my $syspri = (exists($isl_info->{syspri}) ? $isl_info->{syspri} : 0);
    # USER: default is no special information
    my $user = (exists($isl_info->{user}) ? $isl_info->{user} : 0);
    # VLAN_PRI: default is VLAN priority 0, which by default coincides with
    # traffic class 0
    my $vpri = (exists($isl_info->{vpri}) ? $isl_info->{vpri} : 0);
    # CFI: default is 0, i.e. the frame was sent out over an ethernet type
    # network
    my $cfi = (exists($isl_info->{cfi}) ? $isl_info->{cfi} : 0);
    # VID: default is VLAN ID 0
    my $vid = (exists($isl_info->{vid}) ? $isl_info->{vid} : 0);
    # Source GLORT: by default let Bali set the source GLORT
    my $srcGlort = (exists($isl_info->{srcGlort}) ? $isl_info->{srcGlort} : 0);
    # Destination GLORT: by default let Bali set the destination GLORT
    my $dstGlort = (exists($isl_info->{dstGlort}) ? $isl_info->{dstGlort} : 0);

    $tag->[0] = (($ftype & 0x3) << 6)
                | (($vtype & 0x3) << 4)
                | ($syspri & 0xf);
    $tag->[1] = ($user & 0xff);
    $tag->[2] = (($vpri & 0x7) << 5)
                | (($cfi & 0x1) << 4)
                | (($vid >> 8) & 0xf);
    $tag->[3] = ($vid & 0xff);
    $tag->[4] = (($srcGlort >> 8) & 0xff);
    $tag->[5] = ($srcGlort & 0xff);
    $tag->[6] = (($dstGlort >> 8) & 0xff);
    $tag->[7] = ($dstGlort & 0xff);
    return $tag;
}


sub genF96ISLheader(\%)
{
    my ($isl_info)= @_;

    my $f96 = (exists($isl_info->{"f96_rsv"}) ?  $isl_info->{"f96_rsv"} : 0);

    my $tag = genF64ISLheader(%$isl_info);
    $tag->[8]  = (($f96 >> 24) & 0xff);
    $tag->[9]  = (($f96 >> 16) & 0xff);
    $tag->[10] = (($f96 >>  8) & 0xff);
    $tag->[11] = (($f96      ) & 0xff);

    return $tag;
}


##@method char* setISLheader(char *islTag, char *frame)
# Set the ISL header (4B, 8B or 12B) of the frame pointed to by frame to the
# data contained in islTag
# @param islTag The ISL tag which is to be tagged onto the specified frame
# @param frame The frame onto which the ISL tag will be tagged
# @return An ISL tagged frame
sub setISLheader($$)
{
    my ($islTag, $frame) = @_;

    my $length = @$frame;
    my $islLength = scalar(@{$islTag});

    for (my $i=0 ; $i<$islLength ; $i++)
    {
        $frame->[$i + 12] = $islTag->[$i];
    }
    return $frame;
}

1;
