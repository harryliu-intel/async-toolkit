# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Base/Enum.pm
# Creation Date:    11/10/06
# Description:      Enumeration support for Test Engine 2 tests.
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

package Base::Enum;
use strict;
use warnings;

require Exporter;
our @ISA = qw(Exporter);
our %EXPORT_TAGS = (
    FTYPE   => [qw(
        FTYPE_NORMAL
        FTYPE_ROUTED
        FTYPE_SPECIAL
        FTYPE_MGMT
    )],
    ISL     => [qw(
        ISL_NONE
        ISL_F32
        ISL_F64
        ISL_F96
        ISL_OTHER_32B
        ISL_OTHER_64B
        ISL_OTHER_96B
    )]
);
Exporter::export_tags('FTYPE');
Exporter::export_tags('ISL');

{
    $EXPORT_TAGS{CONSTANTS} = [];
    foreach my $key (keys(%EXPORT_TAGS))
    {
        push(@{$EXPORT_TAGS{CONSTANTS}}, @{$EXPORT_TAGS{$key}});
    }
    Exporter::export_tags('CONSTANTS');
}

##@def FTYPE_NORMAL
# ISL FTYPE enumeration for normal traffic
use constant FTYPE_NORMAL       => 0x0;

##@def FTYPE_ROUTED
# ISL FTYPE enumeration for routed traffic
use constant FTYPE_ROUTED       => 0x1;

##@def FTYPE_SPECIAL
# ISL FTYPE enumeration for special delivery traffic, e.g. mirror copies,
# frames trapped to the CPU and frames originating from the CPU
use constant FTYPE_SPECIAL      => 0x2;

##@def FTYPE_MGMT
# ISL FTYPE enumeration for in-band management traffic
use constant FTYPE_MGMT         => 0x3;

##@def ISL_NONE
# ISL enumeration for non-ISL tagged frames
use constant ISL_NONE           => 0x0;

##@def ISL_F32
# ISL enumeration for a Fulcrum F32 tag
use constant ISL_F32            => 0x1;

##@def ISL_F64
# ISL enumeration for a Fulcrum F64 tag
use constant ISL_F64            => 0x2;

##@def ISL_F96
# ISL enumeration for a Fulcrum F96 tag
use constant ISL_F96            => 0x3;

##@def ISL_OTHER_32B
# ISL enumeration for a generic 32-bit ISL tag
use constant ISL_OTHER_32B      => 0x4;

##@def ISL_OTHER_64B
# ISL enumeration for a generic 64-bit ISL tag
use constant ISL_OTHER_64B      => 0x5;

##@def ISL_OTHER_96B
# ISL enumeration for a generic 96-bit ISL tag
use constant ISL_OTHER_96B      => 0x6;

1;
