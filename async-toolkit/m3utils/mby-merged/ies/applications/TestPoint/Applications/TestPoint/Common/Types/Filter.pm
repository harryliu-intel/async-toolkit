# vim:et:sw=4:ts=4:tw=79:

###############################################################################
# File:             Applications/TestPoint/Common/Types/Filter.pm
# Creation Date:    10/17/07
# Description:      TestPoint XML filter class
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

##@class Applications::TestPoint::Common::Types::Filter
package Applications::TestPoint::Common::Types::Filter;
use strict;
use warnings;

require Exporter;
use Class::Struct;
our @EXPORT = qw(
    FILTER_TYPE_PLATFORM
    FILTER_TYPE_FAMILY
    FILTER_TYPE_SWITCH_COUNT
    FILTER_TYPE_UPDATE_SUPPORT

    FILTER_ACTION_DENY
    FILTER_ACTION_ALLOW
);

##@var FILTER_TYPE_PLATFORM
#
# @brief        Filters on the Perl platform
use constant FILTER_TYPE_PLATFORM       => 0;

##@var FILTER_TYPE_FAMILY
#
# @brief        Filters on the switch family
use constant FILTER_TYPE_FAMILY         => 1;

##@var FILTER_TYPE_SWITCH_COUNT
#
# @brief        Filters on the number of switches present
use constant FILTER_TYPE_SWITCH_COUNT   => 2;

##@var FILTER_TYPE_UPDATE_SUPPORT
#
# @brief        Filters on the update command support
use constant FILTER_TYPE_UPDATE_SUPPORT => 3;

##@var FILTER_ACTION_DENY
#
# @brief        Excludes the filter target
use constant FILTER_ACTION_DENY         => 0;

##@var FILTER_ACTION_ALLOW
#
# @brief        Includes the filter target
use constant FILTER_ACTION_ALLOW        => 1;

##@struct Applications::TestPoint::Common::Types::Filter
struct(
    type        => '$',
    value       => '$',
    action      => '$',
);

##@cmethod public Applications::TestPoint::Common::Types::Filter clone(void)
sub clone
{
    my ($self) = @_;

    my $filter = Applications::TestPoint::Common::Types::Filter->new();
    $filter->type  ($self->type);
    $filter->value ($self->value);
    $filter->action($self->action);
    return $filter;
}

##@method private void import
sub import
{
    # Class::Struct forbids use of @ISA
    goto &Exporter::import
}

1;
