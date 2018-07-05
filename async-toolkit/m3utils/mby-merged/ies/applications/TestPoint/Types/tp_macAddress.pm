# vim:et:sw=4:ts=4:tw=79:
###############################################################################
# File:             Types/tp_macAddress.pm
# Creation Date:    11/2/07
# Description:      
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

##@class Types::tp_macAddress
package Types::tp_macAddress;

use strict;
use warnings;
use base qw(Exporter);

use fields qw(
    macAddress
    mask
);

sub new
{
    my ($class, $type) = @_;

    $class = ref($class) || $class;
    my Types::tp_macAddress $self = fields::new($class);

    $self->{"macAddress"} = undef;
    $self->{"mask"} = undef;

    return $self;
}

1;
