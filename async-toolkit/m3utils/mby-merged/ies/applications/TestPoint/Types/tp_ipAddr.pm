# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Types/tp_ipAddr.pm
# Creation Date:    10/22/07
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

##@class Types::tp_ipAddr
#
# The @c address field contains the IP address in network byte order. For IP
# version 4 only index 0 is used, while for IP version 6 index 0 contains the
# least significant 32 bits of the IP address.
package Types::tp_ipAddr;

use base qw(Exporter);

use fields qw(
    af
    address
    prefix
);

our @EXPORT = qw(
    TP_AF_INET
    TP_AF_INET6
);

use constant TP_AF_INET     => 4;
use constant TP_AF_INET6    => 6;

sub new
{
    my ($class) = @_;

    $class = ref($class) || $class;
    my Types::tp_ipAddr $self = fields::new($class);
    $self->{'af'} = 0;
    $self->{'address'} = [(0) x 4];
    $self->{'prefix'} = undef;
    return $self;
}

1;
