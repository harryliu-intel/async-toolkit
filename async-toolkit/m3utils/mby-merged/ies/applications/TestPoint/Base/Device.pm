# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Device
# Creation Date:    07/14/2006
# Description:      Base object for all devices
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

##@class Base::Device
package Base::Device;
use strict;
use warnings;

##@cmethod public Base::Device& new(int                 id,
#                                   char                *type,
#                                   Base::Interface     &interface
#                                   Base::Environment   &environment)
#
# @desc         Instantiates a Perl Base::Device object
#
# @param[in]    id The global identifier for the Base::Device object to be
#               created
#
# @param[in]    type The device type identifier
#
# @param[in]    interface The device's communication interface
#
# @param[in]    environment The environment in which the device is to be
#               executed
#
# @return       An instantiated Perl Base::Device object
sub new
{
    my ($class, $id, $type, $interface, $environment) = @_;

    $class = ref($class) || $class;
    my $self  = {
        ID      => $id,
        TYPE    => $type,
        IFACE   => $interface,
        ENV     => $environment,
    };
    return bless($self, $class);
}

##@cmethod int id(void)
#
# @desc         Retrieves the global identifier for this Perl object
#
# @return       The global identifier for this Perl object
sub id
{
    my ($self) = @_;

    return $self->{ID};
}

##@cmethod char* type(void)
#
# @desc         Retrieves the chip type identifier
#
# @return       The chip type identifier
sub type
{
    my ($self) = @_;

    return $self->{TYPE};
}

1;
