# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/SwitchCore.pm
# Creation Date:    12/04/07
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

package Applications::TestPoint::Common::SwitchCore;
use strict;
use warnings;

#This is only loaded when FM4000 switch is inserted
#use base qw(
#    Applications::TestPoint::Common::FM4000::SwitchCore
#);
#

use SDKScalars;

###############################################################################
#
#                           PUBLIC FUNCTIONS
#
###############################################################################

##@cmethod public int tpHandleSetCPUProtection(char *state)
#
# @brief        Handles enabling CPU protection
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleSetCPUProtection
{
    my ($self, $state) = @_;

    if (!defined($state))
    {
        print("Must specify <state>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
    foreach my $switchNum ($self->tpGetSwitches)
    {
        FAMILY: for (($self->tpGetSwitchInfo)[$switchNum]->{'switchFamily'})
        {
            $_ == $FM_SWITCH_FAMILY_FM4000 && do
            {
                my $status = $self->tp4000HandleSetCPUProtection($switchNum,
                                                                 $state);
                $result = $status != $FM_OK ? $FM_FAIL : $result;
            };

            do
            {
                # Ignore
            };
        }
    }
    return $result;
}

##@cmethod public int tpHandleSetTrapClass(char *class,
#                                          char *attribute,
#                                          char *value)
#
# @brief        Handles setting trap_class attributes
#
# @param[in]    class The trap_class to be modified
#
# @param[in]    attribute The attribute to be set
#
# @param[in]    value The value the attribute is to be set to
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleSetTrapClass
{
    my ($self, $class, $attribute, $value) = @_;

    if (!defined($class) || !defined($attribute) || !defined($value))
    {
        print("Must specify <class> <attribute> <value>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $family = ($self->tpGetSwitchInfo)[$switchNum]->{'switchFamily'};
        if ($family == $FM_SWITCH_FAMILY_FM4000)
        {
            my $status = $self->tp4000HandleSetTrapClass($switchNum,
                                                         $class,
                                                         $attribute,
                                                         $value);
            $result = $status != $FM_OK ? $FM_FAIL : $result;
        }
        elsif ($family == $FM_SWITCH_FAMILY_FM6000)
        {
            my $status = $self->tp6000HandleSetTrapClass($switchNum,
                                                         $class,
                                                         $attribute,
                                                         $value);
            $result = $status != $FM_OK ? $FM_FAIL : $result;
        }
    }
    return $result;
}

1;
