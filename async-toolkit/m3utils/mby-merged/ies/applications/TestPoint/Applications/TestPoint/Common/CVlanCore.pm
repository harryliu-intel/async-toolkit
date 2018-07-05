# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/CVlanCore.pm
# Creation Date:    10/10/08
# Description:      
#
# INTEL CONFIDENTIAL
# Copyright 2008 - 2011 Intel Corporation. All Rights Reserved. 
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

##@class Applications::TestPoint::Common::CVlanCore.pm
#
# @code
#   create cVlan <port> <cVlan> <sVlan>
#   del cVlan <port> <cVlan>
#   show cVlans
# @endcode
package Applications::TestPoint::Common::CVlanCore;
use strict;
use warnings;

use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

###############################################################################
#
#                           CONSTANTS & TYPES
#
###############################################################################


###############################################################################
#
#                           LOCAL VARIABLES
#
###############################################################################


###############################################################################
#
#                           PUBLIC FUNCTIONS
#
###############################################################################

##@cmethod public int tpHandleCreateCVlan(int port, uint16 cVlan, uint16 sVlan)
#
# @brief        Handles creating a customer vlan instance
#
# @param[in]    port The port to map from
#
# @param[in]    cVlan The customer vlan ID to map from 
#
# @param[in]    sVlan The service provider ID to map to
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleCreateCVlan
{
    my ($self, $port, $cVlan, $sVlan) = @_;

    my $chip = $self->{'CHIP'};
    my $switchCount = scalar(@{[$self->tpGetSwitches]});

    if (!defined($port) || (!defined($cVlan)) || (!defined($sVlan)))
    {
        print("Must specify <port>  <cVlan> <sVlan>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    # Ensure that $port is an integer number.
    if ( (!defined(($port = $self->str2intnum($port))) )
        || ($port > POSIX::INT_MAX ) || ($port < POSIX::INT_MIN ) )
    {
        return $FM_ERR_INVALID_ARGUMENT;
    }

    # Ensure that $cVlan is an integer number.
    if ( (!defined(($cVlan = $self->str2intnum($cVlan))) )
        || ($cVlan > POSIX::INT_MAX ) || ($cVlan < POSIX::INT_MIN ) )
    {
        return $FM_ERR_INVALID_ARGUMENT;
    }

    # Ensure that $sVlan is an integer number.
    if ( (!defined(($sVlan = $self->str2intnum($sVlan))) )
        || ($sVlan > POSIX::INT_MAX ) || ($sVlan < POSIX::INT_MIN ) )
    {
        return $FM_ERR_INVALID_ARGUMENT;
    }

    SWITCH: foreach my $switchNum ($self->tpGetSwitches)
    {
        my ($status);
#        $chip->disableErrors();
        $status = $chip->fmAddCVlan($switchNum, $port, $cVlan, $sVlan);
#        $chip->enableErrors();

        if ($status != $FM_OK)
        {
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            printf("cVlan (%d, %d) => %d cannot be created!\n", $port, $cVlan,
$sVlan);
            return $status;
        }
        next SWITCH;
    }
    return $FM_OK;
}

##@cmethod public int tpHandleDeleteCVlan(int port, uint16 cVlan)
#
# @brief        Handles deleting a custermer vlan instance
#
# @param[in]    port The port to operate on
#
# @param[in]    cVlan The cVlan ID to operate on
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleDeleteCVlan
{
    my ($self, $port, $cVlan) = @_;

    my $chip = $self->{'CHIP'};
    my $switchCount = scalar(@{[$self->tpGetSwitches]});

    if (!defined($port) || !defined($cVlan) )
    {
        print("Must specify <port> <cVlan>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    # Ensure that $port is an integer number.
    if ( (!defined(($port = $self->str2intnum($port))) )
        || ($port > POSIX::INT_MAX ) || ($port < POSIX::INT_MIN ) )
    {
        return $FM_ERR_INVALID_ARGUMENT;
    }

    # Ensure that $cVlan is an integer number.
    if ( (!defined(($cVlan = $self->str2intnum($cVlan))) )
        || ($cVlan > POSIX::INT_MAX ) || ($cVlan < POSIX::INT_MIN ) )
    {
        return $FM_ERR_INVALID_ARGUMENT;
    }

    SWITCH: foreach my $switchNum ($self->tpGetSwitches)
    {
        my ($status);

#        $chip->disableErrors();
        $status = $chip->fmDeleteCVlan($switchNum, $port, $cVlan);
#        $chip->enableErrors();

        if ($status != $FM_OK)
        {
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            printf("cVlan (%d, %d) cannot be deleted!\n", $port, $cVlan);
            return $status;
        }
        next SWITCH;
    }
    return $FM_OK;
}


##@cmethod public int tpHandleShowCVlans()
#
# @brief        Handles showing all cVlan mappings
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleShowCVlans
{
    my ($self) = @_;

    my $chip = $self->{'CHIP'};
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    my $status = $FM_OK;

    print("------------------------------------------------------------\n");
    $chip->disableErrors();

    SWITCH: foreach my $switchNum ($self->tpGetSwitches)
    {
        printf("Switch %d: ", $switchNum) if ($switchCount > 1);

        if ( ($status != $FM_OK) && ($status != $FM_ERR_NO_MORE) )
        {
            return $FM_FAIL;
        }

        my $firstPort;
        my $firstCVlan; 
        my $sVlan;
        my $nextPort;
        my $nextCVlan;

        $status = $chip->fmGetCVlanFirst($switchNum, \$firstPort, \$firstCVlan);

        while ($status == $FM_OK)
        {
            $chip->fmGetSVlanFromPortCVlan($switchNum, 
                                           $firstPort, 
                                           $firstCVlan,
                                           \$sVlan);
            printf("(%d, %d) => %d\n", $firstPort, $firstCVlan, $sVlan);

            $status = $chip->fmGetCVlanNext($switchNum,
                                            $firstPort, 
                                            $firstCVlan,
                                            \$nextPort,
                                            \$nextCVlan);
            if ($status == $FM_OK)
            {
                $firstPort = $nextPort;
                $firstCVlan = $nextCVlan;
            }
        }
        next SWITCH;
        print("\n");
    }
    $chip->enableErrors();
    return $status;
}

1;
