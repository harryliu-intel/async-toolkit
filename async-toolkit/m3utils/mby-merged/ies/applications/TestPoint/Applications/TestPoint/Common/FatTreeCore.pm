# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FatTreeCore.pm
# Creation Date:    3/19/08
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

##@class Applications::TestPoint::Common::FatTreeCore
#
# @code
#   # Enables Primitives For Setting Up a Fat Tree
# @endcode
#
# @code
# @endcode
package Applications::TestPoint::Common::FatTreeCore;
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
#                           LOCAL FUNCTIONS
#
###############################################################################

my $glortForwardIndex = 0;

my $glortCAMstart = 200;

###############################################################################
#
#                           PUBLIC FUNCTIONS
#
###############################################################################

##@cmethod public int tpHandleGlortForward(int  glort,
#                                          int  mask,
#                                          int lagNumber)
#
# @brief        Forwards a glort space over a specified LAG
#
# @param[in]    glort The glort to forward to the LAG
#
# @param[in]    glort mask The mask to use to include multiple glorts
#
# @param[in]    lagNumber The LAG to forward it on
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleGlortForward
{
    my ($self, $glort, $gmask, $lagPort) = @_;


    if ( $glort =~ m/^0x/ )
    {
        $glort = hex($glort);
    }

    if ( $gmask =~ m/^0x/ )
    {
        $gmask = hex($gmask);
    }

    print "Val: FM_GLORT_CAM " . ($glort | ($gmask << 16)) . " " . (
                               $glortCAMstart + $glortForwardIndex) . "\n";
    $self->handleRegWriteIndex("FM_GLORT_CAM", $glort | ($gmask << 16),
                               $glortCAMstart + $glortForwardIndex);
    my $destIndex = 0x500 + $lagPort;
    my $numports = 2;
    $self->handleRegWriteIndex("FM_GLORT_RAM", 
                               sprintf("0x%08x", 4 | ($destIndex<<3) | (1<<31)),
                               $glortCAMstart + $glortForwardIndex);
    $glortForwardIndex++;
    return $FM_OK;
}

##@cmethod public int tpHandleSetGlortSpace(int  glort,
#                                           int  mask)
#
# @brief        Configures this chip to have a specific glort space
#
# @param[in]    glort The glort to assign to this chip
#
# @param[in]    glort mask The mask to use to pick out the chip number
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleSetGlortSpace
{
    my ($self, $glort, $gmask, $lagPort) = @_;

    my $chip = $self->{CHIP};

    if ( $glort =~ m/^0x/ )
    {
        $glort = hex($glort);
    }

    if ( $gmask =~ m/^0x/ )
    {
        $gmask = hex($gmask);
    }

    print "Val: FM_GLORT_CAM " . ($glort | ($gmask << 16)) . " " . (
                               $glortCAMstart + $glortForwardIndex) . "\n";
    $self->handleRegWriteIndex("FM_GLORT_CAM", $glort | ($gmask << 16),
                               1);
    for (my $i=1;$i<25;$i++)
    {
        my $regVal;
        $chip->fmReadUINT32(0,
                            $chip->FM_PORT_CFG_ISL($i),
                            \$regVal);
        $chip->fmWriteUINT32(0,
                             $chip->FM_PORT_CFG_ISL($i),
                             $regVal | $glort);
    }
    return $FM_OK;
}

1;
