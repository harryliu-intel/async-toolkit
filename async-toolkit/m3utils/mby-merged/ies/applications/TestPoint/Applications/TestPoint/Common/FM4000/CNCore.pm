# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FM4000/CNCore.pm
# Creation Date:    11/20/07
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

package Applications::TestPoint::Common::FM4000::CNCore;
use strict;
use warnings;

use base qw();

use List::Util qw(sum);
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;
use Math::BigInt;

##@cmethod void tpHandleSetCN(int    sw, char* cmd, ... arguments)
# @brief        Set Congestion Notification Properties
# 
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpFM4000HandleSetCN
{

    my ($self, @args) = @_;

    print "tpFM4000HandleSetCN, args: @args\n";

    return $FM_OK;

}

##@cmethod 
sub tpFM4000HandleShowCN
{
    my ($self, @args) = @_;

    print "tpFM4000HandleShowCN args: @args\n";   

    if (scalar @args < 1)
    {
        print "missing argument\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $cmd = shift @args; 
    if ($cmd eq "mode")
    {
        print "show mode not yet implemented\n";
        return $FM_FAIL;
    }
    else
    {

        print "unrecognized command <$cmd>\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    return $FM_FAIL;
}


###############################################################################
#
#                           LOCAL VARIABLES
#
###############################################################################


#TODO need an array of switch data structures for per switch global 
#cn parameters

###############################################################################
#
#                           LOCAL FUNCTIONS
#
###############################################################################

sub SetCNMode
{
    my ($self, @args) = @_;
    
    print "SetCNMode\n";
    print "@args\n";

    print "not yet implemented\n";
    return $FM_FAIL;
}




# TODO An apply function to apply all of the CN configuration
# including the per port configuration
# when enabling CN, the configuration order should be
# 1. port boundaries
# 2. addresses CPID, VCN DMACs etc
# 3. watermarks
# 4. set mode bit to turn on CN
#
# when disabling CN, the order should be
# 1. set mode bit to turn off CN
# XXX do we need to clear the watermarks, addresses and port boundaries?
#



1;
