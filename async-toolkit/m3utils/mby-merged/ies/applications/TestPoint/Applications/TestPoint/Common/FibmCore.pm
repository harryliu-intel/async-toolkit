# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FibmCore.pm
# Creation Date:    11/2/08
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

package Applications::TestPoint::Common::FibmCore;
use strict;
use warnings;

use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

#These two functions below can move to a common fibm code
##@cmethod public int[] handleInsertSwitch(int sw)
#
# @desc         Trigger an switch insertion event 
#
# @return       None 
sub handleInsertSwitch
{
    my ($self,$sw) = @_;
    my $chip = $self->{CHIP};

    printf("Inserting switch $sw ...\n");
    $chip->fm4000FibmAddSlaveSwitch($sw);
    
}

##@cmethod public int[] handleRemoveSwitch(int sw)
#
# @desc         Trigger an switch removal event 
#
# @return       None 
sub handleRemoveSwitch
{
    my ($self,$sw) = @_;
    my $chip = $self->{CHIP};

    printf("Removing switch $sw ...\n");
    $chip->fm4000FibmRemoveSlaveSwitch($sw);
    
}

##@cmethod public int[] handleShowFibmStats(int sw)
#
# @desc         Show fibm statistics for a given switch
#
# @return       None 
sub handleShowFibmStats
{
    my ($self,$sw) = @_;
    my $chip = $self->{CHIP};
    my $stats = new SDK::fm_fibmStats();

    if (!defined($sw))
    {
        $sw = "0..$FM_MAX_NUM_SWITCHES";
    }

    my @swList = $self->validateList($sw, 0, $FM_MAX_NUM_SWITCHES);
    if (!defined($sw) || (scalar(@swList) == 0))
    {
        print("Must specify a valid switch or switch range!\n");
        return;
    }

    foreach my $s (@swList)
    {
        my $status = $chip->fmFibmGetStats($s, $stats);

        if ($status != $FM_OK)
        {
            printf("ERROR: Switch #%d: %d=%s\n", $s, $status,
                   $chip->fmErrorMsg($status));
            return $status;
        }

        printf("\nSwitch #$s Fibm Stats:\n");

        printf("  read:               %u\n",$stats->{'read'});
        printf("  readMulti:          %u\n",$stats->{'readMulti'});
        printf("  read64:             %u\n",$stats->{'read64'});
        printf("  readMulti64:        %u\n",$stats->{'readMulti64'});
        printf("  readScatterGather:  %u\n",$stats->{'readSg'});
        printf("  write:              %u\n",$stats->{'write'});
        printf("  writeMulti:         %u\n",$stats->{'writeMulti'});
        printf("  write64:            %u\n",$stats->{'write64'});
        printf("  writeMulti64:       %u\n",$stats->{'writeMulti64'});
        printf("  writeScatterGather: %u\n",$stats->{'writeSg'});
        printf("  mask:               %u\n",$stats->{'mask'});

        printf("\n");
        printf("  intrRx:             %u\n",$stats->{'intrRx'});
        printf("  request:            %u\n",$stats->{'request'});
        printf("  response:           %u\n",$stats->{'response'});
        printf("  resendOnce:         %u\n",$stats->{'resendOnce'});
        printf("  resendTwiceOrMore:  %u\n",$stats->{'resendTwiceOrMore'});
        printf("  staleRxPkts:        %u\n",$stats->{'staleRxPkts'});
        printf("  sendFail:           %u\n",$stats->{'sendFail'});
        printf("  unexpectedTag:      %u\n",$stats->{'unexpectedTag'});
    }

}

##@cmethod public int[] handleResetFibmStats(int sw)
#
# @desc         Show fibm statistics for a given switch
#
# @return       None 
sub handleResetFibmStats
{
    my ($self,$sw) = @_;
    my $chip = $self->{CHIP};

    if (!defined($sw))
    {
        $sw = "0..$FM_MAX_NUM_SWITCHES";
    }

    my @swList = $self->validateList($sw, 0, $FM_MAX_NUM_SWITCHES);
    if (!defined($sw) || (scalar(@swList) == 0))
    {
        print("Must specify a valid switch or switch range!\n");
        return;
    }

    foreach my $s (@swList)
    {
        my $status = $chip->fmFibmResetStats($s);
        if ($status != $FM_OK)
        {
            printf("ERROR: Switch #%d: %d=%s\n", $s, $status,
                   $chip->fmErrorMsg($status));
        }
    }

}


1;
