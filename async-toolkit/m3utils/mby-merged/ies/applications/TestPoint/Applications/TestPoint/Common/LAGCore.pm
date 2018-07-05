# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/LAGCore.pm
# Creation Date:    04/06/07
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

package Applications::TestPoint::Common::LAGCore;
use strict;
use warnings;

use base qw(Exporter);

use List::Util qw(sum);
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;
our @EXPORT = qw(
    tpHandleAddLagPort
    tpHandleCreateLag
    tpHandleDelLag
    tpHandleDelLagPort
    tpHandleResetLag
    tpHandleShowLag
);
our @EXPORT_OK = qw(
    _tpIsValidLAG
);

##@cmethod public void tpHandleAddLagPort(int globalLag, char *port)
#
# @desc         Handles adding a set of ports to a link-aggregation group (LAG)
#
# @param[in]    globalLag The system wide logical LAG number the set ports are
#               to be added to
#
# @param[in]    port The set of ports to add
sub tpHandleAddLagPort
{
    my ($self, $globalLag, $port) = @_;

    my $chip = $self->{CHIP};
    my @affectedPortList = ();
    my @globalPortList;
    
    $globalLag = $self->str2intnum($globalLag);
    if (!defined($globalLag))
    {
        print($TP_MSG_ERR_LAG_INVALID_SCALAR);
        return;
    }
    $chip->disableErrors();
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my ($sw, $logicalLag) =
                               $self->tpPlatformMapGlobalToLogicalLAG($globalLag);
                               
        if (scalar(grep {$switchNum == $_} $self->tpGetSwitches() ) == 0 )
        {
            printf("The lag $globalLag does not belong to the selected switches\n");
            return;
        }
        if (!$self->_tpIsValidLAG($switchNum, $logicalLag))
        {
            print($TP_MSG_ERR_LAG_INVALID_ID);
            return;
        }
    
        @globalPortList = $self->validateList($port, 
                                        $self->tpPlatformGetPortRange());
        if (!defined($port) || (scalar(@globalPortList) == 0))
        {
            print($TP_MSG_ERR_PORT_INVALID_ARRAY);
            return $FM_ERR_INVALID_ARGUMENT;
        }
        my @indices = arrayFind(map {$self->tpPlatformIsCPUPort($_)} @globalPortList);
        if ($port ne "all" && scalar(@indices) > 0)
        {
            print($TP_MSG_ERR_PORT_INVALID_ARRAY);
            return $FM_ERR_INVALID_ARGUMENT;
        }
    
        my @portList = $self->validateExplicitList($TRUE, $port, 
                                $self->tpPlatformGetSwitchPortList($switchNum, 
                                                                   $FALSE));
        
        if (scalar(@portList) == 0)
        {
            printf("None of the ports appear on the switch the lag $globalLag belongs to\n");
            return;
        }
        
        push(@affectedPortList, @portList);
    
        foreach my $port (@portList)
        {
            next if $self->tpPlatformIsCPUPort($port);
    
            my ($sw, $logicalPort) =
                              $self->tpPlatformMapGlobalToLogicalPort($port);
    
            $chip->disableErrors();
            my $status = $chip->fmAddLAGPort($switchNum, $logicalLag, $logicalPort);
            $chip->enableErrors();
            if ($status == $FM_ERR_ALREADYUSED_PORT)
            {
                printf("Port %d is already a member of LAG %d!\n",
                       $port, $globalLag);
            }
            elsif ($status == $FM_ERR_FULL_LAG)
            {
                printf("LAG %d is full!\n", $globalLag);
                return;
            }
            elsif ($status != $FM_OK)
            {
                printf("Port %d cannot be added to LAG %d!\n",
                       $port, $globalLag);
                return;
            }
        }
    }
    $chip->enableErrors();
   
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
}

##@cmethod public void tpHandleCreateLag(void)
#
# @desc         Handles creating a set of link-aggregation groups (LAGs)
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleCreateLag
{
    my ($self) = @_;

    my $chip = $self->{CHIP};

    my $lags = [];
    my $previousLag = undef;
    $chip->disableErrors();
    foreach my $sw ($self->tpGetSwitches)
    {
        my ($status);

        $status = $chip->fmCreateLAGExt($sw, \$lags->[$sw]);
        if ($status != $FM_OK
            || (defined($previousLag) && $lags->[$sw] != $previousLag))
        {
            goto ABORT;
        }
        $previousLag = $lags->[$sw];
        printf("Switch $sw: LAG logical port number %d created\n", $previousLag);
    }
    $chip->enableErrors();

    return $FM_OK;

ABORT:
    print("Cannot create LAG!\n");
    foreach my $sw ($self->tpGetSwitches)
    {
        if (defined($lags->[$sw]))
        {
            $chip->fmDeleteLAGExt($sw, $lags->[$sw]);
        }
    }
    $chip->enableErrors();
    return $FM_FAIL;

}   # end tpHandleCreateLag


##@cmethod public void tpHandleDelLag(char *lag)
#
# @desc         Handles deleting a set of link-aggregation groups (LAGs)
#
# @param[in]    lag The set of LAGs to be deleted
sub tpHandleDelLag
{
    my ($self, $lag) = @_;

    my $chip = $self->{CHIP};

    if (!defined($lag))
    {
        print($TP_MSG_ERR_LAG_INVALID_ARRAY);
        return;
    }

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @list = $self->tpPlatformGetLogicalPortList($switchNum, "lag");
        my @lagList = $self->validateExplicitList($FALSE, $lag, @list);

        if (scalar(@lagList) == 0)
        {
            print($TP_MSG_ERR_LAG_INVALID_ARRAY);
            return;
        }

        foreach my $globalLag (@lagList)
        {
            my ($sw, $logicalLag) =
                                $self->tpPlatformMapGlobalToLogicalLAG($globalLag);
    
            if (scalar(grep {$switchNum == $_} $self->tpGetSwitches() ) == 0 )
            {
                printf("The lag $globalLag does not belong to the selected switches\n");
                next;
            }
    
            if (!$self->_tpIsValidLAG($switchNum, $logicalLag))
            {
                next;
            }

            $chip->disableErrors();
            my $status = $chip->fmDeleteLAGExt($switchNum, $logicalLag);
            $chip->enableErrors();
            if ($status != $FM_OK)
            {
                printf("LAG %d cannot be deleted!\n", $globalLag);
            }
        }
    }
}

##@cmethod public void tpHandleDelLagPort(int globalLag, char *port)
#
# @desc         Handles deleting a set of ports from a link-aggregation group
#               (LAG)
#
# @param[in]    globalLag The LAG from the set of ports are to be deleted
#
# @param[in]    port The set of ports to be deleted
sub tpHandleDelLagPort
{
    my ($self, $globalLag, $port) = @_;

    my $chip = $self->{CHIP};
    my @affectedPortList = ();
    my @globalPortList; 

    $globalLag = $self->str2intnum($globalLag);
    if (!defined($globalLag))
    {
        print($TP_MSG_ERR_LAG_INVALID_SCALAR);
        return;
    }

    $chip->disableErrors();
    
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my ($sw, $logicalLag) =
                                $self->tpPlatformMapGlobalToLogicalLAG($globalLag);
    
        if (scalar(grep {$switchNum == $_} $self->tpGetSwitches() ) == 0 )
        {
            printf("The lag $globalLag does not belong to the selected switches\n");
            return;
        }
    
        if (!$self->_tpIsValidLAG($switchNum, $logicalLag))
        {
            print($TP_MSG_ERR_LAG_INVALID_ID);
            return;
        }
    
        @globalPortList = $self->validateList($port, 
                                        $self->tpPlatformGetPortRange());
        if (!defined($port) || (scalar(@globalPortList) == 0))
        {
            print($TP_MSG_ERR_PORT_INVALID_ARRAY);
            return;
        }
        my @indices = arrayFind(map {$self->tpPlatformIsCPUPort($_)} @globalPortList);
        if ($port ne "all" && scalar(@indices) > 0)
        {
            print($TP_MSG_ERR_PORT_INVALID_ARRAY);
            return $FM_ERR_INVALID_ARGUMENT;
        }
    
        my @portList = $self->validateExplicitList($TRUE, $port, 
                                $self->tpPlatformGetSwitchPortList($switchNum, 
                                                                   $FALSE));
        
        if (scalar(@portList) == 0)
        {
            printf("None of the ports appear on the switch the lag $globalLag belongs to\n");
            return;
        }
        
        push(@affectedPortList, @portList);
    
        foreach my $port (@portList)
        {
            next if $self->tpPlatformIsCPUPort($port);
    
            my ($sw, $logicalPort) =
                              $self->tpPlatformMapGlobalToLogicalPort($port);
    
            $chip->fmDeleteLAGPort($switchNum, $logicalLag, $logicalPort);
        }
    }        
    $chip->enableErrors();

    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
}

##@cmethod public void tpHandleResetLag(void)
#
# @desc         Handles deleting all link-aggregation groups
sub tpHandleResetLag
{
    my ($self) = @_;

    my $chip = $self->{CHIP};

    $chip->disableErrors();
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $lagNumbers = [(0) x $FM_MAX_NUM_LAGS];
        my ($nLAG);

        $chip->fmGetLAGListExt($switchNum, \$nLAG, $lagNumbers,
                               $FM_MAX_NUM_LAGS);
        for (my $i = 0; $i < $nLAG; $i++)
        {
            $chip->fmDeleteLAGExt($switchNum, $lagNumbers->[$i]);
        }
    }
    $chip->enableErrors();
}


##@cmethod public void tpHandleShowLag(char *lag)
#
# @desc         Handles showing a set of link aggegration groups (LAG)
sub tpHandleShowLag
{
    my ($self, $lag) = @_;

    my $chip = $self->{CHIP};
    my @membership = ();

    if (!defined($lag))
    {
        print($TP_MSG_ERR_LAG_INVALID_ARRAY);
        return;
    }

    printf("%-4s %-4s %-4s %-s\n", "LAG", "SW", "LPRT", "MEMBERSHIP");
    print("------------------------------------------------------------\n");

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @list = $self->tpPlatformGetLogicalPortList($switchNum, "lag");
        if (scalar(@list) == 0)
        {
            print("There are no LAGs configured!\n");
            return;
        }
        my @lagList = $self->validateExplicitList($FALSE, $lag, @list);

        if (scalar(@lagList) == 0)
        {
            print($TP_MSG_ERR_LAG_INVALID_ARRAY);
            return;
        }

        foreach my $globalLag (@lagList)
        {
            @membership = ();
            my $ports = [(0) x $FM_MAX_NUM_LAG_MEMBERS];

            my ($sw, $logicalLag) =
                                $self->tpPlatformMapGlobalToLogicalLAG($globalLag);
            my ($numPorts);
    
            if (!$self->_tpIsValidLAG($switchNum, $logicalLag))
            {
                next;
            }
            
            $chip->disableErrors();
            my $status = $chip->fmGetLAGPortListExt($switchNum, $logicalLag,
                                                    \$numPorts, $ports,
                                                    $FM_MAX_NUM_LAG_MEMBERS);
            $chip->enableErrors();

            for (my $i = 0; $i < $numPorts; $i++)
            {
                my $faceplatePort =
                          $self->tpPlatformMapLogicalToFaceplatePort($switchNum,
                                                                     $ports->[$i]);
                push(@membership, $faceplatePort);
            }
    
            my $n = scalar(@membership);
            # Prevent all LAGs that only contain internal ports, e.g.
            # crosslinks, from being shown.
            if ($n == 0 ||
                ($n > 0 && sum(map {int($_ == -1)} @membership) != $n))
            {
                printf("%-4d ", $globalLag);
                printf("%-4d ", $switchNum);
                printf("%-4d ", $logicalLag);
                if ($n > 0)
                {
                    map {
                        if ($_ >= 0)
                        {
                            printf("%-2d ", $_);
                        }
                        else
                        {
                            # This is an internal port, e.g. a crosslink,
                            # which is not being shown.
                        }
                    } sort {$a <=> $b} @membership;
                    print("\n");
                }
                else
                {
                    print("No members\n");
                }
            }
        }
    }
}

##@cmethod private bool _tpIsValidLAG(int switchNum, int lag)
#
# @desc         Determines whether a LAG has been previously configured
#
# @param[in]    switchNum The switch to be inquired
#
# @param[in]    lag The LAG whose status is to be determined
#
# @return       TRUE if the LAG has been previously configured
# @return       FALSE otherwise
sub _tpIsValidLAG
{
    my ($self, $switchNum, $lag) = @_;

    my $chip = $self->{CHIP};

    my ($currentLag, $nextLag);

    $chip->disableErrors();
    $chip->fmGetLAGFirstExt($switchNum, \$currentLag);
    while ($currentLag != -1)
    {
        if ($currentLag == $lag)
        {
            return $TRUE;
        }
        $chip->fmGetLAGNextExt($switchNum, $currentLag, \$nextLag);
        $currentLag = $nextLag;
    }
    $chip->enableErrors();
    return $FALSE;
}

1;
