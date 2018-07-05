# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FM2000/PriorityCore.pm
# Creation Date:    Jan. 26, 2008
# Description:      Handles functions related to configuring different priority
#                   maps on FM2xxx.
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

package Applications::TestPoint::Common::FM2000::PriorityCore;
use strict;
use warnings;

use base qw(
    Exporter
);

use List::Util qw(sum);
use SDK qw(/^\$/);
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

our @EXPORT = qw(
    tpFM2000HandleShowSwitchPriMap
    tpFM2000HandleShowPortPriMap
);

sub tpFM2000HandleSetPortMapInpriToSwpri
{
    my ($self, $port, $inpri, $swpri) = @_;
    my $chip = $self->{CHIP};
    my $status = $FM_OK;

    if (!defined($port) || !defined($inpri) || !defined($swpri))
    {
        printf("Must specify <port> <ingress-pri> <swpri>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, 
                                    $self->tpPlatformGetFaceplatePortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @inpriValues = $self->validateList($inpri, 0, 15);

    if (scalar(@inpriValues) == 0)
    {
        printf("Must specify valid ingress priority list!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, 
                                    $self->tpPlatformGetSwitchPortList($sw, 
                                                                       $TRUE));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            
            foreach my $inpriValue (@inpriValues)
            {
                my %void = ( type => "fm_uint32", value => $swpri );

                $status = $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_RX_PRIORITY_MAP,
                                              $inpriValue, \%void);

                if ($status != $FM_OK)
                {
                    printf("ERROR: unable to set map: %s\n",
                           $chip->fmErrorMsg($status));

                    return $status;
                }
                
            }   # end foreach my $inpriValue (@inpriValues)
            
        }   # end foreach my $port (@portList)
        
    }   # end foreach my $sw ($self->tpGetSwitches)
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);

    return $status;
}

sub tpFM2000HandleSetPortMapSwpriToEgpri
{
    my ($self, $port, $swpri, $egpri) = @_;
    my $chip = $self->{CHIP};
    my $status = $FM_OK;

    if (!defined($port) || !defined($swpri) || !defined($egpri))
    {
        printf("Must specify <port> <swpri> <egress-pri>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, 
                                    $self->tpPlatformGetFaceplatePortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @swpriValues = $self->validateList($swpri, 0, 15);

    if (scalar(@swpriValues) == 0)
    {
        printf("Must specify valid internal VLAN priority list!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, 
                                    $self->tpPlatformGetSwitchPortList($sw,
                                                                       $TRUE));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            
            foreach my $swpriValue (@swpriValues)
            {
                my %void = ( type => "fm_uint32", value => $egpri );

                $status = $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_TX_PRIORITY_MAP,
                                              $swpriValue, \%void);

                if ($status != $FM_OK)
                {
                    printf("ERROR: unable to set map: %s\n",
                           $chip->fmErrorMsg($status));

                    return $status;
                }
                
            }   # end foreach my $swpriValue (@swpriValues)
            
        }   # end foreach my $port (@portList)
        
    }   # end foreach my $sw ($self->tpGetSwitches)
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);

    return $status;
}

sub tpFM2000HandleShowPortPriMap
{
    my ($self, $sw, $logPort, $port) = @_;
    my $chip = $self->{CHIP};
    my $status = $FM_OK;

    printf("Port %02d\n", $port);
    printf("---------\n");

    my %void = ( type => "fm_uint32", value => 0 );

    printf("\n");

    printf("%12s %5s | %5s %11s\n", "Ingress Pri.", "SwPri", "SwPri", "Egress Pri."); 
    printf("%s\n", "-" x 36);

    foreach my $i (0..15)
    {
        $void{type} = "fm_int";

        if ($i < 8)
        {
            $status = $chip->fmGetPortQOS($sw, $logPort, $FM_QOS_RX_PRIORITY_MAP, $i, \%void);

            if ($status != $FM_OK)
            {
                printf("\nERROR: unable to get port priority map: %s\n",
                       $chip->fmErrorMsg($status));

                return $status;
            }

            printf("%12d %5d | ", $i, $void{value});
        }
        else
        {
            printf("%s | ", " " x 18);
        }

        $status = $chip->fmGetPortQOS($sw, $logPort, $FM_QOS_TX_PRIORITY_MAP, $i, \%void);

        if ($status != $FM_OK)
        {
            printf("\nERROR: unable to get port priority map: %s\n",
                   $chip->fmErrorMsg($status));

            return $status;
        }

        printf("%5d %11d\n", $i, $void{value});
    }

    printf("\n");

    return $status;
}


sub tpFM2000HandleShowSwitchPriMap
{
    my ($self, $sw) = @_;
    my $chip = $self->{CHIP};

    printf("\n");
    printf("%5s %s\n", "SwPri", "TC");
    printf("%s\n", "-" x 8);

    foreach my $swpri (0..15)
    {
        my %void = ( type => "fm_int", value => 0 );
        my $status = $FM_OK;

        $status = $chip->fmGetSwitchQOS($sw, 
                                        $FM_QOS_SCHEDULER_PRIORITY,
                                        $swpri, 
                                        \%void);

        if ($status != $FM_OK)
        {
            printf("\nError showing priority maps: %s\n", 
                   $chip->fmErrorMsg($status));

            last $swpri;
        }
        else
        {
            printf("%5d %d\n", $swpri, $void{value});
        }
    }

    printf("\n");

    return $FM_OK;
}

1;
