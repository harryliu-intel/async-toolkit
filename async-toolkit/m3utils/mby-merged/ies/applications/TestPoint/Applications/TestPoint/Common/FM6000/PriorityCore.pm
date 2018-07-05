# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FM6000/PriorityCore.pm
# Creation Date:    May. 27, 2011
# Description:      Handles functions related to configuring different priority
#                   maps on FM6xxx.
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

package Applications::TestPoint::Common::FM6000::PriorityCore;
use strict;
use warnings;

use base qw(
    Exporter
);

use List::Util qw(sum);
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

our @EXPORT = qw(
   tpFM6000HandleShowSwitchPriMap
   tpFM6000HandleSetSwitchMapVpri2ToSwpri
   tpFM6000HandleSetSwitchMapVpri2ToSwpri
   tpFM6000HandleShowPortPriMap
   tpFM6000HandleSetPortMapInpriToVpri
   tpFM6000HandleSetPortSwpriSource
);




#****************************************************************************/
# tpFM6000HandleShowSwitchPriMap
#
# \desc            Show the switch based global priority tables.
#
# \return          NA
#
#****************************************************************************/
sub tpFM6000HandleShowSwitchPriMap
{
    my ($self, $sw) = @_;
    my $chip = $self->{CHIP};
    my $status = $FM_OK;

    printf("\n");
    printf("%5s %5s | %5s %5s | %4s %5s %4s %5s %4s %5s %4s %5s\n", 
           "VPRI1", "SwPri", "VPRI2", "SwPri", 
           "DSCP", "SwPri", "DSCP", "SwPri", "DSCP", "SwPri", "DSCP", "SwPri", );
    printf("%s\n", "-" x 71);

    foreach my $i (0..15)
    {
        my %void = ( type => "fm_int", value => 0 );

#       FIXME:   Need to display traffic classes
#       # Get the SwPri (Switch Priority) to TC (traffic class) mapping column
#
#
#       $status = $chip->fmGetSwitchQOS($sw,
#                                       $FM_QOS_SWPRI_TC_MAP,
#                                       $i,
#                                       \%void);
#
#       if ($status != $FM_OK)
#       {
#           printf("\nError showing SwPri to TC priority maps: %s\n",
#                  $chip->fmErrorMsg($status));
#
#           last $i;
#       }
#       else
#       {
#           printf("%5d %2d | ", $i, $void{value});
#       }

        # Get the VPRI1 to SwPri (Switch Priority) mapping column
        $status = $chip->fmGetSwitchQOS($sw, 
                                        $FM_QOS_L2_VPRI1_TO_ISLPRI_MAP,
                                        $i, 
                                        \%void);
        if ($status != $FM_OK)
        {
            printf("\nError showing priority maps: %s\n", 
                   $chip->fmErrorMsg($status));

            last $i;
        }
        else
        {
            printf("%5d %5d | ", $i, $void{value});
        }       

        # Get the VPRI2 to SwPri (Switch Priority) mapping column
        $status = $chip->fmGetSwitchQOS($sw, 
                                        $FM_QOS_L2_VPRI2_TO_ISLPRI_MAP,
                                        $i, 
                                        \%void);
        if ($status != $FM_OK)
        {
            printf("\nError showing priority maps: %s\n", 
                   $chip->fmErrorMsg($status));

            last $i;
        }
        else
        {
            printf("%5d %5d | ", $i, $void{value});
        }

        # Get the DSCP to SwPri (Switch Priority) mapping column
        $void{type} = "fm_uint32";

        foreach my $j (0..3)
        {
            $status = $chip->fmGetSwitchQOS($sw, 
                                            $FM_QOS_DSCP_SWPRI_MAP,
                                            ($j * 16) + $i, 
                                            \%void);
            if ($status != $FM_OK)
            {
                printf("\nError showing priority maps: %s\n", 
                       $chip->fmErrorMsg($status));

                last $i;
            }
            else
            {
                printf("%4d %5d ", ($j * 16) + $i, $void{value});
            }
        }

        printf("\n");
    }

    printf("\n");

#   FIXME: Need to add Traffic class mapping
#   printf("TC (Traffic Class) to SMP (Shared Memory Partition mapping)\n");
#   printf(" Tc0  Tc1  Tc2  Tc3  Tc4  Tc5  Tc6  Tc7\n");
#   printf("----------------------------------------\n");
#
#   foreach my $i (0..7)
#   {
#       my %void = ( type => "fm_int", value => 0 );
#
#       # Get the SMP (Shared Memory Partition) to TC (traffic class) mapping column
#       $status = $chip->fmGetSwitchQOS($sw,
#                                       $FM_QOS_TC_SMP_MAP,
#                                       $i,
#                                       \%void);
#       if ($status != $FM_OK)
#       {
#           printf("\nError showing SMP priority maps: %s\n",
#                  $chip->fmErrorMsg($status));
#
#           last $i;
#       }
#       else
#       {
#           if($void{value} == $FM_QOS_TC_SMP_NONE)
#           {
#               printf(" NONE");
#           } elsif($void{value} == $FM_QOS_TC_SMP_0)
#           {
#               printf(" SMP0");
#           } elsif($void{value} == $FM_QOS_TC_SMP_1)
#           {
#               printf(" SMP1");
#           } else {
#               printf(" %4d", $void{value});
#           }
#       }
#   }

    printf("\n");
    return $status;
}




sub tpFM6000HandleSetSwitchMapVpri1ToSwpri
{
    my ($self, $vpri, $swpri) = @_;
    my $chip = $self->{CHIP};
    my $status = $FM_OK;

    if (!defined($swpri) || !defined($vpri))
    {
        print("Must specify <vpri> <swpri>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (($swpri < 0) || ($swpri > 15))
    {
        printf("Must specify valid switch priority!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @vpriValues = $self->validateList($vpri, 0, 15);

    foreach my $sw ($self->tpGetSwitches)
    {
        foreach my $vpriValue (@vpriValues)
        {
            if (($vpriValue < 0) || ($vpriValue > 15))
            {
                printf("Must specify valid internal vlan priority ($vpriValue is invalid)!\n");
                return $FM_ERR_INVALID_ARGUMENT;
            }

            my %void = ( type => "fm_int", value => $swpri );

            $status = $chip->fmSetSwitchQOS($sw, $FM_QOS_L2_VPRI1_TO_ISLPRI_MAP, 
                                            $vpriValue, \%void);
        }
    }
}




sub tpFM6000HandleSetSwitchMapVpri2ToSwpri
{
    my ($self, $vpri, $swpri) = @_;
    my $chip = $self->{CHIP};
    my $status = $FM_OK;

    if (!defined($swpri) || !defined($vpri))
    {
        print("Must specify <vpri> <swpri>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (($swpri < 0) || ($swpri > 15))
    {
        printf("Must specify valid switch priority!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @vpriValues = $self->validateList($vpri, 0, 15);

    foreach my $sw ($self->tpGetSwitches)
    {
        foreach my $vpriValue (@vpriValues)
        {
            if (($vpriValue < 0) || ($vpriValue > 15))
            {
                printf("Must specify valid internal vlan priority ($vpriValue is invalid)!\n");
                return $FM_ERR_INVALID_ARGUMENT;
            }

            my %void = ( type => "fm_int", value => $swpri );

            $status = $chip->fmSetSwitchQOS($sw, $FM_QOS_L2_VPRI2_TO_ISLPRI_MAP, 
                                            $vpriValue, \%void);
        }
    }
}




#****************************************************************************/
#* tpFM6000HandleShowPortPriMap
#
# \desc            Show the port based global priority tables.
#
# \return          NA
#
#****************************************************************************/
sub tpFM6000HandleShowPortPriMap
{
    my ($self, $sw, $logPort, $port) = @_;
    my $chip = $self->{CHIP};
    my $status = $FM_OK;

    printf("\nPort %02d\n", $port);
    printf("---------\n");

    my %void = ( type => "fm_uint32", value => 0 );
    my %pref = ( type => "fm_bool", value => 0 );

    printf("Switch priority source (in preference order): ");

    $status = $chip->fmGetPortAttribute($sw, $logPort, $FM_PORT_SWPRI_SOURCE, \%void);

    if ($status != $FM_OK)
    {
        printf("\nERROR: unable to get port priority source: %s\n",
               $chip->fmErrorMsg($status));

        return $status;
    }

    $status = $chip->fmGetPortAttribute($sw, $logPort, $FM_PORT_SWPRI_DSCP_PREF, \%pref);

    if ($status != $FM_OK)
    {
        printf("\nERROR: unable to get port priority DSCP preference: %s\n",
               $chip->fmErrorMsg($status));

        return $status;
    }

    my $s1 = "";
    my $s2 = "";
    my $s3 = "";
    my $s4 = "";

    if ($void{value} & $FM_PORT_SWPRI_ISL_TAG)
    {
        $s1 = "ISL Tag";
    }

    if ($void{value} & $FM_PORT_SWPRI_DSCP)
    {
        $s2 = "DSCP field";
    }

    if ($void{value} & $FM_PORT_SWPRI_VPRI1)
    {
        $s3 = "VPRI1";
    }

    if ($void{value} & $FM_PORT_SWPRI_VPRI2)
    {
        $s4 = "VPRI2";
    }

    my @tmp = (); 
    push (@tmp, $s1);

    if ($pref{value})
    {
        push (@tmp, $s2);
        push (@tmp, $s4);
        push (@tmp, $s3);
    }
    else
    {
        push (@tmp, $s4);
        push (@tmp, $s3);
        push (@tmp, $s2);
    }
    
    printf("%s\n\n", join(" , ", @tmp));

    printf("%8s %8s | %8s %8s\n", "Ingress", "Internal", "Ingress", "Internal"); 
    printf("%8s %8s | %8s %8s\n", "VPRI1", "VPRI1", "VPRI2", "VPRI2"); 
    printf("%s\n", "-" x 37);

    foreach my $i (0..15)
    {
        $void{type} = "fm_int";

        $status = $chip->fmGetPortQOS($sw, $logPort, $FM_QOS_L2_VPRI1_MAP, $i, \%void);

        if ($status != $FM_OK)
        {
            printf("\nERROR: unable to get port priority source (for VPRI1): %s\n",
                   $chip->fmErrorMsg($status));

            return $status;
        }

        printf("%8d %8d | ", $i, $void{value});

        $status = $chip->fmGetPortQOS($sw, $logPort, $FM_QOS_L2_VPRI2_MAP, $i, \%void);

        if ($status != $FM_OK)
        {
            printf("\nERROR: unable to get port priority source (for VPRI2): %s\n",
                   $chip->fmErrorMsg($status));

            return $status;
        }

        printf("%8d %8d\n", $i, $void{value});
    }

    printf("\n");

    return $status;
}




sub tpFM6000HandleSetPortMapInpriToVpri
{
    my ($self, $vpritag, $port, $inpri, $vpri) = @_;
    my $chip = $self->{CHIP};
    my $vpriAttribute;
    my $status = $FM_OK;

    if (!defined($vpritag) || !defined($port) || !defined($inpri) || !defined($vpri))
    {
        printf("Must specify <vpri-tag> <port> <ingress-pri> <vpri>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if ($vpritag eq "vpri1")
    {
        $vpriAttribute = $FM_QOS_L2_VPRI1_MAP;
    }
    elsif ($vpritag eq "vpri2")
    {
        $vpriAttribute = $FM_QOS_L2_VPRI2_MAP;
    }
    else
    {
        printf("vpri-tag may only take one of these values: \'vpri1\' or \'vpri2\'!\n");
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
                my %void = ( type => "fm_uint32", value => $vpri );

                $status = $chip->fmSetPortQOS($sw, 
                                              $logPort, 
                                              $vpriAttribute,
                                              $inpriValue, 
                                              \%void);

                if ($status != $FM_OK)
                {
                    printf("ERROR: unable to set map: %s\n",
                           $chip->fmErrorMsg($status));

                    return $status;
                }
                
            }   # end foreach my $inpriValue (@inpriValues)
            
        }   # foreach my $port (@portList)
        
    }   # foreach my $sw ($self->tpGetSwitches)
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    
    return $status;
}




sub tpFM6000HandleSetPortSwpriSource
{
    my ($self, $sw, $port, $source, $state) = @_;
    my $chip = $self->{CHIP};
    my $status = $FM_OK;

    if (($source ne "dscp") &&
        ($source ne "vlan") &&
        ($source ne "vlan2") &&
        ($source ne "isl"))
    {
        printf("Must specify valid source ($source)!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (($state ne "on") && 
        ($state ne "off") && 
        ($state ne "preferred"))
    {
        printf("Must specify valid state!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (($state eq "preferred") &&
        (($source eq "dscp") || ($source eq "vlan") || ($source eq "vlan2")))
    {
        my %pref = ( type => "fm_bool", 
                     value => (($source eq "dscp") ?  1 : 0) );

        $status = $chip->fmSetPortAttribute($sw, $port,
                                            $FM_PORT_SWPRI_DSCP_PREF, 
                                            \%pref);

        if ($status != $FM_OK)
        {
            printf("ERROR: unable to set source preference: %s\n",
                   $chip->fmErrorMsg($status));
        }
    }
    else
    {
        my %void = ( type => "fm_uint32", value => 0 );

        $status = $chip->fmGetPortAttribute($sw, $port, 
                                            $FM_PORT_SWPRI_SOURCE, 
                                            \%void);

        if ($status != $FM_OK)
        {
            printf("ERROR: unable to get current source: %s\n",
                   $chip->fmErrorMsg($status));
        }

        my $mask = 0;
        my $currentSource = $void{value};

        if ($source eq "dscp")
        {
            $mask |= $FM_PORT_SWPRI_DSCP;
        }
        elsif ($source eq "vlan")
        {
            $mask |= $FM_PORT_SWPRI_VPRI1;
        }
        elsif ($source eq "vlan2")
        {
            $mask |= $FM_PORT_SWPRI_VPRI2;
        }
        elsif ($source eq "isl")
        {
            $mask |= $FM_PORT_SWPRI_ISL_TAG;
        }

        if ($state eq "off")
        {
            $currentSource &= ~$mask;
        }
        elsif ($state eq "on")
        {
            $currentSource |= $mask;
        }

        $void{value} = $currentSource;

        $status = $chip->fmSetPortAttribute($sw, $port, 
                                            $FM_PORT_SWPRI_SOURCE, 
                                            \%void);

        if ($status != $FM_OK)
        {
            printf("ERROR: unable to set source: %s\n",
                   $chip->fmErrorMsg($status));
        }
    }
    
}

1;
