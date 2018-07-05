# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/PriorityCore.pm
# Creation Date:    Jan. 26, 2008
# Description:      Handles functions related to configuring different priority
#                   maps.
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

package Applications::TestPoint::Common::PriorityCore;
use strict;
use warnings;

#Loaded when chip family is detected
#use base qw(
#    Exporter
#    Applications::TestPoint::Common::FM2000::PriorityCore
#    Applications::TestPoint::Common::FM4000::PriorityCore
#    Applications::TestPoint::Common::FM6000::PriorityCore
#);

use List::Util qw(sum);
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

our @EXPORT = qw(
    tpHandleShowSwitchPriMap
    tpHandleShowPortPriMap
    tpHandleSetSwitchMapSwpriToTc
    tpHandleSetSwitchMapSMPToTc
    tpHandleSetPortSwpriSource
    tpHandleSetSwitchMapDscpToSwpri
);


#****************************************************************************/
#* tpHandleShowSwitchPriMap
#
# @desc            Show the Switch global priority mapping tables.
#
# @return          NA
#
#****************************************************************************/
sub tpHandleShowSwitchPriMap
{
    my ($self) = @_;
    my $chip = $self->{CHIP};

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $sw ($self->tpGetSwitches)
    {
        if ($switchCount > 1)
        {
            print "Switch $sw:\n";
        }
        
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);

        # FIXME: Need to handle FM_SWITCH_FAMILY_SWAG for both FM4000 and FM6000
        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
        {
            $self->tpFM6000HandleShowSwitchPriMap($sw);
        }
        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000 ||
               $info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
        {
            $self->tpFM4000HandleShowSwitchPriMap($sw);
        }
        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000)
        {
            $self->tpFM2000HandleShowSwitchPriMap($sw);
        }
    }
}

#****************************************************************************/
#* tpHandleShowPortPriMap
#
# @desc            Show the port based global priority tables.
#
# @return          NA
#
#****************************************************************************/
sub tpHandleShowPortPriMap
{
    my ($self, $port) = @_;
    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, 
                                    $self->tpPlatformGetFaceplatePortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, 
                                    $self->tpPlatformGetSwitchPortList($switchNum,
                                                                       $TRUE));

        if (scalar(@portList) == 0)
        {
            next;
        }
    
        push(@affectedPortList, @portList);

        if (scalar($self->tpGetSwitches) > 1)
        {
            printf("\nSwitch $switchNum:\n");
        }
        
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($switchNum, $info);

        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned sw since it should match switchNum and may actually be
            # wrong for some platforms.
            my ($sw, $logicalPort) =
                                $self->tpPlatformMapGlobalToLogicalPort($port);

            # FIXME: Need to handle FM_SWITCH_FAMILY_SWAG for both FM4000 and FM6000
            if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
            {
                $self->tpFM6000HandleShowPortPriMap($switchNum, $logicalPort, $port);
            }
            elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000 ||
                   $info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
            {
                $self->tpFM4000HandleShowPortPriMap($switchNum, $logicalPort, $port);
            }
            elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000)
            {
                $self->tpFM2000HandleShowPortPriMap($switchNum, $logicalPort, $port);
            }
            
        }   # end foreach my $port (@portList)
        
    }   # end foreach my $sw ($self->tpGetSwitches)
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    
}

#****************************************************************************/
# tpHandleSetSwitchMapSwpriToTc
#
# @desc            
#
# @return          NA
#
#****************************************************************************/
sub tpHandleSetSwitchMapSwpriToTc
{
    my ($self, $swpri, $tc) = @_;
    my $chip = $self->{CHIP};
    my $status = $FM_OK;

    if (!defined($swpri) || !defined($tc))
    {
        print("Must specify <swpri> <tc>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @swpriValues = $self->validateList($swpri, 0, 15);

    foreach my $sw ($self->tpGetSwitches)
    {
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);

        # Chip specific attributes and bounds
        my $attrName = "";
        my $maxTc = 0;

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000 || 
            $info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
        {
            $attrName = $FM_QOS_SWPRI_TC_MAP;
            $maxTc = 7;
        }
        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000)
        {
            $attrName = $FM_QOS_SCHEDULER_PRIORITY;
            $maxTc = 3;
        }

        if (($tc < 0) || ($tc > $maxTc))
        {
            printf("Must specify valid traffic class!\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }

        foreach my $swpriValue (@swpriValues)
        {
            if (($swpriValue < 0) || ($swpriValue > 15))
            {
                printf("Must specify valid switch priority ($swpriValue is invalid)!\n");
                return $FM_ERR_INVALID_ARGUMENT;
            }

            my %void = ( type => "fm_int", value => $tc );

            $status = $chip->fmSetSwitchQOS($sw, $attrName,
                                            $swpriValue, \%void);
        }
    }
}

#****************************************************************************/
# tpHandleSetSwitchMapSMPToTc
#
# @desc             Map the Shared Memory Partition (SMP) that each
#                   Traffic Class (TC) belongs to.
#
# @param[in]        tc - Traffic Class (0 - 7)
#
# @param[in]        smp - Shared Memory Partition (0 or 1)
#
# @return          NA
#
#****************************************************************************/
sub tpHandleSetSwitchMapSMPToTc
{
    my ($self, $tc, $smp) = @_;
    my $chip = $self->{CHIP};
    my $status = $FM_OK;
    my $smp_val;
    my @smpvals = ( $FM_QOS_TC_SMP_0, $FM_QOS_TC_SMP_1, $FM_QOS_TC_SMP_2,
                    $FM_QOS_TC_SMP_3, $FM_QOS_TC_SMP_4, $FM_QOS_TC_SMP_5,
                    $FM_QOS_TC_SMP_6, $FM_QOS_TC_SMP_7, $FM_QOS_TC_SMP_8,
                    $FM_QOS_TC_SMP_9, $FM_QOS_TC_SMP_10, $FM_QOS_TC_SMP_11);
    my $firstSwitch;
    my $lastSwitch;
    my @tcValues;
    my $tcMax;

    if (!defined($smp) || !defined($tc))
    {
        print("Must specify <traffic-class> <smp>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    # Check if we are running in SWAG
    my $info = new SDK::fm_switchInfo();
    $chip->fmGetSwitchInfo(0, $info);

    if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
    {
        @tcValues = $self->validateList($tc, 0, 11);
        my (@switch_array) = $self->tpPlatformGetSwitchRange();
        $firstSwitch = $switch_array[0];
        $lastSwitch = $switch_array[1];
        $tcMax = 11;
    }
    elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
    {
        @tcValues = $self->validateList($tc, 0, 7);
        $firstSwitch = 0;
        $lastSwitch = 0;
        $tcMax = 7;
    }
    else
    {
        @tcValues = $self->validateList($tc, 0, 7);
        my (@switch_array) = $self->tpPlatformGetSwitchRange();
        $firstSwitch = $switch_array[0];
        $lastSwitch = $switch_array[1];
        $tcMax = 7;
    }

    for (my $sw = $firstSwitch; $sw <= $lastSwitch; $sw++)
    {
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);

        # Chip specific attributes and bounds
        my $attrName = "";
        my $maxSmp = 0;

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000 ||
            $info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000 ||
            $info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
        {
            $attrName   = $FM_QOS_TC_SMP_MAP;
        }
        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000)
        {
            printf("FM2000 doesn't support SMP to TC!\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }

        if (lc($smp) eq  "none")
        {
            $smp_val = $FM_QOS_TC_SMP_NONE;
        } else {
            if ( ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000) &&
                 ((int($smp) >= 0) && (int($smp) <= 11)) )
            {
                $smp_val = $smpvals[$smp];
            }
            elsif (($smp eq "0") || ($smp eq "1"))
            {
                $smp_val = $smpvals[$smp];
            } else {
                printf("Must specify valid SMP (Shared Memory Partition)!\n");
                return $FM_ERR_INVALID_ARGUMENT;
            }
        }

        foreach my $tcValue (@tcValues)
        {
            if (($tcValue < 0) || ($tcValue > $tcMax))
            {
                printf("Must specify valid traffic class (tc) value ".
                       "($tcValue is invalid)!\n");
                return $FM_ERR_INVALID_ARGUMENT;
            }

            my %void = ( type => "fm_int", value => $smp_val );

            $status = $chip->fmSetSwitchQOS($sw, $attrName,
                                            $tcValue, \%void);
        }
    }
}

#****************************************************************************/
# tpHandleSetSwitchMapIsl
#
# @desc             Map the Inter Switch Link (ISL) priority to RXMP, TXMP, or
#                   TC.
#                   
# @param[in]        map is the map name to target, rxmp, txmp, or tc.
#
# @param[in]        isl is the ISL priority to map.
#
# @param[in]        dest is the destination rxmp, txmp, or tc to map to.
#
# @return           FM_OK if successful
#                   FM_ERR_INVALID_ARGUMENT if incorrect arguments are
#                   specified
#
#****************************************************************************/
sub tpHandleSetSwitchMapIsl
{
    my ($self, $map, $isl, $dest) = @_;
    my $chip = $self->{CHIP};
    my $status = $FM_OK;
    my $firstSwitch;
    my $lastSwitch;

    if ( (!defined $map) || (!defined $isl) || (!defined $dest) )
    {
        printf("Must specify all arguments!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @islPris = $self->validateList($isl, 0, 15);

    # Check if we are running in SWAG
    my $info = new SDK::fm_switchInfo();
    $chip->fmGetSwitchInfo(0, $info);

    if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
    {
        $firstSwitch = 0;
        $lastSwitch = 0;
    }
    else
    {
        my (@switch_array) = $self->tpPlatformGetSwitchRange();
        $firstSwitch = $switch_array[0];
        $lastSwitch = $switch_array[1];
    }

    for (my $sw = $firstSwitch; $sw <= $lastSwitch; $sw++)
    {
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);

        # Chip specific attributes and bounds
        my $attrName = "";
        my $maxSmp = 0;

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000 ||
            $info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
        {
            if (lc($map) eq "rxmp") 
            {
                $attrName = $FM_QOS_SWPRI_RXMP_MAP;
            }
            elsif (lc($map) eq "txmp")
            {
                $attrName = $FM_QOS_SWPRI_TXMP_MAP;
            }
            elsif (lc($map) eq "tc")
            {
                $attrName = $FM_QOS_SWPRI_TC_MAP;
            }
        }
        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000)
        {
            printf("FM4000 doesn't support this mapping!\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }
        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000)
        {
            printf("FM2000 doesn't support this mapping!\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }

        foreach my $islValue (@islPris)
        {
            if ( ($islValue < 0) || ($islValue > 15) )
            {
                printf("Invalid ISL specified: $islValue\n");
                return $FM_ERR_INVALID_ARGUMENT;
            }

            my %void = ( type => "fm_int", value => $dest );
            $status = $chip->fmSetSwitchQOS($sw, $attrName, $islValue, \%void);
        }
    }
}

#****************************************************************************/
# tpHandleSetPortMapTc
#
# @desc             Map the traffic class (TC) to pause class (PC).
#                   
# @param[in]        map is the map name to target, bsg or pc.
#
# @param[in]        tc is the traffic class to map.
#
# @param[in]        dest is the destination pc to map to.
#
# @return           FM_OK if successful
#                   FM_ERR_INVALID_ARGUMENT if incorrect arguments are
#                   specified
#
#****************************************************************************/
sub tpHandleSetPortMapTc 
{ 
    my ($self, $port, $map, $tc, $dest) = @_;
    my $chip = $self->{CHIP};
    my $status = $FM_OK;
    my $firstSwitch;
    my $lastSwitch;
    my @destValues;

    if ( (!defined $map) || (!defined $tc) || (!defined $dest) )
    {
        printf("Must specify all arguments!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    # Check if we are running in SWAG
    my $info = new SDK::fm_switchInfo();
    $chip->fmGetSwitchInfo(0, $info);

    if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
    {
        $firstSwitch = 0;
        $lastSwitch = 0;
    }
    else
    {
        my (@switch_array) = $self->tpPlatformGetSwitchRange();
        $firstSwitch = $switch_array[0];
        $lastSwitch = $switch_array[1];
    }

    for (my $sw = $firstSwitch; $sw <= $lastSwitch; $sw++)
    {
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);

        # Chip specific attributes and bounds
        my $attrName = "";
        my $maxSmp = 0;
        my $maxTc = 11;

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000 ||
            $info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
        {
            if (lc($map) eq "pc")
            {
                @destValues = $self->validateList($dest, 0, 7);
                $attrName = $FM_QOS_TC_PC_MAP;
            }
        }
        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000)
        {
            printf("FM4000 doesn't support this mapping!\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }
        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000)
        {
            printf("FM2000 doesn't support this mapping!\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }
        
        my @trafficClasses = $self->validateList($tc, 0, $maxTc);
        foreach my $tcValue (@trafficClasses)
        {
            if ( ($tcValue < 0) || ($tcValue > $maxTc) )
            {
                printf("Invalid TC specified: $tcValue\n");
                return $FM_ERR_INVALID_ARGUMENT;
            }

            my %void = ( type => "fm_int", value => $dest );
            $status = $chip->fmSetPortQOS($sw, $port, $attrName, $tcValue, 
                                          \%void);
        }
    }
}

#****************************************************************************/
# tpHandleSetPortMapPc
#
# @desc             Map the pause class (PC) to RX memory partition (RXMP).
#                   
# @param[in]        map is the map name to target: rxmp
#
# @param[in]        pc is the pause class to map.
#
# @param[in]        dest is the destination rxmp to map to.
#
# @return           FM_OK if successful
#                   FM_ERR_INVALID_ARGUMENT if incorrect arguments are
#                   specified
#
#****************************************************************************/
sub tpHandleSetPortMapPc 
{ 
    my ($self, $port, $map, $pc, $dest) = @_;
    my $chip = $self->{CHIP};
    my $status = $FM_OK;
    my $firstSwitch;
    my $lastSwitch;
    my @destValues;

    if ( (!defined $map) || (!defined $pc) || (!defined $dest) )
    {
        printf("Must specify all arguments!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @pauseClasses = $self->validateList($pc, 0, 7);

    # Check if we are running in SWAG
    my $info = new SDK::fm_switchInfo();
    $chip->fmGetSwitchInfo(0, $info);

    if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
    {
        $firstSwitch = 0;
        $lastSwitch = 0;
    }
    else
    {
        my (@switch_array) = $self->tpPlatformGetSwitchRange();
        $firstSwitch = $switch_array[0];
        $lastSwitch = $switch_array[1];
    }

    for (my $sw = $firstSwitch; $sw <= $lastSwitch; $sw++)
    {
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);

        # Chip specific attributes and bounds
        my $attrName = "";
        my $maxSmp = 0;

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000 ||
            $info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
        {
            if (lc($map) eq "rxmp") 
            {
                @destValues = $self->validateList($dest, 0, 11);
                $attrName = $FM_QOS_PC_RXMP_MAP;
            }
        }
        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000)
        {
            printf("FM4000 doesn't support this mapping!\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }
        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000)
        {
            printf("FM2000 doesn't support this mapping!\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }

        foreach my $pcValue (@pauseClasses)
        {
            my %void = ( type => "fm_int", value => $dest );
            $status = $chip->fmSetPortQOS($sw, $port, $attrName, $pcValue, 
                                          \%void);
        }
    }
}




#****************************************************************************/
# tpHandleSetPortSwpriSource
#
# @desc             Map the Shared Memory Partition (SMP) that each
#                   Traffic Class (TC) belongs to.
#
# @param[in]        tc - Traffic Class (0 - 7)
#
# @param[in]        smp - Shared Memory Partition (0 or 1)
#
# @return          NA
#
#****************************************************************************/
sub tpHandleSetPortSwpriSource
{
    my ($self, $port, $source, $state) = @_;
    my $chip = $self->{CHIP};
    my $status = $FM_OK;

    if ( !defined($port) || !defined($source) || !defined($state) )
    {
        printf("Must specify <port> <source> <state>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, 
                                    $self->tpPlatformGetFaceplatePortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, 
                                $self->tpPlatformGetSwitchPortList($sw, $TRUE));
        
        if (scalar(@portList) == 0)
        {
            next;
        }

        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);
        
        push(@affectedPortList, @portList);

        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            
            # FIXME: Need to handle FM_SWITCH_FAMILY_SWAG for both FM4000 and FM6000
            if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
            {
                $self->tpFM6000HandleSetPortSwpriSource($sw, 
                                                        $port, 
                                                        $source, 
                                                        $state);
            }
            elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000 ||
                   $info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
            {
                $self->tpFM4000HandleSetPortSwpriSource($sw,
                                                        $port, 
                                                        $source, 
                                                        $state);
            }
                        
        }   # end foreach my $port (@portList)
        
    }   # end foreach my $sw ($self->tpGetSwitches)
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);

}




sub tpHandleSetSwitchMapDscpToSwpri
{
    my ($self, $dscp, $swpri) = @_;
    my $chip = $self->{CHIP};
    my $status = $FM_OK;

    if (!defined($swpri) || !defined($dscp))
    {
        print("Must specify <dscp> <swpri>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (($swpri < 0) || ($swpri > 15))
    {
        printf("Must specify valid switch priority!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @dscpValues = $self->validateList($dscp, 0, 63);

    foreach my $sw ($self->tpGetSwitches)
    {
        foreach my $dscpValue (@dscpValues)
        {
            if (($dscpValue < 0) || ($dscpValue > 63))
            {
                printf("Must specify valid DSCP value ($dscpValue is invalid)!\n");
                return $FM_ERR_INVALID_ARGUMENT;
            }

            my %void = ( type => "fm_int", value => $swpri );

            $status = $chip->fmSetSwitchQOS($sw, $FM_QOS_DSCP_SWPRI_MAP,
                                            $dscpValue, \%void);
        }
    }

    return $status;
}

1;

