# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/MirrorCore.pm
# Creation Date:    12/17/07
# Description:      TestPoint perl code for handling cli commands invoked via the 
#                   xml parsing.
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
package Applications::TestPoint::Common::MirrorCore;
use strict;
use warnings;

# imports 
use SDKScalars; 

use Applications::TestPoint::Common::Messages;

###############################################################################
#
#                           LOCAL VARIABLES
#
###############################################################################


our %__mirrVlanType =
(
    $FM_MIRROR_VLAN_INGRESS        => 'I',
    $FM_MIRROR_VLAN_EGRESS         => 'E',
    $FM_MIRROR_VLAN_BIDIRECTIONAL  => 'B',
);


our %__mirrPortType =
(
    $FM_MIRROR_TYPE_INGRESS               => 'I',
    $FM_MIRROR_TYPE_EGRESS                => 'E',
    $FM_MIRROR_TYPE_BIDIRECTIONAL         => 'B',
    $FM_MIRROR_TYPE_RX_INGRESS_TX_EGRESS  => 'B',
);


###############################################################################
#
#                           LOCAL FUNCTIONS
#
###############################################################################
#
#

##@cmethod private bool* _tpIsValidMirrorGroupList(int *mirrorGroup)
#
# @desc         Determines whether a set of mirror group ids has been previously
#               configured
#
# @param[in]    mirrorList The set of mirror group ids whose status is to be 
#                           determined
#
# @return       hash hash list of indicators of whether each entry is valid
sub _tpIsValidMirrorGroupList
{
    my ($self, @mirrorList) = @_;

    my $chip = $self->{CHIP};
    
    my ($mirrorPort, $mirrorType);
    my $switchNum = shift(@{[$self->tpGetSwitches]});
    
    # creates hashed list 
    my %hash = map {$_ => $FALSE} @mirrorList;
    my ($currentGroup, $nextGroup);       

    $chip->disableErrors();

    $chip->fmGetMirrorFirst($switchNum, \$currentGroup, 
                            \$mirrorPort, \$mirrorType); 
    while ($currentGroup != -1)
    {
        if (exists($hash{$currentGroup}))
        {
            $hash{$currentGroup} = $TRUE;
        }
        $chip->fmGetMirrorNext($switchNum, $currentGroup, \$nextGroup,
                               \$mirrorPort, \$mirrorType);                
        $currentGroup = $nextGroup;
    }
    $chip->enableErrors();
    return (%hash);
}


###############################################################################
#
#                           PUBLIC FUNCTIONS
#
###############################################################################

##@cmethod public int tpHandleCreateMirror
#
# @desc         Handles creating a Port Mirroring instance identifier or group.
#
# @param[in]    mirrorGroup - id for the mirror group
#
# @param[in]    mirrorPort - port number of the destination/egress port
#
# @param[in]    mirrorType - direction of traffic on port being monitored.
#                  Can be ingress or egress (bi-directional is coming)
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise

sub tpHandleCreateMirror
{
    my ($self, $mirrorGroup, $mirrorPort, $mirrorType) = @_;

    my $chip = $self->{CHIP};
      
    # validate range of port mirror groups
    my @mirrorList = $self->validateList($mirrorGroup, $self->tpPlatformGetMirrorRange);
    if (!defined($mirrorGroup) || scalar(@mirrorList) == 0)
    {
        print($TP_MSG_ERR_PMIR_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    
    # Validate port and handle a list of ports passed via a range (i.e., 1..4,9)
    my @globalPortList = $self->validateList($mirrorPort,
                                       $self->tpPlatformGetFaceplatePortRange);
    if (!defined($mirrorPort) || scalar(@globalPortList) == 0)
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }
     
    # Validate the mirrorType and convert to enum fm_mirrorType for calling
    my $mirrorTypeEnum;
    if ( !defined($mirrorType))
    {
        print($TP_MSG_ERR_PMIR_INVALID_TYPE);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    elsif ($mirrorType eq "egress")
    {
     $mirrorTypeEnum = $FM_MIRROR_TYPE_EGRESS;
    }
    elsif ($mirrorType eq "ingress")
    {
     $mirrorTypeEnum = $FM_MIRROR_TYPE_INGRESS;
    }
    elsif ($mirrorType eq "bi-directional")
    {
     $mirrorTypeEnum = $FM_MIRROR_TYPE_BIDIRECTIONAL;
    }
    elsif ($mirrorType eq "redirect")
    {
     $mirrorTypeEnum = $FM_MIRROR_TYPE_REDIRECT;
    }
    elsif ($mirrorType eq "tx-egress")
    {
     $mirrorTypeEnum = $FM_MIRROR_TYPE_TX_EGRESS;
    }
    elsif ($mirrorType eq "rx-ingress-tx-egress")
    {
     $mirrorTypeEnum = $FM_MIRROR_TYPE_RX_INGRESS_TX_EGRESS;
    }
    else
    {
        print($TP_MSG_ERR_PMIR_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @affectedPortList = ();
    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $mirrorPort, 
                                $self->tpPlatformGetSwitchPortList($switchNum, 
                                                                   $TRUE));
            
        if (scalar(@portList) == 0)
        {
            # None of the ports appear on this switch
            next;
        }
        
        push(@affectedPortList, @portList);

        foreach my $globalPort (@portList)
        {
           my ($sw, $logicalPort) = 
                              $self->tpPlatformMapGlobalToLogicalPort($globalPort);
           foreach my $mirrorGroup (@mirrorList)
           {
                   $chip->disableErrors();
                   my $status = $chip->fmCreateMirror($switchNum, $mirrorGroup, 
                                $logicalPort, $mirrorTypeEnum);
                   $chip->enableErrors();
                   if ($status != $FM_OK)
                   {
                       $result = $FM_FAIL;
                       printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                       printf("Mirroring Group %s cannot be created\n", $mirrorGroup);
                   }
           }
        }

    }
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $result;
}

##@cmethod public int tpHandleDeleteMirror
#
# @desc         Handles deleting a Port Mirroring instance identifier or group.
#
# @param[in]    mirrorGroup - id for the mirror group
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise

sub tpHandleDeleteMirror
{
    my ($self, $mirrorGroup) = @_;

    my $chip = $self->{CHIP};
      
    # validate range of port mirror groups
    my @mirrorList = $self->validateList($mirrorGroup, $self->tpPlatformGetMirrorRange);
    if (!defined($mirrorGroup) || scalar(@mirrorList) == 0)
    {
        print($TP_MSG_ERR_PMIR_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $mirrorGroup (@mirrorList)
    {
        foreach my $switchNum ($self->tpGetSwitches)
        {
            $chip->disableErrors();
            my $status = $chip->fmDeleteMirror($switchNum, $mirrorGroup);
            $chip->enableErrors();
            if ($status != $FM_OK)
            {
                $result = $FM_FAIL;
                printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                printf("Mirroring Group %s cannot be deleted\n", $mirrorGroup);
            }
        }
    }
    return $result;
}

##@cmethod public int tpHandleMirrorAddPort
#
# @desc         Handles adding a port to a Port Mirroring Group.
#
# @param[in]    mirrorGroup - id for the mirror group
#               all or a port range/list should add a port to many groups
#               a specific group id should add a port to only that group
#
# @param[in]    portList - port(s) to add to the mirror group.
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise

sub tpHandleMirrorAddPort
{
    my ($self, $mirrorGroup, $port, $type) = @_;

    my $chip = $self->{CHIP};
    
    # validate range of port mirror groups
    my @mirrorList = $self->validateList($mirrorGroup, $self->tpPlatformGetMirrorRange);
    if (!defined($mirrorGroup) || scalar(@mirrorList) == 0)
    {
        print($TP_MSG_ERR_PMIR_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    
    # Validate port and handle a list of ports passed via a range (i.e., 1..4,9)
    my @globalPortList = $self->validateList($port,
                                       $self->tpPlatformGetFaceplatePortRange);
    if (!defined($port) || scalar(@globalPortList) == 0)
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @affectedPortList = ();     
    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, 
                                $self->tpPlatformGetSwitchPortList($switchNum, 
                                                                   $TRUE));
            
        if (scalar(@portList) == 0)
        {
            # None of the ports appear on this switch
            next;
        }
        
        push(@affectedPortList, @portList);

        foreach my $globalPort (@portList)
        {
           my ($sw, $logicalPort) = 
                              $self->tpPlatformMapGlobalToLogicalPort($globalPort);
           foreach my $mirrorGroup (@mirrorList)
           {
               $chip->disableErrors();
               my $status;

               if (!defined($type))
               {
                   $status = $chip->fmAddMirrorPort($switchNum, $mirrorGroup, 
                             $logicalPort);
               }
               else
               {
                   my $mirrorType;
                   if ($type eq "ingress")
                   {
                       $mirrorType = $FM_MIRROR_TYPE_INGRESS;
                   }
                   elsif ($type eq "egress")
                   {
                       $mirrorType = $FM_MIRROR_TYPE_EGRESS;
                   }
                   elsif ($type eq "bi-directional")
                   {
                       $mirrorType = $FM_MIRROR_TYPE_BIDIRECTIONAL;
                   }

                   $status = $chip->fmAddMirrorPortExt($switchNum, $mirrorGroup, 
                             $logicalPort, $mirrorType);
               }

               $chip->enableErrors();
               if ($status != $FM_OK)
               {
                   $result = $FM_FAIL;
                   printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                   printf("Mirroring Group %s cannot add port: %s\n", 
                   $mirrorGroup, $logicalPort);
               }
           }
        }
    }
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $result;
}

##@cmethod public int tpHandleMirrorAddVlan
#
# @desc         Handles adding a vlan to a Port Mirroring Group.
#
# @param[in]    mirrorGroup - id for the mirror group
#               all or a port range/list should add a port to many groups
#               a specific group id should add a port to only that group
#
# @param[in]    vlanList - vlan(s) to add to the mirror group.
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise

sub tpHandleMirrorAddVlan
{
    my ($self, $mirrorGroup, $vlan, $type) = @_;

    my $chip = $self->{CHIP};
    
    # validate range of mirror groups
    my @mirrorList = $self->validateList($mirrorGroup, $self->tpPlatformGetMirrorRange);
    if (!defined($mirrorGroup) || scalar(@mirrorList) == 0)
    {
        print($TP_MSG_ERR_PMIR_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @vlanList = $self->validateList($vlan, $self->tpPlatformGetVLANRange);
    if (!defined($vlan) || scalar(@vlanList) == 0)
    {
        print($TP_MSG_ERR_VLAN_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    
    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        foreach my $globalVlan (@vlanList)
        {
           foreach my $mirrorGroup (@mirrorList)
           {
               $chip->disableErrors();
               my $status;

               my $mirrorType;
               if ($type eq "ingress")
               {
                   $mirrorType = $FM_MIRROR_VLAN_INGRESS;
               }
               elsif ($type eq "egress")
               {
                   $mirrorType = $FM_MIRROR_VLAN_EGRESS;
               }
               elsif ($type eq "bi-directional")
               {
                   $mirrorType = $FM_MIRROR_VLAN_BIDIRECTIONAL;
               }

               $status = $chip->fmAddMirrorVlan($switchNum, $mirrorGroup, 
                         $globalVlan, $mirrorType);

               $chip->enableErrors();
               if ($status != $FM_OK)
               {
                   $result = $FM_FAIL;
                   printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                   printf("Mirroring Group %s cannot add vlan: %s\n", 
                   $mirrorGroup, $globalVlan);
               }
           }
        }
    }
    return $result;
}

##@cmethod public int tpHandleMirrorAddVlan2
#
# @desc         Handles adding a vlan2 to a Port Mirroring Group.
#
# @param[in]    mirrorGroup - id for the mirror group
#               all or a port range/list should add a port to many groups
#               a specific group id should add a port to only that group
#
# @param[in]    vlanList - vlan(s) to add to the mirror group.
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise

sub tpHandleMirrorAddVlan2
{
    my ($self, $mirrorGroup, $vlan, $type) = @_;

    my $chip = $self->{CHIP};
    
    # validate range of mirror groups
    my @mirrorList = $self->validateList($mirrorGroup, $self->tpPlatformGetMirrorRange);
    if (!defined($mirrorGroup) || scalar(@mirrorList) == 0)
    {
        print($TP_MSG_ERR_PMIR_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @vlanList = $self->validateList($vlan, $self->tpPlatformGetVLANRange);
    if (!defined($vlan) || scalar(@vlanList) == 0)
    {
        print($TP_MSG_ERR_VLAN_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    
    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        foreach my $globalVlan (@vlanList)
        {
           foreach my $mirrorGroup (@mirrorList)
           {
               $chip->disableErrors();
               my $status;

               my $mirrorType;
               if ($type eq "ingress")
               {
                   $mirrorType = $FM_MIRROR_VLAN_INGRESS;
               }
               elsif ($type eq "egress")
               {
                   $mirrorType = $FM_MIRROR_VLAN_EGRESS;
               }
               elsif ($type eq "bi-directional")
               {
                   $mirrorType = $FM_MIRROR_VLAN_BIDIRECTIONAL;
               }

               $status = $chip->fmAddMirrorVlanExt($switchNum, $mirrorGroup, 
                         $FM_VLAN_SELECT_VLAN2, $globalVlan, $mirrorType);

               $chip->enableErrors();
               if ($status != $FM_OK)
               {
                   $result = $FM_FAIL;
                   printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                   printf("Mirroring Group %s cannot add vlan2: %s\n", 
                   $mirrorGroup, $globalVlan);
               }
           }
        }
    }
    return $result;
}

##@cmethod public int tpHandleMirrorDeletePort
#
# @desc         Handles deleting a port from a Port Mirroring Group.
#
# @param[in]    mirrorGroup - id for the mirror group
#               all or a range/list should delete from many groups at once
#               a specific group id should delete a port from only that group
#
# @param[in]    portList - port to delete from the mirror group.
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise

sub tpHandleMirrorDeletePort
{
    my ($self, $mirrorGroup, $port) = @_;

    my $chip = $self->{CHIP};
    
    # validate range of port mirror groups
    my @mirrorList = $self->validateList($mirrorGroup, $self->tpPlatformGetMirrorRange);
    if (!defined($mirrorGroup) || scalar(@mirrorList) == 0)
    {
        print($TP_MSG_ERR_PMIR_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    
    # Validate port and handle a list of ports passed via a range (i.e., 1..4,9)
    my @globalPortList = $self->validateList($port,
                                       $self->tpPlatformGetFaceplatePortRange);
    if (!defined($port) || scalar(@globalPortList) == 0)
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    
    my @affectedPortList = (); 
    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, 
                                $self->tpPlatformGetSwitchPortList($switchNum, 
                                                                   $TRUE));
            
        if (scalar(@portList) == 0)
        {
            # None of the ports appear on this switch
            next;
        }
        
        push(@affectedPortList, @portList);

        foreach my $globalPort (@portList)
        {
           my ($sw, $logicalPort) = 
                              $self->tpPlatformMapGlobalToLogicalPort($globalPort);
           foreach my $mirrorGroup (@mirrorList)
           {
               $chip->disableErrors();
               my $status = $chip->fmDeleteMirrorPort($switchNum, $mirrorGroup, 
                            $logicalPort);
               $chip->enableErrors();
               if ($status != $FM_OK)
               {
                   $result = $FM_FAIL;
                   printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                   printf("Mirroring Group %s cannot remove port: %s\n", 
                   $mirrorGroup, $logicalPort);
               }
           }
        }
    }
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $result;
}

##@cmethod public int tpHandleMirrorDeleteVlan
#
# @desc         Handles deleting a vlan from a Port Mirroring Group.
#
# @param[in]    mirrorGroup - id for the mirror group
#               all or a range/list should delete from many groups at once
#               a specific group id should delete a port from only that group
#
# @param[in]    vlanList - vlan to delete from the mirror group.
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise

sub tpHandleMirrorDeleteVlan
{
    my ($self, $mirrorGroup, $vlan) = @_;

    my $chip = $self->{CHIP};
    
    # validate range of port mirror groups
    my @mirrorList = $self->validateList($mirrorGroup, $self->tpPlatformGetMirrorRange);
    if (!defined($mirrorGroup) || scalar(@mirrorList) == 0)
    {
        print($TP_MSG_ERR_PMIR_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    
    my @vlanList = $self->validateList($vlan, $self->tpPlatformGetVLANRange);
    if (!defined($vlan) || scalar(@vlanList) == 0)
    {
        print($TP_MSG_ERR_VLAN_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    
    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        foreach my $globalVlan (@vlanList)
        {
           foreach my $mirrorGroup (@mirrorList)
           {
               $chip->disableErrors();
               my $status;

               $status = $chip->fmDeleteMirrorVlan($switchNum, $mirrorGroup, 
                         $globalVlan);

               $chip->enableErrors();
               if ($status != $FM_OK)
               {
                   $result = $FM_FAIL;
                   printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                   printf("Mirroring Group %s cannot delete vlan: %s\n", 
                   $mirrorGroup, $globalVlan);
               }
           }
        }
    }
    return $result;
}

##@cmethod public int tpHandleMirrorDeleteVlan2
#
# @desc         Handles deleting a vlan2 from a Port Mirroring Group.
#
# @param[in]    mirrorGroup - id for the mirror group
#               all or a range/list should delete from many groups at once
#               a specific group id should delete a port from only that group
#
# @param[in]    vlanList - vlan to delete from the mirror group.
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise

sub tpHandleMirrorDeleteVlan2
{
    my ($self, $mirrorGroup, $vlan) = @_;

    my $chip = $self->{CHIP};
    
    # validate range of port mirror groups
    my @mirrorList = $self->validateList($mirrorGroup, $self->tpPlatformGetMirrorRange);
    if (!defined($mirrorGroup) || scalar(@mirrorList) == 0)
    {
        print($TP_MSG_ERR_PMIR_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    
    my @vlanList = $self->validateList($vlan, $self->tpPlatformGetVLANRange);
    if (!defined($vlan) || scalar(@vlanList) == 0)
    {
        print($TP_MSG_ERR_VLAN_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    
    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        foreach my $globalVlan (@vlanList)
        {
           foreach my $mirrorGroup (@mirrorList)
           {
               $chip->disableErrors();
               my $status;

               $status = $chip->fmDeleteMirrorVlanExt($switchNum, $mirrorGroup, 
                         $FM_VLAN_SELECT_VLAN2, $globalVlan);

               $chip->enableErrors();
               if ($status != $FM_OK)
               {
                   $result = $FM_FAIL;
                   printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                   printf("Mirroring Group %s cannot delete vlan: %s\n", 
                   $mirrorGroup, $globalVlan);
               }
           }
        }
    }
    return $result;
}

##@cmethod public int tpHandleShowMirror
#
# @desc         Handles displaying all existing Port Mirroring instances or groups.
#
# @param[in]    mirrorGroup - id for the mirror group
#               all or nothing should display all groups
#               a specific group id should display only that group
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise

sub tpHandleShowMirror
{
    my ($self, $mirrorGroup) = @_;

    my $chip = $self->{CHIP};
    
    # validate range of port mirror groups
    my @mirrorList = $self->validateList($mirrorGroup, 
                     $self->tpPlatformGetMirrorRange);
    if (!defined($mirrorGroup) || scalar(@mirrorList) == 0)
    {
        print($TP_MSG_ERR_PMIR_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    
    print("\nMirroring Groups:\n");
    printf("%-4s|%-10s|%-12s|%-14s|%-18s|%-18s|%-8s|%-8s\n", "Id", "Dest. Port", "Mirror Type", 
    "Mirrored Ports", "Mirrored Vlan1s", "Mirrored Vlan2s", "Sample", "ACL Ext");
    print("--------------------------------------------------------------------------------------------------\n");

    my $switchCount = scalar(@{[$self->tpGetSwitches]});    

    # This creates a hash list for each group entered at the command line
    # and marks each that doesn't not exist as false
    my %valid = $self->_tpIsValidMirrorGroupList(@mirrorList);
 
    foreach my $mirrorGroup (@mirrorList)
    {
        my ($mirrorTypeEnum, $mirrorType);
        my ($currentPort, $portMask, $dstPort, $nextPort, $portString, $portType);
        my %portList;
        my ($currentVlan, $nextVlan, $vlanString, $vlanType);
        my %vlanList;
        my %vlan2List;
        my ($sampleString, $aclString);
        $dstPort = 0;
        $mirrorType = "default";

        # skip all mirror groups that don't have an entry     
        if ($valid{$mirrorGroup} == $FALSE)
        {
            next;                 
        }   
       
        $chip->disableErrors();    
        foreach my $switchNum ($self->tpGetSwitches)
        {
            # get the type and mirror port for each 
            my $status = $chip->fmGetMirror($switchNum, $mirrorGroup, 
                                            \$dstPort, \$mirrorTypeEnum);
            $dstPort = $self->tpPlatformMapLogicalToFaceplatePort($switchNum, 
                                                                  $dstPort);
            if ($status != $FM_OK)
            {
                next;
            }
            # get each mirrored port in the mirror group
            $status = $chip->fmGetMirrorPortFirstV2($switchNum, $mirrorGroup, 
                            \$currentPort, \$portType);
            # get each mirrored port in the mirror group

            while (($status == $FM_OK) && ($currentPort != -1))
            {
                my $facePlatePort = $self->tpPlatformMapLogicalToFaceplatePort($switchNum, 
                                                                  $currentPort);
                push(@{$portList{$portType}}, $facePlatePort);
                my $status = $chip->fmGetMirrorPortNextV2($switchNum, $mirrorGroup, 
                            $currentPort, \$nextPort, \$portType);
         
                $currentPort = $nextPort;
            } # while 

            # get each vlan in the mirror group
            $status = $chip->fmGetMirrorVlanFirst($switchNum, $mirrorGroup, 
                            \$currentVlan, \$vlanType);

            # get each mirrored vlan in the mirror group
            while ($status == $FM_OK)
            {
                push(@{$vlanList{$vlanType}}, $currentVlan);
                $status = $chip->fmGetMirrorVlanNext($switchNum, $mirrorGroup, 
                          $currentVlan, \$nextVlan, \$vlanType);
         
                $currentVlan = $nextVlan;
            } # while 
    
            # get each vlan2 in the mirror group
            $status = $chip->fmGetMirrorVlanFirstExt($switchNum, $mirrorGroup, 
                            $FM_VLAN_SELECT_VLAN2, \$currentVlan, \$vlanType);

            # get each mirrored vlan2 in the mirror group
            while ($status == $FM_OK)
            {
                push(@{$vlan2List{$vlanType}}, $currentVlan);
                $status = $chip->fmGetMirrorVlanNextExt($switchNum, $mirrorGroup, 
                          $FM_VLAN_SELECT_VLAN2, $currentVlan, \$nextVlan, \$vlanType);
         
                $currentVlan = $nextVlan;
            } # while 
    
           # convert mirror type enum to a string
           if ($mirrorTypeEnum == $FM_MIRROR_TYPE_EGRESS)
           {
               $mirrorType = "egress"; 
           }
           elsif ($mirrorTypeEnum == $FM_MIRROR_TYPE_INGRESS)
           {
               $mirrorType = "ingress"; 
           }
           elsif ($mirrorTypeEnum == $FM_MIRROR_TYPE_BIDIRECTIONAL)
           {
               $mirrorType = "bi-direct"; 
           }
           elsif ($mirrorTypeEnum == $FM_MIRROR_TYPE_REDIRECT)
           {
               $mirrorType = "redirect"; 
           }
           elsif ($mirrorTypeEnum == $FM_MIRROR_TYPE_TX_EGRESS)
           {
               $mirrorType = "tx-egress";
           }
           elsif ($mirrorTypeEnum == $FM_MIRROR_TYPE_RX_INGRESS_TX_EGRESS)
           {
               $mirrorType = "rx-in-tx-eg";
           }
           else
           {
               $mirrorType = "error";
           }

           my %sample = (type => "fm_int", value => $FM_MIRROR_SAMPLE_RATE_DISABLED);
           $status = $chip->fmGetMirrorAttribute($switchNum, $mirrorGroup, 
                            $FM_MIRROR_SAMPLE_RATE, \%sample);

           if ($status != $FM_OK)
           {
               $sampleString = 'N/A';
           }
           elsif ($sample{value} == $FM_MIRROR_SAMPLE_RATE_DISABLED)
           {
               $sampleString = 'disabled';
           }
           else
           {
               $sampleString = "$sample{value}";
           }

           my %acl = (type => "fm_bool", value => $FALSE);
           $status = $chip->fmGetMirrorAttribute($switchNum, $mirrorGroup, 
                            $FM_MIRROR_ACL, \%acl);

           if ($status != $FM_OK)
           {
               $sampleString = 'N/A';
           }
           elsif ($acl{value} == $FALSE)
           {
               $aclString = 'off';
           }
           else
           {
               $aclString = 'on';
           }
           
           # print out a line item for the show command for each group
           printf("%-4s|%-10s|%-12s|", $mirrorGroup, $dstPort, $mirrorType);
           # turn the port mask into a string and print
           $portString = "";
           foreach my $key (sort keys %portList)
           {
               if ($portString ne "")
               {
                   $portString = $portString . " ";
               }
               $portString = "$portString" . $__mirrPortType{$key};
               $portString = "$portString" . $self->StringifyList(@{$portList{$key}});
           }
           printf("%-14s|", $portString);

           # VLAN1
           $vlanString = "";
           foreach my $key (sort keys %vlanList)
           {
               if ($vlanString ne "")
               {
                   $vlanString = $vlanString . " ";
               }
               $vlanString = "$vlanString" . $__mirrVlanType{$key};
               $vlanString = "$vlanString" . $self->StringifyList(@{$vlanList{$key}});
           }
           if ($vlanString eq "")
           {
               $vlanString = "all";
           }

           #printf("%-18s|%-8s|%-8s\n", $vlanString, $sampleString, $aclString);
           printf("%-18s|", $vlanString);

           # VLAN2
           $vlanString = "";
           foreach my $key (sort keys %vlan2List)
           {
               if ($vlanString ne "")
               {
                   $vlanString = $vlanString . " ";
               }
               $vlanString = "$vlanString" . $__mirrVlanType{$key};
               $vlanString = "$vlanString" . $self->StringifyList(@{$vlan2List{$key}});
           }
           if ($vlanString eq "")
           {
               $vlanString = "all";
           }

           printf("%-18s|%-8s|%-8s\n", $vlanString, $sampleString, $aclString);

        } # end foreach $SwitchNum

        $chip->enableErrors();

    } # end foreach $MirrorGroup
        
    return $FM_OK;
} # end tpHandleShowMirror

##@cmethod public int tpHandleMirrorAttribute
#
# @desc         Handles to configure various Mirroring attribute.
# 
# @param[in]    command - attribute
# 
# @param[in]    mirrorGroup - id for the mirror group
# 
# @param[in]    arg - argument
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise

sub tpHandleMirrorAttribute
{
    my ($self, $command, $mirrorGroup, $arg) = @_;

    my $chip = $self->{CHIP};
      
    # validate range of port mirror groups
    my @mirrorList = $self->validateList($mirrorGroup, $self->tpPlatformGetMirrorRange);
    if (!defined($mirrorGroup) || scalar(@mirrorList) == 0)
    {
        print($TP_MSG_ERR_PMIR_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (!defined($command) || !defined($arg))
    {
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $mirrorGroup (@mirrorList)
    {
        foreach my $switchNum ($self->tpGetSwitches)
        {
            if ($command eq "dest")
            {
                my $portId = SDK::fm_portIdentifier->new();
                my @globalPortList = ();

                if(!(lc($arg) eq "none"))
                {
                    # Validate port and handle a list of ports passed via a range (i.e., 1..4,9)
                    @globalPortList = $self->validateList($arg,
                                                   $self->tpPlatformGetFaceplatePortRange);
                }
                if (scalar(@globalPortList) == 1)
                {
                    $portId->{'identifierType'} = $FM_PORT_IDENTIFIER_PORT_NUMBER;
                    $portId->{'port'} = pop(@globalPortList);
                }
                else
                {
                    $portId->{'identifierType'} = $FM_PORT_IDENTIFIER_PORT_MASK;
                    $chip->fmCreateBitArray($portId->{'portMask'}, 76);

                    foreach my $port (@globalPortList)
                    {
                        $chip->fmSetBitArrayBit($portId->{'portMask'}, $port, 1);
                    }
                }
                $chip->disableErrors();
                my $status = $chip->fmSetMirrorDestinationExt($switchNum, $mirrorGroup,
                             $portId);
                $chip->enableErrors();
                if ($status != $FM_OK)
                {
                    $result = $FM_FAIL;
                    printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                    printf("Invalid attribute or Config for Mirroring Group %s\n", $mirrorGroup);
                }
            }
            elsif($command eq "truncate-mask")
            {
                my $bitmask = SDK::fm_bitArray->new();
                $chip->fmCreateBitArray($bitmask, 76);

                my %void = ( type => "fm_bitArray", value => $bitmask );

                if(!(lc($arg) eq "none"))
                {
                    # Validate port and handle a list of ports passed via a range (i.e., 1..4,9)
                    my @globalPortList = $self->validateList($arg,
                                                             $self->tpPlatformGetFaceplatePortRange);
                    foreach my $port (@globalPortList)
                    {
                        $chip->fmSetBitArrayBit($bitmask, $port, 1);
                    }
                }

                $chip->disableErrors();
                my $status = $chip->fmSetMirrorAttribute($switchNum, $mirrorGroup,
                             $FM_MIRROR_TRUNCATE_MASK, \%void);
                $chip->enableErrors();
                if ($status != $FM_OK)
                {
                    $result = $FM_FAIL;
                    printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                    printf("Invalid attribute or Config for Mirroring Group %s\n", $mirrorGroup);
                }

                $chip->fmDeleteBitArray($bitmask);
            }
            else
            {
                my %void;
                my $attribute;
                if ($command eq "priority")
                {
                    if ($arg eq "original")
                    {
                        %void = (type => "fm_int", value => $FM_MIRROR_PRIORITY_ORIGINAL);
                    }
                    else
                    {
                        %void = (type => "fm_int", value => $arg);
                    }
                    $attribute = $FM_MIRROR_PRIORITY;
                }
                elsif ($command eq "truncation")
                {
                    %void = (type => "fm_bool", value => ($arg eq "on") ? 1 : 0);
                    $attribute = $FM_MIRROR_TRUNCATE;
                }
                elsif ($command eq "truncate-others")
                {
                    %void = (type => "fm_bool", value => ($arg eq "on") ? 1 : 0);
                    $attribute = $FM_MIRROR_TRUNCATE_OTHERS;
                }
                elsif ($command eq "acl")
                {
                    %void = (type => "fm_bool", value => ($arg eq "on") ? 1 : 0);
                    $attribute = $FM_MIRROR_ACL;
                }
                elsif ($command eq "sample")
                {
                    if ($arg eq "off")
                    {
                        %void = (type => "fm_int", value => $FM_MIRROR_SAMPLE_RATE_DISABLED);
                    }
                    else
                    {
                        %void = (type => "fm_int", value => $arg);
                    }
                    $attribute = $FM_MIRROR_SAMPLE_RATE;
                }
                elsif ($command eq "tx_egress_port")
                {
                    if ($arg eq "automatic")
                    {
                        %void = (type => "fm_int", value => $FM_MIRROR_TX_EGRESS_PORT_FIRST);
                    }
                    else
                    {
                        %void = (type => "fm_int", value => $arg);
                    }
                    $attribute = $FM_MIRROR_TX_EGRESS_PORT;
                }
                $chip->disableErrors();
                my $status = $chip->fmSetMirrorAttribute($switchNum, $mirrorGroup,
                             $attribute, \%void);
                $chip->enableErrors();
                if ($status != $FM_OK)
                {
                    $result = $FM_FAIL;
                    printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                    printf("Invalid attribute or Config for Mirroring Group %s\n", $mirrorGroup);
                }
            }
        }
    }
    return $result;
}
 
1;
