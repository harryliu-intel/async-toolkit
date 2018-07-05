# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/VLANCore.pm
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

package Applications::TestPoint::Common::VLANCore;
use strict;
use warnings;

use List::Util qw(max min);
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

###############################################################################
#
#                               CONSTANTS
#
###############################################################################

use constant DEBUG  => $FALSE;

###############################################################################
#
#                               LOCAL VARIABLES
#
###############################################################################

##@var <char*,int> __vlanAttrMap
our %__vlanAttrMap =
(
    'igmp_snooping' => $FM_VLAN_IGMP_TRAPPING,
    'reflect'       => $FM_VLAN_REFLECT,
    'routing'       => $FM_VLAN_ROUTABLE,
    'mtu'           => $FM_VLAN_MTU,
    'fid2_lookup'   => $FM_VLAN_FID2_IVL,
    'ip_tunneling'  => $FM_VLAN_IP_TUNNELING,
);

###############################################################################
#
#                               LOCAL FUNCTIONS
#
###############################################################################

##@cmethod private bool* _tpIsValidVLAN(int *vlanID)
#
# @desc         Determines whether a set of VLANs has been previously
#               configured
#
# @param[in]    vlanID The set of VLAN IDs whose status is to be determined
#
# @return       TRUE if the VLAN has been configured
# @return       FALSE otherwise
sub _tpIsValidVLAN
{
    my ($self, @vlanID) = @_;

    my $chip = $self->{CHIP};

    my $switchNum = shift(@{[$self->tpGetSwitches]});
    my %hash = map {$_ => $FALSE} @vlanID;
    my ($currentVlan, $nextVlan);

    my $status = $chip->fmGetVlanFirst($switchNum, \$currentVlan);
    while ($status == $FM_OK && $currentVlan != -1)
    {
        if (exists($hash{$currentVlan}))
        {
            $hash{$currentVlan} = $TRUE;
        }
        $chip->fmGetVlanNext($switchNum, $currentVlan, \$nextVlan);
        $currentVlan = $nextVlan;
    }
    my @result = map {$hash{$_}} @vlanID;
    return (scalar(@vlanID) == 1 ? shift(@result) : @result);
}

##@cmethod private bool* _tpIsValidVLANOnSwitch(int switchNum, int *vlanID)
#
# @desc         Determines whether a set of VLANs has been previously
#               configured
#
# @param[in]    switchNum is the switch number on which to check.
#
# @param[in]    vlanID The set of VLAN IDs whose status is to be determined
#
# @return       TRUE if the VLAN has been configured
# @return       FALSE otherwise
sub _tpIsValidVLANOnSwitch
{
    my ($self, $switchNum, @vlanID) = @_;

    my $chip = $self->{CHIP};

    my %hash = map {$_ => $FALSE} @vlanID;
    my ($currentVlan, $nextVlan);

    my $status = $chip->fmGetVlanFirst($switchNum, \$currentVlan);
    while ($status == $FM_OK && $currentVlan != -1)
    {
        if (exists($hash{$currentVlan}))
        {
            $hash{$currentVlan} = $TRUE;
        }
        $chip->fmGetVlanNext($switchNum, $currentVlan, \$nextVlan);
        $currentVlan = $nextVlan;
    }
    my @result = map {$hash{$_}} @vlanID;
    return (scalar(@vlanID) == 1 ? shift(@result) : @result);
}

##@cmethod private int GetVlanList(int **vlanIDs)
#
# @brief        Retrieves all VLAN IDs in use
#
# @param[in]    vlanIDs Points to a Perl ARRAY
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub GetVlanList
{
    my ($self, $vlanIDs) = @_;

    my $chip = $self->{'CHIP'};

    # Initialize the list of VLAN IDs to the empty list.
    @{$vlanIDs} = ();

    my ($currentVlan, $nextVlan, $status);

    my $switchNum = shift(@{[$self->tpGetSwitches]});
    $chip->disableErrors() if (!DEBUG);
    $status = $chip->fmGetVlanFirst($switchNum, \$currentVlan);
    my $id = 0;
    while ($status == $FM_OK && $currentVlan != -1)
    {
        $vlanIDs->[$id++] = $currentVlan;

        $status = $chip->fmGetVlanNext($switchNum, $currentVlan, \$nextVlan);
        $currentVlan = $nextVlan;
    }
    $chip->enableErrors() if (!DEBUG);
    return $status == $FM_OK ? $FM_OK : $FM_FAIL;
}

##@cmethod private int ValidateVlan(char *vlan, int **vlanList)
#
# @brief        Validates a VLAN ID or set of VLAN IDs
#
# @param[in]    vlan The VLAN ID or set of VLAN IDs to be validated
#
# @param[in]    vlanList Points to a Perl ARRAY in which the validated VLAN
#               IDs will be stored
#
# @result       FM_OK if successful
# @result       FM_FAIL otherwise
sub ValidateVlan
{
    my ($self, $vlan, $vlanList) = @_;

    my $chip = $self->{'CHIP'};

    my (@vlanIDs);

    my $status = $self->GetVlanList(\@vlanIDs);
    if ($status != $FM_OK)
    {
        return $FM_FAIL;
    }
    @{$vlanList} = $self->validateList($vlan, min(@vlanIDs), max(@vlanIDs));
    if (scalar(@{$vlanList}) == 0)
    {
        return $FM_FAIL;
    }
    if ($vlan ne 'all')
    {
        my %vlanIDs = map {$_ => $TRUE} @vlanIDs;
        foreach my $vlan (@{$vlanList})
        {
            if (!exists($vlanIDs{$vlan}))
            {
                return $FM_FAIL;
            }
        }
    }
    return $FM_OK;
}

###############################################################################
#
#                               PUBLIC FUNCTIONS
#
###############################################################################

##@cmethod public int tpHandleAddVlanCounter(int vlanID)
#
# @desc         Handles adding VLAN statistics counters to a VLAN
#
# @param[in]    vlanID The VLAN ID the statistics counters are to be added to
#
# @note         It is assumed that on a multi-chip system the VLAN tables on
#               all chips are in sync.
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleAddVlanCounter
{
    my ($self, $vlanID) = @_;

    my $chip = $self->{CHIP};

    $vlanID = $self->str2intnum($vlanID);
    if (!defined($vlanID))
    {
        print($TP_MSG_ERR_VLAN_INVALID_SCALAR);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    if (!$self->_tpIsValidVLAN($vlanID))
    {
        print($TP_MSG_ERR_VLAN_INVALID_ID);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        $chip->disableErrors();
        my $status = $chip->fmAllocateVLANCounters($switchNum, $vlanID);
        $chip->enableErrors();
        if ($status != $FM_OK)
        {
            $result = $FM_FAIL;
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            if ($status == $FM_ERR_NO_VLANCOUNTER)
            {
                print("Out of VLAN counters!\n");
                next;
            }
            printf("Cannot associate counters with VLAN %d!\n", $vlanID);
        }
    }
    return $result;
}

##@cmethod public int tpHandleAddVlanPort(char *vlan, char *port);
#
# @desc         Handles adding a set of ports to a set of VLANs
#
# @param[in]    vlan The set of VLANs the ports are to be added to
#
# @param[in]    port The set of ports to be added
#
# @note         It is assumed that on a multi-chip system the VLAN tables on
#               all chips are in sync.
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
sub tpHandleAddVlanPort
{
    my ($self, $vlan, $port) = @_;

    my $chip = $self->{CHIP};

    if (!defined($vlan) || !defined($port))
    {
        print("Must specify <vlan> <port>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @vlanList = $self->validateList($vlan, $self->tpPlatformGetVLANRange);
    if (!defined($vlan) || scalar(@vlanList) == 0)
    {
        print($TP_MSG_ERR_VLAN_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @affectedPortList = ();
    my @validateList = ();
    foreach ($self->tpGetSwitches)
    {
        push(@validateList, $self->tpPlatformGetLogicalPortList($_, "local,lag"));
    }
    @validateList = sort { $a <=> $b } arrayUnique(@validateList);
    my @globalPortList = $self->validateExplicitList($FALSE, $port,
                                                        @validateList);
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @list = $self->tpPlatformGetLogicalPortList($switchNum,
                                                        "local,lag");
        my @portList = $self->validateExplicitList($TRUE, $port, @list);
        
        if (scalar(@portList) == 0)
        {
            # None of the ports appear on this switch
            next;
        }
        
        push(@affectedPortList, @portList);

        my @valid = $self->_tpIsValidVLANOnSwitch($switchNum, @vlanList);
        my %valid = map {$vlanList[$_] => $valid[$_]} (0 .. $#vlanList);
        VID: foreach my $vlanID (@vlanList)
        {
            if ($valid{$vlanID} == $FALSE)
            {
                my $ports = $self->StringifyList(@portList);
                my $moreThanOnePort = $ports =~ /\D/;
                
                printf("VLAN $vlanID does not appear on switch $switchNum for port%s $ports.\n",
                    $moreThanOnePort ? "s" : "");
                next VID;
            }
    
            foreach my $port (@portList)
            {
                # Map port number for the benefit of non-SWAG Vegas. Ignore the
                # returned sw since it should match switchNum and may actually be
                # wrong for some platforms.
                my ($sw, $logicalPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
                $chip->fmAddVlanPort($switchNum, $vlanID, $logicalPort, $FALSE);
            
            }   # for each port
            
        }   # for each VLAN
        
    }   # for each switch
    
    # Warn user if some ports were not operated on.
     $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    
    # FIXME: API status checking has to be added.
    return $FM_OK;
}

##@cmethod public int tpHandleCreateVlan(char *vlan)
#
# @desc         Handles creating a set of VLANs
#
# @param[in]    vlan The set of VLANs to be created
#
# @note         It is assumed that on a multi-chip system the VLAN tables on
#               all chips are in sync.
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleCreateVlan
{
    my ($self, $vlan) = @_;

    my $chip = $self->{CHIP};

    my @vlanList = $self->validateList($vlan, $self->tpPlatformGetVLANRange);
    if (!defined($vlan) || scalar(@vlanList) == 0)
    {
        print($TP_MSG_ERR_VLAN_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $vlanID (@vlanList)
    {
        foreach my $switchNum ($self->tpGetSwitches)
        {
            $chip->disableErrors();
            my $status = $chip->fmCreateVlan($switchNum, $vlanID);
            $chip->enableErrors();
            if ($status != $FM_OK)
            {
                $result = $FM_FAIL;
                printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                printf("VLAN %s cannot be created (errno=%d)\n", $vlanID,$status);
            }
        }
    }
    return $result;
}

##@cmethod public int tpHandleDelVlan(char *vlan)
#
# @desc         Handles deleting a set of VLANs
#
# @param[in]    vlan The set of VLANs to be deleted
#
# @note         It is assumed that on a multi-chip system the VLAN tables on
#               all chips are in sync.
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleDelVlan
{
    my ($self, $vlan) = @_;

    my $chip = $self->{CHIP};

    my @vlanList = $self->validateList($vlan, $self->tpPlatformGetVLANRange);
    if (!defined($vlan) || (scalar(@vlanList) == 0))
    {
        print($TP_MSG_ERR_VLAN_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @valid = $self->_tpIsValidVLANOnSwitch($switchNum, @vlanList);
        my %valid = map {$vlanList[$_] => $valid[$_]} (0 .. $#vlanList);
        VID: foreach my $vlanID (@vlanList)
        {
            if ($valid{$vlanID} == $FALSE)
            {
                # Remove this annoying message
                # printf("VLAN $vlanID does not appear on switch $switchNum.\n");
                next VID;
            }
    
            $chip->disableErrors();
            my $status = $chip->fmDeleteVlan($switchNum, $vlanID);
            $chip->enableErrors();
            if ($status != $FM_OK)
            {
                $result = $FM_FAIL;
                printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                printf("VLAN %s cannot be deleted! (errno=%d)\n", $vlanID, $status);
            }
            
        }   # for each VLAN
        
    }   # for each switch
    
    return $result;
}

##@cmethod public int tpHandleDelVlanCounter(int vlanID)
#
# @desc         Handles deleting VLAN statistics counters associated with a
#               VLAN
#
# @param[in]    vlanID The VLAN ID the VLAN statistics counters are to be
#               deleted from
#
# @note         It is assumed that on a multi-chip system the VLAN tables on
#               all chips are in sync.
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleDelVlanCounter
{
    my ($self, $vlanID) = @_;

    my $chip = $self->{CHIP};

    $vlanID = $self->str2intnum($vlanID);
    if (!defined($vlanID))
    {
        print($TP_MSG_ERR_VLAN_INVALID_SCALAR);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    if (!$self->_tpIsValidVLAN($vlanID))
    {
        print($TP_MSG_ERR_VLAN_INVALID_ID);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        $chip->disableErrors();
        my $status = $chip->fmFreeVLANCounters($switchNum, $vlanID);
        $chip->enableErrors();
        if ($status != $FM_OK)
        {
            $result = $FM_FAIL;
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            if ($status == $FM_ERR_NO_VLANCOUNTER)
            {
                printf($TP_MSG_WARN_VLAN_INVALID_COUNTER, $vlanID);
                next;
            }
            printf("VLAN counters cannot be deleted from VLAN %d!\n", $vlanID);
        }
    }
    return $result;
}

##@cmethod public int tpHandleDelVlanPort(char *vlan, char *port)
#
# @desc         Handles deleting a set of ports from a set of VLANs
#
# @param[in]    vlan The set of VLAN the set of ports are to be deleted from
#
# @param[in]    port The set of ports to be deleted
#
# @note         It is assumed that on a multi-chip system the VLAN tables on
#               all chips are in sync.
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleDelVlanPort
{
    my ($self, $vlan, $port) = @_;

    my $chip = $self->{CHIP};

    if (!defined($vlan) || !defined($port))
    {
        print("Must specify <vlan> <port>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @vlanList = $self->validateList($vlan, $self->tpPlatformGetVLANRange);
    if (!defined($vlan) || scalar(@vlanList) == 0)
    {
        print($TP_MSG_ERR_VLAN_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @affectedPortList = ();
    my @validateList = ();
    foreach ($self->tpGetSwitches)
    {
        push(@validateList, $self->tpPlatformGetLogicalPortList($_, "local,lag"));
    }
    @validateList = sort { $a <=> $b } arrayUnique(@validateList);
    my @globalPortList = $self->validateExplicitList($FALSE, $port,
                                                        @validateList);
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
    
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @list = $self->tpPlatformGetLogicalPortList($switchNum,
                                                        "local,lag");
        my @portList = $self->validateExplicitList($TRUE, $port, @list);
        
        if (scalar(@portList) == 0)
        {
            # None of the ports appear on this switch
            next;
        }
        
        push(@affectedPortList, @portList);

        my @valid = $self->_tpIsValidVLANOnSwitch($switchNum, @vlanList);
        my %valid = map {$vlanList[$_] => $valid[$_]} (0 .. $#vlanList);
        VID: foreach my $vlanID (@vlanList)
        {
            if ($valid{$vlanID} == $FALSE)
            {
                my $ports = $self->StringifyList(@portList);
                my $moreThanOnePort = $ports =~ /\D/;
                
                printf("VLAN $vlanID does not appear on switch $switchNum for port%s $ports.\n",
                    $moreThanOnePort ? "s" : "");
                next VID;
            }
    
            foreach my $port (@portList)
            {
                # Map port number for the benefit of non-SWAG Vegas. Ignore the
                # returned sw since it should match switchNum and may actually be
                # wrong for some platforms.
                my ($sw, $logicalPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
                
                $chip->disableErrors();
                my $status = 
                        $chip->fmDeleteVlanPort($switchNum, $vlanID, $logicalPort);
                $chip->enableErrors();
                if ($status != $FM_OK)
                {
                    $result = $FM_FAIL;
                    printf("Port %port (on switch $switchNum) cannot be deleted from VLAN $vlanID!\n");
                }
            
            }   # for each port
            
        }   # for each VLAN
        
    }   # for each switch
    
    # Warn user if some ports were not operated on.
     $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    
    return $result;
}

##@cmethod public int tpHandleResetVlan(void)
#
# @desc         Handles resetting all VLAN tables
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub tpHandleResetVlan
{
    my ($self, $type) = @_;  # is this a bug ???? - type isn't used

    my $chip = $self->{CHIP};

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    my ($minimum, $maximum) = $self->tpPlatformGetVLANRange;
    SWITCH: foreach my $switchNum ($self->tpGetSwitches)
    {
        my $vlanList = [(0) x $FM_MAX_VLAN];
        my ($nVLAN, $status);

        $chip->disableErrors();
        $status = $chip->fmGetVlanList($switchNum, \$nVLAN,
                                       $vlanList, $FM_MAX_VLAN);
        $chip->enableErrors();
        if ($status != $FM_OK)
        {
            $result = $FM_FAIL;
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            print("VLAN tables cannot be reset!\n");
            next SWITCH;
        }

        VID: for (my $i = 0; $i < $nVLAN; $i++)
        {
            my $vlanID = $vlanList->[$i];

            if ($vlanID >= $minimum && $vlanID <= $maximum)
            {
                $chip->disableErrors();
                $status = $chip->fmDeleteVlan($switchNum, $vlanID);
                $chip->enableErrors();
                if ($status != $FM_OK)
                {
                    $result = $FM_FAIL;
                    printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                    printf("VLAN %d cannot be deleted!\n", $vlanID);
                }
            }
        }
    }
    return $result;
}

##@cmethod public int tpHandleResetVlanCounter(char *vlan)
#
# @desc         Handles resetting VLAN counters
#
# @param[in]    vlan The set of VLAN IDs whose associated VLAN counters are to
#               be reset
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleResetVlanCounter
{
    my ($self, $vlan) = @_;

    my $chip = $self->{CHIP};

    my @vlanList = $self->validateList($vlan, $self->tpPlatformGetVLANRange);
    if (!defined($vlan) || scalar(@vlanList) == 0)
    {
        print($TP_MSG_ERR_VLAN_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    my @valid = $self->_tpIsValidVLAN(@vlanList);
    my %valid = map {$vlanList[$_] => $valid[$_]} (0 .. $#vlanList);
    VID: foreach my $vlanID (@vlanList)
    {
        if ($valid{$vlanID} == $FALSE)
        {
            printf($TP_MSG_WARN_VLAN_INVALID_ID, $vlanID);
            next VID; 
        }

        SWITCH: foreach my $switchNum ($self->tpGetSwitches)
        {
            $chip->disableErrors();
            my $status = $chip->fmResetVLANCounters($switchNum, $vlanID);
            $chip->enableErrors();
            if ($status != $FM_OK)
            {
                $result = $FM_FAIL;
                printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                if ($status == $FM_ERR_NO_VLANCOUNTER)
                {
                    printf($TP_MSG_WARN_VLAN_INVALID_COUNTER, $vlanID);
                }
                printf("Counters associated with VLAN %d cannot be reset!\n",
                       $vlanID);
            }
        }
    }
    return $result;
}

##@cmethod public int tpHandleSetVlanConfig(char *attribute,
#                                           char *vlan,
#                                           char *state)
#
# @desc         Handles setting attributes for a set of VLANs
#
# @param[in]    attribute The attribute to be set
#
# @param[in]    vlan The set of VLANs for which the reflect is to be set
#
# @param[in]    state The attribute state. Valid values are "off" and "on"
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
sub tpHandleSetVlanConfig
{
    my ($self, $attribute, $vlan, $state) = @_;

    my $chip = $self->{CHIP};

    if (!defined($attribute) || !defined($vlan) || !defined($state))
    {
        print("Must specify <attribute> <vlan> <state>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (!exists($__vlanAttrMap{$attribute}))
    {
        print("Must specify a valid VLAN attribute!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @vlanList = $self->validateList($vlan, $self->tpPlatformGetVLANRange);
    if (!defined($vlan) || (scalar(@vlanList) == 0))
    {
        print($TP_MSG_ERR_VLAN_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (!defined($state) || 
        ($attribute ne "mtu" && $state ne "on" && $state ne "off"))
    {
        print("Must specify a valid VLAN attribute state!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    my @valid = $self->_tpIsValidVLAN(@vlanList);
    my %valid = map {$vlanList[$_] => $valid[$_]} (0 .. $#vlanList);

    my %void;

    if ($attribute eq "mtu")
    {
        %void = (type => "fm_int", value => $state);
    }
    else
    {
        %void = (type => "fm_bool", value => ($state eq "on") ? 1 : 0);
    }
    VID: foreach my $vlanID (@vlanList)
    {
        if ($valid{$vlanID} == $FALSE)
        {
            printf($TP_MSG_WARN_VLAN_INVALID_ID, $vlanID);
            next VID;
        }

        SWITCH: foreach my $switchNum ($self->tpGetSwitches)
        {
            $chip->fmSetVlanAttribute($switchNum, $vlanID,
                                      $__vlanAttrMap{$attribute}, \%void);
        }
    }
    # FIXME: API status checking has to be added.
    return $FM_OK;
}

##@cmethod public int tpHandleSetVlanSpt(char *vlan, char *port, char *state)
#
# @desc         Handles setting the VLAN spanning-tree state of a set of ports
#               for a set of VLANs
#
# @param[in]    vlan The set of VLANs for which the spanning-tree state is to
#               be set
#
# @param[in]    port The set of ports whose spanning-tree state is to be set
#
# @param[in]    state The spanning-tree state. Valid values are "forwarding",
#               "learning", "listening" and "discarding"
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleSetVlanSpt
{
    my ($self, $vlan, $port, $state) = @_;

    my $chip = $self->{CHIP};

    my %stpStates = (
        forwarding      => $FM_STP_STATE_FORWARDING,
        learning        => $FM_STP_STATE_LEARNING,
        listening       => $FM_STP_STATE_LISTENING,
        discarding      => $FM_STP_STATE_DISABLED,
        blocking        => $FM_STP_STATE_BLOCKING
    );

    if (!defined($vlan) || !defined($port) || !defined($state))
    {
        print("Must specify <vlan> <port> <state>\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @vlanList = $self->validateList($vlan, $self->tpPlatformGetVLANRange);
    if (scalar(@vlanList) == 0)
    {
        print($TP_MSG_ERR_VLAN_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port,
                                    $self->tpPlatformGetFaceplatePortRange());
    if (!defined ($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (!exists($stpStates{$state}))
    {
        print("Must specify valid spanning-tree state!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
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

        my @indices = arrayFind(map {$self->tpPlatformIsCPUPort($_)} @portList);
        if ($port ne "all" && scalar(@indices) > 0)
        {
            print($TP_MSG_ERR_PORT_INVALID_ARRAY);
            return $FM_ERR_INVALID_ARGUMENT;
        }

        my @valid = $self->_tpIsValidVLANOnSwitch($switchNum, @vlanList);
        my %valid = map {$vlanList[$_] => $valid[$_]} (0 .. $#vlanList);
        VID: foreach my $vlanID (@vlanList)
        {
            if ($vlan eq "all")
            {
                if ( ($vlanID % 100) == 0 )
                {
                    printf("setting stp state for vlan %d\n", $vlanID);
                }
            }
            if ($valid{$vlanID} == $FALSE)
            {
                my $ports = $self->StringifyList(@portList);
                my $moreThanOnePort = $ports =~ /\D/;
                
                printf("VLAN $vlanID does not appear on switch $switchNum for port%s $ports.\n",
                    $moreThanOnePort ? "s" : "");
                next VID;
            }

            PORT: foreach my $globalPort (@portList)
            {
                # Map port number for the benefit of non-SWAG Vegas. Ignore the
                # returned sw since it should match switchNum and may actually be
                # wrong for some platforms.
                my ($sw, $logicalPort) = $self->tpPlatformMapGlobalToLogicalPort($globalPort);
                next PORT if $self->tpPlatformIsCPUPort($globalPort);

                $chip->disableErrors();
                my $status = $chip->fmSetVlanPortState($switchNum, $vlanID,
                                                       $logicalPort,
                                                       $stpStates{$state});
                $chip->enableErrors();
                if ($status != $FM_OK)
                {
                    $result = $FM_FAIL;
                    printf("Spanning-tree state cannot be changed to %s for port %d\n", 
                           $stpStates{$state}, $globalPort);
                }
                
            }   # for each port
            
        }   # for each VLAN
        
    }   # for each switch

    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);

    return $result;
}

##@cmethod public int tpHandleSetVlanTag(char *vlan, char *port, char *tag)
#
# @desc         Handles setting the tagging state of a set of ports for a set
#               of VLANs
#
# @param[in]    vlan The set of VLANs for which the tagging state is to be set
#
# @param[in]    port The set of ports whose tagging state is to be set
#
# @param[in]    tag The tagging state. Valid values are "tag" and "untag"
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleSetVlanTag
{
    my ($self, $vlan, $port, $tag) = @_;

    my $chip = $self->{CHIP};

    my @affectedPortList = ();

    if (!defined($vlan) || !defined($port) || !defined($tag))
    {
        print("Must specify <vlan> <port> <tag>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (!defined($tag) || ($tag ne "tag" && $tag ne "untag" && $tag ne "tag2" && $tag ne "untag2"))
    {
        print("Must specify valid tagging state!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @vlanList = $self->validateList($vlan, $self->tpPlatformGetVLANRange);
    if (!defined($vlan) || scalar(@vlanList) == 0)
    {
        print($TP_MSG_ERR_VLAN_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    # slightly different than usual multiswitch config due to vegas swag.
    my @globalPortList = $self->validateList($port, 
        $self->tpPlatformGetFaceplatePortRange);
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port,
            $self->tpPlatformGetSwitchPortList($sw));

        if (scalar(@portList) == 0)
        {
            next;
        }

        push(@affectedPortList, @portList);

        my @valid = $self->_tpIsValidVLAN(@vlanList);
        my %valid = map {$vlanList[$_] => $valid[$_]} (0 .. $#vlanList);
        VID: foreach my $vlanID (@vlanList)
        {
            if ($valid{$vlanID} == $FALSE)
            {
                printf($TP_MSG_WARN_VLAN_INVALID_ID, $vlanID);
                next VID;
            }

            PORT: foreach my $globalPort (@portList)
            {
                my ($switchNum, $logicalPort) =
                    $self->tpPlatformMapGlobalToLogicalPort($globalPort);
                my ($status);

                $chip->disableErrors();
                if ($tag eq "tag" || $tag eq "untag")
                {
                    $status = $chip->fmChangeVlanPort($sw, $vlanID, $logicalPort,
                                                      $tag eq "tag" ? $TRUE : $FALSE);
                }
                else
                {
                    $status = $chip->fmChangeVlanPortExt($sw, $FM_VLAN_SELECT_VLAN2,
                                                         $vlanID, $logicalPort,
                                                         $tag eq "tag2" ? $TRUE : $FALSE);
                }

                $chip->enableErrors();
                if ($status != $FM_OK)
                {
                    $result = $FM_FAIL;
                    printf("%s\n", $chip->fmErrorMsg($status));
                    printf("sw: $sw  vid: $vlanID  port: $logicalPort  tag:
                        $tag\n");
                    printf("Tagging state cannot be set to %s for port %d!\n",
                           $tag, $globalPort);
                }
            }
        }
    }

    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);

    return $result;
}

##@cmethod public int tpHandleShowVlan(char *vlan)
#
# @desc         Handles showing the VLAN table
#
# @param[in]    vlan The set of VLANs to be shown. If set to @c undef, all
#               VLANs are shown
#
# @note         It is assumed that on a multi-chip system the VLAN tables on
#               all chips are in sync.
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
sub tpHandleShowVlan
{
    my ($self, $vlan) = @_;

    my $chip = $self->{CHIP};
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    my $info = SDK::fm_switchInfo->new();

    $vlan = !defined($vlan) ? "all" : $vlan;
    my %vlanList = map {
        $_ => $TRUE
    } $self->validateList($vlan, $self->tpPlatformGetVLANRange);
    if (scalar(keys(%vlanList)) == 0)
    {
        print($TP_MSG_ERR_VLAN_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my %reflect = (type => "fm_bool", value => 0);
    
    for my $sw ($self->tpGetSwitches)
    {
        my ($currentPort, $currentVlan, $isTagged, $nextPort, $nextVlan);
    
        $chip->fmGetSwitchInfo($sw, $info);
        
        printf("\nSwitch %d:", $sw) if ($switchCount > 1);
        printf("\n%-4s %-4s %-5s %-s\n", "VLAN", "REF.", "MTU", "MEMBERSHIP/TAGGING");
        print("------------------------------------------------------------\n");
    
        $chip->fmGetVlanFirst($sw, \$currentVlan);
        while ($currentVlan != -1)
        {
            my %tagTable = ();
            if (exists($vlanList{$currentVlan}))
            {
                $chip->fmGetVlanAttribute($sw, $currentVlan,
                                          $FM_VLAN_REFLECT, \%reflect);
                printf("%4d %4d ", $currentVlan, $reflect{value});
    
                if ($info->{'switchFamily'} != $FM_SWITCH_FAMILY_FM2000)
                {
                    my $mtuEntry = SDK::fm_mtuEntry->new();
                    my %mtuIndex = (type => "fm_int", value => 0);
                    my %void = (type => "fm_mtuEntry", value => $mtuEntry);
    
                    $chip->fmGetVlanAttribute($sw, $currentVlan,
                                              $FM_VLAN_MTU, \%mtuIndex);
    
                    $mtuEntry->{'index'} = $mtuIndex{value};
                    $chip->fmGetSwitchAttribute($sw, $FM_MTU_LIST, \%void);
    
                    printf("%-5d ", $mtuEntry->{'mtu'});
                }
                else
                {
                    printf("%-5s ", "--");
                }

                $chip->disableErrors();
    
                my $status = $chip->fmGetVlanPortFirst($sw,
                                                       $currentVlan,
                                                       \$currentPort);
                while ($status == $FM_OK && $currentPort != -1)
                {
                    my $faceplatePort =
                      $self->tpPlatformMapLogicalToFaceplatePort($sw,
                                                                 $currentPort);
                    if ($faceplatePort != -1)
                    {
                        $chip->fmGetVlanPortTag($sw, $currentVlan,
                                                $currentPort, \$isTagged);
                        $tagTable{$faceplatePort} = $isTagged;
                    }

                    $status = $chip->fmGetVlanPortNext($sw,
                                                       $currentVlan,
                                                       $currentPort,
                                                       \$nextPort);
                    $currentPort = $nextPort;
                }

                $chip->enableErrors();
    
                my @vlanPorts = sort {$a <=> $b} keys(%tagTable);
                my @tagTable = map {$tagTable{$_}} @vlanPorts;
                if (scalar(@vlanPorts) > 0)
                {
                    map {printf("%-2d ", $_)} @vlanPorts;
                }
                else
                {
                    print("No members");
                }
                printf("\n%16s"," ");
                map {printf("%-2s ", $_ ? "T" : "U")} @tagTable;
                print("\n\n");
            }
    
            $chip->fmGetVlanNext($sw, $currentVlan, \$nextVlan);
            $currentVlan = $nextVlan;
            
        }   # until last VLAN
        
    }   # for each switch
    
    # FIXME: API status checking has to be added.
    return $FM_OK;
}

##@cmethod public int tpHandleShowVlanCounters(int vlanID)
#
# @desc         Shows VLAN statistics counters for the given VLAN ID
#
# @param[in]    vlanID The VLAN ID to show statistics for
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleShowVlanCounters
{
    my ($self, $vlanID) = @_;

    my $chip = $self->{CHIP};

    $vlanID = $self->str2intnum($vlanID);
    if (!defined($vlanID))
    {
        print($TP_MSG_ERR_VLAN_INVALID_SCALAR);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    if (!$self->_tpIsValidVLAN($vlanID))
    {
        print($TP_MSG_ERR_VLAN_INVALID_ID);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    my $vlanCounters;
    foreach my $switchNum ($self->tpGetSwitches)
    {
        $chip->disableErrors();
        my $status =
                 $chip->fmGetVLANCounters($switchNum, $vlanID, \$vlanCounters);
        $chip->enableErrors();
        if ($status != $FM_OK)
        {
            $result = $FM_FAIL;
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            if ($status == $FM_ERR_NO_VLANCOUNTER)
            {
                printf($TP_MSG_WARN_VLAN_INVALID_COUNTER, $vlanID);
                next;
            }
            printf("Counters associated with VLAN %d cannot be shown!\n",
                   $vlanID);
            next;
        }

        print("\n");
        printf("Switch %d: ", $switchNum) if ($switchCount > 1);
        printf("Receive statistics for VLAN %d\n", $vlanID);
        print("-------------------------------------------------------------\n");
        if ($vlanCounters->{cntVersion} & 5)
        {
            $self->printCounter($vlanCounters->{cntUcstPkts},
                                "Unicast frames");
            $self->printCounter($vlanCounters->{cntXcstPkts},
                                "Multicast and broadcast frames");
            $self->printCounter($vlanCounters->{cntUcstOctets},
                                "Octets in unicast frames");
            $self->printCounter($vlanCounters->{cntXcstOctets},
                                "Octets in multicast and broadcast frames");
        }
        else
        {
            $self->printCounter($vlanCounters->{cntRxUcstPkts},
                                "Received unicast frames");
            $self->printCounter($vlanCounters->{cntRxMcstPkts},
                                "Received multicast frames");
            $self->printCounter($vlanCounters->{cntRxBcstPkts},
                                "Received broadcast frames");
            $self->printCounter($vlanCounters->{cntRxDropPkts},
                                "Frames dropped on ingress");
            $self->printCounter($vlanCounters->{cntRxUcstOctets},
                                "Received unicast octets");
            $self->printCounter($vlanCounters->{cntRxMcstOctets},
                                "Received multicast octets");
            $self->printCounter($vlanCounters->{cntRxBcstOctets},
                                "Received broadcast octets");
            $self->printCounter($vlanCounters->{cntRxDropOctets},
                                "Octets dropped on ingress");

            $self->printCounter($vlanCounters->{cntTxUcstPkts},
                                "Transmitted unicast frames");
            $self->printCounter($vlanCounters->{cntTxMcstPkts},
                                "Transmitted multicast frames");
            $self->printCounter($vlanCounters->{cntTxBcstPkts},
                                "Transmitted broadcast frames");
            $self->printCounter($vlanCounters->{cntTxDropPkts},
                                "Frames dropped on ingress");
            $self->printCounter($vlanCounters->{cntTxUcstOctets},
                                "Transmitted unicast octets");
            $self->printCounter($vlanCounters->{cntTxMcstOctets},
                                "Transmitted multicast octets");
            $self->printCounter($vlanCounters->{cntTxBcstOctets},
                                "Transmitted broadcast octets");
            $self->printCounter($vlanCounters->{cntTxDropOctets},
                                "Octets dropped on egress");
        }

        print("\n");
    }
    return $result;
}

##@cmethod public int tpHandleShowVlanSpt(char *vlan)
#
# @desc         Handles showing the VLAN spanning tree state
#
# @param[in]    vlan The set of VLAN IDs for which the spanning tree state is
#               to be shown
#
# @note         It is assumed that on a multi-chip system the VLAN tables on
#               all chips are in sync.
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
sub tpHandleShowVlanSpt
{
    my ($self,$vlan) = @_;

    my $chip = $self->{CHIP};

    my @vlanList = $self->validateList($vlan, $self->tpPlatformGetVLANRange);
    if (!defined($vlan) || scalar(@vlanList) == 0)
    {
        print($TP_MSG_ERR_VLAN_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    print("\nSpanning Tree States: Forwarding(Fw), Learning(Le),");
    print("\n                      Listening(Li), Discarding(D), Blocking(B)\n");

    printf("\n%-4s %-s\n", "VLAN", "Spanning Tree State");
    print("------------------------------------------------------------\n");

    my ($currentPort, $nextPort, $portState);

    my @valid = $self->_tpIsValidVLAN(@vlanList);
    my %valid = map {$vlanList[$_] => $valid[$_]} (0 .. $#vlanList);
    foreach my $vlanID (@vlanList)
    {
        if ($valid{$vlanID} == $FALSE)
        {
            next;
        }

        my %stpStates = ();
        foreach my $switchNum ($self->tpGetSwitches)
        {
            my ($firstPort, $lastPort) = $self->tpPlatformGetPortRange();
            foreach $currentPort ($firstPort..$lastPort)
            {
                my $faceplatePort =
                      $self->tpPlatformMapLogicalToFaceplatePort($switchNum,
                                                                 $currentPort);
                if ($faceplatePort != -1)
                {
                    if ($self->tpPlatformIsCPUPort($faceplatePort))
                    {
                        # The STP state for the CPU port is always set to
                        # FORWARDING.
                        $stpStates{$faceplatePort} = "Fw";
                    }
                    else
                    {
                        $chip->fmGetVlanPortState($switchNum, $vlanID,
                                                  $currentPort, \$portState);

                        if ($portState == $FM_STP_STATE_FORWARDING)
                        {
                            $stpStates{$faceplatePort} = "Fw";
                        }
                        elsif ($portState == $FM_STP_STATE_LISTENING)
                        {
                            $stpStates{$faceplatePort} = "Li";
                        }
                        elsif ($portState == $FM_STP_STATE_LEARNING)
                        {
                            $stpStates{$faceplatePort} = "Le";
                        }
                        elsif ($portState == $FM_STP_STATE_BLOCKING)
                        {
                            $stpStates{$faceplatePort} = "B";
                        }
                        else
                        {
                            $stpStates{$faceplatePort} = "D";
                        }
                    }
                }
            }
        }

        printf("%-4d%-4s", $vlanID, " ");
        my @vlanPorts = sort {$a <=> $b} keys(%stpStates);
        my @stpStates = map {$stpStates{$_}} @vlanPorts;
        map {printf("%-2d ", $_)} @vlanPorts;
        printf("\n%-8s", " ");
        map {printf("%-2s ", $_)} @stpStates;
        print("\n");
    }
    # FIXME: API status checking has to be added.
    return $FM_OK;
}

1;
