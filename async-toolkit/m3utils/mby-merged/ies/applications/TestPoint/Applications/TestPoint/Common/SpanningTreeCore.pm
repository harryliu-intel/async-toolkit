# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/SpanningTreeCore.pm
# Creation Date:    Jan. 13, 2008
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

package Applications::TestPoint::Common::SpanningTreeCore;
use strict;
use warnings;

use base qw(Exporter);

use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Applications::TestPoint::Common::Utilities;
use Scripts::Utilities;
our @EXPORT = qw(
    tpHandleResetMultipleStp
    tpHandleSetSpanningTreePortState
    tpHandleShowSpanningTree
    tpHandleCreateSpanningTree
    tpHandleDeleteSpanningTree
    tpHandleDeleteVlanSpanningTree
    tpHandleAddVlanSpanningTree
);

sub _tpGetStpInstances
{
    my ($self, $switchNum) = @_;

    my $chip = $self->{CHIP};

    my @instances = ();
    my $currentInstance = -1;
    my $nextInstance = -1;
    my $status = $FM_OK;

    $chip->disableErrors();
    $status = $chip->fmGetSpanningTreeFirst($switchNum, \$currentInstance);
    $chip->enableErrors();

    while ($status != $FM_ERR_NO_MORE)
    {
        push(@instances, $currentInstance);

        $chip->disableErrors();
        $status = $chip->fmGetSpanningTreeNext($switchNum, 
                                               $currentInstance,
                                               \$nextInstance);
        $chip->enableErrors();

        $currentInstance = $nextInstance;
    }

    return @instances;
}

sub _tpGetStpInstanceVlans
{
    my ($self, $switchNum, $instance) = @_;

    my $chip = $self->{CHIP};

    my @vlans = ();
    my $currentVlan = -1;
    my $nextVlan = -1;
    my $status = $FM_OK;

    $chip->disableErrors();
    $status = $chip->fmGetSpanningTreeVlanFirst($switchNum, 
                                                $instance, 
                                                \$currentVlan);
    $chip->enableErrors();

    while ( ($status != $FM_ERR_NO_MORE) && ($status != $FM_ERR_NOT_FOUND ))
    {
        push(@vlans, $currentVlan);

        $chip->disableErrors();
        $status = $chip->fmGetSpanningTreeVlanNext($switchNum, 
                                                   $instance,
                                                   $currentVlan,
                                                   \$nextVlan);
        $chip->enableErrors();

        $currentVlan = $nextVlan;
    }

    return @vlans;
}
##@cmethod public int tpHandleResetMultipleStp()
#
# @desc         Resets the multiple spanning tree state to defaults.
#
# @note         This function only executes succesfully when the switch
#               spanning-tree mode is set correctly to multiple.
#
# @return       FM_OK if successful
sub tpHandleResetMultipleStp
{
    my ($self) =  @_;

    my $chip = $self->{CHIP};

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @instances = $self->_tpGetStpInstances($switchNum);
        my $status = $FM_OK;

        foreach my $delInst (@instances)
        {
            if ($delInst != 0)
            {
                $chip->disableErrors();
                $status = $chip->fmDeleteSpanningTree($switchNum, $delInst);
                $chip->enableErrors();
                
                if ($status != $FM_OK)
                {
                    printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                    printf("Cannot reset spanning-tree state (deleting instance %d)\n",
                           $delInst);
                }
            }
        }
    }

    return $FM_OK;
}

##@cmethod public int tpHandleSetSpanningTreePortState(int instance, int port)
#
# @desc         Sets the port state of a port in a STP instance.
#
# @param[in]    instance is the STP instance number to set the state on.
#
# @param[in]    port is the logical port number to set the state on.
#
# @param[in]    state is the spanning-tree state to set.
#
# @note         This function only executes succesfully when the switch
#               spanning-tree mode is set correctly to multiple.
#
# @return       FM_OK if successful
sub tpHandleSetSpanningTreePortState
{
    my ($self, $instance, $port, $state) =  @_;

    my $chip = $self->{CHIP};

    my @affectedPortList = ();

    my %stpStates = (
        forwarding      => $FM_STP_STATE_FORWARDING,
        learning        => $FM_STP_STATE_LEARNING,
        listening       => $FM_STP_STATE_LISTENING,
        discarding      => $FM_STP_STATE_DISABLED,
        blocking        => $FM_STP_STATE_BLOCKING
    );

    if (!defined($instance) || !defined($port) || !defined($state))
    {
        print("Must specify <instance> <port> <state>\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my %void = (type => 'fm_int', value => -1);
        $chip->fmGetSwitchAttribute($sw, $FM_SPANNING_TREE_MODE, \%void);
        if ($void{value} ne $FM_SPANNING_TREE_MULTIPLE)
        {
            printf("This operation is only valid when the switch spanning-tree mode is 'multiple'.\n");
            return $FM_ERR_INVALID_STP_MODE;
        }
    }

    my @instanceList = $self->validateList($instance, 
                                           $self->tpPlatformGetVLANRange);
    if (scalar(@instanceList) == 0)
    {
        print($TP_MSG_ERR_STP_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }


    my @globalPortList = $self->validateList($port,
        $self->tpPlatformGetFaceplatePortRange());

    if (scalar(@globalPortList) == 0)
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

        my @indices = arrayFind(map {$self->tpPlatformIsCPUPort($_)} @portList);
        if ($port ne "all" && scalar(@indices) > 0)
        {
            print($TP_MSG_ERR_PORT_INVALID_ARRAY);
            return $FM_ERR_INVALID_ARGUMENT;
        }

        if (!exists($stpStates{$state}))
        {
            print("Must specify valid spanning-tree state!\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }

        IID: foreach my $instanceID (@instanceList)
        {
            PORT: foreach my $globalPort (@portList)
            {
                next PORT if $self->tpPlatformIsCPUPort($globalPort);

                my ($switchNum, $logicalPort) = 
                              $self->tpPlatformMapGlobalToLogicalPort($globalPort);

                $chip->disableErrors();
                my $status = $chip->fmSetSpanningTreePortState($sw,
                                                               $instanceID,
                                                               $logicalPort,
                                                               $stpStates{$state});
                $chip->enableErrors();
                if ($status != $FM_OK)
                {
                    $result = $FM_FAIL;
                    printf("Spanning-tree state cannot be changed to %s for port %d in instance %d: %s\n", 
                           $stpStates{$state}, $globalPort, $instanceID,
                           $chip->fmErrorMsg($status));
                }
            }
        }
    }

    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);

    return $result;
}


##@cmethod public int tpHandleShowSpanningTree(int instance)
#
# @desc         Handles showing details about a set of spanning tree instances
#
# @param[in]    instance The set of spanning tree instances to be shown.
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
sub tpHandleShowSpanningTree
{
    my ($self,$instance) = @_;

    my $chip = $self->{CHIP};

    my @instanceList = $self->validateList($instance, $self->tpPlatformGetVLANRange);
    if (!defined($instance) || scalar(@instanceList) == 0)
    {
        print($TP_MSG_ERR_STP_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my %void = (type => 'fm_int', value => -1);
        $chip->fmGetSwitchAttribute($sw, $FM_SPANNING_TREE_MODE, \%void);
        if ($void{value} ne $FM_SPANNING_TREE_MULTIPLE)
        {
            printf("This operation is only valid when the switch spanning-tree mode is 'multiple'.\n");
            return $FM_ERR_INVALID_STP_MODE;
        }
    }

    # override "all" to only existent instances
    if ($instance =~ m/all/i)
    {
        # assume all switches have identical instance lists
        @instanceList = $self->_tpGetStpInstances(0);
    }

    print("\nSpanning Tree States: Forwarding(Fw), Learning(Le),");
    print("\n                      Listening(Li), Discarding(D), Blocking(B)\n");

    my ($currentPort, $nextPort, $portState);

    foreach my $instanceNumber (@instanceList)
    {
        my $status = $FM_OK;

        printf("\nInstance $instanceNumber\n");
        printf("------------------------------------------------------------\n");
        printf("Vlan Members    ");

        if ($instanceNumber == 0)
        {
            printf("(All unassigned VLANs)\n");
        }
        else
        {
            my @vlanMembers 
                = $self->_tpGetStpInstanceVlans(0, $instanceNumber);

            printf("%s\n", $self->StringifyList(@vlanMembers));
        }

        my %stpStates = ();
        foreach my $switchNum ($self->tpGetSwitches)
        {
            my ($firstPort, $lastPort) = $self->tpPlatformGetPortRange();
            foreach my $currentPort ($firstPort..$lastPort)
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
                        $chip->disableErrors();
                        my $status = $chip->fmGetSpanningTreePortState($switchNum, 
                                                                       $instanceNumber,
                                                                       $currentPort, 
                                                                       \$portState);
                        if ($status == $FM_OK)
                        {
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
                        $chip->enableErrors();
                    }
                }
            }
        }

        printf("\nPort States     ");
        my @vlanPorts = sort {$a <=> $b} keys(%stpStates);
        my @stpStates = map {$stpStates{$_}} @vlanPorts;
        map {printf("%-2d ", $_)} @vlanPorts;
        printf("\n%-16s", " ");
        map {printf("%-2s ", $_)} @stpStates;
        print("\n");
    }

    # FIXME: API status checking has to be added.
    return $FM_OK;
}

##@cmethod public int tpHandleCreateSpanningTree(char *instance)
#
# @desc         Creates STP instances.
#
# @param[in]    instance The set of spanning tree instances to be created.
#               Note that this cannot be "all".
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
sub tpHandleCreateSpanningTree
{
    my ($self, $instance) =  @_;

    my $chip = $self->{CHIP};

    if (!defined($instance))
    {
        print("Must specify <instance>\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my %void = (type => 'fm_int', value => -1);
        $chip->fmGetSwitchAttribute($sw, $FM_SPANNING_TREE_MODE, \%void);
    
        if ($void{value} ne $FM_SPANNING_TREE_MULTIPLE)
        {
            printf("This operation is only valid when the switch spanning-tree mode is 'multiple'.\n");
            return $FM_ERR_INVALID_STP_MODE;
        }
    }

    my @instanceList = $self->validateList($instance, 
                                           $self->tpPlatformGetVLANRange);
    if (scalar(@instanceList) == 0)
    {
        print($TP_MSG_ERR_STP_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status = $FM_OK;

        foreach my $instanceID (@instanceList)
        {
            if ($instanceID != 0)
            {
                $chip->disableErrors();
                $status = $chip->fmCreateSpanningTree($switchNum, $instanceID);
                $chip->enableErrors();
                
                if ($status != $FM_OK)
                {
                    printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                    printf("Cannot create spanning-tree instance $instanceID\n");
                }
            }
        }
    }

    return $FM_OK;
}

##@cmethod public int tpHandleDeleteSpanningTree(char *instance)
#
# @desc         Deletes STP instances.
#
# @param[in]    instance The set of spanning tree instances to be deleted.
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
sub tpHandleDeleteSpanningTree
{
    my ($self, $instance) =  @_;

    my $chip = $self->{CHIP};

    if (!defined($instance))
    {
        print("Must specify <instance>\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my %void = (type => 'fm_int', value => -1);
        $chip->fmGetSwitchAttribute($sw, $FM_SPANNING_TREE_MODE, \%void);
        if ($void{value} ne $FM_SPANNING_TREE_MULTIPLE)
        {
            printf("This operation is only valid when the switch spanning-tree mode is 'multiple'.\n");
            return $FM_ERR_INVALID_STP_MODE;
        }
    }

    my @instanceList = $self->validateList($instance, 
                                           $self->tpPlatformGetVLANRange);
    if (scalar(@instanceList) == 0)
    {
        print($TP_MSG_ERR_STP_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status = $FM_OK;

        foreach my $instanceID (@instanceList)
        {
            if ($instanceID != 0)
            {
                $chip->disableErrors();
                $status = $chip->fmDeleteSpanningTree($switchNum, $instanceID);
                $chip->enableErrors();
                
                if ($status != $FM_OK)
                {
                    printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                    printf("Cannot delete spanning-tree instance $instanceID\n");
                }
            }
        }
    }

    return $FM_OK;
}

##@cmethod public int tpHandleDeleteVlanSpanningTree(char *instance, char *vlan)
#
# @desc         Deletes VLAN members from spanning tree instances
#
# @param[in]    instance The set of spanning tree instances to delete VLANs
#               from.
#
# @param[in]    vlan The set of VLAN IDs to delete.
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
sub tpHandleDeleteVlanSpanningTree
{
    my ($self, $instance, $vlan) =  @_;

    my $chip = $self->{CHIP};

    if (!defined($instance) || !defined($vlan))
    {
        print("Must specify <instance> <vlan>\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my %void = (type => 'fm_int', value => -1);
        $chip->fmGetSwitchAttribute($sw, $FM_SPANNING_TREE_MODE, \%void);
        if ($void{value} ne $FM_SPANNING_TREE_MULTIPLE)
        {
            printf("This operation is only valid when the switch spanning-tree mode is 'multiple'.\n");
            return $FM_ERR_INVALID_STP_MODE;
        }
    }

    my @instanceList = $self->validateList($instance, 
                                           $self->tpPlatformGetVLANRange);
    if (scalar(@instanceList) == 0)
    {
        print($TP_MSG_ERR_STP_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @vlanList = $self->validateList($vlan, 
                                           $self->tpPlatformGetVLANRange);
    if (scalar(@vlanList) == 0)
    {
        print($TP_MSG_ERR_VLAN_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status = $FM_OK;

        foreach my $instanceID (@instanceList)
        {
            foreach my $vlanID (@vlanList)
            {
                if ($instanceID != 0)
                {
                    $chip->disableErrors();
                    $status = $chip->fmDeleteSpanningTreeVlan($switchNum, 
                                                              $instanceID,
                                                              $vlanID);
                    $chip->enableErrors();
                    
                    if ($status != $FM_OK)
                    {
                        printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                        printf("Cannot delete VLAN $vlanID from instance $instanceID\n");
                    }
                }
            }
        }
    }

    return $FM_OK;
}

##@cmethod public int tpHandleAddVlanSpanningTree(char *instance, char *vlan)
#
# @desc         Adds VLAN members to spanning tree instances
#
# @param[in]    instance The set of spanning tree instances to add VLANs
#               to.
#
# @param[in]    vlan The set of VLAN IDs to add.
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
sub tpHandleAddVlanSpanningTree
{
    my ($self, $instance, $vlan) =  @_;

    my $chip = $self->{CHIP};

    if (!defined($instance) || !defined($vlan))
    {
        print("Must specify <instance> <vlan>\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my %void = (type => 'fm_int', value => -1);
        $chip->fmGetSwitchAttribute($sw, $FM_SPANNING_TREE_MODE, \%void);
        if ($void{value} ne $FM_SPANNING_TREE_MULTIPLE)
        {
            printf("This operation is only valid when the switch spanning-tree mode is 'multiple'.\n");
            return $FM_ERR_INVALID_STP_MODE;
        }
    }

    my @instanceList = $self->validateList($instance, 
                                           $self->tpPlatformGetVLANRange);
    if (scalar(@instanceList) == 0)
    {
        print($TP_MSG_ERR_STP_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @vlanList = $self->validateList($vlan, 
                                           $self->tpPlatformGetVLANRange);
    if (scalar(@vlanList) == 0)
    {
        print($TP_MSG_ERR_VLAN_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status = $FM_OK;

        foreach my $instanceID (@instanceList)
        {
            foreach my $vlanID (@vlanList)
            {
                if ($instanceID != 0)
                {
                    $chip->disableErrors();
                    $status = $chip->fmAddSpanningTreeVlan($switchNum, 
                                                           $instanceID,
                                                           $vlanID);
                    $chip->enableErrors();
                    
                    if ($status != $FM_OK)
                    {
                        printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                        printf("Cannot add VLAN $vlanID to instance $instanceID\n");
                    }
                }
            }
        }
    }

    return $FM_OK;
}
