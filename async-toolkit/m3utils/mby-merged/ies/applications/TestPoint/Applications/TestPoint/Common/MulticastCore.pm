# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/MulticastCore.pm
# Creation Date:    Oct. 22, 2007
# Description:      FocalPoint multicast group support code
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

package Applications::TestPoint::Common::MulticastCore;
use strict;
use warnings;

use List::Util qw(max min);
use POSIX ();
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;
use Types::tp_ipAddr;
use Types::tp_vpPair;

###############################################################################
#
#                           CONSTANTS
#
###############################################################################

use constant DEBUG  => $FALSE;

###############################################################################
#
#                           LOCAL VARIABLES
#
###############################################################################

##@var <char*,int> __mcastAddrTypes
#
# @brief        Perl HASH containing all recognized multicast group address
#               types
our %__mcastAddrTypes =
(
    'dmac-vlan'     => $FM_MCAST_ADDR_TYPE_L2MAC_VLAN,
    'dip'           => $FM_MCAST_ADDR_TYPE_DSTIP,
    'dip-vlan'      => $FM_MCAST_ADDR_TYPE_DSTIP_VLAN,
    'dip-sip'       => $FM_MCAST_ADDR_TYPE_DSTIP_SRCIP,
    'dip-sip-vlan'  => $FM_MCAST_ADDR_TYPE_DSTIP_SRCIP_VLAN,
);

##@var <char*,int> __mcastAttributes
#
# @brief        Perl HASH containing all recognized multicast attributes
our %__mcastAttributes =
(
    'bypassEgressSTP'  => $FM_MCASTGROUP_BYPASS_EGRESS_STP_CHECK,
    'L2SwitchOnly'     => $FM_MCASTGROUP_L2_SWITCHING_ONLY,
    'L3SwitchOnly'     => $FM_MCASTGROUP_L3_SWITCHING_ONLY,
    'L3Flood-set'      => $FM_MCASTGROUP_L3_FLOOD_SET,
    'repli-group'      => $FM_MCASTGROUP_SHARED_REPLICATION_GROUP,
);

###############################################################################
#
#                           LOCAL FUNCTIONS
#
###############################################################################

##@cmethod private int GetMcastGroupList(int **mcastGroupList)
#
# @brief        Retrieves all multicast groups
#
# @param[in]    mcastGroupList Points to a Perl ARRAY
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub GetMcastGroupList
{
    my ($self, $mcastGroupList) = @_;

    my $chip = $self->{'CHIP'};

    # Initialize the list of multicast groups to the empty list.
    @{$mcastGroupList} = ();

    my ($currentGroup, $nextGroup, $status);

    my $switchNum = shift(@{[$self->tpGetSwitches]});
    $chip->disableErrors();
    $status = $chip->fmGetMcastGroupFirst($switchNum, \$currentGroup);
    if ($status == $FM_ERR_NO_MORE)
    {
        $chip->enableErrors();
        return $FM_OK;
    }
    my $group = 0;
    while ($status == $FM_OK)
    {
        $mcastGroupList->[$group++] = $currentGroup;

        $status = $chip->fmGetMcastGroupNext($switchNum, $currentGroup,
                                             \$nextGroup);
        $currentGroup = $nextGroup;
    }
    $chip->enableErrors();
    return $status == $FM_OK || $status == $FM_ERR_NO_MORE ? $FM_OK : $FM_FAIL;
}

##@cmethod private int ValidateMcastGroup(char *group, int **groupList)
#
# @brief        Validates a multicast group or set of multicast groups
#
# @param[in]    group The multicast group or set of multicast groups to be
#               validated
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub ValidateMcastGroup
{
    my ($self, $group, $groupList) = @_;

    my (@mcastGroupList);

    my $status = $self->GetMcastGroupList(\@mcastGroupList);
    if ($status != $FM_OK)
    {
        return $FM_FAIL;
    }
    @{$groupList} = $self->validateList($group,
                                        min(@mcastGroupList),
                                        max(@mcastGroupList));
    if (scalar(@{$groupList}) == 0)
    {
        return $FM_FAIL;
    }
    my %mcastGroupList = map {$_ => $TRUE} @mcastGroupList;
    foreach my $group (@{$groupList})
    {
        if (!exists($mcastGroupList{$group}))
        {
            return $FM_FAIL;
        }
    }
    return $FM_OK;
}

###############################################################################
#
#                           PUBLIC FUNCTIONS
#
###############################################################################

##@cmethod public int tpHandleAddMulticastListener(char *group,
#                                                  int   port,
#                                                  int   vlan)
#
# @brief        Handles adding a listener, i.e. a port / VLAN ID pair, to a
#               multicast group or set of multicast groups
#
# @param[in]    group The multicast group or set of multicast groups to add the
#               listener to
#
# @param[in]    port The port the listener is connected to
#
# @param[in]    vlan The VLAN the listener is associated with
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleAddMulticastListener
{
    my ($self, $group, $port, $vlan, $remote) = @_;

    my $chip = $self->{'CHIP'};

    my (@groupList, @vlanList);

    if (!defined($group) || !defined($port) || !defined($vlan))
    {
        print("Must specify <group> <port> <vlan>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if ($self->ValidateMcastGroup($group, \@groupList) != $FM_OK)
    {
        print($TP_MSG_ERR_MCAST_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my ($min, $max) = $self->tpPlatformGetFaceplatePortRange();

    my @facePlatePorts = ($min..$max);

    my @globalPortList = $self->validateExplicitList($TRUE,
                                                     $port,
                                                     @facePlatePorts);
    
    foreach my $switchNum ($self->tpGetSwitches)
    {
        push(@globalPortList, 
             $self->validateExplicitList($TRUE, 
                                         $port, 
                 $self->tpPlatformGetLogicalPortList($switchNum, "lag")));
    }

    if (scalar(@globalPortList) == 0)
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $status = $self->ValidateVlan($vlan, \@vlanList);
    if (scalar(@globalPortList) == 1)
    {
        if ($status != $FM_OK || scalar(@vlanList) == 0)
        {
            print($TP_MSG_ERR_VLAN_INVALID_ARRAY);
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }
    else
    {
        if ($status != $FM_OK || scalar(@vlanList) != 1)
        {
            print($TP_MSG_ERR_VLAN_INVALID_SCALAR);
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }

    my $listener = new SDK::fm_multicastListener();
    my $result = $FM_OK;

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    my @affectedPortList = ();

    if (defined($remote) && ($remote ne "on") && ($remote ne "off"))
    {
        print("Must specify valid remote state (on or off)! :",
              " <group> <port> <vlan> <remote>\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }
    elsif ( !defined($remote) ) 
    {
       $remote = "off";
    }

    $chip->disableErrors() if (!DEBUG);
    foreach $group (@groupList)
    {
        foreach my $switchNum ($self->tpGetSwitches)
        {
            my @switchLogicalPortList = $self->tpPlatformGetSwitchPortList($switchNum, 
                                                                           $TRUE);

            my @lagPorts = $self->tpPlatformGetLogicalPortList($switchNum, "lag");

            push (@switchLogicalPortList, 
                  @lagPorts);

            my @portList = $self->validateExplicitList($TRUE, 
                                                       $port,
                                                       @switchLogicalPortList);
                
            if (scalar(@portList) == 0)
            {
                # None of the ports appear on this switch
                next;
            }
            
            push(@affectedPortList, @portList);

            foreach my $globalPort (@portList)
            {
                my $sw;
                my $logicalPort;

                my @isLagPort = $self->validateExplicitList($TRUE, 
                                                            $globalPort,
                                                            @lagPorts);

                if (scalar(@isLagPort == 0))
                {
                    ($sw, $logicalPort) = $self->tpPlatformMapGlobalToLogicalPort($globalPort);
                }
                else
                {
                    $logicalPort = $globalPort;
                }

                foreach my $vlan (@vlanList)
                {
                    $listener->{'port'} = $logicalPort;
                    $listener->{'vlan'} = $vlan;
                    $listener->{'remoteFlag'} = $remote eq "on" ? $TRUE : $FALSE;

                    my $status = $chip->fmAddMcastGroupListener($switchNum,
                                                                $group,
                                                                $listener);
                    if ($status != $FM_OK)
                    {
                        $result = $FM_OK;
                        printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                        printf(  "Cannot add multicast listener %d/%d"
                               . " to multicast group %d!\n",
                               $port, $vlan, $group);
                    }
                }
            }
        }
    }
    $chip->enableErrors() if (!DEBUG);

    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $result;
}

##@cmethod public int tpHandleCreateMcastGroup(void)
#
# @brief        Handles creation and activation of a multicast group
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub tpHandleCreateMcastGroup
{
    my ($self) = @_;

    my $chip = $self->{'CHIP'};

    my $groups = [];
    my $previousGroup = undef;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    $chip->disableErrors() if (!DEBUG);
    foreach my $sw ($self->tpGetSwitches)
    {
        my ($status);

        $status = $chip->fmCreateMcastGroup($sw, \$groups->[$sw]);
        if ($status != $FM_OK
            || (defined($previousGroup) && $groups->[$sw] != $previousGroup))
        {
            goto ABORT;
        }
        $previousGroup = $groups->[$sw];
    }
    $chip->enableErrors() if (!DEBUG);

    printf("Created multicast group %d\n", $previousGroup);
    return $FM_OK;

ABORT:
    print("Cannot create multicast group!\n");
    foreach my $sw ($self->tpGetSwitches)
    {
        if (defined($groups->[$sw]))
        {
            $chip->fmDeleteMcastGroup($sw, $groups->[$sw]);
        }
    }
    $chip->enableErrors() if (!DEBUG);
    return $FM_FAIL;
}

##@cmethod public int tpHandleDelMcastGroup(char *group)
#
# @brief        Handles deleting a single multicast group or set of multicast
#               groups
#
# @param[in]    group The multicast group or set of multicast groups to be
#               deleted
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleDelMcastGroup
{
    my ($self, $group) = @_;

    my $chip = $self->{'CHIP'};

    my (@groupList);

    if ($self->ValidateMcastGroup($group, \@groupList) != $FM_OK)
    {
        print($TP_MSG_ERR_MCAST_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    $chip->disableErrors() if (!DEBUG);
    foreach my $switchNum ($self->tpGetSwitches)
    {
        GROUP: foreach my $group (@groupList)
        {
            my ($status);

            $status = $chip->fmDeactivateMcastGroup($switchNum, $group);
            # Only groups for which a group address was set are active. All
            # other groups are inactive.
            if ($status != $FM_ERR_MCAST_GROUP_NOT_ACTIVE && $status != $FM_OK)
            {
                $result = $FM_FAIL;
                printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                printf("Cannot deactivate multicast group %d!\n", $group);
                next GROUP;
            }
            $status = $chip->fmDeleteMcastGroup($switchNum, $group);
            if ($status != $FM_OK)
            {
                $result = $FM_FAIL;
                printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                printf("Cannot delete multicast group %d!\n", $group);
                next GROUP;
            }
        }
    }
    $chip->enableErrors() if (!DEBUG);
    return $result;
}

##@cmethod public int tpHandleDelMulticastListener(char *group,
#                                                  int   port,
#                                                  int   vlan)
#
# @brief        Handles deleting a listener, i.e. a port / VLAN ID pair, from a
#               multicast group or set of multicast groups
#
# @param[in]    group The multicast group or set of multicast groups to delete
#               the listener from
#
# @param[in]    port The port the listener is connected to
#
# @param[in]    vlan The VLAN the listener is associated with
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleDelMulticastListener
{
    my ($self, $group, $port, $vlan) = @_;

    my $chip = $self->{'CHIP'};

    my (@groupList, @vlanList);

    if (!defined($group) || !defined($port) || !defined($vlan))
    {
        print("Must specify <group> <port> <vlan>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if ($self->ValidateMcastGroup($group, \@groupList) != $FM_OK)
    {
        print($TP_MSG_ERR_MCAST_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my ($min, $max) = $self->tpPlatformGetFaceplatePortRange();

    my @facePlatePorts = ($min..$max);

    my @globalPortList = $self->validateExplicitList($TRUE,
                                                     $port,
                                                     @facePlatePorts);
    
    foreach my $switchNum ($self->tpGetSwitches)
    {
        push(@globalPortList, 
             $self->validateExplicitList($TRUE, 
                                         $port, 
                 $self->tpPlatformGetLogicalPortList($switchNum, "lag")));
    }

    if (scalar(@globalPortList) == 0)
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $status = $self->ValidateVlan($vlan, \@vlanList);
    if (scalar(@globalPortList) == 1)
    {
        if ($status != $FM_OK || scalar(@vlanList) == 0)
        {
            print($TP_MSG_ERR_VLAN_INVALID_ARRAY);
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }
    else
    {
        if ($status != $FM_OK || scalar(@vlanList) != 1)
        {
            print($TP_MSG_ERR_VLAN_INVALID_SCALAR);
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }

    my $listener = new SDK::fm_multicastListener();
    my $result = $FM_OK;

    my @affectedPortList = ();

    $chip->disableErrors() if (!DEBUG);
    foreach $group (@groupList)
    {
        foreach my $switchNum ($self->tpGetSwitches)
        {
            my @switchLogicalPortList = $self->tpPlatformGetSwitchPortList($switchNum, 
                                                                           $TRUE);

            my @lagPorts = $self->tpPlatformGetLogicalPortList($switchNum, "lag");

            push (@switchLogicalPortList, 
                  @lagPorts);

            my @portList = $self->validateExplicitList($TRUE, 
                                                       $port,
                                                       @switchLogicalPortList);
                
            if (scalar(@portList) == 0)
            {
                # None of the ports appear on this switch
                next;
            }
            
            push(@affectedPortList, @portList);

            foreach my $globalPort (@portList)
            {
                my $sw;
                my $logicalPort;

                my @isLagPort = $self->validateExplicitList($TRUE, 
                                                            $globalPort,
                                                            @lagPorts);

                if (scalar(@isLagPort == 0))
                {
                    ($sw, $logicalPort) = $self->tpPlatformMapGlobalToLogicalPort($globalPort);
                }
                else
                {
                    $logicalPort = $globalPort;
                }

                foreach my $vlan (@vlanList)
                {
                    $listener->{'port'} = $logicalPort;
                    $listener->{'vlan'} = $vlan;

                    my $status = $chip->fmDeleteMcastGroupListener($switchNum,
                                                                   $group,
                                                                   $listener);
                    if ($status != $FM_OK)
                    {
                        $result = $FM_OK;
                        printf(  "Cannot delete multicast listener %d/%d"
                               . " from multicast group %d!\n",
                               $port, $vlan, $group);
                    }
                }
            }
        }
    }
    $chip->enableErrors() if (!DEBUG);

    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $result;
}

##@cmethod public int tpHandleSetMcastGroupAddress(char *type,
#                                                  char *group,
#                                                  char *address[])
#
# @brief        Handles setting a multicast group's address
#
# @param[in]    type The address type
#
# @param[in]    group The group or range of groups to set the address for
#
# @param[in]    address Perl ARRAY containing the necessary parameters to set
#               the group address
#
# @result       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleSetMcastGroupAddress
{
    my ($self, $type, $group, @address) = @_;

    my $chip = $self->{'CHIP'};

    my (@groupList);
    my $logicalPort;

    if ($self->ValidateMcastGroup($group, \@groupList) != $FM_OK)
    {
        print($TP_MSG_ERR_MCAST_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $multicastAddress = SDK::fm_multicastAddress->new();
    TYPE: for ($type)
    {
        $_ eq 'dmac-vlan' && do
        {
            my ($macAddr, $vlanID, $vlanID2) = @address;
            if (!defined($macAddr) || !defined($vlanID))
            {
                print("Must specify <group> <mac address> <vlan>!\n");
                return $FM_ERR_INVALID_ARGUMENT;
            }

            if ($self->validateL2Address($macAddr) != $FM_OK)
            {
                print($TP_MSG_ERR_MAC_INVALID_ADDRESS);
                return $FM_ERR_INVALID_ARGUMENT;
            }

            if (!$self->_tpIsValidVLAN($vlanID))
            {
                print($TP_MSG_ERR_VLAN_INVALID_SCALAR);
                return $FM_ERR_INVALID_ARGUMENT;
            }

            if (!defined($vlanID2))
            {
                $vlanID2 = 0;
            }

            $multicastAddress->{'addressType'} = $__mcastAddrTypes{$type};
            my $mcastInfo = $multicastAddress->{'info'}->{'mac'};
            $mcastInfo->{'destMacAddress'} = $macAddr;
            $mcastInfo->{'vlan'} = $vlanID;
            $mcastInfo->{'vlan2'} = $vlanID2;

            last TYPE;
        };

        $_ eq 'dip' && do
        {
            my ($destination) = @address;
            if (!defined($destination))
            {
                print("Must specify <group> <ip address>!\n");
                return $FM_ERR_INVALID_ARGUMENT;
            }

            my Types::tp_ipAddr $ipAddr = Types::tp_ipAddr->new();
            if ($self->InetPToN($destination, $ipAddr) != $FM_OK
                || !$self->IsIPMulticast($ipAddr))
            {
                print($TP_MSG_ERR_ROUTING_INVALID_ADDR);
                return $FM_ERR_INVALID_ARGUMENT;
            }

            $multicastAddress->{'addressType'} = $__mcastAddrTypes{$type};
            my $mcastInfo = $multicastAddress->{'info'}->{'dstIpRoute'};
            $self->SetIpAddr($ipAddr, $mcastInfo->{'dstAddr'});
            $mcastInfo->{'dstPrefixLength'} = $ipAddr->{'prefix'};

            last TYPE;
        };

        $_ eq 'dip-vlan' && do
        {
            my ($destination, $vlan) = @address;
            if (!defined($destination) || !defined($vlan))
            {
                print("Must specify <group> <ip address> <vlan>!\n");
                return $FM_ERR_INVALID_ARGUMENT;
            }

            my Types::tp_ipAddr $ipAddr = Types::tp_ipAddr->new();
            if ($self->InetPToN($destination, $ipAddr) != $FM_OK
                || !$self->IsIPMulticast($ipAddr))
            {
                print($TP_MSG_ERR_ROUTING_INVALID_ADDR);
                return $FM_ERR_INVALID_ARGUMENT;
            }

            my Types::tp_vpPair $vlanID = Types::tp_vpPair->new(12);
            if ($self->VPPairPToN($vlan, $vlanID) != $FM_OK)
            {
                print($TP_MSG_ERR_MCAST_INVALID_VLAN);
                return $FM_ERR_INVALID_ARGUMENT;
            }
            if (!$self->_tpIsValidVLAN($vlanID->{'value'}))
            {
                print($TP_MSG_ERR_VLAN_INVALID_SCALAR);
                return $FM_ERR_INVALID_ARGUMENT;
            }

            $multicastAddress->{'addressType'} = $__mcastAddrTypes{$type};
            my $mcastInfo = $multicastAddress->{'info'}->{'dstIpVlanRoute'};
            $self->SetIpAddr($ipAddr, $mcastInfo->{'dstAddr'});
            $mcastInfo->{'dstPrefixLength'} = $ipAddr->{'prefix'};
            $mcastInfo->{'vlan'} = $vlanID->{'value'};
            $mcastInfo->{'vlanPrefixLength'} = $vlanID->{'prefix'};

            last TYPE;
        };

        $_ eq 'dip-sip' && do
        {
            my ($destination, $source) = @address;
            if (!defined($destination) || !defined($source))
            {
                print("Must specify <group> <destination> <source>!\n");
                return $FM_ERR_INVALID_ARGUMENT;
            }

            my Types::tp_ipAddr $dipAddr = Types::tp_ipAddr->new();
            if ($self->InetPToN($destination, $dipAddr) != $FM_OK
                || !$self->IsIPMulticast($dipAddr))
            {
                print($TP_MSG_ERR_ROUTING_INVALID_ADDR);
                return $FM_ERR_INVALID_ARGUMENT;
            }

            my Types::tp_ipAddr $sipAddr = Types::tp_ipAddr->new();
            if ($self->InetPToN($source, $sipAddr) != $FM_OK
                || $self->IsIPMulticast($sipAddr))
            {
                print($TP_MSG_ERR_ROUTING_INVALID_ADDR);
                return $FM_ERR_INVALID_ARGUMENT;
            }

            if ($dipAddr->{'af'} != $sipAddr->{'af'})
            {
                print($TP_MSG_ERR_MCAST_BAD_IP_VERSION);
                return $FM_ERR_INVALID_ARGUMENT;
            }

            $multicastAddress->{'addressType'} = $__mcastAddrTypes{$type};
            my $mcastInfo = $multicastAddress->{'info'}->{'dstSrcIpRoute'};
            $self->SetIpAddr($dipAddr, $mcastInfo->{'dstAddr'});
            $mcastInfo->{'dstPrefixLength'} = $dipAddr->{'prefix'};
            $self->SetIpAddr($sipAddr, $mcastInfo->{'srcAddr'});
            $mcastInfo->{'srcPrefixLength'} = $sipAddr->{'prefix'};

            last TYPE;
        };

        $_ eq 'dip-sip-vlan' && do
        {
            my ($destination, $source, $vlan) = @address;
            if (!defined($destination) || !defined($source) || !defined($vlan))
            {
                print("Must specify <group> <destination> <source> <vlan>!\n");
                return $FM_ERR_INVALID_ARGUMENT;
            }

            my Types::tp_ipAddr $dipAddr = Types::tp_ipAddr->new();
            if ($self->InetPToN($destination, $dipAddr) != $FM_OK
                || !$self->IsIPMulticast($dipAddr))
            {
                print($TP_MSG_ERR_ROUTING_INVALID_ADDR);
                return $FM_ERR_INVALID_ARGUMENT;
            }

            my Types::tp_ipAddr $sipAddr = Types::tp_ipAddr->new();
            if ($self->InetPToN($source, $sipAddr) != $FM_OK
                || $self->IsIPMulticast($sipAddr))
            {
                print($TP_MSG_ERR_ROUTING_INVALID_ADDR);
                return $FM_ERR_INVALID_ARGUMENT;
            }

            if ($dipAddr->{'af'} != $sipAddr->{'af'})
            {
                print($TP_MSG_ERR_MCAST_BAD_IP_VERSION);
                return $FM_ERR_INVALID_ARGUMENT;
            }

            my Types::tp_vpPair $vlanID = Types::tp_vpPair->new(12);
            if ($self->VPPairPToN($vlan, $vlanID) != $FM_OK)
            {
                print($TP_MSG_ERR_MCAST_INVALID_VLAN);
                return $FM_ERR_INVALID_ARGUMENT;
            }
            if (!$self->_tpIsValidVLAN($vlanID->{'value'}))
            {
                print($TP_MSG_ERR_VLAN_INVALID_SCALAR);
                return $FM_ERR_INVALID_ARGUMENT;
            }

            $multicastAddress->{'addressType'} = $__mcastAddrTypes{$type};
            my $mcastInfo = $multicastAddress->{'info'}->{'dstSrcIpVlanRoute'};
            $self->SetIpAddr($dipAddr, $mcastInfo->{'dstAddr'});
            $mcastInfo->{'dstPrefixLength'} = $dipAddr->{'prefix'};
            $self->SetIpAddr($sipAddr, $mcastInfo->{'srcAddr'});
            $mcastInfo->{'srcPrefixLength'} = $sipAddr->{'prefix'};
            $mcastInfo->{'vlan'} = $vlanID->{'value'};
            $mcastInfo->{'vlanPrefixLength'} = $vlanID->{'prefix'};

            last TYPE;
        };
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        GROUP: foreach my $group (@groupList)
        {
            my ($status);

            $multicastAddress->{'mcastGroup'} = $group;
            $chip->disableErrors() if (!DEBUG);
            $status = $chip->fmDeactivateMcastGroup($switchNum, $group);
            if ($status != $FM_ERR_MCAST_GROUP_NOT_ACTIVE && $status != $FM_OK)
            {
                goto NEXT;
            }
            $status = $chip->fmClearMcastGroupAddress($switchNum, $group);
            if ($status != $FM_ERR_MCAST_ADDR_NOT_ASSIGNED && $status != $FM_OK)
            {
                goto NEXT;
            }
            $status = $chip->fmSetMcastGroupAddress($switchNum,
                                                    $group,
                                                    $multicastAddress);
            if ($status != $FM_OK)
            {
                goto NEXT;
            }
            $status = $chip->fmActivateMcastGroup($switchNum, $group);
            if ($status != $FM_OK)
            {
                goto NEXT;
            }

            $status = $chip->fmGetMcastGroupPort($switchNum, $group, 
                                                 \$logicalPort);
            if ($status != $FM_OK)
            {
                goto NEXT;
            }
            else
            {
                printf("Multicast Group Activated on switch %d LogicalPort = %d \n", 
                       $switchNum, $logicalPort);
            }
            
            next GROUP;

NEXT:
            $chip->enableErrors() if (!DEBUG);
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            printf("Cannot set multicast group address for group %d!\n",
                   $group);
            next GROUP;
        }
    }
}

##@cmethod public int tpHandleShowMcastGroup(char *group)
#
# @brief        Handles showing a multicast group or range of multicast groups
#
# @param[in]    group The group or range of groups to be shown
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
sub tpHandleShowMcastGroup
{
    my ($self, $group) = @_;

    my $chip = $self->{'CHIP'};

    my (@groupList);
    my $logicalPort;

    if ($self->ValidateMcastGroup($group, \@groupList) != $FM_OK)
    {
        print($TP_MSG_ERR_MCAST_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }



    my $listener = new SDK::fm_multicastListener();
    my $nextListener = new SDK::fm_multicastListener();
    my $numListener = 0;
    my $multicastAddress = SDK::fm_multicastAddress->new();
    my %types = map {$__mcastAddrTypes{$_} => $_} keys(%__mcastAddrTypes);
    $chip->disableErrors() if (!DEBUG);
    foreach my $switchNum ($self->tpGetSwitches)
    { 
        printf("Switch %4d: \n", $switchNum);

        print("\n");
        printf("%-6s %-12s %-12s %-18s %-18s %-7s %s\n",
               'Group#', 'LPRT', 'Type', 'Group Address', 'Host Address', 'VLAN',
               'Port/VLAN');
        printf("%s\n", '-' x 90);

        foreach my $group (@groupList)
        {
            my ($status);

            printf("  %4d ", $group);

            $status = $chip->fmGetMcastGroupPort($switchNum, $group, 
                                                 \$logicalPort);
            if ($status == $FM_OK)
            {
                printf("%-12s ", $logicalPort);
            }
            else
            {
                printf("%-12s ", 'unknown');
            }

            $status = $chip->fmGetMcastGroupAddress($switchNum, $group,
                                                    $multicastAddress);
            if ($status == $FM_OK
                && exists($types{$multicastAddress->{'addressType'}}))
            {
                printf("%-12s ", $types{$multicastAddress->{'addressType'}});
                TYPE: for ($types{$multicastAddress->{'addressType'}})
                {
                    my ($destination, $source, $vlan);

                    my $info = $multicastAddress->{'info'};
                    my Types::tp_ipAddr $ipAddr = Types::tp_ipAddr->new();
                    my Types::tp_vpPair $vlanID = Types::tp_vpPair->new(12);

                    $_ eq 'dmac-vlan' && do
                    {
                        my $mcastInfo = $info->{'mac'};
                        printf("%-17s %s %-7s ",
                               $mcastInfo->{'destMacAddress'},
                               ' ' x 19, $mcastInfo->{'vlan'});

                        last TYPE;
                    };

                    $_ eq 'dip' && do
                    {
                        my $mcastInfo = $info->{'dstIpRoute'};
                        $self->GetIpAddr($mcastInfo->{'dstAddr'}, $ipAddr);
                        $ipAddr->{'prefix'} = $mcastInfo->{'dstPrefixLength'};
                        $self->InetNToP($ipAddr, \$destination);
                        printf("%-18s %s ", $destination, ' ' x 26);

                        last TYPE;
                    };

                    $_ eq 'dip-vlan' && do
                    {
                        my $mcastInfo = $info->{'dstIpVlanRoute'};
                        $self->GetIpAddr($mcastInfo->{'dstAddr'}, $ipAddr);
                        $ipAddr->{'prefix'} = $mcastInfo->{'dstPrefixLength'};
                        $self->InetNToP($ipAddr, \$destination);
                        $vlanID->{'value'} = $mcastInfo->{'vlan'};
                        $vlanID->{'prefix'} = $mcastInfo->{'vlanPrefixLength'};
                        $self->VPPairNToP($vlanID, \$vlan);
                        printf("%-18s %s %-7s ", $destination, ' ' x 18, $vlan);

                        last TYPE;
                    };

                    $_ eq 'dip-sip' && do
                    {
                        my $mcastInfo = $info->{'dstSrcIpRoute'};
                        $self->GetIpAddr($mcastInfo->{'dstAddr'}, $ipAddr);
                        $ipAddr->{'prefix'} = $mcastInfo->{'dstPrefixLength'};
                        $self->InetNToP($ipAddr, \$destination);
                        $self->GetIpAddr($mcastInfo->{'srcAddr'}, $ipAddr);
                        $ipAddr->{'prefix'} = $mcastInfo->{'srcPrefixLength'};
                        $self->InetNToP($ipAddr, \$source);
                        printf("%-18s %-18s %s ", $destination, $source, ' ' x 7);

                        last TYPE;
                    };

                    $_ eq 'dip-sip-vlan' && do
                    {
                        my $mcastInfo = $info->{'dstSrcIpVlanRoute'};
                        $self->GetIpAddr($mcastInfo->{'dstAddr'}, $ipAddr);
                        $ipAddr->{'prefix'} = $mcastInfo->{'dstPrefixLength'};
                        $self->InetNToP($ipAddr, \$destination);
                        $self->GetIpAddr($mcastInfo->{'srcAddr'}, $ipAddr);
                        $ipAddr->{'prefix'} = $mcastInfo->{'srcPrefixLength'};
                        $self->InetNToP($ipAddr, \$source);
                        $vlanID->{'value'} = $mcastInfo->{'vlan'};
                        $vlanID->{'prefix'} = $mcastInfo->{'vlanPrefixLength'};
                        $self->VPPairNToP($vlanID, \$vlan);
                        printf("%-18s %-18s %-7s ", $destination, $source, $vlan);

                        last TYPE;
                    };
                }
            }
            else
            {
                printf("%-12s %s ", 'unknown', ' ' x 45);
            }

            $numListener = 0;

            $status = $chip->fmGetMcastGroupListenerFirst($switchNum, $group,
                                                          $listener); 
            my (%ports, %vlanIDs) = ((), ());

            while ($status != $FM_ERR_NO_MORE)
            {
                # The necessary Perl HASHes are created on the fly.
                $ports{$listener->{'port'}}->{$listener->{'vlan'}} = $TRUE;
                $vlanIDs{$listener->{'vlan'}}->{$listener->{'port'}} = $TRUE;
                $numListener++;

                $status = $chip->fmGetMcastGroupListenerNext($switchNum, $group,
                                                             $listener,
                                                             $nextListener); 
                $listener = $nextListener;
            }

            if ($numListener == 0)
            {
                print("No members");
            }
            else
            {
                foreach my $vlan (sort({$a <=> $b} keys(%vlanIDs)))
                {
                    my @ports = sort({$a <=> $b} keys(%{$vlanIDs{$vlan}}));
                    if (scalar(@ports) > 1)
                    {
                        foreach my $port (@ports)
                        {
                            if (scalar(keys(%{$ports{$port}})) > 1)
                            {
                                delete($vlanIDs{$vlan}->{$port});
                            }
                            else
                            {
                                delete($ports{$port}->{$vlan});
                            }
                        }
                        @ports = sort({$a <=> $b} keys(%{$vlanIDs{$vlan}}));
                        if (scalar(@ports) > 0)
                        {
                            printf("%s/%d ", $self->StringifyList(@ports), $vlan);
                        }
                    }
                }
                foreach my $port (sort({$a <=> $b} keys(%ports)))
                {
                    my @vlanIDs = sort({$a <=> $b} keys(%{$ports{$port}}));
                    if (scalar(@vlanIDs) > 0)
                    {
                        printf("%d/%s ", $port, $self->StringifyList(@vlanIDs));
                    }
                }
            }

            print("\n");
        }
    }
    $chip->enableErrors() if (!DEBUG);

    print("\n");

    return $FM_OK;
}



sub tpHandleSetMcastGroupAttribute
{
    my ($self, $group, $type, $value) = @_;

    my $chip = $self->{'CHIP'};

    my (@groupList);

    if ($self->ValidateMcastGroup($group, \@groupList) != $FM_OK)
    {
        print($TP_MSG_ERR_MCAST_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my %void;

    if ( ($type eq 'bypassEgressSTP') ||
         ($type eq 'L2SwitchOnly') ||
         ($type eq 'L3SwitchOnly') ||
         ($type eq 'L3Flood-set') )
    {
        $void{type} = 'fm_bool';
        if ($value eq "on")
        {
            $void{value} = $FM_ENABLED;
        }
        else
        {
            $void{value} = $FM_DISABLED;
        }
    }
    elsif ($type eq 'repli-group')
    {
        $void{type} = 'fm_int';

        if ($value eq 'private')
        {
            $void{value} = $FM_MCASTGROUP_REPLICATION_GROUP_DISABLE;
        }
        else
        {
            $void{value} = $value;
        }
    }
    else
    {
        print("Must specify a valid multicast attribute!\n");
        return $FM_FAIL;
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my ($status);
        GROUP: foreach my $group (@groupList)
        {
            $status = $chip->fmSetMcastGroupAttribute($switchNum,
                                                      $group,
                                                      $__mcastAttributes{$type},
                                                      \%void);
            if ($status != $FM_OK)
            {
                goto NEXT;
            }
            
            next GROUP;

NEXT:
            $chip->enableErrors() if (!DEBUG);
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            printf("Cannot set multicast group attribute for group %d!\n",
                   $group);
            next GROUP;
        }
    }
}

sub tpHandleSetMcastGroupState
{
    my ($self, $group, $state) = @_;

    my $chip = $self->{'CHIP'};

    my (@groupList);

    if ($self->ValidateMcastGroup($group, \@groupList) != $FM_OK)
    {
        print($TP_MSG_ERR_MCAST_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my ($status);
        GROUP: foreach my $group (@groupList)
        {
            if ($state eq 'active' )
            {
                $status = $chip->fmActivateMcastGroup($switchNum, $group);
            }
            else
            {
                $status = $chip->fmDeactivateMcastGroup($switchNum, $group);
            }

            if ($status != $FM_OK)
            {
                goto NEXT;
            }

            next GROUP;

NEXT:
            $chip->enableErrors() if (!DEBUG);
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            printf("Cannot set state of this multicast group %d!\n",
                   $group);
            next GROUP;
        }
    }
}


##@cmethod public int tpHandleCreateRepliGroup(void)
#
# @brief        Handles creation of a replicate group
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub tpHandleCreateRepliGroup
{
    my ($self) = @_;

    my $chip = $self->{'CHIP'};

    my $groups = [];
    my $previousGroup = undef;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    $chip->disableErrors() if (!DEBUG);
    foreach my $sw ($self->tpGetSwitches)
    {
        my ($status);

        $status = $chip->fmCreateReplicationGroup($sw, \$groups->[$sw]);
        if ($status != $FM_OK
            || (defined($previousGroup) && $groups->[$sw] != $previousGroup))
        {
            goto ABORT;
        }
        $previousGroup = $groups->[$sw];
    }
    $chip->enableErrors() if (!DEBUG);

    printf("Created replicate group %d\n", $previousGroup);
    return $FM_OK;

ABORT:
    print("Cannot create replicate group!\n");
    foreach my $sw ($self->tpGetSwitches)
    {
        if (defined($groups->[$sw]))
        {
            $chip->fmDeleteReplicationGroup($sw, $groups->[$sw]);
        }
    }
    $chip->enableErrors() if (!DEBUG);
    return $FM_FAIL;
}

##@cmethod public int tpHandleDelRepliGroup(char *group)
#
# @brief        Handles deleting a replicate group
#
# @param[in]    group The replicate group to be deleted
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleDelRepliGroup
{
    my ($self, $group) = @_;

    my $chip = $self->{'CHIP'};
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    $chip->disableErrors() if (!DEBUG);
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my ($status);
        $status = $chip->fmDeleteReplicationGroup($switchNum, $group);
        if ($status != $FM_OK)
        {
            goto ABORT;
        }
    }
    $chip->enableErrors() if (!DEBUG);
    return $FM_OK;

ABORT:
    printf("Cannot delete replicate group %d!\n", $group);
    $chip->enableErrors() if (!DEBUG);
    return $FM_FAIL;
}

##@cmethod public int tpHandleAddMulticastEcmp(char *group,
#                                              int   vlan)
#
# @brief        Handles adding a ECMP to a multicast group
#
# @param[in]    group The multicast group
#
# @param[in]    vlan The egress VLAN the add to the next-hop entry
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleAddMulticastEcmp
{
    my ($self, $group, $vlan) = @_;

    my $chip = $self->{'CHIP'};

    my (@groupList, @vlanList);

    my $result = $FM_OK;

    if (!defined($group) || !defined($vlan))
    {
        print("Must specify <group> <vlan>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if ($self->ValidateMcastGroup($group, \@groupList) != $FM_OK)
    {
        print($TP_MSG_ERR_MCAST_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});

    $chip->disableErrors() if (!DEBUG);
    foreach $group (@groupList)
    {
        foreach my $switchNum ($self->tpGetSwitches)
        {
           my $ecmpHandle;

           my $status = $chip->fmAddMcastGroupEcmp($switchNum,
                                                   $group,
                                                   $vlan,
                                                   \$ecmpHandle);
           if ($status != $FM_OK)
           {
              $result = $FM_OK;
              printf("Switch %d: ", $switchNum) if ($switchCount > 1);
              printf(  "Cannot add multicast ecmp "
                       . "to multicast group %d vlan=%d!\n",
                       $group, $vlan);
           }
           else
           {
              printf("ECMP group %d added to multicast group %d\n", $ecmpHandle, $group);
           }
        }
    }
    $chip->enableErrors() if (!DEBUG);

    return $result;
}

##@cmethod public int tpHandleDeleteMulticastEcmp(char *group,
#                                                 int   vlan)
#
# @brief        Handles deleting a ECMP from a multicast group
#
# @param[in]    group The multicast group
#
# @param[in]    ecmp The ECMP ID to delete
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleDeleteMulticastEcmp
{
    my ($self, $group, $ecmp) = @_;

    my $chip = $self->{'CHIP'};

    my $result = $FM_OK;

    my @groupList;

    if (!defined($group) || !defined($ecmp))
    {
        print("Must specify <group> <ecmp>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if ($self->ValidateMcastGroup($group, \@groupList) != $FM_OK)
    {
        print($TP_MSG_ERR_MCAST_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});

    $chip->disableErrors() if (!DEBUG);
    foreach $group (@groupList)
    {
        foreach my $switchNum ($self->tpGetSwitches)
        {
           my $ecmpHandle;

           my $status = $chip->fmDeleteMcastGroupEcmp($switchNum,
                                                      $group,
                                                      $ecmp);
           if ($status != $FM_OK)
           {
              $result = $FM_OK;
              printf("Switch %d: ", $switchNum) if ($switchCount > 1);
              printf(  "Cannot delete multicast ecmp "
                       . "from multicast group %d ecmp=%d!\n",
                       $group, $ecmp);
           }
           else
           {
              printf("ECMP group %d deleted from multicast group %d\n", $ecmp, $group);
           }
        }
    }
    $chip->enableErrors() if (!DEBUG);

    return $result;
}
1;
