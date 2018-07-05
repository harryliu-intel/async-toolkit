# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/VNCore.pm
# Creation Date:    11/03/2012
# Description:
#
# INTEL CONFIDENTIAL
# Copyright 2012 Intel Corporation. All Rights Reserved.
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

package Applications::TestPoint::Common::VNCore;
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


##@var <char*,int> __vnTunnelAttributes
#
# @brief        Perl HASH containing all recognized VN Tunnel attributes
our %__vnTunnelAttributes =
(
    'local-ip'           => $FM_VNTUNNEL_ATTR_LOCAL_IP,
    'remote-ip'          => $FM_VNTUNNEL_ATTR_REMOTE_IP,
    'traffic-identifier' => $FM_VNTUNNEL_ATTR_TRAFFIC_IDENTIFIER,
    'vrid'               => $FM_VNTUNNEL_ATTR_VRID,
    'mcast-group'        => $FM_VNTUNNEL_ATTR_MCAST_GROUP,
    'mcast-dmac'         => $FM_VNTUNNEL_ATTR_MCAST_DMAC,
);




###############################################################################
#
#                               LOCAL FUNCTIONS
#
###############################################################################


##@cmethod private int GetVNTunnelList(int **VNTunnelList)
#
# @brief        Retrieves all virtual network tunnels
#
# @param[in]    VNTunnelList Points to a Perl ARRAY
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub GetVNTunnelList
{
    my ($self, $VNTunnelList) = @_;

    my $chip = $self->{'CHIP'};

    # Initialize the list of tunnels to the empty list.
    @{$VNTunnelList} = ();

    my ($currentTunnel, $searchToken, $status);

    my $switchNum = shift(@{[$self->tpGetSwitches]});
    $chip->disableErrors();
    $status = $chip->fmGetVNTunnelFirst($switchNum,
                                        \$searchToken,
                                        \$currentTunnel);
    if ($status == $FM_ERR_NO_MORE)
    {
        $chip->enableErrors();
        return $FM_OK;
    }

    my $tunnel = 0;
    while ($status == $FM_OK)
    {
        $VNTunnelList->[$tunnel++] = $currentTunnel;

        $status = $chip->fmGetVNTunnelNext($switchNum,
                                           \$searchToken,
                                           \$currentTunnel);
    }

    $chip->enableErrors();

    return ($status == $FM_OK || $status == $FM_ERR_NO_MORE) ? $FM_OK : $FM_FAIL;

}   # end GetVNTunnelList




##@cmethod private int ValidateVNTunnelId(char *tunnel, int **tunnelList)
#
# @brief        Validates a VN tunnel or set of VN tunnels
#
# @param[in]    tunnel      The tunnel or set of tunnels to be validated
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub ValidateVNTunnelId
{
    my ($self, $tunnel, $tunnelList) = @_;

    my (@VNTunnelList);

    my $status = $self->GetVNTunnelList(\@VNTunnelList);
    if ($status != $FM_OK)
    {
        return $FM_FAIL;
    }
    @{$tunnelList} = $self->validateList($tunnel,
                                         min(@VNTunnelList),
                                         max(@VNTunnelList));
    if (scalar(@{$tunnelList}) == 0)
    {
        return $FM_FAIL;
    }
    my %VNTunnelList = map {$_ => $TRUE} @VNTunnelList;
    foreach my $tunnel (@{$tunnelList})
    {
        if (!exists($VNTunnelList{$tunnel}))
        {
            return $FM_FAIL;
        }
    }
    return $FM_OK;
}




###############################################################################
#
#                               PUBLIC FUNCTIONS
#
###############################################################################


##@cmethod public int tpHandleCreateVN(const char *vsid, const char *internalId)
#
# @desc         Creates a Virtual Network
#
# @param[in]    vsid contains the virtual subscriber ID.
#
# @param[in]    internalId contains the internal ID number for the VN.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
#
sub tpHandleCreateVN
{
    my ($self, $vsid, $internalId) = @_;
    my $chip = $self->{CHIP};

    my $result = $FM_OK;

    my $descriptor = SDK::fm_vnDescriptor->new();
    $descriptor->{'internalId'} = $internalId;

    my $switchCount = scalar(@{[$self->tpGetSwitches]});

    foreach my $switchNum ($self->tpGetSwitches)
    {
        $chip->disableErrors();
        my $status = $chip->fmCreateVN($switchNum, $vsid, $descriptor);
        $chip->enableErrors();
        if ($status != $FM_OK)
        {
            $result = $FM_FAIL;
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            printf("Virtual Network %s cannot be created (errno=%d)\n", $vsid, $status);
        }
    }

    return $result;
}




##@cmethod public int tpHandleDeleteVN(const char *vsid)
#
# @desc         Deletes a Virtual Network
#
# @param[in]    vsid contains the virtual subscriber ID.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
#
sub tpHandleDeleteVN
{
    my ($self, $vsid) = @_;
    my $chip = $self->{CHIP};

    my $result = $FM_OK;

    my $switchCount = scalar(@{[$self->tpGetSwitches]});

    foreach my $switchNum ($self->tpGetSwitches)
    {
        $chip->disableErrors();
        my $status = $chip->fmDeleteVN($switchNum, $vsid);
        $chip->enableErrors();
        if ($status != $FM_OK)
        {
            $result = $FM_FAIL;
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            printf("Virtual Network %s cannot be deleted (errno=%d)\n", $vsid, $status);
        }
    }

    return $result;
}




##@cmethod public int tpHandleSetVNInternalID(const char *vsid, const char *internalId)
#
# @desc         Changes the Internal ID for a Virtual Network
#
# @param[in]    vsid contains the virtual subscriber ID.
#
# @param[in]    internalId contains the new internal ID number for the VN.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
#
sub tpHandleSetVNInternalID
{
    my ($self, $vsid, $internalId) = @_;
    my $chip = $self->{CHIP};

    my $result = $FM_OK;

    my $descriptor = SDK::fm_vnDescriptor->new();
    $descriptor->{'internalId'} = $internalId;

    my $switchCount = scalar(@{[$self->tpGetSwitches]});

    foreach my $switchNum ($self->tpGetSwitches)
    {
        $chip->disableErrors();
        my $status = $chip->fmUpdateVN($switchNum, $vsid, $descriptor);
        $chip->enableErrors();
        if ($status != $FM_OK)
        {
            $result = $FM_FAIL;
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            printf("Virtual Network %s internal ID cannot be set (errno=%d)\n", $vsid, $status);
        }
    }

    return $result;
}




##@cmethod public int tpHandleCreateVNTunnel(const char *tunnelType)
#
# @desc         Creates a Virtual Network Tunnel
#
# @param[in]    tunnelType contains the VN Tunnel Type.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
#
sub tpHandleCreateVNTunnel
{
    my ($self, $tunnelType) = @_;
    my $chip = $self->{CHIP};

    my $result = $FM_OK;
    my $apiTunnelType;
    my $tunnelID;

    if ($tunnelType eq "vxlan")
    {
        $apiTunnelType = $FM_VN_TUNNEL_TYPE_VXLAN;
    }
    elsif ($tunnelType eq "nvgre")
    {
        $apiTunnelType = $FM_VN_TUNNEL_TYPE_NVGRE;
    }
    else
    {
        print("Invalid Tunnel Type: $tunnelType. Use vxlan or nvgre instead\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});

    foreach my $sw ($self->tpGetSwitches)
    {
        my $tunnelId;
        $chip->disableErrors();
        my $status = $chip->fmCreateVNTunnel($sw, $apiTunnelType, \$tunnelId);
        $chip->enableErrors();
        if ($status != $FM_OK)
        {
            $result = $FM_FAIL;
            printf("Switch %d: ", $sw) if ($switchCount > 1);
            printf("Virtual Network Tunnel cannot be created (errno=%d)\n", $status);
        }
        else
        {
            printf("Switch $sw: Virtual Network Tunnel %d created\n", $tunnelId);
        }
    }

    return $result;
}




##@cmethod public int tpHandleDeleteVNTunnel(const char *vsid)
#
# @desc         Deletes a Virtual Network Tunnel
#
# @param[in]    tunnelId contains the VN Tunnel Identifier.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
#
sub tpHandleDeleteVNTunnel
{
    my ($self, $tunnelId) = @_;
    my $chip = $self->{CHIP};

    my $result = $FM_OK;

    my $switchCount = scalar(@{[$self->tpGetSwitches]});

    foreach my $switchNum ($self->tpGetSwitches)
    {
        $chip->disableErrors();
        my $status = $chip->fmDeleteVNTunnel($switchNum, $tunnelId);
        $chip->enableErrors();
        if ($status != $FM_OK)
        {
            $result = $FM_FAIL;
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            printf("Virtual Network Tunnel %s cannot be deleted (errno=%d)\n", $tunnelId, $status);
        }
    }

    return $result;
}




##@cmethod public int tpHandleSetVNTunnelAttribute(const char *tunnelId, const char *attr, const char *attrVal)
#
# @desc         Sets an attribute for one or more virtual network tunnels
#
# @param[in]    tunnelId contains the VN Tunnel Identifier(s).
#
# @param[in]    attr contains the attribute name to be set.
#
# @param[in]    attrVal contains the new value for the attribute.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
#
sub tpHandleSetVNTunnelAttribute
{
    my ($self, $tunnelId, $attr, $attrVal) = @_;

    my $chip = $self->{'CHIP'};

    if (DEBUG)
    {
        printf("tpHandleSetVNTunnelAttribute: tunnelId=$tunnelId, "
               . "attr=$attr, attrVal=$attrVal\n");
    }

    my (@tunnelList);

    if ($self->ValidateVNTunnelId($tunnelId, \@tunnelList) != $FM_OK)
    {
        print($TP_MSG_ERR_VN_TUNNEL_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my %void;

    if ( ($attr eq 'local-ip') || ($attr eq 'remote-ip') )
    {
        my $ipAddr = Types::tp_ipAddr->new();
        my $status = $self->InetPToN($attrVal, $ipAddr);
        if ($status != $FM_OK)
        {
            print "'$attrVal' is not a legal IP address\n";
            return $status;
        }

        my $value = SDK::fm_ipAddr->new();
        $status = $self->SetIpAddr($ipAddr, $value);
        if ($status != $FM_OK)
        {
            print "Trouble converting IP address\n";
            return $status;
        }
        $void{type}  = 'fm_ipAddr';
        $void{value} = $value;
    }
    elsif ( ($attr eq 'vrid') || ($attr eq 'mcast-group') || ($attr eq 'traffic-identifier'))
    {
        $void{type}  = 'fm_int';
        $void{value} = $attrVal;
    }
    elsif ($attr eq 'mcast-dmac')
    {
        $void{type}  = 'fm_macaddr';
        $void{value} = $attrVal;
    }
    else
    {
        print("Must specify a valid VN Tunnel attribute!\n");
        return $FM_FAIL;
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $sw ($self->tpGetSwitches)
    {
        my ($status);
        TUNNEL: foreach my $tunnel (@tunnelList)
        {
            $status = $chip->fmSetVNTunnelAttribute($sw,
                                                    $tunnel,
                                                    $__vnTunnelAttributes{$attr},
                                                    \%void);
            if ($status == $FM_OK)
            {
                next TUNNEL;
            }

            $chip->enableErrors() if (!DEBUG);
            printf("Switch %d: ", $sw) if ($switchCount > 1);
            my $errmsg = $chip->fmErrorMsg($status);
            printf("Cannot set VN Tunnel attribute for tunnel $tunnel: $errmsg\n");
            next TUNNEL;
        }
    }
}

1;
