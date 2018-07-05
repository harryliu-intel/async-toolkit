# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/QoSCore.pm
# Creation Date:    12/06/09
# Description:
#
# INTEL CONFIDENTIAL
# Copyright 2007 - 2012 Intel Corporation. All Rights Reserved.
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

package Applications::TestPoint::Common::QoSCore;
use strict;
use warnings;


use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Base::Const;


our @EXPORT = qw(
    tpHandleShowQos
    tpHandleSetQos
);



sub buildFinalPortList
{
    my ($self, $sw, $port, $attribute, $perLagMgmt) = @_;

    my $chip = $self->{CHIP};

    my @list = $self->tpPlatformGetLogicalPortList($sw, "local,lag");
    my @portList = $self->validateExplicitList($FALSE, $port, @list);
    my @finalPortList = ();

    if (scalar(@portList) == 0)
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return @finalPortList;
    }

    if ($port eq "all" && $perLagMgmt == $TRUE)
    {
        # In per-LAG management mode per-LAG attributes can be set on
        # LAG logical port only and per-port attributes can be set on
        # member port only. For port that doesn't belong to a LAG then any
        # attribute can be set on the port. Also internal ports and LAGs
        # are not modified when "all" is defined.
        #
        # So build a list of ports that satisfy the above restrictions

        $chip->disableErrors();

        my $perLagAttribute = FM_FALSE;
        my $switchFamily = $self->getSwitchFamily($sw);

        if ($switchFamily == $FM_SWITCH_FAMILY_FM2000)
        {
            $perLagAttribute =
                $self->tpFM2000IsPerLagPortAttribute($sw, $attribute);
        }
        elsif ( ($switchFamily == $FM_SWITCH_FAMILY_FM4000) ||
                ($switchFamily == $FM_SWITCH_FAMILY_REMOTE_FM4000) ||
                ($switchFamily == $FM_SWITCH_FAMILY_SWAG) )
        {
            $perLagAttribute =
                $self->tpFM4000IsPerLagPortAttribute($sw, $attribute);
        }
        elsif ( ($switchFamily == $FM_SWITCH_FAMILY_FM6000) ||
                ($switchFamily == $FM_SWITCH_FAMILY_REMOTE_FM6000) )
        {
            $perLagAttribute =
                $self->tpFM6000IsPerLagPortAttribute($sw, $attribute);
        }

        my ($minPort, $maxPort) = $self->tpPlatformGetPortRange();
        foreach my $port (@portList)
        {
            # Internal ports or LAGs are not modified
            if (not $chip->fmIsInternalPort($sw, $port))
            {
                if ($port <= $maxPort)
                {
                    # Add port to port list if the port is not member of a
                    # LAG or the attribute is not a per-LAG attribute.
                    if ((not $chip->fmPortIsInALAG($sw, $port)) ||
                        (not $perLagAttribute))
                    {
                        push(@finalPortList, $port);
                    }
                }
                else
                {
                    # This is a LAG logical port, add it to the port list
                    # only if it is a per-LAG port attribute
                    if ($perLagAttribute)
                    {
                        push(@finalPortList, $port);
                    }
                }
            }
        }

        $chip->enableErrors();
    }
    else
    {
        @finalPortList = @portList;
    }

    return @finalPortList;
}



##@cmethod public void tpHandleShowQos(char *port)
#
# @desc         Handles showing the QoS configuration for a set of ports
#               Can be a LAG logical port.
#
# @param[in]    port The set of ports whose QoS configuration is to be shown
# 
# @return       None.
# 
sub tpHandleShowQos
{
    my ($self, $port) = @_;

    my $chip = $self->{CHIP};

    # Get switch information from SDK
    my $info = new SDK::fm_switchInfo();

    my @affectedPortList = ();
    my @globalPortList =
        $self->validateList($port, $self->tpPlatformGetPortRange());

    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my @list = $self->tpPlatformGetLogicalPortList($sw, "local,lag");
        my @portList = $self->validateExplicitList($FALSE, $port, @list);

        if (scalar(@portList) == 0)
        {
            next;
        }

        push(@affectedPortList, @portList);

        $chip->fmGetSwitchInfo($sw, $info);

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000)
        {
            $self->tpFM2000HandleShowQos($sw, \@portList);
        }
        elsif ( ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000) ||
                ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_REMOTE_FM4000) ||
                ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG) )
        {
            $self->tpFM4000HandleShowQos($sw, \@portList);
        }
        elsif ( ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000) ||
                ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_REMOTE_FM6000) )
        {
            $self->tpFM6000HandleShowQos($sw, \@portList);
        }
    }

    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
}



##@cmethod public void tpHandleSetQos(char *port)
#
# @desc         Handles showing the QoS port configuration for a set of ports.
#
# @param[in]    attribute: The QoS port attribute to be modified.
# 
# @param[in]    port: The set of ports whose QoS port configuration is to
#                     be modified.
# 
# @param[in]    index: The index of the Qos port attribute whose
#                      configuration is to be modified.
# 
# @param[in]    watermark: The value to set the QoS port attribute at.
# 
# @return       None.
# 
sub tpHandleSetQos
{
    my ($self, $attribute, $port, $index, $watermark) = @_;

    my $chip = $self->{CHIP};


    if ( !defined($attribute) )
    {
        print("Invalid QoS port attribute: $attribute!\n");
        return;
    }

    if (!defined($port))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    $chip->disableErrors();
    my $perLagMgmt =
            $chip->fmGetBoolApiAttribute($SDK::FM_AAK_API_PER_LAG_MANAGEMENT,
                                         $SDK::FM_AAD_API_PER_LAG_MANAGEMENT);
    $chip->enableErrors();

    foreach my $sw ($self->tpGetSwitches)
    {
        my @finalPortList = $self->buildFinalPortList($sw, $port,
                                                      $attribute,
                                                      $perLagMgmt);

        if (scalar(@finalPortList) == 0)
        {
            print($TP_MSG_ERR_PORT_INVALID_ARRAY);
            return;
        }

        my $status = $FM_FAIL;
        my $switchFamily = $self->getSwitchFamily($sw);

        if ($switchFamily == $FM_SWITCH_FAMILY_FM2000)
        {
            $status = $self->tpFM2000HandleSetQos($sw, $attribute,
                                                  \@finalPortList,
                                                  $index, $watermark);
        }
        elsif ( ($switchFamily == $FM_SWITCH_FAMILY_FM4000) ||
                ($switchFamily == $FM_SWITCH_FAMILY_REMOTE_FM4000) ||
                ($switchFamily == $FM_SWITCH_FAMILY_SWAG) )
        {
            $status = $self->tpFM4000HandleSetQos($sw, $attribute,
                                                  \@finalPortList,
                                                  $index, $watermark);
        }
        elsif ( ($switchFamily == $FM_SWITCH_FAMILY_FM6000) ||
                ($switchFamily == $FM_SWITCH_FAMILY_REMOTE_FM6000) )
        {
            $status = $self->tpFM6000HandleSetQos($sw, $attribute,
                                                  \@finalPortList,
                                                  $index, $watermark);
        }

        if ($status != $FM_OK)
        {
            return;
        }
    }
}

