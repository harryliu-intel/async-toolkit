# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/StreamCore.pm
# Creation Date:    August 8, 2008
# Description:
#
# INTEL CONFIDENTIAL
# Copyright 2008 - 2012 Intel Corporation. All Rights Reserved.
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

package Applications::TestPoint::Common::StreamCore;
use strict;
use warnings;

use List::Util qw(sum);
use Math::BigInt;

use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;
use SDKScalars;

###############################################################################
#
#                               LOCAL VARIABLES
#
###############################################################################

our @tpStreamModes = qw(loop-start loop-stop release clear);
our @tpPacketTypes = qw(bytes file-pcap);

###############################################################################
#
#                               LOCAL FUNCTIONS
#
###############################################################################

sub _blockTraffic
{
    validatePrototype(@_, 3);
    my ($self, $info, $port) = @_;

    # Disable TX PCS
    if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
    {
        # Switch the TX drain mode to HOLD_HOLD.
        $self->handleRegSetBitPort($port, "FM_MAC_CFG", 30, 0);
        $self->handleRegSetBitPort($port, "FM_MAC_CFG", 29, 0);
    }
    else    # FM2000 or FM4000
    {
        $self->handleRegSetBitPort($port, "FM_PCS_CFG_1", 26);
    }
}

sub _releaseTraffic
{
    validatePrototype(@_, 3);
    my ($self, $info, $port) = @_;

    # Enable TX PCS
    if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
    {
        # Switch the TX drain mode to HOLD_NORMAL.
        $self->handleRegSetBitPort($port, "FM_MAC_CFG", 30, 0);
        $self->handleRegClearBitPort($port, "FM_MAC_CFG", 29, 0);
    }
    else    # FM2000 or FM4000
    {
        $self->handleRegClearBitPort($port, "FM_PCS_CFG_1", 26);
    }
}

sub _enableDrainTraffic
{
    validatePrototype(@_, 3);
    my ($self, $info, $port) = @_;

    # Disable RX PCS
    if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
    {
        $self->handleRegSetBitPort($port, "FM_MAC_CFG", 55, 0);
    }
    else    # FM2000 or FM4000
    {
        $self->handleRegSetBitPort($port, "FM_PCS_CFG_1", 27);
    }
}

sub _disableDrainTraffic
{
    validatePrototype(@_, 3);
    my ($self, $info, $port) = @_;

    # Enable RX PCS
    if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
    {
        $self->handleRegClearBitPort($port, "FM_MAC_CFG", 55, 0);
    }
    else    # FM2000 or FM4000
    {
        $self->handleRegClearBitPort($port, "FM_PCS_CFG_1", 27);
    }
}

sub _clearStream
{
    validatePrototype(@_, 3);
    my ($self, $info, $port) = @_;

    $self->_enableDrainTraffic($info, $port);
    $self->_releaseTraffic($info, $port);
    $self->_disableDrainTraffic($info, $port);
    $self->_blockTraffic($info, $port);
}

# Modifies the port mask adding or removing the specified stream port.
sub _modifyLoop
{
    validatePrototype(@_, 5);
    my ($self, $switchNum, $info, $port, $state) = @_;

    my $chip = $self->{CHIP};

    my $bitmask = SDK::fm_bitArray->new();
    $chip->fmCreateBitArray($bitmask, $info->{'numPorts'});
    my %void = (type => "fm_bitArray", value => $bitmask);
    my $status = $chip->fmGetPortAttribute($switchNum,
                                           $port,
                                           $FM_PORT_MASK_WIDE,
                                           \%void);
    $chip->fmSetBitArrayBit($bitmask, $port, $state);
    my $value = $self->stringifyPortMaskWide($switchNum, $bitmask);
    $chip->fmDeleteBitArray($bitmask);

    $self->tpSetStreamPort($port, $value);
}

###############################################################################
#
#                               PUBLIC FUNCTIONS
#
###############################################################################


##@cmethod public int tpCreateStream(int streamId)
#
# @desc         Creates a packet stream. Uses a loopback port for packet
#               storage.
#
# @param[in]    streamId The loopback port in which to store the packets.
#
# @note         The current implementation requires that the streamId
#               represent a port which is to be used as a packet store
#
sub tpCreateStream
{
    my ($self, $streamId) = @_;

    my $switchNum = 0;
    my $info = ($self->tpGetSwitchInfo())[$switchNum];
    my @portList = $self->tpPlatformGetLogicalPortList($switchNum, "local");

    my @streamPorts = $self->validateExplicitList($FALSE, $streamId, @portList);
    if (!defined($streamId) || scalar(@streamPorts) == 0)
    {
        print($TP_MSG_ERR_STREAM_INVALID_ID);
        return;
    }

    foreach my $streamPort (@streamPorts)
    {
        $self->handleSetPortConfig($streamPort, "learning", "off");
        $self->_blockTraffic($info, $streamPort);
        $self->handleSetPortConfig($streamPort, "loopback", "on");
        # Disable loopback suppression on Bali and post-Bali based switches.
        if ($info->{'switchFamily'} != $FM_SWITCH_FAMILY_FM2000)
        {
            $self->handleSetPortConfig($streamPort, "loopback_suppress", "off");
        }
        $self->handleSetPortConfig($streamPort, "mask", $streamPort);
    }
}

sub tpDeleteStream
{
    my ($self, $streamId) = @_;

    my $chip = $self->{CHIP};

    my $switchNum = 0;
    my $info = ($self->tpGetSwitchInfo())[$switchNum];
    my @portList = $self->tpPlatformGetLogicalPortList($switchNum, "local");

    my @streamPorts = $self->validateExplicitList($FALSE, $streamId, @portList);
    if (!defined($streamId) || scalar(@streamPorts) == 0)
    {
        print($TP_MSG_ERR_STREAM_INVALID_ID);
        return;
    }

    # Prevent the API from warning about transient lane errors when disabling
    # internal loopback mode.
    my %void = (type => "fm_uint64", value => Math::BigInt->new());
    $chip->fmGetLoggingAttribute($FM_LOG_ATTR_LEVEL_MASK, 0, \%void);
    if (!$void{value}->copy()->band($FM_LOG_LEVEL_ERROR)->is_zero())
    {
        $self->tpHandleSetLoggingLevel("disable", "error");
    }

    foreach my $streamPort (@streamPorts)
    {
        $self->handleSetPortConfig($streamPort, "loopback", "off");
        $self->_releaseTraffic($info, $streamPort);
        # Re-enable loopback suppression on Bali and post-Bali based switches.
        if ($info->{'switchFamily'} != $FM_SWITCH_FAMILY_FM2000)
        {
            $self->handleSetPortConfig($streamPort, "loopback_suppress", "on");
        }
        $self->handleSetPortConfig($streamPort, "mask", $streamPort);
    }

    # Re-enable the error logging level if enabled previously.
    if (!$void{value}->copy()->band($FM_LOG_LEVEL_ERROR)->is_zero())
    {
        $self->tpHandleSetLoggingLevel("enable", "error");
    }
}

sub tpSetStreamPort
{
    my ($self, $streamId, $port) = @_;

    my $switchNum = 0;
    my @portList = $self->tpPlatformGetLogicalPortList($switchNum, "local");

    my @streamPorts = $self->validateExplicitList($FALSE, $streamId, @portList);
    if (!defined($streamId) || scalar(@streamPorts) == 0)
    {
        print($TP_MSG_ERR_STREAM_INVALID_ID);
        return;
    }

    my @ports = $self->validateExplicitList($FALSE, $port, @portList);
    if (!defined($port) || scalar(@ports) == 0)
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    foreach my $streamPort (@streamPorts)
    {
        $self->handleSetPortConfig($streamPort,
                                   "mask",
                                   "$streamId," . join(",", @ports));
    }
}

sub tpSetStreamMode
{
    my ($self, $streamId, $mode) = @_;

    my $switchNum = 0;
    my $info = ($self->tpGetSwitchInfo())[$switchNum];
    my @portList = $self->tpPlatformGetLogicalPortList($switchNum, "local");

    my @streamPorts = $self->validateExplicitList($FALSE, $streamId, @portList);
    if (!defined($streamId) || scalar(@streamPorts) == 0)
    {
        print($TP_MSG_ERR_STREAM_INVALID_ID);
        return;
    }

    if (!defined($mode) || scalar(grep {$_ eq $mode} @tpStreamModes) != 1)
    {
        print("Must specify a valid stream mode!\n");
        return;
    }

    foreach my $streamPort (@streamPorts)
    {
        if ($mode eq "loop-start")
        {
            $self->_modifyLoop($switchNum, $info, $streamPort, $FM_ENABLED);
            $self->_releaseTraffic($info, $streamPort);
        }
        elsif ($mode eq "loop-stop")
        {
            $self->_blockTraffic($info, $streamPort);
        }
        elsif ($mode eq "release")
        {
            $self->_modifyLoop($switchNum, $info, $streamPort, $FM_DISABLED);
            $self->_releaseTraffic($info, $streamPort);
            $self->_blockTraffic($info, $streamPort);
        }
        elsif ($mode eq "clear")
        {
            $self->_clearStream($info, $streamPort);
        }
    }
}

sub tpAddStreamPacket
{
    my ($self, $streamId, $type, @options) = @_;

    my $switchNum = 0;
    my @portList = $self->tpPlatformGetLogicalPortList($switchNum, "local");

    my @streamPorts = $self->validateExplicitList($FALSE, $streamId, @portList);
    if (!defined($streamId) || scalar(@streamPorts) == 0)
    {
        print($TP_MSG_ERR_STREAM_INVALID_ID);
        return;
    }

    if (!defined($type) || scalar(grep {$_ eq $type} @tpPacketTypes) != 1)
    {
        print("Must specify a valid packet type!\n");
        return;
    }

    foreach my $streamPort (@streamPorts)
    {
        if ($type eq "bytes")
        {
            $self->handleSendPacketDirected($streamPort, @options);
        }
        elsif ($type eq "file-pcap")
        {
            my ($filename) = @options;

            if (!defined($filename) || !-f($filename))
            {
                print("Must specify a PCAP file to read!\n");
                return;
            }

            $self->tpSendPacketFilePcap($streamPort,
                                        $filename,
                                        "ignore-timestamps");
        }
    }
}

sub tpResetStream
{
    my ($self, $streamId) = @_;

    my $switchNum = 0;
    my $info = ($self->tpGetSwitchInfo())[$switchNum];
    my @portList = $self->tpPlatformGetLogicalPortList($switchNum, "local");

    my @streamPorts = $self->validateExplicitList($FALSE, $streamId, @portList);
    if (!defined($streamId) || scalar(@streamPorts) == 0)
    {
        print($TP_MSG_ERR_STREAM_INVALID_ID);
        return;
    }

    foreach my $streamPort (@streamPorts)
    {
        $self->_clearStream($info, $streamPort);
    }
}

1;
