# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FM2000/WatermarkCore.pm
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

package Applications::TestPoint::Common::FM2000::WatermarkCore;
use strict;
use warnings;

use base qw(Exporter);

use List::Util qw(sum);
use SDK;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

our @EXPORT = qw(
    tpFM2000SetWatermarkParameters
    tpFM2000HandleShowMemory
);

# Private function
sub tpFM2000GetWatermarkParameters
{
    my ($self, $sw) = @_;
    my $chip = $self->{CHIP};

    my $N = 24;

    my $info = new SDK::fm_switchInfo();
    $chip->fmGetSwitchInfo($sw, $info);

    my $wpm = 
    {
        N                     => $N,
        memory                => 1000,
        segmentSize           => 256,
        disableLevel          => 4095 * 1024,
        numPausingFrames      => 0,
        numBytesToBuffer      => 0,
    };

    my @portList = $self->validateList("all", $self->tpPlatformGetPortRange());

    # Compute total max frame size rounded up to a
    # segment boundary.

    my $maxFrameSizeTotal = 0;
    my %val = (type => "fm_uint32", value => 0);

    # Hard code the cpu and port private memory sizes
    my $rxPrivWm = 14 * 1024;
    my $cpuPrivWm = 10 * 1024;

    # Start with just the CPU Private, add in the other ports below
    my $rxPrivAlloc = $cpuPrivWm;

    #printf("port list: %s\n", join(",", @portList));

    foreach my $p (@portList)
    {
        # Map port number for the benefit of non-SWAG Vegas. Ignore the
        # returned switchNum since it should match sw and may actually be
        # wrong for some platforms.
        my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($p);

        $chip->fmGetPortAttribute($sw, $logPort,
                                  $FM_PORT_MAX_FRAME_SIZE,
                                  \%val);

        # Round up to a segment
        if ($val{value} % 256)
        {
            $val{value} += 256 - ($val{value} % 256);
        }

        $maxFrameSizeTotal += $val{value};

        if ( ($logPort != 0) && ($self->getPauseEn($logPort) == $FM_ENABLED) )
        {
            $wpm->{numPausingFrames} += 1;
            $wpm->{numBytesToBuffer} += $val{value};
            $rxPrivAlloc += 0;
        }elsif ($logPort != 0)
        {
            $rxPrivAlloc += $rxPrivWm;
        }
    }
    
    # Compute rest of the watermarks

    # Skidpad: In order to protect against memory overflow, set the 
    #          Priveleged WM to 1M minus a maximum frame size from each port.
    my $T = 1000 * 1024 - $maxFrameSizeTotal;
    $wpm->{total} = $T;

    if ($info->{'switchVersion'} == $FM_SWITCH_VERSION_FM2224_A5)
    {
        $rxPrivAlloc = ($rxPrivWm * $N) + $cpuPrivWm;
    }

    # Disabling Tx Shared WM
    my $txSharedWm = $T;

    my $globalHighWm = $T - $rxPrivAlloc + 2; # The 2 accounts for PWD

    my $cpuTxSharedWm = 20 * 1024;

    # This assumes that one max frame size is covered by the skidpad
    my $pauseBufSize = $wpm->{numBytesToBuffer} + 
                       4 * 1024 * $wpm->{numPausingFrames};

    my $sharedSpace = $globalHighWm - $pauseBufSize;

    $wpm->{globalPauseOnWm} = $globalHighWm - $pauseBufSize;
    $wpm->{globalPauseOffWm} = $wpm->{globalPauseOnWm} - 3 * 1024 * $N;
    $wpm->{rxPauseOnWm} = $sharedSpace;
    $wpm->{rxPauseOffWm} = $wpm->{rxPauseOnWm} - 3 * 1024;
    $wpm->{rxSharedWm} = $sharedSpace;
    $wpm->{txSharedWm} = $txSharedWm;

    #printf("Total: %d\n", $T);
    #printf("pauseBufSize: %d\n", $pauseBufSize);
    #printf("globalHigh: %d\n", $globalHighWm);
    #printf("sharedSpace: %d\n", $sharedSpace);
    #printf("rxPrivAlloc: %d\n", $rxPrivAlloc);
 
    $wpm->{globalHighWm} = $globalHighWm;
    $wpm->{globalLowWm} = $globalHighWm;
    $wpm->{cpuPrivWm} = $cpuPrivWm;
    $wpm->{cpuTxSharedWm} = $cpuTxSharedWm;
    $wpm->{rxPrivWm} = $rxPrivWm;

    return $wpm;
}

sub tpFM2000SetWatermarkParameters
{
    my ($self, $sw, @pauseEnabledState) = @_;
    my $chip = $self->{CHIP};

    my $wpm = $self->tpFM2000GetWatermarkParameters($sw);

    #################################################################
    # Program the values
    # Hash for storing void * values
    my %val = (type => "fm_uint32", value => 0);

    # Get switch information from SDK
    my $info = new SDK::fm_switchInfo();
    $chip->fmGetSwitchInfo($sw, $info);
    my @portList = $self->validateList("all", $self->tpPlatformGetPortRange());

    $val{value} = $wpm->{total};
    $chip->fmSetSwitchQOS($sw, $FM_QOS_PRIV_WM, 0, \%val);

    $val{value} = $wpm->{globalHighWm};
    $chip->fmSetSwitchQOS($sw, $FM_QOS_HIGH_WM, 0, \%val);
    $val{value} = $wpm->{globalLowWm};
    $chip->fmSetSwitchQOS($sw, $FM_QOS_LOW_WM,  0, \%val);

    $val{value} = $wpm->{cpuPrivWm};
    $chip->fmSetPortQOS($sw, 0, $FM_QOS_RX_PRIVATE_WM, 0, \%val);
    $val{value} = $wpm->{cpuTxSharedWm};
    $chip->fmSetPortQOS($sw, 0, $FM_QOS_TX_SHARED_WM, 0, \%val);
    $val{value} = $wpm->{disableLevel};
    $chip->fmSetPortQOS($sw, 0, $FM_QOS_RX_SHARED_WM, 0, \%val);

    foreach my $p (@portList)
    {
        # Only for non CPU ports (CPU port is taken care of above)
        if ($p != 0)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($swithcNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($p);


            $val{value} = $wpm->{txSharedWm};
            $chip->fmSetPortQOS($sw, $logPort,
                                $FM_QOS_TX_SHARED_WM, 0, \%val);

            $val{value} = $wpm->{rxPrivWm};
            $chip->fmSetPortQOS($sw, $logPort,
                                $FM_QOS_RX_PRIVATE_WM, 0, \%val);

            if ($pauseEnabledState[$p])
            {
                if ($info->{'switchVersion'} != $FM_SWITCH_VERSION_FM2224_A5)
                {
                    $val{value} = 0;
                    $chip->fmSetPortQOS($sw, $logPort,
                                $FM_QOS_RX_PRIVATE_WM, 0, \%val);

                    # In this case, we want to use rxPause with no privates
                    $wpm->{globalPauseOnWm} = $wpm->{disableLevel};
                    $wpm->{globalPauseOffWm} = $wpm->{disableLevel};

                    my %maxFrameSize = (type => "fm_uint32", value => 0);

                    $chip->fmGetPortAttribute($sw, $logPort,
                                              $FM_PORT_MAX_FRAME_SIZE,
                                              \%maxFrameSize);


                    $wpm->{rxPauseOnWm} = int(($wpm->{total} / $wpm->{N}) -
                                          $maxFrameSize{value} - 4*1024);
                    $wpm->{rxPauseOffWm} = $wpm->{rxPauseOnWm} - 3*1024;
                }

                $val{value} = $wpm->{globalPauseOnWm};
                $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_GLOBAL_PAUSE_ON_WM,
                                    0, \%val);

                $val{value} = $wpm->{globalPauseOffWm};
                $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_GLOBAL_PAUSE_OFF_WM,
                                    0, \%val);

                $val{value} = $wpm->{rxPauseOnWm};
                $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_PRIVATE_PAUSE_ON_WM,
                                    0, \%val);

                $val{value} = $wpm->{rxPauseOffWm};
                $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_PRIVATE_PAUSE_OFF_WM,
                                    0, \%val);
                # No dropping
                $val{value} = $wpm->{disableLevel};
                $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_RX_SHARED_WM,
                                    0, \%val);
            } else {
                # Pause is disabled for this port

                $val{value} = $wpm->{disableLevel};

                $chip->fmSetPortQOS($sw, $logPort, 
                                    $FM_QOS_GLOBAL_PAUSE_ON_WM, 0, \%val);
                $chip->fmSetPortQOS($sw, $logPort,
                                    $FM_QOS_GLOBAL_PAUSE_OFF_WM, 0, \%val);
                $chip->fmSetPortQOS($sw, $logPort,
                                    $FM_QOS_PRIVATE_PAUSE_ON_WM, 0, \%val);
                $chip->fmSetPortQOS($sw, $logPort, 
                                    $FM_QOS_PRIVATE_PAUSE_OFF_WM, 0,
\%val);
                # Dropping Enabled
                $val{value} = $wpm->{rxSharedWm};
                $chip->fmSetPortQOS($sw, $logPort, 
                                    $FM_QOS_RX_SHARED_WM, 0, \%val);
            }
        }
    }
}

sub tpFM2000HandleShowMemory
{
    my ($self,$sw,$port) = @_;
    my $chip = $self->{CHIP};

    # initialize pause state if not already
    $self->initPauseState();

    my $wpm = undef;

    # Get switch information from SDK
    my $info = new SDK::fm_switchInfo();
    $chip->fmGetSwitchInfo($sw, $info);

    $wpm = $self->tpFM2000GetWatermarkParameters($sw);

    my %total = (type => "fm_uint32", value => 0); 
    my %shared = (type => "fm_uint32", value => 0);
    $chip->fmGetSwitchQOS($sw, $FM_QOS_GLOBAL_USAGE, 0, \%total);
    $chip->fmGetSwitchQOS($sw, $FM_QOS_SHARED_USAGE, 0, \%shared);
    printf("Sw %3d: Total Memory: %4d   Usage: %4d (shared use: %4d)\n",
           $sw, $wpm->{memory}, $total{value} / 1024,$shared{value} / 1024);

    my $switchCounters;
    $chip->fmGetSwitchCounters($sw, \$switchCounters);

    my %void = (type => "fm_uint32", value => 0);
    $chip->fmGetSwitchQOS($sw, $FM_QOS_PRIV_WM, 0, \%void);
    $self->printCounter($switchCounters->{cntGlobalPrivilegeDropPkts},
            sprintf("Privelege WM: %4d   Drops:",  $void{value} / 1024));
    %void = (type => "fm_uint32", value => 0);
    $chip->fmGetSwitchQOS($sw, $FM_QOS_HIGH_WM, 0, \%void);
    $self->printCounter($switchCounters->{cntGlobalHighDropPkts},
            sprintf("     High WM: %4d   Drops:",  $void{value} / 1024));
    %void = (type => "fm_uint32", value => 0);
    $chip->fmGetSwitchQOS($sw, $FM_QOS_LOW_WM, 0, \%void);
    $self->printCounter($switchCounters->{cntGlobalLowDropPkts},
            sprintf("      Low WM: %4d   Drops:",  $void{value} / 1024));

    my @portList = $self->validateList($port, $self->tpPlatformGetPortRange());

    my $header = 0;
    foreach my $p (@portList)
    {
        # Map port number for the benefit of non-SWAG Vegas. Ignore the
        # returned sw2 since it should match sw and may actually be
        # wrong for some platforms.
        my ($sw2, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($p);
        my $wpm = undef;

        if ($header == 0)
        {
            printf("Port  Rx-Use Tx-Use  TxHog   RxHog   RxPri   Gl-On   Gl-Off Rx-On  Rx-Off\n");
            $header = 1;
        }
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);

        my %txHog = (type => "fm_uint32", value => 0);
        my %rxHog = (type => "fm_uint32", value => 0);
        my %rxPriv = (type => "fm_uint32", value => 0);
        my %globalPauseOn = (type => "fm_uint32", value => 0);
        my %globalPauseOff = (type => "fm_uint32", value => 0);
        my %rxPauseOn = (type => "fm_uint32", value => 0);
        my %rxPauseOff = (type => "fm_uint32", value => 0);
        my %rxUse = (type => "fm_uint32", value => 0);
        my %txUse = (type => "fm_uint32", value => 0);

        $chip->fmGetPortQOS($sw,$logPort, $FM_QOS_RX_USAGE,
                0, \%rxUse);
        $chip->fmGetPortQOS($sw,$logPort, $FM_QOS_TX_USAGE,
                0, \%txUse);

        $chip->fmGetPortQOS($sw,$logPort, $FM_QOS_RX_SHARED_WM,
                0, \%rxHog);
        $chip->fmGetPortQOS($sw,$logPort, $FM_QOS_TX_SHARED_WM,
                0, \%txHog);
        $chip->fmGetPortQOS($sw,$logPort, $FM_QOS_GLOBAL_PAUSE_ON_WM,
                0, \%globalPauseOn);
        $globalPauseOn{value} /= 1024;
        $globalPauseOn{value} = sprintf("%04d", $globalPauseOn{value});
        $chip->fmGetPortQOS($sw,$logPort, $FM_QOS_GLOBAL_PAUSE_OFF_WM,
                0, \%globalPauseOff);
        $globalPauseOff{value} /= 1024;
        $globalPauseOff{value} = sprintf("%04d", $globalPauseOff{value});

        $chip->fmGetPortQOS($sw,$logPort, $FM_QOS_RX_PRIVATE_WM,
                0, \%rxPriv);
        $chip->fmGetPortQOS($sw,$logPort, $FM_QOS_PRIVATE_PAUSE_ON_WM,
                0, \%rxPauseOn);
        $chip->fmGetPortQOS($sw,$logPort, $FM_QOS_PRIVATE_PAUSE_OFF_WM,
                0, \%rxPauseOff);

        printf(" %2d   %4d   %4d    %4d    %4d    %4d    %4s    %4s    %4d %4d\n",
           $p, $rxUse{value} / 1024, $txUse{value} / 1024,
           $txHog{value} / 1024, $rxHog{value} / 1024,
           $rxPriv{value} / 1024,
           $globalPauseOn{value}, $globalPauseOff{value},
           $rxPauseOn{value} / 1024, $rxPauseOff{value} / 1024);
    }
}
1;
