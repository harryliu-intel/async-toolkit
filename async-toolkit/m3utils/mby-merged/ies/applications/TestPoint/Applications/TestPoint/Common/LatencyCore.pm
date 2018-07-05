# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/LatencyCore.pm
# Creation Date:    Sept. 29, 2007
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

package Applications::TestPoint::Common::LatencyCore;
use strict;
use warnings;

use base qw(Exporter);

use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;
use Scripts::GenFrame;
our @EXPORT = qw(
    tpHandleTestLatency
);

sub MeasureUtilization
{
    my ($self, $mode, $port, $frameSize, $ifg, $lstart, $lend)  = @_;
    my $chip = $self->{CHIP};
    my $start = undef;
    my $end = undef;
    
    if (!defined($ifg))
    {
        $ifg = 10;
    }

    # time in seconds
    my $duration = 1;

    # frame size, IFG+preamble
    my $tobits = Math::BigFloat->new(8*($frameSize + $ifg + 8));
    my $portCounters;
    my $bitrate = Math::BigFloat->new($duration);
    my $mbit = Math::BigFloat->new(1.0e6);

    # Recover switch and port
    my ($sw_id, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);

    $chip->fmGetPortCounters($sw_id, $logPort, \$portCounters);
    $start = $portCounters->{cntRxUcstPkts};
    $$lstart = $start->bstr();
    sleep($duration);
    $chip->fmGetPortCounters($sw_id, $logPort, \$portCounters);
    $end = $portCounters->{cntRxUcstPkts};
    $$lend = $end->bstr();
    $end->bsub($start);

    # printf("MeasureUtilization: %d frames in %d seconds\n", $end->bstr(), $duration);
    $end->bmul($tobits);
    # printf("MeasureUtilization: %d bits in %d seconds\n", $end->bstr(), $duration);
    $end->bdiv($bitrate);
    # printf("MeasureUtilization: %3.3f bit/s\n", $end->bstr());
    $end->bdiv($mbit);
    # printf("MeasureUtilization: %3.3f Mbit/s\n", $end->bstr());

    return $end->bstr() * 100.0 / 10000;
}

sub PercentDiff
{
    my ($self, $a, $b) = @_;

    return (($a - $b)*100.0/$b);
}

sub MeasureLatency
{
    my ($self, $mode, $port, $numPorts, $duration, $nframes)  = @_;
    my $chip = $self->{CHIP};
    my $start = undef;
    my $end = undef;
    my $np = Math::BigFloat->new($numPorts);
    my $portCounters;
    my $totalTime = Math::BigFloat->new($nframes * 1e9 * $duration);

    # Recover switch and port
    my ($sw_id, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);

    $chip->fmGetPortCounters($sw_id, $logPort, \$portCounters);
    $start = $portCounters->{cntRxUcstPkts};

    sleep $duration; 

    $chip->fmGetPortCounters($sw_id, $logPort, \$portCounters);
    $end = $portCounters->{cntRxUcstPkts};

    $end->bsub($start);
    $end->bmul($np);
    $totalTime->bdiv($end);

    return $totalTime->bstr();
}

##@cmethod public int tpHandleTestLatency(int frameSize, int *ports)
#
# @desc         Handles testing latency via an internal racetrack loop.
#
# @param[in]    frameSize is the size of the frame in bytes.
#
# @param[in]    ports are the list of ports to use in the latency loop.
#
sub tpHandleTestLatency
{
    my ($self, $mode, $frameSize, $port, $utilization) = @_;
    my ($testpoint) = $self->{TESTPOINT};
    local *OLDOUT;

    my $chip = $self->{CHIP};

    if (!defined($port))
    {
        $port = "all";
    }

    if (!defined($utilization))
    {
        $utilization = 95;
    }

    my ($portMin, $portMax) = $testpoint->tpPlatformGetPortRange;

    # we don't want port 0 in the list, so be explicit
    my @portList = $self->validateList($port, 1, $portMax);

    my $numPorts = scalar(@portList);

    if (!defined($port) || (scalar(@portList) < 2))
    {
        print("Must specify a valid port or port range of at least 2 ports!\n");
        return;
    }

    my $firstPort = shift(@portList);

    # cycle ports to ensure old rings are dead 
    $self->handleSetPort($port, "powerdown");
    sleep(1);
    $self->handleSetPort($port, "up");
    sleep(1);

    # turn on loopback
    $self->handleSetPortConfig($port, "loopback", "on");


    if ($mode eq "L3")
    {
        $self->handleRegSetBit("FM_SYS_CFG_8", 0);
        $self->handleSetPortConfig($port, "parser_cfg", "L4");
        # turn off TTL decrement
        $self->handleRegClearBitPort($port, "FM_MAC_CFG_2", 17);
    }
    else
    {
        $self->handleSetPortConfig($port, "parser_cfg", "L2");
        $self->handleRegClearBit("FM_SYS_CFG_8", 0);
    }

    # setup mode

    open(NULLOUT, ">/dev/null");
    *OLDOUT = *STDOUT;
    *STDOUT = *NULLOUT;
    $self->handleTestSnake($mode, $firstPort, join(",", @portList));
    *STDOUT = *OLDOUT;
    close(NULLOUT);

    my $measuredUtilization = 0;
    my $utilCap = 0;
    my $nframes = 0;
    my $lastStart = "";
    my $lastEnd = "";
    my $SMAC = "00:00:02:00:00:01";
    my $DMAC = "00:0a:00:01:02:03";

    my $params =
    {
        "dmac"      => $DMAC,
        "smac"      => $SMAC,
        "dip"       => "192.0.0.1"
    };

    my $frame = GenFrame::genIPv4UDPFrame($params);
    my @bytes = ();

    for (my $q = 0; $q < (scalar(@{$frame})-12); $q++)
    {
        $bytes[$q] = sprintf("%x", $frame->[$q+12]);
    }

    printf("Pushing frames into ring until utilization matches %2.0f%%\n",
           $utilization);

    $self->handleRegWritePort("0..24", "FM_STAT_RxUcstPktsNonIP", 0);

    my $portCounters;

    do
    {
        $nframes++;

        $self->handleSendPacket($firstPort, $frameSize, $DMAC, $SMAC,
                                @bytes);


        # the following waits for the frame to be sent
        sleep(2);

        $measuredUtilization 
            = $self->MeasureUtilization($mode, $firstPort, $frameSize, 12, 
                                        \$lastStart, \$lastEnd);

        if (($nframes > 0) && ($nframes % 32 == 0))
        {
            printf("$nframes frame(s) in ring, meas. util. %.2f%%, " .
                   "exp. util. %2.0f%%\n",
                   $measuredUtilization, $utilization);
        }

        if ($measuredUtilization <= 0.001)
        {
            printf("Utilization too low, suspect ring configuration\n");
            printf("Last utilization: %.2f Delta: [%s - %s]\n", 
                   $measuredUtilization, $lastEnd, $lastStart);
            return;
        }
        elsif ($measuredUtilization >= 100.0)
        {
            printf("Utilization too high, clearing ring...\n");
            $nframes--;
            $utilCap = 1;

            $measuredUtilization = $utilization;
        }

    } while ($self->PercentDiff($utilization, $measuredUtilization) >= 0.5);

    # now clear the ring and requeue (takes care of cases where we go above
    # 100%)

    if ($utilCap)
    {
        $self->handleSetPort($port, "powerdown");
        sleep(1);
        $self->handleSetPort($port, "up");
        sleep(1);

        $self->handleRegWritePort("0..24", "FM_STAT_RxUcstPktsNonIP", 0);

        for(my $q = 0; $q < $nframes; $q++)
        {
            $self->handleSendPacket($firstPort, $frameSize, $DMAC, $SMAC,
                                    @bytes);

        }

        # the following waits for the frames to be sent
        sleep(2);

        $measuredUtilization 
            = $self->MeasureUtilization($mode, $firstPort, $frameSize, 12, 
                                        \$lastStart, \$lastEnd);
    }

    if ($measuredUtilization <= 0.001)
    {
        printf("Utilization too low, suspect ring configuration\n");
        printf("Last utilization: %.2f Delta: [%s - %s]\n", 
               $measuredUtilization, $lastEnd, $lastStart);
        return;
    }

    printf("$nframes frame(s) in ring, meas. util. %.2f%%, exp. util. %2.0f%%\n",
           $measuredUtilization, $utilization);

    printf("Measuring latency over port(s) $port, " .
           "using $frameSize byte frames...\n");

    my $latency = $self->MeasureLatency($mode, $firstPort, scalar(@portList)+1, 
                                        10, $nframes); 

    printf("Latency: %3.3f ns\n", $latency);

    # cycle ports to ensure ring ends up dead
    $self->handleSetPort($port, "powerdown");
    sleep(1);
    $self->handleSetPort($port, "up");
    $self->handleSetPortConfig($port, "loopback", "off");

    if ($mode eq "L3")
    {
        # turn on TTL decrement
        $self->handleRegSetBitPort($port, "FM_MAC_CFG_2", 17);
    }
}
