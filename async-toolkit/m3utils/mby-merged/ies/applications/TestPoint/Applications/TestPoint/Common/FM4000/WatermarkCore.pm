# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FM4000/WatermarkCore.pm
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

package Applications::TestPoint::Common::FM4000::WatermarkCore;
use strict;
use warnings;

use base qw(Exporter);

use List::Util qw(sum);
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

our @EXPORT = qw(
    tpFM4000SetWatermarkParameters
    tpFM4000HandleShowMemory
);

# Private function
sub tpFM4000GetWatermarkParameters
{
    my ($self) = @_;
    my $chip = $self->{CHIP};
    my %val = (type => "fm_uint32", value => 0);

    my $N = 24;

    my $wpm = 
    {
        memory                => 2000,
        segmentSize           => 512,
        disableLevel          => 4095 * 1024,
        numPausingPorts       => 0,
        numBytesToBuffer      => 0,
    };

    my @portList = $self->validateList("all", $self->tpPlatformGetPortRange());

    # Compute total max frame size rounded up to a
    # segment boundary.

    my $maxFrameSizeTotal = 0;
    foreach my $p (@portList)
    {
        # Map port number for the benefit of non-SWAG Vegas. Ignore the
        # returned switch since it should match the original switch and 
        # may actually be wrong for some platforms.
        my ($sw, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($p);

        # Since bali has 2MB, always assume 10240B frames
        #$chip->fmGetPortAttribute($sw, $logPort,
        #                          $FM_PORT_MAX_FRAME_SIZE,
        #                          \%val);
        $val{value} = 10240;

        # Round up to a segment
        if ($val{value} % 512)
        {
            $val{value} += 512 - ($val{value} % 512);
        }

        $maxFrameSizeTotal += $val{value};

        if ( ($logPort != 0) && ($self->getPauseEn($logPort) == $FM_ENABLED) )
        {
            $wpm->{numPausingPorts} += 1;
            $wpm->{numBytesToBuffer} += $val{value};
        }
    }
    
    # Compute rest of the watermarks

    # Skidpad: In order to protect against memory overflow, set the 
    #          Priveleged WM to 1M minus a maximum frame size from each port.
    my $T = $wpm->{memory} * 1024 - $maxFrameSizeTotal;
    $wpm->{total} = $T;

    # Hard code the cpu and port private memory sizes
    my $rxPrivWm = 14 * 1024;
    my $cpuPrivWm = 10 * 1024;

    # set to 0 for now, need to run lossy prioritized Nto1 to determine
    # correect value
    my $txPrivWm = 8 * 512;
    my $txTcPrivWm = 0;


    # Allocate space for each section
    my $rxPrivAlloc = ($rxPrivWm * $N) + $cpuPrivWm;
    my $txPrivAlloc = ($txPrivWm * $N) + ($txTcPrivWm*8*$N);

    # Disabling Tx Shared WM
    my $txSharedWm = $T;

    #PauseOvershoot
    my $pauseBufAlloc = $wpm->{numBytesToBuffer} + 
                       12 * 1024 * $wpm->{numPausingPorts};

    #TODO: check chip version
    # A0 fix for lost pause on frames
    $pauseBufAlloc *= 2;

    my $sharedSpaceAlloc = $T - $pauseBufAlloc - 
                            $rxPrivAlloc - $txPrivAlloc;

    #
    #   --------------------------  Top of Memory/Global Privledge
    #   |                        |
    #   |   Pause Buffer Alloc   |    
    #   |                        |
    #   +------------------------+  RxShared/GlobalPauseOn
    #   |                        |
    #   |                        |
    #   |   Shared Space Alloc   |     
    #   |                        |
    #   |                        |
    #   +------------------------+  
    #   |   Tx Private Alloc     |      
    #   +------------------------+
    #   |   Rx Private Alloc     |
    #   +------------------------+

    #printf("Top of Memory: %d\n", $T);
    #printf("numBytesToBuffer: %d\n", $wpm->{numBytesToBuffer});
    #printf("numPausingPorts: %d\n", $wpm->{numPausingPorts});
    #printf("rxPrivateAlloc: %d\n", $rxPrivAlloc);
    #printf("txPrivateAlloc: %d\n", $txPrivAlloc);
    #printf("pauseBufAlloc: %d\n", $pauseBufAlloc);
    #printf("sharedSpaceAlloc: %d\n", $sharedSpaceAlloc);

    # Set Wm based on alloc
    my $globalPrivWm = $pauseBufAlloc + $sharedSpaceAlloc;

    my $cpuTxSharedWm = 20 * 1024;

    #if($wpm->{numPausingPorts} == 24)
    #{
        $wpm->{globalPauseOnWm} = $rxPrivAlloc + $txPrivAlloc + $sharedSpaceAlloc;
        $wpm->{globalPauseOffWm} = $wpm->{globalPauseOnWm} - (3 * 1024 * $N);
        $wpm->{rxPauseOnWm} = $wpm->{disableLevel};
        $wpm->{rxPauseOffWm} = $wpm->{disableLevel};
    #}else
    #{
    #    $wpm->{globalPauseOnWm} = $wpm->{disableLevel};
    #    $wpm->{globalPauseOffWm} = $wpm->{disableLevel};
    #    $wpm->{rxPauseOnWm} = $rxPrivAlloc + $txPrivAlloc + $sharedSpaceAlloc;
    #    $wpm->{rxPauseOffWm} = $wpm->{rxPauseOnWm} - 3*1024;
    #}

    $wpm->{rxSharedWm} = $rxPrivAlloc + $txPrivAlloc + $sharedSpaceAlloc;
    $wpm->{txSharedWm} = $txSharedWm;
    $wpm->{txPrivWm} = $txPrivWm;
    $wpm->{txtcPrivWm} = $txTcPrivWm;

    $wpm->{globalPrivWm} = $globalPrivWm;
    $wpm->{cpuPrivWm} = $cpuPrivWm;
    $wpm->{cpuTxSharedWm} = $cpuTxSharedWm;
    $wpm->{rxPrivWm} = $rxPrivWm;

    return $wpm;
}

sub tpFM4000SetWatermarkParameters
{
    my ($self, $sw, @pauseEnabledState) = @_;
    my $chip = $self->{CHIP};
    my @portList = $self->validateList("all", $self->tpPlatformGetPortRange());

    my $wpm = $self->tpFM4000GetWatermarkParameters();

    my %val = (type => "fm_uint32", value => 0);

    my $value = 0;
    $chip->fmReadUINT32($sw, $chip->FM_CM_GLOBAL_CFG, \$value);
    $value = ($value & 0xffff00ff) | (25*4 << 8);
    $chip->fmWriteUINT32($sw, $chip->FM_CM_GLOBAL_CFG, $value);

    # Set Global WM (aka Global priv)
    $val{value} = $wpm->{total};
    $chip->fmSetSwitchQOS($sw, $FM_QOS_PRIV_WM, 0, \%val);

    # This is a work around for bug 10741
    # TODO change to detect rev A1
    for(my $tc=0; $tc < 8; $tc++)
    {
        if($tc < 2)
        {
            $val{value}=$FM_QOS_TC_SMP_0;
        }else{
            $val{value}=$FM_QOS_TC_SMP_NONE;
        }
        $chip->fmSetSwitchQOS($sw, $FM_QOS_TC_SMP_MAP, $tc, \%val)
    }

    # Set Global Shared (aka GlobalHigh)
    $val{value} = $wpm->{globalPrivWm};
    for(my $i=0;$i<16;$i++)
    {
        $chip->fmSetSwitchQOS($sw, $FM_QOS_SHARED_PRI_WM, $i, \%val);
    }

    # Set CPU wms
    $val{value} = $wpm->{cpuPrivWm};
    $chip->fmSetPortQOS($sw, 0, $FM_QOS_RX_PRIVATE_WM, 0, \%val);
    $val{value} = 0;
    $chip->fmSetPortQOS($sw, 0, $FM_QOS_TX_PRIVATE_WM, 0, \%val);
    $val{value} = 0;
    for(my $tc = 0 ; $tc < 8 ; $tc++)
    {
        $chip->fmSetPortQOS($sw, 0, $FM_QOS_TX_TC_PRIVATE_WM, $tc, \%val);
    }
    $val{value} = $wpm->{cpuTxSharedWm};
    $chip->fmSetPortQOS($sw, 0, $FM_QOS_TX_HOG_WM, 0, \%val);
    $val{value} = $wpm->{disableLevel};
    $chip->fmSetPortQOS($sw, 0, $FM_QOS_RX_HOG_WM, 0, \%val);

    foreach my $p (@portList)
    {
        #CPU is set above
        if($p != 0)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($p);

            # Private WMs
            $val{value} = $wpm->{rxPrivWm};
            $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_RX_PRIVATE_WM, 0, \%val);

            $val{value} = $wpm->{txPrivWm};
            $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_TX_PRIVATE_WM, 0, \%val);

            $val{value} = $wpm->{txtcPrivWm};
            for(my $tc = 0 ; $tc < 8 ; $tc++)
            {
                $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_TX_TC_PRIVATE_WM, $tc, \%val);
            }
            
            # Tx Shared
            $val{value} = $wpm->{txSharedWm};
            $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_TX_HOG_WM, 0, \%val);

            if($pauseEnabledState[$p])
            {
                # Shared Pause WM (aka global pause)
                $val{value} = $wpm->{globalPauseOnWm};
                $chip->fmSetSwitchQOS($sw, $FM_QOS_SHARED_PAUSE_ON_WM, 0, \%val);
                $val{value} = $wpm->{globalPauseOffWm};
                $chip->fmSetSwitchQOS($sw, $FM_QOS_SHARED_PAUSE_OFF_WM, 0, \%val);
        
                $val{value} = $wpm->{rxPauseOnWm};
                $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_PRIVATE_PAUSE_ON_WM, 0, \%val);
                $val{value} = $wpm->{rxPauseOffWm};
                $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_PRIVATE_PAUSE_OFF_WM, 0, \%val);
                 
                $val{value} = $wpm->{disableLevel};
                $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_RX_HOG_WM, 0, \%val);
            } else {
                $val{value} = $wpm->{disableLevel};

                $chip->fmSetSwitchQOS($sw, $FM_QOS_SHARED_PAUSE_ON_WM, 0, \%val);
                $chip->fmSetSwitchQOS($sw, $FM_QOS_SHARED_PAUSE_OFF_WM, 0, \%val);

                $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_PRIVATE_PAUSE_ON_WM, 0, \%val);
                $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_PRIVATE_PAUSE_OFF_WM, 0, \%val);

                $val{value} = $wpm->{rxSharedWm};
                $chip->fmSetPortQOS($sw, $logPort, $FM_QOS_RX_HOG_WM, 0, \%val);
            }
        }
    }
}

sub tpFM4000HandleShowMemory
{
    my ($self, $sw, $port) = @_;
    my $chip = $self->{CHIP};
    my $sw2;
    my $logPort;
    my $numberOfSwitches = 1;

    my $wpm = $self->tpFM4000GetWatermarkParameters();

    my $info = new SDK::fm_switchInfo();
    my $infoSw0 = new SDK::fm_switchInfo();
    $chip->fmGetSwitchInfo($sw, $info);
    $chip->fmGetSwitchInfo(0, $infoSw0);

    printf("\n");
    if ($info->{'switchFamily'} != $FM_SWITCH_FAMILY_SWAG)
    {
        my %total = (type => "fm_uint32", value => 0);
        my %shared0 = (type => "fm_uint32", value => 0);
        my %shared1 = (type => "fm_uint32", value => 0);
        $chip->fmGetSwitchQOS($sw, $FM_QOS_GLOBAL_USAGE, 0, \%total);
        $chip->fmGetSwitchQOS($sw, $FM_QOS_SHARED_USAGE, 0, \%shared0);
        $chip->fmGetSwitchQOS($sw, $FM_QOS_SHARED_USAGE, 1, \%shared1);
    
        printf("Sw %3d: Total Mem: %4d Total Usage: %4d SMP0 Usage: %4d SMP1 Usage: %4d\n",
               $sw, $wpm->{memory}, $total{value}/1024, 
               $shared0{value}/1024, $shared1{value}/1024);
    }
    my @portList = $self->validateList($port, $self->tpPlatformGetPortRange());

    my $header = 0;
    foreach my $p (@portList)
    {
        my @port_array;
        my $cpuPort = 0;

        if ($infoSw0->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
        {
            if($sw == 0)
            {
                @port_array = $self->tpPlatformMapGlobalToLogicalPort($p);
            }
            else
            {
                @port_array = $self->tpPlatformMapGlobalToLogicalPortPerSwitch($p);
            }
        }
        else
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switch since it should match the original switch and 
            # may actually be wrong for some platforms.
            @port_array = $self->tpPlatformMapGlobalToLogicalPort($p);
            $port_array[0] = $sw;
        }
        
        $sw2 = $port_array[0];
        $logPort = $port_array[1];
        $numberOfSwitches = 1;
        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG && $p == 0)
        {
            # Special case for CPU port. Force the switch to be the switch
            # we're displaying for
            if ($sw != 0)
            {
               $sw2 = $sw;
            }
            else
            {
                $sw2 = 1;
                $numberOfSwitches = 3;
            }
            $logPort = 0;
            $cpuPort = 1;
        }
        my $n = 0;
        while($n < $numberOfSwitches)
        {
            if ($sw == $sw2 || $cpuPort == 1)
            {
                if ($header == 0)
                {
                    printf("Port |  Rx RxS0 RxS1 | TxS0 TxS1 TxT0 TxT1 TxT2 TxT3 TxT4 TxT5 TxT6 TxT7\n");
                    printf("-----+---------------+--------------------------------------------------\n");
    
                    $header = 1;
                }
                my %rxTotal = (type => "fm_uint32", value=>0);
                my %rxSmp0 = (type => "fm_uint32", value=>0);
                my %rxSmp1 = (type => "fm_uint32", value=>0);
                my %txSmp0 = (type => "fm_uint32", value=>0);
                my %txSmp1 = (type => "fm_uint32", value=>0);
                my @txtc;
    
                $chip->fmGetPortQOS($sw2,$logPort, $FM_QOS_RX_USAGE,
                        0, \%rxTotal);
                $chip->fmGetPortQOS($sw2,$logPort, $FM_QOS_RX_SMP_USAGE,
                        0, \%rxSmp0);
                $chip->fmGetPortQOS($sw2,$logPort, $FM_QOS_RX_SMP_USAGE,
                        1, \%rxSmp1);
                $chip->fmGetPortQOS($sw2,$logPort, $FM_QOS_TX_USAGE,
                        0, \%txSmp0);
                $chip->fmGetPortQOS($sw2,$logPort, $FM_QOS_TX_USAGE,
                        1, \%txSmp1);
    
                $rxTotal{value} /= 1024;
                $rxSmp0{value} /= 1024;
                $rxSmp1{value} /= 1024;
                $txSmp0{value} /= 1024;
                $txSmp1{value} /= 1024;
    
                for(my $tc = 0; $tc < 8 ; $tc++)
                {
                    $txtc[$tc] = {type => "fm_uint32", value=>0};
                    $chip->fmGetPortQOS($sw2,$logPort, $FM_QOS_TX_TC_USAGE,
                            $tc, $txtc[$tc]);
                    $txtc[$tc]->{value} /= 1024;
                }
    
                printf("%2d   |%4d %4d %4d | %4d %4d %4d %4d %4d %4d %4d %4d %4d %4d\n",
                       $p, $rxTotal{value}, $rxSmp0{value}, $rxSmp1{value},
                       $txSmp0{value}, $txSmp1{value}, 
                       $txtc[0]->{value}, $txtc[1]->{value}, $txtc[2]->{value},
                       $txtc[3]->{value}, $txtc[4]->{value}, $txtc[5]->{value},
                       $txtc[6]->{value}, $txtc[7]->{value});
                }
            $n++;
            $sw2++;
        }
    }

}

sub tpFM4000HandleShowWatermarks
{
    my ($self, $sw, $port) = @_;
    my $chip = $self->{CHIP};

    my $wpm = $self->tpFM4000GetWatermarkParameters();

    my @globalSharedWm;
    my %globalWm = ( type=>"fm_uint32", value=>0 );
    my %globalPauseOnWm0 = ( type=>"fm_uint32", value=>0 );
    my %globalPauseOffWm0 = ( type=>"fm_uint32", value=>0 );
    my %globalPauseOnWm1 = ( type=>"fm_uint32", value=>0 );
    my %globalPauseOffWm1 = ( type=>"fm_uint32", value=>0 );
    my $sw2;
    my $logPort;

    my $info = new SDK::fm_switchInfo();
    $chip->fmGetSwitchInfo($sw, $info);

    $chip->fmGetSwitchQOS($sw, $FM_QOS_PRIV_WM, 0, \%globalWm);
    $chip->fmGetSwitchQOS($sw, $FM_QOS_SHARED_PAUSE_ON_WM, 
                          0, \%globalPauseOnWm0);
    $chip->fmGetSwitchQOS($sw, $FM_QOS_SHARED_PAUSE_OFF_WM, 
                          0, \%globalPauseOffWm0);
    $chip->fmGetSwitchQOS($sw, $FM_QOS_SHARED_PAUSE_ON_WM, 
                          1, \%globalPauseOnWm1);
    $chip->fmGetSwitchQOS($sw, $FM_QOS_SHARED_PAUSE_OFF_WM, 
                          1, \%globalPauseOffWm1);

    if($globalPauseOnWm0{value} >= $wpm->{disableLevel})
    {
        $globalPauseOnWm0{value} = "--";
    } else {
        $globalPauseOnWm0{value} = int($globalPauseOnWm0{value}/1024);
    }

    if($globalPauseOffWm0{value} >= $wpm->{disableLevel})
    {
        $globalPauseOffWm0{value} = "--";
    } else {
        $globalPauseOffWm0{value} = int($globalPauseOffWm0{value}/1024);
    }

    if($globalPauseOnWm1{value} >= $wpm->{disableLevel})
    {
        $globalPauseOnWm1{value} = "--";
    } else {
        $globalPauseOnWm1{value} = int($globalPauseOnWm1{value}/1024);
    }

    if($globalPauseOffWm1{value} >= $wpm->{disableLevel})
    {
        $globalPauseOffWm1{value} = "--";
    } else {
        $globalPauseOffWm1{value} = int($globalPauseOffWm1{value}/1024);
    }

    for(my $swPri = 0; $swPri < 16 ; $swPri++ )
    {
        $globalSharedWm[$swPri] = { type=>"fm_uint32", value=>0 };
        $chip->fmGetSwitchQOS($sw, $FM_QOS_SHARED_PRI_WM, $swPri, $globalSharedWm[$swPri]);
    }

    printf("\n");

    if ($info->{'switchFamily'} != $FM_SWITCH_FAMILY_SWAG)
    {
    printf("Global Wm: %4d   SMP0 Pause On: %4s    SMP Pause Off: %4s\n",
        int($globalWm{value}/1024), $globalPauseOnWm0{value}, 
        $globalPauseOffWm0{value});

    printf("                  SMP1 Pause On: %4s    SMP Pause Off: %4s\n",
            $globalPauseOnWm1{value}, $globalPauseOffWm1{value});
    printf("                     Global Shared Wm per Switch Priority\n");
    printf("   0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15\n");
    printf("-------------------------------------------------------------------------------\n");
    printf("%4d %4d %4d %4d %4d %4d %4d %4d %4d %4d %4d %4d %4d %4d %4d %4d\n",
        int($globalSharedWm[0]->{value}/1024), int($globalSharedWm[1]->{value}/1024),
        int($globalSharedWm[2]->{value}/1024), int($globalSharedWm[3]->{value}/1024),
        int($globalSharedWm[4]->{value}/1024), int($globalSharedWm[5]->{value}/1024),
        int($globalSharedWm[6]->{value}/1024), int($globalSharedWm[7]->{value}/1024),
        int($globalSharedWm[8]->{value}/1024), int($globalSharedWm[9]->{value}/1024),
        int($globalSharedWm[10]->{value}/1024), int($globalSharedWm[11]->{value}/1024),
        int($globalSharedWm[12]->{value}/1024), int($globalSharedWm[13]->{value}/1024),
        int($globalSharedWm[14]->{value}/1024), int($globalSharedWm[15]->{value}/1024));
    }


    $chip->fmGetSwitchInfo(0, $info);

    my @portList = $self->validateList($port, $self->tpPlatformGetPortRange());

    foreach my $smp (0,1)
    {
        my $numberOfSwitches = 1;
        my $header = 0;
        foreach my $p (@portList)
        {
        my @port_array;
        my $cpuPort = 0;

            if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
            {
                if($sw == 0)
                {
                    @port_array = $self->tpPlatformMapGlobalToLogicalPort($p);
                }
                else
                {
                    @port_array = $self->tpPlatformMapGlobalToLogicalPortPerSwitch($p);
                }
            }
            else
            {
                # Map port number for the benefit of non-SWAG Vegas. Ignore the
                # returned switch since it should match the original switch and 
                # may actually be wrong for some platforms.
                @port_array = $self->tpPlatformMapGlobalToLogicalPort($p);
                $port_array[0] = $sw;
            }

            $sw2 = $port_array[0];
            $logPort = $port_array[1];
            $numberOfSwitches = 1;
            if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG && $p == 0)
            {
                # Special case for CPU port. Force the switch to be the switch
                # we're displaying for
                if ($sw != 0)
                {
                   $sw2 = $sw;
                }
                else
                {
                    $sw2 = 1;
                    $numberOfSwitches = 3;
                }
                $logPort = 0;
                $cpuPort = 1;
            }

            my $n = 0;
            while($n < $numberOfSwitches)
            {
                if ($sw == $sw2 || $cpuPort == 1)
                {
                    if ($header == 0)
                    {
                        printf("\n");
                        printf("Po |  Rx SMP$smp  |  Rx Pause |  Tx SMP$smp  |                 TX-TC Priv             \n");
                        printf("rt | Priv  Hog |   On  Off | Priv  Hog |  Tc0  Tc1  Tc2  Tc3  Tc4  Tc5  Tc6  Tc7\n");
                        printf("---+-----------+-----------+-----------+----------------------------------------\n");
    
                        $header = 1;
                    }
    
                    my %rxPrivWm = ( type=>"fm_uint32", value=>0 );
                    my %rxHogWm = ( type=>"fm_uint32", value=>0 );
                    my %rxPauseOnWm = ( type=>"fm_uint32", value=>0 );
                    my %rxPauseOffWm = ( type=>"fm_uint32", value=>0 );
                    my @txtcPrivWm;
                    my %txHogWm = ( type=>"fm_uint32", value=>0 );
                    my %txPrivWm = ( type=>"fm_uint32", value=>0 );
    
                    $chip->fmGetPortQOS($sw2, $logPort, $FM_QOS_RX_PRIVATE_WM, $smp, \%rxPrivWm);
                    $chip->fmGetPortQOS($sw2, $logPort, $FM_QOS_RX_HOG_WM, $smp, \%rxHogWm);
             
                    for(my $tc=0 ; $tc < 8 ; $tc++)
                    {
                        $txtcPrivWm[$tc] = { type=>"fm_uint32", value=>0 };
                        $chip->fmGetPortQOS($sw2, $logPort, $FM_QOS_TX_TC_PRIVATE_WM, $tc, $txtcPrivWm[$tc]);
                    }
    
                    $chip->fmGetPortQOS($sw2, $logPort, $FM_QOS_TX_PRIVATE_WM, $smp, \%txPrivWm);
                    $chip->fmGetPortQOS($sw2, $logPort, $FM_QOS_TX_HOG_WM, $smp, \%txHogWm);
    
                    $chip->fmGetPortQOS($sw2, $logPort, $FM_QOS_PRIVATE_PAUSE_ON_WM, $smp, \%rxPauseOnWm);
                    $chip->fmGetPortQOS($sw2, $logPort, $FM_QOS_PRIVATE_PAUSE_OFF_WM, $smp, \%rxPauseOffWm);
    
                    if($rxPauseOnWm{value} >= $wpm->{disableLevel})
                    {
                        $rxPauseOnWm{value} = "--";
                    }else
                    {
                        $rxPauseOnWm{value} = int($rxPauseOnWm{value}/1024);
                    }
    
                    if($rxPauseOffWm{value} >= $wpm->{disableLevel})
                    {
                        $rxPauseOffWm{value} = "--";
                    }else
                    {
                        $rxPauseOffWm{value} = int($rxPauseOffWm{value}/1024);
                    }
    
                    printf("%2d | %4d %4d | %4s %4s | %4d %4d | %4d %4d %4d %4d %4d %4d %4d %4d\n",
                        $p, int($rxPrivWm{value}/1024), int($rxHogWm{value}/1024), 
                        $rxPauseOnWm{value}, $rxPauseOffWm{value},
                        int($txPrivWm{value}/1024), int($txHogWm{value}/1024),
                        int($txtcPrivWm[0]->{value}/1024), int($txtcPrivWm[1]->{value}/1024),
                        int($txtcPrivWm[2]->{value}/1024), int($txtcPrivWm[3]->{value}/1024),
                        int($txtcPrivWm[4]->{value}/1024), int($txtcPrivWm[5]->{value}/1024),
                        int($txtcPrivWm[6]->{value}/1024), int($txtcPrivWm[7]->{value}/1024));
                }
                $n++;
                $sw2++;
            }
        }
    }
}

1;
