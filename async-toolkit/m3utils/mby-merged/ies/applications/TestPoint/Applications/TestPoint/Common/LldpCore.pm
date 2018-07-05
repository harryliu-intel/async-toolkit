# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/LldpCore.pm
# Creation Date:    11/2/08
# Description:      
#
# INTEL CONFIDENTIAL
# Copyright 2008 - 2011 Intel Corporation. All Rights Reserved. 
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

package Applications::TestPoint::Common::LldpCore;
use strict;
use warnings;

use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

##@cmethod public int[] tpHandleShowLldpPort()
#
# @desc         Show LLDP port related information
#
# @return       None 
sub tpHandleShowLldpPort
{
    my ($self, $port, $attr) = @_;
    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());

    printf("\n");

    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print("Must specify a valid port number!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @indices = arrayFind(map {$self->tpPlatformIsCPUPort($_)} @globalPortList);
    if ($port ne "all" && scalar(@indices) > 0)
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($switchNum));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        foreach my $globalPort (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned sw since it should match switchNum and may actually be
            # wrong for some platforms.
            my ($sw, $logicalPort) = 
                        $self->tpPlatformMapGlobalToLogicalPort($globalPort);
            
            next if $self->tpPlatformIsCPUPort($globalPort);

            if (defined($attr) && $attr eq "local") 
            {
                $chip->fmLldpShowLocalData($globalPort);
            } 
            elsif (defined($attr) && $attr eq "remote") 
            {
                $chip->fmLldpShowRemoteData($globalPort);
            } 
            elsif (defined($attr) && $attr eq "config") 
            {
                $chip->fmLldpShowConfiguration($globalPort);
            } 
            elsif (defined($attr) && $attr eq "counters") 
            {
                $chip->fmLldpShowCounters($globalPort);
            }
            else
            {
                print("Must specify a valid data-set!\n");
                return $FM_ERR_INVALID_ARGUMENT;
            }
            
        }   # end foreach my $globalPort (@portList)
        
    }   # end foreach my $switchNum ($self->tpGetSwitches)
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    
    return $FM_OK;
}

##@cmethod public int[] tpHandleShowLldpLocal()
#
# @desc         Show LLDP local-data related information
#
# @return       None 
sub tpHandleShowLldpLocal
{
    my ($self) = @_;
    my $chip = $self->{CHIP};

    printf("\n");

    $chip->fmLldpShowLocalData(-1);
}

##@cmethod public int[] tpHandleShowLldpConfig()
#
# @desc         Show LLDP local-data related information
#
# @return       None 
sub tpHandleShowLldpConfig
{
    my ($self) = @_;
    my $chip = $self->{CHIP};

    printf("\n");

    $chip->fmLldpShowConfiguration(-1);
}

##@cmethod public int[] tpHandleStopLldp()
#
# @desc         Stop LLDP protocol
#
# @return       None 
sub tpHandleStopLldp
{
    my ($self) = @_;
    my $chip = $self->{CHIP};
    my $port = 'all';

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));

        if (scalar(@portList) == 0)
        {
            next;
        }

        $chip->fmSetPacketState($sw, $FM_PACKET_TYPE_LLDP, $FM_DISABLED);

        foreach my $globalPort (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($globalPort);

            next if $self->tpPlatformIsCPUPort($globalPort);

            $chip->fmLldpRemovePort($globalPort);
        }
    }

    # terminate LLDP protocol
    $chip->fmLldpTerminate();

}

##@cmethod public int[] tpHandleStartLldp(void *macaddr)
#
# @desc         Start LLDP protocol
#
# @return       None 
sub tpHandleStartLldp
{
    my ($self, $value) = @_;
    my $chip = $self->{CHIP};
    my $port = 'all';

    if (!defined($value)
        || $self->validateL2Address($value) != $FM_OK)
    {
        print($TP_MSG_ERR_MAC_INVALID_ADDRESS);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    # initialize LLDP protocol
    $chip->fmLldpInitialize($value);

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));

        if (scalar(@portList) == 0)
        {
            next;
        }

        $chip->fmSetPacketState($sw, $FM_PACKET_TYPE_LLDP, $FM_ENABLED);

        foreach my $globalPort (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($globalPort);

            next if $self->tpPlatformIsCPUPort($globalPort);

            $chip->fmLldpAddPort($switchNum, $logPort, $globalPort, 'port'.$globalPort, 'Monaco Port '.$switchNum.'/'.$logPort);
        }
    }
}

##@cmethod public int[] tpHandleSetLldpChassisId()
#
# @desc         Set local chassis id
#
# @return       None 
sub tpHandleSetLldpChassisId
{
    my ($self, $value) = @_;
    my $chip = $self->{CHIP};

    $chip->fmLldpSetChassisId(1, $value, length $value);

}

##@cmethod public int[] tpHandleSetLldpTiming()
#
# @desc         Set local timing parameters
#
# @return       None 
sub tpHandleSetLldpTiming
{
    my ($self, $tx_interval, $tx_hold, $reinit_delay, $tx_delay) = @_;
    my $chip = $self->{CHIP};

    if (!defined($tx_interval)  || !defined($tx_hold) ||
        !defined($reinit_delay) || !defined($tx_delay))
    {
        print("Must specify all four timing-parameters!\n");
    }
    else {
        $chip->fmLldpSetTxTiming($tx_interval, $tx_hold, $reinit_delay, $tx_delay);
    }
}

##@cmethod public int[] tpHandleSetLldpLogging()
#
# @desc         Set local timing parameters
#
# @return       None 
sub tpHandleSetLldpLogging
{
    my ($self, $log_level) = @_;
    my $chip = $self->{CHIP};

    if (!defined($log_level))
    {
        print("Must specify logging-level!\n");
    }
    elsif ($log_level eq 'none') {
        $chip->fmLldpSetLogLevel(0);
    }
    elsif ($log_level eq 'error') {
        $chip->fmLldpSetLogLevel(1);
    }
    elsif ($log_level eq 'warning') {
        $chip->fmLldpSetLogLevel(2);
    }
    elsif ($log_level eq 'info') {
        $chip->fmLldpSetLogLevel(3);
    }
    elsif ($log_level eq 'debug') {
        $chip->fmLldpSetLogLevel(4);
    }
    else
    {
        print("Must specify a valid logging-level!\n");
    }
}

##@cmethod public int[] tpHandleSetLldpSysName()
#
# @desc         Set local system name
#
# @return       None 
sub tpHandleSetLldpSysName
{
    my ($self, $value) = @_;
    my $chip = $self->{CHIP};

    $chip->fmLldpSetSystemName($value);

}

##@cmethod public int[] tpHandleSetLldpSysDesc()
#
# @desc         Set local system description
#
# @return       None 
sub tpHandleSetLldpSysDesc
{
    my ($self, $value) = @_;
    my $chip = $self->{CHIP};

    $chip->fmLldpSetSystemDescription($value);

}

##@cmethod public int[] tpHandleSetLldpPort()
#
# @desc         Set LLDP port parameters
#
# @return       None 
sub tpHandleSetLldpPort
{
    my ($self, $port, $attr, $value, $mode) = @_;
    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @indices = arrayFind(map {$self->tpPlatformIsCPUPort($_)} @globalPortList);
    if ($port ne "all" && scalar(@indices) > 0)
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($switchNum));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        foreach my $globalPort (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned sw since it should match switchNum and may actually be
            # wrong for some platforms.
            my ($sw, $logicalPort) = 
                        $self->tpPlatformMapGlobalToLogicalPort($globalPort);
            
            next if $self->tpPlatformIsCPUPort($globalPort);

            my $status;

            if ($attr eq "id")
            {
                $status = $chip->fmLldpSetPortId($globalPort, 1, $value, length $value);
            } 
            elsif ($attr eq "desc")
            {
                $status = $chip->fmLldpSetPortDesc($globalPort, $value);
            } 
            elsif ($attr eq "tx-enable")
            {
                if ($mode eq "on")
                {
                    if ($value eq "port-desc")
                    {
                        $status = $chip->fmLldpSetTLVsTxEnable($globalPort, 1, 1);
                    }
                    elsif ($value eq "sys-name")
                    {
                        $status = $chip->fmLldpSetTLVsTxEnable($globalPort, 2, 2);
                    }
                    elsif ($value eq "sys-desc")
                    {
                        $status = $chip->fmLldpSetTLVsTxEnable($globalPort, 4, 4);
                    }
                    elsif ($value eq "sys-cap")
                    {
                        $status = $chip->fmLldpSetTLVsTxEnable($globalPort, 8, 8);
                    }
                    elsif ($value eq "all")
                    {
                        $status = $chip->fmLldpSetTLVsTxEnable($globalPort, 15, 15);
                    }
                    else
                    {
                        print("Must specify a valid TLV type!\n");
                        return $FM_ERR_INVALID_ARGUMENT;
                    }
                }
                elsif ($mode eq "off")
                {
                   if ($value eq "port-desc")
                    {
                        $status = $chip->fmLldpSetTLVsTxEnable($globalPort, 1, 0);
                    }
                    elsif ($value eq "sys-name")
                    {
                        $status = $chip->fmLldpSetTLVsTxEnable($globalPort, 2, 0);
                    }
                    elsif ($value eq "sys-desc")
                    {
                        $status = $chip->fmLldpSetTLVsTxEnable($globalPort, 4, 0);
                    }
                    elsif ($value eq "sys-cap")
                    {
                        $status = $chip->fmLldpSetTLVsTxEnable($globalPort, 8, 0);
                    }
                    elsif ($value eq "all")
                    {
                        $status = $chip->fmLldpSetTLVsTxEnable($globalPort, 15, 0);
                    }
                    else
                    {
                        print("Must specify a valid TLV type!\n");
                        return $FM_ERR_INVALID_ARGUMENT;
                    }
                }
                else
                {
                    print("Must specify on or off!\n");
                    return $FM_ERR_INVALID_ARGUMENT;
                }
            } 
            elsif ($attr eq "admin")
            {
                if ($value eq "tx_only")
                {
                    $status = $chip->fmLldpSetPortAdminStatus($globalPort, 0);
                }
                elsif ($value eq "rx_only")
                {
                    $status = $chip->fmLldpSetPortAdminStatus($globalPort, 1);
                }
                elsif ($value eq "tx_and_rx")
                {
                    $status = $chip->fmLldpSetPortAdminStatus($globalPort, 2);
                }
                elsif ($value eq "disabled")
                {
                    $status = $chip->fmLldpSetPortAdminStatus($globalPort, 3);
                }
                else
                {
                    print("Must specify a valid admin state!\n");
                    return $FM_ERR_INVALID_ARGUMENT;
                }
            } 
            else
            {
                print("Must specify a valid parameter!\n");
                return $FM_ERR_INVALID_ARGUMENT;
            }

            if ($status != $FM_OK)
            {
                printf("Set operation failed!\n");
                return $FM_FAIL;
            }
            
        }   # end foreach my $globalPort (@portList)
        
    }   # end foreach my $switchNum ($self->tpGetSwitches)
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    
    return $FM_OK;
}

1;
