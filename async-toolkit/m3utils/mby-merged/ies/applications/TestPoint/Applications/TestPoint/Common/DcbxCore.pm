# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/DcbxCore.pm
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

package Applications::TestPoint::Common::DcbxCore;
use strict;
use warnings;

use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

##@cmethod public int[] create_intArray()
#
# @desc         Convert perl array to C array
#
# @return       C array 
sub create_intArray
{
    my $i;
    my $len = scalar(@_);
    my $ary = new_intArray($len);

    for ($i = 0; $i < $len; $i++) {
        my $val = shift;
        intArray_setitem($ary, $i, $val);
    }

    return $ary;
}

##@cmethod public int[] create_boolArray()
#
# @desc         Convert perl array to C array
#
# @return       C array 
sub create_boolArray
{
    my $i;
    my $len = scalar(@_);
    my $ary = new_boolArray($len);

    for ($i = 0; $i < $len; $i++) {
        my $val = shift;
        boolArray_setitem($ary, $i, $val);
    }

    return $ary;
}

##@cmethod public int[] tpHandleShowDcbxPort()
#
# @desc         Show DCBX related information
#
# @return       None 
sub tpHandleShowDcbxPort
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
                $chip->fmDcbxShowLocalData($globalPort);
            } 
            elsif (defined($attr) && $attr eq "admin") 
            {
                $chip->fmDcbxShowAdminData($globalPort);
            }
            elsif (defined($attr) && $attr eq "remote") 
            {
                $chip->fmDcbxShowRemoteData($globalPort);
            } 
            elsif (defined($attr) && $attr eq "config") 
            {
                $chip->fmDcbxShowConfiguration($globalPort);
            } 
            elsif (defined($attr) && $attr eq "counters") 
            {
                $chip->fmDcbxShowCounters($globalPort);
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

##@cmethod public int[] tpHandleStopDcbx()
#
# @desc         Stop DCBX protocol
#
# @return       None 
sub tpHandleStopDcbx
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

        foreach my $globalPort (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($globalPort);

            next if $self->tpPlatformIsCPUPort($globalPort);

            $chip->fmDcbxRemovePort($globalPort);
        }
    }

    # terminate DCBX protocol
    $chip->fmDcbxTerminate();
}

##@cmethod public int[] tpHandleStartDcbx(void *macaddr)
#
# @desc         Start DCBX protocol
#
# @return       None 
sub tpHandleStartDcbx
{
    my ($self) = @_;
    my $chip = $self->{CHIP};
    my $port = 'all';

    # initialize DCBX protocol
    $chip->fmDcbxInitialize();

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));

        if (scalar(@portList) == 0)
        {
            next;
        }

        foreach my $globalPort (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($globalPort);

            next if $self->tpPlatformIsCPUPort($globalPort);

            $chip->fmDcbxAddPort($globalPort);
        }
    }
}

##@cmethod public int[] tpHandleSetDcbxProtocolVersion()
#
# @desc         Set DCBX port tx-enable
#
# @return       None 
sub tpHandleSetDcbxProtocolVersion
{
    my ($self, $globalPort, $vers) = @_;
    my $chip = $self->{CHIP};

    my $mask;
    my $status = $FM_OK;

    if (!defined($vers)) {
        print("Missing arguments!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if ($vers eq 'auto')
    {
        $status = $chip->fmDcbxSetProtocolVersion($globalPort, -1);
    }
    elsif ($vers == 0 || $vers == 1)
    {
        $status = $chip->fmDcbxSetProtocolVersion($globalPort, $vers);
    }
    else
    {
        print("Unknown protocol version!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    return $status;
}

##@cmethod public int[] tpHandleSetDcbxPortTxEnable()
#
# @desc         Set DCBX port tx-enable
#
# @return       None 
sub tpHandleSetDcbxPortTxEnable
{
    my ($self, $globalPort, $comp, $mode) = @_;
    my $chip = $self->{CHIP};

    my $mask;
    my $status = $FM_OK;

    if (!defined($comp) || !defined($mode)) {
        print("Missing arguments!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if ($comp eq "pfc")
    {
        $mask = 1;
    }
    elsif ($comp eq "ets-conf")
    {
        $mask = 2;
    }
    elsif ($comp eq "ets-reco")
    {
        $mask = 4;
    }
    elsif ($comp eq "app")
    {
        $mask = 8;
    }
    elsif ($comp eq "cn")
    {
        $mask = 8;
    }
    elsif ($comp eq "all")
    {
        $mask = 31;
    }
    else
    {
        print("Must specify a valid TLV type!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if ($mode eq "on")
    {
        $status = $chip->fmDcbxSetTLVsTxEnable($globalPort, $mask, $mask);
    }
    elsif ($mode eq "off")
    {
        $status = $chip->fmDcbxSetTLVsTxEnable($globalPort, $mask, 0);
    }
    else
    {
        print("Must specify on or off!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    return $status;
}

##@cmethod public int[] tpHandleSetDcbxPortPfc()
#
# @desc         Set DCBX port parameters
#
# @return       None 
sub tpHandleSetDcbxPortPfc
{
    my ($self, $globalPort, $attr, $value) = @_;
    my $chip = $self->{CHIP};

    my $status = $FM_OK;

    if (!defined($attr) || !defined($value)) {
        print("Missing arguments!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    $value =~ s/on/1/g;
    $value =~ s/off/0/g;

    my @vec = split(/,/, $value);

    if ($attr eq "willing")
    {
        $status = fmDcbxSetAdminPfcWilling($globalPort, $value);
    }
    elsif ($attr eq "mbc")
    {
        $status = fmDcbxSetAdminPfcMBC($globalPort, $value);
    }
    elsif ($attr eq "capability")
    {
        $status = fmDcbxSetAdminPfcCap($globalPort, $value);
    }
    elsif ($attr eq "enable")
    {
        if (scalar(@vec) == 8)
        {
            #my $ary = create_boolArray(@vec);
            $status = fmDcbxSetAdminPfcEnable($globalPort, $vec[0], $vec[1], $vec[2], $vec[3], $vec[4], $vec[5], $vec[6], $vec[7]);
            #delete_boolArray($ary);
        }
        else
        {
            print("Invalid arguments!\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }
    else
    {
        print("Must specify a valid attribute!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    return $status;
}

##@cmethod public int[] tpHandleSetDcbxPortEtsConfiguration()
#
# @desc         Set DCBX port parameters
#
# @return       None 
sub tpHandleSetDcbxPortEtsConfiguration
{
    my ($self, $globalPort, $attr, $value) = @_;
    my $chip = $self->{CHIP};

    my $status = $FM_OK;

    if (!defined($attr) || !defined($value)) {
        print("Missing arguments!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    $value =~ s/on/1/g;
    $value =~ s/off/0/g;

    my @vec = split(/,/, $value);

    if ($attr eq "willing")
    {
        $status = fmDcbxSetAdminEtsConfigurationWilling($globalPort, $value);
    }
    elsif ($attr eq "cbs")
    {
        $status = fmDcbxSetAdminEtsConfigurationCreditBasedShaperSupport($globalPort, $value);
    }
    elsif ($attr eq "max-tc")
    {
        $status = fmDcbxSetAdminEtsConfigurationTrafficClassesSupported($globalPort, $value);
    }
    elsif ($attr eq "bandwidth")
    {
        if (scalar(@vec) == 8) 
        {
            #my $ary = create_intArray(@vec);
            $status = fmDcbxSetAdminEtsConfigurationTrafficClassBandwidth($globalPort, $vec[0], $vec[1], $vec[2], $vec[3], $vec[4], $vec[5], $vec[6], $vec[7]);
            #delete_intArray($ary);
        }
        else 
        {
            print("Invalid arguments!\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }
    elsif ($attr eq "algorithm")
    {
        if (scalar(@vec) == 8)
        {
            #my $ary = create_intArray(@vec);
            $status = fmDcbxSetAdminEtsConfigurationTrafficSelectionAlgorithm($globalPort, $vec[0], $vec[1], $vec[2], $vec[3], $vec[4], $vec[5], $vec[6], $vec[7]);
            #delete_intArray($ary);
        }
        else 
        {
            print("Invalid arguments!\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }
    elsif ($attr eq "priority")
    {
        if (scalar(@vec) == 8)
        {
            #my $ary = create_intArray(@vec);
            $status = fmDcbxSetAdminEtsConfigurationPriorityAssignment($globalPort, $vec[0], $vec[1], $vec[2], $vec[3], $vec[4], $vec[5], $vec[6], $vec[7]);
            #delete_intArray($ary);
        }
        else
        { 
            print("Invalid arguments!\n");
            return $FM_ERR_INVALID_ARGUMENT; 
        }
    }
    else
    {
        print("Must specify a valid attribute!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    return $status;
}

##@cmethod public int[] tpHandleSetDcbxPortEtsRecommendation()
#
# @desc         Set DCBX port parameters
#
# @return       None 
sub tpHandleSetDcbxPortEtsRecommendation
{
    my ($self, $globalPort, $attr, $value) = @_;
    my $chip = $self->{CHIP};

    my $status = $FM_OK;

    if (!defined($attr) || !defined($value)) {
        print("Missing arguments!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    $value =~ s/on/1/g;
    $value =~ s/off/0/g;

    my @vec = split(/,/, $value);

    if ($attr eq "bandwidth")
    {
        #my $ary = create_intArray(split(/,/, $value));
        $status = fmDcbxSetAdminEtsRecommendationTrafficClassBandwidth($globalPort, $vec[0], $vec[1], $vec[2], $vec[3], $vec[4], $vec[5], $vec[6], $vec[7]);
        #delete_intArray($ary);
    }
    elsif ($attr eq "algorithm")
    {
        #my $ary = create_intArray(split(/,/, $value));
        $status = fmDcbxSetAdminEtsRecommendationTrafficSelectionAlgorithm($globalPort, $vec[0], $vec[1], $vec[2], $vec[3], $vec[4], $vec[5], $vec[6], $vec[7]);
        #delete_intArray($ary);
    }
    elsif ($attr eq "priority")
    {
        #my $ary = create_intArray(split(/,/, $value));
        $status = fmDcbxSetAdminEtsRecommendationPriorityAssignment($globalPort, $vec[0], $vec[1], $vec[2], $vec[3], $vec[4], $vec[5], $vec[6], $vec[7]);
        #delete_intArray($ary);
    }
    else
    {
        print("Must specify a valid attribute!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    return $status;
}

##@cmethod public int[] tpHandleSetDcbxPortApp()
#
# @desc         Set DCBX port parameters
#
# @return       None 
sub tpHandleSetDcbxPortApp
{
    my ($self, $globalPort, $attr, $value, $aux) = @_;
    my $chip = $self->{CHIP};

    my $status = $FM_OK;

    if (!defined($attr) || !defined($value)) {
        print("Missing arguments!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    $value =~ s/on/1/g;
    $value =~ s/off/0/g;

    if ($attr eq "willing")
    {
        $status = fmDcbxSetAdminApplicationPriorityWilling($globalPort, $value);
    }
    else
    {
        if ($aux eq "delete")
        {
            $status = fmDcbxSetAdminApplicationPriorityRemove($globalPort, $attr, $value);
        }
        else
        {
            $status = fmDcbxSetAdminApplicationPriorityAddMod($globalPort, $attr, $value, $aux);
        }
    }

    return $status;
}

##@cmethod public int[] tpHandleSetDcbxPortCn()
#
# @desc         Set DCBX port parameters
#
# @return       None 
sub tpHandleSetDcbxPortCn
{
    my ($self, $globalPort, $attr, $value) = @_;
    my $chip = $self->{CHIP};

    my $status = $FM_OK;

    if (!defined($attr) || !defined($value)) {
        print("Missing arguments!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    $value =~ s/on/1/g;
    $value =~ s/off/0/g;

    my @vec = split(/,/, $value);

    if ($attr eq "supported")
    {
        #my $ary = create_boolArray(split(/,/, $value));
        $status = fmDcbxSetAdminCongestionNotificationCnpvSupported($globalPort, $vec[0], $vec[1], $vec[2], $vec[3], $vec[4], $vec[5], $vec[6], $vec[7]);
        #delete_boolArray($ary);
    }
    elsif ($attr eq "ready")
    {
        #my $ary = create_boolArray(split(/,/, $value));
        $status = fmDcbxSetAdminCongestionNotificationCnpvReady($globalPort, $vec[0], $vec[1], $vec[2], $vec[3], $vec[4], $vec[5], $vec[6], $vec[7]);
        #delete_boolArray($ary);
    }
    else
    {
        print("Must specify a valid attribute!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    return $status;
}

##@cmethod public int[] tpHandleSetDcbxPort()
#
# @desc         Set DCBX port parameters
#
# @return       None 
sub tpHandleSetDcbxPort
{
    my ($self, $port, $comp, @args) = @_;
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

            if (!defined($comp)) {
                print("Missing arguments!\n");
                return $FM_ERR_INVALID_ARGUMENT;
            }
  
            if ($comp eq "version")
            {
                $status = tpHandleSetDcbxProtocolVersion($self, $globalPort, @args);
            }
            elsif ($comp eq "tx-enable")
            {
                $status = tpHandleSetDcbxPortTxEnable($self, $globalPort, @args);
            }
            elsif ($comp eq "pfc")
            {
                $status = tpHandleSetDcbxPortPfc($self, $globalPort, @args);
            }
            elsif ($comp eq "ets-conf")
            {
                $status = tpHandleSetDcbxPortEtsConfiguration($self, $globalPort, @args);
            }
            elsif ($comp eq "ets-reco")
            {
                $status = tpHandleSetDcbxPortEtsRecommendation($self, $globalPort, @args);
            }
            elsif ($comp eq "app")
            {
                $status = tpHandleSetDcbxPortApp($self, $globalPort, @args);
            }
            elsif ($comp eq "cn")
            {
                $status = tpHandleSetDcbxPortCn($self, $globalPort, @args);
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
