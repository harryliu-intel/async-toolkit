# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/QCNCore.pm
# Creation Date:    4/7/2009
# Description:      FM4000 support for QCN TestPoint Interfaces
#
# INTEL CONFIDENTIAL
# Copyright 2009 - 2011 Intel Corporation. All Rights Reserved. 
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

package Applications::TestPoint::Common::FM4000::QCNCore;
use strict;
use warnings;

use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

our @EXPORT = qw(
    tp4000HandleAddQcnCpTrig
    tp4000HandleAddQcnRpTrig
    tp4000HandleDelQcnTrig
    tp4000HandleShowQcnTrig
    tp4000HandleSetQcnEgressQeueue
    tp4000HandleSetQcnRateLimiter
    tp4000HandleResetQcn
);

use constant FM4000_MAX_TC                  => 7;
use constant FM4000_MAX_SMP                 => 1;

# Keep track of triggers
my %triggers;

# Map SMP number to CM_SMP_MEMBERSHIP field value. There must be at least 
# FM4000_MAX_SMP entries.
my %smpMap =
(
    0 => 1,
    1 => 2,
);


##@cmethod public int tp4000HandleAddQcnCpTrig(int switchNum, int swpri, int port)
# @brief        Add a congestion point trigger
#
# @param[in]    switchNum is the switch number on which the trigger hardware
#               is located.
#
# @param[in]    swpri is the switch priority
#
# @param[in]    port is the CP port
#
# @return       $FM_OK
sub tp4000HandleAddQcnCpTrig
{
    my ($self, $switchNum, $swpri, $port) = @_;
    my $chip = $self->{CHIP};
    
    my $status;
    my $trigNumber;
    my $physPort;
    my $sw;
    
    $status = $chip->fmPlatformMapLogicalPortToPhysical($switchNum,
                                                        $port,
                                                        \$sw,
                                                        \$physPort);
    if ($status != $FM_OK)
    {
        printf("Unable to identify port $port.\n");
        printf("   %s\n", fmErrorMsg($status));
        return $FM_FAIL;
    }
    
    # Allocate a hardware trigger
    my $info = SDK::fm_triggerRequestInfo->new();
    $info->{'requestRateLimiter'} = $SDK::FALSE;

    $status = $chip->fmAllocateTrigger($switchNum, \$trigNumber, $info);
    
    if ($status != $FM_OK)
    {
        printf("Unable to allocate a hardware trigger resource.\n");
        printf("   %s\n", fmErrorMsg($status));
        return $FM_FAIL;
    }
    
    my $trig = {'type' => "CP", 'port' => $port, 'swpri' => $swpri};
    $triggers{$trigNumber} = $trig;
        
    # Program trigger hardware
    # Specification for these register values may be found in VantagePoint
    # Functional Specification 1.0.
    my $trigRegValue = 0;
    $trigRegValue |= (2 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchSA);
    $trigRegValue |= (2 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchDA);
    $trigRegValue |= (2 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchHitSA);
    $trigRegValue |= (2 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchHitDA);
    $trigRegValue |= (2 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchHitSADA);
    $trigRegValue |= (2 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchVlan);
    $trigRegValue |= (2 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchFFU);
    $trigRegValue |= (2 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchEtherType);
    $trigRegValue |= (2 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchDestGlort);
    $trigRegValue |= (1 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchSwitchPri);
    $trigRegValue |= (0 << $FM4000_TRIGGER_CONDITION_CFG_b_MatchByPrecedence);
    $trigRegValue |= (1 << $FM4000_TRIGGER_CONDITION_CFG_b_MatchRandomIfLess);
    $trigRegValue |= (16 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchRandomThreshold);
    
    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_TRIGGER_CONDITION_CFG($trigNumber), 
                         $trigRegValue);
    
    $trigRegValue = 0;
    $trigRegValue |= ($swpri << $FM4000_TRIGGER_CONDITION_PARAM_l_SwitchPri);
    $trigRegValue |= (1 << $FM4000_TRIGGER_CONDITION_PARAM_l_FrameClassMask);
    $trigRegValue |= (3 << $FM4000_TRIGGER_CONDITION_PARAM_l_RoutedMask);
    $trigRegValue |= (3 << $FM4000_TRIGGER_CONDITION_PARAM_l_FtypeMask);
    $trigRegValue |= (0 << $FM4000_TRIGGER_CONDITION_PARAM_b_MatchDroppedFrames);
    
    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_TRIGGER_CONDITION_PARAM($trigNumber), 
                         $trigRegValue);
    
    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_TRIGGER_CONDITION_RX($trigNumber), 
                         0x1FFFFFF);
    
    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_TRIGGER_CONDITION_TX($trigNumber), 
                         (1 << $physPort));
    
    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_TRIGGER_CONDITION_AMASK_1($trigNumber), 
                         0xFFFFFFFF);
    
    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_TRIGGER_CONDITION_AMASK_2($trigNumber), 
                         0x1FFF);
    
    $trigRegValue = 0;
    $trigRegValue |= (1 << $FM4000_TRIGGER_ACTION_CFG_1_b_MirroringAction);
    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_TRIGGER_ACTION_CFG_1($trigNumber), 
                         $trigRegValue);
    
    $trigRegValue = 0;
    $trigRegValue |= (1 << $FM4000_TRIGGER_ACTION_CFG_2_b_MirrorTruncate);
    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_TRIGGER_ACTION_CFG_2($trigNumber), 
                         $trigRegValue);
    
    $trigRegValue = 0xC000;
    $trigRegValue |= $physPort;
    $trigRegValue |= ($trigNumber << 5);
    $trigRegValue <<= $FM4000_TRIGGER_ACTION_MIRROR_l_MirrorGlort;
    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_TRIGGER_ACTION_MIRROR($trigNumber), 
                         $trigRegValue);
    
    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_TRIGGER_ACTION_DROP($trigNumber), 
                         0x00000000);
    
    # Assume TRIGGER_IM is all ones.
    
    $trigRegValue = 0;
    $trigRegValue |= (0x16 << $FM4000_TX_TRUNC_l_MirrorTruncationLen);
    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_TX_TRUNC($physPort), 
                         0x00000000);
    
    return $FM_OK;
}
    


##@cmethod public int tp4000HandleAddQcnRpTrig(int switchNum, int *portList)
# @brief        Add a reaction point trigger
#
# @param[in]    switchNum is the switch number on which the trigger hardware
#               is located.
#
# @param[in]    portList is a list of ports on the edge of the CND
#
# @return       $FM_OK
#
sub tp4000HandleAddQcnRpTrig
{
    my ($self, $switchNum, @portList) = @_;
    my $chip = $self->{CHIP};
    
    my $status;
    my $swpri;
    my $ethertype;
    my $macAddr = Math::BigInt->new(0);
    
    ($status, $macAddr, $swpri, $ethertype) = $self->tpPlatformGetQcnCnm();
    
    if ($ethertype == -1)
    {
        printf("Reaction point functionality is disabled. No trigger added.\n");
        return $FM_FAIL;
    }
    
    # Build the port mask
    my $destMask = 0x0;
    
    foreach my $port (@portList)
    {
        my $physPort;
        my $sw;
        
        $status = $chip->fmPlatformMapLogicalPortToPhysical($switchNum,
                                                            $port,
                                                            \$sw,
                                                            \$physPort);
        $destMask |= (1 << $physPort);
    }
    
    my $trigNumber;
    my $info = SDK::fm_triggerRequestInfo->new();
    $info->{'requestRateLimiter'} = $SDK::FALSE;

    # Allocate a hardware trigger
    $status = $chip->fmAllocateTrigger($switchNum, \$trigNumber, $info);
    
    if ($status != $FM_OK)
    {
        printf("Unable to allocate a hardware trigger resource.\n");
        printf("   %s\n", fmErrorMsg($status));
        return $FM_FAIL;
    }
    
    my $trig = {'type' => "RP", 'ports' => \@portList};
    $triggers{$trigNumber} = $trig;
    
    # Program trigger hardware
    # Specification for these register values may be found in VantagePoint
    # Functional Specification 1.0.
    my $trigRegValue = 0;
    $trigRegValue |= (2 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchDA);
    $trigRegValue |= (2 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchSA);
    $trigRegValue |= (2 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchHitSA);
    $trigRegValue |= (2 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchHitDA);
    $trigRegValue |= (2 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchHitSADA);
    $trigRegValue |= (2 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchVlan);
    $trigRegValue |= (2 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchFFU);
    $trigRegValue |= (2 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchSwitchPri);
    $trigRegValue |= (2 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchDestGlort);
    $trigRegValue |= (1 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchEtherType);
    $trigRegValue |= (0 << $FM4000_TRIGGER_CONDITION_CFG_b_MatchByPrecedence);
    $trigRegValue |= (1 << $FM4000_TRIGGER_CONDITION_CFG_b_MatchRandomIfLess);
    $trigRegValue |= (18 << $FM4000_TRIGGER_CONDITION_CFG_l_MatchRandomThreshold);

    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_TRIGGER_CONDITION_CFG($trigNumber), 
                         $trigRegValue);
    
    $trigRegValue = 0;
    $trigRegValue |= (1 << $FM4000_TRIGGER_CONDITION_PARAM_l_FrameClassMask);
    $trigRegValue |= (3 << $FM4000_TRIGGER_CONDITION_PARAM_l_RoutedMask);
    $trigRegValue |= (7 << $FM4000_TRIGGER_CONDITION_PARAM_l_FtypeMask);
    $trigRegValue |= (0 << $FM4000_TRIGGER_CONDITION_PARAM_b_MatchDroppedFrames);
    
    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_TRIGGER_CONDITION_PARAM($trigNumber), 
                         $trigRegValue);
    
    $trigRegValue = $ethertype << $FM4000_TRIGGER_CONDITION_TYPE_l_EtherType;
    $trigRegValue |= 0xffff << $FM4000_TRIGGER_CONDITION_TYPE_l_EtherTypeMask;
    
    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_TRIGGER_CONDITION_TYPE($trigNumber), 
                         $trigRegValue);
    
    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_TRIGGER_CONDITION_RX($trigNumber), 
                         0x1FFFFFF);
    
    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_TRIGGER_CONDITION_TX($trigNumber), 
                         $destMask);
    
    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_TRIGGER_CONDITION_AMASK_1($trigNumber), 
                         0xFFFFFFFF);
    
    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_TRIGGER_CONDITION_AMASK_2($trigNumber), 
                         0x1FFF);
    
    $trigRegValue = 0;
    $trigRegValue |= (2 << $FM4000_TRIGGER_ACTION_CFG_1_l_ForwardingAction);
    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_TRIGGER_ACTION_CFG_1($trigNumber), 
                         $trigRegValue);
    
    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_TRIGGER_ACTION_GLORT($trigNumber), 
                         0x00000000);
    
    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_TRIGGER_ACTION_DROP($trigNumber), 
                         0x00000000);
    
    # Assume TRIGGER_IM is all ones.
    
    return $FM_OK;
}
    

##@cmethod public int tp4000HandleDelQcnTrig(int switchNum, int trig)
# @brief        Delete a QCN trigger
#
# @param[in]    switchNum is the switch number on which the trigger hardware
#               is located.
#
# @param[in]    trig is the trigger number to delete, a range list or "all"
#
# @return       $FM_OK
#
sub tp4000HandleDelQcnTrig
{
    my ($self, $switchNum, $trig) = @_;
    my $chip = $self->{CHIP};
    
    my @affectedTriggers = ();
    my @trigList = $self->validateList($trig, 0, 63, $FALSE);
    
    if (scalar(@trigList) == 0)
    {
        printf("'$trig' does not contain valid trigger numbers.\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }
    
    
    foreach my $trigNumber (@trigList)
    {
        if (exists ($triggers{$trigNumber}))
        {
            push(@affectedTriggers, $trigNumber);
            
            my $trigger = $triggers{$trigNumber};
            printf("Deleting %s trigger number $trigNumber.\n", $trigger->{'type'});
            delete ($triggers{$trigNumber});
            
            my $status = $chip->fmFreeTrigger($switchNum, $trigNumber);
            
            if ($status != $FM_OK)
            {
                printf("Unable to deallocate hardware trigger resource.\n");
                printf("   %s\n", fmErrorMsg($status));
                return $FM_FAIL;
            }
        }
    }
    
    my @unknownTriggers  = ();
    my %count = ();
    my $trigNumber;
    
    foreach $trigNumber (@affectedTriggers)
    { 
        $count{$trigNumber}++;
    }
    
    foreach $trigNumber (@trigList) 
    {
        if (!defined($count{$trigNumber}))
        {
            push(@unknownTriggers, $trigNumber);
        }
    }
    
    
    if (scalar(@affectedTriggers) == 0)
    {
        printf("No triggers specified by '$trig' exist.\n");
        return $FM_FAIL;
    }
    elsif (scalar(@unknownTriggers) > 0 && ($trig ne "all"))
    {
        printf("Trigger numbers %s not found.\n", 
                $self->StringifyList(@unknownTriggers));
    }
    
    return $FM_OK;
}
    


##@cmethod public int tp4000HandleShowQcnTrig(void)
# @brief        Show all QCN triggers
#
# @return       $FM_OK
#
sub tp4000HandleShowQcnTrig
{
    my ($self) = @_;
    
    printf("Congestion Point Triggers:\n");
    printf("    %-9s %-6s  %-4s\n", "Trigger", "Sw PRI", "Port");
    
    foreach my $key (sort keys %triggers) 
    {
        my $trig = $triggers{$key};

        if ($trig->{'type'} eq "CP")
        {
            printf("    %-9s %-6s  %-4s\n", $key, $trig->{'swpri'}, $trig->{'port'});
        }
    }
    
    printf("\nReaction Point Triggers:\n");
    printf("    %-9s %-4s\n", "Trigger", "Ports");
    
    foreach my $key (sort keys %triggers) 
    {
        my $trig = $triggers{$key};

        if ($trig->{'type'} eq "RP")
        {
            my $ports = $self->StringifyList(@{$trig->{'ports'}});
            printf("    %-9s %-4s\n", $key, $ports);
        }
    }
    
    printf("\n");
    
    return $FM_OK;
}



##@cmethod public int tp4000HandleSetQcnEgressQeueue(int switchNum, int tc, int *ports)
# @brief        Add a reaction point trigger
#
# @param[in]    switchNum is the switch number on which the trigger hardware
#               is located.
#
# @param[in]    tc is the traffic class
#
# @param[in]    portList is a list of ports on the edge of the CND
#
# @return       $FM_OK
#
sub tp4000HandleSetQcnEgressQeueue
{
    my ($self, $switchNum, $tc, @portList) = @_;
    my $chip = $self->{CHIP};
    
    # Validate traffic class
    if ($tc > FM4000_MAX_TC)
    {
        printf("Invalid traffic class value $tc.\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }
    
    # Determine the SMP from the traffic class
    my $smpMembership;
    $chip->fmReadUINT32($switchNum, 
                        $chip->FM_CM_SMP_MEMBERSHIP(), 
                        \$smpMembership);
    
    my $tcSmp = $smpMembership >> ($tc * 4);
    $tcSmp &= 0x0f;
    
    my $smp;
    
    if ($tcSmp == 1)
    {
        $smp = 0;
    }
    elsif ($tcSmp == 2)
    {
        $smp = 1;
    }
    else
    {
        printf("Error: traffic class $tc is not assigned to an SMP.\n");
        return $FM_FAIL;
    }
    
    foreach my $port (@portList)
    {
        # Convert port to physical
        my $physPort;
        my $sw;
        my $status = $chip->fmPlatformMapLogicalPortToPhysical($switchNum,
                                                               $port,
                                                               \$sw,
                                                               \$physPort);
        if ($status != $FM_OK)
        {
            printf("Unable to identify port $port.\n");
            printf("   %s\n", fmErrorMsg($status));
            return $FM_FAIL;
        }
        
        # Program watermarks.
        # Specification for these register values may be found in VantagePoint
        # Functional Specification 1.0.
        $chip->fmWriteUINT32($switchNum, 
                             $chip->FM_CM_RX_SMP_PRIVATE_WM($physPort, $smp), 
                             0x00000000);
        
        $chip->fmWriteUINT32($switchNum, 
                             $chip->FM_CM_TX_TC_PRIVATE_WM($physPort, $tc), 
                             0x00000000);
        
        $chip->fmWriteUINT32($switchNum, 
                             $chip->FM_CM_TX_SMP_PRIVATE_WM($physPort, $smp), 
                             0x00000000);
        
        my $hogMap;
        $chip->fmReadUINT32($switchNum, 
                            $chip->FM_CM_TX_HOG_MAP(), 
                            \$hogMap);
        
        # Calculate mask from traffic class
        my $tcMask = 3 << ($tc * 2);
        $tcMask = ~$tcMask;
        $hogMap &= $tcMask;
        
        $chip->fmWriteUINT32($switchNum, 
                             $chip->FM_CM_TX_HOG_MAP(), 
                             $hogMap);
        
        # Get cpQLenMax
        my ($cpQSp, $cpQLenMax, $cpW, $cpSampleBase);
        ($status, $cpQSp, $cpQLenMax, $cpW, $cpSampleBase) = 
                                            $self->tpPlatformGetQcnCpConfig();
        my $hogWM = $cpQLenMax / 512;
        $chip->fmWriteUINT32($switchNum, 
                             $chip->FM_CM_TX_SMP_HOG_WM($physPort, 0), 
                             $hogWM);
    }
    
    return $FM_OK;
}
    

##@cmethod public int tp4000HandleSetQcnRateLimiter(int switchNum, int tc, int *ports)
# @brief        Setup the rate limiter on RP ports
#
# @param[in]    switchNum is the switch number on which the trigger hardware
#               is located.
#
# @param[in]    tc is the traffic class
#
# @param[in]    portList is a list of RP ports
#
# @return       $FM_OK
#
sub tp4000HandleSetQcnRateLimiter
{
    my ($self, $switchNum, @portList) = @_;
    my $chip = $self->{CHIP};
    
    # Get SMP membership
    my $smpMembership;
    $chip->fmReadUINT32($switchNum, 
                        $chip->FM_CM_SMP_MEMBERSHIP(), 
                        \$smpMembership);

    # Get max CNDs supported by platform
    my $maxCnd = $self->tpPlatformGetQcnCndMax();
    
    # Limit to the number supported by Bali
    if ($maxCnd > FM4000_MAX_SMP)
    {
        $maxCnd = FM4000_MAX_SMP;
    }
    
    # Set up decimator #0    
    my $pauseDecimation = 0;
    $pauseDecimation |= (0x0 << $FM4000_CM_PAUSE_DECIMATION_l_K);
    $pauseDecimation |= (0x9 << $FM4000_CM_PAUSE_DECIMATION_l_N);
    $pauseDecimation |= (0x3 << $FM4000_CM_PAUSE_DECIMATION_l_M);
    
    $chip->fmWriteUINT32($switchNum, 
                         $chip->FM_CM_PAUSE_DECIMATION(0), 
                         $pauseDecimation);

    my $rateLimCfg = 0;
    $rateLimCfg |= (1 << $FM4000_RX_RATE_LIM_CFG_l_Capacity);
    $rateLimCfg |= (0x56 << $FM4000_RX_RATE_LIM_CFG_l_RateFrac);
    $rateLimCfg |= (0x53 << $FM4000_RX_RATE_LIM_CFG_l_RateUnit);
    
    my $rateLimThresh = 0;
    $rateLimThresh |= (0 << $FM4000_RX_RATE_LIM_THRESHOLD_l_off);
    $rateLimThresh |= (0xffff << $FM4000_RX_RATE_LIM_THRESHOLD_l_drop);
    
    my $cmPortCfg = 0;
    $cmPortCfg |= (0 << $FM4000_CM_PORT_CFG_l_DECIMATOR);
    $cmPortCfg |= (1 << $FM4000_CM_PORT_CFG_b_PAUSE_TYPE);
    $cmPortCfg |= (0 << $FM4000_CM_PORT_CFG_b_SMP0_RL_PAUSE);
    $cmPortCfg |= (0 << $FM4000_CM_PORT_CFG_b_SMP1_RL_PAUSE);
    
    for (my $cnd = 0 ; $cnd <= $maxCnd ; $cnd++)
    {
        # Get TC used by CNP
        my ($status, $vpri, $tc) = $self->tpPlatformGetQcnCnp($cnd);
        
        $smpMembership &= ~(0x0f << ($tc * 4));
        $smpMembership |= $smpMap{$cnd} << ($tc * 4);
        
        $chip->fmWriteUINT32($switchNum, 
                             $chip->FM_CM_SMP_MEMBERSHIP(), 
                             $smpMembership);
        
        foreach my $port (@portList)
        {
            # Convert port to physical
            my $physPort;
            my $sw;
            my $status = $chip->fmPlatformMapLogicalPortToPhysical($switchNum,
                                                                   $port,
                                                                   \$sw,
                                                                   \$physPort);
            if ($status != $FM_OK)
            {
                printf("Unable to identify port $port.\n");
                printf("   %s\n", fmErrorMsg($status));
                return $FM_FAIL;
            }
            
            # Program rate limiter
            $chip->fmWriteUINT32($switchNum, 
                                 $chip->FM_RX_RATE_LIM_CFG($physPort, $tc), 
                                 $rateLimCfg);
            
            $chip->fmWriteUINT32($switchNum, 
                                 $chip->FM_RX_RATE_LIM_THRESHOLD($physPort, $tc), 
                                 $rateLimThresh);
            
            # Set per-port parameter only once, not for each CND
            if ($cnd == 0)
            {
                $chip->fmWriteUINT32($switchNum, 
                                     $chip->FM_CM_PORT_CFG($physPort), 
                                     $cmPortCfg);
            }
        }
    }

    return $FM_OK;
}
    

##@cmethod public int tp4000HandleResetQcn(void)
# @brief        Reset QCN parameters back to default
#
sub tp4000HandleResetQcn
{
    my ($self) = @_;
    
    # Disable the rate limiters
    # TODO: We need to remember which ports on which to disabe the rate limiter
    
    # TODO: Delete all triggers
    
    return $FM_OK;
}


1;
