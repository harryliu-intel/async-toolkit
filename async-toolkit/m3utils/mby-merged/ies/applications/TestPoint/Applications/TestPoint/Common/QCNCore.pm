# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/QCNCore.pm
# Creation Date:    4/2/2009
# Description:      QCN TestPoint Interfaces
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

package Applications::TestPoint::Common::QCNCore;
use strict;
use warnings;

use SDKScalars;

#Loaded when chip family is detected
#use base qw(
#    Applications::TestPoint::Common::FM4000::QCNCore
#);

use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

our @EXPORT = qw(
    tpHandleSetQcnCnpVPRI
    tpHandleSetQcnCnpTC
    tpHandleShowQcnCnp
    tpHandleSetQcnCpRp
    tpHandleShowQcnCpParm
    tpHandleShowQcnCpStatus
    tpHandleShowQcnRpParm
    tpHandleShowQcnRpStatus
    tpHandleResetQcn
    tpHandleSetQcnCnmMac
    tpHandleSetQcnCnmSwpri
    tpHandleSetQcnCnmEthtype
    tpHandleShowQcnCnm
    tpHandleAddQcnCpTrig
    tpHandleAddQcnRpTrig
    tpHandleDelQcnTrig
    tpHandleShowQcnTrig
);

##@cmethod public int tpHandleSetQcnCnpVPRI(int cnd, int vpri)
# @brief        Set congestion notification priority VPRI
#
# @param[in]    cnd is the CND to set
#
# @param[in]    vpri is the VLAN priority to set the CNP to
#
sub tpHandleSetQcnCnpVPRI
{
    my ($self, $cnd, $vpri) = @_;
    
    if (!defined($cnd) || !defined($vpri))
    {
        return($FM_ERR_INVALID_ARGUMENT);
    }
    
    my $status = $self->tpPlatformSetQcnCnpVpri($cnd, $vpri);
    return $status;
}


##@cmethod public int tpHandleSetQcnCnpTC(int cnd, int vpri)
# @brief        Set congestion notification priority VPRI
#
# @param[in]    cnd is the CND to set
#
# @param[in]    tc is the traffic class to set the CNP to
#
sub tpHandleSetQcnCnpTC
{
    my ($self, $cnd, $tc) = @_;
    
    if (!defined($cnd) || !defined($tc))
    {
        return($FM_ERR_INVALID_ARGUMENT);
    }
    
    my $status = $self->tpPlatformSetQcnCnpTc($cnd, $tc);
    return $status;
}


##@cmethod public int tpHandleShowQcnCnp(int cnd)
# @brief        Show congestion notification priority parameters
#
# @param[in]    cnd is the CND to show
#
sub tpHandleShowQcnCnp
{
    my ($self, $cnd) = @_;
    
    if (!defined($cnd))
    {
        return($FM_ERR_INVALID_ARGUMENT);
    }
    
    my ($status, $vpri, $tc) = $self->tpPlatformGetQcnCnp($cnd);
    
    if ($status == $FM_OK)
    {
        printf("Congestion Notification Priority for CND $cnd: VPRI = $vpri, TC = $tc\n");
    }
    
    return $status;
}

##@cmethod public int tpHandleSetQcnCpRp(char *cmd, char *parm, int value)
# @brief        Set congestion point or reaction point parameter
#
# @param[in]    parm is the parameter to set
#
# @param[in]    val is the value to set
#
sub tpHandleSetQcnCpRp
{
    my ($self, $parm, $val) = @_;
    
    if (!defined($parm) || !defined($val))
    {
        return($FM_ERR_INVALID_ARGUMENT);
    }
    
    my $status = $self->tpPlatformSetQcnCpRp($parm, $val);
    return $status;
}


##@cmethod public int tpHandleShowQcnCpParm(void)
# @brief        Show congestion notification priority parameters
#
sub tpHandleShowQcnCpParm
{
    my ($self) = @_;
    
    my ($status, $cpQSp, $cpQLenMax, $cpW, $cpSampleBase) = 
                                            $self->tpPlatformGetQcnCpConfig();
    
    if ($status == $FM_OK)
    {
        printf("Congestion Point parameters:\n");
        printf("   cpQSp         $cpQSp\n");
        printf("   cpQLenMax     $cpQLenMax\n");
        printf("   cpW           $cpW\n");
        printf("   cpSampleBase  $cpSampleBase\n") unless $cpSampleBase == -1;
    }
    else
    {
        printf("Error: %s\n", fmErrorMsg($status));
    }
    
    return $status;
}

##@cmethod public int tpHandleShowQcnCpStatus(void)
# @brief        Show congestion notification priority parameters
#
sub tpHandleShowQcnCpStatus
{
    my ($self) = @_;
    
    my ($status, 
        $cpQLen, 
        $cpFb, 
        $cpEnqued, 
        $cpMaxFb) = $self->tpPlatformGetQcnCpStatus();
    
    if ($status == $FM_OK)
    {
        printf("Congestion Point status:\n");
        printf("   cpQLen        $cpQLen\n") unless $cpQLen == -1;
        printf("   cpFb          $cpFb\n") unless $cpFb == -1;
        printf("   cpEnqued      $cpEnqued\n") unless $cpEnqued == -1;
        printf("   cpMaxFb       $cpMaxFb\n") unless $cpMaxFb == -1;
    }
    else
    {
        printf("Error: %s\n", fmErrorMsg($status));
    }
    
    return $status;
}

##@cmethod public int tpHandleShowQcnRpParm(void)
# @brief        Show reaction point parameters
#
sub tpHandleShowQcnRpParm
{
    my ($self) = @_;
    
    my ($status,
        $active,
        $rpTimeReset,
        $rpByteReset,
        $rpThreshold,
        $rpMaxRate,
        $rpAiRate,
        $rpHaiRate,
        $rpGd,
        $rpMinDecFac,
        $rpMinRate,
        $rpEnabled) = $self->tpPlatformGetQcnRpConfig();
    
    if ($status == $FM_OK)
    {
        printf("Reaction Point parameters:\n");
        printf("   active        %s\n", $active ? "yes" : "no");
        printf("   rpAiRate      $rpAiRate\n");
        printf("   rpByteReset   $rpByteReset\n");
        printf("   rpEnabled     $rpEnabled\n")   unless $rpEnabled == -1;
        printf("   rpGd          $rpGd\n");
        printf("   rpHaiRate     $rpHaiRate\n");
        printf("   rpMaxRate     $rpMaxRate\n");
        printf("   rpMinDecFac   %f\n", $rpMinDecFac);
        printf("   rpMinRate     $rpMinRate\n");
        printf("   rpThreshold   $rpThreshold\n");
        printf("   rpTimeReset   $rpTimeReset\n");
    }
    else
    {
        printf("Error: %s\n", fmErrorMsg($status));
    }
    
    return $status;
}


##@cmethod public int tpHandleShowQcnRpStatus(void)
# @brief        Show reaction point parameters
#
sub tpHandleShowQcnRpStatus
{
    my ($self) = @_;
    my $chip = $self->{CHIP};
    
    my ($status,
        $rpState,
        $rpByteCount,
        $rpFb) = $self->tpPlatformGetQcnRpStatus();
    
    if ($status == $FM_OK)
    {
        printf("Reaction Point status:\n");
        printf("   rpState       $rpState\n")     unless $rpState == -1;
        printf("   rpByteCount   $rpByteCount\n") unless $rpState == -1;
        printf("   rpFb          $rpFb\n")        unless $rpState == -1;
    }
    else
    {
        printf("Error: %s\n", $chip->fmErrorMsg($status));
    }
    
    return $status;
}


##@cmethod public int tpHandleResetQcn(void)
# @brief        Reset QCN parameters back to default
#
sub tpHandleResetQcn
{
    my ($self) = @_;

    # First do platform-specific reset    
    my $status = $self->tpPlatformResetQcn();
    
    # Then do switch-specific reset
    if ($status == $FM_OK)
    {
        # We will pretend for now to iterate over selected switches, but in
        # fact we will only operate on the first one.    
        foreach my $switchNum ($self->tpGetSwitches)
        {
            for (($self->tpGetSwitchInfo)[$switchNum]->{'switchFamily'})
            {
                $_ == $FM_SWITCH_FAMILY_FM4000 && do
                {
                    $status = $self->tp4000HandleResetQcn();
                };
    
                do
                {
                    # No other switch families supported for now.
                };
            }
            
            # Bail out after first selected switch
            last;
            
        }
    }
    
    if ($status == $FM_OK)
    {
        printf("QCN parameters reset back to defaults.\n");
    }
    else
    {
        printf("Error: %s\n", fmErrorMsg($status));
    }
    
    return $status;
}


##@cmethod public int tpHandleSetQcnCnmMac(char *mac)
# @brief        Set congestion notification message SMAC
#
# @param[in]    mac is the SMAC or "auto" to get it from switch
#
sub tpHandleSetQcnCnmMac
{
    my ($self, $mac) = @_;
    my $chip = $self->{CHIP};
    
    if (!defined($mac))
    {
        return($FM_ERR_INVALID_ARGUMENT);
    }
    
    if ($mac eq "auto")
    {
        my %void;
        
        # Read it from the switch
        foreach my $switchNum ($self->tpGetSwitches)
        {
            %void = (type => 'fm_macaddr', value => 0);
            $chip->fmGetSwitchAttribute($switchNum, $FM_CPU_MAC, \%void);
            
            # Only supporting a single switch for now
            # use the first selected switch
            last;
        }
        
        $mac = $void{value};
    }
    else
    {
        if ($self->validateL2Address($mac) != $FM_OK)
        {
            print("$mac is not a valid MAC address!\n");
            return($FM_ERR_INVALID_ARGUMENT);
        }
    }
    
    my @macBytes = split /:/, $mac;
    my $compactMac = join "", ("0x", @macBytes);
    my $macAddr = Math::BigInt->new($compactMac);
    my $status = $self->tpPlatformSetQcnCnmMac($macAddr);
    return $status;
    
    
}


##@cmethod public int tpHandleSetQcnCnmSwpri(int swpri)
# @brief        Set congestion notification message switch priority
#
# @param[in]    swpri is the switch priority to use
#
sub tpHandleSetQcnCnmSwpri
{
    my ($self, $swpri) = @_;
    
    if (!defined($swpri))
    {
        return($FM_ERR_INVALID_ARGUMENT);
    }
    
    if ($swpri >= $FM_SWITCH_PRIORITY_MAX)
    {
        printf("Switch priority must be between 0 and %d.\n",
                $FM_SWITCH_PRIORITY_MAX - 1);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    
    my $status = $self->tpPlatformSetQcnCnmSwpri($swpri);
    return $status;
}


##@cmethod public int tpHandleSetQcnCnmEthtype(int ethertype)
# @brief        Set congestion notification message switch priority
#
# @param[in]    ethertype is the EtherType to look for
#
sub tpHandleSetQcnCnmEthtype
{
    my ($self, $ethertype) = @_;
    
    if (!defined($ethertype))
    {
        return($FM_ERR_INVALID_ARGUMENT);
    }
    
    my $etype = $ethertype;
    $etype =~ tr/0-9a-fA-F//cd;
    
    if (length($etype) != 4)
    {
        print("Input must be 4 hex digits\n");
        return($FM_ERR_INVALID_ARGUMENT);
    }
    
    $etype = "0x" . $etype;
    $etype = $self->str2intnum($etype);
    
    my $status = $self->tpPlatformSetQcnCnmEthtype($etype);
    return $status;
}


##@cmethod public int tpHandleShowQcnCnm(void)
# @brief        Show congestion notification message parameters
#
sub tpHandleShowQcnCnm
{
    my ($self) = @_;
    
    my ($status, $swpri, $ethertype);
    my $macAddr = Math::BigInt->new(0);

    ($status, $macAddr, $swpri, $ethertype) = $self->tpPlatformGetQcnCnm();
    
    my $hexEtherType = sprintf "0x%04X", $ethertype;
    
    my $macAddrStr = join(':', map {
        sprintf("%02x", $macAddr->copy()->brsft(8 * $_)->band(0xff)->as_number());
    } reverse(0 .. 5));

    if ($status == $FM_OK)
    {
        printf("Congestion Notification Message parameters:\n");
        printf("   SMAC:            $macAddrStr\n");
        printf("   Switch Priority: $swpri\n");
        printf("   EtherType:       $hexEtherType\n");
        printf("\n");
    }
    
    return $status;
}


##@cmethod public int tpHandleAddQcnCpTrig(int swpri, int port)
# @brief        Add a congestion point trigger
#
# @param[in]    swpri is the switch priority
#
# @param[in]    port is the CP port
#
sub tpHandleAddQcnCpTrig
{
    my ($self, $swpri, $port) = @_;
    
    if (!defined($swpri) || !defined($port))
    {
        return($FM_ERR_INVALID_ARGUMENT);
    }
    
    my $status = $FM_OK;
    
    # We will pretend for now to iterate over selected switches, but in
    # fact we will only operate on the first one.    
    foreach my $switchNum ($self->tpGetSwitches)
    {
        for (($self->tpGetSwitchInfo)[$switchNum]->{'switchFamily'})
        {
            $_ == $FM_SWITCH_FAMILY_FM4000 && do
            {
                $status = $self->tp4000HandleAddQcnCpTrig($switchNum,
                                                          $swpri,
                                                          $port);
            };

            do
            {
                # No other switch families supported for now.
            };
        }
        
        # Bail out after first selected switch
        last;
        
    }
    
    return $status;
}
    


##@cmethod public int tpHandleAddQcnRpTrig(int port)
# @brief        Add a reaction point trigger
#
# @param[in]    port is a list of ports on the edge of the CND
#
sub tpHandleAddQcnRpTrig
{
    my ($self, $port) = @_;
    
    if (!defined($port))
    {
        return($FM_ERR_INVALID_ARGUMENT);
    }
    
    my $status = $FM_OK;
    
    # We will pretend for now to iterate over selected switches, but in
    # fact we will only operate on the first one.    
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($FALSE, 
                                                   $port, 
                                    $self->tpPlatformGetSwitchPortList($switchNum));
        
        if (scalar(@portList) == 0)
        {
            printf("Invalid port list %s.\n", $port);
            return($FM_ERR_INVALID_ARGUMENT);
        }
    
        for (($self->tpGetSwitchInfo)[$switchNum]->{'switchFamily'})
        {
            $_ == $FM_SWITCH_FAMILY_FM4000 && do
            {
                $status = $self->tp4000HandleAddQcnRpTrig($switchNum, @portList);
            };

            do
            {
                # No other switch families supported for now.
            };
        }
        
        # Bail out after first selected switch
        last;
        
    }
    
    return $status;
}
    

##@cmethod public int tpHandleDelQcnTrig(int trig)
# @brief        Delete a QCN trigger
#
# @param[in]    trig is the trigger number to delete
#
sub tpHandleDelQcnTrig
{
    my ($self, $trig) = @_;
    if (!defined($trig))
    {
        return($FM_ERR_INVALID_ARGUMENT);
    }
    
    my $status = $FM_OK;
    
    # We will pretend for now to iterate over selected switches, but in
    # fact we will only operate on the first one.    
    foreach my $switchNum ($self->tpGetSwitches)
    {
        for (($self->tpGetSwitchInfo)[$switchNum]->{'switchFamily'})
        {
            $_ == $FM_SWITCH_FAMILY_FM4000 && do
            {
                $status = $self->tp4000HandleDelQcnTrig($switchNum, $trig);
            };

            do
            {
                # No other switch families supported for now.
            };
        }
        
        # Bail out after first selected switch
        last;
        
    }
    
    return $status;
}
    
##@cmethod public int tpHandleSetQcnEgressQeueue(int port, int tc)
# @brief        Set QCN egress queue watermarks
#
# @param[in]    port is a port or list/range of ports
#
# @param[in]    tc is the traffic class
#
sub tpHandleSetQcnEgressQeueue
{
    my ($self, $port, $tc) = @_;
    
    if (!defined($port) || !defined($tc))
    {
        return($FM_ERR_INVALID_ARGUMENT);
    }
    
    my $status = $FM_OK;
    
    # We will pretend for now to iterate over selected switches, but in
    # fact we will only operate on the first one.    
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($FALSE, 
                                                   $port, 
                                    $self->tpPlatformGetSwitchPortList($switchNum));
        
        if (scalar(@portList) == 0)
        {
            printf("Invalid port list %s.\n", $port);
            return($FM_ERR_INVALID_ARGUMENT);
        }
    
        for (($self->tpGetSwitchInfo)[$switchNum]->{'switchFamily'})
        {
            $_ == $FM_SWITCH_FAMILY_FM4000 && do
            {
                $status = $self->tp4000HandleSetQcnEgressQeueue($switchNum,
                                                                $tc,
                                                                @portList);
            };

            do
            {
                # No other switch families supported for now.
            };
        }
        
        # Bail out after first selected switch
        last;
        
    }
    
    return $status;
}
    
##@cmethod public int tpHandleSetQcnRateLimiter(char *ports)
# @brief        Show congestion notification message parameters
#
# @param[in]    ports is a list of RP ports
#
sub tpHandleSetQcnRateLimiter
{
    my ($self, $ports) = @_;
    
    if (!defined($ports))
    {
        return($FM_ERR_INVALID_ARGUMENT);
    }
    
    my $status = $FM_OK;
    
    # We will pretend for now to iterate over selected switches, but in
    # fact we will only operate on the first one.    
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($FALSE, 
                                                   $ports, 
                                    $self->tpPlatformGetSwitchPortList($switchNum));
        
        if (scalar(@portList) == 0)
        {
            printf("Invalid port list %s.\n", $ports);
            return($FM_ERR_INVALID_ARGUMENT);
        }
    
        for (($self->tpGetSwitchInfo)[$switchNum]->{'switchFamily'})
        {
            $_ == $FM_SWITCH_FAMILY_FM4000 && do
            {
                $status = $self->tp4000HandleSetQcnRateLimiter($switchNum, 
                                                               @portList);
            };

            do
            {
                # No other switch families supported for now.
            };
        }
        
        # Bail out after first selected switch
        last;
        
    }
    
    return $status;
}



##@cmethod public int tpHandleShowQcnTrig(void)
# @brief        Show all QCN triggers
#
sub tpHandleShowQcnTrig
{
    my ($self) = @_;
    my $status = $FM_OK;
    
    # We will pretend for now to iterate over selected switches, but in
    # fact we will only operate on the first one.    
    foreach my $switchNum ($self->tpGetSwitches)
    {
        for (($self->tpGetSwitchInfo)[$switchNum]->{'switchFamily'})
        {
            $_ == $FM_SWITCH_FAMILY_FM4000 && do
            {
                $status = $self->tp4000HandleShowQcnTrig();
            };

            do
            {
                # No other switch families supported for now.
            };
        }
        
        # Bail out after first selected switch
        last;
        
    }
    
    return $status;
}



1;
