# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FM4000/LoadBalancingCore.pm
# Creation Date:    11/29/07
# Description:      Load Balancing TestPoint Features
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
# 
# TestPoint Load Balancing Commands Supported:
#
# set options
#
# - enable
#
# - portlist
#
# - distribution
#    - uniform
#    - custom
#
# - match
#    - dip
#
# show options
#
# - distribution
###############################################################################

package Applications::TestPoint::Common::FM4000::LoadBalancingCore;
use strict;
use warnings;

use base qw();

use List::Util qw(sum);
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;
use Math::BigInt;



### Internal Variables ###

# Load balancing internal variables
my %lbParams = 
(
    # Load balancing parameters
    # ports to load balance over 
    # (represented in logical/faceplate port numbers)
    'portList' => undef,        # to be initialized
    # 
    # load distribution
    # Each entry corresponds to one load balancing bin and points
    # to one of the load balancing ports.
    # Entries are used to index into portList, valid ranges are
    # from 0 to (length portList -1)
    # 0 corresponds to first logical/faceplate port in portList
    # number of entries must equal arpCount * glortDestCount
    'distribution' => undef,    # to be initialized
    # FFU resources
    'slice' => 16,
    'case'  => 0,
    'tcamEntry' => 511,

    # Policer counter resources used for diagnostics
    'counterBank' => 0,
    'counterIndex' => 1,    # together with index+1 (for misses)

    # ARP resources
    'arpIndex' => 1024,     # ARP base index
    'arpCount' => 16,       # number of arp entries used

    # Glort resources
    'baseGlort' => 0x1000,  # individual load balancing glorts will be
                            # computed as offsets from the baseGlort
    'glortDestCount' => 16, # amount of spreading of each ECMP entry
                            # determines DestCount field in the 
                            # GLORT_DEST_TABLE
    'GDTBaseIndex' => 3000, # base index into GLORT_DEST_TABLE to start
                            # allocation from
    # index into GLORT_CAM/RAM that load balancing should use,
    # must not conflict with already used GLORT_CAM/RAM entries
    'glortCAMRAMIndex' => 100,
    # GLORT_RAM settings
    'rangeSubIndexAPos' => 0,
    'rangeSubIndexANumBits' => 4, # must be >= log2(arpCount)

    # IP_MULTICAST_TABLE resources
    # Used to match the ingress vlan to trick frame into egressing unmodified
    # (must have only 1 ingress vlan for load balancing for this to work)
    'mtableIndex'   => 1024,  # 25 entries counting from this one is used
    'ingressVlan'   => 1,     # must match the ingress frame

);

### Interfaces ###

##@cmethod public void tpFM4000HandleShowLoadBalancing
#
# @brief        show load balancing options
#
sub tpFM4000HandleShowLoadBalancing
{
    my ($self, @args) = @_;

    if (scalar @args == 0)
    {
        print "invalid number of arguments\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $cmd = shift @args;

    if (!defined $cmd)
    {
        print "argument to set loadbalancing missing\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }
    elsif ($cmd eq "distribution")
    {
        return $self->ShowDistribution(@args);
    }

    print "Unrecognized show loadbalancing command <$cmd>\n";
    return $FM_ERR_INVALID_ARGUMENT;
}

##@method tpFM4000HandleSetLoadBalancing
# 
# @brief    main handler, will fork off calls to individual helper functions
#
sub tpFM4000HandleSetLoadBalancing
{
    my ($self, @args) = @_;

    if (scalar @args == 0)
    {
        print "invalid number of arguments\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $cmd = shift @args;

    if (!defined $cmd)
    {
        print "argument to set loadbalancing missing\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }
    elsif ($cmd eq "enable")
    {
        return $self->EnableLoadBalancing(@args);
    }
    elsif ($cmd eq "match")
    {
        return $self->AddLBDIPRule(@args);
    }
    elsif ($cmd eq "distribution")
    {
        return $self->SetDistribution(@args);
    }
    elsif ($cmd eq "portlist")
    {
        return $self->SetPortList(@args);
    }
   
    print "Unrecognized loadbalancing command <$cmd>\n"; 
    return $FM_ERR_INVALID_ARGUMENT;
}


##@cmethod private void tpFM4000EnableLoadBalancing(void)
#
# @brief        Enable load balancing with the configured settings.
#               Need to set arpCount and glortDestCount before calling
#               this function.
#
sub EnableLoadBalancing
{
    my ($self) = @_;

    print "enabling load balancing\n"; 

    my $resp = $self->ConfigureFFU;
    if ($resp != $FM_OK)
    {
        print "ERROR calling ConfigureFFU\n";
        return $resp;
    }

    $resp = $self->ConfigureARPGLORTIPMT;
    if ($resp != $FM_OK)
    {
        print "ERROR calling ConfigureARPGLORTIPMT\n";
        return $resp;
    }

    return $FM_OK;

}

##@cmethod private void tpFM4000ConfigureFFULoadBalancing(void)
#
# @brief        Configure FFU for load balancing.
#               Set the slice configuration for the selected scenarios.
#
sub ConfigureFFU
{
    my ($self) = @_;
    my $chip = $self->{CHIP};

    my $resp = $FM_OK;

    # check that slice in use is active
    my $ffuInitSlice = $self->PerformRead(0, 'FM_FFU_INIT_SLICE', $chip->FM_FFU_INIT_SLICE);
    my $activeSlices = $ffuInitSlice & 0x3f;
    if ($lbParams{'slice'} >= $activeSlices) 
    { 
        print "Selected load balancing slice is not active";
        return $FM_FAIL;
    }

    # compute the expected scenarioMask for load balancing
    my $ipv4SwitchedScenario =
        1           #Frame format = IPv4
        | (0 << 2); #Frame handling = switched
    my $scenarioMask = 
        (1 << $ipv4SwitchedScenario);
    
    # enable the selected slice for the expected scenario
    print "configuring SLICE_VALID\n";
    $resp = $self->PerformWrite(
        0, 'FM_FFU_SLICE_VALID', 
        $chip->FM_FFU_SLICE_VALID($lbParams{'slice'}),
        $scenarioMask);
    if ($resp != $FM_OK)
    {
        print "ERROR configuring FFU_SLICE_VALID\n";
        return $resp;
    }
    # printf "Wrote FM_FFU_SLICE_VALID ".$lbParams{'slice'}." with 0x%x\n",$scenarioMask;

    

    return $FM_OK;
}

##@cmethod private void ConfigureARPGLORTIPMT
#
# @brief        Configure ARP_TABLE, GLORT_CAM, GLORT_RAM, IP_MULTICAST_TABLE
#               for load balancing.
#               Should be called before addLBRule
#
#
#
# Strategy for ARP_TABLE entries
#   arpCount number of entries will be added.
#   These entries will be type GLORT.
#   Each entry will point to baseGlort | (entryIndex << pos) where pos
#   is described below. baseGlort should not have bits that may interfere
#   with (entryIndex << pos).
#
#
#
# Strategy for allocating glorts
#
# Use 1 entry in GLORT_CAM/RAM
# Use RangeSubIndexA::numBits and RangeSubIndexA::pos 
#   To get different indexes into GLORT_DEST_TABLE for the ECMP hashing.
#   * numBits must be round(log2(arpCount))
#   * arp entry's glort must have bits that correspond to the arp entry's
#     offset from the base arp index and pos must be set to pick out these 
#     bits
#   * for simplicity, use pos=0.
#
# Use DestCount
#   To configure number of hashes for LAG pruning.
#   This increases the number of bins represented in the GLORT_DEST_TABLE.
#
# GLORT_DEST_TABLE's entry allocation
#   Entries are allocated starting from GDTBaseIndex.
#   Entry allocation may be sparse depending on arpCount, if arpCount
#   is not a power of 2 value, then because of RangeSubIndexA::numBits,
#   there will be unused entries in the allocated space for GLORT_DEST_TABLE
#   entries.
#
# Use of IP_MULTICAST_TABLE
#   This table is used to prevent the ingress frame from getting routed
#   due to the ARP_TABLE hit. By matching the ingress vlan in the
#   IP_MULTICAST_TABLE entry, the multicast unit will switch the frame
#   because the entry's vlan is the same as the ingress vlan. This requires that
#   the user use only 1 ingress vlan. Otherwise we need seperate
#   GLORT_DEST_TABLE entries per vlan.
#

sub ConfigureARPGLORTIPMT
{
    my ($self) = @_;
    my $chip = $self->{CHIP};

    my $resp = $FM_OK;

    # configures
    # 1) arpCount number of arp entries starting at arpIndex
    # 2) arpCount number of entries in GLORT_CAM/RAM
    # 3) arpCount * glortDestCount number of entries in GLORT_DEST_TABLE
    #    initializes the GLORT_DEST_TABLE entries to do uniform load balancing
    #
    #
    # 1) configure ARP table
    #
    for (my $arpOffset=0; $arpOffset < $lbParams{'arpCount'}; $arpOffset++) 
    {
        my $arpIndex = $lbParams{'arpIndex'} + $arpOffset;
        my $dstGlort = $lbParams{'baseGlort'} |
                         ($arpOffset << $lbParams{'rangeSubIndexAPos'});

        printf "Arp entry 0x%x offset %d dstGlort 0x%x\n", $arpIndex, $arpOffset, $dstGlort;
        
        my $arpEntryBigInt = Math::BigInt->new('0');
        SetField($arpEntryBigInt, $dstGlort, 0);
        SetField($arpEntryBigInt, 0x1, 60); # type=GLORT
        $resp = $self->PerformWrite(
            0, 'FM_ARP_TABLE',
            $chip->FM_ARP_TABLE($arpIndex, 0),
            $arpEntryBigInt->as_hex);
        if ($resp != $FM_OK)
        {
            print "ERROR writing FM_ARP_TABLE\n";
            return $resp;
        }
    }
    #
    # 2) configure GLORT_CAM/RAM
    #
    # check if GLORT_CAM/RAM index is already used
    my $glortCAMEntry = $self->PerformRead(0, 'FM_GLORT_CAM',
        $chip->FM_GLORT_CAM($lbParams{'glortCAMRAMIndex'}));
    if ($glortCAMEntry != 0)
    {
        print "selected GLORT_CAM/RAM index ".$lbParams{'glortCAMRAMIndex'}." conflicts with existing entry\n";
        return $FM_FAIL;
    }
    my $glortRAMEntry = $self->PerformRead(0, 'FM_GLORT_RAM',
        $chip->FM_GLORT_RAM($lbParams{'glortCAMRAMIndex'},0));
    if (!$glortRAMEntry->is_zero)
    {
        print "selected GLORT_CAM/RAM index ".$lbParams{'glortCAMRAMIndex'}." conflicts with existing entry\n";
        return $FM_FAIL;
    }

    # configure CAM/RAM
    # mask should mask out the bits corresponding to numBits and pos
    my $glortCAMMask = 
        ((1<<$lbParams{'rangeSubIndexANumBits'})-1) 
        << $lbParams{'rangeSubIndexAPos'};
    $glortCAMMask = (~$glortCAMMask) & 0xffff;
    my $glortCAMValue = $lbParams{'baseGlort'};
    my $newGlortCAMEntry = sprintf "0x%x", $glortCAMValue | ($glortCAMMask << 16);
    print "Writing GLORT_CAM[".$lbParams{'glortCAMRAMIndex'}."] = $newGlortCAMEntry\n";
    $resp = $self->PerformWrite(0, 'FM_GLORT_CAM',
        $chip->FM_GLORT_CAM($lbParams{'glortCAMRAMIndex'}),
        $newGlortCAMEntry);
    if ($resp != $FM_OK)
    {
        return $resp;
    }
    # configure RAM
    my $newGlortRAMBigInt = Math::BigInt->new('0');
    SetField($newGlortRAMBigInt, $lbParams{'GDTBaseIndex'}, 3); #DestIndex
    SetField($newGlortRAMBigInt, $lbParams{'rangeSubIndexAPos'}, 15);
    SetField($newGlortRAMBigInt, $lbParams{'rangeSubIndexANumBits'}, 15+4);
    SetField($newGlortRAMBigInt, $lbParams{'glortDestCount'}, 31);
    print "Writing GLORT_RAM[".$lbParams{'glortCAMRAMIndex'}."] = ".$newGlortRAMBigInt->as_hex()."\n";
    $resp = $self->PerformWrite(0, 'FM_GLORT_RAM',
        $chip->FM_GLORT_RAM($lbParams{'glortCAMRAMIndex'}, 0),
        $newGlortRAMBigInt->as_hex);
    if ($resp != $FM_OK)
    {
        print "ERROR Writing GLORT_RAM\n";
        return $resp;
    }

    

    # configure IP_MULTICAST_TABLE
    my $ipmtEntryBigInt = Math::BigInt->new('0');
    SetField($ipmtEntryBigInt, $lbParams{'ingressVlan'}, 4);
    SetField($ipmtEntryBigInt, 1, 1); #tail
    for (my $port=0; $port<25; $port++) 
    {
        my $index = $lbParams{'mtableIndex'} + $port;
        my $value = $ipmtEntryBigInt->as_int;
        printf "Writing IP_MULTICAST_TABLE[$index] 0x%08x\n", $value;
        $resp = $self->PerformWrite(0, 'FM_IP_MULTICAST_TABLE',
            $chip->FM_IP_MULTICAST_TABLE($index),
            $value);
        if ($resp != $FM_OK)
        {
            return $resp;
        }
    }

    return $FM_OK;
}

##@cmethod private void (char* type, char* ipv4Dip)
# 
# @brief        Add FFU rule to match on selected type and value.
#
# @param[in]    type FFU rule key selection, currently only "dip" is
#               supported.
#
# @param[in]    ipv4DipInt  IPv4 DIP expressed as string e.g. '192.168.1.1'.
#
# Configures the slice reserved for load balancing to match on ipv4 dip.
# Configures the tcam entry to match on selected dip.
sub AddLBDIPRule
{
    my ($self, $type, $ipv4Dip) = @_;
    my $chip = $self->{CHIP};

    if (!defined $type || !defined $ipv4Dip)
    {
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $resp = $FM_OK;
    
    printf "AddLBDIPRule: %s %s\n", $type, $ipv4Dip;

    # parse input argument
    my @bytes = split('\.',$ipv4Dip);
    if (scalar @bytes != 4)
    {
        print "could not parse $ipv4Dip, is it in the form n.n.n.n? e.g. 192.168.1.1\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }
    my $ipv4DipInt = 0;
    for (my $b=0; $b<4; $b++) {
        $ipv4DipInt = ($ipv4DipInt << 8) | ($bytes[$b] & 0xff);
    }
    printf "ipv4DipInt 0x%08x\n", $ipv4DipInt;

    if ($type ne 'dip')
    {
        print "load balancing only supports type <dip> for now not <$type>\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $tcamEntry = $lbParams{tcamEntry};
    my $slice     = $lbParams{slice};
    
    # configure selected slice to match on DIP
    my $sliceCaseCfgBigInt = Math::BigInt->new('0');
    SetField($sliceCaseCfgBigInt, 0x0, 0);      #Select0
    SetField($sliceCaseCfgBigInt, 0x0, 6);
    SetField($sliceCaseCfgBigInt, 0x0, 12);
    SetField($sliceCaseCfgBigInt, 0x0, 18);
    SetField($sliceCaseCfgBigInt, 0x3f, 24);    #SelectTop
    SetField($sliceCaseCfgBigInt, 0x1, 30);     #start compare cascade
    SetField($sliceCaseCfgBigInt, 0x1, 31);     #start action cascade
    my $sliceCaseCfg = $sliceCaseCfgBigInt->as_int;
    printf "Writing FFU_SLICE_CASE_CFG($slice,".$lbParams{case}.") = 0x%08x\n", $sliceCaseCfg;
    $resp = $self->PerformWrite(
        0, 'FM_FFU_SLICE_CASE_CFG',
        $chip->FM_FFU_SLICE_CASE_CFG($slice, $lbParams{case}),
        $sliceCaseCfg);
    if ($resp != $FM_OK)
    {
        return $resp;
    }


    # next, configure FFU rule for matching on dip
    my $sliceTcamBigInt = Math::BigInt->new('0');
    SetField($sliceTcamBigInt, $ipv4DipInt, 0); #dip to match
    SetField($sliceTcamBigInt, 0xffffffff, 32);
    SetField($sliceTcamBigInt, 0x100, 64);
    my $sliceTcamEntry = $sliceTcamBigInt->as_hex();
    printf "sliceTcamEntry $sliceTcamEntry\n";
    $resp = $self->PerformWrite(
            0, 'FM_FFU_SLICE_TCAM',
            $chip->FM_FFU_SLICE_TCAM($slice, $tcamEntry, 0), $sliceTcamBigInt->as_hex);

    #my $check = $self->PerformRead(0, 'FM_FFU_SLICE_TCAM', $chip->FM_FFU_SLICE_TCAM($slice, $tcamEntry, 0));
    #printf "read back ".$check->as_hex."\n";


    # now, configure action to 
    # 1) count and 
    # 2) ecmp route
    #
    # 1) count, configure action to count
    my $sliceSramBigInt = Math::BigInt->new('0');
    SetField($sliceSramBigInt, $lbParams{counterBank}, 34);
    SetField($sliceSramBigInt, $lbParams{counterIndex}, 24);
    print "counter bank ".$lbParams{counterBank}." index ".$lbParams{counterIndex}."\n";
    # 2) ecmp hash
    SetField($sliceSramBigInt, 0x1, 22); #command = route
    SetField($sliceSramBigInt, 0x0, 21); #route type = arp
    SetField($sliceSramBigInt, $lbParams{arpIndex}, 0);
    SetField($sliceSramBigInt, $lbParams{arpCount} % 16, 14);
    print "slice sram ".$sliceSramBigInt->as_hex."\n";
    $resp =  $self->PerformWrite(
        0, 'FM_FFU_SLICE_SRAM',
        $chip->FM_FFU_SLICE_SRAM($slice, $tcamEntry, 0), $sliceSramBigInt->as_hex);
    if ($resp != $FM_OK) 
    {
        print "ERROR writing FFU_SLICE_SRAM\n";
        return $resp;
    }
    #my $check = $self->PerformRead(0, 'FM_FFU_SLICE_SRAM', $chip->FM_FFU_SLICE_SRAM($slice, $tcamEntry, 0));
    #printf "read back FFU_SLICE_SRAM[$slice, $tcamEntry]=". $check->as_hex ."\n";
    return $FM_OK;
}


##@cmethod private void SetPortList(char* portListStr)
#
# @brief        Configure the specified logical ports to be load balancing 
#               ports
#
# @param[in]    portListStr Comma seperated list of logical ports, e.g.
#               2,3,4,5
sub SetPortList
{

    my ($self, @args) = @_;

    my $portListStr = shift @args;

    my @portList = split(',',$portListStr);

    $lbParams{portList} = \@portList;

    print "Set load balancing port list to the following logical ports\n";
    print "Load Balancing Port | Logical Port\n";
    for (my $p=0; $p<scalar @portList; $p++)
    {
        printf "%20d %20d\n", $p, $portList[$p];
    }

    return $FM_OK;
}


##@cmethod private void SetDistribution(char* cmd, ...)
#
# @brief        Set the bin to load balancing port distribution.
#               Also supports applying the selected port distribution.
#
# @param[in]    cmd command selecting whether we want to set
#               uniform or custom distribution, or whether we
#               want to apply the previously set distribution.
#
sub SetDistribution
{
    my ($self, @args) = @_;

    my $cmd = shift @args;

    if ($cmd eq "uniform")
    {
        return $self->SetUniformDistribution(@args);
    }
    elsif ($cmd eq "custom")
    {
        return $self->SetCustomDistribution(@args);
    }
    elsif ($cmd eq "apply")
    {
        return $self->ApplyDistribution(@args);
    }
    else
    {
        return $FM_ERR_INVALID_ARGUMENT;
    }
}

##@cmethod private void SetUniformDistribution
#
# @brief        Uniformly assign the load balancing ports to the
#               bins.
sub SetUniformDistribution
{
    my ($self, @args) = @_;

    print "Setting bin to load balancing port distribution to uniform\n";

    my $portListRef = $lbParams{portList};
    if (!defined $portListRef)
    {
        print "port list is not defined, did you do a set loadbalancing portlist first?\n";
        return $FM_FAIL;
    }
    my @portList = @$portListRef;
    my $numPorts = scalar @portList;
    my $numDistEntries = $lbParams{arpCount} * $lbParams{glortDestCount};

    # clear out existing distribution
    my @dist = ();
    $lbParams{distribution} = \@dist;

    my $currPort = 0; #for assignment
    for (my $e=0; $e<$numDistEntries; $e++) 
    {
        $dist[$e] = $currPort;
        #print "Assigned $e : ". $dist[$e] ."\n";
        $currPort++;
        $currPort%=$numPorts;
    }

    return $FM_OK;

}

##@cmethod private void SetCustomDistribution
#
# @brief        Set the bin to load balancing port distribution.
#               Port number range starts from 0 to number of load
#               balancing ports -1.
sub SetCustomDistribution
{

    my ($self, @args) = @_;

    my $bin    = shift @args;
    my $lbPort = shift @args; #load balancing port (0 to N-1)

    # check input range
    my $portListRef = $lbParams{'portList'};
    my @portList = @$portListRef;
    my $numPorts = scalar @portList;
    my $numDistEntries = $lbParams{'arpCount'} * $lbParams{'glortDestCount'};

    print "portlist @portList\n";

    if ($bin < 0 || $bin >= $numDistEntries)
    {
        print "Bin $bin out of range [0,".($numDistEntries-1)."]\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if ($lbPort < 0 || $lbPort >= $numPorts)
    {
        print "Load balancing port $lbPort out of range [0,".($numPorts-1)."]\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $distRef = $lbParams{distribution};

    # check distribution bin list size
    my @dist = @$distRef;
    if (scalar @dist != $numDistEntries)
    {
        print "ERROR distribution bin array size does not match arpCount and glortDestCount\n";
        return $FM_FAIL;
    }
    
    print "Setting bin $bin to map to load balacing port $lbPort\n";
    $distRef->[$bin] = $lbPort;

    return $FM_OK;

}


##@cmethod private void ApplyDistribution
#
# @brief        Apply the distribution already in
#               the load balancing configuration.
sub ApplyDistribution
{
    my ($self, @args) = @_;
    my $chip = $self->{CHIP};
    my $resp = $FM_OK;

    print "applying distribution\n";


    my $distRef = $lbParams{'distribution'};
    if (!defined $distRef)
    {
        print "distribution not set, call set loadbalancing distribution first\n";
        return $FM_FAIL;
    }    
    my @dist = @$distRef;
    
    my $portListRef = $lbParams{'portList'};
    if (!defined $portListRef)
    {
        print "port list not set, call set loadbalancing portlist first\n";
        return $FM_FAIL;
    }
    my @portList = @$portListRef;
    
    # convert logical port list to physical port list
    my @physPortList = ();
    for my $logicalPort (@portList)
    {
        my ($junk, $physPort);
        $chip->fmPlatformMapLogicalPortToPhysical(
            0, $logicalPort, \$junk, \$physPort);
        print "Converting logical port $logicalPort to physical port $physPort\n";
        push @physPortList, $physPort;
    }

    for (my $bin=0; $bin < scalar @dist; $bin++) {
        # convert from load balancing port to logical port number
        my $lbPort = $dist[$bin];
        my $logicalPort = $portList[$lbPort];
        my $physPort = $physPortList[$lbPort];
        
        print "Bin $bin logical port $logicalPort physPort = $physPort\n";

        my $physPortMask = 1 << $physPort;

        my $glortDestTableIndex = &mapBinToGlortDestTableIndex($bin);

        # check that the GLORT_DEST_TABLE entry is not yet used
        my $currGDT =  $self->PerformRead(
                0, 'FM_GLORT_DEST_TABLE', 
                $chip->FM_GLORT_DEST_TABLE($glortDestTableIndex, 0));
        if (!$currGDT->is_zero)
        {
            print "ERROR FM_GLORT_DEST_TABLE $glortDestTableIndex is used, val=".$currGDT->as_hex."\n";
            return $FM_FAIL;
        }


        my $GDTEntryBigInt = Math::BigInt->new('0');
        #DestMask
        SetField($GDTEntryBigInt, $physPortMask, 1); 
        #IP_MULTCAST_TABLE
        SetField($GDTEntryBigInt, $lbParams{'mtableIndex'}, 26); 
        printf "Writing to GLORT_DEST_TABLE %d %s\n", $glortDestTableIndex, $GDTEntryBigInt->as_hex();
        $resp = $self->PerformWrite(
                0, 'FM_GLORT_DEST_TABLE', 
                $chip->FM_GLORT_DEST_TABLE($glortDestTableIndex, 0), $GDTEntryBigInt->as_hex);
        if ($resp != $FM_OK)
        {
            print "ERROR writing FM_GLORT_DEST_TABLE\n";
            return $resp;
        }

        print "Completed write to GLORT_DEST_TABLE\n";

    }
    print "ApplyDistribution complete\n";

    return $FM_OK;
}


##@method mapBinToGlortDestTableIndex(int bin)
#
# @desc         Takes a bin number and maps it uniquely to one of the
#               GLORT_DEST_TABLE entries that are in use. Helper function
#               for apply. Note that consecutive bin entries correspond
#               to the LAG prune hash, every arpCount'th entry corresponds to
#               a ECMP hash.
#
# @param[in]    bin
#
# @return       the index into GLORT_DEST_TABLE
#
sub mapBinToGlortDestTableIndex
{
    my ($bin) = @_;

    my $glortHash = $bin % $lbParams{'glortDestCount'};
    my $arpHash   = int ($bin / $lbParams{'glortDestCount'});
    my $index = $lbParams{'GDTBaseIndex'} + 
                ($glortHash << $lbParams{'rangeSubIndexANumBits'}) +
                $arpHash;

    return $index;
}



sub ShowDistribution
{
    my ($self, @args) = @_;

    print "Load balancing distribution\n";
    
    my $distRef = $lbParams{'distribution'};
    if (!defined $distRef)
    {
        print "distribution not set, call set loadbalancing distribution first\n";
        return $FM_FAIL;
    }    
    my @dist = @$distRef;
    
    my $portListRef = $lbParams{'portList'};
    if (!defined $portListRef)
    {
        print "port list not set, call set loadbalancing portlist first\n";
        return $FM_FAIL;
    }
    my @portList = @$portListRef;

    #also compute port's weight
    my %portWeight = ();
    for (my $port=0; $port<scalar @portList; $port++)
    {
        $portWeight{$port} = 0;
    }

    print "Bin        | Load Balancing Port\n";
    for (my $bin=0; $bin < scalar @dist; $bin++)
    {
        printf "%10d %10d\n", $bin, $dist[$bin];
        $portWeight{$dist[$bin]}++;
    }
    print "Load Balancing Port | Weight\n";
    for (my $port=0; $port<scalar @portList; $port++)
    {
        printf "%20d %10f%%\n", $port, $portWeight{$port} / scalar @dist;
    }

    return $FM_OK;
}

##@cmethod private int (Math::BitInt val, int word)
#
# @return   int in the selected word position of the input BigInt
sub GetWord
{

    my ($val, $word) = @_;

    my $result = $val->copy;
    
    $result->brsft(32*$word);

    my $mask = Math::BigInt->new('1')->blsft(32)->bdec;
    
    $result->band($mask);

    return $result->as_int;
}


##@cmethod private void (Math::BigInt val, char* fieldVal, char* startPos)
#
# @brief        Sets a bit field in the Math::BigInt argument. Assumes
#               that the field position is already zero, otherwise the field
#               value will be bitwise ORed in with the existing value.
#
# @param[in]    val the Math::BigInt value in which to set a field
# 
# @param[in]    fieldVal Value that the field should be set to
#
# @param[in]    startPos Bit position that field starts at
sub SetField
{
    my ($val, $fieldVal, $startPos) = @_;

   $val->bior(Math::BigInt->new($fieldVal)->blsft($startPos));

}

sub PerformRead
{
    my ($self, $switchNum, $register, $address) = @_;

    my $chip = $self->{CHIP};

    my ($status, $value);

    $chip->disableErrors();
    if ($chip->registerIsMultiWord($register))
    {
        my $wordCount = $chip->registerGetWidth($register);
        my $words = [(0) x $wordCount];

        $status =
             $chip->fmReadUINT32Mult($switchNum, $address, $wordCount, $words);
        goto ABORT if $status != $FM_OK;

        $value = new Math::BigInt(0);
        map {
            $value->bior($words->[$_])->blsft($_ > 0 ? 32 : 0)
        } reverse(0 .. ($wordCount - 1));
    }
    else
    {
        $status = $chip->fmReadUINT32($switchNum, $address, \$value);
        goto ABORT if $status != $FM_OK;

        $value = Math::BigInt->new($value);
    }
    $chip->enableErrors();
    return $value;

ABORT:
    $chip->enableErrors();
    printf("%-30s read failed!\n", $register);
    return Math::BigInt->new(-1);
}

sub PerformWrite
{
    my ($self, $switchNum, $register, $address, $value) = @_;

    my $chip = $self->{CHIP};

    $value = new Math::BigInt($value);

    my $status;
    $chip->disableErrors();
    if ($chip->registerIsMultiWord($register))
    {
        my $wordCount = $chip->registerGetWidth($register);
        my $words = [(0) x $wordCount];

        map {
            $words->[$_] = $value->copy()->band(0xFFFFFFFF)->numify();
            $value->brsft(32);
        } (0 .. ($wordCount - 1));

        $status =
            $chip->fmWriteUINT32Mult($switchNum, $address, $wordCount, $words);
    }
    else
    {
        $value = $value->band(0xFFFFFFFF)->numify();

        $status = $chip->fmWriteUINT32($switchNum, $address, $value);
    }
    $chip->enableErrors();
    return $status == $FM_OK ? $FM_OK : $FM_FAIL;
}

1;
