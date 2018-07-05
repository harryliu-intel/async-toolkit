# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/WatermarkCore.pm
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

package Applications::TestPoint::Common::WatermarkCore;
use strict;
use warnings;

#use base qw(
#    Exporter
#    Applications::TestPoint::Common::FM2000::WatermarkCore
#    Applications::TestPoint::Common::FM4000::WatermarkCore
#    Applications::TestPoint::Common::FM6000::WatermarkCore
#);

use List::Util qw(sum);
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

use constant FALSE => 0;
use constant TRUE  => 1;

our @EXPORT = qw(
    tpHandleShowMemory
    tpHandleSetWatermarks
    tpHandleShowWatermarks
);

our @pauseEnabledState = ();

##@method void initPauseState()
#   Will initialize the pause enable vector if it already has not been
#
sub initPauseState
{
    my ($self) = @_;
    my $p;
    my $chip = $self->{CHIP}; 

    if (@pauseEnabledState == 0)
    {
        my @portList = 
            $self->validateList("all", $self->tpPlatformGetPortRange());
        foreach $p (@portList)
        {
             $pauseEnabledState[$p] = $FM_DISABLED;
        }
    }
}

##@method void setPauseEn(int port)
#   Sets whether pause is enabled or not on the given port.
sub setPauseEn($$)
{
    my ($self, $port, $en) = @_;

    my $chip = $self->{CHIP};

#     $pauseEnabledState[$port] = $en;
     # set the pause enable state using the API
    my %en = (type => "fm_bool", value => $en);

    # @affectedPortList keeps track of port numbers actually affected by command
    my @affectedPortList = ();
   
    # Convert $port to an array of port numbers
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    # Operate on each selected switch, one at a time
    foreach my $switchNum ($self->tpGetSwitches)
    {    
        # Convert $port to an array of ports that appear only on this switch
        my @portList = $self->validateExplicitList($TRUE, 
                                                  $port, 
                                                  $self->tpPlatformGetSwitchPortList($switchNum));
        
        # If none of the ports appear on this switch, go to the next switch
        if (scalar(@portList) == 0)
        {
            next;
        }
       
        # Add these ports to the affected list
        push(@affectedPortList, @portList);

        # Operate on each specified port that appears on this switch
        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned sw since it should match switchNum and may actually be
            # wrong for some platforms.
            my ($sw, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
           
            $chip->fmSetPortAttribute($switchNum, $logPort, $FM_PORT_LOSSLESS_PAUSE, \%en);
        }
    }
   
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
}

##@method bool getPauseEn(int port)
#   Returns whether pause is enabled or not on the given port.
sub getPauseEn($)
{
    my ($self, $port) = @_;
    my $chip = $self->{CHIP}; 
    my %en = (type => "fm_bool", value => 0);
    my ($sw, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
    $chip->fmGetPortAttribute($sw, $logPort, $FM_PORT_LOSSLESS_PAUSE, \%en);
    return $en{value}; 
#    return $pauseEnabledState[$port];
}

##@method void setSMPPauseEn(int port, int smpMask)
#   Sets whether pause is enabled per SMP on the given port .
sub setSMPPauseEn($$)
{
    my ($self, $port, $smpMask) = @_;

    my $chip = $self->{CHIP};

    # set the pause enable state using the API
    my %en = (type => "fm_uint32", value => $smpMask);

    # @affectedPortList keeps track of port numbers actually affected by command
    my @affectedPortList = ();
   
    # Convert $port to an array of port numbers
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    # Operate on each selected switch, one at a time
    foreach my $switchNum ($self->tpGetSwitches)
    {    
        # Convert $port to an array of ports that appear only on this switch
        my @portList = $self->validateExplicitList($TRUE, 
                                                  $port, 
                                                  $self->tpPlatformGetSwitchPortList($switchNum));
        
        # If none of the ports appear on this switch, go to the next switch
        if (scalar(@portList) == 0)
        {
            next;
        }
       
        # Add these ports to the affected list
        push(@affectedPortList, @portList);

        # Operate on each specified port that appears on this switch
        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned sw since it should match switchNum and may actually be
            # wrong for some platforms.
            my ($sw, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
           
            $chip->fmSetPortAttribute($switchNum, $logPort, $FM_PORT_SMP_LOSSLESS_PAUSE, \%en);
        }
    }
   
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
}

##@method bool getSMPPauseEn(int port)
#   Returns a bitmask where each bit represents an SMP with PAUSE enabled
sub getSMPPauseEn($)
{
    my ($self, $port) = @_;
    my $chip = $self->{CHIP}; 
    my %en = (type => "fm_uint32", value => 0);
    my ($sw, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
    $chip->fmGetPortAttribute($sw, $logPort, $FM_PORT_SMP_LOSSLESS_PAUSE, \%en);
    return $en{value}; 
}

##@method void tpHandleSetWatermarks()
#   Run this whenever the pauseEnabledState or max frame size on a port changes.
#   Will reconfigure all of the watermarks given these variables.
#
sub tpHandleSetWatermarks
{
    my ($self) = @_;
    my $chip = $self->{CHIP};

    my ($firstSwitch, $lastSwitch) = $self->tpPlatformGetSwitchRange();

    # FIXME: shouldn't this be discovered from switch info?
    my @portList = $self->validateList("all", $self->tpPlatformGetPortRange());

    # initialize pause state if not already
    $self->initPauseState();

    # Set all switch global watermarks
    for (my $sw = $firstSwitch; $sw <= $lastSwitch; $sw++)
    {
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);

        # Hash used for setting void * values
        my %val = (type => "fm_uint32", value => 0);

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000)
        {
            # Set the FM400 Watermarks
            $self->tpFM4000SetWatermarkParameters($sw, @pauseEnabledState);
        }
        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000)
        {
            $self->tpFM2000SetWatermarkParameters($sw, @pauseEnabledState);
        }
    }
}

##@method void tpHandleShowMemory()
# Handler for showing the memory use and watermarks
#
#   @param[in] rxPort The RX port number to show memory for
#   @param[in] txPort The TX port number to show memory for
#   @param[in] rxmp The RX memory partition to show memory for
#   @param[in] txmp The TX memory partition to show memory for
#   @param[in] bsg The BSG to show ERL credits for
#   @param[in] useSegments Whether to show segments or bytes.  "segments" if
#              showing segments, undef to show bytes.
#
sub tpHandleShowMemory
{
    my ($self, $rxPort, $txPort, $rxmp, $txmp, $bsg, $useSegments) = @_;
    my $chip = $self->{CHIP};
    my ($firstSwitch, $lastSwitch) = $self->tpPlatformGetSwitchRange();

    # initialize pause state if not already
    $self->initPauseState();

    for(my $sw = $firstSwitch; $sw <= $lastSwitch; $sw++)
    {
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000 ||
            $info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
        {
            $self->tpFM4000HandleShowMemory($sw, $rxPort);
        }
        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000)
        {
            $self->tpFM2000HandleShowMemory($sw, $rxPort);
        }
        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
        {
            $self->tpFM6000HandleShowMemory($sw, $rxPort, $txPort, $rxmp,
                                            $txmp, $bsg, $useSegments);
        }
    }
}

##@method void tpHandleShowDbgQOS()
# Handler for showing the QOS state
#
#   @param[in] port The port number 
#
sub tpHandleShowDbgQOS
{
    my ($self, $port) = @_;

    my $chip = $self->{CHIP};
    my ($firstSwitch, $lastSwitch) = $self->tpPlatformGetSwitchRange();

    # initialize pause state if not already
    $self->initPauseState();

    for(my $sw = $firstSwitch; $sw <= $lastSwitch; $sw++)
    {
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);
        
        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
        {
            $self->tpFM6000HandleShowDbgQOS($sw, $port);
        }
    }
}

##@method void tpHandleShowSwitchMap()
# Handler for showing the switch mapping tables
#
#   @param[in] map The mapping table to show
#
sub tpHandleShowSwpriMap
{
    my ($self, $map) = @_;

    my $chip = $self->{CHIP};
    my ($firstSwitch, $lastSwitch) = $self->tpPlatformGetSwitchRange();

    for(my $sw = $firstSwitch; $sw <= $lastSwitch; $sw++)
    {
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);
        
        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
        {
            $self->tpFM6000HandleShowSwpriMap($sw, $map);
        }
    }
}

##@method void tpHandleShowPortIdxMap()
# Handler for showing the port index based mapping tables
#
#   @param[in] port The port to show the mapping table for
#
#   @param[in] map The mapping table to show
#
sub tpHandleShowPortIdxMap
{
    my ($self, $port, $map) = @_;

    my $chip = $self->{CHIP};
    my ($firstSwitch, $lastSwitch) = $self->tpPlatformGetSwitchRange();

    for(my $sw = $firstSwitch; $sw <= $lastSwitch; $sw++)
    {
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);
        
        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
        {
            $self->tpFM6000HandleShowPortIdxMap($sw, $port, $map);
        }
    }
}

##@method void tpHandleShowWatermarks()
# Handler for showing the watermarks
#
#   @param[in] port The port number 
#   @param[in] islPri The ISL Priority (FM6000 only)
#   @param[in] rxmp The RXMP (FM6000 only)
#
sub tpHandleShowWatermarks
{
    my ($self, $rxPort, $txPort, $rxmp, $txmp, $islPri) = @_;
    my $chip = $self->{CHIP};
    my ($firstSwitch, $lastSwitch) = $self->tpPlatformGetSwitchRange();
    my $wmScheme = "";
    # initialize pause state if not already
    $self->initPauseState();

    # Get switch information from SDK
    my $info = new SDK::fm_switchInfo();
    $chip->fmGetSwitchInfo(0, $info);

    if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000 ||
        $info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
    {
        $wmScheme = SDK::fmGetTextApiAttribute($FM_AAK_API_FM4000_WMSELECT,
                                               $FM_AAD_API_FM4000_WMSELECT);
    }
    elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
    {
        $wmScheme = SDK::fmGetTextApiAttribute($FM_AAK_API_FM6000_WMSELECT,
                                               $FM_AAD_API_FM6000_WMSELECT);
    }

    printf("\n");
    printf("Watermark scheme selected: $wmScheme \n");
    if ($info->{'switchFamily'} != $FM_SWITCH_FAMILY_FM6000)
    {
        printf("All values are represented in kB\n");
    }
    printf("\n");
    
    for(my $sw = $firstSwitch; $sw <= $lastSwitch; $sw++)
    {
        if ($lastSwitch > 0)
        {
            printf("\n");
            printf("--------------------------------\n");
            printf("|   Watermarks in switch #$sw    |\n");
            printf("--------------------------------\n");
        }
        
        # Get switch information from SDK
        $chip->fmGetSwitchInfo($sw, $info);

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000 ||
            $info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
        {
            $self->tpFM4000HandleShowWatermarks($sw, $rxPort);
        }
        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000)
        {
            # FM2000 just uses the show memory, which already includes the
            # watermarks
            $self->tpFM2000HandleShowMemory($sw, $rxPort);
        }
        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
        {
            $self->tpFM6000HandleShowWatermarks($sw, $rxPort, $txPort, $rxmp,
                                                $txmp, $islPri);
        }
    }
}

##@method void tpHandleSetWatermarksScheme()
# Handler for setting the API's watermark atribute
#
#   @param[in] scheme   Watermark scheme 
#
sub tpHandleSetWatermarksScheme
{
    my ($self, $scheme) = @_;
    my $chip = $self->{CHIP};
    
    $chip->enableErrors();
 
    my %void = (type => 'fm_char', value => $scheme); 

    if (!defined $scheme)
    {
        print "A watermark scheme must be specified.\n";
        return $FM_FAIL;
    }

    # TODO: We probably need to do more for multiple switch setups
    my $info = new SDK::fm_switchInfo();
    $chip->fmGetSwitchInfo(0, $info);
 
    if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000)
    {
        if ($scheme ne "shared"         &&
            $scheme ne "shared_cpu"     &&
            $scheme ne "perport_cpu"    &&
            $scheme ne "combined"       &&
            $scheme ne "combined_cpu")
        {
            print "Invalid Scheme, Scheme must be one of: shared, shared_cpu, perport_cpu, combined, combined_cpu!\n";
            return $FM_FAIL;    
        }

        $chip->fmSetApiAttribute("api.FM4000.wmSelect",
                                 $FM_API_ATTR_TEXT,
                                 \%void);
    }
    elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
    {
        if ($scheme ne "disabled"       &&
            $scheme ne "lossy"          &&
            $scheme ne "lossy_lossless" &&
            $scheme ne "lossless")
        {
            print "Invalid Scheme, Scheme must be one of: disabled, lossy, lossy_lossless, lossless\n";
            return $FM_FAIL;
        }

        $chip->fmSetApiAttribute("api.FM6000.wmSelect",
                                 $FM_API_ATTR_TEXT,
                                 \%void);
    }


    #############################################
    # The GetSwitchQOS and SetSwitchQOS functions
    # are only used to refresh the watermark 
    # configuration within the API
    #############################################

    %void = (type => 'fm_bool', value => 0);
    foreach my $sw ($self->tpGetSwitches)
    {
        $chip->fmGetSwitchQOS($sw, $FM_AUTO_PAUSE_MODE, 0, \%void);
        $chip->fmSetSwitchQOS($sw, $FM_AUTO_PAUSE_MODE, 1, \%void);
    }
}

1;
