# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/whiteModel/Functions.pm
# Creation Date:    June 6, 2012
# Description:      Handlers for the menu tree
#
# INTEL CONFIDENTIAL
# Copyright 2012 Intel Corporation. All Rights Reserved. 
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

package Applications::TestPoint::whiteModel::Functions;
use strict;
use warnings;

use base qw(
    Exporter
    Applications::TestPoint::Common::PlatformCore
    Applications::TestPoint::Common::FunctionCore
    Applications::TestPoint::Common::RegisterUtilCore
    Applications::TestPoint::Common::Utilities
);

use SDK qw(/^\$/);
use Applications::TestPoint::Common::FunctionCore qw(buildMacAddress);
use Time::HiRes qw( usleep );

##@cmethod public int[] tpPlatformGetPortRange(void)
#
# @desc         Retrieves the minimum and maximum system wide logical port
#               numbers
#
# @return       The system wide (minimum logical port, maximum logical port)
#               tupple
sub tpPlatformGetPortRange
{
    my ($self) = @_;
    return (0, $self->{maxPort});
}

##@cmethod public int[] tpPlatformGetFaceplatePortRange(void)
#
# @desc         Retrieves the minimum and maximum faceplate port numbers
#
# @return       The system wide (minimum logical port, maximum logical port)
#               tupple
sub tpPlatformGetFaceplatePortRange
{
    my ($self) = @_;
    return (0, $self->{maxPort});
}


##@cmethod public char * tpPlatformGetSwitchPortList(int switchNum)
#
# @desc         Retrieves the list of ports that appears on a particular
#               switch.
#
# @param[in]    switchNum is the switch number for which to retrieve the port
#               list.
#
# @return       A list of ports.
sub tpPlatformGetSwitchPortList
{
    my ($self, $switchNum) = @_;

    return (0..$self->{maxPort});
}



##@cmethod public void int[] tpPlatformMapGlobalToLogicalPort(int globalPort)
#
# @desc         Reverse maps a system wide logical port to a
#               (switch, logical port) tupple
#
# @param[in]    globalPort The system wide logical port to be mapped to
#               (switch, logical port) tupple
#
# @return       The (switch, logical port) tupple if successful
# @return       (-1, -1) tupple if the system wide logical port cannot be
#               mapped to a (switch, logical port) tupple
sub tpPlatformMapGlobalToLogicalPort
{
    my ($self, $globalPort) = @_;
    my $sw;
    my @switchArray;

    @switchArray = $self->tpGetSwitches;
    $sw = $switchArray[0];
    return ($sw, $globalPort);
}

##@cmethod public tpPlatformMapLogicalToFaceplatePort(int   switchNum
#                                                     int   logicalPort)
#
# @desc         Maps a (switch, logical port) tuple to a faceplate port
#
# @param[in]    switchNum The switch number of the (switch, logical port) tuple
#
# @param[in]    logicalPort The logical port number of the
#               (switch, logical port) tuple
#
# @return       The faceplate port if successful
# @return       -1 if the (switch, logical port) tuple cannot be mapped to a
#               faceplate port
sub tpPlatformMapLogicalToFaceplatePort
{
    my ($self, $switchNum, $logicalPort) = @_;

    return $logicalPort;
}

##@cmethod public int[] tpPlatformGetVLANRange(void)
#
# @desc         Retrieves the minimum and maximum VLAN IDs
#
# @return       The (mimimum VLAN ID, maximum VLAN ID) tupple
sub tpPlatformGetVLANRange
{
    my ($self) = @_;

    return (0, 4094);
}

##@cmethod public int[] tpPlatformGetLAGRange(void)
#
# @desc         Retrieves the minimum and maximum system wide logical LAG
#               numbers
#
# @return       The system wide (minimum logical LAG, maximum logical LAG)
#               tupple
sub tpPlatformGetLAGRange
{
    return (0, 35);
}

##@cmethod public int tpPlatformMapLogicalToGlobalLAG(int   switchNum,
#                                                     int   logicalLag)
#
# @desc         Reverse maps a (switch, logical LAG) tuple to a system wide
#               logical LAG number
#
# @param[in]    switchNum The switch number of the (switch, logical LAG) tupple
#
# @param[in]    logicalLag The logical LAG number of the (switch, logical LAG)
#               tupple
#
# @return       The system wide logical LAG number if successful
# @return       -1 if the (switch, logical LAG) tupple cannot be mapped to a
#               system wide logical LAG number
sub tpPlatformMapLogicalToGlobalLAG
{
    my ($self, $switchNum, $logicalLag) = @_;

    return $logicalLag;
}

##@cmethod public int[] tpPlatformMapGlobalToLogicalLAG(int globalLag)
#
# @desc         Maps a system wide logical LAG number to a
#               (switch, logical LAG) tupple
#
# @param[in]    globalLag The system wide logical LAG number to be mapped to a
#               (switch, logical LAG) tupple
#
# @return       The (switch, logical LAG) tupple if successful
# @return       (-1, -1) tupple if the system wide logical LAG number cannot be
#               mapped to a (switch, logical LAG) tupple
sub tpPlatformMapGlobalToLogicalLAG
{
    my ($self, $globalLag) = @_;

    return (0, $globalLag);
}

##@cmethod public void tpPlatformExit(void)
#
# @desc         Exit the TestPoint interface.
sub tpPlatformExit
{
    my ($self) = @_;
    my $chip = $self->{CHIP};

    $chip->fmModelTestPointIntfExit();
    $chip->fmPlatformCloseInstrumentation();
}

##@cmethod public void handleWModelSend(int     port,
#                                      char    *args[])

sub handleWModelSend
{
    my ($self, $port, @args) = @_;
    my $vid=-1;
    my $dmacHigh=0;
    my $dmacLow=2;
    my $smacHigh=0;
    my $smacLow=1;
    my $length=64;
    my @payload;
    my $vpri=int(0);
    my $etype=0;
    my $tmp=0;
    my @portList=();
    my $chip = $self->{CHIP};
    my $sw = 0; 
    my $useKeys = 0;
    
    ############################################
    # Analyze arguments
    ############################################
    
    if ( defined($args[0]) && $args[0] eq "keys" )
    {
        shift(@args);
        
        $useKeys = 1;

        foreach my $arg (@args)
        {
            my ($type,$val) = split('=',lc($arg));
            
            if ( !defined($val) || $val eq "" || !defined($type) || $type eq "" )
            {
                print "Invalid argument key=<$type>, val=<$val>\n";
                return;
            }
            
            if ( $type eq "dmac" )
            {
                ($dmacLow,$dmacHigh) = $self->buildMacAddress($val);
            }
            elsif ( $type eq "smac" )
            {
                ($smacLow,$smacHigh) = $self->buildMacAddress($val);
            }
            elsif ( $type eq "l" )
            {
                if ( $val =~ m/^0x/ )
                {
                    $length = hex($val);
                }
                else
                {
                    $length = int($val);
                }
                if ( $length < 64 || $length > $FM_MODEL_MAX_PACKET_SIZE )
                {
                    print("Invalid length <$length>, must be between 64 " . 
                          "and $FM_MODEL_MAX_PACKET_SIZE\n"); 
                    return;
                }
            }
            elsif ( $type eq "vid" )
            {
                if ( $val =~ m/^0x/ )
                {
                    $vid = hex($val);
                }
                else
                {
                    $vid = int($val);
                }
                if ( $vid < 0 || $vid > 4095 )
                {
                    print "Invalid vid <$vid>, must be between 0 and 4095\n"; 
                    return;
                }
            }
            elsif ( $type eq "vpri" )
            {
                if ( $val =~ m/^0x/ )
                {
                    $vpri = hex($val);
                }
                else
                {
                    $vpri = int($val);
                }
                if ( $vpri < 0 || $vpri > 7 )
                {
                    print "Invalid vpri <$vpri>, must be between 0 and 7\n"; 
                    return;
                }
            }
            elsif ( $type eq "type" )
            {
                if ( $val =~ m/^0x/ )
                {
                    $etype = hex($val);
                }
                else
                {
                    $etype = int($val);
                }
            }
            
        }   # end foreach my $arg (@args)
        
    }
    elsif ( defined($args[0]) && $args[0] eq "file-pcap" )
    {
        my $filename = $args[1];
        my $ignoretimestamps= $args[2];
        my $buf;
    
        my @portList = $self->validateList($port, $self->tpPlatformGetPortRange);
        if (!defined($port) || scalar(@portList) == 0)
        {
            print "Invalid port array list\n";
            return;
        }
        
        if (not -e $filename) 
        {
            print("File $filename does not exist\n");
            return;
        }
        if (!open (PCAP, "<$filename")) 
        {
            print("Could not open file $filename: $!\n");
            return;
        }
    
        my $skiptime = 0;
        if ((defined($ignoretimestamps)) && ($ignoretimestamps eq "ignore-timestamps")) 
        {
            $skiptime = 1;
        }
    
        binmode PCAP;
    
        # read in the global header
        my $headlen = 6;
        if (not read(PCAP, $buf, $headlen*4)) {
            print("Could not read global header in PCAP file.\n");
        }
        my $byteOrder = 'V';
    
        my $testword = unpack $byteOrder, $buf;
    
        printf "Magic Number: %08x\n", $testword;
        if ($testword == 0xd4c3b2a1) 
        {
            print ("Switching to network order\n");
            $byteOrder = 'N';
        } 
        elsif ($testword != 0xa1b2c3d4) 
        {
            printf ("Bad Magic Word 0x%x != 0xa1b2c3d4 or d4c3b2a1\n", $testword);
            return;
        }
    
        my @header = unpack $byteOrder x $headlen, $buf;
    
        my $packetheadlen = 4;
        my $tare = 0;
        my $delta = 0;
        my $lastTime = 0;
        #read each frame in
        while (read(PCAP, $buf, $packetheadlen*4)) 
        {  
            @header = unpack $byteOrder x $packetheadlen, $buf;
            my $ts = $header[0] + ($header[1] / 1.0E6);
            if ($tare == 0) 
            { 
                $tare = $ts; 
            } 
            else 
            { 
                $delta = $ts-$lastTime; 
            }
        
            if ($skiptime == 0) 
            {
                printf "Timestamp:   %f (Delta %f)\n", $ts - $tare, $delta;
                usleep($delta * 1.0E6 );
            }
            
            $lastTime = $ts;

            read(PCAP, $buf, $header[2]);
            @payload = unpack 'C' x $header[2] , $buf;
            $length = $header[2];
            
            foreach my $switchNum ($self->tpGetSwitches)
            {
                if ($port ne $FM_DIRECT_VLAN_SEND)
                {
                    @portList = $self->validateExplicitList($TRUE, 
                                                            $port, 
                                       $self->tpPlatformGetSwitchPortList($switchNum));
                }        
                
                if (scalar(@portList) == 0)
                {
                    next;
                }
                
                foreach my $globalPort (@portList)
                {
                    my $logicalPort;
                    
                    if ($globalPort == $FM_DIRECT_VLAN_SEND)
                    {
                        $logicalPort = $globalPort;
                    }
                    else
                    {
                        # Map port number for the benefit of non-SWAG Vegas. 
                        # Ignore the returned sw since it should match 
                        # switchNum and may actually be wrong for some platforms.
                        ($sw, $logicalPort) = 
                              $self->tpPlatformMapGlobalToLogicalPort($globalPort);
                    }
            
                    # Adjust packet length in order to include the FCS
                    $length += 4;
                    print "Sending frame on sw $sw port $logicalPort len $length \n";
                    $chip->fmModelTestPointIntfSendFullPacket($switchNum,
                                                              $logicalPort,
                                                              \@payload,
                                                              $length);
            
                }
                
            }   # for each switch 
        }
    
        close PCAP;
        return;             
    }
    else
    {
        my $status;
        
        if (defined($args[0]))
        {
            if ( $args[0] < 64 || $args[0] > $FM_MODEL_MAX_PACKET_SIZE )
            {
                print("Invalid length <$args[0]>, must be between 64 " .
                      "and $FM_MODEL_MAX_PACKET_SIZE\n"); 
                return;
            }
            $length = $args[0];
        }
        
        ($status, @payload) = $self->ParsePacketFields($port, @args);
        
        if ($status != $FM_OK)
        {
            return;
        }
    
        my $pval = 0;
        # Pad the payload with incremental data.
        for (my $i = scalar(@payload); $i < $length; $i++)
        {
            push(@payload, $pval & 0xFF);
            $pval++;
        }
        
        # Extract the arguments for tpSendPacketHelperL2
        my $idx = 0;
        $dmacHigh = ($payload[$idx++] << 16) | ($payload[$idx++] << 8) | $payload[$idx++];
        $dmacLow =  ($payload[$idx++] << 16) | ($payload[$idx++] << 8) | $payload[$idx++];

        $smacHigh = ($payload[$idx++] << 16) | ($payload[$idx++] << 8) | $payload[$idx++];
        $smacLow =  ($payload[$idx++] << 16) | ($payload[$idx++] << 8) | $payload[$idx++];
            
        if ($payload[$idx] == 0x81 && $payload[$idx + 1] == 0x00)
        {
            $idx += 2;
            $vpri = ($payload[$idx] >> 5) & 0x07;
            $vid = (($payload[$idx++] & 0x0f) << 8) | $payload[$idx++];
        }
        
        $etype = ($payload[$idx++] << 8) | $payload[$idx];
        
    }   # end if ( $args[0] eq "keys" )

    ###########################################
    # Send the packet
    ###########################################

    my @affectedPortList = ();
    my @globalPortList = ();
    
    if (scalar(@portList) == 0)
    {
        @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
        if (!defined($port) || (scalar(@globalPortList) == 0))
        {
            print "Invalid port array list\n";
            return;
        }
    }
    else
    {
        @globalPortList = @portList;
    }

    foreach my $switchNum ($self->tpGetSwitches)
    {
        if ($port ne $FM_DIRECT_VLAN_SEND)
        {
            @portList = $self->validateExplicitList($TRUE, 
                                                    $port, 
                                                    $self->tpPlatformGetSwitchPortList($switchNum));
        }        
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        my @switchPortList = (); 
        
        push(@affectedPortList, @portList);
        
        foreach my $globalPort (@portList)
        {
            my $logicalPort;
            
            if ($globalPort == $FM_DIRECT_VLAN_SEND)
            {
                $logicalPort = $globalPort;
            }
            else
            {
                # Map port number for the benefit of non-SWAG Vegas. Ignore the
                # returned sw since it should match switchNum and may actually be
                # wrong for some platforms.
                ($sw, $logicalPort) = 
                                  $self->tpPlatformMapGlobalToLogicalPort($globalPort);
            }
   
            if ($useKeys == 0)
            {
                $chip->fmModelTestPointIntfSendFullPacket($switchNum,
                                                          $logicalPort,
                                                          \@payload,
                                                          $length);
            }
            else
            {
            
                $chip->fmModelTestPointIntfSendL2Packet($switchNum,
                                                        $logicalPort,
                                                        $dmacHigh,
                                                        $dmacLow,
                                                        $smacHigh,
                                                        $smacLow,
                                                        $vid,
                                                        $vpri,
                                                        $etype,
                                                        $length);
            }
        }
        
    }   # for each switch 

    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
}


sub handleWModelTopology
{
    my ($self, $file) = @_;
    my $chip = $self->{CHIP};

    my %void = (type => "fm_char", value => "$file");

    $chip->fmPlatformSetAttribute(0, 0, $FM_PLATFORM_ATTR_TOPOLOGY, \%void);
}


sub handleWModelInit
{
    my ($self) = @_;
    my $chip = $self->{CHIP};
    
    if (!defined($self->{modelInitialized}))
    {
        $chip->fmModelTestPointIntfInit();
        $self->{modelInitialized} = 1;
    }
}

sub handleShowL2lSweepers
{
    my ($self, $doWithRegs) = @_;
    
    my $chip = $self->{CHIP};
    my $withRegs = 0;
    
    if (defined($doWithRegs) && $doWithRegs eq "with-regs")
    {
        $withRegs = 1;
    }
    
    foreach my $switchNum ($self->tpGetSwitches)
    {
        $chip->fmDbgDumpL2LSweepers($switchNum, $withRegs);
    }
}

sub handleWModelShowCapture
{
    my ($self) = @_;
    my $chip = $self->{CHIP};
    
    $chip->fmModelTestPointIntfPrintCapture();
}

sub handleWModelShowStats
{
    my ($self) = @_;
    my $chip = $self->{CHIP};
    
    $chip->fmPlatformPrintServerStats();
}

sub handleWModelShowPackets
{
    my ($self,$onoff) = @_;
    my $chip = $self->{CHIP};
    
    $onoff = lc($onoff);
    if ( $onoff eq "on" )
    {
        print "Turned printing packet on\n";
        $chip->fmModelTestPointIntfPrintPackets(1);
    }
    else
    {
        print "Turned printing packet off\n";
        $chip->fmModelTestPointIntfPrintPackets(0);
    }
    
}


1;
