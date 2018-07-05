# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/StreamCore.pm
# Creation Date:    09/10/08
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

package Applications::TestPoint::Common::PacketCaptureCore;
use strict;
use warnings;

use List::Util qw(sum);
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;
use Time::HiRes qw( usleep );

###############################################################################
#
#                               LOCAL FUNCTIONS
#
###############################################################################


###############################################################################
#
#                               PUBLIC FUNCTIONS
#
###############################################################################


##@cmethod public int tpSendPacketFilePcap(int port, string filename, boolean ignoretimestamps)
#
# @desc         Sends a stream of packets given in a PCAP file.
#
# @param[in]    port The port to send the frames out of
#
# @param[in]    filename The file to read the frames out of
#
# @param[in]    ignoretimestamps Set to true to ignmore the timing in the PCAP
#               file
#
# @note         Currently only one PCAP file can be sent at once         
#
# @return       FM_OK if successful
sub tpSendPacketFilePcap
{
    my ($self, $port, $filename, $ignoretimestamps) = @_;
    my $chip = $self->{CHIP};
    my $switchNum = 0;
    my $buf;

    my @portList = $self->validateList($port, $self->tpPlatformGetPortRange);
    if (!defined($port) || scalar(@portList) == 0)
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }
    
    if (not -e $filename) {
        print("File $filename does not exist\n");
        return;
    }
    if (!open (PCAP, "<$filename")) {
        print("Could not open file $filename: $!\n");
        return;
    }

    my $skiptime = 0;
    if (defined $ignoretimestamps &&
        $ignoretimestamps eq "ignore-timestamps") {
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
    if ($testword == 0xd4c3b2a1) {
        print ("Switching to network order\n");
        $byteOrder = 'N';
    } elsif ($testword != 0xa1b2c3d4) {
        printf ("Bad Magic Word 0x%x != 0xa1b2c3d4 or d4c3b2a1\n", $testword);
        return;
    }

    my @header = unpack $byteOrder x $headlen, $buf;

    printf "Version:      %08x\n", $header[1];
    printf "Timezone:     %08x\n", $header[2];
    printf "Sig Figs:     %08x\n", $header[3];
    printf "Max Length:   %08x\n", $header[4];
    printf "Network:      %08x\n", $header[5];

    my $packetheadlen = 4;
    my $tare = 0;
    my $delta = 0;
    my $lastTime = 0;

    #read each frame in
    while (read(PCAP, $buf, $packetheadlen*4)) {
      @header = unpack $byteOrder x $packetheadlen, $buf;
      my $ts = $header[0] + ($header[1] / 1.0E6);
      if ($tare == 0) { $tare = $ts; } else { $delta = $ts-$lastTime; }
      printf "Included. Length: %d Total Length: %d\n", $header[2], $header[3];

      if ($skiptime == 0) {
          printf "Timestamp:   %f (Delta %f)\n", $ts - $tare, $delta;
          usleep($delta * 1.0E6 );
      }
      read(PCAP, $buf, $header[2]);
      my @bytes = unpack 'C' x $header[2] , $buf;

      # The packet is passed to tpSendPacketDirectHelper using a global
      # intermediate buffer. The packet content is copied into that buffer
      # using tpPacketPayloadSet.
      # Note: the packets read from the pcap file do not include the CRC
      $chip->tpPacketPayloadClear();
      $chip->tpPacketPayloadSet($header[2], \@bytes);

      # Print the packet
      if (scalar(@bytes) > 12) {
          print "packet send manual $port " . ($header[2]+4) . " "; 
          for (my $index = 0; $index<scalar(@bytes); $index++) {
              if ($index < 5 || ($index > 5 && $index < 11)) {
                  printf ("%02x:",$bytes[$index]);
                  next;
              }
              if ($index == 5 || $index == 11) {
                  printf ("%02x ",$bytes[$index]);
                  next;
              }
              printf ("0x%02x ",$bytes[$index]);
          }
      }
      print "\n";

      # Send packet to the specified port(s)
      $chip->tpSendPacketDirectHelper($switchNum,
                                      \@portList,
                                      scalar(@portList),
                                      $header[2]);
      $lastTime = $ts;
    }
    close PCAP;
}

##@cmethod public int tpRxPacketFilePcap(int port, string filename, boolean ignoretimestamps)
#
# @desc         Sends a stream of packets to a PCAP file.
#
# @param[in]    filename The file to write the frames to
#
# @note         The function requires that an ACL is setup, that will trap the intended frames 
#               before the traffic that is to be captured is sent. 
#
# @return       FM_OK if successful

sub tpRxPacketFilePcap
{
    my ($self, $filename) = @_;
    my $chip = $self->{CHIP};
    my $switchNum = 0;
    my $info = ($self->tpGetSwitchInfo())[$switchNum];

    
    if (!defined($filename))
    {
        print("Missing file name!\n");
        return;
    }
    
    # open the file for output, If file does not exist, it is created.  
    # If file does exist, it is truncated to zero bytes.
    if (!open (PCAP, ">$filename")) 
    {
        print("Could not open file $filename: $!\n");
        return;
    }

    binmode PCAP;

    # create the file Section Header Block 
    my $magicnumber = 0xa1b2c3d4;
    my $version = 0x00040002;
    my $sectionLength = -1;
    my $timezone = 0;    my $snapLength = 15360;
    my $network = 1;

    #save the Section Header Block in the file 
    print PCAP pack('l*', $magicnumber, $version, $timezone, $timezone, $snapLength, $network );
    
    my $src = -1;
    my $vlan = -1;
    my $len = -1;
    my $valid = -1;
    my $frameAction = -1;
    my @payload = ();

    # get the rx frames from testpoint 
    while( 1)
    {
        $chip->tpGrabRecvPktInfo(\$src, \$vlan, \$len, \$frameAction, \$valid);

        # invalid frames means no more left we are done
        if($valid == 0)
        {
            # printf("No more packets left in queue!\n");
            return;
        }
        # save frames to the pcap file
        else 
        {
            # Get the frame from the frame buffer
            for(my $i = 0; $i < $len; $i++)
            {
                my $val = -1;
                $chip->tpGrabRecvPktPayload($i, \$val);
                push(@payload, $val);
            }

            printf("Packet received on port $src, VLAN $vlan (length $len bytes, trap code = 0x%X)\n",$frameAction);

            # create the Packet Block Header 
            # we have no timestamp information so just set to 0
            my $timestamp  = 0;
        
            # save the Packet Block Header to the file 
            # the time stamp is 64 bits so save the zero twice.
            print PCAP pack('l*', $timestamp, $timestamp, $len, $len);

            # build the format string with unsigned char and the frame length
            my $format = 'C' . $len;
            # save the frame data to the file 
            print PCAP pack($format, @payload);
    
        }
        
        @payload = ();
        $chip->tpGrabRecvPkt();
    }

    close PCAP;
 
}

1;
