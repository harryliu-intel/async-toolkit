# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/PlatformCore.pm
# Creation Date:    09/11/07
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

package Applications::TestPoint::Common::PlatformCore;
use strict;
use warnings;

use base qw(Exporter);

use List::Util qw(max min);
use SDKScalars;
our @EXPORT = qw(
    tpPlatformGetPortRange
    tpPlatformGetLogicalPortList
    tpPlatformGetFaceplatePortRange
    tpPlatformMapLogicalToGlobalPort
    tpPlatformMapGlobalToLogicalPort
    tpPlatformMapLogicalToFaceplatePort
    tpPlatformIsCPUPort

    tpPlatformGetVLANRange

    tpPlatformGetLAGRange
    tpPlatformMapLogicalToGlobalLAG
    tpPlatformMapGlobalToLogicalLAG

    FM_TP_LOCAL_PORTS
    FM_TP_REMOTE_PORTS
    FM_TP_LAGS
    FM_TP_LBGS
    FM_TP_MCAST_GROUPS
    FM_TP_ALL_LOGICAL_PORTS
);

###############################################################################
#
#   SWITCH RELATED PLATFORM FUNCTIONS
#
###############################################################################

##@cmethod public int[] tpPlatformGetSwitchRange(void)
#
# @desc         Retrieves the minimum and maximum switch numbers
#
# @return       The (minimum switch, maximum switch) tuple
sub tpPlatformGetSwitchRange
{
    my ($self) = @_;

    return (min($self->tpGetPhysicalSwitches), max($self->tpGetPhysicalSwitches));
}

###############################################################################
#
#   PORT RELATED PLATFORM FUNCTIONS
#
###############################################################################

##@cmethod public int[] tpPlatformGetPortRange(void)
#
# @desc         Retrieves the minimum and maximum system wide logical port
#               numbers
#
# @return       The system wide (minimum logical port, maximum logical port)
#               tuple
sub tpPlatformGetPortRange
{
    return (0, 24);
}

##@cmethod public int[] tpPlatformGetFaceplatePortRange(void)
#
# @desc         Retrieves the minimum and maximum faceplate port numbers
#
# @return       The system wide (minimum logical port, maximum logical port)
#               tuple
sub tpPlatformGetFaceplatePortRange
{
    return (0, 24);
}

##@cmethod public char * tpPlatformGetSwitchExternalPortList(int switchNum)
#
# @desc         Retrieves the list of external ports that appears on a particular
#               switch.
#
# @param[in]    switchNum is the switch number for which to retrieve the port
#               list.
#
# @param[in]    facePlate should be set to TRUE if the returned port list should
#               be filtered to faceplate ports only.
#
# @return       A list of ports.
sub tpPlatformGetSwitchExternalPortList
{
    return (1..24);
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
    return (0..24);
}

##@cmethod public char * tpPlatformGetLogicalPortList(int        switchNum, 
#                                                     string     typeVector)
#
# @desc         Retrieves the list of logical ports that appear on a particular
#               switch.
#
# @param[in]    switchNum is the switch number for which to retrieve the
#               logical port list.
#
# @param[in]    typeVector The types of logical ports that should be listed:
#                  local :  Return the local ports in the switch
#                  remote:  Return the remote ports in the switch
#                  lag   :  Return the lag logical ports numbers
#                  lbg   :  Return the lbg logical ports numbers
#                  mcast :  Return the multicast logical port numbers
#                  mult  :  Return the multicast logical port numbers
#               These can be combined such as "local,lag,multicast" will return
#               the logical port numbers for local pors, lags and multicast
#               Punctuation is just for readability                
#
# @return       A list of logical ports.
sub tpPlatformGetLogicalPortList
{
    my ($self, $sw, $typeVector) = @_;
    my @list = ();
    if (!defined($typeVector))
    {
        return $self->tpPlatformGetSwitchPortList($sw);
    }
    if ((lc($typeVector) eq "all") || ($typeVector =~ /local/i))
    {
        push(@list,$self->tpPlatformGetSwitchPortList($sw)); 
    }
    my $chip = $self->{CHIP};
    if ((lc($typeVector) eq "all") || ($typeVector =~ /lag/i))
    {
        my $lagNumbers = [(0) x $FM_MAX_NUM_LAGS];
        my @portNumbers = ();
        my ($nLAG);
    
        $chip->fmGetLAGListExt($sw, \$nLAG, $lagNumbers,
                               $FM_MAX_NUM_LAGS);
        for (my $i = 0; $i < $nLAG; $i++)
        {
            my $num = -1;
            $chip->fmLAGNumberToLogicalPort($sw, $lagNumbers->[$i],\$num);
            push(@portNumbers, $num);
        }
        push(@list,@portNumbers);
    }
    return @list;
}


##@cmethod public int[] tpPlatformMapLogicalToGlobalPort(int    switchNum,
#                                                        int    logicalPort)
#
# @desc         Reverse maps a (switch, logical port) tuple to a system wide
#               logical port
#
# @param[in]    switchNum The switch number of the (switch, logical port) tuple
#
# @param[in]    logicalPort The logical port number of the
#               (switch, logical port) tuple
#
# @return       The system wide logical port number if successful
# @return       -1 if the (switch, logical port) tuple cannot be mapped to a
#               sytem wide logical port
sub tpPlatformMapLogicalToGlobalPort
{
    my ($self, $switchNum, $logicalPort) = @_;

    my ($minimum, $maximum) = $self->tpPlatformGetPortRange();
    for (my $globalPort = $minimum; $globalPort <= $maximum; $globalPort++)
    {
        my ($sw, $port) = $self->tpPlatformMapGlobalToLogicalPort($globalPort);
        if ($sw == $switchNum && $port == $logicalPort)
        {
            return $globalPort;
        }
    }
    return -1;
}

##@cmethod public void int[] tpPlatformMapGlobalToLogicalPort(int globalPort)
#
# @desc         Reverse maps a system wide logical port to a
#               (switch, logical port) tuple
#
# @param[in]    globalPort The system wide logical port to be mapped to
#               (switch, logical port) tuple
#
# @return       The (switch, logical port) tuple if successful
# @return       (-1, -1) tuple if the system wide logical port cannot be
#               mapped to a (switch, logical port) tuple
sub tpPlatformMapGlobalToLogicalPort
{
    my ($self, $globalPort) = @_;

    my @switchList = $self->tpGetSwitches();

    return ($switchList[0], $globalPort);
}

##@cmethod public int tpPlatformMapLogicalToFaceplatePort(int   switchNum,
#                                                         int   logicalPort)
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

##@cmethod public bool tpPlatformIsCPUPort(int globalPort)
#
# @desc         Determines whether the specified system wide port is a CPU port
#
# @param[in]    globalPort The system wide port number to be tested
#
# @return       TRUE if the system wide port is a CPU port
# @return       FALSE otherwise
sub tpPlatformIsCPUPort
{
    my ($self, $globalPort) = @_;

    return ($globalPort == 0 ? $TRUE : $FALSE);
}

###############################################################################
#
#   PORT MIRRORING RELATED PLATFORM FUNCTIONS 
#
#   Port mirroring could be platform specific in the future in that some
#   advanced platforms may required larger numbers of groups.
#
###############################################################################

##@cmethod public int[] tpPlatformGetMirrorRange(void)
#
# @desc         Retrieves the minimum and maximum Group ID range for Port
#               Mirroring
#
# @return       The (mimimum Group Id, maximum Group Id) tuple
sub tpPlatformGetMirrorRange
{
    return (0, 3);
}

###############################################################################
#
#   VLAN RELATED PLATFORM FUNCTIONS
#
###############################################################################

##@cmethod public int[] tpPlatformGetVLANRange(void)
#
# @desc         Retrieves the minimum and maximum VLAN IDs
#
# @return       The (mimimum VLAN ID, maximum VLAN ID) tuple
sub tpPlatformGetVLANRange
{
    return (0, 4095);
}

###############################################################################
#
#   LINK-AGGREGRATION GROUP RELATED PLATFORM FUNCTIONS
#
###############################################################################

##@cmethod public int[] tpPlatformGetLAGRange(void)
#
# @desc         Retrieves the minimum and maximum system wide logical LAG
#               numbers
#
# @return       The system wide (minimum logical LAG, maximum logical LAG)
#               tuple
sub tpPlatformGetLAGRange
{
    return (0, $FM_MAX_NUM_LAGS);
}

##@cmethod public int tpPlatformMapLogicalToGlobalLAG(int   switchNum,
#                                                     int   logicalLag)
#
# @desc         Reverse maps a (switch, logical LAG) tuple to a system wide
#               logical LAG number
#
# @param[in]    switchNum The switch number of the (switch, logical LAG) tuple
#
# @param[in]    logicalLag The logical LAG number of the (switch, logical LAG)
#               tuple
#
# @return       The system wide logical LAG number if successful
# @return       -1 if the (switch, logical LAG) tuple cannot be mapped to a
#               system wide logical LAG number
sub tpPlatformMapLogicalToGlobalLAG
{
    my ($self, $switchNum, $logicalLag) = @_;

    return $logicalLag;
}

##@cmethod public int[] tpPlatformMapGlobalToLogicalLAG(int globalLag)
#
# @desc         Maps a system wide logical LAG number to a
#               (switch, logical LAG) tuple
#
# @param[in]    globalLag The system wide logical LAG number to be mapped to a
#               (switch, logical LAG) tuple
#
# @return       The (switch, logical LAG) tuple if successful
# @return       (-1, -1) tuple if the system wide logical LAG number cannot be
#               mapped to a (switch, logical LAG) tuple
sub tpPlatformMapGlobalToLogicalLAG
{
    my ($self, $globalLag) = @_;

    return (0, $globalLag);
}

##@cmethod public void tpPlatformExit(void)
#
# @desc         Function calls prior to exiting TP
sub tpPlatformExit
{
    
}

1;
