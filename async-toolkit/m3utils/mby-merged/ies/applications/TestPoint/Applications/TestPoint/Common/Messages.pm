# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/Messages.pm
# Creation Date:    09/26/07
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

package Applications::TestPoint::Common::Messages;
use strict;
use warnings;

use base qw(Exporter);

###############################################################################
#
#   SWITCH RELATED MESSAGES
#
###############################################################################

our $TP_MSG_ERR_SWITCH_INVALID_ARRAY    =
    "Must specify a valid switch or switch range!\n";

###############################################################################
#
#   PORT RELATED MESSAGES
#
###############################################################################

our $TP_MSG_ERR_PORT_INVALID_SCALAR     =
    "Must specify a valid port!\n";
our $TP_MSG_ERR_PORT_INVALID_ARRAY      =
    "Must specify a valid port or port range!\n";

###############################################################################
#
#   MAC ADDRESS RELATED MESSAGES
#
###############################################################################

our $TP_MSG_ERR_MAC_INVALID_ADDRESS     =
    "Must specify a valid MAC address!\n";

###############################################################################
#
#   VLAN RELATED MESSAGES
#
###############################################################################

our $TP_MSG_ERR_VLAN_INVALID_SCALAR     =
    "Must specify a valid VLAN ID!\n";
our $TP_MSG_ERR_VLAN_INVALID_ARRAY      =
    "Must specify a valid VLAN ID or VLAN ID range!\n";
our $TP_MSG_ERR_VLAN_INVALID_ID         =
    "Must specify an existing VLAN ID!\n";

our $TP_MSG_WARN_VLAN_INVALID_ID        =
    "VLAN %d does not exist\n";
our $TP_MSG_WARN_VLAN_INVALID_COUNTER   =
    "No counters associated with VLAN %d\n";

our $TP_MSG_ERR_VLAN_INVALID_PRI        =
    "Must specify a valid VLAN priority!\n";

###############################################################################
#
# TRAPPING RELATED MESSAGES
#
###############################################################################

our $TP_MSG_ERR_TRAPPING_INVALID_CLASS  =
    "Must specify a valid trap class!\n";
our $TP_MSG_ERR_TRAPPING_INVALID_ATTR   =
    "Must specify a valid trap class attribute!\n";
our $TP_MSG_ERR_TRAPPING_INVALID_PARAM  =
    "Must specify a valid trap class parameter!\n";
    
###############################################################################
#
#   PORT MIRRORING RELATED MESSAGES
#
###############################################################################

our $TP_MSG_ERR_PMIR_INVALID_GROUP      =
    "Must specify a valid Port Mirroring Group ID!\n";
our $TP_MSG_ERR_PMIR_INVALID_ARRAY      =
    "Must specify a valid Group Id or Group ID range!\n";
our $TP_MSG_ERR_PMIR_INVALID_ID         =
    "Must specify an existing Port Mirroring Group ID!\n";
our $TP_MSG_ERR_PMIR_INVALID_TYPE       =
    "Must specify a valid Port Mirroring Type!\n";  
our $TP_MSG_WARN_PMIR_INVALID_ID        =
    "Port Mirroring Group ID %d does not exist\n";


###############################################################################
#
#   SPANNING TREE RELATED MESSAGES
#
###############################################################################

our $TP_MSG_ERR_STP_INVALID_ARRAY       =
    "Must specify a valid STP instance or instance range!\n";

###############################################################################
#
#   LINK-AGGREGATION GROUP RELATED MESSAGES
#
###############################################################################

our $TP_MSG_ERR_LAG_INVALID_SCALAR      =
    "Must specify a valid LAG ID!\n";
our $TP_MSG_ERR_LAG_INVALID_ARRAY       =
    "Must specify valid LAG ID or LAG ID range!\n";
our $TP_MSG_ERR_LAG_INVALID_ID          =
    "Must specify an existing LAG ID!\n";

our $TP_MSG_WARN_LAG_INVALID_ID         =
    "LAG %d is not an existing LAG\n";

###############################################################################
#
#   PACKET SEND / RECEIVE RELATED MESSAGES
#
###############################################################################

our $TP_MSG_ERR_PKT_INVALID_DMAC        =
    "Must specify a valid destination MAC address!\n";
our $TP_MSG_ERR_PKT_INVALID_SMAC        =
    "Must specify a valid source MAC address!\n";

###############################################################################
#
#   ROUTING RELATED MESSAGES
#
###############################################################################

our $TP_MSG_ERR_ROUTING_INVALID_ADDR        =
    "Must specify a valid IP address!\n";
our $TP_MSG_ERR_ROUTING_INVALID_INTERFACE   =
    "Must specify a valid interface!\n";
our $TP_MSG_ERR_ROUTING_INVALID_NEXTHOP     =
    "Must specify a valid gateway!\n";
our $TP_MSG_ERR_ROUTING_INVALID_VRID        =
    "Must specify a valid virtual router ID!\n";

###############################################################################
#
#   MULTICAST RELATED MESSAGES
#
###############################################################################

our $TP_MSG_ERR_MCAST_BAD_IP_VERSION        =
    "Cannot mix IP version 4 and 6 destination and source addresses!\n";
our $TP_MSG_ERR_MCAST_INVALID_ARRAY         =
    "Must specify a valid multicast group or multicast group range!\n";
our $TP_MSG_ERR_MCAST_INVALID_VLAN          =
    "Must specify a valid (VLAN ID, prefix) pair in the form x/y!\n";

###############################################################################
#
#   STORM CONTROLLER RELATED MESSAGES
#
###############################################################################

our $TP_MSG_ERR_STORM_CTRL_INVALID_ATTR     =
    "Must specify a valid storm controller attribute!\n";
our $TP_MSG_ERR_STORM_CTRL_INVALID_CTRL     =
    "Must specify a valid storm controller!\n";
our $TP_MSG_ERR_STORM_CTRL_INVALID_COND     =
    "Must specify a valid storm controller condition!\n";
our $TP_MSG_ERR_STORM_CTRL_INVALID_ACTION   =
    "Must specify a valid storm controller action!\n";
our $TP_MSG_ERR_STORM_CTRL_INVALID_PARAM   =
    "Must specify a valid storm controller parameter!\n";

###############################################################################
#
#   CSR RELATED MESSAGES
#
###############################################################################

our $TP_MSG_ERR_CSR_GLOBAL_INVALID          =
    "Must specify valid global register!\n";
our $TP_MSG_ERR_CSR_GLOBAL_INVALID_NAME     =
    "Global register access only supports named registers!\n";
our $TP_MSG_ERR_CSR_INDEXED_INVALID         =
    "Must specify valid global indexed register!\n";
our $TP_MSG_ERR_CSR_INDEXED_INVALID_NAME    =
    "Index based register access only supports named registers!\n";
our $TP_MSG_ERR_CSR_PORT_INVALID            =
    "Must specify valid port indexed register!\n";
our $TP_MSG_ERR_CSR_PORT_INVALID_NAME       =
    "Port based register access only supports named registers!\n";
our $TP_MSG_ERR_CSR_INDEX_INVALID_ARRAY     =
    "Must specify a valid register index!\n";
our $TP_MSG_ERR_CSR_INVALID_BIT             =
    "Must specify valid <bit> value!\n";

###############################################################################
#
#   STREAM RELATED MESSAGES
#
###############################################################################

our $TP_MSG_ERR_STREAM_INVALID_ID           =
    "Must specify valid stream ID!\n";

###############################################################################
#
#   SFLOW RELATED MESSAGES
#
###############################################################################

our $TP_MSG_ERR_SFLOW_INVALID_ID            =
    "Must specify a valid sFlow id!\n";
our $TP_MSG_ERR_SFLOW_INVALID_TYPE          =
    "Must specify a valid sFlow type!\n";
our $TP_MSG_ERR_SFLOW_INVALID_ATTR          =
    "Must specify a valid sFlow attribute!\n";
our $TP_MSG_ERR_SFLOW_INVALID_PARAM         =
    "Must specify a valid sFlow parameter!\n";

###############################################################################
#
#   UPDATE RELATED MESSAGES
#
###############################################################################
our $TP_MSG_ERR_UPDATE_SCRIPT_NOT_FOUND         =
    "Can not find startup script!\n";
our $TP_MSG_ERR_UPDATE_SCRIPT_INVALID_VERSION   =
    "Feature not available: startup script must be 2.0.0 or later!\n";
our $TP_MSG_ERR_UPDATE_INVALID_TARBALL_NAME     =
    "Must specify a valid tarball name!\n";
our $TP_MSG_ERR_UPDATE_INVALID_IP_ADDR          =
    "Must specify a valid TFTP server IP address!\n";
our $TP_MSG_ERR_UPDATE_INVALID_TARBALL_NUMBER   =
    "Must specify a valid tarball reference number!\n";

###############################################################################
#
#   VIRTUAL NETWORK RELATED MESSAGES
#
###############################################################################

our $TP_MSG_ERR_VN_TUNNEL_INVALID_ARRAY     =
    "Must specify a valid virtual network tunnel or virtual network tunnel range!\n";


BEGIN
{
    our @EXPORT = qw();
    no strict 'refs';
    foreach my $entry (sort(keys(%{__PACKAGE__ . "::"})))
    {
        if ($entry =~ m/^TP_MSG/)
        {
            my $symbol = $entry;
            $symbol =~ s/^/\$/;
            push(@EXPORT, $symbol);
        }
    }
}

1;
