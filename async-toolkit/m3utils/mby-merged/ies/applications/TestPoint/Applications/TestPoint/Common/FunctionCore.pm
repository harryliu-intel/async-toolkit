# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FunctionCore.pm
# Creation Date:    04/06/07
# Description:      Common SDK code calls to be shared by all platform
#                   implementations for TestPoint.
#
# INTEL CONFIDENTIAL
# Copyright 2007 - 2012 Intel Corporation. All Rights Reserved. 
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

package Applications::TestPoint::Common::FunctionCore;
use strict;
use warnings;

use base qw(
    Exporter
    Applications::TestPoint::Common::Version
    Applications::TestPoint::Common::Utilities
    Applications::TestPoint::Common::MACCore
    Applications::TestPoint::Common::VLANCore
    Applications::TestPoint::Common::LAGCore
    Applications::TestPoint::Common::HashCore
    Applications::TestPoint::Common::LoggingCore
    Applications::TestPoint::Common::LatencyCore
    Applications::TestPoint::Common::MulticastCore
    Applications::TestPoint::Common::RoutingCore
    Applications::TestPoint::Common::WatermarkCore
    Applications::TestPoint::Common::SchedulerCore
    Applications::TestPoint::Common::ShapingCore
    Applications::TestPoint::Common::StormControllerCore
    Applications::TestPoint::Common::SwitchCore
    Applications::TestPoint::Common::LBGCore
    Applications::TestPoint::Common::CNCore
    Applications::TestPoint::Common::AclCore
    Applications::TestPoint::Common::PolicerCore
    Applications::TestPoint::Common::MapperCore
    Applications::TestPoint::Common::SpanningTreeCore
    Applications::TestPoint::Common::MirrorCore
    Applications::TestPoint::Common::PriorityCore
    Applications::TestPoint::Common::FatTreeCore
    Applications::TestPoint::Common::StackingCore
    Applications::TestPoint::Common::FibmCore
    Applications::TestPoint::Common::StreamCore
    Applications::TestPoint::Common::SFlowCore
    Applications::TestPoint::Common::CVlanCore
    Applications::TestPoint::Common::QCNCore
    Applications::TestPoint::Common::PacketCaptureCore
    Applications::TestPoint::Common::UpdateCore
    Applications::TestPoint::Common::LldpCore
    Applications::TestPoint::Common::DcbxCore
    Applications::TestPoint::Common::QoSCore
    Applications::TestPoint::Common::RBridgeCore
    Applications::TestPoint::Common::VNCore
);

#NOTE: family specific packages are loaded when new switches are inserted
our @fm2000Packages = qw(
    Applications::TestPoint::Common::FM2000::MACCore
    Applications::TestPoint::Common::FM2000::PriorityCore
    Applications::TestPoint::Common::FM2000::WatermarkCore
    Applications::TestPoint::Common::FM2000::QoSCore
);

our @fm4000Packages = qw(
    Applications::TestPoint::Common::FM4000::CNCore
    Applications::TestPoint::Common::FM4000::MACCore
    Applications::TestPoint::Common::FM4000::LoadBalancingCore
    Applications::TestPoint::Common::FM4000::PriorityCore
    Applications::TestPoint::Common::FM4000::SchedulerCore
    Applications::TestPoint::Common::FM4000::ShapingCore
    Applications::TestPoint::Common::FM4000::WatermarkCore
    Applications::TestPoint::Common::FM4000::HashCore
    Applications::TestPoint::Common::FM4000::AclSimpleCore
    Applications::TestPoint::Common::FM4000::SwitchCore
    Applications::TestPoint::Common::FM4000::QCNCore
    Applications::TestPoint::Common::FM4000::QoSCore
);

#FIXME: FM4000:HashCore is needed for ECMPHash
our @fm6000Packages = qw(
    Applications::TestPoint::Common::FM6000::MACCore
    Applications::TestPoint::Common::FM4000::HashCore
    Applications::TestPoint::Common::FM6000::HashCore
    Applications::TestPoint::Common::FM6000::PriorityCore
    Applications::TestPoint::Common::FM6000::RegisterUtilCore
    Applications::TestPoint::Common::FM6000::SchedulerCore
    Applications::TestPoint::Common::FM6000::ShapingCore
    Applications::TestPoint::Common::FM6000::WatermarkCore
    Applications::TestPoint::Common::FM6000::SwitchCore
    Applications::TestPoint::Common::FM6000::QoSCore
);

#use base qw(
#    Applications::TestPoint::Common::LoadBalancingCore
#);

use List::Util qw(sum);
use Math::BigFloat;
use Time::HiRes qw(gettimeofday usleep tv_interval);
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Applications::TestPoint::Common::LoggingCore;
use Scripts::Utilities;
our @EXPORT = qw(
    handleSetPort
    handleShowRxForwarding
    handleSetPortSpeed
    handleSetPortConfig
    handleShowPort
    handleShowPortmap
    handleShowPortConfig
    handleSetSwitchState
    handleSetSwitchConfig
    handleShowSwitchConfig
    handleShowSwitchStats
    handleShowStatsAll
    handleShowRxTypes
    handleShowRxPriorities
    handleShowTxPriorities
    handleShowRxSizes
    handleShowTxTypes
    handleShowTxSizes
    handleShowVersion
    handleShowPlatform
    handleResetSwitch
    handleResetSwitchStats
    handleResetPortStats
    handleSendPacket
    handleReceivePacket
    handleI2CRead
    handleI2CWrite
    handleI2CWriteRead
    handleTestLED
    handleTestMap
    handleSetVoltage
    handleTestClean
    handleTestBurninStart
    handleTestBurninCheck
    handleTestBurninStop
    handleMdioRead
    handleMdioWrite
    tpHandleConfigARPSnooping
    tpHandleConfigIGMPSnooping
    tpInitAttributes
    buildMacAddress
);

############################# Generic Helpers ################################


# Note that chip specific attributes should be added in their respective list.
# Those other lists are in FMXXXX/FunctionCore.pm. Doing this ensures that
# chip specific attributes are only displayed if it applies.
# Chip specific lists are initialized in tpInitAttributes.
# 
# Attributes in this list should be common to all chip families
our %port_attr_map = (
    'min_frame_size'            => $FM_PORT_MIN_FRAME_SIZE,
    'max_frame_size'            => $FM_PORT_MAX_FRAME_SIZE,
    'security'                  => $FM_PORT_SECURITY,
    'security_trap'             => $FM_PORT_SECURITY_TRAP,
    'learning'                  => $FM_PORT_LEARNING,
    'tagging'                   => $FM_PORT_TAGGING,
    'tagging2'                  => $FM_PORT_TAGGING2,
    'pvid'                      => $FM_PORT_DEF_VLAN,
    'pri'                       => $FM_PORT_DEF_PRI,
    'cfi'                       => $FM_PORT_DEF_CFI,
    'switch-pri'                => $FM_PORT_DEF_SWPRI,
    'dscp'                      => $FM_PORT_DEF_DSCP,
    'replace_dscp'              => $FM_PORT_REPLACE_DSCP,
    'update_dscp'               => $FM_PORT_UPDATE_DSCP,
    'update_cfi'                => $FM_PORT_TXCFI,
    'update_pri'                => $FM_PORT_TXVPRI,
    'parser_cfg'                => $FM_PORT_PARSER,
    'drop_bv'                   => $FM_PORT_DROP_BV,
    'drop_management_isl'       => $FM_PORT_DROP_MANAGEMENT_ISL,
    'drop_untagged'             => $FM_PORT_DROP_UNTAGGED,
    'drop_tagged'               => $FM_PORT_DROP_TAGGED,
    'loopback'                  => $FM_PORT_LOOPBACK,
    'fabric_loopback'           => $FM_PORT_FABRIC_LOOPBACK,
    'tx_pause_time'             => $FM_PORT_TX_PAUSE,
    'pause_mode'                => $FM_PORT_TX_PAUSE_MODE,
    'rx_pause_en'               => $FM_PORT_RX_PAUSE,
    'tx_pause_en'               => 0,
    'dic'                       => $FM_PORT_DIC_ENABLE,
    'mask'                      => $FM_PORT_MASK,
    'equalizer'                 => 0,
    'routing'                   => $FM_PORT_ROUTABLE,
    'ifg'                       => $FM_PORT_IFG,
    'capture'                   => 0,
    'isl'                       => $FM_PORT_ISL_TAG_FORMAT,
    'update_routed'             => $FM_PORT_UPDATE_ROUTED_FRAME,
    'loopback_suppress'         => $FM_PORT_LOOPBACK_SUPPRESSION,
    'parser_flag'               => $FM_PORT_PARSER_FLAG_OPTIONS,
    'autoneg'                   => $FM_PORT_AUTONEG,
    'serdes_emphasis'           => $FM_PORT_EMPHASIS,
    'serdes_drive'              => $FM_PORT_DRIVE_STRENGTH,
    'ignore_ifg_errors'         => $FM_PORT_IGNORE_IFG_ERRORS,
    'mcast_flooding'            => $FM_PORT_MCAST_FLOODING,
    'autoneg_basepage'          => $FM_PORT_AUTONEG_BASEPAGE,
    'autoneg_partner_basepg'    => $FM_PORT_AUTONEG_PARTNER_BASEPAGE,
    'autoneg_ignore_nonce'      => $FM_PORT_AUTONEG_IGNORE_NONCE,
    'rx_cut_through'            => $FM_PORT_RX_CUT_THROUGH,
    'tx_cut_through'            => $FM_PORT_TX_CUT_THROUGH,
    'tx_tc_private_wm'          => $FM_QOS_TX_TC_PRIVATE_WM,
    'rx_lane_polarity'          => $FM_PORT_RX_LANE_POLARITY,
    'rx_lane_ordering'          => $FM_PORT_RX_LANE_ORDERING,
    'tx_lane_polarity'          => $FM_PORT_TX_LANE_POLARITY,
    'tx_lane_ordering'          => $FM_PORT_TX_LANE_ORDERING,
    'pause_resend_time'         => $FM_PORT_TX_PAUSE_RESEND_TIME,
    'pause_mac_addr'            => $FM_PORT_MAC_ADDR,
    'update_ttl'                => $FM_PORT_UPDATE_TTL,
    'ucast_flooding'            => $FM_PORT_UCAST_FLOODING,
    'mcast_pruning'             => $FM_PORT_MCAST_PRUNING,
    'link_detection'            => $FM_PORT_LINK_INTERRUPT,
    'tx_clk_compensation'       => $FM_PORT_TX_CLOCK_COMPENSATION,
    'discard_frame_errors'      => $FM_PORT_RX_ERRORED_FRAME_DISCARD,
    'parse_inner'               => $FM_PORT_PARSER_UC_PARSE_INNER,
);


our %parser_flag_attr_map =
(
    'ipv4-options'      => $FM_PORT_PARSER_FLAG_IPV4_OPTIONS,
    'ipv6-auth'         => $FM_PORT_PARSER_FLAG_IPV6_AUTH,
    'ipv6-dest'         => $FM_PORT_PARSER_FLAG_IPV6_DEST,
    'ipv6-fragment'     => $FM_PORT_PARSER_FLAG_IPV6_FRAGMENT,
    'ipv6-hop-by-hop'   => $FM_PORT_PARSER_FLAG_IPV6_HOPBYHOP,
    'ipv6-routing'      => $FM_PORT_PARSER_FLAG_IPV6_ROUTING,
);

# Note that chip specific attributes may be added to this list during 
# initialization, refer to tpInitAttributes for more details
# 
# Attributes in this list should be common to all chip families
our %sw_attr_map = (
    'vlan_mode'         => $FM_VLAN_TYPE,
    'spanning-tree'     => $FM_SPANNING_TREE_MODE,
    'cpu_mac'           => $FM_CPU_MAC,
    'ip_options_disp'   => $FM_IP_OPTIONS_DISPOSITION,
    'flood_bcast'       => $FM_BCAST_FLOODING,
    'flood_mcast'       => $FM_MCAST_FLOODING,
    'frame_age_time'    => $FM_FRAME_AGING_TIME_MSEC,
    'lacp_disp'         => $FM_LAG_LACP_DISPOSITION,
    'mstp'              => $FM_MULTIPLE_SPT,
    'mtu_list'          => $FM_MTU_LIST,
    'lbg_mode'          => $FM_LBG_MODE,
    'ifg_penalty'       => $FM_IFG_PENALTY,
    'cpu_port'          => $FM_REDIRECT_CPU_TRAFFIC,
    'flood_ucast'       => $FM_UCAST_FLOODING,
    'igmp_snooping'     => 0,
    'igmp_snooping_type'      => 0,
    'qos_shared_pause_on_wm'  => $FM_QOS_SHARED_PAUSE_ON_WM,
    'qos_shared_pause_off_wm' => $FM_QOS_SHARED_PAUSE_OFF_WM,
);

our %sw_trap_attr_map = (
    'trap_security'     => $FM_TRAP_IEEE_8021X,
    'trap_bpdu'         => $FM_TRAP_IEEE_BPDU,
    'trap_lacp'         => $FM_TRAP_IEEE_LACP,
    'trap_garp'         => $FM_TRAP_IEEE_GARP,
    'trap_mtu_viol'     => $FM_TRAP_MTU_VIOLATIONS,
    'trap_plus_log'     => $FM_TRAP_PLUS_LOG,
    'trap_other'        => $FM_TRAP_IEEE_OTHER, 
);

our %mac_attr_map = (
    'soft_learning'     => $FM_MAC_SOFT_LEARNING,
    'mac_age_time'      => $FM_MAC_TABLE_ADDRESS_AGING_TIME,
    'update_report'     => $FM_MAC_TABLE_EVENT_MODE,
    'learn_threshold'   => $FM_MAC_TABLE_EVENT_LEARNING_THRESHOLD,
    'age_threshold'     => $FM_MAC_TABLE_EVENT_AGING_THRESHOLD,
    'sec_threshold'     => $FM_MAC_TABLE_EVENT_SEC_EVENT_THRESHOLD,
    'sweepPeriod'       => $FM_MAC_SWEEP_PERIOD,
);

my %phy_attr_map = (
    'vegas'    => {
        'mac_loopback' => {
            'reg'          => 0, 
            'page'         => 0, 
            'loopback'     => 1 << 14,
            'mode'         => 0,
            'dev'          => 0,
        },
        'line_loopback'   => {
            'reg'          => 21, 
            'page'         => 2, 
            'loopback'     => 1 << 14,
            'mode'         => 0,
            'dev'          => 0,
        }
    }
);

my %switch_priority_source_map = (
    'vpri'  =>  $FM_PORT_SWPRI_VPRI,
    'dscp'  =>  $FM_PORT_SWPRI_DSCP,
    'isl'   =>  $FM_PORT_SWPRI_ISL_TAG,
    'none'  =>  0,
);

my %igmp_snoop_map = (
    'dip'           => $FM_MCAST_ADDR_TYPE_DSTIP,
    'dip-sip'       => $FM_MCAST_ADDR_TYPE_DSTIP_SRCIP,
    'dip-sip-vlan'  => $FM_MCAST_ADDR_TYPE_DSTIP_SRCIP_VLAN,
    'dip-vlan'      => $FM_MCAST_ADDR_TYPE_DSTIP_VLAN,
    'dmac-vlan'     => $FM_MCAST_ADDR_TYPE_L2MAC_VLAN,
    'unknown'       => $FM_MCAST_ADDR_TYPE_UNKNOWN,
);

my %ffu_slice_cfg_map = (
    ipv4_ucast      => "ipv4Unicast",
    ipv4_mcast      => "ipv4Multicast",
    ipv6_ucast      => "ipv6Unicast",
    ipv6_mcast      => "ipv6Multicast",
    acl             => "acl",
    cvlan           => "cVlan",
);

my %port_ethMode_map = (
    'disabled'          =>  $FM_ETH_MODE_DISABLED,
    'SGMII'             =>  $FM_ETH_MODE_SGMII,
    '1000Base-X'        =>  $FM_ETH_MODE_1000BASE_X,
    '1000Base-KX'       =>  $FM_ETH_MODE_1000BASE_KX,
    '2500Base-X'        =>  $FM_ETH_MODE_2500BASE_X,
    '6GBase-KR'         =>  $FM_ETH_MODE_6GBASE_KR,
    '6GBase-CR'         =>  $FM_ETH_MODE_6GBASE_CR,
    '10GBase-KR'        =>  $FM_ETH_MODE_10GBASE_KR,
    '10GBase-CR'        =>  $FM_ETH_MODE_10GBASE_CR,
    '10GBase-SR'        =>  $FM_ETH_MODE_10GBASE_SR,
    'XAUI'              =>  $FM_ETH_MODE_XAUI,
    '10GBase-KX4'       =>  $FM_ETH_MODE_10GBASE_KX4,
    '10GBase-CX4'       =>  $FM_ETH_MODE_10GBASE_CX4,
    '24GBase-KR4'       =>  $FM_ETH_MODE_24GBASE_KR4,
    '40GBase-KR4'       =>  $FM_ETH_MODE_40GBASE_KR4,
    'XLAUI'             =>  $FM_ETH_MODE_XLAUI,
    '24GBase-CR4'       =>  $FM_ETH_MODE_24GBASE_CR4,
    '40GBase-CR4'       =>  $FM_ETH_MODE_40GBASE_CR4,
    '40GBase-SR4'       =>  $FM_ETH_MODE_40GBASE_SR4,
    'AN-73'             =>  $FM_ETH_MODE_AN_73
);

my %port_ethMode_reverse_map = reverse %port_ethMode_map;

my %randomMacs;

my $attributesInitialized = $FALSE;

# uses gettime of day to get better resolution
# than built in time
sub ftime
{
    my ($sec, $usec) = gettimeofday();

    return $sec + ($usec / 1e6);
}

##
# @desc         Returns the family of the specified switch.
# 
# @param[in]    switchNum is the switch number.
# 
# @return       Family of the specified switch.
#               FM_SWITCH_FAMILY_UNKNOWN if family is undefined
# 
sub getSwitchFamily
{
    my ($self, $switchNum) = @_;

    my $info = ($self->tpGetSwitchInfo())[$switchNum];
    my $family = $info->{'switchFamily'};

    return defined($family) ? $family : $FM_SWITCH_FAMILY_UNKNOWN;
}

##
# @desc         Initialize attributes that are chip specific.
#               Adds the missing chip specific attributes to
#               the *_attr_map hash.
# 
# @return       None
# 
sub tpInitAttributes
{
    my ($self) = @_;
    my %families;

    # Get switch selection.
    #Use tpGetPhysicalSwitches instead of tpGetSwitches for platforms
    #that manually insert switches
    my @switchList = $self->tpGetPhysicalSwitches();

    foreach my $switchNum (@switchList)
    {
        my $family = $self->getSwitchFamily($switchNum);

        if ($family == $FM_SWITCH_FAMILY_UNKNOWN)
        {
            next;
        }

        $attributesInitialized = $TRUE;

        if ($family == $FM_SWITCH_FAMILY_FM2000)
        {
            # Verify that the class object has not already been created
            if (!exists($families{$family}))
            {
                #Loading family specific packages
                foreach my $pkg (@fm2000Packages)
                {
                    #print "LOADING: $pkg\n";
                    eval("require $pkg");
                    printf("ERROR: require $pkg => $@\n") if ($@);

                    no strict "subs";
                    eval("use base $pkg");
                    printf("ERROR: use base $pkg => $@\n") if ($@);
                    use strict "subs";
                }

                require Applications::TestPoint::Common::FM2000::FunctionCore;

                $families{$family} = new Applications::TestPoint::Common::FM2000::FunctionCore();;
            }
        }

        if ($family == $FM_SWITCH_FAMILY_FM4000 || 
            $family == $FM_SWITCH_FAMILY_REMOTE_FM4000 ||
            $family == $FM_SWITCH_FAMILY_SWAG)
        {
            # Verify that the class object has not already been created
            if (!exists($families{$family}))
            {
                #Loading family specific packages
                foreach my $pkg (@fm4000Packages)
                {
                    #print "LOADING: $pkg\n";
                    eval("require $pkg");
                    printf("ERROR: require $pkg => $@\n") if ($@);

                    no strict "subs";
                    eval("use base $pkg");
                    printf("ERROR: use base $pkg => $@\n") if ($@);
                    use strict "subs";
                }

                require Applications::TestPoint::Common::FM4000::FunctionCore;

                $families{$family} = new Applications::TestPoint::Common::FM4000::FunctionCore();
            }
        }

        if ($family == $FM_SWITCH_FAMILY_FM6000 ||
            $family == $FM_SWITCH_FAMILY_REMOTE_FM6000)
        {
            # Verify that the class object has not already been created
            if (!exists($families{$family}))
            {
                #Loading family specific packages
                foreach my $pkg (@fm6000Packages)
                {
                    #print "LOADING: $pkg\n";
                    my $stp = [gettimeofday];
                    eval("require $pkg");
                    printf("ERROR: require $pkg => $@\n") if ($@);

                    no strict "subs";
                    eval("use base $pkg");
                    printf("ERROR: use base $pkg => $@\n") if ($@);
                    use strict "subs";
                }

                require Applications::TestPoint::Common::FM6000::FunctionCore;

                $families{$family} = new Applications::TestPoint::Common::FM6000::FunctionCore();
            }
        }
    }

    foreach my $key (keys(%families))
    {
        my $fc = $families{$key};
        my $portAttrHashRef = $fc->fcGetPortAttributes();
        my $swAttrHashRef = $fc->fcGetSwitchAttributes();
        my $swTrapAttrHashRef = $fc->fcGetTrappingAttributes();

        foreach my $element (keys(%$portAttrHashRef))
        {
            $port_attr_map{$element} = $portAttrHashRef->{$element};
        }

        foreach my $element (keys(%$swAttrHashRef))
        {
            $sw_attr_map{$element} = $swAttrHashRef->{$element};
        }

        foreach my $element (keys(%$swTrapAttrHashRef))
        {
            $sw_trap_attr_map{$element} = $swTrapAttrHashRef->{$element};
        }
    }
}

##
# ParseSwitchPrioritySource
# 
# @desc         Parses the switch priority sources and returns the associated 
#               numeric value.
# 
# @param[in]    $sourcesStr is a string specifying a comma-separated list
#               of <sources> keywords.
# 
# @return       Integer representation of the sources bitmask, or
#               undef if a parsing error occurred.
#               
sub ParseSwitchPrioritySource
{
    my ($sourcesStr) = @_;

    my $sources = 0;

    $sourcesStr =~ s/\s//g;

    foreach my $src (split(/,/, $sourcesStr))
    {
        $src = lc($src);

        if ($src ne "")
        {
            if (defined $switch_priority_source_map{$src})
            {
                $sources |= $switch_priority_source_map{$src};
            }
            else
            {
                printf("$src is not a valid source, valid sources are: ");
                my $result = "";
                foreach my $srcValue (keys(%switch_priority_source_map))
                {
                    $result .= "$srcValue, ";
                }
                $result =~ s/, $//;

                printf("$result \n");
                return undef;
            }
        }
    }
    return $sources;
}   

##@cmethod public void handleSetPort(int port, char *state)
#
# @desc         Generic handler for port state.  Uses the specific
#               implementation to figure out the port ranges and conversion of
#               port numbers
#
#   @param[in] port The port number to set the state for
#
#   @param[in] state The state string, "up" or "down"
sub handleSetPort
{
    my ($self, $port, $state, $subMode) = @_;
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

    goto STATE if !defined($state);

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
            $chip->disableErrors();
            if ($state eq "up")
            {
                $status = $chip->fmSetPortState($switchNum, $logicalPort,
                                                $FM_PORT_STATE_UP, 0);
            }
            elsif($state eq "down")
            {
                $status = $chip->fmSetPortState($switchNum, $logicalPort,
                                                $FM_PORT_STATE_ADMIN_DOWN, 0);
            }
            elsif($state eq "powerdown")
            {
                $status = $chip->fmSetPortState($switchNum, $logicalPort,
                                                $FM_PORT_STATE_ADMIN_PWRDOWN, 0);
            }
            elsif($state eq "fault")
            {
                $status = $chip->fmSetPortState($switchNum, $logicalPort,
                                                $FM_PORT_STATE_REMOTE_FAULT, 0);
            }
            elsif($state eq "bist")
            {
                goto SUBMODE if !defined($subMode);
    
                $self->handleSetPort($port, "up");
                if ($subMode eq "idle")
                {
                    $status = $chip->fmSetPortState($switchNum, $logicalPort,
                                                    $FM_PORT_STATE_BIST,
                                                    $FM_BIST_TX_IDLECHAR);
                }
                elsif ($subMode eq "prbs5")
                {
                    $status = $chip->fmSetPortState($switchNum, $logicalPort,
                                                    $FM_PORT_STATE_BIST,
                                                    $FM_BIST_TX_PRBS_1024);
                }
                elsif ($subMode eq "prbs6")
                {
                    $status = $chip->fmSetPortState($switchNum, $logicalPort,
                                                    $FM_PORT_STATE_BIST,
                                                    $FM_BIST_TX_PRBS_512A);
                }
                elsif ($subMode eq "prbs1")
                {
                    $status = $chip->fmSetPortState($switchNum, $logicalPort,
                                                    $FM_PORT_STATE_BIST,
                                                    $FM_BIST_TX_PRBS_512B);
                }
                elsif ($subMode eq "lowfreq")
                {
                    $status = $chip->fmSetPortState($switchNum, $logicalPort,
                                                    $FM_PORT_STATE_BIST,
                                                    $FM_BIST_TX_LOWFREQ);
                }
                elsif ($subMode eq "highfreq")
                {
                    $status = $chip->fmSetPortState($switchNum, $logicalPort,
                                                    $FM_PORT_STATE_BIST,
                                                    $FM_BIST_TX_HIGHFREQ);
                }
                elsif ($subMode eq "cjpat")
                {
                    $status = $chip->fmSetPortState($switchNum, $logicalPort,
                                                    $FM_PORT_STATE_BIST,
                                                    $FM_BIST_TX_CJPAT);
                }
                else
                {
                        goto SUBMODE;
                }
            }
            else
            {
                goto STATE;
            }
            $chip->enableErrors();
            if ($status == $FM_ERR_INVALID_SWITCH
                || $status == $FM_ERR_INVALID_PORT)
            {
                print($TP_MSG_ERR_PORT_INVALID_ARRAY);
                return $FM_ERR_INVALID_ARGUMENT;
            }
            elsif ($status != $FM_OK)
            {
                printf("Port %2d (sw $switchNum): Port state cannot be set!\n", $globalPort);
                return $FM_FAIL;
            }
            
        }   # end foreach my $globalPort (@portList)
        
    }   # end foreach my $switchNum ($self->tpGetSwitches)
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    
    return $FM_OK;

STATE:
    print("Must specify a valid state!\n");
    return $FM_ERR_INVALID_ARGUMENT;
SUBMODE:
    print("Must specify a valid BIST submode!\n");
    return $FM_ERR_INVALID_ARGUMENT;
}

##@method void handleShowRxForwarding(int port)
#
# @desc         Shows RX forwarding statistics for the given port
#
# @param[in]    port The port number to show stats for
#
# @param[in]    verbose Pass the string "verbose" to show stats equal to zero
sub handleShowRxForwarding
{
    my ($self, $port, $verbose, $showHdr) = @_;

    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }
    
    my $quiet = $TRUE;
    if (defined($verbose) && ($verbose eq "verbose"))
    {
        $quiet = undef;
    }

    my $hdr = $TRUE; #Show headers
    if (defined($showHdr) && ($showHdr == $FALSE))
    {
        $hdr = $FALSE;
    }

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, 
                                $self->tpPlatformGetSwitchPortList($switchNum));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        printf("%s", scalar($self->tpGetSwitches) > 1 ? "\nSwitch $switchNum:\n" : " ") if $hdr;
        
        foreach my $globalPort (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned sw since it should match switchNum and may actually be
            # wrong for some platforms.
            my ($sw, $logicalPort) = 
                            $self->tpPlatformMapGlobalToLogicalPort($globalPort);
            
            my $portCounters;
            $chip->disableErrors();
            my $status =
                $chip->fmGetPortCounters($switchNum, $logicalPort, \$portCounters);
            $chip->enableErrors();
            if ($status != $FM_OK)
            {
                printf("RX forwarding statistics cannot be shown for port %d!\n",
                       $globalPort);
                next;
            }

            my $total = Math::BigInt->new(0);
            if ($hdr)
            {
                print("\n");
                printf("RX Forwarding Stats (Group 6): Port %2d\n", $globalPort);
                print("-------------------------------------------------------------\n");
            }

            $total += $self->printCounter($portCounters->{cntFIDForwardedPkts},
                                          "Forwarded",$quiet);
            $total += $self->printCounter($portCounters->{cntFloodForwardedPkts},
                                          "Flooded",$quiet);
            $total += $self->printCounter($portCounters->{cntVLANTagDropPkts},
                                          "VLAN Tag Drops",$quiet);
            $total += $self->printCounter($portCounters->{cntVLANIngressBVPkts},
                                          "VLAN Ingress Violations",$quiet);
            $total += $self->printCounter($portCounters->{cntVLANEgressBVPkts},
                                          "VLAN Egress Violations",$quiet);
            $total += $self->printCounter($portCounters->{cntSecurityViolationPkts},
                                          "Security Violations",$quiet);
    
            # FM2xxx Only
            if (($portCounters->{cntVersion} & 1) > 0)
            {
                $total += $self->printCounter($portCounters->{cntTriggerMirroredPkts},
                                              "Trigger Mirrors",$quiet);
                $total += $self->printCounter($portCounters->{cntDLFDropPkts},
                                              "Flood Disable Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntBroadcastDropPkts},
                                              "Broadcast Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntReservedTrapPkts},
                                              "Reserved Traps",$quiet);
                $total += $self->printCounter($portCounters->{cntRxCMDropPkts},
                                              "RX Congestion Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntSTPDropPkts},
                                              "STP Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntTriggerDropRedirPkts},
                                              "Trigger Drop/Redirects",$quiet);
            }
    
            # FM4xxx Only
            if (($portCounters->{cntVersion} & 0x4) > 0)
            {
                $total += $self->printCounter($portCounters->{cntTrappedPkts},
                                              "Trapped",$quiet);
                $total += $self->printCounter($portCounters->{cntGlortMissDropPkts},
                                              "Glort Miss Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntFFUDropPkts},
                                              "ACL Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntPolicerDropPkts},
                                              "Policer Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntTTLDropPkts},
                                              "TTL Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntPauseDropPkts},
                                              "PAUSE Sunk",$quiet);
                $total += $self->printCounter($portCounters->{cntSpeciallyHandledPkts},
                                              "Specially Handled",$quiet);
                $total += $self->printCounter($portCounters->{cntParseErrDropPkts},
                                              "Parse Error Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntParityErrorPkts},
                                              "Parity Error Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntCmPrivDropPkts},
                                              "CM Global WM Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntSmp0DropPkts},
                                              "CM SMP0 Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntSmp1DropPkts},
                                              "CM SMP1 Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntRxHog0DropPkts},
                                              "CM RX Hog0 Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntRxHog1DropPkts},
                                              "CM RX Hog1 Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntTxHog0DropPkts},
                                              "CM TX Hog0 Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntTxHog1DropPkts},
                                              "CM TX Hog1 Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntRateLimit0DropPkts},
                                              "CM Rate Limit0 Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntRateLimit1DropPkts},
                                              "CM Rate Limit1 Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntBadSmpDropPkts},
                                              "CM Bad SMP Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntSTPDropPkts},
                                              "STP Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntTriggerDropRedirPkts},
                                              "Trigger Drop/Redirects",$quiet);
            }

            # FM6xxx Only
            if (($portCounters->{cntVersion} & 0x8) > 0)
            {
                $total += $self->printCounter($portCounters->{cntTrappedPkts},
                                              "Trapped",$quiet);
                $total += $self->printCounter($portCounters->{cntSTPIngressDropsPkts},
                                              "STP Ingress Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntSTPEgressDropsPkts},
                                              "STP Egress Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntGlortSwitchedPkts},
                                              "Glort Switched",$quiet);
                $total += $self->printCounter($portCounters->{cntTTLDropPkts},
                                              "TTL Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntGlortRoutedPkts},
                                              "Glort Routed",$quiet);
                $total += $self->printCounter($portCounters->{cntReservedDropPkts},
                                              "Reserved MAC Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntLoopbackDropsPkts},
                                              "Loopback Suppression Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntL2ARDropPkts},
                                              "L2AR Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntParseErrDropPkts},
                                              "Parse Error Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntParityErrorPkts},
                                              "Parity Error Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntFFUDropPkts},
                                              "ACL Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntPolicerDropPkts},
                                              "Policer Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntFloodControlDropPkts},
                                              "Flood Control Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntGlobalWMDropPkts},
                                              "CM Global Watermark Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntRXMPDropPkts},
                                              "CM RX Memory Partition Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntRxHogDropPkts},
                                              "CM RX Hog Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntTxHogDropPkts},
                                              "CM TX Hog Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntOtherPkts},
                                              "Other Packets",$quiet);
            }

            if ($hdr)
            {
                my $totalstr = "                                     Total";
                if (defined($quiet))
                {
                    $totalstr = "          Total (Empty Counters Not Shown)";
                }
                $self->printCounter($total, $totalstr);
            }
        }
    }
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList) if $hdr;
}

##@cmethod public void handleSetPortSpeed(int port, char *speed)
#
# @desc         Handles setting the speed a port operates at
#
# @param[in]    port The set of ports whose speed settings are to be modified
#
# @param[in]    speed The speed the set of ports is to operate at
sub handleSetPortSpeed
{
    my ($self, $port, $speed) = @_;

    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    if (!defined($speed) || !("10G 6G 2.5G 1G 100M 10M Auto-SGMII Auto-Clause37" =~ m/$speed/i))
    {
        print("Must specify a valid speed!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my %void = (type => "fm_uint32", value => undef);
    if ($speed eq "10G")
    {
        $void{value} = 10000;
    }
    elsif ($speed eq "6G")
    {
        $void{value} = 6000;
    }
    elsif ($speed eq "2.5G")
    {
        $void{value} = 2500;
    }
    elsif (($speed eq "1G") || ($speed =~ m/^Auto/i))
    {
        $void{value} = 1000;
    }
    elsif ($speed eq "100M")
    {
        $void{value} = 100;
    }
    elsif ($speed eq "10M")
    {
        $void{value} = 10;
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
            my ($sw, $logicalPort) = $self->tpPlatformMapGlobalToLogicalPort($globalPort);
            
            next if $self->tpPlatformIsCPUPort($globalPort);

            my $switchFamily = $self->getSwitchFamily($switchNum);
    
            if ( ($switchFamily == $FM_SWITCH_FAMILY_FM2000) &&
                 ($speed eq "Auto") )
            {
                printf("Unable to set speed to $speed for port %d\n", $globalPort);
                next;
            }
    
            $chip->disableErrors();
            my $status = $chip->fmSetPortAttribute($switchNum, $logicalPort,
                                                   $FM_PORT_SPEED, \%void);
            $chip->enableErrors();
            if ($status != $FM_OK)
            {
                printf("Port speed cannot be set for port %d!\n", $globalPort);
                next;
            }
    
            # Only do auto-negotiation stuff on Bali (not supported on Tahoe).
            if ( ($switchFamily != $FM_SWITCH_FAMILY_FM2000) ) {
    
                my %void_autoneg = (type => "fm_uint32", value => undef);
    
                if ($speed =~ m/^Auto-SGMII/i)
                {
                    $void_autoneg{value} = $FM_PORT_AUTONEG_SGMII;
                } 
                elsif ($speed =~ m/^Auto-Clause37/i)
                {
                    $void_autoneg{value} = $FM_PORT_AUTONEG_CLAUSE_37;
                } 
                else 
                {
                    $void_autoneg{value} = $FM_PORT_AUTONEG_NONE;
                }
    
                $status = $chip->fmSetPortAttribute($switchNum, $logicalPort,
                        $FM_PORT_AUTONEG, \%void_autoneg);
    
                if ($status != $FM_OK)
                {
                    printf("Port speed cannot be set for port %d!\n", $globalPort);
                    next;
                }
            }
            
        }   # end foreach my $globalPort (@portList)
        
    }   # end foreach my $switchNum ($self->tpGetSwitches)
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    
}


##@cmethod public void handleSetPortConfig(int port, char* attribute, void* value)
#
# @desc         Handles setting the configuration parameters of a port
#               Can be a LAG logical port.   
#
# @param[in]    port The set of ports whose config settings are to be modified
#
# @param[in]    attribute 
#
# @param[in]    value 
#
sub handleSetPortConfig
{
    my ($self, $port, $attribute, $value, @arguments) = @_;

    if ($attributesInitialized == $FALSE)
    {
        $self->tpInitAttributes();
    }

    my $chip = $self->{CHIP};

    if (!defined($port))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    if (!defined($attribute) || !exists($port_attr_map{$attribute}))
    {
        print("Must specify a valid port attribute!\n");
        return;
    }

    if (!defined($value))
    {
        print("Must specify an attribute value!\n");
        return;
    }

    
    $chip->disableErrors();

    my $perLagMgmt =
            $chip->fmGetBoolApiAttribute($SDK::FM_AAK_API_PER_LAG_MANAGEMENT,
                                         $SDK::FM_AAD_API_PER_LAG_MANAGEMENT);    
    $chip->enableErrors();

    foreach my $sw ($self->tpGetSwitches)
    {
        my @list = $self->tpPlatformGetLogicalPortList($sw, "local,lag");
        my @portList = $self->validateExplicitList($FALSE, $port, @list);
        my @finalPortList = ();

        if (scalar(@portList) == 0)
        {
            print($TP_MSG_ERR_PORT_INVALID_ARRAY);
            return;
        }

        if ($port eq "all" && $perLagMgmt == $TRUE)
        {
            # In per-LAG management mode per-LAG attributes can be set on
            # LAG logical port only and per-port attributes can be set on
            # member port only. For port that doesn't belong to a LAG then any
            # attribute can be set on the port. Also internal ports and LAGs 
            # are not modified when "all" is defined.
            #
            # So build a list of ports that satisfy the above restrictions

            $chip->disableErrors();

            my $perLagAttribute = 
                $chip->fmIsPerLagPortAttribute($sw,
                                               $port_attr_map{$attribute});

            my ($minPort, $maxPort) = $self->tpPlatformGetPortRange();
            foreach my $port (@portList)
            {
                # Internal ports or LAGs are not modified
                if (not $chip->fmIsInternalPort($sw, $port))
                {
                    if ($port <= $maxPort)
                    {
                        # Add port to port list if the port is not member of a 
                        # LAG or the attribute is not a per-LAG attribute.
                        if ((not $chip->fmPortIsInALAG($sw, $port)) ||
                            (not $perLagAttribute))
                        {
                            push(@finalPortList, $port);
                        }
                    }
                    else
                    {
                        # This is a LAG logical port, add it to the port list 
                        # only if it is a per-LAG port attribute
                        if ($perLagAttribute)
                        {
                            push(@finalPortList, $port);
                        }
                    }
                }
            }

            $chip->enableErrors();
        }
        else
        {
            @finalPortList = @portList;
        }

        if (scalar(@finalPortList) == 0)
        {
            print($TP_MSG_ERR_PORT_INVALID_ARRAY);
            return;
        }
        
        foreach my $port (@finalPortList)
        {
            my $status = 0;
            
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            
            my $info = ($self->tpGetSwitchInfo())[$sw];
            if ($attribute eq "ifg"
                || $attribute eq "pri"
                || $attribute eq "pri2"
                || $attribute eq "pvid"
                || $attribute eq "pvid2"
                || $attribute eq "isl_user"
                || $attribute eq "cfi"
                || $attribute eq "cfi2"
                || $attribute eq "switch-pri"
                || $attribute eq "l3_deep_bytes"
                || $attribute eq "l4_deep_bytes"
                || $attribute eq "dscp"
                || $attribute eq "autoneg_inhb_timer"
                || $attribute eq "autoneg_inhb_timer_kx"
                || $attribute eq "tx_pause_time"
                || $attribute eq "tx_clk_compensation")
            {
                my %void = (
                    type    => "fm_uint32",
                    value   => $self->str2intnum($value)
                );
                if (!defined($void{value}))
                {
                    print("invalid input value\n");
                    return;
                }
                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif (   $attribute eq "capture_tcp_flags"
                   || $attribute eq "capture_l4_entry"
                   || $attribute eq "check_vid2_learning"
                   || $attribute eq "cjpat"
                   || $attribute eq "di_parsing"
                   || $attribute eq "dic"
                   || $attribute eq "drop_bv"
                   || $attribute eq "drop_management_isl"
                   || $attribute eq "drop_smac_err"
                   || $attribute eq "drop_tagged"
                   || $attribute eq "drop_untagged"
                   || $attribute eq "ignore_ifg_errors"
                   || $attribute eq "learning"
                   || $attribute eq "loopback_suppress"
                   || $attribute eq "mcast_pruning"
                   || $attribute eq "parse_non_ip"
                   || $attribute eq "replace_dscp"
                   || $attribute eq "routing"
                   || $attribute eq "rx_cut_through"
                   || $attribute eq "tx_cut_through"
                   || $attribute eq "update_dscp"
                   || $attribute eq "update_pri"
                   || $attribute eq "update_ttl" 
                   || $attribute eq "discard_frame_errors"
                   || $attribute eq "trap_bpdu"
                   || $attribute eq "trap_garp"
                   || $attribute eq "trap_lacp"
                   || $attribute eq "trap_other"
                   || $attribute eq "trap_security"
                   || $attribute eq "pri2_from_pri1"
                   || $attribute eq "pri1_from_pri2"
                   || $attribute eq "trill_parsing"
                   || $attribute eq "trill_tagging"
                   || $attribute eq "trill_vlan_tagging"
                   || $attribute eq "vlan_translation"
                   || $attribute eq "select_vid2"
                   || $attribute eq "select_vid2_next"
                   || $attribute eq "autoneg_ignore_nonce"
                   || $attribute eq "timestamp_generate"
                   || $attribute eq "timestamp_events"
                   || $attribute eq "parse_inner")
            {
                if ((lc($value) ne "on") && (lc($value) ne "off"))
                {
                    printf("$attribute must be set on or off\n");
                    return;
                }
                my %void = (type => "fm_bool", value => $FM_DISABLED);
                if (lc($value) eq "on")
                {
                    $void{value} = $FM_ENABLED;
                }
                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "autoneg_basepage")
            {
                $value = Math::BigInt->new($value);
                my %void = (type => "fm_uint64", value => $value);
                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "autoneg_nextpages")
            {
                my $numPages = 1;
                my @pages = ();

                if ($value eq "none")
                {
                    $numPages = 0;
                }
                else
                {
                    $pages[0] = Math::BigInt->new($value);

                    my $page = shift(@arguments);
                    while (defined($page))
                    {
                        $pages[$numPages] = Math::BigInt->new($page);
                        $numPages++;
                        $page = shift(@arguments);
                    }
                }

                my %pagesArray = ( type=>"fm_uint64[]", value => \@pages);

                $status +=
                     $chip->fmSWIGSetAnNextPages($sw, $logPort, $numPages, \%pagesArray);
            }
            elsif ($attribute eq "active_mac") 
            {
                my %void = (
                    type    => "fm_int",
                    value   => $self->str2intnum($value)
                );
                if (!defined($void{value}))
                {
                    print("invalid input value\n");
                    return;
                }
                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "serdes_emphasis"
                || $attribute eq "serdes_drive")
            {
                my %void = (
                    type    => "fm_int",
                    value   => $self->str2intnum($value)
                );
                if (!defined($void{value}))
                {
                    print("invalid input value\n");
                    return;
                }
                my $mac = shift(@arguments);
                if (!defined($mac))
                {
                    $mac = $FM_PORT_ACTIVE_MAC;
                }

                if ( $info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000 ||
                     $info->{'switchFamily'} == $FM_SWITCH_FAMILY_REMOTE_FM6000 )
                {
                    # assume command applies to  all lanes
                    # TODO: allow TestPoint to set per-LANE
                    $status = 
                        $chip->fmSetPortAttributeV2( $sw, $logPort, $mac,
                                                 $FM_PORT_LANE_ALL,
                                                 $port_attr_map{$attribute}, 
                                                 \%void);
                }
                else
                {
                    $status +=
                         $chip->fmSetPortAttribute($sw, $logPort,
                                                   $port_attr_map{$attribute}, \%void);
                }
            }
            elsif ( $attribute eq "signal_threshold" || 
                    $attribute eq "slew" )
            {
                my %void = (
                    type    => "fm_int",
                    value   => $self->str2intnum($value)
                );
                if (!defined($void{value}))
                {
                    print("invalid input value\n");
                    return;
                }
                my $mac = shift(@arguments);
                if (!defined($mac))
                {
                    $mac = $FM_PORT_ACTIVE_MAC;
                }


                # assume command applies to  all lanes
                # TODO: allow TestPoint to set per-LANE
                $status = 
                    $chip->fmSetPortAttributeV2( $sw, $logPort, $mac,
                                             $FM_PORT_LANE_ALL,
                                             $port_attr_map{$attribute}, 
                                             \%void);
            }
            elsif ($attribute eq "shaper_ifg_penalty")
            {
                my %void = (
                    type    => "fm_int",
                    value   => $self->str2intnum($value)
                );
                if (!defined($void{value}))
                {
                    print("invalid input value\n");
                    return;
                }
                $status +=
                     $chip->fmSetPortQOS($sw, $logPort, 
                                         $port_attr_map{$attribute}, 0, \%void);
            }
            elsif ($attribute eq "tc_map")
            {
                $self->tpHandleSetPortMapTc($port, $value, @arguments);
            }
            elsif ($attribute eq "pc_map")
            {
                $self->tpHandleSetPortMapPc($port, $value, @arguments);
            }
            elsif ($attribute eq "autoneg")
            {
                my %void = (type => "fm_uint32", value=> 0);
    
                if ($value eq "sgmii")
                {
                    $void{value} = $FM_PORT_AUTONEG_SGMII;
                }
                elsif ($value eq "clause_37")
                {
                    $void{value} = $FM_PORT_AUTONEG_CLAUSE_37;
                }
                elsif ($value eq "clause_73")
                {
                    $void{value} = $FM_PORT_AUTONEG_CLAUSE_73;
                }
                elsif ($value eq "none")
                {
                    $void{value} = $FM_PORT_AUTONEG_NONE;
                }
                else
                {
                    printf("Invalid auto-negotiation value\n");
                    return;
                }
                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "update_cfi")
            {
                my %void = (type => "fm_uint32", value => 0);
    
                if ($value eq "on")
                {
                    $void{value} = $FM_PORT_TXCFI_ISPRIBIT;
                }
                elsif ($value eq "off")
                {
                    $void{value} = $FM_PORT_TXCFI_ASIS;
                }
                else
                {
                    printf("Invalid input value\n");
                    return;
                }
    
                $status +=
                    $chip->fmSetPortAttribute($sw, $logPort,
                            $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "parser_cfg")
            {
                my %void = (type => "fm_uint32", value => 0);
    
                if ($value eq "L2")
                {
                    $void{value} = $FM_PORT_PARSER_STOP_AFTER_L2;
                }
                elsif ($value eq "L3")
                {
                    $void{value} = $FM_PORT_PARSER_STOP_AFTER_L3;
                }
                elsif ($value eq "L4")
                {
                    $void{value} = $FM_PORT_PARSER_STOP_AFTER_L4;
                }
                else
                {
                    printf("Invalid input value\n");
                    return;
                }
    
                $status +=
                    $chip->fmSetPortAttribute($sw, $logPort,
                            $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "isl")
            {
                my %void = (type => "fm_uint32", value => 0);
    
                if ($value eq "none")
                {
                    $void{value} = $FM_ISL_TAG_NONE;
                }
                elsif ($value eq "F32")
                {
                    $void{value} = $FM_ISL_TAG_F32;
                }
                elsif ($value eq "F56")
                {
                    $void{value} = $FM_ISL_TAG_F56;
                }
                elsif ($value eq "F64")
                {
                    $void{value} = $FM_ISL_TAG_F64;
                }
                elsif ($value eq "F96")
                {
                    $void{value} = $FM_ISL_TAG_F96;
                }
                elsif ($value eq "OTHER_32")
                {
                    $void{value} = $FM_ISL_TAG_OTHER_32B;
                }
                elsif ($value eq "OTHER_64")
                {
                    $void{value} = $FM_ISL_TAG_OTHER_64B;
                }
                elsif ($value eq "OTHER_96")
                {
                    $void{value} = $FM_ISL_TAG_OTHER_96B;
                }
                else {
                    printf("Invalid input value\n");
                    return;
                }
                $status +=
                    $chip->fmSetPortAttribute($sw, $logPort,
                            $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "parser_flag")
            {
                my $flagOptions;
    
                if (defined($value))
                {
                    if (defined($parser_flag_attr_map{$value}))
                    {
                        $flagOptions = $parser_flag_attr_map{$value};
                    }
                    elsif ($value eq "all")
                    {
                        $flagOptions = 0;
                        foreach my $key (keys(%parser_flag_attr_map))
                        {
                            $flagOptions |= $parser_flag_attr_map{$key};
                        }
                    }
                }
    
                if (!defined($value) || !defined($flagOptions))
                {
                    print "Invalid parser_flag option\n";
                    return;
                }
    
                # Get the option value and make sure it's valid.
                my $setting = shift(@arguments);
                if (!defined($setting) ||
                    !($setting eq "on" || $setting eq "off") )
                {
                    print "Invalid parser_flag value\n";
                    return;
                }
    
                my %void = (type => "fm_uint32", value => 0);
    
                # Get the parser flag options for this port.
                $status = $chip->fmGetPortAttribute(
                        $sw, 
                        $logPort, 
                        $FM_PORT_PARSER_FLAG_OPTIONS, 
                        \%void);
    
                # Provide explicit feedback if we fail.
                if ($status != $FM_OK)
                {
                    printf("Error getting parser_flag options: %s\n",
                           $chip->fmErrorMsg($status));
                    return;
                }
    
                # Set or clear the specified options.
                if ($setting eq "on")
                {
                    $void{value} |= $flagOptions;
                }
                else
                {
                    $void{value} &= ~$flagOptions;
                }
    
                # Set the parser flag options for this port.
                $status = $chip->fmSetPortAttribute(
                        $sw,
                        $logPort,
                        $FM_PORT_PARSER_FLAG_OPTIONS,
                        \%void);
    
                # Provide explicit feedback if we fail.
                if ($status != $FM_OK)
                {
                    printf("Error setting parser_flag options: %s\n",
                           $chip->fmErrorMsg($status));
                }
            }
            elsif ($attribute eq "update_routed")
            {
                my %void = (type => "fm_bool", value => $FM_ENABLED);
    
                if ($value eq "enable")
                {
                    $void{value} = $FM_ENABLED;
                }
                elsif ($value eq "disable")
                {
                    $void{value} = $FM_DISABLED;
                }
                else {
                    printf("Invalid input value\n");
                    return;
                }
                $status +=
                    $chip->fmSetPortAttribute($sw, $logPort,
                            $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "min_frame_size" || $attribute eq "max_frame_size")
            {
                my %void = (type => "fm_int", value => $self->str2intnum($value));
                if (!$void{value})
                {
                    print("invalid input value\n");
                    return;
                }
                if ($void{value} < 32)
                {
                    print("Frame size too small, must be in 32..15864 range.\n");
                    return;
                }
                elsif ($void{value} > 15864)
                {
                    print("Frame size too large, must be in 32..15864 range.\n");
                    return;
                }
                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "security_trap")
            {
                my %void = (type => "fm_uint32", value => $FM_SECURITY_DISCARD);
                if ($value eq "on")
                {
                    $void{value} = $FM_SECURITY_SEND2CPU;
                }
                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "tx_pad_size")
            {
                my %void = (type => "fm_uint32", value => $self->str2intnum($value));
                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ( $attribute eq "loopback" ||
                    $attribute eq "fabric_loopback" )
            {
                my %void = (type => "fm_int", value => $FM_PORT_LOOPBACK_OFF);
                if ($value eq "on")
                {
                    $void{value} = $FM_PORT_LOOPBACK_TX2RX;
                }
                my $mac = shift(@arguments);
                if (!defined($mac))
                {
                    $mac = $FM_PORT_ACTIVE_MAC;
                }

                $status +=
                     $chip->fmSetPortAttributeV2( $sw, 
                                                  $logPort, 
                                                  $mac,
                                                  $FM_PORT_LANE_NA,
                                                  $port_attr_map{$attribute}, 
                                                  \%void );
            }
            elsif ($attribute eq "pause_mode")
            {
                my %void = (type => "fm_uint32", value => $FM_DISABLED);
                if (lc($value) eq "normal")
                {
                    $void{value} = $FM_PORT_RX_PAUSE_NORMAL;
                }
                elsif (lc($value) eq "class-based")
                {
                    $void{value} = $FM_PORT_RX_PAUSE_CLASS_BASED;
                }
                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "tx_pause_en")
            {
                if ((lc($value) ne "on") && (lc($value) ne "off"))
                {
                    printf("tx_pause_en must be set on or off\n");
                    return;
                }
                # If no SMP given or Tahoe then set old non-SMP pauseEn 
                if ( ( scalar(@arguments) == 0 ) ||
                        ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000) )
                {
                    # the pause state is initialized in the API's fm_api_port.c
                    if ($port != 0)
                    {
                        $self->setPauseEn($port, ($value eq "on") ? 
                                                 $FM_ENABLED : 
                                                 $FM_DISABLED);
                    }
                    if ($info->{'switchFamily'} != $FM_SWITCH_FAMILY_FM2000)
                    {
                        $self->setSMPPauseEn($port, ($value eq "on") ?
                                                 0xFFFFFFFF : # all SMPs
                                                 0);          # no SMPs
                    }
                } else {
                    my @smps = $self->validateList(@arguments, 0, 1);
                    my @smpvals = ( $FM_QOS_TC_SMP_0, $FM_QOS_TC_SMP_1 );
                    if ( scalar(@smps) == 0 )
                    {
                        printf("SMP must be 0, 1 or 0..1\n");
                        return;
                    }
                    my $smpMask = $self->getSMPPauseEn($port);
                    foreach my $i (@smps)
                    {
                        if ($i eq "0")
                        {
                            if (lc($value) eq "on")
                            {
                                $smpMask |= 1;
                            } else {
                                $smpMask &= 0xFFFFFFFE;
                            }
                        } elsif ($i eq "1") {
                            if (lc($value) eq "on")
                            {
                                $smpMask |= 1<<1;
                            } else {
                                $smpMask &= 0xFFFFFFFD;
                            }
                        }
                    }
                    $self->setSMPPauseEn($port, $smpMask);
                }
                $status += $FM_OK;
            }
            elsif ($attribute eq "mask")
            {
                my $physPort;
                my $writeSw = $sw;

                my ($minPhysPort, $maxPhysPort) = 
                   $self->tpPlatformGetPortRange();
                
                if ( ($logPort >= $minPhysPort) && ($logPort <= $maxPhysPort) )
                {   
                    $chip->fmPlatformMapLogicalPortToPhysical($sw, 
                                                              $logPort, 
                                                              \$switchNum, 
                                                              \$physPort);
                }

                if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG) 
                {
                    my $switchNumNew;
        
                    # if SWAG, we need to look for the real chip and port
                    $chip->fmPlatformMapPhysicalPortToLogical($switchNum, 
                                        $physPort, \$switchNumNew, \$logPort);

                    # get the switch info for the new switchNum
                    $info = ($self->tpGetSwitchInfo())[$switchNumNew];
                    $writeSw = $switchNumNew;

                    if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
                    {
                        printf("Setting port mask is only supported on a ".
                            "physical switch, not on a SWAG switch. ".
                            "Aborting...\n");
                        next;
                    }
                }

                if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000) 
                {
                    my $bitmask = SDK::fm_bitArray->new();
                    $chip->fmCreateBitArray($bitmask, $info->{'numPorts'});

                    if(!(lc($value) eq "none"))
                    {
                        my @portList2 =
                            $self->validateList($value, 
                                                $self->tpPlatformGetPortRange());
                        foreach my $port2 (@portList2)
                        {
                            my ($sw2, $logPort2) =
                                $self->tpPlatformMapGlobalToLogicalPort($port2);

                            if ($sw != $sw2)
                            {
                                print("Only single switches are supported " .
                                      "for the mask attribute\n");
                                return;
                            }

                            $chip->fmSetBitArrayBit($bitmask, $logPort2, 1);
                        }
                    }
                    my %void = ( type => "fm_bitArray", value => $bitmask );

                    $status += $chip->fmSetPortAttribute($sw, $logPort,
                                                         $FM_PORT_MASK_WIDE,
                                                         \%void);

                    $chip->fmDeleteBitArray($bitmask);
                }
                else
                {
                    my %void = (type => "fm_uint32", value => 0);
                    
                    if(!(lc($value) eq "none"))
                    {
                        # now parse the mask port list parameter
                        my @portList2 =
                            $self->validateList($value, 
                                                $self->tpPlatformGetPortRange());
    
                        my $destMask = 0x0;
                        foreach my $port2 (@portList2)
                        {
                            my ($sw2, $logPort2) =
                                $self->tpPlatformMapGlobalToLogicalPort($port2);
    
                            my ($physPort2, $maskSw);
    
                            $chip->fmPlatformMapLogicalPortToPhysical($sw,
                                                $logPort2, \$sw2, \$physPort2);
                            
                            my $info2 = ($self->tpGetSwitchInfo())[$sw];
                            $maskSw = $sw;
    
                            if ($info2->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG)
                            {
                                my $swNumNew2;
    
                                $chip->fmPlatformMapPhysicalPortToLogical($sw2,
                                            $physPort2, \$swNumNew2, \$logPort2);
                                
                                $info2 = ($self->tpGetSwitchInfo())[$swNumNew2];
                                $maskSw = $swNumNew2;
    
                                if ($info2->{'switchFamily'} ==
                                                           $FM_SWITCH_FAMILY_SWAG)
                                {
                                    printf("Ignoring register operation on ".
                                        "aggregate switch $swNumNew2\n");
                                    next;
                                }
    
                                # SWAG CPU port is 0 on all devices
                                if ( ($logPort2 == 0) && ($physPort2 == 0) )
                                {
                                    # tell the device it's on the same switch
                                    $maskSw = $writeSw;
                                }
                            }
    
                            if ($writeSw != $maskSw)
                            {
                                print("ports need to be on the same switch as ".
                                      "the source port\n");
                                next;
                            }
                            $destMask = $destMask | (1 << $logPort2);
                        }
                        $void{value} = $destMask;
                    }
        
                    $status += $chip->fmSetPortAttribute($writeSw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
                }
            }
            elsif ($attribute eq "store_and_fwd")
            {
                my $switchInfo = new SDK::fm_switchInfo();
                $chip->fmGetSwitchInfo($sw, $switchInfo);
                if ($switchInfo->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG) 
                {
                   printf("Setting port store_and_fwd mask is only supported". 
                          "on a physical switch, not on a SWAG switch.". 
                          " Aborting...\n");
                   return; 
                }
                my @portList2 =
                         $self->validateList($value, $self->tpPlatformGetPortRange());
                my $destMask = 0x0;
                foreach my $port2 (@portList2)
                {
                    my ($sw2, $logPort2) =
                                   $self->tpPlatformMapGlobalToLogicalPort($port2);
                    if ($sw2 != $switchNum)
                    {
                        print("ports need to be on the same switch as the source port\n");
                        return;
                    }
                    $destMask = $destMask | (1 << $logPort2);
                }
                my %void = (type => "fm_uint32", value => $destMask);
                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "tagging" ||
                   $attribute eq "tagging2")
            {
                my %void = (type => "fm_uint32", value => $FM_TAG_REPLACE);
                if ($value eq "add")
                {
                    $void{value} = $FM_TAG_ADD;
                }
                elsif ($value eq "keep")
                {
                    $void{value} = $FM_TAG_KEEP;
                }
                elsif ($value eq "add-no-def")
                {
                    $void{value} = $FM_TAG_ADD_NO_DEFAULT;
                }
                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "security_pri")
            {
                my %void = (type => "fm_uint32", value => $FM_SECURITY_PRI_NORMAL);
                if ($value eq "high")
                {
                    $void{value} = $FM_SECURITY_PRI_HIGH;
                }
                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "security")
            {
                my %void = (type => "fm_uint32", value => $FM_PORT_SECURITY_SHV);
                if ($value eq "off")
                {
                    $void{value} = $FM_PORT_SECURITY_OFF;
                }
                elsif ($value eq "sav")
                {
                    $void{value} = $FM_PORT_SECURITY_SAV;
                }
                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "ether_type")
            {
                $value = hex($value) if ($value =~ /\D/);

                my %void = (type => "fm_uint32", value => $value);
                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "equalizer")
            {
                my %void = (type => "fm_uint32", value => undef);
                if ($value eq 'auto')
                {
                    $void{'value'} = $FM_PHY_EQUALIZER_AUTO;
                }
                elsif ($value >= 0 && $value <= 31)
                {
                    $void{'value'} = $value;
                }
                else
                {
                    print("Invalid value, must be in range 0..31 or auto\n");
                    return;
                }
                if (!defined($FM_PLATFORM_ATTR_EQUALIZER))
                {
                    print("Equalizer cannot be set on this platform.\n");
                    return;
                }
                $chip->fmPlatformSetAttribute($sw, $logPort,
                                              $FM_PLATFORM_ATTR_EQUALIZER, \%void);
            }
            elsif ($attribute eq "stats_config")
            {
                my $bits = 
                {
                    1 => $FM_STAT_GROUP_PER_PORT_RX_FRAME_CLASS,
                    2 => $FM_STAT_GROUP_PER_PORT_RX_COUNT_BY_LEN,
                    3 => $FM_STAT_GROUP_PER_PORT_RX_OCTETS,
                    4 => $FM_STAT_GROUP_PER_PORT_RX_COUNT_BY_PRIORITY,
                    5 => $FM_STAT_GROUP_PER_PORT_RX_OCTETS_BY_PRIORITY,
                    6 => $FM_STAT_GROUP_PER_PORT_FWD_ACTION,
                    7 => $FM_STAT_GROUP_PER_PORT_TX_FRAME_CLASS,
                    8 => $FM_STAT_GROUP_PER_PORT_TX_COUNT_BY_LEN,
                    9 => $FM_STAT_GROUP_PER_PORT_TX_OCTETS,
                    11 => $FM_STAT_GROUP_VLAN_RX_FRAME_CLASS,
                    12 => $FM_STAT_GROUP_VLAN_RX_OCTETS,
                    14 => $FM_STAT_GROUP_EGRESS_ACL,
                };
    
                if ( !defined($value) )
                {
                    printf("Must specify valid group list!\n");
                    return;
                }
    
                my @groupList = $self->validateList($value, 1, 14);
    
                if (scalar(@groupList))
                {
                    my $statsCfg = 0;
    
                    foreach my $g (@groupList)
                    {
                        if (!defined($bits->{$g}))
                        {
                            printf("WARNING: Group $g is not a valid group, skipping!\n");
                        }
                        else
                        {
                            $statsCfg |= $bits->{$g};
                        }
                    }
    
                    my %void = (type => "fm_uint32", value => $statsCfg);
    
                    my $status = $chip->fmSetPortAttribute($sw, 
                                                           $logPort,
                                                           $FM_PORT_STAT_GROUP_ENABLE,
                                                           \%void);
                                                    
                    if ($status != $FM_OK)
                    {
                        printf("Could not set statistics config: %s\n",
                               $chip->fmErrorMsg($status));
                    }
                }
                else
                {
                    printf("Must specify valid group list!\n");
                    return;
                }
            }

            # handle set port config <port> capture [rx | tx | off]
            elsif ($attribute eq "capture")
            {
                if (lc($value) eq "off")
                {
                    # Remove this port from both mirrors 
                    $self->tpHandleMirrorDeletePort(2, $logPort);
                    $self->tpHandleMirrorDeletePort(3, $logPort);
                } elsif(lc($value) eq "rx") {
                    # If mirror 2 doesn't exist, create it
                    my %mirrors = $self->_tpIsValidMirrorGroupList((2,3));
                    if ($mirrors{2} == $FALSE)
                    {
                        $self->tpHandleCreateMirror(2, 0, "redirect");
                    }
                    if ($mirrors{3} == $TRUE)
                    {
                        # Remove this port from mirror 3
                        $self->tpHandleMirrorDeletePort(3, $logPort);
                    }
                    # add this port to mirror 2
                    $self->tpHandleMirrorAddPort(2,$logPort);
                } elsif(lc($value) eq "tx") {
                    # If mirror 3 doesn't exist, create it
                    my %mirrors = $self->_tpIsValidMirrorGroupList((2,3));
                    if ($mirrors{3} == $FALSE)
                    {
                        $self->tpHandleCreateMirror(3, 0, "egress");
                    }
                    if ($mirrors{2} == $TRUE)
                    {
                        # Remove this port from mirror 2
                        $self->tpHandleMirrorDeletePort(2, $logPort);
                    }
                    # add this port to mirror 3
                    $self->tpHandleMirrorAddPort(3,$logPort);
                } else {
                    printf("Capture must be set to rx, tx, or off\n");
                    return;
                }
            }
            # handle set port config <port> rx_pause_en [on/off] <smp> 
            elsif ($attribute eq "rx_pause_en")
            {
                if ((lc($value) ne "on") && (lc($value) ne "off"))
                {
                    printf("rx_pause_en must be set on or off\n");
                    return;
                }
                my $tcMask = 0;
    
                if ( ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000) )
                {
                    my %void = (type => "fm_bool", value => $FM_DISABLED);
                    if (lc($value) eq "on")
                    {
                            $void{value} = $FM_ENABLED;
                    }
                    $status += $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
                    if ($status != $FM_OK)
                    {
                        printf("\nError setting FM_PORT_RX_PAUSE: %s\n", 
                               $chip->fmErrorMsg($status));
                    }
                } else {
                    # Use Class Based PAUSE Attribute 
                    # Get the smp value and make sure it is appropriate.
                    if ( scalar(@arguments) == 0 )
                    {
                        if (lc($value) eq "on")
                        {
                            if ($info->{'switchFamily'} == 
                                $FM_SWITCH_FAMILY_FM6000)
                            {
                                $tcMask = 0xfff;
                            }
                            else
                            {
                                $tcMask = 0xff;
                            }
                        } 
                        elsif (lc($value) eq "off") 
                        {
                            $tcMask = 0x0;
                        } 
                        else 
                        {
                            printf("rx_pause_on value should be on or off\n");
                            return;
                        }
                    } else {
                        my @smps = $self->validateList(@arguments, 0, 1);
                        my @smpvals = ( $FM_QOS_TC_SMP_0, $FM_QOS_TC_SMP_1 );
                        if ( scalar(@smps) == 0 )
                        {
                            printf("SMP must be 0, 1 or both\n");
                            return;
                        }
    
                        # has been set to class-based
                        my %tcvoid = (type => "fm_uint32", value => 0);
                
                        $status = $chip->fmGetPortAttribute($sw, $logPort,
                                                $FM_PORT_RX_CLASS_PAUSE, \%tcvoid);
                        $tcMask = $tcvoid{value};
                        my @tcRange;
                        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
                        {
                            @tcRange = (0..11);
                        }
                        else
                        {
                            @tcRange = (0..7);
                        }

                        foreach my $i (@tcRange)
                        {
                            my %void3 = (type => "fm_uint32", value => 0);
                            # Get the SMP (Shared Memory Partition) to TC (traffic class) mapping
                            $status = $chip->fmGetSwitchQOS($sw, 
                                                        $FM_QOS_TC_SMP_MAP,
                                                        $i, 
                                                        \%void3);
                            if ($status != $FM_OK)
                            {
                                printf("\nError getting SMP priority maps: %s\n", 
                                       $chip->fmErrorMsg($status));
                                last $i;
                            } 
                            else 
                            {
                                foreach my $smp (@smps)
                                {
                                    if ($void3{value} == $smpvals[$smp])
                                    {
                                        if (lc($value) eq "on")
                                        {
                                            $tcMask |= (1 << $i);
                                        }
                                        elsif (lc($value) eq "off") 
                                        {
                                            $tcMask &= ~(1 << $i);
                                        }
                                        else 
                                        {
                                            printf("rx_pause_en should be on/off <smp>\n");
                                            return;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    my %void4 = (type => "fm_uint32", value => $tcMask);
                    $status += $chip->fmSetPortAttribute($sw, $logPort,
                                                $FM_PORT_RX_CLASS_PAUSE, \%void4);
                    if ($status != $FM_OK)
                    {
                        printf("\nError setting FM_PORT_RX_CLASS_PAUSE: %s\n", 
                               $chip->fmErrorMsg($status));
                        return;
                    }
                }
        
            }
            elsif ($attribute eq "bcast_flooding")
            {
                my %void = (type => "fm_int", value=> 0);
    
                if ($value eq "fwd_exc_cpu")
                {
                    $void{value} = $FM_PORT_BCAST_FWD_EXCPU;
                }
                elsif ($value eq "forward")
                {
                    $void{value} = $FM_PORT_BCAST_FWD;
                }
                elsif ($value eq "discard")
                {
                    $void{value} = $FM_PORT_BCAST_DISCARD;
                }
                else
                {
                    printf("Invalid bcast flooding value\n");
                    return;
                }
                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "mcast_flooding")
            {
                my %void = (type => "fm_int", value=> 0);
    
                if ($value eq "forward")
                {
                    $void{value} = $FM_PORT_MCAST_FWD;
                }
                elsif ($value eq "trap")
                {
                    $void{value} = $FM_PORT_MCAST_TRAP;
                }
                elsif ($value eq "log")
                {
                    $void{value} = $FM_PORT_MCAST_LOG;
                }
                elsif ($value eq "discard")
                {
                    $void{value} = $FM_PORT_MCAST_DISCARD;
                }
                else
                {
                    printf("Invalid mcast flooding value\n");
                    return;
                }
                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "tx_tc_private_wm")
            {
                # Get the tc value and make sure it's valid.
                my $tc = $value;
                if ($tc < 0 || $tc > 8)
                {
                    print "Invalid traffic class must be in range 0-8\n";
                    return;
                }

                my %void = (type => "fm_int", value => $self->str2intnum($arguments[0]));
                if ($void{value} < 0 || $void{value} > 4193792)
                {
                    print("invalid watermark value\n");
                    return;
                }

                $status += $chip->fmSetPortQOS($sw, $logPort, 
                                $port_attr_map{$attribute}, $tc, \%void);
            }
            elsif ($attribute eq "eth_mode")
            {
                my $lane = $FM_PORT_LANE_NA;
                my %void = (type => "fm_int", value => 0);

                my $mac = shift(@arguments);
                if (!defined($mac))
                {
                    $mac = $FM_PORT_ACTIVE_MAC;
                }

                $void{value} = $port_ethMode_map{$value};
                $status += $chip->fmSetPortAttributeV2($sw, 
                                                       $logPort,
                                                       $mac,
                                                       $lane,
                                                       $port_attr_map{$attribute},
                                                       \%void);
            }
            elsif ($attribute eq "rx_lane_polarity")
            {
                my $mac = shift(@arguments);
                if (!defined($mac))
                {
                    $mac = $FM_PORT_ACTIVE_MAC;
                }

                if ( $value =~ /0x[0-9A-F]+/i )
                {
                    # number in hex format
                    $value = hex($value);
                }
                elsif ( $value =~ /\D+/)
                {
                    # Value contains characters but it's not hex: it's an error
                    print "Invalid value\n";
                    return;
                }

                if ($value > 0xF)
                {
                    print "There are only 4 lanes to configure, bit 0, 1, 2 and 3\n";
                    return;
                }

                my %void = (type => "fm_int", value => 0);
                my $family = $self->getSwitchFamily($switchNum);

                # different processing between FM6000 and legacy chips
                if ( $family == $FM_SWITCH_FAMILY_FM6000 || 
                     $family == $FM_SWITCH_FAMILY_REMOTE_FM6000 )
                {
                    my $eth_mode;

                    $status = 
                        $chip->fmGetPortAttribute( $sw, $logPort,
                                         $FM_PORT_ETHERNET_INTERFACE_MODE, 
                                         \%void );
                    if ( $status == $FM_OK )
                    {
                        my $minLane = 0;
                        my $maxLane = 0;

                        $eth_mode = $void{value};

                        if ( $eth_mode & $FM_ETH_MODE_4_LANE_BIT_MASK )
                        {
                            $maxLane = 3;
                        }

                        for ( my $i = $minLane; $i <= $maxLane; $i++ )
                        {
                            $void{value} = $value & (1 << $i);
                            $status += 
                                $chip->fmSetPortAttributeV2($sw, $logPort,
                                                $mac,$i,
                                                $port_attr_map{$attribute},
                                                \%void );
                        }
                    }
                }
                else
                {
                    $void{value} = $value;

                    $status += 
                        $chip->fmSetPortAttribute( $sw, $logPort,
                                               $port_attr_map{$attribute},
                                               \%void );
                }
            }
            elsif ($attribute eq "rx_lane_ordering")
            {
                if ((lc($value) ne "normal") && (lc($value) ne "reversed"))
                {
                    printf("$attribute must be set to normal or reversed\n");
                    return;
                }

                my %void = (type => "fm_int", 
                        value => (lc($value) eq "normal" ? 
                                            $FM_PORT_LANE_ORDERING_NORMAL : 
                                            $FM_PORT_LANE_ORDERING_INVERT));

                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "tx_fcs_mode")
            {
                my %void = (type => "fm_int", value=> 0);
    
                if ($value eq "passthru")
                {
                    $void{value} = $FM_FCS_PASSTHRU;
                }
                elsif ($value eq "insert_normal")
                {
                    $void{value} = $FM_FCS_INSERT_NORMAL;
                }
                elsif ($value eq "replace_normal")
                {
                    $void{value} = $FM_FCS_REPLACE_NORMAL;
                }
                else
                {
                    printf("Invalid tx_fcs_mode value\n");
                    return;
                }
                $status +=
                     $chip->fmSetPortAttribute($sw,
                                               $logPort,
                                               $port_attr_map{$attribute},
                                               \%void);
            }
            elsif ($attribute eq "tx_lane_polarity")
            {
                my $mac = shift(@arguments);
                if (!defined($mac))
                {
                    $mac = $FM_PORT_ACTIVE_MAC;
                }

                # accept only "normal", "reversed" or a number between 0x0 and 0xF
                if (lc($value) eq "normal")
                {
                    # all lanes not inverted
                    $value = $FM_PORT_LANE_POLARITY_NORMAL;
                }
                elsif ( lc($value) eq "reversed" )
                {
                    # all lanes inverted
                    $value = $FM_PORT_LANE_POLARITY_INVERT;
                }
                elsif ( $value =~ /0x[0-9A-F]+/i )
                {
                    # number in hex format
                    $value = hex($value);
                }
                elsif ( $value =~ /\D+/)
                {
                    # Value contains characters but it's not hex: it's an error
                    print "Invalid value\n";
                    return;
                }

                if ($value > 0xF)
                {
                    print "There are only 4 lanes to configure, bit 0, 1, 2 and 3\n";
                    return;
                }

                my %void = (type => "fm_int", value => 0);
                my $family = $self->getSwitchFamily($switchNum);

                # different processing between FM6000 and legacy chips
                if ( $family == $FM_SWITCH_FAMILY_FM6000 || 
                     $family == $FM_SWITCH_FAMILY_REMOTE_FM6000 )
                {
                    my $eth_mode;

                    $status = 
                        $chip->fmGetPortAttribute( $sw, $logPort,
                                         $FM_PORT_ETHERNET_INTERFACE_MODE, 
                                         \%void );
                    if ( $status == $FM_OK )
                    {
                        my $minLane = 0;
                        my $maxLane = 0;

                        $eth_mode = $void{value};

                        if ( $eth_mode & $FM_ETH_MODE_4_LANE_BIT_MASK )
                        {
                            $maxLane = 3;
                        }

                        for ( my $i = $minLane; $i <= $maxLane; $i++ )
                        {
                            $void{value} = $value & (1 << $i);
                            $status += 
                                $chip->fmSetPortAttributeV2($sw, $logPort,
                                                $mac, $i,
                                                $port_attr_map{$attribute},
                                                \%void );
                        }
                    }
                }
                else
                {
                    if ( ($value != 0) && ($value != 0xF) )
                    {
                        # for legacy switches only all or none of the lanes
                        # can be inverted
                        print "Invalid polarity for this switch type\n";
                        return;
                    }

                    $void{value} = $value;

                    $status += $chip->fmSetPortAttribute ( $sw,
                                                           $logPort,
                                                           $port_attr_map{$attribute},
                                                           \%void );
                }
            }
            elsif ($attribute eq "tx_lane_ordering")
            {
                if ((lc($value) ne "normal") && (lc($value) ne "reversed"))
                {
                    printf("$attribute must be set to normal or reversed\n");
                    return;
                }

                my %void = (type => "fm_int", 
                        value => (lc($value) eq "normal" ? 
                                            $FM_PORT_LANE_ORDERING_NORMAL : 
                                            $FM_PORT_LANE_ORDERING_INVERT));

                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "pause_resend_time")
            {
                my %void = (type => "fm_int", value => $value);
                
                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "pause_mac_addr")
            {
                if ($value !~ m/:/)
                {
                    print "'$value' is not a legal MAC address\n";
                    return;
                }

                if ($self->validateL2Address($value) != $FM_OK)
                {
                    print "'$value' is not a legal MAC address\n";
                    return;
                }

                my %void = (type => "fm_macaddr", value => $value);

                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "vlan_tag_a" ||
                   $attribute eq "vlan_tag_b")
            {
                $value = hex($value) if ($value =~ /\D/);

                my %void = (type => "fm_uint32", value => $value);
                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq 'deep_protocol_a' ||
                   $attribute eq 'deep_protocol_b')
            {
                $value = hex($value) if ($value =~ /\D/);

                my %void = (type => "fm_uint32", value => $value);
                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq 'parse_tcp_payload' ||
                   $attribute eq 'parse_udp_payload' ||
                   $attribute eq 'parse_a_payload' ||
                   $attribute eq 'parse_b_payload')
            {
                $value = Math::BigInt->new($value);
                my %void = (type => "fm_uint64", value => $value);
                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq 'ucast_flooding')
            {
                if (!("forward trap log discard" =~ m/$value/i ))
                {
                    printf("$attribute must be one of: forward, trap log or discard\n");
                    return;
                }

                my $result;

                if ($value eq "forward")
                {
                    $result = $FM_PORT_UCAST_FWD;
                }
                elsif ($value eq "trap")
                {
                    $result = $FM_PORT_UCAST_TRAP;
                }
                elsif ($value eq "log")
                {
                    $result = $FM_PORT_UCAST_LOG;
                }
                elsif ($value eq "discard")
                {
                    $result = $FM_PORT_UCAST_DISCARD;
                }

                my %void = (type => "fm_int", value => $result);

                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);
            }
            elsif ($attribute eq "link_detection")
            {
                if ((lc($value) ne "polling") && (lc($value) ne "interrupt"))
                {
                    printf("$attribute must be set to polling or interrupt\n");
                    return;
                }

                my %void = (type => "fm_bool", value => $FM_DISABLED);

                if (lc($value) eq "interrupt")
                {
                    $void{value} = $FM_ENABLED;
                }
                $status +=
                     $chip->fmSetPortAttribute($sw, $logPort,
                                               $port_attr_map{$attribute}, \%void);        
            }
            elsif ($attribute eq "dfe_mode")
            {
                my %void = ( type  => "fm_int", 
                             value => 0 );

                my $mac = shift(@arguments);
                if (!defined($mac))
                {
                    $mac = $FM_PORT_ACTIVE_MAC;
                }

                if (lc($value) eq "static")
                {
                    $void{value} = $FM_DFE_MODE_STATIC;
                }
                elsif (lc($value) eq "one_shot")
                {
                    $void{value} = $FM_DFE_MODE_ONE_SHOT;
                }
                elsif (lc($value) eq "continuous")
                {
                    $void{value} = $FM_DFE_MODE_CONTINUOUS;
                }
                else
                {
                    printf("$attribute must be set to static, ".
                           "one_shot or continuous\n"); 
                    return;
                }

                # assume command applies to  all lanes
                # TODO: allow TestPoint to set per-LANE
                $status += 
                    $chip->fmSetPortAttributeV2($sw, $logPort, $mac,
                                               $FM_PORT_LANE_ALL,
                                               $port_attr_map{$attribute},
                                               \%void );
            }
            elsif ($attribute eq "dfe_params")
            {
                my %void = (
                    type    => "fm_uint32",
                    value   => $self->str2intnum($value)
                );
                if (!defined($void{value}))
                {
                    print("invalid input value\n");
                    return;
                }

                my $mac = shift(@arguments);
                if (!defined($mac))
                {
                    $mac = $FM_PORT_ACTIVE_MAC;
                }

                # assume command applies to  all lanes
                # TODO: allow TestPoint to set per-LANE
                $status += 
                    $chip->fmSetPortAttributeV2($sw, $logPort, $mac,
                                               $FM_PORT_LANE_ALL,
                                               $port_attr_map{$attribute},
                                               \%void );
            }
            elsif ($attribute eq "low_eyescore_mode")
            {
                my %void = ( type  => "fm_int", 
                             value => 0 );

                my $mac = shift(@arguments);
                if (!defined($mac))
                {
                    $mac = $FM_PORT_ACTIVE_MAC;
                }

                if (lc($value) eq "disabled")
                {
                    $void{value} = $FM_PORT_LOW_EYE_SCORE_RECOVERY_OFF;
                }
                elsif (lc($value) eq "soft")
                {
                    $void{value} = $FM_PORT_LOW_EYE_SCORE_SOFT_RECOVERY;
                }
                elsif (lc($value) eq "hard")
                {
                    $void{value} = $FM_PORT_LOW_EYE_SCORE_HARD_RECOVERY;
                }
                else
                {
                    printf("$attribute must be set to disabled, ".
                           "soft or hard\n"); 
                    return;
                }

                # assume command applies to  all lanes
                # TODO: allow TestPoint to set per-LANE
                $status += 
                    $chip->fmSetPortAttributeV2($sw, $logPort, $mac,
                                               $FM_PORT_LANE_ALL,
                                               $port_attr_map{$attribute},
                                               \%void );
            }
            elsif ($attribute eq "low_eyescore_threshold" || 
                   $attribute eq "low_eyescore_timeout")
            {
                my %void = (
                    type    => "fm_int",
                    value   => $self->str2intnum($value)
                );
                if (!defined($void{value}))
                {
                    print("invalid input value\n");
                    return;
                }

                my $mac = shift(@arguments);
                if (!defined($mac))
                {
                    $mac = $FM_PORT_ACTIVE_MAC;
                }

                # assume command applies to  all lanes
                # TODO: allow TestPoint to set per-LANE
                $status += 
                    $chip->fmSetPortAttributeV2($sw, $logPort, $mac,
                                               $FM_PORT_LANE_ALL,
                                               $port_attr_map{$attribute},
                                               \%void );
            }
            elsif ($attribute eq "up_debounce_time"   || 
                   $attribute eq "down_debounce_time")
            {
                my %void = (
                    type    => "fm_uint32",
                    value   => $self->str2intnum($value)
                );

                my %rounded = (
                    type    => "fm_uint32",
                    value   => 0
                );

                if (!defined($void{value}))
                {
                    print("invalid input value\n");
                    return;
                }

                $status +=
                     $chip->fmSetPortAttribute($sw, 
                                               $logPort,
                                               $port_attr_map{$attribute}, 
                                               \%void);

                 # read back the value to see if it was rounded up
                 if ( $status == $FM_OK )
                 {
                     $chip->fmGetPortAttribute($sw, 
                                               $logPort,
                                               $port_attr_map{$attribute}, 
                                               \%rounded);

                     if ($void{value} != $rounded{value})
                     {
                         printf("Debounce time rounded up to %d usec!\n",
                                $rounded{value});
                     }
                 }
            }
          
            elsif ($attribute eq "trill_hop_count")
            {
                my $hopCnt = $self->str2intnum($value);

                if ($hopCnt < 0 || $hopCnt > 63)
                {
                   print "Must specify a valid Hop Count value (0-63)!\n";
                   return $FM_ERR_INVALID_ARGUMENT;
                }

                $status += $chip->fmSetRBridgePortHopCount($sw, 
                                                           $logPort, 
                                                           $hopCnt);

                if ($status != $FM_OK)
                {
                   print "Hop Count could not be set!\n";
                   return $status;
                }
            }

            if ($status != $FM_OK)
            {
                printf("Port attribute %s cannot be set!\n", $attribute);
            }
        } # end foreach port
    }
    
    if (($attribute eq "tx_pause_en") || ($attribute eq "max_frame_size"))
    {
        # Update watermarks when these variables change
        # comment the following line out so the SDK's watermark configuration can take effect
        # $self->tpHandleSetWatermarks();
    }

}   # end handleSetPortConfig

sub tpHandleShowPortMax
{
    my ($self, $port) = @_;

    my $chip = $self->{CHIP};
    my ($firstSwitch, $lastSwitch) = $self->tpPlatformGetSwitchRange();

    for(my $sw = $firstSwitch; $sw <= $lastSwitch; $sw++)
    {
        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($sw, $info);
        
        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
        {
            $chip->fmDbgDumpPortMax($sw, $port);
        }
    }
}

sub handleShowPort
{
    my ($self, $port) = @_;

    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    my @indices = arrayFind(map {$self->tpPlatformIsCPUPort($_)} @globalPortList);
    if ($port ne "all" && scalar(@indices) > 0)
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));

        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        printf("%s\n", scalar($self->tpGetSwitches) > 1 ? "\nSwitch $sw:" : " ");
        print("PORT SPEED    STATE     LANE STATUS\n");
        print("---- ----- ------------ ------------------------------------\n");
        
        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            my $switchInfo;
            my $family;
            my $eth_mode;
            my $multiLane = 0;
            
            next if $self->tpPlatformIsCPUPort($port);

            $switchInfo = new SDK::fm_switchInfo();
            $chip->fmGetSwitchInfo($sw, $switchInfo);
            $family = $switchInfo->{'switchFamily'};

            my @info = (0) x 4;
            my ($mode, $state);
            my %void;

            $chip->fmGetPortState($sw, $logPort, \$mode, \$state, \@info);

            %void = (type => "fm_uint32", value => 0);

            # skip FM6000 ports that are disabled 
            $eth_mode = -1;
            if ($family == $FM_SWITCH_FAMILY_FM6000 ||
                $family == $FM_SWITCH_FAMILY_REMOTE_FM6000)
            {
                $chip->fmGetPortAttribute( $sw, $port,
                                           $FM_PORT_ETHERNET_INTERFACE_MODE,
                                           \%void );
                $eth_mode = $void{value};

            }

            $chip->fmGetPortAttribute($sw, $logPort, $FM_PORT_SPEED, \%void);
            my $speed = "UNKNOWN";
            if ($void{value} == 10000)
            {
                $speed = "10G";
            }
            elsif ($void{value} == 6000)
            {
                $speed = "6G";
            }
            elsif ($void{value} == 2500)
            {
                $speed = "2.5G";
            }
            elsif ($void{value} == 1000)
            {
                $speed = "1G";
            }
            elsif ($void{value} == 100)
            {
                $speed = "100M"
            }
            elsif ($void{value} == 10)
            {
                $speed = "10M";
            }
            elsif ($void{value} == 20000)
            {
                $speed = "20G";
            }
            elsif ($void{value} == 24000)
            {
                $speed = "24G";
            }
            elsif ($void{value} == 40000)
            {
                $speed = "40G";
            }
            else
            {
                $speed = "---";
            }

            printf("%-4s %-5s ", $port, $speed);

            my $laneStatus = 1;
            if ($eth_mode == $FM_ETH_MODE_DISABLED)
            {
                print("DISABLED\n");
                next;
            }
            elsif ($state == $FM_PORT_STATE_UP)
            {
                print("UP          ");
            }
            elsif ($state == $FM_PORT_STATE_ADMIN_DOWN)
            {
                print("Admin DOWN  ");
            }
            elsif ($state == $FM_PORT_STATE_ADMIN_PWRDOWN)
            {
                print("Power DOWN  ");
            }
            elsif ($state == $FM_PORT_STATE_BIST)
            {
                print("BIST        ");
                $laneStatus = 0;
                foreach my $pInfo (@info)
                {
                    printf(" %6d",$pInfo);
                }
            }
            elsif ($state == $FM_PORT_STATE_REMOTE_FAULT)
            {
                print("Remote Fault");
            }
            elsif ($state == $FM_PORT_STATE_DOWN)
            {
                print("DOWN        ");
            }
            elsif ($state == $FM_PORT_STATE_PARTIALLY_UP)
            {
                print("Partially UP");
            }
            elsif ($state == $FM_PORT_STATE_LOCAL_FAULT)
            {
                print("Local Fault ");
            }
            else
            {
                print("Unrecognized");
                $laneStatus = 0;
            }
                
            if ( $eth_mode > $FM_ETH_MODE_DISABLED )
            {
                my $minLane;
                my $maxLane;
                print " ";
                
                if ( $eth_mode & $FM_ETH_MODE_4_LANE_BIT_MASK )
                {              
                    $minLane   = 0;
                    $maxLane   = 3;
                    $multiLane = 1;
                }
                elsif ( $eth_mode & $FM_ETH_MODE_ENABLED_BIT_MASK ) 
                {
                    $minLane = 0;
                    $maxLane = 0;
                }
                else
                {
                    # this shouldn't happen, but prevent any further processing 
                    # if it does
                   
                    $minLane =  0;
                    $maxLane = -1;
                }

                if ( $multiLane )
                {
                    my $al = ($info[0] & (1 << 5)) != 0 ? 'Y':'N';
                    print("Aligned:$al\n");
                }

                for ( my $i = $minLane ; $i <= $maxLane ; $i++ )
                {
                    if ( $multiLane)
                    {      
                        printf(" %-21s  ", "Lane ".$i);
                    }

                    # Serdes readiness
                    my $sl = (($info[$i] & 3) == 0 ) ? 'N' :
                             (($info[$i] & 3) == 1 ) ? 'R' :
                             (($info[$i] & 3) == 2 ) ? 'T' : 'Y';
                    print "SL:$sl ";
                    
                    # Signal detect
                    my $sd;
                    if (($info[$i] & 4) == 0 )
                    {
                        $sd = "N";
                    }
                    else
                    {
                        $sd = ($info[$i] >> 3) & 3; 
                    }
                    print "SD:$sd ";
                    
                    %void = (type => "fm_int", value => 0);
                    $chip->fmGetPortAttributeV2( $sw, 
                                                 $port, 
                                                 $FM_PORT_ACTIVE_MAC,
                                                 $i,
                                                 $FM_PORT_COARSE_DFE_STATE, 
                                                 \%void );
                    my $coarseState;
                    if ( $void{value} == $FM_PORT_DFE_STATE_NOT_STARTED )
                    {
                        $coarseState = "W";
                    }
                    elsif ( $void{value} == $FM_PORT_DFE_STATE_IN_PROGRESS )
                    {
                        $coarseState = "R";
                    }
                    elsif ( $void{value} == $FM_PORT_DFE_STATE_COMPLETE )
                    {
                        $coarseState = "C";
                    }
                    else
                    {
                        $coarseState = "E";
                    }
                    %void = (type => "fm_int", value => 0);
                    $chip->fmGetPortAttributeV2( $sw, 
                                                 $port, 
                                                 $FM_PORT_ACTIVE_MAC,
                                                 $i,
                                                 $FM_PORT_FINE_DFE_STATE, 
                                                 \%void );

                    my $fineState;
                    if ( $void{value} == $FM_PORT_DFE_STATE_NOT_STARTED )
                    {
                        $fineState = "W";
                    }
                    elsif ( $void{value} == $FM_PORT_DFE_STATE_IN_PROGRESS )
                    {
                        $fineState = "R";
                    }
                    elsif ( $void{value} == $FM_PORT_DFE_STATE_COMPLETE )
                    {
                        $fineState = "C";
                    }
                    else
                    {
                        $fineState = "E";
                    }
                    %void = (type => "fm_uint32", value => 0);
                    $chip->fmGetPortAttributeV2( $sw, 
                                                 $port, 
                                                 $FM_PORT_ACTIVE_MAC,
                                                 $i,
                                                 $FM_PORT_DFE_PARAMETERS,
                                                 \%void );
                    my $dfeParms = $void{value};
                    %void = (type => "fm_int", value => 0);
                    $chip->fmGetPortAttributeV2( $sw, 
                                                 $port, 
                                                 $FM_PORT_ACTIVE_MAC,
                                                 $i,
                                                 $FM_PORT_DFE_MODE,
                                                 \%void );
                    my $dfeMode = $void{value} == $FM_DFE_MODE_STATIC ? 'S' :
                                  $void{value} == $FM_DFE_MODE_ONE_SHOT ? '1' :
                                  $void{value} == $FM_DFE_MODE_CONTINUOUS ? 'C' : 'K';
                    
                    printf(" %s 0x%8.8X %s%s\n",
                           $dfeMode,
                           $dfeParms,
                           $coarseState,
                           $fineState); 
                }
            }
            else
            {            
                if ($laneStatus)
                {
                    foreach my $pInfo (@info)
                    {
                        if ($pInfo == $FM_PORT_LANE_LOCKED)
                        {
                            printf(" %-6s", "SymLok");
                        }
                        elsif ($pInfo == $FM_PORT_LANE_NOT_LOCKED)
                        {
                            printf(" %-6s", "Active");
                        }
                        elsif ($pInfo == $FM_PORT_LANE_UNUSED)
                        {
                            printf(" %-6s", "Unused");
                        }
                        else
                        {
                            printf(" [%-6d]", $laneStatus);
                        }
                    }
        
                }
        
                if (defined($FM_PLATFORM_ATTR_EQUALIZER))
                {
                    my %void = (type => "fm_int", value => 0);
                    $chip->disableErrors();
                    my $status = $chip->fmPlatformGetAttribute($sw,
                                                           $logPort,
                                                           $FM_PLATFORM_ATTR_EQUALIZER,
                                                           \%void);
                    $chip->enableErrors();
                    if ($status == $FM_OK && ($void{'value'} & $FM_PHY_EQUALIZER_AUTO))
                    {
                        printf(" (boost %2d)",
                               $void{'value'} & ~$FM_PHY_EQUALIZER_AUTO);
                    }
                }
                print("\n");
            }
    
            
        }   # end foreach my $port (@portList)
        
    }   # end foreach my $sw ($self->tpGetSwitches)
    
    print("\n");
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    
}   # end handleShowPort


##@cmethod public void handleShowPortConfig(char *port)
#
# @desc         Handles showing the configuration for a set of ports
#               Can be a LAG logical port.   
#
# @param[in]    port The set of ports whose configuration is to be shown
sub handleShowPortConfig
{
    my ($self, $port, $mac) = @_;

    if ($attributesInitialized == $FALSE)
    {
        $self->tpInitAttributes();
    }

    my $chip = $self->{CHIP};

    if (!defined($port))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    my $portMac = $FM_PORT_ACTIVE_MAC;
    my $lane = $FM_PORT_LANE_NA;
    
    if (defined($mac))
    {
        $portMac = $mac;
    }
    
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @list = $self->tpPlatformGetLogicalPortList($switchNum, "local,lag");
        my @portList = $self->validateExplicitList($FALSE, $port, @list);

        if (scalar(@portList) == 0)
        {
            print($TP_MSG_ERR_PORT_INVALID_ARRAY);
            return;
        }

        my $attributeWidth = 18;
        my $attributeWidthStr = "%-" . $attributeWidth . "s";
    
        printf("\n%-25s", scalar($self->tpGetSwitches) > 1 ? "Switch $switchNum:" : " ");
        
        foreach my $port (@portList)
        {
           printf($attributeWidthStr, sprintf("%d", $port));
        }

        if (defined($mac))
        {
           printf("\nFor MAC $portMac");
        }

        print("\n" . "-" x 25);

        foreach my $port (@portList)
        {
           printf($attributeWidthStr, "-" x $attributeWidth);
        }

        print("\n");

        foreach my $attribute (sort(keys(%port_attr_map)))
        {
            printf("%-25s", $attribute);

            foreach my $port (@portList)
            {
                # Map port number for the benefit of non-SWAG Vegas. Ignore the
                # returned switchNum since it should match sw and may actually be
                # wrong for some platforms.
                my ($sw, $logicalPort) =
                                    $self->tpPlatformMapGlobalToLogicalPort($port);
    
                my $info = ($self->tpGetSwitchInfo())[$switchNum];
                $chip->disableErrors();
                if ($attribute eq "min_frame_size"
                    || $attribute eq "active_mac"
                    || $attribute eq "max_frame_size")
                {
                    my %void = (type => "fm_int", value => 0);
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);
                    printf($attributeWidthStr, $status != $FM_OK ? "NA" : $void{value});
                }
                elsif ($attribute eq "autoneg")
                {
                    my %void = (type => "fm_uint32", value=> 0);
                    my $result;
    
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);
    
                    if ($void{value} eq $FM_PORT_AUTONEG_NONE)
                    {
                        $result = "none";
                    }
                    elsif ($void{value} eq $FM_PORT_AUTONEG_SGMII)
                    {
                        $result = "sgmii"
                    }
                    elsif ($void{value} eq $FM_PORT_AUTONEG_CLAUSE_37)
                    {
                        $result = "clause 37";
                    }
                    elsif ($void{value} eq $FM_PORT_AUTONEG_CLAUSE_73)
                    {
                        $result = "clause 73";
                    }
                    else
                    {
                        $result = "NA";
                    }
    
                    printf($attributeWidthStr, $status != $FM_OK ? "NA" : $result);
                }
                elsif ($attribute eq "autoneg_partner_basepg" ||
                       $attribute eq "autoneg_basepage")
                {
                    my %void = (type => "fm_uint64", value => Math::BigInt->new(0));
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);
                    printf($attributeWidthStr, $status != $FM_OK ? 
                           "NA" : $void{value}->as_hex());
                }
                elsif ($attribute eq "autoneg_partner_nxtpgs" ||
                       $attribute eq "autoneg_nextpages")
                {
                     #Assume max pages of 32 
                    my @pages = (Math::BigInt->new(0)) x 32;
                    my %value = ( type => "fm_uint64[]", value => \@pages);
                    my %void = ( type => "fm_int", value => 0);

                    my $status = $chip->fmSWIGGetAnNextPages($switchNum, $logicalPort,
                                                             $port_attr_map{$attribute},
                                                             \%void, \%value);

                    if ( $status == $FM_OK )
                    {
                         my $numPages = $void{value};
                         if ($numPages == 0)
                         {
                             printf($attributeWidthStr, "none");
                         }
                         else 
                         {
                            my $nextPagesStr = "";
                            for (my $pageNum = 0; $pageNum < $numPages; $pageNum++)
                            {
                                $nextPagesStr = $nextPagesStr . $pages[$pageNum]->as_hex() . " ";
                            }
                            printf($attributeWidthStr, $nextPagesStr);
                         }
                    }
                    else
                    {
                        printf($attributeWidthStr, "NA");
                    }
                }
                elsif ($attribute eq "ifg"
                       || $attribute eq "pri"
                       || $attribute eq "pri2"
                       || $attribute eq "pvid"
                       || $attribute eq "pvid2"
                       || $attribute eq "switch-pri"
                       || $attribute eq "dscp"
                       || $attribute eq "cfi"
                       || $attribute eq "cfi2"
                       || $attribute eq "l3_deep_bytes"
                       || $attribute eq "l4_deep_bytes"
                       || $attribute eq "autoneg_inhb_timer"
                       || $attribute eq "autoneg_inhb_timer_kx"
                       || $attribute eq "tx_pause_time"
                       || $attribute eq "tx_pad_size"
                       || $attribute eq "up_debounce_time"
                       || $attribute eq "down_debounce_time"
                       || $attribute eq "tx_clk_compensation"
                       || $attribute eq "isl_user")
                {
                    my %void = (type => "fm_uint32", value => 0);
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);
                    printf($attributeWidthStr, $status != $FM_OK ? "NA" : $void{value});
                }
                elsif ($attribute eq "serdes_emphasis")
                {
                    my %void = (type => "fm_int", value => 0);
                    my $status;
                    my $family = $self->getSwitchFamily($switchNum);

                    # different processing between FM6000 and legacy chips
                    if ( $family == $FM_SWITCH_FAMILY_FM6000 || 
                         $family == $FM_SWITCH_FAMILY_REMOTE_FM6000 )
                    {   
                        # Currently displaing value only for lane 0
                        # regardless of the current eth_mode
                        # TODO: identify an output layout that is good
                        # for ports configured in multi-lane modes 
                        $status = 
                            $chip->fmGetPortAttributeV2($switchNum, 
                                               $logicalPort,
                                               $portMac, 0,
                                               $port_attr_map{$attribute},
                                               \%void);

                        if ($status != $FM_OK)
                        {
                            printf($attributeWidthStr, "NA");
                        }
                        else
                        {
                            printf($attributeWidthStr, sprintf("%d", $void{value}));
                        }
                    }
                    else
                    {
                        my $percent;
                        $status = 
                            $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                      $port_attr_map{$attribute},
                                                      \%void);

                        if ($void{value} < 14)
                        {
                            $percent = 4 * $void{value};
                        }
                        elsif ($void{value} == 14)
                        {
                            $percent = 60;
                        }
                        elsif ($void{value} == 15)
                        {
                            $percent = 65;
                        }
                        if ($status != $FM_OK)
                        {
                            printf($attributeWidthStr, "NA");
                        }
                        else
                        {
                            printf($attributeWidthStr, sprintf("%d (%-.2f)", $void{value}, $percent / 100.0));
                        }
                    }
                }
                elsif ($attribute eq "serdes_drive")
                {
                    my %void = (type => "fm_int", value => 0);
                    my $status;
                    my $family = $self->getSwitchFamily($switchNum);

                    # different processing between FM6000 and legacy chips
                    if ( $family == $FM_SWITCH_FAMILY_FM6000 || 
                         $family == $FM_SWITCH_FAMILY_REMOTE_FM6000 )
                    {   
                        # Currently displaing value only for lane 0
                        # regardless of the current eth_mode
                        # TODO: identify an output layout that is good
                        # for ports configured in multi-lane modes 
                        $status = 
                            $chip->fmGetPortAttributeV2($switchNum, 
                                               $logicalPort,
                                               $portMac, 0,
                                               $port_attr_map{$attribute},
                                               \%void);

                        if ($status != $FM_OK)
                        {
                            printf($attributeWidthStr, "NA");
                        }
                        else
                        {
                            printf($attributeWidthStr, sprintf("%d", $void{value}));
                        }
                    }
                    else
                    {
                        $status = 
                            $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                      $port_attr_map{$attribute},
                                                      \%void);
                        printf($attributeWidthStr, $status != $FM_OK ? "NA" : $void{value}, " mA");
                    }
                }
                elsif ($attribute eq "security_trap")
                {
                    my %void = (type => "fm_uint32", value => 0);
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);
                    my $value =
                              $void{value} == $FM_SECURITY_SEND2CPU ? "on" : "off";
                    printf($attributeWidthStr, $status != $FM_OK ? "NA" : $value);
                }
                elsif ($attribute eq "update_cfi")
                {
                    my %void = (type => "fm_uint32", value => 0);
    
                    my $status =
                        $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                $port_attr_map{$attribute}, \%void);
    
                    my $value 
                        = ($void{value} == $FM_PORT_TXCFI_ASIS) ? "off" : "on";
    
                    printf($attributeWidthStr, $status != $FM_OK ? "NA" : $value);
                }
                elsif (   $attribute eq "capture_tcp_flags"
                       || $attribute eq "capture_l4_entry"
                       || $attribute eq "check_vid2_learning"
                       || $attribute eq "cjpat"
                       || $attribute eq "di_parsing"
                       || $attribute eq "dic"
                       || $attribute eq "drop_bv"
                       || $attribute eq "drop_management_isl"
                       || $attribute eq "drop_smac_err"
                       || $attribute eq "drop_tagged"
                       || $attribute eq "drop_untagged"
                       || $attribute eq "ignore_ifg_errors"
                       || $attribute eq "learning"
                       || $attribute eq "loopback_suppress"
                       || $attribute eq "mcast_pruning"
                       || $attribute eq "parse_non_ip"
                       || $attribute eq "replace_dscp"
                       || $attribute eq "routing"
                       || $attribute eq "rx_cut_through"
                       || $attribute eq "tx_cut_through"
                       || $attribute eq "update_dscp"
                       || $attribute eq "update_pri"
                       || $attribute eq "update_routed"
                       || $attribute eq "update_ttl"
                       || $attribute eq "discard_frame_errors"
                       || $attribute eq "trap_bpdu"
                       || $attribute eq "trap_garp"
                       || $attribute eq "trap_lacp"
                       || $attribute eq "trap_other"
                       || $attribute eq "trap_security"
                       || $attribute eq "pri2_from_pri1"
                       || $attribute eq "pri1_from_pri2"
                       || $attribute eq "trill_parsing"
                       || $attribute eq "trill_tagging"
                       || $attribute eq "trill_vlan_tagging"
                       || $attribute eq "vlan_translation"
                       || $attribute eq "select_vid2"
                       || $attribute eq "select_vid2_next"
                       || $attribute eq "autoneg_ignore_nonce"
                       || $attribute eq "timestamp_generate"
                       || $attribute eq "timestamp_events")
                {
                    my %void = (type => "fm_bool", value => $FM_ENABLED);
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);
                    my $value =
                              $void{value} == $FM_ENABLED ? "on" : "off";
                    printf($attributeWidthStr, $status != $FM_OK ? "NA" : $value);
                }
                elsif ($attribute eq "link_detection")
                {
                    my %void = (type => "fm_bool", value => 0);
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);
                    my $value = $void{value} == $FM_ENABLED ? "interrupt" : "polling";
                    printf($attributeWidthStr, $status != $FM_OK ? "NA" : $value);
                }
                elsif ($attribute eq "isl")
                {
                    my %void = (type => "fm_uint32", value => 0);
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);
                    my $value = "error";
                    if ($void{value} == $FM_ISL_TAG_NONE)
                    {
                        $value = "none";
                    }
                    elsif ($void{value} == $FM_ISL_TAG_F32)
                    {
                        $value = "F32";
                    }
                    elsif ($void{value} == $FM_ISL_TAG_F56)
                    {
                        $value = "F56";
                    }
                    elsif ($void{value} == $FM_ISL_TAG_F64)
                    {
                        $value = "F64";
                    }
                    elsif ($void{value} == $FM_ISL_TAG_F96)
                    {
                        $value = "F96";
                    }
                    elsif ($void{value} == $FM_ISL_TAG_OTHER_32B)
                    {
                        $value = "OTHER_32";
                    }
                    elsif ($void{value} == $FM_ISL_TAG_OTHER_64B)
                    {
                        $value = "OTHER_64";
                    }
                    elsif ($void{value} == $FM_ISL_TAG_OTHER_96B)
                    {
                        $value = "OTHER_96";
                    }
                    printf($attributeWidthStr, $status != $FM_OK ? "NA" : $value);
                }
                elsif ($attribute eq "parser_cfg")
                {
                    my %void = (type => "fm_uint32", value => 0);
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);
                    my $value = "error";
                    if ($void{value} == $FM_PORT_PARSER_STOP_AFTER_L2 )
                    {
                        $value = "L2";
                    }
                    elsif ($void{value} == $FM_PORT_PARSER_STOP_AFTER_L3)
                    {
                        $value = "L3";
                    }
                    elsif ($void{value} == $FM_PORT_PARSER_STOP_AFTER_L4)
                    {
                        $value = "L4";
                    }
                    printf($attributeWidthStr, $status != $FM_OK ? "NA" : $value);
                }
                elsif ($attribute eq "parser_flag")
                {
                    my %void = (type => "fm_uint32", value => 0);
    
                    my $status = $chip->fmGetPortAttribute(
                            $switchNum, 
                            $logicalPort,
                            $port_attr_map{$attribute},
                            \%void);
    
                    my @optionList = ();
                    foreach my $option (sort keys(%parser_flag_attr_map))
                    {
                        if ($void{value} & $parser_flag_attr_map{$option})
                        {
                            push(@optionList, $option);
                        }
                    }
    
                    if (scalar(@optionList) == 0)
                    {
                        push(@optionList, "none");
                    }
    
                    printf($attributeWidthStr, join(',', @optionList));
                }
                elsif ( $attribute eq "loopback" || 
                        $attribute eq "fabric_loopback")
                {
                    my %void = (type => "fm_int", value => 0);
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);

                    my $value =
                            $void{value} == $FM_PORT_LOOPBACK_TX2RX ? "on" : "off";
                    printf($attributeWidthStr, $status != $FM_OK ? "NA" : $value);
                }
                elsif ($attribute eq "rx_pause_en")
                {
                    my %void = (type => "fm_bool", value => 0);
                    if ( ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000) )
                    {
                        %void = (type => "fm_bool", value => 0);
                        my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);
                        my $value = $void{value} == $FM_ENABLED ? "on" : "off";
                        printf($attributeWidthStr, 
                               $status != $FM_OK ? "NA" : $value);
                    } 
                    else 
                    {
                        %void = (type => "fm_uint32", value => 0);
                        my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $FM_PORT_RX_CLASS_PAUSE,
                                                        \%void);
                        my $onValue;
                        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
                        {
                            $onValue = 0xFFF;
                        }
                        else
                        {
                            $onValue = 0xFF;
                        }
                        
                        my $value = "off";
                        if ($void{value} > 0)
                        {
                            if ($void{value} == $onValue)
                            {
                                #all TCs are on
                                $value = "on"
                            } 
                            else
                            {
                                $value = $self->stringifyMask($void{value});
                            }
                        }
                        printf($attributeWidthStr, 
                               $status != $FM_OK ? "ERR" : $value);
    
                    }
                }
                elsif ($attribute eq "pause_mode")
                {
                    my %void = (type => "fm_uint32", value => 0);
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);
                    my $value = $void{value} == $FM_PORT_RX_PAUSE_NORMAL ? "normal" : "class-base";
                    printf($attributeWidthStr, $status != $FM_OK ? "NA" : $value);
                }
                elsif ($attribute eq "tx_pause_en")
                {
                    if ( ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000) ) {
                        if ($self->getPauseEn($port) == 1)
                        {
                            printf($attributeWidthStr,"on");
                        } 
                        else
                        {
                            printf($attributeWidthStr,"off");
                        }
                    } else {
                        my $smpMask = ($self->getSMPPauseEn($port)) & 0x3;
                        if ($smpMask == 0)
                        {
                            printf($attributeWidthStr,"off");
                        } elsif( $smpMask == 0x3) {
                            printf($attributeWidthStr,"on");
                        } elsif( $smpMask == 0x1) {
                            printf($attributeWidthStr,"0 ON 1 OFF");
                        } elsif( $smpMask == 0x2) {
                            printf($attributeWidthStr,"0 OFF 1 ON");
                        } else {
                            printf($attributeWidthStr,"ERR $smpMask");
                        }
                    }
                    my $status = $FM_OK;
                }
                elsif (($attribute eq "mask") ||
                       ($attribute eq "store_and_fwd"))
                {
                    # Find out the physical switch and the correspoinding port
                    my ($sw, $lport) = ($switchNum, $logicalPort);
                    my $value;
                    my $status;
                    
                    if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
                    {
                        my $bitmask = new SDK::fm_bitArray();
                        $chip->fmCreateBitArray($bitmask, $info->{'numPorts'});

                        my %void = ( type => "fm_bitArray", value => $bitmask );

                        $status = 
                                  $chip->fmGetPortAttribute($sw, 
                                                            $logicalPort,
                                                            $FM_PORT_MASK_WIDE,
                                                            \%void);

                        my @maskPorts = ();

                        for (my $mport = 0; $mport < $info->{'numPorts'}; $mport++)
                        {
                            my $isSet = 0;

                            $chip->fmGetBitArrayBit($bitmask, $mport, \$isSet);

                            if ($isSet)
                            {
                                push(@maskPorts, $mport);
                            }
                        }

                        $value = $self->StringifyList(@maskPorts);
                        
                        $chip->fmDeleteBitArray($bitmask);
                    }
                    else
                    {
                        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG) 
                        {
                           $chip->fmGetSwitchAndPortForSWAGPort($switchNum,
                                                                $logicalPort,
                                                                \$sw,
                                                                \$lport);
                        }

                        my %void = (type => "fm_uint32", value => 0);
                        $status = 
                                  $chip->fmGetPortAttribute($sw, $lport,
                                                            $port_attr_map{$attribute},
                                                            \%void);

                        $value = $self->stringifyPortMask($sw, $void{value});
                    }

                    if ($value eq "")
                    {
                       $value = "none" ;
                    }
                    printf($attributeWidthStr, $status != $FM_OK ? "NA" : $value);
                }
                elsif ($attribute eq "tagging" ||
                       $attribute eq "tagging2")
                {
                    my %void = (type => "fm_uint32", value => 0);
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);
                    my $value = "replace";
                    if ($void{value} == $FM_TAG_ADD)
                    {
                        $value = "add";
                    }
                    elsif ($void{value} == $FM_TAG_KEEP)
                    {
                        $value = "keep";
                    }
                    elsif ($void{value} == $FM_TAG_ADD_NO_DEFAULT)
                    {
                        $value = "add-no-def";
                    }
                    printf($attributeWidthStr, $status != $FM_OK ? "NA" : $value);
                }
                elsif ($attribute eq "security_pri")
                {
                    my %void = (type => "fm_uint32", value => 0);
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);
                    my $value = "normal";
                    if ($void{value} == $FM_SECURITY_PRI_HIGH)
                    {
                        $value = "high";
                    }
                    printf($attributeWidthStr, $status != $FM_OK ? "NA" : $value);
                }
                elsif ($attribute eq "security")
                {
                    my %void = (type => "fm_uint32", value => 0);
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);
                    my $value = "shv";
                    if ($void{value} == $FM_PORT_SECURITY_OFF)
                    {
                        $value = "off";
                    }
                    elsif ($void{value} == $FM_PORT_SECURITY_SAV)
                    {
                        $value = "sav";
                    }
                    printf($attributeWidthStr, $status != $FM_OK ? "NA" : $value);
                }
                elsif ($attribute eq "ether_type")
                {
                    my %void = (type => "fm_uint32", value => 0);
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);
                    
                    printf($attributeWidthStr, $status != $FM_OK ? 
                           "NA" : sprintf("%#.4x",$void{value}));
                    
                }
                elsif ($attribute eq "equalizer")
                {
                    if (!defined($FM_PLATFORM_ATTR_EQUALIZER))
                    {
                        printf($attributeWidthStr, "NA");
                    } 
                    else 
                    {
                        my %void = (type => "fm_uint32", value => 0);
                        my $status = $chip->fmPlatformGetAttribute(0, 
                                                       $logicalPort,
                                                       $FM_PLATFORM_ATTR_EQUALIZER,
                                                       \%void);
                        my $value = $void{'value'} & ~$FM_PHY_EQUALIZER_AUTO;
                        printf($attributeWidthStr, $status != $FM_OK ? "NA" : "$value");
                    }
                }
                elsif ($attribute eq "stats_config")
                {
                    my $bits = 
                    {
                        1 => $FM_STAT_GROUP_PER_PORT_RX_FRAME_CLASS,
                        2 => $FM_STAT_GROUP_PER_PORT_RX_COUNT_BY_LEN,
                        3 => $FM_STAT_GROUP_PER_PORT_RX_OCTETS,
                        4 => $FM_STAT_GROUP_PER_PORT_RX_COUNT_BY_PRIORITY,
                        5 => $FM_STAT_GROUP_PER_PORT_RX_OCTETS_BY_PRIORITY,
                        6 => $FM_STAT_GROUP_PER_PORT_FWD_ACTION,
                        7 => $FM_STAT_GROUP_PER_PORT_TX_FRAME_CLASS,
                        8 => $FM_STAT_GROUP_PER_PORT_TX_COUNT_BY_LEN,
                        9 => $FM_STAT_GROUP_PER_PORT_TX_OCTETS,
                        11 => $FM_STAT_GROUP_VLAN_RX_FRAME_CLASS,
                        12 => $FM_STAT_GROUP_VLAN_RX_OCTETS,
                        14 => $FM_STAT_GROUP_EGRESS_ACL,
                    };
    
                    my %void = (type => "fm_uint32", value => 0);
                    my $status = $chip->fmGetPortAttribute($switchNum,
                                                           $logicalPort,
                                                           $FM_PORT_STAT_GROUP_ENABLE,
                                                           \%void);
    
                    my $enabledGroups = 0;
    
                    foreach my $key (keys(%$bits))
                    {
                        if ($void{value} & $bits->{$key})
                        {
                            $enabledGroups |= (1 << $key);
                        }
                    }
    
                    printf($attributeWidthStr, 
                        ($status == $FM_OK) ?
                        $self->stringifyPortMask($switchNum, $enabledGroups) :
                        "NA");
                }
                elsif ($attribute eq "bcast_flooding")
                {
                    my %void = (type => "fm_int", value=> 0);
                    my $result;
    
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);
        
                    if ($void{value} eq $FM_PORT_BCAST_FWD_EXCPU)
                    {
                        $result = "fwd_exc_cpu";
                    }
                    elsif ($void{value} eq $FM_PORT_BCAST_FWD)
                    {
                        $result = "forward";
                    }
                    elsif ($void{value} eq $FM_PORT_BCAST_DISCARD)
                    {
                        $result = "discard";
                    }
                    else
                    {
                        $result = "NA";
                    }
    
                    printf($attributeWidthStr, $status != $FM_OK ? "NA" : $result);

                }
                elsif ($attribute eq "mcast_flooding")
                {
                    my %void = (type => "fm_int", value=> 0);
                    my $result;
    
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);
    
                    if ($void{value} eq $FM_PORT_MCAST_FWD)
                    {
                        $result = "forward";
                    }
                    elsif ($void{value} eq $FM_PORT_MCAST_TRAP)
                    {
                        $result = "trap"
                    }
                    elsif ($void{value} eq $FM_PORT_MCAST_LOG)
                    {
                        $result = "log";
                    }
                    elsif ($void{value} eq $FM_PORT_MCAST_DISCARD)
                    {
                        $result = "discard";
                    }
                    else
                    {
                        $result = "NA";
                    }
    
                    printf($attributeWidthStr, $status != $FM_OK ? "NA" : $result);
                }
                elsif ($attribute eq "eth_mode")
                {
                    my %void = (type => "fm_int", value => 0);
                    my $result;
    
                    my $status = 
                              $chip->fmGetPortAttributeV2($switchNum, 
                                                          $logicalPort,
                                                          $portMac,
                                                          $lane,
                                                          $port_attr_map{$attribute},
                                                          \%void);
                              
                    $result = $port_ethMode_reverse_map{$void{value}};
                    printf($attributeWidthStr, $status != $FM_OK ? "NA" : $result);
                }
                elsif ($attribute eq "tx_tc_private_wm")
                {
                    printf($attributeWidthStr, "NA");
                }
                elsif ($attribute eq "rx_lane_polarity")
                {
                    my %void = (type => "fm_int", value=> 0);
                    my $status;
                    my $polarity = 0;
                    
                    # different processing for the FM6000
                    if ( ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000) ||
                         ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_REMOTE_FM6000 ) )
                    {
                        # different processing depending on eth_mode
                        my $eth_mode;

                        $status = $chip->fmGetPortAttribute($switchNum, 
                                                            $logicalPort,
                                                            $FM_PORT_ETHERNET_INTERFACE_MODE,
                                                            \%void);
                        if ( $status == $FM_OK )
                        {
                            my $maxLane = 0;

                            # is this a single- or multi-lane interface?
                            if ( ($void{value} & $FM_ETH_MODE_4_LANE_BIT_MASK) != 0 )
                            {
                                $maxLane = 3;
                            }

                            # get polarity values for all applicable lanes
                            for ( my $i = 0; $i <= $maxLane; $i++ )
                            {
                                $status = 
                                    $chip->fmGetPortAttributeV2($switchNum,
                                                                $logicalPort,
                                                                $portMac,
                                                                $i,
                                                                $FM_PORT_RX_LANE_POLARITY,
                                                                \%void );
                                if ( $status != $FM_OK )
                                {
                                    last;
                                }
                                $polarity |=  $void{value};
                            }
                        }
                    }
                    else
                    {
                        # lane polarity is retrieved per-port for non-FM6000 devices
                        $status = $chip->fmGetPortAttribute( $switchNum, 
                                                             $logicalPort,
                                                             $port_attr_map{$attribute},
                                                             \%void );
                        if ( $status == $FM_OK )
                        {
                            $polarity = $void{value};
                        }
                    }

                    printf($attributeWidthStr, 
                           $status != $FM_OK ? 
                           "NA" : 
                           sprintf("0x%x", $polarity));

                }
                elsif ($attribute eq "rx_lane_ordering")
                {
                    my %void = (type => "fm_int", value=> 0);
                    my $result;

                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);

                    $result = ($void{value} == $FM_PORT_LANE_ORDERING_NORMAL ?
                             "normal" : "reversed");

                    printf($attributeWidthStr, 
                           $status != $FM_OK ? 
                           "NA" : 
                           $result);
                }
                elsif ($attribute eq "tx_fcs_mode")
                {
                    my %void = (type => "fm_uint32", value => 0);
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);
                    my $value = "ERROR";
                    if ($void{value} == $FM_FCS_PASSTHRU)
                    {
                        $value = "passthru";
                    }
                    elsif ($void{value} == $FM_FCS_INSERT_NORMAL)
                    {
                        $value = "insert_normal";
                    }
                    elsif ($void{value} == $FM_FCS_REPLACE_NORMAL)
                    {
                        $value = "replace_normal";
                    }
                    printf($attributeWidthStr, $status != $FM_OK ? "NA" : $value);
                }
                elsif ($attribute eq "tx_lane_polarity")
                {
                    my %void = (type => "fm_int", value=> 0);
                    my $status;

                    # different processing for the FM6000
                    if ( ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000) ||
                         ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_REMOTE_FM6000 ) )
                    {
                        # different processing depending on eth_mode
                        my $polarity = 0;
                        my $eth_mode;

                        $status = $chip->fmGetPortAttribute($switchNum, 
                                                            $logicalPort,
                                                            $FM_PORT_ETHERNET_INTERFACE_MODE,
                                                            \%void);
                        if ( $status == $FM_OK )
                        {
                            my $maxLane = 0;

                            # is this a single- or multi-lane interface?
                            if ( ($void{value} & $FM_ETH_MODE_4_LANE_BIT_MASK) != 0 )
                            {
                                $maxLane = 3;
                            }

                            # get polarity values for all applicable lanes
                            for ( my $i = 0; $i <= $maxLane; $i++ )
                            {
                                $status = 
                                    $chip->fmGetPortAttributeV2($switchNum,
                                                                $logicalPort,
                                                                $portMac,
                                                                $i,
                                                                $FM_PORT_TX_LANE_POLARITY,
                                                                \%void );
                                if ( $status != $FM_OK )
                                {
                                    last;
                                }
                                $polarity |=  $void{value};
                            }
                        }

                        printf($attributeWidthStr, 
                               $status != $FM_OK ? 
                               "NA" : 
                               sprintf("0x%x", $polarity));

                    }
                    else
                    {
                        my $result;

                        # lane polarity is retrieved per-port for non-FM6000 devices
                        my $status = 
                                  $chip->fmGetPortAttribute( $switchNum, 
                                                             $logicalPort,
                                                             $port_attr_map{$attribute},
                                                             \%void );

                        $result = ($void{value} == $FM_PORT_LANE_POLARITY_NORMAL ?
                                 "normal" : "reversed");

                        printf($attributeWidthStr, 
                               $status != $FM_OK ? 
                               "NA" : 
                               $result);

                    }
                }
                elsif ($attribute eq "tx_lane_ordering")
                {
                    my %void = (type => "fm_int", value=> 0);
                    my $result;

                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);

                    $result = ($void{value} == $FM_PORT_LANE_ORDERING_NORMAL ?
                             "normal" : "reversed");

                    printf($attributeWidthStr, 
                           $status != $FM_OK ? 
                           "NA" : 
                           $result);
                }
                elsif ($attribute eq "pause_resend_time")
                {
                    my %void = (type => "fm_int", value=> 0);

                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);

                    printf($attributeWidthStr, 
                           $status != $FM_OK ? 
                           "NA" : 
                           sprintf("%d",$void{value}));
                }
                elsif ($attribute eq "pause_mac_addr")
                {
                    my %void = (type => "fm_macaddr", value => "11:22:33:44:55:66");

                    
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);

                    printf($attributeWidthStr, 
                           $status != $FM_OK ? 
                           "NA" : 
                           $void{value});
                }
                elsif ($attribute eq "vlan_tag_a" || 
                       $attribute eq "vlan_tag_b")
                {
                    my %void = (type => "fm_uint32", value => 0);
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);
                    
                    printf($attributeWidthStr, $status != $FM_OK ? 
                           "NA" : sprintf("%#.4x",$void{value}));
                }
                elsif ($attribute eq 'deep_protocol_a' ||
                       $attribute eq 'deep_protocol_b')
                {
                    my %void = (type => "fm_uint32", value => 0);
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);
                    
                    printf($attributeWidthStr, $status != $FM_OK ? 
                           "NA" : sprintf("0x%.2x",$void{value}));
                }
                elsif ($attribute eq 'parse_tcp_payload' ||
                       $attribute eq 'parse_udp_payload' ||
                       $attribute eq 'parse_a_payload' ||
                       $attribute eq 'parse_b_payload')
                {
                    my %void = (type => "fm_uint64", value => Math::BigInt->new(0));
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);
                    
                    printf($attributeWidthStr, $status != $FM_OK ? 
                           "NA" : $void{value}->as_hex());
                }
                elsif ($attribute eq 'ucast_flooding')
                {
                    my %void = (type => "fm_int", value => 0);
    
                    my $status = 
                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                        $port_attr_map{$attribute},
                                                        \%void);

                    my $str;

                    if ($void{value} == $FM_PORT_UCAST_FWD)
                    {
                        $str = "forward";
                    }
                    elsif ($void{value} == $FM_PORT_UCAST_TRAP)
                    {
                        $str = "trap";
                    }
                    elsif ($void{value} == $FM_PORT_UCAST_LOG)
                    {
                        $str = "log";
                    }
                    elsif ($void{value} == $FM_PORT_UCAST_DISCARD)
                    {
                        $str = "discard";
                    }

                    printf($attributeWidthStr, $status != $FM_OK ? "NA" : $str);
                }   
                elsif ($attribute eq "dfe_mode")
                {
                    my $value;
                    my %void = (type => "fm_int", value => 0);

                    # Currently displaing value only for lane 0
                    # regardless of the current eth_mode
                    # TODO: identify an output layout that is good
                    # for ports configured in multi-lane modes 
                    my $status = 
                    $chip->fmGetPortAttributeV2($switchNum, $logicalPort,
                                               $portMac,   0,
                                               $port_attr_map{$attribute},
                                               \%void);
                    if ( $void{value} == $FM_DFE_MODE_STATIC )
                    {
                        $value = "static";
                    }
                    elsif ( $void{value} == $FM_DFE_MODE_ONE_SHOT )
                    {
                        $value = "one_shot";
                    }
                    elsif ( $void{value} == $FM_DFE_MODE_CONTINUOUS )
                    {
                        $value = "continuous";
                    }
                    else
                    {
                        $value = "kr";
                    }
                    printf( $attributeWidthStr, 
                            $status != $FM_OK ? "NA" : $value);
                }
                elsif ($attribute eq "dfe_params")
                {
                    my %void = (type => "fm_uint32", value => 0);

                    # Currently displaing value only for lane 0
                    # regardless of the current eth_mode
                    # TODO: identify an output layout that is good
                    # for ports configured in multi-lane modes 
                    my $status = 
                              $chip->fmGetPortAttributeV2($switchNum, 
                                                        $logicalPort,
                                                        $portMac, 0,
                                                        $port_attr_map{$attribute},
                                                        \%void);
                    printf($attributeWidthStr, 
                           $status != $FM_OK ? 
                           "NA" : 
                           sprintf("0x%08x", $void{value}));
                }
                elsif ($attribute eq "low_eyescore_mode")
                {
                    my $value;
                    my %void = (type => "fm_int", value => 0);

                    # Currently displaing value only for lane 0
                    # regardless of the current eth_mode
                    # TODO: identify an output layout that is good
                    # for ports configured in multi-lane modes 
                    my $status = 
                    $chip->fmGetPortAttributeV2($switchNum, $logicalPort,
                                                $portMac,   0,
                                                $port_attr_map{$attribute},
                                                \%void);

                    if ( $void{value} == $FM_PORT_LOW_EYE_SCORE_RECOVERY_OFF )
                    {
                        $value = "disabled";
                    }
                    elsif ( $void{value} == $FM_PORT_LOW_EYE_SCORE_SOFT_RECOVERY )
                    {
                        $value = "soft";
                    }
                    else
                    {
                        $value = "hard";
                    }
                    printf( $attributeWidthStr, 
                            $status != $FM_OK ? "NA" : $value);
                }
                elsif ($attribute eq "low_eyescore_threshold" || 
                       $attribute eq "low_eyescore_timeout")
                {
                    my %void = (type => "fm_int", value => 0);

                    # Currently displaing value only for lane 0
                    # regardless of the current eth_mode
                    # TODO: identify an output layout that is good
                    # for ports configured in multi-lane modes 
                    my $status = 
                              $chip->fmGetPortAttributeV2($switchNum, 
                                                          $logicalPort,
                                                          $portMac, 0,
                                                          $port_attr_map{$attribute},
                                                          \%void);
                    printf($attributeWidthStr, 
                           $status != $FM_OK ? 
                           "NA" : 
                           sprintf("%d", $void{value}));
                }
                elsif ($attribute eq "slew")
                {
                    my %void = (type => "fm_int", value => 0);

                    # Currently displaing value only for lane 0
                    # regardless of the current eth_mode
                    # TODO: identify an output layout that is good
                    # for ports configured in multi-lane modes 
                    my $status = 
                              $chip->fmGetPortAttributeV2($switchNum, 
                                                        $logicalPort,
                                                        $portMac, 0,
                                                        $port_attr_map{$attribute},
                                                        \%void);
                    printf($attributeWidthStr, 
                           $status != $FM_OK ? 
                           "NA" : 
                           sprintf("%d", $void{value}));
                }
                elsif ($attribute eq "shaper_ifg_penalty")
                {
                    my %void = (type => "fm_int", value => 0);
                    my $status = 
                              $chip->fmGetPortQOS($switchNum, $logicalPort,
                                                  $port_attr_map{$attribute}, 0,
                                                  \%void);
                    printf($attributeWidthStr, 
                           $status != $FM_OK ? 
                           "NA" : 
                           sprintf("%d", $void{value}));
                }
#                elsif ($attribute eq "eye_score")
#                {
#                    my %void = (type => "fm_uint32", value => 0);
#                    my $status = 
#                              $chip->fmGetPortAttribute($switchNum, $logicalPort,
#                                                        $port_attr_map{$attribute},
#                                                        \%void);
#                    printf($attributeWidthStr, 
#                           $status != $FM_OK ? 
#                           "NA" : 
#                           sprintf("%u (0..64)", $void{value}));
#                }
#                elsif ( $attribute eq "dfe_state (coarse)" || 
#                        $attribute eq "dfe_state (fine)"   )
#                {
#                    my $value;
#                    my %void = (type => "fm_int", value => 0);
#                    my $status = 
#                    $chip->fmGetPortAttribute($switchNum, 
#                                              $logicalPort,
#                                              $port_attr_map{$attribute},
#                                              \%void);
#                    if ( $void{value} == $FM_PORT_DFE_STATE_NOT_STARTED )
#                    {
#                        $value = "Not started";
#                    }
#                    elsif ( $void{value} == $FM_PORT_DFE_STATE_IN_PROGRESS )
#                    {
#                        $value = "In progress";
#                    }
#                    elsif ( $void{value} == $FM_PORT_DFE_STATE_COMPLETE )
#                    {
#                        $value = "Complete";
#                    }
#                    else
#                    {
#                        $value = "Error";
#                    }
#                    printf( $attributeWidthStr, 
#                            $status != $FM_OK ? "NA" : $value);
#                }
#
                elsif ($attribute eq "signal_threshold")
                {
                    my %void = (type => "fm_int", value => 0);

                    # Currently displaing value only for lane 0
                    # regardless of the current eth_mode
                    # TODO: identify an output layout that is good
                    # for ports configured in multi-lane modes 
                    my $status = 
                        $chip->fmGetPortAttributeV2($switchNum, 
                                                  $logicalPort,
                                                  $portMac, 0,
                                                  $port_attr_map{$attribute},
                                                  \%void);
                    printf($attributeWidthStr, 
                           $status != $FM_OK ? 
                           "NA" : 
                           sprintf("%u (0..64)", $void{value}));
                }                

                elsif ($attribute eq "trill_hop_count")
                {
                    my $hopCnt = 0;

                    my $status = $chip->fmGetRBridgePortHopCount($sw, 
                                                                 $logicalPort, 
                                                                 \$hopCnt);

                    printf($attributeWidthStr, 
                           $status != $FM_OK ? 
                           "NA" : 
                           sprintf("%d", $hopCnt));
                }

            }
            print("\n");
        }
        print("\n");
    }
    
}   # end handleShowPortConfig

##@cmethod public void handleShowPortEye(char *port)
#
# @desc         Handles showing the eye height and width
#               Can't be a LAG logical port.   
#
# @param[in]    port The set of ports whose eye is to be shown
sub handleShowPortEye
{
    my ($self, $port) = @_;

    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    my @indices = arrayFind(map {$self->tpPlatformIsCPUPort($_)} @globalPortList);
    if ($port ne "all" && scalar(@indices) > 0)
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));

        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        printf("%s\n", scalar($self->tpGetSwitches) > 1 ? "\nSwitch $sw:" : " ");
        print("PORT SPEED    STATE     EYE HEIGHT/WIDTH\n");
        print("---- ----- ------------ ----------------\n");
        
        foreach my $port (@portList)
        {
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            my $switchInfo;
            my $eth_mode;
            my $multiLane = 0;
            
            next if $self->tpPlatformIsCPUPort($port);

            $switchInfo = new SDK::fm_switchInfo();
            $chip->fmGetSwitchInfo($sw, $switchInfo);

            my @info = (0) x 4;
            my ($mode, $state);
            my %void;

            $chip->fmGetPortState($sw, $logPort, \$mode, \$state, \@info);

            %void = (type => "fm_uint32", value => 0);

            # skip FM6000 ports that are disabled 
            $eth_mode = -1;
            $chip->fmGetPortAttribute( $sw, $port,
                                       $FM_PORT_ETHERNET_INTERFACE_MODE,
                                       \%void );
            $eth_mode = $void{value};


            $chip->fmGetPortAttribute($sw, $logPort, $FM_PORT_SPEED, \%void);
            my $speed = "UNKNOWN";
            if ($void{value} == 10000)
            {
                $speed = "10G";
            }
            elsif ($void{value} == 2500)
            {
                $speed = "2.5G";
            }
            elsif ($void{value} == 1000)
            {
                $speed = "1G";
            }
            elsif ($void{value} == 100)
            {
                $speed = "100M"
            }
            elsif ($void{value} == 10)
            {
                $speed = "10M";
            }
            elsif ($void{value} == 20000)
            {
                $speed = "20G";
            }
            elsif ($void{value} == 40000)
            {
                $speed = "40G";
            }
            else
            {
                $speed = "---";
            }

            printf("%-4s %-5s ", $port, $speed);

            my $laneStatus = 1;
            if ($eth_mode == $FM_ETH_MODE_DISABLED)
            {
                print("DISABLED\n");
                next;
            }
            elsif ($state == $FM_PORT_STATE_UP)
            {
                print("UP          ");
            }
            elsif ($state == $FM_PORT_STATE_ADMIN_DOWN)
            {
                print("Admin DOWN  ");
            }
            elsif ($state == $FM_PORT_STATE_ADMIN_PWRDOWN)
            {
                print("Power DOWN  ");
            }
            elsif ($state == $FM_PORT_STATE_BIST)
            {
                print("BIST        ");
                $laneStatus = 0;
                foreach my $pInfo (@info)
                {
                    printf(" %6d",$pInfo);
                }
            }
            elsif ($state == $FM_PORT_STATE_REMOTE_FAULT)
            {
                print("Remote Fault");
            }
            elsif ($state == $FM_PORT_STATE_DOWN)
            {
                print("DOWN        ");
            }
            elsif ($state == $FM_PORT_STATE_PARTIALLY_UP)
            {
                print("Partially UP");
            }
            elsif ($state == $FM_PORT_STATE_LOCAL_FAULT)
            {
                print("Local Fault ");
            }
            else
            {
                print("Unrecognized");
                $laneStatus = 0;
            }
                
            my $minLane;
            my $maxLane;
            print " ";
            
            if ( $eth_mode == $FM_ETH_MODE_SGMII       ||
                 $eth_mode == $FM_ETH_MODE_1000BASE_X  || 
                 $eth_mode == $FM_ETH_MODE_1000BASE_KX ||
                 $eth_mode == $FM_ETH_MODE_2500BASE_X  ||
                 $eth_mode == $FM_ETH_MODE_6GBASE_KR   ||
                 $eth_mode == $FM_ETH_MODE_6GBASE_CR   ||
                 $eth_mode == $FM_ETH_MODE_10GBASE_KR  ||
                 $eth_mode == $FM_ETH_MODE_10GBASE_CR  ||
                 $eth_mode == $FM_ETH_MODE_10GBASE_SR  ||
                 $eth_mode == $FM_ETH_MODE_AN_73)
            {
                $minLane = 0;
                $maxLane = 0;
            }
            elsif ( $eth_mode == $FM_ETH_MODE_XAUI ||
                    $eth_mode == $FM_ETH_MODE_10GBASE_KX4 ||
                    $eth_mode == $FM_ETH_MODE_10GBASE_CX4 ||
                    $eth_mode == $FM_ETH_MODE_24GBASE_KR4 ||
                    $eth_mode == $FM_ETH_MODE_24GBASE_CR4 ||
                    $eth_mode == $FM_ETH_MODE_40GBASE_KR4 ||
                    $eth_mode == $FM_ETH_MODE_40GBASE_CR4 ||
                    $eth_mode == $FM_ETH_MODE_40GBASE_SR4 ||
                    $eth_mode == $FM_ETH_MODE_XLAUI )
            {              
                $minLane   = 0;
                $maxLane   = 3;
                $multiLane = 1;
            }
            else
            {
                # this shouldn't happen, but prevent any further processing 
                # if it does
               
                $minLane =  0;
                $maxLane = -1;
            }

            if ( $multiLane )
            {
                print("\n");
            }

            for ( my $i = $minLane ; $i <= $maxLane ; $i++ )
            {
                if ( $multiLane)
                {      
                    printf(" %-21s  ", "Lane ".$i);
                }

                # Eye Score
                %void = (type => "fm_uint32", value => 0);
                $chip->fmGetPortAttributeV2( $sw, 
                                             $port, 
                                             $FM_PORT_ACTIVE_MAC,
                                             $i,
                                             $FM_PORT_EYE_SCORE, 
                                             \%void );
                my $eyeScore = $void{value};
                
                printf("%02s/",  (($eyeScore & 0xFF) == 255) ? "NA" : 
                                ($eyeScore & 0xFF));
                printf("%02s\n", (($eyeScore >> 8)   == 255) ? "NA" : 
                                ($eyeScore >> 8));
            }

        }   # end foreach my $port (@portList)
        
    }   # end foreach my $sw ($self->tpGetSwitches)
    
    print("\n");
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    
}   # end handleShowPortEye


##@cmethod public int[] handleShowPortmap(int sw, int port)
#
# @desc         Show port mapping for a given logical port
#
# @return       None 
sub handleShowPortmap
{
    my ($self, $port, $portType) = @_;

    my $chip = $self->{CHIP};

    if (!defined($port))
    {
        $port = -1;
    }

    if (!defined($portType))
    {
        $portType = 0;
    }

    if ($port eq "all")
    {
        $port = -1;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my $status = $chip->fmDbgDumpPortMapV2($sw, $port, $portType);

        if ($status != $FM_OK)
        {
            printf("ERROR: Switch #%d: %d=%s\n", $sw, $status,
                   $chip->fmErrorMsg($status));
            return $status;
        }
    }

}   # end handleShowPortmap



##@cmethod public int[] handleShowPortmapPhysical(int sw, int port)
#
# @desc         Show port mapping for a given physical port
#
# @return       None 
sub handleShowPortmapPhysical
{
    my ($self, $port) = @_;

    my $chip = $self->{CHIP};

    my $portType = 1;

    if (!defined($port))
    {
        $port = -1;
    }

    if ($port eq "all")
    {
        $port = -1;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my $status = $chip->fmDbgDumpPortMapV2($sw, $port, $portType);

        if ($status != $FM_OK)
        {
            printf("ERROR: Switch #%d: %d=%s\n", $sw, $status,
                   $chip->fmErrorMsg($status));
            return $status;
        }
    }

}   # end handleShowPortmapPhysical



##@cmethod public int[] handleShowSwitchState(int sw)
#
# @desc         Show switch state for a given switch
#
# @return       None 
sub handleShowSwitchState
{
    my ($self,$sw) = @_;
    my $chip = $self->{CHIP};
#    my $state = new SDK::fm_switchState();
    my $state;

    if (!defined($sw))
    {
        $sw = "0..$FM_MAX_NUM_SWITCHES";
    }

    my @swList = $self->validateList($sw, 0, $FM_MAX_NUM_SWITCHES);
    if (!defined($sw) || (scalar(@swList) == 0))
    {
        print("Must specify a valid switch or switch range!\n");
        return;
    }

    printf("\n");
    printf("Sw         State\n");
    printf("====  ================\n");

    $chip->disableErrors();

    foreach my $s (@swList)
    {
        $state = $FM_SWITCH_STATE_UNKNOWN;
        $chip->fmGetSwitchStateExt($s, \$state);

        printf("%3d    ",$s);

        if ($state == $FM_SWITCH_STATE_UNKNOWN) { print "NOT PRESENT" }
        elsif ($state == $FM_SWITCH_STATE_DOWN) { print "DOWN" }
        elsif ($state == $FM_SWITCH_STATE_INIT) { print "INIT" }
        elsif ($state == $FM_SWITCH_STATE_BOOT_DONE) { print "BOOT DONE" }
        elsif ($state == $FM_SWITCH_STATE_UP) { print "UP" }
        elsif ($state == $FM_SWITCH_STATE_GOING_DOWN) { print "GOING DOWN" }
        elsif ($state == $FM_SWITCH_STATE_FAILED) { print "FAIL" }
        else		{ print "INVALID" }
        print "\n";

    }
    $chip->enableErrors();
    
}

##@cmethod public int[] handleSetSwitchState(int sw)
#
# @desc         Show switch state for a given switch
#
# @return       None 
sub handleSetSwitchState
{
    my ($self,$sw,$state) = @_;
    my $chip = $self->{CHIP};

    if (!defined($state) || (($state ne "up") && ($state ne "down")))
    {
        print("Must specify a valid state (up|down)!\n");
        return;
    }

    if (!defined($sw))
    {
        $sw = "0..$FM_MAX_NUM_SWITCHES";
    }

    my @swList = $self->validateList($sw, 0, $FM_MAX_NUM_SWITCHES);
    if (!defined($sw) || (scalar(@swList) == 0))
    {
        print("Must specify a valid switch or switch range!\n");
        return;
    }

    foreach my $s (@swList)
    {
        if ($state eq "up")
        {
            printf("Bringing switch $s up...\n");
            if ($chip->fmSetSwitchState($s, $FM_ENABLED))
            {
                printf("Failed to bring up switch\n");
            }
        }
        elsif($state eq "down")
        {
            printf("Bringing switch $s down...\n");
            if ($chip->fmSetSwitchState($s, $FM_DISABLED))
            {
                printf("Failed to bring down switch\n");
            }
        }
    }
    
    # update the switch list
    $chip->SetSwitches();
    if ($state eq "up")
    {
        $chip->SetSwitchInfo();
        # load the register set, if not loaded
        $chip->SetRegisters();
    }

}   # end handleSetSwitchState


##@cmethod public void tpHandleConfigIGMPSnooping(char *mode)
#
# @desc         Handles setting the global IGMP snooping flag
#
# @param[in]    mode The mode to set, on or off
#
sub tpHandleConfigIGMPSnooping
{
    my ($self, $mode) = @_;

    my $chip = $self->{CHIP};
    my $type;

    if (!defined($mode) || (($mode ne "on") && ($mode ne "off")))
    {
        print("Must specify a valid mode!\n");
        return;
    }
    $mode = $mode eq "on" ? $FM_ENABLED : $FM_DISABLED;

    foreach my $switchNum ($self->tpGetSwitches)
    {
        $chip->fmGetIgmpType($switchNum, \$type);

        if ($mode == $FM_ENABLED && $type == $FM_MCAST_ADDR_TYPE_UNKNOWN)
        {
            #Default the igmp_snooping_type to dmac-vlan
            $chip->fmSetIgmpType($switchNum, $FM_MCAST_ADDR_TYPE_L2MAC_VLAN);
        }
        
        $chip->fmSetPacketState($switchNum, $FM_PACKET_TYPE_IGMP, $mode);
    }
}

##@cmethod public void tpHandleConfigIGMPSnoopingType(char *mode)
#
# @desc         Handles setting the global IGMP snooping type
#
# @param[in]    type The type to set: dip, dip-sip, dip-sip-vlan, dip-vlan, dmac-vlan
#
sub tpHandleConfigIGMPSnoopingType
{
    my ($self, $type) = @_;

    my $chip = $self->{CHIP};
    my $result = undef;

    foreach my $igmpType (keys %igmp_snoop_map)
    {
        if ($type eq $igmpType)
        {
            $result = $igmp_snoop_map{$igmpType};
        }
    }
    
    if (!defined($result) || $result == $FM_MCAST_ADDR_TYPE_UNKNOWN)
    {
        printf("$type is not a valid type\n");
        return;
    }
        
    foreach my $switchNum ($self->tpGetSwitches)
    {
        $chip->fmSetIgmpType($switchNum, $result);
    }
}

##@cmethod public void tpHandleConfigLLDP(char *mode)
#
# @desc         Handles setting the global LLDP flag
#
# @param[in]    mode The mode to set, on or off
#
sub tpHandleConfigLLDP
{
    my ($self, $mode) = @_;

    my $chip = $self->{CHIP};

    if (!defined($mode) || (($mode ne "on") && ($mode ne "off")))
    {
        print("Must specify a valid mode!\n");
        return;
    }
    $mode = $mode eq "on" ? $FM_ENABLED : $FM_DISABLED;

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        $chip->fmSetPacketState($switchNum, $FM_PACKET_TYPE_LLDP, $mode);
    }
}

##@cmethod public void tpHandleConfigARPSnooping(char *mode)
#
# @desc         Handles setting the global ARP snooping flag
#
# @param[in]    mode The mode to set, on or off
#
sub tpHandleConfigARPSnooping
{
    my ($self, $mode) = @_;

    my $chip = $self->{CHIP};

    if (!defined($mode) || (($mode ne "on") && ($mode ne "off")))
    {
        print("Must specify a valid mode!\n");
        return;
    }
    $mode = $mode eq "on" ? $FM_ENABLED : $FM_DISABLED;

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        $chip->fmSetPacketState($switchNum, $FM_PACKET_TYPE_ARP, $mode);
    }
}

##@cmethod public void handleSetSwitchConfig(char *attribute, void *input)
#
# @desc         Handles setting switch attributes
#
# @param[in]    attribute The attribute to be set
#
# @param[in]    input The value the attribute is to be set to
sub handleSetSwitchConfig
{
    my ($self, $attribute, $input, $otherInput) = @_;

    if ($attributesInitialized == $FALSE)
    {
        $self->tpInitAttributes();
    }

    my $chip = $self->{CHIP};

    if (!defined($attribute) || !exists($sw_attr_map{$attribute}))
    {
        print("Must specify a valid switch attribute!\n");
        return;
    }

    if (!defined($input))
    {
        print("Must specify an attribute value!\n");
        return;
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        if ($attribute eq "vlan_mode")
        {
            # FIXME: The "vlan_mode" attribute is deprecated in SDK2.0.
            my %void = (type => 'fm_int');
            if ($input eq "802.1q")
            {
                $void{value} = $FM_VLAN_MODE_8021Q;
            }
            else
            {
                $void{value} = $FM_VLAN_MODE_PORT;
            }
            $chip->fmSetSwitchAttribute($switchNum, $sw_attr_map{$attribute},
                                        \%void);
        }
        elsif ($attribute eq "mtu_list")
        {
            my $mtuEntry = SDK::fm_mtuEntry->new();
            my %void = (type => 'fm_mtuEntry', value => $mtuEntry);

            $mtuEntry->{'index'} = $input;    
            $mtuEntry->{'mtu'} = $otherInput;    

            $chip->fmSetSwitchAttribute($switchNum,
                                        $sw_attr_map{$attribute},
                                        \%void);
        }
        elsif ($attribute eq "spanning-tree")
        {
            my %void = (type => 'fm_int', value => $FM_SPANNING_TREE_SHARED);

            if ($input eq "shared")
            {
                $void{value} = $FM_SPANNING_TREE_SHARED;
            }
            elsif ($input eq "per-vlan")
            {
                $void{value} = $FM_SPANNING_TREE_PER_VLAN;
            }
            elsif ($input eq "multiple")
            {
                $void{value} = $FM_SPANNING_TREE_MULTIPLE;
            }
            $chip->fmSetSwitchAttribute($switchNum, $sw_attr_map{$attribute},
                                        \%void);
        }
        elsif ($attribute eq "vlan-learning")
        {
            my %void = (type => 'fm_int', value => $FM_VLAN_LEARNING_MODE);

            if ($input eq "shared")
            {
                $void{value} = $FM_VLAN_LEARNING_MODE_SHARED;
            }
            elsif ($input eq "independent")
            {
                $void{value} = $FM_VLAN_LEARNING_MODE_INDEPENDENT;
            }
            $chip->fmSetSwitchAttribute($switchNum, $sw_attr_map{$attribute},
                                        \%void);
        }
        elsif ($attribute eq "shared-fid")
        {
            my %void = (type => 'fm_int', value => $input);

            $void{value} = $input;
            $chip->fmSetSwitchAttribute($switchNum, $sw_attr_map{$attribute},
                                        \%void);
        }
        elsif ($attribute eq "ip_options_disp")
        {
            my %void = (type => 'fm_int', value => 0);
            
            foreach my $item (split(/,/, $input))
            {
                if ($item eq "trap_all")
                {
                    $void{value} |= $FM_IP_OPTIONS_TRAP;
                }
                elsif ($item eq "log_unicast")
                {
                    $void{value} |= $FM_IP_OPTIONS_LOG_UCST;
                }
                elsif ($item eq "trap_unicast")
                {
                    $void{value} |= $FM_IP_OPTIONS_TRAP_UCST;
                }
                elsif ($item eq "trap_multicast")
                {
                    $void{value} |= $FM_IP_OPTIONS_TRAP_MCST;
                }
                elsif ($item eq "forward")
                {
                    $void{value} |= $FM_IP_OPTIONS_FWD;
                }
                else
                {
                    print("Must specify a valid ip option " .
                          "disposition mode! ($item is not valid)\n");
                    return;
                }
            }

            $chip->fmSetSwitchAttribute($switchNum, 
                                        $sw_attr_map{$attribute},
                                        \%void);
        }
        elsif ( ($attribute eq "mstp") ||
                ($attribute eq "learn_drop_frame") ||
                ($attribute eq "mirror_counters") )
        {
            my %void = (type => 'fm_bool');
            if ($input eq "on")
            {
                $void{value} = $FM_ENABLED;
            }
            else
            {
                $void{value} = $FM_DISABLED;
            }
            $chip->fmSetSwitchAttribute($switchNum, $sw_attr_map{$attribute},
                                        \%void);
        }
        elsif ( ($attribute eq "cpu_mac") ||
                ($attribute eq "rbridge_mac") )
        {
            if ($self->validateL2Address($input) != $FM_OK)
            {
                print("Must specify a valid MAC address!\n");
                return;
            }
            my %void = (type => 'fm_macaddr', value => $input);
            $chip->fmSetSwitchAttribute($switchNum, $sw_attr_map{$attribute},
                                        \%void);
        }
        elsif ($attribute eq "flood_bcast")
        {
            my %void = (type => 'fm_int');
            if ($input eq "discard")
            {
                $void{value} = $FM_BCAST_DISCARD;
            }
            elsif ($input eq "fwd")
            {
                $void{value} = $FM_BCAST_FWD;
            }
            elsif ($input eq "fwd_exc_cpu")
            {
                $void{value} = $FM_BCAST_FWD_EXCPU;
            }
            elsif ($input eq "per-port")
            {
                $void{value} = $FM_BCAST_FLOODING_PER_PORT;
            }
            else
            {
                print("Must specify a valid forwarding mode!\n");
                return;
            }
            $chip->fmSetSwitchAttribute($switchNum, $sw_attr_map{$attribute},
                                        \%void);
        }
        elsif ($attribute eq "flood_mcast")
        {
            my %void = (type => 'fm_int');
            if ($input eq "discard")
            {
                $void{value} = $FM_MCAST_DISCARD;
            }
            elsif ($input eq "fwd")
            {
                $void{value} = $FM_MCAST_FWD ;
            }
            elsif ($input eq "per-port")
            {
                $void{value} = $FM_MCAST_FLOODING_PER_PORT ;
            }
            else
           {
                print("Must specify a valid forwarding mode!\n");
                return;
            }
            $chip->fmSetSwitchAttribute($switchNum, $sw_attr_map{$attribute},
                                        \%void);
        }
        elsif ($attribute eq "flood_ucast")
        {
            my %void = (type => 'fm_int');
            if ($input eq "discard")
            {
                $void{value} = $FM_UCAST_DISCARD;
            }
            elsif ($input eq "fwd")
            {
                $void{value} = $FM_UCAST_FWD ;
            }
            elsif ($input eq "per-port")
            {
                $void{value} = $FM_UCAST_FLOODING_PER_PORT ;
            }
            else
           {
                print("Must specify a valid forwarding mode!\n");
                return;
            }
            $chip->fmSetSwitchAttribute($switchNum, $sw_attr_map{$attribute},
                                        \%void);
        }
        elsif ($attribute eq "frame_age_time")
        {
            my %void = (type => 'fm_int');
            if ($input eq "off")
            {
                $void{value} = 0;
            }
            else
            {
                $void{value} =  $self->str2intnum($input);
                if (!$void{value})
                {
                    print("need a positive integer\n");
                    return;
                }
            }
            $chip->fmSetSwitchAttribute($switchNum, $sw_attr_map{$attribute},
                                        \%void);
        }
        elsif ($attribute eq "lacp_disp")
        {
            my %void = (type => 'fm_int');
            if ($input eq "discard")
            {
                $void{value} = $FM_PDU_DISCARD;
            }
            elsif ($input eq "fwd")
            {
                $void{value} = $FM_PDU_FORWARD ;
            }
            elsif ($input eq "tocpu")
            {
                $void{value} = $FM_PDU_TOCPU ;
            }
            else
            {
                print("Must specify a valid disposition mode!\n");
                return;
            }
            $chip->fmSetLAGAttributeExt($switchNum, $sw_attr_map{$attribute},0,
                                        \%void);
        }
        elsif ($attribute eq "lbg_mode")
        {
            my %void = (type => 'fm_int');
            if ($input eq "mapped")
            {
                $void{value} = $FM_LBG_MODE_MAPPED;
            }
            elsif ($input eq "nplus1")
            {
                $void{value} = $FM_LBG_MODE_NPLUS1;
            }
            elsif ($input eq "redirect")
            {
                $void{value} = $FM_LBG_MODE_REDIRECT;
            }
            else
            {
                print "'$input' is not a valid $attribute!\n";
                return;
            }
            $chip->fmSetSwitchAttribute($switchNum,
                                        $sw_attr_map{$attribute},
                                        \%void);
        }
        elsif ($attribute eq "ifg_penalty")
        {
            my %void = (type => 'fm_int');
            $void{value} = $input;
            $chip->fmSetSwitchAttribute($switchNum,
                                        $sw_attr_map{$attribute},
                                        \%void);
        }
        elsif ($attribute eq "cpu_port")
        {
            my %void = (type => 'fm_int');
            $void{value} = $input;
            $chip->fmSetSwitchAttribute($switchNum,
                                        $sw_attr_map{$attribute},
                                        \%void);
        }
        elsif ($attribute eq "vlan1_eth_a" ||
               $attribute eq "vlan1_eth_b" ||
               $attribute eq "vlan2_eth_a" ||
               $attribute eq "vlan2_eth_b")
        {
            $input = hex($input) if ($input =~ /\D/);
            my %void = (type => "fm_uint32", value => $input);
            $chip->fmSetSwitchAttribute($switchNum,
                                        $sw_attr_map{$attribute},
                                        \%void);
        }
        elsif ($attribute eq "rbridge_nick" ||
               $attribute eq "rbridge_eth" )
        {
            $input = hex($input) if ($input =~ /\D/);
            my %void = (type => "fm_uint16", value => $input);
            $chip->fmSetSwitchAttribute($switchNum,
                                        $sw_attr_map{$attribute},
                                        \%void);
        }
        elsif ($attribute eq "di_l2_start_index" ||
               $attribute eq "di_l4_start_index")
        {
            my %void = (type => 'fm_uint32');
            $void{value} = $input;
            $chip->fmSetSwitchAttribute($switchNum,
                                        $sw_attr_map{$attribute},
                                        \%void);
        }
        elsif ($attribute eq "qos_shared_pause_on_wm"
               || $attribute eq "qos_shared_pause_off_wm")
        {   
            my $family = $self->getSwitchFamily($switchNum);
            my $index  = $self->str2intnum($input);
            my %void = (type  => 'fm_uint32',
                        value => $self->str2intnum($otherInput));

            if ($family == $FM_SWITCH_FAMILY_FM6000 ||
                $family == $FM_SWITCH_FAMILY_REMOTE_FM6000)
            {  
                if ( !defined($index) || $index < 0 || $index > 11 )
                {
                    print("Invalid index value. The range must be between ");
                    print("0-11.\n");
                    return;
                }

                if ( !defined($void{value}) || $void{value} < 0 || 
                     $void{value} > 10485600 )
                {
                    print("Invalid watermark value. The range must be ");
                    print("between 0-10485600.\n");
                    return;
                }
             }
             elsif ($family == $FM_SWITCH_FAMILY_FM4000 ||
                    $family == $FM_SWITCH_FAMILY_REMOTE_FM4000)
             {
                if ( !defined($index) || $index < 0 || $index > 1 )
                {
                    print("Invalid index value. The range must be between ");
                    print("0-1.\n");
                    return;
                }
                
                if ( !defined($void{value}) || $void{value} < 0 || 
                     $void{value} > 4193792 )
                {
                    print("Invalid watermark value. The range must be ");
                    print("between 0-4193792.\n");
                    return;
                }
             }
          
             $chip->fmSetSwitchQOS($switchNum,
                                   $sw_attr_map{$attribute},
                                   $index,
                                   \%void);
        }
        elsif ($attribute eq "pol_bank_drop_mask")
        {
            my $polBank = SDK::fm_policerBankDropMask->new();
            my %void = (type => 'fm_policerBankDropMask', value => $polBank);

            if ($input < 0 || $input > 1)
            {
                print("Must specify a valid bank number (0 or 1)!\n");
                return;
            }

            my ($minPhysPort, $maxPhysPort) = $self->tpPlatformGetPortRange();
            $polBank->{'bankIndex'} = $input;  
            my $bitmask = $polBank->{'dropMask'};
            $chip->fmCreateBitArray($bitmask, $maxPhysPort+1);
            if(!(lc($otherInput) eq "none"))
            {
                my @portList3 =
                    $self->validateList($otherInput, 
                                        $self->tpPlatformGetPortRange());
                foreach my $port3 (@portList3)
                {

                    my ($sw3, $logPort3) =
                        $self->tpPlatformMapGlobalToLogicalPort($port3);

                    my $status1 = $chip->fmSetBitArrayBit($bitmask, $logPort3, 1);
                }
            }
            $chip->fmSetSwitchAttribute($switchNum,
                                        $sw_attr_map{$attribute},
                                        \%void);


            $chip->fmDeleteBitArray($bitmask);
        }

     elsif ($attribute eq "drop_mtu_violation") 
        {
            my %void = (type => 'fm_bool');
            if ($input eq "on")
            {
                $void{value} = $FM_ENABLED;
            }
            else
            {
                $void{value} = $FM_DISABLED;
            }
            $chip->fmSetSwitchAttribute($switchNum, $sw_attr_map{$attribute},
                                        \%void);
        }

    }

}   # end handleSetSwitchConfig

##@cmethod public void handleSetSwitchConfigTrapping(char *attribute, void *input)
#
# @desc         Handles setting switch trapping
#
# @param[in]    attribute The attribute to be set
#
# @param[in]    input The value the attribute is to be set to
sub handleSetSwitchConfigTrapping
{
    my ($self, $attribute, $input, $otherInput) = @_;

    if ($attributesInitialized == $FALSE)
    {
        $self->tpInitAttributes();
    }

    my $chip = $self->{CHIP};

    if (!defined($attribute) || !exists($sw_trap_attr_map{$attribute}))
    {
        print("Must specify a valid switch trapping attribute!\n");
        return;
    }

    if (!defined($input))
    {
        print("Must specify an attribute value!\n");
        return;
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        if ($attribute eq "trap_security"
            || $attribute eq "trap_bpdu"
            || $attribute eq "trap_lacp"
            || $attribute eq "trap_garp"
            || $attribute eq "trap_mtu_viol"
            || $attribute eq "trap_plus_log"
            || $attribute eq "trap_other")
        {
            my %void = (type => 'fm_bool');
            if ($input eq "on")
            {
                $void{value} = $FM_ENABLED;
            }
            else
            {
                $void{value} = $FM_DISABLED;
            }
            $chip->fmSetSwitchAttribute($switchNum, $sw_trap_attr_map{$attribute},
                                        \%void);
        }
        elsif ($attribute eq "trap_ether")
        {
            my %void = (type => 'fm_int');
            if ($input eq "off")
            {
                $void{value} = -1;
            }
            else
            {
                $void{value} = $input;
                $void{value} =~ tr/0-9a-fA-F//cd;
                if (length($void{value}) != 4)
                {
                    print("expecting a 2-byte hex number\n");
                    return;
                }
                $void{value} = "0x" . $void{value};
                $void{value} = $self->str2intnum($void{value});
                if (!$void{value})
                {
                    print("invalid input value\n");
                    return;
                }
            }
            $chip->fmSetSwitchAttribute($switchNum, $sw_trap_attr_map{$attribute},
                                        \%void);
        }
        elsif ($attribute eq "trap_cpu_mac")
        {
            my %void = (type => 'fm_int');

            if ($input eq "ibv_check")
            {
                $void{value} = $FM_CPU_MAC_TRAP_DISP_IBV_CHECK;
            }
            elsif ($input eq "ibv_and_ebv_check")
            {
                $void{value} = $FM_CPU_MAC_TRAP_DISP_IBV_AND_EBV_CHECK;
            }
            elsif ($input eq "always")
            {
                $void{value} = $FM_CPU_MAC_TRAP_DISP_ALWAYS;
            }
            elsif ($input eq "none")
            {
                $void{value} = $FM_CPU_MAC_TRAP_DISP_NONE;
            }
            else
            {
                print "'$input' is not valid for $attribute!\n";
                return;
            }

            $chip->fmSetSwitchAttribute($switchNum,
                                        $sw_trap_attr_map{$attribute},
                                        \%void);
        }
    }

}   # end handleSetSwitchConfigTrapping


##@cmethod public void handleShowSwitchConfig(void)
#
# @desc         Handles showing the switch configuration
sub handleShowSwitchConfig
{
    my ($self) = @_;

    if ($attributesInitialized == $FALSE)
    {
        $self->tpInitAttributes();
    }

    my $chip = $self->{CHIP};

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    $chip->disableErrors();
    for my $switchNum ($self->tpGetSwitches)
    {

        print("\n");

        if ($switchCount > 1)
        {
            printf("Switch %d:\n", $switchNum);
            print("--------------------------------------\n");
        }

        my ($status, $value);

        foreach my $attribute (sort(keys(%sw_attr_map)))
        {
            if ($attribute eq "qos_shared_pause_on_wm"
                || $attribute eq "qos_shared_pause_off_wm")
            {
               my %void = (type => "fm_int", value => 0);
               my $index;
               my $indexValue;

               for ($index = 0; $index < 12; $index++)
               {                     
                  printf("%-28s", $attribute . "[" . $index . "]");
                       
                  $status = $chip->fmGetSwitchQOS($switchNum, 
                                                  $sw_attr_map{$attribute},
                                                  $index, 
                                                  \%void);

                  $indexValue = int($void{value} / 1024);

                  $value = sprintf("%-6s",$indexValue . "k");

                  printf("%-10s", $status != $FM_OK ? "NA" : $value);
                  print("\n");
               }
            }
            else
            { 
               printf("%-28s", $attribute);
            }

            if ($attribute eq "vlan_mode")
            {
                my %void = (type => 'fm_int', value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_attr_map{$attribute},
                                            \%void);
                if ($void{value} == $FM_VLAN_MODE_8021Q)
                {
                    $value = "802.1q";
                }
                else
                {
                    $value = "port_based";
                }
            }
            elsif ($attribute eq "mtu_list")
            {
                my $mtuEntry = SDK::fm_mtuEntry->new();
                my %void = (type => 'fm_mtuEntry', value => $mtuEntry);

                $value = "[ ";

                my $nbMtuListEntries;
                my $switchInfo = new SDK::fm_switchInfo();
                $chip->fmGetSwitchInfo($switchNum, $switchInfo);
                my $family = $switchInfo->{'switchFamily'};
                if ($family == $FM_SWITCH_FAMILY_FM6000 ||
                    $family == $FM_SWITCH_FAMILY_REMOTE_FM6000) 
                {
                    #value of FM6000_MTU_TABLE_ENTRIES(sw) - 1
                    $nbMtuListEntries = 15; 
                }
                elsif ($family == $FM_SWITCH_FAMILY_FM4000 ||
                       $family == $FM_SWITCH_FAMILY_REMOTE_FM4000)
                {
                    #value of $FM4000_MTU_TABLE_ENTRIES - 1; 
                    $nbMtuListEntries = 7; 
                }
                else
                {
                    printf("Invalid family, cannot retrieve mtu_list entries\n");
                    $nbMtuListEntries = 0;
                }

                foreach (0..$nbMtuListEntries)
                {
                    $mtuEntry->{'index'} = $_;    

                    $status = $chip->fmGetSwitchAttribute($switchNum,
                                                          $sw_attr_map{$attribute},
                                                          \%void);

                    if ($status == $FM_OK)
                    {
                        $value .= sprintf("%d: %d ", $_, $mtuEntry->{'mtu'});
                    }
                    elsif ($status == $FM_ERR_INVALID_ATTRIB && $_ == 0)
                    {
                        last;
                    }
                    else
                    {
                        printf("\nERROR: index $_: %s\n", $chip->fmErrorMsg($status));
                    }
                }

                $value .= "]";
            }

            elsif ($attribute eq "pol_bank_drop_mask")
            {
                my $polBank = SDK::fm_policerBankDropMask->new();
                my %void = (type => 'fm_policerBankDropMask', value => $polBank);

                my $status;
                my ($minPhysPort, $maxPhysPort) = $self->tpPlatformGetPortRange();

                my $bitmask = $polBank->{'dropMask'};
                $chip->fmCreateBitArray($bitmask, $maxPhysPort+1);
                
                for (my $mbank = 0; $mbank < 2; $mbank++)
                {
                    $polBank->{'bankIndex'} = $mbank; 

                    $status = $chip->fmGetSwitchAttribute($switchNum,
                                                          $sw_attr_map{$attribute},
                                                          \%void);
                    my @maskPorts = ();

                    for (my $mport = 0; $mport < $maxPhysPort+1; $mport++)
                    {
                        my $isSet = 0;

                        $chip->fmGetBitArrayBit($bitmask, $mport, \$isSet);
                        if ($isSet)
                        {
                            push(@maskPorts, $mport);
                        }
                    }

                    my $list = $self->StringifyList(@maskPorts);
                    if ($list eq "")
                    {
                       $list = "none" ;
                    }

                    if ($mbank == 0)
                    {
                        $value = sprintf("bank_0:(%s)", $list);
                    }
                    else
                    {
                        $value .= sprintf(", bank_1:(%s)", $list);
                    }
                }
            }
            elsif ($attribute eq "drop_mtu_violation")
            {
                my %void = (type => 'fm_bool', value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_attr_map{$attribute},
                                                      \%void);
                if ($void{value} == $FM_ENABLED)
                {
                    $value = "on";
                }
                else
                {
                    $value = "off";
                }
            }
            elsif ($attribute eq "soft_learning")
            {
                my %void = (type => 'fm_bool', value => 0);
                $status = $chip->fmGetAddressTableAttribute($switchNum,
                                                            $sw_attr_map{$attribute},
                                                            \%void);
                if ($void{value} == $FM_ENABLED)
                {
                    $value = "on";
                }
                else
                {
                    $value = "off";
                }
            }
            elsif ($attribute eq "spanning-tree")
            {
                my %void = (type => 'fm_int', value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_attr_map{$attribute},
                                                      \%void);
                if ($void{value} == $FM_SPANNING_TREE_SHARED)
                {
                    $value = "shared";
                }
                elsif ($void{value} == $FM_SPANNING_TREE_PER_VLAN)
                {
                    $value = "per-vlan";
                }
                elsif ($void{value} == $FM_SPANNING_TREE_MULTIPLE)
                {
                    $value = "multiple";
                }
            }
            elsif ($attribute eq "vlan-learning")
            {
                my %void = (type => 'fm_int', value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_attr_map{$attribute},
                                                      \%void);
                if ($void{value} == $FM_VLAN_LEARNING_MODE_SHARED)
                {
                    $value = "shared";
                }
                elsif ($void{value} == $FM_VLAN_LEARNING_MODE_INDEPENDENT)
                {
                    $value = "independent";
                }
            }
            elsif ($attribute eq "shared-fid")
            {
                my %void = (type => 'fm_int', value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_attr_map{$attribute},
                                                      \%void);
                $value = $void{value};
            }
            elsif ($attribute eq "ip_options_disp")
            {
                my %void = (type => 'fm_int', value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_attr_map{$attribute},
                                                      \%void);

                $value = "";

                if ($void{value} & $FM_IP_OPTIONS_TRAP)
                {
                    $value .= "trap_all,";
                }
                if ($void{value} & $FM_IP_OPTIONS_FWD)
                {
                    $value .= "forward,";
                }
                if ($void{value} & $FM_IP_OPTIONS_TRAP_UCST)
                {
                    $value .= "trap_unicast,";
                }
                if ($void{value} & $FM_IP_OPTIONS_LOG_UCST)
                {
                    $value .= "log_unicast,";
                }
                if ($void{value} & $FM_IP_OPTIONS_TRAP_MCST)
                {
                    $value .= "trap_multicast,";
                }
                $value = substr($value, 0, length($value) - 1);
            }
            elsif ( ($attribute eq "mstp") ||
                    ($attribute eq "learn_drop_frame")  ||
                    ($attribute eq "mirror_counters") )
            {
                my %void = (type => 'fm_bool', value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_attr_map{$attribute},
                                            \%void);
                if ($void{value} == $FM_ENABLED)
                {
                    $value = "on";
                }
                else
                {
                    $value = "off";
                }
            }
            elsif ( ($attribute eq "cpu_mac") ||
                    ($attribute eq "rbridge_mac") )
            {
                my %void = (type => 'fm_macaddr', value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_attr_map{$attribute},
                                            \%void);
                $value = $void{value};
            }
            elsif ($attribute eq "flood_bcast")
            {
                my %void = (type => 'fm_int', value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_attr_map{$attribute},
                                            \%void);
                if ($void{value} == $FM_BCAST_DISCARD)
                {
                    $value = "discard";
                }
                elsif ($void{value} == $FM_BCAST_FWD)
                {
                    $value = "fwd";
                }
                elsif ($void{value} == $FM_BCAST_FWD_EXCPU)
                {
                    $value = "fwd_exc_cpu";
                }
                elsif ($void{value} == $FM_BCAST_FLOODING_PER_PORT)
                {
                    $value = "per-port";
                }
            }
            elsif ($attribute eq "flood_mcast")
            {
                my %void = (type => 'fm_int', value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_attr_map{$attribute},
                                            \%void);
                if ($void{value} == $FM_MCAST_DISCARD)
                {
                    $value = "discard";
                }
                elsif ($void{value} == $FM_MCAST_FWD)
                {
                    $value = "fwd";
                }
                elsif ($void{value} == $FM_MCAST_FLOODING_PER_PORT)
                {
                    $value = "per-port";
                }
            }
            elsif ($attribute eq "flood_ucast")
            {
                my %void = (type => 'fm_int', value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_attr_map{$attribute},
                                            \%void);
                if ($void{value} == $FM_UCAST_DISCARD)
                {
                    $value = "discard";
                }
                elsif ($void{value} == $FM_UCAST_FWD)
                {
                    $value = "fwd";
                }
                elsif ($void{value} == $FM_UCAST_FLOODING_PER_PORT)
                {
                    $value = "per-port";
                }
            }
            elsif ($attribute eq "frame_age_time")
            {
                my %void = (type => 'fm_int', value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_attr_map{$attribute},
                                            \%void);
                if ($void{value} == 0)
                {
                    $value = "off";
                }
                else
                {
                    $value = Math::BigInt->new($void{value})->bstr();
                }
            }
            elsif ($attribute eq "lacp_disp")
            {
                my %void = (type => 'fm_int', value => 0);
                $status = $chip->fmGetLAGAttributeExt($switchNum,
                                                      $sw_attr_map{$attribute},0,
                                            \%void);
                                            
                if ($void{value} == $FM_PDU_DISCARD)
                {
                    $value = "discard";
                }
                elsif ($void{value} == $FM_PDU_FORWARD)
                {
                    $value = "fwd";
                }
                elsif ($void{value} == $FM_PDU_TOCPU)
                {
                    $value = "tocpu";
                }
                else
                {
                    $value = "undefined";
                }
            }
            elsif ($attribute eq "lbg_mode")
            {
                my %void = (type => 'fm_int', value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_attr_map{$attribute},
                                                      \%void);

                if ($void{value} == $FM_LBG_MODE_MAPPED)
                {
                    $value = "mapped";
                }
                elsif ($void{value} == $FM_LBG_MODE_NPLUS1)
                {
                    $value = "nplus1";
                }
                elsif ($void{value} == $FM_LBG_MODE_REDIRECT)
                {
                    $value = "redirect";
                }
                else
                {
                    $value = sprintf("unknown(%d)", $void{value});
                }
            }
            elsif ($attribute eq "mac_age_time")
            {
                my %void = (type => 'fm_uint32', value => 0);
                $status = $chip->fmGetAddressTableAttribute($switchNum,
                                                            $sw_attr_map{$attribute},
                                                            \%void);
                if ($void{value} == 0)
                {
                    $value = "off";
                }
                else
                {
                    $value = Math::BigInt->new($void{value})->bstr();
                }
            }
            elsif ($attribute eq "stats_config")
            {
                my %void = (type => 'fm_uint32', value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_attr_map{$attribute},
                                                      \%void);

                my $bits = 
                {
                    1 => $FM_STAT_GROUP_PER_PORT_RX_FRAME_CLASS,
                    2 => $FM_STAT_GROUP_PER_PORT_RX_COUNT_BY_LEN,
                    3 => $FM_STAT_GROUP_PER_PORT_RX_OCTETS,
                    4 => $FM_STAT_GROUP_PER_PORT_RX_COUNT_BY_PRIORITY,
                    5 => $FM_STAT_GROUP_PER_PORT_RX_OCTETS_BY_PRIORITY,
                    6 => $FM_STAT_GROUP_PER_PORT_FWD_ACTION,
                    7 => $FM_STAT_GROUP_PER_PORT_TX_FRAME_CLASS,
                    8 => $FM_STAT_GROUP_PER_PORT_TX_COUNT_BY_LEN,
                    9 => $FM_STAT_GROUP_PER_PORT_TX_OCTETS,
                    11 => $FM_STAT_GROUP_VLAN_RX_FRAME_CLASS,
                    12 => $FM_STAT_GROUP_VLAN_RX_OCTETS,
                    14 => $FM_STAT_GROUP_EGRESS_ACL,
                };

                my $enabledGroups = 0;

                foreach my $key (keys(%$bits))
                {
                    if ($void{value} & $bits->{$key})
                    {
                        $enabledGroups |= (1 << $key);
                    }
                }

                $value = $self->stringifyPortMask($switchNum, $enabledGroups);
            }
            elsif ($attribute eq "ifg_penalty")
            {
                my %void = (type => 'fm_int', value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_attr_map{$attribute},
                                            \%void);
                $value = Math::BigInt->new($void{value})->bstr();
            }
            elsif ($attribute eq "cpu_port")
            {
                my %void = (type => 'fm_int', value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_attr_map{$attribute},
                                            \%void);
                $value = $void{value};
            }
            elsif ($attribute eq "vlan1_eth_a" ||
                   $attribute eq "vlan1_eth_b" ||
                   $attribute eq "vlan2_eth_a" ||
                   $attribute eq "vlan2_eth_b")
            {
                my %void = (type => "fm_uint32", value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_attr_map{$attribute},
                                                      \%void);
                
                $value = sprintf("%#.4x",$void{value})
            }
            elsif ($attribute eq "rbridge_nick" ||
                   $attribute eq "rbridge_eth" )
            {
                my %void = (type => "fm_uint16", value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_attr_map{$attribute},
                                                      \%void);
                $value = sprintf("%#.4x",$void{value})
            }
            elsif ($attribute eq "di_l2_start_index" ||
                   $attribute eq "di_l4_start_index")
            {
                my %void = (type => "fm_uint32", value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_attr_map{$attribute},
                                                      \%void);
                $value = $void{value};
            }
            elsif ($attribute eq "igmp_snooping" )
            {
                $status = $chip->fmGetPacketState($switchNum, 
                                                  $FM_PACKET_TYPE_IGMP, 
                                                  \$value);
                $value = ($value == 0) ? "off" : "on";
            }
            elsif ($attribute eq "igmp_snooping_type" )
            {
                my $type;
                $status = $chip->fmGetIgmpType($switchNum, \$type);
                foreach my $igmpType (keys %igmp_snoop_map)
                {
                    if ($igmp_snoop_map{$igmpType} == $type)
                    {
                        $value = $igmpType;
                    }
                }
            }

            if ($attribute ne "qos_shared_pause_on_wm"
                && $attribute ne "qos_shared_pause_off_wm")
            {
               printf("%-10s", $status != $FM_OK ? "NA" : $value);
               print("\n");
            }   
        }
    }
    print("\n");

    $chip->enableErrors();

}   # end handleShowSwitchConfig

##@cmethod public void handleShowSwitchConfigTrapping(void)
#
# @desc         Handles showing the switch trapping configuration
sub handleShowSwitchConfigTrapping
{
    my ($self) = @_;

    if ($attributesInitialized == $FALSE)
    {
        $self->tpInitAttributes();
    }

    my $chip = $self->{CHIP};

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    $chip->disableErrors();
    for my $switchNum ($self->tpGetSwitches)
    {
        print("\n");

        if ($switchCount > 1)
        {
            printf("Switch %d:\n", $switchNum);
            print("--------------------------------------\n");
        }

        my ($status, $value);

        foreach my $attribute (sort(keys(%sw_trap_attr_map)))
        {
            printf("%-20s", $attribute);

            if ($attribute eq "trap_security"
                || $attribute eq "trap_bpdu"
                || $attribute eq "trap_lacp"
                || $attribute eq "trap_garp"
                || $attribute eq "trap_other"
                || $attribute eq "trap_mtu_viol"
                || $attribute eq "trap_plus_log"
                || $attribute eq "trap_other")
            {
                my %void = (type => 'fm_bool', value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_trap_attr_map{$attribute},
                                            \%void);
                if ($void{value} == $FM_ENABLED)
                {
                    $value = "on";
                }
                else
                {
                    $value = "off";
                }
            }
            elsif ($attribute eq "trap_ether")
            {
                my %void = (type => 'fm_int', value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_trap_attr_map{$attribute},
                                            \%void);
                if ($void{value} == -1)
                {
                    $value = "false";
                }
                else
                {
                    $value = Math::BigInt->new($void{value})->as_hex();
                }
            }
            elsif ($attribute eq "trap_cpu_mac")
            {
                my %void = (type => 'fm_int', value => 0);
                $status = $chip->fmGetSwitchAttribute($switchNum,
                                                      $sw_trap_attr_map{$attribute},
                                                      \%void);

                if ($void{value} == $FM_CPU_MAC_TRAP_DISP_IBV_CHECK)
                {
                    $value = "ibv_check";
                }
                elsif ($void{value} == $FM_CPU_MAC_TRAP_DISP_IBV_AND_EBV_CHECK)
                {
                    $value = "ibv_and_ebv_check";
                }
                elsif ($void{value} == $FM_CPU_MAC_TRAP_DISP_ALWAYS)
                {
                    $value = "always";
                }
                elsif ($void{value} == $FM_CPU_MAC_TRAP_DISP_NONE)
                {
                    $value = "none";
                }
            }
            printf("%-10s", $status != $FM_OK ? "NA" : $value);
            print("\n");
        }
    }
    print("\n");

    $chip->enableErrors();

}   # end handleShowSwitchConfigTrapping


##@cmethod public void handleSetSwitchSelect(char *switch)
#
# @desc         Handles setting the set of switches selected for subsequent
#               commands.
#
# @param[in]    switch is the switch(es) to select for subsequent
#               configuration.
sub handleSetSwitchSelect
{
    my ($self, $switch) = @_;
    
    if ( !($self->validateAndSetSwitchSelect($switch)) )
    {
        print($TP_MSG_ERR_SWITCH_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    
    return $FM_OK;

}

##@cmethod public int validateAndSetSwitchSelect(char *switch)
#
# @desc         Handles setting the set of switches selected for subsequent
#               commands.
#
# @param[in]    switch is the switch(es) to select for subsequent
#               configuration.
sub validateAndSetSwitchSelect
{
    my ($self, $switch) = @_;

    my @switchList = $self->validateExplicitList($FALSE, $switch, 
                                                 @{[$self->tpGetPhysicalSwitches]});
    if (scalar(@switchList) == 0)
    {
        return 0;
    }

    $self->tpSetSelectedSwitches(@switchList);
    return 1;
}

##@cmethod public void handleSetMacConfig(char *attribute, void *input)
#
# @desc         Handles setting MA Table attributes
#
# @param[in]    attribute The attribute to be set
#
# @param[in]    input The value the attribute is to be set to
sub handleSetMacConfig
{
    my ($self, $attribute, $input) = @_;

    my $chip = $self->{CHIP};

    if (!defined($attribute) || !exists($mac_attr_map{$attribute}))
    {
        print("Must specify a valid MA Table attribute!\n");
        return;
    }

    if (!defined($input))
        {
        print("Must specify an attribute value!\n");
            return;
        }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        if ($attribute eq "soft_learning")
        {
            my %void = (type => 'fm_bool');
            if ($input eq "on")
            {
                $void{value} = $FM_ENABLED;
            }
            else
            {
                $void{value} = $FM_DISABLED;
            }

            $chip->fmSetAddressTableAttribute($switchNum, $mac_attr_map{$attribute},
                                              \%void);
        }
        elsif ($attribute eq "mac_age_time")
        {
            my %void = (type => "fm_uint32", value => $self->str2intnum($input));
            if (!defined($void{value}))
            {
                print("invalid input value\n");
                return;
            }
            if ($void{value} == 0)
            {
                print("Disabling MA Table aging.\n");
            }
            
            $chip->fmSetAddressTableAttribute($switchNum, 
                                              $mac_attr_map{$attribute},
                                              \%void);
        }
        elsif ($attribute eq "update_report")
        {
            my %void = (type => 'fm_uint32');
            if ($input eq "guaranteed")
            {
                $void{value} = $FM_MAC_TABLE_EVENT_MODE_GUARANTEED;
            }
            else
            {
                $void{value} = $FM_MAC_TABLE_EVENT_MODE_BEST_EFFORT;
            }

            $chip->fmSetAddressTableAttribute($switchNum, $mac_attr_map{$attribute},
                                              \%void);
        }
        elsif ( $attribute eq "learn_threshold" 
                || $attribute eq "age_threshold"
                || $attribute eq "sec_threshold")
        {
            my %void = (type => "fm_uint32", value => $self->str2intnum($input));
            if (!defined($void{value}))
            {
                print("invalid input value\n");
                return;
            }
            if ($void{value} > 511)
            {
                printf("Invalid input value (must be 0 - 511)\n");
                return;
            }
            
            $chip->fmSetAddressTableAttribute($switchNum, 
                                              $mac_attr_map{$attribute},
                                              \%void);
        }
        elsif ($attribute eq "sweepPeriod")
        {
            my %void = (type => "fm_uint32", value => $self->str2intnum($input));
            if (!defined($void{value}))
            {
                print("invalid input value\n");
                return;
            }
            if ($void{value} == 0)
            {
                print("Disabling MAC Table sweeping\n");
            }
            
            $chip->fmSetAddressTableAttribute($switchNum, 
                                              $mac_attr_map{$attribute},
                                              \%void);
        }
    }
}

##@cmethod public void handleShowMacConfig(void)
#
# @desc         Handles showing the MA Table configuration
sub handleShowMacConfig
{
    my ($self) = @_;

    my $chip = $self->{CHIP};

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    $chip->disableErrors();
    for my $switchNum ($self->tpGetSwitches)
    {

        print("\n");

        if ($switchCount > 1)
        {
            printf("Switch %d:\n", $switchNum);
            print("--------------------------------------\n");
        }

        foreach my $attribute (sort(keys(%mac_attr_map)))
        {
            printf("%-20s", $attribute);

            my ($status, $value);
            if ($attribute eq "soft_learning")
            {
                my %void = (type => 'fm_bool', value => 0);
                $status = $chip->fmGetAddressTableAttribute($switchNum,
                                                            $mac_attr_map{$attribute},
                                                            \%void);
                if ($void{value} == $FM_ENABLED)
                {
                    $value = "on";
                }
                else
                {
                    $value = "off";
                }
            }
            elsif ($attribute eq "mac_age_time"
                   || $attribute eq "sweepPeriod")
            {
                my %void = (type => 'fm_uint32', value => 0);
                $status = $chip->fmGetAddressTableAttribute($switchNum,
                                                            $mac_attr_map{$attribute},
                                                            \%void);
                if ($void{value} == 0)
                {
                    $value = "off";
                }
                else
                {
                    $value = Math::BigInt->new($void{value})->bstr();
                }
            }
            elsif ($attribute eq "update_report")
            {
                my %void = (type => 'fm_uint32', value => 0);
                $status = $chip->fmGetAddressTableAttribute($switchNum,
                                                            $mac_attr_map{$attribute},
                                                            \%void);
                if ($void{value} == $FM_MAC_TABLE_EVENT_MODE_GUARANTEED)
                {
                    $value = "guaranteed";
                }
                else
                {
                    $value = "best_effort";
                }
            }
            elsif ( $attribute eq "learn_threshold" 
                    || $attribute eq "age_threshold"
                    || $attribute eq "sec_threshold" )
            {
                my %void = (type => 'fm_uint32', value => 0);
                $status = $chip->fmGetAddressTableAttribute($switchNum,
                                                            $mac_attr_map{$attribute},
                                                            \%void);
                $value = Math::BigInt->new($void{value})->bstr();
            }
            
            printf("%-10s", $status != $FM_OK ? "NA" : $value);
            print("\n");
        }
    }
    print("\n");

    $chip->enableErrors();
}

##@cmethod public int tpHandleShowSwitchInfo
#
# @desc         Handles showing switch information
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub tpHandleShowSwitchInfo
{
    my ($self) = @_;

    my $chip = $self->{CHIP};

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my ($family, $model, $revision);


        my $infoW = ($self->tpGetSwitchInfo())[$switchNum];

        print("\n");
        printf("Switch %d: ", $switchNum) if ($switchCount > 1);
        print("Switch Information:\n");
        print("-------------------------------------------------------------\n");

        FAMILY: for ($infoW->{'switchFamily'})
        {
            $_ == $FM_SWITCH_FAMILY_FM2000 && do
            {
                $family = "FM2000";
                MODEL: for ($infoW->{'switchModel'})
                {
                    $_ == $FM_SWITCH_MODEL_FM2112 && do
                    {
                        $model = "FM2112";
                        $revision = "unknown";
                        last MODEL;
                    };

                    $_ == $FM_SWITCH_MODEL_FM2224 && do
                    {
                        $model = "FM2224";
                        REVISION: for ($infoW->{'switchVersion'})
                        {
                            $_ == $FM_SWITCH_VERSION_FM2224_A0_A4 && do
                            {
                                $revision = "A0 -- A4";
                                last REVISION;
                            };

                            $_ == $FM_SWITCH_VERSION_FM2224_A5 && do
                            {
                                $revision = "A5";
                                last REVISION;
                            };

                            $revision = "unknown";
                        }
                        last MODEL;
                    };

                    $model = $revision = "unknown";
                }
                last FAMILY;
            };

            $_ == $FM_SWITCH_FAMILY_FM4000 && do
            {
                $family = "FM4000";

                # FIXME: why do I need SDK:: here?

                $chip->disableErrors();

                my $mgmtClk =
                    $chip->fmGetIntApiAttribute($SDK::FM_AAK_API_SYSTEM_EBI_CLOCK,
                                                33);

                my $fhClk =
                    $chip->fmGetIntApiAttribute($SDK::FM_AAK_API_FM4000_BOOT_FHFREQ,
                                                $SDK::FM_AAD_API_FM4000_BOOT_FHFREQ);

                $chip->enableErrors();

                my $pllCtrl = -1;
                $chip->fmReadUINT32($switchNum, $chip->FM_PLL_FH_CTRL(), 
                                    \$pllCtrl);

                printf("%-20s %s (P=%d M=%d N=%d)\n", 
                       "Frame Handler Clk", $fhClk,
                       ($pllCtrl >> 2) & 0x3,
                       ($pllCtrl >> 4) & 0x7f,
                       ($pllCtrl >> 11) & 0xf);

                printf("%-20s %s\n", "Mgmt Clk", $mgmtClk);

                MODEL: for ($infoW->{'switchModel'})
                {
                    $_ == $FM_SWITCH_MODEL_FM4224 && do
                    {
                        $model = "FM4224";
                        REVISION: for ($infoW->{'switchVersion'})
                        {
                            $_ == $FM_SWITCH_VERSION_FM4224_A1 && do
                            {
                                $revision = "A1";
                                last REVISION;
                            };

                            $_ == $FM_SWITCH_VERSION_FM4224_A1_5 && do
                            {
                                $revision = "A1.5";
                                last REVISION;
                            };

                            $_ == $FM_SWITCH_VERSION_FM4224_A1_6 && do
                            {
                                $revision = "A1.6";
                                last REVISION;
                            };

                            $_ == $FM_SWITCH_VERSION_FM4224_A2 && do
                            {
                                $revision = "A2";
                                last REVISION;
                            };

                            $_ == $FM_SWITCH_VERSION_FM4224_A3 && do
                            {
                                $revision = "A3";
                                last REVISION;
                            };

                            $revision = "unknown";
                        }
                        last MODEL;
                    };

                    $model = $revision = "unknown";
                }
                last FAMILY;
            };

            $_ == $FM_SWITCH_FAMILY_FM6000 && do
            {
                $family = "FM6000";

                MODEL: for ($infoW->{'switchModel'})
                {
                    $_ == $FM_SWITCH_MODEL_FM6224 && do
                    {
                        $model = "FM6224";
                        REVISION: for ($infoW->{'switchVersion'})
                        {
                            $_ == $FM_SWITCH_VERSION_FM6224_A0 && do
                            {
                                $revision = "A0";
                                last REVISION;
                            };

                            $revision = "unknown";
                        }
                        last MODEL;
                    };

                    $_ == $FM_SWITCH_MODEL_FM6364 && do
                    {
                        $model = "FM6364";
                        REVISION: for ($infoW->{'switchVersion'})
                        {
                            $_ == $FM_SWITCH_VERSION_FM6364_A0 && do
                            {
                                $revision = "A0";
                                last REVISION;
                            };

                            $_ == $FM_SWITCH_VERSION_FM6364_B0 && do
                            {
                                $revision = "B0";
                                last REVISION;
                            };

                            $_ == $FM_SWITCH_VERSION_FM6364_B1 && do
                            {
                                $revision = "B1";
                                last REVISION;
                            };

                            $revision = "unknown";
                        }
                        last MODEL;
                    };

                    $model = $revision = "unknown";
                }
                last FAMILY;
            };

            $_ == $FM_SWITCH_FAMILY_SWAG && do
            {
                $family   = "SWAG";
                $model    = "N/A";
                $revision = "N/A";
                
                my $topology;

                $chip->disableErrors();

                my $top;
                my $status = $chip->fmGetSWAGTopology($switchNum, \$top); 
                    
                if ($status == $FM_OK)
                {
                    TOPOLOGY: for ($top)
                    {
                        $_ == $FM_SWAG_TOPOLOGY_FAT_TREE && do
                        {
                            $topology = "Fat Tree";
                            last TOPOLOGY;
                        };

                        $_ == $FM_SWAG_TOPOLOGY_MESH && do
                        {
                            $topology = "Mesh";
                            last TOPOLOGY;
                        };

                        $_ == $FM_SWAG_TOPOLOGY_RING && do
                        {
                            $topology = "Ring";
                            last TOPOLOGY;
                        };

                        $_ == $FM_SWAG_TOPOLOGY_UNDEFINED && do
                        {
                            $topology = "Undefined";
                            last TOPOLOGY;
                        };

                        $topology = "unknown";
                    }
                    
                    printf("%-20s %s\n", "Topology", $topology);
                    printf("%-20s ", "Member switches");
                    
                    my $switchMember = -1;
                    my $searchToken = new SDK::fm_voidptrW();
                    
                    $status = $chip->fmGetSWAGSwitchFirst($switchNum, 
                                                          $searchToken, 
                                                          \$switchMember);
                    while ($status == $FM_OK)
                    {
                        printf("%d ", $switchMember);
                        
                        $status = $chip->fmGetSWAGSwitchNext($switchNum,
                                                             $searchToken,
                                                             \$switchMember);
                    }
                    
                    printf("\n");
                    $chip->enableErrors();
                    
                }   # end if ($status == $FM_OK)

                last FAMILY;
            };
            
            $family = $model = $revision = "unknown";
        }

        printf("%-20s %s\n", "Switch Family", $family);
        printf("%-20s %s\n", "Switch Model", $model);
        printf("%-20s %s\n", "Switch Revision", $revision);

    }

    printf("\n");
}

##@cmethod public void handleShowSelectedSwitches(void)
#
# @desc         Handles showing the set of switches selected for
#               configuration.
sub handleShowSelectedSwitches
{
    my ($self) = @_;
    my $switchList = $self->StringifyList($self->tpGetSwitches);
    my $moreThanOneSwitch = $switchList =~ /\D/;

    printf("Switch%s selected for configuration: $switchList\n", 
           $moreThanOneSwitch ? "es" : "");
}

##@cmethod public void handleShowSwitchStats(void)
#
# @desc         Handles showing the global switch statistics counters
sub handleShowSwitchStats
{
    my ($self) = @_;

    my $chip = $self->{CHIP};

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $switchCounters;
        $chip->fmGetSwitchCounters($switchNum, \$switchCounters);

        print("\n");
        printf("Switch %d: ", $switchNum) if ($switchCount > 1);
        print("Global Switch Statistics:\n");
        print("-------------------------------------------------------------\n");

        $self->printCounter($switchCounters->{cntGlobalLowDropPkts},
                     "Global Low Drops");
        $self->printCounter($switchCounters->{cntGlobalHighDropPkts},
                     "Global High Drops");
        $self->printCounter($switchCounters->{cntGlobalPrivilegeDropPkts},
                     "Global Privileged Drops");
        print("\n");
    }
}

##@method void handleResetSwitch()
#   reset the switch
#
#   @param[in] nothing
sub handleResetSwitch
{
    my ($self,$freq) = @_;

    my $chip = $self->{CHIP};
    my $testpoint = $self->{TESTPOINT};
    my $platform = $self->{PLATFORM};

    my $startup = "Config/$platform/testpoint_startup";

    if (!defined($freq))
    {
        $freq = 0;
    }
    else
    {
        $freq = int($freq);
        if ( $freq < 0 || $freq > 500 ) 
        {
            print("Frequency $freq is incorrect, must be between 1 an 500\n");
            return;
        }
    }
    
    my $status = $chip->fmResetAllSwitchesW($freq);
    if ($status == $FM_OK)
    {
        # load the startup file using the TestPoint instance itself
        $testpoint->loadFile($startup, "expert");
    }
    else
    {
        print("Switch(es) cannot be reset!\n");
    }
}

sub handleResetSwitchStats
{
    my ($self) = @_;
    my $chip = $self->{CHIP};

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status = $chip->fmResetSwitchCounters($switchNum);
        if ($status != $FM_OK)
        {
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            print("Switch statistics cannot be reset!\n");
            return;
        }
    }
}

##@cmethod public void handleShowDbgMACTable(fm_int count)
#
# @desc         Handles showing the global API statistics counters
#
#   @param[in] count The number of entries to dump or "all" for all or
#                    "count" to just count entries
#
sub handleShowDbgMACTable
{
    my ($self, $count) = @_;

    my $chip = $self->{CHIP};
    
    my $entryCount = 0;
    
    if (defined($count))
    {
        if ($count eq "all")
        {
            $entryCount = 65536;
        }
        elsif ($count eq "count")
        {
            $entryCount = -1;
        }
        else
        {
            $entryCount = $self->str2intnum($count);
        }   
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $switchCounters;
        $chip->fmDbgDumpMACTable($switchNum, $entryCount);
        print("\n");
    }
}

##@cmethod public void handleShowApiStats(void)
#
# @desc         Handles showing the global API statistics counters
sub handleShowApiStats
{
    my ($self) = @_;
    my $chip = $self->{CHIP};
    my $status = $chip->fmDbgGlobalDiagCountDump();
}

sub handleResetApiStats
{
    my ($self) = @_;
    my $chip = $self->{CHIP};
    my $status = $chip->fmDbgGlobalDiagCountClearAll();
}


sub handleShowApiSwitchStats
{
    my ($self, $switch) = @_;
    my $chip = $self->{CHIP};

    my @switchList = $self->validateList($switch, $self->tpPlatformGetSwitchRange());
    if (!defined($switch) || (scalar(@switchList) == 0))
    {
        print("Must specify a valid switch or switch range!\n");
        return;
    }
    
    my $switchCount = @switchList;
    
    foreach my $sw (@switchList)
    {
        if ($switchCount > 1)
        {
            print "Switch $sw:\n";
        }
        
        $chip->fmDbgDiagCountDump($sw);
        
        if ($switchCount > 1)
        {
            print "\n";
        }
        
    }
    return;
}

sub handleResetApiSwitchStats
{
    my ($self, $switch) = @_;
    my $chip = $self->{CHIP};

    my @switchList = $self->validateList($switch, $self->tpPlatformGetSwitchRange());
    if (!defined($switch) || (scalar(@switchList) == 0))
    {
        print("Must specify a valid switch or switch range!\n");
        return;
    }

    foreach my $sw (@switchList)
    {
        $chip->fmDbgDiagCountClearAll($sw);
    }
    return;
}

sub handleShowStatsAll
{
    my ($self, $port, $verbose) = @_;

    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    my $quiet = "quiet";
    
    if (defined($verbose) && ($verbose eq "verbose"))
    {
        $quiet = "verbose";
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        printf("%s", scalar($self->tpGetSwitches) > 1 ? "\nSwitch $sw:\n" : "");
        
        foreach my $port (@portList)
        {
            print("\n");
            printf("Statistics: Port $port\n");
            print("-------------------------------------------------------------\n");
            print("RX:\n");
            handleShowRxForwarding($self, $port, $verbose, $FALSE);
            handleShowRxTypes($self, $port, $verbose, $FALSE);
            handleShowRxPriorities($self, $port, $quiet, $FALSE);
            handleShowRxSizes($self, $port, $quiet, $FALSE);

            print("TX:\n");
            handleShowTxTypes($self, $port, $verbose, $FALSE);
            handleShowTxPriorities($self, $port, $quiet, $FALSE);
            handleShowTxOctets($self, $port, $quiet, $FALSE);
            handleShowTxSizes($self, $port, $quiet, $FALSE);

        }
    }

    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);

}

sub handleShowRxTypes
{
    my ($self, $port, $verbose, $showHdr) = @_;

    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }
    
    my $quiet = $TRUE;
    
    if (defined($verbose) && ($verbose eq "verbose"))
    {
        $quiet = undef;
    }

    my $hdr = $TRUE; #Show headers
    if (defined($showHdr) && ($showHdr == $FALSE))
    {
        $hdr = $FALSE;
    }
    
    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        printf("%s", scalar($self->tpGetSwitches) > 1 ? "\nSwitch $sw:\n" : " ") if $hdr;
        
        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);

            my $portCounters;
            my $total = Math::BigInt->new(0);
            if ($hdr)
            {
                print("\n");
                printf("RX Type Statistics (Group 1): Port $port\n");
                print("-------------------------------------------------------------\n");
            }

            $chip->fmGetPortCounters($sw, $logPort, \$portCounters);
        
            $self->printCounter($portCounters->{cntRxUcstPktsNonIP}, 
                                "Unicast   (Non-IP)",$quiet);
            $self->printCounter($portCounters->{cntRxBcstPktsNonIP}, 
                                "Broadcast (Non-IP)",$quiet);
            $self->printCounter($portCounters->{cntRxMcstPktsNonIP}, 
                                "Multicast (Non-IP)",$quiet);
            $self->printCounter($portCounters->{cntRxUcstPktsIPv4}, 
                                "Unicast   (IPv4)",$quiet);
            $self->printCounter($portCounters->{cntRxBcstPktsIPv4}, 
                                "Broadcast (IPv4)",$quiet);
            $self->printCounter($portCounters->{cntRxMcstPktsIPv4}, 
                                "Multicast (IPv4)",$quiet);
            $self->printCounter($portCounters->{cntRxUcstPktsIPv6}, 
                                "Unicast   (IPv6)",$quiet);
            $self->printCounter($portCounters->{cntRxBcstPktsIPv6}, 
                                "Broadcast (IPv6)",$quiet);
            $self->printCounter($portCounters->{cntRxMcstPktsIPv6}, 
                                "Multicast (IPv6)",$quiet);
            $total += $self->printCounter($portCounters->{cntRxUcstPkts}, 
                                          "Unicast",$quiet);
            $total += $self->printCounter($portCounters->{cntRxBcstPkts}, 
                                          "Broadcast",$quiet);
            $total += $self->printCounter($portCounters->{cntRxMcstPkts}, 
                                          "Multicast",$quiet);
            $total += $self->printCounter($portCounters->{cntRxPausePkts},
                                          "Pause frames",$quiet);
            $total += $self->printCounter($portCounters->{cntRxCBPausePkts},
                                          "Pause frames (Class Based)",$quiet);
            $total += $self->printCounter($portCounters->{cntRxFCSErrors}, 
                                          "CRC Errors",$quiet);
            # FM4xxx Only
            if (($portCounters->{cntVersion} & 0x4) > 0)
            {
                $total += $self->printCounter($portCounters->{cntRxSymbolErrors},
                                              "Symbol Errors",$quiet);
                $total += $self->printCounter($portCounters->{cntRxJabberPkts},
                                              "Oversized w/ Bad CRC",$quiet);
                $total += $self->printCounter($portCounters->{cntRxFrameSizeErrors},
                                              "Good CRC but Frame Size Errors",$quiet);
            }

            # FM6xxx Only
            if (($portCounters->{cntVersion} & 0x8) > 0)
            {
                $total += $self->printCounter($portCounters->{cntRxFramingErrorPkts},
                                              "With Framing Error",$quiet);
            }

            if ($hdr)
            {
                my $totalstr = "                                     Total";
                if (defined($quiet))
                {
                    $totalstr = "          Total (Empty Counters Not Shown)";
                }
                $self->printCounter($total, $totalstr);
            }

            $self->printCounter($portCounters->{cntOverrunPkts},
                                          "RX Overflow Counter (non-Group 1)",$quiet);
            # FM6xxx Only
            if (($portCounters->{cntVersion} & 0x8) > 0)
            {
                $self->printCounter($portCounters->{cntCodeErrors},
                                    "Code Counter (non-Group 1)",$quiet);
            }
        }
    }

    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList) if $hdr;

}

sub handleShowRxPriorities
{
    my ($self, $port, $verbose, $showHdr) = @_;

    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }
    
    my $quiet = undef;
    
    if (defined($verbose) && ($verbose eq "quiet"))
    {
        $quiet = $TRUE;
    }

    my $hdr = $TRUE; #Show headers
    if (defined($showHdr) && ($showHdr == $FALSE))
    {
        $hdr = $FALSE;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        printf("%s", scalar($self->tpGetSwitches) > 1 ? "\nSwitch $sw:\n" : " ") if $hdr;
        
        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            my $portCounters;
            
            if ($hdr)
            {
                print("\n");
                printf("RX Per-Priority Statistics (Group 4): Port $port\n");
                print("-------------------------------------------------------------\n");
            }

            $chip->fmGetPortCounters($sw, $logPort, \$portCounters);

            my $maxPriority = 8;
            # FM6xxx Only
            if (($portCounters->{cntVersion} & 0x8) > 0)
            {
                $maxPriority = 16;
            }
        
            for (my $i=0 ; $i<$maxPriority ; $i++)
            {
                $self->printCounter($portCounters->{cntRxPriorityPkts}->[$i],
                             sprintf("Priority%d Packets", $i), $quiet);
            }

            for (my $i=0 ; $i<$maxPriority ; $i++)
            {
                $self->printCounter($portCounters->{cntRxPriorityOctets}->[$i],
                             sprintf("Priority%d Octets", $i), $quiet);
            }

            print("\n") if $hdr;
    
        }
    }
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList) if $hdr;

}

sub handleShowTxPriorities
{
    my ($self, $port, $verbose, $showHdr) = @_;

    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, 
                                             $self->tpPlatformGetPortRange());

    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    my $quiet = undef;
    
    if (defined($verbose) && ($verbose eq "quiet"))
    {
        $quiet = $TRUE;
    }

    my $hdr = $TRUE; #Show headers
    if (defined($showHdr) && ($showHdr == $FALSE))
    {
        $hdr = $FALSE;
    }
    
    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, 
                                    $self->tpPlatformGetSwitchPortList($sw));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        printf("%s", 
               scalar($self->tpGetSwitches) > 1 ? "\nSwitch $sw:\n" : " ") if $hdr;
        
        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = 
                            $self->tpPlatformMapGlobalToLogicalPort($port);

            my $portCounters;
            
            $chip->fmGetPortCounters($sw, $logPort, \$portCounters);

            my $maxPriority = 16;

            # Counter is available in FM6xxx Only
            if (($portCounters->{cntVersion} & 0x8) == 0)
            {
                return;
            }

            if ($hdr)
            {
                print("\n");
                printf("TX Per-Priority Statistics (Group 4): Port $port\n");
                print("-" x 60 . "\n");
            }

            for (my $i = 0 ; $i < $maxPriority ; $i++)
            {
                $self->printCounter($portCounters->{cntTxPriorityOctets}->[$i],
                             sprintf("Priority%d Octets", $i), $quiet);
            }
            print("\n") if $hdr;
        }
    }
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList) if $hdr;
} # end handleShowTxPriorities

sub handleShowRxSizes
{
    my ($self, $port, $verbose, $showHdr) = @_;

    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }
    
    my $quiet = undef;
    
    if (defined($verbose) && ($verbose eq "quiet"))
    {
        $quiet = $TRUE;
    }

    my $hdr = $TRUE; #Show headers
    if (defined($showHdr) && ($showHdr == $FALSE))
    {
        $hdr = $FALSE;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        printf("%s", scalar($self->tpGetSwitches) > 1 ? "\nSwitch $sw:\n" : " ") if $hdr;
        
        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            my $portCounters;
            
            if ($hdr)
            {
                print("\n");
                printf("RX Per-Framesize Statistics (Group 2): Port $port\n");
                print("-------------------------------------------------------------\n");
            }

            $chip->fmGetPortCounters($sw, $logPort, \$portCounters);
        
            $self->printCounter($portCounters->{cntRxMinTo63Pkts},     "Min To 63 Bytes", $quiet);
            $self->printCounter($portCounters->{cntRx64Pkts},          "64 Bytes", $quiet);
            $self->printCounter($portCounters->{cntRx65to127Pkts},     "65 To 127 Bytes", $quiet);
            $self->printCounter($portCounters->{cntRx128to255Pkts},    "128 To 255 Bytes", $quiet);
            $self->printCounter($portCounters->{cntRx256to511Pkts},    "256 To 511 Bytes", $quiet);
            $self->printCounter($portCounters->{cntRx512to1023Pkts},   "512 To 1023 Bytes", $quiet);
            $self->printCounter($portCounters->{cntRx1024to1522Pkts},  "1024 To 1522 Bytes", $quiet);
            $self->printCounter($portCounters->{cntRx1523to2047Pkts},  "1523 To 2047 Bytes", $quiet);
            $self->printCounter($portCounters->{cntRx2048to4095Pkts},  "2048 To 4095 Bytes", $quiet);
            $self->printCounter($portCounters->{cntRx4096to8191Pkts},  "4096 To 8191 Bytes", $quiet);
            $self->printCounter($portCounters->{cntRx8192to10239Pkts}, "8192 To 10239 Bytes", $quiet);
            $self->printCounter($portCounters->{cntRx10240toMaxPkts},  "10240 To MAX Bytes", $quiet);

            # FM4xxx Only
            if (($portCounters->{cntVersion} & 0x4) > 0)
            {

            }

            # FM4xxx and FM6xxx
            if (($portCounters->{cntVersion} & 0xc) > 0)
            {
                $self->printCounter($portCounters->{cntRxJabberPkts},     "Jabber", $quiet);
                $self->printCounter($portCounters->{cntRxFragmentPkts},   "Fragmented", $quiet);
                $self->printCounter($portCounters->{cntRxUndersizedPkts}, "Undersized", $quiet);
                $self->printCounter($portCounters->{cntRxOversizedPkts},  "Oversized", $quiet);
            }
            print("\n") if $hdr;
        }
    }
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList) if $hdr;

}

sub handleShowTxTypes
{
    my ($self, $port, $verbose, $showHdr) = @_;

    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }
    
    my $quiet = $TRUE;
    if (defined($verbose) && ($verbose eq "verbose"))
    {
        $quiet = undef;
    }

    my $hdr = $TRUE; #Show headers
    if (defined($showHdr) && ($showHdr == $FALSE))
    {
        $hdr = $FALSE;
    }
 
    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        printf("%s", scalar($self->tpGetSwitches) > 1 ? "\nSwitch $sw:\n" : " ") if $hdr;
        
        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            my $portCounters;
            
            my $total = Math::BigInt->new(0);

            if ($hdr)
            {
                print("\n");
                printf("TX Type Statistics (Group 7): Port $port\n");
                print("-------------------------------------------------------------\n");
            }

            $chip->fmGetPortCounters($sw, $logPort, \$portCounters);

            $total += $self->printCounter($portCounters->{cntTxUcstPkts}, 
                                "Unicast",$quiet);
            $total += $self->printCounter($portCounters->{cntTxBcstPkts}, 
                                "Broadcast",$quiet);
            $total += $self->printCounter($portCounters->{cntTxMcstPkts}, 
                                "Multicast",$quiet);
            $total += $self->printCounter($portCounters->{cntTxFCSErroredPkts}, 
                                "Bad CRC",$quiet);
            $total += $self->printCounter($portCounters->{cntTxPausePkts}, 
                                "PAUSE",$quiet);
            $total += $self->printCounter($portCounters->{cntTxTimeOutPkts}, 
                                "Timeout Drops",$quiet);
            $total += $self->printCounter($portCounters->{cntTxLoopbackPkts}, 
                                "Loopback Suppression Drops",$quiet);

            # FM4xxx Only
            if (($portCounters->{cntVersion} & 0x4) > 0)
            {
                $total += $self->printCounter($portCounters->{cntTxErrorDropPkts}, 
                                    "Error Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntTxCMDropPkts},
                                    "TX Congestion Drops",$quiet);
            }

            # FM6xxx Only
            if (($portCounters->{cntVersion} & 0x8) > 0)
            {
                $self->printCounter($portCounters->{cntTxUcstPktsNonIP}, 
                                    "Unicast   (Non-IP)",$quiet);
                $self->printCounter($portCounters->{cntTxBcstPktsNonIP}, 
                                    "Broadcast (Non-IP)",$quiet);
                $self->printCounter($portCounters->{cntTxMcstPktsNonIP}, 
                                    "Multicast (Non-IP)",$quiet);
                $self->printCounter($portCounters->{cntTxUcstPktsIP}, 
                                    "Unicast   (IP)",$quiet);
                $self->printCounter($portCounters->{cntTxBcstPktsIP}, 
                                    "Broadcast (IP)",$quiet);
                $self->printCounter($portCounters->{cntTxMcstPktsIP}, 
                                    "Multicast (IP)",$quiet);
                
                $total += $self->printCounter($portCounters->{cntTxFramingErrorPkts}, 
                                    "Framing Error",$quiet);
                $total += $self->printCounter($portCounters->{cntTxMirrorPkts}, 
                                    "Mirrored",$quiet);
                $total += $self->printCounter($portCounters->{cntTxOutOfMemErrPkts}, 
                                    "TX Out of Memory Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntTxUnrepairEccPkts}, 
                                    "Unrepairable ECC erors Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntTxFCSErrDropPkts}, 
                                    "Bad CRC Drops",$quiet);
                $total += $self->printCounter($portCounters->{cntTxCMDropPkts},
                                    "CM TX Port Drops",$quiet);
            }

            if ($hdr)
            {
                my $totalstr = "                                     Total";
                if (defined($quiet))
                {
                    $totalstr = "          Total (Empty Counters Not Shown)";
                }
                $self->printCounter($total, $totalstr);
            }

            # FM4xxx Only
            if (($portCounters->{cntVersion} & 0x4) > 0)
            {
                $self->printCounter($portCounters->{cntUnderrunPkts},
                                    "TX Underflow Counter   (non-Group 7)",$quiet);
                $self->printCounter($portCounters->{cntCorruptedPkts},
                                    "Unexpectedly Corrupted (non-Group 7)", $quiet);
            }
            
            # FM6xxx Only
            if (($portCounters->{cntVersion} & 0x4) > 0)
            {
                $self->printCounter($portCounters->{cntUnderrunPkts},
                                    "TX Underflow Counter   (non-Group 7)",$quiet);
            }
        }
    }
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList) if $hdr;

}

sub handleShowTxSizes
{
    my ($self, $port, $verbose, $showHdr) = @_;

    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }
    
    my $quiet = undef;
    
    if (defined($verbose) && ($verbose eq "quiet"))
    {
        $quiet = $TRUE;
    }

    my $hdr = $TRUE; #Show headers
    if (defined($showHdr) && ($showHdr == $FALSE))
    {
        $hdr = $FALSE;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        printf("%s", scalar($self->tpGetSwitches) > 1 ? "\nSwitch $sw:\n" : " ") if $hdr;
        
        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            my $portCounters;
            
            if ($hdr)
            {
                print("\n");
                printf("TX Per-Framesize Statistics (Group 8): Port $port\n");
                print("-------------------------------------------------------------\n");
            }

            $chip->fmGetPortCounters($sw, $logPort, \$portCounters);
        
            $self->printCounter($portCounters->{cntTxMinTo63Pkts},     "Min To 63 Bytes", $quiet);
            $self->printCounter($portCounters->{cntTx64Pkts},          "64 Bytes", $quiet);
            $self->printCounter($portCounters->{cntTx65to127Pkts},     "65 To 127 Bytes", $quiet);
            $self->printCounter($portCounters->{cntTx128to255Pkts},    "128 To 255 Bytes", $quiet);
            $self->printCounter($portCounters->{cntTx256to511Pkts},    "256 To 511 Bytes", $quiet);
            $self->printCounter($portCounters->{cntTx512to1023Pkts},   "512 To 1023 Bytes", $quiet);
            $self->printCounter($portCounters->{cntTx1024to1522Pkts},  "1024 To 1522 Bytes", $quiet);
            $self->printCounter($portCounters->{cntTx1523to2047Pkts},  "1523 To 2047 Bytes", $quiet);
            $self->printCounter($portCounters->{cntTx2048to4095Pkts},  "2048 To 4095 Bytes", $quiet);
            $self->printCounter($portCounters->{cntTx4096to8191Pkts},  "4096 To 8191 Bytes", $quiet);
            $self->printCounter($portCounters->{cntTx8192to10239Pkts}, "8192 To 10239 Bytes", $quiet);
            $self->printCounter($portCounters->{cntTx10240toMaxPkts},  "10240 To MAX Bytes", $quiet);
            print("\n") if $hdr;
    
        }
    }
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList) if $hdr;

}

sub handleShowTxOctets
{
    my ($self, $port, $verbose, $showHdr) = @_;

    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }
    
    my $quiet = undef;
    
    if (defined($verbose) && ($verbose eq "quiet"))
    {
        $quiet = $TRUE;
    }

    my $hdr = $TRUE; #Show headers
    if (defined($showHdr) && ($showHdr == $FALSE))
    {
        $hdr = $FALSE;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        printf("%s", scalar($self->tpGetSwitches) > 1 ? "\nSwitch $sw:\n" : " ") if $hdr;
        
        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            my $portCounters;
            
            if ($hdr)
            {
                print("\n");
                printf("TX Octets Statistics: Port $port\n");
                print("-------------------------------------------------------------\n");
            }

            $chip->fmGetPortCounters($sw, $logPort, \$portCounters);
        
            $self->printCounter($portCounters->{cntTxOctets},          "TX Octets", $quiet);
    
        }
    }
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList) if $hdr;

}

##@cmethod public int tpHandleShowTxRates(char *port, int interval)
#
# @brief        handles showing the TX frame and byte rates
#
# @param[in]    port
#
# @param[in]    interval
sub tpHandleShowTxRates
{
    my ($self, $port, $interval) = @_;

    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    
    if (defined($interval))
    {
        $interval =  $self->str2intnum($interval);
        if (!defined($interval))
        {
            print("<interval> needs to be a positive integer!\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }
    else
    {
        $interval = 1;
    }

    my $result = $FM_OK;
    
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, 
                                $self->tpPlatformGetSwitchPortList($switchNum));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        printf("%s", scalar($self->tpGetSwitches) > 1 ? "\nSwitch $switchNum:\n" : " ");

        my @counters0;
        foreach my $globalPort (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned sw since it should match switchNum and may actually be
            # wrong for some platforms.
            my ($sw, $logicalPort) = 
                            $self->tpPlatformMapGlobalToLogicalPort($globalPort);
                            
            $chip->fmGetPortCounters($switchNum, $logicalPort,
                                     \$counters0[$logicalPort]);
        }

        usleep(1.0E6 * $interval);

        foreach my $globalPort (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned sw since it should match switchNum and may actually be
            # wrong for some platforms.
            my ($sw, $logicalPort) = 
                            $self->tpPlatformMapGlobalToLogicalPort($globalPort);
                            
            my ($counters1, $status);
    
            $status = $chip->fmGetPortCounters($switchNum, $logicalPort,
                                               \$counters1);
            # Compute the number of frames received per-bin within the specified
            # interval.
            my %delta = ();
            $delta{'64'}    = $counters1->{'cntTx64Pkts'}
                              - $counters0[$logicalPort]->{'cntTx64Pkts'};
            $delta{'65'}    = $counters1->{'cntTx65to127Pkts'}
                              - $counters0[$logicalPort]->{'cntTx65to127Pkts'};
            $delta{'128'}   = $counters1->{'cntTx128to255Pkts'}
                              - $counters0[$logicalPort]->{'cntTx128to255Pkts'};
            $delta{'256'}   = $counters1->{'cntTx256to511Pkts'}
                              - $counters0[$logicalPort]->{'cntTx256to511Pkts'};
            $delta{'512'}   = $counters1->{'cntTx512to1023Pkts'}
                              - $counters0[$logicalPort]->{'cntTx512to1023Pkts'};
            $delta{'1024'}  = $counters1->{'cntTx1024to1522Pkts'}
                              - $counters0[$logicalPort]->{'cntTx1024to1522Pkts'};
            $delta{'1523'}  = $counters1->{'cntTx1523to2047Pkts'}
                              - $counters0[$logicalPort]->{'cntTx1523to2047Pkts'};
            $delta{'2048'}  = $counters1->{'cntTx2048to4095Pkts'}
                              - $counters0[$logicalPort]->{'cntTx2048to4095Pkts'};
            $delta{'4096'}  = $counters1->{'cntTx4096to8191Pkts'}
                              - $counters0[$logicalPort]->{'cntTx4096to8191Pkts'};
            $delta{'8192'}  = $counters1->{'cntTx8192to10239Pkts'}
                              - $counters0[$logicalPort]->{'cntTx8192to10239Pkts'};
            $delta{'10240'} = $counters1->{'cntTx10240toMaxPkts'}
                              - $counters0[$logicalPort]->{'cntTx10240toMaxPkts'};
            # Compute the total number of frames sent within the specified
            # interval.
            my $numFrames = Math::BigInt->new(0);
            map {$numFrames += $delta{$_}} keys(%delta);
            # Compute the total number of bytes sent within the specified interval.
            my $numBytes = $counters1->{'cntTxOctets'} - $counters0[$logicalPort]->{'cntTxOctets'};
    
            print("\n");
            printf("TX Transmit Rate Estimates: Port %d\n", $globalPort);
            print("-------------------------------------------------------------\n");
    
            $self->printRate(Math::BigFloat->new($interval),
                             "Sleep Interval (seconds)");
    
            my $elapsedUsec = $counters1->{'timestamp'} - $counters0[$logicalPort]->{'timestamp'};
            my $rate64 = $delta{'64'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate64, "TX 64 B Frame Rate");
            my $rate65 = $delta{'65'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate65, "TX 65 to 123 B Frame Rate");
            my $rate128 = $delta{'128'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate128, "TX 128 to 511 B Frame Rate");
            my $rate256 = $delta{'256'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate256, "TX 256 to 511 B Frame Rate");
            my $rate512 = $delta{'512'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate512, "TX 512 to 1023 B Frame Rate");
            my $rate1024 = $delta{'1024'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate1024, "TX 1024 to 1522 B Frame Rate");
            my $rate1523 = $delta{'1523'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate1523, "TX 1523 to 2048 B Frame Rate");
            my $rate2048 = $delta{'2048'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate2048, "TX 2048 to 4095 B Frame Rate");
            my $rate4096 = $delta{'4096'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate4096, "TX 4096 to 8191 B Frame Rate");
            my $rate8192 = $delta{'8192'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate8192, "TX 8192 to 10239 B Frame Rate");
            my $rate10240 = $delta{'10240'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate10240, "TX 10240 to Max B Frame Rate");
            my $rateTotal = $numFrames->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rateTotal, "Total");

            if ($rateTotal > 0 && $numBytes == 0)
            {
                #No total bytes stats support
                printf("%-42s %20s\n", "TX Bytes Rate", "N/A");
                printf("%-42s %20s\n", "TX Utilization", "N/A");
            }
            else
            {
                # Retrieve the inter-frame gap for the specified port.
                my %void = (type => "fm_uint32", value => 0);
                $status = $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                    $FM_PORT_IFG, \%void);
                my $bandwidth = $numFrames->copy()->bmul($void{'value'} + 8);

                %void = (type => "fm_uint32", value => 0);
                $chip->disableErrors();
                $chip->fmGetPortAttribute($switchNum, $logicalPort, $FM_PORT_SPEED, \%void);
                $chip->enableErrors();
                my $speed = ($status != $FM_OK) ? 100000000 : $void{value}*10000;

                $bandwidth += $numBytes;
                # Convert from byte to bit.
                $bandwidth->bmul(8);
                $bandwidth->bmul(1000000); #Adjust for Usec
                $bandwidth->bdiv($elapsedUsec);
                $bandwidth->bdiv($speed);
                my $byteRate = $numBytes->copy()->bmul(1000000)->bdiv($elapsedUsec);
                $self->printRate($byteRate, "TX Bytes Rate");
                $self->printUtilization($bandwidth, "TX Utilization");
            }
        }
    }
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $result;
}

##@cmethod public int tpHandleShowRxRates(char *port, int interval)
#
# @brief        handles showing the RX frame and byte rates
#
# @param[in]    port
#
# @param[in]    interval
sub tpHandleShowRxRates
{
    my ($self, $port, $interval) = @_;

    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    
    if (defined($interval))
    {
        $interval =  $self->str2intnum($interval);
        if (!defined($interval))
        {
            print("<interval> needs to be a positive integer!\n");
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }
    else
    {
        $interval = 1;
    }

    my $result = $FM_OK;
    
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, 
                                $self->tpPlatformGetSwitchPortList($switchNum));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        printf("%s", scalar($self->tpGetSwitches) > 1 ? "\nSwitch $switchNum:\n" : " ");
       
        my @counters0; 
        foreach my $globalPort (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned sw since it should match switchNum and may actually be
            # wrong for some platforms.
            my ($sw, $logicalPort) = 
                            $self->tpPlatformMapGlobalToLogicalPort($globalPort);
                            
            $chip->fmGetPortCounters($switchNum, $logicalPort,
                                     \$counters0[$logicalPort]);
        }
        usleep(1.0E6 * $interval);

        foreach my $globalPort (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned sw since it should match switchNum and may actually be
            # wrong for some platforms.
            my ($sw, $logicalPort) = 
                            $self->tpPlatformMapGlobalToLogicalPort($globalPort);
                            
            my ($counters1, $status);
    
            $status = $chip->fmGetPortCounters($switchNum, $logicalPort,
                                               \$counters1);
            # Compute the number of frames received per-bin within the specified
            # interval.
            my %delta = ();
            $delta{'64'}    = $counters1->{'cntRx64Pkts'}
                              - $counters0[$logicalPort]->{'cntRx64Pkts'};
            $delta{'65'}    = $counters1->{'cntRx65to127Pkts'}
                              - $counters0[$logicalPort]->{'cntRx65to127Pkts'};
            $delta{'128'}   = $counters1->{'cntRx128to255Pkts'}
                              - $counters0[$logicalPort]->{'cntRx128to255Pkts'};
            $delta{'256'}   = $counters1->{'cntRx256to511Pkts'}
                              - $counters0[$logicalPort]->{'cntRx256to511Pkts'};
            $delta{'512'}   = $counters1->{'cntRx512to1023Pkts'}
                              - $counters0[$logicalPort]->{'cntRx512to1023Pkts'};
            $delta{'1024'}  = $counters1->{'cntRx1024to1522Pkts'}
                              - $counters0[$logicalPort]->{'cntRx1024to1522Pkts'};
            $delta{'1523'}  = $counters1->{'cntRx1523to2047Pkts'}
                              - $counters0[$logicalPort]->{'cntRx1523to2047Pkts'};
            $delta{'2048'}  = $counters1->{'cntRx2048to4095Pkts'}
                              - $counters0[$logicalPort]->{'cntRx2048to4095Pkts'};
            $delta{'4096'}  = $counters1->{'cntRx4096to8191Pkts'}
                              - $counters0[$logicalPort]->{'cntRx4096to8191Pkts'};
            $delta{'8192'}  = $counters1->{'cntRx8192to10239Pkts'}
                              - $counters0[$logicalPort]->{'cntRx8192to10239Pkts'};
            $delta{'10240'} = $counters1->{'cntRx10240toMaxPkts'}
                              - $counters0[$logicalPort]->{'cntRx10240toMaxPkts'};
            # Compute the total number of frames sent within the specified
            # interval.
            my $numFrames = Math::BigInt->new(0);
            map {$numFrames += $delta{$_}} keys(%delta);
            # Compute the total number of bytes sent within the specified interval.
            my $numBytes = $counters1->{'cntRxGoodOctets'} - $counters0[$logicalPort]->{'cntRxGoodOctets'};
    
            print("\n");
            printf("RX Transmit Rate Estimates: Port %d\n", $globalPort);
            print("-------------------------------------------------------------\n");
    
            $self->printRate(Math::BigFloat->new($interval),
                             "Sleep Interval (seconds)");
    
            my $elapsedUsec = $counters1->{'timestamp'} - $counters0[$logicalPort]->{'timestamp'};
            my $rate64 = $delta{'64'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate64, "RX 64 B Frame Rate");
            my $rate65 = $delta{'65'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate65, "RX 65 to 123 B Frame Rate");
            my $rate128 = $delta{'128'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate128, "RX 128 to 511 B Frame Rate");
            my $rate256 = $delta{'256'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate256, "RX 256 to 511 B Frame Rate");
            my $rate512 = $delta{'512'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate512, "RX 512 to 1023 B Frame Rate");
            my $rate1024 = $delta{'1024'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate1024, "RX 1024 to 1522 B Frame Rate");
            my $rate1523 = $delta{'1523'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate1523, "RX 1523 to 2048 B Frame Rate");
            my $rate2048 = $delta{'2048'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate2048, "RX 2048 to 4095 B Frame Rate");
            my $rate4096 = $delta{'4096'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate4096, "RX 4096 to 8191 B Frame Rate");
            my $rate8192 = $delta{'8192'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate8192, "RX 8192 to 10239 B Frame Rate");
            my $rate10240 = $delta{'10240'}->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rate10240, "RX 10240 to Max B Frame Rate");
            my $rateTotal = $numFrames->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($rateTotal, "Total");
    
            # Retrieve the inter-frame gap for the specified port.
            my %void = (type => "fm_uint32", value => 0);
            $status = $chip->fmGetPortAttribute($switchNum, $logicalPort,
                                                $FM_PORT_IFG, \%void);
            my $bandwidth = $numFrames->copy()->bmul($void{'value'} + 8);

            %void = (type => "fm_uint32", value => 0);
            $chip->disableErrors();
            $chip->fmGetPortAttribute($switchNum, $logicalPort, $FM_PORT_SPEED, \%void);
            $chip->enableErrors();
            my $speed = ($status != $FM_OK) ? 100000000 : $void{value}*10000;

            $bandwidth += $numBytes;
            # Convert from byte to bit.
            $bandwidth->bmul(8);
            $bandwidth->bmul(1000000)->bdiv($elapsedUsec);
            $bandwidth->bdiv($speed);
            my $byteRate = $numBytes->copy()->bmul(1000000)->bdiv($elapsedUsec);
            $self->printRate($byteRate, "RX Bytes Rate");
            $self->printUtilization($bandwidth, "RX Utilization");
        }
    }
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    return $result;
}


sub handleResetPortStats
{
    my ($self, $port) = @_;

    my $chip = $self->{CHIP};

    my @affectedPortList = ();
    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port, $self->tpPlatformGetSwitchPortList($sw));
        
        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
            
            $chip->disableErrors();
            $chip->fmResetPortCounters($sw, $logPort);
            $chip->enableErrors();
        }
    }
    
    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    
}

##@method void handleShowVersion()
#   Dump the build info
sub handleShowVersion
{
    my ($self) = @_;
    my $chip = $self->{CHIP};
    my $testpoint = $self->{TESTPOINT};

    $chip->disableErrors();
    print("\n");
    printf("SDK Build ID      : %s\n", $chip->fmDbgGetVersion());
    print("\n");
    $chip->enableErrors();
}

##@method void handleShowPlatform()
#   Dump the build info
sub handleShowPlatform
{
    my ($self) = @_;
    my $chip = $self->{CHIP};
    my $testpoint = $self->{TESTPOINT};

    $chip->disableErrors();
    my %void = (type => "fm_char", value => "");
    $chip->fmPlatformGetAttribute(0, 0, $FM_PLATFORM_ATTR_NAME, \%void);
    printf("Platform %s\n", $void{value});      
    $chip->enableErrors();
}

##@cmethod public void handleSendPacket(int     port,
#                                       int     length,
#                                       char    *DMAC,
#                                       char    *SMAC,
#                                       char    *payload[])
sub handleSendPacket
{
    my ($self, $port, $length, $DMAC, $SMAC, @payload) = @_;
    my $vlan=0;
    my $dvlan=0;
    my $tmp=0;
    my @portList=();
    
    my $chip = $self->{CHIP};
    
    if ( $port eq "vlan" )
    {
        ($self, $tmp, $vlan, $length, $DMAC, $SMAC, @payload) = @_;
        $port = 0;
        @portList = (0);
        if ( $vlan < 0 || $vlan > 4095 ) 
        {
            # $vlan == 0 indicates not in direct vlan send mode
            printf ("Invalid VLAN specified\n");
            return;
        }
    }
    elsif ( $port eq "dvlan" )
    {
        ($self, $tmp, $dvlan, $vlan, $length, $DMAC, $SMAC, @payload) = @_;
        $port = $FM_DIRECT_VLAN_SEND;
        @portList = ($FM_DIRECT_VLAN_SEND);
        if ( $vlan < 0 || $vlan > 4095 ) 
        {
            # $vlan == 0 indicates not in direct vlan send mode
            printf ("Invalid VLAN specified\n");
            return;
        }
    }

    if ( !defined($length) )
    {
        $length = 64;
    }
    
    my $status;
    ($status, @payload) = ParsePacketFields(@_);
    
    if ($status != $FM_OK)
    {
        return;
    }

    $self->PadPayload($length, \@payload);

    $chip->tpPacketPayloadClear();
    $chip->tpPacketPayloadSet($length, \@payload);

    my @affectedPortList = ();
    my @globalPortList = ();
    
    if (scalar(@portList) == 0)
    {
        @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
        if (!defined($port) || (scalar(@globalPortList) == 0))
        {
            print($TP_MSG_ERR_PORT_INVALID_ARRAY);
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
                my $sw; 
                ($sw, $logicalPort) = 
                                  $self->tpPlatformMapGlobalToLogicalPort($globalPort);
            }
    
            push(@switchPortList, $logicalPort);
        }
        
        if (scalar(@switchPortList) > 0)
        {
            $chip->tpSendPacketHelper($switchNum, 
                                      \@switchPortList,
                                      scalar(@switchPortList),
                                      $vlan,
                                      $dvlan,
                                      $length);
        }
        
    }   # for each switch 

    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    
}


##@cmethod public void ParsePacketFields(int     port,
#                                       int     length,
#                                       char    *DMAC,
#                                       char    *SMAC,
#                                       char    *payload[])
#
# +--                  --+
# | <ports>              |
# | vlan    <vid>        |                        +--                                            --+
# | dvlan   <dvid> <vid> | <length> <dmac> <smac> | <octets>                                       |
# +--                  --+                        | vtag[4b]  <vid> <vpri> ethtype <type> <octets> |
#                                                 | ethtype   <type> <octets>                      |
#                                                 +--                                            --+
#
# Returns:  ($err, @payload)
sub ParsePacketFields
{
    my ($self, $port, $length, $DMAC, $SMAC, @payload) = @_;
    my $vlan=0;
    my $dvlan=0;
    my $tmp=0;
    my @portList=();
    my $vlanId;
    my $vpri;
    my $ethType;
    my $dmacHi;    
    my $dmacLo;
    my $smacHi;    
    my $smacLo;
    
    
    if ( $port eq "vlan" )
    {
        ($self, $tmp, $vlan, $length, $DMAC, $SMAC, @payload) = @_;
        $port = 0;
        @portList = (0);
        
        my ($minimum, $maximum) = $self->tpPlatformGetVLANRange;
        if (($vlan < $minimum || $vlan > $maximum))
        {
            print($TP_MSG_ERR_VLAN_INVALID_SCALAR);
            return ($FM_ERR_INVALID_ARGUMENT, "");
        }
    }
    elsif ( $port eq "dvlan" )
    {
        ($self, $tmp, $dvlan, $vlan, $length, $DMAC, $SMAC, @payload) = @_;
        $port = $FM_DIRECT_VLAN_SEND;
        @portList = ($FM_DIRECT_VLAN_SEND);
        
        my ($minimum, $maximum) = $self->tpPlatformGetVLANRange;
        if (($vlan < $minimum || $vlan > $maximum))
        {
            print($TP_MSG_ERR_VLAN_INVALID_SCALAR);
            return ($FM_ERR_INVALID_ARGUMENT, "");
        }
    }

    if (defined($DMAC))
    {
        my $result;
        ($dmacLo, $dmacHi, $result) = $self->buildMacAddress($DMAC);
        
        if ($result != $FM_OK)
        {
            print($TP_MSG_ERR_PKT_INVALID_DMAC);
            return ($FM_ERR_INVALID_ARGUMENT, "");
        }
    }

    if (defined($SMAC))
    {
        my $result;
        ($smacLo, $smacHi, $result) = $self->buildMacAddress($SMAC);
        
        if ($result != $FM_OK)
        {
            print($TP_MSG_ERR_PKT_INVALID_SMAC);
            return ($FM_ERR_INVALID_ARGUMENT, "");
        }
    }

    if (defined($payload[0]) && (!defined($DMAC) || !defined($SMAC)))
    {
        print("Must specify <port> <length> <dmac> <smac> <payload>!\n");
        return ($FM_ERR_INVALID_ARGUMENT, "");
    }
    
    if ( defined($payload[0]) &&
         ( ( $payload[0] eq "vtag" ) || ( $payload[0] eq "vtag4b" ) ) )
    {
        my $command = shift(@payload);

        if (!defined($payload[0]) || !defined($payload[1]))
        {
            print("Must specify <vid> <vpri>!\n");
            return ($FM_ERR_INVALID_ARGUMENT, "");
        }
        
        $vlanId = shift(@payload);
        $vpri = shift(@payload);

        my ($minimum, $maximum) = $self->tpPlatformGetVLANRange;
        if (($vlanId < $minimum || $vlanId > $maximum))
        {
            print($TP_MSG_ERR_VLAN_INVALID_SCALAR);
            return ($FM_ERR_INVALID_ARGUMENT, "");
        }
        
        my $maxVpri = ( $command eq "vtag4b" ? 15 : 7 );
        if ( ( $vpri < 0 ) || ( $vpri > $maxVpri ) )
        {
            print($TP_MSG_ERR_VLAN_INVALID_PRI);
            return ($FM_ERR_INVALID_ARGUMENT, "");
        }
        $vpri = $vpri << ( $command eq "vtag4b" ? 0 : 1 );
    }
    
    if (defined($payload[0]) && $payload[0] eq "ethtype")
    {
        if (!defined($payload[1]))
        {
            print("Must specify <type>!\n");
            return ($FM_ERR_INVALID_ARGUMENT, "");
        }
        
        $tmp = shift(@payload);
        $ethType = shift(@payload);
    }
    
    if (defined($payload[0]))
    {
        @payload = map {$self->str2hexnum($_)} @payload;
        if (sum(map {int(defined($_))} @payload) != scalar(@payload))
        {
            print("Must specify payload as hexadecimal byte values!\n");
            return ($FM_ERR_INVALID_ARGUMENT, "");
        }
    }
    
    if (defined($ethType))
    {
        my $type = $self->str2hexnum($ethType);
        unshift(@payload, ($type & 0x00ff) );
        unshift(@payload, ($type & 0xff00) >> 8 );
    }

    if (defined($vlanId) && defined($vpri))
    {
        my $vtag = (($self->str2decnum($vpri)) << 12) | $self->str2decnum($vlanId);
        unshift(@payload, ($vtag & 0x00ff));
        unshift(@payload, ($vtag & 0xff00) >> 8);
        unshift(@payload, hex("00"));
        unshift(@payload, hex("81"));
    }

    if (defined($SMAC))
    {
        unshift(@payload, ($smacLo & 0xFF));
        unshift(@payload, ($smacLo & 0xFF00) >> 8);
        unshift(@payload, ($smacLo & 0xFF0000) >> 16);
        unshift(@payload, ($smacHi & 0xFF));
        unshift(@payload, ($smacHi & 0xFF00) >> 8);
        unshift(@payload, ($smacHi & 0xFF0000) >> 16);
    }
    
    if (defined($DMAC))
    {
        unshift(@payload, ($dmacLo & 0xFF));
        unshift(@payload, ($dmacLo & 0xFF00) >> 8);
        unshift(@payload, ($dmacLo & 0xFF0000) >> 16);
        unshift(@payload, ($dmacHi & 0xFF));
        unshift(@payload, ($dmacHi & 0xFF00) >> 8);
        unshift(@payload, ($dmacHi & 0xFF0000) >> 16);
    }

    return ($FM_OK, @payload);
    
}

sub PadPayload
{
    my ($self, $length, $payload) = @_;

    # Pad the specified payload buffer to the specified length with incremental
    # data starting at 00.
    my $remaining = $length - scalar(@{$payload});
    push(@{$payload}, map {$_ & 0xFF} 0 .. ($remaining - 1));
}

sub buildMacAddress
{
    my ($self, $val) = @_;
   
    my $macHigh=0;
    my $macLow=0;
    if ($val =~ m/^(u|m).*$/)
    {
        if ( !defined($randomMacs{$val}) )
        {
            if ( $val =~ m/^u/ )
            {
                $macHigh = int(rand(0x1000000)) & 0xFEFFFF;
            }
            else
            {
                $macHigh = int(rand(0x1000000)) | 0x010000;
            }
            $macLow = int(rand(0x1000000));
            $randomMacs{$val} = [$macLow,$macHigh];
            printf("Generated mac=%02x:%02x:%02x:%02x:%02x:%02x for $val\n",
                    ($macHigh & 0xFF0000) >> 16,
                    ($macHigh & 0x00FF00) >> 8,
                    ($macHigh & 0x0000FF),
                    ($macLow & 0xFF0000) >> 16,
                    ($macLow & 0x00FF00) >> 8,
                    ($macLow & 0x0000FF))
        }
        else
        {
            $macHigh = $randomMacs{$val}->[1];
            $macLow = $randomMacs{$val}->[0];
            printf("Using mac=%02x:%02x:%02x:%02x:%02x:%02x for $val\n",
                    ($macHigh & 0xFF0000) >> 16,
                    ($macHigh & 0x00FF00) >> 8,
                    ($macHigh & 0x0000FF),
                    ($macLow & 0xFF0000) >> 16,
                    ($macLow & 0x00FF00) >> 8,
                    ($macLow & 0x0000FF))
        }
    }
    elsif ($val =~ m/^b$/)
    {
        $macHigh = 0xFFFFFF;
        $macLow = 0xFFFFFF;
    }
    elsif ( $val =~ m/:/ )
    {
        if ($self->validateL2Address($val) != $FM_OK)
        {
            return (0, 0, $FM_FAIL);
        }
        
        my @w = (0,0,0,0,0,0);
        my @v = split(/:/,$val);
        my $nv = (@v > 6) ? 6 : @v;
        for (my $i=0; $i < $nv; $i++)
        {
            $w[$i+(6-$nv)] = $v[$i];
        }
        $macHigh = ((hex($w[0])&0xFF)<<16) +
                   ((hex($w[1])&0xFF)<< 8) +
                   ((hex($w[2])&0xFF)<< 0);
        $macLow  = ((hex($w[3])&0xFF)<<16) +
                   ((hex($w[4])&0xFF)<< 8) +
                   ((hex($w[5])&0xFF)<< 0);
    }
    else
    {
        print "Invalid MAC format ($val); try XX:XX:XX:XX:XX:XX or uX or mX or b\n";
        return (0, 0, $FM_FAIL);
    }
    return ($macLow, $macHigh, $FM_OK);
}

sub handleSendPacketDirected
{
    my ($self,@args) = @_;
    
    handleSendPacketDirectedOrSwitched($self,0,@args);
}

sub handleSendPacketSwitched
{
    my ($self,@args) = @_;
    
    handleSendPacketDirectedOrSwitched($self,1,0,@args);
}

sub handleSendPacketDirectedOrSwitched
{
    my ($self, $switched, $swOrPort, @args) = @_;
    my $port = $swOrPort;
    my $vid = -1;
    my $dmacHigh = 0;
    my $dmacLow = 2;
    my $smacHigh = 0;
    my $smacLow = 1;
    my $length = 64;
    my $vpri = 0;
    my $etype = 0;
    my $swpri = 0;
    my $fcs = 0;
    my $tmp = 0;
    my @portList = ();
    my $chip = $self->{CHIP};
    my $sw = int($swOrPort);
    my (@payload);
    
    ############################################
    # Analyze arguments
    ############################################
    
    if ( $args[0] eq "keys" )
    {
        shift(@args);
        
        foreach my $arg (@args)
        {
            my ($type,$val) = split('=', lc($arg));
            
            if ( !defined($val) || $val eq "" || !defined($type) || $type eq "" )
            {
                print "Invalid argument key=<$type>, val=<$val>\n";
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
                if ( $length < 64 || $length > 10240 )
                {
                    print "Invalid length <$length>, must be between 64 and 10240\n"; 
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
                }
            }
            elsif ( ( $type eq "vpri" ) || ( $type eq "vpri4b" ) )
            {
                if ( $val =~ m/^0x/ )
                {
                    $vpri = hex($val);
                }
                else
                {
                    $vpri = int($val);
                }
                my $maxVpri = ( $type eq "vpri4b" ? 15 : 7 );
                if ( $vpri < 0 || $vpri > $maxVpri )
                {
                    print "Invalid vpri <$vpri>, must be between 0 and $maxVpri\n"; 
                }
                $vpri = $vpri << ( $type eq "vpri4b" ? 0 : 1 );
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
            elsif ( $type eq "swpri" )
            {
                if ( $val =~ m/^0x/ )
                {
                    $swpri = hex($val);
                }
                else
                {
                    $swpri = int($val);
                }
            }
            elsif ( $type eq "fcs" )
            {
                if ( $val =~ m/^0x/ )
                {
                    $fcs = hex($val);
                }
                else
                {
                    $fcs = int($val);
                }
            }
            else
            {
                print "Unknown key: $type\n";
            }
        }

        $self->PadPayload($length, \@payload);
    }
    else
    {
        my $status;

        $length = $args[0];
        
        ($status, @payload) = $self->ParsePacketFields($port, @args);
        
        if ($status != $FM_OK)
        {
            return;
        }

        $self->PadPayload($length, \@payload);
    
        # Extract the arguments for tpSendPacketHelperL2
        my $idx = 0;
        $dmacHigh = ($payload[$idx++] << 16) | ($payload[$idx++] << 8) | $payload[$idx++];
        $dmacLow =  ($payload[$idx++] << 16) | ($payload[$idx++] << 8) | $payload[$idx++];

        $smacHigh = ($payload[$idx++] << 16) | ($payload[$idx++] << 8) | $payload[$idx++];
        $smacLow =  ($payload[$idx++] << 16) | ($payload[$idx++] << 8) | $payload[$idx++];
            
        if ($payload[$idx] == 0x81 && $payload[$idx + 1] == 0x00)
        {
            $idx += 2;
            $vpri = ($payload[$idx] >> 4) & 0x0F;
            $vid = (($payload[$idx++] & 0x0F) << 8) | $payload[$idx++];
        }
        
        $etype = ($payload[$idx++] << 8) | $payload[$idx];

        # Remove the Layer 2 header from the payload buffer.
        splice(@payload, 0, $idx + 1);
    }

    $chip->tpPacketPayloadClear();
    $chip->tpPacketPayloadSet($length, \@payload);

    ###########################################
    # Send the packet
    ###########################################

    if ( $switched )
    {
        # Very easy, send to the switch
        $chip->tpSendPacketHelperL2($sw,
                                    -1,
                                    $dmacHigh,
                                    $dmacLow,
                                    $smacHigh,
                                    $smacLow,
                                    $vid,
                                    $vpri,
                                    $etype,
                                    $length,
                                    $swpri,
                                    $fcs);
    }
    else
    {
        # Well, we have to walk through all the ports to {sw,logicalPort}
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

                $chip->tpSendPacketHelperL2($sw,
                                            $logicalPort,
                                            $dmacHigh,
                                            $dmacLow,
                                            $smacHigh,
                                            $smacLow,
                                            $vid,
                                            $vpri,
                                            $etype,
                                            $length,
                                            $swpri,
                                            $fcs);
            }
            
        }   # for each switch 
    
        # Warn user if some ports were not operated on.
        $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
    }
}    


sub handleReceivePacket
{
    my ($self, $type, $num) = @_;
    my $chip = $self->{CHIP};
    my $testpoint = $self->{TESTPOINT};

    my $src = -1;
    my $vlan = -1;
    my $len = -1;
    my $valid = -1;
    my $frameAction = -1;
    my @payload = ();
    my $i;
    my $n = 0;

    if (!defined($type))
    {
        $type = "quiet";
    }
    elsif(($type ne "quiet") && ($type ne "verbose") && ($type ne "file-pcap"))
    {
        printf("Invalid mode \"$type\": valid modes are \"quiet\", \"verbose\" and \"file-pcap\" \n");
        return;
    }

    # if this is asking to save to a pcap file call tpRxPacketFilePcap
    if ($type eq "file-pcap")
    {
        # for this case the num field is the file name
        $self->tpRxPacketFilePcap($num);  
        return;
    }
    
    if (!defined($num) || ($num eq "all"))
    {
        $num = -1;
    }
    elsif(!($num =~ m/\d+/))
    {
        print("Either indicate the number of packets to receive or \"all\"\n");
        return;
    }

    while(($n < $num) || ($num == -1))
    {
        $chip->tpGrabRecvPktInfo(\$src, \$vlan, \$len, \$frameAction, \$valid);

        if($valid == 0)
        {
            # printf("No more packets left in queue!\n");
            return;
        }
        else
        {
            $n++;

            for($i = 0; $i < $len; $i++)
            {
                my $val = -1;
                $chip->tpGrabRecvPktPayload($i, \$val);
                push(@payload, $val);
            }

            printf("Packet received on port $src, VLAN $vlan (length $len bytes, trap code = 0x%X)\n",$frameAction);

            if ($type eq "verbose")
            {
                for($i = 0; $i < $len; $i++)
                {
                    printf("%02x ", $payload[$i]);

                    if(($i > 0) && (($i % 16) == 15))
                    {
                        printf("\n");
                    }
                }
                printf("\n");
            }
        }

        @payload = ();
        $chip->tpGrabRecvPkt();
    }
}

sub handleI2CRead
{
    my ($self, $bus, $dev, $length) = @_;
    my $chip = $self->{CHIP};
    my $testpoint = $self->{TESTPOINT};
    my $val = -1;
    my $status;

    # Get switch selection.
    my @switchList = $self->tpGetSwitches();
    if (scalar(@switchList) > 1)
    {
        print "Must select a single switch!\n";
        return $FM_FAIL;
    }
    my $sw = $switchList[0];

    if ($self->getSwitchFamily($sw) == $FM_SWITCH_FAMILY_SWAG)
    {
        print "Selected switch may not be a SWAG!\n";
        return $FM_FAIL;
    }

    if (!defined($bus))
    {
        print ("The bus number must be specified!\n");
        return;
    }

    if (!defined($dev))
    {
        print ("The device number must be specified!\n");
        return;
    }

    if (!defined($length))
    {
        $length = 1;
    }
    elsif ( !($length >= 1 || $length <= 4) )
    {
        print ("The length must be between 1 and 4!\n");
        return;
    }

    if ( $dev =~ m/^0x/ )
    {
        $dev = hex($dev);
    }

    $status = $chip->fmPlatformI2cRead ($sw,$bus,$dev,\$val,$length);

    if ($status != $FM_OK)
    {
        printf("ERROR: Bus=$bus, Device=0x%02x, rl=$length\n", $dev);
    }
    else
    {
        printf ("Bus=$bus, Device=0x%02x: Read=0x%X\n", $dev, $val);
    }

}

sub handleI2CWrite
{
    my ($self, $bus, $dev, $data, $length) = @_;
    my $chip = $self->{CHIP};
    my $testpoint = $self->{TESTPOINT};
    my $status;

    # Get switch selection.
    my @switchList = $self->tpGetSwitches();
    if (scalar(@switchList) > 1)
    {
        print "Must select a single switch!\n";
        return $FM_FAIL;
    }
    my $sw = $switchList[0];

    if ($self->getSwitchFamily($sw) == $FM_SWITCH_FAMILY_SWAG)
    {
        print "Selected switch may not be a SWAG!\n";
        return $FM_FAIL;
    }

    if (!defined($bus))
    {
        print ("The bus number must be specified!\n");
        return;
    }

    if (!defined($dev))
    {
        print ("The device number must be specified!\n");
        return;
    }

    if (!defined($data))
    {
        print ("The data to write must be specified!\n");
        return;
    }

    if (!defined($length))
    {
        $length = 1;
    }
    elsif ( !($length >= 1 || $length <= 4) )
    {
        print ("The length must be between 1 and 4!\n");
        return;
    }

    if ( $dev =~ m/^0x/ )
    {
        $dev = hex($dev);
    }

    if ( $data =~ m/^0x/ )
    {
        $data = hex($data);
    }

    $status = $chip->fmPlatformI2cWrite ($sw,$bus,$dev,$data,$length);

    if ($status != $FM_OK)
    {
        printf("ERROR: Bus=$bus, Device=0x%02x, wl=$length\n", $dev);
    }
    else
    {
        printf ("Bus=$bus, Device=0x%02x: Write=0x%X\n", $dev, $data);
    }

}

sub handleI2CWriteRead
{
    my ($self, $bus, $dev, $data, $wl, $rl) = @_;
    my $chip = $self->{CHIP};
    my $testpoint = $self->{TESTPOINT};
    my $val = -1;
    my $status;

    # Get switch selection.
    my @switchList = $self->tpGetSwitches();
    if (scalar(@switchList) > 1)
    {
        print "Must select a single switch!\n";
        return $FM_FAIL;
    }
    my $sw = $switchList[0];

    if ($self->getSwitchFamily($sw) == $FM_SWITCH_FAMILY_SWAG)
    {
        print "Selected switch may not be a SWAG!\n";
        return $FM_FAIL;
    }

    if (!defined($bus))
    {
        print ("The bus number must be specified!\n");
        return;
    }

    if (!defined($dev))
    {
        print ("The device number must be specified!\n");
        return;
    }

    if (!defined($data))
    {
        print ("The data to write must be specified!\n");
        return;
    }

    if (!defined($wl))
    {
        $wl = 1;
    }
    elsif ( !($wl >= 1 || $wl <= 4) )
    {
        print ("The length must be between 1 and 4!\n");
        return;
    }

    if (!defined($rl))
    {
        $rl = 1;
    }
    elsif ( !($rl >= 1 || $rl <= 4) )
    {
        print ("The length must be between 1 and 4!\n");
        return;
    }

    if ( $dev =~ m/^0x/ )
    {
        $dev = hex($dev);
    }

    if ( $data =~ m/^0x/ )
    {
        $data = hex($data);
    }

    $val = int($data);

    $status = $chip->fmPlatformI2cWriteRead ($sw,$bus,$dev,\$val,$wl,$rl);

    if ($status != $FM_OK)
    {
        printf("ERROR: Bus=$bus, Device=0x%02x, wl=$wl, rl=$rl\n", $dev);
    }
    else
    {
        printf ("Bus=$bus, Device=0x%02x: Write=0x%X Read=0x%X\n", $dev, $data,$val);
    }

}

sub handleTestSwitch
{
    my ($self, $sw, $repetitions, $verbose) = @_;
    my $chip = $self->{CHIP};
    my $status;
    my $catValue = Math::BigInt->new(0);
    my $levelValue = Math::BigInt->new(0);
    
    # Default swtich
    if (!defined($sw))
    {
        $sw = 0;
    }

    # Default reps
    if (!defined($repetitions) || $repetitions <= 0)
    {
        $repetitions = 1;
    }
    
    # Default verbosity
    if (!defined($verbose))
    {
        $verbose = "quiet";
    }
    else
    {
        if ($verbose ne "verbose" && $verbose ne "debug")
        {
            printf("Invalid choice for verbosity.\n");
            return;
        }
    }

    # Make sure switch number is valid.
    my @physicalSwitches = $self->tpGetPhysicalSwitches();
    unless(grep {$_ == $sw} @physicalSwitches)
    {
        printf("$sw is not a physical switch in the system.\n");
        return;
    }
    
    my $switchFamily = $self->getSwitchFamily($sw);
    my $selfTestFile;
    
    if ($switchFamily == $FM_SWITCH_FAMILY_SWAG)
    {
        printf("Self-test cannot be performed on a SWAG (switch $sw).\n");
        return;
    }
    elsif ($switchFamily == $FM_SWITCH_FAMILY_FM2000)
    {
        $selfTestFile = "fm2000_debug_selftest.c";
        $catValue = $FM_LOG_CAT_SWITCH_FM2000;
        
        # FIXME: When we have a self-test implemented for Tahoe, delete
        # the following line:
        $verbose = "quiet";
    }
    elsif ($switchFamily == $FM_SWITCH_FAMILY_FM4000)
    {
        $selfTestFile = "fm4000_debug_selftest.c";
        $catValue = $FM_LOG_CAT_SWITCH_FM4000;
    }
    else
    {
        $selfTestFile = "fm6000_debug_selftest.c";
        $catValue = $FM_LOG_CAT_SWITCH_FM6000;
    }
    
    # This is a destructive command. Give user a chance to back out.
    my $answer = "";
    
    while ($answer ne "y" && $answer ne "n")
    {
        printf("\nThis command will cause switch $sw to be shut down and all\n");
        printf("configuration to be lost. Continue (y/n): ");
        $answer = <STDIN>;
        chomp($answer);
    }
    
    printf("\n");
    
    if ($answer eq "n")
    {
        return;
    }
    
    # Set logging appropriately
    if ($verbose eq "verbose")
    {
        $levelValue = $FM_LOG_LEVEL_DEBUG;
        $chip->fmSetLoggingFilter($catValue, 
                                  $levelValue, 
                                  "",
                                  $selfTestFile);
        $chip->fmSetLoggingVerbosity($FM_LOG_VERBOSITY_NONE);
    }
    elsif ($verbose eq "debug")
    {
        $levelValue = $FM_LOG_LEVEL_DEBUG | $FM_LOG_LEVEL_DEBUG2;
        $chip->fmSetLoggingFilter($catValue, 
                                  $levelValue, 
                                  "",
                                  $selfTestFile);
        $chip->fmSetLoggingVerbosity($FM_LOG_VERBOSITY_NONE);
    }
    
    # Do the self-test.
    printf("Testing switch $sw $repetitions time%s...\n",
           $repetitions == 1 ? "" : "s");
    
    my $ctrlState = 1;
    my $repCount = 1;
    
    if ($repetitions > 1)
    {
        $ctrlState = 0;
        
        # Since we are doing multiple repetitions of the test, we need
        # to set the switch state.
        $status = $chip->fmSetSwitchState($sw, $FALSE);
        
        if ($status != $FM_OK)
        {
            printf("Unable to reset switch $sw.\n");
            return;
        }
        
        $status = $chip->fmSetSwitchState($sw, $TRUE);
        
        if ($status != $FM_OK)
        {
            printf("Unable to reset switch $sw.\n");
            return;
        }
    }
    
    while ($repCount <= $repetitions)
    {
        $chip->disableErrors();
        $status = $chip->fmDbgSwitchSelfTest($sw, $ctrlState);
        $chip->enableErrors();
        
        if ($status != $FM_OK)
        {
            last;
        }
        
        $repCount++;
    }

    # Restore logging
    if ($verbose ne "quiet")
    {
        $chip->fmResetLogging();
    }   
    
    # Report results.
    if ($status == $FM_OK)
    {
        printf("PASSED\n");
    }
    elsif ($status == $FM_ERR_INVALID_SWITCH)
    {
        printf("Self-test cannot be performed on switch $sw (member of SWAG?).\n");
    }
    else
    {
        printf("FAILED");
        
        if ($repetitions > 1)
        {
            printf(" on repetition #%d", $repCount);
        }
        
        printf("\n");
    }
    
    my $platformSwitchFamily;
    $chip->fmGetRootPlatformSwitchFamilyW($sw, \$platformSwitchFamily);
    
    printf("Resetting switch and bringing back to UP state...\n");

    if ($platformSwitchFamily != $FM_SWITCH_FAMILY_REMOTE_FM4000)
    {
        $self->handleResetSwitch();
    }
    else
    {
        # Only restart the remote switch
        $status = $chip->fmSetSwitchState($sw, $FALSE);
        
        if ($status != $FM_OK)
        {
            printf("Unable to reset switch $sw.\n");
            return;
        }
        
        $status = $chip->fmSetSwitchState($sw, $TRUE);
        
        if ($status != $FM_OK)
        {
            printf("Unable to reset switch $sw.\n");
            return;
        }
    }
    
}


sub handleTestLED
{
    my ($self, $final) = @_;
    my $port;
    my $bus = 1;
    my $dev = 1;
    my $led;
    my @leds = ();
    my $data;
    my $current;

    # Dump current colors.

    print ("Current LED status:\n");
    for ($port=1;$port<=24;$port++) {
        $data = ( $port > 12 ) ? ($port-12+16) : $port;
        fmPlatformI2cWriteRead (0,1,2,\$data,1,1);
        $leds[$port]=$data & 3;
        if ( ($data & 3) == 0 ) {
            $led = "off";
        }
        elsif ( ($data & 3) == 2 ) {
            $led = "yellow";
        }
        elsif ( ($data & 3) == 1 ) {
            $led = "green";
        }
        elsif ( ($data & 3) == 3 ) {
            $led = "yellow-green";
        }
        printf ("  Port $port: $led\n");
    }

    # Write all possible colors

    for ($led=0;$led<=3;$led++) {
        if ( $led == 0 ) {
            print ("Turning all off.\n");
        }
        elsif ( $led == 1 ) {
            print ("Turning all green.\n");
        }
        elsif ( $led == 2 ) {
            print ("Turning all yellow.\n");
        }
        elsif ( $led == 3 ) {
            print ("Turning all blinking yellow-green.\n");
        }

        for ($port=1;$port<=24;$port++) {
            if ( $port <= 12 ) {
                $data = ($port<<8) + $led;
            }
            else {
                $data = (($port-12+16)<<8) + $led;
            }
            fmPlatformI2cWrite (0,1,2,$data,2);
            fmDelay (0,30000000);
        }
        fmDelay (3,0);
    }

    # Apply final colors

    if (!defined($final))
    {
        $final = "save";
    }

    print ("Applying final color.\n");
    for ($port=1;$port<=24;$port++) {
        # Apply final colors requested
        if ( $final =~ m/yellow/i ) {
            $led = 2;
        }
        elsif ( $final =~ m/green/i ) {
            $led = 1;
        }
        elsif ( $final =~ m/blinking/i ) {
            $led = 3;
        }
        elsif ( $final =~ m/off/i ) {
            $led = 0;
        }
        elsif ( $final =~ m/save/i ) {
            $led = $leds[$port];
        }
        else {
            print ("Invalid final color specified\n");
            return;
        }
        if ( $port <= 12 ) {
            $data = ($port<<8) + $led;
        }
        else {
            $data = (($port-12+16)<<8) + $led;
        }
        fmPlatformI2cWrite (0,1,2,$data,2);
    }

}

sub handleTestMap {
    my ($self) = @_;
    my $chip = $self->{CHIP};

    shift (@_);
    foreach my $mapList (@_)
    {
        printf ("mapList=$mapList\n");
        my @mapList = split(/>/,$mapList);
        my $n = scalar(@mapList);
        if ( $n < 2 )
        {
            printf ("Format for map list is incorrect\n");
            return;
        }
        for (my $i=0; $i < $n-1; $i++ ) {
            # Get the list
            my @portListFrom = $self->validateList($mapList[$i], $self->tpPlatformGetPortRange());
            my @portListTo = $self->validateList($mapList[$i+1], $self->tpPlatformGetPortRange());
            # Print the list
            printf ("FROM:  @portListFrom TO: @portListTo\n");
            # Construct a bit mask
            my $mask = 0;
            my $p;
            my $sw;
            my $physicalPort;
            foreach $p (@portListTo)
            {
                $chip->fmPlatformMapLogicalPortToPhysical(0, $p, \$sw, \$physicalPort);
                $mask |= 1 << $physicalPort;
            }
            # Write this bit mask in the port list
            foreach $p (@portListFrom)
            {
                $chip->fmPlatformMapLogicalPortToPhysical(0,$p, \$sw, \$physicalPort);
                $chip->fmWriteUINT32($sw, $chip->FM_PORT_CFG_2($physicalPort), $mask);
                printf ("Port #%d: FM_PORT_CFG_2[%d]=0x%8.8X\n",$p,$physicalPort,$mask);
            }
        }
    }

}

sub isPortUp
{
    my ($self, $port) = @_;
    my $chip = $self->{CHIP};

    my @info = (0) x 4;
    my ($mode, $state);
    my ($sw, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
    $chip->fmGetPortState($sw, $logPort, \$mode, \$state, \@info);
    if ($state != $FM_PORT_STATE_UP)
    {
        return 0;
    }
    return 1;
}

sub detectLinks
{
    my ($self, $portList, $linksptr, $loopsptr) = @_;
    my $chip = $self->{CHIP};
    my $p;
    my @portList = @$portList;
    my $portCounters;
    my $count;
    my $vlan;
    my $sw_id;
    my $logPort;
    my $nPorts;
    my $firstVlan;
    my $lastVlan;
    my $found;
    my $s;
    my ($pMin,$pMax) = $self->tpPlatformGetFaceplatePortRange();
    my $countSent;
    my $countReceived;
    my $countPort;
    
    #***************************************************************
    # Check what is connected to what by sending a frame on each 
    # port and checking who received it. For this purpose, we create
    # one VLAN per port and only have this port on this VLAN (and
    # reflect is off), this means the packet doesn't get replicated. 
    #***************************************************************
    
    # Delete current VLANs we need for this function        

    $nPorts = $pMax-$pMin;
    $pMin++;
    $firstVlan = $pMin+100;
    $lastVlan = $pMax+100;
    
    # First delete all special VLAN to make sure no ports are in them
    # and re-create them (they then have no member).
    
    $s = "$firstVlan..$lastVlan";
    $chip->disableErrors();
    my @vlanList = $self->validateList($s, $self->tpPlatformGetVLANRange);
    my @valid = $self->_tpIsValidVLAN(@vlanList);
    my %valid = map {$vlanList[$_] => $valid[$_]} (0 .. $#vlanList);
    foreach my $vlanID (@vlanList)
    {
        if ($valid{$vlanID} == $TRUE)
        {
            $self->tpHandleDelVlan($vlanID);
            printf("VLAN $vlanID deleted.\n");
        }
    }
    $chip->enableErrors();
    $self->tpHandleCreateVlan($s);
    printf("VLANs $s created.\n");
    
    # For each port in the port list, create a VLAN on which the port is
    # the only member.
    
    foreach $p (@portList)    
    {
        $vlan = $p + 100;
        printf ("Create VLAN $vlan for port $p.\n");
        $self->handleSetPortConfig ("$p", "pvid", "$vlan");     
        $self->handleSetPortConfig ("$p", "drop_bv", "on");     
        $self->tpHandleAddVlanPort("$vlan", "$p");
        $self->tpHandleAddVlanPort("$vlan", "0");
        $self->tpHandleSetVlanSpt("$vlan", "$p", "forwarding");
        $self->handleResetPortStats("$p");
    }
    

    # Now send a packet on every port one by one    
    my %links = %$linksptr;
    my @loops = @$loopsptr;
    $countSent = 0;
    $countReceived = 0;
    $countPort = 0;
    printf ("Discovering connectivity...\n");
    foreach $p (@portList)    
    {
        $self->handleSendPacket("$p", 100, "FF:FF:FF:FF:FF:FF", "00:01:02:03:04:05");
        $countPort++;        
        
        # Recover switch and port
        ($sw_id, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($p);
        
        # Check for port transmission
        for (my $ms=0; $ms < 100; $ms++) 
        {
            # Read the counter
            $chip->fmGetPortCounters($sw_id, $logPort, \$portCounters);
            # Wait a bit. 
            $chip->fmDelay (0,1000000);
            # If TX counter is on, then it was transmitted and end this loop.
            $count = $portCounters->{cntTx65to127Pkts};
            if ( $count == 1 ) 
            {                
                # This was transmitted
                $countSent++;
                last;
            }
        }
        
        
        # Try to check if on same port first (faster!)
        
        $chip->fmGetPortCounters($sw_id, $logPort, \$portCounters);
        $count = $portCounters->{cntRx65to127Pkts}; 
        if ( $count == 1 ) 
        {
            $countReceived++;
            # Yes, loopback in place
            push(@$loopsptr, $p);
            printf("Loopback on port $p\n");
            # And reset the counter
            $self->handleResetPortStats("$p");
        }
        else 
        {
            # No, try all other ports until found
            foreach my $q (@portList)
            {
                if ( $q != $p ) 
                {
                    # Read the broadcast counter
                    ($sw_id, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($q);
                    $chip->fmGetPortCounters($sw_id, $logPort, \$portCounters);
                    $count = $portCounters->{cntRx65to127Pkts}; 
                    # Check if found.
                    if ( $count == 1 ) 
                    {
                        $countReceived++;
                        $linksptr->{$p} = $q;
                        printf("Link confirmed from port $p to port $q\n");
                        # And reset the counter
                        $self->handleResetPortStats("$q");
                        # And terminate this loop
                        last;
                    }
                }
            }
        }
    }

    # Return error code
    
    if ( $countPort > $countSent )
    {
        # fail on sending, return a negative number
        return $countSent-$countPort;
    }
    elsif ( $countPort > $countReceived ) 
    {
        # fail to receive, return a positive number
        return $countPort - $countReceived;
    }
    else
    {
        # return no error
        return 0;
    }
}

sub checkSnake
{
    my ($self, $portList, $testerList) = @_;
    my $chip = $self->{CHIP};
    my @portList = @$portList;
    my $nFramesErr;
    my $nFramesErrTotal = 0;
    my $p;
    my $interval = 1;
    my $counters0;
    my $counters1;
    my $status;
    my @counters0;

    printf ("Read counters\n");
    
    printf("PORT  Observed Utilization  Errors\n");
    printf("----------------------------------\n");

    foreach $p (@portList)
    {
        $chip->fmGetPortCounters(0, $p,\$counters0[$p]);
    }

    usleep(1.0E6 * $interval);

    foreach $p (@portList)
    {

        $chip->fmGetPortCounters(0, $p,\$counters1);
        # Compute the number of frames received per-bin within the specified
        # interval.
        my %delta = ();
        $delta{'64'}    = $counters1->{'cntRx64Pkts'}
                          - $counters0[$p]->{'cntRx64Pkts'};
        $delta{'65'}    = $counters1->{'cntRx65to127Pkts'}
                          - $counters0[$p]->{'cntRx65to127Pkts'};
        $delta{'128'}   = $counters1->{'cntRx128to255Pkts'}
                          - $counters0[$p]->{'cntRx128to255Pkts'};
        $delta{'256'}   = $counters1->{'cntRx256to511Pkts'}
                          - $counters0[$p]->{'cntRx256to511Pkts'};
        $delta{'512'}   = $counters1->{'cntRx512to1023Pkts'}
                          - $counters0[$p]->{'cntRx512to1023Pkts'};
        $delta{'1024'}  = $counters1->{'cntRx1024to1522Pkts'}
                          - $counters0[$p]->{'cntRx1024to1522Pkts'};
        $delta{'1523'}  = $counters1->{'cntRx1523to2047Pkts'}
                          - $counters0[$p]->{'cntRx1523to2047Pkts'};
        $delta{'2048'}  = $counters1->{'cntRx2048to4095Pkts'}
                          - $counters0[$p]->{'cntRx2048to4095Pkts'};
        $delta{'4096'}  = $counters1->{'cntRx4096to8191Pkts'}
                          - $counters0[$p]->{'cntRx4096to8191Pkts'};
        $delta{'8192'}  = $counters1->{'cntRx8192to10239Pkts'}
                          - $counters0[$p]->{'cntRx8192to10239Pkts'};
        $delta{'10240'} = $counters1->{'cntRx10240toMaxPkts'}
                          - $counters0[$p]->{'cntRx10240toMaxPkts'};

        $nFramesErr = $counters1->{cntRxFramingErrorPkts}
                    + $counters1->{cntRxFCSErrors}
                    + $counters1->{cntOverrunPkts} 
                    + $counters1->{cntUnderrunPkts};        

        $nFramesErrTotal += $nFramesErr;
        # Compute the total number of frames sent within the specified
        # interval.
        my $elapsedUsec = $counters1->{'timestamp'} - $counters0[$p]->{'timestamp'};
        my $numFrames = Math::BigInt->new(0);
        map {$numFrames += $delta{$_}} keys(%delta);
        # Compute the total number of bytes sent within the specified interval.
        my $numBytes = $counters1->{'cntRxGoodOctets'} - $counters0[$p]->{'cntRxGoodOctets'};
        my %void = (type => "fm_uint32", value => 0);
        $status = $chip->fmGetPortAttribute(0, $p,
                                            $FM_PORT_IFG, \%void);
        my $bandwidth = $numFrames->copy()->bmul($void{'value'} + 8);

        %void = (type => "fm_uint32", value => 0);
        $chip->disableErrors();
        $chip->fmGetPortAttribute(0, $p, $FM_PORT_SPEED, \%void);
        $chip->enableErrors();
        my $speed = ($status != $FM_OK) ? 100000000 : $void{value}*10000;

        $bandwidth += $numBytes;

        # Convert from byte to bit.
        $bandwidth->bmul(8);
        $bandwidth->bmul(1000000); #Adjust for Usec
        $bandwidth->bdiv($elapsedUsec);
        $bandwidth->bdiv($speed);
        $bandwidth->accuracy(4);
        printf("%2d    %10s%%           %4d \n", $p, $bandwidth->bstr(), $nFramesErr);
    }

    printf("\n");
    my $disp;
    $disp = $nFramesErrTotal == 0 ? "PASS" : "FAIL";
    printf("Packet errors           = %-6d    *** $disp ***\n",$nFramesErrTotal);
}



sub buildL2snake
{
    my ($self, $portList, $testerList, $linksptr, $loopsptr, $frameLength, $numberOfFrames) = @_;
    my $chip = $self->{CHIP};
    my $p;
    my $srcPort;
    my $destPort;
    my $sw;
    my $physicalPort;
    my $physicalSrcPort;
    my $physicalDestPort;
    my $i;
    my $logPort;
    my $mask;
    my ($pMin,$pMax) = $self->tpPlatformGetFaceplatePortRange();
    my $vlan;
    my $len;
    my $valid;
    my $nPorts;
    my $firstVlan;
    my $lastVlan;
    my $s;
    my $nextPort;
    my @testerList = @$testerList;
    my $cpuIsSource = $FALSE;
    my $firstSnakePort = -1;
    my $lastSnakePort = -1;
    my $firstSnakeLinkPort = -1;
    my $lastSnakeLinkPort = -1;
    my $firstSnakeLoopPort = -1;
    my $lastSnakeLoopPort = -1;
    my $t;
    my $nbOfLoopedPorts = 0;
    my $numberOfLinks = 0;
    my $vlan1;
    my $vlan2;

    $nbOfLoopedPorts = scalar(@$loopsptr);
    $numberOfLinks = scalar(keys(%$linksptr));

    if ((scalar(@testerList) == 1) &&
        ($testerList[0] == 0))
    {
        if ( !defined($frameLength) || !defined($numberOfFrames) )
        {
            printf("Must define a frame length and a number of frame to be sent\n");
            return;
        }
        if ($nbOfLoopedPorts != 0 && $numberOfLinks != 0)
        {
            printf("CPU port being used as traffic generator need to be use with all ports externally conected together or all ports loopbacked\n");
            return;
        }
        $cpuIsSource = $TRUE;
    }

    $nPorts = $pMax-$pMin;

    $pMin++;
    $firstVlan = $pMin+100;
    $lastVlan = $pMax+100;
    
    # First delete all special VLAN to make sure no ports are in them
    # and re-create them (they then have no member).
    
    $s = "$firstVlan..$lastVlan";
    $chip->disableErrors();
    my @vlanList = $self->validateList($s, $self->tpPlatformGetVLANRange);
    my @valid = $self->_tpIsValidVLAN(@vlanList);
    my %valid = map {$vlanList[$_] => $valid[$_]} (0 .. $#vlanList);
    foreach my $vlanID (@vlanList)
    {
        if ($valid{$vlanID} == $TRUE)
        {
            $self->tpHandleDelVlan($vlanID);
            printf("VLAN $vlanID deleted.\n");
        }
    }
    $chip->enableErrors();
    $self->tpHandleCreateVlan($s);
    printf("VLANs $s created.\n");
    # Set the default VLAN for each port
    
    my @externalPorts = ();
    for my $switchNum ($self->tpGetSwitches())
    {
        my $infoW = ($self->tpGetSwitchInfo())[$switchNum];
        push(@externalPorts, $self->tpPlatformGetSwitchExternalPortList($switchNum, $TRUE));
    }

    foreach $p (@externalPorts)
    {
        $vlan = $p+100;
        $self->handleSetPortConfig ("$p", "pvid", "$vlan");  
        $self->tpHandleAddVlanPort($vlan, $p);
        $self->tpHandleSetVlanSpt("$vlan", "$p", "forwarding");
    }

    # Get the port list
   
    
    # Now create a snake from the list
    printf("Snake sequence:\n");
    my $testerPort = $testerList->[0];
    my $lastLinkDest = $testerPort;
    $srcPort = $testerPort;    
    $nextPort = undef;

    for (;;)
    {
        my $desc = "";
        # Find a destination which is not a loopback

        if (scalar(keys(%$linksptr)) > 0)
        {
            my @tmp = sort { $a <=> $b } keys(%$linksptr);
            $destPort = shift(@tmp); 
            $nextPort = $linksptr->{$destPort};
            delete($linksptr->{$destPort});
            delete($linksptr->{$nextPort});

            if ($firstSnakeLinkPort == -1)
            {
                $firstSnakeLinkPort = $destPort;
            }
            $lastSnakeLinkPort = $nextPort;
            $lastLinkDest = $nextPort;

            # Add the destination port into the source VLAN
            $vlan1 = $srcPort + 100;
            $self->tpHandleAddVlanPort($vlan1, $destPort);
            $vlan2 = $destPort + 100;
            $self->tpHandleAddVlanPort($vlan2, $srcPort);
            printf("  Internal %2d <-> %2d VLANs $vlan1 $vlan2\n", $srcPort, $destPort);
            printf("  Cable    %2d <-> %2d\n",$destPort, $nextPort);
        }
        elsif(scalar(@$loopsptr) > 0)
        {
            $destPort = shift(@$loopsptr); 
            $nextPort = $destPort;
            $lastSnakeLoopPort = $nextPort;
            $vlan = $srcPort + 100;
            if ($firstSnakeLoopPort == -1)
            {
                $firstSnakeLoopPort = $nextPort;
            }
            $self->tpHandleAddVlanPort($vlan, $destPort);
            printf("  Internal %2d  -> %2d VLAN $vlan\n", $srcPort, $destPort);
            printf("  Loopback %2d  -> %2d\n", $destPort, $nextPort);
        }
        else
        {
            #printf("No ports left\n");
            last;
        }
        $srcPort = $nextPort;
    }    

    if ($cpuIsSource == $FALSE)
    {
        my $endTesterLink = $testerPort;
        if (scalar(@$testerList) == 2)
        {
            $endTesterLink = $testerList->[1];
            $vlan = $srcPort + 100;
            $self->tpHandleAddVlanPort($vlan, $endTesterLink);
            printf("  Internal %2d  -> %2d VLAN $vlan\n",
                   $srcPort, $endTesterLink);
            $srcPort = $endTesterLink;
        }
        # Looping back onto the snake
            
        $vlan = $srcPort + 100;
        $self->tpHandleAddVlanPort($vlan, $lastLinkDest);
        printf("  Internal %2d  -> %2d VLAN $vlan\n",
               $srcPort, $lastLinkDest);
    }
    else
    {
        # Add the destination port into the source VLAN
        if ($firstSnakeLinkPort != -1)
        {
            $vlan1 = $firstSnakeLinkPort + 100;
            $self->tpHandleAddVlanPort($vlan1, $lastSnakeLinkPort);
            $vlan2 = $lastSnakeLinkPort + 100;
            $self->tpHandleAddVlanPort($vlan2, $firstSnakeLinkPort);
            $firstSnakePort = $firstSnakeLinkPort;
            $lastSnakePort = $lastSnakeLinkPort;
           
        }

        if ($firstSnakeLoopPort != -1)
        {
            $firstSnakePort = $firstSnakeLoopPort;
            $vlan1 = $lastSnakeLoopPort + 100;
            $self->tpHandleAddVlanPort($vlan1, $firstSnakeLoopPort);
        }
    }

    if ($cpuIsSource == $TRUE)
    {
        $self->tpr("set port config all max_frame_size 10000");
        $self->tpr("set port config all learning off");

        # Inject the frames
        if ($firstSnakePort != -1)
        {
            printf("Send %d frames of length = %d to port %d\n", $numberOfFrames, $frameLength, $firstSnakePort);    
            for ($t=0;$t<$numberOfFrames;$t++)
            {
                $self->tpr("packet send directed $firstSnakePort $frameLength b");
            }
        }

        if ($lastSnakePort != -1)
        {
            printf("Send %d frames of length = %d to port %d\n", $numberOfFrames, $frameLength, $lastSnakePort);    
            for ($t=0;$t<$numberOfFrames;$t++)
            {
                $self->tpr("packet send directed $lastSnakePort $frameLength b");
            }
        }

        $self->checkSnake($portList, $testerList);
    }
}

sub buildL3snake
{
    my ($self, $portList, $testerList, $linksptr, $loopsptr) = @_;
    my $chip = $self->{CHIP};
    my $p;
    my $srcPort;
    my $destPort;
    my $sw;
    my $physicalPort;
    my $physicalSrcPort;
    my $physicalDestPort;
    my $i;
    my $logPort;
    my $mask;
    my ($pMin,$pMax) = $self->tpPlatformGetFaceplatePortRange();
    my $vlan;
    my $len;
    my $valid;
    my $nPorts;
    my $firstVlan;
    my $lastVlan;
    my $s;
    my $nextPort;

    $nPorts = $pMax-$pMin;
    $pMin++;
    $firstVlan = $pMin+100;
    $lastVlan = $pMax+100;
    
    # First delete all special VLAN to make sure no ports are in them
    # and re-create them (they then have no member).
    
    $s = "$firstVlan..$lastVlan";
    $chip->disableErrors();
    my @vlanList = $self->validateList($s, $self->tpPlatformGetVLANRange);
    my @valid = $self->_tpIsValidVLAN(@vlanList);
    my %valid = map {$vlanList[$_] => $valid[$_]} (0 .. $#vlanList);
    foreach my $vlanID (@vlanList)
    {
        if ($valid{$vlanID} == $TRUE)
        {
            $self->tpHandleDelVlan($vlanID);
            printf("VLAN $vlanID deleted.\n");
        }
    }
    $chip->enableErrors();
    $self->tpHandleCreateVlan($s);
    printf("VLANs $s created.\n");
    # Set the default VLAN for each port
    
    $self->handleRegWriteIndex("FM_FFU_MAP_MAC", "0x4600000a00010203", "0");
    $self->handleSetSwitchConfig("mstp", "on");

    printf("Configuring ports $pMin..$pMax for L3\n");
    my @externalPorts = ();
    for my $switchNum ($self->tpGetSwitches())
    {
        my $infoW = ($self->tpGetSwitchInfo())[$switchNum];
        push(@externalPorts, $self->tpPlatformGetSwitchExternalPortList($switchNum, $TRUE));
    }

    foreach my $p (@externalPorts)
    {
        $vlan = $p+100;
        $self->handleSetPortConfig ("$p", "pvid", "$vlan");     
        $self->handleSetPortConfig ("$p", "drop_bv", "on");     
        $self->tpHandleAddVlanPort("$vlan", "$p");
        $self->tpHandleSetVlanSpt("$vlan", "$p", "forwarding");
        $self->handleRegWriteIndex("FM_FFU_MAP_VLAN", "0x1001",  "$vlan");
        $self->handleRegWritePort("$p","FM_FFU_MAP_SRC","0x11");
        $self->handleRegWritePort("$p","FM_SRC_MAC_LO","0x00010203");
        $self->handleRegWritePort("$p","FM_SRC_MAC_HI","0x000a");
        $self->handleRegWritePort("$p","FM_PARSE_CFG","0x30");
    }

    for(my $i=0;$i<32;$i++)
    {
        $self->handleRegWriteIndex("FM_FFU_SLICE_TCAM",
                                   "0x0000000000000000000","$i","0");
        $self->handleRegWriteIndex("FM_FFU_SLICE_VALID","0x20000","$i");
        $self->handleRegWriteIndex("FM_FFU_SLICE_CASE_CFG",
                                   "0xc100a000","$i","0");
        $self->handleRegWriteIndex("FM_FFU_SLICE_CASE","0x0","$i");
    }
    

    # Get the port list
   
    
    # Now create a snake from the list
    printf("Snake sequence:\n");
    my $testerPort = $testerList->[0];
    my $lastLinkDest = $testerPort;
    $srcPort = $testerPort;    
    $nextPort = undef;
    my $j = 0;
    for (;;)
    {
        my $desc = "";
        # Find a destination which is not a loopback

        if (scalar(keys(%$linksptr)) > 0)
        {
            my @tmp = sort { $a <=> $b } keys(%$linksptr);
            $destPort = shift(@tmp); 
            $nextPort = $linksptr->{$destPort};
            delete($linksptr->{$destPort});
            delete($linksptr->{$nextPort});
            $lastLinkDest = $nextPort;

            # Add the destination port into the source VLAN
            my $vlan1 = $srcPort + 100;
            my $vlan2 = $destPort + 100;

            my $arpIndex = $j*24 + $srcPort;
            my $rule = $j*24 + $srcPort;
            my $slice = 1+int($rule / 512);
            my $index = $rule % 512;

            $self->handleRegWriteIndex("FM_ARP_TABLE", 
                sprintf("0x%x000a00010203",$vlan2),  "$arpIndex");
            $self->handleRegWriteIndex("FM_FFU_SLICE_TCAM",
                sprintf("0x100ffffffffc0%02x%04x",$vlan1,$j+1),
                "$slice", "$index");
            $self->handleRegWriteIndex("FM_FFU_SLICE_SRAM",
                sprintf("0x%x40%04x",$srcPort,(4<<12) + $arpIndex),
                "$slice", "$index");

            $arpIndex = $j*24 + $destPort;
            $rule = $j*24 + $destPort;
            $slice = int($rule / 512);
            $index = $rule % 512;

            $self->handleRegWriteIndex("FM_ARP_TABLE", 
                sprintf("0x%x000a00010203",$vlan1),  "$arpIndex");
            $self->handleRegWriteIndex("FM_FFU_SLICE_TCAM",
                sprintf("0x100ffffffffc0%02x%04x",$vlan2,$j+1),
                "$slice", "$index");
            $self->handleRegWriteIndex("FM_FFU_SLICE_SRAM",
                sprintf("0x%x40%04x",$destPort,(4<<12) + $arpIndex),
                "$slice", "$index");

            printf("  Internal %2d <-> %2d VLANs $vlan1 $vlan2\n", $srcPort, $destPort);
            printf("  Cable    %2d <-> %2d\n",$destPort, $nextPort);

        }
        elsif(scalar(@$loopsptr) > 0)
        {
            $destPort = shift(@$loopsptr); 
            $nextPort = $destPort;

            $vlan = $srcPort + 100;
            my $vlan1 = $srcPort + 100;
            my $vlan2 = $destPort + 100;

            my $arpIndex = $j*24 + $srcPort;
            my $rule = $j*24 + $srcPort;
            my $slice = 1+int($rule / 512);
            my $index = $rule % 512;

            $self->handleRegWriteIndex("FM_ARP_TABLE", 
                sprintf("0x%x000a00010203",$vlan2),  "$arpIndex");
            $self->handleRegWriteIndex("FM_FFU_SLICE_TCAM",
                sprintf("0x100ffffffffc0%02x%04x",$vlan1,$j+1),
                "$slice", "$index");
            $self->handleRegWriteIndex("FM_FFU_SLICE_SRAM",
                sprintf("0x%x40%04x",$srcPort,(4<<12) + $arpIndex),
                "$slice", "$index");


            printf("  Internal %2d  -> %2d VLAN $vlan\n", $srcPort, $destPort);
            printf("  Loopback %2d  -> %2d\n", $destPort, $nextPort);
        }
        else
        {
            #printf("No ports left\n");
            last;
        }


        $srcPort = $nextPort;
    }    

    my $endTesterLink = $testerPort;
    if (scalar(@$testerList) == 2)
    {
        $endTesterLink = $testerList->[1];
        $vlan = $srcPort + 100;
        my $vlan1 = $vlan;
        my $vlan2 = $endTesterLink + 100;

        my $arpIndex = $j*24 + $srcPort;
        my $rule = $j*24 + $srcPort;
        my $slice = 1+int($rule / 512);
        my $index = $rule % 512;

        $self->handleRegWriteIndex("FM_ARP_TABLE", 
            sprintf("0x%x000a00010203",$vlan2),  "$arpIndex");
        $self->handleRegWriteIndex("FM_FFU_SLICE_TCAM",
            sprintf("0x100ffffffffc0%02x%04x",$vlan1,$j+1),
            "$slice", "$index");
        $self->handleRegWriteIndex("FM_FFU_SLICE_SRAM",
            sprintf("0x%x40%04x",$srcPort,(4<<12) + $arpIndex),
            "$slice", "$index");
        printf("  Internal %2d  -> %2d VLAN $vlan\n",
               $srcPort, $endTesterLink);
        $srcPort = $endTesterLink;
    }


    # Looping back onto the snake
        
    $vlan = $srcPort + 100;
    my $vlan1 = $vlan;
    my $vlan2 = $lastLinkDest + 100;

    my $arpIndex = $j*24 + $srcPort;
    my $rule = $j*24 + $srcPort;
    my $slice = 1+int($rule / 512);
    my $index = $rule % 512;

    $self->handleRegWriteIndex("FM_ARP_TABLE", 
        sprintf("0x%x000a00010203",$vlan2),  "$arpIndex");
    $self->handleRegWriteIndex("FM_FFU_SLICE_TCAM",
        sprintf("0x100ffffffffc0%02x%04x",$vlan1,$j+1),
        "$slice", "$index");
    $self->handleRegWriteIndex("FM_FFU_SLICE_SRAM",
        sprintf("0x%x40%04x",$srcPort,(4<<12) + $arpIndex),
        "$slice", "$index");
    printf("  Internal %2d  -> %2d VLAN $vlan\n",
           $srcPort, $lastLinkDest);
}

# Snake test, works with cables and loopbacks.  Can also have an optional 
# tester port for dual snake testing.
sub handleTestSnake {
    my ($self,$snaketype,$testerList,$portList, $frameLength, $numberOfFrames) = @_;
    my $p;
    my $chip = $self->{CHIP};
    my @testerList;
    my @portList;
    my $detectError;

    my %links;
    my @loops;

    #$self->handleResetSwitch();
    #$self->tpHandleSetVlanSpt(1, "all", "forwarding");
    #$self->handleSetPort("all", "up");

#    printf("frameSize = $frameLength numberOfFrames = $numberOfFrames\n", $frameLength, $numberOfFrames);

    # Snake Type Must be L2 or L3
    if (($snaketype ne "check") && ($snaketype ne "L2") 
        && ($snaketype ne "L3"))
    {
        printf ("Snake type must be check, L2, or L3.\n");
        return;
    }

    # Tester port and port list must be used 

    @testerList = $self->validateList($testerList,
                                      $self->tpPlatformGetFaceplatePortRange());
    if (( scalar(@testerList) != 1 ) && ( scalar(@testerList) != 2 ))
    {
        printf ("Tester List can only contain one or two ports\n");
        return;
    }
    if ( !defined($testerList) )
    {
        printf ("One or Two TesterPorts required\n");
        return;
    }
    if ( !defined($portList) )
    {
        printf ("PortList is required\n");
        return;
    }
    
    @portList = $self->validateList($portList, $self->tpPlatformGetFaceplatePortRange());

    if ($snaketype eq "check")
    {
        $self->checkSnake(\@portList, \@testerList);
    }
    else
    {
        $detectError = $self->detectLinks(\@portList, \%links, \@loops);
        # First define the number of port
        
        printf("%d links detected, %d loopbacks detected\n", scalar(@loops),
                                                             scalar(keys(%links)));
        my %fwdLinks = %links;
        my @fwdLoops = @loops;
        if ((scalar(@testerList) == 1) &&
            ($testerList[0] != 0) &&
            (scalar(keys(%fwdLinks)) > 0) && 
            (scalar(@fwdLoops) == 0))
        {
            printf("FAIL: If cables are used, at least one loopback port is necessary\n");
            return;
        }
    
        if ($snaketype eq "L2")
        {
            $self->buildL2snake(\@portList, \@testerList, \%fwdLinks, \@fwdLoops, $frameLength, $numberOfFrames);
        } else {
            $self->buildL3snake(\@portList, \@testerList, \%fwdLinks, \@fwdLoops, $frameLength, $numberOfFrames);
        }
        
        # Print a warning
        
        if ( $detectError > 0 )
        {
            printf("WARNING: missing $detectError links in the snake\n");
        }
        elsif ( $detectError < 0 )
        {
            $detectError = -$detectError;
            printf("WARNING: failed to transmit on $detectError links\n");
        }
    }
}



sub handleTestClean {
    my ($self) = @_;
    my $chip = $self->{CHIP};
    my $p;
    my $sw;
    my $physicalPort;
    my ($pMin,$pMax) = $self->tpPlatformGetPortRange();
    my $firstVlan;
    my $lastVlan;
    my $s;

    # First define the number of port

    $pMin++;
    $firstVlan = $pMin+100;
    $lastVlan = $pMax+100;

    # Felete all special VLAN.

    $s = "$firstVlan..$lastVlan";
    tpHandleDelVlan ($self,$s);
}

our $NUM_BURNIN_PKTS = 16;
our @tpIfgDefault = ();

sub handleTestBurninStart {
    my ($self, $NPKTS) = @_;
    my $chip = $self->{CHIP};
    my $p;
    my $sw;
    my $vlan;
    my $valid;
    my $firstVlan;
    my $lastVlan;
    my $s;

    # Defined the number of packets to transmit

    if(!defined($NPKTS))
    {
        $NPKTS = $NUM_BURNIN_PKTS;
    }
    $self->{BURNIN_NPKTS} = $NPKTS;
    
    # Determine the ports and VLAN to use
    
    $vlan = 101;
    
    # First delete this vlan and re-create it, turn on reflect
    
    $self->tpHandleDelVlan($vlan);
    $self->tpHandleCreateVlan($vlan);
    $self->tpHandleSetVlanConfig("reflect", "$vlan", "on");
    printf("VLANs $vlan created.\n");

    for my $switchNum ($self->tpGetSwitches())
    {
        my $infoW = ($self->tpGetSwitchInfo())[$switchNum];
        my @externalPorts =
            $self->tpPlatformGetSwitchExternalPortList($switchNum, $TRUE);

        foreach my $p (@externalPorts)
        {
            my %void = (type => "fm_bool", value  => $FM_DISABLED);
            my ($sw, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($p);
            # For the FM4000 family, we have to turn off loopback suppress...

            if ( $infoW->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000 )
            {
                printf ("FM2000 family\n");
            }
            else
            {
                $chip->fmSetPortAttribute($sw, 
                                          $logPort, 
                                          $port_attr_map{'loopback_suppress'},
                                           \%void);
            }

            # Set the IFG to 12. It will be set back to the TestPoint default in 
            # handleTestBurninStop. First cache the current default.
            %void = (type => "fm_uint32", value  => 0);
            $chip->fmGetPortAttribute($sw, 
                                      $logPort,
                                      $port_attr_map{'ifg'}, 
                                      \%void);

            $tpIfgDefault[$p] = $void{value};  

            $self->handleSetPortConfig("$p", "ifg", "12");

            $self->handleSetPortConfig ("$p", "pvid", "$vlan");     
            $self->tpHandleAddVlanPort("$vlan", "$p");
            $self->tpHandleSetVlanSpt("$vlan", "$p", "forwarding");
            $self->handleSetPortConfig ("$p", "learning", "off");     
            $self->handleResetPortStats("$p");
        }
        
        # Now mask out the destination to self 
        my %externalPorts = (); 
        if ($infoW->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG) 
        {
            my ($physSwitch, $physPort) = (-1, -1);
            #grouping all SWAG ports according to the underlying physical
            # switches.
            foreach my $swagExternalPort (@externalPorts)
            {
                $chip->fmGetSwitchAndPortForSWAGPort($switchNum,
                                                     $swagExternalPort,
                                                     \$physSwitch,
                                                     \$physPort); 
                if (not exists $externalPorts{$physSwitch})
                {
                    $externalPorts{$physSwitch} = [($physPort)];
                }    
                else
                {
                    push @{$externalPorts{$physSwitch}}, $physPort;
                }
            }                                        
        }
        else
        {
            $externalPorts{$switchNum} = [@externalPorts];
        }

        $self->settingPortMask($switchNum, \%externalPorts);
    }

    # Add port 0 as well to this VLAN 
    $self->tpHandleAddVlanPort("$vlan", "0");
    
    # Save starting point
    $self->{BURNIN_TIME_START} = ftime();

    # Send traffic on all external ports
    for my $switchNum ($self->tpGetSwitches())
    {
        my @externalPorts =
            $self->tpPlatformGetSwitchExternalPortList($switchNum, $TRUE);

        foreach my $p (@externalPorts)
        {
            my @info = (0) x 4;
            my ($mode, $state);
            $chip->fmGetPortState($switchNum, $p, \$mode, \$state, \@info);

            if ($state != $FM_PORT_STATE_UP)
            {
                next;
            }

            printf ("Sending 16 broadcast frame to the port $p.\n");
            for (my $j = 0; $j < $NPKTS; $j++)
            {
                $self->handleSendPacket($p, 64, 
                        "FF:FF:FF:FF:FF:FF",
                        "00:00:00:00:00:01",
                        "FF","FF","FF","FF","FF","FF","FF","FF",   
                        "FF","FF","FF","FF","FF","FF","FF","FF",   
                        "FF","FF","FF","FF","FF","FF","FF","FF",   
                        "FF","FF","FF","FF","FF","FF","FF","FF",   
                        "FF","FF","FF","FF","FF","FF","FF","FF",   
                        "FF","FF","FF","FF","FF","FF","FF","FF"   
                        );
            }
        }
    }
}

sub handleTestBurninStart2 {
    my ($self, $NPKTS, $ALT) = @_;
    my $chip = $self->{CHIP};
    my $p;
    my $sw;
    my ($pMin,$pMax) = $self->tpPlatformGetFaceplatePortRange();
    my $vlan;
    my $valid;
    my $nPorts;
    my $firstVlan;
    my $lastVlan;
    my $s;

    # Defined the number of packets to transmit

    if(!defined($NPKTS))
    {
        $NPKTS = $NUM_BURNIN_PKTS;
    }
    $self->{BURNIN_NPKTS} = $NPKTS;
    
    # If ALT not defined, then force ALT by default
    if(!defined($ALT))
    {
        $ALT = 1;
    }
    
    # Determine the ports and VLAN to use
    
    $nPorts = $pMax-$pMin;
    $pMin++;
    $firstVlan = $pMin+100;
    $lastVlan = $pMax+100;
    
    # First delete all special VLAN to make sure no ports are in them
    # and re-create them (they then have no member).
    
    $s = "$firstVlan..$lastVlan";
    $chip->disableErrors();
    my @vlanList = $self->validateList($s, $self->tpPlatformGetVLANRange);
    my @valid = $self->_tpIsValidVLAN(@vlanList);
    my %valid = map {$vlanList[$_] => $valid[$_]} (0 .. $#vlanList);
    foreach my $vlanID (@vlanList)
    {
        if ($valid{$vlanID} == $TRUE)
        {
           $self->tpHandleDelVlan($vlanID);
            printf("VLAN $vlanID deleted.\n");
        }
    }
    $chip->enableErrors();
    $self->tpHandleCreateVlan($s);
    printf("VLANs $s created.\n");

    # Now put each port into a vlan on its own
    
    for($p = $pMin; $p <= $pMax; $p++)
    {
        my $vlan = 100 + $p;
        $self->handleSetPortConfig ("$p", "pvid", "$vlan");     
        $self->tpHandleAddVlanPort("$vlan", "$p");
        $self->tpHandleSetVlanSpt("$vlan", "$p", "forwarding");
        $self->tpHandleSetVlanConfig("reflect", "$vlan", "on");
        $self->handleSetPortConfig ("$p", "mask", "$pMin..$pMax");     
        $self->handleSetPortConfig ("$p", "learning", "off");     
        $self->handleResetPortStats("$p");
    }

    $self->{BURNIN_TIME_START} = ftime();

    # Send traffic on all ports
    for (my $j = 0; $j < $NPKTS; $j++)
    {
        if ( ($j & 1) || !$ALT ) 
        {
            printf ("Sending one FFs frame to all ports.\n");
            $self->handleSendPacket("$pMin..$pMax", 64, 
                    "FF:FF:FF:FF:FF:FF",
                    "FF:FF:FF:FF:FF:FF",
                    "FF","FF","FF","FF","FF","FF","FF","FF",   
                    "FF","FF","FF","FF","FF","FF","FF","FF",   
                    "FF","FF","FF","FF","FF","FF","FF","FF",   
                    "FF","FF","FF","FF","FF","FF","FF","FF",   
                    "FF","FF","FF","FF","FF","FF","FF","FF",   
                    "FF","FF","FF","FF","FF","FF","FF","FF"   
                    );
        }
        else
        {
            printf ("Sending one 00s frame to all ports.\n");
            $self->handleSendPacket("$pMin..$pMax", 64, 
                    "00:00:00:00:00:00",
                    "00:00:00:00:00:00",
                    "00","00","00","00","00","00","00","00",   
                    "00","00","00","00","00","00","00","00",   
                    "00","00","00","00","00","00","00","00",   
                    "00","00","00","00","00","00","00","00",   
                    "00","00","00","00","00","00","00","00",   
                    "00","00","00","00","00","00","00","00"   
                    );
        }
    }
}

sub verifyBurninPackets()
{
    my ($self, $NPKTS) = @_;
    my $chip = $self->{CHIP};

    if(!defined($NPKTS) || ($NPKTS == 0))
    {
        $NPKTS = $NUM_BURNIN_PKTS;
    }

    my $trigValue = (1 << 16) | (1 << 4);

    $chip->fmWriteUINT32(0, $chip->FM_TRIGGER_RX(15), 0xffffffff);
    $chip->fmWriteUINT32(0, $chip->FM_TRIGGER_CFG(15), $trigValue);

    sleep(1);

    my $src = -1;
    my $vlan = -1;
    my $len = -1;
    my $valid = -1;
    my @count = ();

    for (my $i = 1; $i <= 24; $i++)
    {
        $count[$i] = 0;
    }

    while(1)
    {
        $chip->fmGrabRecvPktInfo(\$src, \$vlan, \$len, \$valid);

        if($valid == 0)
        {
            last;
        }
        else
        {
            $count[$src]++;

            #printf("Packet received on port $src, VLAN $vlan (length $len bytes)\n");
        }

        $chip->fmGrabRecvPkt();
    }

    for (my $port = 1; $port <= 24; $port++)
    {
        if($count[$port] != $NPKTS)
        {
            printf("FAIL: expected to receive $NPKTS " .
                   "packets on port %d, got %d packets\n",
                   $port, $count[$port]);
        }
        else
        {
            #printf("PASS: received %d packets on port %d\n", $count[$port], $port);
        }
    }

    # disable trigger
    $chip->fmWriteUINT32(0, $chip->FM_TRIGGER_CFG(15), 0x3);
}

sub handleTestBurninCheck()
{
    my ($self, $NPKTS) = @_;
    my $chip = $self->{CHIP};
    my $port;
    my $stopTime;
    my $portCounters;
    my $count;
    my $error;
    my ($pMin,$pMax) = $self->tpPlatformGetFaceplatePortRange();
    my $TooSlowCount = 0;
    my $TooFastCount = 0;
    my $ErrorsEncounteredCount = 0;

    if(!defined($NPKTS))
    {
        $NPKTS = $self->{BURNIN_NPKTS};
    }
    else
    {
        $self->{BURNIN_NPKTS} = $NPKTS;
    }

    # Determine the ports and VLAN to use
    $pMin++;
    
    # Take current time. 
    $stopTime = ftime();
    
    printf("SWITCH  PORT  SPEED    #PKTS    Errors  Observed Rate      Status\n");
    printf("-------------------------------------------------------------\n");

    for my $switchNum ($self->tpGetSwitches())
    {
        my @externalPorts =
        $self->tpPlatformGetSwitchExternalPortList($switchNum, $TRUE);
       
        foreach my $port (@externalPorts)
        {
            my ($sw, $logicalPort) =
                              $self->tpPlatformMapGlobalToLogicalPort($port);
                              
            # Retrieve counter and errors                        

            $chip->disableErrors();
            my $status =
                $chip->fmGetPortCounters($switchNum, $logicalPort, \$portCounters);
            $chip->enableErrors();
            if ($status != $FM_OK)
            {
                print("Broadcast counter cannot be retreived for port $port!\n");
                $count = 0;
                $error = 0;
            }
            else
            {
                $count = $portCounters->{cntRxBcstPkts}+$portCounters->{cntRxUcstPkts};
                $error = $portCounters->{cntRxFCSErrors};
            }
            
            # Retrieve speed
            
            my %void = (type => "fm_uint32", value => 0);
            $chip->disableErrors();
            $chip->fmGetPortAttribute($switchNum, $logicalPort, $FM_PORT_SPEED, \%void);
            $chip->enableErrors();
            my $speed;
            my $minRate;
            my $maxRate;
            if ($status != $FM_OK)
            {
                print("Speed cannot be shown for port $port!\n");
                $speed = 1;
            }
            else
            {
                $speed = $void{value};
            }
            my $speeds;
            if ($void{value} == 10000)
            {
                $speeds = "10G";
                $minRate = 14.5;
                $maxRate = 15.3;
            }
            elsif ($void{value} == 2500)
            {
                $speeds = "2.5G";
                $minRate = 14.5/4;
                $maxRate = 15.3/4;
            }
            elsif ($void{value} == 1000)
            {
                $speeds = "1G";
                $minRate = 1.45;
                $maxRate = 1.53;
            }
            elsif ($void{value} == 100)
            {
                $speeds = "100M";
                $minRate = 0.145;
                $maxRate = 0.153;
            }
            elsif ($void{value} == 10)
            {
                $speeds = "10M";
                $minRate = 0.0145;
                $maxRate = 0.0153;
            }

            # Compute rate        
            my $delta = Math::BigFloat->new($count);
            my $time = Math::BigFloat->new(ftime() - $self->{BURNIN_TIME_START});
            my $rate = $delta->bdiv($time)->bstr() / 1e6;
            
            # Display switch, port, speed and counter
            printf(" %2d  %2d   %3s %12s   %3s  %3.3f Mpkts/sec    ",
                   $switchNum, $port,$speeds, $count->bstr(),$error->bstr(),$rate);
            
            # Mark an error if not expected 
            if ( $rate < $minRate )
            {
                print ("***ERROR*** TOO SLOW\n");
                $TooSlowCount++;
            }
            elsif ( $rate > $maxRate )
            {
                print ("***ERROR*** TOO FAST\n");
                $TooFastCount++;
            }
            elsif ( $error )
            {
                print ("***ERROR*** Errors encountered\n");
                $ErrorsEncounteredCount++;
            }
            else
            {
                print ("PASS\n");
            }
        }
    }
    return ($TooSlowCount,$TooFastCount,$ErrorsEncounteredCount);
}

sub handleTestBurninStop2 {
    my ($self) = @_;
    my $chip = $self->{CHIP};
    my $port;
    my $vlan;
    my ($pMin,$pMax) = $self->tpPlatformGetFaceplatePortRange();
    my $firstVlan;
    my $lastVlan;

    # Determine the ports and VLAN to use
    
    $pMin++;
    $firstVlan = $pMin+100;
    $lastVlan = $pMax+100;
    
    # Turn off reflect
    $self->tpHandleSetVlanConfig("reflect", "$firstVlan..$lastVlan", "off");
}

sub handleTestBurninStop {
    my ($self) = @_;
    my $chip = $self->{CHIP};
    my $port;
    my $vlan=-1;
    my $firstVlan;
    my $lastVlan;
    my $count;
    my $src=-1;
    my $len=-1;
    my $frameAction=-1;
    my $valid=-1;

    # Determine the ports and VLAN to use
    
    # Re capture the packets
    for my $switchNum ($self->tpGetSwitches())
    {
        my $infoW = ($self->tpGetSwitchInfo())[$switchNum];
        my @externalPorts =
        $self->tpPlatformGetSwitchExternalPortList($switchNum, $TRUE);

        foreach my $p (@externalPorts)
        {
            # Reset the IFG to the testpoint default 
            $self->handleSetPortConfig("$p", "ifg", "$tpIfgDefault[$p]");

            # Break the loop
            $self->handleSetPortConfig ("$p", "mask", "0");     

            # Now wait a bit
            $chip->fmDelay (0,1000000);

            # Count the packets received from that port
            $count = 0;
            for (;;)
            {
                $chip->tpGrabRecvPktInfo(\$src, \$vlan, \$len, \$frameAction, \$valid);

                if($valid == 0)
                {
                    last;
                }
                else
                {
                    $count++;

                    # Scrap payload
                    # for($i = 0; $i < $len; $i++)
                    #{
                    #    my $val = -1;
                    #    $chip->tpGrabRecvPktPayload($i, \$val);
                    #}

                    printf("Packet received on port $src, VLAN $vlan (length $len bytes)\n");

                }

                $chip->tpGrabRecvPkt();
            }
            printf("Recovered $count packets from port $src\n");
        }
    }
}

sub handleMdioRead
{
    my ($self, $switch, $mode, $address, $dev, $reg) = @_;
    my $chip = $self->{CHIP};
    my $testpoint = $self->{TESTPOINT};
    my $val = -1;
    my $status;

    if (!defined($switch))
    {
        printf ("The switch number must be specified!\n");
        return;
    }

    if (!defined($mode))
    {
        printf ("The mode (1G vs 10G) must be specified!\n");
        return;
    }

    if (!defined($address))
    {
        printf ("The address must be specified!\n");
        return;
    }

    if (!defined($dev))
    {
        printf ("The device number must be specified!\n");
        return;
    }

    if (!defined($reg))
    {
        printf ("The register must be specified!\n");
        return;
    }

    if ( $mode )
    {
        $mode = 1;
    }

    if ( $address =~ m/^0x/ )
    {
        $address = hex($address);
    }

    if ( $dev =~ m/^0x/ )
    {
        $dev = hex($dev);
    }

    if ( $reg =~ m/^0x/ )
    {
        $reg = hex($reg);
    }

    $status = $chip->fmPlatformMdioRead ($switch,$mode,$address,$dev,$reg,\$val);

    printf ("MDIO READ (%d,%d,%d,%d,0x%x) => 0x%x  (status=%d)\n",$switch,$mode,$address,$dev,$reg,$val,$status);

}

sub handleMdioWrite
{
    my ($self, $switch, $mode, $address, $dev, $reg, $data) = @_;
    my $chip = $self->{CHIP};
    my $testpoint = $self->{TESTPOINT};
    my $val = -1;
    my $status;


    if (!defined($switch))
    {
        printf ("The switch number must be specified!\n");
        return;
    }

    if (!defined($mode))
    {
        printf ("The mode (1G vs 10G) must be specified!\n");
        return;
    }

    if (!defined($address))
    {
        printf ("The address must be specified!\n");
        return;
    }

    if (!defined($dev))
    {
        printf ("The device number must be specified!\n");
        return;
    }

    if (!defined($reg))
    {
        printf ("The register must be specified!\n");
        return;
    }

    if ( $mode )
    {
        $mode = 1;
    }

    if ( $address =~ m/^0x/ )
    {
        $address = hex($address);
    }

    if ( $dev =~ m/^0x/ )
    {
        $dev = hex($dev);
    }

    if ( $reg =~ m/^0x/ )
    {
        $reg = hex($reg);
    }

    if ( $data =~ m/^0x/ )
    {
        $data = hex($data);
    }

    $status = $chip->fmPlatformMdioWrite ($switch,$mode,$address,$dev,$reg,$data);

    printf ("MDIO WRITE (%d,%d,%d,%d,0x%x) => 0x%x (status=%d)\n",$switch,$mode,$address,$dev,$reg,$data,$status);

}

sub handleShowApiEventLog
{
    my ($self) = @_;
    my $chip = $self->{CHIP};

    $chip->fmDbgDumpEventLog();    
}

sub handleClearApiEventLog
{
    my ($self) = @_;
    my $chip = $self->{CHIP};

    $chip->fmDbgClearEventLog();    
}

sub handleSetLCILoopback
{
    my ($self, $mode) = @_;

    my $chip = $self->{CHIP};

    if ( !defined($mode) )
    {
        printf("No mode defined, use 'on' or 'off'.\n");
    }
    elsif ( $mode eq "on" )
    {
        $chip->tpSetLCILoopback(1);
    }
    elsif ( $mode eq "off" )
    {
        $chip->tpSetLCILoopback(0);
    }
    else
    {
        printf("Invalid mode defined, use 'on' or 'off'.\n");
    }
}

sub tpFM2000SetSwitchStatsConfig
{
    my ($self, $groups) = @_;
    
    my $chip = $self->{CHIP};

    my $bits = 
    {
        1 => $FM_STAT_GROUP_PER_PORT_RX_FRAME_CLASS,
        2 => $FM_STAT_GROUP_PER_PORT_RX_COUNT_BY_LEN,
        3 => $FM_STAT_GROUP_PER_PORT_RX_OCTETS,
        4 => $FM_STAT_GROUP_PER_PORT_RX_COUNT_BY_PRIORITY,
        5 => $FM_STAT_GROUP_PER_PORT_RX_OCTETS_BY_PRIORITY,
        6 => $FM_STAT_GROUP_PER_PORT_FWD_ACTION,
        7 => $FM_STAT_GROUP_PER_PORT_TX_FRAME_CLASS,
        8 => $FM_STAT_GROUP_PER_PORT_TX_COUNT_BY_LEN,
    };

    if ( !defined($groups) )
    {
        printf("Must specify valid group list!\n");
        return;
    }

    my @groupList = $self->validateList($groups, 1, 8);

    if (scalar(@groupList))
    {
        my $statsCfg = 0;

        foreach my $g (@groupList)
        {
            $statsCfg |= $bits->{$g};
        }

        for my $switchNum ($self->tpGetSwitches)
        {
            my %void = (type => "fm_uint32", value => $statsCfg);

            my $status = $chip->fmSetSwitchAttribute($switchNum,
                                                     $FM_STAT_GROUP_ENABLE,
                                                     \%void);
                                            
            if ($status != $FM_OK)
            {
                printf("Could not set statistics config: %s\n",
                       $chip->fmErrorMsg($status));
            }
        }
    }
    else
    {
        printf("Must specify valid group list!\n");
        return;
    }
}

# This wrapping function will allow other functions to return values
# when called in expert (Perl) mode
#
sub tpr
{
    my ($self, $input) = @_;
        my ($node, @arguments) = $self->getNodeForCommand($input);
        return $self->executeHandler($node->{FUNC}, @arguments);
} 


sub tpHandleCreateTrigger
{
    my ($self, $attrs) = @_;
    my $chip = $self->{CHIP};
    my $info = new SDK::fm_triggerRequestInfo();
    my $attr = {};

    if (defined($attrs))
    {
        foreach my $a (split(",", $attrs))
        {
            if ($a =~ m/(requestRateLimiter)=(.*)/)
            {
                $info->{$1} = $2;
                $attr->{$1} = $2;
            }
            else
            {
                printf("Malformed attribute fragment \"$a\": " .
                       "valid fragments are:\n");
                printf("    requestRateLimiter=<1|0>\n");
                return;
            }
        }

        printf("Allocating trigger with the following request criteria:\n");

        foreach my $key (keys(%$attr))
        {
            printf("    %-25s : %s\n", $key, $info->{$key});
        }
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my $number = -1;

        $chip->disableErrors();
        my $status = $chip->fmAllocateTrigger($sw, \$number, $info);
        $chip->enableErrors();

        if ($status == $FM_OK)
        {
            printf("Allocated trigger number $number%s\n",
                   (scalar($self->tpGetSwitches) > 1) ? " on switch $sw" : "");
        }
        else
        {
            printf("Unable to allocate trigger%s: %s\n",
                   (scalar($self->tpGetSwitches) > 1) ? " on switch $sw" : "",
                   $chip->fmErrorMsg($status));
        }
    }
}

sub tpHandleDelTrigger
{
    my ($self, $number) = @_;
    my $chip = $self->{CHIP};

    if (!defined($number))
    {
        printf("Must specify <number>!\n");
        return;
    }

    if ($number eq "all")
    {
        printf("\"all\" is not valid for this range!\n");
        return;
    }

    my @numbers = $self->validateList($number, 0, 256);

    foreach my $sw ($self->tpGetSwitches)
    {
        foreach $number (@numbers)
        {
            $chip->disableErrors();
            my $status = $chip->fmFreeTrigger($sw, $number);
            $chip->enableErrors();

            if ($status != $FM_OK)
            {
                printf("Unable to delete trigger: %s\n",
                       $chip->fmErrorMsg($status));
            }
        }
    }
}

##@cmethod public void handleSetPhyConfig(int port, char* attribute, char* state)
#
# @desc         Handles setting the configuration parameters of a phy at that port
#
# @param[in]    port The set of ports whose phy config are to be modified
#
# @param[in]    attribute 
#
# @param[in]    state 
#
sub handleSetPhyConfig
{
    my ($self, $port, $attribute, $state) = @_;

    my $chip = $self->{CHIP};
	my $platform = $self->{PLATFORM};

	my $reg  		= $phy_attr_map{$platform}{$attribute}{reg};
	my $page 		= $phy_attr_map{$platform}{$attribute}{page};
	my $loopback 	= $phy_attr_map{$platform}{$attribute}{loopback};
	my $mode 		= $phy_attr_map{$platform}{$attribute}{mode};
	my $dev  		= $phy_attr_map{$platform}{$attribute}{dev};
	my $pageReg 	= 22;

	my @portList = $self->validateList($port, $self->tpPlatformGetPortRange());
	
    if (!defined($port) || (scalar(@portList) == 0))
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

	goto NOT_YET_IMPLEMENTED if ($platform ne 'vegas');

    if (!defined($attribute) || !exists($phy_attr_map{$platform}{$attribute}))
    {
        print("Must specify a valid phy attribute!\n");
        return;
    }

	my @indices = arrayFind(map {$self->tpPlatformIsCPUPort($_)} @portList);
	if ($port ne "all" && scalar(@indices) > 0)
	{
		print($TP_MSG_ERR_PORT_INVALID_ARRAY);
		return $FM_ERR_INVALID_ARGUMENT;
	}

	goto STATE if !defined($state);

	foreach my $globalPort (@portList)
	{
		next if $self->tpPlatformIsCPUPort($globalPort);
		goto NOT_YET_IMPLEMENTED if ($globalPort > 48);

		my ($switchNum, $logicalPort) =
						  $self->tpPlatformMapGlobalToLogicalPort($globalPort);

		my $status;
		my $address=$logicalPort;   # Other platform may need a change here
		my ($dataRead, $data);
		
#		$chip->disableErrors();
		if ($state eq "on")
		{
			$status = $chip->fmPlatformMdioWrite ($switchNum,$mode,$logicalPort,$dev,$pageReg,$page);
			$status = $chip->fmPlatformMdioRead ($switchNum,$mode,$address,$dev,$reg,\$dataRead);
			$data = $dataRead | $loopback;
			$status = $chip->fmPlatformMdioWrite ($switchNum,$mode,$address,$dev,$reg,$data);
			printf ("MDIO WRITE (%d,%d,%d,%d,0x%x(page=%d)) => 0x%x (status=%d)\n",$switchNum,$mode,$address,$dev,$reg,$page,$data,$status);
		}
		elsif($state eq "off")
		{
			$status = $chip->fmPlatformMdioWrite ($switchNum,$mode,$logicalPort,$dev,$pageReg,$page);
			$status = $chip->fmPlatformMdioRead ($switchNum,$mode,$address,$dev,$reg,\$dataRead);
			$data = $dataRead & (~$loopback & (2 ** 16 - 1));
			$status = $chip->fmPlatformMdioWrite ($switchNum,$mode,$address,$dev,$reg,$data);
			printf ("MDIO WRITE (%d,%d,%d,%d,0x%x(page=%d)) => 0x%x (status=%d)\n",$switchNum,$mode,$address,$dev,$reg,$page,$data,$status);
		}
		else
		{
			goto STATE;
		}
#		$chip->enableErrors();
		if ($status == $FM_ERR_INVALID_SWITCH
			|| $status == $FM_ERR_INVALID_PORT)
		{
			print($TP_MSG_ERR_PORT_INVALID_ARRAY);
			return $FM_ERR_INVALID_ARGUMENT;
		}
		elsif ($status != $FM_OK)
		{
			printf("Port %2d: BaseT Phy cannot be set to do Mac_loopback!\n", $globalPort);
			return;
		}
	}
	
	return;
	STATE:
		print("Must specify a valid state!\n");
		return;
	NOT_YET_IMPLEMENTED:
		print("The loopback for this specific phy is not yet implemented!\n");
		return;
		
}

##@cmethod public void handleShowPhyConfig(int port, char* attribute, char* state)
#
# @desc         Handles showing the configuration parameters of a phy at that port
#
# @param[in]    port The set of ports whose phy config are to be showed
#
sub handleShowPhyConfig
{
    my ($self, $port, $verbose) = @_;

    my $chip = $self->{CHIP};
	my $platform = $self->{PLATFORM};

	my $phyType = 'Marvel_BaseT';
	my $pageReg = 22;
	my $state = 'FAIL';
	my @attribute = qw ( mac_loopback line_loopback );


	goto NOT_YET_IMPLEMENTED if ($platform ne 'vegas');

    my @portList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@portList) == 0))
    {
        print("Must specify a valid port or port range!\n");
        return;
    }
    my @indices = arrayFind(map {$self->tpPlatformIsCPUPort($_)} @portList);
    if ($port ne "all" && scalar(@indices) > 0)
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    print("\n");
    print("PORT    PHY_TYPE    STATE   MAC_LOOP  LINE_LOOP \n");
    print("----  ------------  ------  --------  --------- \n");

    foreach my $globalPort (@portList)
    {
        next if $self->tpPlatformIsCPUPort($globalPort);
		goto NOT_YET_IMPLEMENTED if ($globalPort > 48);

		my $macLoopback = 'NO';
		my $lineLoopback= 'NO';
		my $status;
		my $dataRead;

		my ($switchNum, $logicalPort) =
						  $self->tpPlatformMapGlobalToLogicalPort($globalPort);
						  
		foreach (@attribute)
		{
			my $reg  		= $phy_attr_map{$platform}{$_}{reg};
			my $page 		= $phy_attr_map{$platform}{$_}{page};
			my $loopback 	= $phy_attr_map{$platform}{$_}{loopback};
			my $mode 		= $phy_attr_map{$platform}{$_}{mode};
			my $dev  		= $phy_attr_map{$platform}{$_}{dev};
							  
			$status = $chip->fmPlatformMdioWrite ($switchNum,$mode,$logicalPort,$dev,$pageReg,$page);
			if (defined($verbose) && ($verbose eq 'verbose'))
			{
				printf ("MDIO WRITE (%d,%d,%d,%d,0x%x) => 0x%x (status=%d)\n",$switchNum,$mode,$logicalPort,$dev,$pageReg,$page,$status);
			}
			$status = $chip->fmPlatformMdioRead ($switchNum,$mode,$logicalPort,$dev,$reg,\$dataRead);
			if (defined($verbose) && ($verbose eq 'verbose'))
			{
				printf ("MDIO READ (%d,%d,%d,%d,0x%x) => 0x%x (status=%d)\n",$switchNum,$mode,$logicalPort,$dev,$reg,$dataRead,$status);
			}
			
			$macLoopback = 'YES' if (($_ eq 'mac_loopback') and ($dataRead & $loopback));
			$lineLoopback = 'YES' if (($_ eq 'line_loopback') and ($dataRead & $loopback));
		}
		$state = 'OK' if ($status == 0);
			printf("%-5s %-14s %-8s %-10s %-10s\n", $globalPort, $phyType,$state,$macLoopback,$lineLoopback);
    }
    print("\n");

	return;
	NOT_YET_IMPLEMENTED:
		print("49    This specific phy is not yet implemented!\n");
		return;

}   # end handleShowPhyConfig

##@cmethod public void handleShowHistory()
#
# @desc         Handles showing the command history of TestPoint
#
sub handleShowHistory
{
    my ($self, $file, $filename) = @_;

    if (defined $file)
    {
        if ($file eq "file")
        {
            return $self->handleShowFile($filename);
        }
        elsif ($file eq "startup")
        {
            return $self->handleShowFile("startup");
        }
        else
        {
            printf(
            "Pass \"startup\" or \"file\" to history to display history files.\n");
            return;
        }
    }
    my $testpoint = $self->{TESTPOINT};

    my $index = 1;
    foreach my $input (@{$testpoint->{HISTORY}})
    {
        if ($input ne $testpoint->{HISTORY_DELETE_STRING})
        {
            printf("%4d %s\n", $index, $input);
        }
        $index++;
    }
}   # end handleShowHistory

##@cmethod public void handleShowFile()
#
# @desc         Handles showing a file to the screen
# @param file   Filename to print
#
sub handleShowFile
{
    my ($self,$file) = @_;

    my $testpoint = $self->{TESTPOINT};

    if (!defined $file)
    {
        printf("Must specify a valid script name!\n");
        return;
    }
    if ($file eq "startup")
    {
        if (-e $testpoint->{OPTS}->{CFSTARTUP})
        {
            $file = $testpoint->{OPTS}->{CFSTARTUP};
        }
        else
        {
            $file = $testpoint->{OPTS}->{NFSSTARTUP};
        }

        $file .= $testpoint->{OPTS}->{STARTUPFILE};
    }
    elsif ($file !~ m/^(\.|\/)/)
    {
        if (-e $testpoint->{OPTS}->{CFSTARTUP})
        {
            $file = $testpoint->{OPTS}->{CFSTARTUP} . $file;
        }
        else
        {
            $file = $testpoint->{OPTS}->{NFSSTARTUP} . $file;
        }
    }

    if (-e $file)
    {
        if (!open(FH, "$file"))
        {
            printf("Could not open file $file for reading.\n");
            return;
        }
        my $topline = <FH>;

        if ((defined $topline) && ($topline eq 
                "# TestPoint History\n"))
        {
            printf($topline);
            while (my $line = <FH>)
            {
                printf($line);
            }
        } 
        else
        {
            printf("File $file does not appear to be a history file.\n");
        }
        close FH;
    }
    else
    {
        printf("No file found at " . 
                $file . "\n");
    }

}   # end handleShowFile

##@cmethod public void handleSaveHistory()
#
# @desc         Handles saving the current history to file
# @param file   The filename to use, specify "startup" to use the
#               saved startup location for TestPoint boot
#
sub handleSaveHistory
{
    my ($self, $type, $filename) = @_;
    if (!defined($type))
    {
        printf("Must specify either a startup or file save type!\n");
        return;
    }

    my $testpoint = $self->{TESTPOINT};

    if ($type eq "startup")
    {
        if (-e $testpoint->{OPTS}->{CFSTARTUP})
        {
            $filename = $testpoint->{OPTS}->{CFSTARTUP};
        }
        else
        {
            $filename = $testpoint->{OPTS}->{NFSSTARTUP};
        }

        $filename .= $testpoint->{OPTS}->{STARTUPFILE};
    }
    elsif($type ne "file")
    {
        printf("Unknown save type: $type\n");
        return;

    }
    if (!defined $filename)
    {
        printf("Must specify a filename for \"save history file\"\n");
    }

    if ($filename !~ m/^(\.|\/)/)
    {
        if (-e $testpoint->{OPTS}->{CFSTARTUP})
        {
            $filename = $testpoint->{OPTS}->{CFSTARTUP} . $filename;
        }
        else
        {
            $filename = $testpoint->{OPTS}->{NFSSTARTUP} . $filename;
        }
    }

    if (!open(FH, ">$filename"))
    {
        printf("Could not open file $filename for writing.\n");
        return;
    }
    printf(FH "# TestPoint History\n");
    foreach my $input (@{$testpoint->{HISTORY}})
    {
        if ($input ne $testpoint->{HISTORY_DELETE_STRING})
        {
            printf(FH "%s\n", $input);
        }
    }
    printf("Saved history to $filename\n");
    close (FH);
} # end handleDeleteHistory

##@cmethod public void handleDeleteHistory()
#
# @desc         Handles deleting commands in the history of TestPoint
# @param range  The range of history entries to delete
#
sub handleDeleteHistory
{
    my ($self,$range, $filename) = @_;

    my $testpoint = $self->{TESTPOINT};

    if (!defined $range)
    {
        printf(
    "To delete history, specify a range or \"all\" to delete all history\n");
    }

    if (($range eq "all") || ($range eq "\"all\""))
    {
        @{$testpoint->{HISTORY}} = ();
        printf("TestPoint history deleted.\n");
        return;
    }
    elsif ($range eq "startup")
    {
        return $self->handleDeleteFile($range);
    } 
    elsif ($range eq "file")
    {
        return $self->handleDeleteFile($filename);
    }

    my @entryList = 
        sort($self->validateExplicitList($TRUE, 
                                $range, 
                                (1..scalar(@{$testpoint->{HISTORY}}))));
    if (scalar(@entryList) > 0)
    {
        printf("Deleted entries: ");
        foreach my $entry (@entryList)
        {
            printf("%d ", $entry);
            splice (@{$testpoint->{HISTORY}}, $entry-1, 1, 
                    $testpoint->{HISTORY_DELETE_STRING});
        }
        printf("\n");
    } else {
        printf("No entries deleted.\n");
    }

}   # end handleDeleteHistory

##@cmethod public void handleDeleteFile()
#
# @desc         Handles deleting the saved TestPoint startup
# @param file   Filename to delete
#
sub handleDeleteFile
{
    my ($self,$file) = @_;

    my $testpoint = $self->{TESTPOINT};

    if ($file eq "startup")
    {
        if (-e $testpoint->{OPTS}->{CFSTARTUP})
        {
            $file = $testpoint->{OPTS}->{CFSTARTUP};
        }
        else
        {
            $file = $testpoint->{OPTS}->{NFSSTARTUP};
        }

        $file .= $testpoint->{OPTS}->{STARTUPFILE};
    }
    elsif ($file !~ m/^(\.|\/)/)
    {
        if (-e $testpoint->{OPTS}->{CFSTARTUP})
        {
            $file = $testpoint->{OPTS}->{CFSTARTUP} . $file;
        }
        else
        {
            $file = $testpoint->{OPTS}->{NFSSTARTUP} . $file;
        }
    }
    if (-e $file)
    {
        if (!open(FH, "$file"))
        {
            printf("Could not open file $file for reading.\n");
            return;
        }
        my $topline = <FH>;
        close FH;

        if ((defined $topline) && ($topline eq 
                "# TestPoint History\n"))
        {
            if (unlink($file) == 1)
            {
                printf("History file $file deleted\n");
            }
            else
            {
                printf("Error, could not delete history file: $file\n");
            }
        } 
        else
        {
            printf("File $file does not appear to be a history file.\n");
        }
    }
    else
    {
        printf("No file found at " . 
                $file . "\n");
    }

}   # end handleDeleteFile

##@cmethod public void handleShowApiAttribute()
#
# @desc         Handles printing the API attribute
# @param type   The attribute type
# @param attr   The attribute to print
#
sub handleShowApiAttribute
{
    my ($self, $type, $attr) = @_;
    my $chip = $self->{CHIP};
    my $parse_type = $FM_API_ATTR_BOOL;
    my $type_str = "";
    my $defvalue = $FM_DISABLED;
    if (($type eq "all") || ($type eq "list"))
    {
        $chip->fmDbgDumpApiAttributes();
        return;
    }
    elsif (($type eq "bool") || ($type eq "boolean"))
    {
        $parse_type = $FM_API_ATTR_BOOL;
        $type_str = "fm_bool";
        $defvalue = $FM_DISABLED;
    }
    elsif (($type eq "float"))
    {
        $parse_type = $FM_API_ATTR_FLOAT;
        $type_str = "fm_float";
        $defvalue = -1.0;
    }
    elsif (($type eq "int") || ($type eq "integer"))
    {
        $parse_type = $FM_API_ATTR_INT;
        $type_str = "fm_int";
        $defvalue = -1;
    }
    elsif (($type eq "text") || ($type eq "string"))
    {
        $parse_type = $FM_API_ATTR_TEXT;
        $type_str = "fm_char";
        $defvalue = "";
        $chip->disableErrors();
        my $ret = $chip->fmGetTextApiAttribute($attr, '__undefined__');
        $chip->enableErrors();
        if ($ret eq "__undefined__")
        {
            printf("$attr set at default or undefined\n");
        }
        else
        {
            printf("$attr ($type_str) = " . $ret . "\n");
        }
        return;
    }
    if ($type_str eq "")
    {
        printf("Unrecognized API type: $type\n");
        return;
    }
    my %void = (type => $type_str, value => $defvalue);
    $chip->disableErrors();
    my $ret = $chip->fmGetApiAttribute($attr, $parse_type, \%void);
    $chip->enableErrors();

    if ($ret == $FM_OK)
    {
        printf("$attr ($type_str) = " . $void{value} . "\n");
    }
    else
    {
        if ($ret == $FM_ERR_KEY_NOT_FOUND)
        {
            printf("Key not found- may either be at the default or incorrect key name\n");
        }
        else
        {
            printf("Could not get attr: $attr ($type_str) error $ret\n");
        }
    }
}   # end handleShowApiAttribute

##@cmethod public void handleSetApiAttribute()
#
# @desc         Handles setting the API attribute
# @param type   The attribute type
# @param attr   The attribute to print
#
sub handleSetApiAttribute
{
    my ($self, $type, $attr, $value) = @_;
    my $chip = $self->{CHIP};
    my $parse_type = -1;
    my $type_str = undef;
    if (!defined $value)
    {
        printf("Must specify <type> <attr> <value>\n");
        return;
    }
    if (($type eq "bool") || ($type eq "boolean"))
    {
        $parse_type = $FM_API_ATTR_BOOL;
        $type_str = "fm_bool";
        if ((lc($value) eq "true") || (lc($value) eq "on") ||
            (lc($value) eq "enable") || (lc($value) eq "enabled") ||
            (lc($value) eq "1"))
        {
            $value = $FM_ENABLED;
        } elsif ((lc($value) eq "false") || (lc($value) eq "off") ||
            (lc($value) eq "disable") || (lc($value) eq "disabled") ||
            (lc($value eq "0")))
        {
            $value = $FM_DISABLED;
        } 
        else
        {
            printf("Unrecognized boolean value $value\n");
            return;
        }
    }
    elsif (($type eq "float"))
    {
        $parse_type = $FM_API_ATTR_FLOAT;
        $type_str = "fm_float";
    }
    elsif (($type eq "int") || ($type eq "integer"))
    {
        $parse_type = $FM_API_ATTR_INT;
        $type_str = "fm_int";
    }
    elsif (($type eq "text") || ($type eq "string"))
    {
        $parse_type = $FM_API_ATTR_TEXT;
        $type_str = "fm_char";
    }
    if (!defined($type_str))
    {
        printf("Unrecognized API type: $type\n");
        return;
    }
    my %void = (type => $type_str, value => $value);
    my $ret = $chip->fmSetApiAttribute($attr, $parse_type, \%void);

    if ($ret == $FM_OK)
    {
        printf("$attr ($type_str) = " . $void{value} . "\n");
    }
    else
    {
        printf("Could not set attr: $attr ($type_str) error $ret\n");
    }
}   # end handleSetApiAttribute


sub settingPortMask
{
    my ($self, $switchNum, $externalPortList) = @_;
    my $chip = $self->{CHIP};
    my $switchInfo = new SDK::fm_switchInfo();
    $chip->fmGetSwitchInfo($switchNum, $switchInfo);
    if ($switchInfo->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG) 
    {
        my $switchMember = -1;
        my $searchToken = new SDK::fm_voidptrW();
        my $status = $chip->fmGetSWAGSwitchFirst($switchNum, 
                                                 $searchToken, 
                                                 \$switchMember);
        $chip->disableErrors();
        while ($status == $FM_OK)
        {
            $self->settingPortMask($switchMember, $externalPortList);
            $status = $chip->fmGetSWAGSwitchNext($switchNum,
                                                 $searchToken,
                                                 \$switchMember);
        }
        $chip->enableErrors();
    }
    else
    {
        my @portList = $self->tpPlatformGetSwitchPortList($switchNum,$TRUE);
        my $portString = $self->StringifyList(@portList);
        @portList = $self->validateExplicitList($TRUE, 
                                                $portString, 
                                                @{$externalPortList->{$switchNum}});
        my %void = (type => "fm_uint32", value => undef);
        for my $p (@portList)
        {
            my ($sw, $logicalPort) = 
                        $self->tpPlatformMapGlobalToLogicalPort($p);
            if (not $chip->fmIsInternalPort($switchNum, $logicalPort) )
            {
                $void{value} = 1 << $logicalPort;
                #printf("Switch = %d, port = %d, mask = 0x%x\n", $switchNum, 
                #       $logicalPort, $void{value});
                $chip->fmSetPortAttribute($switchNum, 
                                          $logicalPort, 
                                          $FM_PORT_MASK, 
                                          \%void);
            }
        }
    }
}

sub handleSetSwitchFFUSliceAlloc
{
    my ($self, $partition, $values) = @_;
    my $chip = $self->{CHIP};

    if (!defined($ffu_slice_cfg_map{$partition}))
    {
        if ($partition eq "apply")
        {
            if (!defined($self->{FFU}->{sliceAlloc}))
            {
                printf("There is no new slice allocation available\n");
            }
            
            my $err;
            my $switchCount = scalar(@{[$self->tpGetSwitches]});
            foreach my $switchNum ($self->tpGetSwitches)
            {
                 my %void = (type => "fm_ffuSliceAllocations", value => $self->{FFU}->{sliceAlloc});

                 $err = $chip->fmSetSwitchAttribute($switchNum, 
                                                    $FM_FFU_SLICE_ALLOCATIONS, 
                                                    \%void);

                if ($err != $FM_OK)
                {
                    printf("Error occured while applying slice allocations\n");
                    last;
                }
            }
            return;
        }

        printf("Invalid FFU Slice partition\n");
        return;
    }

    if ($values !~ m/^-?[0-9]{1,2}:-?[0-9]{1,2}$/)
    {
        printf("Invalid Range. The range must be in the following format: x:x\n");
        return;
    }

    if (!defined($self->{FFU}->{sliceAlloc}))
    {
        $self->{FFU}->{sliceAlloc} = tpCreateFFUSliceAllocationsConfig();
    }

    #Don't forget the apply

    my ($first, $last) = split(/:/, $values, 2);
    
    my $partitionFirst  = $ffu_slice_cfg_map{$partition} . "FirstSlice";
    my $partitionLast   = $ffu_slice_cfg_map{$partition} . "LastSlice";

    $self->{FFU}->{sliceAlloc}->{$partitionFirst} = $first;
    $self->{FFU}->{sliceAlloc}->{$partitionLast} = $last;
}

sub tpCreateFFUSliceAllocationsConfig
{
    my ($self) = @_;

    my $sliceAlloc = new SDK::fm_ffuSliceAllocations();

    foreach my $partition (keys(%ffu_slice_cfg_map))
    {
        my $partitionFirst  = $ffu_slice_cfg_map{$partition} . "FirstSlice";
        my $partitionLast   = $ffu_slice_cfg_map{$partition} . "LastSlice";

        #All partition are unallocated
        $sliceAlloc->{$partitionFirst} = -1;
        $sliceAlloc->{$partitionLast} = -1;
    }

    return $sliceAlloc;
}

sub handleShowFFUSliceAlloc
{
    my ($self, $slice) = @_;
    my $chip = $self->{CHIP};

    my $sliceAlloc = tpCreateFFUSliceAllocationsConfig;

    if (!defined($slice) || ( ($slice ne "active") && ($slice ne "new") ) )
    {
        printf("Must specify active or new configuration\n");
        return; 
    }

    my $switchCount = scalar(@{[$self->tpGetSwitches]});

    printf("\n");

    printf("------------------------------------------\n");
    printf("      FFU Slice Allocations: $slice       \n");
    printf("------------------------------------------\n");
    
    for my $switchNum ($self->tpGetSwitches)
    {
        print("\n");

        if ($switchCount > 1)
        {
            printf("Switch %d:\n", $switchNum);
            printf("------------------------------------------\n");
        }

        if ($slice eq "active")
        {
            my %void = (type => "fm_ffuSliceAllocations", value => $sliceAlloc);
                
            my $err = $chip->fmGetSwitchAttribute($switchNum, 
                                                  $FM_FFU_SLICE_ALLOCATIONS, 
                                                  \%void);
    
            if ($err != $FM_OK)
            {
                printf("Error occured while reading current slice allocations\n");
                return;
            }
    
            $sliceAlloc = $void{value};
        }
        elsif($slice eq "new")
        {
            if (!defined($self->{FFU}->{sliceAlloc}))
            {
                printf("There is no new slice allocation available\n");
                return
            }
            else
            {
                $sliceAlloc = $self->{FFU}->{sliceAlloc};
            }
        }
    
        printf("\n");
        printf("Partition       First Slice   Last Slice\n");
        printf("------------------------------------------\n");
        
        foreach my $partition (keys(%ffu_slice_cfg_map))
        {
            my $partitionFirst  = $ffu_slice_cfg_map{$partition} . "FirstSlice";
            my $partitionLast   = $ffu_slice_cfg_map{$partition} . "LastSlice";
    
            printf("%-16s%-14s%-14s\n", $partition, $sliceAlloc->{$partitionFirst}, $sliceAlloc->{$partitionLast});
        }

        printf("\n");
    }
}

1;
