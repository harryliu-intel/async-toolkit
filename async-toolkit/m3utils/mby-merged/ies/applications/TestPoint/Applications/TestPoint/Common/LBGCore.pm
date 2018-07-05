# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             LBGCore.pm
# Creation Date:    01/16/2009.
# Description:      TestPoint commands for load-balancing groups.
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

package Applications::TestPoint::Common::LBGCore;

use strict;
use warnings;

use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

###############################################################################
#       CONSTANTS
###############################################################################

# Specifies that ParseLbgParm should accept the "all" specification.
my $PARSE_ALL = 0x01;

# Specifies that ParseLbgParm should accept lists and ranges.
my $PARSE_MULTIPLE = 0x02;

# Specifies that ParseLbgParm should assign an identifier if none is present.
my $PARSE_ASSIGN = 0x04;

# Specifies that validateList should generate a detailed error message
# if it encounters an invalid input.
my $VERBOSE_DETAIL = 2;

# Maximum allowable LBG number.
my $MAX_LBG_NUMBER = 65535;

my $MSG_MULTI_SWITCH_INVALID = "Command not valid in multi-switch mode!\n";

my %lbg_attr_map =
(
    'redirect-method'   =>  $FM_LBG_REDIRECT_METHOD,
    'state'             =>  $FM_LBG_STATE,
    'vlan'              =>  $FM_LBG_VLAN,
    'map'               =>  $FM_LBG_DISTRIBUTION_MAP_RANGE,
    'bins'              =>  $FM_LBG_DISTRIBUTION_MAP_SIZE,
);

my %lbg_port_attr_map =
(
    'mode'              =>  $FM_LBG_PORT_MODE,
    'redirect-target'   =>  $FM_LBG_PORT_REDIRECT_TARGET,
);

my %lbg_mode_map =
(
    'nplus1'            =>  $FM_LBG_MODE_NPLUS1,
    'redirect'          =>  $FM_LBG_MODE_REDIRECT,
    'mapped'            =>  $FM_LBG_MODE_MAPPED,
);


###############################################################################
#       LOCAL VARIABLES
###############################################################################

#
# Used to keep track of the logical port numbers assigned to the LBGS.
#   $__lbgNumbers->{$groupId} --> ($lbgSwitch, $lbgNumber)
# 
my $__lbgNumbers = {};


###############################################################################
#       LOCAL FUNCTIONS
###############################################################################

##
# GetLbgNumber
# 
# @descr        Returns the logical port number assigned to the specified
#               load-balancing group.
# 
# @param[in]    groupId is the user-specified group identifier.
# 
# @return       (lbgSwitch, lbgNumber), where lbgSwitch is the switch
#               number, and lbgNumber is the logical port number.
# 
sub GetLbgNumber
{
    my ($groupId) = @_;

    if (defined($__lbgNumbers->{$groupId}))
    {
        return @{$__lbgNumbers->{$groupId}};
    }
    else
    {
        return (undef, undef);
    }

}   # end GetLbgNumber


##
# SetLbgNumber
# 
# @descr        Stores the logical port number assigned to the specified
#               load-balancing group.
# 
# @param[in]    groupId is the user-specified group identifier.
# 
# @param[in]    lbgSwitch is the switch on which the LBG resides.
# 
# @param[in]    lbgNumber is the logical port number of the LBG.
#               May be undef, in which case the logical port assignment
#               will be deleted.
# 
sub SetLbgNumber
{
    my ($groupId, $lbgSwitch, $lbgNumber) = @_;

    if (defined($lbgNumber))
    {
        $__lbgNumbers->{$groupId} = [$lbgSwitch, $lbgNumber];
    }
    else
    {
        delete $__lbgNumbers->{$groupId};
    }

}   # end SetLbgNumber


##
# ParseLbgParm
# 
# @descr        Parses the LBG specification from a command and returns
#               a list of LBG numbers.
# 
# @param[in]    spec is a string specifying a list of LBG numbers.
# 
# @param[in]    options specifies a bit mask of parse options.
# 
# @return       A list of LBG numbers, or an empty list if an error occurs.
# 
sub ParseLbgParm
{
    my ($self, $spec, $options) = @_;

    $options = 0 if !defined($options);

    if (!defined($spec))
    {
        if ($options & $PARSE_ASSIGN)
        {
            for (my $groupId = 1 ; $groupId <= $MAX_LBG_NUMBER ; ++$groupId)
            {
                if (!defined($__lbgNumbers->{$groupId}))
                {
                    return ($groupId);
                }
            }
        }
    }
    elsif ($spec =~ m/all/i)
    {
        if ($options & $PARSE_ALL)
        {
            return ("all");
        }
        else
        {
            print "May not specify \"all\" LBGs for this command\n";
            return ();
        }
    }

    my @lbgs = $self->validateList($spec, 0, $MAX_LBG_NUMBER, $VERBOSE_DETAIL);

    if (scalar(@lbgs) > 1 && !($options & $PARSE_MULTIPLE))
    {
        print "Must specify a single LBG for this command\n";
        return ();
    }

    if (!@lbgs)
    {
        print "Must specify a valid LBG number\n";
    }

    return @lbgs;

}   # end ParseLbgParm


##
# GetLbgGroupList
# 
# @descr        Returns an expanded list of LBG group identifiers.
# 
# @param[in]    sw is the switch on which to operate.
# 
# @param[in]    lbgs is the list of LBG identifiers specified
#               on the command line.
# 
# @return       A list of group identifiers.
# 
sub GetLbgGroupList
{
    my ($self, $sw, @lbgs) = @_;

    if (scalar(@lbgs) == 1 && $lbgs[0] eq "all")
    {
        @lbgs = ();
        foreach my $groupId (keys(%{$__lbgNumbers}))
        {
            if ($__lbgNumbers->{$groupId}->[0] == $sw)
            {
                push @lbgs, $groupId;
            }
        }
    }

    return sort @lbgs;

}   # end GetLbgGroupList


##
# GetLBGMemberList
# 
# @descr        Returns a list of LBG member ports
# 
# @param[in]    sw is the switch on which to operate.
# 
# @param[in]    lbgNumber is the logical port number of the
#               load balancing group.
# 
# @return       List of member ports.
# 
sub GetLBGMemberList
{
    my ($self, $sw, $lbgNumber) = @_;
    my $chip = $self->{'CHIP'};
    my @memberList = ();
    my $status;
    my $port;

    $chip->disableErrors();
    $status = $chip->fmGetLBGPortFirst($sw, $lbgNumber, \$port);
    while ($status == $FM_OK)
    {
        my $globalPort = $self->tpPlatformMapLogicalToGlobalPort($sw, $port);
        if ($globalPort > 0)
        {
            push @memberList, $globalPort;
        }
        $status = $chip->fmGetLBGPortNext($sw, $lbgNumber, $port, \$port);
    }
    $chip->enableErrors();

    return @memberList;

}   # end GetLBGMemberList


##
# ShowLoadBalancingGroup
# 
# @descr        Displays a single load-balancing group.
# 
# @param[in]    sw is the switch on which to operate.
# 
# @param[in]    groupId is the user-specified group identifier.
# 
sub ShowLoadBalancingGroup
{
    my ($self, $sw, $groupId) = @_;
    my $chip = $self->{'CHIP'};
    my $status;
    my %void;

    # logical port
    my ($lbgSwitch, $lbgNumber) = GetLbgNumber($groupId);

    if (!defined($lbgNumber))
    {
        print "'$groupId' is not a valid LBG!\n";
        return $FM_FAIL;
    }

    if ($lbgSwitch != $sw)
    {
        print "Switch number ($lbgSwitch) of LBG does not match current switch\n";
        return $FM_FAIL;
    }

    # member ports
    my @memberList = $self->GetLBGMemberList($sw, $lbgNumber);

    my $lbgPorts =
        scalar(@memberList) ? $self->StringifyList(@memberList) : "none";

    # redirect-methodvoid
    my $lbgRedirect;

    %void = (type => "fm_int", value => 0);
    $status = $chip->fmGetLBGAttribute(
            $sw,
            $lbgNumber,
            $FM_LBG_REDIRECT_METHOD,
            \%void);
    return $status if ($status != $FM_OK);

    if ($void{value} == $FM_LBG_REDIRECT_PORT)
    {
        $lbgRedirect = "port";
    }
    elsif ($void{value} == $FM_LBG_REDIRECT_STANDBY)
    {
        $lbgRedirect = "standby";
    }
    elsif ($void{value} == $FM_LBG_REDIRECT_ALL_PORTS)
    {
        $lbgRedirect = "all-ports";
    }
    elsif ($void{value} == $FM_LBG_REDIRECT_PREFER_STANDBY)
    {
        $lbgRedirect = "prefer-standby";
    }
    else
    {
        $lbgRedirect = sprintf("unknown (%d)", $void{value});
    }

    # state
    my $lbgState;

    %void = (type => "fm_int", value => 0);
    $status = $chip->fmGetLBGAttribute($sw, $lbgNumber, $FM_LBG_STATE, \%void);
    return $status if ($status != $FM_OK);

    if ($void{value} == $FM_LBG_STATE_ACTIVE)
    {
        $lbgState = "active";
    }
    elsif ($void{value} == $FM_LBG_STATE_INACTIVE)
    {
        $lbgState = "inactive";
    }
    else
    {
        $lbgState = sprintf("unknown(%d)", $void{value});
    }

    # vlan
    %void = (type => "fm_int", value => 0);
    $status = $chip->fmGetLBGAttribute($sw, $lbgNumber, $FM_LBG_VLAN, \%void);
    return $status if ($status != $FM_OK);

    my $lbgVlan = $void{value};

    printf("%5d  %5d  %-8s  %-5d  %-8s  %s\n",
           $groupId,
           $lbgNumber,
           $lbgState,
           $lbgVlan,
           $lbgRedirect,
           $lbgPorts);

    return $FM_OK;

}   # end ShowLoadBalancingGroup

##
# ShowLoadBalancingGroupMap
# 
# @descr        Displays a single load-balancing group map.
# 
# @param[in]    lbg is the user-specified group identifier.
# 
sub ShowLoadBalancingGroupMap
{
    my ($self, $groupId) = @_;
    my $chip = $self->{'CHIP'};
    my $status;
    my %void;

    # logical port
    my ($lbgSwitch, $lbgNumber) = GetLbgNumber($groupId);

    if (!defined($lbgNumber))
    {
        print "'$groupId' is not a valid LBG!\n";
        return $FM_FAIL;
    }

    # Get the number of bins used by this LBG
    %void = (type => "fm_int", value => 0);
    $status = $chip->fmGetLBGAttribute($lbgSwitch,
                                       $lbgNumber,
                                       $FM_LBG_DISTRIBUTION_MAP_SIZE,
                                       \%void);
    return $status if ($status != $FM_OK);

    my $nbrBins = $void{value};
    
    printf(" with $nbrBins bins:\n\n");

    printf("Ports : Bins\n");
    my $distMap = new SDK::fm_LBGDistributionMapRange();
    %void = (type => 'fm_LBGDistributionMapRange', value => $distMap);
    $distMap->{'firstBin'} = 0;
    my @map = (0) x $nbrBins;
    $distMap->{'numberOfBins'} = scalar(@map);
    my %value = ( type=>"fm_int[]", value => \@map);
    $status = $chip->fmSWIGGetLBGBins($lbgSwitch, $lbgNumber, \%void, \%value);
    return $status if ($status != $FM_OK);

    my $j = 0;
    my $ports2bins = {};
    for ( my $i = 0; $i < $nbrBins; $i ++)
    {
        my $lport = $map[$i];
        if (!defined $ports2bins->{$lport})
        {
            $ports2bins->{$lport} = [$i];
        }
        else
        {
            push(@{$ports2bins->{$lport}}, $i);
        }
    }
    foreach my $lport (sort(keys(%{$ports2bins})))
    {
        printf("%5d : %s\n", $lport, 
                $self->StringifyList(@{$ports2bins->{$lport}}));
    }
        
    
}   # end ShowLoadBalancingGroupMap

##
# SynchronizeLBGList
# 
# @descr        Synchronize the load-balancing group from TestPoint handle to
#               the API to cover cases where load-balancing group were created
#               from another application.
# 
# @param[in]    None
# 
sub SynchronizeLBGList
{
    my ($self) = @_;
    my $chip = $self->{'CHIP'};
    my $status;
    my %currGroup;

    my @switchList = $self->tpGetSwitches();

    foreach my $sw (@switchList)
    {
        foreach my $groupId (keys(%{$__lbgNumbers}))
        {
            if ($__lbgNumbers->{$groupId}->[0] == $sw)
            {
                $currGroup{$__lbgNumbers->{$groupId}->[1]} = 1;
            }
        }

        my $currLBG = 0;
        $chip->disableErrors();
        $status = $chip->fmGetLBGFirst($sw, \$currLBG);
        $chip->enableErrors();
        return $status if ($status != $FM_OK);

        if (!defined($currGroup{$currLBG}))
        {
            SetLbgNumber($currLBG, $sw, $currLBG);
        }

        my $nextLBG = 0;
        while (1)
        {
            $chip->disableErrors();
            $status = $chip->fmGetLBGNext($sw, $currLBG, \$nextLBG);
            $chip->enableErrors();
            return $status if ($status != $FM_OK);

            if (!defined($currGroup{$nextLBG}))
            {
                SetLbgNumber($nextLBG, $sw, $nextLBG);
            }
            $currLBG = $nextLBG;
        }
    }
}



###############################################################################
#       PUBLIC FUNCTIONS
###############################################################################

##
# Adds member ports to a load-balancing group.
# 
# Syntax:
#   add lbg port <lbg> <ports>
# 
sub tpHandleAddLBGPort
{
    my ($self, @args) = @_;
    my $chip = $self->{'CHIP'};

    $self->SynchronizeLBGList();

    # Get switch selection.
    my @switchList = $self->tpGetSwitches();
    if (scalar(@switchList) > 1)
    {
        print $MSG_MULTI_SWITCH_INVALID;
        return $FM_FAIL;
    }
    my $sw = $switchList[0];

    # Check number of arguments.
    if (scalar(@args) > 2)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }
    elsif (scalar(@args) < 2)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }

    # Get <lbg> parameter.
    my @lbgs = $self->ParseLbgParm($args[0]);
    return $FM_ERR_INVALID_ARGUMENT if (!@lbgs);

    # Get port list.
    my @portList = $self->validateList($args[1],
                                       $self->tpPlatformGetPortRange());
    if (scalar(@portList) == 0)
    {
        print $TP_MSG_ERR_PORT_INVALID_ARRAY;
        return $FM_FAIL;
    }

    foreach my $groupId (@lbgs)
    {
        my ($lbgSwitch, $lbgNumber) = GetLbgNumber($groupId);

        if (!defined($lbgNumber))
        {
            print "LBG $groupId not defined!\n";
            return $FM_FAIL;
        }

        if ($lbgSwitch != $sw)
        {
            print "LBG $groupId is not on selected switch\n";
            return $FM_FAIL;
        }

        my @switchPortList =
            $self->validateExplicitList(
                $TRUE,
                $args[1],
                $self->tpPlatformGetSwitchPortList($sw, $FALSE));

        if (scalar(@switchPortList) == 0)
        {
            print "None of the ports are defined on switch $sw\n";
            next;
        }

        # Add each port to load-balancing group.
        foreach my $globalPort (@switchPortList)
        {
            # Skip if CPU port.
            next if $self->tpPlatformIsCPUPort($globalPort);

            # Map port number for the benefit of non-SWAG Vegas.
            my (undef, $localPort) = 
                $self->tpPlatformMapGlobalToLogicalPort($globalPort);

            $chip->disableErrors();
            my $status = $chip->fmAddLBGPort($sw, $lbgNumber, $localPort);
            $chip->enableErrors();

            if ($status != $FM_OK)
            {
                print "Failed to add port $globalPort " . 
                      "to LBG $groupId on switch $sw\n";
                print $chip->fmErrorMsg($status) . "\n";
                return $status;
            }

        }   # end foreach my $globalPort (@portList)

    }   # end foreach my $groupId (@groupList)

    return $FM_OK;

}   # end tpHandleAddLBGPort


##
# Creates a load-balancing group.
# 
# Syntax:
#   create lbg <lbg>
#
sub tpHandleCreateLBG
{
    my ($self, @args) = @_;
    my $chip = $self->{'CHIP'};

    $self->SynchronizeLBGList();

    # Get switch selection.
    my @switchList = $self->tpGetSwitches();
    if (scalar(@switchList) > 1)
    {
        print $MSG_MULTI_SWITCH_INVALID;
        return $FM_FAIL;
    }
    my $sw = $switchList[0];

    my %void = (type => 'fm_int', value => 0);
    $chip->fmGetSwitchAttribute($sw,
                                $FM_LBG_MODE,
                                \%void);
    my $lbgMode = $void{value};

    if (defined($args[1]))
    {
        $lbgMode = $lbg_mode_map{$args[1]};
    }

    my $binNum = $FM_LBG_DEFAULT_BINS_PER_DISTRIBUTION;
    if (defined($args[2]))
    {
        $binNum = $args[2];
    }

    # Get <lbg> parameter.
    my @lbgs = $self->ParseLbgParm($args[0], $PARSE_ASSIGN);
    return $FM_ERR_INVALID_ARGUMENT if (!@lbgs);

    my ($groupId, $status);

    foreach $groupId (@lbgs)
    {
        my ($lbgSwitch, $lbgNumber) = GetLbgNumber($groupId);
        if (defined($lbgNumber))
        {
            print "LBG $groupId already exists on switch $lbgSwitch!\n";
            return $FM_FAIL;
        }

        $lbgNumber = $FM_LOGICAL_PORT_ANY;

        my $lbgParam = new SDK::fm_LBGParams();
        $lbgParam->{'numberOfBins'} = $binNum;
        $lbgParam->{'mode'} = $lbgMode;
        $status = $chip->fmCreateLBGExt($sw, \$lbgNumber, $lbgParam);

        if ($status == $FM_OK)
        {
            print "Created LBG $groupId (logical port $lbgNumber) " .
                  "on switch $sw\n";
            SetLbgNumber($groupId, $sw, $lbgNumber);
        }
        else
        {
            print "Failed to create LBG $groupId on switch $sw\n";
            return $status;
        }
    }

    return $FM_OK;

}   # end tpHandleCreateLBG


##
# Deletes one or more load-balancing groups.
# 
# Syntax:
#   del lbg <lbgs>
# 
sub tpHandleDeleteLBG
{
    my ($self, @args) = @_;
    my $chip = $self->{'CHIP'};

    $self->SynchronizeLBGList();

    # Get switch selection.
    my @switchList = $self->tpGetSwitches();
    if (scalar(@switchList) > 1)
    {
        print $MSG_MULTI_SWITCH_INVALID;
        return $FM_FAIL;
    }
    my $sw = $switchList[0];

    # Check number of arguments.
    if (scalar(@args) > 1)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }
    elsif (scalar(@args) < 1)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }

    # Get <lbgs> parameter.
    my @lbgs = $self->ParseLbgParm($args[0], $PARSE_MULTIPLE | $PARSE_ALL);
    return $FM_ERR_INVALID_ARGUMENT if (!@lbgs);

    my @groupList = $self->GetLbgGroupList($sw, @lbgs);
    my $rtnStatus = $FM_OK;

    foreach my $groupId (@groupList)
    {
        my ($lbgSwitch, $lbgNumber) = GetLbgNumber($groupId);

        if (!defined($lbgNumber))
        {
            print "LBG $groupId not defined!\n";
            return $FM_FAIL;
        }

        if ($lbgSwitch != $sw)
        {
            print "LBG $groupId is not on selected switch\n";
            return $FM_FAIL;
        }

        SetLbgNumber($groupId, $sw, undef);

        $chip->disableErrors();
        my $status = $chip->fmDeleteLBG($sw, $lbgNumber);
        $chip->enableErrors();

        if ($status != $FM_OK)
        {
            print "Failed to delete LBG $groupId on switch $sw\n";
            $rtnStatus = $status;
        }

    }   # end foreach my $groupId (@groupList)

    return $rtnStatus;

}   # end tpHandleDeleteLBG


##
# Removes member ports from a load-balancing group.
# 
# Syntax:
#   del lbg port <lbg> <ports>
# 
sub tpHandleDeleteLBGPort
{
    my ($self, @args) = @_;
    my $chip = $self->{'CHIP'};

    $self->SynchronizeLBGList();

    # Get switch selection.
    my @switchList = $self->tpGetSwitches();
    if (scalar(@switchList) > 1)
    {
        print $MSG_MULTI_SWITCH_INVALID;
        return $FM_FAIL;
    }
    my $sw = $switchList[0];

    # Check number of arguments.
    if (scalar(@args) > 2)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }
    elsif (scalar(@args) < 2)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }

    # Get <lbg> parameter.
    my @lbgs = $self->ParseLbgParm($args[0]);
    return $FM_ERR_INVALID_ARGUMENT if (!@lbgs);

    my $rtnStatus = $FM_OK;

    foreach my $groupId (@lbgs)
    {
        my ($lbgSwitch, $lbgNumber) = GetLbgNumber($groupId);

        if (!defined($lbgNumber))
        {
            print "LBG $groupId not defined!\n";
            $rtnStatus = $FM_ERR_INVALID_ARGUMENT;
            next;
        }

        if ($lbgSwitch != $sw)
        {
            print "LBG $groupId is not on selected switch\n";
            return $FM_FAIL;
        }

        my @switchPortList =
            $self->validateExplicitList(
                $TRUE,
                $args[1],
                $self->tpPlatformGetSwitchPortList($sw, $FALSE));

        if (scalar(@switchPortList) == 0)
        {
            print "None of the ports are defined on switch $sw\n";
            next;
        }

        # Remove each specified port from LBG.
        for my $globalPort (@switchPortList)
        {
            # Skip if CPU port.
            next if $self->tpPlatformIsCPUPort($globalPort);

            # Map port number for the benefit of non-SWAG Vegas.
            my (undef, $localPort) = 
                $self->tpPlatformMapGlobalToLogicalPort($globalPort);

            $chip->disableErrors();
            my $status = $chip->fmDeleteLBGPort($sw, $lbgNumber, $localPort);
            $chip->enableErrors();

            if ($status != $FM_OK)
            {
                print "Failed to remove port $globalPort " . 
                      "from LBG $groupId on switch $sw\n";
                print $chip->fmErrorMsg($status) . "\n";
                $rtnStatus = $status;
            }
        }

    }   # end foreach my $groupId (@groupList)

    return $rtnStatus;

}   # end tpHandleDeleteLBGPort


##
# Sets a load-balancing group attribute.
# 
# Syntax:
#   set lbg config <lbg> <attr> <value>
# 
sub tpHandleSetLBGConfig
{
    my ($self, @args) = @_;
    my $chip = $self->{'CHIP'};

    $self->SynchronizeLBGList();

    # Get switch selection.
    my @switchList = $self->tpGetSwitches();
    if (scalar(@switchList) > 1)
    {
        print $MSG_MULTI_SWITCH_INVALID;
        return $FM_FAIL;
    }
    my $sw = $switchList[0];

    # Check number of arguments.
    if (scalar(@args) > 4)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }
    elsif (scalar(@args) < 3)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }

    # Get <lbg> parameter.
    my @lbgs = $self->ParseLbgParm($args[0]);
    return $FM_ERR_INVALID_ARGUMENT if (!@lbgs);

    # Get attribute name and value (%void)
    my $attrName = $args[1];
    my $attrValue = $args[2];

    my %void = (type => 'fm_int', value => 0);

    if ($attrName eq "redirect-method")
    {
        if ($attrValue eq "port")
        {
            $void{value} = $FM_LBG_REDIRECT_PORT;
        }
        elsif ($attrValue eq "standby")
        {
            $void{value} = $FM_LBG_REDIRECT_STANDBY;
        }
        elsif ($attrValue eq "all-ports")
        {
            $void{value} = $FM_LBG_REDIRECT_ALL_PORTS;
        }
        elsif ($attrValue eq "prefer-standby")
        {
            $void{value} = $FM_LBG_REDIRECT_PREFER_STANDBY;
        }
        else
        {
            print "'$attrValue' is not a valid $attrName\n";
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }
    elsif ($attrName eq "state")
    {
        if ($attrValue eq "active")
        {
            $void{value} = $FM_LBG_STATE_ACTIVE;
        }
        elsif ($attrValue eq "inactive")
        {
            $void{value} = $FM_LBG_STATE_INACTIVE;
        }
        else
        {
            print "'$attrValue' is not a valid $attrName\n";
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }
    elsif ($attrName eq "vlan")
    {
        $void{value} = $self->str2intnum($attrValue);
        if (!defined($void{value}))
        {
            print "'$attrValue' is not a valid $attrName\n";
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }
    elsif ($attrName eq "map")
    {

        my $ports = $args[3];
        my @globalPortList;
        my $status;
        
        if ((!defined($attrValue)) && (!defined($ports)))
        {
            print "'$attrValue' is not a valid $attrName\n";
            return $FM_ERR_INVALID_ARGUMENT;
        }

        my $groupId = $lbgs[0];
        
        my ($lbgSwitch, $lbgNumber) = GetLbgNumber($groupId);
        
        @globalPortList = $self->validateList($ports, 0, 0xfff);
        if (!defined($ports) || (scalar(@globalPortList) == 0))
        {
            print($TP_MSG_ERR_PORT_INVALID_ARRAY);
            return $FM_ERR_INVALID_ARGUMENT;
        }
        
        # Get the number of bins used by this LBG
        %void = (type => "fm_int", value => 0);
        $status = $chip->fmGetLBGAttribute($lbgSwitch,
                                           $lbgNumber,
                                           $FM_LBG_DISTRIBUTION_MAP_SIZE,
                                           \%void);
        return $status if ($status != $FM_OK);

        my $maxBins = $void{value};

        my @bins = $self->validateList($attrValue, 0, $maxBins - 1);

        my $lastBin = undef;
        my $currPort = 0;
        my $distMap = new SDK::fm_LBGDistributionMapRange();
        %void = (type => 'fm_LBGDistributionMapRange', value => $distMap);
        $distMap->{'firstBin'} = $bins[0];
        my @map = ();
        my $binIndex = 0;

        foreach my $bin (@bins)
        {
            if (defined($lastBin) && (($bin - 1) != $lastBin))
            {
                $distMap->{'numberOfBins'} = scalar(@map);
                my %value = ( type=>"fm_int[]", value => \@map);
                $status = $chip->fmSWIGSetLBGBins($lbgSwitch, $lbgNumber, \%void, \%value);
                return $status if ($status != $FM_OK);

                $distMap->{'firstBin'} = $bin;
                @map = ();
                $binIndex = 0;
            }
            my $port = $globalPortList[$currPort++];
            $map[$binIndex++] = $port;

            if ($currPort == scalar(@globalPortList))
            {
                $currPort = 0;
            }
            $lastBin = $bin;
        }
        $distMap->{'numberOfBins'} = scalar(@map);
        my %value = ( type=>"fm_int[]", value => \@map);
        $status = $chip->fmSWIGSetLBGBins($lbgSwitch, $lbgNumber, \%void, \%value);

        return $status;

    }
    else
    {
        print "'$attrName' is not a valid LBG attribute\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $rtnStatus = $FM_OK;

    foreach my $groupId (@lbgs)
    {
        my ($lbgSwitch, $lbgNumber) = GetLbgNumber($groupId);

        if (!defined($lbgNumber))
        {
            print "LBG $groupId not defined!\n";
            $rtnStatus = $FM_ERR_INVALID_ARGUMENT;
            next;
        }

        if ($lbgSwitch != $sw)
        {
            print "LBG $groupId is not on selected switch\n";
            return $FM_FAIL;
        }

        # Set specified attribute.
        my $status = $chip->fmSetLBGAttribute($sw,
                                              $lbgNumber,
                                              $lbg_attr_map{$attrName},
                                              \%void);

        if ($status != $FM_OK)
        {
            print "Unable to set $attrName for LBG $groupId " . 
                  "on switch $sw\n";
            return $FM_FAIL;
        }

    }   # end foreach my $groupId (@groupList)

    return $rtnStatus;

}   # end tpHandleSetLBGConfig


##
# Sets an LBG member port attribute.
# 
# Syntax:
#   set lbg port <lbg> <ports> <attr> <value>
# 
sub tpHandleSetLBGPort
{
    my ($self, @args) = @_;
    my $chip = $self->{'CHIP'};

    $self->SynchronizeLBGList();

    # Get switch selection.
    my @switchList = $self->tpGetSwitches();
    if (scalar(@switchList) > 1)
    {
        print $MSG_MULTI_SWITCH_INVALID;
        return $FM_FAIL;
    }
    my $sw = $switchList[0];

    # Check number of arguments.
    if (scalar(@args) > 4)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }
    elsif (scalar(@args) < 4)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }

    # Get <lbg> parameter.
    my @lbgs = $self->ParseLbgParm($args[0]);
    return $FM_ERR_INVALID_ARGUMENT if (!@lbgs);

    # Get attribute name and value (%void)
    my $attrName = $args[2];
    my $attrValue = $args[3];

    my %void = (type => 'fm_int', value => 0);

    if ($attrName eq "mode")
    {
        if ($attrValue eq "active")
        {
            $void{value} = $FM_LBG_PORT_ACTIVE;
        }
        elsif ($attrValue eq "failover")
        {
            $void{value} = $FM_LBG_PORT_FAILOVER;
        }
        elsif ($attrValue eq "standby")
        {
            $void{value} = $FM_LBG_PORT_STANDBY;
        }
        elsif ($attrValue eq "inactive")
        {
            $void{value} = $FM_LBG_PORT_INACTIVE;
        }
        else
        {
            print "'$attrValue' is not a value $attrName\n";
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }
    elsif ($attrName eq "redirect-target")
    {
        $void{value} = $self->str2intnum($attrValue);
        if (!defined($void{value}))
        {
            print "'$attrValue' is not a valid $attrName\n";
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }
    else
    {
        print "'$attrName' is not a valid LBG port attribute\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $rtnStatus = $FM_OK;

    foreach my $groupId (@lbgs)
    {
        my ($lbgSwitch, $lbgNumber) = GetLbgNumber($groupId);

        if (!defined($lbgNumber))
        {
            print "LBG $groupId not defined!\n";
            $rtnStatus = $FM_ERR_INVALID_ARGUMENT;
            next;
        }

        if ($lbgSwitch != $sw)
        {
            print "LBG $groupId is not on selected switch\n";
            return $FM_FAIL;
        }

        # Get <ports> parameter (@portList)
        my @memberList = $self->GetLBGMemberList($sw, $lbgNumber);

        my @portList = $self->validateExplicitList($TRUE,
                                                   $args[1],
                                                   @memberList);

        if (scalar(@portList) == 0)
        {
            print $TP_MSG_ERR_PORT_INVALID_ARRAY;
            $rtnStatus = $FM_ERR_INVALID_ARGUMENT;
            next;
        }

        # Set attribute for each port in list.
        foreach my $globalPort (@portList)
        {
            # Skip if CPU port.
            next if $self->tpPlatformIsCPUPort($globalPort);

            # Map port number for the benefit of non-SWAG Vegas.
            my (undef, $localPort) = 
                $self->tpPlatformMapGlobalToLogicalPort($globalPort);

            my $status = $chip->fmSetLBGPortAttribute(
                    $sw,
                    $lbgNumber,
                    $localPort,
                    $lbg_port_attr_map{$attrName},
                    \%void);

            if ($status != $FM_OK)
            {
                print "Unable to set $attrName for LBG $groupId " .
                      "port $globalPort on switch $sw\n";
                return $FM_FAIL;
            }

        }   # end foreach my $globalPort (@portList)

    }   # end foreach my $groupId (@groupList)

    return $rtnStatus;

}   # end tpHandleSetLBGPort


##
# Displays one or more load-balancing groups.
# 
# Syntax:
#   show lbg <lbgs>
# 
sub tpHandleShowLBG
{
    my ($self, @args) = @_;
    my $chip = $self->{'CHIP'};

    $self->SynchronizeLBGList();

    # Get switch selection.
    my @switchList = $self->tpGetSwitches();

    # Check number of arguments.
    if (scalar(@args) > 1)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }
    elsif (scalar(@args) < 1)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }

    # Get <lbgs> parameter.
    my @lbgs = $self->ParseLbgParm($args[0], $PARSE_MULTIPLE | $PARSE_ALL);
    return $FM_ERR_INVALID_ARGUMENT if (!@lbgs);

    for my $sw (@switchList)
    {
        print "\n";
        print "Switch $sw:\n\n" if $#switchList > 1;

        print "group  lport  state     vlan   redirect  member ports\n";
        print "-----  -----  --------  -----  --------  ------------\n";

        my @groupList = $self->GetLbgGroupList($sw, @lbgs);

        for my $groupId (@groupList)
        {
            $self->ShowLoadBalancingGroup($sw, $groupId);
        }
    }

    return $FM_OK;

}   # end tpHandleShowLBG

##
# Displays one or more load-balancing group maps.
# 
# Syntax:
#   show lbg  map <lbgs>
# 
sub tpHandleShowLBGMap
{
    my ($self, @args) = @_;
    my $chip = $self->{'CHIP'};

    $self->SynchronizeLBGList();

    # Get switch selection.
    my @switchList = $self->tpGetSwitches();

    # Check number of arguments.
    if (scalar(@args) > 1)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }
    elsif (scalar(@args) < 1)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }

    # Get <lbgs> parameter.
    my @lbgs = $self->ParseLbgParm($args[0], $PARSE_MULTIPLE | $PARSE_ALL);
    return $FM_ERR_INVALID_ARGUMENT if (!@lbgs);

    for my $lbg (@lbgs)
    {
        print "\n";
        print "Map for LBG $lbg " ;

        $self->ShowLoadBalancingGroupMap($lbg);
    }

    return $FM_OK;

}   # end tpHandleShowLBGMap



##
# Displays load-balancing member ports.
# 
# Syntax:
#   show lbg port <lbgs> <ports>
# 
sub tpHandleShowLBGPort
{
    my ($self, @args) = @_;
    my $chip = $self->{'CHIP'};

    $self->SynchronizeLBGList();

    # Get switch selection.
    my @switchList = $self->tpGetSwitches();

    # Check number of arguments.
    if (scalar(@args) > 2)
    {
        print "Too many arguments\n";
        return $FM_FAIL;
    }
    elsif (scalar(@args) < 2)
    {
        print "Too few arguments\n";
        return $FM_FAIL;
    }

    # Get <lbgs> parameter.
    my @lbgs = $self->ParseLbgParm($args[0], $PARSE_MULTIPLE | $PARSE_ALL);
    return $FM_ERR_INVALID_ARGUMENT if (!@lbgs);

    my $rtnStatus = $FM_OK;

    foreach my $sw (@switchList)
    {
        my @groupList = $self->GetLbgGroupList($sw, @lbgs);

        foreach my $groupId (@groupList)
        {
            my ($lbgSwitch, $lbgNumber) = GetLbgNumber($groupId);

            if (!defined($lbgNumber))
            {
                print "Switch $sw: " if $#switchList > 1;
                print "LBG $groupId not defined!\n";
                $rtnStatus = $FM_ERR_INVALID_ARGUMENT;
                next;
            }

            # Get <ports> parameter (@portList)
            my @memberList = $self->GetLBGMemberList($sw, $lbgNumber);

            my @portList = $self->validateExplicitList($TRUE,
                                                       $args[1],
                                                       @memberList);

            if (scalar(@portList) == 0)
            {
                print $TP_MSG_ERR_PORT_INVALID_ARRAY;
                $rtnStatus = $FM_ERR_INVALID_ARGUMENT;
                next;
            }

            print "\nLBG $groupId (logical port $lbgNumber) on switch $sw\n\n";

            print "port  local  mode        target\n";
            print "----  -----  ----------  ------\n";

            # Display attributes of each port in list.
            foreach my $globalPort (@portList)
            {
                my $status;
                my %void;

                # Skip if CPU port.
                next if $self->tpPlatformIsCPUPort($globalPort);

                # Map port number for the benefit of non-SWAG Vegas.
                my (undef, $localPort) = 
                    $self->tpPlatformMapGlobalToLogicalPort($globalPort);
                print "skipping port $globalPort ($localPort)\n" if $localPort <= 0;
                next if $localPort <= 0;

                # mode
                my $mode;

                %void = (type => "fm_int", value => 0);
                $status = $chip->fmGetLBGPortAttribute($sw,
                                                       $lbgNumber,
                                                       $localPort,
                                                       $FM_LBG_PORT_MODE,
                                                       \%void);
                last if ($status != $FM_OK);

                if ($void{value} == $FM_LBG_PORT_ACTIVE)
                {
                    $mode = "active";
                }
                elsif ($void{value} == $FM_LBG_PORT_FAILOVER)
                {
                    $mode = "failover";
                }
                elsif ($void{value} == $FM_LBG_PORT_STANDBY)
                {
                    $mode = "standby";
                }
                elsif ($void{value} == $FM_LBG_PORT_INACTIVE)
                {
                    $mode = "inactive";
                }
                else
                {
                    $mode = sprintf("unknown(%d)", $void{value});
                }

                # redirect-target
                %void = (type => "fm_int", value => 0);
                $status = $chip->fmGetLBGPortAttribute($sw,
                                                       $lbgNumber,
                                                       $localPort,
                                                       $FM_LBG_PORT_REDIRECT_TARGET,
                                                       \%void);
                if ($status != $FM_OK)
                {
                    $rtnStatus = $status;
                    last;
                }

                my $target = $void{value};

                printf("%4d  %5d  %-10s  %4d\n",
                       $globalPort,
                       $localPort,
                       $mode,
                       $target);

            }   # end foreach my $globalPort (@portList)

        }   # end foreach my $groupId (@groupList)

    }   # end foreach my $sw (@switchList)

    return $rtnStatus;

}   # end tpHandleShowLBGPort

1;
