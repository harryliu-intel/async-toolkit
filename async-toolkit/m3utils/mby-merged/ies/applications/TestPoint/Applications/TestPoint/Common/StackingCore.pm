# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/StackingCore.pm
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

package Applications::TestPoint::Common::StackingCore;
use strict;
use warnings;

use List::Util qw(sum);
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;


sub tpCreateForwardingRule
{
    my ($self, $glort, $mask, $port) = @_;
    my $chip = $self->{CHIP};
    my $status = $FM_OK;

    my $rule = new SDK::fm_forwardRule();

    $rule->{'glort'} = $glort;
    $rule->{'mask'} = $mask;

    my @globalPortList = ($port);
    my @affectedPortList = ();

    my $switchCount = scalar(@{[$self->tpGetSwitches]});

    # This command only make sense when invoked along with switch select
    # for one switch at a time. 

    if ($switchCount != 1)
    {
        print("This command should be invoked with one selected switch\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    foreach my $sw ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port,
                        $self->tpPlatformGetLogicalPortList($sw,"local,lag")); 

        if (scalar(@portList) == 0)
        {
            next;
        }
        
        push(@affectedPortList, @portList);

        $rule->{'logicalPort'} = $port;

        my $id = -1;

        $status = $chip->fmCreateStackForwardingRule($sw, \$id, $rule);
                                    
        if ($status == $FM_OK)
        {
            printf("Created forwarding rule with ID $id %s\n",
                   ($switchCount > 0) ?
                   "on switch $sw" :
                   "");
        }
        else
        {
            printf("Unable to create forwarding rule%s!\n",
                   ($switchCount > 0) ?
                   " on switch $sw" :
                   "");
        }
    }
    # Warn user if some ports were not operated on.
    #$self->ReportMissedPorts(\@affectedPortList, \@globalPortList);
}

sub tpDeleteForwardingRule
{
    my ($self, $id) = @_;
    my $chip = $self->{CHIP};

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $sw ($self->tpGetSwitches)
    {
        my $status = $chip->fmDeleteStackForwardingRule($sw, $id);
                                    
        if ($status == $FM_OK)
        {
            printf("Deleted forwarding rule%s\n",
                   ($switchCount > 0) ?
                   " on switch $sw" :
                   "");
        }
        else
        {
            printf("Unable to delete forwarding rule%s!\n",
                   ($switchCount > 0) ?
                   " on switch $sw" :
                   "");
        }
    }
}

sub tpShowForwardingRules
{
    my ($self) = @_;
    my $chip = $self->{CHIP};

    $chip->disableErrors();

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $sw ($self->tpGetSwitches)
    {
        printf("\nForwarding Rules%s\n",
               ($switchCount > 0) ? " On Switch $sw" : "");
        printf("-----------------------------\n");

        my $cID = -1;
        my $rID = -1;
        my $rule = new SDK::fm_forwardRule();

        my $status = $chip->fmGetStackForwardingRuleFirst($sw, \$rID, $rule);

        if (($status != $FM_OK) && ($status != $FM_ERR_NO_FORWARDING_RULES))
        {
            printf("ERROR: %s\n", fmErrorMsg($status));
            $chip->enableErrors();
            return;
        }
                                    
        while ($status == $FM_OK)
        {
            if ($status == $FM_OK)
            {
                printf("#%d : 0x%04x/0x%04x => %d\n",
                       $rID,
                       $rule->{'glort'},
                       $rule->{'mask'},
                       $rule->{'logicalPort'});
            }

            $cID = $rID;

            $status = $chip->fmGetStackForwardingRuleNext($sw, $cID, \$rID, $rule);

            if (($status != $FM_OK) && ($status != $FM_ERR_NO_FORWARDING_RULES))
            {
                printf("ERROR: $status : %s\n", fmErrorMsg($status));
                $chip->enableErrors();
                return;
            }
                                    
        }

        printf("\n");
    }

    $chip->enableErrors();
}

sub tpSetStackGlortRange
{
    my ($self, $switchNum, $glortBase, $mask) = @_;
    my $chip = $self->{CHIP};
    my $testpoint = $self->{TESTPOINT};
    my $platform = $self->{PLATFORM};
    my $state;
    my $status;

    $chip->fmGetSwitchState($switchNum, \$state);

    if ($state == $FM_ENABLED)
    {
        printf("Bringing switch down ...\n");
        $status = $chip->fmSetSwitchState($switchNum, $FM_DISABLED);

        if ($status != $FM_OK)
        {
            printf("ERROR: %s\n", fmErrorMsg($status));
            return;
        }
    }

    $status = $chip->fmSetStackGlortRange($switchNum, $glortBase, $mask);

    if ($status != $FM_OK)
    {
        printf("ERROR: %s\n", fmErrorMsg($status));
    }

    if ($state == $FM_ENABLED)
    {
        printf("Bringing switch up ...\n");
        $status = $chip->fmSetSwitchState($switchNum, $FM_ENABLED);

        if ($status != $FM_OK)
        {
            printf("ERROR: %s\n", fmErrorMsg($status));
            return;
        }

        # load the startup file using the TestPoint instance itself
        if ($switchNum == 0)
        {
            if($testpoint->{SKIP} == 0)
            {
                # RC loaded in expert mode
                $testpoint->loadFile($testpoint->{OPTS}->{RC}, "expert");
            }
        }
    }
    
}

sub tpShowStackGlortRange
{
    my ($self) = @_;
    my $chip = $self->{CHIP};

    my $glortBase = -1;
    my $mask      = -1;

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status = $chip->fmGetStackGlortRange($switchNum, 
                    \$glortBase, \$mask);

        if (($status != $FM_OK) && ($status != $FM_ERR_NO_MORE))
        {
            printf("ERROR: %s\n", fmErrorMsg($status));
            return;
        }

        printf("Switch %d : Glort Range 0x%04x/0x%04x\n",
               $switchNum,$glortBase,$mask);
    }
}

sub tpCreateStackLogicalPort
{
    my ($self, $glort,) = @_;
    my $chip = $self->{CHIP};
    my $status = $FM_OK;

    my $switchCount = scalar(@{[$self->tpGetSwitches]});

    foreach my $sw ($self->tpGetSwitches)
    {
        my $lport = -1;
        $status = $chip->fmCreateStackLogicalPort($sw, $glort, \$lport);
                                    
        if ($status == $FM_OK)
        {
            printf("Switch $sw Logical Port $lport -> $glort\n"); 
        }
        else
        {
            printf("ERROR: %s\n", fmErrorMsg($status));
            printf("Unable to create logical port%s!\n",
                   ($switchCount > 0) ?
                   " on switch $sw" :
                   "");
        }
    }
}

sub tpDeleteStackLogicalPort
{
    my ($self, $logicalPort,) = @_;
    my $chip = $self->{CHIP};
    my $status = $FM_OK;

    my $switchCount = scalar(@{[$self->tpGetSwitches]});

    foreach my $sw ($self->tpGetSwitches)
    {
        my $lport = -1;
        $status = $chip->fmDeleteStackLogicalPort($sw, $logicalPort);
                                    
        if ($status == $FM_OK)
        {
            printf("Switch $sw Logical Port $logicalPort deleted\n");
        }
        else
        {
            printf("ERROR: %s\n", fmErrorMsg($status));
            printf("Unable to delete logical port%s!\n",
                   ($switchCount > 0) ?
                   " on switch $sw" :
                   "");
        }
    }
}

sub tpShowStackGlort
{
    my ($self, $port) = @_;

    my $chip = $self->{CHIP};

    my @globalPortList = $self->validateList($port, (0,65536));
    if (!defined($port) || (($port ne "all") && (scalar(@globalPortList) == 0)))
    {
        print("Must specify a valid logical port or port range!\n");
        return;
    }

    print("\n");
    print(" SW  LOGICAL PORT   GLORT   PORT TYPE\n");
    print("---- ------------ --------- ---------\n");

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    my @affectedPortList = ();

    foreach my $switchNum ($self->tpGetSwitches)
    {
        if ($port eq "all")
        {
            @globalPortList = $self->validateList($port, 
                                $self->tpPlatformGetLogicalPortList($switchNum, 
                                                                   "all"));
        }
        my @portList = $self->validateExplicitList($TRUE, $port,
                                $self->tpPlatformGetLogicalPortList($switchNum, 
                                                                   "all"));
        if (scalar(@portList) == 0)
        {
            next;
        }
            
        my $p = $port;
        foreach my $p (@portList)
        {
            my $glort = 0;
            my $status = $chip->fmGetStackGlort($switchNum, $p, \$glort);
            my $ptype = "";
            $chip->fmGetLogicalPortType($switchNum, $p, \$ptype);

            if($ptype == $FM_PORT_TYPE_PHYSICAL)
            {
                $ptype = "Local";
            }
            elsif ($ptype == $FM_PORT_TYPE_LAG)
            {
                $ptype = "LAG";
            }
            elsif ($ptype == $FM_PORT_TYPE_LBG)
            {
                $ptype = "LBG";
            }
            elsif ($ptype == $FM_PORT_TYPE_MULTICAST)
            {
                $ptype = "MC";
            }
            elsif ($ptype == $FM_PORT_TYPE_SPECIAL)
            {
                $ptype = "Special";
            }
            elsif ($ptype == $FM_PORT_TYPE_CPU)
            {
                $ptype = "CPU";
            }
            elsif ($ptype == $FM_PORT_TYPE_CPU_MGMT)
            {
                $ptype = "MGMT";
            }
            elsif ($ptype == $FM_PORT_TYPE_REMOTE)
            {
                $ptype = "Remote";
            }
            else
            {
                $ptype = "?";
            }

            if (($status != $FM_OK) && ($status != $FM_ERR_NO_MORE))
            {
                printf("ERROR: %s\n", fmErrorMsg($status));
            } else {
                printf("%04x %12d    0x%04x %s\n", 
                       $switchNum, $p, $glort, $ptype);
            }
        }
    }
}

sub tpShowLogicalPort
{
    my ($self, $glorts) = @_;

    my $chip = $self->{CHIP};

    my @glortList = $self->validateList($glorts, (0,65536));
    if (!defined($glorts) || (($glorts ne "all") && (scalar(@glortList) == 0)))
    {
        print("Must specify a valid glort or glort range!\n");
        return;
    }

    print("\n");
    print(" SW  GLORT  LOGICAL PORT   PORT TYPE\n");
    print("---- ----- --------------- ---------\n");

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    my @affectedPortList = ();

    foreach my $switchNum ($self->tpGetSwitches)
    {
        if ($glorts eq "all")
        {
            my $glortBase = -1;
            my $mask = -1;
            my $status = $chip->fmGetStackGlortRange($switchNum, 
                        \$glortBase, \$mask);

            if (($status != $FM_OK) && ($status != $FM_ERR_NO_MORE))
            {
                printf("ERROR: %s\n", fmErrorMsg($status));
                return;
            }

            @glortList = $self->validateList($glorts, $glortBase, $glortBase+$mask);
        }
        my $chip = $self->{CHIP};
        foreach my $glort (@glortList)
        {
            my $p = -1;
            $chip->disableErrors();
            my $status = $chip->fmGetGlortLogicalPort($switchNum, $glort, \$p);
            $chip->enableErrors();
            if ($status == $FM_ERR_INVALID_PORT) 
            {
                next;
            }
            if ($status != $FM_OK)
            {
                printf("ERROR: %s\n", fmErrorMsg($status));
                next;
            }
            my $ptype = "";
            $chip->fmGetLogicalPortType($switchNum, $p, \$ptype);

            if($ptype == $FM_PORT_TYPE_PHYSICAL)
            {
                $ptype = "Local";
            }
            elsif ($ptype == $FM_PORT_TYPE_LAG)
            {
                $ptype = "LAG";
            }
            elsif ($ptype == $FM_PORT_TYPE_LBG)
            {
                $ptype = "LBG";
            }
            elsif ($ptype == $FM_PORT_TYPE_MULTICAST)
            {
                $ptype = "MC";
            }
            elsif ($ptype == $FM_PORT_TYPE_SPECIAL)
            {
                $ptype = "Special";
            }
            elsif ($ptype == $FM_PORT_TYPE_CPU)
            {
                $ptype = "CPU";
            }
            elsif ($ptype == $FM_PORT_TYPE_CPU_MGMT)
            {
                $ptype = "MGMT";
            }
            elsif ($ptype == $FM_PORT_TYPE_REMOTE)
            {
                $ptype = "Remote";
            }
            else
            {
                $ptype = "?";
            }

            if (($status != $FM_OK) && ($status != $FM_ERR_NO_MORE))
            {
                printf("ERROR: %s\n", fmErrorMsg($status));
            } else {
                printf("%4d 0x%04x %12d    %-s\n", 
                       $switchNum, $glort, $p, $ptype);
            }
        }
    }
}

sub tpSetStackInternalPort
{
    my ($self, $port) = @_;
    my $chip = $self->{CHIP};

    my @globalPortList = $self->validateList($port, $self->tpPlatformGetPortRange());
    if (!defined($port) || (scalar(@globalPortList) == 0))
    {
        print("Must specify a valid port or port range!\n");
        return;
    }

    my @affectedPortList = ();

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @portList = $self->validateExplicitList($TRUE, $port,
                       $self->tpPlatformGetSwitchPortList($switchNum, $TRUE));
            
        if (scalar(@portList) == 0)
        {
            # None of the ports appear on this switch
            next;
        }
        
        push(@affectedPortList, @portList);

        foreach my $p (@portList) {
            my %void = (type => 'fm_bool', value => $FM_ENABLED);
            my $status = $chip->fmSetPortAttribute($switchNum, $p,
                                                   $FM_PORT_INTERNAL, \%void);
            if ($status != $FM_OK)
            {
                printf("Port internal cannot be set for port %d on switch %d\n", 
                       $p, $switchNum);
                return;
            }
        }
    }

    # Warn user if some ports were not operated on.
    $self->ReportMissedPorts(\@affectedPortList, \@globalPortList);


    $self->handleSetPortConfig ($port, "isl", "F64");
    $self->handleSetPortConfig ($port, "update_routed", "disable");
    $self->handleSetPortConfig ($port, "ifg", "8");
    $self->handleSetPortConfig ($port, "dic", "disable");

    print("Setting tagging on all vlans on $port\n");
    $chip->disableErrors();
    my (@vlanIDs);

    my $status = $self->GetVlanList(\@vlanIDs);
    foreach my $v (@vlanIDs) {
        $self->tpHandleAddVlanPort($v, $port);
        $self->tpHandleSetVlanSpt( $v, $port, "forwarding");
        $self->tpHandleSetVlanTag( $v, $port, "tag");
    }
    $chip->enableErrors();

}

1;
