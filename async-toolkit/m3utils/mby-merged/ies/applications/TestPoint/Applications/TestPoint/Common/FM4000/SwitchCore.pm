# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FM4000/SwitchCore.pm
# Creation Date:    12/03/07
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

package Applications::TestPoint::Common::FM4000::SwitchCore;
use strict;
use warnings;

use List::Util qw(max min);

use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;
use SDKScalars;
use Types::tp_priorityMapper;
use Types::tp_priMap;

###############################################################################
#
#                           LOCAL VARIABLES
#
###############################################################################

##@var private hash[] _hog
my @_hog =
(
    #      TC         |     WM (kB)
    # ----------------+-------------
    { 'tc' => [0 .. 2], 'wm' => 10 },
    { 'tc' => [3 .. 5], 'wm' => 10 },
    { 'tc' => [6],      'wm' => 10 },
    { 'tc' => [7],      'wm' => 10 },
);

##@var private hash _scheduler
my $_scheduler =
{
    #         TC         |     Algorithm      | DRR Weight (MTU)
    # -------------------+--------------------+-----------------
    'cpu'   =>
    [
        { 'tc' => [0 .. 5], 'type' => 'drr',    'weight' => 0  },
        { 'tc' => [6],      'type' => 'drr',    'weight' => 5  },
        { 'tc' => [7],      'type' => 'drr',    'weight' => 10 },
    ],
};

##@var private hash _shaping
my $_shaping =
{
    #          TC        |      Capacity (B)  |     Bandwidth (Mb/s)
    # -------------------+--------------------+-----------------------
    'cpu'   =>
    [
        { 'tc' => [0 .. 7], 'capacity' => 100E3,    'bandwidth' => 2 },
    ],
};

##@var private hash[] _tc
my @_tc =
(
    #      TC         |     WM (kB)
    # ----------------+-------------
    { 'tc' => [0],      'wm' => 10 },
    { 'tc' => [1 .. 5], 'wm' => 0  },
    { 'tc' => [6],      'wm' => 6  },
    { 'tc' => [7],      'wm' => 4  },
);

##@var private <int,int> _vpriSwPriMap
my %_vpriSwPriMap = map {$_ => max(0, ($_ & ~0x1) / 2 - 1)} 0 .. 15;

##@var private <int,int> _swPriTcMap
my %_swPriTcMap = map {$_ => $_ % 8} 0 .. 15;

##@var private Types::tp_priorityMapper** _priorityMappers
my @_priorityMappers = ();

##@var private <char,Types::tp_priMap> __trapClasses
my %__trapClasses  =
(
    'bpdu'      => Types::tp_priMap->new('bpdu', $SDK::FM_QOS_TRAP_CLASS_BPDU),
    'cpu_mac'   => Types::tp_priMap->new('cpu_mac',
                                         $SDK::FM_QOS_TRAP_CLASS_CPU_MAC),
    'garp'      => Types::tp_priMap->new('garp', $SDK::FM_QOS_TRAP_CLASS_GARP),
    'ieee'      => Types::tp_priMap->new('ieee', $SDK::FM_QOS_TRAP_CLASS_IEEE),
    'icmp'      => Types::tp_priMap->new('icmp', $SDK::FM_QOS_TRAP_CLASS_ICMP),
    'igmp'      => Types::tp_priMap->new('igmp', $SDK::FM_QOS_TRAP_CLASS_IGMP),
    'ip_option' => Types::tp_priMap->new('ip_option',
                                         $SDK::FM_QOS_TRAP_CLASS_IP_OPTION),
    'lacp'      => Types::tp_priMap->new('lacp', $SDK::FM_QOS_TRAP_CLASS_LACP),
    '802.1x'    => Types::tp_priMap->new('802.1x',
                                         $SDK::FM_QOS_TRAP_CLASS_802_1X),
);

##@var private <char,Types::tp_priMap>[] _trapClasses
my @_trapClasses = ();

##@var private <char,int> _trapClassAttrs
my %_trapClassAttrs =
(
    'priority'  => $TRUE,
);

###############################################################################
#
#                           LOCAL FUNCTIONS
#
###############################################################################

##@cmethod private int
#          TP4000AddPriMapperMap(int                      switchNum,
#                                Types::tp_priorityMapper &mapper,
#                                Types::tp_priMap         &map)
sub TP4000AddPriMapperMap
{
    my $self                            = shift(@_);
    my $switchNum                       = shift(@_);
    my Types::tp_priorityMapper $mapper = shift(@_);
    my Types::tp_priMap $map            = shift(@_);

    my $chip = $self->{'CHIP'};

    my ($status);

    # Ensure that the specified priority map does not yet exist.
    if (exists($mapper->{'maps'}->{$map->{'id'}}))
    {
        return $SDK::FM_ERR_ALREADY_EXISTS;
    }
    # Add the priority map to the list of priority maps.
    $mapper->{'maps'}->{$map->{'id'}} = $map;
    # Modify the priority mapper.
    return $self->TP4000ApplyPriMapperMaps($switchNum, $mapper);
}

##@cmethod private int
#          TP4000AllocatePriorityMapper(int                      switchNum,
#                                       int                      priority,
#                                       Types::tp_priorityMapper &mapper)
sub TP4000AllocatePriorityMapper
{
    my $self                            = shift(@_);
    my $switchNum                       = shift(@_);
    my $priority                        = shift(@_);
    my Types::tp_priorityMapper $mapper = shift(@_);

    my $chip = $self->{'CHIP'};

    my ($status);

    # Properly initialize the _priorityMappers data structure.
    if (!isArray($_priorityMappers[$switchNum]))
    {
        $_priorityMappers[$switchNum] = [];
    }
    # Allocate a trigger.
    $status = $chip->fm4000AllocTrigger($switchNum,
                                        \$mapper->{'resource'},
                                        $SDK::FALSE);
    if ($status != $FM_OK)
    {
        return $FM_FAIL;
    }
    # Store the switch priority the priority mapper is mapping all matching
    # frames to.
    $mapper->{'priority'} = $priority;
    # Configure the trigger to map all matching frames to the specified switch
    # priority.
    my $action = SDK::fm_triggerAction->new();
    $action->{'triggerActionCfg1'}->{'switchPriAction'} = $SDK::TRUE;
    $action->{'triggerActionCfg2'}->{'newSwitchPri'} = $mapper->{'priority'};
    $status = $chip->fm4000SetTriggerActionConfig($switchNum,
                                                  $mapper->{'resource'},
                                                  $action);
    if ($status != $FM_OK)
    {
        return $FM_FAIL;
    }
    # Store a reference to the priority mapper that has been allocated.
    $_priorityMappers[$switchNum]->[$mapper->{'priority'}] = $mapper;
    return $FM_OK;
}

##@cmethod private int
#          TP4000ApplyPriMapperMaps(int                      switchNum,
#                                   Types::tp_priorityMapper &mapper)
sub TP4000ApplyPriMapperMaps
{
    my $self                            = shift(@_);
    my $switchNum                       = shift(@_);
    my Types::tp_priorityMapper $mapper = shift(@_);

    my $chip = $self->{'CHIP'};

    my ($condition);

    # Create a new set of trigger conditions starting from the default set of
    # conditions.
    $self->SetDefaultConditions($switchNum, \$condition, $SDK::TRUE);
    while (my ($type, $map) = each(%{$mapper->{'maps'}}))
    {
        $condition->{'triggerConditionAmask'} |= $map->{'id'};
    }
    # Store the new set of trigger conditions.
    my $status = $chip->fm4000SetTriggerConditionConfig($switchNum,
                                                        $mapper->{'resource'},
                                                        $condition);
    if ($status != $FM_OK)
    {
        return $FM_FAIL;
    }
    return $FM_OK;
}

##@cmethod private int TP4000SetCpuProtectionScheduler(int switchNum)
#
# @brief        Configures the scheduler
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub TP4000SetCpuProtectionScheduler
{
    my ($self, $switchNum) = @_;

    my $chip = $self->{'CHIP'};
    my $mtu = 10240;

    my @portList = $self->validateList('all',
                                       $self->tpPlatformGetFaceplatePortRange);
    PORT: foreach my $globalPort (@portList)
    {
        my ($status);

        # Map port number for the benefit of non-SWAG Vegas. Ignore the
        # returned sw since it should match switchNum and may actually be
        # wrong for some platforms.
        my ($sw, $port) = $self->tpPlatformMapGlobalToLogicalPort($globalPort);

        my $schedulerGroups = $_scheduler->{'other'};
        my $shapingGroups = $_shaping->{'other'};
        if ($self->tpPlatformIsCPUPort($globalPort))
        {
            $schedulerGroups = $_scheduler->{'cpu'};
            $shapingGroups = $_shaping->{'cpu'};
        }

        if (defined($schedulerGroups))
        {
            my $numGroups = min(scalar(@{$schedulerGroups}), 8);

            # Create a set of scheduler groups.
            $status = $self->tpHandleSchedNumGroups($globalPort, $numGroups);
            return $status if ($status != $FM_OK);

            for (my $group = 0; $group < $numGroups; $group++)
            {
                # Assign traffic classes to each scheduler group.
                my $classes = join(',', @{$schedulerGroups->[$group]->{'tc'}});
                $status = $self->tpHandleSchedTrafficClass($globalPort,
                                                           $group, 
                                                           $classes);
                return $status if ($status != $FM_OK);

                # Configure each scheduler group.
                SWITCH: for ($schedulerGroups->[$group]->{'type'})
                {
                    $_ eq 'drr' && do
                    {
                        # Make the scheduler group a DRR group.
                        $status = $self->tpHandleSchedStrict($globalPort,
                                                              $group,
                                                              "off");
                        return $status if ($status != $FM_OK);

                        my $weight = $schedulerGroups->[$group]->{'weight'};
                        # Convert the DRR weight from units of MTU into units
                        # of bytes.
                        $weight *= $mtu;
                        # Assign a DRR weight to the scheduler group.
                        $status = $self->tpHandleSchedWeight($globalPort,
                                                              $group,
                                                              $weight);
                        return $status if ($status != $FM_OK);
                        last SWITCH;
                    };

                    $_ eq 'strict' && do
                    {
                        # Make the scheduler group strict.
                        $status = $self->tpHandleSchedStrict($globalPort,
                                                              $group,
                                                              "on");
                        return $status if ($status != $FM_OK);
                        last SWITCH;
                    };

                    warn(sprintf("%s: Unknown scheduler group configuration",
                                 $schedulerGroups->[$group]->{'type'}));
                }
            }
            # Apply the above configured scheduler configuration.
            $status = $self->tpHandleSchedApply($globalPort);
            return $status if ($status != $FM_OK);
        }

        if (defined($shapingGroups))
        {
            my $numGroups = min(scalar(@{$shapingGroups}), 8);

            # Create a set of bandwidth shaping groups.
            $status = $self->tpHandleShapingNumGroups($globalPort, $numGroups);
            return $status if ($status != $FM_OK);

            for (my $group = 0; $group < $numGroups; $group++)
            {
                # Assign traffic classes to each bandwidth shaping group.
                my $classes = join(',', @{$shapingGroups->[$group]->{'tc'}});
                $status = $self->tpHandleShapingTrafficClass($globalPort,
                                                             $group,
                                                             $classes);
                return $status if ($status != $FM_OK);

                # Set the token bucket capacity for each bandwidth shaping
                # group. Note that the capacity determines the maximum burst
                # the egress rate limiter can support.
                my $capacity = $shapingGroups->[$group]->{'capacity'};
                $status = $self->tpHandleShapingCapacity($globalPort,
                                                         $group,
                                                         $capacity);
                return $status if ($status != $FM_OK);

                # Set the bandwidth for each bandwidth shaping group.
                my $bandwidth = $shapingGroups->[$group]->{'bandwidth'};
                $status = $self->tpHandleShapingBandwidth($globalPort,
                                                          $group,
                                                          $bandwidth);
                return $status if ($status != $FM_OK);
            }
            # Apply the above configured shaping configuration.
            $status = $self->tpHandleShapingApply($globalPort);
            return $status if ($status != $FM_OK);
        }
    }
    return $FM_OK;
}

##@cmethod private int TP4000SetCpuProtectionWatermarks(int switchNum)
#
# @brief        Configures the CPU port watermarks for an FM4000 device
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub TP4000SetCpuProtectionWatermarks
{
    my ($self, $switchNum) = @_;

    my $chip = $self->{'CHIP'};

    # Disable automatic watermark management.
    my %void = (type => "fm_bool", value => $FM_DISABLED);
    my $status = $chip->fmSetSwitchQOS($switchNum, $FM_AUTO_PAUSE_MODE, 0,
                                       \%void);
    return $FM_FAIL if ($status != $FM_OK);

    # Configure the TX hog watermark map. Note that this is a switch wide
    # configuration setting.
    SLOT: for (my $i = 0; $i < min(scalar(@_hog), 4); $i++)
    {
        TC: foreach my $tc (@{$_hog[$i]->{'tc'}})
        {
            my %void = (type => "fm_int", value => $tc);
            my $status = $chip->fmSetSwitchQOS($switchNum,
                                               $FM_QOS_TX_HOG_WM_MAP,
                                               $i, \%void);
            return $FM_FAIL if ($status != $FM_OK);
        }
    }

    my @portList = $self->validateList('all',
                                       $self->tpPlatformGetFaceplatePortRange);
    PORT: foreach my $globalPort (@portList)
    {
        if (!$self->tpPlatformIsCPUPort($globalPort))
        {
            next PORT;
        }

        # Map port number for the benefit of non-SWAG Vegas. Ignore the
        # returned sw since it should match switchNum and may actually be
        # wrong for some platforms.
        my ($sw, $logicalPort) =
                          $self->tpPlatformMapGlobalToLogicalPort($globalPort);

        # Configure the per port TX hog watermarks for the CPU port.
        HOG_WM: for (my $i = 0; $i < min(scalar(@_hog), 4); $i++)
        {
            my %void = (type => "fm_int", value => 1024 * $_hog[$i]->{'wm'});
            my $status = $chip->fmSetPortQOS($switchNum, $logicalPort,
                                             $FM_QOS_TX_HOG_WM, $i, \%void);
            return $FM_FAIL if ($status != $FM_OK);
        }

        # Configure the per port TX traffic class watermarks for the CPU port.
        TC_WM: for (my $i = 0; $i < min(scalar(@_tc), 8); $i++)
        {
            my %void = (type => "fm_int", value => 1024 * $_tc[$i]->{'wm'});
            TC: foreach my $tc (@{$_tc[$i]->{'tc'}})
            {
                my $status = $chip->fmSetPortQOS($switchNum, $logicalPort,
                                                 $FM_QOS_TX_TC_PRIVATE_WM,
                                                 $tc, \%void);
                return $FM_FAIL if ($status != $FM_OK);
            }
        }

        # Configure the per port RX private watermark for the CPU port.
        my %void = (type => "fm_int", value => 1024 * 10);
        my $status = $chip->fmSetPortQOS($switchNum, $logicalPort,
                                         $FM_QOS_RX_PRIVATE_WM, 0, \%void);
        return $FM_FAIL if ($status != $FM_OK);
    }

    # XXX: 3 kB of switch memory will be allocated for traffic class 7 on all
    # non-CPU ports.
    PORT: foreach my $logicalPort (1 .. 24)
    {
        my %void = (type => "fm_int", value => 1024 * 3);
        my $status = $chip->fmSetPortQOS($switchNum, $logicalPort,
                                         $FM_QOS_TX_TC_PRIVATE_WM, 7, \%void);
        return $FM_FAIL if ($status != $FM_OK);
    }

    return $FM_OK;
}

##@cmethod private int TP4000SetCpuProtectionPriorityMaps(int switchNum)
sub TP4000SetCpuProtectionPriorityMaps
{
    my ($self, $switchNum) = @_;

    my $chip = $self->{'CHIP'};

    my ($status);

    # Reserve traffic class 7 for the exclusive use of the CPU.
    while (my ($vpri, $swPri) = each(%_vpriSwPriMap))
    {
        my %void = (type => "fm_int", value => $swPri);
        $status = $chip->fmSetSwitchQOS($switchNum, $FM_QOS_VPRI_SWPRI_MAP,
                                        $vpri, \%void);
        return $status if ($status != $FM_OK);
    }

    # Install a new switch priority to traffic class map.
    while (my ($swPri, $tc) = each(%_swPriTcMap))
    {
        my %void = (type => "fm_int", value => $swPri);
        $status = $chip->fmSetSwitchQOS($switchNum, $FM_QOS_SWPRI_TC_MAP,
                                        $swPri, \%void);
        return $status if ($status != $FM_OK);
    }

    # Remap all BPDU, LACP and 802.1x frames to switch priority 15.
    foreach my $trapClass ('bpdu', 'lacp', '802.1x')
    {
        $status = $self->tp4000HandleSetTrapClass($switchNum, $trapClass,
                                                  'priority', 15);
        return $status if ($status != $FM_OK);
    }

    # Remap the following set of frames to switch priority 14:
    #   (i)     IGMP frames
    #   (ii)    ICMP frames with TTL <= 1
    #   (iii)   IP frames with TTL <= 1
    #   (iv)    Unrecognized slow protocol frames
    #   (v)     CPU addressed frames
    foreach my $trapClass ('igmp', 'icmp', 'ip_option', 'ieee', 'cpu_mac')
    {
        $status = $self->tp4000HandleSetTrapClass($switchNum, $trapClass,
                                                  'priority', 14);
        return $status if ($status != $FM_OK);
    }
}

##@cmethod private int
#          TP4000DeletePriMapperMap(int                      switchNum,
#                                   Types::tp_priorityMapper &mapper,
#                                   Types::tp_priMap         &map)
sub TP4000DeletePriMapperMap
{
    my $self                            = shift(@_);
    my $switchNum                       = shift(@_);
    my Types::tp_priorityMapper $mapper = shift(@_);
    my Types::tp_priMap $map            = shift(@_);

    my $chip = $self->{'CHIP'};

    my ($status);

    # Check for existence of the specified priority map.
    if (!exists($mapper->{'maps'}->{$map->{'id'}}))
    {
        return $SDK::FM_ERR_NOT_FOUND;
    }
    # Retrieve the set of trigger conditions.
    my $condition = SDK::fm_triggerCondition->new();
    $status = $chip->fm4000GetTriggerConditionConfig($switchNum,
                                                     $mapper->{'resource'},
                                                     $condition);
    if ($status != $FM_OK)
    {
        return $FM_FAIL;
    }
    # Store the modified set of trigger conditions.
    $condition->{'triggerConditionAmask'} &= ~$map->{'id'};
    $status = $chip->fm4000SetTriggerConditionConfig($switchNum,
                                                     $mapper->{'resource'},
                                                     $condition);
    if ($status != $FM_OK)
    {
        return $FM_FAIL;
    }
    # Delete the priority map from the list of priority maps associated with
    # the specified priority mapper.
    delete($mapper->{'maps'}->{$map->{'id'}});
    return $FM_OK;
}

##@cmethod private int
#          TP4000FreePriorityMapper(int                      switchNum,
#                                   Types::tp_priorityMapper &mapper)
sub TP4000FreePriorityMapper
{
    my $self                            = shift(@_);
    my $switchNum                       = shift(@_);
    my Types::tp_priorityMapper $mapper = shift(@_);

    my $chip = $self->{'CHIP'};

    # Free the trigger that associated with the specified priority mapper.
    my $status = $chip->fm4000FreeTrigger($switchNum, $mapper->{'resource'});
    if ($status != $FM_OK)
    {
        return $FM_FAIL;
    }
    # Mark the priority mapper as being freed.
    $_priorityMappers[$switchNum]->[$mapper->{'priority'}] = undef;
    # Free all memory associated with the specified priority mapper.
    undef($mapper);
    return $FM_OK;
}

##@cmethod private int SetDefaultConditions(int                      switchNum,
#                                           SDK::fm_triggerCondition condition,
#                                           bool                     trapped)
#
# @brief        Initializes a fm_triggerCondition structure
#
# @param[in]    switchNum the switch number for which the fm_triggerCondition
#               structure is to be initialized
#
# @param[in]    condition points to the fm_triggerCondition structure to be
#               initialized
#
# @param[in]    trapped boolean indicating whether the fm_triggerCondition
#               structure should be initialized for trapped frames (TRUE) or
#               for non-trapped frames (FALSE)
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub SetDefaultConditions
{
    my ($self, $switchNum, $condition, $trapped) = @_;

    my $chip = $self->{'CHIP'};

    undef(${$condition});
    # The memory backing the fm_triggerCondition structure, that is created in
    # the below statement, is allocated using calloc. As a result, its contents
    # can safely assumed to be zero.
    ${$condition} = SDK::fm_triggerCondition->new();

    foreach my $field ('matchSA',
                       'matchDA',
                       'matchHitSA',
                       'matchHitDA',
                       'matchHitSADA',
                       'matchVlan',
                       'matchFFU',
                       'matchSwitchPri',
                       'matchEtherType',
                       'matchDestGlort')
    {
        # Configure all matching conditions to match unconditionally.
        ${$condition}->{'triggerConditionCfg'}->{$field} =
                                         $TRIGGER_MATCHCASE_MATCHUNCONDITIONAL;
    }
    # The PRNG threshold does not have to be set, since the
    # matchRandomIfLess field is set to zero, i.e. the PRNGs are configured
    # to generate a match whenever the pseudo random value is equal to or
    # greater than 2 ^ matchRandomThreshold.

    my $conditionParam = ${$condition}->{'triggerConditionParam'};
    # Match on unicast, broadcast and multicast frames.
    $conditionParam->{'frameClassMask'} = $FM4000_TRIGGER_COND_UCST
                                          | $FM4000_TRIGGER_COND_BCST
                                          | $FM4000_TRIGGER_COND_MCST;
    # Match on both switched and routed frames.
    $conditionParam->{'routedMask'} = $FM4000_TRIGGER_COND_SWITCHED
                                      | $FM4000_TRIGGER_COND_ROUTED;
    # Match on all frame types except special delivery and in-band management
    # frames.
    $conditionParam->{'ftypeMask'} = $FM4000_TRIGGER_COND_FTYPE_NORMAL
                                     | $FM4000_TRIGGER_COND_FTYPE_ROUTED;

    my @portList = $self->validateList('all',
                                       $self->tpPlatformGetFaceplatePortRange);
    PORT: foreach my $globalPort (@portList)
    {
        my ($logicalPort, $physPort, $sw);

        # Map port number for the benefit of non-SWAG Vegas. Ignore the
        # returned sw since it should match switchNum and may actually be
        # wrong for some platforms.
        ($sw, $logicalPort) =
                          $self->tpPlatformMapGlobalToLogicalPort($globalPort);

        my $status = $chip->fmPlatformMapLogicalPortToPhysical($switchNum,
                                                               $logicalPort,
                                                               \$sw,
                                                               \$physPort);
        return $FM_FAIL if ($status != $FM_OK);

        if ($self->tpPlatformIsCPUPort($globalPort))
        {
            if ($trapped)
            {
                $conditionParam->{'matchDroppedFrames'} = $TRUE;
            }
            else
            {
                ${$condition}->{'triggerConditionTx'} |= 1 << $physPort;
            }
        }
        else
        {
            ${$condition}->{'triggerConditionRx'} |= 1 << $physPort;
        }
    }
    return $FM_OK;
}

##@cmethod private int
#          TP4000ValidatePriorityMapper(int                      switchNum,
#                                       int                      priority,
#                                       Types::tp_priorityMapper &mapper)
sub TP4000ValidatePriorityMapper
{
    my $self                            = shift(@_);
    my $switchNum                       = shift(@_);
    my $priority                        = shift(@_);
    my Types::tp_priorityMapper $mapper = shift(@_);

    if (!isArray($_priorityMappers[$switchNum])
        || !defined($_priorityMappers[$switchNum]->[$priority]))
    {
        return $FM_ERR_NOT_FOUND;
    }
    ${$mapper} = $_priorityMappers[$switchNum]->[$priority];
    return $FM_OK;
}

###############################################################################
#
#                           PUBLIC FUNCTIONS
#
###############################################################################

##@cmethod public int tp4000HandleSetCPUProtection(char *state)
#
# @brief        Handles enabling CPU protection for a FM4000 device
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tp4000HandleSetCPUProtection
{
    my ($self, $switchNum, $state) = @_;

    if ($state eq 'on')
    {
        my ($status);

        $status = $self->TP4000SetCpuProtectionPriorityMaps($switchNum);
        return $status if ($status != $FM_OK);

        $status = $self->TP4000SetCpuProtectionWatermarks($switchNum);
        return $status if ($status != $FM_OK);

        $status = $self->TP4000SetCpuProtectionScheduler($switchNum);
        return $status if ($status != $FM_OK);
    }
    return $FM_OK;
}

##@method public int tp4000HandleSetTrapClass(int  switchNum,
#                                             char *trapClass,
#                                             char *attribute,
#                                             char *parameter)
sub tp4000HandleSetTrapClass
{
    my ($self, $switchNum, $trapClass, $attribute, $parameter) = @_;

    my $chip = $self->{'CHIP'};

    if (!isHash($_trapClasses[$switchNum]))
    {
        $_trapClasses[$switchNum] = {%__trapClasses};
    }

    if (!exists($_trapClasses[$switchNum]->{$trapClass}))
    {
        print($TP_MSG_ERR_TRAPPING_INVALID_CLASS);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    if (!exists($_trapClassAttrs{$attribute}))
    {
        print($TP_MSG_ERR_TRAPPING_INVALID_ATTR);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    SWITCH: for ($attribute)
    {
        $_ eq 'priority' && do
        {
            my ($priority, $status);

            # Ensure that a valid switch priority has been provided.
            if (!defined(($priority = $self->str2intnum($parameter)))
                || scalar(@{[$self->validateList($priority, 0, 15)]}) != 1)
            {
                print($TP_MSG_ERR_TRAPPING_INVALID_PARAM);
                return $FM_ERR_INVALID_ARGUMENT;
            }

            my Types::tp_priMap $map = $_trapClasses[$switchNum]->{$trapClass};

            my %void = (type => "fm_uint32", value => $priority);

            $status = $chip->fmSetSwitchQOS($switchNum,
                                            $SDK::FM_QOS_TRAP_CLASS_SWPRI_MAP,
                                            $map->{'id'},
                                            \%void);

            if ($status != $FM_OK)
            {
                print("Could not set trap class priority!\n");
                return $FM_FAIL;
            }

            last SWITCH;
        };
    }

    return $FM_OK;
}

1;
