# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/StormControllerCore.pm
# Creation Date:    11/12/07
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

##@class Applications::TestPoint::Common::StormControllerCore
#
# @code
#   # To allow the creation of an arbitrary storm controller, the TestPoint
#   # storm controller ID is mapped to the SDK storm controller ID using an
#   # internal TestPoint mapping function.
#   create storm-ctrl <storm controller>
#   set storm-ctrl <storm controller> <attribute> <parameter>
#   add storm-ctrl condition <storm controller> <condition> [<parameter>]
#   add storm-ctrl action <storm controller> <action> [<action>]
#   show storm-ctrl <storm controller>
#   show storm-ctrl stats <storm controller>
# @endcode
#
# @code
#   reset storm-ctrl stats <storm controller>
#   del storm-ctrl condition <storm controller> <condition> [<parameter>]
#   del storm-ctrl action <storm controller> <condition> [<parameter>]
#   del storm-ctrl <storm controller>
# @endcode
package Applications::TestPoint::Common::StormControllerCore;
use strict;
use warnings;

use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

###############################################################################
#
#                           CONSTANTS & TYPES
#
###############################################################################

##@def TP_MAX_NUM_STORM_COND
use constant TP_MAX_NUM_STORM_COND          => $FM_STORM_COND_MAX + 50;

##@def TP_MAX_NUM_STORM_ACTIONS
use constant TP_MAX_NUM_STORM_ACTION        => $FM_STORM_ACTION_MAX + 25;

##@def TP_STORM_CTRL_INVALID
use constant TP_STORM_CTRL_INVALID          => -1;

use constant TP_STORM_CTRL_ADD_ACTION       => 1;
use constant TP_STORM_CTRL_ADD_COND         => 2;
use constant TP_STORM_CTRL_DELETE_ACTION    => 101;
use constant TP_STORM_CTRL_DELETE_COND      => 102;

###############################################################################
#
#                           LOCAL VARIABLES
#
###############################################################################

##@var private int** __stormController
my $__stormController = [];

##@var private SDK::fm_stormAction[]& __stormActions
my $__stormActions =
[
    map {SDK::fm_stormAction->new()} (1 .. TP_MAX_NUM_STORM_ACTION)
];

##@var private SDK::fm_stormCondition[]& __stormConditions
my $__stormConditions =
[
    map {SDK::fm_stormCondition->new()} (1 .. TP_MAX_NUM_STORM_COND)
];

#@var private Map<char*, int> __stormCtrlActions
my %__stormCtrlActions =
(
    'ignore'        => $FM_STORM_ACTION_DO_NOTHING,
    'filter_port'   => $FM_STORM_ACTION_FILTER_PORT,
);

##@var private Map<char*, int> __stormCtrlAttrs
my %__stormCtrlAttrs =
(
    'capacity'      => $FM_STORM_CAPACITY,
    'rate'          => $FM_STORM_RATE,
);

##@var private Map<char*, int> __stormCtrlConds
my %__stormCtrlConds =
(
    '802.1X'        => $FM_STORM_COND_802_1X,
    'bpdu'          => $FM_STORM_COND_BPDU,
    'broadcast'     => $FM_STORM_COND_BROADCAST,
    'cpu'           => $FM_STORM_COND_CPU,
    'egress_port'   => $FM_STORM_COND_EGRESS_PORT,
    'flood'         => $FM_STORM_COND_FLOOD,
    'flood_ucast'   => $FM_STORM_COND_FLOOD_UCAST,
    'flood_mcast'   => $FM_STORM_COND_FLOOD_MCAST,
    'fid'           => $FM_STORM_COND_FIDFORWARD,
    'fid_ucast'     => $FM_STORM_COND_FIDFORWARD_UCAST,
    'fid_mcast'     => $FM_STORM_COND_FIDFORWARD_MCAST,
    'trap_icmp'     => $FM_STORM_COND_TRAP_ICMP,
    'log_icmp'      => $FM_STORM_COND_LOG_ICMP,
    'igmp'          => $FM_STORM_COND_IGMP,
    'ingress_port'  => $FM_STORM_COND_INGRESS_PORT,
    'lacp'          => $FM_STORM_COND_LACP,
    'multicast'     => $FM_STORM_COND_MULTICAST,
    'unicast'       => $FM_STORM_COND_UNICAST,
    'nexthop_miss'  => $FM_STORM_COND_NEXTHOP_MISS,
);

###############################################################################
#
#                           LOCAL FUNCTIONS
#
###############################################################################

##@cmethod private int AllocateStormController(int switchNum, int virtualCtrl)
#
# @brief        Allocates a virtual storm controller identifier
#
# @param[in]    switchNum The switch number for which to allocate a virtual
#               storm controller identifier
#
# @param[in]    virtualCtrl The virtual storm controller identifier
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub AllocateStormController
{
    my ($self, $switchNum, $virtualCtrl) = @_;

    my $tempory;
    if ($self->ValidateStormController($switchNum,
                                       $virtualCtrl,
                                       \$tempory    ) == $FM_OK)
    {
        return $FM_FAIL;
    }
    if (!isArray($__stormController->[$switchNum]))
    {
        $__stormController->[$switchNum] = [];
    }
    $__stormController->[$switchNum]->[$virtualCtrl] = TP_STORM_CTRL_INVALID;
    return $FM_OK;
}

##@cmethod private int FreeStormController(int switchNum, int virtualCtrl)
#
# @brief        Frees a virtual storm controller identifier
#
# @param[in]    switchNum The switch number for which to free a virtual storm
#               controller identifier
#
# @param[in]    virtualCtrl The virtual storm controller
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub FreeStormController
{
    my ($self, $switchNum, $virtualCtrl) = @_;

    my $tempory;
    if ($self->ValidateStormController($switchNum,
                                       $virtualCtrl,
                                       \$tempory    ) != $FM_OK)
    {
        return $FM_FAIL;
    }
    $__stormController->[$switchNum]->[$virtualCtrl] = TP_STORM_CTRL_INVALID;
    return $FM_OK;
}

##@cmethod private int ModifyStormControllerAction(int  modification,
#                                                  int  virtualCtrl,
#                                                  char *condition,
#                                                  int  parameter)
sub ModifyStormControllerAction
{
    my ($self, $modification, $virtualCtrl, $action, $parameter) = @_;

    my $chip = $self->{'CHIP'};

    if (!defined($virtualCtrl) || !defined($action))
    {
        print("Must specify <storm controller> <action>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $stormCtrl = [];
    foreach my $switchNum ($self->tpGetSwitches)
    {
        if ($self->ValidateStormController($switchNum,
                                           $virtualCtrl,
                                           \$stormCtrl->[$switchNum]) != $FM_OK)
        {
            print($TP_MSG_ERR_STORM_CTRL_INVALID_CTRL);
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }

    if (!exists($__stormCtrlActions{$action}))
    {
        print($TP_MSG_ERR_STORM_CTRL_INVALID_ACTION);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
    my $stormAction = SDK::fm_stormAction->new();
    $stormAction->{'type'} = $__stormCtrlActions{$action};
    ACTION: for ($action)
    {
        my ($status);

        $_ eq 'filter_port' && do
        {
            my @portList;
            if ($parameter eq "all")
            {
                @portList = (-1);
            }
            else
            {
                @portList = 
                   $self->validateList($parameter,
                                       $self->tpPlatformGetFaceplatePortRange);
            }
            if (scalar(@portList) == 0)
            {
                print($TP_MSG_ERR_PORT_INVALID_ARRAY);
                return $FM_ERR_INVALID_ARGUMENT;
            }

            foreach my $globalPort (@portList)
            {
               # Operate on each selected switch, one at a time
               foreach my $switchNum ($self->tpGetSwitches)
               {
                    # Convert $port to an array of ports that appear only on this switch
                    my @portList2 = $self->validateExplicitList($TRUE, 
                                                               $globalPort, 
                                                               $self->tpPlatformGetSwitchPortList($switchNum));
                    
                    # If none of the ports appear on this switch, go to the next switch
                    if ((scalar(@portList2) == 0) && ( $globalPort != -1))
                    {
                        next;
                    }

                    # Map port number for the benefit of non-SWAG Vegas. Ignore the
                    # returned sw since it should match switchNum and may actually be
                    # wrong for some platforms.
                    my ($sw, $logicalPort) = $self->tpPlatformMapGlobalToLogicalPort($globalPort);
                    my $ctrl = $stormCtrl->[$switchNum];
                    $stormAction->{'param'} = $logicalPort;
                    MODIFICATION: for ($modification)
                    {
                        $_ == TP_STORM_CTRL_ADD_ACTION && do
                        {
                            $status = $chip->fmAddStormCtrlAction($switchNum,
                                                                  $ctrl,
                                                                  $stormAction);
                            last MODIFICATION;
                        };
                        
                        $_ == TP_STORM_CTRL_DELETE_ACTION && do
                        {
                            $status = $chip->fmDeleteStormCtrlAction($switchNum,
                                                                     $ctrl,
                                                                     $stormAction);
                            last MODIFICATION;
                        };
    
                        do
                        {
                            warn("%d: Unknown modification type", $modification);
                            $result = $FM_FAIL;
                        };
                    
                        $result = $status != $FM_OK ? $FM_FAIL : $result;
                    }
                }
            }

            last ACTION;
        };

        do
        {
            foreach my $switchNum ($self->tpGetSwitches)
            {
                my $ctrl = $stormCtrl->[$switchNum];
                MODIFICATION: for ($modification)
                {
                    $_ == TP_STORM_CTRL_ADD_ACTION && do
                    {
                        $status = $chip->fmAddStormCtrlAction($switchNum,
                                                              $ctrl,
                                                              $stormAction);
                        last MODIFICATION;
                    };

                    $_ == TP_STORM_CTRL_DELETE_COND && do
                    {
                        $status = $chip->fmDeleteStormCtrlAction($switchNum,
                                                                 $ctrl,
                                                                 $stormAction);
                        last MODIFICATION;
                    };

                    do
                    {
                        warn("%d: Unknown modification type", $modification);
                        $result = $FM_FAIL;
                    };
                }
                $result = $status != $FM_OK ? $FM_FAIL : $result;
            }
        }
    }
    return $result;
}

##@cmethod private int ModifyStormControllerCondition(int   modification,
#                                                     int   virtualCtrl,
#                                                     char  *condition,
#                                                     int   parameter)
sub ModifyStormControllerCondition
{
    my ($self, $modification, $virtualCtrl, $condition, $parameter) = @_;

    my $chip = $self->{'CHIP'};

    if (!defined($virtualCtrl) || !defined($condition))
    {
        print("Must specify <storm controller> <condition>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $stormCtrl = [];
    foreach my $switchNum ($self->tpGetSwitches)
    {
        if ($self->ValidateStormController($switchNum,
                                           $virtualCtrl,
                                           \$stormCtrl->[$switchNum]) != $FM_OK)
        {
            print($TP_MSG_ERR_STORM_CTRL_INVALID_CTRL);
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }

    if (!exists($__stormCtrlConds{$condition}))
    {
        print($TP_MSG_ERR_STORM_CTRL_INVALID_COND);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
    my $stormCondition = SDK::fm_stormCondition->new();
    $stormCondition->{'type'} = $__stormCtrlConds{$condition};
    CONDITION: for ($condition)
    {
        my ($status);

        (($_ eq 'ingress_port')  || ($_ eq 'egress_port' )) && do
        {
            my @portList;
            if ($parameter eq "all")
            {
                @portList = (-1);
            }
            else
            {
                @portList = 
                   $self->validateList($parameter,
                                       $self->tpPlatformGetFaceplatePortRange);
            }
            if (scalar(@portList) == 0)
            {
                print($TP_MSG_ERR_PORT_INVALID_ARRAY);
                return $FM_ERR_INVALID_ARGUMENT;
            }

            foreach my $globalPort (@portList)
            {
               # Operate on each selected switch, one at a time
               foreach my $switchNum ($self->tpGetSwitches)
               {
                    # Convert $port to an array of ports that appear only on this switch
                    my @portList2 = $self->validateExplicitList($TRUE, 
                                                               $globalPort, 
                                                               $self->tpPlatformGetSwitchPortList($switchNum));
                    
                    # If none of the ports appear on this switch, go to the next switch
                    if ((scalar(@portList2) == 0) && ( $globalPort != -1))
                    {
                        next;
                    }
       
                    # Map port number for the benefit of non-SWAG Vegas. Ignore the
                    # returned sw since it should match switchNum and may actually be
                    # wrong for some platforms.
                    my ($sw, $logicalPort) = $self->tpPlatformMapGlobalToLogicalPort($globalPort);
                    my $ctrl = $stormCtrl->[$switchNum];
                    $stormCondition->{'param'} = $logicalPort;
                    MODIFICATION: for ($modification)
                    {
                             $_ == TP_STORM_CTRL_ADD_COND && do
                        {
                            $status = 
                                   $chip->fmAddStormCtrlCondition($switchNum,
                                                                  $ctrl,
                                                                  $stormCondition);
                            last MODIFICATION;
                        };
                        
                        $_ == TP_STORM_CTRL_DELETE_COND && do
                        {
                            $status = 
                                $chip->fmDeleteStormCtrlCondition($switchNum,
                                                                  $ctrl,
                                                                  $stormCondition);
                            last MODIFICATION;
                        };
    
                        do
                        {
                            warn("%d: Unknown modification type", $modification);
                            $result = $FM_FAIL;
                        };
                        
                        $result = $status != $FM_OK ? $FM_FAIL : $result;
                    }
                }
            }

            last CONDITION;
        };

        do
        {
            foreach my $switchNum ($self->tpGetSwitches)
            {
                my $ctrl = $stormCtrl->[$switchNum];
                MODIFICATION: for ($modification)
                {
                    $_ == TP_STORM_CTRL_ADD_COND && do
                    {
                        $status = 
                               $chip->fmAddStormCtrlCondition($switchNum,
                                                              $ctrl,
                                                              $stormCondition);
                        last MODIFICATION;
                    };

                    $_ == TP_STORM_CTRL_DELETE_COND && do
                    {
                        $status = 
                            $chip->fmDeleteStormCtrlCondition($switchNum,
                                                              $ctrl,
                                                              $stormCondition);
                        last MODIFICATION;
                    };

                    do
                    {
                        warn("%d: Unknown modification type", $modification);
                        $result = $FM_FAIL;
                    };
                }
                $result = $status != $FM_OK ? $FM_FAIL : $result;
            }
        }
    }
    return $result;
}

##@cmethod private int ValidateStormController(int  switchNum,
#                                              int  virtualCtrl,
#                                              int  stormCtrl)
#
# @brief        Validates a storm controller
#
# @param[in]    switchNum The switch number for which the storm controller is
#               to be validated
#
# @param[in]    virtualCtrl The virtual storm controller identifier
#
# @param[out]   stormCtrl Points to user-allocated storage in which the logical
#               storm controller is to be stored
#
# @return       FM_OK if successful
# @return       FM_ERR_NO_STORM_CONTROLLERS if the storm controller cannot be
#               validated 
# @return       FM_ERR_INVALID_ARGUMENT if an invalid virtual storm controller
#               identifier has been supplied
sub ValidateStormController
{
    my ($self, $switchNum, $virtualCtrl, $stormCtrl) = @_;

    if (!defined(($virtualCtrl = $self->str2intnum($virtualCtrl))))
    {
        return $FM_ERR_INVALID_ARGUMENT;
    }
    if (!defined($__stormController->[$switchNum])
        || !defined($__stormController->[$switchNum]->[$virtualCtrl])
        || $__stormController->[$switchNum]->[$virtualCtrl] < 0)
    {
        return $FM_ERR_NO_STORM_CONTROLLERS
    }
    ${$stormCtrl} = $__stormController->[$switchNum]->[$virtualCtrl];
    return $FM_OK;
}

###############################################################################
#
#                           PUBLIC FUNCTIONS
#
###############################################################################

##@cmethod public int tpHandleAddStormControllerAction(int  virtualCtrl,
#                                                      char *action,
#                                                      int  parameter)
#
# @brief        Handles adding a storm controller action
#
# @param[in]    virtualCtrl The virtual storm controller identifier
#
# @param[in]    action The storm controller action to be added
#
# @param[in]    parameter The storm controller action parameter
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleAddStormControllerAction
{
    my ($self, $virtualCtrl, $action, $parameter) = @_;

    return $self->ModifyStormControllerAction(TP_STORM_CTRL_ADD_ACTION,
                                              $virtualCtrl,
                                              $action,
                                              $parameter);
}

##@cmethod public int tpHandleAddStormControllerCondition(int   virtualCtrl,
#                                                         char  *condition,
#                                                         int   parameter)
#
# @brief        Handles adding storm controller conditions
#
# @param[in]    virtualCtrl The virtual storm controller identifier
#
# @param[in]    condition The storm controller condition to be added
#
# @param[in]    parameter The storm controller condition parameter
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleAddStormControllerCondition
{
    my ($self, $virtualCtrl, $condition, $parameter) = @_;

    return $self->ModifyStormControllerCondition(TP_STORM_CTRL_ADD_COND,
                                                 $virtualCtrl,
                                                 $condition,
                                                 $parameter);
}

##@cmethod public int tpHandleCreateStormController(int virtualCtrl)
#
# @brief        Handles creating a storm controller
#
# @param[in]    virtualCtrl The virtual storm controller identifier
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleCreateStormController
{
    my ($self, $virtualCtrl) = @_;

    my $chip = $self->{'CHIP'};
    my $switchCount = scalar(@{[$self->tpGetSwitches]});

    if (!defined($virtualCtrl))
    {
        print("Must specify <storm controller>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    # Ensure that $virtualCtrl is an integer number.
    if (!defined(($virtualCtrl = $self->str2intnum($virtualCtrl))))
    {
        print($TP_MSG_ERR_STORM_CTRL_INVALID_CTRL);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    # For all switches ensure that $virtualCtrl points to a unused storm
    # controller identifier.
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my ($temporary);

        my $status = $self->ValidateStormController($switchNum, $virtualCtrl,
                                                    \$temporary);
        if ($status != $FM_ERR_NO_STORM_CONTROLLERS)
        {
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            print($TP_MSG_ERR_STORM_CTRL_INVALID_CTRL);
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }

    my $result = $FM_OK;
    SWITCH: foreach my $switchNum ($self->tpGetSwitches)
    {
        my ($status);

        $status = $self->AllocateStormController($switchNum, $virtualCtrl);

        if ($status == $FM_OK)
        {
            $chip->disableErrors();
            $status = $chip->fmCreateStormCtrl($switchNum,
                            \$__stormController->[$switchNum]->[$virtualCtrl]);
            $chip->enableErrors();
        }

        if ($status != $FM_OK)
        {
            $result = $FM_FAIL;
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            printf("Storm controller %d cannot be created!\n", $virtualCtrl);
            next SWITCH;
        }
    }
    return $result;
}

##@cmethod public int tpHandleDelStormController(int virtualCtrl)
#
# @brief        Handles deleting a storm controller
#
# @param[in]    virtualCtrl The virtual storm controller identifier
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleDelStormController
{
    my ($self, $virtualCtrl) = @_;

    my $chip = $self->{'CHIP'};
    my $switchCount = scalar(@{[$self->tpGetSwitches]});

    if (!defined($virtualCtrl))
    {
        print("Must specify <storm controller>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $stormCtrl = [];
    foreach my $switchNum ($self->tpGetSwitches)
    {
        if ($self->ValidateStormController($switchNum,
                                           $virtualCtrl,
                                           \$stormCtrl->[$switchNum]) != $FM_OK)
        {
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            print($TP_MSG_ERR_STORM_CTRL_INVALID_CTRL);
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }

    my $result = $FM_OK;
    SWITCH: foreach my $switchNum ($self->tpGetSwitches)
    {
        my $status = $chip->fmDeleteStormCtrl($switchNum,
                                              $stormCtrl->[$switchNum]);
        if ($status != $FM_OK)
        {
            $result = $FM_FAIL;
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            printf("Cannot delete storm controller %d!\n",
                   $stormCtrl->[$switchNum]);
            next SWITCH;
        }
        $self->FreeStormController($switchNum, $virtualCtrl);
    }
    return $result;
}

##@cmethod public int tpHandleDelStormControllerAction(int  virtualCtrl,
#                                                      char *action,
#                                                      int  parameter)
#
# @brief        Handles deleting a storm controller action
#
# @param[in]    virtualCtrl The virtual storm controller identifier
#
# @param[in]    action The storm controller action to be deleted
#
# @param[in]    parameter The storm controller action parameter
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleDelStormControllerAction
{
    my ($self, $virtualCtrl, $action, $parameter) = @_;

    return $self->ModifyStormControllerAction(TP_STORM_CTRL_DELETE_ACTION,
                                              $virtualCtrl,
                                              $action,
                                              $parameter);
}

##@cmethod public int tpHandleDelStormControllerCondition(int   virtualCtrl,
#                                                         char  *condition,
#                                                         int   parameter)
#
# @brief        Handles deleting a storm controller condition
#
# @param[in]    virtualCtrl The virtual storm controller identifier
#
# @param[in]    condition The storm controller condition to be deleted
#
# @param[in]    parameter The storm controller condition parameter
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleDelStormControllerCondition
{
    my ($self, $virtualCtrl, $condition, $parameter) = @_;

    return $self->ModifyStormControllerCondition(TP_STORM_CTRL_DELETE_COND,
                                                 $virtualCtrl,
                                                 $condition,
                                                 $parameter);
}

##@cmethod public int tpHandleResetStormControllerStats(int virtualCtrl)
#
# @brief        Handles resetting storm controller statistics
#
# @param[in]    virtualCtrl The virtual storm controller identifier
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleResetStormControllerStats
{
    my ($self, $virtualCtrl) = @_;

    my $chip = $self->{'CHIP'};

    if (!defined($virtualCtrl))
    {
        print("Must specify <storm controller>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $stormCtrl = [];
    foreach my $switchNum ($self->tpGetSwitches)
    {
        if ($self->ValidateStormController($switchNum,
                                           $virtualCtrl,
                                           \$stormCtrl->[$switchNum]) != $FM_OK)
        {
            print($TP_MSG_ERR_STORM_CTRL_INVALID_CTRL);
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    SWITCH: foreach my $switchNum ($self->tpGetSwitches)
    {
        my %void = (type => "fm_uint64", value => Math::BigInt->new(0));
        $chip->disableErrors();
        my $status = $chip->fmSetStormCtrlAttribute($switchNum,
                                                    $stormCtrl->[$switchNum],
                                                    $FM_STORM_COUNT, \%void);
        $chip->enableErrors();
        if ($status != $FM_OK)
        {
            $result = $FM_FAIL;
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            printf(  "Statistics counters associated with storm controller %d"
                   . " cannot be reset!\n",
                   $virtualCtrl);
            next SWITCH;
        }
    }
    return $result;
}

##@cmethod public int tpHandleSetStormController(int    virtualCtrl,
#                                                char   *attribute,
#                                                int    value)
#
# @brief        Handles setting a storm controller attribute
#
# @param[in]    virtualCtrl The virtual storm controller identifier
#
# @param[in]    attribute The attribute to be set
#
# @param[in]    value The attribute value
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleSetStormController
{
    my ($self, $virtualCtrl, $attribute, $value) = @_;

    my $chip = $self->{'CHIP'};

    if (!defined($virtualCtrl) || !defined($attribute) || !defined($value))
    {
        print("Must specify <storm controller> <attribute> <value>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $stormCtrl = [];
    foreach my $switchNum ($self->tpGetSwitches)
    {
        if ($self->ValidateStormController($switchNum,
                                           $virtualCtrl,
                                           \$stormCtrl->[$switchNum]) != $FM_OK)
        {
            print($TP_MSG_ERR_STORM_CTRL_INVALID_CTRL);
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }

    if (!exists($__stormCtrlAttrs{$attribute}))
    {
        print($TP_MSG_ERR_STORM_CTRL_INVALID_ATTR);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    SWITCH: for ($attribute)
    {
        ($_ eq 'capacity' || $_ eq 'rate') && do
        {
            if (!defined($value = $self->str2intnum($value)))
            {
                print($TP_MSG_ERR_STORM_CTRL_INVALID_PARAM);
                return $FM_ERR_INVALID_ARGUMENT;
            }

            my %void = (type => "fm_uint32", value => $value);
            $attribute = $__stormCtrlAttrs{$attribute};
            foreach my $switchNum ($self->tpGetSwitches)
            {
                my $status = 
                       $chip->fmSetStormCtrlAttribute($switchNum,
                                                      $stormCtrl->[$switchNum],
                                                      $attribute, \%void);
                $result = $status != $FM_OK ? $FM_FAIL : $result;
            }
            last SWITCH;
        };
    }
    return $result;
}

##@cmethod public int tpHandleShowStormController(int virtualCtrl)
#
# @brief        Handles showing the configuration of the specified storm
#               controller
#
# @param[in]    virtualCtrl The virtual storm controller identifier
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleShowStormController
{
    my ($self, $virtualCtrl) = @_;

    my $chip = $self->{'CHIP'};
    my $result = $FM_OK;

    my ($status);

    if (!defined($virtualCtrl))
    {
        print("Must specify <storm controller>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $stormCtrl = [];
    foreach my $switchNum ($self->tpGetSwitches)
    {
        if ($self->ValidateStormController($switchNum,
                                           $virtualCtrl,
                                           \$stormCtrl->[$switchNum]) != $FM_OK)
        {
            print($TP_MSG_ERR_STORM_CTRL_INVALID_CTRL);
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }

    print("\n");
    printf("Storm controller state: Controller %d\n", $virtualCtrl);
    print("------------------------------------------------------------\n");

    my $switchNum = shift(@{[$self->tpGetSwitches]});
    my %void = (type => "fm_uint32", value => 0);
    $status = $chip->fmGetStormCtrlAttribute($switchNum,
                                             $stormCtrl->[$switchNum],
                                             $FM_STORM_CAPACITY, \%void);
    if ($status == $FM_OK)
    {
        printf("%-21s %9d B\n", "capacity", $void{'value'});
    }
    $result = $status != $FM_OK ? $FM_FAIL : $result;
    $status = $chip->fmGetStormCtrlAttribute($switchNum,
                                             $stormCtrl->[$switchNum],
                                             $FM_STORM_RATE, \%void);
    if ($status == $FM_OK)
    {
        printf("%-21s %9d kb/s\n", "rate", $void{'value'});
    }
    $result = $status != $FM_OK ? $FM_FAIL : $result;

    my %conditions = ();
    foreach my $sw ($self->tpGetSwitches)
    {
        my ($numConditions, $status);

        $status = $chip->fmGetStormCtrlConditionList($sw, $stormCtrl->[$sw],
                                                     \$numConditions,
                                                     $__stormConditions,
                                                     TP_MAX_NUM_STORM_COND);
        if ($status == $FM_OK)
        {
            my %tpConds =
                     map {$__stormCtrlConds{$_} => $_} keys(%__stormCtrlConds);
            for (my $i = 0; $i < $numConditions; $i++)
            {
                my $condition = $tpConds{$__stormConditions->[$i]->{'type'}};
                if (!exists($conditions{$condition}))
                {
                    $conditions{$condition} = [];
                }
                CONDITION: for ($condition)
                {
                    ($_ eq 'ingress_port' || $_ eq 'egress_port')  && do
                    {
                        my $port = $__stormConditions->[$i]->{'param'};
                        push(@{$conditions{$condition}},
                             $self->tpPlatformMapLogicalToFaceplatePort($sw,
                                                                        $port));
                        last CONDITION;
                    };
                }
            }
        }
        $result = $status != $FM_OK ? $FM_FAIL : $result;
    }
    print("conditions:\n") if (scalar(keys(%conditions)) > 0);
    foreach my $condition (sort(keys(%conditions)))
    {
        printf("%4s %-15s", ' ', $condition);
        CONDITION: for ($condition)
        {
            (($_ eq 'ingress_port') || ($_ eq 'egress_port'))  && do
            {
                printf(": %s",
                       $self->StringifyList(@{$conditions{$condition}}));
                last CONDITION;
            }
        }
        print("\n");
    }

    my %actions = ();
    foreach my $sw ($self->tpGetSwitches)
    {
        my ($numActions, $status);

        $status = $chip->fmGetStormCtrlActionList($sw, $stormCtrl->[$sw],
                                                  \$numActions,
                                                  $__stormActions,
                                                  TP_MAX_NUM_STORM_ACTION);
        if ($status == $FM_OK)
        {
            my %tpActions = 
                 map {$__stormCtrlActions{$_} => $_} keys(%__stormCtrlActions);
            for (my $i = 0; $i < $numActions; $i++)
            {
                my $action = $tpActions{$__stormActions->[$i]->{'type'}};
                $actions{$action} = !exists($actions{$action})
                                    ? []
                                    : $actions{$action};
                ACTION: for ($action)
                {
                    $_ eq 'filter_port' && do
                    {
                        my $port = $__stormActions->[$i]->{'param'};
                        push(@{$actions{$action}},
                             $self->tpPlatformMapLogicalToFaceplatePort($sw,
                                                                        $port));
                        last ACTION;
                    };
                }
            }
        }
        $result = $status != $FM_OK ? $FM_FAIL : $result;
    }
    print("actions:\n") if (scalar(keys(%actions)) > 0);
    foreach my $action (sort(keys(%actions)))
    {
        printf("%4s %-15s", ' ', $action);
        ACTION: for ($action)
        {
            $_ eq 'filter_port' && do
            {
                printf(": %s",
                       $self->StringifyList(@{$actions{$action}}));
                last ACTION;
            }
        }
        print("\n");
    }
    print("\n");
    return $result;
}

##@cmethod public int tpHandleShowStormControllerStats(int virtualCtrl)
#
# @brief        Handles showing the statistics counter for the specified storm
#               controller
#
# @param[in]    virtualCtrl The virtual storm controller identifier
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleShowStormControllerStats
{
    my ($self, $virtualCtrl) = @_;

    my $chip = $self->{'CHIP'};
    my $result = $FM_OK;

    if (!defined($virtualCtrl))
    {
        print("Must specify <storm controller>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $stormCtrl = [];
    foreach my $switchNum ($self->tpGetSwitches)
    {
        if ($self->ValidateStormController($switchNum,
                                           $virtualCtrl,
                                           \$stormCtrl->[$switchNum]) != $FM_OK)
        {
            print($TP_MSG_ERR_STORM_CTRL_INVALID_CTRL);
            return $FM_ERR_INVALID_ARGUMENT;
        }
    }

    print("\n");
    printf("Storm controller statistics: Controller %d\n", $virtualCtrl);
    print("------------------------------------------------------------\n");

    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    my $total = Math::BigInt->new(0);
    foreach my $sw ($self->tpGetSwitches)
    {
        my %void = (type => "fm_uint64", value => Math::BigInt->new(0));
        my $status = $chip->fmGetStormCtrlAttribute($sw, $stormCtrl->[$sw],
                                                    $FM_STORM_COUNT, \%void);
        if ($status == $FM_OK)
        {
            $total += $void{'value'};
            printf("Switch %d: ") if ($switchCount > 1);
            $self->printCounter($void{'value'}, "Matches", undef);
        }
        $result = $status != $FM_OK ? $FM_FAIL : $result;
    }
    if ($switchCount > 1)
    {
        $self->printCounter($total, " " x 36 . "Total", undef);
    }
    print("\n");
    return $result;
}

1;
