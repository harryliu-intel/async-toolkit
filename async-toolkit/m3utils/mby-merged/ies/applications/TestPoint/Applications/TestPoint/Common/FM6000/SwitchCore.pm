# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FM4000/SwitchCore.pm
# Creation Date:    04/05/12
# Description:      
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

package Applications::TestPoint::Common::FM6000::SwitchCore;
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


###############################################################################
#
#                           PUBLIC FUNCTIONS
#
###############################################################################

##@method public int tp6000HandleSetTrapClass(int  switchNum,
#                                             char *trapClass,
#                                             char *attribute,
#                                             char *parameter)
sub tp6000HandleSetTrapClass
{
    my ($self, $switchNum, $trapClass, $attribute, $parameter) = @_;
    print("tp6000HandleSetTrapClass $switchNum $trapClass $attribute $parameter\n");

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
            if (!defined(($priority = $self->str2intnum($parameter))))
#               if (!defined(($priority = $self->str2intnum($parameter)))
#                   || scalar(@{[$self->validateList($priority, -1, 15)]}) != 1)
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
