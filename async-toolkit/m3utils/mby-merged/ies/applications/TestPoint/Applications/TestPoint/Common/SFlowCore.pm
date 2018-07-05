# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/SFlowCore.pm
# Creation Date:    06/27/08
# Description:      
#
# INTEL CONFIDENTIAL
# Copyright 2008 - 2011 Intel Corporation. All Rights Reserved. 
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

##@class Applications::TestPoint::Common::SFlowCore
#
# @code
#   create sFlow <sFlow id> <sFlow type>
#   set sFlow <sFlow id> <attribute> <parameter>
#   add sFlow <sFlow id> <port>
#   del sFlow <sFlow id> 
#   del sFlow port <sFlow id> <port>
#   show sFlow <sFlow id>
# @endcode
package Applications::TestPoint::Common::SFlowCore;
use strict;
use warnings;

use SDKScalars;
use POSIX qw();
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

###############################################################################
#
#                           CONSTANTS & TYPES
#
###############################################################################
#FIXME: Cannot use FAMILY specific constants in common file
#use constant TP_MAX_NUM_SFLOW_PORTS        => $FM4000_MAX_PORT + 1;
use constant TP_MAX_NUM_SFLOW_PORTS        => 24 + 1;

###############################################################################
#
#                           LOCAL VARIABLES
#
###############################################################################

##@var private Map<char*, int> __sFlowAttrs
my %__sFlowAttrs =
(
    'vlan'          => $FM_SFLOW_VLAN,
    'sampleRate'    => $FM_SFLOW_SAMPLE_RATE,
    'truncLength'   => $FM_SFLOW_TRUNC_LENGTH
);

my @__sFlowTypes = 
(
    "ingress",
    "egress"
);


###############################################################################
#
#                           PUBLIC FUNCTIONS
#
###############################################################################

##@cmethod public int tpHandleCreateSFlow(int sFlowId, char *sFlowType)
#
# @brief        Handles creating a sFlow instance
#
# @param[in]    sFlow The unique sFlow id  
#
# @param[in]    sFlowType The type of the sFlow  
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleCreateSFlow
{
    my ($self, $sFlowId, $sFlowType) = @_;

    my $chip = $self->{'CHIP'};
    my $switchCount = scalar(@{[$self->tpGetSwitches]});

    if (!defined($sFlowId) || (!defined($sFlowType)))
    {
        print("Must specify <sFlow id>  <sFlow type>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    # Ensure that $sFlowId is an integer number.
    if ( (!defined(($sFlowId = $self->str2intnum($sFlowId))) ) 
        || ($sFlowId > POSIX::INT_MAX ) || ($sFlowId < POSIX::INT_MIN ) )
    {
        print($TP_MSG_ERR_SFLOW_INVALID_ID);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    # Ensure that $sFlowType is either "ingress" or "egress".
    $sFlowType = lc($sFlowType);
    if (($sFlowType ne "ingress") && ($sFlowType ne "egress"))
    {
        print($TP_MSG_ERR_SFLOW_INVALID_TYPE);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if ($sFlowType eq "ingress")
    {
        $sFlowType = $FM_SFLOW_TYPE_INGRESS;
    }
    else
    {
        $sFlowType = $FM_SFLOW_TYPE_EGRESS;
    }

    my $result = $FM_OK;
    SWITCH: foreach my $switchNum ($self->tpGetSwitches)
    {
        my ($status);

#        $chip->disableErrors();
        $status = $chip->fmCreateSFlow($switchNum, $sFlowId, $sFlowType);
#        $chip->enableErrors();

        if ($status != $FM_OK)
        {
            $result = $FM_FAIL;
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            printf("sFlow %d cannot be created!\n", $sFlowId);
            next SWITCH;
        }
    }
    return $result;
}

##@cmethod public int tpHandleDeleteSFlow(int sFlowId)
#
# @brief        Handles deleting a sFlow instance
#
# @param[in]    sFlow The unique sFlow id  
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleDeleteSFlow
{
    my ($self, $sFlowId) = @_;

    my $chip = $self->{'CHIP'};
    my $switchCount = scalar(@{[$self->tpGetSwitches]});

    if (!defined($sFlowId) )
    {
        print("Must specify <sFlow id>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    # Ensure that $sFlowId is an integer number.
    if ( (!defined(($sFlowId = $self->str2intnum($sFlowId))) )
        || ($sFlowId > POSIX::INT_MAX ) || ($sFlowId < POSIX::INT_MIN ) )
    {
        print($TP_MSG_ERR_SFLOW_INVALID_ID);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
    SWITCH: foreach my $switchNum ($self->tpGetSwitches)
    {
        my ($status);

#        $chip->disableErrors();
        $status = $chip->fmDeleteSFlow($switchNum, $sFlowId);
#        $chip->enableErrors();

        if ($status != $FM_OK)
        {
            $result = $FM_FAIL;
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            printf("sFlow %d cannot be created!\n", $sFlowId);
            next SWITCH;
        }
    }
    return $result;
}

##@cmethod public int tpHandleSetSFlowAttribute( int sFlowId
#                                                char   *attribute,
#                                                int    value)
#
# @brief        Handles setting a sFlow attribute
#
# @param[in]    sFlowId The sFlow identifier
#
# @param[in]    attribute The attribute to be set
#
# @param[in]    value The attribute value
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleSetSFlowAttribute
{
    my ($self, $sFlowId, $attribute, $value) = @_;

    my $chip = $self->{'CHIP'};

    if (!defined($sFlowId) || !defined($attribute) || !defined($value))
    {
        print("Must specify <sFlow> <attribute> <value>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (!exists($__sFlowAttrs{$attribute}))
    {
        print($TP_MSG_ERR_SFLOW_INVALID_ATTR);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    SWITCH: for ($attribute)
    {
        if ( !defined($value = $self->str2intnum($value)))
        {
            print($TP_MSG_ERR_SFLOW_INVALID_PARAM);
            return $FM_ERR_INVALID_ARGUMENT;
        }
        my %void = ();
        if ($attribute eq 'vlan')
        {
            %void = (type => "fm_uint16", value => $value);
        }
        elsif ($attribute eq 'sampleRate')
        {
            if ( $value <= 0 )
            {
                print($TP_MSG_ERR_SFLOW_INVALID_PARAM);
                return $FM_ERR_INVALID_ARGUMENT;
            }
            %void = (type => "fm_uint32", value => $value);
        }
        elsif ($attribute eq 'truncLength')
        {
            %void = (type => "fm_int", value => $value);
        }

        $attribute = $__sFlowAttrs{$attribute};

        foreach my $switchNum ($self->tpGetSwitches)
        {
#            $chip->disableErrors();
            my $status = 
                   $chip->fmSetSFlowAttribute($switchNum,
                                              $sFlowId,
                                              $attribute, 
                                              \%void);
#            $chip->enableErrors();
            $result = $status != $FM_OK ? $FM_FAIL : $result;
        }
        last SWITCH;
    }
    return $result;
}

##@cmethod public int tpHandleAddSFlowPort(int sFlowId, char *ports)
#
# @brief        Handles adding ports to a sFlow instance
#
# @param[in]    sFlowId The sFlow instance to add the ports to
#
# @param[in]    ports    A string representing a list of ports
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleAddSFlowPort
{
    my ($self, $sFlowId, $ports) = @_;
    my $chip = $self->{'CHIP'};

    if (!defined($sFlowId) || !defined($ports))
    {
        print("Must specify <sFlow Id> <ports>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @portList = 
           $self->validateList($ports,
                               $self->tpPlatformGetFaceplatePortRange);
    if (scalar(@portList) == 0)
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
    # Operate on each selected switch, one at a time
    foreach my $switchNum ($self->tpGetSwitches)
    {
        # Convert $port to an array of ports that appear only on this switch
        my @portList = $self->validateExplicitList($TRUE, 
                                                   $ports, 
                                                   $self->tpPlatformGetSwitchPortList($switchNum));
        
        # If none of the ports appear on this switch, go to the next switch
        if (scalar(@portList) == 0)
        {
            next;
        }
       
        # Operate on each specified port that appears on this switch
        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned sw since it should match switchNum and may actually be
            # wrong for some platforms.
            my ($sw, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
           
            my $status = $chip->fmAddSFlowPort($switchNum, 
                                               $sFlowId,
                                               $logPort);
            $result = $status != $FM_OK ? $FM_FAIL : $result;
        }
    }
    return $result;
}

##@cmethod public int tpHandleDeleteSFlowPort(int sFlowId, char *ports)
#
# @brief        Handles deleting ports from a sFlow instance
#
# @param[in]    sFlowId The sFlow instance to delete the ports from
#
# @param[in]    ports    A string representing a list of ports
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleDeleteSFlowPort
{
    my ($self, $sFlowId, $ports) = @_;
    my $chip = $self->{'CHIP'};

    if (!defined($sFlowId) || !defined($ports))
    {
        print("Must specify <sFlow Id> <ports>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my @portList = 
           $self->validateList($ports,
                               $self->tpPlatformGetFaceplatePortRange);
    if (scalar(@portList) == 0)
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
    # Operate on each selected switch, one at a time
    foreach my $switchNum ($self->tpGetSwitches)
    {
        # Convert $port to an array of ports that appear only on this switch
        my @portList = $self->validateExplicitList($TRUE, 
                                                   $ports, 
                                                   $self->tpPlatformGetSwitchPortList($switchNum));
        
        # If none of the ports appear on this switch, go to the next switch
        if (scalar(@portList) == 0)
        {
            next;
        }
       
        # Operate on each specified port that appears on this switch
        foreach my $port (@portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned sw since it should match switchNum and may actually be
            # wrong for some platforms.
            my ($sw, $logPort) = $self->tpPlatformMapGlobalToLogicalPort($port);
           
            my $status = $chip->fmDeleteSFlowPort($switchNum, 
                                                  $sFlowId, 
                                                  $logPort);
            $result = $status != $FM_OK ? $FM_FAIL : $result;
        }
    }
    return $result;
}

##@cmethod public int tpHandleShowSFlow(int sFlowId)
#
# @brief        Handles showing the configuration of the specified sFlow
#
# @param[in]    sFlowId The sFlow identifier
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleShowSFlow
{
    my ($self, $sFlowId) = @_;

    my $chip = $self->{'CHIP'};
    my $result = $FM_OK;

    if (!defined($sFlowId))
    {
        print("Must specify <sFlow Id>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    print("\n");
    printf("sFlow id: %d\n", $sFlowId);
    print("------------------------------------------------------------\n");

#    $chip->disableErrors();
    my $switchNum = shift(@{[$self->tpGetSwitches]});

    my $sFlowType = 0;

    my $status = $chip->fmGetSFlowType($switchNum, 
                                    $sFlowId,
                                    \$sFlowType); 
    if ($status == $FM_OK)
    {
        printf("%-21s %-21s\n", "type", $__sFlowTypes[$sFlowType]);
    }

    $result = $status != $FM_OK ? $FM_FAIL : $result;

    my %void = (type => "fm_uint16", value => 0);
    $status = $chip->fmGetSFlowAttribute($switchNum,
                                         $sFlowId,
                                         $FM_SFLOW_VLAN, 
                                         \%void);
    if ($status == $FM_OK)
    {
        if ($void{'value'} == 0)
        {
            printf("%-21s\n", "vlan: not vlan specific");
        }
        else
        {
            printf("%-21s %9d \n", "vlan", $void{'value'});
        }
    }
    $result = $status != $FM_OK ? $FM_FAIL : $result;

    %void = (type => "fm_uint32", value => 0);
    $status = $chip->fmGetSFlowAttribute($switchNum,
                                         $sFlowId,
                                         $FM_SFLOW_SAMPLE_RATE, 
                                         \%void);
    if ($status == $FM_OK)
    {
        printf("%-21s %9d \n", "sample rate", $void{'value'});
    }
    $result = $status != $FM_OK ? $FM_FAIL : $result;

    %void = (type => "fm_int", value => 0);
    $status = $chip->fmGetSFlowAttribute($switchNum,
                                         $sFlowId,
                                         $FM_SFLOW_TRUNC_LENGTH, 
                                         \%void);
    if ($status == $FM_OK)
    {
        if ($void{'value'} == -1)
        {
            printf("%-21s\n", "trunc Length: no truncation");

        }
        else
        {
            printf("%-21s %9d B\n", "trunc Length", $void{'value'});
        }
    }
    elsif ($status == $FM_ERR_INVALID_ARGUMENT)
    {
        # if there is no port configured for this sFlow instance,
        # we don't show any truncation configuration
        $status = $FM_OK;
    }
    $result = $status != $FM_OK ? $FM_FAIL : $result;

    my @ports = ((0) x TP_MAX_NUM_SFLOW_PORTS);
    foreach my $sw ($self->tpGetSwitches)
    {
        my ($numPorts, $status);

        $status = $chip->fmGetSFlowPortList($sw, 
                                            $sFlowId,
                                            \$numPorts,
                                            \@ports,
                                            TP_MAX_NUM_SFLOW_PORTS);

        $result = $status != $FM_OK ? $FM_FAIL : $result;
        
        if ($status == $FM_OK)
        {
            print("ports: ") if ($numPorts > 0);

            my @portList = ();
            for (my $i = 0; $i < $numPorts; $i++)
            {
               push(@portList, $ports[$i]); 
            }

            printf("%s", $self->StringifyList(@portList));
        }
        print("\n");
    }
    print("\n");
#    $chip->enableErrors();
    return $result;
}

1;
