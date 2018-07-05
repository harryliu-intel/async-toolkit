# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/PolicerCore.pm
# Creation Date:    Feburary 21, 2008
# Description:      TestPoint interface to policers
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

package Applications::TestPoint::Common::PolicerCore;
use strict;
use warnings;

use SDKScalars;
use Math::BigInt;

##@cmethod private int tpHandleCreatePolicer(char *bank, char *policer)
#
# @desc         Creates a policer
#
# @param[in]    policer is An Policer number 
#
# @param[in]    bank is A bank number @optional
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleCreatePolicer
{
    my ($self, $policer, $bank) = @_;

    my $chip = $self->{CHIP};

    $policer = $self->str2decnum($policer);

    if (not defined $bank)
    {
        $bank = $FM_POLICER_BANK_AUTOMATIC;
    }
    else
    {
        $bank = $self->str2decnum($bank);

        if ($bank < 0 || $bank >=4)
        {
            printf("bank must be a number between 0 and 3");
            return $FM_FAIL;
        }
    }


    my $status = $FM_OK;
    foreach my $switchNum ($self->tpGetSwitches)
    {
        my $config = SDK::fm_policerConfig->new();

        $chip->disableErrors();
        $status = $chip->fmCreatePolicer($switchNum, $bank, $policer, $config); 
        $chip->enableErrors();
    }

    return $status;

}   # end tpHandleCreatePolicer


# set dscp down map 
# set swpri down map 


# set policer <policer> cir action < drop | markdown >  < swpri | dscp | both >
# set policer <policer> cir capacity <in KB>
# set policer <policer> cir rate <in KB>
# set policer <policer> eir action < drop | markdown >  < swpri | dscp | both >
# set policer <policer> eir capacity <in KB>
# set policer <policer> eir rate <in KB>
# set policer <policer> color_source <green | dscp | swpri>
# 
sub tpHandleSetPolicerConfig
{
    my $self = shift;
    my $policer = $self->str2decnum(shift);
    my $attribute = shift;

    my $chip = $self->{CHIP};

    foreach my $switchNum ($self->tpGetSwitches)
    {
        if (($attribute eq "cir") || ($attribute eq "eir"))
        {
            my $property = shift;

            if ($property eq "capacity")
            {
                my $capacity = int(shift);

                my %value = ( type => 'fm_uint32', value => $capacity * 1000);

                $chip->disableErrors();

                if ($attribute eq "cir")
                {
                    my $status = $chip->fmSetPolicerAttribute($switchNum, 
                        $policer, $FM_POLICER_CIR_CAPACITY, \%value);
                }
                else
                {
                    my $status = $chip->fmSetPolicerAttribute($switchNum, 
                        $policer, $FM_POLICER_EIR_CAPACITY, \%value);
                }

                $chip->enableErrors();
                
            }
            elsif ($property eq "rate")
            {
                my $rate = scalar(shift);

                if ($rate > 240000000)
                {
                    printf("Must specify a rate between 0 and 240,000,000\n");
                    return $FM_FAIL;
                } 

                my %value = ( type => 'fm_uint32', value => $rate);

                $chip->disableErrors();

                if ($attribute eq "cir")
                {
                    my $status = $chip->fmSetPolicerAttribute($switchNum, 
                        $policer, $FM_POLICER_CIR_RATE, \%value);
                }
                else
                {
                    my $status = $chip->fmSetPolicerAttribute($switchNum, 
                        $policer, $FM_POLICER_EIR_RATE, \%value);
                }

                $chip->enableErrors();
                
            }
            elsif ($property eq "action")
            {
                if ($attribute eq "cir")
                {
                    $property = $FM_POLICER_CIR_ACTION;
                }
                else
                {
                    $property = $FM_POLICER_EIR_ACTION;
                }

                my %value = ( type => 'fm_int', value => 0);

                my $action = shift;

                if ($action eq "drop")
                {
                    $value{value} = $FM_POLICER_ACTION_DROP;

                    $chip->disableErrors();
                    my $status = $chip->fmSetPolicerAttribute($switchNum, 
                        $policer, $property, \%value);

                    $chip->enableErrors();
                }
                elsif ($action eq "markdown")
                {
                    $value{value} = $FM_POLICER_ACTION_MKDN;

                    $chip->disableErrors();
                    my $status = $chip->fmSetPolicerAttribute($switchNum, 
                        $policer, $property, \%value);

                    $chip->enableErrors();

                    my %valueDSCP = ( type => 'fm_bool', value => $FM_ENABLED);
                    my %valueSWPRI = ( type => 'fm_bool', value => $FM_ENABLED);

                    my $markdown = shift;

                    if ($markdown eq "swpri")
                    {
                        $valueSWPRI{value} = $FM_ENABLED;
                        $valueDSCP{value} = $FM_DISABLED;
                    }
                    elsif ($markdown eq "dscp")
                    {
                        $valueSWPRI{value} = $FM_DISABLED;
                        $valueDSCP{value} = $FM_ENABLED;
                    }
                    elsif ($markdown eq "both")
                    {
                        $valueSWPRI{value} = $FM_ENABLED;
                        $valueDSCP{value} = $FM_ENABLED;
                    }
                    else
                    {
                        printf("Must specify swpri, dscp, or both");
                        return $FM_FAIL;
                    }

                    $chip->disableErrors();
                    $status = $chip->fmSetPolicerAttribute($switchNum, 
                        $policer, $FM_POLICER_MKDN_SWPRI, \%valueSWPRI);

                    $status = $chip->fmSetPolicerAttribute($switchNum, 
                        $policer, $FM_POLICER_MKDN_DSCP, \%valueDSCP);

                    $chip->enableErrors();
                }
                elsif ($action eq "trap")
                {
                    $value{value} = $FM_POLICER_ACTION_TRAP;

                    $chip->disableErrors();
                    my $status = $chip->fmSetPolicerAttribute($switchNum, 
                        $policer, $property, \%value);

                    $chip->enableErrors();
                }
                elsif ($action eq "swPri_DSCP")
                {
                    $value{value} = $FM_POLICER_ACTION_SET_SWPRI_DSCP;

                    $chip->disableErrors();
                    my $status = $chip->fmSetPolicerAttribute($switchNum, 
                        $policer, $property, \%value);

                    $chip->enableErrors();

                    my $swPri = scalar(shift);
                    $value{value} = $swPri;

                    if ($attribute eq "cir")
                    {
                        $property = $FM_POLICER_CIR_ACTION_DATA_SWPRI;
                    }
                    else
                    {
                        $property = $FM_POLICER_EIR_ACTION_DATA_SWPRI;
                    }

                    $chip->disableErrors();
                    $status = $chip->fmSetPolicerAttribute($switchNum, 
                        $policer, $property, \%value);

                    $chip->enableErrors();

                    my $dscp = scalar(shift);
                    $value{value} = $dscp;

                    if ($attribute eq "cir")
                    {
                        $property = $FM_POLICER_CIR_ACTION_DATA_DSCP;
                    }
                    else
                    {
                        $property = $FM_POLICER_EIR_ACTION_DATA_DSCP;
                    }

                    $chip->disableErrors();
                    $status = $chip->fmSetPolicerAttribute($switchNum, 
                        $policer, $property, \%value);

                    $chip->enableErrors();
                }
                elsif ($action eq "swPri_VPri")
                {
                    $value{value} = $FM_POLICER_ACTION_SET_SWPRI_VPRI;

                    $chip->disableErrors();
                    my $status = $chip->fmSetPolicerAttribute($switchNum, 
                        $policer, $property, \%value);

                    $chip->enableErrors();

                    my $swPri = scalar(shift);
                    $value{value} = $swPri;

                    if ($attribute eq "cir")
                    {
                        $property = $FM_POLICER_CIR_ACTION_DATA_SWPRI;
                    }
                    else
                    {
                        $property = $FM_POLICER_EIR_ACTION_DATA_SWPRI;
                    }

                    $chip->disableErrors();
                    $status = $chip->fmSetPolicerAttribute($switchNum, 
                        $policer, $property, \%value);

                    $chip->enableErrors();

                    my $vpri = scalar(shift);
                    $value{value} = $vpri;

                    if ($attribute eq "cir")
                    {
                        $property = $FM_POLICER_CIR_ACTION_DATA_VPRI;
                    }
                    else
                    {
                        $property = $FM_POLICER_EIR_ACTION_DATA_VPRI;
                    }

                    $chip->disableErrors();
                    $status = $chip->fmSetPolicerAttribute($switchNum, 
                        $policer, $property, \%value);

                    $chip->enableErrors();
                }
                else
                {
                    printf("Must specify: drop, or markdown");
                    return $FM_FAIL;
                }

            }
            else
            {
                printf("Must specify: capacity, rate, or action");
                return $FM_FAIL;        
            }
        }
        elsif ($attribute eq "color_source")
        {
            my $color_source = shift;
            if ( $color_source eq "green")
            {
                $color_source = $FM_POLICER_COLOR_SRC_GREEN;
            }
            elsif ($color_source eq "dscp") 
            {
                $color_source = $FM_POLICER_COLOR_SRC_DSCP;
            }
            elsif ($color_source eq "swpri")
            {
                $color_source = $FM_POLICER_COLOR_SRC_SWPRI;
            }
            else
            {
                printf("Must specify green, dscp, or swpri");
                return $FM_FAIL;
            }

            my %value = (type => 'fm_int', value => $color_source);

            $chip->disableErrors();
            my $status = $chip->fmSetPolicerAttribute($switchNum, 
                $policer, $FM_POLICER_COLOR_SOURCE, \%value);

            $chip->enableErrors();
           
            return $status; 

        }
        elsif ($attribute eq "swpri_markdown_map")
        {
            my $swpriSrc = shift;
            my $swpriDst = shift;

            if ($swpriSrc < 0 || $swpriSrc > 15)
            {
                printf("Please specify a valid switch priority 0-15\n");
                return $FM_FAIL;
            }

            if ($swpriDst < 0 || $swpriDst > 15)
            {
                printf("Please specify a valid downmap priority 0-15\n");
                return $FM_FAIL;
            }

            my @map = (0)x16;
            my %value = ( type=>"fm_int[]", value => \@map);

            # Since we're only changing one value, fetch the entire map
            $chip->disableErrors();
            my $status = $chip->fmGetPolicerAttribute($switchNum, $policer,
                                $FM_POLICER_SWPRI_MKDN_MAP, \%value);
            $chip->enableErrors();

            if ($status != $FM_OK)
            {
                return $status;
            }

            $map[$swpriSrc] = $swpriDst;

            $chip->disableErrors();
            $status = $chip->fmSetPolicerAttribute($switchNum, $policer,
                                $FM_POLICER_SWPRI_MKDN_MAP, \%value);
            $chip->enableErrors();
     
            return $status;

        }
        elsif ($attribute eq "dscp_markdown_map")
        {
            my $dscpSrc = int(shift);
            my $dscpDst = int(shift);

            if ($dscpSrc < 0 || $dscpSrc > 63)
            {
                printf("Please specify a valid dscp codepoint 0-63\n");
                return $FM_FAIL;
            }

            if ($dscpDst < 0 || $dscpDst > 63)
            {
                printf("Please specify a valid downmap dscp codepoint 0-63\n");
                return $FM_FAIL;
            }

            my @map = (0)x64;
            my %value = ( type=>"fm_int[]", value => \@map);

            # Since we're only changing one value, fetch the entire map
            $chip->disableErrors();
            my $status = $chip->fmGetPolicerAttribute($switchNum, $policer,
                                $FM_POLICER_DSCP_MKDN_MAP, \%value);
            $chip->enableErrors();

            if ($status != $FM_OK)
            {
                return $status;
            }

            $map[$dscpSrc] = $dscpDst;

            $chip->disableErrors();
            $status = $chip->fmSetPolicerAttribute($switchNum, $policer,
                                $FM_POLICER_DSCP_MKDN_MAP, \%value);
            $chip->enableErrors();
     
            return $status;
        }
        else
        {
            printf("Invalid attribute, please specify: cir, eir or color_source\n");
            return $FM_FAIL;
        }
    }

}   # end tpHandleSetPolicerConfig


##@cmethod private int tpHandleDeletePolicer(char *policer)
#
# @desc         Deletes the specified policer
#
# @param[in]    policer is An Policer number
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleDeletePolicer
{
    my ($self, $policer) = @_;

    my $chip = $self->{CHIP};

    $policer = $self->str2decnum($policer);

    my $status = $FM_OK;
    foreach my $switchNum ($self->tpGetSwitches)
    {
        $chip->disableErrors();
        $status = $chip->fmDeletePolicer($switchNum, $policer); 
        $chip->enableErrors();
    }

    return $status;

}   # end tpHandleDeletePolicer


##@cmethod private int tpHandleShowPolicer(char *policer)
#
# @desc         Displays the specified policers attributes
#
# @param[in]    policer is An Policer number, or list
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleShowPolicer
{
    my ($self, $policerNum) = @_;

    my $chip = $self->{CHIP};

    foreach my $switchNum ($self->tpGetSwitches)
    {
        my @policerList = $self->validateList($policerNum, 0, 512*4);

        @policerList = sort {$a <=> $b} @policerList;

        for (my $i = 0 ; $i < scalar(@policerList) ; $i++)
        {
            my $policer = $self->_tpGetPolicerAttributes($switchNum, 
                $policerList[$i]);

            if (defined $policer)
            {
                $self->_tpPrintPolicer($switchNum, $policer);
            }
            else
            {
                printf("Invalid policer $policerList[$i]\n");
                return $FM_FAIL;
            }
        }
    }

}   # end tpHandleShowPolicer


##@cmethod private struct _tpGetPolicerAttributes(int policer)
#
# @desc         Fetches all attributes for a given policer number
#
# @param[in]    switchNum is a Switch number
# @param[in]    policer is An Policer number
#
# @return       struct containing the policer attributes
# @return       undef otherwise
# 
sub _tpGetPolicerAttributes
{
    my ($self, $switchNum, $policerNum) = @_;

    my $chip = $self->{CHIP};
    my %policer = ( policerNum      => $policerNum,
                    cirAction       => "",
                    cirActionDataDscp   => 0,
                    cirActionDataVPri   => 0,
                    cirActionDataSwPri  => 0,
                    cirCapacity     => 0,
                    cirRate         => 0,
                    eirAction       => "",
                    eirActionDataDscp   => 0,
                    eirActionDataVPri   => 0,
                    eirActionDataSwPri  => 0,
                    eirCapacity     => 0,
                    eirRate         => 0,
                    colorSource     => ""
                  );

    my %value = ( type => 'fm_int', value => 0);
    my $status = $chip->fmGetPolicerAttribute($switchNum, $policerNum,
                                    $FM_POLICER_CIR_ACTION, \%value);

    if ($status != $FM_OK)
    {
        return undef;
    }

    if ($value{value} == $FM_POLICER_ACTION_DROP)
    {
        $policer{cirAction} = "drop";
    }
    elsif ($value{value} == $FM_POLICER_ACTION_MKDN)
    {
        $policer{cirAction} = "markdown";
    }
    elsif ($value{value} == $FM_POLICER_ACTION_TRAP)
    {
        $policer{cirAction} = "trap";
    }
    elsif ($value{value} == $FM_POLICER_ACTION_SET_SWPRI_DSCP)
    {
        $policer{cirAction} = "swPri_DSCP";
    }
    elsif ($value{value} == $FM_POLICER_ACTION_SET_SWPRI_VPRI)
    {
        $policer{cirAction} = "swPri_VPri";
    }


    $status = $chip->fmGetPolicerAttribute($switchNum, $policerNum,
                                    $FM_POLICER_EIR_ACTION, \%value);

    if ($value{value} == $FM_POLICER_ACTION_DROP)
    {
        $policer{eirAction} = "drop";
    }
    elsif ($value{value} == $FM_POLICER_ACTION_MKDN)
    {
        $policer{eirAction} = "markdown";
    }
    elsif ($value{value} == $FM_POLICER_ACTION_TRAP)
    {
        $policer{eirAction} = "trap";
    }
    elsif ($value{value} == $FM_POLICER_ACTION_SET_SWPRI_DSCP)
    {
        $policer{eirAction} = "swPri_DSCP";
    }
    elsif ($value{value} == $FM_POLICER_ACTION_SET_SWPRI_VPRI)
    {
        $policer{eirAction} = "swPri_VPri";
    }

    $status = $chip->fmGetPolicerAttribute($switchNum, $policerNum,
                                    $FM_POLICER_COLOR_SOURCE, \%value);

    $policer{colorSource} = ($value{value} == $FM_POLICER_COLOR_SRC_GREEN) ?
                                                "green" :
                              ($value{value} == $FM_POLICER_COLOR_SRC_DSCP) ?
                                                "dscp" : 
                                                "swpri" ;

    $value{type} = 'fm_uint32';
    $status = $chip->fmGetPolicerAttribute($switchNum, $policerNum,
                                    $FM_POLICER_CIR_CAPACITY, \%value);
    $policer{cirCapacity} = $value{value}; 
    $status = $chip->fmGetPolicerAttribute($switchNum, $policerNum,
                                    $FM_POLICER_EIR_CAPACITY, \%value);
    $policer{eirCapacity} = $value{value}; 


    $status = $chip->fmGetPolicerAttribute($switchNum, $policerNum,
                                    $FM_POLICER_CIR_RATE, \%value);
    $policer{cirRate} = $value{value}; 
    $status = $chip->fmGetPolicerAttribute($switchNum, $policerNum,
                                    $FM_POLICER_EIR_RATE, \%value);
    $policer{eirRate} = $value{value}; 


    if ($policer{cirAction} eq "markdown")
    {
        $value{type} = 'fm_bool';
                
        $status = $chip->fmGetPolicerAttribute($switchNum, $policerNum,
                                    $FM_POLICER_MKDN_DSCP, \%value);

        my %value2 = (type =>'fm_bool', value => 0);

        $status = $chip->fmGetPolicerAttribute($switchNum, $policerNum,
                                    $FM_POLICER_MKDN_SWPRI, \%value2);

        if (($value{value} == $FM_ENABLED) && ($value2{value} == $FM_ENABLED))
        {
            $policer{cirAction} .= " both";
        }
        elsif ($value{value} == $FM_ENABLED)
        {
            $policer{cirAction} .= " dscp";
        }
        elsif ($value2{value} == $FM_ENABLED)
        {
            $policer{cirAction} .= " swpri";
        }
    }

    if ($policer{eirAction} eq "markdown")
    {
        $value{type} = 'fm_bool';
                
        $status = $chip->fmGetPolicerAttribute($switchNum, $policerNum,
                                    $FM_POLICER_MKDN_DSCP, \%value);

        my %value2 = (type =>'fm_bool', value => 0);

        $status = $chip->fmGetPolicerAttribute($switchNum, $policerNum,
                                    $FM_POLICER_MKDN_SWPRI, \%value2);

        if (($value{value} == $FM_ENABLED) && ($value2{value} == $FM_ENABLED))
        {
            $policer{eirAction} .= " both";
        }
        elsif ($value{value} == $FM_ENABLED)
        {
            $policer{eirAction} .= " dscp";
        }
        elsif ($value2{value} == $FM_ENABLED)
        {
            $policer{eirAction} .= " swpri";
        }
    }

    $value{type} = 'fm_int';
    $status = $chip->fmGetPolicerAttribute($switchNum, $policerNum,
                                    $FM_POLICER_CIR_ACTION_DATA_DSCP, \%value);
    $policer{cirActionDataDscp} = $value{value}; 
    $status = $chip->fmGetPolicerAttribute($switchNum, $policerNum,
                                    $FM_POLICER_CIR_ACTION_DATA_VPRI, \%value);
    $policer{cirActionDataVPri} = $value{value}; 
    $status = $chip->fmGetPolicerAttribute($switchNum, $policerNum,
                                    $FM_POLICER_CIR_ACTION_DATA_SWPRI, \%value);
    $policer{cirActionDataSwPri} = $value{value}; 

    $status = $chip->fmGetPolicerAttribute($switchNum, $policerNum,
                                    $FM_POLICER_EIR_ACTION_DATA_DSCP, \%value);
    $policer{eirActionDataDscp} = $value{value}; 
    $status = $chip->fmGetPolicerAttribute($switchNum, $policerNum,
                                    $FM_POLICER_EIR_ACTION_DATA_VPRI, \%value);
    $policer{eirActionDataVPri} = $value{value}; 
    $status = $chip->fmGetPolicerAttribute($switchNum, $policerNum,
                                    $FM_POLICER_EIR_ACTION_DATA_SWPRI, \%value);
    $policer{eirActionDataSwPri} = $value{value}; 

    return \%policer;

}   # end _tpGetPolicerAttributes


##@cmethod void _tpPrintPolicer(struct *policer)
#
# @desc         Given a policer structure, print the policers attributes
#
# @param[in]    switchNum is a Switch Number
# @param[in]    policer is A policer structure
# 
sub _tpPrintPolicer
{
    my ($self, $switchNum, $policer) = @_;

    printf("Switch %d Policer %d\n", $switchNum, $policer->{policerNum});
    if ( ($policer->{cirAction} eq "markdown") ||
         ($policer->{eirAction} eq "markdown") )
    {
        printf("\tColor Source: %s\n", $policer->{colorSource});
    }
    printf("\tCIR:\n");
    printf("\t\tRate:     %d Kb/s\n", $policer->{cirRate});
    printf("\t\tCapacity: %d KB\n", $policer->{cirCapacity} / 1000);
    printf("\t\tAction:   %s\n", $policer->{cirAction});
    if ( ($policer->{cirAction} eq "swPri_DSCP") ||
         ($policer->{cirAction} eq "swPri_VPri") )
    {
        printf("\t\tNew Switch Pri:    %s\n", $policer->{cirActionDataSwPri});
        if ($policer->{cirAction} eq "swPri_DSCP")
        {
            printf("\t\tNew DSCP:          %s\n", $policer->{cirActionDataDscp});
        }
        else
        {
            printf("\t\tNew Vlan Priority: %s\n", $policer->{cirActionDataVPri});
        }
    }
    printf("\tEIR:\n");
    printf("\t\tRate:     %d Kb/s\n", $policer->{eirRate});
    printf("\t\tCapacity: %d KB\n", $policer->{eirCapacity} / 1000);
    printf("\t\tAction:   %s\n", $policer->{eirAction});
    if ( ($policer->{eirAction} eq "swPri_DSCP") ||
         ($policer->{eirAction} eq "swPri_VPri") )
    {
        printf("\t\tNew Switch Pri:   %s\n", $policer->{eirActionDataSwPri});
        if ($policer->{eirAction} eq "swPri_DSCP")
        {
            printf("\t\tNew DSCP:          %s\n", $policer->{eirActionDataDscp});
        }
        else
        {
            printf("\t\tNew Vlan Priority: %s\n", $policer->{eirActionDataVPri});
        }
    }

}   # end _tpPrintPolicer

1;
