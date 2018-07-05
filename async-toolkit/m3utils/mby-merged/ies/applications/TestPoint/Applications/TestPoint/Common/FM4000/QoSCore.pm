# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FM4000/QoSCore.pm
# Creation Date:    12/06/09
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

package Applications::TestPoint::Common::FM4000::QoSCore;
use strict;
use warnings;


use SDKScalars;
use Applications::TestPoint::Common::Messages;


our @EXPORT = qw(
    tpFM4000HandleShowQos
    tpFM4000HandleSetQos
    tpFM4000IsPerLagPortAttribute
);


sub tpFM4000ShowQosNumeric;

sub tpFM4000ValidatePrivatePauseOffWatermark;
sub tpFM4000ValidatePrivatePauseOnWatermark;
sub tpFM4000ValidateRxSmpHogWatermark;
sub tpFM4000ValidateRxSmpPrivateWatermark;
sub tpFM4000ValidateTxSmpHogWatermark;
sub tpFM4000ValidateTxSmpPrivateWatermark;


###############################################################################
#
# FM4000_qos_attribute_map hash definition
#
# key:   QoS port attribute string
# value: reference to the following array
#
# array elements:
# 1st: attribute ID.
# 2nd: maximun index for this attribute.
# 3rd: reference to a display function.
# 4th: reference to a validation function.
#
###############################################################################

my %FM4000_qos_attribute_map = (
    'private_pause_off_wm' => [$FM_QOS_PRIVATE_PAUSE_OFF_WM,
                               2,
                               \&tpFM4000ShowQosNumeric,
                               \&tpFM4000ValidatePrivatePauseOffWatermark],

    'private_pause_on_wm'  => [$FM_QOS_PRIVATE_PAUSE_ON_WM,
                               2,
                               \&tpFM4000ShowQosNumeric,
                               \&tpFM4000ValidatePrivatePauseOnWatermark],

    'rx_smp_hog_wm'        => [$FM_QOS_RX_HOG_WM,
                               2,
                               \&tpFM4000ShowQosNumeric,
                               \&tpFM4000ValidateRxSmpHogWatermark],

    'rx_smp_private_wm'    => [$FM_QOS_RX_PRIVATE_WM,
                               2,
                               \&tpFM4000ShowQosNumeric,
                               \&tpFM4000ValidateRxSmpPrivateWatermark],

    'tx_smp_hog_wm'        => [$FM_QOS_TX_HOG_WM,
                               4,
                               \&tpFM4000ShowQosNumeric,
                               \&tpFM4000ValidateTxSmpHogWatermark],

    'tx_smp_private_wm'    => [$FM_QOS_TX_PRIVATE_WM,
                               2,
                               \&tpFM4000ShowQosNumeric,
                               \&tpFM4000ValidateTxSmpPrivateWatermark],
);


my $attrNameWidth = 28;
my $attrNameWidthStr = "%-" . $attrNameWidth . "s";

my $attrValueWidth = 14;
my $attrValueWidthStr = "%-" . $attrValueWidth . "s";



sub tpFM4000ShowQosNumeric
{
    my ($self, $switchNum, $portListRef, $attrStr, $attrInt, $indexMax) = @_;

    my $chip = $self->{CHIP};
    my %void = (type => "fm_int", value => 0);
    my $index;
    my $status;
    my $indexValue;
    my $indexStr;


    $chip->disableErrors();

    for ($index = 0; $index < $indexMax; $index++)
    {
        printf($attrNameWidthStr, $attrStr . "[" . $index . "]");

        foreach my $port (@$portListRef)
        {
            my ($sw, $logicalPort) =
                     $self->tpPlatformMapGlobalToLogicalPort($port);
            my $info = ($self->tpGetSwitchInfo())[$switchNum];

            $status = $chip->fmGetPortQOS($switchNum, $logicalPort,
                                          $attrInt, $index, \%void);

            $indexValue = int($void{value} / 1024);
            $indexStr = sprintf("%-6s",$indexValue . "k");

            printf($attrValueWidthStr, $status != $FM_OK ? "NA" : $indexStr);
        }

        printf("\n");
    }

    $chip->enableErrors();
}


sub tpFM4000ValidatePrivatePauseOffWatermark
{
    my ($self, $attrStr, $wm) = @_;


    if ( !defined($wm) || ($wm < 0) || ($wm > 4193792) )
    {
        print("Invalid watermark value for $attrStr. ");
        print("The range must be between 0-4193792.\n");
        return $FM_FAIL;
    }

    return $FM_OK;
}


sub tpFM4000ValidatePrivatePauseOnWatermark
{
    my ($self, $attrStr, $wm) = @_;


    if ( !defined($wm) || ($wm < 0) || ($wm > 4193792) )
    {
        print("Invalid watermark value for $attrStr. ");
        print("The range must be between 0-4193792.\n");
        return $FM_FAIL;
    }

    return $FM_OK;
}


sub tpFM4000ValidateRxSmpHogWatermark
{
    my ($self, $attrStr, $wm) = @_;


    if ( !defined($wm) || ($wm < 0) || ($wm > 4193792) )
    {
        print("Invalid watermark value for $attrStr. ");
        print("The range must be between 0-4193792.\n");
        return $FM_FAIL;
    }

    return $FM_OK;
}


sub tpFM4000ValidateRxSmpPrivateWatermark
{
    my ($self, $attrStr, $wm) = @_;


    if ( !defined($wm) || ($wm < 0) || ($wm > 4193792) )
    {
        print("Invalid watermark value for $attrStr. ");
        print("The range must be between 0-4193792.\n");
        return $FM_FAIL;
    }

    return $FM_OK;
}


sub tpFM4000ValidateTxSmpHogWatermark
{
    my ($self, $attrStr, $wm) = @_;


    if ( !defined($wm) || ($wm < 0) || ($wm > 4193792) )
    {
        print("Invalid watermark value for $attrStr. ");
        print("The range must be between 0-4193792.\n");
        return $FM_FAIL;
    }

    return $FM_OK;
}


sub tpFM4000ValidateTxSmpPrivateWatermark
{
    my ($self, $attrStr, $wm) = @_;


    if ( !defined($wm) || ($wm < 0) || ($wm > 4193792) )
    {
        print("Invalid watermark value for $attrStr. ");
        print("The range must be between 0-4193792.\n");
        return $FM_FAIL;
    }

    return $FM_OK;
}



##@cmethod public void tpFM4000HandleShowQos(int sw, char *portListRef)
#
# @desc         Handles showing the QoS port configuration for a set of ports.
#
# @param[in]    sw: Switch on which the show command applies.
#
# @param[in]    portListRef: Reference to the set of ports whose
#                            QoS port configuration is to be shown.
#
# @return       None.
# 
sub tpFM4000HandleShowQos
{
    my ($self, $sw, $portListRef) = @_;


    if (scalar(@$portListRef) == 0)
    {
        print($TP_MSG_ERR_PORT_INVALID_ARRAY);
        return;
    }

    printf("\n");
    printf($attrNameWidthStr, scalar($self->tpGetSwitches) > 1 ? "Switch $sw:" : " ");

    foreach my $port (@$portListRef)
    {
       printf($attrValueWidthStr, sprintf("%d", $port));
    }

    print("\n" . "-" x $attrNameWidth);

    foreach my $port (@$portListRef)
    {
       printf($attrValueWidthStr, "-" x $attrValueWidth);
    }

    print("\n");

    foreach my $attrStr (sort(keys(%FM4000_qos_attribute_map)))
    {
        my $attrInfoRef = $FM4000_qos_attribute_map{$attrStr};

        &{$attrInfoRef->[2]}($self, $sw, $portListRef, $attrStr,
                             $attrInfoRef->[0], $attrInfoRef->[1]);
    }

    print("\n");
}



##@cmethod public void tpFM4000HandleSetQos(int sw, char attrStr,
#                                           char *portListRef,
#                                           int index, int watermark)
#
# @desc         Handles setting the QoS port configuration for a set of ports.
#
# @param[in]    sw: Switch on which the show command applies.
#
# @param[in]    attrStr: The QoS port attribute to be modified.
# 
# @param[in]    portListRef: Reference to the set of ports whose
#                            QoS port attribute is to be modified.
#
# @param[in]    index: The index of the Qos port attribute whose
#                      configuration is to be modified.
# 
# @param[in]    watermark: The value to set the QoS port attribute at.
# 
# @return       None.
# 
sub tpFM4000HandleSetQos
{
    my ($self, $sw, $attrStr, $portListRef, $index, $watermark) = @_;


    if ( !exists $FM4000_qos_attribute_map{$attrStr} )
    {
        print("Invalid QoS port attribute: $attrStr!\n");
        return $FM_FAIL;
    }

    my $attrInfoRef = $FM4000_qos_attribute_map{$attrStr};
    my $maxIndex = $attrInfoRef->[1];

    if ( !defined($index) || ($index < 0) || ($index >= $maxIndex) )
    {
        print("Invalid index value. ");
        printf("The range must be between 0-%d.\n", $maxIndex - 1);
        return $FM_FAIL;
    }


    # Validate specific QoS attribute watermark
    my $status = &{$attrInfoRef->[3]}($self, $attrStr, $watermark);


    if ($status == $FM_OK)
    {
        my $chip = $self->{CHIP};
        my %void = (type  => "fm_int", value => $watermark);


        foreach my $port (@$portListRef)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned switchNum since it should match sw and may actually be
            # wrong for some platforms.
            my ($switchNum, $logPort) =
                    $self->tpPlatformMapGlobalToLogicalPort($port);

            $status = $chip->fmSetPortQOS($sw, $logPort,
                                          $attrInfoRef->[0],
                                          $index, \%void);

            if ($status != $FM_OK)
            {
                printf("QoS port attribute %s cannot be set!\n", $attrStr);
                return $FM_FAIL;
            }
        }
    }

    return $FM_OK;
}



##@cmethod public bool tpFM4000IsPerLagPortAttribute(int sw, char attrStr)
#
# @desc         Indicate whether the given attribute is a per-lag port
#               attribute (i.e. can be set on a LAG logical port).
#  
# @param[in]    sw: Switch on which the show command applies.
#
# @param[in]    attrStr: The QoS port attribute to query for.
# 
# @return       TRUE  if per-lag attribute.
#               FALSE if not per-lag attribute.
# 
sub tpFM4000IsPerLagPortAttribute
{
    my ($self, $sw, $attrStr) = @_;

    my $chip = $self->{CHIP};
    my $attrInfoRef = $FM4000_qos_attribute_map{$attrStr};

    return $chip->fmIsPerLagPortAttribute($sw, $attrInfoRef->[0]);
}

