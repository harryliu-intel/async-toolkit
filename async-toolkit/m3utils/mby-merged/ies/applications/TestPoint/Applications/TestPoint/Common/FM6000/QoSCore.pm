# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FM6000/QoSCore.pm
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

package Applications::TestPoint::Common::FM6000::QoSCore;
use strict;
use warnings;


use SDKScalars;
use Applications::TestPoint::Common::Messages;


our @EXPORT = qw(
    tpFM6000HandleShowQos
    tpFM6000HandleSetQos
    tpFM6000IsPerLagPortAttribute
);


sub tpFM6000ShowQosBoolean;
sub tpFM6000ShowQosNumeric;

sub tpFM6000ValidatePrivatePauseOffWatermark;
sub tpFM6000ValidatePrivatePauseOnWatermark;
sub tpFM6000ValidateRxSmpHogWatermark;
sub tpFM6000ValidateRxSmpPrivateWatermark;
sub tpFM6000ValidateTxSmpHogWatermark;
sub tpFM6000ValidateTxSmpPrivateWatermark;
sub tpFM6000ValidateTxSoftDropOnPrivate;
sub tpFM6000ValidateTxSoftDropOnRxmpFree;

sub tpFM6000SetBoolQosAttributeWatermark;
sub tpFM6000SetIntQosAttributeWatermark;


###############################################################################
#
# FM6000_qos_attribute_map hash definition
#
# key:   QoS port attribute string
# value: reference to the following array
#
# array elements:
# 1st: attribute ID.
# 2nd: maximun index for this attribute.
# 3rd: reference to a display function.
# 4th: reference to a validation function.
# 5th: reference to a set attribute function.
#
###############################################################################

my %FM6000_qos_attribute_map = (
    'private_pause_off_wm'   => [$FM_QOS_PRIVATE_PAUSE_OFF_WM,
                                 12,
                                 \&tpFM6000ShowQosNumeric,
                                 \&tpFM6000ValidatePrivatePauseOffWatermark,
                                 \&tpFM6000SetIntQosAttributeWatermark],

    'private_pause_on_wm'    => [$FM_QOS_PRIVATE_PAUSE_ON_WM,
                                 12,
                                 \&tpFM6000ShowQosNumeric,
                                 \&tpFM6000ValidatePrivatePauseOnWatermark,
                                 \&tpFM6000SetIntQosAttributeWatermark],

    'rx_smp_hog_wm'          => [$FM_QOS_RX_HOG_WM,
                                 16,
                                 \&tpFM6000ShowQosNumeric,
                                 \&tpFM6000ValidateRxSmpHogWatermark,
                                 \&tpFM6000SetIntQosAttributeWatermark],

    'rx_smp_private_wm'      => [$FM_QOS_RX_PRIVATE_WM,
                                 12,
                                 \&tpFM6000ShowQosNumeric,
                                 \&tpFM6000ValidateRxSmpPrivateWatermark,
                                 \&tpFM6000SetIntQosAttributeWatermark],

    'tx_smp_hog_wm'          => [$FM_QOS_TX_HOG_WM,
                                 16,
                                 \&tpFM6000ShowQosNumeric,
                                 \&tpFM6000ValidateTxSmpHogWatermark,
                                 \&tpFM6000SetIntQosAttributeWatermark],

    'tx_smp_private_wm'      => [$FM_QOS_TX_PRIVATE_WM,
                                 2,
                                 \&tpFM6000ShowQosNumeric,
                                 \&tpFM6000ValidateTxSmpPrivateWatermark,
                                 \&tpFM6000SetIntQosAttributeWatermark],

    'tx_soft_drop_on_private'   => [$FM_QOS_TX_SOFT_DROP_ON_PRIVATE,
                                    16,
                                    \&tpFM6000ShowQosBoolean,
                                    \&tpFM6000ValidateTxSoftDropOnPrivate,
                                    \&tpFM6000SetBoolQosAttributeWatermark],

    'tx_soft_drop_on_rxmp_free' => [$FM_QOS_TX_SOFT_DROP_ON_RXMP_FREE,
                                    16,
                                    \&tpFM6000ShowQosBoolean,
                                    \&tpFM6000ValidateTxSoftDropOnRxmpFree,
                                    \&tpFM6000SetBoolQosAttributeWatermark],
);


my $attrNameWidth = 32;
my $attrNameWidthStr = "%-" . $attrNameWidth . "s";

my $attrValueWidth = 14;
my $attrValueWidthStr = "%-" . $attrValueWidth . "s";


sub tpFM6000ShowQosBoolean
{
    my ($self, $switchNum, $portListRef, $attrStr, $attrInt, $indexMax) = @_;

    my $chip = $self->{CHIP};
    my %void = (type => "fm_int", value => 0);
    my $index;
    my $status;
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

            $indexStr = sprintf("%-6s",
                                $void{value} == $FM_ENABLED ? "on" : "off");

            printf($attrValueWidthStr, $status != $FM_OK ? "NA" : $indexStr);
        }

        printf("\n");
    }

    $chip->enableErrors();
}


sub tpFM6000ShowQosNumeric
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


sub tpFM6000ValidatePrivatePauseOffWatermark
{
    my ($self, $attrStr, $wm) = @_;


    if ( !defined($wm) || ($wm < 480) || ($wm > 10485600) )
    {
        print("Invalid watermark value for $attrStr. ");
        print("The range must be between 480-10485600.\n");
        return $FM_FAIL;
    }

    return $FM_OK;
}


sub tpFM6000ValidatePrivatePauseOnWatermark
{
    my ($self, $attrStr, $wm) = @_;


    if ( !defined($wm) || ($wm < 0) || ($wm > 10485600) )
    {
        print("Invalid watermark value for $attrStr. ");
        print("The range must be between 0-10485600.\n");
        return $FM_FAIL;
    }

    return $FM_OK;
}


sub tpFM6000ValidateRxSmpHogWatermark
{
    my ($self, $attrStr, $wm) = @_;


    if ( !defined($wm) || ($wm < 0) || ($wm > 10485600) )
    {
        print("Invalid watermark value for $attrStr. ");
        print("The range must be between 0-10485600.\n");
        return $FM_FAIL;
    }

    return $FM_OK;
}


sub tpFM6000ValidateRxSmpPrivateWatermark
{
    my ($self, $attrStr, $wm) = @_;


    if ( !defined($wm) || ($wm < 0) || ($wm > 10485600) )
    {
        print("Invalid watermark value for $attrStr. ");
        print("The range must be between 0-10485600.\n");
        return $FM_FAIL;
    }

    return $FM_OK;
}


sub tpFM6000ValidateTxSmpHogWatermark
{
    my ($self, $attrStr, $wm) = @_;


    if ( !defined($wm) || ($wm < 0) || ($wm > 7863840) )
    {
        print("Invalid watermark value for $attrStr. ");
        print("The range must be between 0-7863840.\n");
        return $FM_FAIL;
    }

    return $FM_OK;
}


sub tpFM6000ValidateTxSmpPrivateWatermark
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


sub tpFM6000ValidateTxSoftDropOnPrivate
{
    my ($self, $attrStr, $wm) = @_;


    if ( !defined($wm) || ((lc($wm) ne "on") && (lc($wm) ne "off")) )
    {
        print("$attrStr must be set on or off.\n");
        return $FM_FAIL;
    }

    return $FM_OK;
}


sub tpFM6000ValidateTxSoftDropOnRxmpFree
{
    my ($self, $attrStr, $wm) = @_;


    if ( !defined($wm) || ((lc($wm) ne "on") && (lc($wm) ne "off")) )
    {
        print("$attrStr must be set on or off.\n");
        return $FM_FAIL;
    }

    return $FM_OK;
}


sub tpFM6000SetBoolQosAttributeWatermark
{
    my ($self, $sw, $attrStr, $attrId, $portListRef, $index, $wm) = @_;

    my $chip = $self->{CHIP};

    my %void = (type  => "fm_int", value => $FM_DISABLED);
    if (lc($wm) eq "on")
    {
        $void{value} = $FM_ENABLED;
    }

    foreach my $port (@$portListRef)
    {
        # Map port number for the benefit of non-SWAG Vegas. Ignore the
        # returned switchNum since it should match sw and may actually be
        # wrong for some platforms.
        my ($switchNum, $logPort) =
                $self->tpPlatformMapGlobalToLogicalPort($port);

        my $status = $chip->fmSetPortQOS($sw, $logPort, $attrId,
                                         $index, \%void);
        if ($status != $FM_OK)
        {
            printf("QoS port attribute %s cannot be set!\n", $attrStr);
            return $FM_FAIL;
        }
    }

    return $FM_OK;
}


sub tpFM6000SetIntQosAttributeWatermark
{
    my ($self, $sw, $attrStr, $attrId, $portListRef, $index, $wm) = @_;

    my $chip = $self->{CHIP};
    my %void = (type  => "fm_int", value => $wm);


    foreach my $port (@$portListRef)
    {
        # Map port number for the benefit of non-SWAG Vegas. Ignore the
        # returned switchNum since it should match sw and may actually be
        # wrong for some platforms.
        my ($switchNum, $logPort) =
                $self->tpPlatformMapGlobalToLogicalPort($port);

        my $status = $chip->fmSetPortQOS($sw, $logPort, $attrId,
                                         $index, \%void);
        if ($status != $FM_OK)
        {
            printf("QoS port attribute %s cannot be set!\n", $attrStr);
            return $FM_FAIL;
        }
    }

    return $FM_OK;
}



##@cmethod public void tpFM6000HandleShowQos(int sw, char *portListRef)
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
sub tpFM6000HandleShowQos
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

    foreach my $attrStr (sort(keys(%FM6000_qos_attribute_map)))
    {
        my $attrInfoRef = $FM6000_qos_attribute_map{$attrStr};

        &{$attrInfoRef->[2]}($self, $sw, $portListRef, $attrStr,
                             $attrInfoRef->[0], $attrInfoRef->[1]);
    }

    print("\n");
}



##@cmethod public void tpFM6000HandleSetQos(int sw, char attrStr,
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
sub tpFM6000HandleSetQos
{
    my ($self, $sw, $attrStr, $portListRef, $index, $watermark) = @_;


    if ( !exists $FM6000_qos_attribute_map{$attrStr} )
    {
        print("Invalid QoS port attribute: $attrStr!\n");
        return $FM_FAIL;
    }

    my $attrInfoRef = $FM6000_qos_attribute_map{$attrStr};
    my $maxIndex = $attrInfoRef->[1];

    if ( !defined($index) || ($index < 0) || ($index >= $maxIndex) )
    {
        print("Invalid index value. ");
        printf("The range must be between 0-%d.\n", $maxIndex - 1);
        return $FM_FAIL;
    }


    # Validate specific QoS port attribute watermark
    my $status = &{$attrInfoRef->[3]}($self, $attrStr, $watermark);


    if ($status == $FM_OK)
    {
        # Set QoS port attribute watermark
        $status = &{$attrInfoRef->[4]}($self, $sw, $attrStr,
                                       $attrInfoRef->[0], $portListRef,
                                       $index, $watermark);
    }

    return $status;
}



##@cmethod public bool tpFM6000IsPerLagPortAttribute(int sw, char attrStr)
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
sub tpFM6000IsPerLagPortAttribute
{
    my ($self, $sw, $attrStr) = @_;

    my $chip = $self->{CHIP};
    my $attrInfoRef = $FM6000_qos_attribute_map{$attrStr};

    return $chip->fmIsPerLagPortAttribute($sw, $attrInfoRef->[0]);
}

