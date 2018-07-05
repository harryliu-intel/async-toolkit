# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FM4000/MACCore.pm
# Creation Date:    Sept. 15, 2008
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

package Applications::TestPoint::Common::FM4000::MACCore;
use strict;
use warnings;

use base qw(Exporter);

use SDKScalars;

our @EXPORT = qw(
    tpFM4000CreateMACEntry
);

sub tpFM4000CreateMACEntry
{
    my ($self, $switchNum, $entry, $macAddress, $vlanID, $type, $portList,
        $portIsLag) = @_;

    if (!defined $portIsLag)
    {
        $portIsLag = 0;
    }

    $entry->{"macAddress"} = $macAddress;

    SWITCH: {
        $type eq "locked" && do {
                                    $entry->{"type"} = $FM_ADDRESS_STATIC;
                                    last SWITCH;                                           
                                };

        $type eq "unlocked" && do {
                                    $entry->{"type"} = $FM_ADDRESS_DYNAMIC;
                                    last SWITCH;                                           
                                };

        $type eq "secureLocked" && do {
                                    $entry->{"type"} = $FM_ADDRESS_SECURE_STATIC;
                                    last SWITCH;                                           
                                };

        $type eq "secureUnlocked" && do {
                                    $entry->{"type"} = $FM_ADDRESS_SECURE_DYNAMIC;
                                    last SWITCH;                                           
                                };

    }   # end of switch statement 

    $entry->{"vlanID"} = $vlanID;
    $entry->{"age"} = 1;                           # the entry is not aged

    if (scalar(@$portList) == 1)
    {
        $entry->{"destMask"} = 0xffffffff;

        my $globalPort = $portList->[0];

        my ($sw, $logicalPort);
        if ($portIsLag)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned sw since it should match switchNum and may actually be
            # wrong for some platforms.
            ($sw, $logicalPort) =
                       $self->tpPlatformMapGlobalToLogicalLAG($globalPort);
        }
        else
        {
            ($sw, $logicalPort) =
                        $self->tpPlatformMapGlobalToLogicalPort($globalPort);
        }

        $entry->{"port"} = $logicalPort;
    }
    elsif ((scalar(@$portList) > 1) && ($type eq "locked"))
    {
        $entry->{"destMask"} = 0;
        $entry->{"port"} = -1;

        foreach my $globalPort (@$portList)
        {
            # Map port number for the benefit of non-SWAG Vegas. Ignore the
            # returned sw since it should match switchNum and may actually be
            # wrong for some platforms.
             my ($sw, $logicalPort) =
                              $self->tpPlatformMapGlobalToLogicalPort($globalPort);

            $entry->{"destMask"} |= (1 << $logicalPort);
        }
    }
    else
    {
        # Don't allow a multi-port mask for a dynamic entry because there
        # is no way to purge by port.
        printf("Dynamic entries must specify only a single port!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    return $FM_OK;
}

1;
