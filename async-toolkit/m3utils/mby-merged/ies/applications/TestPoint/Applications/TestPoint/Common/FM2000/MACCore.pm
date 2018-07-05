# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FM2000/MACCore.pm
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

package Applications::TestPoint::Common::FM2000::MACCore;
use strict;
use warnings;

use base qw(Exporter);
use SDK qw(/^\$/);

our @EXPORT = qw(
    tpFM2000CreateMACEntry
);

sub tpFM2000CreateMACEntry
{
    my ($self, $switchNum, $entry, $macAddress, $vlanID, $type, $portList) = @_;

    $entry->{"macAddress"} = $macAddress;
    $entry->{"vlanID"} = $vlanID;
    $entry->{"type"} = $type eq "locked" ? 0 : 1;  # static entry is locked
    $entry->{"age"} = 1;                           # the entry is not aged
    $entry->{"destMask"} = 0;
    $entry->{"port"} = -1;

    foreach my $globalPort (@$portList)
    {
        # Map port number for the benefit of non-SWAG Vegas. Ignore the
        # returned sw since it should match switchNum and may actually be
        # wrong for some platforms.
        my ($sw, $logicalPort) =
                          $self->tpPlatformMapGlobalToLogicalPort($globalPort);

        if ($switchNum == $sw)
        {
            $entry->{"destMask"} |= (1 << $logicalPort);
        }
    }

    return $FM_OK;
}

1;
