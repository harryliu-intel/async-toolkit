# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FM6000/WatermarkCore.pm
# Creation Date:    11/03/11
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

package Applications::TestPoint::Common::FM6000::WatermarkCore;
use strict;
use warnings;

use base qw(Exporter);

use List::Util qw(sum);
use SDKScalars;
use Applications::TestPoint::Common::Messages;
use Scripts::Utilities;

our @EXPORT = qw(
    tpFM6000HandleShowMemory
    tpFM6000HandleShowWatermarks
    tpFM6000HandleShowDbgQOS
    tpFM6000HandleShowSwitchMap
);

##@method void tpFM6000HandleShowSwpriMap()
# Handler for showing the switch mapping tables
#
#   @param[in] sw The switch number
#   @param[in] map The mapping table to show
#
sub tpFM6000HandleShowSwpriMap
{
    my ($self, $sw, $map) = @_;

    my $chip = $self->{CHIP};
    my $status = $FM_OK;
    my $attr;

    if (lc($map) eq "isl")
    {
        # This map will show several attribute tables, only this attribute
        # needs to be specified
        $attr = $FM_QOS_SWPRI_RXMP_MAP;
    }
    elsif (lc($map) eq "tc")
    {
        $attr = $FM_QOS_TC_SMP_MAP;
    }
    else
    {
        $status = $FM_ERR_INVALID_ARGUMENT;
        printf("ERROR: Switch #%d: %d=%s\n", $sw, $status,
            $chip->fmErrorMsg($status));
        return $status;
    }

    $chip->fmDbgDumpSwpriMap($sw, $attr);
}

##@method void tpFM6000HandleShowPortIdxMap()
# Handler for showing the port index based mapping tables
#
#   @param[in] sw The switch number
#   @param[in] port The port to show the mapping tables for
#   @param[in] map The mapping table to show
#
sub tpFM6000HandleShowPortIdxMap
{
    my ($self, $sw, $port, $map) = @_;

    my $chip = $self->{CHIP};
    my $status = $FM_OK;
    my $attr;

    if (lc($map) eq "tc_pc")
    {
        $attr = $FM_QOS_TC_PC_MAP;
    }
    elsif (lc($map) eq "pc_rxmp")
    {
        $attr = $FM_QOS_PC_RXMP_MAP;
    }
    else
    {
        $status = $FM_ERR_INVALID_ARGUMENT;
        printf("ERROR: Switch #%d: %d=%s\n", $sw, $status,
            $chip->fmErrorMsg($status));
        return $status;
    }

    $chip->fmDbgDumpPortIdxMap($sw, $port, $attr);
}

##@method void tpFM6000HandleShowDbgQOS()
# Handler for showing the QOS state parameters
#
#   @param[in] sw The switch number
#   @param[in] port The port number 
#
sub tpFM6000HandleShowDbgQOS
{
    my ($self, $sw, $port) = @_;

    my $chip = $self->{CHIP};

    if (!defined $port)
    {
        $port = -1;
    }

    $chip->fmDbgDumpQOS($sw, $port);
}

##@method void tpFM6000HandleShowMemory()
# Handler for showing the QOS memory
#
#   @param[in] sw The switch number
#   @param[in] rxPort is the RX port
#   @param[in] txPort is the TX port (FM6000 only)
#   @param[in] rxmp is the RX memory partition (FM6000 only)
#   @param[in] txmp is the TX memory partition (FM6000 only)
#   @param[in] bsg is the BSG number (FM6000 only)
#   @param[in] useSegments determines whether to show segments (1) or 
#              bytes (0). (FM6000 only)
#
sub tpFM6000HandleShowMemory
{
    my ($self, $sw, $rxPort, $txPort, $rxmp, $txmp, $bsg, $useSegments) = @_;
    my $chip = $self->{CHIP};

    # show switch memory <rxPort> <txPort> <rxmp> <txmp> <bsg>
    
    if ( (!defined $txPort) || ($txPort eq "none") )
    {
        $txPort = -2;
    }
    elsif ( $txPort eq "all" )
    {
        $txPort = -1;
    }

    if ( (!defined $rxmp) || ($rxmp eq "none") )
    {
        $rxmp = -2;
    }
    elsif ( $rxmp eq "all" )
    {
        $rxmp = -1;
    }

    if ( (!defined $txmp) || ($txmp eq "none") )
    {
        $txmp = -2;
    }
    elsif ( $txmp eq "all" )
    {
        $txmp = -1;
    }

    if ( (!defined $bsg) || ($bsg eq "none") )
    {
        $bsg = -2;
    }
    elsif ( $bsg eq "all" )
    {
        $bsg = -1;
    }

    $useSegments = ((defined $useSegments) && ($useSegments eq "segments")) ?
                   $TRUE : $FALSE;
    
    #foreach my $sw ($self->tpGetSwitches)
    #{
        if ( (!defined $rxPort) || ($rxPort eq "all") || ($rxPort eq "none") )
        {
            if ( (!defined $rxPort) || ($rxPort eq "none") )
            {
                $rxPort = -2;
            }
            elsif ( $rxPort eq "all" )
            {
               $rxPort = -1;
            }

            my $status = $chip->fmDbgDumpMemoryUsageV2($sw, 
                                                       $rxPort, 
                                                       $txPort,
                                                       $rxmp, 
                                                       $txmp, 
                                                       $bsg,
                                                       $useSegments);

            if ($status != $FM_OK)
            {
                printf("ERROR: Switch #%d: %d=%s\n", $sw, $status,
                    $chip->fmErrorMsg($status));
                return $status;
            }
        }
        else
        {
            my @portList = 
                $self->validateList($rxPort, $self->tpPlatformGetPortRange());

            foreach my $p (@portList)
            {
                my $status = $chip->fmDbgDumpMemoryUsageV2($sw, 
                                                           $p, 
                                                           $txPort,
                                                           $rxmp, 
                                                           $txmp, 
                                                           $bsg,
                                                           $useSegments);

                if ($status != $FM_OK)
                {
                    printf("ERROR: Switch #%d: %d=%s\n", $sw, $status,
                        $chip->fmErrorMsg($status));
                    return $status;
                }
            }
        }
    #}
}

##@method void tpFM6000HandleShowWatermarks()
# Handler for showing the QOS watermarks
#
#   @param[in] sw The switch number
#   @param[in] port is the RX port
#   @param[in] txPort is the TX port
#   @param[in] rxmp is the RX memory partition
#   @param[in] txmp is the TX memory partition
#   @param[in] islPri is the ISL (switch) priority
#
sub tpFM6000HandleShowWatermarks
{
    my ($self, $sw, $port, $txPort, $rxmp, $txmp, $islPri) = @_;
    my $chip = $self->{CHIP};

    # show switch watermarks <port> <txPort> <rxmp> <txmp> <islPri>

    if ( (!defined($islPri)) || ($islPri eq "none") )
    {
        $islPri = -2;
    }
    elsif ( $islPri eq "all" )
    {
        $islPri = -1;
    }
    
    if ( (!defined($rxmp)) || ($rxmp eq "none") )
    {
        $rxmp = -2;
    }
    elsif ( $rxmp eq "all" )
    {
        $rxmp = -1;
    }
    
    if ( (!defined($txmp)) || ($txmp eq "none") )
    {
        $txmp = -2;
    }
    elsif ( $txmp eq "all" )
    {
        $txmp = -1;
    }

    if ( (!defined $txPort) || ($txPort eq "none") )
    {
        $txPort = -2;
    }
    elsif ( $txPort eq "all" )
    {
        $txPort = -1;
    }
    
    #foreach my $sw ($self->tpGetSwitches)
    #{
        if ( (!defined $port) || ($port eq "all") || ($port eq "none") )
        {
            if ( (!defined $port) || ($port eq "none") )
            {
               $port = -2;
            }
            elsif ( $port eq "all" )
            {
               $port = -1;
            }

            my $status = $chip->fmDbgDumpWatermarksV2($sw, $port, $txPort,
                                                      $rxmp, $txmp, $islPri);

            if ($status != $FM_OK)
            {
                printf("ERROR: Switch #%d: %d=%s\n", $sw, $status,
                       $chip->fmErrorMsg($status));
                return $status;
            }
        }
        else
        {
            my @portList = 
                $self->validateList($port, $self->tpPlatformGetPortRange());
            
            foreach my $p (@portList)
            {
                my $status = $chip->fmDbgDumpWatermarksV2($sw, $p, $txPort, 
                                                          $rxmp, $txmp, 
                                                          $islPri);

                if ($status != $FM_OK)
                {
                    printf("ERROR: Switch #%d: %d=%s\n", $sw, $status,
                           $chip->fmErrorMsg($status));
                    return $status;
                }
            }
        }
    #}
}

1;
