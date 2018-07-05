# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Scripts/GenFrame/TCP.pm
# Creation Date:    03/01/07
# Description:      TCP frame generation support for Test Engine 2 tests
#
# INTEL CONFIDENTIAL
# Copyright 2006 - 2011 Intel Corporation. All Rights Reserved. 
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

package Scripts::GenFrame::TCP;
use strict;
use warnings;

require Exporter;
use Scripts::GenFrame::L3;
our @ISA = qw(Exporter);
our @EXPORT = qw(
    addTCPOption
);

##@method public char** addTCPOption(char** option, char **pDataArray)
# Adds a new %TCP option to an existing %TCP header
# @param[in] option A Perl reference to an array containing a %TCP option
# @param[in,out] pDataArray A Perl reference to an array containing a %TCP
# header
# @return A Perl reference to an array containing the modified %TCP header
sub addTCPOption($$)
{
    my ($option, $pDataArray) = @_;

    my $l4Index = getL4Index($pDataArray);
    if ($l4Index == -1)
    {
        return $pDataArray;
    }
    for (my $i=0 ; $i<scalar(@{$option}) ; $i++)
    {
        $pDataArray->[$l4Index + 20 + $i] = $option->[$i];
    }
    # Update the header length field.
    my $length = (($pDataArray->[$l4Index + 12] >> 4) & 0xf);
    $length += scalar(@{$option}) / 4;
    $pDataArray->[$l4Index + 12] = (($length << 4) 
                                    | ($pDataArray->[$l4Index + 12] & 0xf));
    return $pDataArray;
}


1;
