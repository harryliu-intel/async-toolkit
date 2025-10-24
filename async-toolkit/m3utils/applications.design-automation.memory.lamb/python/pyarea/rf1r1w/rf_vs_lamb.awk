#!/usr/bin/awk -f 
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

#
# compute area ratio between LAMB and RF1R1W
# RF data from Bin in Excel (converted to CSV)
# assumption that depth in $1
#                 width    $2
#                 area     $6 (/[sq microns])
#
# Author: mika.nystroem@intel.com
# June, 2022
#

BEGIN { FS="," }
    
{
    cmd = "../area.py -q -m -d " $1 " -w " $2
    cmd | getline q
    lambratio = q / $6
    print q ", " lambratio ", " $0 
    close(cmd)
    $0=""
}

