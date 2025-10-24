#!/usr/bin/awk -f
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


BEGIN { FS="," }

{
    sumsq=0.0
    for (i=2; i <= NF; ++i)
        sumsq += $i*$i
    printf("%e %lf\n", $1, sqrt(sumsq)) 
}
