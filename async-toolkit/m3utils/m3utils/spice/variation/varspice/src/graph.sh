#!/bin/sh
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


# graph results of sigma variation optimizations
# run summary.sh FIRST

anal=${M3UTILS}/spice/variation/varspice/src/anal.awk

for f in result_*p?.csv; do
    $anal $f | tail -1 | awk '{print $2 " " $1}'
done
