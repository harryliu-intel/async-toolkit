#!/bin/sh
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


for dir in *.run; do
    pushd $dir
    ${M3UTILS}/spice/fwr_clk_tc2/AMD64_LINUX/measure
    popd
done
