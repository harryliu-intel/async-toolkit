#! /bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


# usage : $0 <width> <depth> <prog>

width=$1
depth=$2
prog=$3

####

out="out_${width}b_${depth}d_${prog}"

../AMD64_LINUX/lambbuilder -vdd 0.75 -w ${width} -d ${depth} -clk 1e9 -prog ${prog} -f hspice -pm io -step 1e-11 -global VDD -global VSS -o ${out}

echo "hspice -i ${out}.spice -o ${out} -x" > runspice.sh
