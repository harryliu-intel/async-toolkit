#! /bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


../AMD64_LINUX/lambbuilder -vdd 0.75 -w 30 -d 16 -clk 1e9 -prog read -f hspice -pm io -step 1e-11 -global VDD -global VSS
