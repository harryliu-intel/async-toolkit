#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

#
../AMD64_LINUX/spicedecorate -i test.sp -root adder_tb_Width32_MaxCarryChain31_AdderType2 -noprobe -S vth.scm -modify M '(modify-mos-vth -0.20 +0.20)' -o test_vth__out.sp
