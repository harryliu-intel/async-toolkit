#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


rm -rf inv_out

time ../AMD64_LINUX/spiceanalyze -f inv.spi -p power.txt -o inv_out

scp -r inv_out mika@10.0.0.8:
