#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

time ../AMD64_LINUX/genpg -skipholes -bits 28 -sv full.sv \
-G 8 PG0 PG1 PG2 PG3 PG4 PG5 PG6 PG7 \
-defpgnm DEFAULT_PG \
/nfs/sc/disks/hlp_0100/yuqisong/hlp-pg-rtl/hlp-work/src/srdl/policy_group.csv 
