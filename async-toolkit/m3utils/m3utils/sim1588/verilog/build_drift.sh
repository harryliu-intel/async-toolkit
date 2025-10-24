#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

#vcs -sverilog sim/tsu_tb.sv -debug_all -top tsu_tb_mark -timescale=1fs/1fs
vcs -sverilog sim/tsu_tb_drift.sv -debug_all -top tsu_tb_mark -timescale=1fs/1fs
