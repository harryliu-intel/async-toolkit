#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


time ../AMD64_LINUX/genpg -D MODULE_NAME m_full_test -skipholes -bits 48 -sv full.sv  -crif crif.xml -G 4 MST_PG0 MST_PG1 MST_PG2 MST_PG3 -defpgnm DEFAULT_PG -basestrapbits some_pkg::BASE_STRAP_BITS -T DEFAULT 
