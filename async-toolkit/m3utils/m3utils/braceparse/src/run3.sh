#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


zcat example2.net.gz | ../AMD64_LINUX/netlist2goxrpt -f - -r test -T gox -l 3
