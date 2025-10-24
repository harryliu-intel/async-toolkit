#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


for file in /nfs/sc/disks/mst_104/mwrighto/mst-mst-a0/results/regflow/fxp/fxp_osxml/ControlRegisters/*; do
	time ../AMD64_LINUX/testxmlparse $file 
done

