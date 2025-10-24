#!/bin/sh
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

chmod +x *.script

for script in *.script; do
	qsub -p -400 -l a=lx24-amd64,mem=4G,centos=5 -now no $script
	#sleep 30
done
