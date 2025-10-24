#!/bin/sh
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


for f in *.out; do
	root=`echo $f | sed s/.scm.out//`
	echo ============ $root ============ 
	find . ../scripts -name ${root}\* -exec mv {} ../done \;

done
