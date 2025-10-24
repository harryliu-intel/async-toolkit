#!/bin/sh 
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

#

# summarize optimization sim data into results files
# run BEFORE graph.sh

for dir in *p?; do
	echo summarizing $dir
	cat $dir/00*/result.csv > result_${dir}.csv &
done	

echo waiting
wait
