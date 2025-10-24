#!/bin/sh 
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


rdls=*.rdl

for f in ${rdls}; do
#	echo checking ${f}
	grep ${f} *.rdl > /dev/null
	st=$?
#	echo $st
	if [ "$st" -ne "0" ]; then
		echo NOT FOUND: ${f} 
	fi
done
