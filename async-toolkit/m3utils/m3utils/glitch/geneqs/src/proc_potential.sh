#!/bin/sh
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


FILES=`awk '{print $4}' potential.txt | sort | uniq`


for file in ${FILES}; do
    grep ^async.*adwr_bin $file 2>&1 > /dev/null 
    no_adwr=$?
    grep ^async.*adrd_bin $file 2>&1 > /dev/null
    no_adrd=$?

    either=$((no_adrd + 2 * no_adwr))
#    echo adrd ${no_adrd} adwr ${no_adwr} either ${either} ${file}
    if [ ${either} -eq 3 ]; then
       echo ${file}
    fi
done
