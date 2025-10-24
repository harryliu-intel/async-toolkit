#!/bin/sh 
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


REM=/nfs/sc/disks/bfn_pd_cb_02/mnystroe/m3utils/glitch/geneqs/src/remaining.txt

nodes=`cat ${REM} | xargs -n1 basename | sed 's/\..*$//'`

for node in ${nodes}; do
    ../procnode.awk ${node} *
done

#    for file in *; do
#        grep ${node} ${file} 2>&1 > /dev/null
#        if [ $? -eq 0 ]; then
#            grep "FOUND GLITCH" ${file}
#        fi
#    done
