#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

GLITCH=/nfs/sc/disks/bfn_pd_cb_02/mnystroe/m3utils/glitch/AMD64_LINUX/glitch
LIMIT="-limit 64"

cd rundir

for dir in *; do

    cd ${dir}

    for file in *.glitch; do
        echo ${dir}/${file}
        time ${GLITCH} ${LIMIT} -f ${file}
    done

    cd ..
    
done
