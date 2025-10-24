#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

NB="nbjob run --target sc_normal --qslot /bfn/fe --class \"SLES12&&1G\""
GLITCH=/nfs/sc/disks/bfn_pd_cb_02/mnystroe/m3utils/glitch/AMD64_LINUX/glitch
LIMIT="-limit 64"

for file in *.glitch; do
    ${NB} ${GLITCH} ${LIMIT} -f ${file}
done
