#!/usr/intel/bin/zsh
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


source rechar_setup_env.zsh

while true; do
    dfdata=`df . | tail -1 | awk '{print $2 " " $3 " " $4 " "$4/$2}'`
#    dudata=`du -xs ${RECHAR_SIS_WORKDIR} | awk '{print $1}'`
#    output="`date +%s` $dfdata $dudata"
    output="`date +%s` $dfdata"
    echo ${output}
    echo ${output} >> monitorspace.out

    sleep 10
done
