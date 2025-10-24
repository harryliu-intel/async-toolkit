#!/bin/sh -x 
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


Z=2
THRESH=ulvt
WORKDIR=run_${Z}_${THRESH}

/bin/rm -rf  $WORKDIR

../AMD64_LINUX/varopt -T ../../varosc/src/circuit.sp -thresh $THRESH -z $Z -r $WORKDIR $* |& tee run_opt_$WORKDIR.0
