#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


. ./bins.script

mkdir ${TGTDIR} || echo Proceeding...

for p in ${PROGS}; do
	ln -sf $p ${TGTDIR}
done

mkdir ${SGTDIR} || echo Proceeding...

for p in ${SCRIPTS}; do
	ln -sf $p ${SGTDIR}
done

