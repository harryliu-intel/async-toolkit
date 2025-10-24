#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


MYDIR=${M3UTILS}/spice/clock_gen4_latch/src
PROG=${M3UTILS}/spice/schemagraph/AMD64_LINUX/graph

#NOLABEL=
NOLABEL=-nolabel

${PROG} ${NOLABEL} -schema ${MYDIR}/clockgen4.schema -S ${MYDIR}/clockgen4.scm -dir graphs *.run/measure.dat.stat
