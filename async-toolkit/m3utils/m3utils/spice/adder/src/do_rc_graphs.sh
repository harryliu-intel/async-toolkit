#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


MYDIR=${M3UTILS}/spice/adder/src
PROG=${M3UTILS}/spice/schemagraph/AMD64_LINUX/graph

${PROG} -schema ${MYDIR}/adder_rc.schema -S ${MYDIR}/adder_rc.scm -dir graphs */measure.dat.stat
