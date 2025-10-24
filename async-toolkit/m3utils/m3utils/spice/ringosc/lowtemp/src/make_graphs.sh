#!/bin/sh
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


GRAPH=${M3UTILS}/spice/schemagraph/program/AMD64_LINUX/schemagraph
HOME=${M3UTILS}/spice/ringosc/lowtemp/src
SCHEMA=${HOME}/schema.dat
DEFS=${HOME}/defs.scm

${GRAPH} -schema ${SCHEMA} -S ${DEFS} -dir graphs */measure.dat.stat
