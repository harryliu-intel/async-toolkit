#!/bin/sh
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


path=$1
shift

~/work/m3utils/spice/schemagraph/schemaeval/AMD64_LINUX/schemaeval -schema ~/work/m3utils/spice/ringosc/lowtemp/src/schema.dat -scm ~/work/m3utils/spice/ringosc/lowtemp/src/defs.scm -data $path -eval "$*"
