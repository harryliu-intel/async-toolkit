#!/bin/sh
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

# savefiles.sh <scriptdir> <unique-name>
scriptdir=$1
uniquename=$2
mv ${scriptdir}/${uniquename}.* ${scriptdir}/../done
