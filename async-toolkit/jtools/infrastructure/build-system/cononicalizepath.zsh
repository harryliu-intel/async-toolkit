#!/bin/zsh
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


DIRNAME=`dirname "$1"`

PUSHD_SILENT=1 pushd "$DIRNAME"

CONONICALDIR=`pwd`
BASENAME=`basename "$1"`
echo $CONONICALDIR/$BASENAME

PUSHD_SILENT=1 popd
