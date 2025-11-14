#!/bin/zsh
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


directory=$1

if [[ ! -d "$directory" ]] ; then
  mkdir -p "$directory"
fi
