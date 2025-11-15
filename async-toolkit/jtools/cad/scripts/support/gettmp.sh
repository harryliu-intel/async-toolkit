#!/bin/bash
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

if [[ -n "$TMP" && -d "$TMP" && -w "$TMP" ]]; then echo "$TMP"; exit 0; fi
for f in /scratch/*.q; do
if [[ -w "$f" ]]; then TMP=$f; fi;
done
if [[ -z "$TMP" ]]; then TMP=/scratch; fi
echo $TMP
