#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


../AMD64_LINUX/bnfgrammar -f ../../csrspec/src/csrspec.bnf -r source_text -d ../../csrspec/AMD64_LINUX -Hy ../../csrspec/src/csrspec.y.0 -Hl ../../csrspec/src/csrspec.l.0 -Ht ../../csrspec/src/csrspec.t.0

