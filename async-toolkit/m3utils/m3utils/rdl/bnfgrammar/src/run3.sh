#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


../AMD64_LINUX/bnfgrammar -f test2.bnf -r expression -d test2_out/src -Hy test2.prec -Hl test2.lex

