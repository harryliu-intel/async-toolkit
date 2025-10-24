#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

../AMD64_LINUX/exampleprog -vdd 0.1 -delp 0.1 -deln 0.1
${M3UTILS}/spice/schemagraph/schemaeval/AMD64_LINUX/schemaeval -schema ./schema.dat -data example.out -eval 'result'

