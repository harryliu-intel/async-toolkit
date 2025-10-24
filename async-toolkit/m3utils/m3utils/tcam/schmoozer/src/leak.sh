#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

schmoozer -design sdg64 -schmooze leakvar 2>&1 | tee leak.out
