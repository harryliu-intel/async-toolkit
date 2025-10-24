# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

#/bin/sh -x
gzip -dc TMNK01_T8R646-01G1_CP1_20201027151803.stdf.gz | time ../AMD64_LINUX/stdfparser -f -
