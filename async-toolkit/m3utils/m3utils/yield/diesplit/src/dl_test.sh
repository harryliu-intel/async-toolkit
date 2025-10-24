# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

#/bin/sh -x
cc -std=c99 -O6 -shared -o liba.so a.c
cc -std=c99 testmain.c -ldl -lm
./a.out
