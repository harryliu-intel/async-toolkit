#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

. builddefs.sh

cp mains/${whichtest}_Main.m3 build/src/Main.m3

cd build/src
cm3 -x
