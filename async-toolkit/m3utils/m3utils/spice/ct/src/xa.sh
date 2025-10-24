#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


/nfs/sc/disks/bfn_cb_05/mnystroe/m3utils/spice/ct/AMD64_LINUX/ct -fsdb /nfs/sc/disks/bfn_cb_05/mnystroe/m3utils/spice/fsdb/src/nanosimrd -threads 1 -wthreads 1 -R 10e-12 xa.fsdb xa
