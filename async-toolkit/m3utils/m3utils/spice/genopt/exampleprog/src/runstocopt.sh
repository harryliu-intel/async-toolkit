#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


genopt=${M3UTILS}/spice/genopt/AMD64_LINUX/chopstix

${genopt} -setparam silly 12 -S defs.scm examplestocopt.scm

