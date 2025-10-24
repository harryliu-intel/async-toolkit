#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


time ../AMD64_LINUX/spiceflat tcam.sp ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl
sed 's/ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_//' gprof.out > s_gprof.out
sed 's/ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_//' bflat.out > s_bflat.out
