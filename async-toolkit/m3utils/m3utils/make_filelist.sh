#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

p4 files ... | grep -v " delete " | sed 's,^//hlp/main/hw/tools/meta\([^ ]*\)#.*$,.\1,' > intel-p4.filelist

find . -type f > intel-find.filelist
