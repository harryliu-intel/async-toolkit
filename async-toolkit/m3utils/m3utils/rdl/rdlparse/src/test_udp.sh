#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


BD=/nfs/sc/disks/hlp_0015/mnystroe/git/meta-git/bin
WD=/p/hlp/romanpar/RDLTest/

cd ${WD}

${BD}/svpp < lib_udp.rdl | ${BD}/perlfe | perl > /tmp/tmp$$

${BD}/parserdl --print-user-def-properties < /tmp/tmp$$

