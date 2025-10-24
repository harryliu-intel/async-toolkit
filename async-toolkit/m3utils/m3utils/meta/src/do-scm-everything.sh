#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


# clean up scripts dir
rm -rf ../scripts
mkdir ../scripts


./runme_scms.sh chip.alta.scheduler.SCHEDULER\(76\).1000
./runme_scms.sh chip.alta.fsched.FRAME_SCHEDULER\(76\).1000
./runme_scms.sh chip.alta.port.PORT4.1000
