#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/nfs/site/disks/tfc_fe_zsc7_01/mnystroe/aplot
export LD_LIBRARY_PATH

/nfs/site/disks/tfc_fe_zsc7_01/mnystroe/aplot/aplot $*
