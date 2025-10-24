# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


# source ~/work/setup/hlp.setup

hspice test00.spice

/nfs/site/home/mnystroe/mst_work/m3utils/spice/ct/AMD64_LINUX/ct test00.tr0 out

aplot out
