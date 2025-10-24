#! /bin/csh -f
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


nbjob run --qslot /bfn/be --target sc_express /p/hdk/cad/fusioncompiler/R-2020.09-SP5-T-20211019/bin/icc2_lm_shell -x 'source ../../gtr/tcl/gtr_main.tcl; gtr_lamb_gen_views -max_data_depth 8 -data_width 4 -tech_node n5; exit'
