// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

mby_mem_1r1w_if    #(.W_DATA(42),    .W_DEEP(4),   .W_INST(8) )    egr_mri_rreq_fifo_if();
mby_mem_1r1w_if    #(.W_DATA(528),    .W_DEEP(4),   .W_INST(8) )    egr_mri_rrsp_fifo_if();
mby_mem_1r1w_if    #(.W_DATA(528),    .W_DEEP(3),   .W_INST(6) )    egr_mri_rrsp_mim_stall_regs_if();
