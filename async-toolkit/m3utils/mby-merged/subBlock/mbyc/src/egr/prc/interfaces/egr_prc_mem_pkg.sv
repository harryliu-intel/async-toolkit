// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

`ifndef EGR_PRC_MEM_PKG
`define EGR_PRC_MEM_PKG

package egr_prc_mem_pkg;

localparam EGR_PRC_RREQ_FIFO_DEPTH = 8;
localparam EGR_PRC_RREQ_FIFO_WIDTH = 64;
localparam EGR_PRC_RREQ_FIFO_INST  = 4;

typedef logic [$clog2(EGR_PRC_RREQ_FIFO_DEPTH)-1:0] egr_prc_rreq_fifo_depth_t;
typedef logic         [EGR_PRC_RREQ_FIFO_WIDTH-1:0] egr_prc_rreq_fifo_width_t;


endpackage:egr_prc_mem_pkg
`endif
