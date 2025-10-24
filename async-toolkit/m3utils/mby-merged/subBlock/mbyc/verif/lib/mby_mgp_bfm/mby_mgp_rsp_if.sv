// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

`ifndef __MBY_MGP_RSP_IF__
`define __MBY_MGP_RSP_IF__
import mby_mgp_bfm_pkg::*;
import mby_msh_pkg::*;

interface mby_mgp_rsp_if (
   input  reset,
   input  cclk,
   inout  [27:0] rd_rsp[NUM_MSH_ROWS-1 : 0][NUM_MSH_ROW_PORTS-1 : 0]
   ) ;

   clocking rsp_mst_cb @(posedge cclk);
      default input #1step output #1step;
      output rd_rsp;
   endclocking

   clocking rsp_slv_cb @(posedge cclk);
      default input #1step output #1step;
      input  rd_rsp;
   endclocking

   modport rsp_mst (clocking rsp_mst_cb);
   modport rsp_slv (clocking rsp_slv_cb);

endinterface
`endif