// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

`ifndef __MBY_MGP_RREQ_IF__
`define __MBY_MGP_RREQ_IF__
import mby_mgp_bfm_pkg::*;
import mby_msh_pkg::*;

interface mby_mgp_rreq_if (
   input  reset,
   input  cclk,
   inout  [42:0]  rd_req[NUM_MSH_ROWS-1 : 0][NUM_MSH_ROW_PORTS-1 : 0]
   ) ;

   clocking rreq_mst_cb @(posedge cclk);
      default input #1step output #1step;
      output  rd_req;
   endclocking

   clocking rreq_slv_cb @(posedge cclk);
      default input #1step output #1step;
      input  rd_req;
   endclocking

   modport rreq_mst (clocking rreq_mst_cb);
   modport rreq_slv (clocking rreq_slv_cb);

endinterface
`endif