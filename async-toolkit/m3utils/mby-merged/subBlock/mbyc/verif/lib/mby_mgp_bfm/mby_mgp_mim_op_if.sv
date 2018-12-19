
`ifndef __MBY_MGP_MIM_OP_IF__
`define __MBY_MGP_MIM_OP_IF__

import mby_mgp_bfm_pkg::*;

   interface mby_mgp_mim_op_if (
     inout  reset,
     inout  cclk,
     inout  req_id,
     inout  seg_ptr,
     inout  wd_sel,
     inout  sema
     ) ;

   clocking op_mst_cb @(posedge cclk);
      default input #1step output #1step;
      output  req_id;
      output  seg_ptr;
      output  wd_sel;
      output  sema;
   endclocking

   clocking op_slv_cb @(posedge cclk);
      default input #1step output #1step;
      input  req_id;
      input  seg_ptr;
      input  wd_sel;
      input  sema;
   endclocking

   modport op_mst (clocking op_mst_cb);
   modport op_slv (clocking op_slv_cb);

endinterface
`endif

