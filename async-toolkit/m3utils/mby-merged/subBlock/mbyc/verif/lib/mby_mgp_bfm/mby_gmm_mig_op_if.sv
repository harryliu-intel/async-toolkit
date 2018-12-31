
`ifndef __MBY_GMM_MIG_OP_IF__
`define __MBY_GMM_MIG_OP_IF__

import mby_mgp_bfm_pkg::*;

//----------------------------------------------------------------
// Interface:    mby_gmm_mig_op_if
//
//  Global interface to Mesh
//
//-----------------------------------------------------------------
   interface mby_gmm_mig_op_if (
     inout  reset,
     inout  cclk,
     inout  req_id_mig,
     inout  seg_ptr_mig,
     inout  wd_sel_mig,
     inout  sema_mig
     ) ;

   clocking op_mst_cb @(posedge cclk);
      default input #1step output #1step;
      output  req_id_mig;
      output  seg_ptr_mig;
      output  wd_sel_mig;
      output  sema_mig;
   endclocking

   clocking op_slv_cb @(posedge cclk);
      default input #1step output #1step;
      input  req_id_mig;
      input  seg_ptr_mig;
      input  wd_sel_mig;
      input  sema_mig;
   endclocking

   modport op_mst (clocking op_mst_cb);
   modport op_slv (clocking op_slv_cb);

endinterface
`endif

