`ifndef __MBY_MGP_WREQ_IF__
`define __MBY_MGP_WREQ_IF__
import mby_mgp_bfm_pkg::*;
import mby_msh_pkg::*;

interface mby_mgp_wreq_if (
   input  reset,
   input  cclk,
   inout  [26:0]  wr_req[NUM_MSH_ROWS-1 : 0][NUM_MSH_ROW_PORTS-1 : 0]
   ) ;

   clocking wreq_mst_cb @(posedge cclk);
      default input #1step output #1step;
      output wr_req;
   endclocking

   clocking wreq_slv_cb @(posedge cclk);
      default input #1step output #1step;
      input  wr_req;
   endclocking

   modport wreq_mst (clocking wreq_mst_cb);
   modport wreq_slv (clocking wreq_slv_cb);
   
endinterface
`endif