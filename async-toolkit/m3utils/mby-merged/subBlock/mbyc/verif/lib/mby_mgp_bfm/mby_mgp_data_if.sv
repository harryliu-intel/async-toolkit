`ifndef __MBY_MGP_DATA_IF__
`define __MBY_MGP_DATA_IF__
import mby_mgp_bfm_pkg::*;
import mby_msh_pkg::*;

interface mby_mgp_data_if (
   input  reset,
   input  cclk,
   inout  [531:0] req_data[NUM_MSH_ROWS-1 : 0][NUM_MSH_ROW_PORTS-1 : 0]
   ) ;

   clocking data_mst_cb @(posedge cclk);
      default input #1step output #1step;
      output  req_data;
   endclocking

   clocking data_slv_cb @(posedge cclk);
      default input #1step output #1step;
      input  req_data;
   endclocking

   modport data_mst (clocking data_mst_cb);
   modport data_slv (clocking data_slv_cb);

endinterface
`endif