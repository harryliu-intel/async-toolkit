
`ifndef __MBY_MGP_MIM_REQ_IF__
`define __MBY_MGP_MIM_REQ_IF__
import mby_mgp_bfm_pkg::*;

interface mby_mgp_mim_req_if (
     input  reset,
     input  cclk,
     inout  [W_REQ_ID-1:0]       req_id[NUM_MSH_ROWS-1 : 0][NUM_MSH_ROW_PORTS-1 : 0],
     inout  [W_SEG_PTR-1:0]      seg_ptr[NUM_MSH_ROWS-1 : 0][NUM_MSH_ROW_PORTS-1 : 0],
     inout  [W_WD_SEL-1:0]       wd_sel[NUM_MSH_ROWS-1 : 0][NUM_MSH_ROW_PORTS-1 : 0],
     inout  [W_SEMA-1:0]         sema[NUM_MSH_ROWS-1 : 0][NUM_MSH_ROW_PORTS-1 : 0],
     inout                       valid[NUM_MSH_ROWS-1 : 0][NUM_MSH_ROW_PORTS-1 : 0],
     inout  [MSH_DATA_WIDTH-1:0] data[NUM_MSH_ROWS-1 : 0][NUM_MSH_ROW_PORTS-1 : 0]
     ) ;

   clocking req_mst_cb @(posedge cclk);
      default input #1step output #1step;
      output  req_id;
      output  seg_ptr;
      output  wd_sel;
      output  sema;
      output  valid;
      output  data;
   endclocking

   clocking req_slv_cb @(posedge cclk);
      default input #1step output #1step;
      input  req_id;
      input  seg_ptr;
      input  wd_sel;
      input  sema;
      input  valid;
      input  data;
   endclocking

   modport req_mst (clocking req_mst_cb);
   modport req_slv (clocking req_slv_cb);

endinterface
`endif

