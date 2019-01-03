`ifndef __MBY_MGP_MIM_RSP_IF__
`define __MBY_MGP_MIM_RSP_IF__
import mby_mgp_bfm_pkg::*;


interface mby_mgp_mim_rsp_if (
     input  reset,
     input  cclk,
     inout  [W_REQ_ID-1:0]           req_id[NUM_MSH_ROWS-1 : 0][NUM_MSH_ROW_PORTS-1 : 0],
     inout  [W_RRSP_DEST_BLOCK-1:0]  rrsp_dest_blk [NUM_MSH_ROWS-1 : 0][NUM_MSH_ROW_PORTS-1 : 0],
     inout  [MSH_DATA_WIDTH-1:0]     data[NUM_MSH_ROWS-1 : 0][NUM_MSH_ROW_PORTS-1 : 0],
     inout                           valid[NUM_MSH_ROWS-1 : 0][NUM_MSH_ROW_PORTS-1 : 0]
     ) ;

   clocking rrsp_slv_cb @(posedge cclk);
      default input #1step output #1step;
      input  req_id;
      input  rrsp_dest_blk;
      input  data;
      input  valid;
   endclocking

   modport rrsp_slv (clocking rrsp_slv_cb);

endinterface
`endif