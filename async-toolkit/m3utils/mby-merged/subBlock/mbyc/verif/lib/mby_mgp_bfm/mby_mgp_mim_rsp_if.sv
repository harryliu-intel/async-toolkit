`ifndef __MBY_MGP_MIM_RSP_IF__
`define __MBY_MGP_MIM_RSP_IF__
import mby_mgp_bfm_pkg::*;


interface mby_mgp_mim_rsp_if (
     input  reset,
     input  cclk,
     inout  [W_REQ_ID-1:0]           req_id[NUM_MSH_ROWS][NUM_MSH_ROW_PORTS],
     inout  [W_RRSP_DEST_BLOCK-1:0]  rrsp_dest_blk [NUM_MSH_ROWS][NUM_MSH_ROW_PORTS],
     inout  [MSH_DATA_WIDTH-1:0]     data[NUM_MSH_ROWS][NUM_MSH_ROW_PORTS]
     ) ;

   clocking rrsp_mst_cb @(posedge cclk);
      default input #1step output #1step;
      output  req_id;
      output  rrsp_dest_blk;
      output  data;
   endclocking

   clocking rrsp_slv_cb @(posedge cclk);
      default input #1step output #1step;
      input  req_id;
      input  rrsp_dest_blk;
      input  data;
   endclocking

   modport rrsp_mst (clocking rrsp_mst_cb);
   modport rrsp_slv (clocking rrsp_slv_cb);

endinterface
`endif