
`ifndef __MBY_MGP_MIM_IF__
`define __MBY_MGP_MIM_IF__
`include "mby_mgp_defines.svh"


//--------------------------------------------------------------
// Interface:    mby_mgp_mim_if
//
// Interface connecting EB and WB req/rsp with the mesh.
//
//--------------------------------------------------------------
interface mby_mgp_mim_if (
     input  reset,
     input  cclk,
     inout  [W_REQ_ID-1:0]       req_id[NUM_MSH_ROWS][NUM_MSH_ROW_PORTS],
     inout  [W_SEG_PTR-1:0]      seg_ptr[NUM_MSH_ROWS][NUM_MSH_ROW_PORTS],
     inout  [W_WD_SEL-1:0]       wd_sel[NUM_MSH_ROWS][NUM_MSH_ROW_PORTS],
     inout  [W_SEMA-1:0]         sema[NUM_MSH_ROWS][NUM_MSH_ROW_PORTS],
     inout                       valid[NUM_MSH_ROWS][NUM_MSH_ROW_PORTS],
     inout  [MSH_DATA_WIDTH-1:0] data[NUM_MSH_ROWS][NUM_MSH_ROW_PORTS]
     ) ;

   clocking op_mst_cb @(posedge cclk);
      default input #1step output #1step;
      output  req_id;
      output  seg_ptr;
      output  wd_sel;
      output  sema;
      output valid;
   endclocking

   clocking op_slv_cb @(posedge cclk);
      default input #1step output #1step;
      input  req_id;
      input  seg_ptr;
      input  wd_sel;
      input  sema;
      input valid;
   endclocking

   modport op_mst (clocking op_mst_cb);
   modport op_slv (clocking op_slv_cb);

endinterface
`endif

