
`ifndef __MBY_MESH_OP_IF__
`define __MBY_MESH_OP_IF__

interface mby_mesh_op_if (
   input        clk,
   input        rst,
   inout [15:0] op_id,
   inout        ps,
   inout [3:0]  py,
   inout [2:0]  mx,
   inout [3:0]  my,
   inout [13:0] addr,
   inout [1:0]  sema,
   inout [7:0]  age
);
   
   clocking op_mst_cb @(posedge clk);
      default input #1step output #1step;
      output  clk;
      output  rst;
      output  op_id;
      output  ps;
      output  py;
      output  mx;
      output  my;  
      output  addr;
      output  sema;
      output  age;
   endclocking

   clocking op_psv_cb @(posedge clk);
      default input #1step output #1step;
      input clk;
      input rst;
      input op_id;
      input ps;
      input py;
      input mx;
      input my;  
      input addr;
      input sema;
      input age;
   endclocking

   modport op_mst (clocking op_mst_cb);
   modport op_psv (clocking op_psv_cb);

endinterface
`endif