interface mby_mesh_data_if (
   input clk,
   input rst,
   inout [511:0] data
);

   clocking data_mst_cb @(posedge clk);
      default input #1step output #1step;
      output data;
   endclocking 

   clocking data_psv_cb @(posedge clk);
      default input #1step output #1step;
      input  data;
   endclocking 

   modport data_master  (clocking data_mst_cb);
   modport data_passive (clocking data_psv_cb);
   
 
endinterface