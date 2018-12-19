
`ifndef __MBY_MGP_MIM_DATA_IF__
`define __MBY_MGP_MIM_DATA_IF__

import mby_mgp_bfm_pkg::*;

   interface mby_mgp_mim_data_if (
      inout msh_data
   );

   clocking data_mst_cb @(posedge cclk);
      default input #1step output #1step;
      output msh_data;
   endclocking 

   clocking data_slv_cb @(posedge cclk);
      default input #1step output #1step;
      input msh_data;
   endclocking

   modport data_master  (clocking data_mst_cb);
   modport data_passive (clocking data_slv_cb);


endinterface
`endif
