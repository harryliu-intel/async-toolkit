
`ifndef __MBY_GMM_MIG_DATA_IF__
`define __MBY_GMM_MIG_DATA_IF__

import mby_mgp_bfm_pkg::*;


   interface mby_gmm_mig_data_if (
      inout msh_data_mig
   );

   clocking data_mst_cb @(posedge cclk);
      default input #1step output #1step;
      output msh_data_mig;
   endclocking 

   clocking data_slv_cb @(posedge cclk);
      default input #1step output #1step;
      input msh_data_mig;
   endclocking

   modport data_master  (clocking data_mst_cb);
   modport data_passive (clocking data_slv_cb);


endinterface
`endif
