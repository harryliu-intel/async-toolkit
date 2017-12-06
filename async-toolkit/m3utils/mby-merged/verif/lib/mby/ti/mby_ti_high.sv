/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    mby_ti_high.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
 
 IP Test island
 
 This moudle will hold all the "shared" TB content between the IP and the integartion level.

 
 MBY_TOP_RTL define should be use to monitor internal signals. This define will be override in integration level.


*/

`ifndef MBY_TOP_RTL
 `define MBY_TOP_RTL mby_tb.mby_top
`endif

module mby_ti_high #(parameter string IP_ENV = "*.env*"
// START CHASSIS_NOT_PRESENT
//                ,parameter bit MBY_HAS_RESET_PKG = 0,
// END CHASSIS_NOT_PRESENT
// START IOSF_NOT_PRESENT
//                parameter bit MBY_IOSF_IS_PASSIVE = 1, 
// END IOSF_NOT_PRESENT
// START CHASSIS_NOT_PRESENT
//	        parameter string RESET_PKG_IS_ACTIVE = 0
// END CHASSIS_NOT_PRESENT
            )
  (
// START IOSF_NOT_PRESENT
//   // IOSF Primary interafce
//   iosf_primary_intf mby_iosf_pri_if,
//   // IOSF SB ineterface
//   iosf_sbc_intf mby_iosf_sb_if,
//   // MBY IP env interface
// END IOSF_NOT_PRESENT
   mby_env_if mby_if
// START CHASSIS_NOT_PRESENT
//   // Power Gating VC interface
//   ,PowerGatingIF 	pg_if,
//   // CCU_VC interface
//   ccu_intf		ccu_if
// END CHASSIS_NOT_PRESENT
   );

  import uvm_pkg::*;
// START IOSF_NOT_PRESENT
//  import IosfPkg::*;
// END IOSF_NOT_PRESENT
  
`ifdef XVM
   import ovm_pkg::*;
   import xvm_pkg::*;
   `include "ovm_macros.svh"
   `include "sla_macros.svh"
`endif

import sla_pkg::*;
  
  // IP parameter file for IOSF primary and sideband signal widths, etc.
`include "mby_params.sv"
`include "mby_defines.sv"
  


    // ===============================================
    // Test Island LOW instance
    // ===============================================
    mby_ti_low #(
// START CHASSIS_NOT_PRESENT
//             .MBY_HAS_RESET_PKG(1),
// END CHASSIS_NOT_PRESENT
// START IOSF_NOT_PRESENT
//             .MBY_IOSF_IS_PASSIVE(0),
// END IOSF_NOT_PRESENT
// START CHASSIS_NOT_PRESENT
//             .RESET_PKG_IS_ACTIVE(1)
// END CHASSIS_NOT_PRESENT
            )
            u_mby_ti_low (
// START IOSF_NOT_PRESENT
//                     .mby_iosf_pri_if     (mby_iosf_pri_if),
//                     .mby_iosf_sb_if      (mby_iosf_sb_if),
// END IOSF_NOT_PRESENT
                     .mby_if          (mby_if)
// START CHASSIS_NOT_PRESENT
//                     ,.pg_if (pg_if),
//                     .ccu_if(ccu_if)
// END CHASSIS_NOT_PRESENT
                 );


  
endmodule
