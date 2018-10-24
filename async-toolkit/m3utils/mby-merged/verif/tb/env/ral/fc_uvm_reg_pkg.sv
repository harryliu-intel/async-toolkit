// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  FC UVM reg package
// -----------------------------------------------------------------------------

`ifndef _Fc_uvm_reg_pkg__sv_
`define _Fc_uvm_reg_pkg__sv_

package fc_uvm_reg_pkg;

  import uvm_pkg::*;
  `include "uvm_macros.svh"

  //import ip3_uvm_reg_pkg::*;

 // FC reg map
 `include "fc_uvm_reg_map.sv"

endpackage : fc_uvm_reg_pkg

`endif // _Fc_uvm_reg_pkg__sv_
