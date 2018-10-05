// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  Include file to create Sub-systems
// -----------------------------------------------------------------------------


`ifndef _FC_SUB_ENV__SVH_
`define _FC_SUB_ENV__SVH_

//-- reset checker
//fc_rst_sig_chk rst_sig_chk;

`ifdef VTE_IP3_UVM_ENV_ENABLE
  vte_ip3Uvm_integ_env vte_ip3_subenv;
`endif

//-----------------------------------------------------------------------

//-- build reset checker
//-----------------------------------------------------------------------
//function void build_reset_checker(); 
//    uvm_config_string::set(this, "rst_sig_chk", "debug_file_name", "reset_sig_checker.out");
//    rst_sig_chk = fc_rst_sig_chk::type_id::create("rst_sig_chk", this);
//endfunction

//-----------------------------------------------------------------------
//-- build sub-system envs
// ------------------------------------------------------------------------
function void build_sub_system_envs();
  //-- build sub-system envs

  `ifdef VTE_IP3_UVM_ENV_ENABLE
      vte_ip3_subenv = vte_ip3Uvm_integ_env::type_id::create("vte_ip3_subenv", this);
  `endif

endfunction: build_sub_system_envs

`endif
