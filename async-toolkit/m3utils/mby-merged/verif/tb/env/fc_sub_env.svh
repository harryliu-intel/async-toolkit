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
  //vte_ip3Uvm_integ_env vte_ip3_subenv;
`endif

`ifdef IGR_ENV_ENABLE
  igr_integ_env igr_subenv;
`endif

`ifdef CDN_PCIE_ENV_ENABLE
    pep_integ_env pep_subenv;
`endif

`ifdef EPC_ENV_ENABLE
  epc_integ_env epc_subenv;
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
      //vte_ip3_subenv = vte_ip3Uvm_integ_env::type_id::create("vte_ip3_subenv", this);
  `endif

  `ifdef IGR_ENV_ENABLE
      igr_subenv = igr_integ_env::type_id::create("igr_subenv", this);
  `endif

  `ifdef CDN_PCIE_ENV_ENABLE
      pep_subenv = pep_integ_env::type_id::create("pep_subenv", this);
  `endif

  `ifdef EPC_ENV_ENABLE
      epc_subenv = epc_integ_env::type_id::create("epc_subenv", this);
  `endif

endfunction: build_sub_system_envs

`endif
