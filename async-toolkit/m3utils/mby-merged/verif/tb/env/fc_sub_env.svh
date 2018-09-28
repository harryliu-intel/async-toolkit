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


`ifdef AXI_ENV_ENABLE
  axi_integ_env axi_subenv;
`endif

`ifdef APB_ENV_ENABLE
  apb_integ_env apb_subenv;
`endif

`ifdef CHI_ENV_ENABLE
  chi_integ_env chi_subenv;
`endif

`ifdef CDN_AXI_ENV_ENABLE
  cdn_axi_integ_env cdn_axi_subenv;
`endif

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

  //-- AXI
  `ifdef AXI_ENV_ENABLE
      axi_subenv = axi_integ_env::type_id::create("axi_subenv", this);
  `endif
    
  //-- APB
  `ifdef APB_ENV_ENABLE
      apb_subenv = apb_integ_env::type_id::create("apb_subenv", this);
  `endif
  //-- CHI
  `ifdef CHI_ENV_ENABLE
      chi_subenv = chi_integ_env::type_id::create("chi_subenv", this);
  `endif

   //-- CDN AXI
  `ifdef CDN_AXI_ENV_ENABLE
      cdn_axi_subenv = cdn_axi_integ_env::type_id::create("cdn_axi_subenv", this);
  `endif

  `ifdef VTE_IP3_UVM_ENV_ENABLE
      vte_ip3_subenv = vte_ip3Uvm_integ_env::type_id::create("vte_ip3_subenv", this);
  `endif

endfunction: build_sub_system_envs

`endif
