// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  FC Env package
// -----------------------------------------------------------------------------


package fc_env_pkg;
    import uvm_pkg::*;
    `include "uvm_macros.svh"

   `ifdef XVM
   import ovm_pkg::*;
   import xvm_pkg::*;
   `include "ovm_macros.svh"
   `include "sla_macros.svh"
   `endif

    import sla_pkg::*;
    `include "sla_defines.svh"
    `include "slu_macros.svh"

    import svt_uvm_pkg::*;
    import svt_bfm_shell_uvm_pkg::*;
    import svt_amba_uvm_pkg::*;
    //import svt_axi_uvm_pkg::*;
    //import svt_apb_uvm_pkg::*;

    // VTE UVM ip1 pkg (native UVM based)
    import ip1_uvm_pkg::*;

    //// VTE UVM ip2 pkg (SLU based)
    //import ip2_sla_pkg::*;

    // VTE UVM ip3 pkg (native UVM based)
    import ip3_uvm_env_pkg::*;

    // FC UVM reg
    import fc_uvm_reg_pkg::*;

    `include "FC.svh"
    `include "fc_im_env.svh"
    `include "fc_report_server.svh"
    `include "fc_mem_access.svh"
   
    // config object
    `include "fc_chip_cfg_obj.svh"
    `include "fc_cfg_obj.svh"

    // fc file name class for managing all the files
    `include "fc_file_names.svh"

    // fc config classes
    //`include "fc_bfm_cfg_obj.svh"

    // fuse classes
//    `include "fc_fuse_env.svh"

    // monitors and scoreboards

    // clock checker
    //`include "fc_clk_sig_chk.svh"

    // ral env
   `include "pmu_mmr_regs.svh"
    `include "fc_ral_env.svh"

    `include "subsystem_base_env.svh"

    `ifdef AXI_ENV_ENABLE
    // AXI BFM
//    `include "axi_bfm_defines.svh"
//    `include "axi_master_txn.svh"
//    `include "axi_system_cfg.svh"
//    `include "axi_scoreboard.svh"
//    `include "svt_axi_virtual_sequencer.svh"
//    `include "axi_bfm_env.svh"
//    `include "axi_integ_env.svh"
    `endif

    // APB BFM
    `ifdef APB_ENV_ENABLE
//    `include "apb_bfm_defines.svh"
//    `include "apb_master_txn.svh"
//    `include "apb_shared_cfg.svh"
//    `include "apb_bfm_env.svh"
//    `include "apb_integ_env.svh"
    `endif

    // CHI BFM
    `ifdef CHI_ENV_ENABLE
    //import svt_chi_uvm_pkg::*;
    `include "chi_system_cfg.svh"
    `include "chi_bfm_env.svh"
    `include "chi_integ_env.svh"
    `endif

    // VTE-UVM-TR
    `ifdef VTE_IP3_UVM_ENV_ENABLE
    `include "vte_ip3_uvm_integ_env.svh"
    `endif // VTE_IP3_UVM_ENV_ENABLE

    // AM, AG, SM classes
    //`include "fc_sm_am_env.svh"
    //`include "fc_sm_ag_env.svh"
    `include "fc_sm_env.svh"

    // TB environment
    `include "fc_tb_env.svh"

endpackage
