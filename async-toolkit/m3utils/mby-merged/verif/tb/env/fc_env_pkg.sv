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

    // VTE UVM ip1 pkg (native UVM based)
    import ip1_uvm_pkg::*;

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

    // VTE-UVM-TR
    `ifdef VTE_IP3_UVM_ENV_ENABLE
        `include "vte_ip3_uvm_integ_env.svh"
    `endif // VTE_IP3_UVM_ENV_ENABLE

    // IGR 
    //`ifdef IGR_ENV_ENABLE
        import ingress_env_pkg::*;
        `include "igr_integ_env.svh"
    //`endif // IGR_ENV_ENABLE

    // PEP
    `ifdef CDN_PCIE_ENV_ENABLE
        import DenaliSvPcie::*;
        import DenaliSvMem::*;
        import cdnPcieUvm::*;
        import cdn_pcie_pkg::*;
        `include "pep_integ_env.svh"
    `endif

    // AM, AG, SM classes
    //`include "fc_sm_am_env.svh"
    //`include "fc_sm_ag_env.svh"
    `include "fc_sm_env.svh"

    // TB environment
    `include "fc_tb_env.svh"

endpackage
