// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  CHI System configuration 
// ------------------------------------------------------------------------------------------------------------------------

class chi_system_cfg extends svt_amba_system_configuration;

  //Utility macro
  //`svt_xvm_object_utils (chi_system_cfg)
  `uvm_object_utils (chi_system_cfg)

  function new (string name="chi_system_cfg");
    super.new(name);
        
    $display("Inside CHI System Env Cfg");
  endfunction
 
  /*
   * Utility method in the testbench to initialize the configuration of AMBA
   * System ENV, and underlying CHI System ENV.
   */
  extern function void set_amba_sys_config();

  /** 
   * Utility method in the testbench to set the CHI System configuration parameters 
   */
  extern function void set_chi_system_configuration(svt_chi_system_configuration chi_sys_cfg);

  /** 
   * Utility method in the testbench to configure the CHI System Address Map
   */
  extern function void configure_chi_system_address_map(svt_chi_system_configuration chi_sys_cfg);
endclass
// =============================================================================

//------------------------------------------------------------------------------
function void chi_system_cfg::set_amba_sys_config();

  /**
   * svt_amba_system_configuration::create_sub_cfgs allows user to allocate
   * system configurations for AXI, AHB, APB and CHI System Envs.
   * Prototype of the method is:
   * void create_sub_cfgs (int num_axi_systems, int num_ahb_systems, int num_apb_systems,int num_chi_systems ) 
   * Here, we are allocating one CHI System configuration within the AMBA System
   * configuration, as we are creating one CHI System ENV within AMBA System Env.
   */
  create_sub_cfgs(0,0,0,1);

`ifndef SVT_AMBA_EXCLUDE_AXI_IN_CHI_SYS_ENV
  /**
   * Allocates the RN and SN node configurations before a user sets the parameters.
   * This function is to be called if (and before) the user sets the configuration
   * parameters by setting each parameter individually and not by randomizing the
   * system configuration. 
   * Prototype of the method is:
   * void create_sub_cfgs(int num_chi_rn = 1, int num_chi_sn = 1, int num_chi_ic_rn = 0, int num_chi_ic_sn = 0, int num_axi_masters = 1, int num_axi_slaves = 1, int num_axi_ic_master_ports = 0, int num_axi_ic_slave_ports = 0);
   */
  this.chi_sys_cfg[0].create_sub_cfgs(1,1,0,0,0,0,0,0);
`else
  /**
   * Allocates the RN and SN node configurations before a user sets the parameters.
   * This function is to be called if (and before) the user sets the configuration
   * parameters by setting each parameter individually and not by randomizing the
   * system configuration. 
   * Prototype of the method is:
   * void create_sub_cfgs(int num_chi_rn = 1, int num_chi_sn = 1, int num_chi_ic_rn = 0, int num_chi_ic_sn = 0);
   */
  this.chi_sys_cfg[0].create_sub_cfgs(1,1,0,0);
`endif
  

  /* Configure the number of CHI Home Nodes in the CHI System ENV. 
   * The number of Home Nodes in a CCN-504 Cache Coherant Network
   * is 8. So, this is configured to 8. 
   * */
  this.chi_sys_cfg[0].num_hn = 0;
  /* Configure the number of CHI Request Nodes in the CHI System ENV. */
  this.chi_sys_cfg[0].num_rn = 1;
  /* Configure the number of CHI Slave Nodes in the CHI System ENV. */
  this.chi_sys_cfg[0].num_sn = 1;
  
  /* Set the CHI System configuration parameters. */
  set_chi_system_configuration(this.chi_sys_cfg[0]);

endfunction
// =============================================================================

//------------------------------------------------------------------------------
function void chi_system_cfg::set_chi_system_configuration(svt_chi_system_configuration chi_sys_cfg);
  chi_sys_cfg.system_monitor_enable = 0;
  //`ifdef SVT_CHI_PA_FSDB_ENABLE
    chi_sys_cfg.enable_xml_gen = 1;
    chi_sys_cfg.enable_summary_tracing = 1;
    chi_sys_cfg.pa_format_type = svt_xml_writer::FSDB;
  //`endif

  /** Set the interface type. */
  chi_sys_cfg.rn_cfg[0].chi_interface_type = svt_chi_node_configuration::RN_I;
  chi_sys_cfg.sn_cfg[0].chi_interface_type = svt_chi_node_configuration::SN_F;

  /** Set unique node id for each node */
  chi_sys_cfg.rn_cfg[0].node_id = 0; 
  chi_sys_cfg.sn_cfg[0].node_id = 6; 

  /** Set the width of Data field within Data VC Flit. */
  chi_sys_cfg.rn_cfg[0].flit_data_width = 128;
  chi_sys_cfg.sn_cfg[0].flit_data_width = 128;
  
  /** Enable transaction level coverage */
  chi_sys_cfg.rn_cfg[0].transaction_coverage_enable = 1;
  chi_sys_cfg.sn_cfg[0].transaction_coverage_enable = 1;
  
  /** Enable XML generation for Protocol Analyzer. */
//  `ifdef PA_ENABLE
    chi_sys_cfg.rn_cfg[0].enable_xact_xml_gen = 1;
    chi_sys_cfg.rn_cfg[0].enable_fsm_xml_gen  = 1;
    chi_sys_cfg.sn_cfg[0].enable_xact_xml_gen = 1;
    chi_sys_cfg.sn_cfg[0].enable_fsm_xml_gen  = 1;
    chi_sys_cfg.pa_format_type = svt_xml_writer::FSDB;
//  `endif
  
  chi_sys_cfg.rn_cfg[0].enable_pl_tracing = 1;
  chi_sys_cfg.rn_cfg[0].enable_ll_tracing = 1;
  chi_sys_cfg.sn_cfg[0].enable_ll_tracing = 1;

  /** Set mode */
  chi_sys_cfg.rn_cfg[0].is_active = 1;
  chi_sys_cfg.sn_cfg[0].is_active = 1;

endfunction
// =============================================================================

//------------------------------------------------------------------------------
function void chi_system_cfg::configure_chi_system_address_map(svt_chi_system_configuration chi_sys_cfg);
endfunction
// =============================================================================

