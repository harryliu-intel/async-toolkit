// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  APB System Configuration Compoenent
// -----------------------------------------------------------------------------

class apb_shared_cfg extends uvm_object ;

  /** Configuration object used by the master VIP */
  svt_apb_system_configuration master_cfg;

  /** Configuration object used by the slave VIP */
  svt_apb_system_configuration slave_cfg;

  bit force_x_on_pclk = 0;
  bit force_x_on_presetn = 0;

  `uvm_object_utils(apb_shared_cfg)

  /** Constructor */
  function new(string name = "apb_shared_cfg_inst");
    master_cfg = new("master_cfg");
    slave_cfg = new("slave_cfg");

    randcase
      10 : this.force_x_on_pclk   = 1;
      10 : this.force_x_on_presetn = 1;
      20 : begin 
             this.force_x_on_pclk   = 1;
             this.force_x_on_presetn = 1;
           end
    endcase

    master_cfg.paddr_width = svt_apb_system_configuration::PADDR_WIDTH_16;
    master_cfg.pdata_width = svt_apb_system_configuration::PDATA_WIDTH_32;
    master_cfg.apb4_enable = 0;
    
    /** Master setup */
    master_cfg.create_sub_cfgs(1);
    
    /** Enable protocol file generation for Protocol Analyzer */
    master_cfg.enable_xml_gen = 1;
    master_cfg.trace_enable = 1;
    master_cfg.is_active = 1;
    master_cfg.slave_cfg[0].enable_xml_gen = 1;
    master_cfg.slave_cfg[0].trace_enable = 1;
    master_cfg.slave_cfg[0].is_active = 0;

    master_cfg.transaction_coverage_enable = 1;
    master_cfg.slave_cfg[0].transaction_coverage_enable = 1;
    master_cfg.protocol_checks_coverage_enable = 1;
    master_cfg.slave_cfg[0].protocol_checks_coverage_enable = 1;
    master_cfg.disable_x_check_of_pclk    = 1;
    master_cfg.disable_x_check_of_presetn = 1;

    slave_cfg.paddr_width = svt_apb_system_configuration::PADDR_WIDTH_16;
    slave_cfg.pdata_width = svt_apb_system_configuration::PDATA_WIDTH_32;
    slave_cfg.apb4_enable = 0;
    slave_cfg.disable_x_check_of_pclk    = 1;
    slave_cfg.disable_x_check_of_presetn = 1;
    
    /** Slave setup */
    slave_cfg.create_sub_cfgs(1);
    
    /** Enable protocol file generation for Protocol Analyzer */
    slave_cfg.is_active = 0;
    slave_cfg.enable_xml_gen = 1;
    slave_cfg.trace_enable = 1;
    slave_cfg.slave_cfg[0].enable_xml_gen = 1;
    slave_cfg.slave_cfg[0].trace_enable = 1;
    slave_cfg.slave_cfg[0].is_active = 1;

    slave_cfg.transaction_coverage_enable = 1;
    slave_cfg.slave_cfg[0].transaction_coverage_enable = 1;
    slave_cfg.protocol_checks_coverage_enable = 1;
    slave_cfg.slave_cfg[0].protocol_checks_coverage_enable = 1;

  endfunction

  /**
   * Checks to see that the data field values are valid, mainly making sure the
   * master and slave interface specifications are consistent. 
   */
  virtual function bit is_valid(bit silent = 1, int kind = -1);
    is_valid = 1;

    if (!master_cfg.is_valid()) begin
      if (!silent) begin
        `uvm_info("is_valid", $sformatf("Invalid Master configuration. Contents:\n%s", master_cfg.sprint()), UVM_HIGH)
      end
      is_valid = 0;   
    end else if (!slave_cfg.is_valid()) begin
      if (!silent) begin
        `uvm_info("is_valid", $sformatf("Invalid Slave configuration. Contents:\n%s", slave_cfg.sprint()), UVM_HIGH)
      end
      is_valid = 0;   
    end
  endfunction

endclass
