// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  Cadence Axi environment - configuring Active slave,
//                 PassiveSlave (to shadow DUT) and a active_master (to drive DUT), 
// -----------------------------------------------------------------------------

class cdn_axi_env extends uvm_env;

  // ***************************************************************
  // The environment instantiates Master and Slave components
  // ***************************************************************
  cdn_axi_active_master_agent active_master;
  cdn_axi_active_slave_agent active_slave;
  cdn_axi_passive_agent passiveSlave;
  cdn_axi_passive_agent passiveMaster;

  `uvm_component_utils_begin(cdn_axi_env)          
  `uvm_component_utils_end

  // ***************************************************************
  // Method : new
  // Desc.  : Call the constructor of the parent class.
  // ***************************************************************
  function new(string name = "cdn_axi_env", uvm_component parent = null);
    super.new(name, parent);
  endfunction : new 

  // ***************************************************************
  // Desc.  : Build all of the components of the environment.  The
  //          environment consists one active slave (mimicking DUT)
  //          one PassiveSlave (to shadow DUT) and one active_master 
  //          (to drive DUT),
  // ***************************************************************
  virtual function void build_phase(uvm_phase phase);    
    super.build_phase(phase);

    // Active Master
    active_master = cdn_axi_active_master_agent::type_id::create("active_master", this);
    begin
      cdn_axi_system_cfg active_masterCfg = cdn_axi_system_cfg::type_id::create("active_masterCfg",this);
      active_masterCfg.is_active = UVM_ACTIVE;
      active_masterCfg.PortType = CDN_AXI_CFG_MASTER;
      active_masterCfg.reset_signals_sim_start = 1;
      active_masterCfg.verbosity = CDN_AXI_CFG_MESSAGEVERBOSITY_LOW;
//      active_masterCfg.addToMemorySegments(32'h0,32'h3000,CDN_AXI_CFG_DOMAIN_NON_SHAREABLE);
            
      active_masterCfg.addToMemorySegments(64'h0,64'hFFFF,CDN_AXI_CFG_DOMAIN_NON_SHAREABLE);
      active_masterCfg.addToMemorySegments(64'h10000,64'h5FFFF,CDN_AXI_CFG_DOMAIN_NON_SHAREABLE);
      active_masterCfg.no_changes_in_address_channels_limit = 100;
      active_masterCfg.max_write_bursts_behavior = CDN_AXI_CFG_MAX_WRITE_BURSTS_BEHAVIOR_CONTINUE_TO_SEND;
      active_masterCfg.write_issuing_capability = 7;
      //active_masterCfg.       
      uvm_config_object::set(this,"active_master","cfg",active_masterCfg); 
       
    end

    // Active Slave
    active_slave = cdn_axi_active_slave_agent::type_id::create("active_slave", this);
    begin
      cdn_axi_system_cfg active_slaveCfg = cdn_axi_system_cfg::type_id::create("active_slaveCfg",this);
      active_slaveCfg.is_active = UVM_ACTIVE;
      active_slaveCfg.PortType = CDN_AXI_CFG_SLAVE;
      active_slaveCfg.reset_signals_sim_start = 1;
     // active_slaveCfg.addToMemorySegments(32'h0,32'h1000,CDN_AXI_CFG_DOMAIN_NON_SHAREABLE);
      //active_slaveCfg.addToMemorySegments(32'h2000,32'h3000,CDN_AXI_CFG_DOMAIN_NON_SHAREABLE);
      
      active_slaveCfg.addToMemorySegments(64'h0,64'hFFFF,CDN_AXI_CFG_DOMAIN_NON_SHAREABLE);
      active_slaveCfg.addToMemorySegments(64'h10000,64'h5FFFF,CDN_AXI_CFG_DOMAIN_NON_SHAREABLE);
        
          
      active_slaveCfg.do_signal_check_only_when_valid = 1;
      active_slaveCfg.no_changes_in_address_channels_limit = 100;     
      active_slaveCfg.write_acceptance_capability = 6;
      active_slaveCfg.disable_memory_update_on_write_burst = 0;
//      active_slaveCfg.
//      active_slaveCfg.
//      active_slaveCfg.
      uvm_config_object::set(this,"active_slave","cfg",active_slaveCfg); 
    end

    // Passive Slave
    passiveSlave = cdn_axi_passive_agent::type_id::create("passiveSlave", this);    
    begin
      cdn_axi_system_cfg passiveSlaveCfg = cdn_axi_system_cfg::type_id::create("passiveSlaveCfg");
      passiveSlaveCfg.is_active = UVM_PASSIVE;
      passiveSlaveCfg.PortType = CDN_AXI_CFG_SLAVE;
      passiveSlaveCfg.verbosity = CDN_AXI_CFG_MESSAGEVERBOSITY_LOW;
      //passiveSlaveCfg.addToMemorySegments(32'h0,32'h1000,CDN_AXI_CFG_DOMAIN_NON_SHAREABLE);
      //passiveSlaveCfg.addToMemorySegments(32'h2000,32'h3000,CDN_AXI_CFG_DOMAIN_NON_SHAREABLE);
      
      passiveSlaveCfg.addToMemorySegments(64'h0,64'hFFFF,CDN_AXI_CFG_DOMAIN_NON_SHAREABLE);
      passiveSlaveCfg.addToMemorySegments(64'h10000,64'h5FFFF,CDN_AXI_CFG_DOMAIN_NON_SHAREABLE);
      
      passiveSlaveCfg.do_signal_check_only_when_valid = 1;
      passiveSlaveCfg.no_changes_in_address_channels_limit = 100;     
      passiveSlaveCfg.max_write_bursts_behavior = CDN_AXI_CFG_MAX_WRITE_BURSTS_BEHAVIOR_CONTINUE_TO_SEND;
      passiveSlaveCfg.write_acceptance_capability = 6;
      passiveSlaveCfg.disable_memory_update_on_write_burst = 0;
//      passiveSlaveCfg.
//      passiveSlaveCfg.
//      passiveSlaveCfg.
      uvm_config_object::set(this,"passiveSlave","cfg",passiveSlaveCfg); 
    end
    
    // Passive Master
    passiveMaster = cdn_axi_passive_agent::type_id::create("passiveMaster", this);    
    begin
      cdn_axi_system_cfg passiveMasterCfg = cdn_axi_system_cfg::type_id::create("passiveMasterCfg");
      passiveMasterCfg.is_active = UVM_PASSIVE;
      passiveMasterCfg.PortType = CDN_AXI_CFG_MASTER;
      passiveMasterCfg.verbosity = CDN_AXI_CFG_MESSAGEVERBOSITY_LOW;
      
      //passiveMasterCfg.addToMemorySegments(32'h0,32'h3000,CDN_AXI_CFG_DOMAIN_NON_SHAREABLE);
      
      passiveMasterCfg.addToMemorySegments(64'h0,64'hFFFF,CDN_AXI_CFG_DOMAIN_NON_SHAREABLE);
      passiveMasterCfg.addToMemorySegments(64'h10000,64'h5FFFF,CDN_AXI_CFG_DOMAIN_NON_SHAREABLE);
      passiveMasterCfg.no_changes_in_address_channels_limit = 100;
      passiveMasterCfg.max_write_bursts_behavior = CDN_AXI_CFG_MAX_WRITE_BURSTS_BEHAVIOR_CONTINUE_TO_SEND;
      passiveMasterCfg.write_issuing_capability = 7;
      //passiveMasterCfg.
      
      uvm_config_object::set(this,"passiveMaster","cfg",passiveMasterCfg);       
    end

  endfunction : build_phase
  
  virtual task run_phase(uvm_phase phase);
      super.run_phase(phase);
      active_master.cfg.write_issuing_capability = 5;
      active_master.reconfigure(active_master.cfg); 
  endtask
  
  
  function void end_of_elaboration_phase(uvm_phase phase);

    super.end_of_elaboration_phase(phase);
	
    `uvm_info(get_type_name(), "Setting callbacks", UVM_LOW);
  
      // Enable PureSpec callbacks. Uncomment as necessary
    // Refer to the User Guide for callbacks description

    // Active Master
    void'(active_master.inst.setCallback( DENALI_CDN_AXI_CB_Error));
    //void'(active_master.inst.setCallback( DENALI_CDN_AXI_CB_ResetStarted));
    //void'(active_master.inst.setCallback( DENALI_CDN_AXI_CB_ResetEnded));
    void'(active_master.inst.setCallback( DENALI_CDN_AXI_CB_BeforeSend));
    //void'(active_master.inst.setCallback( DENALI_CDN_AXI_CB_BeforeSendAddress));
    void'(active_master.inst.setCallback( DENALI_CDN_AXI_CB_BeforeSendResponse));
    //void'(active_master.inst.setCallback( DENALI_CDN_AXI_CB_BeforeSendTransfer));
    //void'(active_master.inst.setCallback( DENALI_CDN_AXI_CB_Started));
    //void'(active_master.inst.setCallback( DENALI_CDN_AXI_CB_StartedAddress));
    //void'(active_master.inst.setCallback( DENALI_CDN_AXI_CB_StartedResponse));
    //void'(active_master.inst.setCallback( DENALI_CDN_AXI_CB_StartedTransfer));
    void'(active_master.inst.setCallback( DENALI_CDN_AXI_CB_Ended));
    //void'(active_master.inst.setCallback( DENALI_CDN_AXI_CB_EndedAddress));
    //void'(active_master.inst.setCallback( DENALI_CDN_AXI_CB_EndedResponse));
    //void'(active_master.inst.setCallback( DENALI_CDN_AXI_CB_EndedTransfer));
    
    // Passive Master
    void'(passiveMaster.inst.setCallback( DENALI_CDN_AXI_CB_Error));
    //void'(passiveMaster.inst.setCallback( DENALI_CDN_AXI_CB_ResetStarted));
    void'(passiveMaster.inst.setCallback( DENALI_CDN_AXI_CB_ResetEnded));
    void'(passiveMaster.inst.setCallback( DENALI_CDN_AXI_CB_Started));
    //void'(passiveMaster.inst.setCallback( DENALI_CDN_AXI_CB_StartedAddress));
    //void'(passiveMaster.inst.setCallback( DENALI_CDN_AXI_CB_StartedResponse));
    //void'(passiveMaster.inst.setCallback( DENALI_CDN_AXI_CB_StartedTransfer));
    void'(passiveMaster.inst.setCallback( DENALI_CDN_AXI_CB_Ended));
    //void'(passiveMaster.inst.setCallback( DENALI_CDN_AXI_CB_EndedAddress));
    //void'(passiveMaster.inst.setCallback( DENALI_CDN_AXI_CB_EndedResponse));
    //void'(passiveMaster.inst.setCallback( DENALI_CDN_AXI_CB_EndedTransfer));
    
    // Active Slave
    void'(active_slave.inst.setCallback( DENALI_CDN_AXI_CB_Error));
    //void'(active_slave.inst.setCallback( DENALI_CDN_AXI_CB_ResetStarted));
    //void'(active_slave.inst.setCallback( DENALI_CDN_AXI_CB_ResetEnded));
    void'(active_slave.inst.setCallback( DENALI_CDN_AXI_CB_BeforeSend));
    //void'(active_slave.inst.setCallback( DENALI_CDN_AXI_CB_BeforeSendAddress));
    void'(active_slave.inst.setCallback( DENALI_CDN_AXI_CB_BeforeSendResponse));
    //void'(active_slave.inst.setCallback( DENALI_CDN_AXI_CB_BeforeSendTransfer));
    //void'(active_slave.inst.setCallback( DENALI_CDN_AXI_CB_Started));
    //void'(active_slave.inst.setCallback( DENALI_CDN_AXI_CB_StartedAddress));
    //void'(active_slave.inst.setCallback( DENALI_CDN_AXI_CB_StartedResponse));
    //void'(active_slave.inst.setCallback( DENALI_CDN_AXI_CB_StartedTransfer));
    void'(active_slave.inst.setCallback( DENALI_CDN_AXI_CB_Ended));
    //void'(active_slave.inst.setCallback( DENALI_CDN_AXI_CB_EndedAddress));
    //void'(active_slave.inst.setCallback( DENALI_CDN_AXI_CB_EndedResponse));
    //void'(active_slave.inst.setCallback( DENALI_CDN_AXI_CB_EndedTransfer));

    `uvm_info(get_type_name(), "Setting callbacks ... DONE", UVM_LOW);
    
  endfunction : end_of_elaboration_phase
  
endclass : cdn_axi_env
