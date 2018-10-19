//-----------------------------------------------------------------------------
// Title         : Ingress main environment class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : ingress_env.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// This class builds and connects the different agents/BFMs/VCs needed by this
// cluster test environment
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------

class ingress_env extends ingress_base_env;

  ingress_ti_config ti_config;
  //protected string ingress_ti_low_path   = "XYZ_tb.u_ingress_ti";

  // Variable: _ingress_env
  // static pointer to the env
  static ingress_env _ingress_env;

  // Variable: ingress_if
  // Ingress env interface
  igr_env_if_t ingress_if;

  // Variable: ingress_epool
  // Ingress event pool
  uvm_event_pool ingress_epool;

  // ---------------------------------------------------------------
  // IP Agents and VC's declaration
  // ---------------------------------------------------------------

  // Variable:  vp_bfms
  // MAC Client BFM agent
  igr_vp_bfm_t vp_bfms[`NUM_VPS_PER_IGR];

  // Variable:  eth_bfm_tx_io
  // MAC Client BFM io policy
  igr_eth_bfm_tx_io_t vp_bfm_tx_io[`NUM_VPS_PER_IGR];

  // Variable:  eth_bfm_rx_io
  // MAC Client BFM io policy
  igr_eth_bfm_rx_io_t vp_bfm_rx_io[`NUM_VPS_PER_IGR];

  // Variable:  eth_bfm_tx_vintf
  // MAC Client BFM virtual interface
  igr_eth_bfm_tx_intf_t vp_bfm_tx_vintf[`NUM_VPS_PER_IGR];

  // Variable:  eth_bfm_rx_vintf
  // MAC Client BFM virtual interface
  igr_eth_bfm_rx_intf_t vp_bfm_rx_vintf[`NUM_VPS_PER_IGR];

  // Variable:  eth_bfms
  // MAC Client BFM agent
  igr_eth_bfm_t eth_bfms[`NUM_EPLS_PER_IGR];

  // Variable:  eth_bfm_tx_io
  // MAC Client BFM io policy
  igr_eth_bfm_tx_io_t eth_bfm_tx_io[`NUM_EPLS_PER_IGR];

  // Variable:  eth_bfm_rx_io
  // MAC Client BFM io policy
  igr_eth_bfm_rx_io_t eth_bfm_rx_io[`NUM_EPLS_PER_IGR];

  // Variable:  eth_bfm_tx_vintf
  // MAC Client BFM virtual interface
  igr_eth_bfm_tx_intf_t eth_bfm_tx_vintf[`NUM_EPLS_PER_IGR];

  // Variable:  eth_bfm_rx_vintf
  // MAC Client BFM virtual interface
  igr_eth_bfm_rx_intf_t eth_bfm_rx_vintf[`NUM_EPLS_PER_IGR];

  // Variable: env_monitor
  // ingress env event monitor
  ingress_env_monitor env_monitor;

  `uvm_component_utils_begin(ingress_env)
  //`uvm_field_string(ingress_ti_low_path, UVM_ALL_ON)
  `uvm_component_utils_end

  function new (string name="ingress_env", uvm_component parent = null);
    super.new(name, parent);
    // Setting the env static pointer
    _ingress_env = this;
  endfunction: new

  //////////////////////////////////////////////////////////////////////////////
  // Function: ingress_env build
  // build phase of ingress_env
  // All VC's and Agent should be build in this phase.
  // For each new VC's/Agent it is recommended to add it an a specific function
  virtual function void build_phase(uvm_phase phase);
    uvm_object tmp_ti_cfg_obj;

    super.build_phase(phase);

    if(uvm_config_object::get(this, "",
        "ingress_ti_config",tmp_ti_cfg_obj)) begin
      assert($cast(ti_config,tmp_ti_cfg_obj));
    end

    build_vpt_bfms();
    build_eth_bfms();

    // Env monitor
    assert($cast(env_monitor, create_component("ingress_env_monitor","env_monitor")));
    env_monitor.set_monitor_enable(cfg.get_monitors_enabled());

    // get global event pool
    ingress_epool = ingress_epool.get_global_pool();

  endfunction // void


  //////////////////////////////////////////////////////////////////////////////
  // Function: ingress_env connect
  // connect phase of ingress_env
  function void connect_phase(uvm_phase phase);
    uvm_object temp;
    super.connect_phase(phase);
    connect_vpt_bfms();
    connect_eth_bfms();
    ingress_if = slu_resource_db#(virtual ingress_env_if)::get("ingress_if",`__FILE__,`__LINE__);

    if (env_monitor != null) begin
      env_monitor.ingress_if = ingress_if;
    end
  endfunction // void

  //////////////////////////////////////////////////////////////////////////////
  // Function: ingress_env end_of_elaboration
  // end_of_elaboration  phase of ingress_env
  // In this pahse we randomize the fuse env
  virtual function void end_of_elaboration_phase (uvm_phase phase);
    super.end_of_elaboration_phase(phase);
  endfunction

  //////////////////////////////////////////////////////////////////////////////
  // Function: ingress_env start_of_simulation
  // start_of_simulation  phase of ingress_env
  virtual function void start_of_simulation_phase (uvm_phase phase);
    super.start_of_simulation_phase(phase);
  endfunction

  //////////////////////////////////////////////////////////////////////////////
  // Function: ingress_env  run
  // run  phase of ingress_env
  virtual task run_phase (uvm_phase phase);
    super.run_phase(phase);
    //fork
    //  if (_level == SLA_TOP)
        //// this task is called only in IP level
        //ingress_im_monitor();
    //join_none
  endtask // run

  // ---------------------------------------------------------------
  // Ingress ENV VC"s & VC's  UVM phases functions / tasks
  // ---------------------------------------------------------------
  //////////////////////////////////////////////////////////////////////////////
  // Function: build_vpt_bfms
  // Gets the virtual port interfaces from the configuration database and 
  // creates the virtual port Bus Functional Model instances
  function void build_vpt_bfms();
    foreach(vp_bfms[i]) begin
      // Get the vp_bfm_vif ptrs
      if(!uvm_config_db#(igr_eth_bfm_tx_intf_t)::get(this, "",
        $sformatf("igr_eth_bfm_tx_vintf%0d", i+4), vp_bfm_tx_vintf[i])) begin
        `uvm_fatal(get_name(),"Config_DB.get() for ENV's igr_eth_bfm_tx_intf_t was not successful!")
      end
      if(!uvm_config_db#(igr_eth_bfm_rx_intf_t)::get(this, "", 
        $sformatf("igr_eth_bfm_rx_vintf%0d", i+4), vp_bfm_rx_vintf[i])) begin
        `uvm_fatal(get_name(),"Config_DB.get() for ENV's igr_eth_bfm_rx_intf_t was not successful!")
      end
      // Create the vp bfm instances
      vp_bfms[i]               = igr_vp_bfm_t::type_id::create($sformatf("igr_vp_bfm%0d", i), this);
      vp_bfms[i].cfg.mode      = eth_bfm_pkg::MODE_MASTER;                            // Configure as MASTER
      vp_bfms[i].cfg.speed     = eth_bfm_pkg::SPEED_400G;                             // Configure speed.
      vp_bfms[i].cfg.num_ports = 1;                                                   // Configure num_ports.
      vp_bfm_tx_io[i] = igr_eth_bfm_tx_io_t::type_id::create($sformatf("vp_bfm_tx_io%0d", i), this);
      vp_bfm_rx_io[i] = igr_eth_bfm_rx_io_t::type_id::create($sformatf("vp_bfm_rx_io%0d", i), this);
    end
  endfunction

  //////////////////////////////////////////////////////////////////////////////
  // Function: build_eth_bfms
  // Gets the eth interfaces from the configuration database and creates
  // the ethernet Bus Functional Model instances
  function void build_eth_bfms();
    foreach(eth_bfms[i]) begin
      // Get the eth_bfm_vif ptrs
      if(!uvm_config_db#(igr_eth_bfm_tx_intf_t)::get(this, "",
        $sformatf("igr_eth_bfm_tx_vintf%0d", i),eth_bfm_tx_vintf[i])) begin
        `uvm_fatal(get_name(),"Config_DB.get() for ENV's igr_eth_bfm_tx_intf_t was not successful!")
      end
      if(!uvm_config_db#(igr_eth_bfm_rx_intf_t)::get(this, "", 
        $sformatf("igr_eth_bfm_rx_vintf%0d", i), eth_bfm_rx_vintf[i])) begin
        `uvm_fatal(get_name(),"Config_DB.get() for ENV's igr_eth_bfm_rx_intf_t was not successful!")
      end
      // Create the bfm instances
      eth_bfms[i]                   = igr_eth_bfm_t::type_id::create($sformatf("igr_eth_bfm%0d", i), this);
      eth_bfms[i].cfg.mode          = eth_bfm_pkg::MODE_MASTER;                            // Configure as MASTER
      eth_bfms[i].cfg.speed         = eth_bfm_pkg::SPEED_400G;                             // Configure speed.
      eth_bfms[i].cfg.num_ports     = 1;                                                   // Configure num_ports.
      eth_bfms[i].cfg.group_size    = 8;
      eth_bfms[i].cfg.sop_alignment = 8;
      eth_bfm_tx_io[i] = igr_eth_bfm_tx_io_t::type_id::create($sformatf("eth_bfm_tx_io%0d", i), this);
      eth_bfm_rx_io[i] = igr_eth_bfm_rx_io_t::type_id::create($sformatf("eth_bfm_rx_io%0d", i), this);
    end
  endfunction

  //////////////////////////////////////////////////////////////////////////////
  // Function: connect_vpt_bfms
  // Sets VIF to the IO policies, adds IO policy class to the BFM and adds sequencer
  // pointer to SLA vsqr
  function void connect_vpt_bfms();
    foreach(vp_bfms[i]) begin
      vp_bfm_tx_io[i].set_vintf(vp_bfm_tx_vintf[i]);
      vp_bfm_rx_io[i].set_vintf(vp_bfm_rx_vintf[i]);
      vp_bfms[i].set_io(vp_bfm_tx_io[i], vp_bfm_rx_io[i]);
      void'(this.add_sequencer($sformatf("vp_bfm_%0d", i), 
        $sformatf("vp_bfm_%0d_tx0", i), vp_bfms[i].tx.frame_sequencer[0]));
      void'(this.add_sequencer($sformatf("vp_bfm_%0d", i), 
        $sformatf("vp_bfm_%0d_tx1", i), vp_bfms[i].tx.frame_sequencer[1]));
      void'(this.add_sequencer($sformatf("vp_bfm_%0d", i), 
        $sformatf("vp_bfm_%0d_tx2", i), vp_bfms[i].tx.frame_sequencer[2]));
      void'(this.add_sequencer($sformatf("vp_bfm_%0d", i), 
        $sformatf("vp_bfm_%0d_tx3", i), vp_bfms[i].tx.frame_sequencer[3]));
    end
  endfunction

  //////////////////////////////////////////////////////////////////////////////
  // Function: connect_eth_bfms
  // Sets VIF to the IO policies, adds IO policy class to the BFM and adds sequencer
  // pointer to SLA vsqr
  function void connect_eth_bfms();
    foreach(eth_bfms[i]) begin
      eth_bfm_tx_io[i].set_vintf(eth_bfm_tx_vintf[i]);
      eth_bfm_rx_io[i].set_vintf(eth_bfm_rx_vintf[i]);
      eth_bfms[i].set_io(eth_bfm_tx_io[i], eth_bfm_rx_io[i]);
      void'(this.add_sequencer($sformatf("eth_bfm_%0d", i), 
        $sformatf("eth_bfm_%0d_tx0", i), eth_bfms[i].tx.frame_sequencer[0]));
      void'(this.add_sequencer($sformatf("eth_bfm_%0d", i), 
        $sformatf("eth_bfm_%0d_tx1", i), eth_bfms[i].tx.frame_sequencer[1]));
      void'(this.add_sequencer($sformatf("eth_bfm_%0d", i), 
        $sformatf("eth_bfm_%0d_tx2", i), eth_bfms[i].tx.frame_sequencer[2]));
      void'(this.add_sequencer($sformatf("eth_bfm_%0d", i), 
        $sformatf("eth_bfm_%0d_tx3", i), eth_bfms[i].tx.frame_sequencer[3]));
    end
  endfunction


  // ---------------------------------------------------------------
  // Ingress ENV Saola functions / tasks
  // ---------------------------------------------------------------

  //////////////////////////////////////////////////////////////////////////////
  // Function: get_ingress_env
  // Returns pointer to self
  static function ingress_env get_ingress_env();
    return _ingress_env;
  endfunction

  //////////////////////////////////////////////////////////////////////////////
  // Saola TB clk
  virtual task set_clk_rst();
    fork
      forever begin
        @(ingress_if.clock);
        #0;
        if (ingress_if.clock === 1'b1)
          ->sys_clk_r;
        if (ingress_if.clock === 1'b0)
          ->sys_clk_f;
      end
      forever begin
        @(ingress_if.reset);
        #0;
        if (ingress_if.reset === 1'b1)
          -> sys_rst_r;
        if (ingress_if.reset === 1'b0)
          -> sys_rst_f;
      end
    join_none
  endtask // set_clk_rst

  // ---------------------------------------------------------------
  // Ingress ENV Specific functions / tasks
  // ---------------------------------------------------------------

  //////////////////////////////////////////////////////////////////////////////
  // Task: ingress_im_monitor
  // Wait for interuppt event and trigger IM
  // ISR (Interrupt Service Routine) only in IP level
  task ingress_im_monitor();
    uvm_event ingress_int_detect_e;
    ingress_int_detect_e = ingress_epool.get("INGRESS_INT_ASSERT");

    forever begin
      ingress_int_detect_e.wait_trigger();
    end
  endtask

endclass // ingress_env
