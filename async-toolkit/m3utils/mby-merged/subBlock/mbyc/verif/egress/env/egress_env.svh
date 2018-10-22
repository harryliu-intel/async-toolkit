//-----------------------------------------------------------------------------
// Title         : Egress main environment class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : egress_env.svh
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

class egress_env extends egress_base_env;

  // Variable: ti_config
  // test island configuration object
  egress_ti_config ti_config;
  //protected string egress_ti_low_path   = "XYZ_tb.u_egress_ti";

  // Variable: _egress_env
  // static pointer to the env
  static egress_env _egress_env;

  // Variable: egress_if
  // Egress env interface
  egr_env_if_t egress_if;

  // Variable: egress_epool
  // Egress event pool
  uvm_event_pool egress_epool;

  // ---------------------------------------------------------------
  // IP Agents and VC's declaration
  // ---------------------------------------------------------------

  // Variable:  eth_bfms
  // MAC Client BFM agent
  egr_eth_bfm_t eth_bfms[`NUM_EPLS_PER_EGR];

  // Variable:  eth_bfm_tx_io
  // MAC Client BFM io policy
  egr_eth_bfm_tx_io_t eth_bfm_tx_io[`NUM_EPLS_PER_EGR];

  // Variable:  eth_bfm_rx_io
  // MAC Client BFM io policy
  egr_eth_bfm_rx_io_t eth_bfm_rx_io[`NUM_EPLS_PER_EGR];

  // Variable:  eth_bfm_tx_vintf
  // MAC Client BFM virtual interface
  egr_eth_bfm_tx_intf_t eth_bfm_tx_vintf[`NUM_EPLS_PER_EGR];

  // Variable:  eth_bfm_rx_vintf
  // MAC Client BFM virtual interface
  egr_eth_bfm_rx_intf_t eth_bfm_rx_vintf[`NUM_EPLS_PER_EGR];

  // Variable: env_monitor
  // egress env event monitor
  egress_env_monitor env_monitor;

  `uvm_component_utils_begin(egress_env)
  //`uvm_field_string(egress_ti_low_path, UVM_ALL_ON)
  `uvm_component_utils_end

  function new (string name="egress_env", uvm_component parent = null);
    super.new(name, parent);
    // Setting the env static pointer
    _egress_env = this;
  endfunction: new

  // Function: egress_env build
  // build phase of egress_env
  // All VC's and Agent should be build in this pahse.
  // For each new VC's/Agnet it is recommand to add it an a specific function
  virtual function void build_phase(uvm_phase phase);
    uvm_object tmp_ti_cfg_obj;

    super.build_phase(phase);

    if(uvm_config_object::get(this, "",
        "egress_ti_config",tmp_ti_cfg_obj)) begin
      assert($cast(ti_config,tmp_ti_cfg_obj));
    end

    foreach(eth_bfms[i]) begin

      // Get the eth_bfm_vif ptrs
      if(!uvm_config_db#(egr_eth_bfm_tx_intf_t)::get(this, "",
        $sformatf("egr_eth_bfm_tx_vintf%0d", i),eth_bfm_tx_vintf[i])) begin
        `uvm_fatal(get_name(),"Config_DB.get() for ENV's egr_eth_bfm_tx_intf_t was not successful!")
      end
      if(!uvm_config_db#(egr_eth_bfm_rx_intf_t)::get(this, "", 
        $sformatf("egr_eth_bfm_rx_vintf%0d", i), eth_bfm_rx_vintf[i])) begin
        `uvm_fatal(get_name(),"Config_DB.get() for ENV's egr_eth_bfm_rx_intf_t was not successful!")
      end

      // Create the bfm instances
      eth_bfms[i]                = egr_eth_bfm_t::type_id::create($sformatf("egr_eth_bfm%0d", i), this);
      eth_bfms[i].cfg.mode       = eth_bfm_pkg::MODE_MASTER;                            // Configure as MASTER
      eth_bfms[i].cfg.port_speed = {eth_bfm_pkg::SPEED_400G,                            // Configure speed.
                                    eth_bfm_pkg::SPEED_OFF,
                                    eth_bfm_pkg::SPEED_OFF,
                                    eth_bfm_pkg::SPEED_OFF};
      //eth_bfms[i].cfg.num_ports = 1;                                                   // Configure num_ports.
      
      eth_bfm_tx_io[i] = egr_eth_bfm_tx_io_t::type_id::create($sformatf("eth_bfm_tx_io%0d", i), this);
      eth_bfm_rx_io[i] = egr_eth_bfm_rx_io_t::type_id::create($sformatf("eth_bfm_rx_io%0d", i), this);
    end

    data_phase_mode = SLA_RANDOM_NONE;
    this.max_run_clocks = 2_000_000_000;

    // Env monitor
    assert($cast(env_monitor, create_component("egress_env_monitor","env_monitor")));
    env_monitor.set_monitor_enable(cfg.get_monitors_enabled());

    // get global event pool
    egress_epool = egress_epool.get_global_pool();

  endfunction // void

  // Function: egress_env connect
  // connect phase of egress_env
  function void connect_phase(uvm_phase phase);
    uvm_object temp;
    super.connect_phase(phase);

    egress_if = slu_resource_db#(virtual egress_env_if)::get("egress_if",`__FILE__,`__LINE__);

    foreach(eth_bfms[i]) begin
      eth_bfm_tx_io[i].set_vintf(eth_bfm_tx_vintf[i]);
      eth_bfm_rx_io[i].set_vintf(eth_bfm_rx_vintf[i]);
      eth_bfms[i].set_io(eth_bfm_tx_io[i], eth_bfm_rx_io[i]);   // Set the IO Policy in the BFM
      void'(this.add_sequencer($sformatf("eth_bfm_%0d", i), $sformatf("eth_bfm_%0d_tx0", i), eth_bfms[i].tx.frame_sequencer[0]));
      void'(this.add_sequencer($sformatf("eth_bfm_%0d", i), $sformatf("eth_bfm_%0d_tx1", i), eth_bfms[i].tx.frame_sequencer[1]));
      void'(this.add_sequencer($sformatf("eth_bfm_%0d", i), $sformatf("eth_bfm_%0d_tx2", i), eth_bfms[i].tx.frame_sequencer[2]));
      void'(this.add_sequencer($sformatf("eth_bfm_%0d", i), $sformatf("eth_bfm_%0d_tx3", i), eth_bfms[i].tx.frame_sequencer[3]));
    end

    if (env_monitor != null) begin
      env_monitor.egress_if = egress_if;
    end
  endfunction // void

  // Function: egress_env end_of_elaboration
  // end_of_elaboration  phase of egress_env
  // In this pahse we randomize the fuse env
  virtual function void end_of_elaboration_phase (uvm_phase phase);
    super.end_of_elaboration_phase(phase);
  endfunction

  // Function: egress_env start_of_simulation
  // start_of_simulation  phase of egress_env
  virtual function void start_of_simulation_phase (uvm_phase phase);
    super.start_of_simulation_phase(phase);
  endfunction

  // Function: egress_env  run
  // run  phase of egress_env
  virtual task run_phase (uvm_phase phase);
    super.run_phase(phase);
    //fork
    //  if (_level == SLA_TOP)
        //// this task is called only in IP level
        //egress_im_monitor();
    //join_none
  endtask // run

  // ---------------------------------------------------------------
  // Egress ENV VC"s & VC's  UVM phases functions / tasks
  // ---------------------------------------------------------------

  // ---------------------------------------------------------------
  // Egress ENV Saola functions / tasks
  // ---------------------------------------------------------------

  static function egress_env get_egress_env();
    return _egress_env;
  endfunction

  // Saola TB clk
  virtual task set_clk_rst();
    fork
      forever begin
        @(egress_if.clock);
        #0;
        if (egress_if.clock === 1'b1)
          ->sys_clk_r;
        if (egress_if.clock === 1'b0)
          ->sys_clk_f;
      end
      forever begin
        @(egress_if.reset);
        #0;
        if (egress_if.reset === 1'b1)
          -> sys_rst_r;
        if (egress_if.reset === 1'b0)
          -> sys_rst_f;
      end
    join_none
  endtask // set_clk_rst

  // ---------------------------------------------------------------
  // Egress ENV Specific functions / tasks
  // ---------------------------------------------------------------

  // Task: egress_im_monitor
  // Wait for interuppt event and trigger IM
  // ISR (Interrupt Service Routine) only in IP level
  task egress_im_monitor();
    uvm_event egress_int_detect_e;
    egress_int_detect_e = egress_epool.get("EGRESS_INT_ASSERT");

    forever begin
      egress_int_detect_e.wait_trigger();
    end
  endtask

endclass // egress_env
