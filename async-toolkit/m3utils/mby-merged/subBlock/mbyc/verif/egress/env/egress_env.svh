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
// This class builds and connects the differents agents/BFMs/VCs needed by this
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
  virtual egress_env_if egress_if;

  // Variable: egress_epool
  // Egress event pool
  uvm_event_pool    egress_epool;

  // ---------------------------------------------------------------
  // IP Agents and VC's declaration
  // ---------------------------------------------------------------

  // Variable:  eth_cdi_bfm
  // MAC Client BFM agent
  ec_env_defines::mby_ec_bfm_t eth_cdi_bfm;

  // Variable:  cdi_tx_io
  // MAC Client BFM io policy
  ec_env_defines::cdi_tx_io_t eth_cdi_tx_io;

  // Variable:  cdi_rx_io
  // MAC Client BFM io policy
  ec_env_defines::cdi_rx_io_t eth_cdi_rx_io;

  // Variable:  cdi_tx_vintf
  // MAC Client BFM virtual interface
  ec_env_defines::cdi_tx_vintf_t cdi_tx_vintf;

  // Variable:  cdi_rx_vintf
  // MAC Client BFM virtual interface
  ec_env_defines::cdi_rx_vintf_t cdi_rx_vintf;

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

    if(uvm_config_object::get(this, "","egress_ti_config",tmp_ti_cfg_obj)) begin
      assert($cast(ti_config,tmp_ti_cfg_obj));
    end
    if(!uvm_config_db#(ec_env_defines::cdi_tx_vintf_t)::get(this, "", "cdi_tx_vintf", cdi_tx_vintf)) begin
      `uvm_fatal(get_name(),"Config_DB.get() for ENV's cdi_tx_vintf was not successful!")
    end
    if(!uvm_config_db#(ec_env_defines::cdi_rx_vintf_t)::get(this, "", "cdi_rx_vintf", cdi_rx_vintf)) begin
      `uvm_fatal(get_name(),"Config_DB.get() for ENV's cdi_rx_vintf was not successful!")
    end

    eth_cdi_bfm               = ec_env_defines::mby_ec_bfm_t::type_id::create("eth_cdi_bfm", this); // Create the bfm instance
    eth_cdi_bfm.cfg.mode      = eth_bfm_pkg::MODE_MASTER;                                           // Configure as MASTER
    eth_cdi_bfm.cfg.speed     = eth_bfm_pkg::SPEED_400G;                                            // Configure speed.
    eth_cdi_bfm.cfg.num_ports = 2;                                                                  // Configure num_ports.
    eth_cdi_bfm.cfg.ack_delay = 0;
    eth_cdi_bfm.cfg.enable_to_data_tx_delay = 0;
    eth_cdi_bfm.cfg.push_down_knobs();                                                              // Push Down the Config Knobs

    eth_cdi_tx_io = ec_env_defines::cdi_tx_io_t::type_id::create("eth_cdi_tx_io", this);
    eth_cdi_rx_io = ec_env_defines::cdi_rx_io_t::type_id::create("eth_cdi_rx_io", this);
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

    eth_cdi_tx_io.set_vintf(cdi_tx_vintf);
    eth_cdi_rx_io.set_vintf(cdi_rx_vintf);
    eth_cdi_bfm.set_io(eth_cdi_tx_io, eth_cdi_rx_io);   // Set the IO Policy in the CDI BFM
    void'(this.add_sequencer("eth_agent", "tx0", eth_cdi_bfm.tx.frame_sequencer[0]));
    void'(this.add_sequencer("eth_agent", "tx1", eth_cdi_bfm.tx.frame_sequencer[1]));
    void'(this.add_sequencer("eth_agent", "tx2", eth_cdi_bfm.tx.frame_sequencer[2]));
    void'(this.add_sequencer("eth_agent", "tx3", eth_cdi_bfm.tx.frame_sequencer[3]));

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
        @(egress_if.primary_clock);
        #0;
        if (egress_if.primary_clock === 1'b1)
          ->sys_clk_r;
        if (egress_if.primary_clock === 1'b0)
          ->sys_clk_f;
      end
      forever begin
        @(egress_if.primary_reset);
        #0;
        if (egress_if.primary_reset === 1'b1)
          -> sys_rst_r;
        if (egress_if.primary_reset === 1'b0)
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
